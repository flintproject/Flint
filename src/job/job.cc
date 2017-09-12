/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>

#include "bc/binary.h"
#include "bc/index.h"
#include "compiler.h"
#include "database.h"
#include "filter/writer.h"
#include "flint/bc.h"
#include "flint/ctrl.h"
#include "flint/ls.h"
#include "job.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "phsp.h"
#include "sedml.h"
#include "solver.h"
#include "task.h"

namespace flint {
namespace job {

bool Job(int id,
		 ctrl::Argument *arg,
		 const boost::filesystem::path &task_dir,
		 const boost::filesystem::path &job_dir,
		 task::Task &task,
		 const fppp::Option *fppp_option,
		 std::vector<double> *data,
		 const boost::filesystem::path &output_file)
{
	boost::system::error_code ec;
	// ensure the job directory
	boost::filesystem::create_directories(job_dir, ec);
	if (ec) {
		std::cerr << "failed to create directory: " << ec << std::endl;
		return false;
	}

	if (!task::Timer(task.length, task.step, data->data()))
		return false;

	job::Option option;
	option.id = id;
	option.dir = job_dir;
	option.input_data = data;
	option.output_data_file = job_dir / "output-data";
	option.output_history_file = job_dir / "output-history";
	option.arg = arg;
	option.fppp_option = fppp_option;

	auto isdh_file = task_dir / "isdh";
	boost::filesystem::copy_file(isdh_file, output_file, ec);
	if (ec) {
		std::cerr << "failed to copy "
			 << isdh_file
			 << " to "
			 << output_file
			 << ": " << ec << std::endl;
		return false;
	}
	boost::filesystem::ofstream ofs(output_file, std::ios::out|std::ios::binary|std::ios::app);
	if (!ofs) {
		std::cerr << "failed to open " << output_file << std::endl;
		return false;
	}
	// write initial values only when output_start_time is 0.
	if (task.output_start_time == 0) {
		if (!task.writer->Write(data->data(), ofs)) {
			ofs.close();
			return false;
		}
	}
	option.output_stream = &ofs;

	bool r;
	if (task.method == compiler::Method::kArk) {
		r = solver::Solve(solver::Method::kArk, task, option);
	} else {
		r = job::Evolve(task, option);
	}
	ofs.close();
	return r;
}

}
}
