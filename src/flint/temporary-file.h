/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TEMORARY_FILE_H_
#define FLINT_TEMORARY_FILE_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {

class TemporaryFile {
public:
	TemporaryFile()
		: path_(boost::filesystem::temp_directory_path() / boost::filesystem::unique_path("flint-%%%%-%%%%-%%%%-%%%%"))
		, ofs_(path_, std::ios::out|std::ios::binary)
		, own_(true)
	{}

	~TemporaryFile() {
		if (ofs_.is_open())
			ofs_.close();
		if (own_) {
			boost::system::error_code ec;
			boost::filesystem::remove(path_, ec);
		}
	}

	const boost::filesystem::path &path() const {return path_;}
	boost::filesystem::ofstream &ofs() {return ofs_;}

	void Close() {
		ofs_.close();
	}

	void Rename(const boost::filesystem::path &target) {
		if (ofs_.is_open())
			ofs_.close();
		boost::system::error_code ec;
		boost::filesystem::rename(path_, target, ec);
		own_ = ec;
	}

private:
	boost::filesystem::path path_;
	boost::filesystem::ofstream ofs_;
	bool own_;
};

}

#endif
