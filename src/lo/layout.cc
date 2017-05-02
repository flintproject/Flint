/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "layout.h"

#include <iomanip>
#include <unordered_set>

#include "bc/locater.h"
#include "bc/mounter.h"
#include "cas.h"
#include "runtime/history.h"
#include "solver/ark/mmdm.h"

namespace flint {

Layout::Layout() = default;

Layout::~Layout() = default;

bool Layout::ContainsDependentVariable() const
{
	for (const auto &dp : dv_) {
		switch (dp->type()) {
		case lo::S:
		case lo::T:
			break;
		default:
			return true;
		}
	}
	return false;
}

int Layout::Calculate(DataOffsetMap *dom, SectorOffsetMap *som)
{
	int offset = kOffsetBase;
	int di = 0;
	int si = 0;
	boost::uuids::uuid track_id;
	boost::uuids::uuid sector_id;
	for (const auto &tp : tv_) {
		std::memcpy(&track_id, tp->id().data(), track_id.size());

		int nos = tp->nos();
		int nod = tp->nod();
		int die = di + nod;
		int sie = si + nos;
		int sector_size = 0;

		std::unique_ptr<Locater> locater(new Locater);
		while (di < die) {
			const auto &dp = dv_.at(di++);
			locater->SetPosition(dp->name(), sector_size);
			if (dom) (*dom)[track_id].emplace(dp->id(), sector_size);
			sector_size += dp->col() * dp->row();
		}
		lm_.emplace(track_id, std::move(locater));

		std::unique_ptr<Mounter> mounter(new Mounter(nos));
		for (int i=0;i<nos;i++) {
			mounter->SetOffset(i, offset);
			const auto &sp = sv_.at(si++);
			if (som) {
				std::memcpy(&sector_id, sp->id().data(), sector_id.size());
				som->emplace(sector_id, offset);
			}
			offset += sector_size;
		}
		mm_.emplace(track_id, std::move(mounter));
		assert(si == sie);
	}
	return offset;
}

const Locater *Layout::GetLocater(const std::string &id) const
{
	boost::uuids::uuid u;
	std::memcpy(&u, id.data(), u.size());
	auto it = lm_.find(u);
	if (it == lm_.end()) return nullptr;
	return it->second.get();
}

const Mounter &Layout::GetMounter(const std::string &id) const
{
	boost::uuids::uuid u;
	std::memcpy(&u, id.data(), u.size());
	assert(mm_.find(u) != mm_.end());
	return *mm_.at(u);
}

bool Layout::SpecifyCapacity(size_t size, History *history) const
{
	size_t offset = kOffsetBase;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				if (offset >= size) {
					std::cerr << "exceed history size: " << size << std::endl;
					return false;
				}
				if (dp->has_capacity()) {
					history[offset].set_capacity(dp->capacity());
				}
				offset += dp->col() * dp->row();
			}
		}
	}
	if (offset != size) {
		std::cerr << "failed to specify capacity at the end" << std::endl;
		return false;
	}
	return true;
}

void Layout::DetectRed(size_t size, const size_t *color) const
{
	std::unordered_set<std::string> red;

	size_t offset = kOffsetBase;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				assert(offset < size);
				if (!color[offset]) {
					boost::uuids::uuid u;
					std::memcpy(&u, tp->id().data(), u.size());
					std::string us = to_string(u);
					us += ":";
					us += dp->name();
					if (red.insert(us).second) {
						std::cerr << us << std::endl;
					}
				}
				offset += dp->col() * dp->row();
			}
		}
	}
	assert(offset == size);
}

void Layout::CollectConstant(int nol, size_t size, std::set<int> *addrs) const
{
	size_t offset = kOffsetBase;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				assert(offset < size);
				switch (dp->type()) {
				case lo::S:
					for (int j=0;j<nol;j++) {
						addrs->insert(offset + (j * size));
					}
					break;
				default:
					// nothing to do
					break;
				}
				offset += dp->col() * dp->row();
			}
		}
	}
	assert(offset == size);
}

size_t Layout::MarkConstant(int nol, size_t size, int *levels) const
{
	size_t num_of_on = 0;
	size_t offset = kOffsetBase;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				assert(offset < size);
				switch (dp->type()) {
				case lo::S:
					for (int j=0;j<nol;j++) {
						levels[offset + (j * size)] = 1;
						num_of_on++;
					}
					break;
				default:
					// nothing to do
					break;
				}
				offset += dp->col() * dp->row();
			}
		}
	}
	assert(offset == size);
	return num_of_on;
}

size_t Layout::MarkLiteral(int nol, size_t size, int *levels, size_t *color) const
{
	size_t k;
	size_t num_of_on = 0;
	size_t offset = kOffsetBase;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				assert(offset < size);
				switch (dp->type()) {
				case lo::S:
				case lo::X:
					if (dp->independent()) {
						for (int j=0;j<nol;j++) {
							k = offset + (j * size);
							levels[k] = 1;
							color[k] = 1;
							num_of_on++;
						}
					}
					break;
				default:
					// nothing to do
					break;
				}
				offset += dp->col() * dp->row();
			}
		}
	}
	assert(offset == size);
	return num_of_on;
}

long Layout::SelectStates(std::vector<std::pair<int, int> > *states) const
{
	long total = 0;
	size_t offset = kOffsetBase;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;
		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				int data_size = dp->col() * dp->row();
				switch (dp->type()) {
				case lo::X:
					if (states)
						states->emplace_back(offset, data_size);
					total += data_size;
					break;
				default:
					break;
				}
				offset += data_size;
			}
		}
	}
	return total;
}

bool Layout::SelectByKeyData(std::map<fppp::KeyData, size_t> *output) const
{
	assert(output);
	fppp::KeyData kd;
	size_t offset = kOffsetBase;
	int si = 0;
	int di = 0;
	for (const auto &tp : tv_) {
		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;
		for (int i=0;i<nos;i++) {
			const char *sector_id = sv_[si++]->id().data();
			std::memcpy(&kd.uuid, sector_id, kd.uuid.size());
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				int data_size = dp->col() * dp->row();
				if (dp->name().size() <= 32) {
					kd.name = dp->name();
					auto it = output->find(kd);
					if (it != output->end())
						it->second = offset; // TODO: vector/matrix case
				}
				offset += data_size;
			}
		}
	}
	return true;
}

bool Layout::GenerateMmdm(const cas::System &system, solver::ark::Mmdm *mmdm) const
{
	long index = 0; // in global mass matrix
	size_t offset = kOffsetBase;
	int di = 0;
	boost::uuids::uuid track_id;
	for (const auto &tp : tv_) {
		memcpy(&track_id, tp->id().data(), track_id.size());

		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;

		int pos = 0;
		std::unordered_map<std::string, int> am;
		std::unordered_map<std::string, std::string> xm;
		for (int i=0;i<nod;i++) {
			const auto &dp = dv_.at(di++);
			switch (dp->type()) {
			case lo::X:
				{
					std::string m;
					if (!system.FindMass(track_id, dp->name(), &m))
						return false;
					xm.emplace(dp->name(), m);
				}
				break;
			default:
				break;
			}
			am.emplace(dp->name(), pos);
			pos += dp->col() * dp->row();
		}
		assert(di == die);

		int bot = offset; // begin of track
		for (int i=0;i<nos;i++) {
			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				int data_size = dp->col() * dp->row();
				switch (dp->type()) {
				case lo::X:
					{
						const std::string &name = xm.at(dp->name());
						if (name.empty()) { // identity
							// column-major
							for (int col=0;col<data_size;col++) {
								for (int row=0;row<data_size;row++) {
									if (row == col)
										mmdm->Add(index + row, index + col, 1);
								}
							}
						} else {
							int m = am.at(name);
							// column-major
							for (int col=0;col<data_size;col++) {
								for (int row=0;row<data_size;row++) {
									mmdm->Add(index + row, index + col,
											  bot + (pos * i) + m + (col * data_size) + row);
								}
							}
						}
					}
					index += data_size;
					break;
				default:
					break;
				}
				offset += data_size;
			}
		}
	}
	return true;
}

void Layout::Debug(size_t size) const
{
	size_t offset = kOffsetBase;
	int si = 0;
	int di = 0;
	for (const auto &tp : tv_) {
		boost::uuids::uuid u;
		std::memcpy(&u, tp->id().data(), u.size());
		std::cout << "T " << u << " " << tp->name() << std::endl;

		int nos = tp->nos();
		int nod = tp->nod();
		int dib = di;
		int die = di + nod;

		for (int i=0;i<nos;i++) {
			std::memcpy(&u, sv_[si++]->id().data(), u.size());
			std::cout << "S " << u << std::endl;

			di = dib;
			while (di < die) {
				const auto &dp = dv_.at(di++);
				assert(offset < size);
				std::cout << '|' << std::setw(4) << offset << '|';
				switch (dp->type()) {
				case lo::S:
					std::cout << 'S';
					break;
				case lo::T:
					std::cout << 'T';
					break;
				case lo::V:
					std::cout << 'V';
					break;
				case lo::X:
					std::cout << 'X';
					break;
				}
				if (dp->independent())
					std::cout << 'i';
				else
					std::cout << ' ';
				std::cout << '|' << dp->name() << std::endl;
				offset += dp->col() * dp->row();
			}
		}
	}
	assert(offset == size);
}

}
