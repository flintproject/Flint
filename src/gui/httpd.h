/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_HTTPD_H_
#define FLINT_GUI_HTTPD_H_

namespace flint {
namespace gui {

class MainFrame;

class Httpd
{
public:
	Httpd();
	~Httpd();

	bool Start(MainFrame *frame);

private:
	void *daemon_;
};

}
}

#endif
