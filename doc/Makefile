### Makefile --- Doc directory

## Copyright (C) 2010, 2011 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>

## This file is part of Declt.

## Declt is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 3,
## as published by the Free Software Foundation.

## Declt is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


### Commentary:

## Contents management by FCM version 0.1.


### Code:

TOP_DIR := ..

include $(TOP_DIR)/Makefile.cnf

hack: all

include $(TOP_DIR)/Makefile.inc
include $(TOP_DIR)/version.inc


INFO_DIR      := $(PREFIX)/share/info
DOC_DIR	      := $(SHARE)/doc
DECLT_DOC_DIR := $(DOC_DIR)/declt
TEXI_FILES    := reference.texi

INFO_FILES     := $(TEXI_FILES:%.texi=declt-%.info)
PDF_FILES      := $(TEXI_FILES:%.texi=%.pdf)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
DVI_FILES      := $(TEXI_FILES:%.texi=%.dvi)
PS_FILES       := $(TEXI_FILES:%.texi=%.ps)
HTML_DIRS      := $(TEXI_FILES:%.texi=%)


all: info pdf html

webref: pdf-webref html-webref

all-formats: all dvi ps

install-at: $(PDF_FILES) $(HTML_DIRS)
	$(INSTALL) -d "$(LOCATION)"
	$(INSTALL) -m 644 $(PDF_FILES) "$(LOCATION)/"
	for i in $(HTML_DIRS); do			\
	  $(INSTALL) -d -m 755 "$(LOCATION)/$$i" ;		\
	  $(INSTALL) -m 644 $$i/* "$(LOCATION)/$$i/" ;	\
	done

install: $(INFO_FILES)
	$(INSTALL) -d "$(INFO_DIR)"
	$(INSTALL) -m 644 $(ALL_INFO_FILES) "$(INFO_DIR)/"
	for i in $(INFO_FILES); do			\
	  $(INSTALL-INFO) --info-dir="$(INFO_DIR)" $$i;	\
	done
	$(MAKE) install-at LOCATION="$(DECLT_DOC_DIR)"
	-for i in $(DVI_FILES) $(PS_FILES); do			\
	  test -f $$i && $(INSTALL) -m 644 $$i "$(DECLT_DOC_DIR)";	\
	done

uninstall:
	-rm -fr "$(DECLT_DOC_DIR)"
	for i in $(INFO_FILES); do				\
	  $(INSTALL-INFO) --remove --info-dir="$(INFO_DIR)" $$i;	\
	done
	cd "$(INFO_DIR)" && rm -f $(ALL_INFO_FILES)

info: $(INFO_FILES)

pdf: $(PDF_FILES)
pdf-webref: webreference.pdf

html: $(HTML_DIRS)
html-webref: webreference

dvi: $(DVI_FILES)

ps: $(PS_FILES)


reference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_EXEC) ./generate.cl

webreference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_EXEC) ./generate.cl --web

declt-%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2DVI) -p $<

$(HTML_DIRS) webreference: %: %.texi
	$(MAKEINFO) --html -o $@ $<
	touch $@

%.dvi: %.texi
	$(TEXI2DVI) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.eps: %.jpg
	$(CONVERT) $< $@

clean:
	-rm *.tmp *.fn* *.vr* *.ky* *.tp* *.cp* *.pg *.toc *.aux *.log *~

distclean: clean
	-rm -fr declt.eps webreference*					\
		$(HTML_DIRS)						\
		$(ALL_INFO_FILES) $(PDF_FILES) $(DVI_FILES) $(PS_FILES)

update-version:
	for f in $(TEXI_FILES); do					   \
	  $(PERL) -pi -e 's/@set VERSION .*/@set VERSION $(LONG_VERSION)/' \
	    $$f;							   \
	done

install-www: webref
	$(MAKE) install-at LOCATION="$(W3DIR)"
	$(INSTALL) -m 644 webreference.pdf "$(W3DIR)/reference.pdf"
	$(INSTALL) -d -m 755 "$(W3DIR)/reference"
	$(INSTALL) -m 644 webreference/* "$(W3DIR)/reference/"


.PHONY: all webref all-formats		\
	install-at install uninstall	\
	info				\
	pdf pdf-webref			\
	html html-webref		\
	dvi				\
	ps				\
	clean distclean			\
	update-version			\
	install-www


### Makefile ends here
