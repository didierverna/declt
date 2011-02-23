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


INFO_DIR        := $(PREFIX)/share/info
DOC_DIR	        := $(SHARE)/doc
DECLT_DOC_DIR := $(DOC_DIR)/declt
TEXI_FILES      :=

INFO_FILES     := $(TEXI_FILES:%.texi=declt-%.info)
PDF_FILES      := $(TEXI_FILES:%.texi=%.pdf)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
DVI_FILES      := $(TEXI_FILES:%.texi=%.dvi)
PS_FILES       := $(TEXI_FILES:%.texi=%.ps)
HTML_DIRS      := $(TEXI_FILES:%.texi=%)


all: info pdf html

ref: info-ref pdf-ref html-ref

webref: pdf-webref html-webref

all-formats: all dvi ps

all-formats-ref: ref dvi-ref ps-ref

install-at: $(PDF_FILES) $(HTML_DIRS)
	install -d "$(LOCATION)"
	install -m 644 $(PDF_FILES) "$(LOCATION)/"
	install -d -m 755 "$(LOCATION)/user"
	install -m 644 user/* "$(LOCATION)/user/"

install: $(INFO_FILES)
	install -d "$(INFO_DIR)"
	install -m 644 $(ALL_INFO_FILES) "$(INFO_DIR)/"
	for i in $(INFO_FILES); do			\
	  install-info --info-dir="$(INFO_DIR)" $$i;	\
	done
	$(MAKE) install-at LOCATION="$(CLON_DOC_DIR)"
	-for i in $(DVI_FILES) $(PS_FILES); do			\
	  test -f $$i && install -m 644 $$i "$(CLON_DOC_DIR)";	\
	done
	-if test -f declt-reference.info; then			\
	  install -m 644 declt-reference.info* "$(INFO_DIR)/" ;	\
	  install-info --info-dir="$(INFO_DIR)" declt-reference.info ;\
	fi
	-if test -f reference.pdf ; then				\
	  install -m 644 reference.pdf "$(DECLT_DOC_DIR)/" ;		\
	fi
	-if test -f reference ; then					\
	  install -d -m 755 "$(DECLT_DOC_DIR)/reference" ;		\
	  install -m 644 reference/* "$(DECLT_DOC_DIR)/reference/" ;	\
	fi
	-for i in reference.dvi reference.ps; do			\
	  test -f $$i && install -m 644 $$i "$(DECLT_DOC_DIR)";	\
	done

uninstall:
	-rm -fr "$(DECLT_DOC_DIR)"
	for i in $(INFO_FILES) declt-reference.info; do	\
	  install-info --remove --info-dir="$(INFO_DIR)" $$i;	\
	done
	cd "$(INFO_DIR)" && rm -f $(ALL_INFO_FILES) declt-reference.info*

info: $(INFO_FILES)
info-ref: declt-reference.info

pdf: $(PDF_FILES)
pdf-ref: reference.pdf
pdf-webref: webreference.pdf

html: $(HTML_DIRS)
html-ref: reference
html-webref: webreference

dvi: $(DVI_FILES)
dvi-ref: reference.dvi

ps: $(PS_FILES)
ps-ref: reference.ps

reference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_EXEC) ./generate.cl

webreference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_EXEC) ./generate.cl --web

declt-%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2DVI) -p $<

$(HTML_DIRS) reference webreference: %: %.texi
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
	-rm -fr declt.eps webreference* declt-reference* reference*	\
		$(HTML_DIRS)						\
		$(ALL_INFO_FILES) $(PDF_FILES) $(DVI_FILES) $(PS_FILES)

update-version:
	for f in $(TEXI_FILES); do					   \
	  $(PERL) -pi -e 's/@set VERSION .*/@set VERSION $(LONG_VERSION)/' \
	    $$f;							   \
	done

install-www: webref
	$(MAKE) install-at LOCATION="$(W3DIR)"
	install -m 644 webreference.pdf "$(W3DIR)/reference.pdf"
	install -d -m 755 "$(W3DIR)/reference"
	install -m 644 webreference/* "$(W3DIR)/reference/"


.PHONY: all ref webref all-formats all-formats-ref	\
	install-at install uninstall			\
	info info-ref					\
	pdf pdf-ref pdf-webref				\
	html html-ref html-webref			\
	dvi dvi-ref					\
	ps ps-ref					\
	clean distclean					\
	update-version					\
	install-www


### Makefile ends here
