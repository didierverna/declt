### Makefile --- Doc directory

## Copyright (C) 2010, 2011, 2013 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

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



### Code:

TOP_DIR := ..

include $(TOP_DIR)/Makefile.cnf

hack: all

include $(TOP_DIR)/Makefile.inc
include $(TOP_DIR)/version.inc


REF_TEXI_FILES     := reference.texi
REF_INFO_FILES     := $(REF_TEXI_FILES:%.texi=$(PROJECT)-%.info)
REF_PDF_FILES      := $(REF_TEXI_FILES:%.texi=%.pdf)
ALL_REF_INFO_FILES := $(REF_INFO_FILES:%.info=%.info*)
REF_HTML_DIRS      := $(REF_TEXI_FILES:%.texi=%)
REF_DVI_FILES      := $(REF_TEXI_FILES:%.texi=%.dvi)
REF_PS_FILES       := $(REF_TEXI_FILES:%.texi=%.ps)

WEBREF_TEXI_FILES := webreference.texi
WEBREF_PDF_FILES  := $(WEBREF_TEXI_FILES:%.texi=%.pdf)
WEBREF_HTML_DIRS  := $(WEBREF_TEXI_FILES:%.texi=%)

TEXI_FILES     := user.texi
INFO_FILES     := $(TEXI_FILES:%.texi=$(PROJECT)-%.info)
PDF_FILES      := $(TEXI_FILES:%.texi=%.pdf)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
HTML_DIRS      := $(TEXI_FILES:%.texi=%)
DVI_FILES      := $(TEXI_FILES:%.texi=%.dvi)
PS_FILES       := $(TEXI_FILES:%.texi=%.ps)

INFO_DIR         := $(PREFIX)/share/info
DOC_DIR          := $(SHARE)/doc
PROJECT_DOC_DIR  := $(DOC_DIR)/$(PROJECT)


all: info pdf html

all-formats: all dvi ps

install: $(INFO_FILES) $(REF_INFO_FILES)	\
	 $(PDF_FILES) $(REF_PDF_FILES)		\
	 $(HTML_DIRS) $(REF_HTML_DIRS)
	$(INSTALL) -d "$(INFO_DIR)"
	$(INSTALL) -m 644 $(ALL_INFO_FILES) $(ALL_REF_INFO_FILES) \
	  "$(INFO_DIR)/"
	for i in $(INFO_FILES) $(REF_INFO_FILES); do		\
	  $(INSTALL-INFO) --info-dir="$(INFO_DIR)" $$i;	\
	done
	$(INSTALL) -d "$(PROJECT_DOC_DIR)"
	$(INSTALL) -m 644 $(PDF_FILES) $(REF_PDF_FILES) "$(PROJECT_DOC_DIR)/"
	for i in $(HTML_DIRS) $(REF_HTML_DIRS); do	\
	  $(INSTALL) -d -m 755 "$(PROJECT_DOC_DIR)/$$i" ;	\
	  $(INSTALL) -m 644 $$i/* "$(PROJECT_DOC_DIR)/$$i/" ;	\
	done
	for i in $(DVI_FILES) $(PS_FILES)				\
		 $(REF_DVI_FILES) $(REF_PS_FILES); do			\
	  test -f $$i && $(INSTALL) -m 644 $$i "$(PROJECT_DOC_DIR)/";	\
	done

uninstall:
	rm -fr "$(PROJECT_DOC_DIR)"
	for i in $(INFO_FILES) $(REF_INFO_FILES); do			\
	  $(INSTALL-INFO) --remove --info-dir="$(INFO_DIR)" $$i;	\
	done
	cd "$(INFO_DIR)" && rm -f $(ALL_INFO_FILES) $(ALL_REF_INFO_FILES)

www-dist: $(PDF_FILES) $(WEBREF_PDF_FILES) $(HTML_DIRS) $(WEBREF_HTML_DIRS)
	scp -r $? $(WWW_HOST):$(WWW_DIR)/
	ssh $(WWW_HOST)					\
	  'cd $(WWW_DIR)					\
	   && mv $(WEBREF_PDF_FILES) $(REF_PDF_FILES)		\
	   && rm -fr $(REF_HTML_DIRS)				\
	   && mv $(WEBREF_HTML_DIRS) $(REF_HTML_DIRS)'

info: $(INFO_FILES) $(REF_INFO_FILES)
pdf:  $(PDF_FILES)  $(REF_PDF_FILES)
html: $(HTML_DIRS)  $(REF_HTML_DIRS)
dvi:  $(DVI_FILES)  $(REF_DVI_FILES)
ps:   $(PS_FILES)   $(REF_PS_FILES)

reference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_LOAD) ./generate.cl

webreference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_LOAD) ./generate.cl --web

$(PROJECT)-%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2PDF) $<

$(HTML_DIRS) $(REF_HTML_DIRS) $(WEBREF_HTML_DIRS): %: %.texi
	$(MAKEINFO) --html -o $@ $<
	touch $@

%.dvi: %.texi
	$(TEXI2DVI) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.eps: %.jpg
	$(CONVERT) $< $@

clean:
	rm -f *.tmp *.fn* *.vr* *.ky* *.tp* *.cp* *.pg *.toc *.aux *.log *~

distclean: clean
	rm -fr $(REF_TEXI_FILES) $(WEBREF_TEXI_FILES)			\
	       $(ALL_INFO_FILES) $(ALL_REF_INFO_FILES)			\
	       $(PDF_FILES) $(REF_PDF_FILES) $(WEBREF_PDF_FILES)	\
	       $(HTML_DIRS) $(REF_HTML_DIRS) $(WEBREF_HTML_DIRS)	\
	       $(DVI_FILES) $(REF_DVI_FILES)				\
	       $(PS_FILES) $(REF_PS_FILES)

update-version:
	for f in $(TEXI_FILES); do					   \
	  $(PERL) -pi -e 's/@set VERSION .*/@set VERSION $(LONG_VERSION)/' \
	    $$f;							   \
	done

.PHONY: hack all all-formats		\
	install uninstall www-dist	\
	info pdf html dvi ps		\
	clean distclean		\
	update-version


### Makefile ends here
