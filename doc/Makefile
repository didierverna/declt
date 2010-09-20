### Makefile --- Doc directory

## Copyright (C) 2010 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun Sep 19 21:21:30 2010
## Last Revision: Mon Sep 20 15:35:42 2010

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

include ../Makefile.cnf
include ../Makefile.inc

INFO_DIR     := $(PREFIX)/share/info
DOC_DIR	     := $(SHARE)/doc
DECLT_DOC_DIR := $(DOC_DIR)/declt
TEXI_FILES   := declt.texi

INFO_FILES     := $(TEXI_FILES:%.texi=%.info)
PDF_FILES      := $(TEXI_FILES:%.texi=%.pdf)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
DVI_FILES      := $(TEXI_FILES:%.texi=%.dvi)
PS_FILES       := $(TEXI_FILES:%.texi=%.ps)
HTML_DIRS      := $(TEXI_FILES:%.texi=%)


all:

ref: info pdf html

all-formats: ref dvi ps

install-at: $(PDF_FILES) $(HTML_DIRS)
	install -d $(LOCATION)
	install -m 644 $(PDF_FILES) $(LOCATION)/
	install -d -m 755 $(LOCATION)/declt
	install -m 644 declt/* $(LOCATION)/declt

install:

install-ref: $(INFO_FILES)
	install -d $(INFO_DIR)
	install -m 644 $(ALL_INFO_FILES) $(INFO_DIR)/
	for i in $(INFO_FILES); do			\
	  install-info --info-dir=$(INFO_DIR) $$i;	\
	done
	$(MAKE) install-at LOCATION=$(DECLT_DOC_DIR)

uninstall:
	-rm -fr $(DECLT_DOC_DIR)
	for i in $(INFO_FILES); do				\
	  install-info --remove --info-dir=$(INFO_DIR) $$i;	\
	done
	cd $(INFO_DIR) && rm -f $(ALL_INFO_FILES)

declt.texi:
	./generate.sh

info: $(INFO_FILES)

pdf: $(PDF_FILES)

html: $(HTML_DIRS)

dvi: $(DVI_FILES)

ps: $(PS_FILES)

%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2DVI) -p $<

$(HTML_DIRS): %: %.texi
	$(MAKEINFO) --html -o $@ $<
	touch $@

%.dvi: %.texi
	$(TEXI2DVI) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

clean:
	-rm *.tmp *.fn* *.vr* *.ky* *.tp* *.cp* *.pg *.toc *.aux *.log *~

distclean: clean
	-rm declt.texi
	-rm -fr $(HTML_DIRS)						\
		$(ALL_INFO_FILES) $(PDF_FILES) $(DVI_FILES) $(PS_FILES)

install-www:
	$(MAKE) install-at LOCATION=$(W3DIR)


.PHONY: all all-formats			\
	install-at install uninstall	\
	info pdf dvi ps html		\
	clean distclean			\
	install-www


### Makefile ends here
