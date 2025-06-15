### Makefile --- Doc directory

## Copyright (C) 2010, 2011, 2013, 2015, 2017, 2021, 2025 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

## This file is part of Declt.

## Permission to use, copy, modify, and distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies.

## THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
## WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
## MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
## ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
## WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
## ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
## OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


### Commentary:

## Contents management by FCM version 0.1.


### Code:

TOP_DIR := ..

include $(TOP_DIR)/make/config.make
include $(TOP_DIR)/make/version.make
include $(TOP_DIR)/make/prologue.make

INFO_DIR        := $(PREFIX)/share/info
DOC_DIR	:= $(SHARE)/doc
PROJECT_DOC_DIR := $(DOC_DIR)/$(PROJECT)

USR_MAN := user
REF_MAN := reference
MANUALS := $(USR_MAN) $(REF_MAN)

TXT_FIGURES := pipeline
PNG_FIGURES := pipeline
DOT_FIGURES := definitions components varoids funcoids classoids aliases

TXT_USR_MAN_DEPS := $(TXT_FIGURES:%=%.txt) $(DOT_FIGURES:%=%.txt)
PNG_USR_MAN_DEPS := $(PNG_FIGURES:%=%.png) $(DOT_FIGURES:%=%.png)
EPS_USR_MAN_DEPS := $(PNG_FIGURES:%=%.eps) $(DOT_FIGURES:%=%.eps)

TEXI_FILES     := $(MANUALS:%=%.texi)
INFO_FILES     := $(MANUALS:%=$(PROJECT)-%.info)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
PDF_FILES      := $(MANUALS:%=%.pdf)
HTML_DIRS      := $(MANUALS)
DVI_FILES      := $(MANUALS:%=%.dvi)
PS_FILES       := $(MANUALS:%=%.ps)

CSS := texinfo-klare


all: info pdf html
all-formats: all dvi ps

# Auxiliary target to factor out some common installation for install and
# install-www. Needs a $(LOCATION) to install to.
install-at: $(PDF_FILES) $(HTML_DIRS)
	install -d "$(LOCATION)"
	install -m 644 $(PDF_FILES) "$(LOCATION)"/
	for i in $(HTML_DIRS); do			\
	  install -d -m 755 "$(LOCATION)"/$$i;		\
	  install -m 644 $$i/* "$(LOCATION)"/$$i/;	\
	done

install: $(INFO_FILES) $(PDF_FILES) $(HTML_DIRS)
	install -d "$(INFO_DIR)"
	install -m 644 $(ALL_INFO_FILES) "$(INFO_DIR)"/
	for i in $(INFO_FILES); do					\
	  $(INSTALL_INFO) --info-dir="$(INFO_DIR)" $$i;		\
	done
	cp -RL $(PROJECT)-$(USR_MAN)-data "$(INFO_DIR)/"
	$(MAKE) install-at LOCATION="$(PROJECT_DOC_DIR)"
	for i in $(DVI_FILES) $(PS_FILES); do				\
	  test -f $$i && install -m 644 $$i "$(PROJECT_DOC_DIR)"/;	\
	done

uninstall:
	rm -fr "$(PROJECT_DOC_DIR)"
	for i in $(INFO_FILES); do					\
	  $(INSTALL_INFO) --remove --info-dir="$(INFO_DIR)" $$i;	\
	done
	cd "$(INFO_DIR)" && rm -rf $(ALL_INFO_FILES) $(PROJECT)-$(USR_MAN)-data

info: $(INFO_FILES)
pdf: logo.png $(PDF_FILES)
html: $(HTML_DIRS)
dvi: logo.eps $(DVI_FILES)
ps: logo.eps $(PS_FILES)

LOGO_INSERT := 's/^\@author/\@vskip 2cm\n\@image\{logo,16cm\}\n\@author/'

$(PROJECT)-$(USR_MAN).info: $(TXT_USR_MAN_DEPS:%=$(PROJECT)-$(USR_MAN)-data/%) \
			    $(PNG_USR_MAN_DEPS:%=$(PROJECT)-$(USR_MAN)-data/%)
$(PROJECT)-%.info: %.texi
	$(MAKEINFO) $<

$(USR_MAN).pdf: $(PNG_USR_MAN_DEPS)
%.pdf: %.texi
	$(MAKEINFO) --pdf $<

$(USR_MAN): $(PNG_USR_MAN_DEPS)
$(HTML_DIRS): %: %.texi
	$(MAKEINFO) --css-include=$(CSS).css --html -o $@ $<
	-test "$@" = "$(USR_MAN)" && cp $(PNG_USR_MAN_DEPS) $@/
	touch $@

$(USR_MAN).dvi: $(EPS_USR_MAN_DEPS)
%.dvi: %.texi
	$(MAKEINFO) --dvi $<

$(USR_MAN).ps: $(EPS_USR_MAN_DEPS)
%.ps: %.texi
	$(MAKEINFO) --ps $<

%.png: %.dot
	$(DOT) -Tpng -o $@ $<

$(PROJECT)-$(USR_MAN)-data/%.png: %.dot
	$(DOT) -Tpng -o $@ $<

$(PROJECT)-$(USR_MAN)-data/%.txt: %.dot
	$(GRAPH_EASY) $< $@

%.eps: %.dot
	$(DOT) -Teps -o $@ $<

%.eps: %.png
	$(CONVERT) $< $@

clean:
	-rm *.tmp *.fn* *.vr* *.ky* *.tp* *.cp* *.pg *.toc *.aux *.log *~

distclean: clean
	-rm -fr logo.eps						\
		$(HTML_DIRS)						\
		$(ALL_INFO_FILES) $(PDF_FILES) $(DVI_FILES) $(PS_FILES) \
		*.eps $(DOT_FIGURES:%=%.txt) $(DOT_FIGURES:%=%.png)	\
		$(DOT_FIGURES:%=$(PROJECT)-$(USR_MAN)-data/%.png)	\
		$(DOT_FIGURES:%=$(PROJECT)-$(USR_MAN)-data/%.txt)

$(USR_MAN:%=%.texi): $(TOP_DIR)/make/version.make
	$(PERL) -pi -e 's/@set VERSION .*/@set VERSION $(LONG_VERSION)/' $@
	$(PERL) -pi -e 's/@set COPYRIGHT_YEARS .*/@set COPYRIGHT_YEARS $(TYPESET_COPYRIGHT_YEARS)/' $@

localref:
	$(MAKE) generate GENERATE_FLAGS=--locations

generate:
	$(SBCL_PATH) $(EVAL_CONFIG) $(SBCL_LOAD) ./generate.cl $(GENERATE_FLAGS)
	$(PERL) -pi -e $(LOGO_INSERT) $(REF_MAN:%=%.texi)

install-www: $(PDF_FILES) $(HTML_DIRS)
	$(MAKE) install-at LOCATION="$(W3DIR)"

install-net: $(PDF_FILES)
	for i in $(PDF_FILES); do				\
	  install -m 644 $$i $(NET_DIR)/$(PROJECT)-$$i;	\
	done

include $(TOP_DIR)/make/epilogue.make

.PHONY: all all-formats		\
	install-at install uninstall	\
	info pdf html dvi ps		\
	localref generate		\
	clean distclean		\
	install-www install-net

### Makefile ends here
