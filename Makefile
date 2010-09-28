### Makefile --- Toplevel directory

## Copyright (C) 2010 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun Sep 19 21:15:21 2010
## Last Revision: Sun Sep 19 21:15:38 2010

## This file is part of Declt

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

## Please use GNU Make with this makefile.


### Code:

TOP_DIR := .

include Makefile.cnf

all:
	$(MAKE) gen TARGET=all

include Makefile.inc
include version.inc

SUBDIRS     := src doc
SYSTEMS_DIR := $(SHARE)/common-lisp/systems
ASDF_FILE   := com.dvlsoft.declt.asd

DIST_NAME := declt-$(SHORT_VERSION)
TARBALL   := $(DIST_NAME).tar.gz
SIGNATURE := $(TARBALL).asc


install:
	ln -fs "`pwd`/$(ASDF_FILE)" "$(SYSTEMS_DIR)/"
	$(MAKE) gen TARGET=install

uninstall:
	-rm -f "$(SYSTEMS_DIR)/$(ASDF_FILE)"
	$(MAKE) gen TARGET=uninstall

clean:
	-rm *~
	$(MAKE) gen TARGET=clean

# #### NOTE: propagate to the subdirs first, otherwise, version.inc will keep
# on being reconstructed.
distclean: clean
	$(MAKE) gen TARGET=distclean
	-rm *.tar.gz *.tar.gz.asc
	-rm -fr version.inc
	-rm -fr sbcl-* # for ASDF Binary Locations
	-rm -fr "${HOME}"/.cache/common-lisp/sbcl-*"`pwd`" # for ASDF 2

tag:
	git tag -a -m 'Version $(LONG_VERSION)' 'version-$(SHORT_VERSION)'

tar: $(TARBALL)
gpg: $(SIGNATURE)
dist: tar gpg

install-www: dist
	-install -m 644 $(TARBALL)   "$(W3DIR)/attic/"
	-install -m 644 $(SIGNATURE) "$(W3DIR)/attic/"
	echo "\
<? lref (\"declt/attic/declt-$(SHORT_VERSION).tar.gz\", \
	 contents (\"Dernière version\", \"Latest version\")); ?> \
| \
<? lref (\"declt/attic/declt-$(SHORT_VERSION).tar.gz.asc\", \
	 contents (\"Signature GPG\", \"GPG Signature\")); ?>" \
	  > "$(W3DIR)/latest.txt"
	chmod 644 "$(W3DIR)/latest.txt"
	$(MAKE) gen TARGET=install-www
	git push --mirror "$(W3DIR)/declt.git"
	cd "$(W3DIR)"					\
	  && ln -fs attic/$(TARBALL) latest.tar.gz	\
	  && ln -fs attic/$(SIGNATURE) latest.tar.gz.asc

gen:
	@for i in $(SUBDIRS) ; do                 \
	   echo "making $(TARGET) in $${i} ..." ; \
	   ( cd $${i} && $(MAKE) $(TARGET) ) ;    \
	 done

$(TARBALL):
	git archive --format=tar --prefix=$(DIST_NAME)/	\
	    --worktree-attributes HEAD			\
	  | gzip -c > $@

$(SIGNATURE): $(TARBALL)
	gpg -b -a $<

.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: all			\
	install uninstall	\
	clean distclean		\
	tag			\
	tar gpg dist		\
	install-www		\
	gen


### Makefile ends here
