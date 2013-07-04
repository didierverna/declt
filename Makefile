### Makefile --- Toplevel directory

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

## Please use GNU Make with this makefile.


### Code:

TOP_DIR := .

include $(TOP_DIR)/Makefile.cnf

hack: all

include $(TOP_DIR)/Makefile.inc
include $(TOP_DIR)/version.inc


SUBDIRS   := src doc
DIST_NAME := $(PROJECT)-$(SHORT_VERSION)
TARBALL   := $(DIST_NAME).tar.gz
SIGNATURE := $(TARBALL).asc

all:
	$(MAKE) gen TARGET=all
	$(MAKE) INSTALL

all-formats dvi ps ref all-formats-ref dvi-ref ps-ref:
	cd doc && $(MAKE) $@

# Needed because we have an INSTALL file which fucks up the gen mechanism
# (remember that Mac OSX is case-insensitive).
install:
	$(MAKE) gen TARGET=install

uninstall:
	$(MAKE) gen TARGET=uninstall

clean:
	-rm *~
	$(MAKE) gen TARGET=clean

# #### NOTE: be sure to propagate to the subdirs first, otherwise, version.inc
# will keep on being reconstructed.
distclean: clean
	$(MAKE) gen TARGET=distclean
	-rm *.tar.gz *.tar.gz.asc
	-rm -fr version.inc
	-rm -fr $($(LISP)_BINLOC)-*
	-rm -fr "${HOME}"/.cache/common-lisp/$($(LISP)_CACHE)-*"`pwd`"

tag:
	git tag -a -m 'Version $(LONG_VERSION)' 'version-$(SHORT_VERSION)'

tar: $(TARBALL)
gpg: $(SIGNATURE)
dist: tar gpg

install-www: dist
	-$(INSTALL) -m 644 $(TARBALL)   "$(W3DIR)/attic/"
	-$(INSTALL) -m 644 $(SIGNATURE) "$(W3DIR)/attic/"
	echo "\
<? lref (\"$(PROJECT)/attic/$(PROJECT)-$(SHORT_VERSION).tar.gz\", \
	 contents (\"Dernière version\", \"Latest version\")); ?> \
| \
<? lref (\"$(PROJECT)/attic/$(PROJECT)-$(SHORT_VERSION).tar.gz.asc\", \
	 contents (\"Signature GPG\", \"GPG Signature\")); ?>" \
	  > "$(W3DIR)/latest.txt"
	chmod 644 "$(W3DIR)/latest.txt"
	git push --tags "$(W3DIR)/$(PROJECT).git" :
	$(MAKE) gen TARGET=install-www
	cd "$(W3DIR)"					\
	  && ln -fs attic/$(TARBALL) latest.tar.gz	\
	  && ln -fs attic/$(SIGNATURE) latest.tar.gz.asc

update-version:
	cd doc && $(MAKE) $@

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

.PHONY: hack all						\
	all-formats dvi ps ref all-formats-ref dvi-ref ps-ref	\
	install install-ref uninstall				\
	clean distclean					\
	tag tar gpg dist install-www				\
	update-version						\
	gen


### Makefile ends here
