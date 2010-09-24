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
	-rm -fr version.inc sbcl-*

tag:
	git tag -a -m 'Version $(LONG_VERSION)' 'version-$(SHORT_VERSION)'

dist:
	git archive --format=tar --prefix=declt-$(SHORT_VERSION)/	\
	    --worktree-attributes HEAD					\
	  | gzip -c > declt-$(SHORT_VERSION).tar.gz

install-www:
	-install -m 644 *.tar.gz "$(W3DIR)/attic/"
	echo '$(LONG_VERSION)' > "$(W3DIR)/version.txt"
	chmod 644 "$(W3DIR)/version.txt"
	echo '<? lref ("declt/attic/declt-$(SHORT_VERSION).tar.gz", contents ("ici", "here")); ?>' \
	  > "$(W3DIR)/current.txt"
	$(MAKE) gen TARGET=install-www

gen:
	@for i in $(SUBDIRS) ; do                 \
	   echo "making $(TARGET) in $${i} ..." ; \
	   ( cd $${i} && $(MAKE) $(TARGET) ) ;    \
	 done

.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: all install uninstall clean tag dist install-www gen


### Makefile ends here
