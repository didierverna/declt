### Makefile.inc --- Inclusion part

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

PROJECT   := declt
PACKAGE   := com.dvlsoft.$(PROJECT)
ASDF_FILE := $(PACKAGE).asd

PERL := perl

SHARE := $(PREFIX)/share

WWW_HOST := www
WWW_DIR  := ~/www/software/lisp/$(PROJECT)


SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_LOAD   := --load

$(TOP_DIR)/version.inc: $(TOP_DIR)/version.cl $(TOP_DIR)/$(ASDF_FILE)
	$($(LISP)_PATH) $($(LISP)_LOAD) $(TOP_DIR)/version.cl | tail -2 > $@


### Makefile.inc ends here
