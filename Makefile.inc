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

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/www/software/lisp/declt

SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_EXEC   := --script

#CMUCL_CACHE  := cmu
#CMUCL_BINLOC := cmu
#CMUCL_EXEC   := -load

#CCL_CACHE  := ccl
#CCL_BINLOC := openmcl
#CCL_EXEC   := --load

#ECL_CACHE  := ecl
#ECL_BINLOC := ecl
#ECL_EXEC   := -load

#CLISP_CACHE  := clisp
#CLISP_BINLOC := clisp
#CLISP_EXEC   := -i

#ABCL_CACHE  := abcl
#ABCL_BINLOC := abcl
#ABCL_EXEC   := --load

$(TOP_DIR)/version.inc: $(TOP_DIR)/version.cl \
			$(TOP_DIR)/com.dvlsoft.declt.asd
	$($(LISP)_PATH) $($(LISP)_EXEC) $(TOP_DIR)/version.cl | tail -2 > $@


### Makefile.inc ends here
