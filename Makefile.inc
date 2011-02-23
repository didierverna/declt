### Makefile.inc --- Inclusion part

## Copyright (C) 2011 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>

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
