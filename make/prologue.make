### prologue.make --- Prologue Makefile

## Copyright (C) 2010-2012, 2015, 2021 Didier Verna

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



### Code:

PROJECT   := declt
PACKAGE   := net.didierverna.$(PROJECT)
ASDF_FILE := $(PACKAGE).asd

TYPESET_COPYRIGHT_YEARS := $(subst -,--,$(COPYRIGHT_YEARS))

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/www/software/lisp/$(PROJECT)

SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_LOAD   := --load
SBCL_EVAL   := --eval
SBCL_DUMP   := $(SBCL_LOAD)

BINLOC := $($(LISP)_BINLOC)

EVAL_CONFIG :=

### proogue.make ends here
