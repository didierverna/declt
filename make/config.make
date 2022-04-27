### config.make --- Configuration part

## Copyright (C) 2010-2012, 2015, 2021, 2022 Didier Verna

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

## Installation prefix. This is used for installing Declt as follows:
# - $(PREFIX)/share/doc/declt/ for the PDF documentation
# - $(PREFIX)/share/info/ for the info documentation
# If any of these are unsatisfactory, you will need to edit the Makefiles, or
# do the installation by hand.
PREFIX := /usr/local

## Currently, only SBCL is supported.
LISP := SBCL

## Global Common Lisp binary cache location.
BINLOC_CACHE := ${HOME}/.cache/common-lisp

SBCL_PATH  := sbcl

## Programs for generating the documentation:
MAKEINFO     = makeinfo
INSTALL_INFO = install-info
CONVERT      = convert
DOT          = dot
GRAPH_EASY   = graph-easy

### config.make ends here
