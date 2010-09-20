### Makefile.inc --- Inclusion part

## Copyright (C) 2010 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>
## Created:       Sun Sep 19 21:14:02 2010
## Last Revision: Sun Sep 19 21:15:04 2010

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

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/www/software/lisp/declt


#### FIXME: this breaks the distclean target because version.sh loads declt,
#### which implies recompiling it!!
LONG_VERSION  := $(shell if test -f ./version.sh; then	\
		     ./version.sh | tail -2 | head -1;	\
		   elif test -f ../version.sh; then     \
		     ../version.sh | tail -2 | head -1;	\
		   else                                 \
		     echo "unknown";                    \
		   fi)

SHORT_VERSION := $(shell if test -f ./version.sh; then	\
		     ./version.sh | tail -1;		\
		   elif test -f ../version.sh; then     \
		     ../version.sh | tail -1;		\
		   else                                 \
		     echo "unknown";                    \
		   fi)


### Makefile.inc ends here
