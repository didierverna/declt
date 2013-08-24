### Makefile --- Source directory

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

TOP_DIR := ..

include $(TOP_DIR)/Makefile.cnf

hack: all

include $(TOP_DIR)/Makefile.inc


all:

clean:
	rm -f *~

distclean: clean
	rm -fr $($(LISP)_BINLOC)-*	\
	       */$($(LISP)_BINLOC)-*	\
	       */*/$($(LISP)_BINLOC)-*
install:
uninstall:
www-dist:

.PHONY: hack all clean distclean install uninstall www-dist


### Makefile ends here
