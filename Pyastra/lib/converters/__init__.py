############################################################################
# $Id$
#
# Description: Pyastra converters. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2005 Alex Ziranov.  All rights reserved.
#
############################################################################
"""
Pyastra converters. U{Pyastra project <http://pyastra.sourceforge.net>}.

This package contains all converters of Pyastra. You should list a convertor in the C{__all__} dictionary to enable it. Pyastra is shipped with all official modules enabled.

Every convertor must contain:
  - C{converts_from} and C{converts_to} fields that are plain strings
    that define where the convertor should be plugget to.
  - C{get_ports} and C{get_procs} functions that return lists of
    supported ports and processors accordingly.
  - C{Converter} class that:
    - Is initialized by C{__init__(self, src, opts)} method, where
      C{src} is an object returned by the previous convertor and
      C{opts} is a dictionary of options.
    - Has C{meta} dictionary that is may contain some meta information.
      Meta is the information that should be returned to the front-end.
    - Has C{data} field that stores the output of the current convertor
      or a copy of C{src} if it shouldn't be modified.
    - Has C{modified} field that is C{True} if the C{data} field is
      modified and not just a copy of C{src}.
    - Has an optional field C{ext} that specifies a file extension to be used
      when the C{data} is going to be saved by a front-end.
    - Has an optional function C{get_options} that returns options tuple that
      the user may send to the function (See L{pyastra.Option}).

Thus, the simplest "dummy" convertor will be::

  converts_from='tree'
  converts_to='tree_op'

  def get_ports():
      return []

  def get_procs(port):
      return []

  class Converter:
      modified=True
      meta={}
      
      def __init__(self, src, opts):
          self.data = src

Currently the following C{src} and C{data} types are known:
  - B{file} - data is a filename.
  - B{py}   - data is a Python source string.
  - B{tree} - data is an AST tree.
  - B{ol}   - data is an object list.
  - B{asm}  - data is an assembler source string.
  - B{bin}  - data is a string with binary data (usually the latest
    compilation stage).
    
@see: L{pyastra}
@author: U{Alex Ziranov <mailto:estyler_at_users_dot_sourceforge_dot_net>}
@copyright: (C) 2004-2006 Alex Ziranov.  All rights reserved.
@license: This program is free software; you can redistribute it and/or
          modify it under the terms of the GNU General Public License as
          published by the Free Software Foundation; either version 2 of
          the License, or (at your option) any later version.
          
          This program is distributed in the hope that it will be useful,
          but WITHOUT ANY WARRANTY; without even the implied warranty of
          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
          GNU Library General Public License for more details.
          
          You should have received a copy of the GNU General Public
          License along with this program; if not, write to the Free
          Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
          MA 02111-1307, USA.
@contact: U{http://pyastra.sourceforge.net}
"""

__all__=['py2tree', 'tree2ol', 'tree2ol16', 'ol2asm',
         'asm2bin', 'file2py']
