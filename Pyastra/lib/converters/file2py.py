############################################################################
# $Id$
#
# Description: File to python convertor. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################
"""
File to python convertor.
U{Pyastra project <http://pyastra.sourceforge.net>}.

Takes a file name as C{src} and returns contents of the file as string.

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
@see: L{converters}
"""

converts_from = 'file'
converts_to = 'py'


def get_ports():
    """@return: A list of supported ports."""
    return []


def get_procs(port):
    """@return: A list of supported processors."""
    return []


class Converter:
    """
    Main convertor class
    @see: L{converters}
    """
    modified = True
    meta = {}

    def __init__(self, src, opts):
        f = open(src)
        self.data = f.read()
        f.close()
