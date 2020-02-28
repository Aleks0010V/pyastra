############################################################################
# $Id$
#
# Description: pic14 assembler to hex convertor. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################
"""
Pic14 assembler to hex convertor.
U{Pyastra project <http://pyastra.sourceforge.net>}.

Saves an assembler file in a temporary folder and calls an external assembler.
The result of assembly is saved in C{data} variable.

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

import os
import os.path
import Pyastra as pyastra

converts_from = 'asm'
converts_to = 'bin'


def get_ports():
    """@return: A list of supported ports."""
    return []


def get_procs(port):
    """@return: A list of supported processors."""
    return []


class Converter:
    """
    Main convertor class
    @cvar ASSEMBLERS: Tuples of supported assemblers. First item of every
      tuple specifies the file name of the assembler and the second one
      specifies default arguments to be appended to the command.
    @type ASSEMBLERS: C{tuple}
    @see: L{converters}
    """
    ext = 'hex'
    ASSEMBLERS = (('gpasm', ''), ('gpasm.exe', ''), ('mpasm.exe', ''), ('mpasmwin.exe', ''),)
    meta = {}

    def __init__(self, src, opts):
        say = opts['pyastra'].say
        self.modified = False
        infile = opts.get('infile')
        if infile:
            asm_fn = os.path.splitext(infile)[0]
        else:
            try:
                asm_fn = self.tmp_asm()
            except:
                say("Can't find appropriate temporary file name!", level=pyastra.lib.ERROR)
                return
        asm_file = open(asm_fn + '.asm', 'w')
        asm_file.write(src)
        asm_file.close()

        if 'PATH' in os.environ:
            paths = os.environ['PATH']
        else:
            paths = os.path.defpath

        asm_path = None
        for path in paths.split(os.pathsep):
            if path[0] == path[-1] and path[0] in ('"', '\''):
                path = path[1:-1]
            for fn, args in self.ASSEMBLERS:
                if os.access(os.path.join(path, fn), os.X_OK):
                    asm_path = os.path.join(path, fn)
                    asm = fn
                    asm_args = args
                    break

            if asm_path:
                break

        if asm_path:
            say(f'Assembling {asm_fn}.asm with {asm}...')
            if os.system(f'"{asm_path}" {asm_fn}.asm {asm_args}'):
                say('Assembling failed.', level=pyastra.lib.ERROR)
                return
            hex_file = open(asm_fn + '.hex')
            self.data = hex_file.read()
            hex_file.close()
            self.modified = True
        else:
            msg = 'No assembler found. Supported assemblers are:\n'
            for a in self.ASSEMBLERS:
                msg += f'    {a[0]}\n'

            say(msg, level=pyastra.lib.ERROR)

        try:
            os.remove(asm_fn + '.asm')
            os.remove(asm_fn + '.hex')
        except OSError:
            pass

    def tmp_asm(self):
        """
        @return: Name of temporary file to be created.
        @rtype: C{str}
        """
        for i in range(100000):
            if not (os.path.exists('tmp%05i.asm' % i) or os.path.exists('tmp%05i.hex' % i)):
                return 'tmp%05i' % i
        raise Exception("Can't find appropriate temporary file name!")
