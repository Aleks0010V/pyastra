#!/usr/bin/python
############################################################################
# $Id$
#
# Description: inc to python compiler. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################
"""
Inc to python compiler.
U{Pyastra project <http://pyastra.sourceforge.net>}.

Converts C{inc} and C{lkr} files to Pyastra processor definitions.

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
@todo: Shared banks should be added to databanks too (if they aren't already).
@todo: Adapt script to work with pic16 port too.
"""

#
# TODO:
# shared banks should be added to databanks too (if they aren't already)
#

import os
import os.path
import sys

HEADER = """############################################################################
# $""" + 'Id' + """$
#
# Description: PIC %s definition. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################

\"""
Pic %s definition. U{Pyastra project <http://pyastra.sourceforge.net>}.

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
\"""

"""

whites = (' ', '\t')
maxram = 0


def main():
    """Main function of the script."""
    global maxram
    inc_aliases = {
        '12c509a': ('12cr509a',),
        '16c54': ('16c54a', '16c54b', '16c54c', '16c55a', '16c56a',
                  '16c57c', '16c58a', '16c58b', '16cr54', '16cr54a',
                  '16cr54b', '16cr54c', '16cr56a', '16cr57a', '16cr57b',
                  '16cr57c', '16cr58a', '16cr58b',
                  ),
        '16c620a': ('16cr620a',),
        '16f5x': ('16f59',),
    }

    inc_aliases_inv = {}
    for key in inc_aliases:
        for p in inc_aliases[key]:
            inc_aliases_inv[p] = key

    if len(sys.argv) < 3:
        print('usage:\ninc2py.py dir proc1 [proc2 [...]]')
        sys.exit(4)

    proclist = ''
    diry = sys.argv[1]

    for proc_name_ in sys.argv[2:]:
        if proc_name_[-1] == 'x':
            names = []
            for i in os.listdir(os.path.join(diry, 'lkr')):
                ii = os.path.splitext(i)[0]
                if ii[:-1] == proc_name_[:-1]:
                    names.append(ii)
        else:
            names = [proc_name_, ]

        for proc_name in names:
            maxram = 0
            if proc_name[-1] == 'i':
                inc_name = proc_name[:-1]
            else:
                inc_name = proc_name
            inc_name = inc_aliases_inv.get(inc_name, inc_name)
            in_name = os.path.join(diry, 'header', 'p%s.inc' % inc_name)
            in2_name = os.path.join(diry, 'lkr', '%s.lkr' % proc_name)
            out_name = 'pic%s.py' % proc_name

            out = open(out_name, 'wb')

            inp = open(in_name)
            inc2py(inp, out, proc_name)
            inp.close()

            inp = open(in2_name)
            lkr2py(inp, out, proc_name)
            inp.close()

            out.write(f'maxram = {hex(maxram)}')

            out.close()
            proclist += "'%s', " % proc_name

    print(f"Added processors:\n{proclist[:-2]}")


def inc2py(inp, out, proc_name):
    """
    Converts data from an C{inc} file.
    @param inp: Input C{inc} file.
    @type  inp: C{file}
    @param out: Output python file.
    @type  inp: C{file}
    @param proc_name: Procesor name
    @type  proc_name: C{str}
    """
    global maxram
    out.write(HEADER % (proc_name.upper(), proc_name.upper()))
    ch = 1
    ignore = 0
    buf = ['', '', '']
    lmn = 0
    b = {}
    #    bits=0

    while ch:
        ch = inp.read(1)
        if ch == '\n' or not ch:
            if (not ignore) and buf[1] == 'EQU' and buf[0][0] != '_' and not (buf[0] in ('F', 'W')):
                if buf[2][:2] == "H'" and buf[2][-1] == "'":
                    a = int(buf[2][2:-1], 16)
                else:
                    a = int(buf[2])
                if b not in buf:
                    b[buf[0]] = "%s" % (hex(a),)
                    if a > maxram:
                        maxram = a

            ignore = 0
            buf = ['', '', '']
            lmn = 0
        elif not ignore:
            if ch == ';':
                ignore = 1
            elif ch in whites:
                if lmn == 0 and not buf[0]:
                    ignore = 1
                elif buf[lmn]:
                    if lmn < 2:
                        lmn += 1
                    else:
                        ignore = 1
            else:
                buf[lmn] += ch
    #        elif (not bits) and ch=='B':
    #            tmpl='Bits'
    #            while tmpl[0]==ch:
    #                tmpl=tmpl[1:]
    #                if not tmpl:
    #                    bits=1
    #                    break
    #                ch=inp.read(1)

    out.write('hdikt={')

    b = b.items()
    b0 = map(lambda it: it[0], b)
    for i in ('RP0', 'RP1', 'STATUS', 'Z', 'C'):
        if i not in b0:
            print(f"WARNING: {proc_name} doesn't have register or bit {i}")

    for i in range(int(len(b) / 3)):
        if i:
            out.write('       ')
        out.write(
            f"'{b[i * 3][0]}': {b[i * 3][1]}, '{b[i * 3 + 1][0]}': {b[i * 3 + 1][1]}, '{b[i * 3 + 2][0]}': {b[i * 3 + 2][1]},\n")

    out.write('       ')

    for i in range(int((len(b) / 3)) * 3, len(b)):  # ToDo - WTF ??
        out.write("'%s': %s, " % b[i])

    out.write('}\n')


def lkr2py(inp, out, proc_name):
    """
    Converts data from an C{lkr} file.
    @param inp: Input C{lkr} file.
    @type  inp: C{file}
    @param out: Output python file.
    @type  inp: C{file}
    @param proc_name: Procesor name
    @type  proc_name: C{str}
    """
    global maxram
    ch = 1
    ignore = 0
    buf = ['', '', '', '', '']
    lmn = 0
    back = ''
    pages = []
    banks = []
    vectors = None
    shareb = {}

    while ch:
        if back:
            ch = back
            back = ''
        else:
            ch = inp.read(1)

        if ch == '\n' or not ch:
            if not ignore and lmn > 0:
                if buf[0] == 'CODEPAGE':
                    if buf[4] != 'PROTECTED':
                        pages.append((buf[2][6:], buf[3][4:]))
                    elif buf[1] == 'NAME=vectors':
                        vectors = (buf[2][6:], buf[3][4:])
                elif buf[0] == 'DATABANK' or buf[0] == 'ACCESSBANK':
                    if buf[4] != 'PROTECTED':
                        banks.append((buf[2][6:], buf[3][4:]))
                        if eval(buf[3][4:]) > maxram:
                            maxram = eval(buf[3][4:])
                elif buf[0] == 'SHAREBANK':
                    if buf[4] != 'PROTECTED':
                        grp = buf[1][5:]
                        if grp not in shareb:
                            shareb[grp] = []
                        shareb[grp].append((buf[2][6:], buf[3][4:]))
                        if eval(buf[3][4:]) > maxram:
                            maxram = eval(buf[3][4:])
                elif buf[0] not in ('LIBPATH', 'SECTION'):
                    print(f'WARNING: unsupported keyword: {buf[0]}')

            ignore = 0
            buf = ['', '', '', '', '']
            lmn = 0
        elif not ignore:
            if ch == '/':
                ch = inp.read(1)
                if ch == '/':
                    ignore = 1
                else:
                    back = ch
            elif ch in whites:
                if buf[lmn]:
                    lmn += 1
            else:
                buf[lmn] += ch

    for i in shareb.keys():
        new = 1
        for j in shareb[i]:
            for k in banks:
                if j[0] == k[0] and j[1] == k[1]:
                    new -= 1
                elif eval(k[0]) <= eval(j[0]) <= eval(k[1]) or eval(k[0]) <= eval(j[1]) <= eval(k[1]):
                    print(f"FIXME: proc {proc_name}: some of shared banks are subsequences of banks or vice versa")
                    break
        if new == 1:
            banks.append(shareb[i][0])
        elif new < 0:
            print(f"FIXME: proc {proc_name}: some of shared banks are multiply represented in banks.")

    cmp0 = lambda x, y: cmp(eval(x[0]), eval(y[0]))  # ToDo - fix this

    pages.sort(cmp0)
    out.write('\npages = (')
    for i in pages:
        out.write(f'({i[0]}, {i[1]}), ')
    out.write(')\n')

    banks.sort(cmp0)
    out.write('\nbanks = (')
    for i in banks:
        out.write(f'({i[0]}, {i[1]}), ')
    out.write(')\n')

    out.write('\nshareb = (\n')
    for i in shareb.keys():
        out.write('        (')
        for j in shareb[i]:
            out.write(f'({j[0]}, {j[1]}), ')
        out.write('),\n')
    out.write(')\n')
    if vectors:
        out.write(f'\nvectors=({vectors[0]}, {vectors[1]})\n')
    else:
        out.write('\nvectors = None\n')


main()
