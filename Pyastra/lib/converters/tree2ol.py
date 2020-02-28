############################################################################
# $Id$
#
# Description: pic14 tree to object list convertor. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################
"""
Pic14 tree to object list convertor.
U{Pyastra project <http://pyastra.sourceforge.net>}.


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

from compiler.ast import *
import types
import compiler
import sys
import os.path
import Pyastra.lib.ports.pic14
import Pyastra.lib.ports.pic14.procs
import Pyastra

from Pyastra.lib import Option, MESSAGE, WARNING, ERROR
from Pyastra.lib.basic_tree2ol import *
from Pyastra.lib import basic_tree2ol

converts_from = 'tree'
converts_to = 'ol'
PORTS = ['pic14']


def get_ports():
    """@return: A list of supported ports."""
    return PORTS


def get_procs(port):
    """@return: A list of supported processors."""
    if port in PORTS:
        procs = pyastra.ports.pic14.procs.__all__
        procs = filter(lambda item: item[-1] != 'i', procs)
        return procs
    else:
        return []


def get_options():
    """@return: A tuple of Options that the user may send to the function."""
    return (
               Option('Enable ICD support (disabled by default)', lkey='icd'),
               Option('Include source code in asm file (disabled by default)', lkey='src'),
           ) + BASIC_OPTIONS


class Convertor(BasicTreeConvertor):
    """
    Main convertor class
    @see: L{converters}
    """
    bank_cmds = ('addwf', 'andwf', 'bcf', 'bsf', 'btfsc', 'btfss', 'clrf',
                 'comf', 'decf', 'decfsz', 'incf', 'incfsz', 'iorwf', 'movf',
                 'movwf', 'rlf', 'rrf', 'subwf', 'swapf', 'xorwf')
    cond_br_cmds = ('btfsc', 'btfss', 'decfsz', 'incfsz')
    pagesel_cmds = ('call', 'goto')
    no_bank_cmds = ('addlw', 'andlw', 'call', 'clrw', 'clrwdt', 'goto', 'iorlw',
                    'movlw', 'nop', 'retfie', 'retlw', 'return', 'sleep',
                    'sublw', 'xorlw')
    directives = {'__config': 0, 'banksel': 2, 'pagesel': 2, 'org': 0}
    PORT = 'pic14'
    DEFAULT_PROC = 'pic16f877'

    def __init__(self, src, opts):
        BasicTreeConvertor.__init__(self, src, opts)

    def conv_div(self, node):
        self._convert(Discard(CallFunc(Name('div'), [node.left, node.right], None, None)))

    def conv_inter_start(self):
        if len(self.pages) > 1:
            oa = Pic14AsmObject("""
        movwf   var_w_temp
        swapf   STATUS, W
        movwf   var_status_temp
        movf    PCLATH, W
        movwf   var_pclath_temp
        movf    var_test, W
        movwf   var_test_temp
        bcf     PCLATH, 3
        bcf     PCLATH, 4
        """, verbatim=True)
            oa.pmem = 9
        else:
            oa = Pic14AsmObject("""
        movwf   var_w_temp
        swapf   STATUS, W
        movwf   var_status_temp
        movf    var_test, W
        movwf   var_test_temp
        """, verbatim=True)
            oa.pmem = 5
        oa.force_bank = None
        self.data += [oa]

    def conv_mod(self, node):
        self._convert(Discard(CallFunc(Name('mod'), [node.left, node.right], None, None)))

    def conv_mul(self, node):
        self._convert(Discard(CallFunc(Name('mul'), [node.left, node.right], None, None)))

    def conv_power(self, node):
        self._convert(Discard(CallFunc(Name('pow'), [node.left, node.right], None, None)))

    def get_header(self):
        header = [Pic14AsmObject("""
\terrorlevel\t-302
\terrorlevel\t-306
""", verbatim=True)]

        if self.vectors:
            header += [Pic14AsmObject('org', hex(self.vectors[0]))]
        else:
            header += [Pic14AsmObject('org', '0x0')]

        if self.ICD:
            header += [Pic14AsmObject('nop')]

        if self.pages[0][0] > 0 or self.interr:
            header += [Pic14AsmObject('goto', 'main')]
            if self.interr:
                if len(self.pages) > 1:
                    ao = Pic14AsmObject("""
        movf    var_test_temp,      W
        movwf   var_test
        movf    var_pclath_temp,    W
        movwf   PCLATH
        swapf   var_status_temp,    W
        movwf   STATUS
        swapf   var_w_temp, F
        swapf   var_w_temp, W
        retfie\n""", verbatim=True)
                    ao.pmem = 9
                else:
                    ao = Pic14AsmObject("""
        movf    var_test_temp,      W
        movwf   var_test
        swapf   var_status_temp,    W
        movwf   STATUS
        swapf   var_w_temp, F
        swapf   var_w_temp, W
        retfie\n""", verbatim=True)
                    ao.pmem = 7
                self.interr += [ao]

                header += ([Pic14AsmObject("org", hex(self.vectors[1]))]
                           + self.interr)
            else:
                header += [Pic14AsmObject("org", hex(self.pages[0][0]))]

        return header

    def get_main_footer(self):
        return [Pic14AsmObject('goto', '$')]

    def get_footer(self):
        return [Pic14AsmObject("\n\tend\n", verbatim=True)]

    def ll_addwf(self, reg, dest):
        self.app('addwf', reg, dest)

    def ll_andwf(self, reg, dest):
        self.app('andwf', reg, dest)

    def ll_bcf(self, reg, bit):
        self.app('bcf', reg, bit)

    def ll_bsf(self, reg, bit):
        self.app('bsf', reg, bit)

    def ll_btfsc(self, reg, bit):
        self.app('btfsc', reg, bit)

    def ll_btfss(self, reg, bit):
        self.app('btfss', reg, bit)

    def ll_call(self, label):
        self.app('call', label)

    def ll_clrf(self, reg):
        self.app('clrf', reg)

    def ll_comf(self, reg, dest):
        self.app('comf', reg, dest)

    def ll_goto(self, label):
        self.app('goto', label)

    def ll_incf(self, reg, dest):
        self.app('incf', reg, dest)

    def ll_iorwf(self, reg, dest):
        self.app('iorwf', reg, dest)

    def ll_movf(self, reg, dest):
        self.app('movf', reg, dest)

    def ll_movlw(self, const, fix_test=False):
        self.app('movlw', const, fix_test)

    def ll_movwf(self, reg):
        self.app('movwf', reg)

    def ll_return(self):
        self.app('return')

    def ll_rlf(self, reg, dest):
        self.app('rlf', reg, dest)

    def ll_rrf(self, reg, dest):
        self.app('rrf', reg, dest)

    def ll_sublw(self, const):
        self.app('sublw', const)

    def ll_subwf(self, reg, dest):
        self.app('subwf', reg, dest)

    def ll_xorlw(self, const):
        self.app('xorlw', const)

    def ll_xorwf(self, reg, dest):
        self.app('xorwf', reg, dest)

    def parse_asm(self, asm):
        prev_op = ''
        for s in asm.splitlines(True):
            # FIXME: bad tokenizing in case of "' '" or "','" as constants
            op = []

            for op0 in s.split():
                for op1 in op0.split(','):
                    if op1:
                        op += [op1]
            comment = ''
            for wn in xrange(len(op)):
                if op[wn][0] == ';':
                    comment = s[s.find(op[wn]):]
                    op = op[:wn]
                    break
            if not op:
                self.app(comment, verbatim=True)
            elif op[0].lower() in self.directives:
                self.app(op[0], ' '.join(op[1:]),
                         comment=comment,
                         pmem=self.directives[op[0].lower()])
            elif (len(op) > 3
                  or ((op[0].lower() not in self.bank_cmds)
                      and (op[0].lower()
                           not in self.no_bank_cmds))):
                if s[0].isspace():
                    self.say('Can\'t parse line in asm function:\n%sIncluding the line unparsed.' % s, WARNING)
                    raw = True
                    prev_op = ''
                    force_bank = False
                else:
                    raw = False
                    force_bank = None

                self.app(s, raw=raw, verbatim=True,
                         force_bank=force_bank)
            else:
                op[0] = op[0].lower()

                if (len(op) > 1 and (op[0].lower() in self.bank_cmds
                                     or op[0] in self.no_bank_cmds)
                        and op[0][-1] in 'fzcs'):
                    op[1] = self.get_var(op[1], system=True)

                fix_cond_br = prev_op in self.cond_br_cmds
                comment = comment[1:]
                if len(op) == 1:
                    self.app(op[0], comment=comment,
                             fix_cond_br=fix_cond_br)

                elif len(op) == 2:
                    self.app(op[0], op[1], comment=comment,
                             fix_cond_br=fix_cond_br)
                else:
                    self.app(op[0], op[1], op[2],
                             comment=comment,
                             fix_cond_br=fix_cond_br)

                if op:
                    prev_op = op[0]


class Pic14AsmObject(AsmObject):
    no_bank_cmds = ('addlw', 'andlw', 'call', 'clrw', 'clrwdt', 'goto',
                    'iorlw', 'movlw', 'nop', 'retfie', 'retlw', 'return',
                    'sleep', 'sublw', 'xorlw')
    bank_indep = ('status', 'fsr', 'pclath', 'intcon', 'pcl')
    label_cmds = ('call', 'goto')
    directives = {'__config': 0, 'banksel': 2, 'pagesel': 2, 'org': 0}

    def __init__(self, cmd='', op1=None, op2=None, verbatim=False,
                 comment='', fix_test=False, raw=False, fix_cond_br=False,
                 org_enabled=False):
        AsmObject.__init__(self, cmd, op1, op2, verbatim, comment, fix_test,
                           raw, fix_cond_br, org_enabled=False)

    def finalize(self):
        """
        Generated code and update variables. Raises errors if some needed
        variables are not defined.
        
        @todo: More intelligent verbatim code analyzing.
        @todo: Count verbatim code size.
        @todo: Intelligent page select.
        """
        self.body = ''
        if self.org_enabled:
            self.bank_after = None
        else:
            self.bank_after = self.bank_before

        if self.force_bank != False:
            self.bank_after = self.force_bank

        if self.raw:
            for line in self.cmd.splitlines():
                if line:
                    s = line.split()
                    if s and s[0] != ';':
                        self.bank_after = None
                        break

        if isinstance(self.op1, Label):
            self.op1 = self.op1.get_label()
        elif self.cmd == 'call':
            self.bank_after = None

        if self.verbatim:
            self.body += '%s\n' % self.cmd
        else:
            if self.comment:
                self.comment = '\t;%s' % self.comment

            if (self.op1 and (self.cmd not in self.no_bank_cmds)
                    and (self.cmd.lower() not in self.directives)):
                if self.op1.__class__ == str:
                    self.op1 = self.uvars[self.op1]
                self.body = self.op1.get_bank(self)
                self.op1 = self.op1.name

            if len(self.pages) > 1 and self.op1 and self.cmd == 'call':
                self.body += '\tpagesel %s\n' % self.op1
                self.pmem += 2

            if self.op2 != None:
                self.body += '\t%s\t%s,\t%s' % (self.cmd, self.op1, self.op2)
            elif self.op1 != None:
                self.body += '\t%s\t%s' % (self.cmd, self.op1)
            else:
                self.body += '\t%s' % (self.cmd,)

            if self.cmd.lower() not in self.directives:
                self.pmem += 1

            if len(self.pages) > 1 and self.op1 and self.cmd == 'call':
                self.body += '\n\tpagesel $+1'
                self.pmem += 2

            self.body += self.comment + '\n'

        if self.fix_test and self.fix_test_enabled:
            self.body += self.uvars['var_test'].get_bank(self)
            self.body += '\tmovwf\tvar_test\n'
            self.body += '\tmovf\tvar_test,\tf\n'
            self.pmem += 2

        if self.fix_cond_br and self.pmem > 1:
            lbl_if = Label('ao_if', used=True).get_label()
            lbl_exit = Label('ao_exit', used=True).get_label()

            self.body = (
                    ('\tgoto\t%s\n' % lbl_if)
                    + ('\tgoto\t%s\n' % lbl_exit)
                    + lbl_if + '\n'
                    + self.body
                    + lbl_exit + '\n')

            self.bank_after = None

            self.pmem += 2


class Pic14Label(Label):
    def __init__(self, prefix='label', fix_test=False, used=False):
        Label.__init__(self, prefix, fix_test, used)

    def finalize(self):
        self.bank_after = None

        if self.used:
            self.set_uid()
            self.body = '%s\n' % self.get_label()
            bank_after = None
        else:
            self.body = ''

        if self.fix_test and self.fix_test_enabled:
            self.body += self.uvars['var_test'].get_bank(self)
            self.body += '\tmovwf\tvar_test\n'
            self.body += '\tmovf\tvar_test,\tf\n'
            self.pmem += 2


class Pic14Variable(Variable):
    def __init__(self, name, addr=None, used=False, special=False):
        Variable.__init__(self, name, addr, used, special)

    def get_bank(self, ao):
        """
        Get assembler code that switches banks to the variable bank.
        """
        addr = self.addr
        if self.name in self.hdikt:
            if self.name.lower() in ao.bank_indep:
                return ''
            addr = self.hdikt[self.name]

        bank = addr >> 7
        addr7 = addr & 0x7f

        if ao.maxram < 0x80 or ('RP0' not in self.hdikt):
            return ''

        ret = ''
        if ao.bank_before is not None:
            for block in ao.shareb:
                addr_in_block = False
                curr_block_has = False

                for sub in block:
                    if sub[0] <= addr <= sub[1]:
                        addr_in_block = True

                    if sub[0] <= (addr7 | (sub[0] >> 7 << 7)) <= sub[1]:
                        curr_block_has = True

                if addr_in_block and curr_block_has:
                    return ''
        else:
            for block in ao.shareb:
                addr_in_block = False
                curr_block_has = True

                for sub in block:
                    if sub[0] <= addr7 <= sub[1]:
                        addr_in_block = True

                    if not (sub[0] <= (addr7 | (sub[0] >> 7 << 7)) <= sub[1]):
                        curr_block_has = False

                if addr_in_block:
                    if curr_block_has:
                        return ''
                    else:
                        break

        if ((ao.bank_before == None or (bank & 1) ^ (ao.bank_before & 1))):
            if bank & 1:
                ret += '\tbsf\tSTATUS,\tRP0\n'
            else:
                ret += '\tbcf\tSTATUS,\tRP0\n'
            ao.pmem += 1

        if ((ao.bank_before == None or ((bank & 2) ^ (ao.bank_before & 2)))
                and 'RP1' in self.hdikt):
            if bank & 2:
                ret += '\tbsf\tSTATUS,\tRP1\n'
            else:
                ret += '\tbcf\tSTATUS,\tRP1\n'
            ao.pmem += 1

        ao.bank_after = bank
        return ret


basic_tree2ol.AsmObject = Pic14AsmObject
basic_tree2ol.Label = Pic14Label
basic_tree2ol.Variable = Pic14Variable
