############################################################################
# $Id$
#
# Description: Python to tree convertor. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################
"""
Python to tree convertor.
U{Pyastra project <http://pyastra.sourceforge.net>}.

Is based on standard python AST compiler. 
Also py2tree does the following operations:
  - Import modules.
  - Determines whether the program uses interrupts or it doesn't.

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
@todo: Test C{Import} AST node.
@todo: Correct code for case when convert() returns more than one root.
"""

import os.path, compiler, pyastra, pyastra.ports.pic14, pyastra.ports.pic14.modules, pyastra.modules
from compiler.ast import *

converts_from='py'
converts_to='tree'

def get_ports():
    """@return: A list of supported ports."""
    return []

def get_procs(port):
    """@return: A list of supported processors."""
    return []

class Convertor:
    """
    Main convertor class
    @see: L{converters}
    """
    modified = True
    interrupts_on=False
    meta={}
    
    def __init__(self, root, opts):
        self.opts=opts
        self.say=self.opts['pyastra'].say
        self.data=self.scan(compiler.parse(root))
        self.data.interrupts_on=self.interrupts_on
        self.data.src = root

    def scan(self, node, namespace=''):
        """
        Recursively scans the python tree and does the operations.
        @param node: AST node to be scanned.
        @type  node: C{Node}
        @param namespace: Namespace of the given node.
        @type  namespace: C{str}
        """
        if isinstance(node, list):
            ret=[]
            for n in node:
                rnode=self.scan(n, namespace)
                if type(rnode) in (list, tuple):
                    ret += rnode
                else:
                    ret += [rnode]
            return ret
        elif isinstance(node, Assign):
            return node
        elif isinstance(node, From):
            if node.names != [('*', None)]:
                self.say('only "from <module_name> import *" input statement is supported while', ERROR, self.lineno(node))
                return
            root=self._import(node.modname)
            if root.interrupts_on:
                self.interrupts_on = True
            return self.scan(root, namespace)
        elif isinstance(node, Function):
            if node.name == 'on_interrupt':
                self.interrupts_on=True
                return node
            else:
                return node
        elif isinstance(node, Import):
            # TODO: Test it!
            ret=[]
            for name in node.names:
                iname=name[1] or name[0]
                root=self._import(name[0])
                if root.interrupts_on:
                    self.interrupts_on = True
                ret += [self.scan(root, iname)]
            return ret
        elif isinstance(node, Module):
            m=Module(node.doc, self.scan(node.node, namespace))
            m.namespace=namespace
            if hasattr(node, 'src'):
                m.src = node.src
            return m
        elif isinstance(node, Stmt):
            return Stmt(self.scan(node.nodes, namespace))
        else:
            return node

    def _import(self, modname):
        """
        Imports a module into the current namespace.
        @param modname: Module name.
        @type  modname: C{str}
        @return: AST tree of the imported module.
        """
        name='%s.py' % (os.path.sep.join(modname.split('.')))
        if not os.path.exists(name):
            name=os.path.join(pyastra.ports.pic14.modules.__path__[0], name)
        if not os.path.exists(name):
            name=os.path.join(pyastra.modules.__path__[0], name)
        c=pyastra.Pyastra(name, self.opts['caller_select'], self.say,
                trg_t=['tree'], opts=self.opts.copy(), src_t='file')
        # FIXME: Correct code for case when convert() returns more
        #        than one root
        # Fix the same code in tree_op2asm
        return c.convert()[0].data
