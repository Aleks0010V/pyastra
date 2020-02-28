###########################################################################
# $Id$
#
# Description: Pyastra modules. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
###########################################################################

"""
Main package of U{Pyastra project <http://pyastra.sourceforge.net>}.

U{Pyastra<http://pyastra.sourceforge.net>} is a python to assembler
translator. It takes source file written in python and, if the code
doesn't contain any errors, generates an assembler file.

Goals
=====
  - to bring a python translator to the world of microcontrollers
  - to support a wide range of microcontrollers and processors
  - to generate compact and effective code
  - to be developer- and user-friendly

Architecture
============
  Pyastra is divided into two layers: back-ends and front-ends. They
  connect through the L{pyastra} module.
  
  Front-ends
  ----------
    At the moment exists only one command-line pyastra front-end. Other
    front-ends (like GUI and web front-ends or modules to some IDEs or
    editors) can be easily created by using pyastra package to stir up
    the back-end. The simplest front-end creates an instance of the
    L{Pyastra} class, calls L{Pyastra.convert()} method and saves
    the result::
      
      import pyastra
      
      in_file  = 'test.py'
      out_file = 'test.asm'
      
      p = Pyastra(in_file)
      results   = p.convert()
      f = file(out_file)
      f.write(results[0].data)
      f.close()
      
  Back-ends
  ---------
    All Pyastra back-ends are L{converters}. To plug in new convertor
    into the chain you just need to add a file to the converters folder.
    See L{converters} package documentation for details.
        
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
@var version: String representing Pyastra version.
@var MESSAGE: Message level constant for C{say()} method.
@var WARNING: Warning level constant for C{say()} method.
@var ERROR:   Error level constant for C{say()} method.

@todo: Test the library.
"""

__all__ = ['ports', 'modules', 'convertors', 'basic_tree2ol']

from Pyastra.lib import converters

revision = '$Revision$'[11:-2]
version = 'pyastra 0.0.5-prerelease (rev %s)' % revision
MESSAGE = 0
WARNING = 1
ERROR = 2


def get_ports():
    """
    Scans converters for supported ports.
    @return: Ports supported by the current Pyastra configuration.
    @rtype:  C{list}
    """
    ports = []
    for mod_n in converters.__all__:
        mod = __import__('pyastra.converters.%s' % mod_n, globals(), locals(), '*')  # ToDo - fix this import
        for port in mod.get_ports():
            if port not in ports:
                ports += [port]

    ports.sort()
    return ports


def get_procs(port):
    """
    Scans converters for supported processors for the given port.
    @param port: Port for which the processors list must be returned.
    @type port:  C{str}
    @return: Processors supported by the current Pyastra configuration.
    @rtype:  C{list}
    """
    procs = []
    for mod_n in converters.__all__:
        mod = __import__('pyastra.converters.%s' % mod_n, globals(), locals(), '*')  # ToDo - fix this import
        for proc in mod.get_procs(port):
            if proc not in procs:
                procs += [proc]

    procs.sort()
    return procs


def get_options():
    """
    Scans converters for port-specific options.
    @return: Port-specific options.
    @rtype:  C{dict} of L{Option}s
    """
    opts = {}
    for mod_n in converters.__all__:
        mod = __import__('pyastra.converters.%s' % mod_n, globals(), locals(), '*')
        if hasattr(mod, 'get_options'):
            opts[mod_n] = mod.get_options()
    return opts


def crt_options():
    """
    A front-end helper method that creates a dictionary of options filled
    with default values: blank lines for options that require arguments and
    False values for other.

    The method is useful as initial dictionary that is to be filled by
    user-specified options.
    
    @return: Dictionary of options filled with default values.
    @rtype:  C{dict}
    """
    opts = {}
    for fopt in get_options().values():
        for opt in fopt:
            if opt.arg:
                opts[opt.lkey] = ''
            else:
                opts[opt.lkey] = False
    return opts


def get_merged_metas(nodes):
    """
    Merges metas of all given nodes.
    
    B{Warning:} some data may be lost (if any two of odes have the same
    meta keys).

    @param nodes: Nodes that contain meta to be merged.
    @type  nodes: C{list}
    @return: Merged meta dictionary.
    @rtype:  C{dict}
    """
    meta = {}
    for node in nodes:
        merge_metas(meta, node.meta)
    return meta


def merge_metas(meta1, meta2):
    """
    Copies meta information from meta2 to meta1 if meta1
    doesn't already have some info with such key.

    @param meta1: Meta that will store the result of merging.
    @type  meta1: C{dict}
    @param meta2: Meta that will be left unchanged.
    @type  meta2: C{dict}
    """
    for key in meta2.keys():
        if not key in meta1:
            meta1[key] = meta2[key]


class Pyastra:
    """
    General class to communicate front-ends and back-ends. Represents
    the whole conversion process from request to results and errors.
    
    @ivar errors: Errors counter.
    @type errors: C{int}
    @ivar warnings: Warnings counter.
    @type warnings: C{int}
    @ivar messages: Messages counter.
    @type messages: C{int}
    @ivar caller_say: Function that brings some information to the user.
      Function takes three arguments:
        - B{message} - message text to be displayed.
        - B{level} - message level (L{MESSAGE}, L{WARNING}, L{ERROR}).
        - B{line} - number of line to which the message is related to or
          C{None}.
      Defaults to C{None} in case when no function were specified.
    @type caller_say: C{function}
    @ivar src: Source data (what to convert from).
    @ivar select: Function that asks the user to choose one of the options.
      Takes options list as its argument and returns one option from the
      list. Defaults to C{None} in case when no function were specified.
    @type select: C{function}
    @ivar trg_t: Data types of the targets.
    @type trg_t: C{list} of C{str}
    @ivar src_t: Data type of the L{src}.
    @type src_t: C{str}
    @ivar opts: Options dictionary.
    @type opts: C{dict}
    """

    def __init__(self, src, select=None, say=None, trg_t=['asm_op'], src_t='py', opts=None):
        """
            @param say: Function that brings some information to the user.
            @type say:  C{function}
            @param src: Source data (what to convert from).
            @param select: Function that asks the user to choose one
              of the options.
            @type select:  C{function}
            @param trg_t: Data types of the targets.
            @type trg_t:  C{list} of C{str}
            @param src_t: Data type of the L{src}.
            @type src_t:  C{str}
            @param opts: Options dictionary.
            @type opts:  C{dict}
            """
        global crt_options

        self.errors = 0
        self.warnings = 0
        self.messages = 0

        if not opts:
            opts = crt_options()
        opts['caller_select'] = select
        opts['pyastra'] = self
        self.caller_say = say
        opts.setdefault('debug', False)
        self.src = src
        self.trg_t = trg_t
        self.src_t = src_t
        self.opts = opts

    def convert(self):
        """
        Starts the conversion and returns only after the conversion
        is finished.
        
        @return: Convertors of successfully achieved targets.
        @rtype:  C{list}
        
        @todo: Replace exception strings with objects and write the @raise
          field.
        """
        b = Branch(self.src, self.opts['caller_select'], self.trg_t,
                   self.src_t, self.opts)

        if b.status == Branch.BROKEN:
            raise "Can't convert %s to %s." % (self.src_t, self.trg_t)
        else:
            result = b.get_trg()
            if b.status == Branch.PARTIALLY_RESOLVED:
                print("The following targets weren't achieved:")
                for trg in self.trg_t:
                    if trg not in result.keys():
                        print(f"    {trg}")
            ret = []
            for val in result.values():
                ret += val
            return ret

    def say(self, message, level=MESSAGE, line=None):
        """
        Counts messages of every level and calls the L{caller_say} method
        if it's not C{None}.
        
        @param message: Message text to be displayed.
        @type  message: C{str}
        @param level: Message level (L{MESSAGE}, L{WARNING}, L{ERROR}).
        @type  level: C{int}
        @param line: Number of line to which the message is related to or
          C{None}.
        @type  line: C{int}

        @todo: Add module name to all messages.
        """
        global MESSAGE, WARNING, ERROR
        if level == ERROR:
            self.errors += 1
        elif level == WARNING:
            self.warnings += 1
        else:
            self.messages += 1

        if self.caller_say:
            self.caller_say(message, level, line)


class Option:
    """Represents an option that may be pushed to a convertor.
       @ivar descr: description of the option
       @ivar lkey:  long key name
       @ivar skey:  short key name (is an arbitrary argument)
       @ivar arg:   boolean that describes whether the option needs
                    an argument or it doesn't (defaults to False)
    """

    def __init__(self, descr, lkey, skey=None, arg=False):
        """
         @param descr: description of the option
         @param lkey:  long key name
         @param skey:  short key name (is an arbitrary argument)
         @param arg:   boolean that describes whether the option needs
                       an argument or it doesn't (defaults to False)
        """
        self.descr = descr
        self.skey = skey
        self.lkey = lkey
        self.arg = arg


class Branch:
    """
    Represents a branch of converters tree. At every stage pyastra scans
    all converters if any of them has C{from_to} of the curren source.
    If multiple converters support this type, branch will fork.
    
    @cvar BROKEN: Constant that marks a branch as broken.
    @cvar ASK_USER: Constant that marks a branch as a fork that can't be
      resolved without user. Forks are resolved by using the L{select()}
      funcion.
    @cvar PARTIALLY_RESOLVED: Constant that marks a branch as a branch
      that has generated data of some target types, but not of all.
    @cvar RESOLVED: Constant that marks a branch as a branch that has
      generated data of all required target types.
    
    @ivar status: Denotes current status of the branch (one of L{BROKEN},
      L{ASK_USER}, L{PARTIALLY_RESOLVED}, L{RESOLVED}).
    @type status: C{int}
    @ivar subnodes: Subnodes of current branch. Target types are
      used as keys. Special key "branches" is used for temporarily
      subbranches storage.
    @type subnodes: C{dict}
    @ivar select: Function that asks the user to choose one of the options.
    @type select: C{function}
    
    @todo: Add support of optimizators.
    """
    BROKEN = 0
    ASK_USER = 1
    PARTIALLY_RESOLVED = 2
    RESOLVED = 3

    def __init__(self, src, select, trg_t, src_t, opts):
        """
        @param src: Source data.
        @param select: Function that asks the user to choose one of the options.
        @type  select: C{function}
        @param trg_t: Data types of the targets.
        @type  trg_t: C{list} of C{str}
        @param src_t: Data type of the L{src}.
        @type  src_t: C{str}
        @param opts: Options dictionary.
        @type  opts: C{dict}
        """
        self.status = None
        self.subnodes = {}
        self.select = select
        for mod_n in converters.__all__:
            mod = __import__(f'pyastra.converters.{mod_n}', globals(), locals(), '*')  # ToDo - fix this import
            if mod.converts_from == src_t:
                if opts['debug']:
                    print(f'{mod_n} {{')
                conv = mod.Convertor(src, opts)
                if opts['debug']:
                    print('  meta:', conv.meta)
                if conv.modified:
                    if mod.converts_to in trg_t:
                        self.add_subnode(conv, mod.converts_to)

                        if len(trg_t) > 1:
                            n_trg = filter(lambda x: x != mod.converts_to, trg_t)
                            self.fork(conv.data, select, n_trg, mod.converts_to, opts)
                    else:
                        self.fork(conv.data, select, trg_t, mod.converts_to, opts, conv.meta)
                if opts['debug']:
                    print('}')

        if len(self.subnodes) == 0:
            self.status = Branch.BROKEN
        elif 'branches' in self.subnodes:
            self.status = Branch.ASK_USER
        else:
            self.status = Branch.RESOLVED
            for trg in trg_t:
                if trg in self.subnodes:
                    if len(self.subnodes[trg]) > 1:
                        self.status = Branch.ASK_USER
                        break
                else:
                    self.status = Branch.PARTIALLY_RESOLVED

        if opts['debug']:
            print('BROKEN', 'ASK_USER', 'PARTIALLY_RESOLVED', 'RESOLVED', [self.status])

    def fork(self, src, select, trg_t, src_t, opts, meta=None):
        """
        Forks the current branch and adds its subnodes to current branch's
        subnodes.

        @param src: Source data.
        @param select: Function that asks the user to choose one of the options.
        @type  select: C{function}
        @param trg_t: Data types of the targets.
        @type  trg_t: C{list} of C{str}
        @param src_t: Data type of the L{src}.
        @type  src_t: C{str}
        @param opts: Options dictionary.
        @type  opts: C{dict}
        @param meta: Meta of the subnode to be used as the meta of the fork.
        @type  meta: C{str}
        """
        b = Branch(src, select, trg_t, src_t, opts)

        if b.status == Branch.ASK_USER:
            self.add_subnode(b, 'branches', meta=meta)
        elif b.status >= Branch.PARTIALLY_RESOLVED:
            self.add_subnode(b.subnodes, meta=meta)

    def add_subnode(self, subnode, trg_t=None, meta=None):
        """
        Adds a subnode to current branch. Supports items, lists, tuples.
        Also other kinds of subnode-dicts merging is supported.
        
        @param subnode: Subnode to be added. Can be a L{Pyastra},
          dictionary of Convertors (target types are keys), list of
          Convertors 
        @type  subnode: C{Pyastra}, C{list}, C{tuple} or C{dict}
        @param trg_t: Type of the achieved target.
        @type  trg_t: C{str}
        @param meta: Meta of the subnode.
        @type  meta: C{str}
        """
        global merge_metas

        if isinstance(subnode, dict):
            for key in subnode.keys():
                self.add_subnode(subnode[key], key, meta)
        else:
            t = self.subnodes.setdefault(trg_t, [])
            if isinstance(subnode, list) or isinstance(subnode, tuple):
                if meta:
                    for node in subnode:
                        merge_metas(node.meta, meta)
                t += subnode
            else:
                if meta:
                    merge_metas(subnode.meta, meta)
                t += [subnode]

    def get_trg(self):
        """
        Resolves the branch.
        
        @return: Convertors of successfully achieved targets.
        @rtype:  C{dict}
        """
        global merge_metas  # ToDo - find this variable
        assert (self.status != Branch.BROKEN)

        if self.status == Branch.ASK_USER:
            if 'branches' in self.subnodes:
                for b in self.subnodes['branches']:
                    self.add_subnode(b.get_trg())

                del self.subnodes['branches']

            for key in self.subnodes:
                if len(self.subnodes[key]) > 1:
                    if self.select:
                        self.subnodes[key] = self.select(self.subnodes[key])
                    else:
                        self.subnodes[key] = self.subnodes[key][0]

        tmeta = {}
        for key in self.subnodes:
            merge_metas(tmeta, self.subnodes[key][0].meta)

        for key in self.subnodes:
            merge_metas(self.subnodes[key][0].meta, tmeta)

        return self.subnodes
