#!/usr/bin/env python
############################################################################
# $Id$
#
# Description: Setup script. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################
"""
Setup script. U{Pyastra project <http://pyastra.sourceforge.net>}.

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
@todo: Compile documentation if epydoc is available.
"""

from distutils.core import setup
import sys
VERSION='0.0.5-prerelease'

# patch distutils if it can't cope with the "classifiers" keyword
if sys.version < '2.2.3':
    from distutils.dist import DistributionMetadata
    DistributionMetadata.classifiers = None
    
setup(name='Pyastra',
      version=VERSION,
      description='PYthon to ASsembler TRAnslator',
      author='Alex Ziranov',
      author_email='estyler@users.sourceforge.net',
      url='http://pyastra.sourceforge.net/',
      package_dir = {'pyastra': 'lib'},
      packages=['pyastra', 'pyastra.ports', 'pyastra.ports.pic14', 'pyastra.ports.pic14.modules', 'pyastra.ports.pic14.procs', 'pyastra.ports.pic16', 'pyastra.ports.pic16.modules', 'pyastra.ports.pic16.procs', 'pyastra.converters', 'pyastra.modules'],
      scripts=['pyastra'],
      data_files=[(f'share/doc/pyastra-{VERSION}', ['doc/STATUS.html', 'doc/TODO.html', 'LICENSE', 'NEWS', 'doc/HACKING.html']),
                  (f'share/doc/pyastra-{VERSION}/scripts', ['scripts/inc2py.py', ]),
                  (f'share/doc/pyastra-{VERSION}/examples', ['doc/examples/led_blink.py', 'doc/examples/eeprom.py', 'doc/examples/pic_adc.py', 'doc/examples/binary_clock.py', 'doc/examples/README', 'doc/examples/example1.py', 'doc/examples/example2.py', 'doc/examples/serial.py']),
                  ('share/pixmaps', ['pyastra_48x48.xpm', ]),
                 ],
      license='GPL',
      long_description="""
PYthon to ASsembler TRAnslator. At this moment supports Microchip PIC16
instruction set only. Its main goals are:

* to support a wide range of microcontrollers and processors

* to generate compact and effective code.

*NOTE: this is a preview release. This is NOT intended for general use
(because it's fairly unfinished).*
""",
      classifiers = [
          'Development Status :: 3 - Alpha',
          'Environment :: Console',
          'Intended Audience :: Developers',
          'Intended Audience :: Education',
          'Intended Audience :: Manufacturing',
          'Intended Audience :: Science/Research',
          'License :: OSI Approved :: GNU General Public License (GPL)',
          'Natural Language :: English',
          'Operating System :: OS Independent',
          'Programming Language :: Python',
          'Topic :: Software Development :: Compilers',
          'Natural Language :: English',
      ],
     )
