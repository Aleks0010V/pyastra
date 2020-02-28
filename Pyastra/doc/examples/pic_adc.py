############################################################################
# $Id$
#
# Description: PIC's internal ADC driver. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
############################################################################

CLOCK_RC    = 0xc0   #F_rc (clock derived from the internal A/D RC oscillator)
CLOCK_OSC2  = 0x00   #F_osc/2
CLOCK_OSC8  = 0x40   #F_osc/8
CLOCK_OSC32 = 0x80   #F_osc/32
JUSTIFY_RIGHT = 0x80 #right justified. 6 Most Significant bits of ADRESH
                     #are read as '0'
JUSTIFY_LEFT  = 0x00 #left justified. 6 Least Significant bits of ADRESL
                     #are read as '0'

#
# Initializates ADC and switches it on (so you don't need to call PIC_ADC_on())
#
# clock_mask: one of CLOCK* constants.
# justification: one of JUSTIFY* constants
# adc_channel:  0..7 integer that selects analog channel for ADC
# chans_conf: A/D Port Configuration Control bits (see "PICmicro(TM)
#             Mid-Range MCU Family Reference Manual" for details)
#
def PIC_ADC_init(clock_mask, justification, adc_channel, chans_conf):
    ADCON1 = justification | chans_conf
    
    if adc_channel < 5:
        TRISA[adc_channel] = 1
    else:
        TRISE[adc_channel-5] = 1
        
    ADCON0 = clock_mask | adc_channel << 3 | 0x01

def PIC_ADC_off():
    ADCON0[0] = 0

def PIC_ADC_on():
    ADCON0[0] = 1

#
# PIC_ADC_get is planned to return a tuple (resl, resh) or
# 16-bit integer when pyastra will support any of these.
#
# At this moment get() method updates resl and resh variables.
#
def PIC_ADC_get():
    ADCON0[GO]=1
    
##    while ADCON0[GO]:
##        pass
    asm("""
	btfsc ADCON0, GO
	goto  $-1
    """)
    
    PIC_ADC_resl=ADRESL
    PIC_ADC_resh=ADRESH

#
def PIC_ADC_switch(adc_channel):
    #clear old channel
    ADCON0 = ADCON0 & fbin('1100 0111')

    #set new channel
    if adc_channel < 5:
        TRISA[adc_channel] = 1
    else:
        TRISE[adc_channel-5] = 1
        
    ADCON0 = ADCON0 | adc_channel << 3

