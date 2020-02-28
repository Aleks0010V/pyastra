############################################################################
# $Id$
#
# Description: pic14 specific built-ins. Pyastra project.
# Author: Alex Ziranov <estyler _at_ users _dot_ sourceforge _dot_ net>
#    
# Copyright (c) 2004 Alex Ziranov.  All rights reserved.
#
############################################################################
"""
Pic14 specific built-ins. U{Pyastra project <http://pyastra.sourceforge.net>}.

This module contains built-in functions and is imported automatically
during compilation. Usually are written in assembler.

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
@todo: Bank selection before variable usage
"""

def lshift(lshift_left, lshift_right):
    """
    Does the left shift (C{<<}) operation. Puts the result in the C{W}
    register.

    @param lshift_left: Left operand.
    @param lshift_right: Right operand.
    """
    asm("""
    movf   _lshift_left,    w
	movf	_lshift_right,	f
	btfsc	STATUS,	Z
	goto	lshift_label_end
lshift_label_beg
    bcf     STATUS, C
	rlf	_lshift_left,	f
	decfsz	_lshift_right,	f
	goto	lshift_label_beg
	movf	_lshift_left,	w
lshift_label_end
""")

def rshift(rshift_left, rshift_right):
    """
    Does the right shift (C{>>}) operation. Puts the result in the C{W}
    register.

    @param rshift_left: Left operand.
    @param rshift_right: Right operand.
    """
    asm("""
    movf   _rshift_left,    w
	movf	_rshift_right,	f
	btfsc	STATUS,	Z
	goto	rshift_label_end
rshift_label_beg
    bcf     STATUS, C
	rrf	_rshift_left,	f
	decfsz	_rshift_right,	f
	goto	rshift_label_beg
	movf	_rshift_left,	w
rshift_label_end
""")

def mul(mul_left, mul_right):
    """
    Multiplies opearands and puts the result in the C{W} register.

    @param mul_left: Left multiplier.
    @param mul_right: Right multiplier.
    """
    asm("""
	clrf	mul_res
	movlw	.8
	movwf	mul_cntr
	movf	_mul_left,	w
mul_beg
	rlf	mul_res,	f
	rlf	_mul_right,	f
	btfsc	STATUS,	C
	addwf	mul_res,	f
	decfsz	mul_cntr,	f
	goto	mul_beg
	movf	mul_res,	w
""")

def div(div_left, div_right):
    """
    Diviedes opearands and puts the quotient in the C{W} register.

    @param div_left: Dividend.
    @param div_right: Divisor.
    """
    asm("""
	clrf	div_buf
	movlw	.8
	movwf	div_cntr
div_beg
	rlf	_div_left,	f
	rlf	div_buf,	f
	movf	_div_right,	w
	subwf	div_buf,	w
	btfss	STATUS,	C
	goto	div_cont
	
	movwf	div_buf
	bsf	STATUS,	C
	
div_cont
	rlf	div_res,	f
	decfsz	div_cntr,	f
	goto	div_beg
	
	movf	div_res,	w
""")

def mod(mod_left, mod_right):
    """
    Diviedes opearands and puts the reminder in the C{W} register.

    @param mod_left: Dividend.
    @param mod_right: Divisor.
    """
    asm("""
	clrf	mod_buf
	movlw	.8
	movwf	mod_cntr
mod_beg
	rlf	_mod_left,	f
	rlf	mod_buf,	f
	movf	_mod_right,	w
	subwf	mod_buf,	w
	btfss	STATUS,	C
	goto	mod_cont
	
	movwf	mod_buf
	
mod_cont
	decfsz	mod_cntr,	f
	goto	mod_beg
	
	movf	mod_buf,	w
""")

def power(pow_left, pow_right):
    """
    Raises the base to the exponent and puts the result to the C{W} register.

    @param pow_left: Base.
    @param pow_right: Exponent.
    """
    asm("""
    movf	_pow_right,	f
	btfss	STATUS,	C
	goto	pow_cont
	movlw	.1
	goto	pow_end
	
pow_cont
	movf	_pow_right,	w
	sublw	.1
	btfss	STATUS,	Z
	goto pow_cont1
	movf	_pow_left,	w
	goto	pow_end
	
pow_cont1
	movf	_pow_left,	w
	movwf	pow_res
pow_beg0
	movf	pow_res,	w
	movwf	pow_buf
	
	clrf	pow_res
	movlw	.8
	movwf	pow_cntr
	movf	_pow_left,	w
pow_beg
	rlf	pow_res,	f
	rlf	pow_buf,	f
	btfsc	STATUS,	C
	addwf	pow_res,	f
	decfsz	pow_cntr,	f
	goto	pow_beg
	
	decfsz	_pow_right,	f
	goto	pow_beg0
	
	movf	pow_res,	w
pow_end
""")
