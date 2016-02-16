################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     MICROBLAZEMethods.py
# @brief    This file is part of the TRAP example processors.
# @details  Instruction helper methods for the Microblaze.
# @author   Luca Fossati
# @date     2008-2013 Luca Fossati
# @copyright
#
# This file is part of TRAP.
#
# TRAP is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
# or see <http://www.gnu.org/licenses/>.
#
# (c) Luca Fossati, fossati@elet.polimi.it, fossati.l@gmail.com
#
################################################################################


import trap
import cxx_writer

#-------------------------------------------------------
# Miscellaneous operations which can be used and
# accessed by any instruction
#-------------------------------------------------------
# *******
# Here we define some helper methods, which are not directly part of the
# instructions, but which can be called by the instruction body
# *******

opCode = cxx_writer.Code("""
if((bitSeq & (1 << (bitSeq_length - 1))) != 0)
    bitSeq |= (((unsigned)0xFFFFFFFF) << bitSeq_length);
return bitSeq;
""")
SignExtend_method = trap.HelperMethod('SignExtend', opCode, 'execute')
SignExtend_method.setSignature(('BIT<32>'), [('bitSeq', 'BIT<32>'), cxx_writer.Parameter('bitSeq_length', cxx_writer.uintType)])

# PC increment
opCode = cxx_writer.Code("""
if (TARGET == 0xffffffff) {
	PC = PC + 4;
	DSFLAG = 0x0;
} else {
	PC = TARGET;
	TARGET = 0xffffffff;
	DSFLAG = 0x1;
}
""")
IncrementPC = trap.HelperOperation('IncrementPC', opCode, inline = False)

opCode = cxx_writer.Code("""
	if ( IMMREG & 0x80000000 ) {  /* IMM instruction */
		imm_value = ((int)imm & 0x0000ffff) + (int)(IMMREG << 16);
		IMMREG &= 0x7fffffff;
	} else {	/* No IMM instruction */
		imm_value = (int)SignExtend(imm,16);
	} 
	
	
""")
IMM_handler = trap.HelperOperation('IMM_handler', opCode)
IMM_handler.addInstructionVar(('imm_value', 'BIT<32>'))
IMM_handler.addUserInstructionElement('imm')

opCode = cxx_writer.Code("""
	IMMREG &= 0x7fffffff;
""")
IMM_reset = trap.HelperOperation('IMM_reset', opCode)

opCode = cxx_writer.Code("""
	ESR[key_DS] = DSFLAG ? 0x1 : 0x0;
	if ( ESR[key_DS] ) {
		BTR = PC; /* In this moment, TARGET value is in PC */
		GPR[17] = 0xffffffff;
	} else {
		GPR[17] = PC; /* In this moment, PC points to the NEXT instruction */		
	}
	PC = 0x00000020;
	MSR[key_EE] = 0x0; MSR[key_EIP] = 0x1;
	MSR[key_UMS] = MSR[key_UM]; MSR[key_UM] = 0x0;
	MSR[key_VMS] = MSR[key_VMS]; MSR[key_VM] = 0x0;
	
	ESR[key_EC] = 0x1;
	ESR[key_W] = W_value;
	ESR[key_S] = S_value;
	ESR[key_Rx] = rd_bit_value; /* the value that identifies rd */
	EAR = addr;
""")
handleMemoryException_method = trap.HelperMethod('handleMemoryException', opCode, 'execute')
handleMemoryException_method.setSignature(parameters = [cxx_writer.Parameter('W_value', cxx_writer.uintType), cxx_writer.Parameter('S_value', cxx_writer.uintType), cxx_writer.Parameter('rd_bit_value', cxx_writer.uintType), cxx_writer.Parameter('addr', cxx_writer.uintType)])

opCode = cxx_writer.Code("""
	ESR[key_DS] = DSFLAG ? 0x1 : 0x0;
	if ( ESR[key_DS] ) {
		BTR = PC; /* In this moment, TARGET value is in PC */
		GPR[17] = 0xffffffff;
	} else {
		GPR[17] = PC; /* In this moment, PC points to the NEXT instruction */		
	}
	PC = 0x00000020;
	MSR[key_EE] = 0x0; MSR[key_EIP] = 0x1;
	MSR[key_UMS] = MSR[key_UM]; MSR[key_UM] = 0x0;
	MSR[key_VMS] = MSR[key_VMS]; MSR[key_VM] = 0x0;
	
	ESR[key_EC] = 0x1c;
""")
handleUserPermissionException_method = trap.HelperMethod('handleUserPermissionException', opCode, 'execute')
