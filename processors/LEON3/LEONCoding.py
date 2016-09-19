################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     LEONCoding.py
# @brief    This file is part of the TRAP example processors.
# @details  Instruction coding definition file for the LEON3.
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

#---------------------------------------------------------
# Instruction Encoding
#---------------------------------------------------------
# Lets now start with defining the instructions, i.e. their bitstring and
# mnemonic and their behavior. Note the zero* field: it is a special identifier and it
# means that all those bits have value 0; the same applies for one*
# As stated in page 44 of "The SPARC Architecture Manual V8" there are
# mainly 6 different format types

# Call instruction format
call_format = trap.MachineCode([('op', 2), ('disp30', 30)])
call_format.setBitfield('op', [0, 1])

# Branch and sethi instructions format
b_sethi_format1 = trap.MachineCode([('op', 2), ('rd', 5), ('op2', 3), ('imm22', 22)])
b_sethi_format1.setBitfield('op', [0, 0])
b_sethi_format1.setVarField('rd', ('REGS', 0), 'out')
b_sethi_format2 = trap.MachineCode([('op', 2), ('a', 1), ('cond', 4), ('op2', 3), ('disp22', 22)])
b_sethi_format2.setBitfield('op', [0, 0])

# Memory instruction format
mem_format1 = trap.MachineCode([('op', 2), ('rd', 5), ('op3', 6), ('rs1', 5), ('zero', 1), ('asi', 8), ('rs2', 5)])
mem_format1.setBitfield('op', [1, 1])
mem_format1.setVarField('rs1', ('REGS', 0), 'in')
mem_format1.setVarField('rs2', ('REGS', 0), 'in')

mem_format2 = trap.MachineCode([('op', 2), ('rd', 5), ('op3', 6), ('rs1', 5), ('one', 1), ('simm13', 13)])
mem_format2.setBitfield('op', [1, 1])
mem_format2.setVarField('rs1', ('REGS', 0), 'in')

# Store Barrier format
stbar_format = trap.MachineCode([('op', 2), ('zero', 5), ('op3', 6), ('rs1', 5), ('zero', 14)])
stbar_format.setBitfield('op', [1, 0])
stbar_format.setBitfield('op3', [1, 0, 1, 0, 0, 0])
stbar_format.setBitfield('rs1', [0, 1, 1, 1, 1])

# logical and remainig instructions format
dpi_format1 = trap.MachineCode([('op', 2), ('rd', 5), ('op3', 6), ('rs1', 5), ('zero', 1), ('asi', 8), ('rs2', 5)])
dpi_format1.setBitfield('op', [1, 0])
dpi_format1.setVarField('rd', ('REGS', 0), 'out')
dpi_format1.setVarField('rs1', ('REGS', 0), 'in')
dpi_format1.setVarField('rs2', ('REGS', 0), 'in')

dpi_format2 = trap.MachineCode([('op', 2), ('rd', 5), ('op3', 6), ('rs1', 5), ('one', 1), ('simm13', 13)])
dpi_format2.setBitfield('op', [1, 0])
dpi_format2.setVarField('rd', ('REGS', 0), 'out')
dpi_format2.setVarField('rs1', ('REGS', 0), 'in')

# Format for reading special instructions
read_special_format = trap.MachineCode([('op', 2), ('rd', 5), ('op3', 6), ('asr', 5), ('zero', 14)])
read_special_format.setBitfield('op', [1, 0])
read_special_format.setVarField('rd', ('REGS', 0), 'out')

# Format for writing special instructions
write_special_format1 = trap.MachineCode([('op', 2), ('rd', 5), ('op3', 6), ('rs1', 5), ('zero', 9), ('rs2', 5)])
write_special_format1.setBitfield('op', [1, 0])
write_special_format1.setVarField('rs1', ('REGS', 0), 'in')
write_special_format1.setVarField('rs2', ('REGS', 0), 'in')

write_special_format2 = trap.MachineCode([('op', 2), ('rd', 5), ('op3', 6), ('rs1', 5), ('one', 1), ('simm13', 13)])
write_special_format2.setBitfield('op', [1, 0])
write_special_format2.setVarField('rs1', ('REGS', 0), 'in')

# Trap on integer condition code format
ticc_format1 = trap.MachineCode([('op', 2), ('reserved1', 1), ('cond', 4), ('op3', 6), ('rs1', 5), ('zero', 1), ('asi', 8), ('rs2', 5)])
ticc_format1.setBitfield('op', [1, 0])
ticc_format1.setVarField('rs1', ('REGS', 0), 'in')
ticc_format1.setVarField('rs2', ('REGS', 0), 'in')

ticc_format2 = trap.MachineCode([('op', 2), ('reserved1', 1), ('cond', 4), ('op3', 6), ('rs1', 5), ('one', 1), ('reserved2', 6), ('imm7', 7)])
ticc_format2.setBitfield('op', [1, 0])
ticc_format2.setVarField('rs1', ('REGS', 0), 'in')

# Coprocessor of fpu instruction format
coprocessor_format = trap.MachineCode([('op', 2), ('rd', 5), ('op3', 6), ('rs1', 5), ('opf', 9), ('rs2', 5)])
coprocessor_format.setBitfield('op', [1, 0])
coprocessor_format.setVarField('rd', ('REGS', 0), 'out')
coprocessor_format.setVarField('rs1', ('REGS', 0), 'in')
coprocessor_format.setVarField('rs2', ('REGS', 0), 'in')
