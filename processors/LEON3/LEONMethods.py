################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     LEONMethods.py
# @brief    This file is part of the TRAP example processors.
# @details  Instruction helper methods for the LEON3.
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

from LEONDefs import *

def updateAliasCode_abi():
    return """
    //ABI model: we simply immediately update the alias
    for(int i = 8; i < 32; i++){
        REGS[i].update_alias(WINREGS[(newCwp*16 + i - 8) % (""" + str(16*numRegWindows) + """)]);
    }
    """

def updateAliasCode_decode():
    import math
    if math.modf(math.log(16*numRegWindows, 2))[0] == 0:
        modCode = '& ' + hex(16*numRegWindows - 1)
    else:
        modCode = '% ' + str(16*numRegWindows)

    code = """for(int i = 8; i < 32; i++){
        REGS[i].update_alias(WINREGS[(newCwp*16 + i - 8) """ + modCode + """]);
    }
    """
    return code

def updateAliasCode_exception():
    import math
    if math.modf(math.log(16*numRegWindows, 2))[0] == 0:
        modCode = '& ' + hex(16*numRegWindows - 1)
    else:
        modCode = '% ' + str(16*numRegWindows)

    code = """for(int i = 8; i < 32; i++){
        REGS[i].update_alias(WINREGS[(newCwp*16 + i - 8) """ + modCode + """]);
    }
    """
    return code

# Methods used (just by the cycle accurate processor) to check that a register window is valid
# when a decrement or an increment are performed
checkIncrementWin_code = """
unsigned newCwp = ((unsigned)(PSR[PSR_CWP] + 1)) % NUM_REG_WIN;
if(((0x01 << (newCwp)) & WIM) != 0){
    return false;
}
else{
    return true;
}
"""
opCode = cxx_writer.Code(checkIncrementWin_code)
checkIncrementWin_method = trap.HelperMethod('checkIncrementWin', opCode, 'decode', exception = False, const = True)
checkIncrementWin_method.setSignature(cxx_writer.boolType)
checkDecrementWin_code = """
unsigned newCwp = ((unsigned)(PSR[PSR_CWP] - 1)) % NUM_REG_WIN;
if(((0x01 << (newCwp)) & WIM) != 0){
    return false;
}
else{
    return true;
}
"""
opCode = cxx_writer.Code(checkDecrementWin_code)
checkDecrementWin_method = trap.HelperMethod('checkDecrementWin', opCode, 'decode', exception = False, const = True)
checkDecrementWin_method.setSignature(cxx_writer.boolType)

# Method used to move to the next register window; this simply consists in
# the check that there is an empty valid window and in the update of
# the window aliases
IncrementRegWindow_code = """
newCwp = ((unsigned)(PSR[PSR_CWP] + 1)) % NUM_REG_WIN;
if(((0x01 << (newCwp)) & WIM) != 0){
    return false;
}
PSR = (PSR & 0xFFFFFFE0) | newCwp;
"""
IncrementRegWindow_code += updateAliasCode_decode()
IncrementRegWindow_code += 'return true;'
opCode = cxx_writer.Code(IncrementRegWindow_code)
IncrementRegWindow_method = trap.HelperMethod('IncrementRegWindow', opCode, 'decode', exception = False)
IncrementRegWindow_method.setSignature(cxx_writer.boolType)
IncrementRegWindow_method.addVariable(('newCwp', 'BIT<32>'))
# Method used to move to the previous register window; this simply consists in
# the check that there is an empty valid window and in the update of
# the window aliases
DecrementRegWindow_code = """
newCwp = ((unsigned)(PSR[PSR_CWP] - 1)) % NUM_REG_WIN;
if(((0x01 << (newCwp)) & WIM) != 0){
    return false;
}
PSR = (PSR & 0xFFFFFFE0) | newCwp;
"""
DecrementRegWindow_code += updateAliasCode_decode()
DecrementRegWindow_code += 'return true;'
opCode = cxx_writer.Code(DecrementRegWindow_code)
DecrementRegWindow_method = trap.HelperMethod('DecrementRegWindow', opCode, 'decode', exception = False)
DecrementRegWindow_method.setSignature(cxx_writer.boolType)
DecrementRegWindow_method.addVariable(('newCwp', 'BIT<32>'))

# Sign extends the input bitstring
opCode = cxx_writer.Code("""
if((bitSeq & (1 << (bitSeq_length - 1))) != 0)
    bitSeq |= (((unsigned)0xFFFFFFFF) << bitSeq_length);
return bitSeq;
""")
SignExtend_method = trap.HelperMethod('SignExtend', opCode, 'execute', exception = False, const = True)
SignExtend_method.setSignature(cxx_writer.intType, [('bitSeq', 'BIT<32>'), cxx_writer.Parameter('bitSeq_length', cxx_writer.uintType)])

# Normal PC increment, used when not in a branch instruction; in a branch instruction
# I will directly modify both PC and nPC in case we are in a the cycle accurate model,
# while just nPC in case we are in the functional one; if the branch has the annuling bit
# set, then also in the functional model both the PC and nPC will be modified
raiseExcCode = """
if(PSR[PSR_ET] == 0){
    if(exceptionId < IRQ_LEV_15){
        // I print a core dump and then I signal an error: an exception happened while
        // exceptions were disabled in the processor core
        THROW_EXCEPTION("Exception " << exceptionId << " happened while the PSR[ET] = 0; PC = " << std::hex << std::showbase << PC << std::endl << "Instruction " << get_mnemonic());
    }
}
else{
    unsigned curPSR = PSR;
    curPSR = (curPSR & 0xffffffbf) | (PSR[PSR_S] << 6);
    curPSR = (curPSR & 0xffffff7f) | 0x00000080;
    curPSR &= 0xffffffdf;
    unsigned newCwp = ((unsigned)(PSR[PSR_CWP] - 1)) % NUM_REG_WIN;
"""
raiseExcCode += updateAliasCode_exception()
raiseExcCode +=  """
    curPSR = (curPSR & 0xffffffe0) + newCwp;
    PSR = curPSR;
    #ifdef ACC_MODEL
    // TODO: Write execute stage.
    PSR = curPSR;
    #endif
    REGS[17] = pcounter;
    REGS[18] = npcounter;
    switch(exceptionId){
        case RESET:{
        }break;
        case DATA_STORE_ERROR:{
            TBR[TBR_TT] = 0x2b;
        }break;
        case INSTR_ACCESS_MMU_MISS:{
            TBR[TBR_TT] = 0x3c;
        }break;
        case INSTR_ACCESS_ERROR:{
            TBR[TBR_TT] = 0x21;
        }break;
        case R_REGISTER_ACCESS_ERROR:{
            TBR[TBR_TT] = 0x20;
        }break;
        case INSTR_ACCESS_EXC:{
            TBR[TBR_TT] = 0x01;
        }break;
        case PRIVILEDGE_INSTR:{
            TBR[TBR_TT] = 0x03;
        }break;
        case ILLEGAL_INSTR:{
            TBR[TBR_TT] = 0x02;
        }break;
        case FP_DISABLED:{
            TBR[TBR_TT] = 0x04;
        }break;
        case CP_DISABLED:{
            TBR[TBR_TT] = 0x24;
        }break;
        case UNIMPL_FLUSH:{
            TBR[TBR_TT] = 0x25;
        }break;
        case WATCHPOINT_DETECTED:{
            TBR[TBR_TT] = 0x0b;
        }break;
        case WINDOW_OVERFLOW:{
            TBR[TBR_TT] = 0x05;
        }break;
        case WINDOW_UNDERFLOW:{
            TBR[TBR_TT] = 0x06;
        }break;
        case MEM_ADDR_NOT_ALIGNED:{
            TBR[TBR_TT] = 0x07;
        }break;
        case FP_EXCEPTION:{
            TBR[TBR_TT] = 0x08;
        }break;
        case CP_EXCEPTION:{
            TBR[TBR_TT] = 0x28;
        }break;
        case DATA_ACCESS_ERROR:{
            TBR[TBR_TT] = 0x29;
        }break;
        case DATA_ACCESS_MMU_MISS:{
            TBR[TBR_TT] = 0x2c;
        }break;
        case DATA_ACCESS_EXC:{
            TBR[TBR_TT] = 0x09;
        }break;
        case TAG_OVERFLOW:{
            TBR[TBR_TT] = 0x0a;
        }break;
        case DIV_ZERO:{
            TBR[TBR_TT] = 0x2a;
        }break;
        case TRAP_INSTRUCTION:{
            TBR[TBR_TT] = 0x80 + customTrapOffset;
        }break;
        case IRQ_LEV_15:{
            TBR[TBR_TT] = 0x1f;
        }break;
        case IRQ_LEV_14:{
            TBR[TBR_TT] = 0x1e;
        }break;
        case IRQ_LEV_13:{
            TBR[TBR_TT] = 0x1d;
        }break;
        case IRQ_LEV_12:{
            TBR[TBR_TT] = 0x1c;
        }break;
        case IRQ_LEV_11:{
            TBR[TBR_TT] = 0x1b;
        }break;
        case IRQ_LEV_10:{
            TBR[TBR_TT] = 0x1a;
        }break;
        case IRQ_LEV_9:{
            TBR[TBR_TT] = 0x19;
        }break;
        case IRQ_LEV_8:{
            TBR[TBR_TT] = 0x18;
        }break;
        case IRQ_LEV_7:{
            TBR[TBR_TT] = 0x17;
        }break;
        case IRQ_LEV_6:{
            TBR[TBR_TT] = 0x16;
        }break;
        case IRQ_LEV_5:{
            TBR[TBR_TT] = 0x15;
        }break;
        case IRQ_LEV_4:{
            TBR[TBR_TT] = 0x14;
        }break;
        case IRQ_LEV_3:{
            TBR[TBR_TT] = 0x13;
        }break;
        case IRQ_LEV_2:{
            TBR[TBR_TT] = 0x12;
        }break;
        case IRQ_LEV_1:{
            TBR[TBR_TT] = 0x11;
        }break;
        case IMPL_DEP_EXC:{
            TBR[TBR_TT] = 0x60 + customTrapOffset;
        }break;
        default:{
        }break;
    }
    if(exceptionId == RESET){
        // I have to jump to address 0 and restart execution
        PC = 0;
        NPC = 4;
    }
    else{
        // I have to jump to the address contained in the TBR register
        PC = TBR;
        NPC = TBR + 4;
    }
    if(exceptionId > TRAP_INSTRUCTION && exceptionId < IMPL_DEP_EXC){
        // finally I acknowledge the interrupt on the external pin port
        irqAck_pin.send_pin_req(IMPL_DEP_EXC - exceptionId, 0);
    }
    flush();
    annul();
}
"""
RaiseException_method = trap.HelperMethod('RaiseException', cxx_writer.Code(raiseExcCode), 'exception')
RaiseException_methodParams = []
RaiseException_methodParams.append(cxx_writer.Parameter('pcounter', cxx_writer.uintType))
RaiseException_methodParams.append(cxx_writer.Parameter('npcounter', cxx_writer.uintType))
RaiseException_methodParams.append(cxx_writer.Parameter('exceptionId', cxx_writer.uintType))
RaiseException_methodParams.append(cxx_writer.Parameter('customTrapOffset', cxx_writer.uintType, initValue = '0'))
RaiseException_method.setSignature(cxx_writer.voidType, RaiseException_methodParams)

# Code used increment the program counter, moving it to the next instruction in
# the instruction stream
opCode = cxx_writer.Code("""unsigned npc = NPC;
PC = npc;
npc += 4;
NPC = npc;
""")
IncrementPC = trap.HelperOperation('IncrementPC', opCode, exception = False)

# Write back of the result of most operations, expecially ALUs;
# such operations do not modify the PSR
opCode = cxx_writer.Code("""
rd = result;
""")
WB_plain = trap.HelperOperation('WB_plain', opCode, exception = False)
WB_plain.addInstructionVar(('result', 'BIT<32>'))
WB_plain.addUserInstructionElement('rd')

# Write back of the result of most operations, expecially ALUs;
# such operations also modify the PSR
opCode = cxx_writer.Code("""
if(!temp_V){
    rd = result;
}
""")
WB_tv = trap.HelperOperation('WB_tv', opCode, exception = False)
WB_tv.addInstructionVar(('result', 'BIT<32>'))
WB_tv.addInstructionVar(('temp_V', 'BIT<1>'))
WB_tv.addUserInstructionElement('rd')

# Modification of the Integer Condition Codes of the Processor Status Register
# after an logical operation or after the multiply operation
opCode = cxx_writer.Code("""
PSR[PSR_ICC_n] = ((result & 0x80000000) >> 31);
PSR[PSR_ICC_z] = (result == 0);
PSR[PSR_ICC_v] = 0;
PSR[PSR_ICC_c] = 0;
""")
ICC_writeLogic = trap.HelperOperation('ICC_writeLogic', opCode, exception = False)
ICC_writeLogic.addInstructionVar(('result', 'BIT<32>'))

# Modification of the Integer Condition Codes of the Processor Status Register
# after an addition operation
opCode = cxx_writer.Code("""
PSR[PSR_ICC_n] = ((result & 0x80000000) >> 31);
PSR[PSR_ICC_z] = (result == 0);
PSR[PSR_ICC_v] = ((unsigned)((rs1_op & rs2_op & (~result)) | ((~rs1_op) & (~rs2_op) & result))) >> 31;
PSR[PSR_ICC_c] = ((unsigned)((rs1_op & rs2_op) | ((rs1_op | rs2_op) & (~result)))) >> 31;
""")
ICC_writeAdd = trap.HelperOperation('ICC_writeAdd', opCode, exception = False)
ICC_writeAdd.addInstructionVar(('result', 'BIT<32>'))
ICC_writeAdd.addInstructionVar(('rs1_op', 'BIT<32>'))
ICC_writeAdd.addInstructionVar(('rs2_op', 'BIT<32>'))

# Modification of the Integer Condition Codes of the Processor Status Register
# after a tagged addition operation
opCode = cxx_writer.Code("""
PSR[PSR_ICC_n] = ((result & 0x80000000) >> 31);
PSR[PSR_ICC_z] = (result == 0);
PSR[PSR_ICC_v] = temp_V;
PSR[PSR_ICC_c] = ((unsigned)((rs1_op & rs2_op) | ((rs1_op | rs2_op) & (~result)))) >> 31;
""")
ICC_writeTAdd = trap.HelperOperation('ICC_writeTAdd', opCode, exception = False)
ICC_writeTAdd.addInstructionVar(('result', 'BIT<32>'))
ICC_writeTAdd.addInstructionVar(('temp_V', 'BIT<1>'))
ICC_writeTAdd.addInstructionVar(('rs1_op', 'BIT<32>'))
ICC_writeTAdd.addInstructionVar(('rs2_op', 'BIT<32>'))

# Modification of the Integer Condition Codes of the Processor Status Register
# after a division operation
opCode = cxx_writer.Code("""
if(!exception){
    PSR[PSR_ICC_n] = ((result & 0x80000000) >> 31);
    PSR[PSR_ICC_z] = (result == 0);
    PSR[PSR_ICC_v] = temp_V;
    PSR[PSR_ICC_c] = 0;
}
""")
ICC_writeDiv = trap.HelperOperation('ICC_writeDiv', opCode, exception = False)
ICC_writeDiv.addInstructionVar(('exception', 'BIT<1>'))
ICC_writeDiv.addInstructionVar(('result', 'BIT<32>'))
ICC_writeDiv.addInstructionVar(('temp_V', 'BIT<1>'))

# Modification of the Integer Condition Codes of the Processor Status Register
# after a tagged addition operation
opCode = cxx_writer.Code("""
if(!temp_V){
    PSR[PSR_ICC_n] = ((result & 0x80000000) >> 31);
    PSR[PSR_ICC_z] = (result == 0);
    PSR[PSR_ICC_v] = 0;
    PSR[PSR_ICC_c] = ((unsigned)((rs1_op & rs2_op) | ((rs1_op | rs2_op) & (~result)))) >> 31;
}
""")
ICC_writeTVAdd = trap.HelperOperation('ICC_writeTVAdd', opCode, exception = False)
ICC_writeTVAdd.addInstructionVar(('result', 'BIT<32>'))
ICC_writeTVAdd.addInstructionVar(('temp_V', 'BIT<1>'))
ICC_writeTVAdd.addInstructionVar(('rs1_op', 'BIT<32>'))
ICC_writeTVAdd.addInstructionVar(('rs2_op', 'BIT<32>'))

# Modification of the Integer Condition Codes of the Processor Status Register
# after a subtraction operation
opCode = cxx_writer.Code("""
PSR[PSR_ICC_n] = ((result & 0x80000000) >> 31);
PSR[PSR_ICC_z] = (result == 0);
PSR[PSR_ICC_v] = ((unsigned)((rs1_op & (~rs2_op) & (~result)) | ((~rs1_op) & rs2_op & result))) >> 31;
PSR[PSR_ICC_c] = ((unsigned)(((~rs1_op) & rs2_op) | (((~rs1_op) | rs2_op) & result))) >> 31;
""")
ICC_writeSub = trap.HelperOperation('ICC_writeSub', opCode, exception = False)
ICC_writeSub.addInstructionVar(('result', 'BIT<32>'))
ICC_writeSub.addInstructionVar(('rs1_op', 'BIT<32>'))
ICC_writeSub.addInstructionVar(('rs2_op', 'BIT<32>'))

# Modification of the Integer Condition Codes of the Processor Status Register
# after a tagged subtraction operation
opCode = cxx_writer.Code("""
PSR[PSR_ICC_n] = ((result & 0x80000000) >> 31);
PSR[PSR_ICC_z] = (result == 0);
PSR[PSR_ICC_v] = temp_V;
PSR[PSR_ICC_c] = ((unsigned)(((~rs1_op) & rs2_op) | (((~rs1_op) | rs2_op) & result))) >> 31;
""")
ICC_writeTSub = trap.HelperOperation('ICC_writeTSub', opCode, exception = False)
ICC_writeTSub.addInstructionVar(('result', 'BIT<32>'))
ICC_writeTSub.addInstructionVar(('temp_V', 'BIT<1>'))
ICC_writeTSub.addInstructionVar(('rs1_op', 'BIT<32>'))
ICC_writeTSub.addInstructionVar(('rs2_op', 'BIT<32>'))

# Modification of the Integer Condition Codes of the Processor Status Register
# after a tagged subtraction operation
opCode = cxx_writer.Code("""
if(!temp_V){
    PSR[PSR_ICC_n] = ((result & 0x80000000) >> 31);
    PSR[PSR_ICC_z] = (result == 0);
    PSR[PSR_ICC_v] = temp_V;
    PSR[PSR_ICC_c] = ((unsigned)(((~rs1_op) & rs2_op) | (((~rs1_op) | rs2_op) & result))) >> 31;
}
""")
ICC_writeTVSub = trap.HelperOperation('ICC_writeTVSub', opCode, exception = False)
ICC_writeTVSub.addInstructionVar(('result', 'BIT<32>'))
ICC_writeTVSub.addInstructionVar(('temp_V', 'BIT<1>'))
ICC_writeTVSub.addInstructionVar(('rs1_op', 'BIT<32>'))
ICC_writeTVSub.addInstructionVar(('rs2_op', 'BIT<32>'))
