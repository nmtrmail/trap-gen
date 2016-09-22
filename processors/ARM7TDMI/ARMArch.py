################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     ARMArch.py
# @brief    This file is part of the TRAP example processors.
# @details  This is the top-level TRAP definition of the ARM7TDMI.
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


# Lets first of all import the necessary files for the
# creation of the processor; note that if the trap modules are
# not in the default search path I have to manually specify the path
try:
    import trap
except ImportError:
    import sys, os
    sys.path.append(os.path.abspath(os.path.join('..', '..')))
    try:
        import trap
    except ImportError:
        print ('Please specify location of core TRAP files in ARMArch.py.')

# It is nice to keep the ISA and the architecture separated
# so we use the import trick
import ARMIsa

# Lets now start building the processor
processor = trap.Processor('ARM7TDMI', version = '0.1', systemc = False, instructionCache = True, fastFetch = True)
processor.setLittleEndian() #little endian
processor.setWordsize(4, 8) #4 bytes per word, 8 bits per byte
processor.setISA(ARMIsa.isa) #lets set the instruction set
processor.systemc= True

# Ok, now we move to the description of more complicated processor
# resources

# A registry bank of 22 registers each one 32 bits wide:
# they are the normal registers and the banked one. In particular:
# RB[0-7]: registers shared among all the modes
# RB[8-12]: registers shared among all modes but FIQ
# RB[13-14]: sp_user, lr_user
# RB[15-16]: sp_svc, lr_svc
# RB[17-18]: sp_abt, lr_abt
# RB[19-20]: sp_und, lr_und
# RB[21-22]: sp_irq, lr_irq
# RB[23-29]: r8_fiq, r14_fiq
regBank = trap.RegisterBank('RB', 30, 32)
processor.addRegBank(regBank)
# A registry bank of 5 registers each one 32 bits wide
# they are the saved processor status registers for the different
# execution modes; note that a bit mask for easily accessing
# the different fields is provided
# SPSR[0] = spsr_fiq, SPSR[1] = spsr_irq, SPSR[2] = spsr_svc,
# SPSR[3] = spsr_abt, SPSR[4] = spsr_und
spsrBitMask = {'N': (31, 31), 'Z': (30, 30), 'C': (29, 29), 'V': (28, 28), 'I': (7, 7), 'F': (6, 6), 'mode': (0, 3)}
spsrBank = trap.RegisterBank('SPSR', 5, 32, spsrBitMask)
processor.addRegBank(spsrBank)
# Current processor status register
cpsrBitMask = {'N': (31, 31), 'Z': (30, 30), 'C': (29, 29), 'V': (28, 28), 'I': (7, 7), 'F': (6, 6), 'mode': (0, 3)}
cpsr = trap.Register('CPSR', 32, cpsrBitMask)
cpsr.setDefaultValue(0x000000D3)
processor.addRegister(cpsr)
# Fake register (not presented in the architecture) indicating
# the processor ID: it is necessary in a multi-processor
# system
mp_id = trap.Register('MP_ID', 32)
mp_id.setDefaultValue('MPROC_ID')
processor.addRegister(mp_id)
# Now I set the alias: they can (and will) be used by the instructions
# to access the registers more easily. Note that, in general, it is
# responsibility of the programmer keeping the alias updated
regs = trap.AliasRegBank('REGS', 16, 'RB[0-15]')
regs.setOffset(15, 4)
processor.addAliasRegBank(regs)
FP = trap.AliasRegister('FP', 'REGS[11]')
processor.addAliasReg(FP)
SP = trap.AliasRegister('SPTR', 'REGS[13]')
processor.addAliasReg(SP)
LR = trap.AliasRegister('LINKR', 'REGS[14]')
processor.addAliasReg(LR)
SP_IRQ = trap.AliasRegister('SP_IRQ', 'RB[21]')
processor.addAliasReg(SP_IRQ)
LR_IRQ = trap.AliasRegister('LR_IRQ', 'RB[22]')
processor.addAliasReg(LR_IRQ)
SP_FIQ = trap.AliasRegister('SP_FIQ', 'RB[28]')
processor.addAliasReg(SP_FIQ)
LR_FIQ = trap.AliasRegister('LR_FIQ', 'RB[29]')
processor.addAliasReg(LR_FIQ)
PC = trap.AliasRegister('PC', 'REGS[15]')
# Special default value, others are PROGRAM_LIMIT ...
PC.setDefaultValue('ENTRY_POINT')
processor.addAliasReg(PC)
# Memory alias: registers which are memory mapped:
idMap = trap.MemoryAlias(0xFFFFFFF0, 'MP_ID')
processor.addMemAlias(idMap)
# register from which the instructions are fetched; note that in the
# functional model there is an offset between the PC and the actual
# fetch address (all of this is to take into account the fact that we do
# not have the pipeline)
processor.setFetchRegister('PC', -4)

# Lets now add details about the processor interconnection (i.e. memory ports,
# interrupt ports, pins, etc.)
if processor.systemc:
    processor.addTLMPort('instrMem', True)
    processor.addTLMPort('dataMem')
else:
    #processor.addMemory('dataMem', 10*1024*1024, True, 'PC')
    processor.addMemory('dataMem', 10*1024*1024)
# Now lets add the interrupt ports
irq = trap.Interrupt('IRQ', 1, priority = 0)
irq.setOperation('CPSR[CPSR_I] == 0', """
//Save LR_irq
LR_IRQ = PC;
//Save the current PSR
SPSR[1] = CPSR;
//I switch the register bank (i.e. I update the
//alias)
REGS[13].update_alias(RB[21]);
REGS[14].update_alias(RB[22]);
//Create the new PSR
CPSR = (CPSR & 0xFFFFFFD0) | 0x00000092;
//Finally I update the PC
PC = 0x18;""")
#processor.addIrq(irq)
fiq = trap.Interrupt('FIQ', 1, priority = 1)
fiq.setOperation('CPSR[CPSR_F] == 0', """
//Save LR_irq
LR_FIQ = PC;
//Save the current PSR
SPSR[0] = CPSR;
//I switch the register bank (i.e. I update the
//alias)
REGS[8].update_alias(RB[23]);
REGS[9].update_alias(RB[24]);
REGS[10].update_alias(RB[25]);
REGS[11].update_alias(RB[26]);
REGS[12].update_alias(RB[27]);
REGS[13].update_alias(RB[28]);
REGS[14].update_alias(RB[29]);
//Create the new PSR
CPSR = (CPSR & 0xFFFFFFD0) | 0x000000D1;
//Finally I update the PC
PC = 0x1C;""")
#processor.addIrq(fiq)

# Now it is time to add the pipeline stages
fetchStage = trap.PipeStage('fetch')
fetchStage.setFetchStage()
processor.addPipeStage(fetchStage)

decodeStage = trap.PipeStage('decode')
decodeStage.setDecodeStage()
decodeStage.setRegsStage()
processor.addPipeStage(decodeStage)

executeStage = trap.PipeStage('execute')
executeStage.setWbStage()
executeStage.setCheckUnknownInstr()
#executeStage.setCheckTools()
processor.addPipeStage(executeStage)

# The ABI is necessary to emulate system calls, personalize the GDB stub and,
# eventually, retarget GCC
abi = trap.ABI('REGS[0]', 'REGS[0-3]', 'PC', 'LINKR', 'SPTR', 'FP')
abi.addVarRegsCorrespondence({'REGS[0-15]': (0, 15), 'CPSR': 16})
# Same consideration as above: this offset is valid just for the functional
# simulator
abi.setOffset('PC', -4)
abi.setOffset('REGS[15]', -4)
abi.addMemory('dataMem')
abi.processorID('MP_ID')
abi.addIgnoreStateReg('MP_ID')
processor.setABI(abi)

# Finally we can dump the processor on file
#processor.write(folder = 'processor', models = {'funcLT': 'funcLT'}, tests = True)
processor.write(folder = 'processor', models = {'accAT': 'accAT', 'funcLT': 'funcLT'}, trace = True)
