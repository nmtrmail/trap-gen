################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     interfaceWriter.py
# @brief    This file is part of the TRAP processor generator module.
# @details
# @author   Luca Fossati
# @author   Lillian Tadros (Technische Universitaet Dortmund)
# @date     2008-2013 Luca Fossati
#           2015-2016 Technische Universitaet Dortmund
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

import cxx_writer

################################################################################
# Processor Interface Class
################################################################################
def getCPPIf(self, model, namespace):
    """Creates the interface used by the tools to access the processor core."""

    if not self.abi:
        return

    wordType = self.bitSizes[1]
    includes = wordType.getIncludes()
    pipeRegisterType = cxx_writer.Type('PipelineRegister', includes = '#include \"registers.hpp\"')
    histInstrType = cxx_writer.Type('HistoryInstrType', 'modules/instruction.hpp')
    histQueueType = cxx_writer.TemplateType('boost::circular_buffer', [histInstrType], 'boost/circular_buffer.hpp')

    from procWriter import abiAttrs
    abiMembers = []
    abiCtorParams = []
    abiCtorInit = []
    abitCtorCode = ''
    regReadCode = '.read_force()'
    regWriteCode = '.write_force'

    #---------------------------------------------------------------------------
    ## @name Attributes and Initialization
    #  @{

    for attr in abiAttrs:
        abiMembers.append(attr)
        abiCtorParams.append(cxx_writer.Parameter(attr.name, attr.varType))
        abiCtorInit.append(attr.name + '(' + attr.name + ')')

    routineEntryStateAttr = cxx_writer.Attribute('routine_entry_state', cxx_writer.intType, 'private')
    abiMembers.append(routineEntryStateAttr)

    routineExitStateAttr = cxx_writer.Attribute('routine_exit_state', cxx_writer.intType, 'private')
    abiMembers.append(routineExitStateAttr)

    abitCtorCode = """this->routine_entry_state = 0;
    this->routine_exit_state = 0;
    """

    StrVectorType = cxx_writer.TemplateType('std::vector', [cxx_writer.stringType], 'vector')
    StrVectorVectorType = cxx_writer.TemplateType('std::vector', [StrVectorType], 'vector')

    routineEntrySequenceAttr = cxx_writer.Attribute('routine_entry_sequence', StrVectorVectorType, 'private')
    abiMembers.append(routineEntrySequenceAttr)

    routineExitSequenceAttr = cxx_writer.Attribute('routine_exit_sequence', StrVectorVectorType, 'private')
    abiMembers.append(routineExitSequenceAttr)

    exitValueAttr = cxx_writer.Attribute('exit_value', cxx_writer.uintType, 'private')
    abiMembers.append(exitValueAttr)

    from isa import Instruction
    abitCtorCode = 'std::vector<std::string> temp_vec;\n'
    for instrList in self.abi.callInstr:
        abitCtorCode += 'temp_vec.clear();\n'
        if not instrList:
            abitCtorCode += 'temp_vec.push_back("");\n'
        elif isinstance(instrList, Instruction):
            abitCtorCode += 'temp_vec.push_back("' + instrList.name + '");\n'
        else:
            for instr in instrList:
                abitCtorCode += 'temp_vec.push_back("' + instr.name + '");\n'
        abitCtorCode += 'this->routine_entry_sequence.push_back(temp_vec);\n'
    for instrList in self.abi.returnCallInstr:
        abitCtorCode += 'temp_vec.clear();\n'
        if not instrList:
            abitCtorCode += 'temp_vec.push_back("");\n'
        elif isinstance(instrList, Instruction):
            abitCtorCode += 'temp_vec.push_back("' + instrList.name + '");\n'
        else:
            for instr in instrList:
                abitCtorCode += 'temp_vec.push_back("' + instr.name + '");\n'
        abitCtorCode += 'this->routine_exit_sequence.push_back(temp_vec);\n'

    ## @} Attributes and Initialization
    #---------------------------------------------------------------------------
    ## @name Interface Methods
    #  @{

    # num_gdb_regs()
    maxGDBId = 0
    numGDBRegsBody = cxx_writer.Code('return ' + str(maxGDBId + 1) + ';')
    numGDBRegsMethod = cxx_writer.Method('num_gdb_regs', numGDBRegsBody, cxx_writer.uintType, 'public', noException = True, const = True)
    abiMembers.append(numGDBRegsMethod)

    # read_gdb_reg()
    Code = 'switch(gdb_id) {\n'
    sortedGDBRegs = sorted(self.abi.regCorrespondence.items(), lambda x,y: cmp(x[1], y[1]))
    for reg, gdbId in sortedGDBRegs:
        if gdbId > maxGDBId:
            maxGDBId = gdbId
        Code += 'case ' + str(gdbId) + ': {\n'
        Code += 'return ' + reg
        if self.abi.offset.has_key(reg) and not model.startswith('acc'):
            Code += ' + ' + str(self.abi.offset[reg])
        Code += ';\nbreak;}\n'
    Code += 'default: {\nreturn 0;\n}\n}\n'
    readGDBRegBody = cxx_writer.Code(Code)
    readGDBRegBody.addInclude(includes)
    readGDBRegParam = cxx_writer.Parameter('gdb_id', cxx_writer.uintType.makeRef().makeConst())
    readGDBRegMethod = cxx_writer.Method('read_gdb_reg', readGDBRegBody, wordType, 'public', [readGDBRegParam], noException = True, const = True)
    abiMembers.append(readGDBRegMethod)

    # set_gdb_reg()
    Code = 'switch(gdb_id) {\n'
    for reg, gdbId in sortedGDBRegs:
        Code += 'case ' + str(gdbId) + ': {\n'
        Code += reg + regWriteCode + '(new_value'
        Code += ');\nbreak;}\n'
    Code += 'default: {\nTHROW_EXCEPTION(\"Register corresponding to GDB id \" << gdb_id << \" not found.\");\n}\n}\n'
    setGDBRegBody = cxx_writer.Code(Code)
    setGDBRegBody.addInclude(includes)
    setGDBRegParam1 = cxx_writer.Parameter('new_value', wordType.makeRef().makeConst())
    setGDBRegParam2 = cxx_writer.Parameter('gdb_id', cxx_writer.uintType.makeRef().makeConst())
    setGDBRegMethod = cxx_writer.Method('set_gdb_reg', setGDBRegBody, cxx_writer.voidType, 'public', [setGDBRegParam1, setGDBRegParam2], noException = True)
    abiMembers.append(setGDBRegMethod)

    addressParam = cxx_writer.Parameter('address', wordType.makeRef().makeConst())

    # read_args()
    vectorType = cxx_writer.TemplateType('std::vector', [wordType], 'vector')
    Code = str(vectorType) + ' args;\n'
    for arg in self.abi.args:
        Code += 'args.push_back(this->' + arg
        if self.abi.offset.has_key(arg) and not model.startswith('acc'):
            Code += ' + ' + str(self.abi.offset[arg])
        Code += ');\n'
    Code += 'return args;\n'
    readArgsBody = cxx_writer.Code(Code)
    readArgsBody.addInclude(includes)
    readArgsMethod = cxx_writer.Method('read_args', readArgsBody, vectorType, 'public', noException = True, const = True)
    abiMembers.append(readArgsMethod)

    # set_args()
    Code = 'if (args.size() > ' + str(len(self.abi.args)) + ') {\nTHROW_EXCEPTION(\"Too many arguments for processor ABI, given \" << args.size() << \", expected up to ' + str(len(self.abi.args)) + ' .\");\n}\n'
    Code += str(vectorType) + '::const_iterator arg_it = args.begin(), arg_end = args.end();\n'
    for arg in self.abi.args:
        Code += 'if (arg_it != arg_end) {\n'
        Code += arg + regWriteCode + '(*arg_it'
        if self.abi.offset.has_key(arg) and not model.startswith('acc'):
            Code += ' - ' + str(self.abi.offset[arg])
        Code += ');\narg_it++;\n}\n'
    setArgsParam = cxx_writer.Parameter('args', vectorType.makeRef().makeConst())
    setArgsMethod = cxx_writer.Method('set_args', cxx_writer.Code(Code), cxx_writer.voidType, 'public', [setArgsParam], noException = True)
    abiMembers.append(setArgsMethod)

    # read_<>(), set_<>()
    for elem in [self.abi.PC, self.abi.LR, self.abi.SP, self.abi.FP, self.abi.RetVal]:
        if not elem:
            continue
        Code = 'return ' + elem
        if self.abi.offset.has_key(elem):
            Code += ' + ' + str(self.abi.offset[elem])
        Code += ';'
        readElemBody = cxx_writer.Code(Code)
        readElemBody.addInclude(includes)
        readElemMethod = cxx_writer.Method('read_' + self.abi.name[elem], readElemBody, wordType, 'public', noException = True, const = True)
        abiMembers.append(readElemMethod)

        Code = elem + regWriteCode + '(new_value);'
        setElemBody = cxx_writer.Code(Code)
        setElemBody.addInclude(includes)
        setElemParam = cxx_writer.Parameter('new_value', wordType.makeRef().makeConst())
        setElemMethod = cxx_writer.Method('set_' + self.abi.name[elem], setElemBody, cxx_writer.voidType, 'public', [setElemParam], noException = True)
        abiMembers.append(setElemMethod)

    # read_mem()
    Code = ''
    if not self.abi.memories:
        Code += 'THROW_EXCEPTION(\"No memory accessible from the ABI or processor ' + self.name + '.\");'
    else:
        if len(self.abi.memories) == 1:
            Code += 'return this->' + self.abi.memories.keys()[0] + '.read_word_dbg(address);'
        else:
            for mem_name, mem_range in self.abi.memories.items():
                Code += 'if (address >= ' + hex(mem_range[0]) + ' && address <= ' + hex(mem_range[1]) + ') {\n'
                Code += 'return this->' + self.abi.memories.keys()[0] + '.read_word_dbg(address);\n} else '
            Code += ' {\nTHROW_EXCEPTION(\"Address \" << std::hex << address << \" out of range.\");\n}'
    readMemMethod = cxx_writer.Method('read_mem', cxx_writer.Code(Code), wordType, 'public', [addressParam])
    abiMembers.append(readMemMethod)

    # read_char_mem()
    Code = ''
    if not self.abi.memories:
        Code += 'THROW_EXCEPTION(\"No memory accessible from the ABI or processor ' + self.name + '.\");'
    else:
        if len(self.abi.memories) == 1:
            Code += 'return this->' + self.abi.memories.keys()[0] + '.read_byte_dbg(address);'
        else:
            for mem_name, mem_range in self.abi.memories.items():
                Code += 'if (address >= ' + hex(mem_range[0]) + ' && address <= ' + hex(mem_range[1]) + ') {\n'
                Code += 'return this->' + self.abi.memories.keys()[0] + '.read_byte_dbg(address);\n} else '
            Code += ' {\nTHROW_EXCEPTION(\"Address \" << std::hex << address << \" out of range.\");\n}'
    readMemMethod = cxx_writer.Method('read_char_mem', cxx_writer.Code(Code), cxx_writer.ucharType, 'public', [addressParam])
    abiMembers.append(readMemMethod)

    # write_mem()
    Code = ''
    if not self.abi.memories:
        Code += 'THROW_EXCEPTION(\"No memory accessible from the ABI or processor ' + self.name + '.\");'
    else:
        if len(self.abi.memories) == 1:
            Code += 'this->' + self.abi.memories.keys()[0] + '.write_word_dbg(address, datum);'
        else:
            for mem_name, mem_range in self.abi.memories.items():
                Code += 'if (address >= ' + hex(mem_range[0]) + ' && address <= ' + hex(mem_range[1]) + ') {\n'
                Code += 'this->' + self.abi.memories.keys()[0] + '.write_word_dbg(address, datum);\n} else '
            Code += ' {\nTHROW_EXCEPTION(\"Address \" << std::hex << address << \" out of range.\");\n}'
    writeMemBody = cxx_writer.Code(Code)
    writeMemBody.addInclude('common/report.hpp')
    datumParam = cxx_writer.Parameter('datum', wordType)
    writeMemMethod = cxx_writer.Method('write_mem', writeMemBody, cxx_writer.voidType, 'public', [addressParam, datumParam])
    abiMembers.append(writeMemMethod)

    # write_char_mem()
    Code = ''
    if not self.abi.memories:
        Code += 'THROW_EXCEPTION(\"No memory accessible from the ABI or processor ' + self.name + '.\");'
    else:
        if len(self.abi.memories) == 1:
            Code += 'this->' + self.abi.memories.keys()[0] + '.write_byte_dbg(address, datum);'
        else:
            for mem_name, mem_range in self.abi.memories.items():
                Code += 'if (address >= ' + hex(mem_range[0]) + ' && address <= ' + hex(mem_range[1]) + ') {\n'
                Code += 'this->' + self.abi.memories.keys()[0] + '.write_byte_dbg(address, datum);\n} else '
            Code += ' {\nTHROW_EXCEPTION(\"Address \" << std::hex << address << \" out of range.\");\n}'
    datumParam = cxx_writer.Parameter('datum', cxx_writer.ucharType)
    writeMemMethod = cxx_writer.Method('write_char_mem', cxx_writer.Code(Code), cxx_writer.voidType, 'public', [addressParam, datumParam])
    abiMembers.append(writeMemMethod)

    # Methods necessary for saving and restoring the complete processor state.
    # Useful, for example, for implementing hardware context-switches, or
    # simulation checkpointing.

    # get_state()
    totalStateSize = 0
    for reg in self.regs:
        totalStateSize += reg.bitWidth/self.byteSize
    for regB in self.regBanks:
        totalStateSize += (regB.bitWidth*regB.numRegs)/self.byteSize
    Code = 'unsigned char* cur_state = new unsigned char[' + str(totalStateSize) + '];\n'
    Code += 'unsigned char* cur_state_temp = cur_state;\n'

    from processor import extractRegInterval
    stateIgnoreRegs = {}
    for reg in self.abi.stateIgnoreRegs:
        index = extractRegInterval(reg)
        if index:
            refName = reg[:reg.find('[')]
            if not refName in stateIgnoreRegs.keys():
                stateIgnoreRegs[refName] = []
            for i in range(index[0], index[1] + 1):
                stateIgnoreRegs[refName].append(i)
        else:
            stateIgnoreRegs[reg] = None

    from isa import resolveBitType
    for reg in self.regs:
        if not reg.name in stateIgnoreRegs.keys():
            regWType = resolveBitType('BIT<' + str(reg.bitWidth) + '>')
            Code += '*((' + str(regWType.makePointer()) + ')cur_state_temp) = ' + reg.name + regReadCode + ';\ncur_state_temp += ' + str(reg.bitWidth/self.byteSize) + ';\n'
    for regB in self.regBanks:
        regWType = resolveBitType('BIT<' + str(regB.bitWidth) + '>')
        if not regB.name in stateIgnoreRegs.keys():
            for i in range(0, regB.numRegs):
                Code += '*((' + str(regWType.makePointer()) + ')cur_state_temp) = ' + regB.name + '[' + str(i) + ']' + regReadCode + ';\ncur_state_temp += ' + str(regB.bitWidth/self.byteSize) + ';\n'
        else:
            for i in range(0, regB.numRegs):
                if i not in stateIgnoreRegs[regB.name]:
                    Code += '*((' + str(regWType.makePointer()) + ')cur_state_temp) = ' + regB.name + '[' + str(i) + ']' + regReadCode + ';\ncur_state_temp += ' + str(regB.bitWidth/self.byteSize) + ';\n'
    Code += 'return cur_state;'
    getStateMethod = cxx_writer.Method('get_state', cxx_writer.Code(Code), cxx_writer.ucharPtrType, 'public', noException = True, const = True)
    abiMembers.append(getStateMethod)

    # set_state()
    Code = 'unsigned char* cur_state_temp = state;\n'
    for reg in self.regs:
        if not reg.name in self.abi.stateIgnoreRegs:
            regWType = resolveBitType('BIT<' + str(reg.bitWidth) + '>')
            Code += reg.name + regWriteCode + '(*((' + str(regWType.makePointer()) + ')cur_state_temp));\ncur_state_temp += ' + str(reg.bitWidth/self.byteSize) + ';\n'
    for regB in self.regBanks:
        regWType = resolveBitType('BIT<' + str(regB.bitWidth) + '>')
        if not regB.name in stateIgnoreRegs.keys():
            for i in range(0, regB.numRegs):
                Code += regB.name + '[' + str(i) + ']' + regWriteCode + '(*((' + str(regWType.makePointer()) + ')cur_state_temp));\ncur_state_temp += ' + str(regB.bitWidth/self.byteSize) + ';\n'
        else:
            for i in range(0, regB.numRegs):
                if i not in stateIgnoreRegs[regB.name]:
                    Code += regB.name + '[' + str(i) + ']' + regWriteCode + '(*((' + str(regWType.makePointer()) + ')cur_state_temp));\ncur_state_temp += ' + str(regB.bitWidth/self.byteSize) + ';\n'
    setStateParam = cxx_writer.Parameter('state', cxx_writer.ucharPtrType)
    setStateMethod = cxx_writer.Method('set_state', cxx_writer.Code(Code), cxx_writer.voidType, 'public', [setStateParam], noException = True)
    abiMembers.append(setStateMethod)

    # get_exit_value()
    Code = 'return this->exit_value;'
    exitValueMethod = cxx_writer.Method('get_exit_value', cxx_writer.Code(Code), wordType, 'public', noException = True)
    abiMembers.append(exitValueMethod)

    # set_exit_value()
    Code = 'this->exit_value = value;'
    exitValueParam = cxx_writer.Parameter('value', wordType)
    exitValueMethod = cxx_writer.Method('set_exit_value', cxx_writer.Code(Code), cxx_writer.voidType, 'public', [exitValueParam], noException = True)
    abiMembers.append(exitValueMethod)

    # pre_call(), post_call(), return_from_call()
    if self.abi.preCallCode:
        abiMembers.append(cxx_writer.Method('pre_call', cxx_writer.Code(self.abi.preCallCode), cxx_writer.voidType, 'public', noException = True))
    if self.abi.postCallCode:
        abiMembers.append(cxx_writer.Method('post_call', cxx_writer.Code(self.abi.postCallCode), cxx_writer.voidType, 'public', noException = True))
    if self.abi.returnCallReg:
        returnCallCode = ''
        for returnReg in self.abi.returnCallReg:
            returnCallCode += returnReg[0] + regWriteCode + '(' + returnReg[1] + ' + ' + str(returnReg[2]) + ');\n'
        abiMembers.append(cxx_writer.Method('return_from_call', cxx_writer.Code(returnCallCode), cxx_writer.voidType, 'public', noException = True))

    # is_executing_instr()
    isExecutingInstrBody = cxx_writer.Code('return this->instr_executing;')
    isExecutingInstrMethod = cxx_writer.Method('is_executing_instr', isExecutingInstrBody, cxx_writer.boolType, 'public', noException = True, const = True)
    abiMembers.append(isExecutingInstrMethod)

    # wait_instr_end()
    if self.systemc:
        waitInstrEndBody = cxx_writer.Code('if (this->instr_executing) {\nwait(this->instr_end_event);\n}\n')
        waitInstrEndBody.addInclude('systemc.h')
    else:
        waitInstrEndBody = cxx_writer.Code('while(this->instr_executing) {\n;\n}\n')
    waitInstrEndMethod = cxx_writer.Method('wait_instr_end', waitInstrEndBody, cxx_writer.voidType, 'public', noException = True, const = True)
    abiMembers.append(waitInstrEndMethod)

    # is_routine_entry()
    # Here is the code for recognizing if we are in the routine entry or exit.
    # The ABI behaves like a state machine, moving to the beginning when an
    # instruction out of the sequence is met.
    isRoutineEntryCode = """std::vector<std::string> next_names = this->routine_entry_sequence[this->routine_entry_state];
    std::vector<std::string>::const_iterator names_it, names_end;
    std::string cur_name = instr->get_name();
    for (names_it = next_names.begin(), names_end = next_names.end(); names_it != names_end; names_it++) {
        if (cur_name == *names_it || *names_it == "") {
            if (this->routine_entry_state == """ + str(len(self.abi.callInstr) -1) + """) {
                this->routine_entry_state = 0;
                return true;
            }
            this->routine_entry_state++;
            return false;
        }
    }
    this->routine_entry_state = 0;
    return false;
    """
    isRoutineEntryBody = cxx_writer.Code(isRoutineEntryCode)
    InstructionBaseType = cxx_writer.Type('InstructionBase', 'modules/instruction.hpp')
    InstructionParam = cxx_writer.Parameter('instr', InstructionBaseType.makePointer().makeConst())
    isRoutineEntryMethod = cxx_writer.Method('is_routine_entry', isRoutineEntryBody, cxx_writer.boolType, 'public', [InstructionParam], noException = True)
    abiMembers.append(isRoutineEntryMethod)

    # is_routine_exit()
    isRoutineExitCode = """std::vector<std::string> next_names = this->routine_exit_sequence[this->routine_exit_state];
    std::vector<std::string>::const_iterator names_it, names_end;
    std::string cur_name = instr->get_name();
    for (names_it = next_names.begin(), names_end = next_names.end(); names_it != names_end; names_it++) {
        if (cur_name == *names_it || *names_it == "") {
            if (this->routine_exit_state == """ + str(len(self.abi.returnCallInstr) -1) + """) {
                this->routine_exit_state = 0;
                return true;
            }
            this->routine_exit_state++;
            return false;
        }
    }
    this->routine_exit_state = 0;
    return false;
    """
    isRoutineExitBody = cxx_writer.Code(isRoutineExitCode)
    isRoutineExitMethod = cxx_writer.Method('is_routine_exit', isRoutineExitBody, cxx_writer.boolType, 'public', [InstructionParam], noException = True)
    abiMembers.append(isRoutineExitMethod)

    # get_history()
    Code = 'return this->history_instr_queue;'
    getInstructionHistoryMethod = cxx_writer.Method('get_history', cxx_writer.Code(Code), histQueueType.makeRef(), 'public')
    abiMembers.append(getInstructionHistoryMethod)

    # get_code_limit()
    Code = 'return this->PROGRAM_LIMIT;'
    codeLimitMethod = cxx_writer.Method('get_code_limit', cxx_writer.Code(Code), wordType, 'public')
    abiMembers.append(codeLimitMethod)

    ## @} Interface Methods
    #---------------------------------------------------------------------------
    ## @name Information and Helper Methods
    #  @{

    # get_id()
    if self.abi.procIdCode:
        getIDBody = cxx_writer.Code('return (' + self.abi.procIdCode + ');\n')
        getIDMethod = cxx_writer.Method('get_id', getIDBody, cxx_writer.intType, 'public', noException = True, const = True)
        abiMembers.append(getIDMethod)

    # is_little_endian()
    if self.isBigEndian:
        isLittleEndianBody = cxx_writer.Code('return false;')
    else:
        isLittleEndianBody = cxx_writer.Code('return true;')
    isLittleEndianMethod = cxx_writer.Method('is_little_endian', isLittleEndianBody, cxx_writer.boolType, 'public', noException = True, const = True)
    abiMembers.append(isLittleEndianMethod)

    ## @} Information and Helper Methods
    #---------------------------------------------------------------------------
    ## @name Constructors and Destructors
    #  @{

    abiCtor = cxx_writer.Constructor(cxx_writer.Code(abitCtorCode), 'public', abiCtorParams, abiCtorInit)
    abiDtor = cxx_writer.Destructor(cxx_writer.Code(''), 'public', True)

    ## @} Constructors and Destructors
    #---------------------------------------------------------------------------

    abiType = cxx_writer.TemplateType('ABIIf', [wordType], 'modules/abi_if.hpp')
    abiClass = cxx_writer.ClassDeclaration('Interface', abiMembers, [abiType], namespaces = [namespace])
    abiClass.addDocString(brief = 'Interface Class', detail = 'Creates the interface used by TRAP-Gen tools to access the processor core.')
    abiClass.addConstructor(abiCtor)
    abiClass.addDestructor(abiDtor)
    return abiClass

################################################################################
