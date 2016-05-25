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

def getCPPIf(self, model, namespace):
    """creates the interface which is used by the tools
    to access the processor core"""
    if not self.abi:
        return

    if model.startswith('acc'):
        regWriteCode = '.write_all'
        regReadCode = ''
    else:
        regWriteCode = '.write_force'
        regReadCode = '.read_force()'

    wordType = self.bitSizes[1]
    includes = wordType.getIncludes()
    pipeRegisterType = cxx_writer.Type('PipelineRegister', '#include \"registers.hpp\"')

    instrHistType = cxx_writer.Type('HistoryInstrType', 'modules/instruction.hpp')
    histQueueType = cxx_writer.TemplateType('boost::circular_buffer', [instrHistType], 'boost/circular_buffer.hpp')

    ifClassElements = []
    initElements = []
    baseInstrConstrParams = []

    ####################################################
    # Lets first of all declare the variables and the attributes;
    # they are mainly references to the corresponding elements
    # of the processor or of the pipeline stage
    ####################################################
    progLimitAttr = cxx_writer.Attribute('PROGRAM_LIMIT', wordType.makeRef(), 'pri')
    ifClassElements.append(progLimitAttr)
    baseInstrConstrParams.append(cxx_writer.Parameter('PROGRAM_LIMIT', wordType.makeRef()))
    initElements.append('PROGRAM_LIMIT(PROGRAM_LIMIT)')
    memIfType = cxx_writer.Type('MemoryInterface', '#include \"memory.hpp\"')
    for mem_name in self.abi.memories.keys():
        ifClassElements.append(cxx_writer.Attribute(mem_name, memIfType.makeRef(), 'pri'))
        baseInstrConstrParams.append(cxx_writer.Parameter(mem_name, memIfType.makeRef()))
        initElements.append(mem_name + '(' + mem_name + ')')
    if (self.regs or self.regBanks):
        from registerWriter import registerContainerType
        ifClassElements.append(cxx_writer.Attribute('R', registerContainerType.makeRef(), 'pri'))
        baseInstrConstrParams.append(cxx_writer.Parameter('R', registerContainerType.makeRef()))
        initElements.append('R(R)')
    # TODO
    '''
    if model.startswith('acc'):
        curRegBType = pipeRegisterType
        for reg in self.regs:
            attribute = cxx_writer.Attribute(reg.name, curRegBType.makeRef(), 'pri')
            baseInstrConstrParams.append(cxx_writer.Parameter(reg.name, curRegBType.makeRef()))
            initElements.append(reg.name + '(' + reg.name + ')')
            ifClassElements.append(attribute)
        for regB in self.regBanks:
            if (regB.constValue and len(regB.constValue) < regB.numRegs)  or ((regB.delay and len(regB.delay) < regB.numRegs) and not model.startswith('acc')):
                if model.startswith('acc'):
                    curRegBType = pipeRegisterType.makePointer()
                else:
                    curRegBType = resourceType[regB.name].makeRef()
            else:
                if model.startswith('acc'):
                    curRegBType = pipeRegisterType.makePointer()
                else:
                    curRegBType = resourceType[regB.name]
            attribute = cxx_writer.Attribute(regB.name, curRegBType, 'pri')
            baseInstrConstrParams.append(cxx_writer.Parameter(regB.name, curRegBType))
            initElements.append(regB.name + '(' + regB.name + ')')
            ifClassElements.append(attribute)
        for alias in self.aliasRegs:
            attribute = cxx_writer.Attribute(alias.name, resourceType[alias.name].makeRef(), 'pri')
            baseInstrConstrParams.append(cxx_writer.Parameter(alias.name, resourceType[alias.name].makeRef()))
            initElements.append(alias.name + '(' + alias.name + ')')
            ifClassElements.append(attribute)
        for aliasB in self.aliasRegBanks:
            attribute = cxx_writer.Attribute(aliasB.name, resourceType[aliasB.name].makePointer(), 'pri')
            baseInstrConstrParams.append(cxx_writer.Parameter(aliasB.name, resourceType[aliasB.name].makePointer()))
            initElements.append(aliasB.name + '(' + aliasB.name + ')')
            ifClassElements.append(attribute)'''
    attribute = cxx_writer.Attribute('instr_executing', cxx_writer.boolType.makeRef(), 'pri')
    baseInstrConstrParams.append(cxx_writer.Parameter('instr_executing', cxx_writer.boolType.makeRef()))
    initElements.append('instr_executing(instr_executing)')
    ifClassElements.append(attribute)
    if self.systemc:
        attribute = cxx_writer.Attribute('instr_end_event', cxx_writer.sc_eventType.makeRef(), 'pri')
        baseInstrConstrParams.append(cxx_writer.Parameter('instr_end_event', cxx_writer.sc_eventType.makeRef()))
        initElements.append('instr_end_event(instr_end_event)')
        ifClassElements.append(attribute)
    instHistoryQueueAttr = cxx_writer.Attribute('history_queue', histQueueType.makeRef(), 'pri')
    ifClassElements.append(instHistoryQueueAttr)
    baseInstrConstrParams.append(cxx_writer.Parameter('history_queue', histQueueType.makeRef()))
    initElements.append('history_queue(history_queue)')

    ###############################################################
    # Now lets move to the actual implementation of the methods which
    # enable communication of the interface with the processor
    ###############################################################
    if self.isBigEndian:
        endianessCode = cxx_writer.Code('return false;')
    else:
        endianessCode = cxx_writer.Code('return true;')
    endianessMethod = cxx_writer.Method('is_little_endian', endianessCode, cxx_writer.boolType, 'pu', noException = True, const = True)
    ifClassElements.append(endianessMethod)

    # Here are the methods used to discriminate when an instruction is executing or not
    if self.abi.procIdCode:
        processorIDCode = cxx_writer.Code('return (' + self.abi.procIdCode + ');\n')
        processorIDMethod = cxx_writer.Method('get_id', processorIDCode, cxx_writer.intType, 'pu', noException = True, const = True)
        ifClassElements.append(processorIDMethod)
    instrExecutingCode = cxx_writer.Code('return this->instr_executing;')
    instrExecutingMethod = cxx_writer.Method('is_executing_instr', instrExecutingCode, cxx_writer.boolType, 'pu', noException = True, const = True)
    ifClassElements.append(instrExecutingMethod)
    if self.systemc:
        waitInstrEndCode = cxx_writer.Code('if (this->instr_executing) {\nwait(this->instr_end_event);\n}\n')
        waitInstrEndCode.addInclude('systemc.h')
    else:
        waitInstrEndCode = cxx_writer.Code('while(this->instr_executing) {\n;\n}\n')
    waitInstrEndMethod = cxx_writer.Method('wait_instr_end', waitInstrEndCode, cxx_writer.voidType, 'pu', noException = True, const = True)
    ifClassElements.append(waitInstrEndMethod)

    if self.abi.preCallCode:
        ifClassElements.append(cxx_writer.Method('pre_call', cxx_writer.Code(self.abi.preCallCode), cxx_writer.voidType, 'pu', noException = True))
    if self.abi.postCallCode:
        ifClassElements.append(cxx_writer.Method('post_call', cxx_writer.Code(self.abi.postCallCode), cxx_writer.voidType, 'pu', noException = True))
    if self.abi.returnCallReg:
        returnCallCode = ''
        for returnReg in self.abi.returnCallReg:
            returnCallCode += returnReg[0] + regWriteCode + '(' + returnReg[1] + ' + ' + str(returnReg[2]) + ');\n'
        ifClassElements.append(cxx_writer.Method('return_from_call', cxx_writer.Code(returnCallCode), cxx_writer.voidType, 'pu', noException = True))

    # Here is the code for recognizing if we are in the routine entry or
    # exit; we behave like a state machine,moving to the beginning when
    # an instruction out of the sequence is met
    entryStateAttribute = cxx_writer.Attribute('routine_entry_state', cxx_writer.intType, 'pri')
    ifClassElements.append(entryStateAttribute)
    exitStateAttribute = cxx_writer.Attribute('routine_exit_state', cxx_writer.intType, 'pri')
    ifClassElements.append(exitStateAttribute)
    exitValueAttribute = cxx_writer.Attribute('exit_value', cxx_writer.uintType, 'pri')
    ifClassElements.append(exitValueAttribute)
    vector_strType = cxx_writer.TemplateType('std::vector', [cxx_writer.stringType], 'vector')
    vector_v_strType = cxx_writer.TemplateType('std::vector', [vector_strType], 'vector')
    entrySequenceAttribute = cxx_writer.Attribute('routine_entry_sequence', vector_v_strType, 'pri')
    ifClassElements.append(entrySequenceAttribute)
    exitSequenceAttribute = cxx_writer.Attribute('routine_exit_sequence', vector_v_strType, 'pri')
    ifClassElements.append(exitSequenceAttribute)
    routineStatesInit = """this->routine_exit_state = 0;
    this->routine_entry_state = 0;
    std::vector<std::string> temp_vec;
    """
    from isa import Instruction
    for instrList in self.abi.callInstr:
        routineStatesInit += 'temp_vec.clear();\n'
        if not instrList:
            routineStatesInit += 'temp_vec.push_back("");\n'
        elif isinstance(instrList, Instruction):
            routineStatesInit += 'temp_vec.push_back("' + instrList.name + '");\n'
        else:
            for instr in instrList:
                routineStatesInit += 'temp_vec.push_back("' + instr.name + '");\n'
        routineStatesInit += 'this->routine_entry_sequence.push_back(temp_vec);\n'
    for instrList in self.abi.returnCallInstr:
        routineStatesInit += 'temp_vec.clear();\n'
        if not instrList:
            routineStatesInit += 'temp_vec.push_back("");\n'
        elif isinstance(instrList, Instruction):
            routineStatesInit += 'temp_vec.push_back("' + instrList.name + '");\n'
        else:
            for instr in instrList:
                routineStatesInit += 'temp_vec.push_back("' + instr.name + '");\n'
        routineStatesInit += 'this->routine_exit_sequence.push_back(temp_vec);\n'
    instructionBaseType = cxx_writer.Type('InstructionBase', 'modules/instruction.hpp')
    baseInstrParam = cxx_writer.Parameter('instr', instructionBaseType.makePointer().makeConst())
    isRoutineEntryBody = """std::vector<std::string> next_names = this->routine_entry_sequence[this->routine_entry_state];
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
    isRoutineEntryCode = cxx_writer.Code(isRoutineEntryBody)
    isRoutineEntryMethod = cxx_writer.Method('is_routine_entry', isRoutineEntryCode, cxx_writer.boolType, 'pu', [baseInstrParam], noException = True)
    ifClassElements.append(isRoutineEntryMethod)
    isRoutineExitBody = """std::vector<std::string> next_names = this->routine_exit_sequence[this->routine_exit_state];
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
    isRoutineExitCode = cxx_writer.Code(isRoutineExitBody)
    isRoutineExitMethod = cxx_writer.Method('is_routine_exit', isRoutineExitCode, cxx_writer.boolType, 'pu', [baseInstrParam], noException = True)
    ifClassElements.append(isRoutineExitMethod)

    # Here I add the methods mecessary to save and restore the complete
    # processor status (useful, for example, to implement hardware context-switches,
    # or simulation chepointing)
    totalStateSize = 0
    for reg in self.regs:
        totalStateSize += reg.bitWidth/self.byteSize
    for regB in self.regBanks:
        totalStateSize += (regB.bitWidth*regB.numRegs)/self.byteSize
    getStateBody = 'unsigned char* cur_state = new unsigned char[' + str(totalStateSize) + '];\n'
    getStateBody += 'unsigned char* cur_state_temp = cur_state;\n'

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
            getStateBody += '*((' + str(regWType.makePointer()) + ')cur_state_temp) = ' + reg.name + regReadCode + ';\ncur_state_temp += ' + str(reg.bitWidth/self.byteSize) + ';\n'
    for regB in self.regBanks:
        regWType = resolveBitType('BIT<' + str(regB.bitWidth) + '>')
        if not regB.name in stateIgnoreRegs.keys():
            for i in range(0, regB.numRegs):
                getStateBody += '*((' + str(regWType.makePointer()) + ')cur_state_temp) = ' + regB.name + '[' + str(i) + ']' + regReadCode + ';\ncur_state_temp += ' + str(regB.bitWidth/self.byteSize) + ';\n'
        else:
            for i in range(0, regB.numRegs):
                if i not in stateIgnoreRegs[regB.name]:
                    getStateBody += '*((' + str(regWType.makePointer()) + ')cur_state_temp) = ' + regB.name + '[' + str(i) + ']' + regReadCode + ';\ncur_state_temp += ' + str(regB.bitWidth/self.byteSize) + ';\n'
    getStateBody += 'return cur_state;'
    getStateCode = cxx_writer.Code(getStateBody)
    getStateMethod = cxx_writer.Method('get_state', getStateCode, cxx_writer.ucharPtrType, 'pu', noException = True, const = True)
    ifClassElements.append(getStateMethod)
    setStateBody = 'unsigned char* cur_state_temp = state;\n'
    for reg in self.regs:
        if not reg.name in self.abi.stateIgnoreRegs:
            regWType = resolveBitType('BIT<' + str(reg.bitWidth) + '>')
            setStateBody += reg.name + regWriteCode + '(*((' + str(regWType.makePointer()) + ')cur_state_temp));\ncur_state_temp += ' + str(reg.bitWidth/self.byteSize) + ';\n'
    for regB in self.regBanks:
        regWType = resolveBitType('BIT<' + str(regB.bitWidth) + '>')
        if not regB.name in stateIgnoreRegs.keys():
            for i in range(0, regB.numRegs):
                setStateBody += regB.name + '[' + str(i) + ']' + regWriteCode + '(*((' + str(regWType.makePointer()) + ')cur_state_temp));\ncur_state_temp += ' + str(regB.bitWidth/self.byteSize) + ';\n'
        else:
            for i in range(0, regB.numRegs):
                if i not in stateIgnoreRegs[regB.name]:
                    setStateBody += regB.name + '[' + str(i) + ']' + regWriteCode + '(*((' + str(regWType.makePointer()) + ')cur_state_temp));\ncur_state_temp += ' + str(regB.bitWidth/self.byteSize) + ';\n'
    setStateCode = cxx_writer.Code(setStateBody)
    stateParam = cxx_writer.Parameter('state', cxx_writer.ucharPtrType)
    setStateMethod = cxx_writer.Method('set_state', setStateCode, cxx_writer.voidType, 'pu', [stateParam], noException = True)
    ifClassElements.append(setStateMethod)

    exitValueCode = cxx_writer.Code('this->exit_value = value;')
    exitValueParam = cxx_writer.Parameter('value', wordType)
    exitValueMethod = cxx_writer.Method('set_exit_value', exitValueCode, cxx_writer.voidType, 'pu', [exitValueParam], noException = True)
    ifClassElements.append(exitValueMethod)
    exitValueCode = cxx_writer.Code('return this->exit_value;')
    exitValueMethod = cxx_writer.Method('get_exit_value', exitValueCode, wordType, 'pu', noException = True)
    ifClassElements.append(exitValueMethod)

    codeLimitCode = cxx_writer.Code('return this->PROGRAM_LIMIT;')
    codeLimitMethod = cxx_writer.Method('get_code_limit', codeLimitCode, wordType, 'pu')
    ifClassElements.append(codeLimitMethod)

    for elem in [self.abi.LR, self.abi.PC, self.abi.SP, self.abi.FP, self.abi.RetVal]:
        if not elem:
            continue
        readElemBody = 'return ' + elem
        if self.abi.offset.has_key(elem):
            readElemBody += ' + ' + str(self.abi.offset[elem])
        readElemBody += ';'
        readElemCode = cxx_writer.Code(readElemBody)
        readElemCode.addInclude(includes)
        readElemMethod = cxx_writer.Method('read_' + self.abi.name[elem], readElemCode, wordType, 'pu', noException = True, const = True)
        ifClassElements.append(readElemMethod)
        setElemBody = elem + regWriteCode + '(new_value);'
        setElemCode = cxx_writer.Code(setElemBody)
        setElemCode.addInclude(includes)
        setElemParam = cxx_writer.Parameter('new_value', wordType.makeRef().makeConst())
        setElemMethod = cxx_writer.Method('set_' + self.abi.name[elem], setElemCode, cxx_writer.voidType, 'pu', [setElemParam], noException = True)
        ifClassElements.append(setElemMethod)
    vectorType = cxx_writer.TemplateType('std::vector', [wordType], 'vector')
    readArgsBody = str(vectorType) + ' args;\n'
    for arg in self.abi.args:
        readArgsBody += 'args.push_back(this->' + arg
        if self.abi.offset.has_key(arg) and not model.startswith('acc'):
            readArgsBody += ' + ' + str(self.abi.offset[arg])
        readArgsBody += ');\n'
    readArgsBody += 'return args;\n'
    readArgsCode = cxx_writer.Code(readArgsBody)
    readArgsCode.addInclude(includes)
    readArgsMethod = cxx_writer.Method('read_args', readArgsCode, vectorType, 'pu', noException = True, const = True)
    ifClassElements.append(readArgsMethod)
    setArgsBody = 'if (args.size() > ' + str(len(self.abi.args)) + ') {\nTHROW_EXCEPTION(\"Too many arguments for processor ABI, given \" << args.size() << \", expected up to ' + str(len(self.abi.args)) + ' .\");\n}\n'
    setArgsBody += str(vectorType) + '::const_iterator arg_it = args.begin(), arg_end = args.end();\n'
    for arg in self.abi.args:
        setArgsBody += 'if (arg_it != arg_end) {\n'
        setArgsBody += arg + regWriteCode + '(*arg_it'
        if self.abi.offset.has_key(arg) and not model.startswith('acc'):
            setArgsBody += ' - ' + str(self.abi.offset[arg])
        setArgsBody += ');\narg_it++;\n}\n'
    setArgsCode = cxx_writer.Code(setArgsBody)
    setArgsParam = cxx_writer.Parameter('args', vectorType.makeRef().makeConst())
    setArgsMethod = cxx_writer.Method('set_args', setArgsCode, cxx_writer.voidType, 'pu', [setArgsParam], noException = True)
    ifClassElements.append(setArgsMethod)
    maxGDBId = 0
    readGDBRegBody = 'switch(gdb_id) {\n'
    sortedGDBRegs = sorted(self.abi.regCorrespondence.items(), lambda x,y: cmp(x[1], y[1]))
    for reg, gdbId in sortedGDBRegs:
        if gdbId > maxGDBId:
            maxGDBId = gdbId
        readGDBRegBody += 'case ' + str(gdbId) + ': {\n'
        readGDBRegBody += 'return ' + reg
        if self.abi.offset.has_key(reg) and not model.startswith('acc'):
            readGDBRegBody += ' + ' + str(self.abi.offset[reg])
        readGDBRegBody += ';\nbreak;}\n'
    readGDBRegBody += 'default: {\nreturn 0;\n}\n}\n'
    readGDBRegCode = cxx_writer.Code(readGDBRegBody)
    readGDBRegCode.addInclude(includes)
    readGDBRegParam = cxx_writer.Parameter('gdb_id', cxx_writer.uintType.makeRef().makeConst())
    readGDBRegMethod = cxx_writer.Method('read_gdb_reg', readGDBRegCode, wordType, 'pu', [readGDBRegParam], noException = True, const = True)
    ifClassElements.append(readGDBRegMethod)
    nGDBRegsCode = cxx_writer.Code('return ' + str(maxGDBId + 1) + ';')
    nGDBRegsMethod = cxx_writer.Method('num_gdb_regs', nGDBRegsCode, cxx_writer.uintType, 'pu', noException = True, const = True)
    ifClassElements.append(nGDBRegsMethod)
    setGDBRegBody = 'switch(gdb_id) {\n'
    for reg, gdbId in sortedGDBRegs:
        setGDBRegBody += 'case ' + str(gdbId) + ': {\n'
        setGDBRegBody += reg + regWriteCode + '(new_value'
        setGDBRegBody += ');\nbreak;}\n'
    setGDBRegBody += 'default: {\nTHROW_EXCEPTION(\"Register corresponding to GDB id \" << gdb_id << \" not found.\");\n}\n}\n'
    setGDBRegCode = cxx_writer.Code(setGDBRegBody)
    setGDBRegCode.addInclude(includes)
    setGDBRegParam1 = cxx_writer.Parameter('new_value', wordType.makeRef().makeConst())
    setGDBRegParam2 = cxx_writer.Parameter('gdb_id', cxx_writer.uintType.makeRef().makeConst())
    setGDBRegMethod = cxx_writer.Method('set_gdb_reg', setGDBRegCode, cxx_writer.voidType, 'pu', [setGDBRegParam1, setGDBRegParam2], noException = True)
    ifClassElements.append(setGDBRegMethod)
    readMemBody = ''
    if not self.abi.memories:
        readMemBody += 'THROW_EXCEPTION(\"No memory accessible from the ABI or processor ' + self.name + '.\");'
    else:
        if len(self.abi.memories) == 1:
            readMemBody += 'return this->' + self.abi.memories.keys()[0] + '.read_word_dbg(address);'
        else:
            for mem_name, mem_range in self.abi.memories.items():
                readMemBody += 'if (address >= ' + hex(mem_range[0]) + ' && address <= ' + hex(mem_range[1]) + ') {\n'
                readMemBody += 'return this->' + self.abi.memories.keys()[0] + '.read_word_dbg(address);\n} else '
            readMemBody += ' {\nTHROW_EXCEPTION(\"Address \" << std::hex << address << \" out of range.\");\n}'
    readMemCode = cxx_writer.Code(readMemBody)
    readMemParam1 = cxx_writer.Parameter('address', wordType.makeRef().makeConst())
    readMemMethod = cxx_writer.Method('read_mem', readMemCode, wordType, 'pu', [readMemParam1])
    ifClassElements.append(readMemMethod)

    readByteMemBody = ''
    if not self.abi.memories:
        readByteMemBody += 'THROW_EXCEPTION(\"No memory accessible from the ABI or processor ' + self.name + '.\");'
    else:
        if len(self.abi.memories) == 1:
            readByteMemBody += 'return this->' + self.abi.memories.keys()[0] + '.read_byte_dbg(address);'
        else:
            for mem_name, mem_range in self.abi.memories.items():
                readByteMemBody += 'if (address >= ' + hex(mem_range[0]) + ' && address <= ' + hex(mem_range[1]) + ') {\n'
                readByteMemBody += 'return this->' + self.abi.memories.keys()[0] + '.read_byte_dbg(address);\n} else '
            readByteMemBody += ' {\nTHROW_EXCEPTION(\"Address \" << std::hex << address << \" out of range.\");\n}'
    readByteMemCode = cxx_writer.Code(readByteMemBody)
    readByteMemParam = cxx_writer.Parameter('address', wordType.makeRef().makeConst())
    readByteMemMethod = cxx_writer.Method('read_char_mem', readByteMemCode, cxx_writer.ucharType, 'pu', [readByteMemParam])
    ifClassElements.append(readByteMemMethod)

    writeMemBody = ''
    if not self.abi.memories:
        writeMemBody += 'THROW_EXCEPTION(\"No memory accessible from the ABI or processor ' + self.name + '.\");'
    else:
        if len(self.abi.memories) == 1:
            writeMemBody += 'this->' + self.abi.memories.keys()[0] + '.write_word_dbg(address, datum);'
        else:
            for mem_name, mem_range in self.abi.memories.items():
                writeMemBody += 'if (address >= ' + hex(mem_range[0]) + ' && address <= ' + hex(mem_range[1]) + ') {\n'
                writeMemBody += 'this->' + self.abi.memories.keys()[0] + '.write_word_dbg(address, datum);\n} else '
            writeMemBody += ' {\nTHROW_EXCEPTION(\"Address \" << std::hex << address << \" out of range.\");\n}'
    writeMemCode = cxx_writer.Code(writeMemBody)
    writeMemCode.addInclude('common/report.hpp')
    writeMemParam1 = cxx_writer.Parameter('address', wordType.makeRef().makeConst())
    writeMemParam2 = cxx_writer.Parameter('datum', wordType)
    writeMemMethod = cxx_writer.Method('write_mem', writeMemCode, cxx_writer.voidType, 'pu', [writeMemParam1, writeMemParam2])
    ifClassElements.append(writeMemMethod)
    writeMemBody = ''
    if not self.abi.memories:
        writeMemBody += 'THROW_EXCEPTION(\"No memory accessible from the ABI or processor ' + self.name + '.\");'
    else:
        if len(self.abi.memories) == 1:
            writeMemBody += 'this->' + self.abi.memories.keys()[0] + '.write_byte_dbg(address, datum);'
        else:
            for mem_name, mem_range in self.abi.memories.items():
                writeMemBody += 'if (address >= ' + hex(mem_range[0]) + ' && address <= ' + hex(mem_range[1]) + ') {\n'
                writeMemBody += 'this->' + self.abi.memories.keys()[0] + '.write_byte_dbg(address, datum);\n} else '
            writeMemBody += ' {\nTHROW_EXCEPTION(\"Address \" << std::hex << address << \" out of range.\");\n}'
    writeMemCode = cxx_writer.Code(writeMemBody)
    writeMemParam1 = cxx_writer.Parameter('address', wordType.makeRef().makeConst())
    writeMemParam2 = cxx_writer.Parameter('datum', cxx_writer.ucharType)
    writeMemMethod = cxx_writer.Method('write_char_mem', writeMemCode, cxx_writer.voidType, 'pu', [writeMemParam1, writeMemParam2])
    ifClassElements.append(writeMemMethod)

    getInstructionHistoryCode = cxx_writer.Code('return this->history_queue;')
    getInstructionHistoryMethod = cxx_writer.Method('get_history', getInstructionHistoryCode, histQueueType.makeRef(), 'pu')
    ifClassElements.append(getInstructionHistoryMethod)

    ABIIfType = cxx_writer.TemplateType('ABIIf', [wordType], 'modules/abi_if.hpp')
    ifClassDecl = cxx_writer.ClassDeclaration('Interface', ifClassElements, [ABIIfType], namespaces = [namespace])
    ifClassDecl.addDocString(brief = 'Interface Class', detail = 'Creates the interface used by TRAP-Gen tools to access the processor core.')
    publicIfConstr = cxx_writer.Constructor(cxx_writer.Code(routineStatesInit), 'pu', baseInstrConstrParams, initElements)
    emptyBody = cxx_writer.Code('')
    opDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
    ifClassDecl.addDestructor(opDestr)
    ifClassDecl.addConstructor(publicIfConstr)
    return ifClassDecl
