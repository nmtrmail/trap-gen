################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     isaWriter.py
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
# Globals and Helpers
################################################################################
archWordType = None

def toBinStr(intNum, maxLen = -1):
    """Converts an integer to a bitstring. maxLen is used only if the number to
    be converted is negative."""

    bitStr = []
    negative = (intNum < 0)
    intNum = abs(intNum)
    if negative:
        intNum = intNum - 1
    while intNum > 0:
        bitStr.append(str(intNum % 2))
        intNum = intNum / 2
    if negative:
        if maxLen < 0:
            raise Exception('Specify maximum number of bits for converting negative number ' + str(intNum) + '.')
        if len(bitStr) >= maxLen:
            raise Exception('Not enough bits specified for converting negative number ' + str(intNum) + '.')
        for i in range(len(bitStr), maxLen):
            bitStr.append(0)
        for i in range(0, len(bitStr)):
            if bitStr[i] == '1':
                bitStr[i] = '0'
            else:
                bitStr[i] = '1'
    bitStr.reverse()
    return bitStr

################################################################################
# HelperMethod Instruction Class Method
################################################################################
def getCPPInstrMethod(self, model, namespace):
    """Returns the code implementing a helper method."""

    # We need a new variable in case we are generating more than one model.
    methodCode = cxx_writer.Code(self.code.code)
    methodCode.addInclude('common/report.hpp')
    for var in self.localvars:
        methodCode.addVariable(var)

    methodMethod = cxx_writer.Method(self.name, methodCode, self.retType, 'public', self.parameters, inline = self.inline, noException = not self.exception)

    return methodMethod

################################################################################
# HelperOperation Class
################################################################################
# Contains, for each behavior, the type corresponding to the class which defines
# it. If a behavior is not here it means that it must be explicitly inlined
# in the instruction itself
def getCPPOperation(self, namespace):
    """Returns the code implementing a helper operation."""

    # We need a new variable in case we are generating more than one model.
    operationCode = cxx_writer.Code('unsigned num_cycles = 0;\n\n' + self.code.code + '\nreturn num_cycles;\n')
    operationCode.addInclude('common/report.hpp')
    for var in self.localvars:
        operationCode.addVariable(var)

    from registerWriter import aliasType
    operationCallParams = []
    for elem in self.archElems:
        operationCallParams.append(cxx_writer.Parameter(elem, aliasType.makeRef()))
        operationCallParams.append(cxx_writer.Parameter(elem + '_bit', cxx_writer.uintRefType))
    for elem in self.archVars:
        operationCallParams.append(cxx_writer.Parameter(elem, cxx_writer.uintRefType))
    for var in self.instrvars:
        operationCallParams.append(cxx_writer.Parameter(var.name, var.varType.makeRef()))
    operationCallMethod = cxx_writer.Method(self.name, operationCode, cxx_writer.uintType, 'protected', operationCallParams, noException = not self.exception)

    operationClass = cxx_writer.ClassDeclaration(self.name + 'Op', [operationCallMethod], virtual_superclasses = [cxx_writer.Type('Instruction')], namespaces = [namespace])

    from procWriter import instrCtorParams, instrCtorValues
    operationCtor = cxx_writer.Constructor(cxx_writer.Code(''), 'public', parameters = instrCtorParams, initList = ['Instruction(' + instrCtorValues + ')'])
    operationClass.addConstructor(operationCtor)

    return operationClass

################################################################################
# Instruction Classes
################################################################################
def getCPPInstructions(self, processor, model, trace, combinedTrace, namespace):
    """Returns the special instruction classes Base, Invalid and NOP as well
    as classes for HelperOperations and each individual instruction."""

    from procWriter import instrCtorParams, instrCtorValues
    from registerWriter import registerType, registerContainerType
    memoryType = cxx_writer.Type('MemoryInterface', '#include \"memory.hpp\"')
    instructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')

    emptyBody = cxx_writer.Code('')
    instrClasses = []

    # Any special defines
    for i in self.defines:
        instrClasses.append(cxx_writer.Define(i))

    #---------------------------------------------------------------------------
    ## @name Instruction Class
    #  @{

    from procWriter import instrAttrs, instrCtorParams
    instructionMembers = []

    # Methods: set_params()
    setParamsParam = cxx_writer.Parameter('bitstring', processor.bitSizes[1].makeRef().makeConst())
    setParamsMethod = cxx_writer.Method('set_params', emptyBody, cxx_writer.voidType, 'public', [setParamsParam], noException = True, virtual = True)
    instructionMembers.append(setParamsMethod)

    # Methods: replicate()
    replicateMethod = cxx_writer.Method('replicate', emptyBody, instructionType.makePointer(), 'public', parameters = [cxx_writer.Parameter('instr', instructionType.makePointer(), initValue = 'NULL')], noException = True, const = True, pure = True)
    instructionMembers.append(replicateMethod)

    # Methods: behavior()
    if not model.startswith('acc'):
        behaviorMethod = cxx_writer.Method('behavior', cxx_writer.Code('return 0;\n'), cxx_writer.uintType, 'public', virtual = True)
        instructionMembers.append(behaviorMethod)
    else:
        for pipeStage in processor.pipes:
            behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, cxx_writer.Code('return 0;\n'), cxx_writer.uintType, 'public', virtual = True)
            instructionMembers.append(behaviorMethod)

    # Methods: check_regs(), lock_regs(), unlock_regs()
    if model.startswith('acc'):
        checkRegsMethod = cxx_writer.Method('check_regs', cxx_writer.Code('return 0;\n'), cxx_writer.uintType, 'public', virtual = True)
        instructionMembers.append(checkRegsMethod)
        lockRegsMethod = cxx_writer.Method('lock_regs', cxx_writer.Code('return true;\n'), cxx_writer.boolType, 'public', virtual = True)
        instructionMembers.append(lockRegsMethod)
        unlockRegsMethod = cxx_writer.Method('unlock_regs', cxx_writer.Code('return true;\n'), cxx_writer.boolType, 'public', virtual = True)
        instructionMembers.append(unlockRegsMethod)

        # Attributes
        fetchPCAttr = cxx_writer.Attribute('fetch_PC', processor.bitSizes[1], 'public')
        instructionMembers.append(fetchPCAttr)
        inPipelineAttr = cxx_writer.Attribute('in_pipeline', cxx_writer.boolType, 'public')
        instructionMembers.append(inPipelineAttr)
        toDestroyAttr = cxx_writer.Attribute('to_destroy', cxx_writer.boolType, 'public')
        instructionMembers.append(toDestroyAttr)

    # Methods: print_trace(), print_busy_regs()
    if trace:
        # I have to print the value of all the registers in the processor
        traceStage = processor.pipes[-1]
        Code = ''

        if model.startswith('acc'):
            # Renames all resources so that their usage can be transparent to
            # the developer.
            Code += 'R.set_stage(' + str(processor.pipes.index(traceStage)) + ');\n'

        if not combinedTrace:
            if not processor.systemc and not model.startswith('acc') and not model.endswith('AT'):
                Code += 'std::cerr << \"Simulated time: \" << std::dec << this->total_cycles << " cycles." << std::endl;\n'
            else:
                Code += 'std::cerr << \"Simulated time: \" << sc_time_stamp().to_double() << \'.\' << std::endl;\n'
        Code += 'std::cerr << \"Instruction: \" << this->get_name() << \'.\' << std::endl;\n'
        Code += 'std::cerr << \"Mnemonic: \" << this->get_mnemonic() << std::endl;\n'

        if self.traceRegs:
            bankNames = [i.name for i in processor.regBanks + processor.aliasRegBanks]
            for reg in self.traceRegs:
                if reg.name in bankNames:
                    Code += 'for (int reg_i = 0; reg_i < ' + str(reg.numRegs) + '; reg_i++) {\n'
                    Code += 'std::cerr << \"' + reg.name + '[\" << std::dec << reg_i << \"] = \" << std::hex << std::showbase << ' + reg.name + '[reg_i] << std::endl;\n}\n'
                else:
                    Code += 'std::cerr << \"' + reg.name + ' = \" << std::hex << std::showbase << ' + reg.name + ' << std::endl;\n'
        else:
            for reg in processor.regs:
                Code += 'std::cerr << \"' + reg.name + ' = \" << std::hex << std::showbase << ' + reg.name + ' << std::endl;\n'
            for regB in processor.regBanks:
                Code += 'for (int reg_i = 0; reg_i < ' + str(regB.numRegs) + '; reg_i++) {\n'
                Code += 'std::cerr << \"' + regB.name + '[\" << std::dec << reg_i << \"] = \" << std::hex << std::showbase << ' + regB.name + '[reg_i] << std::endl;\n}\n'
        Code += 'std::cerr << std::endl;\n'
        if model.startswith('acc'):
            # Renames back all resources so that their usage can be transparent
            # to the developer.
            Code += 'R.unset_stage();\n'
        printTraceBody = cxx_writer.Code(Code)
        printTraceMethod = cxx_writer.Method('print_trace', printTraceBody, cxx_writer.voidType, 'public')
        instructionMembers.append(printTraceMethod)

        if model.startswith('acc'):
            printBusyRegsMethod = cxx_writer.Method('print_busy_regs', cxx_writer.Code('return "";'), cxx_writer.stringType, 'public', virtual = True)
            instructionMembers.append(printBusyRegsMethod)

    # Methods: annul()
    # Stops the execution of the current operation.
    annulBody = cxx_writer.Code('throw annul_exception();')
    annulBody.addInclude('common/report.hpp')
    annulMethod = cxx_writer.Method('annul', annulBody, cxx_writer.voidType, 'public', inline = True)
    instructionMembers.append(annulMethod)

    # Methods: flush()
    if not model.startswith('acc'): flushBody = emptyBody
    else: flushBody = cxx_writer.Code('this->flush_pipeline = true;')
    flushMethod = cxx_writer.Method('flush', flushBody, cxx_writer.voidType, 'public', inline = True)
    instructionMembers.append(flushMethod)

    # Methods: stall()
    stallParam = cxx_writer.Parameter('num_cycles', processor.bitSizes[1].makeRef().makeConst())
    Code = 'this->'
    if model.startswith('acc'): Code += 'num_stage_cycles'
    else: Code += 'num_instr_cycles'
    Code += ' += num_cycles;\n'
    stallBody = cxx_writer.Code(Code)
    stallMethod = cxx_writer.Method('stall', stallBody, cxx_writer.voidType, 'public', [stallParam], inline = True)
    instructionMembers.append(stallMethod)

    # Methods: get_count_std_alloc()
    # TODO: To eliminate, only for statistics
    #returnStatsMethod = cxx_writer.Method('get_count_my_alloc', emptyBody, cxx_writer.uintType, 'public', virtual = True)
    #instructionMembers.append(returnStatsMethod )
    #returnStatsMethod = cxx_writer.Method('get_count_std_alloc', emptyBody, cxx_writer.uintType, 'public', virtual = True)
    #instructionMembers.append(returnStatsMethod )

    # Methods: HelperMethods
    for helperMethod in self.methods:
        if helperMethod:
            instructionMembers.append(helperMethod.getCPPInstrMethod(model, namespace))

    # Attributes
    if not model.startswith('acc'):
        instructionMembers.append(cxx_writer.Attribute('num_instr_cycles', cxx_writer.uintType, 'public'))
        instrCtorCode = 'this->num_instr_cycles = 0;'
    if model.startswith('acc'):
        instructionMembers.append(cxx_writer.Attribute('num_stage_cycles', cxx_writer.uintType, 'protected'))
        instructionMembers.append(cxx_writer.Attribute('flush_pipeline', cxx_writer.boolType, 'public'))
        instrCtorCode = 'this->num_stage_cycles = 0;\nthis->flush_pipeline = false;\nthis->fetch_PC = 0;\nthis->to_destroy = false;\nthis->in_pipeline = false;\n'

    # Constructors and Destructors
    instrCtorInit = []
    for attr in instrAttrs:
        instrCtorInit.append(attr.name + '(' + attr.name + ')')
    for constant in self.constants:
        instructionMembers.append(cxx_writer.Attribute(constant[1], constant[0].makeConst(), 'protected'))
        instrCtorInit.append(constant[1] + '(' + str(constant[2]) + ')')
    instrCtor = cxx_writer.Constructor(cxx_writer.Code(instrCtorCode), 'public', parameters = instrCtorParams, initList = instrCtorInit)
    instrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

    # Class
    instructionBaseType = cxx_writer.Type('InstructionBase', 'modules/instruction.hpp')
    instructionClass = cxx_writer.ClassDeclaration('Instruction', instrAttrs + instructionMembers, [instructionBaseType], namespaces = [namespace])
    instructionClass.addDocString(brief = 'Instruction Class', detail = 'All individual instructions derive from this class.')
    instructionClass.addConstructor(instrCtor)
    instructionClass.addDestructor(instrDtor)
    instrClasses.append(instructionClass)

    ## @} Instruction Class
    #---------------------------------------------------------------------------
    ## @name InvalidInstruction Class
    #  @{

    invalidInstrMembers = []

    # Methods: get_id()
    getIdBody = cxx_writer.Code('return ' + str(len(self.instructions)) + ';')
    getIdMethod = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'public', noException = True, const = True)
    invalidInstrMembers.append(getIdMethod)

    # Methods: get_name()
    getNameBody = cxx_writer.Code('return \"Invalid\";')
    getNameMethod = cxx_writer.Method('get_name', getNameBody, cxx_writer.stringType, 'public', noException = True, const = True)
    invalidInstrMembers.append(getNameMethod)

    # Methods: get_mnemonic()
    getMnemonicBody = cxx_writer.Code('return \"invalid\";')
    getMnemonicMethod = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'public', noException = True, const = True)
    invalidInstrMembers.append(getMnemonicMethod)

    # Methods: replicate()
    replicateBody = cxx_writer.Code('return new InvalidInstruction(' + instrCtorValues + ');')
    replicateMethod = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'public', parameters = [cxx_writer.Parameter('instr', instructionType.makePointer(), initValue = 'NULL')], noException = True, const = True)
    invalidInstrMembers.append(replicateMethod)

    # Methods: behavior()
    behaviorCode = ''
    if model.startswith('func'):
        behaviorCode = 'THROW_EXCEPTION(\"Invalid instruction at PC=\" << std::hex << std::showbase << this->' + processor.fetchReg[0] + ' << \'.\''
        if processor.fetchReg[1] < 0:
            behaviorCode += str(processor.fetchReg[1])
        elif processor.fetchReg[1] > 0:
            behaviorCode += '+' + str(processor.fetchReg[1])
        behaviorCode += ');\nreturn 0;'
    else:
        behaviorCode = 'THROW_EXCEPTION(\"Invalid Instruction at PC=\" << std::hex << std::showbase << this->fetch_PC << \'.\');\nreturn 0;'
    behaviorBody = cxx_writer.Code(behaviorCode)
    if model.startswith('acc'):
        for pipeStage in processor.pipes:
            if pipeStage.checkUnknown:
                behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'public')
                invalidInstrMembers.append(behaviorMethod)
    else:
        behaviorMethod = cxx_writer.Method('behavior', behaviorBody, cxx_writer.uintType, 'public')
        invalidInstrMembers.append(behaviorMethod)

    # Constructors and Destructors
    invalidInstrCtor = cxx_writer.Constructor(emptyBody, 'public', parameters = instrCtorParams, initList = ['Instruction(' + instrCtorValues + ')'])
    invalidInstrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

    # Class
    invalidInstrClass = cxx_writer.ClassDeclaration('InvalidInstruction', invalidInstrMembers, [instructionClass.getType()], namespaces = [namespace])
    invalidInstrClass.addConstructor(invalidInstrCtor)
    invalidInstrClass.addDestructor(invalidInstrDtor)
    instrClasses.append(invalidInstrClass)

    ## @} InvalidInstruction Class
    #---------------------------------------------------------------------------
    ## @name NOPInstruction Class
    #  The NOP instruction is used whenever the pipeline is flushed.
    #  @{

    if model.startswith('acc'):
        NOPInstrMembers = []

        # Methods: get_id()
        getIdBody = cxx_writer.Code('return (unsigned)-1;')
        getIdMethod = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'public', noException = True, const = True)
        NOPInstrMembers.append(getIdMethod)

        # Methods: get_name()
        getNameBody = cxx_writer.Code('return \"NOP\";')
        getNameMethod = cxx_writer.Method('get_name', getNameBody, cxx_writer.stringType, 'public', noException = True, const = True)
        NOPInstrMembers.append(getNameMethod)

        # Methods: get_mnemonic()
        getMnemonicBody = cxx_writer.Code('return \"nop\";')
        getMnemonicMethod = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'public', noException = True, const = True)
        NOPInstrMembers.append(getMnemonicMethod)

        # Methods: replicate()
        replicateBody = cxx_writer.Code('return new NOPInstruction(' + instrCtorValues + ');')
        replicateMethod = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'public', parameters = [cxx_writer.Parameter('instr', instructionType.makePointer(), initValue = 'NULL')], noException = True, const = True)
        NOPInstrMembers.append(replicateMethod)

        # Methods: behavior()
        for pipeStage in processor.pipes:
            if self.nopBeh.has_key(pipeStage.name):
                defineCode = 'R.set_stage(' + str(processor.pipes.index(pipeStage)) + ');\n'
                undefineCode = 'R.unset_stage();\n'
                behaviorBody = cxx_writer.Code(defineCode + '\n' + self.nopBeh[pipeStage.name] + '\n' + undefineCode)
                behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'public')
                NOPInstrMembers.append(behaviorMethod)

        # Constructors and Destructors
        NOPInstrCtor = cxx_writer.Constructor(emptyBody, 'public', parameters = instrCtorParams, initList = ['Instruction(' + instrCtorValues + ')'])
        NOPInstrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

        # Class
        NOPInstructionClass = cxx_writer.ClassDeclaration('NOPInstruction', NOPInstrMembers, [instructionClass.getType()], namespaces = [namespace])
        NOPInstructionClass.addConstructor(NOPInstrCtor)
        NOPInstructionClass.addDestructor(NOPInstrDtor)
        instrClasses.append(NOPInstructionClass)

    ## @} NOPInstruction Class
    #---------------------------------------------------------------------------
    ## @name HelperOperation Classes
    #  @{

    helperOpDone = []
    for instr in self.instructions.values():
        for behaviors in instr.postbehaviors.values() + instr.prebehaviors.values():
            for behavior in behaviors:
                if not behavior.inline and not behavior.name in helperOpDone:
                    instrClasses.append(behavior.getCPPOperation(namespace))
                    helperOpDone.append(behavior.name)
    for helperOp in self.helperOps + [self.startup, self.shutdown]:
        if helperOp:
            instrClasses.append(helperOp.getCPPOperation(namespace))

    ## @} HelperOperation Class
    #---------------------------------------------------------------------------
    ## @name Instruction Classes
    #  @{

    for instr in self.instructions.values():
        instrClasses += instr.getCPPClass(model, processor, trace, combinedTrace, namespace)

    ## @} Instruction Classes
    #---------------------------------------------------------------------------

    return instrClasses

################################################################################
# Instruction Classes
################################################################################
def getCPPInstrMnemonic(obj, i):
    """Parses the instruction mnemonic definition and returns the code
    implementing Instruction::get_mnemonic(). The mnemonic can include strings,
    or instruction elements, possibly with choices or executable C-code:

    Strings: 'ADD'

    Instruction elements: '%rn'

    Choices, possibly with defaults: Useful when the mnemonic is different based
    on the values of certain bits.
    ('%bits': {int('01', 2): 'ONE', int('10', 2): 'TWO', int('11', 2): 'THREE', 'default': 'ZERO'})

    Nested choices:
    ('%bit0': {int('0', 2): ('%bit1': {int('0', 2): 'ZERO', int('1', 2): 'TWO'}),
              int('1', 2): ('%bit1': {int('0', 2): 'ONE', int('1', 2): 'THREE'})})

    Executable code: Useful when an instruction element needs preprocessing
    before it can be output, such as when fields have to be appended or bit
    operations applied to a given field. The specification should be a list
    containing '$' as the first element. Subsequent elements can be any of the
    mnemonic elements defined above. After replacing the structural elements,
    the resulting string should contain valid C++ code.
    ('$', '%instr_element_or_c_code', ...)
    ('$', '((', '%imm', ' >> (2 * ', '%rotate', ')) & (((unsigned)0xFFFFFFFF) >> (2 * ', '%rotate', '))) | ((', '%imm', ' << (32 - 2 * ', '%rotate', ')) & (((unsigned)0xFFFFFFFF) << (32 - 2 * ', '%rotate', ')))')
    """
    Code = ''
    if type(i) == str:
        # Instruction Element
        if i.startswith('%'):
            Code = 'oss << '
            # Register
            if i[1:] in obj.machineCode.bitCorrespondence.keys() + obj.bitCorrespondence.keys():
                Code += 'std::dec << this->' + i[1:] + '_bit'
            # Non-register
            else:
                Code += 'std::showbase << std::hex << this->' + i[1:]
            Code += ';\n'
        # String
        else:
            Code = 'oss << "' + i + '";\n'
    else:
        # Choice
        if i[0].startswith('%'):
            Code = 'switch(this->' + i[0][1:]
            if i[0][1:] in obj.machineCode.bitCorrespondence.keys() + obj.bitCorrespondence.keys():
                Code += '_bit'
            Code += ') {\n'
            for code, value in i[1].items():
                if code != 'default':
                    Code += 'case '
                Code += str(code) + ': {\n'
                if type(value) == str:
                    Code += 'oss << "' + value + '";\n'
                else:
                    Code += getCPPInstrMnemonic(obj, value)
                Code += 'break;}\n'
            Code += '}\n'
        # Executable Code
        elif i[0].startswith('$'):
            Code = 'oss << std::showbase << std::hex << ('
            for j in i[1:]:
                if j.startswith('%'):
                    Code += 'this->' + j[1:]
                    if j[1:] in obj.machineCode.bitCorrespondence.keys() + obj.bitCorrespondence.keys():
                        Code += '_bit'
                else:
                    Code += j
            Code += ');\n'
        else:
            raise Exception('Expected % as the first element of multi-word mnemonic in instruction ' + obj.name + '.')
    return Code

def getCPPInstr(self, model, processor, trace, combinedTrace, namespace):
    """Returns the class implementing a single instruction. Implements all
    abstract methods of the base instruction class."""

    from procWriter import instrCtorParams, instrCtorValues
    from registerWriter import registerType, aliasType
    instructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
    externalClock = processor.externalClock

    emptyBody = cxx_writer.Code('')
    instrBases = []
    instrMembers = []
    instrCtorInit = ['Instruction(' + instrCtorValues + ')']

    archVars = []
    for behaviors in self.postbehaviors.values() + self.prebehaviors.values():
        for beh in behaviors:
            if (model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun):
                archVars += beh.archVars

    #---------------------------------------------------------------------------
    ## @name Access and Modification Methods
    #  @{

    # Methods: get_id()
    getIdBody = cxx_writer.Code('return ' + str(self.id) + ';')
    getIdMethod = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'public', noException = True, const = True)
    instrMembers.append(getIdMethod)

    # Methods: get_name()
    getNameBody = cxx_writer.Code('return \"' + self.name + '\";')
    getNameMethod = cxx_writer.Method('get_name', getNameBody, cxx_writer.stringType, 'public', noException = True, const = True)
    instrMembers.append(getNameMethod)

    # Methods: get_mnemonic()
    # The instruction mnemonic returns the assembly code of the instruction. The
    # interface for defining mnemonics is powerful enough to enable some
    # processing of the instruction fields before output.
    Code = 'std::ostringstream oss (std::ostringstream::out);\n'
    for i in self.mnemonic:
        Code += getCPPInstrMnemonic(self, i)
    Code += 'return oss.str();'
    getMnemonicBody = cxx_writer.Code(Code)
    getMnemonicBody.addInclude('sstream')
    getMnemonicMethod = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'public', noException = True, const = True)
    instrMembers.append(getMnemonicMethod)

    # Methods: set_params()
    # The instruction does not receive the full machine-code string. Instead,
    # each instruction defines variables for the relevant fields, which are set
    # here. Immediate-value fields are saved as unsigned, registers are saved
    # twice: Both a variable for the register index as well as an alias to the
    # register are created, Fixed-value fields are not saved, except where a
    # HelperOperation, used by several similar instructions, uses these fields
    # for disambiguation.
    Code = ''
    # Fields coming from the instruction coding.
    for name, correspondence in self.machineCode.bitCorrespondence.items() + self.bitCorrespondence.items():
        instrMembers.append(cxx_writer.Attribute(name, aliasType, 'private'))
        instrMembers.append(cxx_writer.Attribute(name + '_bit', cxx_writer.uintType, 'private'))
        mask = ''
        for i in range(0, self.machineCode.bitPos[name]):
            mask += '0'
        for i in range(0, self.machineCode.bitLen[name]):
            mask += '1'
        for i in range(0, self.machineCode.instrLen - self.machineCode.bitPos[name] - self.machineCode.bitLen[name]):
            mask += '0'
        shiftAmm = self.machineCode.instrLen - self.machineCode.bitPos[name] - self.machineCode.bitLen[name]
        Code += 'this->' + name + '_bit = (bitstring & ' + hex(int(mask, 2)) + ')'
        if shiftAmm > 0:
            Code += ' >> ' + str(shiftAmm)
        Code += ';\n'
        #if processor.instructionCache:
            #updateMethodName = 'update_alias'
        #else:
            #updateMethodName = 'set_alias'
        updateMethodName = 'set_alias'
        if correspondence[1]:
            Code += 'this->' + name + '.' + updateMethodName +  '(' + correspondence[0] + '[' + str(correspondence[1]) + ' + this->' + name + '_bit]);\n'
        else:
            Code += 'this->' + name + '.' + updateMethodName +  '(' + correspondence[0] + '[this->' + name + '_bit]);\n'
    # Instruction-specific fields.
    for name, length in self.machineCode.bitFields:
        if name in self.machineCode.bitCorrespondence.keys() + self.bitCorrespondence.keys():
            continue
            # NOTE: This one-liner saved 50+ compilation errors:
            # Fixed bitfields of an instruction would otherwise not be passed.
            # This usually makes sense, except when we want to use a generic
            # operation, where the calling instructions possibly have different
            # fixed-values for a given bitfield, upon which the operation must
            # decide what to do. So we add an additional check where we see
            # whether the field will be used in some operation, in which case
            # we do keep the fixed-value of the bitfield as an instruction
            # member.
        if name in self.machineBits.keys() + self.machineCode.bitValue.keys() and name not in archVars:
            continue
        instrMembers.append(cxx_writer.Attribute(name, cxx_writer.uintType, 'private'))
        mask = ''
        for i in range(0, self.machineCode.bitPos[name]):
            mask += '0'
        for i in range(0, self.machineCode.bitLen[name]):
            mask += '1'
        for i in range(0, self.machineCode.instrLen - self.machineCode.bitPos[name] - self.machineCode.bitLen[name]):
            mask += '0'
        shiftAmm = self.machineCode.instrLen - self.machineCode.bitPos[name] - self.machineCode.bitLen[name]
        Code += 'this->' + name + ' = (bitstring & ' + hex(int(mask, 2)) + ')'
        if shiftAmm > 0:
            Code += ' >> ' + str(shiftAmm)
        Code += ';\n'
    setParamsBody = cxx_writer.Code(Code)
    setParamsParam = cxx_writer.Parameter('bitstring', processor.bitSizes[1].makeRef().makeConst())
    setParamsMethod = cxx_writer.Method('set_params', setParamsBody, cxx_writer.voidType, 'public', [setParamsParam], noException = True)
    instrMembers.append(setParamsMethod)

    # Methods: replicate()
    Code = self.name + '* new_instr = new ' + self.name + '(' + instrCtorValues + ');\n'
    Code += '\n// Set instruction fields.\n'
    Code += 'if (instr) {\n'
    Code += self.name + '* old_instr = dynamic_cast<' + self.name + '*>(instr);\n'
    Code += 'if (old_instr) {\n'
    for name, correspondence in self.machineCode.bitCorrespondence.items() + self.bitCorrespondence.items():
        Code += 'new_instr->' + name + '_bit = old_instr->' + name + '_bit;\n'
        if correspondence[1]:
            Code += 'new_instr->' + name + '.set_alias(' + correspondence[0] + '[' + str(correspondence[1]) + ' + new_instr->' + name + '_bit]);\n'
        else:
            Code += 'new_instr->' + name + '.set_alias(' + correspondence[0] + '[new_instr->' + name + '_bit]);\n'
    # Fields for the variable parts of the instruction.
    for name, length in self.machineCode.bitFields:
        if name in self.machineCode.bitCorrespondence.keys() + self.bitCorrespondence.keys():
            continue
            # NOTE: This one-liner saved 50+ compilation errors:
            # Fixed bitfields of an instruction would otherwise not be passed.
            # This usually makes sense, except when we want to use a generic
            # operation, where the calling instructions possibly have different
            # fixed-values for a given bitfield, upon which the operation must
            # decide what to do. So we add an additional check where we see
            # whether the field will be used in some operation, in which case
            # we do keep the fixed-value of the bitfield as an instruction
            # member.
        if name in self.machineBits.keys() + self.machineCode.bitValue.keys() and name not in archVars:
            continue
        Code += 'new_instr->' + name + ' = old_instr->' + name + ';\n'
    Code += '}\n}\n'
    Code += 'return new_instr;\n'
    replicateMethod = cxx_writer.Method('replicate', cxx_writer.Code(Code), instructionType.makePointer(), 'public', parameters = [cxx_writer.Parameter('instr', instructionType.makePointer(), initValue = 'NULL')], noException = True, const = True)
    instrMembers.append(replicateMethod)

    ## @} Access and Modification Methods
    #---------------------------------------------------------------------------
    ## @name Behavior Method
    #  @{

    helperOpInline = []
    helperOpVars = []
    for behaviors in self.postbehaviors.values() + self.prebehaviors.values():
        for beh in behaviors:
            if (model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun):
                if beh.inline:
                    helperOpInline.append(beh.name)
                else:
                    instrBases.append(cxx_writer.Type(beh.name + 'Op'))
                    instrCtorInit.append(beh.name + 'Op(' + instrCtorValues + ')')
                for var in beh.instrvars:
                    if not var.name in helperOpVars:
                        instrMembers.append(cxx_writer.Attribute(var.name, var.varType, 'protected',  var.static))
                        helperOpVars.append(var.name)
    if not instrBases:
        instrBases.append(instructionType)

    # Behavior dealing with data hazards.
    hasHazard = False
    decodeStage = None
    regsStage = None
    wbStage = None
    if model.startswith('acc'):
        for pipeStage in processor.pipes:
            if pipeStage.decodeStage:
                decodeStage = processor.pipes.index(pipeStage)
            if pipeStage.regsStage:
                if processor.pipes.index(pipeStage) + 1 < len(processor.pipes):
                    # There exist stages between the beginning and the end of the hazard.
                    if not processor.pipes[processor.pipes.index(pipeStage) + 1].wbStage:
                        hasHazard = True
                regsStage = processor.pipes.index(pipeStage)
            if pipeStage.wbStage:
                wbStage = processor.pipes.index(pipeStage)

    # User-defined instruction behavior.
    behaviorUserCode = ''
    for pipeStage in processor.pipes:
        if self.prebehaviors.has_key(pipeStage.name):
            for beh in self.prebehaviors[pipeStage.name]:
                if not ((model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun)):
                    continue
                if beh.name in helperOpInline:
                    behaviorUserCode += '{\nunsigned num_cycles = 0;\n'
                    for var in beh.localvars:
                        behaviorUserCode += str(var)
                    behaviorUserCode += str(beh.code)
                    behaviorUserCode += 'this->'
                    if model.startswith('acc'): behaviorUserCode += 'num_stage_cycles'
                    else: behaviorUserCode += 'num_instr_cycles'
                    behaviorUserCode += ' += num_cycles;\n}\n'
                else:
                    behaviorUserCode += 'this->'
                    if model.startswith('acc'): behaviorUserCode += 'num_stage_cycles'
                    else: behaviorUserCode += 'num_instr_cycles'
                    behaviorUserCode += ' += ' + beh.name + '('
                    for elem in beh.archElems:
                        behaviorUserCode += 'this->' + elem + ', '
                        behaviorUserCode += 'this->' + elem + '_bit'
                        if beh.archVars or beh.instrvars or elem != beh.archElems[-1]:
                            behaviorUserCode += ', '
                    for elem in beh.archVars:
                        behaviorUserCode += 'this->' + elem
                        if beh.instrvars or elem != beh.archVars[-1]:
                            behaviorUserCode += ', '
                    for var in beh.instrvars:
                        behaviorUserCode += 'this->' + var.name
                        if var != beh.instrvars[-1]:
                            behaviorUserCode += ', '
                    behaviorUserCode += ');\n'
        if self.code.has_key(pipeStage.name):
            behaviorUserCode += str(self.code[pipeStage.name].code)
        if self.postbehaviors.has_key(pipeStage.name):
            for beh in self.postbehaviors[pipeStage.name]:
                if not ((model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun)):
                    continue
                if beh.name in helperOpInline:
                    behaviorUserCode += '{\nunsigned num_cycles = 0;\n'
                    for var in beh.localvars:
                        behaviorUserCode += str(var)
                    behaviorUserCode += str(beh.code)
                    behaviorUserCode += 'this->'
                    if model.startswith('acc'): behaviorUserCode += 'num_stage_cycles'
                    else: behaviorUserCode += 'num_instr_cycles'
                    behaviorUserCode += ' += num_cycles;\n}\n'
                else:
                    behaviorUserCode += 'this->'
                    if model.startswith('acc'): behaviorUserCode += 'num_stage_cycles'
                    else: behaviorUserCode += 'num_instr_cycles'
                    behaviorUserCode += ' += ' + beh.name + '('
                    for elem in beh.archElems:
                        behaviorUserCode += 'this->' + elem + ', '
                        behaviorUserCode += 'this->' + elem + '_bit'
                        if beh.archVars or beh.instrvars or elem != beh.archElems[-1]:
                            behaviorUserCode += ', '
                    for elem in beh.archVars:
                        behaviorUserCode += 'this->' + elem
                        if beh.instrvars or elem != beh.archVars[-1]:
                            behaviorUserCode += ', '
                    for var in beh.instrvars:
                        behaviorUserCode += 'this->' + var.name
                        if var != beh.instrvars[-1]:
                            behaviorUserCode += ', '
                    behaviorUserCode += ');\n'

        if model.startswith('acc'):
            behaviorCode = 'this->num_stage_cycles = 0;\n'
            if behaviorUserCode:
                # Set the pipeline stage so that the usage of the registers is
                # transparent to the user.
                behaviorCode += '\nR.set_stage(' + str(processor.pipes.index(pipeStage)) + ');\n'
                behaviorCode += behaviorUserCode
                behaviorCode += '\nR.unset_stage();\n'
                behaviorCode += 'return this->num_stage_cycles;\n'
                behaviorBody = cxx_writer.Code(behaviorCode)
                behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'public')
                instrMembers.append(behaviorMethod)
                behaviorUserCode = ''
    if not model.startswith('acc'):
        behaviorCode = 'this->num_instr_cycles = 0;\n'
        behaviorCode += behaviorUserCode
        behaviorCode += 'return this->num_instr_cycles;\n'
        behaviorBody = cxx_writer.Code(behaviorCode)
        behaviorMethod = cxx_writer.Method('behavior', behaviorBody, cxx_writer.uintType, 'public')
        instrMembers.append(behaviorMethod)

    ## @} Behavior Method
    #---------------------------------------------------------------------------
    ## @name Hazard Detection Methods
    #  check_regs(), lock_regs(), unlock_regs(), print_busy_regs()
    #  @{

    # These methods deal with hazard detection and prevention. Different types
    # of hazards can occur depend on the combination of register type and
    # pipeline bypasses:
    #
    # Register Types:
    # - Most general-purpose registers are read in the stage specified by
    #   <pipe>.setRegs() and written in the stage specified by <pipe>.setWb().
    # - Special, instruction-specific registers specified by <instr>.
    #   addSpecialRegister() are read and written in the given stages. This is
    #   often the case for status registers that might be read and written in
    #   execute.
    #
    # Pipeline Feedback:
    # - Default behavior: The default behavior of the pipeline is to propagate
    #   each register from regsStage to wbStage one stage forward every cycle.
    #   In addition, wbStage is considered the stable value and bypassed to all
    #   stages from [0..regsStage] as well as all stages from [wbStage+1..
    #   lastStage].
    # - Common feedback loops via <processor>Arch.py:<pipe>.setWbStageOrder():
    #   A backward path is created for each stage in the list back to <pipe>
    #   for all registers.
    # - Register-specific feedback loops via <reg>.setWbStageOrder():
    #   This behaves as in common feedback loops, but only for the given
    #   registers.
    # - Global registers are assumed to be immediately visible to all stages at
    #   the end of cycle where the were written.
    #
    # Hazard Functions:
    # To prevent data hazards, each instruction needs to perform the following
    # three actions:
    #
    # - check_regs():
    #   Calling sequence:
    #      Pipeline[decode]::behavior()
    #   -> Instruction::check_regs()
    #   -> Register::is_locked()
    #   Before reading a register, an instruction needs to make sure no other
    #   instruction currently in the pipeline intends to write or has already
    #   written the register locally without write-back. We test the lock as
    #   early as possible, i.e. in the decode stage, even if the read is yet
    #   to come. Example: If an instruction i wants to read a register in the
    #   regs stage, pipe[decode].behavior() calls i.check_regs(), which calls
    #   reg.is_locked(stage=regs). Since the register itself has no knowledge
    #   of bypass loops, this returns an upper limit on the required stall
    #   cycles, if any.
    #
    # - lock_regs():
    #   Calling sequence:
    #      Pipeline[decode]::behavior()
    #   -> Instruction::lock_regs()
    #   -> Register::lock()
    #   If the instruction intends to write a general or instruction-specific
    #   register, this function calls the lock() method of the register with
    #   the correct latency and stage. As in check_regs(), it is best to signal
    #   the intention to write asap. Since we use register renaming, there are
    #   no WAW hazards. This means reg.lock() will return true as long as the
    #   write-order is preserved, i.e. the program is well-formed. If i0 calls
    #   lock(stage0, latency0), an instruction i1 coming one cc later will
    #   always either 1) request to write a stage1 < stage0, 2) request to write
    #   stage0 with latency1 = latency0+1 or 3) request to write stage1 > stage0
    #   with latency1 > latency0 where stage1-stage0 = latency1-latency0. This
    #   can only be guaranteed so long as 1) the instructions are issued
    #   in-order and 2) all instructions signal the write in the same stage.
    #
    # - unlock_regs():
    #   Calling sequence:
    #      Pipeline[regs..wb]::behavior():catch(annul_exception)
    #   -> Instruction::unlock_regs()
    #   -> Register::unlock()
    #   This method unlocks all registers previously unlocked. Called only when
    #   an instruction is annulled. The regular write-back case does not require
    #   an explicit unlock_regs(), since the registers themselves undertake
    #   decrementing the latencies and advancing the locked stages.
    #
    # @see register_abstraction.hpp:is_locked(), lock() and unlock().
    if model.startswith('acc'):
        if hasHazard:
            # Regular input registers: Aliases specified by instruction fields.
            # Unfortunately, we cannot consider special bypasses since we do not
            # what the alias is pointing to at the time of code generation. This
            # is erring on the safe side, and should have little effect in
            # practice.
            # TODO: I wonder about the PC: Will it happen that we assume
            # there exists a common bypass, which is nullified by PC-
            # specific bypasses, and end up not stalling long enough?
            # Tricky...The only error-proof way is storing bypass
            # information in the register itself after all. This is
            # precisely what I tried to avoid by hard-coding
            # clock_cycle. Maybe I could keep clock_cycle as it is, and
            # just use the stored info for calculating locks only. Then
            # I could move the following calculation to reg.is_locked()
            # and have it return the required stall that considers all
            # stages and bypasses.
            instrInRegs = []
            for reg, correspondence in self.machineCode.bitCorrespondence.items():
                if 'in' in self.machineCode.bitDirection[reg]:
                    if reg not in self.notLockRegs:
                        instrInRegs.append(reg)
            for reg, correspondence in self.bitCorrespondence.items():
                if 'in' in self.bitDirection[reg]:
                    if reg not in self.notLockRegs:
                        instrInRegs.append(reg)
            # Special input registers.
            instrSpecialInRegs = {}
            for pipeName, regList in self.specialInRegs.items():
                for reg in regList:
                    stageIndex = [pipeStageInner.name for pipeStageInner in processor.pipes].index(pipeName)
                    # Only if the register will be read in an earlier stage than
                    # we already calculated.
                    if reg not in instrInRegs:
                        if reg not in instrSpecialInRegs.keys():
                            instrSpecialInRegs[reg] = stageIndex
                        elif stageIndex < instrSpecialInRegs[reg]:
                            instrSpecialInRegs[reg] = stageIndex
                    elif stageIndex < regsStage:
                        instrSpecialInRegs[reg] = stageIndex
                        instrInRegs.remove(reg)
            # Special input registers from HelperOperations.
            for pipeName, behaviors in self.prebehaviors.items() + self.postbehaviors.items():
                for beh in behaviors:
                    for reg in beh.specialInRegs:
                        stageIndex = [pipeStageInner.name for pipeStageInner in processor.pipes].index(pipeName)
                        # Only if the register will be read in an earlier stage
                        # than we already calculated.
                        if reg not in instrInRegs:
                            if reg not in instrSpecialInRegs.keys():
                                instrSpecialInRegs[reg] = stageIndex
                            elif stageIndex < instrSpecialInRegs[reg]:
                                instrSpecialInRegs[reg] = stageIndex
                        elif stageIndex < regsStage:
                            instrSpecialInRegs[reg] = stageIndex
                            instrInRegs.remove(reg)
            # Regular output registers: Aliases specified by instruction fields.
            instrOutRegs = []
            for reg, correspondence in self.machineCode.bitCorrespondence.items():
                if 'out' in self.machineCode.bitDirection[reg]:
                    if not reg in self.notLockRegs:
                        instrOutRegs.append(reg)
            for reg, correspondence in self.bitCorrespondence.items():
                if 'out' in self.bitDirection[reg] and not reg in instrOutRegs:
                    if not reg in self.notLockRegs:
                        instrOutRegs.append(reg)
            # Special output registers.
            instrSpecialOutRegs = {}
            for pipeName, regList in self.specialOutRegs.items():
                for reg in regList:
                    stageIndex = [pipeStageInner.name for pipeStageInner in processor.pipes].index(pipeName)
                    # Only if the register will be written in an earlier stage
                    # than we already calculated.
                    if reg not in instrOutRegs:
                        if reg not in instrSpecialOutRegs.keys():
                            instrSpecialOutRegs[reg] = stageIndex
                        elif stageIndex < instrSpecialOutRegs[reg]:
                            instrSpecialOutRegs[reg] = stageIndex
                    elif stageIndex < regsStage:
                        instrSpecialOutRegs[reg] = stageIndex
                        instrOutRegs.remove(reg)
            # Special output registers from HelperOperations.
            for pipeName, behaviors in self.prebehaviors.items() + self.postbehaviors.items():
                for beh in behaviors:
                    for reg in beh.specialOutRegs:
                        stageIndex = [pipeStageInner.name for pipeStageInner in processor.pipes].index(pipeName)
                        if reg not in instrOutRegs:
                            if reg not in instrSpecialOutRegs.keys():
                                instrSpecialOutRegs[reg] = stageIndex
                            elif stageIndex < instrSpecialOutRegs[reg]:
                                instrSpecialOutRegs[reg] = stageIndex
                        elif stageIndex < regsStage:
                            instrSpecialOutRegs[reg] = stageIndex
                            instrOutRegs.remove(reg)

            # check_regs()
            Code = ''
            # Regular input registers: Aliases specified by instruction fields.
            for reg in instrInRegs:
                Code += 'reg_stall = this->' + reg + '.is_locked(' + str(regsStage) + ', ' + str(regsStage-decodeStage) + ');\n'
                Code += 'if (reg_stall > max_stall) max_stall = reg_stall;\n'
            # Special input registers.
            for reg, stageIndex in instrSpecialInRegs.items():
                Code += 'reg_stall = this->' + reg + '.is_locked(' + str(stageIndex) + ', ' + str(stageIndex-decodeStage) + ');\n'
                Code += 'if (reg_stall > max_stall) max_stall = reg_stall;\n'
            # Custom hazard checking operations.
            if self.customCheckHazardOp.has_key(pipeStage.name):
                for customRegToCheck in self.customCheckHazardOp[pipeStage.name]:
                    Code += 'reg_stall = ' + customRegToCheck + '.is_locked(' + str(processor.pipes.index(pipeStage)) + ', ' + str(processor.pipes.index(pipeStage)-decodeStage) + ');\n'
                    Code += 'if (reg_stall > max_stall) max_stall = reg_stall;\n'
            if Code:
                checkRegsCode = 'unsigned reg_stall, max_stall = 0;\n'
                checkRegsCode += Code
                checkRegsCode += 'return max_stall;\n'
                checkRegsMethod = cxx_writer.Method('check_regs', cxx_writer.Code(checkRegsCode), cxx_writer.uintType, 'public')
                instrMembers.append(checkRegsMethod)

            # lock_regs()
            Code = ''
            # Regular output registers: Aliases specified by instruction fields.
            for reg in instrOutRegs:
                Code += 'can_lock = can_lock && this->' + reg + '.lock(this, ' + str(wbStage) + ', ' + str(wbStage-decodeStage) + ');\n'
            # Special output registers.
            for reg, stageIndex in instrSpecialOutRegs.items():
                Code += 'can_lock = can_lock && this->' + reg + '.lock(this, ' + str(stageIndex) + ', ' + str(stageIndex-decodeStage) + ');\n'
            if Code:
                lockRegsCode = 'bool can_lock = true;\n'
                lockRegsCode += Code
                lockRegsCode += 'return can_lock;\n'
                lockRegsMethod = cxx_writer.Method('lock_regs', cxx_writer.Code(lockRegsCode), cxx_writer.boolType, 'public')
                instrMembers.append(lockRegsMethod)

            # unlock_regs()
            Code = ''
            # Regular and special output registers.
            for reg in instrOutRegs + instrSpecialOutRegs.keys():
                Code += 'can_unlock = can_unlock && this->' + reg + '.unlock(this);\n'
            if Code:
                unlockRegsCode = 'bool can_unlock = true;\n'
                unlockRegsCode += Code
                unlockRegsCode += 'return can_unlock;\n'
                unlockRegsMethod = cxx_writer.Method('unlock_regs', cxx_writer.Code(unlockRegsCode), cxx_writer.boolType, 'public')
                instrMembers.append(unlockRegsMethod)

        # Methods: print_busy_regs()
        if trace and hasHazard:
            Code = 'std::string ret_val = "";\n'
            # Regular input registers: Aliases specified by instruction fields.
            for reg in instrInRegs:
                Code += 'if (this->' + reg + '.is_locked(' + str(regsStage) + ', ' + str(regsStage-decodeStage) + ') > 0) {\n'
                Code += 'ret_val += "' + reg + ' - ";\n'
                Code += '}\n'
            # Special input registers.
            for reg, stageIndex in instrSpecialInRegs.items():
                Code += 'if (this->' + reg + '.is_locked(' + str(stageIndex) + ', ' + str(stageIndex-decodeStage) + ') > 0) {\n'
                Code += 'ret_val += "' + reg + ' - ";\n'
                Code += '}\n'
            Code += 'return ret_val;\n'
            printBusyRegsBody = cxx_writer.Code(Code)
            printBusyRegsMethod = cxx_writer.Method('print_busy_regs', printBusyRegsBody, cxx_writer.stringType, 'public')
            instrMembers.append(printBusyRegsMethod)

    ## @} Hazard Detection Methods
    #---------------------------------------------------------------------------
    ## @name Information and Helper Methods
    #  @{

    # Finally now I have to override the basic new operator in
    # order to speed up memory allocation (***** Commented since it does not give any speedup ******)
    #num_allocated = processor.alloc_buffer_size*self.frequency
    #poolVar = cxx_writer.Variable(self.name + '_pool[' + str(num_allocated) + '*sizeof(' + self.name + ')]', cxx_writer.ucharType, namespaces = [namespace])
    #operatorNewCode = """
    #if (""" + self.name + """::allocated < """ + str(num_allocated) + """) {
        #""" + self.name + """::allocated++;
        #return """ + self.name + """_pool + (""" + self.name + """::allocated - 1)*sizeof(""" + self.name + """);
    #}
    #else {
        #void* newMem = ::malloc(bytes_to_alloc);
        #if (newMem == NULL)
            #throw std::bad_alloc();
        #return newMem;
    #}
    #"""
    #operatorNewBody =  cxx_writer.Code(operatorNewCode)
    #operatorNewBody.addInclude('cstddef')
    #operatorNewBody.addInclude('cstdlib')
    #operatorNewBody.addInclude('new')
    #operatorNewParams = [cxx_writer.Parameter('bytes_to_alloc', cxx_writer.Type('std::size_t'))]
    #operatorNewMethod = cxx_writer.MemberOperator('new', operatorNewBody, cxx_writer.voidPtrType, 'public', operatorNewParams)
    #instrMembers.append(operatorNewMethod)
    #operatorDelCode = """
        #if (m != NULL && (m < """ + self.name + """_pool || m > (""" + self.name + """_pool + """ + str(num_allocated - 1) + """*sizeof(""" + self.name + """)))) {
            #::free(m);
        #}
    #"""
    #operatorDelBody =  cxx_writer.Code(operatorDelCode)
    #operatorDelParams = [cxx_writer.Parameter('m', cxx_writer.voidPtrType)]
    #operatorDelMethod = cxx_writer.MemberOperator('delete', operatorDelBody, cxx_writer.voidType, 'public', operatorDelParams)
    #instrMembers.append(operatorDelMethod)
    #allocatedAttr = cxx_writer.Attribute('allocated', cxx_writer.uintType, 'private', initValue = '0', static = True)
    #instrMembers.append(allocatedAttr)

    ########################## TODO: to eliminate, only for statistics ####################
    #allocatedOutAttr = cxx_writer.Attribute('allocated_out', cxx_writer.uintType, 'private', static = True)
    #instrMembers.append(allocatedOutAttr)
    #returnStatsMethod = cxx_writer.Method('get_count_my_alloc', cxx_writer.Code('return ' + self.name + '::allocated;'), cxx_writer.uintType, 'public')
    #instrMembers.append(returnStatsMethod)
    #returnStatsMethod = cxx_writer.Method('get_count_std_alloc', cxx_writer.Code('return ' + self.name + '::allocated_out;'), cxx_writer.uintType, 'public')
    #instrMembers.append(returnStatsMethod)
    ########################################################################################

    ## @} Information and Helper Methods
    #---------------------------------------------------------------------------
    ## @name Attributes and Initialization
    #  @{

    for var in self.variables:
        if not var.name in helperOpVars:
            instrMembers.append(cxx_writer.Attribute(var.name, var.varType, 'protected',  var.static))

    ## @} Attributes and Initialization
    #---------------------------------------------------------------------------
    ## @name Constructors and Destructors
    #  @{

    instrCtor = cxx_writer.Constructor(emptyBody, 'public', parameters = instrCtorParams, initList = instrCtorInit)
    instrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

    ## @} Constructors and Destructors
    #---------------------------------------------------------------------------

    instrClass = cxx_writer.ClassDeclaration(self.name, instrMembers, superclasses = instrBases, namespaces = [namespace])
    instrClass.addDocString(brief = self.docbrief, detail = self.docdetail)
    instrClass.addConstructor(instrCtor)
    instrClass.addDestructor(instrDtor)

    #return [poolVar, instrClass] *** Again removed, related to the instruction pre-allocation
    return [instrClass]

################################################################################
# Instruction Test Functions
################################################################################
def getCPPInstrTest(self, processor, model, trace, combinedTrace, namespace = ''):
    """Returns the code for testing the current instruction. A test consists of
    setting the instruction variables, performing the instruction behavior and
    then comparing the registers with the expected value. Some custom code is
    necessary at the beginning to be able to access the private members of the
    instruction."""

    # Code common to all tests of a single instruction: Variables are declared
    # and the instruction instantiated with stub parameters.
    declCode = ''
    initInstrCode = '('

    # Registers, Aliases and Register Banks
    if processor.regs or processor.regBanks:
        from registerWriter import registerContainerType
        declCode += registerContainerType.name + ' R('
        # Register const or reset values could be processor variables.
        # Since we do not have the values for those (probably program-dependent),
        # we pass on zeros to the Registers ctor.
        Code = ''
        for reg in processor.regs:
            if isinstance(reg.constValue, str):
                Code += '0, '
            if isinstance(reg.defValue, str):
                Code += '0, '

        for regBank in processor.regBanks:
            for regConstValue in regBank.constValue.values():
                if isinstance(regConstValue, str):
                    Code += '0, '
            for regDefaultValue in regBank.defValues:
                if isinstance(regDefaultValue, str):
                    Code += '0, '
        if Code:
            declCode += Code[:-2] + ');\n'
        else:
            declCode += ');\n'
        # We also explicitly reset all regs to zero, instead of the reset value.
        # Test writers tend to mask status registers apart from the bits they
        # are interested in, which is perhaps not quite correct but intuitive.
        declCode += 'R.write_force(0);\n'
        initInstrCode += 'R, '

    #memAliasInit = ''
    #for alias in processor.memAlias:
        #memAliasInit += ', ' + alias.alias

    # Memory
    if (trace or (any(memAttr[1] == True for memAttr in processor.memories.values()))) and not processor.systemc:
        declCode += 'unsigned total_cycles;\n'
    for memName, memAttr in processor.memories.items():
        Code = ''
        if memAttr[1]:
            Code += ', total_cycles'
        if memAttr[2]:
            Code += ', ' + memAttr[2]
        declCode += namespace + '::LocalMemory ' + memName + '(' + str(memAttr[0]) + Code + ');\n'
        initInstrCode += memName + ', '

    # Ports
    # Local memories are declared even for TLM ports. The default memory size is
    # 1MB.
    for portName in processor.tlmPorts:
        declCode += namespace + '::LocalMemory ' + portName + '(' + str(1024*1024) + ');\n'
        initInstrCode += portName + ', '

    # Pins
    outPinPorts = []
    for pinPort in processor.pins:
        if not pinPort.inbound:
            outPinPorts.append(pinPort.name)
            if pinPort.systemc:
                pinPortTypeName = 'SC'
            else:
                pinPortTypeName = 'TLM'
            if pinPort.inbound:
                pinPortTypeName += 'InPin_'
            else:
                pinPortTypeName += 'OutPin_'
            pinPortTypeName += str(pinPort.portWidth)
            declCode += namespace + '::' + pinPortTypeName + ' ' + pinPort.name + '_pin(\"' + pinPort.name + '_pin\");\n'
            declCode += 'PINTarget<' + str(pinPort.portWidth) + '> ' + pinPort.name + '_target_pin(\"' + pinPort.name + '_target_pin\");\n'
            declCode += pinPort.name + '_pin.init_socket.bind(' + pinPort.name + '_target_pin.target_socket);\n'
            initInstrCode += pinPort.name + '_pin, '

    if trace and not processor.systemc:
        initInstrCode += 'total_cycles, '
    initInstrCode = initInstrCode[:-2] + ')'

    # Individual tests of a single instruction.
    from procWriter import testNames
    instrTestFunctions = []
    for test in self.tests:
        # Instantiate architectural elements.
        instrTestCode = declCode + '\n'

        # Instantiate instruction under test.
        instrTestCode += self.name + ' test_instruction' + initInstrCode + ';\n'

        # Initialize global resources.
        for resource, value in test[1].items():
            bracket = resource.find('[')
            resources = processor.memories.keys() + processor.memoryifs + processor.tlmPorts
            if bracket > 0 and resource[:bracket] in resources:
                try:
                    instrTestCode += resource[:bracket] + '.write_word_dbg(' + hex(int(resource[bracket + 1:-1])) + ', ' + hex(value) + ');\n'
                except ValueError:
                    instrTestCode += resource[:bracket] + '.write_word_dbg(' + hex(int(resource[bracket + 1:-1], 16)) + ', ' + hex(value) + ');\n'
            else:
                instrTestCode += resource + '.write_force(' + hex(value) + ');\n'

        # Set instruction fields.
        Code = ['0' for i in range(0, self.machineCode.instrLen)]
        for name, elemValue in test[0].items():
            if self.machineCode.bitLen.has_key(name):
                curBitCode = toBinStr(elemValue, self.machineCode.bitLen[name])
                curBitCode.reverse()
                if len(curBitCode) > self.machineCode.bitLen[name]:
                    raise Exception('Cannot represent value ' + hex(elemValue) + ' of field ' + name + ' in test of instruction ' + self.name + ' in ' + str(self.machineCode.bitLen[name]) + ' bits.')
                for i in range(0, len(curBitCode)):
                    Code[self.machineCode.bitLen[name] + self.machineCode.bitPos[name] - i -1] = curBitCode[i]
            else:
                raise Exception('Field ' + name + ' in test of instruction ' + self.name + ' does not exist in the machine code.')
        instrTestCode += 'test_instruction.set_params(' + hex(int(''.join(Code), 2)) + ');\n'

        # Run instruction behavior.
        instrTestCode += 'try {\n'
        instrTestCode += 'test_instruction.behavior();'
        instrTestCode += '\n}\ncatch(annul_exception& etc) {\n}\n\n'

        # Test the output values.
        for resource, value in test[2].items():
            instrTestCode += 'BOOST_CHECK_EQUAL('
            bracket = resource.find('[')
            resources = processor.memories.keys() + processor.memoryifs + processor.tlmPorts
            if bracket > 0 and resource[:bracket] in resources:
                try:
                    instrTestCode += resource[:bracket] + '.read_word_dbg(' + hex(int(resource[bracket + 1:-1])) + ')'
                except ValueError:
                    instrTestCode += resource[:bracket] + '.read_word_dbg(' + hex(int(resource[bracket + 1:-1], 16)) + ')'
            elif bracket > 0 and resource[:bracket] in outPinPorts:
                try:
                    instrTestCode += resource[:bracket] + '_target_pin.read_pin(' + hex(int(resource[bracket + 1:-1])) + ')'
                except ValueError:
                    instrTestCode += resource[:bracket] + '_target_pin.read_pin(' + hex(int(resource[bracket + 1:-1], 16)) + ')'
            else:
                instrTestCode += resource + '.read_force()'
            instrTestCode += ', (' + str(processor.bitSizes[1]) + ')' + hex(value) + ');\n\n'

        instrTestBody = cxx_writer.Code(instrTestCode)
        disableWarningsCode = '#ifdef _WIN32\n#pragma warning(disable : 4101\n#endif\n'
        includeUnprotectedCode = '#define private public\n#define protected public\n#include \"instructions.hpp\"\n#include \"registers.hpp\"\n#include \"memory.hpp\"\n#undef private\n#undef protected\n'
        instrTestBody.addInclude(['boost/test/test_tools.hpp', 'common/report.hpp', disableWarningsCode, includeUnprotectedCode])

        testName = self.name + '_' + str(len(instrTestFunctions))
        instrTestFunction = cxx_writer.Function(testName, instrTestBody, cxx_writer.voidType)
        instrTestFunctions.append(instrTestFunction)
        testNames.append(testName)

    return instrTestFunctions

def getCPPInstrTests(self, processor, modelType, trace, combinedTrace, namespace):
    testFunctions = []
    for instr in self.instructions.values():
        testFunctions += instr.getCPPTest(processor, modelType, trace, combinedTrace, namespace)
    return testFunctions

################################################################################
