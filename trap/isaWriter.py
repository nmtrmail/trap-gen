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

def getToUnlockRegs(self, processor, pipeStage, getAll, delayedUnlock):
    """Returns the list of registers which have to be unlocked in the current
    pipeline stage; in case getAll is true, I have to unlock all of the
    locked registers, and I return the pipeline register (ending with _pipe).
    Else, I have to return only the registers for this particular stage,
    so that only the stage register is unlocked."""

    code = ''
    regsToUnlock = []

    if getAll or pipeStage.wb:
        # Calculate preceding and remaining stages.
        precedingStages = []
        remainingStages = []
        foundCur = False
        for curPipe in processor.pipes:
            if curPipe.name == pipeStage.name:
                foundCur = True
            if foundCur:
                remainingStages.append(curPipe.name)
            else:
                precedingStages.append(curPipe.name)

        # Special registers specify the stages where they should remain locked.
        # We therefore unlock all other stages except for the ones in this list.
        specialRegStages = {}
        for toUnlockStage, regToUnlockList in self.specialOutRegs.items():
            for regToUnlock in regToUnlockList:
                # The fact that we want to unlock them.
                regsToUnlock.append(regToUnlock)
                # The stages that should NOT be unlocked.
                if specialRegStages.has_key(regToUnlock):
                    specialRegStages[regToUnlock].append(toUnlockStage)
                else:
                    specialRegStages[regToUnlock] = [toUnlockStage]

        # Non-special registers can be unlocked for all stages.
        for regToUnlock in self.machineCode.bitCorrespondence.keys():
            if 'out' in self.machineCode.bitDirection[regToUnlock] and not regToUnlock in regsToUnlock:
                regsToUnlock.append(regToUnlock)
        for regToUnlock in self.bitCorrespondence.keys():
            if 'out' in self.bitDirection[regToUnlock] and not regToUnlock in regsToUnlock:
                regsToUnlock.append(regToUnlock)
    else:
        # Unlock only this stage.
        if self.specialOutRegs.has_key(pipeStage.name):
            for regToUnlock in self.specialOutRegs[pipeStage.name]:
                regsToUnlock.append(regToUnlock)

    # Only registers can be unlocked, not aliases.
    regNames = [i.name for i in processor.regBanks + processor.regs]
    # TODO: Think of how to store the stage index in the unlock_queue. Map?
    for regToUnlock in regsToUnlock:
        if not regToUnlock in self.notLockRegs:
            regName = regToUnlock
            bracket = regName.find('[')
            if bracket > 0:
                regName = regName[:bracket]

            delay = '0'
            if delayedUnlock and self.delayedWb.has_key(regToUnlock):
                delay = str(self.delayedWb[regToUnlock])

            # Determine the stages which have to be unlocked:
            # - If neither getAll nor pipeStage.wb is true, unlock only the
            #   current stage of both regular and special registers.
            # - Else unlock all stages of regular registers and, for special
            #   registers, unlock all upcoming stages as well as all preceding
            #   stages not explicity specified.
            if getAll or pipeStage.wb:
                # Regular Register/Alias
                if not specialRegStages.has_key(regToUnlock):
                    if regName in regNames:
                        code += 'unlock_queue[' + delay + '].push_back(' + regToUnlock + '/* all */);\n'
                    else:
                        code += 'unlock_queue[' + delay + '].push_back(' + regToUnlock + '.get_reg(/* all */));\n'
                # Special Register/Alias
                # Unlock all upcoming stages as well as all preceding stages
                # not explicity specified.
                else:
                    if regName in regNames:
                        code += 'unlock_queue[' + delay + '].push_back(' + regToUnlock + '/* all */);\n'
                    else:
                        code += 'unlock_queue[' + delay + '].push_back(' + regToUnlock + '.get_reg(/* all */));\n'
                    toUnlockStages = []
                    for precedingStage in precedingStages:
                        if not precedingStage in specialRegStages[regToUnlock]:
                            toUnlockStages.append(precedingStage)
                    toUnlockStages += remainingStages
                    for toUnlockStage in toUnlockStages:
                        if regName in regNames:
                            code += 'unlock_queue[' + delay + '].push_back(' + regToUnlock + '/* ' + toUnlockStage + ' */);\n'
                        else:
                            code += 'unlock_queue[' + delay + '].push_back(' + regToUnlock + '.get_reg(/* ' + toUnlockStage + ' */));\n'

            # Unlock the current stage of both regular and special registers.
            else:
                if regName in regNames:
                    code += 'unlock_queue[' + delay + '].push_back(' + regToUnlock + '/* ' + pipeStage.name + ' */);\n'
                else:
                    code += 'unlock_queue[' + delay + '].push_back(' + regToUnlock + '.get_reg(/* ' + pipeStage.name + ' */);\n'

    return code

def toBinStr(intNum, maxLen = -1):
    """Given an integer number it converts it to a bitstring; maxLen is used only
    in case a negative number have to be converted"""
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
    unlockQueueType = cxx_writer.TemplateType('std::map', ['unsigned', cxx_writer.TemplateType('std::vector', [registerType.makePointer()], 'vector')], 'map')
    instructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')

    emptyBody = cxx_writer.Code('')
    instrClasses = []

    # Any special defines
    for i in self.defines:
        instrClasses.append(cxx_writer.Define(i + '\n'))

    #---------------------------------------------------------------------------
    ## @name Instruction Class
    #  @{

    from procWriter import instrAttrs, instrCtorParams
    instructionElements = []

    # Methods: HelperMethods
    for helperMethod in self.methods:
        if helperMethod:
            instructionElements.append(helperMethod.getCPPInstrMethod(model, namespace))

    # Methods: behavior()
    if not model.startswith('acc'):
        behaviorMethod = cxx_writer.Method('behavior', cxx_writer.Code('return 0;\n'), cxx_writer.uintType, 'public', virtual = True)
        instructionElements.append(behaviorMethod)
    else:
        unlockQueueParam = cxx_writer.Parameter('unlock_queue', unlockQueueType.makeRef())
        for pipeStage in processor.pipes:
            behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, cxx_writer.Code('return 0;\n'), cxx_writer.uintType, 'public', [unlockQueueParam], virtual = True)
            instructionElements.append(behaviorMethod)

    # Methods: replicate()
    replicateMethod = cxx_writer.Method('replicate', emptyBody, instructionType.makePointer(), 'public', parameters = [cxx_writer.Parameter('instr', instructionType.makePointer(), initValue = 'NULL')], noException = True, const = True, pure = True)
    instructionElements.append(replicateMethod)

    # Methods: set_params()
    setParamsParam = cxx_writer.Parameter('bitstring', processor.bitSizes[1].makeRef().makeConst())
    setParamsMethod = cxx_writer.Method('set_params', emptyBody, cxx_writer.voidType, 'public', [setParamsParam], noException = True, virtual = True)
    instructionElements.append(setParamsMethod)

    # Methods: check_hazard(), lock_regs(), get_unlock()
    if model.startswith('acc'):
        # Now I have to add the code for checking data hazards
        hasCheckHazard = False
        for pipeStage in processor.pipes:
            if pipeStage.checkHazard:
                if processor.pipes.index(pipeStage) + 1 < len(processor.pipes):
                    # There exist stages between the beginning and the end of the hazard.
                    if not processor.pipes[processor.pipes.index(pipeStage) + 1].wb:
                        hasCheckHazard = True
        if hasCheckHazard:
            for pipeStage in processor.pipes:
                checkHazardMethod = cxx_writer.Method('check_hazard_' + pipeStage.name, cxx_writer.Code('return true;\n'), cxx_writer.boolType, 'public', virtual = True)
                instructionElements.append(checkHazardMethod)
                lockMethod = cxx_writer.Method('lock_regs_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'public', virtual = True)
                instructionElements.append(lockMethod)
                getUnlockMethod = cxx_writer.Method('get_unlock_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'public', [unlockQueueParam], virtual = True)
                instructionElements.append(getUnlockMethod)

        # Attributes
        # I also have to add the program counter attribute
        fetchPCAttr = cxx_writer.Attribute('fetch_PC', processor.bitSizes[1], 'public')
        instructionElements.append(fetchPCAttr)
        # and the inInPipeline attribute, specifying if the instruction is currently already
        # in the pipeline or not
        inPipelineAttr = cxx_writer.Attribute('in_pipeline', cxx_writer.boolType, 'public')
        instructionElements.append(inPipelineAttr)
        toDestroyAttr = cxx_writer.Attribute('to_destroy', cxx_writer.boolType, 'public')
        instructionElements.append(toDestroyAttr)

    # Methods: print_trace(), print_busy_regs()
    if trace:
        # I have to print the value of all the registers in the processor
        traceStage = processor.pipes[-1]
        Code = ''

        if model.startswith('acc'):
            # Renames all resources so that their usage can be transparent
            # to the developer.
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
        instructionElements.append(printTraceMethod)

        # Now we have to print the method for creating the data hazards
        if model.startswith('acc'):
            printBusyRegsMethod = cxx_writer.Method('print_busy_regs', cxx_writer.Code('return "";'), cxx_writer.stringType, 'public', virtual = True)
            instructionElements.append(printBusyRegsMethod)

    # Methods: annul()
    # Stops the execution of the current operation.
    annulBody = cxx_writer.Code('throw annul_exception();')
    annulBody.addInclude('common/report.hpp')
    annulMethod = cxx_writer.Method('annul', annulBody, cxx_writer.voidType, 'public', inline = True)
    instructionElements.append(annulMethod)

    # Methods: flush()
    if not model.startswith('acc'): flushBody = emptyBody
    else: flushBody = cxx_writer.Code('this->flush_pipeline = true;')
    flushMethod = cxx_writer.Method('flush', flushBody, cxx_writer.voidType, 'public', inline = True)
    instructionElements.append(flushMethod)

    # Methods: stall()
    stallParam = cxx_writer.Parameter('num_cycles', processor.bitSizes[1].makeRef().makeConst())
    Code = 'this->'
    if model.startswith('acc'): Code += 'num_stage_cycles'
    else: Code += 'num_instr_cycles'
    Code += ' += num_cycles;\n'
    stallBody = cxx_writer.Code(Code)
    stallMethod = cxx_writer.Method('stall', stallBody, cxx_writer.voidType, 'public', [stallParam], inline = True)
    instructionElements.append(stallMethod)

    # Methods: get_count_std_alloc()
    # TODO: To eliminate, only for statistics
    #returnStatsMethod = cxx_writer.Method('get_count_my_alloc', emptyBody, cxx_writer.uintType, 'public', virtual = True)
    #instructionElements.append(returnStatsMethod )
    #returnStatsMethod = cxx_writer.Method('get_count_std_alloc', emptyBody, cxx_writer.uintType, 'public', virtual = True)
    #instructionElements.append(returnStatsMethod )

    # Attributes
    # Now create references to the architectural elements contained in the processor and
    # initialize them through the constructor
    if not model.startswith('acc'):
        instructionElements.append(cxx_writer.Attribute('num_instr_cycles', cxx_writer.uintType, 'public'))
        instrCtorCode = 'this->num_instr_cycles = 0;'
    if model.startswith('acc'):
        instructionElements.append(cxx_writer.Attribute('num_stage_cycles', cxx_writer.uintType, 'protected'))
        instructionElements.append(cxx_writer.Attribute('flush_pipeline', cxx_writer.boolType, 'public'))
        instrCtorCode = 'this->num_stage_cycles = 0;\nthis->flush_pipeline = false;\nthis->fetch_PC = 0;\nthis->to_destroy = false;\nthis->in_pipeline = false;\n'

    # Constructors and Destructors
    instrCtorInit = []
    for attr in instrAttrs:
        instrCtorInit.append(attr.name + '(' + attr.name + ')')
    for constant in self.constants:
        instructionElements.append(cxx_writer.Attribute(constant[1], constant[0].makeConst(), 'protected'))
        instrCtorInit.append(constant[1] + '(' + str(constant[2]) + ')')
    instrCtor = cxx_writer.Constructor(cxx_writer.Code(instrCtorCode), 'public', parameters = instrCtorParams, initList = instrCtorInit)
    instrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

    # Class
    instructionBaseType = cxx_writer.Type('InstructionBase', 'modules/instruction.hpp')
    instructionClass = cxx_writer.ClassDeclaration('Instruction', instrAttrs + instructionElements, [instructionBaseType], namespaces = [namespace])
    instructionClass.addDocString(brief = 'Instruction Class', detail = 'All individual instructions derive from this class.')
    instructionClass.addConstructor(instrCtor)
    instructionClass.addDestructor(instrDtor)
    instrClasses.append(instructionClass)

    ## @} Instruction Class
    #---------------------------------------------------------------------------
    ## @name InvalidInstruction Class

    invalidInstrElements = []

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
                behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'public', [unlockQueueParam])
                invalidInstrElements.append(behaviorMethod)
    else:
        behaviorMethod = cxx_writer.Method('behavior', behaviorBody, cxx_writer.uintType, 'public')
        invalidInstrElements.append(behaviorMethod)

    # Methods: replicate()
    replicateBody = cxx_writer.Code('return new InvalidInstruction(' + instrCtorValues + ');')
    replicateMethod = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'public', parameters = [cxx_writer.Parameter('instr', instructionType.makePointer(), initValue = 'NULL')], noException = True, const = True)
    invalidInstrElements.append(replicateMethod)

    # Methods: get_name()
    getNameBody = cxx_writer.Code('return \"InvalidInstruction\";')
    getNameMethod = cxx_writer.Method('get_name', getNameBody, cxx_writer.stringType, 'public', noException = True, const = True)
    invalidInstrElements.append(getNameMethod)

    # Methods: get_mnemonic()
    getMnemonicBody = cxx_writer.Code('return \"invalid\";')
    getMnemonicMethod = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'public', noException = True, const = True)
    invalidInstrElements.append(getMnemonicMethod)

    # Methods: get_id()
    getIdBody = cxx_writer.Code('return ' + str(len(self.instructions)) + ';')
    getIdMethod = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'public', noException = True, const = True)
    invalidInstrElements.append(getIdMethod)

    # Constructors and Destructors
    invalidInstrCtor = cxx_writer.Constructor(emptyBody, 'public', parameters = instrCtorParams, initList = ['Instruction(' + instrCtorValues + ')'])
    invalidInstrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

    # Class
    invalidInstrClass = cxx_writer.ClassDeclaration('InvalidInstruction', invalidInstrElements, [instructionClass.getType()], namespaces = [namespace])
    invalidInstrClass.addConstructor(invalidInstrCtor)
    invalidInstrClass.addDestructor(invalidInstrDtor)
    instrClasses.append(invalidInstrClass)

    ## @} InvalidInstruction Class
    #---------------------------------------------------------------------------
    ## @name NOPInstruction Class

    if model.startswith('acc'):
        # finally I print the NOP instruction, which I put in the pipeline when flushes occur.
        NOPInstrElements = []

        # Methods: behavior()
        for pipeStage in processor.pipes:
            if self.nopBeh.has_key(pipeStage.name):
                defineCode = 'R.set_stage(' + str(processor.pipes.index(pipeStage)) + ');\n'
                undefineCode = 'R.unset_stage();\n'
                behaviorBody = cxx_writer.Code(defineCode + '\n' + self.nopBeh[pipeStage.name] + '\n' + undefineCode)
                behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'public', [unlockQueueParam])
                NOPInstrElements.append(behaviorMethod)

        # Methods: replicate()
        replicateBody = cxx_writer.Code('return new NOPInstruction(' + instrCtorValues + ');')
        replicateMethod = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'public', parameters = [cxx_writer.Parameter('instr', instructionType.makePointer(), initValue = 'NULL')], noException = True, const = True)
        NOPInstrElements.append(replicateMethod)

        # Methods: get_name()
        getNameBody = cxx_writer.Code('return \"NOPInstruction\";')
        getNameMethod = cxx_writer.Method('get_name', getNameBody, cxx_writer.stringType, 'public', noException = True, const = True)
        NOPInstrElements.append(getNameMethod)

        # Methods: get_mnemonic()
        getMnemonicBody = cxx_writer.Code('return \"nop\";')
        getMnemonicMethod = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'public', noException = True, const = True)
        NOPInstrElements.append(getMnemonicMethod)

        # Methods: get_id()
        getIdBody = cxx_writer.Code('return (unsigned)-1;')
        getIdMethod = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'public', noException = True, const = True)
        NOPInstrElements.append(getIdMethod)

        # Constructors and Destructors
        NOPInstrCtor = cxx_writer.Constructor(emptyBody, 'public', parameters = instrCtorParams, initList = ['Instruction(' + instrCtorValues + ')'])
        NOPInstrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

        # Class
        NOPInstructionClass = cxx_writer.ClassDeclaration('NOPInstruction', NOPInstrElements, [instructionClass.getType()], namespaces = [namespace])
        NOPInstructionClass.addConstructor(NOPInstrCtor)
        NOPInstructionClass.addDestructor(NOPInstrDtor)
        instrClasses.append(NOPInstructionClass)

    ## @} NOPInstruction Class
    #---------------------------------------------------------------------------
    ## @name HelperOperation Classes

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
    """Returns the code implementing a single instruction. Implements
    all abstract methods of the base instruction class."""

    from procWriter import instrCtorParams, instrCtorValues
    from registerWriter import registerType, aliasType
    instructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
    unlockQueueType = cxx_writer.TemplateType('std::map', ['unsigned', cxx_writer.TemplateType('std::vector', [registerType.makePointer()], 'vector')], 'map')
    unlockQueueParam = cxx_writer.Parameter('unlock_queue', unlockQueueType.makeRef())
    externalClock = processor.externalClock

    emptyBody = cxx_writer.Code('')
    instrBases = []
    instrMembers = []
    instrCtorInit = ['Instruction(' + instrCtorValues + ')']

    #---------------------------------------------------------------------------
    ## @name Methods
    #  @{

    # Methods: behavior()
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

    hasCheckHazard = False
    checkHazardStage = None
    if model.startswith('acc'):
        # Now I have to add the code for checking data hazards
        for pipeStage in processor.pipes:
            if pipeStage.checkHazard:
                if processor.pipes.index(pipeStage) + 1 < len(processor.pipes):
                    # There exist stages between the beginning and the end of the hazard.
                    if not processor.pipes[processor.pipes.index(pipeStage) + 1].wb:
                        hasCheckHazard = True
                checkHazardStage = pipeStage.name

    behaviorUserCode = ''
    for pipeStage in processor.pipes:
        # Now I start computing the actual user-defined behavior of this instruction
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

        # Now I have to specify the code to manage data hazards in the processor.pipes; in particular to
        # add, if the current one is the writeBack stage, the registers locked in the read stage
        # to the unlock queue
        if model.startswith('acc'):
            if hasCheckHazard:
                behaviorUserCode += getToUnlockRegs(self, processor, pipeStage, False, True)
            behaviorCode = 'this->num_stage_cycles = 0;\n'
            if behaviorUserCode:
                # now I have to take all the resources and create a define which
                # renames such resources so that their usage can be transparent
                # to the developer
                behaviorCode += '\nR.set_stage(' + str(processor.pipes.index(pipeStage)) + ');\n'
                behaviorCode += behaviorUserCode
                behaviorCode += '\nR.unset_stage();\n'
                behaviorCode += 'return this->num_stage_cycles;\n'
                behaviorBody = cxx_writer.Code(behaviorCode)
                behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'public', [unlockQueueParam])
                instrMembers.append(behaviorMethod)
                behaviorUserCode = ''
    if not model.startswith('acc'):
        behaviorCode = 'this->num_instr_cycles = 0;\n'
        behaviorCode += behaviorUserCode
        behaviorCode += 'return this->num_instr_cycles;\n'
        behaviorBody = cxx_writer.Code(behaviorCode)
        behaviorMethod = cxx_writer.Method('behavior', behaviorBody, cxx_writer.uintType, 'public')
        instrMembers.append(behaviorMethod)

    # Methods: check_hazard(), lock_regs(), get_unlock(), print_busy_regs()
    # Here we deal with the code for checking data hazards: three methods are used for this purpose:
    # --- checkHazard: is called at the beginning of the register read stage to check that the
    #     registers needed by the current instruction are not being written by a previous one in
    #     the processor.pipes; in case this happens, the method contains the code to halt the processor.pipes stage;
    #     I have to check for the in/inout registers and special registers as needed by the instruction
    # --- lockRegs: also called at the beginning of the register read processor.pipes stage to lock the out/inout
    #     registers needed by the instruction
    # --- getUnlock: called in every stage when the instruction is annulled: this means that it is substituted
    #     with a nop instruction: as such, registers which were previously locked are added to the unlock queue;
    #     since it can be called from any processor.pipes stage, we have a copy of this method for all the stages
    # @see TODO in pipelineWriter.py preamble: The logic here does not honor special write-back sequences.
    if model.startswith('acc'):
        if hasCheckHazard:
            unlockHazard = False
            for pipeStage in processor.pipes:
                # check_hazard()
                checkHazardCode = ''
                if pipeStage.checkHazard:
                    regsToCheck = []
                    checkHazardCode = 'bool reg_locked = false;\n'
                    for name, correspondence in self.machineCode.bitCorrespondence.items():
                        if 'in' in self.machineCode.bitDirection[name]:
                            regsToCheck.append(name)
                    for name, correspondence in self.bitCorrespondence.items():
                        if 'in' in self.bitDirection[name]:
                            regsToCheck.append(name)
                    for regToCheck in regsToCheck:
                        if regToCheck not in self.notLockRegs:
                            # TODO:This checks if ANY stage is locked. Doesn't
                            # make sense of course, but errs on the safe side.
                            # @see TODO in pipelineWriter.py preamble.
                            checkHazardCode += 'reg_locked = this->' + regToCheck + '.is_locked(' + str(len(processor.pipes)) + ') || reg_locked;\n'
                    for pipeName, regList in self.specialInRegs.items():
                        for regToCheck in regList:
                            if regToCheck not in regsToCheck and pipeName != pipeStage.name:
                                checkHazardCode += 'reg_locked = this->' + regToCheck + '.is_locked(' + str([ pipeStageInner.name for pipeStageInner in processor.pipes ].index(pipeName)) + ') || reg_locked;\n'
                if self.customCheckHazardOp.has_key(pipeStage.name):
                    for customRegToCheck in self.customCheckHazardOp[pipeStage.name]:
                        checkHazardCode += 'reg_locked = ' + customRegToCheck + '.is_locked(' + str(processor.pipes.index(pipeStage)) + ') || reg_locked;\n'
                if checkHazardCode:
                    checkHazardCode += 'return !reg_locked;\n'
                    checkHazardBody = cxx_writer.Code(checkHazardCode)
                    checkHazardMethod = cxx_writer.Method('check_hazard_' + pipeStage.name, checkHazardBody, cxx_writer.boolType, 'public')
                    instrMembers.append(checkHazardMethod)

                # lock_regs()
                if pipeStage.checkHazard:
                    regsToLock = []
                    lockCode = ''
                    for name, correspondence in self.machineCode.bitCorrespondence.items():
                        if 'out' in self.machineCode.bitDirection[name]:
                            regsToLock.append(name)
                    for name, correspondence in self.bitCorrespondence.items():
                        if 'out' in self.bitDirection[name]:
                            regsToLock.append(name)
                    specialRegs = []
                    for reg in self.specialOutRegs.values():
                        specialRegs += reg
                    for specialRegName in specialRegs:
                        if specialRegName not in regsToLock:
                            regsToLock.append(specialRegName)
                    for regToLock in regsToLock:
                        if not regToLock in self.notLockRegs:
                            lockCode += 'this->' + regToLock + '.lock(' + str(processor.pipes.index(pipeStage)) + ');\n'
                    lockBody = cxx_writer.Code(lockCode)
                    lockMethod = cxx_writer.Method('lock_regs_' + pipeStage.name, lockBody, cxx_writer.voidType, 'public')
                    instrMembers.append(lockMethod)

                # get_unlock()
                if pipeStage.checkHazard:
                    unlockHazard = True
                if unlockHazard:
                    getUnlockCode = getToUnlockRegs(self, processor, pipeStage, True, False)
                    getUnlockBody = cxx_writer.Code(getUnlockCode)
                    getUnlockMethod = cxx_writer.Method('get_unlock_' + pipeStage.name, getUnlockBody, cxx_writer.voidType, 'public', [unlockQueueParam])
                    instrMembers.append(getUnlockMethod)

        if trace and hasCheckHazard:
            # Now we have to print the method for creating the data hazards
            regsToCheck = []
            Code = 'std::string ret_val = "";\n'
            for name, correspondence in self.machineCode.bitCorrespondence.items():
                if 'in' in self.machineCode.bitDirection[name]:
                    regsToCheck.append(name)
            for name, correspondence in self.bitCorrespondence.items():
                if 'in' in self.bitDirection[name]:
                    regsToCheck.append(name)
            specialRegList = []
            for reg in self.specialInRegs.values():
                specialRegList += reg
            for specialRegName in specialRegList:
                if not specialRegName in regsToCheck:
                    regsToCheck.append(specialRegName)

            for regToCheck in regsToCheck:
                if not regToCheck in self.notLockRegs:
                    Code += 'if (this->' + regToCheck + '.is_locked(' + str([ pipeStageInner.name for pipeStageInner in processor.pipes ].index(checkHazardStage)) + ')) {\n'
                    Code += 'ret_val += "' + regToCheck + ' - ";\n'
                    Code += '}\n'
            Code += 'return ret_val;\n'
            printBusyRegsBody = cxx_writer.Code(Code)
            printBusyRegsMethod = cxx_writer.Method('print_busy_regs', printBusyRegsBody, cxx_writer.stringType, 'public')
            instrMembers.append(printBusyRegsMethod)

    # Methods: replicate()
    archVars = []
    for behaviors in self.postbehaviors.values() + self.prebehaviors.values():
        for beh in behaviors:
            if (model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun):
                archVars += beh.archVars
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
    # now I need to declare the fields for the variable parts of the
    # instruction
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

    # Methods: get_name()
    getNameBody = cxx_writer.Code('return \"' + self.name + '\";')
    getNameMethod = cxx_writer.Method('get_name', getNameBody, cxx_writer.stringType, 'public', noException = True, const = True)
    instrMembers.append(getNameMethod)

    # Methods: get_id()
    getIdBody = cxx_writer.Code('return ' + str(self.id) + ';')
    getIdMethod = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'public', noException = True, const = True)
    instrMembers.append(getIdMethod)

    # Methods: set_params()
    # We need to create the attribute for the variables referenced by the non-constant parts of the instruction;
    # they are the bitCorrespondence variable of the machine code (they establish the correspondence with either registers
    # or aliases); they other remaining undefined parts of the instruction are normal integer variables.
    # Note, anyway, that I add the integer variable also for the parts of the instructions specified in
    # bitCorrespondence.
    Code = ''
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
    # now I need to declare the fields for the variable parts of the
    # instruction
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

    # Methods: get_mnemonic()
    # Here I declare the methods necessary to create the current instruction mnemonic given the current value of
    # the variable parts of the instruction
    Code = 'std::ostringstream oss (std::ostringstream::out);\n'
    for i in self.mnemonic:
        Code += getCPPInstrMnemonic(self, i)
    Code += 'return oss.str();'
    getMnemonicBody = cxx_writer.Code(Code)
    getMnemonicBody.addInclude('sstream')
    getMnemonicMethod = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'public', noException = True, const = True)
    instrMembers.append(getMnemonicMethod)

    # Attributes
    # Now I declare the instruction variables
    for var in self.variables:
        if not var.name in helperOpVars:
            instrMembers.append(cxx_writer.Attribute(var.name, var.varType, 'protected',  var.static))

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
    #num_allocatedAttribute = cxx_writer.Attribute('allocated', cxx_writer.uintType, 'private', initValue = '0', static = True)
    #instrMembers.append(num_allocatedAttribute)

    ########################## TODO: to eliminate, only for statistics ####################
    #out_poolAttribute = cxx_writer.Attribute('allocated_out', cxx_writer.uintType, 'private', static = True)
    #instrMembers.append(out_poolAttribute)
    #returnStatsMethod = cxx_writer.Method('get_count_my_alloc', cxx_writer.Code('return ' + self.name + '::allocated;'), cxx_writer.uintType, 'public')
    #instrMembers.append(returnStatsMethod)
    #returnStatsMethod = cxx_writer.Method('get_count_std_alloc', cxx_writer.Code('return ' + self.name + '::allocated_out;'), cxx_writer.uintType, 'public')
    #instrMembers.append(returnStatsMethod)
    ########################################################################################

    ## @} Methods
    #---------------------------------------------------------------------------

    # Constructors and Destructors
    instrCtor = cxx_writer.Constructor(emptyBody, 'public', parameters = instrCtorParams, initList = instrCtorInit)
    instrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

    # Class
    instrClass = cxx_writer.ClassDeclaration(self.name, instrMembers, superclasses = instrBases, namespaces = [namespace])
    instrClass.addDocString(brief = self.docbrief, detail = self.docdetail)
    instrClass.addConstructor(instrCtor)
    instrClass.addDestructor(instrDtor)

    #return [poolVar, instrClass] *** Again removed, related to the instruction pre-allocation
    return [instrClass]


################################################################################
# Instruction Test
################################################################################
def getCPPInstrTest(self, processor, model, trace, combinedTrace, namespace = ''):
    """Returns the code testing the current instruction. A test consists of
    setting the instruction variables, performing the instruction behavior and
    then comparing the registers with the expected value."""
    archElemsDeclStr = ''
    baseInitElement = '('
    destrDecls = ''

    if processor.regs or processor.regBanks:
        from registerWriter import registerContainerType
        archElemsDeclStr += registerContainerType.name + ' R('
        # Register const or reset values could be processor variables.
        # Since we do not have the values for those (probably program-dependent),
        # we pass on zeros to the Registers ctor.
        initRegCode = ''
        for reg in processor.regs:
            if isinstance(reg.constValue, str):
                initRegCode += '0, '
            if isinstance(reg.defValue, str):
                initRegCode += '0, '

        for regBank in processor.regBanks:
            for regConstValue in regBank.constValue.values():
                if isinstance(regConstValue, str):
                    initRegCode += '0, '
            for regDefaultValue in regBank.defValues:
                if isinstance(regDefaultValue, str):
                    initRegCode += '0, '
        if initRegCode:
            archElemsDeclStr += initRegCode[:-2] + ');\n'
        else:
            archElemsDeclStr += ');\n'
        # We also explicitly reset all regs to zero, instead of the reset value.
        # Test writers tend to mask status registers apart from teh bits they
        # are interested in, which is perhaps not quite correct but intuitive.
        archElemsDeclStr += 'R.write_force(0);\n'
        baseInitElement += 'R, '

    #memAliasInit = ''
    #for alias in processor.memAlias:
        #memAliasInit += ', ' + alias.alias

    if (trace or (processor.memory and processor.memory[2])) and not processor.systemc:
        archElemsDeclStr += 'unsigned total_cycles;\n'
    if processor.memory:
        memDebugInit = ''
        memCyclesInit = ''
        if processor.memory[2]:
            memCyclesInit += ', total_cycles'
        if processor.memory[3]:
            memDebugInit += ', ' + processor.memory[3]
        archElemsDeclStr += namespace + '::LocalMemory ' + processor.memory[0] + '(' + str(processor.memory[1]) + memCyclesInit + memDebugInit + ');\n'
        baseInitElement += processor.memory[0] + ', '
    # Note how I declare local memories even for TLM ports. I use 1MB as default dimension
    for tlmPorts in processor.tlmPorts.keys():
        archElemsDeclStr += namespace + '::LocalMemory ' + tlmPorts + '(' + str(1024*1024) + ');\n'
        baseInitElement += tlmPorts + ', '
    # Now I declare the PIN stubs for the outgoing PIN ports
    # and alts themselves
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
            archElemsDeclStr += namespace + '::' + pinPortTypeName + ' ' + pinPort.name + '_pin(\"' + pinPort.name + '_pin\");\n'
            archElemsDeclStr += 'PINTarget<' + str(pinPort.portWidth) + '> ' + pinPort.name + '_target_pin(\"' + pinPort.name + '_target_pin\");\n'
            archElemsDeclStr += pinPort.name + '_pin.init_socket.bind(' + pinPort.name + '_target_pin.target_socket);\n'
            baseInitElement += pinPort.name + '_pin, '

    if trace and not processor.systemc:
        baseInitElement += 'total_cycles, '
    baseInitElement = baseInitElement[:-2] + ')'
    tests = []
    for test in self.tests:
        # First of all I create the instance of the instruction and of all the
        # processor elements
        code = archElemsDeclStr + '\n'
        code += self.name + ' test_instruction' + baseInitElement + ';\n'
        # Now I set the value of the instruction fields
        instrCode = ['0' for i in range(0, self.machineCode.instrLen)]
        for name, elemValue in test[0].items():
            if self.machineCode.bitLen.has_key(name):
                curBitCode = toBinStr(elemValue, self.machineCode.bitLen[name])
                curBitCode.reverse()
                if len(curBitCode) > self.machineCode.bitLen[name]:
                    raise Exception('Cannot represent value ' + hex(elemValue) + ' of field ' + name + ' in test of instruction ' + self.name + ' in ' + str(self.machineCode.bitLen[name]) + ' bits.')
                for i in range(0, len(curBitCode)):
                    instrCode[self.machineCode.bitLen[name] + self.machineCode.bitPos[name] - i -1] = curBitCode[i]
            else:
                raise Exception('Field ' + name + ' in test of instruction ' + self.name + ' does not exist in the machine code.')
        for resource, value in test[1].items():
            # I set the initial value of the global resources
            bracket = resource.find('[')
            memories = processor.tlmPorts.keys()
            if processor.memory:
                memories.append(processor.memory[0])
            if bracket > 0 and resource[:bracket] in memories:
                try:
                    code += resource[:bracket] + '.write_word_dbg(' + hex(int(resource[bracket + 1:-1])) + ', ' + hex(value) + ');\n'
                except ValueError:
                    code += resource[:bracket] + '.write_word_dbg(' + hex(int(resource[bracket + 1:-1], 16)) + ', ' + hex(value) + ');\n'
            else:
                code += resource + '.write_force(' + hex(value) + ');\n'
        code += 'test_instruction.set_params(' + hex(int(''.join(instrCode), 2)) + ');\n'
        code += 'try {\n'
        code += 'test_instruction.behavior();'
        code += '\n}\ncatch(annul_exception& etc) {\n}\n\n'
        for resource, value in test[2].items():
            # I check the value of the listed resources to make sure that the
            # computation executed correctly
            code += 'BOOST_CHECK_EQUAL('
            bracket = resource.find('[')
            memories = processor.tlmPorts.keys()
            if processor.memory:
                memories.append(processor.memory[0])
            if bracket > 0 and resource[:bracket] in memories:
                try:
                    code += resource[:bracket] + '.read_word_dbg(' + hex(int(resource[bracket + 1:-1])) + ')'
                except ValueError:
                    code += resource[:bracket] + '.read_word_dbg(' + hex(int(resource[bracket + 1:-1], 16)) + ')'
            elif bracket > 0 and resource[:bracket] in outPinPorts:
                try:
                    code += resource[:bracket] + '_target_pin.read_pin(' + hex(int(resource[bracket + 1:-1])) + ')'
                except ValueError:
                    code += resource[:bracket] + '_target_pin.read_pin(' + hex(int(resource[bracket + 1:-1], 16)) + ')'
            else:
                code += resource + '.read_force()'
            code += ', (' + str(processor.bitSizes[1]) + ')' + hex(value) + ');\n\n'
        code += destrDecls
        curTest = cxx_writer.Code(code)
        wariningDisableCode = '#ifdef _WIN32\n#pragma warning(disable : 4101\n#endif\n'
        includeUnprotectedCode = '#define private public\n#define protected public\n#include \"instructions.hpp\"\n#include \"registers.hpp\"\n#include \"memory.hpp\"\n#undef private\n#undef protected\n'
        curTest.addInclude(['boost/test/test_tools.hpp', 'common/report.hpp', wariningDisableCode, includeUnprotectedCode])
        curTestFunction = cxx_writer.Function(self.name + '_' + str(len(tests)), curTest, cxx_writer.voidType)
        from procWriter import testNames
        testNames.append(self.name + '_' + str(len(tests)))
        tests.append(curTestFunction)
    return tests


################################################################################
# Test Top Level
################################################################################
def getCPPInstrTests(self, processor, modelType, trace, combinedTrace, namespace):
    if not processor.memory:
        return None
    # for each instruction I print the test: I do have to add some custom
    # code at the beginning in order to being able to access the private
    # part of the instructions
    tests = []
    for instr in self.instructions.values():
        tests += instr.getCPPTest(processor, modelType, trace, combinedTrace, namespace)
    return tests

################################################################################
