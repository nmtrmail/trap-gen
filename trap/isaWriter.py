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

# Contains, for each behavior, the type corresponding to the class which defines
# it. If a behavior is not here it means that it must be explicitly inlined
# in the instruction itself
baseBehaviors = []
behClass = {}
archWordType = None
alreadyDeclared = []
baseInstrConstrParams = []

def getToUnlockRegs(self, processor, pipeStage, getAll, delayedUnlock):
    code = ''
    regsToUnlock = []

    # Returns the list of registers which have to be unlocked in the current
    # pipeline stage; in case getAll is true, I have to unlock all of the
    # locked registers, and I return the pipeline register (ending with _pipe).
    # Else, I have to return only the registers for this particular stage,
    # so that only the stage register is unlocked

    # Now I have to insert the code to fill in the queue of registers to unlock
    if getAll or pipeStage.wb:
        # I have to save what are the special stages for each registers;
        # then I have to unlock all the stages but the ones specified
        # in this list
        regToStages = {}
        for toUnlockStage, regToUnlockList in self.specialOutRegs.items():
            for regToUnlock in regToUnlockList:
                regsToUnlock.append(regToUnlock)
                if regToStages.has_key(regToUnlock):
                    regToStages[regToUnlock].append(toUnlockStage)
                else:
                    regToStages[regToUnlock] = [toUnlockStage]
        # Now I compute the preceding stages:
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
    else:
        if self.specialOutRegs.has_key(pipeStage.name):
            for regToUnlock in self.specialOutRegs[pipeStage.name]:
                regsToUnlock.append(regToUnlock)
    if getAll or pipeStage.wb:
        # Here I have to unlock all the registers for which a special unlock stage
        # was not specified
        for regToUnlock in self.machineCode.bitCorrespondence.keys():
            if 'out' in self.machineCode.bitDirection[regToUnlock] and not regToUnlock in regsToUnlock:
                regsToUnlock.append(regToUnlock)
        for regToUnlock in self.bitCorrespondence.keys():
            if 'out' in self.bitDirection[regToUnlock] and not regToUnlock in regsToUnlock:
                regsToUnlock.append(regToUnlock)

    # Now I have to add registers to the queue of registers to unlock; note that
    # only the register itself can be added to the queue, not aliases.
    regsNames = [i.name for i in processor.regBanks + processor.regs]
    for regToUnlock in regsToUnlock:
        if not regToUnlock in self.notLockRegs:
            # Now I have to determine in detail what is/are the stages which have to be unlocked:
            # in case we are not in getAll or pipeStage.wb there are no problems, the current stage
            # has to be unlocked; otherwise, for non-special registers, we have to unlock all the stages
            # (i.e. we unlock the general register), while for special registers, we have to unlock
            # all the stages but the preceding ones already unlocked
            if getAll or pipeStage.wb:
                # Here I have to unlock all the normal registers and, for the special ones,
                # all the stages but the preceding ones already unlocked
                if not regToStages.has_key(regToUnlock):
                    # No special register
                    realName = regToUnlock
                    parenthesis = realName.find('[')
                    if parenthesis > 0:
                        realName = realName[:parenthesis]
                    if realName in regsNames:
                        if parenthesis > 0:
                            realRegName = realName + '_pipe' + regToUnlock[parenthesis:]
                        else:
                            realRegName = regToUnlock + '_pipe'
                    else:
                        if parenthesis > 0:
                            realRegName = realName + '_' + pipeStage.name + regToUnlock[parenthesis:]
                        else:
                            realRegName = regToUnlock + '_' + pipeStage.name
                    # finally now we can produce the code to perform the unlock operation
                    if delayedUnlock and self.delayedWb.has_key(regToUnlock):
                        if not realName in regsNames:
                            code += 'unlock_queue[' + str(self.delayedWb[regToUnlock]) + '].push_back(' + realRegName + '.get_pipe_reg());\n'
                        else:
                            code += 'unlock_queue[' + str(self.delayedWb[regToUnlock]) + '].push_back(&' + realRegName + ');\n'
                    else:
                        if not realName in regsNames:
                            code += 'unlock_queue[0].push_back(' + realRegName + '.get_pipe_reg());\n'
                        else:
                            code += 'unlock_queue[0].push_back(&' + realRegName + ');\n'
                else:
                    # Here we have a special register: lets determine all the stages which
                    # need to be unlocked:
                    toUnlockStages = []
                    for stageToUnlock in precedingStages:
                        if not stageToUnlock in regToStages[regToUnlock]:
                            toUnlockStages.append(stageToUnlock)
                    toUnlockStages += remainingStages
                    realName = regToUnlock
                    parenthesis = realName.find('[')
                    if parenthesis > 0:
                        realName = realName[:parenthesis]

                    # now I procede to create the code to unlock all the necessary pipeline stages
                    for toUnlockStage in toUnlockStages:
                        if realName in regsNames:
                            if parenthesis > 0:
                                realRegName = realName + '_' + toUnlockStage + regToUnlock[parenthesis:]
                            else:
                                realRegName = regToUnlock + '_' + toUnlockStage
                        else:
                            if parenthesis > 0:
                                realRegName = realName + '_' + toUnlockStage + regToUnlock[parenthesis:]
                            else:
                                realRegName = regToUnlock + '_' + toUnlockStage
                        # finally now we can produce the code to perform the unlock operation
                        if delayedUnlock and self.delayedWb.has_key(regToUnlock):
                            if not realName in regsNames:
                                code += 'unlock_queue[' + str(self.delayedWb[regToUnlock]) + '].push_back(' + realRegName + '.get_reg());\n'
                            else:
                                code += 'unlock_queue[' + str(self.delayedWb[regToUnlock]) + '].push_back(&' + realRegName + ');\n'
                        else:
                            if not realName in regsNames:
                                code += 'unlock_queue[0].push_back(' + realRegName + '.get_reg());\n'
                            else:
                                code += 'unlock_queue[0].push_back(&' + realRegName + ');\n'

                    # and finally we unlock the all stage
                    if realName in regsNames:
                        if parenthesis > 0:
                            realRegName = realName + '_pipe' + regToUnlock[parenthesis:]
                        else:
                            realRegName = regToUnlock + '_pipe'
                    else:
                        if parenthesis > 0:
                            realRegName = realName + '_' + pipeStage.name + regToUnlock[parenthesis:]
                        else:
                            realRegName = regToUnlock + '_' + pipeStage.name
                    # finally now we can produce the code to perform the unlock operation
                    if delayedUnlock and self.delayedWb.has_key(regToUnlock):
                        if not realName in regsNames:
                            code += 'unlock_queue[' + str(self.delayedWb[regToUnlock]) + '].push_back(' + realRegName + '.get_pipe_reg()->get_register());\n'
                        else:
                            code += 'unlock_queue[' + str(self.delayedWb[regToUnlock]) + '].push_back(' + realRegName + '.get_register());\n'
                    else:
                        if not realName in regsNames:
                            code += 'unlock_queue[0].push_back(' + realRegName + '.get_pipe_reg()->get_register());\n'
                        else:
                            code += 'unlock_queue[0].push_back(' + realRegName + '.get_register());\n'
            else:
                # here all the registers to unlock are the special registers:
                # this means that I simply have to unlock the registers corresponding
                # to the current stage
                realName = regToUnlock
                parenthesis = realName.find('[')
                if parenthesis > 0:
                    realName = realName[:parenthesis]
                    realRegName = realName + '_' + pipeStage.name + regToUnlock[parenthesis:]
                else:
                    realRegName = regToUnlock + '_' + pipeStage.name
                # finally now we can produce the code to perform the unlock operation
                if delayedUnlock and self.delayedWb.has_key(regToUnlock):
                    if not realName in regsNames:
                        code += 'unlock_queue[' + str(self.delayedWb[regToUnlock]) + '].push_back(' + realRegName + '.get_reg());\n'
                    else:
                        code += 'unlock_queue[' + str(self.delayedWb[regToUnlock]) + '].push_back(&' + realRegName + ');\n'
                else:
                    if not realName in regsNames:
                        code += 'unlock_queue[0].push_back(' + realRegName + '.get_reg());\n'
                    else:
                        code += 'unlock_queue[0].push_back(&' + realRegName + ');\n'

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

def getCppMethod(self, model, processor):
    """Returns the code implementing a helper method"""
    for var in self.localvars:
        self.code.addVariable(var)
    self.code.addInclude('common/report.hpp')
    import copy
    codeTemp = copy.deepcopy(self.code)

    defineCode = ''
    if model.startswith('acc'):
        # now I have to take all the resources and create a define which
        # renames such resources so that their usage can be transparent
        # to the developer
        defineCode += '\n'
        for reg in processor.regs + processor.regBanks + processor.aliasRegs + processor.aliasRegBanks:
            defineCode += '#define ' + reg.name + ' ' + reg.name + '_' + self.stage + '\n'
        defineCode += '\n'

    codeTemp.prependCode(defineCode)

    undefCode = ''
    if model.startswith('acc'):
        # now I have to take all the resources and create a define which
        # renames such resources so that their usage can be transparent
        # to the developer
        undefCode += '\n'
        for reg in processor.regs + processor.regBanks + processor.aliasRegs + processor.aliasRegBanks:
            undefCode += '#undef ' + reg.name + '\n'
        undefCode += '\n'

    codeTemp.appendCode(undefCode)

    methodDecl = cxx_writer.Method(self.name, codeTemp, self.retType, 'pu', self.parameters, noException = not self.exception, const = self.const)

    return methodDecl

def getCppOperation(self, parameters = False):
    """Returns the code implementing a helper operation"""
    aliasType = cxx_writer.Type('Alias', '#include \"alias.hpp\"')
    for var in self.localvars:
        self.code.addVariable(var)
    self.code.addInclude('common/report.hpp')
    metodParams = []
    if parameters:
        for elem in self.archElems:
            metodParams.append(cxx_writer.Parameter(elem, aliasType.makeRef()))
            metodParams.append(cxx_writer.Parameter(elem + '_bit', cxx_writer.uintRefType))
        for elem in self.archVars:
            metodParams.append(cxx_writer.Parameter(elem, cxx_writer.uintRefType))
        for var in self.instrvars:
            metodParams.append(cxx_writer.Parameter(var.name, var.type.makeRef()))
    methodDecl = cxx_writer.Method(self.name, self.code, cxx_writer.voidType, 'pro', metodParams, inline = True, noException = not self.exception)
    return methodDecl

def getCppOpClass(self, namespace):
    """Returns a class (directly deriving from instruction) implementing the
    method corresponding to the current operation"""
    global baseInstrConstrParams
    from procWriter import baseInstrInitElement
    aliasType = cxx_writer.Type('Alias', '#include \"alias.hpp\"')
    instructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
    emptyBody = cxx_writer.Code('')
    for var in self.localvars:
        self.code.addVariable(var)
    self.code.addInclude('common/report.hpp')
    classElements = []
    # Now I also need to declare the instruction variables and referenced architectural elements
    metodParams = []
    for elem in self.archElems:
        metodParams.append(cxx_writer.Parameter(elem, aliasType.makeRef()))
        metodParams.append(cxx_writer.Parameter(elem + '_bit', cxx_writer.uintRefType))
    for elem in self.archVars:
        metodParams.append(cxx_writer.Parameter(elem, cxx_writer.uintRefType))
    for var in self.instrvars:
        metodParams.append(cxx_writer.Parameter(var.name, var.varType.makeRef()))
    methodDecl = cxx_writer.Method(self.name, self.code, cxx_writer.voidType, 'pro', metodParams, inline = True, noException = not self.exception)
    classElements.append(methodDecl)
    opConstr = cxx_writer.Constructor(emptyBody, 'pu', baseInstrConstrParams, ['Instruction(' + baseInstrInitElement + ')'])
    opDecl = cxx_writer.ClassDeclaration(self.name + 'Operation', classElements, virtual_superclasses = [instructionType], namespaces = [namespace])
    opDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
    opDecl.addDestructor(opDestr)
    opDecl.addConstructor(opConstr)
    return opDecl

def getCPPInstrSwitch(obj, i):
    if type(i) == type(''):
        if i.startswith('%'):
            getMnemonicCode = 'oss << '
            if i[1:] in obj.machineCode.bitCorrespondence.keys() + obj.bitCorrespondence.keys():
                getMnemonicCode += 'std::dec << this->' + i[1:] + '_bit'
            else:
                getMnemonicCode += 'std::showbase << std::hex << this->' + i[1:]
            getMnemonicCode += ';\n'
        else:
            getMnemonicCode = 'oss << "' + i + '";\n'
    else:
        # I have a switch
        if i[0].startswith('%'):
            getMnemonicCode = 'switch(this->' + i[0][1:]
            if i[0][1:] in obj.machineCode.bitCorrespondence.keys() + obj.bitCorrespondence.keys():
                getMnemonicCode += '_bit'
            getMnemonicCode += ') {\n'
            for code, mnemValue in i[1].items():
                if code != 'default':
                    getMnemonicCode += 'case '
                getMnemonicCode += str(code) + ': {\n'
                if type(mnemValue) == type(''):
                    getMnemonicCode += 'oss << "' + mnemValue + '";\n'
                else:
                    getMnemonicCode += getCPPInstrSwitch(obj, mnemValue)
                getMnemonicCode += 'break;}\n'
            getMnemonicCode += '}\n'
        # I have a computation
        elif i[0].startswith('$'):
            getMnemonicCode = 'oss << std::showbase << std::hex << ('
            for j in i[1:]:
                if j.startswith('%'):
                    getMnemonicCode += 'this->' + j[1:]
                    if j[1:] in obj.machineCode.bitCorrespondence.keys() + obj.bitCorrespondence.keys():
                        getMnemonicCode += '_bit'
                else:
                    getMnemonicCode += j
            getMnemonicCode += ');\n'
        else:
            raise Exception('Expected % as the first element of multi-word mnemonic in instruction ' + obj.name + '.')
    return getMnemonicCode

def getCPPInstr(self, model, processor, trace, combinedTrace, namespace):
    """Returns the code implementing the current instruction: we have to provide the
    implementation of all the abstract methods and call from the behavior method
    all the different behaviors contained in the type hierarchy of this class"""
    pipeline = processor.pipes
    externalClock = processor.externalClock
    aliasType = cxx_writer.Type('Alias', '#include \"alias.hpp\"')
    instructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
    emptyBody = cxx_writer.Code('')
    classElements = []
    baseClasses = []
    toInline = []
    behVars = []
    from procWriter import baseInstrInitElement
    global baseInstrConstrParams
    constrInitList = ['Instruction(' + baseInstrInitElement + ')']
    global alreadyDeclared
    global behClass
    for behaviors in self.postbehaviors.values() + self.prebehaviors.values():
        for beh in behaviors:
            if (model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun):
                if behClass.has_key(beh.name):
                    baseClasses.append(behClass[beh.name].getType())
                    constrInitList.append(beh.name + 'Operation(' + baseInstrInitElement + ')')
                elif beh.inline and not beh.name in alreadyDeclared and not model.startswith('acc'):
                    classElements.append(beh.getCppOperation())
                elif not beh.name in alreadyDeclared:
                    toInline.append(beh.name)
                for var in beh.instrvars:
                    if not var.name in behVars:
                        classElements.append(cxx_writer.Attribute(var.name, var.varType, 'pro',  var.static))
                        behVars.append(var.name)
    if not baseClasses:
        baseClasses.append(instructionType)

    if model.startswith('acc'):
        # Now I have to add the code for checking data hazards
        hasCheckHazard = False
        hasWb = False
        for pipeStage in pipeline:
            if pipeStage.checkHazard:
                if pipeline.index(pipeStage) + 1 < len(pipeline):
                    if not pipeline[pipeline.index(pipeStage) + 1].wb:
                        hasCheckHazard = True
            if pipeStage.wb:
                if pipeline.index(pipeStage) - 1 >= 0:
                    if not pipeline[pipeline.index(pipeStage) - 1].checkHazard:
                        hasWb = True

    if not model.startswith('acc'):
        behaviorCode = 'this->total_instr_cycles = 0;\n'

    for pipeStage in pipeline:
        userDefineBehavior = ''
        if model.startswith('acc'):
            behaviorCode = 'this->stage_cycles = 0;\n'

        # Now I start computing the actual user-defined behavior of this instruction
        if self.prebehaviors.has_key(pipeStage.name):
            for beh in self.prebehaviors[pipeStage.name]:
                if not ((model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun)):
                    continue
                if beh.name in toInline:
                    userDefineBehavior += ' {\n'
                    for var in beh.localvars:
                        userDefineBehavior += str(var)
                    userDefineBehavior += str(beh.code)
                    userDefineBehavior += '}\n'
                elif behClass.has_key(beh.name) or beh.name in baseBehaviors:
                    userDefineBehavior += 'this->' + beh.name + '('
                    for elem in beh.archElems:
                        userDefineBehavior += 'this->' + elem + ', '
                        userDefineBehavior += 'this->' + elem + '_bit'
                        if beh.archVars or beh.instrvars or elem != beh.archElems[-1]:
                            userDefineBehavior += ', '
                    for elem in beh.archVars:
                        userDefineBehavior += 'this->' + elem
                        if beh.instrvars or elem != beh.archVars[-1]:
                            userDefineBehavior += ', '
                    for var in beh.instrvars:
                        userDefineBehavior += 'this->' + var.name
                        if var != beh.instrvars[-1]:
                            userDefineBehavior += ', '
                    userDefineBehavior += ');\n'
                else:
                    userDefineBehavior += 'this->' + beh.name + '();\n'
        if self.code.has_key(pipeStage.name):
            userDefineBehavior += str(self.code[pipeStage.name].code)
        if self.postbehaviors.has_key(pipeStage.name):
            for beh in self.postbehaviors[pipeStage.name]:
                if not ((model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun)):
                    continue
                if beh.name in toInline:
                    userDefineBehavior += str(beh.code)
                elif behClass.has_key(beh.name) or beh.name in baseBehaviors:
                    userDefineBehavior += 'this->' + beh.name + '('
                    for elem in beh.archElems:
                        userDefineBehavior += 'this->' + elem + ', '
                        userDefineBehavior += 'this->' + elem + '_bit'
                        if beh.archVars or beh.instrvars or elem != beh.archElems[-1]:
                            userDefineBehavior += ', '
                    for elem in beh.archVars:
                        userDefineBehavior += 'this->' + elem
                        if beh.instrvars or elem != beh.archVars[-1]:
                            userDefineBehavior += ', '
                    for var in beh.instrvars:
                        userDefineBehavior += 'this->' + var.name
                        if var != beh.instrvars[-1]:
                            userDefineBehavior += ', '
                    userDefineBehavior += ');\n'
                else:
                    userDefineBehavior += 'this->' + beh.name + '();\n'

        # Now I have to specify the code to manage data hazards in the pipeline; in particular to
        # add, if the current one is the writeBack stage, the registers locked in the read stage
        # to the unlock queue
        if model.startswith('acc'):
            if hasCheckHazard:
                userDefineBehavior += getToUnlockRegs(self, processor, pipeStage, False, True)

            if userDefineBehavior:
                # now I have to take all the resources and create a define which
                # renames such resources so that their usage can be transparent
                # to the developer
                behaviorCode += '\n'
                for reg in processor.regs + processor.regBanks + processor.aliasRegs + processor.aliasRegBanks:
                    behaviorCode += '#define ' + reg.name + ' ' + reg.name + '_' + pipeStage.name + '\n'
                for instrFieldName in self.machineCode.bitCorrespondence.keys() + self.bitCorrespondence.keys():
                    behaviorCode += '#define ' + instrFieldName + ' ' + instrFieldName + '_' + pipeStage.name + '\n'
                behaviorCode += '\n'

        behaviorCode += userDefineBehavior

        if model.startswith('acc'):
            if userDefineBehavior:
                behaviorCode += '\n'
                for reg in processor.regs + processor.regBanks + processor.aliasRegs + processor.aliasRegBanks:
                    behaviorCode += '#undef ' + reg.name + '\n'
                for instrFieldName in self.machineCode.bitCorrespondence.keys() + self.bitCorrespondence.keys():
                    behaviorCode += '#undef ' + instrFieldName + '\n'

            behaviorCode += 'return this->stage_cycles;\n\n'
            registerType = cxx_writer.Type('Register')
            unlockQueueType = cxx_writer.TemplateType('std::map', ['unsigned', cxx_writer.TemplateType('std::vector', [registerType.makePointer()], 'vector')], 'map')
            unlockQueueParam = cxx_writer.Parameter('unlock_queue', unlockQueueType.makeRef())
            behaviorBody = cxx_writer.Code(behaviorCode)
            behaviorDecl = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'pu', [unlockQueueParam])
            classElements.append(behaviorDecl)
    if not model.startswith('acc'):
        behaviorCode += 'return this->total_instr_cycles;'
        behaviorBody = cxx_writer.Code(behaviorCode)
        behaviorDecl = cxx_writer.Method('behavior', behaviorBody, cxx_writer.uintType, 'pu')
        classElements.append(behaviorDecl)

    # Here we deal with the code for checking data hazards: three methods are used for this purpose:
    # --- checkHazard: is called at the beginning of the register read stage to check that the
    #     registers needed by the current instruction are not being written by a previous one in
    #     the pipeline; in case this happens, the method contains the code to halt the pipeline stage;
    #     I have to check for the in/inout registers and special registers as needed by the instruction
    # --- lockRegs: also called at the beginning of the register read pipeline stage to lock the out/inout
    #     registers needed by the instruction
    # --- getUnlock: called in every stage when the instruction is annulled: this means that it is substituted
    #     with a nop instruction: as such, registers which were previously locked are added to the unlock queue;
    #     since it can be called from any pipeline stage, we have a copy of this method for all the stages
    if model.startswith('acc'):
        from pipelineWriter import hasCheckHazard
        from pipelineWriter import wbStage
        from pipelineWriter import chStage

        for ps in processor.pipes:
            if ps.checkHazard:
                checkHazardStage = ps.name

        # Now we have to print the method for creating the data hazards
        regsToCheck = []
        printBusyRegsCode = 'std::string ret_val = "";\n'
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
                parenthesis = regToCheck.find('[')
                if parenthesis > 0:
                    realRegName = regToCheck[:parenthesis] + '_' + checkHazardStage + regToCheck[parenthesis:]
                else:
                    realRegName = regToCheck + '_' + checkHazardStage
                printBusyRegsCode += 'if (this->' + realRegName + '.is_locked()) {\n'
                printBusyRegsCode += 'ret_val += "' + regToCheck + ' - ";\n'
                printBusyRegsCode += '}\n'
        printBusyRegsCode += 'return ret_val;\n'
        printBusyRegsBody = cxx_writer.Code(printBusyRegsCode)
        printBusyRegsDecl = cxx_writer.Method('print_busy_regs', printBusyRegsBody, cxx_writer.stringType, 'pu')
        classElements.append(printBusyRegsDecl)

        if hasCheckHazard:
            regsNames = [i.name for i in processor.regBanks + processor.regs]
            # checkHazard: I have to build such a method for each pipeline stage
            for pipeStage in pipeline:
                regsToCheck = []
                checkHazardCode = 'bool reg_locked = false;\n'
                if pipeStage.checkHazard:
                    for name, correspondence in self.machineCode.bitCorrespondence.items():
                        if 'in' in self.machineCode.bitDirection[name]:
                            regsToCheck.append(name)
                    for name, correspondence in self.bitCorrespondence.items():
                        if 'in' in self.bitDirection[name]:
                            regsToCheck.append(name)
                    regsToCheckTemp = regsToCheck
                    regsToCheck = []
                    for regToCheck in regsToCheckTemp:
                        if not regToCheck in self.notLockRegs:
                            parenthesis = regToCheck.find('[')
                            if parenthesis > 0:
                                regsToCheck.append(regToCheck[:parenthesis] + '_' + pipeStage.name + regToCheck[parenthesis:])
                            else:
                                regsToCheck.append(regToCheck + '_' + pipeStage.name)
                    for pipeName, regList in self.specialInRegs.items():
                        for regToCheck in regList:
                            parenthesis = regToCheck.find('[')
                            if parenthesis > 0:
                                regsToCheck.append(regToCheck[:parenthesis] + '_' + pipeName + regToCheck[parenthesis:])
                            else:
                                regsToCheck.append(regToCheck + '_' + pipeName)

                for regToCheck in regsToCheck:
                    checkHazardCode += 'reg_locked = this->' + regToCheck + '.is_locked() || reg_locked;\n'

                if self.customCheckHazardOp.has_key(pipeStage.name):
                    checkHazardCode += 'reg_locked = ' + self.customCheckHazardOp[pipeStage.name] + ' || reg_locked;\n'

                checkHazardCode += 'return !reg_locked;\n'
                checkHazardBody = cxx_writer.Code(checkHazardCode)
                checkHazardDecl = cxx_writer.Method('check_hazard_' + pipeStage.name, checkHazardBody, cxx_writer.boolType, 'pu')
                classElements.append(checkHazardDecl)
                # lockRegs
                regsToLock = []
                lockCode = ''
                if pipeStage.checkHazard:
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
                            parenthesis = regToLock.find('[')
                            if parenthesis > 0:
                                if regToLock[:parenthesis] in regsNames:
                                    realRegName = regToLock[:parenthesis] + '_pipe' + regToLock[parenthesis:]
                                else:
                                    realRegName = regToLock[:parenthesis] + '_' + pipeStage.name + regToLock[parenthesis:]
                            else:
                                if regToLock in regsNames:
                                    realRegName = regToLock + '_pipe'
                                else:
                                    realRegName = regToLock + '_' + pipeStage.name
                            lockCode += 'this->' + realRegName + '.lock();\n'
                lockBody = cxx_writer.Code(lockCode)
                lockDecl = cxx_writer.Method('lock_regs_' + pipeStage.name, lockBody, cxx_writer.voidType, 'pu')
                classElements.append(lockDecl)

            unlockHazard = False
            for pipeStage in pipeline:
                if pipeStage.checkHazard:
                    unlockHazard = True
                if unlockHazard:
                    # getUnlock
                    getUnlockCode = getToUnlockRegs(self, processor, pipeStage, True, False)
                    getUnlockBody = cxx_writer.Code(getUnlockCode)
                    getUnlockDecl = cxx_writer.Method('get_unlock_' + pipeStage.name, getUnlockBody, cxx_writer.voidType, 'pu', [unlockQueueParam])
                    classElements.append(getUnlockDecl)

    replicateBody = cxx_writer.Code('return new ' + self.name + '(' + baseInstrInitElement + ');')
    replicateDecl = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'pu', noException = True, const = True)
    classElements.append(replicateDecl)
    getIstructionNameBody = cxx_writer.Code('return \"' + self.name + '\";')
    getIstructionNameDecl = cxx_writer.Method('get_name', getIstructionNameBody, cxx_writer.stringType, 'pu', noException = True, const = True)
    classElements.append(getIstructionNameDecl)
    getIdBody = cxx_writer.Code('return ' + str(self.id) + ';')
    getIdDecl = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'pu', noException = True, const = True)
    classElements.append(getIdDecl)

    # We need to create the attribute for the variables referenced by the non-constant parts of the instruction;
    # they are the bitCorrespondence variable of the machine code (they establish the correspondence with either registers
    # or aliases); they other remaining undefined parts of the instruction are normal integer variables.
    # Note, anyway, that I add the integer variable also for the parts of the instructions specified in
    # bitCorrespondence.
    if model.startswith('acc'):
        bitCorrInit = ''
    setParamsCode = ''
    for name, correspondence in self.machineCode.bitCorrespondence.items() + self.bitCorrespondence.items():
        if model.startswith('acc'):
            curPipeId = 0
            for pipeStage in pipeline:
                classElements.append(cxx_writer.Attribute(name + '_' + pipeStage.name, aliasType, 'pri'))
                bitCorrInit += 'this->' + name + '_' + pipeStage.name + '.set_pipe_id(' + str(curPipeId) + ');\n'
                curPipeId += 1
        else:
            classElements.append(cxx_writer.Attribute(name, aliasType, 'pri'))
        classElements.append(cxx_writer.Attribute(name + '_bit', cxx_writer.uintType, 'pri'))
        mask = ''
        for i in range(0, self.machineCode.bitPos[name]):
            mask += '0'
        for i in range(0, self.machineCode.bitLen[name]):
            mask += '1'
        for i in range(0, self.machineCode.instrLen - self.machineCode.bitPos[name] - self.machineCode.bitLen[name]):
            mask += '0'
        shiftAmm = self.machineCode.instrLen - self.machineCode.bitPos[name] - self.machineCode.bitLen[name]
        setParamsCode += 'this->' + name + '_bit = (bitstring & ' + hex(int(mask, 2)) + ')'
        if shiftAmm > 0:
            setParamsCode += ' >> ' + str(shiftAmm)
        setParamsCode += ';\n'
        #if processor.instructionCache:
            #updateMetodName = 'update_alias'
        #else:
            #updateMetodName = 'direct_set_alias'
        updateMetodName = 'direct_set_alias'
        if correspondence[1]:
            if model.startswith('acc'):
                for pipeStage in pipeline:
                    setParamsCode += 'this->' + name + '_' + pipeStage.name + '.' + updateMetodName + '(this->' + correspondence[0] + '_' + pipeStage.name + '[' + str(correspondence[1]) + ' + this->' + name + '_bit]);\n'
            else:
                setParamsCode += 'this->' + name + '.' + updateMetodName + '(this->' + correspondence[0] + '[' + str(correspondence[1]) + ' + this->' + name + '_bit]);\n'
        else:
            if model.startswith('acc'):
                for pipeStage in pipeline:
                    setParamsCode += 'this->' + name + '_' + pipeStage.name + '.' + updateMetodName + '(this->' + correspondence[0] + '_' + pipeStage.name + '[this->' + name + '_bit]);\n'
            else:
                setParamsCode += 'this->' + name + '.' + updateMetodName + '(this->' + correspondence[0] + '[this->' + name + '_bit]);\n'
    # now I need to declare the fields for the variable parts of the
    # instruction
    archVars = []
    for behaviors in self.postbehaviors.values() + self.prebehaviors.values():
        for beh in behaviors:
            if (model.startswith('acc') and beh.name in self.behaviorAcc) or (model.startswith('func') and beh.name in self.behaviorFun):
                archVars += beh.archVars
    for name, length in self.machineCode.bitFields:
        if name in self.machineCode.bitCorrespondence.keys() + self.bitCorrespondence.keys():
            continue
            # NOTE; This one-liner saved 50+ compilation errors:
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
        classElements.append(cxx_writer.Attribute(name, cxx_writer.uintType, 'pri'))
        mask = ''
        for i in range(0, self.machineCode.bitPos[name]):
            mask += '0'
        for i in range(0, self.machineCode.bitLen[name]):
            mask += '1'
        for i in range(0, self.machineCode.instrLen - self.machineCode.bitPos[name] - self.machineCode.bitLen[name]):
            mask += '0'
        shiftAmm = self.machineCode.instrLen - self.machineCode.bitPos[name] - self.machineCode.bitLen[name]
        setParamsCode += 'this->' + name + ' = (bitstring & ' + hex(int(mask, 2)) + ')'
        if shiftAmm > 0:
            setParamsCode += ' >> ' + str(shiftAmm)
        setParamsCode += ';\n'
    setParamsBody = cxx_writer.Code(setParamsCode)
    setparamsParam = cxx_writer.Parameter('bitstring', processor.bitSizes[1].makeRef().makeConst())
    setparamsDecl = cxx_writer.Method('set_params', setParamsBody, cxx_writer.voidType, 'pu', [setparamsParam], noException = True)
    classElements.append(setparamsDecl)

    # Here I declare the methods necessary to create the current instruction mnemonic given the current value of
    # the variable parts of the instruction
    getMnemonicCode = 'std::ostringstream oss (std::ostringstream::out);\n'

    for i in self.mnemonic:
        getMnemonicCode += getCPPInstrSwitch(self, i)
    getMnemonicCode += 'return oss.str();'
    getMnemonicBody = cxx_writer.Code(getMnemonicCode)
    getMnemonicBody.addInclude('sstream')
    getMnemonicDecl = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'pu', noException = True, const = True)
    classElements.append(getMnemonicDecl)

    # Now I declare the instruction variables
    for var in self.variables:
        if not var.name in behVars:
            classElements.append(cxx_writer.Attribute(var.name, var.varType, 'pro',  var.static))

    # Finally now I have to override the basic new operator in
    # order to speed up memory allocation (***** Commented since it does not give any speedup ******)
    #num_allocated = processor.alloc_buffer_size*self.frequency
    #poolDecl = cxx_writer.Variable(self.name + '_pool[' + str(num_allocated) + '*sizeof(' + self.name + ')]', cxx_writer.ucharType, namespaces = [namespace])
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
    #operatorNewDecl = cxx_writer.MemberOperator('new', operatorNewBody, cxx_writer.voidPtrType, 'pu', operatorNewParams)
    #classElements.append(operatorNewDecl)
    #operatorDelCode = """
        #if (m != NULL && (m < """ + self.name + """_pool || m > (""" + self.name + """_pool + """ + str(num_allocated - 1) + """*sizeof(""" + self.name + """)))) {
            #::free(m);
        #}
    #"""
    #operatorDelBody =  cxx_writer.Code(operatorDelCode)
    #operatorDelParams = [cxx_writer.Parameter('m', cxx_writer.voidPtrType)]
    #operatorDelDecl = cxx_writer.MemberOperator('delete', operatorDelBody, cxx_writer.voidType, 'pu', operatorDelParams)
    #classElements.append(operatorDelDecl)
    #num_allocatedAttribute = cxx_writer.Attribute('allocated', cxx_writer.uintType, 'pri', initValue = '0', static = True)
    #classElements.append(num_allocatedAttribute)

    ########################## TODO: to eliminate, only for statistics ####################
    #out_poolAttribute = cxx_writer.Attribute('allocated_out', cxx_writer.uintType, 'pri', static = True)
    #classElements.append(out_poolAttribute)
    #returnStatsDecl = cxx_writer.Method('get_count_my_alloc', cxx_writer.Code('return ' + self.name + '::allocated;'), cxx_writer.uintType, 'pu')
    #classElements.append(returnStatsDecl)
    #returnStatsDecl = cxx_writer.Method('get_count_std_alloc', cxx_writer.Code('return ' + self.name + '::allocated_out;'), cxx_writer.uintType, 'pu')
    #classElements.append(returnStatsDecl)
    ########################################################################################

    # Now I have to declare the constructor
    from procWriter import baseInstrInitElement
    if model.startswith('acc'):
        publicConstrBody = cxx_writer.Code(bitCorrInit)
    else:
        publicConstrBody = emptyBody
    publicConstr = cxx_writer.Constructor(publicConstrBody, 'pu', baseInstrConstrParams, constrInitList)
    instructionDecl = cxx_writer.ClassDeclaration(self.name, classElements, superclasses = baseClasses, namespaces = [namespace])
    instructionDecl.addDocString(brief = self.docbrief, detail = self.docdetail)
    instructionDecl.addConstructor(publicConstr)
    publicDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
    instructionDecl.addDestructor(publicDestr)
    #return [poolDecl, instructionDecl] *** Again removed, related to the instruction pre-allocation
    return [instructionDecl]

def getCPPInstrTest(self, processor, model, trace, combinedTrace, namespace = ''):
    """Returns the code testing the current instruction: note that a test
    consists in setting the instruction variables, performing the instruction
    behavior and then comparing the registers with what we expect."""
    archElemsDeclStr = ''
    baseInitElement = '('
    destrDecls = ''
    from procWriter import resourceType
    for reg in processor.regs:
        archElemsDeclStr += namespace + '::' + str(resourceType[reg.name]) + ' ' + reg.name + ';\n'
        baseInitElement += reg.name + ', '
    for regB in processor.regBanks:
        if (regB.constValue and len(regB.constValue) < regB.numRegs)  or (regB.delay and len(regB.delay) < regB.numRegs):
            archElemsDeclStr += str(resourceType[regB.name]) + ' ' + regB.name + '(' + str(regB.numRegs) + ');\n'
            for i in range(0, regB.numRegs):
                if regB.constValue.has_key(i) or regB.delay.has_key(i):
                    archElemsDeclStr += regB.name + '.set_new_register(' + str(i) + ', new ' + str(resourceType[regB.name + '[' + str(i) + ']']) + '());\n'
                else:
                    archElemsDeclStr += regB.name + '.set_new_register(' + str(i) + ', new ' + str(resourceType[regB.name + '_baseType']) + '());\n'
        else:
            archElemsDeclStr += str(resourceType[regB.name]) + ' ' + regB.name + ' = new ' + str(resourceType[regB.name].makeNormal()) + '[' + str(regB.numRegs) + '];\n'
            destrDecls += 'delete [] ' + regB.name + ';\n'
        baseInitElement += regB.name + ', '
    for alias in processor.aliasRegs:
        archElemsDeclStr += namespace + '::' + str(resourceType[alias.name]) + ' ' + alias.name + ';\n'
        baseInitElement += alias.name + ', '
    for aliasB in processor.aliasRegBanks:
        archElemsDeclStr += namespace + '::' + str(resourceType[aliasB.name].makePointer()) + ' ' + aliasB.name + ' = new ' + namespace + '::' + str(resourceType[aliasB.name]) + '[' + str(aliasB.numRegs) + '];\n'
        baseInitElement += aliasB.name + ', '
        destrDecls += 'delete [] ' + aliasB.name + ';\n'
    memAliasInit = ''
    for alias in processor.memAlias:
        memAliasInit += ', ' + alias.alias

    if (trace or (processor.memory and processor.memory[2])) and not processor.systemc:
        archElemsDeclStr += 'unsigned total_cycles;\n'
    if processor.memory:
        memDebugInit = ''
        memCyclesInit = ''
        if processor.memory[2]:
            memCyclesInit += ', total_cycles'
        if processor.memory[3]:
            memDebugInit += ', ' + processor.memory[3]
        archElemsDeclStr += namespace + '::LocalMemory ' + processor.memory[0] + '(' + str(processor.memory[1]) + memCyclesInit + memAliasInit + memDebugInit + ');\n'
        baseInitElement += processor.memory[0] + ', '
    # Note how I declare local memories even for TLM ports. I use 1MB as default dimension
    for tlmPorts in processor.tlmPorts.keys():
        archElemsDeclStr += namespace + '::LocalMemory ' + tlmPorts + '(' + str(1024*1024) + memAliasInit + ');\n'
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
    # Now we perform the alias initialization; note that they need to be initialized according to the initialization graph
    # (there might be dependences among the aliases)
    aliasInit = ''
    import networkx as NX
    from procWriter import aliasGraph
    from processor import extractRegInterval
    orderedNodes = NX.topological_sort(aliasGraph)
    for alias in orderedNodes:
        if alias == 'stop':
            continue
        if isinstance(alias.initAlias, type('')):
            index = extractRegInterval(alias.initAlias)
            if index:
                curIndex = index[0]
                try:
                    for i in range(0, alias.numRegs):
                        aliasInit += alias.name + '[' + str(i) + '].update_alias(' + alias.initAlias[:alias.initAlias.find('[')] + '[' + str(curIndex) + ']);\n'
                        curIndex += 1
                except AttributeError:
                    aliasInit += alias.name + '.update_alias(' + alias.initAlias[:alias.initAlias.find('[')] + '[' + str(curIndex) + '], ' + str(alias.offset) + ');\n'
            else:
                aliasInit += alias.name + '.update_alias(' + alias.initAlias + ', ' + str(alias.offset) + ');\n'
        else:
            curIndex = 0
            for curAlias in alias.initAlias:
                index = extractRegInterval(curAlias)
                if index:
                    for curRange in range(index[0], index[1] + 1):
                        aliasInit += alias.name + '[' + str(curIndex) + '].update_alias(' + curAlias[:curAlias.find('[')] + '[' + str(curRange) + ']);\n'
                        curIndex += 1
                else:
                    aliasInit += alias.name + '[' + str(curIndex) + '].update_alias(' + curAlias + ');\n'
                    curIndex += 1
    tests = []
    for test in self.tests:
        # First of all I create the instance of the instruction and of all the
        # processor elements
        code = archElemsDeclStr + '\n' + aliasInit + '\n'
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
            brackIndex = resource.find('[')
            memories = processor.tlmPorts.keys()
            if processor.memory:
                memories.append(processor.memory[0])
            if brackIndex > 0 and resource[:brackIndex] in memories:
                try:
                    code += resource[:brackIndex] + '.write_word_dbg(' + hex(int(resource[brackIndex + 1:-1])) + ', ' + hex(value) + ');\n'
                except ValueError:
                    code += resource[:brackIndex] + '.write_word_dbg(' + hex(int(resource[brackIndex + 1:-1], 16)) + ', ' + hex(value) + ');\n'
            else:
                code += resource + '.immediate_write(' + hex(value) + ');\n'
        code += 'test_instruction.set_params(' + hex(int(''.join(instrCode), 2)) + ');\n'
        code += 'try {\n'
        code += 'test_instruction.behavior();'
        code += '\n}\ncatch(annul_exception& etc) {\n}\n\n'
        for resource, value in test[2].items():
            # I check the value of the listed resources to make sure that the
            # computation executed correctly
            code += 'BOOST_CHECK_EQUAL('
            brackIndex = resource.find('[')
            memories = processor.tlmPorts.keys()
            if processor.memory:
                memories.append(processor.memory[0])
            if brackIndex > 0 and resource[:brackIndex] in memories:
                try:
                    code += resource[:brackIndex] + '.read_word_dbg(' + hex(int(resource[brackIndex + 1:-1])) + ')'
                except ValueError:
                    code += resource[:brackIndex] + '.read_word_dbg(' + hex(int(resource[brackIndex + 1:-1], 16)) + ')'
            elif brackIndex > 0 and resource[:brackIndex] in outPinPorts:
                try:
                    code += resource[:brackIndex] + '_target_pin.read_pin(' + hex(int(resource[brackIndex + 1:-1])) + ')'
                except ValueError:
                    code += resource[:brackIndex] + '_target_pin.read_pin(' + hex(int(resource[brackIndex + 1:-1], 16)) + ')'
            else:
                code += resource + '.read_new_value()'
            code += ', (' + str(processor.bitSizes[1]) + ')' + hex(value) + ');\n\n'
        code += destrDecls
        curTest = cxx_writer.Code(code)
        wariningDisableCode = '#ifdef _WIN32\n#pragma warning(disable : 4101\n#endif\n'
        includeUnprotectedCode = '#define private public\n#define protected public\n#include \"instructions.hpp\"\n#include \"registers.hpp\"\n#include \"memory.hpp\"\n#undef private\n#undef protected\n'
        curTest.addInclude(['boost/test/test_tools.hpp', 'common/report.hpp', wariningDisableCode, includeUnprotectedCode, '#include \"alias.hpp\"'])
        curTestFunction = cxx_writer.Function(self.name + '_' + str(len(tests)), curTest, cxx_writer.voidType)
        from procWriter import testNames
        testNames.append(self.name + '_' + str(len(tests)))
        tests.append(curTestFunction)
    return tests

def getCPPClasses(self, processor, model, trace, combinedTrace, namespace):
    """I go over each instruction and print the class representing it"""
    memoryType = cxx_writer.Type('MemoryInterface', '#include \"memory.hpp\"')
    registerType = cxx_writer.Type('Register')
    unlockQueueType = cxx_writer.TemplateType('std::map', ['unsigned', cxx_writer.TemplateType('std::vector', [registerType.makePointer()], 'vector')], 'map')

    classes = []
    # Now I add the custon definitions
    for i in self.defines:
        classes.append(cxx_writer.Define(i + '\n'))

    # First of all I create the base instruction type: note that it contains references
    # to the architectural elements
    instructionType = cxx_writer.Type('Instruction')
    instructionElements = []
    emptyBody = cxx_writer.Code('')
    if not model.startswith('acc'):
        behaviorDecl = cxx_writer.Method('behavior', emptyBody, cxx_writer.uintType, 'pu', pure = True)
        instructionElements.append(behaviorDecl)
    else:
        unlockQueueParam = cxx_writer.Parameter('unlock_queue', unlockQueueType.makeRef())
        for pipeStage in processor.pipes:
            behaviorDecl = cxx_writer.Method('behavior_' + pipeStage.name, emptyBody, cxx_writer.uintType, 'pu', [unlockQueueParam], pure = True)
            instructionElements.append(behaviorDecl)
    replicateDecl = cxx_writer.Method('replicate', emptyBody, instructionType.makePointer(), 'pu', pure = True, noException = True, const = True)
    instructionElements.append(replicateDecl)
    setparamsParam = cxx_writer.Parameter('bitstring', processor.bitSizes[1].makeRef().makeConst())
    setparamsDecl = cxx_writer.Method('set_params', emptyBody, cxx_writer.voidType, 'pu', [setparamsParam], pure = True, noException = True)
    instructionElements.append(setparamsDecl)

    ########################## TODO: to eliminate, only for statistics ####################
    #returnStatsDecl = cxx_writer.Method('get_count_my_alloc', emptyBody, cxx_writer.uintType, 'pu', virtual = True)
    #instructionElements.append(returnStatsDecl)
    #returnStatsDecl = cxx_writer.Method('get_count_std_alloc', emptyBody, cxx_writer.uintType, 'pu', virtual = True)
    #instructionElements.append(returnStatsDecl)
    ########################################################################################

    if model.startswith('acc'):
        # Now I have to add the code for checking data hazards
        hasCheckHazard = False
        hasWb = False
        for pipeStage in processor.pipes:
            if pipeStage.checkHazard:
                if processor.pipes.index(pipeStage) + 1 < len(processor.pipes):
                    if not processor.pipes[processor.pipes.index(pipeStage) + 1].wb:
                        hasCheckHazard = True
            if pipeStage.wb:
                if processor.pipes.index(pipeStage) - 1 >= 0:
                    if not processor.pipes[processor.pipes.index(pipeStage) - 1].checkHazard:
                        hasWb = True
        if hasCheckHazard:
            for pipeStage in processor.pipes:
                checkHazardDecl = cxx_writer.Method('check_hazard_' + pipeStage.name, emptyBody, cxx_writer.boolType, 'pu', pure = True)
                instructionElements.append(checkHazardDecl)
                lockDecl = cxx_writer.Method('lock_regs_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'pu', pure = True)
                instructionElements.append(lockDecl)
            unlockHazard = False
            for pipeStage in processor.pipes:
                if pipeStage.checkHazard:
                    unlockHazard = True
                if unlockHazard:
                    getUnlockDecl = cxx_writer.Method('get_unlock_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'pu', [unlockQueueParam], pure = True)
                    instructionElements.append(getUnlockDecl)
        # I also have to add the program counter attribute
        fetchPCAttr = cxx_writer.Attribute('fetch_PC', processor.bitSizes[1], 'pu')
        instructionElements.append(fetchPCAttr)
        # and the inInPipeline attribute, specifying if the instruction is currently already
        # in the pipeline or not
        inPipelineAttr = cxx_writer.Attribute('in_pipeline', cxx_writer.boolType, 'pu')
        instructionElements.append(inPipelineAttr)
        toDestroyAttr = cxx_writer.Attribute('to_destroy', cxx_writer.boolType, 'pu')
        instructionElements.append(toDestroyAttr)

    if trace:
        traceStage = processor.pipes[-1]

        # I have to print the value of all the registers in the processor
        printTraceCode = ''
        if model.startswith('acc'):
            # now I have to take all the resources and create a define which
            # renames such resources so that their usage can be transparent
            # to the developer
            for reg in processor.regs:
                printTraceCode += '#define ' + reg.name + ' ' + reg.name + '_' + traceStage.name + '\n'
            for regB in processor.regBanks:
                printTraceCode += '#define ' + regB.name + ' ' + regB.name + '_' + traceStage.name + '\n'
            for alias in processor.aliasRegs:
                printTraceCode += '#define ' + alias.name + ' ' + alias.name + '_' + traceStage.name + '\n'
            for aliasB in processor.aliasRegBanks:
                printTraceCode += '#define ' + aliasB.name + ' ' + aliasB.name + '_' + traceStage.name + '\n'
            printTraceCode += '\n'

        if not combinedTrace:
            if not processor.systemc and not model.startswith('acc') and not model.endswith('AT'):
                printTraceCode += 'std::cerr << \"Simulated time: \" << std::dec << this->total_cycles << " cycles." << std::endl;\n'
            else:
                printTraceCode += 'std::cerr << \"Simulated time: \" << sc_time_stamp().to_double() << \'.\' << std::endl;\n'
        printTraceCode += 'std::cerr << \"Instruction: \" << this->get_name() << \'.\' << std::endl;\n'
        printTraceCode += 'std::cerr << \"Mnemonic: \" << this->get_mnemonic() << std::endl;\n'
        if self.traceRegs:
            bankNames = [i.name for i in processor.regBanks + processor.aliasRegBanks]
            for reg in self.traceRegs:
                if reg.name in bankNames:
                    printTraceCode += 'for (int reg_i = 0; reg_i < ' + str(reg.numRegs) + '; reg_i++) {\n'
                    printTraceCode += 'std::cerr << \"' + reg.name + '[\" << std::dec << reg_i << \"] = \" << std::hex << std::showbase << this->' + reg.name + '[reg_i] << std::endl;\n}\n'
                else:
                    printTraceCode += 'std::cerr << \"' + reg.name + ' = \" << std::hex << std::showbase << this->' + reg.name + ' << std::endl;\n'
        else:
            for reg in processor.regs:
                printTraceCode += 'std::cerr << \"' + reg.name + ' = \" << std::hex << std::showbase << this->' + reg.name + ' << std::endl;\n'
            for regB in processor.regBanks:
                printTraceCode += 'for (int reg_i = 0; reg_i < ' + str(regB.numRegs) + '; reg_i++) {\n'
                printTraceCode += 'std::cerr << \"' + regB.name + '[\" << std::dec << reg_i << \"] = \" << std::hex << std::showbase << this->' + regB.name + '[reg_i] << std::endl;\n}\n'
        printTraceCode += 'std::cerr << std::endl;\n'
        if model.startswith('acc'):
            # now I have to take all the resources and create a define which
            # renames such resources so that their usage can be transparent
            # to the developer
            for reg in processor.regs:
                printTraceCode += '#undef ' + reg.name + '\n'
            for regB in processor.regBanks:
                printTraceCode += '#undef ' + regB.name + '\n'
            for alias in processor.aliasRegs:
                printTraceCode += '#undef ' + alias.name + '\n'
            for aliasB in processor.aliasRegBanks:
                printTraceCode += '#undef ' + aliasB.name + '\n'
        printTraceBody = cxx_writer.Code(printTraceCode)
        printTraceDecl = cxx_writer.Method('print_trace', printTraceBody, cxx_writer.voidType, 'pu')
        instructionElements.append(printTraceDecl)
        # Now we have to print the method for creating the data hazards
        if model.startswith('acc'):
            printBusyRegsDecl = cxx_writer.Method('print_busy_regs', emptyBody, cxx_writer.stringType, 'pu', pure = True)
            instructionElements.append(printBusyRegsDecl)


    getIstructionNameDecl = cxx_writer.Method('get_name', emptyBody, cxx_writer.stringType, 'pu', noException = True, const = True, pure = True)
    instructionElements.append(getIstructionNameDecl)
    getMnemonicDecl = cxx_writer.Method('get_mnemonic', emptyBody, cxx_writer.stringType, 'pu', noException = True, const = True, pure = True)
    instructionElements.append(getMnemonicDecl)
    getIdDecl = cxx_writer.Method('get_id', emptyBody, cxx_writer.uintType, 'pu', noException = True, const = True, pure = True)
    instructionElements.append(getIdDecl)

    # Note how the annul operation stops the execution of the current operation
    annulCode = 'throw annul_exception();'
    annulBody = cxx_writer.Code(annulCode)
    annulBody.addInclude('common/report.hpp')
    annulDecl = cxx_writer.Method('annul', annulBody, cxx_writer.voidType, 'pu', inline = True)
    instructionElements.append(annulDecl)

    if not model.startswith('acc'):
        flushCode = ''
    else:
        flushCode = 'this->flush_pipeline = true;'
    flushBody = cxx_writer.Code(flushCode)
    flushDecl = cxx_writer.Method('flush', flushBody, cxx_writer.voidType, 'pu', inline = True)
    instructionElements.append(flushDecl)

    stallParam = cxx_writer.Parameter('num_cycles', processor.bitSizes[1].makeRef().makeConst())
    if not model.startswith('acc'):
        stallBody = cxx_writer.Code('this->total_instr_cycles += num_cycles;')
    else:
        stallBody = cxx_writer.Code('this->stage_cycles += num_cycles;')
    stallDecl = cxx_writer.Method('stall', stallBody, cxx_writer.voidType, 'pu', [stallParam], inline = True)
    instructionElements.append(stallDecl)
    # we now have to check if there is a non-inline behavior common to all instructions:
    # in this case I declare it here in the base instruction class
    global alreadyDeclared
    alreadyDeclared = []
    global baseBehaviors
    baseBehaviors = []
    if not model.startswith('acc'):
        for instr in self.instructions.values():
            for behaviors in instr.postbehaviors.values() + instr.prebehaviors.values():
                for beh in behaviors:
                    if beh.numUsed == len(self.instructions) and not beh.name in alreadyDeclared:
                        # This behavior is present in all the instructions: I declare it in
                        # the base instruction class
                        alreadyDeclared.append(beh.name)
                        instructionElements.append(beh.getCppOperation(True))
                        baseBehaviors.append(beh.name)
    # Ok, now I add the generic helper methods and operations
    for helpOp in self.helperOps + [self.startup, self.shutdown]:
        if helpOp and not helpOp.name in alreadyDeclared:
            instructionElements.append(helpOp.getCppOperation(True))
    for helpMeth in self.methods:
        if helpMeth:
            instructionElements.append(helpMeth.getCppMethod(model, processor))
    # Now create references to the architectural elements contained in the processor and
    # initialize them through the constructor
    initElements = []
    global baseInstrConstrParams
    baseInstrConstrParams = []
    baseInitElement = 'Instruction('
    from procWriter import resourceType
    if not model.startswith('acc'):
        for reg in processor.regs:
            attribute = cxx_writer.Attribute(reg.name, resourceType[reg.name].makeRef(), 'pro')
            baseInstrConstrParams.append(cxx_writer.Parameter(reg.name, resourceType[reg.name].makeRef()))
            initElements.append(reg.name + '(' + reg.name + ')')
            baseInitElement += reg.name + ', '
            instructionElements.append(attribute)
        for regB in processor.regBanks:
            if (regB.constValue and len(regB.constValue) < regB.numRegs)  or (regB.delay and len(regB.delay) < regB.numRegs):
                curRegBType = resourceType[regB.name].makeRef()
            else:
                curRegBType = resourceType[regB.name]
            attribute = cxx_writer.Attribute(regB.name, curRegBType, 'pro')
            baseInstrConstrParams.append(cxx_writer.Parameter(regB.name, curRegBType))
            initElements.append(regB.name + '(' + regB.name + ')')
            baseInitElement += regB.name + ', '
            instructionElements.append(attribute)
        for alias in processor.aliasRegs:
            attribute = cxx_writer.Attribute(alias.name, resourceType[alias.name].makeRef(), 'pro')
            baseInstrConstrParams.append(cxx_writer.Parameter(alias.name, resourceType[alias.name].makeRef()))
            initElements.append(alias.name + '(' + alias.name + ')')
            baseInitElement += alias.name + ', '
            instructionElements.append(attribute)
        for aliasB in processor.aliasRegBanks:
            attribute = cxx_writer.Attribute(aliasB.name, resourceType[aliasB.name].makePointer(), 'pro')
            baseInstrConstrParams.append(cxx_writer.Parameter(aliasB.name, resourceType[aliasB.name].makePointer()))
            initElements.append(aliasB.name + '(' + aliasB.name + ')')
            baseInitElement += aliasB.name + ', '
            instructionElements.append(attribute)
    else:
        pipeRegisterType = cxx_writer.Type('PipelineRegister', '#include \"registers.hpp\"')
        for reg in processor.regs:
            attribute = cxx_writer.Attribute(reg.name + '_pipe', pipeRegisterType.makeRef(), 'pu')
            baseInstrConstrParams.append(cxx_writer.Parameter(reg.name + '_pipe', pipeRegisterType.makeRef()))
            initElements.append(reg.name + '_pipe(' + reg.name + '_pipe)')
            baseInitElement += reg.name + '_pipe, '
            instructionElements.append(attribute)
        for regB in processor.regBanks:
            attribute = cxx_writer.Attribute(regB.name + '_pipe', pipeRegisterType.makePointer(), 'pu')
            baseInstrConstrParams.append(cxx_writer.Parameter(regB.name + '_pipe', pipeRegisterType.makePointer()))
            initElements.append(regB.name + '_pipe(' + regB.name + '_pipe)')
            baseInitElement += regB.name + '_pipe, '
            instructionElements.append(attribute)
        for pipeStage in processor.pipes:
            for reg in processor.regs:
                attribute = cxx_writer.Attribute(reg.name + '_' + pipeStage.name, resourceType[reg.name].makeRef(), 'pu')
                baseInstrConstrParams.append(cxx_writer.Parameter(reg.name + '_' + pipeStage.name, resourceType[reg.name].makeRef()))
                initElements.append(reg.name + '_' + pipeStage.name + '(' + reg.name + '_' + pipeStage.name + ')')
                baseInitElement += reg.name + '_' + pipeStage.name + ', '
                instructionElements.append(attribute)
            for regB in processor.regBanks:
                if (regB.constValue and len(regB.constValue) < regB.numRegs):
                    curRegBType = resourceType[regB.name].makeRef()
                else:
                    curRegBType = resourceType[regB.name]
                attribute = cxx_writer.Attribute(regB.name + '_' + pipeStage.name, curRegBType, 'pu')
                baseInstrConstrParams.append(cxx_writer.Parameter(regB.name + '_' + pipeStage.name, curRegBType))
                initElements.append(regB.name + '_' + pipeStage.name + '(' + regB.name + '_' + pipeStage.name + ')')
                baseInitElement += regB.name + '_' + pipeStage.name + ', '
                instructionElements.append(attribute)
            for alias in processor.aliasRegs:
                attribute = cxx_writer.Attribute(alias.name + '_' + pipeStage.name, resourceType[alias.name].makeRef(), 'pu')
                baseInstrConstrParams.append(cxx_writer.Parameter(alias.name + '_' + pipeStage.name, resourceType[alias.name].makeRef()))
                initElements.append(alias.name + '_' + pipeStage.name + '(' + alias.name + '_' + pipeStage.name + ')')
                baseInitElement += alias.name + '_' + pipeStage.name + ', '
                instructionElements.append(attribute)
            for aliasB in processor.aliasRegBanks:
                attribute = cxx_writer.Attribute(aliasB.name + '_' + pipeStage.name, resourceType[aliasB.name].makePointer(), 'pu')
                baseInstrConstrParams.append(cxx_writer.Parameter(aliasB.name + '_' + pipeStage.name, resourceType[aliasB.name].makePointer()))
                initElements.append(aliasB.name + '_' + pipeStage.name + '(' + aliasB.name + '_' + pipeStage.name + ')')
                baseInitElement += aliasB.name + '_' + pipeStage.name + ', '
                instructionElements.append(attribute)
    if processor.memory:
        attribute = cxx_writer.Attribute(processor.memory[0], cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"').makeRef(), 'pro')
        baseInstrConstrParams.append(cxx_writer.Parameter(processor.memory[0], cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"').makeRef()))
        initElements.append(processor.memory[0] + '(' + processor.memory[0] + ')')
        baseInitElement += processor.memory[0] + ', '
        instructionElements.append(attribute)
    for tlmPorts in processor.tlmPorts.keys():
        attribute = cxx_writer.Attribute(tlmPorts, cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"').makeRef(), 'pro')
        baseInstrConstrParams.append(cxx_writer.Parameter(tlmPorts, cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"').makeRef()))
        initElements.append(tlmPorts + '(' + tlmPorts + ')')
        baseInitElement += tlmPorts + ', '
        instructionElements.append(attribute)
    for pinPort in processor.pins:
        if not pinPort.inbound:
            if pinPort.systemc:
                pinPortTypeName = 'SC'
            else:
                pinPortTypeName = 'TLM'
            if pinPort.inbound:
                pinPortTypeName += 'InPin_'
            else:
                pinPortTypeName += 'OutPin_'
            pinPortType = cxx_writer.Type(pinPortTypeName + str(pinPort.portWidth), '#include \"externalPins.hpp\"')
            attribute = cxx_writer.Attribute(pinPort.name + '_pin', pinPortType.makeRef(), 'pro')
            baseInstrConstrParams.append(cxx_writer.Parameter(pinPort.name + '_pin', pinPortType.makeRef()))
            initElements.append(pinPort.name + '_pin(' + pinPort.name + '_pin)')
            baseInitElement += pinPort.name + '_pin, '
            instructionElements.append(attribute)
    if trace and not processor.systemc and not model.startswith('acc'):
        attribute = cxx_writer.Attribute('total_cycles', cxx_writer.uintType.makeRef(), 'pro')
        baseInstrConstrParams.append(cxx_writer.Parameter('total_cycles', cxx_writer.uintType.makeRef()))
        initElements.append('total_cycles(total_cycles)')
        baseInitElement += 'total_cycles, '
        instructionElements.append(attribute)
    baseInitElement = baseInitElement[:-2]
    baseInitElement += ')'
    if not model.startswith('acc'):
        instructionElements.append(cxx_writer.Attribute('total_instr_cycles', cxx_writer.uintType, 'pu'))
        constrBody = 'this->total_instr_cycles = 0;'
    else:
        instructionElements.append(cxx_writer.Attribute('flush_pipeline', cxx_writer.boolType, 'pu'))
        instructionElements.append(cxx_writer.Attribute('stage_cycles', cxx_writer.uintType, 'pro'))
        constrBody = 'this->stage_cycles = 0;\nthis->flush_pipeline = false;\nthis->fetch_PC = 0;\nthis->to_destroy = false;\nthis->in_pipeline = false;\n'

    for constant in self.constants:
        instructionElements.append(cxx_writer.Attribute(constant[1], constant[0].makeConst(), 'pro'))
        initElements.append(constant[1] + '(' + str(constant[2]) + ')')

    publicConstr = cxx_writer.Constructor(cxx_writer.Code(constrBody), 'pu', baseInstrConstrParams, initElements)
    instructionBaseType = cxx_writer.Type('InstructionBase', 'modules/instruction.hpp')
    instructionDecl = cxx_writer.ClassDeclaration('Instruction', instructionElements, [instructionBaseType], namespaces = [namespace])
    instructionDecl.addDocString(brief = 'Instruction Class', detail = 'All individual instructions derive from this class.')
    instructionDecl.addConstructor(publicConstr)
    publicDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
    instructionDecl.addDestructor(publicDestr)
    classes.append(instructionDecl)

    # we now have to check all the operation and the behaviors of the instructions and create
    # the classes for each shared non-inline behavior
    global behClass
    behClass = {}
    if not model.startswith('acc'):
        for instr in self.instructions.values():
            for behaviors in instr.postbehaviors.values() + instr.prebehaviors.values():
                for beh in behaviors:
                    if not behClass.has_key(beh.name) and beh.inline and beh.numUsed > 1 and not beh.name in alreadyDeclared:
                        behClass[beh.name] = beh.getCppOpClass(namespace)
                        classes.append(behClass[beh.name])

    #########################################################################
    ############### Now I print the INVALID instruction #####################

    invalidInstrElements = []
    behaviorReturnBody = cxx_writer.Code('return 0;')
    if model.startswith('func'):
        codeString = 'THROW_EXCEPTION(\"Invalid instruction at PC=\" << std::hex << std::showbase << this->' + processor.fetchReg[0] + ' << \'.\''
        if processor.fetchReg[1] < 0:
            codeString += str(processor.fetchReg[1])
        elif processor.fetchReg[1] > 0:
            codeString += '+' + str(processor.fetchReg[1])
        codeString += ');\nreturn 0;'
    else:
        codeString = 'THROW_EXCEPTION(\"Invalid Instruction at PC=\" << std::hex << std::showbase << this->fetch_PC << \'.\');\nreturn 0;'
    behaviorBody = cxx_writer.Code(codeString)
    if model.startswith('acc'):
        for pipeStage in processor.pipes:
            if pipeStage.checkUnknown:
                behaviorDecl = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'pu', [unlockQueueParam])
            else:
                behaviorDecl = cxx_writer.Method('behavior_' + pipeStage.name, behaviorReturnBody, cxx_writer.uintType, 'pu', [unlockQueueParam])
            invalidInstrElements.append(behaviorDecl)
    else:
        behaviorDecl = cxx_writer.Method('behavior', behaviorBody, cxx_writer.uintType, 'pu')
        invalidInstrElements.append(behaviorDecl)
    from procWriter import baseInstrInitElement
    replicateBody = cxx_writer.Code('return new InvalidInstr(' + baseInstrInitElement + ');')
    replicateDecl = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'pu', noException = True, const = True)
    invalidInstrElements.append(replicateDecl)
    setparamsParam = cxx_writer.Parameter('bitstring', processor.bitSizes[1].makeRef().makeConst())
    setparamsDecl = cxx_writer.Method('set_params', emptyBody, cxx_writer.voidType, 'pu', [setparamsParam], noException = True)
    invalidInstrElements.append(setparamsDecl)
    getIstructionNameBody = cxx_writer.Code('return \"InvalidInstruction\";')
    getIstructionNameDecl = cxx_writer.Method('get_name', getIstructionNameBody, cxx_writer.stringType, 'pu', noException = True, const = True)
    invalidInstrElements.append(getIstructionNameDecl)
    getMnemonicBody = cxx_writer.Code('return \"invalid\";')
    getMnemonicDecl = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'pu', noException = True, const = True)
    invalidInstrElements.append(getMnemonicDecl)
    getIdBody = cxx_writer.Code('return ' + str(len(self.instructions)) + ';')
    getIdDecl = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'pu', noException = True, const = True)
    invalidInstrElements.append(getIdDecl)
    if model.startswith('acc'):
        printBusyRegsDecl = cxx_writer.Method('print_busy_regs', cxx_writer.Code('return "";'), cxx_writer.stringType, 'pu')
        invalidInstrElements.append(printBusyRegsDecl)
        if hasCheckHazard:
            returnTrueBody = cxx_writer.Code('return true;')
            for pipeStage in processor.pipes:
                checkHazardDecl = cxx_writer.Method('check_hazard_' + pipeStage.name, returnTrueBody, cxx_writer.boolType, 'pu')
                invalidInstrElements.append(checkHazardDecl)
                lockDecl = cxx_writer.Method('lock_regs_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'pu')
                invalidInstrElements.append(lockDecl)
            unlockHazard = False
            for pipeStage in processor.pipes:
                if pipeStage.checkHazard:
                    unlockHazard = True
                if unlockHazard:
                    getUnlockDecl = cxx_writer.Method('get_unlock_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'pu', [unlockQueueParam])
                    invalidInstrElements.append(getUnlockDecl)
    from procWriter import baseInstrInitElement
    publicConstr = cxx_writer.Constructor(emptyBody, 'pu', baseInstrConstrParams, ['Instruction(' + baseInstrInitElement + ')'])
    invalidInstrDecl = cxx_writer.ClassDeclaration('InvalidInstr', invalidInstrElements, [instructionDecl.getType()], namespaces = [namespace])
    invalidInstrDecl.addConstructor(publicConstr)
    publicDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
    invalidInstrDecl.addDestructor(publicDestr)
    classes.append(invalidInstrDecl)

    #########################################################################
    ############### Now I print the NOP instruction #####################

    if model.startswith('acc'):
        # finally I print the NOP instruction, which I put in the pipeline when flushes occurr
        NOPInstructionElements = []
        for pipeStage in processor.pipes:
            if self.nopBeh.has_key(pipeStage.name):
                defineCode = ''
                for reg in processor.regs:
                    defineCode += '#define ' + reg.name + ' ' + reg.name + '_' + pipeStage.name + '\n'
                for regB in processor.regBanks:
                    defineCode += '#define ' + regB.name + ' ' + regB.name + '_' + pipeStage.name + '\n'
                for alias in processor.aliasRegs:
                    defineCode += '#define ' + alias.name + ' ' + alias.name + '_' + pipeStage.name + '\n'
                for aliasB in processor.aliasRegBanks:
                    defineCode += '#define ' + aliasB.name + ' ' + aliasB.name + '_' + pipeStage.name + '\n'
                undefineCode = ''
                for reg in processor.regs:
                    undefineCode += '#undef ' + reg.name + '\n'
                for regB in processor.regBanks:
                    undefineCode += '#undef ' + regB.name + '\n'
                for alias in processor.aliasRegs:
                    undefineCode += '#undef ' + alias.name + '\n'
                for aliasB in processor.aliasRegBanks:
                    undefineCode += '#undef ' + aliasB.name + '\n'
                behaviorBody = cxx_writer.Code(defineCode + '\n' + self.nopBeh[pipeStage.name] + '\n' + undefineCode)
            else:
                behaviorBody = behaviorReturnBody
            behaviorDecl = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'pu', [unlockQueueParam])
            NOPInstructionElements.append(behaviorDecl)
        from procWriter import baseInstrInitElement
        replicateBody = cxx_writer.Code('return new NOPInstruction(' + baseInstrInitElement + ');')
        replicateDecl = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'pu', noException = True, const = True)
        NOPInstructionElements.append(replicateDecl)
        setparamsParam = cxx_writer.Parameter('bitstring', processor.bitSizes[1].makeRef().makeConst())
        setparamsDecl = cxx_writer.Method('set_params', emptyBody, cxx_writer.voidType, 'pu', [setparamsParam], noException = True)
        NOPInstructionElements.append(setparamsDecl)
        getIstructionNameBody = cxx_writer.Code('return \"NOPInstruction\";')
        getIstructionNameDecl = cxx_writer.Method('get_name', getIstructionNameBody, cxx_writer.stringType, 'pu', noException = True, const = True)
        NOPInstructionElements.append(getIstructionNameDecl)
        getMnemonicBody = cxx_writer.Code('return \"nop\";')
        getMnemonicDecl = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'pu', noException = True, const = True)
        NOPInstructionElements.append(getMnemonicDecl)
        getIdBody = cxx_writer.Code('return (unsigned)-1;')
        getIdDecl = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'pu', noException = True, const = True)
        NOPInstructionElements.append(getIdDecl)

        printBusyRegsDecl = cxx_writer.Method('print_busy_regs', cxx_writer.Code('return "";'), cxx_writer.stringType, 'pu')
        NOPInstructionElements.append(printBusyRegsDecl)

        if hasCheckHazard:
            returnTrueBody = cxx_writer.Code('return true;')
            for pipeStage in processor.pipes:
                checkHazardDecl = cxx_writer.Method('check_hazard_' + pipeStage.name, returnTrueBody, cxx_writer.boolType, 'pu')
                NOPInstructionElements.append(checkHazardDecl)
                lockDecl = cxx_writer.Method('lock_regs_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'pu')
                NOPInstructionElements.append(lockDecl)
            unlockHazard = False
            for pipeStage in processor.pipes:
                if pipeStage.checkHazard:
                    unlockHazard = True
                if unlockHazard:
                    getUnlockDecl = cxx_writer.Method('get_unlock_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'pu', [unlockQueueParam])
                    NOPInstructionElements.append(getUnlockDecl)
        from procWriter import baseInstrInitElement
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu', baseInstrConstrParams, ['Instruction(' + baseInstrInitElement + ')'])
        NOPInstructionClass = cxx_writer.ClassDeclaration('NOPInstruction', NOPInstructionElements, [instructionDecl.getType()], namespaces = [namespace])
        NOPInstructionClass.addConstructor(publicConstr)
        publicDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
        NOPInstructionClass.addDestructor(publicDestr)
        classes.append(NOPInstructionClass)
    # Now I go over all the other instructions and I declare them
    for instr in self.instructions.values():
        classes += instr.getCPPClass(model, processor, trace, combinedTrace, namespace)
    return classes

def getCPPTests(self, processor, modelType, trace, combinedTrace, namespace):
    if not processor.memory:
        return None
    # for each instruction I print the test: I do have to add some custom
    # code at the beginning in order to being able to access the private
    # part of the instructions
    tests = []
    for instr in self.instructions.values():
        tests += instr.getCPPTest(processor, modelType, trace, combinedTrace, namespace)
    return tests
