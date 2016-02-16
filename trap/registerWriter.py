################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     registerWriter.py
# @brief    This file is part of the TRAP processor generator module.
# @details
# @author   Luca Fossati
# @date     2008-2013 Luca Fossati
#           2015-2016 Lillian Tadros (Technische Universitaet Dortmund)
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

# List of the operators which are redefined inside the register classes to enable
# direct access to the registers themselves
assignmentOps = ['=', '+=', '-=', '*=', '/=', '|=', '&=', '^=', '<<=', '>>=']
binaryOps = ['+', '-', '*', '/', '|', '&', '^', '<<', '>>']
unaryOps = ['~']
comparisonOps = ['<', '>', '<=', '>=', '==', '!=']

# Helper variables use during register type computation
regMaxType = None

import cxx_writer

def getRegistersBitfields(self):
    addedKeys = []
    defineCode = ''
    numKeys = 0
    for reg in self.regs + self.regBanks:
        for key in reg.bitMask.keys():
            if not key in addedKeys:
                defineCode += '#define key_' + key + ' ' + str(numKeys) + '\n'
                addedKeys.append(key)
                numKeys += 1
    return cxx_writer.Code(defineCode + '\n\n')

def getCPPRegClass(self, model, regType, namespace):
    """returns the class implementing the current register; I have to
    define all the operators;"""
    emptyBody = cxx_writer.Code('')
    regWidthType = regMaxType

    registerType = cxx_writer.Type('Register')
    InnerFieldType = cxx_writer.Type('InnerField')
    registerElements = []
    normalRegType = regType.makeNormal()

    # First of all I determine if there is the need to create a const element
    constReg = False
    if self.constValue != None and type(self.constValue) != type({}):
        assignValueItem = str(self.constValue)
        readValueItem = str(self.constValue)
        constReg = True
    elif model.startswith('acc') or type(self.delay) == type({}) or self.delay == 0:
        assignValueItem = 'this->value'
        readValueItem = 'this->value'
    else:
        assignValueItem = 'this->update_slot[' + str(self.delay - 1) + '] = true;\nthis->values[' + str(self.delay - 1) + ']'
        readValueItem = 'this->value'

    if constReg and model.startswith('acc'):
        isLockedBody = cxx_writer.Code('return false;')
        isLockedMethod = cxx_writer.Method('is_locked', isLockedBody, cxx_writer.boolType, 'pu', noException = True)
        registerElements.append(isLockedMethod)


    ####################### Lets declare the operators used to access the register fields ##############
    if self.bitMask:
        codeOperatorBody = 'switch(bitfield) {\n'
        for key in self.bitMask.keys():
            codeOperatorBody += 'case key_' + key + ': {\nreturn this->field_' + key + ';\nbreak;\n}\n'
        codeOperatorBody += 'default: {\nreturn this->field_empty;\nbreak;\n}\n}\n'
    else:
        codeOperatorBody = 'return this->field_empty;'
    operatorBody = cxx_writer.Code(codeOperatorBody)
    operatorParam = [cxx_writer.Parameter('bitfield', cxx_writer.intType)]
    operatorDecl = cxx_writer.MemberOperator('[]', operatorBody, InnerFieldType.makeRef(), 'pu', operatorParam, noException = True, inline = True)
    registerElements.append(operatorDecl)

    ################ Methods used for the update of delayed registers ######################
    if not model.startswith('acc') and type(self.delay) != type({}) and self.delay > 0:
        clockCycleCode = """if (this->update_slot[0]) {
                    this->value = this->values[0];
                    this->update_slot[0] = false;
                }
                """
        for i in range(1, self.delay):
            clockCycleCode += """if (this->update_slot[""" + str(i) + """]) {
                        this->values[""" + str(i - 1) + """] = this->values[""" + str(i) + """];
                        this->update_slot[""" + str(i) + """] = false;
                        this->update_slot[""" + str(i - 1) + """] = true;
                    }
                    """
        clockCycleBody = cxx_writer.Code(clockCycleCode)
        clockCycleMethod = cxx_writer.Method('clock_cycle', clockCycleBody, cxx_writer.voidType, 'pu', inline = True, noException = True)
        registerElements.append(clockCycleMethod)
    elif model.startswith('acc'):
        if self.constValue == None or type(self.constValue) == type({}):
            forceValueBody = cxx_writer.Code('this->value = value;')
        else:
            forceValueBody = cxx_writer.Code('')
        forceValueParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
        forceValueMethod = cxx_writer.Method('force_value', forceValueBody, cxx_writer.voidType, 'pu', forceValueParam, noException = True)
        registerElements.append(forceValueMethod)
    if self.constValue == None or type(self.constValue) == type({}):
        immediateWriteCode = 'this->value = value;\n'
        if not model.startswith('acc') and type(self.delay) != type({}) and self.delay > 0:
            for i in range(0, self.delay):
                immediateWriteCode += 'this->update_slot[' + str(i) + '] = false;\n'
        elif model.startswith('acc'):
            immediateWriteCode += '*(this->has_to_propagate) = true;\nthis->time_stamp = sc_time_stamp();\n'
    else:
        immediateWriteCode = ''
    immediateWriteBody = cxx_writer.Code(immediateWriteCode)
    immediateWriteParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
    immediateWriteMethod = cxx_writer.Method('immediate_write', immediateWriteBody, cxx_writer.voidType, 'pu', immediateWriteParam, noException = True)
    registerElements.append(immediateWriteMethod)

    if not model.startswith('acc') and type(self.delay) != type({}) and self.delay > 0:
        readNewValueCode = ''
        delays = range(0, self.delay)
        delays.reverse()
        if self.offset:
            for i in delays:
                readNewValueCode += """if (this->update_slot[""" + str(i) + """]) {
                            return this->values[""" + str(i) + """] + """ + str(self.offset) + """;
                        }
                        """
            readNewValueCode += 'return this->value + ' + str(self.offset) + ';\n'
        else:
            for i in delays:
                readNewValueCode += """if (this->update_slot[""" + str(i) + """]) {
                            return this->values[""" + str(i) + """];
                        }
                        """
            readNewValueCode += 'return this->value;\n'
    else:
        if not model.startswith('acc') and self.offset:
            readNewValueCode = 'return this->value + ' + str(self.offset) + ';\n'
        else:
            readNewValueCode = 'return this->value;\n'
        #try:
            #for i in range(0, self.delay):
                #immediateWriteCode += 'this->update_slot[' + str(i) + '] = false;\n'
        #except TypeError:
            #pass
    readNewValueBody = cxx_writer.Code(readNewValueCode)
    readNewValueMethod = cxx_writer.Method('read_new_value', readNewValueBody, regMaxType, 'pu', noException = True)
    registerElements.append(readNewValueMethod)

    #################### Lets declare the normal operators (implementation of the pure operators of the base class) ###########
    for i in unaryOps:
        if self.offset and not model.startswith('acc'):
            operatorBody = cxx_writer.Code('return ' + i + '(' + readValueItem + ' + ' + str(self.offset) + ');')
        else:
            operatorBody = cxx_writer.Code('return ' + i + '(' + readValueItem + ');')
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', noException = True)
        registerElements.append(operatorDecl)
    # Now I have the three versions of the operators, depending whether they take
    # in input the integer value, the specific register or the base one
    # INTEGER
    for i in assignmentOps:
        if self.constValue != None and type(self.constValue) != type({}):
            operatorBody = cxx_writer.Code('return *this;')
        elif model.startswith('acc'):
            operatorBody = cxx_writer.Code(assignValueItem + ' ' + i + ' other;\n*(this->has_to_propagate) = true;\nthis->time_stamp = sc_time_stamp();\nreturn *this;')
        else:
            operatorBody = cxx_writer.Code(assignValueItem + ' ' + i + ' other;\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regType.makeRef(), 'pu', [operatorParam], noException = True)
        registerElements.append(operatorDecl)
    # SPECIFIC REGISTER
    for i in binaryOps:
        if self.offset and not model.startswith('acc'):
            operatorBody = cxx_writer.Code('return ((' + readValueItem + '  + ' + str(self.offset) + ') ' + i + ' (other.value + ' + str(self.offset) + '));')
        else:
            operatorBody = cxx_writer.Code('return (' + readValueItem + ' ' + i + ' other.value);')
        operatorParam = cxx_writer.Parameter('other', regType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True, noException = True)
        registerElements.append(operatorDecl)
    for i in comparisonOps:
        if self.offset and not model.startswith('acc'):
            operatorBody = cxx_writer.Code('return ((' + readValueItem + ' + ' + str(self.offset) + ') ' + i + ' (other.value + ' + str(self.offset) + '));')
        else:
            operatorBody = cxx_writer.Code('return (' + readValueItem + ' ' + i + ' other.value);')
        operatorParam = cxx_writer.Parameter('other', regType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True, noException = True)
        registerElements.append(operatorDecl)
    for i in assignmentOps:
        if self.constValue != None and type(self.constValue) != type({}):
            operatorBody = cxx_writer.Code('return *this;')
        elif model.startswith('acc'):
            operatorBody = cxx_writer.Code(assignValueItem + ' ' + i + ' other;\n*(this->has_to_propagate) = true;\nthis->time_stamp = sc_time_stamp();\nreturn *this;')
        else:
            operatorBody = cxx_writer.Code(assignValueItem + ' ' + i + ' other;\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', regType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regType.makeRef(), 'pu', [operatorParam], noException = True)
        registerElements.append(operatorDecl)
    # GENERIC REGISTER: this case is look more complicated; actually I simply used the
    # operators of parameter other
    for i in binaryOps:
        if self.offset and not model.startswith('acc'):
            operatorBody = cxx_writer.Code('return ((' + readValueItem + '  + ' + str(self.offset) + ') ' + i + ' other);')
        else:
            operatorBody = cxx_writer.Code('return (' + readValueItem + ' ' + i + ' other);')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True, noException = True)
        registerElements.append(operatorDecl)
    for i in comparisonOps:
        if self.offset and not model.startswith('acc'):
            operatorBody = cxx_writer.Code('return ((' + readValueItem + '  + ' + str(self.offset) + ') ' + i + ' other);')
        else:
            operatorBody = cxx_writer.Code('return (' + readValueItem + ' ' + i + ' other);')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True, noException = True)
        registerElements.append(operatorDecl)
    for i in assignmentOps:
        if self.constValue != None and type(self.constValue) != type({}):
            operatorBody = cxx_writer.Code('return *this;')
        elif model.startswith('acc'):
            operatorBody = cxx_writer.Code(assignValueItem + ' ' + i + ' other;\n*(this->has_to_propagate) = true;\nthis->time_stamp = sc_time_stamp();\nreturn *this;')
        else:
            operatorBody = cxx_writer.Code(assignValueItem + ' ' + i + ' other;\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regType.makeRef(), 'pu', [operatorParam], noException = True)
        registerElements.append(operatorDecl)
    # Scalar value cast operator
    if self.offset and not model.startswith('acc'):
        operatorBody = cxx_writer.Code('return (' + readValueItem + '  + ' + str(self.offset) + ');')
    else:
        operatorBody = cxx_writer.Code('return ' + readValueItem + ';')
    operatorIntDecl = cxx_writer.MemberOperator(str(regMaxType), operatorBody, cxx_writer.Type(''), 'pu', const = True, noException = True, inline = True)
    registerElements.append(operatorIntDecl)

    # Constructors
    fieldInit = []
    if not model.startswith('acc') and type(self.delay) != type({}) and self.delay > 0:
        for field in self.bitMask.keys():
            fieldInit.append('field_' + field + '(this->value, this->values[' + str(self.delay - 1) + '], this->update_slot[' + str(self.delay - 1) + '])')
    elif model.startswith('acc'):
        for field in self.bitMask.keys():
            fieldInit.append('field_' + field + '(this->value, this->time_stamp, this->has_to_propagate)')
    else:
        for field in self.bitMask.keys():
            fieldInit.append('field_' + field + '(this->value)')
    if self.constValue != None and type(self.constValue) != type({}):
        constructorCode = 'this->value = ' + readValueItem + ';\n'
    else:
        constructorCode = 'this->value = 0;\n'
    if not model.startswith('acc') and type(self.delay) != type({}) and self.delay != 0:
        for i in range(0, self.delay):
            constructorCode += 'this->update_slot[' + str(i) + '] = false;\n'
    constructorBody = cxx_writer.Code(constructorCode)
    publicMainClassEmptyConstr = cxx_writer.Constructor(constructorBody, 'pu', initList = fieldInit)

    # Stream Operators
    outStreamType = cxx_writer.Type('std::ostream', 'ostream')
    code = 'stream << std::hex << std::showbase << ' + readValueItem + ' << std::dec;\nreturn stream;'
    operatorBody = cxx_writer.Code(code)
    operatorParam = cxx_writer.Parameter('stream', outStreamType.makeRef())
    operatorDecl = cxx_writer.MemberOperator('<<', operatorBody, outStreamType.makeRef(), 'pu', [operatorParam], const = True, noException = True)
    registerElements.append(operatorDecl)

    # Attributes and inner classes declarations
    attrs = []
    innerClasses = []
    for field, length in self.bitMask.items():
        InnerFieldElems = []
        # Here I have to define the classes that represent the different fields
        negatedMask = ''
        mask = ''
        fieldLenMask = ''
        onesMask = ''.join(['1' for i in range(0, self.bitWidth)])
        for i in range(0, self.bitWidth):
            if (i >= length[0]) and (i <= length[1]):
                negatedMask = '0' + negatedMask
                mask = '1' + mask
            else:
                negatedMask = '1' + negatedMask
                mask = '0' + mask
            if i <= (length[1] - length[0]):
                fieldLenMask = '1' + fieldLenMask
            else:
                fieldLenMask = '0' + fieldLenMask
        operatorCode = ''
        if not model.startswith('acc') and type(self.delay) != type({}) and self.delay > 0:
            operatorCode += 'this->last_valid = true;\n'
            if type(readValueItem) != type(0):
                if onesMask != negatedMask:
                    operatorCode += 'this->value_last = (this->value & ' + hex(int(negatedMask, 2)) + ');\n'
                operatorCode += 'this->value_last |= '
                if length[0] > 0:
                    operatorCode += '((other & ' + hex(int(fieldLenMask, 2)) + ') << ' + str(length[0]) + ');\n'
                else:
                    operatorCode += 'other;\n'
        else:
            if type(readValueItem) != type(0):
                if onesMask != negatedMask:
                    operatorCode += 'this->value &= ' + hex(int(negatedMask, 2)) + ';\n'
                operatorCode += 'this->value |= '
                if length[0] > 0:
                    operatorCode += '((other & ' + hex(int(fieldLenMask, 2)) + ') << ' + str(length[0]) + ');\n'
                else:
                    operatorCode += 'other;\n'
        if model.startswith('acc'):
            operatorCode += '*(this->has_to_propagate) = true;\nthis->time_stamp = sc_time_stamp();\n'
        operatorCode += 'return *this;'
        operatorBody = cxx_writer.Code(operatorCode)
        operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
        operatorEqualDecl = cxx_writer.MemberOperator('=', operatorBody, cxx_writer.Type('InnerField').makeRef(), 'pu', [operatorParam], noException = True)
        InnerFieldElems.append(operatorEqualDecl)
        operatorCode = 'return (this->value & ' + hex(int(mask, 2)) + ')'
        if length[0] > 0:
            operatorCode += ' >> ' + str(length[0])
        operatorCode += ';'
        operatorBody = cxx_writer.Code(operatorCode)
        operatorIntDecl = cxx_writer.MemberOperator(str(regMaxType), operatorBody, cxx_writer.Type(''), 'pu', const = True, noException = True, inline = True)
        InnerFieldElems.append(operatorIntDecl)
        fieldAttribute = cxx_writer.Attribute('value', regMaxType.makeRef(), 'pri')
        InnerFieldElems.append(fieldAttribute)
        if not model.startswith('acc') and type(self.delay) != type({}) and self.delay > 0:
            fieldAttribute = cxx_writer.Attribute('value_last', regMaxType.makeRef(), 'pri')
            InnerFieldElems.append(fieldAttribute)
            fieldAttribute = cxx_writer.Attribute('last_valid', cxx_writer.boolType.makeRef(), 'pri')
            InnerFieldElems.append(fieldAttribute)
        elif model.startswith('acc'):
            timeStampAttribute = cxx_writer.Attribute('time_stamp', cxx_writer.sc_timeType.makeRef(), 'pri')
            InnerFieldElems.append(timeStampAttribute)
            propagateAttribute = cxx_writer.Attribute('has_to_propagate', cxx_writer.boolType.makePointer().makeRef(), 'pri')
            InnerFieldElems.append(propagateAttribute)
        constructorParams = [cxx_writer.Parameter('value', regMaxType.makeRef())]
        constructorInit = ['value(value)']
        if not model.startswith('acc') and type(self.delay) != type({}) and self.delay > 0:
            constructorParams.append(cxx_writer.Parameter('last_value', regMaxType.makeRef()))
            constructorInit.append('value_last(last_value)')
            constructorParams.append(cxx_writer.Parameter('last_valid', cxx_writer.boolType.makeRef()))
            constructorInit.append('last_valid(last_valid)')
        elif model.startswith('acc'):
            constructorParams.append(cxx_writer.Parameter('time_stamp', cxx_writer.sc_timeType.makeRef()))
            constructorInit.append('time_stamp(time_stamp)')
            constructorParams.append(cxx_writer.Parameter('has_to_propagate', cxx_writer.boolType.makePointer().makeRef()))
            constructorInit.append('has_to_propagate(has_to_propagate)')
        publicConstr = cxx_writer.Constructor(cxx_writer.Code(''), 'pu', constructorParams, constructorInit)
        InnerFieldClass = cxx_writer.ClassDeclaration('InnerField_' + field, InnerFieldElems, [cxx_writer.Type('InnerField')])
        InnerFieldClass.addConstructor(publicConstr)
        publicDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
        InnerFieldClass.addDestructor(publicDestr)
        innerClasses.append(InnerFieldClass)
        fieldAttribute = cxx_writer.Attribute('field_' + field, InnerFieldClass.getType(), 'pri')
        attrs.append(fieldAttribute)
    operatorBody = cxx_writer.Code('return *this;')
    operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
    operatorEqualDecl = cxx_writer.MemberOperator('=', operatorBody, cxx_writer.Type('InnerField').makeRef(), 'pu', [operatorParam], noException = True)
    operatorBody = cxx_writer.Code('return 0;')
    operatorIntDecl = cxx_writer.MemberOperator(str(regMaxType), operatorBody, cxx_writer.Type(''), 'pu', const = True, noException = True, inline = True)
    publicConstr = cxx_writer.Constructor(cxx_writer.Code(''), 'pu')
    InnerFieldClass = cxx_writer.ClassDeclaration('InnerField_Empty', [operatorEqualDecl, operatorIntDecl], [cxx_writer.Type('InnerField')])
    InnerFieldClass.addConstructor(publicConstr)
    publicDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
    InnerFieldClass.addDestructor(publicDestr)
    innerClasses.append(InnerFieldClass)
    fieldAttribute = cxx_writer.Attribute('field_empty', InnerFieldClass.getType(), 'pri')
    attrs.append(fieldAttribute)
    valueAttribute = cxx_writer.Attribute('value', regWidthType, 'pri')
    attrs.append(valueAttribute)
    if self.offset and not model.startswith('acc'):
        offsetAttribute = cxx_writer.Attribute('offset', cxx_writer.intType, 'pri')
        attrs.append(offsetAttribute)
    if not model.startswith('acc') and type(self.delay) != type({}) and self.delay != 0:
        delaySlotAttribute = cxx_writer.Attribute('values[' + str(self.delay) + ']', regWidthType, 'pri')
        attrs.append(delaySlotAttribute)
        updateSlotAttribute = cxx_writer.Attribute('update_slot[' + str(self.delay) + ']', cxx_writer.boolType, 'pri')
        attrs.append(updateSlotAttribute)
    registerElements = attrs + registerElements

    registerDecl = cxx_writer.ClassDeclaration(regType.name, registerElements, [registerType], namespaces = [namespace])
    registerDecl.addConstructor(publicMainClassEmptyConstr)
    for i in innerClasses:
        registerDecl.addInnerClass(i)
    return registerDecl

def getCPPRegBankClass(self, model, regType, namespace):
    """returns the class implementing the single register of
    the register bank"""
    return getCPPRegClass(self, model, regType, namespace)

def getCPPPipelineReg(self, trace, combinedTrace, namespace):
    """This method returns the pipeline registers, which is a special registers
    containing both the value of the registres itself and all the pipeline
    latches.
    I also contains special methods to propagate registers values in the
    pipeline.
    Note that there are different kinds of such registers, one for the
    normal latched registers and one for the registers which are immediately
    visible to all the other stages (e.g. for LEON they are the PC and NPC
    registers)."""

    # Lets start with the creation of the latched registers: they have exactly
    # the same methods of the base registers
    pipelineRegClasses = []
    registerElements = []
    registerType = cxx_writer.Type('Register')

    ################ Constructor: it initializes the internal registers ######################
    fullConstructorParams = []
    innerConstrInit = ''
    fullConstructorCode = ''
    emptyConstructorCode = ''
    i = 0
    for pipeStage in self.pipes:
        fullConstructorCode += 'this->reg_stage[' + str(i) + '] = reg_' + pipeStage.name + ';\n'
        fullConstructorCode += 'this->reg_stage[' + str(i) + ']->has_to_propagate = &(this->has_to_propagate);\n'
        emptyConstructorCode += 'this->reg_stage[' + str(i) + '] = NULL;\n'
        fullConstructorParams.append(cxx_writer.Parameter('reg_' + pipeStage.name, registerType.makePointer()))
        innerConstrInit += 'reg_' + pipeStage.name + ', '
        i += 1
    fullConstructorCode += 'this->reg_all = reg_all;\n'
    fullConstructorCode += 'this->reg_all->has_to_propagate = &(this->has_to_propagate);\n'
    fullConstructorCode += 'this->has_to_propagate = false;\n'
    emptyConstructorCode += 'this->reg_all = NULL;\n'
    emptyConstructorCode += 'this->has_to_propagate = false;\n'
    innerConstrInit += 'reg_all'
    fullConstructorParams.append(cxx_writer.Parameter('reg_all', registerType.makePointer()))
    constructorBody = cxx_writer.Code(fullConstructorCode)
    publicFullConstr = cxx_writer.Constructor(constructorBody, 'pu', fullConstructorParams)
    constructorBody = cxx_writer.Code(emptyConstructorCode)
    publicEmptyConstr = cxx_writer.Constructor(constructorBody, 'pu')

    stagesRegsAttribute = cxx_writer.Attribute('reg_stage[' + str(len(self.pipes)) + ']', registerType.makePointer(), 'pro')
    registerElements.append(stagesRegsAttribute)
    generalRegAttribute = cxx_writer.Attribute('reg_all', registerType.makePointer(), 'pro')
    registerElements.append(generalRegAttribute)
    propagateAttribute = cxx_writer.Attribute('has_to_propagate', cxx_writer.boolType, 'pu')
    registerElements.append(propagateAttribute)

    ################ Lock and Unlock methods used for hazards detection ######################
    # For the general register they have a particular behavior: they have to lock all versions of the
    # registers, unlock unlocks all of them; concerning is_locked method, it returns the status
    # of the general register. Note that is_locked takes an additional parameter: if specified
    # the locked status of the corresponding stage register (used for bypass operations, where
    # we do not wait for the WB, but we take the value from a pipeline register)

    lockBody = cxx_writer.Code("""for (int i = 0; i < """ + str(len(self.pipes)) + """; i++) {
        this->reg_stage[i]->lock();
    }
    this->reg_all->lock();""")
    lockMethod = cxx_writer.Method('lock', lockBody, cxx_writer.voidType, 'pu', virtual = True, noException = True)
    registerElements.append(lockMethod)
    unlockBody = cxx_writer.Code("""for (int i = 0; i < """ + str(len(self.pipes)) + """; i++) {
        this->reg_stage[i]->unlock();
    }
    this->reg_all->unlock();""")
    unlockMethod = cxx_writer.Method('unlock', unlockBody, cxx_writer.voidType, 'pu', virtual = True, noException = True)
    registerElements.append(unlockMethod)
    latencyParam = cxx_writer.Parameter('wb_latency', cxx_writer.intType)
    unlockMethod = cxx_writer.Method('unlock', unlockBody, cxx_writer.voidType, 'pu', [latencyParam], virtual = True, noException = True)
    registerElements.append(unlockMethod)
    isLockedBody = cxx_writer.Code('return this->reg_all->is_locked();')
    isLockedMethod = cxx_writer.Method('is_locked', isLockedBody, cxx_writer.boolType, 'pu', noException = True)
    registerElements.append(isLockedMethod)
    stageIdParam = cxx_writer.Parameter('stage_id', cxx_writer.intType)
    isLockedBody = cxx_writer.Code('return this->reg_stage[stage_id]->is_locked();')
    isLockedMethod = cxx_writer.Method('is_locked', isLockedBody, cxx_writer.boolType, 'pu', [stageIdParam], noException = True)
    registerElements.append(isLockedMethod)

    # Now some virtual methods inherited from the base class
    forceValueBody = cxx_writer.Code('this->reg_all->force_value(value);')
    forceValueParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
    forceValueMethod = cxx_writer.Method('force_value', forceValueBody, cxx_writer.voidType, 'pu', forceValueParam, noException = True)
    registerElements.append(forceValueMethod)

    writeAllCode = '*(this->reg_all) = value;\n'
    writeAllCode += 'for (int i = 0; i < ' + str(len(self.pipes)) + '; i++) {\n'
    writeAllCode += '*(this->reg_stage[i]) = value;\n}\n'
    writeAllBody = cxx_writer.Code(writeAllCode)
    writeAllParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
    writeAllMethod = cxx_writer.Method('write_all', writeAllBody, cxx_writer.voidType, 'pu', writeAllParam, noException = True)
    registerElements.append(writeAllMethod)
    immediateWriteBody = cxx_writer.Code('this->reg_all->immediate_write(value);')
    immediateWriteParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
    immediateWriteMethod = cxx_writer.Method('immediate_write', immediateWriteBody, cxx_writer.voidType, 'pu', immediateWriteParam, noException = True)
    registerElements.append(immediateWriteMethod)
    readNewValueBody = cxx_writer.Code('return this->reg_all->read_new_value();')
    readNewValueMethod = cxx_writer.Method('read_new_value', readNewValueBody, regMaxType, 'pu', noException = True)
    registerElements.append(readNewValueMethod)

    # Propagate method, used to move register values from one stage to the other; the default implementation of such
    # method proceeded from the last stage to the first one: wb copied in REGS_all, exec in wb, .... REGS_all in fetch
    regReadUpdate = ''
    firstStages = 0
    for pipeStage in self.pipes:
        regReadUpdate += 'if (this->reg_all->time_stamp > this->reg_stage[' + str(firstStages) + ']->time_stamp) {\n'
        ######
        if trace and not combinedTrace:
            regReadUpdate += 'std::cerr << "Propagating stage all into stage ' + str(firstStages) + '" << \'.\' << std::endl;\n'
        ######
        regReadUpdate += 'has_changes = true;\n'
        regReadUpdate += 'this->reg_stage[' + str(firstStages) + ']->time_stamp = this->reg_all->time_stamp;\n'
        regReadUpdate += 'this->reg_stage[' + str(firstStages) + ']->force_value(*(this->reg_all));\n}\n'
        if pipeStage.checkHazard:
            break
        else:
            firstStages += 1
    propagateCode = 'bool has_changes = false;\n'
    #propagateCode += 'if (!this->has_to_propagate) {\nreturn;\n}\n'
    propagateCode += 'if (this->reg_stage[' + str(len(self.pipes) - 1) + ']->time_stamp > this->reg_all->time_stamp) {\n'
    ######
    if trace and not combinedTrace:
        propagateCode += 'std::cerr << "Propagating stage ' + str(len(self.pipes) - 1) + ' into reg_all." << std::endl;\n'
    ######
    propagateCode += 'has_changes = true;\n'
    propagateCode += 'this->reg_all->time_stamp = this->reg_stage[' + str(len(self.pipes) - 1) + ']->time_stamp;\n'
    propagateCode += 'this->reg_all->force_value(*(this->reg_stage[' + str(len(self.pipes) - 1) + ']));\n}\n'
    propagateCode += 'for (int i = ' + str(len(self.pipes) - 2) + '; i >= ' + str(firstStages) + '; i--) {\n'
    propagateCode += 'if (this->reg_stage[i]->time_stamp > this->reg_stage[i + 1]->time_stamp) {\n'
    ######
    if trace and not combinedTrace:
        propagateCode += 'std::cerr << "Propagating stage " << std::dec << i << " into stage " << std::dec << i + 1 << \'.\' << std::endl;\n'
    ######
    propagateCode += 'has_changes = true;\n'
    propagateCode += 'this->reg_stage[i + 1]->time_stamp = this->reg_stage[i]->time_stamp;\n'
    propagateCode += 'this->reg_stage[i + 1]->force_value(*(this->reg_stage[i]));\n}\n'
    propagateCode += '}\n'
    propagateCode += regReadUpdate
    propagateCode += 'this->has_to_propagate = has_changes;\n'
    propagateBody = cxx_writer.Code(propagateCode)
    propagateMethod = cxx_writer.Method('propagate', propagateBody, cxx_writer.voidType, 'pu', noException = True, virtual = True)
    registerElements.append(propagateMethod)

    # Now here I have to define the method to get/set the pointer to the various
    # registers
    regIndexParam = cxx_writer.Parameter('reg_idx', cxx_writer.intType, initValue = '-1')
    getRegisterBody = cxx_writer.Code("""if (reg_idx < 0) {
        return this->reg_all;
    } else {
        return this->reg_stage[reg_idx];
    }""")
    getRegisterMethod = cxx_writer.Method('get_register', getRegisterBody, registerType.makePointer(), 'pu', [regIndexParam], inline = True, noException = True)
    registerElements.append(getRegisterMethod)
    regPointerParam = cxx_writer.Parameter('reg_ptr', registerType.makePointer())
    getRegisterBody = cxx_writer.Code("""if (reg_idx < 0) {
        if (this->reg_all != NULL) {
            THROW_EXCEPTION("Cannot initialize main register after the pipeline register initialization has completed.");
        }
        this->reg_all = reg_ptr;
        this->reg_all->has_to_propagate = &(this->has_to_propagate);
    } else {
        if (this->reg_stage[reg_idx] != NULL) {
            THROW_EXCEPTION("Cannot initialize register " << reg_idx << " after the pipeline register initialization has completed.");
        }
        this->reg_stage[reg_idx] = reg_ptr;
        this->reg_stage[reg_idx]->has_to_propagate = &(this->has_to_propagate);
    }""")
    setRegisterMethod = cxx_writer.Method('set_register', getRegisterBody, cxx_writer.voidType, 'pu', [regPointerParam, regIndexParam])
    registerElements.append(setRegisterMethod)

    # Simple base methods used to access the general register operators: we simply perform a forward towards
    # the corresponding operator of the general register (reg_all)
    InnerFieldType = cxx_writer.Type('InnerField')
    operatorParam = cxx_writer.Parameter('bitfield', cxx_writer.intType)
    operatorBody = cxx_writer.Code('return (*(this->reg_all))[bitfield];')
    operatorDecl = cxx_writer.MemberOperator('[]', operatorBody, InnerFieldType.makeRef(), 'pu', [operatorParam], noException = True)
    registerElements.append(operatorDecl)
    for i in unaryOps:
        operatorBody = cxx_writer.Code('return ' + i + '(*(this->reg_all));')
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', noException = True)
        registerElements.append(operatorDecl)
    for i in binaryOps:
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorBody = cxx_writer.Code('return (*(this->reg_all)) ' + i + ' other;')
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True, noException = True)
        registerElements.append(operatorDecl)
    for i in comparisonOps:
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorBody = cxx_writer.Code('return (*(this->reg_all)) ' + i + ' other;')
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True, noException = True)
        registerElements.append(operatorDecl)

    pureDeclTypes = [regMaxType, registerType]
    for pureDecls in pureDeclTypes:
        for i in assignmentOps:
            operatorParam = cxx_writer.Parameter('other', pureDecls.makeRef().makeConst())
            operatorBody = cxx_writer.Code('return (*(this->reg_all)) ' + i + ' other;')
            operatorDecl = cxx_writer.MemberOperator(i, operatorBody, registerType.makeRef(), 'pu', [operatorParam], noException = True)
            registerElements.append(operatorDecl)
    # Stream Operators
    outStreamType = cxx_writer.Type('std::ostream', 'ostream')
    operatorParam = cxx_writer.Parameter('stream', outStreamType.makeRef())
    operatorBody = cxx_writer.Code('return stream << (*(this->reg_all));')
    operatorDecl = cxx_writer.MemberOperator('<<', operatorBody, outStreamType.makeRef(), 'pu', [operatorParam], const = True, noException = True)
    registerElements.append(operatorDecl)
    operatorBody = cxx_writer.Code('return *(this->reg_all);')
    operatorIntDecl = cxx_writer.MemberOperator(str(regMaxType), operatorBody, cxx_writer.Type(''), 'pu', const = True, noException = True)
    registerElements.append(operatorIntDecl)

    pipelineRegClass = cxx_writer.ClassDeclaration('PipelineRegister', registerElements, [registerType], namespaces = [namespace])
    pipelineRegClass.addConstructor(publicFullConstr)
    pipelineRegClass.addConstructor(publicEmptyConstr)
    pipelineRegClasses.append(pipelineRegClass)
    PipelineRegisterType = pipelineRegClass.getType()

    ############################################################################
    # Now I have to examine all the registers with special write back conditions
    # and create a special propagate method for all of them
    # First I need to create correspondence pipeline stages, numbers
    pipeNumbers = {}
    i = 0
    for pipeStage in self.pipes:
        pipeNumbers[pipeStage.name] = i
        i += 1
    orders = []
    for reg in self.regs:
        if reg.wbStageOrder:
            if not reg.wbStageOrder in orders:
                orders.append(reg.wbStageOrder)
    for order in orders:
        registerElements = []
        propagateCode = 'bool has_changes = false;\n'
        #propagateCode += 'if (!this->has_to_propagate) {\nreturn;\n}\n'
        for pipeStage in order:
            if pipeStage != order[0]:
                propagateCode += 'else '
            propagateCode += 'if (this->reg_stage[' + str(pipeNumbers[pipeStage]) + ']->time_stamp > this->reg_all->time_stamp) {\n'
            ######
            if trace and not combinedTrace:
                propagateCode += 'std::cerr << "Propagating stage ' + str(pipeNumbers[pipeStage]) + ' into all stages." << std::endl;\n'
            ######
            propagateCode += 'has_changes = true;\n'
            propagateCode += 'this->reg_all->force_value(*(this->reg_stage[' + str(pipeNumbers[pipeStage]) + ']));\n'
            propagateCode += 'this->reg_all->time_stamp = this->reg_stage[' + str(pipeNumbers[pipeStage]) + ']->time_stamp;\n'
            propagateCode += 'for (int i = 0; i < ' + str(len(self.pipes)) + '; i++) {\n'
            propagateCode += 'this->reg_stage[i]->force_value(*(this->reg_stage[' + str(pipeNumbers[pipeStage]) + ']));\n'
            propagateCode += 'this->reg_stage[i]->time_stamp = this->reg_stage[' + str(pipeNumbers[pipeStage]) + ']->time_stamp;\n'
            propagateCode += '}\n}\n'
        propagateCode += 'this->has_to_propagate = has_changes;\n'
        propagateBody = cxx_writer.Code(propagateCode)
        propagateMethod = cxx_writer.Method('propagate', propagateBody, cxx_writer.voidType, 'pu', noException = True)
        registerElements.append(propagateMethod)

        emptyBody = cxx_writer.Code('')
        publicFullConstr = cxx_writer.Constructor(emptyBody, 'pu', fullConstructorParams, ['PipelineRegister(' + innerConstrInit + ')'])
        publicEmptyConstr = cxx_writer.Constructor(emptyBody, 'pu', initList = ['PipelineRegister()'])
        pipelineRegClass = cxx_writer.ClassDeclaration('PipelineRegister_' + str(order)[1:-1].replace(', ', '_').replace('\'', ''), registerElements, [PipelineRegisterType], namespaces = [namespace])
        pipelineRegClass.addConstructor(publicFullConstr)
        pipelineRegClass.addConstructor(publicEmptyConstr)
        pipelineRegClasses.append(pipelineRegClass)

    return pipelineRegClasses

def getCPPRegisters(self, trace, combinedTrace, model, namespace):
    """This method creates all the classes necessary for declaring
    the registers: in particular the register base class
    and all the classes for the different bitwidths; in order to
    do this I simply iterate over the registers"""
    regLen = 0
    for reg in self.regs + self.regBanks:
        # I have to determine the register with the longest width and
        # accordingly create the type
        if reg.bitWidth > regLen:
            regLen = reg.bitWidth

    # Now I create the base class for all the registers
    registerElements = []

    from procWriter import resourceType
    global resourceType

    from isa import resolveBitType
    global regMaxType
    regMaxType = resolveBitType('BIT<' + str(regLen) + '>')
    registerType = cxx_writer.Type('Register')
    emptyBody = cxx_writer.Code('')

    ################ Constructor: it initializes the width of the register ######################
    constructorCode = ''
    if model.startswith('acc'):
        constructorCode += 'this->num_locked = 0;\n'
        constructorCode += 'this->locked_latency = -1;\n'
        constructorCode += 'this->has_to_propagate = NULL;\n'
        constructorCode += 'this->time_stamp = SC_ZERO_TIME;\n'
    constructorBody = cxx_writer.Code(constructorCode)
    publicConstr = cxx_writer.Constructor(constructorBody, 'pu')

    constructorCode = ''
    if model.startswith('acc'):
        constructorCode += 'this->num_locked = other.num_locked;\n'
        constructorCode += 'this->locked_latency = other.locked_latency;\n'
        constructorCode += 'this->has_to_propagate = other.has_to_propagate;\n'
        constructorCode += 'this->time_stamp = other.time_stamp;\n'
    copyConstrParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
    copyConstrBody = cxx_writer.Code(constructorCode)
    copyConstr = cxx_writer.Constructor(copyConstrBody, 'pu', [copyConstrParam])

    ################ Lock and Unlock methods used for hazards detection ######################
    if model.startswith('acc'):
        lockedLatencyAttribute = cxx_writer.Attribute('locked_latency', cxx_writer.intType, 'pri')
        registerElements.append(lockedLatencyAttribute)
        lockedAttribute = cxx_writer.Attribute('num_locked', cxx_writer.intType, 'pri')
        registerElements.append(lockedAttribute)
        propagateAttribute = cxx_writer.Attribute('has_to_propagate', cxx_writer.boolType.makePointer(), 'pu')
        registerElements.append(propagateAttribute)
        timeStampAttribute = cxx_writer.Attribute('time_stamp', cxx_writer.sc_timeType, 'pu')
        registerElements.append(timeStampAttribute)
        lockBody = cxx_writer.Code('this->locked_latency = -1;\nthis->num_locked++;')
        lockMethod = cxx_writer.Method('lock', lockBody, cxx_writer.voidType, 'pu', virtual = True, noException = True)
        registerElements.append(lockMethod)
        unlockBody = cxx_writer.Code('this->locked_latency = -1;\nif (this->num_locked > 0) {\nthis->num_locked--;\n}')
        unlockMethod = cxx_writer.Method('unlock', unlockBody, cxx_writer.voidType, 'pu', virtual = True, noException = True)
        registerElements.append(unlockMethod)
        latencyParam = cxx_writer.Parameter('wb_latency', cxx_writer.intType)
        unlockBody = cxx_writer.Code("""if (wb_latency > 0) {
            this->locked_latency = wb_latency;
        } else {
            this->locked_latency = -1;
            if (this->num_locked > 0) {
                this->num_locked--;
            }
        }""")
        unlockMethod = cxx_writer.Method('unlock', unlockBody, cxx_writer.voidType, 'pu', [latencyParam], virtual = True, noException = True)
        registerElements.append(unlockMethod)
        isLockedBody = cxx_writer.Code("""if (this->locked_latency > 0) {
            this->locked_latency--;
            if (this->locked_latency == 0) {
                this->num_locked--;
            }
        }
        if (this->num_locked > 0) {
            return true;
        } else {
            this->num_locked = 0;
            return false;
        }
        """)
        isLockedMethod = cxx_writer.Method('is_locked', isLockedBody, cxx_writer.boolType, 'pu', noException = True, virtual = True)
        registerElements.append(isLockedMethod)

    ################ Special Methods ######################
    immediateWriteParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
    immediateWriteMethod = cxx_writer.Method('immediate_write', emptyBody, cxx_writer.voidType, 'pu', immediateWriteParam, pure = True, noException = True)
    registerElements.append(immediateWriteMethod)
    readNewValueMethod = cxx_writer.Method('read_new_value', emptyBody, regMaxType, 'pu', pure = True, noException = True)
    registerElements.append(readNewValueMethod)
    if not model.startswith('acc'):
        clockCycleMethod = cxx_writer.Method('clock_cycle', emptyBody, cxx_writer.voidType, 'pu', virtual = True, noException = True)
        registerElements.append(clockCycleMethod)
    else:
        forceValueParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
        forceValueMethod = cxx_writer.Method('force_value', emptyBody, cxx_writer.voidType, 'pu', forceValueParam, pure = True, noException = True)
        registerElements.append(forceValueMethod)

    ################ Operators working with the base class, employed when polimorphism is used ##################
    # First lets declare the class which will be used to manipulate the
    # bitfields
    InnerFieldType = cxx_writer.Type('InnerField')
    operatorBody = cxx_writer.Code('*this = (unsigned)other;\nreturn *this;')
    operatorParam = cxx_writer.Parameter('other', InnerFieldType.makeRef().makeConst())
    operatorEqualInnerDecl = cxx_writer.MemberOperator('=', operatorBody, InnerFieldType.makeRef(), 'pu', [operatorParam], noException = True)
    operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
    operatorEqualDecl = cxx_writer.MemberOperator('=', emptyBody, InnerFieldType.makeRef(), 'pu', [operatorParam], pure = True, noException = True)
    operatorIntDecl = cxx_writer.MemberOperator(str(regMaxType), emptyBody, cxx_writer.Type(''), 'pu', const = True, pure = True, noException = True)
    InnerFieldClass = cxx_writer.ClassDeclaration('InnerField', [operatorEqualInnerDecl, operatorEqualDecl, operatorIntDecl], namespaces = [namespace])
    publicDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
    InnerFieldClass.addDestructor(publicDestr)

    # Now lets procede with the members of the main class
    operatorParam = cxx_writer.Parameter('bitfield', cxx_writer.intType)
    operatorDecl = cxx_writer.MemberOperator('[]', emptyBody, InnerFieldClass.getType().makeRef(), 'pu', [operatorParam], pure = True, noException = True)
    registerElements.append(operatorDecl)
    for i in unaryOps:
        operatorDecl = cxx_writer.MemberOperator(i, emptyBody, regMaxType, 'pu', pure = True, noException = True)
        registerElements.append(operatorDecl)
    for i in binaryOps:
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, emptyBody, regMaxType, 'pu', [operatorParam], const = True, pure = True, noException = True)
        registerElements.append(operatorDecl)
    for i in comparisonOps:
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, emptyBody, cxx_writer.boolType, 'pu', [operatorParam], const = True, pure = True, noException = True)
        registerElements.append(operatorDecl)

    pureDeclTypes = [regMaxType, registerType]
    for pureDecls in pureDeclTypes:
        for i in assignmentOps:
            operatorParam = cxx_writer.Parameter('other', pureDecls.makeRef().makeConst())
            operatorDecl = cxx_writer.MemberOperator(i, emptyBody, registerType.makeRef(), 'pu', [operatorParam], pure = True, noException = True)
            registerElements.append(operatorDecl)
    # Stream Operators
    outStreamType = cxx_writer.Type('std::ostream', 'ostream')
    operatorParam = cxx_writer.Parameter('other', outStreamType.makeRef())
    operatorDecl = cxx_writer.MemberOperator('<<', emptyBody, outStreamType.makeRef(), 'pu', [operatorParam], const = True, pure = True, noException = True)
    registerElements.append(operatorDecl)
    operatorIntDecl = cxx_writer.MemberOperator(str(regMaxType), emptyBody, cxx_writer.Type(''), 'pu', const = True, pure = True, noException = True)
    registerElements.append(operatorIntDecl)

    ################ Here we determine the different register types which have to be declared ##################
    customRegBanksElemens = []
    for i in self.regBanks:
        customRegBanksElemens += i.getConstRegs()
        customDelayElem = i.getDelayRegs()
        for j in customDelayElem:
            if not j.name in [rb.name for rb in customRegBanksElemens]:
                customRegBanksElemens.append(j)

    regTypes = []
    regTypeNames = []
    bitFieldHash = {}
    for reg in self.regs + self.regBanks + customRegBanksElemens:
        bitFieldSig = ''
        for maskName, maskPos in reg.bitMask.items():
            bitFieldSig += maskName + str(maskPos[0]) + str(maskPos[1])
        if not bitFieldSig in bitFieldHash:
            bitFieldHash[bitFieldSig] = len(bitFieldHash)
        curName = str(reg.bitWidth) + '_' + str(bitFieldHash[bitFieldSig])
        if not model.startswith('acc'):
            curName += '_' + str(reg.offset)
        if type(reg.constValue) == type(0):
            curName += '_' + str(reg.constValue)
        if type(reg.delay) == type(0) and not model.startswith('acc') and reg.delay > 0:
            curName += '_' + str(reg.delay)
        if not curName in regTypeNames:
            regTypes.append(reg)
            regTypeNames.append(curName)
        regTypeName = 'Reg' + str(reg.bitWidth) + '_' + str(bitFieldHash[bitFieldSig])
        if reg.offset and not model.startswith('acc'):
            regTypeName += '_off_' + str(reg.offset)
        if type(reg.constValue) == type(0):
            regTypeName += '_const_' + str(reg.constValue)
        if type(reg.delay) == type(0) and not model.startswith('acc') and reg.delay > 0:
            regTypeName += '_delay_' + str(reg.delay)
        resourceType[reg.name] = cxx_writer.Type(regTypeName, '#include \"registers.hpp\"')
        if reg in self.regBanks:
            if (reg.constValue and len(reg.constValue) < reg.numRegs)  or ((reg.delay and len(reg.delay) < reg.numRegs) and not model.startswith('acc')):
                resourceType[reg.name + '_baseType'] = resourceType[reg.name]
                resourceType[reg.name] = cxx_writer.Type('RegisterBankClass', '#include \"registers.hpp\"')
            else:
                resourceType[reg.name] = resourceType[reg.name].makePointer()
    realRegClasses = []
    for regType in regTypes:
        realRegClasses.append(regType.getCPPClass(model, resourceType[regType.name].makeNormal(), namespace))
    ################ End of part where we determine the different register types which have to be declared ##################

    registerDecl = cxx_writer.ClassDeclaration('Register', registerElements, namespaces = [namespace])
    registerDecl.addDocString(brief = 'Register Class', detail = 'An register holds fields that can be accessed in array notation. Most operators are defined, as well as callback functions per field. Some concepts were inspired by the by the Cadence scireg implementation.')
    registerDecl.addConstructor(publicConstr)
    registerDecl.addConstructor(copyConstr)

    ################ Finally I put everything together##################
    classes = [InnerFieldClass, registerDecl] + realRegClasses

    ###################################################################################
    # I also need to declare a global RegisterBank Class in case there are register banks
    # containing registers of different types
    hasRegBankClass = False
    for reg in self.regBanks:
        if reg.constValue:
            hasRegBankClass = True
            break
    if hasRegBankClass:
        registerType = cxx_writer.Type('Register', '#include \"registers.hpp\"')
        regBankElements = []
        regBankElements.append(cxx_writer.Attribute('registers', registerType.makePointer().makePointer(), 'pri'))
        regBankElements.append(cxx_writer.Attribute('size', cxx_writer.uintType, 'pri'))
        setNewRegisterBody = cxx_writer.Code("""if (reg_num > this->size - 1) {
            THROW_EXCEPTION("Register index " << reg_num << " is out of register bank bounds.");
        } else {
            this->registers[reg_num] = new_reg;
        }""")
        setNewRegisterBody.addInclude('common/report.hpp')
        setNewRegisterParams = [cxx_writer.Parameter('reg_num', cxx_writer.uintType), cxx_writer.Parameter('new_reg', registerType.makePointer())]
        setNewRegisterMethod = cxx_writer.Method('set_new_register', setNewRegisterBody, cxx_writer.voidType, 'pu', setNewRegisterParams)
        regBankElements.append(setNewRegisterMethod)
        setSizeBody = cxx_writer.Code("""
            for (unsigned i = 0; i < this->size; i++) {
                if (this->registers[i] != NULL) {
                    delete this->registers[i];
                }
            }
            if (this->registers != NULL) {
                delete [] this->registers;
            }
            this->size = size;
            this->registers = new """ + str(registerType.makePointer()) + """[this->size];
            for (unsigned i = 0; i < this->size; i++) {
                this->registers[i] = NULL;
            }
        """)
        setSizeParams = [cxx_writer.Parameter('size', cxx_writer.uintType)]
        setSizeMethod = cxx_writer.Method('set_size', setSizeBody, cxx_writer.voidType, 'pu', setSizeParams, noException = True)
        regBankElements.append(setSizeMethod)
        operatorBody = cxx_writer.Code('return *(this->registers[reg_num]);')
        operatorParam = [cxx_writer.Parameter('reg_num', cxx_writer.uintType)]
        operatorDecl = cxx_writer.MemberOperator('[]', operatorBody, registerType.makeRef(), 'pu', operatorParam, inline = True, noException = True)
        regBankElements.append(operatorDecl)
        regBankClass = cxx_writer.ClassDeclaration('RegisterBankClass', regBankElements, namespaces = [namespace])
        constructorBody = cxx_writer.Code("""this->size = size;
            this->registers = new """ + str(registerType.makePointer()) + """[this->size];
            for (unsigned i = 0; i < this->size; i++) {
                this->registers[i] = NULL;
            }
        """)
        constructorParams = [cxx_writer.Parameter('size', cxx_writer.uintType)]
        publicRegBankConstr = cxx_writer.Constructor(constructorBody, 'pu', constructorParams)
        regBankClass.addConstructor(publicRegBankConstr)
        constructorBody = cxx_writer.Code("""this->size = 0;
            this->registers = NULL;
        """)
        publicRegBankConstr = cxx_writer.Constructor(constructorBody, 'pu')
        regBankClass.addConstructor(publicRegBankConstr)
        destructorBody = cxx_writer.Code("""
            for (unsigned i = 0; i < this->size; i++) {
                if (this->registers[i] != NULL) {
                    delete this->registers[i];
                }
            }
            if (this->registers != NULL) {
                delete [] this->registers;
            }
        """)
        publicRegBankDestr = cxx_writer.Destructor(destructorBody, 'pu', True)
        regBankClass.addDestructor(publicRegBankDestr)
        classes.append(regBankClass)

    # Now I have to add the classes for the pipeline registers for the cycle accurate processor
    if model.startswith('acc'):
        classes += self.getCPPPipelineReg(trace, combinedTrace, namespace)

    return classes

def getCPPAlias(self, namespace):
    """This method creates the class describing a register
    alias: note that an alias simply holds a pointer to a register; the
    operators are then redefined in order to call the corresponding operators
    of the register. In addition there is the update_alias operation which updates
    the register this alias points to (and eventually the offset)."""
    regWidthType = regMaxType
    registerType = cxx_writer.Type('Register', '#include \"registers.hpp\"')
    aliasType = cxx_writer.Type('Alias', '#include \"alias.hpp\"')
    aliasElements = []
    global resourceType
    from procWriter import resourceType

    for i in self.aliasRegs + self.aliasRegBanks:
        resourceType[i.name] = aliasType

    ####################### Lets declare the operators used to access the register fields ##############
    codeOperatorBody = 'return (*this->reg)[bitfield];'
    InnerFieldType = cxx_writer.Type('InnerField')
    operatorBody = cxx_writer.Code(codeOperatorBody)
    operatorParam = [cxx_writer.Parameter('bitfield', cxx_writer.intType)]
    operatorDecl = cxx_writer.MemberOperator('[]', operatorBody, InnerFieldType.makeRef(), 'pu', operatorParam, noException = True, inline = True)
    aliasElements.append(operatorDecl)

    ################ Methods used for the management of delayed registers ######################
    immediateWriteBody = isLockedBody = cxx_writer.Code('this->reg->immediate_write(value);')
    immediateWriteParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
    immediateWriteMethod = cxx_writer.Method('immediate_write', immediateWriteBody, cxx_writer.voidType, 'pu', immediateWriteParam, noException = True)
    aliasElements.append(immediateWriteMethod)
    readNewValueBody = isLockedBody = cxx_writer.Code('return this->reg->read_new_value();')
    readNewValueMethod = cxx_writer.Method('read_new_value', readNewValueBody, regMaxType, 'pu', noException = True)
    aliasElements.append(readNewValueMethod)

    getRegBody = cxx_writer.Code('return this->reg;')
    getRegMethod = cxx_writer.Method('get_reg', getRegBody, registerType.makePointer(), 'pu', const = True, inline = True, noException = True)
    aliasElements.append(getRegMethod)

    #################### Lets declare the normal operators (implementation of the pure operators of the base class) ###########
    for i in unaryOps:
        operatorBody = cxx_writer.Code('return ' + i + '(*this->reg + this->offset);')
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', noException = True)
        aliasElements.append(operatorDecl)
    # Now I have the three versions of the operators, depending whether they take
    # in input the integer value, the specific register or the base one
    # INTEGER
#     for i in binaryOps:
#         operatorBody = cxx_writer.Code('return (*this->reg ' + i + ' other);')
#         operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
#         operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True)
#         aliasElements.append(operatorDecl)
#     for i in comparisonOps:
#         operatorBody = cxx_writer.Code('return (*this->reg ' + i + ' (other - this->offset));')
#         operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
#         operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True)
#         aliasElements.append(operatorDecl)
    for i in assignmentOps:
        operatorBody = cxx_writer.Code('*this->reg ' + i + ' other;\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, aliasType.makeRef(), 'pu', [operatorParam], inline = True, noException = True)
        aliasElements.append(operatorDecl)
    # Alias Register
    for i in binaryOps:
        operatorBody = cxx_writer.Code('return ((*this->reg + this->offset) ' + i + ' *other.reg);')
        operatorParam = cxx_writer.Parameter('other', aliasType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True, noException = True)
        aliasElements.append(operatorDecl)
#    for i in comparisonOps:
#        operatorBody = cxx_writer.Code('return ((*this->reg + this->offset) ' + i + ' *other.reg);')
#        operatorParam = cxx_writer.Parameter('other', aliasType.makeRef().makeConst())
#        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True)
#        aliasElements.append(operatorDecl)
    for i in assignmentOps:
        operatorBody = cxx_writer.Code('*this->reg ' + i + ' *other.reg;\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', aliasType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, aliasType.makeRef(), 'pu', [operatorParam], noException = True)
        aliasElements.append(operatorDecl)
    # GENERIC REGISTER:
    for i in binaryOps:
        operatorBody = cxx_writer.Code('return ((*this->reg + this->offset) ' + i + ' other);')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True, inline = True, noException = True)
        aliasElements.append(operatorDecl)
    for i in comparisonOps:
        operatorBody = cxx_writer.Code('return ((*this->reg + this->offset) ' + i + ' other);')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True, noException = True)
        aliasElements.append(operatorDecl)
    for i in assignmentOps:
        operatorBody = cxx_writer.Code('*this->reg ' + i + ' other;\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, aliasType.makeRef(), 'pu', [operatorParam], noException = True)
        aliasElements.append(operatorDecl)
    # Scalar value cast operator
    operatorBody = cxx_writer.Code('return *this->reg + this->offset;')
    operatorIntDecl = cxx_writer.MemberOperator(str(regMaxType), operatorBody, cxx_writer.Type(''), 'pu', const = True, noException = True, inline = True)
    aliasElements.append(operatorIntDecl)

    ######### Constructor: takes as input the initial register #########
    constructorBody = cxx_writer.Code('this->referring_aliases = NULL;')
    constructorParams = [cxx_writer.Parameter('reg', registerType.makePointer())]
    constructorInit = ['reg(reg)']
    constructorParams.append(cxx_writer.Parameter('offset', cxx_writer.uintType, initValue = '0'))
    constructorInit += ['offset(offset)', 'default_offset(0)']
    publicMainClassConstr = cxx_writer.Constructor(constructorBody, 'pu', constructorParams, constructorInit)
    constructorInit = ['offset(0)', 'default_offset(0)']
    publicMainEmptyClassConstr = cxx_writer.Constructor(constructorBody, 'pu', [], constructorInit)
    # Constructor: takes as input the initial alias
    constructorBody = cxx_writer.Code('init_alias->referred_aliases.push_back(this);\nthis->referring_aliases = init_alias;')
    constructorParams = [cxx_writer.Parameter('init_alias', aliasType.makePointer())]
    publicAliasConstrInit = ['reg(init_alias->reg)']
    constructorParams.append(cxx_writer.Parameter('offset', cxx_writer.uintType, initValue = '0'))
    publicAliasConstrInit += ['offset(init_alias->offset + offset)', 'default_offset(offset)']
    publicAliasConstr = cxx_writer.Constructor(constructorBody, 'pu', constructorParams, publicAliasConstrInit)
    destructorBody = cxx_writer.Code("""std::list<Alias*>::iterator referred_it, referred_end;
        for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
            if ((*referred_it)->referring_aliases == this) {
                (*referred_it)->referring_aliases = NULL;
            }
        }
        if (this->referring_aliases != NULL) {
            this->referring_aliases->referred_aliases.remove(this);
        }
        this->referring_aliases = NULL;""")
    publicAliasDestr = cxx_writer.Constructor(destructorBody, 'pu')

    # Stream Operators
    outStreamType = cxx_writer.Type('std::ostream', 'ostream')
    code = 'stream << *this->reg + this->offset;\nreturn stream;'
    operatorBody = cxx_writer.Code(code)
    operatorParam = cxx_writer.Parameter('stream', outStreamType.makeRef())
    operatorDecl = cxx_writer.MemberOperator('<<', operatorBody, outStreamType.makeRef(), 'pu', [operatorParam], const = True, noException = True)
    aliasElements.append(operatorDecl)

    # Update method: updates the register pointed by this alias: Standard Alias
    updateCode = """this->reg = new_alias.reg;
    this->offset = new_alias.offset + new_offset;
    this->default_offset = new_offset;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
        (*referred_it)->new_referred_alias(new_alias.reg, new_alias.offset + new_offset);
    }
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = &new_alias;
    new_alias.referred_aliases.push_back(this);
    """
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', aliasType.makeRef())]
    updateParam.append(cxx_writer.Parameter('new_offset', cxx_writer.uintType))
    updateDecl = cxx_writer.Method('update_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, inline = True, noException = True)
    aliasElements.append(updateDecl)
    updateCode = """this->offset = new_alias.offset;
    this->default_offset = 0;
    """
    updateCode += """this->reg = new_alias.reg;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
    """
    updateCode += '(*referred_it)->new_referred_alias(new_alias.reg, new_alias.offset);'
    updateCode += """
    }
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = &new_alias;
    new_alias.referred_aliases.push_back(this);
    """
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', aliasType.makeRef())]
    updateDecl = cxx_writer.Method('update_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, inline = True, noException = True)
    aliasElements.append(updateDecl)

    updateCode = """this->reg = &new_alias;
    this->offset = new_offset;
    this->default_offset = new_offset;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
        (*referred_it)->new_referred_alias(&new_alias, new_offset);
    }
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = NULL;
    """
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', registerType.makeRef())]
    updateParam.append(cxx_writer.Parameter('new_offset', cxx_writer.uintType))
    updateDecl = cxx_writer.Method('update_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, inline = True, noException = True)
    aliasElements.append(updateDecl)

    updateCode = """this->offset = 0;
    this->default_offset = 0;
    """
    updateCode += """this->reg = &new_alias;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
        (*referred_it)->new_referred_alias(&new_alias);
    }
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = NULL;
    """
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', registerType.makeRef())]
    updateDecl = cxx_writer.Method('update_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, inline = True, noException = True)
    aliasElements.append(updateDecl)

    directSetCode = 'this->reg = new_alias.reg;\n'
    directSetCode += 'this->offset = new_alias.offset;\n'
    directSetCode += """if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = &new_alias;
    new_alias.referred_aliases.push_back(this);
    """
    directSetBody = cxx_writer.Code(directSetCode)
    directSetParam = [cxx_writer.Parameter('new_alias', aliasType.makeRef())]
    directSetDecl = cxx_writer.Method('direct_set_alias', directSetBody, cxx_writer.voidType, 'pu', directSetParam, noException = True)
    aliasElements.append(directSetDecl)

    directSetBody = cxx_writer.Code("""this->reg = &new_alias;
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = NULL;""")
    directSetParam = [cxx_writer.Parameter('new_alias', registerType.makeRef())]
    directSetDecl = cxx_writer.Method('direct_set_alias', directSetBody, cxx_writer.voidType, 'pu', directSetParam, noException = True)
    aliasElements.append(directSetDecl)

    updateCode = """this->reg = new_alias;
    this->offset = new_offset + this->default_offset;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
        (*referred_it)->new_referred_alias(new_alias, new_offset);
    }
    """
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', registerType.makePointer())]
    updateParam.append(cxx_writer.Parameter('new_offset', cxx_writer.uintType))
    updateDecl = cxx_writer.Method('new_referred_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, inline = True, noException = True)
    aliasElements.append(updateDecl)

    updateCode = 'this->offset = this->default_offset;\n'
    updateCode += """this->reg = new_alias;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
        (*referred_it)->new_referred_alias(new_alias);
    }"""
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', registerType.makePointer())]
    updateDecl = cxx_writer.Method('new_referred_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, inline = True, noException = True)
    aliasElements.append(updateDecl)


    regAttribute = cxx_writer.Attribute('reg', registerType.makePointer(), 'pri')
    aliasElements.append(regAttribute)
    offsetAttribute = cxx_writer.Attribute('offset', cxx_writer.uintType, 'pri')
    aliasElements.append(offsetAttribute)
    offsetAttribute = cxx_writer.Attribute('default_offset', cxx_writer.uintType, 'pri')
    aliasElements.append(offsetAttribute)

    # Finally I declare the class and pass to it all the declared members: Standard Alias
    aliasesAttribute = cxx_writer.Attribute('referred_aliases', cxx_writer.TemplateType('std::list', [aliasType.makePointer()], 'list'), 'pri')
    aliasElements.append(aliasesAttribute)
    aliasesAttribute = cxx_writer.Attribute('referring_aliases', aliasType.makePointer(), 'pri')
    aliasElements.append(aliasesAttribute)
    aliasDecl = cxx_writer.ClassDeclaration(aliasType.name, aliasElements, namespaces = [namespace])
    aliasDecl.addDocString(brief = 'Register Alias Class', detail = 'An alias simply holds a pointer to a register. The operators are redefined in order to call the corresponding operators of the register. There is also an update_alias() method which updates the register this alias points to (and possibly the offset).')
    aliasDecl.addConstructor(publicMainClassConstr)
    aliasDecl.addConstructor(publicMainEmptyClassConstr)
    aliasDecl.addConstructor(publicAliasConstr)
    aliasDecl.addDestructor(publicAliasDestr)

    classes = [aliasDecl]
    return classes

def getCPPPipelineAlias(self, namespace):
    """This method creates the class describing a register
    alias: note that an alias simply holds a pointer to a register; the
    operators are then redefined in order to call the corresponding operators
    of the register. In addition there is the update_alias operation which updates
    the register this alias points to (and eventually the offset)."""
    regWidthType = regMaxType
    # If the cycle accurate processor is used, then each alias contains also the pipeline
    # register, i.e. the registers with the copies (latches) for each pipeline stage
    pipeRegisterType = cxx_writer.Type('PipelineRegister', '#include \"registers.hpp\"')
    registerType = cxx_writer.Type('Register', '#include \"registers.hpp\"')
    aliasType = cxx_writer.Type('Alias', '#include \"alias.hpp\"')
    aliasElements = []
    global resourceType
    from procWriter import resourceType

    for i in self.aliasRegs + self.aliasRegBanks:
        resourceType[i.name] = aliasType

    ####################### Lets declare the operators used to access the register fields ##############
    codeOperatorBody = 'return (*this->pipeline_reg->get_register(this->pipeline_id))[bitfield];'
    InnerFieldType = cxx_writer.Type('InnerField')
    operatorBody = cxx_writer.Code(codeOperatorBody)
    operatorParam = [cxx_writer.Parameter('bitfield', cxx_writer.intType)]
    operatorDecl = cxx_writer.MemberOperator('[]', operatorBody, InnerFieldType.makeRef(), 'pu', operatorParam, noException = True, inline = True)
    aliasElements.append(operatorDecl)

    ################ Lock and Unlock methods used for hazards detection ######################
    pipeIdParam = cxx_writer.Parameter('pipeline_id', cxx_writer.uintType)
    setpipeIdBody = cxx_writer.Code('if (this->pipeline_id < 0) {\nthis->pipeline_id = pipeline_id;\n} else {\nTHROW_ERROR(\"Pipeline id of alias can only be set during alias initialization and has already been initialized to \" << this->pipeline_id << \'.\');\n}')
    setpipeIdMethod = cxx_writer.Method('set_pipe_id', setpipeIdBody, cxx_writer.voidType, 'pu', [pipeIdParam])
    aliasElements.append(setpipeIdMethod)
    getPipeRegBody = cxx_writer.Code('return this->pipeline_reg;')
    getPipeRegMethod = cxx_writer.Method('get_pipe_reg', getPipeRegBody, pipeRegisterType.makePointer(), 'pu', const = True, inline = True, noException = True)
    aliasElements.append(getPipeRegMethod)
    getRegBody = cxx_writer.Code('return this->pipeline_reg->get_register(this->pipeline_id);')
    getRegMethod = cxx_writer.Method('get_reg', getRegBody, registerType.makePointer(), 'pu', const = True, inline = True, noException = True)
    aliasElements.append(getRegMethod)
    newPipelineRegParam = cxx_writer.Parameter('newPipelineReg', pipeRegisterType.makePointer())
    #setPipeRegBody = cxx_writer.Code('this->pipeline_reg = newPipelineReg;')
    #setPipeRegMethod = cxx_writer.Method('set_pipe_reg', setPipeRegBody, cxx_writer.voidType, 'pu', [newPipelineRegParam], inline = True, noException = True)
    #aliasElements.append(setPipeRegMethod)
    lockBody = cxx_writer.Code('this->pipeline_reg->lock();')
    lockMethod = cxx_writer.Method('lock', lockBody, cxx_writer.voidType, 'pu', inline = True, noException = True)
    aliasElements.append(lockMethod)
    unlockBody = cxx_writer.Code('this->pipeline_reg->unlock();')
    unlockMethod = cxx_writer.Method('unlock', unlockBody, cxx_writer.voidType, 'pu', inline = True, noException = True)
    aliasElements.append(unlockMethod)
    latencyParam = cxx_writer.Parameter('wb_latency', cxx_writer.intType)
    unlockBody = cxx_writer.Code('this->pipeline_reg->unlock(wb_latency);')
    unlockMethod = cxx_writer.Method('unlock', unlockBody, cxx_writer.voidType, 'pu', [latencyParam], inline = True, noException = True)
    aliasElements.append(unlockMethod)
    isLockedBody = cxx_writer.Code('return this->pipeline_reg->is_locked();')
    isLockedMethod = cxx_writer.Method('is_locked', isLockedBody, cxx_writer.boolType, 'pu', inline = True, noException = True)
    aliasElements.append(isLockedMethod)

    writeAllBody = cxx_writer.Code('this->pipeline_reg->write_all(value);')
    writeAllParam = [cxx_writer.Parameter('value', regMaxType.makeRef().makeConst())]
    writeAllMethod = cxx_writer.Method('write_all', writeAllBody, cxx_writer.voidType, 'pu', writeAllParam, noException = True)
    aliasElements.append(writeAllMethod)

    #################### Lets declare the normal operators (implementation of the pure operators of the base class) ###########
    for i in unaryOps:
        operatorBody = cxx_writer.Code('return ' + i + '*this->pipeline_reg->get_register(this->pipeline_id);')
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', noException = True)
        aliasElements.append(operatorDecl)
    # Now I have the three versions of the operators, depending whether they take
    # in input the integer value, the specific register or the base one
    # INTEGER
#     for i in binaryOps:
#         operatorBody = cxx_writer.Code('return (*this->reg ' + i + ' other);')
#         operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
#         operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True)
#         aliasElements.append(operatorDecl)
#     for i in comparisonOps:
#         operatorBody = cxx_writer.Code('return (*this->reg ' + i + ' (other - this->offset));')
#         operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
#         operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True)
#         aliasElements.append(operatorDecl)
    for i in assignmentOps:
        operatorBody = cxx_writer.Code('*this->pipeline_reg->get_register(this->pipeline_id) ' + i + ' other;\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', regMaxType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, aliasType.makeRef(), 'pu', [operatorParam], inline = True, noException = True)
        aliasElements.append(operatorDecl)
    # Alias Register
#    for i in binaryOps:
#        operatorBody = cxx_writer.Code('return (*this->pipeline_reg->get_register(this->pipeline_id) ' + i + ' *other.pipeline_reg->get_register(other.pipeline_id));')
#        operatorParam = cxx_writer.Parameter('other', aliasType.makeRef().makeConst())
#        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True, noException = True)
#        aliasElements.append(operatorDecl)
#    for i in comparisonOps:
#        operatorBody = cxx_writer.Code('return ((*this->reg + this->offset) ' + i + ' *other.reg);')
#        operatorParam = cxx_writer.Parameter('other', aliasType.makeRef().makeConst())
#        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True)
#        aliasElements.append(operatorDecl)
    for i in assignmentOps:
        operatorBody = cxx_writer.Code('*this->pipeline_reg->get_register(this->pipeline_id) ' + i + ' *other.pipeline_reg->get_register(other.pipeline_id);\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', aliasType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, aliasType.makeRef(), 'pu', [operatorParam], noException = True)
        aliasElements.append(operatorDecl)
    # GENERIC REGISTER:
    for i in binaryOps:
        operatorBody = cxx_writer.Code('return  (*this->pipeline_reg->get_register(this->pipeline_id) ' + i + ' other);')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, regMaxType, 'pu', [operatorParam], const = True, inline = True, noException = True)
        aliasElements.append(operatorDecl)
    for i in comparisonOps:
        operatorBody = cxx_writer.Code('return (*this->pipeline_reg->get_register(this->pipeline_id) ' + i + ' other);')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True, noException = True)
        aliasElements.append(operatorDecl)
    for i in assignmentOps:
        operatorBody = cxx_writer.Code('*this->pipeline_reg->get_register(this->pipeline_id) ' + i + ' other;\nreturn *this;')
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, aliasType.makeRef(), 'pu', [operatorParam], noException = True)
        aliasElements.append(operatorDecl)
    # Scalar value cast operator
    operatorBody = cxx_writer.Code('return *this->pipeline_reg->get_register(this->pipeline_id);')
    operatorIntDecl = cxx_writer.MemberOperator(str(regMaxType), operatorBody, cxx_writer.Type(''), 'pu', const = True, noException = True, inline = True)
    aliasElements.append(operatorIntDecl)

    ######### Constructor: takes as input the initial register #########
    constructorBody = cxx_writer.Code('this->referring_aliases = NULL;')
    pipeRegParam = cxx_writer.Parameter('pipeline_reg', pipeRegisterType.makePointer())
    pipeIdParam = cxx_writer.Parameter('pipeline_id', cxx_writer.intType)
    publicMainClassConstr = cxx_writer.Constructor(constructorBody, 'pu', [pipeRegParam, pipeIdParam], ['pipeline_reg(pipeline_reg), pipeline_id(pipeline_id)'])
    publicMainEmptyClassConstr = cxx_writer.Constructor(constructorBody, 'pu', [pipeIdParam], ['pipeline_reg(NULL), pipeline_id(pipeline_id)'])
    publicMainEmpty2ClassConstr = cxx_writer.Constructor(constructorBody, 'pu', initList = ['pipeline_reg(NULL), pipeline_id(-1)'])
    # Constructor: takes as input the initial alias
    constructorBody = cxx_writer.Code('init_alias->referred_aliases.push_back(this);\nthis->referring_aliases = init_alias;')
    constructorParams = [cxx_writer.Parameter('init_alias', aliasType.makePointer())]
    constructorParams.append(cxx_writer.Parameter('pipeline_id', cxx_writer.intType, initValue = '-1'))
    publicAliasConstrInit = ['pipeline_reg(init_alias->pipeline_reg), pipeline_id(pipeline_id)']
    publicAliasConstr = cxx_writer.Constructor(constructorBody, 'pu', constructorParams, publicAliasConstrInit)
    destructorBody = cxx_writer.Code("""std::list<Alias*>::iterator referred_it, referred_end;
        for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
            if ((*referred_it)->referring_aliases == this) {
                (*referred_it)->referring_aliases = NULL;
            }
        }
        if (this->referring_aliases != NULL) {
            this->referring_aliases->referred_aliases.remove(this);
        }
        this->referring_aliases = NULL;""")
    publicAliasDestr = cxx_writer.Constructor(destructorBody, 'pu')

    # Stream Operators
    outStreamType = cxx_writer.Type('std::ostream', 'ostream')
    code = 'stream << *this->pipeline_reg->get_register(this->pipeline_id);\nreturn stream;'
    operatorBody = cxx_writer.Code(code)
    operatorParam = cxx_writer.Parameter('stream', outStreamType.makeRef())
    operatorDecl = cxx_writer.MemberOperator('<<', operatorBody, outStreamType.makeRef(), 'pu', [operatorParam], const = True, noException = True)
    aliasElements.append(operatorDecl)

    # Update method: updates the register pointed by this alias: Standard Alias
    updateCode = """this->pipeline_reg = new_alias.pipeline_reg;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
    """
    updateCode += '(*referred_it)->new_referred_alias(new_alias.pipeline_reg);'
    updateCode += """
    }
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = &new_alias;
    new_alias.referred_aliases.push_back(this);
    """
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', aliasType.makeRef())]
    updateDecl = cxx_writer.Method('update_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, noException = True)
    aliasElements.append(updateDecl)

    updateCode = """this->pipeline_reg = &new_alias;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
        (*referred_it)->new_referred_alias(&new_alias);
    }
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = NULL;
    """
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', pipeRegisterType.makeRef())]
    # changed here
    updateDecl = cxx_writer.Method('update_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, inline = False, noException = True)
    aliasElements.append(updateDecl)

    propagateCode = """if (this->referring_aliases != NULL) {
        return;
    }
    this->pipeline_reg = &new_alias;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
        (*referred_it)->new_referred_alias(&new_alias);
    }
    """
    propagateBody = cxx_writer.Code(propagateCode)
    propagateParam = [cxx_writer.Parameter('new_alias', pipeRegisterType.makeRef())]
    propagateDecl = cxx_writer.Method('propagate_alias', propagateBody, cxx_writer.voidType, 'pu', propagateParam, inline = True, noException = True)
    aliasElements.append(propagateDecl)

    directSetCode = """this->pipeline_reg = new_alias.pipeline_reg;
    this->pipeline_id = new_alias.pipeline_id;
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = &new_alias;
    new_alias.referred_aliases.push_back(this);
    """
    directSetBody = cxx_writer.Code(directSetCode)
    directSetParam = [cxx_writer.Parameter('new_alias', aliasType.makeRef())]
    directSetDecl = cxx_writer.Method('direct_set_alias', directSetBody, cxx_writer.voidType, 'pu', directSetParam, noException = True)
    aliasElements.append(directSetDecl)

    directSetBody = cxx_writer.Code("""this->pipeline_reg = &new_alias;
    if (this->referring_aliases != NULL) {
        this->referring_aliases->referred_aliases.remove(this);
    }
    this->referring_aliases = NULL;""")
    directSetParam = [cxx_writer.Parameter('new_alias', pipeRegisterType.makeRef())]
    directSetDecl = cxx_writer.Method('direct_set_alias', directSetBody, cxx_writer.voidType, 'pu', directSetParam, noException = True)
    aliasElements.append(directSetDecl)

    updateCode = """this->pipeline_reg = new_alias;
    std::list<Alias*>::iterator referred_it, referred_end;
    for (referred_it = this->referred_aliases.begin(), referred_end = this->referred_aliases.end(); referred_it != referred_end; referred_it++) {
        (*referred_it)->new_referred_alias(new_alias);
    }"""
    updateBody = cxx_writer.Code(updateCode)
    updateParam = [cxx_writer.Parameter('new_alias', pipeRegisterType.makePointer())]
    updateDecl = cxx_writer.Method('new_referred_alias', updateBody, cxx_writer.voidType, 'pu', updateParam, inline = True, noException = True)
    aliasElements.append(updateDecl)

    regAttribute = cxx_writer.Attribute('pipeline_reg', pipeRegisterType.makePointer(), 'pri')
    aliasElements.append(regAttribute)
    pipelineIdAttribute = cxx_writer.Attribute('pipeline_id', cxx_writer.intType, 'pri')
    aliasElements.append(pipelineIdAttribute)

    # Finally I declare the class and pass to it all the declared members: Standard Alias
    aliasesAttribute = cxx_writer.Attribute('referred_aliases', cxx_writer.TemplateType('std::list', [aliasType.makePointer()], 'list'), 'pri')
    aliasElements.append(aliasesAttribute)
    aliasesAttribute = cxx_writer.Attribute('referring_aliases', aliasType.makePointer(), 'pri')
    aliasElements.append(aliasesAttribute)
    aliasDecl = cxx_writer.ClassDeclaration(aliasType.name, aliasElements, namespaces = [namespace])
    aliasDecl.addDocString(brief = 'Register Alias Class', detail = 'An alias simply holds a pointer to a register. The operators are redefined in order to call the corresponding operators of the register. There is also an update_alias() method which updates the register this alias points to (and possibly the offset).')
    aliasDecl.addConstructor(publicMainClassConstr)
    aliasDecl.addConstructor(publicMainEmptyClassConstr)
    aliasDecl.addConstructor(publicMainEmpty2ClassConstr)
    aliasDecl.addConstructor(publicAliasConstr)
    aliasDecl.addDestructor(publicAliasDestr)

    classes = [aliasDecl]
    return classes
