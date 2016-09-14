################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     irqWriter.py
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
# IntrInstruction Class
################################################################################
def getCPPIRQInstr(self, model, trace, namespace):
    from procWriter import instrCtorParams, instrCtorValues
    instructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
    from registerWriter import registerType

    IRQInstrClasses = []
    emptyBody = cxx_writer.Code('')

    for irq in self.irqs:
        IRQInstrMembers = []

        # Methods: get_id()
        getIdBody = cxx_writer.Code('return (unsigned)-1;')
        getIdMethod = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'public', noException = True, const = True)
        IRQInstrMembers.append(getIdMethod)

        # Methods: get_name()
        getNameBody = cxx_writer.Code('return \"' + irq.name + 'IntrInstruction\";')
        getNameMethod = cxx_writer.Method('get_name', getNameBody, cxx_writer.stringType, 'public', noException = True, const = True)
        IRQInstrMembers.append(getNameMethod)

        # Methods: get_mnemonic()
        getMnemonicBody = cxx_writer.Code('return \"' + irq.name + '\";')
        getMnemonicMethod = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'public', noException = True, const = True)
        IRQInstrMembers.append(getMnemonicMethod)

        # Methods: replicate()
        replicateBody = cxx_writer.Code('return new ' + irq.name + 'IntrInstruction(' + instrCtorValues + ', this->' + irq.name + ');')
        replicateMethod = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'public', parameters = [cxx_writer.Parameter('instr', instructionType.makePointer(), initValue = 'NULL')], noException = True, const = True)
        IRQInstrMembers.append(replicateMethod)

        # Methods: behavior()
        for pipeStage in self.pipes:
            behaviorUserCode = ''
            if irq.operation.has_key(pipeStage.name):
                behaviorUserCode += '{\nunsigned num_cycles = 0;\n'
                behaviorUserCode += str(irq.operation[pipeStage.name])
                behaviorUserCode += 'this->'
                if model.startswith('acc'): behaviorUserCode += 'num_stage_cycles'
                else: behaviorUserCode += 'num_instr_cycles'
                behaviorUserCode += ' += num_cycles;\n}\n'

            if model.startswith('acc'):
                behaviorCode = 'this->num_stage_cycles = 0;\n'
                if behaviorUserCode:
                    # Set the pipeline stage so that the usage of the registers
                    # is transparent to the user.
                    behaviorCode += '\nR.set_stage(' + str(self.pipes.index(pipeStage)) + ');\n'
                    behaviorCode += behaviorUserCode
                    behaviorCode += '\nR.unset_stage();\n'
                behaviorCode += 'return this->num_stage_cycles;\n'
                behaviorBody = cxx_writer.Code(behaviorCode)
                behaviorMethod = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'public')
                IRQInstrMembers.append(behaviorMethod)
        if not model.startswith('acc'):
            behaviorCode = 'this->num_instr_cycles = 0;\n'
            behaviorCode += behaviorUserCode
            behaviorCode += 'return this->num_instr_cycles;\n'
            behaviorBody = cxx_writer.Code(behaviorCode)
            behaviorMethod = cxx_writer.Method('behavior', behaviorBody, cxx_writer.uintType, 'public')
            IRQInstrMembers.append(behaviorMethod)

        # Methods: set_interrupt_value()
        # Sets the value of the received interrupt and the related attribute.
        from isa import resolveBitType
        irqWidthType = resolveBitType('BIT<' + str(irq.portWidth) + '>')
        IRQAttribute = cxx_writer.Attribute(irq.name, irqWidthType.makeRef(), 'public')
        IRQInstrMembers.append(IRQAttribute)
        IRQInstrCtorParams = [cxx_writer.Parameter(irq.name, irqWidthType.makeRef())]
        IRQInstrCtorInit = [irq.name + '(' + irq.name + ')']
        InterruptValueParam = cxx_writer.Parameter('interrupt_value', irqWidthType.makeRef().makeConst())
        setInterruptValueBody = cxx_writer.Code('this->' + irq.name + ' = interrupt_value;')
        setInterruptValueMethod = cxx_writer.Method('set_interrupt_value', setInterruptValueBody, cxx_writer.voidType, 'public', [InterruptValueParam], noException = True, inline = True)
        IRQInstrMembers.append(setInterruptValueMethod)

        # Attributes
        for var in irq.variables:
            IRQInstrMembers.append(cxx_writer.Attribute(var.name, var.varType, 'protected',  var.static))

        # Constructors and Destructors
        IRQInstrCtor = cxx_writer.Constructor(emptyBody, 'public', parameters = instrCtorParams + IRQInstrCtorParams, initList = ['Instruction(' + instrCtorValues + ')'] + IRQInstrCtorInit)
        IRQInstrDtor = cxx_writer.Destructor(emptyBody, 'public', True)

        # Class
        IRQInstrClass = cxx_writer.ClassDeclaration(irq.name + 'IntrInstruction', IRQInstrMembers, [instructionType], namespaces = [namespace])
        IRQInstrClass.addDocString(brief = 'IRQ Instruction Class', detail = 'Wraps IRQ exception handling behavior as a dummy instruction.')
        IRQInstrClass.addConstructor(IRQInstrCtor)
        IRQInstrClass.addDestructor(IRQInstrDtor)
        IRQInstrClasses.append(IRQInstrClass)

    return IRQInstrClasses

################################################################################
