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

def getGetIRQInstr(self, model, trace, namespace):
    from procWriter import instrCtorParams, instrCtorValues
    from pipelineWriter import hasCheckHazard
    from registerWriter import registerType
    instructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
    emptyBody = cxx_writer.Code('')

    IRQInstrClasses = []

    for irq in self.irqs:
        IRQInstrElements = []

        # now I have to go over the behavior of this interrupt and create, like for the instructions,
        # the behavior for the different pipeline stages
        if not model.startswith('acc'):
            behaviorCode = 'this->total_instr_cycles = 0;\n'
        userDefineBehavior = ''
        for pipeStage in self.pipes:
            userDefineBehavior = ''
            if model.startswith('acc'):
                behaviorCode = 'this->stage_cycles = 0;\n'
            if irq.operation.has_key(pipeStage.name):
                userDefineBehavior += str(irq.operation[pipeStage.name])
                if model.startswith('acc'):
                    # now I have to take all the resources and create a define which
                    # renames such resources so that their usage can be transparent
                    # to the developer
                    for reg in self.regs + self.regBanks + self.aliasRegs + self.aliasRegBanks:
                        behaviorCode += '#define ' + reg.name + ' ' + reg.name + '_' + pipeStage.name + '\n'
                    behaviorCode += '\n'

                behaviorCode += userDefineBehavior

                if model.startswith('acc'):
                    for reg in self.regs + self.regBanks + self.aliasRegs + self.aliasRegBanks:
                        behaviorCode += '#undef ' + reg.name + '\n'
            if model.startswith('acc'):
                behaviorCode += 'return this->stage_cycles;\n\n'
                registerType = cxx_writer.Type('Register')
                unlockQueueType = cxx_writer.TemplateType('std::map', ['unsigned', cxx_writer.TemplateType('std::vector', [registerType.makePointer()], 'vector')], 'map')
                unlockQueueParam = cxx_writer.Parameter('unlock_queue', unlockQueueType.makeRef())
                behaviorBody = cxx_writer.Code(behaviorCode)
                behaviorDecl = cxx_writer.Method('behavior_' + pipeStage.name, behaviorBody, cxx_writer.uintType, 'pu', [unlockQueueParam])
                IRQInstrElements.append(behaviorDecl)
        if not model.startswith('acc'):
            behaviorCode += 'return this->total_instr_cycles;'
            behaviorBody = cxx_writer.Code(behaviorCode)
            behaviorDecl = cxx_writer.Method('behavior', behaviorBody, cxx_writer.uintType, 'pu')
            IRQInstrElements.append(behaviorDecl)

        # Standard Instruction methods, there is not much to do since the IRQ instruction does nothing special
        replicateBody = cxx_writer.Code('return new ' + irq.name + 'IntrInstruction(' + instrCtorValues + ', this->' + irq.name + ');')
        replicateDecl = cxx_writer.Method('replicate', replicateBody, instructionType.makePointer(), 'pu', noException = True, const = True)
        IRQInstrElements.append(replicateDecl)
        setparamsParam = cxx_writer.Parameter('bitstring', self.bitSizes[1].makeRef().makeConst())
        setparamsDecl = cxx_writer.Method('set_params', emptyBody, cxx_writer.voidType, 'pu', [setparamsParam], noException = True)
        IRQInstrElements.append(setparamsDecl)
        getIstructionNameBody = cxx_writer.Code('return \"' + irq.name + 'IntrInstruction\";')
        getIstructionNameDecl = cxx_writer.Method('get_name', getIstructionNameBody, cxx_writer.stringType, 'pu', noException = True, const = True)
        IRQInstrElements.append(getIstructionNameDecl)
        getMnemonicBody = cxx_writer.Code('return \"' + irq.name + '\";')
        getMnemonicDecl = cxx_writer.Method('get_mnemonic', getMnemonicBody, cxx_writer.stringType, 'pu', noException = True, const = True)
        IRQInstrElements.append(getMnemonicDecl)
        getIdBody = cxx_writer.Code('return (unsigned)-1;')
        getIdDecl = cxx_writer.Method('get_id', getIdBody, cxx_writer.uintType, 'pu', noException = True, const = True)
        IRQInstrElements.append(getIdDecl)

        # Now we have all the methods related to data hazards detection and management:
        # TODO: is an implementation needed for the IRQ instruction?
        if model.startswith('acc'):
            if hasCheckHazard:
                for pipeStage in self.pipes:
                    checkHazardDecl = cxx_writer.Method('check_hazard_' + pipeStage.name, emptyBody, cxx_writer.boolType, 'pu')
                    IRQInstrElements.append(checkHazardDecl)
                    lockDecl = cxx_writer.Method('lock_regs_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'pu')
                    IRQInstrElements.append(lockDecl)
                unlockHazard = False
                for pipeStage in self.pipes:
                    if pipeStage.checkHazard:
                        unlockHazard = True
                    if unlockHazard:
                        getUnlockDecl = cxx_writer.Method('get_unlock_' + pipeStage.name, emptyBody, cxx_writer.voidType, 'pu', [unlockQueueParam])
                        IRQInstrElements.append(getUnlockDecl)

            printBusyRegsDecl = cxx_writer.Method('print_busy_regs', cxx_writer.Code('return "";'), cxx_writer.stringType, 'pu')
            IRQInstrElements.append(printBusyRegsDecl)


        # Here I add a method to specify the value of the received interrupt and the related attribute
        from isa import resolveBitType
        irqWidthType = resolveBitType('BIT<' + str(irq.portWidth) + '>')
        IRQAttribute = cxx_writer.Attribute(irq.name, irqWidthType.makeRef(), 'pu')
        IRQInstrElements.append(IRQAttribute)
        irqParams = [cxx_writer.Parameter(irq.name, irqWidthType.makeRef())]
        irqInit = [irq.name + '(' + irq.name + ')']
        InterruptValueParam = cxx_writer.Parameter('interrupt_value', irqWidthType.makeRef().makeConst())
        setInterruptValueBody = cxx_writer.Code('this->' + irq.name + ' = interrupt_value;')
        setInterruptValueDecl = cxx_writer.Method('set_interrupt_value', setInterruptValueBody, cxx_writer.voidType, 'pu', [InterruptValueParam], noException = True, inline = True)
        IRQInstrElements.append(setInterruptValueDecl)

        # Now I declare the instruction variables for this IRQ instruction
        for var in irq.variables:
            IRQInstrElements.append(cxx_writer.Attribute(var.name, var.varType, 'pro',  var.static))

        # Finally I can declare the IRQ class for this specific IRQ
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu', instrCtorParams + irqParams, ['Instruction(' + instrCtorValues + ')'] + irqInit)
        IRQInstrClass = cxx_writer.ClassDeclaration(irq.name + 'IntrInstruction', IRQInstrElements, [instructionType], namespaces = [namespace])
        IRQInstrClass.addDocString(brief = 'IRQ Instruction Class', detail = 'Wraps IRQ exception handling behavior as a dummy instruction.')
        IRQInstrClass.addConstructor(publicConstr)
        publicDestr = cxx_writer.Destructor(emptyBody, 'pu', True)
        IRQInstrClass.addDestructor(publicDestr)
        IRQInstrClasses.append(IRQInstrClass)

    return IRQInstrClasses
