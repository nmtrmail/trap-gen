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

from operator import itemgetter

try:
    import networkx as NX
except:
    import traceback
    traceback.print_exc()
    raise Exception('Cannot import required module networkx. Please install networkx >= 0.36.')
try:
    import re
    p = re.compile('([a-z]|[A-Z])*')
    nxVersion = p.sub('', NX.__version__)
    if nxVersion.count('.') > 1:
        nxVersion = '.'.join(nxVersion.split('.')[:2])
    nxVersion = float(nxVersion)
except:
    import traceback
    traceback.print_exc()
    raise Exception('Cannot determine networkx version. Try changing versions >= 0.36.')


################################################################################
# Globals and Helpers
################################################################################
aliasGraph = None
registerType = None
registerContainerType = cxx_writer.Type('Registers', '#include "registers.hpp"')
aliasType = None


################################################################################
# Register Field Index
################################################################################
def getCPPRegisterFields(self):
    """Returns a set of enum classes containing names of register fields to be
    used as register indices."""
    # NOTE: This generates an enum containing the field names for each register,
    # register bank, alias and alias register bank. This is obviously redundant,
    # since many registers might share the same mask. The reason is that we need
    # to somehow scope the masks so that different masks with the same field
    # names can co-exist (relevant for the Microblaze e.g.). Scoping field names
    # with the name of the register is not too difficult for the user - at least
    # much less error-prone than scoping via mask names, which the user might
    # get mixed up.
    registerFieldEnums = []
    for reg in self.regs + self.regBanks:
        if reg.bitMask:
            registerFieldEnum = cxx_writer.Enum(name = 'FIELDS_' + reg.name, values = {})#, superclass = cxx_writer.uintType)
            # Sort by value i.e. bit position
            for key, value in sorted(reg.bitMask.items(), key=itemgetter(1), reverse=True):
                registerFieldEnum.addValue(name = reg.name + '_' + key)
            registerFieldEnums.append(registerFieldEnum)
    return registerFieldEnums


################################################################################
# Register Defines
################################################################################
def getCPPRegisterDefines(self):
    """Returns a list of defines for getting rid of the R. syntax. Thus, a
    given REG will be replaced by R.reg. Assumes the calling object has a member
    RegisterContainer<> R; This is just a nicety to keep the old interface where
    each register was an inividual member."""
    # TODO: I obviously hate this ugly hack. But I don't like forcing the R.x
    # syntax either. I need to think of a better solution, esp since this leads
    # to cryptic compilation errors for name collisions, as was the case for
    # LEON2/LEON3 Y register, which I had to rename YREG since it collided with
    # a Y template parameter in boost! Very ugly and very hard to trace.
    registerDefines = []
    for reg in self.regs + self.regBanks + self.aliasRegs + self.aliasRegBanks:
        registerDefines.append(cxx_writer.Define('#define ' + reg.name + ' R.' + reg.name.lower()))
    return registerDefines


################################################################################
# Register Container
################################################################################
def getCPPRegisters(self, trace, combinedTrace, model, namespace):
    """Creates a container register bank for all registers, register banks,
    aliases and alias register banks of a processor. This encapsulates the
    register instantiation details (defining fields, etc) away from the
    processor. It also eases passes the registers to instructions, since only
    the container class needs to be passed.
    @see trap/runtime/modules/register/register_bank.hpp for a discussion."""

    #---------------------------------------------------------------------------
    ## @name Preprocessing
    #  @{

    # Abstraction Level
    abstractionType = cxx_writer.Type('trap::amba_layer_ids', 'register_abstraction.hpp') #'amba_parameters.h')
    abstraction = ''
    if model.startswith('acc'):
        abstraction = 'trap::amba_CT'
    elif model.startswith('func'):
        if model.endswith('AT'):
            abstraction = 'trap::amba_AT'
        else:
            abstraction = 'trap::amba_LT'

    # Register Types
    regLen = 0
    # Determine the register with the largest bitwidth.
    for reg in self.regs + self.regBanks:
        if reg.bitWidth > regLen:
            regLen = reg.bitWidth
    from isa import resolveBitType
    global registerInterfaceType, registerType, aliasType
    registerMaxBitwidth = resolveBitType('BIT<' + str(regLen) + '>')
    registerFieldType = cxx_writer.TemplateType('trap::RegisterField', [registerMaxBitwidth], 'modules/register.hpp')
    registerInterfaceType = cxx_writer.TemplateType('trap::RegisterInterface', [registerMaxBitwidth, registerFieldType], 'modules/register.hpp')
    registerType = cxx_writer.TemplateType('trap::Register', [registerMaxBitwidth], 'modules/register.hpp')
    aliasType = cxx_writer.TemplateType('trap::RegisterAlias', [registerMaxBitwidth], 'modules/register.hpp')

    # Alias Register and Alias Register Bank Initialization Order
    # Aliases that depend on each other need to be initialized in order. We
    # therefore create a dependency graph for both alias registers and alias
    # register banks.
    if nxVersion < 0.99:
        aliasUnsortedGraph = NX.XDiGraph()
    else:
        aliasUnsortedGraph = NX.DiGraph()
    for alias in self.aliasRegs + self.aliasRegBanks:
        aliasUnsortedGraph.add_node(alias)
    for alias in self.aliasRegs + self.aliasRegBanks:
        aliasPredecessors = []
        if isinstance(alias.initAlias, str):
            bracketIdx = alias.initAlias.find('[')
            if bracketIdx > 0:
                aliasPredecessors.append(alias.initAlias[:bracketIdx])
            else:
                aliasPredecessors.append(alias.initAlias)
        else:
            for aliasPredecessor in alias.initAlias:
                bracketIdx = aliasPredecessor.find('[')
                if bracketIdx > 0:
                    aliasPredecessors.append(aliasPredecessor[:bracketIdx])
                else:
                    aliasPredecessors.append(aliasPredecessor)
        for aliasPredecessor in aliasPredecessors:
            for aliasTarget in self.aliasRegs + self.aliasRegBanks:
                if aliasPredecessor == aliasTarget.name:
                    aliasUnsortedGraph.add_edge(aliasTarget, alias)
    # Check for circular dependencies.
    # NOTE: We might have some false positives here. We discarded indices for
    # banks, so a perfectly valid REGS1[x1]->REGS2[y1]; REGS2[y2]->REGS1[x2]
    # with ((x1 != x2) || (y1 != y2)) will be stored as REG1->REGS2;
    # REGS2->REGS1 and raise an error.
    # In reality, this will probably never happen anyway, since the visibility
    # of banks as a whole tends to be hierarchical.
    if not NX.is_directed_acyclic_graph(aliasUnsortedGraph):
        raise Exception('Detected circular dependence in alias initializations.')
    # Sort dependency graph.
    global aliasGraph
    aliasGraph = NX.topological_sort(aliasUnsortedGraph)

    ## @} Preprocessing
    #---------------------------------------------------------------------------
    ## @name Attributes, Constructors and Destructors
    #  @{

    from processor import extractRegInterval
    registerMembers = []
    registerCtorParams = []
    registerCtorInit = []
    registerCtorBody = ''

    # Registers
    registerCtorBody += '// Initialize registers.\n'
    for reg in self.regs:
        # Attribute Declaration
        registerMembers.append(cxx_writer.Attribute(reg.name.lower(), registerType, 'pu'))

        # Constructor Parameters
        if isinstance(reg.constValue, str):
            if reg.constValue not in [param.name for param in registerCtorParams]:
                registerCtorParams.append(cxx_writer.Parameter(reg.constValue, registerMaxBitwidth))
        # Iterable element, i.e. initialization with a constant
        # and an offset.
        if isinstance(reg.defValue, tuple):
            if reg.defValue[0] not in [param.name for param in registerCtorParams]:
                registerCtorParams.append(cxx_writer.Parameter(str(reg.defValue[0]), registerMaxBitwidth))
        elif isinstance(reg.defValue, str):
            if reg.defValue not in [param.name for param in registerCtorParams]:
                registerCtorParams.append(cxx_writer.Parameter(str(reg.defValue), registerMaxBitwidth))

        # Constructor Initialization List
        Code = '("' + reg.name.lower() + '", ' + abstraction + ', '
        if self.pipes: Code += str(len(self.pipes)) + ', '
        else: Code += '1, '
        if reg.constValue: Code += 'true, '
        else: Code += 'false, '
        if reg.offset: Code += str(reg.offset) + ', '
        else: Code += '0, '
        if reg.delay: Code += str(reg.delay) + ', '
        else: Code += '0, '
        if reg.constValue != None:
            try: Code += hex(reg.constValue)
            except TypeError: Code += str(reg.constValue)
        elif reg.defValue != None:
            # Iterable element, i.e. initialization with a constant
            # and an offset.
            if isinstance(reg.defValue, tuple):
                Code += str(reg.defValue[0]) + ' + '
                try: Code += hex(reg.defValue[1])
                except TypeError: Code += str(reg.defValue[1])
            else:
                try: Code += hex(reg.defValue)
                except TypeError: Code += str(reg.defValue)
        else: Code += '0, '
        '''# TODO: Generate clock_cycle_func for special write-back sequences.
        # stage_n.setWriteBack() should be treated as a shortcut for:
        # for reg in self.regs + self.regBanks: reg.setWbStageOrder({'stage_n': ['stage_n-1', ... 'stage_0']})
        for fromStage, toStages in reg.wbStageOrder:
            if fromStage not in self.pipes:
                raise Exception('Cannot set write-back order for register ' + reg.name + '. Pipeline stage ' + fromStage + ' does not exist.')
            for toStage in toStages:
                if toStage not in self.pipes:
                    raise Exception('Cannot set write-back order for register ' + reg.name + '. Pipeline stage ' + toStage + ' does not exist.')
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
        '''
        Code += ')'
        registerCtorInit.append(reg.name.lower() + Code)

        # Constructor Body: Add fields
        if reg.bitMask:
            for registerFieldMaskName, registerFieldMaskPos in reg.bitMask.items():
                registerCtorBody += reg.name.lower() + '.add_field("' + registerFieldMaskName + '", ' + str(registerFieldMaskPos[1]) + ', ' + str(registerFieldMaskPos[0]) + ');\n'
            registerCtorBody += '\n'

    # Register Banks
    registerCtorBody += '// Initialize register banks.\n'
    for regBank in self.regBanks:
        # Attribute Declaration
        registerMembers.append(cxx_writer.Attribute(regBank.name.lower() + '[' + str(regBank.numRegs) + ']', registerType, 'pu'))

        # Constructor Parameters
        for regConstValue in regBank.constValue.values():
            if isinstance(regConstValue, str):
                if regConstValue not in [param.name for param in registerCtorParams]:
                    registerCtorParams.append(cxx_writer.Parameter(regConstValue, registerMaxBitwidth))
        for regDefaultValue in regBank.defValues:
            # Iterable element, i.e. initialization with a constant
            # and an offset.
            if isinstance(regDefaultValue, tuple):
                if regDefaultValue[0] not in [param.name for param in registerCtorParams]:
                    registerCtorParams.append(cxx_writer.Parameter(str(regDefaultValue[0]), registerMaxBitwidth))
            elif isinstance(regDefaultValue, str):
                if regDefaultValue not in [param.name for param in registerCtorParams]:
                    registerCtorParams.append(cxx_writer.Parameter(str(regDefaultValue), registerMaxBitwidth))

        # Constructor Initialization List
        Code = ''
        for reg in range(0, regBank.numRegs):
            Code += '{"' + regBank.name.lower() + '[' + str(reg) + ']", ' + abstraction + ', '
            if self.pipes: Code += str(len(self.pipes)) + ', '
            else: Code += '1, '
            if regBank.constValue.has_key(reg): Code += 'true, '
            else: Code += 'false, '
            if regBank.offset: Code += str(regBank.offset) + ', '
            else: Code += '0, '
            if regBank.delay.has_key(reg): Code += str(regBank.delay[reg]) + ', '
            else: Code += '0, '
            if regBank.constValue.has_key(reg):
                try: Code += hex(regBank.constValue[reg])
                except TypeError: Code += str(regBank.constValue[reg])
            elif regBank.defValues[reg] != None:
                # Iterable element, i.e. initialization with a constant
                # and an offset.
                if isinstance(regBank.defValues[reg], tuple):
                    Code += str(regBank.defValues[reg][0]) + ' + '
                    try: Code += hex(regBank.defValues[reg][1])
                    except TypeError: Code += str(regBank.defValues[reg][1])
                else:
                    try: Code += hex(regBank.defValues[reg])
                    except TypeError: Code += str(regBank.defValues[reg])
            else: Code += '0, '
            Code += '},\n'
        registerCtorInit.append(regBank.name.lower() + ' {' + Code[:-2] + '}')

        # Constructor Body: Add fields.
        if regBank.bitMask:
            registerCtorBody += 'for (unsigned i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'

            for registerFieldMaskName, registerFieldMaskPos in regBank.bitMask.items():
                registerCtorBody += regBank.name.lower() + '[i].add_field("' + registerFieldMaskName + '", ' + str(registerFieldMaskPos[1]) + ', ' + str(registerFieldMaskPos[0]) + ');\n'

            registerCtorBody += '}\n\n'

    # Alias Registers
    for alias in self.aliasRegs:
        # Attribute Declaration
        #aliasType = cxx_writer.TemplateType('std::reference_wrapper', [registerType], 'functional')
        #aliasBankType = cxx_writer.TemplateType('std::vector', [aliasType], 'vector')
        registerMembers.append(cxx_writer.Attribute(alias.name.lower(), aliasType, 'pu'))

        # Constructor Initialization List
        registerCtorInit.append(alias.name.lower() + '("' + alias.name.lower() + '")')

    # Alias Register Banks
    for aliasBank in self.aliasRegBanks:
        # Attribute Declaration
        registerMembers.append(cxx_writer.Attribute(aliasBank.name.lower() + '[' + str(aliasBank.numRegs) + ']', aliasType, 'pu'))

        # Constructor Initialization List
        Code = ''
        for aliasIdx in range(0, aliasBank.numRegs):
            Code += '{"' + aliasBank.name.lower() + '[' + str(aliasIdx) + ']"},'
        registerCtorInit.append(aliasBank.name.lower() + ' {' + Code[:-1] + '}')

    # Alias Registers and Alias Register Banks
    # Constructor Body: Update alias targets.
    registerCtorBody += '// Initialize alias registers and alias register banks.\n'
    for alias in aliasGraph:
        if isinstance(alias.initAlias, str):
            regName = alias.initAlias[:alias.initAlias.find('[')]
            regRange = extractRegInterval(alias.initAlias)
            # REGBANK[n], REGBANK[n:m], REGBANK
            if regRange or regName in self.regBanks + self.aliasRegBanks:
                aliasIdx = 0
                if regRange:
                    regStart = regRange[0]
                    regEnd = regRange[1]+1
                else:
                    regStart = 0
                    regEnd = reg.numRegs
                for regIdx in range(regStart, regEnd):
                    registerCtorBody += 'this->' + alias.name.lower()
                    if alias in self.aliasRegBanks:
                        registerCtorBody += '[' + str(aliasIdx) + ']'
                    registerCtorBody += '.update_alias(this->' + regName.lower() + '[' + str(regIdx) + ']'
                    if alias in self.aliasRegs:
                        registerCtorBody += ', ' + str(alias.offset)
                    elif alias.offsets.has_key(aliasIdx):
                        registerCtorBody += ', ' + str(alias.offsets[aliasIdx])
                    else:
                        registerCtorBody += ', 0'
                    registerCtorBody += ');\n'
                    aliasIdx = aliasIdx + 1
            # REG
            else:
                registerCtorBody += 'this->' + alias.name.lower() + '.update_alias(this->' + regName.lower()
                if alias in self.aliasRegs:
                    registerCtorBody += ', ' + str(alias.offset)
                elif alias.offsets.has_key(aliasIdx):
                    registerCtorBody += ', ' + str(alias.offsets[aliasIdx])
                else:
                    registerCtorBody += ', 0'
                registerCtorBody += ');\n'
        else:
            aliasIdx = 0
            for reg in alias.initAlias:
                regName = reg[:reg.find('[')]
                regRange = extractRegInterval(reg)
                # REGBANK[n], REGBANK[n:m], REGBANK
                if regRange or regName in self.regBanks + self.aliasRegBanks:
                    if regRange:
                        regStart = regRange[0]
                        regEnd = regRange[1]+1
                    else:
                        regStart = 0
                        regEnd = reg.numRegs
                    for regIdx in range(regStart, regEnd):
                        registerCtorBody += 'this->' + alias.name.lower() + '[' + str(aliasIdx) + '].update_alias(this->' + regName.lower() + '[' + str(regIdx) + ']'
                        if alias.offsets.has_key(aliasIdx):
                            registerCtorBody += ', ' + str(alias.offsets[aliasIdx])
                        else:
                            registerCtorBody += ', 0'
                        registerCtorBody += ');\n'
                        aliasIdx = aliasIdx + 1
                # REG
                else:
                    registerCtorBody += 'this->' + alias.name.lower() + '[' + str(aliasIdx) + '].update_alias(this->' + regName.lower()
                    if alias.offsets.has_key(aliasIdx):
                        registerCtorBody += ', ' + str(alias.offsets[aliasIdx])
                    else:
                        registerCtorBody += ', 0'
                    registerCtorBody += ');\n'
                    aliasIdx = aliasIdx + 1

    registerCtor = cxx_writer.Constructor(cxx_writer.Code(registerCtorBody), 'pu', parameters = registerCtorParams, initList = registerCtorInit)

    ## @} Attributes, Constructors and Destructors
    #---------------------------------------------------------------------------
    ## @name Methods
    #  Access and Modification: reset(), write(), write_dbg(), write_force()
    #  Observer: execute_callbacks()
    #  Information and Helper: operator<<()
    #  @{

    # Method Bodies: Registers
    registerResetBody = '// Reset registers.\n'
    registerWriteBody = 'bool ret = true;\n\n// Write registers.\n'
    registerWriteDbgBody = 'bool ret = true;\n\n// Write registers.\n'
    registerWriteForceBody = 'bool ret = true;\n\n// Write registers.\n'
    registerExecuteCallbacksBody = '// Execute callbacks on registers.\n'
    registerPrintBody = 'os << std::hex << std::showbase;\n\n// Print registers.\n'
    for reg in self.regs:
        if reg.constValue == None:
            registerResetBody += reg.name.lower() + '.reset();\n'
            registerWriteBody += 'ret = ret && ' + reg.name.lower() + '.write(data);\n'
            registerWriteDbgBody += 'ret = ret && ' + reg.name.lower() + '.write_dbg(data);\n'
            registerWriteForceBody += 'ret = ret && ' + reg.name.lower() + '.write_force(data);\n'
        registerExecuteCallbacksBody += reg.name.lower() + '.execute_callbacks(type, 0, sizeof(' + registerMaxBitwidth.name.lower() + '));\n'
        registerPrintBody += 'os << ' + reg.name.lower() + '.name() << ": " << ' + reg.name.lower() + '.read_dbg() << \'\\n\';\n'

    # Method Bodies: Register Banks
    registerResetBody += '\n// Reset register banks.\n'
    registerWriteBody += '\n// Write register banks.\n'
    registerWriteDbgBody += '\n// Write register banks.\n'
    registerWriteForceBody += '\n// Write register banks.\n'
    registerExecuteCallbacksBody += '\n// Execute callbacks on register banks.\n'
    registerPrintBody += '\n// Print register banks.\n'
    for regBank in self.regBanks:
        registerResetBody += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerResetBody += regBank.name.lower() + '[i].reset();\n'
        registerResetBody += '}\n\n'
        registerWriteBody += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerWriteBody += 'ret = ret && ' + regBank.name.lower() + '[i].write(data);\n'
        registerWriteBody += '}\n\n'
        registerWriteDbgBody += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerWriteDbgBody += 'ret = ret && ' + regBank.name.lower() + '[i].write_dbg(data);\n'
        registerWriteDbgBody += '}\n\n'
        registerWriteForceBody += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerWriteForceBody += 'ret = ret && ' + regBank.name.lower() + '[i].write_force(data);\n'
        registerWriteForceBody += '}\n\n'
        registerExecuteCallbacksBody += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerExecuteCallbacksBody += regBank.name.lower() + '[i].execute_callbacks(type, 0, sizeof(' + registerMaxBitwidth.name.lower() + '));\n'
        registerExecuteCallbacksBody += '}\n\n'
        registerPrintBody += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerPrintBody += 'os << ' + regBank.name.lower() + '[i].name() << ": " << ' + regBank.name.lower() + '[i].read_dbg() << \'\\n\';\n'
        registerPrintBody += '}\n\n'
    registerWriteBody += 'return ret;\n'
    registerWriteDbgBody += 'return ret;\n'
    registerWriteForceBody += 'return ret;\n'
    registerPrintBody += 'os << std::dec;\nreturn os;\n'

    # Method Declarations
    registerResetMethod = cxx_writer.Method('reset', cxx_writer.Code(registerResetBody), cxx_writer.voidType, 'pu')
    registerMembers.append(registerResetMethod)

    registerWriteDataParam = cxx_writer.Parameter('data', registerMaxBitwidth.makeConst().makeRef())
    registerWriteMethod = cxx_writer.Method('write', cxx_writer.Code(registerWriteBody), cxx_writer.boolType, 'pu', [registerWriteDataParam])
    registerMembers.append(registerWriteMethod)
    registerWriteDbgMethod = cxx_writer.Method('write_dbg', cxx_writer.Code(registerWriteDbgBody), cxx_writer.boolType, 'pu', [registerWriteDataParam])
    registerMembers.append(registerWriteDbgMethod)
    registerWriteForceMethod = cxx_writer.Method('write_force', cxx_writer.Code(registerWriteForceBody), cxx_writer.boolType, 'pu', [registerWriteDataParam])
    registerMembers.append(registerWriteForceMethod)

    registerExecuteCallbacksUint32RefType = cxx_writer.Type('uint32_t', const = True).makeRef()
    registerExecuteCallbacksMethod = cxx_writer.Method('execute_callbacks', cxx_writer.Code(registerExecuteCallbacksBody), cxx_writer.voidType, 'pu',
      [cxx_writer.Parameter('type', cxx_writer.Type('scireg_ns::scireg_callback_type', const = True).makeRef()),
      cxx_writer.Parameter('offset', registerExecuteCallbacksUint32RefType, initValue = '0'),
      cxx_writer.Parameter('size', cxx_writer.Type('uint32_t', const = True).makeRef(), initValue = '0')])
    registerMembers.append(registerExecuteCallbacksMethod)

    registerPrintOstreamRefType = cxx_writer.Type('std::ostream', 'iostream').makeRef()
    registerPrintMethod = cxx_writer.MemberOperator('<<', cxx_writer.Code(registerPrintBody), registerPrintOstreamRefType, 'pu',
      [cxx_writer.Parameter('os', registerPrintOstreamRefType)], const = True)
    registerMembers.append(registerPrintMethod)

    ## @} Methods
    #---------------------------------------------------------------------------

    registerClass = cxx_writer.ClassDeclaration('Registers', registerMembers, namespaces = [namespace])
    registerClass.addDocString(brief = 'Register Container Class', detail = 'Contains all registers and register banks of the processor as member variables. It serves for encapsulating the instantiation details (defining fields, etc) away from the processor. It also simplifies passing the registers to the instructions, instead of passing each register individually.')
    registerClass.addConstructor(registerCtor)
    return registerClass

################################################################################
