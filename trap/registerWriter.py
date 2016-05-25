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
        Code += ')'
        registerCtorInit.append(reg.name.lower() + Code)

        # Constructor Body: Add fields
        if reg.bitMask:
            for registerFieldMaskName, registerFieldMaskPos in reg.bitMask.items():
                registerCtorBody += reg.name.lower() + '.add_field("' + registerFieldMaskName + '", ' + str(registerFieldMaskPos[1]) + ', ' + str(registerFieldMaskPos[0]) + ');\n'
            registerCtorBody += '\n'

    '''#TODO: Pipeline
    pipeRegisterType = cxx_writer.Type('PipelineRegister', '#include \"registers.hpp\"')
    for reg in self.regs:
        bodyInits += 'this->' + reg.name + '_pipe.set_register(&' + reg.name + ');\n'
        pipeCount = 0
        for pipeStage in self.pipes:
            attribute = cxx_writer.Attribute(reg.name + '_' + pipeStage.name, registerType, 'pu')
            processorElements.append(attribute)
            bodyInits += 'this->' + reg.name + '_pipe.set_register(&' + reg.name + '_' + pipeStage.name + ', ' + str(pipeCount) + ');\n'
            pipeCount += 1
            bodyInits += reg.name + '_' + pipeStage.name + ' = ' + reg.name + ';\n'
        bodyInits += reg.name + '_pipe.has_to_propagate = false;\n'
        if reg.wbStageOrder:
            # The atribute is of a special type since write back has to be performed in
            # a special order
            customPipeRegisterType = cxx_writer.Type('PipelineRegister_' + str(reg.wbStageOrder)[1:-1].replace(', ', '_').replace('\'', ''), '#include \"registers.hpp\"')
            attribute = cxx_writer.Attribute(reg.name + '_pipe', customPipeRegisterType, 'pu')
        else:
            attribute = cxx_writer.Attribute(reg.name + '_pipe', pipeRegisterType, 'pu')
        processorElements.append(attribute)
    '''

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

    '''#TODO: Pipeline
    for regB in self.regBanks:
        attribute = cxx_writer.Attribute(regB.name + '[' + str(regB.numRegs) + ']', resourceType[regB.name].makeNormal(), 'pu')
        processorElements.append(attribute)
        if model.startswith('acc'):
            for pipeStage in self.pipes:
                attribute = cxx_writer.Attribute(regB.name + '_' + pipeStage.name + '[' + str(regB.numRegs) + ']', resourceType[regB.name].makeNormal(), 'pu')
                processorElements.append(attribute)
            attribute = cxx_writer.Attribute(regB.name + '_pipe[' + str(regB.numRegs) + ']',  pipeRegisterType, 'pu')
            processorElements.append(attribute)
        bodyInits += 'for (int i = 0; i < ' + str(regB.numRegs) + '; i++) {\n'
        pipeCount = 0
        for pipeStage in self.pipes:
            bodyInits += 'this->' + regB.name + '_pipe[i].set_register(&' + regB.name + '_' + pipeStage.name + '[i], ' + str(pipeCount) + ');\n'
            pipeCount += 1
        bodyInits += 'this->' + regB.name + '_pipe[i].set_register(&' + regB.name + '[i]);\n'
        bodyInits += '}\n'

        initString += 'for (int i = 0; i < ' + str(regB.numRegs) + '; i++) {\n'
        for pipeStage in self.pipes:
            initString += regB.name + '_' + pipeStage.name + '[i] = ' + regB.name + '[i];\n'
        initString += regB.name + '_pipe[i].has_to_propagate = false;\n'
        initString += '}\n'
    '''

    # Alias Registers
    for alias in self.aliasRegs:
        # Attribute Declaration
        #aliasType = cxx_writer.TemplateType('std::reference_wrapper', [registerType], 'functional')
        #aliasBankType = cxx_writer.TemplateType('std::vector', [aliasType], 'vector')
        registerMembers.append(cxx_writer.Attribute(alias.name.lower(), aliasType, 'pu'))

        # Constructor Initialization List
        registerCtorInit.append(alias.name.lower() + '("' + alias.name.lower() + '")')

    '''#TODO: Pipeline
    for alias in self.aliasRegs:
        if model.startswith('acc'):
            curPipeNum = 0
            for pipeStage in self.pipes:
                attribute = cxx_writer.Attribute(alias.name + '_' + pipeStage.name, resourceType[alias.name], 'pu')
                processorElements.append(attribute)
                bodyInits += alias.name + '_' + pipeStage.name + '.set_pipe_id(' + str(curPipeNum) + ');\n'
                curPipeNum += 1
            curStageId = 0
            for pipeStage in self.pipes:
                aliasInitStr = alias.name + '_' + pipeStage.name + '(' + str(curStageId)
                if alias.initAlias.find('[') > -1:
                    aliasInitStr += ', &' + alias.initAlias[:alias.initAlias.find('[')] + '_pipe' + alias.initAlias[alias.initAlias.find('['):]
                else:
                    aliasInitStr += ', &' + alias.initAlias
                aliasInit[alias.name + '_' + pipeStage.name] = (aliasInitStr + ')')
                curStageId += 1
    '''

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

    '''#TODO: Pipeline
    regsNames = [i.name for i in self.regBanks + self.regs]
    bodyInits += '// Initialize the alias registers (plain and banks).\n'
    for aliasB in self.aliasRegBanks:
        bodyAliasInit[aliasB.name] = ''
        if model.startswith('acc'):
            bodyAliasInit[aliasB.name] += 'for (int i = 0; i < ' + str(aliasB.numRegs) + '; i++) {\n'
            curStageId = 0
            for pipeStage in self.pipes:
                attribute = cxx_writer.Attribute(aliasB.name + '_' + pipeStage.name + '[' + str(aliasB.numRegs) + ']', resourceType[aliasB.name], 'pu')
                processorElements.append(attribute)
                bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '_' + pipeStage.name + '[i].set_pipe_id(' + str(curStageId) + ');\n'
                curStageId += 1
            bodyAliasInit[aliasB.name] += '}\n'
        else:
            attribute = cxx_writer.Attribute(aliasB.name + '[' + str(aliasB.numRegs) + ']', resourceType[aliasB.name], 'pu')
            processorElements.append(attribute)
        # Lets now deal with the initialization of the single elements of the regBank
        if isinstance(aliasB.initAlias, str):
            index = extractRegInterval(aliasB.initAlias)
            curIndex = index[0]
            if model.startswith('acc'):
                bodyAliasInit[aliasB.name] += 'for (int  i = 0; i < ' + str(aliasB.numRegs) + '; i++) {\n'
                for pipeStage in self.pipes:
                    offsetStr = ''
                    if index[0] != 0:
                        offsetStr = ' + ' + str(index[0])
                    if aliasB.initAlias[:aliasB.initAlias.find('[')] in regsNames:
                        bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '_' + pipeStage.name + '[i].update_alias(this->' + aliasB.initAlias[:aliasB.initAlias.find('[')] + '_pipe[i' + offsetStr + ']);\n'
                    else:
                        bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '_' + pipeStage.name + '[i].update_alias(this->' + aliasB.initAlias[:aliasB.initAlias.find('[')] + '_' + pipeStage.name + '[i' + offsetStr + ']);\n'
                bodyAliasInit[aliasB.name] += '}\n'
            else:
                for i in range(0, aliasB.numRegs):
                    bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '[' + str(i) + '].update_alias(this->' + aliasB.initAlias[:aliasB.initAlias.find('[')] + '[' + str(curIndex) + ']'
                    if aliasB.offsets.has_key(i):
                        bodyAliasInit[aliasB.name] += ', ' + str(aliasB.offsets[i])
                    bodyAliasInit[aliasB.name] += ');\n'
                    curIndex += 1
        else:
            if model.startswith('acc'):
                curIndex = 0
                for curAlias in aliasB.initAlias:
                    index = extractRegInterval(curAlias)
                    if index:
                        offsetStr = ''
                        if index[0] != 0:
                            offsetStr = ' + ' + str(index[0])
                        indexStr = ''
                        if curIndex != 0:
                            indexStr = ' + ' + str(curIndex)
                        bodyAliasInit[aliasB.name] += 'for (int i = 0; i < ' + str(index[1] + 1 - index[0]) + '; i++) {\n'
                        for pipeStage in self.pipes:
                            if curAlias[:curAlias.find('[')] in regsNames:
                                bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '_' + pipeStage.name + '[i' + indexStr + '].update_alias(this->' + curAlias[:curAlias.find('[')] + '_pipe[i' + offsetStr + ']);\n'
                            else:
                                bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '_' + pipeStage.name + '[i' + indexStr + '].update_alias(this->' + curAlias[:curAlias.find('[')] + '_' + pipeStage.name +'[i' + offsetStr + ']);\n'
                        bodyAliasInit[aliasB.name] += '}\n'
                        curIndex += index[1] + 1 - index[0]
                    else:
                        if curAlias in regsNames:
                            bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '_' + pipeStage.name + '[' + str(curIndex) + '].update_alias(this->' + curAlias + '_pipe);\n'
                        else:
                            bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '_' + pipeStage.name + '[' + str(curIndex) + '].update_alias(this->' + curAlias + '_' + pipeStage.name + ');\n'
                        curIndex += 1
            else:
                curIndex = 0
                for curAlias in aliasB.initAlias:
                    index = extractRegInterval(curAlias)
                    if index:
                        for curRange in range(index[0], index[1] + 1):
                            bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '[' + str(curIndex) + '].update_alias(this->' + curAlias[:curAlias.find('[')] + '[' + str(curRange) + ']'
                            if aliasB.offsets.has_key(curIndex):
                                bodyAliasInit[aliasB.name] += ', ' + str(aliasB.offsets[curIndex])
                            bodyAliasInit[aliasB.name] += ');\n'
                            curIndex += 1
                    else:
                        bodyAliasInit[aliasB.name] += 'this->' + aliasB.name + '[' + str(curIndex) + '].update_alias(this->' + curAlias
                        if aliasB.offsets.has_key(curIndex):
                            bodyAliasInit[aliasB.name] += ', ' + str(aliasB.offsets[curIndex])
                        bodyAliasInit[aliasB.name] += ');\n'
                        curIndex += 1

        if index:
            # we are dealing with a member of a register bank
            curIndex = index[0]
            registerCtorInitAlias[alias.name] = ''
            for pipeStage in self.pipes:
                if alias.initAlias[:alias.initAlias.find('[')] in regsNames:
                    registerCtorInitAlias[alias.name] += 'this->' + alias.name + '_' + pipeStage.name + '.update_alias(this->' + alias.initAlias[:alias.initAlias.find('[')] + '_pipe[' + str(curIndex) + ']);\n'
                else:
                    registerCtorInitAlias[alias.name] += 'this->' + alias.name + '_' + pipeStage.name + '.update_alias(this->' + alias.initAlias[:alias.initAlias.find('[')] + '_' + pipeStage.name + '[' + str(curIndex) + ']);\n'
        else:
            for pipeStage in self.pipes:
                if alias.initAlias in regsNames:
                    registerCtorInitAlias[alias.name] += 'this->' + alias.name + '_' + pipeStage.name + '.update_alias(this->' + alias.initAlias + '_pipe);\n'
                else:
                    registerCtorInitAlias[alias.name] += 'this->' + alias.name + '_' + pipeStage.name + '.update_alias(this->' + alias.initAlias + '_' + pipeStage.name + ');\n'
                        '''
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

    '''#TODO: Pipeline
    if model.startswith('acc'):
        for reg in self.regs:
            for pipeStage in self.pipes:
                registerResetBody += reg.name + '_' + pipeStage.name + ' = ' + reg.name + ';\n'
            registerResetBody += reg.name + '_pipe.has_to_propagate = false;\n'
        for regB in self.regBanks:
            registerResetBody += 'for (int i = 0; i < ' + str(regB.numRegs) + '; i++) {\n'
            for pipeStage in self.pipes:
                registerResetBody += regB.name + '_' + pipeStage.name + '[i] = ' + regB.name + '[i];\n'
            registerResetBody += regB.name + '_pipe[i].has_to_propagate = false;\n'
            registerResetBody += '}\n'
    '''

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
# PipelineRegister
################################################################################
# TODO
"""Returns the pipeline registers, which are special registers containing both
the value of the registers themselves as well as all the pipeline latches.
It also defines special methods for propagating registers values in the
pipeline.
Note that there are different kinds of such registers, one for the normal
latched registers and one for the registers which are immediately visible to all
the other stages (e.g. for LEON they are the PC and NPC registers)."""
def getCPPPipelineReg(self, trace, combinedTrace, namespace):
    # Lets start with the creation of the latched registers: they have exactly
    # the same methods of the base registers
    pipelineRegClasses = []
    registerElements = []

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

    stagesRegsAttr = cxx_writer.Attribute('reg_stage[' + str(len(self.pipes)) + ']', registerType.makePointer(), 'pro')
    registerElements.append(stagesRegsAttr)
    generalRegAttr = cxx_writer.Attribute('reg_all', registerType.makePointer(), 'pro')
    registerElements.append(generalRegAttr)
    propagateAttr = cxx_writer.Attribute('has_to_propagate', cxx_writer.boolType, 'pu')
    registerElements.append(propagateAttr)

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
    forceValueParam = [cxx_writer.Parameter('value', registerType.makeRef().makeConst())]
    forceValueMethod = cxx_writer.Method('force_value', forceValueBody, cxx_writer.voidType, 'pu', forceValueParam, noException = True)
    registerElements.append(forceValueMethod)

    writeAllCode = '*(this->reg_all) = value;\n'
    writeAllCode += 'for (int i = 0; i < ' + str(len(self.pipes)) + '; i++) {\n'
    writeAllCode += '*(this->reg_stage[i]) = value;\n}\n'
    writeAllBody = cxx_writer.Code(writeAllCode)
    writeAllParam = [cxx_writer.Parameter('value', registerType.makeRef().makeConst())]
    writeAllMethod = cxx_writer.Method('write_all', writeAllBody, cxx_writer.voidType, 'pu', writeAllParam, noException = True)
    registerElements.append(writeAllMethod)
    immediateWriteBody = cxx_writer.Code('this->reg_all->write_force(value);')
    immediateWriteParam = [cxx_writer.Parameter('value', registerType.makeRef().makeConst())]
    immediateWriteMethod = cxx_writer.Method('write_force', immediateWriteBody, cxx_writer.voidType, 'pu', immediateWriteParam, noException = True)
    registerElements.append(immediateWriteMethod)
    readNewValueBody = cxx_writer.Code('return this->reg_all->read_force();')
    readNewValueMethod = cxx_writer.Method('read_force', readNewValueBody, registerType, 'pu', noException = True)
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
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, registerType, 'pu', noException = True)
        registerElements.append(operatorDecl)
    for i in binaryOps:
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorBody = cxx_writer.Code('return (*(this->reg_all)) ' + i + ' other;')
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, registerType, 'pu', [operatorParam], const = True, noException = True)
        registerElements.append(operatorDecl)
    for i in comparisonOps:
        operatorParam = cxx_writer.Parameter('other', registerType.makeRef().makeConst())
        operatorBody = cxx_writer.Code('return (*(this->reg_all)) ' + i + ' other;')
        operatorDecl = cxx_writer.MemberOperator(i, operatorBody, cxx_writer.boolType, 'pu', [operatorParam], const = True, noException = True)
        registerElements.append(operatorDecl)

    pureDeclTypes = [registerType, registerType]
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
    operatorIntDecl = cxx_writer.MemberOperator(str(registerType), operatorBody, cxx_writer.Type(''), 'pu', const = True, noException = True)
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

################################################################################
