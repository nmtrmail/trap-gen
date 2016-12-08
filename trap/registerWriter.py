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

def getPipeClockCycleFunction(self, registerMaxBitwidth):
    """In the accurate (pipelined) model, register values are propagated every
    cycle according to the three following specifications:

    1. Default behavior:
       a. Propagation to stages after wb:
          wbStage is considered the stable value and fed forward to all stages
          from [wbStage+1..lastStage]. This is only for consistency, since
          registers should obviously neither be read nor written after wbStage
          (last write).
       b. Propagation to stages before and up to regsStage:
          wbStage is also fed back to all stages from [0..regsStage]. This is
          only for consistency, since registers should obviously neither be read
          nor written before regsStage (first read).
       c. Regular step-wise forward propagation:
          Each register from regsStage to wbStage propagates one stage forward
          every cycle. A stalled pipeline is split into three sections: The
          stalled stages (ideally [0..regsStage]) retain their values, the
          following stages containing bubbles (NOPs) either retain their stale
          values or explicitly reset them (they will be overwritten anyway), the
          non-stalled stages propagate normally.

    2. Common feedback loops via <processor>Arch.py:<pipe>.setWbStageOrder():
       Aside from the default behavior in (1), TRAP interprets <pipe>.
       setWbStageOrder() as a backward path from each stage in the list back to
       <pipe> for all registers. Hence, the default behavior still executes but
       the pipe forwarding paths take precedence in case of contention. If the
       forwarding stage is stalled, the bypass is deactivated.

    3. Register-specific feedback loops via <reg>.setWbStageOrder():
       the common pipe behavior in (1) and (2) does not run except for the step-
       wise forward propagation, in which case all stages propagate, not just
       [regsStage..wbStage] If <reg>.setWbStageOrder() is used, care should
       therefore be taken to re-define all feedback loops as required. If the
       forwarding stage is stalled, the bypass is deactivated.

    This function generates the callback function containing default propagation
    logic (1) as well as any forwarding specified by <pipe>.setWbStageOrder(
    ['pipe_x', ... 'pipe_0']) as described in (2). The list of stages is
    interpreted in order of decreasing precedence. The calling sequence is:
       pipeline.cpp:            <fetchStage>::behavior()
    -> registers.cpp:           clock_cycle()
    -> register_abstraction.hpp:clock_cycle()
    -> registers.cpp:           regs_clock_cycle()
    The fetch stage calls a clock_cycle function which iterates through all
    registers and calls their clock_cycle() method defined in
    register_abstraction.hpp. This, in turn, calls the callback function
    regs_clock_cycle, generated by TRAP in the registers.cpp file."""

    if not self.pipes: return

    regsStage = 0
    wbStage = len(self.pipes) - 1
    pipeNumbers = {}
    i = 0
    for pipeStage in self.pipes:
        if pipeStage.regsStage:
            regsStage = self.pipes.index(pipeStage)
        if pipeStage.wbStage:
            wbStage = self.pipes.index(pipeStage)
        pipeNumbers[pipeStage.name] = i
        i += 1

    Code = """bool has_changes = false;

    // Copy over old values, just in case stalls should prevent propagation.
    for (unsigned i = 0; i < """ + str(len(self.pipes)) + """; ++i) {
      temp_values[i] = values[i];
      temp_time_stamps[i] = time_stamps[i];
    }
    """

    # 1a. Default behavior: Propagation to stages after wb.
    if wbStage == len(self.pipes) - 2:
      Code += """
        // Feed stable value to all stages after write-back. This is only for consistency.
        if (!(nop_stages & """ + hex(1 << wbStage) + """)) {
          if (time_stamps[""" + str(wbStage) + """] >= time_stamps[""" + str(wbStage+1) + """]) {
            temp_values[""" + str(wbStage+1) + """] = values[""" + str(wbStage) + """];
            temp_time_stamps[""" + str(wbStage+1) + """] = time_stamps[""" + str(wbStage) + """];
            has_changes = true;
          }
        }
        """
    elif wbStage < len(self.pipes) - 2:
      Code += """
        // Feed stable value to all stages after write-back. This is only for consistency.
        if (!(nop_stages & """ + hex(1 << wbStage) + """)) {
          for (unsigned i = """ + str(wbStage+1) + """; i < this->m_num_stages; ++i) {
            if (time_stamps[""" + str(wbStage) + """] >= time_stamps[i]) {
              temp_values[i] = values[""" + str(wbStage) + """];
              temp_time_stamps[i] = time_stamps[""" + str(wbStage) + """];
              has_changes = true;
            }
          }
        }
        """

    # 1b. Default behavior: Propagation to stages before and up to regsStage.
    if regsStage == 0:
      Code += """
        // Feed stable value back to first-read.
        if (!(cur_nop_stages & """ + hex(1 << wbStage) + """)) {
          if (time_stamps[""" + str(wbStage) + """] >= time_stamps[0]) {
            temp_values[0] = values[""" + str(wbStage) + """];
            temp_time_stamps[0] = time_stamps[""" + str(wbStage) + """];
            has_changes = true;
          }
        }
        """
    elif regsStage > 1:
      Code += """
        // Feed stable value to first-read and all stages before. The latter is only for consistency.
        if (!(cur_nop_stages & """ + hex(1 << wbStage) + """)) {
          for (unsigned i = 0; i <= """ + str(regsStage) + """; ++i) {
            if (time_stamps[""" + str(wbStage) + """] >= time_stamps[i]) {
              temp_values[i] = values[""" + str(wbStage) + """];
              temp_time_stamps[i] = time_stamps[""" + str(wbStage) + """];
              has_changes = true;
            }
          }
        }
        """

    # 1c. Default Behavior: Regular step-wise forward propagation.
    Code += """
      // Regular step-wise forward propagation.
      for (int i = """ + str(regsStage+1) + """; i <= """ + str(wbStage) + """; ++i) {
        // The stage will contain a bubble.
        if (next_nop_stages & (1 << i)) {
          temp_values[i] = 0;
          temp_time_stamps[i] = 0;
        } else if ((i > stall_stage) && (time_stamps[i-1] >= time_stamps[i])) {
          temp_values[i] = values[i-1];
          temp_time_stamps[i] = time_stamps[i-1];
          has_changes = true;
        }
      }
      """

    # 2. Common feedback loops via <processor>Arch.py:<pipe>.setWbStageOrder().
    for toStage in self.pipes:
        if toStage.wbStageOrder:
            for fromStage in reversed(toStage.wbStageOrder):
                Code += """
                  // Feedback path """ + fromStage + """ -> """ + toStage.name + """
                  if (!(cur_nop_stages & """ + hex(1 << pipeNumbers[fromStage]) + """)) {
                    if (time_stamps[""" + str(pipeNumbers[fromStage]) + """] >= temp_time_stamps[""" + str(pipeNumbers[toStage.name]) + """]) {
                      temp_values[""" + str(pipeNumbers[toStage.name]) + """] = values[""" + str(pipeNumbers[fromStage]) + """];
                      temp_time_stamps[""" + str(pipeNumbers[toStage.name]) + """] = time_stamps[""" + str(pipeNumbers[fromStage]) + """];
                        has_changes = true;
                    }
                  }
                  """

    Code += '\nreturn has_changes;\n'
    pipeClockCycleValueParam = cxx_writer.Parameter('values', registerMaxBitwidth.makePointer())
    pipeClockCycleTempValueParam = cxx_writer.Parameter('temp_values', registerMaxBitwidth.makePointer())
    pipeClockCycleTimeStampParam = cxx_writer.Parameter('time_stamps', cxx_writer.ulonglongPtrType)
    pipeClockCycleTempTimeStampParam = cxx_writer.Parameter('temp_time_stamps', cxx_writer.ulonglongPtrType)
    pipeClockCycleStallStageParam = cxx_writer.Parameter('stall_stage', cxx_writer.intType)
    pipeClockCycleCurNopStagesParam = cxx_writer.Parameter('cur_nop_stages', cxx_writer.uintType)
    pipeClockCycleNextNopStagesParam = cxx_writer.Parameter('next_nop_stages', cxx_writer.uintType)
    pipeClockCycleFunction = cxx_writer.Function('regs_clock_cycle', cxx_writer.Code(Code), cxx_writer.boolType, [pipeClockCycleValueParam, pipeClockCycleTempValueParam, pipeClockCycleTimeStampParam, pipeClockCycleTempTimeStampParam, pipeClockCycleStallStageParam, pipeClockCycleCurNopStagesParam, pipeClockCycleNextNopStagesParam])

    return pipeClockCycleFunction

def getRegisterClockCycleFunction(self, regName, wbStageOrder, registerMaxBitwidth):
    """Generates the callback function set by
    <reg>.setWbStageOrder({'pipe_n': ['pipe_x', ... 'pipe_0']}))
    as described in @see getPipeClockCycleFunction (3). The list of stages is
    interpreted in order of decreasing precedence. Note that step-wise forward
    propagation (1c) still takes place, but the feed-forward/-back from wb is
    not implicitly assumed. The user therefore needs to explicitly define all
    stage connections.
    The calling sequence is:
       pipeline.cpp:            <fetchStage>::behavior()
    -> registers.cpp:           clock_cycle()
    -> register_abstraction.hpp:clock_cycle()
    -> registers.cpp:           <reg>_clock_cycle()
    The fetch stage calls a clock_cycle function which iterates through all
    registers and calls their clock_cycle() method defined in
    register_abstraction.hpp. This, in turn, calls the register-specific
    callback function <reg>_clock_cycle, generated by TRAP in the registers.cpp
    file."""

    if not wbStageOrder: return

    pipeNumbers = {}
    i = 0
    for pipeStage in self.pipes:
        pipeNumbers[pipeStage.name] = i
        i += 1

    Code = """bool has_changes = false;

    // Copy over old values, just in case stalls should prevent propagation.
    for (unsigned i = 0; i < """ + str(len(self.pipes)) + """; ++i) {
      temp_values[i] = values[i];
      temp_time_stamps[i] = time_stamps[i];
    }
    """

    # 1c. Default Behavior: Regular step-wise forward propagation.
    # Note that in this case, all stages propagate, not just [regsStage..
    # wbStage]
    Code += """
      // Regular step-wise forward propagation.
      for (int i = 1; i <= """ + str(len(self.pipes)-1) + """; ++i) {
        // The stage will contain a bubble.
        if (next_nop_stages & (1 << i)) {
          temp_values[i] = 0;
          temp_time_stamps[i] = 0;
        } else if ((i > stall_stage) && (time_stamps[i-1] >= time_stamps[i])) {
          temp_values[i] = values[i-1];
          temp_time_stamps[i] = time_stamps[i-1];
          has_changes = true;
        }
      }
      """

    # 3. Register-specific feedback loops via <reg>.setWbStageOrder():
    #for fromStage in sorted(wbStageOrder, key=lambda x: [pipe.name for pipe in self.pipes].index(x), reverse = True):
    for toStage, fromStages in wbStageOrder.items():
        for fromStage in reversed(fromStages):
            Code += """
              // """ + fromStage + """ -> """ + toStage + """
              if (!(cur_nop_stages & """ + hex(1 << pipeNumbers[fromStage]) + """)) {
                if (time_stamps[""" + str(pipeNumbers[fromStage]) + """] >= temp_time_stamps[""" + str(pipeNumbers[toStage]) + """]) {
                  temp_values[""" + str(pipeNumbers[toStage]) + """] = values[""" + str(pipeNumbers[fromStage]) + """];
                  temp_time_stamps[""" + str(pipeNumbers[toStage]) + """] = time_stamps[""" + str(pipeNumbers[fromStage]) + """];
                    has_changes = true;
                }
              }
              """
    Code += '\nreturn has_changes;\n'
    registerClockCycleValueParam = cxx_writer.Parameter('values', registerMaxBitwidth.makePointer())
    registerClockCycleTempValueParam = cxx_writer.Parameter('temp_values', registerMaxBitwidth.makePointer())
    registerClockCycleTimeStampParam = cxx_writer.Parameter('time_stamps', cxx_writer.ulonglongPtrType)
    registerClockCycleTempTimeStampParam = cxx_writer.Parameter('temp_time_stamps', cxx_writer.ulonglongPtrType)
    registerClockCycleStallStageParam = cxx_writer.Parameter('stall_stage', cxx_writer.intType)
    registerClockCycleCurNopStagesParam = cxx_writer.Parameter('cur_nop_stages', cxx_writer.uintType)
    registerClockCycleNextNopStagesParam = cxx_writer.Parameter('next_nop_stages', cxx_writer.uintType)
    registerClockCycleFunction = cxx_writer.Function(regName.lower() + '_clock_cycle', cxx_writer.Code(Code), cxx_writer.boolType, [registerClockCycleValueParam, registerClockCycleTempValueParam, registerClockCycleTimeStampParam, registerClockCycleTempTimeStampParam, registerClockCycleStallStageParam, registerClockCycleCurNopStagesParam, registerClockCycleNextNopStagesParam])

    return registerClockCycleFunction

################################################################################
# Register Field Index Enums
################################################################################
def getCPPRegisterFields(self):
    """Returns a list of enum classes containing names of register fields to be
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
    """Returns a list of defines for getting rid of the "R." syntax. Thus, a
    given REG will be a synonym for R.reg. Assumes the calling object has a
    member RegisterContainer<> R; This is just a nicety to keep the old
    interface where each register was an inividual member."""
    # TODO: I obviously hate this ugly hack. But I don't like forcing the R.x
    # syntax either. I need to think of a better solution, esp since this leads
    # to cryptic compilation errors for name collisions, as was the case for
    # LEON2/LEON3 Y register, which I had to rename YREG since it collided with
    # a Y template parameter in boost! Very ugly and very hard to trace.
    registerDefines = ''
    for reg in self.regs + self.regBanks + self.aliasRegs + self.aliasRegBanks:
        registerDefines += '#define ' + reg.name + ' R.' + reg.name.lower() + '\n'
    return cxx_writer.Define(registerDefines)

################################################################################
# Register Container Class
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

    registerElements = []
    # reg_clock_cycle()
    # @see getPipeClockCycleFunction.
    pipeClockCycleFunction = getPipeClockCycleFunction(self, registerMaxBitwidth)
    if model.startswith('acc'):
        registerElements.append(pipeClockCycleFunction)

    ## @} Preprocessing
    #---------------------------------------------------------------------------
    ## @name Attributes and Initialization
    #  @{

    from processor import extractRegInterval
    registerMembers = []
    registerCtorParams = []
    registerCtorInit = []
    registerCtorCode = ''

    # Registers
    registerCtorCode += '// Initialize registers.\n'
    for reg in self.regs:
        # Attribute Declaration
        registerMembers.append(cxx_writer.Attribute(reg.name.lower(), registerType, 'public'))

        # Constructor Parameters
        if isinstance(reg.constValue, str):
            if reg.constValue not in [param.name for param in registerCtorParams]:
                registerCtorParams.append(cxx_writer.Parameter(reg.constValue, registerMaxBitwidth))
        # Iterable element, i.e. initialization with a constant and an offset.
        # TODO: Some of the default values are processor variables such as
        # ENTRY_POINT and MPROC_ID. These are sadly globals and set by the
        # loader at some point. The current solution passes uninitialized
        # values. There are several options:
        # 1. Force program loading before processor construction. Easy but
        #    limits the user.
        # 2. Keep the current proc if, but pass a reference instead of a value.
        #    Create a stub bool = 0 for all other registers. Minimal impact but
        #    ugly.
        # 3. Create processor.load(), which sets both the processor vars and
        #    resets the registers. Works for the PC, but not for others. Also
        #    still ugly, as the register should do its own resetting.
        # 4. Create a processor.load(), which calls a register.
        #    set_reset_value(). 1) Clean processor if, 2) keeps the reset value
        #    in the register and 3) works for both const and non-const reset
        #    values.
        if isinstance(reg.defValue, tuple):
            if reg.defValue[0] not in [param.name for param in registerCtorParams]:
                registerCtorParams.append(cxx_writer.Parameter(str(reg.defValue[0]), registerMaxBitwidth))
        elif isinstance(reg.defValue, str):
            if reg.defValue not in [param.name for param in registerCtorParams]:
                registerCtorParams.append(cxx_writer.Parameter(str(reg.defValue), registerMaxBitwidth))

        # Constructor Initialization List
        # name, abstraction
        Code = '("' + reg.name.lower() + '", ' + abstraction + ', '
        # is_const
        if reg.constValue: Code += 'true, '
        else: Code += 'false, '
        # offset
        if reg.offset: Code += str(reg.offset) + ', '
        else: Code += '0, '
        # delay
        if reg.delay: Code += str(reg.delay) + ', '
        else: Code += '0, '
        # reset_val
        if reg.constValue != None:
            try: Code += hex(reg.constValue)
            except TypeError: Code += str(reg.constValue)
        elif reg.defValue != None:
            # Iterable element, i.e. initialization with a constant and an
            # offset.
            if isinstance(reg.defValue, tuple):
                Code += str(reg.defValue[0]) + ' + '
                try: Code += hex(reg.defValue[1])
                except TypeError: Code += str(reg.defValue[1])
            else:
                try: Code += hex(reg.defValue)
                except TypeError: Code += str(reg.defValue)
        else: Code += '0'
        Code += ', '
        # num_pipe_stages
        if self.pipes: Code += str(len(self.pipes))
        else: Code += '1'
        if model.startswith('acc'):
            if reg.isGlobal:
                Code += ', NULL, true'
            elif reg.wbStageOrder:
                registerClockCycleFunction = getRegisterClockCycleFunction(self, reg.name, reg.wbStageOrder, registerMaxBitwidth)
                registerElements.append(registerClockCycleFunction)
                Code += ', ' + registerClockCycleFunction.name
            elif pipeClockCycleFunction:
                Code += ', ' + pipeClockCycleFunction.name
        Code += ')'
        registerCtorInit.append(reg.name.lower() + Code)

        # Constructor Body: Add fields
        if reg.bitMask:
            for registerFieldMaskName, registerFieldMaskPos in reg.bitMask.items():
                registerCtorCode += reg.name.lower() + '.add_field("' + registerFieldMaskName + '", ' + str(registerFieldMaskPos[1]) + ', ' + str(registerFieldMaskPos[0]) + ');\n'
            registerCtorCode += '\n'

    # Register Banks
    registerCtorCode += '// Initialize register banks.\n'
    for regBank in self.regBanks:
        # Attribute Declaration
        registerMembers.append(cxx_writer.Attribute(regBank.name.lower() + '[' + str(regBank.numRegs) + ']', registerType, 'public'))

        # Constructor Parameters
        for regConstValue in regBank.constValue.values():
            if isinstance(regConstValue, str):
                if regConstValue not in [param.name for param in registerCtorParams]:
                    registerCtorParams.append(cxx_writer.Parameter(regConstValue, registerMaxBitwidth))
        for regDefaultValue in regBank.defValues:
            # Iterable element, i.e. initialization with a constant and an
            # offset.
            if isinstance(regDefaultValue, tuple):
                if regDefaultValue[0] not in [param.name for param in registerCtorParams]:
                    registerCtorParams.append(cxx_writer.Parameter(str(regDefaultValue[0]), registerMaxBitwidth))
            elif isinstance(regDefaultValue, str):
                if regDefaultValue not in [param.name for param in registerCtorParams]:
                    registerCtorParams.append(cxx_writer.Parameter(str(regDefaultValue), registerMaxBitwidth))

        # Constructor Initialization List
        Code = ''
        for reg in range(0, regBank.numRegs):
            # name, abstraction
            Code += '{"' + regBank.name.lower() + '[' + str(reg) + ']", ' + abstraction + ', '
            # is_const
            if regBank.constValue.has_key(reg): Code += 'true, '
            else: Code += 'false, '
            # offset
            if regBank.offset: Code += str(regBank.offset) + ', '
            else: Code += '0, '
            # delay
            if regBank.delay.has_key(reg): Code += str(regBank.delay[reg]) + ', '
            else: Code += '0, '
            # reset_val
            if regBank.constValue.has_key(reg):
                try: Code += hex(regBank.constValue[reg])
                except TypeError: Code += str(regBank.constValue[reg])
            elif regBank.defValues[reg] != None:
                # Iterable element, i.e. initialization with a constant and an
                # offset.
                if isinstance(regBank.defValues[reg], tuple):
                    Code += str(regBank.defValues[reg][0]) + ' + '
                    try: Code += hex(regBank.defValues[reg][1])
                    except TypeError: Code += str(regBank.defValues[reg][1])
                else:
                    try: Code += hex(regBank.defValues[reg])
                    except TypeError: Code += str(regBank.defValues[reg])
            else: Code += '0'
            Code += ', '
            # num_pipe_stages
            if self.pipes: Code += str(len(self.pipes))
            else: Code += '1'
            if model.startswith('acc'):
                if regBank.isGlobal:
                    Code += ', NULL, true'
                elif regBank.wbStageOrder.has_key(reg):
                    registerClockCycleFunction = getRegisterClockCycleFunction(self, regBank.name + str(reg), regBank.wbStageOrder[reg], registerMaxBitwidth)
                    registerElements.append(registerClockCycleFunction)
                    Code += ', ' + registerClockCycleFunction.name
                elif pipeClockCycleFunction:
                    Code += ', ' + pipeClockCycleFunction.name
            Code += '},\n'
        registerCtorInit.append(regBank.name.lower() + ' {' + Code[:-2] + '}')

        # Constructor Body: Add fields.
        if regBank.bitMask:
            registerCtorCode += 'for (unsigned i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'

            for registerFieldMaskName, registerFieldMaskPos in regBank.bitMask.items():
                registerCtorCode += regBank.name.lower() + '[i].add_field("' + registerFieldMaskName + '", ' + str(registerFieldMaskPos[1]) + ', ' + str(registerFieldMaskPos[0]) + ');\n'

            registerCtorCode += '}\n\n'

    # Alias Registers
    for alias in self.aliasRegs:
        # Attribute Declaration
        #aliasType = cxx_writer.TemplateType('std::reference_wrapper', [registerType], 'functional')
        #aliasBankType = cxx_writer.TemplateType('std::vector', [aliasType], 'vector')
        registerMembers.append(cxx_writer.Attribute(alias.name.lower(), aliasType, 'public'))

        # Constructor Initialization List
        registerCtorInit.append(alias.name.lower() + '("' + alias.name.lower() + '")')

    # Alias Register Banks
    for aliasBank in self.aliasRegBanks:
        # Attribute Declaration
        registerMembers.append(cxx_writer.Attribute(aliasBank.name.lower() + '[' + str(aliasBank.numRegs) + ']', aliasType, 'public'))

        # Constructor Initialization List
        Code = ''
        for aliasIdx in range(0, aliasBank.numRegs):
            Code += '{"' + aliasBank.name.lower() + '[' + str(aliasIdx) + ']"},'
        registerCtorInit.append(aliasBank.name.lower() + ' {' + Code[:-1] + '}')

    # Alias Registers and Alias Register Banks
    # Constructor Body: Update alias targets.
    registerCtorCode += '// Initialize alias registers and alias register banks.\n'
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
                    registerCtorCode += 'this->' + alias.name.lower()
                    if alias in self.aliasRegBanks:
                        registerCtorCode += '[' + str(aliasIdx) + ']'
                    registerCtorCode += '.update_alias(this->' + regName.lower() + '[' + str(regIdx) + ']'
                    if alias in self.aliasRegs:
                        registerCtorCode += ', ' + str(alias.offset)
                    elif alias.offsets.has_key(aliasIdx):
                        registerCtorCode += ', ' + str(alias.offsets[aliasIdx])
                    else:
                        registerCtorCode += ', 0'
                    registerCtorCode += ');\n'
                    aliasIdx = aliasIdx + 1
            # REG
            else:
                registerCtorCode += 'this->' + alias.name.lower() + '.update_alias(this->' + regName.lower()
                if alias in self.aliasRegs:
                    registerCtorCode += ', ' + str(alias.offset)
                elif alias.offsets.has_key(aliasIdx):
                    registerCtorCode += ', ' + str(alias.offsets[aliasIdx])
                else:
                    registerCtorCode += ', 0'
                registerCtorCode += ');\n'
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
                        registerCtorCode += 'this->' + alias.name.lower() + '[' + str(aliasIdx) + '].update_alias(this->' + regName.lower() + '[' + str(regIdx) + ']'
                        if alias.offsets.has_key(aliasIdx):
                            registerCtorCode += ', ' + str(alias.offsets[aliasIdx])
                        else:
                            registerCtorCode += ', 0'
                        registerCtorCode += ');\n'
                        aliasIdx = aliasIdx + 1
                # REG
                else:
                    registerCtorCode += 'this->' + alias.name.lower() + '[' + str(aliasIdx) + '].update_alias(this->' + regName.lower()
                    if alias.offsets.has_key(aliasIdx):
                        registerCtorCode += ', ' + str(alias.offsets[aliasIdx])
                    else:
                        registerCtorCode += ', 0'
                    registerCtorCode += ');\n'
                    aliasIdx = aliasIdx + 1

    # Constructor
    registerCtor = cxx_writer.Constructor(cxx_writer.Code(registerCtorCode), 'public', parameters = registerCtorParams, initList = registerCtorInit)

    ## @} Attributes and Initialization
    #---------------------------------------------------------------------------
    ## @name Methods
    #  Access and Modification: reset(), write(), write_dbg(), write_force()
    #  Observer: execute_callbacks(), set_stage(), unset_stage(), clock_cycle()
    #  Information and Helper: operator<<()
    #  @{

    # TODO: Consider a visitor pattern, where the function visiting the
    # registers is passed as a parameter.
    # Method Bodies: Registers
    registerResetCode = '// Reset registers.\n'
    registerWriteCode = 'bool ret = true;\n\n// Write registers.\n'
    registerWriteDbgCode = 'bool ret = true;\n\n// Write registers.\n'
    registerWriteForceCode = 'bool ret = true;\n\n// Write registers.\n'
    registerExecuteCallbacksCode = '// Execute callbacks on registers.\n'
    registerSetStageCode = '// Set pipeline stage for registers.\n'
    registerUnsetStageCode = '// Unset pipeline stage for registers.\n'
    registerClockCycleCode = '// Propagate pipeline stage for registers.\n'
    registerStallCode = '// Stall pipeline for registers.\n'
    registerAdvanceCode = '// Advance pipeline for registers.\n'
    registerFlushCode = '// Flush registers.\n'
    registerPrintCode = 'os << std::hex << std::showbase;\n\n// Print registers.\n'
    for reg in self.regs:
        if reg.constValue == None:
            registerResetCode += reg.name.lower() + '.reset();\n'
            registerWriteCode += 'ret = ret && ' + reg.name.lower() + '.write(data);\n'
            registerWriteDbgCode += 'ret = ret && ' + reg.name.lower() + '.write_dbg(data);\n'
            registerWriteForceCode += 'ret = ret && ' + reg.name.lower() + '.write_force(data);\n'
        registerExecuteCallbacksCode += reg.name.lower() + '.execute_callbacks(type, 0, sizeof(' + registerMaxBitwidth.name.lower() + '));\n'
        registerSetStageCode += reg.name.lower() + '.get_strategy()->set_stage(stage);\n'
        registerUnsetStageCode += reg.name.lower() + '.get_strategy()->unset_stage();\n'
        registerClockCycleCode += reg.name.lower() + '.clock_cycle();\n'
        registerStallCode += reg.name.lower() + '.stall(stage);\n'
        registerAdvanceCode += reg.name.lower() + '.advance();\n'
        registerFlushCode += reg.name.lower() + '.flush(stage);\n'
        registerPrintCode += 'os << ' + reg.name.lower() + '.name() << ": " << ' + reg.name.lower() + '.read_dbg() << \'\\n\';\n'

    # Method Bodies: Register Banks
    registerResetCode += '\n// Reset register banks.\n'
    registerWriteCode += '\n// Write register banks.\n'
    registerWriteDbgCode += '\n// Write register banks.\n'
    registerWriteForceCode += '\n// Write register banks.\n'
    registerExecuteCallbacksCode += '\n// Execute callbacks on register banks.\n'
    registerSetStageCode += '\n// Set pipeline stage for register banks.\n'
    registerUnsetStageCode += '\n// Unset pipeline stage for register banks.\n'
    registerClockCycleCode += '// Propagate pipeline stage for register banks.\n'
    registerStallCode += '// Stall pipeline for register banks.\n'
    registerAdvanceCode += '// Advance pipeline for register banks.\n'
    registerFlushCode += '// Flush register banks.\n'
    registerPrintCode += '\n// Print register banks.\n'
    for regBank in self.regBanks:
        registerResetCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerResetCode += regBank.name.lower() + '[i].reset();\n'
        registerResetCode += '}\n\n'
        registerWriteCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerWriteCode += 'ret = ret && ' + regBank.name.lower() + '[i].write(data);\n'
        registerWriteCode += '}\n\n'
        registerWriteDbgCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerWriteDbgCode += 'ret = ret && ' + regBank.name.lower() + '[i].write_dbg(data);\n'
        registerWriteDbgCode += '}\n\n'
        registerWriteForceCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerWriteForceCode += 'ret = ret && ' + regBank.name.lower() + '[i].write_force(data);\n'
        registerWriteForceCode += '}\n\n'
        registerExecuteCallbacksCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerExecuteCallbacksCode += regBank.name.lower() + '[i].execute_callbacks(type, 0, sizeof(' + registerMaxBitwidth.name.lower() + '));\n'
        registerExecuteCallbacksCode += '}\n\n'
        registerSetStageCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerSetStageCode += regBank.name.lower() + '[i].set_stage(stage);\n'
        registerSetStageCode += '}\n\n'
        registerUnsetStageCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerUnsetStageCode += regBank.name.lower() + '[i].unset_stage();\n'
        registerUnsetStageCode += '}\n\n'
        registerClockCycleCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerClockCycleCode += regBank.name.lower() + '[i].clock_cycle();\n'
        registerClockCycleCode += '}\n\n'
        registerStallCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerStallCode += regBank.name.lower() + '[i].stall(stage);\n'
        registerStallCode += '}\n\n'
        registerAdvanceCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerAdvanceCode += regBank.name.lower() + '[i].advance();\n'
        registerAdvanceCode += '}\n\n'
        registerFlushCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerFlushCode += regBank.name.lower() + '[i].flush(stage);\n'
        registerFlushCode += '}\n\n'
        registerPrintCode += 'for (int i = 0; i < ' + str(regBank.numRegs) + '; ++i) {\n'
        registerPrintCode += 'os << ' + regBank.name.lower() + '[i].name() << ": " << ' + regBank.name.lower() + '[i].read_dbg() << \'\\n\';\n'
        registerPrintCode += '}\n\n'
    registerWriteCode += 'return ret;\n'
    registerWriteDbgCode += 'return ret;\n'
    registerWriteForceCode += 'return ret;\n'
    registerPrintCode += 'os << std::dec;\nreturn os;\n'

    # Method Declarations
    registerResetMethod = cxx_writer.Method('reset', cxx_writer.Code(registerResetCode), cxx_writer.voidType, 'public')
    registerMembers.append(registerResetMethod)

    registerWriteDataParam = cxx_writer.Parameter('data', registerMaxBitwidth.makeConst().makeRef())
    registerWriteMethod = cxx_writer.Method('write', cxx_writer.Code(registerWriteCode), cxx_writer.boolType, 'public', [registerWriteDataParam])
    registerMembers.append(registerWriteMethod)
    registerWriteDbgMethod = cxx_writer.Method('write_dbg', cxx_writer.Code(registerWriteDbgCode), cxx_writer.boolType, 'public', [registerWriteDataParam])
    registerMembers.append(registerWriteDbgMethod)
    registerWriteForceMethod = cxx_writer.Method('write_force', cxx_writer.Code(registerWriteForceCode), cxx_writer.boolType, 'public', [registerWriteDataParam])
    registerMembers.append(registerWriteForceMethod)

    registerExecuteCallbacksUint32RefType = cxx_writer.Type('uint32_t', const = True).makeRef()
    registerExecuteCallbacksMethod = cxx_writer.Method('execute_callbacks', cxx_writer.Code(registerExecuteCallbacksCode), cxx_writer.voidType, 'public',
      [cxx_writer.Parameter('type', cxx_writer.Type('scireg_ns::scireg_callback_type', const = True).makeRef()),
      cxx_writer.Parameter('offset', registerExecuteCallbacksUint32RefType, initValue = '0'),
      cxx_writer.Parameter('size', cxx_writer.Type('uint32_t', const = True).makeRef(), initValue = '0')])
    registerMembers.append(registerExecuteCallbacksMethod)

    registerSetStageMethod = cxx_writer.Method('set_stage', cxx_writer.Code(registerSetStageCode), cxx_writer.voidType, 'public', [cxx_writer.Parameter('stage', cxx_writer.uintType)])
    registerMembers.append(registerSetStageMethod)
    registerUnsetStageMethod = cxx_writer.Method('unset_stage', cxx_writer.Code(registerUnsetStageCode), cxx_writer.voidType, 'public')
    registerMembers.append(registerUnsetStageMethod)
    registerClockCycleMethod = cxx_writer.Method('clock_cycle', cxx_writer.Code(registerClockCycleCode), cxx_writer.voidType, 'public')
    registerMembers.append(registerClockCycleMethod)
    registerStallMethod = cxx_writer.Method('stall', cxx_writer.Code(registerStallCode), cxx_writer.voidType, 'public', [cxx_writer.Parameter('stage', cxx_writer.uintType)])
    registerMembers.append(registerStallMethod)
    registerAdvanceMethod = cxx_writer.Method('advance', cxx_writer.Code(registerAdvanceCode), cxx_writer.voidType, 'public')
    registerMembers.append(registerAdvanceMethod)
    registerFlushMethod = cxx_writer.Method('flush', cxx_writer.Code(registerFlushCode), cxx_writer.voidType, 'public', [cxx_writer.Parameter('stage', cxx_writer.uintType)])
    registerMembers.append(registerFlushMethod)

    registerPrintOstreamRefType = cxx_writer.Type('std::ostream', 'iostream').makeRef()
    registerPrintMethod = cxx_writer.MemberOperator('<<', cxx_writer.Code(registerPrintCode), registerPrintOstreamRefType, 'public',
      [cxx_writer.Parameter('os', registerPrintOstreamRefType)], const = True)
    registerMembers.append(registerPrintMethod)

    ## @} Methods
    #---------------------------------------------------------------------------

    registerClass = cxx_writer.ClassDeclaration('Registers', registerMembers, namespaces = [namespace])
    registerClass.addDocString(brief = 'Register Container Class', detail = 'Contains all registers and register banks of the processor as member variables. It serves for encapsulating the instantiation details (defining fields, etc) away from the processor. It also simplifies passing the registers to the instructions, instead of passing each register individually.')
    registerClass.addConstructor(registerCtor)
    return [registerClass] + registerElements

################################################################################
