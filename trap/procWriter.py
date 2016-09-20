################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     procWriter.py
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
# Helper variables
pipeFetchAttrs = []
pipeCtorParams = []
pipeFetchCtorParams = []
instrAttrs = []
instrCtorParams = []
instrCtorValues = ''
testNames = []
abiAttrs = []

# Note that even if we use a separate namespace for
# every processor, it helps also having separate names
# for the different processors, as some bugged versions
# of the dynamic loader complains
processor_name = ''

hash_map_include = """
#ifdef __GNUC__
#ifdef __GNUC_MINOR__
#if (__GNUC__ >= 4 && __GNUC_MINOR__ >= 3)
#include <tr1/unordered_map>
#define template_map std::tr1::unordered_map
#else
#include <ext/hash_map>
#define  template_map __gnu_cxx::hash_map
#endif
#else
#include <ext/hash_map>
#define  template_map __gnu_cxx::hash_map
#endif
#else
#ifdef _WIN32
#include <hash_map>
#define  template_map stdext::hash_map
#else
#include <map>
#define  template_map std::map
#endif
#endif
"""

# Computes current program counter, in order to fetch instrutions from it.
def getFetchAddressCode(self, model):
    """Sets cur_PC to the address of the instruction to be fetched."""
    Code = ''
    if model.startswith('func'):
        Code += str(self.bitSizes[1]) + ' cur_PC = ' + 'this->' + self.fetchReg[0]
        # PC offsets are only relevant for functional models.
        if self.fetchReg[1] < 0:
            Code += str(self.fetchReg[1])
        elif self.fetchReg[1] > 0:
            Code += ' + ' + str(self.fetchReg[1])
        Code += ';\n'
    else:
        fetchStage = 0
        for i in range(0, len(self.pipes)):
            if self.pipes[i].fetchStage:
                fetchStage = i
                break;
        # Read the latched PC belonging to this pipeline stage.
        Code += 'this->' + self.fetchReg[0] + '.set_stage(' + str(fetchStage) + ');\n'
        Code += str(self.bitSizes[1]) + ' cur_PC = ' + 'this->' + self.fetchReg[0] + ';\n'
        Code += 'this->' + self.fetchReg[0] + '.unset_stage();\n'
    return Code

# Computes the code for the fetch address
def getDoFetchCode(self):
    """Reads the instruction at the address pointed to by cur_PC."""
    Code = str(self.bitSizes[1]) + ' bitstring = this->'
    # Fetching is either from memory or through TLM ports.
    if self.memory:
        Code += self.memory[0]
    else:
        for name, isFetch in self.tlmPorts.items():
            if isFetch:
                Code += name
        if Code.endswith('this->'):
            raise Exception('Neither TLM port nor internal memory defined for instruction fetch.')
    Code += '.read_word(cur_PC);\n'
    return Code

def getCacheInstrFetchCode(self, doFetchCode, trace, combinedTrace, issueCodeFunction, hasHazard = False, pipeStage = None):
    Code = ''
    if self.fastFetch:
        mapKey = 'cur_PC'
    else:
        mapKey = 'bitstring'
    Code += 'template_map< ' + str(self.bitSizes[1]) + ', CacheElem >::iterator cached_instr = this->instr_cache.find(' + mapKey + ');'
    # I have found the instruction in the cache
    Code += """if (cached_instr != icache_end) {
        cur_instr = cached_instr->second.instr;
        // Instruction found, call it.
        if (cur_instr != NULL) {
    """

    # Here we add the details about the instruction to the current history element
    Code += """#ifdef ENABLE_HISTORY
    if (this->history_en) {
        instr_queue_elem.name = cur_instr->get_name();
        instr_queue_elem.mnemonic = cur_instr->get_mnemonic();
    }
    #endif
    """

    if pipeStage:
        Code += 'if (cur_instr->in_pipeline) {\n'
        Code += 'cur_instr = cur_instr->replicate(cur_instr);\n'
        Code += 'cur_instr->to_destroy = true;\n'
        Code += '}\n'
        Code += 'cur_instr->in_pipeline = true;\n'
        Code += 'cur_instr->fetch_PC = cur_PC;\n'
    Code += issueCodeFunction(self, trace, combinedTrace, hasHazard, pipeStage)

    # I have found the element in the cache, but not the instruction
    Code += '} else {\n// Current instruction is indexed but not present in the cache. Perform regular decoding.\n'
    if self.fastFetch:
        Code += doFetchCode
    Code += 'unsigned& cur_instr_count = cached_instr->second.count;\n'
    Code += getDoDecodeCode(self, trace, combinedTrace, issueCodeFunction, hasHazard, pipeStage, ' && cur_instr_count < ' + str(self.cacheLimit))
    Code += """if (cur_instr_count < """ + str(self.cacheLimit) + """) {
            cur_instr_count++;
        } else {
            // Add the instruction to the i-cache.
            cached_instr->second.instr = cur_instr;
    """
    if pipeStage:
        Code += """if (cur_instr->to_destroy) {
                cur_instr->to_destroy = false;
            } else {
            """
        Code += 'this->INSTRUCTIONS[cur_instr_id] = cur_instr->replicate();\n'
        Code += '}\n'
    else:
        Code += 'this->INSTRUCTIONS[cur_instr_id] = cur_instr->replicate();\n'
    Code += '}\n'

    # and now finally I have found nothing and I have to add everything
    Code += """}
    } else {
        // Current instruction is not present in the cache. Perform regular decoding.
    """
    if self.fastFetch:
        Code += doFetchCode
    Code += getDoDecodeCode(self, trace, combinedTrace, issueCodeFunction, hasHazard, pipeStage)
    Code += """this->instr_cache.insert(std::pair<unsigned, CacheElem>(bitstring, CacheElem()));
        icache_end = this->instr_cache.end();
        }
    """
    return Code

# Returns the code necessary for performing a standard instruction fetch: i.e.
# read from memory and set the instruction parameters
def getDoDecodeCode(self, trace, combinedTrace, issueCodeFunction, hasHazard = False, pipeStage = None, checkDestroyCode = ''):
    Code = 'int cur_instr_id = this->decoder.decode(bitstring);\n'
    if pipeStage:
        Code += 'cur_instr = this->INSTRUCTIONS[cur_instr_id];\n'
        Code += 'if (cur_instr->in_pipeline) {\n'
        Code += 'cur_instr = cur_instr->replicate();\n'
        Code += 'cur_instr->to_destroy = true;\n'
        Code += '}\n'
        Code += 'cur_instr->in_pipeline = true;\n'
        Code += 'cur_instr->fetch_PC = cur_PC;\n'
    else:
        Code += 'cur_instr = this->INSTRUCTIONS[cur_instr_id];\n'
    Code += 'cur_instr->set_params(bitstring);\n'

    # Here we add the details about the instruction to the current history element
    Code += """#ifdef ENABLE_HISTORY
    if (this->history_en) {
        instr_queue_elem.name = cur_instr->get_name();
        instr_queue_elem.mnemonic = cur_instr->get_mnemonic();
    }
    #endif
    """

    Code += issueCodeFunction(self, trace, combinedTrace, hasHazard, pipeStage, checkDestroyCode)
    return Code

# Computes the code defining the execution of an instruction and
# of the processor tools.
def getInstrIssueCode(self, trace, combinedTrace, hasHazard = False, pipeStage = None, checkDestroyCode = ''):
    Code = """try {
            #ifndef DISABLE_TOOLS
            if (!(this->tool_manager.issue(cur_PC, cur_instr))) {
            #endif
            num_cycles = cur_instr->behavior();
    """
    if trace:
        Code += 'cur_instr->print_trace();\n'
    Code += '#ifndef DISABLE_TOOLS\n}\n'
    if trace:
        Code += """else {
            std::cerr << "Instruction annulled by tools." << std::endl << std::endl;
        }
        """
    Code +='#endif\n}\ncatch(annul_exception& etc) {\n'
    if trace:
        Code += """cur_instr->print_trace();
                std::cerr << "Skipped Instruction " << cur_instr->get_name() << '.' << std::endl;
        """
    Code += """num_cycles = 0;
        }
        """
    return Code

# Computes the code defining the execution of an instruction and
# of the processor tools.
def getPipeInstrIssueCode(self, trace, combinedTrace, hasHazard, pipeStage, checkDestroyCode = ''):
    fetchStage = self.pipes[0]
    for i in self.pipes:
        if i.fetchStage:
            fetchStage = i
            break;

    unlockHazard = False
    for i in self.pipes:
        if i.decodeStage:
            unlockHazard = True
        if i == pipeStage:
            break

    Code = 'try {\n'
    if pipeStage == fetchStage:
        Code += """#ifndef DISABLE_TOOLS
            if (!(this->tool_manager.issue(cur_instr->fetch_PC, cur_instr))) {
            #endif
    """
    Code += """num_cycles = cur_instr->behavior_""" + pipeStage.name + """();
    """
    if pipeStage == fetchStage:
        Code += """#ifndef DISABLE_TOOLS
                    } else {
        """
        if trace:
            Code += """
            std::cerr << std::setw(15) << std::left << \"Stage=""" + pipeStage.name + """\" << \", PC=\" << std::hex << std::showbase << std::setw(10) << cur_instr->fetch_PC << \", Instruction=\" << std::setw(10) << std::left << cur_instr->get_name() << \", Mnemonic=\" << cur_instr->get_mnemonic() << \": Instruction annulled by tools.\" << std::endl;
            """
        Code += """
        if (cur_instr->to_destroy""" + checkDestroyCode + """) {
            delete cur_instr;
        } else {
            cur_instr->in_pipeline = false;
        }
        cur_instr = this->NOP_instr;
        """
        Code +='}\n#endif\n'
    Code +='}\ncatch(annul_exception& etc) {\n'
    if trace:
        Code += """
        std::cerr << std::setw(15) << std::left << \"Stage=""" + pipeStage.name + """\" << \", PC=\" << std::hex << std::showbase << std::setw(10) << cur_instr->fetch_PC << \", Instruction=\" << std::setw(10) << std::left << cur_instr->get_name() << \", Mnemonic=\" << cur_instr->get_mnemonic() << \": Skipped instruction.\" << std::endl;
        """
    if pipeStage != fetchStage:
        Code += 'instr_annulled = cur_instr->flush_pipeline;\n'
        Code += 'cur_instr->flush_pipeline = false;\n'
    if hasHazard and unlockHazard:
        Code +=  'cur_instr->unlock_regs();\n'
    Code += """
            if (cur_instr->to_destroy""" + checkDestroyCode + """) {
                delete cur_instr;
            } else {
                cur_instr->in_pipeline = false;
            }
            cur_instr = this->NOP_instr;
            num_cycles = 0;
        }
        """
    return Code

# Computes and prints the code necessary for dealing with interrupts
def getInterruptCode(self, trace, pipeStage = None):
    interruptCode = ''
    orderedIrqList = sorted(self.irqs, lambda x,y: cmp(y.priority, x.priority))
    for irqPort in orderedIrqList:
        if irqPort != orderedIrqList[0]:
            interruptCode += 'else '
        interruptCode += 'if ('
        if (irqPort.condition):
            interruptCode += '('
        interruptCode += irqPort.name
        if (irqPort.condition):
            interruptCode += ') && (' + irqPort.condition + ')'
        interruptCode += ') {\n'
        # Now I have to call the actual interrrupt instruction: again, this
        # depends on whether we are in the cycle accurate processor or
        # in the functional one.
        # Functional: we only need to call the instruction behavior
        # Cycle accurate, we need to add the instruction to the pipeline
        if trace:
            interruptCode += 'std::cerr << "Received interrupt " << std::hex << std::showbase << IRQ << \'.\' << std::endl;'
        interruptCode += 'this->' + irqPort.name + '_instr->set_interrupt_value(' + irqPort.name + ');\n'
        interruptCode += 'try {\n'
        if pipeStage:
            interruptCode += 'num_cycles = this->' + irqPort.name + '_instr->behavior_' + pipeStage.name + '();\n'
            interruptCode += 'cur_instr = this->' + irqPort.name + '_instr;\n'
        else:
            interruptCode += 'num_cycles = this->' + irqPort.name + '_instr->behavior();\n'
        interruptCode +='}\ncatch(annul_exception& etc) {\n'
        if trace:
            if pipeStage:
                interruptCode += """
                std::cerr << std::setw(15) << std::left << \"Stage=""" + pipeStage.name + """\" << \", PC=\" << std::hex << std::showbase << std::setw(10) << cur_instr->fetch_PC << \", Instruction=\" << std::setw(10) << std::left << this->""" + irqPort.name + """_instr->get_name() << \", Mnemonic=\" << this->""" + irqPort.name + """_instr->get_mnemonic() << \": Skipped instruction.\" << std::endl;
                """
            else:
                interruptCode += """
                std::cerr << \"Instruction=\" << std::setw(10) << std::left << this->""" + irqPort.name + """_instr->get_name() << \", Mnemonic=\" << this->""" + irqPort.name + """_instr->get_mnemonic() << \": Skipped instruction.\" << std::endl;
                """
        interruptCode += """num_cycles = 0;
            }
            """
        interruptCode += '\n}\n'
    return interruptCode

def initPipeline(self, processorMembers, processorCtorInit):
    """Creates the pipeleine stages and the code necessary to initialize them"""
    from registerWriter import registerContainerType
    from isa import resolveBitType
    InstructionType = cxx_writer.Type('Instruction', includes = ['#include \"instructions.hpp\"'])
    InstructionPtrType = InstructionType.makePointer()
    pipeType = cxx_writer.Type('BasePipeStage')

    global pipeFetchAttrs, pipeCtorParams, pipeFetchCtorParams
    pipeFetchAttrs = []
    pipeCtorParams = []
    pipeFetchCtorParams = []
    pipeCtorValues = []

    fetchStage = self.pipes[0]
    for i in self.pipes:
        if i.fetchStage:
            fetchStage = i
            break;

    # Base/Common Constructor Parameters
    pipeCtorParams.append(cxx_writer.Parameter('pipe_name', cxx_writer.sc_module_nameType))
    for otherPipeStage in self.pipes:
        pipeCtorParams.append(cxx_writer.Parameter('stage_' + otherPipeStage.name, pipeType.makePointer()))
    pipeCtorParams.append(cxx_writer.Parameter('prev_stage', pipeType.makePointer()))#, initValue = 'NULL'))
    pipeCtorParams.append(cxx_writer.Parameter('succ_stage', pipeType.makePointer()))#, initValue = 'NULL'))
    if (self.regs or self.regBanks):
        pipeCtorParams.append(cxx_writer.Parameter('R', registerContainerType.makeRef()))
    pipeCtorParams.append(cxx_writer.Parameter('latency', cxx_writer.sc_timeType.makeRef()))

    # Attributes, Constructor Parameters and Constructor Values
    for pipeStage in reversed(self.pipes):
        # Processor Attributes
        pipeStageType = cxx_writer.Type(pipeStage.name + 'PipeStage', '#include \"pipeline.hpp\"')
        processorMembers.append(cxx_writer.Attribute(pipeStage.name + '_stage', pipeStageType, 'public'))

        # Base/Common Constructor Values
        pipeCtorValues = ['\"' + pipeStage.name + '\"']
        for otherPipeStage in self.pipes:
            if otherPipeStage != pipeStage:
                pipeCtorValues.append('&' + otherPipeStage.name + '_stage')
            else:
                pipeCtorValues.append('NULL')
        if self.pipes.index(pipeStage) - 1 >= 0:
            pipeCtorValues.append('&' + self.pipes[self.pipes.index(pipeStage) - 1].name + '_stage')
        else:
            pipeCtorValues.append('NULL')
        if self.pipes.index(pipeStage) + 1 < len(self.pipes):
            pipeCtorValues.append('&' + self.pipes[self.pipes.index(pipeStage) + 1].name + '_stage')
        else:
            pipeCtorValues.append('NULL')
        if (self.regs or self.regBanks):
            pipeCtorValues.append('R')
        pipeCtorValues.append('latency')

        # Fetch Stage Attributes, Constructor Parameters and Constructor Values
        if pipeStage == fetchStage:
            # Tools
            ToolsManagerType = cxx_writer.TemplateType('ToolsManager', [self.bitSizes[1]], 'common/tools_if.hpp')
            pipeFetchAttrs.append(cxx_writer.Attribute('tool_manager', ToolsManagerType.makeRef(), 'private'))
            pipeFetchCtorParams.append(cxx_writer.Parameter('tool_manager', ToolsManagerType.makeRef()))
            pipeCtorValues.append('tool_manager')

            pipeFetchAttrs.append(cxx_writer.Attribute('profiler_time_start', cxx_writer.sc_timeRefType, 'public'))
            pipeFetchCtorParams.append(cxx_writer.Parameter('profiler_time_start', cxx_writer.sc_timeRefType))
            pipeCtorValues.append('profiler_time_start')

            pipeFetchAttrs.append(cxx_writer.Attribute('profiler_time_end', cxx_writer.sc_timeRefType, 'public'))
            pipeFetchCtorParams.append(cxx_writer.Parameter('profiler_time_end', cxx_writer.sc_timeRefType))
            pipeCtorValues.append('profiler_time_end')

            # Memory
            if self.memory:
                memRefType = cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"').makeRef()
                pipeFetchAttrs.append(cxx_writer.Attribute(self.memory[0], memRefType, 'private'))
                pipeFetchCtorParams.append(cxx_writer.Parameter(self.memory[0], memRefType))
                pipeCtorValues.append(self.memory[0])
            else:
                for name, isFetch in self.tlmPorts.items():
                    if isFetch:
                        memRefType = cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"').makeRef()
                        pipeFetchAttrs.append(cxx_writer.Attribute(name, memRefType, 'private'))
                        pipeFetchCtorParams.append(cxx_writer.Parameter(name, memRefType))
                        pipeCtorValues.append(name)
                        break

            # Interrupts
            for irq in self.irqs:
                irqWidthType = resolveBitType('BIT<' + str(irq.portWidth) + '>')
                pipeFetchAttrs.append(cxx_writer.Attribute(irq.name, irqWidthType.makeRef(), 'private'))
                pipeFetchCtorParams.append(cxx_writer.Parameter(irq.name, irqWidthType.makeRef()))
                pipeCtorValues.append('this->' + irq.name)

            # Instructions
            pipeFetchAttrs.append(cxx_writer.Attribute('INSTRUCTIONS', InstructionPtrType.makePointer().makeRef(), 'private'))
            pipeFetchCtorParams.append(cxx_writer.Parameter('INSTRUCTIONS', InstructionPtrType.makePointer().makeRef()))
            pipeCtorValues.append('this->INSTRUCTIONS')

            pipeFetchAttrs.append(cxx_writer.Attribute('num_instructions', cxx_writer.uintType.makeRef(), 'private'))
            pipeFetchCtorParams.append(cxx_writer.Parameter('num_instructions', cxx_writer.uintType.makeRef()))
            pipeCtorValues.append('num_instructions')

            pipeFetchAttrs.append(cxx_writer.Attribute('instr_executing', cxx_writer.boolType.makeRef(), 'private'))
            pipeFetchCtorParams.append(cxx_writer.Parameter('instr_executing', cxx_writer.boolType.makeRef()))
            pipeCtorValues.append('instr_executing')

            pipeFetchAttrs.append(cxx_writer.Attribute('instr_end_event', cxx_writer.sc_eventType.makeRef(), 'private'))
            pipeFetchCtorParams.append(cxx_writer.Parameter('instr_end_event', cxx_writer.sc_eventType.makeRef()))
            pipeCtorValues.append('instr_end_event')

        processorCtorInit.append(pipeStage.name + '_stage(' + ', '.join(pipeCtorValues)  + ')')

################################################################################
# Processor Class
################################################################################
def getCPPProcessor(self, model, trace, combinedTrace, namespace):
    """Returns the class representing the processor."""
    fetchWordType = self.bitSizes[1]
    includes = fetchWordType.getIncludes()
    ToolsManagerType = cxx_writer.TemplateType('ToolsManager', [fetchWordType], 'common/tools_if.hpp')
    if self.abi:
        interfaceType = cxx_writer.Type('Interface', '#include \"interface.hpp\"')
    IntructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
    InstructionTypePtr = IntructionType.makePointer()
    CacheElemType = cxx_writer.Type('CacheElem')

    # fetchStage = [pipeIdx for pipeIdx, pipeStage in enumerate(self.pipes) if pipeStage.fetchStage][0]
    # wbStage = [pipeIdx for pipeIdx, pipeStage in enumerate(self.pipes) if pipeStage.wbStage][0]
    fetchStage = self.pipes[0]
    wbStage = self.pipes[-1]
    for pipeStage in self.pipes:
        if pipeStage.fetchStage:
            fetchStage = pipeStage
        if pipeStage.wbStage:
            wbStage = pipeStage

    processorMembers = []
    processorCtorInit = []
    processorCtorCode = ''
    Code = ''

    global abiAttrs
    abiAttrs = []
    abiCtorValues = []

    #---------------------------------------------------------------------------
    ## @name Attributes and Initialization: Configuration
    #  @{

    # Attributes and Initialization: Processor Configuration
    processorCtorParams = [cxx_writer.Parameter('name', cxx_writer.sc_module_nameType)]
    processorCtorInit.append('sc_module(name)')

    numProcAttr = cxx_writer.Attribute('num_instances', cxx_writer.intType, 'private', True, '0')
    processorMembers.append(numProcAttr)
    processorCtorCode += processor_name + '::num_instances++;\n'

    # Attributes and Initialization: Timing
    if self.systemc or model.startswith('acc') or model.endswith('AT'):
        latencyAttr = cxx_writer.Attribute('latency', cxx_writer.sc_timeType, 'public')
        processorMembers.append(latencyAttr)
        if not self.externalClock:
            processorCtorParams.append(cxx_writer.Parameter('latency', cxx_writer.sc_timeType))
            processorCtorInit.append('latency(latency)')
    else:
        totalCyclesAttr = cxx_writer.Attribute('total_cycles', cxx_writer.uintType, 'public')
        processorMembers.append(totalCyclesAttr)
        processorCtorCode += 'this->total_cycles = 0;\n'

    if model.endswith('LT') and len(self.tlmPorts) > 0 and not model.startswith('acc'):
        quantumKeeperType = cxx_writer.Type('tlm_utils::tlm_quantumkeeper', 'tlm_utils/tlm_quantumkeeper.h')
        quantumKeeperAttr = cxx_writer.Attribute('quant_keeper', quantumKeeperType, 'private')
        processorMembers.append(quantumKeeperAttr)
        processorCtorCode += 'this->quant_keeper.set_global_quantum(this->latency*100);\nthis->quant_keeper.reset();\n'

    for param in self.parameters:
        configAttr = cxx_writer.Attribute(param.name, param.type, 'private')
        processorMembers.append(configAttr)
        processorCtorParams.append(param)
        processorCtorInit.append(param.name + '(' + param.name + ')')

    resetCalledAttr = cxx_writer.Attribute('reset_called', cxx_writer.boolType, 'private')
    processorMembers.append(resetCalledAttr)
    processorCtorCode += 'this->reset_called = false;\n'

    if not model.startswith('acc'):
        processorCtorCode += 'SC_THREAD(main_loop);\n'

    ## @} Attributes and Initialization: Configuration
    #---------------------------------------------------------------------------
    ## @name Attributes and Initialization: Pipeline, Storage and Interface
    #  @{

    # Attributes and Initialization: Pipeline
    if model.startswith('acc'):
        initPipeline(self, processorMembers, processorCtorInit)

    # Attributes and Initialization: Registers, Aliases and Register Banks
    if (self.regs or self.regBanks):
        from registerWriter import registerContainerType
        processorMembers.append(cxx_writer.Attribute('R', registerContainerType, 'public'))
        registerCtorValues = []
        for reg in self.regs:
            if isinstance(reg.constValue, str):
                if reg.constValue not in registerCtorValues:
                    registerCtorValues.append(reg.constValue)
            if isinstance(reg.defValue, tuple):
                if reg.defValue[0] not in registerCtorValues:
                    registerCtorValues.append(reg.defValue[0])
            elif isinstance(reg.defValue, str):
                if reg.defValue not in registerCtorValues:
                    registerCtorValues.append(reg.defValue)
        for regBank in self.regBanks:
            for regConstValue in regBank.constValue.values():
                if isinstance(regConstValue, str):
                    if regConstValue not in registerCtorValues:
                        registerCtorValues.append(regConstValue)
            for regDefaultValue in regBank.defValues:
                if isinstance(regDefaultValue, tuple):
                    if regDefaultValue[0] not in registerCtorValues:
                        registerCtorValues.append(regDefaultValue[0])
                elif isinstance(regDefaultValue, str):
                    if regDefaultValue not in registerCtorValues:
                        registerCtorValues.append(regDefaultValue)
        processorCtorInit.append('R(' + ', '.join(registerCtorValues) + ')')
        abiAttrs.append(cxx_writer.Attribute('R', registerContainerType.makeRef(), 'private'))
        abiCtorValues.append('this->R')

    # Attributes and Initialization: Memory
    if self.memory:
        memoryAttr = cxx_writer.Attribute(self.memory[0], cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"'), 'public')
        processorMembers.append(memoryAttr)
        memoryCtorValues = [str(self.memory[1])]
        if self.memory[2] and not self.systemc and not model.startswith('acc') and not model.endswith('AT'):
            memoryCtorValues.append('total_cycles')
        if self.memAlias:
            memoryCtorValues.append('this->R')
        if self.memory[2] and self.memory[3]:
            memoryCtorValues.append(self.memory[3])
        processorCtorInit.append(self.memory[0] + '(' + ', '.join(memoryCtorValues) + ')')
        if self.memory[0] in self.abi.memories.keys():
            abiAttrs.append(cxx_writer.Attribute(self.memory[0], cxx_writer.Type('MemoryInterface', '#include \"memory.hpp\"').makeRef(), 'private'))
            abiCtorValues.append('this->' + self.memory[0])

    # Attributes and Initialization: Interrupts
    for irqPort in self.irqs:
        from isa import resolveBitType
        irqWidthType = resolveBitType('BIT<' + str(irqPort.portWidth) + '>')
        irqSignalAttr = cxx_writer.Attribute(irqPort.name, irqWidthType, 'private')
        processorMembers.append(irqSignalAttr)

        if irqPort.tlm:
            irqPortType = cxx_writer.Type('TLMIntrPort_' + str(irqPort.portWidth), '#include \"irqPorts.hpp\"')
        else:
            irqPortType = cxx_writer.Type('SCIntrPort_' + str(irqPort.portWidth), '#include \"irqPorts.hpp\"')
        irqPortAttr = cxx_writer.Attribute(irqPort.name + '_port', irqPortType, 'public')
        processorMembers.append(irqPortAttr)
        processorCtorInit.append(irqPort.name + '_port(\"' + irqPort.name + '_port\", ' + irqPort.name + ')')

    # Attributes and Initialization: Ports
    for tlmPortName in self.tlmPorts.keys():
        portAttr = cxx_writer.Attribute(tlmPortName, cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"'), 'public')
        processorMembers.append(portAttr)
        portCtorValues = ['\"' + tlmPortName + '\"']
        if self.systemc and model.endswith('LT') and not model.startswith('acc'):
            portCtorValues.append('this->quant_keeper')
        for memAl in self.memAlias:
            portCtorValues.append('this->R')
        processorCtorInit.append(tlmPortName + '(' + ', '.join(portCtorValues) + ')')
        if tlmPortName in self.abi.memories.keys():
            abiAttrs.append(cxx_writer.Attribute(tlmPortName, cxx_writer.Type('MemoryInterface', '#include \"memory.hpp\"').makeRef(), 'private'))
            abiCtorValues.append('this->' + tlmPortName)

    # Attributes and Initialization: Pins
    for pinPort in self.pins:
        if pinPort.systemc:
            pinPortTypeName = 'SC'
        else:
            pinPortTypeName = 'TLM'
        if pinPort.inbound:
            pinPortTypeName += 'InPin_'
        else:
            pinPortTypeName += 'OutPin_'
        pinPortType = cxx_writer.Type(pinPortTypeName + str(pinPort.portWidth), '#include \"externalPins.hpp\"')
        pinPortAttr = cxx_writer.Attribute(pinPort.name + '_pin', pinPortType, 'public')
        processorMembers.append(pinPortAttr)
        processorCtorInit.append(pinPort.name + '_pin(\"' + pinPort.name + '_pin\")')

    ## @} Attributes and Initialization: Pipeline, Storage and Interface
    #---------------------------------------------------------------------------
    ## @name Attributes and Initialization: Instructions
    #  @{

    # Instruction Attributes
    instructionsAttr = cxx_writer.Attribute('INSTRUCTIONS', InstructionTypePtr.makePointer(), 'private')
    processorMembers.append(instructionsAttr)

    # Base Instruction
    global instrAttrs, instrCtorParams, instrCtorValues
    instrAttrs = []
    instrCtorParams = []
    instrCtorValues = ''
    processorCtorCode += '// Initialize the array containing the initial instance of the instructions.\n'
    instrMaxId = max([instr.id for instr in self.isa.instructions.values()]) + 1
    processorCtorCode += 'this->INSTRUCTIONS = new Instruction*[' + str(instrMaxId + 1) + '];\n'
    from registerWriter import registerContainerType
    if (self.regs or self.regBanks):
        instrAttrs.append(cxx_writer.Attribute('R', registerContainerType.makeRef(), 'public'))
        instrCtorParams.append(cxx_writer.Parameter('R', registerContainerType.makeRef()))
        instrCtorValues += 'R, '
    if self.memory:
        instrAttrs.append(cxx_writer.Attribute(self.memory[0], cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"').makeRef(), 'public'))
        instrCtorParams.append(cxx_writer.Parameter(self.memory[0], cxx_writer.Type('LocalMemory').makeRef()))
        instrCtorValues += self.memory[0] + ', '
    for tlmPort in self.tlmPorts.keys():
        instrAttrs.append(cxx_writer.Attribute(tlmPort, cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"').makeRef(), 'public'))
        instrCtorParams.append(cxx_writer.Parameter(tlmPort, cxx_writer.Type('TLMMemory').makeRef()))
        instrCtorValues += tlmPort + ', '
    for pinPort in self.pins:
        if pinPort.systemc:
            pinPortTypeName = 'SC'
        else:
            pinPortTypeName = 'TLM'
        if pinPort.inbound:
            pinPortTypeName += 'InPin_'
        else:
            pinPortTypeName += 'OutPin_'
        pinPortType = cxx_writer.Type(pinPortTypeName + str(pinPort.portWidth), '#include \"externalPins.hpp\"')
        # TODO: Remove this restriction.
        if not pinPort.inbound:
            instrAttrs.append(cxx_writer.Attribute(pinPort.name + '_pin', pinPortType.makeRef(), 'public'))
            instrCtorParams.append(cxx_writer.Parameter(pinPort.name + '_pin', pinPortType.makeRef()))
            instrCtorValues += pinPort.name + '_pin, '
    if trace and not self.systemc and not model.startswith('acc'):
        instrAttrs.append(cxx_writer.Attribute('total_cycles', cxx_writer.uintType.makeRef(), 'public'))
        instrCtorParams.append(cxx_writer.Parameter('total_cycles', cxx_writer.uintType.makeRef()))
        instrCtorValues += 'total_cycles, '

    if instrCtorValues: instrCtorValues = instrCtorValues[:-2]

    # Individual Instructions
    for name, instr in self.isa.instructions.items():
        processorCtorCode += 'this->INSTRUCTIONS[' + str(instr.id) + '] = new ' + name + '(' + instrCtorValues + ');\n'
    processorCtorCode += 'this->INSTRUCTIONS[' + str(instrMaxId) + '] = new InvalidInstruction(' + instrCtorValues + ');\n'
    if model.startswith('acc'):
        NOPIntructionType = cxx_writer.Type('NOPInstruction', '#include \"instructions.hpp\"')
        NOPinstructionsAttribute = cxx_writer.Attribute('NOP_instr', NOPIntructionType.makePointer(), 'public', True)
        processorMembers.append(NOPinstructionsAttribute)
        processorCtorCode += 'if (' + processor_name + '::num_instances == 1) {\n'
        processorCtorCode += processor_name + '::NOP_instr = new NOPInstruction(' + instrCtorValues + ');\n'
        for pipeStage in self.pipes:
            processorCtorCode += pipeStage.name + '_stage.NOP_instr = ' + processor_name + '::NOP_instr;\n'
        processorCtorCode += '}\n'
    for irq in self.irqs:
        processorCtorCode += 'this->' + irq.name + '_instr = new ' + irq.name + 'IntrInstruction(' + instrCtorValues + ', this->' + irq.name + ');\n'
        if model.startswith('acc'):
            for pipeStage in self.pipes:
                processorCtorCode += 'this->' + pipeStage.name + '_stage.' + irq.name + '_instr = this->' + irq.name + '_instr;\n'

    numInstrAttr = cxx_writer.Attribute('num_instructions', cxx_writer.uintType, 'public')
    processorMembers.append(numInstrAttr)
    processorCtorCode += 'this->num_instructions = 0;\n'
    instrExecutingAttr = cxx_writer.Attribute('instr_executing', cxx_writer.boolType, 'private')
    processorMembers.append(instrExecutingAttr)
    abiAttrs.append(cxx_writer.Attribute('instr_executing', cxx_writer.boolType.makeRef(), 'private'))
    abiCtorValues.append('this->instr_executing')
    if self.systemc:
        instrEndEventAttr = cxx_writer.Attribute('instr_end_event', cxx_writer.sc_eventType, 'private')
        processorMembers.append(instrEndEventAttr)
        abiAttrs.append(cxx_writer.Attribute('instr_end_event', cxx_writer.sc_eventType.makeRef(), 'private'))
        abiCtorValues.append('this->instr_end_event')

    if self.instructionCache:
        cacheAttr = cxx_writer.Attribute('instr_cache', cxx_writer.TemplateType('template_map',
                         [fetchWordType, CacheElemType], hash_map_include), 'private')
        processorMembers.append(cacheAttr)

    # Interrupt Instructions
    for irq in self.irqs:
        IRQInstrType = cxx_writer.Type(irq.name + 'IntrInstruction', '#include \"instructions.hpp\"')
        IRQInstrAttr = cxx_writer.Attribute(irq.name + '_instr', IRQInstrType.makePointer(), 'public')
        processorMembers.append(IRQInstrAttr)

    ## @} Attributes and Initialization: Instructions
    #---------------------------------------------------------------------------
    ## @name Attributes and Initialization: Tools
    #  @{

    # Attributes and Initialization: Tools
    toolManagerAttr = cxx_writer.Attribute('tool_manager', ToolsManagerType, 'public')
    processorMembers.append(toolManagerAttr)

    # Attributes and Initialization: Profiler
    # Enable measuring the number of cycles spent between two program portions.
    # The elapsed SystemC time is divided by the processor frequency.
    if self.systemc or model.startswith('acc') or model.endswith('AT'):
        profilerTimeStartAttr = cxx_writer.Attribute('profiler_time_start', cxx_writer.sc_timeType, 'public')
        processorMembers.append(profilerTimeStartAttr)
        processorCtorCode += 'this->profiler_time_start = SC_ZERO_TIME;\n'
        profilerTimeEndAttr = cxx_writer.Attribute('profiler_time_end', cxx_writer.sc_timeType, 'public')
        processorMembers.append(profilerTimeEndAttr)
        processorCtorCode += 'this->profiler_time_end = SC_ZERO_TIME;\n'
    if self.systemc and model.startswith('func'):
        profilerAddrStartAttr = cxx_writer.Attribute('profiler_start_addr', fetchWordType, 'private')
        processorMembers.append(profilerAddrStartAttr)
        processorCtorCode += 'this->profiler_start_addr = (' + str(fetchWordType) + ')-1;\n'
        profilerAddrEndAttr = cxx_writer.Attribute('profiler_end_addr', fetchWordType, 'private')
        processorMembers.append(profilerAddrEndAttr)
        processorCtorCode += 'this->profiler_end_addr = (' + str(fetchWordType) + ')-1;\n'

    # Attributes and Initialization: History
    if model.startswith('func'):
        histEnabledAttr = cxx_writer.Attribute('history_en', cxx_writer.boolType, 'private')
        processorMembers.append(histEnabledAttr)
        processorCtorCode += 'this->history_en = false;\n'
        instrHistFileAttr = cxx_writer.Attribute('history_file', cxx_writer.ofstreamType, 'private')
        processorMembers.append(instrHistFileAttr)

    instrHistType = cxx_writer.Type('HistoryInstrType', 'modules/instruction.hpp')
    instrHistQueueType = cxx_writer.TemplateType('boost::circular_buffer', [instrHistType], 'boost/circular_buffer.hpp')
    abiAttrs.append(cxx_writer.Attribute('history_instr_queue', instrHistQueueType.makeRef(), 'private'))
    if model.startswith('func'):
        instrHistoryQueueAttr = cxx_writer.Attribute('history_instr_queue', instrHistQueueType, 'public')
        processorMembers.append(instrHistoryQueueAttr)
        processorCtorCode += 'this->history_instr_queue.set_capacity(1000);\n'
        abiCtorValues.append('this->history_instr_queue')
    else:
        abiCtorValues.append('this->' + fetchStage.name + '_stage.history_instr_queue')

    if model.startswith('func'):
        undumpedHistElemsAttr = cxx_writer.Attribute('history_undumped_elements', cxx_writer.uintType, 'public')
        processorMembers.append(undumpedHistElemsAttr)
        processorCtorCode += 'this->history_undumped_elements = 0;\n'

    # Attributes and Initialization: Loader
    entryPointAttr = cxx_writer.Attribute('ENTRY_POINT', fetchWordType, 'public')
    processorMembers.append(entryPointAttr)
    processorCtorCode += 'this->ENTRY_POINT = 0;\n'
    mprocIdAttr = cxx_writer.Attribute('MPROC_ID', fetchWordType, 'public')
    processorMembers.append(mprocIdAttr)
    processorCtorCode += 'this->MPROC_ID = 0;\n'
    programLimitAttr = cxx_writer.Attribute('PROGRAM_LIMIT', fetchWordType, 'public')
    processorMembers.append(programLimitAttr)
    processorCtorCode += 'this->PROGRAM_LIMIT = 0;\n'
    abiAttrs.append(cxx_writer.Attribute('PROGRAM_LIMIT', fetchWordType.makeRef(), 'private'))
    abiCtorValues.append('this->PROGRAM_LIMIT')
    programStartAttr = cxx_writer.Attribute('PROGRAM_START', fetchWordType, 'public')
    processorMembers.append(programStartAttr)
    processorCtorCode += 'this->PROGRAM_START = 0;\n'

    # Attributes and Initialization: ABI
    if self.abi:
        abiAttr = cxx_writer.Attribute('ABIIf', interfaceType.makePointer(), 'public')
        processorMembers.append(abiAttr)
        processorCtorCode += 'this->ABIIf = new ' + str(interfaceType) + '(' + ', '.join(abiCtorValues) + ');\n'

    ## @} Attributes and Initialization: Tools
    #---------------------------------------------------------------------------
    ## @name Constructors and Destructors
    #  @{

    # Constructor
    processorCtor = cxx_writer.Constructor(cxx_writer.Code(processorCtorCode + 'end_module();\n'), 'public', processorCtorParams, processorCtorInit)

    # Destructor
    Code = processor_name + """::num_instances--;
    for (int i = 0; i < """ + str(instrMaxId + 1) + """; i++) {
        delete this->INSTRUCTIONS[i];
    }
    delete [] this->INSTRUCTIONS;
    """
    if model.startswith('acc'):
        Code += 'if (' + processor_name + '::num_instances == 0) {\n'
        Code += 'delete ' + processor_name + '::NOP_instr;\n'
        Code += '}\n'
    if self.instructionCache and not model.startswith('acc'):
        Code += """template_map<""" + str(fetchWordType) + """, CacheElem>::const_iterator cache_it, cache_end;
        for (cache_it = this->instr_cache.begin(), cache_end = this->instr_cache.end(); cache_it != cache_end; cache_it++) {
            delete cache_it->second.instr;
        }
        """
    for irq in self.irqs:
        Code += 'delete this->' + irq.name + '_instr;\n'
    # Now, before the processor elements is destructed I have to make sure that the history dump file is correctly closed
    if model.startswith('func'):
        Code += """#ifdef ENABLE_HISTORY
        if (this->history_en) {
            // In case a queue dump file has been specified, check if it needs to be saved.
            if (this->history_file) {
                if (this->history_undumped_elements > 0) {
                    std::vector<std::string> history_vector;
                    boost::circular_buffer<HistoryInstrType>::const_reverse_iterator history_it, history_end;
                    unsigned history_read = 0;
                    for (history_read = 0, history_it = this->history_instr_queue.rbegin(), history_end = this->history_instr_queue.rend(); history_it != history_end && history_read < this->history_undumped_elements; history_it++, history_read++) {
                        history_vector.push_back(history_it->get_mnemonic());
                    }
                    std::vector<std::string>::const_reverse_iterator history_vector_it, history_vector_end;
                    for (history_vector_it = history_vector.rbegin(), history_vector_end = history_vector.rend(); history_vector_it != history_vector_end; history_vector_it++) {
                        this->history_file << *history_vector_it << std::endl;
                    }
                }
                this->history_file.flush();
                this->history_file.close();
            }
        }
        #endif
        """
    if self.abi:
        Code += 'delete this->ABIIf;\n'
    processorDtor = cxx_writer.Destructor(cxx_writer.Code(Code), 'public')

    ## @} Constructors and Destructors
    #---------------------------------------------------------------------------
    ## @name Behavior Methods: main_loop()
    #  @{

    if not model.startswith('acc'):
        Code = ''
        if self.systemc:
            Code += '// Wait for SystemC infrastructure, otherwise register callbacks will crash.\nwait(SC_ZERO_TIME);\n'
        if self.instructionCache:
            # Declaration of the instruction buffer for speeding up decoding
            Code += 'template_map<' + str(self.bitSizes[1]) + ', CacheElem >::iterator icache_end = this->instr_cache.end();\n\n'

        Code += 'while(true) {\n'

        # Here is the code to notify start of the instruction execution
        Code += 'this->instr_executing = true;\n'
        Code += 'unsigned num_cycles = 0;\n'

        # Here is the code to deal with interrupts
        Code += getInterruptCode(self, trace)
        if self.irqs:
            Code += 'else {\n'
        # computes the address from which the next instruction shall be fetched
        Code += getFetchAddressCode(self, model)

        # Lets insert the code to keep statistics
        if self.systemc:
            Code += """if (cur_PC == this->profiler_start_addr) {
                this->profiler_time_start = sc_time_stamp();
            }
            if (cur_PC == this->profiler_end_addr) {
                this->profiler_time_end = sc_time_stamp();
            }
            """

        # Lets start with the code for the instruction queue
        Code += """#ifdef ENABLE_HISTORY
        HistoryInstrType instr_queue_elem;
        if (this->history_en) {
        """
        if len(self.tlmPorts) > 0 and model.endswith('LT'):
            Code += 'instr_queue_elem.cycle = (unsigned)(this->quant_keeper.get_current_time()/this->latency);'
        elif model.startswith('acc') or self.systemc or model.endswith('AT'):
            Code += 'instr_queue_elem.cycle = (unsigned)(sc_time_stamp()/this->latency);'
        Code += """
            instr_queue_elem.address = cur_PC;
        }
        #endif
        """

        # computes the correct memory and/or memory port from which fetching the instruction stream
        doFetchCode = getDoFetchCode(self)
        # We need to fetch the instruction ... only if the cache is not used or if
        # the index of the cache is the current instruction
        if not (self.instructionCache and self.fastFetch):
            Code += doFetchCode
        if trace:
            Code += 'std::cerr << \"Current PC: \" << std::hex << std::showbase << cur_PC << \'.\' << std::endl;\n'

        # Finally I declare the fetch, decode, execute loop, where the instruction is actually executed;
        # Note the possibility of performing it with the instruction fetch
        Code += 'Instruction* cur_instr = NULL;\n'
        if self.instructionCache:
            Code += getCacheInstrFetchCode(self, doFetchCode, trace, combinedTrace, getInstrIssueCode)
        else:
            Code += getDoDecodeCode(self, trace, combinedTrace, getInstrIssueCode)

        for pipeStage in self.pipes:
            # User-defined operations are executed after the regular instruction behavior.
            if pipeStage.operation:
                Code += pipeStage.operation

        # Lets finish with the code for the instruction queue: I just still have to
        # check if it is time to save to file the instruction queue
        Code += """#ifdef ENABLE_HISTORY
        if (this->history_en) {
            // Add current instruction to history queue.
            this->history_instr_queue.push_back(instr_queue_elem);
            // In case a queue dump file has been specified, check if it needs to be saved.
            if (this->history_file) {
                this->history_undumped_elements++;
                if (history_undumped_elements == this->history_instr_queue.capacity()) {
                    boost::circular_buffer<HistoryInstrType>::const_iterator history_it, history_end;
                    for (history_it = this->history_instr_queue.begin(), history_end = this->history_instr_queue.end(); history_it != history_end; history_it++) {
                        this->history_file << history_it->get_mnemonic() << std::endl;
                    }
                    this->history_undumped_elements = 0;
                }
            }
        }
        #endif
        """

        if self.irqs:
            Code += '} // if (!IRQ)\n'
        if len(self.tlmPorts) > 0 and model.endswith('LT'):
            Code += 'this->quant_keeper.inc((num_cycles + 1)*this->latency);\nif (this->quant_keeper.need_sync()) {\nthis->quant_keeper.sync();\n}\n'
        elif model.startswith('acc') or self.systemc or model.endswith('AT'):
            Code += 'wait((num_cycles + 1)*this->latency);\n'
        else:
            Code += 'this->total_cycles += (num_cycles + 1);\n'

        # Here is the code to notify start of the instruction execution
        Code += 'this->instr_executing = false;\n'
        if self.systemc:
            Code += 'this->instr_end_event.notify();\n'

        Code += 'this->num_instructions++;\n\n'
        # Now I have to call the update method for all the delayed registers
        for reg in self.regs:
            if reg.delay:
                Code += reg.name + '.clock_cycle();\n'
        for reg in self.regBanks:
            for regNum in reg.delay.keys():
                Code += reg.name + '[' + str(regNum) + '].clock_cycle();\n'
        Code += '}'

        mainLoopBody = cxx_writer.Code(Code)
        mainLoopBody.addInclude(includes)
        mainLoopBody.addInclude('common/report.hpp')
        mainLoopMethod = cxx_writer.Method('main_loop', mainLoopBody, cxx_writer.voidType, 'public')
        processorMembers.append(mainLoopMethod)

    ## @} Behavior Methods: main_loop()
    #---------------------------------------------------------------------------
    ## @name Initialization and Configuration Methods
    #  @{

    # startup(), shutdown()
    if self.startup:
        startupMethod = cxx_writer.Method('startup', cxx_writer.Code(self.startup), cxx_writer.voidType, 'private')
        processorMembers.append(startupMethod)

    if self.shutdown:
        shutdownMethod = cxx_writer.Method('shutdown', cxx_writer.Code(self.shutdown), cxx_writer.voidType, 'private')
        processorMembers.append(shutdownMethod)

    # reset()
    if not self.reset:
        resetBody = cxx_writer.Code('')
    else:
        import copy
        resetBody = cxx_writer.Code(copy.deepcopy(self.reset))
    Code = ''
    if self.regs or self.regBanks:
        Code += 'this->R.reset();\n'
    # TODO: Ugly special case for the fetch register: Since ENTRY_POINT is
    # public and usually written after initialization, the reset value is stale.
    # I need to replace public variables with get/set methods or parameters.
    # NOTE: All stages up to the fetch stage are set [0..fetchStage] (in
    # practice the first one or two stages) as well as all stages [wbStage..
    # lastStage]
    for i in range(0, self.pipes.index(fetchStage)+1):
        Code += 'this->' + self.fetchReg[0] + '.set_stage(' + str(i) + ');\n'
        Code += 'this->' + self.fetchReg[0] + ' = this->ENTRY_POINT;\n'
    #for i in range(self.pipes.index(wbStage), len(self.pipes)):
    #    Code += 'this->' + self.fetchReg[0] + '.set_stage(' + str(i) + ');\n'
    #    Code += 'this->' + self.fetchReg[0] + ' = this->ENTRY_POINT;\n'
    Code += 'this->' + self.fetchReg[0] + '.unset_stage();\n'
    for irqPort in self.irqs:
        Code += 'this->' + irqPort.name + ' = 0;\n'
    resetBody.prependCode(Code + '\n')
    if self.startup:
        resetBody.appendCode('// User-defined initialization.\nthis->startup();\n')
    resetBody.appendCode('this->reset_called = true;')
    resetMethod = cxx_writer.Method('reset', resetBody, cxx_writer.voidType, 'public')
    processorMembers.append(resetMethod)

    # end_of_elaboration()
    endElabBody = cxx_writer.Code('if (!this->reset_called) {\nthis->reset();\n}')
    endElabMethod = cxx_writer.Method('end_of_elaboration', endElabBody, cxx_writer.voidType, 'public')
    processorMembers.append(endElabMethod)

    # set_profiling_range()
    if self.systemc and model.startswith('func'):
        setProfilingRangeBody = cxx_writer.Code('this->profiler_start_addr = start_addr;\nthis->profiler_end_addr = end_addr;')
        setProfilingRangeParams = [cxx_writer.Parameter('start_addr', fetchWordType), cxx_writer.Parameter('end_addr', fetchWordType)]
        setProfilingRangeMethod = cxx_writer.Method('set_profiling_range', setProfilingRangeBody, cxx_writer.voidType, 'public', setProfilingRangeParams)
        processorMembers.append(setProfilingRangeMethod)
    elif model.startswith('acc'):
        setProfilingRangeBody = cxx_writer.Code('this->' + fetchStage.name + '_stage.profiler_start_addr = start_addr;\nthis->' + fetchStage.name + '_stage.profiler_end_addr = end_addr;')
        setProfilingRangeParams = [cxx_writer.Parameter('start_addr', fetchWordType), cxx_writer.Parameter('end_addr', fetchWordType)]
        setProfilingRangeMethod = cxx_writer.Method('set_profiling_range', setProfilingRangeBody, cxx_writer.voidType, 'public', setProfilingRangeParams)
        processorMembers.append(setProfilingRangeMethod)

    # enable_history()
    if model.startswith('acc'):
        enableHistoryBody = cxx_writer.Code('this->' + fetchStage.name + '_stage.history_en = true;\nthis->' + fetchStage.name + '_stage.history_file.open(file_name.c_str(), ios::out | ios::ate);')
        enableHistoryParams = [cxx_writer.Parameter('file_name', cxx_writer.stringType, initValue = '""')]
        enableHistoryMethod = cxx_writer.Method('enable_history', enableHistoryBody, cxx_writer.voidType, 'public', enableHistoryParams)
        processorMembers.append(enableHistoryMethod)
    else:
        enableHistoryBody = cxx_writer.Code('this->history_en = true;\nthis->history_file.open(file_name.c_str(), ios::out | ios::ate);')
        enableHistoryParams = [cxx_writer.Parameter('file_name', cxx_writer.stringType, initValue = '""')]
        enableHistoryMethod = cxx_writer.Method('enable_history', enableHistoryBody, cxx_writer.voidType, 'public', enableHistoryParams)
        processorMembers.append(enableHistoryMethod)

    ## @} Initialization and Configuration Methods
    #---------------------------------------------------------------------------
    ## @name Information and Helper Methods
    #  @{

    # decode()
    if not model.startswith('acc'):
        decoderAttr = cxx_writer.Attribute('decoder', cxx_writer.Type('Decoder', '#include \"decoder.hpp\"'), 'private')
        processorMembers.append(decoderAttr)

    if model.startswith('acc'):
        Code = 'int instr_id = this->' + fetchStage.name + '_stage.decoder.decode(bitstring);\n'
    else:
        Code = 'int instr_id = this->decoder.decode(bitstring);\n'
    Code += """if (instr_id >= 0) {
                        Instruction* instr = this->INSTRUCTIONS[instr_id];
                        instr->set_params(bitstring);
                        return instr;
                    }
                    return NULL;
                  """
    decodeBody = cxx_writer.Code(Code)
    decodeParams = [cxx_writer.Parameter('bitstring', fetchWordType)]
    decodeMethod = cxx_writer.Method('decode', decodeBody, InstructionTypePtr, 'public', decodeParams)
    processorMembers.append(decodeMethod)

    # get_interface()
    if self.abi:
        getInterfaceBody = cxx_writer.Code('return *this->ABIIf;')
        getInterfaceMethod = cxx_writer.Method('get_interface', getInterfaceBody, interfaceType.makeRef(), 'public')
        processorMembers.append(getInterfaceMethod)

    ## @} Information and Helper Methods
    #---------------------------------------------------------------------------

    processorClass = cxx_writer.SCModule(processor_name, processorMembers, namespaces = [namespace])
    processorClass.addDocString(brief = 'Processor Class', detail = 'The top-level processor class holding the pipeline, registers and ports.')
    processorClass.addConstructor(processorCtor)
    processorClass.addDestructor(processorDtor)
    return [processorClass]

################################################################################
# Testbench Model
################################################################################
def getCPPMain(self, model, namespace):
    """Returns the code which instantiate the processor
    in order to execute simulations"""
    wordType = self.bitSizes[1]
    code = 'using namespace ' + namespace + ';\nusing namespace trap;\n\n'
    code += 'std::cerr << banner << std::endl;\n'
    code += """
    boost::program_options::options_description desc("Processor simulator for """ + self.name + """", 120);
    desc.add_options()
        ("help,h", "produces the help message")
    """
    if self.abi:
        code += """("debugger,d", "activates the use of the software debugger")
        ("profiler,p", boost::program_options::value<std::string>(),
            "activates the use of the software profiler, specifying the name of the output file")
        ("prof_range,g", boost::program_options::value<std::string>(),
            "specifies the range of addresses restricting the profiler instruction statistics")
        ("disable_fun_prof,n", "disables profiling statistics for the application routines")
        """
    if self.systemc or model.startswith('acc') or model.endswith('AT'):
        code += """("frequency,f", boost::program_options::value<double>(),
                    "processor clock frequency specified in MHz [Default 1MHz]")
                    ("cycles_range,c", boost::program_options::value<std::string>(),
                    "start-end addresses between which computing the execution cycles")
        """
    code += """("application,a", boost::program_options::value<std::string>(),
                                    "application to be executed on the simulator")
               ("disassembler,i", "prints the disassembly of the application")
               ("history,y", boost::program_options::value<std::string>(),
                            "prints on the specified file the instruction history")
            """
    if self.abi:
        code += """("arguments,r", boost::program_options::value<std::string>(),
                    "command line arguments (if any) of the application being simulated - comma separated")
            ("environment,e", boost::program_options::value<std::string>(),
                "environmental variables (if any) visible to the application being simulated - comma separated")
            ("sysconf,s", boost::program_options::value<std::string>(),
                    "configuration information (if any) visible to the application being simulated - comma separated")
        """
    code += """;

    std::cerr << std::endl;

    boost::program_options::variables_map vm;
    try {
        boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
    }
    catch(boost::program_options::invalid_command_line_syntax& e) {
        std::cerr << "Cannot parse command line parameters." << std::endl << std::endl;
        std::cerr << e.what() << std::endl << std::endl;
        std::cerr << desc << std::endl;
        return -1;
    }
    catch(boost::program_options::validation_error& e) {
        std::cerr << "Cannot parse command line parameters." << std::endl << std::endl;
        std::cerr << e.what() << std::endl << std::endl;
        std::cerr << desc << std::endl;
        return -1;
    }
    catch(boost::program_options::error& e) {
        std::cerr << "Cannot parse command line parameters." << std::endl << std::endl;
        std::cerr << e.what() << std::endl << std::endl;
        std::cerr << desc << std::endl;
        return -1;
    }
    boost::program_options::notify(vm);

    // Check that the parameters are specified correctly.
    if (vm.count("help") != 0) {
        std::cout << desc << std::endl;
        return 0;
    }
    if (vm.count("application") == 0) {
        std::cerr << "Use the --application command line option to specify the application to be simulated." << std::endl << std::endl;
        std::cerr << desc << std::endl;
        return -1;
    }
    """
    if (self.systemc or model.startswith('acc') or model.endswith('AT')) and not self.externalClock:
        code += """double latency = 1; // 1us
        if (vm.count("frequency") != 0) {
            latency = 1/(vm["frequency"].as<double>());
        }
        // Proceed with the actual instantiation of the processor.
        """ + processor_name + """ processor(\"""" + self.name + """\", sc_time(latency, SC_US));
        """
    else:
        code += """
        // Proceed with the actual instantiation of the processor.
        """ + processor_name + """ processor(\"""" + self.name + """\");
        """
    instrMemName = ''
    instrDissassName = ''
    if len(self.tlmPorts) > 0:
        code += """// Instantiate the memory and connect it to the processor.\n"""
        if self.tlmFakeMemProperties and self.tlmFakeMemProperties[2]:
            code += 'SparseMemory'
        else:
            code += 'Memory'
        if model.endswith('LT'):
            code += 'LT'
        else:
            code += 'AT'
        code += '<' + str(len(self.tlmPorts)) + """, """ + str(self.wordSize*self.byteSize) + """> mem("procMem", """
        if self.tlmFakeMemProperties:
            code += str(self.tlmFakeMemProperties[0]) + ', sc_time(latency*' + str(self.tlmFakeMemProperties[1]) + ', SC_US));\n'
        else:
            code += """1024*1024*10, sc_time(latency*2, SC_US));
            """
        numPort = 0
        for tlmPortName, fetch in self.tlmPorts.items():
            code += 'processor.' + tlmPortName + '.init_socket.bind(*(mem.target_socket[' + str(numPort) + ']));\n'
            numPort += 1
            if fetch:
                instrMemName = 'mem'
                instrDissassName = 'processor.' + tlmPortName
    if instrMemName == '' and self.memory:
        instrMemName = 'processor.' + self.memory[0]
        instrDissassName = instrMemName

    code += """
    std::cout << std::endl << "Loading the application and initializing the tools ..." << std::endl;
    // Load the executable code.
    boost::filesystem::path application_path = boost::filesystem::system_complete(boost::filesystem::path(vm["application"].as<std::string>()));
    if (!boost::filesystem::exists(application_path)) {
        std::cerr << "Application " << vm["application"].as<std::string>() << " does not exist." << std::endl;
        return -1;
    }
    ExecLoader loader(vm["application"].as<std::string>());
    // Copy the binary code into memory.
    unsigned char* program_data = loader.get_program_data();
    unsigned program_dim = loader.get_program_dim();
    unsigned program_data_start = loader.get_data_start();
    for (unsigned i = 0; i < program_dim; i++) {
        """ + instrMemName + """.write_byte_dbg(program_data_start + i, program_data[i]);
    }
    if (vm.count("disassembler") != 0) {
        std::cout << "Entry Point: " << std::hex << std::showbase << loader.get_program_start() << std::endl << std::endl;
        for (unsigned i = 0; i < program_dim; i+= """ + str(self.wordSize) + """) {
            Instruction* cur_instr = processor.decode(""" + instrDissassName + """.read_word_dbg(loader.get_data_start() + i));
            std::cout << std::hex << std::noshowbase << std::setw(8) << std::setfill(' ') << program_data_start + i << ":   " << std::setw(8) << std::setfill('0') << """ + instrDissassName + """.read_word_dbg(program_data_start + i);
            if (cur_instr != NULL) {
                 std::cout << "    " << cur_instr->get_mnemonic();
            }
            std::cout << std::endl;
        }
        return 0;
    }

    // Set the processor variables.
    processor.ENTRY_POINT = loader.get_program_start();
    processor.PROGRAM_LIMIT = program_dim + program_data_start;
    processor.PROGRAM_START = program_data_start;
    """

    for irq in self.irqs:
      code += 'TLMIntrInitiator_' + str(irq.portWidth) + ' ' + irq.name + '_initiator(\"' + irq.name + '_initiator\");\n' + irq.name + '_initiator.init_socket.bind(processor.' + irq.name + '_port.target_socket);\n'

    if self.systemc or model.startswith('acc') or model.endswith('AT'):
        code += """// Check if counting the cycles between two locations (addresses or symbols) is required.
        std::pair<""" + str(wordType) + ', ' + str(wordType) + """> decoded_range((""" + str(wordType) + """)-1, (""" + str(wordType) + """)-1);
        if (vm.count("cycles_range") != 0) {
            // The range is in the form start-end, where start and end can be both integers (both normal and hex) or symbols of the binary file.
            std::string cycles_range = vm["cycles_range"].as<std::string>();
            decoded_range = get_cycle_range(cycles_range, vm["application"].as<std::string>());
            // Initialize the processor with the given address range values.
            processor.set_profiling_range(decoded_range.first, decoded_range.second);
        }
        """
    code += """
    // Initialize the instruction history management. Note that both need to be enabled if the debugger is being used and/or if history needs to be dumped to an output file.
    if (vm.count("debugger") > 0) {
        processor.enable_history();
    }
    if (vm.count("history") > 0) {
        #ifndef ENABLE_HISTORY
        std::cout << std::endl << "Unable to initialize instruction history as it has been disabled at compilation time." << std::endl << std::endl;
        #endif
        processor.enable_history(vm["history"].as<std::string>());
    }
    """
    if self.abi:
        code += """
        // Initialize the tools (e.g. debugger, os emulator, etc).
        """
        #if model.startswith('acc'):
            #code += 'OSEmulatorCA<' + str(wordType) + ', -' + str(execOffset*self.wordSize) + '> os_emulator(processor.ABIIf, Processor::NOP_instr, ' + str(self.abi.emulOffset) + ');\n'
        #else:
        code += 'OSEmulator<' + str(wordType) + '> os_emulator(processor.ABIIf);\n'
        code += """GDBStub<""" + str(wordType) + """> gdb_stub(processor.ABIIf);
        Profiler<""" + str(wordType) + """> profiler(processor.ABIIf, vm["application"].as<std::string>(), vm.count("disable_fun_prof") > 0);

        os_emulator.init_sys_calls(vm["application"].as<std::string>());
        std::vector<std::string> options;
        options.push_back(vm["application"].as<std::string>());
        if (vm.count("arguments") > 0) {
            // Parse the environment. The options are in the form 'option,option,...'
            std::string packedOpts = vm["arguments"].as<std::string>();
            while(packedOpts.size() > 0) {
                std::size_t foundComma = packedOpts.find(',');
                if (foundComma != std::string::npos) {
                    options.push_back(packedOpts.substr(0, foundComma));
                    packedOpts = packedOpts.substr(foundComma + 1);
                } else {
                    options.push_back(packedOpts);
                    break;
                }
            }
        }
        os_emulator.set_program_args(options);
        if (vm.count("environment") > 0) {
            // Parse the environment. The options are in the form 'option=value,option=value,...'
            std::string packedEnv = vm["environment"].as<std::string>();
            while(packedEnv.size() > 0) {
                std::size_t foundComma = packedEnv.find(',');
                std::string curEnv;
                if (foundComma != std::string::npos) {
                    curEnv = packedEnv.substr(0, foundComma);
                    packedEnv = packedEnv.substr(foundComma + 1);
                } else {
                    curEnv = packedEnv;
                    packedEnv = "";
                }
                // Split the current environment.
                std::size_t equalPos = curEnv.find('=');
                if (equalPos == std::string::npos) {
                    std::cerr << "Invalid command line environment option: Expected 'var=value', got " << curEnv << '.' << std::endl;
                    return -1;
                }
                os_emulator.set_env(curEnv.substr(0, equalPos), curEnv.substr(equalPos + 1));
            }
        }
        if (vm.count("sysconf") > 0) {
            // Parse the environment. The options are in the form 'option=value,option=value,...'
            std::string packedEnv = vm["sysconf"].as<std::string>();
            while(packedEnv.size() > 0) {
                std::size_t foundComma = packedEnv.find(',');
                std::string curEnv;
                if (foundComma != std::string::npos) {
                    curEnv = packedEnv.substr(0, foundComma);
                    packedEnv = packedEnv.substr(foundComma + 1);
                } else {
                    curEnv = packedEnv;
                    packedEnv = "";
                }
                // Split the current environment.
                std::size_t equalPos = curEnv.find('=');
                if (equalPos == std::string::npos) {
                    std::cerr << "Invalid command line sysconf option: Expected 'var=value', got " << curEnv << '.' << std::endl;
                    return -1;
                }
                try {
                    os_emulator.set_sysconf(curEnv.substr(0, equalPos), boost::lexical_cast<int>(curEnv.substr(equalPos + 1)));
                }
                catch(...) {
                    std::cerr << "Invalid command line sysconf option: Expected 'var=value', got " << curEnv << '.' << std::endl;
                    return -1;
                }
            }
        }
        processor.tool_manager.add_tool(os_emulator);
        if (vm.count("debugger") != 0) {
            processor.tool_manager.add_tool(gdb_stub);
            gdb_stub.initialize();
    """
        for tlmPortName in self.tlmPorts.keys():
            code += 'processor.' + tlmPortName + '.set_debugger(&gdb_stub);\n'
        if self.memory:
            code += 'processor.' + self.memory[0] + '.set_debugger(&gdb_stub);\n'
        code += 'gdb_stub_ref = &gdb_stub;\n}\n'
    code += """if (vm.count("profiler") != 0) {
                std::set<std::string> toIgnoreFuns = os_emulator.get_registered_functions();
                toIgnoreFuns.erase("main");
                profiler.add_ignored_functions(toIgnoreFuns);
                // Check if the profiler needs to be restricted to a specific cycle range.
                if (vm.count("prof_range") != 0) {
                    std::pair<""" + str(wordType) + ', ' + str(wordType) + """> decodedProfRange = get_cycle_range(vm["prof_range"].as<std::string>(), vm["application"].as<std::string>());
                    // The range is in the form start-end, where start and end can be both integers (both normal and hex) or symbols of the binary file.
                    profiler.set_profiling_range(decodedProfRange.first, decodedProfRange.second);
                }
                processor.tool_manager.add_tool(profiler);
            }

    // Register the signal handlers for the CTRL^C key combination.
    (void) signal(SIGINT, stop_simulation);
    (void) signal(SIGTERM, stop_simulation);
    (void) signal(10, stop_simulation);

    std::cout << "Initialized tools." << std::endl;

    // Start execution.
    boost::timer t;
    sc_start();
    double elapsed_sec = t.elapsed();
    if (vm.count("profiler") != 0) {
        profiler.print_csv_stats(vm["profiler"].as<std::string>());
    }
    std::cout << std::endl << "Elapsed real time:" << elapsed_sec << 's' << std::endl;
    std::cout << "Executed Instructions: " << processor.num_instructions << std::endl;
    std::cout << "Execution speed: " << (double)processor.num_instructions/(elapsed_sec*1e6) << "MIPS" << std::endl;
    """
    if self.systemc or model.startswith('acc') or model.endswith('AT'):
        code +="""std::cout << "Simulated time: " << ((sc_time_stamp().to_default_time_units())/(sc_time(1, SC_US).to_default_time_units())) << "us" << std::endl;
        std::cout << "Elapsed time: " << std::dec << (unsigned)(sc_time_stamp()/sc_time(latency, SC_US)) << " cycles" << std::endl;
        if (decoded_range.first != (""" + str(wordType) + """)-1 || decoded_range.second != (""" + str(wordType) + """)-1) {
            if (processor.profiler_time_end == SC_ZERO_TIME) {
                processor.profiler_time_end = sc_time_stamp();
                std::cout << "End address: " << std::hex << std::showbase << decoded_range.second << " not found, counting until the end." << std::endl;
            }
            std::cout << "Cycles between addresses " << std::hex << std::showbase << decoded_range.first << " - " << decoded_range.second << ": " << std::dec << (unsigned)((processor.profiler_time_end - processor.profiler_time_start)/sc_time(latency, SC_US)) << std::endl;
        }
                """
    else:
        code += 'std::cout << "Elapsed time: " << std::dec << processor.total_cycles << " cycles" << std::endl;\n'
    code += 'std::cout << std::endl;\n'
    if self.shutdown:
        code += '// Simulation ended. Call cleanup methods.\processor.shutdown();\n'
    code += """
    return 0;
    """
    mainCode = cxx_writer.Code(code)
    mainCode.addInclude("""#ifdef _WIN32
#pragma warning(disable : 4101)
#endif
""")

    mainCode.addInclude('#define WIN32_LEAN_AND_MEAN')

    mainCode.addInclude('iostream')
    mainCode.addInclude('iomanip')
    mainCode.addInclude('string')
    mainCode.addInclude('vector')
    mainCode.addInclude('set')
    mainCode.addInclude('signal.h')

    mainCode.addInclude('tlm_utils/multi_passthrough_initiator_socket.h')
    mainCode.addInclude('boost/program_options.hpp')
    mainCode.addInclude('boost/timer.hpp')
    mainCode.addInclude('boost/filesystem.hpp')

    if model.endswith('LT'):
        if self.tlmFakeMemProperties and self.tlmFakeMemProperties[2]:
            mainCode.addInclude('modules/sparse_memory_lt.hpp')
        else:
            mainCode.addInclude('modules/memory_lt.hpp')
    else:
        if self.tlmFakeMemProperties and self.tlmFakeMemProperties[2]:
            mainCode.addInclude('modules/sparse_memory_at.hpp')
        else:
            mainCode.addInclude('modules/memory_at.hpp')
    mainCode.addInclude('#include \"processor.hpp\"')
    mainCode.addInclude('#include \"instructions.hpp\"')
    mainCode.addInclude('common/report.hpp')
    mainCode.addInclude('systemc.h')
    mainCode.addInclude('elfloader/elf_frontend.hpp')
    mainCode.addInclude('elfloader/exec_loader.hpp')
    mainCode.addInclude('stdexcept')
    if self.abi:
        mainCode.addInclude('debugger/gdb_stub.hpp')
        mainCode.addInclude('profiler/profiler.hpp')
        #if model.startswith('acc'):
            #mainCode.addInclude('osemu_ca.hpp')
        #else:
        mainCode.addInclude('osemu/osemu.hpp')
    parameters = [cxx_writer.Parameter('argc', cxx_writer.intType), cxx_writer.Parameter('argv', cxx_writer.charPtrType.makePointer())]
    mainFunction = cxx_writer.Function('sc_main', mainCode, cxx_writer.intType, parameters)
    mainFunction.addDocString(brief = 'Main Processor Testbench', detail = 'Instantiates a processor and performs basic connections where required. TRAP-Gen debugging and profiling tools as instantiated as chosen by the command-line options.')

    stop_simulation = cxx_writer.Code("""if (gdb_stub_ref != NULL && gdb_stub_ref->simulation_paused) {
        std::cerr << std::endl << "Cannot pause simulation, simulation already paused. Use the connected GDB debugger to control it." << std::endl << std::endl;
    } else {
        std::cerr << std::endl << "Interrupted the simulation." << std::endl << std::endl;
        sc_stop();
        wait(SC_ZERO_TIME);
    }""")
    parameters = [cxx_writer.Parameter('sig', cxx_writer.intType)]
    signalFunction = cxx_writer.Function('stop_simulation', stop_simulation, cxx_writer.voidType, parameters)

    getCycleRangeCode = cxx_writer.Code("""std::pair<""" + str(wordType) + ', ' + str(wordType) + """> decoded_range;
        std::size_t foundSep = cycles_range.find('-');
        if (foundSep == std::string::npos) {
            THROW_EXCEPTION("Invalid address range " << cycles_range << ", expected address range in the format start-end.");
        }
        std::string start = cycles_range.substr(0, foundSep);
        std::string end = cycles_range.substr(foundSep + 1);
        // First try decimal numbers, then hex, then check if a corresponding symbol exists. If neither works return an error.
        try {
            decoded_range.first = boost::lexical_cast<""" + str(wordType) + """>(start);
        }
        catch(...) {
            try {
                decoded_range.first = (unsigned)std::strtoul(start.c_str(), NULL, 0);
            }
            catch(...) {
                trap::ELFFrontend& elf_frontend = trap::ELFFrontend::get_instance(application);
                bool valid = true;
                decoded_range.first = elf_frontend.get_sym_addr(start, valid);
                if (!valid) {
                    THROW_EXCEPTION("Start address range " << start << " does not specify a valid address or a valid symbol.");
                }
            }
        }
        try {
            decoded_range.second = boost::lexical_cast<""" + str(wordType) + """>(end);
        }
        catch(...) {
            try {
                decoded_range.second = (unsigned)std::strtoul(end.c_str(), NULL, 0);
            }
            catch(...) {
                trap::ELFFrontend& elf_frontend = trap::ELFFrontend::get_instance(application);
                bool valid = true;
                decoded_range.second = elf_frontend.get_sym_addr(end, valid);
                if (!valid) {
                    THROW_EXCEPTION("End address range " << end << " does not specify a valid address or a valid symbol.");
                }
            }
        }

        return decoded_range;
    """)
    parameters = [cxx_writer.Parameter('cycles_range', cxx_writer.stringRefType.makeConst()),
            cxx_writer.Parameter('application', cxx_writer.stringRefType.makeConst())]
    wordPairType = cxx_writer.TemplateType('std::pair', [wordType, wordType])
    cycleRangeFunction = cxx_writer.Function('get_cycle_range', getCycleRangeCode, wordPairType, parameters)

    # Finally here lets declare the variable holding the global reference to the debugger
    debuggerType = cxx_writer.TemplateType('GDBStub', [wordType])
    debuggerVariable = cxx_writer.Variable('gdb_stub_ref', debuggerType.makePointer(), initValue = 'NULL')

    # and here the variable holding the printable banner
    bannerInit = 'std::string("\\n\\\n'
    for bannerLine in self.banner.split('\n'):
        bannerInit += '\\t' + bannerLine.replace('\\', '\\\\') + '\\n\\\n'
    bannerInit += '\\n\\n' + '\\t'
    for bannerLine in self.developer_name.split('\n'):
        bannerInit += '\\t' + bannerLine.replace('\\', '\\\\') + '\\n\\\n'
    if self.developer_email:
        bannerInit += '\t-\t email: ' + self.developer_email
    bannerInit += '\\n\\n")'

    bannerVariable = cxx_writer.Variable('banner', cxx_writer.stringType, initValue = bannerInit)

    """Returns the code for a stub initiator socket class to be connected with
    interrupt ports. Sadly, TLM does not implement SC_ZERO_OR_MORE_BOUND properly,
    so the interrupt ports have to be connected."""
    # Create socket classes for each unique port width.
    for portWidth in list(set([irq.portWidth for irq in self.irqs])):
        # multi_passthrough_initiator_socket init_socket;
        tlmInitiatorSocketMember = cxx_writer.Attribute('init_socket', cxx_writer.Type('tlm_utils::multi_passthrough_initiator_socket<TLMIntrInitiator_' + str(portWidth) + ', ' + str(portWidth) + ', tlm::tlm_base_protocol_types>'), visibility = 'public', initValue = '\"init_socket\"')
        # nb_transport_bw() {}
        code = """return tlm::TLM_COMPLETED;"""
        tlmInitiatorCode = cxx_writer.Code(code)
        parameters = [cxx_writer.Parameter('tag', cxx_writer.intType), cxx_writer.Parameter('payload', cxx_writer.Type('tlm::tlm_generic_payload').makeRef()), cxx_writer.Parameter('phase', cxx_writer.Type('tlm::tlm_phase').makeRef()), cxx_writer.Parameter('delay', cxx_writer.Type('sc_core::sc_time').makeRef())]
        tlmInitiatorNBMethod = cxx_writer.Method('nb_transport_bw', tlmInitiatorCode, cxx_writer.Type('tlm::tlm_sync_enum'), 'public', parameters)
        # invalidate_direct_mem_ptr() {}
        tlmInitiatorCode = cxx_writer.Code('')
        parameters = [cxx_writer.Parameter('tag', cxx_writer.intType), cxx_writer.Parameter('start_range', cxx_writer.Type('sc_dt::uint64')), cxx_writer.Parameter('end_range', cxx_writer.Type('sc_dt::uint64'))]
        tlmInitiatorDMMethod = cxx_writer.Method('invalidate_direct_mem_ptr', tlmInitiatorCode, cxx_writer.voidType, 'public', parameters)
        # initiator_<portWidth>() {}
        code = 'init_socket.register_nb_transport_bw(this, &TLMIntrInitiator_' + str(portWidth) + '::nb_transport_bw);\ninit_socket.register_invalidate_direct_mem_ptr(this, &TLMIntrInitiator_' + str(portWidth) + '::invalidate_direct_mem_ptr);'
        tlmInitiatorCode = cxx_writer.Code(code)
        tlmInitiatorCtor = cxx_writer.Constructor(tlmInitiatorCode, visibility = 'public', parameters = [cxx_writer.Parameter('_name', cxx_writer.Type('sc_core::sc_module_name'))], initList = ['sc_module(_name)', 'init_socket(\"init_socket\")'])
        # class initiator_<portWidth> {};
        tlmInitiatorClass = cxx_writer.ClassDeclaration('TLMIntrInitiator_'+ str(portWidth), [tlmInitiatorSocketMember, tlmInitiatorNBMethod, tlmInitiatorDMMethod], [cxx_writer.Type('sc_core::sc_module')])
        tlmInitiatorClass.addConstructor(tlmInitiatorCtor)

    if self.irqs:
        return [bannerVariable, debuggerVariable, signalFunction, cycleRangeFunction, tlmInitiatorClass, mainFunction]
    else:
        return [bannerVariable, debuggerVariable, signalFunction, cycleRangeFunction, mainFunction]

################################################################################
# Testbench Model
################################################################################
def getCPPTestMain(self):
    """Returns the code for a formatting class called from within the boost
    test framework that outputs values in hex."""
    code = """output << std::showbase << std::hex << value;"""
    formatCode = cxx_writer.Code(code)
    parameters = [cxx_writer.Parameter('output', cxx_writer.Type('std::ostream').makeRef()), cxx_writer.Parameter('value', cxx_writer.Type('::boost::unit_test::const_string'))]
    formatMethod = cxx_writer.Method('log_entry_value', formatCode, cxx_writer.voidType, 'public', parameters)
    formatClass = cxx_writer.ClassDeclaration('trap_log_formatter', [formatMethod], [cxx_writer.Type('::boost::unit_test::output::compiler_log_formatter')])
    """Returns the code for the file which contains the main
    routine for the execution of the tests."""
    global testNames
    code = 'trap_log_formatter* trap_log_formatter_ptr = new trap_log_formatter;\nboost::unit_test::unit_test_log.set_formatter(trap_log_formatter_ptr);\n'
    for test in testNames:
        code += 'boost::unit_test::framework::master_test_suite().add(BOOST_TEST_CASE(&' + test + '));\n'
    code += '\nreturn 0;'
    initCode = cxx_writer.Code(code)
    initCode.addInclude('boost/test/included/unit_test.hpp')
    parameters = [cxx_writer.Parameter('argc', cxx_writer.intType), cxx_writer.Parameter('argv[]', cxx_writer.charPtrType)]
    initFunction = cxx_writer.Function('init_unit_test_suite', initCode, cxx_writer.Type('boost::unit_test::test_suite').makePointer(), parameters)

    code = 'return boost::unit_test::unit_test_main(&init_unit_test_suite, argc, argv);'
    mainCode = cxx_writer.Code(code)
    mainCode.addInclude('systemc.h')
    mainCode.addInclude('boost/test/included/unit_test.hpp')
    parameters = [cxx_writer.Parameter('argc', cxx_writer.intType), cxx_writer.Parameter('argv', cxx_writer.charPtrType.makePointer())]
    mainFunction = cxx_writer.Function('sc_main', mainCode, cxx_writer.intType, parameters)
    mainFunction.addDocString(brief = 'Main Processor Component Testbench', detail = 'Uses the boost::test framework to call the tests defined in decoderTests and isaTests*. Each test instantiates the required DUT submodules individually.')
    return [formatClass, initFunction, mainFunction]

################################################################################
