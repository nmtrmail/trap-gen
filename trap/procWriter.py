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

# This file contains the methods used to print on file the architectural
# elements; this includes the processor structure, the registers, the
# pipeline stages, etc..
# Note how these methods are in a separate file, but they are actually part of the
# processor class

import cxx_writer

# Helper variables
instrAttrs = []
instrCtorParams = []
instrCtorValues = ''
testNames = []

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

# Computes the code defining the execution of an instruction and
# of the processor tools.
def getInstrIssueCode(self, trace, combinedTrace, instrVarName, hasCheckHazard = False, pipeStage = None, checkDestroyCode = ''):
    codeString = """try {
            #ifndef DISABLE_TOOLS
            if (!(this->tool_manager.issue(cur_PC, """ + instrVarName + """))) {
            #endif
            num_cycles = """ + instrVarName + """->behavior();
    """
    if trace:
        codeString += instrVarName + '->print_trace();\n'
    codeString += '#ifndef DISABLE_TOOLS\n}\n'
    if trace:
        codeString += """else {
            std::cerr << "Instruction anulled by tools." << std::endl << std::endl;
        }
        """
    codeString +='#endif\n}\ncatch(annul_exception& etc) {\n'
    if trace:
        codeString += instrVarName + """->print_trace();
                std::cerr << "Skipped Instruction " << """ + instrVarName + """->get_name() << '.' << std::endl;
        """
    codeString += """num_cycles = 0;
        }
        """
    return codeString

# Computes the code defining the execution of an instruction and
# of the processor tools.
def getInstrIssueCodePipe(self, trace, combinedTrace, instrVarName, hasCheckHazard, pipeStage, checkDestroyCode = ''):
    unlockHazard = False
    for i in self.pipes:
        if i.checkHazard:
            unlockHazard = True
        if i == pipeStage:
            break
    codeString = ''
    codeString += 'try {\n'
    if pipeStage == self.pipes[0]:
        codeString += """#ifndef DISABLE_TOOLS
            if (!(this->tool_manager.issue(""" + instrVarName + """->fetch_PC, """ + instrVarName + """))) {
            #endif
    """
    codeString += """num_cycles = """ + instrVarName + """->behavior_""" + pipeStage.name + """(BasePipeStage::unlock_queue);
    """
    if instrVarName != 'this->cur_instr':
        codeString += """this->cur_instr = """ + instrVarName + """;
        """
    if trace:
        if pipeStage == self.pipes[-1]:
            if combinedTrace:
                codeString += 'if (this->cur_instr != this->NOP_instr) {\n'
            codeString += instrVarName + '->print_trace();\n'
            if combinedTrace:
                codeString += '}\n'
    if pipeStage == self.pipes[0]:
        codeString += """#ifndef DISABLE_TOOLS
                    } else {
            if (""" + instrVarName + """->to_destroy""" + checkDestroyCode + """) {
                delete """ + instrVarName + """;
            } else {
                """ + instrVarName + """->in_pipeline = false;
            }
            this->cur_instr = this->NOP_instr;
        """
        if trace:
            codeString += """std::cerr << "Instruction anulled by tools." << std::endl << std::endl;
            """
        codeString +='}\n#endif\n'
    codeString +='}\ncatch(annul_exception& etc) {\n'
    if trace:
        codeString += instrVarName + """->print_trace();
        std::cerr << "Skipped Instruction " << """ + instrVarName + """->get_name() << " at stage """ + pipeStage.name + """ ." << std::endl << std::endl;
        """
    if pipeStage != self.pipes[0]:
        codeString += 'flush_annulled = this->cur_instr->flush_pipeline;\n'
        codeString += 'this->cur_instr->flush_pipeline = false;\n'
    if hasCheckHazard and unlockHazard:
        codeString +=  instrVarName + '->get_unlock_' + pipeStage.name + '(BasePipeStage::unlock_queue);\n'
    codeString += """
            if (""" + instrVarName + """->to_destroy""" + checkDestroyCode + """) {
                delete """ + instrVarName + """;
            } else {
                """ + instrVarName + """->in_pipeline = false;
            }
            this->cur_instr = this->NOP_instr;
            num_cycles = 0;
        }
        """
    return codeString

# Computes the code for the fetch address
def computeFetchCode(self):
    fetchCode = str(self.bitSizes[1]) + ' bitstring = this->'
    # Now I have to check what is the fetch: if there is a TLM port or
    # if I have to access local memory
    if self.memory:
        # I perform the fetch from the local memory
        fetchCode += self.memory[0]
    else:
        for name, isFetch  in self.tlmPorts.items():
            if isFetch:
                fetchCode += name
        if fetchCode.endswith('this->'):
            raise Exception('Neither TLM port nor internal memory defined for instruction fetch.')
    fetchCode += '.read_word(cur_PC);\n'
    return fetchCode

# Computes current program counter, in order to fetch
# instrutions from it
def computeCurrentPC(self, model):
    fetchAddress = self.fetchReg[0]
    if model.startswith('func'):
        if self.fetchReg[1] < 0:
            fetchAddress += str(self.fetchReg[1])
        elif self.fetchReg[1] > 0:
            fetchAddress += ' + ' + str(self.fetchReg[1])
    return fetchAddress

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
            interruptCode += 'num_cycles = this->' + irqPort.name + '_instr->behavior_' + pipeStage.name + '(BasePipeStage::unlock_queue);\n'
            interruptCode += 'this->cur_instr = this->' + irqPort.name + '_instr;\n'
        else:
            interruptCode += 'num_cycles = this->' + irqPort.name + '_instr->behavior();\n'
        interruptCode +='}\ncatch(annul_exception& etc) {\n'
        if trace:
            interruptCode += 'this->' + irqPort.name + """_instr->print_trace();
                    std::cerr << "Skipped Instruction " << this->""" + irqPort.name + """_instr->get_name() << '.' << std::endl;
            """
        interruptCode += """num_cycles = 0;
            }
            """
        interruptCode += '\n}\n'
    if self.irqs:
        interruptCode += 'else {\n'
    return interruptCode

# Returns the code necessary for performing a standard instruction fetch: i.e.
# read from memory and set the instruction parameters
def standardInstrFetch(self, trace, combinedTrace, issueCodeGenerator, hasCheckHazard = False, pipeStage = None, checkDestroyCode = ''):
    codeString = 'int instr_id = this->decoder.decode(bitstring);\n'
    if pipeStage:
        codeString += 'Instruction* instr = this->INSTRUCTIONS[instr_id];\n'
        codeString += 'if (instr->in_pipeline) {\n'
        codeString += 'instr = instr->replicate();\n'
        codeString += 'instr->to_destroy = true;\n'
        codeString += '}\n'
        codeString += 'instr->in_pipeline = true;\n'
        codeString += 'instr->fetch_PC = cur_PC;\n'
    else:
        codeString += 'Instruction* instr = this->INSTRUCTIONS[instr_id];\n'
    codeString += 'instr->set_params(bitstring);\n'

    # Here we add the details about the instruction to the current history element
    codeString += """#ifdef ENABLE_HISTORY
    if (this->history_en) {
        instr_queue_elem.name = instr->get_name();
        instr_queue_elem.mnemonic = instr->get_mnemonic();
    }
    #endif
    """

    codeString += issueCodeGenerator(self, trace, combinedTrace, 'instr', hasCheckHazard, pipeStage, checkDestroyCode)
    return codeString

def fetchWithCacheCode(self, fetchCode, trace, combinedTrace, issueCodeGenerator, hasCheckHazard = False, pipeStage = None):
    codeString = ''
    if self.fastFetch:
        mapKey = 'cur_PC'
    else:
        mapKey = 'bitstring'
    codeString += 'template_map< ' + str(self.bitSizes[1]) + ', CacheElem >::iterator cached_instr = this->instr_cache.find(' + mapKey + ');'
    # I have found the instruction in the cache
    codeString += """
    if (cached_instr != icache_end) {
        Instruction* cur_instr_ptr = cached_instr->second.instr;
        // Instruction found, call it.
        if (cur_instr_ptr != NULL) {
    """

    # Here we add the details about the instruction to the current history element
    codeString += """#ifdef ENABLE_HISTORY
    if (this->history_en) {
        instr_queue_elem.name = cur_instr_ptr->get_name();
        instr_queue_elem.mnemonic = cur_instr_ptr->get_mnemonic();
    }
    #endif
    """

    if pipeStage:
        codeString += 'if (cur_instr_ptr->in_pipeline) {\n'
        codeString += 'cur_instr_ptr = cur_instr_ptr->replicate();\n'
        codeString += 'cur_instr_ptr->set_params(bitstring);\n'
        codeString += 'cur_instr_ptr->to_destroy = true;\n'
        codeString += '}\n'
        codeString += 'cur_instr_ptr->in_pipeline = true;\n'
        codeString += 'cur_instr_ptr->fetch_PC = cur_PC;\n'
    codeString += issueCodeGenerator(self, trace, combinedTrace, 'cur_instr_ptr', hasCheckHazard, pipeStage)

    # I have found the element in the cache, but not the instruction
    codeString += '} else {\n'
    if self.fastFetch:
        codeString += fetchCode
    codeString += 'unsigned& cur_count = cached_instr->second.count;\n'
    codeString += standardInstrFetch(self, trace, combinedTrace, issueCodeGenerator, hasCheckHazard, pipeStage, ' && cur_count < ' + str(self.cacheLimit))
    codeString += """if (cur_count < """ + str(self.cacheLimit) + """) {
            cur_count++;
        } else {
            // Add the instruction to the i-cache.
            cached_instr->second.instr = instr;
    """
    if pipeStage:
        codeString += """if (instr->to_destroy) {
                instr->to_destroy = false;
            } else {
            """
        codeString += 'this->INSTRUCTIONS[instr_id] = instr->replicate();\n'
        codeString += '}\n'
    else:
        codeString += 'this->INSTRUCTIONS[instr_id] = instr->replicate();\n'
    codeString += '}\n'

    # and now finally I have found nothing and I have to add everything
    codeString += """}
    } else {
        // Current instruction is not present in the cache. Perform regular decoding.
    """
    if self.fastFetch:
        codeString += fetchCode
    codeString += standardInstrFetch(self, trace, combinedTrace, issueCodeGenerator, hasCheckHazard, pipeStage)
    codeString += """this->instr_cache.insert(std::pair<unsigned, CacheElem>(bitstring, CacheElem()));
        icache_end = this->instr_cache.end();
        }
    """
    return codeString

def createPipeStage(self, processorElements, initElements):
    """Creates the pipeleine stages and the code necessary to initialize them"""
    regsNames = [i.name for i in self.regBanks + self.regs]
    for pipeStage in reversed(self.pipes):
        pipelineType = cxx_writer.Type(pipeStage.name.upper() + '_PipeStage', '#include \"pipeline.hpp\"')
        curStageAttr = cxx_writer.Attribute(pipeStage.name + '_stage', pipelineType, 'pu')
        processorElements.append(curStageAttr)
        curPipeInit = ['\"' + pipeStage.name + '\"']
        curPipeInit.append('latency')
        for otherPipeStage in self.pipes:
            if otherPipeStage != pipeStage:
                curPipeInit.append('&' + otherPipeStage.name + '_stage')
            else:
                curPipeInit.append('NULL')
        if self.pipes.index(pipeStage) - 1 >= 0:
            curPipeInit.append('&' + self.pipes[self.pipes.index(pipeStage) - 1].name + '_stage')
        else:
            curPipeInit.append('NULL')
        if self.pipes.index(pipeStage) + 1 < len(self.pipes):
            curPipeInit.append('&' + self.pipes[self.pipes.index(pipeStage) + 1].name + '_stage')
        else:
            curPipeInit.append('NULL')
        if pipeStage == self.pipes[0]:
            for reg in self.regs:
                if self.fetchReg[0] != reg.name:
                    curPipeInit = [reg.name + '_pipe'] + curPipeInit
            for regB in self.regBanks:
                curPipeInit = [regB.name + '_pipe'] + curPipeInit
            for pipeStage_2 in self.pipes:
                for alias in self.aliasRegs:
                    curPipeInit = [alias.name + '_' + pipeStage_2.name] + curPipeInit
                for aliasB in self.aliasRegBanks:
                    curPipeInit = [aliasB.name + '_' + pipeStage_2.name] + curPipeInit
            # It is the first stage, I also have to allocate the memory
            if self.memory:
                # I perform the fetch from the local memory
                memName = self.memory[0]
            else:
                for name, isFetch  in self.tlmPorts.items():
                    if isFetch:
                        memName = name
            if self.fetchReg[0] in regsNames:
                curPipeInit = [self.fetchReg[0] + '_pipe', 'this->INSTRUCTIONS', memName] + curPipeInit
            else:
                curPipeInit = [self.fetchReg[0], 'this->INSTRUCTIONS', memName] + curPipeInit
            curPipeInit = ['num_instructions'] + curPipeInit
            curPipeInit = ['instr_executing'] + curPipeInit
            curPipeInit = ['instr_end_event'] + curPipeInit
            for irq in self.irqs:
                curPipeInit = ['this->' + irq.name] + curPipeInit
        if pipeStage == self.pipes[0]:
            curPipeInit = ['profiler_time_end', 'profiler_time_start', 'tool_manager'] + curPipeInit
        initElements.append('\n' + pipeStage.name + '_stage(' + ', '.join(curPipeInit)  + ')')
    NOPIntructionType = cxx_writer.Type('NOPInstruction', '#include \"instructions.hpp\"')
    NOPinstructionsAttribute = cxx_writer.Attribute('NOP_instr', NOPIntructionType.makePointer(), 'pu', True)
    processorElements.append(NOPinstructionsAttribute)

def getCPPProc(self, model, trace, combinedTrace, namespace):
    """creates the class describing the processor"""
    fetchWordType = self.bitSizes[1]
    includes = fetchWordType.getIncludes()
    if self.abi:
        interfaceType = cxx_writer.Type('Interface', '#include \"interface.hpp\"')
    ToolsManagerType = cxx_writer.TemplateType('ToolsManager', [fetchWordType], 'common/tools_if.hpp')
    IntructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
    CacheElemType = cxx_writer.Type('CacheElem')
    IntructionTypePtr = IntructionType.makePointer()
    emptyBody = cxx_writer.Code('')
    processorElements = []
    codeString = ''

    ################################################
    # Start declaration of the main processor loop
    ###############################################
    # An here I start declaring the real processor content
    if not model.startswith('acc'):
        if self.systemc:
            codeString += 'bool start_met = false;\n'
        if self.instructionCache:
            # Declaration of the instruction buffer for speeding up decoding
            codeString += 'template_map<' + str(self.bitSizes[1]) + ', CacheElem >::iterator icache_end = this->instr_cache.end();\n\n'

        codeString += 'while(true) {\n'
        codeString += 'unsigned num_cycles = 0;\n'

        # Here is the code to notify start of the instruction execution
        codeString += 'this->instr_executing = true;\n'

        # Here is the code to deal with interrupts
        codeString += getInterruptCode(self, trace)
        # computes the correct memory and/or memory port from which fetching the instruction stream
        fetchCode = computeFetchCode(self)
        # computes the address from which the next instruction shall be fetched
        fetchAddress = computeCurrentPC(self, model)
        codeString += str(fetchWordType) + ' cur_PC = ' + fetchAddress + ';\n'
        # Lets insert the code to keep statistics
        if self.systemc:
            codeString += """if (!start_met && cur_PC == this->profiler_start_addr) {
                this->profiler_time_start = sc_time_stamp();
            }
            if (start_met && cur_PC == this->profiler_end_addr) {
                this->profiler_time_end = sc_time_stamp();
            }
            """
        # Lets start with the code for the instruction queue
        codeString += """#ifdef ENABLE_HISTORY
        HistoryInstrType instr_queue_elem;
        if (this->history_en) {
        """
        if len(self.tlmPorts) > 0 and model.endswith('LT'):
            codeString += 'instr_queue_elem.cycle = (unsigned)(this->quant_keeper.get_current_time()/this->latency);'
        elif model.startswith('acc') or self.systemc or model.endswith('AT'):
            codeString += 'instr_queue_elem.cycle = (unsigned)(sc_time_stamp()/this->latency);'
        codeString += """
            instr_queue_elem.address = cur_PC;
        }
        #endif
        """

        # We need to fetch the instruction ... only if the cache is not used or if
        # the index of the cache is the current instruction
        if not (self.instructionCache and self.fastFetch):
            codeString += fetchCode
        if trace:
            codeString += 'std::cerr << \"Current PC: \" << std::hex << std::showbase << cur_PC << \'.\' << std::endl;\n'

        # Finally I declare the fetch, decode, execute loop, where the instruction is actually executed;
        # Note the possibility of performing it with the instruction fetch
        if self.instructionCache:
            codeString += fetchWithCacheCode(self, fetchCode, trace, combinedTrace, getInstrIssueCode)
        else:
            codeString += standardInstrFetch(self, trace, combinedTrace, getInstrIssueCode)

        # Lets finish with the code for the instruction queue: I just still have to
        # check if it is time to save to file the instruction queue
        codeString += """#ifdef ENABLE_HISTORY
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
            codeString += '}\n'
        if len(self.tlmPorts) > 0 and model.endswith('LT'):
            codeString += 'this->quant_keeper.inc((num_cycles + 1)*this->latency);\nif (this->quant_keeper.need_sync()) {\nthis->quant_keeper.sync();\n}\n'
        elif model.startswith('acc') or self.systemc or model.endswith('AT'):
            codeString += 'wait((num_cycles + 1)*this->latency);\n'
        else:
            codeString += 'this->total_cycles += (num_cycles + 1);\n'

        # Here is the code to notify start of the instruction execution
        codeString += 'this->instr_executing = false;\n'
        if self.systemc:
            codeString += 'this->instr_end_event.notify();\n'

        codeString += 'this->num_instructions++;\n\n'
        # Now I have to call the update method for all the delayed registers
        for reg in self.regs:
            if reg.delay:
                codeString += reg.name + '.clock_cycle();\n'
        for reg in self.regBanks:
            for regNum in reg.delay.keys():
                codeString += reg.name + '[' + str(regNum) + '].clock_cycle();\n'
        codeString += '}'
        mainLoopCode = cxx_writer.Code(codeString)
        mainLoopCode.addInclude(includes)
        mainLoopCode.addInclude('common/report.hpp')
        mainLoopMethod = cxx_writer.Method('main_loop', mainLoopCode, cxx_writer.voidType, 'pu')
        processorElements.append(mainLoopMethod)
    ################################################
    # End declaration of the main processor loop
    ###############################################

    # Now I start declaring the other methods and attributes of the processor class

    ##########################################################################
    # Start declaration of begin, end, and reset operations (to be performed
    # at begin or end of simulation or to reset it)
    ##########################################################################
    resetCalledAttribute = cxx_writer.Attribute('reset_called', cxx_writer.boolType, 'pri')
    processorElements.append(resetCalledAttribute)
    if self.startup:
        beginOpMethod = cxx_writer.Method('startup', cxx_writer.Code(self.startup), cxx_writer.voidType, 'pri')
        processorElements.append(beginOpMethod)
    if self.shutdown:
        endOpMethod = cxx_writer.Method('shutdown', cxx_writer.Code(self.shutdown), cxx_writer.voidType, 'pri')
        processorElements.append(endOpMethod)
    if not self.reset:
        resetOpTemp = cxx_writer.Code('')
    else:
        import copy
        resetOpTemp = cxx_writer.Code(copy.deepcopy(self.reset))
    initString = ''
    if self.regs or self.regBanks:
        initString += 'this->R.reset();\n'
    # TODO: Ugly special case for the fetch register: Since ENTRY_POINT is public
    # and usually written after initialization, the reset value is stale.
    initString += 'this->' + self.fetchReg[0] + ' = this->ENTRY_POINT;\n'
    for irqPort in self.irqs:
        initString += 'this->' + irqPort.name + ' = 0;\n'
    resetOpTemp.prependCode(initString + '\n')
    if self.startup:
        resetOpTemp.appendCode('// User-defined initialization.\nthis->startup();\n')
    resetOpTemp.appendCode('this->reset_called = true;')
    resetOpMethod = cxx_writer.Method('reset', resetOpTemp, cxx_writer.voidType, 'pu')
    processorElements.append(resetOpMethod)

    # Now I declare the end of elaboration method, called by systemc just before starting the simulation
    endElabCode = cxx_writer.Code('if (!this->reset_called) {\nthis->reset();\n}\nthis->reset_called = false;')
    endElabMethod = cxx_writer.Method('end_of_elaboration', endElabCode, cxx_writer.voidType, 'pu')
    processorElements.append(endElabMethod)
    ##########################################################################
    # END declaration of begin, end, and reset operations (to be performed
    # at begin or end of simulation or to reset it)
    ##########################################################################

    ##########################################################################
    # Method for external instruction decoding
    ##########################################################################
    if model.startswith('acc'):
        decodeBody = 'int instr_id = this->' + self.pipes[0].name + '_stage.decoder.decode(bitstring);\n'
    else:
        decodeBody = 'int instr_id = this->decoder.decode(bitstring);\n'
    decodeBody += """if (instr_id >= 0) {
                Instruction* instr = this->INSTRUCTIONS[instr_id];
                instr->set_params(bitstring);
                return instr;
            }
            return NULL;
    """
    decodeCode = cxx_writer.Code(decodeBody)
    decodeParams = [cxx_writer.Parameter('bitstring', fetchWordType)]
    decodeMethod = cxx_writer.Method('decode', decodeCode, IntructionTypePtr, 'pu', decodeParams)
    processorElements.append(decodeMethod)
    if not model.startswith('acc'):
        decoderAttribute = cxx_writer.Attribute('decoder', cxx_writer.Type('Decoder', '#include \"decoder.hpp\"'), 'pri')
        processorElements.append(decoderAttribute)

    ####################################################################################
    # Instantiation of the ABI for accessing the processor internals (registers, etc)
    # from the outside world
    ####################################################################################
    if self.abi:
        interfaceAttribute = cxx_writer.Attribute('ABIIf', interfaceType.makePointer(), 'pu')
        processorElements.append(interfaceAttribute)
        interfaceMethodCode = cxx_writer.Code('return *this->ABIIf;')
        interfaceMethod = cxx_writer.Method('get_interface', interfaceMethodCode, interfaceType.makeRef(), 'pu')
        processorElements.append(interfaceMethod)
    toolManagerAttribute = cxx_writer.Attribute('tool_manager', ToolsManagerType, 'pu')
    processorElements.append(toolManagerAttribute)

    #############################################################################
    # Declaration of all the attributes of the processor class, including, in
    # particular, registers, aliases, memories, etc. The code
    # for their initialization/destruction is also created.
    #############################################################################
    initElements = []
    abiIfInit = ''
    bodyInits = ''
    bodyDestructor = ''
    if model.endswith('LT') and len(self.tlmPorts) > 0 and not model.startswith('acc'):
        quantumKeeperType = cxx_writer.Type('tlm_utils::tlm_quantumkeeper', 'tlm_utils/tlm_quantumkeeper.h')
        quantumKeeperAttribute = cxx_writer.Attribute('quant_keeper', quantumKeeperType, 'pri')
        processorElements.append(quantumKeeperAttribute)
        bodyInits += 'this->quant_keeper.set_global_quantum(this->latency*100);\nthis->quant_keeper.reset();\n'

    # Lets now add the registers, the reg banks, the aliases, etc.
    checkToolPipeStage = self.pipes[-1]
    for pipeStage in self.pipes:
        if pipeStage == self.pipes[0]:
            checkToolPipeStage = pipeStage
            break
    if (self.regs or self.regBanks):
        from registerWriter import registerContainerType
        processorElements.append(cxx_writer.Attribute('R', registerContainerType, 'pu'))
        # Register const or reset values could be processor variables.
        initRegList = []
        for reg in self.regs:
            if isinstance(reg.constValue, str):
                if reg.constValue not in initRegList:
                    initRegList.append(reg.constValue)
            if isinstance(reg.defValue, tuple):
                if reg.defValue[0] not in initRegList:
                    initRegList.append(reg.defValue[0])
            elif isinstance(reg.defValue, str):
                if reg.defValue not in initRegList:
                    initRegList.append(reg.defValue)
        for regBank in self.regBanks:
            for regConstValue in regBank.constValue.values():
                if isinstance(regConstValue, str):
                    if regConstValue not in initRegList:
                        initRegList.append(regConstValue)
            for regDefaultValue in regBank.defValues:
                if isinstance(regDefaultValue, tuple):
                    if regDefaultValue[0] not in initRegList:
                        initRegList.append(regDefaultValue[0])
                elif isinstance(regDefaultValue, str):
                    if regDefaultValue not in initRegList:
                        initRegList.append(regDefaultValue)
        initRegCode = ''
        for initRegElement in initRegList:
            initRegCode += initRegElement + ', '
        if initRegCode:
          initElements.append('R(' + initRegCode[:-2] + ')')
        if self.abi:
            abiIfInit += 'this->R'

    # Finally memories, TLM ports, etc.
    if self.memory:
        attribute = cxx_writer.Attribute(self.memory[0], cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"'), 'pu')
        initMemCode = self.memory[0] + '(' + str(self.memory[1])
        if self.memory[2] and not self.systemc and not model.startswith('acc') and not model.endswith('AT'):
            initMemCode += ', total_cycles'
        if self.memAlias:
            initMemCode += ', this->R'
        if self.memory[2] and self.memory[3]:
            initMemCode += ', ' + self.memory[3]
        initMemCode += ')'
        if self.abi and self.memory[0] in self.abi.memories.keys():
            abiIfInit = 'this->' + self.memory[0] + ', ' + abiIfInit
        initElements.append(initMemCode)
        processorElements.append(attribute)
    for tlmPortName in self.tlmPorts.keys():
        attribute = cxx_writer.Attribute(tlmPortName, cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"'), 'pu')
        initPortCode = tlmPortName + '(\"' + tlmPortName + '\"'
        if self.systemc and model.endswith('LT') and not model.startswith('acc'):
            initPortCode += ', this->quant_keeper'
        for memAl in self.memAlias:
            initPortCode += ', this->R'
        initPortCode += ')'
        if self.abi and tlmPortName in self.abi.memories.keys():
            abiIfInit = 'this->' + tlmPortName + ', ' + abiIfInit
        initElements.append(initPortCode)
        processorElements.append(attribute)
    if self.systemc or model.startswith('acc') or model.endswith('AT'):
        latencyAttribute = cxx_writer.Attribute('latency', cxx_writer.sc_timeType, 'pu')
        processorElements.append(latencyAttribute)
    else:
        totCyclesAttribute = cxx_writer.Attribute('total_cycles', cxx_writer.uintType, 'pu')
        processorElements.append(totCyclesAttribute)
        bodyInits += 'this->total_cycles = 0;\n'

    for par in self.parameters:
        attribute = cxx_writer.Attribute(par.name, par.type, 'pri')
        processorElements.append(attribute)

    # Some variables for profiling: they enable measuring the number of cycles spent among two program portions
    # (they actually count SystemC time and then divide it by the processor frequency)
    if self.systemc or model.startswith('acc') or model.endswith('AT'):
        profilingTimeStartAttribute = cxx_writer.Attribute('profiler_time_start', cxx_writer.sc_timeType, 'pu')
        processorElements.append(profilingTimeStartAttribute)
        bodyInits += 'this->profiler_time_start = SC_ZERO_TIME;\n'
        profilingTimeEndAttribute = cxx_writer.Attribute('profiler_time_end', cxx_writer.sc_timeType, 'pu')
        processorElements.append(profilingTimeEndAttribute)
        bodyInits += 'this->profiler_time_end = SC_ZERO_TIME;\n'
    if self.systemc and model.startswith('func'):
        profilingAddrStartAttribute = cxx_writer.Attribute('profiler_start_addr', fetchWordType, 'pri')
        processorElements.append(profilingAddrStartAttribute)
        bodyInits += 'this->profiler_start_addr = (' + str(fetchWordType) + ')-1;\n'
        profilingAddrEndAttribute = cxx_writer.Attribute('profiler_end_addr', fetchWordType, 'pri')
        bodyInits += 'this->profiler_end_addr = (' + str(fetchWordType) + ')-1;\n'
        processorElements.append(profilingAddrEndAttribute)

    # Here are the variables used to manage the instruction history queue
    if model.startswith('func'):
        instrQueueFileAttribute = cxx_writer.Attribute('history_file', cxx_writer.ofstreamType, 'pri')
        processorElements.append(instrQueueFileAttribute)
        historyEnabledAttribute = cxx_writer.Attribute('history_en', cxx_writer.boolType, 'pri')
        processorElements.append(historyEnabledAttribute)
        bodyInits += 'this->history_en = false;\n'
        instrHistType = cxx_writer.Type('HistoryInstrType', 'modules/instruction.hpp')
        histQueueType = cxx_writer.TemplateType('boost::circular_buffer', [instrHistType], 'boost/circular_buffer.hpp')
        instHistoryQueueAttribute = cxx_writer.Attribute('history_instr_queue', histQueueType, 'pu')
        processorElements.append(instHistoryQueueAttribute)
        bodyInits += 'this->history_instr_queue.set_capacity(1000);\n'
        undumpedHistElemsAttribute = cxx_writer.Attribute('history_undumped_elements', cxx_writer.uintType, 'pu')
        processorElements.append(undumpedHistElemsAttribute)
        bodyInits += 'this->history_undumped_elements = 0;\n'

    num_instructions = cxx_writer.Attribute('num_instructions', cxx_writer.uintType, 'pu')
    processorElements.append(num_instructions)
    bodyInits += 'this->num_instructions = 0;\n'
    # Now I have to declare some special constants used to keep track of the loaded executable file
    entryPointAttr = cxx_writer.Attribute('ENTRY_POINT', fetchWordType, 'pu')
    processorElements.append(entryPointAttr)
    bodyInits += 'this->ENTRY_POINT = 0;\n'
    mpIDAttr = cxx_writer.Attribute('MPROC_ID', fetchWordType, 'pu')
    processorElements.append(mpIDAttr)
    bodyInits += 'this->MPROC_ID = 0;\n'
    progLimitAttr = cxx_writer.Attribute('PROGRAM_LIMIT', fetchWordType, 'pu')
    processorElements.append(progLimitAttr)
    bodyInits += 'this->PROGRAM_LIMIT = 0;\n'
    if self.abi:
        abiIfInit = 'this->PROGRAM_LIMIT, ' + abiIfInit
    progStarttAttr = cxx_writer.Attribute('PROGRAM_START', fetchWordType, 'pu')
    processorElements.append(progStarttAttr)
    bodyInits += 'this->PROGRAM_START = 0;\n'
    # Here are the variables used to keep track of the end of each instruction execution
    attribute = cxx_writer.Attribute('instr_executing', cxx_writer.boolType, 'pri')
    processorElements.append(attribute)
    if self.abi:
        abiIfInit += ', this->instr_executing'
    if self.systemc:
        attribute = cxx_writer.Attribute('instr_end_event', cxx_writer.sc_eventType, 'pri')
        processorElements.append(attribute)
        if self.abi:
            abiIfInit += ', this->instr_end_event'
    if model.startswith('func'):
        abiIfInit += ', this->history_instr_queue'
    else:
        abiIfInit += ', this->' + self.pipes[0].name + '_stage.history_instr_queue'
    if self.abi:
        bodyInits += 'this->ABIIf = new ' + str(interfaceType) + '(' + abiIfInit + ');\n'

    instructionsAttribute = cxx_writer.Attribute('INSTRUCTIONS',
                            IntructionTypePtr.makePointer(), 'pri')
    processorElements.append(instructionsAttribute)
    if self.instructionCache:
        cacheAttribute = cxx_writer.Attribute('instr_cache',
                        cxx_writer.TemplateType('template_map',
                            [fetchWordType, CacheElemType], hash_map_include), 'pri')
        processorElements.append(cacheAttribute)
    numProcAttribute = cxx_writer.Attribute('num_instances',
                            cxx_writer.intType, 'pri', True, '0')
    processorElements.append(numProcAttribute)

    # Iterrupt ports
    for irqPort in self.irqs:
        if irqPort.tlm:
            irqPortType = cxx_writer.Type('TLMIntrPort_' + str(irqPort.portWidth), '#include \"irqPorts.hpp\"')
        else:
            irqPortType = cxx_writer.Type('SCIntrPort_' + str(irqPort.portWidth), '#include \"irqPorts.hpp\"')
        from isa import resolveBitType
        irqWidthType = resolveBitType('BIT<' + str(irqPort.portWidth) + '>')
        irqSignalAttr = cxx_writer.Attribute(irqPort.name, irqWidthType, 'pri')
        irqPortAttr = cxx_writer.Attribute(irqPort.name + '_port', irqPortType, 'pu')
        processorElements.append(irqSignalAttr)
        processorElements.append(irqPortAttr)
        initElements.append(irqPort.name + '_port(\"' + irqPort.name + '_port\", ' + irqPort.name + ')')
    # Generic PIN ports
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
        pinPortAttr = cxx_writer.Attribute(pinPort.name + '_pin', pinPortType, 'pu')
        processorElements.append(pinPortAttr)
        initElements.append(pinPort.name + '_pin(\"' + pinPort.name + '_pin\")')

    ####################################################################
    # Method for initializing the profiling start and end addresses
    ####################################################################
    if self.systemc and model.startswith('func'):
        setProfilingRangeCode = cxx_writer.Code('this->profiler_start_addr = start_addr;\nthis->profiler_end_addr = end_addr;')
        parameters = [cxx_writer.Parameter('start_addr', fetchWordType), cxx_writer.Parameter('end_addr', fetchWordType)]
        setProfilingRangeFunction = cxx_writer.Method('set_profiling_range', setProfilingRangeCode, cxx_writer.voidType, 'pu', parameters)
        processorElements.append(setProfilingRangeFunction)
    elif model.startswith('acc'):
        setProfilingRangeCode = cxx_writer.Code('this->' + self.pipes[0].name + '_stage.profiler_start_addr = start_addr;\nthis->' + self.pipes[0].name + '_stage.profiler_end_addr = end_addr;')
        parameters = [cxx_writer.Parameter('start_addr', fetchWordType), cxx_writer.Parameter('end_addr', fetchWordType)]
        setProfilingRangeFunction = cxx_writer.Method('set_profiling_range', setProfilingRangeCode, cxx_writer.voidType, 'pu', parameters)
        processorElements.append(setProfilingRangeFunction)

    ####################################################################
    # Method for initializing history management
    ####################################################################
    if model.startswith('acc'):
        enableHistoryCode = cxx_writer.Code('this->' + self.pipes[0].name + '_stage.history_en = true;\nthis->' + self.pipes[0].name + '_stage.history_file.open(file_name.c_str(), ios::out | ios::ate);')
        parameters = [cxx_writer.Parameter('file_name', cxx_writer.stringType, initValue = '""')]
        enableHistoryMethod = cxx_writer.Method('enable_history', enableHistoryCode, cxx_writer.voidType, 'pu', parameters)
        processorElements.append(enableHistoryMethod)
    else:
        enableHistoryCode = cxx_writer.Code('this->history_en = true;\nthis->history_file.open(file_name.c_str(), ios::out | ios::ate);')
        parameters = [cxx_writer.Parameter('file_name', cxx_writer.stringType, initValue = '""')]
        enableHistoryMethod = cxx_writer.Method('enable_history', enableHistoryCode, cxx_writer.voidType, 'pu', parameters)
        processorElements.append(enableHistoryMethod)

    ####################################################################
    # Cycle accurate model, lets proceed with the declaration of the
    # pipeline stages, together with the code necessary for thei initialization
    ####################################################################
    if model.startswith('acc'):
        # I have to instantiate the pipeline and its stages ...
        createPipeStage(self, processorElements, initElements)

    # Lets declare the interrupt instructions in case we have any
    for irq in self.irqs:
        IRQIntructionType = cxx_writer.Type(irq.name + 'IntrInstruction', '#include \"instructions.hpp\"')
        IRQinstructionAttribute = cxx_writer.Attribute(irq.name + '_instr', IRQIntructionType.makePointer(), 'pu')
        processorElements.append(IRQinstructionAttribute)

    ########################################################################
    # Ok, here I have to create the code for the constructor: I have to
    # initialize the INSTRUCTIONS array, the local memory (if present)
    # the TLM ports, the pipeline stages, etc.
    ########################################################################
    constrCode = 'this->reset_called = false;\n' + processor_name + '::num_instances++;\n'

    # Initialize base Instruction class.
    global instrAttrs, instrCtorParams, instrCtorValues
    constrCode += '// Initialize the array containing the initial instance of the instructions.\n'
    maxInstrId = max([instr.id for instr in self.isa.instructions.values()]) + 1
    constrCode += 'this->INSTRUCTIONS = new Instruction*[' + str(maxInstrId + 1) + '];\n'
    # Initialize static members of base Instruction class.
    from registerWriter import registerContainerType
    if (self.regs or self.regBanks):
        instrAttrs.append(cxx_writer.Attribute('R', registerContainerType.makeRef(), 'pu'))
        instrCtorParams.append(cxx_writer.Parameter('R', registerContainerType.makeRef()))
        instrCtorValues += 'R, '
    '''# TODO
    if model.startswith('acc'):
        pipeRegisterType = cxx_writer.Type('PipelineRegister', '#include \"registers.hpp\"')
        for reg in self.regs:
            instrAttrs.append(cxx_writer.Attribute(reg.name + '_pipe', pipeRegisterType.makeRef(), 'pu'))
            instrCtorParams.append(cxx_writer.Parameter(reg.name + '_pipe', pipeRegisterType.makeRef()))
            instrCtorInit.append(reg.name + '_pipe(' + reg.name + '_pipe)')
        for regB in self.regBanks:
            instrAttrs.append(cxx_writer.Attribute(regB.name + '_pipe', pipeRegisterType.makePointer(), 'pu'))
            instrCtorParams.append(cxx_writer.Parameter(regB.name + '_pipe', pipeRegisterType.makePointer()))
            instrCtorInit.append(regB.name + '_pipe(' + regB.name + '_pipe)')
        for pipeStage in self.pipes:
            for reg in self.regs:
                instrAttrs.append(cxx_writer.Attribute(reg.name + '_' + pipeStage.name, registerType.makeRef(), 'pu'))
                instrCtorParams.append(cxx_writer.Parameter(reg.name + '_' + pipeStage.name, registerType.makeRef()))
                instrCtorInit.append(reg.name + '_' + pipeStage.name + '(' + reg.name + '_' + pipeStage.name + ')')
            for regB in self.regBanks:
                if (regB.constValue and len(regB.constValue) < regB.numRegs):
                    curRegBType = registerType.makeRef()
                else:
                    curRegBType = registerType
                instrAttrs.append(cxx_writer.Attribute(regB.name + '_' + pipeStage.name, curRegBType, 'pu'))
                instrCtorParams.append(cxx_writer.Parameter(regB.name + '_' + pipeStage.name, curRegBType))
                instrCtorInit.append(regB.name + '_' + pipeStage.name + '(' + regB.name + '_' + pipeStage.name + ')')
            for alias in self.aliasRegs:
                instrAttrs.append(cxx_writer.Attribute(alias.name + '_' + pipeStage.name, registerType.makeRef(), 'pu'))
                instrCtorParams.append(cxx_writer.Parameter(alias.name + '_' + pipeStage.name, registerType.makeRef()))
                instrCtorInit.append(alias.name + '_' + pipeStage.name + '(' + alias.name + '_' + pipeStage.name + ')')
            for aliasB in self.aliasRegBanks:
                instrAttrs.append(cxx_writer.Attribute(aliasB.name + '_' + pipeStage.name, registerType.makePointer(), 'pu'))
                instrCtorParams.append(cxx_writer.Parameter(aliasB.name + '_' + pipeStage.name, registerType.makePointer()))
                instrCtorInit.append(aliasB.name + '_' + pipeStage.name + '(' + aliasB.name + '_' + pipeStage.name + ')')'''
    if self.memory:
        instrAttrs.append(cxx_writer.Attribute(self.memory[0], cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"').makeRef(), 'pu'))
        instrCtorParams.append(cxx_writer.Parameter(self.memory[0], cxx_writer.Type('LocalMemory').makeRef()))
        instrCtorValues += self.memory[0] + ', '
    for tlmPort in self.tlmPorts.keys():
        instrAttrs.append(cxx_writer.Attribute(tlmPort, cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"').makeRef(), 'pu'))
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
            instrAttrs.append(cxx_writer.Attribute(pinPort.name + '_pin', pinPortType.makeRef(), 'pu'))
            instrCtorParams.append(cxx_writer.Parameter(pinPort.name + '_pin', pinPortType.makeRef()))
            instrCtorValues += pinPort.name + '_pin, '
    if trace and not self.systemc and not model.startswith('acc'):
        instrAttrs.append(cxx_writer.Attribute('total_cycles', cxx_writer.uintType.makeRef(), 'pu'))
        instrCtorParams.append(cxx_writer.Parameter('total_cycles', cxx_writer.uintType.makeRef()))
        instrCtorValues += 'total_cycles, '

    if instrCtorValues: instrCtorValues = instrCtorValues[:-2]

    # Initialize individual instruction classes.
    for name, instr in self.isa.instructions.items():
        constrCode += 'this->INSTRUCTIONS[' + str(instr.id) + '] = new ' + name + '(' + instrCtorValues + ');\n'
    constrCode += 'this->INSTRUCTIONS[' + str(maxInstrId) + '] = new InvalidInstr(' + instrCtorValues + ');\n'
    if model.startswith('acc'):
        constrCode += 'if (' + processor_name + '::num_instances == 1) {\n'
        constrCode += processor_name + '::NOP_instr = new NOPInstruction(' + instrCtorValues + ');\n'
        for pipeStage in self.pipes:
            constrCode += pipeStage.name + '_stage.NOP_instr = ' + processor_name + '::NOP_instr;\n'
        constrCode += '}\n'
    for irq in self.irqs:
        constrCode += 'this->' + irq.name + '_instr = new ' + irq.name + 'IntrInstruction(' + instrCtorValues + ', this->' + irq.name + ');\n'
        if model.startswith('acc'):
            for pipeStage in self.pipes:
                constrCode += 'this->' + pipeStage.name + '_stage.' + irq.name + '_instr = this->' + irq.name + '_instr;\n'
    constrCode += bodyInits
    if not model.startswith('acc'):
        constrCode += 'SC_THREAD(main_loop);\n'
    if not self.systemc and not model.startswith('acc'):
        constrCode += 'this->total_cycles = 0;\n'
    constrCode += 'end_module();'
    constructorBody = cxx_writer.Code(constrCode)
    constructorParams = [cxx_writer.Parameter('name', cxx_writer.sc_module_nameType)]
    constructorInit = ['sc_module(name)']
    if (self.systemc or model.startswith('acc') or len(self.tlmPorts) > 0 or model.endswith('AT')) and not self.externalClock:
        constructorParams.append(cxx_writer.Parameter('latency', cxx_writer.sc_timeType))
        constructorInit.append('latency(latency)')

    for par in self.parameters:
        constructorInit.append(par.name+'(_'+par.name+')')
        par.name = '_' + par.name
        constructorParams.append(par)

    publicConstr = cxx_writer.Constructor(constructorBody, 'pu', constructorParams, constructorInit + initElements)
    destrCode = processor_name + """::num_instances--;
    for (int i = 0; i < """ + str(maxInstrId + 1) + """; i++) {
        delete this->INSTRUCTIONS[i];
    }
    delete [] this->INSTRUCTIONS;
    """
    if model.startswith('acc'):
        destrCode += 'if (' + processor_name + '::num_instances == 0) {\n'
        destrCode += 'delete ' + processor_name + '::NOP_instr;\n'
        destrCode += '}\n'
    if self.instructionCache and not model.startswith('acc'):
        destrCode += """template_map<""" + str(fetchWordType) + """, CacheElem>::const_iterator cache_it, cache_end;
        for (cache_it = this->instr_cache.begin(), cache_end = this->instr_cache.end(); cache_it != cache_end; cache_it++) {
            delete cache_it->second.instr;
        }
        """
    if self.abi:
        destrCode += 'delete this->ABIIf;\n'
    for irq in self.irqs:
        destrCode += 'delete this->' + irq.name + '_instr;\n'
    # Now, before the processor elements is destructed I have to make sure that the history dump file is correctly closed
    if model.startswith('func'):
        destrCode += """#ifdef ENABLE_HISTORY
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
                        this->history_file <<  *history_vector_it << std::endl;
                    }
                }
                this->history_file.flush();
                this->history_file.close();
            }
        }
        #endif
        """
    destrCode += bodyDestructor
    destructorBody = cxx_writer.Code(destrCode)
    publicDestr = cxx_writer.Destructor(destructorBody, 'pu')
    processorDecl = cxx_writer.SCModule(processor_name, processorElements, namespaces = [namespace])
    processorDecl.addDocString(brief = 'Processor Class', detail = 'The top-level processor class holding the pipeline, registers and ports.')
    processorDecl.addConstructor(publicConstr)
    processorDecl.addDestructor(publicDestr)
    return [processorDecl]

#########################################################################################
# Lets complete the declaration of the processor with the main files: one for the
# tests and one for the main file of the simulator itself
#########################################################################################

def getTestMainCode(self):
    """Returns the code for a formatting class called from within the boost
    test framework that outputs values in hex."""
    code = """output << std::showbase << std::hex << value;"""
    formatCode = cxx_writer.Code(code)
    parameters = [cxx_writer.Parameter('output', cxx_writer.Type('std::ostream').makeRef()), cxx_writer.Parameter('value', cxx_writer.Type('::boost::unit_test::const_string'))]
    formatMethod = cxx_writer.Method('log_entry_value', formatCode, cxx_writer.voidType, 'pu', parameters)
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

def getMainCode(self, model, namespace):
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
        std::cout << std::endl << "Unable to initialize instruction history as it has " << "been disabled at compilation time." << std::endl << std::endl;
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
        code += '// Simulation ended. Call cleanup methods.\nprocInst.shutdown();\n'
    code += """
    return 0;
    """
    mainCode = cxx_writer.Code(code)
    mainCode.addInclude("""#ifdef _WIN32
#pragma warning(disable : 4101)
#endif""")

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
        tlmInitiatorSocketMember = cxx_writer.Attribute('init_socket', cxx_writer.Type('tlm_utils::multi_passthrough_initiator_socket<TLMIntrInitiator_' + str(portWidth) + ', ' + str(portWidth) + ', tlm::tlm_base_protocol_types>'), visibility = 'pu', initValue = '\"init_socket\"')
        # nb_transport_bw() {}
        code = """return tlm::TLM_COMPLETED;"""
        tlmInitiatorCode = cxx_writer.Code(code)
        parameters = [cxx_writer.Parameter('tag', cxx_writer.intType), cxx_writer.Parameter('payload', cxx_writer.Type('tlm::tlm_generic_payload').makeRef()), cxx_writer.Parameter('phase', cxx_writer.Type('tlm::tlm_phase').makeRef()), cxx_writer.Parameter('delay', cxx_writer.Type('sc_core::sc_time').makeRef())]
        tlmInitiatorNBMethod = cxx_writer.Method('nb_transport_bw', tlmInitiatorCode, cxx_writer.Type('tlm::tlm_sync_enum'), 'pu', parameters)
        # invalidate_direct_mem_ptr() {}
        tlmInitiatorCode = cxx_writer.Code('')
        parameters = [cxx_writer.Parameter('tag', cxx_writer.intType), cxx_writer.Parameter('start_range', cxx_writer.Type('sc_dt::uint64')), cxx_writer.Parameter('end_range', cxx_writer.Type('sc_dt::uint64'))]
        tlmInitiatorDMMethod = cxx_writer.Method('invalidate_direct_mem_ptr', tlmInitiatorCode, cxx_writer.voidType, 'pu', parameters)
        # initiator_<portWidth>() {}
        code = 'init_socket.register_nb_transport_bw(this, &TLMIntrInitiator_' + str(portWidth) + '::nb_transport_bw);\ninit_socket.register_invalidate_direct_mem_ptr(this, &TLMIntrInitiator_' + str(portWidth) + '::invalidate_direct_mem_ptr);'
        tlmInitiatorCode = cxx_writer.Code(code)
        tlmInitiatorCtor = cxx_writer.Constructor(tlmInitiatorCode, visibility = 'pu', parameters = [cxx_writer.Parameter('_name', cxx_writer.Type('sc_core::sc_module_name'))], initList = ['sc_module(_name)', 'init_socket(\"init_socket\")'])
        # class initiator_<portWidth> {};
        tlmInitiatorClass = cxx_writer.ClassDeclaration('TLMIntrInitiator_'+ str(portWidth), [tlmInitiatorSocketMember, tlmInitiatorNBMethod, tlmInitiatorDMMethod], [cxx_writer.Type('sc_core::sc_module')])
        tlmInitiatorClass.addConstructor(tlmInitiatorCtor)

    if self.irqs:
        return [bannerVariable, debuggerVariable, signalFunction, cycleRangeFunction, tlmInitiatorClass, mainFunction]
    else:
        return [bannerVariable, debuggerVariable, signalFunction, cycleRangeFunction, mainFunction]
