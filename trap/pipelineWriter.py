################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     pipelineWriter.py
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
from procWriter import hash_map_include, getFetchAddressCode, getDoFetchCode, getCacheInstrFetchCode, getDoDecodeCode, getPipeInstrIssueCode, getInterruptCode

################################################################################
# Pipeline Classes
################################################################################
def getCPPPipeline(self, trace, combinedTrace, model, namespace):
    # Returns the class representing a pipeline stage.

    from registerWriter import registerType, aliasType, registerContainerType
    InstructionType = cxx_writer.Type('Instruction', includes = ['#include \"instructions.hpp\"', 'iomanip'])
    pipeType = cxx_writer.Type('BasePipeStage')

    #---------------------------------------------------------------------------
    ## @name Pipeline Base Class
    #  @{

    pipeClasses = []
    pipeBaseMembers = []
    pipeBaseCtorParams = []
    pipeBaseCtorValues = ''
    pipeBaseCtorInit = []
    pipeBaseCtorCode = ''

    # Attributes and Initialization
    for pipe in self.pipes:
        otherStageAttr = cxx_writer.Attribute('stage_' + pipe.name, pipeType.makePointer(), 'protected')
        pipeBaseMembers.append(otherStageAttr)
        otherStageParam = cxx_writer.Parameter('stage_' + pipe.name, pipeType.makePointer())
        pipeBaseCtorParams.append(otherStageParam)
        pipeBaseCtorValues += 'stage_' + pipe.name + ', '
        pipeBaseCtorInit.append('stage_' + pipe.name + '(stage_' + pipe.name + ')')

    stageAttr = cxx_writer.Attribute('prev_stage', pipeType.makePointer(), 'public')
    pipeBaseMembers.append(stageAttr)
    stageParam = cxx_writer.Parameter('prev_stage', pipeType.makePointer())#, initValue = 'NULL')
    pipeBaseCtorParams.append(stageParam)
    pipeBaseCtorValues += 'prev_stage, '
    pipeBaseCtorInit.append('prev_stage(prev_stage)')

    stageAttr = cxx_writer.Attribute('succ_stage', pipeType.makePointer(), 'public')
    pipeBaseMembers.append(stageAttr)
    stageParam = cxx_writer.Parameter('succ_stage', pipeType.makePointer())#, initValue = 'NULL')
    pipeBaseCtorParams.append(stageParam)
    pipeBaseCtorValues += 'succ_stage, '
    pipeBaseCtorInit.append('succ_stage(succ_stage)')

    stageBeginEventAttr = cxx_writer.Attribute('stage_begin_event', cxx_writer.sc_eventType, 'public')
    pipeBaseMembers.append(stageBeginEventAttr)

    stageEndedEventAttr = cxx_writer.Attribute('stage_end_event', cxx_writer.sc_eventType, 'public')
    pipeBaseMembers.append(stageEndedEventAttr)

    stageBeginningAttr = cxx_writer.Attribute('stage_beginning', cxx_writer.boolType, 'public')
    pipeBaseMembers.append(stageBeginningAttr)
    pipeBaseCtorCode += 'this->stage_beginning = false;\n'

    stageEndedAttrFlag = cxx_writer.Attribute('stage_ended', cxx_writer.boolType, 'public')
    pipeBaseMembers.append(stageEndedAttrFlag)
    pipeBaseCtorCode += 'this->stage_ended = false;\n'

    hasToFlushAttr = cxx_writer.Attribute('has_to_flush', cxx_writer.boolType, 'public')
    pipeBaseMembers.append(hasToFlushAttr)
    pipeBaseCtorCode += 'this->has_to_flush = false;\n'

    stalledAttr = cxx_writer.Attribute('stalled', cxx_writer.boolType, 'public')
    pipeBaseMembers.append(stalledAttr)
    pipeBaseCtorCode += 'this->stalled = false;\n'

    latencyAttr = cxx_writer.Attribute('latency', cxx_writer.sc_timeType, 'protected')
    pipeBaseMembers.append(latencyAttr)
    latencyParam = cxx_writer.Parameter('latency', cxx_writer.sc_timeType.makeRef())
    pipeBaseCtorParams.append(latencyParam)
    pipeBaseCtorValues += 'latency, '
    pipeBaseCtorInit.append('latency(latency)')

    registerAttr = cxx_writer.Attribute('R', registerContainerType.makeRef(), 'protected')
    pipeBaseMembers.append(registerAttr)
    registerParam = cxx_writer.Parameter('R', registerContainerType.makeRef())
    pipeBaseCtorParams.append(registerParam)
    pipeBaseCtorValues += 'R, '
    pipeBaseCtorInit.append('R(R)')

    NOPInstrType = cxx_writer.Type('NOPInstruction', '#include \"instructions.hpp\"')
    NOPInstrAttr = cxx_writer.Attribute('NOP_instr', NOPInstrType.makePointer(), 'public')
    pipeBaseMembers.append(NOPInstrAttr)
    pipeBaseCtorCode += 'this->NOP_instr = NULL;\n'

    # Declare the interrupt instructions if any and the corresponding signal
    # attribute.
    for irq in self.irqs:
        IRQInstrType = cxx_writer.Type(irq.name + 'IntrInstruction', '#include \"instructions.hpp\"')
        IRQInstrAttr = cxx_writer.Attribute(irq.name + '_instr', IRQInstrType.makePointer(), 'public')
        pipeBaseMembers.append(IRQInstrAttr)
        pipeBaseCtorCode += 'this->' + irq.name + '_instr = NULL;\n'

    curInstrAttr = cxx_writer.Attribute('cur_instr', InstructionType.makePointer(), 'public')
    pipeBaseMembers.append(curInstrAttr)
    pipeBaseCtorCode += 'cur_instr = NULL;\n'
    nextInstrAttr = cxx_writer.Attribute('next_instr', InstructionType.makePointer(), 'public')
    pipeBaseMembers.append(nextInstrAttr)
    pipeBaseCtorCode += 'this->next_instr = NULL;\n'

    # Constructors and Destructors
    pipeBaseCtor = cxx_writer.Constructor(cxx_writer.Code(pipeBaseCtorCode), 'public', pipeBaseCtorParams, pipeBaseCtorInit)

    # Methods: flush()
    flushCode = """this->has_to_flush = true;
    if (this->prev_stage != NULL) {
        this->prev_stage->flush();
    }
    """
    flushMethod = cxx_writer.Method('flush', cxx_writer.Code(flushCode), cxx_writer.voidType, 'public', noException = True)
    pipeBaseMembers.append(flushMethod)

    # Class
    pipeBaseClass = cxx_writer.ClassDeclaration('BasePipeStage', pipeBaseMembers, namespaces = [namespace])
    pipeBaseClass.addConstructor(pipeBaseCtor)
    pipeClasses.append(pipeBaseClass)

    ## @} Pipeline Base Class
    #---------------------------------------------------------------------------
    ## @name Pipeline Stage Classes
    #  @{

    from procWriter import pipeFetchAttrs, pipeCtorParams, pipeFetchCtorParams
    hasHazard = False
    hazardStarted = False
    seenStages = 0

    for pipeStage in self.pipes:
        if pipeStage.regsStage:
            if self.pipes.index(pipeStage) + 1 < len(self.pipes):
                # There exist stages between the beginning and the end of the hazard.
                if not self.pipes[self.pipes.index(pipeStage) + 1].wbStage:
                    hasHazard = True

    for pipeStage in self.pipes:
        seenStages += 1
        pipeMembers = []
        pipeCtorInit = []
        if pipeStage.fetchStage:
            for attr in pipeFetchAttrs:
                pipeMembers.append(attr)
                pipeCtorInit.append(attr.name + '(' + attr.name + ')')
        pipeCtorCode = ''

        # Methods: behavior()
        Code = """cur_instr = this->NOP_instr;
        this->next_instr = this->NOP_instr;

        // Wait for SystemC infrastructure, otherwise register callbacks will crash.
        wait(SC_ZERO_TIME);
        """

        if pipeStage.fetchStage:
            Code += 'unsigned num_NOPs = 0;\n'
            if self.instructionCache:
                Code += 'template_map< ' + str(self.bitSizes[1]) + ', CacheElem>::iterator icache_end = this->instr_cache.end();\n\n'

        Code += """while(true) {
        // Wait for other pipeline stages to begin.
        this->wait_pipe_begin();

        unsigned num_cycles = 0;
        """

        if pipeStage.fetchStage:
            Code +='this->instr_executing = true;\n'
        else:
            Code += """cur_instr = this->next_instr;
            bool instr_annulled = false;
            """

        # Hazard Detection
        if hasHazard and pipeStage.decodeStage:
            Code += """
            // Hazard Detection
            bool was_stalled = this->stalled, will_stall = false;
            // Check read registers.
            if (cur_instr->check_regs() > 0) {
                will_stall = true;
            // Lock write registers: Only if read registers are available, otherwise do not lock yet.
            } else if (!cur_instr->lock_regs()) {
                will_stall = true;
            }

            // Just discovered stall: Stall registers:
            if (!was_stalled && will_stall) {
              R.stall(""" + str(self.pipes.index(pipeStage)) + """);
            // Just discovered end of stall: Advance registers.
            } else if (was_stalled && !will_stall) {
              this->next_instr = this->prev_stage->cur_instr;
              R.advance();
            }

            // Stall this pipeline stage.
            this->stalled = will_stall;
            """

        if hasHazard:
            Code += 'if (!this->stalled) {\n'

        if pipeStage.fetchStage:
            # Interrupts
            # If an interrupt is raised, we need to deal with it in the correct
            # stage, i.e. we need to create a special instruction that reaches
            # the correct stage and deals with the interrupt properly.
            Code += getInterruptCode(self, trace, pipeStage)
            if self.irqs:
                Code += 'else /* !IRQ */ {\n'

            # Instruction Fetch Address
            Code += getFetchAddressCode(self, model)

            # Logging and Tracing: Update cycle count.
            Code += """// Logging and Tracing: Update cycle count.
            if (cur_PC == this->profiler_start_addr) {
                this->profiler_time_start = sc_time_stamp();
            }
            if (cur_PC == this->profiler_end_addr) {
                this->profiler_time_end = sc_time_stamp();
            }
            """

            # Tools: Check whether the tools require the pipeline to be empty
            # before proceeding with execution.
            Code += """
                #ifndef DISABLE_TOOLS
                // Check whether the tools require the pipeline to be empty before proceeding with execution.
                if (this->tool_manager.is_pipeline_empty(cur_PC)) {
                    num_NOPs++;
                } else {
                    num_NOPs = 0;
                }
                if (num_NOPs > 0 && num_NOPs < """ + str(len(self.pipes)) + """) {
                    cur_instr = this->NOP_instr;
                """

            if trace and not combinedTrace:
                Code += 'std::cerr << std::setw(15) << std::left << \"Stage=' + pipeStage.name + '\" << \", PC=\" << std::hex << std::showbase << std::setw(10) << cur_PC << \", Instruction=NOP       , Mnemonic=NOP: Propagating NOP as required by tools.\" << std::endl;\n'
            Code += """} else {
                    num_NOPs = 0;
                #endif
                    // Either the pipeline is actually empty or no tool requires it to be so. Proceed with execution.
                    wait((""" + str(1 - float(seenStages - 1)/(len(self.pipes) - 1)) + """)*this->latency);
            """

            # Tools: Instruction History
            Code += """
            // Tools: Instruction History
            #ifdef ENABLE_HISTORY
            HistoryInstrType instr_queue_elem;
            if (this->history_en) {
                instr_queue_elem.cycle = (unsigned)(sc_time_stamp()/this->latency);
                instr_queue_elem.address = cur_PC;
            }
            #endif
            """

            # Instruction Fetch and Issue
            # Computes the correct memory and/or memory port from which to
            # perform the fetch.
            doFetchCode = getDoFetchCode(self)
            # Perform the fetch only if the cache is not used or if the index of
            # the cache is the current instruction.
            if not (self.instructionCache and self.fastFetch):
                Code += doFetchCode

            # Two fetch paths are possible: the instruction buffer or the normal
            # instruction stream.
            # getPipeInstrIssueCode() executes this stage's instruction behavior.
            if self.instructionCache:
                Code += getCacheInstrFetchCode(self, doFetchCode, trace, combinedTrace, getPipeInstrIssueCode, hasHazard, pipeStage)
            else:
                Code += getDoDecodeCode(self, trace, combinedTrace, getPipeInstrIssueCode, hasHazard, pipeStage)

            if trace:
                if combinedTrace:
                    Code += 'if (cur_instr != this->NOP_instr) {\n'
                Code += 'std::cerr << std::setw(15) << std::left << \"Stage=' + pipeStage.name + '\" << \", PC=\" << std::hex << std::showbase << std::setw(10) << cur_PC << \", Instruction=\" << std::setw(10) << std::left << cur_instr->get_name() << \", Mnemonic=\" << cur_instr->get_mnemonic() << \'.\' << std::endl;\n'
                Code += 'cur_instr->print_trace();\n'
                if combinedTrace:
                    Code += '}\n'

        else:
            Code += 'wait((' + str(1 - float(seenStages - 1)/(len(self.pipes) - 1)) + ')*this->latency);\n'
            if trace:
                if not combinedTrace:
                    Code += 'std::cerr << std::setw(15) << std::left << \"Stage=' + pipeStage.name + '\" << \", PC=\" << std::hex << std::showbase << std::setw(10) << cur_instr->fetch_PC << \", Instruction=\" << std::setw(10) << std::left << cur_instr->get_name() << \", Mnemonic=\" << cur_instr->get_mnemonic() << \'.\' << std::endl;\n'

            # Instruction Issue: Execute this stage's instruction behavior.
            Code += getPipeInstrIssueCode(self, trace, combinedTrace, hasHazard, pipeStage)

        # User-defined Operations
        if pipeStage.operation:
            Code += '\n// User-defined Operations\n'
            Code += 'this->R.set_stage(' + str(self.pipes.index(pipeStage)) + ');\n'
            Code += pipeStage.operation
            Code += 'this->R.unset_stage();\n'

        if pipeStage.fetchStage:
            # Tools: Instruction History
            # Check if it is time to save to file the instruction queue.
            Code += """
            // Tools: Instruction History
            #ifdef ENABLE_HISTORY
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

            Code += """this->num_instructions++;
                #ifndef DISABLE_TOOLS
                } // Pipeline is empty or no tools require it to be so.
                #endif
            """

            # Interrupts
            if self.irqs:
                Code += '} // if (!IRQ)\n'

        # Synchronize with other stages.
        if pipeStage.fetchStage:
            Code += """// Instruction-induced Latency
            wait((num_cycles + """ + str(float(seenStages - 1)/(len(self.pipes) - 1)) + """)*this->latency);
            // Wait for other pipeline stages to end.
            this->wait_pipe_end();
            """
        else:
            Code += """// Instruction-induced Latency
            wait((num_cycles + """ + str(float(seenStages - 1)/(len(self.pipes) - 1)) + """)*this->latency);
            // Wait for other pipeline stages to end.
            this->wait_pipe_end();

            // Flush pipeline.
            if (instr_annulled) {
                cur_instr->flush_pipeline = false;
                // Flush preceding pipeline stages.
                this->prev_stage->flush();
                // Flush registers.
                R.flush(""" + str(self.pipes.index(pipeStage)) + """);
            } else if (cur_instr->flush_pipeline) {
                cur_instr->flush_pipeline = false;
                // Flush preceding pipeline stages.
                this->prev_stage->flush();
                // Flush registers.
                R.flush(""" + str(self.pipes.index(pipeStage)-1) + """);
            }
            """

        # Stalled Due to Hazard
        if hasHazard:
            Code += """} else {
                // One of the following stages is blocked due to a data hazard, so the current stage is not doing anything.
                wait(this->latency);
            """

            if trace:
                if combinedTrace and pipeStage.fetchStage:
                    Code += 'if (cur_instr != this->NOP_instr) {\n'
                if not combinedTrace or pipeStage.fetchStage:
                    Code += 'std::cerr << std::setw(15) << std::left << \"Stage=' + pipeStage.name + '\" << \", PC=\" << std::hex << std::showbase << std::setw(10) << cur_instr->fetch_PC << \", Instruction=\" << std::setw(10) << std::left << cur_instr->get_name() << \", Mnemonic=\" << cur_instr->get_mnemonic() << \": Stalled on a data hazard.\" << std::endl;\nstd::cerr << "Stalled registers: " << cur_instr->print_busy_regs() << std::endl;'
                if pipeStage.fetchStage:
                    Code += 'cur_instr->print_trace();\n'
                if combinedTrace and pipeStage.fetchStage:
                    Code += '}\n'

            Code += """
                this->wait_pipe_end();
            } // if (this->stalled)
            """

        if hasHazard and pipeStage.decodeStage:
            Code += """
            // Stall pipeline stages.
            BasePipeStage* stage = this;
            while (stage) {
                stage->stalled = will_stall;
                stage = stage->prev_stage;
            }
            """

        Code += """
        // Instruction Propagation
        if (this->has_to_flush) {
            if (cur_instr->to_destroy) {
                delete cur_instr;
            } else {
                cur_instr->in_pipeline = false;
            }
            this->has_to_flush = false;
            cur_instr = this->NOP_instr;
            this->next_instr = this->NOP_instr;
            this->succ_stage->next_instr = this->NOP_instr;
        }"""
        if hasHazard and pipeStage.decodeStage:
            Code += """ else if (this->stalled) {
                // All preceding stages will retain next_instr = cur_instr.
                this->next_instr = cur_instr;
                // Propagate NOP only because the stall is caused by the instruction in this stage. Otherwise the next stage will retain next_instr = cur_instr.
                this->succ_stage->next_instr = this->NOP_instr;
            } else {
                this->succ_stage->next_instr = cur_instr;
            }
            """
        elif hasHazard and pipeStage != self.pipes[-1]:
            Code += """ else if (this->stalled) {
                // All preceding stages will retain next_instr = cur_instr.
                this->next_instr = cur_instr;
            } else {
                this->succ_stage->next_instr = cur_instr;
            }
            """
        elif pipeStage != self.pipes[-1]:
            Code += '\nthis->succ_stage->next_instr = cur_instr;\n'

        if pipeStage.fetchStage:
            # Register Propagation
            Code += """
            // Register Propagation
            this->refresh_registers();
            this->instr_executing = false;
            this->instr_end_event.notify();
            """

        if pipeStage.fetchStage and trace and not combinedTrace:
            Code += 'std::cerr << \"------------------------------------------------------------------------\" << std::endl << std::endl;\n'

        Code += '} // while (true)\n'

        if pipeStage.regsStage:
            hazardStarted = True

        behaviorMethod = cxx_writer.Method('behavior', cxx_writer.Code(Code), cxx_writer.voidType, 'public')
        pipeMembers.append(behaviorMethod)
        pipeCtorCode += 'SC_THREAD(behavior);\n'

        # Methods: wait_pipe_begin()
        Code = """this->stage_beginning = true;
        this->stage_begin_event.notify();
        """
        for pipeStageInner in self.pipes:
            if pipeStageInner != pipeStage:
                Code += """if (!this->stage_""" + pipeStageInner.name + """->stage_beginning) {
                    wait(this->stage_""" + pipeStageInner.name + """->stage_begin_event);
                }
                """
        Code += 'this->stage_ended = false;'
        waitPipeBeginMethod = cxx_writer.Method('wait_pipe_begin', cxx_writer.Code(Code), cxx_writer.voidType, 'private', noException = True)
        pipeMembers.append(waitPipeBeginMethod)

        # Methods: wait_pipe_end()
        Code = """this->stage_beginning = false;
        this->stage_ended = true;
        this->stage_end_event.notify();
        """
        for pipeStageInner in self.pipes:
            if pipeStageInner != pipeStage:
                Code += """if (!this->stage_""" + pipeStageInner.name + """->stage_ended) {
                    wait(this->stage_""" + pipeStageInner.name + """->stage_end_event);
                }
                """
        waitPipeEndMethod = cxx_writer.Method('wait_pipe_end', cxx_writer.Code(Code), cxx_writer.voidType, 'private', noException = True)
        pipeMembers.append(waitPipeEndMethod)

        # Methods: refresh_registers() for fetch stage
        if pipeStage.fetchStage:
            # I create the refresh_registers method; note that in order to update the registers
            # i simply have to call the "propagate" method; I also have to deal with the update of the alias
            # by manually moving the pointer to the pipeline register from one stage alias to
            # the other to update the alias
            Code = """// Update the registers to propagate the values in the pipeline.
            R.clock_cycle();
            """
            refreshRegistersMethod = cxx_writer.Method('refresh_registers', cxx_writer.Code(Code), cxx_writer.voidType, 'private', noException = True)
            pipeMembers.append(refreshRegistersMethod)

            # Fetch Stage Attributes, Constructors and Destructors
            decoderAttribute = cxx_writer.Attribute('decoder', cxx_writer.Type('Decoder', '#include \"decoder.hpp\"'), 'public')
            pipeMembers.append(decoderAttribute)

            if self.instructionCache:
                CacheElemType = cxx_writer.Type('CacheElem')
                template_mapType = cxx_writer.TemplateType('template_map', [self.bitSizes[1], CacheElemType], hash_map_include)
                cacheAttr = cxx_writer.Attribute('instr_cache', template_mapType, 'private')
                pipeMembers.append(cacheAttr)

            profilerStartAddrAttr = cxx_writer.Attribute('profiler_start_addr', self.bitSizes[1], 'public')
            pipeMembers.append(profilerStartAddrAttr)
            pipeCtorCode += 'this->profiler_start_addr = (' + str(self.bitSizes[1]) + ')-1;\n'

            profilerEndAddrAttr = cxx_writer.Attribute('profiler_end_addr', self.bitSizes[1], 'public')
            pipeCtorCode += 'this->profiler_end_addr = (' + str(self.bitSizes[1]) + ')-1;\n'
            pipeMembers.append(profilerEndAddrAttr)

            # Here are the attributes for the instruction history queue
            historyFileAttr = cxx_writer.Attribute('history_file', cxx_writer.ofstreamType, 'public')
            pipeMembers.append(historyFileAttr)

            historyEnabledAttr = cxx_writer.Attribute('history_en', cxx_writer.boolType, 'public')
            pipeMembers.append(historyEnabledAttr)
            pipeCtorCode += 'this->history_en = false;\n'

            historyQueueType = cxx_writer.Type('HistoryInstrType', 'modules/instruction.hpp')
            histQueueType = cxx_writer.TemplateType('boost::circular_buffer', [historyQueueType], 'boost/circular_buffer.hpp')
            historyQueueAttr = cxx_writer.Attribute('history_instr_queue', histQueueType, 'public')
            pipeMembers.append(historyQueueAttr)
            pipeCtorCode += 'this->history_instr_queue.set_capacity(1000);\n'

            historyUndumpedElementsAttr = cxx_writer.Attribute('history_undumped_elements', cxx_writer.uintType, 'public')
            pipeMembers.append(historyUndumpedElementsAttr)
            pipeCtorCode += 'this->history_undumped_elements = 0;\n'

            # Now, before the processor elements is destructed I have to make sure that the history dump file is correctly closed
            pipeDtorCode = """#ifdef ENABLE_HISTORY
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
            # Constructors and Destructors
            pipeCtorInit = ['sc_module(pipe_name)', 'BasePipeStage(' + pipeBaseCtorValues[:-2] + ')'] + pipeCtorInit
            pipeCtorBody = cxx_writer.Code(pipeCtorCode + 'end_module();')
            pipeCtor = cxx_writer.Constructor(pipeCtorBody, 'public', pipeCtorParams + pipeFetchCtorParams, pipeCtorInit)
            pipeDtor = cxx_writer.Destructor(cxx_writer.Code(pipeDtorCode), 'public')
        else:
            # Constructors and Destructors
            pipeCtorInit = ['sc_module(pipe_name)', 'BasePipeStage(' + pipeBaseCtorValues[:-2] + ')'] + pipeCtorInit
            pipeCtorBody = cxx_writer.Code(pipeCtorCode + 'end_module();')
            pipeCtor = cxx_writer.Constructor(pipeCtorBody, 'public', pipeCtorParams, pipeCtorInit)

        # Class
        pipeClass = cxx_writer.SCModule(pipeStage.name + 'PipeStage', pipeMembers, [pipeType], namespaces = [namespace])
        pipeClass.addDocString(brief = 'Pipeline Class', detail = 'Implements a pipeline stage. Addresses hazards.')
        pipeClass.addConstructor(pipeCtor)
        if pipeStage.fetchStage:
            pipeClass.addDestructor(pipeDtor)
        pipeClasses.append(pipeClass)

    ## @} Pipeline Stage Classes
    #---------------------------------------------------------------------------

    return pipeClasses

################################################################################
