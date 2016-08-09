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
# @todo
# The logic for hazard detection is way too simple:
# - Only one hazard sequence is allowed. Ok, maybe this is sufficient, but we
#   should at least have proper checks to verify that the user has given a
#   sensible sequence (start < end, only one start, only one end).
# - Special write-back sequences (forwarding-paths) are not honored. A linear
#   flow is assumed, which leads to many unnecessary stalls which should have
#   been prevented by forwarding.
#
################################################################################

import cxx_writer


################################################################################
# Globals and Helpers
################################################################################
from procWriter import hash_map_include, getFetchDataCode, getFetchAddressCode, getCacheInstrFetchCode, getInstrFetchCode, getPipeInstrIssueCode, getInterruptCode


################################################################################
# Pipeline Class
################################################################################
def getCPPPipeline(self, trace, combinedTrace, model, namespace):
    # Returns the class representing a pipeline stage.

    from registerWriter import registerType, aliasType, registerContainerType
    InstructionType = cxx_writer.Type('Instruction', includes = ['#include \"instructions.hpp\"'])
    pipeType = cxx_writer.Type('BasePipeStage')

    #---------------------------------------------------------------------------
    ## @name Pipeline Base Class
    #  @{

    pipeBaseMembers = []
    pipeBaseCtorParams = []
    pipeBaseCtorValues = ''
    pipeBaseCtorInit = []
    pipeBaseCtorCode = ''
    pipeElements = []

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

    chStalledAttr = cxx_writer.Attribute('ch_stalled', cxx_writer.boolType, 'public')
    pipeBaseMembers.append(chStalledAttr)
    pipeBaseCtorCode += 'this->ch_stalled = false;\n'

    stalledAttr = cxx_writer.Attribute('stalled', cxx_writer.boolType, 'public')
    pipeBaseMembers.append(stalledAttr)
    pipeBaseCtorCode += 'this->stalled = false;\n'
    unlockQueueType = cxx_writer.TemplateType('std::map', ['unsigned', cxx_writer.TemplateType('std::vector', [registerType.makePointer()], 'vector')], 'map')

    unlockQueueAttr = cxx_writer.Attribute('unlock_queue', unlockQueueType, 'protected', static = True)
    pipeBaseMembers.append(unlockQueueAttr)

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

    # Lets declare the interrupt instructions in case we have any and we also declare the signal attribute
    for irq in self.irqs:
        IRQInstrType = cxx_writer.Type(irq.name + 'IntrInstruction', '#include \"instructions.hpp\"')
        IRQInstrAttr = cxx_writer.Attribute(irq.name + '_instr', IRQInstrType.makePointer(), 'public')
        pipeBaseMembers.append(IRQInstrAttr)
        pipeBaseCtorCode += 'this->' + irq.name + '_instr = NULL;\n'

    curInstrAttr = cxx_writer.Attribute('cur_instr', InstructionType.makePointer(), 'public')
    pipeBaseMembers.append(curInstrAttr)
    pipeBaseCtorCode += 'this->cur_instr = NULL;\n'
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
    pipeElements.append(pipeBaseClass)

    ## @} Pipeline Base Class
    #---------------------------------------------------------------------------
    ## @name Pipeline Stage Classes
    #  @{

    from procWriter import pipeFetchAttrs, pipeCtorParams, pipeFetchCtorParams
    fetchStage = self.pipes[0]
    hasCheckHazard = False
    hasWb = False
    checkHazardsSeen = False
    seenStages = 0

    # NOTE: hasCheckHazard will equal hasWb for all well-formed cases, i.e.
    # where each checkHazard is eventually followed by a endHazard. All cases
    # where they are different imply an error:
    # - Only one of either check- or endHazard is declared.
    # - Both check- and endHazard are declared, but in wrong order, and one of
    #   them is the last/first pipeline stage, respectively.
    # - There is an odd number of either check- or endHazard stages.
    # Note that this does not catch the equally erroneous case of wrong order,
    # where both are in the middle of the pipeline or both are at the borders.
    for pipeStage in self.pipes:
        if pipeStage.fetch:
            fetchStage = pipeStage
        if pipeStage.checkHazard:
            if self.pipes.index(pipeStage) + 1 < len(self.pipes):
                # There exist stages between the beginning and the end of the hazard.
                if not self.pipes[self.pipes.index(pipeStage) + 1].endHazard:
                    hasCheckHazard = True
        if pipeStage.endHazard:
            if self.pipes.index(pipeStage) - 1 >= 0:
                # There exist stages between the beginning and the end of the hazard.
                if not self.pipes[self.pipes.index(pipeStage) - 1].checkHazard:
                    hasWb = True

    # Now lets determine the stages which need a call to check hazard
    #checkHazadsStagesDecl = []
    #if hasCheckHazard:
        #for instr in self.isa.instructions.values():
            #for stageName in instr.specialInRegs.keys():
                #if not stageName in checkHazadsStagesDecl:
                    #checkHazadsStagesDecl.append(stageName)
    # Remember that all the stages preceding the last one where we check for
    # hazards have to check if the following stages are stalled.

    # Now I have to actually declare the different pipeline stages, all of them
    # being equal apart from the fetch stage which has to fetch instructions and
    # check interrupts before calling the appropriate behavior method.
    for pipeStage in self.pipes:
        seenStages += 1
        pipeMembers = []
        pipeCtorInit = []
        if pipeStage == fetchStage:
            for attr in pipeFetchAttrs:
                pipeMembers.append(attr)
                pipeCtorInit.append(attr.name + '(' + attr.name + ')')
        pipeCtorCode = ''

        # Methods: behavior()
        Code = """this->cur_instr = this->NOP_instr;
        this->next_instr = this->NOP_instr;

        // Wait for SystemC infrastructure, otherwise register callbacks will crash.
        wait(SC_ZERO_TIME);
        """

        # Methods: behavior() for fetch stage
        if pipeStage == fetchStage:
            # This is the fetch pipeline stage, I have to fetch instructions
            Code += 'unsigned num_NOPs = 0;\n'
            if self.instructionCache:
                Code += 'template_map< ' + str(self.bitSizes[1]) + ', CacheElem>::iterator icache_end = this->instr_cache.end();\n\n'

            Code += 'while(true) {\n'

            # Here is the code to notify start of the instruction execution
            Code += 'this->instr_executing = true;\n'
            Code += 'unsigned num_cycles = 0;\n'
            if hasCheckHazard:
                Code += 'if (!this->ch_stalled) {\n'

            Code += '\n// Wait for the pipeline to begin.\nthis->wait_pipe_begin();\n'

            # Here is the code to deal with interrupts; note one problem: if an interrupt is raised, we need to
            # deal with it in the correct stage, i.e. we need to create a special instruction reaching the correct
            # stage and dealing with it properly.
            Code += getInterruptCode(self, trace, pipeStage)
            if self.irqs:
                Code += 'else {\n'
            # computes the address from which the next instruction shall be fetched
            Code += getFetchAddressCode(self, model)

            # Here is the code for updating cycle counts
            Code += """if (cur_PC == this->profiler_start_addr) {
                this->profiler_time_start = sc_time_stamp();
            }
            if (cur_PC == this->profiler_end_addr) {
                this->profiler_time_end = sc_time_stamp();
            }
            """

            # Now lets start with the code necessary to check the tools, to see if they need
            # the pipeline to be empty before being able to procede with execution
            Code += """
                #ifndef DISABLE_TOOLS
                // Check whether the tools require the pipeline to be empty before proceeding with execution.
                if (this->tool_manager.is_pipeline_empty(cur_PC)) {
                    num_NOPs++;
                } else {
                    num_NOPs = 0;
                }
                if (num_NOPs > 0 && num_NOPs < """ + str(len(self.pipes)) + """) {
                    this->cur_instr = this->NOP_instr;
                """
            if trace and not combinedTrace:
                Code += 'std::cerr << \"PC \" << std::hex << std::showbase << cur_PC << " propagating NOP because tools need it." << std::endl;\n'
            Code += """} else {
                    num_NOPs = 0;
                #endif
                    wait(this->latency);
                    // Either the pipeline is actually empty or no tool requires it to be so. Proceed with execution.
            """

            # Lets start with the code for the instruction queue
            Code += """#ifdef ENABLE_HISTORY
            HistoryInstrType instr_queue_elem;
            if (this->history_en) {
                instr_queue_elem.cycle = (unsigned)(sc_time_stamp()/this->latency);
                instr_queue_elem.address = cur_PC;
            }
            #endif
            """

            # computes the correct memory and/or memory port from which fetching the instruction stream
            fetchCode = getFetchDataCode(self)
            # We need to fetch the instruction ... only if the cache is not used or if
            # the index of the cache is the current instruction
            if not (self.instructionCache and self.fastFetch):
                Code += fetchCode
            if trace and not combinedTrace:
                Code += 'std::cerr << \"Fetching PC \" << std::hex << std::showbase << cur_PC << \'.\' << std::endl;\n'

            # Now lets starts the real instruction fetch: two paths are possible: the instruction buffer
            # and the normal instruction stream.
            if self.instructionCache:
                Code += getCacheInstrFetchCode(self, fetchCode, trace, combinedTrace, getPipeInstrIssueCode, hasCheckHazard, pipeStage)
            else:
                Code += getInstrFetchCode(self, trace, combinedTrace, getPipeInstrIssueCode, hasCheckHazard, pipeStage)

            # User-defined operations are executed after the regular instruction behavior.
            if pipeStage.operation:
                Code += 'this->R.set_stage(' + str(self.pipes.index(pipeStage)) + ');\n'
                Code += pipeStage.operation
                Code += 'this->R.unset_stage();\n'

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

            # Finally we have completed waiting for the other cycles in order to be able to go on
            # with this cycle.
            Code += """this->num_instructions++;
                #ifndef DISABLE_TOOLS
                }
                #endif
            """
            if self.irqs:
                Code += '} // if (!IRQ)\n'
            Code += """wait(num_cycles*this->latency);
            // Wait for all pipeline stages to end.
            this->wait_pipe_end();

            """

            Code += """
            // Propagate the instruction to the next cycle if the next stage has completed elaboration.
            if (this->has_to_flush) {
                if (this->cur_instr->to_destroy) {
                    delete this->cur_instr;
                } else {
                    this->cur_instr->in_pipeline = false;
                }
                this->cur_instr = this->NOP_instr;
                this->next_instr = this->NOP_instr;
                this->has_to_flush = false;
            }
            """
            Code += 'this->succ_stage->next_instr = this->cur_instr;\n'
            if hasCheckHazard:
                Code += """} else {
                    // One of the following stages is blocked due to a data hazard, so the current stage is not doing anything.
                    this->wait_pipe_begin();
                    // The scheduler needs to be controlled, otherwise it will be impossible to proceed. This thread will always execute.
                    wait(this->latency);
                    this->wait_pipe_end();
                    if (this->has_to_flush) {
                        if (this->cur_instr->to_destroy) {
                            delete this->cur_instr;
                        } else {
                            this->cur_instr->in_pipeline = false;
                        }
                        this->cur_instr = this->NOP_instr;
                        this->next_instr = this->NOP_instr;
                        this->has_to_flush = false;
                    }
                } // if (this->ch_stalled)"""
            # Here is the code to notify start of the instruction execution
            Code += """\nthis->refresh_registers();
                this->instr_executing = false;
                this->instr_end_event.notify();
            """
            # Now I have to insert the code for checking the presence of hazards;
            # in particular I have to see if the instruction to be executed next in
            # the checkHazardsStage will lock
            if hasCheckHazard:
                Code += 'Instruction* succ_instr = this->'
                for pipeStageTemp in self.pipes:
                    if pipeStageTemp.checkHazard:
                        break
                    else:
                        Code += 'succ_stage->'
                Code += 'next_instr;\n'
                Code += 'if (!succ_instr->check_hazard_' + pipeStageTemp.name + '()) {\n'
                codeTemp = 'this->'
                for pipeStageTemp in self.pipes:
                    Code += codeTemp + 'ch_stalled = true;\n'
                    codeTemp += 'succ_stage->'
                    if pipeStageTemp.checkHazard:
                        break
                Code += '} else {\n'
                codeTemp = 'this->'
                for pipeStageTemp in self.pipes:
                    Code += codeTemp + 'ch_stalled = false;\n'
                    codeTemp += 'succ_stage->'
                    if pipeStageTemp.checkHazard:
                        break
                Code += '}\n'
            if trace and not combinedTrace:
                Code += 'std::cerr << \"---------------------------------------------------------------\" << std::endl << std::endl;\n'
            Code += '} // while (true)\n'

        # Methods: behavior() for all other stages
        else:
            # This is a normal pipeline stage

            # First of all I have to wait for the completion of the other pipeline stages before being able
            # to go on.
            if hasCheckHazard and not checkHazardsSeen:
                Code += 'Instruction* next_stage_instr;\n'
            Code += """while(true) {
            unsigned num_cycles = 0;
            bool flush_annulled = false;

            // Wait for the pipeline to begin.
            this->wait_pipe_begin();

            this->cur_instr = this->next_instr;
            """

            if hasCheckHazard and not checkHazardsSeen:
                Code += 'if (!this->ch_stalled) {\n'
            Code += 'wait((' + str(1 - float(seenStages - 1)/(len(self.pipes) - 1)) + ')*this->latency);\n'
            if trace and not combinedTrace:
                Code += 'std::cerr << \"Stage ' + pipeStage.name + ' instruction at PC \" << std::hex << std::showbase << this->cur_instr->fetch_PC << \'.\' << std::endl;\n'

            #if hasCheckHazard and pipeStage.name in checkHazadsStagesDecl:
            if hasCheckHazard and pipeStage.checkHazard:
                Code += 'this->cur_instr->lock_regs_' + pipeStage.name + '();\n'

            if trace and combinedTrace and pipeStage == self.pipes[-1]:
                Code += 'if (this->cur_instr != this->NOP_instr) {\n'
                Code += 'std::cerr << \"Current PC: \" << std::hex << std::showbase << this->cur_instr->fetch_PC << \'.\' << std::endl;\n'
                Code += '}\n'

            # Now we issue the instruction, i.e. we execute its behavior related to this pipeline stage
            Code += getPipeInstrIssueCode(self, trace, combinedTrace, 'this->cur_instr', hasCheckHazard, pipeStage)
            # Finally I finalize the pipeline stage by synchrnonizing with the others
            Code += 'wait((num_cycles + ' + str(float(seenStages - 1)/(len(self.pipes) - 1)) + ')*this->latency);\n'
            Code += """// Flush current pipeline stage.
            if (this->cur_instr->flush_pipeline || flush_annulled) {
                this->cur_instr->flush_pipeline = false;
                // Flush preceding pipeline stages.
                this->prev_stage->flush();
            }
            """

            # User-defined operations are executed after the regular instruction behavior.
            if pipeStage.operation:
                Code += 'this->R.set_stage(' + str(self.pipes.index(pipeStage)) + ');\n'
                Code += pipeStage.operation
                Code += 'this->R.unset_stage();\n'

            if not hasCheckHazard or checkHazardsSeen:
                Code += """// Wait for all pipeline stages to end.
                this->wait_pipe_end();

                """

            if pipeStage != self.pipes[-1]:
                Code += """if (this->has_to_flush) {
                        if (this->cur_instr->to_destroy) {
                            delete this->cur_instr;
                        } else {
                            this->cur_instr->in_pipeline = false;
                        }
                        // Free any used resource, if any.
                        this->cur_instr = this->NOP_instr;
                        this->next_instr = this->NOP_instr;
                        this->has_to_flush = false;
                    }
                """
                if hasCheckHazard and not checkHazardsSeen:
                    Code += 'next_stage_instr = this->cur_instr;\n'

            if pipeStage == self.pipes[-1]:
                # Here I have to check if it is the case of destroying the instruction
                Code += 'if (this->cur_instr->to_destroy) {\n'
                Code += 'delete this->cur_instr;\n'
                Code += '} else {\n'
                Code += 'this->cur_instr->in_pipeline = false;\n'
                Code += '}\n'
            if hasCheckHazard and not checkHazardsSeen:
                Code += """} else {
                    // One of the following stages is blocked due to a data hazard, so the current stage is not doing anything.
                    // The scheduler needs to be controlled, otherwise it will be impossible to proceed. This thread will always execute.
                    wait(this->latency);
                """
                if trace and hasCheckHazard and pipeStage.checkHazard and not combinedTrace:
                    Code += """std::cerr << "Stage """ + pipeStage.name + """ - instruction " << this->cur_instr->get_name() << " with mnemonic " << this->cur_instr->get_mnemonic() << " at PC " << std::hex << std::showbase << this->cur_instr->fetch_PC << " stalled on a data hazard." << std::endl;
                    std::cerr << "Stalled registers: " << this->cur_instr->print_busy_regs() << '.' << std::endl << std::endl;
                    """
                Code += """if (this->has_to_flush) {
                        if (this->cur_instr->to_destroy) {
                            delete this->cur_instr;
                        } else {
                            this->cur_instr->in_pipeline = false;
                        }
                        // Free any used resource, if any.
                        this->cur_instr = this->NOP_instr;
                        this->next_instr = this->NOP_instr;
                        this->has_to_flush = false;
                    }
                    next_stage_instr = this->NOP_instr;
                }
                """
            if hasCheckHazard and not checkHazardsSeen:
                Code += """// Wait for all pipeline stages to end.
                this->wait_pipe_end();

                """
            if pipeStage != self.pipes[-1]:
                if hasCheckHazard and not checkHazardsSeen:
                    Code += 'this->succ_stage->next_instr = next_stage_instr;\n'
                else:
                    Code += 'this->succ_stage->next_instr = this->cur_instr;\n'
            Code += '}\n'

        if pipeStage.checkHazard:
            checkHazardsSeen = True

        # Methods: behavior() for all stages
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
        if pipeStage == fetchStage:
            # I create the refresh_registers method; note that in order to update the registers
            # i simply have to call the "propagate" method; I also have to deal with the update of the alias
            # by manually moving the pointer to the pipeline register from one stage alias to
            # the other to update the alias
            Code = '// Update the registers to propagate the values in the pipeline.\n'
            Code += 'R.clock_cycle();\n'

            # Now I have to produce the code for unlocking the registers in the unlock_queue
            Code += """
            // Unlock registers, so that stalls due to data hazards can be resolved.
            std::map<unsigned, std::vector<""" + str(registerType.makePointer()) + """> >::iterator unlock_queue_it, unlock_queue_end;
            for (unlock_queue_it = BasePipeStage::unlock_queue.begin(), unlock_queue_end = BasePipeStage::unlock_queue.end(); unlock_queue_it != unlock_queue_end; unlock_queue_it++) {
                std::vector<""" + str(registerType.makePointer()) + """>::iterator unlock_reg_it, unlock_reg_end;
                if (unlock_queue_it->first == 0) {
                    for (unlock_reg_it = unlock_queue_it->second.begin(), unlock_reg_end = unlock_queue_it->second.end(); unlock_reg_it != unlock_reg_end; unlock_reg_it++) {
                        (*unlock_reg_it)->unlock();
                    }
                } else {
                    for (unlock_reg_it = unlock_queue_it->second.begin(), unlock_reg_end = unlock_queue_it->second.end(); unlock_reg_it != unlock_reg_end; unlock_reg_it++) {
                        (*unlock_reg_it)->unlock(unlock_queue_it->first);
                    }
                }
                unlock_queue_it->second.clear();
            }
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
        if pipeStage == fetchStage:
            pipeClass.addDestructor(pipeDtor)
        pipeElements.append(pipeClass)

    ## @} Pipeline Stage Classes
    #---------------------------------------------------------------------------

    return pipeElements

################################################################################
