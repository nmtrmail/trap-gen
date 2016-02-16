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

from procWriter import getInstrIssueCodePipe, getInterruptCode, computeFetchCode, computeCurrentPC, fetchWithCacheCode, standardInstrFetch

from procWriter import hash_map_include

hasCheckHazard = False
wbStage = None
chStage = None

def getGetPipelineStages(self, trace, combinedTrace, model, namespace):
    global hasCheckHazard
    global wbStage
    global chStage
    from procWriter import resourceType
    # Returns the code implementing the class representing a pipeline stage
    pipeCodeElements = []
    pipelineElements = []
    constructorCode = ''
    constructorParamsBase = []
    constructorInit = []
    baseConstructorInit = ''
    pipeType = cxx_writer.Type('BasePipeStage')
    IntructionType = cxx_writer.Type('Instruction', includes = ['#include \"instructions.hpp\"'])
    registerType = cxx_writer.Type('Register', includes = ['#include \"registers.hpp\"'])

    stageEndedFlag = cxx_writer.Attribute('stage_ended', cxx_writer.boolType, 'pu')
    pipelineElements.append(stageEndedFlag)
    constructorCode += 'this->ch_stalled = false;\n'
    constructorCode += 'this->stalled = false;\n'
    constructorCode += 'this->stage_ended = false;\n'
    stageBeginningFlag = cxx_writer.Attribute('stage_beginning', cxx_writer.boolType, 'pu')
    pipelineElements.append(stageBeginningFlag)
    constructorCode += 'this->stage_beginning = false;\n'
    hasToFlush = cxx_writer.Attribute('has_to_flush', cxx_writer.boolType, 'pu')
    pipelineElements.append(hasToFlush)
    constructorCode += 'this->has_to_flush = false;\n'
    stageEndedEvent = cxx_writer.Attribute('stage_ended_event', cxx_writer.sc_eventType, 'pu')
    pipelineElements.append(stageEndedEvent)
    stageBeginningEvent = cxx_writer.Attribute('stage_begin_event', cxx_writer.sc_eventType, 'pu')
    pipelineElements.append(stageBeginningEvent)

    NOPIntructionType = cxx_writer.Type('NOPInstruction', '#include \"instructions.hpp\"')
    NOPinstructionsAttribute = cxx_writer.Attribute('NOP_instr', NOPIntructionType.makePointer(), 'pu')
    pipelineElements.append(NOPinstructionsAttribute)
    constructorCode += 'this->NOP_instr = NULL;\n'

    # Lets declare the interrupt instructions in case we have any and we also declare the signal attribute
    for irq in self.irqs:
        IRQIntructionType = cxx_writer.Type(irq.name + 'IntrInstruction', '#include \"instructions.hpp\"')
        IRQinstructionAttribute = cxx_writer.Attribute(irq.name + '_irq_instr', IRQIntructionType.makePointer(), 'pu')
        pipelineElements.append(IRQinstructionAttribute)
        constructorCode += 'this->' + irq.name + '_irq_instr = NULL;\n'

    latencyAttribute = cxx_writer.Attribute('latency', cxx_writer.sc_timeType, 'pro')
    pipelineElements.append(latencyAttribute)
    latencyParam = cxx_writer.Parameter('latency', cxx_writer.sc_timeType.makeRef())
    constructorParamsBase.append(latencyParam)
    constructorInit.append('latency(latency)')
    baseConstructorInit += 'latency, '

    curInstrAttr = cxx_writer.Attribute('cur_instr', IntructionType.makePointer(), 'pu')
    pipelineElements.append(curInstrAttr)
    constructorCode += 'this->cur_instr = NULL;\n'
    nextInstrAttr = cxx_writer.Attribute('next_instr', IntructionType.makePointer(), 'pu')
    pipelineElements.append(nextInstrAttr)
    constructorCode += 'this->next_instr = NULL;\n'

    flushCode = """this->has_to_flush = true;
    if (this->prev_stage != NULL) {
        this->prev_stage->flush();
    }
    """
    flushBody = cxx_writer.Code(flushCode)
    flushDecl = cxx_writer.Method('flush', flushBody, cxx_writer.voidType, 'pu', noException = True)
    pipelineElements.append(flushDecl)

    for pipe in self.pipes:
        otherStageAttr = cxx_writer.Attribute('stage_' + pipe.name, pipeType.makePointer(), 'pro')
        pipelineElements.append(otherStageAttr)
        otherStageParam = cxx_writer.Parameter('stage_' + pipe.name, pipeType.makePointer())
        constructorParamsBase.append(otherStageParam)
        constructorInit.append('stage_' + pipe.name + '(stage_' + pipe.name + ')')
        baseConstructorInit += 'stage_' + pipe.name + ', '

    chStalledAttr = cxx_writer.Attribute('ch_stalled', cxx_writer.boolType, 'pu')
    pipelineElements.append(chStalledAttr)
    stalledAttr = cxx_writer.Attribute('stalled', cxx_writer.boolType, 'pu')
    pipelineElements.append(stalledAttr)
    stageAttr = cxx_writer.Attribute('prev_stage', pipeType.makePointer(), 'pu')
    pipelineElements.append(stageAttr)
    stageAttr = cxx_writer.Attribute('succ_stage', pipeType.makePointer(), 'pu')
    pipelineElements.append(stageAttr)
    unlockQueueType = cxx_writer.TemplateType('std::map', ['unsigned', cxx_writer.TemplateType('std::vector', [registerType.makePointer()], 'vector')], 'map')
    unlockQueueAttr = cxx_writer.Attribute('unlock_queue', unlockQueueType, 'pro', static = True)
    pipelineElements.append(unlockQueueAttr)
    prevStageParam = cxx_writer.Parameter('prev_stage', pipeType.makePointer(), initValue = 'NULL')
    succStageParam = cxx_writer.Parameter('succ_stage', pipeType.makePointer(), initValue = 'NULL')
    constructorParamsBase.append(prevStageParam)
    constructorParamsBase.append(succStageParam)
    constructorInit.append('prev_stage(prev_stage)')
    constructorInit.append('succ_stage(succ_stage)')
    baseConstructorInit += 'prev_stage, '
    baseConstructorInit += 'succ_stage, '
    pipelineDecl = cxx_writer.ClassDeclaration('BasePipeStage', pipelineElements, namespaces = [namespace])
    constructorBody = cxx_writer.Code(constructorCode)
    publicPipelineConstr = cxx_writer.Constructor(constructorBody, 'pu', constructorParamsBase, constructorInit)
    pipelineDecl.addConstructor(publicPipelineConstr)
    pipeCodeElements.append(pipelineDecl)

    hasCheckHazard = False
    hasWb = False
    for pipeStage in self.pipes:
        if pipeStage.checkHazard:
            if self.pipes.index(pipeStage) + 1 < len(self.pipes):
                if not self.pipes[self.pipes.index(pipeStage) + 1].endHazard:
                    hasCheckHazard = True
        if pipeStage.endHazard:
            if self.pipes.index(pipeStage) - 1 >= 0:
                if not self.pipes[self.pipes.index(pipeStage) - 1].checkHazard:
                    hasWb = True

    # Now lets determine the stages which need a call to check hazard
    #checkHazadsStagesDecl = []
    #if hasCheckHazard:
        #for instr in self.isa.instructions.values():
            #for stageName in instr.specialInRegs.keys():
                #if not stageName in checkHazadsStagesDecl:
                    #checkHazadsStagesDecl.append(stageName)
    # Remember that all the stages preceding the the last one where we check for
    # hazards have to check if the following stages are stalled.

    # Now I have to actually declare the different pipeline stages, all of them being equal a part from
    # the fecth stage which have to fetch instructions and check interrupts before calling
    # the appropriate behavior method
    checkHazardsMet = False
    wbStage = self.pipes[-1]
    chStage = self.pipes[0]
    seenStages = 0
    for pipeStage in self.pipes:
        seenStages += 1
        if pipeStage.wb:
            wbStage = pipeStage
        if pipeStage.checkHazard:
            chStage = pipeStage

        pipeNameParam = cxx_writer.Parameter('pipe_name', cxx_writer.sc_module_nameType)
        curPipeElements = []
        constructorCode = ''
        constructorInit = []
        constructorParams = [pipeNameParam] + constructorParamsBase

        codeString = """this->cur_instr = this->NOP_instr;
        this->next_instr = this->NOP_instr;
        """
        if pipeStage == self.pipes[0]:
            # This is the fetch pipeline stage, I have to fetch instructions
            codeString += 'unsigned num_NOPs = 0;\n'
            codeString += 'bool start_met = false;\n'
            if self.instructionCache:
                codeString += 'template_map< ' + str(self.bitSizes[1]) + ', CacheElem>::iterator icache_end = this->instr_cache.end();\n\n'
            codeString += 'while(true) {\n'

            # Here is the code to notify start of the instruction execution
            codeString += 'this->instr_executing = true;\n'

            codeString += 'unsigned num_cycles = 0;\n'
            if hasCheckHazard:
                codeString += 'if (!this->ch_stalled) {\n'

            codeString += '\n// Wait for the pipeline to begin.\nthis->wait_pipe_begin();\n'

            # Here is the code to deal with interrupts; note one problem: if an interrupt is raised, we need to
            # deal with it in the correct stage, i.e. we need to create a special instruction reaching the correct
            # stage and dealing with it properly.
            codeString += getInterruptCode(self, trace, pipeStage)
            # computes the correct memory and/or memory port from which fetching the instruction stream
            fetchCode = computeFetchCode(self)
            # computes the address from which the next instruction shall be fetched
            fetchAddress = computeCurrentPC(self, model)
            codeString += str(self.bitSizes[1]) + ' cur_PC = ' + fetchAddress + ';\n'

            # Here is the code for updating cycle counts
            codeString += """if (!start_met && cur_PC == this->profiler_start_addr) {
                this->profiler_time_start = sc_time_stamp();
            }
            if (start_met && cur_PC == this->profiler_end_addr) {
                this->profiler_time_end = sc_time_stamp();
            }
            """

            # Now lets start with the code necessary to check the tools, to see if they need
            # the pipeline to be empty before being able to procede with execution
            codeString += """
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
                codeString += 'std::cerr << \"PC \" << std::hex << std::showbase << cur_PC << " propagating NOP because tools need it." << std::endl;\n'
            codeString += """} else {
                    num_NOPs = 0;
                #endif
                    wait(this->latency);
                    // Either the pipeline is actually empty or no tool requires it to be so. Proceed with execution.
            """

            # Lets start with the code for the instruction queue
            codeString += """#ifdef ENABLE_HISTORY
            HistoryInstrType instr_queue_elem;
            if (this->history_en) {
                instr_queue_elem.cycle = (unsigned)(sc_time_stamp()/this->latency);
                instr_queue_elem.address = cur_PC;
            }
            #endif
            """

            # We need to fetch the instruction ... only if the cache is not used or if
            # the index of the cache is the current instruction
            if not (self.instructionCache and self.fastFetch):
                codeString += fetchCode
            if trace and not combinedTrace:
                codeString += 'std::cerr << \"Fetching PC \" << std::hex << std::showbase << cur_PC << \'.\' << std::endl;\n'

            # Now lets starts the real instruction fetch: two paths are possible: the instruction buffer
            # and the normal instruction stream.
            if self.instructionCache:
                codeString += fetchWithCacheCode(self, fetchCode, trace, combinedTrace, getInstrIssueCodePipe, hasCheckHazard, pipeStage)
            else:
                codeString += standardInstrFetch(self, trace, combinedTrace, getInstrIssueCodePipe, hasCheckHazard, pipeStage)

            # Lets finish with the code for the instruction queue: I just still have to
            # check if it is time to save to file the instruction queue
            codeString += """#ifdef ENABLE_HISTORY
            if (this->history_en) {
                // Add the new element to the queue.
                this->history_queue.push_back(instr_queue_elem);
                // In case a queue dump file has been specified, check if it needs to be saved.
                if (this->history_file) {
                    this->history_undumped_elements++;
                    if (history_undumped_elements == this->history_queue.capacity()) {
                        boost::circular_buffer<HistoryInstrType>::const_iterator history_it, history_end;
                        for (history_it = this->history_queue.begin(), history_end = this->history_queue.end(); history_it != history_end; history_it++) {
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
            codeString += """this->num_instructions++;
                #ifndef DISABLE_TOOLS
                }
                #endif
            """
            if self.irqs:
                codeString += '}\n'
            codeString += """wait(num_cycles*this->latency);
            // Wait for the all pipeline stages to end.
            this->wait_pipe_end();

            """

            codeString += """
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
            codeString += 'this->succ_stage->next_instr = this->cur_instr;\n'
            if hasCheckHazard:
                codeString += """} else {
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
                }
            """
            # Here is the code to notify start of the instruction execution
            codeString += """this->refresh_registers();
                this->instr_executing = false;
                this->instr_end_event.notify();
            """
            # Now I have to insert the code for checking the presence of hazards;
            # in particular I have to see if the instruction to be executed next in
            # the checkHazardsStage will lock
            if hasCheckHazard:
                codeString += 'Instruction* succ_instr = this->'
                for pipeStageTemp in self.pipes:
                    if pipeStageTemp.checkHazard:
                        break
                    else:
                        codeString += 'succ_stage->'
                codeString += 'next_instr;\n'
                codeString += 'if (!succ_instr->check_hazard_' + pipeStageTemp.name + '()) {\n'
                codeTemp = 'this->'
                for pipeStageTemp in self.pipes:
                    codeString += codeTemp + 'ch_stalled = true;\n'
                    codeTemp += 'succ_stage->'
                    if pipeStageTemp.checkHazard:
                        break
                codeString += '} else {\n'
                codeTemp = 'this->'
                for pipeStageTemp in self.pipes:
                    codeString += codeTemp + 'ch_stalled = false;\n'
                    codeTemp += 'succ_stage->'
                    if pipeStageTemp.checkHazard:
                        break
                codeString += '}\n'
            if trace and not combinedTrace:
                codeString += 'std::cerr << \"---------------------------------------------------------------\" << std::endl << std::endl;\n'
            codeString += '}\n'
        else:
            # This is a normal pipeline stage

            # First of all I have to wait for the completion of the other pipeline stages before being able
            # to go on.
            if hasCheckHazard and not checkHazardsMet:
                codeString += 'Instruction* next_stage_instr;\n'
            codeString += """while(true) {
            unsigned num_cycles = 0;
            bool flush_annulled = false;

            // Wait for the pipeline to begin.
            this->wait_pipe_begin();

            this->cur_instr = this->next_instr;
            """

            if hasCheckHazard and not checkHazardsMet:
                codeString += 'if (!this->ch_stalled) {\n'
            codeString += 'wait((' + str(1 - float(seenStages - 1)/(len(self.pipes) - 1)) + ')*this->latency);\n'
            if trace and not combinedTrace:
                codeString += 'std::cerr << \"Stage ' + pipeStage.name + ' instruction at PC \" << std::hex << std::showbase << this->cur_instr->fetch_PC << \'.\' << std::endl;\n'

            #if hasCheckHazard and pipeStage.name in checkHazadsStagesDecl:
            if hasCheckHazard and pipeStage.checkHazard:
                codeString += 'this->cur_instr->lock_regs_' + pipeStage.name + '();\n'

            if trace and combinedTrace and pipeStage == self.pipes[-1]:
                codeString += 'if (this->cur_instr != this->NOP_instr) {\n'
                codeString += 'std::cerr << \"Current PC: \" << std::hex << std::showbase << this->cur_instr->fetch_PC << \'.\' << std::endl;\n'
                codeString += '}\n'
            # Now we issue the instruction, i.e. we execute its behavior related to this pipeline stage
            codeString += getInstrIssueCodePipe(self, trace, combinedTrace, 'this->cur_instr', hasCheckHazard, pipeStage)
            # Finally I finalize the pipeline stage by synchrnonizing with the others
            codeString += 'wait((num_cycles + ' + str(float(seenStages - 1)/(len(self.pipes) - 1)) + ')*this->latency);\n'
            codeString += """// Flush current pipeline stage.
            if (this->cur_instr->flush_pipeline || flush_annulled) {
                this->cur_instr->flush_pipeline = false;
                // Flush preceding pipeline stages.
                this->prev_stage->flush();
            }
            """

            if not hasCheckHazard or checkHazardsMet:
                codeString += """// Wait for all pipeline stages to end.
                this->wait_pipe_end();

                """

            if pipeStage != self.pipes[-1]:
                codeString += """if (this->has_to_flush) {
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
                if hasCheckHazard and not checkHazardsMet:
                    codeString += 'next_stage_instr = this->cur_instr;\n'

            if pipeStage == self.pipes[-1]:
                # Here I have to check if it is the case of destroying the instruction
                codeString += 'if (this->cur_instr->to_destroy) {\n'
                codeString += 'delete this->cur_instr;\n'
                codeString += '} else {\n'
                codeString += 'this->cur_instr->in_pipeline = false;\n'
                codeString += '}\n'
            if hasCheckHazard and not checkHazardsMet:
                codeString += """} else {
                    // One of the following stages is blocked due to a data hazard, so the current stage is not doing anything.
                    // The scheduler needs to be controlled, otherwise it will be impossible to proceed. This thread will always execute.
                    wait(this->latency);
                """
                if trace and hasCheckHazard and pipeStage.checkHazard and not combinedTrace:
                    codeString += """std::cerr << "Stage """ + pipeStage.name + """ - instruction " << this->cur_instr->get_name() << " with mnemonic " << this->cur_instr->get_mnemonic() << " at PC " << std::hex << std::showbase << this->cur_instr->fetch_PC << " stalled on a data hazard." << std::endl;
                    std::cerr << "Stalled registers: " << this->cur_instr->print_busy_regs() << '.' << std::endl << std::endl;
                    """
                codeString += """if (this->has_to_flush) {
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
            if hasCheckHazard and not checkHazardsMet:
                codeString += """// Wait for the all pipeline stages to end.
                this->wait_pipe_end();

                """
            if pipeStage != self.pipes[-1]:
                if checkHazardsMet:
                    codeString += 'this->succ_stage->next_instr = this->cur_instr;\n'
                else:
                    codeString += 'this->succ_stage->next_instr = next_stage_instr;\n'
            codeString += '}\n'
        if pipeStage.checkHazard:
            checkHazardsMet = True

        behaviorMethodBody = cxx_writer.Code(codeString)
        behaviorMethodDecl = cxx_writer.Method('behavior', behaviorMethodBody, cxx_writer.voidType, 'pu')
        curPipeElements.append(behaviorMethodDecl)
        constructorCode += 'SC_THREAD(behavior);\n'

        waitPipeBeginCode = """this->stage_beginning = true;
        this->stage_begin_event.notify();
        """
        for pipeStageInner in self.pipes:
            if pipeStageInner != pipeStage:
                waitPipeBeginCode += """if (!this->stage_""" + pipeStageInner.name + """->stage_beginning) {
                    wait(this->stage_""" + pipeStageInner.name + """->stage_begin_event);
                }
                """
        waitPipeBeginCode += 'this->stage_ended = false;'
        waitPipeBeginBody = cxx_writer.Code(waitPipeBeginCode)
        waitPipeBeginDecl = cxx_writer.Method('wait_pipe_begin', waitPipeBeginBody, cxx_writer.voidType, 'pri', noException = True)
        curPipeElements.append(waitPipeBeginDecl)

        waitPipeEndCode = """this->stage_beginning = false;
        this->stage_ended = true;
        this->stage_ended_event.notify();
        """
        for pipeStageInner in self.pipes:
            if pipeStageInner != pipeStage:
                waitPipeEndCode += """if (!this->stage_""" + pipeStageInner.name + """->stage_ended) {
                    wait(this->stage_""" + pipeStageInner.name + """->stage_ended_event);
                }
                """
        waitPipeEndBody = cxx_writer.Code(waitPipeEndCode)
        waitPipeEndDecl = cxx_writer.Method('wait_pipe_end', waitPipeEndBody, cxx_writer.voidType, 'pri', noException = True)
        curPipeElements.append(waitPipeEndDecl)

        IntructionType = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"')
        IntructionTypePtr = IntructionType.makePointer()

        pipelineStageMap = {}
        notCheckStages = []
        checkHazardsStageMet = False
        for curPipeMap in self.pipes:
            pipelineStageMap[curPipeMap.name] = curPipeMap
            if checkHazardsStageMet:
                notCheckStages.append(curPipeMap.name)
            if curPipeMap.checkHazard:
                checkHazardsStageMet = True

        if pipeStage == self.pipes[0]:
            # I create the refresh_registers method; note that in order to update the registers
            # i simply have to call the "propagate" method; I also have to deal with the update of the alias
            # by manually moving the pointer to the pipeline register from one stage alias to
            # the other to update the alias
            codeString = '// Update the registers to propagate the values in the pipeline.\n'
            for reg in self.regs:
                codeString += 'if (this->' + reg.name + '.has_to_propagate) {\n'
                ########
                if trace and not combinedTrace:
                    codeString += 'std::cerr << "Propagating register ' + reg.name + '" << std::endl;\n'
                ########
                codeString += 'this->' + reg.name + '.propagate();\n'
                codeString += '}\n'
            for regB in self.regBanks:
                codeString += 'for (int i = 0; i < ' + str(regB.numRegs) + '; i++) {\n'
                codeString += 'if (this->' + regB.name + '[i].has_to_propagate) {\n'
                ########
                if trace and not combinedTrace:
                    codeString += 'std::cerr << "Propagating register ' + regB.name + '[" << std::dec << i << "]" << std::endl;\n'
                ########
                codeString += 'this->' + regB.name + '[i].propagate();\n'
                codeString += '}\n}\n'
            # Now lets procede to the update of the alias: for each stage alias I have to copy the reference
            # of the general pipeline register from one stage to the other
            codeString += '\n// Update the aliases, so that what they point to is updated in the pipeline.\n'
            for i in reversed(range(0, len(self.pipes) -1)):
                for alias in self.aliasRegs:
                    if not alias.isFixed:
                        codeString += 'if (this->' + alias.name + '_' + self.pipes[i + 1].name + '.get_pipe_reg() != this->' + alias.name + '_' + self.pipes[i].name + '.get_pipe_reg()) {\n'
                        if trace and not combinedTrace:
                            codeString += 'std::cerr << "Updating alias ' + alias.name + '_' + self.pipes[i + 1].name + '" << std::endl;\n'
                        codeString += 'this->' + alias.name + '_' + self.pipes[i + 1].name + '.propagate_alias(*(this->' + alias.name + '_' + self.pipes[i].name + '.get_pipe_reg()));\n'
                        codeString += '}\n'
                for aliasB in self.aliasRegBanks:
                    checkContiguous = True
                    for j in range(0, len(aliasB.fixedIndices) - 1):
                        if aliasB.fixedIndices[j] + 1 != aliasB.fixedIndices[j + 1]:
                            checkContiguous = False
                            break
                    if checkContiguous:
                        if aliasB.fixedIndices[0] > 0:
                            if aliasB.checkGroup:
                                codeString += 'if (this->' + aliasB.name + '_' + self.pipes[i + 1].name + '[0].get_pipe_reg() != this->' + aliasB.name + '_' + self.pipes[i].name + '[0].get_pipe_reg()) {\n'
                            codeString += 'for (int i = 0; i < ' + str(aliasB.fixedIndices[-1]) + '; i++) {\n'
                            if not aliasB.checkGroup:
                                codeString += 'if (this->' + aliasB.name + '_' + self.pipes[i + 1].name + '[i].get_pipe_reg() != this->' + aliasB.name + '_' + self.pipes[i].name + '[i].get_pipe_reg()) {\n'
                            if trace and not combinedTrace:
                                codeString += 'std::cerr << "Updating alias ' + aliasB.name + '_' + self.pipes[i + 1].name + '[" << i << "]." << std::endl;\n'
                            codeString += 'this->' + aliasB.name + '_' + self.pipes[i + 1].name + '[i].propagate_alias(*(this->' + aliasB.name + '_' + self.pipes[i].name + '[i].get_pipe_reg()));\n'
                            codeString += '}\n'
                            codeString += '}\n'
                        if aliasB.fixedIndices[-1] + 1 < aliasB.numRegs:
                            if aliasB.checkGroup:
                                codeString += 'if (this->' + aliasB.name + '_' + self.pipes[i + 1].name + '[' + str(aliasB.fixedIndices[-1] + 1) + '].get_pipe_reg() != this->' + aliasB.name + '_' + self.pipes[i].name + '[' + str(aliasB.fixedIndices[-1] + 1) + '].get_pipe_reg()) {\n'
                            codeString += 'for (int i = ' + str(aliasB.fixedIndices[-1] + 1) + '; i < ' + str(aliasB.numRegs) + '; i++) {\n'
                            if not aliasB.checkGroup:
                                codeString += 'if (this->' + aliasB.name + '_' + self.pipes[i + 1].name + '[i].get_pipe_reg() != this->' + aliasB.name + '_' + self.pipes[i].name + '[i].get_pipe_reg()) {\n'
                            if trace and not combinedTrace:
                                codeString += 'std::cerr << "Updating alias ' + aliasB.name + '_' + self.pipes[i + 1].name + '[" << i << "]." << std::endl;\n'
                            codeString += 'this->' + aliasB.name + '_' + self.pipes[i + 1].name + '[i].propagate_alias(*(this->' + aliasB.name + '_' + self.pipes[i].name + '[i].get_pipe_reg()));\n'
                            codeString += '}\n'
                            codeString += '}\n'
                    else:
                        for j in range(0, aliasB.numRegs):
                            if not j in aliasB.fixedIndices:
                                codeString += 'if (this->' + aliasB.name + '_' + self.pipes[i + 1].name + '[' + str(j) + '].get_pipe_reg() != this->' + aliasB.name + '_' + self.pipes[i].name + '[' + str(j) + '].get_pipe_reg()) {\n'
                                if trace and not combinedTrace:
                                    codeString += 'std::cerr << "Updating alias ' + aliasB.name + '_' + self.pipes[i + 1].name + '[" << ' + str(i) + ' << "]." << std::endl;\n'
                                codeString += 'this->' + aliasB.name + '_' + self.pipes[i + 1].name + '[' + str(j) + '].propagate_alias(*(this->' + aliasB.name + '_' + self.pipes[i].name + '[' + str(j) + '].get_pipe_reg()));\n'
                                codeString += '}\n'
            # Now I have to produce the code for unlocking the registers in the unlock_queue
            codeString += """
            // Unlock registers, so that stalls due to data hazards can be resolved.
            std::map<unsigned, std::vector<Register*> >::iterator unlock_queue_it, unlock_queue_end;
            for (unlock_queue_it = BasePipeStage::unlock_queue.begin(), unlock_queue_end = BasePipeStage::unlock_queue.end(); unlock_queue_it != unlock_queue_end; unlock_queue_it++) {
                std::vector<Register*>::iterator unlock_reg_it, unlock_reg_end;
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
            refreshRegistersBody = cxx_writer.Code(codeString)
            refreshRegistersDecl = cxx_writer.Method('refresh_registers', refreshRegistersBody, cxx_writer.voidType, 'pri', noException = True)
            curPipeElements.append(refreshRegistersDecl)
            # Here I declare the references to the pipeline registers and to the alias
            pipeRegisterType = cxx_writer.Type('PipelineRegister', '#include \"registers.hpp\"')
            for reg in self.regs:
                if self.fetchReg[0] != reg.name:
                    attribute = cxx_writer.Attribute(reg.name, pipeRegisterType.makeRef(), 'pri')
                    constructorParams = [cxx_writer.Parameter(reg.name, pipeRegisterType.makeRef())] + constructorParams
                    constructorInit.append(reg.name + '(' + reg.name + ')')
                    curPipeElements.append(attribute)
            for regB in self.regBanks:
                attribute = cxx_writer.Attribute(regB.name, pipeRegisterType.makePointer(), 'pri')
                constructorParams = [cxx_writer.Parameter(regB.name, pipeRegisterType.makePointer())] + constructorParams
                constructorInit.append(regB.name + '(' + regB.name + ')')
                curPipeElements.append(attribute)
            aliasType = cxx_writer.Type('Alias', '#include \"alias.hpp\"')
            for pipeStageInner in self.pipes:
                for alias in self.aliasRegs:
                    attribute = cxx_writer.Attribute(alias.name + '_' + pipeStageInner.name, aliasType.makeRef(), 'pri')
                    constructorParams = [cxx_writer.Parameter(alias.name + '_' + pipeStageInner.name, aliasType.makeRef())] + constructorParams
                    constructorInit.append(alias.name + '_' + pipeStageInner.name + '(' + alias.name + '_' + pipeStageInner.name + ')')
                    curPipeElements.append(attribute)
                for aliasB in self.aliasRegBanks:
                    attribute = cxx_writer.Attribute(aliasB.name + '_' + pipeStageInner.name, aliasType.makePointer(), 'pri')
                    constructorParams = [cxx_writer.Parameter(aliasB.name + '_' + pipeStageInner.name, aliasType.makePointer())] + constructorParams
                    constructorInit.append(aliasB.name + '_' + pipeStageInner.name + '(' + aliasB.name + '_' + pipeStageInner.name + ')')
                    curPipeElements.append(attribute)
            # I have to also instantiate the reference to the memories, in order to be able to
            # fetch instructions
            if self.memory:
                # I perform the fetch from the local memory
                mem_name = self.memory[0]
                memType = cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"').makeRef()
            else:
                for name, isFetch  in self.tlmPorts.items():
                    if isFetch:
                        mem_name = name
                        memType = cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"').makeRef()
            constructorParams = [cxx_writer.Parameter(mem_name, memType)] + constructorParams
            constructorInit.append(mem_name + '(' + mem_name + ')')
            memRefAttr = cxx_writer.Attribute(mem_name, memType, 'pri')
            curPipeElements.append(memRefAttr)
            decoderAttribute = cxx_writer.Attribute('decoder', cxx_writer.Type('Decoder', '#include \"decoder.hpp\"'), 'pu')
            curPipeElements.append(decoderAttribute)
            # I also have to add the map containig the ISA instructions to this stage
            instructionsAttribute = cxx_writer.Attribute('INSTRUCTIONS', IntructionTypePtr.makePointer().makeRef(), 'pri')
            curPipeElements.append(instructionsAttribute)
            constructorParams = [cxx_writer.Parameter('INSTRUCTIONS', IntructionTypePtr.makePointer().makeRef())] + constructorParams
            constructorInit.append('INSTRUCTIONS(INSTRUCTIONS)')
            # fetch register;
            regsNames = [i.name for i in self.regBanks + self.regs]
            fetchRegType = resourceType[self.fetchReg[0]]
            if self.fetchReg[0] in regsNames:
                fetchRegType = pipeRegisterType
            fetchAttr = cxx_writer.Attribute(self.fetchReg[0], fetchRegType.makeRef(), 'pri')
            constructorParams = [cxx_writer.Parameter(self.fetchReg[0], fetchRegType.makeRef())] + constructorParams
            constructorInit.append(self.fetchReg[0] + '(' + self.fetchReg[0] + ')')
            curPipeElements.append(fetchAttr)
            num_instructions = cxx_writer.Attribute('num_instructions', cxx_writer.uintType.makeRef(), 'pri')
            constructorParams = [cxx_writer.Parameter('num_instructions', cxx_writer.uintType.makeRef())] + constructorParams
            constructorInit.append('num_instructions(num_instructions)')
            curPipeElements.append(num_instructions)
            attribute = cxx_writer.Attribute('instr_executing', cxx_writer.boolType.makeRef(), 'pri')
            constructorParams = [cxx_writer.Parameter('instr_executing', cxx_writer.boolType.makeRef())] + constructorParams
            constructorInit.append('instr_executing(instr_executing)')
            curPipeElements.append(attribute)
            attribute = cxx_writer.Attribute('instr_end_event', cxx_writer.sc_eventType.makeRef(), 'pri')
            constructorParams = [cxx_writer.Parameter('instr_end_event', cxx_writer.sc_eventType.makeRef())] + constructorParams
            constructorInit.append('instr_end_event(instr_end_event)')
            curPipeElements.append(attribute)

            for irq in self.irqs:
                from isa import resolveBitType
                irqWidthType = resolveBitType('BIT<' + str(irq.portWidth) + '>')
                IRQAttribute = cxx_writer.Attribute(irq.name, irqWidthType.makeRef(), 'pri')
                constructorParams = [cxx_writer.Parameter(irq.name, irqWidthType.makeRef())] + constructorParams
                constructorInit.append(irq.name + '(' + irq.name + ')')
                curPipeElements.append(IRQAttribute)
            if self.instructionCache:
                CacheElemType = cxx_writer.Type('CacheElem')
                template_mapType = cxx_writer.TemplateType('template_map', [self.bitSizes[1], CacheElemType], hash_map_include)
                cacheAttribute = cxx_writer.Attribute('instr_cache', template_mapType, 'pri')
                curPipeElements.append(cacheAttribute)

            # The fetch stage also contains the tools manager
            ToolsManagerType = cxx_writer.TemplateType('ToolsManager', [self.bitSizes[1]], 'common/tools_if.hpp')
            toolManagerAttribute = cxx_writer.Attribute('tool_manager', ToolsManagerType.makeRef(), 'pri')
            curPipeElements.append(toolManagerAttribute)
            constructorParams = [cxx_writer.Parameter('tool_manager', ToolsManagerType.makeRef())] + constructorParams
            constructorInit.append('tool_manager(tool_manager)')
            # Lets finally declare the attributes and constructor parameters for counting the cycles in a specified time
            # frame
            profilingTimeStartAttribute = cxx_writer.Attribute('profiler_time_start', cxx_writer.sc_timeRefType, 'pu')
            curPipeElements.append(profilingTimeStartAttribute)
            constructorParams = [cxx_writer.Parameter('profiler_time_start', cxx_writer.sc_timeRefType)] + constructorParams
            constructorInit.append('profiler_time_start(profiler_time_start)')
            profilingTimeEndAttribute = cxx_writer.Attribute('profiler_time_end', cxx_writer.sc_timeRefType, 'pu')
            curPipeElements.append(profilingTimeEndAttribute)
            constructorParams = [cxx_writer.Parameter('profiler_time_end', cxx_writer.sc_timeRefType)] + constructorParams
            constructorInit.append('profiler_time_end(profiler_time_end)')
            profilingAddrStartAttribute = cxx_writer.Attribute('profiler_start_addr', self.bitSizes[1], 'pu')
            curPipeElements.append(profilingAddrStartAttribute)
            constructorCode += 'this->profiler_start_addr = (' + str(self.bitSizes[1]) + ')-1;\n'
            profilingAddrEndAttribute = cxx_writer.Attribute('profiler_end_addr', self.bitSizes[1], 'pu')
            constructorCode += 'this->profiler_end_addr = (' + str(self.bitSizes[1]) + ')-1;\n'
            curPipeElements.append(profilingAddrEndAttribute)
            # Here are the attributes for the instruction history queue
            instrQueueFileAttribute = cxx_writer.Attribute('history_file', cxx_writer.ofstreamType, 'pu')
            curPipeElements.append(instrQueueFileAttribute)
            historyEnabledAttribute = cxx_writer.Attribute('history_en', cxx_writer.boolType, 'pu')
            curPipeElements.append(historyEnabledAttribute)
            constructorCode += 'this->history_en = false;\n'
            instrHistType = cxx_writer.Type('HistoryInstrType', 'modules/instruction.hpp')
            histQueueType = cxx_writer.TemplateType('boost::circular_buffer', [instrHistType], 'boost/circular_buffer.hpp')
            instHistoryQueueAttribute = cxx_writer.Attribute('history_queue', histQueueType, 'pu')
            curPipeElements.append(instHistoryQueueAttribute)
            constructorCode += 'this->history_queue.set_capacity(1000);\n'
            undumpedHistElemsAttribute = cxx_writer.Attribute('history_undumped_elements', cxx_writer.uintType, 'pu')
            curPipeElements.append(undumpedHistElemsAttribute)
            constructorCode += 'this->history_undumped_elements = 0;\n'
            # Now, before the processor elements is destructed I have to make sure that the history dump file is correctly closed
            destrCode = """#ifdef ENABLE_HISTORY
            if (this->history_en) {
                // In case a queue dump file has been specified, check if it needs to be saved.
                if (this->history_file) {
                    if (this->history_undumped_elements > 0) {
                        std::vector<std::string> history_vector;
                        boost::circular_buffer<HistoryInstrType>::const_reverse_iterator history_it, history_end;
                        unsigned history_read = 0;
                        for (history_read = 0, history_it = this->history_queue.rbegin(), history_end = this->history_queue.rend(); history_it != history_end && history_read < this->history_undumped_elements; history_it++, history_read++) {
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
            destructorBody = cxx_writer.Code(destrCode)
            publicDestr = cxx_writer.Destructor(destructorBody, 'pu')

        constructorInit = ['sc_module(pipe_name)', 'BasePipeStage(' + baseConstructorInit[:-2] + ')'] + constructorInit
        curPipeDecl = cxx_writer.SCModule(pipeStage.name.upper() + '_PipeStage', curPipeElements, [pipeType], namespaces = [namespace])
        curPipeDecl.addDocString(brief = 'Pipeline Class', detail = 'Implements a pipeline stage. Addresses hazards.')
        constructorBody = cxx_writer.Code(constructorCode + 'end_module();')
        publicCurPipeConstr = cxx_writer.Constructor(constructorBody, 'pu', constructorParams, constructorInit)
        if pipeStage == self.pipes[0]:
            curPipeDecl.addDestructor(publicDestr)
        curPipeDecl.addConstructor(publicCurPipeConstr)
        pipeCodeElements.append(curPipeDecl)

    return pipeCodeElements
