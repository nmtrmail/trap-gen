################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     processor.py
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
import procWriter, pipelineWriter, registerWriter, memWriter, interfaceWriter, portsWriter, irqWriter, licenses


################################################################################
# Globals and Helpers
################################################################################
validModels = ['funcLT', 'funcAT', 'accLT', 'accAT']

def extractRegInterval(regBankString):
    """Given a string, it check if it specifies an interval
    of registers of a register bank. In case it returns the
    interval, None otherwise. An exception is raised in case
    the string is malformed"""
    if ('[' in regBankString and not ']' in regBankString) or (']' in regBankString and not '[' in regBankString):
        raise Exception('Malformed brackets in registry bank identifier ' + regBankString + '.')
    if not '[' in regBankString:
        return None
    if not regBankString.endswith(']'):
        raise Exception('Missing closing bracket in registry bank identifier ' + regBankString + '.')
    interval = regBankString[regBankString.index('[') + 1:-1].split('-')
    if len(interval) > 2:
        raise Exception('Invalid number of intervals ' + str(len(interval)) + ' in registry bank identifier ' + regBankString + '.')
    if len(interval) == 1:
        return (int(interval[0]), int(interval[0]))
    if int(interval[0]) > int(interval[1]):
        raise Exception('Invalid indices in registry bank identifier ' + regBankString + '.')
    return (int(interval[0]), int(interval[1]))


################################################################################
# Processor
################################################################################
class Processor:
    """
    Defined by (a) name (b) endianess (c) wordsize (in terms of
    bytes) (d) bytesize (it is the minimum addressing quantity)
    (e) all the architectural elements here defined (f) the
    behavior for the received interrupt method; note that this
    has to be specified only if there are actually interrupts
    registered (this method is called at every cycle and the content,
    which has to be specified by the user, checks if there are
    interrupts, if they are not masked and takes the appropriate
    action) (g) if we are describing a processor or a coprocessor
    (the different with the coprocessor is that it is not active,
    instructions are passed to it and not actively fetched)
    Three special operations are defined, (1) begin, (2) end
    and (3) reset, used at the begining of the simulation (or
    after a reset), at the end of the simulation and to reset
    the processor.
    The systemc parameter in the constructor specifies whether systemc
    will be used for keeping time or not in the completely
    functional processor in case a local memory is used (in case TLM ports
    are used the systemc parameter is not taken into account)
    """

    #---------------------------------------------------------------------------
    ## @name Initialization and Configuration
    #  @{

    # ..........................................................................
    # Processor Configuration

    def __init__(self, name, version, systemc = True, coprocessor = False, instructionCache = True, fastFetch = False, externalClock = False, cacheLimit = 256):
        if coprocessor:
            raise Exception('Generation of co-processors not yet supported.')
        if externalClock:
            raise Exception('Use of an external clock signal not yet supported.')

        self.name = name
        self.version = version
        self.isBigEndian = None
        #self.alloc_buffer_size = alloc_buffer_size # Commented since preallocating instruction does not give any speedup
        self.wordSize = None
        self.byteSize = None
        self.bitSizes = None
        self.cacheLimit = cacheLimit
        self.defines = []
        self.parameters = []
        self.regs = []
        self.regBanks = []
        self.aliasRegs = []
        self.aliasRegBanks = []
        self.abi = None
        self.pipes = []
        self.isa = None
        self.coprocessor = coprocessor
        self.startup = None
        self.shutdown = None
        self.reset = None
        self.irqs = []
        self.pins = []
        self.fetchReg = None
        self.memory = None
        self.tlmPorts = {}
        #self.regOrder = {}
        self.memAlias = []
        self.systemc = systemc
        self.instructionCache = instructionCache
        self.fastFetch = fastFetch
        self.externalClock = externalClock
        self.preProcMacros = []
        self.tlmFakeMemProperties = ()
        self.license_text = licenses.create_gpl_license(self.name)
        self.license = 'gpl'
        self.developer_name = ''
        self.developer_email = ''
        self.banner = ''
        self.invalid_instr = None

    def setIpRights(self, license, developer_name = '', developer_email = '', banner = '', license_text = ''):
        validLicense = ['gpl', 'lgpl', 'affero', 'esa', 'custom']
        if not license.lower() in validLicense:
            raise Exception('Invalid license ' + license + ', expected one of ' + ' '.join(validLicense))
        if license.lower() == 'custom':
            if not license_text:
                raise Exception('Expected license_text parameter for custom license.')
            else:
                self.license_text = license_text
        else:
            self.license_text = getattr(licenses, 'create_' + license.lower() + '_license')(self.name)
        self.developer_name = developer_name
        self.developer_email = developer_email
        self.banner = banner
        self.license = license.lower()

    def setBigEndian(self):
        self.isBigEndian = True

    def setLittleEndian(self):
        self.isBigEndian = False

    def setWordsize(self, wordSize = 4, byteSize = 8):
        self.wordSize = wordSize
        self.byteSize = byteSize

    # ..........................................................................
    # Instruction Set Architecture Configuration

    def setISA(self, isa):
        self.isa = isa

    # ..........................................................................
    # Pipeline Configuration

    def addPipeStage(self, pipe):
        for i in self.pipes:
            if i.name == pipe.name:
                raise Exception('Pipeline stage ' + pipe.name + ' already exists in processor ' + self.name + '.')
        self.pipes.append(pipe)

    # ..........................................................................
    # Storage Configuration

    def addRegister(self, reg):
        for i in self.regs:
            if reg.name == i.name:
                raise Exception('Register ' + reg.name + ' conflicts with register ' + i.name + ' in processor ' + self.name + '.')
        for i in self.regBanks:
            if reg.name == i.name:
                raise Exception('Register ' + reg.name + ' conflicts with register bank ' + i.name + ' in processor ' + self.name + '.')
        for i in self.aliasRegs:
            if reg.name == i.name:
                raise Exception('Register ' + reg.name + ' conflicts with alias register ' + i.name + ' in processor ' + self.name + '.')
        for i in self.aliasRegBanks:
            if reg.name == i.name:
                raise Exception('Register ' + reg.name + ' conflicts with alias register bank ' + i.name + ' in processor ' + self.name + '.')
        self.regs.append(reg)

    def addRegBank(self, regBank):
        for i in self.regs:
            if regBank.name == i.name:
                raise Exception('Register bank ' + regBank.name + ' conflicts with register ' + i.name + ' in processor ' + self.name + '.')
        for i in self.regBanks:
            if regBank.name == i.name:
                raise Exception('Register bank ' + regBank.name + ' conflicts with register bank ' + i.name + ' in processor ' + self.name + '.')
        for i in self.aliasRegs:
            if regBank.name == i.name:
                raise Exception('Register bank ' + regBank.name + ' conflicts with alias register ' + i.name + ' in processor ' + self.name + '.')
        for i in self.aliasRegBanks:
            if regBank.name == i.name:
                raise Exception('Register bank ' + regBank.name + ' conflicts with alias register bank ' + i.name + ' in processor ' + self.name + '.')
        self.regBanks.append(regBank)

    def addAliasReg(self, alias):
        for i in self.regs:
            if alias.name == i.name:
                raise Exception('Alias register ' + alias.name + ' conflicts with register ' + i.name + ' in processor ' + self.name + '.')
        for i in self.regBanks:
            if alias.name == i.name:
                raise Exception('Alias register ' + alias.name + ' conflicts with register bank ' + i.name + ' in processor ' + self.name + '.')
        for i in self.aliasRegs:
            if alias.name == i.name:
                raise Exception('Alias register ' + alias.name + ' conflicts with alias register ' + i.name + ' in processor ' + self.name + '.')
        for i in self.aliasRegBanks:
            if alias.name == i.name:
                raise Exception('Alias register ' + alias.name + ' conflicts with alias register bank ' + i.name + ' in processor ' + self.name + '.')
        self.aliasRegs.append(alias)

    def addAliasRegBank(self, alias):
        for i in self.regs:
            if alias.name == i.name:
                raise Exception('Alias register bank ' + alias.name + ' conflicts with register ' + i.name + ' in processor ' + self.name + '.')
        for i in self.regBanks:
            if alias.name == i.name:
                raise Exception('Alias register bank ' + alias.name + ' conflicts with register bank ' + i.name + ' in processor ' + self.name + '.')
        for i in self.aliasRegs:
            if i.name in alias.name or alias.name in i.name:
                raise Exception('Alias register bank ' + alias.name + ' conflicts with alias register ' + i.name + ' in processor ' + self.name + '.')
        for i in self.aliasRegBanks:
            if alias.name == i.name:
                raise Exception('Alias register bank ' + alias.name + ' conflicts with alias register bank ' + i.name + ' in processor ' + self.name + '.')
        self.aliasRegBanks.append(alias)

    def setFetchRegister(self, fetchReg,  offset = 0):
        """Sets the correspondence between the fetch address
        and a register inside the processor"""
        found = False
        for i in self.aliasRegs + self.regs:
            if i.name == fetchReg:
                found = True
                break
        if not found:
            index = extractRegInterval(fetchReg)
            if not index:
                raise Exception('Cannot set register ' + fetchReg + ' as a fetch register. Register does not exist.')
            if index[0] != index[1]:
                raise Exception('Cannot set multiple registers ' + fetchReg + ' as a fetch register.')
            name = fetchReg[:fetchReg.index('[')]
            for i in self.aliasRegBanks + self.regBanks:
                if i.name == name:
                    if index[0] < i.numRegs:
                        found = True
                        break
                    else:
                        raise Exception('Register ' + fetchReg + ' does not exist in register bank ' + name + ' containing ' + str(i.numRegs) + ' registers.')
        if not found:
            raise Exception('Cannot set register ' + fetchReg + ' as a fetch register. Register does not exist.')
        self.fetchReg = (fetchReg,  offset)

    def addMemAlias(self, memAlias):
        self.memAlias.append(memAlias)

    def setMemory(self, name, mem_size, debug = False, program_counter = ''):
        for name, isFetch in self.tlmPorts.items():
            if isFetch:
                raise Exception('Cannot specify internal memory for fetching instructions since an external fetch port ' + name + ' is already set.')
        self.memory = (name, mem_size, debug, program_counter)

    def setTLMMem(self, mem_size, memLatency, sparse = False):
        """the memory latency is exrepssed in us"""
        self.tlmFakeMemProperties = (mem_size, memLatency, sparse)

    # ..........................................................................
    # Interface Configuration

    def addIrq(self, irq):
        for i in self.irqs:
            if i.name == irq.name:
                raise Exception('Interrupt port ' + i.name + ' already exists in processor ' + self.name + '.')
        self.irqs.append(irq)

    def addPin(self, pin):
        for i in self.pins:
            if i.name == pin.name:
                raise Exception('External pin ' + i.name + ' already exists in processor ' + self.name + '.')
        self.pins.append(pin)

    def addTLMPort(self, portName, fetch = False):
        """Note that for the TLM port, if only one is specified and the it is the
        port for the fetch, another port called portName + '_fetch' will be automatically
        instantiated. the port called portName can be, instead, used for accessing normal
        data"""
        if not self.systemc:
            raise Exception('Cannot use TLM ports if SystemC is not enabled.')
        if fetch and self.memory:
            raise Exception('Cannot specify port ' + portName + ' as a fetch port since the internal memory is already set for fetching instructions.')
        for name,isFetch  in self.tlmPorts.items():
            if name == portName:
                raise Exception('Port ' + portName + ' already exists.')
            if fetch and isFetch:
                raise Exception('Cannot specify port ' + portName + ' as a fetch port since port ' + name + ' is already set as a fetch port.')
        self.tlmPorts[portName] = fetch

    def setABI(self, abi):
        if self.coprocessor:
            print ('Warning: No need to set an ABI for coprocessor ' + self.name + '.')
        self.abi = abi

    # ..........................................................................
    # Behavior Configuration

    def addDefine(self, define, includes = []):
        self.defines.append(cxx_writer.Define(define + '\n', includes = includes))

    def setPreProcMacro(self, wafOption, macro):
        self.preProcMacros.append( (wafOption, macro) )

    def addParameter(self, name, varType, default):
        for i in self.parameters:
            if i.name == name:
                raise Exception('Parameter ' + name + ' already exists in processor ' + self.name + '.')
        parameter = cxx_writer.Parameter(name, type = varType, initValue = default)
        self.parameters.append(parameter)

    def setBeginOperation(self, code):
        """if is an instance of cxx_writer.CustomCode,
        containing the code for the behavior
        If no begin operation is specified, the default
        values for the registers are used"""
        self.startup = code

    def setEndOperation(self, code):
        """if is an instance of cxx_writer.CustomCode,
        containing the code for the behavior
        If no end operation is specified, nothing
        is done"""
        self.shutdown = code

    def setResetOperation(self, code):
        """if is an instance of cxx_writer.CustomCode,
        containing the code for the behavior
        if no reset operation is specified, the
        begin operation is called"""
        self.reset = code

    ## @} Initialization and Configuration
    #---------------------------------------------------------------------------
    ## @name Checks
    #  @{

    # ..........................................................................
    # Pipeline Checks

    def checkPipeStages(self):
        for instrName, instr in self.isa.instructions.items():
            for stage, beh in instr.prebehaviors.items():
                if not stage in [i.name for i in self.pipes]:
                    raise Exception('Pipeline stage ' + stage + ' specified for behavior ' + beh.name + ' in instruction ' + instrName + ' does not exist.')
            for stage, beh in instr.postbehaviors.items():
                if not stage in [i.name for i in self.pipes]:
                    raise Exception('Pipeline stage ' + stage + ' specified for behavior ' + beh.name + ' in instruction ' + instrName + ' does not exist.')
            for stage in instr.code.keys():
                if not stage in [i.name for i in self.pipes]:
                    raise Exception('Pipeline stage ' + stage + ' specified for code in instruction ' + instrName + ' does not exist.')
        for method in self.isa.methods:
            if not method.stage in [i.name for i in self.pipes]:
                raise Exception('Pipeline stage ' + stage + ' specified for method ' + method.name + ' does not exist.')

        fetchStage = False
        checkHazardStage = False
        wbStage = False
        for pipeStage in self.pipes:
            if pipeStage.fetch:
                if fetchStage:
                    raise Exception('Cannot have more than one pipeline stage for fetching instructions.')
                else:
                    fetchStage = True
            if pipeStage.checkHazard:
                checkHazardStage = True
            if pipeStage.wb or pipeStage.endHazard:
                wbStage = True
            for fromStage in pipeStage.wbStageOrder:
                if fromStage not in self.pipes + [pipe.name for pipe in self.pipes]:
                    raise Exception('Cannot set write-back order for pipe stage ' + pipeStage.name + '. Pipeline stage ' + fromStage + ' does not exist.')
        if (wbStage and not checkHazardStage) or (not wbStage and checkHazardStage):
            raise Exception('Both writeback and check hazards stages must be specified.')

    def checkPipeRegs(self):
        for reg in self.regs:
            for fromStage, toStages in reg.wbStageOrder.iteritems():
                if fromStage not in self.pipes + [pipe.name for pipe in self.pipes]:
                    raise Exception('Cannot set write-back order for register ' + reg.name + '. Pipeline stage ' + fromStage + ' does not exist.')
                for toStage in toStages:
                    if toStage not in self.pipes + [pipe.name for pipe in self.pipes]:
                        raise Exception('Cannot set write-back order for register ' + reg.name + '. Pipeline stage ' + toStage + ' does not exist.')

    # ..........................................................................
    # Storage Checks

    def isRegExisting(self, name, index = None):
        if index:
            # I have to check for a register bank
            for i in self.regBanks:
                if name == i.name:
                    if index[0] >= 0 and index[1] <= i.numRegs:
                        return i
                    else:
                        raise Exception('Register ' + str(index[1]) + ' does not exist in register bank ' + i.name + ' containing ' + str(i.numRegs) + ' registers.')
            for i in self.aliasRegBanks:
                if name == i.name:
                    if index[0] >= 0 and index[1] <= i.numRegs:
                        return i
                    else:
                        raise Exception('Register ' + str(index[1]) + ' does not exist in alias register bank ' + i.name + ' containing ' + str(i.numRegs) + ' registers.')
        else:
            for i in self.regs:
                if name == i.name:
                    return i
            for i in self.aliasRegs:
                if name == i.name:
                    return i
        return None

    def isBank(self, bankName):
        for i in self.regBanks + self.aliasRegBanks:
            if bankName == i.name:
                return True
        return False

    def checkAliases(self):
        """checks that the declared aliases actually refer to
        existing registers"""
        for alias in self.aliasRegBanks:
            # I have to check that the registers alised by
            # this register bank actually exists and that
            # intervals, if used, are correct
            if isinstance(alias.initAlias, str):
                index = extractRegInterval(alias.initAlias)
                # I'm aliasing part of a register bank or another alias:
                # I check that it exists and that I am still within
                # boundaries
                refName = alias.initAlias[:alias.initAlias.find('[')]
                regInstance = self.isRegExisting(refName, index)
                if regInstance is None:
                    raise Exception('Register bank ' + refName + ' referenced by alias ' + alias.name + ' does not exist.')
                try:
                    try:
                        for value in alias.defValues:
                            if value != None and value > 0:
                                import math
                                if math.log(value, 2) > regInstance.bitWidth:
                                    raise Exception('Default value ' + str(value) + ' of alias register bank ' + alias.name + ' requires ' + str(int(math.ceil(math.log(value, 2)))) + ' bits, but the register has a width of ' + str(regInstance.bitWidth) + ' bits.')
                    except TypeError:
                        pass
                except AttributeError:
                    pass
            else:
                totalRegs = 0
                for i in range(0, len(alias.initAlias)):
                    index = extractRegInterval(alias.initAlias[i])
                    if index:
                        # I'm aliasing part of a register bank or another alias:
                        # I check that it exists and that I am still within
                        # boundaries
                        refName = alias.initAlias[i][:alias.initAlias[i].find('[')]
                        regInstance = self.isRegExisting(refName, index)
                        if regInstance is None:
                            raise Exception('Register bank ' + alias.initAlias[i] + ' referenced by alias ' + alias.name + ' does not exist.')
                        try:
                            try:
                                if alias.defValues[i] != None and alias.defValues[i] > 0:
                                    import math
                                    if math.log(alias.defValues[i], 2) > regInstance.bitWidth:
                                        raise Exception('Default value ' + str(alias.defValues[i]) + ' of alias register bank ' + alias.name + ' requires ' + str(int(math.ceil(math.log(alias.defValues[i], 2)))) + ' bits, but the register has a width of ' + str(regInstance.bitWidth) + ' bits.')
                            except TypeError:
                                pass
                        except AttributeError:
                            pass
                    else:
                        # Single register or alias: I check that it exists
                        regInstance = self.isRegExisting(alias.initAlias[i])
                        if regInstance is None:
                            raise Exception('Register ' + alias.initAlias[i] + ' referenced by alias ' + alias.name + ' does not exist.')
                        try:
                            try:
                                if alias.defValues[i] != None and alias.defValues[i] > 0:
                                    import math
                                    if math.log(alias.defValues[i], 2) > regInstance.bitWidth:
                                        raise Exception('Default value ' + str(alias.defValues[i]) + ' of alias register bank ' + alias.name + ' requires ' + str(int(math.ceil(math.log(alias.defValues[i], 2)))) + ' bits, but the register has a width of ' + str(regInstance.bitWidth) + ' bits.')
                            except TypeError:
                                pass
                        except AttributeError:
                            pass
        for alias in self.aliasRegs:
            index = extractRegInterval(alias.initAlias)
            if index:
                # I'm aliasing part of a register bank or another alias:
                # I check that it exists and that I am still within
                # boundaries
                refName = alias.initAlias[:alias.initAlias.find('[')]
                regInstance = self.isRegExisting(refName, index)
                if regInstance is None:
                    raise Exception('Register bank ' + alias.initAlias + ' referenced by alias ' + alias.name + ' does not exist.')
                try:
                    try:
                        if alias.defValue != None and alias.defValue > 0:
                            import math
                            if math.log(alias.defValue, 2) > regInstance.bitWidth:
                                raise Exception('Default value ' + str(alias.defValue) + ' of alias register bank ' + alias.name + ' requires ' + str(int(math.ceil(math.log(alias.defValue, 2)))) + ' bits, but the register has a width of ' + str(regInstance.bitWidth) + ' bits.')
                    except TypeError:
                        pass
                except AttributeError:
                    pass
            else:
                # Single register or alias: I check that it exists
                regInstance = self.isRegExisting(refName, index)
                if regInstance is None:
                    raise Exception('Register ' + alias.initAlias + ' referenced by alias ' + alias.name + ' does not exist.')
                try:
                    try:
                        if alias.defValue != None and alias.defValue > 0:
                            import math
                            if math.log(alias.defValue, 2) > regInstance.bitWidth:
                                raise Exception('Default value ' + str(alias.defValue) + ' of alias register bank ' + alias.name + ' requires ' + str(int(math.ceil(math.log(alias.defValue, 2)))) + ' bits, but the register has a width of ' + str(regInstance.bitWidth) + ' bits.')
                    except TypeError:
                        pass
                except AttributeError:
                    pass

    def checkMemRegisters(self):
        if not self.memory and not self.tlmPorts:
            raise Exception('Please specify either an internal memory (using setMemory()) or a TLM memory port (using addTLMPort()).')
        if not self.fetchReg:
            raise Exception('Please specify a fetch register using setFetchRegister() (usually the PC).')
        for memAliasReg in self.memAlias:
            index = extractRegInterval(memAliasReg.alias)
            if index:
                # I'm aliasing part of a register bank or another alias:
                # I check that it exists and that I am still within
                # boundaries
                regName = memAliasReg.alias[:memAliasReg.alias.find('[')]
                if self.isRegExisting(regName, index) is None:
                    raise Exception('Cannot set memory alias for register ' + memAliasReg.alias + ' at address ' + memAliasReg.address + '. Register does not exist.')
            else:
                # Single register or alias: I check that it exists
                if self.isRegExisting(memAliasReg.alias) is None:
                    raise Exception('Cannot set memory alias for register ' + memAliasReg.alias + ' at address ' + memAliasReg.address + '. Register does not exist.')
        if self.memory and self.memory[3]:
            index = extractRegInterval(self.memory[3])
            if index:
                # I'm aliasing part of a register bank or another alias:
                # I check that it exists and that I am still within
                # boundaries
                regName = self.memory[3][:self.memory[3].find('[')]
                if self.isRegExisting(regName, index) is None:
                    raise Exception('Cannot set register ' + self.memory[3] + ' as the program counter. Register does not exist in local memory.')
            else:
                # Single register or alias: I check that it exists
                if self.isRegExisting(self.memory[3]) is None:
                    raise Exception('Cannot set register ' + self.memory[3] + ' as the program counter. Register does not exist in local memory.')

    def checkISARegs(self):
        """Checks that registers declared in the instruction encoding and the ISA really exists"""
        architecturalNames = [archElem.name for archElem in self.regs + self.regBanks + self.aliasRegs + self.aliasRegBanks]
        for name, instruction in self.isa.instructions.items():
            # inside each instruction I have to check for registers defined in the machine code (bitCorrespondence),
            # the correspondence declared inside the instruction itself (bitCorrespondence), the input and output
            # special registers (specialInRegs, specialOutRegs)
            for regName in instruction.machineCode.bitCorrespondence.values():
                if not regName[0] in architecturalNames:
                    raise Exception('Architectural element ' + str(regName[0]) + ' specified in the machine code of instruction ' + name + ' does not exist.')
            for regName in instruction.bitCorrespondence.values():
                if not regName[0] in architecturalNames:
                    raise Exception('Architectural element ' + str(regName[0]) + ' specified in the machine code of instruction ' + name + ' does not exist.')
            outRegs = []
            for regList in instruction.specialOutRegs.values():
                outRegs += regList
            inRegs = []
            for regList in instruction.specialInRegs.values():
                inRegs += regList
            for regName in inRegs + outRegs:
                index = extractRegInterval(regName)
                if index:
                    # I'm aliasing part of a register bank or another alias:
                    # I check that it exists and that I am still within
                    # boundaries
                    refName = regName[:regName.find('[')]
                    if self.isRegExisting(refName, index) is None:
                        raise Exception('Register bank ' + regName + ' referenced as spcial register in instruction ' + name + ' does not exist.')
                else:
                    # Single register or alias: I check that it exists
                    if self.isRegExisting(regName) is None:
                        raise Exception('Register ' + regName + ' referenced as spcial register in instruction ' + name + ' does not exist.')
            pipeStageName = [i.name for i in self.pipes] + ['default']
            beforeCheck = []
            wbStageName = 'default'
            hazardStageName = 'default'
            for i in self.pipes:
                if i.checkHazard:
                    hazardStageName = i.name
                elif i.endHazard:
                    wbStageName = i.name
            newOutRegs = {}
            for stage, regs in instruction.specialOutRegs.items():
                if stage == 'default':
                    stage = wbStageName
                if not stage in pipeStageName:
                    raise Exception('Stage ' + stage + ' specified for special register of instruction ' + name + ' does not exist.')
                newOutRegs[stage] = regs
            instruction.specialOutRegs = newOutRegs
            newInRegs = {}
            for stage, regs in instruction.specialInRegs.items():
                if stage == 'default':
                    stage = hazardStageName
                if not stage in pipeStageName:
                    raise Exception('Stage ' + stage + ' specified for special register of instruction ' + name + ' does not exist.')
                newInRegs[stage] = regs
            instruction.specialInRegs = newInRegs

    def checkTestRegs(self):
        """We check that the registers specified in the tests exist"""
        outPinPorts = []
        for pinPort in self.pins:
            if not pinPort.inbound:
                outPinPorts.append(pinPort.name)

        for instr in self.isa.instructions.values():
            for test in instr.tests:
                # Now I check the existence of the instruction fields
                for name, elemValue in test[0].items():
                    if not instr.machineCode.bitLen.has_key(name):
                        raise Exception('Field ' + name + ' in test of instruction ' + instr.name + ' does not exist in the machine code.')
                for resource, value in test[1].items():
                    # Now I check the existence of the global resources
                    brackIndex = resource.find('[')
                    memories = self.tlmPorts.keys()
                    if self.memory:
                        memories.append(self.memory[0])
                    if not (brackIndex > 0 and resource[:brackIndex] in memories):
                        index = extractRegInterval(resource)
                        if index:
                            resourceName = resource[:brackIndex]
                            if self.isRegExisting(resourceName, index) is None:
                                raise Exception('Resource ' + resource + ' in test of instruction ' + instr.name + ' does not exist.')
                        else:
                            if self.isRegExisting(resource) is None:
                                raise Exception('Resource ' + resource + ' in test of instruction ' + instr.name + ' does not exist.')
                for resource, value in test[2].items():
                    brackIndex = resource.find('[')
                    memories = self.tlmPorts.keys()
                    if self.memory:
                        memories.append(self.memory[0])
                    if not (brackIndex > 0 and (resource[:brackIndex] in memories or resource[:brackIndex] in outPinPorts)):
                        index = extractRegInterval(resource)
                        if index:
                            resourceName = resource[:brackIndex]
                            if self.isRegExisting(resourceName, index) is None:
                                raise Exception('Resource ' + resource + ' in test of instruction ' + instr.name + ' does not exist.')
                        else:
                            if self.isRegExisting(resource) is None:
                                raise Exception('Resource ' + resource + ' in test of instruction ' + instr.name + ' does not exist.')

    # ..........................................................................
    # Interface Checks

    def checkIRQPorts(self):
        """So far I only have to check that the stages of the IRQ operations are existing"""
        stageNames = [i.name for i in self.pipes]
        for irq in self.irqs:
            for stage in irq.operation.keys():
                if not stage in stageNames:
                    raise Exception('Pipeline stage ' + stage + ' specified for interrupt ' + irq.name + ' does not exist.')

    def checkABI(self):
        """checks that the registers specified for the ABI interface
        refer to existing registers"""
        index = extractRegInterval(self.abi.RetVal)
        if index:
            regBound = self.abi.RetVal[self.abi.RetVal.find('['):self.abi.RetVal.find(']')]
            if '-' in regBound:
                raise Exception('Cannot set multiple registers as an ABI return value register.')
        toCheck = [self.abi.RetVal, self.abi.PC]
        if self.abi.LR:
            toCheck.append(self.abi.LR)
        if self.abi.FP:
            toCheck.append(self.abi.FP)
        if self.abi.SP:
            toCheck.append(self.abi.SP)
        for reg in self.abi.stateIgnoreRegs:
            toCheck.append(reg)
        if isinstance(self.abi.args, str):
            toCheck.append(self.abi.args)
        else:
            for i in self.abi.args:
                toCheck.append(i)
        for i in self.abi.regCorrespondence.keys():
            toCheck.append(i)
        if self.abi.returnCallReg:
            for returnReg in self.abi.returnCallReg:
                toCheck.append(returnReg[0])
                toCheck.append(returnReg[1])
        # ok, now I finally perform the check
        for i in toCheck:
            index = extractRegInterval(i)
            if index:
                # I'm aliasing part of a register bank or another alias:
                # I check that it exists and that I am still within
                # boundaries
                refName = i[:i.find('[')]
                if self.isRegExisting(refName, index) is None:
                    raise Exception('Register bank ' + i + ' used in the ABI does not exist.')
            else:
                # Single register or alias: I check that it exists
                if self.isRegExisting(i) is None:
                    raise Exception('Register ' + i + ' used in the ABI does not exist.')
        # warning in case details are not specified
        if not self.abi.returnCallInstr or not self.abi.callInstr:
            print('Warning: "returnCallInstr" or "callInstr" not specified in the ABI. The profiler may give incorrect results.')
        ################# TODO: check also the memories #######################

    ## @} Checks
    #---------------------------------------------------------------------------
    ## @name Model Generation
    #  @{

    def write(self, folder = '', models = validModels, namespace = '', dumpDecoderName = '', trace = False, combinedTrace = False, forceDecoderCreation = False, tests = True, memPenaltyFactor = 4):
        """Ok: this method does two things: first of all it performs all
        the possible checks to ensure that the processor description is
        coherent. Second it actually calls the write method of the
        processor components (registers, instructions, etc.) to create
        the code of the simulator"""

        print ('\tCreating processor model ' + self.name + '...')
        print ('\t\tChecking the consistency of the specification...')

        # ..........................................................................
        # Checks

        # Instruction Set Architecture Checks
        self.isa.computeCoding()
        self.isa.checkCoding()

        # Pipeline Checks
        self.checkPipeStages()
        self.checkPipeRegs()

        # Storage Checks
        self.checkAliases()
        self.checkMemRegisters()
        self.checkISARegs()
        self.isa.checkRegisters(extractRegInterval, self.isRegExisting)
        self.checkTestRegs()

        # Interface Checks
        if ('funcAT' in models or 'accAT' in models or 'accLT' in models) and not self.tlmPorts:
            raise Exception('TLM ports are required for all models other than funcLT. Please specify at least one TLM port.')
        self.checkIRQPorts()
        if self.abi:
            self.checkABI()

        # ..........................................................................
        # Model Generation

        # Decoder Model Generation
        from isa import resolveBitType
        import decoder, os
        import cxx_writer
        # Now I declare a couple of variables used for keeping track of the size of each words and parts for them
        self.bitSizes = [resolveBitType('BIT<' + str(self.wordSize*self.byteSize*2) + '>'),
                        resolveBitType('BIT<' + str(self.wordSize*self.byteSize) + '>'),
                        resolveBitType('BIT<' + str(self.wordSize*self.byteSize/2) + '>'),
                        resolveBitType('BIT<' + str(self.byteSize) + '>')]

        cxx_writer.FileDumper.license = self.license
        cxx_writer.FileDumper.license_text = self.license_text
        cxx_writer.FileDumper.developer_name = self.developer_name
        cxx_writer.FileDumper.developer_email = self.developer_email
        cxx_writer.FileDumper.banner = self.banner

        # Here we check if the decoder signature changed; in case it hasn't we create the decoder,
        # otherwise we load it from file
        instructionSignature = self.isa.getInstructionSig()
        if not forceDecoderCreation:
            if os.path.exists(os.path.join(os.path.expanduser(os.path.expandvars(folder)), '.decoderSig')) and os.path.exists(os.path.join(os.path.expanduser(os.path.expandvars(folder)), '.decoderDump.pickle')):
                # Now I have to compare the saved signature with the signature of the current
                # instructions
                try:
                    decSigFile = open(os.path.join(os.path.expanduser(os.path.expandvars(folder)), '.decoderSig'), 'r')
                    savedSig = decSigFile.read()
                    decSigFile.close()
                    if savedSig != instructionSignature:
                        forceDecoderCreation = True
                except:
                    try:
                        decSigFile.close()
                    except:
                        pass
                    forceDecoderCreation = True
            else:
                forceDecoderCreation = True
        if forceDecoderCreation:
            print ('\t\tCreating the decoder...')
            dec = decoder.decoderCreator(self.isa.instructions, self.isa.subInstructions, memPenaltyFactor)
            dec.invalid_instr = self.invalid_instr
            import copy
            decCopy = copy.deepcopy(dec)
        else:
            try:
                print ('\t\tLoading the decoder from cache...')
                import pickle
                decDumpFile = open(os.path.join(os.path.expanduser(os.path.expandvars(folder)), '.decoderDump.pickle'), 'r')
                dec = pickle.load(decDumpFile)
                decDumpFile.close()
            except:
                print ('\t\tError in loading the decoder.')
                print ('\t\tRe-creating the decoder...')
                dec = decoder.decoderCreator(self.isa.instructions, self.isa.subInstructions, memPenaltyFactor)
                dec.invalid_instr = self.invalid_instr
                import copy
                decCopy = copy.deepcopy(dec)
                forceDecoderCreation = True
        if dumpDecoderName:
            dec.printDecoder(dumpDecoderName)

        mainFolder = cxx_writer.Folder(os.path.expanduser(os.path.expandvars(folder)))

        for model in models:
            # Here I add the define code, defining the type of the current model;
            # such define code has to be added to each created header file
            defString = '#define ' + model[:-2].upper() + '_MODEL\n'
            defString += '#define ' + model[-2:].upper() + '_IF\n'
            defCode = cxx_writer.Define(defString)
            cxx_writer.FileDumper.def_prefix = 'CORE_' + self.name.upper() + '_' +  model[:-2].upper() + '_' + model[-2:].upper() + '_'

            # Now I also set the processor class name: note that even if each model has a
            # separate namespace, some buggy dynamic linkers complain, so we must also
            # use separate names for the processor class
            procWriter.processor_name = 'Core' + self.name + model[0].upper() + model[1:]

            print ('\t\tCreating ' + model + ' implementation...')
            if not model in validModels:
                raise Exception('Invalid model ' + model + '.')
            if not namespace:
                namespace = 'core_' + self.name.lower() + '_' + model.lower()
            namespaceUse = cxx_writer.UseNamespace(namespace)
            namespaceTrapUse = cxx_writer.UseNamespace('trap')

            curFolder = cxx_writer.Folder(os.path.join(folder, model))
            mainFolder.addSubFolder(curFolder)

            # Decoder Model Generation
            decoderClasses = dec.getCPPClass(self.bitSizes[1], self.instructionCache, namespace)
            decoderHeadFile = cxx_writer.FileDumper('decoder.hpp', True)
            decoderHeadFile.addMember(defCode)
            decoderImplFile = cxx_writer.FileDumper('decoder.cpp', False)
            decoderImplFile.addInclude('#include \"decoder.hpp\"')
            decoderImplFile.addMember(namespaceUse)
            for i in decoderClasses:
                decoderHeadFile.addMember(i)
                decoderImplFile.addMember(i)
            curFolder.addHeader(decoderHeadFile)
            curFolder.addCode(decoderImplFile)

            # Register Model Generation
            registerClasses = registerWriter.getCPPRegisters(self, trace, combinedTrace, model, namespace)
            registerHeadFile = cxx_writer.FileDumper('registers.hpp', True)
            registerHeadFile.addMember(defCode)
            registerHeadFile.addMember(self.defines)
            registerHeadFile.addMember(registerWriter.getCPPRegisterDefines(self))
            registerHeadFile.addMember(registerWriter.getCPPRegisterFields(self))
            registerHeadFile.addMember(registerClasses)
            registerImplFile = cxx_writer.FileDumper('registers.cpp', False)
            registerImplFile.addInclude('#include \"registers.hpp\"')
            registerImplFile.addMember(namespaceUse)
            registerImplFile.addMember(registerClasses)
            curFolder.addHeader(registerHeadFile)
            curFolder.addCode(registerImplFile)

            # Processor Model Generation
            processorClass = procWriter.getCPPProcessor(self, model, trace, combinedTrace, namespace)
            processorHeadFile = cxx_writer.FileDumper('processor.hpp', True)
            processorHeadFile.addMember(defCode)
            processorHeadFile.addMember(namespaceTrapUse)
            processorHeadFile.addMember(processorClass)
            processorImplFile = cxx_writer.FileDumper('processor.cpp', False)
            processorImplFile.addInclude('#include \"processor.hpp\"')
            processorImplFile.addMember(namespaceUse)
            processorImplFile.addMember(namespaceTrapUse)
            processorImplFile.addMember(processorClass)
            curFolder.addHeader(processorHeadFile)
            curFolder.addCode(processorImplFile)

            # Instruction Set Architecture Model Generation
            instrClasses = self.isa.getCPPInstructions(self, model, trace, combinedTrace, namespace)
            if self.irqs:
                instrClasses += irqWriter.getCPPIRQInstr(self, model, trace, namespace)
            instrHeadFile = cxx_writer.FileDumper('instructions.hpp', True)
            instrHeadFile.addMember(defCode)
            instrImplFile = cxx_writer.FileDumper('instructions.cpp', False)
            instrImplFile.addMember(namespaceUse)
            for i in instrClasses:
                instrHeadFile.addMember(i)
                instrImplFile.addMember(i)
            curFolder.addHeader(instrHeadFile)
            curFolder.addCode(instrImplFile)

            # Pipeline Model Generation
            if model.startswith('acc'):
                pipelineClasses = pipelineWriter.getCPPPipeline(self, trace, combinedTrace, model, namespace)
                pipelineHeadFile = cxx_writer.FileDumper('pipeline.hpp', True)
                pipelineHeadFile.addMember(defCode)
                pipelineHeadFile.addMember(namespaceTrapUse)
                pipelineImplFile = cxx_writer.FileDumper('pipeline.cpp', False)
                pipelineImplFile.addInclude('#include \"pipeline.hpp\"')
                pipelineImplFile.addMember(namespaceUse)
                pipelineImplFile.addMember(namespaceTrapUse)
                for i in pipelineClasses:
                    pipelineHeadFile.addMember(i)
                    pipelineImplFile.addMember(i)
                curFolder.addHeader(pipelineHeadFile)
                curFolder.addCode(pipelineImplFile)

            # Memory Model Generation
            memoryClasses = memWriter.getCPPMemoryIf(self, model, namespace)
            memoryHeadFile = cxx_writer.FileDumper('memory.hpp', True)
            memoryHeadFile.addMember(defCode)
            memoryHeadFile.addMember(namespaceTrapUse)
            memoryImplFile = cxx_writer.FileDumper('memory.cpp', False)
            memoryImplFile.addInclude('#include \"memory.hpp\"')
            memoryImplFile.addMember(namespaceUse)
            memoryImplFile.addMember(namespaceTrapUse)
            for i in memoryClasses:
                memoryHeadFile.addMember(i)
                memoryImplFile.addMember(i)
            curFolder.addHeader(memoryHeadFile)
            curFolder.addCode(memoryImplFile)

            # Interface Model Generation
            if self.irqs:
                irqClasses = portsWriter.getCPPIRQPorts(self, namespace)
                if irqClasses:
                    irqHeadFile = cxx_writer.FileDumper('irqPorts.hpp', True)
                    irqHeadFile.addMember(defCode)
                    irqImplFile = cxx_writer.FileDumper('irqPorts.cpp', False)
                    irqImplFile.addInclude('#include \"irqPorts.hpp\"')
                    irqImplFile.addMember(namespaceUse)
                    for i in irqClasses:
                        irqHeadFile.addMember(i)
                        irqImplFile.addMember(i)
                    curFolder.addHeader(irqHeadFile)
                    curFolder.addCode(irqImplFile)

            if self.pins:
                pinClasses = portsWriter.getCPPPINPorts(self, namespace)
                if pinClasses:
                    pinHeadFile = cxx_writer.FileDumper('externalPins.hpp', True)
                    pinHeadFile.addMember(defCode)
                    pinImplFile = cxx_writer.FileDumper('externalPins.cpp', False)
                    pinImplFile.addInclude('#include \"externalPins.hpp\"')
                    pinImplFile.addMember(namespaceUse)
                    for i in pinClasses:
                        pinHeadFile.addMember(i)
                        pinImplFile.addMember(i)
                    curFolder.addHeader(pinHeadFile)
                    curFolder.addCode(pinImplFile)

            portClasses = portsWriter.getCPPExternalPorts(self, model, namespace)
            if portClasses:
                portHeadFile = cxx_writer.FileDumper('externalPorts.hpp', True)
                portHeadFile.addMember(defCode)
                portHeadFile.addMember(portClasses)
                portImplFile = cxx_writer.FileDumper('externalPorts.cpp', False)
                portImplFile.addInclude('#include \"externalPorts.hpp\"')
                portImplFile.addMember(namespaceUse)
                portImplFile.addMember(portClasses)
                curFolder.addHeader(portHeadFile)
                curFolder.addCode(portImplFile)

            if self.abi:
                abiClasses = interfaceWriter.getCPPIf(self, model, namespace)
                abiHeadFile = cxx_writer.FileDumper('interface.hpp', True)
                abiHeadFile.addMember(defCode)
                abiHeadFile.addMember(namespaceTrapUse)
                abiHeadFile.addMember(abiClasses)
                abiImplFile = cxx_writer.FileDumper('interface.cpp', False)
                abiImplFile.addInclude('#include \"interface.hpp\"')
                abiImplFile.addMember(namespaceUse)
                abiImplFile.addMember(namespaceTrapUse)
                abiImplFile.addMember(abiClasses)
                curFolder.addHeader(abiHeadFile)
                curFolder.addCode(abiImplFile)

            # Platform Generation
            mainCode = procWriter.getCPPMain(self, model, namespace)
            mainHeadFile = cxx_writer.FileDumper('main.hpp', True)
            mainHeadFile.addMember(mainCode)
            mainImplFile = cxx_writer.FileDumper('main.cpp', False)
            mainImplFile.addInclude('#include \"main.hpp\"')
            mainImplFile.addMember(mainCode)
            curFolder.addHeader(mainHeadFile)
            curFolder.addCode(mainImplFile)

            # Testbench Generation
            if (model == 'funcLT') and (not self.systemc) and tests:
                testFolder = cxx_writer.Folder('tests')
                curFolder.addSubFolder(testFolder)

                mainTests = procWriter.getCPPTestMain(self)
                mainTestHeadFile = cxx_writer.FileDumper('main.hpp', True)
                mainTestImplFile = cxx_writer.FileDumper('main.cpp', False)
                mainTestImplFile.addInclude('#include \"main.hpp\"')
                mainTestImplFile.addMember(mainTests)
                mainTestHeadFile.addMember(mainTests)
                testFolder.addHeader(mainTestHeadFile)
                testFolder.addCode(mainTestImplFile)
                testFolder.addUseLib(os.path.split(curFolder.path)[-1] + '_objs')

                decoderTests = dec.getCPPDecoderTests(namespace)
                decoderTestHeadFile = cxx_writer.FileDumper('decoderTests.hpp', True)
                decoderTestHeadFile.addMember(decoderTests)
                decoderTestImplFile = cxx_writer.FileDumper('decoderTests.cpp', False)
                decoderTestImplFile.addInclude('#include \"decoderTests.hpp\"')
                decoderTestImplFile.addMember(decoderTests)
                mainTestImplFile.addInclude('#include \"decoderTests.hpp\"')
                testFolder.addHeader(decoderTestHeadFile)
                testFolder.addCode(decoderTestImplFile)

                isaTests = self.isa.getCPPInstrTests(self, model, trace, combinedTrace, namespace)
                testsPerFile = 100
                numTestFiles = len(isaTests)/testsPerFile
                for i in range(0, numTestFiles):
                    isaTestHeadFile = cxx_writer.FileDumper('isaTests' + str(i) + '.hpp', True)
                    isaTestHeadFile.addMember(isaTests[testsPerFile*i:testsPerFile*(i+1)])
                    isaTestImplFile = cxx_writer.FileDumper('isaTests' + str(i) + '.cpp', False)
                    isaTestImplFile.addInclude('#include \"isaTests' + str(i) + '.hpp\"')
                    if pinClasses:
                        isaTestImplFile.addInclude('modules/pin_target.hpp')
                        isaTestImplFile.addInclude('externalPins.hpp')
                    isaTestImplFile.addMember(namespaceUse)
                    isaTestImplFile.addMember(namespaceTrapUse)
                    isaTestImplFile.addMember(isaTests[testsPerFile*i:testsPerFile*(i+1)])
                    mainTestImplFile.addInclude('#include \"isaTests' + str(i) + '.hpp\"')
                    testFolder.addHeader(isaTestHeadFile)
                    testFolder.addCode(isaTestImplFile)
                if testsPerFile*numTestFiles < len(isaTests):
                    isaTestHeadFile = cxx_writer.FileDumper('isaTests' + str(numTestFiles) + '.hpp', True)
                    isaTestHeadFile.addMember(isaTests[testsPerFile*numTestFiles:])
                    isaTestImplFile = cxx_writer.FileDumper('isaTests' + str(numTestFiles) + '.cpp', False)
                    isaTestImplFile.addInclude('#include \"isaTests' + str(numTestFiles) + '.hpp\"')
                    if pinClasses:
                        isaTestImplFile.addInclude('modules/pin_target.hpp')
                        isaTestImplFile.addInclude('externalPins.hpp')
                    isaTestImplFile.addMember(namespaceUse)
                    isaTestImplFile.addMember(namespaceTrapUse)
                    isaTestImplFile.addMember(isaTests[testsPerFile*numTestFiles:])
                    mainTestImplFile.addInclude('#include \"isaTests' + str(numTestFiles) + '.hpp\"')
                    testFolder.addHeader(isaTestHeadFile)
                    testFolder.addCode(isaTestImplFile)

                irqTests = portsWriter.getCPPIRQTests(self, trace, combinedTrace, namespace)
                if irqTests:
                    irqTestHeadFile = cxx_writer.FileDumper('irqTests.hpp', True)
                    irqTestHeadFile.addMember(irqTests)
                    irqTestImplFile = cxx_writer.FileDumper('irqTests.cpp', False)
                    irqTestImplFile.addInclude('#include \"irqTests.hpp\"')
                    if pinClasses:
                        irqTestImplFile.addInclude('modules/pin_target.hpp')
                        irqTestImplFile.addInclude('externalPins.hpp')
                    irqTestImplFile.addMember(namespaceUse)
                    irqTestImplFile.addMember(namespaceTrapUse)
                    irqTestImplFile.addMember(irqTests)
                    mainTestImplFile.addInclude('#include \"irqTests.hpp\"')
                    testFolder.addHeader(irqTestHeadFile)
                    testFolder.addCode(irqTestImplFile)

            curFolder.setMain(mainImplFile.name)
            curFolder.create()
            if (model == 'funcLT') and (not self.systemc) and tests:
                testFolder.create(configure = False, tests = True)
            print ('\t\tProcessor model successfully created in folder ' + os.path.expanduser(os.path.expandvars(folder)))
            namespace = ''

        mainFolder.create(configure = True, tests = (not self.systemc) and tests, projectName = self.name, version = self.version, customOptions = self.preProcMacros)

        if forceDecoderCreation:
            try:
                import pickle
                decDumpFile = open(os.path.join(os.path.expanduser(os.path.expandvars(folder)), '.decoderDump.pickle'), 'w')
                pickle.dump(decCopy, decDumpFile, pickle.HIGHEST_PROTOCOL)
                decDumpFile.close()
                # Now I have to save the instruction signature
                decSigFile = open(os.path.join(os.path.expanduser(os.path.expandvars(folder)), '.decoderSig'), 'w')
                decSigFile.write(instructionSignature)
                decSigFile.close()
            except:
                pass

    ## @} Code Generation
    #---------------------------------------------------------------------------


################################################################################
# Pipeline Model
################################################################################
# TODO: The whole pipeline model needs an overhaul. The syntax for specifying
# write-back paths is not ideal. Check applicability of ideas in E. Harcourt and
# J. Perconti, "A SystemC Library for Specifying Pipeline Abstractions,"
# Microprocessors and Microsystems, vol. 38, no. 1, pp. 76-81, Feb. 2014.
# Additionally, we cannot model microarchitectural features such as:
# - Out-of-order instruction issue
# - Wide pipelines (multi-issue)
# - Branch prediction
# - Speculative prefetching
# - VLIW
class PipeStage:
    """Identified by (a) name (b) optional, if it is wb,
    the stage where the hazards are checked or where the
    bypassing is started. Note that this is just the default
    information which can be overridden by each instruction"""
    def __init__(self, name):
        self.name = name
        self.fetch = False
        self.checkHazard = False
        # TODO: Cannot see any difference between endHazard and wb. One should go, methinks.
        self.endHazard = False
        self.wb = False
        self.checkUnknown = False
        self.wbStageOrder = []
        self.operation = ''

    def setFetchPC(self):
        self.fetch = True

    def setWriteBack(self):
        self.wb = True

    def setHazard(self):
        self.checkHazard = True

    def setEndHazard(self):
        self.endHazard = True

    def setCheckUnknownInstr(self):
        self.checkUnknown = True

    # This is a list of forwarding paths, so e.g. ['ID', 'EX'], means this stage
    # is fed back from both ID and EX, where ID is given precedence.
    def setWbStageOrder(self, order):
        self.wbStageOrder = order

    def setOperation(self, code):
        self.operation = code


################################################################################
# Storage Models: Registers, Aliases, Register Banks and Memory
################################################################################
class Register:
    """Register of a processor. It is identified by (a) the
    width in bits, (b) the name. It is eventually possible
    to associate names to the different register fields and
    then refer to them instead of having to use bit masks"""
    def __init__(self, name, bitWidth, bitMask = {}):
        self.name = name
        self.bitWidth = bitWidth
        if bitMask:
            for key, value in bitMask.items():
                if value[0] > value[1]:
                    raise Exception('Bit mask start value ' +  str(value[0]) + ' for field ' + key + ' in register ' + self.name + ' is larger than end value ' + str(value[1]) + '.')
                if value[1] >= bitWidth:
                    raise Exception('Bit mask size ' + str(value[1]) + ' for field ' + key + ' in register ' + self.name + ' is larger than register size ' + str(bitWidth) + '.')
                for key1, value1 in bitMask.items():
                    if key1 != key:
                        if (value1[0] <= value[0] and value1[1] >= value[0]) or (value1[0] <= value[1] and value1[1] >= value[1]):
                            raise Exception('Bit mask for field ' + key + ' in register ' + self.name + ' intersects with the mask of field ' + key1 + '.')
        self.bitMask = bitMask
        self.defValue = 0
        self.offset = 0
        self.constValue = None
        self.delay = 0
        self.wbStageOrder = {}

    def setDefaultValue(self, value):
        if self.defValue:
            raise Exception('Default value for register ' + self.name +  ' already set.')
        try:
            if value > 0:
                import math
                if math.log(value, 2) > self.bitWidth:
                    raise Exception('Default value ' + str(value) + ' of register ' + self.name + ' requires ' + str(int(math.ceil(math.log(value, 2)))) + ' bits, but the register has a width of ' + str(self.bitWidth) + ' bits.')
        except TypeError:
            pass
        self.defValue = value

    def setConst(self, value):
        if self.delay:
            raise Exception('Cannot set register ' + self.name + ' as constant because it contains a delay assignment.')
        self.constValue = value

    def setDelay(self, value):
        if self.constValue != None:
            raise Exception('Cannot set a delay assignment for register ' + self.name + ' because it is set as constant.')
        self.delay = value

    def setOffset(self, value):
        """TODO: eliminate this restriction"""
        if self.bitMask:
            raise Exception('Cannot set offset for register ' + self.name + ' because it uses a bit mask.')
        self.offset = value

    # This is a map of forwarding paths stage -> stages, so e.g.
    # {'IF': ['ID', 'EX'], 'EX': ['WB']} means IF is fed back from both
    # ID and EX, where ID is given precedence, while WB feeds back to EX.
    def setWbStageOrder(self, order):
        self.wbStageOrder = order

class RegisterBank:
    """Same thing of a register, it also specifies the
    number of registers in the bank. In case register
    fields are specified, they have to be the same for the
    whole bank"""
    def __init__(self, name, numRegs, bitWidth, bitMask = {}):
        self.name = name
        self.bitWidth = bitWidth
        totalBits = 0
        if bitMask:
            for key, value in bitMask.items():
                if value[0] > value[1]:
                    raise Exception('Bit mask start value ' +  str(value[0]) + ' for field ' + key + ' in register bank ' + self.name + ' is larger than end value ' + str(value[1]) + '.')
                if value[1] >= bitWidth:
                    raise Exception('Bit mask size ' + str(value[1]) + ' for field ' + key + ' in register bank ' + self.name + ' is larger than register size ' + str(bitWidth) + '.')
                for key1, value1 in bitMask.items():
                    if key1 != key:
                        if (value1[0] <= value[0] and value1[1] >= value[0]) or (value1[0] <= value[1] and value1[1] >= value[1]):
                            raise Exception('Bit mask of field ' + key + ' in register bank ' + self.name + ' intersects with the mask of field ' + key1 + '.')
        self.bitMask = bitMask
        self.numRegs = numRegs
        self.defValues = [0 for i in range(0, numRegs)]
        self.offset = 0
        self.constValue = {}
        self.delay = {}
        self.wbStageOrder = {}

    def setConst(self, numReg, value):
        if self.delay.has_key(numReg):
            raise Exception('Cannot set register ' + str(numReg) + ' in register bank ' + self.name + ' as constant because it contains a delay assignment.')
        self.constValue[numReg] = value

    def getConstRegs(self):
        constRegs = []
        for key, constVal in self.constValue.items():
            fakeReg = Register(self.name + '[' + str(key) + ']', self.bitWidth, self.bitMask)
            fakeReg.setOffset(self.offset)
            if self.delay.has_key(key):
                fakeReg.setDelay(self.delay[key])
            fakeReg.setConst(constVal)
            constRegs.append(fakeReg)
        return constRegs

    def setDelay(self, numReg, value):
        if self.constValue.has_key(numReg):
            raise Exception('Cannot set a delay assignment for register ' + str(numReg) + ' in register bank ' + self.name + ' because it is set as constant.')
        if value > 0:
            self.delay[numReg] = value

    def setGlobalDelay(self, value):
        if value > 0:
            for i in range(0, self.numRegs):
                if self.constValue.has_key(i):
                    raise Exception('Cannot set a delay assignment for register ' + str(i) + ' in register bank ' + self.name + ' because it is set as constant.')
                self.delay[i] = value

    def getDelayRegs(self):
        delayRegs = []
        for key, delayVal in self.delay.items():
            fakeReg = Register(self.name + '[' + str(key) + ']', self.bitWidth, self.bitMask)
            fakeReg.setOffset(self.offset)
            fakeReg.setDelay(delayVal)
            if self.constValue.has_key(key):
                fakeReg.setConst(self.constValue[key])
            delayRegs.append(fakeReg)
        return delayRegs

    def setDefaultValues(self, values):
        for i in range(0, len(self.defValues)):
            if self.defValues[i]:
                raise Exception('Default value for register ' + str(i) + ' in register bank' + self.name + ' already set.')
            try:
                if values[i] > 0:
                    import math
                    if math.log(values[i], 2) > self.bitWidth:
                        raise Exception('Default value '  + str(values[i]) + ' of register ' + str(i) + ' in register bank ' + self.name + ' requires ' + str(int(math.ceil(math.log(values[i], 2)))) + ' bits, but the register has a width of ' + str(self.bitWidth) + ' bits.')
            except TypeError:
                pass
        if len(values) != self.numRegs:
            raise Exception('The number of initialization values for register bank ' + self.name + ' does not match the number of registers.')
        self.defValues = values

    def setDefaultValue(self, value, position):
        if position < 0 or position >= self.numRegs:
            raise Exception('Cannot set initialization value for register ' + position + ' in register bank ' + self.name + '. Index out of range.')
        if self.defValues[position]:
            raise Exception('Default value for register ' + str(position) + ' in register bank' + self.name + ' already set.')
        try:
            if value > 0:
                import math
                if math.log(value, 2) > self.bitWidth:
                    raise Exception('Default value ' + str(value) + ' of register ' + str(position) + ' in register bank ' + self.name + ' requires ' + str(int(math.ceil(math.log(value, 2)))) + 'bits, but the register has a width of ' + str(self.bitWidth) + ' bits.')
        except TypeError:
            pass
        self.defValues[position] = value

    def setWbStageOrder(self, numReg, order):
        if numReg < 0 or numReg >= self.numRegs:
            raise Exception('Cannot set write-back order for register ' + numReg + ' in register bank ' + self.name + '. Index out of range.')
        self.wbStageOrder[numReg] = order

class AliasRegister:
    """Alias for a register of the processor;
    actually this is a pointer to a register; this pointer
    might be updated during program execution to point to
    the right register; updating it is responsibility of
    the programmer; it is also possible to directly specify
    a target for the alias"""
    # TODO: it might be a good idea to introduce 0 offset aliases: they are aliases
    # for which it is not possible to use any offset by which are much faster than
    # normal aliases at runtime
    # Update: @see runtime/modules/register/register_alias.hpp
    def __init__(self, name, initAlias, offset = 0):
        self.name = name
        # I make sure that there is just one registers specified for
        # the alias
        index = extractRegInterval(initAlias)
        if index:
            if index[0] != index[1]:
                raise Exception('Cannot specify an interval of more than one register in ' + initAlias + ' when aliasing a single register.')
        self.initAlias = initAlias
        self.offset = offset
        self.defValue = None
        self.isFixed = False

    def setDefaultValue(self, value):
        self.defValue = value

    def setFixed(self):
        self.isFixed = True

class AliasRegBank:
    """Alias for a register of the processor;
    actually this is a pointer to a register; this pointer
    might be updated during program execution to point to
    the right register; updating it is responsibility of
    the programmer; it is also possible to directly specify
    a target for the alias: in this case the alias is fixed"""
    def __init__(self, name, numRegs, initAlias):
        self.name = name
        self.numRegs = numRegs
        # Now I have to make sure that the registers specified for the
        # alias have the same lenght of the alias width
        if isinstance(initAlias, str):
            index = extractRegInterval(initAlias)
            # Part of a register bank or alias register bank.
            if index:
                if index[1] - index[0] + 1 != numRegs:
                    raise Exception('Alias register bank ' + str(initAlias) + ' contains ' + str(index[1]-index[0]+1) + ' registers but the aliased register bank contains ' + str(numRegs) + ' registers.')
            # Single register or alias register.
            else:
                if numRegs > 1:
                    raise Exception('Alias register bank ' + str(initAlias) + ' contains one register but the aliased register bank contains ' + str(numRegs) + ' registers.')
        # List of registers, alias registers, register banks or alias register banks.
        else:
            totalRegs = 0
            for i in initAlias:
                index = extractRegInterval(i)
                if index:
                    totalRegs += index[1] - index[0] + 1
                else:
                    totalRegs += 1
            if totalRegs != numRegs:
                raise Exception('Alias register bank ' + str(initAlias) + ' contains ' + str(totalregs) + ' registers but the aliased register bank contains ' + str(numRegs) + ' registers.')
        self.initAlias = initAlias
        self.defValues = [None for i in range(0, numRegs)]
        self.offsets = {}
        self.fixedIndices = []
        self.checkGroup = False

    def setCheckGroup(self):
        self.checkGroup = True

    def setFixed(self, indices):
        for index in indices:
            if index >= self.numRegs:
                raise Exception('Cannot set fixed index ' + str(index) + ' for alias register bank ' + self.name + '. Index out of range.')
        for i in range(0, len(self.fixedIndices) - 1):
            if self.fixedIndices[i] > self.fixedIndices[i + 1]:
                raise Exception('Indices specified for alias register bank ' + self.name + ' are not in ascending order.')
        self.fixedIndices = indices

    def setOffset(self, regId, offset):
        self.offsets[regId] = offset

    def setDefaultValues(self, values):
        if len(values) != self.numRegs:
            raise Exception('The number of initialization values for alias register bank ' + self.name + ' does not match the number of registers.')
        self.defValues = values

    def setDefaultValue(self, value, position):
        if position < 0 or position >= self.numRegs:
            raise Exception('Cannot set initialization value for register ' + position + ' in alias register bank ' + self.name + '. Index out of range.')
        self.defValues[position] = value

class MemoryAlias:
    """Alias for a register through a memory address: by reading and writing to the
    memory address we actually read/write to the register"""
    def __init__(self, address, alias):
        self.address = address
        self.alias = alias


################################################################################
# Interface Models: Interrupts, Ports, Pins and ABI
################################################################################
class Interrupt:
    """Specifies an interrupt port for the processor.
    Note that I will render both systemc and TLM ports as systemc
    signals: there is a check interrupts routine which will be
    called every cycle, in which the user can check the IRQs and
    take the appropriate actions. The signal will be automatically
    raised, lowered etc... depending whether edge triggered, level etc.."""
    def __init__(self, name, portWidth, tlm = True, priority = 0):
        self.name = name
        self.tlm = tlm
        self.portWidth = portWidth
        self.priority = priority
        self.condition = ''
        self.tests = []
        self.operation = {}
        self.variables = []

    def addVariable(self, variable):
        """adds a variable global to the instruction; note that
        variable has to be an instance of cxx_writer.Variable"""
        if isinstance(variable, type(())):
            from isa import resolveBitType
            variable = cxx_writer.Variable(variable[0], resolveBitType(variable[1]))
        for instrVar in self.variables:
            if variable.name == instrVar.name:
                if variable.varType.name != instrVar.varType.name:
                    raise Exception('Variable ' + variable.name + ' of type ' + variable.varType.name + ' already exists in instruction ' + self.name + ' with type ' + instrVar.varType.name + '.')
                else:
                    return
        self.variables.append(variable)

    def setOperation(self, operation, stage):
        self.operation[stage] = operation

    def setCondition(self, condition):
        self.condition = condition

    def addTest(self, inputState, expOut):
        """The test is composed of 2 parts: the status before the
        execution of the interrupt and the status after; note that
        in the status before execution of the interrupt we also have
        to specify the value of the interrupt line"""
        self.tests.append((inputState, expOut))

class Pins:
    """Custom pins; checking them or writing to them is responsibility ofnon
    the programmer. They are identified by (a) name (b) type. They are
    rendered with systemc or TLM ports. The type of the port should also
    be specified, as is the direction. For outgoing TLM ports, the requested
    type and the content of the payload are insignificant and only the
    address is important"""
    def __init__(self, name, portWidth, inbound = False, systemc = False):
        """Note how the type of the must be of class cxx_writer.Type; a
        systemc port using this type as a template will be created"""
        self.name = name
        self.portWidth = portWidth
        self.systemc = systemc
        self.inbound = inbound
        self.operation = None

    def setOperation(self, operation):
        if not self.inbound:
            raise Exception('Cannot specify operation for outbound port ' + self.name + '.')
        self.operation = operation

class ABI:
    """Defines the ABI for the processor: this is necessary both for
    the systemcalls implementation and for the gcc retargeting
    We need to specify in which register the return value is written,
    which are the registers holding the parameters of the call.
    I also have to specify the function prologue and epilogue, where
    the address of the previous execution path is saved, how to branch
    to a fixed address (here we can also reference instructions of
    the ISA).
    The correspondence between the real architectural elements
    and the variables of the simulator must be defined: I have to
    list all the registers as seen (for example) by GDB,
    so (0-n), and then to refer to variables I can either specify
    register name, registry bank with indexes, constants.
    I have to specify the program counter, sp, lr, fp.
    Note that for each of these correspondences I can also specify
    an offset (in the sense that PC can be r15 + 8 for ex).
    """
    def __init__(self, RetVal, args, PC, LR = None, SP = None, FP = None):
        """Regsiter for the return value (either a register or a tuple
        regback, index)"""
        self.RetVal = RetVal
        # Register cprrespondence (offsets should also be specified)
        self.LR = LR
        self.PC = PC
        self.SP = SP
        self.FP = FP
        # A list of the registers for the I argument, II arg etc.
        self.args = []
        if isinstance(args, str):
            index = extractRegInterval(args)
            if index:
                # I'm aliasing part of a register bank or another alias:
                refName = args[:args.find('[')]
                for i in range(index[0], index[1] + 1):
                    self.args.append(refName + '[' + str(i) + ']')
            else:
                # Single register or alias
                self.args.append(args)
        else:
            for j in args:
                index = extractRegInterval(j)
                if index:
                    # I'm aliasing part of a register bank or another alias:
                    refName = j[:j.find('[')]
                    for i in range(index[0], index[1] + 1):
                        self.args.append(refName + '[' + str(i) + ']')
                else:
                    # Single register or alias
                    self.args.append(j)
        # Correspondence between regs as seen by GDB and the architectural
        # variables
        self.regCorrespondence = {}
        # offsets which must be taken into consideration when dealing with the
        # functional model
        self.offset = {}
        # set the names: to the PC register the name PC, etc.
        self.name = {self.PC: 'PC'}
        if self.LR:
            self.name[self.LR] = 'LR'
        if self.SP:
            self.name[self.SP] = 'SP'
        if self.FP:
            self.name[self.FP] = 'FP'
        if self.RetVal:
            self.name[self.RetVal] = 'return_value'
        # Specifies the memories which can be accessed; if more than one memory is specified,
        # we have to associate the address range to each of them
        self.memories = {}
        # C++ Code which has to be executed during emulation of system calls in order to
        # correctly enter a and return from a system call
        self.preCallCode = None
        self.postCallCode = None
        # Registers which have to be updated in order to correctly return from a function call
        self.returnCallReg = None
        # Sequences of instructions which identify a call to a routine and the return from the call
        # to a routine; such sequences are in the form [a, b, (c, d)] which means that, for example,
        # we enter in a new routine when instructions a, b, and c or d are executed in sequence
        self.callInstr = []
        self.returnCallInstr = []
        # Code used to determine the processor ID in a multi-processor environment
        self.procIdCode = None
        # Registers which do not need to be included when saving and restoring the
        # state
        self.stateIgnoreRegs = []

    def addIgnoreStateReg(self, toIgnore):
        self.stateIgnoreRegs.append(toIgnore)

    def processorID(self, procIdCode):
        self.procIdCode = procIdCode

    def setCallInstr(self, instrList):
        self.callInstr = instrList

    def setReturnCallInstr(self, instrList):
        self.returnCallInstr = instrList

    def returnCall(self, regList):
        self.returnCallReg = regList

    def setECallPreCode(self, code):
        self.preCallCode = code

    def setECallPostCode(self, code):
        self.postCallCode = code

    def addVarRegsCorrespondence(self, correspondence):
        for key, value in correspondence.items():
            try:
                value[0]
                value[1]
            except:
                value = (value, value)
            index = extractRegInterval(key)
            if index:
                if index[1] - index[0] != value[1] - value[0]:
                    raise Exception('Cannot set correspondence between ' + str(value) + ' and ' + str(key) + '. Mismatch in number of registers.')
            else:
                if value[1] - value[0]:
                    raise Exception('Cannot set correspondence between ' + str(value) + ' and ' + str(key) + '. Mismatch in number of registers.')
            for i in range(value[0], value[1] + 1):
                if i in self.regCorrespondence.values():
                    raise Exception('Correspondence for register ' + str(i) + ' already exists.')
                if index:
                    self.regCorrespondence[key[:key.find('[')] + '[' + str(index[0] + i - value[0]) + ']'] = i
                else:
                    self.regCorrespondence[key] = value[0]

    def setOffset(self, register, offset):
        if not register in [self.LR, self.PC, self.SP, self.FP, self.RetVal, self.args] + self.regCorrespondence.keys():
            # Ok, the offset register specified does not encode a single register
            # I try to see if it is part of a register bank
            index = extractRegInterval(register)
            if not index:
                raise Exception('Cannot specify offset for register ' + register + '. Register does not exist in the ABI.')
            rangeToCheck = self.corrReg
            argsIndex = extractRegInterval(self.args)
            if argsIndex:
                for i in range(argsIndex[0], argsIndex[1] + 1):
                    rangeToCheck.append(i)
            for i in range(index[0], index[1] + 1):
                if not i in rangeToCheck:
                    raise Exception('Cannot specify offset for register ' + register + '. Register does not exist in the ABI.')
        self.offset[register] = offset

    def addMemory(self, memory, addr = ()):
        if self.memories and not addr:
            raise Exception('More than one memory specified in the ABI. An address range is required for memory ' + memory + '.')
        for name, savedAddr in self.memories.items():
            if not savedAddr:
                raise Exception('More than one memory specified in the ABI. An address range is required for memory ' + name + '.')
            else:
                if (savedAddr[0] <= addr[0] and savedAddr[1] >= addr[0]) or (savedAddr[0] <= addr[1] and savedAddr[1] >= addr[1]):
                    raise Exception('Address range overlap between memories ' + name + ' and ' + memory + '.')
        self.memories[memory] = addr


################################################################################
# Coprocessor Model
################################################################################
class Coprocessor:
    """Specifies the presence of a specific coprocessor; in particular
    it specifies what are the instructions of the ISA (already defined
    instructions) which are coprocessor instructions and for each of
    them it specifies what is the co-processor method which must be called
    note that also the necessary include file must be provided.
    it also specifies the bits which identify if the instruction if
    for this coprocessor or not.
    Alternatively a custom behavior can be provided, which will be
    executed instead of the method call.
    Note that the coprocessor name must be given: the processor will
    contain a variable with this name; this variable will have to
    be initialized to the instance of the coprocessor (by calling a
    special method addCoprocessor ....) Note that the coprocessor
    might need to access the Integer unit to read or set some registers.
    This can either be done by passing a reference to the Ingeter Unit
    registers to the coprocessor at construction or by using the custom
    behavior in each co-processor instruction"""
    # TODO: an accurate interface is also needed. This means that
    # we need to define control signals and pins for the communication
    # between the processor and the coprocessor
    def __init__(self, name, type):
        """Specifies the name of the coprocessor variable in the
        processor. It also specifies its type"""
        self.name = name
        self.type = type
        self.isa = {}

    def addIsaCustom(self, name, code, idBits):
        """Specifies that ISA instruction with name name
        is a co-processor instruction and that
        custom code is provided if the instruction is for
        this coprocessor. idBits specifies what it the
        value of the bits which specify if the instruction
        is for this co-processor or not"""
        if self.isa.has_key(name):
            raise Exception('Instruction ' + name + ' already assigned to coprocessor ' + self.name + '.')
        self.isa[name] = (idBits, code)

    def addIsaCall(self, name, functionName, idBits):
        """Specifies that ISA instruction with name name
        is a co-processor instruction and that
        a function call is provided if the instruction is for
        this coprocessor. idBits specifies what it the
        value of the bits which specify if the instruction
        is for this co-processor or not"""
        if self.isa.has_key(name):
            raise Exception('Instruction ' + name + ' already assigned to coprocessor ' + self.name + '.')
        self.isa[name] = (idBits, functionName)

################################################################################
