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
    """Checks whether the given string specifies an interval within a register
    bank. If so, the interval is returned, if not, None is returned."""
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
    """Defined by (a) name (b) endianess (c) wordsize [byte] (d) bytesize [bit]
    (minimum addressable field) (e) all the contained architectural elements
    (f) the behavior for the received interrupt method. Note that this has to be
    specified only if there are interrupts present. (g) if we are describing a
    processor or a coprocessor (the difference is that a coprocessor is not
    active; instructions are passed to it and not actively fetched by it.)
    Three special operations are defined, (1) begin, (2) end and (3) reset,
    used at the begining of the simulation (or after a reset), at the end of
    the simulation and to reset the processor.
    The systemc parameter in the constructor is relevant only for functional
    processors with a local memory and specifies whether SystemC will be used
    for keeping time or not. In case TLM ports are used the systemc parameter is
    not taken into account."""

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
        self.isBigEndian = None
        self.wordSize = None
        self.byteSize = None
        self.bitSizes = None
        self.cacheLimit = cacheLimit
        self.preProcMacros = []
        self.defines = []
        self.features = {}
        self.parameters = []
        self.systemc = systemc

        self.version = version
        self.license = 'gpl'
        self.license_text = licenses.create_gpl_license(self.name)
        self.developer_name = ''
        self.developer_email = ''
        self.banner = ''

        # Pre-allocating instructions does not give any speedup.
        #self.alloc_buffer_size = alloc_buffer_size
        self.isa = None
        self.invalid_instr = None

        self.externalClock = externalClock
        self.coprocessor = coprocessor
        self.pipes = []
        self.instructionCache = instructionCache
        self.fastFetch = fastFetch

        self.regs = []
        self.regBanks = []
        #self.regOrder = {}
        self.aliasRegs = []
        self.aliasRegBanks = []

        self.fetchReg = None
        self.fetchMem = None
        self.memAlias = []
        self.memoryParams = {}
        self.memories = {}
        self.memoryifs = []
        self.tlmPorts = []
        self.tlmFakeMemProperties = ()
        self.irqs = []
        self.pins = []

        self.abi = None
        self.startup = None
        self.shutdown = None
        self.reset = None

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

    def addFeatures(self, features):
        for category, values in features.items():
            if not isinstance(values, list):
                raise Exception('Invalid values ' + values + ' for feature ' + category + ', expected list of available values + active values.')
            if not isinstance(values[0], list):
                raise Exception('Invalid values ' + values[0] + ' for feature ' + category + ', expected list.')
            if values[1] != None and not values[1] in values[0]:
                raise Exception('Value ' + values[1] + ' for feature ' + category + ', does not match any of the available values: ' + values[0] + '.')
        self.features = features


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

    def addRegister(self, reg, features = []):
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
        if not features:
            self.regs.append(reg)
        else:
            for feature in features:
                if feature in [ self.features[key][1] for key in self.features.keys() ]:
                    self.regs.append(reg)
                    break

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
        """Specifies that a processor register will be used as the fetch
        register."""
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

    def addMemoryParam(self, name, paramType, value, default):
        for paramName in self.memoryParams.keys():
            if name == paramName:
                raise Exception('Memory parameter ' + name + ' already exists.')
            if not isinstance(paramType, cxx_writer.Type):
                raise Exception('Memory parameter ' + name + ' should be an instance of cxx_writer.Type.')
        self.memoryParams[name] = (paramType, value, default)

    def addMemory(self, name, mem_size, debug = False, program_counter = '', fetch = True):
        """Defines a processor class member of type LocalMemory that implements
        MemoryInterface. Instruction fetch can optionally be performed from this
        memory. Multiple memories can be instantiated."""
        for memName, memAttrs in self.memories.items():
            if name == memName:
                raise Exception('Memory ' + name + ' already exists.')
            if program_counter and memAttrs[2]:
                raise Exception('Cannot specify program counter for memory ' + name + '. Program counter is already specified for memory ' + memName + '.')
        if fetch:
            if self.fetchMem:
                raise Exception('Cannot specify internal memory ' + name + ' for fetching instructions. ' + self.fetchMem[0] + ' is already set.')
            # Instruction fetch is performed from the local memory.
            self.fetchMem = (name, cxx_writer.Type('LocalMemory', '#include \"memory.hpp\"').makeRef())
        self.memories[name] = (mem_size, debug, program_counter)

    def addMemoryInterface(self, name, fetch = False):
        """Defines a processor class member of type MemoryInterface& as well as
        a constructor parameter that receives the MemoryInterface object.
        Instruction fetch can optionally be performed from this memory. Multiple
        memory interfaces can be passed."""
        for memName in self.memoryifs:
            if name == memName:
                raise Exception('Memory interface ' + name + ' already exists.')
        if fetch:
            if self.fetchMem:
                raise Exception('Cannot specify memory interface ' + name + ' for fetching instructions. ' + self.fetchMem[0] + ' is already set.')
            # Instruction fetch is performed from the local memory.
            self.fetchMem = (name, cxx_writer.Type('MemoryInterface', '#include \"memory.hpp\"').makeRef())
        self.memoryifs.append(name)

    def setTLMMem(self, mem_size, memLatency, sparse = False):
        """Instantiates a fake memory to be connected to the processor TLM port
        in case the port is declared as the instruction fetch port. The latency
        is expressed in us."""
        self.tlmFakeMemProperties = (mem_size, memLatency, sparse)

    # ..........................................................................
    # Interface Configuration

    def addTLMPort(self, name, fetch = False):
        """Defines a processor class member that implements MemoryInterface and
        contains a simple_initiator_socket<> socket. Instruction fetch can
        optionally be performed via the socket."""
        if not self.systemc:
            raise Exception('Cannot use TLM ports if SystemC is not enabled.')
        for portName in self.tlmPorts:
            if name == portName:
                raise Exception('Port ' + name + ' already exists.')
        if fetch:
            if self.fetchMem:
                raise Exception('Cannot specify port ' + name + ' as a fetch port. ' + portName + ' is already set.')
            # Instruction fetch is performed via the TLM port.
            self.fetchMem = (name, cxx_writer.Type('TLMMemory', '#include \"externalPorts.hpp\"').makeRef())
        self.tlmPorts.append(name)

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

    def setABI(self, abi):
        if self.coprocessor:
            print ('Warning: No need to set an ABI for coprocessor ' + self.name + '.')
        self.abi = abi

    # ..........................................................................
    # Behavior Configuration

    def addDefine(self, define, includes = []):
        self.defines.append(cxx_writer.Define(define.strip() + '\n', includes = includes))

    def setPreProcMacro(self, wafOption, macro):
        self.preProcMacros.append( (wafOption, macro) )

    def addParameter(self, name, varType, default):
        for i in self.parameters:
            if i.name == name:
                raise Exception('Parameter ' + name + ' already exists in processor ' + self.name + '.')
        parameter = cxx_writer.Parameter(name, type = varType, initValue = default)
        self.parameters.append(parameter)

    def setBeginOperation(self, code):
        """Instance of cxx_writer.CustomCode containing behavior to be run at
        the begining of the simulation."""
        self.startup = code

    def setResetOperation(self, code):
        """Instance of cxx_writer.CustomCode containig behavior to be run at
        reset."""
        self.reset = code

    def setEndOperation(self, code):
        """Instance of cxx_writer.CustomCode containing behavior to be run at
        the end of the simulation."""
        self.shutdown = code

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
        decodeStage = -1
        regsStage = -1
        wbStage = -1
        for pipeStage in self.pipes:
            if pipeStage.fetchStage:
                if fetchStage:
                    raise Exception('Cannot have more than one pipeline stage for fetching instructions (<pipe>.setFetchStage()).')
                else:
                    fetchStage = True
            if pipeStage.decodeStage:
                if decodeStage != -1:
                    raise Exception('Cannot have more than one pipeline stage for decoding registers (<pipe>.setDecodeStage()).')
                else:
                    decodeStage = self.pipes.index(pipeStage)
            if pipeStage.regsStage:
                if regsStage != -1:
                    raise Exception('Cannot have more than one pipeline stage for reading registers (<pipe>.setRegsStage()).')
                else:
                    regsStage = self.pipes.index(pipeStage)
            if pipeStage.wbStage:
                if wbStage != -1:
                    raise Exception('Cannot have more than one pipeline stage for writing registers (<pipe>.setWbStage()).')
                else:
                    wbStage = self.pipes.index(pipeStage)
            for fromStage in pipeStage.wbStageOrder:
                if fromStage not in self.pipes + [pipe.name for pipe in self.pipes]:
                    raise Exception('Cannot set write-back order for pipe stage ' + pipeStage.name + '. Pipeline stage ' + fromStage + ' does not exist.')
        if (len(self.pipes) > 1):
            if not fetchStage:
                raise Exception('Please specify a fetch stage using <pipe>.setFetchStage().')
            if decodeStage == -1:
                raise Exception('Please specify a decode stage using <pipe>.setDecodeStage().')
            if regsStage == -1:
                raise Exception('Please specify a read-register stage using <pipe>.setRegsStage().')
            if wbStage == -1:
                raise Exception('Please specify a write-back stage using <pipe>.setWbStage().')
            if (decodeStage < fetchStage):
                raise Exception('The decode stage cannot occur before the fetch stage.')
            if (regsStage < decodeStage):
                raise Exception('The read-register stage cannot occur before the decode stage.')
            if (wbStage <= regsStage):
                raise Exception('The write-back stage must occur after the read-register stage.')
        else:
            self.pipes[0].fetchStage = True
            self.pipes[0].decodeStage = True
            self.pipes[0].regsStage = True
            self.pipes[0].wbStage = True

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
        """Checks that the declared aliases actually refer to existing
        registers."""
        for alias in self.aliasRegBanks:
            # Alias bank refers to one register, alias, register bank or alias
            # register bank.
            if isinstance(alias.initAlias, str):
                index = extractRegInterval(alias.initAlias)
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
            # Alias bank refers to a combination of registers, aliases, register
            # banks or alias register banks.
            else:
                totalRegs = 0
                for i in range(0, len(alias.initAlias)):
                    index = extractRegInterval(alias.initAlias[i])
                    # Alias bank refers to (a subset of) a register bank or
                    # alias register bank.
                    if index:
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
                    # Alias bank refers to a single register or alias.
                    else:
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
            # Alias refers to one register inside a register bank or alias
            # register bank.
            if index:
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
            # Alias refers to a single register or alias.
            else:
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
        if not self.fetchReg:
            raise Exception('Please specify a fetch register using setFetchRegister() (usually the PC).')
        # Either a local memory, a TLM port or an external MemoryInterface have
        # to be specified for fetching instructions.
        if not self.fetchMem:
            raise Exception('Please specify either an internal memory (addMemory()), a TLM memory port (addTLMPort()) or an external memory interface (addMemoryInterface()) for fetching instructions.')
        for memAliasReg in self.memAlias:
            index = extractRegInterval(memAliasReg.alias)
            # Alias refers to one register inside a register bank or alias
            # register bank.
            if index:
                regName = memAliasReg.alias[:memAliasReg.alias.find('[')]
                if self.isRegExisting(regName, index) is None:
                    raise Exception('Cannot set memory alias for register ' + memAliasReg.alias + ' at address ' + memAliasReg.address + '. Register does not exist.')
            # Alias refers to a single register or alias.
            else:
                if self.isRegExisting(memAliasReg.alias) is None:
                    raise Exception('Cannot set memory alias for register ' + memAliasReg.alias + ' at address ' + memAliasReg.address + '. Register does not exist.')
        for memName, memAttrs in self.memories.items():
            if memAttrs[2]:
                index = extractRegInterval(memAttrs[2])
                # Alias refers to one register inside a register bank or alias
                # register bank.
                if index:
                    regName = memAttrs[2][:memAttrs[2].find('[')]
                    if self.isRegExisting(regName, index) is None:
                        raise Exception('Cannot set register ' + memAttrs[2] + ' as the program counter. Register does not exist.')
                # Alias refers to a single register or alias.
                else:
                    if self.isRegExisting(memAttrs[2]) is None:
                        raise Exception('Cannot set register ' + memAttrs[2] + ' as the program counter. Register does not exist.')

    def checkISARegs(self):
        """Checks that the registers declared in the instruction encoding and
        the ISA really exist."""
        architecturalNames = [archElem.name for archElem in self.regs + self.regBanks + self.aliasRegs + self.aliasRegBanks]
        for name, instruction in self.isa.instructions.items():
            # Inside each instruction I have to check for registers defined in
            # the machine code (bitCorrespondence), the correspondence declared
            # inside the instruction itself (bitCorrespondence), the input and
            # output special registers (specialInRegs, specialOutRegs).
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
                # Alias refers to one register inside a register bank or alias
                # register bank.
                if index:
                    refName = regName[:regName.find('[')]
                    if self.isRegExisting(refName, index) is None:
                        raise Exception('Register bank ' + regName + ' referenced as spcial register in instruction ' + name + ' does not exist.')
                # Alias refers to a single register or alias.
                else:
                    if self.isRegExisting(regName) is None:
                        raise Exception('Register ' + regName + ' referenced as spcial register in instruction ' + name + ' does not exist.')
            pipeStageName = [i.name for i in self.pipes] + ['default']
            beforeCheck = []
            regsStageName = 'default'
            wbStageName = 'default'
            for i in self.pipes:
                if i.regsStage:
                    regsStageName = i.name
                elif i.wbStage:
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
                    stage = regsStageName
                if not stage in pipeStageName:
                    raise Exception('Stage ' + stage + ' specified for special register of instruction ' + name + ' does not exist.')
                newInRegs[stage] = regs
            instruction.specialInRegs = newInRegs

    def checkTestRegs(self):
        """Checks that the registers specified in the tests exist."""
        outPinPorts = []
        for pinPort in self.pins:
            if not pinPort.inbound:
                outPinPorts.append(pinPort.name)

        for instr in self.isa.instructions.values():
            for test in instr.tests:
                # Check whether the instruction fields exist.
                for name, elemValue in test[0].items():
                    if not instr.machineCode.bitLen.has_key(name):
                        raise Exception('Field ' + name + ' in test of instruction ' + instr.name + ' does not exist in the machine code.')
                # Check whether the global resources exist.
                resources = self.memories.keys() + self.memoryifs + self.tlmPorts
                for resource, value in test[1].items():
                    brackIndex = resource.find('[')
                    if not (brackIndex > 0 and resource[:brackIndex] in resources):
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
                    if not (brackIndex > 0 and (resource[:brackIndex] in resources or resource[:brackIndex] in outPinPorts)):
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
        """Checks that the stages of the IRQ operations exist."""
        stageNames = [i.name for i in self.pipes]
        for irq in self.irqs:
            for stage in irq.operation.keys():
                if not stage in stageNames:
                    raise Exception('Pipeline stage ' + stage + ' specified for interrupt ' + irq.name + ' does not exist.')

    def checkABI(self):
        """Checks that the registers specified for the ABI interface exist."""
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

        for i in toCheck:
            index = extractRegInterval(i)
            # ABI register refers to one register inside a register bank or an
            # alias register bank.
            if index:
                refName = i[:i.find('[')]
                if self.isRegExisting(refName, index) is None:
                    raise Exception('Register bank ' + i + ' used in the ABI does not exist.')
            # ABI register refers to a single register or alias.
            else:
                if self.isRegExisting(i) is None:
                    raise Exception('Register ' + i + ' used in the ABI does not exist.')
        # Warning in case of missing details.
        if not self.abi.returnCallInstr or not self.abi.callInstr:
            print('Warning: "returnCallInstr" or "callInstr" not specified in the ABI. The profiler may give incorrect results.')
        ################# TODO: check also the memories #######################

    ## @} Checks
    #---------------------------------------------------------------------------
    ## @name Model Generation
    #  @{

    def write(self, folder = '.', models = validModels, namespace = '', dumpDecoderName = '', trace = False, combinedTrace = False, forceDecoderCreation = False, tests = True, memPenaltyFactor = 4):
        """This method does two things: First, it performs all possible checks
        to ensure that the processor description is coherent. Second, it calls
        the write method of the processor components (registers, instructions,
        etc.) to create the simulator code."""

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
        #if ('funcAT' in models or 'accAT' in models or 'accLT' in models) and not self.tlmPorts:
        #    raise Exception('TLM ports are required for all models other than funcLT. Please specify at least one TLM port.')
        self.checkIRQPorts()
        if self.abi:
            self.checkABI()

        # ..........................................................................
        # Model Generation

        # Decoder Model Generation
        from isa import resolveBitType
        import decoder, os
        import cxx_writer
        # Variables used for keeping track of memory field sizes.
        self.bitSizes = [resolveBitType('BIT<' + str(self.wordSize*self.byteSize*2) + '>'),
                        resolveBitType('BIT<' + str(self.wordSize*self.byteSize) + '>'),
                        resolveBitType('BIT<' + str(self.wordSize*self.byteSize/2) + '>'),
                        resolveBitType('BIT<' + str(self.byteSize) + '>')]

        cxx_writer.FileDumper.license = self.license
        cxx_writer.FileDumper.license_text = self.license_text
        cxx_writer.FileDumper.developer_name = self.developer_name
        cxx_writer.FileDumper.developer_email = self.developer_email
        cxx_writer.FileDumper.banner = self.banner

        # Check if the decoder signature has changed. In case not, the decoder
        # is is loaded from file, otherwise it is re-created.
        instructionSignature = self.isa.getInstructionSig()
        if not forceDecoderCreation:
            if os.path.exists(os.path.join(os.path.expanduser(os.path.expandvars(folder)), '.decoderSig')) and os.path.exists(os.path.join(os.path.expanduser(os.path.expandvars(folder)), '.decoderDump.pickle')):
                # Compare the saved signature with the signature of the current
                # instruction set.
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

        for model, modelFolder in models.items():
            # Add the define code, defining the type of the current model. The
            # define code has to be added to each generated header file.
            defString = '#define ' + model[:-2].upper() + '_MODEL\n'
            defString += '#define ' + model[-2:].upper() + '_IF\n'
            defCode = cxx_writer.Define(defString)
            cxx_writer.FileDumper.def_prefix = 'CORE_' + self.name.upper() + '_' +  model[:-2].upper() + '_' + model[-2:].upper() + '_'

            # Set the processor class name. Note that even if each model has a
            # separate namespace, some buggy dynamic linkers complain, so we
            # must also use separate names for the processor class.
            procWriter.processor_name = 'Core' + self.name + model[0].upper() + model[1:]

            print ('\t\tCreating ' + model + ' implementation...')
            if not model in validModels:
                raise Exception('Invalid model ' + model + '.')
            if not namespace:
                namespace = 'core_' + self.name.lower() + '_' + model.lower()
            namespaceUse = cxx_writer.UseNamespace(namespace)
            namespaceTrapUse = cxx_writer.UseNamespace('trap')

            curFolder = cxx_writer.Folder(os.path.join(folder, modelFolder))
            mainFolder.addSubFolder(curFolder)

            # Decoder Model Generation
            decoderClasses = dec.getCPPDecoder(self.bitSizes[1], self.instructionCache, namespace)
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

            pinClasses = []
            if self.pins:
                pinClasses = portsWriter.getCPPPinPorts(self, namespace)
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

                mainTestHeadFile = cxx_writer.FileDumper('main.hpp', True)
                mainTestImplFile = cxx_writer.FileDumper('main.cpp', False)
                mainTestImplFile.addInclude('#include \"main.hpp\"')
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

                mainTests = procWriter.getCPPTestMain(self)
                mainTestImplFile.addMember(mainTests)
                mainTestHeadFile.addMember(mainTests)

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
    """Identified by (a) name (b) whether it is a special stage (fetch, decode,
    regs, wb)."""
    def __init__(self, name):
        self.name = name
        self.fetchStage = False
        self.decodeStage = False
        self.regsStage = False
        self.wbStage = False
        self.checkUnknown = False
        self.wbStageOrder = []
        self.operation = ''

    def setFetchStage(self):
        """Exactly one pipeline stage needs to be set as the fetch stage. The
        pipeline stage behavior will then contain the code for fetching
        instructions."""
        self.fetchStage = True

    def setDecodeStage(self):
        """Exactly one pipeline stage needs to be set as the decode stage. As
        soon as the instruction register ids are decoded, the input registers
        can be checked for possible data hazards, and the output registers can
        be locked."""
        self.decodeStage = True

    def setRegsStage(self):
        """Exactly one pipeline stage needs to be set as the register stage.
        Unless otherwise specified, this is the point where registers are read.
        This is important for correct hazard detection and register locking
        logic."""
        self.regsStage = True

    def setWbStage(self):
        """Exactly one pipeline stage needs to be set as the write-back stage.
        Unless otherwise specified, this is the point where registers are
        written. This is important for correct hazard detection, register
        locking and value propagation logic."""
        self.wbStage = True

    # This is a list of forwarding paths, so e.g. ['ID', 'EX'], means this stage
    # is fed back from both ID and EX, where ID is given precedence.
    def setWbStageOrder(self, order):
        self.wbStageOrder = order

    def setCheckUnknownInstr(self):
        self.checkUnknown = True

    def setOperation(self, code):
        self.operation = code

################################################################################
# Storage Models: Registers, Aliases, Register Banks and Memory
################################################################################
class Register:
    """Register of a processor. It is identified by (a) the width in bits, (b)
    the name. Register fields can be accessed by name instead of having to use
    bit masks."""
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
        self.isGlobal = False

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
        # TODO: Eliminate this restriction.
        if self.bitMask:
            raise Exception('Cannot set offset for register ' + self.name + ' because it uses a bit mask.')
        self.offset = value

    # This is a map of forwarding paths stage -> stages, so e.g.
    # {'IF': ['ID', 'EX'], 'EX': ['WB']} means IF is fed back from both
    # ID and EX, where ID is given precedence, while WB feeds back to EX.
    def setWbStageOrder(self, order):
        if self.isGlobal:
            raise Exception('Cannot set write-back order for register ' + self.name + '. Register is set as global and is therefore not pipelined.')
        self.wbStageOrder = order

    # The default behavior in the accurate model is to create one latch per
    # pipeline stage per register (register renaming). This flag prevents this
    # behavior and creates only one copy of the register, where all instructions
    # in the pipeline see the same value. This is often the case for Program
    # Status Registers for example. There is no read/write race, in that case:
    # Since the pipeline stages execute in reverse order, the later stages
    # will write first, which enables earlier stages to read the most recent
    # value.
    def setGlobal(self):
        if self.wbStageOrder:
            raise Exception('Cannot set register ' + self.name + ' as global (non-pipelined). Register has a pipeline write-back order.')
        self.isGlobal = True

class RegisterBank:
    """Registers inside a bank share the same bitmasks/field names and offsets.
    Reset values, delays and pipeline bypasses can be specified for individual
    registers."""
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
        self.isGlobal = False

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
        if self.isGlobal:
            raise Exception('Cannot set write-back order for register ' + numReg + ' in register bank ' + self.name + '. Register is set as global and is therefore not pipelined.')
        self.wbStageOrder[numReg] = order

    def setGlobal(self):
        if self.wbStageOrder:
            raise Exception('Cannot set register bank ' + self.name + ' as global (non-pipelined). Register bank has a pipeline write-back order.')
        self.isGlobal = True

class AliasRegister:
    """Alias of a processor register. It is basically a class that contains a
    pointer to a register and delegates all operations there. This is useful for
    processors where the user sees only a subset (logical registers) of the
    physical registers at a given time. An offset added to the register value
    can be specified. The target register can by changed during runtime."""
    def __init__(self, name, initAlias, offset = 0):
        self.name = name
        # Check that the alias points to only one register.
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
    """Alias bank pointing to a subset of the processor registers. It is
    basically a container of aliases, where each points to a register and
    delegates all operations there. This is useful for processors where the user
    sees only a subset (logical registers) of the physical registers at a given
    time. An offset added to the register value can be specified. The target
    register can by changed during runtime."""
    def __init__(self, name, numRegs, initAlias):
        self.name = name
        self.numRegs = numRegs
        # Check that the width of the registers and aliases match.
        # Alias bank refers to one register or register bank.
        if isinstance(initAlias, str):
            index = extractRegInterval(initAlias)
            # Alias bank refers to (a subset of) a register bank or alias
            # register bank.
            if index:
                if index[1] - index[0] + 1 != numRegs:
                    raise Exception('Alias register bank ' + str(initAlias) + ' contains ' + str(index[1]-index[0]+1) + ' registers but the aliased register bank contains ' + str(numRegs) + ' registers.')
            # Alias bank refers to a single register or alias.
            else:
                if numRegs > 1:
                    raise Exception('Alias register bank ' + str(initAlias) + ' contains one register but the aliased register bank contains ' + str(numRegs) + ' registers.')
        # Alias bank refers to a combination of registers, aliases, register
        # banks or alias register banks.
        else:
            totalRegs = 0
            for i in initAlias:
                index = extractRegInterval(i)
                # Alias bank refers to (a subset of) a register bank or alias
                # register bank.
                if index:
                    totalRegs += index[1] - index[0] + 1
                # Alias bank refers to a single register or alias.
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
    """Alias of a processor register through a memory address: By reading and
    writing to the memory address we actually read/write the register."""
    def __init__(self, address, alias):
        self.address = address
        self.alias = alias

################################################################################
# Interface Models: Ports, Interrupts, Pins and ABI
################################################################################
class Interrupt:
    """Specifies an interrupt port for the processor.
    Note that an interrupt checking routine is created for both SystemC and TLM
    signals. It is called every cycle and runs user-specified code for checking
    and processing the interrupts. The signal will be automatically raised or
    lowered depending on whether it is edge- or level-triggered."""
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
        """Adds the given cxx_writer.Variable as a member to the interrupt
        instruction."""
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
        """The test is composed of two parts: The status before the triggering
        of the interrupt and the status after. Note that the status before
        the interrupt should include the value of the interrupt line."""
        self.tests.append((inputState, expOut))

class Pins:
    """Custom pins identified by (a) name (b) type. Checking or writing pins is
    the responsibility of the programmer. They are implemented as SystemC or TLM
    ports. For outgoing TLM ports, the requested type and the content of the
    payload are insignificant and only the address is considered."""
    def __init__(self, name, portWidth, inbound = False, systemc = False):
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
    """Defines the ABI for the processor. This is necessary both for the
    implementation of system calls as well as gcc retargeting. The ABI supplies
    information regarding:
    - Names of registers holding function parameters or return values.
    - Prologues and epilogues to function calls, such as where the address of
      the previous execution path is saved, how to branch to a fixed address,
      etc. Here we can also reference instructions or methods from the ISA.
    - The correspondence between the real architectural elements and the
      variables of the simulator: All registers as seen e.g. by GDB are listed.
    """
    def __init__(self, RetVal, args, PC, LR = None, SP = None, FP = None):
        """Register holding the return value (either a register or a (regBank,
        index) tuple."""
        # Special registers, possibly including offsets.
        self.PC = PC
        self.name = {self.PC: 'PC'}
        self.LR = LR
        if self.LR:
            self.name[self.LR] = 'LR'
        self.SP = SP
        if self.SP:
            self.name[self.SP] = 'SP'
        self.FP = FP
        if self.FP:
            self.name[self.FP] = 'FP'
        self.RetVal = RetVal
        if self.RetVal:
            self.name[self.RetVal] = 'return_value'

        # Registers holding function arguments.
        self.args = []
        # One register holds function arguments.
        if isinstance(args, str):
            index = extractRegInterval(args)
            if index:
                # Register refers to one register inside a register bank or
                # alias register bank.
                refName = args[:args.find('[')]
                for i in range(index[0], index[1] + 1):
                    self.args.append(refName + '[' + str(i) + ']')
            else:
                # Register refers to a single register or alias.
                self.args.append(args)
        # Multiple registers hold function arguments.
        else:
            for j in args:
                index = extractRegInterval(j)
                if index:
                    # Register refers to one register inside a register bank or
                    # alias register bank.
                    refName = j[:j.find('[')]
                    for i in range(index[0], index[1] + 1):
                        self.args.append(refName + '[' + str(i) + ']')
                else:
                    # Register refers to a single register or alias.
                    self.args.append(j)

        # Correspondence between regs as seen by GDB and the architectural
        # variables.
        self.regCorrespondence = {}
        # Offsets are only relevant for the functional model.
        self.offset = {}
        # Specifies the memories which can be accessed. If more than one memory
        # is specified, each has to specify an address range.
        self.memories = {}
        # C++ Code which is executed during emulation of system calls in order
        # to correctly enter and return from a system call.
        self.preCallCode = None
        self.postCallCode = None
        # Registers which have to be updated in order to correctly return from a
        # function call.
        self.returnCallReg = None
        # Sequences of instructions which identify a call to a routine and the
        # return from the call. Such sequences are in the form [a, b, (c, d)],
        # where we enter a new routine whenever instructions a, b, and c or d
        # are executed in sequence.
        self.callInstr = []
        self.returnCallInstr = []
        # Code used to determine the processor ID in a multi-processor
        # environment.
        self.procIdCode = None
        # Registers which do not need to be included when saving and restoring
        # the state.
        self.stateIgnoreRegs = []

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

    def addIgnoreStateReg(self, toIgnore):
        self.stateIgnoreRegs.append(toIgnore)

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

    def processorID(self, procIdCode):
        self.procIdCode = procIdCode

################################################################################
# Coprocessor Model
################################################################################
class Coprocessor:
    """A coprocessor needs to know the subset of instructions that are
    coprocessor instructions. Each instruction is assigned a coprocessor method
    which should be called. Alternatively a custom behavior can be provided,
    which is to be executed instead of the method call.
    The processor should contain a coprocessor variable holding the name of the
    coprocessor, as given here. Depending on the architecture, the coprocessor
    communicates with the processor either via special instructions that
    transfer data from each set of registers, or by passing a reference to the
    architectural elements at construction."""
    # TODO: The accurate interface is missing: We need to define control signals
    # and pins for the communication between the processor and the coprocessor.
    def __init__(self, name, type):
        """Specifies the name and type of the coprocessor variable in the
        processor."""
        self.name = name
        self.type = type
        self.isa = {}

    def addIsaCustom(self, name, code, idBits):
        """Specifies that ISA instruction <name> is a coprocessor instruction
        and that custom code is provided for the instruction behavior. idBits
        specifies a variable field in the instruction that indicates whether
        the instruction is addressed to this coprocessor or not."""
        if self.isa.has_key(name):
            raise Exception('Instruction ' + name + ' already assigned to coprocessor ' + self.name + '.')
        self.isa[name] = (idBits, code)

    def addIsaCall(self, name, functionName, idBits):
        """Specifies that ISA instruction <name> is a coprocessor instruction
        and that a method is provided for the instruction behavior. idBits
        specifies a variable field in the instruction that indicates whether
        the instruction is addressed to this coprocessor or not."""
        if self.isa.has_key(name):
            raise Exception('Instruction ' + name + ' already assigned to coprocessor ' + self.name + '.')
        self.isa[name] = (idBits, functionName)

################################################################################
