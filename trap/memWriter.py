################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     memWriter.py
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
readMethodNames = ['read_dword', 'read_word', 'read_half', 'read_byte', 'read_instr']
readMethodNames_dbg = ['read_dword_dbg', 'read_word_dbg', 'read_half_dbg', 'read_byte_dbg']
writeMethodNames = ['write_dword', 'write_word', 'write_half', 'write_byte']
writeMethodNames_dbg = ['write_dword_dbg', 'write_word_dbg', 'write_half_dbg', 'write_byte_dbg']
genericMethodNames = ['lock', 'unlock']

methodTypes = None
methodTypeLen = None

def addMemoryMethods(self, localMemoryMembers, methodsCode, methodsAttrs):
    archDWordType = self.bitSizes[0]
    archWordType = self.bitSizes[1]
    archHWordType = self.bitSizes[2]
    archByteType = self.bitSizes[3]

    addressParam = cxx_writer.Parameter('address', archWordType.makeRef().makeConst())
    userParams = [cxx_writer.Parameter(paramName, paramAttr[0], initValue = paramAttr[2]) for paramName, paramAttr in self.memoryParams.items()]

    for methName in readMethodNames + readMethodNames_dbg:
        if methName in methodsCode.keys() and methName in methodsAttrs.keys():
            readMethod = cxx_writer.Method(methName, methodsCode[methName], methodTypes[methName], 'public', [addressParam] + userParams, inline = 'inline' in methodsAttrs[methName], pure = 'pure' in methodsAttrs[methName], virtual = 'virtual'  in methodsAttrs[methName], const = len(self.tlmPorts) == 0, noException = 'noexc'  in methodsAttrs[methName])
            localMemoryMembers.append(readMethod)

    for methName in writeMethodNames + writeMethodNames_dbg:
        if methName in methodsCode.keys() and methName in methodsAttrs.keys():
            datumParam = cxx_writer.Parameter('datum', methodTypes[methName])
            writeMethod = cxx_writer.Method(methName, methodsCode[methName], cxx_writer.voidType, 'public', [addressParam, datumParam] + userParams, inline = 'inline' in methodsAttrs[methName], pure = 'pure' in methodsAttrs[methName], virtual = 'virtual'  in methodsAttrs[methName], noException = 'noexc'  in methodsAttrs[methName])
            localMemoryMembers.append(writeMethod)

    for methName in genericMethodNames:
        if methName in methodsCode.keys() and methName in methodsAttrs.keys():
            lockMethod = cxx_writer.Method(methName, methodsCode[methName], cxx_writer.voidType, 'public', inline = 'inline' in methodsAttrs[methName], pure = 'pure' in methodsAttrs[methName], virtual = 'virtual'  in methodsAttrs[methName], noException = 'noexc'  in methodsAttrs[methName])
            localMemoryMembers.append(lockMethod)

################################################################################
# Memory Classes
################################################################################
def getCPPMemoryIf(self, model, namespace):
    """Creates the necessary structures for communicating with the memory: An
    array in case of an internal memory or a TLM port for TLM memories."""

    archDWordType = self.bitSizes[0]
    archWordType = self.bitSizes[1]
    archHWordType = self.bitSizes[2]
    archByteType = self.bitSizes[3]

    global methodTypes, methodTypeLen
    methodTypes = {'read_dword': archDWordType, 'read_word': archWordType, 'read_half': archHWordType, 'read_byte': archByteType, 'read_instr': archWordType,
                'read_dword_dbg': archDWordType, 'read_word_dbg': archWordType, 'read_half_dbg': archHWordType, 'read_byte_dbg': archByteType,
                'write_dword': archDWordType, 'write_word': archWordType, 'write_half': archHWordType, 'write_byte': archByteType,
                'write_dword_dbg': archDWordType, 'write_word_dbg': archWordType, 'write_half_dbg': archHWordType, 'write_byte_dbg': archByteType}
    methodTypeLen = {'read_dword': self.wordSize*2, 'read_word': self.wordSize, 'read_half': self.wordSize/2, 'read_byte': 1, 'read_instr': self.wordSize,
                    'read_dword_dbg': self.wordSize*2, 'read_word_dbg': self.wordSize, 'read_half_dbg': self.wordSize/2, 'read_byte_dbg': 1,
                    'write_dword': self.wordSize*2, 'write_word': self.wordSize, 'write_half': self.wordSize/2, 'write_byte': 1,
                    'write_dword_dbg': self.wordSize*2, 'write_word_dbg': self.wordSize, 'write_half_dbg': self.wordSize/2, 'write_byte_dbg': 1}

    #---------------------------------------------------------------------------
    ## @name Memory Interface Class
    #  @{

    memoryClasses = []
    memoryIfMembers = []
    emptyBody = cxx_writer.Code('')

    # Methods: read(), write()
    methodsCode = {}
    methodsAttrs = {}
    for methName in readMethodNames + writeMethodNames:
        methodsAttrs[methName] = ['pure', 'noexc']
        methodsCode[methName] = emptyBody
    for methName in readMethodNames_dbg:
        methodsAttrs[methName] = ['virtual']
        methodsCode[methName] = cxx_writer.Code('return this->' + methName[:-4] + '(address);')
    for methName in writeMethodNames_dbg:
        methodsAttrs[methName] = ['virtual']
        methodsCode[methName] = cxx_writer.Code('this->' + methName[:-4] + '(address, datum);')
    for methName in genericMethodNames:
        methodsAttrs[methName] = ['pure']
        methodsCode[methName] = emptyBody
    addMemoryMethods(self, memoryIfMembers, methodsCode, methodsAttrs)

    # Methods: swap_endianess()
    for cur_type in [archWordType, archHWordType]:
        swapEndianessCode = str(archByteType) + """ helper_byte = 0;
        for (unsigned i = 0; i < sizeof(""" + str(cur_type) + """)/2; i++) {
            helper_byte = ((""" + str(archByteType) + """ *)&datum)[i];
            ((""" + str(archByteType) + """ *)&datum)[i] = ((""" + str(archByteType) + """ *)&datum)[sizeof(""" + str(cur_type) + """) -1 -i];
            ((""" + str(archByteType) + """ *)&datum)[sizeof(""" + str(cur_type) + """) -1 -i] = helper_byte;
        }
        """
        swapEndianessBody = cxx_writer.Code(swapEndianessCode)
        swapEndianessParam = cxx_writer.Parameter('datum', cur_type.makeRef())
        swapEndianessMethod = cxx_writer.Method('swap_endianess', swapEndianessBody, cxx_writer.voidType, 'public', [swapEndianessParam], inline = True, noException = True, const = True)
        memoryIfMembers.append(swapEndianessMethod)

    # Constructors and Destructors
    memoryIfDtor = cxx_writer.Destructor(emptyBody, 'public', True)

    # Class
    memoryIfClass = cxx_writer.ClassDeclaration('MemoryInterface', memoryIfMembers, namespaces = [namespace])
    memoryIfClass.addDestructor(memoryIfDtor)
    memoryClasses.append(memoryIfClass)

    ## @} Memory Interface Class
    #---------------------------------------------------------------------------
    ## @name Local Memory Class
    #  @{

    from registerWriter import registerType, aliasType, registerContainerType
    MemoryToolsIfType = cxx_writer.TemplateType('MemoryToolsIf', [str(archWordType)], 'common/tools_if.hpp')

    localMemoryMembers = []
    aliasAttrs = []
    aliasParams = []
    aliasInit = []

    # Attributes and Initialization
    if self.memAlias:
        aliasAttrs.append(cxx_writer.Attribute('R', registerContainerType.makeRef(), 'private'))
        aliasParams.append(cxx_writer.Parameter('R', registerContainerType.makeRef()))
        aliasInit.append('R(R)')

    localMemoryMembers.append(cxx_writer.Attribute('debugger', MemoryToolsIfType.makePointer(), 'private'))

    # Methods: set_debugger()
    Code = 'this->debugger = debugger;'
    localMemoryMembers.append(cxx_writer.Method('set_debugger', cxx_writer.Code(Code), cxx_writer.voidType, 'public', [cxx_writer.Parameter('debugger', MemoryToolsIfType.makePointer())]))

    # Methods: Building Blocks
    checkAddressCode = 'if (address >= this->size) {\nTHROW_ERROR("Address " << std::hex << std::showbase << address << " out of memory.");\n}\n'
    checkAddressCodeException = 'if (address >= this->size) {\nTHROW_EXCEPTION("Address " << std::hex << std::showbase << address << " out of memory.");\n}\n'

    checkWatchPointCode = """if (this->debugger != NULL) {
        this->debugger->notify_address(address, sizeof(datum));
    }
    """

    swapEndianessCode = '// Endianess conversion: The processor is always modeled with the host endianess. In case they are different, the endianess is swapped.\n'
    if self.isBigEndian:
        swapEndianessDefine = '#ifdef LITTLE_ENDIAN_BO\n'
    else:
        swapEndianessDefine = '#ifdef BIG_ENDIAN_BO\n'
    swapEndianessCode += swapEndianessDefine + 'this->swap_endianess(datum);\n#endif\n'

    if self.isBigEndian:
        swapDEndianessCode = '#ifdef LITTLE_ENDIAN_BO\n'
    else:
        swapDEndianessCode = '#ifdef BIG_ENDIAN_BO\n'
    swapDEndianessCode += str(archWordType) + ' datum1 = (' + str(archWordType) + ')(datum);\nthis->swap_endianess(datum1);\n'
    swapDEndianessCode += str(archWordType) + ' datum2 = (' + str(archWordType) + ')(datum >> ' + str(self.wordSize*self.byteSize) + ');\nthis->swap_endianess(datum2);\n'
    swapDEndianessCode += 'datum = datum1 | (((' + str(archDWordType) + ')datum2) << ' + str(self.wordSize*self.byteSize) + ');\n#endif\n'

    endianessCode = {'read_dword': swapDEndianessCode, 'read_word': swapEndianessCode, 'read_half': swapEndianessCode, 'read_byte': '', 'read_instr': swapEndianessCode,
                'read_dword_dbg': swapDEndianessCode, 'read_word_dbg': swapEndianessCode, 'read_half_dbg': swapEndianessCode, 'read_byte_dbg': '',
                'write_dword': swapDEndianessCode, 'write_word': swapEndianessCode, 'write_half': swapEndianessCode, 'write_byte': '',
                'write_dword_dbg': swapDEndianessCode, 'write_word_dbg': swapEndianessCode, 'write_half_dbg': swapEndianessCode, 'write_byte_dbg': ''}

    readAliasCode = {}
    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\nreturn this->' + alias.alias + ';\n}\n'
    readAliasCode['read_dword'] = readMemAliasCode
    readAliasCode['read_word'] = readMemAliasCode
    readAliasCode['read_dword_dbg'] = readMemAliasCode
    readAliasCode['read_word_dbg'] = readMemAliasCode
    readAliasCode['read_instr'] = readMemAliasCode
    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn (' + str(archHWordType) + ')' + alias.alias + '_temp;\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archHWordType) + ' *)&(' + alias.alias + '_temp)) + 1);\n}\n'
    readAliasCode['read_half_dbg'] = readMemAliasCode
    readAliasCode['read_half'] = readMemAliasCode
    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn (' + str(archByteType) + ')' + alias.alias + '_temp;\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 1);\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 2);\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 3);\n}\n'
    readAliasCode['read_byte_dbg'] = readMemAliasCode
    readAliasCode['read_byte'] = readMemAliasCode

    writeAliasCode = {}
    writeMemAliasCode = ''
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n this->' + alias.alias + ' = datum;\nreturn;\n}\n'
    writeAliasCode['write_dword'] = writeMemAliasCode
    writeAliasCode['write_word'] = writeMemAliasCode
    writeAliasCode['write_dword_dbg'] = writeMemAliasCode
    writeAliasCode['write_word_dbg'] = writeMemAliasCode
    writeMemAliasCode = swapEndianessDefine
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) + 1) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#else\n'
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) + 1) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#endif\n'
    writeAliasCode['write_half'] = writeMemAliasCode
    writeAliasCode['write_half_dbg'] = writeMemAliasCode
    writeMemAliasCode = swapEndianessDefine
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archByteType) + '*)&' + alias.alias + '_temp) = (' + str(archByteType) + ')datum;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 1) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 2) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 3) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#else\n'
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archByteType) + '*)&' + alias.alias + '_temp) = (' + str(archByteType) + ')datum;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 1) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 2) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 3) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#endif\n'
    writeAliasCode['write_byte'] = writeMemAliasCode
    writeAliasCode['write_byte_dbg'] = writeMemAliasCode

    # If there is no memory or debugging is disabled.
    if not self.memories or not any(memAttr[1] == True for memAttr in processor.memories.values()):
        # Methods: read(), write(), lock(), unlock()
        methodsCode = {}
        methodsAttrs = {}
        for methName in readMethodNames + readMethodNames_dbg:
            methodsAttrs[methName] = []
            if methName.endswith('_gdb'):
                readBody = cxx_writer.Code(readAliasCode[methName] + checkAddressCodeException + '\n' + str(methodTypes[methName]) + ' datum = *(' + str(methodTypes[methName].makePointer()) + ')(this->memory + (unsigned long)address);\n' + endianessCode[methName] + '\nreturn datum;')
            else:
                methodsAttrs[methName].append('noexc')
                readBody = cxx_writer.Code(readAliasCode[methName] + checkAddressCode + '\n' + str(methodTypes[methName]) + ' datum = *(' + str(methodTypes[methName].makePointer()) + ')(this->memory + (unsigned long)address);\n' + endianessCode[methName] + '\nreturn datum;')
                if methName == 'read_word':
                    methodsAttrs[methName].append('inline')
            readBody.addInclude('common/report.hpp')
            methodsCode[methName] = readBody
        for methName in writeMethodNames + writeMethodNames_dbg:
            methodsAttrs[methName] = []
            if methName.endswith('_gdb'):
                methodsCode[methName] = cxx_writer.Code(writeAliasCode[methName] + checkAddressCodeException + checkWatchPointCode + '\n' + endianessCode[methName] + '\n*(' + str(methodTypes[methName].makePointer()) + ')(this->memory + (unsigned long)address) = datum;')
            else:
                methodsAttrs[methName].append('noexc')
                methodsCode[methName] = cxx_writer.Code(writeAliasCode[methName] + checkAddressCode + checkWatchPointCode + '\n' + endianessCode[methName] + '\n*(' + str(methodTypes[methName].makePointer()) + ')(this->memory + (unsigned long)address) = datum;')
                if methName == 'write_word':
                    methodsAttrs[methName].append('inline')
        for methName in genericMethodNames:
            methodsAttrs[methName] = []
            methodsCode[methName] = emptyBody
        addMemoryMethods(self, localMemoryMembers, methodsCode, methodsAttrs)

        # Attributes and Initialization
        arrayAttr = cxx_writer.Attribute('memory', cxx_writer.charPtrType, 'private')
        localMemoryMembers.append(arrayAttr)

        sizeAttr = cxx_writer.Attribute('size', cxx_writer.uintType, 'private')
        localMemoryMembers.append(sizeAttr)

        # Constructors and Destructors
        localMemoryCtorParams = [cxx_writer.Parameter('size', cxx_writer.uintType)]
        localMemoryCtorBody = cxx_writer.Code('this->memory = new char[size];\nthis->debugger = NULL;')
        localMemoryCtor = cxx_writer.Constructor(localMemoryCtorBody, 'public', localMemoryCtorParams + aliasParams, ['size(size)'] + aliasInit)
        localMemoryDtorBody = cxx_writer.Code('delete [] this->memory;')
        localMemoryDtor = cxx_writer.Destructor(localMemoryDtorBody, 'public', True)

        # Class
        localMemoryClass = cxx_writer.ClassDeclaration('LocalMemory', localMemoryMembers + aliasAttrs, [memoryIfClass.getType()], namespaces = [namespace])
        localMemoryClass.addDocString(brief = 'Memory Interface Class', detail = 'Interface used by the core to communicate with memory. Defines the required TLM ports.')
        localMemoryClass.addConstructor(localMemoryCtor)
        localMemoryClass.addDestructor(localMemoryDtor)
        memoryClasses.append(localMemoryClass)

    # Debugging is enabled.
    else:
        # Methods: read(), write(), lock()
        dumpCode1 = '\n\nMemAccessType dump_info;\n'
        if not self.systemc and not model.startswith('acc') and not model.endswith('AT'):
            dumpCode1 += 'dump_info.simulation_time = cur_cycle;\n'
        else:
            dumpCode1 += 'dump_info.simulation_time = sc_time_stamp().to_double();\n'
        if [memName for memName, memAttr in processor.memories.items() if memAttr[2] != '']:
            dumpCode1 += 'dump_info.program_counter = this->' + self.memories[memName][2] + ';\n'
        else:
            dumpCode1 += 'dump_info.program_counter = 0;\n'
        dumpCode1 += 'for (unsigned i = 0; i < '
        dumpCode2 = """; i++) {
            dump_info.address = address + i;
            dump_info.val = (char)((datum & (0xFF << i*8)) >> i*8);
            this->dump_file.write((char*)&dump_info, sizeof(MemAccessType));
        }
        """
        methodsCode = {}
        methodsAttrs = {}
        for methName in readMethodNames + readMethodNames_dbg:
            methodsAttrs[methName] = ['noexc']
            if methName.endswith('_gdb'):
                readBody = cxx_writer.Code(readAliasCode[methName] + checkAddressCodeException + '\n' + str(methodTypes[methName]) + ' datum = *(' + str(methodTypes[methName].makePointer()) + ')(this->memory + (unsigned long)address);\n' + endianessCode[methName] + '\nreturn datum;')
            else:
                readBody = cxx_writer.Code(readAliasCode[methName] + checkAddressCode + '\n' + str(methodTypes[methName]) + ' datum = *(' + str(methodTypes[methName].makePointer()) + ')(this->memory + (unsigned long)address);\n' + endianessCode[methName] + '\nreturn datum;')
                if methName == 'read_word':
                    methodsAttrs[methName].append('inline')
            readBody.addInclude('common/report.hpp')
            methodsCode[methName] = readBody
        for methName in writeMethodNames + writeMethodNames_dbg:
            methodsAttrs[methName] = []
            if methName.endswith('_gdb'):
                methodsCode[methName] = cxx_writer.Code(writeAliasCode[methName] + checkAddressCodeException + checkWatchPointCode + '\n' + endianessCode[methName] + '\n*(' + str(methodTypes[methName].makePointer()) + ')(this->memory + (unsigned long)address) = datum;' + dumpCode1 + str(methodTypeLen[methName]) + dumpCode2)
            else:
                methodsAttrs[methName].append('noexc')
                methodsCode[methName] = cxx_writer.Code(writeAliasCode[methName] + checkAddressCode + checkWatchPointCode + '\n' + endianessCode[methName] + '\n*(' + str(methodTypes[methName].makePointer()) + ')(this->memory + (unsigned long)address) = datum;' + dumpCode1 + str(methodTypeLen[methName]) + dumpCode2)
                if methName == 'write_word':
                    methodsAttrs[methName].append('inline')
        for methName in genericMethodNames:
            methodsAttrs[methName] = []
            methodsCode[methName] = emptyBody
        addMemoryMethods(self, localMemoryMembers, methodsCode, methodsAttrs)

        # Methods: end_of_simulation()
        endOfSimBody = cxx_writer.Code("""if (this->dump_file) {
           this->dump_file.flush();
           this->dump_file.close();
        }
        """)
        endOfSimMethod = cxx_writer.Method('end_of_simulation', endOfSimBody, cxx_writer.voidType, 'public')
        localMemoryMembers.append(endOfSimMethod)

        # Attributes and Initialization
        arrayAttr = cxx_writer.Attribute('memory', cxx_writer.charPtrType, 'private')
        localMemoryMembers.append(arrayAttr)

        sizeAttr = cxx_writer.Attribute('size', cxx_writer.uintType, 'private')
        localMemoryMembers.append(sizeAttr)

        dumpFileAttr = cxx_writer.Attribute('dump_file', cxx_writer.ofstreamType, 'private')
        localMemoryMembers.append(dumpFileAttr)

        if not self.systemc and not model.startswith('acc') and not model.endswith('AT'):
            cycleAttr = cxx_writer.Attribute('cur_cycle', cxx_writer.uintType.makeRef(), 'private')
            localMemoryCtorParams.append(cxx_writer.Parameter('cur_cycle', cxx_writer.uintType.makeRef()))
            localMemoryCtorInit.append('cur_cycle(cur_cycle)')
            localMemoryMembers.append(cycleAttr)

        if [memName for memName, memAttr in processor.memories.items() if memAttr[2] != '']:
            # Find out type of fetch register.
            from processor import extractRegInterval
            fetchReg = self.memories[memName][2]
            fetchIndex = extractRegInterval(fetchReg)
            if fetchIndex: fetchReg = fetchReg[0:fetchReg.index('[')]
            fetchType = None
            if fetchReg in [i.name for i in self.regs + self.regBanks]:
                fetchType = registerType
            if fetchType == None and fetchReg in [i.name for i in self.aliasRegs + self.aliasRegBanks]:
                fetchType = aliasType
            localMemoryMembers.append(cxx_writer.Attribute(self.memories[memName][2], fetchType.makeRef(), 'private'))
            pcRegParam = [cxx_writer.Parameter(self.memories[memName][2], fetchType.makeRef())]
            pcRegInit = [self.memories[memName][2] + '(' + self.memories[memName][2] + ')']

        # Constructors and Destructors
        localMemoryCtorParams = [cxx_writer.Parameter('size', cxx_writer.uintType)]
        localMemoryCtorInit = ['size(size)']
        localMemoryCtorBody = cxx_writer.Code("""this->memory = new char[size];
            this->debugger = NULL;
            this->dump_file.open("memoryDump.dmp", ios::out | ios::binary | ios::ate);
            if (!this->dump_file) {
                THROW_EXCEPTION("Cannot open file memoryDump.dmp for writing.");
            }
        """)
        localMemoryCtor = cxx_writer.Constructor(localMemoryCtorBody, 'public', localMemoryCtorParams + aliasParams + pcRegParam, localMemoryCtorInit + aliasInit + pcRegInit)
        localMemoryDtorBody = cxx_writer.Code("""delete [] this->memory;
        if (this->dump_file) {
           this->dump_file.flush();
           this->dump_file.close();
        }
        """)
        localMemoryDtor = cxx_writer.Destructor(localMemoryDtorBody, 'public', True)

        # Class
        localMemoryClass = cxx_writer.ClassDeclaration('LocalMemory', localMemoryMembers + aliasAttrs, [memoryIfClass.getType()], namespaces = [namespace])
        localMemoryClass.addDocString(brief = 'Memory Interface Class', detail = 'Interface used by the core to communicate with memory. Defines the required TLM ports.')
        localMemoryClass.addConstructor(localMemoryCtor)
        localMemoryClass.addDestructor(localMemoryDtor)
        memoryClasses.append(localMemoryClass)

    ## @} Local Memory Class
    #---------------------------------------------------------------------------

    return memoryClasses

################################################################################
