################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     SimpleDecls.py
# @brief    This file is part of the TRAP CXX code generator module.
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

import copy
import CustomCode, Writer

class DumpElement:
    """Base element of all the elements which have to be dumped. All printable elements like
    classes, attributes, methods .... derive from this class"""

    def __init__(self,  name):
        self.name = name
        self.docbrief = ''
        self.docdetail = ''
    def addDocString(self, brief, detail):
        self.docbrief = brief
        self.docdetail = detail
    def printDocString(self, writer, detail = True):
      if self.docbrief == '' and self.docdetail == '': return
      if detail and self.docdetail != '':
          writer.write('/**\n* @brief ')
          writer.write(self.docbrief + '\n', prefix = '*        ')
          writer.write('*\n' + self.docdetail + '\n', prefix = '* ')
          writer.write('*/\n')
      else:
          writer.write('/// ' + self.docbrief + '\n')
    def __str__(self):
        try:
            stringWriter = Writer.StringWriter()
            self.writeDeclaration(stringWriter)
            return str(stringWriter)
        except:
            return self.name

class Define:
    def __init__(self, defineStr, namespace = [], includes = []):
        self.defineStr = defineStr
        if type(namespace) == type(''):
            self.namespace = [namespace]
        else:
            self.namespace = namespace
        if type(includes) == type(''):
            self.includes = [includes]
        else:
            self.includes = includes

    def writeImplementation(self, writer):
        pass

    def writeDeclaration(self, writer):
        for namespace in self.namespace:
            writer.write('namespace ' + namespace + ' {\n')
        writer.write(self.defineStr)
        for namespace in self.namespace:
            writer.write('} // namespace ' + namespace + '\n')

    def getIncludes(self):
        return self.includes

    def __str__(self):
        return self.defineStr

class UseNamespace:
    def __init__(self, namespace):
        self.namespace = namespace

    def writeImplementation(self, writer):
        writer.write('using namespace ' + self.namespace + ';\n')

    def writeDeclaration(self, writer):
        self.writeImplementation(writer)

    def getIncludes(self):
        return []

    def __str__(self):
        return 'using namespace ' + self.namespace

class Type(DumpElement):
    """Represents a type; this is use for variable declaration, function parameter declaration ..."""

    def __init__(self, name, includes = [], const = False):
        DumpElement.__init__(self, name)
        if type(includes) == type(''):
            self.includes = [includes]
        else:
            self.includes = includes
        self.modifiers = []
        self.const = const

    def makePointer(self):
        import copy
        newType = copy.deepcopy(self)
        newType.modifiers.append('*')
        return newType
    def makeRef(self):
        import copy
        newType = copy.deepcopy(self)
        newType.modifiers.append('&')
        return newType
    def makeNormal(self):
        import copy
        newType = copy.deepcopy(self)
        if not self.modifiers:
            return newType
        else:
            newType.modifiers.pop()
            return newType
    def makeConst(self):
        import copy
        newType = copy.deepcopy(self)
        newType.const = True
        return newType
    def removeConst(self):
        import copy
        newType = copy.deepcopy(self)
        newType.const = False
        return newType

    def writeDeclaration(self, writer):
        if self.const:
            writer.write('const ')
        writer.write(self.name)
        for i in self.modifiers:
            writer.write(i)

    def getIncludes(self):
        return copy.copy(self.includes)

    def __str__(self):
        typeStr = ''
        if self.const:
            typeStr += 'const '
        typeStr += self.name
        for i in self.modifiers:
            typeStr += i
        return typeStr

class TemplateType(Type):
    """Represents a templated type; this is use for variable declaration, function parameter declaration ..."""

    def __init__(self, name, template = [], includes = [], const = False):
        Type.__init__(self, name, includes, const)
        if type(template) != type([]):
            self.template = [template]
        else:
            self.template = template

    def writeDeclaration(self, writer):
        currentModifiers = self.modifiers
        self.modifiers = []
        Type.writeDeclaration(self, writer)
        if self.template:
            writtenTempl = 0
            writer.write('<')
            for i in self.template:
                writtenTempl += 1
                try:
                    i.writeDeclaration(writer)
                except AttributeError:
                    writer.write(str(i))
                if writtenTempl < len(self.template):
                    writer.write(', ')
                # Avoid >> in nested templates
                if isinstance(i, TemplateType):
                    writer.write(' ')
            writer.write('>')
        for i in currentModifiers:
            writer.write(i)
        self.modifiers = currentModifiers

    def getIncludes(self):
        includes = copy.copy(Type.getIncludes(self))
        for i in self.template:
            try:
                for j in i.getIncludes():
                    if not j in includes:
                        includes.append(j)
            except AttributeError:
                pass
        return includes

    def __str__(self):
        currentModifiers = copy.copy(self.modifiers)
        self.modifiers = []
        typeStr = Type.__str__(self)
        if self.template:
            typeStr += '<'
            for i in self.template:
                typeStr += str(i)
                if i != self.template[-1]:
                    typeStr += ', '
            typeStr += ' >'
        for i in currentModifiers:
            typeStr += i
        return typeStr

intType = Type('int')
longlongType = Type('long long')
ulonglongType = Type('unsigned long long')
shortType = Type('short int')
ushortType = Type('unsigned short int')
uintType = Type('unsigned')
floatType = Type('float')
doubleType = Type('double')
charType = Type('char')
ucharType = Type('unsigned char')
boolType = Type('bool')
sc_uint64Type = Type('sc_dt::uint64', 'systemc.h')
sc_eventType = Type('sc_event', 'systemc.h')
sc_moduleType = Type('sc_module', 'systemc.h')
sc_module_nameType = Type('sc_module_name', 'systemc.h')
sc_timeType = Type('sc_time', 'systemc.h')
stringType = Type('std::string', 'string')
ofstreamType = Type('std::ofstream', ['iostream', 'fstream'])
voidType = Type('void')
intRefType = intType.makeRef()
longlongRefType = longlongType.makeRef()
ulonglongRefType = ulonglongType.makeRef()
shortRefType = shortType.makeRef()
ushortRefType = ushortType.makeRef()
uintRefType = uintType.makeRef()
floatRefType = floatType.makeRef()
doubleRefType = doubleType.makeRef()
charRefType = charType.makeRef()
ucharRefType = ucharType.makeRef()
boolRefType = boolType.makeRef()
sc_uint64RefType = sc_uint64Type.makeRef()
sc_eventRefType = sc_eventType.makeRef()
sc_moduleRefType = sc_moduleType.makeRef()
sc_module_nameRefType = sc_module_nameType.makeRef()
sc_timeRefType = sc_timeType.makeRef()
stringRefType = stringType.makeRef()
intPtrType = intType.makePointer()
longlongPtrType = longlongType.makePointer()
ulonglongPtrType = ulonglongType.makePointer()
uintPtrType = uintType.makePointer()
shortPrtType = shortType.makePointer()
ushortPrtType = ushortType.makePointer()
floatPtrType = floatType.makePointer()
doublePtrType = doubleType.makePointer()
charPtrType = charType.makePointer()
ucharPtrType = ucharType.makePointer()
boolPtrType = boolType.makePointer()
sc_uint64PtrType = sc_uint64Type.makePointer()
sc_eventPtrType = sc_eventType.makePointer()
sc_modulePtrType = sc_moduleType.makePointer()
sc_module_namePtrType = sc_module_nameType.makePointer()
sc_timePtrType = sc_timeType.makePointer()
stringPtrType = stringType.makePointer()
voidPtrType = voidType.makePointer()

class Parameter(DumpElement):
    """Represents a parameter of a function; this parameter can be either input or output
    (even though in C++ output parameters are not really used)"""

    def __init__(self, name, type, restrict = False, input = True, initValue = ''):
        DumpElement.__init__(self, name)
        self.type = type
        self.input = input
        self.restrict = restrict
        self.initValue = initValue

    def writeImplementation(self, writer):
        self.type.writeDeclaration(writer)
        if self.input:
            if self.restrict:
                writer.write(' restrict')
            writer.write(' ' + self.name)

    def writeDeclaration(self, writer):
        self.type.writeDeclaration(writer)
        if self.input:
            if self.restrict:
                writer.write(' restrict')
            writer.write(' ' + self.name)
            if self.initValue:
                writer.write(' = ' + self.initValue)

    def getIncludes(self):
        return copy.copy(self.type.getIncludes())

    def __str__(self):
        outStr = str(self.type)
        if self.input:
            if self.restrict:
                outStr += ' restrict'
            outStr += ' ' + self.name
            if self.initValue:
                outStr += ' = ' + self.initValue
        return outStr

    def __repr__(self):
        return self.__str__()

class Variable(DumpElement):
    """Represents a variable of the program; this is a global variable, in the
    sense that it is not a member of a class; it can, anyway, be a variable
    of a method"""

    def __init__(self, name, varType, static = False, initValue = '', namespaces = []):
        DumpElement.__init__(self, name)
        self.varType = varType
        self.static = static
        self.namespaces = namespaces
        self.initValue = initValue

    def writeDeclaration(self, writer):
        pass
        #for namespace in self.namespaces:
            #writer.write('namespace ' + namespace + '{\n')
        #if self.docbrief:
            #self.printDocString(writer)
        #writer.write('extern ')
        #if self.static:
            #writer.write('static ')
        #self.varType.writeDeclaration(writer)
        #writer.write(' ' + self.name)
        #if self.initValue:
            #writer.write(' = ' + self.initValue)
        #writer.write(';\n')
        #for namespace in self.namespaces:
            #writer.write('};\n')

    def writeImplementation(self, writer):
        for namespace in self.namespaces:
            writer.write('namespace ' + namespace + ' {\n')
        if self.docbrief:
            self.printDocString(writer)
        if self.static:
            writer.write('static ')
        self.varType.writeDeclaration(writer)
        writer.write(' ' + self.name)
        if self.initValue:
            writer.write(' = ' + self.initValue)
        writer.write(';\n')
        for namespace in self.namespaces:
            writer.write('} // namespace ' + namespace + '\n')

    def getIncludes(self):
        return copy.copy(self.varType.getIncludes())

    def __str__(self):
        varStr = ''
        if self.docbrief:
            varStr += self.docbrief
        if self.static:
            varStr += 'static '
        varStr += str(self.varType) + ' ' + self.name
        if self.initValue:
            varStr += ' = ' + self.initValue
        varStr += ';\n'
        return varStr

class Function(DumpElement):
    """Represents a function of the program; this function is not
    a method of a class"""

    def __init__(self, name, body, retType = Type('void'), parameters = [], static = False, inline = False, template = [], noException = False, namespaces = []):
        DumpElement.__init__(self, name)
        self.body = body
        self.parameters = parameters
        self.retType = retType
        self.template = template
        self.static = static
        self.inline = inline
        self.namespaces = namespaces
        self.noException = noException

    def writeDeclaration(self, writer):
        for namespace in self.namespaces:
            writer.write('namespace ' + namespace + ' {\n')
        if self.docbrief:
            self.printDocString(writer)
        if self.template:
            writer.write('template <typename ')
            for i in self.template:
                writer.write(i)
                if i != self.template[-1]:
                    writer.write(', ')
            writer.write('> ')
        if self.static:
            writer.write('static ')
        if self.inline:
            writer.write('inline ')
        try:
            if self.virtual:
                if self.static or self.inline:
                    raise Exception('Cannot set inline or static operation ' + self.name + ' as virtual.')
                writer.write('virtual ')
        except AttributeError:
            pass
        self.retType.writeDeclaration(writer)
        if self.retType.name:
            writer.write(' ')
        writer.write(self.name + '(')
        split = False
        indent = -1
        if (len(self.parameters) > 1):
            writer.write('\n')
            indent = writer.curIndent+2
        for i in self.parameters:
            i.writeDeclaration(writer)
            if i != self.parameters[-1]:
                writer.write(', ', indent = indent, split = ',')
        writer.write(')', indent = indent, split = ',')
        try:
            if self.const:
                writer.write(' const', indent = indent, split = ',')
        except AttributeError:
            pass
        if self.noException:
            writer.write(' throw()', indent = indent, split = ',')
        if self.template or self.inline:
            writer.write(' {\n', indent = indent, split = ',')
            self.body.writeImplementation(writer)
            writer.write('} // ' + self.name + '()\n\n')
        else:
            try:
                if self.pure:
                    writer.write(' = 0', indent = indent, split = ',')
            except AttributeError:
                pass
            writer.write(';\n', indent = indent, split = ',')
        for namespace in self.namespaces:
            writer.write('} // namespace ' + namespace + '\n')

    def writeImplementation(self, writer):
        if self.template or self.inline:
            return
        if self.docbrief:
            self.printDocString(writer)
        self.retType.writeDeclaration(writer)
        writer.write(' ')
        for namespace in self.namespaces:
            writer.write(namespace + '::')
        writer.write(self.name + '(')
        split = False
        indent = -1
        if (len(self.parameters) > 1):
            if (len(self.parameters) < 10):
                split = True
            writer.write('\n')
            indent = writer.curIndent+2
        for i in self.parameters:
            i.writeImplementation(writer)
            if i != self.parameters[-1]:
                if (split == True):
                    writer.write(',\n', indent = indent)
                else:
                    writer.write(', ', indent = indent, split = ',')
        writer.write(')', indent = indent, split = ',')
        if self.noException:
            writer.write(' throw()', indent = indent, split = ',')
        writer.write(' {\n', indent = indent, split = ',')
        self.body.writeImplementation(writer)
        writer.write('} // ' + self.name + '()\n')

    def getIncludes(self):
        includes = copy.copy(self.retType.getIncludes())
        for i in self.parameters:
            for j in i.getIncludes():
                if not j in includes:
                    includes.append(j)
        for j in self.body.getIncludes():
            if not j in includes:
                includes.append(j)
        return includes

    def getRetValIncludes(self):
        return self.retType.getIncludes()

class Operator(Function):
    """Represents an operator of the program; this operator is not
    a method of a class"""

    def __init__(self, name, body, retType = Type('void'), parameters = [], static = False, inline = False, template = [], noException = False, namespaces = []):
        Function.__init__(self, 'operator ' + name, body, retType, parameters, static, inline, template, noException, namespaces)

class Enum(DumpElement):
    """Represents the declaration of an enumeration type"""

    def __init__(self, name, values, namespaces = []):
        DumpElement.__init__(self, name)
        self.values = values
        self.namespaces = namespaces

    def addValue(self, name, value):
        self.values[name] = value

    def writeDeclaration(self, writer):
        for namespace in self.namespaces:
            writer.write('namespace ' + namespace + ' {\n')
        if self.docbrief:
            self.printDocString(writer)
        if not self.values:
            raise Exception('Cannot print empty enum.')
        code = 'enum ' + self.name + ' {\n'
        for key, val in self.values.items():
            code += key + ' = ' + str(val) + ' \n,'
        writer.write(code[:-1] + '};\n')
        for namespace in self.namespaces:
            writer.write('} // namespace ' + namespace + '\n')

class Union(DumpElement):
    """Represents a union"""

    def __init__(self, name, members = [], namespaces = []):
        DumpElement.__init__(self, name)
        self.members = members
        self.namespaces = namespaces

    def addMember(self, member):
        self.members.append(member)

    def writeDeclaration(self, writer):
        for namespace in self.namespaces:
            writer.write('namespace ' + namespace + '{\n')
        if self.docbrief:
            self.printDocString(writer)
        if not self.members:
            raise Exception('Cannot print empty union.')
        writer.write('union ' + self.name + ' {\n')
        for i in self.members:
            i.writeImplementation(writer)
        writer.write('};\n')
        for namespace in self.namespaces:
            writer.write('} // namespace ' + namespace + '\n')

    def getIncludes(self):
        includes = []
        for i in self.members:
            for j in i.getIncludes():
                if not j in includes:
                    includes.append(j)
        return includes

    def getType(self):
        return Type(self.name)

class BitField(DumpElement):
    """Represents a bitfield"""

    def __init__(self, name, members = [], namespaces = []):
        DumpElement.__init__(self, name)
        self.members = members
        self.namespaces = namespaces

    def addMember(self, member):
        self.members.append(member)

    def writeDeclaration(self, writer):
        for namespace in self.namespaces:
            writer.write('namespace ' + namespace + '{\n')
        if self.docbrief:
            self.printDocString(writer)
        if not self.members:
            raise Exception('Cannot print empty bitfield.')
        writer.write('struct ' + self.name + ' {\n')
        for i in self.members:
            writer.write('unsigned ' + str(i[0]) + ':' + str(i[1]) + ';\n')
        writer.write('};\n')
        for namespace in self.namespaces:
            writer.write('} // namespace ' + namespace + '\n')

    def getIncludes(self):
        return []

    def getType(self):
        return Type(self.name)

class Typedef(DumpElement):
    """Represents a typedef of an existing type"""

    def __init__(self, name, oldType, namespaces = []):
        DumpElement.__init__(self, name)
        self.oldType = oldType
        self.namespaces = namespaces

    def writeDeclaration(self, writer):
        for namespace in self.namespaces:
            writer.write('namespace ' + namespace + '{\n')
        if self.docbrief:
            self.printDocString(writer)
        writer.write('typedef ' + self.name + ' ')
        self.oldType.writeDeclaration(writer)
        writer.write(';\n')
        for namespace in self.namespaces:
            writer.write('} // namespace ' + namespace + '\n')

    def getIncludes(self):
        return copy.copy(self.oldType.getIncludes())

    def getType(self):
        return Type(self.name)
