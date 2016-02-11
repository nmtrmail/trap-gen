# -*- coding: iso-8859-1 -*-
####################################################################################
#         ___        ___           ___           ___
#        /  /\      /  /\         /  /\         /  /\
#       /  /:/     /  /::\       /  /::\       /  /::\
#      /  /:/     /  /:/\:\     /  /:/\:\     /  /:/\:\
#     /  /:/     /  /:/~/:/    /  /:/~/::\   /  /:/~/:/
#    /  /::\    /__/:/ /:/___ /__/:/ /:/\:\ /__/:/ /:/
#   /__/:/\:\   \  \:\/:::::/ \  \:\/:/__\/ \  \:\/:/
#   \__\/  \:\   \  \::/~~~~   \  \::/       \  \::/
#        \  \:\   \  \:\        \  \:\        \  \:\
#         \  \ \   \  \:\        \  \:\        \  \:\
#          \__\/    \__\/         \__\/         \__\/
#
#   This file is part of TRAP.
#
#   TRAP is free software; you can redistribute it and/or modify
#   it under the terms of the GNU Lesser General Public License as published by
#   the Free Software Foundation; either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU Lesser General Public License for more details.
#
#   You should have received a copy of the GNU Lesser General Public License
#   along with this TRAP; if not, write to the
#   Free Software Foundation, Inc.,
#   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
#   or see <http://www.gnu.org/licenses/>.
#
#   (c) Luca Fossati, fossati@elet.polimi.it, fossati.l@gmail.com
#
####################################################################################


from SimpleDecls import *
from CustomCode import *

class ClassMember:
    """Base class for all the elements which can be members of a
    class, i.e. attributes and methods"""

    def __init__(self, visibility, name):
        if not visibility in ['pri', 'pro', 'pu']:
            raise Exception(str(visibility) + ' is not a valid visibility attribute for member ' + name)
        self.visibility = visibility

# TODO: add the possibility of defining templated methods as done for
# normal functions
class Method(ClassMember, Function):
    """Method of a class; note how it is nothing but a normal function
    with the addition of the visibility attribute"""

    def __init__(self, name, body, retType, visibility, parameters = [],
                 static = False, inline = False, noException = False, virtual = False, pure = False, const = False):
        ClassMember.__init__(self, visibility, name)
        Function.__init__(self, name, body, retType, parameters, static, inline, [], noException)
        self.virtual = virtual
        self.pure = pure
        if self.pure:
            self.virtual = True
        self.const = const

    def writeImplementation(self, writer, className = '', namespaces = []):
        if self.inline or self.pure:
            return
        if self.docbrief:
            self.printDocString(writer)
        self.retType.writeDeclaration(writer)
        writer.write(' ')
        for namespace in namespaces:
            writer.write(namespace + '::')
        if className:
            writer.write(className + '::')
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
                    writer.write(', ', split = ',', indent = indent)
        writer.write(')', split = ',', indent = indent)
        if self.const:
            writer.write(' const')
        if self.noException:
            writer.write(' throw()')
        writer.write(' {\n', split = ',', indent = indent)
        self.body.writeImplementation(writer)
        writer.write('} // ' + self.name + '()\n\n')

class MemberOperator(ClassMember, Operator):
    """Operator of a class; note how it is nothing but a normal operator
    with the addition of the visibility attribute"""

    def __init__(self, name, body, retType, visibility, parameters = [], static = False,
                 inline = False, noException = False, virtual = False, pure = False, const = False):
        ClassMember.__init__(self, visibility, name)
        Operator.__init__(self, name, body, retType, parameters, static, inline, [], noException)
        self.virtual = virtual
        self.pure = pure
        if self.pure:
            self.virtual = True
        self.const = const

    def writeImplementation(self, writer, className = '', namespaces = []):
        if self.inline or self.pure:
            return
        if self.docbrief:
            self.printDocString(writer)
        self.retType.writeDeclaration(writer)
        writer.write(' ')
        for namespace in namespaces:
            writer.write(namespace + '::')
        if className:
            writer.write(className + '::')
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
                    writer.write(', ', split = ',', indent = indent)
        writer.write(')', split = ',', indent = indent)
        if self.const:
            writer.write(' const')
        if self.noException:
            writer.write(' throw()')
        writer.write(' {\n', split = ',', indent = indent)
        self.body.writeImplementation(writer)
        writer.write('} // ' + self.name + '()\n\n')

class Constructor(ClassMember, Function):
    def __init__(self, body, visibility, parameters = [], initList = []):
        ClassMember.__init__(self, visibility, 'constructor')
        Function.__init__(self, '', body, Type(''), parameters)
        self.initList = initList

    def writeImplementation(self, writer, className = '', namespaces = []):
        if self.docbrief:
            self.printDocString(writer)
        for namespace in namespaces:
            writer.write(namespace + '::')
        if className:
            writer.write(className + '::')
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
                    writer.write(', ', split = ',', indent = indent)
        writer.write(')', split = ',', indent = indent)
        if self.initList:
            writer.write(' :\n', split = ',', indent = indent)
            indent = writer.curIndent+1
        else: indent = -1
        split = False
        if (len(self.initList) > 1):
            if (len(self.initList) < 10):
                split = True
        else: writer.write(' ')
        for i in self.initList:
            writer.write(i)
            if i != self.initList[-1]:
                if (split == True):
                    writer.write(',\n', indent = indent)
                else:
                    writer.write(', ', split = ',', indent = indent)
            else: writer.write(' ')
        writer.write('{\n', split = ',', indent = indent)
        self.body.writeImplementation(writer)
        writer.write('} // ' + self.name + '()\n\n')

class Destructor(ClassMember, Function):
    def __init__(self, body, visibility, virtual = False):
        ClassMember.__init__(self, visibility, 'destructor')
        Function.__init__(self, '', body, Type(''), [])
        self.virtual = virtual

    def writeImplementation(self, writer, className = '', namespaces = []):
        if self.docbrief:
            self.printDocString(writer)
        for namespace in namespaces:
            writer.write(namespace + '::')
        if className:
            writer.write(className + '::')
        writer.write(self.name + '() {\n')
        self.body.writeImplementation(writer)
        writer.write('} // ' + self.name + '()\n\n')

class Attribute(ClassMember, Variable):
    """Attribute of a class; note how, a part from the visibility,
    it is simply a normal variable"""

    def __init__(self, name, varType, visibility, static = False, initValue = ''):
        ClassMember.__init__(self, visibility, name)
        Variable.__init__(self, name, varType, static, initValue)

    def writeDeclaration(self, writer):
        if self.docbrief:
            self.printDocString(writer)
        if self.static:
            writer.write('static ')
        self.varType.writeDeclaration(writer)
        writer.write(' ' + self.name + ';\n')

    def writeImplementation(self, writer, className = '', namespaces = []):
        if not className:
            self.writeDeclaration(writer)
        elif self.static:
            if self.docbrief:
                self.printDocString(writer)
            self.varType.writeDeclaration(writer)
            writer.write(' ')
            for namespace in namespaces:
                writer.write(namespace + '::')
            writer.write(className + '::' + self.name)
            if self.initValue:
                writer.write(' = ' + self.initValue)
            writer.write(';\n')

class ClassDeclaration(DumpElement):
    """Declaration of a class; it is simply a container of members,
    be them attributes or variables; it is actually also possible
    to set as members other classes, custom code ... as long as they have
    the writeDeclaration method. If these members also have the
    writeImplementation method, then a separate implementation is
    also dumped.
    Note that all the elements are grouped in private, protected
    and public and they are dumped in that order; if a member does
    not have the visibility attribute it is considered public."""

    def __init__(self, className, members = [], superclasses = [], template = [], virtual_superclasses = [], namespaces = []):
        DumpElement.__init__(self, className)
        self.members = members
        self.superclasses = superclasses
        self.virtual_superclasses = virtual_superclasses
        self.template = template
        self.namespaces = namespaces
        self.innerClasses = []

    def addMember(self, member):
        self.members.append(member)

    def addConstructor(self, constructor):
        constructor.name = self.name
        self.members.append(constructor)

    def addInnerClass(self, innerClass):
        self.innerClasses.append(innerClass)

    def addDestructor(self, destructor):
        destructor.name = '~' + self.name
        self.members.append(destructor)

    def addSuperclass(self, superclass):
        self.superclasses.append(superclass)

    def computeMemVisibility(self):
        self.private = []
        self.protected = []
        self.public = []
        for i in self.members:
            try:
                if i.visibility == 'pri':
                    if isinstance(i, Constructor):
                        self.private = [i] + self.private
                    else:
                        self.private.append(i)
                elif i.visibility == 'pro':
                    if isinstance(i, Constructor):
                        self.protected = [i] + self.protected
                    else:
                        self.protected.append(i)
                elif i.visibility == 'pu':
                    if isinstance(i, Constructor):
                        self.public = [i] + self.public
                    else:
                        self.public.append(i)
            except AttributeError:
                if isinstance(i, Constructor):
                    self.public = [i] + self.public
                else:
                    self.public.append(i)

    def writeDeclaration(self, writer):
        self.computeMemVisibility()
        for namespace in self.namespaces:
            writer.write('namespace ' + namespace + ' {\n\n')
        # Now I can simply print the declarations
        if self.docbrief:
            self.printDocString(writer)
        if self.template:
            writer.write('template <')
            for i in self.template:
                writer.write('typename ')
                writer.write(i)
                if i != self.template[-1]:
                    writer.write(', ')
            writer.write('>\n')
        writer.write('class ' + self.name)
        if self.superclasses or self.virtual_superclasses:
            writer.write(' : ')
        for i in self.virtual_superclasses:
            writer.write('public virtual ')
            i.writeDeclaration(writer)
            if i != self.virtual_superclasses[-1] or self.superclasses:
                writer.write(', ')
        for i in self.superclasses:
            writer.write('public ')
            i.writeDeclaration(writer)
            if i != self.superclasses[-1]:
                writer.write(', ')
        writer.write(' {\n')
        # First of all I create the inner classes:
        if self.innerClasses:
            writer.write('public:\n')
            for i in self.innerClasses:
                if self.template:
                    try:
                        i.writeImplementation(writer)
                    except AttributeError:
                        pass
                else:
                    i.writeDeclaration(writer)
        # Now I create the normal members
        if self.public:
            writer.write('public:\n')
            for i in self.public:
                if self.template:
                    try:
                        i.writeImplementation(writer)
                    except AttributeError:
                        pass
                else:
                    i.writeDeclaration(writer)
            writer.write('\n')
        if self.protected:
            writer.write('protected:\n')
            for i in self.protected:
                if self.template:
                    try:
                        i.writeImplementation(writer)
                    except AttributeError:
                        pass
                else:
                    i.writeDeclaration(writer)
            writer.write('\n')
        if self.private:
            writer.write('private:\n')
            for i in self.private:
                if self.template:
                    try:
                        i.writeImplementation(writer)
                    except AttributeError:
                        pass
                else:
                    i.writeDeclaration(writer)
            writer.write('\n')
        writer.write('}; // class ' + self.name + '\n\n')
        for namespace in self.namespaces:
            writer.write('} // namespace ' + namespace + '\n\n')

    def writeImplementation(self, writer, namespaces = []):
        if self.template:
            return
        # Now I print the implementation; note
        # that the order to the prints does not
        # matter anymore
        for i in self.innerClasses:
            try:
                i.writeImplementation(writer, namespaces + self.namespaces + [self.name])
            except AttributeError:
                pass
        for i in self.members:
            try:
                i.writeImplementation(writer, self.name, namespaces + self.namespaces)
            except AttributeError:
                pass

    def getIncludes(self):
        includes = []
        for i in self.superclasses + self.virtual_superclasses:
            for j in i.getIncludes():
                if not j in includes:
                    includes.append(j)
        for i in self.members:
            for j in i.getIncludes():
                if not j in includes:
                    includes.append(j)
        return includes

    def getType(self):
        return Type(self.name)

class SCModule(ClassDeclaration):
    """Represents an SC module; the biggest difference with respect to a
    normal class lies in the presence of defines inside the class declaration"""
    def __init__(self, className, members = [], superclasses = [], template = [], namespaces = []):
        ClassDeclaration.__init__(self, className, members, superclasses + [sc_moduleType], template, [], namespaces)

    def computeMemVisibility(self):
        self.private = []
        self.protected = []
        self.public = []
        added = False
        for i in self.members:
            try:
                if i.visibility == 'pri':
                    if isinstance(i, Constructor):
                        added = True
                        self.private = [Code('SC_HAS_PROCESS(' + self.name + ');'), i] + self.private
                    else:
                        self.private.append(i)
                elif i.visibility == 'pro':
                    if isinstance(i, Constructor):
                        if not added:
                            added = True
                            self.protected = [Code('SC_HAS_PROCESS(' + self.name + ');'), i] + self.protected
                        else:
                            self.protected = [i] + self.protected
                    else:
                        self.protected.append(i)
                elif i.visibility == 'pu':
                    if isinstance(i, Constructor):
                        if not added:
                            added = True
                            self.public = [Code('SC_HAS_PROCESS(' + self.name + ');'), i] + self.public
                        else:
                            self.public = [i] + self.public
                    else:
                        self.public.append(i)
            except AttributeError:
                if isinstance(i, Constructor):
                    if not added:
                        added = True
                        self.public = [Code('SC_HAS_PROCESS(' + self.name + ');'), i] + self.public
                    else:
                        self.public = [i] + self.public
                else:
                    self.public.append(i)
