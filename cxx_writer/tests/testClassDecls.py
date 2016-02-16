################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     testClassDecls.py
# @brief    This file is part of the TRAP CXX code generator testsuite.
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


try:
    import cxx_writer
except ImportError:
    import sys, os
    sys.path.append(os.path.abspath(os.path.join('..')))
    try:
        import cxx_writer
    except ImportError:
        sys.path.append(os.path.abspath(os.path.join('..', '..')))
        try:
            import cxx_writer
        except ImportError:
            print ('Please specify location of core TRAP files in testClassDecls.py.')

import unittest
import os

class TestClassDecls(unittest.TestCase):
    def setUp(self):
        try:
            os.remove('prova.cpp')
        except:
            pass
        self.writer = cxx_writer.CodeWriter('prova.cpp', indentSize = 4, lineWidth = 80)

    def tearDown(self):
        del self.writer
        os.remove('prova.cpp')

    def testEmptyClassImpl(self):
        classDecl = cxx_writer.ClassDeclaration('emptyClass')
        classDecl.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 3)
        self.assertEqual(lines[0], 'class EmptyClass {\n')
        self.assertEqual(lines[1], '}; // class EmptyClass\n')

    def testEmptyClassDecl(self):
        classDecl = cxx_writer.ClassDeclaration('emptyClass')
        classDecl.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 0)

    def testNormalDecl(self):
        intDecl = cxx_writer.intType
        privateVar = cxx_writer.Attribute('pippo', intDecl, 'pri')
        emptyBody = cxx_writer.Code('')
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu')
        classDecl = cxx_writer.ClassDeclaration('MyClass', [privateVar])
        classDecl.addConstructor(publicConstr)
        classDecl.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 9)
        self.assertEqual(lines[0], 'class MyClass {\n')
        self.assertEqual(lines[1], '    public:\n')
        self.assertEqual(lines[2], '    MyClass();\n')
        self.assertEqual(lines[3], '\n')
        self.assertEqual(lines[4], '    private:\n')
        self.assertEqual(lines[5], '    int pippo;\n')
        self.assertEqual(lines[6], '\n')
        self.assertEqual(lines[7], '}; // class MyClass\n')

    def testNormalImpl(self):
        intDecl = cxx_writer.intType
        privateVar = cxx_writer.Attribute('pippo', intDecl, 'pri')
        emptyBody = cxx_writer.Code('')
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu')
        classDecl = cxx_writer.ClassDeclaration('MyClass', [privateVar])
        classDecl.addConstructor(publicConstr)
        classDecl.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 4)
        self.assertEqual(lines[0], 'MyClass::MyClass() {\n')
        self.assertEqual(lines[1], '\n')
        self.assertEqual(lines[2], '} // MyClass()\n')

    def testTemplateDecl(self):
        intDecl = cxx_writer.intType
        stringDecl = cxx_writer.stringType
        privateVar = cxx_writer.Attribute('pippo', intDecl, 'pri')
        emptyBody = cxx_writer.Code('')
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu', [], ['std::string()'])
        classDecl = cxx_writer.ClassDeclaration('MyClass', [privateVar], [stringDecl], ['T'])
        classDecl.addConstructor(publicConstr)
        classDecl.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 14)
        self.assertEqual(lines[0], 'template <typename T>\n')
        self.assertEqual(lines[1], 'class MyClass : public std::string {\n')
        self.assertEqual(lines[2], '    public:\n')
        self.assertEqual(lines[3], '    MyClass() :\n')
        self.assertEqual(lines[4], '        std::string() {\n')
        self.assertEqual(lines[5], '\n')
        self.assertEqual(lines[6], '    } // MyClass()\n')
        self.assertEqual(lines[7], '\n')
        self.assertEqual(lines[8], '\n')
        self.assertEqual(lines[9], '    private:\n')
        self.assertEqual(lines[10], '    int pippo;\n')
        self.assertEqual(lines[11], '\n')
        self.assertEqual(lines[12], '}; // class MyClass\n')

    def testTemplateImpl(self):
        intDecl = cxx_writer.intType
        stringDecl = cxx_writer.stringType
        privateVar = cxx_writer.Attribute('pippo', intDecl, 'pri')
        emptyBody = cxx_writer.Code('')
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu', [], ['std::string()'])
        classDecl = cxx_writer.ClassDeclaration('MyClass', [privateVar], [stringDecl], ['T'])
        classDecl.addConstructor(publicConstr)
        classDecl.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 0)

    def testStaticAttrsDecl(self):
        intDecl = cxx_writer.intType
        stringDecl = cxx_writer.stringType
        privateVar = cxx_writer.Attribute('pippo', intDecl, 'pri', True, '0')
        emptyBody = cxx_writer.Code('')
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu', [], ['std::string()'])
        classDecl = cxx_writer.ClassDeclaration('MyClass', [privateVar], [stringDecl])
        classDecl.addConstructor(publicConstr)
        classDecl.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 9)
        self.assertEqual(lines[0], 'class MyClass : public std::string {\n')
        self.assertEqual(lines[1], '    public:\n')
        self.assertEqual(lines[2], '    MyClass();\n')
        self.assertEqual(lines[3], '\n')
        self.assertEqual(lines[4], '    private:\n')
        self.assertEqual(lines[5], '    static int pippo;\n')
        self.assertEqual(lines[6], '\n')
        self.assertEqual(lines[7], '}; // class MyClass\n')

    def testStaticAttrsImpl(self):
        intDecl = cxx_writer.intType
        privateVar = cxx_writer.Attribute('pippo', intDecl, 'pri', True, '0')
        emptyBody = cxx_writer.Code('')
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu', [], ['std::string()'])
        classDecl = cxx_writer.ClassDeclaration('MyClass', [privateVar])
        classDecl.addConstructor(publicConstr)
        classDecl.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 6)
        self.assertEqual(lines[0], 'int MyClass::pippo = 0;\n')
        self.assertEqual(lines[1], 'MyClass::MyClass() :\n')
        self.assertEqual(lines[2], '    std::string() {\n')
        self.assertEqual(lines[3], '\n')
        self.assertEqual(lines[4], '} // MyClass()\n')

    def testSCModuleDecl(self):
        intDecl = cxx_writer.intType
        stringDecl = cxx_writer.stringType
        module_nameDecl = cxx_writer.sc_module_nameType
        privateVar = cxx_writer.Attribute('pippo', intDecl, 'pri')
        emptyBody = cxx_writer.Code('end_module();')
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu', [cxx_writer.Parameter('name', module_nameDecl)], ['std::string()'])
        classDecl = cxx_writer.SCModule('MyClass', [privateVar], [stringDecl])
        classDecl.addConstructor(publicConstr)
        classDecl.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 10)
        self.assertEqual(lines[0], 'class MyClass : public std::string, public sc_module {\n')
        self.assertEqual(lines[1], '    public:\n')
        self.assertEqual(lines[2], '    SC_HAS_PROCESS(MyClass);\n')
        self.assertEqual(lines[3], '    MyClass(sc_module_name name);\n')
        self.assertEqual(lines[4], '\n')
        self.assertEqual(lines[5], '    private:\n')
        self.assertEqual(lines[6], '    int pippo;\n')
        self.assertEqual(lines[7], '\n')
        self.assertEqual(lines[8], '}; // class MyClass\n')

    def testSCModuleImpl(self):
        intDecl = cxx_writer.intType
        stringDecl = cxx_writer.stringType
        module_nameDecl = cxx_writer.sc_module_nameType
        privateVar = cxx_writer.Attribute('pippo', intDecl, 'pri')
        emptyBody = cxx_writer.Code('end_module();')
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu', [cxx_writer.Parameter('name', module_nameDecl)], ['std::string()', 'sc_module(name)'])
        classDecl = cxx_writer.SCModule('MyClass', [privateVar], [stringDecl])
        classDecl.addConstructor(publicConstr)
        classDecl.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 6)
        self.assertEqual(lines[0], 'MyClass::MyClass(sc_module_name name) :\n')
        self.assertEqual(lines[1], '    std::string(),\n')
        self.assertEqual(lines[2], '    sc_module(name) {\n')
        self.assertEqual(lines[3], '    end_module();\n')
        self.assertEqual(lines[4], '} // MyClass()\n')

    def testInlineMethodDecl(self):
        intDecl = cxx_writer.intType
        emptyBody = cxx_writer.Code('')
        inlineMethod = cxx_writer.Method('pippo', emptyBody, intDecl, 'pri', [], False, True)
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu')
        classDecl = cxx_writer.ClassDeclaration('MyClass', [inlineMethod])
        classDecl.addConstructor(publicConstr)
        classDecl.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 11)
        self.assertEqual(lines[0], 'class MyClass {\n')
        self.assertEqual(lines[1], '    public:\n')
        self.assertEqual(lines[2], '    MyClass();\n')
        self.assertEqual(lines[3], '\n')
        self.assertEqual(lines[4], '    private:\n')
        self.assertEqual(lines[5], '    inline int pippo() {\n')
        self.assertEqual(lines[6], '\n')
        self.assertEqual(lines[7], '    } // pippo()\n')
        self.assertEqual(lines[8], '\n')
        self.assertEqual(lines[9], '}; // class MyClass\n')

    def testInlineMethodImpl(self):
        intDecl = cxx_writer.intType
        emptyBody = cxx_writer.Code('')
        inlineMethod = cxx_writer.Method('pippo', emptyBody, intDecl, 'pri', [], False, True)
        publicConstr = cxx_writer.Constructor(emptyBody, 'pu')
        classDecl = cxx_writer.ClassDeclaration('MyClass', [inlineMethod])
        classDecl.addConstructor(publicConstr)
        classDecl.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 4)
        self.assertEqual(lines[0], 'MyClass::MyClass() {\n')
        self.assertEqual(lines[1], '\n')
        self.assertEqual(lines[2], '} // MyClass()\n')
