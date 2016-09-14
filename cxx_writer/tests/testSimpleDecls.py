################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     testSimpleDecls.py
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
            print ('Please specify location of core TRAP files in testSimpleDecls.py.')

import unittest
import os

class TestSimpleDecls(unittest.TestCase):
    def setUp(self):
        try:
            os.remove('prova.cpp')
        except:
            pass
        self.writer = cxx_writer.CodeWriter('prova.cpp', indentSize = 4, lineWidth = 80)

    def tearDown(self):
        del self.writer
        os.remove('prova.cpp')

    def testSimpleTemplateType(self):
        innerType = cxx_writer.stringType
        templ = cxx_writer.TemplateType('std::vector', [innerType])
        templ.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 1)
        self.assertEqual(lines[0], 'std::vector<std::string>')

    def testDoubleTemplateType(self):
        innerType1 = cxx_writer.stringType
        innerType2 = cxx_writer.intType
        templ = cxx_writer.TemplateType('std::map', [innerType1, innerType2])
        templ.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 1)
        self.assertEqual(lines[0], 'std::map<std::string, int>')

    def testNestedTemplateType(self):
        innerType1 = cxx_writer.stringType
        innerType2 = cxx_writer.intType
        innerType3 = cxx_writer.doubleType
        templ1 = cxx_writer.TemplateType('std::map', [innerType2, innerType3])
        templ2 = cxx_writer.TemplateType('std::map', [innerType1, templ1])
        templ2.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 1)
        self.assertEqual(lines[0], 'std::map<std::string, std::map<int, double> >')

    def testSimpleVariable(self):
        type = cxx_writer.stringType
        var = cxx_writer.Variable('pippo', type)
        var.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 0)

    def testVariableInit(self):
        type = cxx_writer.stringType
        var = cxx_writer.Variable('pippo', type, False, '\"pippa\"')
        var.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 0)

    def testTemplatedVariable(self):
        innerType1 = cxx_writer.stringType
        innerType2 = cxx_writer.intType
        innerType3 = cxx_writer.doubleType
        templ1 = cxx_writer.TemplateType('std::map', [innerType2, innerType3])
        type = cxx_writer.TemplateType('std::map', [innerType1, templ1])
        var = cxx_writer.Variable('pippo', type)
        var.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 0)

    def testEnum(self):
        enumInst = cxx_writer.Enum('myEnum', {'ONE':1, 'TWO':2, 'THREE':3})
        enumInst.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 5)
        self.assertEqual(lines[0], 'enum myEnum {\n')
        self.assertEqual(lines[1], '    THREE = 3\n')
        self.assertEqual(lines[2], '    ,TWO = 2\n')
        self.assertEqual(lines[3], '    ,ONE = 1\n')
        self.assertEqual(lines[4], '};\n')

    def testUnion(self):
        unionInst = cxx_writer.Union('myUnion')
        type = cxx_writer.stringType
        var = cxx_writer.Variable('pippo', type)
        unionInst.addMember(var)
        type = cxx_writer.intType
        var = cxx_writer.Variable('duck', type)
        unionInst.addMember(var)
        unionInst.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 4)
        self.assertEqual(lines[0], 'union myUnion {\n')
        self.assertEqual(lines[1], '    std::string pippo;\n')
        self.assertEqual(lines[2], '    int duck;\n')
        self.assertEqual(lines[3], '};\n')

    def testTypedef(self):
        type = cxx_writer.intType
        typedef = cxx_writer.Typedef('duck', type)
        typedef.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 1)
        self.assertEqual(lines[0], 'typedef duck int;\n')

    def testSimpleFunction(self):
        code = cxx_writer.Code('printf(\"Wow\");')
        function = cxx_writer.Function('dummy_func', code)
        function.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 3)
        self.assertEqual(lines[0], 'void dummy_func() {\n')
        self.assertEqual(lines[1], '    printf(\"Wow\");\n')
        self.assertEqual(lines[2], '} // dummy_func()\n')

    def testReturnFunction(self):
        code = cxx_writer.Code('if (works) {\nprintf(\"hummm\\n\");\nreturn 1;\n} else {\nreturn 0;\n}')
        retType = cxx_writer.intType
        function = cxx_writer.Function('dummy_func', code, retType)
        function.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 8)
        self.assertEqual(lines[0], 'int dummy_func() {\n')
        self.assertEqual(lines[1], '    if (works) {\n')
        self.assertEqual(lines[2], '        printf(\"hummm\\n\");\n')
        self.assertEqual(lines[3], '        return 1;\n')
        self.assertEqual(lines[4], '    } else {\n')
        self.assertEqual(lines[5], '        return 0;\n')
        self.assertEqual(lines[6], '    }\n')
        self.assertEqual(lines[7], '} // dummy_func()\n')

    def testParameterFunction(self):
        code = cxx_writer.Code('if (works) {\nprintf(\"hummm\\n\");\nreturn 1;\n} else {\nreturn 0;\n}')
        intType = cxx_writer.intType
        parameters = [cxx_writer.Parameter('param1', intType)]
        function = cxx_writer.Function('dummy_func', code, intType, parameters)
        function.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 8)
        self.assertEqual(lines[0], 'int dummy_func(int param1) {\n')
        self.assertEqual(lines[1], '    if (works) {\n')
        self.assertEqual(lines[2], '        printf(\"hummm\\n\");\n')
        self.assertEqual(lines[3], '        return 1;\n')
        self.assertEqual(lines[4], '    } else {\n')
        self.assertEqual(lines[5], '        return 0;\n')
        self.assertEqual(lines[6], '    }\n')
        self.assertEqual(lines[7], '} // dummy_func()\n')

    def testTemplateFunction(self):
        code = cxx_writer.Code('if (works) {\nprintf(\"hummm\\n\");\nreturn 1;\n} else {\nreturn 0;\n}')
        intType = cxx_writer.intType
        parameters = [cxx_writer.Parameter('param1', intType)]
        function = cxx_writer.Function('dummy_func', code, intType, parameters, template = ['A'])
        function.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 8)
        self.assertEqual(lines[0], 'template <typename A> int dummy_func(int param1) {\n')
        self.assertEqual(lines[1], '    if (works) {\n')
        self.assertEqual(lines[2], '        printf(\"hummm\\n\");\n')
        self.assertEqual(lines[3], '        return 1;\n')
        self.assertEqual(lines[4], '    } else {\n')
        self.assertEqual(lines[5], '        return 0;\n')
        self.assertEqual(lines[6], '    }\n')
        self.assertEqual(lines[7], '} // dummy_func()\n')

    def testInlineFunction(self):
        code = cxx_writer.Code('if (works) {\nprintf(\"hummm\\n\");\nreturn 1;\n} else {\nreturn 0;\n}')
        intType = cxx_writer.intType
        parameters = [cxx_writer.Parameter('param1', intType)]
        function = cxx_writer.Function('dummy_func', code, intType, parameters, inline = True)
        function.writeDeclaration(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 8)
        self.assertEqual(lines[0], 'inline int dummy_func(int param1) {\n')
        self.assertEqual(lines[1], '    if (works) {\n')
        self.assertEqual(lines[2], '        printf(\"hummm\\n\");\n')
        self.assertEqual(lines[3], '        return 1;\n')
        self.assertEqual(lines[4], '    } else {\n')
        self.assertEqual(lines[5], '        return 0;\n')
        self.assertEqual(lines[6], '    }\n')
        self.assertEqual(lines[7], '} // dummy_func()\n')

    def testFunctionDoc(self):
        intType = cxx_writer.intType
        code = cxx_writer.Code('')
        parameters = [cxx_writer.Parameter('param1', intType)]
        function = cxx_writer.Function('dummy_func', code, intType, parameters)
        function.addDocString('Documentation test\nanother line\n')
        function.writeImplementation(self.writer)
        self.writer.flush()
        testFile = open('prova.cpp', 'r')
        lines = testFile.readlines()
        testFile.close()
        self.assertEqual(len(lines), 5)
        self.assertEqual(lines[0], '/// Documentation test\n')
        self.assertEqual(lines[1], '/// another line\n')
        self.assertEqual(lines[2], 'int dummy_func(int param1) {\n')
        self.assertEqual(lines[3], '\n')
        self.assertEqual(lines[4], '} // dummy_func()\n')

################################################################################
