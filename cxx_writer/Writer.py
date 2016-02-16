################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     Writer.py
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

def printOnFile(line, destFile):
    """Function for printing on file; printing on file has been enclosed
    in this function so that this is the only function that needs to be
    replaced for the transition of python 3.0"""
    print >> destFile, line

class StringWriter:
    def __init__(self):
        self.code = ''

    def write(self, code):
        self.code += code

    def __str__(self):
        return self.code

class CodeWriter:
    """This class is simply used to write strings to an output file; the added value is that we do not
    need to care about indenting since this is automatically managed"""

    def __init__(self, file, indentSize = 2, lineWidth = 80):
        if type(file) == type(''):
            self.file = open(file, 'at')
            self.opened = True
        else:
            file.flush()
            self.file = file
            self.opened = False
        self.curIndent = 0
        self.indentSize = indentSize
        self.codeBuffer = ''
        if lineWidth < 20:
            raise Exception('Specify a minimum line length of at least 20 characters.')
        self.lineWidth = lineWidth

    def __del__(self):
        if self.opened:
            self.file.close()

    def write(self, code, indent = -1, split = ' ', prefix = ''):
        """(After/Before) each delimiter start ({) I have to increment
        the size of the current indent. Before each delimiter
        end (}) I have to decrement it
        Note that after each newline I have to print the current indenting"""
        self.codeBuffer += code.expandtabs(self.indentSize)
        if not '\n' in code: return

        iLine = 0
        for line in self.codeBuffer.split('\n')[:-1]:
            line = line.strip()
            # Calculate current nesting level.
            if ((line.endswith('}') and not line.endswith('@}')) or line.startswith('}')) and self.curIndent >= 1:
                self.curIndent -= 1

            # Add prefix also after newlines.
            if iLine > 0 and prefix != '' and line != '\n':
                line = prefix + line

            # Print current line. writeLine() takes care of additional/forced
            # indentation, line-splitting and line-prefixes.
            if line:
                # Indent calculations
                curIndent = ''
                if indent != -1:
                    for i in range(0, indent * self.indentSize):
                        curIndent += ' '
                elif not line.startswith('#'):
                    for i in range(0, self.curIndent * self.indentSize):
                        curIndent += ' '
                printOnFile(self.writeLine(curIndent + line, indent = indent, split = split, prefix = prefix), self.file)
            else:
                printOnFile('', self.file)

            # Calculate subsequent nesting level.
            if line.endswith('{') and not line.endswith('@{'):
                self.curIndent += 1

            iLine = iLine + 1

        lastLine = self.codeBuffer.split('\n')[-1]
        if not lastLine.endswith('\n'):
            self.codeBuffer = lastLine

    def writeLine(self, line, indent, split = ' ', prefix = '', postfix = ''):
        """Given a string the function introduces newline characters to respect
        the line width constraint."""

        # Indent calculations
        curIndent = ''
        if indent != -1:
            for i in range(0, indent * self.indentSize):
                curIndent += ' '
        elif not line.startswith('#'):
            for i in range(0, self.curIndent * self.indentSize):
                curIndent += ' '

        # Line width calculations
        # Check for crazy indentations > lineWidth.
        lineWidth = self.lineWidth
        if ((len(curIndent) + len(prefix)) > self.lineWidth):
            lineWidth += len(curIndent) + len(prefix)
        # The calling functions should have already split on newlines, so
        # endOfLine should always equal len(line).
        endOfLine = line.find('\n')
        if endOfLine < 0:
            endOfLine = len(line)

        # Terminal case: Line does not need splitting.
        if (len(line) <= lineWidth):
            return line

        # Find the split char nearest to the target line width.
        found = -1
        for i in range(0, 8):
            if (lineWidth-i)>0:
                if (line[lineWidth-i] == split and line[lineWidth-i-1] != "'"):
                    found = lineWidth-i
                    break
        # If no split char was found, we retry with whitespace.
        if (found == -1 and split != ' '):
            for i in range(0, 8):
                if (lineWidth-i)>0:
                    # Bizarre case where we split a single-quoted whitespace - really happened!
                    if (line[lineWidth-i] == ' ' and line[lineWidth-i-1] != "'"):
                        found = lineWidth-i
                        break
        # If we still found nothing, we search upwards from lineWidth.
        if (found == -1):
            for i in range(lineWidth+1, endOfLine):
                if (line[i] == split and line[i-1] != "'"):
                    found = i
                    break
        # If still no split char was found, we retry with whitespace.
        if (found == -1 and split != ' '):
            for i in range(lineWidth+1, endOfLine):
                # Bizarre case where we split a single-quoted whitespace - really happened!
                if (line[i] == ' ' and line[i-1] != "'"):
                    found = i
                    break

        # Terminal case: Cannot split.
        if (found == -1):
            return line

        # This line's postfix
        if postfix == '':
            # Special case: Add line continuation chars if splitting preprocessing code.
            if line.startswith('#'):
                postfix = ' \\'
            # Special case: Add line continuation chars if splitting strings.
            elif ((line.count('"', 0, found) % 2) == 1):
                postfix = '\\'
        curPostfix = postfix
        # If the split char is not whitespace, make sure we don't delete it!
        if (line[found] != ' '): curPostfix = line[found] + curPostfix

        # Next line's prefix
        # Special case: Align and add prefixes if splitting doxygen-style comments.
        if prefix == '':
            if (line.strip().startswith('/**')):
                prefix = ' *  '
            elif (line.strip().startswith('///')):
                prefix = '/// '
            elif (line.strip().startswith('//')):
                prefix = '// '
            # Add one level of indentation to all lines after the first, unless the
            # indentation is forced (aligned list of parameters or comments).
            elif (indent == ''):
                for i in range(0, self.indentSize):
                    curIndent += ' '
        curPrefix = curIndent + prefix

        # Recursion
        return line[:found] + curPostfix + '\n' + self.writeLine(curPrefix + line[(found+1):endOfLine].strip(), indent = indent, split = split, prefix = prefix, postfix = postfix)

    def writeFill(self, fill = '*'):
        if self.codeBuffer != '':
            write('\n')
        line = ''
        for i in range(0, self.curIndent * self.indentSize):
            line += ' '
        line += '/// '
        lenFill = (self.lineWidth - len(line)) / len(fill)
        for i in range(0, lenFill):
            line += fill
        printOnFile(line, self.file)

    def flush(self):
        self.file.write(self.codeBuffer)
        self.codeBuffer = ''
        self.file.flush()
