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
            raise Exception('A minimum line length of at least 20 characters should be specified')
        self.lineWidth = lineWidth

    def __del__(self):
        if self.opened:
            self.file.close()

    def write(self, code, split = ' ', indent = -1, comment = ''):
        """(After/Before) each delimiter start ({) I have to increment
        the size of the current indent. Before each delimiter
        end (}) I have to decrement it
        Note that after each newline I have to print the current indenting"""
        self.codeBuffer += code.expandtabs(self.indentSize)
        if not '\n' in code:
            return
        if (indent == -1):
            indent = self.curIndent
            force = False
        else: force = True
        for line in self.codeBuffer.split('\n')[:-1]:
            line = line.strip()
            # I check if it is the case to unindent
            if (line.endswith('}') or line.startswith('}')) and self.curIndent >= 1:
                self.curIndent -= 1
                indent -= 1
            # Now I print the current line, making sure that It is not too long
            # in case I send it to a new line
            if line:
                # [tadros]: Moved the calculation of the indenting out of the
                # recursive go_new_line for efficiency.
                singleIndent = ''
                totalIndent = ''
                for i in range(0, self.indentSize):
                    singleIndent += ' '
                if (not line.startswith('#')):
                    for i in range(0, indent * self.indentSize):
                        self.file.write(' ')
                        totalIndent += ' '
                printOnFile(self.go_new_line(line, singleIndent, totalIndent, split, force, comment = comment), self.file)
            else:
                printOnFile('', self.file)
            # Finally I compute the nesting level for the next lines
            if line.endswith('{'):
                self.curIndent += 1
                indent += 1
        lastLine = self.codeBuffer.split('\n')[-1]
        if not lastLine.endswith('\n'):
            self.codeBuffer = lastLine

    def go_new_line(self, toModify, singleIndent, totalIndent, split = ' ', force = False, cpp = False, comment = ''):
        """Given a string the function introduces newline characters to respect
        the line width constraint (+/-8 chars)"""

        # Terminal case: Line does not need splitting (up to 8 extra chars)
        toModify = toModify.strip()
        if (len(totalIndent) + len(comment) + len(toModify)) < (self.lineWidth+8):
            return toModify

        # The calling functions should have already split on newlines, so
        # endOfLine should always equal len(toModify).
        endOfLine = toModify.find('\n')
        if endOfLine < 0:
            endOfLine = len(toModify)
        # Check for crazy indentations > linewidth.
        if ((len(totalIndent) + len(comment)) > self.lineWidth):
            self.lineWidth += len(totalIndent) + len(comment)
        # Find the split char nearest to the target line width.
        found = -1
        for i in range(len(totalIndent)+len(comment), len(totalIndent)+len(comment)+8):
            if (toModify[self.lineWidth-i] == split and toModify[self.lineWidth-i-1] != "'"):
                found = self.lineWidth-i
                break
        # If no split char was found, we retry with whitespace.
        if (found == -1 and split != ' '):
            for i in range(len(totalIndent), len(totalIndent)+8):
                # Bizarre case where we split a single-quoted whitespace - really happened!
                if (toModify[self.lineWidth-i] == ' ' and toModify[self.lineWidth-i-1] != "'"):
                    found = self.lineWidth-i
                    break
        # If we still found nothing, we search upwards from linewidth.
        if (found == -1):
            for i in range(self.lineWidth-len(totalIndent)+1, endOfLine):
                if (toModify[i] == split and toModify[i-1] != "'"):
                    found = i
                    break
        # If still no split char was found, we retry with whitespace.
        if (found == -1 and split != ' '):
            for i in range(self.lineWidth-len(totalIndent)+1, endOfLine):
                # Bizarre case where we split a single-quoted whitespace - really happened!
                if (toModify[i] == ' ' and toModify[i-1] != "'"):
                    found = i
                    break
        # Recursion
        if (found != -1):
          insert_pre = ''
          insert_post = comment
          # Special case: Add line continuation chars if splitting preprocessing code.
          if (toModify.startswith('#')):
              cpp = True
              insert_pre = ' \\'
          # Special case: Add line continuation chars if splitting strings.
          elif ((toModify.count('"', 0, found) % 2) == 1):
              insert_pre = '\\'
          # Special case: Align and add prefixes (here: *) for doxygen-style comments.
          elif comment == '':
              if (toModify.startswith('/**')):
                  force = True
                  insert_post = '*             '
              elif (toModify.startswith('///')):
                  force = True
                  insert_post = '/// '
              elif (toModify.startswith('//')):
                  force = True
                  insert_post = '// '

          # Add one level of indentation to all lines after the first, unless the
          # indentation is forced (aligned list of parameters or comments).
          if comment != '': force = True
          if (force == False):
              totalIndent += singleIndent
              force = True
          # If the split char is not whitespace, make sure we don't delete it!
          if (toModify[found] != ' '): insert_pre = toModify[found] + insert_pre
          return toModify[:found] + insert_pre + '\n' + totalIndent + insert_post + self.go_new_line(toModify[(found+1):endOfLine], singleIndent, totalIndent, split, force, cpp, insert_post)
        else:
            return toModify


    def flush(self):
        self.file.write(self.codeBuffer)
        self.codeBuffer = ''
        self.file.flush()
