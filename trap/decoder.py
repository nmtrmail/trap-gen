################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     decoder.py
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

try:
    import networkx as NX
except:
    import traceback
    traceback.print_exc()
    raise Exception('Cannot import required module networkx. Please install networkx >= 0.36.')

try:
    import re
    p = re.compile( '([a-z]|[A-Z])*')
    nxVersion = p.sub('', NX.__version__)
    if nxVersion.count('.') > 1:
        nxVersion = '.'.join(nxVersion.split('.')[:2])
    nxVersion = float(nxVersion)
except:
    import traceback
    traceback.print_exc()
    raise Exception('Cannot determine networkx version. Try changing versions >= 0.36.')

import operator

################################################################################
# Globals and Helpers
################################################################################
def expandPatterns(curPattern, genericPattern, tablePattern):
    """Recursively computes all possible strings (curPattern) conforming to an
    instruction (genericPattern) masked by some mask (tablePattern):
    genericPattern is the instruction coding {(0,1,x)} and tablePattern is the
    mask {(0,1): 0=don't-care && 1=test}. If the set-bits in the mask {1} are
    also set in the instruction {0,1}, instr & mask is returned. If the mask
    tests don't-care bits, a list of curPattern is returned, where a result is
    generated for each possible outcome. Example:
    genericPattern = 10x1, tablePattern = 1110, curPattern = (100x, 101x)
    Note that the number of returned patterns is 2^dont't-care-bits."""
    if len(curPattern) < len(genericPattern):
        if tablePattern[len(curPattern)] == 1:
            # Bit is set by mask but not defined by instruction:
            # Return both possibilities.
            if genericPattern[len(curPattern)] == None:
                return expandPatterns(curPattern + [1], genericPattern, tablePattern) + expandPatterns(curPattern + [0], genericPattern, tablePattern)
            # Bit is defined by instruction and set by mask:
            # Return bit value in instruction.
            else:
                return expandPatterns(curPattern + [genericPattern[len(curPattern)]], genericPattern, tablePattern)
        # Bit is not set by mask:
        # Return don't-care whether it is defined by the instruction or not.
        else:
            return expandPatterns(curPattern + [None], genericPattern, tablePattern)
    else:
        return [curPattern]

def bitStringUnion(bitString, noCare = None):
    """Computes the union of a list of bitstrings using three-state logic.
    This is stronger than bitStringValid: The latter only requires that a bit is
    set (0/1) in all patterns, while this one requires that the bit is set to
    the same value in all patterns."""
    maxLen = 0
    for curPattern in bitString:
        if len(curPattern) > maxLen:
            maxLen = len(curPattern)
    paddedbitString = []
    for curPattern in bitString:
        if len(curPattern) < maxLen:
            paddedbitString.append(list(curPattern) + [None for i in range(len(curPattern), maxLen)])
        else:
            paddedbitString.append(curPattern)
    validPattern = []
    for curPattern in paddedbitString:
        for i in range(0, len(curPattern)):
            if len(validPattern) > i:
                if validPattern[i] != curPattern[i]:
                    validPattern[i] = noCare
            else:
                validPattern.append(curPattern[i])
    return validPattern

def bitStringValid(bitStrings, noCare = None):
    """Given a list of bitstring it computes which bits are not don't-care in
    all bitstrings. This bit is given 1. This is weaker than bitStringUnion:
    This only requires that a bit is set (0/1) in ALL patterns, while the latter
    requires that the bit is set AND equal."""
    validPattern = []
    for curPattern in bitStrings:
        for i in range(0, len(curPattern)):
            if len(validPattern) > i:
                if validPattern[i] is None or curPattern[i] is None:
                    validPattern[i] = noCare
            else:
                if curPattern[i] != None:
                    validPattern.append(1)
                else:
                    validPattern.append(noCare)
    return validPattern

def patternLen(pattern):
    """Determines the maximum length of a list of patterns (bit strings)."""
    maxLen = 0
    for i in pattern:
        if len(i) > maxLen:
            maxLen = len(i)
    return maxLen

class SplitFunction:
    """Defines the split function of a decoding node. It can either be a table
    or a pattern function."""
    def __init__(self, pattern = None, table = None):
        self.pattern = pattern
        self.table = table

    def toCode(self):
        mask = ''
        value = ''
        if self.pattern:
            for i in reversed(self.pattern):
                if i !=  None:
                    value += str(i)
                    mask += '1'
                else:
                    value += '0'
                    mask += '0'
            return (hex(int(mask, 2)), hex(int(value, 2)))
        if self.table:
            for i in reversed(self.table):
                mask += str(i)
            return hex(int(mask, 2))

    def __repr__(self):
        ret_val = ''
        if self.pattern:
            for i in reversed(self.pattern):
                if i !=  None:
                    ret_val += str(i)
                else:
                    ret_val += '-'
            return ret_val
        if self.table:
            for i in reversed(self.table):
                if i !=  0:
                    ret_val += str(i)
                else:
                    ret_val += '-'
            return ret_val

    def __str__(self):
        return repr(self)

################################################################################
# Decoder Node
################################################################################
class DecodingNode:
    def __init__(self, patterns):
        self.patterns = patterns
        self.splitFunction = None
        self.instrId = None

    def __cmp__(self, other):
        """Returns the outcome of the comparison between two instances of the
        current object."""
        if list(self.patterns) == list(other.patterns) and self.splitFunction == other.splitFunction and self.instrId == other.instrId:
            return 0
        if len(self.patterns) < len(other.patterns):
            return -1
        return 1

    def __eq__(self, other):
        """Returns the outcome of the comparison between two instances of the
        current object."""
        if not isinstance(other, type(self)):
            return False
        return list(self.patterns) == list(other.patterns) and self.splitFunction == other.splitFunction and self.instrId == other.instrId

    def __ne__(self, other):
        """Returns the outcome of the comparison between two instances of the
        current object."""
        if not isinstance(other, type(self)):
            return True
        return not self == other

    def __hash__(self):
        """Returns a hash of the object, required for the utilization as a graph
        node."""
        return hash(str(self.patterns))

    def __repr__(self):
        ret_val = ''
        if self.instrId != None:
            ret_val += 'id=' + str(self.instrId) + ' ** '
        if self.splitFunction:
            ret_val += str(self.splitFunction) + ' ** '
        # Compute a summary of the patterns associated with this node and add it
        # to the representation.
        validPattern = bitStringUnion([i[0] for i in reversed(self.patterns)], 'x')
        for i in validPattern:
            if i !=  None:
                ret_val += str(i)
            else:
                ret_val += '-'
        return ret_val + ''

    def __str__(self):
        """Returns a representation of the current node in the dot notation."""
        return repr(self)

class HuffmanNode:
    def __init__(self, frequency, count = 1):
        self.frequency = frequency
        self.count = count
    def __repr__(self):
        return str(self.frequency) + ' -- ' + str(self.count)
    def __str__(self):
        return repr(self)

################################################################################
# Decoder
################################################################################
class decoderCreator:
    """Taking as input the different instructions with their associated
    bitstrings. Creates the corresponding decoder that can associate any
    bitstring with the correct instruction.
    The decoder is created according to the algorithm described by Qin and Malik
    in Automated Synthesis of Efficient Binary Decodes for Retargetable Software
    Toolkits."""

    def __init__(self, instructions, subInstructions, memPenaltyFactor = 2):
        """memPenaltyFactor represent how much the heuristic has to take into
        account memory consumption: The lower the value, the more memory is
        consumed by the created decoder."""
        self.memPenaltyFactor = memPenaltyFactor
        self.instrId = {}
        self.instrName = {}
        self.instrSub = {}
        self.instrPattern = []
        self.invalid_instr = None
        # Compute the probabilities of the instructions given their frequencies.
        self.minFreq = 0
        self.totalCount = 1
        if instructions:
            self.minFreq = instructions.values()[0].frequency
            self.totalCount = 0
        for instr in instructions.values():
            self.totalCount += instr.frequency
            if instr.frequency < self.minFreq:
                self.minFreq = instr.frequency
        self.instrNum = len(instructions)
        # Note how the most significant bit of the bitstring is the first one of
        # instr.bitstring. In order to correctly perform the computation, the
        # bistring has to be reversed. At the end, when it is time to print the
        # decoder into C++ code, the patterns are reversed back.
        for name, instr in instructions.items():
            if not name in subInstructions.keys():
                revBitstring = list(instr.bitstring)
                revBitstring.reverse()
                self.instrName[instr.id] = name
                self.instrSub[instr.id] = instr.subInstructions
                self.instrId[instr.id] = (revBitstring, float(instr.frequency)/float(self.totalCount))
                self.instrPattern.append((revBitstring, float(instr.frequency)/float(self.totalCount)))
        if nxVersion < 0.99:
            self.decodingTree = NX.XDiGraph()
        else:
            self.decodingTree = NX.DiGraph()
        self.computeIllegalBistreams()
        self.computeDecoder()

    def getSubInstrCode(self, subInstructions):
        code = ''
        for instr in subInstructions:
            revBitstring = list(instr.bitstring)
            mask = ''
            value = ''
            for i in revBitstring:
                if i == None:
                    mask += '0'
                else:
                    mask += '1'
                if i == 1:
                    value += '1'
                else:
                    value += '0'
            code += 'if ((instr_code & ' + hex(int(mask, 2)) + 'U) == ' + hex(int(value, 2)) + ') {\n// Instruction ' + instr.name + '\nreturn ' + str(instr.id) + ';\n}\n'
        return code

    def createPatternDecoder(self, subtree):
        if subtree.instrId:
            if subtree.instrId != -1:
                return self.getSubInstrCode(self.instrSub[instrId]) + '// Instruction ' + self.instrName[subtree.instrId] + '\nreturn ' + str(subtree.instrId) + ';\n'
            else:
                if self.invalid_instr:
                    return '// Invalid pattern\nreturn ' + str(self.invalid_instr.id) + ';\n'
                else:
                    return '// Invalid pattern\nreturn ' + str(self.instrNum) + ';\n'
        if self.decodingTree.out_degree(subtree) != 2:
            raise Exception('Invalid number of out edges ' + str(self.decodingTree.out_degree(subtree)) + ' for subtree ' + str(subtree) + ', expected two.')
        if nxVersion < 0.99:
            outEdges = self.decodingTree.edges(subtree)
        else:
            outEdges = self.decodingTree.edges(subtree, data = True)
        (mask, value) = subtree.splitFunction.toCode()
        if nxVersion > 0.99:
            decodePatternZero = outEdges[0][-1]['decodePattern']
            decodePatternOne = outEdges[1][-1]['decodePattern']
        else:
            decodePatternZero = outEdges[0][-1]
            decodePatternOne = outEdges[1][-1]

        if decodePatternZero[-1] > decodePatternOne[-1]:
            nodeIf = outEdges[0][1]
            nodeElse = outEdges[1][1]
            if decodePatternZero[0] == 1:
                compareFun = '=='
            else:
                compareFun = '!='
        else:
            nodeIf = outEdges[1][1]
            nodeElse = outEdges[0][1]
            if decodePatternOne[0] == 1:
                compareFun = '=='
            else:
                compareFun = '!='
        code = 'if ((instr_code & ' + mask + 'U) ' + compareFun + ' ' + value + ') {\n'
        if nodeIf.instrId != None:
            if nodeIf.instrId != -1:
                #code += '\n' + str(nodeIf.patterns) + '\n'
                code += self.getSubInstrCode(self.instrSub[nodeIf.instrId]) + '// Instruction ' + self.instrName[nodeIf.instrId] + '\nreturn ' + str(nodeIf.instrId) + ';\n'
            else:
                if self.invalid_instr:
                    code += '// Invalid pattern\nreturn ' + str(self.invalid_instr.id) + ';\n'
                else:
                    code += '// Invalid pattern\nreturn ' + str(self.instrNum) + ';\n'
        elif nodeIf.splitFunction.pattern:
            #code += '\n' + str(nodeIf.patterns) + '\n'
            code += self.createPatternDecoder(nodeIf)
        else:
            #code += '\n' + str(nodeIf.patterns) + '\n'
            code += self.createTableDecoder(nodeIf)
        code += '} else {\n'
        if nodeElse.instrId != None:
            if nodeElse.instrId != -1:
                #code += '\n' + str(nodeElse.patterns) + '\n'
                code += self.getSubInstrCode(self.instrSub[nodeElse.instrId]) + '// Instruction ' + self.instrName[nodeElse.instrId] + '\nreturn ' + str(nodeElse.instrId) + ';\n'
            else:
                if self.invalid_instr:
                    code += '// Non-valid pattern\nreturn ' + str(self.invalid_instr.id) + ';\n'
                else:
                    code += '// Non-valid pattern\nreturn ' + str(self.instrNum) + ';\n'
        elif nodeElse.splitFunction.pattern:
            #code += '\n' + str(nodeElse.patterns) + '\n'
            code += self.createPatternDecoder(nodeElse)
        else:
            #code += '\n' + str(nodeElse.patterns) + '\n'
            code += self.createTableDecoder(nodeElse)
        code += '}\n'
        return code

    def createTableDecoder(self, subtree):
        if subtree.instrId:
            if subtree.instrId != -1:
                return self.getSubInstrCode(self.instrSub[instrId]) + '// Instruction ' + self.instrName[subtree.instrId] + '\nreturn ' + str(subtree.instrId) + ';\n'
            else:
                if self.invalid_instr:
                    return '// Invalid pattern\nreturn ' + str(self.invalid_instr.id) + ';\n'
                else:
                    return '// Invalid pattern\nreturn ' + str(self.instrNum) + ';\n'
        if nxVersion < 0.99:
            outEdges = self.decodingTree.edges(subtree)
        else:
            outEdges = self.decodingTree.edges(subtree, data = True)
        mask = subtree.splitFunction.toCode()
        if nxVersion > 0.99:
            outEdges = sorted(outEdges, lambda x, y: cmp(y[-1]['decodePattern'][-1], x[-1]['decodePattern'][-1]))
        else:
            outEdges = sorted(outEdges, lambda x, y: cmp(y[-1][-1], x[-1][-1]))
        hasToDeclareMask = True
        if (len(mask) - 2)*4 == len(subtree.splitFunction.table):
            numFs = 0
            for i in mask[2:]:
                if i == 'f':
                    numFs += 1
                else:
                    break
            if numFs == len(mask) - 2:
                hasToDeclareMask = False
        if hasToDeclareMask:
            code = 'switch(instr_code & ' + mask + 'U) {\n'
        else:
            code = 'switch(instr_code) {\n'
        for edge in outEdges:
            if nxVersion > 0.99:
                decodePattern = edge[-1]['decodePattern'][0]
            else:
                decodePattern = edge[-1][0]
            code += 'case ' + hex(decodePattern) + 'U: {\n'
            if edge[1].instrId != None:
                if edge[1].instrId != -1:
                    #code += '\n' + str(edge[1].patterns) + '\n'
                    code += self.getSubInstrCode(self.instrSub[edge[1].instrId]) + '// Instruction ' + self.instrName[edge[1].instrId] + '\nreturn ' + str(edge[1].instrId) + ';\n'
                else:
                    if self.invalid_instr:
                        code += '// Invalid pattern\nreturn ' + str(self.invalid_instr.id) + ';\n'
                    else:
                        code += '// Invalid pattern\nreturn ' + str(self.instrNum) + ';\n'
            elif edge[1].splitFunction.pattern:
                #code += '\n' + str(edge[1].patterns) + '\n'
                code += self.createPatternDecoder(edge[1])
            else:
                #code += '\n' + str(edge[1].patterns) + '\n'
                code += self.createTableDecoder(edge[1])
            code += 'break;}\n'
        code += 'default: {\n// Invalid pattern\nreturn '
        if self.invalid_instr:
            code += str(self.invalid_instr.id)
        else:
            code += str(self.instrNum)
        code += ';\n}\n}\n'
        return code

    def getCPPDecoder(self, fetchSizeType, instructionCache, namespace = ''):
        """Returns the class representing the decoder."""
        import cxx_writer

        # Go over the decoding tree.
        if self.rootNode.splitFunction.pattern:
            codeString = self.createPatternDecoder(self.rootNode)
        else:
            codeString = self.createTableDecoder(self.rootNode)
        codeString += '// Invalid pattern\nreturn '
        if self.invalid_instr:
            codeString += str(self.invalid_instr.id)
        else:
            codeString += str(self.instrNum)
        codeString += ';\n'
        code = cxx_writer.Code(codeString)
        parameters = [cxx_writer.Parameter('instr_code', fetchSizeType)]
        decodeMethod = cxx_writer.Method('decode', code, cxx_writer.intType, 'public', parameters, const = True, noException = True)
        decodeClass = cxx_writer.ClassDeclaration('Decoder', [decodeMethod], namespaces = [namespace])
        decodeClass.addDocString(brief = 'Decoder Class', detail = 'Implements the state-machine that decodes an instruction string and returns an ID specifying the instruction.')

        # Declare the type which shall be contained in the cache.
        if instructionCache:
            emptyBody = cxx_writer.Code('')
            InstructionTypePtr = cxx_writer.Type('Instruction', '#include \"instructions.hpp\"').makePointer()
            instrAttr = cxx_writer.Attribute('instr', InstructionTypePtr, 'public')
            countAttr = cxx_writer.Attribute('count', cxx_writer.uintType, 'public')
            cacheTypeElements = [instrAttr, countAttr]
            cacheType = cxx_writer.ClassDeclaration('CacheElem', cacheTypeElements, namespaces = [namespace])
            instrParam = cxx_writer.Parameter('instr', InstructionTypePtr)
            countParam = cxx_writer.Parameter('count', cxx_writer.uintType)
            cacheTypeConstr = cxx_writer.Constructor(emptyBody, 'public', [instrParam, countParam], ['instr(instr)', 'count(count)'])
            cacheType.addConstructor(cacheTypeConstr)
            emptyCacheTypeConstr = cxx_writer.Constructor(emptyBody, 'public', [], ['instr(NULL)', 'count(1)'])
            cacheType.addConstructor(emptyCacheTypeConstr)

        if instructionCache:
            return [cacheType, decodeClass]
        else:
            return [decodeClass]

    def getCPPDecoderTests(self, namespace = ''):
        """Returns the decoder tests. All the instruction patterns, including
        invalid patterns, are fed to the decoder and the decoding output
        checked. Random values are chosen for don't-care bits."""
        import cxx_writer
        import random, math
        ranGen = random.SystemRandom()
        allTests = []
        testCount = 0
        for instrId, instruction in self.instrId.items():
            code = namespace + '::Decoder dec;\n'
            pattern = instruction[0]
            try:
                pattern[0][0]
                pattern = pattern[0]
            except:
                pass

            for i in range(0, len(pattern)):
                if pattern[i] == None:
                    if ranGen.random() > 0.5:
                        pattern[i] = '1'
                    else:
                        pattern[i] = '0'
                else:
                    pattern[i] = str(pattern[i])
            if instrId == -1:
                expectedId = self.instrNum
            else:
                # Check for the presence of sub-instructions and possibly
                # adapt the expected test outcome.
                expectedId = None
                for instr in self.instrSub[instrId]:
                    found = True
                    revInstrPattern = instr.bitstring
                    revInstrPattern.reverse()
                    for i in range(0, len(pattern)):
                        if revInstrPattern[i] != None and int(pattern[i]) != revInstrPattern[i]:
                            found = False
                    if found:
                        expectedId = instr.id
                        break
                if expectedId == None:
                    expectedId = instrId
            pattern.reverse()
            if instrId != -1:
                code += '// Checking Instruction ' + self.instrName[instrId] + '.\n'
            else:
                code += '// Checking Invalid Instruction.\n'
            code += 'BOOST_CHECK_EQUAL(dec.decode(' + hex(int(''.join(pattern), 2)) + '), ' + str(expectedId) + ');\n'
            curTest = cxx_writer.Code(code)
            curTest.addInclude(['boost/test/test_tools.hpp', 'decoder.hpp'])
            if instrId != -1:
                testName = self.instrName[instrId] + '_' + str(testCount) + '_decode'
            else:
                testName = 'Invalid_' + str(testCount) + '_decode'
            curTestFunction = cxx_writer.Function(testName, curTest, cxx_writer.voidType)
            from procWriter import testNames
            testNames.append(testName)
            allTests.append(curTestFunction)
            testCount += 1
        return allTests

    # Here are some helper methods used in the creation of the decoder; they
    # are called by the constructor
    def computationalCost(self, subtree):
        """Computes the metric for estimating the computational cost of the
        decoding subtree. In this version the height of the Huffman Tree is used
        as an estimation of the cost."""
        # First create the huffman nodes and order them in a list.
        if not subtree.patterns:
            return 0
        huffmanList = []
        for i in subtree.patterns:
            huffmanList.append(HuffmanNode(i[1]))
        huffmanList = sorted(huffmanList, lambda x, y: cmp(x.frequency, y.frequency))
        while len(huffmanList) > 1:
            newElem = HuffmanNode(huffmanList[0].frequency + huffmanList[1].frequency, max(huffmanList[0].count, huffmanList[1].count) + 1)
            huffmanList = huffmanList[2:]
            huffmanList.append(newElem)
            huffmanList = sorted(huffmanList, lambda x, y: cmp(x.frequency, y.frequency))
        return huffmanList[0].count

    def computePatternCost(self, subtree, curMask, newBit, newBitVal):
        """Given the current leaf node, it computes the cost that would derive
        from using the pattern given by curMask + [newBit] for the split of the
        current node. It also phisically performs the split, returning curCost,
        curLeaves, where the leaves are formed by the subtrees and the
        respective splitting criteria (pattern equal or not)."""
        if newBit in curMask[0]:
            raise Exception('Cannot set bit ' + newBit + ' as a new bit. Bit already exists in the current mask.')
        eqPattern = []
        neqPattern = []
        for pattern in subtree.patterns:
            added = False
            for i in range(0, len(curMask[0])):
                if (pattern[0][curMask[0][i]] != None) and (pattern[0][curMask[0][i]] != curMask[1][i]):
                    neqPattern.append(pattern)
                    added = True
                    break
            if not added:
                if (pattern[0][newBit] != None) and (pattern[0][newBit] != newBitVal):
                    neqPattern.append(pattern)
                else:
                    dontCare = False
                    for i in curMask[0]:
                        if pattern[0][i] is None:
                            dontCare = True
                            break
                    if dontCare or (pattern[0][newBit] is None):
                        neqPattern.append(pattern)
                        eqPattern.append(pattern)
                    else:
                        eqPattern.append(pattern)
        # We now have the two split nodes.
        eqSubtree = DecodingNode(eqPattern)
        neqSubtree = DecodingNode(neqPattern)
        eqProb = 1.0
        neqProb = 1.0
        for i in eqPattern:
            eqProb += i[1]
        for i in neqPattern:
            neqProb += i[1]
        memoryCost = float(len(eqPattern) + len(neqPattern) - 1)/float(len(subtree.patterns) -1)
        import math
        cost = 1 + self.memPenaltyFactor*math.log(memoryCost, 2) + (eqProb/(neqProb + eqProb))*self.computationalCost(eqSubtree) + (neqProb/(neqProb + eqProb))*self.computationalCost(neqSubtree)
        if len(eqPattern) == 0 or len(neqPattern) == 0 or len(neqPattern) == len(subtree.patterns) or len(eqPattern) == len(subtree.patterns):
            return (None, None)
        return (cost, ((eqSubtree, (1, eqProb)), (neqSubtree, (0, neqProb))))

    def computeTableCost(self, subtree, startTable, curTableLen):
        """Given the current leaf node, it computes the cost that would derive
        from using the current table function for the split of the current node.
        It also phisically performs the split, returning curCost,
        curLeaves, where the leaves are formed by the subtrees and the
        value of the table. The frequencies of each subtree is also returned."""

        import math
        maxPatternLen = patternLen([i[0] for i in subtree.patterns])
        tablePattern = []
        for i in range(0, maxPatternLen):
            if i >= startTable and i < startTable + curTableLen:
                tablePattern.append(1)
            else:
                tablePattern.append(0)
        leavesPatterns = {}
        for pattern in subtree.patterns:
            # If some of the pattern bits are don't-care, the patterns need to
            # be assigned to all leaves.
            expandedPatterns = expandPatterns([], pattern[0], tablePattern)
            for curPattern in expandedPatterns:
                curTableVal = 0
                for i in range(0, len(tablePattern)):
                    if tablePattern[i] == 1:
                        curTableVal += int(curPattern[i]*math.pow(2, i))
                if leavesPatterns.has_key(curTableVal):
                    leavesPatterns[curTableVal].append(pattern)
                else:
                    leavesPatterns[curTableVal] = [pattern]
        for key, value in leavesPatterns.items():
            if len(value) == len(subtree.patterns):
                return (None, None)
        # We now have the two split nodes.
        cost = 0
        retTuple = []
        probs = {}
        probTotal = 1.0
        memoryCost = 1 + math.pow(2, curTableLen)
        for key, value in leavesPatterns.items():
            for i in value:
                if probs.has_key(key):
                    probs[key] += i[1]
                else:
                    probs[key] = i[1]
                probTotal += i[1]
            memoryCost += len(value) -1
        memoryCost = float(memoryCost + 1.0)/float(len(subtree.patterns) -1)
        import math
        for key, value in leavesPatterns.items():
            curNode = DecodingNode(value)
            cost += (probs[key]/probTotal)*self.computationalCost(curNode)
            retTuple.append((curNode, (key, probs[key])))
        cost += 1 + self.memPenaltyFactor*math.log(memoryCost, 2)
        return (cost, retTuple)

    def findBestPattern(self, subtree):
        """Given a subtree, it finds the best pattern for the split of the top
        node of the subtree. It returns the tuple (bestPattern, bestLeaves,
        bestCost). It first computes the union of the pattern then takes the bits
        one by one and builds the pattern with them: Each candidate pattern is
        evaluated and the best chosen."""
        bestCost = None
        tabuBits = []
        chosenBits = []
        chosenBitVals = []
        maxPatternLen = patternLen([i[0] for i in subtree.patterns])
        bestBit = 0
        bestBitVal = 0
        bestLeaves = None
        while bestBit != None:
            bestBit = None
            for bit in range(0, maxPatternLen):
                if not bit in chosenBits and not bit in tabuBits:
                    curCost0, curLeaves0 = self.computePatternCost(subtree, (chosenBits, chosenBitVals), bit, 0)
                    curCost1, curLeaves1 = self.computePatternCost(subtree, (chosenBits, chosenBitVals), bit, 1)
                    if not curLeaves0 or not curLeaves1:
                        if not chosenBits:
                            tabuBits.append(bit)
                        continue
                    if curCost0 > curCost1:
                        curCost = curCost1
                        curLeaves = curLeaves1
                        curBitVal = 1
                    else:
                        curCost = curCost0
                        curLeaves = curLeaves0
                        curBitVal = 0
                    if bestCost is None or bestCost > curCost:
                        bestBit = bit
                        bestBitVal = curBitVal
                        bestCost = curCost
                        bestLeaves = curLeaves
            if bestBit != None:
                chosenBits.append(bestBit)
                chosenBitVals.append(bestBitVal)
        # Now we return the tuple (bestPattern, bestLeaves, bestCost), where
        # bestPattern represents the best split function for this subtree,
        # bestLeaves the nodes directly descending from the current subtree
        # according to the split function and, finally, costPattern the cost of
        # this split.
        matchPattern = []
        for i in range(0, maxPatternLen):
            matchPattern.append(None)
        for i in range(0, len(chosenBits)):
            matchPattern[chosenBits[i]] = chosenBitVals[i]
        if bestCost == None:
            return(None, None, None)
        return (SplitFunction(pattern = matchPattern), bestLeaves, bestCost)

    def findBestTable(self, subtree):
        """Given a subtree, it finds the best table for the split of the top
        node of the subtree. It returns the tuple (bestTable, bestLeaves,
        bestCost). We first evaluate all 2-bit tables, then 3-bit tables, etc.
        The best m-bits table is returned, where 2 < m <
        len(importantBits == 1)."""
        maxPatternLen = patternLen([i[0] for i in subtree.patterns])
        bestCost = None
        bestLeaves = None
        bestMask = ()
        curTableLen = 2
        improvement = True
        while curTableLen <= maxPatternLen and improvement:
            improvement = False
            for startTable in range(0, maxPatternLen - curTableLen + 1):
                curCost, curLeaves = self.computeTableCost(subtree, startTable, curTableLen)
                if not curCost:
                    continue
                if bestCost is None or curCost < bestCost:
                    bestMask = (startTable, curTableLen)
                    bestCost = curCost
                    bestLeaves = curLeaves
                    improvement = True
            curTableLen += 1
        if not bestMask:
            return (None, None, None)
        # Found best table.
        tablePattern = []
        foundMask = False
        encounteredImportant = 0
        for i in range(0, maxPatternLen):
            if i >= bestMask[0] and i < bestMask[0] + bestMask[1]:
                tablePattern.append(1)
                foundMask = True
            else:
                tablePattern.append(0)
        if not foundMask:
            return (None, None, None)
        return (SplitFunction(table = tablePattern), bestLeaves, bestCost)

    def computeIllegalBistreams(self):
        """Given all instructions it computes the illegal patterns as the
        complement of the union of all valid patterns, followed by logic
        minimization."""
        validPattern = bitStringUnion([i[0] for i in self.instrPattern])
        self.complementPattern(validPattern, 0)

    def complementPattern(self, pattern, bit):
        """Given a pattern it computes the negation starting from bit bit
        this function is recursive. When each computed negated pattern is
        added to self.instrId and self.instrPattern"""
        if bit == len(pattern):
            return
        import copy
        flipped = False
        for i in range(bit, len(pattern)):
            if flipped:
                pattern[i] = None
            elif pattern[i] == 1:
                self.complementPattern(copy.deepcopy(pattern), i + 1)
                pattern[i] = 0
                flipped = True
            elif pattern[i] == 0:
                self.complementPattern(copy.deepcopy(pattern), i + 1)
                pattern[i] = 1
                flipped = True
        if flipped and pattern:
            if self.instrId.has_key(-1):
                self.instrId[-1][0].append(copy.deepcopy(pattern))
            else:
                self.instrId[-1] = ([copy.deepcopy(pattern)], (float(self.minFreq)/float(100))/float(self.totalCount))
            self.instrPattern.append((copy.deepcopy(pattern), (float(self.minFreq)/float(100))/float(self.totalCount)))

    def computeDecoder(self):
        """The recursive algorithm for computing the decoder is as follows:
        - Add fake instructions corresponding to illegal bitstreams (done by computeIllegalBistreams).
        - Start with the decoding tree composed of all instructions.
        - For each leaf of the tree:
        -   Call findBestPattern (use memoryCost and huffmanHeight to compute the cost),
        -   Call findBestTable (use memoryCost and huffmanHeight to compute the cost),
        -   Pick up the best one (use memoryCost and huffmanHeight to compute the cost),
        -   Split the tree creating new leaves,
        -   Iterate on the new leaves,
        -   Proceed until each leaf is composed of only one instruction.
        """
        # Compute the starting node of the decoding tree containing all
        # instructions.
        self.rootNode = DecodingNode(self.instrPattern)
        self.decodingTree.add_node(self.rootNode)
        self.computeDecoderRec(self.rootNode)

    def computeDecoderRec(self, subtree):
        """It recursively computes the decoder for a given subtree. The terminal
        condition is when the subtree contains only one instruction. The node is
        then tagged as a leaf associated to the corresponding instruction id.
        This is also done for nodes containing more than one instruction, where
        all share the same id,"""

        # Terminal case: Check if the instructions in the current node refers to
        # more than one class.
        curClass = None
        different = False
        for instr in subtree.patterns:
            foundPattern = False
            for instrId, instrVals in self.instrId.items():
                if instrVals[0] == instr[0]:
                    foundPattern = True
                    if curClass == None:
                        curClass = instrId
                    elif curClass != instrId:
                        different = True
                    break
            if not foundPattern:
                if curClass == None:
                    curClass = -1
                elif curClass != -1:
                    different = True

            if different:
                break
        if not different:
            # Invalid instruction.
            if curClass is None:
                subtree.instrId = -1
            # Valid instruction: Assign id.
            else:
                subtree.instrId = curClass
            return
        # Recursive case: If we are here it means that we still have to split
        # the node.
        bestPattern, leavesPattern, costPattern = self.findBestPattern(subtree)
        bestTable, leavesTable, costTable = self.findBestTable(subtree)
        if not bestTable and not bestPattern:
            curInstrNames = []
            for instr in subtree.patterns:
                found = False
                for instrId, instrVals in self.instrId.items():
                    if instrVals[0] == instr[0]:
                        found = True
                        curInstrNames.append(self.instrName[instrId])
                        break
                if not found:
                    curInstrNames.append('Invalid')
            raise Exception('No table or pattern decoder found for remaining instructions ' + str(curInstrNames) + '.')
        if bestTable and not bestPattern:
            curInstrNames = []
            for instr in subtree.patterns:
                for instrId, instrVals in self.instrId.items():
                    if instrVals[0] == instr[0]:
                        curInstrNames.append(self.instrName[instrId])
                        break
            raise Exception('No pattern decoder found for remaining instructions ' + str(curInstrNames) + '.')
        if bestTable and costPattern > costTable and len(leavesTable) > 1:
            # It is better to split on the table.
            subtree.splitFunction = bestTable
            for i in leavesTable:
                self.decodingTree.add_node(i[0])
                if nxVersion > 0.99:
                    self.decodingTree.add_edge(subtree, i[0], decodePattern=i[1])
                else:
                    self.decodingTree.add_edge(subtree, i[0], i[1])
                self.computeDecoderRec(i[0])
        else:
            # It is better to split on the pattern
            subtree.splitFunction = bestPattern
            if len(leavesPattern) > 1:
                for i in leavesPattern:
                    self.decodingTree.add_node(i[0])
                    if nxVersion > 0.99:
                        self.decodingTree.add_edge(subtree, i[0], decodePattern=i[1])
                    else:
                        self.decodingTree.add_edge(subtree, i[0], i[1])
                    self.computeDecoderRec(i[0])

    def printDecoder(self, filename):
        try:
            NX.write_dot(self.decodingTree, filename)
        except:
            import traceback
            traceback.print_exc()
            print ('Error in printing the decoding tree on file ' + filename)

################################################################################
