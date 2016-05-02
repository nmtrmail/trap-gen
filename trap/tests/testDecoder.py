################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     testDecoder.py
# @brief    This file is part of the TRAP processor generator testsuite.
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


import decoder

import unittest
import os

class dummyInstruction:
    def __init__(self, id, bitstring):
        self.frequency = 1
        self.subInstructions = []
        self.bitstring = bitstring
        self.id = id
    
class TestDecoder(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testComplement1(self):
        """Test the correct operation of the complementPattern function
        in a normal case"""
        dec = decoder.decoderCreator({}, {})
        dec.complementPattern([0, 1], 0)
        self.assertEqual(2, len(dec.instrPattern))
        self.assert_([1, None] in [i[0] for i in dec.instrPattern])
        self.assert_([0, 0] in [i[0] for i in dec.instrPattern])

    def testComplement2(self):
        """Test the correct operation of the complementPattern function
        in a normal case"""
        dec = decoder.decoderCreator({}, {})
        dec.complementPattern([0, None], 0)
        self.assertEqual(1, len(dec.instrPattern))
        self.assert_([1, None] in [i[0] for i in dec.instrPattern])

    def testComplementDegen(self):
        """Test the correct operation of the complementPattern function
        in a degenrated case where the complement set is null"""
        dec = decoder.decoderCreator({}, {})
        dec.complementPattern([None, None], 0)
        self.assertEqual(0, len(dec.instrPattern))

    def testIllegalBitsream1(self):
        """Tests the computation of the negation of the union of the valid
        bitstrings"""
        dec = decoder.decoderCreator({}, {})
        dec.instrPattern += [([1, 1], 0)]
        dec.computeIllegalBistreams()
        self.assertEqual(3, len(dec.instrPattern))
        self.assert_([1, 1] in [i[0] for i in dec.instrPattern])
        self.assert_([0, None] in [i[0] for i in dec.instrPattern])
        self.assert_([1, 0] in [i[0] for i in dec.instrPattern])

    def testIllegalBitsream2(self):
        """Tests the computation of the negation of the union of the valid
        bitstrings"""
        dec = decoder.decoderCreator({}, {})
        dec.instrPattern += [([1, 1], 0), ([1, 0], 0)]
        dec.computeIllegalBistreams()
        self.assertEqual(3, len(dec.instrPattern))
        self.assert_([0, None] in [i[0] for i in dec.instrPattern])
        self.assert_([1, 1] in [i[0] for i in dec.instrPattern])
        self.assert_([1, 0] in [i[0] for i in dec.instrPattern])

    def testIllegalBitsream3(self):
        """Tests the computation of the negation of the union of the valid
        bitstrings"""
        dec = decoder.decoderCreator({}, {})
        dec.instrPattern += [([1, None], 0)]
        dec.computeIllegalBistreams()
        self.assertEqual(2, len(dec.instrPattern))
        self.assert_([1, None] in [i[0] for i in dec.instrPattern])
        self.assert_([0, None] in [i[0] for i in dec.instrPattern])

    def testbitStringValid(self):
        """Tests that the function that returns the valid bits in a set
        of bitStrings behaves correctly"""
        result = decoder.bitStringValid(((1, 1), (0, 0)))
        self.assertEqual([1, 1], result)

    def testbitStringValid0(self):
        """Tests that the function that returns the valid bits in a set
        of bitStrings behaves correctly"""
        result = decoder.bitStringValid(((1, 1), (0, None)))
        self.assertEqual([1, None], result)

    def testbitStringValid1(self):
        """Tests that the function that returns the valid bits in a set
        of bitStrings behaves correctly when there are no valid bits"""
        result = decoder.bitStringValid(((None, 1), (0, None)))
        self.assertEqual([None, None], result)

    def testbitStringUnion(self):
        """Tests that the function that returns the union of a set
        of bitStrings behaves correctly"""
        result = decoder.bitStringUnion(((1, 1), (1, 0)))
        self.assertEqual([1, None], result)

    def testbitStringUnion0(self):
        """Tests that the function that returns the union of a set
        of bitStrings behaves correctly"""
        result = decoder.bitStringUnion(((0, 1), (1, 0)))
        self.assertEqual([None, None], result)

    def testbitStringUnion1(self):
        """Tests that the function that returns the union of a set
        of bitStrings behaves correctly"""
        result = decoder.bitStringUnion(((None, None), (None, None)))
        self.assertEqual([None, None], result)

    def testbitStringUnion2(self):
        """Tests that the function that returns the union of a set
        of bitStrings behaves correctly"""
        result = decoder.bitStringUnion(((1, None), (None, 1)))
        self.assertEqual([None, None], result)

    def testComputeCost1(self):
        """Tests that, given a subtree, the computational cost is correctly computed
        this means that the corresponding huffman tree is comrrectly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1), 0.1), ((1, 1), 0.1)))
        self.assertEqual(2, dec.computationalCost(surSubtree))

    def testComputeCost2(self):
        """Tests that, given a subtree, the computational cost is correctly computed
        this means that the corresponding huffman tree is comrrectly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1), 0.1), ((1, 1), 0.1), ((1, 1), 0.8), ((1, 1), 0.05)))
        self.assertEqual(4, dec.computationalCost(surSubtree))

    def testComputeCost3(self):
        """Tests that, given a subtree, the computational cost is correctly computed
        this means that the corresponding huffman tree is comrrectly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1), 0.1), ((1, 1), 0.1), ((1, 1), 0.1), ((1, 1), 0.1)))
        self.assertEqual(3, dec.computationalCost(surSubtree))

    def testTableCost1(self):
        """Tests that, given a subtree, the computational cost is correctly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1), 0.1), ((0, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1)))
        cost, subTree = dec.computeTableCost(surSubtree, 0, 2)
        self.assertEqual((0, 0.1), subTree[0][1])
        self.assertEqual((1, 0.1), subTree[1][1])
        self.assertEqual((2, 0.1), subTree[2][1])
        self.assertEqual((3, 0.1), subTree[3][1])
        self.assertEqual(decoder.DecodingNode((((0, 0), 0.1), )), subTree[0][0])
        self.assertEqual(decoder.DecodingNode((((0, 1), 0.1), )), subTree[2][0])
        self.assertEqual(decoder.DecodingNode((((1, 0), 0.1), )), subTree[1][0])
        self.assertEqual(decoder.DecodingNode((((1, 1), 0.1), )), subTree[3][0])

    def testTableCost2(self):
        """Tests that, given a subtree, the computational cost is correctly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 0, 1), 0.1), ((0, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1)))
        cost, subTree = dec.computeTableCost(surSubtree, 0, 2)
        self.assertEqual((0, 0.1), subTree[0][1])
        self.assertEqual((2, 0.1), subTree[2][1])
        self.assertEqual((1, 0.2), subTree[1][1])
        self.assertEqual(decoder.DecodingNode((((0, 0), 0.1), )), subTree[0][0])
        self.assertEqual(decoder.DecodingNode((((1, 0, 1), 0.1), ((1, 0), 0.1))), subTree[1][0])
        self.assertEqual(decoder.DecodingNode((((0, 1), 0.1), )), subTree[2][0])

    def testTableCost3(self):
        """Tests that, given a subtree, the computational cost is correctly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 0, None), 0.1), ((0, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1)))
        cost, subTree = dec.computeTableCost(surSubtree, 0, 2)
        self.assertEqual((0, 0.1), subTree[0][1])
        self.assertEqual((2, 0.1), subTree[2][1])
        self.assertEqual((1, 0.2), subTree[1][1])
        self.assertEqual(decoder.DecodingNode((((0, 0), 0.1), )), subTree[0][0])
        self.assertEqual(decoder.DecodingNode((((1, 0, None), 0.1), ((1, 0), 0.1))), subTree[1][0])
        self.assertEqual(decoder.DecodingNode((((0, 1), 0.1), )), subTree[2][0])

    def testPatternCost1(self):
        """Tests that, given a subtree, the computational cost is correctly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1), 0.1), ((0, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1)))
        cost, (subTreeEq, subTreeNe) = dec.computePatternCost(surSubtree, ([0], [0]), 1, 0)
        self.assertEqual((1, 1.1), subTreeEq[1])
        self.assertEqual((0, 1.3000000000000003), subTreeNe[1])
        self.assertEqual(decoder.DecodingNode((((0, 0), 0.1), )), subTreeEq[0])
        self.assertEqual(decoder.DecodingNode((((1, 1), 0.1), ((0, 1), 0.1), ((1, 0), 0.1))), subTreeNe[0])

    def testPatternCost2(self):
        """Tests that, given a subtree, the computational cost is correctly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1), 0.1), ((0, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1)))
        cost, (subTreeEq, subTreeNe) = dec.computePatternCost(surSubtree, ([], []), 1, 0)
        self.assertEqual((1, 1.2000000000000002), subTreeEq[1])
        self.assertEqual((0, 1.2000000000000002), subTreeNe[1])
        self.assertEqual(decoder.DecodingNode((((0, 0), 0.1), ((1, 0), 0.1))), subTreeEq[0])
        self.assertEqual(decoder.DecodingNode((((1, 1), 0.1), ((0, 1), 0.1))), subTreeNe[0])

    def testPatternCost3(self):
        """Tests that, given a subtree, the computational cost is correctly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1, 0), 0.1), ((0, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1)))
        cost, (subTreeEq, subTreeNe) = dec.computePatternCost(surSubtree, ([0], [0]), 1, 0)
        self.assertEqual((1, 1.1), subTreeEq[1])
        self.assertEqual((0, 1.3000000000000003), subTreeNe[1])
        self.assertEqual(decoder.DecodingNode((((0, 0), 0.1), )), subTreeEq[0])
        self.assertEqual(decoder.DecodingNode((((1, 1, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1))), subTreeNe[0])

    def testPatternCost4(self):
        """Tests that, given a subtree, the computational cost is correctly computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, None, 0), 0.1), ((0, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1)))
        cost, (subTreeEq, subTreeNe) = dec.computePatternCost(surSubtree, ([0], [0]), 1, 0)
        self.assertEqual((1, 1.1), subTreeEq[1])
        self.assertEqual((0, 1.3000000000000003), subTreeNe[1])
        self.assertEqual(decoder.DecodingNode((((0, 0), 0.1), )), subTreeEq[0])
        self.assertEqual(decoder.DecodingNode((((1, None, 0), 0.1), ((0, 1), 0.1), ((1, 0), 0.1))), subTreeNe[0])

    def testBestPattern(self):
        """Tests that, given a subtree, the correct best pattern matching
        function is computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1, 0), 0.1), ((1, 0, 0), 0.1), ((1, 0, 1), 0.1), ((0, 0, 0), 0.1)))
        splitFunc, bestLeaves, bestCost = dec.findBestPattern(surSubtree)
        self.assertEqual([0, None, None], splitFunc.pattern)
        self.assertEqual(decoder.DecodingNode((((0, 0, 0), 0.1), )), bestLeaves[0][0])
        self.assertEqual(decoder.DecodingNode((((1, 1, 0), 0.1), ((1, 0, 0), 0.1), ((1, 0, 1), 0.1))), bestLeaves[1][0])

    def testBestTable(self):
        """Tests that, given a subtree, the correct best pattern matching
        function is computed"""
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode((((1, 1, 0), 0.1), ((1, 0, 0), 0.1), ((1, 0, 1), 0.1), ((0, 0, 0), 0.1)))
        splitFunc, bestLeaves, bestCost = dec.findBestTable(surSubtree)
        self.assertEqual([1, 1, 0], splitFunc.table)
        self.assertEqual(decoder.DecodingNode((((0, 0, 0), 0.1), )), bestLeaves[0][0])
        self.assertEqual(decoder.DecodingNode((((1, 0, 0), 0.1), ((1, 0, 1), 0.1))), bestLeaves[1][0])
        self.assertEqual(decoder.DecodingNode((((1, 1, 0), 0.1), )), bestLeaves[2][0])

    def testBestPattern1(self):
        """Tests that, given a subtree, the correct best pattern matching
        function is computed"""
        bitString1 = [None for i in range(0, 4)] + [0, 0, 0, 0, 1, 0, 1] + [None for i in range(0, 16)] + [0] + [None for i in range(0, 4)]
        bitString2 = [None for i in range(0, 4)] + [0, 0, 0, 0, 1, 0, 1] + [None for i in range(0, 13)] + [0] + [None,  None] + [1] + [None for i in range(0, 4)]
        bitString3 = [None for i in range(0, 4)] + [0, 0, 1, 0, 1, 0, 1] + [None for i in range(0, 21)]
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode(((bitString1, 1), (bitString2, 1), (bitString3, 1)))
        splitFunc, bestLeaves, bestCost = dec.findBestPattern(surSubtree)
        self.assertEqual([None for i in range(0, 6)] + [1] + [None for i in range(0, 25)], splitFunc.pattern)
        self.assertEqual(decoder.DecodingNode(((bitString1, 1), (bitString2, 1))), bestLeaves[1][0])
        self.assertEqual(decoder.DecodingNode(((bitString3, 1), )), bestLeaves[0][0])

    def testBestTable1(self):
        """Tests that, given a subtree, the correct best pattern matching
        function is computed"""
        bitString1 = [None for i in range(0, 4)] + [0, 0, 0, 0, 1, 0, 1] + [None for i in range(0, 16)] + [0] + [None for i in range(0, 4)]
        bitString2 = [None for i in range(0, 4)] + [0, 0, 0, 0, 1, 0, 1] + [None for i in range(0, 13)] + [0] + [None,  None] + [1] + [None for i in range(0, 4)]
        bitString3 = [None for i in range(0, 4)] + [0, 0, 1, 0, 1, 0, 1] + [None for i in range(0, 21)]
        dec = decoder.decoderCreator({}, {})
        surSubtree = decoder.DecodingNode(((bitString1, 1), (bitString2, 1), (bitString3, 1)))
        splitFunc, bestLeaves, bestCost = dec.findBestTable(surSubtree)
        self.assertEqual([0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], splitFunc.table)
        self.assertEqual(decoder.DecodingNode(((bitString1, 1), (bitString2, 1))), bestLeaves[0][0])
        self.assertEqual(decoder.DecodingNode(((bitString3, 1), )), bestLeaves[1][0])

    #def testComplexPattern(self):
        #instructions = {}
        #bitString1 = [1, 0, 0, 1, 1, 1, 0] + [None for i in range(0, 25)]
        #instructions['one'] = dummyInstruction(1, bitString1)
        #bitString2 = [1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0] + [None for i in range(0, 21)]
        #instructions['two'] = dummyInstruction(2, bitString2)
        #bitString3 = [1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0] + [None for i in range(0, 21)]
        #instructions['three'] = dummyInstruction(3, bitString3)
        #bitString4 = [1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1] + [None for i in range(0, 21)]
        #instructions['four'] = dummyInstruction(4, bitString4)
        #bitString5 = [1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1] + [None for i in range(0, 21)]
        #instructions['five'] = dummyInstruction(5, bitString5)
        #bitString6 = [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0] + [None for i in range(0, 21)]
        #instructions['six'] = dummyInstruction(6, bitString6)
        #bitString7 = [1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0] + [None for i in range(0, 21)]
        #instructions['seven'] = dummyInstruction(7, bitString7)
        #bitString8 = [1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0] + [None for i in range(0, 21)]
        #instructions['eight'] = dummyInstruction(8, bitString8)
        #bitString9 = [1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1] + [None for i in range(0, 21)]
        #instructions['nine'] = dummyInstruction(9, bitString9)
        #bitString10 = [1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0] + [None for i in range(0, 21)]
        #instructions['ten'] = dummyInstruction(10, bitString10)
        #bitString11 = [1, 0, 0, 1, 1, 1, 1, 1, 1] + [None for i in range(0, 23)]
        #instructions['eleven'] = dummyInstruction(11, bitString11)

        #dec = decoder.decoderCreator(instructions, {})
        #dec.printDecoder('dec.dot')
