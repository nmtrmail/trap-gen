################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     __speed_graph.py
# @brief    This file is part of the TRAP processor benchmark suite.
# @details
# @author   Luca Fossati
# @date     2008-2013 Luca Fossati
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

import sys, os, csv
from pyx import *

# In case there are more than 20 benchmarks, it executes a sampling so that only
# 1 out of total/20 is displayed
SAMPLE = True

if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise Exception('Specify file containing benchmarking results.')
    if not os.path.exists(sys.argv[1]):
        raise Exception('File ' + sys.argv[1] + ' containing benchmarking results does not exist.')

    mypainter = graph.axis.painter.bar(nameattrs=[trafo.rotate(35),
                                              text.halign.right,text.size.scriptsize],
                                   innerticklength=0.1)
    myaxis = graph.axis.bar(painter=mypainter)
    chosen_style = [graph.style.errorbar(), graph.style.bar([color.rgb.green])]

    fileHandle = open(sys.argv[1], 'r')
    fileCsvReader = csv.reader(fileHandle, delimiter = ';')
    graphPointsTemp = []
    for row in fileCsvReader:
        graphPointsTemp.append((row[0].replace('_', '\_'), float(row[-2]), float(row[-1])))

    graphPoints = []
    if SAMPLE and len(graphPointsTemp) > 20:
        reduceFactor = 20.0/len(graphPointsTemp)
    else:
        reduceFactor = 1.0

    for i in range(0, len(graphPointsTemp)):
        curTargetIndex = int(i*reduceFactor)
        try:
            graphPoints[curTargetIndex]
        except IndexError:
            graphPoints.append(graphPointsTemp[i])

    g_cat = graph.graphxy(width=8, x=myaxis, y=graph.axis.linear(min=0, title="Execution Speed [MIPS]",painter=graph.axis.painter.regular(gridattrs=[style.linestyle.dashed,style.linewidth.Thin,color.gray(0.5)])))
    g_cat.plot([graph.data.points(graphPoints, xname=0, addlinenumbers = 0, y=1, dy=2)], styles=chosen_style)

    g_cat.writePDFfile(os.path.splitext(os.path.basename(sys.argv[1]))[0])
