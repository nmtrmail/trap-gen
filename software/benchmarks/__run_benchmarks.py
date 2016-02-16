################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     __benchmark.py
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

import csv, os, sys, numpy

if __name__ == "__main__":
    if len(sys.argv) < 4:
        raise Exception('Usage: processor_executable numRuns benchmark1 benchmark2 ...')

    maxSpeed = 0
    fastBench = ''
    runTime = []
    benchSpeed = {}
    for benchmark in sys.argv[3:]:
        if not os.path.exists(benchmark):
            raise Exception('Benchmark ' + benchmark + ' does not exist.')
        print ('Executing benchmark ' + benchmark + '....')
        benchSpeed[benchmark] = []
        for i in range(0, int(sys.argv[2])):
            result = os.popen(sys.argv[1] + ' -a ' + benchmark).readlines()
            if not 'Program exited with value 0\n' in result:
                print('Benchmark ' + benchmark + ' failed. Wrong exit value.')
                break
            for res in result:
                if 'Execution Speed' in res:
                    speed = float(res.split()[2])
                    if speed > maxSpeed:
                        maxSpeed = speed
                        fastBench = benchmark
                    runTime.append(speed)
                    benchSpeed[benchmark].append(speed)
                    break

    for benchmark in sys.argv[3:]:
        if len(benchSpeed[benchmark]) < int(sys.argv[2]):
            print('Benchmark ' + benchmark + ' failed in at least one run.')
            benchSpeed.pop(benchmark, None)

    # Lets now print the csv file with all the benchmarks
    fileHandle = open('TRAP_stats.csv', 'w')
    fileCsvWriter = csv.writer(fileHandle, delimiter = ';')
    orderedBenchSpeed = sorted(benchSpeed.items(), lambda x, y: cmp(numpy.average(y[1]), numpy.average(x[1])))
    for bench, speedRuns in orderedBenchSpeed:
        fileCsvWriter.writerow([bench] + [str(i) for i in speedRuns] + [str(numpy.average(speedRuns)), str(numpy.std(speedRuns))])
    fileHandle.close()

    # Finally I print some statistics
    fileHandle = open('TRAP_summary.csv', 'w')
    fileCsvWriter = csv.writer(fileHandle, delimiter = ';')
    fileCsvWriter.writerow(['avg', str(numpy.average(runTime))])
    fileCsvWriter.writerow(['std', str(numpy.std(runTime))])
    fileHandle.close()

    print ('Executed ' + str(len(sys.argv[3:])) + ' benchmarks with ' + sys.argv[2] + ' runs per benchmark.')
    print ('Average execution speed: ' + str(numpy.average(runTime)) + ' MIPS.')
    print ('Standard deviation: ' + str(numpy.std(runTime)) + ' MIPS.')
    print ('Fastest benchmark: ' + fastBench + ' (' + str(maxSpeed) + ' MIPS).')
