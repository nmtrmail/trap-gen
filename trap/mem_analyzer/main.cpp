/***************************************************************************//**
*
*  _/_/_/_/_/  _/_/_/           _/        _/_/_/
*     _/      _/    _/        _/_/       _/    _/
*    _/      _/    _/       _/  _/      _/    _/
*   _/      _/_/_/        _/_/_/_/     _/_/_/
*  _/      _/    _/     _/      _/    _/
* _/      _/      _/  _/        _/   _/
*
* @file     main.cpp
* @brief    This file is part of the TRAP memory analyzer tool.
* @details
* @author   Luca Fossati
* @author   Lillian Tadros (Technische Universitaet Dortmund)
* @date     2008-2013 Luca Fossati
*           2015-2016 Technische Universitaet Dortmund
* @copyright
*
* This file is part of TRAP.
*
* TRAP is free software; you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as
* published by the Free Software Foundation; either version 3 of the
* License, or (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this program; if not, write to the
* Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
* or see <http://www.gnu.org/licenses/>.
*
* (c) Luca Fossati, fossati@elet.polimi.it, fossati.l@gmail.com
*
*******************************************************************************/
#include <map>
#include <string>
#include <iostream>

#include "analyzer.hpp"
#include "common/report.hpp"

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

using namespace trap;

/**
 * @brief main()
 */
int main(int argc, char* argv[]) {
  boost::program_options::options_description desc("Memory Analyzer");
  desc.add_options()
  ("help,h", "display this help and exit")
  ("operation,o", boost::program_options::value<int>(), "specifies the operation to be executed [1: create memory image - 2: get all modifications to a specified address - 3: gets the first modification to an address after a given simulation time - 4: gets the last modification to an address]")
  ("dump,d", boost::program_options::value<std::string>(), "the name of the dump file")
  ("outfile,f", boost::program_options::value<std::string>(), "the output file (only operations 1 and 2)")
  ("address,a", boost::program_options::value<std::string>(), "the address at which we want to get the modifications")
  ("start_time,s", boost::program_options::value<double>(), "the time at which we want to analyze the modifications (operation-dependent)")
  ("end_time,e", boost::program_options::value<double>(), "the end time until which we want to get the modification")
  ("mem_size,m", boost::program_options::value<std::string>(), "the maximum memory size [default 5MB]")
  ("width,w", boost::program_options::value<unsigned>(), "the width of each data operation in bytes [default 4 bytes] (operations 2, 3 and 4)")
  ;

  boost::program_options::variables_map vm;
  boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
  boost::program_options::notify(vm);

  // Check that parameters are specified correctly.
  if (vm.count("help") != 0) {
    std::cout << desc << std::endl;
    return 0;
  }
  if (vm.count("operation") == 0) {
    std::cerr << "Error: Specify the operation to be executed." << std::endl;
    std::cerr << desc << std::endl;
    return -1;
  }
  if (vm.count("dump") == 0) {
    std::cerr << "Error: Specify the name of the dump file." << std::endl;
    std::cerr << desc << std::endl;
    return -1;
  }
  std::string mem_size = boost::lexical_cast<std::string>(5242880);
  unsigned width = 4;
  if (vm.count("width") > 0) {
    width = vm["width"].as<unsigned>();
  }
  if (vm.count("mem_size") > 0) {
    mem_size = vm["mem_size"].as<std::string>();
  }
  MemAnalyzer analyzer(vm["dump"].as<std::string>(), mem_size);
  switch (vm["operation"].as<int>()) {
    case 1: {
      if (vm.count("outfile") == 0) {
        std::cerr << "Error: Specify an output file for operation (1)." << std::endl;
        std::cerr << desc << std::endl;
        return -1;
      }
      boost::filesystem::path outfile = boost::filesystem::system_complete(boost::filesystem::path(vm["outfile"].as<std::string>()));
      if (vm.count("start_time") == 0) {
        analyzer.create_mem_image(outfile);
      } else {
        analyzer.create_mem_image(outfile, vm["start_time"].as<double>());
      }
    break;}
    case 2: {
      if (vm.count("address") == 0) {
        std::cerr << "Error: Specify the address for operation (2)." << std::endl;
        std::cerr << desc << std::endl;
        return -1;
      }
      if (vm.count("outfile") == 0) {
        std::cerr << "Error: Specify the output file for operation (2)." << std::endl;
        std::cerr << desc << std::endl;
        return -1;
      }
      double start_time = 0;
      if (vm.count("start_time") > 0) {
        start_time = vm["start_time"].as<double>();
      }
      double end_time = -1;
      if (vm.count("end_time") > 0) {
        end_time = vm["end_time"].as<double>();
      }
      boost::filesystem::path outfile = boost::filesystem::system_complete(boost::filesystem::path(vm["outfile"].as<std::string>()));
      analyzer.get_all_modifications(vm["address"].as<std::string>(), outfile, width, start_time, end_time);
    break;}
    case 3: {
      if (vm.count("address") == 0) {
        std::cerr << "Error: Specify the address for operation (3)." << std::endl;
        std::cerr << desc << std::endl;
        return -1;
      }
      std::map<unsigned, MemAccessType> modification;
      if (vm.count("start_time") == 0) {
        modification = analyzer.get_first_mod_after(vm["address"].as<std::string>(), width);
      } else {
        modification = analyzer.get_first_mod_after(vm["address"].as<std::string>(), width, vm["start_time"].as<double>());
      }
      std::map<unsigned, MemAccessType>::iterator mod_it, mod_end;
      for(mod_it = modification.begin(), mod_end = modification.end(); mod_it != mod_end; mod_it++) {
        std::cout << "Address " << std::hex << std::showbase << mod_it->second.address << " - Value " << std::hex << std::showbase << mod_it->second.val << " - PC " << std::hex << std::showbase << mod_it->second.program_counter << " - Time " << std::dec << mod_it->second.simulation_time << std::endl;
      }
    break;}
    case 4: {
      if (vm.count("address") == 0) {
        std::cerr << "Error: Specify the address for operation (4)." << std::endl;
        std::cerr << desc << std::endl;
        return -1;
      }
      std::map<unsigned, MemAccessType> modification = analyzer.get_last_mod(vm["address"].as<std::string>(), width);
      std::map<unsigned, MemAccessType>::iterator mod_it, mod_end;
      for(mod_it = modification.begin(), mod_end = modification.end(); mod_it != mod_end; mod_it++) {
        std::cout << "address=" << std::hex << std::showbase << mod_it->second.address << ", value=" << std::hex << std::showbase << mod_it->second.val << ", PC=" << std::hex << std::showbase << mod_it->second.program_counter << ", time=" << std::dec << mod_it->second.simulation_time << std::endl;
      }
    break;}
    default:
      THROW_EXCEPTION("Unrecognized option " << vm["operation"].as<int>() << '.');
    break;
  }

  return 0;
} // main()

/// ****************************************************************************
