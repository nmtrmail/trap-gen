/***************************************************************************//**
*
*  _/_/_/_/_/  _/_/_/           _/        _/_/_/
*     _/      _/    _/        _/_/       _/    _/
*    _/      _/    _/       _/  _/      _/    _/
*   _/      _/_/_/        _/_/_/_/     _/_/_/
*  _/      _/    _/     _/      _/    _/
* _/      _/      _/  _/        _/   _/
*
* @file     analyzer.cpp
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
#include "analyzer.hpp"
#include "common/report.hpp"

#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>

#include <ctype.h>
#include <cstdlib>

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <map>

/**
 * @brief MemAnalyzer()
 */
trap::MemAnalyzer::MemAnalyzer(std::string filename, std::string mem_size) {
  this->mem_size = (unsigned)std::strtoul(mem_size.c_str(), NULL, 0);
  boost::filesystem::path mem_dump_path = boost::filesystem::system_complete(boost::filesystem::path(filename));
  if (!boost::filesystem::exists(mem_dump_path)) {
    THROW_EXCEPTION("Memory dump path " << filename << " does not exist.");
  } else {
    this->dumpfile.open(filename.c_str(), std::ifstream::in | std::ifstream::binary);
    if (!this->dumpfile.good())
      THROW_EXCEPTION("Cannot open file " << filename << '.');
  }
} // MemAnalyzer()

/// ----------------------------------------------------------------------------

/**
 * @brief ~MemAnalyzer()
 */
trap::MemAnalyzer::~MemAnalyzer() {
  if (this->dumpfile.is_open()) {
    this->dumpfile.close();
  }
} // ~MemAnalyzer()

/// ----------------------------------------------------------------------------

/**
 * @brief create_mem_image()
 *
 * Creates the image of the memory as it was at sim_time.
 */
void trap::MemAnalyzer::create_mem_image(
    boost::filesystem::path& outfile,
    double sim_time) {
  char* mem_image_temp = new char[this->mem_size];
  MemAccessType read_val;
  unsigned max_addr = 0;

  ::bzero(mem_image_temp, this->mem_size);

  while(this->dumpfile.good()) {
    this->dumpfile.read((char*)&read_val, sizeof(MemAccessType));
    if (this->dumpfile.good()) {
      if (read_val.simulation_time > sim_time && sim_time > 0) // Reached the desired cycle.
        break;
      if (read_val.address < this->mem_size) {
        mem_image_temp[read_val.address] = read_val.val;
        if (read_val.address > max_addr)
          max_addr = read_val.address;
      }
    }
  }
  this->dumpfile.seekg(std::ifstream::beg);

  // Print the memory image to the output file.
  std::ofstream mem_image_file(outfile.string().c_str());
  for(int i = 0; i < max_addr; i+=sizeof(int)) {
    mem_image_file << "MEM[" << std::hex << std::showbase << i << "] = " << ((int*)mem_image_temp)[i/sizeof(int)] << std::endl;
  }
  mem_image_file.close();
  delete [] mem_image_temp;
} // create_mem_image()

/// ----------------------------------------------------------------------------

/**
 * @brief get_first_mod_after()
 *
 * Returns the first memory access that modified the address addr after
 * sim_time.
 */
std::map<unsigned, trap::MemAccessType> trap::MemAnalyzer::get_first_mod_after(
    std::string addr, unsigned width, double sim_time) {
  MemAccessType read_val;
  std::map<unsigned, trap::MemAccessType> ret_val;
  unsigned address = (unsigned)std::strtoul(addr.c_str(), NULL, 0);

  while(this->dumpfile.good()) {
    this->dumpfile.read((char*)&read_val, sizeof(MemAccessType));
    if (this->dumpfile.good()) {
      if (read_val.simulation_time >= sim_time && read_val.address >= address && read_val.address < (address + width)) {
        if (ret_val.find(read_val.address) == ret_val.end()) {
          ret_val[read_val.address] = read_val;
          if (ret_val.size() == width) {
            this->dumpfile.seekg(std::ifstream::beg);
            return ret_val;
          }
        }
      }
    }
  }

  THROW_EXCEPTION("No modifications performed to address " << std::hex << std::showbase << address << '.');
  this->dumpfile.seekg(std::ifstream::beg);

  return ret_val;
} // get_first_mod_after()

/// ----------------------------------------------------------------------------

/**
 * @brief get_last_mod()
 *
 * Returns the last memory access that modified addr.
 */
std::map<unsigned, trap::MemAccessType> trap::MemAnalyzer::get_last_mod(
    std::string addr, unsigned width) {
  MemAccessType read_val;
  std::map<unsigned, trap::MemAccessType> found_val;
  bool found = false;
  unsigned address = (unsigned)std::strtoul(addr.c_str(), NULL, 0);

  while(this->dumpfile.good()) {
    this->dumpfile.read((char*)&read_val, sizeof(MemAccessType));
    if (this->dumpfile.good()) {
      if (read_val.address >= address && read_val.address < (address + width)) {
        found_val[read_val.address] = read_val;
      }
    }
  }

  if (found_val.size() == 0)
    THROW_EXCEPTION("No modifications performed to address " << std::hex << std::showbase << address << '.');

  this->dumpfile.seekg(std::ifstream::beg);
  return found_val;
} // get_last_mod()

/// ----------------------------------------------------------------------------

/**
 * @brief get_all_modifications()
 *
 * Prints all modifications to address addr.
 */
void trap::MemAnalyzer::get_all_modifications(std::string addr, boost::filesystem::path& outfile, unsigned width, double init_sim_time, double end_sim_time) {
  MemAccessType read_val;
  unsigned address = (unsigned)std::strtoul(addr.c_str(), NULL, 0);
  std::ofstream mem_image_file(outfile.string().c_str());

  while(this->dumpfile.good()) {
    this->dumpfile.read((char*)&read_val, sizeof(MemAccessType));
    if (this->dumpfile.good()) {
      if (read_val.address >= address && read_val.address < (address + width) && read_val.simulation_time >= init_sim_time && (end_sim_time < 0 || read_val.simulation_time <= end_sim_time)) {
        mem_image_file << "MEM[" << std::hex << std::showbase << read_val.address << "] = " << (int)read_val.val << " time " << std::dec << read_val.simulation_time << " program counter " << std::hex << std::showbase << read_val.program_counter << std::endl;
      }
    }
  }

  mem_image_file.close();
  this->dumpfile.seekg(std::ifstream::beg);
} // get_all_modifications

/// ****************************************************************************
