/***************************************************************************//**
*
*  _/_/_/_/_/  _/_/_/           _/        _/_/_/
*     _/      _/    _/        _/_/       _/    _/
*    _/      _/    _/       _/  _/      _/    _/
*   _/      _/_/_/        _/_/_/_/     _/_/_/
*  _/      _/    _/     _/      _/    _/
* _/      _/      _/  _/        _/   _/
*
* @file     analyzer.hpp
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
#ifndef ANALYZER_HPP
#define ANALYZER_HPP

#include <boost/filesystem.hpp>

#include <iostream>
#include <fstream>
#include <string>
#include <map>

namespace trap {

struct MemAccessType;

/**
 * @brief MemAnalyzer
 */
class MemAnalyzer {
  public:
  MemAnalyzer(std::string filename, std::string mem_size);
  ~MemAnalyzer();

  /// Creates the image of the memory as it was at sim_time.
  void create_mem_image(boost::filesystem::path& outfile, double sim_time = -1);

  /// Returns the first memory access that modified the address addr after
  /// sim_time.
  std::map<unsigned, MemAccessType> get_first_mod_after(std::string addr, unsigned width, double sim_time = 0);

  /// Returns the last memory access that modified addr.
  std::map<unsigned, MemAccessType> get_last_mod(std::string addr, unsigned width);

  /// Prints all modifications to address addr.
  void get_all_modifications(std::string addr, boost::filesystem::path& outfile, unsigned width,

  double init_sim_time = 0, double end_sim_time = -1);

  private:
  std::ifstream dumpfile;
  unsigned mem_size;
}; // class MemAnalyzer

} // namespace trap

/// ****************************************************************************
#endif
