// Copyright (C) 2018-2024 Iñaki Ucar
//
// This file is part of simmer.
//
// simmer is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// simmer is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with simmer. If not, see <http://www.gnu.org/licenses/>.

#ifndef simmer__process_datasrc_h
#define simmer__process_datasrc_h

#include <simmer/process/source.h>
#include <simmer/process/arrival.h>
#include <simmer/activity.h>

namespace simmer {

  class DataSrc : public Source {
  public:
    DataSrc(Simulator* sim, const std::string& name_prefix, int mon,
            const REnv& trj, RData data, int batch, const std::string& col_time,
            const VEC<std::string>& col_attrs, const OPT<std::string>& col_priority,
            const OPT<std::string>& col_preemptible, const OPT<std::string>& col_restart)
      : Source(sim, name_prefix, mon, trj, Order()), source_(data), batch(batch),
        col_time(col_time), col_attrs(col_attrs), col_priority(col_priority),
        col_preemptible(col_preemptible), col_restart(col_restart) { reset(); }

    void reset() {
      Source::reset();
      source = source_;
      set_source_impl(source);
    }

    void run() {
      double delay = 0;
      int i = 0;

      while (i++ != batch) {
        if (time.size() <= index || check_stop(time[index]))
          return;
        delay += time[index];

        Arrival* arrival = new_arrival(delay);

        for (size_t j = 0; j < col_attrs.size(); ++j)
          arrival->set_attribute(col_attrs[j], attrs[j][index]);

        if (col_priority)
          arrival->order.set_priority(priority[index]);
        if (col_preemptible)
          arrival->order.set_preemptible(preemptible[index]);
        if (col_restart)
          arrival->order.set_restart(restart[index]);

        index++;
      }
      // schedule the generator
      sim->schedule(delay, this, Source::priority);
    }

  private:
    int index;
    RData source_, source;
    int batch;
    std::string col_time;
    VEC<std::string> col_attrs;
    OPT<std::string> col_priority;
    OPT<std::string> col_preemptible;
    OPT<std::string> col_restart;
    RNum time;
    VEC<RNum> attrs;
    RInt priority;
    RInt preemptible;
    RBool restart;

    void set_source_impl(const std::any& new_source) {
      if (new_source.type() != typeid(RData))
        Rcpp::stop("data frame required");
      RData df = STDANYCAST<RData>(new_source);

      if (!df.containsElementNamed(col_time.c_str()))
        Rcpp::stop("column '%s' not present", col_time);
      for (const auto& col_attr : col_attrs) {
        if (!df.containsElementNamed(col_attr.c_str()))
          Rcpp::stop("column '%s' not present", col_attr);
      }
      if (col_priority && !df.containsElementNamed((*col_priority).c_str()))
        Rcpp::stop("column '%s' not present", *col_priority);
      if (col_preemptible && !df.containsElementNamed((*col_preemptible).c_str()))
        Rcpp::stop("column '%s' not present", *col_preemptible);
      if (col_restart && !df.containsElementNamed((*col_restart).c_str()))
        Rcpp::stop("column '%s' not present", *col_restart);

      index = 0;
      source = df;
      time = source[col_time];
      attrs.clear();
      for (const auto& col_attr : col_attrs)
        attrs.push_back(source[col_attr]);
      if (col_priority) priority = source[*col_priority];
      if (col_preemptible) preemptible = source[*col_preemptible];
      if (col_restart) restart = source[*col_restart];
    }
  };

} // namespace simmer

#endif
