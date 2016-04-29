#include "process.h"
#include "simulator.h"
#include "activity.h"

void Process::deactivate(bool restart) { sim->unschedule(this); }

void Generator::run() {
  // get the delay for the next arrival
  double delay = Rcpp::as<double>(dist());
  if (delay < 0) return;
  
  // format the name and create the next arrival
  char numstr[21];
  sprintf(numstr, "%d", count);
  Arrival* arrival = new Arrival(sim, name + numstr, is_monitored(), first_activity, this);
  
  if (sim->verbose) Rcpp::Rcout << 
    FMT_0 << sim->now() << " |" << FMT_11 << "generator: " << FMT_12 << name << "|" << 
    FMT_21 << "new: " << FMT_22 << (name + numstr) << "| " << (sim->now() + delay) << std::endl;
  
  // schedule the arrival and the generator itself
  sim->schedule(delay, arrival, count);
  sim->schedule(delay, this, PRIORITY_GENERATOR);
  
  count++;
}

void Manager::run() {
  if (!sim->now() && duration[index]) goto finish;
  if (sim->verbose) Rcpp::Rcout <<
    FMT_0 << sim->now() << " |" << FMT_11 << "manager: " << FMT_12 << name << "|" << 
    FMT_21 << "parameter: " << FMT_22 << param << "| " << value[index] << std::endl;
  
  set(value[index]);
  index++;
  if (index == duration.size()) {
    if (period < 0) goto end;
    index = 1;
  }
  
finish:
  sim->schedule(duration[index], this, PRIORITY_MANAGER);
end:
  return;
}

void Arrival::run() {
  double delay;
  
  if (!activity) goto finish;
  if (lifetime.start < 0) lifetime.start = sim->now();
  if (sim->verbose) Rcpp::Rcout <<
    FMT_0 << sim->now() << " |" << FMT_11 << "arrival: " << FMT_12 << name << "|" << 
    FMT_21 << "activity: " << FMT_22 << activity->name << "| " << activity->resource << std::endl;
  
  delay = activity->run(this);
  if (delay == REJECTED) goto end;
  activity = activity->get_next();
  if (delay == ENQUEUED) goto end;
  
  busy_until = sim->now() + delay;
  lifetime.activity += delay;
  sim->schedule(delay, this, activity ? activity->priority : 0);
  goto end;
  
finish:
  terminate(sim->now(), true);
end:
  return;
}

void Arrival::activate() {
  Process::activate();
  busy_until = sim->now() + remaining;
  sim->schedule(remaining, this, 1);
  remaining = 0;
}

void Arrival::deactivate(bool restart) {
  Process::deactivate(restart);
  remaining = busy_until - sim->now();
  if (remaining && restart) {
    lifetime.activity -= remaining;
    remaining = 0;
    activity = activity->get_prev();
  }
}

int Arrival::set_attribute(std::string key, double value) {
  attributes[key] = value;
  if (is_monitored() >= 2) 
    gen->observe(sim->now(), name, key, value);
  return 0;
}
