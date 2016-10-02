#include "process.h"
#include "simulator.h"
#include "activity.h"

void Process::deactivate() { sim->unschedule(this); }

void Generator::run() {
  // get the delay for the next (n) arrival(s)
  Rcpp::NumericVector delays = dist();
  int n = delays.size();
  double delay = 0;

  for(int i = 0; i < n; ++i) {
    if (delays[i] < 0)
      return;
    delay += delays[i];

    // format the name and create the next arrival
    std::string arr_name = name + boost::lexical_cast<std::string>(count++);
    Arrival* arrival = new Arrival(sim, arr_name, is_monitored(), order, first_activity);

    if (sim->verbose)Rcpp::Rcout <<
      FMT(10, right) << sim->now() << " |" <<
      FMT(12, right) << "generator: " << FMT(15, left) << name << "|" <<
      FMT(12, right) << "new: " << FMT(15, left) << arr_name << "| " <<
      (sim->now() + delay) << std::endl;

    // schedule the arrival
    sim->register_arrival(arrival);
    sim->schedule(delay, arrival, count);
  }
  // schedule the generator
  sim->schedule(delay, this, PRIORITY_GENERATOR);
}

void Manager::run() {
  if (!sim->now() && duration[index])
    goto finish;
  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" <<
    FMT(12, right) << "manager: " << FMT(15, left) << name << "|" <<
    FMT(12, right) << "parameter: " << FMT(15, left) << param << "| " <<
    value[index] << std::endl;

  set(value[index]);
  index++;
  if (index == duration.size()) {
    if (period < 0)
      goto end;
    index = 1;
  }

finish:
  sim->schedule(duration[index], this, PRIORITY_MANAGER);
end:
  return;
}

void Task::run() {
  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" <<
    FMT(12, right) << "task: " << FMT(15, left) << name << "|" <<
    FMT(12, right) << " " << FMT(15, left) << " " << "| " << std::endl;

  task();
  delete this;
}

void Arrival::reset() {
  cancel_timeout();
  foreach_ (ResMSet::value_type& itr, resources)
    Rcpp::warning("`%s`: leaving without releasing `%s`", name, itr->name);
  if (!--(*clones))
    delete clones;
  sim->unregister_arrival(this);
}

void Arrival::run() {
  double delay;

  if (!activity)
    goto finish;
  if (lifetime.start < 0)
    lifetime.start = sim->now();

  if (sim->verbose) {
    Rcpp::Rcout <<
      FMT(10, right) << sim->now() << " |" <<
      FMT(12, right) << "arrival: " << FMT(15, left) << name << "|" <<
      FMT(12, right) << "activity: " << FMT(15, left) << activity->name << "| ";
    activity->print(0, true);
  }

  delay = activity->run(this);
  if (delay == REJECTED)
    goto end;
  activity = activity->get_next();
  if (delay == ENQUEUED)
    goto end;

  lifetime.busy_until = sim->now() + delay;
  lifetime.activity += delay;
  sim->schedule(delay, this, activity ? activity->priority : 0);
  goto end;

finish:
  terminate(true);
end:
  return;
}

void Arrival::activate() {
  Process::activate();
  lifetime.busy_until = sim->now() + lifetime.remaining;
  sim->schedule(lifetime.remaining, this, 1);
  lifetime.remaining = 0;
}

void Arrival::deactivate() {
  Process::deactivate();
  lifetime.remaining = lifetime.busy_until - sim->now();
  if (lifetime.remaining && order.get_restart()) {
    lifetime.activity -= lifetime.remaining;
    lifetime.remaining = 0;
    activity = activity->get_prev();
  }
}

void Arrival::leave(std::string resource) {
  sim->record_release(name, restime[resource].start, restime[resource].activity, resource);
}

void Arrival::leave(std::string resource, double start, double activity) {
  sim->record_release(name, start, activity, resource);
}

void Arrival::terminate(bool finished) {
  foreach_ (ResMSet::value_type& itr, resources)
    itr->erase(this, true);
  lifetime.activity -= lifetime.remaining;
  if (is_monitored() >= 1)
    sim->record_end(name, lifetime.start, lifetime.activity, finished);
  delete this;
}

void Arrival::renege(Activity* next) {
  bool ret = false;
  timer = NULL;
  if (batch) {
    if (batch->is_permanent())
      return;
    ret = true;
    batch->erase(this);
  }
  while (resources.begin() != resources.end())
    ret |= (*resources.begin())->erase(this);
  if (!ret)
    Process::deactivate();
  lifetime.remaining = lifetime.busy_until - sim->now();
  if (next) {
    activity = next;
    sim->schedule(0, this);
  } else terminate(false);
}

int Arrival::set_attribute(std::string key, double value) {
  attributes[key] = value;
  if (is_monitored() >= 2)
    sim->record_attribute(name, key, value);
  return 0;
}

double Arrival::get_start(std::string name) {
  double start = restime[name].start;
  if (batch) {
    double up = batch->get_start(name);
    if (up >= 0 && (start < 0 || up < start))
      start = up;
  }
  return start;
}

void Arrival::set_timeout(double timeout, Activity* next) {
  cancel_timeout();
  timer = new Task(sim, "Renege-Timer", boost::bind(&Arrival::renege, this, next));
  sim->schedule(timeout, timer, PRIORITY_MIN);
}

void Batched::terminate(bool finished) {
  lifetime.activity -= lifetime.remaining;
  foreach_ (VEC<Arrival*>::value_type& itr, arrivals) {
    itr->set_activity(itr->get_activity() + lifetime.activity);
    itr->terminate(finished);
  }
  arrivals.clear();
  delete this;
}

void Batched::pop_all(Activity* next) {
  foreach_ (VEC<Arrival*>::value_type& itr, arrivals) {
    itr->set_activity(itr->get_activity() + lifetime.activity);
    itr->set_activity(next);
    itr->unregister_entity(this);
    sim->schedule(0, itr);
  }
  arrivals.clear();
}

int Batched::set_attribute(std::string key, double value) {
  attributes[key] = value;
  foreach_ (VEC<Arrival*>::value_type& itr, arrivals)
    itr->set_attribute(key, value);
  return 0;
}

void Batched::erase(Arrival* arrival) {
  bool del = activity;
  if (arrivals.size() > 1 || (batch && batch->is_permanent())) {
    del = false;
    if (arrival->is_monitored()) {
      Batched* up = this;
      while (up) {
        up->report(arrival);
        up = up->batch;
      }
    }
  } else if (arrivals.size() == 1 && !batch) {
    bool ret = !activity;
    while (resources.begin() != resources.end())
      ret |= (*resources.begin())->erase(this);
    if (!ret)
      Process::deactivate();
  } else batch->erase(this);
  arrivals.erase(std::remove(arrivals.begin(), arrivals.end(), arrival), arrivals.end());
  arrival->unregister_entity(this);
  if (del) delete this;
}

void Batched::report(Arrival* arrival) {
  foreach_ (ResMSet::value_type& itr, resources) {
    double last = get_activity(itr->name);
    arrival->leave(itr->name, restime[itr->name].start, sim->now() - last);
  }
}
