#ifndef ACTIVITY_H
#define ACTIVITY_H

#include "simmer.h"
#include "simulator.h"
#include "policy.h"

/**
 *  Base class.
 */
class Activity {
public:
  BASE_CLONEABLE(Activity)

  std::string name;
  bool verbose;
  VEC<int> provide_attrs;
  int n;
  int priority;

  /**
   * Constructor.
   * @param name          the name of the activity
   * @param resource      the resource associated
   * @param provide_attrs whether the activity should expose the arrival's attributes
   * @param priority      simulation priority
   */
  Activity(std::string name, bool verbose,
           VEC<int> provide_attrs = VEC<int>(0), int priority = 0)
    : name(name), verbose(verbose), provide_attrs(provide_attrs), n(1),
      priority(priority), next(NULL), prev(NULL) {}

  virtual ~Activity() {}

  /**
   * Print the activity info.
   * @param indent number of spaces at the beginning of each line
   */
  virtual void print(int indent = 0, bool brief = false) {
    if (!brief) {
      for (int i = 0; i < indent; ++i)
        Rcpp::Rcout << " ";
      Rcpp::Rcout << "{ Activity: " << FMT(12, left) << name << " | ";
      if (verbose) Rcpp::Rcout <<
        FMT(9, right) << prev << " <- " <<
        FMT(9, right) << this << " -> " <<
        FMT(9, left) << next << " | ";
    }
  }

  /**
   * Run the activity.
   * @param arrival a pointer to the calling arrival
   */
  virtual double run(Arrival* arrival) = 0;

  /**
   * Getter/setter for the next activity in the chain.
   */
  virtual Activity* get_next() { return next; }
  virtual void set_next(Activity* activity) { next = activity; }

  /**
   * Getter/setter for the previous activity in the chain.
   */
  virtual Activity* get_prev() { return prev; }
  virtual void set_prev(Activity* activity) { prev = activity; }

protected:
  Activity* next;
  Activity* prev;

  template <typename T>
  T get(T var, int index, Arrival* arrival) { return var; }

  template <typename T>
  T get(Rcpp::Function call, int index, Arrival* arrival) {
    switch (provide_attrs[index]) {
    case 1:
      return Rcpp::as<T>(call(*arrival->get_attributes()));
    case 2:
      return Rcpp::as<T>(call(*arrival->get_attributes(),
                              *arrival->sim->get_attributes()));
    default:
      return Rcpp::as<T>(call());
    }
  }
};

// abstract class for multipath activities
class Fork : public Activity {
public:
  Fork(std::string name, bool verbose, VEC<bool> cont, VEC<Rcpp::Environment> trj,
       VEC<int> provide_attrs = VEC<int>(0), int priority = 0)
    : Activity(name, verbose, provide_attrs, priority),
      cont(cont), trj(trj), selected(NULL)
  {
    foreach_ (VEC<Rcpp::Environment>::value_type& itr, trj) {
      Rcpp::Function get_head(itr["get_head"]);
      Rcpp::Function get_tail(itr["get_tail"]);
      heads.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(get_head()));
      tails.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(get_tail()));
      Rcpp::Function get_n_activities(itr["get_n_activities"]);
      n += Rcpp::as<int>(get_n_activities());
    }
    foreach_ (VEC<Activity*>::value_type& itr, heads)
      itr->set_prev(this);
  }

  Fork(const Fork& o)
    : Activity(o.name, o.verbose, o.provide_attrs, o.priority),
      cont(o.cont), trj(o.trj), selected(NULL)
  {
    heads.clear();
    tails.clear();
    foreach_ (VEC<Rcpp::Environment>::value_type& itr, trj) {
      Rcpp::Function clone(itr["clone"]);
      itr = clone();
      Rcpp::Function get_head(itr["get_head"]);
      Rcpp::Function get_tail(itr["get_tail"]);
      heads.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(get_head()));
      tails.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(get_tail()));
    }
    foreach_ (VEC<Activity*>::value_type& itr, heads)
      itr->set_prev(this);
  }

  void print(int indent = 0, bool brief = false) {
    indent += 2;
    if (!brief) {
      if (indent > 10) return; // max 6 levels
      for (unsigned int i = 0; i < trj.size(); i++) {
        for (int j = 0; j < indent; ++j) Rcpp::Rcout << " ";
        Rcpp::Rcout << "Fork " << i+1 << (cont[i] ? ", continue," : ", stop,");
        Rcpp::Function print(trj[i]["print"]);
        print(indent);
      }
    } else Rcpp::Rcout << trj.size() << " paths" << std::endl;
  }

  void set_next(Activity* activity) {
    Activity::set_next(activity);
    for (unsigned int i = 0; i < tails.size(); i++) {
      if (cont[i]) tails[i]->set_next(activity);
    }
  }

  Activity* get_next() {
    if (selected) {
      Activity* aux = selected;
      selected = NULL;
      return aux;
    }
    return Activity::get_next();
  }

protected:
  VEC<bool> cont;
  VEC<Rcpp::Environment> trj;
  Activity* selected;
  VEC<Activity*> heads;
  VEC<Activity*> tails;
};

// abstract class for resource retrieval
class ResGetter {
public:
  BASE_CLONEABLE(ResGetter)

  ResGetter(std::string resource) : resource(resource) {}

protected:
  std::string resource;

  virtual Resource* get_resource(Arrival* arrival) {
    return arrival->sim->get_resource(resource);
  }
};

/**
 * Seize a resource.
 */
template <typename T>
class Seize : public Fork, public ResGetter {
public:
  CLONEABLE(Seize<T>)

  Seize(bool verbose, std::string resource, T amount, int provide_attrs,
        VEC<bool> cont, VEC<Rcpp::Environment> trj, unsigned short mask)
    : Fork("Seize", verbose, cont, trj, VEC<int>(1, provide_attrs)),
      ResGetter(resource), amount(amount), mask(mask) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "resource: " << resource << " | " << "amount: " << amount << " }" << std::endl;
    else Rcpp::Rcout << resource << ", " << amount << ", ";
    Fork::print(indent, brief);
  }

  double run(Arrival* arrival) {
    int value = std::abs(get<int>(amount, 0, arrival));
    return select_path(arrival, get_resource(arrival)->seize(arrival, value));
  }

protected:
  T amount;
  unsigned short mask;

  int select_path(Arrival* arrival, int ret) {
    switch (ret) {
    case REJECT:
      if (mask & 2) {
        ret = SUCCESS;
        if (mask & 1)
          selected = heads[1];
        else
          selected = heads[0];
      } else arrival->terminate(false);
      break;
    default:
      if (mask & 1)
        selected = heads[0];
      break;
    }
    return ret;
  }
};

/**
 * Seize a selected resource.
 */
template <typename T>
class SeizeSelected : public Seize<T> {
public:
  CLONEABLE(SeizeSelected<T>)

  SeizeSelected(bool verbose, int id, T amount, int provide_attrs,
                VEC<bool> cont, VEC<Rcpp::Environment> trj, unsigned short mask)
    : Seize<T>(verbose, "[]", amount, provide_attrs, cont, trj, mask), id(id) {}

protected:
  int id;

  Resource* get_resource(Arrival* arrival) {
    return arrival->get_selected(id);
  }
};

/**
 * Release a resource.
 */
template <typename T>
class Release : public Activity, public ResGetter {
public:
  CLONEABLE(Release<T>)

  Release(bool verbose, std::string resource, T amount, int provide_attrs)
    : Activity("Release", verbose, VEC<int>(1, provide_attrs), PRIORITY_RELEASE),
      ResGetter(resource), amount(amount) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "resource: " << resource << " | " << "amount: " << amount << " }" << std::endl;
    else Rcpp::Rcout << resource << ", " << amount << std::endl;
  }

  double run(Arrival* arrival) {
    int value = std::abs(get<int>(amount, 0, arrival));
    return get_resource(arrival)->release(arrival, value);
  }

protected:
  T amount;
};

/**
 * Release a selected resource.
 */
template <typename T>
class ReleaseSelected : public Release<T> {
public:
  CLONEABLE(ReleaseSelected<T>)

  ReleaseSelected(bool verbose, int id, T amount, int provide_attrs)
    : Release<T>(verbose, "[]", amount, provide_attrs), id(id) {}

protected:
  int id;

  Resource* get_resource(Arrival* arrival) {
    return arrival->get_selected(id);
  }
};

/**
 * Set a resource's capacity.
 */
template <typename T>
class SetCapacity : public Activity, public ResGetter {
public:
  CLONEABLE(SetCapacity<T>)

  SetCapacity(bool verbose, std::string resource, T value, int provide_attrs)
    : Activity("SetCapacity", verbose, VEC<int>(1, provide_attrs)),
      ResGetter(resource), value(value) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "resource: " << resource << ", value: " << value << " }" << std::endl;
    else Rcpp::Rcout << resource << ", " << value << std::endl;
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(value, 0, arrival));
    if (ret == R_PosInf) ret = -1;
    get_resource(arrival)->set_capacity((int)ret);
    return 0;
  }

protected:
  T value;
};

/**
* Set a selected resource's capacity.
*/
template <typename T>
class SetCapacitySelected : public SetCapacity<T> {
public:
  CLONEABLE(SetCapacitySelected<T>)

  SetCapacitySelected(bool verbose, int id, T value, int provide_attrs)
    : SetCapacity<T>(verbose, "[]", value, provide_attrs), id(id) {}

protected:
  int id;

  Resource* get_resource(Arrival* arrival) {
    return arrival->get_selected(id);
  }
};

/**
* Set a resource's queue size.
*/
template <typename T>
class SetQueue : public Activity, public ResGetter {
public:
  CLONEABLE(SetQueue<T>)

  SetQueue(bool verbose, std::string resource, T value, int provide_attrs)
    : Activity("SetQueue", verbose, VEC<int>(1, provide_attrs)),
      ResGetter(resource), value(value) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "resource: " << resource << ", value: " << value << " }" << std::endl;
    else Rcpp::Rcout << resource << ", " << value << std::endl;
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(value, 0, arrival));
    if (ret == R_PosInf) ret = -1;
    get_resource(arrival)->set_queue_size((int)ret);
    return 0;
  }

protected:
  T value;
};

/**
* Set a selected resource's queue size.
*/
template <typename T>
class SetQueueSelected : public SetQueue<T> {
public:
  CLONEABLE(SetQueueSelected<T>)

  SetQueueSelected(bool verbose, int id, T value, int provide_attrs)
    : SetQueue<T>(verbose, "[]", value, provide_attrs), id(id) {}

protected:
  int id;

  Resource* get_resource(Arrival* arrival) {
    return arrival->get_selected(id);
  }
};

/**
 * Select a resource based on some policy.
 */
template <typename T>
class Select : public Activity {
public:
  CLONEABLE(Select<T>)

  Select(bool verbose, T resources, int provide_attrs, std::string policy, int id)
    : Activity("Select", verbose, VEC<int>(1, provide_attrs)), resources(resources),
      policy(policy), id(id), dispatcher(Policy(resources, policy)) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "resources: " << resources << ", policy: " << policy << " }" << std::endl;
    else Rcpp::Rcout << resources << ", " << policy << std::endl;
  }

  double run(Arrival* arrival) {
    Resource* selected;
    if (typeid(T) == typeid(Rcpp::Function)) {
      VEC<std::string> res = get<VEC<std::string> >(resources, 0, arrival);
      selected = arrival->sim->get_resource(res[0]);
    } else selected = dispatcher.dispatch(arrival->sim);
    arrival->set_selected(id, selected);
    return 0;
  }

protected:
  T resources;
  std::string policy;
  int id;
  Policy dispatcher;
};

/**
 * Set attributes.
 */
template <typename T>
class SetAttribute : public Activity {
public:
  CLONEABLE(SetAttribute<T>)

  SetAttribute(bool verbose, std::string key, T value, int provide_attrs, bool global)
    : Activity("SetAttribute", verbose, VEC<int>(1, provide_attrs)),
      key(key), value(value), global(global) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "key: " << key << ", value: " << value <<
      "global: " << global << " }" << std::endl;
    else Rcpp::Rcout << key << ", " << value << ", " << global << std::endl;
  }

  double run(Arrival* arrival) {
    double ret = get<double>(value, 0, arrival);
    if (global)
      arrival->sim->set_attribute(key, ret);
    else arrival->set_attribute(key, ret);
    return 0;
  }

protected:
  std::string key;
  T value;
  bool global;
};

/**
 * Activate a generator.
 */
template <typename T>
class Activate : public Activity {
public:
  CLONEABLE(Activate<T>)

  Activate(bool verbose, T generator, int provide_attrs)
    : Activity("Activate", verbose, VEC<int>(1, provide_attrs), PRIORITY_MAX),
      generator(generator) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "generator: " << generator << " }" << std::endl;
    else Rcpp::Rcout << generator << std::endl;
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(generator, 0, arrival);
    arrival->sim->get_generator(ret)->activate();
    return 0;
  }

protected:
  T generator;
};

/**
 * Deactivate a generator.
 */
template <typename T>
class Deactivate : public Activity {
public:
  CLONEABLE(Deactivate<T>)

  Deactivate(bool verbose, T generator, int provide_attrs)
    : Activity("Deactivate", verbose, VEC<int>(1, provide_attrs), PRIORITY_MAX),
      generator(generator) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "generator: " << generator << " }" << std::endl;
    else Rcpp::Rcout << generator << std::endl;
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(generator, 0, arrival);
    arrival->sim->get_generator(ret)->deactivate();
    return 0;
  }

protected:
  T generator;
};

/**
 * Set a generator's trajectory.
 */
template <typename T>
class SetTraj : public Activity {
public:
  CLONEABLE(SetTraj<T>)

  SetTraj(bool verbose, T generator, int provide_attrs, Rcpp::Environment trj)
    : Activity("SetTraj", verbose, VEC<int>(1, provide_attrs), PRIORITY_MAX),
      generator(generator), trj(trj) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "generator: " << generator << ", trajectory: " << trj << " }" << std::endl;
    else Rcpp::Rcout << generator << ", " << trj << std::endl;
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(generator, 0, arrival);
    arrival->sim->get_generator(ret)->set_trajectory(trj);
    return 0;
  }

protected:
  T generator;
  Rcpp::Environment trj;
};

/**
 * Set a generator's distribution.
 */
template <typename T>
class SetDist : public Activity {
public:
  CLONEABLE(SetDist<T>)

  SetDist(bool verbose, T generator, int provide_attrs, Rcpp::Function dist)
    : Activity("SetDist", verbose, VEC<int>(1, provide_attrs), PRIORITY_MAX),
      generator(generator), dist(dist) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "generator: " << generator << ", distribution: " << dist << " }" << std::endl;
    else Rcpp::Rcout << generator << ", " << dist << std::endl;
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(generator, 0, arrival);
    arrival->sim->get_generator(ret)->set_distribution(dist);
    return 0;
  }

protected:
  T generator;
  Rcpp::Function dist;
};

/**
 * Set prioritization.
 */
template <typename T>
class SetPrior : public Activity {
public:
  CLONEABLE(SetPrior<T>)

  SetPrior(bool verbose, T values, int provide_attrs)
    : Activity("SetPrior", verbose, VEC<int>(1, provide_attrs)), values(values) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "values: " << values << " }" << std::endl;
    else Rcpp::Rcout << values << std::endl;
  }

  double run(Arrival* arrival) {
    VEC<int> ret = get<VEC<int> >(values, 0, arrival);
    if (ret.size() != 3)
      Rcpp::stop("%s: 3 values needed", name);
    if (ret[0] >= 0) arrival->order.set_priority(ret[0]);
    if (ret[1] >= 0) arrival->order.set_preemptible(ret[1]);
    if (ret[2] >= 0) arrival->order.set_restart((bool)ret[2]);
    return 0;
  }

protected:
  T values;
};

/**
 * Timeout.
 */
template <typename T>
class Timeout : public Activity {
public:
  CLONEABLE(Timeout<T>)

  Timeout(bool verbose, T delay, int provide_attrs)
    : Activity("Timeout", verbose, VEC<int>(1, provide_attrs)), delay(delay) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout << "delay: " << delay << " }" << std::endl;
    else Rcpp::Rcout << delay << std::endl;
  }

  double run(Arrival* arrival) {
    double value = get<double>(delay, 0, arrival);
    return std::abs(value);
  }

protected:
  T delay;
};

/**
 * Branch. It runs as another activity, but encloses other trajectories
 * that are selected at runtime through a user-defined function.
 */
class Branch : public Fork {
public:
  CLONEABLE(Branch)

  Branch(bool verbose, Rcpp::Function option, int provide_attrs,
         VEC<bool> cont, VEC<Rcpp::Environment> trj)
    : Fork("Branch", verbose, cont, trj, VEC<int>(1, provide_attrs)), option(option) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "option: " << option << " }" << std::endl;
    else Rcpp::Rcout << option << ", ";
    Fork::print(indent, brief);
  }

  double run(Arrival* arrival) {
    int ret = get<int>(option, 0, arrival);
    if (ret < 0 || ret > (int)heads.size())
      Rcpp::stop("%s: index out of range", name);
    if (ret) selected = heads[ret-1];
    return 0;
  }

protected:
  Rcpp::Function option;
};

/**
 * Rollback to a previous activity.
 */
class Rollback : public Activity {
public:
  CLONEABLE(Rollback)

  Rollback(bool verbose, int amount, int times,
           OPT<Rcpp::Function> check = NONE, int provide_attrs = false)
    : Activity("Rollback", verbose, VEC<int>(1, provide_attrs)), amount(std::abs(amount)),
      times(times), check(check), cached(NULL), selected(NULL) {}

  Rollback(const Rollback& o)
    : Activity(o.name, o.verbose, o.provide_attrs), amount(o.amount),
      times(o.times), check(o.check), cached(NULL), selected(NULL)
  {
    pending.clear();
  }

  void print(int indent = 0, bool brief = false) {
    if (!cached) cached = goback();
    Activity::print(indent, brief);
    if (!brief) {
      Rcpp::Rcout << "amount: " << amount << " (" << cached->name << "), ";
      if (check) Rcpp::Rcout << "check: " << *check;
      else {
        if (times >= 0) Rcpp::Rcout << "times: " << times;
        else Rcpp::Rcout << "times: Inf";
      }
      Rcpp::Rcout << " }" << std::endl;
    } else Rcpp::Rcout << cached->name << std::endl;
  }

  double run(Arrival* arrival) {
    if (check) {
      if (!get<bool>(*check, 0, arrival))
        return 0;
    } else if (times >= 0) {
      if (pending.find(arrival) == pending.end())
        pending[arrival] = times;
      if (!pending[arrival]) {
        pending.erase(arrival);
        return 0;
      }
      pending[arrival]--;
    }
    if (!cached) cached = goback();
    selected = cached;
    return 0;
  }

  Activity* get_next() {
    if (selected) {
      Activity* aux = selected;
      selected = NULL;
      return aux;
    }
    return Activity::get_next();
  }

protected:
  int amount;
  int times;
  OPT<Rcpp::Function> check;
  Activity* cached, *selected;
  UMAP<Arrival*, int> pending;

  Activity* goback() {
    int n = amount;
    Activity* ptr = this;
    while (ptr->get_prev() && n--)
      ptr = ptr->get_prev();
    return ptr;
  }
};

/**
 * Leave the trajectory with some probability.
 */
template <typename T>
class Leave : public Activity {
public:
  CLONEABLE(Leave<T>)

  Leave(bool verbose, T prob, int provide_attrs)
    : Activity("Leave", verbose, VEC<int>(1, provide_attrs)), prob(prob) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "prob: " << prob << " }" << std::endl;
    else Rcpp::Rcout << prob << std::endl;
  }

  double run(Arrival* arrival) {
    if (Rcpp::runif(1)[0] > get<double>(prob, 0, arrival))
      return 0;
    arrival->terminate(false);
    return REJECT;
  }

protected:
  T prob;
};

/**
 * Clone an arrival.
 */
template <typename T>
class Clone : public Fork {
public:
  CLONEABLE(Clone<T>)

  Clone(bool verbose, T n, int provide_attrs, VEC<Rcpp::Environment> trj)
    : Fork("Clone", verbose, VEC<bool>(trj.size(), true),
      trj, VEC<int>(1, provide_attrs)), n(n) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "n: " << n << " }" << std::endl;
    else Rcpp::Rcout << n << ", ";
    Fork::print(indent, brief);
  }

  double run(Arrival* arrival) {
    int value = std::abs(get<int>(n, 0, arrival));
    for (int i = 1; i < value; i++) {
      if (i < (int)heads.size())
        selected = heads[i];
      Arrival* new_arrival = arrival->clone();
      new_arrival->set_activity(this->get_next());
      new_arrival->activate();
    }
    if (heads.size())
      selected = heads[0];
    return 0;
  }

protected:
  T n;
};

/**
 * Synchronize clones.
 */
class Synchronize : public Activity {
public:
  CLONEABLE(Synchronize)

  Synchronize(bool verbose, bool wait, bool terminate)
    : Activity("Synchronize", verbose), wait(wait), terminate(terminate) {}

  Synchronize(const Synchronize& o)
    : Activity(o.name, o.verbose), wait(o.wait), terminate(o.terminate)
  {
    pending.clear();
  }

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "wait: " << wait << " }" << std::endl;
    else Rcpp::Rcout << wait << std::endl;
  }

  double run(Arrival* arrival) {
    if (!wait) {
      UMAP<std::string, int>::iterator search = pending.find(arrival->name);
      if (search == pending.end()) {
        if (*(arrival->clones) > 1)
          pending.emplace(arrival->name, *(arrival->clones)-1);
        return 0;
      } else {
        search->second--;
        if (!search->second)
          pending.erase(search);
      }
    } else if (*(arrival->clones) == 1)
      return 0;

    if (!terminate)
      delete arrival;
    else
      arrival->terminate(true);
    return REJECT;
  }

protected:
  bool wait;
  bool terminate;
  UMAP<std::string, int> pending;
};

/**
 * Create a batch.
 */
class Batch : public Activity {
public:
  CLONEABLE(Batch)

  Batch(bool verbose, int n, double timeout, bool permanent, std::string id = "",
        OPT<Rcpp::Function> rule = NONE, int provide_attrs = false)
    : Activity("Batch", verbose, VEC<int>(1, provide_attrs)), n(n),
      timeout(std::abs(timeout)), permanent(permanent), id(id), rule(rule) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "n: " << n << ", timeout: " << timeout << ", permanent: " << permanent <<
      ", name: " << id << " }" << std::endl;
    else Rcpp::Rcout << n << ", " << timeout << ", " << permanent << ", " << id << std::endl;
  }

  double run(Arrival* arrival) {
    if (rule && !get<bool>(*rule, 0, arrival))
      return 0;
    Batched** ptr = arrival->sim->get_batch(this, id);
    if (!(*ptr))
      *ptr = init(arrival->sim);
    (*ptr)->insert(arrival);
    if ((int)(*ptr)->size() == n)
      trigger(arrival->sim, *ptr);
    return REJECT;
  }

protected:
  int n;
  double timeout;
  bool permanent;
  std::string id;
  OPT<Rcpp::Function> rule;

  Batched* init(Simulator* sim) {
    std::string str;
    Batched* ptr = NULL;
    if (id.size()) {
      str = "batch_" + id;
      ptr = new Batched(sim, str, permanent);
    } else {
      int count = sim->get_batch_count();
      str= "batch" + boost::lexical_cast<std::string>(count);
      ptr = new Batched(sim, str, permanent, count);
    }
    if (timeout) {
      Task* task = new Task(sim, "Batch-Timer",
                            boost::bind(&Batch::trigger, this, sim, ptr),
                            PRIORITY_MIN);
      task->activate(timeout);
    }
    return ptr;
  }

  void trigger(Simulator* sim, Batched* target) {
    Batched** ptr = sim->get_batch(this, id);
    if (!(*ptr) || *ptr != target)
      return;
    if ((*ptr)->size()) {
      (*ptr)->set_activity(this->get_next());
      (*ptr)->activate();
      *ptr = init((*ptr)->sim);
    } else {
      delete *ptr;
      *ptr = NULL;
    }
  }
};

/**
 * Separate a batch.
 */
class Separate : public Activity {
public:
  CLONEABLE(Separate)

  Separate(bool verbose) : Activity("Separate", verbose) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout << " }" << std::endl;
    else Rcpp::Rcout << std::endl;
  }

  double run(Arrival* arrival) {
    Batched* batched = dynamic_cast<Batched*>(arrival);
    if (!batched || batched->is_permanent())
      return 0;
    batched->pop_all(this->get_next());
    delete batched;
    return REJECT;
  }
};

/**
 * Renege after some time.
 */
template <typename T>
class RenegeIn : public Fork {
public:
  CLONEABLE(RenegeIn<T>)

  RenegeIn(bool verbose, T t, int provide_attrs, VEC<Rcpp::Environment> trj)
    : Fork("RenegeIn", verbose, VEC<bool>(trj.size(), false),
      trj, VEC<int>(1, provide_attrs)), t(t) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "t: " << t << " }" << std::endl;
    else Rcpp::Rcout << t << ", ";
    Fork::print(indent, brief);
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(t, 0, arrival));
    Activity* next = NULL;
    if (heads.size())
      next = heads[0];
    arrival->set_renege(ret, next);
    return 0;
  }

protected:
  T t;
};

/**
 * Renege if a signal is received.
 */
template <typename T>
class RenegeIf : public Fork {
public:
  CLONEABLE(RenegeIf<T>)

  RenegeIf(bool verbose, T signal, int provide_attrs, VEC<Rcpp::Environment> trj)
    : Fork("RenegeIf", verbose, VEC<bool>(trj.size(), false),
      trj, VEC<int>(1, provide_attrs)), signal(signal) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "signal: " << signal << " }" << std::endl;
    else Rcpp::Rcout << signal << ", ";
    Fork::print(indent, brief);
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(signal, 0, arrival);
    Activity* next = NULL;
    if (heads.size())
      next = heads[0];
    arrival->set_renege(ret, next);
    return 0;
  }

protected:
  T signal;
};

/**
 * Abort reneging.
 */
class RenegeAbort : public Activity {
public:
  CLONEABLE(RenegeAbort)

  RenegeAbort(bool verbose) : Activity("RenegeAbort", verbose) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout << " }" << std::endl;
    else Rcpp::Rcout << std::endl;
  }

  double run(Arrival* arrival) {
    arrival->cancel_renege();
    return 0;
  }
};

/**
 * Send signals.
 */
template <typename T, typename U>
class Send : public Activity {
public:
  CLONEABLE(Send<T COMMA U>)

  Send(bool verbose, T signals, U delay, VEC<int> provide_attrs)
    : Activity("Send", verbose, provide_attrs), signals(signals), delay(delay) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "signals: " << signals << ", delay: " << delay << " }" << std::endl;
    else Rcpp::Rcout << signals << ", " << delay << std::endl;
  }

  double run(Arrival* arrival) {
    VEC<std::string> sigs = get<VEC<std::string> >(signals, 0, arrival);
    double t = std::abs(get<double>(delay, 1, arrival));
    Task* task =
      new Task(arrival->sim, "Broadcast",
               boost::bind(&Simulator::broadcast, arrival->sim, sigs),
               PRIORITY_MIN);
    task->activate(t);
    return 0;
  }

protected:
  T signals;
  U delay;
};

/**
 * Subscribe to signals and assign a handler.
 */
template <typename T>
class Trap : public Fork {
public:
  CLONEABLE(Trap<T>)

  Trap(bool verbose, T signals, int provide_attrs,
       VEC<Rcpp::Environment> trj, bool interruptible)
    : Fork("Trap", verbose, VEC<bool>(trj.size(), false),
      trj, VEC<int>(1, provide_attrs)),
      signals(signals), interruptible(interruptible) {}

  Trap(const Trap& o)
    : Fork(o.name, o.verbose, o.cont, o.trj, o.provide_attrs),
      signals(o.signals), interruptible(o.interruptible)
  {
    pending.clear();
  }

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) {
      Rcpp::Rcout << "signals: " << signals << " }" << std::endl;
    } else Rcpp::Rcout << signals << ", ";
    Fork::print(indent, brief);
  }

  double run(Arrival* arrival) {
    if (!interruptible && pending.find(arrival) != pending.end()) {
      arrival->sim->subscribe(arrival);
      arrival->set_activity(pending[arrival]);
      pending.erase(arrival);
      arrival->activate();
      return REJECT;
    }
    VEC<std::string> sigs = get<VEC<std::string> >(signals, 0, arrival);
    arrival->sim->subscribe(sigs, arrival,
                            boost::bind(&Trap::launch_handler, this, arrival));
    return 0;
  }

protected:
  T signals;
  bool interruptible;
  UMAP<Arrival*, Activity*> pending;

  void launch_handler(Arrival* arrival) {
    if (!arrival->is_active())
      return;
    arrival->stop();
    if (heads.size()) {
      if (!interruptible) {
        arrival->sim->unsubscribe(arrival);
        pending[arrival] = arrival->get_activity();
        tails[0]->set_next(this);
      } else {
        tails[0]->set_next(arrival->get_activity());
      }
      arrival->set_activity(heads[0]);
    }
    arrival->activate();
  }
};

/**
 * Unsubscribe to signals.
 */
template <typename T>
class UnTrap : public Activity {
public:
  CLONEABLE(UnTrap<T>)

  UnTrap(bool verbose, T signals, int provide_attrs)
    : Activity("UnTrap", verbose, VEC<int>(1, provide_attrs)), signals(signals) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout <<
      "signals: " << signals << " }" << std::endl;
    else Rcpp::Rcout << signals << std::endl;
  }

  double run(Arrival* arrival) {
    VEC<std::string> sigs = get<VEC<std::string> >(signals, 0, arrival);
    arrival->sim->unsubscribe(sigs, arrival);
    return 0;
  }

protected:
  T signals;
};

/**
 * Block until a signal is received.
 */
class Wait : public Activity {
public:
  CLONEABLE(Wait)

  Wait(bool verbose) : Activity("Wait", verbose) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout << " }" << std::endl;
    else Rcpp::Rcout << std::endl;
  }

  double run(Arrival* arrival) { return BLOCK; }
};

/**
 * Print a message.
 */
template <typename T>
class Log : public Activity {
public:
  CLONEABLE(Log<T>)

  Log(bool verbose, T message, int provide_attrs)
    : Activity("Log", verbose, VEC<int>(1, provide_attrs)), message(message) {}

  void print(int indent = 0, bool brief = false) {
    Activity::print(indent, brief);
    if (!brief) Rcpp::Rcout << "message }" << std::endl;
    else Rcpp::Rcout << "message" << std::endl;
  }

  double run(Arrival* arrival) {
    Rcpp::Rcout << arrival->sim->now() << ": " << arrival->name << ": " <<
      get<std::string>(message, 0, arrival) << std::endl;
    return 0;
  }

protected:
  T message;
};

#endif
