#ifndef ACTIVITY_H
#define ACTIVITY_H

#include "simmer.h"
#include "policy.h"

// forward declarations
class Arrival;

/** 
 *  Base class.
 */
class Activity {
public:
  BASE_CLONEABLE(Activity)
  
  std::string name;
  bool verbose;
  bool provide_attrs;
  int n;
  int priority;
  
  /**
   * Constructor.
   * @param name          the name of the activity
   * @param resource      the resource associated
   * @param provide_attrs whether the activity should expose the arrival's attributes
   * @param priority      resource accessing priority
   */
  Activity(std::string name, bool verbose, bool provide_attrs, int priority = 0): 
    name(name), verbose(verbose), provide_attrs(provide_attrs), n(1), 
    priority(priority), next(NULL), prev(NULL) {}
  
  virtual ~Activity(){}
  
  /**
   * Print the activity info.
   * @param indent number of spaces at the beginning of each line
   */
  virtual void print(int indent=0, bool brief=false) {
    if (!brief) {
      for (int i = 0; i < indent; ++i) Rcpp::Rcout << " ";
      Rcpp::Rcout << "{ Activity: " << FMT(12, left) << name << " | ";
      if (verbose) {
        Rcpp::Rcout << FMT(9, right) << prev;
        Rcpp::Rcout << " <- " << FMT(9, right) << this << " -> ";
        Rcpp::Rcout << FMT(9, left) << next << " | ";
      }
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
};

/**
 * Seize a resource.
 */
template <typename T>
class Seize: public Activity {
public:
  CLONEABLE(Seize<T>)
  
  Seize(bool verbose, std::string resource, T amount, bool provide_attrs, 
        int priority, int preemptible, bool restart):
    Activity("Seize", verbose, provide_attrs, std::abs(priority)), resource(resource),
    amount(amount), preemptible(std::abs(preemptible)), restart(restart) {}
  
  virtual void print(int indent=0, bool brief=false);
  virtual double run(Arrival* arrival);
  
protected:
  std::string resource;
  T amount;
  int preemptible;
  bool restart;
};

/**
 * Seize a selected resource.
 */
template <typename T>
class SeizeSelected: public Seize<T> {
public:
  CLONEABLE(SeizeSelected<T>)
  
  SeizeSelected(bool verbose, int id, T amount, bool provide_attrs, 
                int priority, int preemptible, bool restart):
    Seize<T>(verbose, "[]", amount, provide_attrs, priority, preemptible, restart),
    id(id) {}
  
  double run(Arrival* arrival);
  
protected:
  int id;
};

/**
 * Release a resource.
 */
template <typename T>
class Release: public Activity {
public:
  CLONEABLE(Release<T>)
  
  Release(bool verbose, std::string resource, T amount, bool provide_attrs):
    Activity("Release", verbose, provide_attrs, PRIORITY_RELEASE), 
    resource(resource), amount(amount) {}
  
  virtual void print(int indent=0, bool brief=false);
  virtual double run(Arrival* arrival);
  
protected:
  std::string resource;
  T amount;
};

/**
 * Release a selected resource.
 */
template <typename T>
class ReleaseSelected: public Release<T> {
public:
  CLONEABLE(ReleaseSelected<T>)
  
  ReleaseSelected(bool verbose, int id, T amount, bool provide_attrs):
    Release<T>(verbose, "<selected>", amount, provide_attrs), id(id) {}
  
  double run(Arrival* arrival);
  
protected:
  int id;
};

/**
 * Select a resource based on some policy.
 */
template <typename T>
class Select: public Activity {
public:
  CLONEABLE(Select<T>)
  
  Select(bool verbose, T resources, bool provide_attrs, std::string policy, int id):
    Activity("Select", verbose, provide_attrs), resources(resources),
    policy(policy), id(id), dispatcher(Policy(resources, policy)) {}
  
  void print(int indent=0, bool brief=false);
  double run(Arrival* arrival);
  
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
class SetAttribute: public Activity {
public:
  CLONEABLE(SetAttribute<T>)
  
  SetAttribute(bool verbose, std::string key, T value, bool provide_attrs):
    Activity("SetAttribute", verbose, provide_attrs), key(key), value(value) {}
  
  void print(int indent=0, bool brief=false);
  double run(Arrival* arrival);
  
protected:
  std::string key;
  T value;
};

/**
 * Timeout.
 */
template <typename T>
class Timeout: public Activity {
public:
  CLONEABLE(Timeout<T>)
  
  Timeout(bool verbose, T delay, bool provide_attrs):
    Activity("Timeout", verbose, provide_attrs), delay(delay) {}
  
  void print(int indent=0, bool brief=false);
  double run(Arrival* arrival);
  
protected:
  T delay;
};

/**
 * Branch. It runs as another activity, but encloses other trajectories
 * that are selected at runtime through a user-defined function.
 */
class Branch: public Activity {
public:
  CLONEABLE(Branch)
  
  Branch(bool verbose, Rcpp::Function option, bool provide_attrs, VEC<bool> cont, VEC<Rcpp::Environment> trj):
    Activity("Branch", verbose, provide_attrs), option(option), cont(cont), trj(trj), selected(NULL) {
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
  
  Branch(const Branch& o): Activity("Branch", o.verbose, o.provide_attrs), 
    option(o.option), cont(o.cont), trj(o.trj), selected(NULL) {
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
  
  void print(int indent=0, bool brief=false) {
    if (!brief)
      for (unsigned int i = 0; i < trj.size(); i++) {
        Activity::print(indent, brief);
        Rcpp::Rcout << "continue: " << cont[i] << " }" << std::endl;
        Rcpp::Function print(trj[i]["print"]);
        print(indent+2);
      }
    else Rcpp::Rcout << trj.size() << " options" << std::endl;
  }
  
  double run(Arrival* arrival);
  
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
  Rcpp::Function option;
  VEC<bool> cont;
  VEC<Rcpp::Environment> trj;
  Activity* selected;
  VEC<Activity*> heads;
  VEC<Activity*> tails;
};

/**
 * Rollback to a previous activity.
 */
template <typename T>
class Rollback: public Activity {
public:
  CLONEABLE(Rollback<T>)
  
  Rollback(bool verbose, int amount, T times, bool provide_attrs):
    Activity("Rollback", verbose, provide_attrs), amount(std::abs(amount)), times(times),
    cached(NULL), selected(NULL) {}
  
  Rollback(const Rollback& o): Activity("Rollback", o.verbose, "-", o.provide_attrs), 
    amount(o.amount), times(o.times), cached(NULL), selected(NULL) {
    pending.clear();
  }
  
  void print(int indent=0, bool brief=false);
  double run(Arrival* arrival);
  
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
  T times;
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
class Leave: public Activity {
public:
  CLONEABLE(Leave<T>)
  
  Leave(bool verbose, T prob, bool provide_attrs):
    Activity("Leave", verbose, provide_attrs), prob(prob) {}
  
  void print(int indent=0, bool brief=false);
  double run(Arrival* arrival);
  
protected:
  T prob;
};

#endif
