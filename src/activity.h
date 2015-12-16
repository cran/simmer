#ifndef ACTIVITY_H
#define ACTIVITY_H

#include "simmer.h"

// forward declarations
class Arrival;

/** 
 *  Base class.
 */
class Activity {
public:
  std::string name;
  std::string resource;
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
  Activity(std::string name, std::string resource, bool provide_attrs, int priority = 0): 
    name(name), resource(resource), provide_attrs(provide_attrs), 
    n(1), priority(priority), next(NULL), prev(NULL)  {}
  
  virtual ~Activity(){}
  
  /**
   * Print the activity info.
   * @param indent number of spaces at the beginning of each line
   */
  virtual void print(int indent=0) {
    for (int i = 0; i < indent; ++i)
      Rcpp::Rcout << " ";
    Rcpp::Rcout << "{ Activity: " << name << "(" << resource << ") | ";
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
  
private:
  Activity* next;
  Activity* prev;
};

/**
 * Seize a resource.
 */
template <class T>
class Seize: public Activity {
public:
  Seize(std::string resource, T amount, bool provide_attrs, int priority):
    Activity("Seize", resource, provide_attrs, fabs(priority)), amount(amount) {}
  
  void print(int indent=0);
  double run(Arrival* arrival);
  
private:
  T amount;
};

/**
 * Release a resource.
 */
template <class T>
class Release: public Activity {
public:
  Release(std::string resource, T amount, bool provide_attrs):
    Activity("Release", resource, provide_attrs, -1), amount(amount) {}
  
  void print(int indent=0);
  double run(Arrival* arrival);
  
private:
  T amount;
};

/**
 * Set attributes.
 */
template <class T>
class SetAttribute: public Activity {
public:
  SetAttribute(std::string key, T value, bool provide_attrs):
    Activity("SetAttribute", "none", provide_attrs), key(key), value(value) {}
  
  void print(int indent=0);
  double run(Arrival* arrival);
  
private:
  std::string key;
  T value;
};

/**
 * Timeout.
 */
template <class T>
class Timeout: public Activity {
public:
  Timeout(T delay, bool provide_attrs):
    Activity("Timeout", "none", provide_attrs), delay(delay) {}
  
  void print(int indent=0);
  double run(Arrival* arrival);
  
private:
  T delay;
};

/**
 * Branch. It runs as another activity, but encloses other trajectories
 * that are selected at runtime through a user-defined function.
 */
class Branch: public Activity {
public:
  Branch(Rcpp::Function option, VEC<bool> merge, VEC<Rcpp::Environment> trj):
    Activity("Branch", "none", 0), option(option), merge(merge), trj(trj), selected(NULL) {
    n = 0;
    for (unsigned int i = 0; i < trj.size(); i++) {
      Rcpp::Function get_head(trj[i]["get_head"]);
      path.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(get_head()));
      if (merge[i]) {
        Rcpp::Function get_tail(trj[i]["get_tail"]);
        Rcpp::XPtr<Activity> tail(get_tail());
        tail->set_next(this);
      }
      Rcpp::Function get_n_activities(trj[i]["get_n_activities"]);
      n += Rcpp::as<int>(get_n_activities());
    }
  }
  
  ~Branch() {
    merge.clear();
    path.clear();
    pending.clear();
  }
  
  void print(int indent=0) {
    for (unsigned int i = 0; i < trj.size(); i++) {
      Activity::print(indent);
      Rcpp::Rcout << "merge: " << merge[i] << " }" << std::endl;
      Rcpp::Function print(trj[i]["print"]);
      print(indent+2);
    }
  }
  
  double run(Arrival* arrival) {
    if (pending.find(arrival) != pending.end())
      pending.erase(arrival);
    else {
      unsigned int i = Rcpp::as<unsigned int>(option());
      if (i < 1 || i > path.size())
        Rcpp::stop("index out of range");
      selected = path[i-1];
      if (merge[i-1])
        pending.insert(arrival);
    }
    return 0;
  }
  
  Activity* get_next() {
    if (selected) {
      Activity* aux = selected;
      selected = NULL;
      return aux;
    } else 
      return Activity::get_next();
  }
  
private:
  Rcpp::Function option;
  VEC<bool> merge;
  VEC<Rcpp::Environment> trj;
  Activity* selected;
  VEC<Activity*> path;
  SET<Arrival*> pending;
};

/**
 * Rollback to a previous activity.
 */
template <class T>
class Rollback: public Activity {
public:
  Rollback(int amount, T times, bool provide_attrs):
    Activity("Rollback", "none", provide_attrs), amount(fabs(amount)), times(times),
    cached(NULL), selected(NULL) {}
  
  ~Rollback() { pending.clear(); }
  
  void print(int indent=0);
  double run(Arrival* arrival);
  
  Activity* get_next() {
    if (selected) {
      Activity* aux = selected;
      selected = NULL;
      return aux;
    } else 
      return Activity::get_next();
  }
  
private:
  int amount;
  T times;
  Activity* cached, *selected;
  MAP<Arrival*, int> pending;
  
  inline Activity* goback() {
    int n = amount;
    Activity* ptr = this;
    while (ptr->get_prev() && n--)
      ptr = ptr->get_prev();
    return ptr;
  }
};

#endif
