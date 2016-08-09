#ifndef RESOURCE_H
#define RESOURCE_H

#include "process.h"

/** 
 *  Generic resource, a passive entity that comprises server + a queue.
 */
class Resource: public Entity {
public:
  /**
  * Constructor.
  * @param sim         a pointer to the simulator
  * @param name        the name
  * @param mon         int that indicates whether this entity must be monitored
  * @param capacity    server capacity (-1 means infinity)
  * @param queue_size  room in the queue (-1 means infinity)
  */
  Resource(Simulator* sim, std::string name, int mon, int capacity, int queue_size): 
    Entity(sim, name, mon), capacity(capacity), queue_size(queue_size), server_count(0), 
    queue_count(0) {}
  
  /**
  * Reset the resource: server, queue
  */
  void reset() {
    server_count = 0;
    queue_count = 0;
  }
  
  /**
  * Seize resources.
  * @param   arrival  a pointer to the arrival trying to seize resources
  * @param   amount   the amount of resources needed
  * 
  * @return  SUCCESS, ENQUEUED, REJECTED
  */
  int seize(Arrival* arrival, int amount);
  
  /**
  * Release resources.
  * @param   arrival a pointer to the arrival that releases resources
  * @param   amount  the amount of resources released
  * 
  * @return  SUCCESS
  */
  int release(Arrival* arrival, int amount);
  bool erase(Arrival* arrival);
  
  void set_capacity(int value);
  int get_capacity() { return capacity; }
  void set_queue_size(int value);
  int get_queue_size() { return queue_size; }
  int get_server_count() { return server_count; }
  int get_queue_count() { return queue_count; }
  
protected:
  int capacity;
  int queue_size;
  int server_count;     /**< number of arrivals being served */
  int queue_count;      /**< number of arrivals waiting */
  
  int post_release();
  
  void verbose_print(double time, std::string arrival, std::string status) {
    Rcpp::Rcout << 
      FMT(10, right) << time << " |" << FMT(12, right) << "resource: " << FMT(15, left) << name << "|" << 
        FMT(12, right) << "arrival: " << FMT(15, left) << arrival << "| " << status << std::endl;
  }
  
  virtual bool room_in_server(int amount, int priority) = 0;
  virtual bool room_in_queue(int amount, int priority) = 0;
  virtual bool try_free_server(bool verbose, double time) = 0;
  virtual bool try_serve_from_queue(bool verbose, double time) = 0;
  virtual void insert_in_server(bool verbose, double time, Arrival* arrival, int amount) = 0;
  virtual void insert_in_queue(bool verbose, double time, Arrival* arrival, int amount) = 0;
  virtual void remove_from_server(bool verbose, double time, Arrival* arrival, int amount) = 0;
  virtual bool remove_from_queue(bool verbose, double time, Arrival* arrival) = 0;
};

struct RSeize {
  double arrived_at;
  Arrival* arrival;
  int amount;
  
  RSeize(double arrived_at, Arrival* arrival, int amount):
    arrived_at(arrived_at), arrival(arrival), amount(amount) {}
  
  int priority() const { return arrival->order.get_priority(); }
  int preemptible() const { return arrival->order.get_preemptible(); }
  bool restart() const { return arrival->order.get_restart(); }
  double remaining() const { return arrival->get_remaining(); }
};

struct RQComp {
  bool operator()(const RSeize& lhs, const RSeize& rhs) const {
    if (lhs.priority() == rhs.priority()) {
      if (lhs.arrived_at == rhs.arrived_at)
        return lhs.remaining() > rhs.remaining();
      return lhs.arrived_at < rhs.arrived_at;
    }
    return lhs.priority() > rhs.priority();
  }
};

struct RSCompFIFO {
  bool operator()(const RSeize& lhs, const RSeize& rhs) const {
    if (lhs.preemptible() == rhs.preemptible())
      return lhs.arrived_at < rhs.arrived_at;
    return lhs.preemptible() < rhs.preemptible();
  }
};

struct RSCompLIFO {
  bool operator()(const RSeize& lhs, const RSeize& rhs) const {
    if (lhs.preemptible() == rhs.preemptible())
      return lhs.arrived_at > rhs.arrived_at;
    return lhs.preemptible() < rhs.preemptible();
  }
};

typedef MSET<RSeize, RQComp> RPQueue;
typedef UMAP<Arrival*, RPQueue::iterator> QueueMap;
typedef MSET<RSeize, RSCompFIFO> FIFO;
typedef MSET<RSeize, RSCompLIFO> LIFO;

/** 
*  Priority resource.
*/
template <typename T>
class PriorityRes: public Resource {
  typedef UMAP<Arrival*, typename T::iterator> ServerMap;
  
public:
  PriorityRes(Simulator* sim, std::string name, int mon, int capacity, int queue_size): 
    Resource(sim, name, mon, capacity, queue_size) {}
  
  ~PriorityRes() { reset(); }
  
  void reset() {
    Resource::reset();
    foreach_ (RPQueue::value_type& itr, queue)
      delete itr.arrival;
    queue.clear();
    queue_map.clear();
    server.clear();
    server_map.clear();
  }
  
protected:
  RPQueue queue;
  QueueMap queue_map;
  T server;
  ServerMap server_map;
  
  bool room_in_server(int amount, int priority) {
    if (capacity < 0) return true;
    return server_count + amount <= capacity;
  }
  
  bool room_in_queue(int amount, int priority) {
    if (queue_size < 0) return true;
    if (queue_count + amount <= queue_size) return true;
    int count = 0;
    foreach_r_ (RPQueue::value_type& itr, queue) {
      if (priority > itr.priority())
        count += itr.amount;
      else break;
      if (count >= amount) return true;
    }
    return false;
  }
  
  bool try_free_server(bool verbose, double time) { return false; }
  
  bool try_serve_from_queue(bool verbose, double time) {
    RPQueue::iterator next = queue.begin();
    if (room_in_server(next->amount, next->priority())) {
      if (next->arrival->is_monitored()) {
        double last = next->arrival->get_activity(this->name);
        next->arrival->set_activity(this->name, time - last);
      }
      next->arrival->activate();
      insert_in_server(verbose, time, next->arrival, next->amount);
      queue_count -= next->amount;
      queue_map.erase(next->arrival);
      queue.erase(next);
      return true;
    }
    return false;
  }
  
  void insert_in_server(bool verbose, double time, Arrival* arrival, int amount) {
    if (capacity > 0) while (server_count + amount > capacity)
      try_free_server(verbose, time);
    if (verbose) verbose_print(time, arrival->name, "SERVE");
    server_count += amount;
    typename ServerMap::iterator search = server_map.find(arrival);
    if (search == server_map.end())
      server_map[arrival] = server.emplace(time, arrival, amount);
    else search->second->amount += amount;
  }
  
  void insert_in_queue(bool verbose, double time, Arrival* arrival, int amount) {
    int count = 0;
    if (queue_size > 0) while (queue_count + amount > queue_size && amount > count) {
      RPQueue::iterator last = --queue.end();
      if (verbose) verbose_print(time, last->arrival->name, "REJECT");
      last->arrival->terminate(false);
      queue_count -= last->amount;
      count += last->amount;
      queue_map.erase(last->arrival);
      queue.erase(last);
    }
    if (verbose) verbose_print(time, arrival->name, "ENQUEUE");
    queue_count += amount;
    queue_map[arrival] = queue.emplace(time, arrival, amount);
  }
  
  void remove_from_server(bool verbose, double time, Arrival* arrival, int amount) {
    if (verbose) verbose_print(time, arrival->name, "DEPART");
    typename ServerMap::iterator search = server_map.find(arrival);
    if (search == server_map.end())
      Rcpp::stop("%s: release: not previously seized", name);
    if (search->second->amount < amount)
      Rcpp::stop("%s: release: incorrect amount (%d)", name, amount);
    else if (amount < 0 || amount == search->second->amount) {
      server_count -= search->second->amount;
      server.erase(search->second);
      server_map.erase(search);
    } else {
      server_count -= amount;
      search->second->amount -= amount;
    }
  }
  
  bool remove_from_queue(bool verbose, double time, Arrival* arrival) {
    QueueMap::iterator search = queue_map.find(arrival);
    if (search == queue_map.end()) return false;
    if (verbose) verbose_print(time, arrival->name, "DEPART");
    queue_count -= search->second->amount;
    queue.erase(search->second);
    queue_map.erase(search);
    return true;
  }
};

/** 
*  Preemptive resource.
*/
template <typename T>
class PreemptiveRes: public PriorityRes<T> {
public:
  PreemptiveRes(Simulator* sim, std::string name, int mon, int capacity, int queue_size, bool keep_queue):
    PriorityRes<T>(sim, name, mon, capacity, queue_size), keep_queue(keep_queue) {}
  
  ~PreemptiveRes() { reset(); }
  
  void reset() {
    PriorityRes<T>::reset();
    foreach_ (RPQueue::value_type& itr, preempted)
      delete itr.arrival;
    preempted.clear();
    preempted_map.clear();
  }
  
protected:
  bool keep_queue;
  RPQueue preempted;
  QueueMap preempted_map;
  
  bool room_in_server(int amount, int priority) {
    if (this->capacity < 0) return true;
    if (this->server_count + amount <= this->capacity) return true;
    int count = 0;
    foreach_ (typename T::value_type& itr, this->server) {
      if (priority > itr.preemptible())
        count += itr.amount;
      else break;
      if (count >= amount) return true;
    }
    return false;
  }
  
  bool try_free_server(bool verbose, double time) {
    typename T::iterator first = this->server.begin();
    first->arrival->deactivate();
    if (verbose) this->verbose_print(time, first->arrival->name, "PREEMPT");
    if (first->arrival->is_monitored()) {
      double last = first->arrival->get_activity(this->name);
      first->arrival->set_activity(this->name, time - last);
    }
    if (keep_queue) {
      if (!this->room_in_queue(first->amount, first->priority())) {
        if (verbose) this->verbose_print(time, first->arrival->name, "REJECT");
        first->arrival->terminate(false);
      } else this->insert_in_queue(verbose, time, first->arrival, first->amount);
    } else {
      preempted_map[first->arrival] = preempted.insert((*first));
      this->queue_count += first->amount;
    }
    this->server_count -= first->amount;
    this->server_map.erase(first->arrival);
    this->server.erase(first);
    return true;
  }
  
  bool try_serve_from_queue(bool verbose, double time) {
    RPQueue::iterator next;
    bool flag = false;
    if (!preempted.empty()) {
      next = preempted.begin();
      flag = true;
    }
    else next = this->queue.begin();
    if (room_in_server(next->amount, next->priority())) {
      if (next->arrival->is_monitored()) {
        double last = next->arrival->get_activity(this->name);
        next->arrival->set_activity(this->name, time - last);
      }
      next->arrival->activate();
      this->insert_in_server(verbose, time, next->arrival, next->amount);
      this->queue_count -= next->amount;
      if (flag) {
        preempted_map.erase(next->arrival);
        preempted.erase(next);
      } else {
        this->queue_map.erase(next->arrival);
        this->queue.erase(next);
      }
      return true;
    }
    return false;
  }
  
  bool remove_from_queue(bool verbose, double time, Arrival* arrival) {
    if (PriorityRes<T>::remove_from_queue(verbose, time, arrival))
      return true;
    QueueMap::iterator search = preempted_map.find(arrival);
    if (search == preempted_map.end()) return false;
    if (verbose) this->verbose_print(time, arrival->name, "DEPART");
    this->queue_count -= search->second->amount;
    preempted.erase(search->second);
    preempted_map.erase(search);
    return true;
  }
};

#endif
