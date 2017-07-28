#include "simmer.h"
#include "activity.h"
#include "entity.h"
#include "process.h"
#include "resource.h"
#include "monitor.h"
#include "simulator.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Simulator__new(const std::string& name, bool verbose) {
  return XPtr<Simulator>(new Simulator(name, verbose));
}

//[[Rcpp::export]]
void reset_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  sim->reset();
}

//[[Rcpp::export]]
double now_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->now();
}

//[[Rcpp::export]]
DataFrame peek_(SEXP sim_, int steps) {
  XPtr<Simulator> sim(sim_);
  return sim->peek(steps);
}

//[[Rcpp::export]]
void step_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  sim->step();
}

//[[Rcpp::export]]
void run_(SEXP sim_, double until) {
  XPtr<Simulator> sim(sim_);
  sim->run(until);
}

//[[Rcpp::export]]
bool add_generator_(SEXP sim_, const std::string& name_prefix, const Environment& trj,
                    const Function& dist, int mon, int priority, int preemptible, bool restart)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_generator(name_prefix, trj, dist, mon, priority, preemptible, restart);
}

//[[Rcpp::export]]
bool add_resource_(SEXP sim_, const std::string& name, int capacity, int queue_size, bool mon,
                   bool preemptive, const std::string& preempt_order, bool queue_size_strict)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_resource(name, capacity, queue_size, mon, preemptive, preempt_order, queue_size_strict);
}

//[[Rcpp::export]]
bool add_resource_manager_(SEXP sim_, const std::string& name, const std::string& param,
                           const std::vector<double>& intervals, const std::vector<int>& values, int period)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_resource_manager(name, param, intervals, values, period);
}

//[[Rcpp::export]]
DataFrame get_mon_arrivals_(SEXP sim_, bool per_resource, bool ongoing) {
  XPtr<Simulator> sim(sim_);
  return sim->get_mon_arrivals(per_resource, ongoing);
}

//[[Rcpp::export]]
DataFrame get_mon_attributes_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_mon_attributes();
}

//[[Rcpp::export]]
DataFrame get_mon_resources_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_mon_resources();
}

//[[Rcpp::export]]
DataFrame get_mon_resources_counts_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_mon_resources_counts();
}

//[[Rcpp::export]]
DataFrame get_mon_resources_limits_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_mon_resources_limits();
}

//[[Rcpp::export]]
int get_n_generated_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_generator(name)->get_n_generated();
}

//[[Rcpp::export]]
std::string get_name_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_running_arrival()->name;
}

//[[Rcpp::export]]
double get_attribute_(SEXP sim_, const std::string& key, bool global) {
  XPtr<Simulator> sim(sim_);
  if (global) return sim->get_attribute(key);
  else return sim->get_running_arrival()->get_attribute(key);
}

//[[Rcpp::export]]
IntegerVector get_prioritization_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  Arrival* a = sim->get_running_arrival();
  return IntegerVector::create(
    a->order.get_priority(), a->order.get_preemptible(), (int)a->order.get_restart()
  );
}

//[[Rcpp::export]]
int get_capacity_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_capacity();
}

//[[Rcpp::export]]
int get_queue_size_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_queue_size();
}

//[[Rcpp::export]]
int get_server_count_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_server_count();
}

//[[Rcpp::export]]
int get_queue_count_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_queue_count();
}

//[[Rcpp::export]]
SEXP Seize__new(const std::string& resource, int amount,
                std::vector<bool> cont, const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Seize<int> >(new Seize<int>(resource, amount, 0, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP Seize__new_func(const std::string& resource, const Function& amount, int provide_attrs,
                     std::vector<bool> cont, const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Seize<Function> >(
      new Seize<Function>(resource, amount, provide_attrs, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP SeizeSelected__new(int id, int amount,
                        std::vector<bool> cont, const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Seize<int> >(new Seize<int>(id, amount, 0, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP SeizeSelected__new_func(int id, const Function& amount, int provide_attrs,
                             std::vector<bool> cont, const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Seize<Function> >(new Seize<Function>(id, amount, provide_attrs, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP Release__new(const std::string& resource, int amount) {
  return XPtr<Release<int> >(new Release<int>(resource, amount, 0));
}

//[[Rcpp::export]]
SEXP Release__new_func(const std::string& resource, const Function& amount, int provide_attrs) {
  return XPtr<Release<Function> >(new Release<Function>(resource, amount, provide_attrs));
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new(int id, int amount) {
  return XPtr<Release<int> >(new Release<int>(id, amount, 0));
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new_func(int id, const Function& amount, int provide_attrs) {
  return XPtr<Release<Function> >(new Release<Function>(id, amount, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetCapacity__new(const std::string& resource, double value) {
  return XPtr<SetCapacity<double> >(new SetCapacity<double>(resource, value, 0));
}

//[[Rcpp::export]]
SEXP SetCapacity__new_func(const std::string& resource, const Function& value, int provide_attrs) {
  return XPtr<SetCapacity<Function> >(
      new SetCapacity<Function>(resource, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetCapacitySelected__new(int id, double value) {
  return XPtr<SetCapacity<double> >(new SetCapacity<double>(id, value, 0));
}

//[[Rcpp::export]]
SEXP SetCapacitySelected__new_func(int id, const Function& value, int provide_attrs) {
  return XPtr<SetCapacity<Function> >(
      new SetCapacity<Function>(id, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetQueue__new(const std::string& resource, double value) {
  return XPtr<SetQueue<double> >(new SetQueue<double>(resource, value, 0));
}

//[[Rcpp::export]]
SEXP SetQueue__new_func(const std::string& resource, const Function& value, int provide_attrs) {
  return XPtr<SetQueue<Function> >(
      new SetQueue<Function>(resource, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetQueueSelected__new(int id, double value) {
  return XPtr<SetQueue<double> >(new SetQueue<double>(id, value, 0));
}

//[[Rcpp::export]]
SEXP SetQueueSelected__new_func(int id, const Function& value, int provide_attrs) {
  return XPtr<SetQueue<Function> >(new SetQueue<Function>(id, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP Select__new(const std::vector<std::string>& resources, const std::string& policy, int id) {
  return XPtr<Select<VEC<std::string> > >(
      new Select<VEC<std::string> >(resources, 0, policy, id));
}

//[[Rcpp::export]]
SEXP Select__new_func(const Function& resources, int provide_attrs, const std::string& policy, int id) {
  return XPtr<Select<Function> >(
      new Select<Function>(resources, provide_attrs, policy, id));
}

//[[Rcpp::export]]
SEXP SetAttribute__new(const std::string& key, double value, bool global) {
  return XPtr<SetAttribute<double> >(new SetAttribute<double>(key, value, 0, global));
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func(const std::string& key,
                            const Function& value, int provide_attrs, bool global) {
  return XPtr<SetAttribute<Function> >(
      new SetAttribute<Function>(key, value, provide_attrs, global));
}

//[[Rcpp::export]]
SEXP Activate__new(const std::string& generator) {
  return XPtr<Activate<std::string> >(new Activate<std::string>(generator, 0));
}

//[[Rcpp::export]]
SEXP Activate__new_func(const Function& generator, int provide_attrs) {
  return XPtr<Activate<Function> >(
      new Activate<Function>(generator, provide_attrs));
}

//[[Rcpp::export]]
SEXP Deactivate__new(const std::string& generator) {
  return XPtr<Deactivate<std::string> >(new Deactivate<std::string>(generator, 0));
}

//[[Rcpp::export]]
SEXP Deactivate__new_func(const Function& generator, int provide_attrs) {
  return XPtr<Deactivate<Function> >(
      new Deactivate<Function>(generator, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetTraj__new(const std::string& generator, const Environment& trj) {
  return XPtr<SetTraj<std::string> >(new SetTraj<std::string>(generator, 0, trj));
}

//[[Rcpp::export]]
SEXP SetTraj__new_func(const Function& generator, int provide_attrs, const Environment& trj) {
  return XPtr<SetTraj<Function> >(
      new SetTraj<Function>(generator, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP SetDist__new(const std::string& generator, const Function& dist) {
  return XPtr<SetDist<std::string> >(new SetDist<std::string>(generator, 0, dist));
}

//[[Rcpp::export]]
SEXP SetDist__new_func(const Function& generator, int provide_attrs, const Function& dist) {
  return XPtr<SetDist<Function> >(
      new SetDist<Function>(generator, provide_attrs, dist));
}

//[[Rcpp::export]]
SEXP SetPrior__new(const std::vector<int>& values) {
  return XPtr<SetPrior<VEC<int> > >(new SetPrior<VEC<int> >(values, 0));
}

//[[Rcpp::export]]
SEXP SetPrior__new_func(const Function& values, int provide_attrs) {
  return XPtr<SetPrior<Function> >(new SetPrior<Function>(values, provide_attrs));
}

//[[Rcpp::export]]
SEXP Timeout__new(double delay) {
  return XPtr<Timeout<double> >(new Timeout<double>(delay, 0));
}

//[[Rcpp::export]]
SEXP Timeout__new_func(const Function& task, int provide_attrs) {
  return XPtr<Timeout<Function> >(new Timeout<Function>(task, provide_attrs));
}

//[[Rcpp::export]]
SEXP Branch__new(const Function& option, int provide_attrs,
                 std::vector<bool> cont, const std::vector<Environment>& trj)
{
  return XPtr<Branch>(new Branch(option, provide_attrs, cont, trj));
}

//[[Rcpp::export]]
SEXP Rollback__new(int amount, int times) {
  return XPtr<Rollback>(new Rollback(amount, times));
}

//[[Rcpp::export]]
SEXP Rollback__new_func(int amount, const Function& check, int provide_attrs) {
  return XPtr<Rollback>(new Rollback(amount, 0, check, provide_attrs));
}

//[[Rcpp::export]]
SEXP Leave__new(double prob) {
  return XPtr<Leave<double> >(new Leave<double>(prob, 0));
}

//[[Rcpp::export]]
SEXP Leave__new_func(const Function& prob, int provide_attrs) {
  return XPtr<Leave<Function> >(new Leave<Function>(prob, provide_attrs));
}

//[[Rcpp::export]]
SEXP Clone__new(int n, const std::vector<Environment>& trj) {
  return XPtr<Clone<int> >(new Clone<int>(n, 0, trj));
}

//[[Rcpp::export]]
SEXP Clone__new_func(const Function& n, int provide_attrs, const std::vector<Environment>& trj) {
  return XPtr<Clone<Function> >(new Clone<Function>(n, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP Synchronize__new(bool wait, bool terminate) {
  return XPtr<Synchronize>(new Synchronize(wait, terminate));
}

//[[Rcpp::export]]
SEXP Batch__new(int n, double timeout, bool permanent, const std::string& name) {
  VEC<int> p = VEC<int>(2, 0);
  return XPtr<Batch<double> >(new Batch<double>(n, timeout, p, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func1(int n, const Function& timeout, bool permanent, const std::string& name,
                      int provide_attrs)
{
  VEC<int> p = VEC<int>(2, 0);
  p[0] = provide_attrs;
  return XPtr<Batch<Function> >(new Batch<Function>(n, timeout, p, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func2(int n, double timeout, bool permanent,
                      const std::string& name, const Function& rule, int provide_attrs)
{
  VEC<int> p = VEC<int>(2, 0);
  p[1] = provide_attrs;
  return XPtr<Batch<double> >(new Batch<double>(n, timeout, p, permanent, name, rule));
}

//[[Rcpp::export]]
SEXP Batch__new_func4(int n, const Function& timeout, bool permanent, const std::string& name,
                      const Function& rule, const std::vector<int>& provide_attrs)
{
  VEC<int> p = provide_attrs;
  return XPtr<Batch<Function> >(new Batch<Function>(n, timeout, p, permanent, name, rule));
}

//[[Rcpp::export]]
SEXP Separate__new() {
  return XPtr<Separate>(new Separate());
}

//[[Rcpp::export]]
SEXP RenegeIn__new(double t, const std::vector<Environment>& trj) {
  return XPtr<RenegeIn<double> >(new RenegeIn<double>(t, 0, trj));
}

//[[Rcpp::export]]
SEXP RenegeIn__new_func(const Function& t, int provide_attrs, const std::vector<Environment>& trj) {
  return XPtr<RenegeIn<Function> >(new RenegeIn<Function>(t, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP RenegeIf__new(const std::string& signal, const std::vector<Environment>& trj) {
  return XPtr<RenegeIf<std::string> >(new RenegeIf<std::string>(signal, 0, trj));
}

//[[Rcpp::export]]
SEXP RenegeIf__new_func(const Function& signal, int provide_attrs, const std::vector<Environment>& trj) {
  return XPtr<RenegeIf<Function> >(new RenegeIf<Function>(signal, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP RenegeAbort__new() {
  return XPtr<RenegeAbort>(new RenegeAbort());
}

//[[Rcpp::export]]
SEXP Send__new(const std::vector<std::string>& signals, double delay) {
  VEC<int> p = VEC<int>(2, 0);
  return XPtr<Send<VEC<std::string>, double> >(
      new Send<VEC<std::string>, double>(signals, delay, p));
}

//[[Rcpp::export]]
SEXP Send__new_func1(const Function& signals,
                     double delay, int provide_attrs)
{
  VEC<int> p = VEC<int>(2, 0);
  p[0] = provide_attrs;
  return XPtr<Send<Function, double> >(new Send<Function, double>(signals, delay, p));
}

//[[Rcpp::export]]
SEXP Send__new_func2(const std::vector<std::string>& signals,
                     const Function& delay, int provide_attrs)
{
  VEC<int> p = VEC<int>(2, 0);
  p[1] = provide_attrs;
  return XPtr<Send<VEC<std::string>, Function> >(
      new Send<VEC<std::string>, Function>(signals, delay, p));
}

//[[Rcpp::export]]
SEXP Send__new_func4(const Function& signals,
                     const Function& delay, const std::vector<int>& provide_attrs)
{
  VEC<int> p = provide_attrs;
  return XPtr<Send<Function, Function> >(new Send<Function, Function>(signals, delay, p));
}

//[[Rcpp::export]]
SEXP Trap__new(const std::vector<std::string>& signals,
               const std::vector<Environment>& trj, bool interruptible)
{
  return XPtr<Trap<VEC<std::string> > >(
      new Trap<VEC<std::string> >(signals, 0, trj, interruptible));
}

//[[Rcpp::export]]
SEXP Trap__new_func(const Function& signals, int provide_attrs,
                    const std::vector<Environment>& trj, bool interruptible) {
  return XPtr<Trap<Function> >(
      new Trap<Function>(signals, provide_attrs, trj, interruptible));
}

//[[Rcpp::export]]
SEXP UnTrap__new(const std::vector<std::string>& signals) {
  return XPtr<UnTrap<VEC<std::string> > >(new UnTrap<VEC<std::string> >(signals, 0));
}

//[[Rcpp::export]]
SEXP UnTrap__new_func(const Function& signals, int provide_attrs) {
  return XPtr<UnTrap<Function> >(new UnTrap<Function>(signals, provide_attrs));
}

//[[Rcpp::export]]
SEXP Wait__new() {
  return XPtr<Wait>(new Wait());
}

//[[Rcpp::export]]
SEXP Log__new(const std::string& message) {
  return XPtr<Log<std::string> >(new Log<std::string>(message, 0));
}

//[[Rcpp::export]]
SEXP Log__new_func(const Function& message, int provide_attrs) {
  return XPtr<Log<Function> >(
      new Log<Function>(message, provide_attrs));
}

//[[Rcpp::export]]
int activity_get_count_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  return activity->count;
}

//[[Rcpp::export]]
void activity_print_(SEXP activity_, int indent, bool verbose) {
  XPtr<Activity> activity(activity_);
  return activity->print(indent, verbose);
}

//[[Rcpp::export]]
SEXP activity_get_next_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  Activity* the_next = activity->get_next();
  if (the_next)
    return XPtr<Activity>(the_next, false);
  return R_NilValue;
}

//[[Rcpp::export]]
SEXP activity_get_prev_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  Activity* the_prev = activity->get_prev();
  if (the_prev)
    return XPtr<Activity>(the_prev, false);
  return R_NilValue;
}

//[[Rcpp::export]]
void activity_chain_(SEXP first_, SEXP second_) {
  XPtr<Activity> first(first_);
  XPtr<Activity> second(second_);
  first->set_next(second);
  second->set_prev(first);
}

//[[Rcpp::export]]
SEXP activity_clone_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  return XPtr<Activity>(activity->clone());
}
