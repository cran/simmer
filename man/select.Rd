% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectory-activities.R
\name{select}
\alias{select}
\title{Select Resources}
\usage{
select(.trj, resources, policy = c("shortest-queue",
  "shortest-queue-available", "round-robin", "round-robin-available",
  "first-available", "random", "random-available"), id = 0, ..., tag)
}
\arguments{
\item{.trj}{the trajectory object.}

\item{resources}{one or more resource names, or a callable object (a function)
which must return one or more resource names.}

\item{policy}{if \code{resources} is a character vector, this parameter
determines the criteria for selecting a resource among the set of policies
available (see details).}

\item{id}{selection identifier for nested usage.}

\item{...}{unused.}

\item{tag}{activity tag name to perform named rollbacks (see
\code{\link{rollback}}) or just to better identify your activities.}
}
\value{
Returns the trajectory object.
}
\description{
Activity for selecting a resource for a subsequent seize/release or setting
its parameters (capacity or queue size). Resources must be defined in the
simulation environment (see \code{\link{add_resource}}).
}
\details{
The 'shortest-queue' policy selects the least busy resource;
'round-robin' selects resources in cyclical order; 'first-available' selects
the first resource available, and 'random' selects a resource randomly.

All the 'available'-ending policies ('first-available', but also
'shortest-queue-available', 'round-robin-available' and 'random-available')
check for resource availability (i.e., whether the capacity is non-zero),
and exclude from the selection procedure those resources with capacity set
to zero. This means that, for these policies, an error will be raised if all
resources are unavailable.
}
\examples{
## predefined policy
traj <- trajectory() \%>\%
  select(paste0("doctor", 1:3), "round-robin") \%>\%
  seize_selected(1) \%>\%
  timeout(5) \%>\%
  release_selected(1)

simmer() \%>\%
  add_resource("doctor1") \%>\%
  add_resource("doctor2") \%>\%
  add_resource("doctor3") \%>\%
  add_generator("patient", traj, at(0, 1, 2)) \%>\%
  run() \%>\%
  get_mon_resources()

## custom policy
env <- simmer()
res <- paste0("doctor", 1:3)

traj <- trajectory() \%>\%
  select(function() {
    occ <- get_server_count(env, res) + get_queue_count(env, res)
    res[which.min(occ)[1]]
  }) \%>\%
  seize_selected(1) \%>\%
  timeout(5) \%>\%
  release_selected(1)

for (i in res) env \%>\%
  add_resource(i)
env \%>\%
  add_generator("patient", traj, at(0, 1, 2)) \%>\%
  run() \%>\%
  get_mon_resources()

}
\seealso{
\code{\link{seize_selected}}, \code{\link{release_selected}},
\code{\link{set_capacity_selected}}, \code{\link{set_queue_size_selected}}
}
