% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectory.R
\name{select}
\alias{select}
\title{Select a resource}
\usage{
select(traj, resources, policy = c("shortest-queue", "round-robin",
  "first-available", "random"), id = 0)
}
\arguments{
\item{traj}{the trajectory object.}

\item{resources}{one or more resource names, or a callable object (a function) which
must return a resource name to select.}

\item{policy}{if \code{resources} is a vector of names, this parameter determines
the criteria for selecting a resource among the set of policies available; otherwise,
it is ignored.}

\item{id}{selection identifier for nested usage.}
}
\value{
The trajectory object.
}
\description{
Selects a resource for a subsequent seize/release.
}
\seealso{
\link{seize_selected}, \link{release_selected}. 
Other methods for dealing with trajectories:
\link{create_trajectory}, \link{get_head}, \link{get_tail}, 
\link{get_n_activities}, \link{join}, \link{seize}, \link{release}, 
\link{set_attribute}, \link{timeout}, \link{branch}, \link{rollback}, \link{leave}.
}
