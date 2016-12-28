## ---- cache = FALSE, include=FALSE---------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>",
                      fig.width = 6, fig.height = 4, fig.align = "center")
library(ggplot2)
theme_set(theme_bw())

## ---- message=FALSE, warning=FALSE---------------------------------------
library(simmer)

NUM_MACHINES <- 2  # Number of machines in the carwash
WASHTIME <- 5      # Minutes it takes to clean a car
T_INTER <- 7       # Create a car every ~7 minutes
SIM_TIME <- 20     # Simulation time in minutes

# setup
set.seed(42)
env <- simmer()

## ---- message=FALSE, warning=FALSE---------------------------------------
car <- trajectory() %>%
  log_("arrives at the carwash") %>%
  seize("wash", 1) %>%
  log_("enters the carwash") %>%
  timeout(WASHTIME) %>%
  set_attribute("dirt_removed", function() sample(50:99, 1)) %>%
  log_(function(attr) 
    paste0(attr["dirt_removed"], "% of dirt was removed")) %>%
  release("wash", 1) %>%
  log_("leaves the carwash")

## ---- message=FALSE, warning=FALSE---------------------------------------
env %>%
  add_resource("wash", NUM_MACHINES) %>%
  # feed the trajectory with 4 initial cars
  add_generator("car_initial", car, at(rep(0, 4))) %>%
  # new cars approx. every T_INTER minutes
  add_generator("car", car, function() sample((T_INTER-2):(T_INTER+2), 1)) %>%
  # start the simulation
  run(SIM_TIME)

## ---- message=FALSE, warning=FALSE---------------------------------------
detach("package:simmer")
library(dplyr)
library(simmer)

PT_MEAN <- 10.0         # Avg. processing time in minutes
PT_SIGMA <- 2.0         # Sigma of processing time
MTTF <- 300.0           # Mean time to failure in minutes
BREAK_MEAN <- 1 / MTTF  # Param. for exponential distribution
REPAIR_TIME <- 30.0     # Time it takes to repair a machine in minutes
JOB_DURATION <- 30.0    # Duration of other jobs in minutes
NUM_MACHINES <- 10      # Number of machines in the machine shop
WEEKS <- 4              # Simulation time in weeks
SIM_TIME <- WEEKS * 7 * 24 * 60  # Simulation time in minutes

# setup
set.seed(42)
env <- simmer()

## ---- message=FALSE, warning=FALSE---------------------------------------
make_parts <- function(machine)
  trajectory() %>%
    set_attribute("parts", 0) %>%
    seize(machine, 1) %>%
    timeout(function() rnorm(1, PT_MEAN, PT_SIGMA)) %>%
    set_attribute("parts", function(attr) attr["parts"] + 1) %>%
    rollback(2, Inf) # go to 'timeout' over and over

## ---- message=FALSE, warning=FALSE---------------------------------------
other_jobs <- trajectory() %>%
  seize("repairman", 1) %>%
  timeout(JOB_DURATION) %>%
  rollback(1, Inf)

## ---- message=FALSE, warning=FALSE---------------------------------------
machines <- paste0("machine", 1:NUM_MACHINES-1)

failure <- trajectory() %>%
  select(machines, policy = "random") %>%
  seize_selected(1) %>%
  seize("repairman", 1) %>%
  timeout(REPAIR_TIME) %>%
  release("repairman", 1) %>%
  release_selected(1)

## ---- message=FALSE, warning=FALSE---------------------------------------
lapply(machines, function(i) {
  env %>%
    add_resource(i, 1, 0, preemptive = TRUE) %>%
    add_generator(paste0(i, "_worker"), make_parts(i), at(0), mon = 2)
}) %>% invisible

## ---- message=FALSE, warning=FALSE---------------------------------------
env %>%
  add_resource("repairman", 1, Inf, preemptive = TRUE) %>%
  add_generator("repairman_worker", other_jobs, at(0)) %>%
  invisible

## ---- message=FALSE, warning=FALSE---------------------------------------
env %>%
  add_generator("failure", failure, 
                function() rexp(1, BREAK_MEAN * NUM_MACHINES), 
                priority = 1) %>%
  run(SIM_TIME) %>% invisible

## ---- message=FALSE, warning=FALSE---------------------------------------
get_mon_attributes(env) %>%
  group_by(name) %>%
  slice(n()) %>%
  arrange(name)

## ---- message=FALSE, warning=FALSE---------------------------------------
detach("package:simmer")
library(dplyr)
library(simmer)

TICKETS <- 50     # Number of tickets per movie
SIM_TIME <- 120   # Simulate until
movies <- c("R Unchained", "Kill Process", "Pulp Implementation")

# setup
set.seed(42)
env <- simmer()

## ---- message=FALSE, warning=FALSE---------------------------------------
moviegoer <- trajectory() %>%
  # select a movie
  set_attribute("movie", function() sample(3, 1)) %>%
  select(function(attr) movies[attr["movie"]]) %>%
  # set reneging condition
  renege_if(function(attr) paste0(movies[attr["movie"]], " sold out")) %>%
  # leave immediately if the movie was already sold out
  leave(function(attr) get_capacity(env, movies[attr["movie"]]) == 0) %>%
  # wait for my turn
  seize("counter", 1) %>%
  # buy tickets
  seize_selected(function() sample(6, 1), continue = FALSE,
                 reject = trajectory() %>%
                   # not enough tickets, leave after some discussion
                   timeout(0.5) %>%
                   release("counter", 1)
  ) %>%
  # abort reneging condition
  renege_abort() %>%
  # check the tickets available
  branch(function(attr) get_server_count(env, movies[attr["movie"]]) > (TICKETS - 2),
         continue = TRUE,
         # trigger the "sold out" event for the movie
         trajectory() %>%
           set_capacity_selected(0) %>%
           send(function(attr) paste0(movies[attr["movie"]], " sold out"))
  ) %>%
  timeout(1) %>%
  release("counter", 1) %>%
  # watch the movie
  wait()

## ---- message=FALSE, warning=FALSE---------------------------------------
# add movies as resources with capacity TICKETS and no queue
lapply(movies, function(i) add_resource(env, i, TICKETS, 0)) %>% invisible

# add ticket counter with capacity 1 and infinite queue
env %>% add_resource("counter", 1, Inf)

## ---- message=FALSE, warning=FALSE---------------------------------------
# add a moviegoer generator and start simulation
env %>%
  add_generator("moviegoer", moviegoer, function() rexp(1, 1 / 0.5), mon=2) %>%
  run(SIM_TIME)

## ---- message=FALSE, warning=FALSE---------------------------------------
arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)

## ---- message=FALSE, warning=FALSE---------------------------------------
# get the three rows with the sold out instants
sold_time <- resources %>%
  filter(resource != "counter" & capacity == 0)

# get the arrivals that left at the sold out instants
# count the number of arrivals per movie
n_reneges <- left_join(
    arrivals %>% 
      filter(finished == FALSE & end_time %in% sold_time$time),
    attributes
  ) %>%
  mutate(resource = movies[value]) %>%
  group_by(resource) %>%
  count()

# merge the info
result <- left_join(sold_time, n_reneges)
# and print
apply(result, 1, function(i) {
  cat("Movie '", i["resource"], "' sold out ", i["time"], 
      " minutes after ticket counter opening.\n",
      "  Number of people leaving queue when film sold out: ", i["n"], "\n", sep="")
}) %>% invisible

## ---- message=FALSE, warning=FALSE---------------------------------------
library(simmer)

GAS_STATION_SIZE <- 200     # liters
THRESHOLD <- 10             # Threshold for calling the tank truck (in %)
FUEL_TANK_SIZE <- 50        # liters
FUEL_TANK_LEVEL <- c(5, 25) # Min/max levels of fuel tanks (in liters)
REFUELING_SPEED <- 2        # liters / second
TANK_TRUCK_TIME <- 300      # Seconds it takes the tank truck to arrive
T_INTER <- c(30, 100)       # Create a car every [min, max] seconds
SIM_TIME <- 1000            # Simulation time in seconds

# setup
set.seed(42)
env <- simmer()

## ---- message=FALSE, warning=FALSE---------------------------------------
GAS_STATION_LEVEL <- GAS_STATION_SIZE
signal <- "gas station refilled"

refuelling <- trajectory() %>%
  # check if there is enough fuel available
  branch(function(attr) FUEL_TANK_SIZE - attr["level"] > GAS_STATION_LEVEL, 
         continue = TRUE,
         # if not, block until the signal "gas station refilled" is received
         trajectory() %>%
           trap(signal) %>%
           wait() %>%
           untrap(signal)
  ) %>%
  # refuel
  timeout(function(attr) {
    liters_required <- FUEL_TANK_SIZE - attr["level"]
    GAS_STATION_LEVEL <<- GAS_STATION_LEVEL - liters_required
    return(liters_required / REFUELING_SPEED)
  })

## ---- message=FALSE, warning=FALSE---------------------------------------
car <- trajectory() %>%
  set_attribute("start", function() now(env)) %>%
  set_attribute("level", function() 
    sample(FUEL_TANK_LEVEL[1]:FUEL_TANK_LEVEL[2], 1)) %>%
  log_("arriving at gas station") %>%
  seize("pump", 1) %>%
  # 'join()' concatenates the refuelling trajectory here
  join(refuelling) %>%
  release("pump", 1) %>%
  log_(function(attr) 
    paste0("finished refuelling in ", now(env) - attr["start"], " seconds"))

## ---- message=FALSE, warning=FALSE---------------------------------------
tank_truck <- trajectory() %>%
  timeout(TANK_TRUCK_TIME) %>%
  log_("tank truck arriving at gas station") %>%
  log_(function() {
    refill <- GAS_STATION_SIZE - GAS_STATION_LEVEL
    GAS_STATION_LEVEL <<- GAS_STATION_SIZE
    paste0("tank truck refilling ", refill, " liters")
  }) %>%
  send(signal)

## ---- message=FALSE, warning=FALSE---------------------------------------
controller <- trajectory() %>%
  branch(function() GAS_STATION_LEVEL / GAS_STATION_SIZE * 100 < THRESHOLD, 
         continue = TRUE,
         trajectory() %>%
           log_("calling the tank truck") %>%
           join(tank_truck)
  ) %>%
  timeout(10) %>%
  rollback(2, Inf)

## ---- message=FALSE, warning=FALSE---------------------------------------
env %>%
  add_resource("pump", 2) %>%
  add_generator("controller", controller, at(0)) %>%
  add_generator("car", car, function() sample(T_INTER[1]:T_INTER[2], 1)) %>%
  run(SIM_TIME)

