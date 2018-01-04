## ---- cache = FALSE, include=FALSE---------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", 
                      fig.width = 6, fig.height = 4, fig.align = "center")

required <- c("simmer.plot", "dplyr")

if (!all(sapply(required, requireNamespace, quietly = TRUE)))
  knitr::opts_chunk$set(eval = FALSE)

## ---- message=FALSE------------------------------------------------------
library(simmer)
library(simmer.plot)
set.seed(1234)

## ------------------------------------------------------------------------
lambda <- 3
mu <- 4

mm23.trajectory <- trajectory() %>%
  seize("server", amount=1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("server", amount=1)

mm23.env <- simmer() %>%
  add_resource("server", capacity=2, queue_size=1) %>%
  add_generator("arrival", mm23.trajectory, function() rexp(1, lambda)) %>%
  run(until=2000)

## ------------------------------------------------------------------------
mm23.arrivals <- get_mon_arrivals(mm23.env)

rejection_rate <- mm23.arrivals %>%
  dplyr::summarise(rejection_rate = sum(!finished)/length(finished)) %>%
  dplyr::pull(rejection_rate)
rejection_rate

## ------------------------------------------------------------------------
# Theoretical value
rho <- lambda/mu
div <- 1 / c(1, 1, factorial(2) * 2^(2:3-2))
mm23.N <- sum(0:3 * rho^(0:3) * div) / sum(rho^(0:3) * div)

# Evolution of the average number of customers in the system
plot(mm23.env, "resources", "usage", "server", items="system") +
  geom_hline(yintercept=mm23.N)

## ------------------------------------------------------------------------
mean_pkt_size <- 100        # bytes
lambda1 <- 2                # pkts/s
lambda3 <- 0.5              # pkts/s
lambda4 <- 0.6              # pkts/s
rate <- 2.2 * mean_pkt_size # bytes/s

# set an exponential message size of mean mean_pkt_size
set_msg_size <- function(.)
  set_attribute(., "size", function() rexp(1, 1/mean_pkt_size))

# seize an M/D/1 queue by id; the timeout is function of the message size
md1 <- function(., id)
  seize(., paste0("md1_", id), 1) %>%
  timeout(function() get_attribute(env, "size") / rate) %>%
  release(paste0("md1_", id), 1)

## ------------------------------------------------------------------------
to_queue_1 <- trajectory() %>%
  set_msg_size() %>%
  md1(1) %>%
  leave(0.25) %>%
  md1(2) %>%
  branch(
    function() (runif(1) > 0.65) + 1, continue=c(F, F),
    trajectory() %>% md1(3),
    trajectory() %>% md1(4)
  )

to_queue_3 <- trajectory() %>%
  set_msg_size() %>%
  md1(3)

to_queue_4 <- trajectory() %>%
  set_msg_size() %>%
  md1(4)

## ------------------------------------------------------------------------
env <- simmer()
for (i in 1:4) env %>% 
  add_resource(paste0("md1_", i))
env %>%
  add_generator("arrival1_", to_queue_1, function() rexp(1, lambda1), mon=2) %>%
  add_generator("arrival3_", to_queue_3, function() rexp(1, lambda3), mon=2) %>%
  add_generator("arrival4_", to_queue_4, function() rexp(1, lambda4), mon=2) %>%
  run(4000)

## ------------------------------------------------------------------------
res <- get_mon_arrivals(env, per_resource = TRUE) %>%
  dplyr::select(name, resource) %>%
  dplyr::filter(resource %in% c("md1_3", "md1_4"))
arr <- get_mon_arrivals(env) %>%
  dplyr::mutate(waiting_time = end_time - (start_time + activity_time),
                generator = regmatches(name, regexpr("arrival[[:digit:]]", name))) %>%
  dplyr::left_join(res) %>%
  dplyr::group_by(generator, resource)

dplyr::summarise(arr, average = sum(waiting_time) / n())
get_n_generated(env, "arrival1_") + get_n_generated(env, "arrival4_")
dplyr::count(arr)

