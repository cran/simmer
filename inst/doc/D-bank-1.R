## ---- cache = FALSE, include=FALSE---------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>",
                      fig.width = 6, fig.height = 4, fig.align = "center")

required <- c("dplyr")

if (!all(unlist(lapply(required, function(pkg) requireNamespace(pkg, quietly = TRUE)))))
  knitr::opts_chunk$set(eval = FALSE)

## ------------------------------------------------------------------------
library(simmer)

customer <- 
  trajectory("Customer's path") %>%
  timeout(10) 

bank <- 
  simmer("bank") %>% 
  add_generator("Customer", customer, at(5))

bank %>% run(until = 100) 
bank %>% get_mon_arrivals

## ------------------------------------------------------------------------
library(simmer)

set.seed(10212)

customer <- 
  trajectory("Customer's path") %>%
  timeout(10) 

bank <- 
  simmer("bank") %>% 
  add_generator("Customer", customer, at(rexp(1, 1/5)))

bank %>% run(until = 100) 
bank %>% get_mon_arrivals

## ------------------------------------------------------------------------
# Function to specify a series of waiting times, that loop around
loop <- function(...) {
    time_diffs <- c(...)
    i <- 0
    function() {		
      if (i < length(time_diffs)) {		
        i <<- i+1		
      } else {		
        i <<- 1		
      }		
      return(time_diffs[i])		
    }		
  }

x <- loop(10, 7, 20)
x(); x(); x(); x(); x()

## ------------------------------------------------------------------------
library(simmer)

# Function to specify a series of waiting times in a loop
loop <- function(...) {
    time_diffs <- c(...)
    i <- 0
    function() {		
      if (i < length(time_diffs)) {		
        i <<- i+1		
      } else {		
        i <<- 1		
      }		
      return(time_diffs[i])		
    }		
  }

customer <- 
  trajectory("Customer's path") %>%
  timeout(loop(10, 7, 20))

bank <- 
  simmer("bank") %>% 
  add_generator("Customer", customer, at(2, 5, 12))

bank %>% run(until = 400) 
bank %>% get_mon_arrivals

## ------------------------------------------------------------------------
library(simmer)

customer <- 
  trajectory("Customer's path") %>%
  timeout(12) 

bank <- 
  simmer("bank") %>% 
  add_generator("Customer", customer, 
                function() {c(0, rep(10, 4), -1)}) # every 10 starting at 0 for 5 customers

bank %>% run(until = 400) 
bank %>% get_mon_arrivals

## ------------------------------------------------------------------------
library(simmer)

set.seed(1289)

customer <- 
  trajectory("Customer's path") %>%
  timeout(12) 

bank <- 
  simmer("bank") %>% 
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

bank %>% run(until = 400) 
bank %>% get_mon_arrivals

## ---- message = FALSE----------------------------------------------------
library(simmer)

set.seed(99999)

customer <- 
  trajectory("Customer's path") %>%
  seize("counter") %>%
  timeout(12) %>%
  release("counter")

bank <- 
  simmer("bank") %>% 
  add_resource("counter") %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

bank %>% run(until = 400) 
bank %>% 
  get_mon_arrivals %>%
  dplyr::mutate(waiting_time = end_time - start_time - activity_time)

## ---- message = FALSE----------------------------------------------------
library(simmer)

set.seed(99999)

customer <- 
  trajectory("Customer's path") %>%
  seize("counter") %>%
  timeout(function() {rexp(1, 1/12)}) %>%
  # timeout(rexp(1, 1/12)) %>% # This line would use the same time for everyone
  release("counter")

bank <- 
  simmer("bank") %>% 
  add_resource("counter") %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

bank %>% run(until = 400) 
bank %>% 
  get_mon_arrivals %>%
  dplyr::mutate(waiting_time = end_time - start_time - activity_time)

## ---- message = FALSE----------------------------------------------------
library(simmer)

set.seed(99999)

customer <- 
  trajectory("Customer's path") %>%
  seize("counter") %>%
  timeout(function() {rexp(1, 1/12)}) %>%
  # timeout(rexp(1, 1/12)) %>% # This line would use the same time for everyone
  release("counter")

bank <- 
  simmer("bank") %>% 
  add_resource("counter", 2) %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

bank %>% run(until = 400) 
bank %>% 
  get_mon_arrivals %>%
  dplyr::mutate(waiting_time = end_time - start_time - activity_time)

## ---- message = FALSE----------------------------------------------------
library(simmer)

set.seed(1014)

customer <- 
  trajectory("Customer's path") %>%
  select(c("counter1", "counter2"), policy = "shortest-queue") %>%
  seize_selected %>%
  timeout(function() {rexp(1, 1/12)}) %>%
  release_selected

bank <- 
  simmer("bank") %>% 
  add_resource("counter1", 1) %>%
  add_resource("counter2", 1) %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

bank %>% run(until = 400) 
bank %>% 
  get_mon_arrivals %>%
  dplyr::mutate(service_start_time = end_time - activity_time) %>%
  dplyr::arrange(start_time)
bank %>% 
  get_mon_resources %>%
  dplyr::arrange(time)

## ---- message = FALSE----------------------------------------------------
library(simmer)

set.seed(100005)

customer <- 
  trajectory("Customer's path") %>%
  seize("counter") %>%
  timeout(function() {rexp(1, 1/12)}) %>%
  release("counter")

bank <- 
  simmer("bank") %>% 
  add_resource("counter", 2) %>%
  add_generator("Customer", customer, function() {c(0, rexp(49, 1/10), -1)})

bank %>% run(until = 400) 
result <- 
  bank %>% 
  get_mon_arrivals %>%
  dplyr::mutate(waiting_time = end_time - start_time - activity_time)
paste("Average wait for ", sum(result$finished), " completions was ",
      mean(result$waiting_time), "minutes.")

## ---- message = FALSE----------------------------------------------------
library(simmer)
library(parallel)

customer <- 
  trajectory("Customer's path") %>%
  seize("counter") %>%
  timeout(function() {rexp(1, 1/12)}) %>%
  release("counter")

mclapply(c(393943, 100005, 777999555, 319999772), function(the_seed) {
  set.seed(the_seed)

  bank <- 
    simmer("bank") %>% 
    add_resource("counter", 2) %>%
    add_generator("Customer", customer, function() {c(0, rexp(49, 1/10), -1)})

  bank %>% run(until = 400) 
  result <- 
    bank %>% 
    get_mon_arrivals %>%
    dplyr::mutate(waiting_time = end_time - start_time - activity_time)
  paste("Average wait for ", sum(result$finished), " completions was ",
        mean(result$waiting_time), "minutes.")
}) %>% unlist()

