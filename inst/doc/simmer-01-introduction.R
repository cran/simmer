## ---- cache = FALSE, include=FALSE---------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", 
                      fig.width = 6, fig.height = 4, fig.align = "center")

## ---- message=FALSE------------------------------------------------------
library(simmer)

env <- simmer("SuperDuperSim")
env

## ------------------------------------------------------------------------
patient <- trajectory("patients' path") %>%
  ## add an intake activity 
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

## ------------------------------------------------------------------------
env %>%
  add_resource("nurse", 1) %>%
  add_resource("doctor", 2) %>%
  add_resource("administration", 1) %>%
  add_generator("patient", patient, function() rnorm(1, 10, 2))

## ---- message=FALSE------------------------------------------------------
env %>% run(until=80)
env %>% now()
env %>% peek(3)

## ---- message=FALSE------------------------------------------------------
env %>% onestep()
env %>% onestep() %>% onestep() %>% onestep()
env %>% now()
env %>% peek(Inf, verbose=TRUE)

## ---- message=FALSE------------------------------------------------------
env %>% 
  run(until=120) %>%
  now()

## ---- message=FALSE------------------------------------------------------
env %>% 
  reset() %>% 
  run(until=80) %>%
  now()

## ------------------------------------------------------------------------
envs <- lapply(1:100, function(i) {
  simmer("SuperDuperSim") %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", patient, function() rnorm(1, 10, 2)) %>%
    run(80)
})

## ------------------------------------------------------------------------
library(parallel)

envs <- mclapply(1:100, function(i) {
  simmer("SuperDuperSim") %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", patient, function() rnorm(1, 10, 2)) %>%
    run(80) %>%
    wrap()
})

## ---- message=FALSE------------------------------------------------------
envs[[1]] %>% get_n_generated("patient")
envs[[1]] %>% get_capacity("doctor")
envs[[1]] %>% get_queue_size("doctor")
head(
  envs %>% get_mon_resources()
)
head(
  envs %>% get_mon_arrivals()
)

