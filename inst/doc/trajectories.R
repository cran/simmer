## ------------------------------------------------------------------------
library(simmer)

patient_traj<-
  create_trajectory(name = "patient_trajectory") %>%
  seize(resource = "doctor", amount = 1) %>%
  timeout(3) %>%
  release(resource = "doctor", amount = 1)
  

## ------------------------------------------------------------------------
patient_traj<-
  create_trajectory(name = "patient_trajectory") %>%
  set_attribute("my_key", 123) %>%
  timeout(5) %>%
  set_attribute("my_key", 456)

env<-
  simmer() %>%
  add_generator("patient", patient_traj, at(0), mon = 2) %>%
  run()

get_mon_attributes(env)
  

## ------------------------------------------------------------------------
patient_traj<-
  create_trajectory(name = "patient_trajectory") %>%
  set_attribute("my_key", 123) %>%
  timeout(5) %>%
  set_attribute("my_key", function(attrs) attrs[["my_key"]] + 1) %>%
  timeout(5) %>%
  set_attribute("dependent_key", function(attrs) ifelse(attrs[["my_key"]]<=123, 1, 0)) %>%
  timeout(5) %>%
  set_attribute("independent_key", function() runif(1))

env<-
  simmer() %>%
  add_generator("patient", patient_traj, at(0), mon = 2) %>%
  run()

get_mon_attributes(env)
  

## ------------------------------------------------------------------------
patient_traj<-
  create_trajectory(name = "patient_trajectory") %>%
  seize(resource = "doctor", amount = 1) %>%
  timeout(3) %>%
  release(resource = "doctor", amount = 1)

env<-
  simmer() %>%
  add_resource("doctor", capacity=1, mon = 1) %>%
  add_generator("patient", patient_traj, at(0)) %>%
  run()

get_mon_resources(env)

## ------------------------------------------------------------------------
patient_traj<-
  create_trajectory(name = "patient_trajectory") %>%
  set_attribute("health", function() sample(20:80, 1)) %>%
  set_attribute("docs_to_seize", function(attrs) ifelse(attrs[["health"]]<50, 1, 2)) %>%
  seize("doctor", function(attrs) attrs[["docs_to_seize"]]) %>%
  timeout(3) %>%
  release("doctor", function(attrs) attrs[["docs_to_seize"]])

env<-
  simmer() %>%
  add_resource("doctor", capacity=2, mon = 1) %>%
  add_generator("patient", patient_traj, at(0), mon=2) %>%
  run()

get_mon_resources(env)
get_mon_attributes(env)


## ------------------------------------------------------------------------
patient_traj<-
  create_trajectory(name = "patient_trajectory") %>%
  timeout(3)

env<-
  simmer() %>%
  add_resource("doctor", capacity=2, mon = 1) %>%
  add_generator("patient", patient_traj, at(0), mon=2) %>%
  run()

get_mon_arrivals(env)


## ------------------------------------------------------------------------
patient_traj<-
  create_trajectory(name = "patient_trajectory") %>%
  set_attribute("health", function() sample(20:80, 1)) %>%
  # distribution-based timeout
  timeout(function() rpois(1, 10)) %>%
  # attribute-dependent timeout
  timeout(function(attrs) (100 - attrs[["health"]]) * 2)

env<-
  simmer() %>%
  add_generator("patient", patient_traj, at(0), mon=2) %>%
  run()

get_mon_arrivals(env)
get_mon_attributes(env)


## ------------------------------------------------------------------------
t0<-create_trajectory() %>%
  timeout(function(){
    print("Hello!")
    0}) %>%
  rollback(amount=1, times=3)


simmer() %>%
  add_generator("hello_sayer", t0, at(0)) %>% 
  run()


## ------------------------------------------------------------------------
t0<-create_trajectory() %>%
  set_attribute("happiness", 0) %>%
  # the timeout function is used simply to print something and returns 0,
  # hence it is a dummy timeout
  timeout(function(attrs){
    cat(">> Happiness level is at: ", attrs[["happiness"]], " -- ")
    cat(ifelse(attrs[["happiness"]]<25,"PETE: I'm feeling crappy...",
           ifelse(attrs[["happiness"]]<50,"PETE: Feelin' a bit moody",
                  ifelse(attrs[["happiness"]]<75,"PETE: Just had a good espresso",
                         "PETE: Let's do this! (and stop this loop...)")))
    , "\n")
    return(0)
  }) %>%
  set_attribute("happiness", function(attrs) attrs[["happiness"]] + 25) %>%
  rollback(amount=2, check=function(attrs) attrs[["happiness"]] < 100)


simmer() %>%
  add_generator("mood_swinger", t0, at(0)) %>% 
  run()


