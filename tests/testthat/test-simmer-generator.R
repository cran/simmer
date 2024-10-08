# Copyright (C) 2015 Iñaki Ucar and Bart Smeets
# Copyright (C) 2015-2024 Iñaki Ucar
#
# This file is part of simmer.
#
# simmer is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# simmer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with simmer. If not, see <http://www.gnu.org/licenses/>.

test_that("a generator without a trajectory fails", {
  expect_error(
    simmer(verbose = env_verbose) %>%
      add_generator("customer", 4, 1))
})

test_that("a non-function dist fails", {
  expect_error(
    simmer(verbose = env_verbose) %>%
      add_generator("customer", trajectory(), 1))
})

test_that("a dist that returns a non-numeric value fails", {
  expect_error(
    simmer(verbose = env_verbose) %>%
      add_generator("customer", trajectory(), function() {}) %>%
      step())
})

test_that("generates the expected amount", {
  t <- trajectory() %>% timeout(0)
  env <- simmer(verbose = env_verbose) %>%
    add_generator("customer", t, at(1:3)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(env %>% get_sources(), "customer")
  expect_error(env %>% get_n_generated("asdf"))
  expect_equal(env %>% get_n_generated("customer"), 3)
  expect_error(env %>% get_trajectory("asdf"))
  expect_equal((env %>% get_trajectory("customer"))[[1]], t)
  expect_equal(arr$start_time, 1:3)
  expect_equal(arr$end_time, 1:3)
  expect_equal(arr$activity_time, rep(0, 3))
})

test_that("generators are reset", {
  expect_equal(3, simmer(verbose = env_verbose) %>%
    add_generator("dummy", trajectory(), at(0, 1, 2)) %>%
    run() %>% reset() %>% run() %>%
    get_mon_arrivals() %>% nrow()
  )

  t <- trajectory() %>%
    set_source("dummy", at(10)) %>%
    set_trajectory("dummy", trajectory() %>% timeout(1))

  env <- simmer(verbose = env_verbose) %>%
    add_generator("dummy", t, at(0))

  df1 <- env %>%
    run() %>%
    get_mon_arrivals()
  df2 <- env %>%
    reset() %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(df1, df2)
})

test_that("preemptible < priority shows a warning", {
  expect_warning(
    simmer(verbose = env_verbose) %>%
      add_generator("dummy", trajectory(), at(0), priority = 3, preemptible = 1))
})

test_that("arrival names and start times are correctly retrieved", {
  t <- trajectory() %>%
    log_(function() paste(get_name(env), get_start_time(env)))

  env <- simmer() %>%
    add_generator("dummy", t, at(1))

  expect_output(run(env), "1: dummy0: dummy0 1")
  expect_error(get_name(env))
  expect_error(get_start_time(env))
})

test_that("arrivals are correctly monitored", {
  a <- trajectory() %>%
    seize("res2", 1) %>%
    batch(1) %>%
    seize("res1", 1) %>%
    timeout(5) %>%
    release("res1", 1) %>%
    separate() %>%
    release("res2", 1)

  b <- trajectory() %>%
    seize("res1", 1) %>%
    timeout(6) %>%
    release("res1", 1)

  c <- trajectory() %>%
    seize("res1", 1) %>%
    timeout(1) %>%
    rollback(1, times = Inf)

  env <- simmer(verbose = env_verbose) %>%
    add_resource("res1", 1) %>%
    add_resource("res2") %>%
    add_generator("a", a, at(0)) %>%
    add_generator("b", b, at(0)) %>%
    add_generator("c", c, at(0)) %>%
    add_generator("d", c, at(1), mon = FALSE) %>%
    run(until = 4)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(NA_real_, NA, NA))
  expect_equal(arr1$activity_time, c(NA_real_, NA, NA))
  expect_equal(arr1$finished, rep(FALSE, 3))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(NA_real_, NA, NA, NA))
  expect_equal(arr2$activity_time, c(NA_real_, NA, NA, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))

  env %>%
    reset() %>%
    run(until = 10)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(5, NA, NA))
  expect_equal(arr1$activity_time, c(5, NA, NA))
  expect_equal(arr1$finished, c(TRUE, FALSE, FALSE))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(5, 5, NA, NA))
  expect_equal(arr2$activity_time, c(5, 5, NA, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))

  env %>%
    reset() %>%
    run(until = 12)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(5, 11, NA))
  expect_equal(arr1$activity_time, c(5, 6, NA))
  expect_equal(arr1$finished, c(TRUE, TRUE, FALSE))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(5, 5, 11, NA))
  expect_equal(arr2$activity_time, c(5, 5, 6, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))
})

test_that("several generators can be attached at once", {
  env <- simmer(verbose = env_verbose) %>%
    add_generator(letters[1:3], trajectory(), function() 1)

  expect_equal(get_sources(env), letters[1:3])
})
