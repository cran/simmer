# Copyright (C) 2015-2016 Iñaki Ucar
# Copyright (C) 2016 Iñaki Ucar and Bart Smeets
# Copyright (C) 2016-2023 Iñaki Ucar
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

test_that("a numeric rollback points to the correct activity", {
  t0 <- trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, TRUE, trajectory() %>% timeout(function() 1)) %>%
    rollback(3)
  expect_output(activity_print_(t0$tail(), 0, 0), "Seize")

  t0 <- trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, TRUE, trajectory() %>% timeout(function() 1)) %>%
    rollback(Inf)
  expect_output(activity_print_(t0$tail(), 0, 0), "Seize")

  t0 <- trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, TRUE, trajectory() %>% timeout(function() 1)) %>%
    rollback(5)
  expect_output(activity_print_(t0$tail(), 0, 0), "Seize")

  expect_output(activity_print_(join(t0, t0)$tail(), 0, 0), "Branch")

  t0 <- trajectory() %>%
    seize("dummy", 1) %>%
    branch(function() 1, TRUE,
           trajectory() %>%
             rollback(2))

  env <- simmer(verbose = env_verbose) %>%
    add_resource("dummy", 2, 0) %>%
    add_generator("one", t0, at(0))

  expect_warning(run(env))
  expect_equal(env %>% get_server_count("dummy"), 2)
})

test_that("a character rollback points to the correct activity", {
  t0 <- trajectory() %>%
    timeout(1, tag="foo")
  t1 <- trajectory() %>%
    rollback("foo", 1)

  expect_output(activity_print_(t0$tail(), 0, 0), "| [foo]")
  expect_output(activity_print_(t1$tail(), 0, 0), "target: foo")

  env <- simmer(verbose = env_verbose) %>%
    add_generator("dummy", t1, at(0))
  expect_error(run(env), "rollback failed") # tag not found

  env <- simmer(verbose = env_verbose) %>%
    add_generator("dummy", join(t0, t1), at(0)) %>%
    run()
  expect_equal(get_mon_arrivals(env)$end_time, 2)
})

test_that("a rollback loops the correct number of times", {
  three_times <- function() {
    count <- 0
    function() {
      if (count < 3) {
        count <<- count + 1
        TRUE
      } else FALSE
    }
  }

  t0 <- trajectory() %>% rollback(0, 3)
  t1 <- trajectory() %>% rollback(0, check = three_times())

  env0 <- evaluate_promise(simmer(verbose = TRUE) %>% add_generator("dummy", t0, at(0)))$result
  env1 <- evaluate_promise(simmer(verbose = TRUE) %>% add_generator("dummy", t1, at(0)))$result

  output <- paste0(".*(",
    ".*0.*dummy0.*Rollback",
    ".*0.*dummy0.*Rollback",
    ".*0.*dummy0.*Rollback",
    ".*0.*dummy0.*Rollback",
  ").*")

  expect_output(env0 %>% run(), output)
  expect_output(env1 %>% run(), output)

  t0 <- trajectory() %>% rollback(0, Inf)
  expect_output(print(tail(t0)), "-1")
})

test_that("a negative amount is converted to positive", {
  t0 <- trajectory() %>% seize("dummy", 1)

  expect_silent(t0 %>% rollback(-1, -1))
  expect_output(activity_print_(t0$tail(), 0, 0), "target: 1 (Seize), times: 1", fixed = TRUE)
})

test_that("a check function that returns a non-boolean value fails", {
  t0 <- trajectory() %>%
    rollback(1, check = function() "dummy")

  env <- simmer(verbose = env_verbose) %>%
    add_generator("entity", t0, function() 1)

  expect_error(env %>% run(100))
})

test_that("incorrect types fail", {
  expect_error(trajectory() %>% rollback(0, "dummy"))
  expect_error(trajectory() %>% rollback(0, check = 0))
  expect_error(trajectory() %>% rollback(0, tag=0))
})
