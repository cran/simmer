# Copyright (C) 2015-2023 Iñaki Ucar
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

test_that("the wrapper behaves as expected", {
  expect_error("asdf" %>% wrap())

  t0 <- trajectory() %>%
    seize("server", 1) %>%
    set_attribute("attr", 1) %>%
    release("server", 1)

  t1 <- trajectory() %>%
    seize("server", 1) %>%
    timeout(1) %>%
    rollback(1, times = Inf)

  env <- simmer(verbose = env_verbose) %>%
    add_resource("server", 1, 0) %>%
    add_generator("dummy", t0, at(1:10), mon = 2) %>%
    add_generator("rollback", t1, at(11)) %>%
    run(11.5) %>%
    wrap()

  expect_output(print(env))

  expect_equal(env %>% now(), 11.5)
  expect_equal(env %>% peek(), 12)

  arrivals <- env %>% get_mon_arrivals(ongoing = TRUE)
  arrivals_res <- env %>% get_mon_arrivals(TRUE, ongoing = TRUE)
  expect_equal(nrow(arrivals), 11)
  expect_equal(nrow(arrivals_res), 11)

  arrivals <- env %>% get_mon_arrivals(ongoing = FALSE)
  arrivals_res <- env %>% get_mon_arrivals(TRUE, ongoing = FALSE)
  attributes <- env %>% get_mon_attributes()
  resources <- env %>% get_mon_resources()

  expect_equal(nrow(arrivals), 10)
  expect_equal(nrow(arrivals_res), 10)
  expect_equal(nrow(attributes), 10)
  expect_equal(nrow(resources), 21)
  expect_equal(env %>% get_sources(), c("dummy", "rollback"))
  expect_error(env %>% get_n_generated("asdf"))
  expect_equal(env %>% get_n_generated("dummy"), 10)
  expect_equal(env %>% get_resources(), "server")
  expect_error(env %>% get_capacity("asdf"))
  expect_equal(env %>% get_capacity("server"), 1)
  expect_error(env %>% get_queue_size("asdf"))
  expect_equal(env %>% get_queue_size("server"), 0)
  expect_error(env %>% get_server_count("asdf"))
  expect_equal(env %>% get_server_count("server"), 1)
  expect_error(env %>% get_queue_count("asdf"))
  expect_equal(env %>% get_queue_count("server"), 0)
})
