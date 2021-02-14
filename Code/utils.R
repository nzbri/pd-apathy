# Copyright 2020 New Zealand Brain Research Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# utils.R

###############################################################################
# Simple wrappers around lubridate functions for calculating manipulating dates

# Using lubridate deals with the fact that months/years are not a fixed unit
# of time. Using these functions is useful for manipulating ages, which are
# functions of calendar years, not fixed-length intervals.
# See e.g. https://stackoverflow.com/q/15569333

# start_date = lubridate::ymd("2020-01-01")  # Leap year
# end_date   = lubridate::ymd("2021-01-01")
# days_between(start_date, end_date)
# years_between(start_date, end_date)
# as.numeric(end_date - start_date, units = "days") / 365.25

time_between <- function(start_date, end_date, unit) {
  delta <- lubridate::time_length(
    lubridate::interval(start_date, end_date),
    unit = unit
  )
  return(delta)
}

days_between <- function(start_date, end_date) {
  return(time_between(start_date, end_date, unit = "days"))
}

years_between <- function(start_date, end_date) {
  return(time_between(start_date, end_date, unit = "years"))
}

###############################################################################
