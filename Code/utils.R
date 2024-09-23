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

# To use these functions, simply `source("initialise_environment.R")`. That
# will ensure dependencies are appropriately managed.

###############################################################################

utils.load_data <- function() {
  data <- readRDS(file.path("..", "Data", constants.data_file))
  return(data)
}

###############################################################################

# The built-in mode doesn't do what you expect, it's essentially typeof
# https://stackoverflow.com/a/8189441
utils.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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

# Save any printed output from a function to file
utils.save_output <- function(func, filename) {
  filepath <- file.path(
    "..", "Results", paste(
      filename,
      "_", constants.date_string,
      "_npi-", constants.NPI_apathy_threshold,
      ".txt",
      sep = ""
    )
  )

  sink(file = filepath)
  options(width = 1024)

  cat(paste("Data: ", constants.data_file, "\n", sep = ""))
  cat(paste("NPI apathy >= ", constants.NPI_apathy_threshold, "\n", sep = ""))
  cat(paste("Date: ", constants.date_string, "\n", sep = ""))
  cat(paste("\n", paste(rep("-", 79), collapse = ""), "\n\n", sep = ""))

  output <- func()

  cat(paste("\n", paste(rep("-", 79), collapse = ""), "\n\n", sep = ""))
  print(sessionInfo())

  sink()
  options(width = 80)

  file.show(filepath)

  return(output)
}

###############################################################################
# Useful plotting-related functions

utils.save_plot <- function(plt, filename, ..., width = 6, height = 4, units = "in") {
  # Note that this saves to `../Figures/`!
  ggsave(
    file.path("..", "Figures", paste(filename, ".pdf", sep = "")),
    plt, ..., width = width, height = height, units = units
  )
  ggsave(
    file.path("..", "Figures", paste(filename, ".jpg", sep = "")),
    plt, ..., width = width, height = height, units = units, dpi = "screen"
  )
}

# Just display if want to temporarily disable saving
#save_plot <- function(plt, filename, ...) {
#  print(filename)
#}
save_plot <- utils.save_plot

###############################################################################
# Useful table-related functions

utils.save_table <- function(gtsummary_tbl, filename, ...) {
  # Note that this saves to `../Tables/`!
  filepath = file.path(
    "..",
    "Tables",
    paste(filename, "_npi-", constants.NPI_apathy_threshold, ".md", sep = "")
  )
  gtsummary_tbl %>%
    gtsummary::as_kable(format = "markdown") %>%
    cat(file = filepath, sep = "\n", ...)
}

###############################################################################
