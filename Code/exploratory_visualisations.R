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

# exploratory_visualisations.R

###############################################################################

source("initialise_environment.R")

###############################################################################

full_data <- readRDS(
  file.path("..", "Data", "raw-data_2020-10-27.rds")
)

###############################################################################
# (Optional) Read from CSV using type hints

# https://stackoverflow.com/a/44581290
types <- as_tibble(list(
    col_type = c("chr", "int", "dbl", "lgl", "fct", "ord", "date"),
    code = c("c", "i", "d", "l", "f", "f", "D")
  )
)

col_types <- read_csv(
  file.path("..", "Data", "raw-data_2020-10-27_types.csv"),
  col_types = "cc"
)
col_types <- col_types %>%
  left_join(types, by = "col_type") %>%
  summarise(col_types = str_c(code, collapse = "")) %>%
  unlist(use.names = FALSE)

full_data <- read_csv(
  file.path("..", "Data", "raw-data_2020-10-27.csv"),
  col_types = col_types
)

###############################################################################

ggplot(full_data, aes(years_from_baseline)) +
  geom_histogram(binwidth = 1.0, boundary = 0.0, color="white") +
  theme_light()

ggplot(full_data, aes(apathy_score)) +
  geom_histogram(binwidth = 1, center = 0, color="white") +
  theme_light()

###############################################################################