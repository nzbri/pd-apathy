# Copyright 2024 New Zealand Brain Research Institute
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

# summary_tables.R

###############################################################################

source("initialise_environment.R")
source("constants.R")
source("utils.R")

###############################################################################

full_data <- utils.load_data()

###############################################################################
# Data summary tables
# http://www.danieldsjoberg.com/gtsummary/

# Oh seriously :-(
gtsummary_names <- mapply(
  {function(item, name) as.formula(paste(name, " ~ \"", item, "\"", sep = ""))},
  constants.variable_names,
  names(constants.variable_names)
) %>%
  as.list()
gtsummary_names[["first_session_date"]] <- first_session_date ~ "Enrolment decade"
gtsummary_names[["years_since_diagnosis"]] <- years_since_diagnosis ~ "Years since diagnosis"

session_tbl <- full_data %>%
  #drop_na(NPI_apathy_present) %>%
  mutate(
    # Give apathy better names as we stratify based on this
    NPI_apathy_present = factor(
      NPI_apathy_present,
      levels = c(TRUE, FALSE, NA),
      labels = c("Yes", "No", "Unknown"),
      exclude = NULL,
    ),
    # Clean up other variables
    taking_medication = (LED > 0.0),
    years_since_diagnosis = age - age_at_diagnosis,
    education = forcats::fct_collapse(
      as.factor(education),
      `<10` = as.character(seq(0, 9)),
      `10-14` = as.character(seq(10, 14)),
      `15-19` = as.character(seq(15, 19)),
      `>20` = as.character(seq(20, 29)),
    ),
    first_session_date = as.factor(
      floor(lubridate::year(first_session_date) / 10) * 10  # Round to decade,
    ),
    mutate(across(c(MoCA, HADS_anxiety, HADS_depression), as.integer)),
  ) %>%
  # Variables of interest
  select(
    NPI_apathy_present,
    age, sex, ethnicity, education,
    age_at_diagnosis, years_since_diagnosis,
    taking_medication, LED, taking_antidepressants,
    UPDRS_motor_score, MoCA, HADS_depression, HADS_anxiety,
    first_session_date,
  ) %>%
  # Make the table!
  gtsummary::tbl_summary(
    by = NPI_apathy_present,
    label = unname(gtsummary_names[colnames(.)]),
    type = list(
      gtsummary::all_continuous() ~ "continuous2"
    ),
    statistic = list(
      where(is.integer) ~ "{median} ({p25} \U2013 {p75})",
      where(is.double) ~ "{mean} (±{sd})",
      gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = list(
      where(is.double) ~ 1
    )
  ) %>%
  gtsummary::add_n() %>%  # add column with total number of non-missing observations
  gtsummary::add_p(  # test for a difference between groups
    # https://stackoverflow.com/a/68687663
    test.args = gtsummary::all_tests("fisher.test") ~ list(simulate.p.value = TRUE)
  ) %>%
  gtsummary::bold_labels() %>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  gtsummary::modify_spanning_header(
    c("stat_1", "stat_2", "stat_3") ~ "**Apathetic**"
  )

print(session_tbl)
utils.save_table(session_tbl, "sessions")

###############################################################################