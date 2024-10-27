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

# Use gtsummary for data summary tables
# http://www.danieldsjoberg.com/gtsummary/

###############################################################################

source("initialise_environment.R")

###############################################################################

# Whether to include sessions where the NPI apathy measure is missing
options.include_missing = TRUE

###############################################################################

full_data <- utils.load_data() %>%
  {if (!options.include_missing) drop_na(., NPI_apathy_present) else .}

###############################################################################
# Common preprocessing utilities

# Oh seriously :-(
gtsummary_names <- mapply(
  {function(item, name) as.formula(paste(name, " ~ \"", item, "\"", sep = ""))},
  constants.variable_names,
  names(constants.variable_names)
) %>%
  as.list()
gtsummary_names[["first_session_date"]] <- first_session_date ~ "Enrolment decade"
gtsummary_names[["years_since_diagnosis"]] <- years_since_diagnosis ~ "Years since diagnosis"

# Converts TRUE/FALSE to Yes/No for readability
logical_to_factor <- function(data) {
  return(factor(data, levels = c(TRUE, FALSE), labels = c("Yes", "No")))
}
# Converts NA to `name` for readability
na_to_level <- function(data, name) {
  return(factor(
    data,
    levels = c(levels(data), NA),
    labels = c(levels(data), name),
    exclude = NULL
  ))
}

###############################################################################
# Common table generation code

make_summary_tbl <- function(full_data) {
  tbl_data <- full_data %>%
    # Clean up variables
    mutate(
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
      )
    ) %>%
    mutate(across(c(MoCA, HADS_anxiety, HADS_depression), as.integer)) %>%
    # Give logical variables better names
    mutate(across(where(is.logical), logical_to_factor)) %>%
    # And better names for missing apathy
    mutate(NPI_apathy_present = na_to_level(NPI_apathy_present, "Unknown")) %>%
    # Reduce to variables of interest
    select(
      NPI_apathy_present,
      age, sex, ethnicity, education,
      age_at_diagnosis, years_since_diagnosis,
      taking_medication, LED, taking_antidepressants,
      UPDRS_motor_score, MoCA, HADS_depression, HADS_anxiety,
      first_session_date,
    )

  # Make the table!
  summary_tbl <- tbl_data %>%
    gtsummary::tbl_summary(
      by = NPI_apathy_present,
      percent = "row", # Summarise within condition rather than by apathy status
      label = unname(gtsummary_names[colnames(.)]),
      type = list(
        # Put continuous data on a separate line (matches categorical)
        gtsummary::all_continuous() ~ "continuous2",
        # Show both true and false for dichotomous variables
        gtsummary::all_dichotomous() ~ "categorical"
      ),
      # Different statistics for continuous and discrete numerical values
      statistic = list(
        where(is.integer) ~ "{median} ({p25} \U2013 {p75})",
        where(is.double) ~ "{mean} (±{sd})",
        gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
      ),
      # No. of significant figures
      digits = list(
        where(is.double) ~ 1
      )
    ) %>%
    gtsummary::add_n() %>%  # add column with total number of non-missing observations
    gtsummary::add_p(  # test for a difference between groups
      # https://stackoverflow.com/a/68687663
      test.args = gtsummary::all_tests("fisher.test") ~ list(simulate.p.value = TRUE)
    ) %>%
    # Tidy up headers
    gtsummary::bold_labels() %>%
    gtsummary::modify_header(label ~ "**Variable**") %>%
    gtsummary::modify_spanning_header(
      c("stat_1", "stat_2", "stat_3") ~ "**Apathetic**"
    )

  return(summary_tbl)
}

###############################################################################
# Full table across all sessions

session_tbl <- full_data %>%
  # Make the table!
  make_summary_tbl()

print(session_tbl)
utils.save_table(session_tbl, "sessions")

###############################################################################
# Summarise first sessions only

first_session_tbl <- full_data %>%
  # First sessions only
  group_by(subject_id) %>%
  arrange(session_date) %>%
  slice(1) %>%
  ungroup() %>%
  # Make the table!
  make_summary_tbl()

print(first_session_tbl)
utils.save_table(first_session_tbl, "initial_sessions")

###############################################################################
