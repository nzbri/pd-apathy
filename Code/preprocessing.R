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

# preprocessing.R

# To use these functions, simply `source("initialise_environment.R")`. That
# will ensure dependencies are appropriately managed.

###############################################################################

# Selects a sensible subset of variables for further analysis
preprocessing.select_key_variables <- function(data) {
  data <- data %>%
    # For convenience
    arrange(subject_id, session_date) %>%
    # Variables of interest
    select(
      # Database
      subject_id, session_id,
      # Subject-level
      sex, ethnicity, education, handedness, side_of_onset,
      # Age related
      age, age_at_symptom_onset, age_at_diagnosis, age_at_death,
      # Cognitive scores
      global_z, MoCA, WTAR,
      #  Neuropsych subdomains
      attention_domain, executive_domain, language_domain,
      learning_memory_domain, visuo_domain,
      # Individual neuropsych tests
      starts_with("nptest_"),
      # Key psychological screens
      NPI_apathy_present, NPI_total, HADS_anxiety, HADS_depression,
      # Clinical measures
      diagnosis, Hoehn_Yahr, UPDRS_motor_score, LED, taking_antidepressants,
      # Confounds
      first_session_date, session_date, UPDRS_source
    )

  return(data)
}

###############################################################################

# Applies a set of transformations to make variables more interpretable or
# amenable to modelling
preprocessing.transform_key_variables <- function(data) {
  data <- data %>%
    # Add / replace some variables
    mutate(
      # Add some useful extra timing info
      years_since_diagnosis = age - age_at_diagnosis,
      #years_since_symptom_onset = age - age_at_symptom_onset,
      age = NULL,
      # And transform LED to make more Gaussian
      transformed_dose = sqrt(LED),
      LED = NULL,
      # Extra confounds
      session_date2 = scale(session_date) ^ 2,
      first_session_date2 = scale(first_session_date) ^ 2
    ) %>%
    # Center & rescale
    mutate(across(
      c(
        # Subject-level
        education,
        # Cognitive scores
        MoCA, WTAR,
        # Key psychological screens
        NPI_total, HADS_anxiety, HADS_depression,
        # Clinical measures
        UPDRS_motor_score, transformed_dose,
        # Confounds
        session_date, session_date2, first_session_date, first_session_date2,
        # Note that global_z / subdomains / individual tests are designed such
        # that they are approximately scaled already
      ),
      # Rescale, keeping the mean/std attrs around
      # ~ as.vector(scale(.x, center = TRUE, scale = TRUE))
      function(col) {
        scaled_raw = scale(col, center = TRUE, scale = TRUE)
        scaled_col = as.vector(scaled_raw)
        attr(scaled_col, "scaled:center") <- attr(scaled_raw, "scaled:center")
        attr(scaled_col, "scaled:scale") <- attr(scaled_raw, "scaled:scale")
        return(scaled_col)
      }
    )) %>%
    # Recode years -> decades (and roughly center)
    mutate(
      #age = (age - 70.0) / 10.0,
      age_at_symptom_onset = (age_at_symptom_onset - 70.0) / 10.0,
      age_at_diagnosis = (age_at_diagnosis - 70.0) / 10.0,
      age_at_death = (age_at_death - 70.0) / 10.0,
      years_since_diagnosis = years_since_diagnosis / 10.0,
    ) %>%
    # Set reference level for some factors
    mutate(
      ethnicity = relevel(ethnicity, ref = "New Zealand European"),
      taking_antidepressants = factor(as.logical(taking_antidepressants), levels = c(FALSE, TRUE), labels = c("No", "Yes")),
      taking_antidepressants = relevel(taking_antidepressants, ref = "No")
    )

  return(data)
}

###############################################################################
