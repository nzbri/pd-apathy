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
# Imputation methods

# Start by filling in data within subject
# MICE imputation is pretty complex for proper two-level modelling, but the
# naive approach doesn't account for subject structure giving funny results.
# Here we simply take the previous data point forwards in time (which isn't as
# silly as it sounds given that a lot of what we are imputing is `global_z`
# from the more recent and frequent short sessions).
preprocessing.fill_within_subject <- function(data) {
  data <- data %>%
    arrange(subject_id, session_date) %>%
    group_by(subject_id) %>%
    fill(everything(), .direction = "downup") %>%
    ungroup()

  return(data)
}

# Multiple imputation
preprocessing.run_mice <- function(data) {
  data <- data %>%
    # For MICE
    mutate(subject_int = as.integer(as.factor(subject_id)))

  # Prepare MICE settings
  # Note that we cannot have a variable be used as a predictor but not imputed
  # itself (e.g. we may not be interested in `WTAR` per se, but think it might
  # help interpolating other cognitive tests: however, we still need to impute it)
  method <- mice::make.method(data)
  pred = mice::make.predictorMatrix(data)  # target = rows, predictors = columns
  # Remove database specific variables as predictors
  pred[, c("subject_id", "subject_int", "session_id")] <- 0
  # Remove variables that cause problems as predictors
  # https://stefvanbuuren.name/fimd/sec-toomany.html#finding-problems-loggedevents
  pred[, c("diagnosis", "Hoehn_Yahr", "UPDRS_source")] <- 0
  # Remove age at death: valid missingness!
  pred[, "age_at_death"] <- 0
  pred["age_at_death", ] <- 0
  method["age_at_death"] <- ""
  # Tweak default methods for imputing data to account for 2-level structure, where continuous
  # https://stefvanbuuren.name/fimd/sec-mlguidelines.html
  # https://www.gerkovink.com/miceVignettes/Multi_level/Multi_level_data.html
  method[c("age_at_symptom_onset", "age_at_diagnosis", "education")] <- "2lonly.pmm"
  pred[c("age_at_symptom_onset", "age_at_diagnosis", "education"), "subject_int"] <- -2
  # Break feedback loop between correlated age variables
  # full_data %>% select(contains("age")) %>% mice::md.pattern()
  pred["age_at_symptom_onset", "age_at_diagnosis"] <- 0

  # Run imputation
  imputed_data <- data %>%
    mice::mice(m = 10, maxit = 10, method = method, predictorMatrix = pred) %>%
    # https://stackoverflow.com/a/30892119
    mice::complete(action = "long", include = TRUE) %>%
    as_tibble() %>%
    # Remove dummy variables
    mutate(subject_int = NULL) %>%
    # Ensure subject-level variables are consistent
    group_by(.imp, subject_id) %>%
    mutate(
      across(c(sex, ethnicity, handedness, side_of_onset), ~ utils.mode(.x))
    ) %>%
    ungroup() %>%
    mice::as.mids()

  return(imputed_data)
}

###############################################################################
