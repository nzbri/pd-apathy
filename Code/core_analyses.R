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

# core_analyses.R

###############################################################################

source("initialise_environment.R")

###############################################################################

full_data <- readRDS(
  file.path("..", "Data", "raw-data_2020-11-26.rds")
)

###############################################################################
# Additional exclusion criteria

full_data <- full_data %>%
  drop_na(NPI_apathy_present)

###############################################################################
# Data preprocessing / variable selection

# TODO:
#  + Transformation / scaling of variables

full_data <- full_data %>%
  # For convenience
  arrange(subject_id, session_date) %>%
  # Variables of interest
  select(
    # Database
    subject_id, session_id,
    # Subject-level
    sex, ethnicity, education, handedness, side_of_onset,
    # Age related
    age, age_at_symptom_onset, age_at_diagnosis,
    # Cognitive scores
    global_z, MoCA, WTAR,
    # Neuropsych tests
    NPI_apathy_present, NPI_total, HADS_anxiety, HADS_depression,
    # Clinical measures
    diagnosis, Hoehn_Yahr, UPDRS_motor_score, UPDRS_source, LED,
    # Confounds
    full_assessment, first_session_date
  ) %>%
  # Add some useful extra timing info
  mutate(years_since_diagnosis = age - age_at_diagnosis)

###############################################################################
# Imputation

# TODO:
#  + MICE for imputation
#  + Do we formulate this as a two-level model?

###############################################################################
# Fit models

# TODO:
#  + Set priors
#  + Wrap this in an outer loop for groups of covariates

base_formula <- "NPI_apathy_present ~ 1"
covariates <- c("age_at_diagnosis", "education", "sex")  # "ethnicity"
#covariates <- c("(1 | subject_id)")
combinations <- unlist(
  lapply(seq_along(covariates), combn, x = covariates, simplify = FALSE),
  recursive = FALSE
)
formulas <- combinations %>%
  lapply(
    function(combination) {
      paste(base_formula, "+", paste(combination, collapse = " + "))
    }
  ) %>%
  prepend(base_formula) %>%
  lapply(as.formula)
print(formulas)

# Fit each model in turn, recording LOO info
models = vector("list", length(formulas))
for (i in seq_along(formulas)) {
  print(formulas[[i]])

  model <- brms::brm(
    formula = formulas[[i]],
    family = brms::bernoulli(link = "logit"),
    data = imputed_data
  )
  model <- add_criterion(model, "loo")
  #print(summary(model))

  models[[i]] <- model
}
print(models)
#tail(models, n = 1)

# This is awful! For some reason the canonical `do.call` versions crash RStudio
comparison <- eval(parse(text = paste(
  "brms::loo_compare(",
  "models[[",
  paste(seq_along(formulas), collapse = "]], models[["),
  "]]",
  ", criterion = \"loo\", model_names = formulas)",
  sep = ""
)))
# brms::loo_compare(models) #, criterion = "loo")
# do.call(brms::loo_compare, c(models, criterion = "loo"))
# do.call(brms::loo_compare, models)
print(comparison)
winning_formula <- rownames(comparison)[1]
print(winning_formula)

###############################################################################
