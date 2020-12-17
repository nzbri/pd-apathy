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

source("utils.R")

date_string = format(lubridate::ymd(lubridate::today()))

###############################################################################

full_data <- readRDS(
  file.path("..", "Data", "raw-data_2020-11-26.rds")
)

###############################################################################
# Additional exclusion criteria

full_data <- full_data %>%
  drop_na(NPI_apathy_present)

###############################################################################
# Variable selection

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
    diagnosis, Hoehn_Yahr, UPDRS_motor_score, LED,
    # Confounds
    first_session_date, session_date, UPDRS_source
  )

###############################################################################
# Imputation

imputed_data <- full_data %>%
  mutate(subject_int = as.integer(as.factor(subject_id)))

# Prepare MICE settings
method <- mice::make.method(imputed_data)
pred = mice::make.predictorMatrix(imputed_data)  # target = rows, predictors = columns
# Remove database specific variables
pred[, c("subject_id", "subject_int", "session_id")] <- 0
# Remove variables that cause problems as predictors
# https://stefvanbuuren.name/fimd/sec-toomany.html#finding-problems-loggedevents
pred[, c("diagnosis", "Hoehn_Yahr", "UPDRS_source")] <- 0
# Tweak default methods for imputing data to account for 2-level structure, where continuous
# https://stefvanbuuren.name/fimd/sec-mlguidelines.html
# https://www.gerkovink.com/miceVignettes/Multi_level/Multi_level_data.html
method[c("age_at_symptom_onset", "age_at_diagnosis", "education")] <- "2lonly.pmm"
pred[c("age_at_symptom_onset", "age_at_diagnosis", "education"), "subject_int"] <- -2
# Break feedback loop between correlated age variables
# full_data %>% select(contains("age")) %>% mice::md.pattern()
pred["age_at_symptom_onset", "age_at_diagnosis"] <- 0

# Run imputation
imputed_data <- imputed_data %>%
  mutate(subject_id = as.integer(as.factor(subject_id))) %>%
  mice::mice(
    m = 1, maxit = 10,
    method = method, predictorMatrix = pred
  ) %>%
  mice::complete(action = 1) %>%
  as_tibble() %>%
  mutate(subject_int = NULL)

# Ensure subject-level variables are consistent
# https://stackoverflow.com/a/8189441
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

imputed_data <- imputed_data %>%
  group_by(subject_id) %>%
  mutate(
    across(c(sex, ethnicity, handedness, side_of_onset), ~ mode(.x))
  ) %>%
  ungroup()

###############################################################################
# Data preprocessing / transformations

imputed_data <- imputed_data %>%
  # Add some useful extra timing info
  mutate(years_since_diagnosis = age - age_at_diagnosis)

# Store means / standard deviations for later
imputed_data.mean <- imputed_data %>%
  summarise(across(
    where(is.numeric) | where(is.logical),
    ~ mean(.x, na.rm = TRUE)
  ))
imputed_data.sd <- imputed_data %>%
  summarise(across(
    where(is.numeric) | where(is.logical),
    ~ sd(.x, na.rm = TRUE)
  ))

# Center / rescale selected columns
transformed_data <- imputed_data %>%
  mutate(first_session_date2 = scale(first_session_date) ^ 2) %>%
  # Center & rescale
  mutate(across(
    c(age_at_diagnosis, education, first_session_date, first_session_date2),
    ~ as.vector(scale(.x, center = TRUE, scale = TRUE))
  )) %>%
  # Rescale only
  mutate(across(
    c(years_since_diagnosis, HADS_depression, UPDRS_motor_score),
    ~ as.vector(scale(.x, center = FALSE, scale = TRUE))
  )) %>%
  # Split LED into two regressors, and rescale
  mutate(
    taking_medication = (LED > 0.0),
    transformed_dose = sqrt(LED)
  ) %>%
  group_by(taking_medication) %>%
  mutate(
    transformed_dose = as.vector(scale(transformed_dose, center = TRUE, scale = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    transformed_dose = if_else(is.na(transformed_dose), 0.0, transformed_dose)
  ) %>%
  #select(taking_medication, transformed_dose) %>% print(n = 100)
  #ggplot(aes(transformed_dose)) + geom_histogram()
  # Set reference level for some factors
  mutate(
    ethnicity = relevel(ethnicity, ref = "New Zealand European"),
    taking_medication = factor(taking_medication, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    taking_medication = relevel(taking_medication, ref = "Yes"),
  )

###############################################################################
# Fit models

filename <- file.path(
  "..", "Results", paste("core-analyses_", date_string, ".Rout", sep = "")
)
sink(file = filename)
options(width = 1024)

# Data summary?
#writeLines(paste("\n", strrep("*", 72), "\n", sep = ""))

base_formula <- "NPI_apathy_present ~ 1"
for (
  covariates in list(
    c("first_session_date", "first_session_date2"),
    c("sex", "ethnicity", "education", "age_at_diagnosis"),
    c("(1 | subject_id)"),
    c("years_since_diagnosis", "global_z", "UPDRS_motor_score", "HADS_depression", "taking_medication + transformed_dose")
  )
) {
  writeLines(paste("\n", strrep("*", 72), sep = ""))
  writeLines(paste("Base formula:", sQuote(base_formula)))
  writeLines(paste("Covariates:  ", paste(lapply(covariates, sQuote), collapse = ", ")))

  # ---------------------------------------------------------------------------

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
  #print(formulas)

  # ---------------------------------------------------------------------------
  writeLines(paste("\n", strrep("-", 72), sep = ""))
  writeLines("Fitting individual models")

  # Fit each model in turn, recording LOO info
  models = vector("list", length(formulas))
  for (i in seq_along(formulas)) {
    writeLines(paste("\n", strrep("-", 36), sep = ""))
    writeLines(paste("Model", i))
    #writeLines("Formula:")
    #writeLines(format(formulas[[i]]))
    writeLines("")

    prior <- brms::get_prior(
      formula = formulas[[i]],
      family = brms::bernoulli(link = "logit"),
      data = transformed_data
    )
    if (any(prior$class == "b")) {
      prior <- brms::set_prior("normal(0.0, 1.0)", class = "b")
    } else {
      prior <- brms::empty_prior()
    }

    model <- brms::brm(
      formula = formulas[[i]],
      family = brms::bernoulli(link = "logit"),
      prior = prior,
      data = transformed_data,
      silent = TRUE, refresh = 0
    )
    model <- add_criterion(model, "loo")
    print(summary(model))

    models[[i]] <- model
  }

  # ---------------------------------------------------------------------------
  writeLines(paste("\n", strrep("-", 72), sep = ""))
  writeLines("Model comparison")

  #print(models)
  #print(tail(models, n = 1))

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
  writeLines(paste("\nWinning formula:", sQuote(winning_formula)))

  # ---------------------------------------------------------------------------
  writeLines(paste("\n", strrep("-", 72), sep = ""))
  writeLines("Comparison of individual terms to baseline")

  for (i in seq_along(covariates)) {
    individual_comparison <- brms::loo_compare(
      models[[1]], models[[i + 1]],
      criterion = "loo",
      model_names = c("base_model", paste("base_model +", covariates[[i]]))
    )
    writeLines("")
    print(individual_comparison)
  }

  # ---------------------------------------------------------------------------

  base_formula <- winning_formula
}
writeLines(paste("\n", strrep("*", 72), "\n", sep = ""))

sink()
options(width = 80)
file.show(filename)
#writeLines(readLines(filename))

###############################################################################
