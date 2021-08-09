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
  file.path("..", "Data", "raw-data_2021-05-25.rds")
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
    # Cognitive scores (and subdomains)
    global_z, MoCA, WTAR,
    attention_domain, executive_domain, language_domain,
    learning_memory_domain, visuo_domain,
    # Neuropsych tests
    NPI_apathy_present, NPI_total, HADS_anxiety, HADS_depression,
    # Clinical measures
    diagnosis, Hoehn_Yahr, UPDRS_motor_score, LED, taking_antidepressants,
    # Confounds
    first_session_date, session_date, UPDRS_source
  )

full_data %>%
  summarise(across(everything(), ~mean(is.na(.x)))) %>%
  print(width = Inf)

# Quick visualisation of structure in missingness
full_data %>%
  arrange(session_date) %>%
  #group_by(subject_id) %>%
  #fill(everything(), .direction = "downup") %>%
  #ungroup() %>%
  mutate(across(everything(), is.na)) %>%
  mutate(y = row_number()) %>%
  pivot_longer(cols = !y, names_to = "x", values_to = "value") %>%
  mutate(x = factor(x, levels = colnames(full_data))) %>%
  ggplot(aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_y_reverse() +
  labs(
    x = NULL,
    y = "Subject (ordered by session date)",
    fill = "Missing",
    title = "Data missingness"
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################
# Data preprocessing / transformations

# Store means / standard deviations for later
full_data.mean <- full_data %>%
  summarise(across(
    where(is.numeric) | where(is.logical),
    ~ mean(.x, na.rm = TRUE)
  ))
full_data.sd <- full_data %>%
  summarise(across(
    where(is.numeric) | where(is.logical),
    ~ sd(.x, na.rm = TRUE)
  ))

transformed_data <- full_data %>%
  mutate(
    # Add some useful extra timing info
    years_since_diagnosis = age - age_at_diagnosis,
    #years_since_symptom_onset = age - age_at_symptom_onset,
    age = NULL,
    # Extra confounds
    session_date2 = scale(session_date) ^ 2,
    first_session_date2 = scale(first_session_date) ^ 2
  ) %>%
  # Center & rescale
  mutate(across(
    c(
      age_at_diagnosis, years_since_diagnosis, education,
      session_date, session_date2, first_session_date, first_session_date2,
      MoCA, HADS_depression, HADS_anxiety, UPDRS_motor_score
    ),
    ~ as.vector(scale(.x, center = TRUE, scale = TRUE))
  )) %>%
  ungroup() %>%
  # Rescale only
  #mutate(across(
  #  c(years_since_diagnosis), #, HADS_depression, HADS_anxiety, UPDRS_motor_score),
  #  ~ as.vector(scale(.x, center = FALSE, scale = TRUE))
  #)) %>%
  #ungroup() %>%
  # Split LED into two regressors, and rescale
  mutate(
    taking_medication = (LED > 0.0),
    transformed_dose = sqrt(LED),
    LED = NULL
  ) %>%
  group_by(taking_medication) %>%
  mutate(
    transformed_dose = as.vector(scale(
      transformed_dose, center = TRUE, scale = TRUE
    ))
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
    taking_antidepressants = factor(as.logical(taking_antidepressants), levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    taking_antidepressants = relevel(taking_antidepressants, ref = "No")
  )

###############################################################################
# Imputation

# Start by filling in data within subject
# MICE imputation is pretty complex for proper two-level modelling, but the
# naive approach doesn't account for subject structure giving funny results.
# Here we simply take the previous data point forwards in time (which isn't as
# silly as it sounds given that a lot of what we are imputing is `global_z`
# from the more recent and frequent short sessions).
imputed_data <- transformed_data %>%
  group_by(subject_id) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  # For MICE
  mutate(subject_int = as.integer(as.factor(subject_id)))

# Prepare MICE settings
# Note that we cannot have a variable be used as a predictor but not imputed
# itself (e.g. we may not be interested in `WTAR` per se, but think it might
# help interpolating other cognitive tests: however, we still need to impute it)
method <- mice::make.method(imputed_data)
pred = mice::make.predictorMatrix(imputed_data)  # target = rows, predictors = columns
# Remove database specific variables as predictors
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
# Conditional structure of transformed variables
# Don't predict medication yes/no from dose, former comes first in hypothetical generative model
pred["taking_medication", "transformed_dose"] <- 0

# Run imputation
imputed_data <- imputed_data %>%
  mice::mice(
    m = 10, maxit = 10,
    method = method, predictorMatrix = pred
  ) %>%
  # https://stackoverflow.com/a/30892119
  mice::complete(action = "long", include = TRUE) %>%
  as_tibble() %>%
  mutate(subject_int = NULL) %>%
  mice::as.mids()

# Ensure subject-level variables are consistent
# https://stackoverflow.com/a/8189441
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

imputed_data <- imputed_data %>%
  mice::complete(action = "long", include = TRUE) %>%
  as_tibble() %>%
  group_by(.imp, subject_id) %>%
  mutate(
    across(c(sex, ethnicity, handedness, side_of_onset), ~ mode(.x))
  ) %>%
  ungroup() %>%
  mice::as.mids()

###############################################################################

full_formula <-
  NPI_apathy_present ~ 1 +
  first_session_date + first_session_date2 + session_date + session_date2 +
  sex + education + age_at_diagnosis +  # ethnicity
  (1 | subject_id) +
  years_since_diagnosis + taking_medication + transformed_dose + taking_antidepressants +
  UPDRS_motor_score + HADS_depression + HADS_anxiety + global_z # MoCA
  #attention_domain + executive_domain + language_domain + learning_memory_domain + visuo_domain

prior <- brms::get_prior(
  formula = full_formula,
  family = brms::bernoulli(link = "logit"),
  data = mice::complete(imputed_data, action = 1)
)
if (any(prior$class == "b")) {
  prior <- brms::set_prior("normal(0.0, 1.0)", class = "b")
} else {
  prior <- brms::empty_prior()
}

model <- brms::brm_multiple(
  formula = full_formula,
  family = brms::bernoulli(link = "logit"),
  prior = prior,
  data = imputed_data,
  chains = 8, iter = 5000,
  silent = TRUE, refresh = 0
)
print(summary(model))

# Plot odds ratios
# TODO:
#  + Rescale by stddev?
#  + Flip to standard higher is better?
plt <- fixef(model) %>%
  exp() %>%  # Odds ratio := exp(beta)
  as_tibble(rownames = "covariate") %>%
  filter(covariate != "Intercept") %>%
  mutate(covariate = factor(covariate, levels = rev(covariate))) %>%
  ggplot(aes(x = covariate, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
  scale_y_continuous(trans = "log", breaks = c(0.33, 1.0, 3.0)) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(NULL) +
  ylab("Odds ratio (95% CI)") +
  theme_bw()  # use a white background

print(plt)
#ggsave("test.pdf", plt, width = 6, height = 4, units = "in")

###############################################################################
# Fit models

filename <- file.path(
  "..", "Results", paste("core-analyses_", date_string, ".Rout", sep = "")
)
sink(file = filename)
options(width = 1024)

# Data summary?
#writeLines(paste("\n", strrep("*", 72), "\n", sep = ""))

# brms::negbinomial()
# https://mc-stan.org/docs/2_25/functions-reference/nbalt.html
# https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html

base_formula <- "NPI_apathy_present ~ 1"  # NPI_apathy_present, HADS_anxiety, HADS_depression
#base_formula <- "NPI_apathy_present ~ 1 + sex + age_at_diagnosis + (1 | subject_id)"
for (
  covariates in list(
    c("first_session_date", "first_session_date2"),
    c("sex", "ethnicity", "education", "age_at_diagnosis"),
    c("(1 | subject_id)"),
    c("years_since_diagnosis", "global_z", "UPDRS_motor_score", "HADS_depression", "taking_medication + transformed_dose")
    #c("attention_domain", "executive_domain", "language_domain", "learning_memory_domain", "visuo_domain")
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
  models <- vector("list", length(formulas))
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

    model <- brms::brm_multiple(
      formula = formulas[[i]],
      family = brms::bernoulli(link = "logit"),
      prior = prior,
      data = lapply(
        group_split(transformed_data, .imp, .keep = FALSE),
        as.data.frame
      ),
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
sessionInfo()

sink()
options(width = 80)
file.show(filename)
#writeLines(readLines(filename))

###############################################################################

full_formula <-
  NPI_apathy_present ~ 1 +
    first_session_date + first_session_date2 +
    sex + education + age_at_diagnosis +  # ethnicity
    (1 | subject_id) +
    years_since_diagnosis + taking_medication + transformed_dose + taking_antidepressants +
    UPDRS_motor_score + HADS_depression + global_z  # MoCA
    #attention_domain + executive_domain + language_domain + learning_memory_domain + visuo_domain

prior <- brms::get_prior(
  formula = full_formula,
  family = brms::bernoulli(link = "logit"),
  data = transformed_data
)
if (any(prior$class == "b")) {
  prior <- brms::set_prior("normal(0.0, 1.0)", class = "b")
} else {
  prior <- brms::empty_prior()
}

model <- brms::brm_multiple(
  formula = full_formula,
  family = brms::bernoulli(link = "logit"),
  prior = prior,
  data = lapply(
    group_split(transformed_data, .imp, .keep = FALSE),
    as.data.frame
  ),
  silent = TRUE, refresh = 0
)
print(summary(model))

# Plot odds ratios
plt <- fixef(model) %>%
  exp() %>%  # Odds ratio := exp(beta)
  as_tibble(rownames = "covariate") %>%
  filter(covariate != "Intercept") %>%
  mutate(covariate = factor(covariate, levels = rev(covariate))) %>%
  ggplot(aes(x = covariate, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
  scale_y_continuous(trans = "log", breaks = c(0.33, 1.0, 3.0)) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(NULL) +
  ylab("Odds ratio (95% CI)") +
  theme_bw()  # use a white background

print(plt)
#ggsave("test.pdf", plt, width = 6, height = 4, units = "in")

###############################################################################
