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
  file.path("..", "Data", "raw-data_2022-08-31.rds")
)

variable_names <- list(
  NPI_apathy_present = "Apathetic",
  first_session_date = "Enrolment date",
  first_session_date2 = "Enrolment date (squared)",
  session_date = "Session date",
  session_date2 = "Session date (squared)",
  sex = "Sex",
  sexFemale = "Sex: female",
  sexMale = "Sex: male",
  education = "Education (years)",
  ethnicity = "Ethnicity",
  age = "Age",
  age_at_diagnosis = "Age at diagnosis",
  years_since_diagnosis = "Decades since diagnosis",
  taking_medication = "Taking levodopa (or equivalent)",
  taking_medicationNo = "Unmedicated",
  taking_medicationYes = "Taking levodopa (or equivalent)",
  transformed_dose = "Levodopa equivalent dose",
  LED = "Levodopa equivalent dose",
  taking_antidepressants = "Taking antidepressants",
  taking_antidepressantsYes = "Taking antidepressants",
  UPDRS_motor_score = "Motor symptoms (UPDRS)",
  MoCA = "Cognition (MoCA)",
  HADS_depression = "Depression (HADS)",
  HADS_anxiety = "Anxiety (HADS)",
  # Individual tests
  # https://github.com/nzbri/redcap/blob/master/python/export.py
  # https://doi.org/10.1101/2020.05.31.126870
  # Attention
  nptest_digits_fb_z = "Digits forwards/backwards [A]",
  nptest_digit_ordering_test_z = "Digit ordering [A]",
  nptest_map_search_z = "Map search (1 min) [A]",
  nptest_stroop_colour_z = "Stroop color reading [A]",
  nptest_stroop_words_z = "Stroop word reading [A]",
  nptest_trails_a_z = "Trail making (part A) [A]",
  # Executive
  nptest_action_fluency_z = "Action fluency [E]",
  nptest_letter_fluency_z = "Letter fluency [E]",
  nptest_category_fluency_z = "Category fluency [E]",
  nptest_category_switching_z = "Category switching [E]",
  nptest_trails_b_z = "Trail making (part B) [E]",
  nptest_stroop_inhibition_z = "Stroop interference [E]",
  # Visuo
  nptest_jlo_z = "Judgement of line orientation [V]",
  nptest_vosp_z = "VOSP fragmented letters [V]",
  nptest_picture_completion_z = "Picture completion [V]",
  nptest_rey_complex_copy_z = "RCFT copy [V]",
  # Memory
  nptest_cvlt_free_recall_z = "CVLT-II SF immediate recall [M]",
  nptest_cvlt_short_delay_z = "CVLT-II SF short delay [M]",
  nptest_cvlt_long_delay_z = "CVLT-II SF long delay [M]",
  nptest_rey_complex_immediate_z = "RCFT immediate recall [M]",
  nptest_rey_complex_delay_z = "RCFT delayed recall [M]",
  # Language
  nptest_boston_naming_z = "Boston naming test [L]",
  nptest_language_adas_cog = "ADAS-Cog: language [L]",
  nptest_language_drs2 = "Mattis DRS-2: similarities [L]"
)

###############################################################################
# Additional exclusion criteria

full_data <- full_data %>%
  # Drop sessions missing apathy measure
  drop_na(NPI_apathy_present)
  # Drop PDD
  # Could argue either way about dropping unknowns
  #filter(is.na(diagnosis) | diagnosis != "PDD") %>%
  # Drop `global_z`: useful for individual test analyses
  #filter(!is.na(global_z))  ## nptest

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
    age, age_at_symptom_onset, age_at_diagnosis, age_at_death,
    # Cognitive scores
    global_z, MoCA, WTAR,
    #  Neuropsych subdomains
    attention_domain, executive_domain, language_domain,
    learning_memory_domain, visuo_domain,
    # Individual neuropsych tests
    #starts_with("nptest_"),  ## nptest
    # Key psychological screens
    NPI_apathy_present, NPI_total, HADS_anxiety, HADS_depression,
    # Clinical measures
    diagnosis, Hoehn_Yahr, UPDRS_motor_score, LED, taking_antidepressants,
    # Confounds
    first_session_date, session_date, UPDRS_source
  ) %>%
  mutate(
    # Add some useful extra timing info
    years_since_diagnosis = age - age_at_diagnosis,
    #years_since_symptom_onset = age - age_at_symptom_onset,
    age = NULL,
    # And transform LED to make more Gaussian
    transformed_dose = sqrt(LED)
  )

full_data %>%
  summarise(across(everything(), ~mean(is.na(.x)))) %>%
  print(width = Inf)

# Quick visualisation of structure in missingness
plt <- full_data %>%
  #filter(!is.na(global_z)) %>%
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
    y = "Session (ordered by date)",
    fill = "Missing",
    title = "Data missingness"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plt)
save_plot(plt, "data-missingness")

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
    # Extra confounds
    session_date2 = scale(session_date) ^ 2,
    first_session_date2 = scale(first_session_date) ^ 2,
    # Remove duplicates
    LED = NULL # transformed_dose
  ) %>%
  # Center & rescale
  mutate(across(
    c(
      education,
      session_date, session_date2, first_session_date, first_session_date2,
      transformed_dose, MoCA, HADS_depression, HADS_anxiety, UPDRS_motor_score
    ),
    ~ as.vector(scale(.x, center = TRUE, scale = TRUE))
  )) %>%
  ungroup() %>%
  # Recode years -> decades (and roughly center)
  mutate(
    #age = (age - 70.0) / 10.0,
    age_at_symptom_onset = (age_at_symptom_onset - 70.0) / 10.0,
    age_at_diagnosis = (age_at_diagnosis - 70.0) / 10.0,
    age_at_death = (age_at_death - 70.0) / 10.0,
    years_since_diagnosis = years_since_diagnosis / 10.0,
  ) %>%
  # Split LED into two regressors, and rescale
  #mutate(
  #  taking_medication = (LED > 0.0),
  #  transformed_dose = sqrt(LED),
  #  LED = NULL
  #) %>%
  #group_by(taking_medication) %>%
  #mutate(
  #  transformed_dose = as.vector(scale(
  #    transformed_dose, center = TRUE, scale = TRUE
  #  ))
  #) %>%
  #ungroup() %>%
  #mutate(
  #  transformed_dose = if_else(is.na(transformed_dose), 0.0, transformed_dose)
  #) %>%
  #select(taking_medication, transformed_dose) %>% print(n = 100)
  #ggplot(aes(transformed_dose)) + geom_histogram()
  # Set reference level for some factors
  mutate(
    ethnicity = relevel(ethnicity, ref = "New Zealand European"),
    #taking_medication = factor(taking_medication, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    #taking_medication = relevel(taking_medication, ref = "No"),
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
# Conditional structure of transformed variables
# Don't predict medication yes/no from dose, former comes first in hypothetical generative model
#pred["taking_medication", "transformed_dose"] <- 0

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
# Simple cross-sectional model

full_formula <-
  NPI_apathy_present ~ 1 +
  first_session_date +
  sex + education + age_at_diagnosis +
  (1 | subject_id) +
  years_since_diagnosis + transformed_dose + taking_antidepressants +
  UPDRS_motor_score + MoCA + HADS_depression + HADS_anxiety
  # Can't include session_date as advances at the same rate as years since diagnosis etc

prior <- brms::set_prior("normal(0.0, 1.0)", class = "b")

model <- brms::brm_multiple(
  formula = full_formula,
  family = brms::bernoulli(link = "logit"),
  prior = prior,
  data = imputed_data,
  chains = 8, iter = 5000,
  silent = TRUE, refresh = 0
)
summary(model)

# Save to file
filename <- file.path(
  "..", "Results", paste("logistic-regression_", date_string, ".Rout", sep = "")
)
sink(file = filename)
options(width = 1024)
print(summary(model))
cat("\n")
cat("Odds ratios:\n")
print(exp(fixef(model)[,-2])) # Remove Est.Error column: exp(error(...)) =/= error(exp(...))
cat("\n\n\n")
sessionInfo()
sink()
options(width = 80)
file.show(filename)

# Look at the predictive performance of the model
# Note that this does not account for the repeated sessions per subject, nor is
# it cross-validated. Interpret with extreme caution!
pred <- predict(model)
caret::confusionMatrix(
  as.factor(pred[,'Estimate'] > 0.5),
  as.factor(mice::complete(imputed_data, action = 1)$NPI_apathy_present)
)
proc <- pROC::roc(
  mice::complete(imputed_data, action = 1)$NPI_apathy_present,
  pred[,'Estimate']
)
plot(proc)

# Plot odds ratios
plt <- fixef(model) %>%
  exp() %>%  # Odds ratio := exp(beta)
  as_tibble(rownames = "covariate") %>%
  filter(covariate != "Intercept") %>%
  mutate(confound = str_detect(covariate, "session_date")) %>%
  mutate(covariate = recode(covariate, !!!variable_names)) %>%
  mutate(covariate = factor(covariate, levels = rev(covariate))) %>%
  ggplot(aes(
    x = covariate, y = Estimate, ymin = Q2.5, ymax = Q97.5, colour = confound
  )) +
  geom_pointrange() +
  geom_vline(xintercept = 4.5, colour = "grey92") +  # https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
  geom_vline(xintercept = 7.5, colour = "grey92") +
  geom_vline(xintercept = 10.5, colour = "grey92") +
  geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
  scale_y_continuous(trans = "log", breaks = c(0.25, 0.5, 1.0, 2.0), limits = c(NA, 2.1)) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_colour_manual(values = c("black", "grey"), guide = "none") +
  xlab(NULL) +
  ylab("Odds ratio (95% CI)") +
  theme_bw() +  # use a white background
  # https://stackoverflow.com/a/8992102
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_blank()
  )

print(plt)
save_plot(plt, "logistic-regression")

# -----------------------------------------------------------------------------
# Repeat but predicting dose

full_formula <-
  transformed_dose ~ 1 +
  first_session_date +
  sex + education + age_at_diagnosis +
  (1 | subject_id) +
  years_since_diagnosis + taking_antidepressants +
  UPDRS_motor_score + MoCA + HADS_depression + HADS_anxiety

prior <- brms::set_prior("normal(0.0, 1.0)", class = "b")

model <- brms::brm_multiple(
  formula = full_formula,
  family = gaussian(link = "identity"),
  prior = prior,
  data = imputed_data,
  chains = 8, iter = 5000,
  silent = TRUE, refresh = 0
)
summary(model)

# Save to file
filename <- file.path(
  "..", "Results", paste("dose-regression_", date_string, ".Rout", sep = "")
)
sink(file = filename)
options(width = 1024)
print(summary(model))
cat("\n\n\n")
sessionInfo()
sink()
options(width = 80)
file.show(filename)

# Plot coefficients
plt <- fixef(model) %>%
  as_tibble(rownames = "covariate") %>%
  filter(covariate != "Intercept") %>%
  mutate(confound = str_detect(covariate, "session_date")) %>%
  mutate(covariate = recode(covariate, !!!variable_names)) %>%
  mutate(covariate = factor(covariate, levels = rev(covariate))) %>%
  ggplot(aes(
    x = covariate, y = Estimate, ymin = Q2.5, ymax = Q97.5, colour = confound
  )) +
  geom_pointrange() +
  geom_vline(xintercept = 4.5, colour = "grey92") +  # https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
  geom_vline(xintercept = 6.5, colour = "grey92") +
  geom_vline(xintercept = 9.5, colour = "grey92") +
  geom_hline(yintercept = 0, linetype = "dashed") +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_colour_manual(values = c("black", "grey"), guide = "none") +
  xlab(NULL) +
  ylab("Regression coefficients (95% CI)") +
  theme_bw() +  # use a white background
  # https://stackoverflow.com/a/8992102
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_blank()
  )

print(plt)
save_plot(plt, "dose-regression")

###############################################################################
