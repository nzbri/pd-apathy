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
  file.path("..", "Data", "raw-data_2021-08-17.rds")
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
# Data summary tables
# http://www.danieldsjoberg.com/gtsummary/

# Oh seriously :-(
gtsummary_names <- mapply(
    {function(item, name) as.formula(paste(name, " ~ \"", item, "\"", sep = ""))},
    variable_names,
    names(variable_names)
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
  gtsummary::add_p() %>%  # test for a difference between groups
  gtsummary::bold_labels() %>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  gtsummary::modify_spanning_header(
    c("stat_1", "stat_2", "stat_3") ~ "**Apathetic**"
  )
print(session_tbl)

# Save as Word doc
# Often needs some postprocessing (extra borders in particular)
# This is not easy via `as_flex_table()`
session_tbl %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(
    path = "../Results/session_summary.docx",
    pr_section = officer::prop_section(officer::page_size(orient = "landscape"))
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
    age = NULL
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
    first_session_date2 = scale(first_session_date) ^ 2
  ) %>%
  # Center & rescale
  mutate(across(
    c(
      education,
      session_date, session_date2, first_session_date, first_session_date2,
      MoCA, HADS_depression, HADS_anxiety, UPDRS_motor_score
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
    taking_medication = relevel(taking_medication, ref = "No"),
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
# Simple cross-sectional model

full_formula <-
  NPI_apathy_present ~ 1 +
  first_session_date + first_session_date2 +
  sex + education + age_at_diagnosis +  # ethnicity
  (1 | subject_id) +
  years_since_diagnosis + taking_medication + transformed_dose + taking_antidepressants +
  UPDRS_motor_score + MoCA + HADS_depression + HADS_anxiety
  # global_z
  # attention_domain + executive_domain + language_domain + learning_memory_domain + visuo_domain
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
cat("\n\n\n")
sessionInfo()
sink()
options(width = 80)
file.show(filename)

# Plot odds ratios
# TODO:
#  + Rescale by stddev?
#  + Flip to standard higher is better?
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
  geom_vline(xintercept = 11.5, colour = "grey92") +
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

###############################################################################
# Regularised GLM for individual tests

# Some code snippets marked with `## nptest` need to be enabled during
# preprocessing

# Note that confidence intervals are not really meaningful here
# https://cran.r-project.org/web/packages/penalized/vignettes/penalized.pdf
# What we do instead is run multiple times on each individual imputation and
# collate the results post hoc

# Furthermore, `glmnet` cannot deal with repeated measures (i.e. multiple
# sessions per subject): would be overconfident and give biased
# cross-validation. Therefore we generate multiple datasets and randomly
# subsample down to one session per subject.

# Alternative is `brms` with lasso / horseshoe prior (could then only apply
# shrinkage to `nptest` scores)
# https://betanalpha.github.io/assets/case_studies/modeling_sparsity.html
# This is a complete nightmare, see below :-(

# -----------------------------------------------------------------------------
# Function to do the model fitting

fit_regularised_model <- function(data) {

  predictors <- data %>%
    # `glmnet` just runs on all variables rather than requiring a formula
    select(
      first_session_date, first_session_date2,
      sex, education, age_at_diagnosis,
      years_since_diagnosis, taking_medication, transformed_dose, taking_antidepressants,
      UPDRS_motor_score, HADS_depression, HADS_anxiety, starts_with("nptest_")
    ) %>%
    #mutate(across(where(is.character), as.factor)) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()

  response <- data %>%
    select(NPI_apathy_present) %>%
    as.matrix()

  cvfit <- glmnet::cv.glmnet(predictors, response, family = "binomial")
  return(cvfit)
}

# -----------------------------------------------------------------------------
# Function to resample to one observation per subject

subsample_sessions <- function(data) {
  data <- data %>%
    group_by(subject_id) %>%
    slice_sample(n = 1) %>%
    ungroup()
  return(data)
}

# -----------------------------------------------------------------------------
# Function to repeatedly subsample data and return fits

repeated_fits <- function(data, n_resamplings) {
  cvfits = list()
  for (i in seq(1, n_resamplings)) {
    cvfits[[i]] <- data %>%
      subsample_sessions() %>%
      fit_regularised_model()
  }
  return(cvfits)
}

# -----------------------------------------------------------------------------
# Illustrative example on single dataset

cvfit <- mice::complete(imputed_data, action = 1) %>%
  subsample_sessions() %>%
  fit_regularised_model()
print(cvfit)
plot(cvfit)
coef(cvfit, s = "lambda.min")
plt <- coef(cvfit, s = "lambda.min") %>%  # lambda.1se
  as.matrix() %>%
  exp() %>%  # Odds ratio := exp(beta)
  as_tibble(rownames = "covariate") %>%
  filter(covariate != "(Intercept)") %>%
  mutate(covariate = factor(covariate, levels = rev(covariate))) %>%
  ggplot(aes(x = covariate, y = s1)) +
  geom_point() +
  #geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
  scale_y_continuous(trans = "log") +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(NULL) +
  ylab("Odds ratio (95% CI)") +
  theme_bw()  # use a white background
print(plt)

# -----------------------------------------------------------------------------
# Full run on multiple datasets

# Generate all the fits
cvfits <- lapply(
    mice::complete(imputed_data, action = "all"),
    {function(data) repeated_fits(data, n_resamplings = 100)}
  )
# Flatten to single list
cvfits <- do.call(list, unlist(cvfits, recursive = FALSE))

# Combine coefficients
coefs <- cvfits %>%
  lapply({function(cvfit) coef(cvfit, s = "lambda.min")}) %>%
  lapply(as.matrix) %>%
  {function(mat_list) do.call(cbind, mat_list)}() %>%
  as_tibble(rownames = "covariate", .name_repair = "unique") %>% #View()
  mutate(covariate = factor(covariate, levels = covariate)) %>%  # Needed to maintain ordering
  # https://stackoverflow.com/a/69052207
  pivot_longer(cols = !covariate) %>%
  mutate(value = na_if(value, 0.0)) %>%  # Useful for statistics conditional on selection
  group_by(covariate) %>%
  summarise(
    # Proportions non-zero
    p_pos = sum(value > 0.0, na.rm = TRUE) / n(),
    p_neg = sum(value < 0.0, na.rm = TRUE) / n(),
    # Summary stats / confidence intervals for selected coefs
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    lower = quantile(value, 0.025, na.rm = TRUE),
    upper = quantile(value, 0.975, na.rm = TRUE)
  ) #%>% print(n = Inf)
  #mutate(lower = mean - 2 * sd, upper = mean + 2 * sd)

# Plots of selection frequency
plt <- coefs %>%
  filter(covariate != "(Intercept)") %>%
  select(covariate, p_pos, p_neg) %>%
  mutate(p_neg = -1.0 * p_neg) %>%
  pivot_longer(!covariate) %>%
  mutate(covariate = recode(covariate, !!!variable_names)) %>%
  ggplot(aes(x = covariate, y = value, fill = name)) +
  geom_col() +
  # Split into predefined domains
  geom_vline(xintercept = 24.5, colour = "grey92") +  # https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
  geom_vline(xintercept = 18.5, colour = "grey92") +
  geom_vline(xintercept = 12.5, colour = "grey92") +
  geom_vline(xintercept = 8.5, colour = "grey92") +
  geom_vline(xintercept = 3.5, colour = "grey92") +
  scale_x_discrete(limits = rev) +
  scale_fill_discrete(limits = c("p_pos", "p_neg")) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(NULL) +
  ylab("Proportion of non-zero coefficients (split positive / negative)") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )
print(plt)
save_plot(plt, "regularised-regression_proportions", width = 6.0, height = 6.0)

# And plot along with resampling uncertainty
plt <- coefs %>%
  mutate(across(c(lower, mean, upper), exp)) %>%  # Odds ratio := exp(beta)
  filter(covariate != "(Intercept)") %>%
  arrange(covariate) %>%
  mutate(covariate = recode(covariate, !!!variable_names)) %>%
  ggplot(aes(
    x = covariate, y = mean, ymin = lower, ymax = upper
  )) +
  geom_pointrange() +  # fatten = 1.5
  # Split into predefined domains
  geom_vline(xintercept = 24.5, colour = "grey92") +  # https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
  geom_vline(xintercept = 18.5, colour = "grey92") +
  geom_vline(xintercept = 12.5, colour = "grey92") +
  geom_vline(xintercept = 8.5, colour = "grey92") +
  geom_vline(xintercept = 3.5, colour = "grey92") +
  geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
  scale_x_discrete(limits = rev) +
  scale_y_continuous(trans = "log", breaks = seq(0.5, 1.5, 0.25)) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(NULL) +
  ylab("Odds ratio (95% CI) for non-zero coefficients") +
  theme_bw() +  # use a white background
  # https://stackoverflow.com/a/8992102
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_blank()
  )
print(plt)
save_plot(plt, "regularised-regression_coefs", width = 6.0, height = 6.0)

# -----------------------------------------------------------------------------

# # Full brms version of this is a nightmare
# base_formula <-
#   nla ~ 1 +
#   first_session_date + first_session_date2 +
#   sex + education + age_at_diagnosis
#   (1 | subject_id) +
#   years_since_diagnosis + taking_medication + transformed_dose + taking_antidepressants +
#   UPDRS_motor_score + HADS_depression + HADS_anxiety + MoCA
#
# # https://discourse.mc-stan.org/t/horseshoe-prior-on-subset-of-predictors/8140/3
# nptest_formula <- full_data %>%
#   select(starts_with("nptest_")) %>%
#   colnames() %>%
#   (function(combination) {
#     paste("nlb ~ 0 +", paste(combination, collapse = " + "))
#   })() %>%
#   as.formula()
#
# full_formula <- bf(NPI_apathy_present ~ nla + nlb, nl = TRUE) +
#   lf(base_formula, center = TRUE) +
#   lf(nptest_formula, center = TRUE)
#
# prior <-
#   brms::set_prior("normal(0.0, 1.0)", class = "b", nlpar = "nla") +
#   brms::set_prior("horseshoe(3)", class = "b", nlpar = "nlb")

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
# Predictive model

# -----------------------------------------------------------------------------
# Use `VIM` to do single imputation
# https://cran.r-project.org/web/views/MissingData.html#single
# Not straightforward to combine multiple predictive models together

simputed_data <- transformed_data %>%
  # Fill within subject
  group_by(subject_id) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  # Then across subjects
  select(-subject_id, -session_id, -age_at_death) %>%
  VIM::kNN(
    weights = "auto",
    imp_var = FALSE
  )
# Combine back together with variables not needed / imputed
simputed_data <- transformed_data %>%
  select(subject_id, session_id, age_at_death) %>%
  bind_cols(simputed_data) %>%
  group_by(subject_id) %>%
  mutate(
    across(c(sex, ethnicity, handedness, side_of_onset), ~ mode(.x))
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# Function to do the data preprocessing and model fitting

# vignette("msm-manual")

fit_predictive_model <- function(data) {

  # Sessions encoding when subjects died
  # As these are the last session by definition, no covariates are needed
  death_proxy_sessions <- data %>%
    filter(!is.na(age_at_death)) %>%
    select(subject_id, age_at_death, age_at_diagnosis) %>%
    # Just take one session per subject
    group_by(subject_id) %>%
    distinct(subject_id, .keep_all = TRUE) %>%
    ungroup() %>%
    # Make key variables
    mutate(
      session_id = paste(subject_id, "death", sep = "_"),
      years_since_diagnosis = (age_at_death - age_at_diagnosis) + 0.01,
      # Fudge factor above breaks ties in case sessions rounded to same month
      status = "dead"
    )

  # Now combine together and calculate apathy status
  data <- data %>%
    # Have subjects had previous apathy?
    group_by(subject_id) %>%  # within each subject:
    arrange(session_date) %>%  # order by session
    mutate(
      ever_apathetic = as.logical(cummax(NPI_apathy_present)),
    ) %>%
    ungroup() %>%
    # Combine with dummy sessions
    full_join(death_proxy_sessions) %>%
    arrange(subject_id, session_id) %>%
    mutate(
      # Predict current apathy
      #status = if_else(is.na(status), as.character(NPI_apathy_present), status),
      # Predict apathy onset
      status = if_else(is.na(status), as.character(ever_apathetic), status),
      status = ordered(
        status,
        labels = c("A-", "A+", "dead"),
        levels = c("FALSE", "TRUE", "dead")
      ),
      state = as.numeric(status)
    ) %>%
  filter(years_since_diagnosis > 0.0)
  #select(subject_id, session_date, years_since_diagnosis, age, age_at_death, NPI_apathy_present) %>%
  #View()

  # Number of sessions
  print(msm::statetable.msm(state, subject_id, data = data))
  # Number of subjects
  data %>%
    group_by(subject_id) %>%
    filter(n() > 1) %>%
    slice_head() %>%
    ungroup() %>%
    nrow() %>%
    print()

  Q.mask <- rbind(
    c( NA, 1.0, 1.0),  # A-
    c(0.0,  NA, 1.0),  # A+
    c(0.0, 0.0,  NA)   # dead
  )
  Q.init <- msm::crudeinits.msm(
    state ~ years_since_diagnosis,
    #state ~ years_since_first_session,
    subject = subject_id,
    data = data,
    qmatrix = Q.mask
  )

  # Here, we tie transition 2 (A- -> death) to 3 (A+ -> death)
  # Matches Q.mask, ordered along rows then down columns
  # + 1 2
  # + + 3
  # + + +
  constrained <- c(1,2,2)

  # Fit model!
  mfit <- msm::msm(
    state ~ years_since_diagnosis,
    #state ~ years_since_first_session,
    subject = subject_id,
    data = data,
    qmatrix = Q.init,
    deathexact = 3,  # I.e. state 3 (death) is at a known time, rather than just between visits
    covariates =
      ~ first_session_date + first_session_date2 +
      sex + education + age_at_diagnosis +
      taking_medication + transformed_dose + taking_antidepressants +
      UPDRS_motor_score + MoCA + HADS_depression + HADS_anxiety,  #transformed_dose:MoCA
    constraint = list(
      first_session_date = constrained, first_session_date2 = constrained,
      sexFemale = constrained, education = constrained, age_at_diagnosis = constrained,
      taking_medicationYes = constrained, transformed_dose = constrained,
      UPDRS_motor_score = constrained
    )
  )

  return(mfit)
}

# -----------------------------------------------------------------------------
# Fit the model and generate key summaries

#mfits <- lapply(mice::complete(imputed_data, action = "all"), fit_predictive_model)
mfit <- fit_predictive_model(simputed_data)
print(mfit)
#summary(mfit)
#msm::hazard.msm(mfit)

# Raw transition matrix
msm::qmatrix.msm(mfit, covariates = "mean")
msm::pmatrix.msm(mfit, t = 0.2, covariates = "mean")  # 2 year projection (remember factor of 10!)

# Summaries thereof
msm::pnext.msm(  # Probability of next state
  mfit, covariates = "mean" #covariates = list(sex = "Male", taking_medication = "Yes", global_z = 0.0)
)
#msm::totlos.msm(mfit, start = 1, covariates = "mean")  # Time spent in each state
#msm::envisits.msm(mfit, start = 1, covariates = "mean")  # Number of visits
msm::efpt.msm(mfit, tostate = 3, covariates = "mean", ci = "normal")  # Time to death
#msm::sojourn.msm(mfit)

# Random plotting functions
msm::plot.msm(mfit, range = c(0.01, 3.0))  # Remember factor of 10!
msm::plot.prevalence.msm(
  mfit, mintime = 0.0, maxtime = 2.5, initstates = c(1.0, 0.0, 0.0),
  xlab = "Decades since diagnosis"
)
msm::plot.survfit.msm(mfit, from = 1, to = 3, range = c(0.01, 3.0))
msm::plot.survfit.msm(mfit, from = 2, to = 3, range = c(0.01, 3.0))

# Plot difference in baseline hazard for death
plt <- msm::qmatrix.msm(mfit, covariates = "mean") %>%
  # Aaargh! Need to turn the Qmatrix structure into some kind of tibble
  unlist(recursive = FALSE) %>%
  as.list() %>%
  as.tibble() %>%
  # End up with e.g. `estimate7`
  #    S1 S2 S3
  # S1  1  4  7
  # S2  2  5  8
  # S3  3  6  9
  select(ends_with("7") | ends_with("8")) %>%
  pivot_longer(everything()) %>%
  mutate(
    group = str_sub(name, start = -1),
    #group = recode(group, "7" = "A- -> death", "8" = "A+ -> death"),
    group = recode(group, "7" = "No apathy", "8" = "Apathetic"),
    name = str_sub(name, end = -2),
  ) %>%
  pivot_wider(names_from = name) %>%
  # And plot
  ggplot(aes(x = group, y = estimates, ymin = L, ymax = U, fill = group)) +
  geom_col() +
  geom_errorbar(width = 0.2, size = 0.75) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E"), guide = "none") +
  labs(x = NULL, y = "Baseline hazard for mortality") +
  theme_light() +
  theme(axis.text = element_text(size = rel(1.0)))
print(plt)
save_plot(plt, "msm-predictive_baseline-hazard", width = 3.0, height = 4.0)

# -----------------------------------------------------------------------------
# Plot hazard ratios

for (transition in list(
  list(states = "State 1 - State 2", label = "A- to A+", name = "1-2", constraint = FALSE),
  list(states = "State 1 - State 3", label = "A- to death", name = "1-3", constraint = TRUE),
  list(states = "State 2 - State 3", label = "A+ to death", name = "2-3", constraint = TRUE)
)) {

  plt <- msm::hazard.msm(mfit) %>%
    lapply("[", transition$states, ) %>%
    bind_rows(.id = "covariate") %>%
    mutate(covariate = factor(covariate, levels = rev(covariate))) %>%
    mutate(confound = str_detect(covariate, "session_date")) %>%
    mutate(
      constrained = (covariate %in% c(
        "sexFemale", "education", "age_at_diagnosis", "taking_medicationYes",
        "transformed_dose", "UPDRS_motor_score"
      )),
      constrained = constrained & (transition$constraint),
    ) %>%
    # Colours for (1) normal (2) constrained and (3) confound variables
    mutate(
      colour = if_else(
        confound, "confound", if_else(constrained, "constrained", "free")
      ),
      colour = factor(colour, levels = c("confound", "constrained", "free"))
    ) %>%
    mutate(covariate = recode(covariate, !!!variable_names)) %>%
    # Plot itself
    # https://stackoverflow.com/a/38064297
    ggplot(aes(x = covariate, y = HR, ymin = L, ymax = U, colour = colour)) +
    geom_pointrange() +
    geom_vline(xintercept = 4.5, colour = "grey92") +  # https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
    geom_vline(xintercept = 7.5, colour = "grey92") +
    geom_vline(xintercept = 10.5, colour = "grey92") +
    geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
    scale_y_continuous(trans = "log", breaks = c(0.33, 1.0, 3.0), limits = c(0.2, 5.0)) +
    coord_flip() +  # flip coordinates (puts labels on y axis)
    scale_colour_manual(values = c("grey", "royalblue3", "black"), drop = FALSE, guide = "none") +
    labs(x = NULL, y = "Hazard ratio (95% CI)", title = transition$label) +
    theme_bw() +  # use a white background
    # https://stackoverflow.com/a/8992102
    theme( # remove the vertical grid lines
      panel.grid.major.y = element_blank()
    )
  print(plt)
  save_plot(plt, paste("msm-predictive_coefs-", transition$name, sep = ""))
}

# -----------------------------------------------------------------------------
# Plot predicted progression

# Function to pull out probability of apathy at a given time, with a set of covariates
p_apathy <- function(
  years_since_diagnosis, MoCA = 0.0, UPDRS_motor_score = 0.0, transformed_dose = 0.0
) {
  pmat = msm::pmatrix.msm(
    mfit,
    t = years_since_diagnosis / 10.0,
    covariates = list(
      sex = "Male", taking_medication = "Yes", taking_antidepressants = "No",
      MoCA = MoCA,
      UPDRS_motor_score = UPDRS_motor_score,
      transformed_dose = transformed_dose
    )
  )
  return(
    # p(A- -> A+ | alive)
    #pmat[["State 1", "State 2"]] / (pmat[["State 1", "State 1"]] + pmat[["State 1", "State 2"]])
    # p(A- -> A+, alive)
    pmat[["State 1", "State 2"]]
  )
}

for (variable in list(
  list(name = "MoCA", values = seq(-2.0, 2.0, 0.25)),
  list(name = "UPDRS_motor_score", values = seq(-2.0, 2.0, 0.25)),
  list(name = "transformed_dose", values = seq(-2.0, 2.0, 0.25))
)) {

  plt <- expand_grid(
      years_since_diagnosis = seq(0.0, 20.0, 0.1), values = variable$values
    ) %>%
    mutate(p_apathy = map2_dbl(
      years_since_diagnosis, values,
      function(t,x) {do.call(p_apathy, setNames(list(t,x), c("years_since_diagnosis", variable$name)))}
      #function(t,x) {p_apathy(t, MoCA = x)}
    )) %>%
    ggplot(aes(x = years_since_diagnosis, y = p_apathy, group = values, colour = values)) +
    geom_line() +
    # scale_y_continuous(limits = c(0.0, 0.55)) +
    scale_colour_viridis_c() +
    theme_bw() +
    labs(
      x = "Years since diagnosis",
      #y = "p(apathetic | alive)",
      y = "p(apathetic, alive)",
      colour = NULL,
      title = variable_names[[variable$name]]
    )
  print(plt)
  save_plot(plt, paste("msm-predictive_trajectories-", variable$name, sep = ""))
}

###############################################################################
