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
  HADS_anxiety = "Anxiety (HADS)"
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
  #filter(!is.na(global_z)) %>%
  # Null implausible values
  #mutate(LED = replace(LED, LED > 5000, NA))

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
    #starts_with("nptest_"),
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
full_data %>%
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
print(summary(model))

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
  geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
  geom_vline(xintercept = 4.5, colour = "grey92") +  # https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
  geom_vline(xintercept = 7.5, colour = "grey92") +
  geom_vline(xintercept = 11.5, colour = "grey92") +
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
# Predictive model

#install.packages("VIM")

imputed_data2 <- transformed_data %>%
  group_by(subject_id) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  select(-subject_id, -session_id, -age_at_death) %>%
  VIM::kNN(
    weights = "auto",
    imp_var = FALSE
  )
imputed_data2 <- transformed_data %>%
  select(subject_id, session_id, age_at_death) %>%
  bind_cols(imputed_data2) %>%
  group_by(subject_id) %>%
  mutate(
    across(c(sex, ethnicity, handedness, side_of_onset), ~ mode(.x))
  ) %>%
  ungroup()

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
      years_since_diagnosis = 0.01 + age_at_death - (full_data.sd$age_at_diagnosis * age_at_diagnosis + full_data.mean$age_at_diagnosis),
      years_since_diagnosis = (years_since_diagnosis - full_data.mean$years_since_diagnosis) / full_data.sd$years_since_diagnosis,
      status = "dead"
    )

  # Now combine together
  data <- data %>%
    group_by(subject_id) %>%  # within each subject:
    arrange(session_date) %>%  # order by session
    mutate(
      ever_apathetic = as.logical(cummax(NPI_apathy_present)),
    ) %>%
    ungroup() %>%
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
    ) #%>%
  #filter(years_since_diagnosis > 0.0)
  #select(subject_id, session_date, years_since_diagnosis, age, age_at_death, NPI_apathy_present) %>%
  #View()

  #msm::statetable.msm(state, subject_id, data = data)

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

  constrained <- c(1,2,2)
  mfit <- msm::msm(
    state ~ years_since_diagnosis,
    #state ~ years_since_first_session,
    subject = subject_id,
    data = data,
    qmatrix = Q.init,
    deathexact = 3,
    covariates =
      ~ first_session_date + first_session_date2 #+ session_date + session_date2
      + sex + education + age_at_diagnosis
      + taking_medication + transformed_dose + taking_antidepressants + UPDRS_motor_score
      + HADS_depression + HADS_anxiety + global_z, #+ cog_delta + ever_apathetic
      #+ attention_domain + executive_domain + language_domain + learning_memory_domain + visuo_domain,
    constraint = list(
      first_session_date = constrained, first_session_date2 = constrained,
      sexFemale = constrained, education = constrained, age_at_diagnosis = constrained,
      taking_medicationNo = constrained, transformed_dose = constrained,
      UPDRS_motor_score = constrained
    )
  )
  #print(mfit)
  #summary(mfit)

  return(mfit)
}

#mfits <- lapply(mice::complete(imputed_data, action = "all"), fit_predictive_model)
mfit <- fit_predictive_model(imputed_data2)

#for (mfit in mfits) {
for (transition in list(
  list(states = "State 1 - State 2", label = "A- to A+"),
  list(states = "State 1 - State 3", label = "A- to death"),
  list(states = "State 2 - State 3", label = "A+ to death")
)) {
  plt <- msm::hazard.msm(mfit) %>%
    lapply("[", transition$states, ) %>%
    bind_rows(.id = "covariate") %>%
    mutate(covariate = factor(covariate, levels = rev(covariate))) %>%
    # https://stackoverflow.com/a/38064297
    ggplot(aes(x = covariate, y = HR, ymin = L, ymax = U)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
    scale_y_continuous(trans = "log", breaks = c(0.33, 1.0, 3.0), limits = c(0.2, 5.0)) +
    coord_flip() +  # flip coordinates (puts labels on y axis)
    labs(x = NULL, y = "Hazard ratio (95% CI)", title = transition$label) +
    theme_bw()  # use a white background
  print(plt)
}

###############################################################################
