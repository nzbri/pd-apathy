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
  file.path("..", "Data", "raw-data_2021-02-15.rds")
)

###############################################################################
# Additional exclusion criteria etc

full_data <- full_data %>%
  # Drop sessions missing apapthy measure
  drop_na(NPI_apathy_present) %>%  # NPI_apathy_present, HADS_anxiety, HADS_depression
  # Null implausible values
  mutate(LED = replace(LED, LED > 5000, NA))

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
    # Cognitive scores (and subdomains)
    global_z, MoCA, WTAR,
    attention_domain, executive_domain, language_domain,
    learning_memory_domain, visuo_domain,
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
method["age_at_death"] <- ""
pred = mice::make.predictorMatrix(imputed_data)  # target = rows, predictors = columns
# Remove database specific variables
pred[, c("subject_id", "subject_int", "session_id")] <- 0
pred[, c("age_at_death")] <- 0
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
  mice::mice(
    m = 1, maxit = 10,
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
# Data preprocessing / transformations

transformed_data <- imputed_data %>%
  mice::complete(action = "long", include = FALSE) %>%
  as_tibble() %>%
  mutate(.id = NULL) %>%
  # Add some useful extra timing info
  mutate(
    years_since_diagnosis = age - age_at_diagnosis,
    years_since_first_session = years_between(first_session_date, session_date)  
  )

# Store means / standard deviations for later
transformed_data.mean <- transformed_data %>%
  group_by(.imp) %>%
  summarise(across(
    where(is.numeric) | where(is.logical),
    ~ mean(.x, na.rm = TRUE)
  ))
transformed_data.sd <- transformed_data %>%
  group_by(.imp) %>%
  summarise(across(
    where(is.numeric) | where(is.logical),
    ~ sd(.x, na.rm = TRUE)
  ))

# Center / rescale selected columns
transformed_data <- transformed_data %>%
  mutate(first_session_date2 = scale(first_session_date) ^ 2) %>%
  # Center & rescale
  group_by(.imp) %>%
  mutate(across(
    c(education, MoCA, first_session_date, first_session_date2),  # age_at_diagnosis, 
    ~ as.vector(scale(.x, center = TRUE, scale = TRUE))
  )) %>%
  ungroup() %>%
  # Rescale only
  group_by(.imp) %>%
  mutate(across(
    c(HADS_depression, UPDRS_motor_score),  # years_since_diagnosis, 
    ~ as.vector(scale(.x, center = FALSE, scale = TRUE))
  )) %>%
  ungroup() %>%
  # Years
  mutate(
    age = (age - 70.0) / 10.0,
    age_at_diagnosis = (age_at_diagnosis - 70.0) / 10.0,
    age_at_death = (age_at_death - 70.0) / 10.0,
    years_since_diagnosis = years_since_diagnosis / 10.0,
    years_since_first_session = years_since_first_session / 10.0
  ) %>%
  # Split LED into two regressors, and rescale
  mutate(
    taking_medication = (LED > 0.0),
    transformed_dose = sqrt(LED)
  ) %>%
  group_by(.imp, taking_medication) %>%
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
# Descriptive statistics

# Pull out up to first apathetic session only
transformed_data <- transformed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  mutate(apathy_ever = max(as.integer(NPI_apathy_present))) %>%
  slice_head() %>%
  filter(NPI_apathy_present == FALSE) %>%
  ungroup() %>%
  arrange(subject_id, session_date)

for (variable in c(
  "first_session_date",
  #"first_session_date2",
  "years_since_diagnosis",
  "sex",
  "education",
  "age_at_diagnosis",
  "global_z",
  "MoCA",
  "UPDRS_motor_score",
  "HADS_depression",
  "LED"
)) {
  print(wilcox.test(
    as.formula(paste("as.numeric(", variable, ") ~ apathy_ever")),
    data = transformed_data,
    paired = FALSE
  ))
}

###############################################################################
# Survival analysis
vignette("survival")
vignette("timedep")
vignette("compete")
# https://stats.stackexchange.com/questions/247959/interval-censored-survival-analysis-with-time-dependent-covariates

# TODO:
# Proper MICE multiple analyses
# Multistate apathy/death model

# Pull out up to first apathetic session only
transformed_data <- transformed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  # Want to predict next apathy status
  mutate(next_apathy = lead(NPI_apathy_present)) %>%
  #filter(!is.na(next_apathy)) %>%
  # Recode as (time1, time2) for survival analysis
  mutate(
    time1 = years_since_diagnosis,
    time2 = lead(years_since_diagnosis)
    #time1 = replace_na(lag(years_since_diagnosis), 0.0),
    #time2 = years_since_diagnosis
  ) %>%
  filter(!is.na(time2) & !is.na(next_apathy) & ((time2 - time1) > 0.0)) %>%
  # Remove all sessions after (and including) first apathy presence
  mutate(apathy_present.worst_to_date = cummax(NPI_apathy_present)) %>%
  filter(apathy_present.worst_to_date == FALSE) %>%
  ungroup() %>%
  arrange(subject_id, session_date)

# Predict death
transformed_data <- transformed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  #filter(!is.na(next_apathy)) %>%
  # Recode as (time1, time2) for survival analysis
  mutate(
    time1 = years_since_diagnosis,
    time2 = lead(years_since_diagnosis),
    dead = (!is.na(age_at_death) & is.na(time2)),
    time2 = if_else(is.na(time2), age_at_death - age_at_diagnosis, time2)
  ) %>%
  filter(!is.na(time2) & ((time2 - time1) > 0.001)) %>%
  ungroup() %>%
  arrange(subject_id, session_date)

# Recode as (time1, time2) for survival analysis
# transformed_data <- transformed_data %>%
#   filter(years_since_diagnosis > 0.0) %>%
#   group_by(subject_id) %>%  # within each subject:
#   arrange(session_date) %>%  # order by session
#   mutate(
#     time1 = years_since_diagnosis,
#     time2 = lead(years_since_diagnosis)
#     #time1 = replace_na(lag(years_since_diagnosis), 0.0),
#     #time2 = years_since_diagnosis
#   ) %>%
#   ungroup() %>%
#   filter(!is.na(time2) & ((time2 - time1) > 0.0)) %>%
#   arrange(subject_id, session_date)


strata <- survival::strata
vfit <- survival::survfit(
  survival::Surv(time1, time2, NPI_apathy_present) ~ 1 + strata(sex),
  data = transformed_data,
  id = subject_id
)
summary(vfit)
plot(vfit, xlab = "Years since diagnosis", ylab = "Survival")

vfit <- survival::coxph(
  survival::Surv(time1, time2, next_apathy)  # dead
    ~ 1 + sex + age_at_diagnosis + education
    + UPDRS_motor_score + taking_medication + transformed_dose
    + HADS_depression #+ HADS_anxiety + NPI_apathy_present
    #+ MoCA,
    + global_z,
    #+ attention_domain + executive_domain + language_domain + learning_memory_domain + visuo_domain,
  data = transformed_data,
  id = subject_id
)
summary(vfit)
anova(vfit)
car::Anova(vfit, test.statistic = "LR")
drop1(vfit, test = "Chisq")
# https://github.com/kassambara/survminer/issues/440
survminer::ggsurvplot(
  survival::survfit(vfit, data = transformed_data),
  data = transformed_data,
  #fun = "cumhaz"
  xscale = 1.0 / 10.0,
  xlab = "Years since diagnosis",
  censor.shape = "|",
  conf.int = TRUE,
  surv.median.line = "hv",
  risk.table = TRUE,
  #cumevents = TRUE,
  #cumcensor = TRUE,
  #ncensor.plot = TRUE
)
# https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
survminer::ggcoxzph(survival::cox.zph(vfit))
survminer::ggcoxdiagnostics(vfit, type = "martingale", ox.scale = "linear.predictions")
# ggforest(vfit)
# ggadjustedcurves(vfit)

fit <- survival::survfit(vfit, data = transformed_data)
data.frame(
  time = fit$time,
  n.risk = fit$n.risk,
  n.event = fit$n.event,
  n.censor = fit$n.censor,
  surv = fit$surv,
  upper = fit$upper,
  lower = fit$lower
) %>% as_tibble() %>% print(n = Inf)

###############################################################################
vignette("msm-manual")

# Recode in terms of next status (including death)
# transformed_data <- transformed_data %>%
#   group_by(subject_id) %>%  # within each subject:
#   arrange(session_date) %>%  # order by session
#   #filter(!is.na(next_apathy)) %>%
#   # Recode as (time1, time2) for survival analysis
#   mutate(
#     next_status = lead(NPI_apathy_present),
#     #next_status = factor(next_status, labels = c("A-", "A+"), levels = c(FALSE, TRUE)),
#     time1 = years_since_diagnosis,
#     time2 = lead(years_since_diagnosis),
#     dead = (!is.na(age_at_death) & is.na(time2)),
#     next_status = if_else(dead, "dead", as.character(next_status)),
#     next_status = ordered(
#       next_status,
#       labels = c("A-", "A+", "dead"),
#       levels = c("FALSE", "TRUE", "dead")
#     ),
#     time2 = if_else(is.na(time2), age_at_death - age_at_diagnosis, time2)
#   ) %>%
#   #filter(!is.na(time2) & ((time2 - time1) > 0.001)) %>%
#   ungroup() %>%
#   arrange(subject_id, session_date) %>%
#   select(subject_id, session_date, years_since_diagnosis, age, age_at_death, NPI_apathy_present, next_status, time1, time2) %>%
#   View()

death_proxy_sessions <- transformed_data %>%
  filter(!is.na(age_at_death)) %>%
  select(subject_id, age_at_death, age_at_diagnosis, age, years_since_first_session) %>%
  group_by(subject_id) %>%  # within each subject:
  distinct(subject_id, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(
    session_id = paste(subject_id, "death", sep = "_"),
    years_since_first_session = (age_at_death - age) + years_since_first_session + 0.05,
    age = age_at_death,
    years_since_diagnosis = age_at_death - age_at_diagnosis,
    status = "dead"
  )

transformed_data <- transformed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  mutate(
    ever_apathetic = as.logical(cummax(NPI_apathy_present)),
    # Change in key scores
    cog_delta = (global_z - lag(global_z)) #/ years_between(lag(session_date), session_date)
  ) %>%
  ungroup() %>%
  #filter(!is.na(cog_delta)) %>%
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

transformed_data$cog_delta_resid <- residuals(
  lm(cog_delta ~ 1 + global_z, transformed_data, na.action = "na.exclude")
)
cor(transformed_data$global_z, transformed_data$cog_delta, use = "pairwise.complete.obs")
transformed_data %>% ggplot(aes(x = global_z, y = cog_delta)) + geom_point()
# NEEDS TO TAKE INTO ACCOUNT SUBJECT STRUCTURE
#cor(transformed_data$global_z, transformed_data$cog_delta2, use = "pairwise.complete.obs")
#transformed_data$cog_delta2 = residuals(
#  lm(global_z ~ 1 + lag(global_z), transformed_data, na.action = "na.exclude")
#)

msm::statetable.msm(state, subject_id, data = transformed_data)

Q.mask <- rbind(
  c( NA, 1.0, 1.0),  # A-
  c(1.0,  NA, 1.0),  # A+
  c(0.0, 0.0,  NA)   # dead
)
Q.init <- msm::crudeinits.msm(
  state ~ years_since_diagnosis,
  #state ~ years_since_first_session,
  subject = subject_id,
  data = transformed_data,
  qmatrix = Q.mask
)

mfit <- msm::msm(
  state ~ years_since_diagnosis,
  #state ~ years_since_first_session,
  subject = subject_id,
  data = transformed_data,
  qmatrix = Q.init,
  deathexact = 3,
  covariates =
    ~ sex + age_at_diagnosis + education
      + UPDRS_motor_score + taking_medication + transformed_dose
      + global_z + HADS_depression # + cog_delta + ever_apathetic
)
#print(mfit)
summary(mfit)
msm::hazard.msm(mfit)
msm::pmatrix.msm(mfit, t = 0.2)  # 2 year projection (remember factor of 10!)
msm::totlos.msm(mfit)        # Time spent in each state
msm::envisits.msm(mfit)      # Number of visits
msm::efpt.msm(mfit, tostate = 3)  # Time to death
msm::plot.msm(mfit, range = c(0.01, 3.0))  # Remember factor of 10!
msm::plot.prevalence.msm(
  mfit, mintime = 0.0, maxtime = 5.0, initstates = c(0.9, 0.1, 0.0), censtime = 2.0
)
msm::plot.survfit.msm(mfit, from = 1, to = 3, range = c(0.01, 3.0))
msm::plot.survfit.msm(mfit, from = 2, to = 3, range = c(0.01, 3.0))

###############################################################################
# Recode data as follow-ups

# Pull out up to first apathetic session only
transformed_data <- transformed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  mutate(apathy_present.worst_to_date = cummax(as.integer(NPI_apathy_present))) %>%
  filter(cumsum(apathy_present.worst_to_date) <= 1) %>%
  ungroup() %>%
  arrange(subject_id, session_date)

# Transform to predicting *next* status
transformed_data <- transformed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  mutate(next_apathy = lead(NPI_apathy_present)) %>%
  ungroup() %>%
  arrange(subject_id, session_date)

# And only pull out sessions that are within 2 years
transformed_data <- transformed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  mutate(years_to_next_session = years_between(session_date, lead(session_date))) %>%
  filter(!is.na(next_apathy) & (years_to_next_session <= 2.0)) %>%
  ungroup() %>%
  arrange(subject_id, session_date)

###############################################################################
# Fit models

filename <- file.path(
  "..", "Results", paste("predictive-analyses_", date_string, ".Rout", sep = "")
)
sink(file = filename)
options(width = 1024)

# Data summary?
#writeLines(paste("\n", strrep("*", 72), "\n", sep = ""))

# brms::negbinomial()
# https://mc-stan.org/docs/2_25/functions-reference/nbalt.html
# https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html

base_formula <- "next_apathy ~ 1"  # NPI_apathy_present, HADS_anxiety, HADS_depression
for (
  covariates in list(
    c("first_session_date", "first_session_date2"),
    c("sex", "ethnicity", "education", "age_at_diagnosis"),
    #c("(1 | subject_id)"),
    c("years_to_next_session"),
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
  models <- vector("list", length(formulas))
  for (i in seq_along(formulas)) {
    writeLines(paste("\n", strrep("-", 36), sep = ""))
    writeLines(paste("Model", i))
    #writeLines("Formula:")
    #writeLines(format(formulas[[i]]))
    writeLines("")

    prior <- brms::get_prior(
      formula = formulas[[i]],
      family = brms::bernoulli(link = "logit"),  #brms::negbinomial()
      data = transformed_data
    )
    if (any(prior$class == "b")) {
      prior <- brms::set_prior("normal(0.0, 1.0)", class = "b")
    } else {
      prior <- brms::empty_prior()
    }

    model <- brms::brm_multiple(
      formula = formulas[[i]],
      family = brms::bernoulli(link = "logit"),  #brms::negbinomial()
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

sink()
options(width = 80)
file.show(filename)
#writeLines(readLines(filename))

###############################################################################
