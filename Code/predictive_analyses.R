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

# predictive_analyses.R

# Use MSM to predict future develpment of apathy
# https://cran.r-project.org/web/packages/msm/index.html

###############################################################################

source("initialise_environment.R")
source("constants.R")
source("utils.R")
source("preprocessing.R")

###############################################################################

full_data <- utils.load_data() %>%
  # Drop sessions missing apathy measure
  drop_na(NPI_apathy_present) %>%
  # Reduce to core variables
  preprocessing.select_key_variables() %>%
  # Drop individual neuropsych tests
  select(!starts_with("nptest_"))

transformed_data <- full_data %>%
  preprocessing.transform_key_variables()

###############################################################################
# Use `VIM` to do single imputation
# https://cran.r-project.org/web/views/MissingData.html#single
# Not straightforward to combine multiple predictive models together

imputed_data <- transformed_data %>%
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
imputed_data <- transformed_data %>%
  select(subject_id, session_id, age_at_death) %>%
  bind_cols(imputed_data) %>%
  group_by(subject_id) %>%
  mutate(
    across(c(sex, ethnicity, handedness, side_of_onset), utils.mode)
  ) %>%
  ungroup()

###############################################################################
# Basic survival model for predicting death

# vignette("survival")

# These analyses are very simple, and primarily for visualisation. The MSM
# version is much more detailed.

# Firstly, we generate simple Kaplan-Meier survival based on either the most
# recent visit or time of death, stratified by if patients developed apathy at
# any point. While not really a predictive analysis, it does negate issues with
# patients switching groups as they develop apathy (while not 100% clear, this
# is probably a violation of the standard KM assumptions). This gives the
# nicest plots.

# Secondly, we do a simple Cox proportional-hazard model, with apathy as the
# only covariate. Again, very simple but useful to get a single summary
# parameter.

# -----------------------------------------------------------------------------
# 1) Simple Kaplan-Meier survival plots

# Make dataset with (time, death) structure
survival_data <- imputed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  mutate(
    time = if_else(
      is.na(age_at_death),
      max(years_since_diagnosis),
      age_at_death - age_at_diagnosis
    ),
    dead = !is.na(age_at_death),
    ever_apathetic = as.logical(max(NPI_apathy_present)),
  ) %>%
  select(subject_id, time, dead, ever_apathetic) %>%
  slice_head() %>%
  ungroup() %>%
  arrange(subject_id)

# Fit a basic stratified survival model
vfit <- survival::survfit(
  survival::Surv(time, dead) ~ 1 + ever_apathetic,
  data = survival_data,
  id = subject_id
)

print(vfit)
plot(vfit, xlab = "Decades since diagnosis", ylab = "Survival")

# Plot the different survival curves
plt <- survminer::ggsurvplot(
  vfit,
  #fun = "cumhaz",
  xscale = 1.0 / 10.0,
  xlim = c(0.0, 2.5),
  xlab = "Years since diagnosis",
  censor.shape = "|",
  palette = c("#1F77B4", "#FF7F0E"),
  conf.int = TRUE,
  #surv.median.line = "hv",
  #pval = TRUE,
  #risk.table = TRUE,
  #cumevents = TRUE,
  #cumcensor = TRUE,
  #ncensor.plot = TRUE,
  legend.labs = c("Never apathetic", "Recorded apathy"),
  legend.title = element_blank(),
  legend = c(0.85, 0.85),
  ggtheme = theme_light()
)
print(plt)
utils.save_plot(plt$plot, "survival_ever-apathetic")

# -----------------------------------------------------------------------------
# 2) Cox proportional hazards model

# Make dataset with (time1, time2, death) structure
survival_data <- imputed_data %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  mutate(
    apathetic_to_date = as.logical(cummax(NPI_apathy_present)),  # Now or previously
    # Recode as (time1, time2) for survival analysis
    time1 = years_since_diagnosis,
    time2 = lead(years_since_diagnosis),
    dead = (!is.na(age_at_death) & is.na(time2)),
    time2 = if_else(is.na(time2), age_at_death - age_at_diagnosis, time2)
  ) %>%
  filter(!is.na(time2) & ((time2 - time1) > 0.001)) %>%
  ungroup() %>%
  arrange(subject_id, session_date)

# Simple Cox proportional hazards model
# survival::survcheck(
#   survival::Surv(time1, time2, dead) ~ 1 + apathetic_to_date,
#   data = survival_data,
#   id = subject_id
# )
vfit <- survival::coxph(
  survival::Surv(time1, time2, dead) ~ 1 + apathetic_to_date,
  data = survival_data,
  id = subject_id
)
summary(vfit)

# 2) Quick visualisation, though statistics from `summary(vfit)` are more important
survminer::ggadjustedcurves(
  vfit,
  data = as.data.frame(survival_data),  # https://github.com/kassambara/survminer/issues/501
  variable = "apathetic_to_date",
  method = "average",
  ci = TRUE
)
# Not straightforward to use `ggsurvplot()` here: `apathetic_to_date` isn't really a strata
# https://github.com/kassambara/survminer/issues/440

###############################################################################
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
    mutate(ever_apathetic = as.logical(cummax(NPI_apathy_present))) %>%
    ungroup() %>%
    # Combine with dummy sessions
    full_join(death_proxy_sessions) %>%
    arrange(subject_id, session_id) %>%
    mutate(
      # Predict current apathy (i.e. with remissions)
      #status = if_else(is.na(status), as.character(NPI_apathy_present), status),
      # Predict apathy onset
      status = if_else(is.na(status), as.character(ever_apathetic), status),
      status = ordered(
        status,
        labels = c("A-", "A+", "Dead"),
        levels = c("FALSE", "TRUE", "dead")
      ),
      # Predict mortality
      # status = if_else(is.na(status), "alive", status),
      # status = ordered(
      #   status,
      #   labels = c("alive", "dead")
      # ),
      state = as.numeric(status)
    ) %>%
    filter(years_since_diagnosis > 0.0)
  #select(subject_id, session_date, years_since_diagnosis, age, age_at_death, NPI_apathy_present) %>%
  #View()
  
  # Number of subjects
  print("No. of subjects")
  data %>%
    group_by(subject_id) %>%
    filter(n() > 1) %>%
    slice_head() %>%
    ungroup() %>%
    nrow() %>%
    print()
  # Number of sessions
  print("No. of transitions")
  print(msm::statetable.msm(state, subject_id, data = data))
  
  # Predict apathy
  Q.mask <- rbind(
    c( NA, 1.0, 1.0),  # A-
    c(0.0,  NA, 1.0),  # A+
    c(0.0, 0.0,  NA)   # dead
  )
  # With remissions
  # Q.mask <- rbind(
  #   c( NA, 1.0, 1.0),  # A-
  #   c(1.0,  NA, 1.0),  # A+
  #   c(0.0, 0.0,  NA)   # dead
  # )
  # Predict mortality
  # Q.mask <- rbind(
  #   c( NA, 1.0),  # alive
  #   c(0.0,  NA)   # dead
  # )
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
  # With remissions
  # Here, we tie transition 2 (A- -> death) to 4 (A+ -> death)
  # Matches Q.mask, ordered along rows then down columns
  # + 1 2
  # 3 + 4
  # + + +
  #constrained <- c(1,2,3,2)
  constrained_covariates = c(
    "first_session_date", "sexFemale", "education", "age_at_diagnosis",
    "transformed_dose", "UPDRS_motor_score"
  )

  # Fit model!
  mfit <- msm::msm(
    state ~ years_since_diagnosis,
    #state ~ years_since_first_session,
    subject = subject_id,
    data = data,
    qmatrix = Q.init,
    deathexact = 3,  # I.e. state 3 (death) is at a known time, rather than just between visits
    covariates =
      ~ first_session_date + # first_session_date2 +
      sex + education + age_at_diagnosis + # taking_medication +
      transformed_dose + taking_antidepressants +
      UPDRS_motor_score + MoCA + HADS_depression + HADS_anxiety,
    constraint = Map(function(x) {constrained}, constrained_covariates)
    # With remissions
    #control = list(fnscale = 500)
  )

  mfit$constrained_covariates <- constrained_covariates

  mfit$states <- data %>%
    pull(status) %>%
    levels() %>%
    (function(x) setNames(seq_along(x), x)) %>%
    (function(x) list(msm_index = x, msm_name = map_chr(x, ~paste("State", .x))))
  print("States")
  print(mfit$states)

  return(mfit)
}

# -----------------------------------------------------------------------------
# Run the analyses and save key results

mfit <- utils.save_output(
  function() {
    mfit <- fit_predictive_model(imputed_data)
    states <- mfit$states

    cat("\n\n\n")

    print(mfit)

    cat("\n\n\n")

    # Compare risk of death for A+ v. A-
    print("Relative risk of death (A+ / A-)")
    print(msm::qratio.msm(
      mfit,
      states$msm_index[c("A+", "Dead")],
      states$msm_index[c("A-", "Dead")]
    ))

    return(mfit)
  },
  "predictive-analyses"
)
states <- mfit$states

# -----------------------------------------------------------------------------
# A collection of assorted functions for interrogating the fitted model
# Not needed for the key visualisations, but some useful insights

if (FALSE) {
  # Raw transition matrix
  msm::qmatrix.msm(mfit, covariates = "mean")
  msm::pmatrix.msm(mfit, t = 0.2, covariates = "mean")  # 2 year projection (remember factor of 10!)

  # Summaries thereof
  msm::pnext.msm(  # Probability of next state
    mfit, covariates = "mean" #covariates = list(sex = "Male", taking_medication = "Yes", global_z = 0.0)
  )
  #msm::totlos.msm(mfit, start = 1, covariates = "mean")  # Time spent in each state
  #msm::envisits.msm(mfit, start = 1, covariates = "mean")  # Number of visits
  msm::efpt.msm(mfit, tostate = states$msm_index[["Dead"]], covariates = "mean", ci = "normal")  # Time to death
  #msm::sojourn.msm(mfit)

  # Random plotting functions
  msm::plot.msm(mfit, range = c(0.01, 3.0))  # Remember factor of 10!
  msm::plot.prevalence.msm(
    mfit, mintime = 0.0, maxtime = 2.5, initstates = c(1.0, 0.0, 0.0),
    xlab = "Decades since diagnosis"
  )
  msm::plot.survfit.msm(mfit, from = states$msm_index[["A-"]], to = states$msm_index[["Dead"]], range = c(0.01, 3.0))
  msm::plot.survfit.msm(mfit, from = states$msm_index[["A+"]], to = states$msm_index[["Dead"]], range = c(0.01, 3.0))
}

###############################################################################
# Plot difference in baseline hazard for death

plt <- msm::qmatrix.msm(mfit, covariates = "mean") %>%
  map(~.x) %>% # Strip off the MSM magic
  discard_at(c("fixed")) %>% # Remove the stuff with weird row/column names
  # Transform list of matrices to a single tibble
  imap(function(data, idx) {
    tbl <- data %>%
      as_tibble(rownames = "from") %>%
      pivot_longer(cols = !from, names_to = "to", values_to = idx)
    return(tbl)
  }) %>%
  reduce(~inner_join(.x, .y, by = c("from", "to"))) %>%
  # Switch to our naming system
  # https://github.com/tidyverse/dplyr/issues/6623#issuecomment-1362887413
  mutate(
    from = case_match(from, !!!imap(states$msm_name, rlang::new_formula)),
    to = case_match(to, !!!imap(states$msm_name, rlang::new_formula))
  ) %>%
  # Get ready for ggplot
  filter(from == "A-" | from == "A+", to == "Dead") %>%
  mutate(
    from = case_match(from, "A-" ~ "No apathy", "A+" ~ "Apathetic"),
    from = forcats::fct_rev(factor(from))
  ) %>%
  # And plot
  ggplot(aes(x = from, y = estimates, ymin = L, ymax = U, fill = from)) +
  geom_col() +
  geom_errorbar(width = 0.2, linewidth = 0.75) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E"), guide = "none") +
  labs(x = NULL, y = "Baseline hazard for mortality") +
  theme_light() +
  theme(axis.text = element_text(size = rel(1.0)))

print(plt)
utils.save_plot(plt, "predictive-analyses_baseline-hazard", width = 3.0, height = 4.0)

###############################################################################
# Plot hazard ratios

for (transition in list(
  list(from = "A-", to = "A+", constraint = FALSE),
  # With remissions
  #list(from = "A+", to = "A-", constraint = FALSE),
  list(from = "A-", to = "Dead", constraint = TRUE),
  list(from = "A+", to = "Dead", constraint = TRUE)
)) {
  
  msm_transition <- paste(
    states$msm_name[c(transition$from, transition$to)],
    collapse = " - "
  )
  title <- str_replace(
    paste(transition$from, transition$to, sep = " to "), "Dead", "death"
  )
  
  plt <- msm::hazard.msm(mfit) %>%
    lapply("[", msm_transition, ) %>%
    bind_rows(.id = "covariate") %>%
    mutate(
      covariate_type = case_when(
        covariate %in% c("first_session_date") ~ "confound",
        covariate %in% mfit$constrained_covariates & transition$constraint ~ "constrained",
        .default = "free"
      ),
      covariate_type = factor(
        covariate_type, levels = c("confound", "constrained", "free")
      )
    ) %>%
    mutate(
      covariate = factor(covariate, levels = rev(covariate)),
      covariate = recode(covariate, !!!constants.variable_names)
    ) %>%
    # Plot itself
    # https://stackoverflow.com/a/38064297
    ggplot(aes(x = covariate, y = HR, ymin = L, ymax = U, colour = covariate_type)) +
    geom_pointrange() +
    geom_vline(xintercept = 4.5, colour = "grey92") +  # https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
    geom_vline(xintercept = 6.5, colour = "grey92") +
    geom_vline(xintercept = 9.5, colour = "grey92") +
    geom_hline(yintercept = 1, linetype = "dashed") +  # add a dotted line at x=1 after flip
    scale_y_continuous(trans = "log", breaks = c(0.33, 1.0, 3.0)) +
    coord_flip(ylim = c(0.2, 5.0)) +  # flip coordinates (puts labels on y axis)
    scale_colour_manual(values = c("grey", "royalblue3", "black"), drop = FALSE, guide = "none") +
    labs(x = NULL, y = "Hazard ratio (95% CI)", title = title) +
    theme_bw() +  # use a white background
    # https://stackoverflow.com/a/8992102
    theme( # remove the vertical grid lines
      panel.grid.major.y = element_blank()
    )

  print(plt)
  utils.save_plot(plt, paste(
    "predictive-analyses_coefs-",
    paste(states$msm_index[c(transition$from, transition$to)], collapse = "-"),
    sep = ""
  ))
}

###############################################################################
# Plot predicted progression as a function of key covariates

# Function to pull out probability of apathy at a given time, with a set of covariates
p_apathy <- function(mfit, years_since_diagnosis, covariate_values) {
  # Be clear about what the defaults are for factors. Everything else will
  # default to zero which is what we want given that the data has been centered.
  covariates = list(
    sex = "Male", taking_antidepressants = "No"
  )
  for (covariate in names(covariate_values)) {
    covariates[[covariate]] <- covariate_values[[covariate]]
  }

  pmat = msm::pmatrix.msm(
    mfit,
    t = years_since_diagnosis / 10.0,
    covariates = covariates
  )
  return(
    # p(A- -> A+ | alive)
    #pmat[["State 1", "State 2"]] / (pmat[["State 1", "State 1"]] + pmat[["State 1", "State 2"]])
    # p(A- -> A+, alive)
    pmat[[states$msm_index[["A-"]], states$msm_index[["A+"]]]]
  )
}

# -----------------------------------------------------------------------------

for (variable in list(
  list(name = "MoCA", transform = function(x) x, trans = "identity"),
  list(name = "UPDRS_motor_score", transform = function(x) x, trans = "identity"),
  list(name = "transformed_dose", transform = function(x) x^2, trans = "sqrt"),
  list(name = "HADS_depression", transform = function(x) x, trans = "identity")
)) {
  
  plt <- expand_grid(
    years_since_diagnosis = seq(0.0, 20.0, 0.1),
    values = seq(-2.0, 2.0, 0.25)
  ) %>%
    mutate(p_apathy = map2_dbl(
      years_since_diagnosis, values,
      function(years, value) {p_apathy(mfit, years, lst(!!variable$name := value))}
    )) %>%
    ggplot(aes(
      x = years_since_diagnosis,
      y = p_apathy,
      group = values,
      colour = variable$transform(
        attr(transformed_data[[variable$name]], "scaled:center") +
        values * attr(transformed_data[[variable$name]], "scaled:scale")
      )
    )) +
    geom_line() +
    # scale_y_continuous(limits = c(0.0, 0.55)) +
    scale_colour_viridis_c(trans = variable$trans) +
    theme_bw() +
    labs(
      x = "Years since diagnosis",
      #y = "p(apathetic | alive)",
      y = "p(apathetic, alive)",
      colour = NULL,
      title = constants.variable_names[[variable$name]]
    )
  print(plt)
  utils.save_plot(plt, paste("predictive-analyses_trajectories-", variable$name, sep = ""))
}

###############################################################################
