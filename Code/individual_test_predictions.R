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

# individual_test_predictions.R

# Use glmnet to assess predictive power of individual tests
# https://cran.r-project.org/web/packages/glmnet/index.html

###############################################################################

source("initialise_environment.R")

###############################################################################

full_data <- utils.load_data() %>%
  # Drop sessions missing apathy measure
  drop_na(NPI_apathy_present) %>%
  # Reduce to core variables
  preprocessing.select_key_variables() %>%
  # Drop `global_z`: don't want too many of the individual tests missing
  filter(!is.na(global_z)) %>%
  # Drop aggregates of individual scores
  select(!c(global_z, ends_with("_domain")))

transformed_data <- full_data %>%
  preprocessing.transform_key_variables()

imputed_data <- transformed_data %>%
  preprocessing.fill_within_subject() %>%
  preprocessing.run_mice()

###############################################################################
# Helper functions to run regularised GLMs

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
# Unfortunately this is a complete nightmare :-(

# -----------------------------------------------------------------------------
# Function to do the model fitting

fit_regularised_model <- function(data) {
  predictors <- data %>%
    # `glmnet` just runs on all variables rather than requiring a formula
    select(
      first_session_date,
      sex, education, age_at_diagnosis,
      years_since_diagnosis,  transformed_dose, taking_antidepressants,
      UPDRS_motor_score, HADS_depression, HADS_anxiety, starts_with("nptest_")
    ) %>%
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

repeated_fits <- function(data, n_resamplings, shuffle) {
  cvfits = list()
  for (i in seq(1, n_resamplings)) {
    print(i)
    cvfits[[i]] <- data %>%
      subsample_sessions() %>%
      {if (shuffle)
        mutate(
          ., NPI_apathy_present = sample(NPI_apathy_present, replace = FALSE)
        )
        else .} %>%
      fit_regularised_model()
  }
  return(cvfits)
}

# -----------------------------------------------------------------------------
# Function to extract coefficient summaries across multiple repeats

extract_coefs <- function(cvfits) {
  # Flatten to single list
  cvfits <- do.call(list, unlist(cvfits, recursive = FALSE))

  # Combine coefficients
  # Hideously inefficient but only needs to run once
  coefs <- cvfits %>%
    lapply({function(cvfit) coef(cvfit, s = "lambda.min")}) %>%
    lapply(as.matrix) %>%
    {function(mat_list) do.call(cbind, mat_list)}() %>%
    as_tibble(rownames = "covariate", .name_repair = "unique") %>%
    mutate(covariate = factor(covariate, levels = covariate)) %>%  # Needed to maintain ordering
    # https://stackoverflow.com/a/69052207
    pivot_longer(cols = !covariate) %>%
    mutate(value = na_if(value, 0.0)) %>%  # Useful for statistics conditional on selection
    group_by(covariate) %>%
    summarise(
      # Raw numbers
      n = n(),
      n_pos = sum(value > 0.0, na.rm = TRUE),
      n_neg = sum(value < 0.0, na.rm = TRUE),
      # Proportions non-zero
      p_pos = n_pos / n,
      p_neg = n_neg / n,
      # Summary stats / confidence intervals for selected coefs
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      lower = quantile(value, 0.025, na.rm = TRUE),
      upper = quantile(value, 0.975, na.rm = TRUE)
    )

  return(coefs)
}

###############################################################################
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

###############################################################################
# Full run on multiple datasets

# Generate all the fits
cvfits <- lapply(
  mice::complete(imputed_data, action = "all"),
  {function(data) repeated_fits(data, n_resamplings = 100, shuffle = FALSE)}
)
coefs = extract_coefs(cvfits)

# And for the null
null_cvfits <- lapply(
  mice::complete(imputed_data, action = "all"),
  {function(data) repeated_fits(data, n_resamplings = 100, shuffle = TRUE)}
)
# Turn the null into a significance cut off
# This is a two step process: given the proportion of non-zero coefficients
# from the shuffled data, estimate the posterior over the true proportion.
# This is a beta distribution (and we use a uniform (Bayes-Laplace) prior).
# Next, combine this with the uncertainty from only repeating the true analysis
# a finite number of times, which is a binomial distribution. We can use the
# quantiles of this combined beta-binomial distribution to set our significance
# threshold.
null_coefs = extract_coefs(null_cvfits) %>%
  mutate(
    # Beta-binomial 99% CI
    n99 = rmutil::qbetabinom(
      0.99,
      n, # Number of samples for the true analysis
      (n_pos + n_neg + 1) / (n + 2), # beta: a / (a + b)
      n + 2 # beta: (a + b)
    ),
    p99 = n99 / n,
    n99.bonferroni = rmutil::qbetabinom(
      1.0 - ((1.0 - 0.99) / (length(levels(covariate)) - 1)),
      n,
      (n_pos + n_neg + 1) / (n + 2),
      n + 2
    ),
    p99.bonferroni = n99.bonferroni / n,
  )

combined_coefs <- inner_join(
  coefs %>% mutate(p = p_pos + p_neg),
  null_coefs %>% mutate(p = p_pos + p_neg),
  by = "covariate",
  suffix = c("", "_null")
) %>%
  mutate(significant = p > p99.bonferroni)

# Export summary of selection frequencies
utils.save_output(
  function() {
    combined_coefs %>%
      #select(covariate, p_pos, p_neg, p, p99.bonferroni, significant) %>%
      mutate(significant = if_else(significant, "*", "")) %>%
      print(n = Inf, width = Inf)
  },
  "individual-tests"
)

# Plots of selection frequency
plt <- combined_coefs %>%
  filter(covariate != "(Intercept)") %>%
  select(covariate, p_pos, p_neg, significant) %>%
  mutate(
    p_neg = -1.0 * p_neg,
    significant = significant * (2 * ((p_pos + p_neg) > 0) - 1),
  ) %>%
  pivot_longer(c(p_pos, p_neg)) %>%
  mutate(covariate = recode(covariate, !!!constants.variable_names)) %>%
  ggplot(aes(x = covariate, y = value, fill = value)) +  # fill = name
  geom_col() +
  geom_point(data = ~filter(.x, significant != 0), mapping = aes(x = covariate, y = significant)) +
  # Split into predefined domains
  geom_vline(xintercept = 24.5, colour = "grey92") +  # https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
  geom_vline(xintercept = 18.5, colour = "grey92") +
  geom_vline(xintercept = 12.5, colour = "grey92") +
  geom_vline(xintercept = 8.5, colour = "grey92") +
  geom_vline(xintercept = 3.5, colour = "grey92") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(-1.0, 1.0)) +
  #scale_fill_discrete(limits = c("p_pos", "p_neg")) +
  #scale_fill_gradient2(
  #  limits = c(-1.0, 1.0),
  #  low = "blue", mid = "white", high = "red"
  #) +
  colorspace::scale_fill_continuous_diverging(
    palette = "Blue-Red2",
    limits = c(-1.0, 1.0),
  ) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(NULL) +
  ylab("Proportion of non-zero coefficients (split positive / negative)") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    #axis.text.x = element_text(angle = 90)
  )
print(plt)
utils.save_plot(plt, "individual-tests_proportions", width = 6.0, height = 6.0)

# And plot betas along with resampling uncertainty
plt <- coefs %>%
  mutate(across(c(lower, mean, upper), exp)) %>%  # Odds ratio := exp(beta)
  filter(covariate != "(Intercept)") %>%
  arrange(covariate) %>%
  mutate(covariate = recode(covariate, !!!constants.variable_names)) %>%
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
utils.save_plot(plt, "individual-tests_coefs", width = 6.0, height = 6.0)

###############################################################################
