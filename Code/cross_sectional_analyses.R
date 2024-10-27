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

# cross_sectional_analyses.R

# Use BRMS to examine what is associated with the presence of apathy
# https://cran.r-project.org/web/packages/brms/index.html

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

imputed_data <- transformed_data %>%
  preprocessing.fill_within_subject() %>%
  preprocessing.run_mice()

###############################################################################
# Quick visualisation of structure in data missingness

full_data %>%
  summarise(across(everything(), ~mean(is.na(.x)))) %>%
  print(width = Inf)

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
utils.save_plot(plt, "data-missingness")

###############################################################################
# Simple cross-sectional model predicting apathy status

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
  chains = 8, iter = 5000
)
summary(model)

# Save to file
utils.save_output(
  function() {
    print(summary(model))

    cat("\n\n\n")

    cat("Odds ratios:\n")
    print(exp(fixef(model)[,-2])) # Remove Est.Error column: exp(error(...)) =/= error(exp(...))
  },
  "cross-sectional"
)

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
  mutate(covariate = recode(covariate, !!!constants.variable_names)) %>%
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
utils.save_plot(plt, "cross-sectional_logistic-regression")

###############################################################################
