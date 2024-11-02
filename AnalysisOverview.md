# Core Analyses

This contains some visualisations of the key results from the various models
fitted to the raw data. For these analyses, apathy is defined as an NPI apathy
score >= 1.

All images are clickable links to the higher-quality PDFs.

### Cross-sectional models

The following explore what variables are associated with current apathy status
across the cohort. There are two key models: a Bayesian logistic regression
using a small set of key variables, and a regularised regression that seeks to
tease apart the contributions from individual cognitive tests.

---------

<a href="Figures/cross-sectional_logistic-regression_npi-1.pdf">
<img src="Figures/cross-sectional_logistic-regression_npi-1.jpg" width="90%">
</a>

**Figure**: Odds ratios for the main cross-sectional logistic regression,
showing the associations between a set of key variables and apathy.

---------

<a href="Figures/individual-tests_proportions_npi-1.pdf">
<img src="Figures/individual-tests_proportions_npi-1.jpg" width="45%">
</a>
<a href="Figures/individual-tests_coefs_npi-1.pdf">
<img src="Figures/individual-tests_coefs_npi-1.jpg" width="45%">
</a>

**Figure**: Regularised regressions looking for the associations between
individual cognitive tests and apathy presence. The proportion of times the
variable had a non-zero coefficient across repeats is shown on the left
(significance shown with dots), and the odds ratios are shown on the right.

---------

### Predictive models

The following use a multi-state Markov model to explore which covariates
predict whether or not a particular patient will develop apathy in the future.

---------

<a href="Figures/survival_ever-apathetic_npi-1.pdf">
<img src="Figures/survival_ever-apathetic_npi-1.jpg" width="60%">
</a>
<a href="Figures/predictive-analyses_baseline-hazard_npi-1.pdf">
<img src="Figures/predictive-analyses_baseline-hazard_npi-1.jpg" width="30%">
</a>

<a href="Figures/predictive-analyses_coefs-1-2_npi-1.pdf">
<img src="Figures/predictive-analyses_coefs-1-2_npi-1.jpg" width="90%">
</a>

<a href="Figures/predictive-analyses_coefs-1-3_npi-1.pdf">
<img src="Figures/predictive-analyses_coefs-1-3_npi-1.jpg" width="45%">
</a>
<a href="Figures/predictive-analyses_coefs-2-3_npi-1.pdf">
<img src="Figures/predictive-analyses_coefs-2-3_npi-1.jpg" width="45%">
</a>

**Figure**: Multi-state model coefficients, as well as an illustrative survival
plot. The top row demonstrates that apathy was associated with higher mortality
after controlling for all other variables. The next two rows show the which
variables predict the onset of apathy, and then which variables are associated
with higher mortality. For the prediction of mortality, some coefficients were
constrained to have the same values irrespective of the presence or absence of
apathy to reduce the number of parameters of the model, and these are plotted
in blue.

---------

<a href="Figures/predictive-analyses_trajectories-MoCA_npi-1.pdf">
<img src="Figures/predictive-analyses_trajectories-MoCA_npi-1.jpg" width="45%">
</a>
<a href="Figures/predictive-analyses_trajectories-HADS_depression_npi-1.pdf">
<img src="Figures/predictive-analyses_trajectories-HADS_depression_npi-1.jpg" width="45%">
</a>

<a href="Figures/predictive-analyses_trajectories-transformed-dose_npi-1.pdf">
<img src="Figures/predictive-analyses_trajectories-transformed_dose_npi-1.jpg" width="45%">
</a>
<a href="Figures/predictive-analyses_trajectories-UPDRS_motor_score_npi-1.pdf">
<img src="Figures/predictive-analyses_trajectories-UPDRS_motor_score_npi-1.jpg" width="45%">
</a>

**Figure**: Predictions from the multi-state model, exploring how the
probability that a patient will still be alive, but will have developed apathy,
as both a function of time since diagnosis and some of the key covariates.

---------
