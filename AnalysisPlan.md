# Analysis Plan

This contains the core hypotheses and details of the key analyses to be
performed. A [separate document](Imaging/AnalysisPlan.md) details the imaging
analyses in more detail.

### Quantifying prevalence and dynamics of apathy

Core analysis: a Bayesian, mixed effects, logistic regression relating
diagnosed apathy to core patient information.

Fixed effects of interest:
 + Intercept (i.e. overall prevalence).
 + Linear relationship with age at diagnosis (or symptom onset?).
 + Linear relationship with time since diagnosis.
 + Linear relationship with sex.

Random effects:
 + Subject-specific intercept (i.e. baseline propensity)
 + Subject-specific slope with time since diagnosis (i.e. rate of progression).

Fixed effects of no interest:
 + Medication: levodopa equivalent dose (LED)
 + Ethnicity
 + Education

Methods: see [`brms`](https://github.com/paul-buerkner/brms).
