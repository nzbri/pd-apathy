# Analysis Plan

This contains the core hypotheses and details of the key analyses to be
performed. A [separate document](Imaging/AnalysisPlan.md) details the imaging
analyses in more detail.

### Quantifying prevalence and dynamics of apathy

Core analysis: a Bayesian, mixed effects, logistic regression relating
reported apathy to core patient information. The model itself seeks to explain
both the cross-sectional prevalence of apathy (including the effects of age,
sex, etc.) and its first-order temporal dynamics (including whether progression
is better modelled by correlations with motor/cognitive scores than simply
years since diagnosis).

Model definition:
 + Fixed effects (cross sectional):
    + Intercept (i.e. overall prevalence).
    + Sex.
    + Ethnicity.
    + Education.
    + Age at diagnosis.

 + Fixed effects (within subject):
    + Time since diagnosis.
    + UPDRS motor score.
    + Global cognitive score (aggregate z-score across four domains).
    + Medication: levodopa equivalent dose (LED).

 + Fixed effects (interactions):
    + Age at diagnosis and sex with time since diagnosis.

 + Random effects:
    + Subject-specific intercept (i.e. baseline propensity)
    + Subject-specific slope with time since diagnosis (i.e. rate of
      progression).

 + Confounds:
    + Date of baseline session (quadratic).

Methods / inference:
 + [`brms`](https://github.com/paul-buerkner/brms)\
   See e.g. Horne et al., medRxiv, 2020 (DOI:
   [10.1101/2020.09.01.20186312](https://doi.org/10.1101/2020.09.01.20186312))
   for a related set of analyses focusing on PDD.

This would render the following (pseudo) BRMS formula:
```R
model <- brms::brm(
  formula = apathy ~
    # Cross subject
    1 + sex + ethnicity + education + diagnosis_age +
    # Within subject
    years_from_diagnosis + UPDRS_motor_score + global_z + LED +
    # Interactions
    (sex + diagnosis_age):years_from_diagnosis +
    # Confounds
    poly(baseline_date, 2) +
    # Random effects
    (1 + years_from_diagnosis | subject_id),
  family = brms::bernoulli(link = "logit"),
  data = data
)
```

##### Rationale

*Temporal baseline:* While the dates of symptom onset, diagnosis, and the
baseline session could all have been used as each subject's reference point,
the date of diagnosis was chosen as the most naturally available clinical
metric. Of course, all are only estimates of a common timeframe of disease
progression, and the random intercepts within-subject will (amongst others)
account for differences in the time of diagnosis across subjects due to the
linearity of the model.

*Within-subject fixed effects:* The goal here was to define a broad set of
standard measures that capture different notions of 'progression' in a clinical
context. The interest is in whether apathy develops over time, and, more
specifically, whether this accompanies changes in other symptom domains.


### Data selection

##### Definition of apathy

There are three possible sources of information about apathy within the data
itself:
the [Neuropsychiatric Inventory (NPI)](http://npitest.net/npi/about-npi.html),
the [MDS-Unified Parkinson's Disease Rating Scale (MDS-UPDRS)](https://www.movementdisorders.org/MDS/MDS-Rating-Scales/MDS-Unified-Parkinsons-Disease-Rating-Scale-MDS-UPDRS.htm),
and the [Geriatric Depression Scale (GDS)](https://doi.org/10.1300/J018v05n01_09).

They all differ slightly in the consistency of administration, scoring of
apathetic symptoms, and procedure (see
[issue #1](https://github.com/nzbri/pd-apathy/issues/1)).

Our core measure of apathy here is the binary response for the presence/absence
of symptoms from the NPI. This has been most consistently administered within
the cohort here, and widely used in other studies. The core hypotheses and
analyses therefore pertain to this metric, but it should be noted that the
sub-scores are available for more detailed analyses, and the responses from the
other tests are available to examine the consistency of the measures.

##### Inclusion criteria

 + Subject: `participant_group == 'PD'`
 + Subject: must have at least one included session
 + Session: neuropsychiatric assessments performed\
   N.B. this includes incomplete assessments (i.e. `full_assessment == FALSE`).

##### Exclusion criteria

 + Subject: `diagnosis_baseline == 'PDD'`\
   Rationale: Cognitive decline will be too strong a confound for patients who
   are already presenting with dementia.
 + Subject: Incomplete neuropsychiatric assessment at baseline
 + Session: `np_excluded == TRUE`


### Exploratory analyses & visualisations

 + Summarise number of participants, follow up visits, time between sessions
   etc.

 + Are there changes over time with the demographics of the enrolled
   participants (age, time since sympton onset)?

 + 'Matchstick' plot for apathy v. time since baseline and time since symptom
   onset. Similar for `PD-N -> PD-MCI -> PDD`?

 + Plots of apathy scores over time (maybe just frequency × severity against
   age?). Repeat for e.g. key cognitive scores.
