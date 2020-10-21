# Analysis Plan

This contains the core hypotheses and details of the key analyses to be
performed. A [separate document](Imaging/AnalysisPlan.md) details the imaging
analyses in more detail.

### Quantifying prevalence and dynamics of apathy

Core analysis: a Bayesian, mixed effects, logistic regression relating
diagnosed apathy to core patient information.

Fixed effects:
 + Intercept (i.e. overall prevalence).
 + Sex.
 + Ethnicity.
 + Education.
 + Age at diagnosis (or symptom onset?).
 + Time since diagnosis.
 + Medication: levodopa equivalent dose (LED).
 + Global cognitive score (aggregate z-score across four domains).

Random effects:
 + Subject-specific intercept (i.e. baseline propensity)
 + Subject-specific slope with time since diagnosis (i.e. rate of progression).

Methods:
 + [`brms`](https://github.com/paul-buerkner/brms)\
   See e.g. Horne et al., medRxiv, 2020 (DOI:
   [10.1101/2020.09.01.20186312](https://doi.org/10.1101/2020.09.01.20186312))
   for a related set of analyses focusing on PDD.


### Data selection

##### Inclusion criteria

 + `participant_group == 'PD'`

##### Exclusion criteria

 + `diagnosis_baseline == 'PDD'`\
   Rationale: Cognitive decline will be too strong a confound for patients who
   are already presenting with dementia.


### Exploratory analyses & visualisations

 + Summarise number of participants, follow up visits, time between sessions
   etc.

 + Are there changes over time with the demographics of the enrolled
   participants (age, time since sympton onset)?

 + 'Matchstick' plot for apathy v. time since baseline and time since symptom
   onset. Similar for `PD-N -> PD-MCI -> PDD`?

 + Plots of apathy scores over time (maybe just frequency × severity against
   age?). Repeat for e.g. key cognitive scores.
