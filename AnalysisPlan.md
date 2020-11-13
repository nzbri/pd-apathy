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
