# Analysis Plan

This contains the core hypotheses and details of the key analyses to be
performed. A [separate document](Imaging/AnalysisPlan.md) details the imaging
analyses in more detail.


### Table of contents

 + [Core analysis: summary](#core-analysis-summary)
 + [Data collection and variable definitions](#data-collection)
 + [Data selection](#data-selection)
 + [Exploratory analyses & visualisations](#exploratory-analyses)
 + [Core analysis: methodological details.](#core-analysis-details)
 + [Additional analyses](#additional-analyses)


<a name="core-analysis-summary"></a>
### Core analysis: summary &mdash; Quantifying prevalence and dynamics of apathy

Core analysis: a hierarchical Bayesian logistic regression relating reported
apathy to core patient information. The model itself seeks to explain both the
cross-sectional prevalence of apathy (including the effects of age, sex, etc.)
and its first-order temporal dynamics (including whether progression is better
modelled by correlations with motor/cognitive scores than simply years since
diagnosis).

Model definition:
 + Intercept (i.e. overall prevalence).

 + Subject-level predictors:
    + Sex.
    + Ethnicity.
    + Education.
    + Age at diagnosis.

 + Hierarchically modelled variability, grouped by subject:
    + Subject-specific intercept (i.e. baseline propensity)

 + Session-level predictors:
    + Time since diagnosis.
    + UPDRS motor score.
    + Global cognitive score (aggregate z-score across four domains).
    + HADS depression score.
    + Medication: levodopa equivalent dose (LED).

 + Interactions:
    + Age at diagnosis and sex with time since diagnosis.

 + Hierarchically modelled variability, grouped by subject:
    + Subject-specific slopes with session-level predictors (i.e. rates of
      progression).

 + Confounds:
    + Date of first visit (quadratic).

##### Rationale

*Temporal baseline:* While the dates of symptom onset, diagnosis, and the
baseline session could all have been used as each subject's reference point,
the date of diagnosis was chosen as the most naturally available clinical
metric. Of course, all are only estimates of a common timeframe of disease
progression, and the random intercepts within-subject will (amongst others)
account for differences in the time of diagnosis across subjects due to the
linearity of the model.

*Session-level predictors:* The goal here was to define a broad set of
standard measures that capture different notions of 'progression' in a clinical
context. The interest is in whether apathy develops over time, and, more
specifically, whether this accompanies changes in other symptom domains.


<a name="data-collection"></a>
### Data collection and variable definitions

This subsection contains a brief overview of the most salient information with
regards to the study protocols and derived variables.

##### Session types

Broadly speaking, there are three types of sessions:
 + Full sessions are the most comprehensive, containing the full battery of ≈25
   neuropsychiatric tests (i.e. that are used for the global cognitive scores
   discussed below), as well as all the other measures.
 + Short sessions are a stripped down version of the above. The key data from
   our point of view is MoCA, HADS, UPDRS Part III, and medications.
 + Screening sessions were used when enrolling the 2015 cohort. These were even
   shorter than the 'short' sessions and included MoCA, HADS, and a handful of
   neuropsych tests (but no UPDRS).

Furthermore, some information is collected from significant others /
caregivers. This happens at both short and full sessions, but not at the
screening sessions. As such, some other measures (e.g. the NPI, as discussed
below) are also absent from screening sessions. Finally, this significant other
session may take place on a different date, but should be within a month or so
of the participant session.

For more information, see the discussion in
[issue #8](https://github.com/nzbri/pd-apathy/issues/8).

##### Definition of apathy

There are three possible sources of information about apathy within the data
itself:
the [Neuropsychiatric Inventory (NPI)](http://npitest.net/npi/about-npi.html),
the [MDS-Unified Parkinson's Disease Rating Scale (MDS-UPDRS)](https://www.movementdisorders.org/MDS/MDS-Rating-Scales/MDS-Unified-Parkinsons-Disease-Rating-Scale-MDS-UPDRS.htm),
and the [Geriatric Depression Scale (GDS)](https://doi.org/10.1300/J018v05n01_09).

They all differ slightly in the consistency of administration, scoring of
apathetic symptoms, and procedure.
 + *NPI:* This is administered at the significant other sessions.
 + *MDS-UPDRS:* This is collected from the patient at full sessions. We only
   have the apathy specific portions of the MDS-UPDRS (Part IA, Q1.5) after
   2010 when the new test was introduced. Furthermore, in short sessions only
   Part III (motor examination) is collected, so neither these or the screening
   sessions contain this apathy measure.
 + *GDS:* This is collected at both the short and full sessions in a
   conditional two-step process. Four screening questions (including two on
   apathy) are given, and any positive responses gives a further eleven
   questions (with another two apathy-related questions).

For more information, see the discussion in
[issue #1](https://github.com/nzbri/pd-apathy/issues/1).

Our core measure of apathy here is the binary response for the presence/absence
of symptoms from the NPI. This has been more consistently administered than the
MDS-UPDRS question within the cohort here, does not have the complication of
the two-step procedure like the GDS, and has been widely used in other studies.
The core hypotheses and analyses therefore pertain to this metric, but it
should be noted that the sub-scores are available for more detailed analyses,
and the responses from the other tests are available to examine the consistency
of the measures (as well as to potentially compare self-report v. significant
other responses).

##### Cognitive scores

A large number of cognitive tests are administered to participants, and most of
these are automatically collapsed down to a simple set of summary measures.
Primarily, this is as a cross-domain global z-score (i.e. `global_z`), which is
itself the mean over more than 20 tests. The individual tests are z-scored
based on either control data or published conversion tables, where available.

Finally, these scores are also available on a per domain basis:
 + `attention_domain`: Attention, working memory and processing speed.
 + `executive_domain`: Executive function.
 + `visuo_domain`: Visuoperceptual / visuospatial.
 + `learning_memory_domain`: Learning and memory.
 + `language_domain`: Language.

References:
 + `NZBRI PD Progression Study/Admin/Study description/Progression_study_Explanation of Data Export for MJFF_NZBRI_KW01.docx`
 + <https://github.com/nzbri/redcap/blob/6090f1ee3449130a85a32cc9a9badb170be1b0bb/python/export.py#L828>


<a name="data-selection"></a>
### Data selection

##### Inclusion criteria

 + Subject: `participant_group == 'PD'`
 + Session: neuropsychiatric assessments performed\
   N.B. this includes incomplete assessments (i.e. `full_assessment == FALSE`).

##### Exclusion criteria

 + Subject: `diagnosis_baseline == 'PDD'`\
   Rationale: Cognitive decline will be too strong a confound for patients who
   are already presenting with dementia.
 + Session: screening visit only
 + Session: `any(study_excluded) | np_excluded | study_group == "Excluded"`
 + Data inconsistencies:
    + Session: Gap between patient and significant other sessions greater than
      90 days.
    + UPDRS measures: Gap between patient and UPDRS sessions greater than 90
      days.


<a name="exploratory-analyses"></a>
### Exploratory analyses & visualisations

 + Summarise number of participants, follow up visits, time between sessions
   etc.

 + Are there changes over time with the demographics of the enrolled
   participants (age, time since sympton onset)?

 + 'Matchstick' plot for apathy v. time since baseline and time since symptom
   onset. Similar for `PD-N -> PD-MCI -> PDD`?

 + Plots of apathy scores over time (maybe just frequency × severity against
   age?). Repeat for e.g. key cognitive scores.


<a name="core-analysis-details"></a>
### Core analysis: methodological details

##### Additional exclusion criteria

 + Session: Core measure of apathy missing (i.e. `is.na(NPI_apathy_present)`).

##### Variable selection

The following subset of variables is taken as the 'core' set of variables going
forwards. While not all are used in the main model, they provide a slightly
richer description of subjects/sessions for imputation.
```R
    # Database
    subject_id, session_id,
    # Subject-level
    sex, ethnicity, education, handedness, side_of_onset,
    # Age related
    age, age_at_symptom_onset, age_at_diagnosis,
    # Cognitive scores
    global_z, MoCA, WTAR,
    # Neuropsych tests
    NPI_apathy_present, NPI_total, HADS_anxiety, HADS_depression,
    # Clinical measures
    diagnosis, Hoehn_Yahr, UPDRS_motor_score, UPDRS_source, LED,
    # Confounds
    first_session_date
```

##### Data imputation

 + MICE procedure

<https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html>


##### Variable transformations

 + z-scoring
 + LED transformation
 + Derived variables

##### Model specification and inference

Model specification / inference is performed by the [`brms`](https://github.com/paul-buerkner/brms)
package. See e.g. Horne et al., medRxiv, 2020 (DOI: [10.1101/2020.09.01.20186312](https://doi.org/10.1101/2020.09.01.20186312))
for a related set of analyses focusing on PDD. The full set of variables to
test is detailed earlier.

We place weakly informative `N(0,1)` priors on each of the population-level
('fixed') effects regression parameters, and these should be appropriate given
the data transformations detailed above.

Together with the model description outlined earlier, this would render the
following (pseudo) BRMS formula:
```R
# See vignette("brms_multilevel") for notation and nomenclature
model <- brms::brm(
  formula = apathy ~ 1 + ... + (... | subject_id),
  family = brms::bernoulli(link = "logit"),
  prior = brms::set_prior("normal(0.0, 1.0)", class = "b"),
  data = transformed_data
)
```

##### Model comparison

We iteratively compare nested families of models, and pick the winning model
based on generalisability to unseen subjects as measured via the LOOIC/ELPD.
See [issue #6](https://github.com/nzbri/pd-apathy/issues/6) for a more
detailed discussion of this approach.

The procedure is roughly as follows:
 + Start with a base/null model (e.g. `apathy ~ 1`), and a set of variables to
   test (e.g. `age, sex`).
 + Fit the full set of models implied by the different possible combinations.
   For the example here, this would be:
    + `apathy ~ 1`
    + `apathy ~ 1 + age`
    + `apathy ~ 1 + sex`
    + `apathy ~ 1 + age + sex`
 + Score each model using the ELPD, estimated via leave-one-out
   cross-validation.
 + Take the winning model as the base model for the next set of variables, and
   repeat.

We will also report the difference in ELPD between each of the single-term
models and the base model, though this is not used explicitly. Rather this is
simply another readout of model performance, albeit one that is less useful in
the presence of strongly correlated variables.

##### Conditional analyses

 + Interactions / random effects
 + Temporal predictability
 + Cognitive subdomains


<a name="additional-analyses"></a>
### Additional analyses


