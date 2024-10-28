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

# constants.R

# To use these functions, simply `source("initialise_environment.R")`. That
# will ensure dependencies are appropriately managed.

###############################################################################

constants.data_file = "raw-data_2021-08-17.rds"

constants.NPI_apathy_threshold = (function() {
  threshold <- as.numeric(Sys.getenv("NPI_APATHY_THRESHOLD"))
  if (is.na(threshold)) {
    threshold <- 1
  }
  return(threshold)
})()

constants.date_string = format(lubridate::ymd(lubridate::today()))

###############################################################################

constants.variable_names <- list(
  NPI_apathy_present = "Apathetic",
  first_session_date = "Enrolment date",
  first_session_date2 = "Enrolment date (squared)",
  session_date = "Session date",
  session_date2 = "Session date (squared)",
  sex = "Sex",
  sexFemale = "Sex: female",
  sexMale = "Sex: male",
  education = "Education (years)",
  ethnicity = "Ethnicity",
  age = "Age",
  age_at_diagnosis = "Age at diagnosis",
  years_since_diagnosis = "Decades since diagnosis",
  taking_medication = "Taking levodopa (or equivalent)",
  taking_medicationNo = "Unmedicated",
  taking_medicationYes = "Taking levodopa (or equivalent)",
  transformed_dose = "Levodopa equivalent dose",
  LED = "Levodopa equivalent dose",
  taking_antidepressants = "Taking antidepressants",
  taking_antidepressantsYes = "Taking antidepressants",
  UPDRS_motor_score = "Motor symptoms (UPDRS)",
  MoCA = "Cognition (MoCA)",
  HADS_depression = "Depression (HADS)",
  HADS_anxiety = "Anxiety (HADS)",
  # Individual tests
  # https://github.com/nzbri/redcap/blob/master/python/export.py
  # https://doi.org/10.1101/2020.05.31.126870
  # Attention
  nptest_digits_fb_z = "Digits forwards/backwards [A]",
  nptest_digit_ordering_test_z = "Digit ordering [A]",
  nptest_map_search_z = "Map search (1 min) [A]",
  nptest_stroop_colour_z = "Stroop color reading [A]",
  nptest_stroop_words_z = "Stroop word reading [A]",
  nptest_trails_a_z = "Trail making (part A) [A]",
  # Executive
  nptest_action_fluency_z = "Action fluency [E]",
  nptest_letter_fluency_z = "Letter fluency [E]",
  nptest_category_fluency_z = "Category fluency [E]",
  nptest_category_switching_z = "Category switching [E]",
  nptest_trails_b_z = "Trail making (part B) [E]",
  nptest_stroop_inhibition_z = "Stroop interference [E]",
  # Visuo
  nptest_jlo_z = "Judgement of line orientation [V]",
  nptest_vosp_z = "VOSP fragmented letters [V]",
  nptest_picture_completion_z = "Picture completion [V]",
  nptest_rey_complex_copy_z = "RCFT copy [V]",
  # Memory
  nptest_cvlt_free_recall_z = "CVLT-II SF immediate recall [M]",
  nptest_cvlt_short_delay_z = "CVLT-II SF short delay [M]",
  nptest_cvlt_long_delay_z = "CVLT-II SF long delay [M]",
  nptest_rey_complex_immediate_z = "RCFT immediate recall [M]",
  nptest_rey_complex_delay_z = "RCFT delayed recall [M]",
  # Language
  nptest_boston_naming_z = "Boston naming test [L]",
  nptest_language_adas_cog = "ADAS-Cog: language [L]",
  nptest_language_drs2 = "Mattis DRS-2: similarities [L]"
)

###############################################################################
