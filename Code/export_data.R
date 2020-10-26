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

# export_data.R

###############################################################################

source("initialise_environment.R")

###############################################################################
# Load from database, excluding as necessary

participants <- chchpd::import_participants()
participants <- participants %>%
  filter(participant_group == "PD")

# -----------------------------------------------------------------------------

sessions <- chchpd::import_sessions()
sessions <- sessions %>%
  filter(!is.na(session_id)) %>%
  filter(is.na(study_excluded) | study_excluded != TRUE) %>%
  filter(!is.na(session_date) & session_date <= today())  # Exclude anything only scheduled to happen

# @m-macaskill: Convention for e.g. `study_excluded` is to go off positive
# evidence, as some of the data predates some of the auto-validations that
# generate these fields (i.e. exclude on `FALSE` but not `NA`).
# See e.g. the source for `chchpd::import_sessions()` itself
#sessions %>% count(study_excluded) %>% print(n = Inf)

# Remove duplicates and study-specific information
sessions <- sessions %>%
  select(subject_id, session_id, session_date, age, mri_scan_no)  %>%
  distinct(session_id, .keep_all = TRUE)
# TODO: Sanity check for different `mri_scan_no`?

# -----------------------------------------------------------------------------

neuropsych <- chchpd::import_neuropsyc(concise = TRUE)
neuropsych <- neuropsych %>%
  filter(is.na(np_excluded) | np_excluded != TRUE)

# -----------------------------------------------------------------------------

npi <- read_csv(file.path("..", "Data", "npi_2020-07-13.csv"))
# Remove columns that are irrelevant / already covered by `sessions` and
# `neuropsych` / specific to other domains
npi <- npi %>%
  filter(is.na(session_neuropsych_excluded) | session_neuropsych_excluded != "Y" ) %>%
  select(
    -X1, -subject_id, -record_id, -npi_data_missing, -npi_data_missing_reason,
    -npi_notes, -npi_occ_disruption_total, -npi_session) %>%
  select(-matches("session_[^i][^d]")) %>%
  select(-contains("redcap")) %>%
  select(-matches("npi_[a-f]_")) %>%
  select(-matches("npi_[h-l]_"))

# -----------------------------------------------------------------------------

medications <- chchpd::import_medications(concise = TRUE)

# -----------------------------------------------------------------------------

hads <- chchpd::import_HADS(concise = TRUE)

# -----------------------------------------------------------------------------

updrs <- chchpd::import_motor_scores()

###############################################################################
# Join all the records together, linked by subject or session IDs

# Use `inner_join` to maintain the exclusions specific to the individual
# databases
full_data <-
  inner_join(participants, sessions, by = "subject_id") %>%
  inner_join(neuropsych, by = "session_id") %>%
  left_join(npi, by = "session_id") %>%
  left_join(medications, by = "session_id") %>%
  left_join(hads, by = "session_id") %>%
  left_join(updrs, by = "session_id")

# Remove subjects with missing / incomplete baselines
full_data <- filter(
  full_data,
  !(subject_id %in%
    (
      full_data %>%
        filter((session_date == date_baseline) & (full_assessment == FALSE)) %>%
        pull(subject_id)
    )
  )
)

# TODO: Missing sessions in NPI CSV file?
# ggplot(full_data %>% filter(!is.na(npi) & is.na(npi_total))) + geom_histogram(aes(session_date))

colnames(full_data)

###############################################################################
# Summaries

full_data$subject_id %>% unique %>% length
full_data$session_id %>% unique %>% length
full_data %>% filter(full_assessment) %>% pull(session_id) %>% unique %>% length
full_data %>% count(study) %>% print(n = Inf)
full_data %>% count(study_group) %>% print(n = Inf)
full_data %>% count(diagnosis_baseline) %>% print(n = Inf)
full_data %>% count(diagnosis) %>% print(n = Inf)
full_data %>% summarise(across(.fns = ~ sum(is.na(.x)))) %>% print(width = Inf)

###############################################################################
# Save to file

write_csv(
  full_data,
  file.path("..", "Data", paste("raw-data_", ymd(today()), ".csv", sep=""))
)

###############################################################################