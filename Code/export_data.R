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
# Load from database

participants <- chchpd::import_participants()
sessions <- chchpd::import_sessions() # from_study = 'Follow-up'
neuropsych <- chchpd::import_neuropsyc(concise = TRUE)
medications <- chchpd::import_medications(concise = TRUE)
hads <- chchpd::import_HADS(concise = TRUE)
updrs <- chchpd::import_motor_scores()
# Using namespace qualifiers doesn't seem to be an R thing

# bind the records together, linked by subject or session IDs:
full_data <-
  right_join(participants, sessions, by = 'subject_id') %>%
  left_join(neuropsych, by = 'session_id') %>%
  left_join(medications, by = 'session_id') %>%
  left_join(hads, by = 'session_id') %>%
  left_join(updrs, by = 'session_id')
colnames(full_data)

###############################################################################
# Inclusion / exclusion criteria

full_data <-
  full_data %>%
  filter(participant_group == 'PD') %>%
  filter(!is.na(date_baseline)) %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(education))
# study == 'Follow-up'
# diagnosis_baseline != 'PDD'
# excluded_from_followup == FALSE
full_data$subject_id %>% unique %>% length
full_data %>% count(study) %>% print(n = Inf)
full_data %>% count(study_group) %>% print(n = Inf)
full_data %>% count(diagnosis_baseline) %>% print(n = Inf)
full_data %>% count(diagnosis) %>% print(n = Inf)

###############################################################################
# Save to file

write_csv(
  full_data,
  file.path("..", "Data", paste("raw-data_", ymd(today()), ".csv", sep=""))
)

###############################################################################