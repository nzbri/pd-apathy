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

# Small set of first-pass data consistency checks / modifications
sanitise_data <- function(data) {
  nrows = nrow(data)

  # Check ID columns
  cleaned_data <- data %>%
    filter(across(
      matches("subject_id") | matches("session_id"),
      ~ !is.na(.x)
    ))
  # Warn if missing
  nrows_removed = nrows - nrow(cleaned_data)
  if (nrows_removed > 0) {
    warning(paste("Removed", nrows_removed, "rows missing subject/session IDs."))
  }

  return(cleaned_data)
}

###############################################################################
# Load from database, excluding as necessary

participants <- chchpd::import_participants()
participants <- participants %>%
  sanitise_data() %>%
  select(-survey_id) %>%
  mutate(across(c(participant_group, ethnicity), as.factor)) %>%
  mutate(across(c(excluded_from_followup), as.logical))

participants <- participants %>%
  filter(participant_group == "PD")

# -----------------------------------------------------------------------------

sessions <- chchpd::import_sessions()
sessions <- sessions %>%
  sanitise_data() %>%
  group_by(session_id) %>%
  # Tidy up and collate all the different studies
  arrange(study) %>%
  mutate(studies = list(as.character(study))) %>%
  mutate(mri_scan_no = na_if(mri_scan_no, "None")) %>%
  fill(study_excluded, mri_scan_no, .direction = "downup") %>%
  ungroup %>%
  # Remove study-specific variables and duplicate sessions
  mutate(across(c(study_excluded), as.logical)) %>%
  select(
    subject_id, session_id, session_date,
    studies, study_excluded, age, mri_scan_no
  ) %>%
  distinct(.keep_all = TRUE)

# Check we have got unique sessions
stopifnot(
  sessions %>%
    group_by(session_id) %>%
    filter(n() > 1) %>%
    is_empty()
)

# @m-macaskill: Convention for e.g. `study_excluded` is to go off positive
# evidence, as some of the data predates some of the auto-validations that
# generate these fields (i.e. exclude on `FALSE` but not `NA`).
# See e.g. the source for `chchpd::import_sessions()` itself
#sessions %>% count(study_excluded) %>% print(n = Inf)

sessions <- sessions %>%
  filter(!is.na(session_id)) %>%
  filter(is.na(study_excluded) | study_excluded != TRUE) %>%
  filter(!is.na(session_date) & session_date <= lubridate::today())  # Exclude anything only scheduled to happen

# TODO: Sanity check for different `mri_scan_no`?

# -----------------------------------------------------------------------------

neuropsych <- chchpd::import_neuropsyc(concise = TRUE)
neuropsych <- neuropsych %>%
  sanitise_data() %>%
  mutate(across(c(diagnosis, diagnosis_baseline, np_group), as.factor)) %>%
  select(-npi)  # Use data from CSV below explicitly

neuropsych <- neuropsych %>%
  filter(is.na(np_excluded) | np_excluded != TRUE)

# -----------------------------------------------------------------------------

# http://npitest.net/about-npi.html

npi <- read_csv(file.path("..", "Data", "npi_2020-07-13.csv"))
# Remove columns that are irrelevant / already covered by `sessions` and
# `neuropsych` / specific to other domains
npi <- npi %>%
  sanitise_data() %>%
  filter(is.na(session_neuropsych_excluded) | session_neuropsych_excluded != "Y" ) %>%
  select(
    -X1, -subject_id, -record_id, -npi_data_missing, -npi_data_missing_reason,
    -npi_notes, -npi_occ_disruption_total, -npi_session) %>%
  select(-matches("session_[^i][^d]")) %>%
  select(-contains("redcap")) %>%
  select(-matches("npi_[a-f]_")) %>%
  select(-matches("npi_[h-l]_")) %>%
  rename_with(~ gsub("npi_", "NPI_", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("NPI_g_", "NPI_apathy_", .x, fixed = TRUE)) %>%
  rename_with(~ paste("NPI_", .x, sep = ""), !starts_with("NPI_") & !session_id) %>%
  mutate(across(matches("_present"), as.logical))

# -----------------------------------------------------------------------------

medications <- chchpd::import_medications(concise = TRUE)
medications <- medications %>%
  sanitise_data()

# -----------------------------------------------------------------------------

hads <- chchpd::import_HADS(concise = TRUE)
hads <- hads %>%
  sanitise_data()

# -----------------------------------------------------------------------------

# https://www.movementdisorders.org/MDS/MDS-Rating-Scales/MDS-Unified-Parkinsons-Disease-Rating-Scale-MDS-UPDRS.htm

motor_scores <- chchpd::import_motor_scores()
motor_scores <- motor_scores %>%
  sanitise_data() %>%
  rename(UPDRS_motor_score = Part_III)

# Import raw MDS-UPDRS scores for apathy question
# Q1.5 is apathy
mds_updrs = chchpd::import_MDS_UPDRS(concise = FALSE)
mds_updrs <- mds_updrs %>%
  sanitise_data() %>%
  mutate(
    UPDRS_apathy = factor(
      Q1_5,
      levels = c(0, 1, 2, 3, 4),
      labels = c("Normal", "Slight", "Mild", "Moderate", "Severe"),
      ordered = TRUE
    )
  ) %>%
  select(session_id, UPDRS_apathy, UPDRS_date)

# Note that some subjects have data that predates the MDS-UPDRS (i.e. from the
# 1987 UPDRS). However, we ignore those for now as that version does not seem
# to contain any apathy-specific questions.
# old_updrs = chchpd::import_old_UPDRS()

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
  left_join(motor_scores, by = "session_id") %>%
  left_join(mds_updrs, by = "session_id")

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

dim(full_data)
full_data$subject_id %>% unique %>% length
full_data$session_id %>% unique %>% length
full_data %>% filter(full_assessment) %>% pull(session_id) %>% unique %>% length
#full_data %>% count(study) %>% print(n = Inf)
#full_data %>% count(study_group) %>% print(n = Inf)
full_data %>% count(diagnosis_baseline) %>% print(n = Inf)
full_data %>% count(diagnosis) %>% print(n = Inf)
full_data %>% summarise(across(.fns = ~ sum(is.na(.x)))) %>% print(width = Inf)

###############################################################################
# Save to file

date_string = lubridate::ymd(lubridate::today())

saveRDS(
  full_data,
  file.path("..", "Data", paste("raw-data_", date_string, ".rds", sep = ""))
)

write_csv(
  full_data,
  file.path("..", "Data", paste("raw-data_", date_string, ".csv", sep = ""))
)

col_types <- full_data %>%
  summarise(across(.fns = type_sum)) %>%
  gather(col_name, col_type)
write_csv(
  col_types,
  file.path("..", "Data", paste("raw-data_", date_string, "_types.csv", sep = ""))
)

###############################################################################