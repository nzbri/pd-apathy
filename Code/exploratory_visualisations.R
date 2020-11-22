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

# exploratory_visualisations.R

###############################################################################

source("initialise_environment.R")

###############################################################################

full_data <- readRDS(
  file.path("..", "Data", "raw-data_2020-11-10.rds")
)

###############################################################################
# (Optional) Read from CSV using type hints

# https://stackoverflow.com/a/44581290
types <- as_tibble(list(
    col_type = c("chr", "int", "dbl", "lgl", "fct", "ord", "date"),
    code = c("c", "i", "d", "l", "f", "f", "D")
  )
)

col_types <- read_csv(
  file.path("..", "Data", "raw-data_2020-11-10_types.csv"),
  col_types = "cc"
)
col_types <- col_types %>%
  left_join(types, by = "col_type") %>%
  summarise(col_types = str_c(code, collapse = "")) %>%
  unlist(use.names = FALSE)

full_data <- read_csv(
  file.path("..", "Data", "raw-data_2020-11-10.csv"),
  col_types = col_types
)

###############################################################################
# Preprocess data

# Add some useful extra timing info
full_data <- full_data %>%
  mutate(
    years_from_diagnosis = age - diagnosis_age,
    years_from_symptom_onset = age - symptom_onset_age,
    years_between_symptoms_and_diagnosis = diagnosis_age - symptom_onset_age
  )

# Reorder and relabel diagnosis
full_data <- full_data %>%
  mutate(diagnosis = factor(
    diagnosis,
    levels = c(NA, "PD-N", "PD-MCI", "PDD"),
    labels = c(NA, "PD-N", "PD-MCI", "PD-D"),
    exclude = NULL  # i.e. include NA as a level, helps with plot ordering
  ))

# Several `_present` columns are coded as "Yes"/"No" factors for plotting

# Add a simple yes/no for presence of apathy
full_data <- full_data %>%
  mutate(
    apathy_present = as.integer(NPI_apathy_present) + 1,
    apathy_present.interp = apathy_present
  ) %>%
  group_by(subject_id) %>%  # within each subject:
  arrange(session_date) %>%  # order by session
  fill(apathy_present.interp, .direction = "down") %>%
  mutate(across(starts_with("apathy_present"), ~ replace_na(.x, 0))) %>%
  mutate(apathy_present.worst_to_date = cummax(apathy_present)) %>%
  ungroup() %>%
  mutate(across(
    starts_with("apathy_present"),
    ~ factor(.x, levels = c(0, 1, 2), labels = c("Unknown", "No", "Yes"))
  ))
#full_data %>% select(subject_id, session_date, starts_with("apathy_present")) %>% print(n = 20)

colours.apathy_present = c('#7F7F7F', '#1F77B4', '#FF7F0E')  # grey, blue, orange: https://matplotlib.org/users/dflt_style_changes.html

# For convenience
full_data <- full_data %>%
  arrange(subject_id, session_date)

###############################################################################
# Pull out baseline sessions

baseline_data <- filter(full_data, session_number == 1)

# Check we don't have any obviously weird baseline sessions
stopifnot(all(
  (baseline_data %>% pull(years_from_baseline)) == 0.0
))
stopifnot(all(
  (
    full_data %>%
      filter(session_number > 1) %>%
      pull(years_from_baseline)
  ) > 0.0
))

###############################################################################
# Useful functions

save_plot <- function(plt, filename, ..., width = 6, height = 4, units = "in") {
  ggsave(
    file.path("..", "Figures", paste(filename, ".pdf", sep = "")),
    plt, ..., width = width, height = height, units = units
  )
  ggsave(
    file.path("..", "Figures", paste(filename, ".jpg", sep = "")),
    plt, ..., width = width, height = height, units = units, dpi = "screen"
  )
}

###############################################################################
# Key summaries of data collection process itself

# When was the data collected
plt <- full_data %>%
  mutate(session_date = lubridate::floor_date(session_date, unit = "year")) %>%
  ggplot(aes(x = session_date, fill = (session_number == 1))) +
  geom_bar(position = "stack") +
  scale_fill_discrete(
    breaks = c(TRUE, FALSE),
    labels = c("Baseline", "Follow-up")
  ) +
  theme_light() +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0.0, 1.0),
    legend.background = element_rect(colour = "black")
  ) +
  labs(
    x = "Session year",
    y = "Number of sessions",
    fill = "Session type",
    title = paste(dim(full_data)[1], "sessions")
  )
print(plt)
save_plot(plt, "session-type_v_year")

# How many follow-ups per subject
plt <- full_data %>%
  #mutate(date_baseline = lubridate::floor_date(date_baseline, unit = "year")) %>%
  group_by(subject_id) %>%
  filter(session_number == max(session_number)) %>%
  ungroup() %>%
  ggplot(aes(x = session_number)) +  # fill = ordered(date_baseline)
  geom_bar(position = "stack") +
  theme_light() +
  labs(
    x = "Total number of sessions",
    y = "Number of patients"
  )
print(plt)
save_plot(plt, "session-number")

# How far after baseline did the follow-ups occur?
plt <- full_data %>%
  filter(session_number != 1) %>%
  ggplot(aes(x = years_from_baseline)) +
  geom_histogram(
    position = "stack", binwidth = 1.0, boundary = 0.0, color = "white"
  ) +
  theme_light() +
  labs(
    x = "Years from baseline",
    y = "Number of sessions (excluding baselines)"
  )
print(plt)
save_plot(plt, "years-from-baseline")

###############################################################################
# Key properties of 'raw' data, independent of apathy

#Grouping?
# sex
# ethnicity
# diagnosis

#Session vars?
# age
# MoCA
# global_z
# UPDRS_motor_score
# LED
# NPI_total
# NPI_apathy_score
# HADS_anxiety
# HADS_depression
# Hoehn_Yahr

#Baseline vars?
# symptom_onset_age
# diagnosis_age
# global_z_baseline
# education

for (dataset in list(
  list(data = full_data, at = "session", of = "sessions"),
  list(data = baseline_data, at = "baseline", of = "patients")
)) {
  print(dataset$at)

  # Age / sex
  bw = 1.0
  plt <- dataset$data %>%
    ggplot(aes(age, fill = sex)) +
    geom_histogram(
      position = "stack", binwidth = bw, boundary = 0.0, color = "white", alpha = 0.5
    ) +
    stat_density(
      geom = "line", position = "identity", size = 1.0, show.legend = FALSE,
      aes(y = bw * ..count.., colour = ..fill..)  # https://stackoverflow.com/a/37404727
    ) +
    scale_x_continuous(limits = c(35.0, 95.0)) +
    labs(
      x = paste("Age at", dataset$at),
      y = paste("Number of", dataset$of),
      fill = "Sex",
      title = paste(sum(!is.na(dataset$data$age)), dataset$of)
    ) +
    theme_light()
  print(plt)
  save_plot(plt, paste("age_at_", dataset$at, sep = ""))

  # MoCA / diagnosis
  bw = 1.0
  plt <- dataset$data %>%
    ggplot(aes(MoCA, fill = diagnosis)) +
    #geom_bar(color = "white", alpha = 0.5) +
    geom_histogram(
      position = "stack", binwidth = bw, center = 0.0, color = "white", alpha = 0.5
    ) +
    stat_density(
      geom = "line", position = "identity", size = 1.0, show.legend = FALSE,
      aes(y = bw * ..count.., colour = ..fill..)  # https://stackoverflow.com/a/37404727
    ) +
    scale_x_continuous(limits = c(0.0, 31.0)) +
    labs(
      x = paste("MoCA at", dataset$at),
      y = paste("Number of", dataset$of),
      fill = "Diagnosis",
      title = paste(sum(!is.na(dataset$data$MoCA)), dataset$of)
    ) +
    theme_light()
  print(plt)
  save_plot(plt, paste("MoCA_at_", dataset$at, sep = ""))

  # Motor scores / diagnosis
  bw = 2.0
  plt <- dataset$data %>%
    ggplot(aes(UPDRS_motor_score, fill = diagnosis)) +
    geom_histogram(
      position = "stack", binwidth = bw, boundary = 0.0, color = "white", alpha = 0.5
    ) +
    stat_density(
      geom = "line", position = "identity", size = 1.0, show.legend = FALSE,
      aes(y = bw * ..count.., colour = ..fill..)  # https://stackoverflow.com/a/37404727
    ) +
    scale_x_continuous(limits = c(0.0, 100.0)) +
    labs(
      x = paste("UPDRS (Part III) motor score at", dataset$at),
      y = paste("Number of", dataset$of),
      fill = "Diagnosis",
      title = paste(sum(!is.na(dataset$data$UPDRS_motor_score)), dataset$of)
    ) +
    theme_light()
  print(plt)
  save_plot(plt, paste("motor-scores_at_", dataset$at, sep = ""))

  # Cognitive scores / diagnosis
  bw = 0.2
  plt <- dataset$data %>%
    ggplot(aes(global_z, fill = diagnosis)) +
    geom_histogram(
      position = "stack", binwidth = bw, center = 0.0, color = "white", alpha = 0.5
    ) +
    stat_density(
      geom = "line", position = "identity", size = 1.0, show.legend = FALSE,
      aes(y = bw * ..count.., colour = ..fill..)  # https://stackoverflow.com/a/37404727
    ) +
    scale_x_continuous(limits = c(-3.5, 3.5)) +
    labs(
      x = paste("Global cognitive z-score at", dataset$at),
      y = paste("Number of", dataset$of),
      fill = "Diagnosis",
      title = paste(sum(!is.na(dataset$data$global_z)), dataset$of)
    ) +
    theme_light()
  print(plt)
  save_plot(plt, paste("cognitive-scores_at_", dataset$at, sep = ""))

  # Medication / diagnosis
  bw = 100.0
  plt <- dataset$data %>%
    ggplot(aes(LED, fill = diagnosis)) +
    geom_histogram(
      position = "stack", binwidth = bw, boundary = 0.0, color = "white", alpha = 0.5
    ) +
    stat_density(
      geom = "line", position = "identity", size = 1.0, show.legend = FALSE,
      aes(y = bw * ..count.., colour = ..fill..)  # https://stackoverflow.com/a/37404727
    ) +
    scale_x_continuous(limits = c(0.0, 3000.0)) +
    labs(
      x = paste("Medication (LED) at", dataset$at),
      y = paste("Number of", dataset$of),
      fill = "Diagnosis",
      title = paste(sum(!is.na(dataset$data$LED)), dataset$of)
    ) +
    theme_light()
  print(plt)
  save_plot(plt, paste("medication_at_", dataset$at, sep = ""))

}

###############################################################################
# Simple look at longitudinal measures

# Global-z / diagnosis v. age
plt <- full_data %>%
  #drop_na(global_z) %>%
  #group_by(subject_id) %>% # within each subject:
  #arrange(subject_id, session_date) %>% # order by session
  #mutate(diagnosis = lead(diagnosis)) %>%
  #ungroup() %>%
  #filter(diagnosis == "PD-D") %>%
  ggplot(aes(
    x = age,
    y = global_z,
    group = subject_id,
    colour = diagnosis
  )) +
  geom_line(alpha = 0.75, size = 0.25) +
  geom_point(size = 0.5) +
  geom_smooth(
    method = glm, formula = y ~ poly(x, 2) + 1,
    aes(group = NULL, colour = NULL), colour = "black"
  ) +
  labs(
    x = "Age", y = "Global cognitive z-score", colour = "Diagnosis",
    title = paste(sum(!is.na(full_data$global_z)), "sessions")
    ) +
  theme_light()
print(plt)
save_plot(plt, "cognitive-scores_v_age", width = 10.0, height = 4.0)

# Motor scores / diagnosis v. age
plt <- full_data %>%
  ggplot(aes(
    x = age,
    y = UPDRS_motor_score,
    group = subject_id,
    colour = diagnosis
  )) +
  geom_line(alpha = 0.75, size = 0.25) +
  geom_point(size = 0.5) +
  geom_smooth(
    method = glm, formula = y ~ poly(x, 2) + 1,
    method.args = list(family = Gamma(link = "inverse")),
    mapping = aes(group = NULL, colour = NULL), colour = "black"
  ) +
  labs(
    x = "Age", y = "UPDRS (Part III) motor score", colour = "Diagnosis",
    title = paste(sum(!is.na(full_data$UPDRS_motor_score)), "sessions")
  ) +
  theme_light()
print(plt)
save_plot(plt, "motor-scores_v_age", width = 10.0, height = 4.0)

###############################################################################
# Do the demographics of recruited subjects change over time?

# Plot different variables v baseline date
for (variable in list(
  list(name = "diagnosis_age", description = "Age at diagnosis", family = Gamma(link = "log")),
  list(name = "age", description = "Age at baseline", family = Gamma(link = "log")),
  list(name = "years_between_symptoms_and_diagnosis", description = "Years between symptom onset and diagnosis", family = gaussian),
  list(name = "years_from_diagnosis", description = "Years between diagnosis and baseline", family = gaussian),
  list(name = "global_z", filename = "cognitive-scores", description = "Global cognitive z-score", family = gaussian),
  list(name = "UPDRS_motor_score", filename = "motor-scores", description = "UPDRS (Part III) motor score", family = Gamma(link = "log"))
)) {
  print(variable$description)

  plt <- baseline_data %>%
    ggplot(
      aes_string(x = "date_baseline", y = variable$name)
    ) +
    geom_point(
      aes(colour = diagnosis),
      # position = position_jitter(w = 0.0, h = 0.2),
      # colour = "grey",
      size = 0.5
    ) +
    geom_smooth(
      method = glm, formula = y ~ poly(x, 2),
      method.args = list(family = variable$family),
      colour = "black"
    ) +
    theme_light() +
    labs(
      x = "Date of baseline measurement",
      y = variable$description,
      title = paste(sum(!is.na(baseline_data[variable$name])), "patients")
    )
  print(plt)

  if (!exists("filename", where = variable)) {
    variable$filename <- variable$name
  }
  save_plot(
    plt, paste(gsub("_", "-", variable$filename), "_v_baseline-date", sep = "")
  )

}

###############################################################################
# Look at whether there are patterns in 'missingness' of key measures

for (variable in list(
  list(name = "NPI_apathy_present", description = "NPI apathy", filename = "npi"),
  list(name = "UPDRS_apathy", description = "UPDRS apathy", filename = "updrs")
)) {

  # Missing by year?
  plt <- full_data %>%
    mutate(session_date = lubridate::floor_date(session_date, unit = "year")) %>%
    #filter(full_assessment == TRUE) %>%
    ggplot(aes_string(
      x = "session_date", fill = paste("!is.na(", variable$name, ")", sep = "")
    )) +
    geom_bar(position = "stack") +
    scale_fill_discrete(
      breaks = c(FALSE, TRUE),
      labels = c("Missing", "Present")
    ) +
    theme_light() +
    theme(
      legend.position = c(0.05, 0.95),
      legend.justification = c(0.0, 1.0),
      legend.background = element_rect(colour = "black")
    ) +
    labs(
      x = "Session year",
      y = "Number of sessions",
      fill = variable$description,
      title = paste(dim(full_data)[1], "sessions")
    )
  print(plt)
  save_plot(plt, paste(variable$filename, "-presence_v_year", sep = ""))

  # Missing by session number?
  plt <- full_data %>%
    #filter(full_assessment == TRUE) %>%
    ggplot(aes_string(
      x = "session_number", fill = paste("!is.na(", variable$name, ")", sep = "")
    )) +
    geom_bar(position = "stack") +
    scale_fill_discrete(
      breaks = c(FALSE, TRUE),
      labels = c("Missing", "Present")
    ) +
    theme_light() +
    theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c(1.0, 1.0),
      legend.background = element_rect(colour = "black")
    ) +
    labs(
      x = "Session number",
      y = "Number of sessions",
      fill = variable$description,
      title = paste(dim(full_data)[1], "sessions")
    )
  print(plt)
  save_plot(plt, paste(variable$filename, "-presence_v_session", sep = ""))

}

###############################################################################
# Cross-tabulate NPI and MDS-UPDRS scores

# table(full_data$NPI_apathy_present, full_data$UPDRS_apathy, useNA = "always")

for (plot_config in list(
  list(position = "fill", y_label = "Proportion"),
  list(position = "stack", y_label = "Number")
)) {

  plt <- full_data %>%
    mutate(NPI_apathy_present = factor(
      NPI_apathy_present,
      levels = c(NA, FALSE, TRUE),
      labels = c(NA, "No", "Yes"),
      exclude = NULL  # i.e. include NA as a level, helps with plot ordering
    )) %>%
    ggplot(aes(x = UPDRS_apathy, fill = NPI_apathy_present)) +
    geom_bar(position = plot_config$position, na.rm = FALSE) +
    theme_light() +
    labs(
      x = "UPDRS apathy",
      y = paste(plot_config$y_label, "of sessions"),
      fill = "NPI apathy",
      title = paste(dim(full_data)[1], "sessions")
    )
  print(plt)
  save_plot(plt, paste("apathy_UPDRS-v-NPI_", plot_config$position, sep = ""))

}

###############################################################################
# Distributions of apathy scores

plt <- full_data %>%
  ggplot(aes(ordered(NPI_apathy_score, levels = 0:12), fill = NULL)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  labs(x = "NPI apathy score (frequency × severity)", y = "Number of sessions") +
  theme_light()
print(plt)
#save_plot(plt, "npi-apathy-scores_at_session")

plt <- full_data %>%
  ggplot(aes(UPDRS_apathy, fill = NULL)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  labs(x = "MDS-UPDRS apathy rating", y = "Number of sessions") +
  theme_light()
print(plt)
#save_plot(plt, "updrs-apathy-scores_at_session")

###############################################################################
# "Matchstick" plots of progression

# Apathy status v. years from baseline
plt <- full_data %>%
  # Calculate how long each subject has been followed up for in total
  group_by(subject_id) %>%
  arrange(session_date) %>%
  mutate(latest_years_from_baseline = max(years_from_baseline)) %>%
  ungroup() %>%
  # And order subjects based on that
  arrange(desc(latest_years_from_baseline)) %>%
  mutate(case_num = match(subject_id, unique(subject_id))) %>%
  filter(latest_years_from_baseline > 0.0) %>%
  # And then plot!
  ggplot(aes(
    x = years_from_baseline,
    y = case_num,
    group = subject_id
  )) +
  geom_line(aes(colour = apathy_present.interp), alpha = 0.75, size = 0.25) +
  geom_point(aes(colour = apathy_present), size = 0.25) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_y_discrete(breaks = NULL) +
  scale_colour_manual(values = colours.apathy_present) +
  labs(
    x = "Years from baseline",
    y = "Patients (at least one follow-up)",
    colour = "Apathetic"
  ) +
  theme_minimal() +
  theme(
    axis.ticks = element_line(colour = "lightgrey"),
    axis.ticks.y = element_blank(),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1.0, 1.0),
    legend.background = element_rect(fill = "white")
  )
print(plt)
save_plot(plt, "apathy_v_years-from-baseline", width = 6.0, height = 8.0)

# Apathy status v. session date
plt <- full_data %>%
  # Order subjects based on date of baseline
  arrange(desc(date_baseline)) %>%
  mutate(case_num = match(subject_id, unique(subject_id))) %>%
  # And then plot!
  ggplot(aes(
    x = session_date,
    y = case_num,
    group = subject_id
  )) +
  geom_line(aes(colour = apathy_present.interp), alpha = 0.75, size = 0.25) +
  geom_point(aes(colour = apathy_present), size = 0.25) +
  scale_y_discrete(breaks = NULL) +
  scale_colour_manual(values = colours.apathy_present) +
  labs(
    x = "Session date",
    y = "Patients",
    colour = "Apathetic"
  ) +
  theme_minimal() +
  theme(
    axis.ticks = element_line(colour = "lightgrey"),
    axis.ticks.y = element_blank(),
    legend.position = c(0.05, 0.05),
    legend.justification = c(0.0, 0.0),
    legend.background = element_rect(fill = "white")
  )
print(plt)
save_plot(plt, "apathy_v_session-date", width = 6.0, height = 8.0)

###############################################################################
# Alluvial plots of changing apathy status

status_by_year = full_data %>% select(subject_id) %>% unique()
delta = 2.0  # years
for (years in seq(from = 0.0, to = 14.0, by = delta)) {
  print(years)

  # Pull out the status within the current time window
  current_status <- full_data %>%
    # Extract all sessions with recorded apathy within window of past `delta` years
    filter(
      (years_from_baseline <= years) & (years_from_baseline > years - delta)
    ) %>%
    # And take the most recent (favouring positive evidence)
    group_by(subject_id) %>%
    slice_max(years_from_baseline - delta * (apathy_present == "Unknown")) %>%
    ungroup() %>%
    # Record status (as character as we add more categories)
    mutate(current_status = as.character(apathy_present)) %>%
    mutate(current_status = if_else(
      (current_status == "No") & (apathy_present.worst_to_date == "Yes"),
      "Remission",
      current_status
    )) %>%
    select(subject_id, current_status)

  # And see if we can work out why data is missing
  current_status <-
    left_join(full_data, current_status, by = "subject_id") %>%
    # Does a subject have data at a later date?
    group_by(subject_id) %>%
    mutate(subsequent_sessions_present = (FU_latest >= years)) %>%
    slice_head() %>%  # Back to one session per subject
    ungroup() %>%
    # Use that info to recode missing data
    mutate(
      "apathy_status.{years}_years" := if_else(
        is.na(current_status),
        if_else(
          subsequent_sessions_present,
          "Unknown",
          if_else(dead, "Deceased", "No follow-ups"),
        ),
        current_status
      )
    ) %>%
    select(subject_id | contains("apathy_status"))
  print(current_status %>% count(across(contains("apathy_status"))))

  status_by_year <- status_by_year %>%
    left_join(current_status, by = "subject_id")

}

status_by_year <- status_by_year %>%
  mutate(across(
    contains("apathy_status"),
    ~ factor(
      .x,
      levels = c("Yes", "Remission", "No", "Unknown", "Deceased", "No follow-ups"),
      labels = c("Y", "R", "N", "U", "D", NA),  # , "?",
      exclude = NULL  # i.e. include NA as a level, helps with plot ordering
    )
  ))

#status_by_year <- rownames_to_column(as_tibble(sapply(select(status_by_year, contains("apathy_status")), function(x) table(x)), rownames = NA))
#status_by_year <- status_by_year %>%
#  count(across(contains("apathy_status")))

# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
status_by_year %>%
  count(across(contains("apathy_status"))) %>%
  to_lodes_form(axes = 1:6, weight = n) %>%
  ggplot(aes(
    x = x,
    y = n,
    alluvium = alluvium,
    stratum = stratum,
    fill = stratum,
    label = stratum
  )) +
  scale_x_discrete(expand = c(.1, .1), labels = seq(0,10,2)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  scale_fill_discrete(labels = c("Yes", "Remission", "No", "Unknown", "Deceased", "No follow-ups")) +
  scale_y_reverse() +
  #theme(legend.position = "none") +
  labs(
    x = "Years from baseline",
    y = "Patients",
    fill = "Apathy status"
  ) +
  theme_minimal()

###############################################################################