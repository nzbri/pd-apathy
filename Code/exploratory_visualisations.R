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
  file.path("..", "Data", "raw-data_2020-10-27.rds")
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
  file.path("..", "Data", "raw-data_2020-10-27_types.csv"),
  col_types = "cc"
)
col_types <- col_types %>%
  left_join(types, by = "col_type") %>%
  summarise(col_types = str_c(code, collapse = "")) %>%
  unlist(use.names = FALSE)

full_data <- read_csv(
  file.path("..", "Data", "raw-data_2020-10-27.csv"),
  col_types = col_types
)

###############################################################################

ggplot(full_data, aes(years_from_baseline)) +
  geom_histogram(binwidth = 1.0, boundary = 0.0, color="white") +
  theme_light()

ggplot(full_data, aes(apathy_score)) +
  geom_histogram(binwidth = 1, center = 0, color="white") +
  theme_light()

###############################################################################

apathy <- full_data %>%
  rename(apathy_yn = npi_g_present,
         apathy_freq = npi_g_frequency,
         apathy_severity = npi_g_severity) %>%
  mutate(apathy_score = apathy_freq * apathy_severity) %>%
  mutate(apathy_status = replace_na(as.integer(apathy_yn) + 1, 0)) %>%
  arrange(subject_id, session_date) %>% # order by session
  group_by(subject_id) %>% # within each subject:
  mutate(apathy_status_interp = apathy_yn) %>%
  fill(apathy_status_interp, .direction = "down") %>%
  mutate(apathy_status_interp = replace_na(as.integer(apathy_status_interp) + 1, 0)) %>%
  mutate(apathy_status_worst_to_date = cummax(apathy_status)) %>%
  ungroup() %>%
  mutate(apathy_yn = factor(apathy_yn, labels = c("No", "Yes"))) %>%
  mutate(across(
    c(apathy_status, apathy_status_interp, apathy_status_worst_to_date),
    ~ factor(.x, levels = c(0,1,2), labels = c("Unkown", "No", "Yes"))))

apathy %>% select(subject_id, session_date, apathy_yn, apathy_status, apathy_status_interp, apathy_status_worst_to_date) %>% print(n = 20)

# calculate values relative to baseline, conversion status, etc:
apathy <- apathy %>%
  arrange(subject_id, session_date) %>% # order by session
  group_by(subject_id) %>% # within each subject:
  mutate(session_order = as.integer(rank(session_date)), 
         baseline_date = first(session_date),
         baseline_status = first(cognitive_status),
         worst_status = max(cognitive_status, na.rm = TRUE),
         best_status = min(cognitive_status, na.rm = TRUE),
         converted_from_baseline = 
           as.numeric(baseline_status) < as.numeric(worst_status),
         converted_from_baseline = 
           factor(converted_from_baseline, 
                  levels = c(FALSE, TRUE),
                  labels = c('Non-converter', 'Converter')),
         reverted_from_baseline = 
           as.numeric(baseline_status) > as.numeric(best_status),
         years_from_baseline = 
           difftime(session_date, baseline_date, unit='days')/365.25,
         years_from_baseline = 
           round(as.numeric(years_from_baseline), digits = 2),
         years_before_latest_NP1 = 
           difftime(last(session_date), session_date, unit='days')/365.25,
         years_before_latest_NP1 = 
           round(as.numeric(years_before_latest_NP1), digits = 2),
         years_from_diagnosis = round(age - diagnosis_age, digits = 1),
         latest_year = max(years_from_baseline),
         latest_session = max(session_order)) %>% 
  # to make line colours match the destination point rather than the origin 
  # point, need to use a diagnosis variable that 'leads' the current diagnosis
  # for each person by one step:
  mutate(apathy_yn_lead = lead(apathy_yn)) %>% 
  # this makes the last value for each person NA, which can cause isues. 
  # fill() replaces them with the previous diagnosis:
  fill(apathy_yn_lead) %>% 
  # make a variable that will allow a graph line to change colour permanently
  # once a person has become apathetic, even if they subsequently revert to 
  # non-apathy. ie it is 0 until the first 'yes' apathy score, and 1 thereafter.
  mutate(apathy_cumsum = cumsum(as.numeric(apathy_yn) - 1)) %>% 
  mutate(apathy_converted = 
           case_when(apathy_cumsum == 0 ~ 0,
                     TRUE ~ 1)) %>% # 1 after any 1st apathy "yes"
  mutate(apathy_converted = 
           factor(apathy_converted, levels = c(0,1),
                  labels = c('No', 'Yes'))) %>% 
  ungroup()

orange_blue = c('#D55E00', '#0072B2')
colours = c('#7F7F7F', '#1F77B4', '#FF7F0E')  # grey, blue, orange: https://matplotlib.org/users/dflt_style_changes.html

# graph summarising each subjects study timeline
# (matchstick-like plot):
plt <- apathy %>%
  group_by(subject_id) %>% # within each subject,
  arrange(session_date) %>%
  # calculate latest session:
  mutate(latest_years_from_baseline = max(years_from_baseline)) %>% 
  ungroup() %>% # now across all subjects,
  arrange(latest_years_from_baseline, subject_id) %>% 
  mutate(case_num = match(subject_id, unique(subject_id)),
         rev_case_num = max(case_num) - case_num) %>% 
  ggplot(aes(y = rev_case_num,
             x = years_from_baseline,
             group = subject_id)) + 
  geom_line(aes(color = apathy_status_worst_to_date), alpha = 0.75, size = 0.25) + 
  geom_point(aes(color = apathy_status), size = 0.2) + 
  scale_colour_manual(values = colours, name = 'Apathetic') +
  xlab('Years from baseline') +
  theme(axis.title.y=element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = seq(0, 12 ,2)) + 
  scale_y_discrete(breaks=NULL) +
  theme(legend.position = c(0.83, 0.87), 
        legend.background = element_rect(fill = 'white'),
        axis.ticks = element_line(colour = 'lightgrey')) + 
  theme_minimal()

print(plt)
ggsave("matchstick.pdf", plt, width = 6, height = 12, units = "in")

###############################################################################