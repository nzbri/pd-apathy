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

# initialise_environment.R

###############################################################################

required_packages = c(
  "brms", "chchpd", "flextable", "ggalluvial", "glmnet", "gtsummary", "rstan",
  "tidyverse", "VIM"
)

# Check all present and correct
check_packages <- function(required_packages) {
  missing_packages <-
    required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if (length(missing_packages) != 0) {
    stop(paste("Missing packages:", paste(missing_packages, collapse = ", ")))
  }
  return()
}
check_packages(required_packages)

###############################################################################
# Load as necessary

# -----------------------------------------------------------------------------
# Installation of CHCHPD package: https://github.com/nzbri/chchpd
#install.packages("devtools")
#devtools::install_github("nzbri/chchpd")
#library(chchpd)

# Set up authentication
#chchpd::google_authenticate(email = "example@nzbri.org", use_server = TRUE)

#ls("package:chchpd")
#environment(google_authenticate)
#?chchpd::import_MRI / ?import_MRI

# -----------------------------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)  # Cache compiled models

# -----------------------------------------------------------------------------

#install.packages("brms")
library(brms)
#methods(class = "brmsfit")

# -----------------------------------------------------------------------------

library(tidyverse)
library(ggalluvial)

###############################################################################
# Print summary of environment

print(paste("Current directory:", getwd()))

print("Core packages:")
print(
  required_packages %>%
    map(packageDescription, fields = c("Package", "Version")) %>%
    pmap(c) %>%
    as.data.frame()
)

#print(.packages())

###############################################################################

rm(required_packages, check_packages)

###############################################################################
