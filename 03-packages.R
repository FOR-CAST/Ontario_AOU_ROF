####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

Require(c("plyr", "dplyr"), upgrade = FALSE) ## ensure plyr loaded before dplyr or there will be problems

if (FALSE) {
  remotes::install_github("PredictiveEcology/reproducible@CopyGenericChange") ## 2021-03-17
  remotes::install_github("PredictiveEcology/SpaDES.core@rasterToMemoryUpdates", force = TRUE) ## 2021-03-17
}

Require("PredictiveEcology/SpaDES.core@development (>= 1.0.6.9018)",
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core
Require("PredictiveEcology/pemisc@development", upgrade = FALSE)
Require("PredictiveEcology/fireSenseUtils@development", require = FALSE) ## force pemisc and others to be installed correctly
Require(c("jimhester/archive", "slackr"))

moduleRqdPkgs <- lapply(basename(dir(paths1$modulePath)), function(m) {
  packages(modules = m, paths = paths1$modulePath)
}) %>%
  unlist() %>%
  unname() %>%
  unique() %>%
  sort()

fromCRAN <- names(which(!pemisc::isGitHubPkg(moduleRqdPkgs))) %>%
  sapply(., function(x) strsplit(x, " ")[[1]][[1]]) %>%
  unname() %>%
  unique()

fromGitHub <- names(which(pemisc::isGitHubPkg(moduleRqdPkgs))) %>%
  sapply(., function(x) strsplit(x, " ")[[1]][[1]]) %>%
  unname() %>%
  gsub(pattern = "fireSenseUtils@development", replacement = "fireSenseUtils", x = .) %>%
  gsub(pattern = "fireSenseUtils", replacement = "fireSenseUtils@development", x = .) %>%
  unique()

Require(fromCRAN, upgrade = FALSE)
Require(fromGitHub, upgrade = FALSE) ## TODO: confirm this is what I want
