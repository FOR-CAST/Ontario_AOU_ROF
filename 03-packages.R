####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

Require(c("plyr", "dplyr"), upgrade = FALSE) ## ensure plyr loaded before dplyr or there will be problems
Require("PredictiveEcology/SpaDES.core@development",
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core
Require("PredictiveEcology/pemisc@development", upgrade = FALSE)
Require("slackr")

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
