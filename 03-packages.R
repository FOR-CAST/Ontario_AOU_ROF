####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

Require("data.table")
Require(c("plyr", "dplyr"), upgrade = FALSE) ## ensure plyr loaded before dplyr or there will be problems

Require("PredictiveEcology/reproducible@CopyGenericChange (>= 1.2.6.9011)") ## 2021-03-17
Require("PredictiveEcology/SpaDES.core@rasterToMemoryUpdates (>= 1.0.6.9022)", ## 2021-03-17
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core)

Require("PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9052)", require = FALSE) ## force pemisc and others to be installed correctly

if (FALSE) {
  Require::Require("PredictiveEcology/SpaDES.install (>= 0.0.2)")
  out <- makeSureAllPackagesInstalled(modulePath = "modules")
}

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

Require(fromCRAN, upgrade = FALSE, require = FALSE)
Require(fromGitHub, upgrade = FALSE, require = FALSE)
