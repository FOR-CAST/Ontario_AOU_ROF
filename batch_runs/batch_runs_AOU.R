fireModel <- "scfm" ## "fireSense"
nrvType <- "HRV" ## "FRV"

fit <- FALSE

if (tolower(fireModel) == "firesense") {
  # sAN <- "ON_AOU_6.1"
  # sAN <- "ON_AOU_6.2"
  # sAN <- "ON_AOU_6.5"
  sAN <- "ON_AOU_6.6"
} else {
  sAN <- "ON_AOU"
}

if (nrvType == "FRV") {
  gcm <- "CanESM5"
  # gcm <- "CNRM-ESM2-1"

  # ssp <- 245
  # ssp <- 370
  ssp <- 585
} else {
  gcm <- NULL
  ssp <- NULL
}

Nstart <- 31L
Nreps <- 50L

delay <- 5L * 3600L

reps2run <- if (isTRUE(fit)) {
  Nstart ## fitting loops already in script
} else {
  Nstart:Nreps
}

lapply(reps2run, function(rep) {
  cmd <- sprintf("screen -d -m -S %s_%02d Rscript -e", sAN, rep)
  cmd <- paste(cmd, "'")

  cmd <- paste(cmd, sprintf(".rep <- %d;", rep))
  cmd <- paste(cmd, sprintf(".studyAreaName <- \"%s\";", sAN))
  cmd <- paste(cmd, sprintf(".fireModel <- \"%s\";", fireModel))

  if (nrvType == "FRV") {
    cmd <- paste(cmd, sprintf(".climateGCM <- \"%s\"; .climateSSP <- \"%d\";", gcm, ssp))
  }

  if (isTRUE(fit)) {
    cmd <- paste(cmd, sprintf(".mode <- c(\"production\", \"fit\");"))
  } else {
    cmd <- paste(cmd, ".mode <- \"production\";")
  }

  cmd <- paste(cmd, sprintf("Sys.sleep(%d);", delay))
  cmd <- paste(cmd, sprintf("source(\"00-main.R\")"))
  cmd <- paste(cmd, "'")
  system(cmd, intern = TRUE)
  Sys.sleep(5)
})

rm(delay, fireModel, fit, gcm, Nstart, Nreps, nrvType, reps2run, sAN, ssp)
