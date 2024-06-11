.ncores <- min(parallelly::availableCores(constraints = "connections") / 2, 32L)

.nodename <- SpaDES.config::machine()
.user <- SpaDES.config::user()

## allow setting run context info from outside this script (e.g., bash script) ----------------

if (exists(".mode", .GlobalEnv)) {
  stopifnot(all(.mode %in% c("development", "fit", "postprocess", "production")))
} else {
  .mode <- if (interactive()) "development" else "production"
  # .mode <- "postprocess"
}

if (exists(".nrvType", .GlobalEnv)) {
  .nrvType <- tolower(.nrvType)
  stopifnot(.nrvType %in% c("hrv", "frv"))
} else {
  .nrvType <- tolower("hrv")
}

if (exists(".climateGCM", .GlobalEnv)) {
  stopifnot(.climateGCM %in% c("CanESM5", "CNRM-ESM2-1"))
} else {
  .climateGCM <- "CanESM5"
}

if (exists(".climateSSP", .GlobalEnv)) {
  stopifnot(.climateSSP %in% c(245, 370, 585))
} else {
  .climateSSP <- 370
}

if (exists(".rep", .GlobalEnv)) {
  .rep <- if ("postprocess" %in% .mode) NA_integer_ else as.integer(.rep)
} else {
  .rep <- if ("postprocess" %in% .mode) NA_integer_ else 1L
}

if (exists(".res", .GlobalEnv)) {
  stopifnot(.res %in% c(125, 250))
} else {
  .res <- 250
}

if (!exists(".studyAreaName", .GlobalEnv)) {
  .studyAreaName <- "ON_AOU_1" ## FRTs in AOU: 1, 5 (small parts of 2, 6, 7)
  #.studyAreaName <- "ON_ROF_5" ## FRTs in ROF: 1, 5 (small parts of 2)
  #.studyAreaName <- "ON_ROF_shield" ## ecozones in ROF: Boreal Shield, Hudson Plain
  #.studyAreaName <- "QC_boreal_5" ## FRTs in QC_boreal: 1, 5 (also 4)
}
