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

if (exists(".fireCause", .GlobalEnv)) {
  stopifnot(.fireCause %in% c("L", "H", "N"))
} else {
  # .fireCause <- c("L")
  .fireCause <- c("L", "H")
}

if (exists(".fireModel", .GlobalEnv)) {
  ## fireSense: Marchal et al. climate/vegetation sensitive stand replacing fire;
  ## scfm: Cumming et al. stand replacing fires;
  .fireModel <- tolower(.fireModel)
  stopifnot(.fireModel %in% c("firesense", "scfm"))
} else {
  # .fireModel <- tolower("firesense")
  .fireModel <- tolower("scfm")
}

if (exists(".fireRegimePolysType", .GlobalEnv)) {
  stopifnot(.fireRegimePolysType %in%
              c("ECODISTRICT", "ECOREGION", "FRT", "FRU"))
} else {
  .fireRegimePolysType <- ifelse(.fireModel == "scfm", "FRT", "ECOREGION") ## TODO: re-evaluate firesense
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
  if (.fireModel == "scfm") {
    .studyAreaName <- "ON_AOU" ## scfm can handle multiple fire regime polygons
  } else {
    .studyAreaName <- "ON_AOU_1" ## FRTs in AOU: 1, 5 (small parts of 2, 6, 7)
    #.studyAreaName <- "ON_ROF_5" ## FRTs in ROF: 1, 5 (small parts of 2)
    #.studyAreaName <- "ON_ROF_shield" ## ecozones in ROF: Boreal Shield, Hudson Plain
    #.studyAreaName <- "QC_boreal_5" ## FRTs in QC_boreal: 1, 5 (also 4)
  }
}

## TODO: manually adjust/calibrate while fixing scfm; eventually remove.
if (exists(".burnRateMultiplier", .GlobalEnv)) {
  if (.fireModel != "scfm") {
    txt <- ".burnRateMultiplier ignored for model runs not using scfm"
    message(txt)
    warning(txt)
    rm(txt)
  }
} else {
  .burnRateMultiplier <- as.numeric(.rep) ## TODO: use 1.0 as default after testing
}
