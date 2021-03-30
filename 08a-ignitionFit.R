do.call(setPaths, ignitionFitPaths)

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["ignitionOut"]] == ""

#ub and lb have to be provided for now

biggestObj <- as.numeric(object.size(fSsimDataPrep[["fireSense_ignitionCovariates"]]))/1e6 * 1.2

form <- fSsimDataPrep[["fireSense_ignitionFormula"]]

nCores <- pmin(14, pemisc::optimalClusterNum(biggestObj)/2 - 6)
ignitionFitParams <- list(
  fireSense_IgnitionFit = list(
    cores = nCores,
    fireSense_ignitionFormula = form,
    lb = list(coef = 0,
              knots = round(quantile(fSsimDataPrep$fireSense_ignitionCovariates$MDC, probs = 0.05), digits = 0)),
    ## Ian: I don't know if this is the MDC value of the knot....
    ##      if using binomial need to pass theta to lb and ub
    ub = list(coef = 4,
              knots = round(quantile(fSsimDataPrep$fireSense_ignitionCovariates$MDC, probs = 0.8), digits = 0)),
    family = quote(MASS::negative.binomial(theta = 1, link = "identity")),
    iterDEoptim = 300
  )
)

ignitionFitObjects <- list(
  fireSense_ignitionCovariates = fSsimDataPrep$fireSense_ignitionCovariates
)

#dignitionOut <- file.path(Paths$outputPath, paste0("fS_IgnitionFit_", studyAreaName)) %>%
#  checkPath(create = TRUE)
#aignitionOut <- paste0(dignitionOut, ".7z")
fignitionOut <- file.path(Paths$outputPath, paste0("fS_IgnitionFit_", studyAreaName, ".qs"))
if (isTRUE(usePrerun)) {
  ignitionOut <- loadSimList(fignitionOut)
} else {
  ignitionOut <- Cache(
    simInitAndSpades,
    times = list(start = 0, end = 1),
    # ignitionSim <- simInit(times = list(start = 0, end = 1),
    params = ignitionFitParams,
    modules = "fireSense_IgnitionFit",
    paths = ignitionFitPaths,
    objects = ignitionFitObjects,
    userTags = c("ignitionFit")
  )
  saveSimList(
    sim = ignitionOut,
    filename = fignitionOut,
    #filebackedDir = dignitionOut,
    fileBackend = 2 ## TODO use fileBackend = 1
  )
  #archive::archive_write_dir(archive = aignitionOut, dir = dignitionOut)

  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("`fireSense_IgnitionFit` for ", runName, " completed on host `", Sys.info()[["nodename"]], "`."),
      channel = config::get("slackchannel"), preformatted = FALSE
    )
  }
}

if (isTRUE(uplaod2GDrive)) {
  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = fignitionOut, path = gdriveURL, name = basename(fignitionOut), verbose = TRUE)
    #googledrive::drive_put(media = aignitionOut, path = gdriveURL, name = basename(aignitionOut), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["ignitionOut"]]), media = fignitionOut)
    #googledrive::drive_update(file = as_id(gdriveSims[["ignitionOutArchive"]]), media = aignitionOut)
  }
}
