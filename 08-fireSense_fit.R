# 08a-ignitionFit.R --------------------------------------------------------------------------------

source("05-google-ids.R")

gid_fireSenseFit <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "fireSenseFit", gid]
upload_fireSenseFit <- config$args[["reupload"]] # | length(gid_fireSenseFit) == 0

fSFitParams <- list(
  fireSense_IgnitionFit = config$params[["fireSense_IgnitionFit"]],
  fireSense_EscapeFit = config$params[["fireSense_EscapeFit"]],
  fireSense_SpreadFit = config$params[["fireSense_SpreadFit"]]
)

fSFitObjects <- list(
  ## fireSense_IgnitionFit
  climateVariablesForFire = fSsimDataPrep[["climateVariablesForFire"]],
  fireSense_ignitionCovariates = fSsimDataPrep[["fireSense_ignitionCovariates"]],
  fireSense_ignitionFormula = fSsimDataPrep[["fireSense_ignitionFormula"]],
  flammableRTM = fSsimDataPrep[["flammableRTM"]],
  ignitionFitRTM = fSsimDataPrep[["ignitionFitRTM"]],

  ## fireSense_EscapeFit
  fireSense_escapeCovariates = fSsimDataPrep[["fireSense_escapeCovariates"]],
  fireSense_escapeFormula = fSsimDataPrep[["fireSense_escapeFormula"]],

  ## fireSense_SpreadFit
  fireBufferedListDT = fSsimDataPrep[["fireBufferedListDT"]],
  firePolys = fSsimDataPrep[["firePolys"]],
  fireSense_annualSpreadFitCovariates = fSsimDataPrep[["fireSense_annualSpreadFitCovariates"]],
  fireSense_nonAnnualSpreadFitCovariates = fSsimDataPrep[["fireSense_nonAnnualSpreadFitCovariates"]],
  fireSense_spreadFormula = fSsimDataPrep[["fireSense_spreadFormula"]],
  flammableRTM = fSsimDataPrep[["flammableRTM"]],
  rasterToMatch = fSsimDataPrep[["rasterToMatch"]],
  spreadFirePoints = fSsimDataPrep[["spreadFirePoints"]],
  studyArea = fSsimDataPrep[["studyArea"]]
)

ffSFit <- simFile(paste0("fSFitOut_", config$context[["studyAreaName"]]),
                  config$paths[["outputPath"]], ext = config$args[["fsimext"]])

if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_fireSenseFit)) {
  if (!file.exists(ffSFitt)) {
    googledrive::drive_download(file = as_id(gid_fireSenseFit), path = ffSFitt)
  }
  fireSenseFit <- loadSimList(ffSFit)
} else {
  fireSenseFit <- Cache(
    simInitAndSpades,
    times = config$args[["simYears"]],
    params = fSFitParams,
    modules = c(
      "fireSense_IgnitionFit",
      "fireSense_EscapeFit",
      "fireSense_SpreadFit"
    ),
    objects = fSFitObjects,
    userTags = c("ignitionFit", "escapeFit", "spreadFit")
  )

  if (isUpdated(fireSenseFit) || isFALSE(config$args[["useCache"]])) {
    fireSenseFit@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    saveSimList(
      fireSenseFit,
      ffSFit,
      inputs = FALSE,
      outputs = FALSE,
      cache = FALSE,
      files = FALSE
    )
  }

  if (isTRUE(upload_fireSenseFit)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = ffSFit, path = as_id(gdriveURL), name = basename(ffSFit))
    gid_fireSenseFit <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "fireSenseFit", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_fireSenseFit),
      gdriveSims
    )
  }

  if (isTRUE(config$context[["rep"]] == 1)) {
    source("R/upload_ignitionFit.R") ## TODO: add to the module -- but need to ensure only run once
  }

  source("R/upload_spreadFit.R") ## TODO: add to the module

  ## TODO: put this in the module to allow running all three fit modules together
  if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
    notifications::notify_google(
      paste0("fireSense fit for `", config$context[["studyAreaName"]], "` completed on host `",
             SpaDES.config::machine(), "`.")
    )
  }
}
