source("05-google-ids.R")

gid_escapeOut <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "escapeOut", gid]
upload_escapeOut <- config$args[["reupload"]] | length(gid_escapeOut) == 0

escapeFitParams <- list(
  fireSense_EscapeFit = config$params[["fireSense_EscapeFit"]]
)

escapeFitParams[["fireSense_EscapeFit"]][["fireSense_escapeFormula"]] <- fSsimDataPrep[["fireSense_escapeFormula"]]

escapeFitObjects <- list(
  fireSense_escapeCovariates = fSsimDataPrep[["fireSense_escapeCovariates"]]
)

fescapeOut <- simFile(paste0("escapeOut_", config$context[["studyAreaName"]]),
                      config$paths[["outputPath"]], ext = "rds") ## TODO use qs

if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_preamble)) {
  if (!file.exists(fescapeOut)) {
    googledrive::drive_download(file = as_id(gid_escapeOut), path = fescapeOut)
  }
  escapeOut <- loadSimList(fescapeOut)
} else {
  escapeOut <- simInitAndSpades(
    times = list(start = 0, end = 1),
    params = escapeFitParams,
    modules = "fireSense_EscapeFit",
    objects = escapeFitObjects
  )

  #if (isUpdated(escapeOut) || isFALSE(config$args[["useCache"]])) {
    escapeOut@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    saveSimList(
      escapeOut,
      fescapeOut,
      inputs = FALSE,
      outputs = FALSE,
      cache = FALSE,
      files = FALSE
    )
  #}

  if (isTRUE(upload_escapeOut)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = fescapeOut, path = as_id(gdriveURL), name = basename(fescapeOut))
    gid_escapeOut <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "escapeOut",  runID = NA,
                 gcm = NA, ssp = NA, gid = gid_escapeOut),
      gdriveSims
    )
  }
}
