################################################################################
## Preamble (creates study areas, etc.)
################################################################################

do.call(SpaDES.core::setPaths, paths1) # Set them here so that we don't have to specify at each call to Cache

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["simOutPreamble"]] == ""

objects1 <- list(
  ".runName" = runName
)

parameters1 <- list(
  Ontario_preamble = list(
    ".plotInitialTime" = ifelse(usePlot, 0, NA),
    ".useCache" = TRUE,
    ".resolution" = resolution,
    "runName" = runName
  )
)

simOutPreamble <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters1,
                        modules = c("Ontario_preamble"),
                        objects = objects1,
                        paths = paths1,
                        debug = 1,
                        omitArgs = c("debug", "paths"),
                        #useCache = "overwrite",
                        useCloud = useCloudCache,
                        cloudFolderID = cloudCacheFolderID)

fsimOutPreamble <- simFile("simOutPreamble", Paths$outputPath, ext = "qs")
saveSimList(sim = simOutPreamble, filename = fsimOutPreamble, fileBackend = 2) ## TODO: use fileBackend = 1 ?

if (isTRUE(newGoogleIDs)) {
  googledrive::drive_put(media = fsimOutPreamble, path = gdriveURL, name = basename(fsimOutPreamble), verbose = TRUE)
  #googledrive::drive_put(media = asimOutPreamble, path = gdriveURL, name = basename(asimOutPreamble), verbose = TRUE)
} else {
  googledrive::drive_update(file = as_id(gdriveSims[["simOutPreamble"]]), media = fsimOutPreamble)
  #googledrive::drive_update(file = as_id(gdriveSims[["simOutPreambleArchive"]]), media = asimOutPreamble)
}
