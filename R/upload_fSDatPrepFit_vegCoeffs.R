Require::Require("reproducible")
Require::Require("googledrive")

source("05-google-ids.R")

try(file.move(
  file.path("outputs", studyAreaName, paste0("fireSense_SpreadFit_veg_coeffs_", studyAreaName, ".txt")),
  file.path("outputs", studyAreaName, sprintf("fireSense_SpreadFit_veg_coeffs_%s_run_%02d.txt", studyAreaName, run))
))

filesToUpload <- c(
  sprintf("fireSense_SpreadFit_veg_coeffs_%s_run_%02d.txt", studyAreaName, run)
)

gid_results <- gdriveSims[studyArea == studyAreaName & simObject == "results", gid]
lapply(filesToUpload, function(f) {
  retry(quote(drive_put(file.path("outputs", studyAreaName, f), unique(as_id(gid_results)))),
        retries = 5, exponentialDecayBase = 2)
})
