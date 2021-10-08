Require::Require("reproducible")
Require::Require("googledrive")

source("05-google-ids.R")

file.move(
  file.path("outputs", studyAreaName, "figures", "spreadFit_coeffs.png"),
  file.path("outputs", studyAreaName, "figures", paste0("spreadFit_coeffs_", studyAreaName, ".png"))
)

filesToUpload <- c(
  list.files(file.path("outputs", studyAreaName),
             pattern = paste0("^fireSense_SpreadFit_veg_coeffs_", studyAreaName, "[.]txt$")),
  paste0("figures/PCAcoeffLoadings_", studyAreaName, ".png"),
  paste0("figures/spreadFit_coeffs_", studyAreaName, ".png")
)

lapply(filesToUpload, function(f) {
  retry(quote(drive_upload(file.path("outputs", studyAreaName, f), as_id(gdriveSims[["results"]]), overwrite = TRUE)),
        retries = 5, exponentialDecayBase = 2)
})
