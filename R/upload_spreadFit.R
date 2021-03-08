Require::Require("googledrive")

filesToUpload <- c("fireSense_SpreadFit_veg_coeffs.txt",
                   "figures/PCAcoeffLoadings.png",
                   "figures/spreadFit_coeffs.png")

gdrive_ID <- if (grepl("AOU", runName)) {
  "1DWOgy-XxZO9pmgfRXEzHJPX7jU4x3Vki"
} else if (grepl("ROF", runName)) {
  "1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4"
}

lapply(filesToUpload, function(f) {
  drive_upload(file.path("outputs", runName, f), as_id(gdrive_ID), overwrite = TRUE)
})
