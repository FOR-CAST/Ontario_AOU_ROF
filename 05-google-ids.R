## Google Drive locations for pre-run simulation objects

gdriveURL <- if (grepl("AOU", runName)) {
  "https://drive.google.com/drive/folders/1DWOgy-XxZO9pmgfRXEzHJPX7jU4x3Vki/"
} else if (grepl("ROF", runName)) {
  "https://drive.google.com/drive/folders/1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4/"
}

gdriveSims <- data.table::fread("05-google-ids.csv")

lvls <- c("simOutPreamble", "biomassMaps2001", "biomassMaps2011", "fSsimDataPrep",
          "ignitionOut", "escapeOut", "spreadOut", "results")
data.table::set(gdriveSims, NULL, "simObject", factor(gdriveSims$simObject, levels = lvls))
data.table::setkeyv(gdriveSims, c("studyArea", "simObject", "runID", "gcm", "ssp"))

update_googleids <- function(x, gdriveSims) {
  gdriveSims_updated <- rbind(gdriveSims, x)
  setorder(gdriveSims_updated)
  fwrite(x = gdriveSims_updated, file = "05-google-ids.csv")

  return(gdriveSims_updated)
}
