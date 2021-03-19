## Google Drive locations for pre-run simulation objects
## these are used when config option 'prerun' is true

gdriveURL <- "https://drive.google.com/drive/folders/1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4/"

gdriveSims <- if (grepl("AOU", runName)) {
  list(
    simOutPreamble = "",
    simOutPreambleArchive = "",
    biomassMaps2001 = "",
    biomassMaps2001Archive = "",
    biomassMaps2011 = "",
    biomassMaps2011Archive = "",
    fSsimDataPrep = "",
    fSsimDataPrepArchive = "",
    ignitionOut = "",
    ignitionOutArchive = "",
    escapeOut = "",
    escapeOutArchive = "",
    spreadOut = "",
    spreadOutArchive = ""
  )
} else if (grepl("ROF", runName)) {
  list(
    simOutPreamble = "11oTUX0R1i65uIjuI1PmYYd3ibjVu0cyP",
    simOutPreambleArchive = "",
    biomassMaps2001 = "",
    biomassMaps2001Archive = "",
    biomassMaps2011 = "",
    biomassMaps2011Archive = "",
    fSsimDataPrep = "",
    fSsimDataPrepArchive = "",
    ignitionOut = "",
    ignitionOutArchive = "",
    escapeOut = "",
    escapeOutArchive = "",
    spreadOut = "",
    spreadOutArchive = ""
  )
}
