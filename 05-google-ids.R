## Google Drive locations for pre-run simulation objects
## these are used when config option 'prerun' is true

gdriveURL <- "https://drive.google.com/drive/folders/1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4/"

gdriveSims <- if (grepl("AOU", runName)) {
  list(
    simOutPreamble = "1InmRgjKLZip36AzUy6XzRufdt2mk1nwA",
    simOutPreambleArchive = "",
    biomassMaps2001 = "1eAYoAKDOBkYvdIF5Txwy8FyBlmpL9s19",
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
    biomassMaps2001 = "1o0_Y8_2Eqq8-hznhVyA72z1wd47enK0f",
    biomassMaps2001Archive = "",
    biomassMaps2011 = "135tLgXpKzp-UA274A3KvXPg1M9BviACY",
    biomassMaps2011Archive = "",
    fSsimDataPrep = "179z3eiBexZZraVkpSXfp-K0VjZzuNwSW",
    fSsimDataPrepArchive = "",
    ignitionOut = "",
    ignitionOutArchive = "",
    escapeOut = "",
    escapeOutArchive = "",
    spreadOut = "",
    spreadOutArchive = ""
  )
}
