## Google Drive locations for pre-run simulation objects
## these are used when config option 'prerun' is true

gdriveURL <- "https://drive.google.com/drive/folders/1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4/"

gdriveSims <- if (grepl("AOU", runName)) {
  list(
    simOutPreamble = "1InmRgjKLZip36AzUy6XzRufdt2mk1nwA",
    simOutPreambleArchive = "",
    biomassMaps2001 = "1eAYoAKDOBkYvdIF5Txwy8FyBlmpL9s19",
    biomassMaps2001Archive = "",
    biomassMaps2011 = "1UlOm5zx7wWvRvLIUoCoC4HQg188WwKHv",
    biomassMaps2011Archive = "",
    fSsimDataPrep = "1t_4fap26_W9cRXbXaWzX22LUMBOvaySE",
    fSsimDataPrepArchive = "",
    ignitionOut = "1_NtOc8lG5GMIftWXsxPdeW_pu9Z7EZGa",
    ignitionOutArchive = "",
    escapeOut = "1zinlswysrTcQ1OZxBsfR-S5-kpMbDC8J",
    escapeOutArchive = "",
    spreadOut = "1-NuFcAcnG0G5SEEH5LlX0moh5h6EYcAs",
    spreadOutArchive = "",
    results = "1DWOgy-XxZO9pmgfRXEzHJPX7jU4x3Vki"
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
    ignitionOut = "1AbWVAZVvwIm-L--qx__EnNswHMT6CJTO",
    ignitionOutArchive = "",
    escapeOut = "10v5bXiRxPoTgINZBv70uxb-zNr1HTlUi",
    escapeOutArchive = "",
    spreadOut = "1aEqMNXiI9h25fd5LoH-PtoDrwm8xuDtM",
    spreadOutArchive = "",
    results = "1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4"
  )
}
