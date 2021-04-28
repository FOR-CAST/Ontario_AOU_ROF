## Google Drive locations for pre-run simulation objects
## these are used when config option 'prerun' is true

gdriveURL <- if (grepl("AOU", runName)) {
  "https://drive.google.com/drive/folders/1DWOgy-XxZO9pmgfRXEzHJPX7jU4x3Vki/"
} else if (grepl("ROF", runName)) {
  "https://drive.google.com/drive/folders/1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4/"
}

gdriveSims <- if (grepl("AOU", runName)) {
  if (climateScenario == "CCSM4_RCP45") {
    list(
      simOutPreamble = "1qQZnwqtcZMYJ43MYN8QZBxtd0tHHBbha",
      simOutPreambleArchive = "",
      biomassMaps2001 = "1eAYoAKDOBkYvdIF5Txwy8FyBlmpL9s19", ## same for all climate scenarios
      biomassMaps2001Archive = "",
      biomassMaps2011 = "1UlOm5zx7wWvRvLIUoCoC4HQg188WwKHv", ## same for all climate scenarios
      biomassMaps2011Archive = "",
      fSsimDataPrep = "1zB0c__IYQiQfUguXaYkkC0Hzp1wRVSHh", ## TODO: why is this different for diff CC scenarios?
      fSsimDataPrepArchive = "",
      ignitionOut = "1_NtOc8lG5GMIftWXsxPdeW_pu9Z7EZGa", ## same for all climate scenarios
      ignitionOutArchive = "",
      escapeOut = "1zinlswysrTcQ1OZxBsfR-S5-kpMbDC8J", ## same for all climate scenarios
      escapeOutArchive = "",
      spreadOut = "1-NuFcAcnG0G5SEEH5LlX0moh5h6EYcAs", ## same for all climate scenarios
      spreadOutArchive = "",
      results = basename(gdriveURL)
    )
  } else if (climateScenario == "CCSM4_RCP85") {
    list(
      simOutPreamble = "1InmRgjKLZip36AzUy6XzRufdt2mk1nwA",
      simOutPreambleArchive = "",
      biomassMaps2001 = "1eAYoAKDOBkYvdIF5Txwy8FyBlmpL9s19", ## same for all climate scenarios
      biomassMaps2001Archive = "",
      biomassMaps2011 = "1UlOm5zx7wWvRvLIUoCoC4HQg188WwKHv", ## same for all climate scenarios
      biomassMaps2011Archive = "",
      fSsimDataPrep = "1t_4fap26_W9cRXbXaWzX22LUMBOvaySE", ## TODO: why is this different for diff CC scenarios?
      fSsimDataPrepArchive = "",
      ignitionOut = "1_NtOc8lG5GMIftWXsxPdeW_pu9Z7EZGa", ## same for all climate scenarios
      ignitionOutArchive = "",
      escapeOut = "1zinlswysrTcQ1OZxBsfR-S5-kpMbDC8J", ## same for all climate scenarios
      escapeOutArchive = "",
      spreadOut = "1-NuFcAcnG0G5SEEH5LlX0moh5h6EYcAs", ## same for all climate scenarios
      spreadOutArchive = "",
      results = basename(gdriveURL)
    )
  }
} else if (grepl("ROF", runName)) {
  if (climateScenario == "CCSM4_RCP45") {
    list(
      simOutPreamble = "1LIZEM4Jyga0CjNG-Z_1ql2rj11or54Tg",
      simOutPreambleArchive = "",
      biomassMaps2001 = "1o0_Y8_2Eqq8-hznhVyA72z1wd47enK0f", ## same for all climate scenarios
      biomassMaps2001Archive = "",
      biomassMaps2011 = "135tLgXpKzp-UA274A3KvXPg1M9BviACY", ## same for all climate scenarios
      biomassMaps2011Archive = "",
      fSsimDataPrep = "1t623sGLj61PwUK2LPUpxpOyK64dMTdCn", ## TODO: why is this different for diff CC scenarios?
      fSsimDataPrepArchive = "",
      ignitionOut = "1AbWVAZVvwIm-L--qx__EnNswHMT6CJTO", ## same for all climate scenarios
      ignitionOutArchive = "",
      escapeOut = "10v5bXiRxPoTgINZBv70uxb-zNr1HTlUi", ## same for all climate scenarios
      escapeOutArchive = "",
      spreadOut = "1aEqMNXiI9h25fd5LoH-PtoDrwm8xuDtM", ## same for all climate scenarios
      spreadOutArchive = "",
      results = basename(gdriveURL)
    )
  } else if (climateScenario == "CCSM4_RCP85") {
    list(
      simOutPreamble = "11oTUX0R1i65uIjuI1PmYYd3ibjVu0cyP",
      simOutPreambleArchive = "",
      biomassMaps2001 = "1o0_Y8_2Eqq8-hznhVyA72z1wd47enK0f", ## same for all climate scenarios
      biomassMaps2001Archive = "",
      biomassMaps2011 = "135tLgXpKzp-UA274A3KvXPg1M9BviACY", ## same for all climate scenarios
      biomassMaps2011Archive = "",
      fSsimDataPrep = "179z3eiBexZZraVkpSXfp-K0VjZzuNwSW", ## TODO: why is this different for diff CC scenarios?
      fSsimDataPrepArchive = "",
      ignitionOut = "1AbWVAZVvwIm-L--qx__EnNswHMT6CJTO", ## same for all climate scenarios
      ignitionOutArchive = "",
      escapeOut = "10v5bXiRxPoTgINZBv70uxb-zNr1HTlUi", ## same for all climate scenarios
      escapeOutArchive = "",
      spreadOut = "1aEqMNXiI9h25fd5LoH-PtoDrwm8xuDtM", ## same for all climate scenarios
      spreadOutArchive = "",
      results = basename(gdriveURL)
    )
  }
}
