###############################################################################
## additional simulation object definitions
################################################################################

data("sppEquivalencies_CA", package = "LandR")
sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                             EN_generic_full = "Pine",
                                             Leading = "Pine leading")]

# Make LandWeb spp equivalencies
sppEquivalencies_CA[, LandWeb := c(Pice_mar = "Pice_mar", Pice_gla = "Pice_gla",
                                   Pinu_con = "Pinu_sp", Pinu_ban = "Pinu_sp",
                                   Popu_tre = "Popu_sp", Betu_pap = "Popu_sp",
                                   Abie_bal = "Abie_sp", Abie_las = "Abie_sp", Abie_sp = "Abie_sp")[LandR]]

sppEquivalencies_CA[LandWeb == "Abie_sp", EN_generic_full := "Fir"]
sppEquivalencies_CA[LandWeb == "Abie_sp", EN_generic_short := "Fir"]
sppEquivalencies_CA[LandWeb == "Abie_sp", Leading := "Fir leading"]

sppEquivalencies_CA[LandWeb == "Popu_sp", EN_generic_full := "Deciduous"]
sppEquivalencies_CA[LandWeb == "Popu_sp", EN_generic_short := "Decid"]
sppEquivalencies_CA[LandWeb == "Popu_sp", Leading := "Deciduous leading"]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(LandWeb), ]

sppColorVect <- sppColors(sppEquivalencies_CA, sppEquivCol, newVals = "Mixed", palette = "Accent")

speciesTable <- getSpeciesTable(dPath = paths1$inputPath) ## uses default URL
speciesParams <- list( ## TODO
  growthcurve = list(Abie_sp = 0, Pice_gla = 1, Pice_mar = 1, Pinu_sp = 0, Popu_sp = 0),
  mortalityshape = list(Abie_sp = 15L, Pice_gla = 15L, Pice_mar = 15L, Pinu_sp = 15L, Popu_sp = 25L),
  resproutage_min = list(Popu_sp = 25L), # default 10L
  #resproutprob = list(Popu_sp = 0.1), # default 0.5
  shadetolerance = list(Abie_sp = 3, Pice_gla = 2, Pice_mar = 3, Pinu_sp = 1, Popu_sp = 1) # defaults 4, 3, 4, 1, 1
)
speciesParams <- append(speciesParams, list(
    seeddistance_eff = list(Abie_sp = 25L, Pice_gla = 100L, Pice_mar = 80L, Pinu_sp = 30L, Popu_sp = 200L),
    seeddistance_max = list(Abie_sp = 160L, Pice_gla = 303L, Pice_mar = 200L, Pinu_sp = 100L, Popu_sp = 2000L)
  )
)

