library(magrittr)
library(sf)
library(sp)
library(raster)
library(reproducible)

cPath <- "cache"
dPath <- "inputs"

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

canProvs <- Cache(prepInputs,
                  "GADM",
                  fun = "base::readRDS",
                  dlFun = "raster::getData",
                  country = "CAN", level = 1, path = dPath,
                  #targetCRS = targetCRS, ## TODO: fails on Windows
                  targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                  cacheRepo = cPath,
                  destinationPath = dPath) %>%
  st_as_sf(.)

ON <- canProvs[canProvs$NAME_1 == "Ontario", ] %>%
  st_transform(., crs = targetCRS) %>%
  as_Spatial(.)

studyArea <- prepInputs(
  url = "https://drive.google.com/file/d/1DzVRglqJNvZA8NZZ7XKe3-6Q5f8tlydQ",
  targetCRS = targetCRS, ## TODO: fails on Windows
  targetFile = "ROF_RA_def.shp", alsoExtract = "similar",
  fun = "sf::st_read", destinationPath = dPath, filename2 = "ROF_RA_def", overwrite = TRUE
) %>%
  as_Spatial(.)

studyAreaLarge <- prepInputs(
  url = "https://drive.google.com/file/d/1iOXXIkvY-YaR9BTG_SRd5R_iLstk99n0",
  targetCRS = targetCRS, ## TODO: fails on Windows
  targetFile = "ROF_RA_def_50km_buff.shp", alsoExtract = "similar",
  fun = "sf::st_read", destinationPath = dPath, filename2 = "ROF_RA_def_50km_buff", overwrite = TRUE
) %>%
  as_Spatial(.)

area_ha_ROF_buff <- rgeos::gArea(studyAreaLarge) / 100^2
nPixels_ROF_buff <- area_ha_ROF_buff / 6.25

png(file.path("images", "RoF_buffer.png"), width = 600, height = 600)
plot(ON)
plot(studyArea, add = TRUE, col = "lightblue")
plot(studyAreaLarge, add = TRUE)
dev.off()

