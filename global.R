library(sf)
library(sp)
library(reproducible)

cacheDir <- checkPath("cache", create = TRUE)
inputDir <- checkPath("inputs", create = TRUE)
outputDir <- checkPath("outputs", create = TRUE)

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

## provincial boundary

canProvs <- Cache(prepInputs,
                  "GADM",
                  fun = "base::readRDS",
                  dlFun = "raster::getData",
                  country = "CAN", level = 1, path = inputDir,
                  #targetCRS = targetCRS, ## TODO: fails on Windows
                  targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                  cacheRepo = cacheDir,
                  destinationPath = inputDir) %>%
  st_as_sf(.)

ON <- canProvs[canProvs$NAME_1 == "Ontario", ] %>%
  st_transform(., crs = targetCRS)

## study area

url <- "https://drive.google.com/file/d/1SmaLtI4-oueFTqBsW8pv9294l3a8h2l8"
url_buffered <- "https://drive.google.com/file/d/1wdF4bKNKTv_P9b784mV0bBZQiwk4I5OI"

AOU <- prepInputs(url,
                  destinationPath = inputDir,
                  targetFile = "CEON_def.shp",
                  alsoExtract = "similar",
                  targetCRS = targetCRS,
                  fun = "sf::st_read",
                  overwrite = TRUE,
                  team_drive = TRUE) %>%
  as_Spatial(.)

AOU_buffered <- prepInputs(url_buffered,
                           destinationPath = inputDir,
                           targetFile = "CEON_def_50km_buff.shp",
                           alsoExtract = "similar",
                           targetCRS = targetCRS,
                           fun = "sf::st_read",
                           overwrite = TRUE,
                           team_drive = TRUE) %>%
  as_Spatial(.)


checkPath("images", create = TRUE)

png(file.path("images", "AOU.png"), width = 600, height = 600)
plot(as_Spatial(ON))
plot(AOU, add = TRUE, col = "lightblue")
dev.off()
