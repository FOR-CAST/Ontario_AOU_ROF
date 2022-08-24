library(magrittr)
library(sf)
library(sp)
library(raster)
library(reproducible)
library(amc)

cPath <- "cache"
dPath <- "data"

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
  st_transform(., crs = targetCRS)

### *preliminary study area* based on caribou ranges ------------------------------------------- ###
## ==> this has been replaced by an official study area definition

# linked at https://geohub.lio.gov.on.ca/datasets/caribou-range-boundary/data
url <- "https://opendata.arcgis.com/datasets/f1cf5d1b5ef346feafa386ba626d536b_8.zip"

caribou <- prepInputs(url = url, targetFile = "Caribou_Range_Boundary.shp",
                      ## targetCRS = targetCRS, ## TODO: fails on Windows
                      alsoExtract = "similar", fun = "sf::st_read",
                      filename2 = "Caribou_Range_Boundary-shp.zip", destinationPath = dPath)
ranges <- c("Missisa", "Ozhiski", "James Bay", "Pagwachuan", "Nipigon")
caribou_RoF <- caribou[caribou[["RANGE_NAME"]] %in% ranges,]

studyArea <- postProcess(ON, studyArea = caribou_RoF, useSAcrs = TRUE, cacheRepo = cPath,
                         filename2 = NULL, overwrite = TRUE) %>%
  st_transform(., crs = targetCRS) %>%
  as_Spatial(.)

checkPath("images", create = TRUE)

png(file.path("images", "studyArea.png"), width = 600, height = 600)
plot(as_Spatial(ON))
plot(studyArea, add = TRUE, col = "lightblue")
dev.off()


## *preliminary* buffered study area ----------------------------------------------------------- ###

studyAreaLarge <- amc::outerBuffer(studyArea, 50000) ## 50 km

png(file.path("images", "studyArea_buffer.png"), width = 600, height = 600)
plot(as_Spatial(ON))
plot(studyArea, add = TRUE, col = "lightblue")
plot(studyAreaLarge, add = TRUE)
dev.off()
