library(Require)
Require("magrittr")
Require("sf")
Require("sp")
Require("raster")
Require("reproducible")
Require("ggplot2")
Require("ggspatial")

cacheDir <- checkPath("cache", create = TRUE)
inputDir <- checkPath("inputs", create = TRUE)
outputDir <- checkPath("outputs", create = TRUE)
mapDir <- checkPath("images", create = TRUE)

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

# AOU -----------------------------------------------------------------------------------------

#url <- "https://drive.google.com/file/d/1SmaLtI4-oueFTqBsW8pv9294l3a8h2l8"
#url_buffered <- "https://drive.google.com/file/d/1wdF4bKNKTv_P9b784mV0bBZQiwk4I5OI"
url2 <- "https://drive.google.com/file/d/1Idtreoo51hGBdfJXp0BmN83bOkKHTXvk"
url2_buffered <- "https://drive.google.com/file/d/1ngQshBgoyLjkjuXnloXPg7IUMdLmppkB"

AOU <- prepInputs(url2,
                  destinationPath = inputDir,
                  targetFile = "CEON_def.shp",
                  alsoExtract = "similar",
                  targetCRS = targetCRS,
                  fun = "sf::st_read",
                  overwrite = TRUE,
                  team_drive = TRUE)

area_ha_AOU <- rgeos::gArea(AOU) / 100^2
nPixels_AOU <- area_ha_AOU / 6.25

AOU_buffered <- prepInputs(url2_buffered,
                           destinationPath = inputDir,
                           targetFile = "CEON_def_50km_buff.shp",
                           alsoExtract = "similar",
                           targetCRS = targetCRS,
                           fun = "sf::st_read",
                           overwrite = TRUE,
                           team_drive = TRUE)
area_ha_AOU_buff <- sf::st_area(AOU_buffered) %>% units::set_units(., ha)
nPixels_AOU_buff <- units::set_units(area_ha_AOU_buff, NULL) / 6.25

# ROF -----------------------------------------------------------------------------------------

ROF <- prepInputs(
  url = "https://drive.google.com/file/d/1DzVRglqJNvZA8NZZ7XKe3-6Q5f8tlydQ",
  targetCRS = targetCRS, ## TODO: fails on Windows
  targetFile = "ROF_RA_def.shp", alsoExtract = "similar",
  fun = "sf::st_read", destinationPath = inputDir, filename2 = "ROF_RA_def", overwrite = TRUE
)

ROF_buffered <- prepInputs(
  url = "https://drive.google.com/file/d/1iOXXIkvY-YaR9BTG_SRd5R_iLstk99n0",
  targetCRS = targetCRS, ## TODO: fails on Windows
  targetFile = "ROF_RA_def_50km_buff.shp", alsoExtract = "similar",
  fun = "sf::st_read", destinationPath = inputDir, filename2 = "ROF_RA_def_50km_buff", overwrite = TRUE
)

area_ha_ROF_buff <- sf::st_area(ROF_buffered) %>% units::set_units(., ha)
nPixels_ROF_buff <- units::set_units(area_ha_ROF_buff, NULL) / 6.25

# maps ----------------------------------------------------------------------------------------

alpha <- 0.3
cols <- c("forestgreen", "darkblue")

## plot AOU only
AOU_gg <- ggplot(ON) +
  geom_sf(fill = "white", colour = "black", alpha = alpha) +
  geom_sf(data = AOU, fill = cols[1], col = cols[1], alpha = alpha) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Ontario Area of Undertaking (AOU)") +
  theme_bw()
ggsave(AOU_gg, filename = file.path(mapDir, "AOU_v2.png"), width = 8, height = 8)

## plot ROf only
ROF_gg <- ggplot(ON) +
  geom_sf(fill = "white", colour = "black", alpha = alpha) +
  geom_sf(data = ROF, fill = cols[2], col = cols[2], alpha = alpha) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Ontario Ring of Fire (ROF) study area") +
  theme_bw()
ggsave(ROF_gg, filename = file.path(mapDir, "RoF.png"), width = 8, height = 8)

## plot both areas
names(AOU) <- c("NAME", names(AOU)[-1])
names(ROF) <- c("NAME", names(ROF)[-1])
both <- rbind(AOU, ROF)
both$NAME <- c("AOU", "ROF")
both_gg <- ggplot(ON) +
  geom_sf(fill = "white", colour = "black", alpha = alpha) +
  geom_sf(data = both, aes(fill = NAME, col = NAME), alpha = alpha) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Ontario Area of Undertaking (AOU) and Ring of Fire (ROF) study areas") +
  scale_colour_manual(name = "Study Area", values = cols) +
  scale_fill_manual(name = "Study Area", values = cols) +
  theme_bw()

ggsave(both_gg, filename = file.path(mapDir, "AOU-ROF.png"), width = 8, height = 8)

