library(Require)
Require(c("ggplot2", "ggspatial", "raster", "reproducible", "sf", "sp"))

cacheDir <- checkPath("cache", create = TRUE)
inputDir <- checkPath("inputs", create = TRUE)
outputDir <- checkPath("outputs", create = TRUE)
mapDir <- checkPath("images", create = TRUE)

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

ecoprov <- prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
  targetFile = "ecoprovinces.shp",
  alsoExtract = "similar",
  fun = "sf::st_read",
  destinationPath = inputDir
) |>
  dplyr::rename(ECOPROVINCE = "ECOPROVINC") |>
  st_transform(targetCRS)

## Ontario -----------------------------------------------------------------------------------------

## provincial boundary
ON <- geodata::gadm(country = "CAN", level = 1, path = inputDir) %>%
  sf::st_as_sf(.) %>%
  subset(., NAME_1 %in% c("Ontario")) %>%
  sf::st_transform(., targetCRS)

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

area_ha_AOU <- units::set_units(st_area(AOU), ha)
nPixels_AOU <- units::drop_units(area_ha_AOU) / 6.25

AOU_buffered <- prepInputs(url2_buffered,
                           destinationPath = inputDir,
                           targetFile = "CEON_def_50km_buff.shp", ## studyAreaLarge
                           alsoExtract = "similar",
                           targetCRS = targetCRS,
                           fun = "sf::st_read",
                           overwrite = TRUE,
                           team_drive = TRUE)
area_ha_AOU_buff <- sf::st_area(AOU_buffered) %>% units::set_units(ha)
nPixels_AOU_buff <- units::drop_units(area_ha_AOU_buff) / 6.25

AOU_ecoprov <- st_intersection(AOU, ecoprov)

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

area_ha_ROF_buff <- sf::st_area(ROF_buffered) %>% units::set_units(ha)
nPixels_ROF_buff <- units::drop_units(area_ha_ROF_buff) / 1.5625

ROF_ecoprov <- st_intersection(ROF, ecoprov)

# maps ----------------------------------------------------------------------------------------

alpha <- 0.3
cols <- c("forestgreen", "darkblue")

## plot AOU only
AOU_gg <- ggplot(ON) +
  geom_sf(fill = "white", colour = "black", alpha = alpha) +
  geom_sf(data = AOU_ecoprov, aes(fill = ECOPROVINCE), alpha = alpha) +
  scale_fill_brewer(palette = "Greens") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Ontario Area of Undertaking (AOU) by national ecoprovince") +
  theme_bw()
ggsave(AOU_gg, filename = file.path(mapDir, "AOU_v2.png"), width = 8, height = 8)

## plot ROf only
ROF_gg <- ggplot(ON) +
  geom_sf(fill = "white", colour = "black", alpha = alpha) +
  geom_sf(data = ROF_ecoprov, aes(fill = ECOPROVINCE), alpha = alpha) +
  scale_fill_brewer(palette = "Blues") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Ontario Ring of Fire (ROF) study area by national ecoprovince") +
  theme_bw()
ggsave(ROF_gg, filename = file.path(mapDir, "RoF.png"), width = 8, height = 8)

## plot both areas
ON_both <- rbind(AOU_ecoprov, ROF_ecoprov)
ON_both[ON_both$NAME == 1, ]$NAME <- "AOU"
ON_both[ON_both$NAME == 107, ]$NAME <- "ROF"
ON_both_gg <- ggplot(ON) +
  geom_sf(fill = "white", colour = "black", alpha = alpha) +
  geom_sf(data = ON_both, aes(fill = NAME, col = NAME), alpha = alpha) +
  geom_sf_text(data = ON_both, aes(label = ECOPROVINCE)) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Ontario Area of Undertaking (AOU) and Ring of Fire (ROF) study areas by national ecoprovince") +
  scale_colour_manual(name = "Study Area", values = cols) +
  scale_fill_manual(name = "Study Area", values = cols) +
  theme_bw()
ggsave(ON_both_gg, filename = file.path(mapDir, "AOU-ROF.png"), width = 8, height = 8)

## Québec ------------------------------------------------------------------------------------------

## provincial boundary
QC <- geodata::gadm(country = "CAN", level = 1, path = inputDir) %>%
  sf::st_as_sf() |>
  subset(NAME_1 %in% c("Québec"))  |>
  sf::st_transform(targetCRS)

QC_boreal <- Cache({
  prepInputs(
    url = "https://drive.google.com/file/d/1enlgSf4-EJKuHZL4J79TQthoktSRBarQ/",
    targetFile = "NABoreal.shp",
    archive = asPath("boreal.zip"),
    destinationPath = inputDir,
    fun = "sf::read_sf",
    filename2 = NULL
  ) |>
    subset(TYPE %in% "BOREAL") |> ## only boreal forests for now; could include hemiboreal later
    st_transform(targetCRS) |>
    st_intersection(QC) |>
    st_cast("POLYGON") |>
    spatialEco::remove.holes() |>
    `st_crs<-`(targetCRS)
})

## https://www.donneesquebec.ca/recherche/dataset/unite-d-amenagement
## fails to extract the zip due to 'invalid multibyte character' in filename of the metadata pdf
tryCatch(preProcess(
  url = "https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/LIM_TERRITOIRE_FOREST_PUBLIC/UA/UA_SHP.zip",
  targetFile = "STF_UA.shp",
  alsoExtract = "similar",
  destinationPath = inputDir,
  filename2 = NULL,
  fun = "sf::read_sf"
), error = function(e) NULL)

files <- unzip(file.path(inputDir, "UA_SHP.zip"), list = TRUE)$Name
files2extract <- grep("STF_UA", files, value = TRUE)
unzip(file.path(inputDir, "UA_SHP.zip"), files = files2extract, exdir = inputDir)

## 1. read in data
## 2. remove most eastward polys + islands via subset
## 3. buffer 0 to get rid of 'lines'
## 4. buffer out, union the polys, they buffer back in (good enough approx of the study area)
## 5. remove holes
## 6. reproject the resulting single polygon
d <- units::set_units(5, km) ## this might need to be tweaked; keep as small as possible
ua <- Cache({
  st_read(file.path(inputDir, "UA_SHP", "STF_UA.shp")) |>
    subset(!NO_UG_RESP %in% c("011", "012", "035", "051", "93", "094", "111", "112")) |>
    st_make_valid() |>
    st_transform(targetCRS) |>
    st_buffer(0) |>
    st_buffer(d) |>
    st_union() |>
    st_buffer(-d) |>
    nngeo::st_remove_holes()
})

QC_boreal_ua <- st_intersection(QC_boreal, ua)
QC_boreal_ua_ecoprov <- st_intersection(QC_boreal_ua, ecoprov)

## plot boreal Québec
QC_boreal_gg <- ggplot(QC) +
  geom_sf(fill = "white", colour = "black", alpha = alpha) +
  geom_sf(data = QC_boreal_ua_ecoprov, aes(fill = ECOPROVINCE), alpha = alpha) +
  scale_fill_brewer(palette = "Purples") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Québec's western managed boreal forest by national ecoprovince") +
  theme_bw()
ggsave(AOU_gg, filename = file.path(mapDir, "QC_boreal.png"), width = 8, height = 8)
