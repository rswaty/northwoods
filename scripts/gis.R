
# Notes ----

## Get and process LANDFIRE data for northwoods
## Randy Swaty
## June 5, 2025
## Outputs moved to GIS folder


# Set up ----

## packages

library(foreign)
#library(raster)
library(rlandfire)
library(scales)
library(sf)
library(terra)
library(tidyverse)


## read in area of interest

shp <- st_read("inputs/northwoods.shp") %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

vect(shp)
plot(shp)

# attribute and other tables
bps_disturbances <- read.csv("inputs/bps_transitions.csv")
bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")
fri_conus_atts <- read.csv("inputs/LF16_FRI.csv")
evt_conus_atts <- read.csv("inputs/LF22_EVT_230.csv")

hist_dist_conus_atts <- read.csv("inputs/LF2024_HDist24.csv") %>%
  mutate(Dist_Year = paste(DIST_TYPE, HDIST_YR, sep = "_"))

# Get LANDFIRE data ----


aoi <- getAOI(shp)

#products <-  c("220BPS", "240SCLASS", "240EVC", "240EVH", "240EVT", "200FRI", "HDIST2023")
products <- c("HDIST2023")
projection <- 5070
resolution <- 30
email <- "rswaty@tnc.org" # Replace with your email address. LANDFIRE APIv2 requires a valid email address.

# R specific arguments
save_file <- tempfile(fileext = ".zip")

# call API
ncal <- landfireAPIv2(
  products, 
  aoi, 
  projection, 
  resolution, 
  path = save_file,
  email = email)

# Define the destination path
dest_file <- file.path("inputs", "mfri_data.zip")

# Move and rename the file
file.rename(save_file, dest_file)

# Create a temporary directory for unzipping
temp_dir <- tempfile()
dir.create(temp_dir)

# Unzip the file into the temporary directory
unzip(dest_file, exdir = temp_dir)

# Get the list of unzipped files
unzipped_files <- list.files(temp_dir, full.names = TRUE)

# Rename each unzipped file to "landfire_data" with its full original extension
for (file in unzipped_files) {
  file_name <- basename(file)
  file_extension <- sub("^[^.]*", "", file_name)  # Extract the full extension
  new_file_path <- file.path("inputs", paste0("mfri_data", file_extension))
  file.rename(file, new_file_path)
}

# Clean up the temporary directory
unlink(temp_dir, recursive = TRUE)

stacked_rasters <- rast("inputs/landfire_data.tif")

# "split" downloaded raster into separate layers
for(lyr in names(stacked_rasters)) assign(lyr, stacked_rasters[[lyr]])

# BpS data workup ----

bps_aoi <- US_220BPS %>%
  crop(shp) %>%
  mask(shp)

levels(bps_aoi)[[1]] <- bps_conus_atts
activeCat(bps_aoi) <- "VALUE"

bps_aoi_atts <- values(bps_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(bps_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


writeRaster(bps_aoi, "outputs/bps_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(bps_aoi_atts, "outputs/bps_aoi.tif.vat.dbf")

## write csv for fun
write.csv(bps_aoi_atts, "outputs/bps_aoi_attributes.csv")

## try new color file
summary_bps_name <- bps_aoi_atts %>%
  subset(BPS_NAME != "Open Water" & BPS_NAME != "Barren-Rock/Sand/Clay") %>%
  group_by(BPS_NAME) %>%
  summarise(bps_name_totals = sum(REL_PERCENT)) %>%
  ungroup()

## if using top 10
# top_groups <- summary_bps_name %>%
#   top_n(10, wt = bps_name_totals)

## if using BpSs with amounts > certain percent

top_groups <- summary_bps_name %>%
  filter(bps_name_totals >= 1)

filtered_bps_name_groups <- bps_aoi_atts %>%
  filter(BPS_NAME %in% top_groups$BPS_NAME)


BpSColorFile <- filtered_bps_name_groups %>%
  add_column(z = 255) %>%
  dplyr::select(
    VALUE,
    R,
    G,
    B,
    z,
    BPS_NAME)  %>%
  arrange(BPS_NAME) 

geographies <- c(
  "Boreal ",
  "Central Interior and Appalachian ",
  "Great Lakes ",
  "Laurentian ",
  "Laurentian-Acadian ",
  "North-Central Interior ")

BpSColorFile$BPS_NAME <- gsub(paste(geographies, collapse = "|"), "", BpSColorFile$BPS_NAME)

# 
write.table(BpSColorFile, file = "outputs/BpSColorFile.txt", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)




# Historical BpS disturbances ----


bps_aoi_disturbances <- left_join(bps_disturbances, bps_aoi_atts, 
                                  by = c("Model_Code" = "BPS_MODEL")) %>%
  drop_na(VALUE) %>%
  mutate(annual_dist_acres = annual_probability*ACRES) %>%
  select(-c(3, 7:25))

write.csv(bps_aoi_disturbances, "outputs/bps_aoi_disturbances.csv")



# MFRI data ----

mfri_aoi <- US_200FRI %>%
  crop(shp) %>%
  mask(shp)

levels(mfri_aoi)[[1]] <- fri_conus_atts
activeCat(mfri_aoi) <- "VALUE"

fri_aoi_atts <- values(mfri_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(bps_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


writeRaster(mfri_aoi, "outputs/mfri_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(fri_aoi_atts, "outputs/mfri_aoi.tif.vat.dbf")

## write csv for fun
write.csv(bps_aoi_atts, "outputs/mfri_aoi_attributes.csv")

# EVT ----


evt_aoi <- US_240EVT %>%
  crop(shp) %>%
  mask(shp)

levels(evt_aoi)[[1]] <- evt_conus_atts
activeCat(evt_aoi) <- "VALUE"


evt_aoi_atts <- values(evt_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(evt_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) 


writeRaster(evt_aoi, "outputs/evt_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = TRUE)
write.dbf(evt_aoi_atts, "outputs/evt_aoi.tif.vat.dbf")

## write csv for fun
write.csv(evt_aoi_atts, "outputs/evt_aoi_attributes.csv")


##  color file for use in QGIS    
EVTColorFile <- evt_aoi_atts %>%
  subset(EVT_NAME != "Open Water" & EVT_NAME != "Barren-Rock/Sand/Clay") %>%
  top_n(n = 10, wt = REL_PERCENT) %>%
  add_column(z = 255) %>%
  dplyr::select(VALUE,
                R,
                G,
                B,
                z,
                EVT_NAME)


write.table(EVTColorFile, file = "outputs/EVTColorFile.txt", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)   

# Hist Dist ----

US_HDIST2024 <- rast("inputs/mfri_data.tif")

hist_dist_aoi <- US_HDIST2024 %>%
  crop(shp) %>%
  mask(shp)

plot(hist_dist_aoi)



levels(hist_dist_aoi)[[1]] <- hist_dist_conus_atts
activeCat(hist_dist_aoi) <- "VALUE"


hist_dist_atts <- values(hist_dist_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(hist_dist_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


# writeRaster(hist_dist_aoi, "outputs/hdist.tif",
#             gdal = c("COMPRESS=NONE", "TFW=YES"),
#             datatype = "INT2S",
#             overwrite = T)


## reclassify based on Dist_Year field and values

# Create a mapping between Dist_Year values and numeric codes
unique_dist_years <- unique(hist_dist_atts$Dist_Year)

dist_year_mapping <- data.frame(
  Code = seq_along(unique_dist_years),
  Dist_Year = unique_dist_years) %>%
  separate(Dist_Year, into = c("Disturbance", "Year"),
           sep = "_",
           remove = FALSE)

# Merge the mapping with the original data
hist_dist_atts <- hist_dist_atts %>%
  left_join(dist_year_mapping, by = "Dist_Year") %>%
  dplyr::mutate(
    VALUE = as.numeric(VALUE)
  )

# Create the reclassification matrix using the numeric codes
reclass_matrix <- hist_dist_atts %>%
  dplyr::select(VALUE, Code) %>%
  data.matrix()

# Print the reclassification matrix to verify
print(reclass_matrix)

# Reclassify the raster using the numeric codes
reclassified_raster <- classify(hist_dist_aoi, reclass_matrix)

dist_year_mapping <- dist_year_mapping %>%
  rename(VALUE = Code)

levels(reclassified_raster)[[1]] <- dist_year_mapping
activeCat(reclassified_raster) <- "VALUE"


reclass_hdist_atts <- values(reclassified_raster, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(reclassified_raster)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT)) %>%
  separate(Dist_Year, into = c("Disturbance", "Year"),
           sep = "_",
           remove = FALSE)

writeRaster(reclassified_raster, "outputs/reclassified_hdist_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

row.names(reclass_hdist_atts) <- NULL

write.dbf(reclass_hdist_atts, 
          "outputs/reclassified_hdist_aoi.tif.vat.dbf"
)

## write csv for fun
write.csv(reclass_hdist_atts, 
          "outputs/reclass_hdist_atts.csv",
          row.names = FALSE)

reclass_dbf <- read.dbf("outputs/reclass_hdist_atts.tif.vat.dbf")


# EVT ----


evt_aoi <- US_240EVT %>%
  crop(shp) %>%
  mask(shp)


levels(evt_aoi)[[1]] <- evt_conus_atts
activeCat(evt_aoi) <- "VALUE"


evt_aoi_atts <- values(evt_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(evt_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) 


writeRaster(evt_aoi, "outputs/evt_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = TRUE)
write.dbf(evt_aoi_atts, "outputs/evt_aoi.tif.vat.dbf")

## write csv for fun
write.csv(evt_aoi_atts, "outputs/evt_aoi_attributes.csv")


##  color file for use in QGIS    
EVTColorFile <- evt_aoi_atts %>%
  subset(EVT_NAME != "Open Water" & EVT_NAME != "Barren-Rock/Sand/Clay") %>%
  top_n(n = 10, wt = REL_PERCENT) %>%
  add_column(z = 255) %>%
  dplyr::select(VALUE,
                R,
                G,
                B,
                z,
                EVT_NAME)


write.table(EVTColorFile, file = "outputs/EVTColorFile.txt", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)   









