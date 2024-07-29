
library(dplyr)
library(sp)
library(sf)
library(tmap)


# Read in the file------

ihs4 <- read.csv(here::here("data", "inter-output", "hh_mod_g_processed.csv")) %>% 
  dplyr::filter(consYN == 1)
nsu.df <- read.csv(here::here('data', "factors", 'nsu.factors.csv'), fileEncoding="UTF-8-BOM")
geo <- read.csv(here::here('data', 'HouseholdGeovariablesIHS4.csv'))

length(unique(ihs4$case_id)) 
names(geo)

geo %>% filter(grepl("10304156", case_id))

# Loading shapefile
mwi <- st_read(here::here("data", "mwi-boundaries", 
                   "mwi_adm_nso_hotosm_20230329_shp", 
                   "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))

# Loading shapefile
ea <- st_read(here::here("data", "mwi-boundaries", 
                          "EN_NSO", "eas_bnd.shp"))
st_crs(ea)

tm_shape(ea) +
  tm_borders() +
  tm_shape(ea %>% filter(grepl("lake", DISTRICT, ignore.case = TRUE))) +
  #tm_shape(ea %>% filter(DIST_CODE == 0)) +
  tm_polygons(col="darkgreen")

# Excluding the lakes


# Checking map
head(mwi)
plot(mwi[, "ADM2_EN"])

# Checking the district of the "cities". 
test <- mwi %>% filter(grepl("Mzuzu|Mzimba", ADM2_EN, ignore.case = TRUE)) 
plot(test[, "ADM2_EN"])

mwi$district <- as.integer(gsub("MW", "", mwi$ADM2_PCODE))

# Checking HH location w/ EA ------
# Checking missing lon/lat
ihs4 %>% distinct(case_id, ea_id, district, region, reside) %>% left_join(., geo) %>% 
  filter(is.na(lat_modified))

# Joining IHS4 w/ GPS location  
ihs_geo <- ihs4 %>% distinct(case_id, ea_id, district, region, reside) %>% 
  left_join(., geo) %>% filter(!is.na(lat_modified)) %>% 
  #converting into a spatial obs
  st_as_sf(., coords = c("lon_modified", "lat_modified"), crs = st_crs(ea) )

ihs_geo %>% count(ea_id)

tm_shape(ea) +
  tm_borders() +
tm_shape(ihs_geo$geometry[ihs_geo$ea_id=="31436841"]) +
  tm_symbols()

## Checking the variables of location of hh

ihs_geo %>% filter(ea_id=="31436841") %>% 
  summarise(mean_road = mean(dist_agmrkt), 
            sd_road =sd(dist_agmrkt))


test <- ihs_geo %>% select(case_id, ea_id, district, reside)

#  Generating the offset buffer ----

for(i in 1:nrow(test)){
  
  # Assigning buffer size (in m) acc. to Urban (U) or Rural (R)
  offset.dist<-ifelse(test$reside[i]=="1", 2000, 5000)
  
  # Generating the buffers around the centroids
  test$buffer[i] <- st_buffer(test$geometry[i], dist = offset.dist)
  
}

# Saving the buffer (bc takes very long to generate)
#test %>% st_drop_geometry() %>% rename(geometry = "buffer") %>% 
#  st_as_sf(.,  crs = st_crs(test)) %>%
#  st_write(., here::here("data", 
#  "inter-output", "boundaries", "ihs4-offset.shp"))

# Transforming the list into spatial class
# test$buffer <- st_as_sfc(test$buffer)

# Checking that the output
plot(test$buffer) # Plotting the buffer
plot(test$buffer[test$reside == "1"],col='red',add=TRUE) # colouring red those that are urban (smaller radius)

tm_shape(ea) +
  tm_borders() +
  tm_shape(ihs_geo$geometry) +
  tm_symbols(size = 0.1, col = "blue") +
tm_shape(test$buffer) +
  tm_borders(col="red")

geodata.df <- test %>% st_drop_geometry() %>% rename(geometry = "buffer") %>% 
  st_as_sf(.,  crs = st_crs(test))
