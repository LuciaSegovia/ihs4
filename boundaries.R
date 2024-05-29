

library(sp)
library(sf)

# Loading shapefile
mwi <- st_read(here::here("data", "mwi-boundaries", 
                   "mwi_adm_nso_hotosm_20230329_shp", 
                   "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))

# Checking map
head(mwi)
plot(mwi[, "ADM2_EN"])

# Checking the district of the "cities". 
test <- mwi %>% filter(grepl("Mzuzu|Mzimba", ADM2_EN, ignore.case = TRUE)) 
plot(test[, "ADM2_EN"])

mwi$district <- as.integer(gsub("MW", "", mwi$ADM2_PCODE))
