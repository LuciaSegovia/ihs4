
# Loading the data
library(dplyr) # Data cleaning
library(tidyr) # Data manipulation
library(ggplot2) # Data visualisation
library(sf) # Spatial data manipulation

# Loading shapefile
mwi <- st_read(here::here("data", "mwi-boundaries", 
                          "mwi_adm_nso_hotosm_20230329_shp", 
                          "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))

## NCT data
nct <- read.csv(here::here("data", "fct_ihs5_v2.1.csv"))
head(nct)

# Checking maize items
nct %>% filter(grepl("maize", ihs5_fooditem, ignore.case = TRUE)) 
# Only in ihs5?
# sum(unique(ihs4$item_code) == 118)

# Checking Se values
hist(nct$SE)
# Mostly dried fish
nct %>% filter(SE>50)
plot(SE ~WATER, nct)

# Removing bottled water
Se_nct <- nct %>% mutate(Se_dry = ifelse(WATER==100, 0, 
                           SE*(100)/(100-WATER))) %>% 
  select(1:8, SE, Se_dry)


# Checking Se values
hist(Se_nct$Se_dry)
# Mostly animal source foods, worth checking:
# Thobwa (WAFCT - 12_002) - where the value is coming from? 
# WA doesn't report Se
Se_nct %>% filter(Se_dry>50)
plot(SE ~ENERC1, Se_nct)
# I think Mucuna was only intro in IHS5 & for infant formula we can assume zero
Se_nct %>% filter(is.na(SE))

# Obtaining only maize
maize <- nct %>% filter(grepl("maize", ihs5_fooditem, ignore.case = TRUE)) %>%  
  filter(ihs5_foodid != 118) %>% 
  select(starts_with("ihs5_"),starts_with("ref_"), WATER, ZN, SE) %>% 
mutate(ADM0_PCODE = "MW")

## Block kriging Zn (mg kg-1 DM)

data.df <- read.csv(here::here("data", "Zinc_block_kriging.csv"))
head(data.df)

# Need water concentration for conversion from DW to FW. (DW*(100-Water)/100)
# From Malawi FCT (from the IH5 NCT)

maize.df <- data.df %>% # Water and unit conversion (mg 100g-1 FW) [(DW*(100-Water)/100)/10]
  mutate(
    maize_101 = Block_mean*(100-maize$WATER[maize$ihs5_foodid == "101"])/1000, 
    maize_102 = Block_mean*(100-maize$WATER[maize$ihs5_foodid == "102"])/1000, 
    maize_103 = Block_mean*(100-maize$WATER[maize$ihs5_foodid == "103"])/1000, 
    maize_104 = Block_mean*(100-maize$WATER[maize$ihs5_foodid == "104"])/1000, 
    maize_105 = Block_mean*(100-maize$WATER[maize$ihs5_foodid == "105"])/1000, 
    maize_820 = Block_mean*(100-maize$WATER[maize$ihs5_foodid == "820"])/1000) %>% 
  select(-c(1:4))

maize.df <- maize.df %>% pivot_longer(., 
                          cols = starts_with("maize"),
                          names_to = "food_code", 
                          names_prefix = "maize_", 
                          values_to = "ZN") %>%
  arrange(food_code)

## Maize to flour ration (Supl.Table6, Joy et al., 2015)
ratio <- readxl::read_excel(here::here("data",  
                                       "40795_2015_36_MOESM1_ESM.xlsx"), 
                   sheet = 6, skip = 2)
head(ratio)

# Only need column 1 (element) and column 7 ratio
ratio <- ratio %>% select(c(1, 7)) %>%
  filter(!is.na(Element)) %>% 
  rename(ratio_refine="Mean ratio refined flour:whole grain") %>% 
  mutate(crop = "maize")

names(ratio)

# Generating bran ratio
ratio <- ratio %>% 
  mutate(ratio_bran = ifelse(ratio_refine<1, 1-ratio_refine, NA))

# Matching ratio w/ food id

ratio <- ratio %>% 
  # Combining the ratios as one column
  pivot_longer(., 
  cols = starts_with("ratio"),
  names_to = "food_code", 
   values_to = "ratio") %>% 
  # Changing the name to food_ids (IHS4)
  mutate(food_code =
           case_when(
             food_code == "ratio_refine" ~ 102, 
             food_code == "ratio_bran" ~ 103
           )) %>% 
# Removing one unnecessary column
    select(-crop) %>% 
  # Converting every mineral ratio into a column
  pivot_wider(names_from = "Element", 
              names_prefix = "ratio_",
              values_from = "ratio") %>% 
  mutate_at("food_code", as.character)
  
# Ratio for maize 

maize.df <- maize.df %>% left_join(.,ratio %>% select(food_code, ratio_Zn)) %>% 
  mutate(Zn = ifelse(!is.na(ratio_Zn), ZN*ratio_Zn, ZN)) %>% 
  select(District,food_code, Zn)
names(maize.df)

length(unique(maize.df$District)) # four cities missing
# length(unique(ihs4$district)) 32 
# Duplicating values for the "city districts"
# Mzuzu, Lilongwe, Zomba, Blantyre
maize.df <- maize.df %>% 
  filter(grepl("Mzimba|Lilongwe|Zomba|Blantyre", District)) %>% 
  mutate(District = case_when(
    District == "Mzimba" ~ "Mzuzu City", 
    District == "Lilongwe" ~ "Lilongwe City", 
    District == "Zomba" ~ "Zomba City", 
    District == "Blantyre" ~ "Blantyre City" 
  )) %>% 
  bind_rows(., maize.df) 

# Generating district-level NCT
head(nct)
nrow(nct)*32
# Adding district (duplication)
nct <- nct %>% 
   mutate(ADM0_PCODE = "MW") %>% 
  left_join(., mwi %>% st_drop_geometry() %>% 
   select(ADM0_PCODE, ADM2_EN)) %>% 
  rename(District = "ADM2_EN")

# Checking
nrow(nct)
nrow(maize.df)

## District-level NCT for Malawi (maize and products Zn) ------
# Adding Zn_district
dist_nct <- nct %>% left_join(., maize.df %>% rename(Zn_dist = "Zn"),
                  by = c("ihs5_foodid" = "food_code", "District")) %>% 
  mutate(Zn_dist = ifelse(is.na(Zn_dist), ZN, Zn_dist)) 

# write.csv(dist_nct, here::here("data", "inter-output", 
#                       "MWI_Zn-District_NCT_v1.0.0.csv"), row.names = FALSE)
#
## District-level NCT for Malawi (maize and products Zn) ------

# Adding district info
maize %>% left_join(., mwi %>% st_drop_geometry() %>% 
                      select(ADM0_PCODE, ADM2_EN)) %>% 
  rename(District = "ADM2_EN")
head(maize)

ihs4 %>% mutate_at("item_code", as.character) %>%
left_join(., mwi %>% st_drop_geometry() %>% 
            select(ADM2_EN, district)) %>% 
  filter(is.na(ADM2_EN))

ihs4 <- ihs4 %>% mutate_at("item_code", as.character) %>%
  left_join(., mwi %>% st_drop_geometry() %>% 
              select(ADM2_EN, district))

ihs4 %>% 
left_join(., maize.df, by = c("item_code" = "food_code", 
                              "ADM2_EN" = "District")) %>% 
  filter(is.na(Zn)) %>% filter(grepl("maize", item, ignore.case = TRUE)) %>% 
  distinct(item, ADM2_EN)

consumption <- ihs4 %>% 
  left_join(., maize.df, by = c("item_code" = "food_code", 
                                "ADM2_EN" = "District")) 

head(consumption)

consumption %>% 
  mutate(Zn_maize = kg_d/1000*Zn/100) %>% 
  filter(!is.na(Zn_maize)) %>% 
  ggplot(aes(Zn_maize,ADM2_EN )) + geom_boxplot()


%>% 
  ggplot() + geom_sf(aes(fill = cons_mean)) +
  labs(title = food)