
# Loading libraries -----

library(srvyr)
library(tidyverse)
options(scipen=999)


# Read in the files ------


ihs4 <- read.csv(here::here("data", "inter-output", "hh_mod_g_processed.csv")) %>% 
  dplyr::filter(consYN == 1)
nsu.df <- read.csv(here::here('data', "factors", 'nsu.factors.csv'), fileEncoding="UTF-8-BOM")
geo <- read.csv(here::here('data', 'HouseholdGeovariablesIHS4.csv'))

names(geo)

## Prepare NSU file ----

nsu.df <- nsu.df %>% 
  # Converting three region columns into one column
  pivot_longer(., 
               cols = starts_with("ihs4factor"),
               names_to = "region", 
               values_to = "ihs4_factor") %>% 
# Separating the item code and the unit code
  separate(measure_id, c("item_code", "cons_unitA"), sep ="_", remove = FALSE) %>% 
  # Joining by cons_unitA to get unit (name)
  left_join(., ihs4 %>% distinct(cons_unitA, unit)) 

# Changing the factor for the region to their numeric code (n->s==1->3)
nsu.df$region[nsu.df$region == "ihs4factor_n"] <- 1
nsu.df$region[nsu.df$region == "ihs4factor_c"] <- 2
nsu.df$region[nsu.df$region == "ihs4factor_s"] <- 3

# Converting it inot an interger
nsu.df$region<- as.integer(nsu.df$region)
nsu.df$item_code <- as.integer(nsu.df$item_code)

# Own-production NSU to SU (kg/d) ----

# Checking missing food item/unit missing-ness
ihs4 %>% 
  left_join(., nsu.df, by = c("item_code_st"= "item_code", "region",
                              "prod_unitA" = "cons_unitA")) %>% 
  filter(is.na(ihs4_factor))

ihs4 %>% 
  left_join(., nsu.df, by = c("item_code_st"= "item_code", "region",
                              "prod_unitA" = "cons_unitA")) %>% 
  filter(is.na(ihs4_factor)) %>% count()

# Adding the units & calculating kg/d for those available
ihs4 <- ihs4 %>% 
  left_join(., nsu.df, by = c("item_code_st"= "item_code", "region",
                              "prod_unitA" = "cons_unitA")) %>% 
  mutate(prod_kg_d = (prod_quant*ihs4_factor)/7) 
  
sum(is.na(ihs4$prod_kg_d))


# Own-production from total consumption

ihs4 %>% 
  filter(is.na(prod_kg_d)) %>% 
  mutate(prod_kg_d = ifelse(cons_quant == prod_quant & cons_unitA == prod_unitA, 
                             kg_d, prod_kg_d)) %>%
           filter(is.na(prod_kg_d) & !is.na(prod_quant)) %>% 
          filter(prod_quant >0) %>% 
  dplyr::select(ends_with("quant"), cons_unitA, prod_unitA,  kg_d, prod_kg_d) %>% View()

# Checking the units missing
missing <- ihs4 %>% 
  filter(is.na(prod_kg_d)) %>% 
  mutate(prod_kg_d = ifelse(cons_quant == prod_quant & cons_unitA == prod_unitA, 
                            kg_d, prod_kg_d)) %>%
  filter(is.na(prod_kg_d) & !is.na(prod_quant)) %>% 
  filter(prod_quant >0) %>% distinct(prod_unitA, item_code) 

nsu.df %>% filter(cons_unitA %in% unique(missing$prod_unitA)) %>% View()

# 1) Using food consumed in kg/d calculated from total consumption (same quantity & same units) to own production
ihs4  <- ihs4 %>% 
  mutate( comment = ifelse(is.na(ihs4$prod_kg_d) &
                             cons_quant == prod_quant & 
                             cons_unitA == prod_unitA,
                          "Quantity imputed from variable \"kg_d\" because quantity and units were the same", NA), 
    prod_kg_d = ifelse(is.na(ihs4$prod_kg_d) &
                          cons_quant == prod_quant & 
                            cons_unitA == prod_unitA, 
                             kg_d, prod_kg_d)) 
 
sum(is.na(ihs4$prod_kg_d))

# 2) Using food consumed in kg/d calculated from total consumption (same quantity, but different units) to own production - Assumption.         

ihs4  <- ihs4 %>%         
  mutate(
    comment = ifelse(is.na(prod_kg_d) & cons_quant == prod_quant,
        "Quantity imputed from variable \"kg_d\" using only quantity, as units were not the same", comment), 
    prod_kg_d = ifelse(is.na(prod_kg_d) & cons_quant == prod_quant, 
                            kg_d, prod_kg_d))


# 3) Calculating from factor from NSU from consumed to own production

ihs4  <- ihs4 %>% 
  mutate(comment = ifelse(is.na(prod_kg_d) & !is.na(prod_quant) & cons_unitA == prod_unitA,
                          "Quantity calculated from factor (same cons_unitA) different quantity", comment),        
      prod_kg_d = ifelse(is.na(prod_kg_d) & !is.na(prod_quant) & cons_unitA == prod_unitA,  
                            (prod_quant*factor)/7, prod_kg_d))

ihs4 %>% 
  filter(is.na(prod_kg_d) & !is.na(prod_quant)) %>% 
  filter(prod_quant >0) %>% select(ends_with("quant"), cons_unitA, 
                                   prod_unitA, item_code_st, region) %>% 
  left_join(., nsu.df, by = c("item_code_st"= "item_code", "region",
                              "prod_unitA" = "cons_unitA")) %>% 
  filter(is.na(ihs4_factor)) %>% distinct(prod_unitA, cons_unitA)

ihs4 %>% 
  filter(is.na(prod_kg_d)  & prod_quant >0) 

# Checking effect for maize
ihs4 %>% 
  filter(is.na(prod_kg_d)  & prod_quant >0) %>% 
  filter(grepl("maize", item, ignore.case = TRUE))

# 4) Calculating from factor from NSU from consumed to own production
# Because it was a fraction of total consumption and we did not have a value for NSU

ihs4  <- ihs4 %>% 
  mutate(comment = ifelse(is.na(prod_kg_d) & !is.na(prod_quant),
                          "Quantity calculated from factor (different cons_unitA) different quantity", comment),        
         prod_kg_d = ifelse(is.na(prod_kg_d) & !is.na(prod_quant),  
                            (prod_quant*factor)/7, prod_kg_d))
ihs4 %>% 
  filter(is.na(prod_kg_d)  & prod_quant >0) 

ihs4 %>% 
  filter(is.na(prod_kg_d)  & prod_quant >0) %>% 
  filter(grepl("maize", item, ignore.case = TRUE))

nsu.df %>% filter(cons_unitA %in% c("29", "38", "39", "28", "40", "30"))

nsu.df %>% filter(grepl("9B", measure_id))



ihs4 %>% 
  filter(is.na(prod_kg_d) & !is.na(prod_quant)) %>% 
  filter(prod_quant >0) %>% select(ends_with("quant"), cons_unitA, prod_unitA)

ihs4 %>% filter(item_code == "820" & !is.na(cons_quant))

variables <- c("item_code", "item", "region")
variables <- c("item_code", "item", "reside")

ihs4 %>% filter(grepl("maize|rice|sorg|millet|Wheat|grain", item, ignore.case = TRUE)) %>% 
  dplyr::group_by(across(variables)) %>% 
  dplyr::summarise( 
            prod_sum = sum(prod_kg_d, na.rm = TRUE),
            cons_sum = sum(kg_d, na.rm = TRUE),
            prod_perc = prod_sum/cons_sum*100
           # prod_mean= mean(prod_kg_d, na.rm = TRUE), 
          #  cons_mean = mean(kg_d, na.rm = TRUE), 
           # SD = sd(prod_kg_d, na.rm = TRUE)
          ) %>% arrange(desc(cons_sum)) %>%
           filter(region == "3") %>% View()



ihs4 %>% filter(grepl("maize", item, ignore.case = TRUE)) %>% 
#  filter(prod_kg_d <20) %>% 
  # filter(region == "3") %>% 
  ggplot(aes(as.factor(item), cons_kg)) + 
  geom_boxplot(aes(colour = as.factor(region))) +
  labs(x = "")

ihs4 %>% filter(grepl("fish", item, ignore.case = TRUE)) %>% 
  filter(prod_kg_d <20) %>% 
  ggplot(aes(as.factor(item), prod_kg_d)) + 
  geom_boxplot(aes(colour = as.factor(region))) +
  coord_flip()

ihs4 %>% filter(grepl("fish", item, ignore.case = TRUE)) %>% 
  filter(kg_d <20) %>% 
  ggplot(aes(as.factor(item), kg_d)) + 
  geom_boxplot(aes(colour = as.factor(region))) +
  coord_flip()


# MAP: 
source(here::here("boundaries.R"))

variables <- c("item_code", "item", "district")
variables <- c( "district")

c("millet", "sorghum", "rice", "maize")
food <- "maize"
food <- "fish"

mwi %>% left_join(., ihs4 %>%
        filter(grepl(food, item, ignore.case = TRUE)) %>% 
  dplyr::group_by(across(variables)) %>% 
  dplyr::summarise( 
    prod_sum = sum(prod_kg_d, na.rm = TRUE),
    cons_mean = mean(kg_d, na.rm = TRUE))) %>% 
  ggplot() + geom_sf(aes(fill = cons_mean)) +
  labs(title = food)


# Survey design ----

ihs4_survey <-  ihs4 %>%
  srvyr::as_survey_design(id = ea_id, strata = reside, weights = hh_wgt)


ihs4_survey %>% 
  #filter(grepl("maize|fish", item, ignore.case = TRUE)) %>% 
group_by(across(variables)) %>%
  summarise(prod_mean = survey_mean(kg_d, na.rm = TRUE, vartype = "ci"), 
            cons_mean = survey_mean(kg_d, na.rm = TRUE, vartype = "ci")) %>% 
mutate(prod_perc = prod_mean/cons_mean*100) %>%
  arrange(desc(cons_mean)) 

# Plotting crops by district

food <- "wheat"

mwi %>% left_join(., ihs4_survey %>% 
  filter(grepl(food, item, ignore.case = TRUE)) %>% 
  #filter(grepl("maize|fish", item, ignore.case = TRUE)) %>% 
  group_by(across(variables)) %>%
  summarise(cons_mean = survey_mean(kg_d, na.rm = TRUE, vartype = "ci"), 
            cons_sum = survey_total(kg_d, na.rm = TRUE, vartype = "ci"))) %>% 
  ggplot() + geom_sf(aes(fill = cons_mean)) +
  labs(title = food)
            