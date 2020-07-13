
#####------------START HERE: Loading dataset-----------#####

library(tidyverse)

#-------Read in the file------#
#

#ihs4 <- read.csv(here::here('data', 'hces', 'mwi','ihs4', 'hh_mod_g1.csv')

#Rename variables

ihs4 %>% rename(id ='case_id',
                consYN =  'hh_g01',
                item_code = 'hh_g02',
                item_oth =  'hh_g01_oth',
                cons_quant =  'hh_g03a',
                cons_unitA =  'hh_g03b',
                cons_unit = 'hh_g03b_label',
                cons_unit_oth = 'hh_g03b_oth',
                purc_quant = 'hh_g04a', 
                gift_quant =  'hh_g07a',
                prod_quant =  'hh_g06a',
                purc_unitA = 'hh_g04b',
                gift_unitA =  'hh_g04b',
                purc_unit = 'hh_g04b_label',
                gift_unit = 'hh_g04b_label',
                purc_unit_oth = 'hh_g04b_oth',
                gift_unit_oth = 'hh_g04b_oth',
                purc_unit_size= 'hh_g04c',
                gift_unit_size= 'hh_g04c',
                purc_price =  'hh_g05',
                prod_unitA =  'hh_g06b',
                prod_unit = 'hh_g06b_label',
                prod_unit_oth = 'hh_g06b_oth',
                cons_unit_size= 'hh_g03c',
                prod_unit_size= 'hh_g06c') 


#Loading the food items code

#There are two discrepancy between the dataset and the questionnaire pdf
#1) there are other food items that are not presented/ coded in the questionnaire (fish)
#2) the food groups are included in the dataset, the hundreds 

HHitem_code <- read_csv(here::here('mwi_item_code.csv')) %>% 
  filter(!code %in% c(200, 300, 400, 500, 600, 700, 800, 809, 819, 900)) #removing food groups codes (duplicated)


#Creating a new variable, 'food category' for the food items acc. to ihs4 categories
#plus the dried fish 
#UPDATE: Due to the 'variety' of codes for 'Meat, Fish and Animal products', that would be coded at the
#end

HHitem_code <-  HHitem_code %>% 
  mutate(Category = ifelse(code %in% 100:117, 'Cereals, Grains and Cereal Products',
                           ifelse(code %in% 201:209, 'Roots, Tubers, and Plantains',
                                  ifelse(code %in% 301:314, 'Nuts and Pulses', 
                                         ifelse(code %in% 401:414, 'Vegetables',
                                                ifelse(code %in% 601:610, 'Fruits',
                                                       ifelse(code %in% 820:838, 'Cooked Foods from Vendors',
                                                              ifelse(code %in% 701:709, 'Milk and Milk Products',
                                                                     ifelse(code %in% 801:804, 'Sugar, Fats, and Oil', 
                                                                            ifelse(code %in% 901:916, 'Beverages',
                                                                                   ifelse(code %in% 810:818, 'Spices & Miscellaneous', 'Meat, Fish and Animal products')))))))))))

