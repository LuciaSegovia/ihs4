
#####------------START HERE: Loading dataset-----------#####

library(tidyverse)

#-------Read in the file------#
#

ihs4 <- read.csv(here::here("data", "inter-output", "hh_mod_g_processed.csv"))
wb_factor <- read.csv(here::here('data', "factors", 'wb_conv_factors_mwi.csv'), fileEncoding="UTF-8-BOM")
nsu.df <- read.csv(here::here('data', "factors", 'nsu.factors.csv'), fileEncoding="UTF-8-BOM")

# 

nsu.df <- nsu.df %>% 
  mutate(cons_unitA = str_extract(measure_id, "(?<=\\_)[:alnum:]+")) %>% 
  left_join(., ihs4 %>% distinct(cons_unitA, unit))

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


# Cleaning "own production" NSU

ihs4 %>% distinct(prod_unitA)

#Standardisation of NA values
ihs4$prod_unitA[!grepl("\\d", ihs4$prod_unitA)] <- NA
ihs4$prod_unit_oth[grepl("N\\/A", ihs4$prod_unit_oth)] <- NA

ihs4 %>% left_join(., wb_factor, by = c("item_code", "prod_unitA" = "unit_code")) %>% 
  filter(!is.na(prod_unitA) & is.na(wb_factor_n))

wb_factor %>% filter(item_code == "25")
wb_factor %>% filter(unit == "PAIL")

nsu.df %>% filter(grepl("DOZEN", unit, ignore.case = TRUE)) %>% 
  distinct(cons_unitA, unit)

ihs4 %>% filter(prod_unitA==26)
ihs4 %>% filter(grepl("DOZEN", prod_unit_oth, ignore.case = TRUE))
ihs4 %>% filter(grepl("DOZEN", unit, ignore.case = TRUE))

units_na <- ihs4 %>% 
  mutate(prod_nsu = ifelse(prod_unitA!=23, prod_unitA, 
        case_when(
prod_unit_oth == "KILOGRAM" ~ "1",
prod_unit_oth == "50 KG" ~ "2",
prod_unit_oth %in% c("PAIL (LARGE)") ~ "4C",
prod_unit_oth %in% c("PAILMEDIUM", "MEDIUMPALL") ~ "4B",
prod_unit_oth %in% c("PAIL (SMALL)", "PAILSMALL") ~ "4A",
prod_unit_oth %in% c("PAIL", "20L PAIL" ,"10LITREPAIL,"  ) ~ "4",
prod_unit_oth == "PAIL (MEDIUM)" ~ "4B",
prod_unit_oth %in% c("NO. 10 PLATE", "PLATE") ~ "6", 
prod_unit_oth == "NO. 12 PLATE" ~ "7", 
prod_unit_oth == "NO. 24 PLATE" ~ "7B", 
prod_unit_oth %in% c("BIGBUNCH") ~ "8C",
prod_unit_oth %in% c("BUNDLE", "BUCH", "BUSH") ~ "8",
prod_unit_oth == "PIECE (SMALL)"~"9A", 
prod_unit_oth %in% c("PIECE MEDIUM","PIECE (MEDIUM)") ~"9B",
prod_unit_oth == "PIECE (LARGE)" ~ "9C", 
prod_unit_oth %in% c("PIECES","PIECE", "MANGO", "PIECE (ONE)", "PEACE", "PIACE", "PEICE", "PIEC3", "MICE" ) ~ "9",
prod_unit_oth %in% c("HEAP", "HEA", "HEP") ~ "10", 
prod_unit_oth %in% c("HEAP", "HEAP (SMALL)","HEAP (MEDIUM)") ~ "10", 
prod_unit_oth == "HEAP (LARGE)" ~ "10C", 
prod_unit_oth == "BOTTLE" & item_code == "803"~ "15",
prod_unit_oth == "CUP" ~ "16",
prod_unit_oth == "GRAMS"~ "18", 
prod_unit_oth %in% c("TEASPOON", "SPOON") ~ "20",
prod_unit_oth %in% c("SATCHET (MEDIUM)") ~ "22B",
prod_unit_oth %in% c("SATCHET (SMALL)") ~ "22B",
prod_unit_oth %in% c("TUBE /SACHET","SATCHET (500 GRAM)", "TUBE") ~ "22",
prod_unit_oth %in% c("TINA(FLAT)", "TINA(SMALL)" ) ~ "25A",
prod_unit_oth %in% c("TINA (HEAPED)", "MEDIUM (PLATE)") ~ "25B",
prod_unit_oth %in% c("5L BUCKET", "5 LITRES", "BUCKET") ~ "26", 
prod_unit_oth %in% c("PACKET 100 GRAM", "PACKET 150 GRAM") ~ "34",
prod_unit_oth == "SATCHET (20 GRAM)"~ "41",
prod_unit_oth %in% c("SATCHET (MEDIUM)", "TUBE (MEDIUM)") ~ "42",
prod_unit_oth %in%c("TUBE (LARGE)","SATCHET (LARGE)", "SATCHET (150 GRAM)") ~ "43", 
prod_unit_oth %in%c("CLUSTER") ~ "44", 
prod_unit_oth == "TABLESPOON"~ "50", 
prod_unit_oth == "PACKET (SMALL)"~ "54", 
prod_unit_oth == "PACKETS"~ "60",
prod_unit_oth %in% c("SATCHET (250 GRAM)","PACKET 250 GRAM") ~  "65", 
prod_unit_oth == "TIN"~ "72",
prod_unit_oth == "TIN (500G)"~ "73"))) %>% 
  filter(prod_unitA==23 & is.na(prod_nsu))  %>% 
  arrange(desc(kg_d)) %>% 
  distinct(prod_unit_oth) %>% pull()



code_list <- vector()

for(i in 1:length(units_na)){
  
  code_list[i] <- wb_factor %>% filter(grepl(units_na[i], unit, ignore.case = TRUE)) %>% 
    distinct(unit_code)
}


# Consumption calculation 

ihs4 <- ihs4 %>% 
  mutate(prod_nsu = ifelse(prod_unitA!=23, prod_unitA, 
                           case_when(
                             prod_unit_oth == "KILOGRAM" ~ "1",
                             prod_unit_oth == "50 KG" ~ "2",
                             prod_unit_oth %in% c("PAIL (LARGE)") ~ "4C",
                             prod_unit_oth %in% c("PAILMEDIUM", "MEDIUMPALL") ~ "4B",
                             prod_unit_oth %in% c("PAIL (SMALL)", "PAILSMALL") ~ "4A",
                             prod_unit_oth %in% c("PAIL", "20L PAIL" ,"10LITREPAIL,"  ) ~ "4",
                             prod_unit_oth == "PAIL (MEDIUM)" ~ "4B",
                             prod_unit_oth %in% c("NO. 10 PLATE", "PLATE") ~ "6", 
                             prod_unit_oth == "NO. 12 PLATE" ~ "7", 
                             prod_unit_oth == "NO. 24 PLATE" ~ "7B", 
                             prod_unit_oth %in% c("BIGBUNCH") ~ "8C",
                             prod_unit_oth %in% c("BUNDLE", "BUCH", "BUSH") ~ "8",
                             prod_unit_oth == "PIECE (SMALL)"~"9A", 
                             prod_unit_oth %in% c("PIECE MEDIUM","PIECE (MEDIUM)") ~"9B",
                             prod_unit_oth == "PIECE (LARGE)" ~ "9C", 
                             prod_unit_oth %in% c("PIECES","PIECE", "MANGO", "PIECE (ONE)", "PEACE", "PIACE", "PEICE", "PIEC3", "MICE" ) ~ "9",
                             prod_unit_oth %in% c("HEAP", "HEA", "HEP") ~ "10", 
                             prod_unit_oth %in% c("HEAP", "HEAP (SMALL)","HEAP (MEDIUM)") ~ "10", 
                             prod_unit_oth == "HEAP (LARGE)" ~ "10C", 
                             prod_unit_oth == "BOTTLE" & item_code == "803"~ "15",
                             prod_unit_oth == "CUP" ~ "16",
                             prod_unit_oth == "GRAMS"~ "18", 
                             prod_unit_oth %in% c("TEASPOON", "SPOON") ~ "20",
                             prod_unit_oth %in% c("SATCHET (MEDIUM)") ~ "22B",
                             prod_unit_oth %in% c("SATCHET (SMALL)") ~ "22B",
                             prod_unit_oth %in% c("TUBE /SACHET","SATCHET (500 GRAM)", "TUBE") ~ "22",
                             prod_unit_oth %in% c("TINA(FLAT)", "TINA(SMALL)" ) ~ "25A",
                             prod_unit_oth %in% c("TINA (HEAPED)", "MEDIUM (PLATE)") ~ "25B",
                             prod_unit_oth %in% c("5L BUCKET", "5 LITRES", "BUCKET") ~ "26", 
                             prod_unit_oth %in% c("PACKET 100 GRAM", "PACKET 150 GRAM") ~ "34",
                             prod_unit_oth == "SATCHET (20 GRAM)"~ "41",
                             prod_unit_oth %in% c("SATCHET (MEDIUM)", "TUBE (MEDIUM)") ~ "42",
                             prod_unit_oth %in%c("TUBE (LARGE)","SATCHET (LARGE)", "SATCHET (150 GRAM)") ~ "43", 
                             prod_unit_oth %in%c("CLUSTER") ~ "44", 
                             prod_unit_oth == "TABLESPOON"~ "50", 
                             prod_unit_oth == "PACKET (SMALL)"~ "54", 
                             prod_unit_oth == "PACKETS"~ "60",
                             prod_unit_oth %in% c("SATCHET (250 GRAM)","PACKET 250 GRAM") ~  "65", 
                             prod_unit_oth == "TIN"~ "72",
                             prod_unit_oth == "TIN (500G)"~ "73"))) 

ihs4 %>% filter(is.na(prod_nsu) & !is.na(prod_quant) & prod_quant >0)

ihs4 %>% 
  mutate(dummy_prod = ifelse(cons_quant == prod_quant & cons_unitA == prod_unitA, 
                             kg_d, NA)) %>%
           filter(is.na(dummy_prod) & is.na(prod_quant) & prod_quant ==0)

ihs4  <- ihs4 %>% 
  mutate(prod_kg_d = ifelse(cons_quant == prod_quant & cons_unitA == prod_unitA, 
                             kg_d, NA)) 

ihs4 %>% filter(item_code == "820" & !is.na(cons_quant))

ihs4 %>% filter(grepl("maize", item, ignore.case = TRUE)) %>% 
  group_by(item_code, item, region ) %>% 
  summarise( N = n(),
            mean = mean(prod_kg_d, na.rm = TRUE), 
            SD = sd(prod_kg_d, na.rm = TRUE))

2481+4220+5736
  

