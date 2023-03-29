
#####------------START HERE: Loading dataset-----------#####

library(tidyverse)

#-------Read in the file------#


ihs4 <- read.csv(here::here("data", "inter-output", "hh_mod_g_processed.csv"))
nsu.df <- read.csv(here::here('data', "factors", 'nsu.factors.csv'), fileEncoding="UTF-8-BOM")


# Prepare NSU file

nsu.df <- nsu.df %>% 
  pivot_longer(., 
               cols = starts_with("ihs4factor"),
               names_to = "region", 
               values_to = "ihs4_factor") %>% 
#  mutate(cons_unitA = str_extract(measure_id, "(?<=\\_)[:alnum:]+")) %>% 
 separate(measure_id, c("item_code", "cons_unitA"), sep ="_", remove = FALSE) %>% 
  left_join(., ihs4 %>% distinct(cons_unitA, unit)) 

nsu.df$region[nsu.df$region == "ihs4factor_n"] <- 1
nsu.df$region[nsu.df$region == "ihs4factor_c"] <- 2
nsu.df$region[nsu.df$region == "ihs4factor_s"] <- 3

nsu.df$region<- as.integer(nsu.df$region)
nsu.df$item_code <- as.integer(nsu.df$item_code)

# Own-production from total consumption

ihs4 %>% 
  mutate(dummy_prod = ifelse(cons_quant == prod_quant & cons_unitA == prod_unitA, 
                             kg_d, NA)) %>%
           filter(is.na(dummy_prod) & !is.na(prod_quant)) %>% 
          filter(prod_quant >0) %>% select(ends_with("quant"), cons_unitA, prod_unitA)

# 1) Using food consumed in kg/d calculated from total consumption (same quantity & same units) to own production
ihs4  <- ihs4 %>% 
  mutate(prod_kg_d = ifelse(cons_quant == prod_quant & cons_unitA == prod_unitA, 
                             kg_d, NA), 
         comment = ifelse(!is.na(prod_kg_d), "Quantity imputed from variable \"kg_d\" because quantity and units were the same", NA))
 
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
  filter(is.na(prod_kg_d)  & prod_quant >0) %>% 
  filter(grepl("maize", item, ignore.case = TRUE))

nsu.df %>% filter(cons_unitA %in% c("29", "38", "39", "28", "40", "30"))

nsu.df %>% filter(grepl("9B", measure_id))



ihs4 %>% 
  filter(is.na(prod_kg_d) & !is.na(prod_quant)) %>% 
  filter(prod_quant >0) %>% select(ends_with("quant"), cons_unitA, prod_unitA)

ihs4 %>% filter(item_code == "820" & !is.na(cons_quant))

ihs4 %>% filter(grepl("maize", item, ignore.case = TRUE)) %>% 
  dplyr::group_by(item_code, item, region) %>% 
  dplyr::summarise( 
            prod_mean= mean(prod_kg_d, na.rm = TRUE), 
            cons_mean = mean(kg_d, na.rm = TRUE), 
            SD = sd(prod_kg_d, na.rm = TRUE)) 

  

