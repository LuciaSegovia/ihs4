# Outliers at consumption level

# Load required library
install.packages("data.table")
library(data.table)

# Convert data frames to data tables
setDT(cons_g_AFE_day)
setDT(ihs5_cons)

ihs5_cons_unique <- ihs5_cons[, .(afe = first(afe)), by = HHID]

# Perform the merge using data.table syntax
cons_g_AFE_day <- merge(
  cons_g_AFE_day, 
  ihs5_cons_unique, 
  by = 'HHID', 
  all.x = TRUE
)

# Create the new column kg_ame_d ----
## Note 1 -----
## The variable is "quantity of food consumed per 100g a day
## So you need to change the units from 100g to kg! if you want to 
## report it by kg
cons_g_AFE_day$kg_afe_d <- cons_g_AFE_day$quantity_100g_day / cons_g_AFE_day$afe

# Convert back to data.frame
cons_g_AFE_day <- as.data.frame(cons_g_AFE_day)

# Example hist consumption
cons_g_AFE_day %>% filter(item_code==114) %>% ggplot(., aes(x=kg_afe_d)) + 
  geom_histogram() +
  theme_bw()

# Convert to log
cons_g_AFE_day <- cons_g_AFE_day %>% mutate(log.kg_afe_d.plus1 =log(kg_afe_d+1))

# Calculate SDs and medians
# Calculate medians and SDs, this is where ihs4.summ is created
ihs5.summ <- cons_g_AFE_day %>% group_by(item_code) %>% 
  summarise(n = n(),
    mean.logplus1 = mean(log(kg_afe_d+1), na.rm = TRUE), 
    median = median(kg_afe_d, na.rm = TRUE),
    sd.logplus1 = sd(log(kg_afe_d+1), na.rm = TRUE)
  )

# Prepare summary with additional calculations
ihs5.summ.merg <- ihs5.summ %>% mutate(sd4 = sd.logplus1*4) %>% 
  mutate(sd5 = sd.logplus1*5) %>% 
  select(item_code, mean.logplus1, median, sd4, sd5)

ihs5.summ.merg <- ihs5.summ.merg %>% mutate(cut4 = sd4 + mean.logplus1) %>% 
  mutate(cut5 = sd5 + mean.logplus1) %>%
  select(item_code, median, cut4, cut5)

cons_g_AFE_day <- merge(x=cons_g_AFE_day, y=ihs5.summ.merg,
                        by.x='item_code', by.y='item_code', fill=-9999, 
                        all.x = TRUE) %>% arrange(HHID)


# Calculate outliers based on cut4 and cut5 (# Seria mejor hacerlo con cut3¿)
cons_g_AFE_day <- cons_g_AFE_day %>% mutate(ol4 = cut4-log.kg_afe_d.plus1)
cons_g_AFE_day <- cons_g_AFE_day %>% mutate(outlier4 = ifelse(ol4<0, 1, NA)) 

cons_g_AFE_day <- cons_g_AFE_day %>% mutate(ol5 = cut5-log.kg_afe_d.plus1)
cons_g_AFE_day <- cons_g_AFE_day %>% mutate(outlier5 = ifelse(ol5<0, 1, NA))

# Create unique outlier identifier
cons_g_AFE_day$outlier.id <- paste0(as.character(cons_g_AFE_day$HHID), "_", as.character(cons_g_AFE_day$item_code))

cons_g_AFE_day$missing_kg_afe_d_outlier <- NA

# New data set with outliers -----
## Note 2 ------
## I think here you are missing the variable "afe" in your
## selected variables, then you need to use it in line 81
outliers <- cons_g_AFE_day %>% filter(outlier5==1) %>%
  select(HHID, District, outlier5, item_code, food.group, kg_afe_d, outlier.id, 
         cut5, median) %>% arrange(item_code)

outliers$sdkg5 <- 10^outliers$cut5-1
outliers$kg_d_replace <-outliers$median * outliers$afe

write.csv(outliers, "C:/THESIS/THESISdata/outliers.csv") 

## Note3: ERROR ----
# Esto es porque la varibale "kg_d_replace" no se ha podido crear
## En el paso anterior!
# Creo que esta es la parte que me da errores. El merge aqui no funciona bien (no se ve en la tabla)
outliers.c <- outliers %>% select(outlier.id, kg_d_replace)
cons_g_AFE_day <- merge(x=cons_g_AFE_day, y=outliers.c , by.x='outlier.id', by.y='outlier.id', fill=-9999, all.x = TRUE)

# Intenté hacerlo asi pero tampoco me sale
cons_g_AFE_day <- cons_g_AFE_day %>%
  left_join(outliers.c, by = 'outlier.id')

cons_g_AFE_day <- cons_g_AFE_day %>% mutate(kg_d2=ifelse(!is.na(kg_d_replace),kg_d_replace, kg_afe_d))
write.csv(cons_g_AFE_day, "C:/THESIS/THESISdata/cons_processed.csv") ## Fix

# El resto son intentos de histograms y otras historias :)
# Visualisations for outliers per food type
cons_g_AFE_day <- cons_g_AFE_day %>%
  mutate(outlier = ifelse(HHID %in% outliers$HHID & item_code %in% outliers$item_code, 1, NA))

cons_g_AFE_day$outlier5 <- ifelse(is.na(cons_g_AFE_day$outlier5), "NA", as.character(cons_g_AFE_day$outlier5))
cons_g_AFE_day$outlier5 <- factor(cons_g_AFE_day$outlier5, levels = c("NA", "1"))

# Verify the levels of outlier5
levels(cons_g_AFE_day$outlier5)

# Plot the data for all food types together
ggplot(cons_g_AFE_day, aes(x = kg_afe_d, fill = outlier5)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7) + 
  scale_fill_manual(values = c("1" = "red", "NA" = "blue"), name = "Outlier") +
  labs(
    title = "Distribution of Food Types Consumed Per Day",
    x = "Kg/AFE/Day",
    y = "Frequency"
  ) + 
  theme_minimal()

fish_data <- cons_g_AFE_day %>%
  filter(food.group == "Fish")

ggplot(fish_data, aes(x = outliers$kg_d_replace, fill = outlier5)) + 
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7) + 
  scale_fill_manual(values = c("1" = "red", "NA" = "blue"), name = "Outlier") +
  labs(
    title = "Distribution of Fish Consumption Per Day",
    x = "Kg/AFE/Day",
    y = "Frequency"
  ) + 
  theme_minimal()

ggplot(cons_g_AFE_day, aes(x = outliers$kg_d_replace, fill = outlier5)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("NA" = "blue", "1" = "red"), name = "Outlier") +
  theme_minimal()


