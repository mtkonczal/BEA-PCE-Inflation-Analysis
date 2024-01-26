library(quantmod)
library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

#beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
#beaKey <- as.character(beaKey)
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

########### THE BIG ONE
PCE_Weight <- get_NIPA_data(beaKey, 'U20405', 'A', '1980,1981,1982', data_set_name = 'NIUnderlyingDetail')

GDP_Weight <- PCE_Weight %>% filter(SeriesCode == "DPCERC") %>%
  select(TimePeriod, TotalGDP = DataValue)

PCE_Weight <- PCE_Weight %>%
  left_join(GDP_Weight, by="TimePeriod") %>%
  # The weight is approximated as nominal consumption shares as a percent of the total.
  mutate(PCEweight = DataValue/TotalGDP) %>%
  select(TimePeriod, LineDescription, PCEweight)

pce <- get_NIPA_data(beaKey, 'U20404', 'A', '1980,1981,1982', data_set_name = 'NIUnderlyingDetail')

PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'A', '1980,1981,1982', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- BEA_date_monthly(PCE_Q) %>% select(LineDescription, TimePeriod, Quantity = DataValue)

pce <- pce %>%
  left_join(PCE_Weight, by=c('TimePeriod' = 'TimePeriod','LineDescription' = 'LineDescription')) %>%
  left_join(PCE_Q, by=c('TimePeriod' = 'TimePeriod','LineDescription' = 'LineDescription'))

pce <- pce %>%
  group_by(SeriesCode) %>%
  mutate(DataValue_P1 = (DataValue - lag(DataValue,1))/lag(DataValue,1)) %>%
  # Use the lagged weight
  mutate(WDataValue_P1 = DataValue_P1*lag(PCEweight,1)) %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^1-1) %>%
  mutate(Quantity_P1 = (Quantity - lag(Quantity,1))/lag(Quantity,1)) %>%
  ungroup()

rm(PCE_Weight, GDP_Weight, PCE_Q)

#save(pce, file = "data/pce.RData")


# Function to remove outliers for graphics
remove_outliers <- function(x, multiplier = 1.5) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - multiplier * IQR
  upper_bound <- Q3 + multiplier * IQR
  x[(x < lower_bound) | (x > upper_bound)] <- NA
  return(x)
}

# Function for formatting percent signs in charts.
f_pct <- function(n) {
  return(str_c(sprintf('%.f', 100*n), "%"))
}

# Get preloaded categories and levels for each from data file.
lowest <- read_csv("data/pce_items_lowest.csv")

months_change <- 1

recent <- pce %>% group_by(TimePeriod) %>%
  # This removes duplicates values put in the BEA underlying table.
  distinct(LineNumber, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(LineDescription) %>%
  # Take the six-month change in inflation and quantities for all values and all dates.
  mutate(QuantityFinal = Quantity/lag(Quantity,months_change)-1,
         PriceFinal = DataValue/lag(DataValue,months_change)-1,
  ) %>%
  # Isolate the latest date and the comparison date.
  filter(TimePeriod == 1982 | TimePeriod == 1981) %>%
  # Take the difference in rates, so using a deceleration, and keep the latest weight.
  summarize(QuantityFinal = QuantityFinal[TimePeriod == 1982] - QuantityFinal[TimePeriod == 1981],
            PriceFinal = PriceFinal[TimePeriod == 1982] - PriceFinal[TimePeriod == 1981],
            weight = PCEweight[TimePeriod == 1982]) %>%
  ungroup()

quandrants <-
recent %>%
  # The next 5 lines isolates out the Level 4 categories we want.
  inner_join(lowest,by="LineDescription") %>%
  filter(category != "Aggregate") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  arrange(LineDescription) %>%
  filter(level == "Level 4") %>%
  # Assign to a quadrant.
  mutate(quadrant = case_when(
    QuantityFinal > 0 & PriceFinal > 0 ~ "1Demand+",
    QuantityFinal < 0 & PriceFinal > 0 ~ "2Supply-",
    QuantityFinal < 0 & PriceFinal < 0 ~ "3Demand-",
    QuantityFinal > 0 & PriceFinal < 0 ~ "4Supply+",
    TRUE ~ "Undefined"))

# This creates the csv file of all the Level 4 values to display in Github.
quandrants_print <- quandrants %>% select(-leading_spaces, -lowest)
quandrants_print$quadrant <- substr(quandrants_print$quadrant, 2, nchar(quandrants_print$quadrant))
write_csv(quandrants_print, "data/quandrants_data.csv")
quandrants_print %>% filter(category != "Food and Energy") %>% write_csv("data/pce_supply.csv")
pce_supply <- quandrants_print %>% filter(category != "Food and Energy")

# First do analysis for all core items.
all <- quandrants %>%
  filter(category != "Food and Energy") %>%
  # Just one category
  mutate(category = "All_Core") %>%
  group_by(quadrant, category) %>%
  # Sum of weight and weighted change.
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  # The sums above as a percent of total sum.
  mutate(Sn = n/sum(n), SnW = nW/sum(nW))

# Second, do analysis for categories goods and services.
g_s <-  quandrants %>%
  #execute code %>%
  filter(category != "Food and Energy") %>%
  group_by(quadrant, category) %>%
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(Sn = n/sum(n), SnW = nW/sum(nW)) %>%
  ungroup() %>%
  rbind(all)

# Formatting for chart 1.
chart1 <- g_s %>% filter(category != "Food and Energy") %>% select(-n,-nW) %>%
  pivot_wider(names_from = category, values_from = c(Sn,SnW)) %>%
  mutate(fall_all = if_else(SnW_All_Core > 0, SnW_All_Core/(SnW_All_Core[3]+SnW_All_Core[4]),as.numeric(NA)),
         fall_services = if_else(SnW_Goods > 0, SnW_Services/(SnW_Services[3]+SnW_Services[4]),as.numeric(NA)),
         fall_goods = if_else(SnW_Goods > 0, SnW_Goods/(SnW_Goods[3]+SnW_Goods[4]),as.numeric(NA))) %>%
  select(quadrant, Sn_All_Core, SnW_All_Core, fall_all, Sn_Goods, SnW_Goods, fall_goods,Sn_Services, SnW_Services, fall_services)

chart1$Sn_All_Core <- f_pct(chart1$Sn_All_Core)
chart1$Sn_Goods <- f_pct(chart1$Sn_Goods)
chart1$Sn_Services <- f_pct(chart1$Sn_Services)
chart1$fall_all <- f_pct(chart1$fall_all) %>% str_replace_all("NA%",as.character(NA))
chart1$fall_services <- f_pct(chart1$fall_services) %>% str_replace_all("NA%",as.character(NA))
chart1$fall_goods <- f_pct(chart1$fall_goods) %>% str_replace_all("NA%",as.character(NA))
chart1$SnW_All_Core <- round(chart1$SnW_All_Core,2)
chart1$SnW_Goods <- round(chart1$SnW_Goods,2)
chart1$SnW_Services <- round(chart1$SnW_Services,2)
chart1$quadrant <- substr(chart1$quadrant, 2, nchar(chart1$quadrant))


pce_supply %>%
  mutate(QuantityFinal = remove_outliers(QuantityFinal, 4)) %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight, color=category)) + geom_point(aes(fill="skyblue"), alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme(text = element_text(size = 22)) +
  #facet_wrap(~category) +
  labs(y = "Price (Inflation)",
       x = "Quantity",
       subtitle="Change in 12-month change, inflation and quantity, 123 PCE categories.\n12-month change Nov 2023 compared to baseline 12-month change November 2022.",
       title="80 Percent of Inflation Deceleration is Happening Where Supply is Increasing",
       caption = "NIPA tables 2.4.3U, 2.4.4U, 2.4.5U. Mike Konczal, Roosevelt Institute.") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +  theme(strip.background = element_blank())
