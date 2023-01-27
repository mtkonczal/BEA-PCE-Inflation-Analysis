library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

setwd("/Users/mkonczal/Documents/GitHub/BEA-PCE-Inflation-Analysis/")
beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
beaKey <- as.character(beaKey)
#beaKey <- "F5927FF7-CA99-4687-87EE-34135A3B9071"
source("0_helper_functions.R")
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

########### THE BIG ONE
PCE_Weight <- get_NIPA_data(beaKey, 'U20405', 'M', '2014,2015,2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Weight <- BEA_date_monthly(PCE_Weight)

GDP_Weight <- PCE_Weight %>% filter(SeriesCode == "DPCERC") %>%
  select(date, TotalGDP = DataValue)

PCE_Weight <- PCE_Weight %>%
  left_join(GDP_Weight, by="date") %>%
  mutate(PCEweight = DataValue/TotalGDP) %>%
  select(date, LineDescription, PCEweight)

pce <- get_NIPA_data(beaKey, 'U20404', 'M', '2014,2015,2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
pce <- BEA_date_monthly(pce)

PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'M', '2014,2015,2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- BEA_date_monthly(PCE_Q) %>% select(LineDescription, date, Quantity = DataValue)

pce <- pce %>%
  left_join(PCE_Weight, by=c('date' = 'date','LineDescription' = 'LineDescription')) %>%
  left_join(PCE_Q, by=c('date' = 'date','LineDescription' = 'LineDescription'))

pce <- pce %>%
  group_by(SeriesCode) %>%
  mutate(DataValue_P1 = (DataValue - lag(DataValue,1))/lag(DataValue,1)) %>%
  # TESTING WHAT MIGHT BE BEA'S ADVICE
  #  mutate(WDataValue_P1 = DataValue_P1*PCEweight) %>%
  mutate(WDataValue_P1 = DataValue_P1*lag(PCEweight,1)) %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^12-1) %>%
  mutate(Quantity_P1 = (Quantity - lag(Quantity,1))/lag(Quantity,1)) %>%
  ungroup()

rm(PCE_Weight, GDP_Weight, PCE_Q)

#save(pce, file = "data/pce.RData")

###############

sanity_check <- c("Goods","Services","PCE energy goods and services")


# CORE GOODS TEST
core_goods_fields <- c("Goods","Gasoline and other energy goods","Food and beverages purchased for off-premises consumption","Services","Electricity and gas")

core_analysis <- pce %>% filter(LineDescription %in% core_goods_fields) %>%
  filter(date >= "2018-01-01") %>%
  select(date, LineDescription, WDataValue_P1) %>%
  pivot_wider(names_from=LineDescription, values_from = WDataValue_P1) %>%
  clean_names() %>%
  mutate(core_goods = goods - food_and_beverages_purchased_for_off_premises_consumption - gasoline_and_other_energy_goods) %>%
  mutate(core_services = services - electricity_and_gas) %>%
  mutate(core_inflation = core_goods + core_services) %>%
  pivot_longer(-date, names_to = "item_name", values_to = "WDataValue_P1") %>%
  filter(item_name %in% c("core_goods","core_services","core_inflation"))

# Let's check how clsoe this is
BEA_core_indexes <- c("PCE goods excluding food and energy","PCE services excluding energy","PCE excluding food and energy")

BEA_core <- pce %>% filter(LineDescription %in% BEA_core_indexes) %>%
  filter(date >= "2021-01-01") %>%
  select(date, item_name = LineDescription, WDataValue_P1)

export <- rbind(BEA_core,core_analysis) %>%
  mutate(WDataValue_P1 = (1+WDataValue_P1)^12-1) %>%
  pivot_wider(names_from=item_name, values_from = WDataValue_P1) %>%
  clean_names()

write_csv(export, "exported_check.csv")

core_analysis %>% mutate(WDataValue_P1a = (WDataValue_P1+1)^12-1) %>%
  ggplot(aes(date,WDataValue_P1a)) + geom_bar(stat="identity") + facet_wrap(~item_name) + theme_classic()

##### THIS IS WHERE WE ARE
#MARKET BASED TEST
mb_pce_fields <- c("Market-based PCE services","Market-based PCE housing services")

#### MARKET BASED GRAPHIC
pce %>% filter(LineDescription %in% mb_pce_fields) %>%
  filter(date >= "2019-01-01") %>%
  select(date, LineDescription, WDataValue_P1) %>%
  pivot_wider(names_from=LineDescription, values_from = WDataValue_P1) %>%
  clean_names() %>%
  mutate(market_based_services_minus_shelter = market_based_pce_services - market_based_pce_housing_services) %>%
  pivot_longer(-date, names_to = "item_name", values_to = "WDataValue_P1") %>%
  mutate(WDataValue_P1a = (WDataValue_P1+1)^12-1) %>%
  
  ggplot(aes(date, WDataValue_P1a)) + geom_bar(stat="identity") + facet_wrap(~item_name) + theme_classic()

##### BIG ASIDE - OTHER THAN MARKET BASED ####

## Other than market based basket:
otm_basket_total <- c('DMARRG',
'DUTMRG',
'DREERG',
'DFFDRG',
'DMICRG',
'DNFRRG',
'DFARRG',
'DGAMRG',
'DFOORG',
'DIMPRG',
'DMUTRG',
'DPMIRG',
'DTRURG',
'DLIFRG',
'DMINRG',
'DIINRG',
'DPWCRG',
'DTINRG',
'DUNSRG',
'DSCWRG',
'DSADRG',
'DRELRG',
'DGIVRG',
'DDMIRG',
'DFORRG',
'DNPIRG')

otm_basket_services <- c('DFARRG',
                         'DGAMRG',
                         'DFOORG',
                         'DIMPRG',
                         'DMUTRG',
                         'DPMIRG',
                         'DTRURG',
                         'DLIFRG',
                         'DMINRG',
                         'DIINRG',
                         'DPWCRG',
                         'DTINRG',
                         'DUNSRG',
                         'DSCWRG',
                         'DSADRG',
                         'DRELRG',
                         'DGIVRG',
                         'DDMIRG',
                         'DFORRG')



pce %>% filter(SeriesCode %in% otm_basket_services) %>%
  filter(date > "2018-01-01") %>%
  group_by(date) %>%
  summarize(tot_otm_services = sum(WDataValue_P1)) %>%
  ungroup() %>%
  mutate(tot_otm_servicesa = (1+tot_otm_services)^12-1) %>%
  ggplot(aes(date, tot_otm_servicesa)) + geom_bar(stat="identity") + theme_classic()



########

  pce %>% filter(LineDescription == )
key_index <- c("Goods","Services","Housing and utilities")

# basic
pce %>% filter(LineDescription %in% key_index) %>%
  filter(date > "2019-01-01") %>%
  ggplot(aes(date,WDataValue_P1a)) + geom_bar(stat="identity") +
  facet_wrap(~LineDescription)

# seeing percent change annualized (not weighted) for comparison
pce %>% filter(LineDescription %in% key_index) %>%
  filter(date > "2019-01-01") %>%
  mutate(DataValue_P1a = (DataValue_P1+1)^12-1) %>%
  ggplot(aes(date,DataValue_P1a)) + geom_bar(stat="identity") +
  facet_wrap(~LineDescription)

ex_shelter <- pce %>% filter(LineDescription %in% key_index) %>%
  filter(date > "2019-01-01") %>%
  mutate(item_name = as.factor(LineDescription))

# Replace name (without breaking )
levels(ex_shelter$item_name)[levels(ex_shelter$item_name) == "Housing and utilities"] <-"Shelter"

ex_shelter %>%  
  ggplot(aes(date,WDataValue_P1a)) + geom_bar(stat="identity") +
  facet_wrap(~item_name)