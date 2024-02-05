library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)

source("load_flatfiles.R")
long_pce_org <- load_pce_data()

long_pce <- long_pce_org %>% rename(LineDescription = series_label)

#First make a graphic of non-housing services
#### Basic Index ####
nhs_index <-
  long_pce %>%
  select(date, LineDescription, WDataValue_P1, PCEweight) %>%
  group_by(date) %>%
  summarize(nhsWP1 = WDataValue_P1[LineDescription == "PCE services excluding energy"] - WDataValue_P1[LineDescription == "Housing"],
            nhs_weight = PCEweight[LineDescription == "PCE services excluding energy"] - PCEweight[LineDescription == "Housing"],
           mb_nhsWP1 = WDataValue_P1[LineDescription == "Market-based PCE services"] - WDataValue_P1[LineDescription == "Market-based PCE housing services"], #- WDataValue_P1[LineDescription == "Market-based PCE household utilities"],
            mb_nhs_weight = PCEweight[LineDescription == "Market-based PCE services"] - PCEweight[LineDescription == "Market-based PCE housing services"] #- PCEweight[LineDescription == "Market-based PCE household utilities"]
  ) %>%
  ungroup() %>%
  mutate(nhsWP1A = nhsWP1/nhs_weight) %>%
  mutate(index = nhsWP1/nhs_weight+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index)) %>%
  mutate(mb_nhsWP1A = mb_nhsWP1/mb_nhs_weight) %>%
  mutate(mb_index = mb_nhsWP1/mb_nhs_weight+1) %>% filter(!is.na(mb_index)) %>%
  mutate(mb_index = cumprod(mb_index))
#####

nhs_index_display <-
  nhs_index %>%
  select(date, core_non_housing_services = index, market_based_non_housing_services = mb_index) %>%
  mutate(core_non_housing_services = 100*core_non_housing_services/core_non_housing_services[date == "2017-12-01"],
         market_based_non_housing_services = 100*market_based_non_housing_services/market_based_non_housing_services[date == "2017-12-01"])
  
