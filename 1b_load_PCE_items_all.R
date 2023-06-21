library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
beaKey <- as.character(beaKey)
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

########### THE BIG ONE
PCE_Weight <- get_NIPA_data(beaKey, 'U20405', 'M', 'All', data_set_name = 'NIUnderlyingDetail')
PCE_Weight <- BEA_date_monthly(PCE_Weight)

pce <- get_NIPA_data(beaKey, 'U20404', 'M', 'All', data_set_name = 'NIUnderlyingDetail')
pce <- BEA_date_monthly(pce)

GDP_Weight <- PCE_Weight %>% filter(SeriesCode == "DPCERC") %>%
  select(date, TotalGDP = DataValue)

PCE_Weight <- PCE_Weight %>%
  left_join(GDP_Weight, by="date") %>%
  # The weight is approximated as nominal consumption shares as a percent of the total.
  mutate(PCEweight = DataValue/TotalGDP) %>%
  select(date, LineDescription, PCEweight)

pce <- pce %>%
  left_join(PCE_Weight, by=c('date' = 'date','LineDescription' = 'LineDescription'))

pce <- pce %>%
  group_by(SeriesCode) %>%
  mutate(DataValue_P1 = (DataValue - lag(DataValue,1))/lag(DataValue,1)) %>%
  # Use the lagged weight
  mutate(WDataValue_P1 = DataValue_P1*lag(PCEweight,1)) %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^12-1) %>%
  ungroup()

rm(PCE_Weight, GDP_Weight)

save(pce, file = "data/pce_long.RData")

