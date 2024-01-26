library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)
library(quantmod)
library(gt)

check_items <- c("PCE services excluding energy","PCE goods excluding food and energy", "PCE food and energy","Housing", "Personal consumption expenditures")

a <-
pce %>% 
  select(date, DataValue, PCEweight, LineDescription) %>%
  filter(LineDescription %in% check_items) %>%
  group_by(LineDescription) %>%
  mutate(DataValue_P3 = (DataValue/lag(DataValue,3))-1) %>%
  # Use the lagged weight
  mutate(WDataValue_P3 = DataValue_P3*lag(PCEweight,1)) %>%
  mutate(WDataValue_P3a = (1+WDataValue_P3)^4-1) %>%
  # Now six month
  mutate(DataValue_P6 = (DataValue/lag(DataValue,6))-1) %>%
  mutate(WDataValue_P6 = DataValue_P6*lag(PCEweight,1)) %>%
  mutate(WDataValue_P6a = (1+WDataValue_P6)^2-1) %>%
  mutate(before = (DataValue[date == "2019-12-01"]/DataValue[date == "2017-12-01"]) - 1,
         Wbefore = before*PCEweight[date=="2019-11-01"],
         Wbeforea = (1+Wbefore)^(0.5) - 1) %>%
  filter(date == max(date))
    
b <- a %>% filter(LineDescription %in% c("PCE services excluding energy", "Housing")) %>%
  select(WDataValue_P3, WDataValue_P6, Wbefore) %>%
  pivot_wider(names_from = "LineDescription", values_from = WDataValue_P3:Wbefore) %>%
  clean_names() %>%
  summarize(WDataValue_P3 = w_data_value_p3_pce_services_excluding_energy - w_data_value_p3_housing,
            WDataValue_P6 = w_data_value_p6_pce_services_excluding_energy - w_data_value_p6_housing,
            Wbefore = wbefore_pce_services_excluding_energy - wbefore_housing) %>%
  mutate(WDataValue_P3a = (1+WDataValue_P3)^4-1,
         WDataValue_P6a = (1+WDataValue_P6)^2-1,
         Wbeforea = (1+Wbefore)^(0.5) - 1,
         LineDescription = "4 - Core Non-Housing Services") %>%
  relocate(LineDescription)

table <- a %>%
  select(LineDescription, WDataValue_P3, WDataValue_P6, Wbefore, WDataValue_P3a, WDataValue_P6a, Wbeforea) %>%
  rbind(b) %>%
  select(LineDescription, WDataValue_P3a, WDataValue_P6a, Wbeforea) %>%
  filter(LineDescription != "PCE services excluding energy") %>%
  mutate(LineDescription = case_when(
    LineDescription == "Personal consumption expenditures" ~ "1 - Total PCE Inflation",
    LineDescription == "PCE food and energy" ~ "2 - Food and Energy",
    LineDescription == "PCE goods excluding food and energy" ~ "3 - Core Goods",
    LineDescription == "Housing" ~ "5 - Housing",
    .default = LineDescription
  )) %>%
  arrange(LineDescription) %>%
  mutate(LineDescription = substr(LineDescription, 5, nchar(LineDescription))) %>%
  ungroup()
  
table %>%
  gt(rowname_col = "LineDescription") %>%
  tab_header(
    title = "Four Components of PCE Inflation",
    subtitle = "All data annualized"
  ) %>%
  cols_label(
    WDataValue_P3a = "Past 3 Months",
    WDataValue_P6a = "Past 6 Months",
    Wbeforea = "2018-2019"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  fmt_percent(
    columns = c(WDataValue_P3a, WDataValue_P6a, Wbeforea)
  ) %>%
  tab_footnote(
    footnote = "Source: [your data source]",
    locations = cells_column_labels(columns = vars(LineDescription))
  ) %>%
  opt_stylize(style = 6, color = "blue") %>%
  tab_source_note(
    source_note = "Total +/- ~0.1% due to rounding. BEA, Author's Analysis. Mike Konczal, Roosevelt Institute."
  ) %>%
  gtsave(., filename="graphics/inflation_chart.png")

