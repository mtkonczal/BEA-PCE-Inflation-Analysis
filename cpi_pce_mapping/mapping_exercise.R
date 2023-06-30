library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

load("data/last_cpi_data.RData")
load("data/pce_long.RData")
mapping <- read_csv("data/cpi_pce_item_mapping.csv") %>% clean_names()

pce %>% filter(LineDescription == "Food services")

#### Set up CPI ####
cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(three_months = (lag(value, 1)/lag(value, 4)-1)) %>%
  mutate(Wthree_months = (three_months*weight)/100) %>%
  mutate(Wthree_monthsA = (1 + Wthree_months)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()

cpi_nhs <- cpi %>% filter(date > "2020-12-01", item_name %in% c("Services less energy services", "Shelter")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`), names_to = "item_name", values_to = "Wchange1a") %>%
  filter(item_name == "Rest of core services")

cpi_nhs_index <-
  cpi %>%
  mutate(weight_nhs = if_else(!is.na(weight_2023), weight_2023, weight)) %>%
  select(date, item_name, Wchange1, weight_nhs) %>%
  mutate(weight_nhs = weight_nhs/100) %>%
  group_by(date) %>%
  summarize(nhsWP1 = Wchange1[item_name == "Services less energy services"] - Wchange1[item_name == "Shelter"],
            nhs_weight = weight_nhs[item_name == "Services less energy services"] - weight_nhs[item_name == "Shelter"],
  ) %>%
  ungroup() %>%
  mutate(nhsWP1A = nhsWP1/nhs_weight) %>%
  mutate(nhsWP1A = (nhsWP1A+1)^12-1) %>%
  mutate(index = nhsWP1/nhs_weight+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index), type = "cpi")
#####

#### Basic Index ####
nhs_index <-
  pce %>%
  select(date, LineDescription, WDataValue_P1, PCEweight) %>%
  group_by(date) %>%
  summarize(nhsWP1 = WDataValue_P1[LineDescription == "PCE services excluding energy"] - WDataValue_P1[LineDescription == "Housing"],
            nhs_weight = PCEweight[LineDescription == "PCE services excluding energy"] - PCEweight[LineDescription == "Housing"],
  ) %>%
  ungroup() %>%
  mutate(nhsWP1A = nhsWP1/nhs_weight) %>%
  mutate(nhsWP1A = (nhsWP1A+1)^12-1) %>%
  mutate(index = nhsWP1/nhs_weight+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index))
#####


#### Attempt One ####
nhs_index_1 <-
  pce %>%
  select(date, LineDescription, WDataValue_P1, PCEweight) %>%
  group_by(date) %>%
  summarize(nhsWP1 = WDataValue_P1[LineDescription == "PCE services excluding energy"] - WDataValue_P1[LineDescription == "Housing"],
            nhs_weight = PCEweight[LineDescription == "PCE services excluding energy"] - PCEweight[LineDescription == "Housing"],
            nhs_excluding_finservices = nhsWP1 - WDataValue_P1[LineDescription == "Financial services furnished without payment (107)"],
            nhs_w_excluding_finservices = nhs_weight - PCEweight[LineDescription == "Financial services furnished without payment (107)"],
            nhs_ex_fin_hos = nhs_excluding_finservices - WDataValue_P1[LineDescription == "Hospital and nursing home services"],
            nhs_ex_fin_hos_w = nhs_w_excluding_finservices - PCEweight[LineDescription == "Hospital and nursing home services"],
            nhs_ex_fin_hos_food = nhs_ex_fin_hos - WDataValue_P1[LineDescription == "Food services"],
            nhs_ex_fin_hos_food_w = nhs_ex_fin_hos_w - PCEweight[LineDescription == "Food services"],
            
            ) %>%
  ungroup() %>%
  mutate(nhsWP1 = nhsWP1/nhs_weight) %>%
  select(-nhs_weight) %>%
  
  mutate(nhs_excluding_finservices = nhs_excluding_finservices/nhs_w_excluding_finservices) %>%
  select(-nhs_w_excluding_finservices) %>%
  
  mutate(nhs_ex_fin_hos = nhs_ex_fin_hos/nhs_ex_fin_hos_w) %>%
  select(-nhs_ex_fin_hos_w) %>%
  
  mutate(nhs_ex_fin_hos_food = nhs_ex_fin_hos_food/nhs_ex_fin_hos_food_w) %>%
  select(-nhs_ex_fin_hos_food_w) %>%
  
  filter(!is.na(nhs_excluding_finservices), date > "2019-01-01") %>%
  pivot_longer(cols =c("nhsWP1","nhs_excluding_finservices","nhs_ex_fin_hos","nhs_ex_fin_hos_food"), names_to = "type", values_to = "value") %>%
  mutate(value = (value+1)^12-1) %>%
  filter(date > "2022-05-01") %>%
  ggplot(aes(date,value,color=type)) + geom_line() + theme_classic() + theme(legend.position = "bottom") +
  geom_line(data=cpi_nhs_index[cpi_nhs_index$date > "2022-05-01",], aes(date, nhsWP1A), color="blue",
            linetype="dotted", linesize=1.2)
  
  
  mutate(nhsWP1A = (nhsWP1A+1)^12-1) %>%
  mutate(index = nhsWP1/nhs_weight+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index))

nhs_index %>%
  mutate(six_change = (index/lag(index,6))^2-1) %>%
  ggplot(aes(date,six_change)) + geom_line() + theme_classic() +
  scale_y_continuous(labels = percent) +
  labs(y="", subtitle="PCE non-housing services, six-month change, annualized. Weights approximated as nominal consumption shares as a percent of the total.",
       caption="NIPA Tables 2.4.4U and 2.4.5U. Mike Konczal",
       title="Wow, That's Crazy") +
  theme(plot.title.position = "plot")


ggsave("long_nhs.png", dpi="retina", width = 12, height=6.75, units = "in")






pce$OtherSeriesCode <- sub("(.)$", "C", pce$SeriesCode)

intersect_pce <- intersect(mapping$pce_series_code, pce$OtherSeriesCode)
a <- pce %>% filter(pce$OtherSeriesCode %in% intersect_pce) %>% select(LineDescription)
unique(a)


str(cpi_data)
str(mapping$pce_series_code)
head(cpi_data)

cpi_data$item_code

b <- cpi_data %>% distinct(item_code, .keep_all = TRUE) %>% select(item_code, item_name)

intersect(mapping$eli, b$item_code)

tail(pce %>% filter(LineDescription == "Pork") %>% select(date, LineDescription, DataValue_P1))
tail(cpi_data %>% filter(item_name == "Pork", seasonal == "S") %>% select(date, item_name, value) %>% mutate(change = value/lag(value,1)-1))


mapping %>% filter(pce_description == "Pork")

pce[grepl("food and energy", pce$LineDescription),]$LineDescription

b %>% filter(item_name == "Pork")
mapping %>% filter(pce_description == "Pork")


pce_differences <- c("Financial services furnished without payment (107)", "Physician services (44)")
pce

tail(pce %>% filter(LineDescription == "Pork") %>% select(date, LineDescription, DataValue_P1))
tail(cpi_data %>% filter(item_name == "Pork", seasonal == "S") %>% select(date, item_name, value) %>% mutate(change = value/lag(value,1)-1))
tail(pce %>% filter(LineDescription == "Physician services (44)") %>% select(date, LineDescription, DataValue_P1))
tail(cpi_data %>% filter(item_name == "Physicians' services", seasonal == "S") %>% select(date, item_name, value) %>% mutate(change = value/lag(value,1)-1))


#### Airlines ####
pce[grepl("Food", pce$LineDescription),] %>% select(LineDescription) %>% distinct()

cpi_air <-
cpi_data %>% filter(item_name == "Airline fares", seasonal == "S") %>%
  mutate(change = value/lag(value,1)-1, type = "cpi") %>%
  select(date, change, type)

pce_air <-
  pce %>% filter(LineDescription == "Air transportation (64)") %>%
  mutate(type = "pce") %>%
  select(date, change = DataValue_P1, type)


pce %>% filter(LineDescription %in% c("Food services","Food and beverages purchased for off-premises consumption")) %>%
  filter(year(date) > 2015) %>%
  mutate(DataValue_P1 = (DataValue_P1+1)^12-1) %>%
  ggplot(aes(date,DataValue_P1, color=LineDescription)) + geom_line() +
  theme_classic() + theme(legend.position = "bottom") +
  geom_hline(yintercept = 0)


##### First graphic ####
cpi_core <- cpi_data %>% filter(item_name == "All items less food and energy", seasonal == "S") %>%
  mutate(diff = value/lag(value,1), diff = diff^12-1) %>%
  select(date, diff) %>% mutate(type = "cpi")

pce_core <-
  pce %>% filter(LineDescription == "PCE excluding food and energy") %>%
  select(date, diff = DataValue_P1) %>%
  mutate(diff = (diff+1)^12-1, type = "pce")

rbind(cpi_core, pce_core) %>%
  filter(year(date)>2014) %>%
  ggplot(aes(date,diff,color=type)) + geom_line() + theme_classic() +
  theme(legend.position = "bottom")

#####
nhs_index %>% mutate(type = "pce") %>%
  rbind(cpi_nhs_index) %>%
  filter(year(date)>2014) %>%
  ggplot(aes(date,nhsWP1A,color=type)) + geom_line() + theme_classic() +
  theme(legend.position = "bottom")

nhs_index %>% mutate(type = "pce") %>%
  rbind(cpi_nhs_index) %>%
  filter(year(date)>2014) %>%
  group_by(type) %>%
  mutate(threemonths = index/lag(index,3), threemonths = threemonths^4-1) %>%
  ungroup() %>%
  ggplot(aes(date,threemonths,color=type)) + geom_line() + theme_classic() +
  theme(legend.position = "bottom")
####

tail(cpi %>% filter(item_name == "Food away from home") %>% select(date, item_name, Wchange1a))


#### This ####

