library(tidyverse)
library(janitor)


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



#### Basic Index ####
nhs_index <-
  pce %>%
  select(date, LineDescription, WDataValue_P1, PCEweight) %>%
  group_by(date) %>%
  summarize(nhsWP1 = WDataValue_P1[LineDescription == "PCE services excluding energy"] - WDataValue_P1[LineDescription == "Housing"],
            nhs_weight = PCEweight[LineDescription == "PCE services excluding energy"] - PCEweight[LineDescription == "Housing"],
            nhs_excluding_finservices = nhsWP1 - WDataValue_P1[LineDescription == "Financial services furnished without payment (107)"],
            nhs_w_excluding_finservices = nhs_weight - PCEweight[LineDescription == "Financial services furnished without payment (107)"],
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
  ggplot(aes(date,value,color=type)) + geom_line() + theme_classic() + theme(legend.position = "bottom") +
  geom_line(data=cpi_nhs, aes(date, Wchange1a), color="purple")
  
  
  mutate(nhsWP1A = (nhsWP1A+1)^12-1) %>%
  mutate(index = nhsWP1/nhs_weight+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index))

nhs_index %>%
  mutate(six_change = (index/lag(index,6))^2-1) %>%
  ggplot(aes(date,six_change)) + geom_line() + theme_classic()








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

pce[grepl("hysic", pce$LineDescription),]

b %>% filter(item_name == "Pork")
mapping %>% filter(pce_description == "Pork")


pce_differences <- c("Financial services furnished without payment (107)", "Physician services (44)")
pce

tail(pce %>% filter(LineDescription == "Pork") %>% select(date, LineDescription, DataValue_P1))
tail(cpi_data %>% filter(item_name == "Pork", seasonal == "S") %>% select(date, item_name, value) %>% mutate(change = value/lag(value,1)-1))
tail(pce %>% filter(LineDescription == "Physician services (44)") %>% select(date, LineDescription, DataValue_P1))
tail(cpi_data %>% filter(item_name == "Physicians' services", seasonal == "S") %>% select(date, item_name, value) %>% mutate(change = value/lag(value,1)-1))
