# This creates a graphic with three images

if(!exists("title_three_dashboard")){
title_three_dashboard <- "This is a test showing how the PCE inflation breaks down default"}


source("1c_load_cpi_data.R")

#SET UP DATA:
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


core_cpi <- cpi %>% filter(item_name == "All items less food and energy") %>%
  mutate(Pchange1a = (Pchange1+1)^12-1) %>%
  select(date, core_cpi = Pchange1a)


pce %>% filter(LineDescription == "PCE excluding food and energy") %>%
  left_join(core_cpi, by="date") %>%
  mutate(DataValue_P1a = (1+DataValue_P1)^12-1) %>%
  mutate(diff = core_cpi - DataValue_P1a) %>%
  group_by(year(date)) %>%
  summarize(avg_diff = mean(diff))

##### SERVICES

pce %>% filter(LineDescription == "PCE excluding food and energy") %>%
  left_join(core_cpi, by="date") %>%
  mutate(DataValue_P1a = (1+DataValue_P1)^12-1) %>%
  mutate(diff = core_cpi - DataValue_P1a) %>%
  
  ggplot(aes(date,diff)) + geom_line()

core_services_cpi <- cpi %>% filter(item_name == "Services less energy services") %>%
  mutate(Pchange1a = (Pchange1+1)^12-1) %>%
  select(date, core_cpi = Pchange1a)

pce %>% filter(LineDescription == "PCE services excluding energy") %>%
  left_join(core_services_cpi, by="date") %>%
  mutate(DataValue_P1a = (1+DataValue_P1)^12-1) %>%
  mutate(diff = core_cpi - DataValue_P1a) %>%
  
  ggplot(aes(date,diff)) + geom_line()


core_services_cpi <- cpi %>% filter(item_name == "Services less energy services") %>%
  mutate(Pchange1a = (Pchange1+1)^12-1) %>%
  select(date, core_cpi = Pchange1a)

pce %>% filter(LineDescription == "Market-based PCE services") %>%
  left_join(core_services_cpi, by="date") %>%
  mutate(DataValue_P1a = (1+DataValue_P1)^12-1) %>%
  mutate(diff = core_cpi - DataValue_P1a) %>%
  
  ggplot(aes(date,diff)) + geom_line()


pce %>% filter(LineDescription == "PCE services excluding energy") %>%
  left_join(core_services_cpi, by="date") %>%
  mutate(DataValue_P1a = (1+DataValue_P1)^12-1) %>%
  mutate(diff = core_cpi - DataValue_P1a) %>%
  group_by(year(date)) %>%
  summarize(avg_diff = mean(diff))




core_goods_fields <- c("Goods","Gasoline and other energy goods","Food and beverages purchased for off-premises consumption","Services","Electricity and gas","Housing")

core_analysis <- pce %>% filter(LineDescription %in% core_goods_fields) %>%
  filter(date >= "2018-01-01") %>%
  select(date, LineDescription, WDataValue_P1) %>%
  pivot_wider(names_from=LineDescription, values_from = WDataValue_P1) %>%
  clean_names() %>%
  mutate(core_goods = goods - food_and_beverages_purchased_for_off_premises_consumption - gasoline_and_other_energy_goods) %>%
  mutate(core_services = services - electricity_and_gas - housing) %>%
  mutate(core_inflation = core_goods + core_services) %>%
  pivot_longer(-date, names_to = "item_name", values_to = "WDataValue_P1") %>%
  filter(item_name %in% c("core_goods","core_services","housing")) %>%
  mutate(item_nameF = factor(item_name, levels = c("core_goods", "housing", "core_services")))

levels(core_analysis$item_nameF)[levels(core_analysis$item_nameF) == "core_goods"] <-"Core Goods"
levels(core_analysis$item_nameF)[levels(core_analysis$item_nameF) == "core_services"] <-"Core Services excluding Housing"
levels(core_analysis$item_nameF)[levels(core_analysis$item_nameF) == "housing"] <-"Housing"

pce_services_ex_housing <- core_analysis %>% filter(item_nameF == "Core Services excluding Housing") %>%
  mutate(WDataValue_P1 = (1+WDataValue_P1)^12-1) %>%
  select(date, item_name, value = WDataValue_P1) %>% mutate(type = "PCE", form="Core Services ex Housing")
  
  
  
  ###### Graphic 3: Services Breakdown #####
cpi_services_ex_housing <- cpi %>% filter(date > "2017-12-01", item_name %in% c("Services less energy services", "Shelter")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`), names_to = "item_name", values_to = "Wchange1a") %>%
  filter(item_name == "Rest of core services") %>%
  select(date, item_name, value = Wchange1a) %>% mutate(type = "CPI", form="Core Services ex Housing")

cpi_services_diff <- pce_services_ex_housing %>% rename(value_pce = value) %>% left_join(cpi_services_ex_housing, by="date") %>%
  mutate(diff = value_pce - value) %>%
  select(date, item_name = item_name.x, value = diff) %>% mutate(type = "PCE minus CPI Value", form="PCE minus CPI Value, Core Services ex Housing")

rbind(pce_services_ex_housing, cpi_services_ex_housing, cpi_services_diff) %>%
  ggplot(aes(date,value,color=type)) + geom_line() + theme_lass + facet_wrap(~form) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  labs(title="How Divergent Has Core Services ex Housing Been Between CPI and PCE?",
       subtitle="Monthly Weighted Contribution to Inflation, Annualized",
       caption="PRELIMINARY: PCE excludes 'Housing', CPI excludes 'Shelter', PCE weights are nominal consumption shares as a percent of total spending, Mike Konczal, Roosevelt Institute")
  
ggsave("graphics/divergence_q.png", dpi="retina", width = 12, height=6.75, units = "in")



