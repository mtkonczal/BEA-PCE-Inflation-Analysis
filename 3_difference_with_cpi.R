# This creates a graphic with three images

if(!exists("title_three_dashboard")){
title_three_dashboard <- "This is a test showing how the PCE inflation breaks down default"}

pce %>% filter(LineDescription == "PCE excluding food and energy")

#First make a graphic of non-housing services
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

##### Graphic for Non-Housing Services ####
nhs_index_1_month <- nhs_index %>%
  mutate(month1 = (index/lag(index,1))^12-1) %>%
  select(date, month1) %>% mutate(type="month3") %>%
  filter(month1 > -0.03)

nhs_index %>%
  mutate(month3 = (index/lag(index,3))^4-1,
         month6 = (index/lag(index,6))^2-1) %>%
  select(date, month3, month6) %>%
  filter(date >= "2017-01-01") %>%
  pivot_longer(month3:month6, names_to = "type", values_to = "value") %>%
  left_join(nhs_index_1_month, by=c("date","type")) %>%
  mutate(last_value = ifelse(date == max(date), value, as.numeric(NA))) %>%
  ggplot(aes(date, value, color=type, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title="I am a graphicTKKTKTKT of Non-Housing Services",
       subtitle = "Tester",
       caption = "April 2020 excluded on the 1-month. TKTKTKTAll items less food and energy, monthly percent change, BLS, Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
#  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(legend.position = c(0.90,0.15), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text(show.legend=FALSE, nudge_x = 120) +
  geom_col(aes(date, month1), alpha=0.1, size=0, show.legend = FALSE)

ggsave("graphics/pce_nhs_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")


##### SuperCore ####
#First make a graphic of non-housing services
#### Basic Index ####
supercore_index <-
  pce %>%
  select(date, LineDescription, WDataValue_P1, PCEweight) %>%
  group_by(date) %>%
  summarize(nhsWP1 = WDataValue_P1[LineDescription == "PCE excluding food and energy"] - WDataValue_P1[LineDescription == "Housing"] - WDataValue_P1[LineDescription == "Net purchases of used motor vehicles (56)"],
            nhs_weight = PCEweight[LineDescription == "PCE excluding food and energy"] - PCEweight[LineDescription == "Housing"] - PCEweight[LineDescription == "Net purchases of used motor vehicles (56)"],
  ) %>%
  ungroup() %>%
  mutate(nhsWP1A = nhsWP1/nhs_weight) %>%
  mutate(nhsWP1A = (nhsWP1A+1)^12-1) %>%
  mutate(index = nhsWP1/nhs_weight+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index))
#####

##### Graphic for Non-Housing Services ####
supercore_index_1_month <- supercore_index %>%
  mutate(month1 = (index/lag(index,1))^12-1) %>%
  select(date, month1) %>% mutate(type="month3") %>%
  filter(month1 > -0.03)

supercore_index %>%
  mutate(month3 = (index/lag(index,3))^4-1,
         month6 = (index/lag(index,6))^2-1) %>%
  select(date, month3, month6) %>%
  filter(date >= "2017-01-01") %>%
  pivot_longer(month3:month6, names_to = "type", values_to = "value") %>%
  left_join(supercore_index_1_month, by=c("date","type")) %>%
  mutate(last_value = ifelse(date == max(date), value, as.numeric(NA))) %>%
  ggplot(aes(date, value, color=type, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title="I am a graphicTKKTKTKT of Supercore",
       subtitle = "Tester",
       caption = "April 2020 excluded on the 1-month. TKTKTKTAll items less food and energy, monthly percent change, BLS, Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  #  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(legend.position = c(0.90,0.15), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("red", "purple")) +
  geom_text(show.legend=FALSE, nudge_x = 120) +
  geom_col(aes(date, month1), alpha=0.1, size=0, show.legend = FALSE)

ggsave("graphics/pce_nhs_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")


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
#### Make difference ####


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


ggsave("graphics/nhs_difference_pce_cpi.png", dpi="retina", width = 12, height=6.75, units = "in")



##### Quarterly Check #####
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

cpi_diff <- get_NIPA_data(beaKey, 'U90100', 'Q', '2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023', data_set_name = 'NIUnderlyingDetail')
cpi_diff <- BEA_date_quarterly(cpi_diff)
#cpi_diff <- BEA_date_monthly(cpi_diff)

diff_high <- c("PCE Chain-type price index (percent change)","Equals: CPI (percent change)","Rent of shelter")

diff_types <- c("Less: Weight effect (percentage points)",
          "Less: Scope effect - PCE price index items out-of-scope of the CPI (percentage points)",
          "Plus: Scope effect - CPI items out-of-scope of the PCE price index (percentage points)",
          "Less: Other effects (percentage points)"
          )

cpi_diff %>% filter(LineDescription %in% diff_types) %>%
  ggplot(aes(date,DataValue, color=LineDescription)) + geom_line(size=1.2) +
  theme_classic() + theme(legend.position = "bottom") + facet_wrap(~LineDescription, scales = "free")

cpi_diff

cpi_housing_diff <-
  cpi_diff %>% filter(LineDescription %in% diff_high) %>%
  group_by(date) %>%
  mutate(DataValue = if_else(LineDescription=="Rent of shelter",
                             DataValue[LineDescription=="PCE Chain-type price index (percent change)"]-DataValue[LineDescription=="Rent of shelter"], DataValue)) %>%
  ungroup() %>%
  ggplot(aes(date,DataValue, color=LineDescription)) + geom_line(size=1.2) +
  theme_classic() + theme(legend.position = "bottom") + facet_wrap(~LineDescription)


cpi_diff %>% group_by(date) %>%
  summarize(pce = DataValue[LineDescription == "PCE Chain-type price index (percent change)"],
            less_weight = pce + DataValue[LineDescription == "Less: Weight effect (percentage points)"],
            less_weight_scope = less_weight + DataValue[LineDescription=="Less: Scope effect - PCE price index items out-of-scope of the CPI (percentage points)"] + DataValue[LineDescription=="Plus: Scope effect - CPI items out-of-scope of the PCE price index (percentage points)"],
            cpi = DataValue[LineDescription == "Equals: CPI (percent change)"])




cpi_diff %>% filter(LineDescription == "Seasonal adjustment") %>%
  mutate(DataValue = DataValue/100) %>%
  ggplot(aes(date,DataValue)) + geom_line(size=1.2) + theme_classic() +
  theme(legend.position = "bottom", plot.title.position = "plot") +
  labs(subtitle="Differences in seasonal adjustment methodology between CPI and PCE is having larger effects. Quarterly, Annualized.",
       caption="BEA: Table 9.1U. Reconciliation of Percent Change in the CPI with Percent Change in the PCE Price Index, Mike Konczal, Roosevelt Institute",
       y="Difference between CPI and PCE",x=NULL) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y")

ggsave("graphics/diff.png", dpi="retina", width = 12, height=6.75, units = "in")




cpi_diff %>% filter(LineDescription == "Seasonal adjustment") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(addded = sum(DataValue)) %>%
  ggplot(aes(year,addded)) + geom_line()