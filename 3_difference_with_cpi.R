load("data/cpi_data.RData")
#### FIX ME!

cpi_comparison <- 
cpi_data %>% filter(seasonal == "S",
                    item_name %in%
                      c("Services less energy services",
                        "All items less food and energy")
                      ) %>%
  group_by(item_name) %>%
  mutate(change1 = value/lag(value,3),
         cpi_change1A = change1^4-1) %>%
  ungroup() %>%
  rename(LineDescription = item_name) %>%
  mutate(LineDescription = str_replace_all(LineDescription, "Services less energy services","Core Services"),
         LineDescription = str_replace_all(LineDescription, "All items less food and energy", "Core Inflation")) %>%
  select(date, LineDescription, cpi_change1A) %>%
  mutate(type = "CPI")

pce %>% filter(LineDescription %in% c("PCE services excluding energy","PCE excluding food and energy")) %>%
  select(date, LineDescription, DataValue) %>%
  group_by(LineDescription) %>%
  mutate(cpi_change1A = DataValue/lag(DataValue,3),
         cpi_change1A = cpi_change1A^4-1) %>%
  ungroup() %>%
  select(-DataValue) %>%
  mutate(type = "PCE") %>%
  mutate(LineDescription = str_replace_all(LineDescription, "PCE services excluding energy","Core Services"),
         LineDescription = str_replace_all(LineDescription, "PCE excluding food and energy", "Core Inflation")) %>%
  rbind(cpi_comparison) %>%
  filter(year(date) > 2017) %>%
  ggplot(aes(date,cpi_change1A, color=type)) + geom_line() + theme_lass + facet_wrap(~LineDescription) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(legend.position = c(0.15,0.85)) +
  theme(subtitle="Three-month change, annualized.",
        caption="Mike Konczal, Roosevelt Institute")















########
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
date_breaks <- sort(unique(pce$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 12)]

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
  mutate(type = str_replace_all(type,"month3","3-month average")) %>%
  mutate(type = str_replace_all(type,"month6","6-month average")) %>%
  ggplot(aes(date, value, color=type, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title="PCE Non-Housing Services Has a Lower Month",
       subtitle = "",
       caption = "April 2020 excluded on the 1-month. NIPA Tables 2.4.4U and 2.4.5U, weights approximated as nominal consumption shares as a percent of the total. Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
#  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
  theme(legend.position = c(0.25,0.85), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text(show.legend=FALSE, nudge_x = 80) +
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
  mutate(type = str_replace_all(type,"month3","3-month average")) %>%
  mutate(type = str_replace_all(type,"month6","6-month average")) %>%
  ggplot(aes(date, value, color=type, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title="Supercore: Core Inflation Excluding Used Cars and Housing",
       subtitle = "",
       caption = "April 2020 excluded on the 1-month. NIPA Tables 2.4.4U and 2.4.5U./nweights approximated as nominal consumption shares as a percent of the total. Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  #  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(legend.position = c(0.15,0.85), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#FFD3B5", "#F67280")) +
  geom_text(show.legend=FALSE, nudge_x = 90) +
  geom_col(aes(date, month1), alpha=0.1, size=0, show.legend = FALSE, fill="#FFD3B5")

ggsave("graphics/supercore_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")

