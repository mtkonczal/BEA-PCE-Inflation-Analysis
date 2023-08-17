# This creates a graphic with three images

if(!exists("title_three_dashboard")){
title_three_dashboard <- "This is a test showing how the PCE inflation breaks down default"}

if(!exists("three_twelve_graphic")){
  three_twelve_graphic <- "This compares 3 and 12 month changes going back"}

if(!exists("title_overview")){
  title_overview <- "Core graphic by itself"}

date_breaks <- sort(unique(pce$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 12)]

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

core_analysis %>% mutate(WDataValue_P1a = (WDataValue_P1+1)^12-1) %>%
  filter(WDataValue_P1a > -0.02) %>%
  ggplot(aes(date,WDataValue_P1a, fill=item_name)) + geom_bar(stat="identity", size=0) + facet_wrap(~item_nameF) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = title_three_dashboard,
       subtitle = "Monthly contribution to PCE inflation, annualized.",
       caption ="BEA, NIPA Tables 2.4.4 and 2.4.5, weights approximated as nominal consumption shares as a percent of the total. Housing is rental and imputed rental.\n April 2020 Core Services ex Housing value excluded as large negative outlier. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_fill_brewer(palette="RdPu", name = "item_name") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(plot.title = element_text(size = 22, face="bold"))

ggsave("graphics/three_categories.png", dpi="retina", width = 12, height=6.75, units = "in")


##### BASELINE GRAPHIC
##### Graphic1: Core Inflation ####

pce %>% filter(date > "2017-12-01", LineDescription == "PCE excluding food and energy") %>%
  mutate(DataValue_P1a = (1+DataValue_P1)^12-1) %>%
  filter(DataValue_P1a > -0.04) %>%
  select(date, LineDescription, DataValue_P1a) %>%
  mutate(num_label = round(100*DataValue_P1a, 1)) %>%
  mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  mutate(num_label = ifelse(date < "2022-01-01", NA, num_label)) %>%
  ggplot(aes(x = date, y = DataValue_P1a, fill = LineDescription, label = num_label)) +
  geom_bar(stat = 'identity', size=0) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = title_overview,
       subtitle = "Monthly percent increase in PCE core goods and services, annualized.",
       caption ="BEA, NIPA Tables 2.4.4 and 2.4.5, author's calculations. Author's calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(x=date, y=DataValue_P1a, label=num_label), nudge_y = 0.002, size=3, color="pink") +
  scale_x_date(date_labels = "%b %y", breaks = date_breaks)

ggsave("graphics/g1_core.png", dpi="retina", width = 12, height=6.75, units = "in")


###### BETTER THREE SIX #### THIS IS REPLACE CODE ######
core <- pce %>% filter(LineDescription == "PCE excluding food and energy") %>%
  select(date, DataValue) %>%
  mutate(ThreeMonth = (DataValue/lag(DataValue,3))^4-1) %>%
  mutate(SixMonth = (DataValue/lag(DataValue,6))^2-1) %>%
  select(-DataValue) %>%
  pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
  mutate(time_length = str_replace_all(time_length,"SixMonth", "6-Month Change")) %>%
  mutate(time_length = str_replace_all(time_length,"ThreeMonth", "3-Month Change")) %>%
  mutate(last_value = ifelse(date==max(date),change,NA))

date_start = "2017-01-01"
date_end = "2019-01-01"
date_period <- interval(date_start, date_end)
date_period = date_period %/% months(1)

pre_core <- pce %>% filter(LineDescription == "PCE excluding food and energy", date == date_start | date == date_end) %>%
  mutate(change = DataValue/lag(DataValue,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(change)
pre_core <- as.numeric(pre_core)

one_month <- pce %>% filter(LineDescription == "PCE excluding food and energy") %>% select(date, DataValue) %>%
  mutate(time_length = "3-Month Change", p1A = (DataValue/lag(DataValue,1))^12-1) %>%
  filter(p1A > -0.02)

core %>% filter(date > "2016-12-01") %>%
  left_join(one_month, by=c("date","time_length")) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title="Prices Are Moving Sideways in Recent Months",
       subtitle = paste("Core PCE inflation, monthly percentage change, annualized. Dotted line represented 2017 to 2019 value of ", round(pre_core,3)*100, "%, annualized.", sep=""),
       caption = "PCE excluding food and energy, 1-month value for April 2020 removed from graphic as negative outlier. Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  theme(legend.position = c(0.35,0.85), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text_repel(show.legend=FALSE, nudge_x = 75) +
  geom_col(aes(date, p1A), alpha=0.1, size=0, show.legend = FALSE)

ggsave("graphics/three_six_core_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")

