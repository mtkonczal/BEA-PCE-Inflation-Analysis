# This creates a graphic with three images

if(!exists("title_three_dashboard")){
title_three_dashboard <- "This is a test showing how the PCE inflation breaks down default"}

if(!exists("three_twelve_graphic")){
  three_twelve_graphic <- "This compares 3 and 12 month changes going back"}

if(!exists("title_overview")){
  title_overview <- "Core graphic by itself"}


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
  scale_x_date(date_labels = "%Y") +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(plot.title = element_text(size = 22, face="bold"))

ggsave("graphics/three_categories.png", dpi="retina", width = 12, height=6.75, units = "in")


#### LONGER HORIZON


##### 3 and 6 month core ####
#### DONE CORRECTLY LONG VERSION
pce_lag <- pce %>% filter(LineDescription == "PCE excluding food and energy") %>%
  mutate(value = DataValue) %>%
  mutate(ThreeMonth = (value/lag(value,3))^4-1) %>%
  mutate(SixMonth = (value/lag(value,6))^2-1) %>%
  mutate(YoY = (value/lag(value,12))-1)

pce_lag %>% filter(date > "2010-12-01") %>%
  ggplot(aes(date, ThreeMonth)) + geom_line(size=1.2, color="#03a0ff") +
  geom_line(aes(date,YoY), color="#FF00FF", size=0.9) +
  annotate(
    "text", label = "12-Month\nChange",
    x=as.Date("2019-03-15"), y = 0.035, size = 7, colour = "#FF00FF") +
  annotate(
    "text", label = "3-Month\nChange",
    x=as.Date("2016-03-15"), y = 0.035, size = 7, colour = "#03a0ff") +
  labs(x="", y="",
       title=three_twelve_graphic,
       subtitle = "PCE core inflation, monthly percentage change, annualized.",
       caption = "BEA, NIPA Tables 2.4.4 and 2.4.5, author's calculations. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %y")

ggsave("graphics/Three_vs_YoY_long_change.png", dpi="retina", width = 12, height=6.75, units = "in")



##### BASELINE GRAPHIC
##### Graphic1: Core Inflation ####
MI_dates <- pce %>% filter(date > "2017-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 4)]

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
  geom_text(aes(x=date, y=DataValue_P1a, label=num_label), nudge_y = 0.002, size=2.5, color="pink") +
  scale_x_date(date_labels = "%b %y", breaks = MI_dates)

ggsave("graphics/g1_core.png", dpi="retina", width = 12, height=6.75, units = "in")