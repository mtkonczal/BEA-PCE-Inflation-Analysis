# This creates a graphic with three images

title_three_dashboard <- "I Love it When a Plan Comes Together: Goods Zero, Non-Housing Services Slowing"
three_twelve_graphic <- "Inflation Over the Past Three Months Have Finally Dropped"
title_overview <- "PCE Inflation Fell Last Month"

date_breaks <- date_breaks_n(pce$date, 6)


##### BASELINE GRAPHIC
##### Graphic1: Core Inflation ####
pce %>%
  filter(date >= "2022-01-01", LineDescription == "PCE excluding food and energy") %>%
  mutate(DataValue_P1a = (1 + DataValue_P1)^12 - 1) %>%
  filter(DataValue_P1a > -0.04) %>%
  select(date, LineDescription, DataValue_P1a) %>%
  mutate(num_label = round(100 * DataValue_P1a, 1)) %>%
  mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  mutate(num_label = ifelse(date >= "2025-01-01", num_label, NA)) %>%
  ggplot(aes(x = date, y = DataValue_P1a, label = num_label)) +
  geom_bar(stat = "identity", size = 0, fill=esp_navy) +
  labs(
    y = NULL,
    x = NULL,
    title = title_overview,
    subtitle = "Monthly percent increase in PCE core goods and services, annualized.",
    caption = "BEA, NIPA Tables 2.4.4 and 2.4.5, author's calculations. Author's calculation. Mike Konczal, Roosevelt Institute"
  ) +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.major.y = element_line(size = 0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(x = date, y = DataValue_P1a, label = num_label), nudge_y = 0.002, size = 3, color = esp_navy) +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
  theme_esp()

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

one_month <- pce %>% filter(LineDescription == "PCE excluding food and energy") %>% select(date, DataValue) %>%
  mutate(time_length = "3-Month Change", p1A = (DataValue/lag(DataValue,1))^12-1) %>%
  filter(p1A > -0.02)

core %>% filter(date >= "2019-01-01") %>%
  left_join(one_month, by=c("date","time_length")) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title="Core PCE Inflation Now at 2 Percent?!",
       subtitle = "Core PCE inflation, monthly percentage change, annualized.",
       caption = "PCE excluding food and energy, 1-month value for April 2020 removed from graphic as negative outlier. Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_esp() +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  theme(legend.position = "top", legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text_repel(show.legend=FALSE, nudge_x = 75) +
  geom_col(aes(date, p1A), alpha=0.5, size=0, show.legend = FALSE)

ggsave("graphics/three_six_core_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")


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
       caption ="BEA, NIPA Tables 2.4.4 and 2.4.5, weights approximated as nominal consumption shares as a percent of the total. Housing is rental and imputed rental.\n April 2020 Core Services ex Housing value excluded as large negative outlier. Author's Calculation. Mike Konczal") +
  scale_fill_brewer(palette="RdPu", name = "item_name") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(plot.title = element_text(size = 22, face="bold"))

ggsave("graphics/three_categories.png", dpi="retina", width = 12, height=6.75, units = "in")

