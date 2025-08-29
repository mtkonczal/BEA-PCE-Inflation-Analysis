# This creates a graphic with three images


if(!exists("title_three_dashboard")){
title_three_dashboard <- "This is a test showing how the PCE inflation breaks down default"}

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
  mutate(index = nhsWP1/nhs_weight+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index))
#####



##### Graphic for Non-Housing Services ####
date_breaks <- sort(unique(pce$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 6)]

nhs_index_1_month <- nhs_index %>%
  mutate(month1 = (index/lag(index,1))^12-1) %>%
  select(date, month1) %>% mutate(type="month3") %>%
  filter(month1 > -0.03)

avg_2024 <- nhs_index %>%
  reframe(avg_2024 = index[date == "2024-12-01"]/index[date == "2023-12-01"]-1)

nhs_index %>%
  mutate(month3 = (index/lag(index,3))^4-1,
         month6 = (index/lag(index,6))^2-1) %>%
  select(date, month3, month6) %>%
  filter(date >= "2022-01-01") %>%
  pivot_longer(month3:month6, names_to = "type", values_to = "value") %>%
  left_join(nhs_index_1_month, by=c("date","type")) %>%
  mutate(last_value = ifelse(date == max(date), value, as.numeric(NA))) %>%
  mutate(type = str_replace_all(type,"month3","3-month average")) %>%
  mutate(type = str_replace_all(type,"month6","6-month average")) %>%
  ggplot(aes(date, value, color=type, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title="PCE Non-Housing Core Services Jumped Up This Month",
       subtitle = "PCE Services Excluding Energy and Housing (Chain-Type Price Index), Manually Calculated.",
       caption = "NIPA Tables 2.4.4U and 2.4.5U, weights approximated as nominal consumption shares as a percent of the total. Author's calculations. Mike Konczal.") +
  theme_esp() +
#  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
  theme(legend.position = "top", legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#ff8361", "#70ad8f")) +
  geom_text(show.legend=FALSE, nudge_x = 80) +
  geom_col(aes(date, month1), alpha=0.3, color="#2c3254", size=0, show.legend = FALSE)

ggsave("graphics/pce_nhs_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")



