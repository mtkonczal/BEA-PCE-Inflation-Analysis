# This creates a graphic with three images


date_breaks <- sort(unique(pce$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 6)]


nipa %>% filter(TableId == "T20806") %>%
  filter(SeriesLabel == "Personal consumption expenditures") %>%
  mutate(Value = Value/1000) %>%
  mutate(projection = logLinearProjection(., date, Value, "2021-12-01", "2024-12-01")) %>%
  filter(date >= "2023-06-01") %>%
  ggplot(aes(date, Value)) +
  geom_line(size = 1.2, color = esp_navy) +
  geom_line(aes(date, projection), size = 1.2, color = esp_navy, linetype = "dotted") +
  labs(x="", y="",
       title="Consumers Are Not Spending More Money in 2025",
       subtitle = "Real Personal Consumption Expenditures, Trillions of Chained (2017) dollars. Dotted Line Log-Linear Projection 2022-2024",
       caption = "Table 2.8.6. Mike Konczal.") +
  theme_esp() +
  scale_x_date(date_labels = "%B\n%Y", breaks=date_breaks) +
  scale_y_continuous(labels = dollar_format()) +
  geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dotted")

ggsave("graphics/pce_overall_spending.png", dpi="retina", width = 12, height=6.75, units = "in")


pce_categories <- c("PCE services excluding energy and housing", "Housing", "Durable goods", "Nondurable goods")

#pce_categories <- c("Food services and accommodations", "Net foreign travel and expenditures abroad by U.S. residents")


nipa %>% filter(TableId == "T20806") %>%
  filter(SeriesLabel %in% pce_categories) %>%
  mutate(Value = Value/1000) %>%
  mutate(projection = logLinearProjection(., date, Value, "2021-12-01", "2024-12-01", group = SeriesLabel)) %>%
  filter(date >= "2021-06-01") %>%
  ggplot(aes(date, Value)) +
  geom_line(size = 1.2, color = esp_navy) +
  geom_line(aes(date, projection), size = 1.2, color = esp_navy, linetype = "dotted") +
  facet_wrap(~SeriesLabel, scales="free") +
  labs(x="", y="",
       title="Consumers Are Not Spending More Money on Services in 2025",
       subtitle = "Real Personal Consumption Expenditures, Trillions of Chained (2017) dollars. Dotted Line Log-Linear Projection 2022-2024",
       caption = "Table 2.8.6. Mike Konczal.") +
  theme_esp() +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  scale_y_continuous(labels = dollar_format()) +
  geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dotted")

ggsave("graphics/pce_broken_down.png", dpi="retina", width = 12, height=6.75, units = "in")