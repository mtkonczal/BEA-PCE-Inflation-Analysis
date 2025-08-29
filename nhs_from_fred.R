nhs_fred <- getFRED(DataValue = "IA001260M")

core <- nhs_fred %>%
  select(date, DataValue) %>%
  mutate(ThreeMonth = (DataValue / lag(DataValue, 3))^4 - 1) %>%
  mutate(SixMonth = (DataValue / lag(DataValue, 6))^2 - 1) %>%
  select(-DataValue) %>%
  pivot_longer(
    ThreeMonth:SixMonth,
    names_to = "time_length",
    values_to = "change"
  ) %>%
  mutate(
    time_length = str_replace_all(time_length, "SixMonth", "6-Month Change")
  ) %>%
  mutate(
    time_length = str_replace_all(time_length, "ThreeMonth", "3-Month Change")
  ) %>%
  mutate(last_value = ifelse(date == max(date), change, NA))

one_month <- nhs_fred %>%
  select(date, DataValue) %>%
  mutate(
    time_length = "3-Month Change",
    p1A = (DataValue / lag(DataValue, 1))^12 - 1
  ) %>%
  filter(p1A > -0.02)

core %>%
  filter(date >= "2019-01-01") %>%
  left_join(one_month, by = c("date", "time_length")) %>%
  ggplot(aes(
    date,
    change,
    color = time_length,
    label = label_percent()(last_value)
  )) +
  geom_line(size = 1.2) +
  labs(
    x = "",
    y = "",
    title = "Non-Housing Services is Picking Up in Recent Months",
    subtitle = "Personal Consumption Expenditures: Services Excluding Energy and Housing (Chain-Type Price Index) (IA001260M).",
    caption = "BEA. FRED. Author's calculations. Mike Konczal, Roosevelt Institute."
  ) +
  theme_esp() +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
  theme(legend.position = "top", legend.text = element_text(size = 15)) +
  scale_color_manual(values = c("#2D779C", "#A4CCCC")) +
  geom_text_repel(show.legend = FALSE, nudge_x = 75) +
  geom_col(aes(date, p1A), alpha = 0.5, size = 0, show.legend = FALSE)

ggsave("graphics/nhs_fred.png", dpi="retina", width = 12, height=6.75, units = "in")
