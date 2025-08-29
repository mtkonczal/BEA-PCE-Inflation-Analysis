# This creates a graphic with three images


date_breaks <- sort(unique(pce$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 12)]


mbs <- pce %>% filter(LineDescription == "Market-based PCE services")

mbs_index_1_month <- mbs %>%
  mutate(month1 = (Value/lag(Value,1))^12-1) %>%
  select(date, month1) %>% mutate(type="month3") %>%
  filter(month1 > -0.03)

mbs %>%
  mutate(month3 = (Value/lag(Value,3))^4-1,
         month6 = (Value/lag(Value,6))^2-1) %>%
  select(date, month3, month6) %>%
  filter(date >= "2019-01-01") %>%
  pivot_longer(month3:month6, names_to = "type", values_to = "value") %>%
  left_join(mbs_index_1_month, by=c("date","type")) %>%
  mutate(last_value = ifelse(date == max(date), value, as.numeric(NA))) %>%
  mutate(type = str_replace_all(type,"month3","3-month average")) %>%
  mutate(type = str_replace_all(type,"month6","6-month average")) %>%
  ggplot(aes(date, value, color=type, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title="Market-Based PCE Services Inflation Not Down Though",
       subtitle = "Market-based PCE services, From BEA.",
       caption = "NIPA Tables 2.4.4U and 2.4.5U. Author's calculations. Mike Konczal.") +
  theme_esp() +
#  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
  theme(legend.position = "top", legend.text = element_text(size=15)) +
  scale_color_manual(values=c("navy", "red")) +
  geom_text(show.legend=FALSE, nudge_x = 80) +
  geom_col(aes(date, month1), alpha=0.35, color="#2c3254", size=0, show.legend = FALSE)

ggsave("graphics/mbs_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")



