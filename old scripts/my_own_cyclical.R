library(quantmod)
library(broom)




##### Phillips Curves #####

getSymbols("UNRATE", src="FRED")
unrate <- as_tibble(data.frame(date = index(UNRATE), unrate <- UNRATE[,1]))
colnames(unrate) <- tolower(colnames(unrate))

getSymbols("NROU", src="FRED")
nrou <- as_tibble(data.frame(date = index(NROU), NROU <- NROU[,1]))
colnames(nrou) <- tolower(colnames(nrou))

unrate <- unrate %>% left_join(nrou, by="date")
unrate$nrou <- na.locf(unrate$nrou, na.rm = FALSE)
unrate$u_gap <- unrate$unrate - unrate$nrou
unrate <- unrate %>% mutate(unrate = unrate/100, nrou = nrou/100, u_gap = u_gap/100)

lm_results <- pce %>% left_join(unrate, by="date") %>%
  filter(date >= "2000-01-01") %>%
  mutate(DataValue_P1a = (DataValue_P1+1)^12-1) %>%
  group_by(LineDescription) %>%
  # Regression: inflation = lagged inflation + unemployment gap
  # do(tidy(lm(DataValue_P1 ~ lag(DataValue_P1) + I(unrate - nrou), data = .))) %>%
  do(tidy(lm(DataValue_P1 ~ I(unrate - nrou), data = .))) %>%
  # Filter for statistically significant negative esimates on unemployment gap
  filter(term == "I(unrate - nrou)", statistic < -1.95) %>% arrange(statistic)


pce %>% filter(LineDescription %in% lm_results$LineDescription, year(date) > 2014) %>%
  group_by(date) %>%
  summarize(change = sum(WDataValue_P1),
            weight = sum(PCEweight),
            changeW = change/weight,
            changeWA = (changeW+1)^12-1) %>%
  ungroup() %>% mutate(type = "All Phillips Curve items excluding housing") %>%
  ggplot(aes(date,changeWA)) + geom_line(size=1.1) + theme_classic()





+ theme_lass +
  labs(x="", y="",
       title="Demand Sensitive Items Also Reversing with Unemployment Low",
       subtitle = "Lowest level CPI items with a statistically significant negative relationship between\ninflation and unemployment gap + previous inflation, bundled. 1-month change annualized.",
       caption = "BLS, Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = food_energy_dates) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) + theme(legend.position = c(0.35,0.9))
