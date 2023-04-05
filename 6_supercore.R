# This creates a graphic with three images

if(!exists("title_supercore")){
title_supercore <- "This is a test run of supercore inflation, but for PCE"}

core_supercore <- c("PCE excluding food and energy","Used autos","Housing")




core_analysis <- pce %>% filter(LineDescription %in% core_supercore) %>%
  select(date, LineDescription, WDataValue_P1) %>%
  pivot_wider(names_from=LineDescription, values_from = WDataValue_P1) %>%
  clean_names() %>%
  mutate(supercore = pce_excluding_food_and_energy - housing - used_autos) %>%
  select(date, supercore) %>%
  mutate(Three_month_change = (supercore+lag(supercore,1)+lag(supercore,2)), Three_month_change = Three_month_change/3)

core_analysis %>% mutate(WDataValue_P1a = (supercore+1)^12-1, Three_month_changea = (1+Three_month_change)^12-1) %>%
  filter(WDataValue_P1a > -0.02) %>%
  ggplot(aes(date,WDataValue_P1a)) + geom_bar(stat="identity", size=0) + theme_lass +
  #geom_line(aes(date,Three_month_changea), color="red") + 
  labs(y = NULL,
       x = NULL,
       title = title_supercore,
       subtitle = "Monthly contribution to inflation of core PCE excluding used autos and housing, annualized.",
       caption ="BEA, NIPA Tables 2.4.4 and 2.4.5, weights approximated as nominal consumption shares as a percent of the total. Housing is rental and imputed rental.\n April 2020 Core Services ex Housing value excluded as large negative outlier. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_fill_brewer(palette="RdPu") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%Y") +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(plot.title = element_text(size = 22, face="bold"))

ggsave("graphics/supercore.png", dpi="retina", width = 12, height=6.75, units = "in")
