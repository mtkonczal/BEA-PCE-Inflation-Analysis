

cyclical_categories <- c("Accessories and parts",
"Veterinary and other services for pets",
"Bicycles and accessories",
"Child care",
"Amusement parks, campgrounds, and related recreational services",
"Clothing and footwear services",
"Household care services",
"Social advocacy and civic and social organizations",
"Less: Personal remittances in kind to nonresidents",
"Household cleaning products",
"Nursing homes (52)",
"Package tours",
"Labor organization dues",
"Museums and libraries",
"Lotteries",
"Imputed rental of owner-occupied nonfarm housing (21)",
"Pari-mutuel net receipts",
"Miscellaneous household products",
"Group housing (23)",
"Pleasure boats, aircraft, and other recreational vehicles",
"Admissions to specified spectator amusements",
"Social assistance",
"Purchased meals and beverages (102)",
"Casino gambling",
"Religious Organizations' Services to HHs",
"Motor vehicle maintenance and repair (60)",
"Household paper products",
"Domestic services",
"Rental of tenant-occupied nonfarm housing (20)",
"Final consumption expenditures of nonprofit institutions serving households (NPISHs) (132)")

cyclical_housing_categories <- c("Imputed rental of owner-occupied nonfarm housing (21)",
                                 "Rental of tenant-occupied nonfarm housing (20)")

# Missing two
date_breaks <- date_breaks_n(pce$date)

pce %>% filter(LineDescription %in% cyclical_categories) %>% group_by(date) %>%
  summarize(demand_inflation = sum(WDataValue_P1)) %>% ungroup() %>%
  mutate(demand_inflation = (demand_inflation+1)^12-1) %>%
  filter(year(date)>=2018) %>%
  ggplot(aes(date, demand_inflation)) + geom_col(size=0) + theme_classic() +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
  labs(subtitle="cyclical inflation (Mahedy/Shapiro), attempted replication by Mike Konczal")


ggsave("graphics/cyclical_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")