
##### THIS IS WHERE WE ARE
#MARKET BASED TEST
mb_pce_fields <- c("Market-based PCE services","Market-based PCE housing services")

#### MARKET BASED GRAPHIC
pce %>% filter(LineDescription %in% mb_pce_fields) %>%
  filter(date >= "2019-01-01") %>%
  select(date, LineDescription, WDataValue_P1) %>%
  pivot_wider(names_from=LineDescription, values_from = WDataValue_P1) %>%
  clean_names() %>%
  mutate(market_based_services_minus_shelter = market_based_pce_services - market_based_pce_housing_services) %>%
  pivot_longer(-date, names_to = "item_name", values_to = "WDataValue_P1") %>%
  mutate(WDataValue_P1a = (WDataValue_P1+1)^12-1) %>%
  
  ggplot(aes(date, WDataValue_P1a)) + geom_bar(stat="identity") + facet_wrap(~item_name) + theme_classic()

##### BIG ASIDE - OTHER THAN MARKET BASED ####

## Other than market based basket:
otm_basket_total <- c('DMARRG',
'DUTMRG',
'DREERG',
'DFFDRG',
'DMICRG',
'DNFRRG',
'DFARRG',
'DGAMRG',
'DFOORG',
'DIMPRG',
'DMUTRG',
'DPMIRG',
'DTRURG',
'DLIFRG',
'DMINRG',
'DIINRG',
'DPWCRG',
'DTINRG',
'DUNSRG',
'DSCWRG',
'DSADRG',
'DRELRG',
'DGIVRG',
'DDMIRG',
'DFORRG',
'DNPIRG')

otm_basket_services <- c('DFARRG',
                         'DGAMRG',
                         'DFOORG',
                         'DIMPRG',
                         'DMUTRG',
                         'DPMIRG',
                         'DTRURG',
                         'DLIFRG',
                         'DMINRG',
                         'DIINRG',
                         'DPWCRG',
                         'DTINRG',
                         'DUNSRG',
                         'DSCWRG',
                         'DSADRG',
                         'DRELRG',
                         'DGIVRG',
                         'DDMIRG',
                         'DFORRG')



market_services <- pce %>% filter(LineDescription %in% mb_pce_fields) %>%
  filter(date >= "2018-01-01") %>%
  select(date, item_name = LineDescription, WDataValue_P1) %>%
  pivot_wider(names_from=item_name, values_from = WDataValue_P1) %>%
  clean_names() %>%
  mutate(market_based_services_minus_shelter = market_based_pce_services - market_based_pce_housing_services) %>%
  pivot_longer(-date, names_to = "item_name", values_to = "WDataValue_P1") %>%
  filter(item_name == "market_based_services_minus_shelter") %>%
  mutate(WDataValue_P1a = (WDataValue_P1+1)^12-1)


market_services <- pce %>% filter(LineDescription == "Market-based PCE services") %>%
  filter(date >= "2018-01-01") %>%
  select(date, item_name = LineDescription, WDataValue_P1) %>%
  mutate(WDataValue_P1a = (WDataValue_P1+1)^12-1)

pce %>% filter(SeriesCode %in% otm_basket_services) %>%
  filter(date >= "2018-01-01") %>%
  group_by(date) %>%
  summarize(WDataValue_P1 = sum(WDataValue_P1)) %>%
  ungroup() %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^12-1) %>%
  mutate(item_name = "Other-Than-Market Price Changes in Services PCE") %>%
  select(date, item_name, 2:3) %>%
  rbind(market_services) %>%
  ggplot(aes(date, WDataValue_P1a, fill=item_name)) + geom_bar(stat="identity", size=0) + facet_wrap(~item_name) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
  labs(title="Changes in Market-Based Services Inflation Versus Imputed",
       subtitle = "Contribution to PCE inflation, annualized",
       caption = "NIPA Tables 2.4.4U and 2.4.5U, PCE weights are nominal consumption shares as a percent of total spending,\nOther-than-market components calculated from BEA's 'What is the 'market-based' PCE price index?' list, Mike Konczal, Roosevelt Institute", x="") +
  scale_fill_brewer(palette="Paired", name = "item_name") + theme(legend.position = "none")

ggsave("graphics/imputed.png", dpi="retina", width = 12, height=6.75, units = "in")


pce %>% filter(SeriesCode %in% otm_basket_services) %>%
  filter(date > "2018-01-01") %>%
  ggplot(aes(date, WDataValue_P1a)) + geom_bar(stat="identity") + theme_classic() + facet_wrap(~LineDescription)


pce %>% filter(LineNumber %in% c(375:400)) %>%
  filter(date > "2018-01-01") %>%
  ggplot(aes(date, WDataValue_P1a)) + geom_bar(stat="identity") + theme_classic() + facet_wrap(~LineDescription)

########

pce %>% filter(LineNumber %in% c(375:400)) %>%
  mutate(year = year(date)) %>%
  group_by(year,LineDescription) %>%
  summarize(avg = mean(WDataValue_P1a)) %>%
  ungroup() %>%
  filter(year %in% c(2019,2022)) %>%
  group_by(LineDescription) %>%
  arrange(year) %>%
  mutate(diff = avg-lag(avg)) %>%
  ungroup() %>%
  filter(year == 2022) %>%
  mutate(diffP = diff*100) %>%
  filter(diffP < 2) %>%
  mutate(ordered = fct_reorder(LineDescription,diffP)) %>%
  ggplot(aes(ordered, diffP)) + geom_bar(stat="identity") + theme_classic() + coord_flip()


a <- 
  pce %>% filter(LineNumber %in% c(375:400)) %>%
  mutate(year = year(date)) %>%
  group_by(year,LineDescription) %>%
  summarize(avg = mean(WDataValue_P1a)) %>%
  ungroup() %>%
  filter(year %in% c(2019,2022)) %>%
  group_by(LineDescription) %>%
  arrange(year) %>%
  mutate(diff = avg-lag(avg)) %>%
  filter(year == 2022) %>%
  mutate(diffP = diff*100) %>%
  mutate(ordered = fct_reorder(LineDescription,diffP))

