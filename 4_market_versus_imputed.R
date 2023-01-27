
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


pce %>% filter()

market_services <- pce %>% filter(LineDescription %in% mb_pce_fields) %>%
  filter(date >= "2018-01-01") %>%
  select(date, item_name = LineDescription, WDataValue_P1) %>%
  pivot_wider(names_from=LineDescription, values_from = WDataValue_P1) %>%
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
  ggplot(aes(date, WDataValue_P1a, fill=item_name)) + geom_bar(stat="identity", size=0) + theme_lass + facet_wrap(~item_name) +
  scale_y_continuous(labels = percent) +
  labs(title="Slowdown is in Market-Based Prices, Not Driven by Imputed Prices",
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



##### PUTTING THIS HERE FOR NOW:

g_s_index <- c("Goods","Services")

pce %>% filter(LineDescription %in% g_s_index) %>% arrange(date) %>%
  mutate(TimePeriod = str_replace_all(TimePeriod,"Q","\nQ")) %>%
  mutate(LineDescription = str_replace_all(LineDescription,"Furnishings and durable household equipment","Furnishings/Household Equiment")) %>%
  mutate(TimePeriod_a = ifelse(month(date)==11,TimePeriod,NA)) %>%
  ggplot(aes(Quantity, DataValue, label=TimePeriod_a)) + geom_point() + theme_lass + geom_path(color="skyblue") +
  geom_label() + facet_grid(~LineDescription, scales = "free") +
  labs(title="Goods Face a Convex Supply Curve While Services Show Downward Nominal Rigidity", subtitle="Nominal growth for goods and services categories, split into their price and quantity indexes, national accounts, quarterly",
       x="Quantity (Index, 2012=100)", y="Price (Index, 2012=100)",
       caption="Table 2.4.3, 2.4.4, NIPA, BEA. Mike Konczal, Roosevelt Institute") +
  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20,angle = 90), strip.text.x = element_text(size = 25))

ggsave("graphics/g_s_Q_P.png", dpi="retina", width = 16, height=6.75, units = "in")




pce %>% filter(LineDescription %in% g_s_index) %>% arrange(date) %>%
  mutate(TimePeriod = str_replace_all(TimePeriod,"Q","\nQ")) %>%
  mutate(LineDescription = str_replace_all(LineDescription,"Furnishings and durable household equipment","Furnishings/Household Equiment")) %>%
  mutate(TimePeriod_a = ifelse(month(date)==11,TimePeriod,NA)) %>%
  ggplot(aes(Quantity, DataValue, label=TimePeriod_a)) + geom_point() + theme_lass + geom_path(color="skyblue") +
  geom_text_repel() + facet_grid(~LineDescription, scales = "free")

pce %>% filter(LineDescription == "Goods") %>% arrange(date) %>%
  mutate(TimePeriod = str_replace_all(TimePeriod,"Q","\nQ")) %>%
  mutate(LineDescription = str_replace_all(LineDescription,"Furnishings and durable household equipment","Furnishings/Household Equiment")) %>%
  mutate(TimePeriod_a = ifelse(month(date)==11,TimePeriod,NA)) %>%
  ggplot(aes(Quantity, DataValue, label=TimePeriod_a)) + geom_point() + theme_lass + geom_path(color="skyblue") +
  geom_text_repel() + facet_grid(~LineDescription, scales = "free")

pre_services <- pce %>% filter(LineDescription == "Services", date < "2020-02-01")
a <- lm(DataValue ~ Quantity, data = pre_services)
coefficients(a)[1]
summary(a)

pce %>% filter(LineDescription == "Services") %>% arrange(date) %>%
  mutate(TimePeriod = str_replace_all(TimePeriod,"Q","\nQ")) %>%
  mutate(LineDescription = str_replace_all(LineDescription,"Furnishings and durable household equipment","Furnishings/Household Equiment")) %>%
  mutate(TimePeriod_a = ifelse(month(date)==11,TimePeriod,NA)) %>%
  ggplot(aes(Quantity, DataValue, label=TimePeriod_a)) + geom_point() + theme_lass + geom_path(color="skyblue") +
  geom_text_repel() + facet_grid(~LineDescription, scales = "free") +
  geom_abline(slope = coefficients(a)[2], intercept = coefficients(a)[1], color="pink")


####### PORTFOLIO MANAGEMENT ######
core_goods_fields <- c("Goods","Gasoline and other energy goods","Food and beverages purchased for off-premises consumption","Services","Electricity and gas","Housing")

core_analysis <- pce %>% filter(LineDescription %in% core_goods_fields) %>%
  filter(date >= "2018-01-01") %>%
  select(date, LineDescription, WDataValue_P1a) %>%
  pivot_wider(names_from=LineDescription, values_from = WDataValue_P1a) %>%
  clean_names() %>%
  mutate(core_goods = goods - food_and_beverages_purchased_for_off_premises_consumption - gasoline_and_other_energy_goods) %>%
  mutate(core_services = services - electricity_and_gas - housing) %>%
  mutate(core_inflation = core_goods + core_services) %>%
  pivot_longer(-date, names_to = "item_name", values_to = "WDataValue_P1a") %>%
  filter(item_name %in% c("core_goods","core_services","housing")) %>%
  mutate(item_nameF = factor(item_name, levels = c("core_goods", "housing", "core_services")))

levels(core_analysis$item_nameF)[levels(core_analysis$item_nameF) == "core_goods"] <-"Core Goods"
levels(core_analysis$item_nameF)[levels(core_analysis$item_nameF) == "core_services"] <-"Core Services excluding Housing"
levels(core_analysis$item_nameF)[levels(core_analysis$item_nameF) == "housing"] <-"Housing"


core_analysis <- core_analysis %>% filter(item_name == "core_services") %>%
  select(date, item_name, WDataValue_P1a)

portfolio_shifted <- pce %>% filter(LineDescription == "Portfolio management and investment advice services") %>%
  select(date, item_name = LineDescription, WDataValue_P1a) %>%
  rbind(core_analysis) %>%
  pivot_wider(names_from=item_name, values_from = WDataValue_P1a) %>%
  clean_names() %>%
  mutate(core_services_ex_portfolio = core_services-portfolio_management_and_investment_advice_services) %>%
  pivot_longer(-date, names_to = "item_name", values_to = "WDataValue_P1a") %>%
  filter(item_name %in% c("core_services","core_services_ex_portfolio")) %>%
  filter(!is.na(WDataValue_P1a))
  

portfolio_shifted %>% group_by(item_name) %>%
  mutate(lagged_3 = ((1+WDataValue_P1a)*(1+lag(WDataValue_P1a,1))*(1+lag(WDataValue_P1a,2)))^(1/3)-1) %>%
  ggplot(aes(date,lagged_3, color=item_name)) + geom_line()


portfolio_shifted %>% group_by(item_name) %>%
  mutate(lagged_3 = ((1+WDataValue_P1a)*(1+lag(WDataValue_P1a,1))*(1+lag(WDataValue_P1a,2)))^(1/3)-1) %>%
  ggplot(aes(date,lagged_3)) + geom_bar(stat="identity") + facet_wrap(~item_name)