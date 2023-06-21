
##### PUTTING THIS HERE FOR NOW:

g_s_index <- c("Goods","Services")

pce %>% filter(LineDescription %in% g_s_index) %>% arrange(date) %>%
  mutate(TimePeriod = str_replace_all(TimePeriod,"M","\nM")) %>%
  mutate(LineDescription = str_replace_all(LineDescription,"Furnishings and durable household equipment","Furnishings/Household Equiment")) %>%
  mutate(TimePeriod_a = ifelse(month(date)==month(max(date)),TimePeriod,NA)) %>%
  ggplot(aes(Quantity, DataValue, label=TimePeriod_a)) + geom_point() + theme_lass + geom_path(color="skyblue") +
  geom_text_repel(color="skyblue", size=2.5,  box.padding = 1, min.segment.length = Inf) +
  facet_wrap(~LineDescription, scales = "free") +
  labs(title="Goods Face a Convex Supply Curve While Services Show Downward Nominal Rigidity", subtitle="Nominal growth for goods and services categories, split into their price and quantity indexes, national accounts, quarterly",
       x="Quantity (Index, 2012=100)", y="Price (Index, 2012=100)",
       caption="Table 2.4.3, 2.4.4, NIPA, BEA. Mike Konczal, Roosevelt Institute") +
  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20,angle = 90), strip.text.x = element_text(size = 25))

ggsave("graphics/g_s_Q_P.png", dpi="retina", width = 16, height=6.75, units = "in")


pce %>% filter(LineDescription == "PCE goods excluding food and energy") %>% arrange(date) %>%
  mutate(TimePeriod = str_replace_all(TimePeriod,"M","\nM")) %>%
  mutate(TimePeriod_a = ifelse(month(date) == month(max(date)) | month(date) == month(max(date) %m-% months(6)), TimePeriod, NA)) %>%
  mutate(TimePeriod_d = ifelse(!is.na(TimePeriod_a), as.character(date, "%b\n%Y"), NA)) %>%
  ggplot(aes(Quantity, DataValue, label=TimePeriod_d))+ geom_point() + theme_lass + geom_path(color="skyblue") +
  geom_text_repel(color="skyblue", size=5, segment.linetype = 6, box.padding = 1, nudge_y = 0.3,
                  segment.size  = 0.2,
                  segment.color = "grey50") +
  facet_wrap(~LineDescription, scales = "free") +
  labs(title="Goods Face a Convex Supply Curve", subtitle="Nominal growth for core goods, split into its price and quantity indexes, national accounts, quarterly",
       x="Quantity (Index, 2012=100)", y="Price (Index, 2012=100)",
       caption="Table 2.4.3, 2.4.4, NIPA, BEA. Inspired by Adam Shapiro's work on inflation. Mike Konczal, Roosevelt Institute") +
  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20,angle = 90), strip.text.x = element_text(size = 25))

ggsave("graphics/goods_supply_demand.png", dpi="retina", width = 12, height=12, units = "in")


##### Spending Graphic ####

g_s <- c("DGDSRX", "DSERRX")
BEA_T20806 <- get_NIPA_data(beaKey, 'T20806', 'M', 'All')
BEA_T20806 <- BEA_date_monthly(BEA_T20806)


##### NORMALIZED ####
normalized <- BEA_T20806 %>% filter(SeriesCode %in% g_s) %>%
  filter(date >= "2013-12-01") %>%
  group_by(SeriesCode) %>%
  mutate(DataValue = DataValue/DataValue[date=="2020-01-01"]) %>%
  draw_ll_Trendline("12/1/2014", "12/1/2019",12) %>%
  ungroup() %>% mutate(category = "Normalized (01/2020 = 100)")


graphic_shift <- normalized %>%  
  ggplot(aes(date,DataValue,color=LineDescription)) + theme_classic() + geom_line(size=1.2) +
  geom_line(aes(date,trendline), size=0.6, linetype="longdash") +
  labs(title=NULL,
       x=NULL,
       y="Jan 2020 = 1",
       subtitle=NULL,
       caption=NULL) +
  #       subtitle="Real Personal Consumption Expenditures by Major Type of Product, Monthly, Chained Dollars, Jan 2020 = 1",
  #       caption="Table 2.8.6, log-linear trendline from 2015-2019, author's calculation, Mike Konczal, Roosevelt Institute")
  scale_fill_brewer(palette="Paired") + theme(legend.position = c(0.3,0.8), legend.title = element_blank())

##### PCE Overall ####

BEA_T20805 <- get_NIPA_data(beaKey, 'T20805', 'M', 'All')
BEA_T20805 <- BEA_date_monthly(BEA_T20805)

pce_monthly <- BEA_T20805 %>% filter(SeriesCode == "DPCERC")

pce_monthly %>%
  mutate(p6 = (DataValue/lag(DataValue,6))^2-1,
         p12 = (DataValue/lag(DataValue,12))-1) %>%
  select(date, p6, p12) %>%
  pivot_longer(p6:p12, names_to = "type", values_to = "values") %>%
  ggplot(aes(date,values, color=type)) + geom_line() + theme_classic()


pce_monthly %>%
  mutate(p6 = (DataValue/lag(DataValue,6))^2-1) %>%
  ggplot(aes(date, p6)) + geom_line() + theme_classic()

pce_YoY <- pce_monthly %>%
  mutate(pce_YoY = (DataValue/lag(DataValue,12))-1) %>%
  select(date, pce_YoY)

pce %>% 
  filter(LineDescription == "PCE excluding food and energy") %>%
  mutate(inflation_YoY = (DataValue/lag(DataValue,12))-1) %>%
  select(date, inflation_YoY) %>%
  left_join(pce_YoY, by="date")