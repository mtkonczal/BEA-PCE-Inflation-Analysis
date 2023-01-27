
##### THIS IS WHERE WE ARE
#MARKET BASED TEST

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

### DIFFERENCES CHART

trend_quantity_services <- pce %>% filter(LineNumber %in% 148:340) %>%
  select(date, SeriesCode, LineDescription, DataValue = Quantity) %>%
  draw_ll_Trendline("2014-01-01","2019-12-01")

trend_quantity_services %>% filter(date == max(date)) %>%
  select(item_name = LineDescription, value = DataValue, trendline) %>%
  mutate(diff = value/trendline -1) %>%
  arrange((diff)) %>%
  mutate(item_nameF = fct_reorder(item_name,diff, .desc = TRUE)) %>%
  slice_head(n = 20) %>%
ggplot(aes(item_nameF,diff)) + geom_bar(stat="identity", size = 0) +
  coord_flip() + theme_lass +
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=12)) +
  scale_y_continuous(labels = percent) +
  labs(title="Top 20 Services Who Haven't Returned in This Recovery",
       subtitle="Quantity Index; Value is current quantity against the category's own predicted trendline value.",
       caption="NIPA Table 2.4.3.U, log-linear trendline from 2014 to 2019, Mike Konczal, Roosevelt Institute")


ggsave("graphics/trends_diff.png", dpi="retina", width = 12, height=6.75, units = "in")

######## MOVIES ######
pce %>% filter(LineDescription == "Motion picture theaters") %>%
  rename(Price = DataValue) %>%
  mutate(TimePeriod_a = ifelse(date==max(date),TimePeriod,NA)) %>%
  
  ggplot(aes(Quantity,Price,label=TimePeriod_a)) + geom_point() + geom_path() + theme_classic() +
  labs(title= "Movies Went to Zero, Still Down 50 Percent; Prices Have Increased 13 Percent",
      subtitle = "Quantity and Price Index for 'Motion picture theaters'",
       caption = "NIPA Table 2.4.3U, 2.4.4U, Mike Konczal, Roosevelt Insitute") +
  geom_label(size=4)

ggsave("graphics/movies.png", dpi="retina", width = 12, height=6.75, units = "in")

#########


Employment agency services
pce %>% filter(LineDescription == "Employment agency services") %>%
  ggplot(aes(date,Quantity)) + geom_point() + geom_path() + theme_classic()


real_pce <- get_NIPA_data(beaKey, 'U20406', 'M', '2014,2015,2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
real_pce <- BEA_date_monthly(real_pce)

real_pce %>% filter(LineDescription %in% c("Goods","Services")) %>%
  group_by(LineDescription) %>%
  mutate(DataValue = 100*DataValue/DataValue[date == "2020-01-01"]) %>%
  draw_ll_Trendline("2014-01-01","2019-01-01") %>%
  ungroup() %>%
  ggplot(aes(date,DataValue, color=LineDescription.x)) + geom_line(size = 1.2) +
  geom_line(aes(date,trendline), linetype="dashed", size = 1) + theme_lass +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette="RdPu") +
  scale_x_date(date_labels = "%Y") +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(plot.title = element_text(size = 22, face="bold")) +
  labs(title="Large Relative Shift From Services to Goods During Recovery, Slowly Reversing",
       subtitle="100 equals value in January 2020",
       caption="BEA, NIPA Table 2.4.6, Log-linear trendline 2014-2019, author's calculations, Mike Konczal, Roosevelt Institute")

ggsave("graphics/trends.png", dpi="retina", width = 12, height=6.75, units = "in")