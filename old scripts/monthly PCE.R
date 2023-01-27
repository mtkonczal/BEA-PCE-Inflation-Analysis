library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

setwd("/Users/mkonczal/Documents/R Code/BEA Stiglitz/")
beaKey <- "C0C06CC5-8A78-4345-90E7-77A37296C0CC"
source("/Users/mkonczal/Documents/R Code/BEA Stiglitz/0_helper_functions.R")
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

#Graphic 1: Core Inflation
BEA_PCE <- get_NIPA_data(beaKey, 'T20804', 'M', '2019,2020,2021,2022')
BEA_PCE <- BEA_date_monthly(BEA_PCE)
# Get the number of months in 2022
months2022 <- interval(ymd("2021-12-01"), max(BEA_PCE$date))
months2022 = months2022 %/% months(1)

start_value <- BEA_PCE %>% filter(LineDescription == "PCE excluding food and energy", date == "2021-12-01") %>%
  select(DataValue)
start_value <- as.numeric(start_value)
end_value <- BEA_PCE %>% filter(LineDescription == "PCE excluding food and energy", date == max(date)) %>%
  select(DataValue)
end_value <- as.numeric(end_value)

Pchange_2022 = end_value/start_value - 1
Wchange_2022a = (1+Pchange_2022)^(12/months2022)-1

BEA_PCE <- BEA_PCE %>% filter(LineDescription == "PCE excluding food and energy") %>%
  mutate(PCE_change = (DataValue - lag(DataValue,1))/lag(DataValue,1)) %>%
  mutate(PCE_changeA = (1+PCE_change)^12-1) %>%
  mutate(trend = ifelse(date >= "2021-12-01", Wchange_2022a, NA))

BEA_PCE %>% ggplot(aes(x = date, y = PCE_changeA)) +
  geom_bar(stat = 'identity', fill='light blue') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "After a Big June Print, Welcome Drop in Core PCE Inflation For July",
       subtitle = paste("Core PCE Inflation, Annualized. Dotted line reflects an average of ", round(Env_trend,3)*100, "% in 2022.", sep= ""),
       caption ="BEA, NIPA Table 2.8.4, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_line(aes(x=date, y=trend), size=1.1, linetype=2) +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month") +
  theme(plot.title = element_text(size = 22, face="bold"), plot.subtitle = element_text(size=15, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=15, margin=margin(19,0,11,0), lineheight=1.05),
        axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=9))

ggsave("core_PCE.png", dpi="retina", width = 12, height=6.75, units = "in")


# Graphic 2: CPI/PC Core Difference
source(file = "/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/1_load_cpi_data.R")
#######
#SET UP DATA:
cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(three_months = (lag(value, 1)/lag(value, 4)-1)) %>%
  mutate(Wthree_months = (three_months*weight)/100) %>%
  mutate(Wthree_monthsA = (1 + Wthree_months)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()

cpi_merge <- cpi %>% filter(item_name == "All items less food and energy") %>%
  select(cpi_1mA = Wchange1a, date)

PCE_CPI <- BEA_PCE %>% left_join(cpi_merge, by="date") %>%
  mutate(diff = cpi_1mA - PCE_changeA)

PCE_CPI %>% ggplot(aes(date, diff)) + geom_bar(stat = 'identity', fill='light blue') + theme_classic() +
  labs(y = NULL,
       x = NULL,
       title = "CPI Core Inflation Minus PCE Core Inflation, Monthly, Annualized",
       subtitle = "A negative value means that PCE core was higher than CPI core",
       caption ="CPI, BEA, NIPA Table 2.8.4, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute")


########### THE BIG ONE

PCE_Weight <- get_NIPA_data(beaKey, 'U20405', 'M', '2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Weight <- BEA_date_monthly(PCE_Weight)

GDP_Weight <- PCE_Weight %>% filter(SeriesCode == "DPCERC") %>%
  select(date, TotalGDP = DataValue)

PCE_Weight <- PCE_Weight %>%
  left_join(GDP_Weight, by="date") %>%
  mutate(PCEweight = DataValue/TotalGDP) %>%
  select(date, LineDescription, PCEweight)

PCE_Items <- get_NIPA_data(beaKey, 'U20404', 'M', '2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Items <- BEA_date_monthly(PCE_Items)

PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'M', '2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- BEA_date_monthly(PCE_Q) %>% select(LineDescription, date, Quantity = DataValue)

PCE_Items <- PCE_Items %>%
  left_join(PCE_Weight, by=c('date' = 'date','LineDescription' = 'LineDescription')) %>%
  left_join(PCE_Q, by=c('date' = 'date','LineDescription' = 'LineDescription'))

PCE_Items <- PCE_Items %>%
  group_by(SeriesCode) %>%
  mutate(DataValue_P1 = (DataValue - lag(DataValue,1))/lag(DataValue,1)) %>%
  mutate(WDataValue_P1 = DataValue_P1*PCEweight) %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^12-1) %>%
  mutate(Quantity_P1 = (Quantity - lag(Quantity,1))/lag(Quantity,1)) %>%
  ungroup()


PCE_Items %>% filter(year(date) == 2019 | year(date) == 2021) %>%
  mutate(year = as.character(year(date))) %>%
  ggplot(aes(Quantity_P1, DataValue_P1, color=year)) + geom_point() + theme_classic()



PCE_Items %>% filter(LineDescription %in% c("Services", "Goods")) %>%
  ggplot(aes(x = date, y = WDataValue_P1a, fill = LineDescription)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "PCE Inflation, Consistent Since October 2021, Slows Last Month",
       subtitle = "hello",
       caption ="BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month")

ggsave("test_2.png", dpi="retina", width = 12, height=6.75, units = "in")

PCE_Quantities <- get_NIPA_data(beaKey, 'U20403', 'M', '2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Quantities <- BEA_date_monthly(PCE_Quantities)

PCE_Quantities %>% filter(LineDescription %in% c("Food purchased for off-premises consumption","Food services and accommodations")) %>%
  group_by(LineDescription) %>% #mutate(DataValue = 100*DataValue/DataValue[date=="2018-01-01"]) %>%
  ggplot(aes(date,DataValue, color=LineDescription)) + geom_line() + theme_classic()

PCE_Items %>% filter(LineDescription %in% c("Food purchased for off-premises consumption","Food services and accommodations")) %>%
  group_by(LineDescription) %>% mutate(valueNormal = 100*DataValue/DataValue[date=="2019-01-01"]) %>%
  ggplot(aes(date,valueNormal, color=LineDescription)) + geom_line() + theme_classic()

# PCE goods excluding food and energy

PCE_Quantities %>% filter(LineDescription %in% c("Food purchased for off-premises consumption","Food services and accommodations")) %>%
  draw_Trendline("12/1/2017", "12/1/2019", 1) %>%
  ggplot(aes(date,DataValue, color=LineDescription)) + geom_line() + theme_classic() +
  geom_line(aes(date,trendline, color=LineDescription), linetype="dotted")


### TRY QUARTERLY
PCE_Items <- get_NIPA_data(beaKey, 'U20404', 'A', '2019,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'A', '2019,2021,2022', data_set_name = 'NIUnderlyingDetail') %>%
  select(LineDescription, TimePeriod, Quantity = DataValue)

PCE_Items <- PCE_Items %>%
  left_join(PCE_Q, by=c('TimePeriod' = 'TimePeriod','LineDescription' = 'LineDescription'))


food_chart <- PCE_Items %>% filter(LineDescription %in% c("Food purchased for off-premises consumption","Food services and accommodations")) %>%
  rename(Price = DataValue) %>% select(LineDescription, TimePeriod, Price, Quantity) %>%
  group_by(LineDescription) %>% arrange(TimePeriod) %>%
  mutate(Price2019 = lag(Price,1), Quantity2019 = lag(Quantity,1)) %>%
  ungroup()

food_chart %>% filter(TimePeriod == 2021) %>% select(Price2021 = Price, Quantity2021 = Quantity, LineDescription, TimePeriod) %>%
  right_join(food_chart, by=c("LineDescription","TimePeriod")) %>%
  ggplot(aes(Quantity, Price, color=LineDescription)) + geom_point(size=5) + geom_line(size=1.2) +
  geom_point(aes(Quantity2021,Price2021), size=10) + theme_classic() +
  theme(legend.position='bottom') +
  labs(title="Food at Home Moved Up a Demand Curve, Food Services a Supply Curve",
       subtitle= "The larger circle is the value for 2021, the smaller circle is the value for 2019. Index numbers, 2012=100",
       caption="Inspired by Adam Shapiro's SF Fed Inflation work. NIPA Table 2.4.3 and 2.4.4, Author's Calculations. Mike Konczal, Roosevelt Institute",
       x="Quantity (Index)", y="Price (Index)") +
  theme(plot.title = element_text(size = 25),
        plot.caption = element_text(size=12),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=18, margin=ggplot2::margin(9,0,15,0),lineheight=1.05)) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=15, color="#222222"), panel.background = element_blank())  +
  scale_colour_brewer(palette="Set1")


ggsave("food_inflation.png", dpi="retina", width = 12, height=6, units = "in")


#####

PCE_Quantities <- get_NIPA_data(beaKey, 'U20403', 'M', '2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Quantities <- BEA_date_monthly(PCE_Quantities)

PCE_Quantities %>% filter(LineDescription %in% c("Food purchased for off-premises consumption","Food services and accommodations")) %>%
  draw_Trendline("12/1/2017", "12/1/2019", 1) %>%
  ggplot(aes(date,DataValue, color=LineDescription)) + geom_line() + theme_classic() +
  geom_line(aes(date,trendline, color=LineDescription), linetype="dotted") +
  theme(legend.position='bottom') +
  labs(title="Where We Eat Food is Starting to Return to Trend",
       subtitle= "Quantity Indexes, Real Personal Consumption Expenditures by Type of Product, 2012=100",
       caption="NIPA Table 2.4.3, Trend Line is 12/2017 to 12/2019, Author's Calculations. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(plot.title = element_text(size = 25),
        plot.caption = element_text(size=12),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=18, margin=ggplot2::margin(9,0,15,0),lineheight=1.05)) +
  theme(axis.text = element_text(size=15), axis.title = element_text(size=0),
        legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=15, color="#222222"), panel.background = element_blank()) +
  scale_colour_brewer(palette="Set1")

ggsave("food_amount.png", dpi="retina", width = 12, height=6, units = "in")



### TRY QUARTERLY
PCE_Items <- get_NIPA_data(beaKey, 'U20404', 'M', '2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'M', '2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail') %>%
  select(LineDescription, TimePeriod, Quantity = DataValue)

PCE_Items <- PCE_Items %>%
  left_join(PCE_Q, by=c('TimePeriod' = 'TimePeriod','LineDescription' = 'LineDescription')) %>%
  rename(Price = DataValue)
PCE_Items <- BEA_date_monthly(PCE_Items) %>% mutate(year = year(date), year = as.factor(year))

PCE_Items %>% filter(LineDescription %in% c("New motor vehicles (55)","Net purchases of used motor vehicles (56)")) %>% arrange(date) %>%
  ggplot(aes(Quantity, Price, color=year, label=TimePeriod)) + geom_point() + theme_classic() + geom_line() +
  geom_label() + facet_wrap(~LineDescription, scales = "free")

PCE_Items %>% filter(LineDescription %in% c("New motor vehicles (55)","Net purchases of used motor vehicles (56)")) %>% arrange(date) %>%
  group_by(LineDescription) %>%
  mutate(LagQ = (Quantity - lag(Quantity,1))/lag(Quantity,1)) %>%
  mutate(LagP = (Price - lag(Price,1))/lag(Price,1)) %>%
  ungroup() %>%
  ggplot(aes(LagQ, LagP, color=year, label=TimePeriod)) + geom_point() + theme_classic() + geom_line() +
  geom_label() + facet_wrap(~LineDescription, scales = "free")


PCE_Items %>% filter(LineDescription %in% c("New motor vehicles (55)","Net purchases of used motor vehicles (56)")) %>%
  ggplot(aes(date,Quantity, color=LineDescription)) + geom_line() + theme_classic()


Q_All <- get_NIPA_data(beaKey, 'U20403', 'Q', 'All', data_set_name = 'NIUnderlyingDetail') %>%
  mutate(Quantity = DataValue)
Q_All <- BEA_date_quarterly(Q_All) %>% mutate(year = year(date), year = as.factor(year))

# Wait what durable goods have actually decreased?
Q_All %>% filter(LineDescription %in% c("New motor vehicles (55)","Net purchases of used motor vehicles (56)")) %>%
  filter(date > "2010-01-01") %>%
  draw_Trendline("12/1/2015", "12/1/2019", 1) %>%
  ggplot(aes(date,DataValue, color=LineDescription)) + geom_line() + theme_classic() +
  geom_line(aes(date,trendline, color=LineDescription), linetype="dotted", size=1.5)



#### CHECK OUT PORTFOLIO MANAGEMENT
PCE_Weight <- get_NIPA_data(beaKey, 'U20405', 'M', '2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Weight <- BEA_date_monthly(PCE_Weight)

GDP_Weight <- PCE_Weight %>% filter(SeriesCode == "DPCERC") %>%
  select(date, TotalGDP = DataValue)

PCE_Weight <- PCE_Weight %>%
  left_join(GDP_Weight, by="date") %>%
  mutate(PCEweight = DataValue/TotalGDP) %>%
  select(date, LineDescription, PCEweight)

PCE_Items <- get_NIPA_data(beaKey, 'U20404', 'M', '2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Items <- BEA_date_monthly(PCE_Items)

PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'M', '2016,2017,2018,2019,2020,2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- BEA_date_monthly(PCE_Q) %>% select(LineDescription, date, Quantity = DataValue)

PCE_Items <- PCE_Items %>%
  left_join(PCE_Weight, by=c('date' = 'date','LineDescription' = 'LineDescription')) %>%
  left_join(PCE_Q, by=c('date' = 'date','LineDescription' = 'LineDescription'))

PCE_Items <- PCE_Items %>%
  group_by(SeriesCode) %>%
  mutate(DataValue_P1 = (DataValue - lag(DataValue,1))/lag(DataValue,1)) %>%
  mutate(WDataValue_P1 = DataValue_P1*PCEweight) %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^12-1) %>%
  mutate(Quantity_P1 = (Quantity - lag(Quantity,1))/lag(Quantity,1)) %>%
  ungroup()

PCE_Items %>% filter(LineDescription =="Portfolio management and investment advice services") %>%
  mutate(year = as.character(year(date))) %>%
  ggplot(aes(date, WDataValue_P1a)) + geom_line(color="blue") + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Portfolio management and investment advice services, Contribution to PCE inflation, Annualized",
       subtitle = "",
       caption ="BEA, NIPA Tables 2.4.4 and 2.4.5, Weights approximated as nominal consumption shares as a percent of the total, Author's Calculation. Mike Konczal, Roosevelt Institute") +
  theme(plot.title.position = "plot", plot.title = element_text(size = 18), axis.text.y = element_text(size=15),axis.text.x = element_text(size=9)) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%b %Y", breaks = "6 month")
ggsave("portfolios.png", dpi="retina", width = 12, height=6.75, units = "in")





### TRY QUARTERLY
### TKTKTK ADD BACK AUTOS NEW AND USED HERE
PCE_Items <- get_NIPA_data(beaKey, 'U20404', 'M', '2021,2022', data_set_name = 'NIUnderlyingDetail')
PCE_Q <- get_NIPA_data(beaKey, 'U20403', 'M', '2021,2022', data_set_name = 'NIUnderlyingDetail') %>%
  select(LineDescription, TimePeriod, Quantity = DataValue)

PCE_Items <- PCE_Items %>%
  left_join(PCE_Q, by=c('TimePeriod' = 'TimePeriod','LineDescription' = 'LineDescription'))


food_chart <- PCE_Items %>% filter(LineDescription == "Portfolio management and investment advice services") %>%
  rename(Price = DataValue) %>% select(LineDescription, TimePeriod, Price, Quantity) %>%
  group_by(LineDescription) %>% arrange(TimePeriod) %>%
  mutate(Price2019 = lag(Price,1), Quantity2019 = lag(Quantity,1)) %>%
  ungroup()

food_chart <- BEA_date_monthly(food_chart)

food_chart %>%
  filter(date > "2021-09-01") %>% arrange(date) %>%
  ggplot(aes(Quantity, Price, label=TimePeriod)) + geom_point(size=5) + theme_classic() +
  geom_segment(aes(
    xend=c(tail(Quantity, n=-1), NA), 
    yend=c(tail(Price, n=-1), NA))) +
  geom_label_repel() +
  labs(title="Portfolio management and investment advice services, expanded supply?",
       subtitle= "",
       caption="Inspired by Adam Shapiro's SF Fed Inflation work. NIPA Table 2.4.3 and 2.4.4, Author's Calculations. Mike Konczal, Roosevelt Institute",
       x="Quantity (Index)", y="Price (Index)") +
  theme(plot.title.position = "plot", plot.title = element_text(size = 18),
        axis.text.y = element_text(size=15),axis.text.x = element_text(size=15),
        axis.title = element_text(size=15))

ggsave("supply_demand_portfolio.png", dpi="retina", width = 12, height=6, units = "in")


+
  theme(plot.title = element_text(size = 25),
        plot.caption = element_text(size=12),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=18, margin=ggplot2::margin(9,0,15,0),lineheight=1.05)) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=15, color="#222222"), panel.background = element_blank())  +
  scale_colour_brewer(palette="Set1")


ggsave("food_inflation.png", dpi="retina", width = 12, height=6, units = "in")


#######

T11500



beaKey <- "C0C06CC5-8A78-4345-90E7-77A37296C0CC"
bea_1_15_request <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T11500',
  'Frequency' = 'Q',
  'Year' = 'All',
  'ResultFormat' = 'json'
);
bea_1_15_data <- beaGet(bea_1_15_request, asWide = FALSE)

bea_1_15_data <- bea_1_15_data %>%
  mutate(year = substr(TimePeriod, 1, 4)) %>%
  mutate(quarter = substr(TimePeriod, 5,6)) %>%
  mutate(month = case_when(
    quarter == "Q1" ~ 3,
    quarter == "Q2" ~ 6,
    quarter == "Q3" ~ 9,
    quarter == "Q4" ~ 12))
bea_1_15_data$date <- paste(bea_1_15_data$month, "01", bea_1_15_data$year, sep="/")
bea_1_15_data$date <- as.Date(bea_1_15_data$date, "%m/%d/%Y")

filter_1_15 <- c('Price per unit of real gross value added of nonfinancial corporate business','Compensation of employees (unit labor cost)', 'Unit nonlabor cost', 'Corporate profits with IVA and CCAdj (unit profits from current production)')

bea_1_15 <- bea_1_15_data %>%
  filter(LineDescription %in% filter_1_15) %>%
  mutate(item_name = case_when(
    LineDescription == 'Price per unit of real gross value added of nonfinancial corporate business' ~ 'Total',
    LineDescription == 'Compensation of employees (unit labor cost)' ~ 'Labor Cost',
    LineDescription == 'Unit nonlabor cost' ~ 'Nonlabor Cost',
    LineDescription == 'Corporate profits with IVA and CCAdj (unit profits from current production)' ~ 'Corporate Profits',
  )) %>%
  select(item_name, date, DataValue)

bea_1_15 <- bea_1_15 %>% pivot_wider(names_from = item_name, values_from = DataValue) %>%
  mutate(pCP = `Corporate Profits`/Total, cpCP = pCP-lag(pCP,12)) %>%
  mutate(lCP = `Corporate Profits`-lag(`Corporate Profits`,12)) %>%
  mutate(lT = Total - lag(Total,12)) %>%
  mutate(percentage = lCP/lT)

bea_1_15 %>% ggplot(aes(date,pCP)) + geom_line() + theme_classic()
bea_1_15 %>% ggplot(aes(date,cpCP)) + geom_line() + theme_classic()
bea_1_15 %>% filter(date > "1980-01-01") %>%
                      ggplot(aes(date,percentage)) + geom_line() + theme_classic()

%>%
  mutate(item_name = factor(item_name, levels = c("Corporate Profits", "Nonlabor Cost", "Labor Cost"))) %>%
  ggplot(aes(x = date, y = DataValue, fill = item_name)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "After Skyrocketing, Shrinking Corporate Profits Have Been Lowering Costs in Recent Quarters",
       subtitle = "Price per unit of real gross value added of nonfinancial corporate business",
       caption ="BEA, NIPA, Table 1.15, Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Set1") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month")


# By Lagged
bea_1_15_data %>%
  filter(date > "2019-12-01") %>%
  filter(LineDescription %in% filter_1_15) %>%
  mutate(item_name = case_when(
    LineDescription == 'Compensation of employees (unit labor cost)' ~ 'Labor Cost',
    LineDescription == 'Unit nonlabor cost' ~ 'Nonlabor Cost',
    LineDescription == 'Corporate profits with IVA and CCAdj (unit profits from current production)' ~ 'Corporate Profits',
  )) %>%
  group_by(item_name) %>%
  mutate(DataValueLagged = DataValue - lag(DataValue,1)) %>%
  ungroup() %>%
  mutate(item_name = factor(item_name, levels = c("Corporate Profits", "Nonlabor Cost", "Labor Cost"))) %>%
  ggplot(aes(x = date, y = DataValueLagged, fill = item_name)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "After Slightly Shrinking, Corporate Profits Dramatically Increase Costs Last Quarter",
       subtitle = "Quarterly change, price per unit of real gross value added of nonfinancial corporate business",
       caption ="BEA, NIPA, Table 1.15, Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Set1") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month") +
  theme(plot.title = element_text(size = 30, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=20, margin=margin(19,0,11,0), lineheight=1.05),
        legend.text=element_text(size=20))

ggsave("1_15.png", width = 19, height=10.68, dpi="retina")


# Long trend

PCE_inflation <- get_NIPA_data(beaKey, 'T20804', 'M', 'All') %>%
  filter(SeriesCode == "DPCCRG")
PCE_inflation <- BEA_date_monthly(PCE_inflation) %>%
  mutate(pIncrease = (DataValue- lag(DataValue,1))/lag(DataValue,1)) %>%
  mutate(pIncreaseA = (pIncrease+1)^12-1) %>%
  mutate(value = DataValue)

long_plot <- PCE_inflation %>%
  filter(date >= "2006-01-01") %>%
  # Probably an easier way to do the following projections past dates, may revisit someday
  mutate(logical2009 = (date >= "2008-01-01")) %>%
  mutate(value2009 = 1.02^(cumsum(logical2009)/12)) %>%
  mutate(value2009 = logical2009*value2009*value[date=="2008-01-01"]) %>%
  mutate(logical2020 = (date >= "2020-01-01")) %>%
  mutate(value2020 = 1.02^(cumsum(logical2020)/12)) %>%
  mutate(value2020 = logical2020*value2020*value[date=="2020-01-01"])

long_plot$final_value2 <- na_if(long_plot$value2020, 0)
long_plot$final_value <- na_if(long_plot$value2009, 0)

ggplot(long_plot, aes(x=date)) + 
  geom_line(aes(y = value), colour = "#007f7f", size = 1.5) + 
  geom_line(aes(y = final_value), color="steelblue", linetype="dashed", size=1) +
  geom_line(aes(y = final_value2), color="steelblue", linetype="dashed", size=1)  +
  labs(title = "Wait, Did the Fed Clarify Which Trend the Target is Flexible Around?",
       subtitle = "If PCE core inflation continues at last three months' average, we'll cross 2009 Core PCE path in January 2023",
       caption = "BEA, NIPA Table 2.8.4, PCE excluding food and energy, projected at 2% increase, author's calculation. Mike Konczal, Roosevelt Institute",
       x="", y="") + theme_classic() +
  theme(axis.text = element_text(size=18))

  
  ggsave("long_PCE.png", dpi="retina", width = 12, height=6.75, units = "in")