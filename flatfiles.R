library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(viridis)

#### Format the date column, takes the column
BEA_format_date <- function(x) {
  year <- substr(x, 1, 4)
  # Assumes there is only one type of data in the call.
  identifier <- substr(x[1], 5, 5)
  
  if(identifier == "Q") {
    quarter <- substr(x, 6, 6)
    month <- ifelse(quarter == "1", "03",
                    ifelse(quarter == "2", "06",
                           ifelse(quarter == "3", "09", "12")))
  }
  
  if(identifier == "M") {
    month <- substr(x, 6, 7)
  }
  
  FormattedDate <- as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")
  return(FormattedDate)
}

#### Download and format data. No analysis.
load_flat_files <- function(location, type = "Q"){
  print(Sys.time())
  series <- read_csv(paste(location,"SeriesRegister.txt", sep=""), show_col_types = FALSE) %>% clean_names() %>% rename(series_code = percent_series_code)
  tables <- read_csv(paste(location,"TablesRegister.txt", sep=""), show_col_types = FALSE) %>% clean_names()
  print(paste("Loading ", type," data"))
  data <- read_csv(paste(location,"nipadata",type,".txt", sep=""), show_col_types = FALSE)  %>% clean_names()  %>% rename(series_code = percent_series_code)
  print("Formatting date")
  data$date <- BEA_format_date(data$period)
  print("Formatting tables")
  final_data <- data %>% left_join(series, by="series_code") %>% separate_rows(table_id_line_no, sep = "\\|") %>%
  separate(table_id_line_no, into = c("table_id", "line_no"), sep = ":", remove=FALSE) %>% left_join(tables, by="table_id")
  print(Sys.time())
  return(final_data)
  }

#### 

location <- "../FlatFiles/"
location <- "https://apps.bea.gov/national/Release/TXT/"
quarterly_data <- load_flat_files(location, type = "Q")

old_q <- read_csv("../R Code/FlatFiles_2023_9_27/nipadataQ.txt") %>% rename(old_value = Value) %>% clean_names() %>% rename(series_code = percent_series_code)
old_m <- read_csv("../R Code/FlatFiles_2023_9_27/nipadataM.txt") %>% rename(old_value = Value) %>% clean_names() %>% rename(series_code = percent_series_code)
q_df <- quarterly_data %>% left_join(old_q, by=c("series_code", "period"))

q_df %>% filter(table_id_line_no == "T10106:1") %>% filter(year(date)>2022) %>%
  select(date, series_label, value, old_value)

save(quarterly_data, file = "quarterly.Rdata")
monthly_data <- load_flat_files(location, type="M")

q_df %>% filter(table_id_line_no == "T10101:1", year(date) >= 2021) %>%
  ggplot(aes(date,value)) + geom_line() + geom_line(aes(date,old_value),color="red")

quarterly_data %>% filter(table_id == "U20404", series_label == "PCE excluding food and energy") %>% mutate(YoY = value/lag(value,4)-1) %>%
  ggplot(aes(date, YoY)) + geom_line()

q_df %>% filter(table_id == "U20404", series_label == "PCE excluding food and energy") %>% mutate(YoY = value/lag(value,4)-1, YoY_old = old_value/lag(old_value,4)-1) %>%
  filter(year(date) > 2017) %>%
  ggplot(aes(date, YoY)) + geom_line() + geom_line(aes(date,YoY_old),color="red")

q_df %>% filter(table_id == "U20404", series_label == "PCE excluding food and energy") %>% mutate(YoY = value/lag(value,1), YoY = YoY^4-1, YoY_old = old_value/lag(old_value,1), YoY_old = YoY_old^4-1) %>%
  filter(year(date) >= 2016) %>%
  ggplot(aes(date, YoY)) + geom_line() + geom_line(aes(date,YoY_old),color="red")


q_df %>% filter(table_id == "U20404", series_label == "PCE excluding food and energy") %>% select(value, old_value, date) %>%
  pivot_longer(value:old_value, names_to = "type", values_to = "value") %>%
  group_by(type) %>% mutate(qchangeA = value/lag(value,1), qchangeA = qchangeA^4-1) %>%
  ungroup() %>%
  mutate(type = if_else(type=="value","New Value", "Old Value")) %>%
  filter(year(date)>2018) %>%
  ggplot(aes(date,qchangeA,color=type)) + geom_line(size=1.2) + theme_classic() +
  theme(legend.position = c(0.8,0.6), plot.title.position = "plot") +
  scale_y_continuous(label=percent) + theme(legend.title=element_blank()) +
  labs(subtitle="PCE excluding food and energy, quarterly change annualized, through Q2 2023, before and after 5-year revisions.", y="",
       caption="Mike Konczal, Roosevelt Institute") +
  scale_color_manual(values = c("New Value" = "#b73779", "Old Value" = "#3b528b"))

ggsave("graphics/pce_before_after.png", dpi="retina", width = 8, height=8, units = "in")

q_df %>% filter(table_id_line_no == "T10101:1") %>% select(value, old_value, date) %>%
  pivot_longer(value:old_value, names_to = "type", values_to = "value") %>%
  mutate(value = value/100) %>%
  mutate(type = if_else(type=="value","New Value", "Old Value")) %>%
  filter(year(date)>=2021) %>%
  ggplot(aes(date,value,color=type)) + geom_line(size=1.2) + theme_classic() +
  theme(legend.position = c(0.8,0.75), plot.title.position = "plot") +
  scale_y_continuous(label=percent) + theme(legend.title=element_blank()) +
  labs(subtitle="Table 1.1.1:1, Percent Change From Preceding Period in Real Gross Domestic Product, Seasonally adjusted at annual rates, through Q2 2023, before and after 5-year revisions.",
       caption="Mike Konczal, Roosevelt Institute", y="") +
  scale_color_manual(values = c("New Value" = "#b73779", "Old Value" = "#3b528b"))

ggsave("graphics/gdp_before_after.png", dpi="retina", width = 12, height=12, units = "in")

q_df %>% filter(table_id_line_no == "T11701:2") %>% select(value, old_value, date) %>%
  pivot_longer(value:old_value, names_to = "type", values_to = "value") %>%
  mutate(value = value/100) %>%
  mutate(type = if_else(type=="value","New Value", "Old Value")) %>%
  filter(year(date)>=2016) %>%
  ggplot(aes(date,value,color=type)) + geom_line(size=1.2) + theme_classic() +
  theme(legend.position = c(0.8,0.75), plot.title.position = "plot") +
  scale_y_continuous(label=percent) + theme(legend.title=element_blank()) +
  labs(title="Found some nominal income in the couch cushions.",
       subtitle="Table 1.17.1. Percent Change From Preceding Period in Real Gross Domestic Income,Seasonally adjusted at annual rates, through Q2 2023, before and after 5-year revisions.",
       caption="Mike Konczal, Roosevelt Institute", y="") +
  scale_color_manual(values = c("New Value" = "#b73779", "Old Value" = "#3b528b"))

ggsave("graphics/rgdi_before_after.png", dpi="retina", width = 12, height=12, units = "in")



pce_weight <- q_df %>% filter(table_id =="U20405", series_code == "DPCERC") %>% select(date, TotalGDP = value)

PCE_Weight <- q_df %>% filter(table_id =="U20405") %>%
  left_join(pce_weight, by="date") %>%
  # The weight is approximated as nominal consumption shares as a percent of the total.
  mutate(PCEweight = value/TotalGDP) %>%
  select(date, series_label, PCEweight)

pce_df <- q_df %>% filter(table_id =="U20404") %>%
  left_join(PCE_Weight, by=c('date' = 'date','series_label' = 'series_label')) %>%
  group_by(series_label) %>%
  mutate(DataValue_P1 = (value - lag(value,1))/lag(value,1)) %>%
  # Use the lagged weight
  mutate(WDataValue_P1 = DataValue_P1*lag(PCEweight,1)) %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^4-1) %>%
  ungroup()


pce_df