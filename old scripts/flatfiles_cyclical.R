library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(viridis)
library(quantmod)
library(broom)


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
load_flat_files <- function(location = "https://apps.bea.gov/national/Release/TXT/", type = "Q"){
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

monthly_data <- load_flat_files(type = "M")



monthly_data %>% filter(table_id == "U20404", series_label == "PCE excluding food and energy") %>% mutate(YoY = value/lag(value, 12)) %>%
  ggplot(aes(date, YoY)) + geom_line()


###
lowest <- read_csv("data/pce_items_lowest.csv") %>% rename(series_label = LineDescription)
lowest

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

lm_results <- monthly_data %>% filter(table_id == "U20404") %>%
  group_by(series_code) %>%
  mutate(DataValue_P1 = value/lag(value)-1) %>%
  ungroup() %>%
  left_join(unrate, by="date") %>%
  left_join(lowest, by="series_label") %>%
  filter(date >= "2000-01-01") %>%
  filter(level == "Level 4") %>%
  mutate(DataValue_P1a = (DataValue_P1+1)^12-1) %>%
  group_by(series_label) %>%
  # Regression: inflation = lagged inflation + unemployment gap
  do(tidy(lm(DataValue_P1a ~ lag(DataValue_P1a) + I(unrate - nrou), data = .))) %>%
  # Filter for statistically significant negative esimates on unemployment gap
  filter(term == "I(unrate - nrou)", statistic < -1.95) %>% arrange(estimate)
print(lm_results, n=Inf)

monthly_data %>% filter(table_id == "U20404") %>%
  group_by(series_code) %>%
  mutate(DataValue_P1 = value/lag(value)-1) %>%
  ungroup() %>%
  filter(series_label %in% lm_results$series_label, year(date) > 2014) %>%
  group_by(date) %>%
  summarize(change = sum(DataValue_P1),
            #weight = sum(PCEweight),
            weight = 1,
            changeW = change/weight,
            changeWA = (changeW+1)^12-1) %>%
  ungroup() %>% mutate(type = "All Phillips Curve items excluding housing") %>%
  ggplot(aes(date,changeWA)) + geom_line(size=1.1) + theme_classic()




########### THE BIG ONE
PCE_Weight <- monthly_data %>% filter(table_id == "U20405")

GDP_Weight <- PCE_Weight %>% filter(series_code == "DPCERC") %>%
  select(date, TotalGDP = value)

PCE_Weight <- PCE_Weight %>%
  left_join(GDP_Weight, by="date") %>%
  # The weight is approximated as nominal consumption shares as a percent of the total.
  mutate(PCEweight = value/TotalGDP) %>%
  select(date, series_label, PCEweight)

pce <- monthly_data %>% filter(table_id == "U20404")

pce <- pce %>%
  left_join(PCE_Weight, by=c('date' = 'date','series_label' = 'series_label'))

pce <- pce %>%
  group_by(series_label) %>%
  mutate(DataValue_P1 = (value - lag(value,1))/lag(value,1)) %>%
  # Use the lagged weight
  mutate(WDataValue_P1 = DataValue_P1*lag(PCEweight,1)) %>%
  mutate(WDataValue_P1a = (1+WDataValue_P1)^12-1) %>%
  ungroup()

pce %>%
  filter(series_label %in% lm_results$series_label, year(date) > 2014) %>%
  group_by(date) %>%
  summarize(change = sum(WDataValue_P1),
            weight = sum(PCEweight),
            changeW = change/weight,
            changeWA = (changeW+1)^12-1) %>%
  ungroup() %>% mutate(type = "All Phillips Curve items excluding housing") %>%
  ggplot(aes(date,changeWA)) + geom_line(size=1.1) + theme_classic()
