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

load_pce_data <- function(){
  full_data <- load_flat_files(type = "M")

  pce_weight <- full_data %>%
    filter(table_id =="U20405", series_code == "DPCERC") %>%
    select(date, TotalGDP = value)
  
  pce_weight <- full_data %>% filter(table_id =="U20405") %>%
    left_join(pce_weight, by="date") %>%
    # The weight is approximated as nominal consumption shares as a percent of the total.
    mutate(PCEweight = value/TotalGDP) %>%
    select(date, series_label, PCEweight) %>%
    distinct(date, series_label, .keep_all = TRUE)
    
  PCE_Q <- full_data %>% filter(table_id =="U20403") %>%
    select(series_label, date, quantity = value) %>%
    distinct(date, series_label, .keep_all = TRUE)
  
  pce_df <- full_data %>% filter(table_id =="U20404") %>%
    left_join(pce_weight, by=c("date","series_label")) %>%
    left_join(PCE_Q, by=c("date","series_label")) %>%
    group_by(series_label) %>%
    mutate(DataValue_P1 = (value - lag(value,1))/lag(value,1)) %>%
    mutate(DataValue_P3 = value/lag(value,3) -1 ) %>%
    mutate(DataValue_P6 = value/lag(value,6) -1 ) %>%
    # Use the lagged weight
    mutate(WDataValue_P1 = DataValue_P1*lag(PCEweight,1)) %>%
    mutate(WDataValue_P1a = (1+WDataValue_P1)^4-1) %>%
    ungroup()
  
  return(pce_df)
  
}
