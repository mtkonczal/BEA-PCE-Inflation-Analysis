library(quantmod)
library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(hrbrthemes)

setwd("/Users/mkonczal/Documents/GitHub/BEA-PCE-Inflation-Analysis/")
beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
beaKey <- as.character(beaKey)
source("1a_helper_functions.R")
#source("1b_load_PCE_items_all.R")
#source("1b_load_PCE_items.R")
load("data/pce_long.RData")
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt


remove_outliers <- function(x, multiplier = 1.5) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - multiplier * IQR
  upper_bound <- Q3 + multiplier * IQR
  
  x[(x < lower_bound) | (x > upper_bound)] <- NA
  return(x)
}

lowest <- read_csv("data/pce_items_lowest.csv")

months_change <- 12
compare_date <- max(pce$date) %m-% months(12)

recent <-pce %>% group_by(LineDescription) %>% filter(LineDescription != 323 | LineDescription != 324) %>%
  mutate(QuantityFinal = Quantity/lag(Quantity,months_change)-1,
         PriceFinal = DataValue/lag(DataValue,months_change)-1,
  ) %>%
  filter(date == max(date) | date == compare_date) %>%
  summarize(QuantityFinal = QuantityFinal[date == max(date)] - QuantityFinal[date == compare_date],
            PriceFinal = PriceFinal[date == max(date)] - PriceFinal[date == compare_date],
            weight = PCEweight[date == max(date)]) %>%
  ungroup()

recent %>% inner_join(lowest,by="LineDescription") %>% filter(category != "Aggregate") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  arrange(LineDescription) %>%
  mutate(x = LineDescription == lag(LineDescription)) %>%
  filter(!x) %>%
  filter(level == "Level 4") %>%
  mutate(QuantityFinal = remove_outliers(QuantityFinal, 3)) %>%
  mutate(PriceFinal = remove_outliers(PriceFinal, 3)) %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight, color=category)) + geom_point(aes(fill="skyblue"), alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(subtitle = "12-month Change June 2023 Minus 12-month Change June 2023, Quantity and Inflation, for 130 PCE Item Categories",
       caption = "Outliers 3x IQR range removed. Based on Adam Shapiro's work, San Francisco Fed. Mike Konczal, Roosevelt Institute",
       y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", legend.position = c(0.85,0.9)) +
  scale_size_continuous(range = c(3, 10)) + facet_wrap(~category, scales = "free_y")

ggsave("graphics/supply_demand.png", dpi="retina", width = 12, height=6.75, units = "in")

recent %>% inner_join(lowest,by="LineDescription") %>%
  filter(category != "Aggregate") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  arrange(LineDescription) %>%
  mutate(x = LineDescription == lag(LineDescription)) %>%
  filter(!x) %>%
  filter(level == "Level 4") %>%
  mutate(quadrant = case_when(
    QuantityFinal > 0 & PriceFinal > 0 ~ "Quadrant 1 - Demand+",
    QuantityFinal < 0 & PriceFinal > 0 ~ "Quadrant 2 - Supply-",
    QuantityFinal < 0 & PriceFinal < 0 ~ "Quadrant 3 - Demand-",
    QuantityFinal > 0 & PriceFinal < 0 ~ "Quadrant 4 - Supply+",
    TRUE ~ "Undefined")) %>%
  group_by(quadrant, category) %>%
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(n = n/sum(n)) %>%
  ungroup() %>%
  arrange(category)