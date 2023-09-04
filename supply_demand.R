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
source("1b_load_PCE_items.R")
#load("data/pce_supply_demand.RData")
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt


# Function to remove outliers for graphics
remove_outliers <- function(x, multiplier = 1.5) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - multiplier * IQR
  upper_bound <- Q3 + multiplier * IQR
  x[(x < lower_bound) | (x > upper_bound)] <- NA
  return(x)
}

# Get preloaded categories and levels for each
lowest <- read_csv("data/pce_items_lowest.csv")

months_change <- 6
compare_date <- "2022-12-01"
compare_date <- max(pce$date) %m-% months(months_change+1)
#compare_date <- as.Date("2022-07-01")

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
  filter(category != "Food and Energy") %>%
  arrange(LineDescription) %>%
  mutate(x = LineDescription == lag(LineDescription)) %>%
  #filter(!x) %>%
  filter(level == "Level 4") %>%
  mutate(QuantityFinal = remove_outliers(QuantityFinal, 3)) %>%
  mutate(PriceFinal = remove_outliers(PriceFinal, 3)) %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight, color=category)) + geom_point(aes(fill="skyblue"), alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(subtitle = "3-month Change July 2023 Minus 3-month Change December 2022, Quantity and Inflation, for ~130 Core PCE Item Categories",
       title="Deceleration is Driven by Expanded Supply",
       caption = "Outliers 3x IQR range removed. Based on Adam Shapiro's work, San Francisco Fed. Mike Konczal, Roosevelt Institute",
       y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", legend.position = c(0.85,0.9)) +
  scale_size_continuous(range = c(3, 10)) +
  facet_wrap(~category, scales = "free_y")

ggsave("graphics/supply_demand.png", dpi="retina", width = 12, height=6.75, units = "in")


pce_sd <- pce %>% group_by(LineDescription) %>% filter(LineDescription != 323 | LineDescription != 324) %>%
  filter(year(date)<2020) %>%
  mutate(QuantityFinal = Quantity/lag(Quantity,months_change)-1,
         QuantityFinal = QuantityFinal - lag(QuantityFinal,7)
  ) %>%
  na.omit() %>%
  summarize(sd_quantity = sd(QuantityFinal)) %>% ungroup()

b <-
recent %>% inner_join(lowest,by="LineDescription") %>%
  filter(category != "Aggregate") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  arrange(LineDescription) %>%
  mutate(x = LineDescription == lag(LineDescription)) %>%
  filter(!x) %>%
  filter(level == "Level 4") %>%
  #left_join(pce_sd, by="LineDescription") %>%
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
  mutate(Sn = n/sum(n), SnW = nW/sum(nW)) %>%
  ungroup() %>%
  arrange(category)


c <- b %>% filter(category != "Food and Energy") %>% select(-n,-nW) %>%
  pivot_wider(names_from = category, values_from = c(Sn,SnW))

b %>% group_by(category) %>% summarize(sum(nW))
### assume we have shapiro loaded  - cyclical_categories

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

recent %>% 
  filter(LineDescription %in% cyclical_categories) %>%
  inner_join(lowest,by="LineDescription") %>%
#  mutate(QuantityFinal = remove_outliers(QuantityFinal, 3)) %>%
#  mutate(PriceFinal = remove_outliers(PriceFinal, 3)) %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight,fill=category)) + geom_point(alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(subtitle = "3-month Change June 2023 Minus 3-month Change June 2022, Quantity and Inflation, for ~130 Core PCE Item Categories",
       title="Deceleration is Driven by Expanded Supply",
       caption = "Outliers 3x IQR range removed. Based on Adam Shapiro's work, San Francisco Fed. Mike Konczal, Roosevelt Institute",
       y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", legend.position = "bottom") +
  scale_size_continuous(range = c(3, 10))

recent %>% filter(LineDescription %in% cyclical_housing_categories)


b <-
  recent %>% inner_join(lowest,by="LineDescription") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  filter(LineDescription %in% cyclical_categories) %>%
  #left_join(pce_sd, by="LineDescription") %>%
  mutate(quadrant = case_when(
    QuantityFinal > 0 & PriceFinal > 0 ~ "Quadrant 1 - Demand+",
    QuantityFinal < 0 & PriceFinal > 0 ~ "Quadrant 2 - Supply-",
    QuantityFinal < 0 & PriceFinal < 0 ~ "Quadrant 3 - Demand-",
    QuantityFinal > 0 & PriceFinal < 0 ~ "Quadrant 4 - Supply+",
    TRUE ~ "Undefined")) %>%
  group_by(quadrant) %>%
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  mutate(Sn = n/sum(n), SnW = nW/sum(nW))


c <- b %>% filter(category != "Food and Energy") %>% select(-n,-nW) %>%
  pivot_wider(names_from = category, values_from = c(Sn,SnW))

b %>% group_by(category) %>% summarize(sum(nW))

#### Historical dive ####

pce %>% filter(LineDescription %in% lowest$LineDescription) %>%
  group_by(date) %>%
  summarize(median = median(DataValue_P1),
            p25 = quantile(DataValue_P1, probs = 0.25, na.rm=TRUE),
            p90 = quantile(DataValue_P1, probs = 0.9, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(median:p90, names_to = "type", values_to = "change") %>%
  filter(change > -0.75) %>%
  ggplot(aes(date,change)) + geom_point() + theme_classic() + facet_wrap(~type, scales = "free")


pce %>% filter(LineDescription %in% lowest$LineDescription) %>%
  group_by(LineDescription) %>%
  mutate(DataValue_P6 = DataValue/lag(DataValue,6),
         DataValue_P6 = DataValue_P6^2-1) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(median = median(DataValue_P6, rm.na = TRUE),
            p10 = quantile(DataValue_P6, probs = 0.10, na.rm=TRUE),
            p25 = quantile(DataValue_P6, probs = 0.25, na.rm=TRUE),
            p90 = quantile(DataValue_P6, probs = 0.9, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(median:p90, names_to = "type", values_to = "change") %>%
  filter(year(date) > 1965, type != "p90") %>%
  ggplot(aes(date,change, color=type)) + geom_line() + theme_classic() +
  geom_hline(yintercept = 0)
