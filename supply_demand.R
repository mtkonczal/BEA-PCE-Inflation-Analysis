library(quantmod)
library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

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

# Function for formatting percent signs in charts.
f_pct <- function(n) {
  return(str_c(sprintf('%.f', 100*n), "%"))
}

# Get preloaded categories and levels for each from data file.
lowest <- read_csv("data/pce_items_lowest.csv")

months_change <- 3
compare_date <- "2022-12-01"

recent <-pce %>% group_by(date) %>%
  # This removes duplicates values put in the BEA underlying table.
  distinct(LineNumber, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(LineDescription) %>%
  # Take the six-month change in inflation and quantities for all values and all dates.
  mutate(QuantityFinal = Quantity/lag(Quantity,months_change)-1,
         PriceFinal = DataValue/lag(DataValue,months_change)-1,
  ) %>%
  # Isolate the latest date and the comparison date.
  filter(date == max(date) | date == compare_date) %>%
  # Take the difference in rates, so using a deceleration, and keep the latest weight.
  summarize(QuantityFinal = QuantityFinal[date == max(date)] - QuantityFinal[date == compare_date],
            PriceFinal = PriceFinal[date == max(date)] - PriceFinal[date == compare_date],
            weight = PCEweight[date == max(date)]) %>%
  ungroup()

quandrants <-
recent %>%
  # The next 5 lines isolates out the Level 4 categories we want.
  inner_join(lowest,by="LineDescription") %>%
  filter(category != "Aggregate") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  arrange(LineDescription) %>%
  filter(level == "Level 4") %>%
  # Assign to a quadrant.
  mutate(quadrant = case_when(
    QuantityFinal > 0 & PriceFinal > 0 ~ "1Demand+",
    QuantityFinal < 0 & PriceFinal > 0 ~ "2Supply-",
    QuantityFinal < 0 & PriceFinal < 0 ~ "3Demand-",
    QuantityFinal > 0 & PriceFinal < 0 ~ "4Supply+",
    TRUE ~ "Undefined"))

# This creates the csv file of all the Level 4 values to display in Github.
quandrants_print <- quandrants %>% select(-leading_spaces, -lowest)
quandrants_print$quadrant <- substr(quandrants_print$quadrant, 2, nchar(quandrants_print$quadrant))
write_csv(quandrants_print, "data/quandrants_data.csv")
quandrants_print %>% filter(category != "Food and Energy") %>% write_csv("data/pce_supply.csv")

# First do analysis for all core items.
all <- quandrants %>%
  filter(category != "Food and Energy") %>%
  # Just one category
  mutate(category = "All_Core") %>%
  group_by(quadrant, category) %>%
  # Sum of weight and weighted change.
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  # The sums above as a percent of total sum.
  mutate(Sn = n/sum(n), SnW = nW/sum(nW))

# Second, do analysis for categories goods and services.
g_s <-  quandrants %>%
  #execute code %>%
  filter(category != "Food and Energy") %>%
  group_by(quadrant, category) %>%
  summarize(n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(Sn = n/sum(n), SnW = nW/sum(nW)) %>%
  ungroup() %>%
  rbind(all)

# Formatting for chart 1.
chart1 <- g_s %>% filter(category != "Food and Energy") %>% select(-n,-nW) %>%
  pivot_wider(names_from = category, values_from = c(Sn,SnW)) %>%
  mutate(fall_all = if_else(SnW_All_Core > 0, SnW_All_Core/(SnW_All_Core[3]+SnW_All_Core[4]),as.numeric(NA)),
         fall_services = if_else(SnW_Goods > 0, SnW_Services/(SnW_Services[3]+SnW_Services[4]),as.numeric(NA)),
         fall_goods = if_else(SnW_Goods > 0, SnW_Goods/(SnW_Goods[3]+SnW_Goods[4]),as.numeric(NA))) %>%
  select(quadrant, Sn_All_Core, SnW_All_Core, fall_all, Sn_Goods, SnW_Goods, fall_goods,Sn_Services, SnW_Services, fall_services)

chart1$Sn_All_Core <- f_pct(chart1$Sn_All_Core)
chart1$Sn_Goods <- f_pct(chart1$Sn_Goods)
chart1$Sn_Services <- f_pct(chart1$Sn_Services)
chart1$fall_all <- f_pct(chart1$fall_all) %>% str_replace_all("NA%",as.character(NA))
chart1$fall_services <- f_pct(chart1$fall_services) %>% str_replace_all("NA%",as.character(NA))
chart1$fall_goods <- f_pct(chart1$fall_goods) %>% str_replace_all("NA%",as.character(NA))
chart1$SnW_All_Core <- round(chart1$SnW_All_Core,2)
chart1$SnW_Goods <- round(chart1$SnW_Goods,2)
chart1$SnW_Services <- round(chart1$SnW_Services,2)
chart1$quadrant <- substr(chart1$quadrant, 2, nchar(chart1$quadrant))

# Create chart 1.
write_csv(chart1, "data/chart1.csv")

#### Center Analysis ####

# In the paper I look at what changes are relevant if we have a small threshold for
# no-change, here as 0.001. Here are those results.
quandrants_center <- quandrants %>%
  filter(PriceFinal < 0) %>%
  mutate(quadrant = if_else(abs(QuantityFinal) < 0.001,"center-",quadrant)) %>%
  group_by(quadrant, category) %>%
  summarize(n = sum(weight)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(Sn = n/sum(n)) %>%
  arrange(category)

quandrants_center <- quandrants %>%
  filter(category != "Food and Energy") %>%
  filter(PriceFinal < 0) %>%
  mutate(quadrant = if_else(abs(QuantityFinal) < 0.001,"center-",quadrant)) %>%
  group_by(quadrant) %>%
  summarize(n = sum(weight)) %>%
  ungroup() %>%
  mutate(Sn = n/sum(n))


#### Cyclical Demand categories. ####

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


cyclical_pce <- 
recent %>% inner_join(lowest,by="LineDescription") %>%
  filter(LineDescription %in% cyclical_categories)

write_csv(cyclical_pce, "data/cyclical_pce.csv")

quandrants <-
  recent %>% inner_join(lowest,by="LineDescription") %>%
  mutate(category = if_else(category %in% c("Energy","Food"), "Food and Energy",category)) %>%
  filter(LineDescription %in% cyclical_categories) %>%
  filter(category != "Food and Energy") %>%
  mutate(quadrant = case_when(
    QuantityFinal > 0 & PriceFinal > 0 ~ "1Demand+",
    QuantityFinal < 0 & PriceFinal > 0 ~ "2Supply-",
    QuantityFinal < 0 & PriceFinal < 0 ~ "3Demand-",
    QuantityFinal > 0 & PriceFinal < 0 ~ "4Supply+",
    TRUE ~ "Undefined")) %>%
  group_by(quadrant) %>%
  summarize(nn = n(), n = sum(weight), nW = sum(weight*PriceFinal)) %>%
  ungroup() %>%
  mutate(Sn = n/sum(n), SnW = nW/sum(nW)) %>%
  ungroup()

quandrants %>%
  mutate(QuantityFinal = remove_outliers(QuantityFinal, 4)) %>%
  ggplot(aes(QuantityFinal,PriceFinal,size=weight, color=category)) + geom_point(aes(fill="skyblue"), alpha=0.5, shape = 21, color = "black", stroke = 1.5, show.legend=FALSE) +
  theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(y = "Percentage Point Change in Price (Inflation)",
       x = "Percentage Point Change in Quantity (Real Value)",
       caption = "Outliers 4x the IQR not displayed in graphic.") +
  facet_wrap(~category) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", legend.position = c(0.85,0.9)) +  theme(strip.background = element_blank())
