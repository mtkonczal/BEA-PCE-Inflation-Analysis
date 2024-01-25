library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)
library(quantmod)

source("load_flatfiles.R")

create_chart <- function(quads) {
  all <- quads %>%
    filter(category != "Food and Energy") %>%
    # Just one category
    mutate(category = "All_Core") %>%
    group_by(quadrant, category) %>%
    # Sum of weight and weighted change.
    summarize(n = sum(weight), nW = sum(weight * PriceFinal)) %>%
    ungroup() %>%
    # The sums above as a percent of total sum.
    mutate(Sn = n / sum(n), SnW = nW / sum(nW))
  
  # Second, do analysis for categories goods and services.
  g_s <- quads %>%
    # execute code %>%
    filter(category != "Food and Energy") %>%
    group_by(quadrant, category) %>%
    summarize(n = sum(weight), nW = sum(weight * PriceFinal)) %>%
    ungroup() %>%
    group_by(category) %>%
    mutate(Sn = n / sum(n), SnW = nW / sum(nW)) %>%
    ungroup() %>%
    rbind(all)
  
  
  chart1 <- g_s %>%
    filter(category != "Food and Energy") %>%
    select(-n, -nW) %>%
    pivot_wider(names_from = category, values_from = c(Sn, SnW)) %>%
    mutate(
      fall_all = if_else(SnW_All_Core > 0, SnW_All_Core / (SnW_All_Core[3] + SnW_All_Core[4]), as.numeric(NA)),
      fall_services = if_else(SnW_Goods > 0, SnW_Services / (SnW_Services[3] + SnW_Services[4]), as.numeric(NA)),
      fall_goods = if_else(SnW_Goods > 0, SnW_Goods / (SnW_Goods[3] + SnW_Goods[4]), as.numeric(NA))
    ) %>%
    select(quadrant, Sn_All_Core, SnW_All_Core, fall_all, Sn_Goods, SnW_Goods, fall_goods, Sn_Services, SnW_Services, fall_services)
  
  chart1$Sn_All_Core <- f_pct(chart1$Sn_All_Core)
  chart1$Sn_Goods <- f_pct(chart1$Sn_Goods)
  chart1$Sn_Services <- f_pct(chart1$Sn_Services)
  chart1$fall_all <- f_pct(chart1$fall_all) %>% str_replace_all("NA%", as.character(NA))
  chart1$fall_services <- f_pct(chart1$fall_services) %>% str_replace_all("NA%", as.character(NA))
  chart1$fall_goods <- f_pct(chart1$fall_goods) %>% str_replace_all("NA%", as.character(NA))
  chart1$SnW_All_Core <- round(chart1$SnW_All_Core, 2)
  chart1$SnW_Goods <- round(chart1$SnW_Goods, 2)
  chart1$SnW_Services <- round(chart1$SnW_Services, 2)
  chart1$quadrant <- substr(chart1$quadrant, 2, nchar(chart1$quadrant))
  
  return(chart1)
}

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

# This uses the flat files.
get_quandrants <- function(long_pce, months_change, compare_end, compare_start, lowest) {
  recent <- long_pce %>%
    group_by(date) %>%
    distinct(line_no, .keep_all = TRUE) %>%
    ungroup() %>%
    group_by(series_label) %>%
    mutate(
      QuantityFinal = quantity / lag(quantity, months_change) - 1,
      PriceFinal = value / lag(value, months_change) - 1,
    ) %>%
    filter(date == compare_end | date == compare_start) %>%
    summarize(
      QuantityFinal = QuantityFinal[date == compare_end] - QuantityFinal[date == compare_start],
      PriceFinal = PriceFinal[date == compare_end] - PriceFinal[date == compare_start],
      weight = PCEweight[date == compare_end]
    ) %>%
    ungroup()
  
  recent <-
    recent %>%
    # The next 5 lines isolates out the Level 4 categories we want.
    inner_join(lowest, by = c("series_label" = "LineDescription")) %>%
    filter(category != "Aggregate") %>%
    mutate(category = if_else(category %in% c("Energy", "Food"), "Food and Energy", category)) %>%
    arrange(series_label) %>%
    filter(level == "Level 4") %>%
    # Assign to a quadrant.
    mutate(quadrant = case_when(
      QuantityFinal > 0 & PriceFinal > 0 ~ "1Demand+",
      QuantityFinal < 0 & PriceFinal > 0 ~ "2Supply-",
      QuantityFinal < 0 & PriceFinal < 0 ~ "3Demand-",
      QuantityFinal > 0 & PriceFinal < 0 ~ "4Supply+",
      TRUE ~ "Undefined"
    ))
  
  return(recent)
}


draw_quads <- function(quads, filter = c("Goods", "Services")) {
  quads %>%
    mutate(QuantityFinal = remove_outliers(QuantityFinal, 4)) %>%
    mutate(PriceFinal = remove_outliers(PriceFinal, 4)) %>%
    filter(category %in% filter) %>%
    ggplot(aes(QuantityFinal, PriceFinal, size = weight, color = category)) +
    geom_point(aes(fill = "skyblue"), alpha = 0.5, shape = 21, color = "black", stroke = 1.5, show.legend = FALSE) +
    theme_classic() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme(text = element_text(size = 22)) +
    labs(
      y = "Price (Inflation)",
      x = "Quantity",
      subtitle = "Change in 12-month change, inflation and quantity, 123 PCE categories.\n12-month change Nov 2023 compared to baseline 12-month change November 2022.",
      title = "80 Percent of Inflation Deceleration is Happening Where Supply is Increasing",
      caption = "NIPA tables 2.4.3U, 2.4.4U, 2.4.5U. Mike Konczal, Roosevelt Institute."
    ) +
    facet_wrap(~category) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    theme(plot.title.position = "plot") +
    theme(strip.background = element_blank())
}


##### START CODE ######

#long_pce <- load_pce_data()

lowest <- read_csv("data/pce_items_lowest.csv")


months_change <- 12
compare_end <- as.Date("2023-11-01")
compare_start <- as.Date("2022-11-01")

quads <- get_quandrants(long_pce, months_change, compare_end, compare_start, lowest)

draw_quads(quads)
ggsave(paste0("graphics/change_ending_", year(compare_end), ".png"), dpi = "retina", width = 12.5, height = 8.5)

a <- create_chart(quads)
convertToDecimal <- function(percentage) {
  return(as.numeric(sub("%", "", percentage)) / 100)
}

months_change <- 12
quads_final <- list()

for (year in c(1976,2022)) {
  # Update the dates for each year
  compare_end <- as.Date(paste0(year, "-12-01"))
  compare_start <- as.Date(paste0(year - 1, "-12-01"))
  
  # Your existing code for data processing and plotting
  quads <- get_quandrants(long_pce, months_change, compare_end, compare_start, lowest) %>% mutate(Year = year)
  #draw_quads(quads)
  #ggsave(paste0("graphics/change_ending_", year, ".png"), dpi = "retina", width = 12.5, height = 8.5)

  # If you need to create additional charts
  a <- create_chart(quads)
  b <- convertToDecimal(a[3,4])

  quads_final <- rbind(quads_final, quads)
}

quads_final <- get_quandrants(long_pce, months_change, as.Date("2023-11-01"), as.Date("2022-11-01"), lowest) %>% mutate(Year = 2023) %>%
  rbind(quads_final) %>% filter(Year != 2022)

filter <- c("Goods", "Services")

quads_final %>%
  mutate(QuantityFinal = remove_outliers(QuantityFinal, 4)) %>%
  mutate(PriceFinal = remove_outliers(PriceFinal, 4)) %>%
  filter(category %in% filter) %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(QuantityFinal, PriceFinal, size = weight, color = Year)) +
  geom_point(show.legend = FALSE) + #alpha = 0.5, shape = 21, color = "black", stroke = 1.5, show.legend = FALSE) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(text = element_text(size = 22)) +
  facet_wrap(~Year) +
  labs(
    y = "Price (Inflation)",
    x = "Quantity",
    subtitle = "Change in 12-month change, inflation and quantity, 123 PCE categories.\n12-month change Nov 2023 compared to baseline 12-month change November 2022.",
    title = "80 Percent of Inflation Deceleration is Happening Where Supply is Increasing",
    caption = "NIPA tables 2.4.3U, 2.4.4U, 2.4.5U. Mike Konczal, Roosevelt Institute."
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  theme(strip.background = element_blank()) +
  annotate("text", x = -0.25, y = -0.15, label = "Lower Demand", hjust = 0, vjust = 0) +
  annotate("text", x = 0.25, y = -0.15, label = "Higher Supply", hjust = 0, vjust = 0)