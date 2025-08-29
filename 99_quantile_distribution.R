library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)
library(quantmod)


#saveRDS(long_pce, file = "data/long_pce_2024_01_22.rds")
fed_median_cat <- read_csv("data/median_pce_components.csv")

distribution_data <- pce %>%
  filter(SeriesLabel %in% fed_median_cat$Component,
         # Duplicate child care category in NIPA tables
         !(SeriesLabel == "Child care" & LineNo == 397),
         !is.na(PCEweight), !is.na(DataValue_P3), year(date) >= 1960) %>%
  group_by(date) %>%
  mutate(
    normalized = sum(PCEweight),
    weightN = PCEweight / normalized
  ) %>%
  arrange(DataValue_P3) %>%
  mutate(cumsumN = cumsum(weightN)) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(DataValue_P3a = (1 + DataValue_P3)^2 - 1)



get_distribution_quantile <- function(df, quantile) {
  quantiles <- df %>%
    arrange(date) %>%
    group_by(date) %>%
    filter(cumsumN >= quantile) %>%
    top_n(-1, DataValue_P3) %>%
    ungroup()

  return(quantiles)
}
# Inefficient - need way to group on an array of values. Workaround for now.
p50 <- get_distribution_quantile(distribution_data, 0.5) %>% mutate(percentile = "50th (median)")



p25 <- get_distribution_quantile(distribution_data, 0.25) %>% mutate(percentile = "25th")# %>% distinct(date, .keep_all = TRUE)
p75 <- get_distribution_quantile(distribution_data, 0.75) %>% mutate(percentile = "75th") #%>% distinct(date, .keep_all = TRUE)

p10 <- get_distribution_quantile(distribution_data, 0.1) %>% mutate(percentile = "10th") #%>% distinct(date, .keep_all = TRUE)
p20 <- get_distribution_quantile(distribution_data, 0.2) %>% mutate(percentile = "20th")# %>% distinct(date, .keep_all = TRUE)
p30 <- get_distribution_quantile(distribution_data, 0.3) %>% mutate(percentile = "30th") #%>% distinct(date, .keep_all = TRUE)
p40 <- get_distribution_quantile(distribution_data, 0.4) %>% mutate(percentile = "40th") #%>% distinct(date, .keep_all = TRUE)
p50 <- get_distribution_quantile(distribution_data, 0.5) %>% mutate(percentile = "50th") #%>% distinct(date, .keep_all = TRUE)

#rbind(p50, p25, p75) %>%
rbind(p10, p20, p30, p40, p50) %>%
  mutate(percentile = as.factor(percentile),
         percentile = fct_rev(percentile)) %>%
  filter(year(date) >= 2018) %>%
  ggplot(aes(date, DataValue_P3, color=percentile)) + geom_line(size=1.1) + theme_classic(base_size = 18) +
  labs(subtitle="Annualized 6-Month PCE Inflation by Quantiles, Weighted by Percent of Nominal Spending. ~200 PCE Categories (From Cleveland Fed's Median PCE)",
       caption = "Mike Konczal, Roosevelt Institute") +
  theme(legend.position = c(0.7,0.75), plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = as.Date("2025-01-01"))