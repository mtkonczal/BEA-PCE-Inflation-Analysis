# This is code for loading data and preparing analysis for Phillips Curve blog post.
# Mike Konczal
# August 15th, 2023


# Install and load required packages
library(quantmod)
library(tidyverse)
library(lubridate)
library(latex2exp)
library(readxl)
library(zoo)
library(ggrepel)

#### SECTION 1: Preparing Data ####
# COMMENT ME OUT TOMORROW
#long_pce <- prep_FRED_data("PCEPILFE") %>% rename(pce_index = pcepilfe)

#long_pce_pc <- long_pce %>%
#  select(date, pce_index = DataValue)


# Function for downloading data from FRED
prep_FRED_data <- function(x) {
  getSymbols(x, src="FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}

# List of variables to download
fred_variables <- c("UNRATE","NROU","PCEPILFE")

# Download process, doing some manipulations so the characters become variable names
for(i in fred_variables){
  prep_FRED_data(i)
  data <- prep_FRED_data(i)
  assign(tolower(i), data, envir = .GlobalEnv)
  rm(data)
}

pc_data <- get(tolower(fred_variables[1]))
# Joining them all into one dataset. This dataset is monthly, with quarterly values missing dates as NA.
for(i in fred_variables[-1]) {
  pc_data <- full_join(pc_data, get(tolower(i)), by="date")
}


long_exp <- read_delim("data/LONGBASE.TXT") %>%
  select(OBS, PTR) %>%
  mutate(year = as.numeric(substr(OBS, 1, 4)), quarter = as.numeric(substr(OBS, 6, 6))) %>%
  mutate(month = quarter*2+quarter-2) %>%
  mutate(date = as.Date(paste(year, month,1, sep = "-"), "%Y-%m-%d")) %>%
  select(date, FRB_exp = PTR) %>%
  filter(year(date) > 1970)


pc_data <- pc_data %>% rename(pce_index = pcepilfe)
pc_analysis <- pc_data %>%
  left_join(long_exp, by="date") %>%
  #left_join(long_pce, by="date") %>%
  mutate(
    core_pce_changeA = (pce_index/lag(pce_index,3))^4 - 1,
    core_pce_changeA = core_pce_changeA*100,
  ) %>%
  filter(!is.na(core_pce_changeA))


# For some quarterly data, we just go ahead and fill in the gaps with the previous version.
# Future updates TKTK might extrapolate in-between, not sure best process.
# Also there must be a way to not do this in a for loop but we're just going to move right along.
pc_analysis$nrou <- na.locf(pc_analysis$nrou, na.rm = FALSE)
pc_analysis$FRB_exp <- na.locf(pc_analysis$FRB_exp, na.rm = FALSE)

pc_analysis$unrate_slack = pc_analysis$unrate - pc_analysis$nrou

# Get the 4 quarters that move off the maximum date.
start_month <- month(max(pc_analysis$date))
quarters <- ((seq(start_month, start_month + 9, by=3) - 1) %% 12) + 1


pc_analysis <- pc_analysis %>% filter(month(date) %in% quarters) %>%
  mutate(FRB_post1991 = year(date)>=1992,
         FRB_post1991 = if_else(FRB_post1991,FRB_exp, as.numeric(NA)))

# Create lagged variables so they aren't cut off when we start in 1992.
pc_analysis <- pc_analysis %>%
  mutate(lagged_1 = lag(core_pce_changeA, 1),
         lagged_2 = lag(core_pce_changeA, 2))

#### Section 2: Phillips Curve ####
# Run regression: PCE_Core_Inflation ~ lagged PCE_Core_Inflation + Expected_Inflation + Noncyclical_Unemployment
pc_analysis_1991 <- pc_analysis %>% filter(year(date)>=1992, year(date)<=2019)
model <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp +
              unrate_slack, data = pc_analysis_1991)
summary(model)

model2 <- lm(core_pce_changeA ~ lagged_1 + lagged_2 + FRB_exp +
               unrate_slack, data = pc_analysis[pc_analysis$date<"2020-01-01" & pc_analysis$date>="1970-01-01",])
summary(model2)

pc_analysis$predicted_1980_2019 <- predict(model, newdata = pc_analysis)
pc_analysis$predicted_1970_2019 <- predict(model2, newdata = pc_analysis)

date_breaks <- sort(unique(pc_analysis$date), decreasing = TRUE)
date_breaks <- date_breaks[seq(1, length(date_breaks), 24)]

pc_analysis %>% filter(year(date)>2010) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = core_pce_changeA, color = "Actual Inflation"), size=1.2) +
  geom_line(aes(y = predicted_1980_2019, color = "Predicted Inflation, 1991- training data")) +
  geom_line(aes(y = predicted_1970_2019, color = "Predicted Inflation, 1970- training data")) +
  labs(title = "Actual vs. Predicted PCE Core Inflation on Federal Reserve's Phillips Curve Specification", y = "Inflation Rate", x = "Date",
       subtitle=TeX(r"(Predicted is trained on 1991-2019 (red), 1970-2019 (purple), quarterly backwards from October 2023, of $\pi_t = \pi^{e}_t + \pi_{t-1} + \pi_{t-2} + (u - u^*)$)"),
       caption="Expected inflation is: Philly Fed SPF for 1991-; FRB/US data for 1970-. U-star from CBO. Author's Calculations, Mike Konczal, Roosevelt Institute.") +
  scale_color_manual(values = c("Actual Inflation" = "blue", "Predicted Inflation, 1991- training data" = "red","Predicted Inflation, 1970- training data" = "purple")) +
  theme_classic() +
  theme(legend.position = c(0.5,0.8), plot.title.position = "plot") +
  scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks)

ggsave("graphics/pc_model.png", dpi="retina", width = 12, height=6.75, units = "in")
