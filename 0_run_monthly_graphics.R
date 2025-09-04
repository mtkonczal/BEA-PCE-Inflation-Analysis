library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(janitor)
library(ggrepel)
library(quantmod)
library(tidyusmacro)

nipa <- getNIPAFiles(type = "M")

#write_rds(nipa, "nipa.rds")

pce <- getPCEInflation(frequency = "M", NIPA_data = nipa) %>%
  mutate(LineDescription = SeriesLabel, DataValue = Value)
max(pce$date)



title_three_dashboard <- "I Love it When a Plan Comes Together: Goods Zero, Non-Housing Services Slowing"
three_twelve_graphic <- "Inflation Over the Past Three Months Have Finally Dropped"
title_overview <- "PCE Inflation Fell Last Month"
source("1_three_dashboard_graphics.R")
source("2_pce_spending.R")
source("3_nhs_supercore.R")
source("4_inflation_chart.R")

source("5_supply_demand_charts.R")
source("6_shapiro_cyclical.R")