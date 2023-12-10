library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)
library(quantmod)


source("1a_helper_functions.R")
source("load_flatfiles.R")
source("1b_load_PCE_items.R")


title_three_dashboard <- "I Love it When a Plan Comes Together: Goods Zero, Non-Housing Services Slowing"
three_twelve_graphic <- "Inflation Over the Past Three Months Have Finally Dropped"
title_overview <- "PCE Inflation Fell Last Month"
source("2_three_dashboard_graphics.R")
source("3_nhs_supercore.R")
source("5_market_versus_imputed.R")


long_pce <- load_pce_data()
long_core_pce <- long_pce %>% filter(series_label == "PCE excluding food and energy")
source("4_monthly_phillips_curve_charts.R")
source("7_long_graphic.R")
long_pce <- pce  %>% filter(LineDescription == "PCE excluding food and energy")
pce_versus_unrate(long_core_pce, "Accelerationist This! Current Disinflation Alongside Low Unemployment is Unprecedented in the Past 60 Years")
ggsave("graphics/long_inflation2.png", dpi = "retina", width = 12.5, height = 8.5)