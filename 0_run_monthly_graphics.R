library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)


source("1a_helper_functions.R")
source("1b_load_PCE_items.R")


title_three_dashboard <- "Goods at Zero, Services Inflation Slowing"
three_twelve_graphic <- "Inflation Over the Past Three Months Have Finally Dropped"
title_overview <- "PCE Inflation Fell Last Month"
source("2_three_dashboard_graphics.R")
source("3_nhs_supercore.R")
source("4_monthly_phillips_curve_charts.R")
source("5_market_versus_imputed.R")