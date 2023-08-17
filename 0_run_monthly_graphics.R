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


title_three_dashboard <- "Non-Housing Servies Finally Breaks"
three_twelve_graphic <- "Inflation Over the Past Three Months Have Finally Dropped"
title_overview <- "PCE Inflation Fell Last Month"
source("2_three_dashboard_graphics.R")


source("3_difference_with_cpi.R")
source("6_supercore.R")