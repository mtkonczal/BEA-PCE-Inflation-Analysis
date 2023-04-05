library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)

setwd("/Users/mkonczal/Documents/GitHub/BEA-PCE-Inflation-Analysis/")
beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
beaKey <- as.character(beaKey)

source("1a_helper_functions.R")
source("1b_load_PCE_items.R")


title_three_dashboard <- "PCE Stumble: Goods Inflating, Services Not Slowing"
three_twelve_graphic <- "Inflation Over the Past Three Months Have Finally Dropped"
title_overview <- "Inflation Is Moving Sideways"
source("2_three_dashboard_graphics.R")


source("3_difference_with_cpi.R")
source("6_supercore.R")