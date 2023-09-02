library(vars)
library(quantmod)
library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(hrbrthemes)
library(janitor)

setwd("/Users/mkonczal/Documents/GitHub/BEA-PCE-Inflation-Analysis/")
beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
beaKey <- as.character(beaKey)
source("1a_helper_functions.R")
#source("1b_load_PCE_items_all.R")
#source("1b_load_PCE_items.R")
load("data/pce_long.RData")
# Table IDs
# https://www.bea.gov/system/files/2021-07/TablesRegisterPreview.txt

supply_chains <- read_csv("data/supply_chains.csv") %>% select(-date_full)
u_v <- read_csv("data/u_v.csv") %>% rename(date = DATE, v_u = LNU03000000_JTUJOL) %>% mutate(v_u = 1/v_u)

q <- pce %>% filter(LineDescription %in% c("PCE goods excluding food and energy","PCE services excluding energy")) %>%
  select(date, LineDescription, Quantity) %>%
  pivot_wider(names_from = LineDescription, values_from = Quantity) %>%
  clean_names() %>%
  rename(goods_quantity = pce_goods_excluding_food_and_energy, services_quantity = pce_services_excluding_energy) %>%
  mutate(goods_quantity = goods_quantity/lag(goods_quantity,1)-1, services_quantity = services_quantity/lag(services_quantity,1)-1)

df <- pce %>% filter(LineDescription %in% c("PCE goods excluding food and energy","PCE services excluding energy")) %>%
  select(date, LineDescription, DataValue) %>%
  pivot_wider(names_from = LineDescription, values_from = DataValue) %>%
  clean_names() %>%
  rename(goods_inflation = pce_goods_excluding_food_and_energy, services_inflation = pce_services_excluding_energy) %>%
  mutate(goods_inflation = goods_inflation/lag(goods_inflation,1)-1, services_inflation = services_inflation/lag(services_inflation,1)-1) %>%
  left_join(q, by="date") %>%
  left_join(supply_chains, by="date") %>%
  left_join(u_v, by="date") %>%
  na.omit()


df <- df %>% filter(year(date) <)
#df <- df %>% filter(year(date)<2020)
ts_data <- ts(df[, c("goods_inflation", "services_inflation","goods_quantity","services_quantity","supply_chains", "v_u")], start=c(year(min(df$date)), month(min(df$date))), frequency=12)

VARselect(ts_data)
var_model <- VAR(ts_data, p=10)
summary(var_model)

irf_results <- irf(var_model, impulse="supply_chains", response=c("goods_inflation", "services_inflation"), n.ahead=48, boot=TRUE, runs=100)
plot(irf_results)

irf_results <- irf(var_model, impulse="goods_quantity", response=c("supply_chains"), n.ahead=48, boot=TRUE, runs=100)
plot(irf_results)