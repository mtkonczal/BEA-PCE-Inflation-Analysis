library(tidyverse)
library(lubridate)
library(quantmod)
library(gt)

prep_FRED_data <- function(x) {
  getSymbols(x, src = "FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}

fill_in_values <- function(df, change_value, random_add = FALSE, random_array = tibble()) {
  for (i in seq(1:length(df))) {
    if (is.na(df[i])) {
      if(random_add == TRUE)
        df[i] <- lag(df, 1)[i] * (sample(random_array,1) + 1)
      else
        df[i] <- lag(df, 1)[i] * (change_value + 1)
    }
  }
  return(df)
}

calculate_diff <- function(df, lag_length = 6, annualize = TRUE) {
  df <- df / lag(df, lag_length)
  if (annualize) {
    df <- df^(12 / lag_length) - 1
  } else{
    df <- df - 1
  }
  return(df)
}

# setup data
df <- prep_FRED_data("PCEPILFE") %>%
  rename(pce = pcepilfe)
a <- ym(c("2022-09", "2023-12"))
r <- range(a, na.rm = TRUE)
a <- tibble(date = seq(r[1], r[2], "month"))
pce <- left_join(a, df, by = "date")


# Start here
# initialize empty frame
pce_chart <- tibble(length_variable = character(),
               annual = numeric())
for (i in c(1,3,6,12)) {
  x <- calculate_diff(pce$pce, i,annualize = FALSE)
  x <- tail(x[!is.na(x)], 1)
  x <- (1+x)^(1/i) - 1
  pce$tester <- fill_in_values(pce$pce, x)
  values <- calculate_diff(pce$tester, 12, annualize = TRUE)
  values6 <- calculate_diff(pce$tester, 6, annualize = TRUE)
  pce_chart <- rbind(pce_chart, tibble(date =paste0(i, "-month change"),
                                       value = (1+x)^12-1,
         second_half = tail(values6[!is.na(values6)],1),
         annual = tail(values[!is.na(values)],1)))
}

run_analysis <- function(length_of_sampling = 6, n_trials = 10000){
  
  # get six monthly changes
  random_pce_array <- na.omit(pce) %>% mutate(change = pce/lag(pce)-1) %>%
    filter(date >= max(date) %m-% months(length_of_sampling-1)) %>%
    filter(!is.na(change)) %>%
    select(change) %>% pull()
  
  #set.seed(1001)
  # Pre-allocate space for the results
  random_array <- tibble(second_half = numeric(n_trials), annual = numeric(n_trials))
  
  for (i in seq_len(n_trials)) {
    pce$tester <- fill_in_values(pce$pce, x, random_add = TRUE, random_array = random_pce_array)
    values <- calculate_diff(pce$tester, 12, annualize = TRUE)
    values6 <- calculate_diff(pce$tester, 6, annualize = TRUE)
    
    # Fill in the results directly without rbind
    random_array$second_half[i] <- tail(values6[!is.na(values6)], 1)
    random_array$annual[i] <- tail(values[!is.na(values)], 1)
  }
  
  
  random_table <- random_array %>%
    pivot_longer(second_half:annual, names_to = "names", values_to = "values") %>%
    group_by(names) %>%
    summarize(mean_value = mean(values),
             median_value = median(values),
             q20 = quantile(values, 0.20),
             q80 = quantile(values, 0.80)
             ) %>%
    mutate(names = if_else(names == "annual", "Year-Over-Year 2023", "Second Half, 2023"))
  
  
  
  # Calculate the required statistics
  random_array %>% select(second_half) %>%
    summarise_all(list(
      mean = ~mean(.),
      median = ~median(.),
      `20th_percentile` = ~quantile(., 0.2),
      `80th_percentile` = ~quantile(., 0.8)
    )) %>%
    pivot_longer(mean:`80th_percentile`, names_to = "types", values_to = "these")
    
  
  
  imean <- c("Mean", mean(random_array$annual),mean(random_array$second_half))
  imedian <- c("Median",median(random_array$annual),median(random_array$second_half))
  i20 <- c("20th Percentile",quantile(random_array$annual,0.2),quantile(random_array$second_half,0.2))
  i80 <- c("80th Percentile",quantile(random_array$annual,0.8),quantile(random_array$second_half,0.8))
  r_array <- as_tibble(rbind(imean,imedian,i20,i80))
  colnames(r_array) <- c("type","yoy","month6")
  
  r_array <- r_array %>% mutate(yoy = as.numeric(yoy),
                     month6 = as.numeric(month6)) %>%
    mutate(value = NA) %>%
    select(type, value, yoy, month6) %>%
    mutate(chart_type = paste0("Random Sampling From Last ", length_of_sampling, " Months:"))
  return(r_array)
}

# Transpose the result for the desired format

r_array <- run_analysis()
r_array2 <- run_analysis(length_of_sampling = 12)


pce_chart %>% select(type = date,
                     value,
                     yoy = annual,
                     month6 = second_half) %>%
  mutate(chart_type = "Last X Months Projected Forward:") %>%
  rbind(r_array) %>%
  rbind(r_array2) %>%
  gt(groupname_col = "chart_type") %>%
  tab_header(title=md("**What Will Core PCE Inflation Be at the End of 2023?**"),
  subtitle = "Assigning Q4 values based on previous months and randomly sampling last 6 and 12 months.") %>%
  cols_label(
    value = "Initial Value",
    yoy = html("Year-over-Year\n2023"),
    month6 = "2nd Half 2023",
    type = ""
  ) %>%
  tab_source_note(
    source_note = "Random sampling done at monthly level for both sampling and forecasting, 10K trials."
  ) %>%
  tab_source_note(
    source_note = "All values annualized. Preliminary. Mike Konczal, Roosevelt Institute"
  ) %>%
  fmt_percent(decimals = 1) %>%
  opt_stylize(style = 6, color = 'blue') %>%
  sub_missing(column = value, missing_text = "") %>%
  gtsave(., filename="graphic/projections_test.png")
