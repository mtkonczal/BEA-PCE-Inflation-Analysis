# ── Libraries ───────────────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(janitor)
library(gt)

# ── Config (edit these) ─────────────────────────────────────────────────────────
CHECK_ITEMS <- c(
  "PCE services excluding energy",
  "PCE goods excluding food and energy",
  "PCE food and energy",
  "Housing",
  "Personal consumption expenditures"
)

# Optional mapping to pretty names + ordering
RENAME_ORDER <- c(
  "Personal consumption expenditures"   = "1 - Total PCE Inflation",
  "PCE food and energy"                 = "2 - Food and Energy",
  "PCE goods excluding food and energy" = "3 - Core Goods",
  "Housing"                             = "5 - Housing"
)

# ── Helper: safe lagged weight (use t-1 weight where possible) ──────────────────
lagged_weight <- function(w, n = 1) {
  out <- dplyr::lag(w, n)
  dplyr::coalesce(out, w)
}

# ── Helper: integer months between two month-start dates (no days) ──────────────
months_between <- function(start_date, end_date) {
  12L * (lubridate::year(end_date) - lubridate::year(start_date)) +
    (lubridate::month(end_date) - lubridate::month(start_date))
}

# ── Helper: compute weighted contribution for a rolling k-month window ─────────
period_contrib <- function(x, k_months, annualize = FALSE) {
  lvl_now  <- x$DataValue
  lvl_then <- dplyr::lag(x$DataValue, k_months)
  raw_rate <- (lvl_now / lvl_then) - 1
  rate <- if (annualize) (1 + raw_rate)^(12 / k_months) - 1 else raw_rate
  w_tm1 <- lagged_weight(x$PCEweight, 1)
  rate * w_tm1
}

# ── Helper: fixed-date pair contribution (t1 vs t0), month-based annualizing ───
fixed_pair_contrib <- function(df_g, start_date, end_date, annualize = FALSE) {
  v1 <- df_g$DataValue[df_g$date == end_date]
  v0 <- df_g$DataValue[df_g$date == start_date]
  if (length(v1) == 0 || length(v0) == 0) return(NA_real_)
  raw_rate <- (v1 / v0) - 1
  
  if (annualize) {
    m_span <- months_between(start_date, end_date)
    if (m_span <= 0) return(NA_real_)
    rate <- (1 + raw_rate)^(12 / m_span) - 1
  } else {
    rate <- raw_rate
  }
  
  # use t-1 weight at end_date when available; else end_date weight
  w_end  <- df_g$PCEweight[df_g$date == end_date]
  w_lag1 <- df_g$PCEweight[df_g$date == (end_date %m-% months(1))]
  w_use  <- ifelse(length(w_lag1) == 1, w_lag1, w_end)
  if (length(w_use) == 0) return(NA_real_)
  rate * w_use
}

# ── Helper: “since {start} to latest” using month-based annualizing ────────────
since_date_contrib <- function(df_g, start_date, annualize = FALSE) {
  end_date <- max(df_g$date, na.rm = TRUE)
  fixed_pair_contrib(df_g, start_date, end_date, annualize = annualize)
}

# ── Core: compute all requested columns for each category ───────────────────────
compute_contribs <- function(pce, categories = CHECK_ITEMS) {
  p <- pce %>%
    filter(LineDescription %in% categories) %>%
    select(date, LineDescription, DataValue, PCEweight) %>%
    arrange(LineDescription, date)
  
  rolls <- p %>%
    group_by(LineDescription) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      m1_ann = period_contrib(pick(everything()), k_months = 1, annualize = TRUE),
      m3_ann = period_contrib(pick(everything()), k_months = 3, annualize = TRUE)
    ) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    select(LineDescription, m1_ann, m3_ann)
  
  anchors <- p %>%
    group_by(LineDescription) %>%
    summarize(
      # Since Dec 2024 → latest, ANNUALIZED (month-count based)
      since_d  = since_date_contrib(pick(everything()), as.Date("2024-12-01"), annualize = TRUE),
      
      # Dec '23 → Dec '24 (12-month span): leave as raw Dec/Dec rate
      yoy_2024 = fixed_pair_contrib(pick(everything()), as.Date("2023-12-01"), as.Date("2024-12-01"), annualize = FALSE),
      
      # Dec '17 → Dec '19 (24 months), ANNUALIZED (month-count based)
      a_1819   = fixed_pair_contrib(pick(everything()), as.Date("2017-12-01"), as.Date("2019-12-01"), annualize = TRUE),
      .groups = "drop"
    )
  
  out <- rolls %>% left_join(anchors, by = "LineDescription")
  
  out %>%
    mutate(LineDescription = dplyr::recode(LineDescription, !!!RENAME_ORDER)) %>%
    arrange(LineDescription)
}

# ── Core Non-Housing Services = services ex energy minus housing ────────────────
make_core_non_housing_services <- function(pce) {
  parts <- compute_contribs(
    pce,
    categories = c("PCE services excluding energy", "Housing")
  ) %>%
    mutate(key = sub("^\\d+\\s*[-–]\\s*", "", LineDescription))  # strip any "5 - " prefix
  
  row_services <- parts %>% filter(str_detect(key, "(?i)^PCE services excluding energy$"))
  row_housing  <- parts %>% filter(str_detect(key, "(?i)^Housing$"))
  
  if (nrow(row_services) != 1 || nrow(row_housing) != 1) return(tibble())
  
  measure_cols <- c("m1_ann","m3_ann","since_d","yoy_2024","a_1819")
  
  diffs <- tibble(
    LineDescription = "4 - Core Non-Housing Services"
  ) |>
    bind_cols(as_tibble(purrr::map_dfc(measure_cols, ~ row_services[[.x]] - row_housing[[.x]])) |>
                set_names(measure_cols))
  
  diffs
}

# ── Assemble final table (adds Core NHS, drops baseline services row) ───────────
assemble_table <- function(pce) {
  base <- compute_contribs(pce, categories = CHECK_ITEMS)
  core_nhs <- make_core_non_housing_services(pce)
  
  # Remove the baseline services-ex-energy row (now represented by Core NHS)
  cleaned <- base %>%
    filter(!str_detect(LineDescription, "(?i)services excluding energy"))
  
  final <- bind_rows(cleaned, core_nhs) %>%
    arrange(LineDescription) %>%
    mutate(LineDescription = sub("^\\d+\\s*[-–]\\s*", "", LineDescription)) %>%
    rename(
      `Past 1 Month`  = m1_ann,
      `Past 3 Months` = m3_ann,
      `2025 (So far)` = since_d,   # ANNUALIZED since Dec 2024
      `2024`          = yoy_2024,  # Dec/Dec raw
      `2018-2019`     = a_1819     # ANNUALIZED over 24 months
    )
  
  final
}

# ── Render GT table and save ────────────────────────────────────────────────────
render_gt <- function(tbl, pce, file = "graphics/inflation_chart.png") {
  chart_date <- format(max(pce$date, na.rm = TRUE), "%B %Y")
  
  tbl %>%
    gt(rowname_col = "LineDescription") %>%
    tab_header(
      title    = md(paste0("**Breakdown of Weighted Contribution to ", chart_date, " PCE Inflation**")),
      subtitle = "All rates are annualized. Weights approximated as nominal consumption shares."
    ) %>%
    cols_align(align = "center", columns = everything()) %>%
    fmt_percent(
      columns = c(
        "Past 1 Month",
        "Past 3 Months",
        "2025 (So far)",
        "2024",
        "2018-2019"
      ),
      decimals = 2
    ) %>%
    tab_source_note("Totals may differ slightly due to rounding. BEA, Author’s analysis. Mike Konczal.") %>%
    opt_stylize(style = 6, color = "blue") %>%
    gtsave(filename = file)
}

# ── Driver (one call) ───────────────────────────────────────────────────────────
build_pce_table <- function(pce,
                            categories = CHECK_ITEMS,
                            out_file = "graphics/inflation_chart.png") {
  tbl <- assemble_table(pce)
  render_gt(tbl, pce, file = out_file)
  invisible(tbl)
}

# ── EXAMPLE (uncomment to run) ─────────────────────────────────────────────────
tbl <- build_pce_table(pce)
# View(tbl)
