# Load libraries ---------------------------------------------------------------
library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)
library(quantmod)
library(gt)

# Items to check
check_items <- c(
  "PCE services excluding energy",
  "PCE goods excluding food and energy",
  "PCE food and energy",
  "Housing",
  "Personal consumption expenditures"
)

# Calculate changes ------------------------------------------------------------
# 'pce' is assumed to be your existing data frame with columns:
#   date, DataValue, PCEweight, LineDescription

a <- pce %>%
  filter(LineDescription %in% check_items) %>%
  select(date, DataValue, PCEweight, LineDescription) %>%
  group_by(LineDescription) %>%
  arrange(date) %>%
  mutate(
    # 1-month change (annualized)
    DataValue_P1 = (DataValue / lag(DataValue, 1)) - 1,
    WDataValue_P1 = DataValue_P1 * lag(PCEweight, 1),
    # Annualize 1-month by raising to the 12th
    WDataValue_P1a = (1 + WDataValue_P1)^12 - 1,
    
    # 3-month change (annualized)
    DataValue_P3 = (DataValue / lag(DataValue, 3)) - 1,
    WDataValue_P3 = DataValue_P3 * lag(PCEweight, 1),
    WDataValue_P3a = (1 + WDataValue_P3)^4 - 1,
    
    # 6-month change (annualized)
    DataValue_P6 = (DataValue / lag(DataValue, 6)) - 1,
    WDataValue_P6 = DataValue_P6 * lag(PCEweight, 1),
    WDataValue_P6a = (1 + WDataValue_P6)^2 - 1,
    
    # 12-month change (yoy)
    DataValue_P12 = (DataValue / lag(DataValue, 12)) - 1,
    WDataValue_P12 = DataValue_P12 * lag(PCEweight, 1),
    # If you want to keep pure yoy (not re-annualizing a yoy),
    # you can just leave WDataValue_P12 as-is. Or you could do:
    # WDataValue_P12a = (1 + WDataValue_P12)^1 - 1, which is basically the same.
    #
    # Historical anchors: 2018–2019 and 2022 yoy
    before = (DataValue[date == "2019-12-01"] /
                DataValue[date == "2017-12-01"]) - 1,
    Wbefore = before * PCEweight[date == "2019-11-01"],
    Wbeforea = (1 + Wbefore)^(0.5) - 1,
    
    y2022  = (DataValue[date == "2022-12-01"] /
                DataValue[date == "2021-12-01"]) - 1,
    Wy2022a = y2022 * PCEweight[date == "2022-11-01"]
  ) %>%
  # Keep only the most recent observation for each category
  filter(date == max(date)) %>%
  ungroup()

# Create "Core Non-Housing Services" by taking difference -----------------------
# We'll combine "PCE services excluding energy" minus "Housing"
b <- a %>%
  filter(LineDescription %in% c("PCE services excluding energy", "Housing")) %>%
  select(
    LineDescription,
    WDataValue_P1,  WDataValue_P3,  WDataValue_P6,  WDataValue_P12,
    Wbefore,        Wy2022a,
    WDataValue_P1a, WDataValue_P3a, WDataValue_P6a, Wbeforea
  ) %>%
  pivot_wider(
    names_from = LineDescription,
    values_from = c(WDataValue_P1,  WDataValue_P3,  WDataValue_P6,  WDataValue_P12,
                    Wbefore,        Wy2022a,
                    WDataValue_P1a, WDataValue_P3a, WDataValue_P6a, Wbeforea)
  ) %>%
  clean_names() %>%
  summarize(
    WDataValue_P1  = w_data_value_p1_pce_services_excluding_energy - w_data_value_p1_housing,
    WDataValue_P3  = w_data_value_p3_pce_services_excluding_energy - w_data_value_p3_housing,
    WDataValue_P6  = w_data_value_p6_pce_services_excluding_energy - w_data_value_p6_housing,
    WDataValue_P12 = w_data_value_p12_pce_services_excluding_energy - w_data_value_p12_housing,
    Wbefore        = wbefore_pce_services_excluding_energy         - wbefore_housing,
    Wy2022a        = wy2022a_pce_services_excluding_energy         - wy2022a_housing
  ) %>%
  # Re-annualize the differences for 1-, 3-, and 6-month, etc.
  mutate(
    WDataValue_P1a = (1 + WDataValue_P1)^12 - 1,
    WDataValue_P3a = (1 + WDataValue_P3)^4 - 1,
    WDataValue_P6a = (1 + WDataValue_P6)^2 - 1,
    Wbeforea       = (1 + Wbefore)^(0.5) - 1,
    LineDescription = "4 - Core Non-Housing Services"
  ) %>%
  relocate(LineDescription)

# Build the final table --------------------------------------------------------
table_data <- a %>%
  select(
    LineDescription,
    WDataValue_P1,  WDataValue_P3,  WDataValue_P6,  WDataValue_P12,
    Wbefore,        Wy2022a,
    WDataValue_P1a, WDataValue_P3a, WDataValue_P6a, Wbeforea
  ) %>%
  # Add the "Core Non-Housing Services" row
  rbind(b) %>%
  # For the GT table, choose whichever columns you want to display:
  select(LineDescription,
         WDataValue_P1a,  # 1-month annualized
         WDataValue_P3a,  # 3-month annualized
         WDataValue_P6a,  # 6-month annualized
         WDataValue_P12,  # 12-month yoy
         Wbeforea         # 2018-2019 anchor, half-year annualized
  ) %>%
  # Remove the baseline services row so it doesn't duplicate
  filter(LineDescription != "PCE services excluding energy") %>%
  # Rename line descriptions
  mutate(LineDescription = case_when(
    LineDescription == "Personal consumption expenditures"    ~ "1 - Total PCE Inflation",
    LineDescription == "PCE food and energy"                  ~ "2 - Food and Energy",
    LineDescription == "PCE goods excluding food and energy"  ~ "3 - Core Goods",
    LineDescription == "Housing"                              ~ "5 - Housing",
    TRUE                                                     ~ LineDescription
  )) %>%
  # Remove numeric prefix from newly-coded description for final print
  arrange(LineDescription) %>%
  mutate(LineDescription = substr(LineDescription, 5, nchar(LineDescription))) %>%
  ungroup()

# Dynamically label the table date in the header
chart_date <- format(max(pce$date, na.rm = TRUE), "%B %Y")

# Build the GT table ----------------------------------------------------------
table_data %>%
  gt(rowname_col = "LineDescription") %>%
  tab_header(
    title = md(paste0("**Breakdown of Weighted Contribution to ", chart_date, " PCE Inflation**")),
    subtitle = "All data annualized"
  ) %>%
  cols_label(
    WDataValue_P1a  = "Past 1 Month",
    WDataValue_P3a  = "Past 3 Months",
    WDataValue_P6a  = "Past 6 Months",
    WDataValue_P12  = "Past 12 Months",
    Wbeforea        = "2018-2019"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  fmt_percent(
    columns = c(
      WDataValue_P1a,
      WDataValue_P3a,
      WDataValue_P6a,
      WDataValue_P12,
      Wbeforea
    )
  ) %>%
  tab_footnote(
    footnote = "Source: [Your Data Source]",
    locations = cells_column_labels(columns = vars(LineDescription))
  ) %>%
  opt_stylize(style = 6, color = "blue") %>%
  tab_source_note(
    source_note = "Total may differ by ~0.2% due to rounding. BEA, Author's Analysis. Mike Konczal."
  ) %>%
  tab_style(
    style = list(
      cell_borders(sides = "bottom", color = "black", weight = px(2))
    ),
    locations = cells_body(rows = 1)
  ) %>%
  gtsave(filename = "graphics/old_inflation_chart.png")
