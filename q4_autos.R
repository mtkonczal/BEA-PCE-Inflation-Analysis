library(janitor)
library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)
library(viridis)
library(forcats)
library(govMacroTools)


# Generate 100 dates, starting from the maximum date in the input vector and going back X months at a time
generate_dates <- function(dates, X) {
  max_date <- max(dates)
  generated_dates <- seq(max_date, length.out = 100, by = paste0("-", X, " months"))
  return(generated_dates)
}


mnipa <- govMacroTools::getNIPAFiles(type = "M") %>%
  clean_names()


autos <- mnipa %>% filter(table_id == "U70205S", line_no == 4)

#First Graphic - Total Auto Sales
autos %>%
  filter(year(date) >= 2018) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "#005A9C", size = 1.2) +  # Using blue from Set1
  labs(
    title    = "An Extra 188,500 Total Cars Sold Anticipating Trump's Tariffs?",
    subtitle = "Total Vehicle Sales, Millions of Units, Monthly, Seasonally Adjusted Annual Rate.",
    x        = "",
    y        = "",
    caption  = "BEA, Supplemental Motor vehicles. Mike Konczal"
  ) +
  scale_x_date(
    breaks = generate_dates(autos$date, 12),
    date_labels = "%b %Y"
  ) +
  theme_classic(base_size = 9) +
  theme(
    plot.title          = element_text(face = "bold", size = 17),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title.position = "plot"
  )
ggsave("tester1.png", width = 8, height = 4, units = "in", dpi = "retina")


