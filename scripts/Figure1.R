# SECTION 1: Environment Setup & Data Preparation

# Load core libraries for data manipulation and formatting
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Load the cleaned regional dataset
journeys_raw <- read_excel("data/clean_regional_data.xlsx", sheet ="1510_journeys_great_britain")

# Data Transformation

fig1_data <- journeys_raw %>%
# Restrict analysis to 2015-2024 to focus on pre and post COVID trends
  filter(year>=2015, year<=2024) %>%
  
# Select only specific internal and cross-border flows required for this visualisation
  select(
    year,
    within_england, within_scotland, within_wales,
    england_scotland, england_wales, scotland_wales
  ) %>%
  
  pivot_longer(
    cols = -year,
    names_to = "sector",
    values_to = "journeys"
  ) %>%
  
# Feature engineering and Cleaning
  mutate(
    journeys_m = journeys / 1000,
    sector = gsub("_"," ",sector),
    sector = tools::toTitleCase(sector)
  )

# Enforce logical ordering of sectors for faceting
fig1_data$sector <- factor(fig1_data$sector, levels = c(
  "Within England", "Within Scotland", "Within Wales",
  "England Scotland", "England Wales", "Scotland Wales"
))

# SECTION 2: Visualisation Construction

fig1 <- ggplot(fig1_data, aes(x = year, y = journeys_m, colour = sector)) +
  
# COVID-19 structural break marker
  geom_vline(xintercept = 2020,
             linetype = "dashed",
             linewidth = 0.6,
             colour = "grey50") +
  
# Trend Representation
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  
# Handling Scale Disparity
  facet_wrap(~sector, scales = "free_y", ncol = 3) +
  scale_color_brewer(palette = "Dark2") +
  
# Labels
  labs(
    title = "Passenger Recovery Trends by Sector (2015-2024)",
    subtitle = "Comparison of Internal Networks vs. Cross-border Flows",
    x = "Year",
    y = "Passenger Journeys (Millions)"
  ) +

# Thematic styling
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  ) +

# Formatting
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks =seq(2015, 2024, 2))
  
# Render plot
  fig1
  
  
  
  
  
  
  
  
  
  
  