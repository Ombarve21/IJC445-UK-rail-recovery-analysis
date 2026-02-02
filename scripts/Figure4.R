# Section 1: Data Preparation for Comparative Analysis

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Load regional data
journeys_raw <- read_excel("data/clean_regional_data.xlsx", 
                           sheet = "1510_journeys_great_britain")

# Defining the Comparison Window
y_pre <- 2019
y_post <- 2024

sectors <- c("within_england", "within_scotland", "within_wales",
             "england_scotland", "england_wales", "scotland_wales")

recovery_data <- journeys_raw %>%
  # Filter for only the two years of interest
  filter(year %in% c(y_pre, y_post)) %>%
  select(year, all_of(sectors)) %>%
  
  # Reshape to long format for data cleaning
  pivot_longer(cols = all_of(sectors),
               names_to = "sector",
               values_to = "journeys_thousands") %>%
  
  # Cleaning and Unit Conversion
  mutate(
    sector = tools::toTitleCase(gsub("_", " ", sector)),
    journeys_m = journeys_thousands / 1000
  ) %>%
  select(year, sector, journeys_m) %>%
  
  # Reshape to WIde Format for "Dumbbell" Calculation
  pivot_wider(names_from = year, values_from = journeys_m) %>%
  rename(Pre =!!as.character(y_pre),
         Post =!!as.character(y_post)) %>%
  
  filter(!is.na(Pre), !is.na(Post), Pre > 0, Post > 0) %>%
  
  mutate(
    pct_change = (Post - Pre) / Pre,
    gap_label = paste0(ifelse(pct_change >= 0, "+", ""), round(pct_change * 100, 1), "%"),
    mid_log = exp((log(Pre) + log(Post)) / 2)
  )

recovery_data$sector <- factor(recovery_data$sector, levels = c(
  "Within England", "Within Scotland", "Within Wales",
  "England Scotland", "England Wales", "Scotland Wales"
))

# Section 2: Dumbbell Chart Visualisation
fig4 <- ggplot(recovery_data) +

# 1. The Bar
geom_segment(aes(y = sector, yend = sector, x = Pre, xend = Post),
             colour = "grey65", linewidth = 1.1) +
  
  # 2. Points
  geom_point(aes(x  =Pre, y = sector), size = 4.5, colour = "grey70") +
  geom_point(aes(x = Post, y = sector), size = 4.5, colour = "#d95f02") +
  
  # 3. Contextual Labels
  geom_text(aes(x = mid_log, y = sector, label = gap_label),
            vjust = -1.2, size = 3.5, fontface = "bold", colour = "#d95f02") +
  geom_text(aes(x =Pre, y =sector, label = y_pre),
            vjust = 2.2, size = 3, colour = "grey55") +
  geom_text(aes(x = Post, y = sector, label = y_post),
            vjust = 2.2, size = 3, colour = "#d95f02") +
  
  # 4. Logarithmic Scale
  scale_x_log10(
    labels = label_number(suffix = "m", accuracy = 0.1),
    breaks = c(0.1,1,10,100,1000)
  ) +
  
  # 5. Annotation and Theme
  labs(
    title = paste0("The Recovery Gap: Rail Journeys (", y_pre, " vs ", y_post, ")"),
    subtitle = "Figure 4: Dumbbell chart highlighting sector-level change from pre-pandemic levels",
    x = "Passenger journeys (millions, log scale)",
    y = "",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face ="bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),     # Remove horizontal grid lines
    panel.grid.minor.x = element_blank()
  )

# Render plot
fig4