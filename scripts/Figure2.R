# Load core libraries for data manipulation and formatting
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Load the cleaned regional dataset
journeys_raw <- read_excel("data/clean_regional_data.xlsx", sheet = "1510_journeys_great_britain")

# Isolate the target year to create a cross-sectional snapshot
target_year <- 2024
row_yr <- journeys_raw %>% filter(year == target_year)

# Constructing the OD (Origin-Destination) Matrix
od_data <- tibble(
# Factor ordering
  Origin = factor(
    c("England", "England", "England",
      "Scotland", "Scotland", "Scotland",
      "Wales", "Wales", "Wales"),
    levels = c("Wales", "Scotland", "England") # Y-axis order
  ),
  
  Destination = factor(
    c("England", "Scotland", "Wales",
      "England", "Scotland", "Wales",
      "England", "Scotland", "Wales"),
    levels = c("England", "Scotland", "Wales") # X-axis order
  ),
  
# Mapping Flows 
  Journeys_m = c(
  row_yr$within_england/1000,     row_yr$england_scotland/1000, row_yr$england_wales/1000,
  row_yr$england_scotland/1000,   row_yr$within_scotland/1000,  row_yr$scotland_wales/1000,
  row_yr$england_wales/1000,      row_yr$scotland_wales/1000,   row_yr$within_wales/1000
  )
) %>%
  mutate(
    # Logarithmic adjustments
    Journeys_adj = Journeys_m + 0.001,
    
    # Accessibility
    label_col = ifelse(Journeys_m < 1, "white", "black"),
    
    # Adaptive Formatting
    label_txt  = ifelse(Journeys_m >= 1,
                        comma(Journeys_m, accuracy = 1),
                        comma(Journeys_m, accuracy = 0.01))
  )

# SECTION 2: Heatmap Visualisation
fig2 <- ggplot(od_data, aes(x = Destination, y = Origin, fill = Journeys_adj)) +
  
  # 1. Title Geometry
  geom_tile(color = "white", linewidth = 1) +
  
  # 2. Data Labels
  geom_text(aes(label= label_txt, colour = label_col),
            size = 5, fontface = "bold", show.legend = FALSE) +
  
  #Apply the computed contrast colours
  scale_colour_identity() +
  
  # 3. Colour Scale
  scale_fill_viridis_c(
    option = "mako",
    trans = "log10",
    breaks = c(0.1,1,10,100,1000),
    labels = function(x) paste0(comma(x), "m"),
    name = "Journeys (M)"
  ) +
  
  # Ensure squares remain square regardless of window size
  coord_fixed() +
  
  # 4. Annotation
  labs(
    title = paste0("Origin-Destination Matrix (", target_year, ")"),
    subtitle = "Log-scale heatmap highlighting an England-centric network structure",
    x = "Destination",
    y = "Origin"
  ) + 
  annotate("text", x =1.5, y = 3.6,
           label = "Diagonal = within-country journeys",
           size = 3.5, colour = "grey30") +
  
  # 5. Clean Theme
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Render plot
  fig2

















  