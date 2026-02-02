# Section 1: Data Preparation & Feature Engineering

# Load core libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Load the cleaned fares dataset
fares<- read_excel("data/rail_fares.xlsx", sheet = "clean_fares")

# Calculating Year-on-Year Volatility
fares_15_24 <- fares %>% 
  
  # Restrict analysis window 
  filter(year >= 2015, year <= 2024) %>%
  arrange(year) %>% 
  mutate(
    # 1. Rebase the index to 2015 = 0%
    base_2015 = fare_change[year == 2015][1],
    cum_pct_change = (fare_change / base_2015 - 1) *100,

# Calculating YoY change
  annual_change = cum_pct_change - lag(cum_pct_change) 
  )

# Statistical Benchmarking
median_hike <- median(fares_15_24$annual_change[fares_15_24$year > 2015], na.rm = TRUE)

# Classification Logic
fares_15_24 <- fares_15_24 %>% 
  mutate(
    impact = case_when(
      is.na(annual_change) ~ "Baseline",
      annual_change <= median_hike ~ "Below Average",
      TRUE ~ "Above Average"
    ),
# Label Formatting
    label_txt = case_when(
      is.na(annual_change) ~ "Baseline",
      annual_change >= 0 ~ paste0("+", round(annual_change, 1), "%"),
      TRUE ~ paste0(round(annual_change, 1), "%")
    )
  )
    
# SECTION 2: Visualisation
fig3 <- ggplot(fares_15_24, aes(x=factor(year), y= annual_change, fill=impact)) +
    
  # 1. Bar Geometry
  geom_col(width= 0.75, alpha = 0.9)+
  
  # 2. Reference Lines    
  geom_hline(yintercept = median_hike, linetype= "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linewidth =0.6, color = "grey20") +
  
  # 3. Dynamic Labelling
  geom_text(
    aes(label = label_txt,
        vjust = ifelse(is.na(annual_change), -0.5,
                       ifelse(annual_change >= 0, -0.5, 1.2))),
        size = 3.8, fontface ="bold"
      )+
        
  # 4. Colour Strategy
  scale_fill_manual(values = c(
    "Baseline" = "grey70",
    "Below Average"= "#1b9e77",
    "Above Average" = "#d95f02"
    ))+
        
  # 5. Scale Management
  scale_y_continuous(
    labels = function(x) paste0(round(x, 1), "%"),
    expand = expansion(mult = c(0.1, 0.2))
    )+
     
  # Annotation and Theme   
  labs(
    title ="Annual Real-Term Rail Fare Change (2015-2024)", 
    subtitle ="Figure 3: Bars show year-on-year change; dashed line indicates the median annual change",
    x= "Year",
    y = "Annual change (percentage point, real terms)",
    )+
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),   # Remove vertical grid lines
    panel.grid.minor = element_blank()
    )+
        
  coord_cartesian(clip ="off")

# Render plot
fig3 