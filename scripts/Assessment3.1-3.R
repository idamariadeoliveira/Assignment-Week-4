library("pacman")
pacman::p_load(
  rio, here, tidyverse, knitr, kableExtra, skimr, 
  formatR, gridExtra, janitor, ggplot2, dplyr, readr, cowplot, ggpubr, RColorBrewer, viridis
)

# Import the cleaned NHANES dataset
nhanes_clean <- import(here("data", "cleaned_NHANES.csv"))

# Preview the first six rows
# Ensure gender, age and ethnicity are properly plotted
head(nhanes_clean)

# --------------------------------------------------------
# Exercise 1 - Reproducing and arranging ggplot2 figures

(rio, here, ggplot2, cowplot, ggpubr)

#Figure 1: Histogram of Age by Gender (5-Year Bin Width)
# In the original plot, RIDAGEYR was plotted on the x-axis with raw gender codes 1 and 2. 
# Using the cleaned dataset, we map age to x, gender to fill, and set binwidth = 5
# Figure 1: Age distribution by Gender (5-year bin width)
p1 <- ggplot(nhanes_clean, aes(x = age, fill = gender)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(
    x = "Age (years)",
    y = "count",
    fill = "Gender"
  )

# Figure 2: Relative proportion of Gender across Ethnicity groups
# Filter out missing ethnicity values if any exist
p2 <- ggplot(nhanes_clean, aes(x = ethnicity_2, fill = gender)) +
  geom_bar(position = "fill") +
  labs(
    x = "Race / Ethnicity",
    y = "count",
    fill = "Gender"
  )

# Multi-Panel Plot Arrangements
# Combine using cowplot
p1_p2_cowplot_combined <- plot_grid(
  p1, p2,
  rel_widths = c(1, 1),
  nrow = 1, ncol = 2
)
p1_p2_cowplot_combined

# Combine using ggpubr with shared legend
p1_p2_ggpubr_combined <- ggarrange(
  p1, p2,
  ncol = 2, nrow = 1,
  common.legend = TRUE,
  legend = "top"
)
p1_p2_ggpubr_combined

# --------------------------------------------
# Exercise 2 - Visualizing key characteristics

library(ggplot2)

# 1. Distribution of Age
p_age <- ggplot(nhanes_clean, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "deepskyblue4", color = "white", boundary = 0) +
  labs(
    title = "Age Distribution of NHANES Participants (2013-2018)",
    x = "Age (Years)",
    y = "Participant Count"
  ) 
p_age 

ggsave("plot_age.png", 
       plot = p_age, 
       path = here("figures", "Assessment3"), 
       width = 7, height = 4.5, dpi = 300
       )

# 2. Distribution of Gender
p_gender <- ggplot(nhanes_clean, aes(x = gender, fill = gender)) +
  geom_bar(width = 0.5) +
  scale_fill_manual(values = c("Male" = "#56B4E9", "Female" = "#CC79A7")) +
  labs(
    title = "Gender Distribution of NHANES Participants",
    x = "Biological Sex / Gender",
    y = "Participant Count",
    fill = "Gender"
  ) +
  theme_minimal()+
  theme (
    axis.text.x = element_text(color = "black", size = 12), 
    axis.text.y = element_text(color = "black", size = 8), 
  )

ggsave(
  "plot_gender.png", 
  plot = p_gender, 
  path = here("figures", "Assessment3"), 
  width = 6, height = 4.5, dpi = 300
  )


# 3. Distribution of Ethnicity 1 (RIDRETH1)
p_eth1 <- ggplot(nhanes_clean, aes(x = ethnicity_1, fill = ethnicity_1)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Black"          = "#0072B2", 
    "White"          = "#009E73", 
    "Mexican"        = "#D55E00",
    "Other Hispanic" = "#E69F00", 
    "Other"          = "#999999")
    )+
  labs(
    title = "Distribution of Race/Ethnicity 1",
    x = "Race / Ethnicity Group",
    y = "Participant Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 12, angle = 35, hjust = 1),
    plot.title  = element_text(face = "bold", size = 14)
    )

ggsave(
  "plot_ethnicity_1.png", 
  plot = p_eth1,   
  path = here("figures", "Assessment3"), 
  width = 7, height = 4.5
  )


# 4. Distribution of Ethnicity 2 (RIDRETH3)
p_eth2 <- ggplot(nhanes_clean, aes(x = ethnicity_2, fill = ethnicity_2)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Black"          = "#0072B2", 
    "White"          = "#009E73", 
    "Mexican"        = "#D55E00",
    "Other Hispanic" = "#E69F00",
    "Asian"          = "#CC79A7",
    "Other"          = "#999999")
  )+
  labs(
    title = "Distribution of Race/Ethnicity 2",
    x = "Race / Ethnicity Group",
    y = "Participant Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 12, angle = 35, hjust = 1),
    plot.title  = element_text(face = "bold", size = 14)
  )

ggsave(
  "plot_ethnicity_2.png", 
  plot = p_eth2, 
  path = here("figures", "Assessment3"), 
  width = 7, height = 4.5
)


# 5. Distribution of Ethnicity Combined
p_combined_eth <- plot_grid(
  p_eth1, p_eth2, 
  labels = c("A", "B"), 
  ncol = 2
)

print(p_combined_eth)
ggsave(
  "combined_ethnicity.png", 
  plot = p_combined_eth, 
  path = here("figures", "Assessment3"),
  width = 11, height = 5)



# -------------------------------------
# Exercise 3 - Improving ggplot figure

library(tidyverse)

# 1. Import diet dataset
diet_data <- import(here("data", "diet.csv"))

head(diet_data)

# 2. Calculate weight change relative to baseline (Week 0)
diet_data <- diet_data %>%
  group_by(Participant) %>%
  mutate(Weight_Change = Weight - Weight[Week == 0],
         Final_Weight = Weight_Change[Week == 16]) %>%
  ungroup()

# 3. Create the ggplot graphic
ggplot(diet_data, aes(x = Week, y = Weight_Change, color = Final_Weight, group = Participant)) +
  # Add horizontal dashed line representing baseline (0 change)
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  # Draw lines and points for each participant
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.2) +
  # Color gradient
  scale_color_viridis(
    option = "viridis", 
    direction = -1,  
    name = "Total Change (kg)"
  ) +
  # Customize axis labels and titles
  labs(
    title = "Participant weight trajectories over 16 weeks",
    subtitle = "Weight change calculated relative to week 0 baseline",
    x = "Time (Weeks)",
    y = "Weight change from baseline (kg)",
    color = "Participant ID"
  ) +
  scale_x_continuous(breaks = seq(0, 16, by = 2)) +
  theme_minimal()


# Scatter Plot to Compare Baseline vs. Final Outcome
final_summary <- diet_data %>%
  group_by(Participant) %>%
  summarise(
    Starting_Weight = Weight[Week == 0],
    Total_Weight_Change = Weight[Week == 16] - Weight[Week == 0]
  )

ggplot(final_summary, aes(x = Starting_Weight, y = Total_Weight_Change,)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 4, color = "darkolivegreen") +
  labs(
    title = "Starting weight (Week 0) vs. Total weight change at week 16",
    subtitle = "16-week diet intervention outcome by participant baseline weight",
    x = "Baseline Weight (kg)",
    y = "Net change from baseline (kg)"
  ) +
  theme_minimal() +
  theme(axis.line.x = element_line(linewidth = 0.5), 
        axis.line.y = element_line(linewidth = 0.5))

