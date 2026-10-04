# Import the cleaned NHANES dataset
nhanes_clean <- import(here("data", "cleaned_NHANES.csv"))

# Step 1: Calculate Average SBP across all available readings
nhanes_prep <- nhanes_clean %>%
  rowwise() %>%
  mutate(
    avg_sbp = mean(
      c_across(starts_with("systolic_bp_")),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# check data types
str(nhanes_prep)

# Step 2: Filter complete cases for age, gender, and avg_sbp
final_nhanes_sample <- nhanes_prep %>% 
  filter(!is.na(age), !is.na(gender), !is.na(avg_sbp))

str(final_nhanes_sample)

# Step 3: Classify SBP categories according to avg SBP
final_nhanes_sample <- final_nhanes_sample %>%
  mutate(
    sbp_category = cut(
      avg_sbp,
      breaks = c(-Inf, 120, 130, 140, Inf),
      labels = c(
        "Normal",
        "Elevated",
        "Stage 1 hypertension",
        "Stage 2 hypertension"
      ),
      right = FALSE
    )
  )

str(final_nhanes_sample)



# Plot: Comparison of Gender Distribution (Reused from Exercise 2)
p_genderfinal <- ggplot(final_nhanes_sample, aes(x = gender, fill = gender)) +
  geom_bar(width = 0.5) +
  scale_fill_manual(values = c("Male" = "#0072B2", "Female" = "#D55E00")) +
  scale_y_continuous(breaks = seq(0, 15000, by = 5000)) +
    labs(
    title = "Gender Distribution in Final Sample",
    x = "Biological Sex / Gender",
    y = "Participant Count",
    fill = "Gender"
  ) +
  theme_minimal()+
  theme (
    axis.text.x = element_text(color = "black", size = 12), 
    axis.text.y = element_text(color = "black", size = 8), 
    legend.position = "none"
  )

ggsave(
  "plot_genderfinal.png", 
  plot = p_genderfinal, 
  path = here("figures", "Assessment3"), 
  width = 6, height = 4.5, dpi = 300
)

p_gender_comparison <- plot_grid(p_gender, p_genderfinal, ncol = 2, labels = c("A", "B"))
p_gender_comparison 

# Comparison of Age Distribution (Reused from Exercise 2)
fig_1a_age_initial <- ggplot(nhanes_clean, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#2b5c8f", color = "white", boundary = 0) +
  scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  labs(
    title = "Age Distribution", 
    subtitle = "Initial Sample (N = 29,400)", x = "Age (Years)", y = "Participant Count"
    ) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"))

fig_1b_age_final <- ggplot(final_nhanes_sample, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#0072B2", color = "white", boundary = 0) +
  scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
    labs(
    title = "Age Distribution",
    subtitle = "Final Analytic Sample (N = 21,604)", 
    x = "Age (Years)", 
    y = "Participant Count"
    ) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"))

p_age_comparison <- plot_grid(fig_1a_age_initial, fig_1b_age_final, ncol = 2, labels = c("A", "B"))
p_age_comparison

# Plot - Hypertension Category Counts
p3_hypertension_counts <- ggplot(final_nhanes_sample, aes(x = sbp_category, fill = sbp_category)) +
geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Normal"               = "darkseagreen2",
    "Elevated"             = "darkseagreen4",
    "Stage 1 hypertension" = "darkorange",
    "Stage 2 hypertension" = "red"
    )) +
  labs(title = "Distribution of Systolic Blood Pressure Categories", x = "Blood Pressure Category", y = "Participant Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", face = "bold", size = 12),
    axis.text.y = element_text(color = "black", size = 8),
    plot.title = element_text(face = "bold", size = 14)
    )


# Step 5
# Plot Prevalence of Hypertension Categories Across Age Groups by Gender
p_sbpcat_age <- ggplot(final_nhanes_sample, aes(x = age_cat, fill = sbp_category)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ gender) +
  scale_fill_manual(
    values = c(
      "Normal"               = "lightblue",
      "Elevated"             = "darkseagreen",
      "Stage 1 hypertension" = "orange",
      "Stage 2 hypertension" = "red"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = "Prevalence of Hypertension Categories Across Age Groups by Gender",
    x = "Age Group (Years)",
    y = "Individual Counts",
    fill = "SBP Category"
  )

ggsave("plot_sbpcat_age.png", 
       plot = p_sbpcat_age, 
       path = here("figures", "Assessment3"), 
)