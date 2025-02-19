# =============================================================================
# Displacement Risk Analysis for Champaign County, IL
# =============================================================================
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)

# Load Census Variables
tidyvars_acs5_2023 <- load_variables(2023, 'acs5', cache = TRUE)

# -----------------------------------------------------------------------------
# 1) Load API key
# -----------------------------------------------------------------------------
census_api_key("34f17c95b8c724b261582ca3ce529cd441191d8c", install = TRUE, overwrite = TRUE)

# -----------------------------------------------------------------------------
# 2) Retrieve a Census measure for displacement risk (rent burden) & LAI data
# -----------------------------------------------------------------------------
census_data <- get_acs(
  geography = "tract",
  variables = "B25070_001",  # Gross Rent as a Percentage of Household Income
  year = 2022,
  state = "IL",
  county = "Champaign"
)

lai_data <- read.csv("https://raw.githubusercontent.com/tshangiris11/Housing-Crisis-Project/refs/heads/main/Champaign_County_LAI.csv")

lai_data$GEOID <- as.character(lai_data$GEOID)
census_data$GEOID <- as.character(census_data$GEOID)

# -----------------------------------------------------------------------------
# 3) Merge data, create a 'displacement_risk' measure
# -----------------------------------------------------------------------------
merged_data <- census_data %>%
  left_join(lai_data, by = "GEOID") %>%
  mutate(displacement_risk = (
    estimate +
      hh1_model_h_cost_renters +
      hh1_model_h_cost_owners +
      pct_renters +
      median_hh_income / area_median_hh_income +
      median_rooms_per_renter_hu / median_rooms_per_owner_hu +
      job_density_simple +
      retail_density_simple
  ) / 8)

# -----------------------------------------------------------------------------
# 4) Retrieve & reshape Census data for (1) renters, (2) homeowners,
#    (3) racial/ethnic groups, and (4) seniors, (5) children
# -----------------------------------------------------------------------------
pop_vars <- c(
  # Renters, Homeowners
  renters = "B25003_003",             # Renter-occupied housing units
  homeowners = "B25003_002",          # Owner-occupied housing units
  
  # Race / Ethnicity Totals (B25003-based)
  white = "B25003A_001",              # White alone (Total)
  black = "B25003B_001",              # Black alone (Total)
  asian = "B25003D_001",              # Asian alone (Total)
  hispanic = "B25003I_001",           # Hispanic or Latino (Total)
  
  # Seniors (male 65+)
  senior_m_65_66 = "B01001_020",
  senior_m_67_69 = "B01001_021",
  senior_m_70_74 = "B01001_022",
  senior_m_75_79 = "B01001_023",
  senior_m_80_84 = "B01001_024",
  senior_m_85_plus = "B01001_025",
  
  # Seniors (female 65+)
  senior_f_65_66 = "B01001_044",
  senior_f_67_69 = "B01001_045",
  senior_f_70_74 = "B01001_046",
  senior_f_75_79 = "B01001_047",
  senior_f_80_84 = "B01001_048",
  senior_f_85_plus = "B01001_049",
  
  # Children (male under 18)
  child_m_under5 = "B01001_003",
  child_m_5_9   = "B01001_004",
  child_m_10_14 = "B01001_005",
  child_m_15_17 = "B01001_006",
  
  # Children (female under 18)
  child_f_under5 = "B01001_027",
  child_f_5_9   = "B01001_028",
  child_f_10_14 = "B01001_029",
  child_f_15_17 = "B01001_030"
)

population_data <- get_acs(
  geography = "tract",
  variables = pop_vars,
  year = 2022,
  state = "IL",
  county = "Champaign"
) %>%

  select(GEOID, variable, estimate) %>%
  pivot_wider(
    names_from = variable, 
    values_from = estimate,
    values_fn = sum
  ) %>%
  # Summation across the senior columns
  mutate(
    seniors = rowSums(
      select(
        ., 
        starts_with("senior_m_"), 
        starts_with("senior_f_")
      ),
      na.rm = TRUE
    ),
    children = rowSums(
      select(
        ., 
        starts_with("child_m_"),
        starts_with("child_f_")
      ),
      na.rm = TRUE
    )
  )

# -----------------------------------------------------------------------------
# 5) Merge population data & compute proportions at risk
# -----------------------------------------------------------------------------
full_data <- merged_data %>%
  left_join(population_data, by = "GEOID")

risk_analysis <- full_data %>%
  summarize(
    # (1) Renters
    renters_at_risk = sum(renters * displacement_risk, na.rm = TRUE) /
      sum(renters, na.rm = TRUE),
    
    # (2) Homeowners
    homeowners_at_risk = sum(homeowners * displacement_risk, na.rm = TRUE) /
      sum(homeowners, na.rm = TRUE),
    
    # (3) Racial / Ethnic Groups
    white_at_risk = sum(white * displacement_risk, na.rm = TRUE) /
      sum(white, na.rm = TRUE),
    
    black_at_risk = sum(black * displacement_risk, na.rm = TRUE) /
      sum(black, na.rm = TRUE),
    
    asian_at_risk = sum(asian * displacement_risk, na.rm = TRUE) /
      sum(asian, na.rm = TRUE),
    
    hispanic_at_risk = sum(hispanic * displacement_risk, na.rm = TRUE) /
      sum(hispanic, na.rm = TRUE),
    
    # (4) Seniors
    seniors_at_risk = sum(seniors * displacement_risk, na.rm = TRUE) /
      sum(seniors, na.rm = TRUE),
    
    # (5) Children
    children_at_risk = sum(children * displacement_risk, na.rm = TRUE) /
      sum(children, na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# 6) Print results
# -----------------------------------------------------------------------------
print(risk_analysis)

# -----------------------------------------------------------------------------
# 7) Visualizations
# -----------------------------------------------------------------------------
# Link for Perplexity AI: https://www.perplexity.ai/search/how-to-ensure-the-r-script-is-Q839hABBR5SlnXSEGQBbJA
# A) Distribution of Displacement Risk (Histogram + Density Overlay)
ggplot(full_data, aes(x = displacement_risk)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "steelblue", color = "white", alpha = 0.6) +
  geom_density(color = "darkred", size = 1) +
  labs(
    title = "Displacement Risk Distribution (Density + Histogram)",
    x = "Displacement Risk",
    y = "Density"
  ) +
  theme_minimal()

# B) Bar chart of the risk_analysis summary
risk_plot_data <- risk_analysis %>%
  pivot_longer(
    cols = everything(), 
    names_to = "group", 
    values_to = "prop_risk"
  )

ggplot(risk_plot_data, aes(
  x = reorder(group, prop_risk), 
  y = prop_risk, 
  fill = prop_risk
)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(
    aes(label = scales::percent(prop_risk, accuracy = 1)),
    color = "black",
    size = 3,
    hjust = 1.1 
  ) +
  labs(
    title = "Proportion of Each Group at Risk",
    x = "Group",
    y = "Proportion at Risk"
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Risk"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# C) Choropleth Map of Displacement Risk
# 1) Get geometry for tracts
options(tigris_use_cache = TRUE, tigris_class = "sf")
champaign_tracts <- tracts(state = "IL", county = "Champaign", year = 2022)

# 2) Join geometry to analysis data
map_data <- champaign_tracts %>%
  select(GEOID) %>%
  left_join(full_data, by = "GEOID")

# 3) Plot using tmap in static (plot) mode
tmap_mode("plot")  # STATIC map instead of interactive
tm_shape(map_data) +
  tm_polygons(
    col = "displacement_risk",   #
    style = "jenks",
    palette = "Reds",
    title = "Displacement Risk"
  ) +
  tm_layout(
    legend.outside = TRUE,
    main.title = "Displacement Risk in Champaign County, IL"
  )


# D) Boxplots: Distribution of Displacement Risk by Census Race/Ethnicity
# Create a 'majority_race' label for each tract
full_data <- full_data %>%
  mutate(
    majority_race = case_when(
      white > black & white > asian & white > hispanic ~ "White-Majority",
      black > white & black > asian & black > hispanic ~ "Black-Majority",
      asian > white & asian > black & asian > hispanic ~ "Asian-Majority",
      hispanic > white & hispanic > black & hispanic > asian ~ "Hispanic-Majority",
      TRUE ~ "Mixed/Other"
    )
  )

# Boxplot of displacement_risk by majority_race group
ggplot(full_data, aes(x = majority_race, y = displacement_risk, fill = majority_race)) +
  geom_boxplot(alpha = 0.6, outlier.color = "red") +
  labs(
    title = "Displacement Risk by Majority Race/Ethnicity",
    x = "Majority Race/Ethnicity in Tract",
    y = "Displacement Risk"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# E)Facet Histogram: Compare Distribution of Displacement Risk in High‐ vs. Low‐Renter Tracts
# 1) Define 'high renter' (>= 50%) vs 'low renter' (< 50%), dropping NA
full_data_clean <- full_data %>%
  mutate(renter_category = case_when(
    pct_renters >= 50 ~ "High Renter%",
    pct_renters < 50  ~ "Low Renter%",
    TRUE ~ NA_character_  # anything else (rare)
  )) %>%
  filter(!is.na(renter_category))

# 2) Facet histogram, controlling bin size
ggplot(full_data_clean, aes(x = displacement_risk)) +
  geom_histogram(
    fill = "springgreen4",
    color = "white",
    alpha = 0.6,
    bins = 20 
  ) +
  facet_wrap(~ renter_category, scales = "free_y") +
  labs(
    title = "Displacement Risk Distribution by Renter Category",
    subtitle = "Comparison of High vs. Low Renter% Census Tracts",
    x = "Displacement Risk",
    y = "Number of Census Tracts"
  ) +
  theme_minimal(base_size = 14)

# F) Scatter: Displacement Risk vs. Median Household Income
ggplot(full_data, aes(x = median_hh_income, y = displacement_risk)) +
  geom_point(aes(color = pct_renters), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_gradient(low = "yellow", high = "red", name = "Pct Renters") +
  labs(
    title = "Displacement Risk vs. Median Household Income",
    x = "Median HH Income",
    y = "Displacement Risk"
  ) +
  theme_minimal()


