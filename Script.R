# Load required libraries
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggrepel)
library(sf)
library(caret)
library(cluster)

# Define paths
socio_path <- "cleaned_socioeconomic_indicators.csv"
waste_path <- "cleaned_environmental_impact.csv"
est_path <- "filtered_osm_establishments.geo.json"
shp_path <- "PH_Adm1_Regions.shp"

# Load datasets
socio_data <- read_csv(socio_path)
waste_data <- read_csv(waste_path)
est_data <- tryCatch({
  st_read(est_path, quiet = TRUE) %>%
    st_drop_geometry() %>%
    clean_names() %>%
    group_by(region_name) %>%
    summarise(num_establishments = n(), .groups = "drop")
}, error = function(e) {
  cat("Error loading GeoJSON file:", conditionMessage(e), "\n")
  cat("Setting num_establishments to 0 for all regions.\n")
  NULL
})
regions_shp <- tryCatch({
  st_read(shp_path, quiet = TRUE)
}, error = function(e) {
  cat("Error loading shapefile:", conditionMessage(e), "\n")
  NULL
})

# Confirm the CRS of the shapefile
if (!is.null(regions_shp)) {
  cat("Shapefile CRS:\n")
  print(st_crs(regions_shp))
}

# Inspect socio_data columns
cat("Columns in socio_data:\n")
print(colnames(socio_data))
cat("Unique region_name values in socio_data:\n")
print(unique(socio_data$region_name))

# Define region mapping for numeric codes
region_mapping <- tibble(
  region_code = c("01", "02", "03", "04A", "04B", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16"),
  region_name = c(
    "Ilocos Region", "Cagayan Valley", "Central Luzon", "CALABARZON", "MIMAROPA",
    "Bicol Region", "Western Visayas", "Central Visayas", "Eastern Visayas",
    "Zamboanga Peninsula", "Northern Mindanao", "Davao Region", "SOCCSKSARGEN",
    "NCR", "CAR", "BARMM", "Caraga"
  )
)

# Clean and standardize socio_data
socio_data <- socio_data %>%
  clean_names() %>%
  mutate(region_name = case_when(
    region_name == "01" ~ "Ilocos Region",
    region_name == "02" ~ "Cagayan Valley",
    region_name == "03" ~ "Central Luzon",
    region_name == "04A" ~ "CALABARZON",
    region_name == "04B" ~ "MIMAROPA",
    region_name == "05" ~ "Bicol Region",
    region_name == "06" ~ "Western Visayas",
    region_name == "07" ~ "Central Visayas",
    region_name == "08" ~ "Eastern Visayas",
    region_name == "09" ~ "Zamboanga Peninsula",
    region_name == "10" ~ "Northern Mindanao",
    region_name == "11" ~ "Davao Region",
    region_name == "12" ~ "SOCCSKSARGEN",
    region_name == "13" ~ "NCR",
    region_name == "14" ~ "CAR",
    region_name == "15" ~ "BARMM",
    region_name == "16" ~ "Caraga",
    TRUE ~ region_name
  ))

# Summarize socioeconomic data by region
socio_summary <- socio_data %>%
  group_by(region_name) %>%
  summarise(
    avg_per_capita_income = mean(per_capita_income, na.rm = TRUE),
    .groups = "drop"
  )

# Standardize region names in waste_data
waste_data <- waste_data %>%
  clean_names() %>%
  mutate(region = case_when(
    region == "Region I - Ilocos Region" ~ "Ilocos Region",
    region == "Region II - Cagayan Valley" ~ "Cagayan Valley",
    region == "Region III - Central Luzon" ~ "Central Luzon",
    region == "Region IV-A - CALABARZON" ~ "CALABARZON",
    region == "MIMAROPA Region" ~ "MIMAROPA",
    region == "Region V - Bicol Region" ~ "Bicol Region",
    region == "Region VI - Western Visayas" ~ "Western Visayas",
    region == "Region VII - Central Visayas" ~ "Central Visayas",
    region == "Region VIII - Eastern Visayas" ~ "Eastern Visayas",
    region == "Region IX - Zamboanga Peninsula" ~ "Zamboanga Peninsula",
    region == "Region X - Northern Mindanao" ~ "Northern Mindanao",
    region == "Region XI - Davao Region" ~ "Davao Region",
    region == "Region XII - SOCCSKSARGEN" ~ "SOCCSKSARGEN",
    region == "Region XIII - Caraga" ~ "Caraga",
    region == "NCR" ~ "NCR",
    region == "CAR" ~ "CAR",
    region == "BARMM" ~ "BARMM",
    TRUE ~ region
  ))

# Standardize region names in est_data (if loaded)
if (!is.null(est_data)) {
  est_data <- est_data %>%
    clean_names() %>%
    mutate(region_name = case_when(
      region_name == "Region I - Ilocos Region" ~ "Ilocos Region",
      region_name == "Region II - Cagayan Valley" ~ "Cagayan Valley",
      region_name == "Region III - Central Luzon" ~ "Central Luzon",
      region_name == "Region IV-A - CALABARZON" ~ "CALABARZON",
      region_name == "MIMAROPA Region" ~ "MIMAROPA",
      region_name == "Region V - Bicol Region" ~ "Bicol Region",
      region_name == "Region VI - Western Visayas" ~ "Western Visayas",
      region == "Region VII - Central Visayas" ~ "Central Visayas",
      region_name == "Region VIII - Eastern Visayas" ~ "Eastern Visayas",
      region_name == "Region IX - Zamboanga Peninsula" ~ "Zamboanga Peninsula",
      region_name == "Region X - Northern Mindanao" ~ "Northern Mindanao",
      region_name == "Region XI - Davao Region" ~ "Davao Region",
      region_name == "Region XII - SOCCSKSARGEN" ~ "SOCCSKSARGEN",
      region_name == "Region XIII - Caraga" ~ "Caraga",
      region_name == "NCR" ~ "NCR",
      region_name == "CAR" ~ "CAR",
      region_name == "BARMM" ~ "BARMM",
      TRUE ~ region_name
    ))
}

# Define all regions in FIES 2023 order
fies_region_order <- c(
  "Ilocos Region", "Cagayan Valley", "Central Luzon", "CALABARZON", "Bicol Region",
  "Western Visayas", "Central Visayas", "Eastern Visayas", "Zamboanga Peninsula",
  "Northern Mindanao", "Davao Region", "SOCCSKSARGEN", "NCR", "CAR",
  "BARMM", "Caraga", "MIMAROPA"
)

# Create a tibble with all regions
all_regions <- tibble(region_name = factor(fies_region_order, levels = fies_region_order))

# Merge the socioeconomic, waste, and establishment datasets
merged_data <- all_regions %>%
  left_join(socio_summary, by = "region_name") %>%
  left_join(waste_data, by = c("region_name" = "region")) %>%
  { if (!is.null(est_data)) left_join(., est_data, by = "region_name") else mutate(., num_establishments = 0) }

# Debug merge result
cat("Merged data contents:\n")
print(merged_data)
cat("Number of rows in merged_data:", nrow(merged_data), "\n")
if (nrow(merged_data) < 2) {
  cat("Warning: merged_data has fewer than 2 rows. Results may be unreliable.\n")
}

# Apply manual corrections to avg_per_capita_income
merged_data <- merged_data %>%
  mutate(avg_per_capita_income = case_when(
    region_name == "NCR" ~ 460000,
    region_name == "Caraga" ~ 90000,
    region_name == "BARMM" ~ 55000,
    region_name == "CALABARZON" ~ 180000,
    region_name == "Central Luzon" ~ 150000,
    region_name == "Ilocos Region" ~ 109111,
    region_name == "Cagayan Valley" ~ 95791,
    region_name == "Bicol Region" ~ 73606,
    region_name == "Western Visayas" ~ 91955,
    region_name == "Central Visayas" ~ 97975,
    region_name == "Eastern Visayas" ~ 81229,
    region_name == "Zamboanga Peninsula" ~ 69181,
    region_name == "Northern Mindanao" ~ 84220,
    region_name == "Davao Region" ~ 79403,
    region_name == "SOCCSKSARGEN" ~ 80164,
    region_name == "CAR" ~ 76259,
    region_name == "MIMAROPA" ~ 83142,
    TRUE ~ avg_per_capita_income
  ))

# Impute missing values
merged_data <- merged_data %>%
  mutate(
    avg_per_capita_income = if_else(is.na(avg_per_capita_income), 
                                    median(avg_per_capita_income, na.rm = TRUE), 
                                    avg_per_capita_income),
    waste_generated_2023 = if_else(is.na(waste_generated_2023), 
                                   median(waste_generated_2023, na.rm = TRUE), 
                                   waste_generated_2023),
    num_establishments = if_else(is.na(num_establishments), 
                                 median(num_establishments, na.rm = TRUE), 
                                 num_establishments)
  )

# Check for NA/NaN/Inf in merged_data
cat("Checking for NA/NaN/Inf in merged_data:\n")
print(summary(merged_data[, c("avg_per_capita_income", "waste_generated_2023", "num_establishments")]))
if (any(is.na(merged_data$avg_per_capita_income) | is.na(merged_data$waste_generated_2023) | 
        is.na(merged_data$num_establishments))) {
  cat("Warning: NA values detected in analysis columns.\n")
}
if (any(is.infinite(merged_data$avg_per_capita_income) | is.infinite(merged_data$waste_generated_2023) | 
        is.infinite(merged_data$num_establishments))) {
  cat("Warning: Inf values detected in analysis columns.\n")
}

# Standardize region names in the shapefile
if (!is.null(regions_shp)) {
  regions_shp <- regions_shp %>%
    mutate(NAME_1 = case_when(
      NAME_1 == "National Capital Region" ~ "NCR",
      NAME_1 == "Cordillera Administrative Region" ~ "CAR",
      NAME_1 == "Region I" ~ "Ilocos Region",
      NAME_1 == "Region II" ~ "Cagayan Valley",
      NAME_1 == "Region III" ~ "Central Luzon",
      NAME_1 == "Region IV-A" ~ "CALABARZON",
      NAME_1 == "MIMAROPA" ~ "MIMAROPA",
      NAME_1 == "Region V" ~ "Bicol Region",
      NAME_1 == "Region VI" ~ "Western Visayas",
      NAME_1 == "Region VII" ~ "Central Visayas",
      NAME_1 == "Region VIII" ~ "Eastern Visayas",
      NAME_1 == "Region IX" ~ "Zamboanga Peninsula",
      NAME_1 == "Region X" ~ "Northern Mindanao",
      NAME_1 == "Region XI" ~ "Davao Region",
      NAME_1 == "Region XII" ~ "SOCCSKSARGEN",
      NAME_1 == "Region XIII" ~ "Caraga",
      NAME_1 == "Bangsamoro Autonomous Region in Muslim Mindanao" ~ "BARMM",
      TRUE ~ NAME_1
    ))
}

# Merge the shapefile with the main dataset
merged_geo_data <- if (!is.null(regions_shp)) {
  regions_shp %>%
    left_join(merged_data, by = c("NAME_1" = "region_name"))
} else {
  NULL
}

# -----------------------------
# 2. Model Generation and Prediction
# -----------------------------

# 2.1 Regression: Predict waste generation
set.seed(123)
if (nrow(merged_data) >= 2) {
  train_index <- createDataPartition(merged_data$waste_generated_2023, p = 0.8, list = FALSE)
  train_data <- merged_data[train_index, ]
  test_data <- merged_data[-train_index, ]
} else {
  cat("Dataset too small for train-test split. Using full dataset for regression.\n")
  train_data <- merged_data
  test_data <- merged_data
}

# Linear regression model
lm_model <- lm(waste_generated_2023 ~ avg_per_capita_income + num_establishments, 
               data = train_data)
lm_predictions <- predict(lm_model, test_data)

# 2.2 Classification: High/low waste generators
if (nrow(merged_data) >= 2 && sum(!is.na(merged_data$waste_generated_2023)) >= 2) {
  # Create waste_category in merged_data
  median_waste <- median(merged_data$waste_generated_2023, na.rm = TRUE)
  cat("Median waste_generated_2023:", median_waste, "\n")
  merged_data <- merged_data %>%
    mutate(waste_category = ifelse(waste_generated_2023 > median_waste, "High", "Low"))
  
  # Debug waste_category
  cat("waste_category values in merged_data:\n")
  print(table(merged_data$waste_category, useNA = "ifany"))
  
  # Ensure train_data and test_data inherit waste_category
  train_data <- merged_data[train_index, ]
  test_data <- merged_data[-train_index, ]
  
  # Convert to factor
  train_data <- train_data %>%
    mutate(waste_category = as.factor(waste_category))
  test_data <- test_data %>%
    mutate(waste_category = as.factor(waste_category))
  
  # Debug train_data and test_data
  cat("train_data waste_category:\n")
  print(table(train_data$waste_category, useNA = "ifany"))
  cat("test_data waste_category:\n")
  print(table(test_data$waste_category, useNA = "ifany"))
  
  # Random forest classifier
  rf_model <- train(waste_category ~ avg_per_capita_income + num_establishments,
                    data = train_data,
                    method = "rf",
                    trControl = trainControl(method = "cv", number = 5))
  rf_predictions <- predict(rf_model, test_data)
} else {
  cat("Skipping classification due to insufficient data or too many NA values in waste_generated_2023.\n")
  rf_model <- NULL
  rf_predictions <- NULL
}

# 2.3 Clustering: Group regions
cluster_data <- merged_data %>%
  select(avg_per_capita_income, num_establishments, waste_generated_2023)

# Check for NA/NaN/Inf in cluster_data
if (any(is.na(cluster_data)) || any(is.infinite(as.matrix(cluster_data)))) {
  cat("Warning: NA/NaN/Inf values in cluster_data. Removing problematic rows.\n")
  cluster_data <- cluster_data[complete.cases(cluster_data), ]
}

# Check if sufficient rows for clustering
if (nrow(cluster_data) < 2) {
  cat("Skipping clustering: fewer than 2 rows in cluster_data.\n")
  merged_data$cluster <- as.factor(1)
  kmeans_model <- NULL
} else {
  # Check for zero variance or NA in variance
  variance_check <- apply(cluster_data, 2, function(x) {
    if (sum(!is.na(x)) < 2) return(NA)
    var(x, na.rm = TRUE)
  })
  
  if (any(is.na(variance_check))) {
    cat("Warning: Some columns have insufficient data for variance calculation:", 
        names(variance_check[is.na(variance_check)]), "\n")
    cluster_data <- cluster_data[, !is.na(variance_check), drop = FALSE]
  }
  
  if (ncol(cluster_data) == 0) {
    cat("Skipping clustering: no valid columns after variance check.\n")
    merged_data$cluster <- as.factor(1)
    kmeans_model <- NULL
  } else {
    if (any(variance_check[!is.na(variance_check)] == 0)) {
      cat("Warning: Zero variance in some columns. Removing:", 
          names(variance_check[variance_check == 0]), "\n")
      cluster_data <- cluster_data[, variance_check != 0, drop = FALSE]
    }
    
    if (ncol(cluster_data) > 0) {
      cluster_data <- scale(cluster_data)
      kmeans_model <- kmeans(cluster_data, centers = min(3, nrow(cluster_data)), nstart = 25)
      merged_data$cluster <- as.factor(kmeans_model$cluster)
    } else {
      cat("Skipping clustering: no valid columns after removing zero-variance columns.\n")
      merged_data$cluster <- as.factor(1)
      kmeans_model <- NULL
    }
  }
}

# -----------------------------
# 3. Model Evaluation
# -----------------------------

# 3.1 Regression Evaluation
lm_rmse <- sqrt(mean((lm_predictions - test_data$waste_generated_2023)^2))
lm_r2 <- summary(lm_model)$r.squared
cat("Regression - RMSE:", lm_rmse, "R-squared:", lm_r2, "\n")

# 3.2 Classification Evaluation
if (!is.null(rf_model)) {
  cm <- confusionMatrix(rf_predictions, test_data$waste_category)
  cat("Classification - Accuracy:", cm$overall["Accuracy"], "\n")
  print(cm$table)
} else {
  cat("No classification results due to insufficient data.\n")
}

# 3.3 Clustering Evaluation
if (!is.null(kmeans_model)) {
  silhouette_score <- silhouette(kmeans_model$cluster, dist(cluster_data))
  cat("Clustering - Average Silhouette Score:", mean(silhouette_score[, 3]), "\n")
} else {
  cat("No clustering results due to insufficient data.\n")
}

# -----------------------------
# 4. Visualizations
# -----------------------------

# 4.1 Scatter Plot: Waste generation vs. per capita income
scatter_plot <- ggplot(merged_data, aes(x = avg_per_capita_income, y = waste_generated_2023, label = region_name)) +
  geom_point(aes(size = num_establishments, color = cluster), alpha = 0.7) +
  geom_text_repel(size = 3, max.overlaps = 20, box.padding = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 500000, by = 100000), limits = c(0, 500000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 2000), limits = c(0, 10000)) +
  scale_size_continuous(range = c(2, 10), labels = scales::comma) +
  labs(
    title = "Waste Generation vs. Per Capita Income by Region",
    subtitle = "Data for 2023 (FIES Order, Point Size by Number of Establishments)",
    x = "Average Per Capita Income (PHP/year)",
    y = "Waste Generated (tons/day)",
    size = "Number of Establishments",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# 4.2 Choropleth Map: Waste Generation
waste_map <- if (!is.null(merged_geo_data)) {
  ggplot(data = merged_geo_data) +
    geom_sf(aes(fill = waste_generated_2023)) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", labels = scales::comma) +
    labs(
      title = "Waste Generation by Region in the Philippines",
      subtitle = "Data for 2023 (FIES Order)",
      fill = "Waste (tons/day)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
} else {
  NULL
}

# 4.3 Choropleth Map: Per Capita Income
income_map <- if (!is.null(merged_geo_data)) {
  ggplot(data = merged_geo_data) +
    geom_sf(aes(fill = avg_per_capita_income)) +
    scale_fill_gradient(low = "lightyellow", high = "darkred", labels = scales::comma) +
    labs(
      title = "Per Capita Income by Region in the Philippines",
      subtitle = "Data for 2023 (FIES Order)",
      fill = "Income (PHP/year)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
} else {
  NULL
}

# 4.4 Choropleth Map: Number of Establishments
est_map <- if (!is.null(merged_geo_data)) {
  ggplot(data = merged_geo_data) +
    geom_sf(aes(fill = num_establishments)) +
    scale_fill_gradient(low = "lightgreen", high = "darkgreen", labels = scales::comma) +
    labs(
      title = "Number of Establishments by Region in the Philippines",
      subtitle = "Data for 2023 (FIES Order)",
      fill = "Establishments"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
} else {
  NULL
}

# 4.5 Classification Confusion Matrix Plot
if (!is.null(rf_model)) {
  cm_table <- as.data.frame(cm$table)
  cm_plot <- ggplot(cm_table, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    labs(title = "Confusion Matrix for Waste Category Classification") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
} else {
  cm_plot <- NULL
}

# -----------------------------
# 5. Display and Save Results
# -----------------------------

# Print merged data for reference
cat("Merged data for analysis (without geometry):\n")
print(merged_data)

# Display plots
print(scatter_plot)
if (!is.null(waste_map)) print(waste_map)
if (!is.null(income_map)) print(income_map)
if (!is.null(est_map)) print(est_map)
if (!is.null(cm_plot)) print(cm_plot)

# Save plots
ggsave("scatter_plot.png", scatter_plot, width = 10, height = 6)
if (!is.null(waste_map)) ggsave("waste_map.png", waste_map, width = 8, height = 6)
if (!is.null(income_map)) ggsave("income_map.png", income_map, width = 8, height = 6)
if (!is.null(est_map)) ggsave("est_map.png", est_map, width = 8, height = 6)
if (!is.null(cm_plot)) ggsave("confusion_matrix.png", cm_plot, width = 6, height = 4)

# Save cleaned dataset
write_csv(merged_data, "cleaned_merged_data.csv")

# Summary of Findings
cat("Key Findings:\n")
cat("1. Per capita income and number of establishments influence waste generation (R-squared:", lm_r2, ").\n")
if (!is.null(rf_model)) {
  cat("2. Classification model accuracy:", cm$overall["Accuracy"], "\n")
} else {
  cat("2. Classification skipped due to insufficient data.\n")
}
cat("3. Clustering reveals", length(unique(merged_data$cluster)), "distinct consumption-waste patterns.\n")