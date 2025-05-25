
# Document Summary: Analyzing Consumption Patterns and Waste Management Practices in the Philippines using Big Data Analytics


**Authors**: Jhon Arol De Chavez, Juma Francois Galvez  
**Institution**: Technological Institute of the Philippines  

**Contact**: *qjadechavez@tip.edu.ph, qjfogalvez@tip.edu.ph*  


## Overview

This study aligns with UNSDG #12 (Responsible Consumption and Production) by analyzing consumption patterns and waste management practices across 17 Philippine regions in 2023. Using big data analytics, it examines socioeconomic (e.g., per capita income) and commercial (e.g., fast food outlets) factors to identify waste generation drivers. The study employs log-linear regression, logistic classification, k-means clustering, and correlation analysis to provide insights for evidence-based waste management policies.

## Key Objectives

- **Data Preparation**: Standardize and merge datasets (socioeconomic, environmental, establishment) for reliable modeling, ensuring data integrity for 17 regions.
- **Model Generation**: Develop predictive and descriptive models (regression, classification, clustering, correlation) to analyze waste generation drivers.
- **Model Evaluation**: Assess dataset and model performance using metrics like R-squared, accuracy, silhouette scores, and correlation significance.

## Methodology

- **Data**: Integrated 2023 datasets on per capita income, daily waste generation, and establishment counts (e.g., fast food outlets). Standardized region names, imputed missing data, and trimmed outliers.
- **Models**:
  - **Regression**: Log-linear model to predict waste generation using income and fast food outlets (R-squared: 0.6008).
  - **Classification**: Logistic regression to classify regions as high/low waste generators (accuracy: 88.24%).
  - **Clustering**: K-means clustering to group regions into three profiles (high, medium, low waste).
  - **Association**: Pearson correlation to identify relationships (e.g., 0.8699 for fast food outlets vs. waste).
- **Evaluation**: Validated dataset integrity and model performance using statistical metrics and diagnostics.

## Key Findings
- **Regression**: Income and fast food outlets moderately predict waste generation, suggesting additional factors (e.g., population density) for improved models.
- **Classification**: High accuracy in identifying high-waste regions (e.g., NCR, CALABARZON), with minor misclassifications near the median waste threshold.
- **Clustering**: Identified three regional profiles: NCR (high waste, 9,290 tons/day), CALABARZON/Central Luzon (medium-high), and 14 low-waste regions.
- **Association**: Strong correlations between waste generation and commercial factors (e.g., fast food outlets, supermarkets) and income.

## Recommendations
- **Commercial Regulation**: Enforce waste audits and single-use packaging bans in high-waste regions; promote recycling/reuse in fast food and supermarkets.
- **Region-Specific Strategies**: Deploy advanced recycling in high-waste areas; sustain low-waste regions with community programs.
- **Data Enhancement**: Build a national waste database and integrate additional variables (e.g., population density) for better predictions.
- **Education**: Launch campaigns to reduce single-use plastics and promote upcycling, tailored to urban/rural contexts.

## Future Directions
- Conduct longitudinal studies to track waste trends.
- Incorporate qualitative data from local government and businesses.
- Explore sector-specific waste dynamics (e.g., fast food packaging).
- Analyze urban vs. rural waste differences.
- Test advanced methods like hierarchical clustering or machine learning.

## Conclusion
This study provides a robust framework for understanding waste generation drivers in the Philippines, supporting UNSDG #12’s goal of reducing waste by 2030. By identifying high-waste regions and key predictors, it informs targeted policies for sustainable consumption and production.

**References**: *Beigl et al. (2008), DENR (2023), UNEP (2024), PSA (2023), and others cited in the paper.*
