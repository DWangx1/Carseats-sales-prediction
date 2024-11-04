# STAT-350 Term Project

# Child Car Seat Sales Prediction

## Introduction

This project aims to predict the unit sales of child car seats at various store locations using statistical modeling techniques. By analyzing a dataset of 400 stores, we develop models to forecast sales, focusing on model adequacy, variable selection, and model building to identify the most accurate predictor for sales.

## Methodology

### Preliminary Testing and Model Adequacy
- Exploration of data through scatterplots, histograms, and summary statistics.
- Assessment of model fit using residuals analysis, normal Q-Q plots, leverage, and influence diagnostics.

### Variable Selection
- Employed forward and backward selection, along with all subset regression, to refine the variables.
- Key variables identified: CompPrice, Income, Advertising, Price, ShelveLoc, Age. Less emphasis on Education and US, while Population and Urban were excluded.

### Model Building
- Comparison of Multiple Linear Regression (MLR) with Ridge Regression and LASSO, emphasizing the balance between bias and variance.
- Utilization of cross-validation for tuning λ in Ridge and LASSO models.

### Model Selection
- Evaluation of models based on Cross-Validation (CV) and Relative Mean Squared Prediction Error (RMSPE) to select the optimal model.

## Tools and Technologies
- **Statistical Analysis Software:** R for advanced modeling and visualization.
- **Modeling Techniques:** MLR, Ridge Regression, LASSO/

## Key Findings
- The MLR model demonstrated the highest predictive accuracy, validating the initial model adequacy checks and variable selection process.
- ShelveLoc emerged as a significant predictor, indicating the importance of product placement on sales.
- The analysis suggests store owners prioritize shelving location quality to maximize sales, with store location in the US being a secondary factor.

## Conclusion
The comprehensive analysis underscores the importance of shelving location in predicting child car seat sales. Future work could explore more complex models like Neural Networks and Random Forests for potentially improved predictions.

## How to Use
1. Clone this repository to access the analysis scripts and datasets.
2. Explore the Markdown file for a walkthrough of the project's thought process and analytical approach.
3. Run the `sales_prediction_analysis.R` script for a detailed walkthrough of the data processing, analysis, and modeling steps.

**Sources:**  
- [Carseats Dataset on rdrr.io](https://rdrr.io/cran/ISLR/man/Carseats.html)

