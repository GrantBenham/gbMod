# gbMod
Moderation Analysis using Hayes' PROCESS - R Shiny Application

## Overview
gbMod is a comprehensive Shiny application designed to conduct moderation analysis using Hayes' PROCESS methodology. The application provides an intuitive interface for testing interaction effects between variables, with robust diagnostic capabilities and detailed visualization options. The app automatically handles both continuous and binary outcomes, using linear regression for continuous outcomes and logistic regression for binary outcomes.

## Features

### Core Analysis
- **Moderation Analysis**: Implements Hayes' PROCESS Model 1 for testing interaction effects
- **Automatic Model Selection**: 
  - Linear regression for continuous outcomes
  - Logistic regression for binary (0/1) outcomes
  - Automatic detection of binary variables
- **Variable Selection**: Support for outcome, predictor, moderator, and optional covariate variables
- **Data Formats**: Accepts both CSV and SPSS (.sav) file formats
- **Bootstrapping**: Optional bootstrap confidence intervals with customizable sample size (default: 5000)
- **Bivariate Correlations**: Zero-order correlations between predictor and outcome
  - For continuous outcomes: Pearson's r and Spearman's ρ
  - For binary outcomes: Point-biserial correlation (Pearson's r) with appropriate interpretation
  - Guidance on interpreting differences from moderation coefficients

### Diagnostic Capabilities
- **Assumption Checks**: Comprehensive evaluation of regression assumptions
  - **For Continuous Outcomes**:
    - Normality of residuals (Shapiro-Wilk test and Q-Q plots)
    - Homoscedasticity (Breusch-Pagan test and scale-location plots)
    - Multicollinearity (VIF analysis)
    - Outlier detection using standardized residuals
  - **For Binary Outcomes**:
    - Appropriate diagnostics for logistic regression
    - Pearson residuals vs. fitted probabilities plot
    - VIF analysis for multicollinearity
    - Note: Normality and homoscedasticity assumptions do not apply to logistic regression
    - Outlier removal based on standardized residuals is disabled (not standard practice for binary outcomes)
- **Missing Data Reporting**: Detailed breakdown of missing data by variable
  - Shows which variables have missing values and how many cases are missing
  - Helps identify data quality issues before analysis
- **Data Validation**: Automatic validation of complete cases before analysis
  - Prevents errors from insufficient data
  - Clear error messages if analysis cannot proceed
- **Interactive Plots**: Diagnostic plots with appropriate interpretation for each model type
- **Real-time Updates**: Assumption checks update automatically based on variable selection

### Visualization Options
- **Johnson-Neyman Plot**: Identifies regions of significance for the moderator
  - **Note**: Automatically disabled for dichotomous (binary) moderators, as JN analysis requires a continuous moderator
- **Simple Slopes Plot**: Visualizes interaction effects at different moderator levels
  - **For Continuous Outcomes**: Shows predicted values on the original scale
  - **For Binary Outcomes**: Shows predicted probabilities (0-1 range) instead of log-odds for better interpretability
  - Curved lines (sigmoid shape) are expected and normal for binary outcomes
- **Customizable Plotting**: 
  - Color vs. line type options
  - Custom axis labels and titles
  - Adjustable y-axis ranges (automatically set to 0-1 for binary outcomes)
  - Decimal precision control

### Analysis Settings
- **Centering Options**: 
  - No centering
  - Mean center all variables that define products
  - Mean center only continuous variables that define products (recommended when moderator is dichotomous)
- **Conditioning Values**: Mean ±1 SD or 16th/50th/84th percentiles
- **Heteroscedasticity-Consistent Standard Errors**: Multiple HC estimators (HC0-HC4) for continuous outcomes
  - **Note**: HC estimators apply to linear regression only; not applicable to logistic regression
- **Bootstrap Configuration**: Customizable number of bootstrap samples (1000-10000)
- **Outlier Handling**: 
  - For continuous outcomes: Option to run analysis with or without standardized residual outliers
  - For binary outcomes: Outlier removal button is disabled (not standard practice for logistic regression)
- **Covariate Support**: Full support for multiple covariates with proper integration into PROCESS analysis

### Export Capabilities
- **Results Download**: HTML-formatted analysis results with appropriate formatting for linear vs. logistic regression
- **Plot Downloads**: High-quality JPG images of Johnson-Neyman and simple slopes plots
- **Filtered Dataset**: Download dataset with outliers removed (CSV or SAV format) - available only for continuous outcomes

## Usage Instructions

### 1. Data Preparation
- Prepare your dataset in CSV or SPSS (.sav) format
- Ensure variables are numeric for analysis
- Binary variables should be coded as 0/1 (or have exactly 2 unique values)
- Include all variables you plan to use in the analysis

### 2. Upload and Variable Selection
- Upload your data file using the file input
- Select your outcome variable (dependent variable)
  - **Note**: The app will automatically detect if your outcome is binary (0/1) and use logistic regression
- Select your predictor variable (independent variable)
- Select your moderator variable
  - **Note**: If your moderator is dichotomous (binary), the Johnson-Neyman plot will be automatically disabled
- Optionally add covariates if needed

### 3. Configure Analysis Settings
- Choose centering method for product terms
  - **Recommendation**: Use "Only continuous variables that define products" when the moderator is dichotomous
- Select conditioning values for simple slopes
- Decide whether to use bootstrapping (recommended: 5000+ samples)
- Choose heteroscedasticity-consistent standard errors if needed (for continuous outcomes only)

### 4. Review Assumptions
- Check the "Assumption Checks" tab for diagnostic information
  - **For Continuous Outcomes**:
    - Review standardized residual outliers
    - Examine diagnostic plots for normality and homoscedasticity
    - Consider the impact of outliers on your analysis
  - **For Binary Outcomes**:
    - Review Pearson residuals vs. fitted probabilities plot
    - Check VIF values for multicollinearity
    - Note that normality and homoscedasticity assumptions do not apply
    - Outlier removal is not available (not standard practice for binary outcomes)
- The app automatically adapts the diagnostic information based on your outcome variable type

### 5. Run Analysis
- Navigate to the "Moderation Analysis" tab
- Choose "With Original Dataset" to include all cases
- Choose "With Outliers Removed" to exclude cases above the residual threshold
  - **Note**: This option is only available for continuous outcomes
  - For binary outcomes, this button is automatically disabled
- **Important**: Analysis runs only when you click one of these buttons (not automatically)
- Review results in the "Moderation Analysis" tab

### 6. Interpret Results
- **Sample Size Information**: Review the sample size breakdown
  - Original dataset size
  - Number of outliers removed (if applicable, continuous outcomes only)
  - Missing data summary with detailed breakdown by variable
  - Final sample size used in analysis
- **Bivariate Correlations**: Review zero-order correlations between predictor and outcome
  - For continuous outcomes: Pearson's r and Spearman's ρ
  - For binary outcomes: Point-biserial correlation (Pearson's r) with appropriate interpretation
  - Understand the difference between zero-order (bivariate) and partial (moderation) effects
  - Note that bivariate correlations use the original dataset, while moderation may use filtered data
- **Model Fit Statistics**: 
  - For continuous outcomes: R² and F-test
  - For binary outcomes: -2LL, pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke), and likelihood ratio test
- **Regression Coefficients**: 
  - For continuous outcomes: Coefficients on the original scale
  - For binary outcomes: Coefficients are log-odds; exponentiate to get odds ratios
- **Interaction Test**: Examine the interaction test for statistical significance
  - For continuous outcomes: R² change and p-value
  - For binary outcomes: Chi-square (likelihood ratio test) and p-value
  - **Note**: The interaction test section is highlighted in red font for easy identification
- **Conditional Effects**: Review conditional effects at different moderator levels
- **Johnson-Neyman Plot**: Use to identify regions of significance for the moderator
  - **Note**: Only available for continuous moderators; automatically disabled for dichotomous moderators
- **Simple Slopes Plot**: Interpret for practical significance and visualization of interaction effects
  - For continuous outcomes: Shows predicted values on the original scale
  - For binary outcomes: Shows predicted probabilities (0-1 range); curved lines are normal and expected

### 7. Export Results
- Download formatted results for reporting
- Save plots for presentations or publications
- Export filtered dataset if outliers were removed (continuous outcomes only)

## Technical Requirements
- **R Version**: 3.6.0 or higher
- **Required Packages**:
  - shiny
  - bslib
  - ggplot2
  - stringr
  - dplyr
  - shinyjs
  - car (for VIF and heteroscedasticity tests)
  - haven (for SPSS file support)
  - PROCESSR (Hayes' PROCESS for R package)

## Best Practices

### General Guidelines
- **Sample Size**: Ensure adequate sample size for moderation analysis (typically N > 100)
- **Variable Quality**: Use reliable and valid measures for all variables
- **Effect Size**: Consider practical significance in addition to statistical significance
- **Bootstrapping**: Use bootstrapping for more robust inference, especially with non-normal data (recommended: 5000+ samples)

### Missing Data
- Review the missing data breakdown to identify data quality issues
- Check which variables have missing values and how many cases are affected
- Consider the impact of listwise deletion on your final sample size
- Ensure sufficient complete cases remain after accounting for missing data

### Continuous Outcomes
- **Assumption Checking**: Always review diagnostic plots and tests in the "Assumption Checks" tab
- **Outlier Handling**: Document any cases removed and justify the decision based on both statistical and theoretical considerations
- **Heteroscedasticity**: If homoscedasticity is violated, consider using HC standard errors (HC3 recommended for small samples)

### Binary Outcomes
- **Model Interpretation**: Remember that coefficients are on the log-odds scale; exponentiate to interpret as odds ratios
- **Residual Patterns**: For binary outcomes, residuals form distinct bands (one for each outcome level); this is expected behavior
- **Outlier Removal**: Do not remove cases based on residuals for binary outcomes; if you have concerns about specific cases, examine them individually
- **Model Fit**: Focus on pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke) rather than traditional R²

### Covariates
- Include relevant covariates to control for confounding variables
- Be aware that covariates with missing values will reduce your sample size through listwise deletion
- The app will validate that sufficient complete cases remain before running analysis

### Centering
- **Recommendation**: Use "Mean center all variables that define products" for continuous moderators
- **For Dichotomous Moderators**: Use "Only continuous variables that define products" to avoid centering the binary moderator

### Bivariate Correlations
- Review bivariate correlations to understand the unadjusted relationship before interpreting moderation effects
- For binary outcomes, understand that the correlation coefficient and logistic regression coefficients are on different scales

## Interpretation Guidelines

### Bivariate vs. Partial Effects
- Bivariate correlations show the zero-order relationship without controlling for other variables
- Moderation coefficients are partial effects that control for moderator, interaction, and covariates
- These will differ; the moderation analysis provides more nuanced information
- For continuous outcomes: Use Pearson's r for linear relationships with normal data; Spearman's ρ for rank-based or non-normal data
- For binary outcomes: Point-biserial correlation (Pearson's r) is valid but assumes linearity; logistic regression in the moderation analysis uses a different scale (log-odds)

### Interaction Effect
- **For Continuous Outcomes**: Focus on the R² change and p-value for the interaction term
- **For Binary Outcomes**: Focus on the Chi-square (likelihood ratio test) and p-value for the interaction term
- The interaction test section is highlighted in red font for easy identification of statistical significance

### Conditional Effects
- Examine how the predictor's effect varies across moderator levels
- For binary outcomes, interpret conditional effects in terms of log-odds or odds ratios (after exponentiation)

### Johnson-Neyman Analysis
- Use to identify specific moderator values where effects become significant
- **Note**: Only available for continuous moderators; not applicable to dichotomous moderators

### Simple Slopes
- Visualize the nature and direction of interaction effects
- For continuous outcomes: Shows predicted values on the original scale
- For binary outcomes: Shows predicted probabilities (0-1 range); curved (sigmoid) lines are normal and expected

### Effect Sizes
- Consider both statistical and practical significance of findings
- For binary outcomes, consider odds ratios in addition to statistical significance

### Covariates
- When covariates are included, their effects are controlled in the moderation analysis
- Covariates are included in both the main model and interaction test

## Support
For questions or issues with the application, please refer to the diagnostic information provided within the app or consult Hayes' PROCESS documentation for theoretical guidance on moderation analysis.

## Citation
When using this application, please cite:
- Hayes, A. F. (2022). Introduction to mediation, moderation, and conditional process analysis: A regression-based approach (3rd ed.). Guilford Press.
