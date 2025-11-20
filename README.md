# gbMod
Moderation Analysis using Hayes' PROCESS - R Shiny Application

## Overview
gbMod is a comprehensive Shiny application designed to conduct moderation analysis using Hayes' PROCESS methodology. The application provides an intuitive interface for testing interaction effects between variables, with robust diagnostic capabilities and detailed visualization options.

## Features

### Core Analysis
- **Moderation Analysis**: Implements Hayes' PROCESS Model 1 for testing interaction effects
- **Variable Selection**: Support for outcome, predictor, moderator, and optional covariate variables
- **Data Formats**: Accepts both CSV and SPSS (.sav) file formats
- **Bootstrapping**: Optional bootstrap confidence intervals with customizable sample size (default: 5000)
- **Bivariate Correlations**: Zero-order correlations (Pearson's r and Spearman's ρ) between predictor and outcome, with guidance on interpreting differences from moderation coefficients

### Diagnostic Capabilities
- **Assumption Checks**: Comprehensive evaluation of regression assumptions
  - Normality of residuals (Shapiro-Wilk test and Q-Q plots)
  - Homoscedasticity (Breusch-Pagan test)
  - Multicollinearity (VIF analysis)
  - Outlier detection using standardized residuals
- **Interactive Plots**: Diagnostic plots with outlier highlighting
- **Real-time Updates**: Assumption checks update automatically based on variable selection

### Visualization Options
- **Johnson-Neyman Plot**: Identifies regions of significance for the moderator
- **Simple Slopes Plot**: Visualizes interaction effects at different moderator levels
- **Customizable Plotting**: 
  - Color vs. line type options
  - Custom axis labels and titles
  - Adjustable y-axis ranges
  - Decimal precision control

### Analysis Settings
- **Centering Options**: No centering, mean centering, or standardization
- **Conditioning Values**: Mean ±1 SD or 16th/50th/84th percentiles
- **Heteroscedasticity-Consistent Standard Errors**: Multiple HC estimators (HC0-HC4) with proper implementation
- **Bootstrap Configuration**: Customizable number of bootstrap samples (1000-10000)
- **Outlier Handling**: Option to run analysis with or without standardized residual outliers
- **Covariate Support**: Full support for multiple covariates with proper integration into PROCESS analysis

### Export Capabilities
- **Results Download**: HTML-formatted analysis results
- **Plot Downloads**: High-quality JPG images of Johnson-Neyman and simple slopes plots
- **Filtered Dataset**: Download dataset with outliers removed (CSV or SAV format)

## Usage Instructions

### 1. Data Preparation
- Prepare your dataset in CSV or SPSS (.sav) format
- Ensure variables are numeric for analysis
- Include all variables you plan to use in the analysis

### 2. Upload and Variable Selection
- Upload your data file using the file input
- Select your outcome variable (dependent variable)
- Select your predictor variable (independent variable)
- Select your moderator variable
- Optionally add covariates if needed

### 3. Configure Analysis Settings
- Choose centering method (recommended: mean centering)
- Select conditioning values for simple slopes
- Decide whether to use bootstrapping
- Choose heteroscedasticity-consistent standard errors if needed

### 4. Review Assumptions
- Check the "Assumption Checks" tab for diagnostic information
- Review standardized residual outliers
- Examine diagnostic plots for normality and homoscedasticity
- Consider the impact of outliers on your analysis

### 5. Run Analysis
- Navigate to the "Moderation Analysis" tab
- Choose "With Original Dataset" to include all cases
- Choose "With Outliers Removed" to exclude cases above the residual threshold
- **Note**: Analysis runs only when you click one of these buttons (not automatically)
- Review results in the "Moderation Analysis" tab

### 6. Interpret Results
- **Bivariate Correlations**: Review zero-order correlations (Pearson's r and Spearman's ρ) between predictor and outcome
  - Understand the difference between zero-order (bivariate) and partial (moderation) effects
  - Note that bivariate correlations use the original dataset, while moderation may use filtered data
- **Interaction Test**: Examine the interaction test for statistical significance
- **Conditional Effects**: Review conditional effects at different moderator levels
- **Johnson-Neyman Plot**: Use to identify regions of significance for the moderator
- **Simple Slopes Plot**: Interpret for practical significance and visualization of interaction effects

### 7. Export Results
- Download formatted results for reporting
- Save plots for presentations or publications
- Export filtered dataset if outliers were removed

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

## Best Practices
- **Sample Size**: Ensure adequate sample size for moderation analysis (typically N > 100)
- **Variable Quality**: Use reliable and valid measures for all variables
- **Assumption Checking**: Always review diagnostic plots and tests in the "Assumption Checks" tab
- **Outlier Handling**: Document any cases removed and justify the decision based on both statistical and theoretical considerations
- **Effect Size**: Consider practical significance in addition to statistical significance
- **Bootstrapping**: Use bootstrapping for more robust inference, especially with non-normal data (recommended: 5000+ samples)
- **Heteroscedasticity**: If homoscedasticity is violated, consider using HC standard errors (HC3 recommended for small samples)
- **Covariates**: Include relevant covariates to control for confounding variables
- **Bivariate Correlations**: Review bivariate correlations to understand the unadjusted relationship before interpreting moderation effects

## Interpretation Guidelines
- **Bivariate vs. Partial Effects**: 
  - Bivariate correlations show the zero-order relationship without controlling for other variables
  - Moderation coefficients are partial effects that control for moderator, interaction, and covariates
  - These will differ; the moderation analysis provides more nuanced information
  - Use Pearson's r for linear relationships with normal data; Spearman's ρ for rank-based or non-normal data
- **Interaction Effect**: Focus on the R² change and p-value for the interaction term
- **Conditional Effects**: Examine how the predictor's effect varies across moderator levels
- **Johnson-Neyman**: Use to identify specific moderator values where effects become significant
- **Simple Slopes**: Visualize the nature and direction of interaction effects
- **Effect Sizes**: Consider both statistical and practical significance of findings
- **Covariates**: When covariates are included, their effects are controlled in the moderation analysis

## Support
For questions or issues with the application, please refer to the diagnostic information provided within the app or consult Hayes' PROCESS documentation for theoretical guidance on moderation analysis.

## Citation
When using this application, please cite:
- Hayes, A. F. (2022). Introduction to mediation, moderation, and conditional process analysis: A regression-based approach (3rd ed.). Guilford Press.
