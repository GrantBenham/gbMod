# gbMod
Moderation Analysis using Hayes' PROCESS - R Shiny Application

## Overview
gbMod is a comprehensive Shiny application designed to conduct moderation analysis using Hayes' PROCESS methodology. The application provides an intuitive interface for testing interaction effects between variables, with robust diagnostic capabilities and detailed visualization options.

## Features

### Core Analysis
- **Moderation Analysis**: Implements Hayes' PROCESS Model 1 for testing interaction effects
- **Variable Selection**: Support for outcome, predictor, moderator, and optional covariate variables
- **Data Formats**: Accepts both CSV and SPSS (.sav) file formats
- **Bootstrapping**: Optional bootstrap confidence intervals for enhanced statistical inference

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
- **Heteroscedasticity-Consistent Standard Errors**: Multiple HC estimators (HC0-HC4)
- **Outlier Handling**: Option to run analysis with or without standardized residual outliers

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
- Choose "With Original Dataset" to include all cases
- Choose "With Outliers Removed" to exclude cases above the residual threshold
- Review results in the "Moderation Analysis" tab

### 6. Interpret Results
- Examine the interaction test for statistical significance
- Review conditional effects at different moderator levels
- Use Johnson-Neyman plot to identify regions of significance
- Interpret simple slopes plot for practical significance

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
- **Assumption Checking**: Always review diagnostic plots and tests
- **Outlier Handling**: Document any cases removed and justify the decision
- **Effect Size**: Consider practical significance in addition to statistical significance
- **Bootstrapping**: Use bootstrapping for more robust inference, especially with non-normal data

## Interpretation Guidelines
- **Interaction Effect**: Focus on the R² change and p-value for the interaction term
- **Conditional Effects**: Examine how the predictor's effect varies across moderator levels
- **Johnson-Neyman**: Use to identify specific moderator values where effects become significant
- **Simple Slopes**: Visualize the nature and direction of interaction effects
- **Effect Sizes**: Consider both statistical and practical significance of findings

## Support
For questions or issues with the application, please refer to the diagnostic information provided within the app or consult Hayes' PROCESS documentation for theoretical guidance on moderation analysis.

## Citation
When using this application, please cite:
- Hayes, A. F. (2022). Introduction to mediation, moderation, and conditional process analysis: A regression-based approach (3rd ed.). Guilford Press.
