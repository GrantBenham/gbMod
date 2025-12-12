# gbMod & gbMed
Moderation and Mediation Analysis using Hayes' PROCESS - R Shiny Applications

## Overview
This repository contains two comprehensive Shiny applications for conducting statistical analyses using Hayes' PROCESS methodology:

- **gbMod** (`app.R`): Moderation Analysis (PROCESS Model 1) - Tests interaction effects between variables
- **gbMed** (`gbMed.R`): Mediation Analysis (PROCESS Model 6) - Tests direct and indirect effects through mediator(s)

Both applications provide intuitive interfaces with robust diagnostic capabilities and detailed visualization options. They automatically handle both continuous and binary outcomes, using linear regression for continuous outcomes and logistic regression for binary outcomes, and adapt diagnostics, plots, and outlier/influential-case handling accordingly.

## Shared Features

### Core Functionality
- **Automatic Model Selection**: 
  - Linear regression for continuous outcomes
  - Logistic regression for binary (0/1) outcomes
  - Automatic detection of binary variables
- **Data Formats**: Accepts both CSV and SPSS (.sav) file formats
- **Bootstrapping**: Optional bootstrap confidence intervals with customizable sample size (default: 5000)
- **Bivariate Correlations**: Zero-order correlations between predictor and outcome
  - For continuous outcomes: Pearson's r and Spearman's ρ
  - For binary outcomes: Point-biserial correlation (Pearson's r) with appropriate interpretation
  - Guidance on interpreting differences from analysis coefficients
- **Covariate Support**: Full support for multiple covariates with proper integration into PROCESS analysis

### Diagnostic Capabilities (Both Apps)
- **Assumption Checks**: Comprehensive evaluation of regression assumptions
  - **For Continuous Outcomes**:
    - Normality of residuals (Shapiro-Wilk test and Q-Q plots)
    - Homoscedasticity (Breusch-Pagan test and scale-location plots)
    - Multicollinearity (VIF analysis)
    - Outlier detection using standardized residuals (user-specified threshold)
    - Continuous-variable violin plots show side-by-side distributions for original vs after-removal datasets (one row per variable, free y-scales)
    - Binary-variable counts shown for original and after-removal datasets
  - **For Binary Outcomes**:
    - Appropriate diagnostics for logistic regression
    - Pearson residuals vs. fitted probabilities plot with Cook's D highlighting
    - VIF analysis for multicollinearity
    - Note: Normality and homoscedasticity assumptions do not apply to logistic regression
    - Influential-case detection using Cook's distance (configurable threshold: 4/n conservative, 1.0 liberal, or custom), with counts shown when applicable
- **Missing Data Reporting**: Detailed breakdown of missing data by variable
  - Shows which variables have missing values and how many cases are missing
  - Helps identify data quality issues before analysis
- **Data Validation**: Automatic validation of complete cases before analysis
  - Prevents errors from insufficient data
  - Clear error messages if analysis cannot proceed
  - **Outlier/Influential-Case Button State**: The "With Outliers Removed" button is automatically disabled when:
    - No outliers/influential cases are detected
    - Removing outliers would leave fewer than 3 complete cases
    - Button state updates reactively when threshold values change
- **Interactive Plots**: Diagnostic plots with appropriate interpretation for each model type
- **Real-time Updates**: Assumption checks update automatically based on variable selection

### Analysis Settings (Both Apps)
- **Centering Options**: 
  - No centering
  - Mean center all variables that define products
  - Mean center only continuous variables that define products
- **Heteroscedasticity-Consistent Standard Errors**: Multiple HC estimators (HC0-HC4) for continuous outcomes
  - **Note**: HC estimators apply to linear regression only; not applicable to logistic regression
- **Bootstrap Configuration**: Customizable number of bootstrap samples (1000-10000)
- **Outlier / Influential-Case Handling**: 
  - For continuous outcomes: Option to run analysis with or without standardized residual outliers (button enabled only when outliers exist and removal leaves ≥3 cases)
  - For binary outcomes: Option to run analysis with or without Cook's distance influential cases (button enabled only when influential cases exist and removal leaves ≥3 cases)
  - Violin plots and binary counts reflect removals
- **Export Capabilities**:
  - **Results Download**: HTML-formatted analysis results with appropriate formatting for linear vs. logistic regression
  - **Filtered Dataset**: Download dataset with outliers/influential cases removed (CSV or SAV format)

## gbMod - Moderation Analysis (app.R)

### Moderation-Specific Features
- **PROCESS Model 1**: Tests interaction effects between predictor and moderator
- **Variable Selection**: Outcome, predictor, moderator, and optional covariates
- **Visualization Options**:
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
- **Conditioning Values**: Mean ±1 SD or 16th/50th/84th percentiles for simple slopes
- **Plot Downloads**: High-quality JPG images of Johnson-Neyman and simple slopes plots

### Moderation Analysis Results
- **Interaction Test**: Highlighted in red font for easy identification
  - For continuous outcomes: R² change and p-value
  - For binary outcomes: Chi-square (likelihood ratio test) and p-value
- **Conditional Effects**: Shows how the predictor's effect varies across moderator levels
- **Regression Coefficients**: 
  - For continuous outcomes: Coefficients on the original scale
  - For binary outcomes: Coefficients are log-odds; see odds ratio guidance in output

## gbMed - Mediation Analysis (gbMed.R)

### Mediation-Specific Features
- **PROCESS Model 6**: Tests direct and indirect effects through mediator(s)
  - Supports single mediator (simple mediation)
  - Supports multiple mediators (parallel mediation)
- **Variable Selection**: Outcome, predictor, mediator(s) (multiple selection), and optional covariates
- **Pairwise Contrasts**: Optional comparison of indirect effects to determine which mediators are most important
  - Available when multiple mediators are selected
  - Contrasts are highlighted in red font when significant
- **Mediation Analysis Results**:
  - **Direct Effect**: Effect of predictor on outcome controlling for mediator(s) - highlighted in red if significant
  - **Indirect Effect(s)**: Effect through mediator(s) - highlighted in red if significant
    - For parallel mediation: Multiple indirect effects (one per mediator)
    - Serial indirect effects (through multiple mediators in sequence) when applicable
  - **Total Effect**: Direct + indirect effects - highlighted in red if significant
  - **Pairwise Contrasts**: Comparison of indirect effects (when enabled)
    - Shows which mediators contribute significantly more/less to the total indirect effect
    - Highlighted in red when significant
- **Odds Ratio Guidance** (for binary outcomes):
  - Comprehensive explanation of odds ratios vs. log-odds
  - Guidance on when to report each
  - Instructions for calculating odds ratios from log-odds coefficients
  - Note on SPSS PROCESS output format

## Usage Instructions

### Getting Started
1. **Data Preparation**:
   - Prepare your dataset in CSV or SPSS (.sav) format
   - Ensure variables are numeric for analysis
   - Binary variables should be coded as 0/1 (or have exactly 2 unique values)
   - Include all variables you plan to use in the analysis

2. **Launch the App**:
   - For moderation analysis: `runApp('app.R')`
   - For mediation analysis: `runApp('gbMed.R')`

### gbMod - Moderation Analysis Workflow

#### 1. Upload and Variable Selection
- Upload your data file using the file input
- Select your outcome variable (dependent variable)
  - **Note**: The app will automatically detect if your outcome is binary (0/1) and use logistic regression
- Select your predictor variable (independent variable)
- Select your moderator variable
  - **Note**: If your moderator is dichotomous (binary), the Johnson-Neyman plot will be automatically disabled
- Optionally add covariates if needed

#### 2. Configure Analysis Settings
- Choose centering method for product terms
  - **Recommendation**: Use "Only continuous variables that define products" when the moderator is dichotomous
- Select conditioning values for simple slopes (Mean ±1 SD or percentiles)
- Decide whether to use bootstrapping (recommended: 5000+ samples)
- Choose heteroscedasticity-consistent standard errors if needed (for continuous outcomes only)

#### 3. Review Assumptions
- Check the "Assumption Checks" tab for diagnostic information
  - **For Continuous Outcomes**:
    - Review standardized residual outliers (threshold-driven)
    - Examine diagnostic plots for normality and homoscedasticity
    - Continuous-variable violin plots show original vs after-removal distributions
    - Binary-variable counts shown for original and after-removal datasets
  - **For Binary Outcomes**:
    - Review Pearson residuals vs. fitted probabilities plot (with Cook's D highlighting)
    - Check VIF values for multicollinearity
    - Influential cases detected via Cook's distance (configurable threshold); counts shown when applicable
    - Note that normality and homoscedasticity assumptions do not apply

#### 4. Run Analysis
- Navigate to the "Moderation Analysis" tab
- Choose "With Original Dataset" to include all cases
- Choose "With Outliers/Influential Cases Removed" (only enabled when outliers exist and removal leaves ≥3 cases)
- **Important**: Analysis runs only when you click one of these buttons (not automatically)

#### 5. Interpret Results
- **Interaction Test**: Highlighted in red font - examine for statistical significance
- **Conditional Effects**: Review how the predictor's effect varies across moderator levels
- **Johnson-Neyman Plot**: Use to identify regions of significance (continuous moderators only)
- **Simple Slopes Plot**: Visualize interaction effects

### gbMed - Mediation Analysis Workflow

#### 1. Upload and Variable Selection
- Upload your data file using the file input
- Select your outcome variable (dependent variable)
  - **Note**: The app will automatically detect if your outcome is binary (0/1) and use logistic regression
- Select your predictor variable (independent variable)
- Select your mediator variable(s)
  - **Single Mediator**: Select one mediator for simple mediation
  - **Multiple Mediators**: Select multiple mediators for parallel mediation
- Optionally add covariates if needed

#### 2. Configure Analysis Settings
- Choose centering method for product terms
- Decide whether to use bootstrapping (recommended: 5000+ samples)
- Choose heteroscedasticity-consistent standard errors if needed (for continuous outcomes only)
- **Pairwise Contrasts**: Check this option if you have multiple mediators and want to compare indirect effects
  - Only available when multiple mediators are selected
  - Compares indirect effects to determine which mediators are most important

#### 3. Review Assumptions
- Check the "Assumption Checks" tab for diagnostic information
  - Same diagnostic options as moderation analysis
  - Assumption checks are performed on the outcome model (Y ~ X + M + covariates)

#### 4. Run Analysis
- Navigate to the "Mediation Analysis" tab
- Choose "With Original Dataset" to include all cases
- Choose "With Outliers/Influential Cases Removed" (only enabled when outliers exist and removal leaves ≥3 cases)
- **Important**: Analysis runs only when you click one of these buttons (not automatically)

#### 5. Interpret Results
- **Direct Effect**: Effect of predictor on outcome controlling for mediator(s) - highlighted in red if significant
- **Indirect Effect(s)**: Effect through mediator(s) - highlighted in red if significant
  - For parallel mediation: Multiple indirect effects shown
  - Serial indirect effects shown when applicable
- **Total Effect**: Direct + indirect effects - highlighted in red if significant
- **Pairwise Contrasts** (if enabled): Compare indirect effects to identify which mediators are most important
- **Odds Ratios** (for binary outcomes): Comprehensive guidance on interpreting log-odds vs. odds ratios

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
- **Sample Size**: Ensure adequate sample size for analysis (typically N > 100)
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
- **Influential Cases**: Use Cook's distance to identify influential cases; consider removing them if justified theoretically
- **Model Fit**: Focus on pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke) rather than traditional R²
- **Odds Ratios**: Many researchers report odds ratios in results sections for easier interpretation, while log-odds are used in statistical models

### Covariates
- Include relevant covariates to control for confounding variables
- Be aware that covariates with missing values will reduce your sample size through listwise deletion
- The app will validate that sufficient complete cases remain before running analysis

### Centering
- **Moderation**: Use "Mean center all variables that define products" for continuous moderators
- **Moderation with Dichotomous Moderators**: Use "Only continuous variables that define products" to avoid centering the binary moderator
- **Mediation**: Centering options apply to product terms if any are created (less common in mediation)

### Bivariate Correlations
- Review bivariate correlations to understand the unadjusted relationship before interpreting analysis results
- For binary outcomes, understand that the correlation coefficient and logistic regression coefficients are on different scales
- Note that bivariate correlations use the original dataset; analysis may use filtered data when removals occur

## Interpretation Guidelines

### Bivariate vs. Partial Effects
- Bivariate correlations show the zero-order relationship without controlling for other variables
- Analysis coefficients are partial effects that control for other variables
- These will differ; the analysis provides more nuanced information
- For continuous outcomes: Use Pearson's r for linear relationships with normal data; Spearman's ρ for rank-based or non-normal data
- For binary outcomes: Point-biserial correlation (Pearson's r) is valid but assumes linearity; logistic regression uses a different scale (log-odds)

### Moderation Analysis (gbMod)

#### Interaction Effect
- **For Continuous Outcomes**: Focus on the R² change and p-value for the interaction term
- **For Binary Outcomes**: Focus on the Chi-square (likelihood ratio test) and p-value for the interaction term
- The interaction test section is highlighted in red font for easy identification of statistical significance

#### Conditional Effects
- Examine how the predictor's effect varies across moderator levels
- For binary outcomes, interpret conditional effects in terms of log-odds or odds ratios (after exponentiation)

#### Johnson-Neyman Analysis
- Use to identify specific moderator values where effects become significant
- **Note**: Only available for continuous moderators; not applicable to dichotomous moderators

#### Simple Slopes
- Visualize the nature and direction of interaction effects
- For continuous outcomes: Shows predicted values on the original scale
- For binary outcomes: Shows predicted probabilities (0-1 range); curved (sigmoid) lines are normal and expected

### Mediation Analysis (gbMed)

#### Direct Effect
- The effect of the predictor on the outcome, controlling for the mediator(s)
- Represents the effect that is not through the mediator(s)
- Highlighted in red font when significant (p < .05 or CI does not contain zero)

#### Indirect Effect(s)
- The effect of the predictor on the outcome through the mediator(s)
- Represents the mediated portion of the total effect
- For parallel mediation: Multiple indirect effects (one per mediator)
- Serial indirect effects occur when one mediator affects another mediator
- Highlighted in red font when significant (95% CI does not contain zero)

#### Total Effect
- The total effect of the predictor on the outcome (direct + indirect)
- Sum of the direct effect and all indirect effects
- Highlighted in red font when significant

#### Pairwise Contrasts
- Compares indirect effects to determine which mediators are most important
- Significant contrasts (CI does not contain zero) indicate that one indirect effect is significantly different from another
- Highlighted in red font when significant
- Use to identify which mediators contribute most to the total indirect effect

#### Odds Ratios (Binary Outcomes)
- **What are odds ratios?** Odds ratios (OR) show how the odds of the outcome change for a one-unit increase in the predictor
- **Interpretation**: OR > 1 means higher predictor values increase the odds of outcome = 1. OR < 1 means higher predictor values decrease the odds
- **When to report**: Many researchers report odds ratios in results sections for easier interpretation, while log-odds are used in statistical models
- **SPSS PROCESS**: The SPSS PROCESS macro shows log-odds coefficients in the main output. Odds ratios can be calculated by exponentiating the coefficients (e^coeff)
- **Calculation**: OR = e^coeff; Confidence intervals for OR can be obtained by exponentiating the LLCI and ULCI values

### Effect Sizes
- Consider both statistical and practical significance of findings
- For binary outcomes, consider odds ratios in addition to statistical significance

## Support
For questions or issues with the applications, please refer to the diagnostic information provided within the apps or consult Hayes' PROCESS documentation for theoretical guidance on moderation and mediation analysis.

## Citation
When using these applications, please cite:
- Hayes, A. F. (2022). Introduction to mediation, moderation, and conditional process analysis: A regression-based approach (3rd ed.). Guilford Press.
