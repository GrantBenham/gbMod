# gbMod
Moderation analysis using Hayes' PROCESS R shinyapp

## Overview
gbMod is a comprehensive Shiny application designed to guide users through the process of conducting Exploratory Factor Analysis (EFA) in a systematic and rigorous manner. The application follows best practices in factor analysis and provides detailed diagnostics at each step of the analysis process.

## Program Structure
The application is built using R Shiny and consists of five integrated modules:

### 1. Introduction (mod0_introduction.R)
- Provides an overview of the application and its workflow
- Explains the purpose and features of each module
- Guides users through the analysis process

### 2. Multicollinearity Check (mod1_multicollinearity.R)
- Assesses and addresses multicollinearity in the data
- Features:
  - Variance Inflation Factor (VIF) analysis
  - Correlation matrix determinant evaluation
  - Automated identification of problematic variables
  - Option to download cleaned dataset

### 3. Factor Estimation (mod2_factors.R)
- Determines the optimal number of factors to extract
- Features:
  - Multiple factor retention criteria (Kaiser criterion, Parallel analysis, Velicer's MAP test)
  - Interactive scree plot
  - Very Simple Structure (VSS) analysis
  - Comprehensive interpretation guidelines

### 4. Diagnostics (mod3_diagnostics.R)
- Evaluates data suitability for factor analysis
- Features:
  - Sampling adequacy tests (KMO, Bartlett's test)
  - Normality assessment (univariate and multivariate)
  - Correlation matrix analysis
  - Interactive correlation heatmap

### 5. Exploratory Factor Analysis (mod4_efa.R)
- Performs the final EFA with optimal settings
- Features:
  - Multiple extraction methods (Principal Axis Factoring, Minimum Residual)
  - Various rotation options (Promax, Varimax, Oblimin, Quartimax)
  - Correlation matrix options (Pearson, Spearman, Polychoric)
  - Customizable thresholds for factor loadings and communalities
  - Iterative refinement process
  - Comprehensive reliability analysis (Cronbach's alpha, McDonald's omega)
  - Detailed results export options

## Usage
1. Start with the Multicollinearity Check tab to prepare your data
2. Use the Factor Estimation tab to determine the optimal number of factors
3. Review your data's suitability in the Diagnostics tab
4. Finally, perform the EFA in the Exploratory Factor Analysis tab

## Technical Details
- Built using R Shiny framework
- Modular architecture for easy maintenance and updates
- Each module is self-contained with its own UI and server logic
- Comprehensive error handling and user feedback
- Export capabilities for results and cleaned datasets

## Requirements
- R (version 3.6.0 or higher)
- Shiny package
- Additional dependencies as specified in the program files

## Note
Each module provides detailed interpretations and recommendations to help users make informed decisions throughout the analysis process.
