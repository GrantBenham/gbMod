# Load necessary libraries
library(shiny)
library(bslib)
library(ggplot2)
library(stringr)
library(dplyr)
library(shinyjs)
library(car)  # For VIF and ncvTest functions
library(haven)  # Add this line to load haven package for read_sav

# Increase file upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$style(type="text/css", "body { max-width: 1400px; margin: auto; }"),
  titlePanel("Moderation Analysis with PROCESS for R"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Upload Data"),
      div(
        id = "file_input_div",
        fileInput("data_file", "Choose CSV or SAV File", accept = c(".csv", ".sav"))
      ),
      
      h4("Select Variables"),
      uiOutput("variable_selectors"),
      
      numericInput("residual_threshold", "Standardized Residual Threshold", 
                  value = 2, min = 1, max = 10, step = 0.1),
      
      # Wrap all PROCESS settings in a conditional panel
      conditionalPanel(
        condition = "input.tabset_panel === 'Moderation Analysis'",
        
        h4("PROCESS Settings"),
        checkboxInput("use_bootstrap", "Use bootstrapping", FALSE),
        conditionalPanel(
          condition = "input.use_bootstrap == true",
          numericInput("boot_samples", "Number of bootstrap samples:", 5000, min = 1000, max = 10000)
        ),
        
        radioButtons("centering", "Mean Centering:",
          choices = list(
            "No centering" = "0",
            "All variables that define products" = "1",
            "Only continuous variables that define products" = "2"
          ),
          selected = "0"
        ),
        radioButtons("conditioning_values", "Conditioning Values:",
          choices = list(
            "Mean and ±1 SD" = "0",
            "16th, 50th, 84th percentiles" = "1"
          ),
          selected = "0"
        ),
        selectInput("hc_method", "Heteroscedasticity-consistent inference:",
          choices = list(
            "None" = "none",
            "HC0 (Huber-White)" = "0",
            "HC1 (Hinkley)" = "1",
            "HC2" = "2",
            "HC3 (Davidson-MacKinnon)" = "3",
            "HC4 (Cribari-Neto)" = "4"
          ),
          selected = "none"
        ),
        
        h4("Simple Slopes Plot Settings"),
        selectInput("slopes_type", "Plot Type:",
          choices = list(
            "Full range of predictor" = "full",
            "Restricted range (matching conditioning points)" = "restricted"
          ),
          selected = "full"
        ),
        textInput("slopes_title", "Plot Title", "Simple Slopes Plot"),
        checkboxInput("use_color_lines", "Use color for lines", value = TRUE),
        checkboxInput("custom_y_axis", "Customize y-axis range", value = FALSE),
        conditionalPanel(
          condition = "input.custom_y_axis == true",
          numericInput("y_axis_min", "Y-axis minimum", value = 0),
          numericInput("y_axis_max", "Y-axis maximum", value = 100)
        ),
        textInput("x_label", "Label for Predictor", ""),
        textInput("y_label", "Label for Outcome", ""),
        textInput("moderator_label", "Label for Moderator", ""),
        numericInput("decimal_places", "Decimal Places for Moderator Levels", 2, min = 0, max = 5),
        
        h4("Run Analysis"),
        div(style = "margin-bottom: 10px; width: 100%;",
          actionButton("run_analysis", "With Original Dataset", 
            class = "btn-primary",
            style = "width: 100%;"
          )
        ),
        div(style = "margin-bottom: 20px; width: 100%;",
          actionButton("run_analysis_no_outliers", "With Outliers Removed", 
            class = "btn-warning",
            style = "width: 100%;"
          )
        ),
        
        h4("Download Options"),
        downloadButton("download_results", "Results Text"),
        downloadButton("download_jn", "JN Plot"),
        downloadButton("download_slopes", "Simple Slopes Plot"),
        conditionalPanel(
          condition = "input.run_analysis_no_outliers > input.run_analysis",
          h4("Download Reduced Dataset"),
          radioButtons("filtered_data_format", "Format:",
            choices = list(
              "CSV (.csv)" = "csv",
              "SPSS (.sav)" = "sav"
            ),
            selected = "csv",
            inline = TRUE
          ),
          downloadButton("download_filtered_data", "Dataset Without Outliers",
            class = "btn-warning")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabset_panel",  # Add this ID
        tabPanel("Assumption Checks",
          h4("Detailed Assumption Check Results"),
          
          # Add new explanatory note at the top
          div(style = "margin-bottom: 20px",
            p(strong("Note:"), " These assumption checks are always performed on the original dataset. Results update automatically based on your selected variables and standardized residual threshold value."),
            p(strong("Binary Outcomes:"), " If your outcome variable is binary (0/1), the app will automatically use logistic regression diagnostics. Standard regression assumptions (normality, homoscedasticity) do not apply to binary outcomes.")
          ),
          
          div(style = "margin-bottom: 20px",
            h5("Understanding Standardized Residual Outliers"),
            p("Standardized residuals (SR) represent how many standard deviations an observed value deviates from the model's prediction. Outliers can affect moderation analyses in several ways:",
              tags$ul(
                tags$li(strong("Impact on Results:"), " Outliers can inflate or deflate moderation effects and influence statistical significance"),
                tags$li(strong("Threshold Guidelines:"), " Common cutoffs include:"),
                tags$ul(
                  tags$li("|SR| > 2: Potentially influential cases"),
                  tags$li("|SR| > 2.5: More stringent criterion"),
                  tags$li("|SR| > 3: Very conservative criterion")
                ),
                tags$li(strong("Handling Outliers:"), " This program offers two approaches:"),
                tags$ul(
                  tags$li("Run analysis with all cases to maintain complete data"),
                  tags$li("Remove cases above the threshold to assess impact on results")
                ),
                tags$li(strong("Bootstrapping Consideration:"), " While bootstrapping can make analyses more robust to violations of normality, it does not directly address the impact of outliers. Consider running analyses both with and without outliers to assess their influence.")
              ),
              "Note: The decision to remove outliers should be based on both statistical criteria and theoretical considerations. Document and justify any case removals in your research."
            )
          ),
          
          htmlOutput("assumption_details"),
          
          h4("Diagnostic Plots"),
          div(style = "margin-bottom: 30px",
            h5("Normal Q-Q Plot"),
            p("This plot checks if residuals follow a normal distribution. Points should follow the diagonal line closely.",
              "Deviations at the ends are common and usually not problematic.",
              "When bootstrapping is used, normality is less crucial as bootstrap methods don't assume normality.",
              strong("Note: This plot is not applicable for binary outcomes."), " For binary outcomes, logistic regression does not assume normality of residuals."),
            plotOutput("qq_plot", height = "400px", width = "600px")
          ),
          
          div(style = "margin-bottom: 30px",
            h5("Residuals vs Fitted Plot"),
            p("This plot checks for linearity and homoscedasticity (constant variance).",
              "Look for:",
              tags$ul(
                tags$li("Random scatter around the horizontal line (linearity)"),
                tags$li("Even spread of points vertically (homoscedasticity)"),
                tags$li("No clear patterns or curves in the blue line")
              ),
              "With bootstrapping, minor violations of homoscedasticity are less concerning."),
            plotOutput("residual_plot", height = "400px", width = "600px")
          ),
          
          # Move heteroscedasticity explanation here
          div(style = "margin-bottom: 20px",
            h5("Note on Heteroscedasticity-Consistent Standard Errors"),
            p("When heteroscedasticity is detected (non-constant variance), PROCESS can apply heteroscedasticity-consistent (HC) standard error estimators. These adjustments modify how standard errors are calculated without changing the actual coefficients:",
              tags$ul(
                tags$li(strong("None (Default):"), " Uses standard OLS estimation assuming homoscedasticity"),
                tags$li(strong("HC0 (Huber-White):"), " The original 'sandwich' estimator, robust but can be biased in small samples"),
                tags$li(strong("HC1 (Hinkley):"), " A modification of HC0 with small sample correction"),
                tags$li(strong("HC2:"), " Further refinement that accounts for leverage values"),
                tags$li(strong("HC3 (Davidson-MacKinnon):"), " Conservative estimator, often recommended for small samples"),
                tags$li(strong("HC4 (Cribari-Neto):"), " Designed for high-leverage observations")
              ),
              "Note: The assumption check results shown here are based on the raw data, regardless of which HC method is selected. The HC selection only affects the standard errors in the PROCESS analysis output."
            )
          ),
          
          conditionalPanel(
            condition = "output.outcome_is_continuous === true",
            div(style = "margin-bottom: 30px",
              h5("Scale-Location Plot"),
              p("This plot helps assess if the variance of residuals changes across the range of predicted values.",
                "Look for:",
                tags$ul(
                  tags$li("Relatively horizontal blue line"),
                  tags$li("Even spread of points around the line"),
                  tags$li("No clear funnel or fan shapes")
                ),
                "When bootstrapping is used, this assumption is relaxed somewhat."),
              plotOutput("scale_location_plot", height = "400px", width = "600px")
            )
          ),
        ),
        tabPanel("Moderation Analysis",
          h4("Analysis Results"),
          htmlOutput("analysis_output"),
          
          h4("Johnson-Neyman Plot"),
          plotOutput("jn_plot", height = "500px", width = "800px"),
          
          h4("Simple Slopes Plot"),
          plotOutput("slopes_plot", height = "500px", width = "800px")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Add the new function at the start of the server
  create_formatted_output <- function(analysis_results) {
    # Get settings from analysis results
    settings <- analysis_results$settings
    
    # Start with dataset and variable information
    output_text <- c(
      "<strong>ANALYSIS SUMMARY</strong>",
      "",
      sprintf("Dataset name: %s", settings$dataset_name),
      sprintf("Dataset size: %d cases", settings$original_n),
      
      # Outlier information
      if(settings$outliers_removed) {
        sprintf("%d standardized residual outliers (|SR| > %.1f) removed", 
                settings$outliers_count, settings$outliers_threshold)
      } else {
        "Original dataset with all cases"
      },
      
      # Variable information
      sprintf("Predictor variable: %s", settings$predictor_var),
      sprintf("Outcome variable: %s", settings$outcome_var),
      sprintf("Moderator variable: %s", settings$moderator_var),
      if(!is.null(settings$covariates)) {
        sprintf("Covariate(s): %s", paste(settings$covariates, collapse = ", "))
      } else {
        "Covariate(s): none"
      },
      "",  # Add blank line before analysis settings
      
      # Original settings info
      "<strong>ANALYSIS SETTINGS</strong>",
      "<em>Configuration used for this analysis:</em>",
      paste("Centering:", switch(settings$centering,
        "0" = "No centering",
        "1" = "All variables that define products",
        "2" = "Only continuous variables that define products"
      )),
      if(settings$use_bootstrap) paste("Bootstrap samples:", settings$boot_samples),
      paste("Conditioning values:", if(settings$conditioning_values == "0") 
        "Mean and ±1 SD" else "16th, 50th, and 84th percentiles"),
      paste("Heteroscedasticity-consistent SE:", switch(settings$hc_method,
        "none" = "None",
        "0" = "HC0 (Huber-White)",
        "1" = "HC1 (Hinkley)",
        "2" = "HC2",
        "3" = "HC3 (Davidson-MacKinnon)",
        "4" = "HC4 (Cribari-Neto)"
      )),
      ""  # Add blank line before bivariate correlations
    )
    
    # Add bivariate correlations section
    bivariate_cor <- create_bivariate_correlations(
      analysis_results$original_data,  # Use original dataset
      settings$predictor_var,
      settings$outcome_var,
      settings  # Pass settings for dataset description
    )
    
    # Process the PROCESS output
    filtered_output <- analysis_results$output[
      !grepl("^Bootstrap", analysis_results$output, ignore.case = TRUE) &
      !grepl("^Percentile bootstrap", analysis_results$output, ignore.case = TRUE) &
      !grepl("^\\*+ BOOTSTRAP", analysis_results$output) &
      !grepl("^Level of confidence", analysis_results$output) &
      !grepl("^\\s*$", analysis_results$output) &
      !grepl("^\\*+$", analysis_results$output) &
      !grepl("^\\s*\\|", analysis_results$output) &
      !grepl("^\\s*\\d+%", analysis_results$output) &
      !grepl("^\\s*>+\\s*$", analysis_results$output)
    ]
    
    # Process the output to add formatting and section explanations
    processed_output <- character(0)
    missing_data_note <- filtered_output[grep("NOTE: Some cases with missing data were deleted", filtered_output)]
    filtered_output <- filtered_output[!grepl("NOTE: Some cases with missing data were deleted", filtered_output)]
    skip_next <- 0
    
    for(line in filtered_output) {
      if(skip_next > 0) {
        skip_next <- skip_next - 1
        next
      } else if(grepl("^Sample size:", line)) {
        sample_size <- as.numeric(gsub("Sample size: ", "", line))
        
        # Debug: Print values to console
        print(paste("DEBUG - settings$original_n:", settings$original_n))
        print(paste("DEBUG - sample_size:", sample_size))
        print(paste("DEBUG - settings$outliers_removed:", settings$outliers_removed))
        print(paste("DEBUG - settings$outliers_count:", settings$outliers_count))
        
        # Calculate missing data cases
        missing_cases <- settings$original_n - sample_size
        if(settings$outliers_removed) {
          missing_cases <- missing_cases - settings$outliers_count
        }
        
        print(paste("DEBUG - calculated missing_cases:", missing_cases))
        
        # Get missing data breakdown by variable
        missing_breakdown <- create_missing_data_breakdown(analysis_results$original_data, settings)
        
        processed_output <- c(processed_output,
          sprintf("Original dataset sample size: %d", settings$original_n),
          if(settings$outliers_removed) {
            sprintf("Standardized residual outliers (|SR| > %.1f) removed: %d cases", 
                    settings$outliers_threshold, settings$outliers_count)
          },
          if(missing_cases > 0) {
            c(
              sprintf("Missing data: %d cases with missing data (listwise) were excluded from moderation analysis", missing_cases),
              if(!is.null(missing_breakdown)) {
                c("Missing data by variable:", missing_breakdown)
              }
            )
          },
          sprintf("Final sample size: %d", sample_size)
        )
      } else if(grepl("^Test\\(s\\) of highest order", line)) {
        processed_output <- c(processed_output,
          "<br><strong>INTERACTION TEST</strong>",
          "<em>Tests whether the moderation effect is statistically significant:</em>",
          "<em>- R2-chng: Amount of additional variance explained by the interaction</em>",
          "<em>- p-value: Statistical significance of the moderation effect</em>",
          "<span style='color: red;'>",
          line,
          filtered_output[which(filtered_output == line) + 1],
          gsub("(\\s+[0-9.]+$)", "<strong>\\1</strong>", 
               filtered_output[which(filtered_output == line) + 2]),
          "</span>"
        )
        skip_next <- 2
      } else if(grepl("^Model Summary:", line)) {
        processed_output <- c(processed_output,
          "<br><strong>MODEL FIT STATISTICS</strong>",
          "<em>This section shows how well the overall model fits the data:</em>",
          "<em>- R-squared indicates the proportion of variance explained</em>",
          "<em>- F-test shows if the model is significantly better than no predictors</em>",
          "<br>",
          line)
      } else if(grepl("^Model:", line)) {
        processed_output <- c(processed_output,
          "<br><strong>REGRESSION COEFFICIENTS</strong>",
          "<em>Key statistics for each predictor in the model:</em>",
          "<em>- Coefficient (coeff): The strength and direction of relationships</em>",
          "<em>- p-value (p): Statistical significance (p < .05 typically considered significant)</em>",
          "<em>- LLCI/ULCI: 95% confidence intervals (significant if they don't contain zero)</em>",
          "<br>",
          line)
      } else if(grepl("^Covariance matrix", line)) {
        processed_output <- c(processed_output,
          "<br><strong>COVARIANCE MATRIX</strong>",
          "<em>Shows relationships between parameter estimates:</em>",
          "<em>- Large covariances between predictor and moderator might indicate estimation problems</em>",
          "<em>- Some relationship between interaction term and its components is normal</em>",
          "<br>",
          line)
      } else if(grepl("^Conditional effects", line)) {
        processed_output <- c(processed_output,
          "<br><strong>CONDITIONAL EFFECTS</strong>",
          "<em>Shows how the focal predictor's effect changes at different moderator values:</em>",
          "<em>- Effect: Strength of relationship at each moderator level</em>",
          "<em>- Significant effects (p < .05) indicate meaningful relationships at that level</em>",
          "<br>",
          line)
      } else if(grepl("^Moderator value\\(s\\) defining Johnson-Neyman", line)) {
        processed_output <- c(processed_output,
          "<br><strong>JOHNSON-NEYMAN SIGNIFICANCE REGION</strong>",
          "<em>Identifies the specific values of the moderator where the relationship between the predictor and outcome transitions between significant and non-significant:</em>",
          "<em>- The 'Value' shows the moderator level where this transition occurs</em>",
          "<em>- '% below' and '% above' indicate the proportion of cases in each significance region</em>",
          "<em>- A visual representation of these regions is provided in the Johnson-Neyman plot below</em>",
          "<em>- When bootstrapping is used, these values are based on normal-theory tests</em>",
          "<br>",
          line)
      } else if(grepl("^There are no statistical significance", line)) {
        processed_output <- c(processed_output,
          "<br><strong>JOHNSON-NEYMAN SIGNIFICANCE REGION</strong>",
          "<em>Identifies the specific values of the moderator where the relationship between the predictor and outcome transitions between significant and non-significant:</em>",
          "<em>- The relationship between predictor and outcome can change significance across moderator values</em>",
          "<em>- When no transition points are found, the relationship is consistently significant or non-significant</em>",
          "<em>- A visual representation is provided in the Johnson-Neyman plot below</em>",
          "<em>- When bootstrapping is used, these values are based on normal-theory tests</em>",
          "<br>",
          line
        )
      } else {
        processed_output <- c(processed_output, line)
      }
    }
    
    # Combine settings, bivariate correlations, and processed output
    paste(
      "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
      paste(c(output_text, "", bivariate_cor, "", processed_output), collapse = "<br>"),  # Add bivariate correlations
      "</div>"
    )
  }

  # Modify the dataset handling at the top of server
  rv <- reactiveValues(
    original_dataset = NULL,
    current_dataset = NULL,
    outliers_info = NULL,
    analysis_results = NULL,
    current_model = NULL  # Add this to store the current model
  )
  
  # Update dataset handling
  observeEvent(input$data_file, {
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$datapath)
    
    if (ext == "sav") {
      data <- read_sav(input$data_file$datapath)
    } else if (ext == "csv") {
      data <- read.csv(input$data_file$datapath)
    } else {
      stop("Invalid file type. Please upload a CSV or SAV file.")
    }
    
    rv$original_dataset <- data
    rv$current_dataset <- data
    print(paste("DEBUG - Dataset loaded with", nrow(data), "rows"))  # Add this line
    rv$analysis_results <- NULL  # Reset analysis results when new file is loaded
  })
  
  # Function to identify outliers
  identify_outliers <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$moderator_var)
    
    # Create model formula
    formula_terms <- c(input$outcome_var, "~", input$predictor_var, "*", input$moderator_var)
    if (length(input$covariates) > 0) {
      formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
    }
    model_formula <- as.formula(paste(formula_terms, collapse = " "))
    
    # Check if outcome is binary
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    if(outcome_is_binary) {
      # For binary outcomes, outlier removal is not standard practice
      # Return empty list to disable outlier removal
      return(list(
        cases = integer(0),
        values = numeric(0),
        count = 0,
        percentage = 0,
        is_binary = TRUE
      ))
    } else {
      # For continuous outcomes, use linear regression
      model <- lm(model_formula, data = rv$original_dataset)
      std_resid <- rstandard(model)
      
      # Identify outliers
      outlier_cases <- which(abs(std_resid) > input$residual_threshold)
      outlier_values <- std_resid[outlier_cases]
      
      return(list(
        cases = outlier_cases,
        values = outlier_values,
        count = length(outlier_cases),
        percentage = length(outlier_cases) / length(std_resid) * 100,
        is_binary = FALSE
      ))
    }
  })
  
  # Update dataset reactive to handle both original and filtered cases
  dataset <- reactive({
    req(rv$current_dataset)
    rv$current_dataset
  })
  
  # Analysis with original dataset
  original_analysis <- eventReactive(input$run_analysis, {
    # Add validation before proceeding
    validate(
      need(rv$original_dataset, "Dataset not loaded"),
      need(input$outcome_var, "Outcome variable not selected"),
      need(input$predictor_var, "Predictor variable not selected"),
      need(input$moderator_var, "Moderator variable not selected"),
      need(input$outcome_var != input$predictor_var && 
           input$outcome_var != input$moderator_var && 
           input$predictor_var != input$moderator_var, 
           "Variables must be different from each other")
    )
    
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$moderator_var)
    source("process.R")
    
    withProgress(message = 'Running analysis...', value = 0, {
      print("Running analysis with original dataset")
      rv$outliers_info <- NULL
      
      # Store settings at time of analysis
      analysis_settings <- list(
        centering = input$centering,
        use_bootstrap = input$use_bootstrap,
        boot_samples = if(input$use_bootstrap) input$boot_samples else NULL,
        conditioning_values = input$conditioning_values,
        hc_method = input$hc_method,
        
        dataset_name = tools::file_path_sans_ext(basename(input$data_file$name)),
        original_n = nrow(rv$original_dataset),
        outliers_removed = FALSE,
        outliers_count = 0,
        outliers_threshold = input$residual_threshold,
        predictor_var = input$predictor_var,
        outcome_var = input$outcome_var,
        moderator_var = input$moderator_var,
        covariates = if(length(input$covariates) > 0) input$covariates else NULL
      )
      
      # Prepare process arguments
      # Get all variables needed for analysis
      all_vars_orig <- c(input$outcome_var, input$predictor_var, input$moderator_var)
      if(length(input$covariates) > 0) {
        all_vars_orig <- c(all_vars_orig, input$covariates)
      }
      
      # Check complete cases for original dataset
      complete_cases_orig <- complete.cases(rv$original_dataset[all_vars_orig])
      n_complete_orig <- sum(complete_cases_orig)
      
      # Validate that we have enough complete cases
      if(n_complete_orig < 3) {
        stop(sprintf("Only %d complete cases available in the original dataset. This is insufficient for moderation analysis. Please check for missing data in your variables.", n_complete_orig))
      }
      
      # Use complete cases only to avoid PROCESS errors
      process_data_orig <- rv$original_dataset[complete_cases_orig, ]
      
      process_args <- list(
        data = process_data_orig,
        y = input$outcome_var,
        x = input$predictor_var,
        w = input$moderator_var,
        model = 1,
        center = as.numeric(input$centering),
        moments = ifelse(input$conditioning_values == "0", 1, 0),
        jn = 1,
        modelbt = if(input$use_bootstrap) 1 else 0,
        covcoeff = 1,
        cov = if(length(input$covariates) > 0) input$covariates else "xxxxx",
        hc = if(input$hc_method == "none") 5 else as.numeric(input$hc_method),
        boot = if(input$use_bootstrap) input$boot_samples else 5000
      )
      
      # Run PROCESS and capture output
      process_output <- capture.output({
        result <- do.call(process, process_args)
      })
      
      # Extract coefficients from the text output
      coef_start <- which(grepl("^Model:", process_output))
      coefficients <- NULL
      
      if (length(coef_start) > 0) {
        # Find where the coefficient section ends (blank line or next section)
        coef_end <- coef_start + 2
        for(i in (coef_start + 3):min(length(process_output), coef_start + 20)) {
          if(i > length(process_output)) break
          line <- trimws(process_output[i])
          # Stop at blank line, "Product terms key:", or next section header
          if(line == "" || grepl("^Product terms key:", line) || 
             grepl("^Covariance matrix", line) || grepl("^Test\\(s\\)", line)) {
            coef_end <- i - 1
            break
          }
        }
        
        coef_lines <- process_output[(coef_start + 3):coef_end]
        print("Coefficient lines found in Model section:")
        print(coef_lines)
        
        coef_data <- numeric(4)
        names(coef_data) <- c("constant", "predictor", "moderator", "interaction")
        
        for(i in seq_along(coef_lines)) {
          parts <- strsplit(trimws(coef_lines[i]), "\\s+")[[1]]
          if(length(parts) >= 2) {
            var_name <- parts[1]
            coef_value <- as.numeric(parts[2])
            if(grepl("constant|Intercept", var_name, ignore.case = TRUE)) {
              coef_data[1] <- coef_value
            } else if(grepl(paste0("^", input$predictor_var, "$"), var_name)) {
              coef_data[2] <- coef_value
            } else if(grepl(paste0("^", input$moderator_var, "$"), var_name)) {
              coef_data[3] <- coef_value
            } else if(grepl("^Int_1", var_name)) {
              coef_data[4] <- coef_value
            }
          }
        }
        
        print("Extracted coefficients with names:")
        print(coef_data)
        
        coefficients <- coef_data
      }
      
      # Get variables in order: outcome, predictor, moderator, then covariates
      vars <- c(input$outcome_var, input$predictor_var, input$moderator_var)
      if(length(input$covariates) > 0) {
        vars <- c(vars, input$covariates)
      }
      
      # Create correlation data at time of analysis
      complete_data <- rv$original_dataset[complete.cases(rv$original_dataset[vars]), ]
      correlation_info <- list(
        table = create_correlation_matrix(complete_data, vars),
        n = nrow(complete_data),
        dataset_type = "ORIGINAL"
      )
      
      list(
        output = process_output, 
        data_used = process_data_orig,  # Use processed data (complete cases)
        original_data = rv$original_dataset,  # Keep original for bivariate correlations
        coefficients = coefficients,
        correlation_info = correlation_info,
        settings = analysis_settings  # Add settings to the output
      )
    })
  }, ignoreNULL = TRUE)  # Only run when button is actually clicked
  
  # Analysis with outliers removed
  outliers_analysis <- eventReactive(input$run_analysis_no_outliers, {
    # Add validation before proceeding
    validate(
      need(rv$original_dataset, "Dataset not loaded"),
      need(input$outcome_var, "Outcome variable not selected"),
      need(input$predictor_var, "Predictor variable not selected"),
      need(input$moderator_var, "Moderator variable not selected"),
      need(input$outcome_var != input$predictor_var && 
           input$outcome_var != input$moderator_var && 
           input$predictor_var != input$moderator_var, 
           "Variables must be different from each other")
    )
    
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$moderator_var)
    source("process.R")
    
    withProgress(message = 'Running analysis...', value = 0, {
      print("Running analysis with outliers removed")
      outliers <- identify_outliers()
      # Store both outliers info and the threshold used
      rv$outliers_info <- list(
        count = outliers$count,
        threshold = input$residual_threshold  # Store threshold at time of analysis
      )
      reduced_data <- rv$original_dataset[-outliers$cases, ]
      
      # Check for complete cases after outlier removal
      # Get all variables needed for analysis
      all_vars <- c(input$outcome_var, input$predictor_var, input$moderator_var)
      if(length(input$covariates) > 0) {
        all_vars <- c(all_vars, input$covariates)
      }
      
      # Check complete cases
      complete_cases <- complete.cases(reduced_data[all_vars])
      n_complete <- sum(complete_cases)
      
      # Validate that we have enough complete cases
      if(n_complete < 3) {
        stop(sprintf("After removing outliers, only %d complete cases remain. This is insufficient for moderation analysis. Please check for missing data in your covariates or use a different outlier threshold.", n_complete))
      }
      
      # Store settings at time of analysis
      analysis_settings <- list(
        centering = input$centering,
        use_bootstrap = input$use_bootstrap,
        boot_samples = if(input$use_bootstrap) input$boot_samples else NULL,
        conditioning_values = input$conditioning_values,
        hc_method = input$hc_method,
        
        dataset_name = tools::file_path_sans_ext(basename(input$data_file$name)),
        original_n = nrow(rv$original_dataset),
        outliers_removed = TRUE,
        outliers_count = outliers$count,
        outliers_threshold = input$residual_threshold,
        predictor_var = input$predictor_var,
        outcome_var = input$outcome_var,
        moderator_var = input$moderator_var,
        covariates = if(length(input$covariates) > 0) input$covariates else NULL
      )
      
      # Prepare process arguments
      # Use complete cases only to avoid PROCESS errors
      process_data <- reduced_data[complete_cases, ]
      
      process_args <- list(
        data = process_data,
        y = input$outcome_var,
        x = input$predictor_var,
        w = input$moderator_var,
        model = 1,
        center = as.numeric(input$centering),
        moments = ifelse(input$conditioning_values == "0", 1, 0),
        jn = 1,
        modelbt = if(input$use_bootstrap) 1 else 0,
        covcoeff = 1,
        cov = if(length(input$covariates) > 0) input$covariates else "xxxxx",
        hc = if(input$hc_method == "none") 5 else as.numeric(input$hc_method),
        boot = if(input$use_bootstrap) input$boot_samples else 5000
      )
      
      # Run PROCESS and capture output
      process_output <- capture.output({
        result <- do.call(process, process_args)
      })
      
      # Extract coefficients from the text output
      coef_start <- which(grepl("^Model:", process_output))
      coefficients <- NULL
      
      if (length(coef_start) > 0) {
        # Find where the coefficient section ends (blank line or next section)
        coef_end <- coef_start + 2
        for(i in (coef_start + 3):min(length(process_output), coef_start + 20)) {
          if(i > length(process_output)) break
          line <- trimws(process_output[i])
          # Stop at blank line, "Product terms key:", or next section header
          if(line == "" || grepl("^Product terms key:", line) || 
             grepl("^Covariance matrix", line) || grepl("^Test\\(s\\)", line)) {
            coef_end <- i - 1
            break
          }
        }
        
        coef_lines <- process_output[(coef_start + 3):coef_end]
        print("Coefficient lines found in Model section:")
        print(coef_lines)
        
        coef_data <- numeric(4)
        names(coef_data) <- c("constant", "predictor", "moderator", "interaction")
        
        for(i in seq_along(coef_lines)) {
          parts <- strsplit(trimws(coef_lines[i]), "\\s+")[[1]]
          if(length(parts) >= 2) {
            var_name <- parts[1]
            coef_value <- as.numeric(parts[2])
            if(grepl("constant|Intercept", var_name, ignore.case = TRUE)) {
              coef_data[1] <- coef_value
            } else if(grepl(paste0("^", input$predictor_var, "$"), var_name)) {
              coef_data[2] <- coef_value
            } else if(grepl(paste0("^", input$moderator_var, "$"), var_name)) {
              coef_data[3] <- coef_value
            } else if(grepl("^Int_1", var_name)) {
              coef_data[4] <- coef_value
            }
          }
        }
        
        print("Extracted coefficients with names:")
        print(coef_data)
        
        coefficients <- coef_data
      }
      
      # Get variables in order: outcome, predictor, moderator, then covariates
      vars <- c(input$outcome_var, input$predictor_var, input$moderator_var)
      if(length(input$covariates) > 0) {
        vars <- c(vars, input$covariates)
      }
      
      # Create correlation data at time of analysis
      complete_data <- reduced_data[complete.cases(reduced_data[vars]), ]
      correlation_info <- list(
        table = create_correlation_matrix(complete_data, vars),
        n = nrow(complete_data),
        dataset_type = "REDUCED"
      )
      
      list(
        output = process_output, 
        data_used = process_data,  # Use processed data (complete cases after outlier removal)
        original_data = rv$original_dataset,  # Keep original for bivariate correlations
        coefficients = coefficients,
        correlation_info = correlation_info,
        settings = analysis_settings  # Add settings to the output
      )
    })
  }, ignoreNULL = TRUE)  # Only run when button is actually clicked
  
  # Combined results reactive
  analysis_results <- reactive({
    if (input$run_analysis > 0 && input$run_analysis >= input$run_analysis_no_outliers) {
      original_analysis()
    } else {
      outliers_analysis()
    }
  })
  
  # Dynamically generate variable selectors
  output$variable_selectors <- renderUI({
    req(rv$original_dataset)  # Use original dataset for variable options
    vars <- names(rv$original_dataset)
    tagList(
      selectInput("outcome_var", "Outcome Variable", 
                 choices = c("Select variable" = "", vars), 
                 selected = ""),
      selectInput("predictor_var", "Predictor Variable", 
                 choices = c("Select variable" = "", vars), 
                 selected = ""),
      selectInput("moderator_var", "Moderator Variable", 
                 choices = c("Select variable" = "", vars), 
                 selected = ""),
      selectInput("covariates", "Covariates (optional)", vars, multiple = TRUE)
    )
  })
  
  # Add a reactive value to track JN plot availability
  jn_available <- reactiveVal(TRUE)
  
  # Helper function to detect if a variable is binary (0/1 or only 2 unique values)
  is_binary_variable <- function(data, var_name) {
    if(is.null(data) || is.null(var_name) || !var_name %in% names(data)) {
      return(FALSE)
    }
    var_data <- data[[var_name]]
    var_data <- var_data[!is.na(var_data)]
    unique_vals <- unique(var_data)
    # Check if only 2 unique values
    if(length(unique_vals) == 2) {
      return(TRUE)
    }
    return(FALSE)
  }
  
  # Helper function to detect if outcome is binary from PROCESS output
  is_binary_outcome <- reactive({
    req(analysis_results())
    process_output <- analysis_results()$output
    # Check for "Coding of binary Y for logistic regression analysis" in output
    any(grepl("Coding of binary Y for logistic regression", process_output, ignore.case = TRUE))
  })
  
  # Helper function to check if moderator is dichotomous
  is_dichotomous_moderator <- reactive({
    req(rv$original_dataset, input$moderator_var)
    is_binary_variable(rv$original_dataset, input$moderator_var)
  })
  
  # Output to track if outcome is continuous (for conditionalPanel)
  output$outcome_is_continuous <- reactive({
    req(rv$original_dataset, input$outcome_var)
    !is_binary_variable(rv$original_dataset, input$outcome_var)
  })
  outputOptions(output, "outcome_is_continuous", suspendWhenHidden = FALSE)
  
  # Update JN availability based on moderator being dichotomous
  observe({
    req(rv$original_dataset, input$moderator_var)
    if(is_dichotomous_moderator()) {
      jn_available(FALSE)
    } else {
      jn_available(TRUE)
    }
  })

  # Diagnostic check functions
  check_normality <- function(model) {
    # Shapiro-Wilk test
    sw_test <- shapiro.test(residuals(model))
    # Create Q-Q plot and capture it
    qq_plot <- ggplot(data.frame(residuals = residuals(model)), aes(sample = residuals)) +
      stat_qq() + stat_qq_line() +
      theme_minimal() +
      labs(title = "Normal Q-Q Plot of Residuals")
    
    return(list(
      test = sw_test,
      plot = qq_plot,
      text = sprintf("Normality (Shapiro-Wilk): W = %.3f, p %s",
                    sw_test$statistic,
                    ifelse(sw_test$p.value < .001, "< .001",
                          sprintf("= %.3f", sw_test$p.value)))
    ))
  }
  
  test_homoscedasticity <- function(model) {
    # Breusch-Pagan test
    bp_test <- car::ncvTest(model)
    return(sprintf("Homoscedasticity (Breusch-Pagan): χ²(%d) = %.3f, p %s",
                  bp_test$Df,
                  bp_test$ChiSquare,
                  ifelse(bp_test$p < .001, "< .001", sprintf("= %.3f", bp_test$p))))
  }
  
  diagnostic_report <- function(model) {
    tryCatch({
      # Basic model diagnostics
      n <- nobs(model)
      
      # Check if model is logistic (glm with binomial family)
      is_logistic <- inherits(model, "glm") && model$family$family == "binomial"
      
      # Handle VIF calculation for moderation
      vif_result <- tryCatch({
        # Get model terms
        terms <- attr(terms(model), "term.labels")
        
        if(length(terms) > 1) {
          # Calculate VIF only for main effects (not interaction)
          main_effects <- terms[!grepl(":", terms)]  # Exclude interaction terms
          if(length(main_effects) > 1) {
            # Create model without interaction for VIF
            main_formula <- reformulate(main_effects, response = all.vars(formula(model))[1])
            
            # Use appropriate model type
            if(is_logistic) {
              main_model <- glm(main_formula, data = model$model, family = binomial())
            } else {
              main_model <- lm(main_formula, data = model$model)
            }
            
            vif_values <- suppressWarnings(car::vif(main_model))
            
            # Format VIF results
            sprintf("VIF for main effects: %s", 
                    paste(names(vif_values), sprintf("%.2f", vif_values), 
                    collapse = ", "))
          } else {
            "VIF not calculated (insufficient predictors)"
          }
        } else {
          "VIF not calculated (single predictor)"
        }
      }, error = function(e) {
        "VIF calculation unavailable"
      })
      
      # Return diagnostics
      c(
        sprintf("Sample size: %d", n),
        vif_result,
        if(is_logistic) {
          "Note: VIF calculated only for main effects, excluding interaction term. For binary outcomes, VIF interpretation is similar to linear regression."
        } else {
          "Note: VIF calculated only for main effects, excluding interaction term"
        }
      )
    }, error = function(e) {
      c(
        "Unable to compute some diagnostic measures",
        paste("Error details:", e$message)
      )
    })
  }

  # Modify the analysis_output renderUI
  output$analysis_output <- renderUI({
    req(analysis_results())
    HTML(create_formatted_output(analysis_results()))
  })
  
  # Replace both full_range_plot and spss_plot with a single slopes_plot
  output$slopes_plot <- renderPlot({
    req(analysis_results())
    coeffs <- analysis_results()$coefficients
    data_used <- analysis_results()$data_used  # Use stored data
    
    # Check if outcome is binary
    outcome_is_binary <- is_binary_outcome()
    
    # Get moderator levels based on stored data
    # For dichotomous moderators, use the two unique values
    if(is_dichotomous_moderator()) {
      moderator_levels <- sort(unique(data_used[[input$moderator_var]][!is.na(data_used[[input$moderator_var]])]))
      moderator_levels <- round(moderator_levels, input$decimal_places)
    } else {
      moderator_levels <- if(input$conditioning_values == "0") {
        mod_mean <- mean(data_used[[input$moderator_var]], na.rm = TRUE)
        mod_sd <- sd(data_used[[input$moderator_var]], na.rm = TRUE)
        values <- c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd)
        round(values, input$decimal_places)
      } else {
        values <- quantile(data_used[[input$moderator_var]], 
                          probs = c(0.16, 0.50, 0.84), 
                          na.rm = TRUE)
        round(values, input$decimal_places)
      }
    }
    
    # Create predictor sequence based on plot type
    if(input$slopes_type == "full") {
      pred_seq <- seq(
        min(data_used[[input$predictor_var]], na.rm = TRUE),
        max(data_used[[input$predictor_var]], na.rm = TRUE),
        length.out = 100
      )
    } else {  # restricted range
      if(input$conditioning_values == "0") {
        # Use Mean ± 1 SD for predictor when using SD conditioning
        predictor_mean <- mean(data_used[[input$predictor_var]], na.rm = TRUE)
        predictor_sd <- sd(data_used[[input$predictor_var]], na.rm = TRUE)
        pred_seq <- c(predictor_mean - predictor_sd, predictor_mean, predictor_mean + predictor_sd)
      } else {
        # Use matching percentiles for predictor when using percentile conditioning
        pred_seq <- quantile(data_used[[input$predictor_var]], 
                           probs = c(0.16, 0.50, 0.84), 
                           na.rm = TRUE)
      }
    }
    
    # Create plotting data frame
    plot_data <- expand.grid(
      Predictor = pred_seq,
      Moderator = moderator_levels
    )
    
    # Calculate predicted values
    if(outcome_is_binary) {
      # For binary outcomes, coefficients are log-odds
      # Convert to probabilities using plogis (inverse logit)
      log_odds <- coeffs["constant"] +
        coeffs["predictor"] * plot_data$Predictor +
        coeffs["moderator"] * plot_data$Moderator +
        coeffs["interaction"] * plot_data$Predictor * plot_data$Moderator
      plot_data$Outcome <- plogis(log_odds)  # Convert log-odds to probabilities
    } else {
      # For continuous outcomes, use linear prediction
      plot_data$Outcome <- coeffs["constant"] +
        coeffs["predictor"] * plot_data$Predictor +
        coeffs["moderator"] * plot_data$Moderator +
        coeffs["interaction"] * plot_data$Predictor * plot_data$Moderator
    }
    
    # Create plot with formatted moderator levels
    y_label_text <- if(outcome_is_binary) {
      if(input$y_label != "") paste(input$y_label, "(Probability)") else "Predicted Probability"
    } else {
      if(input$y_label != "") input$y_label else input$outcome_var
    }
    
    p <- ggplot(plot_data, aes(
      x = Predictor, 
      y = Outcome,
      color = if(input$use_color_lines) 
        factor(Moderator, labels = format(moderator_levels, nsmall = input$decimal_places)) else NULL,
      linetype = if(!input$use_color_lines) 
        factor(Moderator, labels = format(moderator_levels, nsmall = input$decimal_places)) else NULL
    )) +
      geom_line(size = 1) +
      {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
      {if(outcome_is_binary && !input$custom_y_axis) coord_cartesian(ylim = c(0, 1))} +
      labs(
        title = input$slopes_title,
        x = input$x_label,
        y = y_label_text,
        color = ifelse(input$moderator_label != "", 
                      input$moderator_label, 
                      paste0(input$moderator_var, " Levels")),
        linetype = if(!input$use_color_lines) 
                    paste0(input$moderator_var, " Levels") else NULL
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 14),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5)
      )
    
    print(p)
  })
  
  # Update the download handler for slopes plot
  output$download_slopes <- downloadHandler(
    filename = function() {
      paste0("slopes_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
    },
    content = function(file) {
      tryCatch({
        # Use the same code as slopes_plot but save instead of display
        req(analysis_results())
        coeffs <- analysis_results()$coefficients
        dataset <- analysis_results()$data_used  # Use stored data
        
        # Check if outcome is binary
        outcome_is_binary <- is_binary_outcome()
        
        # Get moderator levels based on stored data
        # For dichotomous moderators, use the two unique values
        if(is_dichotomous_moderator()) {
          moderator_levels <- sort(unique(dataset[[input$moderator_var]][!is.na(dataset[[input$moderator_var]])]))
          moderator_levels <- round(moderator_levels, input$decimal_places)
        } else {
          moderator_levels <- if(input$conditioning_values == "0") {
            mod_mean <- mean(dataset[[input$moderator_var]], na.rm = TRUE)
            mod_sd <- sd(dataset[[input$moderator_var]], na.rm = TRUE)
            c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd)
          } else {
            quantile(dataset[[input$moderator_var]], probs = c(0.16, 0.50, 0.84), na.rm = TRUE)
          }
          moderator_levels <- round(moderator_levels, input$decimal_places)
        }
        
        # Create predictor sequence based on plot type
        if(input$slopes_type == "full") {
          pred_seq <- seq(
            min(dataset[[input$predictor_var]], na.rm = TRUE),
            max(dataset[[input$predictor_var]], na.rm = TRUE),
            length.out = 100
          )
        } else {  # restricted range
          if(input$conditioning_values == "0") {
            # Use Mean ± 1 SD for predictor when using SD conditioning
            predictor_mean <- mean(dataset[[input$predictor_var]], na.rm = TRUE)
            predictor_sd <- sd(dataset[[input$predictor_var]], na.rm = TRUE)
            pred_seq <- c(predictor_mean - predictor_sd, predictor_mean, predictor_mean + predictor_sd)
          } else {
            # Use matching percentiles for predictor when using percentile conditioning
            pred_seq <- quantile(dataset[[input$predictor_var]], 
                               probs = c(0.16, 0.50, 0.84), 
                               na.rm = TRUE)
          }
        }
        
        # Create plotting data frame
        plot_data <- expand.grid(
          Predictor = pred_seq,
          Moderator = moderator_levels
        )
        
        # Calculate predicted values
        if(outcome_is_binary) {
          # For binary outcomes, coefficients are log-odds
          # Convert to probabilities using plogis (inverse logit)
          log_odds <- coeffs["constant"] +
            coeffs["predictor"] * plot_data$Predictor +
            coeffs["moderator"] * plot_data$Moderator +
            coeffs["interaction"] * plot_data$Predictor * plot_data$Moderator
          plot_data$Outcome <- plogis(log_odds)  # Convert log-odds to probabilities
        } else {
          # For continuous outcomes, use linear prediction
          plot_data$Outcome <- coeffs["constant"] +
            coeffs["predictor"] * plot_data$Predictor +
            coeffs["moderator"] * plot_data$Moderator +
            coeffs["interaction"] * plot_data$Predictor * plot_data$Moderator
        }
        
        # Create and save plot
        y_label_text <- if(outcome_is_binary) {
          if(input$y_label != "") paste(input$y_label, "(Probability)") else "Predicted Probability"
        } else {
          if(input$y_label != "") input$y_label else input$outcome_var
        }
        
        p <- ggplot(plot_data, aes(
          x = Predictor, 
          y = Outcome,
          color = if(input$use_color_lines) factor(Moderator) else NULL,
          linetype = if(!input$use_color_lines) factor(Moderator) else NULL
        )) +
          geom_line(size = 1) +
          {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
          {if(outcome_is_binary && !input$custom_y_axis) coord_cartesian(ylim = c(0, 1))} +
          labs(
            title = input$slopes_title,
            x = input$x_label,
            y = y_label_text,
            color = ifelse(input$moderator_label != "", 
                          input$moderator_label, 
                          paste0(input$moderator_var, " Levels")),
            linetype = if(!input$use_color_lines) 
                        paste0(input$moderator_var, " Levels") else NULL
          ) +
          theme_minimal() +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
        
        ggsave(file, plot = p, device = "jpg", width = 10, height = 8, dpi = 600)
      }, error = function(e) {
        print(paste("Error saving slopes plot:", e$message))
      })
    }
  )
  
  # Update the JN Plot code to use the availability flag
  output$jn_plot <- renderPlot({
    req(analysis_results())
    process_output <- analysis_results()$output
    dataset <- analysis_results()$data_used  # Use stored data
    
    # Check JN availability
    if(!jn_available()) {
      # Create an informative plot for categorical moderators
      plot.new()
      text(0.5, 0.5, 
           "Johnson-Neyman plot not available for categorical moderators.\nUse the simple slopes plots instead.", 
           cex = 1.2)
      return()
    }
    
    tryCatch({
      start_idx <- which(grepl("Conditional effect of focal predictor", process_output))
      data_start <- start_idx + 2
      end_idx <- which(grepl("^\\s*$", process_output[data_start:length(process_output)]))[1] + data_start - 1
      
      if (!is.na(end_idx) && end_idx > data_start) {
        data_lines <- process_output[data_start:end_idx]
        jn_data <- read.table(text = paste(data_lines, collapse = "\n"),
                             col.names = c("Moderator", "Effect", "se", "t", "p", "LLCI", "ULCI"),
                             stringsAsFactors = FALSE)
        
        # Create plot with explicit significance variable
        jn_data$significant <- jn_data$p < 0.05
        
        # Find the transition point (where p is closest to 0.05)
        transition_point <- jn_data$Moderator[which.min(abs(jn_data$p - 0.05))]
        
        p <- ggplot(jn_data, aes(x = Moderator, y = Effect)) +
          geom_ribbon(aes(ymin = LLCI, ymax = ULCI, fill = !significant), alpha = 0.4) +
          scale_fill_manual(values = if(input$use_color_lines) 
                            c(`TRUE` = "pink", `FALSE` = "lightblue") else 
                            c(`TRUE` = "grey70", `FALSE` = "grey50"),
                          labels = c(`TRUE` = "n.s.", `FALSE` = "p < .05"),
                          name = "") +
          geom_line(size = 1, 
                   color = if(input$use_color_lines) "blue" else "black") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          geom_vline(xintercept = transition_point,  # Use the calculated transition point
                    linetype = "dashed", 
                    color = if(input$use_color_lines) "cyan" else "grey40") +
          theme_minimal() +
          labs(title = "Johnson-Neyman Plot",
               x = input$moderator_label,
               y = paste("Effect of", input$x_label)) +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
        
        print(p)
      }
    }, error = function(e) {
      print(paste("Error in JN plot:", e$message))
    })
  })
  
  # Observe JN availability to enable/disable download button
  observe({
    shinyjs::toggleState("download_jn", condition = jn_available())
  })
  
  # Update the JN plot download handler
  output$download_jn <- downloadHandler(
    filename = function() {
      paste0("jn_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
    },
    content = function(file) {
      tryCatch({
        req(analysis_results())
        process_output <- analysis_results()$output
        dataset <- analysis_results()$data_used  # Use stored data
        
        # Find the JN data section
        start_idx <- which(grepl("Conditional effect of focal predictor", process_output))
        if (length(start_idx) > 0) {
          data_start <- start_idx + 2
          end_idx <- which(grepl("^\\s*$", process_output[data_start:length(process_output)]))[1] + data_start - 1
          
          if (!is.na(end_idx) && end_idx > data_start) {
            data_lines <- process_output[data_start:end_idx]
            jn_data <- read.table(text = paste(data_lines, collapse = "\n"),
                                 col.names = c("Moderator", "Effect", "se", "t", "p", "LLCI", "ULCI"),
                                 stringsAsFactors = FALSE)
            
            # Create plot with explicit significance variable
            jn_data$significant <- jn_data$p < 0.05
            
            # Find the transition point
            transition_point <- jn_data$Moderator[which.min(abs(jn_data$p - 0.05))]
            
            # Create plot
            p <- ggplot(jn_data, aes(x = Moderator, y = Effect)) +
              geom_ribbon(aes(ymin = LLCI, ymax = ULCI, fill = !significant), alpha = 0.4) +
              scale_fill_manual(values = if(input$use_color_lines) 
                                c(`TRUE` = "pink", `FALSE` = "lightblue") else 
                                c(`TRUE` = "grey70", `FALSE` = "grey50"),
                              labels = c(`TRUE` = "n.s.", `FALSE` = "p < .05"),
                              name = "") +
              geom_line(size = 1, 
                       color = if(input$use_color_lines) "blue" else "black") +
              geom_hline(yintercept = 0, linetype = "dashed") +
              geom_vline(xintercept = transition_point,
                        linetype = "dashed", 
                        color = if(input$use_color_lines) "cyan" else "grey40") +
              theme_minimal() +
              labs(title = "Johnson-Neyman Plot",
                   x = input$moderator_label,
                   y = paste("Effect of", input$x_label)) +
              theme(
                text = element_text(size = 14),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 14),
                plot.title = element_text(size = 18, hjust = 0.5),
                legend.title = element_blank(),
                legend.text = element_text(size = 14),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black", linewidth = 0.5),
                axis.ticks = element_line(color = "black", linewidth = 0.5)
              )
            
            # Save the plot
            ggsave(file, plot = p, device = "jpg", width = 10, height = 8, dpi = 600)
          }
        }
      }, error = function(e) {
        print(paste("Error saving JN plot:", e$message))
      })
    }
  )
  
  # Add download handler for results
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("process_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(analysis_results())
      output_content <- create_formatted_output(analysis_results())
      
      # Write the content with HTML wrapper
      writeLines(sprintf('
        <!DOCTYPE html>
        <html>
        <head>
          <style>
            body { font-family: Courier, monospace; white-space: pre-wrap; padding: 20px; }
            table { border-collapse: collapse; }
            th, td { border: 1px solid #dee2e6; padding: 8px; }
          </style>
        </head>
        <body>
          %s
        </body>
        </html>
      ', output_content), file)
    },
    contentType = "text/html"
  )
  
  # Update the diagnostic plots to use original dataset
  output$qq_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$moderator_var)
    
    tryCatch({
      # Create model formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var, "*", input$moderator_var)
      if (length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        # For binary outcomes, return NULL to hide the plot
        return(NULL)
      } else {
        # Always use original dataset for assumption checks
        model <- lm(model_formula, data = rv$original_dataset)
        
        # Create Q-Q plot
        ggplot(data.frame(residuals = rstandard(model)), aes(sample = residuals)) +
          stat_qq() + 
          stat_qq_line() +
          theme_minimal() +
          labs(title = "Normal Q-Q Plot",
               x = "Theoretical Quantiles",
               y = "Sample Quantiles") +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating Q-Q plot", cex = 1.2)
    })
  })
  
  # Residuals vs Fitted Plot
  output$residual_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$moderator_var)
    
    tryCatch({
      # Create model formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var, "*", input$moderator_var)
      if (length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        # For binary outcomes, use logistic regression
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        fitted_values <- fitted(model)  # These are probabilities
        pearson_resid <- residuals(model, type = "pearson")
        # Note: For binary outcomes, we don't highlight "outliers" in the same way
        # as they have different interpretation
        
        plot_data <- data.frame(
          fitted = fitted_values,
          residuals = pearson_resid
        )
        
        ggplot(plot_data, aes(x = fitted, y = residuals)) +
          geom_point(alpha = 0.6, color = "black") +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted (Logistic Regression)",
               x = "Fitted probabilities",
               y = "Pearson residuals",
               subtitle = "Note: For binary outcomes, residuals show different patterns than linear regression") +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            legend.position = "none",
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      } else {
        # Fit model using original dataset
        model <- lm(model_formula, data = rv$original_dataset)
        
        # Create diagnostic data
        fitted_values <- fitted(model)
        residuals <- residuals(model)
        
        # Add outlier information
        std_resid <- rstandard(model)
        is_outlier <- abs(std_resid) > input$residual_threshold
        
        # Create plot data
        plot_data <- data.frame(
          fitted = fitted_values,
          residuals = residuals,
          is_outlier = is_outlier
        )
        
        # Create enhanced residuals vs fitted plot
        ggplot(plot_data, aes(x = fitted, y = residuals)) +
          geom_point(aes(color = is_outlier), alpha = 0.6) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted",
               x = "Fitted values",
               y = "Residuals") +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating residuals plot", cex = 1.2)
    })
  })
  
  # Scale-Location Plot
  output$scale_location_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$moderator_var)
    
    tryCatch({
      # Create model formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var, "*", input$moderator_var)
      if (length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        # For binary outcomes, return NULL to hide the plot
        return(NULL)
      } else {
        # Fit model using original dataset
        model <- lm(model_formula, data = rv$original_dataset)
        
        # Create diagnostic data
        fitted_values <- fitted(model)
        std_residuals <- rstandard(model)
        sqrt_abs_resid <- sqrt(abs(std_residuals))
        
        # Add outlier information
        is_outlier <- abs(std_residuals) > input$residual_threshold
        
        # Create plot data
        plot_data <- data.frame(
          fitted = fitted_values,
          sqrt_abs_resid = sqrt_abs_resid,
          is_outlier = is_outlier
        )
        
        # Create enhanced scale-location plot
        ggplot(plot_data, aes(x = fitted, y = sqrt_abs_resid)) +
          geom_point(aes(color = is_outlier), alpha = 0.6) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          theme_minimal() +
          labs(title = "Scale-Location Plot",
               x = "Fitted values",
               y = expression(sqrt("|Standardized residuals|"))) +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating scale-location plot", cex = 1.2)
    })
  })
  
  # Add this reactive for outlier summary
  outlier_summary <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$moderator_var)
    
    tryCatch({
        # Create model formula
        formula_terms <- c(input$outcome_var, "~", input$predictor_var, "*", input$moderator_var)
        if (length(input$covariates) > 0) {
            formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
        }
        model_formula <- as.formula(paste(formula_terms, collapse = " "))
        
        # Check if outcome is binary
        outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
        
        if(outcome_is_binary) {
          # For binary outcomes, standardized residuals from linear regression are inappropriate
          return(c(
            "<strong>Standardized Residual Analysis:</strong>",
            "<em>Note: Your outcome variable is binary (0/1).</em>",
            "<br>",
            "<strong>Important:</strong> Standardized residuals from linear regression are not appropriate for binary outcomes.",
            "For binary outcomes:",
            "<ul>",
            "<li>PROCESS uses logistic regression, which has different assumptions</li>",
            "<li>Residuals in logistic regression are not normally distributed</li>",
            "<li>Outlier removal based on residuals is not standard practice for binary outcomes</li>",
            "<li>Large residuals may indicate model misspecification rather than outliers</li>",
            "</ul>",
            "<strong>Recommendation:</strong> Do not remove cases based on residuals for binary outcomes.",
            "If you have concerns about specific cases, examine them individually for data entry errors or other issues."
          ))
        }
        
        # For continuous outcomes, use linear regression
        model <- lm(model_formula, data = rv$original_dataset)
        
        # Get standardized residuals
        sresid <- rstandard(model)
        
        # Find outliers
        outliers <- which(abs(sresid) > input$residual_threshold)
        
        if(length(outliers) > 0) {
            # Sort outliers by absolute value of residuals
            sorted_indices <- order(-abs(sresid[outliers]))
            sorted_cases <- outliers[sorted_indices]
            sorted_resids <- sresid[sorted_cases]
            
            # Create case summaries for all cases
            case_summaries <- sapply(seq_along(sorted_cases), function(i) {
                sprintf("Case %d: SR = %.3f", sorted_cases[i], sorted_resids[i])
            })
            
            # Create summary text with scrollable div for cases
            c(
                "Standardized Residual Analysis:",
                sprintf("Cases exceeding threshold (|SR| > %.1f):", input$residual_threshold),
                sprintf("Number of cases: %d (%.1f%%)", 
                        length(outliers),
                        100 * length(outliers) / length(sresid)),
                "Top cases (sorted by |SR|):",
                paste0('<div style="max-height: 100px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; margin: 5px 0;">', 
                      paste(case_summaries, collapse = "<br>"),
                      '</div>')
            )
        } else {
            c(
                "Standardized Residual Analysis:",
                sprintf("No cases exceed the threshold (|SR| > %.1f)", input$residual_threshold)
            )
        }
    }, error = function(e) {
        c(
            "Standardized Residual Analysis:",
            "Unable to compute residuals. Please check that all variables are numeric.",
            paste("Error details:", e$message)
        )
    })
  })
  
  # Modify the assumption_details renderUI to use the new reactive
  output$assumption_details <- renderUI({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$moderator_var)
    
    tryCatch({
      # Create model formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var, "*", input$moderator_var)
      if (length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        # For binary outcomes, use logistic regression
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        
        # Get outlier summary
        outlier_text <- paste(outlier_summary(), collapse = "<br>")
        
        # For binary outcomes, skip normality and homoscedasticity tests
        # These assumptions don't apply to logistic regression
        diagnostics <- diagnostic_report(model)
        
        output_text <- paste(
          "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
          "<strong>Note: Binary Outcome Detected</strong><br>",
          "<em>Your outcome variable is binary (0/1). PROCESS will use logistic regression for this analysis.</em><br><br>",
          "<strong>Important:</strong> Standard regression assumptions (normality, homoscedasticity) do not apply to logistic regression.<br>",
          "For binary outcomes, different diagnostic approaches are needed:<br>",
          "<ul>",
          "<li><strong>Linearity:</strong> Check linearity of continuous predictors with the logit of the outcome</li>",
          "<li><strong>Influential observations:</strong> Review leverage values and Cook's distance (not shown here)</li>",
          "<li><strong>Model fit:</strong> Use pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke) shown in PROCESS output</li>",
          "<li><strong>Multicollinearity:</strong> VIF can still be calculated for predictors</li>",
          "</ul><br>",
          outlier_text,
          "<br><br>",
          "<strong>Additional Diagnostics:</strong><br>",
          paste(diagnostics, collapse = "<br>"),
          "<br><em>Note: VIF calculated for main effects only. For binary outcomes, focus on model fit statistics and residual patterns rather than normality/homoscedasticity.</em>",
          "</div>",
          sep = ""
        )
      } else {
        # For continuous outcomes, use linear regression
        model <- lm(model_formula, data = rv$original_dataset)
        
        # Get outlier summary
        outlier_text <- paste(outlier_summary(), collapse = "<br>")
        
        # Run other diagnostics
        normality <- check_normality(model)
        homoscedasticity <- test_homoscedasticity(model)
        diagnostics <- diagnostic_report(model)
        
        # Create final output
        output_text <- paste(
          "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
          outlier_text,
          "<br><br>",
          "<strong>Normality Test:</strong><br>",
          normality$text,
          "<br><em>Interpretation: A significant p-value (< .05) suggests non-normality. ",
          "However, with large samples, minor deviations often become significant. ",
          "Visual inspection of the Q-Q plot is often more informative.</em>",
          "<br><br>",
          "<strong>Homoscedasticity Test:</strong><br>",
          homoscedasticity,
          "<br><em>Interpretation: A significant p-value suggests non-constant variance. ",
          "Consider the Residuals vs Fitted plot for visual confirmation.</em>",
          "<br><br>",
          "<strong>Additional Diagnostics:</strong><br>",
          paste(diagnostics, collapse = "<br>"),
          "<br><em>Interpretation:<br>",
          "- VIF > 5 suggests potential multicollinearity issues<br>",
          "- With bootstrapping, these diagnostics become less crucial as bootstrap methods are more robust to violations</em>",
          "</div>",
          sep = ""
        )
      }
      
      HTML(output_text)
      
    }, error = function(e) {
      print("Error in assumption checks:")
      print(e$message)
      return(HTML(paste(
        "<div class='alert alert-danger'>",
        "Error in assumption checks: ", e$message,
        "</div>"
      )))
    })
  })
  
  # Add this near the top of your server function
  observeEvent(input$data_file, {
    # Reset all inputs to their default values ONLY when a new file is loaded
    updateTextInput(session, "x_label", value = "")
    updateTextInput(session, "y_label", value = "")
    updateTextInput(session, "moderator_label", value = "")
    updateSelectInput(session, "predictor_var", choices = character(0))
    updateSelectInput(session, "outcome_var", choices = character(0))
    updateSelectInput(session, "moderator_var", choices = character(0))
    updateNumericInput(session, "decimal_places", value = 2)
    updateTextInput(session, "slopes_title", value = "Simple Slopes Plot")
    updateCheckboxInput(session, "use_color_lines", value = TRUE)
    
    # Clear any results
    rv$analysis_results <- NULL
  })
  
  # Update variable choices when dataset changes
  observe({
    req(rv$original_dataset)  # Use original dataset for choices
    vars <- names(rv$original_dataset)
    
    # Add blank option to choices
    choices <- c("Select variable" = "", vars)
    
    # Only update choices if they're empty
    if(is.null(input$predictor_var) || input$predictor_var == "") {
      updateSelectInput(session, "predictor_var", 
                       choices = choices,
                       selected = "")
    }
    if(is.null(input$outcome_var) || input$outcome_var == "") {
      updateSelectInput(session, "outcome_var", 
                       choices = choices,
                       selected = "")
    }
    if(is.null(input$moderator_var) || input$moderator_var == "") {
      updateSelectInput(session, "moderator_var", 
                       choices = choices,
                       selected = "")
    }
  })
  
  # Update label defaults when variables are selected
  observe({
    req(input$predictor_var, input$outcome_var, input$moderator_var)
    
    updateTextInput(session, "x_label", 
                   value = input$predictor_var)
    updateTextInput(session, "y_label", 
                   value = input$outcome_var)
    updateTextInput(session, "moderator_label", 
                   value = input$moderator_var)
  })
  
  # Add these observers to handle button states
  observe({
    if (is.null(rv$original_dataset) || 
        is.null(input$outcome_var) || 
        is.null(input$predictor_var) || 
        is.null(input$moderator_var)) {
      shinyjs::disable("run_analysis")
      shinyjs::disable("run_analysis_no_outliers")
    } else {
      shinyjs::enable("run_analysis")
      # Disable outlier removal for binary outcomes
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      if(outcome_is_binary) {
        shinyjs::disable("run_analysis_no_outliers")
      } else {
        shinyjs::enable("run_analysis_no_outliers")
      }
    }
  })
  
  # Add in the server section, after the other download handlers
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      # Set extension based on selected format
      ext <- if(input$filtered_data_format == "sav") "sav" else "csv"
      paste0("filtered_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    },
    content = function(file) {
      req(rv$original_dataset, input$outcome_var, input$predictor_var, 
          input$moderator_var, input$residual_threshold)
      
      # Create model formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var, "*", input$moderator_var)
      if (length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Fit model and identify outliers
      model <- lm(model_formula, data = rv$original_dataset)
      std_resid <- rstandard(model)
      outlier_cases <- which(abs(std_resid) > input$residual_threshold)
      
      # Create filtered dataset
      filtered_data <- rv$original_dataset[-outlier_cases, ]
      
      # Save in selected format
      if (input$filtered_data_format == "sav") {
        haven::write_sav(filtered_data, file)
      } else {
        write.csv(filtered_data, file, row.names = FALSE)
      }
    }
  )
  
  # Function to calculate missing data breakdown by variable
  create_missing_data_breakdown <- function(data, settings) {
    # Get all variables used in analysis
    all_vars <- c(settings$outcome_var, settings$predictor_var, settings$moderator_var)
    if(!is.null(settings$covariates) && length(settings$covariates) > 0) {
      all_vars <- c(all_vars, settings$covariates)
    }
    
    # Count missing values for each variable
    missing_breakdown <- sapply(all_vars, function(var) {
      sum(is.na(data[[var]]))
    })
    
    # Only include variables with missing data
    missing_breakdown <- missing_breakdown[missing_breakdown > 0]
    
    # Create formatted output
    if(length(missing_breakdown) > 0) {
      breakdown_lines <- sapply(names(missing_breakdown), function(var) {
        count <- missing_breakdown[var]
        sprintf("  %s: %d case%s missing", var, count, ifelse(count == 1, "", "s"))
      })
      paste(breakdown_lines, collapse = "<br>")
    } else {
      NULL
    }
  }
  
  # Function to calculate bivariate correlations with guidance
  create_bivariate_correlations <- function(data, predictor_var, outcome_var, settings) {
    # Check if outcome is binary
    outcome_is_binary <- is_binary_variable(data, outcome_var)
    
    # Remove missing data for both variables
    complete_data <- data[complete.cases(data[c(predictor_var, outcome_var)]), ]
    n <- nrow(complete_data)
    
    # Determine dataset description
    dataset_desc <- if(settings$outliers_removed) {
      sprintf("the original dataset (before %d outlier%s removed)", 
              settings$outliers_count, ifelse(settings$outliers_count == 1, "", "s"))
    } else {
      "the original dataset (all cases included)"
    }
    
    if(outcome_is_binary) {
      # For binary outcomes, provide point-biserial correlation (which is a Pearson correlation)
      # but with appropriate interpretation
      pearson_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                               method = "pearson")
      pearson_r <- pearson_test$estimate
      pearson_p <- pearson_test$p.value
      pearson_ci <- pearson_test$conf.int
      
      # Create formatted output for binary outcomes
      output <- c(
        "<br><strong>BIVARIATE CORRELATION: PREDICTOR AND OUTCOME</strong>",
        sprintf("<em>This shows the zero-order (unadjusted) relationship between the predictor and outcome variables, calculated on %s:</em>", dataset_desc),
        "",
        sprintf("<strong>Point-biserial correlation (Pearson's r):</strong> %.4f, 95%% CI [%.4f, %.4f], p %s", 
                pearson_r, pearson_ci[1], pearson_ci[2],
                ifelse(pearson_p < .001, "< .001", sprintf("= %.3f", pearson_p))),
        sprintf("<em>Sample size: %d cases (listwise deletion for these two variables)</em>", n),
        "",
        "<strong>Understanding This Correlation:</strong>",
        "<ul>",
        "<li><strong>Point-biserial correlation:</strong> This is a special case of Pearson's r when one variable is binary (0/1) and the other is continuous. It measures the linear relationship between the continuous predictor and the binary outcome.</li>",
        "<li><strong>Interpretation:</strong> The correlation coefficient indicates the strength and direction of the relationship. A positive correlation means higher predictor values are associated with the outcome being 1 (vs. 0).</li>",
        "<li><strong>Note:</strong> For binary outcomes, PROCESS uses logistic regression in the moderation analysis below, which provides odds ratios and log-odds coefficients. These are on a different scale than this correlation coefficient.</li>",
        "<li><strong>Why moderation coefficients differ:</strong> The regression coefficients in the moderation analysis are <strong>partial effects</strong> (log-odds) that control for:",
        "<ul>",
        "<li>The moderator variable</li>",
        "<li>The interaction term (predictor × moderator)</li>",
        if(!is.null(settings$covariates)) sprintf("<li>Covariate(s): %s</li>", paste(settings$covariates, collapse = ", ")) else NULL,
        "</ul>",
        "Additionally, the moderation analysis uses logistic regression (log-odds scale), while this correlation is on the original scale. This is why they differ.</li>",
        "</ul>",
        "<em>Note: A significant zero-order correlation does not guarantee a significant partial effect in moderation, and vice versa. The moderation analysis provides more nuanced information about how the relationship changes across moderator levels.</em>",
        ""
      )
    } else {
      # For continuous outcomes, provide both Pearson and Spearman
      pearson_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                               method = "pearson")
      pearson_r <- pearson_test$estimate
      pearson_p <- pearson_test$p.value
      pearson_ci <- pearson_test$conf.int
      
      spearman_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                                method = "spearman")
      spearman_rho <- spearman_test$estimate
      spearman_p <- spearman_test$p.value
      
      # Create formatted output for continuous outcomes
      output <- c(
        "<br><strong>BIVARIATE CORRELATION: PREDICTOR AND OUTCOME</strong>",
        sprintf("<em>This shows the zero-order (unadjusted) relationship between the predictor and outcome variables, calculated on %s:</em>", dataset_desc),
        "",
        sprintf("<strong>Pearson's r:</strong> %.4f, 95%% CI [%.4f, %.4f], p %s", 
                pearson_r, pearson_ci[1], pearson_ci[2],
                ifelse(pearson_p < .001, "< .001", sprintf("= %.3f", pearson_p))),
        sprintf("<strong>Spearman's ρ:</strong> %.4f, p %s",
                spearman_rho,
                ifelse(spearman_p < .001, "< .001", sprintf("= %.3f", spearman_p))),
        sprintf("<em>Sample size: %d cases (listwise deletion for these two variables)</em>", n),
        "",
        "<strong>Understanding These Correlations:</strong>",
        "<ul>",
        "<li><strong>Zero-order correlation:</strong> This is the simple bivariate relationship between predictor and outcome, without controlling for any other variables.</li>",
        "<li><strong>Pearson's r:</strong> Measures linear relationships. Assumes normality and linearity. Use when variables are continuous and normally distributed.</li>",
        "<li><strong>Spearman's ρ:</strong> Measures monotonic relationships (rank-based). More robust to outliers and non-normality. Use when variables are ordinal, have outliers, or show non-linear monotonic relationships.</li>",
        "<li><strong>Important note about datasets:</strong> This bivariate correlation is calculated on the <strong>original dataset (all cases)</strong>, regardless of whether outliers were removed for the moderation analysis below. This provides a baseline comparison showing the unadjusted relationship in the complete data.</li>",
        "<li><strong>Why moderation coefficients differ:</strong> The regression coefficients in the moderation analysis are <strong>partial effects</strong> that control for:",
        "<ul>",
        "<li>The moderator variable</li>",
        "<li>The interaction term (predictor × moderator)</li>",
        if(!is.null(settings$covariates)) sprintf("<li>Covariate(s): %s</li>", paste(settings$covariates, collapse = ", ")) else NULL,
        "</ul>",
        if(settings$outliers_removed) {
          sprintf("Additionally, the moderation analysis below is based on data with %d outlier%s removed, which may also contribute to differences from the bivariate correlation above.", 
                  settings$outliers_count, ifelse(settings$outliers_count == 1, "", "s"))
        } else {
          "The moderation analysis uses the same dataset as this bivariate correlation."
        },
        "This means the predictor's coefficient represents the effect of the predictor <em>while holding the moderator (and interaction, and covariates) constant</em>, which is why it differs from the zero-order correlation above.</li>",
        "</ul>",
        "<em>Note: A significant zero-order correlation does not guarantee a significant partial effect in moderation, and vice versa. The moderation analysis provides more nuanced information about how the relationship changes across moderator levels.</em>",
        ""
      )
    }
    
    # Remove NULL elements
    output <- output[!sapply(output, is.null)]
    
    paste(output, collapse = "<br>")
  }
  
  # Add this function in the server section
  create_correlation_matrix <- function(data, vars) {
    # Calculate correlations and p-values
    n <- length(vars)
    cor_matrix <- matrix(NA, n, n)
    p_matrix <- matrix(NA, n, n)
    
    for(i in 1:n) {
      for(j in 1:n) {
        test <- cor.test(data[[vars[i]]], data[[vars[j]]], use = "complete.obs")
        cor_matrix[i,j] <- test$estimate
        p_matrix[i,j] <- test$p.value
      }
    }
    
    # Create HTML table with formatting
    table_html <- c(
      "<div style='overflow-x: auto;'>",  # Add horizontal scroll if needed
      "<table class='table table-bordered' style='margin-top: 10px; margin-bottom: 10px; width: auto;'>",
      "<thead>",
      "<tr>",
      "<th style='background-color: #f8f9fa; border: 1px solid #dee2e6;'></th>"
    )
    
    # Add header row
    for(i in 1:n) {
      table_html <- c(table_html,
        sprintf("<th style='background-color: #f8f9fa; border: 1px solid #dee2e6;'>%d. %s</th>",
                i, vars[i]))
    }
    table_html <- c(table_html, "</tr></thead><tbody>")
    
    # Add data rows
    for(i in 1:n) {
      table_html <- c(table_html,
        sprintf("<tr><td style='background-color: #f8f9fa; border: 1px solid #dee2e6;'>%d. %s</td>",
                i, vars[i]))
      
      for(j in 1:n) {
        value <- sprintf("%.4f", cor_matrix[i,j])
        cell_style <- "style='border: 1px solid #dee2e6; padding: 8px; text-align: right;'"
        
        if(i != j) {  # Don't bold diagonal
          if(p_matrix[i,j] < .05) {
            value <- sprintf("<strong>%s</strong>", value)
          }
        }
        
        table_html <- c(table_html,
          sprintf("<td %s>%s</td>", cell_style, value))
      }
      table_html <- c(table_html, "</tr>")
    }
    
    table_html <- c(table_html,
      "</tbody></table></div>",
      "<p style='margin-top: 5px;'><em>Note: Coefficients in <strong>bold</strong> are significant at p < .05</em></p>"
    )
    
    paste(table_html, collapse = "\n")
  }
}

# Run the app
shinyApp(ui, server)
