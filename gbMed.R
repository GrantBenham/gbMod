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
  titlePanel("Mediation Analysis with PROCESS for R"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Upload Data"),
      div(
        id = "file_input_div",
        fileInput("data_file", "Choose CSV or SAV File", accept = c(".csv", ".sav"))
      ),
      
      h4("Select Variables"),
      uiOutput("variable_selectors"),
      
      # Conditional threshold input based on outcome type
      conditionalPanel(
        condition = "output.outcome_is_continuous === true",
        h5("Outlier Detection (Continuous Outcomes)"),
        numericInput("residual_threshold", "Standardized Residual Threshold", 
                    value = 2, min = 1, max = 10, step = 0.1),
        p(em("Cases with |standardized residual| > threshold will be identified as outliers"))
      ),
      conditionalPanel(
        condition = "output.outcome_is_continuous === false",
        h5("Influential Case Detection (Binary Outcomes)"),
        radioButtons("cooks_threshold_type", "Cook's Distance Threshold:",
          choices = list(
            "Conservative (4/n)" = "conservative",
            "Liberal (1.0)" = "liberal",
            "Custom" = "custom"
          ),
          selected = "conservative"
        ),
        conditionalPanel(
          condition = "input.cooks_threshold_type == 'custom'",
          numericInput("cooks_threshold_custom", "Custom Cook's Distance Threshold", 
                      value = 0.01, min = 0, max = 1, step = 0.001)
        ),
        p(em("Cases with Cook's distance > threshold will be identified as influential"))
      ),
      
      # Wrap all PROCESS settings in a conditional panel
      conditionalPanel(
        condition = "input.tabset_panel === 'Mediation Analysis'",
        
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
        
        checkboxInput("pairwise_contrasts", "Pairwise contrasts of indirect effects", FALSE),
        conditionalPanel(
          condition = "input.pairwise_contrasts == true",
          p(em("Compare indirect effects to determine which mediators are most important"))
        ),
        
        h4("Run Analysis"),
        div(style = "margin-bottom: 10px; width: 100%;",
          actionButton("run_analysis", "With Original Dataset", 
            class = "btn-primary",
            style = "width: 100%;"
          )
        ),
        div(style = "margin-bottom: 20px; width: 100%;",
          uiOutput("outlier_removal_button")
        ),
        
        h4("Download Options"),
        downloadButton("download_results", "Results Text"),
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
          
          # Show a prompt until variables are selected
          conditionalPanel(
            condition = "!output.outcome_is_selected",
            div(style = "margin-bottom: 20px",
              p("Select your outcome, predictor, and mediator(s) to view assumption guidance and diagnostics.")
            )
          ),
          
          # Continuous outcome guidance
          conditionalPanel(
            condition = "output.outcome_is_selected && output.outcome_is_continuous === true",
            div(style = "margin-bottom: 20px",
              p(strong("Note:"), " These assumption checks are always performed on the original dataset. Results update automatically based on your selected variables and standardized residual threshold value.")
            ),
            div(style = "margin-bottom: 20px",
              h5("Understanding Standardized Residual Outliers"),
              p("Standardized residuals (SR) represent how many standard deviations an observed value deviates from the model's prediction. Outliers can affect mediation analyses in several ways:",
                tags$ul(
                  tags$li(strong("Impact on Results:"), " Outliers can inflate or deflate direct and indirect effects and influence statistical significance"),
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
            )
          ),
          
          # Binary outcome guidance
          conditionalPanel(
            condition = "output.outcome_is_selected && output.outcome_is_continuous === false",
            div(style = "margin-bottom: 20px",
              p(strong("Note:"), " For binary outcomes (0/1), the app uses logistic regression diagnostics. Normality and homoscedasticity assumptions do not apply.")
            ),
            div(style = "margin-bottom: 20px",
              h5("Understanding Influential Cases (Cook's Distance)"),
              p("Cook's distance measures the influence of each case on all parameter estimates. High values suggest influential cases:",
                tags$ul(
                  tags$li(strong("Impact on Results:"), " Influential cases can materially change coefficient estimates and significance"),
                  tags$li(strong("Threshold Guidelines:"), " Common choices: 4/n (conservative), 1.0 (liberal), or a custom threshold"),
                  tags$li(strong("Handling Influential Cases:"), " This program offers two approaches:"),
                  tags$ul(
                    tags$li("Run analysis with all cases to maintain sample size"),
                    tags$li("Remove cases with Cook's D above the threshold to assess impact")
                  )
                ),
                "Note: The decision to remove influential cases should consider both statistical criteria and theory. Document and justify any removals."
              )
            )
          ),
          
          htmlOutput("assumption_details"),
          
          conditionalPanel(
            condition = "output.outcome_is_selected",
            h4("Diagnostic Plots"),
            conditionalPanel(
              condition = "output.outcome_is_continuous === true",
              div(style = "margin-bottom: 30px",
                h5("Normal Q-Q Plot (Outcome Model)"),
                p("This plot checks if residuals follow a normal distribution. Points should follow the diagonal line closely.",
                  "Deviations at the ends are common and usually not problematic.",
                  "When bootstrapping is used, normality is less crucial as bootstrap methods don't assume normality."),
                plotOutput("qq_plot", height = "400px", width = "600px")
              )
            ),
            
            div(style = "margin-bottom: 30px",
              h5("Residuals vs Fitted Plot (Outcome Model)"),
              conditionalPanel(
                condition = "output.outcome_is_continuous === true",
                p("This plot checks for linearity and homoscedasticity (constant variance).",
                  "Look for:",
                  tags$ul(
                    tags$li("Random scatter around the horizontal line (linearity)"),
                    tags$li("Even spread of points vertically (homoscedasticity)"),
                    tags$li("No clear patterns or curves in the blue line")
                  ),
                  "With bootstrapping, minor violations of homoscedasticity are less concerning.")
              ),
              conditionalPanel(
                condition = "output.outcome_is_continuous === false",
                p("For binary outcomes, this plot shows Pearson residuals from logistic regression.",
                  "The patterns will differ from linear regression:",
                  tags$ul(
                    tags$li("Residuals form distinct bands (one for each outcome level)"),
                    tags$li("Heteroscedasticity is expected and not a violation"),
                    tags$li("Focus on identifying potential model misspecification or influential cases")
                  ),
                  "Note: Homoscedasticity is not an assumption of logistic regression.")
              ),
              plotOutput("residual_plot", height = "400px", width = "600px")
            )
          ),
          
          # Move heteroscedasticity explanation here (only for continuous outcomes)
          conditionalPanel(
            condition = "output.outcome_is_continuous === true && output.outcome_is_selected",
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
                "Note: The assumption check results shown here are based on the raw data, regardless of which HC method is selected. The HC selection only affects the standard errors in the PROCESS analysis output.")
            )
          ),
          
          conditionalPanel(
            condition = "output.outcome_is_continuous === true",
            div(style = "margin-bottom: 30px",
              h5("Scale-Location Plot (Outcome Model)"),
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
          
          # Violin plots for continuous variables (original vs analysis)
          conditionalPanel(
            condition = "output.has_continuous_selected === true && output.outcome_is_selected",
            h4("Continuous Variable Distributions"),
            p("Distributions are shown for the original dataset and, if cases were removed, the analysis dataset. Only continuous variables are included."),
            plotOutput("violin_plot", height = "400px", width = "700px")
          )
        ),
        tabPanel("Mediation Analysis",
          conditionalPanel(
            condition = "output.analysis_ready === true",
            h4("Analysis Results"),
            htmlOutput("analysis_output")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Helper to summarize binary counts (available to entire server)
  binary_count_lines <- function(data, var, label) {
    if (is.null(var) || var == "" || is.null(data)) return(NULL)
    vals <- data[[var]]
    vals <- vals[!is.na(vals)]
    if (length(unique(vals)) > 2) return(NULL)
    tab <- table(vals)
    levels_sorted <- sort(unique(vals))
    counts <- sapply(levels_sorted, function(x) tab[as.character(x)])
    names(counts) <- levels_sorted
    sprintf("%s (%s): %s", label, var,
            paste(sprintf("%s = %d", names(counts), counts), collapse = "; "))
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
    print(paste("DEBUG - Dataset loaded with", nrow(data), "rows"))
    rv$analysis_results <- NULL  # Reset analysis results when new file is loaded
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
      selectInput("mediator_vars", "Mediator Variable(s)", vars, multiple = TRUE),
      p(em("Select one mediator for simple mediation, or multiple mediators for parallel mediation")),
      selectInput("covariates", "Covariates (optional)", vars, multiple = TRUE)
    )
  })
  
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
  
  # Helper to detect continuous variables (numeric/integer/labelled and not binary)
  is_continuous_variable <- function(data, var_name) {
    if(is.null(data) || is.null(var_name) || !var_name %in% names(data)) {
      return(FALSE)
    }
    if(is_binary_variable(data, var_name)) return(FALSE)
    v <- data[[var_name]]
    is.numeric(v) || is.integer(v) || inherits(v, "labelled")
  }
  
  # Helper function to detect if outcome is binary from PROCESS output
  is_binary_outcome <- reactive({
    req(analysis_results())
    process_output <- analysis_results()$output
    # Check for "Coding of binary Y for logistic regression analysis" in output
    any(grepl("Coding of binary Y for logistic regression", process_output, ignore.case = TRUE))
  })
  
  # Output to track if outcome is continuous (for conditionalPanel)
  output$outcome_is_continuous <- reactive({
    req(rv$original_dataset, input$outcome_var)
    !is_binary_variable(rv$original_dataset, input$outcome_var)
  })
  outputOptions(output, "outcome_is_continuous", suspendWhenHidden = FALSE)
  
  # Output to track if outcome is selected (for conditionalPanel)
  output$outcome_is_selected <- reactive({
    !is.null(input$outcome_var) && input$outcome_var != ""
  })
  outputOptions(output, "outcome_is_selected", suspendWhenHidden = FALSE)
  
  # Output to track if any continuous variables are selected (for violin plot)
  output$has_continuous_selected <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    selected_vars <- c(input$outcome_var, input$predictor_var, input$mediator_vars)
    any(vapply(selected_vars, function(v) is_continuous_variable(rv$original_dataset, v), logical(1)))
  })
  outputOptions(output, "has_continuous_selected", suspendWhenHidden = FALSE)
  
  # Output to track if analysis results exist
  output$analysis_ready <- reactive({
    !is.null(analysis_results())
  })
  outputOptions(output, "analysis_ready", suspendWhenHidden = FALSE)
  
  # Reactive to compute Cook's distance threshold
  cooks_threshold_value <- reactive({
    req(rv$original_dataset, input$outcome_var)
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    if(!outcome_is_binary) {
      return(NULL)
    }
    
    n <- nrow(rv$original_dataset)
    
    if(input$cooks_threshold_type == "conservative") {
      return(4 / n)
    } else if(input$cooks_threshold_type == "liberal") {
      return(1.0)
    } else {
      return(input$cooks_threshold_custom)
    }
  })
  
  # Function to identify outliers - for mediation, we check the outcome model
  identify_outliers <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    
    # Create outcome model formula: Y ~ X + M + covariates
    formula_terms <- c(input$outcome_var, "~", input$predictor_var)
    if (length(input$mediator_vars) > 0) {
      formula_terms <- c(formula_terms, "+", paste(input$mediator_vars, collapse = " + "))
    }
    if (length(input$covariates) > 0) {
      formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
    }
    model_formula <- as.formula(paste(formula_terms, collapse = " "))
    
    # Check if outcome is binary
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    if(outcome_is_binary) {
      # For binary outcomes, use logistic regression diagnostics
      model <- glm(model_formula, data = rv$original_dataset, family = binomial())
      
      # Calculate leverage (hat values) and Cook's distance
      leverage <- hatvalues(model)
      cooks_d <- cooks.distance(model)
      
      # Get threshold
      threshold <- cooks_threshold_value()
      
      # Identify influential cases based on Cook's distance
      influential_cases <- which(cooks_d > threshold)
      
      return(list(
        cases = influential_cases,
        values = cooks_d[influential_cases],
        leverage = leverage[influential_cases],
        count = length(influential_cases),
        percentage = length(influential_cases) / length(cooks_d) * 100,
        is_binary = TRUE,
        threshold = threshold,
        method = "Cook's Distance"
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
        is_binary = FALSE,
        threshold = input$residual_threshold,
        method = "Standardized Residuals"
      ))
    }
  })
  
  # Analysis with original dataset
  original_analysis <- eventReactive(input$run_analysis, {
    # Add validation before proceeding
    validate(
      need(rv$original_dataset, "Dataset not loaded"),
      need(input$outcome_var, "Outcome variable not selected"),
      need(input$predictor_var, "Predictor variable not selected"),
      need(input$mediator_vars, "At least one mediator variable must be selected"),
      need(length(input$mediator_vars) > 0, "At least one mediator variable must be selected"),
      need(input$outcome_var != input$predictor_var, 
           "Outcome and predictor must be different variables"),
      need(!input$outcome_var %in% input$mediator_vars,
           "Outcome cannot be the same as a mediator"),
      need(!input$predictor_var %in% input$mediator_vars,
           "Predictor cannot be the same as a mediator")
    )
    
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    source("process.R")
    
    withProgress(message = 'Running analysis...', value = 0, {
      print("Running analysis with original dataset")
      rv$outliers_info <- NULL
      
      # Store settings at time of analysis
      analysis_settings <- list(
        centering = input$centering,
        use_bootstrap = input$use_bootstrap,
        boot_samples = if(input$use_bootstrap) input$boot_samples else NULL,
        hc_method = input$hc_method,
        
        dataset_name = tools::file_path_sans_ext(basename(input$data_file$name)),
        original_n = nrow(rv$original_dataset),
        outliers_removed = FALSE,
        outliers_count = 0,
        outliers_threshold = if(is_binary_variable(rv$original_dataset, input$outcome_var)) {
          cooks_threshold_value()
        } else {
          input$residual_threshold
        },
        outliers_method = if(is_binary_variable(rv$original_dataset, input$outcome_var)) {
          "Cook's Distance"
        } else {
          "Standardized Residuals"
        },
        predictor_var = input$predictor_var,
        outcome_var = input$outcome_var,
        mediator_vars = input$mediator_vars,
        covariates = if(length(input$covariates) > 0) input$covariates else NULL
      )
      
      # Prepare process arguments
      # Get all variables needed for analysis
      all_vars_orig <- c(input$outcome_var, input$predictor_var, input$mediator_vars)
      if(length(input$covariates) > 0) {
        all_vars_orig <- c(all_vars_orig, input$covariates)
      }
      
      # Check complete cases for original dataset
      complete_cases_orig <- complete.cases(rv$original_dataset[all_vars_orig])
      n_complete_orig <- sum(complete_cases_orig)
      
      # Validate that we have enough complete cases
      if(n_complete_orig < 3) {
        stop(sprintf("Only %d complete cases available in the original dataset. This is insufficient for mediation analysis. Please check for missing data in your variables.", n_complete_orig))
      }
      
      # Use complete cases only to avoid PROCESS errors
      process_data_orig <- rv$original_dataset[complete_cases_orig, ]
      
      # Prepare mediator argument - can be single or multiple
      mediator_arg <- if(length(input$mediator_vars) == 1) {
        input$mediator_vars[1]
      } else {
        input$mediator_vars
      }
      
      # Set contrast parameter: 0 = no contrasts, 1 = pairwise contrasts
      contrast_val <- if(input$pairwise_contrasts && length(input$mediator_vars) > 1) {
        1  # Pairwise contrasts of indirect effects
      } else {
        0  # No contrasts
      }
      
      process_args <- list(
        data = process_data_orig,
        y = input$outcome_var,
        x = input$predictor_var,
        m = mediator_arg,
        model = 6,  # Model 6 for parallel mediation (works for single mediator too)
        center = as.numeric(input$centering),
        contrast = contrast_val,
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
      
      # Extract coefficients from the text output (for mediation, we may not need this)
      # But we'll keep the structure similar
      coefficients <- NULL
      
      # Create correlation data at time of analysis
      vars <- c(input$outcome_var, input$predictor_var, input$mediator_vars)
      if(length(input$covariates) > 0) {
        vars <- c(vars, input$covariates)
      }
      
      complete_data <- process_data_orig[complete.cases(process_data_orig[vars]), ]
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
  })
  
  # Analysis with outliers removed
  outliers_analysis <- eventReactive(input$run_analysis_no_outliers, {
    # Add validation before proceeding
    validate(
      need(rv$original_dataset, "Dataset not loaded"),
      need(input$outcome_var, "Outcome variable not selected"),
      need(input$predictor_var, "Predictor variable not selected"),
      need(input$mediator_vars, "At least one mediator variable must be selected"),
      need(length(input$mediator_vars) > 0, "At least one mediator variable must be selected"),
      need(input$outcome_var != input$predictor_var, 
           "Outcome and predictor must be different variables"),
      need(!input$outcome_var %in% input$mediator_vars,
           "Outcome cannot be the same as a mediator"),
      need(!input$predictor_var %in% input$mediator_vars,
           "Predictor cannot be the same as a mediator")
    )
    
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    source("process.R")
    
    withProgress(message = 'Running analysis...', value = 0, {
      print("Running analysis with outliers removed")
      outliers <- identify_outliers()
      # Store both outliers info and the threshold used
      rv$outliers_info <- list(
        count = outliers$count,
        threshold = outliers$threshold
      )
      reduced_data <- rv$original_dataset[-outliers$cases, ]
      
      # Check for complete cases after outlier removal
      # Get all variables needed for analysis
      all_vars <- c(input$outcome_var, input$predictor_var, input$mediator_vars)
      if(length(input$covariates) > 0) {
        all_vars <- c(all_vars, input$covariates)
      }
      
      # Check complete cases
      complete_cases <- complete.cases(reduced_data[all_vars])
      n_complete <- sum(complete_cases)
      
      # Validate that we have enough complete cases
      if(n_complete < 3) {
        stop(sprintf("After removing outliers, only %d complete cases remain. This is insufficient for mediation analysis. Please check for missing data in your covariates or use a different outlier threshold.", n_complete))
      }
      
      # Store settings at time of analysis
      analysis_settings <- list(
        centering = input$centering,
        use_bootstrap = input$use_bootstrap,
        boot_samples = if(input$use_bootstrap) input$boot_samples else NULL,
        hc_method = input$hc_method,
        
        dataset_name = tools::file_path_sans_ext(basename(input$data_file$name)),
        original_n = nrow(rv$original_dataset),
        outliers_removed = TRUE,
        outliers_count = outliers$count,
        outliers_threshold = outliers$threshold,
        outliers_method = outliers$method,
        predictor_var = input$predictor_var,
        outcome_var = input$outcome_var,
        mediator_vars = input$mediator_vars,
        covariates = if(length(input$covariates) > 0) input$covariates else NULL
      )
      
      # Prepare process arguments
      # Use complete cases only to avoid PROCESS errors
      process_data <- reduced_data[complete_cases, ]
      
      # Prepare mediator argument
      mediator_arg <- if(length(input$mediator_vars) == 1) {
        input$mediator_vars[1]
      } else {
        input$mediator_vars
      }
      
      # Set contrast parameter: 0 = no contrasts, 1 = pairwise contrasts
      contrast_val <- if(input$pairwise_contrasts && length(input$mediator_vars) > 1) {
        1  # Pairwise contrasts of indirect effects
      } else {
        0  # No contrasts
      }
      
      process_args <- list(
        data = process_data,
        y = input$outcome_var,
        x = input$predictor_var,
        m = mediator_arg,
        model = 6,  # Model 6 for parallel mediation
        center = as.numeric(input$centering),
        contrast = contrast_val,
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
      
      # Extract coefficients
      coefficients <- NULL
      
      # Create correlation data at time of analysis
      vars <- c(input$outcome_var, input$predictor_var, input$mediator_vars)
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
      
      # Handle VIF calculation for mediation (no interaction term)
      vif_result <- tryCatch({
        # Get model terms
        terms <- attr(terms(model), "term.labels")
        
        if(length(terms) > 1) {
          # For mediation, calculate VIF for all predictors
          vif_values <- suppressWarnings(car::vif(model))
          
          # Format VIF results
          sprintf("VIF for predictors: %s", 
                  paste(names(vif_values), sprintf("%.2f", vif_values), 
                  collapse = ", "))
        } else {
          "VIF not calculated (insufficient predictors)"
        }
      }, error = function(e) {
        "VIF calculation unavailable"
      })
      
      # Return diagnostics
      c(
        sprintf("Sample size: %d", n),
        vif_result,
        if(is_logistic) {
          "Note: VIF calculated for all predictors. For binary outcomes, VIF interpretation is similar to linear regression."
        } else {
          "Note: VIF calculated for all predictors"
        }
      )
    }, error = function(e) {
      c(
        "Unable to compute some diagnostic measures",
        paste("Error details:", e$message)
      )
    })
  }
  
  # Add this reactive for outlier summary
  outlier_summary <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    
    tryCatch({
        # Create outcome model formula: Y ~ X + M + covariates
        formula_terms <- c(input$outcome_var, "~", input$predictor_var)
        if (length(input$mediator_vars) > 0) {
            formula_terms <- c(formula_terms, "+", paste(input$mediator_vars, collapse = " + "))
        }
        if (length(input$covariates) > 0) {
            formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
        }
        model_formula <- as.formula(paste(formula_terms, collapse = " "))
        
        # Check if outcome is binary
        outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
        
        if(outcome_is_binary) {
          # For binary outcomes, use logistic regression diagnostics
          model <- glm(model_formula, data = rv$original_dataset, family = binomial())
          
          leverage <- hatvalues(model)
          cooks_d <- cooks.distance(model)
          
          # Get threshold
          n <- nrow(rv$original_dataset)
          threshold <- if(input$cooks_threshold_type == "conservative") {
            4 / n
          } else if(input$cooks_threshold_type == "liberal") {
            1.0
          } else {
            input$cooks_threshold_custom
          }
          
          # Identify influential cases
          influential_cases <- which(cooks_d > threshold)
          
          if(length(influential_cases) > 0) {
            # Sort by Cook's distance
            sorted_indices <- order(-cooks_d[influential_cases])
            sorted_cases <- influential_cases[sorted_indices]
            sorted_cooks <- cooks_d[sorted_cases]
            sorted_leverage <- leverage[sorted_cases]
            
            case_summaries <- sapply(seq_along(sorted_cases), function(i) {
              sprintf("Case %d: Cook's D = %.4f, Leverage = %.4f", 
                      sorted_cases[i], sorted_cooks[i], sorted_leverage[i])
            })
            
            threshold_label <- if(input$cooks_threshold_type == "conservative") {
              sprintf("4/n = %.4f", threshold)
            } else if(input$cooks_threshold_type == "liberal") {
              "1.0"
            } else {
              sprintf("%.4f", threshold)
            }
            
            return(c(
              "<strong>Influential Case Analysis (Logistic Regression):</strong>",
              "<em>Note: Your outcome variable is binary (0/1). Using leverage and Cook's distance.</em>",
              "<br>",
              sprintf("Cases exceeding threshold (Cook's D > %s):", threshold_label),
              sprintf("Number of cases: %d (%.1f%%)", 
                      length(influential_cases),
                      100 * length(influential_cases) / length(cooks_d)),
              "<br>",
              "<strong>Understanding these metrics:</strong>",
              "<ul>",
              "<li><strong>Cook's Distance:</strong> Measures the influence of each case on all parameter estimates. Values > 4/n (conservative) or > 1.0 (liberal) suggest influential cases.</li>",
              "<li><strong>Leverage:</strong> Measures how unusual a case's predictor values are. High leverage cases can have disproportionate influence on the model.</li>",
              "</ul>",
              "<strong>Top cases (sorted by Cook's D):</strong>",
              paste0('<div style="max-height: 100px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; margin: 5px 0;">', 
                    paste(case_summaries, collapse = "<br>"),
                    '</div>')
            ))
          } else {
            threshold_label <- if(input$cooks_threshold_type == "conservative") {
              sprintf("4/n = %.4f", threshold)
            } else if(input$cooks_threshold_type == "liberal") {
              "1.0"
            } else {
              sprintf("%.4f", threshold)
            }
            
            return(c(
              "<strong>Influential Case Analysis (Logistic Regression):</strong>",
              "<em>Note: Your outcome variable is binary (0/1). Using leverage and Cook's distance.</em>",
              "<br>",
              sprintf("No cases exceed the threshold (Cook's D > %s)", threshold_label)
            ))
          }
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
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    
    tryCatch({
      # Create outcome model formula: Y ~ X + M + covariates
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if (length(input$mediator_vars) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$mediator_vars, collapse = " + "))
      }
      if (length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        # For binary outcomes, use logistic regression
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        
        # Get outlier/influential summary
        outlier_text <- paste(outlier_summary(), collapse = "<br>")
        
        # Build filtered data after removal
        outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
        filtered_data <- rv$original_dataset
        if(!is.null(outliers) && length(outliers$cases) > 0) {
          filtered_data <- rv$original_dataset[-outliers$cases, ]
        }
        
        # Binary counts for outcome/mediators
        bin_counts <- c(
          binary_count_lines(rv$original_dataset, input$outcome_var, "Outcome (original)"),
          binary_count_lines(rv$original_dataset, input$predictor_var, "Predictor (original)")
        )
        # Add mediator counts
        for(med in input$mediator_vars) {
          bin_counts <- c(bin_counts,
            binary_count_lines(rv$original_dataset, med, paste0("Mediator ", med, " (original)"))
          )
        }
        # After removal counts
        bin_counts <- c(bin_counts,
          binary_count_lines(filtered_data, input$outcome_var, "Outcome (after removal)"),
          binary_count_lines(filtered_data, input$predictor_var, "Predictor (after removal)")
        )
        for(med in input$mediator_vars) {
          bin_counts <- c(bin_counts,
            binary_count_lines(filtered_data, med, paste0("Mediator ", med, " (after removal)"))
          )
        }
        
        # For binary outcomes, skip normality and homoscedasticity tests
        diagnostics <- diagnostic_report(model)
        
        output_text <- paste(
          "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
          "<strong>Note: Binary Outcome Detected</strong><br>",
          "<em>Your outcome variable is binary (0/1). PROCESS will use logistic regression for this analysis.</em><br><br>",
          "<strong>Important:</strong> Standard regression assumptions (normality, homoscedasticity) do not apply to logistic regression.<br>",
          "For binary outcomes, different diagnostic approaches are needed:<br>",
          "<ul>",
          "<li><strong>Linearity:</strong> Check linearity of continuous predictors with the logit of the outcome</li>",
          "<li><strong>Influential observations:</strong> Review leverage values and Cook's distance in the outlier summary above</li>",
          "<li><strong>Model fit:</strong> Use pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke) shown in PROCESS output</li>",
          "<li><strong>Multicollinearity:</strong> VIF can still be calculated for predictors</li>",
          "</ul><br>",
          if(length(na.omit(bin_counts)) > 0) {
            paste(
              "<strong>Binary Variable Counts (original dataset):</strong><br>",
              paste(na.omit(bin_counts), collapse = "<br>"),
              "<br><br>"
            )
          } else { "" },
          outlier_text,
          "<br><br>",
          "<strong>Additional Diagnostics:</strong><br>",
          paste(diagnostics, collapse = "<br>"),
          "<br><em>Note: VIF calculated for all predictors. For binary outcomes, focus on model fit statistics and residual patterns rather than normality/homoscedasticity.</em>",
          "</div>",
          sep = ""
        )
      } else {
        # For continuous outcomes, use linear regression
        model <- lm(model_formula, data = rv$original_dataset)
        
        # Get outlier summary
        outlier_text <- paste(outlier_summary(), collapse = "<br>")
        
        # Build filtered data after removal
        outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
        filtered_data <- rv$original_dataset
        if(!is.null(outliers) && length(outliers$cases) > 0) {
          filtered_data <- rv$original_dataset[-outliers$cases, ]
        }
        
        # Binary counts for outcome/predictor/mediators (only if binary)
        bin_counts <- c(
          binary_count_lines(rv$original_dataset, input$outcome_var, "Outcome (original)"),
          binary_count_lines(rv$original_dataset, input$predictor_var, "Predictor (original)")
        )
        for(med in input$mediator_vars) {
          bin_counts <- c(bin_counts,
            binary_count_lines(rv$original_dataset, med, paste0("Mediator ", med, " (original)"))
          )
        }
        bin_counts <- c(bin_counts,
          binary_count_lines(filtered_data, input$outcome_var, "Outcome (after removal)"),
          binary_count_lines(filtered_data, input$predictor_var, "Predictor (after removal)")
        )
        for(med in input$mediator_vars) {
          bin_counts <- c(bin_counts,
            binary_count_lines(filtered_data, med, paste0("Mediator ", med, " (after removal)"))
          )
        }
        
        # Run other diagnostics
        normality <- check_normality(model)
        homoscedasticity <- test_homoscedasticity(model)
        diagnostics <- diagnostic_report(model)
        
        # Create final output
        output_text <- paste(
          "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
          if(length(na.omit(bin_counts)) > 0) {
            paste(
              "<strong>Binary Variable Counts (original dataset):</strong><br>",
              paste(na.omit(bin_counts), collapse = "<br>"),
              "<br><br>"
            )
          } else { "" },
          outlier_text,
          if(length(na.omit(bin_counts)) > 0) {
            paste(
              "<br><strong>Binary Variable Counts (original vs after removal):</strong><br>",
              paste(na.omit(bin_counts), collapse = "<br>"),
              "<br><br>"
            )
          } else { "" },
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
  
  # Update the diagnostic plots to use original dataset
  output$qq_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    
    tryCatch({
      # Create outcome model formula: Y ~ X + M + covariates
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if (length(input$mediator_vars) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$mediator_vars, collapse = " + "))
      }
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
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    
    tryCatch({
      # Create outcome model formula: Y ~ X + M + covariates
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if (length(input$mediator_vars) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$mediator_vars, collapse = " + "))
      }
      if (length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        # For binary outcomes, use logistic regression with Cook's distance highlighting
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        fitted_values <- fitted(model)  # Probabilities
        pearson_resid <- residuals(model, type = "pearson")
        
        # Get influential cases via identify_outliers (Cook's distance)
        outliers <- identify_outliers()
        is_outlier <- seq_along(fitted_values) %in% outliers$cases
        
        subtitle_text <- if(outliers$count > 0) {
          sprintf("Cook's D > %.4f: %d influential case%s highlighted",
                  outliers$threshold, outliers$count,
                  ifelse(outliers$count == 1, "", "s"))
        } else {
          sprintf("Cook's D ≤ %.4f: No influential cases detected", outliers$threshold)
        }
        
        plot_data <- data.frame(
          fitted = fitted_values,
          residuals = pearson_resid,
          is_outlier = is_outlier
        )
        
        ggplot(plot_data, aes(x = fitted, y = residuals)) +
          geom_point(aes(color = is_outlier), alpha = 0.6) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted (Logistic Regression)",
               x = "Fitted probabilities",
               y = "Pearson residuals",
               subtitle = subtitle_text,
               color = "Influential (Cook's D)") +
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
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    
    tryCatch({
      # Create outcome model formula: Y ~ X + M + covariates
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if (length(input$mediator_vars) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$mediator_vars, collapse = " + "))
      }
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
  
  # Violin plot for continuous variables (original vs analysis dataset)
  output$violin_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
    
    # Determine height dynamically based on number of continuous vars
    selected_vars <- c(input$outcome_var, input$predictor_var, input$mediator_vars)
    cont_vars <- selected_vars[vapply(selected_vars, function(v) is_continuous_variable(rv$original_dataset, v), logical(1))]
    plot_height <- max(350, 250 * max(1, length(cont_vars)))
    session$sendCustomMessage(type = "resize-violin", message = list(height = plot_height))
    
    tryCatch({
      if(length(cont_vars) == 0) {
        plot.new()
        text(0.5, 0.5, "No continuous variables selected.\nViolin plot not shown.", cex = 1.1)
        return(NULL)
      }
      
      # Build data for original dataset
      orig_long <- do.call(rbind, lapply(cont_vars, function(v) {
        data.frame(
          variable = v,
          value = rv$original_dataset[[v]],
          dataset = "Original",
          stringsAsFactors = FALSE
        )
      }))
      
      # Build analysis dataset by removing identified outliers/influential cases
      analysis_long <- NULL
      outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
      filtered_data <- rv$original_dataset
      if(!is.null(outliers) && length(outliers$cases) > 0) {
        filtered_data <- rv$original_dataset[-outliers$cases, ]
      }
      analysis_long <- do.call(rbind, lapply(cont_vars, function(v) {
        data.frame(
          variable = v,
          value = filtered_data[[v]],
          dataset = "After removal",
          stringsAsFactors = FALSE
        )
      }))
      
      plot_data <- orig_long
      if(!is.null(analysis_long)) {
        plot_data <- rbind(plot_data, analysis_long)
      }
      plot_data <- plot_data[!is.na(plot_data$value), ]
      
      # Coerce values to numeric to handle labelled types
      plot_data$value <- suppressWarnings(as.numeric(plot_data$value))
      
      # Ensure variable is factor and dataset is factor for plotting
      plot_data$variable <- factor(plot_data$variable, levels = cont_vars)
      dataset_levels <- c("Original", "After removal")
      plot_data$dataset <- factor(plot_data$dataset, levels = dataset_levels)
      
      if(nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for violin plot.", cex = 1.1)
        return(NULL)
      }
      
      # Build plot
      p <- tryCatch({
        ggplot(plot_data, aes(x = dataset, y = value, fill = dataset)) +
          geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
          geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.6) +
          facet_wrap(~ variable, scales = "free_y", ncol = 1) +
          labs(title = "Continuous Variable Distributions",
               x = "Dataset (Original vs After removal)",
               y = "Value",
               fill = "Dataset") +
          theme_minimal() +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "right",
            strip.text = element_text(size = 14)
          )
      }, error = function(e) {
        NULL
      })
      
      if(is.null(p)) {
        plot.new()
        text(0.5, 0.5, "Error generating violin plot (ggplot failed). See console.", cex = 1.1)
        return(NULL)
      }
      
      print(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating violin plot:\n", e$message), cex = 1.1)
    })
  })
  
  # Function to create formatted output for mediation analysis
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
        if(settings$outliers_method == "Cook's Distance") {
          sprintf("%d influential cases (Cook's D > %.4f) removed", 
                  settings$outliers_count, settings$outliers_threshold)
        } else {
          sprintf("%d standardized residual outliers (|SR| > %.1f) removed", 
                  settings$outliers_count, settings$outliers_threshold)
        }
      } else {
        "Original dataset with all cases"
      },
      
      # Variable information
      sprintf("Predictor variable: %s", settings$predictor_var),
      sprintf("Outcome variable: %s", settings$outcome_var),
      sprintf("Mediator variable(s): %s", paste(settings$mediator_vars, collapse = ", ")),
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
    
    # Binary counts (only if outcome, predictor, or mediators are binary)
    original_counts <- c(
      binary_count_lines(analysis_results$original_data, settings$outcome_var, "Outcome (original)"),
      binary_count_lines(analysis_results$original_data, settings$predictor_var, "Predictor (original)")
    )
    for(med in settings$mediator_vars) {
      original_counts <- c(original_counts,
        binary_count_lines(analysis_results$original_data, med, paste0("Mediator ", med, " (original)"))
      )
    }
    used_counts <- c(
      binary_count_lines(analysis_results$data_used, settings$outcome_var, "Outcome (analysis dataset)"),
      binary_count_lines(analysis_results$data_used, settings$predictor_var, "Predictor (analysis dataset)")
    )
    for(med in settings$mediator_vars) {
      used_counts <- c(used_counts,
        binary_count_lines(analysis_results$data_used, med, paste0("Mediator ", med, " (analysis dataset)"))
      )
    }
    
    if(length(na.omit(c(original_counts, used_counts))) > 0) {
      output_text <- c(
        output_text,
        "<strong>BINARY VARIABLE COUNTS</strong>",
        c(original_counts, used_counts),
        ""
      )
    }
    
    # Add bivariate correlations section
    bivariate_cor <- create_bivariate_correlations(
      analysis_results$original_data,  # Use original dataset
      settings$predictor_var,
      settings$outcome_var,
      settings  # Pass settings for dataset description
    )
    
    # Process the PROCESS output
    process_output <- analysis_results$output
    filtered_output <- process_output[
      !grepl("^Bootstrap", process_output, ignore.case = TRUE) &
      !grepl("^Percentile bootstrap", process_output, ignore.case = TRUE) &
      !grepl("^\\*+ BOOTSTRAP", process_output) &
      !grepl("^Level of confidence", process_output) &
      !grepl("^\\s*$", process_output) &
      !grepl("^\\*+$", process_output) &
      !grepl("^\\s*\\|", process_output) &
      !grepl("^\\s*\\d+%", process_output) &
      !grepl("^\\s*>+\\s*$", process_output)
    ]
    
    # Check if outcome is binary (logistic regression)
    is_binary <- any(grepl("Coding of binary Y for logistic regression", 
                          process_output, ignore.case = TRUE))
    
    # Process the output to add formatting and section explanations
    processed_output <- character(0)
    missing_data_note <- filtered_output[grep("NOTE: Some cases with missing data were deleted", filtered_output)]
    filtered_output <- filtered_output[!grepl("NOTE: Some cases with missing data were deleted", filtered_output)]
    
    for(line in filtered_output) {
      if(grepl("^Sample size:", line)) {
        sample_size <- as.numeric(gsub("Sample size: ", "", line))
        
        # Calculate missing data cases
        missing_cases <- settings$original_n - sample_size
        if(settings$outliers_removed) {
          missing_cases <- missing_cases - settings$outliers_count
        }
        
        # Get missing data breakdown by variable
        missing_breakdown <- create_missing_data_breakdown(analysis_results$original_data, settings)
        
        processed_output <- c(processed_output,
          sprintf("Original dataset sample size: %d", settings$original_n),
          if(settings$outliers_removed) {
            if(settings$outliers_method == "Cook's Distance") {
              sprintf("Influential cases (Cook's D > %.4f) removed: %d cases", 
                      settings$outliers_threshold, settings$outliers_count)
            } else {
              sprintf("Standardized residual outliers (|SR| > %.1f) removed: %d cases", 
                      settings$outliers_threshold, settings$outliers_count)
            }
          },
          if(missing_cases > 0) {
            c(
              sprintf("Missing data: %d cases with missing data (listwise) were excluded from mediation analysis", missing_cases),
              if(!is.null(missing_breakdown)) {
                c("Missing data by variable:", missing_breakdown)
              }
            )
          },
          sprintf("Final sample size: %d", sample_size)
        )
      } else if(grepl("^Direct effect", line, ignore.case = TRUE)) {
        # Find the next few lines to check for significance
        line_idx <- which(filtered_output == line)
        direct_effect_lines <- character(0)
        if(line_idx + 1 <= length(filtered_output)) {
          direct_effect_lines <- c(direct_effect_lines, filtered_output[line_idx + 1])  # Header
        }
        if(line_idx + 2 <= length(filtered_output)) {
          data_line <- filtered_output[line_idx + 2]  # Data line
          # Check if p-value is significant (extract p-value and check)
          p_match <- regmatches(data_line, regexpr("\\s+[0-9.]+$", data_line))
          if(length(p_match) > 0) {
            p_val <- as.numeric(trimws(p_match))
            if(!is.na(p_val) && p_val < 0.05) {
              # Significant - highlight in red
              data_line <- gsub("(\\s+[0-9.]+$)", "<strong>\\1</strong>", data_line)
              direct_effect_lines <- c(direct_effect_lines, data_line)
              processed_output <- c(processed_output,
                "<br><strong>DIRECT EFFECT</strong>",
                "<em>The effect of the predictor on the outcome, controlling for the mediator(s):</em>",
                "<em>- This represents the effect of X on Y that is not through the mediator(s)</em>",
                "<em>- Significant if p < .05 (highlighted in red below)</em>",
                "<br>",
                "<span style='color: red;'>",
                line,
                direct_effect_lines,
                "</span>")
            } else {
              # Not significant - regular formatting
              processed_output <- c(processed_output,
                "<br><strong>DIRECT EFFECT</strong>",
                "<em>The effect of the predictor on the outcome, controlling for the mediator(s):</em>",
                "<em>- This represents the effect of X on Y that is not through the mediator(s)</em>",
                "<br>",
                line,
                direct_effect_lines)
            }
          } else {
            processed_output <- c(processed_output,
              "<br><strong>DIRECT EFFECT</strong>",
              "<em>The effect of the predictor on the outcome, controlling for the mediator(s):</em>",
              "<em>- This represents the effect of X on Y that is not through the mediator(s)</em>",
              "<br>",
              line,
              direct_effect_lines)
          }
        } else {
          processed_output <- c(processed_output,
            "<br><strong>DIRECT EFFECT</strong>",
            "<em>The effect of the predictor on the outcome, controlling for the mediator(s):</em>",
            "<em>- This represents the effect of X on Y that is not through the mediator(s)</em>",
            "<br>",
            line)
        }
      } else if(grepl("^Indirect effect", line, ignore.case = TRUE)) {
        # Find the next few lines to check for significance
        line_idx <- which(filtered_output == line)
        indirect_effect_lines <- character(0)
        contrast_lines <- character(0)
        skip_indirect_lines <- 0
        
        # Check if this is the first "Indirect effect" line (header)
        if(line_idx + 1 <= length(filtered_output)) {
          next_line <- filtered_output[line_idx + 1]
          if(grepl("Effect|BootSE|BootLLCI", next_line)) {
            indirect_effect_lines <- c(indirect_effect_lines, next_line)
            skip_indirect_lines <- 1
            
            # Check subsequent lines for indirect effect data
            has_significant <- FALSE
            in_contrast_section <- FALSE
            has_significant_contrast <- FALSE
            
            for(i in (line_idx + 2):min(line_idx + 20, length(filtered_output))) {
              if(i > length(filtered_output)) break
              data_line <- filtered_output[i]
              # Stop if we hit a blank line or new major section (but not contrast definitions)
              if(grepl("^\\s*$", data_line)) {
                # Check if next line is contrast definitions
                if(i + 1 <= length(filtered_output) && 
                   grepl("Specific indirect effect contrast|Indirect effect key", filtered_output[i + 1], ignore.case = TRUE)) {
                  # Continue to capture contrast information
                  in_contrast_section <- TRUE
                } else {
                  break
                }
              }
              if(grepl("^[A-Z]{3,}", data_line) && !grepl("Specific indirect effect contrast|Indirect effect key|Outcome variable", data_line, ignore.case = TRUE)) {
                break
              }
              
              # Check if this is a contrast line (starts with (C1), (C2), etc.)
              if(grepl("^\\(C[0-9]+\\)", data_line)) {
                contrast_lines <- c(contrast_lines, data_line)
                # Check for significance
                ci_match <- regmatches(data_line, gregexpr("\\s+[0-9.-]+", data_line))
                if(length(ci_match[[1]]) >= 2) {
                  llci <- as.numeric(trimws(ci_match[[1]][length(ci_match[[1]])-1]))
                  ulci <- as.numeric(trimws(ci_match[[1]][length(ci_match[[1]])]))
                  if(!is.na(llci) && !is.na(ulci) && (llci > 0 || ulci < 0)) {
                    has_significant_contrast <- TRUE
                  }
                }
              } else if(grepl("Specific indirect effect contrast|Indirect effect key", data_line, ignore.case = TRUE)) {
                # Capture contrast definition and key lines
                contrast_lines <- c(contrast_lines, data_line)
                in_contrast_section <- TRUE
              } else if(in_contrast_section && grepl("Ind[0-9]|->", data_line)) {
                # Capture indirect effect key lines
                contrast_lines <- c(contrast_lines, data_line)
              } else if(grepl("[0-9]", data_line) && !in_contrast_section) {
                # Regular indirect effect data line
                indirect_effect_lines <- c(indirect_effect_lines, data_line)
                skip_indirect_lines <- skip_indirect_lines + 1
                
                # Check if confidence intervals don't contain zero (significant)
                ci_match <- regmatches(data_line, gregexpr("\\s+[0-9.-]+", data_line))
                if(length(ci_match[[1]]) >= 2) {
                  llci <- as.numeric(trimws(ci_match[[1]][length(ci_match[[1]])-1]))
                  ulci <- as.numeric(trimws(ci_match[[1]][length(ci_match[[1]])]))
                  if(!is.na(llci) && !is.na(ulci) && (llci > 0 || ulci < 0)) {
                    has_significant <- TRUE
                  }
                }
              }
            }
            
            # Output indirect effects
            if(has_significant) {
              processed_output <- c(processed_output,
                "<br><strong>INDIRECT EFFECT(S)</strong>",
                "<em>The effect of the predictor on the outcome through the mediator(s):</em>",
                "<em>- This represents the mediated portion of the total effect</em>",
                "<em>- For parallel mediation, there will be multiple indirect effects (one per mediator)</em>",
                "<em>- Significant if 95% CI does not contain zero (highlighted in red below)</em>",
                "<br>",
                "<span style='color: red;'>",
                line,
                indirect_effect_lines,
                "</span>")
            } else {
              processed_output <- c(processed_output,
                "<br><strong>INDIRECT EFFECT(S)</strong>",
                "<em>The effect of the predictor on the outcome through the mediator(s):</em>",
                "<em>- This represents the mediated portion of the total effect</em>",
                "<em>- For parallel mediation, there will be multiple indirect effects (one per mediator)</em>",
                "<br>",
                line,
                indirect_effect_lines)
            }
            
            # Output pairwise contrasts if they exist
            if(length(contrast_lines) > 0) {
              if(has_significant_contrast) {
                processed_output <- c(processed_output,
                  "<br><strong>PAIRWISE CONTRASTS OF INDIRECT EFFECTS</strong>",
                  "<em>Compares indirect effects to determine which mediators are most important:</em>",
                  "<em>- Significant contrasts (CI does not contain zero) indicate that one indirect effect is significantly different from another</em>",
                  "<em>- Significant contrasts are highlighted in red below</em>",
                  "<br>",
                  "<span style='color: red;'>",
                  contrast_lines,
                  "</span>")
              } else {
                processed_output <- c(processed_output,
                  "<br><strong>PAIRWISE CONTRASTS OF INDIRECT EFFECTS</strong>",
                  "<em>Compares indirect effects to determine which mediators are most important:</em>",
                  "<em>- Significant contrasts (CI does not contain zero) indicate that one indirect effect is significantly different from another</em>",
                  "<br>",
                  contrast_lines)
              }
            }
          } else {
            processed_output <- c(processed_output,
              "<br><strong>INDIRECT EFFECT(S)</strong>",
              "<em>The effect of the predictor on the outcome through the mediator(s):</em>",
              "<em>- This represents the mediated portion of the total effect</em>",
              "<em>- For parallel mediation, there will be multiple indirect effects (one per mediator)</em>",
              "<br>",
              line)
          }
        } else {
          processed_output <- c(processed_output,
            "<br><strong>INDIRECT EFFECT(S)</strong>",
            "<em>The effect of the predictor on the outcome through the mediator(s):</em>",
            "<em>- This represents the mediated portion of the total effect</em>",
            "<em>- For parallel mediation, there will be multiple indirect effects (one per mediator)</em>",
            "<br>",
            line)
        }
      } else if(grepl("^Total effect", line, ignore.case = TRUE)) {
        # Find the next line to check for significance
        line_idx <- which(filtered_output == line)
        total_effect_lines <- character(0)
        if(line_idx + 1 <= length(filtered_output)) {
          total_effect_lines <- c(total_effect_lines, filtered_output[line_idx + 1])  # Header
        }
        if(line_idx + 2 <= length(filtered_output)) {
          data_line <- filtered_output[line_idx + 2]  # Data line
          # Check if p-value is significant or CI doesn't contain zero
          p_match <- regmatches(data_line, regexpr("\\s+[0-9.]+$", data_line))
          ci_match <- regmatches(data_line, gregexpr("\\s+[0-9.-]+", data_line))
          is_significant <- FALSE
          
          if(length(p_match) > 0) {
            p_val <- as.numeric(trimws(p_match))
            if(!is.na(p_val) && p_val < 0.05) {
              is_significant <- TRUE
            }
          } else if(length(ci_match[[1]]) >= 2) {
            llci <- as.numeric(trimws(ci_match[[1]][length(ci_match[[1]])-1]))
            ulci <- as.numeric(trimws(ci_match[[1]][length(ci_match[[1]])]))
            if(!is.na(llci) && !is.na(ulci) && (llci > 0 || ulci < 0)) {
              is_significant <- TRUE
            }
          }
          
          if(is_significant) {
            data_line <- gsub("(\\s+[0-9.]+$)", "<strong>\\1</strong>", data_line)
            total_effect_lines <- c(total_effect_lines, data_line)
            processed_output <- c(processed_output,
              "<br><strong>TOTAL EFFECT</strong>",
              "<em>The total effect of the predictor on the outcome (direct + indirect):</em>",
              "<em>- This is the sum of the direct effect and all indirect effects</em>",
              "<em>- Significant if p < .05 or CI does not contain zero (highlighted in red below)</em>",
              "<br>",
              "<span style='color: red;'>",
              line,
              total_effect_lines,
              "</span>")
          } else {
            processed_output <- c(processed_output,
              "<br><strong>TOTAL EFFECT</strong>",
              "<em>The total effect of the predictor on the outcome (direct + indirect):</em>",
              "<em>- This is the sum of the direct effect and all indirect effects</em>",
              "<br>",
              line,
              total_effect_lines)
          }
        } else {
          processed_output <- c(processed_output,
            "<br><strong>TOTAL EFFECT</strong>",
            "<em>The total effect of the predictor on the outcome (direct + indirect):</em>",
            "<em>- This is the sum of the direct effect and all indirect effects</em>",
            "<br>",
            line)
        }
      } else if(grepl("^Specific indirect effect contrast|^Indirect effect key", line, ignore.case = TRUE)) {
        # Skip these lines as they're now handled in the indirect effects section
        # This prevents duplication
        processed_output <- c(processed_output, line)
      } else if(grepl("^Model Summary:", line)) {
        if(is_binary) {
          processed_output <- c(processed_output,
            "<br><strong>MODEL FIT STATISTICS</strong>",
            "<em>This section shows how well the overall model fits the data (logistic regression):</em>",
            "<em>- -2LL: -2 times the log-likelihood (lower is better)</em>",
            "<em>- ModelLL: Model log-likelihood</em>",
            "<em>- McFadden, Cox-Snell, Nagelkerke: Pseudo-R² measures (analogous to R² in linear regression)</em>",
            "<em>- p-value: Likelihood ratio test (tests if model is significantly better than null model)</em>",
            "<br>",
            line)
        } else {
          processed_output <- c(processed_output,
            "<br><strong>MODEL FIT STATISTICS</strong>",
            "<em>This section shows how well the overall model fits the data:</em>",
            "<em>- R-squared indicates the proportion of variance explained</em>",
            "<em>- F-test shows if the model is significantly better than no predictors</em>",
            "<br>",
            line)
        }
      } else if(grepl("^Model:", line)) {
        if(is_binary) {
          processed_output <- c(processed_output,
            "<br><strong>REGRESSION COEFFICIENTS (LOG-ODDS SCALE)</strong>",
            "<em>Key statistics for each predictor in the model (logistic regression):</em>",
            "<em>- Coefficient (coeff): Log-odds (logit scale) - the strength and direction of relationships</em>",
            "<em>- p-value (p): Statistical significance (p < .05 typically considered significant)</em>",
            "<em>- LLCI/ULCI: 95% confidence intervals for log-odds (significant if they don't contain zero)</em>",
            "<em>- Note: These coefficients are on the log-odds scale. See ODDS RATIOS section below for more interpretable values.</em>",
            "<br>",
            line)
        } else {
          processed_output <- c(processed_output,
            "<br><strong>REGRESSION COEFFICIENTS</strong>",
            "<em>Key statistics for each predictor in the model:</em>",
            "<em>- Coefficient (coeff): The strength and direction of relationships</em>",
            "<em>- p-value (p): Statistical significance (p < .05 typically considered significant)</em>",
            "<em>- LLCI/ULCI: 95% confidence intervals (significant if they don't contain zero)</em>",
            "<br>",
            line)
        }
      } else if(grepl("^These results are expressed in a log-odds metric", line, ignore.case = TRUE)) {
        # Add odds ratio guidance after the log-odds note
        if(is_binary) {
          processed_output <- c(processed_output,
            line,
            "<br><strong>ODDS RATIOS</strong>",
            "<em>For binary outcomes, odds ratios are often more interpretable than log-odds coefficients:</em>",
            "<ul>",
            "<li><strong>What are odds ratios?</strong> Odds ratios (OR) show how the odds of the outcome change for a one-unit increase in the predictor.</li>",
            "<li><strong>Interpretation:</strong> OR > 1 means higher predictor values increase the odds of outcome = 1. OR < 1 means higher predictor values decrease the odds.</li>",
            "<li><strong>Example:</strong> OR = 1.5 means a one-unit increase in the predictor multiplies the odds by 1.5 (50% increase).</li>",
            "<li><strong>When to report:</strong> Many researchers report odds ratios in results sections for easier interpretation, while log-odds are used in statistical models.</li>",
            "<li><strong>SPSS PROCESS:</strong> The SPSS PROCESS macro shows log-odds coefficients in the main output (as shown above). Odds ratios can be calculated by exponentiating the coefficients (e^coeff).</li>",
            "</ul>",
            "<em>To calculate odds ratios from the coefficients above, exponentiate each coefficient: OR = e^coeff</em>",
            "<em>For example, if coeff = 0.5, then OR = e^0.5 = 1.65</em>",
            "<em>Note: Confidence intervals for odds ratios can be obtained by exponentiating the LLCI and ULCI values above.</em>")
        } else {
          processed_output <- c(processed_output, line)
        }
      } else {
        processed_output <- c(processed_output, line)
      }
    }
    
    # Combine settings, bivariate correlations, and processed output
    paste(
      "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
      paste(c(output_text, "", bivariate_cor, "", processed_output), collapse = "<br>"),
      "</div>"
    )
  }
  
  # Modify the analysis_output renderUI
  output$analysis_output <- renderUI({
    req(analysis_results())
    HTML(create_formatted_output(analysis_results()))
  })
  
  # Function to calculate missing data breakdown by variable
  create_missing_data_breakdown <- function(data, settings) {
    # Get all variables used in analysis
    all_vars <- c(settings$outcome_var, settings$predictor_var, settings$mediator_vars)
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
      # For binary outcomes, provide point-biserial correlation
      pearson_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                               method = "pearson")
      pearson_r <- pearson_test$estimate
      pearson_p <- pearson_test$p.value
      pearson_ci <- pearson_test$conf.int
      
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
        "<li><strong>Point-biserial correlation:</strong> This is a special case of Pearson's r when one variable is binary (0/1) and the other is continuous.</li>",
        "<li><strong>Note:</strong> For binary outcomes, PROCESS uses logistic regression in the mediation analysis below, which provides odds ratios and log-odds coefficients.</li>",
        "</ul>",
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
        "<li><strong>Zero-order correlation:</strong> This is the simple bivariate relationship between predictor and outcome, without controlling for mediator(s) or covariates.</li>",
        "<li><strong>Pearson's r:</strong> Measures linear relationships. Assumes normality and linearity.</li>",
        "<li><strong>Spearman's ρ:</strong> Measures monotonic relationships (rank-based). More robust to outliers and non-normality.</li>",
        "<li><strong>Note:</strong> The mediation analysis below controls for mediator(s) and covariates, so coefficients will differ from this zero-order correlation.</li>",
        "</ul>",
        ""
      )
    }
    
    paste(output, collapse = "<br>")
  }
  
  # Function to create correlation matrix
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
      "<div style='overflow-x: auto;'>",
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
  
  # UI output for outlier removal button with dynamic text
  output$outlier_removal_button <- renderUI({
    req(rv$original_dataset, input$outcome_var)
    
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    button_text <- if(outcome_is_binary) {
      "With Cook's Distance Outliers Removed"
    } else {
      "With Standardized Residual Outliers Removed"
    }
    
    actionButton("run_analysis_no_outliers", button_text, 
                class = "btn-warning",
                style = "width: 100%;")
  })
  
  # Add these observers to handle button states
  observe({
    if (is.null(rv$original_dataset) || 
        is.null(input$outcome_var) || 
        is.null(input$predictor_var) || 
        is.null(input$mediator_vars) || 
        length(input$mediator_vars) == 0) {
      shinyjs::disable("run_analysis")
      shinyjs::disable("run_analysis_no_outliers")
    } else {
      shinyjs::enable("run_analysis")
      # Check if there are any outliers/influential cases
      outliers <- identify_outliers()
      if(outliers$count > 0) {
        shinyjs::enable("run_analysis_no_outliers")
      } else {
        shinyjs::disable("run_analysis_no_outliers")
      }
    }
  })
  
  # Download handler for results
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
  
  # Download handler for filtered dataset
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      ext <- if(input$filtered_data_format == "sav") "sav" else "csv"
      paste0("filtered_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    },
    content = function(file) {
      req(rv$original_dataset, input$outcome_var, input$predictor_var, input$mediator_vars)
      
      # Use identify_outliers to get the cases to remove
      outliers <- identify_outliers()
      
      if(outliers$count == 0) {
        stop("No outliers/influential cases found to remove")
      }
      
      # Create filtered dataset
      filtered_data <- rv$original_dataset[-outliers$cases, ]
      
      # Save in selected format
      if (input$filtered_data_format == "sav") {
        haven::write_sav(filtered_data, file)
      } else {
        write.csv(filtered_data, file, row.names = FALSE)
      }
    }
  )
}

# Run the app
shinyApp(ui, server)

