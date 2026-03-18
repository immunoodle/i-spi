# ---- concentration_calc_df — session-scoped so batch observers can use it ----
concentration_calc_df <- reactive({
  req(
    input$sc_plate_select,
    input$readxMap_study_accession    != "Click here",
    input$readxMap_experiment_accession != "Click here"
  )
  concentrationUIRefresher()
  
  calc_df <- get_existing_concentration_calc(
    conn                 = conn,
    project_id           = userWorkSpaceID(),
    study_accession      = input$readxMap_study_accession,
    experiment_accession = input$readxMap_experiment_accession,
    plate_nom            = input$sc_plate_select
  )
  
  # Overlay in-memory "pending" states for MCMC
  pending <- mcmc_pending_scopes()
  if (length(pending) > 0) {
    for (entry in pending) {
      match_rows <- (
        calc_df$scope                     == entry[["scope"]] &
          calc_df$concentration_calc_method == entry[["method"]]
      )
      if (any(match_rows)) calc_df$job_status[match_rows] <- "pending"
    }
  }
  
  # Overlay in-memory "pending" states for interpolated
  interp_pending <- interp_pending_scopes()
  if (length(interp_pending) > 0) {
    for (entry in interp_pending) {
      match_rows <- (
        calc_df$scope                      == entry[["scope"]] &
          calc_df$concentration_calc_method == entry[["method"]]
      )
      if (any(match_rows)) calc_df$job_status[match_rows] <- "pending"
    }
  }
  
  # When mcmc_robust is active at a scope, show interpolated as completed
  # BUT NOT if interpolated is currently pending (running)
  mcmc_active_scopes <- unique(
    calc_df$scope[
      calc_df$concentration_calc_method == "mcmc_robust" &
        calc_df$job_status                != "not begun"
    ]
  )
  calc_df$job_status <- ifelse(
    calc_df$concentration_calc_method == "interpolated" &
      calc_df$scope %in% mcmc_active_scopes &
      calc_df$job_status != "pending",
    "completed",
    calc_df$job_status
  )
  
  calc_df
})

# ============================================================================
# Navigation Observer
# Handles UI setup when the user navigates to the Standard Curve tab.
# ============================================================================
observeEvent(
  list(
    input$readxMap_experiment_accession,
    input$readxMap_study_accession,
    input$qc_component,
    input$study_level_tabs,
    input$main_tabs
  ),
  {
    req(
      input$qc_component                  == "Standard Curve",
      input$readxMap_study_accession      != "Click here",
      input$readxMap_experiment_accession != "Click here",
      input$study_level_tabs              == "Experiments",
      input$main_tabs                     == "view_files_tab"
    )
    
    selected_study      <- input$readxMap_study_accession
    selected_experiment <- input$readxMap_experiment_accession
    
    verbose                    <- FALSE
    model_names                <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
    param_group                <- "standard_curve_options"
    allowed_constraint_methods <- c(
      "default", "user_defined", "range_of_blanks", "geometric_mean_of_blanks"
    )
    
    loaded_data <- pull_data(
      study_accession      = selected_study,
      experiment_accession = selected_experiment,
      project_id           = userWorkSpaceID(),
      conn                 = conn
    )
    
    #loaded_data_v <<- loaded_data
    
    response_var <- loaded_data$response_var
    indep_var    <- loaded_data$indep_var
    
    se_antigen_table <- compute_antigen_se_table(
      standards_data = loaded_data$standards,
      response_col   = response_var,
      dilution_col   = "dilution",
      plate_col      = "plate",
      grouping_cols  = c("project_id", "study_accession", "experiment_accession", "source", "antigen"),
      #method         = "pooled_within",
      verbose        = TRUE
    )
    

    dil_series_se_table <- compute_dil_series_se(standards_data = loaded_data$standards, 
                                                response_col  = response_var,
                                                dilution_col  = "dilution",
                                                plate_col     = "plate_nom",
                                                grouping_cols = c("project_id",
                                                                  "study_accession",
                                                                  "experiment_accession",
                                                                  "source_nom",
                                                                  "antigen",
                                                                  "feature"),
                                                min_reps = 2,
                                                verbose  = FALSE) 
    

    study_params <- fetch_study_parameters(
      study_accession = selected_study,
      param_user      = currentuser(),
      param_group     = param_group,
      project_id      = userWorkSpaceID(),
      conn            = conn
    )
    
    # ------------------------------------------------------------------
    # Top-level UI shell
    # ------------------------------------------------------------------
    output$std_curver_ui <- renderUI({
      tagList(
        div(
          style = paste0(
            "background-color:#f0f8ff; border:1px solid #4a90e2;",
            "padding:10px; margin-bottom:15px; border-radius:5px;"
          ),
          tags$h4(
            "Current Standard Curve Context",
            style = "margin-top:0; color:#2c5aa0;"
          ),
          uiOutput("standard_curve_context")
        ),
        
        conditionalPanel(
          condition = "output.can_fit_standard_curve == true",
          
          fluidRow(
            column(3, uiOutput("sc_plate_selector")),
            column(3, uiOutput("sc_antigen_selector")),
            column(3, uiOutput("sc_source_selector"))
          ),
          
          fluidRow(
            column(3, actionButton("show_comparisions",
                                   "Show Model Comparisons")),
            column(3, downloadButton("download_best_fit_parameter_estimates",
                                     "Download Parameter Estimates for Selected Fit")),
            column(3, downloadButton("download_samples_above_ulod",
                                     "Download Samples above the Upper Limit of Detection")),
            column(3, downloadButton("download_samples_below_llod",
                                     "Download Samples below the Lower Limit of Detection"))
          ),
          
          fluidRow(
            column(3, uiOutput("is_display_log_response")),
            column(3, uiOutput("is_display_log_independent_variable"))
          ),
          
          shinycssloaders::withSpinner(
            plotlyOutput("standard_curve", width = "75vw", height = "800px"),
            type    = 6,
            color   = "#4a90e2",
            caption = "Fitting standard curve, please wait..."
          ),
          
          br(),
          
          bsCollapsePanel(
            title = "Standard Curve QC Glossary",
            style = "success",
            tagList(
              tags$dl(
                style = "margin-bottom:0;",
                
                tags$dt(tags$strong("Standards")),
                tags$dd("Known concentration reference points used to construct the standard curve."),
                
                tags$dt(tags$strong("FDA + 2018")),
                tags$dd("Parameter QC Levels. Standard curve reference points with precision ≤ 20% unless more concentrated than the
                        ULOQ where ≤25% is used. In addition, between-plate accuracy is ±20% of the nominal concentration."),
                
                tags$dt(tags$strong("FDA - 2018")),
                tags$dd("Standard curve reference points with precision > 20% (> ULOQ: > 25%) and 
                        between-plate accuracy is > ±20% of the nominal concentration.
                        U.S. Food and Drug Administration. Bioanalytical Method Validation:
                        Guidance for Industry. Center for Drug Evaluation and Research (CDER) / Center for Veterinary Medicine (CVM). May 2018.
                        Available ", tags$a(
                          href   = "https://www.fda.gov/files/drugs/published/Bioanalytical-Method-Validation-Guidance-for-Industry.pdf",
                          "here",
                          target = "_blank")
                        ),
                
                tags$dt(tags$strong("Samples")),
                tags$dd(paste0(
                  "Unknown test samples interpolated against the standard curve to determine ",
                  "their concentrations based on measured assay response values."
                )),
                
                tags$dt(tags$strong("Fitted Curve")),
                tags$dd("The fitted sigmoidal curve to the standard data points."),
                
                tags$dt(tags$strong("95% CI (Confidence Interval)")),
                tags$dd("The 95% CI around the fitted standard curve."),
                
                tags$dt(tags$strong("Lower and Upper LODs (Limit of Detection)")),
                tags$dd(
                  paste0(
                    "Lower and upper LODs are defined as the upper 97.5% confidence bound of the ",
                    "lower asymptote and the lower 2.5% confidence bound of the upper asymptote, ",
                    "respectively "
                  ),
                  tags$a(
                    href   = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
                    "(Rajam et al.).",
                    target = "_blank"
                  ),
                  paste0(
                    " Limits of Detection correspond to the y-coordinate in the legend, ",
                    "as they are defined on the response axis."
                  )
                ),
                
                tags$dt(tags$strong("Minimum Detectable Concentration")),
                tags$dd(
                  "The smallest antibody concentration that produces a signal the assay can detect above background ",
                  tags$a(
                    href   = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
                    "(Rajam et al.).",
                    target = "_blank"
                  ),
                  paste0(
                    " This corresponds to the x-coordinate of the Lower Limit of Detection in the legend, ",
                    "as it is on the concentration axis."
                  )
                ),
                
                tags$dt(tags$strong("Lower and Upper RDL (Reliable Detection Limit)")),
                tags$dd(
                  "Lower RDL: The lowest concentration at which the assay consistently produces a signal above background with 95% confidence based on the fit of the standard curve ",
                  tags$a(
                    href   = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
                    "(Rajam et al.).",
                    target = "_blank"
                  ),
                  tags$br(),
                  " Upper RDL: Analogously, the highest concentration at which the assay consistently produces a signal below the upper asymptote (saturation) with 95% confidence, based on the fit of the standard curve."
                ),
                
                tags$dt(tags$strong("Lower and Upper LOQs (Limits of Quantification)")),
                tags$dd(
                  "Defines a region of assay response (MFI) and concentration where sample estimates have less measurement error. Limits of Quantification are derived from the local minimum and maximum of the second derivative of x given y of the standard curve ",
                  tags$a(
                    href   = "Daly%20et.%20al%202005_BMCBioinformatics_Evaluating%20concentration%20estimation%20errors%20in%20ELISA%20microarray%20experiments1471-2105-6-17.pdf",
                    "(Daly et al.)",
                    target = "_blank"
                  ),
                  ", ",
                  tags$a(
                    href   = "LinearPortion_BendPoints.pdf",
                    "(Jeanne L Sebaugh and P. D. McCray)",
                    target = "_blank"
                  ),
                  ", ",
                  tags$a(
                    href   = "drLumi-An_open-source_package_to_manage_data_calibrate_and_conduct_quality_control_of_multiplex_bead-based_immunoassays_data_analysis.pdf",
                    "(Sanz et al.).",
                    target = "_blank"
                  )
                ),
                
                tags$dt(tags$strong("2nd Derivative of x given y")),
                tags$dd("The second derivative curve used to identify Limits of Quantification."),
                
                tags$dt(tags$strong("pCoV (predicted concentration Coefficient of Variation)")),
                tags$dd(
                  "A measure of the concentration estimation error corresponding to each sample measurement and is on the concentration uncertainty axis ",
                  tags$a(
                    href   = "Daly%20et.%20al%202005_BMCBioinformatics_Evaluating%20concentration%20estimation%20errors%20in%20ELISA%20microarray%20experiments1471-2105-6-17.pdf",
                    "(Daly et al.).",
                    target = "_blank"
                  )
                ),
                
                tags$dt(tags$strong("pCoV Threshold")),
                tags$dd("The acceptable cutoff for the predicted concentration coefficient of variation."),
                
                tags$dt(tags$strong("Inflection Point")),
                tags$dd(paste0(
                  "The point on the standard curve where the concavity transitions from concave up ",
                  "to concave down. It is the point where the assay is most sensitive to measurement ",
                  "errors in the measured response of the assay."
                ))
              )
            )
          ),
          
          div(class = "table-container", tableOutput("summary_statistics")),
          uiOutput("concentrationMethodUI")
        )
      )
    })
    
    
    # ------------------------------------------------------------------
    # Prerequisites check (shared between context UI and conditional panel)
    # ------------------------------------------------------------------
    sc_prereqs <- reactive({
      standards_n         <- nrow(loaded_data$standards[
        loaded_data$standards$experiment_accession == selected_experiment, ])
      blanks_n            <- nrow(loaded_data$blanks[
        loaded_data$blanks$experiment_accession == selected_experiment, ])
      blank_option        <- study_params$blank_option
      constraints_methods <- unique(loaded_data$antigen_constraints$l_asy_constraint_method)
      invalid_constraints <- setdiff(constraints_methods, allowed_constraint_methods)
      
      list(
        standards_n         = standards_n,
        blanks_n            = blanks_n,
        blank_option        = blank_option,
        constraints_methods = constraints_methods,
        invalid_constraints = invalid_constraints,
        standards_missing   = standards_n == 0,
        blanks_missing      = blanks_n == 0,
        constraints_invalid = length(invalid_constraints) > 0,
        blank_required      = blank_option != "ignored",
        blank_blocking      = blanks_n == 0 && blank_option != "ignored"
      )
    })
    
    
    # ------------------------------------------------------------------
    # Context banner
    # ------------------------------------------------------------------
    output$standard_curve_context <- renderUI({
      p <- sc_prereqs()
      
      blank_labels <- c(
        ignored        = "Ignored",
        included       = "Included",
        subtracted     = "Subtracted 1 \u00d7 Geometric Mean",
        subtracted_3x  = "Subtracted 3 \u00d7 Geometric Mean",
        subtracted_5x  = "Subtracted 5 \u00d7 Geometric Mean",
        subtracted_10x = "Subtracted 10 \u00d7 Geometric Mean"
      )
      blank_label <- blank_labels[[p$blank_option]]
      
      reasons <- character(0)
      if (p$standards_missing)
        reasons <- c(reasons,
                     glue::glue("{p$standards_n} standards found in {selected_experiment}."))
      if (p$constraints_invalid)
        reasons <- c(reasons,
                     glue::glue("Invalid constraint methods: {paste(p$invalid_constraints, collapse=', ')}"))
      if (p$blank_blocking)
        reasons <- c(reasons, glue::glue(
          "Blank control is set to {blank_label} in the study settings but {p$blanks_n} blanks found."
        ))
      
      blank_note <- if (!p$blank_blocking) {
        glue::glue(
          " (ok since blank control is currently set to {blank_label} in study settings.)"
        )
      } else {
        ""
      }
      
      if (length(reasons) > 0) {
        return(HTML(glue::glue(
          "Standard curve fitting cannot proceed for {selected_experiment}.<br><br>",
          "Unmet requirements:<br>",
          "{paste(paste0('&nbsp;- ', reasons), collapse = '<br>')}<br><br>",
          "Details:<br>",
          "- Standards: {p$standards_n}<br>",
          "- Blanks: {p$blanks_n}{blank_note}<br>",
          "- Constraint method(s) found in {selected_experiment}: {paste(p$constraints_methods, collapse=', ')}<br>",
          "- Constraint method(s) valid: {ifelse(p$constraints_invalid, 'no', 'yes')}"
        )))
      }
      
      HTML(glue::glue(
        "{p$standards_n} standards found for {selected_experiment}.<br>",
        "{p$blanks_n} blanks found for {selected_experiment}.<br>",
        "Constraint method(s) found in {selected_experiment}: {paste(p$constraints_methods, collapse=', ')}<br>",
        "Constraint method(s) valid: {ifelse(p$constraints_invalid, 'no', 'yes')}<br>",
        "Current Blank Option selected (in study settings): {blank_label}<br><br>",
        "Standard curve fitting may proceed."
      ))
    })
    
    
    # ------------------------------------------------------------------
    # Gate for the conditional panel
    # ------------------------------------------------------------------
    can_fit_standard_curve <- reactive({
      p       <- sc_prereqs()
      reasons <- character(0)
      if (p$standards_missing)
        reasons <- c(reasons,
                     glue::glue("{p$standards_n} standards found in {selected_experiment}."))
      if (p$constraints_invalid)
        reasons <- c(reasons,
                     glue::glue("Invalid constraint methods: {paste(p$invalid_constraints, collapse=', ')}"))
      if (p$blank_blocking)
        reasons <- c(reasons,
                     glue::glue("Blanking is '{p$blank_option}' but {p$blanks_n} blanks found."))
      length(reasons) == 0
    })
    
    output$can_fit_standard_curve <- reactive({ can_fit_standard_curve() })
    outputOptions(output, "can_fit_standard_curve", suspendWhenHidden = FALSE)
    
    
    # ------------------------------------------------------------------
    # Selectors
    # ------------------------------------------------------------------
    output$sc_plate_selector <- renderUI({
      req(loaded_data$standards$study_accession,
          loaded_data$standards$experiment_accession,
          nrow(loaded_data$standards) > 0)

      updateSelectInput(session, "sc_plate_select", selected = NULL)  # Reset the plateSelection
      req(nrow(loaded_data$standards) > 0)

     unique_plates <- unique(loaded_data$standards$plate_nom)

      selectInput("sc_plate_select",
                  label = "Plate - Sample Dilution(s)",
                  choices = unique_plates)
    })

    output$sc_antigen_selector <- renderUI({
      req(loaded_data$standards$study_accession, loaded_data$standards$experiment_accession)
      updateSelectInput(session, "sc_antigen_select", selected = NULL)
      
      response_var <- loaded_data$response_var  # "absorbance" for ELISA, "mfi" for Luminex
      
      cat("debug in antigen selector:\n")
      print(response_var)
      
      print(selected_study)
      print(selected_experiment)
      print(input$sc_plate_select)
      
      print(head(loaded_data$standards))

      dat_antigen <- loaded_data$standards[loaded_data$standards$study_accession %in% selected_study &
                                          loaded_data$standards$experiment_accession %in% selected_experiment &
                                            loaded_data$standards$plate_nom %in% input$sc_plate_select, ]
      
      # Use the correct response variable column name
      dat_antigen <- dat_antigen[!is.na(dat_antigen[[response_var]]),]
      
      req(nrow(dat_antigen) > 0)
      
      sc_feature_select(dat_antigen$feature)
      
      selectInput("sc_antigen_select",
                  label = "Antigen",
                  choices = unique(dat_antigen$antigen))
    })

    output$sc_source_selector <- renderUI({
      req(loaded_data$standards, input$sc_plate_select, input$sc_antigen_select)
      
      cat("debug in source selector:\n")
      print(selected_study)
      print(selected_experiment)
      print(input$sc_plate_select)
      print(input$sc_antigen_select)
      
      print(head(loaded_data$standards))
      

      dat_source <- loaded_data$standards[
          loaded_data$standards$study_accession %in% selected_study &
          loaded_data$standards$experiment_accession %in% selected_experiment &
          loaded_data$standards$plate_nom %in% input$sc_plate_select &
          loaded_data$standards$antigen %in% input$sc_antigen_select, ]


      req(nrow(dat_source) > 0)

      # Use source_nom for the selector (includes wavelength for ELISA)
      source_choices <- if ("source_nom" %in% names(dat_source)) {
        unique(dat_source$source_nom)
      } else {
        unique(dat_source$source)
      }

      radioButtons(
        "sc_source_select",
        label = "Source",
        choices = source_choices,
        selected = source_choices[1]
      )
    })
    
    # output$sc_antigen_selector <- renderUI({
    #   req(loaded_data$standards$study_accession,
    #       loaded_data$standards$experiment_accession)
    #   
    #   updateSelectInput(session, "sc_antigen_select", selected = NULL)
    #   
    #   dat <- loaded_data$standards[
    #     loaded_data$standards$study_accession      %in% selected_study &
    #       loaded_data$standards$experiment_accession %in% selected_experiment &
    #       loaded_data$standards$plate_nom            %in% input$sc_plate_select, ]
    #   dat <- dat[!is.na(dat$mfi), ]
    #   req(nrow(dat) > 0)
    #   
    #   selectInput("sc_antigen_select",
    #               label   = "Antigen",
    #               choices = unique(dat$antigen))
    # })
    
    # output$sc_source_selector <- renderUI({
    #   req(loaded_data$standards, input$sc_plate_select, input$sc_antigen_select)
    #   
    #   dat <- loaded_data$standards[
    #     loaded_data$standards$study_accession      %in% selected_study &
    #       loaded_data$standards$experiment_accession %in% selected_experiment &
    #       loaded_data$standards$plate_nom            %in% input$sc_plate_select &
    #       loaded_data$standards$antigen              %in% input$sc_antigen_select, ]
    #   req(nrow(dat) > 0)
    #   
    #   radioButtons(
    #     "sc_source_select",
    #     label    = "Source",
    #     choices  = unique(dat$source),
    #     selected = unique(dat$source)[1]
    #   )
    # })
    # 
    
    # ------------------------------------------------------------------
    # Loading notification
    # ------------------------------------------------------------------
    sc_loading_id      <- reactiveVal(NULL)
    sc_best_fit_ready  <- reactiveVal(FALSE)
    
    observeEvent(
      list(input$sc_plate_select, input$sc_antigen_select, input$sc_source_select),
      ignoreInit = TRUE,
      {
        req(input$sc_plate_select, input$sc_antigen_select)
        
        if (!is.null(sc_loading_id())) {
          removeNotification(sc_loading_id())
          sc_loading_id(NULL)
        }
        sc_best_fit_ready(FALSE)
        
        id <- showNotification(
          ui = tagList(
            tags$b("Fitting standard curve..."),
            tags$br(),
            tags$span(
              style = "font-size:0.9em; color:#555;",
              paste0("Plate: ", input$sc_plate_select,
                     " | Antigen: ", input$sc_antigen_select)
            )
          ),
          duration    = NULL,
          closeButton = FALSE,
          type        = "message",
          id          = "sc_loading"
        )
        sc_loading_id(id)
      }
    )
    
    observeEvent(best_fit(), ignoreNULL = TRUE, ignoreInit = TRUE, {
      shinyjs::delay(100, {
        if (!is.null(sc_loading_id())) {
          removeNotification(sc_loading_id())
          sc_loading_id(NULL)
        }
        sc_best_fit_ready(TRUE)
      })
    })
    
    
    # ------------------------------------------------------------------
    # Curve-fitting reactives
    # ------------------------------------------------------------------
    antigen_plate <- reactive({
      req(input$sc_source_select,
          input$sc_antigen_select,
          input$sc_plate_select,
          loaded_data,
          loaded_data$antigen_constraints,
          loaded_data$standards,
          nrow(loaded_data$standards) > 0)

      antigen_constraints <- loaded_data$antigen_constraints[
        loaded_data$antigen_constraints$antigen %in% input$sc_antigen_select, ,
        drop = FALSE]
      req(nrow(antigen_constraints) > 0)
      
      result <- select_antigen_plate(
        loaded_data          = loaded_data,
        study_accession      = selected_study,
        experiment_accession = selected_experiment,
        source               = input$sc_source_select,
        antigen              = input$sc_antigen_select,
        plate                = input$sc_plate_select,
        antigen_constraints  = antigen_constraints
      )
      req(result)
      req(result$plate_standard, nrow(result$plate_standard) > 0)
      result
    })
    
    prepped_data <- reactive({
      plate <- antigen_plate()
      req(plate)
      
      preprocess_robust_curves(
        data                 = plate$plate_standard,
        antigen_settings     = plate$antigen_settings,
        response_variable    = loaded_data$response_var,
        independent_variable = loaded_data$indep_var,
        is_log_response      = study_params$is_log_response,
        blank_data           = plate$plate_blanks,
        blank_option         = study_params$blank_option,
        is_log_independent   = study_params$is_log_independent,
        apply_prozone        = study_params$applyProzone,
        verbose              = verbose
      )
    })
    
    formulas <- reactive({
      plate <- antigen_plate()
      req(plate)
      
      select_model_formulas(
        fixed_constraint  = plate$fixed_a_result,
        response_variable = loaded_data$response_var,
        is_log_response   = study_params$is_log_response
      )
    })
    
    model_constraints <- reactive({
      plate <- antigen_plate()
      pdata <- prepped_data()
      f     <- formulas()
      req(plate, pdata, f)
      
      obtain_model_constraints(
        data                 = pdata$data,
        formulas             = f,
        independent_variable = loaded_data$indep_var,
        response_variable    = loaded_data$response_var,
        is_log_response      = TRUE,
        is_log_concentration = TRUE,
        antigen_settings     = plate$antigen_settings,
        max_response         = max(pdata$data[[loaded_data$response_var]], na.rm = TRUE),
        min_response         = min(pdata$data[[loaded_data$response_var]], na.rm = TRUE)
      )
    })
    
    start_lists <- reactive({
      mc <- model_constraints()
      req(mc)
      
      make_start_lists(
        model_constraints = mc,
        frac_generate     = 0.8,
        quants            = c(low = 0.2, mid = 0.5, high = 0.8)
      )
    })
    
    fit_robust_lm <- reactive({
      pdata <- prepped_data()
      f     <- formulas()
      mc    <- model_constraints()
      sl    <- start_lists()
      req(pdata, f, mc, sl)
      
      compute_robust_curves(
        prepped_data         = pdata$data,
        response_variable    = loaded_data$response_var,
        independent_variable = loaded_data$indep_var,
        formulas             = f,
        model_constraints    = mc,
        start_lists          = sl,
        verbose              = verbose
      )
    })
    
    fit_summary <- reactive({
      fit <- fit_robust_lm()
      req(fit)
      summarize_model_fits(fit, verbose = verbose)
    })
    
    fit_params <- reactive({
      fit <- fit_robust_lm()
      req(fit)
      
      summarize_model_parameters(
        models_fit_list = fit,
        level           = 0.95,
        model_names     = model_names
      )
    })
    
    plot_data <- reactive({
      pdata  <- prepped_data()
      fit    <- fit_robust_lm()
      plate  <- antigen_plate()
      params <- fit_params()
      req(pdata, fit, plate, params)
      
      get_plot_data(
        models_fit_list = fit,
        prepped_data    = pdata$data,
        fit_params      = params,
        fixed_a_result  = plate$fixed_a_result,
        model_names     = model_names,
        x_var           = loaded_data$indep_var,
        y_var           = loaded_data$response_var
      )
    })
    
    best_fit <- reactive({
      params     <- fit_params()
      fit        <- fit_robust_lm()
      summary    <- fit_summary()
      pdata      <- prepped_data()
      plate      <- antigen_plate()
      pdata_plot <- plot_data()
      mc         <- model_constraints()
      req(params, fit, summary, pdata, plate, pdata_plot, mc)
      
      current_se <- lookup_antigen_se(
        se_table             = se_antigen_table,
        study_accession      = selected_study,
        experiment_accession = selected_experiment,
        source = input$sc_source_select,
        antigen = input$sc_antigen_select,
        feature = sc_feature_select()
      )
  
      bf <- select_model_fit_AIC(
        fit_summary   = summary,
        fit_robust_lm = fit,
        fit_params    = params,
        plot_data     = pdata_plot,
        verbose       = verbose
      )
      # ── Ensure response column is valid ──────────────────────────────
      resolved <- ensure_response_column(
        df           = bf$best_data,
        response_var = response_var,
        coerce_numeric = TRUE,
        context      = "bf_reactive"
      )
      
      bf$best_data <- resolved$df
      
      # If the column was found under a different name, update response_var
      # for all downstream calls in this reactive
      if (resolved$ok && resolved$response_var != response_var) {
        message(sprintf(
          "[bf reactive] response_var changed from '%s' to '%s'",
          response_var, resolved$response_var
        ))
        response_var <- resolved$response_var
      }
      
      # ── Ensure response column is numeric in best_data ──────────────
      response_var <- loaded_data$response_var
      if (!is.null(bf$best_data) && 
          response_var %in% names(bf$best_data) &&
          !is.numeric(bf$best_data[[response_var]])) {
        message(sprintf(
          "[bf reactive] Coercing '%s' from %s to numeric in best_data",
          response_var, class(bf$best_data[[response_var]])[1]
        ))
        bf$best_data[[response_var]] <- suppressWarnings(
          as.numeric(bf$best_data[[response_var]])
        )
      }
      
      dil_series_df_filtered <- tryCatch({
        # Filter to the specific antigen + plate + source being fitted
        mask <- (
          dil_series_se_table$plate_nom  == input$sc_plate_select  &
            dil_series_se_table$source_nom == input$sc_source_select &
            dil_series_se_table$antigen    == input$sc_antigen_select
        )
        sub <- dil_series_se_table[mask, , drop = FALSE]
        if (nrow(sub) == 0L) {
          if (verbose) message("[best_fit] dil_series_se_table: no rows after antigen filter — skipping accuracy")
          NULL
        } else {
          sub
        }
      }, error = function(e) {
        message("[best_fit] dil_series_se_table filter error: ", e$message)
        NULL
      })
        
        dil_series_acc <- if (!is.null(dil_series_df_filtered)) {
          tryCatch(
            compute_dil_series_accuracy(
              best_fit                   = bf,
              dil_series_df              = dil_series_df_filtered,
              response_col               = response_var,
              independent_variable       = loaded_data$indep_var,
              dilution_col               = "dilution",
              fixed_a_result             = plate$fixed_a_result,
              is_log_response            = study_params$is_log_response,
              is_log_concentration       = study_params$is_log_independent,
              undiluted_sc_concentration = plate$antigen_settings$standard_curve_concentration,
              cv_threshold               = 20, #plate$antigen_settings$pcov_threshold, #15 # pCoV threshold 
              lloq_cv_threshold          = 25,  # if it is the lowest dilution factor/highest concentration use this. 
              accuracy_lo                = 80,
              accuracy_hi                = 120,
              verbose                    = verbose
            ),
            error = function(e) {
              message("[best_fit] compute_dil_series_accuracy error: ", e$message)
              dil_series_df_filtered   # return unmodified if accuracy fails
            }
          )
        } else {
          NULL
        }
        
      #dil_series_acc_v <<- dil_series_acc
      
      bf <- fit_qc_glance(
        best_fit             = bf,
        response_variable    = response_var,
        independent_variable = loaded_data$indep_var,
        fixed_a_result       = plate$fixed_a_result,
        antigen_settings     = plate$antigen_settings,
        antigen_fit_options  = pdata$antigen_fit_options,
        dil_series_se_plate_source = dil_series_acc,
        verbose              = verbose
      )
      

      
      bf <- tidy.nlsLM(
        best_fit            = bf,
        fixed_a_result      = plate$fixed_a_result,
        model_constraints   = mc,
        antigen_settings    = plate$antigen_settings,
        antigen_fit_options = pdata$antigen_fit_options,
        verbose             = verbose
      )
        
      bf <- predict_and_propagate_error(
        best_fit        = bf,
        response_var    = response_var,
        antigen_plate   = plate,
        study_params    = study_params,
        se_std_response = current_se,
        verbose         = verbose
      )
      
      gate_samples(
        best_fit          = bf,
        response_variable = loaded_data$response_var,
        pcov_threshold    = plate$antigen_settings$pcov_threshold,
        verbose           = verbose
      )
    })
    
    
    # ------------------------------------------------------------------
    # Toggle switches
    # ------------------------------------------------------------------
    output$is_display_log_response <- renderUI({
      input_switch("display_log_response",
                   "Display as Log Response",
                   value = TRUE)
    })
    
    output$is_display_log_independent_variable <- renderUI({
      input_switch("display_log_independent",
                   "Display independent variable as logged",
                   value = TRUE)
    })
    
    
    # ------------------------------------------------------------------
    # Standard curve plot
    # ------------------------------------------------------------------
    output$standard_curve <- renderPlotly({
      bf    <- best_fit()
      plate <- antigen_plate()
      req(bf, plate)
      
      plot_standard_curve(
        best_fit                   = bf,
        is_display_log_response    = input$display_log_response,
        is_display_log_independent = input$display_log_independent,
        pcov_threshold             = plate$antigen_settings$pcov_threshold,
        independent_variable       = loaded_data$indep_var,
        response_variable          = loaded_data$response_var
      )
    })
    
    
    # ------------------------------------------------------------------
    # Model comparisons modal
    # ------------------------------------------------------------------
    output$model_comparisions <- renderPlot({
      pd <- plot_data()
      req(pd)
      
      plot_model_comparisons(
        plot_data                  = pd,
        model_names                = model_names,
        x_var                      = loaded_data$indep_var,
        y_var                      = loaded_data$response_var,
        is_display_log_response    = input$display_log_response,
        is_display_log_independent = input$display_log_independent,
        use_patchwork              = TRUE
      )
    })
    
    observeEvent(input$show_comparisions, {
      pd <- tryCatch(plot_data(), error = function(e) NULL)
      
      if (is.null(pd) || is.null(pd$dat)) {
        showNotification(
          "No plot data available. Please ensure standard curve data is loaded.",
          type = "warning"
        )
        return()
      }
      
      showModal(modalDialog(
        title     = paste("Model Comparisons for",
                          unique(pd$dat$antigen), "on", unique(pd$dat$plate)),
        size      = "l",
        plotOutput("model_comparisions"),
        downloadButton("download_model_comparisons", "Download Model Comparisons"),
        easyClose = TRUE,
        footer    = modalButton("Close")
      ))
    })
    
    output$download_model_comparisons <- downloadHandler(
      filename = function() {
        pd <- plot_data()
        paste0(
          "model_comparison_",
          unique(pd$dat$study_accession),
          unique(pd$dat$experiment_accession),
          unique(pd$dat$plate_nom),
          unique(pd$dat$antigen),
          ".pdf"
        )
      },
      content = function(file) {
        pd <- plot_data()
        req(pd)
        
        p <- plot_model_comparisons(
          plot_data                  = pd,
          model_names                = model_names,
          x_var                      = loaded_data$indep_var,
          y_var                      = loaded_data$response_var,
          is_display_log_response    = input$display_log_response,
          is_display_log_independent = input$display_log_independent,
          use_patchwork              = TRUE
        )
        
        ggsave(filename = file, plot = p, device = "pdf",
               width = 8, height = 10, units = "in")
      }
    )
    
    
    # ------------------------------------------------------------------
    # Summary statistics table
    # ------------------------------------------------------------------
    output$summary_statistics <- renderTable({
      bf <- best_fit()
      req(bf)
      bf$best_glance
    },
    caption           = "Summary Statistics",
    caption.placement = getOption("xtable.caption.placement", "top"))
    
    
    # ------------------------------------------------------------------
    # Downloads
    # ------------------------------------------------------------------
    output$download_best_fit_parameter_estimates <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession,
              input$readxMap_experiment_accession,
              input$sc_plate_select, "x",
              input$sc_antigen_select,
              "tidy_parameter_estimates.csv", sep = "_")
      },
      content = function(file) {
        bf <- best_fit()
        req(bf)
        write.csv(bf$best_tidy, file, row.names = FALSE)
      }
    )
    
    output$download_samples_above_ulod <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession,
              input$readxMap_experiment_accession,
              input$sc_plate_select, "x",
              input$sc_antigen_select,
              "samples_above_ulod.csv", sep = "_")
      },
      content = function(file) {
        bf <- best_fit()
        req(bf)
        se <- bf$sample_se
        drop_cols <- c("plate_id", "assay_response_variable",
                       "assay_independent_variable", "y_new", "overall_se")
        write.csv(
          se[se$gate_class_lod == "Too Concentrated",
             !(names(se) %in% drop_cols)],
          file, row.names = FALSE
        )
      }
    )
    
    output$download_samples_below_llod <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession,
              input$readxMap_experiment_accession,
              input$sc_plate_select, "x",
              input$sc_antigen_select,
              "samples_below_llod.csv", sep = "_")
      },
      content = function(file) {
        bf <- best_fit()
        req(bf)
        se <- bf$sample_se
        drop_cols <- c("plate_id", "assay_response_variable",
                       "assay_independent_variable", "y_new", "overall_se")
        write.csv(
          se[se$gate_class_lod == "Too Diluted",
             !(names(se) %in% drop_cols)],
          file, row.names = FALSE
        )
      }
    )
    
    
    # ------------------------------------------------------------------
    # Concentration method UI (references session-scoped concentration_calc_df)
    # ------------------------------------------------------------------
    # output$concentrationMethodUI <- renderUI({
    #   df  <- concentration_calc_df()
    #   msg <- mcmc_progress_msg()
    #   
    #   if (nrow(df) == 0) {
    #     HTML("<p>No concentration methods have been calculated for this plate.</p>")
    #   } else {
    #     createStandardCurveConcentrationTypeUI(
    #       existing_concentration_calc = df,
    #       progress_msg                = msg
    #     )
    #   }
    # })
    output$concentrationMethodUI <- renderUI({
      df         <- concentration_calc_df()
      interp_msg <- interp_progress_msg()
      mcmc_msg   <- mcmc_progress_msg()
      #msg        <- if (!is.null(interp_msg)) interp_msg else mcmc_msg
      
      createStandardCurveConcentrationTypeUI(
        existing_concentration_calc = df,
        progress_msg                = mcmc_msg,
        interp_progress_msg = interp_msg
      )
    })
    
  } # end if Standard Curve
)   # end navigation observer


# ============================================================================
# Scope selector
# Rendered once; selected_scope() restores the user's last choice so the
# radio buttons don't snap back to "study" when concentration_calc_df()
# invalidates mid-calculation.
# ============================================================================
output$calculation_scope_ui <- renderUI({
  radioButtons(
    inputId  = "save_scope",
    label    = "Calculation scope",
    choices  = c(
      "Current plate"      = "plate",
      "Current experiment" = "experiment",
      "All experiments"    = "study"
    ),
    selected = selected_scope(),
    inline   = TRUE
  )
})

# Mirror every user change into the persistent reactiveVal
observeEvent(input$save_scope, ignoreInit = TRUE, {
  selected_scope(input$save_scope)
})


# ============================================================================
# Concentration buttons UI
# Uses selected_scope() (not input$save_scope) so the buttons remain stable
# while a long-running calculation causes concentration_calc_df() to re-run.
# ============================================================================
output$concentration_buttons_ui <- renderUI({
  df    <- concentration_calc_df()
  scope <- selected_scope()
  
  interp_status <- get_status(df, scope, "interpolated")
  mcmc_status   <- get_status(df, scope, "mcmc_robust")
  
  scope_label <- switch(scope,
                        "study"      = "(All Experiments)",
                        "experiment" = "(Current Experiment)",
                        "plate"      = "(Current Plate)"
  )
  
  interp_btn <- make_method_btn(
    status       = interp_status,
    input_id     = paste0("run_batch_fit_", scope),
    method_label = "Interpolated",
    scope_label  = scope_label
  )
  
  mcmc_btn <- if (interp_status %in% c("completed", "partially completed")) {
    make_method_btn(
      status       = mcmc_status,
      input_id     = paste0("run_mcmc_calc_", scope),
      method_label = "MCMC Robust",
      scope_label  = scope_label
    )
  } else {
    NULL
  }
  
  tagList(interp_btn, mcmc_btn)
})


# ============================================================================
# Interpolated concentration observers — one per scope
# ============================================================================
lapply(c("study", "experiment", "plate"), function(s) {
  
  observeEvent(input[[paste0("run_batch_fit_", s)]], ignoreInit = TRUE, {
    
    scope <- s
    
    if (is_batch_processing()) {
      showNotification("Batch processing is already running. Please wait.",
                       type = "warning", duration = 10, closeButton = TRUE)
      return()
    }
    
    req(
      input$qc_component                  == "Standard Curve",
      input$readxMap_study_accession      != "Click here",
      input$readxMap_experiment_accession != "Click here",
      input$study_level_tabs              == "Experiments",
      input$main_tabs                     == "view_files_tab"
    )
    
    if (is.null(input$readxMap_study_accession) ||
        input$readxMap_study_accession == "") {
      showNotification("Please select a study before running batch processing.",
                       type = "error", duration = 10, closeButton = TRUE)
      return()
    }
    
    scope_label <- switch(scope,
                          "study"      = "all experiments",
                          "experiment" = "current experiment",
                          "plate"      = "current plate"
    )
    
    # ── Rerun check: warn before overwriting existing interpolated results ──
    df        <- concentration_calc_df()
    is_rerun  <- get_status(df, scope, "interpolated") %in%
      c("completed", "partially completed")
    
    if (is_rerun) {
      showModal(modalDialog(
        title = tagList(
          tags$i(class = "fa fa-exclamation-triangle",
                 style = "color:#ffc107; margin-right:8px;"),
          "Confirm Interpolated Rerun"
        ),
        tagList(
          p(paste0(
            "Interpolated concentrations have already been calculated for ",
            scope_label, "."
          )),
          p(strong("Running again will overwrite all existing interpolated results for this scope.")),
          p("Are you sure you want to continue?")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            paste0("confirm_interp_rerun_", scope),
            label = tagList(
              tags$i(class = "fa fa-redo", style = "margin-right:5px;"),
              "Yes, Rerun Interpolated"
            ),
            class = "btn-warning"
          )
        ),
        easyClose = TRUE
      ))
      return()   # wait for confirmation observer below
    }
    
    # ── Not a rerun — proceed directly ──
    # .run_interpolated(scope, scope_label, session)
    .run_interpolated(
      scope       = scope,
      study       = input$readxMap_study_accession,
      experiment  = input$readxMap_experiment_accession,
      plate       = input$sc_plate_select,
      proj        = userWorkSpaceID(),
      current_user = currentuser(),
      scope_label = scope_label,
      session     = session
    )
  })
})

# ============================================================================
# Interpolated rerun confirmation observers — one per scope
# ============================================================================
lapply(c("study", "experiment", "plate"), function(s) {
  observeEvent(input[[paste0("confirm_interp_rerun_", s)]], ignoreInit = TRUE, {
    removeModal()
    
    scope_label <- switch(s,
                          "study"      = "all experiments",
                          "experiment" = "current experiment",
                          "plate"      = "current plate"
    )
    
    .run_interpolated(
      scope       = s,
      study       = input$readxMap_study_accession,
      experiment  = input$readxMap_experiment_accession,
      plate       = input$sc_plate_select,
      proj        = userWorkSpaceID(),
      current_user = currentuser(),
      scope_label = scope_label,
      session     = session
    )
  })
})

.run_interpolated <- function(scope, study, experiment, plate, proj,
                              current_user, scope_label, session) {
  is_batch_processing(TRUE)
  
  # ── Progress setup ──
  prog_file <- tempfile(pattern = "interp_progress_", fileext = ".txt")
  writeLines(paste0("Starting Interpolated...\nScope: ", scope_label), prog_file)
  interp_progress_file(prog_file)
  interp_progress_msg(paste0("Starting Interpolated...\nScope: ", scope_label))
  interp_pending_scopes(c(
    interp_pending_scopes(),
    list(c(scope = scope, method = "interpolated"))
  ))
  concentrationUIRefresher(concentrationUIRefresher() + 1)
  
  showNotification(
    id = "batch_sc_fit_notify",
    HTML(
      paste0(
        "<div class='big-notification'>",
        "Starting standard curves:<br>",
        "interpolated concentrations<br>",
        "for ", scope_label, "<span class='dots'></span>",
        "</div>"
      )
    ),
    duration = NULL
  )
  
  # ── ALL DATA LOADING ON MAIN THREAD ──
  headers <- fetch_db_header_experiments(
    study_accession = study, conn = conn
  )
  exp_list <- switch(scope,
                     "study"      = unique(headers$experiment_accession),
                     "experiment" = ,
                     "plate"      = experiment
  )
  
  loaded_data_list <- lapply(
    stats::setNames(exp_list, exp_list),
    function(exp) pull_data(
      study_accession      = study,
      experiment_accession = exp,
      project_id           = proj,
      conn                 = conn
    )
  )
  
  # ── Compute dil_series SE on FULL unfiltered standards (all plates) ──
  # Must happen BEFORE plate-scope filtering so we get proper cross-plate
  # replication (n >= min_reps) and valid SE estimates for FDA LOQ computation.
  all_standards_full <- do.call(rbind, lapply(loaded_data_list, `[[`, "standards"))
  response_var <- loaded_data_list[[exp_list[1]]]$response_var
  
  dil_series_se_table_batch <- compute_dil_series_se(
    standards_data = all_standards_full,
    response_col   = response_var,
    dilution_col   = "dilution",
    plate_col      = "plate_nom",
    grouping_cols  = c("project_id",
                       "study_accession",
                       "experiment_accession",
                       "source_nom",
                       "antigen",
                       "feature"),
    min_reps = 2,
    verbose  = FALSE
  )
  
  # ── NOW apply plate scope filter to loaded_data_list ──
  if (scope == "plate") {
    loaded_data_list <- lapply(loaded_data_list, function(x) {
      for (tbl in c("plates", "standards", "samples", "blanks")) {
        if (!is.null(x[[tbl]]) && nrow(x[[tbl]]) > 0)
          x[[tbl]] <- x[[tbl]][x[[tbl]]$plate_nom == plate, , drop = FALSE]
      }
      x
    })
  }
  
  all_standards <- do.call(rbind, lapply(loaded_data_list, `[[`, "standards"))
  
  se_antigen_table_batch <- compute_antigen_se_table(
    standards_data = all_standards,
    response_col   = response_var,
    dilution_col   = "dilution",
    plate_col      = "plate",
    grouping_cols  = c("project_id", "study_accession", "experiment_accession",
                       "source_nom", "antigen", "feature"),
    verbose        = FALSE
  )
  
  study_params_batch <- fetch_study_parameters(
    study_accession = study,
    param_user      = current_user,
    param_group     = "standard_curve_options",
    project_id      = proj,
    conn            = conn
  )
  
  model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
  
  antigen_list_res       <- build_antigen_list(exp_list, loaded_data_list, study)
  antigen_plate_list_res <- build_antigen_plate_list(antigen_list_res, loaded_data_list)
  prepped_data_list_res  <- prep_plate_data_batch(
    antigen_plate_list_res, study_params_batch, verbose = FALSE
  )
  antigen_plate_list_res$antigen_plate_list_ids <-
    prepped_data_list_res$antigen_plate_name_list
  n_total <- length(prepped_data_list_res$antigen_plate_name_list)
  
  db_conn_args <- get_db_connection_args()
  
  # ── FUTURE: Only fitting + DB writes ──
  future_promise <- future::future({
    bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
    on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
    
    batch_fit_res <- fit_experiment_plate_batch(
      prepped_data_list_res   = prepped_data_list_res,
      antigen_plate_list_res  = antigen_plate_list_res,
      model_names             = model_names,
      study_params            = study_params_batch,
      se_antigen_table        = se_antigen_table_batch,
      dil_series_se_table     = dil_series_se_table_batch,
      prog_file               = prog_file,
      dil_series_response_col = response_var,
      verbose                 = FALSE
    )
    
    batch_outputs <- create_batch_fit_outputs(
      batch_fit_res, antigen_plate_list_res
    )
    batch_outputs <- process_batch_outputs(
      batch_outputs, response_var, proj
    )
    
    # ── Debug: check FDA LOQ columns ──
    tryCatch(writeLines(
      paste0("Interpolated: saving best_glance_all...\nScope: ", scope_label),
      prog_file), error = function(e) NULL)
    
    message("[debug] best_glance_all columns: ",
            paste(names(batch_outputs$best_glance_all), collapse = ", "))
    
    lloq_check_cols <- grep("lloq_fda2018|uloq_fda2018",
                            names(batch_outputs$best_glance_all), value = TRUE)
    message("[debug] lloq cols present in best_glance_all: ",
            if (length(lloq_check_cols) == 0) "NONE"
            else paste(lloq_check_cols, collapse = ", "))
    
    if (length(lloq_check_cols) > 0) {
      message("[debug] lloq values (first 3 rows):")
      print(batch_outputs$best_glance_all[
        1:min(3, nrow(batch_outputs$best_glance_all)),
        lloq_check_cols, drop = FALSE])
      message("[debug] lloq NA counts:")
      print(colSums(is.na(
        batch_outputs$best_glance_all[, lloq_check_cols, drop = FALSE]
      )))
    }
    

    # ── Save best_glance_all first (parent table) ──
    upsert_best_curve(
      conn = bg_conn, df = batch_outputs$best_glance_all,
      schema = "madi_results", table = "best_glance_all",
      notify = NULL, shiny_mode = FALSE
    )
    
    # ── Fetch glance lookup for FK joins ──
    study_to_save   <- unique(batch_outputs$best_glance_all$study_accession)
    project_to_save <- unique(batch_outputs$best_glance_all$project_id)
    exp_list_sql    <- paste0("'", paste(exp_list, collapse = "','"), "'")
    
    glance_lookup <- DBI::dbGetQuery(bg_conn, glue::glue(
      "SELECT best_glance_all_id, project_id, study_accession, experiment_accession,
              plateid, plate, nominal_sample_dilution, source, antigen, feature, wavelength
       FROM madi_results.best_glance_all
       WHERE project_id = {project_to_save}
         AND study_accession = '{study_to_save}'
         AND experiment_accession IN ({exp_list_sql});"
    ))
    glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)
    
    keys <- c("project_id", "study_accession", "experiment_accession", "plateid",
              "plate", "nominal_sample_dilution", "source", "antigen", "feature", "wavelength")
    
    # ── Normalize wavelength in glance_lookup AND all child tables ──
    glance_lookup$wavelength <- normalize_wavelength(glance_lookup$wavelength)
    
    for (tbl_name in c("best_pred_all", "best_sample_se_all", "best_standard_all",
                       "best_plate_all", "best_tidy_all")) {
      if (!is.null(batch_outputs[[tbl_name]]) &&
          "wavelength" %in% names(batch_outputs[[tbl_name]])) {
        batch_outputs[[tbl_name]]$wavelength <-
          normalize_wavelength(batch_outputs[[tbl_name]]$wavelength)
      }
    }
    
    message(sprintf("[save] glance_lookup: %d rows", nrow(glance_lookup)))
    message(sprintf("[save] glance_lookup wavelengths: %s",
                    paste(unique(glance_lookup$wavelength), collapse = ", ")))
    
    # ── FK joins: attach best_glance_all_id to each child table ──
    if (!is.null(batch_outputs$best_pred_all) &&
        nrow(batch_outputs$best_pred_all) > 0) {
      n_before <- nrow(batch_outputs$best_pred_all)
      batch_outputs$best_pred_all <- dplyr::inner_join(
        batch_outputs$best_pred_all, glance_lookup, by = keys
      )
      n_after <- nrow(batch_outputs$best_pred_all)
      message(sprintf("[save] best_pred_all FK join: %d -> %d rows", n_before, n_after))
      if (n_after == 0 && n_before > 0)
        message("[save] WARNING: FK join dropped ALL best_pred_all rows — likely key mismatch.")
    }
    
    if (!is.null(batch_outputs$best_sample_se_all) &&
        nrow(batch_outputs$best_sample_se_all) > 0) {
      n_before <- nrow(batch_outputs$best_sample_se_all)
      batch_outputs$best_sample_se_all <- dplyr::inner_join(
        batch_outputs$best_sample_se_all, glance_lookup, by = keys
      )
      n_after <- nrow(batch_outputs$best_sample_se_all)
      message(sprintf("[save] best_sample_se_all FK join: %d -> %d rows", n_before, n_after))
      if (n_after == 0 && n_before > 0)
        message("[save] WARNING: FK join dropped ALL best_sample_se_all rows — likely key mismatch.")
    }
    
    if (!is.null(batch_outputs$best_standard_all) &&
        nrow(batch_outputs$best_standard_all) > 0) {
      n_before <- nrow(batch_outputs$best_standard_all)
      batch_outputs$best_standard_all <- dplyr::inner_join(
        batch_outputs$best_standard_all, glance_lookup, by = keys
      )
      n_after <- nrow(batch_outputs$best_standard_all)
      message(sprintf("[save] best_standard_all FK join: %d -> %d rows", n_before, n_after))
      if (n_after == 0 && n_before > 0)
        message("[save] WARNING: FK join dropped ALL best_standard_all rows — likely key mismatch.")
    }
    
    # ── Save all child tables ──
    for (pair in list(
      list(df = "best_plate_all",     table = "best_plate_all"),
      list(df = "best_tidy_all",      table = "best_tidy_all"),
      list(df = "best_pred_all",      table = "best_pred_all"),
      list(df = "best_sample_se_all", table = "best_sample_se_all"),
      list(df = "best_standard_all",  table = "best_standard_all")
    )) {
      tryCatch(writeLines(
        paste0("Interpolated: saving ", pair$table, "...\nScope: ", scope_label),
        prog_file), error = function(e) NULL)
      
      upsert_best_curve(
        conn = bg_conn, df = batch_outputs[[pair$df]],
        schema = "madi_results", table = pair$table,
        notify = NULL, shiny_mode = FALSE
      )
    }
    
    list(ok = TRUE, n_curves = n_total, scope_label = scope_label)
  }, seed = TRUE)
  
  removeNotification("batch_sc_fit_notify")
  
  progress_poller <- reactivePoll(
    intervalMillis = 2000, session = session,
    checkFunc = function() {
      pf <- interp_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(0)
      file.info(pf)$mtime
    },
    valueFunc = function() {
      pf <- interp_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(NULL)
      tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
    }
  )
  
  progress_observer <- observe({
    msg <- progress_poller()
    if (!is.null(msg) && nzchar(msg)) interp_progress_msg(msg)
  })
  
  .cleanup_interp <- function(label, type = "message", duration = 10) {
    progress_observer$destroy()
    pf <- interp_progress_file()
    if (!is.null(pf) && file.exists(pf)) file.remove(pf)
    interp_progress_file(NULL)
    interp_progress_msg(NULL)
    .remove_pending(interp_pending_scopes, scope, "interpolated")
    showNotification(label, type = type, duration = duration)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
  }
  
  promises::then(
    future_promise,
    onFulfilled = function(result) {
      .cleanup_interp(
        paste0("Interpolated completed for ", result$scope_label,
               " (", result$n_curves, " curves).")
      )
    },
    onRejected = function(err) {
      .cleanup_interp(
        paste0("Interpolated error: ", conditionMessage(err)),
        type = "error", duration = 15
      )
      message("Interpolated future rejected: ", conditionMessage(err))
    }
  )
  
  NULL
}
# 
# .run_interpolated <- function(scope, study, experiment, plate, proj,
#                               current_user, scope_label, session) {
#   is_batch_processing(TRUE)
#   
#   # ── Progress setup (same as before) ──
#   prog_file <- tempfile(pattern = "interp_progress_", fileext = ".txt")
#   writeLines(paste0("Starting Interpolated...\nScope: ", scope_label), prog_file)
#   interp_progress_file(prog_file)
#   interp_progress_msg(paste0("Starting Interpolated...\nScope: ", scope_label))
#   interp_pending_scopes(c(
#     interp_pending_scopes(),
#     list(c(scope = scope, method = "interpolated"))
#   ))
#   concentrationUIRefresher(concentrationUIRefresher() + 1)
#   
#   showNotification(
#     id = "batch_sc_fit_notify",
#     HTML(
#       paste0(
#         "<div class='big-notification'>",
#         "Starting standard curves:<br>",
#         "interpolated concentrations<br>",
#         "for ", scope_label, "<span class='dots'></span>",
#         "</div>"
#       )
#     ),
#     duration = NULL
#   )
#   
#   # ── ALL DATA LOADING ON MAIN THREAD (mirrors .launch_mcmc step 4) ──
#   headers <- fetch_db_header_experiments(
#     study_accession = study, conn = conn
#   )
#   exp_list <- switch(scope,
#                      "study"      = unique(headers$experiment_accession),
#                      "experiment" = ,
#                      "plate"      = experiment
#   )
#   
#   loaded_data_list <- lapply(
#     stats::setNames(exp_list, exp_list),
#     function(exp) pull_data(
#       study_accession      = study,
#       experiment_accession = exp,
#       project_id           = proj,
#       conn                 = conn
#     )
#   )
#   
#   # ── Compute dil_series SE on FULL unfiltered standards (all plates) ──
#   # Must happen BEFORE plate-scope filtering so we get proper cross-plate
#   # replication (n >= min_reps) and valid SE estimates for FDA LOQ computation.
#   all_standards_full <- do.call(rbind, lapply(loaded_data_list, `[[`, "standards"))
#   response_var <- loaded_data_list[[exp_list[1]]]$response_var
#   
#   dil_series_se_table_batch <- compute_dil_series_se(
#     standards_data = all_standards_full,
#     response_col   = response_var,
#     dilution_col   = "dilution",
#     plate_col      = "plate_nom",
#     grouping_cols  = c("project_id",
#                        "study_accession",
#                        "experiment_accession",
#                        "source_nom",
#                        "antigen",
#                        "feature"),
#     min_reps = 2,
#     verbose  = FALSE
#   )
#   
#   # ── NOW apply plate scope filter to loaded_data_list ──
#   if (scope == "plate") {
#     loaded_data_list <- lapply(loaded_data_list, function(x) {
#       for (tbl in c("plates", "standards", "samples", "blanks")) {
#         if (!is.null(x[[tbl]]) && nrow(x[[tbl]]) > 0)
#           x[[tbl]] <- x[[tbl]][x[[tbl]]$plate_nom == plate, , drop = FALSE]
#       }
#       x
#     })
#   }
#   
#   all_standards <- do.call(rbind, lapply(loaded_data_list, `[[`, "standards"))
#   
#   se_antigen_table_batch <- compute_antigen_se_table(
#     standards_data = all_standards,
#     response_col   = response_var,
#     dilution_col   = "dilution",
#     plate_col      = "plate",
#     grouping_cols  = c("project_id","study_accession", "experiment_accession",
#                        "source_nom", "antigen", "feature"),
#     verbose        = FALSE
#   )
#   
#   study_params_batch <- fetch_study_parameters(
#     study_accession = study,
#     param_user      = current_user,
#     param_group     = "standard_curve_options",
#     project_id      = proj,
#     conn            = conn
#   )
#   
#   model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
#   
#   antigen_list_res       <- build_antigen_list(exp_list, loaded_data_list, study)
#   antigen_plate_list_res <- build_antigen_plate_list(antigen_list_res, loaded_data_list)
#   prepped_data_list_res  <- prep_plate_data_batch(
#     antigen_plate_list_res, study_params_batch, verbose = FALSE
#   )
#   antigen_plate_list_res$antigen_plate_list_ids <-
#     prepped_data_list_res$antigen_plate_name_list
#   n_total <- length(prepped_data_list_res$antigen_plate_name_list)
#   
#   db_conn_args <- get_db_connection_args()
#   
#   # ── FUTURE: Only fitting + DB writes ──
#   future_promise <- future::future({
#     bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
#     on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
#     
#     batch_fit_res <- fit_experiment_plate_batch(
#       prepped_data_list_res  = prepped_data_list_res,
#       antigen_plate_list_res = antigen_plate_list_res,
#       model_names            = model_names,
#       study_params           = study_params_batch,
#       se_antigen_table       = se_antigen_table_batch,
#       dil_series_se_table    = dil_series_se_table_batch,
#       prog_file              = prog_file,
#       dil_series_response_col = response_var,
#       verbose                = FALSE
#     )
#     
#     batch_outputs <- create_batch_fit_outputs(
#       batch_fit_res, antigen_plate_list_res
#     )
#     batch_outputs <- process_batch_outputs(
#       batch_outputs, response_var, proj
#     )
#     
#     tryCatch(writeLines(
#       paste0("Interpolated: saving best_glance_all...\nScope: ", scope_label),
#       prog_file), error = function(e) NULL)
#     
#     message("[debug] best_glance_all columns: ",
#             paste(names(batch_outputs$best_glance_all), collapse = ", "))
#     
#     lloq_check_cols <- grep("lloq_fda2018|uloq_fda2018", names(batch_outputs$best_glance_all), value = TRUE)
#     message("[debug] lloq cols present in best_glance_all: ",
#             if (length(lloq_check_cols) == 0) "NONE" else paste(lloq_check_cols, collapse = ", "))
#     
#     if (length(lloq_check_cols) > 0) {
#       message("[debug] lloq values (first 3 rows):")
#       print(batch_outputs$best_glance_all[1:min(3, nrow(batch_outputs$best_glance_all)),
#                                           lloq_check_cols, drop = FALSE])
#       message("[debug] lloq NA counts:")
#       print(colSums(is.na(batch_outputs$best_glance_all[, lloq_check_cols, drop = FALSE])))
#     }
#     
#     ## debug in future needs to save 
#     saveRDS(batch_outputs$best_glance_all, "best_glance_debug.rds")
#     
#     upsert_best_curve(
#       conn = bg_conn, df = batch_outputs$best_glance_all,
#       schema = "madi_results", table = "best_glance_all",
#       notify = NULL, shiny_mode = FALSE
#     )
#     
#     study_to_save   <- unique(batch_outputs$best_glance_all$study_accession)
#     project_to_save <- unique(batch_outputs$best_glance_all$project_id)
#     
#     exp_list_sql <- paste0("'", paste(exp_list, collapse = "','"), "'")
#     glance_lookup <- DBI::dbGetQuery(bg_conn, glue::glue(
#       "SELECT best_glance_all_id, project_id, study_accession, experiment_accession,
#               plateid, plate, nominal_sample_dilution, source, antigen, feature, wavelength
#        FROM madi_results.best_glance_all
#        WHERE project_id = {project_to_save}
#          AND study_accession = '{study_to_save}'
#          AND experiment_accession IN ({exp_list_sql});"
#     ))
#     glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)
#     
#     keys <- c("project_id","study_accession", "experiment_accession", "plateid",
#               "plate", "nominal_sample_dilution", "source", "antigen", "feature", "wavelength")
#     
#     glance_lookup$wavelength <- normalize_wavelength(glance_lookup$wavelength)
#     
#     message(sprintf("[save] glance_lookup: %d rows, keys: %s",
#                     nrow(glance_lookup), paste(keys, collapse = ", ")))
#     message(sprintf("[save] glance_lookup wavelengths: %s",
#                     paste(unique(glance_lookup$wavelength), collapse = ", ")))
#     
#     if (!is.null(batch_outputs$best_pred_all) && nrow(batch_outputs$best_pred_all) > 0) {
#       n_before <- nrow(batch_outputs$best_pred_all)
#       batch_outputs$best_pred_all <- dplyr::inner_join(batch_outputs$best_pred_all, glance_lookup, by = keys)
#       n_after <- nrow(batch_outputs$best_pred_all)
#       message(sprintf("[save] best_pred_all FK join: %d -> %d rows", n_before, n_after))
#       if (n_after == 0 && n_before > 0)
#         message("[save] WARNING: FK join dropped ALL best_pred_all rows — likely wavelength mismatch.")
#     }
#     
#     if (!is.null(batch_outputs$best_sample_se_all) && nrow(batch_outputs$best_sample_se_all) > 0) {
#       n_before <- nrow(batch_outputs$best_sample_se_all)
#       batch_outputs$best_sample_se_all <- dplyr::inner_join(batch_outputs$best_sample_se_all, glance_lookup, by = keys)
#       n_after <- nrow(batch_outputs$best_sample_se_all)
#       message(sprintf("[save] best_sample_se_all FK join: %d -> %d rows", n_before, n_after))
#       if (n_after == 0 && n_before > 0)
#         message("[save] WARNING: FK join dropped ALL best_sample_se_all rows — likely wavelength mismatch.")
#     }
#     
#     if (!is.null(batch_outputs$best_standard_all) && nrow(batch_outputs$best_standard_all) > 0) {
#       n_before <- nrow(batch_outputs$best_standard_all)
#       batch_outputs$best_standard_all <- dplyr::inner_join(batch_outputs$best_standard_all, glance_lookup, by = keys)
#       n_after <- nrow(batch_outputs$best_standard_all)
#       message(sprintf("[save] best_standard_all FK join: %d -> %d rows", n_before, n_after))
#       if (n_after == 0 && n_before > 0)
#         message("[save] WARNING: FK join dropped ALL best_standard_all rows — likely wavelength mismatch.")
#     }
#     
#     for (tbl_name in c("best_pred_all", "best_sample_se_all", "best_standard_all")) {
#       batch_outputs[[tbl_name]] <- dplyr::inner_join(
#         batch_outputs[[tbl_name]], glance_lookup, by = keys
#       )
#     }
#     
#     for (pair in list(
#       list(df = "best_plate_all",     table = "best_plate_all"),
#       list(df = "best_tidy_all",      table = "best_tidy_all"),
#       list(df = "best_pred_all",      table = "best_pred_all"),
#       list(df = "best_sample_se_all", table = "best_sample_se_all"),
#       list(df = "best_standard_all",  table = "best_standard_all")
#     )) {
#       tryCatch(writeLines(
#         paste0("Interpolated: saving ", pair$table, "...\nScope: ", scope_label),
#         prog_file), error = function(e) NULL)
#       
#       upsert_best_curve(
#         conn = bg_conn, df = batch_outputs[[pair$df]],
#         schema = "madi_results", table = pair$table,
#         notify = NULL, shiny_mode = FALSE
#       )
#     }
#     
#     list(ok = TRUE, n_curves = n_total, scope_label = scope_label)
#   }, seed = TRUE)
#   
#   removeNotification("batch_sc_fit_notify")
#   
#   progress_poller <- reactivePoll(
#     intervalMillis = 2000, session = session,
#     checkFunc = function() {
#       pf <- interp_progress_file()
#       if (is.null(pf) || !file.exists(pf)) return(0)
#       file.info(pf)$mtime
#     },
#     valueFunc = function() {
#       pf <- interp_progress_file()
#       if (is.null(pf) || !file.exists(pf)) return(NULL)
#       tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
#     }
#   )
#   
#   progress_observer <- observe({
#     msg <- progress_poller()
#     if (!is.null(msg) && nzchar(msg)) interp_progress_msg(msg)
#   })
#   
#   .cleanup_interp <- function(label, type = "message", duration = 10) {
#     progress_observer$destroy()
#     pf <- interp_progress_file()
#     if (!is.null(pf) && file.exists(pf)) file.remove(pf)
#     interp_progress_file(NULL)
#     interp_progress_msg(NULL)
#     .remove_pending(interp_pending_scopes, scope, "interpolated")
#     showNotification(label, type = type, duration = duration)
#     concentrationUIRefresher(concentrationUIRefresher() + 1)
#     is_batch_processing(FALSE)
#   }
#   
#   promises::then(
#     future_promise,
#     onFulfilled = function(result) {
#       .cleanup_interp(
#         paste0("Interpolated completed for ", result$scope_label,
#                " (", result$n_curves, " curves).")
#       )
#     },
#     onRejected = function(err) {
#       .cleanup_interp(
#         paste0("Interpolated error: ", conditionMessage(err)),
#         type = "error", duration = 15
#       )
#       message("Interpolated future rejected: ", conditionMessage(err))
#     }
#   )
#   
#   NULL
# }

# ============================================================================
# .launch_mcmc — async helper (future + promises + progress polling)
# Called by both the initial MCMC observer and the rerun confirmation observer.
# ============================================================================
.launch_mcmc <- function(scope, study, experiment, plate, proj,
                         scope_label, session) {
  
  is_batch_processing(TRUE)
  
  # ── 1. Temp file for IPC progress messages ──────────────────────────────
  prog_file <- tempfile(pattern = "mcmc_progress_", fileext = ".txt")
  #writeLines("Starting MCMC Robust...", prog_file)
  mcmc_progress_file(prog_file)
  mcmc_progress_msg(paste0(
    "Running MCMC Robust\n",
    "Scope: ",      scope_label, "\n",
    "Study: ",      study,       "\n",
    "Experiment: ", experiment
  ))
  
  # ── 2. Inject "pending" into in-memory overlay so UI updates immediately ─
  current_pending <- mcmc_pending_scopes()
  mcmc_pending_scopes(c(
    current_pending,
    list(list(scope = scope, method = "mcmc_robust"))
  ))
  concentrationUIRefresher(concentrationUIRefresher() + 1)
  
  # ── 3. Persistent running notification ──────────────────────────────────
  showNotification(
    id  = "mcmc_calc_notify",
    div(class = "big-notification",
        paste0("Starting MCMC Robust for ", scope_label, "...")),
    duration = 10
  )
  
  # ── 4. Fetch data snapshots in the main session before handing off ───────
  best_glance_snapshot <- tryCatch(
    fetch_best_glance_mcmc(
      study_accession = study,
      project_id      = proj,
      conn            = conn
    ),
    error = function(e) NULL
  )
  
  if (is.null(best_glance_snapshot) || nrow(best_glance_snapshot) == 0) {
    showNotification("No fitted curves found.", type = "warning")
    .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
    mcmc_progress_msg(NULL)
    mcmc_progress_file(NULL)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
    return()
  }
  
  best_glance_snapshot <- filter_glance_scope(
    best_glance_snapshot, scope, experiment, plate
  )
  
  if (nrow(best_glance_snapshot) == 0) {
    showNotification("No fitted curves found for this scope.", type = "warning")
    .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
    mcmc_progress_msg(NULL)
    mcmc_progress_file(NULL)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
    return()
  }
  
  id_set  <- best_glance_snapshot$best_glance_all_id
  study <- study
  proj <- proj
  n_total <- length(id_set)
  
  message("IDs: ", paste(id_set, collapse = ", "))
  message("Study: ", study)
  message("Project: ", proj)
  
  
  combined_df_snapshot <- tryCatch(
    fetch_combined_mcmc(
      study_accession = study,
      project_id      = proj,
      best_glance_ids = id_set,
      conn            = conn
    ),
    error = function(e) NULL
  )
  
  if (is.null(combined_df_snapshot) || nrow(combined_df_snapshot) == 0) {
    showNotification("No prediction data found for MCMC.", type = "error")
    .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
    mcmc_progress_msg(NULL)
    mcmc_progress_file(NULL)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
    return()
  }
  
  # Snapshot DB connection args so the future can open its own connection
  db_conn_args <- get_db_connection_args()
  
  # ── 5. Launch future (runs in a separate process) ────────────────────────
  future_promise <- future::future({
    
    bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
    on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
    
    results     <- vector("list", length(id_set))
    best_glance <- best_glance_snapshot
    
    for (i in seq_along(id_set)) {
      
      id  <- id_set[i]
      row <- best_glance[best_glance$best_glance_all_id == id, ]
      
      # Write progress so the main session poller can read it
      progress_text <- paste0(
        "MCMC Robust: ", i, " / ", n_total,       "\n",
        "Study:      ", row$study_accession,       "\n",
        "Experiment: ", row$experiment_accession,  "\n",
        "Plate:      ", row$plate_nom,             "\n",
        "Antigen:    ", row$antigen,               "\n",
        "Model:      ", row$model_name
      )
      tryCatch(writeLines(progress_text, prog_file), error = function(e) NULL)
      message(progress_text)
      
      curve_df <- combined_df_snapshot[
        combined_df_snapshot$best_glance_all_id == id, ]
      pred_df  <- curve_df[curve_df$mcmc_set == "pred_se", ]
      if (nrow(pred_df) == 0) next
      
      res <- tryCatch(
        run_jags_predicted_concentration(
          glance_row   = row,
          best_pred_df = pred_df,
          sample_df    = curve_df,
          response_col = "assay_response",
          verbose      = TRUE
        ),
        error = function(e) {
          message("JAGS error for ID ", id, ": ", e$message)
          NULL
        }
      )
      
      if (!is.null(res)) {
        if (!"mcmc_set" %in% names(res)) {
          res$mcmc_set <- curve_df$mcmc_set[match(res$row_id, curve_df$row_id)]
        }
        results[[i]] <- res
      }
    }
    
    # Combine all results
    results_df <- do.call(rbind, Filter(Negate(is.null), results))
    if (is.null(results_df) || nrow(results_df) == 0) stop("MCMC produced no results.")
    
    result_pred_all   <- results_df[results_df$mcmc_set == "pred_se",   ]
    result_sample_all <- results_df[results_df$mcmc_set == "sample_se", ]
    
    result_sample_all$final_robust_concentration <-
      result_sample_all$dilution * result_sample_all$raw_robust_concentration
    
    best_glance$last_concentration_calc_method[
      best_glance$best_glance_all_id %in% id_set
    ] <- "mcmc_robust"
    
    result_pred_all2   <- process_jag_result(result_pred_all,   df_name = "pred_se")
    result_sample_all2 <- process_jag_result(result_sample_all, df_name = "sample_se")
    
    update_combined_mcmc_bulk(
      pred_all_mcmc        = result_pred_all2,
      sample_all_mcmc      = result_sample_all2,
      best_glance_complete = best_glance,
      conn                 = bg_conn
    )
    
    list(ok = TRUE, n_curves = nrow(best_glance), scope_label = scope_label)
    
  }, seed = TRUE)
  
  # ── 6. Poll the progress file every 2 s while the future runs ───────────
  progress_poller <- reactivePoll(
    intervalMillis = 2000,
    session        = session,
    
    checkFunc = function() {
      pf <- mcmc_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(0)
      file.info(pf)$mtime
    },
    
    valueFunc = function() {
      pf <- mcmc_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(NULL)
      tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
    }
  )
  
  # Keep the poller alive by observing it
  progress_observer <- observe({
    msg <- progress_poller()
    if (!is.null(msg) && nzchar(msg)) mcmc_progress_msg(msg)
  })
  
  # ── 7. Handle promise resolution ────────────────────────────────────────
  promises::then(
    future_promise,
    
    onFulfilled = function(result) {
      progress_observer$destroy()
      
      pf <- mcmc_progress_file()
      if (!is.null(pf) && file.exists(pf)) file.remove(pf)
      mcmc_progress_file(NULL)
      mcmc_progress_msg(NULL)
      
      .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
      
      showNotification(
        paste0("MCMC Robust completed for ", result$scope_label, "."),
        type = "message", duration = 10
      )
      
      concentrationUIRefresher(concentrationUIRefresher() + 1)
      is_batch_processing(FALSE)
    },
    
    onRejected = function(err) {
      progress_observer$destroy()
      
      pf <- mcmc_progress_file()
      if (!is.null(pf) && file.exists(pf)) file.remove(pf)
      mcmc_progress_file(NULL)
      mcmc_progress_msg(NULL)
      
      .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
      
      showNotification(
        paste0("MCMC Robust error: ", conditionMessage(err)),
        type = "error", duration = 15
      )
      message("MCMC future rejected: ", conditionMessage(err))
      
      concentrationUIRefresher(concentrationUIRefresher() + 1)
      is_batch_processing(FALSE)
    }
  )
  
  NULL
}


# ============================================================================
# MCMC Robust observers — one per scope
# ============================================================================
lapply(c("study", "experiment", "plate"), function(s) {
  
  # ── Initial click ──
  observeEvent(input[[paste0("run_mcmc_calc_", s)]], ignoreInit = TRUE, {
    
    scope      <- s
    study      <- input$readxMap_study_accession
    experiment <- input$readxMap_experiment_accession
    plate      <- input$sc_plate_select
    proj       <- userWorkSpaceID()
    
    if (is_batch_processing()) {
      showNotification("Batch processing is already running.",
                       type = "warning", duration = 10)
      return()
    }
    
    req(input$qc_component == "Standard Curve",
        study != "Click here", experiment != "Click here")
    
    df <- concentration_calc_df()
    
    if (!(get_status(df, scope, "interpolated") %in%
          c("completed", "partially completed"))) {
      showNotification("Interpolated concentrations must be completed first.",
                       type = "error", duration = 10)
      return()
    }
    
    is_rerun <- get_status(df, scope, "mcmc_robust") %in%
      c("completed", "partially completed")
    
    scope_label <- c(
      study      = "all experiments",
      experiment = "current experiment",
      plate      = "current plate"
    )[scope]
    
    if (is_rerun) {
      # Show confirmation modal before overwriting
      showModal(modalDialog(
        title = tagList(
          tags$i(class = "fa fa-exclamation-triangle",
                 style = "color:#ffc107; margin-right:8px;"),
          "Confirm MCMC Rerun"
        ),
        tagList(
          p(paste0(
            "MCMC Robust concentrations have already been calculated for ",
            scope_label, "."
          )),
          p(strong("Running again will overwrite all existing MCMC results for this scope.")),
          p("Are you sure you want to continue?")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            paste0("confirm_mcmc_rerun_", scope),
            label = tagList(
              tags$i(class = "fa fa-redo", style = "margin-right:5px;"),
              "Yes, Rerun MCMC"
            ),
            class = "btn-warning"
          )
        ),
        easyClose = TRUE
      ))
      return()   # wait for confirmation observer below
    }
    
    # Not a rerun — proceed immediately
    .launch_mcmc(
      scope       = scope,
      study       = study,
      experiment  = experiment,
      plate       = plate,
      proj        = proj,
      scope_label = scope_label,
      session     = session
    )
  })
})


# ============================================================================
# MCMC Rerun confirmation observers — one per scope
# ============================================================================
lapply(c("study", "experiment", "plate"), function(s) {
  
  observeEvent(input[[paste0("confirm_mcmc_rerun_", s)]], ignoreInit = TRUE, {
    removeModal()
    
    scope_label <- c(
      study      = "all experiments",
      experiment = "current experiment",
      plate      = "current plate"
    )[s]
    
    .launch_mcmc(
      scope       = s,
      study       = input$readxMap_study_accession,
      experiment  = input$readxMap_experiment_accession,
      plate       = input$sc_plate_select,
      proj        = userWorkSpaceID(),
      scope_label = scope_label,
      session     = session
    )
  })
})