# ============================================================================
# Navigation Observer - handles UI setup when navigating to Standard Curve tab
# ============================================================================
observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$qc_component,
  input$study_level_tabs,
  input$main_tabs), {
    
    req(input$qc_component == "Standard Curve",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")
    
    
    if (input$qc_component == "Standard Curve") {
      selected_study <- input$readxMap_study_accession
      selected_experiment <- input$readxMap_experiment_accession
      
      verbose = FALSE
      model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
      param_group = "standard_curve_options"
      allowed_constraint_methods = c("default","user_defined","range_of_blanks", "geometric_mean_of_blanks")
      
      loaded_data <- pull_data(study_accession = selected_study,
                               experiment_accession = selected_experiment,
                               project_id = userWorkSpaceID(),
                               conn = conn)
      
      #  loaded_data <<- loaded_data
      
      response_var <- loaded_data$response_var
      indep_var <-  loaded_data$indep_var
      
      # se_std_response <- assay_se(loaded_data$standards,
      #                             dilution_col = "dilution",
      #                             response_col = response_var)
      se_antigen_table <- compute_antigen_se_table(
        standards_data = loaded_data$standards,
        response_col = response_var,
        dilution_col = "dilution",
        plate_col = "plate",
        grouping_cols = c("study_accession", "experiment_accession", "source", "antigen"),
        method = "pooled_within",
        verbose = TRUE
      )
      
      study_params <- fetch_study_parameters(study_accession = selected_study,
                                             param_user = currentuser(),
                                             param_group =param_group,
                                             project_id = userWorkSpaceID(),
                                             conn = conn)
      
      
      output$std_curver_ui <- renderUI({
        tagList(
          div(
            style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
                              padding: 10px; margin-bottom: 15px; border-radius: 5px;",
            tags$h4("Current Standard Curve Context", style = "margin-top: 0; color: #2c5aa0;"),
            uiOutput("standard_curve_context"),
          ),
          conditionalPanel(
            condition = "output.can_fit_standard_curve == true",
            
            fluidRow(
              column(3, uiOutput("sc_plate_selector")),
              column(3, uiOutput("sc_antigen_selector")),
              column(3, uiOutput("sc_source_selector"))
            ),
            fluidRow(
              column(3, actionButton("show_comparisions", "Show Model Comparisons")),
              column(3, downloadButton("download_best_fit_parameter_estimates", "Download Parameter Estimates for Selected Fit")),
              column(3, downloadButton("download_samples_above_ulod", "Download Samples above the Upper Limit of Detection")),
              column(3, downloadButton("download_samples_below_llod", "Download Samples below the Lower Limit of Detection"))
            ),
            #verbatimTextOutput("print_list"),
            fluidRow(
              column(3,uiOutput("is_display_log_response")),
              column(3, uiOutput("is_display_log_independent_variable"))
            ),
            shinycssloaders::withSpinner(
              plotlyOutput("standard_curve", width = "75vw", height = "800px"),
              type = 6,
              color = "#4a90e2",
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
                  
                  tags$dt(tags$strong("Samples")),
                  tags$dd("Unknown test samples interpolated against the standard curve to determine their concentrations based on measured assay response values."),
                  
                  tags$dt(tags$strong("Fitted Curve")),
                  tags$dd("The fitted sigmoidal curve to the standard data points."),
                  
                  tags$dt(tags$strong("95% CI (Confidence Interval)")),
                  tags$dd("The 95% CI around the fitted standard curve."),
                  
                  tags$dt(tags$strong("Lower and Upper LODs (Limit of Detection)")),
                  tags$dd(
                    "Lower and upper LODs are defined as the upper 97.5% confidence bound of the lower asymptote and the lower 2.5% confidence bound of the upper asymptote, respectively ",
                    tags$a(
                      href = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
                      "(Rajam et al.).",
                      target = "_blank"
                    ),
                    " Limits of Detection correspond to the y-coordinate in the legend, as they are defined on the response axis."
                  ),
                  
                  tags$dt(tags$strong("Mininum Detectable Concentration")),
                  tags$dd(
                    "The smallest antibody concentration that produces a signal the assay can detect above background ",
                    tags$a(
                      href = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
                      "(Rajam et al.).",
                      target = "_blank"
                    ),
                    " This corresponds to the x-coordinate of the Lower Limit of Detection in the legend, as it is on the concentration axis."
                  ),
                  
                  tags$dt(tags$strong("Lower and Upper RDL (Reliable Detection Limit)")),
                  tags$dd(
                    "Lower RDL: The lowest concentration at which the assay consistently produces a signal above background with 95% confidence based on the fit of the standard curve ",
                    tags$a(
                      href = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
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
                      href = "Daly%20et.%20al%202005_BMCBioinformatics_Evaluating%20concentration%20estimation%20errors%20in%20ELISA%20microarray%20experiments1471-2105-6-17.pdf",
                      "(Daly et al.)",
                      target = "_blank"
                    ),
                    ", ",
                    tags$a(
                      href = "LinearPortion_BendPoints.pdf",
                      "(Jeanne L Sebaugh and P. D. McCray)",
                      target = "_blank"
                    ),
                    ", ",
                    tags$a(
                      href = "drLumi-An_open-source_package_to_manage_data_calibrate_and_conduct_quality_control_of_multiplex_bead-based_immunoassays_data_analysis.pdf",
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
                      href = "Daly%20et.%20al%202005_BMCBioinformatics_Evaluating%20concentration%20estimation%20errors%20in%20ELISA%20microarray%20experiments1471-2105-6-17.pdf",
                      "(Daly et al.).",
                      target = "_blank"
                    )
                  ),
                  
                  tags$dt(tags$strong("pCoV Threshold")),
                  tags$dd("The acceptable cutoff for the predicted concentration coefficient of variation."),
                  
                  tags$dt(tags$strong("Inflection Point")),
                  tags$dd(
                    "The point on the standard curve where the concavity transitions from concave up to concave down.
           It is the point where the assay is most sensitive to measurement errors in the measured response of the assay."
                  )
                )
              )
            ),
            
            
            # # div(class = "table-container",tableOutput("parameter_estimates")),
            div(class = "table-container",tableOutput("summary_statistics")),
            uiOutput("concentrationMethodUI")
            # radioButtons(
            #   inputId = "save_scope",
            #   label = "Calculation scope",
            #   choices = c(
            #     "Current plate"      = "plate",
            #     "Current experiment" = "experiment",
            #     "All experiments"    = "study"
            #   ),
            #   selected = "study",
            #   inline = TRUE
            # ),
            # actionButton("run_batch_fit", "Calculate Standard Curves")
            #uiOutput("run_batch_fit")
            
          )
          # div(class = "table-container",tableOutput("samples_above_ulod")),
          # div(class = "table-container",tableOutput("samples_below_llod"))
          # uiOutput("sc_antigen_selector"),
          # uiOutput("sc_source_selector"),
          # output$summary_table <- renderTable({
          #   fit_summary()
          # }),
          # plotOutput("model_comparisions")
        )
      })
      
      
      
      
      
      
      # ── Shared prerequisites check (computed once, used by context UI and conditional panel) ──
      sc_prereqs <- reactive({
        standards_n         <- nrow(loaded_data$standards[loaded_data$standards$experiment_accession == selected_experiment, ])
        blanks_n            <- nrow(loaded_data$blanks[loaded_data$blanks$experiment_accession == selected_experiment, ])
        blank_option        <- study_params$blank_option
        constraints_methods <- unique(loaded_data$antigen_constraints$l_asy_constraint_method)
        invalid_constraints <- setdiff(constraints_methods, allowed_constraint_methods)
        
        standards_missing   <- standards_n == 0
        blanks_missing      <- blanks_n == 0
        constraints_invalid <- length(invalid_constraints) > 0
        blank_required      <- blank_option != "ignored"
        blank_blocking      <- blanks_missing && blank_required
        
        list(
          standards_n         = standards_n,
          blanks_n            = blanks_n,
          blank_option        = blank_option,
          constraints_methods = constraints_methods,
          invalid_constraints = invalid_constraints,
          standards_missing   = standards_missing,
          blanks_missing      = blanks_missing,
          constraints_invalid = constraints_invalid,
          blank_required      = blank_required,
          blank_blocking      = blank_blocking
        )
      })
      
      output$standard_curve_context <- renderUI({
        p <- sc_prereqs()
        
        blank_labels <- c(
          ignored        = "Ignored",
          included       = "Included",
          subtracted     = "Subtracted 1 × Geometric Mean",
          subtracted_3x  = "Subtracted 3 × Geometric Mean",
          subtracted_5x  = "Subtracted 5 × Geometric Mean",
          subtracted_10x = "Subtracted 10 × Geometric Mean"
        )
        blank_label <- blank_labels[[p$blank_option]]
        
        reasons <- c()
        if (p$standards_missing)
          reasons <- c(reasons, glue::glue("{p$standards_n} standards found in {selected_experiment}."))
        if (p$constraints_invalid)
          reasons <- c(reasons, glue::glue("Invalid constraint methods: {paste(p$invalid_constraints, collapse=', ')}"))
        if (p$blank_blocking)
          reasons <- c(reasons, glue::glue(
            "Blank control is set to {blank_label} in the study settings but {p$blanks_n} blanks found."))
        
        blank_note <- if (!p$blank_blocking) {
          glue::glue(" (ok since blank control is currently set to {blank_label} in study settings.)")
        } else {
          ""
        }
        
        if (length(reasons) > 0) {
          return(HTML(glue::glue(
            "Standard curve fitting cannot proceed for {selected_experiment}.<br><br>",
            " Unmet requirements:<br>",
            "{paste(paste0('&nbsp;- ', reasons), collapse = '<br>')}<br><br>",
            "Details:<br>",
            "- Standards: {p$standards_n}<br>",
            "- Blanks: {p$blanks_n}{blank_note}<br>",
            "- Constraint method(s) found in {selected_experiment}: {paste(p$constraints_methods, collapse=', ')}<br>",
            "- Constraint method(s) valid: {ifelse(p$constraints_invalid, 'no', 'yes')}"
          )))
        }
        
        return(HTML(glue::glue(
          "{p$standards_n} standards found for {selected_experiment}.<br>",
          "{p$blanks_n} blanks found for {selected_experiment}.<br>",
          "Constraint method(s) found in {selected_experiment}: {paste(p$constraints_methods, collapse=', ')}<br>",
          "Constraint method(s) valid: {ifelse(p$constraints_invalid, 'no', 'yes')}<br>",
          "Current Blank Option selected (in study settings): {blank_label}<br><br>",
          "Standard curve fitting may proceed."
        )))
      })
      
      # reactive and exported output for conditional Panel
      can_fit_standard_curve <- reactive({
        p <- sc_prereqs()
        reasons <- c()
        if (p$standards_missing)
          reasons <- c(reasons, glue::glue("{p$standards_n} standards found in {selected_experiment}."))
        if (p$constraints_invalid)
          reasons <- c(reasons, glue::glue("Invalid constraint methods: {paste(p$invalid_constraints, collapse=', ')}"))
        if (p$blank_blocking)
          reasons <- c(reasons, glue::glue("Blanking is '{p$blank_option}' but {p$blanks_n} blanks found."))
        !(length(reasons) > 0)
      })
      
      output$can_fit_standard_curve <- reactive({
        can_fit_standard_curve()
      })
      
      outputOptions(output, "can_fit_standard_curve", suspendWhenHidden = FALSE)
      
      output$sc_plate_selector <- renderUI({
        req(loaded_data$standards$study_accession, loaded_data$standards$experiment_accession)
        
        
        
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
        
        dat_antigen <- loaded_data$standards[loaded_data$standards$study_accession %in% selected_study &
                                               loaded_data$standards$experiment_accession %in% selected_experiment &
                                               loaded_data$standards$plate_nom %in% input$sc_plate_select, ]
        
        dat_antigen <- dat_antigen[!is.na(dat_antigen$mfi),]
        req(nrow(dat_antigen) > 0)
        
        
        selectInput("sc_antigen_select",
                    label = "Antigen",
                    choices = unique(dat_antigen$antigen))
      })
      
      output$sc_source_selector <- renderUI({
        req(loaded_data$standards, input$sc_plate_select, input$sc_antigen_select)
        
        dat_source <- loaded_data$standards[
          loaded_data$standards$study_accession %in% selected_study &
            loaded_data$standards$experiment_accession %in% selected_experiment &
            loaded_data$standards$plate_nom %in% input$sc_plate_select &
            loaded_data$standards$antigen %in% input$sc_antigen_select, ]
        
        req(nrow(dat_source) > 0)
        
        radioButtons(
          "sc_source_select",
          label = "Source",
          choices = unique(dat_source$source),
          selected = unique(dat_source$source)[1]
        )
      })
      
      # ── Loading notification: show when plate/antigen changes, dismiss when curve is ready ──
      sc_loading_id <- reactiveVal(NULL)
      
      observeEvent(list(input$sc_plate_select, input$sc_antigen_select, input$sc_source_select), {
        req(input$sc_plate_select, input$sc_antigen_select)
        
        # Dismiss any existing notification first
        if (!is.null(sc_loading_id())) {
          removeNotification(sc_loading_id())
          sc_loading_id(NULL)
        }
        
        id <- showNotification(
          ui       = tagList(
            tags$b("Fitting standard curve..."),
            tags$br(),
            tags$span(
              style = "font-size: 0.9em; color: #555;",
              paste0("Plate: ", input$sc_plate_select, " | Antigen: ", input$sc_antigen_select)
            )
          ),
          duration = NULL,
          closeButton = FALSE,
          type = "message",
          id = "sc_loading"
        )
        sc_loading_id(id)
      }, ignoreInit = TRUE)
      
      observeEvent(best_fit(), {
        if (!is.null(sc_loading_id())) {
          removeNotification(sc_loading_id())
          sc_loading_id(NULL)
        }
      })
      
      antigen_plate <- reactive({
        req(
          loaded_data,
          loaded_data$antigen_constraints,
          input$sc_source_select,
          input$sc_antigen_select,
          input$sc_plate_select
        )
        
        antigen_constraints <- loaded_data$antigen_constraints[
          loaded_data$antigen_constraints$antigen %in% input$sc_antigen_select,
          , drop = FALSE]
        req(nrow(antigen_constraints) > 0)
        
        result <- select_antigen_plate(
          loaded_data = loaded_data,
          study_accession = selected_study,
          experiment_accession = selected_experiment,
          source = input$sc_source_select,
          antigen = input$sc_antigen_select,
          plate = input$sc_plate_select,
          antigen_constraints = antigen_constraints
        )
        req(result)
        result
      })
      
      prepped_data <- reactive({
        plate <- antigen_plate()
        req(plate)
        response_var <- loaded_data$response_var
        indep_var <- loaded_data$indep_var
        req(response_var, indep_var)
        
        preprocess_robust_curves(
          data = plate$plate_standard,
          antigen_settings = plate$antigen_settings,
          response_variable = response_var,
          independent_variable = indep_var,
          is_log_response = study_params$is_log_response,
          blank_data = plate$plate_blanks,
          blank_option = study_params$blank_option,
          is_log_independent = study_params$is_log_independent,
          apply_prozone = study_params$applyProzone,
          verbose = verbose
        )
      })
      
      formulas <- reactive({
        plate <- antigen_plate()
        response_var <- loaded_data$response_var
        req(plate)
        
        select_model_formulas(
          fixed_constraint = plate$fixed_a_result,
          response_variable = response_var,
          is_log_response = study_params$is_log_response
        )
      })
      #
      model_constraints <- reactive({
        plate <- antigen_plate()
        pdata <- prepped_data()
        f <- formulas()
        response_var <- loaded_data$response_var
        indep_var <- loaded_data$indep_var
        
        req(plate, pdata, f, response_var, indep_var)
        
        #pdata_v <<- pdata
        
        obtain_model_constraints(
          data = pdata$data,
          formulas = f,
          independent_variable = indep_var,
          response_variable = response_var,
          is_log_response = TRUE,
          is_log_concentration = TRUE,
          antigen_settings = plate$antigen_settings,
          max_response = max(pdata$data[[response_var]], na.rm = TRUE),
          min_response = min(pdata$data[[response_var]], na.rm = TRUE)
        )
      })
      
      
      start_lists <- reactive({
        mc <- model_constraints()
        req(mc)
        
        make_start_lists(
          model_constraints = mc,
          frac_generate = 0.8,
          quants = c(low = 0.2, mid = 0.5, high = 0.8)
        )
      })
      
      fit_robust_lm <- reactive({
        pdata <- prepped_data()
        f <- formulas()
        mc <- model_constraints()
        sl <- start_lists()
        response_var <- loaded_data$response_var
        indep_var <- loaded_data$indep_var
        
        req(pdata, f, mc, sl)
        
        compute_robust_curves(
          prepped_data = pdata$data,
          response_variable = response_var,
          independent_variable = indep_var,
          formulas = f,
          model_constraints = mc,
          start_lists = sl,
          verbose = verbose
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
        summarize_model_parameters(models_fit_list = fit,
                                   level = 0.95,
                                   model_names = model_names)
      })
      #
      plot_data <- reactive({
        pdata    <- prepped_data()
        fit      <- fit_robust_lm()
        plate    <- antigen_plate()
        params   <- fit_params()
        req(pdata, fit, plate, params)
        
        response_var <- loaded_data$response_var
        indep_var <- loaded_data$indep_var
        
        plot_data <- get_plot_data(models_fit_list = fit,
                                   prepped_data = pdata$data,
                                   fit_params = params,
                                   fixed_a_result = plate$fixed_a_result,
                                   model_names = model_names,
                                   x_var = indep_var,
                                   y_var = response_var)
      })
      #
      output$model_comparisions <- renderPlot({
        pd <- plot_data()
        req(pd)
        response_var <- loaded_data$response_var
        indep_var <- loaded_data$indep_var
        
        plot_model_comparisons(plot_data = pd,
                               model_names = model_names,
                               x_var = indep_var,
                               y_var = response_var,
                               is_display_log_response = input$display_log_response,
                               is_display_log_independent = input$display_log_independent,
                               use_patchwork = TRUE)
      })
      
      output$download_model_comparisons <- downloadHandler(
        filename = function() {
          pd <- plot_data()
          paste0("model_comparison_", unique(pd$dat$study_accession), unique(pd$dat$experiment_accession),
                 unique(pd$dat$plate_nom), unique(pd$dat$antigen), ".pdf")
        },
        
        content = function(file) {
          pd <- plot_data()
          req(pd)
          
          response_var <- loaded_data$response_var
          indep_var    <- loaded_data$indep_var
          
          p <- plot_model_comparisons(
            plot_data   = pd,
            model_names = model_names,
            x_var       = indep_var,
            y_var       = response_var,
            is_display_log_response = input$display_log_response,
            is_display_log_independent = input$display_log_independent,
            use_patchwork = TRUE
          )
          
          ggsave(
            filename = file,
            plot     = p,
            device   = "pdf",
            width    = 8,
            height   = 10,
            units    = "in"
          )
        }
      )
      
      #
      #
      observeEvent(input$show_comparisions, {
        # Validate that plot_data is available before trying to access it
        pd <- tryCatch({
          plot_data()
        }, error = function(e) {
          NULL
        })
        
        # Check if plot_data is valid and has the expected structure
        if (is.null(pd) || is.null(pd$dat)) {
          showNotification("No plot data available. Please ensure standard curve data is loaded.",
                           type = "warning")
          return()
        }
        #
        # plate <- unique(plot_data()$dat$plate)
        # antigen <- unique(plot_data()$dat$antigen)
        # title <- paste("Model Comparisons for ", antigen, "on", plate)
        
        plate <- unique(pd$dat$plate)
        antigen <- unique(pd$dat$antigen)
        title <- paste("Model Comparisons for ", antigen, "on", plate)
        
        showModal(
          modalDialog(
            title = title,
            size = "l",    # "s", "m", or "l"
            plotOutput("model_comparisions"),
            downloadButton("download_model_comparisons", "Download Model Comparisons"),
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      })
      #
      best_fit <- reactive({
        params   <- fit_params()
        fit      <- fit_robust_lm()
        summary  <- fit_summary()
        pdata    <- prepped_data()
        plate    <- antigen_plate()
        pdata_plot <- plot_data()
        mc       <- model_constraints()
        req(params, fit, summary, pdata, plate, pdata_plot, mc)
        
        response_var <- loaded_data$response_var
        indep_var <- loaded_data$indep_var
        
        # Look up SE for current antigen selection
        current_se <- lookup_antigen_se(
          se_table = se_antigen_table,
          study_accession = selected_study,
          experiment_accession = selected_experiment,
          source = input$sc_source_select,
          antigen = input$sc_antigen_select
        )
        
        best_fit <- select_model_fit_AIC(fit_summary = summary,
                                         fit_robust_lm = fit,
                                         fit_params = params,
                                         plot_data = pdata_plot,
                                         verbose = verbose)
        
        # add the glance for the best fit
        best_fit <- fit_qc_glance(best_fit = best_fit,
                                  response_variable = response_var,
                                  independent_variable = indep_var,
                                  fixed_a_result = plate$fixed_a_result,
                                  antigen_settings = plate$antigen_settings,
                                  antigen_fit_options = pdata$antigen_fit_options,
                                  verbose = verbose)
        
        ## add the tidy to the best fit object
        best_fit <- tidy.nlsLM(best_fit = best_fit,
                               fixed_a_result = plate$fixed_a_result,
                               model_constraints = mc,
                               antigen_settings = plate$antigen_settings,
                               antigen_fit_options = pdata$antigen_fit_options,
                               verbose = verbose)
        
        best_fit <- predict_and_propagate_error(
          best_fit = best_fit,
          response_var = response_var,
          antigen_plate = plate,
          study_params = study_params,
          se_std_response = current_se,
          verbose = verbose
        )
        
        best_fit <- gate_samples(best_fit = best_fit,
                                 response_variable = response_var,
                                 pcov_threshold = plate$antigen_settings$pcov_threshold,
                                 verbose = verbose)
        return(best_fit)
        
      })
      
      
      output$is_display_log_response <- renderUI({
        input_switch("display_log_response", "Display as Log Response", value = T)
      })
      output$is_display_log_independent_variable <- renderUI({
        input_switch("display_log_independent", "Display independent variable as logged", value = T)
      })
      
      output$standard_curve <- renderPlotly({
        bf    <- best_fit()
        plate <- antigen_plate()
        req(bf, plate)
        
        response_var <- loaded_data$response_var
        indep_var <- loaded_data$indep_var
        
        plot_standard_curve(best_fit = bf,
                            is_display_log_response = input$display_log_response,
                            is_display_log_independent = input$display_log_independent,
                            pcov_threshold = plate$antigen_settings$pcov_threshold,
                            independent_variable = indep_var,
                            response_variable = response_var)
      })
      
      output$download_best_fit_parameter_estimates <- downloadHandler(
        filename = function() {
          paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, "x",
                input$sc_antigen_select, "tidy_parameter_estimates.csv", sep = "_")
        },
        content = function(file) {
          bf <- best_fit()
          req(bf)
          write.csv(bf$best_tidy, file, row.names = FALSE)
        }
      )
      
      output$summary_statistics <- renderTable({
        bf <- best_fit()
        req(bf)
        bf$best_glance
      }, caption = "Summary Statistics",
      caption.placement = getOption("xtable.caption.placement", "top"))
      
      output$download_samples_above_ulod <- downloadHandler(
        filename = function() {
          paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, "x",
                input$sc_antigen_select, "samples_above_ulod.csv", sep = "_")
        },
        content = function(file) {
          bf <- best_fit()
          req(bf)
          se <- bf$sample_se
          write.csv(se[se$gate_class_lod == "Too Concentrated",
                       !(names(se) %in% c("plate_id", "assay_response_variable",
                                          "assay_independent_variable", "y_new", "overall_se"))],
                    file, row.names = FALSE)
        }
      )
      
      output$download_samples_below_llod <- downloadHandler(
        filename = function() {
          paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, "x",
                input$sc_antigen_select, "samples_below_llod.csv", sep = "_")
        },
        content = function(file) {
          bf <- best_fit()
          req(bf)
          se <- bf$sample_se
          write.csv(se[se$gate_class_lod == "Too Diluted",
                       !(names(se) %in% c("plate_id", "assay_response_variable",
                                          "assay_independent_variable", "y_new", "overall_se"))],
                    file, row.names = FALSE)
        }
      )
      
      # -----------------------------------------------------------------
      # Reactive fetch of the job‑status table whenever the plate changes
      # -----------------------------------------------------------------
      concentration_calc_df <- reactive({
        req(input$sc_plate_select,
            input$readxMap_study_accession != "Click here",
            input$readxMap_experiment_accession != "Click here")
        
        get_existing_concentration_calc(
          conn                = conn,
          project_id          = userWorkSpaceID(),
          study_accession     = input$readxMap_study_accession,
          experiment_accession = input$readxMap_experiment_accession,
          plate_nom           = input$sc_plate_select
        )
      })
      
      output$concentrationMethodUI <- renderUI({
        df <- concentration_calc_df()               # ← the fresh data‑frame
        if (nrow(df) == 0) {
          HTML("<p>No concentration methods have been calculated for this plate.</p>")
        } else {
          createStandardCurveConcentrationTypeUI(df)   # ← pass it in
        }
      })
      
      # # ── React to Study selection ──
      # observeEvent(input$concentrationMethodStudy, {
      #   selected_method <- input$concentrationMethodStudy
      #   cat("Study method selected:", selected_method, "\n")
      #   # ... fetch study-level concentration results for this method
      # })
      # 
      # # ── React to Experiment selection ──
      # observeEvent(input$concentrationMethodExperiment, {
      #   selected_method <- input$concentrationMethodExperiment
      #   cat("Experiment method selected:", selected_method, "\n")
      #   # ... fetch experiment-level concentration results
      # })
      # 
      # # ── React to Plate selection ──
      # observeEvent(input$concentrationMethodPlate, {
      #   selected_method <- input$concentrationMethodPlate
      #   cat("Plate method selected:", selected_method, "\n")
      #   # ... fetch plate-level concentration results
      # })
      # concentration_calc_df <- reactive({
      #   req(input$sc_plate_select,
      #       input$readxMap_study_accession != "Click here",
      #       input$readxMap_experiment_accession != "Click here")
      #   
      #   get_existing_concentration_calc(
      #     conn                = conn,
      #     project_id          = userWorkSpaceID(),
      #     study_accession     = input$readxMap_study_accession,
      #     experiment_accession= input$readxMap_experiment_accession,
      #     plate_nom           = input$sc_plate_select)
      # })
      # 
      # output$concentrationMethodUI <- renderUI({
      #   df <- concentration_calc_df()
      #   if (nrow(df) == 0) {
      #     HTML("<p>No concentration methods have been calculated for this plate.</p>")
      #   } else {
      #     createStandardCurveConcentrationTypeUI(df)   # <- df is passed in
      #   }
      # })
      # observeEvent(input$sc_plate_select, {
      #   req(
      #     input$sc_plate_select,
      #     input$readxMap_study_accession != "Click here",
      #     input$readxMap_experiment_accession != "Click here"
      #   )
      #   
      #   existing_concentration_calc <- get_existing_concentration_calc(
      #     conn                = conn,
      #     project_id          = userWorkSpaceID(),
      #     study_accession     = input$readxMap_study_accession,
      #     experiment_accession = input$readxMap_experiment_accession,
      #     plate_nom           = input$sc_plate_select
      #   )
      #   existing_concentration_calc_v <<- existing_concentration_calc
      #   
      #   output$concentrationMethodUI <- renderUI({
      #     req(existing_concentration_calc)          # make sure the query finished
      #     if (nrow(existing_concentration_calc) == 0) {
      #       HTML("<p>No concentration methods have been calculated for this plate.</p>")
      #     } else {
      #       createStandardCurveConcentrationTypeUI(existing_concentration_calc)
      #     }
      #   })
      # })
      
      # observeEvent(input$sc_plate_select, {
      #   
      #   existing_concentration_calc <- get_existing_concentration_calc(conn = conn, project_id = userWorkSpaceID(),
      #                                                                  study_accession = selected_study,
      #                                                                  experiment_accession = selected_experiment,
      #                                                                  plate_nom = input$sc_plate_select)
      #   existing_concentration_calc_v <<- existing_concentration_calc
      #   
      #   output$concentrationMethodUI <- createStandardCurveConcentrationTypeUI(existing_concentration_calc)
      #   
      # })
      
      
    } # end if
    
    
  }) # end navigation observer


# observeEvent(input$sc_plate_select, {
# 
#   existing_concentration_calc <- get_existing_concentration_calc(conn = conn, project_id = userWorkSpaceID(),
#                                   study_accession = input$readxMap_study_accession,
#                                   experiment_accession = input$readxMap_experiment_accession,
#                                   plate_nom = input$sc_plate_select)
#   existing_concentration_calc_v <<- existing_concentration_calc
# 
#   output$concentrationMethodUI <- createStandardCurveConcentrationTypeUI(existing_concentration_calc)
#   
# })
# ============================================================================
# Batch Processing Button Observer - completely separate from navigation
# ============================================================================



# is_batch_processing <- reactiveVal(FALSE)
# 
# observeEvent(input$save_scope, {
# 
#   label <- switch(
#     input$save_scope,
#     plate      = "Calculate Standard Curves\n with interpolated concentrations\n(Current Plate)",
#     experiment = "Calculate Standard Curves \n with interpolated concentrations\n(Current Experiment)",
#     study      = "Calculate Standard Curves \n with interpolated concentrations\n (All Experiments)"
#   )
# 
#   updateActionButton(
#     session,
#     "run_batch_fit",
#     label = label
#   )
# })
# 
# 
# observeEvent(input$run_batch_fit, ignoreInit = TRUE, {
# 
#   scope <- input$save_scope
# 
#   # First check: Is processing already running?
#   if (is_batch_processing()) {
#     showNotification("Batch processing is already running. Please wait for it to complete.",
#                     type = "warning", duration = 10, closeButton = TRUE)
#     return()
#   }
# 
#   # Second check: Ensure we're on the right tab and have the required data
#   req(input$qc_component == "Standard Curve",
#       input$readxMap_study_accession != "Click here",
#       input$readxMap_experiment_accession != "Click here",
#       input$study_level_tabs == "Experiments",
#       input$main_tabs == "view_files_tab")
# 
#   # Third check: Validate that study accession is available
#   if (is.null(input$readxMap_study_accession) || input$readxMap_study_accession == "") {
#     showNotification("Please select a study before running batch processing.",
#                     type = "error", duration = 10, closeButton = TRUE)
#     return()
#   }
# 
#   # Set flag to indicate processing has started
#   is_batch_processing(TRUE)
# 
#   # Use tryCatch to ensure flag is reset even if there's an error
#   tryCatch({
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Fitting standard curves for all experiments."),
#                     duration = NULL, closeButton = TRUE)
# 
#     # Pull fresh data for the batch processing
#     headers <- fetch_db_header_experiments(study_accession = input$readxMap_study_accession, conn = conn)
#     #exp_list <- unique(headers$experiment_accession)
# 
#     exp_list <- switch(
#       scope,
#       "study"      = unique(headers$experiment_accession),
#       "experiment" = input$readxMap_experiment_accession,
#       "plate"      = input$readxMap_experiment_accession
#     )
# 
#     loaded_data_list <- list()
#     for(exp in exp_list) {
#       loaded_data_list[[exp]] <- pull_data(study_accession = input$readxMap_study_accession,
#                                            experiment_accession = exp,
#                                            project_id = userWorkSpaceID(),
#                                            conn = conn)
#     }
# 
#     if (scope == "plate") {
#       plate <- input$sc_plate_select
#       loaded_data_list <- lapply(loaded_data_list, function(x) {
# 
#         if (!is.null(x$plates) && nrow(x$plates) > 0) {
#           x$plates <- x$plates[x$plates$plate_nom == plate,, drop = F]
#         }
#         if (!is.null(x$standards) && nrow(x$standards) > 0) {
#           x$standards <- x$standards[x$standards$plate_nom == plate,, drop = F]
#         }
# 
#         if (!is.null(x$samples) && nrow(x$samples) > 0) {
#           x$samples <- x$samples[x$samples$plate_nom == plate,, drop = F]
#         }
# 
#         if (!is.null(x$blanks) && nrow(x$blanks) > 0) {
#           x$blanks <- x$blanks[x$blanks$plate_nom == plate,, drop = F]
#         }
# 
#         x
#       })
# 
#     }
# 
# 
#     # Get response variable from first experiment's loaded data
#     response_var <- loaded_data_list[[exp_list[1]]]$response_var
# 
#     # Compute SE table for all antigens across all experiments
#     all_standards <- do.call(rbind, lapply(loaded_data_list, function(x) x$standards))
# 
#     se_antigen_table <- compute_antigen_se_table(
#       standards_data = all_standards,
#       response_col = response_var,
#       dilution_col = "dilution",
#       plate_col = "plate",
#       grouping_cols = c("study_accession", "experiment_accession", "source", "antigen"),
#       method = "pooled_within",
#       verbose = TRUE
#     )
# 
#     # Get study parameters
#     verbose = FALSE
#     model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
#     param_group = "standard_curve_options"
# 
#     study_params <- fetch_study_parameters(study_accession = input$readxMap_study_accession,
#                                            param_user = currentuser(),
#                                            param_group = param_group,
#                                            project_id = userWorkSpaceID(),
#                                            conn = conn)
# 
#     antigen_list_res <- build_antigen_list(exp_list = exp_list,
#                                            loaded_data_list = loaded_data_list,
#                                            study_accession = input$readxMap_study_accession)
# 
#     cat(paste("after antigen_list_res", exp))
#     antigen_plate_list_res <- build_antigen_plate_list(antigen_list_result = antigen_list_res,
#                                                        loaded_data_list = loaded_data_list)
# 
#     prepped_data_list_res <- prep_plate_data_batch(antigen_plate_list_res = antigen_plate_list_res,
#                                                    study_params = study_params,
#                                                    verbose = verbose)
# 
#     # the list of antigen and plates is truncated to exclude standard data with < 6 points
#     antigen_plate_list_res$antigen_plate_list_ids <- prepped_data_list_res$antigen_plate_name_list
# 
#     cat(paste("after prepped_data_list_res", exp))
#     batch_fit_res <- fit_experiment_plate_batch(prepped_data_list_res = prepped_data_list_res,
#                                                 antigen_plate_list_res = antigen_plate_list_res,
#                                                 model_names = model_names,
#                                                 study_params = study_params,
#                                                 se_antigen_table = se_antigen_table,
#                                                 verbose = verbose)
# 
#     batch_outputs <- create_batch_fit_outputs(batch_fit_res = batch_fit_res, antigen_plate_list_res)
# 
#     # add unique identifiers and rename the response variable to be generic for saving
#     batch_outputs_processed <- process_batch_outputs(batch_outputs = batch_outputs,
#                                                      response_var = response_var,
#                                                      project_id = userWorkSpaceID())
#     #batch_outputs_processed_v <<- batch_outputs_processed
# 
# 
# 
# 
#     upsert_best_curve(
#       conn   = conn,
#       df     = batch_outputs_processed$best_glance_all,
#       schema = "madi_results",
#       table  = "best_glance_all",
#       notify = shiny_notify(session)
#     )
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Best Fit Statistics saved"),
#                     duration = NULL, closeButton = TRUE)
# 
#     # Retrieve lookup of IDs using NK
#     study_to_save <- unique(batch_outputs_processed$best_glance_all$study_accession)
# 
#     glance_lookup <- DBI::dbGetQuery(
#       conn,
#       glue::glue("
#     SELECT
#       best_glance_all_id,
#       study_accession,
#       experiment_accession,
#       plateid,
#       plate,
#       nominal_sample_dilution,
#       source,
#       antigen
#     FROM madi_results.best_glance_all
#     WHERE study_accession = '{study_to_save}';
#   ")
#     )
# 
#     glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)
# 
# 
#     # natural key (nk) vector for the glance
#     keys <- c("study_accession", "experiment_accession", "plateid",
#               "plate", "nominal_sample_dilution", "source", "antigen")
# 
# 
#     # attach the best_glance_all_id to the table
#     batch_outputs_processed$best_pred_all <-
#       dplyr::inner_join(
#         batch_outputs_processed$best_pred_all,
#         glance_lookup,
#         by = keys
#       )
# 
#     batch_outputs_processed$best_sample_se_all <-
#       dplyr::inner_join(
#         batch_outputs_processed$best_sample_se_all,
#         glance_lookup,
#         by = keys
#       )
# 
#     batch_outputs_processed$best_standard_all <-
#       dplyr::inner_join(
#         batch_outputs_processed$best_standard_all,
#         glance_lookup,
#         by = keys
#       )
# 
# 
#     upsert_best_curve(
#       conn   = conn,
#       df     = batch_outputs_processed$best_plate_all,
#       schema = "madi_results",
#       table  = "best_plate_all",
#       notify = shiny_notify(session)
#     )
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Best Plates saved"),
#                     duration = NULL, closeButton = TRUE)
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Saving parameter estimates..."),
#                     duration = NULL, closeButton = TRUE)
# 
#     upsert_best_curve(
#       conn   = conn,
#       df     = batch_outputs_processed$best_tidy_all,
#       schema = "madi_results",
#       table  = "best_tidy_all",
#       notify = shiny_notify(session)
#     )
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Best parameter estimates saved"),
#                     duration = NULL, closeButton = TRUE)
# 
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Saving predicted standards..."),
#                     duration = NULL, closeButton = TRUE)
# 
#     upsert_best_curve(
#       conn   = conn,
#       df     = batch_outputs_processed$best_pred_all,
#       schema = "madi_results",
#       table  = "best_pred_all",
#       notify = shiny_notify(session)
#     )
# 
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Best Predicted standards saved"),
#                     duration = NULL, closeButton = TRUE)
# 
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Saving Predicted samples..."),
#                     duration = NULL, closeButton = TRUE)
# 
#     upsert_best_curve(
#       conn   = conn,
#       df     = batch_outputs_processed$best_sample_se_all,
#       schema = "madi_results",
#       table  = "best_sample_se_all",
#       notify = shiny_notify(session)
#     )
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Best Predicted Samples saved"),
#                     duration = NULL, closeButton = TRUE)
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Saving Best Standards..."),
#                     duration = NULL, closeButton = TRUE)
# 
#     upsert_best_curve(
#       conn   = conn,
#       df     = batch_outputs_processed$best_standard_all,
#       schema = "madi_results",
#       table  = "best_standard_all",
#       notify = shiny_notify(session)
#     )
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Best Standards saved"),
#                     duration = NULL, closeButton = TRUE)
# 
# 
# 
#     showNotification(id = "batch_sc_fit_notify",
#                     div(class = "big-notification", "Standard Curves Calculated for all Experiments"),
#                     duration = NULL, closeButton = TRUE)
#     removeNotification("batch_sc_fit_notify")
# 
#   }, error = function(e) {
#     # Handle errors
#     showNotification(
#       paste("Error during batch processing:", e$message),
#       type = "error",
#       duration = NULL,
#       closeButton = TRUE
#     )
#   removeNotification("batch_sc_fit_notify")
#   }, finally = {
#     # Always reset the processing flag, even if there was an error
#     is_batch_processing(FALSE)
#   })
# 
# })
# =================================================================
# CONCENTRATION CALCULATION - SERVER
# =================================================================

is_batch_processing <- reactiveVal(FALSE)

# -----------------------------------------------------------------
# A. Fetch concentration calc data reactively
# -----------------------------------------------------------------
concentration_calc_df <- reactive({
  req(input$sc_plate_select,
      input$readxMap_study_accession != "Click here",
      input$readxMap_experiment_accession != "Click here")
  
  get_existing_concentration_calc(
    conn                 = conn,
    project_id           = userWorkSpaceID(),
    study_accession      = input$readxMap_study_accession,
    experiment_accession = input$readxMap_experiment_accession,
    plate_nom            = input$sc_plate_select
  )
})

output$concentrationMethodUI <- renderUI({
  df <- concentration_calc_df()
  if (nrow(df) == 0) {
    HTML("<p>No concentration methods have been calculated for this plate.</p>")
  } else {
    createStandardCurveConcentrationTypeUI(df)
  }
})

# -----------------------------------------------------------------
# B. Render conditional buttons based on scope + statuses
# -----------------------------------------------------------------
output$concentration_buttons_ui <- renderUI({
  req(input$save_scope)
  df <- concentration_calc_df()
  scope <- input$save_scope
  
  ## Get status for interpolated at the selected scope
  interp_status <- get_status(df, scope, "interpolated")
  mcmc_status   <- get_status(df, scope, "mcmc_robust")
  
  scope_label <- switch(scope,
                        "study"      = "(All Experiments)",
                        "experiment" = "(Current Experiment)",
                        "plate"      = "(Current Plate)"
  )
  
  buttons <- tagList()
  
  ## ── Always show the "Calculate Standard Curves" button ──
  calc_sc_label <- HTML(paste0(
    "Calculate Standard Curves<br>",
    "with <strong>Interpolated</strong> concentrations<br>",
    scope_label
  ))
  
  ## Disable if interpolated is already running
  if (interp_status == "pending") {
    buttons <- tagList(
      buttons,
      tags$button(
        class = "conc-btn conc-btn-disabled",
        disabled = "disabled",
        tags$i(class = "fa fa-spinner fa-spin", style = "margin-right:5px;"),
        HTML(paste0(
          "Calculating Interpolated...<br>", scope_label
        ))
      )
    )
  } else {
    buttons <- tagList(
      buttons,
      actionButton(
        inputId = "run_batch_fit",
        label   = calc_sc_label
        # icon    = icon("play"),
        #class   = "conc-btn conc-btn-green"
      )
    )
  }
  
  ## ── Show MCMC button ONLY when interpolated is "completed" at this scope ──
  if (interp_status == "completed") {
    if (mcmc_status == "pending") {
      buttons <- tagList(
        buttons,
        tags$button(
          class = "conc-btn conc-btn-disabled",
          disabled = "disabled",
          tags$i(class = "fa fa-spinner fa-spin", style = "margin-right:5px;"),
          HTML(paste0(
            "Calculating MCMC Robust...<br>", scope_label
          ))
        )
      )
    } else {
      mcmc_label <- HTML(paste0(
        "Calculate <strong>MCMC Robust</strong> concentrations<br>",
        scope_label
      ))
      
      buttons <- tagList(
        buttons,
        actionButton(
          inputId = "run_mcmc_calc",
          label   = mcmc_label
          #icon    = icon("chart-line"),
          #class   = "conc-btn conc-btn-blue"
        )
      )
    }
  }
  
  buttons
  ## Wrap buttons in centered div
  # div(
  #   style = "text-align:center; margin-top:15px;",
  #   buttons
  # )
})

# -----------------------------------------------------------------
# C. Interpolated calculation handler
# -----------------------------------------------------------------
observeEvent(input$run_batch_fit, ignoreInit = TRUE, {
  
  scope <- input$save_scope
  
  if (is_batch_processing()) {
    showNotification("Batch processing is already running. Please wait.",
                     type = "warning", duration = 10, closeButton = TRUE)
    return()
  }
  
  req(input$qc_component == "Standard Curve",
      input$readxMap_study_accession != "Click here",
      input$readxMap_experiment_accession != "Click here",
      input$study_level_tabs == "Experiments",
      input$main_tabs == "view_files_tab")
  
  if (is.null(input$readxMap_study_accession) ||
      input$readxMap_study_accession == "") {
    showNotification("Please select a study before running batch processing.",
                     type = "error", duration = 10, closeButton = TRUE)
    return()
  }
  
  is_batch_processing(TRUE)
  
  tryCatch({
    
    scope_label <- switch(scope,
                          "study"      = "all experiments",
                          "experiment" = "current experiment",
                          "plate"      = "current plate"
    )
    
    showNotification(
      id = "batch_sc_fit_notify",
      div(class = "big-notification",
          paste0("Running interpolated for ", scope_label, "...")),
      duration = NULL, closeButton = TRUE
    )
    
    ## ── Build experiment list based on scope ──
    headers <- fetch_db_header_experiments(
      study_accession = input$readxMap_study_accession, conn = conn
    )
    
    exp_list <- switch(scope,
                       "study"      = unique(headers$experiment_accession),
                       "experiment" = input$readxMap_experiment_accession,
                       "plate"      = input$readxMap_experiment_accession
    )
    
    ## ── Pull data ──
    loaded_data_list <- list()
    for (exp in exp_list) {
      loaded_data_list[[exp]] <- pull_data(
        study_accession      = input$readxMap_study_accession,
        experiment_accession = exp,
        project_id           = userWorkSpaceID(),
        conn                 = conn
      )
    }
    
    ## ── Filter to plate if scope = "plate" ──
    if (scope == "plate") {
      plate <- input$sc_plate_select
      loaded_data_list <- lapply(loaded_data_list, function(x) {
        for (tbl in c("plates", "standards", "samples", "blanks")) {
          if (!is.null(x[[tbl]]) && nrow(x[[tbl]]) > 0) {
            x[[tbl]] <- x[[tbl]][x[[tbl]]$plate_nom == plate, , drop = FALSE]
          }
        }
        x
      })
    }
    
    ## ── Interpolated calculation ──
    response_var <- loaded_data_list[[exp_list[1]]]$response_var
    
    all_standards <- do.call(rbind, lapply(loaded_data_list, function(x) x$standards))
    se_antigen_table <- compute_antigen_se_table(
      standards_data = all_standards,
      response_col   = response_var,
      dilution_col   = "dilution",
      plate_col      = "plate",
      grouping_cols  = c("study_accession", "experiment_accession", "source", "antigen"),
      method         = "pooled_within",
      verbose        = TRUE
    )
    
    verbose     <- FALSE
    model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
    param_group <- "standard_curve_options"
    
    study_params <- fetch_study_parameters(
      study_accession = input$readxMap_study_accession,
      param_user      = currentuser(),
      param_group     = param_group,
      project_id      = userWorkSpaceID(),
      conn            = conn
    )
    
    antigen_list_res <- build_antigen_list(
      exp_list         = exp_list,
      loaded_data_list = loaded_data_list,
      study_accession  = input$readxMap_study_accession
    )
    
    antigen_plate_list_res <- build_antigen_plate_list(
      antigen_list_result = antigen_list_res,
      loaded_data_list    = loaded_data_list
    )
    
    prepped_data_list_res <- prep_plate_data_batch(
      antigen_plate_list_res = antigen_plate_list_res,
      study_params           = study_params,
      verbose                = verbose
    )
    
    antigen_plate_list_res$antigen_plate_list_ids <-
      prepped_data_list_res$antigen_plate_name_list
    
    batch_fit_res <- fit_experiment_plate_batch(
      prepped_data_list_res  = prepped_data_list_res,
      antigen_plate_list_res = antigen_plate_list_res,
      model_names            = model_names,
      study_params           = study_params,
      se_antigen_table       = se_antigen_table,
      verbose                = verbose
    )
    
    batch_outputs <- create_batch_fit_outputs(
      batch_fit_res          = batch_fit_res,
      antigen_plate_list_res = antigen_plate_list_res
    )
    
    batch_outputs_processed <- process_batch_outputs(
      batch_outputs = batch_outputs,
      response_var  = response_var,
      project_id    = userWorkSpaceID()
    )
    
    ## ── Save all results ──
    upsert_best_curve(conn = conn, df = batch_outputs_processed$best_glance_all,
                      schema = "madi_results", table = "best_glance_all",
                      notify = shiny_notify(session))
    showNotification(id = "batch_sc_fit_notify",
                     div(class = "big-notification", "Best Fit Statistics saved"),
                     duration = NULL, closeButton = TRUE)
    
    study_to_save <- unique(batch_outputs_processed$best_glance_all$study_accession)
    glance_lookup <- DBI::dbGetQuery(conn, glue::glue("
      SELECT best_glance_all_id, study_accession, experiment_accession,
             plateid, plate, nominal_sample_dilution, source, antigen
      FROM madi_results.best_glance_all
      WHERE study_accession = '{study_to_save}';
    "))
    glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)
    
    keys <- c("study_accession", "experiment_accession", "plateid",
              "plate", "nominal_sample_dilution", "source", "antigen")
    
    batch_outputs_processed$best_pred_all <-
      dplyr::inner_join(batch_outputs_processed$best_pred_all, glance_lookup, by = keys)
    batch_outputs_processed$best_sample_se_all <-
      dplyr::inner_join(batch_outputs_processed$best_sample_se_all, glance_lookup, by = keys)
    batch_outputs_processed$best_standard_all <-
      dplyr::inner_join(batch_outputs_processed$best_standard_all, glance_lookup, by = keys)
    
    upsert_best_curve(conn = conn, df = batch_outputs_processed$best_plate_all,
                      schema = "madi_results", table = "best_plate_all",
                      notify = shiny_notify(session))
    
    upsert_best_curve(conn = conn, df = batch_outputs_processed$best_tidy_all,
                      schema = "madi_results", table = "best_tidy_all",
                      notify = shiny_notify(session))
    
    upsert_best_curve(conn = conn, df = batch_outputs_processed$best_pred_all,
                      schema = "madi_results", table = "best_pred_all",
                      notify = shiny_notify(session))
    
    upsert_best_curve(conn = conn, df = batch_outputs_processed$best_sample_se_all,
                      schema = "madi_results", table = "best_sample_se_all",
                      notify = shiny_notify(session))
    
    upsert_best_curve(conn = conn, df = batch_outputs_processed$best_standard_all,
                      schema = "madi_results", table = "best_standard_all",
                      notify = shiny_notify(session))
    
    showNotification(
      id = "batch_sc_fit_notify",
      div(class = "big-notification",
          paste0("Completed: interpolated (", scope_label, ")")),
      duration = 5, closeButton = TRUE
    )
    removeNotification("batch_sc_fit_notify")
    
  }, error = function(e) {
    showNotification(paste("Error during batch processing:", e$message),
                     type = "error", duration = NULL, closeButton = TRUE)
    removeNotification("batch_sc_fit_notify")
  }, finally = {
    is_batch_processing(FALSE)
  })
})

# -----------------------------------------------------------------
# D. MCMC Robust calculation handler
# -----------------------------------------------------------------
observeEvent(input$run_mcmc_calc, ignoreInit = TRUE, {
  scope <- input$save_scope
  if (is_batch_processing()) {
    showNotification("Batch processing is already running. Please wait.",
                     type = "warning", duration = 10, closeButton = TRUE)
    return()
  }
  req(input$qc_component == "Standard Curve",
      input$readxMap_study_accession != "Click here",
      input$readxMap_experiment_accession != "Click here")
  df <- concentration_calc_df()
  interp_status <- get_status(df, scope, "interpolated")
  if (interp_status != "completed") {
    showNotification(
      "Interpolated concentrations must be completed before running MCMC Robust.",
      type = "error", duration = 10, closeButton = TRUE
    )
    return()
  }
  is_batch_processing(TRUE)
  tryCatch({
    scope_label <- switch(scope,
                          "study"      = "all experiments",
                          "experiment" = "current experiment",
                          "plate"      = "current plate"
    )
    showNotification(
      id = "mcmc_calc_notify",
      div(class = "big-notification",
          paste0("Running MCMC Robust for ", scope_label, "...")),
      duration = NULL, closeButton = TRUE
    )
    
    study      <- input$readxMap_study_accession
    experiment <- input$readxMap_experiment_accession
    plate      <- input$sc_plate_select
    proj       <- userWorkSpaceID()
    
    ## ── 1. Fetch and filter best_glance by scope ──
    best_glance <- fetch_best_glance_mcmc(
      study_accession = study,
      project_id      = proj,
      conn            = conn
    )
    
    best_glance <- switch(scope,
                          "plate"      = best_glance[best_glance$experiment_accession == experiment &
                                                       best_glance$plate_nom == plate, ],
                          "experiment" = best_glance[best_glance$experiment_accession == experiment, ],
                          "study"      = best_glance
    )
    
    if (nrow(best_glance) == 0) {
      showNotification("No fitted curves found for this scope.",
                       type = "warning", duration = 10, closeButton = TRUE)
      is_batch_processing(FALSE)
      return()
    }
    
    ## ── 2. Fetch pred and sample for all matching IDs ──
    id_set      <- best_glance$best_glance_all_id
    best_pred   <- fetch_best_pred_mcmc(study, proj, id_set, conn)
    best_sample <- fetch_best_sample_se_mcmc(study, proj, id_set, conn)
    
    ## ── 3. Loop over each curve (plate × antigen × feature) ──
    n_total           <- length(id_set)
    all_result_pred   <- vector("list", n_total)
    all_result_sample <- vector("list", n_total)
    
    for (i in seq_along(id_set)) {
      my_id <- id_set[i]
      
      glance_row <- best_glance[best_glance$best_glance_all_id == my_id, ]
      pred_df    <- best_pred[best_pred$best_glance_all_id == my_id, ]
      sample_df  <- best_sample[best_sample$best_glance_all_id == my_id, ]
      
      if (nrow(pred_df) == 0 || nrow(sample_df) == 0) {
        warning("Skipping ID ", my_id, ": no pred or sample data")
        next
      }
      
      progress_msg <- paste0(
        "MCMC Robust: ", i, " / ", n_total, "\n",
        "Study: ", glance_row$study_accession, "\n",
        "Experiment: ", glance_row$experiment_accession, "\n",
        "Plate: ", glance_row$plate_nom, "\n",
        "Antigen: ", glance_row$antigen, "\n",
        "Model: ", glance_row$model_name
      )
      
      showNotification(
        id = "mcmc_calc_notify",
        div(class = "big-notification", style = "white-space: pre-line;", progress_msg),
        duration = NULL, closeButton = TRUE
      )
      
      # SE on prediction grid
      all_result_pred[[i]] <- run_jags_predicted_concentration(
        glance_row   = glance_row,
        best_pred_df = pred_df,
        sample_df    = pred_df,
        response_col = "yhat",
        verbose      = FALSE
      )
      
      # SE on actual samples
      all_result_sample[[i]] <- run_jags_predicted_concentration(
        glance_row   = glance_row,
        best_pred_df = pred_df,
        sample_df    = sample_df,
        response_col = "assay_response",
        verbose      = FALSE
      )
    }
    
    ## ── 4. Combine results ──
    result_pred_all   <<- do.call(rbind, Filter(Negate(is.null), all_result_pred))
    result_sample_all <<- do.call(rbind, Filter(Negate(is.null), all_result_sample))
    
    ## ── 5. Save / update DB ──
    # save_mcmc_pred_results(result_pred_all, study, proj, conn)
    # save_mcmc_sample_results(result_sample_all, study, proj, conn)
    
    showNotification(
      id = "mcmc_calc_notify",
      div(class = "big-notification",
          paste0("MCMC Robust complete: ", n_total, " curves processed")),
      duration = 10, closeButton = TRUE
    )
    
  }, error = function(e) {
    showNotification(
      paste("MCMC Robust error:", e$message),
      type = "error", duration = 15, closeButton = TRUE
    )
  }, finally = {
    is_batch_processing(FALSE)
  })
})
# observeEvent(input$run_mcmc_calc, ignoreInit = TRUE, {
#   
#   scope <- input$save_scope
#   
#   if (is_batch_processing()) {
#     showNotification("Batch processing is already running. Please wait.",
#                      type = "warning", duration = 10, closeButton = TRUE)
#     return()
#   }
#   
#   req(input$qc_component == "Standard Curve",
#       input$readxMap_study_accession != "Click here",
#       input$readxMap_experiment_accession != "Click here")
#   
#   df <- concentration_calc_df()
#   interp_status <- get_status(df, scope, "interpolated")
#   
#   if (interp_status != "completed") {
#     showNotification(
#       "Interpolated concentrations must be completed before running MCMC Robust.",
#       type = "error", duration = 10, closeButton = TRUE
#     )
#     return()
#   }
#   
#   is_batch_processing(TRUE)
#   
#   tryCatch({
#     
#     scope_label <- switch(scope,
#                           "study"      = "all experiments",
#                           "experiment" = "current experiment",
#                           "plate"      = "current plate"
#     )
#     
#     showNotification(
#       id = "mcmc_calc_notify",
#       div(class = "big-notification",
#           paste0("Running MCMC Robust for ", scope_label, "...")),
#       duration = NULL, closeButton = TRUE
#     )
#     
#     study      <- input$readxMap_study_accession
#     experiment <- input$readxMap_experiment_accession
#     plate      <- input$sc_plate_select
#     proj       <- userWorkSpaceID()
#     
#     ## ── 1. Same prep pipeline as interpolated observer ──
#     headers <- fetch_db_header_experiments(
#       study_accession = study, conn = conn
#     )
#     
#     exp_list <- switch(scope,
#                        "study"      = unique(headers$experiment_accession),
#                        "experiment" = experiment,
#                        "plate"      = experiment
#     )
#     
#     best_glance <- fetch_best_glance_mcmc(study_accession = input$readxMap_study_accession, 
#                                           project_id =userWorkSpaceID() , conn = conn)
#     if (scope == "plate") {
#       plate <- input$sc_plate_select
#       best_glance <- best_glance[best_glance$experiment_accession == input$readxMap_experiment_accession &
#                                    best_glance$plate_nom == plate,]
#     } else if (scope == "experiment") {
#       best_glance <- best_glance[best_glance$experiment_accession == input$readxMap_experiment_accession,]
#     }
#     
#     best_glance <<- best_glance
#     best_glance_id_set <- best_glance$best_glance_all_id
#     best_pred   <<- fetch_best_pred_mcmc(study, proj, best_glance_id_set, conn)
#     best_sample <<- fetch_best_sample_se_mcmc(study, proj, best_glance_id_set, conn)
#     
#     my_id      <- "some_best_glance_all_id"
#     glance_row <- best_glance[best_glance$best_glance_all_id == my_id, ]
#     pred_df    <- best_pred[best_pred$best_glance_all_id == my_id, ]
#     sample_df  <- best_sample[best_sample$best_glance_all_id == my_id, ]
#     
#     # On the prediction grid
#     result_pred <- run_jags_predicted_concentration(
#       glance_row   = glance_row,
#       best_pred_df = pred_df,
#       sample_df    = pred_df,
#       response_col = "yhat"
#     )
#     
#     # On actual samples
#     result_samples <- run_jags_predicted_concentration(
#       glance_row   = glance_row,
#       best_pred_df = pred_df,
#       sample_df    = sample_df,
#       response_col = "assay_response"
#     )
#     
#     # loaded_data_list <- list()
#     # for (exp in exp_list) {
#     #   loaded_data_list[[exp]] <- pull_data(
#     #     study_accession      = study,
#     #     experiment_accession = exp,
#     #     project_id           = proj,
#     #     conn                 = conn
#     #   )
#     # }
#     # 
#     # if (scope == "plate") {
#     #   loaded_data_list <- lapply(loaded_data_list, function(x) {
#     #     for (tbl in c("plates", "standards", "samples", "blanks")) {
#     #       if (!is.null(x[[tbl]]) && nrow(x[[tbl]]) > 0) {
#     #         x[[tbl]] <- x[[tbl]][x[[tbl]]$plate_nom == plate, , drop = FALSE]
#     #       }
#     #     }
#     #     x
#     #   })
#     # }
#     # 
#     # response_var <- loaded_data_list[[exp_list[1]]]$response_var
#     # 
#     # all_standards <- do.call(rbind, lapply(loaded_data_list, function(x) x$standards))
#     # se_antigen_table <- compute_antigen_se_table(
#     #   standards_data = all_standards,
#     #   response_col   = response_var,
#     #   dilution_col   = "dilution",
#     #   plate_col      = "plate",
#     #   grouping_cols  = c("study_accession", "experiment_accession", "source", "antigen"),
#     #   method         = "pooled_within",
#     #   verbose        = FALSE
#     # )
#     # 
#     # verbose     <- FALSE
#     # model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
#     # param_group <- "standard_curve_options"
#     # 
#     # study_params <- fetch_study_parameters(
#     #   study_accession = study,
#     #   param_user      = currentuser(),
#     #   param_group     = param_group,
#     #   project_id      = proj,
#     #   conn            = conn
#     # )
#     # 
#     # antigen_list_res <- build_antigen_list(
#     #   exp_list         = exp_list,
#     #   loaded_data_list = loaded_data_list,
#     #   study_accession  = study
#     # )
#     # 
#     # antigen_plate_list_res <- build_antigen_plate_list(
#     #   antigen_list_result = antigen_list_res,
#     #   loaded_data_list    = loaded_data_list
#     # )
#     # 
#     # prepped_data_list_res <- prep_plate_data_batch(
#     #   antigen_plate_list_res = antigen_plate_list_res,
#     #   study_params           = study_params,
#     #   verbose                = verbose
#     # )
#     # 
#     # antigen_plate_list_res$antigen_plate_list_ids <-
#     #   prepped_data_list_res$antigen_plate_name_list
#     # 
#     # batch_fit_res <- fit_experiment_plate_batch(
#     #   prepped_data_list_res  = prepped_data_list_res,
#     #   antigen_plate_list_res = antigen_plate_list_res,
#     #   model_names            = model_names,
#     #   study_params           = study_params,
#     #   se_antigen_table       = se_antigen_table,
#     #   verbose                = verbose
#     # )
#     # 
#     # batch_outputs <- create_batch_fit_outputs(
#     #   batch_fit_res          = batch_fit_res,
#     #   antigen_plate_list_res = antigen_plate_list_res
#     # )
#     # 
#     # batch_outputs_processed <- process_batch_outputs(
#     #   batch_outputs = batch_outputs,
#     #   response_var  = response_var,
#     #   project_id    = userWorkSpaceID()
#     # )
#     # 
#     # ## ── 2. Now loop over antigen_plate_list to run MCMC ──
#     # mcmc_pred_list   <- list()
#     # mcmc_sample_list <- list()
#     # 
#     # antigen_plate_list <- batch_outputs_processed$antigen_plate_list$antigen_plate_list
#     # 
#     # for (fit_name in names(antigen_plate_list)) {
#     #   
#     #   plate_entry <- antigen_plate_list[[fit_name]]
#     # 
#     #   # if (!fit_name %in% names(batch_fit_res)) {
#     #   #   warning(paste("No batch_fit_res entry for:", fit_name))
#     #   #   next
#     #   # }
#     #   
#     #   fit_entry <- batch_fit_res[[fit_name]]
#     #   
#     #  # if (is.null(fit_entry$pred_se) || nrow(fit_entry$pred_se) == 0) {
#     #  #    message("Skipping ", fit_name, " - pred_se is empty")
#     #  #    next
#     #  #  }
#     #   showNotification(
#     #     id = "mcmc_calc_notify",
#     #     div(class = "big-notification", HTML(paste0(
#     #       "Running MCMC:<br>", fit_name
#     #     ))),
#     #     duration = NULL, closeButton = TRUE
#     #   )
#     #   
#     #   # Build the structure expected by run_jags_predicted_concentration_wrapper
#     #   best_fit_out <<- list(
#     #     # Raw plate data
#     #     plate_standard   = plate_entry$plate_standard,
#     #     plate_samples    = plate_entry$plate_samples,
#     #     plate_blanks     = plate_entry$plate_blanks,
#     #     antigen_settings = plate_entry$antigen_settings,
#     #     fixed_a_result   = plate_entry$fixed_a_result,
#     #     std_error_blank  = plate_entry$std_error_blank,
#     #     
#     #     # pred_se branch needs $fit (not $best_fit) [1]
#     #     fit              = fit_entry$best_fit,
#     #     best_model_name  = fit_entry$best_model_name,
#     #     pred_se          = fit_entry$best_pred,
#     #     
#     #     # sample_se branch needs a nested $best_fit list [1]
#     #     best_fit = list(
#     #       best_model_name = fit_entry$best_model_name,
#     #       best_fit        = fit_entry$best_fit,   
#     #       best_data       = fit_entry$best_data
#     #     ),
#     #     sample_se  = fit_entry$sample_se,
#     #     response_var = response_var
#     #   )
#     #  
#     #   
#     #   bayes_pred <- tryCatch(
#     #     run_jags_predicted_concentration_wrapper(best_fit_out, input_df = "pred_se"),
#     #     error = function(e) {
#     #       warning(paste("MCMC pred failed for", fit_name, ":", e$message))
#     #       NULL
#     #     }
#     #   )
#     #   
#     #   bayes_sample <- tryCatch(
#     #     run_jags_predicted_concentration_wrapper(best_fit_out, input_df = "sample_se"),
#     #     error = function(e) {
#     #       warning(paste("MCMC sample failed for", fit_name, ":", e$message))
#     #       NULL
#     #     }
#     #   )
#     #   
#     #   if (!is.null(bayes_pred))   mcmc_pred_list[[fit_name]]   <- bayes_pred
#     #   if (!is.null(bayes_sample)) mcmc_sample_list[[fit_name]] <- bayes_sample
#     # }
#     # 
#     # if (length(mcmc_pred_list) == 0) {
#     #   warning("MCMC produced no results - check input data.")
#     # }
#     # 
#     # ## ── 3. Bind results ──
#     # mcmc_pred_all   <<- dplyr::bind_rows(mcmc_pred_list)
#     # mcmc_sample_all <<- dplyr::bind_rows(mcmc_sample_list)
# 
#     # ## ── 4. Process pred output ──
#     
# 
#     # ## ── 5. Process sample output ──
#     # mcmc_sample_processed <- mcmc_sample_all %>%
#     #   dplyr::rename(
#     #     raw_robust_concentration   = predicted_concentration_median,
#     #     final_robust_concentration = predicted_concentration_median,
#     #     se_robust_concentration    = predicted_concentration_se,
#     #     pcov_robust_concentration  = cv_concentration
#     #   ) %>%
#     #   dplyr::rename(assay_response = all_of(
#     #     unique(mcmc_sample_all$assay_response_variable)
#     #   )) %>%
#     #   dplyr::select(
#     #     study_accession,
#     #     experiment_accession,
#     #     patientid,
#     #     timeperiod,
#     #     sampleid,
#     #     well,
#     #     stype,
#     #     agroup,
#     #     dilution,
#     #     antigen,
#     #     source,
#     #     plateid,
#     #     plate,
#     #     nominal_sample_dilution,
#     #     assay_response,
#     #     assay_response_variable,
#     #     assay_independent_variable,
#     #     raw_predicted_concentration,
#     #     final_predicted_concentration,
#     #     pcov,
#     #     gate_class_loq,
#     #     gate_class_lod,
#     #     gate_class_pcov,
#     #     feature,
#     #     raw_robust_concentration,
#     #     final_robust_concentration,
#     #     se_robust_concentration,
#     #     pcov_robust_concentration
#     #   ) %>%
#     #   dplyr::mutate(project_id = as.numeric(proj)) %>%
#     #   dplyr::distinct()
#     # 
#     # ## ── 6. Join best_glance_all_id ──
#     # glance_lookup <- DBI::dbGetQuery(conn, glue::glue("
#     #   SELECT best_glance_all_id, study_accession, experiment_accession,
#     #          plateid, plate, nominal_sample_dilution, source, antigen
#     #   FROM madi_results.best_glance_all
#     #   WHERE study_accession = '{study}';
#     # "))
#     # glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)
#     # 
#     # keys <- c("study_accession", "experiment_accession", "plateid",
#     #           "plate", "nominal_sample_dilution", "source", "antigen")
#     # 
#     # mcmc_pred_processed    <- dplyr::inner_join(mcmc_pred_processed,    glance_lookup, by = keys)
#     # mcmc_sample_processed  <- dplyr::inner_join(mcmc_sample_processed,  glance_lookup, by = keys)
#     # 
#     ## ── 7. Save results ──
#     # showNotification(
#     #   id = "mcmc_calc_notify",
#     #   div(class = "big-notification", "Saving MCMC pred results..."),
#     #   duration = NULL, closeButton = TRUE
#     # )
#     # 
#     # upsert_best_curve(
#     #   conn   = conn,
#     #   df     = mcmc_pred_processed,
#     #   schema = "madi_results",
#     #   table  = "best_pred_all",
#     #   notify = shiny_notify(session)
#     # )
#     # 
#     # showNotification(
#     #   id = "mcmc_calc_notify",
#     #   div(class = "big-notification", "Saving MCMC sample results..."),
#     #   duration = NULL, closeButton = TRUE
#     # )
#     # 
#     # upsert_best_curve(
#     #   conn   = conn,
#     #   df     = mcmc_sample_processed,
#     #   schema = "madi_results",
#     #   table  = "best_sample_se_all",
#     #   notify = shiny_notify(session)
#     # )
#     
#     showNotification(
#       id = "mcmc_calc_notify",
#       div(class = "big-notification",
#           paste0("Completed: MCMC Robust (", scope_label, ")")),
#       duration = 5, closeButton = TRUE
#     )
#     
#     removeNotification("mcmc_calc_notify")
#     
#   }, error = function(e) {
#     showNotification(paste("Error during MCMC processing:", e$message),
#                      type = "error", duration = NULL, closeButton = TRUE)
#     removeNotification("mcmc_calc_notify")
#     
#   }, finally = {
#     is_batch_processing(FALSE)
#   })
# })
# # observeEvent(input$run_mcmc_calc, ignoreInit = TRUE, {
# #   
# #   scope <- input$save_scope
# #   
# #   if (is_batch_processing()) {
# #     showNotification("Batch processing is already running. Please wait.",
# #                      type = "warning", duration = 10, closeButton = TRUE)
# #     return()
# #   }
# #   
# #   req(input$qc_component == "Standard Curve",
# #       input$readxMap_study_accession != "Click here",
# #       input$readxMap_experiment_accession != "Click here")
# #   
# #   df <- concentration_calc_df()
# #   interp_status <- get_status(df, scope, "interpolated")
# #   
# #   if (interp_status != "completed") {
# #     showNotification(
# #       "Interpolated concentrations must be completed before running MCMC Robust.",
# #       type = "error", duration = 10, closeButton = TRUE
# #     )
# #     return()
# #   }
# #   
# #   is_batch_processing(TRUE)
# #   
# #   tryCatch({
# #     
# #     scope_label <- switch(scope,
# #                           "study"      = "all experiments",
# #                           "experiment" = "current experiment",
# #                           "plate"      = "current plate"
# #     )
# #     
# #     showNotification(
# #       id = "mcmc_calc_notify",
# #       div(class = "big-notification",
# #           paste0("Running MCMC Robust for ", scope_label, "...")),
# #       duration = NULL, closeButton = TRUE
# #     )
# #     
# #     study      <- input$readxMap_study_accession
# #     experiment <- input$readxMap_experiment_accession
# #     plate      <- input$sc_plate_select
# #     proj       <- userWorkSpaceID()
# #     
# #     ## ── 1. Fetch glance at correct scope ──
# #     best_glance_all <- switch(scope,
# #                               "study"      = fetch_best_glance_all_study(study, proj, conn),
# #                               "experiment" = fetch_best_glance_all_experiment(study, experiment, proj, conn),
# #                               "plate"      = fetch_best_glance_all_plate(study, experiment, plate, proj, conn)
# #     )
# #     
# #     ids_to_run <- as.integer(unique(best_glance_all$best_glance_all_id))
# #     
# #     if (length(ids_to_run) == 0) {
# #       stop("No interpolated results found for selected scope.")
# #     }
# #     
# #     cat("IDs to run:", ids_to_run, "\n")
# #     
# #     ## ── 2. Fetch child tables by IDs ──
# #     best_pred_all   <- fetch_best_pred_all_by_ids(ids_to_run, proj, conn)
# #     best_sample_all <- fetch_best_sample_se_all_by_ids(ids_to_run, proj, conn)
# #     
# #     ## ── 3. Loop per glance ID ──
# #     mcmc_pred_list   <- list()
# #     mcmc_sample_list <- list()
# #     
# #     for (pid in ids_to_run) {
# #       
# #       glance_row <- best_glance_all[best_glance_all$best_glance_all_id == pid, ]
# #       pred_df    <- best_pred_all[best_pred_all$best_glance_all_id == pid, ]
# #       sample_df  <- best_sample_all[best_sample_all$best_glance_all_id == pid, ]
# #       
# #       if (nrow(pred_df) == 0 || nrow(sample_df) == 0) {
# #         warning(paste("Skipping pid", pid, "- empty pred or sample data"))
# #         next
# #       }
# #       
# #       model_name   <- trimws(as.character(glance_row$crit))
# #       response_var <- unique(sample_df$assay_response_variable)
# #       
# #       if (length(response_var) == 0 || is.na(response_var)) {
# #         warning(paste("Skipping pid", pid, "- missing response variable"))
# #         next
# #       }
# #       
# #       ## ── Build params from stored glance values — no refit ──
# #       params_stored <- c(a = glance_row$a, b = glance_row$b,
# #                          c = glance_row$c, d = glance_row$d)
# #       if (model_name %in% c("Y5", "Yd5")) params_stored["g"] <- glance_row$g
# #       params_stored <- params_stored[!is.na(params_stored)]
# #       
# #       if (length(params_stored) == 0) {
# #         warning(paste("Skipping pid", pid, "- no valid parameters found"))
# #         next
# #       }
# #       
# #       ## ── Rename sample assay_response back to response_var ──
# #       sample_df_jags <- sample_df
# #       names(sample_df_jags)[names(sample_df_jags) == "assay_response"] <- response_var
# #       
# #       ## ── Build best_fit_out — no fit object ──
# #       best_fit_out <- list(
# #         best_model_name = model_name,
# #         params          = params_stored,
# #         resid_sigma     = glance_row$mse,
# #         pred_se         = pred_df,
# #         fixed_a_result  = if (!is.na(glance_row$a) && glance_row$bkg_method != "ignored") glance_row$a else NULL,
# #         response_var    = response_var,
# #         plate_samples   = sample_df_jags,
# #         best_fit = list(
# #           best_model_name = model_name,
# #           params          = params_stored,
# #           resid_sigma     = glance_row$mse,
# #           best_data       = pred_df        # wrapper renames x/yhat internally
# #         )
# #       )
# #       
# #       showNotification(
# #         id = "mcmc_calc_notify",
# #         div(class = "big-notification", HTML(paste0(
# #           "Running MCMC\n ",
# #           "Antigen: ", glance_row$antigen, "\n",
# #           "Plate: ",   glance_row$plate,   "\n",
# #           "Source: ",  glance_row$source
# #         ))),
# #         duration = NULL, closeButton = TRUE
# #       )
# #       
# #       bayes_pred <- tryCatch(
# #         run_jags_predicted_concentration_wrapper(best_fit_out, input_df = "pred_se"),
# #         error = function(e) {
# #           warning(paste("MCMC pred failed for pid", pid, ":", e$message))
# #           NULL
# #         }
# #       )
# #       
# #       bayes_sample <- tryCatch(
# #         run_jags_predicted_concentration_wrapper(best_fit_out, input_df = "sample_se"),
# #         error = function(e) {
# #           warning(paste("MCMC sample failed for pid", pid, ":", e$message))
# #           NULL
# #         }
# #       )
# #       
# #       if (!is.null(bayes_pred)) {
# #         bayes_pred$best_glance_all_id <- pid
# #         mcmc_pred_list[[as.character(pid)]] <- bayes_pred
# #       }
# #       
# #       if (!is.null(bayes_sample)) {
# #         bayes_sample$best_glance_all_id <- pid
# #         mcmc_sample_list[[as.character(pid)]] <- bayes_sample
# #       }
# #     }
# #     
# #     if (length(mcmc_pred_list) == 0) {
# #       stop("MCMC produced no results - check input data.")
# #     }
# #     
# #     ## ── 4. Bind results ──
# #     mcmc_pred_all   <<- dplyr::bind_rows(mcmc_pred_list)
# #     mcmc_sample_all <<- dplyr::bind_rows(mcmc_sample_list)
# #     
# # 
# #     
# #     # ## ── 7. Save results ──
# #     # showNotification(
# #     #   id = "mcmc_calc_notify",
# #     #   div(class = "big-notification", "Saving MCMC pred results..."),
# #     #   duration = NULL, closeButton = TRUE
# #     # )
# #     # 
# #     # upsert_best_curve(
# #     #   conn   = conn,
# #     #   df     = mcmc_pred_processed,
# #     #   schema = "madi_results",
# #     #   table  = "best_pred_all",
# #     #   notify = shiny_notify(session)
# #     # )
# #     # 
# #     # showNotification(
# #     #   id = "mcmc_calc_notify",
# #     #   div(class = "big-notification", "Saving MCMC sample results..."),
# #     #   duration = NULL, closeButton = TRUE
# #     # )
# #     # 
# #     # upsert_best_curve(
# #     #   conn   = conn,
# #     #   df     = mcmc_sample_processed,
# #     #   schema = "madi_results",
# #     #   table  = "best_sample_se_all",
# #     #   notify = shiny_notify(session)
# #     # )
# #     # 
# #     showNotification(
# #       id = "mcmc_calc_notify",
# #       div(class = "big-notification",
# #           paste0("Completed: MCMC Robust (", scope_label, ")")),
# #       duration = 5, closeButton = TRUE
# #     )
# #     
# #     removeNotification("mcmc_calc_notify")
# #     
# #   }, error = function(e) {
# #     showNotification(paste("Error during MCMC processing:", e$message),
# #                      type = "error", duration = NULL, closeButton = TRUE)
# #     removeNotification("mcmc_calc_notify")
# #     
# #   }, finally = {
# #     is_batch_processing(FALSE)
# #   })
# # })
# # observeEvent(input$run_mcmc_calc, ignoreInit = TRUE, {
# #   
# #   scope <- input$save_scope
# #   
# #   if (is_batch_processing()) {
# #     showNotification("Batch processing is already running. Please wait.",
# #                      type = "warning", duration = 10, closeButton = TRUE)
# #     return()
# #   }
# #   
# #   req(input$qc_component == "Standard Curve",
# #       input$readxMap_study_accession != "Click here",
# #       input$readxMap_experiment_accession != "Click here")
# #   
# #   ## Verify interpolated completed
# #   df <- concentration_calc_df()
# #   interp_status <- get_status(df, scope, "interpolated")
# #   
# #   if (interp_status != "completed") {
# #     showNotification(
# #       "Interpolated concentrations must be completed before running MCMC Robust.",
# #       type = "error", duration = 10, closeButton = TRUE
# #     )
# #     return()
# #   }
# #   
# #   is_batch_processing(TRUE)
# #   
# #   tryCatch({
# #     
# #     scope_label <- switch(scope,
# #                           "study"      = "all experiments",
# #                           "experiment" = "current experiment",
# #                           "plate"      = "current plate")
# #     
# #     showNotification(
# #       id = "mcmc_calc_notify",
# #       div(class = "big-notification",
# #           paste0("Running MCMC Robust for ", scope_label, "...")),
# #       duration = NULL, closeButton = TRUE
# #     )
# #     
# #     study      <- input$readxMap_study_accession
# #     experiment <- input$readxMap_experiment_accession
# #     plate      <- input$sc_plate_select
# #     proj       <- userWorkSpaceID()
# #     
# #     ## ── 1. Fetch glance at correct scope (SQL handles filtering) ──
# #     best_glance_all <- switch(scope,
# #                               "study"      = fetch_best_glance_all_study(study, proj, conn),
# #                               "experiment" = fetch_best_glance_all_experiment(study, experiment, proj, conn),
# #                               "plate"      = fetch_best_glance_all_plate(study, experiment, plate, proj, conn)
# #     )
# #     
# #     ids_to_run <- unique(best_glance_all$best_glance_all_id)
# #     # 
# #     # if (length(ids_to_run) == 0) {
# #     #   stop("No interpolated results found for selected scope.")
# #     # }
# #     
# #     ## ── 2. Fetch child tables by IDs ──
# #     best_pred_all   <- fetch_best_pred_all_by_ids(ids_to_run, proj, conn)
# #     best_sample_all <- fetch_best_sample_se_all_by_ids(ids_to_run, proj, conn)
# #     
# #     ## ── 3. Loop per glance ID ──
# #     mcmc_pred_list   <- list()
# #     mcmc_sample_list <- list()
# #     
# #     for (pid in ids_to_run) {
# #       
# #       glance_row <- best_glance_all[best_glance_all$best_glance_all_id == pid, ]
# #       pred_df    <- best_pred_all[best_pred_all$best_glance_all_id == pid, ]
# #       sample_df  <- best_sample_all[best_sample_all$best_glance_all_id == pid, ]
# #       
# #       ## Guard against empty subsets
# #       if (nrow(pred_df) == 0 || nrow(sample_df) == 0) {
# #         warning(paste("Skipping pid", pid, "- empty pred or sample data"))
# #         next
# #       }
# #       
# #       best_fit_out <- list(
# #         best_model_name = glance_row$crit,        # formula stores the model
# #         a               = glance_row$a,
# #         b               = glance_row$b,
# #         c               = glance_row$c,
# #         d               = glance_row$d,
# #         g               = glance_row$g,
# #         is_log_response = glance_row$is_log_response,
# #         is_log_x        = glance_row$is_log_x,
# #         bkg_method      = glance_row$bkg_method,
# #         pred_se         = pred_df,
# #         sample_se       = sample_df
# #       )
# #       
# #       bayes_pred <- run_jags_predicted_concentration_wrapper(
# #         best_fit_out,
# #         input_df = "pred_se"
# #       )
# #       
# #       bayes_sample <- run_jags_predicted_concentration_wrapper(
# #         best_fit_out,
# #         input_df = "sample_se"
# #       )
# #       
# #       bayes_pred$best_glance_all_id   <- pid
# #       bayes_sample$best_glance_all_id <- pid
# #       
# #       mcmc_pred_list[[as.character(pid)]]   <- bayes_pred
# #       mcmc_sample_list[[as.character(pid)]] <- bayes_sample
# #     }
# #     
# #     if (length(mcmc_pred_list) == 0) {
# #       stop("MCMC produced no results - check input data.")
# #     }
# #     
# #     mcmc_pred_all   <<- dplyr::bind_rows(mcmc_pred_list)
# #     mcmc_sample_all <<- dplyr::bind_rows(mcmc_sample_list)
# #     
# #     # ## ── 4. Save results ──
# #     # upsert_best_curve(conn = conn, df = mcmc_pred_all,
# #     #                   schema = "madi_results", table = "mcmc_pred_all",
# #     #                   notify = shiny_notify(session))
# #     # 
# #     # upsert_best_curve(conn = conn, df = mcmc_sample_all,
# #     #                   schema = "madi_results", table = "mcmc_sample_all",
# #     #                   notify = shiny_notify(session))
# #     # 
# #     showNotification(
# #       id = "mcmc_calc_notify",
# #       div(class = "big-notification",
# #           paste0("Completed: MCMC Robust (", scope_label, ")")),
# #       duration = 5, closeButton = TRUE
# #     )
# #     removeNotification("mcmc_calc_notify")
# #     
# #   }, error = function(e) {
# #     showNotification(paste("Error during MCMC processing:", e$message),
# #                      type = "error", duration = NULL, closeButton = TRUE)
# #     removeNotification("mcmc_calc_notify")
# #   }, finally = {
# #     is_batch_processing(FALSE)
# #   })
# # })
# # observeEvent(input$run_mcmc_calc, ignoreInit = TRUE, {
# # 
# #   scope <- input$save_scope
# # 
# #   if (is_batch_processing()) {
# #     showNotification("Batch processing is already running. Please wait.",
# #                      type = "warning", duration = 10, closeButton = TRUE)
# #     return()
# #   }
# # 
# #   req(input$qc_component == "Standard Curve",
# #       input$readxMap_study_accession != "Click here",
# #       input$readxMap_experiment_accession != "Click here")
# # 
# #   ## Verify interpolated completed
# #   df <- concentration_calc_df()
# #   interp_status <- get_status(df, scope, "interpolated")
# # 
# #   if (interp_status != "completed") {
# #     showNotification(
# #       "Interpolated concentrations must be completed before running MCMC Robust.",
# #       type = "error", duration = 10, closeButton = TRUE
# #     )
# #     return()
# #   }
# # 
# #   is_batch_processing(TRUE)
# # 
# #   tryCatch({
# # 
# #     scope_label <- switch(scope,
# #                           "study"      = "all experiments",
# #                           "experiment" = "current experiment",
# #                           "plate"      = "current plate")
# # 
# #     showNotification(
# #       id = "mcmc_calc_notify",
# #       div(class = "big-notification",
# #           paste0("Running MCMC Robust for ", scope_label, "...")),
# #       duration = NULL, closeButton = TRUE
# #     )
# # 
# #     study      <- input$readxMap_study_accession
# #     experiment <- input$readxMap_experiment_accession
# #     plate     <- input$sc_plate_select
# #     proj       <- userWorkSpaceID()
# # 
# #     ## ─────────────────────────────
# #     ## 1️⃣ Pull study-level data
# #     ## ─────────────────────────────
# # 
# #     best_glance_all <- switch(scope,
# #                               "study"      = fetch_best_glance_all_study(study, proj, conn),
# #                               "experiment" = fetch_best_glance_all_experiment(study, experiment, proj, conn),
# #                               "plate"      = fetch_best_glance_all_plate(study, experiment, plate, proj, conn)
# #     )
# #     
# #     ids_to_run <- unique(best_glance_all$best_glance_all_id)
# #     
# #     best_pred_all   <- fetch_best_pred_all_by_ids(ids_to_run, proj, conn)
# #     best_sample_all <- fetch_best_sample_se_all_by_ids(ids_to_run, proj, conn)
# # 
# #     ## ─────────────────────────────
# #     ## 2️⃣ Apply scope filtering
# #     ## ─────────────────────────────
# # 
# #     if (scope == "experiment") {
# #       best_glance_all <- best_glance_all[
# #         best_glance_all$experiment_accession == experiment_id, ]
# #     }
# # 
# #     if (scope == "plate") {
# #       best_glance_all <- best_glance_all[
# #         best_glance_all$experiment_accession == experiment_id &
# #           best_glance_all$plate == plate_id, ]
# #     }
# # 
# #     ids_to_run <- unique(best_glance_all$best_glance_all_id)
# # 
# #     if (length(ids_to_run) == 0) {
# #       stop("No interpolated results found for selected scope.")
# #     }
# # 
# #     ## ─────────────────────────────
# #     ## 3️⃣ Restrict child tables by IDs
# #     ## ─────────────────────────────
# # 
# #     best_pred_all <- best_pred_all[
# #       best_pred_all$best_glance_all_id %in% ids_to_run, ]
# # 
# #     best_sample_all <- best_sample_all[
# #       best_sample_all$best_glance_all_id %in% ids_to_run, ]
# # 
# #     ## ─────────────────────────────
# #     ## 4️⃣ Loop per plate-model
# #     ## ─────────────────────────────
# # 
# #     mcmc_pred_list   <- list()
# #     mcmc_sample_list <- list()
# # 
# #     for (pid in ids_to_run) {
# # 
# #       glance_row <- best_glance_all[
# #         best_glance_all$best_glance_all_id == pid, ]
# # 
# #       pred_df <- best_pred_all[
# #         best_pred_all$best_glance_all_id == pid, ]
# # 
# #       sample_df <- best_sample_all[
# #         best_sample_all$best_glance_all_id == pid, ]
# # 
# #       best_fit_out <- list(
# #         best_model_name = glance_row$model_name,
# #         fit             = unserialize(glance_row$fit_object[[1]]),
# #         pred_se         = pred_df,
# #         plate_samples   = sample_df,
# #         best_fit        = list(
# #           best_model_name = glance_row$model_name,
# #           best_fit        = unserialize(glance_row$fit_object[[1]]),
# #           best_data       = pred_df
# #         ),
# #         fixed_a_result  = glance_row$fixed_a_result,
# #         response_var    = glance_row$response_variable
# #       )
# # 
# #       ## Run JAGS
# # 
# #       bayes_pred <- run_jags_predicted_concentration_wrapper(
# #         best_fit_out,
# #         input_df = "pred_se"
# #       )
# # 
# #       bayes_sample <- run_jags_predicted_concentration_wrapper(
# #         best_fit_out,
# #         input_df = "sample_se"
# #       )
# # 
# #       bayes_pred$best_glance_all_id   <- pid
# #       bayes_sample$best_glance_all_id <- pid
# # 
# #       mcmc_pred_list[[as.character(pid)]]   <- bayes_pred
# #       mcmc_sample_list[[as.character(pid)]] <- bayes_sample
# #     }
# # 
# #     mcmc_pred_all   <<- dplyr::bind_rows(mcmc_pred_list)
# #     mcmc_sample_all <<- dplyr::bind_rows(mcmc_sample_list)
# 
#     ## ─────────────────────────────
#     ## 5️⃣ Save results
#     ## ─────────────────────────────
#     #
#     # upsert_best_curve(
#     #   conn   = conn,
#     #   df     = mcmc_pred_all,
#     #   schema = "madi_results",
#     #   table  = "mcmc_pred_all",
#     #   notify = shiny_notify(session)
#     # )
#     #
#     # upsert_best_curve(
#     #   conn   = conn,
#     #   df     = mcmc_sample_all,
#     #   schema = "madi_results",
#     #   table  = "mcmc_sample_all",
#     #   notify = shiny_notify(session)
#     # )
#     #
#     # showNotification(
#     #   id = "mcmc_calc_notify",
#     #   div(class = "big-notification",
#     #       paste0("Completed: MCMC Robust (", scope_label, ")")),
#     #   duration = 5, closeButton = TRUE
#     # )
# 
# #     removeNotification("mcmc_calc_notify")
# # 
# #   }, error = function(e) {
# # 
# #     showNotification(
# #       paste("Error during MCMC processing:", e$message),
# #       type = "error",
# #       duration = NULL,
# #       closeButton = TRUE
# #     )
# # 
# #     removeNotification("mcmc_calc_notify")
# # 
# #   }, finally = {
# # 
# #     is_batch_processing(FALSE)
# # 
# #   })
# # })
# 
# # observeEvent(input$run_mcmc_calc, ignoreInit = TRUE, {
# #   
# #   scope <- input$save_scope
# #   
# #   if (is_batch_processing()) {
# #     showNotification("Batch processing is already running. Please wait.",
# #                      type = "warning", duration = 10, closeButton = TRUE)
# #     return()
# #   }
# #   
# #   req(input$qc_component == "Standard Curve",
# #       input$readxMap_study_accession != "Click here",
# #       input$readxMap_experiment_accession != "Click here")
# #   
# #   ## Verify interpolated is completed for this scope
# #   df <- concentration_calc_df()
# #   interp_status <- get_status(df, scope, "interpolated")
# #   
# #   if (interp_status != "completed") {
# #     showNotification(
# #       "Interpolated concentrations must be completed before running MCMC Robust.",
# #       type = "error", duration = 10, closeButton = TRUE
# #     )
# #     return()
# #   }
# #   
# #   is_batch_processing(TRUE)
# #   
# #   tryCatch({
# #     
# #     scope_label <- switch(scope,
# #                           "study"      = "all experiments",
# #                           "experiment" = "current experiment",
# #                           "plate"      = "current plate"
# #     )
# #     
# #     showNotification(
# #       id = "mcmc_calc_notify",
# #       div(class = "big-notification",
# #           paste0("Running MCMC Robust for ", scope_label, "...")),
# #       duration = NULL, closeButton = TRUE
# #     )
# #     
# #     ## ──────────────────────────────────────────────
# #     ## Add your MCMC Robust calculation logic here
# #     ## ──────────────────────────────────────────────
# #     ## ──────────────────────────────────────────────
# #     ## Pull best interpolated results
# #     ## ──────────────────────────────────────────────
# #     
# #     study_id      <- input$readxMap_study_accession
# #     experiment_id <- input$readxMap_experiment_accession
# #     plate_id      <- if (scope == "plate") input$sc_plate_select else NULL
# #     
# #     best_glance <- fetch_best_glance_all(
# #       conn              = conn,
# #       study_accession   = study_id,
# #       experiment_accession = if (scope == "study") NULL else experiment_id,
# #       plate             = plate_id
# #     )
# #     
# #     best_pred <- fetch_best_pred_all(
# #       conn              = conn,
# #       study_accession   = study_id,
# #       experiment_accession = if (scope == "study") NULL else experiment_id,
# #       plate             = plate_id
# #     )
# #     
# #     best_sample_se <- fetch_best_sample_se_all(
# #       conn              = conn,
# #       study_accession   = study_id,
# #       experiment_accession = if (scope == "study") NULL else experiment_id,
# #       plate             = plate_id
# #     )
# #     
# #     ## ──────────────────────────────────────────────
# #     ## Split by best_glance_all_id (one plate-model per row)
# #     ## ──────────────────────────────────────────────
# #     
# #     plate_ids <- unique(best_glance$best_glance_all_id)
# #     
# #     mcmc_pred_all   <- list()
# #     mcmc_sample_all <- list()
# #     
# #     for (pid in plate_ids) {
# #       
# #       glance_row <- best_glance[best_glance$best_glance_all_id == pid, ]
# #       pred_df    <- best_pred[best_pred$best_glance_all_id == pid, ]
# #       sample_df  <- best_sample_se[best_sample_se$best_glance_all_id == pid, ]
# #       
# #       ## Rebuild best_fit_out structure
# #       best_fit_out <- list(
# #         best_model_name = glance_row$model_name,
# #         fit             = unserialize(glance_row$fit_object),
# #         pred_se         = pred_df,
# #         plate_samples   = sample_df,
# #         best_fit        = list(
# #           best_model_name = glance_row$model_name,
# #           best_fit        = unserialize(glance_row$fit_object),
# #           best_data       = pred_df
# #         ),
# #         fixed_a_result  = glance_row$fixed_a_result,
# #         response_var    = glance_row$response_variable
# #       )
# #       
# #       ## ── Run JAGS for pred_se
# #       bayes_pred <- run_jags_predicted_concentration_wrapper(
# #         best_fit_out,
# #         input_df = "pred_se"
# #       )
# #       
# #       ## ── Run JAGS for sample_se
# #       bayes_sample <- run_jags_predicted_concentration_wrapper(
# #         best_fit_out,
# #         input_df = "sample_se"
# #       )
# #       
# #       bayes_pred$best_glance_all_id   <- pid
# #       bayes_sample$best_glance_all_id <- pid
# #       
# #       mcmc_pred_all[[as.character(pid)]]   <- bayes_pred
# #       mcmc_sample_all[[as.character(pid)]] <- bayes_sample
# #     }
# #     
# #     mcmc_pred_all   <<- dplyr::bind_rows(mcmc_pred_all)
# #     mcmc_sample_all <<- dplyr::bind_rows(mcmc_sample_all)
# #     
# #     ## ──────────────────────────────────────────────
# #     ## Save results
# #     ## ──────────────────────────────────────────────
# #     # upsert_best_curve(
# #     #   conn   = conn,
# #     #   df     = mcmc_pred_all,
# #     #   schema = "madi_results",
# #     #   table  = "mcmc_pred_all",
# #     #   notify = shiny_notify(session)
# #     # )
# #     # 
# #     # upsert_best_curve(
# #     #   conn   = conn,
# #     #   df     = mcmc_sample_all,
# #     #   schema = "madi_results",
# #     #   table  = "mcmc_sample_all",
# #     #   notify = shiny_notify(session)
# #     # )
# #     # 
# #     # showNotification(
# #     #   id = "mcmc_calc_notify",
# #     #   div(class = "big-notification",
# #     #       paste0("Completed: MCMC Robust (", scope_label, ")")),
# #     #   duration = 5,
# #     #   closeButton = TRUE
# #     # )
# #     # removeNotification("mcmc_calc_notify")
# #     # 
# #     # showNotification("MCMC Robust calculation not yet implemented.",
# #     #                  type = "warning", duration = 5)
# #     removeNotification("mcmc_calc_notify")
# #     
# #   }, error = function(e) {
# #     showNotification(paste("Error during MCMC processing:", e$message),
# #                      type = "error", duration = NULL, closeButton = TRUE)
# #     removeNotification("mcmc_calc_notify")
# #   }, finally = {
# #     is_batch_processing(FALSE)
# #   })
# # })
# 
# # =================================================================
# # OLDSERVER
# # =================================================================
# 
# # is_batch_processing <- reactiveVal(FALSE)
# # 
# # # -----------------------------------------------------------------
# # # A. Track active scope — clear other panels when one is clicked
# # # -----------------------------------------------------------------
# # observeEvent(input$concentrationMethodStudy, {
# #   updateTextInput(session, "activeScope", value = "study")
# #   shinyjs::runjs("
# #     $('input[name=\"concentrationMethodExperiment\"]').prop('checked', false);
# #     $('input[name=\"concentrationMethodPlate\"]').prop('checked', false);
# #     Shiny.setInputValue('concentrationMethodExperiment', null);
# #     Shiny.setInputValue('concentrationMethodPlate', null);
# #   ")
# # })
# # 
# # observeEvent(input$concentrationMethodExperiment, {
# #   updateTextInput(session, "activeScope", value = "experiment")
# #   shinyjs::runjs("
# #     $('input[name=\"concentrationMethodStudy\"]').prop('checked', false);
# #     $('input[name=\"concentrationMethodPlate\"]').prop('checked', false);
# #     Shiny.setInputValue('concentrationMethodStudy', null);
# #     Shiny.setInputValue('concentrationMethodPlate', null);
# #   ")
# # })
# # 
# # observeEvent(input$concentrationMethodPlate, {
# #   updateTextInput(session, "activeScope", value = "plate")
# #   shinyjs::runjs("
# #     $('input[name=\"concentrationMethodStudy\"]').prop('checked', false);
# #     $('input[name=\"concentrationMethodExperiment\"]').prop('checked', false);
# #     Shiny.setInputValue('concentrationMethodStudy', null);
# #     Shiny.setInputValue('concentrationMethodExperiment', null);
# #   ")
# # })
# # 
# # # -----------------------------------------------------------------
# # # B. Highlight active panel + dim inactive ones
# # # -----------------------------------------------------------------
# # observeEvent(input$activeScope, {
# #   scopes <- c("study", "experiment", "plate")
# #   active <- input$activeScope
# #   
# #   for (s in scopes) {
# #     if (s == active) {
# #       shinyjs::runjs(sprintf(
# #         "$('#panel_%s').addClass('scope-panel-active').removeClass('scope-panel-inactive');", s
# #       ))
# #     } else {
# #       shinyjs::runjs(sprintf(
# #         "$('#panel_%s').removeClass('scope-panel-active').addClass('scope-panel-inactive');", s
# #       ))
# #     }
# #   }
# # })
# # 
# # # -----------------------------------------------------------------
# # # C. Render the button with HTML multi-line label
# # # -----------------------------------------------------------------
# # output$run_batch_fit_ui <- renderUI({
# #   scope  <- input$activeScope
# #   method <- switch(scope,
# #                    "study"      = input$concentrationMethodStudy,
# #                    "experiment" = input$concentrationMethodExperiment,
# #                    "plate"      = input$concentrationMethodPlate,
# #                    NULL
# #   )
# #   
# #   if (!is.null(scope) && nzchar(scope) && !is.null(method)) {
# #     
# #     method_label <- switch(method,
# #                            "interpolated" = "interpolated",
# #                            "mcmc_robust"  = "MCMC Robust",
# #                            method
# #     )
# #     
# #     scope_label <- switch(scope,
# #                           "study"      = "(All Experiments)",
# #                           "experiment" = "(Current Experiment)",
# #                           "plate"      = "(Current Plate)"
# #     )
# #     
# #     btn_label <- HTML(paste0(
# #       "Calculate Standard Curves<br>",
# #       "with <strong>", method_label, "</strong> concentrations<br>",
# #       scope_label
# #     ))
# #     
# #     btn_style <- "background-color: #7DAF4C; border-color: #91CF60; color: white;
# #                    padding: 12px 40px; font-size: 15px; line-height: 1.5;
# #                    white-space: normal;"
# #   } else {
# #     btn_label <- "Select a scope and method above"
# #     btn_style <- "background-color: #cccccc; border-color: #bbbbbb; color: #666666;
# #                    padding: 12px 40px; font-size: 15px; line-height: 1.5;
# #                    white-space: normal; cursor: not-allowed;"
# #   }
# #   
# #   actionButton(
# #     inputId = "run_batch_fit",
# #     label   = btn_label,
# #     icon    = icon("play"),
# #     style   = btn_style
# #   )
# # })
# # 
# # # -----------------------------------------------------------------
# # # D. Single button handler — runs calculation for selected scope+method
# # # -----------------------------------------------------------------
# # observeEvent(input$run_batch_fit, ignoreInit = TRUE, {
# #   
# #   ## 1. Determine scope + method
# #   scope  <- input$activeScope
# #   method <- switch(scope,
# #                    "study"      = input$concentrationMethodStudy,
# #                    "experiment" = input$concentrationMethodExperiment,
# #                    "plate"      = input$concentrationMethodPlate,
# #                    NULL
# #   )
# #   
# #   ## 2. Validate selection
# #   if (is.null(scope) || !nzchar(scope) || is.null(method)) {
# #     showNotification("Please select a scope and method first.",
# #                      type = "warning", duration = 5)
# #     return()
# #   }
# #   
# #   ## 3. Check if already running
# #   if (is_batch_processing()) {
# #     showNotification("Batch processing is already running. Please wait.",
# #                      type = "warning", duration = 10, closeButton = TRUE)
# #     return()
# #   }
# #   
# #   ## 4. Prereqs
# #   req(input$qc_component == "Standard Curve",
# #       input$readxMap_study_accession != "Click here",
# #       input$readxMap_experiment_accession != "Click here",
# #       input$study_level_tabs == "Experiments",
# #       input$main_tabs == "view_files_tab")
# #   
# #   if (is.null(input$readxMap_study_accession) ||
# #       input$readxMap_study_accession == "") {
# #     showNotification("Please select a study before running batch processing.",
# #                      type = "error", duration = 10, closeButton = TRUE)
# #     return()
# #   }
# #   
# #   ## 5. Start processing
# #   is_batch_processing(TRUE)
# #   
# #   tryCatch({
# #     
# #     scope_label <- switch(scope,
# #                           "study"      = "all experiments",
# #                           "experiment" = "current experiment",
# #                           "plate"      = "current plate"
# #     )
# #     
# #     method_label <- switch(method,
# #                            "interpolated" = "interpolated",
# #                            "mcmc_robust"  = "MCMC Robust",
# #                            method
# #     )
# #     
# #     showNotification(
# #       id = "batch_sc_fit_notify",
# #       div(class = "big-notification",
# #           paste0("Running ", method_label, " for ", scope_label, "...")),
# #       duration = NULL, closeButton = TRUE
# #     )
# #     
# #     ## ── Build experiment list based on scope ──
# #     headers <- fetch_db_header_experiments(
# #       study_accession = input$readxMap_study_accession,
# #       conn = conn
# #     )
# #     
# #     exp_list <- switch(scope,
# #                        "study"      = unique(headers$experiment_accession),
# #                        "experiment" = input$readxMap_experiment_accession,
# #                        "plate"      = input$readxMap_experiment_accession
# #     )
# #     
# #     ## ── Pull data ──
# #     loaded_data_list <- list()
# #     for (exp in exp_list) {
# #       loaded_data_list[[exp]] <- pull_data(
# #         study_accession      = input$readxMap_study_accession,
# #         experiment_accession = exp,
# #         project_id           = userWorkSpaceID(),
# #         conn                 = conn
# #       )
# #     }
# #     
# #     ## ── Filter to plate if scope = "plate" ──
# #     if (scope == "plate") {
# #       plate <- input$sc_plate_select
# #       loaded_data_list <- lapply(loaded_data_list, function(x) {
# #         for (tbl in c("plates", "standards", "samples", "blanks")) {
# #           if (!is.null(x[[tbl]]) && nrow(x[[tbl]]) > 0) {
# #             x[[tbl]] <- x[[tbl]][x[[tbl]]$plate_nom == plate, , drop = FALSE]
# #           }
# #         }
# #         x
# #       })
# #     }
# #     
# #     ## ── Branch on method ──
# #     if (method == "interpolated") {
# #       
# #       ## ── Get response variable ──
# #       response_var <- loaded_data_list[[exp_list[1]]]$response_var
# #       
# #       ## ── Compute SE table ──
# #       all_standards <- do.call(rbind, lapply(loaded_data_list, function(x) x$standards))
# #       se_antigen_table <- compute_antigen_se_table(
# #         standards_data = all_standards,
# #         response_col   = response_var,
# #         dilution_col   = "dilution",
# #         plate_col      = "plate",
# #         grouping_cols  = c("study_accession", "experiment_accession", "source", "antigen"),
# #         method         = "pooled_within",
# #         verbose        = TRUE
# #       )
# #       
# #       ## ── Study parameters ──
# #       verbose     <- FALSE
# #       model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
# #       param_group <- "standard_curve_options"
# #       
# #       study_params <- fetch_study_parameters(
# #         study_accession = input$readxMap_study_accession,
# #         param_user      = currentuser(),
# #         param_group     = param_group,
# #         project_id      = userWorkSpaceID(),
# #         conn            = conn
# #       )
# #       
# #       ## ── Build antigen lists ──
# #       antigen_list_res <- build_antigen_list(
# #         exp_list         = exp_list,
# #         loaded_data_list = loaded_data_list,
# #         study_accession  = input$readxMap_study_accession
# #       )
# #       
# #       antigen_plate_list_res <- build_antigen_plate_list(
# #         antigen_list_result = antigen_list_res,
# #         loaded_data_list    = loaded_data_list
# #       )
# #       
# #       ## ── Prep data ──
# #       prepped_data_list_res <- prep_plate_data_batch(
# #         antigen_plate_list_res = antigen_plate_list_res,
# #         study_params           = study_params,
# #         verbose                = verbose
# #       )
# #       
# #       antigen_plate_list_res$antigen_plate_list_ids <-
# #         prepped_data_list_res$antigen_plate_name_list
# #       
# #       ## ── Fit curves ──
# #       batch_fit_res <- fit_experiment_plate_batch(
# #         prepped_data_list_res  = prepped_data_list_res,
# #         antigen_plate_list_res = antigen_plate_list_res,
# #         model_names            = model_names,
# #         study_params           = study_params,
# #         se_antigen_table       = se_antigen_table,
# #         verbose                = verbose
# #       )
# #       
# #       ## ── Create outputs ──
# #       batch_outputs <- create_batch_fit_outputs(
# #         batch_fit_res          = batch_fit_res,
# #         antigen_plate_list_res = antigen_plate_list_res
# #       )
# #       
# #       batch_outputs_processed <- process_batch_outputs(
# #         batch_outputs = batch_outputs,
# #         response_var  = response_var,
# #         project_id    = userWorkSpaceID()
# #       )
# #       
# #       ## ── Save: best_glance_all ──
# #       upsert_best_curve(
# #         conn = conn, df = batch_outputs_processed$best_glance_all,
# #         schema = "madi_results", table = "best_glance_all",
# #         notify = shiny_notify(session)
# #       )
# #       showNotification(id = "batch_sc_fit_notify",
# #                        div(class = "big-notification", "Best Fit Statistics saved"),
# #                        duration = NULL, closeButton = TRUE)
# #       
# #       ## ── Retrieve lookup IDs ──
# #       study_to_save <- unique(batch_outputs_processed$best_glance_all$study_accession)
# #       glance_lookup <- DBI::dbGetQuery(conn, glue::glue("
# #         SELECT best_glance_all_id, study_accession, experiment_accession,
# #                plateid, plate, nominal_sample_dilution, source, antigen
# #         FROM madi_results.best_glance_all
# #         WHERE study_accession = '{study_to_save}';
# #       "))
# #       glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)
# #       
# #       keys <- c("study_accession", "experiment_accession", "plateid",
# #                 "plate", "nominal_sample_dilution", "source", "antigen")
# #       
# #       ## ── Join lookup IDs ──
# #       batch_outputs_processed$best_pred_all <-
# #         dplyr::inner_join(batch_outputs_processed$best_pred_all,
# #                           glance_lookup, by = keys)
# #       batch_outputs_processed$best_sample_se_all <-
# #         dplyr::inner_join(batch_outputs_processed$best_sample_se_all,
# #                           glance_lookup, by = keys)
# #       batch_outputs_processed$best_standard_all <-
# #         dplyr::inner_join(batch_outputs_processed$best_standard_all,
# #                           glance_lookup, by = keys)
# #       
# #       ## ── Save: best_plate_all ──
# #       upsert_best_curve(
# #         conn = conn, df = batch_outputs_processed$best_plate_all,
# #         schema = "madi_results", table = "best_plate_all",
# #         notify = shiny_notify(session)
# #       )
# #       showNotification(id = "batch_sc_fit_notify",
# #                        div(class = "big-notification", "Best Plates saved"),
# #                        duration = NULL, closeButton = TRUE)
# #       
# #       ## ── Save: best_tidy_all ──
# #       upsert_best_curve(
# #         conn = conn, df = batch_outputs_processed$best_tidy_all,
# #         schema = "madi_results", table = "best_tidy_all",
# #         notify = shiny_notify(session)
# #       )
# #       showNotification(id = "batch_sc_fit_notify",
# #                        div(class = "big-notification", "Best parameter estimates saved"),
# #                        duration = NULL, closeButton = TRUE)
# #       
# #       ## ── Save: best_pred_all ──
# #       upsert_best_curve(
# #         conn = conn, df = batch_outputs_processed$best_pred_all,
# #         schema = "madi_results", table = "best_pred_all",
# #         notify = shiny_notify(session)
# #       )
# #       showNotification(id = "batch_sc_fit_notify",
# #                        div(class = "big-notification", "Best Predicted standards saved"),
# #                        duration = NULL, closeButton = TRUE)
# #       
# #       ## ── Save: best_sample_se_all ──
# #       upsert_best_curve(
# #         conn = conn, df = batch_outputs_processed$best_sample_se_all,
# #         schema = "madi_results", table = "best_sample_se_all",
# #         notify = shiny_notify(session)
# #       )
# #       showNotification(id = "batch_sc_fit_notify",
# #                        div(class = "big-notification", "Best Predicted Samples saved"),
# #                        duration = NULL, closeButton = TRUE)
# #       
# #       ## ── Save: best_standard_all ──
# #       upsert_best_curve(
# #         conn = conn, df = batch_outputs_processed$best_standard_all,
# #         schema = "madi_results", table = "best_standard_all",
# #         notify = shiny_notify(session)
# #       )
# #       showNotification(id = "batch_sc_fit_notify",
# #                        div(class = "big-notification", "Best Standards saved"),
# #                        duration = NULL, closeButton = TRUE)
# #       
# #     } else if (method == "mcmc_robust") {
# #       
# #       ## ── Future: MCMC Robust logic ──
# #       showNotification("MCMC Robust not yet implemented",
# #                        type = "warning", duration = 5)
# #       
# #     }
# #     
# #     ## ── Done ──
# #     showNotification(
# #       id = "batch_sc_fit_notify",
# #       div(class = "big-notification",
# #           paste0("Completed: ", method_label, " (", scope_label, ")")),
# #       duration = 5, closeButton = TRUE
# #     )
# #     removeNotification("batch_sc_fit_notify")
# #     
# #   }, error = function(e) {
# #     showNotification(
# #       paste("Error during batch processing:", e$message),
# #       type = "error", duration = NULL, closeButton = TRUE
# #     )
# #     removeNotification("batch_sc_fit_notify")
# #   }, finally = {
# #     is_batch_processing(FALSE)
# #   })
# # })
