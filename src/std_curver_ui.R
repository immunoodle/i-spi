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
              plotlyOutput("standard_curve", width = "75vw", height = "800px"),
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
              radioButtons(
                inputId = "save_scope",
                label = "Calculation scope",
                choices = c(
                  "Current plate"      = "plate",
                  "Current experiment" = "experiment",
                  "All experiments"    = "study"
                ),
                selected = "study",
                inline = TRUE
              ),
              actionButton("run_batch_fit", "Calculate Standard Curves")
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






    output$standard_curve_context <- renderUI({
      standards_n <- nrow(loaded_data$standards[loaded_data$standards$experiment_accession == selected_experiment,])
      blanks_n <- nrow(loaded_data$blanks[loaded_data$blanks$experiment_accession == selected_experiment,])
      blank_option <- study_params$blank_option
      constraints_methods <- unique(loaded_data$antigen_constraints$l_asy_constraint_method)
      invalid_constraints <- setdiff(constraints_methods, allowed_constraint_methods)

      standards_missing   <- standards_n == 0
      blanks_missing <- blanks_n == 0
      constraints_invalid <- length(invalid_constraints) > 0
      blank_required  <- blank_option != "ignored" # blank is required for all options except ignored.
      blank_blocking  <- blanks_missing && blank_required

      blank_labels <- c(
        ignored        = "Ignored",
        included       = "Included",
        subtracted     = "Subtracted 1 × Geometric Mean",
        subtracted_3x  = "Subtracted 3 × Geometric Mean",
        subtracted_5x  = "Subtracted 5 × Geometric Mean",
        subtracted_10x = "Subtracted 10 × Geometric Mean"
      )

      blank_label <- blank_labels[[blank_option]]


      reasons <- c()

      if (standards_missing) {
        reasons <- c(reasons, glue::glue("{standards_n} standards found in {selected_experiment}."))
      }

      if (constraints_invalid) {
        reasons <- c(reasons, glue::glue(
          "Invalid constraint methods: {paste(invalid_constraints, collapse=', ')}"
        ))
      }

      if (blank_blocking) {
        reasons <- c(reasons, glue::glue(
          "Blank control is set to {blank_label} in the study settings but {blanks_n} blanks found."
        ))
      }

      # conditional note to show next to blank when it is ok to have 0 blanks
      blank_note <- if (!blank_blocking) {
        glue::glue(" (ok since blank control is currently set to {blank_label} in study settings.)")
      } else {
        ""
      }

      # If any blocking reasons exist, show blocking message
      if (length(reasons) > 0) {

        return(HTML(glue::glue(
          "Standard curve fitting cannot proceed for {selected_experiment}.<br><br>",
          " Unmet requirements:<br>",
          "{paste(paste0('&nbsp;- ', reasons), collapse = '<br>')}<br><br>",
          "Details:<br>",
          "- Standards: {standards_n}<br>",
          "- Blanks: {blanks_n}{blank_note}<br>",
          "- Constraint method(s) found in {selected_experiment}: {paste(constraints_methods, collapse=', ')}<br>",
          "- Constraint method(s) valid: {ifelse(constraints_invalid, 'no', 'yes')}"
        )))
      }

      # Success case (none blocking)
      return(HTML(glue::glue(
        "{standards_n} standards found for {selected_experiment}.<br>",
        "{blanks_n} blanks found for {selected_experiment}.<br>",
        "Constraint method(s) found in {selected_experiment}: {paste(constraints_methods, collapse=', ')}<br>",
        "Constraint method(s) valid: {ifelse(constraints_invalid, 'no', 'yes')}<br>",
        "Current Blank Option selected (in study settings): {blank_label}<br><br>",
        "Standard curve fitting may proceed."
      )))
    })

    # reactive and exported output for conditional Panel
    can_fit_standard_curve <- reactive({
      standards_n <- nrow(loaded_data$standards[loaded_data$standards$experiment_accession == selected_experiment,])
      blanks_n <- nrow(loaded_data$blanks[loaded_data$blanks$experiment_accession == selected_experiment,])
      blank_option <- study_params$blank_option
      constraints_methods <- unique(loaded_data$antigen_constraints$l_asy_constraint_method)
      invalid_constraints <- setdiff(constraints_methods, allowed_constraint_methods)

      standards_missing   <- standards_n == 0
      blanks_missing      <- blanks_n == 0
      constraints_invalid <- length(invalid_constraints) > 0
      blank_required      <- blank_option != "ignored"
      blank_blocking      <- blanks_missing && blank_required

      # accumulate reasons
      reasons <- c()
      if (standards_missing)
        reasons <- c(reasons, glue::glue("{standards_n} standards found in {selected_experiment}."))

      if (constraints_invalid)
        reasons <- c(reasons, glue::glue("Invalid constraint methods: {paste(invalid_constraints, collapse=', ')}"))

      if (blank_blocking)
        reasons <- c(reasons, glue::glue("Blanking is '{blank_option}' but {blanks_n} blanks found."))

      !(length(reasons) > 0)

    })

    output$can_fit_standard_curve <- reactive({
      standards_n <- nrow(loaded_data$standards[loaded_data$standards$experiment_accession == selected_experiment,])
      blanks_n <- nrow(loaded_data$blanks[loaded_data$blanks$experiment_accession == selected_experiment,])
      blank_option <- study_params$blank_option
      constraints_methods <- unique(loaded_data$antigen_constraints$l_asy_constraint_method)
      invalid_constraints <- setdiff(constraints_methods, allowed_constraint_methods)

      standards_missing   <- standards_n == 0
      blanks_missing      <- blanks_n == 0
      constraints_invalid <- length(invalid_constraints) > 0
      blank_required      <- blank_option != "ignored"
      blank_blocking      <- blanks_missing && blank_required

      # accumulate reasons
      reasons <- c()
      if (standards_missing)
        reasons <- c(reasons, glue::glue("{standards_n} standards found."))

      if (constraints_invalid)
        reasons <- c(reasons, glue::glue("Invalid constraint methods: {paste(invalid_constraints, collapse=', ')}"))

      if (blank_blocking)
        reasons <- c(reasons, glue::glue("Blanking is '{blank_option}' but {blanks_n} blanks found."))

      !(length(reasons) > 0)

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

    antigen_plate <- reactive({
      req(input$sc_source_select,
          input$sc_antigen_select,
          input$sc_plate_select,
          loaded_data)

      result <- select_antigen_plate(
        loaded_data = loaded_data,
        study_accession = selected_study,
        experiment_accession = selected_experiment,
        source = input$sc_source_select,
        antigen = input$sc_antigen_select,
        plate = input$sc_plate_select,
        antigen_constraints =
          loaded_data$antigen_constraints[
            loaded_data$antigen_constraints$antigen == input$sc_antigen_select,
          ]
      )
      req(result)
      result
    })

    prepped_data <- reactive({
      plate <- antigen_plate()  # pull reactive value
      response_var <- loaded_data$response_var
      indep_var <- loaded_data$indep_var
      req(plate)


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
      summarize_model_parameters(models_fit_list = fit_robust_lm(),
                                 level = 0.95,
                                 model_names = model_names)

    })
#
    plot_data <- reactive({
      req(prepped_data())
      req(fit_robust_lm())
      req(antigen_plate())
      req(fit_params())

      response_var <- loaded_data$response_var
      indep_var <- loaded_data$indep_var

      plot_data <- get_plot_data(models_fit_list = fit_robust_lm(),
                                prepped_data = prepped_data()$data,
                                fit_params = fit_params(),
                                fixed_a_result =  antigen_plate()$fixed_a_result,
                                model_names = model_names,
                                x_var = indep_var,
                                y_var = response_var)
    })
#
    output$model_comparisions <- renderPlot({
      req(plot_data())
      response_var <- loaded_data$response_var
      indep_var <- loaded_data$indep_var

      plot_model_comparisons(plot_data = plot_data(),
                             model_names = model_names,
                             x_var = indep_var,
                             y_var = response_var,
                             is_display_log_response = input$display_log_response,
                             is_display_log_independent = input$display_log_independent,
                             use_patchwork = TRUE)

    })

    output$download_model_comparisons <- downloadHandler(
      filename = function() {
        paste0("model_comparison_", unique(plot_data()$dat$study_accession), unique(plot_data()$dat$experiment_accession),
               unique(plot_data()$dat$plate_nom), unique(plot_data()$dat$antigen), ".pdf")
      },

      content = function(file) {
        req(plot_data())

        response_var <- loaded_data$response_var
        indep_var    <- loaded_data$indep_var

        p <- plot_model_comparisons(
          plot_data   = plot_data(),
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
      req(fit_params())
      req(fit_robust_lm())
      req(fit_summary())
      req(plot_data())
      req(antigen_plate())
      req(prepped_data())
      req(model_constraints())

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

      best_fit <- select_model_fit_AIC(fit_summary = fit_summary(),
                                       fit_robust_lm = fit_robust_lm(),
                                       fit_params = fit_params(),
                                       plot_data = plot_data(),
                                       verbose = verbose)

      # add the glance for the best fit
      best_fit <- fit_qc_glance(best_fit = best_fit,
                                response_variable = response_var,
                                independent_variable = indep_var,
                                fixed_a_result = antigen_plate()$fixed_a_result,
                                antigen_settings = antigen_plate()$antigen_settings,
                                antigen_fit_options = prepped_data()$antigen_fit_options,
                                verbose = verbose)

      ## add the tidy to the best fit object
      best_fit <- tidy.nlsLM(best_fit = best_fit, fixed_a_result = antigen_plate()$fixed_a_result, model_constraints =
                               model_constraints(), antigen_settings = antigen_plate()$antigen_settings,
                               antigen_fit_options = prepped_data()$antigen_fit_options,
                            verbose = verbose)

      best_fit <- predict_and_propagate_error(
        best_fit = best_fit,
        response_var = response_var,
        antigen_plate = antigen_plate(),
        study_params = study_params,
        se_std_response = current_se,
        verbose = verbose
      )

      best_fit <- gate_samples(best_fit = best_fit,
                               response_variable = response_var,
                               pcov_threshold = antigen_plate()$antigen_settings$pcov_threshold,
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
      req(best_fit())
      req(antigen_plate())

      response_var <- loaded_data$response_var
      indep_var <- loaded_data$indep_var

      plot_standard_curve(best_fit = best_fit(),
                          is_display_log_response = input$display_log_response,
                          is_display_log_independent = input$display_log_independent,
                          pcov_threshold = antigen_plate()$antigen_settings$pcov_threshold,
                          independent_variable = indep_var,
                          response_variable = response_var)
    })

    output$download_best_fit_parameter_estimates  <-  downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, "x",
              input$sc_antigen_select, "tidy_parameter_estimates.csv", sep = "_")
      },
      content = function(file) {

        req(best_fit())


        # download data component (data frame)
        write.csv(best_fit()$best_tidy, file, row.names = FALSE)
      }
    )

    output$summary_statistics <- renderTable({
      req(best_fit())
      best_fit()$best_glance
    }, caption= "Summary Statistics",
    caption.placement = getOption("xtable.caption.placement", "top"))

   output$download_samples_above_ulod <- downloadHandler(
     filename = function() {
       paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, "x",
             input$sc_antigen_select, "samples_above_ulod.csv", sep = "_")
     },
     content = function(file) {

       req(best_fit())
       req(best_fit())

       # download data component (data frame)
       write.csv(best_fit()$sample_se[best_fit()$sample_se$gate_class_lod == "Too Concentrated",
                                      !(names(best_fit()$sample_se) %in% c("plate_id", "assay_response_variable",
                                                                           "assay_independent_variable", "y_new", "overall_se"))]
                 , file, row.names = FALSE)
     }
   )


    output$download_samples_below_llod <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, "x",
              input$sc_antigen_select, "samples_below_llod.csv", sep = "_")
      },
      content = function(file) {

        req(best_fit())
        req(best_fit())

        # download data component (data frame)
        write.csv(best_fit()$sample_se[best_fit()$sample_se$gate_class_lod == "Too Diluted",
                                       !(names(best_fit()$sample_se) %in% c("plate_id", "assay_response_variable",
                                                                            "assay_independent_variable", "y_new", "overall_se"))]
                  , file, row.names = FALSE)
      }
    )


} # end if

}) # end navigation observer


# ============================================================================
# Batch Processing Button Observer - completely separate from navigation
# ============================================================================

# Reactive value to track if batch processing is currently running
is_batch_processing <- reactiveVal(FALSE)

observeEvent(input$save_scope, {

  label <- switch(
    input$save_scope,
    plate      = "Calculate Standard Curves (Current Plate)",
    experiment = "Calculate Standard Curves (Current Experiment)",
    study      = "Calculate Standard Curves (All Experiments)"
  )

  updateActionButton(
    session,
    "run_batch_fit",
    label = label
  )
})


observeEvent(input$run_batch_fit, ignoreInit = TRUE, {

  scope <- input$save_scope

  # First check: Is processing already running?
  if (is_batch_processing()) {
    showNotification("Batch processing is already running. Please wait for it to complete.",
                    type = "warning", duration = 10, closeButton = TRUE)
    return()
  }

  # Second check: Ensure we're on the right tab and have the required data
  req(input$qc_component == "Standard Curve",
      input$readxMap_study_accession != "Click here",
      input$readxMap_experiment_accession != "Click here",
      input$study_level_tabs == "Experiments",
      input$main_tabs == "view_files_tab")

  # Third check: Validate that study accession is available
  if (is.null(input$readxMap_study_accession) || input$readxMap_study_accession == "") {
    showNotification("Please select a study before running batch processing.",
                    type = "error", duration = 10, closeButton = TRUE)
    return()
  }

  # Set flag to indicate processing has started
  is_batch_processing(TRUE)

  # Use tryCatch to ensure flag is reset even if there's an error
  tryCatch({

    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Fitting standard curves for all experiments."),
                    duration = NULL, closeButton = TRUE)

    # Pull fresh data for the batch processing
    headers <- fetch_db_header_experiments(study_accession = input$readxMap_study_accession, conn = conn)
    #exp_list <- unique(headers$experiment_accession)

    exp_list <- switch(
      scope,
      "study"      = unique(headers$experiment_accession),
      "experiment" = input$readxMap_experiment_accession,
      "plate"      = input$readxMap_experiment_accession
    )

    loaded_data_list <- list()
    for(exp in exp_list) {
      loaded_data_list[[exp]] <- pull_data(study_accession = input$readxMap_study_accession,
                                           experiment_accession = exp,
                                           project_id = userWorkSpaceID(),
                                           conn = conn)
    }

    if (scope == "plate") {
      plate <- input$sc_plate_select
      loaded_data_list <- lapply(loaded_data_list, function(x) {

        if (!is.null(x$plates) && nrow(x$plates) > 0) {
          x$plates <- x$plates[x$plates$plate_nom == plate,, drop = F]
        }
        if (!is.null(x$standards) && nrow(x$standards) > 0) {
          x$standards <- x$standards[x$standards$plate_nom == plate,, drop = F]
        }

        if (!is.null(x$samples) && nrow(x$samples) > 0) {
          x$samples <- x$samples[x$samples$plate_nom == plate,, drop = F]
        }

        if (!is.null(x$blanks) && nrow(x$blanks) > 0) {
          x$blanks <- x$blanks[x$blanks$plate_nom == plate,, drop = F]
        }

        x
      })

    }


    # Get response variable from first experiment's loaded data
    response_var <- loaded_data_list[[exp_list[1]]]$response_var

    # Compute SE table for all antigens across all experiments
    all_standards <- do.call(rbind, lapply(loaded_data_list, function(x) x$standards))

    se_antigen_table <- compute_antigen_se_table(
      standards_data = all_standards,
      response_col = response_var,
      dilution_col = "dilution",
      plate_col = "plate",
      grouping_cols = c("study_accession", "experiment_accession", "source", "antigen"),
      method = "pooled_within",
      verbose = TRUE
    )

    # Get study parameters
    verbose = FALSE
    model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
    param_group = "standard_curve_options"

    study_params <- fetch_study_parameters(study_accession = input$readxMap_study_accession,
                                           param_user = currentuser(),
                                           param_group = param_group,
                                           project_id = userWorkSpaceID(),
                                           conn = conn)

    antigen_list_res <- build_antigen_list(exp_list = exp_list,
                                           loaded_data_list = loaded_data_list,
                                           study_accession = input$readxMap_study_accession)

    cat(paste("after antigen_list_res", exp))
    antigen_plate_list_res <- build_antigen_plate_list(antigen_list_result = antigen_list_res,
                                                       loaded_data_list = loaded_data_list)

    prepped_data_list_res <- prep_plate_data_batch(antigen_plate_list_res = antigen_plate_list_res,
                                                   study_params = study_params,
                                                   verbose = verbose)

    # the list of antigen and plates is truncated to exclude standard data with < 6 points
    antigen_plate_list_res$antigen_plate_list_ids <- prepped_data_list_res$antigen_plate_name_list

    cat(paste("after prepped_data_list_res", exp))
    batch_fit_res <- fit_experiment_plate_batch(prepped_data_list_res = prepped_data_list_res,
                                                antigen_plate_list_res = antigen_plate_list_res,
                                                model_names = model_names,
                                                study_params = study_params,
                                                se_antigen_table = se_antigen_table,
                                                verbose = verbose)

    batch_outputs <- create_batch_fit_outputs(batch_fit_res = batch_fit_res, antigen_plate_list_res)

    # add unique identifiers and rename the response variable to be generic for saving
    batch_outputs_processed <- process_batch_outputs(batch_outputs = batch_outputs,
                                                     response_var = response_var,
                                                     project_id = userWorkSpaceID())
    #batch_outputs_processed_v <<- batch_outputs_processed




    upsert_best_curve(
      conn   = conn,
      df     = batch_outputs_processed$best_glance_all,
      schema = "madi_results",
      table  = "best_glance_all",
      notify = shiny_notify(session)
    )
    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Best Fit Statistics saved"),
                    duration = NULL, closeButton = TRUE)

    # Retrieve lookup of IDs using NK
    study_to_save <- unique(batch_outputs_processed$best_glance_all$study_accession)

    glance_lookup <- DBI::dbGetQuery(
      conn,
      glue::glue("
    SELECT
      best_glance_all_id,
      study_accession,
      experiment_accession,
      plateid,
      plate,
      nominal_sample_dilution,
      source,
      antigen
    FROM madi_results.best_glance_all
    WHERE study_accession = '{study_to_save}';
  ")
    )

    glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)


    # natural key (nk) vector for the glance
    keys <- c("study_accession", "experiment_accession", "plateid",
              "plate", "nominal_sample_dilution", "source", "antigen")


    # attach the best_glance_all_id to the table
    batch_outputs_processed$best_pred_all <-
      dplyr::inner_join(
        batch_outputs_processed$best_pred_all,
        glance_lookup,
        by = keys
      )

    batch_outputs_processed$best_sample_se_all <-
      dplyr::inner_join(
        batch_outputs_processed$best_sample_se_all,
        glance_lookup,
        by = keys
      )

    batch_outputs_processed$best_standard_all <-
      dplyr::inner_join(
        batch_outputs_processed$best_standard_all,
        glance_lookup,
        by = keys
      )


    upsert_best_curve(
      conn   = conn,
      df     = batch_outputs_processed$best_plate_all,
      schema = "madi_results",
      table  = "best_plate_all",
      notify = shiny_notify(session)
    )

    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Best Plates saved"),
                    duration = NULL, closeButton = TRUE)

    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Saving parameter estimates..."),
                    duration = NULL, closeButton = TRUE)

    upsert_best_curve(
      conn   = conn,
      df     = batch_outputs_processed$best_tidy_all,
      schema = "madi_results",
      table  = "best_tidy_all",
      notify = shiny_notify(session)
    )
    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Best parameter estimates saved"),
                    duration = NULL, closeButton = TRUE)


    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Saving predicted standards..."),
                    duration = NULL, closeButton = TRUE)

    upsert_best_curve(
      conn   = conn,
      df     = batch_outputs_processed$best_pred_all,
      schema = "madi_results",
      table  = "best_pred_all",
      notify = shiny_notify(session)
    )


    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Best Predicted standards saved"),
                    duration = NULL, closeButton = TRUE)


    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Saving Predicted samples..."),
                    duration = NULL, closeButton = TRUE)

    upsert_best_curve(
      conn   = conn,
      df     = batch_outputs_processed$best_sample_se_all,
      schema = "madi_results",
      table  = "best_sample_se_all",
      notify = shiny_notify(session)
    )

    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Best Predicted Samples saved"),
                    duration = NULL, closeButton = TRUE)

    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Saving Best Standards..."),
                    duration = NULL, closeButton = TRUE)

    upsert_best_curve(
      conn   = conn,
      df     = batch_outputs_processed$best_standard_all,
      schema = "madi_results",
      table  = "best_standard_all",
      notify = shiny_notify(session)
    )

    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Best Standards saved"),
                    duration = NULL, closeButton = TRUE)



    showNotification(id = "batch_sc_fit_notify",
                    div(class = "big-notification", "Standard Curves Calculated for all Experiments"),
                    duration = NULL, closeButton = TRUE)
    removeNotification("batch_sc_fit_notify")

  }, error = function(e) {
    # Handle errors
    showNotification(
      paste("Error during batch processing:", e$message),
      type = "error",
      duration = NULL,
      closeButton = TRUE
    )
  removeNotification("batch_sc_fit_notify")
  }, finally = {
    # Always reset the processing flag, even if there was an error
    is_batch_processing(FALSE)
  })

})
