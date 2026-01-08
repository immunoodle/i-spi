observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$qc_component,
  input$study_level_tabs,
  input$main_tabs), {

    req(input$qc_component == "Standard Curver",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")


    if (input$qc_component == "Standard Curver") {
      selected_study <- input$readxMap_study_accession
      selected_experiment <- input$readxMap_experiment_accession

      verbose = FALSE
      model_names <<- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
      param_group = "standard_curve_options"

      loaded_data <- pull_data(study_accession = selected_study, experiment_accession = selected_experiment, conn = conn)
      response_var <- loaded_data$response_var
      indep_var <-  loaded_data$indep_var

      se_std_response <- assay_se(loaded_data$standards,
                                  dilution_col = "dilution",
                                  response_col = response_var)

      study_params <<- fetch_study_parameters(study_accession = selected_study,
                                             param_user = currentuser(),
                                             param_group =param_group, conn = conn)



      output$std_curver_ui <- renderUI({
        tagList(
          fluidRow(
            column(3, uiOutput("sc_plate_selector")),
            column(3, uiOutput("sample_dilution_factor_selector")),
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
          # # div(class = "table-container",tableOutput("parameter_estimates")),
          div(class = "table-container",tableOutput("summary_statistics")),
          actionButton("run_batch_fit", "Calculate Standard Curves for all Experiments")
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

 #   }


    output$sc_plate_selector <- renderUI({
      req(loaded_data$standards$study_accession, loaded_data$standards$experiment_accession)

      updateSelectInput(session, "sc_plate_select", selected = NULL)  # Reset the plateSelection
      req(nrow(loaded_data$standards) > 0)

      selectInput("sc_plate_select",
                  label = "Plate",
                  choices = unique(loaded_data$plates$plate))
    })

    output$sc_antigen_selector <- renderUI({
      req(loaded_data$standards$study_accession, loaded_data$standards$experiment_accession)
      updateSelectInput(session, "sc_antigen_select", selected = NULL)

      dat_antigen <- loaded_data$standards[loaded_data$standards$study_accession %in% selected_study &
                                          loaded_data$standards$experiment_accession %in% selected_experiment &
                                            loaded_data$standards$plate %in% input$sc_plate_select, ]

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
          loaded_data$standards$plate %in% input$sc_plate_select &
          loaded_data$standards$antigen %in% input$sc_antigen_select, ]

      req(nrow(dat_source) > 0)

      radioButtons(
        "sc_source_select",
        label = "Source",
        choices = unique(dat_source$source),
        selected = unique(dat_source$source)[1]
      )
    })

    output$sample_dilution_factor_selector <- renderUI({
      req(loaded_data$standards$study_accession, loaded_data$standards$experiment_accession)
      dat_filtered_sample_dil_factor <- loaded_data$standards[loaded_data$standards$study_accession %in% selected_study &
                                             loaded_data$standards$experiment_accession %in% selected_experiment &
                                             loaded_data$standards$plate %in% input$sc_plate_select, ]
      dat_filtered_sample_dil_factor <- dat_filtered_sample_dil_factor[!is.na(dat_filtered_sample_dil_factor$mfi),]

      req(nrow(dat_filtered_sample_dil_factor) > 0)

      sample_dilution_factor_choices <- sort(unique(dat_filtered_sample_dil_factor$sample_dilution_factor))

      selectInput("sc_sample_dilution_factor_select",
                  label = "Sample Dilution Factor",
                  choices = sample_dilution_factor_choices)

    })





    antigen_plate <- reactive({
      req(input$sc_source_select,
          input$sc_antigen_select,
          input$sc_plate_select,
          input$sc_sample_dilution_factor_select,
          loaded_data)


      select_antigen_plate(
        loaded_data = loaded_data,
        study_accession = selected_study,
        experiment_accession = selected_experiment,
        source = input$sc_source_select,
        antigen = input$sc_antigen_select,
        plate = input$sc_plate_select,
        sample_dilution_factor = input$sc_sample_dilution_factor_select,
        antigen_constraints =
          loaded_data$antigen_constraints[
            loaded_data$antigen_constraints$antigen == input$sc_antigen_select,
          ]
      )
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

      pdata_v <<- pdata

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
                             use_patchwork = TRUE)

    })
#
#
    observeEvent(input$show_comparisions, {
      plate <- unique(plot_data()$dat$plate)
      antigen <- unique(plot_data()$dat$antigen)
      title <- paste("Model Comparisons for ", antigen, "on", plate)

      showModal(
        modalDialog(
          title = title,
          size = "l",    # "s", "m", or "l"
          plotOutput("model_comparisions"),
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


      best_fit <- predict_and_propagate_error(best_fit = best_fit,
                                              response_var = response_var,
                                              antigen_plate = antigen_plate(),
                                              study_params = study_params,
                                              verbose = verbose)

      best_fit <- gate_samples(best_fit = best_fit,
                               response_variable = response_var,
                               pcov_threshold = antigen_plate()$antigen_settings$pcov_threshold,
                               verbose = verbose)



      return(best_fit)

    })
#
#     output$print_list <- renderPrint({
#       best_fit()   # your reactive list
#     })
#
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
#

    output$download_best_fit_parameter_estimates  <-  downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, input$sc_sample_dilution_factor_select, "x",
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
       paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, input$sc_sample_dilution_factor_select, "x",
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


    # observeEvent(input$show_samples_above_ulod, {
    #   showModal(
    #     modalDialog(
    #       title = "Sample Values above the Upper Limit of Detection",
    #       size = "xl",
    #       div(class = "table-container",tableOutput("samples_above_ulod")),
    #       easyClose = TRUE,
    #       footer = modalButton("Close")
    #     )
    #   )
    # })
    # output$samples_above_ulod <- renderTable({
    #   req(best_fit())
    #   best_fit()$sample_se[best_fit()$sample_se$gate_class_lod == "Too Concentrated",
    #                        !(names(best_fit()$sample_se) %in% c("plate_id", "assay_response_variable",
    #                                                            "assay_independent_variable", "y_new", "overall_se"))]
    # }, caption = "Sample Values above the Upper Limit of Detection",
    # caption.placement = getOption("xtable.caption.placement", "top"))

    # observeEvent(input$show_samples_below_llod, {
    #   showModal(
    #     modalDialog(
    #       title = "Sample Values below the Lower Limit of Detection",
    #       size = "xl",
    #       div(class = "table-container",tableOutput("samples_below_llod")),
    #       easyClose = TRUE,
    #       footer = modalButton("Close")
    #     )
    #   )
    # })
    #
    # output$samples_below_llod <- renderTable({
    #   req(best_fit())
    #   best_fit()$sample_se[best_fit()$sample_se$gate_class_lod == "Too Diluted",
    #                        !(names(best_fit()$sample_se) %in% c("plate_id", "assay_response_variable",
    #                                                             "assay_independent_variable", "y_new", "overall_se"))]
    # }, caption = "Sample Values below the Lower Limit of Detection",
    # caption.placement = getOption("xtable.caption.placement", "top"))
    #


    output$download_samples_below_llod <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, input$readxMap_experiment_accession, input$sc_plate_select, input$sc_sample_dilution_factor_select, "x",
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


    observeEvent(input$run_batch_fit, {
      showNotification(id = "batch_sc_fit_notify", "Fitting standard curves for all experiments.", duration = NULL)
      response_var <- loaded_data$response_var
      headers <- fetch_db_header_experiments(study_accession = input$readxMap_study_accession, conn = conn)
      exp_list <- unique(headers$experiment_accession) #[1]

      loaded_data_list <- list()
      for(exp in exp_list) {
        loaded_data_list[[exp]] <- pull_data(study_accession = input$readxMap_study_accession, experiment_accession = exp, conn = conn)
      }

      antigen_list_res <- build_antigen_list(exp_list = exp_list, loaded_data_list = loaded_data_list, study_accession = input$readxMap_study_accession)

      cat(paste("after antigen_list_res", exp))
      antigen_plate_list_res <- build_antigen_plate_list(antigen_list_result = antigen_list_res, loaded_data_list = loaded_data_list)

      prepped_data_list_res <- prep_plate_data_batch(antigen_plate_list_res = antigen_plate_list_res, study_params = study_params, verbose = verbose)

      # the list of antigen and plates is truncated to exclude standard data with < 6 points
      antigen_plate_list_res$antigen_plate_list_ids <- prepped_data_list_res$antigen_plate_name_list


      cat(paste("after prepped_data_list_res", exp))
      batch_fit_res <- fit_experiment_plate_batch(prepped_data_list_res = prepped_data_list_res,
                                                  antigen_plate_list_res = antigen_plate_list_res,
                                                  model_names = model_names, study_params = study_params, verbose = verbose)

      # cat(paste("after antigen_list_res", exp))
      batch_outputs <- create_batch_fit_outputs(batch_fit_res = batch_fit_res, antigen_plate_list_res)

      # add unique identifiers and rename the response variable to be generic for saving
     batch_outputs_proccessed <-  process_batch_outputs(batch_outputs = batch_outputs, response_var = response_var)


     # cat("AFTER PROCES sample_se  ", "uid" %in% names(batch_outputs$best_sample_se_all), "\n")
     #
     # cat("AFTER PROCES pred  ", "id_match" %in% names(batch_outputs$best_pred_all), "\n")

      # batch_outputs$best_pred_all <- batch_outputs$best_pred_all %>%
      #   dplyr::group_by(study_accession,
      #                   experiment_accession,
      #                   plateid,
      #                   antigen,
      #                   source) %>%
      #   dplyr::mutate(id_match = dplyr::row_number()) %>%
      #   dplyr::ungroup()
      #
      #
      # batch_outputs$best_sample_se_all <- batch_outputs$best_sample_se_all |>
      #   dplyr::rename(assay_response = all_of(response_var)) |>
      #   dplyr::mutate(uid = seq_len(n()))
      #
      # batch_outputs$best_standard_all <- batch_outputs$best_standard_all |>
      #   dplyr::rename(assay_response = all_of(response_var))
      #
      # batch_outputs <<- batch_outputs

      upsert_best_curve(
        conn   = conn,
        df     = batch_outputs_proccessed$best_plate_all,
        schema = "madi_results",
        table  = "best_plate_all",
        notify = shiny_notify(session)
      )

      showNotification(id = "batch_sc_fit_notify","Best Plates saved", duration = NULL)

      upsert_best_curve(
        conn   = conn,
        df     = batch_outputs_proccessed$best_tidy_all,
        schema = "madi_results",
        table  = "best_tidy_all",
        notify = shiny_notify(session)
      )
      showNotification(id = "batch_sc_fit_notify","Best parameter estimates saved", duration = NULL)

      upsert_best_curve(
        conn   = conn,
        df     = batch_outputs_proccessed$best_glance_all,
        schema = "madi_results",
        table  = "best_glance_all",
        notify = shiny_notify(session)
      )
      showNotification(id = "batch_sc_fit_notify","Best Fit Statistics saved", duration = NULL)

     # batch_outputs$best_pred_all$id_match <- match(batch_outputs$best_pred_all$x, unique(batch_outputs$best_pred_all$x))



      upsert_best_curve(
        conn   = conn,
        df     = batch_outputs_proccessed$best_pred_all,
        schema = "madi_results",
        table  = "best_pred_all",
        notify = shiny_notify(session)
      )

      showNotification(id = "batch_sc_fit_notify","Best Predicted standards saved", duration = NULL)


      upsert_best_curve(
      conn   = conn,
      df     = batch_outputs_proccessed$best_sample_se_all,
      schema = "madi_results",
      table  = "best_sample_se_all",
      notify = shiny_notify(session)
    )

      showNotification(id = "batch_sc_fit_notify","Best Predicted Samples saved", duration = NULL)


      upsert_best_curve(
        conn   = conn,
        df     = batch_outputs_proccessed$best_standard_all,
        schema = "madi_results",
        table  = "best_standard_all",
        notify = shiny_notify(session)


      )

    showNotification(id = "batch_sc_fit_notify","Best Standards saved", duration = NULL)



      showNotification(id = "batch_sc_fit_notify","Standard Curves Calculated for all Experiments", duration = NULL)
      removeNotification("batch_sc_fit_notify")

    })


} # end if

}) # end in the tab



