
observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$qc_component,
  input$study_level_tabs,
  input$main_tabs), {


    req(input$qc_component == "Standard Curver Summary",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")

    if (input$qc_component == "Standard Curver Summary") {
      message("Std Curver Summary")
      selected_study <- input$readxMap_study_accession
      selected_experiment <- input$readxMap_experiment_accession
      param_group <- "standard_curve_options"

      study_params <- fetch_study_parameters(study_accession = selected_study,
                                              param_user = currentuser(),
                                              param_group =param_group, conn = conn)

      best_plate_all <- fetch_best_plate_all(study_accession = selected_study,
                                             experiment_accession = selected_experiment,
                                             conn = conn)

      best_standard_all <- fetch_best_standard_all(study_accession = selected_study,
                                                   experiment_accession = selected_experiment,
                                                   conn = conn)



      best_pred_all <- fetch_best_pred_all(study_accession = selected_study,
                                           experiment_accession = selected_experiment,
                                           conn = conn)


      antigen_families <- fetch_antigen_family_table(selected_study)


      best_pred_all <- attach_antigen_familes(best_pred_all = best_pred_all,
                                              antigen_families = antigen_families)

      best_pred_all_2 <- best_pred_all

      best_glance_all <- fetch_best_glance_all(study_accession = selected_study,
                                               experiment_accession = selected_experiment,
                                                conn = conn)

      best_sample_se_all <- fetch_best_sample_se_all(study_accession = selected_study,
                                                 experiment_accession = selected_experiment,
                                                 conn = conn)


      antigen_settings <- fetch_antigen_parameters(
        study_accession = selected_study,
        experiment_accession = selected_experiment,
        conn = conn
      )


      message("Antigen Settings")
      print(unique(antigen_settings$study_accession))
      print(unique(antigen_settings$experiment_accession))

      cv_df <- calculate_cv_dilution_platewise(best_standard = best_standard_all, antigen_settings = antigen_settings)
      message("after calculate cv_df")

      output$std_curver_summary_ui <- renderUI({
        tagList(
          fluidRow(
            column(4, uiOutput("best_std_antigen_family_ui")),
            column(4, uiOutput("best_std_antigen_ui")),
            column(4, uiOutput("best_std_antigen_source_ui"))
          ),
          plotlyOutput("std_curve_summary_plot"),
          actionButton("save_norm_assay_response", "Save Normalized Assay Response")

        ) # end tagList
      })


      output$best_std_antigen_family_ui <- renderUI({

        # req(best_standard_all$antigen_family)

        selectInput("best_std_antigen_family",
                    label = "Antigen Family",
                    choices =   unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]$antigen_family))

      })

      output$best_std_antigen_ui <- renderUI({

       # req(input$best_std_antigen_family)
        best_std_antigen_fam <- best_pred_all[best_pred_all$experiment_accession == selected_experiment &
                                                best_pred_all$antigen_family == input$best_std_antigen_family,]
        req(nrow(best_std_antigen_fam) > 0)

        antigen_options <- unique(best_std_antigen_fam$antigen)

        my_label <- paste0("Select a Single Antigen in ", input$best_std_antigen_family," for plotting Standard Curves")

        selectInput("best_std_antigen",
                    label = my_label,
                    choices = antigen_options )
      })

      output$best_std_antigen_source_ui <- renderUI({
        req(input$best_std_antigen_family)
        req(input$best_std_antigen)

        # best_std_source <- best_standard_all[
        #   best_standard_all$antigen_family == input$best_std_antigen_family &
        #     best_standard_all$antigen == input$best_std_antigen, ]

        # req(nrow(best_std_source) > 0)

        selected_source <- unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment &
                                                 best_pred_all$antigen_family == input$best_std_antigen_family  &
                                                 best_pred_all$antigen ==  input$best_std_antigen,]$source)


        req(length(selected_source) > 0)

        radioButtons("best_std_source",
                     label = "Source",
                     choices = selected_source)
      })




      aggregated_fit <- reactive({
        req(best_glance_all)
        req(best_pred_all)
        selected_study <- input$readxMap_study_accession
        selected_experiment <- input$readxMap_experiment_accession

        aggregate_standard_curves(best_pred_all = best_pred_all,  best_glance_all = best_glance_all,
                                  experiment_accession = selected_experiment,
                                  antigen = input$best_std_antigen,
                                  source = input$best_std_source,
                                  indep_var = "concentration",
                                  response_var = "mfi",
                                  antigen_settings = antigen_settings)

        })

      output$std_curve_summary_plot <- renderPlotly({
        selected_experiment <- input$readxMap_experiment_accession
        aggregated_fit<- aggregated_fit()
        #aggregated_fit_v <<- aggregated_fit
        summarize_sc_fits_plotly(best_pred_all = best_pred_all, cv_df = cv_df, aggregated_fit = aggregated_fit(),
                                  best_plate_all = best_plate_all,
                                  study_params = study_params, experiment_accession = selected_experiment,
                                  antigen =  input$best_std_antigen, source =  input$best_std_source)

      })



      observeEvent(input$save_norm_assay_response, {
        cat("pressed save norm_assay_response")

        showNotification(id = "save_norm_assay_response_progress", "Saving Normalized Assay Response for all Antigens.",
                         duration = NULL)

        req(best_pred_all, best_glance_all, best_sample_se_all)


      agg_curves_all_antigens <- compute_aggregated_curves(
            best_pred_all = best_pred_all,
            best_glance_all = best_glance_all,
            experiment_accession = input$readxMap_experiment_accession,
            antigen_settings = antigen_settings
          )


      norm_best_sample <- conduct_linear_interpolation_batch(
        best_sample_se_all = best_sample_se_all,
        aggregated_fit_v   = agg_curves_all_antigens
      )

      # Upsert the normalized assay response
      upsert_best_curve(
        conn   = conn,
        df     = norm_best_sample,
        schema = "madi_results",
        table  = "best_sample_se_all",
        notify = shiny_notify(session)
      )
      cat("after normalization")

      removeNotification(id = "save_norm_assay_response_progress")

      showNotification("Normalized Assay Response Saved for all Antigens.")
      })






    } # end inside standard curver summary tab
  }) # end observeEvent
