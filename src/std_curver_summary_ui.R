
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

      best_glance_all <- fetch_best_glance_all(study_accession = selected_study,
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
          plotlyOutput("std_curve_summary_plot")

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
        summarize_sc_fits_plotly(best_pred_all = best_pred_all, cv_df = cv_df, aggregated_fit = aggregated_fit(),
                                  best_plate_all = best_plate_all,
                                  study_params = study_params, experiment_accession = selected_experiment,
                                  antigen =  input$best_std_antigen, source =  input$best_std_source)

      })



      # utput$antigenSelectionUI2 <- renderUI({
      #   req(fitted_curve_parameters)
      #   req(input$readxMap_study_accession, input$readxMap_experiment_accession)
      #   req(fitted_curve_parameters$study_accession, fitted_curve_parameters$experiment_accession)
      #   # require the antigen family
      #   req(input$antigenFamilySelection)
      #
      #   updateSelectInput(session, "antigenSelection2", selected = NULL)
      #
      #   dat_antigen <- fitted_curve_parameters[fitted_curve_parameters$study_accession %in% input$readxMap_study_accession &
      #                                            fitted_curve_parameters$experiment_accession %in% input$readxMap_experiment_accession &
      #                                            fitted_curve_parameters$antigen_family %in% input$antigenFamilySelection, ]
      #   req(nrow(dat_antigen) > 0)
      #
      #   my_label <- paste0("Select a Single Antigen in ", input$antigenFamilySelection," for plotting Standard Curves")
      #   selectInput("antigenSelection2",
      #               label = my_label,
      #               choices = unique(dat_antigen$antigen)) #unique(dat_antigen$antigen)
      #   #}
      # })




    } # end inside standard curver summary tab
  }) # end observeEvent
