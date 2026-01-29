observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$qc_component,
  input$study_level_tabs,
  input$main_tabs), {


    req(input$qc_component == "Standard Curve Summary",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")

    if (input$qc_component == "Standard Curve Summary") {
      message("Std Curver Summary")
      selected_study <- input$readxMap_study_accession
      selected_experiment <- input$readxMap_experiment_accession
      param_group <- "standard_curve_options"

      study_params <- fetch_study_parameters(study_accession = selected_study,
                                             param_user = currentuser(),
                                             param_group =param_group, conn = conn)

      best_plate_all <- fetch_best_plate_all(study_accession = selected_study,
                                             experiment_accession = selected_experiment,
                                             project_id = userWorkSpaceID(),
                                             conn = conn)

      # best_standard_all <- fetch_best_standard_all(study_accession = selected_study,
      #                                              experiment_accession = selected_experiment,
      #                                              conn = conn)
      best_standard_all <- fetch_best_standard_all_summary(study_accession = selected_study,
                                                           experiment_accession = selected_experiment,
                                                           param_user = currentuser(),
                                                           project_id = userWorkSpaceID(),
                                                           conn = conn)



      # best_pred_all <- fetch_best_pred_all(study_accession = selected_study,
      #                                      experiment_accession = selected_experiment,
      #                                      conn = conn)
      best_pred_all <- fetch_best_pred_all_summary(study_accession = selected_study,
                                                   experiment_accession = selected_experiment,
                                                   param_user = currentuser(),
                                                   project_id = userWorkSpaceID(),
                                                   conn = conn)



      antigen_families <- fetch_antigen_family_table(selected_study)


      best_pred_all <- attach_antigen_familes(best_pred_all = best_pred_all,
                                              antigen_families = antigen_families)

      best_pred_all_2 <- best_pred_all

      best_glance_all <- fetch_best_glance_all_summary(study_accession = selected_study,
                                                       experiment_accession = selected_experiment,
                                                       param_user = currentuser(),
                                                       project_id = userWorkSpaceID(),
                                                       conn = conn)

      # best_sample_se_all <- fetch_best_sample_se_all(study_accession = selected_study,
      #                                            experiment_accession = selected_experiment,
      #                                            conn = conn)

      best_sample_se_all <- fetch_best_sample_se_all_summary(study_accession = selected_study,
                                                             experiment_accession = selected_experiment,
                                                             param_user = currentuser(),
                                                             project_id = userWorkSpaceID(),
                                                             conn = conn)



      antigen_settings <- fetch_antigen_parameters(
        study_accession = selected_study,
        experiment_accession = selected_experiment,
        project_id = userWorkSpaceID(),
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
            column(9,
                   div(
                     style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
                              padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                     tags$h4("Current Standard Curve Summary Context", style = "margin-top: 0; color: #2c5aa0;"),
                     uiOutput("current_sc_summary_context")
                   )
            )
          ),
          fluidRow(
            column(4, uiOutput("best_std_antigen_family_ui")),
            column(4, uiOutput("best_std_antigen_ui")),
            column(4, uiOutput("best_std_antigen_source_ui"))
          ),
          plotlyOutput("std_curve_summary_plot"),
          uiOutput("download_standard_curve_fits_data_button_ui"),
          uiOutput("save_norm_btn_ui")

        ) # end tagList
      })

      output$current_sc_summary_context <- renderUI({
        best_pred_exp <- best_pred_all[best_pred_all$experiment_accession == selected_experiment,]
        if (nrow(best_pred_exp) > 0) {
          is_log_response <- unique(best_pred_exp$is_log_response)
          is_log_independent <- unique(best_pred_exp$is_log_x)
          blank_option <- unique(best_pred_exp$bkg_method)
          apply_prozone_correction <- unique(best_pred_exp$apply_prozone)

          return(HTML(glue::glue(
            "Showing Standard Curves Fit with: ",
            "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
            "Concentration Scale: {ifelse(is_log_independent, 'log<sub>10</sub>', 'linear')} | ",
            "Blank Handling: {blank_option} | ",
            "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
          )))
        } else {
          current_sc_options <- fetch_current_sc_options_wide(currentuser = currentuser(),
                                                              study_accession = selected_study,
                                                              project_id = userWorkSpaceID(),
                                                              conn = conn)
          is_log_response <- unique(current_sc_options$is_log_mfi_axis)
          #print(is_log_response)
          blank_option <- unique(current_sc_options$blank_option)
          apply_prozone_correction <- unique(current_sc_options$apply_prozone)
          #print(blank_option)
          return(HTML(glue::glue(
            "Standard Curves have not been saved for the current combination of standard curve options selected:\n",
            "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
            "Concentration Scale: waiting for first fit | ",
            "Blank Handling: {blank_option} | ",
            "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
          )))

        }

      })

      output$best_std_antigen_family_ui <- renderUI({
        pred_exp <- best_pred_all[best_pred_all$experiment_accession == selected_experiment,]
        req(nrow(pred_exp) > 0)

        # Get unique antigen families, filtering out NA values
        family_choices <- unique(pred_exp$antigen_family)
        family_choices <- family_choices[!is.na(family_choices) & family_choices != ""]

        # If no valid families found, this shouldn't happen with the updated attach_antigen_familes
        # but add defensive check anyway
        if (length(family_choices) == 0) {
          family_choices <- "All Antigens"
        }

        selectInput("best_std_antigen_family",
                    label = "Antigen Family",
                    choices = family_choices)

      })

      output$best_std_antigen_ui <- renderUI({
        req(input$best_std_antigen_family)

        # Filter by experiment and antigen family
        best_std_antigen_fam <- best_pred_all[best_pred_all$experiment_accession == selected_experiment &
                                                !is.na(best_pred_all$antigen_family) &
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
                                                  !is.na(best_pred_all$antigen_family) &
                                                  best_pred_all$antigen_family == input$best_std_antigen_family  &
                                                  best_pred_all$antigen ==  input$best_std_antigen,]$source)


        req(length(selected_source) > 0)

        radioButtons("best_std_source",
                     label = "Source",
                     choices = selected_source)
      })




      aggregated_fit <- reactive({
        req(best_glance_all)
        req(nrow(best_pred_all) > 0)
        req(input$best_std_antigen)
        req(input$best_std_source)
        selected_study <- input$readxMap_study_accession
        selected_experiment <- input$readxMap_experiment_accession

        result <- aggregate_standard_curves(best_pred_all = best_pred_all,  best_glance_all = best_glance_all,
                                            experiment_accession = selected_experiment,
                                            antigen = input$best_std_antigen,
                                            source = input$best_std_source,
                                            indep_var = "concentration",
                                            response_var = "mfi",
                                            antigen_settings = antigen_settings)

        # Validate that we got data back
        if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
          return(NULL)
        }
        result
      })

      output$std_curve_summary_plot <- renderPlotly({
        agg_fit <- aggregated_fit()

        # Check if aggregated_fit returned valid data
        if (is.null(agg_fit) || (is.data.frame(agg_fit) && nrow(agg_fit) == 0)) {
          # Return an empty plot with a message
          plot_ly() %>%
            layout(
              title = list(
                text = paste("No standard curve data available for",
                             input$best_std_antigen, "from", input$best_std_source,
                             "<br><sup>Please verify that standard curves have been fit with the current study parameters</sup>"),
                font = list(size = 14)
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        } else {
          selected_experiment <- input$readxMap_experiment_accession
          summarize_sc_fits_plotly(best_pred_all = best_pred_all, cv_df = cv_df, aggregated_fit = agg_fit,
                                   best_plate_all = best_plate_all,
                                   experiment_accession = selected_experiment,
                                   antigen = input$best_std_antigen, source = input$best_std_source)
        }
      })


      output$save_norm_btn_ui <- renderUI({
        req(selected_experiment)
        if (nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0) {
          actionButton("save_norm_assay_response", "Save Normalized Assay Response")
        }
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


        tbl_cols <- dbListFields(conn, DBI::Id(schema="madi_results", table="best_sample_se_all"))

        norm_best_sample <- norm_best_sample[, intersect(names(norm_best_sample), tbl_cols)]

        norm_best_sample$best_sample_se_all_id  <- as.numeric(norm_best_sample$best_sample_se_all_id)
        norm_best_sample$best_glance_all_id     <- as.numeric(norm_best_sample$best_glance_all_id)

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




      output$download_standard_curve_fits_data_button_ui <- renderUI({
        req(best_glance_all)
        req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
        req(input$readxMap_study_accession, input$readxMap_experiment_accession)
        button_label <-  paste0("Download Standard Curve Fits Data for ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession)

        downloadButton("download_standard_curve_fits_data", button_label)
      })


      output$download_standard_curve_fits_data <-  downloadHandler(
        filename = function() {
          paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "_fits_data", ".csv", sep = "_")
        },
        content = function(file) {
          req(best_glance_all)
          req(input$readxMap_study_accession, input$readxMap_experiment_accession)

          download_df <- best_glance_all[best_glance_all$experiment_accession == input$readxMap_experiment_accession,]

          # download data component (data frame)
          write.csv(download_df, file, row.names = FALSE)
        }
      )





    } # end inside standard curver summary tab
  }) # end observeEvent



# observeEvent(list(
#   input$readxMap_experiment_accession,
#   input$readxMap_study_accession,
#   input$qc_component,
#   input$study_level_tabs,
#   input$main_tabs), {
#
#
#     req(input$qc_component == "Standard Curve Summary",
#         input$readxMap_study_accession != "Click here",
#         input$readxMap_experiment_accession != "Click here",
#         input$study_level_tabs == "Experiments",
#         input$main_tabs == "view_files_tab")
#
#     if (input$qc_component == "Standard Curve Summary") {
#       message("Std Curver Summary")
#       selected_study <- input$readxMap_study_accession
#       selected_experiment <- input$readxMap_experiment_accession
#       param_group <- "standard_curve_options"
#
#       study_params <- fetch_study_parameters(study_accession = selected_study,
#                                              param_user = currentuser(),
#                                              param_group =param_group, conn = conn)
#
#       best_plate_all <- fetch_best_plate_all(study_accession = selected_study,
#                                              experiment_accession = selected_experiment,
#                                              conn = conn)
#
#       # best_standard_all <- fetch_best_standard_all(study_accession = selected_study,
#       #                                              experiment_accession = selected_experiment,
#       #                                              conn = conn)
#       best_standard_all <- fetch_best_standard_all_summary(study_accession = selected_study,
#                                                            experiment_accession = selected_experiment,
#                                                            param_user = currentuser(),
#                                                            conn = conn)
#
#
#
#       # best_pred_all <- fetch_best_pred_all(study_accession = selected_study,
#       #                                      experiment_accession = selected_experiment,
#       #                                      conn = conn)
#       best_pred_all <- fetch_best_pred_all_summary(study_accession = selected_study,
#                                                    experiment_accession = selected_experiment,
#                                                    param_user = currentuser(),
#                                                    conn = conn)
#
#
#
#       antigen_families <- fetch_antigen_family_table(selected_study)
#
#
#       best_pred_all <- attach_antigen_familes(best_pred_all = best_pred_all,
#                                               antigen_families = antigen_families)
#
#       best_pred_all_2 <- best_pred_all
#
#       best_glance_all <- fetch_best_glance_all_summary(study_accession = selected_study,
#                                                        experiment_accession = selected_experiment,
#                                                        param_user = currentuser(),
#                                                        conn = conn)
#
#       # best_sample_se_all <- fetch_best_sample_se_all(study_accession = selected_study,
#       #                                            experiment_accession = selected_experiment,
#       #                                            conn = conn)
#
#       best_sample_se_all <- fetch_best_sample_se_all_summary(study_accession = selected_study,
#                                                              experiment_accession = selected_experiment,
#                                                              param_user = currentuser(),
#                                                              conn = conn)
#
#
#
#       antigen_settings <- fetch_antigen_parameters(
#         study_accession = selected_study,
#         experiment_accession = selected_experiment,
#         conn = conn
#       )
#
#
#       message("Antigen Settings")
#       print(unique(antigen_settings$study_accession))
#       print(unique(antigen_settings$experiment_accession))
#
#       cv_df <- calculate_cv_dilution_platewise(best_standard = best_standard_all, antigen_settings = antigen_settings)
#       message("after calculate cv_df")
#
#       output$std_curver_summary_ui <- renderUI({
#         tagList(
#           fluidRow(
#             column(9,
#                    div(
#                      style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
#                               padding: 10px; margin-bottom: 15px; border-radius: 5px;",
#                      tags$h4("Current Standard Curve Summary Context", style = "margin-top: 0; color: #2c5aa0;"),
#                      uiOutput("current_sc_summary_context")
#                    )
#             )
#           ),
#           fluidRow(
#             column(4, uiOutput("best_std_antigen_family_ui")),
#             column(4, uiOutput("best_std_antigen_ui")),
#             column(4, uiOutput("best_std_antigen_source_ui"))
#           ),
#           plotlyOutput("std_curve_summary_plot"),
#           uiOutput("download_standard_curve_fits_data_button_ui"),
#           uiOutput("save_norm_btn_ui")
#
#         ) # end tagList
#       })
#
#       output$current_sc_summary_context <- renderUI({
#         best_pred_exp <- best_pred_all[best_pred_all$experiment_accession == selected_experiment,]
#         if (nrow(best_pred_exp) > 0) {
#           is_log_response <- unique(best_pred_exp$is_log_response)
#           is_log_independent <- unique(best_pred_exp$is_log_x)
#           blank_option <- unique(best_pred_exp$bkg_method)
#           apply_prozone_correction <- unique(best_pred_exp$apply_prozone)
#
#           return(HTML(glue::glue(
#             "Showing Standard Curves Fit with: ",
#             "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
#             "Concentration Scale: {ifelse(is_log_independent, 'log<sub>10</sub>', 'linear')} | ",
#             "Blank Handling: {blank_option} | ",
#             "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
#           )))
#         } else {
#           current_sc_options <- fetch_current_sc_options_wide(currentuser = currentuser(),
#                                                               study_accession = selected_study, conn = conn)
#           is_log_response <- unique(current_sc_options$is_log_mfi_axis)
#           #print(is_log_response)
#           blank_option <- unique(current_sc_options$blank_option)
#           apply_prozone_correction <- unique(current_sc_options$apply_prozone)
#           #print(blank_option)
#           return(HTML(glue::glue(
#             "Standard Curves have not been saved for the current combination of standard curve options selected:\n",
#             "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
#             "Concentration Scale: waiting for first fit | ",
#             "Blank Handling: {blank_option} | ",
#             "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
#           )))
#
#         }
#
#       })
#
#       output$best_std_antigen_family_ui <- renderUI({
#         req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
#         selectInput("best_std_antigen_family",
#                     label = "Antigen Family",
#                     choices =   unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]$antigen_family))
#
#       })
#
#       output$best_std_antigen_ui <- renderUI({
#
#         # req(input$best_std_antigen_family)
#         best_std_antigen_fam <- best_pred_all[best_pred_all$experiment_accession == selected_experiment &
#                                                 best_pred_all$antigen_family == input$best_std_antigen_family,]
#         req(nrow(best_std_antigen_fam) > 0)
#
#         antigen_options <- unique(best_std_antigen_fam$antigen)
#
#         my_label <- paste0("Select a Single Antigen in ", input$best_std_antigen_family," for plotting Standard Curves")
#
#         selectInput("best_std_antigen",
#                     label = my_label,
#                     choices = antigen_options )
#       })
#
#       output$best_std_antigen_source_ui <- renderUI({
#         req(input$best_std_antigen_family)
#         req(input$best_std_antigen)
#
#         # best_std_source <- best_standard_all[
#         #   best_standard_all$antigen_family == input$best_std_antigen_family &
#         #     best_standard_all$antigen == input$best_std_antigen, ]
#
#         # req(nrow(best_std_source) > 0)
#
#         selected_source <- unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment &
#                                                   best_pred_all$antigen_family == input$best_std_antigen_family  &
#                                                   best_pred_all$antigen ==  input$best_std_antigen,]$source)
#
#
#         req(length(selected_source) > 0)
#
#         radioButtons("best_std_source",
#                      label = "Source",
#                      choices = selected_source)
#       })
#
#
#
#
#       aggregated_fit <- reactive({
#         req(best_glance_all)
#         req(nrow(best_pred_all) > 0)
#         req(input$best_std_antigen)
#         req(input$best_std_source)
#         selected_study <- input$readxMap_study_accession
#         selected_experiment <- input$readxMap_experiment_accession
#
#         result <- aggregate_standard_curves(best_pred_all = best_pred_all,  best_glance_all = best_glance_all,
#                                             experiment_accession = selected_experiment,
#                                             antigen = input$best_std_antigen,
#                                             source = input$best_std_source,
#                                             indep_var = "concentration",
#                                             response_var = "mfi",
#                                             antigen_settings = antigen_settings)
#
#         # Validate that we got data back
#         if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
#           return(NULL)
#         }
#         result
#       })
#
#       output$std_curve_summary_plot <- renderPlotly({
#         agg_fit <- aggregated_fit()
#
#         # Check if aggregated_fit returned valid data
#         if (is.null(agg_fit) || (is.data.frame(agg_fit) && nrow(agg_fit) == 0)) {
#           # Return an empty plot with a message
#           plot_ly() %>%
#             layout(
#               title = list(
#                 text = paste("No standard curve data available for",
#                              input$best_std_antigen, "from", input$best_std_source,
#                              "<br><sup>Please verify that standard curves have been fit with the current study parameters</sup>"),
#                 font = list(size = 14)
#               ),
#               xaxis = list(visible = FALSE),
#               yaxis = list(visible = FALSE)
#             )
#         } else {
#           selected_experiment <- input$readxMap_experiment_accession
#           summarize_sc_fits_plotly(best_pred_all = best_pred_all, cv_df = cv_df, aggregated_fit = agg_fit,
#                                    best_plate_all = best_plate_all,
#                                    experiment_accession = selected_experiment,
#                                    antigen = input$best_std_antigen, source = input$best_std_source)
#         }
#       })
#
#
#       output$save_norm_btn_ui <- renderUI({
#         req(selected_experiment)
#         if (nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0) {
#           actionButton("save_norm_assay_response", "Save Normalized Assay Response")
#         }
#       })
#
#
#       observeEvent(input$save_norm_assay_response, {
#         cat("pressed save norm_assay_response")
#
#         showNotification(id = "save_norm_assay_response_progress", "Saving Normalized Assay Response for all Antigens.",
#                          duration = NULL)
#
#         req(best_pred_all, best_glance_all, best_sample_se_all)
#
#
#         agg_curves_all_antigens <- compute_aggregated_curves(
#           best_pred_all = best_pred_all,
#           best_glance_all = best_glance_all,
#           experiment_accession = input$readxMap_experiment_accession,
#           antigen_settings = antigen_settings
#         )
#
#
#
#
#         norm_best_sample <- conduct_linear_interpolation_batch(
#           best_sample_se_all = best_sample_se_all,
#           aggregated_fit_v   = agg_curves_all_antigens
#         )
#
#
#         tbl_cols <- dbListFields(conn, DBI::Id(schema="madi_results", table="best_sample_se_all"))
#
#         norm_best_sample <- norm_best_sample[, intersect(names(norm_best_sample), tbl_cols)]
#
#         norm_best_sample$best_sample_se_all_id  <- as.numeric(norm_best_sample$best_sample_se_all_id)
#         norm_best_sample$best_glance_all_id     <- as.numeric(norm_best_sample$best_glance_all_id)
#
#         # Upsert the normalized assay response
#         upsert_best_curve(
#           conn   = conn,
#           df     = norm_best_sample,
#           schema = "madi_results",
#           table  = "best_sample_se_all",
#           notify = shiny_notify(session)
#         )
#         cat("after normalization")
#
#         removeNotification(id = "save_norm_assay_response_progress")
#
#         showNotification("Normalized Assay Response Saved for all Antigens.")
#       })
#
#
#
#
#       output$download_standard_curve_fits_data_button_ui <- renderUI({
#         req(best_glance_all)
#         req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
#         req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#         button_label <-  paste0("Download Standard Curve Fits Data for ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession)
#
#         downloadButton("download_standard_curve_fits_data", button_label)
#       })
#
#
#       output$download_standard_curve_fits_data <-  downloadHandler(
#         filename = function() {
#           paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "_fits_data", ".csv", sep = "_")
#         },
#         content = function(file) {
#           req(best_glance_all)
#           req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#
#           download_df <- best_glance_all[best_glance_all$experiment_accession == input$readxMap_experiment_accession,]
#
#           # download data component (data frame)
#           write.csv(download_df, file, row.names = FALSE)
#         }
#       )
#
#
#
#
#
#     } # end inside standard curver summary tab
#   }) # end observeEvent



# observeEvent(list(
#   input$readxMap_experiment_accession,
#   input$readxMap_study_accession,
#   input$qc_component,
#   input$study_level_tabs,
#   input$main_tabs), {
#
#
#     req(input$qc_component == "Standard Curve Summary",
#         input$readxMap_study_accession != "Click here",
#         input$readxMap_experiment_accession != "Click here",
#         input$study_level_tabs == "Experiments",
#         input$main_tabs == "view_files_tab")
#
#     if (input$qc_component == "Standard Curve Summary") {
#       message("Std Curver Summary")
#       selected_study <- input$readxMap_study_accession
#       selected_experiment <- input$readxMap_experiment_accession
#       param_group <- "standard_curve_options"
#
#       study_params <- fetch_study_parameters(study_accession = selected_study,
#                                               param_user = currentuser(),
#                                               param_group =param_group, conn = conn)
#
#       best_plate_all <- fetch_best_plate_all(study_accession = selected_study,
#                                              experiment_accession = selected_experiment,
#                                              conn = conn)
#
#       # best_standard_all <- fetch_best_standard_all(study_accession = selected_study,
#       #                                              experiment_accession = selected_experiment,
#       #                                              conn = conn)
#       best_standard_all <- fetch_best_standard_all_summary(study_accession = selected_study,
#                                                            experiment_accession = selected_experiment,
#                                                            param_user = currentuser(),
#                                                            conn = conn)
#
#
#
#       # best_pred_all <- fetch_best_pred_all(study_accession = selected_study,
#       #                                      experiment_accession = selected_experiment,
#       #                                      conn = conn)
#       best_pred_all <- fetch_best_pred_all_summary(study_accession = selected_study,
#                                                    experiment_accession = selected_experiment,
#                                                    param_user = currentuser(),
#                                                    conn = conn)
#
#
#
#       antigen_families <- fetch_antigen_family_table(selected_study)
#
#
#       best_pred_all <- attach_antigen_familes(best_pred_all = best_pred_all,
#                                               antigen_families = antigen_families)
#
#       best_pred_all_2 <- best_pred_all
#
#       best_glance_all <- fetch_best_glance_all(study_accession = selected_study,
#                                                experiment_accession = selected_experiment,
#                                                 conn = conn)
#
#       # best_sample_se_all <- fetch_best_sample_se_all(study_accession = selected_study,
#       #                                            experiment_accession = selected_experiment,
#       #                                            conn = conn)
#
#       best_sample_se_all <- fetch_best_sample_se_all_summary(study_accession = selected_study,
#                                        experiment_accession = selected_experiment,
#                                        param_user = currentuser(),
#                                        conn = conn)
#
#
#
#       antigen_settings <- fetch_antigen_parameters(
#         study_accession = selected_study,
#         experiment_accession = selected_experiment,
#         conn = conn
#       )
#
#
#       message("Antigen Settings")
#       print(unique(antigen_settings$study_accession))
#       print(unique(antigen_settings$experiment_accession))
#
#       cv_df <- calculate_cv_dilution_platewise(best_standard = best_standard_all, antigen_settings = antigen_settings)
#       message("after calculate cv_df")
#
#       output$std_curver_summary_ui <- renderUI({
#         tagList(
#           fluidRow(
#             column(9,
#                      div(
#                        style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
#                               padding: 10px; margin-bottom: 15px; border-radius: 5px;",
#                        tags$h4("Current Standard Curve Summary Context", style = "margin-top: 0; color: #2c5aa0;"),
#                        uiOutput("current_sc_summary_context")
#                      )
#             )
#           ),
#           fluidRow(
#             column(4, uiOutput("best_std_antigen_family_ui")),
#             column(4, uiOutput("best_std_antigen_ui")),
#             column(4, uiOutput("best_std_antigen_source_ui"))
#           ),
#           plotlyOutput("std_curve_summary_plot"),
#           uiOutput("download_standard_curve_fits_data_button_ui"),
#           uiOutput("save_norm_btn_ui")
#
#         ) # end tagList
#       })
#
#       output$current_sc_summary_context <- renderUI({
#         best_pred_exp <- best_pred_all[best_pred_all$experiment_accession == selected_experiment,]
#         if (nrow(best_pred_exp) > 0) {
#             is_log_response <- unique(best_pred_exp$is_log_response)
#             is_log_independent <- unique(best_pred_exp$is_log_x)
#             blank_option <- unique(best_pred_exp$bkg_method)
#             apply_prozone_correction <- unique(best_pred_exp$apply_prozone)
#
#             return(HTML(glue::glue(
#               "Showing Standard Curves Fit with: ",
#               "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
#               "Concentration Scale: {ifelse(is_log_independent, 'log<sub>10</sub>', 'linear')} | ",
#               "Blank Handling: {blank_option} | ",
#               "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
#             )))
#         } else {
#           current_sc_options <- fetch_current_sc_options_wide(currentuser = currentuser(),
#                                                                study_accession = selected_study, conn = conn)
#           is_log_response <- unique(current_sc_options$is_log_mfi_axis)
#           #print(is_log_response)
#           blank_option <- unique(current_sc_options$blank_option)
#           apply_prozone_correction <- unique(current_sc_options$apply_prozone)
#           #print(blank_option)
#           return(HTML(glue::glue(
#             "Standard Curves have not been saved for the current combination of standard curve options selected:\n",
#             "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
#             "Concentration Scale: waiting for first fit | ",
#             "Blank Handling: {blank_option} | ",
#             "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
#           )))
#
#         }
#
#       })
#
#       output$best_std_antigen_family_ui <- renderUI({
#         req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
#         selectInput("best_std_antigen_family",
#                     label = "Antigen Family",
#                     choices =   unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]$antigen_family))
#
#       })
#
#       output$best_std_antigen_ui <- renderUI({
#
#        # req(input$best_std_antigen_family)
#         best_std_antigen_fam <- best_pred_all[best_pred_all$experiment_accession == selected_experiment &
#                                                 best_pred_all$antigen_family == input$best_std_antigen_family,]
#         req(nrow(best_std_antigen_fam) > 0)
#
#         antigen_options <- unique(best_std_antigen_fam$antigen)
#
#         my_label <- paste0("Select a Single Antigen in ", input$best_std_antigen_family," for plotting Standard Curves")
#
#         selectInput("best_std_antigen",
#                     label = my_label,
#                     choices = antigen_options )
#       })
#
#       output$best_std_antigen_source_ui <- renderUI({
#         req(input$best_std_antigen_family)
#         req(input$best_std_antigen)
#
#         # best_std_source <- best_standard_all[
#         #   best_standard_all$antigen_family == input$best_std_antigen_family &
#         #     best_standard_all$antigen == input$best_std_antigen, ]
#
#         # req(nrow(best_std_source) > 0)
#
#         selected_source <- unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment &
#                                                  best_pred_all$antigen_family == input$best_std_antigen_family  &
#                                                  best_pred_all$antigen ==  input$best_std_antigen,]$source)
#
#
#         req(length(selected_source) > 0)
#
#         radioButtons("best_std_source",
#                      label = "Source",
#                      choices = selected_source)
#       })
#
#
#
#
#       aggregated_fit <- reactive({
#         req(best_glance_all)
#         req(nrow(best_pred_all) > 0)
#         selected_study <- input$readxMap_study_accession
#         selected_experiment <- input$readxMap_experiment_accession
#
#         aggregate_standard_curves(best_pred_all = best_pred_all,  best_glance_all = best_glance_all,
#                                   experiment_accession = selected_experiment,
#                                   antigen = input$best_std_antigen,
#                                   source = input$best_std_source,
#                                   indep_var = "concentration",
#                                   response_var = "mfi",
#                                   antigen_settings = antigen_settings)
#
#         })
#
#       output$std_curve_summary_plot <- renderPlotly({
#         req(aggregated_fit)
#         selected_experiment <- input$readxMap_experiment_accession
#         aggregated_fit<- aggregated_fit()
#         #aggregated_fit_v <<- aggregated_fit
#         summarize_sc_fits_plotly(best_pred_all = best_pred_all, cv_df = cv_df, aggregated_fit = aggregated_fit(),
#                                   best_plate_all = best_plate_all,
#                                   experiment_accession = selected_experiment,
#                                   antigen =  input$best_std_antigen, source =  input$best_std_source)
#
#       })
#
#
#       output$save_norm_btn_ui <- renderUI({
#         req(selected_experiment)
#         if (nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0) {
#           actionButton("save_norm_assay_response", "Save Normalized Assay Response")
#         }
#       })
#
#
#       observeEvent(input$save_norm_assay_response, {
#         cat("pressed save norm_assay_response")
#
#         showNotification(id = "save_norm_assay_response_progress", "Saving Normalized Assay Response for all Antigens.",
#                          duration = NULL)
#
#         req(best_pred_all, best_glance_all, best_sample_se_all)
#
#
#       agg_curves_all_antigens <- compute_aggregated_curves(
#             best_pred_all = best_pred_all,
#             best_glance_all = best_glance_all,
#             experiment_accession = input$readxMap_experiment_accession,
#             antigen_settings = antigen_settings
#           )
#
#
#
#
#       norm_best_sample <- conduct_linear_interpolation_batch(
#         best_sample_se_all = best_sample_se_all,
#         aggregated_fit_v   = agg_curves_all_antigens
#       )
#
#
#       tbl_cols <- dbListFields(conn, DBI::Id(schema="madi_results", table="best_sample_se_all"))
#
#       norm_best_sample <- norm_best_sample[, intersect(names(norm_best_sample), tbl_cols)]
#
#       norm_best_sample$best_sample_se_all_id  <- as.numeric(norm_best_sample$best_sample_se_all_id)
#       norm_best_sample$best_glance_all_id     <- as.numeric(norm_best_sample$best_glance_all_id)
#
#       # Upsert the normalized assay response
#       upsert_best_curve(
#         conn   = conn,
#         df     = norm_best_sample,
#         schema = "madi_results",
#         table  = "best_sample_se_all",
#         notify = shiny_notify(session)
#       )
#       cat("after normalization")
#
#       removeNotification(id = "save_norm_assay_response_progress")
#
#       showNotification("Normalized Assay Response Saved for all Antigens.")
#       })
#
#
#
#
#       output$download_standard_curve_fits_data_button_ui <- renderUI({
#         req(best_glance_all)
#         req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
#         req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#         button_label <-  paste0("Download Standard Curve Fits Data for ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession)
#
#         downloadButton("download_standard_curve_fits_data", button_label)
#       })
#
#
#       output$download_standard_curve_fits_data <-  downloadHandler(
#         filename = function() {
#           paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "_fits_data", ".csv", sep = "_")
#         },
#         content = function(file) {
#           req(best_glance_all)
#           req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#
#           download_df <- best_glance_all[best_glance_all$experiment_accession == input$readxMap_experiment_accession,]
#
#           # download data component (data frame)
#           write.csv(download_df, file, row.names = FALSE)
#         }
#       )
#
#
#
#
#
#     } # end inside standard curver summary tab
#   }) # end observeEvent
