standard_curve_table_list <- list()

data_summary <- list()

#std_curve_tab_active <- reactiveVal(FALSE)
# print("STARTING summary module source")
# standardCurveSummaryModuleUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fluidRow(
#       column(12,
#              bsCollapse(
#                id = ns("std_curve_summaryCollapse"),
#                bsCollapsePanel(
#                  title = "Standard Summary Methods",
#                  tagList(
#                    tags$p("Select an antigen family of interest using the dropdown menu titled 'Antigen Family'. To modify the family of any antigen, navigate to the 'Antigen Family Tab'. If no fitted standard curve models are available for the selected study, experiment and antigen family, the message 'There are no Standard Curve models saved by the algorithm in the Standard Curve Tab' will be displayed.
#                If multiple sources are available, use the radio buttons to select the desired source."),
#                    tags$p("If there are standard curve fits saved to the database, a dropdown menu under the antigen family allows for one antigen from that family to be selected.
#                       When an antigen is selected, all of the standard curves for the selected antigen are plotted on the same figure with the coefficient of variation at each dilution factor.
#                       Additionally, an aggregated standard curve is calculated and displayed. The standard curves are labeled by plate and model class.
#                       In addition, the linetype of the standard curve indicates the class of model.
#                       The correspondence between model class and the linetype is the following:"),
#                    tags$ul(tags$li("drda five parameter logistic model: solid"),
#                            tags$li("nls exponential model: dash"),
#                            tags$li("nls five parameter logistic model: dot"),
#                            tags$li("nls four parameter logistic model: dashdot")),
#                    tags$p("Below the figure or the message that is displayed when there are no standard curve models retrieved by the standard curve algorithm,
#                       there is a button which when clicked downloads a spreadsheet containing the standard curve fits from the database for the current experiment selected in the current study.
#                        The second button below that downloads a spreadsheet containing the sample data from the database for the current experiment selected in the current study."),
#                    tags$p("Following the download buttons for the standard curve model fits and the sample data there is a select box to select one or more antigens from the currently selected antigen family.
#                       This selection generates a figure displaying the coefficient of variation as a percentage, plotted against the log of the dilution factor for the chosen antigens."),
#                    tags$p("Beneath the figure of the coefficient of variation by the log of the dilution factor for the antigens selected, an additional download button is available.
#                       When pressed, this download button downloads the coefficient of variation by each log dilution factor for the current experiment in the selected study as an excel file.
#                       In addition to the  coefficient of variation, the mean log dilution and standard deviation of log dilution are found as columns in the spreadsheet since they are required for the coefficient of variation calculation.")
#                  ), # end tagList
#                  style = "success"
#                )
#              ) #end bsCollapse
#              ,
#              mainPanel(
#                # Family Antigens
#                fluidRow(
#                  column(4 ,uiOutput(ns("antigenFamilySelectionUI"))),
#                  column(4, uiOutput(ns("antigenSelectionUI2")))
#                  #column(4, uiOutput(ns("sourceSelectionUI2")))
#                ) #,
#
#
#                # uiOutput(ns("infoSavedMesssage")),
#                # br(),
#                #
#                # uiOutput(ns("standard_curve_antigenContainer")),
#                # br(),
#                # uiOutput(ns("download_button_ui")),
#                # br(),
#                # uiOutput(ns("download_sample_data_ui")),
#                # br(),
#                # uiOutput(ns("save_norm_mfiUI")),
#                # br(),
#                # # CV Plot Controls
#                # br(),
#                # uiOutput(ns("antigens_in_family_UI")),
#                # br(),
#                # plotlyOutput(ns("cv_log_dilution_plot"), width = "75vw"),
#                # br(),
#                # uiOutput(ns("download_cv_log_dilution_study_level"))
#              ) # end mainPanel
#       ) # end col
#
#     ) # end fluidRow
#   ) # end tagList
# }
#
# standardCurveSummaryModuleServer <- function(id, selected_study, selected_experiment, currentuser) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     sample_data_sc_summary <- fetch_db_samples(study_accession = selected_study(), experiment_accession = selected_experiment())
#     standard_data_curve_sc_summary <- fetch_db_standards(study_accession = selected_study(), experiment_accession = selected_experiment())
#
#     if (!is.null(selected_study()) && length(selected_study()) > 0 &&
#         !is.null(selected_experiment()) && length(selected_experiment()) > 0 &&
#         !is.null(sample_data_sc_summary) && length(sample_data_sc_summary > 0)){
#
#       # Filter sample data
#       sample_data_sc_summary$selected_str <- paste0(sample_data_sc_summary$study_accession, sample_data_sc_summary$experiment_accession)
#       sample_data_sc_summary <- sample_data_sc_summary[sample_data_sc_summary$selected_str == paste0(selected_study(), selected_experiment()), ]
#
#       # Summarize sample data
#       cat("Viewing sample data fitting summary module")
#       print(names(sample_data_sc_summary))
#       print(table(sample_data_sc_summary$plateid))
#       print(table(sample_data_sc_summary$antigen))
#       cat("After summarizing sample data fitting summary module")
#
#
#       # Rename columns
#
#       sample_data_sc_summary <- dplyr::rename(sample_data_sc_summary, arm_name = agroup)
#       sample_data_sc_summary <- dplyr::rename(sample_data_sc_summary, visit_name = timeperiod)
#
#
#       sample_data_sc_summary$subject_accession <- sample_data_sc_summary$patientid
#
#       sample_data_sc_summary <- dplyr::rename(sample_data_sc_summary, value_reported = antibody_mfi)
#
#       arm_choices <- unique(sample_data_sc_summary$arm_name)
#       visits <- unique(sample_data_sc_summary$visit_name)
#     }
#
#       antigen_families <- fetch_antigen_family_table(selected_study())
#
#       if (!is.null(selected_study()) && length(selected_study()) > 0 &&
#           !is.null(selected_experiment()) && length(selected_experiment()) > 0 &&
#           !is.null(standard_data_curve_sc_summary) && length(standard_data_curve_sc_summary) > 0){
#
#         # Filter sample data
#         standard_data_curve_sc_summary$selected_str <- paste0(standard_data_curve_sc_summary$study_accession, standard_data_curve_sc_summary$experiment_accession)
#         standard_data_curve_sc_summary <- standard_data_curve_sc_summary[standard_data_curve_sc_summary$selected_str == paste0(selected_study(), selected_experiment()), ]
#
#         # Summarize std curve data data
#         cat("View Standard Curve data plateid")
#         print(table(standard_data_curve_sc_summary$plateid))
#         cat("View Standard Curve data antigen")
#         print(table(standard_data_curve_sc_summary$antigen))
#
#         std_curve_data_sc_summary <- standard_data_curve_sc_summary
#
#         std_curve_data_sc_summary$subject_accession <- std_curve_data_sc_summary$patientid
#
#         std_curve_data_sc_summary <- calculate_log_dilution(std_curve_data_sc_summary)
#         std_curve_data_sc_summary <- assign_antigen_families(standard_curve_study_data = std_curve_data_sc_summary, antigen_family_lookup = antigen_families )
#       }
#
#       ## Load study configuration for the user
#       study_configuration <- fetch_study_configuration(study_accession = selected_study(), user = currentuser())
#       user_bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
#       is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#
#       fitted_curve_parameters <- fetch_standard_curves(selected_study(), selected_experiment(), bkg_method = user_bkg_method, is_log_mfi_axis = is_log_mfi)
#       fitted_curve_parameters <- assign_antigen_families(standard_curve_study_data = fitted_curve_parameters, antigen_family_lookup = antigen_families)
#
#       fitted_curve_feature <- fetch_standard_curves_mse_feature(selected_study(), bkg_method = user_bkg_method)
#       fitted_curve_feature <- assign_antigen_families(standard_curve_study_data = fitted_curve_feature, antigen_family_lookup = antigen_families)
#
#       ##Sample data to download
#       sample_data_feature_download <- fetch_sample_data_feature(study_accession = selected_study(), experiment_accession = selected_experiment())
#
#       std_curve_data_sc_summary <- std_curve_data_sc_summary
#
#       if (nrow(fitted_curve_parameters) > 0) {
#
#         output$antigenFamilySelectionUI <- renderUI({
#
#           req(std_curve_data_sc_summary$antigen_family)
#
#           selectInput(ns("antigenFamilySelection"),
#                       label = "Antigen Family",
#                       choices = unique(std_curve_data_sc_summary$antigen_family))
#
#         })
#         output$antigenSelectionUI2 <- renderUI({
#           req(fitted_curve_parameters)
#          # req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#           req(fitted_curve_parameters$study_accession, fitted_curve_parameters$experiment_accession)
#           # require the antigen family
#           req(input$antigenFamilySelection)
#
#           updateSelectInput(session, ns("antigenSelection2"), selected = NULL)
#
#           dat_antigen <- fitted_curve_parameters[fitted_curve_parameters$study_accession %in% selected_study() &
#                                                    fitted_curve_parameters$experiment_accession %in% selected_experiment() &
#                                                    fitted_curve_parameters$antigen_family %in% input$antigenFamilySelection, ]
#           req(nrow(dat_antigen) > 0)
#
#           my_label <- paste0("Select a Single Antigen in ", input$antigenFamilySelection," for plotting Standard Curves")
#           selectInput(ns("antigenSelection2"),
#                       label = my_label,
#                       choices = unique(dat_antigen$antigen)) #unique(dat_antigen$antigen)
#           #}
#         })
#
#       } else {
#         output$standardCurveSummaryUI <- renderUI({
#             br()
#             uiOutput(ns("nofits"))
#         })
#            output$nofits <- renderUI({
#             HTML(paste0("<span style='font-size:20px;'>There are no standard curves saved by the algorithm in the standard curve Fitting Panel for
#                           ", selected_experiment()," in ", selected_study(),
#                         " using the selected method for the blanks.</span>"))
#
#           })
#       } # end else
#   }) # end moduleServer
# }
#
# cat("typeof:", typeof(standardCurveSummaryModuleServer), "\n")
# cat("class:", class(standardCurveSummaryModuleServer), "\n")
# cat("Function:")
# print(is.function(standardCurveSummaryModuleServer))  # Should return TRUE
#
# # # --- Destroyable wrappers ---
# destroyableStandardCurveSummaryModuleUI <- makeModuleUIDestroyable(standardCurveSummaryModuleUI)
# print("Calling makeModuleServerDestroyable")
# print(standardCurveSummaryModuleServer)
# destroyableStandardCurveSummaryModuleServer <- makeModuleServerDestroyable(standardCurveSummaryModuleServer)
# #


observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$qc_component,
  input$study_level_tabs,
  input$main_tabs), {

# observeEvent(input$inLoadedData, {
    req(input$qc_component == "Standard Curve Summary",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")

  if (input$qc_component == "Standard Curve Summary") {

    selected_study <- selected_studyexpplate$study_accession
    selected_experiment <- selected_studyexpplate$experiment_accession

    cat("Antigen family df ")
    #obtain it here in this tab also
    # antigen_families_rv(NULL)
    # antigen_families_rv(fetch_antigen_family_table(selected_study))
    # print(head(antigen_families_rv()))

    # Load sample data
    sample_data <- stored_plates_data$stored_sample
    # Check if selected study, experiment, and sample data are available
    if (!is.null(selected_study) && length(selected_study) > 0 &&
        !is.null(selected_experiment) && length(selected_experiment) > 0 &&
        !is.null(sample_data) && length(sample_data) > 0){

      # Filter sample data
      sample_data$selected_str <- paste0(sample_data$study_accession, sample_data$experiment_accession)
      sample_data <- sample_data[sample_data$selected_str == paste0(selected_study, selected_experiment), ]

      # Summarize sample data
      cat("Viewing sample dat in summary tab ")
      print(names(sample_data))
      print(table(sample_data$plateid))
      print(table(sample_data$antigen))
      cat("After summarizing sample data in summary tab")


      # Rename columns

      sample_data <- dplyr::rename(sample_data, arm_name = agroup)
      sample_data <- dplyr::rename(sample_data, visit_name = timeperiod)


      sample_data$subject_accession <- sample_data$patientid

      sample_data <- dplyr::rename(sample_data, value_reported = mfi)

      arm_choices <- unique(sample_data$arm_name)
      visits <- unique(sample_data$visit_name)

    }

    antigen_families <- fetch_antigen_family_table(selected_study)
    standard_data_curve <- stored_plates_data$stored_standard

    if (!is.null(selected_study) && length(selected_study) > 0 &&
        !is.null(selected_experiment) && length(selected_experiment) > 0 &&
        !is.null(standard_data_curve) && length(standard_data_curve) > 0){

      # Filter sample data
      standard_data_curve$selected_str <- paste0(standard_data_curve$study_accession, standard_data_curve$experiment_accession)
      standard_data_curve <- standard_data_curve[standard_data_curve$selected_str == paste0(selected_study, selected_experiment), ]

      # Summarize std curve data data
      cat("View Standard Curve data plateid")
      print(table(standard_data_curve$plateid))
      cat("View Standard Curve data antigen")
      print(table(standard_data_curve$antigen))

      std_curve_data <- standard_data_curve

      std_curve_data$subject_accession <- std_curve_data$patientid

      std_curve_data <- calculate_log_dilution(std_curve_data)
      std_curve_data <- assign_antigen_families(standard_curve_study_data = std_curve_data, antigen_family_lookup = antigen_families )
    }

    ## Load study configuration for the user
    study_configuration <- fetch_study_configuration(study_accession = selected_study , user = currentuser())
    user_bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
    is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))

    fitted_curve_parameters <- fetch_standard_curves(selected_study, selected_experiment, bkg_method = user_bkg_method, is_log_mfi_axis = is_log_mfi)
    fitted_curve_parameters <- assign_antigen_families(standard_curve_study_data = fitted_curve_parameters, antigen_family_lookup = antigen_families)
    #  cat("Fitted Curve Parameters")


    fitted_curve_feature <- fetch_standard_curves_mse_feature(selected_study, bkg_method = user_bkg_method)
    fitted_curve_feature <- assign_antigen_families(standard_curve_study_data = fitted_curve_feature, antigen_family_lookup = antigen_families)
    #experiment_data <- experiment_data()

    ##Sample data to download
    sample_data_feature_download <- fetch_sample_data_feature(study_accession = selected_study, experiment_accession = selected_experiment )

    # Determine if standard curve data is available for study and experiment
    # is_standard_avaliable <- checkStandardCurves(standard_data_curve, selected_study, selected_experiment)
    # cat("Standard Curve avaliablilty")
    # print(is_standard_avaliable)

    std_curve_data <- std_curve_data
    # # Log transform MFI if user configurations has it selected.
    # if (is_log_mfi) {
    #   std_curve_data$mfi <- log10(std_curve_data$mfi)
    # }


    #std_curve_data <<- dplyr::rename(std_curve_data, value_reported = mfi)
    # std_curve_data$antibody_mfi <- std_curve_data$value_reported
    # std_curve_data$mfi <- std_curve_data$value_reported
    # data$antibody_mfi <- data$value_reported
    # data$mfi <- data$value_reported
    #arm_choices <- unique(data$arm_name)
    #visits <- unique(data$visit_name)
    if (nrow(fitted_curve_parameters) > 0) {
      # experiment_data <<- experiment_data
      # This must be present too in database.
      output$standardCurveSummaryUI <- renderUI({
        req(study_configuration)
        tagList(
          fluidRow(
            column(12,
            #br(),
            #   sidebarPanel(
            #     uiOutput("studySelectionUI2"),
            #     uiOutput("expirementSelectionUI2"),
            #     uiOutput("antigenSelectionUI2"),
            #     uiOutput("sourceSelectionUI2")
            #    # uiOutput("show_model_classUI")
            #
            # ), # end sidebarPanel
            bsCollapse(
              id = "std_curve_summaryCollapse",
              bsCollapsePanel(
                title = "Standard Summary Methods",
              tagList(
                tags$p("Select an antigen family of interest using the dropdown menu titled 'Antigen Family'. To modify the family of any antigen, navigate to the 'Antigen Family Tab'. If no fitted standard curve models are available for the selected study, experiment and antigen family, the message 'There are no Standard Curve models saved by the algorithm in the Standard Curve Tab' will be displayed.
               If multiple sources are available, use the radio buttons to select the desired source."),
                tags$p("If there are standard curve fits saved to the database, a dropdown menu under the antigen family allows for one antigen from that family to be selected.
                      When an antigen is selected, all of the standard curves for the selected antigen are plotted on the same figure with the coefficient of variation at each dilution factor.
                      Additionally, an aggregated standard curve is calculated and displayed. The standard curves are labeled by plate and model class.
                      In addition, the linetype of the standard curve indicates the class of model.
                      The correspondence between model class and the linetype is the following:"),
                tags$ul(tags$li("drda five parameter logistic model: solid"),
                        tags$li("nls exponential model: dash"),
                        tags$li("nls five parameter logistic model: dot"),
                        tags$li("nls four parameter logistic model: dashdot")),
                tags$p("Below the figure or the message that is displayed when there are no standard curve models retrieved by the standard curve algorithm,
                      there is a button which when clicked downloads a spreadsheet containing the standard curve fits from the database for the current experiment selected in the current study.
                       The second button below that downloads a spreadsheet containing the sample data from the database for the current experiment selected in the current study."),
                tags$p("Following the download buttons for the standard curve model fits and the sample data there is a select box to select one or more antigens from the currently selected antigen family.
                      This selection generates a figure displaying the coefficient of variation as a percentage, plotted against the log of the dilution factor for the chosen antigens."),
                tags$p("Beneath the figure of the coefficient of variation by the log of the dilution factor for the antigens selected, an additional download button is available.
                      When pressed, this download button downloads the coefficient of variation by each log dilution factor for the current experiment in the selected study as an excel file.
                      In addition to the  coefficient of variation, the mean log dilution and standard deviation of log dilution are found as columns in the spreadsheet since they are required for the coefficient of variation calculation.")
              ), # end tagList
              style = "success"
              )
            ) #end bsCollapse
            ,
            mainPanel(
              #  uiOutput("studySelectionUI2"),
              # uiOutput("expirementSelectionUI2"),
              # Family Antigens
              fluidRow(
                column(4 ,uiOutput("antigenFamilySelectionUI")),
                column(4, uiOutput("antigenSelectionUI2")),
                column(4, uiOutput("sourceSelectionUI2"))
              ),

              #uiOutput("standard_curve_antigenUI"),
              #fluidRow(

              uiOutput("infoSavedMesssage"),
              #actionButton("summaryMethods", "View Methods"),
              br(),
              # textOutput("norm_mfi_view"),

              uiOutput("standard_curve_antigenContainer"),
              # plotlyOutput("standard_curve_antigenUI", width = "85vw"),

              br(),
              uiOutput("download_button_ui"),
              br(),
              uiOutput("download_sample_data_ui"),
              br(),
              uiOutput("save_norm_mfiUI"),
              br(),
              # uiOutput("save_norm_mfi_antigensUI"),
              # CV Plot Controls
              br(),
              uiOutput("antigens_in_family_UI"),
              # br(),
              #uiOutput("cv_log_dilution_plot_antigen_UI"),
              # br(),
              # uiOutput("download_cv_log_dilution_antigen"),

              br(),
              # uiOutput("length_display"),
              plotlyOutput("cv_log_dilution_plot", width = "75vw"),
              br(),
              uiOutput("download_cv_log_dilution_study_level")

              # uiOutput("mse_cv_plotUI"),

            ) # end mainPanel
            )

          ) # end column
        )  #end fluid row

        # end tagList
      })

      # uiOutput("expirementSelectionUI2"),
      # uiOutput("antigenSelectionUI2"),
      # uiOutput("sourceSelectionUI2"),
      #  readxMap_study_accession
      #readxMap_experiment_accession

      # output$studySelectionUI2 <- renderUI({
      #   req(fitted_curve_parameters$study_accession)
      #   selectInput("studySelection2",
      #               label = "Study",
      #               choices = unique(fitted_curve_parameters$study_accession))
      #
      # })

      # output$expirementSelectionUI2 <- renderUI({
      #   req(fitted_curve_parameters$experiment_accession)
      #   selectInput("experimentSelection2",
      #               label = "Experiment",
      #               choices = unique(fitted_curve_parameters$experiment_accession))
      #
      # })

      output$antigenSelectionUI2 <- renderUI({
        req(fitted_curve_parameters)
        # req(experiment_fitted_rv())
        # if (nrow(experiment_fitted_rv()) == 0) {
        #   return(NULL)
        # } else {
          req(input$readxMap_study_accession, input$readxMap_experiment_accession)
          req(fitted_curve_parameters$study_accession, fitted_curve_parameters$experiment_accession)
          # require the antigen family
          req(input$antigenFamilySelection)

          updateSelectInput(session, "antigenSelection2", selected = NULL)

          dat_antigen <- fitted_curve_parameters[fitted_curve_parameters$study_accession %in% input$readxMap_study_accession &
                                                   fitted_curve_parameters$experiment_accession %in% input$readxMap_experiment_accession &
                                                   fitted_curve_parameters$antigen_family %in% input$antigenFamilySelection, ]
          req(nrow(dat_antigen) > 0)

          my_label <- paste0("Select a Single Antigen in ", input$antigenFamilySelection," for plotting Standard Curves")
          selectInput("antigenSelection2",
                      label = my_label,
                      choices = unique(dat_antigen$antigen)) #unique(dat_antigen$antigen)
        #}
      })

      # req(family_antigens_reactive())
      # req(input$antigenFamilySelection)
      # updateSelectInput(session, "antigenSelection2", selected = NULL)
      # selectInput("antigenSelection2",
      #             label = paste0("Select one Antigen in ",input$antigenFamilySelection, "for plotting Standard Curves"),
      #             choices = unique(family_antigens_reactive()$antigen),
      #             multiple = F)

      output$sourceSelectionUI2 <- renderUI({
        req(fitted_curve_parameters$study_accession, fitted_curve_parameters$experiment_accession)
        req(input$readxMap_study_accession, input$readxMap_experiment_accession)
        updateRadioButtons(session, "sourceSelection2", selected = NULL)

        dat_source <- fitted_curve_parameters[fitted_curve_parameters$study_accession %in% input$readxMap_study_accession &
                                                fitted_curve_parameters$experiment_accession %in% input$readxMap_experiment_accession &
                                                fitted_curve_parameters$antigen %in% input$antigenSelection2, ]
        req(nrow(dat_source) > 0)

        radioButtons("sourceSelection2",
                     label = "Source",
                     choices = unique(dat_source$source))

      })

      # Choose Antigen Family must be visable
      output$antigenFamilySelectionUI <- renderUI({

        req(std_curve_data$antigen_family)

        selectInput("antigenFamilySelection",
                    label = "Antigen Family",
                    choices = unique(std_curve_data$antigen_family))

      })

      output$save_norm_mfiUI <- renderUI ({
        req(experiment_fitted_rv())
        if (nrow(experiment_fitted_rv()) == 0) {
          return(NULL)
        } else {
          req(input$readxMap_study_accession, input$readxMap_experiment_accession)
          actionButton("save_norm_mfi",
                       label = paste0("Save Normalized MFI values for each antigen within ",
                                      input$readxMap_experiment_accession, " in ", input$readxMap_study_accession))
        }
      })


      # Save Normalized MFI for each antigen within the current experiment
      observeEvent(input$save_norm_mfi, {
        req(sample_data_reactive(), aggregated_curve_rv())
        sample_data_norm_mfi <- preform_linear_interpolation(sample_data_reactive(), aggregated_curve_rv())
        save_normalized_mfi(sample_data_norm_mfi)

      })

      #family-specific subset of std_curve_data
      family_antigens_reactive <- reactive({
        req(std_curve_data)
        req(input$antigenFamilySelection)

        family_antigens <- std_curve_data[std_curve_data$antigen_family == input$antigenFamilySelection,]
        return(family_antigens)
      })

      # Choose antigens within the family
      output$antigens_in_family_UI <- renderUI({
        req(family_antigens_reactive())
        req(input$antigenFamilySelection)
        updateSelectInput(session, "selected_familial_antigen", selected = NULL)
        selectInput("selected_familial_antigen",
                    label = paste0("Select Antigen(s) in ",input$antigenFamilySelection, " for plotting Coefficient of Variation vs Log Dilution"),
                    choices = unique(family_antigens_reactive()$antigen),
                    multiple = TRUE)
      })


      experiment_fitted_rv <- reactive({
        req(fitted_curve_parameters)
        req(std_curve_data)
       # req(background_control_rv())
        req(study_configuration)
        bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))

        # find the appropriate min log dilution to start the graph
        std_curve_data_antigen <- std_curve_data[std_curve_data$antigen == input$antigenSelection2,]
        min_log_dilution <- round(min(std_curve_data_antigen$log_dilution), 0)
        # filter out models with NA crit value and to the selected bkg method.
        fitted_curve_parameters <- fitted_curve_parameters[!is.na(fitted_curve_parameters$crit),]
        fitted_curve_parameters <<- fitted_curve_parameters[fitted_curve_parameters$bkg_method == bkg_method,]

         if (nrow(fitted_curve_parameters) > 0) {

          experiment_fitted_df <- compute_fitted_df(fitted_curve_parameters, min_log_dilution)
          # Account if log mfi is selected or not in user settings - This will Take it 2x
          # if (is_log_mfi) {
          #   experiment_fitted_df$fitted <- log10(experiment_fitted_df$fitted)
          # }
          #experiment_fitted_df_v <- experiment_fitted_df
        } else {
          experiment_fitted_df <- data.frame()
        }


       # xperiment_fitted_df_v <<- experiment_fitted_df

        return(experiment_fitted_df)
      })

      # aggregated curve fit and data
      aggregated_curve_rv <- reactive({
        req(input$antigenSelection2)
        req(experiment_fitted_rv())
        req(nrow(experiment_fitted_rv()) > 0)
     #   req(background_control_rv())
        req(study_configuration)
        bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
        req(std_curve_data)
        std_curve_data_antigen <- std_curve_data[std_curve_data$antigen == input$antigenSelection2,]
        min_log_dilution <- round(min(std_curve_data_antigen$log_dilution), 0)

        aggregated_curve <- aggregate_standard_curves(experiment_fitted_rv(), antigen_selection = input$antigenSelection2,
                                                      min_log_dilution = min_log_dilution,
                                                      g_value = 0.5,
                                                      bkg = bkg_method,
                                                      is_log_mfi_axis = is_log_mfi)
        return(aggregated_curve)
      })

      # reactive value for the sample data
      sample_data_reactive <- reactive({
        req(sample_data)
        return(sample_data)
      })

      output$standard_curve_antigenUI <- renderPlotly({
        req(study_configuration)
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))

        req(experiment_fitted_rv())
        if (nrow(experiment_fitted_rv()) == 0) {
          return(NULL)
        } else {
          #req(nrow(experiment_fitted_rv()) > 0)
          #  req(nrow(aggregated_curve_rv()) > 0)  # Ensure aggregated data has rows
          req(aggregated_curve_rv())
          req(cv_log_dilution_rv())

          # norm_mfi <- compute_normalizezd_mfi(aggrigated_fit_in = aggregated_curve_rv(),
          #                         sample_data = sample_data_reactive(),
          #                         dat = std_curve_data[std_curve_data$antigen == input$antigenSelection2,])
          plot_standard_curves(fitted_df_in = experiment_fitted_rv(),
                               cv_dilutions = cv_log_dilution_rv(),
                               aggrigated_fit_df = aggregated_curve_rv(),
                               antigen_selection = input$antigenSelection2,
                               is_log_mfi_axis = is_log_mfi)
        }
      })


      output$standard_curve_antigenContainer <- renderUI({
        req(experiment_fitted_rv())

        # Only show the plot if the condition is met
        if (nrow(experiment_fitted_rv()) > 0) {
          plotlyOutput("standard_curve_antigenUI", width = "75vw")
        } else {
          NULL  # Hide the plot completely
        }
      })

      output$infoSavedMesssage <- renderUI({
        req(experiment_fitted_rv())
        req(background_control_rv())
        if (background_control_rv() == "subtracted") {
          control_string <- "subtracting 1 x Geometric mean"
        } else if (background_control_rv() == "subtracted_3x") {
          control_string <- "subtracting 3 x Geometric mean"
        } else if (background_control_rv() == "subtracted_10x") {
          control_string <- "subtracting 10 x Geometric mean"
        } else if (background_control_rv() == "included") {
          control_string <- "inclduing geometric mean in standards"
        } else {
          control_string <- " ignoring the blanks."
        }
        if (nrow(experiment_fitted_rv()) > 0) {
          NULL
        } else {
          HTML(paste("<span style='font-size:20px;'>There are no Standard Curve models saved by the algorithm in the Standard Curve Fitting Panel using the method: ",
                     control_string, ".<br></span>"))
        }
      })


      # data frame with cv log dilution reactive
      cv_log_dilution_rv <- reactive({
        req(std_curve_data)
        calculate_cv_log_dilution_platewise(std_curve_data)
      })
      # plot the cv and log dilution
      output$cv_log_dilution_plot <- renderPlotly({
        #req(length_rv())
        # if (is.null(length_rv())) {
        #   NULL
        # } else {
        req(cv_log_dilution_rv())
        req(input$selected_familial_antigen > 0)
        plot_log_dilution_cv(cv_log_dilution_rv(), antigen_selection = input$selected_familial_antigen)

      })

      output$cv_log_dilution_plot_antigen_UI <- renderUI({
        req(cv_log_dilution_rv())
        req(input$antigenSelection2)
        plot_log_dilution_cv(cv_log_dilution_rv(), antigen_selection = input$antigenSelection2)
      })


      # Data frame with MSE and CV for plotting
      mse_cv_df_rv <- reactive({
        req(fitted_curve_feature)
        obtain_mse_cv(standard_curve_fits = fitted_curve_feature)
      })

      output$mse_cv_plotUI<- renderUI({
        req(mse_cv_df_rv())
        plot_mse_cv(mse_cv_df = mse_cv_df_rv())
      })

      # download standard curve data
      output$download_button_ui <- renderUI({
        req(fitted_curve_feature)
        req(input$readxMap_study_accession, input$readxMap_experiment_accession)
        #download_fits_experiment(download_df = fitted_curve_feature, input$readxMap_study_accession, input$readxMap_experiment_accession)
        button_label <-  paste0("Download Standard Curve Fits Data for ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession)

        downloadButton("download_standard_curve_data", button_label)
      })

      output$download_standard_curve_data <-  downloadHandler(
        filename = function() {
          paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "_fits_data", ".csv", sep = "_")
        },
        content = function(file) {
          req(fitted_curve_feature)
          req(input$readxMap_study_accession, input$readxMap_experiment_accession)

          download_df <- fitted_curve_feature[fitted_curve_feature$experiment_accession == input$readxMap_experiment_accession,]

          # download data component (data frame)
          write.csv(download_df, file, row.names = FALSE)
        }
      )



      #download sample data with the bend line gating.
      output$download_sample_data_ui <- renderUI({
        req(sample_data_feature_download)
        req(input$readxMap_study_accession, input$readxMap_experiment_accession)

        # download_sample_data_experiment(download_df = sample_data_feature_download,
        #                                 selected_study = input$readxMap_study_accession,
        #                                 selected_experiment = input$readxMap_experiment_accession)
        button_label <-  paste0("Download Sample Data for ",input$readxMap_experiment_accession, " in ",input$readxMap_study_accession)

        downloadButton("download_sample_data_handle", button_label)
      })


      output$download_sample_data_handle <-  downloadHandler(
        filename = function() {
          paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "sample_data", ".csv", sep = "_")
        },
        content = function(file) {
          req(sample_data_feature_download)
          req(input$readxMap_study_accession, input$readxMap_experiment_accession)


          download_df <- sample_data_feature_download[sample_data_feature_download$experiment_accession == input$readxMap_experiment_accession,]


          # download data component (data frame)
          write.csv(download_df, file, row.names = FALSE)
        }
      )



      # download the coefficient of variation, log dilution df at the antigen level
      output$download_cv_log_dilution_antigen <- renderUI({
        req(cv_log_dilution_rv())
        req(input$readxMap_study_accession, input$readxMap_experiment_accession, input$antigenSelection2)
        download_cv_log_dilution_data(download_df = cv_log_dilution_rv(),
                                      selected_study = input$readxMap_study_accession,
                                      selected_experiment = input$readxMap_experiment_accession,
                                      selected_antigen = input$antigenSelection2)
      })



      # download the coefficient of variation, log dilution at the study level.
      output$download_cv_log_dilution_study_level <- renderUI({
        req(cv_log_dilution_rv())
        req(input$readxMap_study_accession, input$readxMap_experiment_accession)
        # download_cv_log_dilution_data(download_df = cv_log_dilution_rv(),
        #                               selected_study = input$readxMap_study_accession,
        #                               selected_experiment = input$readxMap_experiment_accession,
        #                               selected_antigen = NULL)
       collapse_antigens<- ifelse(!is.null(input$selected_familial_antigen), paste(input$selected_familial_antigen, collapse = ", "), "")

        button_label <- ifelse(is.null(input$selected_familial_antigen),
                               paste0("Download Coefficent of Variation by Log Dilution and Log Dilution Data for ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession),
                               paste0("Download Coefficent of Variation by Log Dilution and Log Dilution Data for ", collapse_antigens, " ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession))

        downloadButton("download_cv_log_dilution_study_level_handle", button_label)

      })


      output$download_cv_log_dilution_study_level_handle <- downloadHandler(
        filename = function() {
          collapse_antigens <- ifelse(!is.null(input$selected_familial_antigen), paste(input$selected_familial_antigen, collapse = "_"), "")

          ifelse(is.null(input$selected_familial_antigen),paste0(input$readxMap_study_accession, "_",input$readxMap_experiment_accession, "_cv_log_dilution_data.csv"),
                 paste0(input$readxMap_study_accession, "_",input$readxMap_experiment_accession, "_", collapse_antigens, "_cv_log_dilution_data.csv"))


        },
        content = function(file) {
          req(cv_log_dilution_rv())
          req(input$readxMap_study_accession, input$readxMap_experiment_accession)

          if (!is.null(input$selected_familial_antigen)) {
            download_df <- cv_log_dilution_rv()[cv_log_dilution_rv()$antigen == input$selected_familial_antigen,]

          }
          write.csv(download_df, file, row.names = FALSE)
        }
      )




    } else {
      output$standardCurveSummaryUI <- renderUI({
        br()
        uiOutput("nofits")
      })
      output$nofits <- renderUI({
        HTML(paste0("<span style='font-size:20px;'>There are no standard curves saved by the algorithm in the standard curve Fitting Panel for
                      ", selected_studyexpplate$experiment_accession," in ", selected_studyexpplate$study_accession,
                       " using the selected method for the blanks.</span>"))

      })
    } # end is standard curves avaliable in db check
  } # end is loaded data
    else {
      output$standardCurveSummaryUI <- renderUI(NULL)
      output$antigenFamilySelectionUI <- renderUI(NULL)
      output$antigenSelectionUI2 <- renderUI(NULL)
      output$sourceSelectionUI2 <- renderUI(NULL)
      output$infoSavedMesssage <- renderUI(NULL)
      output$standard_curve_antigenContainer <- renderUI(NULL)
      output$download_button_ui <- renderUI(NULL)
      output$download_sample_data_ui <- renderUI(NULL)
      output$save_norm_mfiUI <- renderUI(NULL)
      output$antigens_in_family_UI <- renderUI(NULL)
      output$cv_log_dilution_plot <- renderPlotly(NULL)
      output$download_cv_log_dilution_study_level <- renderUI(NULL)

    }
}
)# end observe event
