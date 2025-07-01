standard_curve_table_list <- list()

data_summary <- list()

#std_curve_tab_active <- reactiveVal(FALSE)

# experiment_data <- reactive({
#   selected_study <- selected_studyexpplate$study_accession
#   selected_experiment <- selected_studyexpplate$experiment_accession
#
#   # Fetch the standard curves using the selected study and experiment
#   if (!is.null(selected_study) && !is.null(selected_experiment)) {
#     return(fetch_standard_curves(selected_study, selected_experiment))
#   } else {
#     return(NULL)
#   }
# })

observeEvent(input$inLoadedData, {
  if (input$inLoadedData == "Standard Curve") {

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
          # Account if log mfi is selected or not in user settings
          if (is_log_mfi) {
            experiment_fitted_df$fitted <- log10(experiment_fitted_df$fitted)
          }
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
        download_fits_experiment(download_df = fitted_curve_feature, input$readxMap_study_accession, input$readxMap_experiment_accession)
      })

      #download sample data with the bend line gating.
      output$download_sample_data_ui <- renderUI({
        req(sample_data_feature_download)
        req(input$readxMap_study_accession, input$readxMap_experiment_accession)

        download_sample_data_experiment(download_df = sample_data_feature_download,
                                        selected_study = input$readxMap_study_accession,
                                        selected_experiment = input$readxMap_experiment_accession)
      })

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
        download_cv_log_dilution_data(download_df = cv_log_dilution_rv(),
                                      selected_study = input$readxMap_study_accession,
                                      selected_experiment = input$readxMap_experiment_accession,
                                      selected_antigen = NULL)
      })


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
}
)# end observe event
