observeEvent(list(input$inLoadedData, input$readxMap_experiment_accession), {
  req(input$inLoadedData, input$readxMap_experiment_accession)

  if (input$inLoadedData == "Bead Count") {
    selected_study <- selected_studyexpplate$study_accession
    selected_experiment <- selected_studyexpplate$experiment_accession

    sample_data <- stored_plates_data$stored_sample
    standard_data_curve <- stored_plates_data$stored_standard
    buffer_data <- stored_plates_data$stored_buffer

    # Check if selected study, experiment, and sample data are available
    if (!is.null(selected_study) && length(selected_study) > 0 &&
        !is.null(selected_experiment) && length(selected_experiment) > 0 &&
        !is.null(sample_data) && length(sample_data) > 0){

      # Filter sample data
      sample_data$selected_str <- paste0(sample_data$study_accession, sample_data$experiment_accession)
      sample_data <- sample_data[sample_data$selected_str == paste0(selected_study, selected_experiment), ]

      # Summarize sample data
      cat("Viewing sample data")
      print(names(sample_data))
      print(table(sample_data$plateid))
      print(table(sample_data$antigen))
      cat("After summarizing sample data")


      # Rename columns

      sample_data <- dplyr::rename(sample_data, arm_name = agroup)
      sample_data <- dplyr::rename(sample_data, visit_name = timeperiod)


      sample_data$subject_accession <- sample_data$patientid

      sample_data <- dplyr::rename(sample_data, value_reported = mfi)

      arm_choices <- unique(sample_data$arm_name)
      visits <- unique(sample_data$visit_name)

    }

    # Check if selected study, experiment, and standard data are available
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


      # Rename columns

      # data <- dplyr::rename(data, arm_name = agroup)
      #data <- dplyr::rename(data, visit_name = timeperiod)


      std_curve_data$subject_accession <- std_curve_data$patientid

      std_curve_data <- calculate_log_dilution(std_curve_data)
      cat("Standard Curve data after calculating log dilutions")
      print(names(std_curve_data))

    }

    ## Load study configuration
    study_configuration <- fetch_study_configuration(study_accession = selected_study , user = currentuser())
    failed_well_criteria <- study_configuration[study_configuration$param_name == "failed_well_criteria",]$param_character_value
    upper_bc_threshold <- study_configuration[study_configuration$param_name == "upper_bc_threshold",]$param_integer_value
    lower_bc_threshold <-  study_configuration[study_configuration$param_name == "lower_bc_threshold",]$param_integer_value
    pct_agg_threshold <- study_configuration[study_configuration$param_name == "pct_agg_threshold",]$param_integer_value
    print(failed_well_criteria)
    print(upper_bc_threshold)
    print(lower_bc_threshold)
    print(pct_agg_threshold)




  output$beadCountAnalysisUI <- renderUI({
    req(study_configuration)
    tagList(
      br(),
      fluidRow(
        bsCollapse(
          id = "bead_count_collapse",
          bsCollapsePanel(
            title = "Bead Count Analysis Methods",
            tagList(
              tags$p("Use the dropdown menu labeled ‘Plate in Sample Data’ to select a plate from the sample data in the currently selected experiment. Then, choose an antigen from the selected plate to analyze bead counts for that antigen."),
              tags$p("The figure below the dropdown menus displays bead counts on the y-axis for each of the 96 wells in the multiplex bead assay on the x-axis (Matson, Zachary et al). In the figure:"),
              tags$ul(
                tags$li("The red dotted horizontal line represents the lower threshold value."),
                tags$li("The blue dotted horizontal line represents the upper threshold value."),
                tags$li("The default lower and upper thresholds are 35 and 50, respectively."),
                tags$li("The black solid horizontal line represents the average bead count across all wells."),
                tags$li("Red-colored wells indicate a low bead count, while blue-colored wells have sufficient bead counts based on the failed well criterion.")
              ),

              tags$p("The failed well criterion is either wells with bead counts below the upper threshold or wells with bead counts below the upper threshold.
                     To adjust the lower and upper thresholds and modify the failed well criterion, go to the Study Overview tab and navigate to Bead Count Options."),

           tags$p("Below the figure, a table titled ‘Sample Values with Low Bead Counts’ lists samples that meet the low bead count criteria. The table includes:"),
           tags$ul(
                tags$li("bead_count_gc: The gate class of the bead count, indicating whether it is sufficient or low."),
                tags$li("is_low_bead_count: A Boolean column where true indicates a low bead count and false indicates a sufficient bead count.")
           ),

          tags$p("To download the bead count gate class for all samples in the currently selected experiment within the selected study, click the orange download button below the table."),
          tags$h3("References"),
          tags$p("Matson, Zachary et al. “shinyMBA: a novel R shiny application for quality control of the multiplex bead assay for serosurveillance studies.” Scientific reports vol. 14,1 7442. 28 Mar. 2024, doi:10.1038/s41598-024-57652-4")
            ), #end tagList
            style = "success"
          )

        ),
      mainPanel(
      fluidRow(
        column(6, uiOutput("plateSelection_bead_count_UI")),
        column(6,uiOutput("sample_data_antigenUI")),
        #column(4, uiOutput("failed_well_criteriaUI"))
      ),
      # fluidRow(
      #   column(4, uiOutput("lower_threshold_ui")),
      #   column(4, uiOutput("upper_threshold_ui"))
      #  # tableOutput("sample_data_well_view")
      # ),
      plotlyOutput("beadCountPlot", width = "75vw"),
      br(),
      bsCollapse(
        id = "sample_bead_count_collapse",
        bsCollapsePanel(
          title = "Samples with low Bead Counts",
          div(style = "overflow-x: auto; width: 100%;",tableOutput("sample_low_bead_count_table")),
          style = "primary"
        )
      ),
      #div(style = "overflow-x: auto; width: 100%;",tableOutput("sample_low_bead_count_table")),
      br(),
      uiOutput("download_bead_gating")
    )
    )
    )
})


  # sample data plates
  output$plateSelection_bead_count_UI <- renderUI({
    req(input$readxMap_study_accession, input$readxMap_experiment_accession)
    req(sample_data$study_accession, sample_data$experiment_accession)
    updateSelectInput(session, "plateSelection_bead", selected = NULL)  # Reset the plateSelection
    plate_data <- sample_data[sample_data$study_accession %in% input$readxMap_study_accession &
                                   sample_data$experiment_accession %in% input$readxMap_experiment_accession, ]

    req(nrow(plate_data) > 0)

    selectInput("plateSelection_bead",
                label = "Plate in Sample data",
                choices = unique(plate_data$plateid))
  })
  # sample data antigens
  output$sample_data_antigenUI <- renderUI({
    req(input$readxMap_study_accession, input$readxMap_experiment_accession)
    req(sample_data$study_accession, sample_data$experiment_accession)
    req(input$plateSelection_bead)  # Ensure a plate is selected

    updateSelectInput(session, "antigenSelectionBead", selected = NULL)
    # Filter data for the selected study, experiment, and plate
    dat_antigen <- sample_data[
      sample_data$study_accession %in% input$readxMap_study_accession &
        sample_data$experiment_accession %in% input$readxMap_experiment_accession &
        sample_data$plateid %in% input$plateSelection_bead,
    ]

    # Ensure there are valid antigen values
    req(nrow(dat_antigen) > 0)

    selectInput("antigenSelectionBead",
                label = "Select Antigen",
                choices = unique(dat_antigen$antigen))
  })

  # output$failed_well_criteriaUI <- renderUI({
  #    radioButtons("thresholdCriteria",
  #                label = "Failed Well Criteria",
  #                choices = c("Below Upper Threshold" = "upper",
  #                            "Below Lower Threshold" = "lower"),
  #                selected = "lower")
  # })

  # get the sample data with well number
  sample_data_well <- reactive({
    req(sample_data)
    req(input$plateSelection_bead, input$antigenSelectionBead)
    sample_data <- sample_data[sample_data$plateid == input$plateSelection_bead & sample_data$antigen == input$antigenSelectionBead,]
    sample_data_with_well_number <- obtain_well_number(sample_data, "well")
    return(sample_data_with_well_number)
  })

 #  # Lower threshold
 #  output$lower_threshold_ui <- renderUI({
 #    numericInput(inputId = "lower_threshold",
 #                 label = "Lower Threshold",
 #                 value = 25)
 #  })
 # # upper threshold
 #  output$upper_threshold_ui <- renderUI({
 #    numericInput(inputId = "upper_threshold",
 #                 label = "Upper Threshold",
 #                 value = 50)
 #  })

 # Plot bead count plot by wells on selected plate and antigen for sample data
  output$beadCountPlot <- renderPlotly({
    req(sample_data_well())
   # req(lower_threshold_rv())
   # req(upper_threshold_rv())
  # req(failed_well_criteria())
    req(study_configuration)
    # req(failed_well_criteria)
    # req(upper_bc_threshold)
    # req(lower_bc_threshold)

    plot_bead_count(df_well = sample_data_well(), lower_threshold = lower_bc_threshold,
                    upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)
    # plot_bead_count(df_well = sample_data_well(), lower_threshold = lower_threshold_rv(),
    #                 upper_threshold = upper_threshold_rv() , failed_well_criteria =  failed_well_criteria())
  })


  # Table of Sample data with low bead counts
  output$sample_low_bead_count_table <- renderTable({
    req(sample_data_well())
    #req(lower_threshold_rv())
    #req(upper_threshold_rv())
    req(study_configuration)
    #req(failed_well_criteria)
    # req(upper_bc_threshold)
    # req(lower_bc_threshold)

    # bead_count_gc_table <- bead_count_gc(df_well = sample_data_well(), lower_threshold = lower_threshold_rv(),
    #                 upper_threshold = upper_threshold_rv() , failed_well_criteria =  failed_well_criteria())
    bead_count_gc_table <- bead_count_gc(df_well = sample_data_well(), lower_threshold = lower_bc_threshold,
                     upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)

    bead_count_gc_table <- bead_count_gc_table[bead_count_gc_table$bead_count_gc == "Low Bead Count",]

    return(bead_count_gc_table)
  }, caption = "Sample Values with Low Bead Counts",
  caption.placement = getOption("xtable.caption.placement", "top"))

  # Download Bead Counts for Sample data
  output$download_bead_gating <- renderUI({
    req(sample_data)
    req(input$readxMap_study_accession, input$readxMap_experiment_accession)
    #req(lower_threshold_rv())
  #  req(upper_threshold_rv())
    req(study_configuration)
   # req(failed_well_criteria)
    # req(lower_bc_threshold)
    # req(upper_bc_threshold)
    # get well number
    sample_data_well <- obtain_well_number(sample_data, "well")

    bead_count_gc_table <- bead_count_gc(df_well = sample_data_well, lower_threshold = lower_bc_threshold,
                                         upper_threshold = upper_bc_threshold , failed_well_criteria =  failed_well_criteria)

    download_bead_count_data(download_df = bead_count_gc_table, selected_study = input$readxMap_study_accession,
                             selected_experiment = input$readxMap_experiment_accession)

  })

  }
})
