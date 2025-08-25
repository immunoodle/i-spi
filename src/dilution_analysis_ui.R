#observeEvent(list(input$inLoadedData, input$readxMap_experiment_accession), {
observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$advanced_qc_component,
  input$study_level_tabs,
  input$main_tabs), {

    req(input$advanced_qc_component == "Dilution Analysis",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")

 # req(input$inLoadedData, input$readxMap_experiment_accession)
 # input$readxMap_experiment_accession
  if (input$advanced_qc_component == "Dilution Analysis") {
    selected_study <- selected_studyexpplate$study_accession
    selected_experiment <- selected_studyexpplate$experiment_accession

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
      cat("Viewing sample dat in Dilution Analysis tab ")
      print(names(sample_data))
      print(table(sample_data$plateid))
      print(table(sample_data$antigen))
      cat("After summarizing sample data in Dilution Analysis tab")


      # Rename columns

      sample_data <- dplyr::rename(sample_data, arm_name = agroup)
      sample_data <- dplyr::rename(sample_data, visit_name = timeperiod)


      sample_data$subject_accession <- sample_data$patientid

      sample_data <- dplyr::rename(sample_data, value_reported = mfi)

      arm_choices <- unique(sample_data$arm_name)
      visits <- unique(sample_data$timeperiod)

    }

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
    }

    # load controls
    controls <- stored_plates_data$stored_control

    # load buffer data
    buffer_data <- stored_plates_data$stored_buffer
    if (!is.null(selected_study) && length(selected_study) >0 &&
        !is.null(selected_experiment) && length(selected_experiment) > 0 &&
        !is.null(buffer_data) && length(buffer_data) > 0) {

      buffer_data$selected_str <- paste0(buffer_data$study_accession, buffer_data$experiment_accession)
      buffer_data <- buffer_data[buffer_data$selected_str == paste0(selected_study, selected_experiment), ]
    }

    ## Load study configuration
    study_configuration <- fetch_study_configuration(study_accession = selected_study , user = currentuser())
    background_control_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
    is_log_mfi_axis <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))

    fitted_curve_parameters <- fetch_standard_curves_dilution(selected_study, selected_experiment, bkg_method = background_control_method,
                                                              is_log_mfi = is_log_mfi_axis)

    dilution_table_cols <- c("study_accession", "experiment_accession", "plateid", "timeperiod", "patientid", "agroup", "dilution",
                             "antigen", "n_pass_dilutions", "concentration_status", "au_treatment", "decision_nodes", "bkg_method", "processed_au")




    output$dilutionAnalysisUI <- renderUI({
      req(study_configuration)
      #study_configuration_v <- study_configuration
      tagList(
        fluidRow(
          column(12,
                 bsCollapse(
                   id = "dilutionAnalysisMethods",
                   bsCollapsePanel(
                     title = "Dilution Analysis Methods",
                     tagList(
                       tags$p("Use 'Select Antigen' dropdown menu to select one or more antigens of interest to conduct dilution analysis on in the sample data.
                     When the Dilution Analysis tab is loaded, all of the available dilutions are initially selected and shown in the ‘Select Dilutions’ dropdown menu.
                     Dilutions can be manually excluded or included before running the dilution analysis by changing this selection."),

                    tags$p("The dilution analysis for the selected antigens begins by following a decision tree structure.
                            When a standard curve is saved, sample values are gated and have attributes of being in the linear range and in the quantifiable range of the standard curve or not.
                            Using these characteristics, a decision tree can be made to classify the sample points as either passing classification or not passing classification. "),

                    tags$p("Figure 1 depicts the decision tree that is produced based on the decisions that are selected above.
                     Since a decision tree structure is used, when there are multiple decision nodes selected samples will pass classification
                     if they are in the path where all the nodes are true."),

                    tags$p("Figure 2 depicts a barplot entitled with the selected experiment's name followed by 'Proportion of Subjects by Antigen, Dilution Factor, and Concentration Status.
                    The antigens are sorted by the antigen order as defined by the researcher in the study configuration options."),

                    tags$p("Table 1, titled 'Number of Subjects Passing Classification by Timepoint, Concentration Status, and Number of Passing Dilutions'.
                      This contingency table lists the number of passing dilutions in the first column, with the number of subjects
                             having that number of passing dilutions across the rows at each timepoint in the experiment for each concentration status.
                             To view which subjects have a particular number of passing dilutions and concentration status at a particular time point,
                             either double click a cell in the table or select a time point, concentration status, number of dilutions from the dropdown menus below the table titled ‘Number of Passing Dilutions’ ,
                             'Select Concentration Status', and 'Select Timepoint' respectively. The color of each cell with patient counts represents how the arbitrary units are treated in the final calculation of the
                             analysis and correspond to the options which are colored by the option selected in the study configuration tab. A legend is provided with this color mapping. "),

                    tags$p("Figure 3 is displayed when the subjects who are passing are populated in the passing subjects selector.
                     Figure 3 depicts the plate dilution series, with log10 plate dilution on the x-axis and arbitrary units on the y-axis.
                     All of the subjects who are passing are selected by default and each subject's data is represented by its own line.
                     The color of the points indicates whether the dilution passes classification: blue for passing and red for not passing. "),

                    tags$p("Table 2, entitled 'Dilution Analysis Sample Output' displays the sample data for all timepoints for the selected antigen and includes a row for how the
                     processed AU is calculated (au_treatment) which corresponds to the color of the cell, the decision nodes used in the decision tree (decision_nodes),
                     the background method which is selected from study overview in the standard curve options and the processed au (procesed_au).")





                     ), # end tagList
                     style = "success"
                   )# end bsCollapsePanel
                 ), # end bsCollapse
                 mainPanel(
                   uiOutput("parameter_dependencies"),
                   br(),
                   grVizOutput("decision_tree", width = "75vw"),
                   br(),
                   uiOutput("linear_message"),
                   uiOutput("quantifiable_message"),
                   uiOutput("gate_message"),
                   br(),
                   plotlyOutput("dilution_summary_barplot", width = "75vw"),
                   br(),
                   uiOutput("download_dilution_contingency_summary"),
                   br(),
                  # tableOutput("view_classified_merged"),
                  # br(),
                   fluidRow(column(6, uiOutput("dilution_selector_UI")),
                            column(6, uiOutput("antigen_selector_UI"))),
                   div(style = "overflow-x: auto; width: 75vw;",
                   DT::dataTableOutput("margin_table_antigen")),
                   div(style = "width: 75vw;", uiOutput("color_legend")),
                   uiOutput("download_classified_sample_UI"),
                   br(),
                bsCollapse(
                  id = "da_subject_level_inspection",
                  bsCollapsePanel(
                    title = "Subject Level Inspection",
                    select_group_ui(
                      id = "da_filters",
                      params = list(list(inputId = "n_pass_dilutions", label = "Number of Passing Dilutions", multiple = F),
                                    list(inputId = "concentration_status", label = "Select Concentration Status", multiple = F),
                                    timeperiod = list(inputId = "timeperiod", label = "Select Timepoint", multiple = F))
                    ),
                    uiOutput("passing_subject_selection"),
                    plotlyOutput("patient_dilution_series_plot", width = "75vw"),
                    style = "primary"
                  )
                ),
                  bsCollapse(
                    id = "da_datasets",
                    bsCollapsePanel(
                      title = "Dilution Analysis Output Dataset",
                     DT::dataTableOutput("final_average_au_table"),
                      br(),
                      downloadButton("download_average_au_table_UI", label = "Download Processed Dilution Data"),
                      style = "primary"
                    )
                  )
                # bsCollapse(
                #   id = "da_linearity",
                #   bsCollapsePanel(
                #     title = "Dilution Linearity",
                #     fluidRow(column(6,
                #     uiOutput("response_selectionUI")),
                #     column(6,uiOutput("plate_lm_selectionUI"))),
                #     plotlyOutput("facet_lm_plot", width = "75vw"),
                #     style = "primary"
                #   )
                # ),
                # bsCollapse(
                #   id = "linearity_stats",
                #   bsCollapsePanel(
                #     title = "Summary Statistics",
                #     div(class = "table-container", tableOutput("facet_model_glance")),
                #     style = "primary"
                #   )
                # )
                 ) # end mainPanel
                 )
          )
        )
    })

    ## Box noting parameter dependencies
    output$parameter_dependencies <- renderUI({
     tagList(bsCollapse(
        id = "param_dependencies",
        bsCollapsePanel(
          title = "Parameter Dependencies",
          HTML(
            "The decision tree is dependent on the selection of the type of sample limit and the order in which they are chosen.<br>
         In addition, the method selected for Blank Control influences the classification of the concentration status, as parameter estimates can vary depending on how the blanks
         are treated when fitting standard curves.<br>
         The final treatment of the arbitrary units is dependent on the selection chosen corresponding to the treatment for the combination of number of passing dilutions and concentration status.<br>
         The order of the timeperiods in the table 'Number of Subjects Passing Classification by Timepoint, Concentration Status, and Number of Passing Dilutions' is ordered by the timeperiod order set in the study parameters. <br>
         For dilutional linearity when the resposne is MFI the determination of if it is log10 of MFI or raw MFI is made in the study configuration for use log units for MFI control."
          ),
          style = "info"
        )
      ),
      actionButton("to_study_parameters", label = "Return to Study Parameters")
     )
    })

    # Switch tabs when click button
    observeEvent(input$to_study_parameters, {
      updateTabsetPanel(session, inputId = "study_level_tabs", selected = "Study Parameters")
    })



    ### Decision tree
    decision_tree_reactive <- reactive({
      #req(input$inLoadedData == "Dilution Analysis")
      req(input$advanced_qc_component == "Dilution Analysis")
      req(study_configuration)

      is_binary <- study_configuration[study_configuration$param_name == "is_binary_gc",]$param_boolean_value
      node_order <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]
      sufficient_gc <- strsplit(study_configuration[study_configuration$param_name == "valid_gate_class",]$param_character_value, ",")[[1]]

      replacements <- c(limits_of_detection = "gate", linear_region = "linear", limits_of_quantification = "quantifiable")
      node_order <- ifelse(node_order %in% names(replacements), replacements[node_order], node_order)

      replacements_gate <- c(Between_Limits_of_Detection = "Between_Limits", Above_Upper_Limit_of_Detection = "Above_Upper_Limit", Below_Lower_Limit_of_Detection = "Below_Lower_Limit")
      sufficient_gc <- ifelse(sufficient_gc %in% names(replacements_gate), replacements_gate[sufficient_gc], sufficient_gc)

      # Create truth table inline
      truth_table <- create_truth_table(
        binary_gate = is_binary,
        exclude_linear = FALSE,
        exclude_quantifiable = FALSE,
        exclude_gate = FALSE
      )

      # Create decision tree
      decision_tree <- create_decision_tree_tt(
        truth_table = truth_table,
        binary_gate = is_binary,
        sufficient_gc_vector = sufficient_gc,
        node_order = node_order
      )

      decision_tree
    })


    ## Decision Tree Plot
    output$decision_tree <- renderGrViz({
      req(decision_tree_reactive())

      decision_tree <- decision_tree_reactive()

      dot_string <- paste(
        "digraph tree {",
        paste(get_edges(decision_tree), collapse = "; "),
        "}",
        sep = "\n"
      )
      grViz(dot_string)
      #decision_tree$ToDiagrammeRGraph()
    })



    # Summary data for contingency table with dilutions
      # summary_gated_data_rv <- reactive({
      #    req(study_configuration)
      #
      #   cat("in reactive summary")
      #     cat(selected_study)
      #     cat(selected_experiment)
      #
      #     node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]
      #
      #     gated_data <- calculate_sample_concentration_status(study_accession = selected_study, experiment_accession = selected_experiment, node_order = node_order_in)
      #     return(gated_data)
      # })

    # summary_gated_data_rv <- reactive({
    #   req(input$main_tabs == "view_files_tab")
    #   req(input$inLoadedData == "Dilution Analysis")
    #   if (input$inLoadedData != "Dilution Analysis") {
    #     return(NULL)
    #   }
    #  # selected_study
    #   selected_experiment
    #   req(study_configuration)
    #   cat("in reactive summary")
    #   cat(selected_study)
    #   cat(selected_experiment)
    #
    #   node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]
    #   valid_gc_in <- strsplit(study_configuration[study_configuration$param_name == "valid_gate_class",]$param_character_value, ",")[[1]]
    #   background_control_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
    #   is_log_mfi_axis <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
    #
    #   replacements <- c(limits_of_detection = "gate", linear_region = "linear", limits_of_quantification = "quantifiable")
    #   node_order_in <- ifelse(node_order_in %in% names(replacements), replacements[node_order_in], node_order_in)
    #
    #   replacements_gate <- c(Between_Limits_of_Detection = "Between_Limits", Above_Upper_Limit_of_Detection = "Above_Upper_Limit", Below_Lower_Limit_of_Detection = "Below_Lower_Limit")
    #   valid_gc_in <- ifelse(valid_gc_in %in% names(replacements_gate), replacements_gate[valid_gc_in], valid_gc_in)
    #
    #   cat("\nbefore joined data")
    #   joined_data <- join_sample_standard_data(study_accession = selected_study,
    #                                            experiment_accession = selected_experiment,
    #                                            bkg_method = background_control_method,
    #                                            node_order = node_order_in,
    #                                            is_log_mfi_axis = is_log_mfi_axis,
    #                                            valid_gate_class = valid_gc_in)
    #   print(nrow(joined_data))
    #   cat("\n after joined data")
    #   if ("linear" %in% node_order_in) {
    #     joined_data <- joined_data[!is.na(joined_data$in_linear_region), ]
    #     if (nrow(joined_data) == 0) {
    #       output$linear_message <- renderUI({
    #         req(study_configuration)
    #         node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]
    #         background_control_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
    #         HTML(paste("<span style='font-size: 1.5em;'> No samples found in the in the linear region for ",selected_experiment, " in ",
    #                    selected_study, " using the ", background_control_method, " method for the blanks."))
    #       })
    #     } else {
    #       output$linear_message <- renderUI({
    #         NULL
    #       })
    #     }
    #   }
    #   if ("quantifiable" %in% node_order_in) {
    #     joined_data <- joined_data[!is.na(joined_data$in_quantifiable_range), ]
    #     if (nrow(joined_data) == 0) {
    #       output$quantifiable_message <- renderUI({
    #         req(study_configuration)
    #         node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]
    #         background_control_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
    #         HTML(paste("<span style='font-size: 1.5em;'> No samples found in the in the quantifiable range for ",selected_experiment, " in ",
    #                    selected_study, " using the ", background_control_method, " method for the blanks."))
    #       })
    #     } else {
    #       output$quantifiable_message <- renderUI({
    #         NULL
    #       })
    #     }
    #   }
    #   if ("gate" %in% node_order_in) {
    #     joined_data <-  joined_data[!is.na(joined_data$llod) & !is.na(joined_data$ulod), ]
    #
    #     if (nrow(joined_data) == 0) {
    #       output$gate_message <- renderUI({
    #         req(study_configuration)
    #         node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]
    #         background_control_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
    #         HTML(paste("<span style='font-size: 1.5em;'> No samples found in the passing Limit of Detection ",selected_experiment, " in ",
    #                    selected_study, " using the ", background_control_method, " method for the blanks."))
    #       })
    #     } else {
    #       output$gate_message <- renderUI({
    #         NULL
    #       })
    #     }
    #   }
    #
    #
    #   print(str(joined_data))
    #  joined_data <<- joined_data
    #   return(joined_data)
    # })


    output$dilution_summary_barplot <- renderPlotly({
     # req(input$inLoadedData == "Dilution Analysis")
      req(input$advanced_qc_component == "Dilution Analysis")
      # if (input$inLoadedData != "Dilution Analysis") {
      #   return(NULL)
      # }
      req(selected_study)
      req(selected_experiment)
    #  req(summary_gated_data_rv())
      req(study_configuration)

      node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]

      gated_data <- calculate_sample_concentration_status(study_accession = selected_study, experiment_accession = selected_experiment, node_order = node_order_in)

      antigen_family_df <- fetch_antigen_family_df(study_accession = selected_study)
      antigen_family_order_in <- strsplit(study_configuration[study_configuration$param_name == "antigen_family_order",]$param_character_value, ",")[[1]]
      contigency_summary_dilution <- produce_contigency_summary(gated_data)
      summary_dilution_plot(dilution_summary_df = contigency_summary_dilution,
                            antigen_families = antigen_family_df,
                            antigen_family_order = antigen_family_order_in)

    })

    output$download_dilution_contingency_summary <- renderUI({
      req(selected_study)
      req(selected_experiment)

      # node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]
      #
      # gated_data <- calculate_sample_concentration_status(study_accession = selected_study, experiment_accession = selected_experiment, node_order = node_order_in)
      #
      # contigency_summary_dilution <- produce_contigency_summary(gated_data)
      # download_dilution_contigency_summary_fun(download_df = contigency_summary_dilution,
      #                                          selected_study = selected_study,
      #                                          selected_experiment = selected_experiment)
      button_label <-  paste0("Download Dilution Summary ", selected_experiment, "-", selected_study)

      downloadButton("download_dilution_contingency_summary_handle", button_label)
    })

    output$download_dilution_contingency_summary_handle <-  downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "dilution_summary.csv", sep = "_")
      },
      content = function(file) {
        req(selected_study)
        req(selected_experiment)
        node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]

        gated_data <- calculate_sample_concentration_status(study_accession = selected_study, experiment_accession = selected_experiment, node_order = node_order_in)

        contigency_summary_dilution <- produce_contigency_summary(gated_data)

        # download data component (data frame)
        write.csv(contigency_summary_dilution, file, row.names = FALSE)
      }
    )




    ##### Narrow down on dilutions
    # Dilution Selection
    output$dilution_selector_UI <- renderUI({
     # req(input$inLoadedData == "Dilution Analysis")
      req(study_configuration)
      node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]

      gated_data <- calculate_sample_concentration_status(study_accession = selected_study, experiment_accession = selected_experiment, node_order = node_order_in)

      # req(summary_gated_data_rv())
    #  req(nrow(summary_gated_data_rv()) > 0)
    #  req(nrow(gated_data) > 0)
      if (nrow(gated_data) == 0) {
        return(NULL)
      }

      selectInput("dilution_da_selector",
                  label = "Select Dilutions",
                  choices = unique(gated_data$dilution),
                  multiple = T,
                  # include all dilutions initially
                  selected = unique(gated_data$dilution))
    })

    # antigen selector
    output$antigen_selector_UI <- renderUI({
     # req(input$inLoadedData == "Dilution Analysis")
      req(study_configuration)
      req(input$dilution_da_selector)
      req(nrow(study_configuration) > 0)
      cat("in antigen selector")
      antigen_choices <- strsplit(study_configuration[study_configuration$param_name == "antigen_order",]$param_character_value, ",")[[1]]
      if (all(is.na(antigen_choices))) {
        #req(summary_gated_data_rv())
        node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]

        gated_data <- calculate_sample_concentration_status(study_accession = selected_study, experiment_accession = selected_experiment, node_order = node_order_in)

        antigen_choices <-  unique(gated_data$antigen)
      }

      selectInput("antigen_da_selector",
                  label = "Select Antigen",
                  choices = antigen_choices,
                  multiple = F)
    })

    ## Data
    # positive_controls_rv <- reactive({
    #   req(controls)
    #   req(input$antigen_da_selector)
    #   positive_controls <- controls[controls$antigen %in% input$antigen_da_selector,]
    #   return(positive_controls)
    #
    # })

    # buffer_data_filtered_rv <- reactive({
    #   req(buffer_data)
    #   req(input$antigen_da_selector)
    #   buffer_data_filtered <- buffer_data[buffer_data$antigen %in% input$antigen_da_selector,]
    #   return(buffer_data_filtered)
    # })

    std_curve_data_filtered_rv <- reactive({
      req(std_curve_data)
      req(input$antigen_da_selector)
      std_curve_data_filtered <- std_curve_data[std_curve_data$antigen %in% input$antigen_da_selector,]
      return(std_curve_data_filtered)
    })


    # Filter the classified  merged
    classified_merged_rv <- reactive({
      if (is.null(input$advanced_qc_component) || input$advanced_qc_component != "Dilution Analysis") {
        return(NULL)
      }

     # req(input$inLoadedData == "Dilution Analysis")
      # req(input$inLoadedData)
      # if (input$inLoadedData != "Dilution Analysis") {
      #   if (nrow(summary_gated_data_rv()) == 0 || is.null(summary_gated_data_rv())) {
      #     return(NULL)
      #   }
      #  # req(nrow(summary_gated_data_rv()) > 0)
      # }

      req(study_configuration)
     # req(summary_gated_data_rv())
     # req(nrow(summary_gated_data_rv()) > 0)
      req(input$dilution_da_selector)

      node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]

      gated_data <- calculate_sample_concentration_status(study_accession = selected_study, experiment_accession = selected_experiment, node_order = node_order_in)
      if (nrow(gated_data) == 0) {
        return(NULL)
      }
       da_subjtime <- compute_classified_merged_update(
        classified_sample = gated_data,
        selectedDilutions = input$dilution_da_selector,
        study_configuration = study_configuration
      )
      cat("Names of da_subjtime")
      print(names(da_subjtime))

     # print(table(da_subjtime$n_pass_d, da_subjtime$au_treatment))
      clm <- da_subjtime # Download this
      return(da_subjtime)

    })


    antigen_margin_table <- reactive({
      # if (is.null(input$inLoadedData) || input$inLoadedData != "Dilution Analysis") {
      #   return(NULL)
      # }
    #   req(input$inLoadedData)
    # #  req(input$inLoadedData == "Dilution Analysis") # Only reload when on dilution analysis tab
    #   if (input$inLoadedData != "Dilution Analysis") {
    #     return(NULL)
    #   }
      req(classified_merged_rv())
      req(input$antigen_da_selector)
      req(input$dilution_da_selector)
      req(study_configuration)
      db_time_order <- strsplit(study_configuration[study_configuration$param_name == "timeperiod_order",]$param_character_value, ",")[[1]]
      cat("merged table reactive names")
      print(names(classified_merged_rv()))
     #if (is.null(updated_classified_merged_rv())) {
        margin_table <- produce_margin_table(classified_merged = classified_merged_rv() , selectedAntigen = input$antigen_da_selector,
                                             selectedDilutions = input$dilution_da_selector,
                                             time_order = db_time_order)


        return(margin_table)
      # } else {
      #   return(updated_margin_antigen_rv())
      # }
   #   return(margin_table)
    })
   #
    output$margin_table_antigen <- renderDataTable({
      req(antigen_margin_table())

      timeperiod_colors <- names(antigen_margin_table())[grepl(pattern = "color", names(antigen_margin_table()))]
      dt <- DT::datatable(antigen_margin_table(),
                          caption = 'Number of Subjects Passing Classification by Timepoint, Concentration Status, and Number of Passing Dilutions',
                          selection = list(target = 'cell', mode = "single"),
                          rownames = F, # all numbers in JS are -1 then before.
                          options = list(columnDefs = list(list(visible=FALSE, targets= timeperiod_colors)),
                                         rowCallback = JS(
                                           'function(row, data, index) {
            // Disable the "Number of Pass Dilutions" column (index of column 1)
            $("td:nth-child(1)", row).css("background-color", "#f2f2f2");
            $("td:nth-child(1)", row).addClass("disabled");
            $("td:nth-child(1)", row).on("click", function(e) {
              e.stopPropagation();
              return false; // Disable click on first column
            });
            // Disable click on second column
            $("td:nth-child(2)", row).css("background-color", "#f2f2f2");
                    $("td:nth-child(2)", row).addClass("disabled");
                    $("td:nth-child(2)", row).on("click", function(e) {
                      e.stopPropagation();
                      return false; // Disable click on second column
                    });


            // Disable the row with "Total as Number of Passing Dilutions"
            if (data[0] === "Total") {
              $(row).find("td").css("background-color", "#f2f2f2");  // Optional: color for the row
              $(row).find("td").addClass("disabled");
              $(row).find("td").on("click", function(e) {
                e.stopPropagation();
                return false; // Disable click on the row
              });
            }
          }'
                                         ) # end RowCallback
                          )
      )

      id_cols <- c("Number of Passing Dilutions", "Concentration Status")
      #
      timeperiods <- setdiff(names(antigen_margin_table()), c(id_cols, timeperiod_colors))

      # match order of time periods to that in the table columns.
      timeperiod_colors <- timeperiod_colors[match(timeperiods, sub("_color$", "", timeperiod_colors))]

      for(i in 1:length(timeperiods)) {
        dil_col <- timeperiods[i]
        dil_color <- timeperiod_colors[i]

        dt <- dt %>% formatStyle(
          dil_col,
          valueColumns = dil_color,
          backgroundColor = styleEqual(c("all_au", "passing_au", "geom_all_au", "geom_passing_au", "replace_blank", "replace_positive_control",
                                         "exclude_au", "Blank"), c("#a1caf1", "lightgrey", '#c2b280', '#875692','#008856','#dcd300','#b3446c', 'white'))
        )
      }

      return(dt)
    })



    output$color_legend <- renderUI({
      div(
        style = "margin-bottom: 15px;",
        tags$b("Cell Color Legend:  "),

        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #a1caf1; border: 1px solid black; margin-right: 5px;"), "Keep all AU Measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: lightgrey; border: 1px solid black; margin: 0 5px 0 15px;"), "Keep Passing AU Measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #c2b280; border: 1px solid black; margin: 0 5px 0 15px;"), "Geometric Mean of all AU Measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #875692; border: 1px solid black; margin: 0 5px 0 15px;"), "Geometric Mean of passing AU Measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #008856; border: 1px solid black; margin: 0 5px 0 15px;"), "Replace AU Measurements with Geometric Mean of blank AUs",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: #b3446c; border: 1px solid black; margin: 0 5px 0 15px;"), "Exclude AU Measurements",
        tags$span(style = "display: inline-block; width: 15px; height: 15px; background-color: white; border: 1px solid black; margin: 0 5px 0 15px;"), "No Subjects Present"
      )
    })



    da_filter_module <- datamods::select_group_server(
      id = "da_filters",
      data = classified_merged_rv,
      vars = c("n_pass_dilutions", "concentration_status", "timeperiod"),
      selected_r = reactive(da_filters_rv$selected)
    )

    #
    filtered_classified_merged <- reactive({
      # if (is.null(input$inLoadedData) || input$inLoadedData != "Dilution Analysis") {
      #   return(NULL)
      # }
      # req(input$inLoadedData == "Dilution Analysis")
      # if (input$inLoadedData != "Dilution Analysis") {
      #   return(NULL)
      # }
      req(da_filter_module())
      req(input$antigen_da_selector)
      req(input$dilution_da_selector)
     #  selected <<- da_filter_module()

      df <- classified_merged_rv()
      df <- df[df$antigen == input$antigen_da_selector, ]
      df <- df[df$dilution %in% input$dilution_da_selector, ]


      if (!is.null(da_filter_module()$n_pass_dilutions)) {
        df <- df[df$n_pass_dilutions %in% unique(da_filter_module()$n_pass_dilutions), ]
      }
      if (!is.null(da_filter_module()$concentration_status)) {
        df <- df[df$concentration_status %in% unique(da_filter_module()$concentration_status), ]
      }
      if (!is.null(da_filter_module()$timeperiod)) {
        df <- df[df$timeperiod %in% unique(da_filter_module()$timeperiod), ]
      }
      return(df)

    })



   #
    output$passing_subject_selection <- renderUI({
    #  req(input$inLoadedData == "Dilution Analysis")
      req(input$antigen_da_selector)
      req(filtered_classified_merged())
      choices_string <- unique(filtered_classified_merged()$patientid)
      selectInput("Passing_subjects_da",
                  label = "Select Subjects",
                  choices = choices_string,
                  multiple = T,
                  selected = choices_string
      )

    })

    output$patient_dilution_series_plot <- renderPlotly({
    #  req(input$inLoadedData == "Dilution Analysis")
      req(filtered_classified_merged())
     # req(summary_gated_data_rv())
      req(input$antigen_da_selector)
      req(input$Passing_subjects_da)
      req(input$dilution_da_selector)
      cat("after requirements in plot\n\n")

      selected_timepoint <- as.character(unique(filtered_classified_merged()$timeperiod))
      selected_status <- unique(filtered_classified_merged()$concentration_status)

      print(selected_timepoint)
      print(selected_status)
      print(input$Passing_subjects_da)

      plot_patient_dilution_series(sample_data = filtered_classified_merged(),
                                   selectedAntigen =input$antigen_da_selector,
                                   selectedPatient = input$Passing_subjects_da,
                                   selectedTimepoints = selected_timepoint,
                                   selectedDilutions = input$dilution_da_selector)


    })

    output$final_average_au_table <- renderDT({
   #   req(input$inLoadedData == "Dilution Analysis")
      req(classified_merged_rv())
      req(study_configuration)
      req(dilution_table_cols)
      classified_au_data <- classified_merged_rv()
      classified_au_data$decision_nodes <- study_configuration[study_configuration$param_name == "node_order",]$param_character_value
      classified_au_data$bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value

      final_average_table <- data.frame()
      for (au_treatment in unique(classified_au_data$au_treatment)) {
        au_treatment_sub <- classified_au_data[classified_au_data$au_treatment == au_treatment,]
        if (au_treatment == "all_au") {
          average_au <- preserve_all_au(au_treatment_sub)
          # cat("\naverage_au:",au_treatment , "\n")
          # print(names(average_au))
        } else if (au_treatment == "passing_au") {
          average_au <- preserve_passing_au(au_treatment_sub)
          # cat("\naverage_au:",au_treatment , "\n")
          # print(names(average_au))
        } else if (au_treatment == "geom_all_au") {
          average_au <- geometric_mean_all_au_2(au_treatment_sub)
          # cat("\naverage_au:",au_treatment , "\n")
          # print(names(average_au))
        } else if (au_treatment == "geom_passing_au") {
          average_au <- geometric_mean_passing_au_2(au_treatment_sub)
          # cat("\naverage_au:",au_treatment , "\n")
          # print(names(average_au))
        } else if (au_treatment == "exclude_au") {
          average_au <- preserve_all_au(au_treatment_sub)
          # cat("\naverage_au:",au_treatment , "\n")
          # print(names(average_au))

        } else if (au_treatment == "replace_positive_control") {
          req(controls)
          positive_controls <- controls[controls$antigen %in% input$antigen_da_selector,]

          average_au <- geometric_mean_positive_controls(au_treatment_sub, positive_controls)
          # cat("\naverage_au:",au_treatment , "\n")
          # print(names(average_au))
        } else if (au_treatment == "replace_blank") {
          req(std_curve_data_filtered_rv())
          average_au <- geometric_mean_blanks(au_treatment_sub, std_curve_data_filtered_rv())
          # cat("\naverage_au:",au_treatment , "\n")
          # print(names(average_au))
        }
         cat("\naverage_au names\n")
         print(names(average_au))
        final_average_table <- rbind(final_average_table, average_au[, dilution_table_cols])
      }


      # replace dilution that is not all au with NA because of averaging
      final_average_table$dilution[!(final_average_table$au_treatment %in% c("all_au", "exclude_au"))] <- NA

      # when exclude au the final au is NA.
      final_average_table$final_au[final_average_table$au_treatment == "exclude_au"] <- NA

      final_average_table$plateid[final_average_table$au_treatment %in% c("geom_all_au", "geom_passing_au", "replace_positive_control", "replace_blank")] <- NA

      # Add type of sample limits used in classification
      final_average_table$decision_nodes <- study_configuration[study_configuration$param_name == "node_order",]$param_character_value

      #final_average_table_v <<- final_average_table
      #reorder and subset the columns for saving
     final_average_table <- final_average_table[, dilution_table_cols]
     final_average_au_table_rv(final_average_table)

      DT::datatable(final_average_table, caption = "Dilution Analysis Sample Output",
                    colnames = colnames(final_average_table), filter = "top")
    })


    observe({
     # req(input$inLoadedData == "Dilution Analysis")
      req(final_average_au_table_rv())
      req(dilution_table_cols)
      final_average_df <- final_average_au_table_rv()
      save_average_au(conn, final_average_df, dilution_table_cols)
    })


    output$download_classified_sample_UI <- renderUI({
      # if (is.null(input$inLoadedData) || input$inLoadedData != "Dilution Analysis") {
      #   return(NULL)
      # }
    #  req(input$inLoadedData == "Dilution Analysis")
      # if (input$inLoadedData != "Dilution Analysis") {
      #   return(NULL)
      # }
      req(classified_merged_rv())
      req(selected_study)
      req(selected_experiment)

      button_label <- paste0("Download Classified Sample Data: ", selected_experiment, "-", selected_study)

      downloadButton("download_classifed_sample_handle", button_label)
      # download_classified_sample(download_df = classified_merged_rv(),
      #                            selected_study = selected_study,
      #                            selected_experiment = selected_experiment)
    })

    output$download_classifed_sample_handle <-  downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "dilution_summary.csv", sep = "_")
      },
      content = function(file) {

        req(classified_merged_rv())
        req(selected_study)
        req(selected_experiment)

        # download data component (data frame)
        write.csv(classified_merged_rv(), file, row.names = FALSE)
      }
    )

    # output$download_average_au_table_UI <- renderUI({
    # #  req(input$inLoadedData == "Dilution Analysis")
    #   req(final_average_au_table_rv())
    #   req(selected_study)
    #   req(selected_experiment)
    #   download_processed_dilution_data(download_df = final_average_au_table_rv(),
    #                                    selected_study = selected_study,
    #                                    selected_experiment = selected_experiment)
    #
    # })

    output$download_average_au_table_UI <-  downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "_sample_data", ".csv", sep = "_")
      },
      content = function(file) {
        req(final_average_au_table_rv())
        req(selected_study)
        req(selected_experiment)
        write.csv(final_average_au_table_rv(), file, row.names = FALSE)
      }
    )

    ### Dilutional Linearity
    # output$da_lin_antigenUI <- renderUI({
    #     req(study_configuration)
    #     req(nrow(study_configuration) > 0)
    #     cat("in antigen lin selector")
    #     antigen_choices <- strsplit(study_configuration[study_configuration$param_name == "antigen_order",]$param_character_value, ",")[[1]]
    #     if (all(is.na(antigen_choices))) {
    #       node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]
    #
    #       gated_data <- calculate_sample_concentration_status(study_accession = selected_study, experiment_accession = selected_experiment, node_order = node_order_in)
    #
    #       antigen_choices <-  unique(gated_data$antigen)
    #     }
    #
    #     selectInput("antigen_da_lin_selector",
    #                 label = "Select Antigen",
    #                 choices = antigen_choices,
    #                 multiple = F)
    #   })
    #
    # output$response_selectionUI <- renderUI({
    #   selectInput(
    #     inputId = "dil_lin_response",
    #     label = "Response type",
    #     choices = c("Arbritary Units" = "au",
    #                 "MFI" = "mfi")
    #   )
    # })
    #
    # output$linear_correction_UI <- renderUI({
    #   checkboxInput(inputId = "apply_lm_corr",
    #                 label = "Apply Linear Correction",
    #                 value = T)
    # })
    #
    # output$exclude_concentrated_samples_UI <- renderUI({
    #   checkboxInput(inputId = "exclude_conc_samples",
    #                 label = "Exclude Concentrated Samples from Model Fittting",
    #                 value = F)
    # })
    #
    # plate_lm_facets <- reactive({
    #   req(input$qc_component == "Dilutional Linearity")
    #   req(selected_study)
    #   req(selected_experiment)
    #   req(input$dil_lin_response)
    #   #req(is_log_mfi_axis)
    #   req(input$antigen_da_lin_selector)
    #
    #   distinct_samples <- prepare_lm_sample_data(
    #     study_accession = selected_study,
    #     experiment_accession = selected_experiment,
    #     is_log_mfi_axis = is_log_mfi_axis,
    #     response_type = input$dil_lin_response
    #   )
    #
    #
    #
    #   dil_lin_regress_list <- dil_lin_regress(
    #     distinct_samples,
    #     response_type = input$dil_lin_response,
    #     exclude_conc_samples = input$exclude_conc_samples
    #   )
    #
    #
    #
    #  produce_all_plate_facets(
    #     distinct_samples = distinct_samples,
    #     dil_lin_regress_list = dil_lin_regress_list,
    #     selected_antigen = input$antigen_da_lin_selector,
    #     is_dil_lin_corr = input$apply_lm_corr,
    #     response_type = input$dil_lin_response,
    #     is_log_mfi_axis = is_log_mfi_axis
    #   )
    #
    # })
    #
    # observe({
    #   req(plate_lm_facets())
    #   for (i in seq_along(plate_lm_facets())) {
    #     local({
    #       my_i <- i
    #       output[[paste0("facet_lm_plot_", my_i)]] <- renderPlotly({
    #         plate_lm_facets()[[my_i]]
    #       })
    #     })
    #   }
    # })
    #
    # output$facet_tabs_ui <- renderUI({
    #   req(plate_lm_facets())
    #   plot_list <- plate_lm_facets()
    #
    #   # Create only non-NULL tabPanels
    #   tab_list <- lapply(seq_along(plot_list), function(i) {
    #     if (!is.null(plot_list[[i]])) {
    #       tabPanel(
    #         title = paste("Plate", i),
    #         plotlyOutput(outputId = paste0("facet_lm_plot_", i), width = "75vw")
    #       )
    #     } else {
    #       NULL
    #     }
    #   })
    #
    #   # Remove NULLs
    #   tab_list <- tab_list[!sapply(tab_list, is.null)]
    #
    #
    #   do.call(tabsetPanel, tab_list)
    # })
    #
    #
    #
    # output$facet_model_glance <- renderTable({
    # req(input$qc_component == "Dilutional Linearity")
    #   req(selected_study)
    #   req(selected_experiment)
    #   req(input$dil_lin_response)
    #   #req(is_log_mfi_axis)
    #   req(input$antigen_da_lin_selector)
    #
    #
    #   distinct_samples <- prepare_lm_sample_data(study_accession = selected_study, experiment_accession = selected_experiment, is_log_mfi_axis = is_log_mfi_axis, response_type = input$dil_lin_response)
    #   dil_lin_regress_list <- dil_lin_regress(distinct_samples, response_type = input$dil_lin_response, exclude_conc_samples = input$exclude_conc_samples)
    #
    #   if (input$apply_lm_corr) {
    #     glance_df <- dil_lin_regress_list$model_corr_glance_df
    #   } else {
    #     glance_df <- dil_lin_regress_list$glance_uncorrect_df
    #   }
    #
    #   glance_df <- glance_df[glance_df$antigen == input$antigen_da_lin_selector,]
    #
    #   return(glance_df)
    # }, caption = "Antigen Model Fit Statistics",
    # caption.placement = getOption("xtable.caption.placement", "top"))
    #
    # # Download associated data for processed lm fits has corrected y value
    # output$download_processed_lm_fit_data <-  downloadHandler(
    #   filename = function() {
    #     paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "processed_lm_fit_data.csv", sep = "_")
    #   },
    #   content = function(file) {
    #     req(selected_study)
    #     req(selected_experiment)
    #     req(input$dil_lin_response)
    #     req(is_log_mfi_axis)
    #     req(input$antigen_da_lin_selector)
    #
    #     distinct_samples <- prepare_lm_sample_data(study_accession = selected_study, experiment_accession = selected_experiment, is_log_mfi_axis = is_log_mfi_axis, response_type = input$dil_lin_response)
    #     dil_lin_regress_list <- dil_lin_regress(distinct_samples, response_type = input$dil_lin_response, exclude_conc_samples = input$exclude_conc_samples)
    #     proccessed_lm_fit_data <- dil_lin_regress_list$observed_data
    #     # download data component (data frame)
    #     write.csv(proccessed_lm_fit_data, file)
    #   }
    # )
    #
    # output$download_model_uncorrected_glance <- downloadHandler(
    #   filename = function() {
    #     paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "lm_uncorrected_glance.csv", sep = "_")
    #   },
    #   content = function(file) {
    #     req(selected_study)
    #     req(selected_experiment)
    #     req(input$dil_lin_response)
    #     req(is_log_mfi_axis)
    #     req(input$antigen_da_selector)
    #
    #
    #     distinct_samples <- prepare_lm_sample_data(study_accession = selected_study, experiment_accession = selected_experiment, is_log_mfi_axis = is_log_mfi_axis, response_type = input$dil_lin_response)
    #     dil_lin_regress_list <- dil_lin_regress(distinct_samples, response_type = input$dil_lin_response, exclude_conc_samples = input$exclude_conc_samples)
    #
    #     glance_df <- dil_lin_regress_list$glance_uncorrect_df
    #
    #
    #     # download data component (data frame)
    #     write.csv(glance_df, file)
    #   }
    # )
    #
    # output$download_model_correction_glance <- downloadHandler(
    #   filename = function() {
    #     paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "lm_correction_glance.csv", sep = "_")
    #   },
    #   content = function(file) {
    #     req(selected_study)
    #     req(selected_experiment)
    #     req(input$dil_lin_response)
    #     req(is_log_mfi_axis)
    #     req(input$antigen_da_selector)
    #
    #
    #     distinct_samples <- prepare_lm_sample_data(study_accession = selected_study, experiment_accession = selected_experiment, is_log_mfi_axis = is_log_mfi_axis, response_type = input$dil_lin_response)
    #     dil_lin_regress_list <- dil_lin_regress(distinct_samples, response_type = input$dil_lin_response, exclude_conc_samples = input$exclude_conc_samples)
    #
    #     glance_df <- dil_lin_regress_list$model_corr_glance_df
    #
    #
    #     # download data component (data frame)
    #     write.csv(glance_df, file)
    #   }
    # )




}
  else {
    output$dilutionAnalysisUI <- NULL  # remove UI if not on the tab
  #  output$dilutionalLinearityUI <- NULL
    output$dilutionAnalysisUI <- NULL
    output$dilutionalLinearityUI <- NULL
    output$dilution_summary_barplot <- NULL
    output$download_classified_sample_UI <- NULL
    output$final_average_au_table <- NULL
    output$download_average_au_table_UI <- NULL
    output$patient_dilution_series_plot <- NULL
    output$passing_subject_selection <- NULL
    output$antigen_selector_UI <- NULL
    output$response_selectionUI <- NULL
    # output$plate_lm_selectionUI <- NULL
    output$linear_correction_UI <- NULL
    output$facet_lm_plot <- NULL
    output$facet_tabs_ui <- NULL
    output$facet_model_glance <- NULL
    output$download_processed_lm_fit_data <- NULL
    output$download_model_uncorrected_glance <- NULL
    output$download_model_correction_glance <- NULL
  }# end in the tab

}) # end in loaded data


# observeEvent(input$inLoadedData, {
#   if (!is.null(input$inLoadedData) && input$inLoadedData != "Dilution Analysis") {
#     message("Not on Dilution Analysis tab")
#
#     # clear outputs
#     output$dilutionAnalysisUI <- NULL
#     output$dilutionalLinearityUI <- NULL
#     output$dilution_summary_barplot <- NULL
#     output$download_classified_sample_UI <- NULL
#     output$final_average_au_table <- NULL
#     output$download_average_au_table_UI <- NULL
#     output$patient_dilution_series_plot <- NULL
#     output$passing_subject_selection <- NULL
#     output$antigen_selector_UI <- NULL
#     output$response_selectionUI <- NULL
#     # output$plate_lm_selectionUI <- NULL
#     output$linear_correction_UI <- NULL
#     output$facet_lm_plot <- NULL
#     output$facet_tabs_ui <- NULL
#     output$facet_model_glance <- NULL
#     output$download_processed_lm_fit_data <- NULL
#     output$download_model_uncorrected_glance <- NULL
#     output$download_model_correction_glance <- NULL
#      #output$download_model_glances <- NULL
#
#
#   }
# })

