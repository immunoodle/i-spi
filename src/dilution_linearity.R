# standard_curve_table_list <- list()
#
# data_summary <- list()
#
# observeEvent(input$inLoadedData, {
#   if (input$inLoadedData == "Dilution Analysis") {
#
#     selected_study <- selected_studyexpplate$study_accession
#     selected_experiment <- selected_studyexpplate$experiment_accession
#
#     # Load sample data
#     sample_data <- stored_plates_data$stored_sample
#     # Check if selected study, experiment, and sample data are available
#     if (!is.null(selected_study) && length(selected_study) > 0 &&
#         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
#         !is.null(sample_data) && length(sample_data) > 0){
#
#       # Filter sample data
#       sample_data$selected_str <- paste0(sample_data$study_accession, sample_data$experiment_accession)
#       sample_data <- sample_data[sample_data$selected_str == paste0(selected_study, selected_experiment), ]
#
#       # Summarize sample data
#       cat("Viewing sample dat in summary tab ")
#       print(names(sample_data))
#       print(table(sample_data$plateid))
#       print(table(sample_data$antigen))
#       cat("After summarizing sample data in summary tab")
#
#
#       # Rename columns
#
#       sample_data <- dplyr::rename(sample_data, arm_name = agroup)
#       sample_data <- dplyr::rename(sample_data, visit_name = timeperiod)
#
#
#       sample_data$subject_accession <- sample_data$patientid
#
#       sample_data <- dplyr::rename(sample_data, value_reported = mfi)
#
#       arm_choices <- unique(sample_data$arm_name)
#       visits <- unique(sample_data$timeperiod)
#
#     }
#
#     standard_data_curve <- stored_plates_data$stored_standard
#     if (!is.null(selected_study) && length(selected_study) > 0 &&
#         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
#         !is.null(standard_data_curve) && length(standard_data_curve) > 0){
#
#       # Filter sample data
#       standard_data_curve$selected_str <- paste0(standard_data_curve$study_accession, standard_data_curve$experiment_accession)
#       standard_data_curve <- standard_data_curve[standard_data_curve$selected_str == paste0(selected_study, selected_experiment), ]
#
#       # Summarize std curve data data
#       cat("View Standard Curve data plateid")
#       print(table(standard_data_curve$plateid))
#       cat("View Standard Curve data antigen")
#       print(table(standard_data_curve$antigen))
#
#       std_curve_data <- standard_data_curve
#
#       std_curve_data$subject_accession <- std_curve_data$patientid
#
#       std_curve_data <- calculate_log_dilution(std_curve_data)
#     }
#
#     # load controls
#     controls <- stored_plates_data$stored_control
#
#     # load buffer data
#     buffer_data <- stored_plates_data$stored_buffer
#     if (!is.null(selected_study) && length(selected_study) >0 &&
#         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
#         !is.null(buffer_data) && length(buffer_data) > 0) {
#
#       buffer_data$selected_str <- paste0(buffer_data$study_accession, buffer_data$experiment_accession)
#       buffer_data <- buffer_data[buffer_data$selected_str == paste0(selected_study, selected_experiment), ]
#     }
#
#     fitted_curve_parameters <- fetch_standard_curves_dilution(selected_study, selected_experiment, bkg_method = background_control_rv())
#
#     ## Obtain Dilution parameter df and extract parameters
#    study_dilution_parameters <- get_dilution_parameters(study_accession = selected_study)
#    node_order <- strsplit(study_dilution_parameters$node_order, ", ")[[1]]
#    sufficient_gc <- study_dilution_parameters$valid_gate_class
#    binary_gc <- study_dilution_parameters$is_binary_gate
#
#     output$dilutionalLinearity_UI <- renderUI({
#       tagList(
#         fluidRow(
#           column(12,
#                  bsCollapse(
#                    id = "dilutionAnalysisMethods",
#                    bsCollapsePanel(
#                      title = "Dilution Analysis Methods",
#                      tagList(
#                      tags$p("Use the dropdown menu to select decision nodes for the decision tree. The available options are Gate Class,
#                      In Linear Region, and In Quantifiable Range. The order in which decision nodes are selected matters as the tree branches
#                      according to that sequence. When gate class is included one can select if the gate class should be treated as categorical,
#                      using the actual gate class, or binary. In both cases, the user can select which gate classes are valid and will result in a
#                      pass classification from the decision from the  dropdown menu titled, ‘Valid Gate Classes’."),
#
#                      tags$p("Below the include decisions and valid gate class selections, Figure 1 is displayed.
#                      Figure 1 depicts the decision tree that is produced based on the decisions that are selected above.
#                      Since a decision tree structure is used, when there are multiple decision nodes selected samples will pass classification
#                      if they are in the path where all the nodes are true."),
#
#                      tags$p("Use ‘Select Antigen’ dropdown menu to select one or more antigens of interest to conduct dilution analysis on in the sample data.
#                      When the Dilution Analysis tab is loaded, all of the available dilutions are initially selected and shown in the ‘Select Dilutions’ dropdown menu.
#                      Dilutions can be manually excluded or included before running the dilution analysis by changing this selection."),
#
#                      tags$p("The dilution analysis for the selected antigens begins by following a decision tree structure.
#                             When a standard curve is saved, sample values are gated and have attributes of being in the linear range and in the quantifiable range of the standard curve or not.
#                             Using these characteristics, a decision tree can be made to classify the sample points as either passing classification or not passing classification. "),
#
#                      tags$p("Below Figure 1 lies Figure 2, a heatmap of the number of dilutions passing the classification determined by
#                      the decision tree by subject, time period and antigen. All timepoints in the sample data are used and the antigen visible is controlled by the
#                      'Select Antigen' dropdown menu. The heatmap displays time period and antigen on the x-axis and subjects on the y-axis. "),
#
#                       tags$p("Following Figure 2 is Table 1, titled 'Number of Subjects Passing Classification by Timepoint, Concentration Status, and Number of Passing Dilutions'.
#                       This contingency table lists the number of passing dilutions in the first column, with the number of subjects
#                              having that number of passing dilutions across the rows at each timepoint in the experiment for each concentration status.
#                              To view which subjects have a particular number of passing dilutions and concentration status at a particular time point,
#                              either double click a cell in the table or select a time point, concentration status, number of dilutions from the dropdown menus below the table titled ‘Number of Passing Dilutions’ ,
#                              'Select Concentration Status', and 'Select Timepoint' respectively. The color of each cell with patient counts represents how the arbitrary units are treated in the final calculation of the
#                              analysis and correspond to the options which are colored in the radio button titled ‘Arbitrary Unit (AU) Treatment.
#                              The current options include keeping all AU measurements, keeping AU measurements that are considered passing,
#                              taking the geometric mean of all AU measurements, taking the geometric mean of passing AU measurements,
#                              replacing AU measurements with the geometric mean of blank AUs and excluding the AU measurements.
#                              To change the AU treatment of a cell choose the desired AU treatment for the number of passing dilutions, concentration status, and time point of interest. "),
#
#                      tags$p("Figure 3 is displayed when the subjects who are passing are populated in the passing subjects selector.
#                      Figure 3 depicts the plate dilution series, with log10 plate dilution on the x-axis and arbitrary units on the y-axis.
#                      All of the subjects who are passing are selected by default and each subject's data is represented by its own line.
#                      The color of the points indicates whether the dilution passes classification: blue for passing and red for not passing. "),
#
#                      tags$p("Table 2, entitled 'Dilution Analysis Sample Output' displays the sample data for all timepoints for the selected antigen and includes a row for how the
#                      final AU is calculated (au_treatment) which corresponds to the color of the cell, the decision nodes used in the decision tree (decision_nodes),
#                      the background method which is selected from study overview in the standard curve options and the final au (final_au)."),
#
#                      tags$p("Table 2 can be saved to the database by clicking on the button 'Save Dilution Analysis'.
#                      To download the classified sample data for the selected antigen in the selected experiment click the first button and to download the processed dilution data click on the button 'Download Processed Dilution Data'.")
#
#                      ), # end tagList
#                      style = "success"
#                    )# end bsCollapsePanel
#                    ), # end bsCollapse
#           mainPanel(
#
#             ## Decision Tree
#             fluidRow(column(4, uiOutput("node_order_selection")),
#                      column(4,uiOutput("gate_class_binary")),
#                      column(4, uiOutput("sufficent_gc_vector_UI"))
#             ),
#             fluidRow(
#               grVizOutput("decision_tree", width = "75vw")),
#
#             uiOutput("linear_message"),
#             uiOutput("quantifiable_message"),
#             uiOutput("gate_message"),
#
#
#
#             br(),
#             uiOutput("antigen_family_importance_order"),
#             br(),
#             plotlyOutput("dilution_summary_barplot", width = "75vw"),
#             br(),
#             uiOutput("download_dilution_contigency_summary"),
#             br(),
#
#             fluidRow(
#               column(6, uiOutput("sample_dilutions_selection")),
#               column(6, uiOutput("antigen_select_linearity"))),
#
#            # Heatmap
#             fluidRow(
#               br(),
#             #  plotlyOutput("passing_dilutions_heatmap", width = "75vw"),
#
#               conditionalPanel(
#                 condition = "input.antigenSelectionLinearity != null && input.antigenSelectionLinearity != ''",
#               div(style = "overflow-x: auto; width: 75vw;",
#                   dataTableOutput("passing_subject_table"))
#               ),
#               ),
#
#
#            # fluidRow(column(4,uiOutput("select_n_pass_dilution"))),
#              #          column(4, uiOutput("concentration_status")),
#              #          column(4,uiOutput("sample_timepoint_selection"))),
#            # selectizeGroupUI(
#            #   id = "my_filters",
#            #   params = list(
#            #     n_pass_dilutions = list(inputId = "n_pass_dilutions", title = "Number of Passing Dilutions"),
#            #     status = list(inputId = "concentration_status_selector", title = "Select Concentration Status", multiple = F),
#            #     timeperiod = list(inputId = "timepointSelectionLinearity", title = "Select Timepoint", multiple = F)
#            #   )
#            # ),
#           conditionalPanel(
#             condition = "input.antigenSelectionLinearity != null && input.antigenSelectionLinearity != ''",
#            select_group_ui(
#              id = "da_filters",
#              params = list(list(inputId = "n_pass_dilutions", label = "Number of Passing Dilutions", multiple = F),
#                            list(inputId = "status", label = "Select Concentration Status", multiple = F),
#                            timeperiod = list(inputId = "timeperiod", label = "Select Timepoint", multiple = F)
#              )
#            )
#            ),
#
#          # tableOutput("filtered_data"),
#
#            fluidRow(column(6, uiOutput("au_treatment_options_UI"),
#                            uiOutput("passing_subject_selection"))),
#
#             ### AU dilution plot
#             fluidRow(
#               plotlyOutput("dilutional_linearity_plot", width = "75vw"),
#               br(),
#               #uiOutput("average_au"),
#               div(style = "overflow-x: auto; width: 75vw;",
#                   dataTableOutput("average_au_table")),
#
#               br(),
#               uiOutput("saveDilutionAnalysis"),
#               br(),
#               uiOutput("download_classified_samples"),
#               br(),
#               uiOutput("download_sample_dilution_linearity"),
#             )
#
#           )
#         )
#       )
#       )
#     })
#
#     # Summary data for contingency table with dilutions
#     summary_gated_data_rv <- reactive({
#       req(input$readxMap_study_accession)
#       req(input$readxMap_experiment_accession)
#       req(input$node_order)
#       req(input$sufficient_gc)
#       req(background_control_rv())
#       joined_data <- join_sample_standard_data(study_accession = input$readxMap_study_accession,
#                                 experiment_accession = input$readxMap_experiment_accession,
#                                 bkg_method = background_control_rv(),
#                                 node_order = input$node_order,
#                                 valid_gate_class = input$sufficient_gc )
#
#       return(joined_data)
#     })
#     # summary_gated_data_rv <- reactive({
#     #   req(input$readxMap_study_accession)
#     #   req(input$readxMap_experiment_accession)
#     #   calculate_sample_concentration_status(study_accession = input$readxMap_study_accession,
#     #                                         experiment_accession = input$readxMap_experiment_accession,
#     #                                         node_order = )
#     # })
#
#     # summary datatable
#     output$dilution_contigency_summary  <- renderDataTable({
#       req(summary_gated_data_rv())
#   #    req("linear" %in% input$node_order)
#       contigency_summary_dilution <-  produce_contigency_summary(summary_gated_data_rv())
#
#       dt <- DT::datatable(contigency_summary_dilution,
#                           caption = 'Dilution Concentration Status Summary',
#                           filter = "top"
#                           #selection = list(target = 'cell', mode = "single")
#       )
#
#       return(dt)
#     })
#
#     output$download_dilution_contigency_summary <- renderUI({
#       req(input$readxMap_study_accession)
#       req(input$readxMap_experiment_accession)
#       req(summary_gated_data_rv())
#       contigency_summary_dilution <- produce_contigency_summary(summary_gated_data_rv())
#       download_dilution_contigency_summary_fun(download_df = contigency_summary_dilution,
#                                   selected_study = input$readxMap_study_accession,
#                                   selected_experiment = input$readxMap_experiment_accession)
#     })
#
#     # retrieve the antigen families
#     study_antigen_family_df <- reactive({
#       req(input$readxMap_study_accession)
#       antigen_families <- fetch_antigen_family_df(study_accession = input$readxMap_study_accession)
#       return(antigen_families)
#     })
#
#     output$antigen_family_importance_order <- renderUI({
#       req(summary_gated_data_rv()) # require this to show in the UI at the appropriate time.
#       req(study_antigen_family_df())
#       orderInput(inputId = 'antigen_family_order', label = 'Move antigen families by importance level',
#                  items = unique(study_antigen_family_df()$antigen_family))
#
#     })
#
#
#     # Summary figure
#     output$dilution_summary_barplot <- renderPlotly({
#       req(input$readxMap_study_accession)
#       req(input$readxMap_experiment_accession)
#       req(study_antigen_family_df())
#       req(summary_gated_data_rv())
#       contigency_summary_dilution <- produce_contigency_summary(summary_gated_data_rv())
#       summary_dilution_plot(dilution_summary_df = contigency_summary_dilution,
#                             antigen_families = study_antigen_family_df(),
#                             antigen_family_order = input$antigen_family_order)
#
#     })
#
#
#
#    # Sample data to use in plot, filter out AUs which are NA
#     sample_dilution_linearity_rv <- reactive({
#       req(input$readxMap_study_accession)
#       req(input$readxMap_experiment_accession)
#       req(input$node_order)
#       req(fitted_curve_parameters)
#
#       # clear reset
#       output$linear_message <- renderUI({
#         NULL
#         })
#       output$quantifiable_message <- renderUI({ NULL })
#       output$gate_message <- renderUI({ NULL })
#
#       sample_dilution_linearity <- fetch_sample_data_linearity(study_accession = input$readxMap_study_accession, experiment_accession = input$readxMap_experiment_accession)
#       sample_dilution_linearity <- sample_dilution_linearity[!is.na(sample_dilution_linearity$au), ]
#       # clean up and add plateid
#       sample_dilution_linearity$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", sample_dilution_linearity$plate_id, fixed=TRUE)))
#
#       #sample_dilution_linearity_view <<- sample_dilution_linearity
#       # remove duplicate entries if exist.
#       fitted_curve_parameters_v <- distinct(fitted_curve_parameters[, c("study_accession", "experiment_accession", "plateid", "antigen",
#                                                              "bendlower", "bendupper", "llod", "ulod", "lloq", "uloq")])
#       fit_params <- distinct(fitted_curve_parameters[, c("study_accession", "experiment_accession", "plateid", "antigen")])
#       #fitted_curve_parameters <<- fitted_curve_parameters[!duplicated(fitted_curve_parameters),] #[, c("study_accession", "experiment_accession", "plateid", "antigen")]), ]
#       # Left join the fitted data to the sample data
#        sample_dilution_linearity_rv_merged  <- merge(sample_dilution_linearity, fitted_curve_parameters_v[, c("study_accession", "experiment_accession", "plateid", "antigen", "bendlower", "bendupper", "llod", "ulod", "lloq", "uloq")],
#                            by = c("study_accession", "experiment_accession", "plateid", "antigen"), all.x = TRUE)
#
#     #   sample_dilution_linearity_rv_merged_1 <<- sample_dilution_linearity_rv_merged
#      # cat("after merge in the fit data")
#       if ("linear" %in% input$node_order) {
#         sample_dilution_linearity_rv_merged <- sample_dilution_linearity_rv_merged[!is.na(sample_dilution_linearity_rv_merged$in_linear_region), ]
#         #sample_dilution_linearity_rv_merged <- sample_dilution_linearity_rv_merged[!is.na(sample_dilution_linearity_rv_merged$bendlower) & !is.na(sample_dilution_linearity_rv_merged$bendupper), ]
#         if (nrow(sample_dilution_linearity_rv_merged) == 0) {
#         output$linear_message <- renderUI({
#           req(input$node_order)
#           HTML(paste("<span style='font-size: 1.5em;'> No samples found for in the in the linear region for ",input$readxMap_experiment_accession, " in ",
#                      selected_study, " using the ", background_control_rv(), " method for the blanks."))
#         })
#         } else {
#           output$linear_message <- renderUI({
#            NULL
#           })
#         }
#       }
#       if ("quantifiable" %in% input$node_order) {
#         sample_dilution_linearity_rv_merged <- sample_dilution_linearity_rv_merged[!is.na(sample_dilution_linearity_rv_merged$in_quantifiable_range), ]
#        # sample_dilution_linearity_rv_merged <- sample_dilution_linearity_rv_merged[!is.na(sample_dilution_linearity_rv_merged$lloq) & !is.na(sample_dilution_linearity_rv_merged$uloq),, ]
#
#         if (nrow(sample_dilution_linearity_rv_merged) == 0) {
#           output$quantifiable_message <- renderUI({
#             req(input$node_order)
#             HTML(paste("<span style='font-size: 1.5em;'> No samples found for in the in the quantifiable range for ",input$readxMap_experiment_accession, " in ",
#                        selected_study," using the ", background_control_rv(), " method for the blanks."))
#           })
#         } else {
#           output$quantifiable_message <- renderUI({
#             NULL
#           })
#         }
#       }
#        if ("gate" %in% input$node_order) {
#          # sample_dilution_linearity_rv_merged <- sample_dilution_linearity_rv_merged[!is.na(sample_dilution_linearity_rv_merged$in_quantifiable_range), ]
#          sample_dilution_linearity_rv_merged <- sample_dilution_linearity_rv_merged[!is.na(sample_dilution_linearity_rv_merged$llod) & !is.na(sample_dilution_linearity_rv_merged$ulod),, ]
#
#          if (nrow(sample_dilution_linearity_rv_merged) == 0) {
#            output$gate_message <- renderUI({
#              req(input$node_order)
#              HTML(paste("<span style='font-size: 1.5em;'> No samples found within the limits of detection for ",input$readxMap_experiment_accession, " in ",
#                         selected_study," using the ", background_control_rv(), " method for the blanks."))
#            })
#          } else {
#            output$gate_message <- renderUI({
#              NULL
#            })
#          }
#        }
#
#      # sample_dilution_linearity_rv_merged_v <<- sample_dilution_linearity_rv_merged
#       return(sample_dilution_linearity_rv_merged)
#      })
#
#
#     # observeEvent(input$node_order, {
#     # #  req(sample_dilution_linearity_rv())
#     #   df <- sample_dilution_linearity_rv()
#     #
#     #   if ("linear" %in% input$node_order) {
#     #     df <- df[!is.na(df$in_linear_region), ]
#     #     if (nrow(df) == 0) {
#     #       output$linear_message <- renderUI({
#     #         HTML(paste("<span style='font-size: 1.5em;'> No samples found in the linear region for ",
#     #                    input$readxMap_experiment_accession, " in ",
#     #                    selected_study, " using the ", background_control_rv(), " method for the blanks.</span>"))
#     #       })
#     #     } else {
#     #       output$linear_message <- renderUI(NULL)
#     #     }
#     #   } else {
#     #     output$linear_message <- renderUI(NULL)
#     #   }
#     #
#     #   if ("quantifiable" %in% input$node_order) {
#     #     df <- df[!is.na(df$in_quantifiable_range), ]
#     #     if (nrow(df) == 0) {
#     #       output$quantifiable_message <- renderUI({
#     #         HTML(paste("<span style='font-size: 1.5em;'> No samples found in the quantifiable range for ",
#     #                    input$readxMap_experiment_accession, " in ",
#     #                    selected_study, " using the ", background_control_rv(), " method for the blanks.</span>"))
#     #       })
#     #     } else {
#     #       output$quantifiable_message <- renderUI(NULL)
#     #     }
#     #   } else {
#     #     output$quantifiable_message <- renderUI(NULL)
#     #   }
#     #
#     #
#     # })
#     #
#
#     # Select one or more antigens from the data
#     output$antigen_select_linearity <- renderUI({
#      req(sample_dilution_linearity_rv())
#      req(input$dilutionsSelectionLinearity)
#      req(nrow(sample_dilution_linearity_rv()) > 0)
#       selectInput("antigenSelectionLinearity",
#                               label = "Select Antigen",
#                               choices = unique(sample_dilution_linearity_rv()$antigen),
#                               multiple = T)
#   })
#
#
#    # Patient Selection
#    output$sample_patient_selection <- renderUI({
#      req(sample_dilution_linearity_rv())
#      req(nrow(sample_dilution_linearity_rv()) > 0)
#      selectInput("patientSelectionLinearity",
#                                    label = "Select Subject",
#                                    choices = unique(sample_dilution_linearity_rv()$patientid),
#                                    multiple = T)
#
#    })
#
#
#    # Dilution Selection
#    output$sample_dilutions_selection <- renderUI({
#      req(sample_dilution_linearity_rv())
#      req(nrow(sample_dilution_linearity_rv()) > 0)
#      #req(input$antigenSelectionLinearity)
#      dat_dilutions <- sample_dilution_linearity_rv()
#      # dat_dilutions <- dat_dilutions[dat_dilutions$dilution %in% input$antigenSelectionLinearity,]
#      selectInput("dilutionsSelectionLinearity",
#                                    label = "Select Dilutions",
#                                    choices = unique(dat_dilutions$dilution),
#                                    multiple = T,
#                                    # include all dilutions initially
#                                    selected = unique(dat_dilutions$dilution))
#    })
#
#    ### Decision Tree
#
#     # Node order
#     output$node_order_selection <- renderUI({
#       selectInput("node_order",
#                   label = "Include decisions",
#                   choices = c("Gate Class" = "gate",
#                               "In Linear Region" = "linear",
#                               "In Quantifiable Range" = "quantifiable"),
#                   multiple = T,
#                   selected = "gate")
#     })
#
#     ## Choose control of if want to treat it gate class as binary or not
#     output$gate_class_binary <- renderUI({
#       req("gate" %in% input$node_order)
#       checkboxInput("binary_gc", "Use Gate Class as binary", value = FALSE)
#     })
#
#
#     ## Sufficient gate class vector
#     output$sufficent_gc_vector_UI <- renderUI({
#       #req(input$binary_gc == F)
#       selectInput("sufficient_gc",
#                   label = "Valid Gate Classes",
#                   choices = c("Between Limits of Detection" = "Between_Limits",
#                               "Above Upper Limit of Detection" = "Above_Upper_Limit",
#                               "Below Lower Limit of Detection" = "Below_Lower_Limit"),
#                   multiple = T,
#                   selected = "Between_Limits")
#     })
#
#     # Truth table reactive
#     truth_table_reactive <- reactive({
#       #req(input$binary_gc)
#
#       create_truth_table(binary_gate = input$binary_gc,
#                          exclude_linear = F,
#                          exclude_quantifiable = F,
#                          exclude_gate = F)
#     })
#
#     output$valid_gc <- renderUI({
#       req(input$sufficient_gc)
#       tags$ul(
#         lapply(input$sufficient_gc, function(x) {
#           tags$li(x)  # Create list items for each element of the vector
#         })
#       )
#
#     })
#
#
#     decision_tree_reactive <- reactive({
#     #  req(input$binary_gc)
#      req(input$node_order)
#
#       # Get the truth table from the truth_table_reactive expression
#       truth_table <- truth_table_reactive()
#
#       if (input$binary_gc == F) {
#         cat("FALSE binary")
#        # req(input$sufficient_gc)
#         decision_tree <- create_decision_tree_tt(truth_table = truth_table,
#                                                  binary_gate = input$binary_gc,
#                                                  sufficient_gc_vector = input$sufficient_gc,
#                                                  node_order = input$node_order)
#       } else {
#         decision_tree <- create_decision_tree_tt(truth_table = truth_table,
#                                                  binary_gate = input$binary_gc,
#                                                  sufficient_gc_vector = c(),
#                                                  node_order = input$node_order)
#       }
#
#       decision_tree
#     })
#
#
#     ## Decision Tree Plot
#     output$decision_tree <- renderGrViz({
#       req(decision_tree_reactive())
#
#       decision_tree <- decision_tree_reactive()
#
#       dot_string <- paste(
#         "digraph tree {",
#         paste(get_edges(decision_tree), collapse = "; "),
#         "}",
#         sep = "\n"
#       )
#       grViz(dot_string)
#       #decision_tree$ToDiagrammeRGraph()
#     })
#
#
#     output$passing_dilutions_heatmap <- renderPlotly({
#       req(decision_tree_reactive())
#       req(sample_dilution_linearity_rv())
#       req(input$dilutionsSelectionLinearity)
#       req(input$antigenSelectionLinearity)
#
#       sample_data <- sample_dilution_linearity_rv()
#
#       paths <- get_leaf_path(decision_tree_reactive())
#       parsed_data <- do.call(rbind, lapply(paths, parse_leaf_path, binary_gc = input$binary_gc,
#                                            sufficient_gc_vector = input$sufficient_gc))
#       # Classify the sample
#       classified_sample <- classify_sample(sample_data = sample_data, parsed_classification = parsed_data)
#       heatmap <- plot_classification_heatmap(classified_sample = classified_sample, selectedAntigens = input$antigenSelectionLinearity,
#                                              selectedDilutions = input$dilutionsSelectionLinearity)
#       return(heatmap)
#
#     })
#
#     ## Reactive Passing contingency table
#     contingency_table_summary  <- reactive({
#       req(decision_tree_reactive())
#       req(sample_dilution_linearity_rv())
#       req(input$antigenSelectionLinearity)
#
#       sample_data <- sample_dilution_linearity_rv()
#
#       paths <- get_leaf_path(decision_tree_reactive())
#       parsed_data <- do.call(rbind, lapply(paths, parse_leaf_path, binary_gc = input$binary_gc,
#                                            sufficient_gc_vector = input$sufficient_gc))
#       # Classify the sample
#       classified_sample <- classify_sample(sample_data = sample_data, parsed_classification = parsed_data)
#
#       contigency_table <- obtain_passing_subject_contigency(classified_sample = classified_sample, selectedAntigens = input$antigenSelectionLinearity)
#
#       return(contigency_table)
#     })
#
#     # contingency margin table
#
#     output$passing_subject_table  <- renderDataTable({
#       req(margin_table_rv())
#
#       timeperiod_colors <- names(margin_table_rv())[grepl(pattern = "color", names(margin_table_rv()))]
#       dt <- DT::datatable(margin_table_rv(),
#                           caption = 'Number of Subjects Passing Classification by Timepoint, Concentration Status, and Number of Passing Dilutions',
#                           selection = list(target = 'cell', mode = "single"),
#                           rownames = F, # all numbers in JS are -1 then before.
#                           options = list(columnDefs = list(list(visible=FALSE, targets= timeperiod_colors)),
#                                          rowCallback = JS(
#                                            'function(row, data, index) {
#             // Disable the "Number of Pass Dilutions" column (index of column 1)
#             $("td:nth-child(1)", row).css("background-color", "#f2f2f2");
#             $("td:nth-child(1)", row).addClass("disabled");
#             $("td:nth-child(1)", row).on("click", function(e) {
#               e.stopPropagation();
#               return false; // Disable click on first column
#             });
#             // Disable click on second column
#             $("td:nth-child(2)", row).css("background-color", "#f2f2f2");
#                     $("td:nth-child(2)", row).addClass("disabled");
#                     $("td:nth-child(2)", row).on("click", function(e) {
#                       e.stopPropagation();
#                       return false; // Disable click on second column
#                     });
#
#
#             // Disable the row with "Sum as Number of Passing Dilutions"
#             if (data[0] === "Sum") {
#               $(row).find("td").css("background-color", "#f2f2f2");  // Optional: color for the row
#               $(row).find("td").addClass("disabled");
#               $(row).find("td").on("click", function(e) {
#                 e.stopPropagation();
#                 return false; // Disable click on the row
#               });
#             }
#           }'
#                                          ) # end RowCallback
#                           )
#       )
#
#       id_cols <- c("Number of Passing Dilutions", "concentration_status")
#
#       timeperiods <- setdiff(names(margin_table_rv()), c(id_cols, timeperiod_colors))
#
#       # match order of time periods to that in the table columns.
#       timeperiod_colors <- timeperiod_colors[match(timeperiods, sub("_color$", "", timeperiod_colors))]
#
#       for(i in 1:length(timeperiods)) {
#         dil_col <- timeperiods[i]
#         dil_color <- timeperiod_colors[i]
#
#         dt <- dt %>% formatStyle(
#           dil_col,
#           valueColumns = dil_color,
#           backgroundColor = styleEqual(c("all_au", "passing_au", "geom_all_au", "geom_passing_au", "replace_blank", "replace_positive_control",
#                                          "exclude_au", "Blank"), c("#a1caf1", "lightgrey", '#c2b280', '#875692','#008856','#dcd300','#b3446c', 'white'))
#         )
#       }
#
#       return(dt)
#     })
#
#
#
#
#     observeEvent(input$passing_subject_table_cell_clicked, {
#    #   req(input$passing_subject_table_cell_clicked$row)  # Ensure a row is selected
#       #req(input$passing_subject_table_cell_clicked$col)  # Ensure a column is selected
#       req(margin_table_rv())  # Ensure the contingency table is available
#
#
#       # Extract contingency table data
#       contingency_table <- margin_table_rv()
#
#       # Exclude the "Sum" row safely
#       valid_rows <- contingency_table$`Number of Passing Dilutions` != "Sum"
#       num_pass_dilutions <- as.numeric(contingency_table$`Number of Passing Dilutions`[valid_rows])
#
#       filtered_table <- contingency_table[valid_rows, , drop = FALSE]
#
#       row_index <- input$passing_subject_table_cell_clicked$row
#       col_index <- input$passing_subject_table_cell_clicked$col
#       # Ensure the row_index maps correctly to the filtered data
#        # if (row_index > nrow(filtered_table) -1 || row_index < 1) return()
#        # if (col_index > ncol(filtered_table) -1 || col_index < 1) return()
#
#       selected_pass_dilution <- as.numeric(filtered_table[row_index, "Number of Passing Dilutions"])
#
#       selected_concentrated_status <- contingency_table[row_index,]$concentration_status
#
#     #  Extract the list of time points
#       timepoints <- setdiff(colnames(filtered_table), c("Number of Passing Dilutions", "concentration_status"))
#       selected_color <-  paste0(timepoints, "_color")
#       timepoints <- setdiff(timepoints, selected_color)
#       # Identify the selected timepoint based on the clicked column
#       selected_timepoint <- colnames(filtered_table)[col_index]
#
#
#
#       da_filters_rv$selected <- list(n_pass_dilutions = selected_pass_dilution,
#                                      status = selected_concentrated_status,
#                                      timeperiod = selected_timepoint)
#
#       cat("After click update")
#       print(da_filters_rv$selected)
#
#       cat("\nrow")
#       if (!is.null(input$passing_subject_table_cell_clicked$row)) {
#         ## re-filter the data after clicking
#         filtered_classified_merged <- reactive({
#         req(da_filters_rv$selected)
#         classified_merged <-  classified_merged_rv()
#
#          classified_merged[classified_merged$n_pass_dilutions == da_filters_rv$selected$n_pass_dilutions &
#                                                                    classified_merged$status == da_filters_rv$selected$status &
#                                                                    classified_merged$timeperiod == da_filters_rv$selected$timeperiod,]
#
#
#         })
#       }
#       # Ensure row index is within bounds
#       #if (row_index > length(num_pass_dilutions) || row_index < 1) return()
#
#
#
#       # ns <- NS("da_filters")
#       # # num_pass_dilutions <<- unique(num_pass_dilutions)
#       # # Update the select input for passing dilutions
#       # updateSelectInput(session = session,
#       #                   inputId = ns("n_pass_dilutions"),
#       #                   label = "Number of Passing Dilutions",
#       #                   choices = unique(num_pass_dilutions),
#       #                   selected = selected_pass_dilution)# Select corresponding value
#
#       # updateVirtualSelect(
#       #   inputId = "n_pass_dilutions",
#       #   selected = current_pass_dilution
#       # )
#
#
#
#       # updateVirtualSelect(
#       #   inputId = "timeperiod",
#       #   selected = selected_timepoint
#       # )
#       # Update the select input for timepoints dynamically
#       # updateSelectInput(session = session,
#       #                   inputId = ns("timeperiod"),
#       #                   label = "Select Timepoint",
#       #                   choices = timepoints,
#       #                   selected = selected_timepoint)  # Pre-select the current timepoint
#       #
#       # updateSelectInput(session = session,
#       #                   inputId = ns("status"),
#       #                   label = "Select Concentration Status",
#       #                   choices = setdiff(unique(contingency_table$concentration_status), "Sum"),
#       #                   selected = selected_concentrated_status)
#
#       selected_row <- contingency_table[row_index, ]
#       selected_color <-  paste0(selected_timepoint, "_color")
#       updated_treatment <- selected_row[[selected_color]] #selected_row$selected_color
#
#      # if (updated_treatment != "Blank") {
#       # update the select input for au_treatment
#       updateRadioGroupButtons(session = session,
#                         inputId = "au_treatment",
#                         label = "Arbitrary Unit (AU) Treatment",
#                         choices = c("Keep all AU measurements" = "all_au",
#                                     "Keep passing AU measurements" = "passing_au",
#                                     "Geometric mean of all AU measurements" = "geom_all_au",
#                                     "Geometric mean of passing AU measurements" = "geom_passing_au",
#                                     "Replace AU measurements with geometric mean of blank AUs" = "replace_blank",
#                                    # "Replace AU measurements with geometric mean of positive control AUs" = "replace_positive_control",
#                                     "Exclude AU measurements" = "exclude_au"
#                         ), # end choices
#                         checkIcon = list(yes = icon("check")),
#                         justified = F,
#                         selected = updated_treatment)
#
#       # } else {
#       #   updateRadioGroupButtons(session = session,
#       #                          inputId = "au_treatment",
#       #                          label = "Arbitrary Unit (AU) Treatment",
#       #                          choices = c("Keep all AU measurements" = "all_au",
#       #                                      "Keep passing AU measurements" = "passing_au",
#       #                                      "Geometric mean of all AU measurements" = "geom_all_au",
#       #                                      "Geometric mean of passing AU measurements" = "geom_passing_au",
#       #                                      "Replace AU measurements with geometric mean of blank AUs" = "replace_blank",
#       #                                      # "Replace AU measurements with geometric mean of positive control AUs" = "replace_positive_control",
#       #                                      "Exclude AU measurements" = "exclude_au"
#       #                          ), # end choices
#       #                          checkIcon = list(yes = icon("check")),
#       #                          justified = F,
#       #                          selected = "all_au")
#       # }
#
#       output$dilutional_linearity_plot <- renderPlotly({
#         req(filtered_classified_merged())
#         #req(classified_sample_dilution_linearity_rv())
#         req(classified_au())
#         req(input$antigenSelectionLinearity)
#         #req(input$concentration_status_selector)
#         #req(input$patientSelectionLinearity)
#         req(input$PassingSubjects)
#         #  req(input$timepointSelectionLinearity)
#         req(input$dilutionsSelectionLinearity)
#         cat("after requirements in dilution plot by clicking\n\n")
#
#         selected_timepoint <- as.character(unique(filtered_classified_merged()$timeperiod))
#         selected_status <- unique(filtered_classified_merged()$status)
#
#         print(selected_timepoint)
#         print(selected_status)
#         print(input$PassingSubjects)
#
#         plot_patient_dilution_series(sample_data = classified_au(),
#                                      selectedAntigen =input$antigenSelectionLinearity,
#                                      selectedPatient = input$PassingSubjects,
#                                      selectedTimepoints = selected_timepoint,# input$timepointSelectionLinearity,
#                                      selectedDilutions = input$dilutionsSelectionLinearity)
#
#
#       })
#
#
#
#
#     })
#
#     # observeEvent(input$passing_subject_table_cell_clicked, {
#     #   req(input$passing_subject_table_cell_clicked$row)  # Ensure a row is selected
#     #   req(input$passing_subject_table_cell_clicked$col)  # Ensure a column is selected
#     #   req(margin_table_rv())  # Ensure the contingency table is available
#     #
#     #   # Extract contingency table data
#     #   contingency_table <- margin_table_rv()
#     #
#     #   # Exclude the "Sum" row safely
#     #   valid_rows <- contingency_table$`Number of Passing Dilutions` != "Sum"
#     #   num_pass_dilutions <- as.numeric(contingency_table$`Number of Passing Dilutions`[valid_rows])
#     #
#     #   filtered_table <- contingency_table[valid_rows, , drop = FALSE]
#     #
#     #   row_index <- input$passing_subject_table_cell_clicked$row
#     #
#     #   # Ensure the row_index maps correctly to the filtered data
#     #   if (row_index > nrow(filtered_table) || row_index < 1) return()
#     #
#     #   current_pass_dilution <- as.numeric(filtered_table[row_index, "Number of Passing Dilutions"])
#     #
#     #
#     #   # Ensure row index is within bounds
#     #   if (row_index > length(num_pass_dilutions) || row_index < 1) return()
#     #
#     #   # Update the select input for passing dilutions
#     #   updateSelectInput(session = session,
#     #                     inputId = "n_pass_dilution",
#     #                     label = "Number of Passing Dilutions",
#     #                     choices = unique(num_pass_dilutions),
#     #                     selected =  current_pass_dilution) # Select corresponding value
#     #
#     #   # Extract the list of time points
#     #   timepoints <- colnames(margin_table_rv())
#     #
#     #   # Identify the selected timepoint based on the clicked column
#     #   selected_col <- input$passing_subject_table_cell_clicked$col
#     #   selected_timepoint <- timepoints[input$passing_subject_table_cell_clicked$col]
#     #
#     #   # Update the select input for timepoints dynamically
#     #   updateSelectInput(session = session,
#     #                     inputId = "my_filters-timepointSelectionLinearity",
#     #                     label = "Select Timepoint",
#     #                     choices = setdiff(timepoints, "Number of Passing Dilutions"),
#     #                     selected = selected_timepoint)  # Pre-select the current timepoint
#     #
#     #   selected_row <- contingency_table[row_index, ]
#     #   selected_color <-  paste0(selected_timepoint, "_color")
#     #   updated_treatment <- selected_row$selected_color
#     #
#     #   # update the select input for au_treatment
#     #   updateRadioGroupButtons(session = session,
#     #                     inputId = "au_treatment",
#     #                     label = "Arbitrary Unit (AU) Treatment",
#     #                     choices = c("Keep all AU measurements" = "all_au",
#     #                                 "Keep passing AU measurements" = "passing_au",
#     #                                 "Geometric mean of all AU measurements" = "geom_all_au",
#     #                                 "Geometric mean of passing AU measurements" = "geom_passing_au",
#     #                                 "Replace AU measurements with geometric mean of blank AUs" = "replace_blank",
#     #                                # "Replace AU measurements with geometric mean of positive control AUs" = "replace_positive_control",
#     #                                 "Exclude AU measurements" = "exclude_au"
#     #                     ), # end choices
#     #                     checkIcon = list(yes = icon("check")),
#     #                     justified = F,
#     #                     selected = updated_treatment)
#     #
#     #
#     # #update the concentration status
#     # updateSelectInput(session = session,
#     #                   inputId = "my_filters-concentration_status_selector",
#     #                   label = "Select Concentration Status",
#     #                   choices = setdiff(unique(contingency_table$concentration_status), "Sum"),
#     #                   selected = contingency_table[row_index,]$concentration_status)
#     #
#     #
#     #
#     # })
#
#
#     observeEvent(input$au_treatment , {
#       req(average_au_table_rv())
#
#       # req(input$n_pass_dilutions)
#       # req(input$status)
#       # req(input$timeperiod)
#
#       #req(filtered_classified_merged())
#       print(unique(filtered_classified_merged()$n_pass_dilutions))
#       print(unique(filtered_classified_merged()$status))
#       print(as.character(unique(filtered_classified_merged()$timeperiod)))
#
#       selected_n_pass_dilutions <- unique(filtered_classified_merged()$n_pass_dilutions)
#       selected_timepoint <- as.character(unique(filtered_classified_merged()$timeperiod))
#       selected_status <- print(unique(filtered_classified_merged()$status))
#       cat("after req in au_treatment")
#        # req(classified_merged_rv())
#       # req(input$timepointSelectionLinearity)
#      #  req(input$n_pass_dilution)
#      # # req(input$my_filters-concentration_status_selector)
#      #  # req(input$concentration_status_selector)
#      #  req(input$antigenSelectionLinearity)
#      #    cat("changing au treatment")
#      #    #print(input$au_treatment)
#      #    # print(input$timepointSelectionLinearity)
#      #    # print(input$n_pass_dilution)
#      #    # print(input$concentration_status_selector)
#      #    print(input$antigenSelectionLinearity)
#       avg_au_table <- average_au_table_rv()
#       #average_au_table_v <<- avg_au_table
#      #  selected_status <- input[["my_filters-concentration_status_selector"]]
#      #  selected_timepoint <- input[["my_filters-timepointSelectionLinearity"]]
#       avg_au_table[avg_au_table$timeperiod == selected_timepoint &
#                     avg_au_table$antigen %in% input$antigenSelectionLinearity &
#                     avg_au_table$n_pass_dilutions == selected_n_pass_dilutions &
#                     avg_au_table$concentration_status == selected_status,]$au_treatment <- input$au_treatment
#        # avg_au_table[avg_au_table$timeperiod == selected_timepoint &
#        #               avg_au_table$antigen %in% input$antigenSelectionLinearity &
#        #               avg_au_table$n_pass_dilutions == as.numeric(input$n_pass_dilution) &
#        #               avg_au_table$concentration_status == selected_status,]$au_treatment <- input$au_treatment
#      #  # avg_au_table$au_treatment[
#      #  #   avg_au_table$timeperiod == input$timepointSelectionLinearity &
#      #  #     avg_au_table$antigen %in% input$antigenSelectionLinearity &
#      #  #     avg_au_table$n_pass_dilutions == input$n_pass_dilution &
#      #  #     avg_au_table$concentration_status == input$concentration_status_selector
#      #  # ] <- input$au_treatment
#      #
#        updated_table <- avg_au_table
#        average_au_table_rv(updated_table)
#      #
#      #
#       classified_merged <- classified_merged_rv()
#
#         classified_merged$au_treatment[classified_merged$timeperiod == selected_timepoint &
#                                        classified_merged$n_pass_dilutions == selected_n_pass_dilutions  &
#                                        classified_merged$status == selected_status] <- input$au_treatment
#         classified_merged$n_dilution_status_treatment <- paste(classified_merged$n_dilution_status,classified_merged$au_treatment)
#
#         classified_merged_rv(classified_merged)
#
#         updated_margin_table <- create_margin_table(classified_merged)
#         margin_table_rv(updated_margin_table)
#     })
#
#
#     # select from the summary table a number of passing dilutions to examine
#     # Remove Sum from options
#     # output$select_n_pass_dilution <- renderUI({
#     #  # req(classified_merged_rv())
#     #   selectInput("n_pass_dilution",
#     #               label = "Number of Passing Dilutions",
#     #               choices =  sort(unique(classified_merged_rv()$n_pass_dilutions)),#as.numeric(unique(contingency_table_summary$`Number of Passing Dilutions`[contingency_table_summary$`Number of Passing Dilutions` != "Sum"])),
#     #               multiple = F)
#     # })
#
#
#
#     filtered_classified_merged <- datamods::select_group_server(
#       id = "da_filters",
#       data = reactive(classified_merged_rv()),
#       vars = c("n_pass_dilutions", "status", "timeperiod"),# only include filters for these columns
#        selected_r =reactive(da_filters_rv$selected)# reactive(da_filters_rv$selected)
#     )
#
#     # observe({
#     #   req(da_filters_rv$selected)  # Ensure selected filters are available
#     #
#     #   # Update the group inputs with the selected filters
#     #   datamods::updateGroupInputs(
#     #     id = "da_filters",
#     #     selected = da_filters_rv$selected
#     #   )
#     # })
#
#     output$filtered_data <- renderTable({
#       req(filtered_classified_merged())
#       fcm <- filtered_classified_merged()
#       filtered_classified_merged()
#     })
#
#
#
#     # observeEvent(input$n_pass_dilution, {
#     #   updateSelectInput(session = session,
#     #                     inputId = "n_pass_dilution",
#     #                     label = "Number of Passing Dilutions",
#     #                     choices = )
#     # })
# #
#
#     # observe({
#     #   req(classified_merged_rv())
#     #   dilution_choices <<-sort(unique(classified_merged_rv()$n_pass_dilutions))
#     #   rv <- reactiveVal(dilution_choices)
#     #   updateSelectizeInput(session, "n_pass_dilution", choices = rv())
#     # })
#
#
#     # observeEvent(input$n_pass_dilution, {
#     #   selected_dilution <<- as.numeric(input$n_pass_dilution)
#     #   req(classified_merged_rv())
#     #   if (is.null(selected_dilution)) return(NULL)  # Exit if no selection
#     #
#     #   classified_merged <<- classified_merged_rv()
#     #
#     #   # Filter status based on the selected n_pass_dilution
#     #   filtered_status <- unique(classified_merged_rv()[classified_merged_rv()$n_pass_dilutions == selected_dilution,]$status)
#     #
#     #
#     #   # Update the status selectize input with the filtered options
#     #   updateSelectizeInput(session, "my_filters-concentration_status_selector", choices = filtered_status)
#     #   updateSelectizeInput(session, "my_filters-timepointSelectionLinearity", selected = "")
#     # })
#
#     # concentration_status selector
#     # output$concentration_status <- renderUI({
#     #  # req(classified_merged_rv())
#     #   #req(input$n_pass_dilution)
#     #   choices_string <- unique(classified_merged_rv()[classified_merged_rv()$n_pass_dilutions == input$n_pass_dilution,]$status)
#     #   selectInput("concentration_status_selector",
#     #               label = "Select Concentration Status",
#     #               choices = choices_string,#setdiff(unique(contigency_table_concentration()$concentration_status), "Sum"),
#     #               multiple = F)
#     # })
#
#     # observeEvent(input[["my_filters-concentration_status_selector"]], {
#     #   selected_status <- input[["my_filters-concentration_status_selector"]]
#     #   selected_dilution <- as.numeric(input$n_pass_dilution)
#     #   req(classified_merged_rv())
#     #
#     #   if (is.null(selected_status) || selected_status == "") return(NULL)
#     #
#     #   # Filter time period based on the selected status and dilution
#     #   filtered_timeperiod <<- unique(
#     #     classified_merged_rv()[
#     #       classified_merged_rv()$n_pass_dilutions == selected_dilution &
#     #         classified_merged_rv()$status == selected_status,
#     #     ]$timeperiod
#     #   )
#     #
#     #   # Update the selectize input with the filtered time periods
#     #   updateSelectizeInput(session, "my_filters-timepointSelectionLinearity", choices = filtered_timeperiod)
#     # })
#     #
#     #
#
#
#     # output$sample_timepoint_selection <- renderUI({
#     #  # req(classified_merged_rv())
#     #   #req(input$n_pass_dilution)
#     #  # req(input$concentration_status_selector)
#     #   choices_time <- classified_merged_rv()[classified_merged_rv()$n_pass_dilutions == input$n_pass_dilution
#     #                                            & classified_merged_rv()$status == input$concentration_status_selector,]$timeperiod
#     #   selectInput("timepointSelectionLinearity",
#     #               label = "Select Timepoint",
#     #               choices = choices_time,
#     #               multiple = F)
#     # })
#     #
#
#
#
#     # res_filtered_data <- callModule(
#     #   module = selectizeGroupServer,
#     #   id = "my_filters",
#     #   data = classified_merged_rv(),
#     #   vars = c("n_pass_dilutions", "status", "timeperiod")
#     # )
#     # output$passing_subject_selection <- renderUI({
#     # #  req(classified_merged_rv())
#     #   #req(input$n_pass_dilution)
#     #   #req(input$concentration_status_selector)
#     #   #req(input$timepointSelectionLinearity)
#     #   choices_string <- unique(classified_merged_rv()[classified_merged_rv()$n_pass_dilutions == input$n_pass_dilution &
#     #                                                     classified_merged_rv()$status == input$concentration_status_selector &
#     #                                                     classified_merged_rv()$timeperiod == input$timepointSelectionLinearity ,]$patientid)
#     #   selectInput("PassingSubjects",
#     #               label = "Select Subjects",
#     #               choices = choices_string,
#     #               multiple = T,
#     #               selected = choices_string
#     #   )
#     #
#     # })
#
#     # output$passing_subject_selection <- renderUI({
#     # #  req(classified_merged_rv())
#     #   #req(input$n_pass_dilution)
#     #   #req(input$concentration_status_selector)
#     #   #req(input$timepointSelectionLinearity)
#     #   # choices_string <- unique(classified_merged_rv()[classified_merged_rv()$n_pass_dilutions == input$n_pass_dilution &
#     #   #                                                   classified_merged_rv()$status == input$concentration_status_selector &
#     #   #                                                   classified_merged_rv()$timeperiod == input$timepointSelectionLinearity ,]$patientid)
#     #   selectInput("PassingSubjects",
#     #               label = "Select Subjects",
#     #               choices = NULL,
#     #               multiple = T,
#     #   )
#     #
#     # })
#
#     output$passing_subject_selection <- renderUI({
#       req(input$antigenSelectionLinearity)
#       req(filtered_classified_merged())
#       choices_string <- unique(filtered_classified_merged()$patientid)
#       selectInput("PassingSubjects",
#                   label = "Select Subjects",
#                   choices = choices_string,
#                   multiple = T,
#                   selected = choices_string
#       )
#
#     })
#
#
#     # output$passing_subject_selection <- renderUI({
#     # req(classified_merged_rv())
#     #   #req(input$n_pass_dilution)
#     #   #req(input$concentration_status_selector)
#     #   #req(input$timepointSelectionLinearity)
#     #   classified_merged_sub <<- classified_merged_rv()
#     #
#     #   choices_string <- unique(classified_merged_rv()[classified_merged_rv()$n_pass_dilutions == input$n_pass_dilution &
#     #                                                     classified_merged_rv()$status == input[["my_filters-concentration_status_selector"]] &
#     #                                                     classified_merged_rv()$timeperiod ==input[["my_filters-timepointSelectionLinearity"]] ,]$patientid)
#     #   selectInput("PassingSubjects",
#     #               label = "Select Subjects",
#     #               choices = choices_string,
#     #               multiple = T
#     #   )
#     #
#     # })
#
#     # observe({
#     #   req(res_filtered_data())  # from selectizeGroupServer
#     #
#     #   res_filtered_data <<- res_filtered_data
#     #   patient_choices <- unique(res_filtered_data()$patientid)
#     #
#     #   updateSelectInput(
#     #     session,
#     #     "PassingSubjects",
#     #     label = "Select Subjects",
#     #     choices = patient_choices,
#     #     selected = patient_choices
#     #   )
#     # })
#
#     # AU treatment options
#     output$au_treatment_options_UI <- renderUI({
#       req(input$antigenSelectionLinearity)
#       req(filtered_classified_merged())
#      # req(classified_merged_rv())
#       #req(input$n_pass_dilution)
#       #req(input$concentration_status_selector)
#        #req(input$timepointSelectionLinearity)
#     #  req(input$PassingSubjects)
#
#       #initial_selection
#       # initial_selection <- unique(classified_merged_rv()[classified_merged_rv()$n_pass_dilutions == input$n_pass_dilution &
#       #                                                   classified_merged_rv()$status == input[["my_filters-concentration_status_selector"]] &
#       #                                                   classified_merged_rv()$timeperiod == input[["my_filters-timepointSelectionLinearity"]]#input$timepointSelectionLinearity
#       #                                                 #  classified_merged_rv()$patientid == input$PassingSubjects
#       #                                                 ,]$au_treatment)
#       #filtered_classified_merged_v <<- filtered_classified_merged()
#
#       initial_selection <- unique(filtered_classified_merged()$au_treatment)
#
#
# tryCatch({tagList(
#       radioGroupButtons(inputId = "au_treatment", label = "Arbitrary Unit (AU) Treatment",
#                    choices = c("Keep all AU measurements" = "all_au",
#                                "Keep passing AU measurements" = "passing_au",
#                                "Geometric mean of all AU measurements" = "geom_all_au",
#                                "Geometric mean of passing AU measurements" = "geom_passing_au",
#                                "Replace AU measurements with geometric mean of blank AUs" = "replace_blank",
#                               # "Replace AU measurements with geometric mean of positive control AUs" = "replace_positive_control",
#                                "Exclude AU measurements" = "exclude_au"
#                    ), # end choices
#                    selected = initial_selection,
#                    checkIcon = list(yes = icon("check")),
#                    justified = F,
#                    individual = T
#      ), # end radioButtons
#      # Style radio buttons to match cell colors of the margin table.
#       tags$script("$(\"input:radio[name='au_treatment'][value='all_au']\").parent().css('background-color', '#a1caf1');"),
#       tags$script("$(\"input:radio[name='au_treatment'][value='passing_au']\").parent().css('background-color', 'lightgrey');"),
#       tags$script("$(\"input:radio[name='au_treatment'][value='geom_all_au']\").parent().css('background-color', '#c2b280');"),
#       tags$script("$(\"input:radio[name='au_treatment'][value='geom_passing_au']\").parent().css('background-color', '#875692');"),
#       tags$script("$(\"input:radio[name='au_treatment'][value='replace_blank']\").parent().css('background-color', '#008856');"),
#       #tags$script("$(\"input:radio[name='au_treatment'][value='replace_positive_control']\").parent().css('background-color', '#dcd300');"),
#       tags$script("$(\"input:radio[name='au_treatment'][value='exclude_au']\").parent().css('background-color', '#b3446c');")
#
# )}, error = function(msg){
#   message(paste("Please select the number of passing dilutions to create a list of subjects"))
# })
#     })
#
#
#     observeEvent(input$antigenSelectionLinearity, {
#       req(decision_tree_reactive())
#       req(sample_dilution_linearity_rv())
#       req(background_control_rv()) # background of blank method.
#     #  req(classified_sample_dilution_linearity_rv())
#       req(input$dilutionsSelectionLinearity)
#       #req(passing_dilution_df_rv())
#       #req(classified_sample_rv())
#       cat("In antigen select observe")
#       #req(classified_concentration_rv)
#       # params = list(list(inputId = "n_pass_dilutions", label = "Number of Passing Dilutions", multiple = F),
#       #               list(inputId = "status", label = "Select Concentration Status", multiple = F),
#       #               timeperiod = list(inputId = "timeperiod", label = "Select Timepoint", multiple = F)
#       # )
#       da_filters_rv$selected <- reactive({
#         list(
#           n_pass_dilutions = 0,
#           status = "Too Diluted",
#           timeperiod = "T0"
#         )
#       })
#
#       # da_filters_rv <- reactiveValues(selected = list(n_pass_dilutions = 0, status = "Too Diluted", timeperiod = "T0")) # for init
#
#       sample_data <- sample_dilution_linearity_rv()
#
#       paths <- get_leaf_path(decision_tree_reactive())
#       parsed_data <- do.call(rbind, lapply(paths, parse_leaf_path, binary_gc = input$binary_gc,
#                                            sufficient_gc_vector = input$sufficient_gc))
#       # Classify the sample
#       classified_sample_in <- classify_sample(sample_data = sample_data, parsed_classification = parsed_data)
#
#       cat("before passsing dilutions df")
#       passing_dilution_df <- obtain_passing_dilutions_df(classified_sample = classified_sample_in,
#                                                          selectedAntigen = input$antigenSelectionLinearity)
#
#        classify_sample_data(sample_data = sample_dilution_linearity_rv() ,
#                                                          selectedAntigen = input$antigenSelectionLinearity,
#                                                          selectedDilutions = input$dilutionsSelectionLinearity,
#                                                          passing_dilution_df = passing_dilution_df)
#
#      ## classified_sample_dilution_v <<- classified_sample_dilution_linearity_rv()
#
#
#       cat("classified sample dil")
#       print(head(classified_sample_dilution_linearity_rv()))
#       cat("after classify sample data")
#       obtain_contigency_table(classified_sample_dilution_linearity_rv(), selectedAntigens = input$antigenSelectionLinearity)
#
#        cat("after obtain contigency table ")
#        classified_merged_static <- classified_merged_rv()
#       #merged_static <<- classified_merged_static
#
#       sample_dil <- classified_sample_dilution_linearity_rv()
#
#       classified_au_treat <- classified_merged_static[, c("patientid", "timeperiod", "au_treatment", "status")]
#      # classified_au_treat_v <<- classified_au_treat
#       average_au_table <- merge(classified_sample_dilution_linearity_rv(), classified_au_treat, by = c("patientid", "timeperiod"))
#
#       # keep all the statuses (pass and no pass dilutions.)
#       names(average_au_table)[names(average_au_table) == "status.y"] <- "status"
#       average_au_table <- average_au_table[, !names(average_au_table) %in% c("status.x", "status.y")]
#
#        average_au_table$decision_nodes <- paste(input$node_order, collapse = "|")
#        average_au_table$bkg_method <- background_control_rv()
#        # dilution x is all. But dilution y is the dilutions that pass and NA if 0
#        average_au_table[is.na(average_au_table$n_pass_dilutions),]$n_pass_dilutions <- 0
#
#
#        classified_au(average_au_table)
#
#        average_au_table <- average_au_table[, c("study_accession", "experiment_accession", "plate_id","agroup","timeperiod", "antigen","n_pass_dilutions", "dilution", "patientid", "au", "status", "au_treatment", "decision_nodes", "bkg_method")]
#        names(average_au_table)[names(average_au_table) == "au"] <- "final_au"
#        names(average_au_table)[names(average_au_table) == "status"] <- "concentration_status"
#
#
#        average_au_table_rv(average_au_table)
#      #classified_merged_static_view <<- classified_merged_static
#      #
#       classified_merged_rv(classified_merged_static)
#      #
#       margin_table <- create_margin_table(classified_merged_rv())
#      # margin_table_v <<- margin_table
#       margin_table_rv(margin_table)
#
#       output$dilutional_linearity_plot <- renderPlotly({
#         req(classified_merged_rv())
#         #req(classified_sample_dilution_linearity_rv())
#         req(classified_au())
#         req(input$antigenSelectionLinearity)
#         #req(input$concentration_status_selector)
#         #req(input$patientSelectionLinearity)
#         req(input$PassingSubjects)
#         #  req(input$timepointSelectionLinearity)
#         req(input$dilutionsSelectionLinearity)
#         cat("after requirements in plot\n\n")
#
#         selected_timepoint <- as.character(unique(classified_merged_rv()$timeperiod))
#         selected_status <- unique(classified_merged_rv()$status)
#
#         print(selected_timepoint)
#         print(selected_status)
#         print(input$PassingSubjects)
#
#         plot_patient_dilution_series(sample_data = classified_au(),
#                                      selectedAntigen =input$antigenSelectionLinearity,
#                                      selectedPatient = input$PassingSubjects,
#                                      selectedTimepoints = selected_timepoint,# input$timepointSelectionLinearity,
#                                      selectedDilutions = input$dilutionsSelectionLinearity)
#
#
#       })
#     })
#
#
#
#     ## update reactive Table when switch to a different choice
#      # observeEvent(input$au_treatment, {
#      #  req(input$timepointSelectionLinearity)
#      #  req(input$node_order)
#      #  req(input$antigenSelectionLinearity)
#      #  req(input$n_pass_dilution)
#      #  req(input$concentration_status_selector)
#      #
#      #  req(average_au_table_rv())
#      #  req(classified_merged_rv())
#      #  # retrieve current table
#      #   avg_au_table <- average_au_table_rv()
#      #
#      #   avg_au_table$au_treatment[avg_au_table$timeperiod == input$timepointSelectionLinearity &
#      #                             avg_au_table$antigen %in% input$antigenSelectionLinearity &
#      #                             avg_au_table$n_pass_dilutions == input$n_pass_dilution &
#      #                             avg_au_table$status == input$concentration_status_selector] <- input$au_treatment
#      #   # update the table
#      #   average_au_table_rv(avg_au_table)
#      #
#      #   classified_merged <- classified_merged_rv()
#      #
#      #   classified_merged$au_treatment[classified_merged$timeperiod == input$timepointSelectionLinearity &
#      #                                  classified_merged$n_pass_dilutions == input$n_pass_dilution  &
#      #                                  classified_merged$status == input$concentration_status_selector] <- input$au_treatment
#      #   classified_merged$n_dilution_status_treatment <- paste(classified_merged$n_dilution_status,classified_merged$au_treatment)
#      #
#      #   classified_merged_rv(classified_merged)
#      #
#      #   updated_margin_table <- create_margin_table(classified_merged)
#      #   margin_table_rv(updated_margin_table)
#      #
#      # })
#      #
#
#
#      positive_controls_rv <- reactive({
#        req(controls)
#        req(input$antigenSelectionLinearity)
#        positive_controls <- controls[controls$antigen %in% input$antigenSelectionLinearity,]
#        return(positive_controls)
#
#      })
#
#      buffer_data_filtered_rv <- reactive({
#        req(buffer_data)
#        req(input$antigenSelectionLinearity)
#        buffer_data_filtered <- buffer_data[buffer_data$antigen %in% input$antigenSelectionLinearity,]
#        return(buffer_data_filtered)
#      })
#
#      std_curve_data_filtered_rv <- reactive({
#        req(std_curve_data)
#        req(input$antigenSelectionLinearity)
#        std_curve_data_filtered <- std_curve_data[std_curve_data$antigen %in% input$antigenSelectionLinearity,]
#        return(std_curve_data_filtered)
#      })
#
#
#
#     # show table
#     output$average_au_table <- renderDT({
#       req(average_au_table_rv())
#       #req(input$timepointSelectionLinearity)
#       req(input$au_treatment)
#       req(input$node_order)
#      # req(input$concentration_status_selector)
#
#
#       average_au_data <- average_au_table_rv()
#       final_average_table <- data.frame()
#       for (au_treatment in unique(average_au_data$au_treatment)) {
#         au_treatment_sub <- average_au_data[average_au_data$au_treatment == au_treatment,]
#         if (au_treatment == "all_au") {
#           average_au <- preserve_all_au(au_treatment_sub)
#         } else if (au_treatment == "passing_au") {
#           average_au <- preserve_passing_au(au_treatment_sub)
#         } else if (au_treatment == "geom_all_au") {
#           average_au <- geometric_mean_all_au(au_treatment_sub)
#         } else if (au_treatment == "geom_passing_au") {
#           average_au <- geometric_mean_passing_au(au_treatment_sub)
#         } else if (au_treatment == "exclude_au") {
#           average_au <- au_treatment_sub
#         } else if (au_treatment == "replace_positive_control") {
#           req(positive_controls_rv())
#           average_au <- geometric_mean_positive_controls(au_treatment_sub, positive_controls_rv())
#         } else if (au_treatment == "replace_blank") {
#           req(req(std_curve_data_filtered_rv()))
#           average_au <- geometric_mean_blanks(au_treatment_sub, std_curve_data_filtered_rv())
#         }
#         final_average_table <- rbind(final_average_table, average_au)
#       }
#
#
#       # replace dilution that is not all au with NA because of averaging
#      # final_average_table$dilution[final_average_table$au_treatment != "all_au"] <- NA
#       final_average_table$dilution[!(final_average_table$au_treatment %in% c("all_au", "exclude_au"))] <- NA
#
#       # when exclude au the final au is NA.
#       final_average_table$final_au[final_average_table$au_treatment == "exclude_au"] <- NA
#
#       final_average_table$plate_id[final_average_table$au_treatment %in% c("geom_all_au", "geom_passing_au", "replace_positive_control", "replace_blank")] <- NA
#
#
#
#
#
#       final_average_rv(final_average_table)
#
#       DT::datatable(final_average_table, caption = "Dilution Analysis Sample Output",
#                     colnames = colnames(final_average_table), filter = "top")
#     })
#
#
#     output$download_classified_samples <- renderUI({
#       req(average_au_table_rv())
#       req(input$antigenSelectionLinearity)
#       req(input$readxMap_study_accession)
#       req(input$readxMap_experiment_accession)
#       download_classified_sample(download_df = average_au_table_rv(),
#                                  selected_study = input$readxMap_study_accession,
#                                  selected_experiment = input$readxMap_experiment_accession,
#                                  selected_antigen = input$antigenSelectionLinearity)
#     })
#
#     output$download_sample_dilution_linearity <- renderUI({
#       req(final_average_rv())
#       req(input$readxMap_study_accession)
#       req(input$readxMap_experiment_accession)
#       req(input$antigenSelectionLinearity)
#       download_sample_data_linearity(download_df = final_average_rv(), selected_study = input$readxMap_study_accession,
#                                      selected_experiment = input$readxMap_experiment_accession,
#                                      selected_antigen = input$antigenSelectionLinearity)
#     })
#
#
#
#
#
#  # output$dilutional_linearity_plot <- renderPlotly({
#  #     req(sample_dilution_linearity_rv())
#  #     req(classified_sample_dilution_linearity_rv())
#  #     req(classified_au())
#  #     #req(passing_dilution_df_rv())
#  #     req(input$antigenSelectionLinearity)
#  #     #req(input$concentration_status_selector)
#  #     #req(input$patientSelectionLinearity)
#  #    req(input$PassingSubjects)
#  #   #  req(input$timepointSelectionLinearity)
#  #    # req(input$n_pass_dilution)
#  #     req(input$dilutionsSelectionLinearity)
#  #
#  #     req(decision_tree_reactive())
#  #     cat("after requirements in plot\n\n")
#  #
#  #     # sample_data <- sample_dilution_linearity_rv()
#  #     #
#  #     # paths <- get_leaf_path(decision_tree_reactive())
#  #     # parsed_data <- do.call(rbind, lapply(paths, parse_leaf_path, binary_gc = input$binary_gc,
#  #     #                                      sufficient_gc_vector = input$sufficient_gc))
#  #     # # Classify the sample via tree
#  #     # classified_sample_in <- classify_sample(sample_data = sample_data, parsed_classification = parsed_data)
#  #     #
#  #     # passing_dilution_df <- obtain_passing_dilutions_df(classified_sample = classified_sample_in,
#  #     #                                                    selectedAntigen = input$antigenSelectionLinearity)
#  #
#  #     selected_timepoint <- as.character(unique(filtered_classified_merged()$timeperiod))
#  #     selected_status <- unique(filtered_classified_merged()$status)
#  #
#  #     print(selected_timepoint)
#  #     print(selected_status)
#  #     print(input$PassingSubjects)
#  #
#  #     plot_patient_dilution_series(sample_data = classified_au(),
#  #                               selectedAntigen =input$antigenSelectionLinearity,
#  #                               selectedPatient = input$PassingSubjects,
#  #                               selectedTimepoints = selected_timepoint,# input$timepointSelectionLinearity,
#  #                               selectedDilutions = input$dilutionsSelectionLinearity)
#  #                               # selected_status = input$concentration_status_selector,
#  #                               # selected_n_dilutions = input$n_pass_dilution,
#  #                               #passing_dilution_df = passing_dilution_df[passing_dilution_df$timeperiod == input$timepointSelectionLinearity,] )
#  #
#  # })
#
#
#  output$saveDilutionAnalysis <- renderUI({
#    req(final_average_rv())
#    actionButton("updateDilutionAnalysis", "Save Dilution Analysis")
#
#  })
#
#  observeEvent(input$updateDilutionAnalysis, {
#    req(final_average_rv())
#    final_average_df <- final_average_rv()
#    save_average_au(conn, final_average_df)
#
#  })
#
#   } # end in the tab
#
# })
