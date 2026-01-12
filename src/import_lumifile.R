getData <- reactive({
  if(is.null(input$upload_to_shiny)) return(NULL)
})

all_completed <- reactive({
  # types present in data
  present_types <- unique_plate_types()

  # Start by requiring P to be completed
  if (!type_p_completed()) return(FALSE)

  # Map of type ->  reactive status
  status_map <- list(
    X = type_x_status,
    S = type_s_status,
    C = type_c_status,
    B = type_b_status
  )

  # For all types that are present (excluding P, which we already handled)
  # check that plate_exists == TRUE and (optionally) n_record > 0
  checks <- sapply(present_types[present_types != "P"], function(t) {
    if (!t %in% names(status_map)) return(TRUE)  # ignore unexpected types
    status <- status_map[[t]]()
    status$plate_exists && status$n_record > 0
  }, USE.NAMES = FALSE)

  # TRUE only if all present types are completed
  all(checks)
})

plate_layout_plots <- reactive({
  req(layout_template_sheets()[["plates_map"]])
  req(layout_template_sheets()[["plate_id"]])

  plates_map <- layout_template_sheets()[["plates_map"]]
  plate_id_data <- layout_template_sheets()[["plate_id"]]

  # Call your function
  plot_plate_layout(plates_map, plate_id_data)
})


### Outputs
output$upload_path_text <- renderText({
  paste(stri_replace_all_charclass(Sys.getenv("upload_template_path"), "\\p{WHITE_SPACE}", ""))
})

output$fileUploaded <- reactive({
  return(!is.null(getData()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

output$readxMapData <- renderUI({
 tabRefreshCounter()$import_tab
  if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab" & input$study_tabs == "import_tab") {
  if (input$readxMap_study_accession != "Click here") {
    import_plate_data_title <- paste("Import", input$readxMap_study_accession, "Plate Data", sep = " ")
    tagList(
      fluidPage(
        tagList(
          h3(import_plate_data_title),
         # if (input$readxMap_study_accession != "Click here") {
            bsCollapsePanel(
              "Instructions",
              p("This is where we can load plate data from either Raw excel files or from the xPONENT format."),
              p("1. Choose an existing study OR Create a new study."),
              p("2. Choose an existing experiment OR Create a new experiment."),
              p("3. Choose an xPONENT format OR a RAW format to upload."),
              p("4. Browse to your plate data and import it in the correct format."),
              p("5. Parse the plate data into correct columns/fields appropriate and required for each data component/type."),
              p("6. Upload each data component/type."),
              style = "success"
            )
        #  }
        ),
        # tagList(
        #   h3(import_plate_data_title),
        #   bsCollapsePanel(
        #     "Instructions",
        #     p("This is where we can load plate data from either Raw excel files or from the xPONENT format."),
        #     p("1. Choose an existing study OR Create a new study."),
        #     p("2. Choose an existing experiment OR Create a new experiment."),
        #     p("3. Choose an xPONENT format OR a RAW format to upload."),
        #     p("4. Browse to your plate data and import it in the correct format."),
        #     p("5. Parse the plate data into correct columns/fields appropriate and required for each data component/type."),
        #     p("6. Upload each data component/type."),
        #     style = "success"
        #   )
        # ),
        br()
        ,
        fluidRow(
          column(5,
                 tagList(
                   div(
                     style = "width: 700px;",
                     tags$label(
                       `for` = "readxMap_experiment_accession_import",
                       style = "display: block; padding-left: 15px;",
                       "Select Experiment Name",
                       tags$br(),
                       tags$small(style = "font-weight: normal;",
                                  "To create a new experiment, type in this box (up to 15 characters)."
                       )
                     ),
                # conditionalPanel(
                #  condition = "input.readxMap_study_accession != 'Click here'",
                   selectizeInput("readxMap_experiment_accession_import",
                                  label = NULL,
                                  # "Choose Existing Experiment Name OR Create a New Experiment Name (by typing up to 15 characters)",
                                  # choices <- c("Click OR Create New" = "Click here"),
                                  choices <- experiment_choices_rv(),
                                  selected = "Click here",
                                  multiple = FALSE,
                                  options = list(create = TRUE,
                                  onType = I("function(str) {
                                                if (str.length > 15) {
                                                  this.setTextboxValue(str.substring(0, 15));
                                                }
                                              }")), width = '700px'
                   )
               # )
               )
             )
          )
        )
        ,
        fluidRow(
          column(9,
                 conditionalPanel(
                   condition = "input.readxMap_experiment_accession_import != 'Click here' && input.readxMap_experiment_accession_import != ''",
                   div(
                     style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
                              padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                     tags$h4("Current Import Context", style = "margin-top: 0; color: #2c5aa0;"),
                     textOutput("current_context_display")
                   )
                 )
          )
        ),
        fluidRow(
          column(9,
                 conditionalPanel(
                   condition = "input.readxMap_experiment_accession_import != 'Click here' && input.readxMap_experiment_accession_import != ''",
                   shinyWidgets::radioGroupButtons(
                     inputId = "xPonentFile",
                     label = "File Format",
                     choices = c("xPONENT", "RAW", "Layout Template"),
                     selected = "RAW",
                     justified = TRUE,
                     checkIcon = list(
                       yes = icon("check", lib = "font-awesome")
                     )
                   ),
                   conditionalPanel(
                     condition = "input.xPonentFile == 'xPONENT'",
                     uiOutput("xPonentReader_fileinput_ui")
                   ),
                   conditionalPanel(
                     condition = "input.xPonentFile == 'RAW'",
                     fileInput("upload_to_shiny"
                               , label="Upload a plate/batch file (only accepts xlsx, xls)"
                               , accept=c(".xlsx",".xls")
                               , multiple=FALSE)
                     ,uiOutput("sheet_ui")
                   ), conditionalPanel(
                     condition = "input.uploaded_sheet != ''",
                     uiOutput("raw_ui")
                   ),
                   conditionalPanel(
                     condition = "input.xPonentFile == 'Layout Template'",
                     tags$div(
                       style = "display: flex; flex-wrap: wrap; align-items: center; gap: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                       tags$div(
                         class = "element-controls",
                         fileInput("upload_experiment_files",
                                   label = "Select all experiment raw data files",
                                   accept = c(".xlsx", ".xls"),
                                   multiple = TRUE),
                         tags$span(style = "font-weight: 600; align-self: center;", "Feature:"),
                         textInput("feature_value", "e.g. Total_IgG; FcgR2a; multiple", "Up to 15 chars")
                       )
                     ),
                     conditionalPanel(
                       condition = "output.hasExperimentPath",
                       tags$div(
                         style = "display: flex; flex-wrap: wrap; align-items: center; gap: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                         tags$div(
                           class = "element-controls",
                           # style = "display: flex; align-items: center; gap: 10px;",
                           tags$span(style = "font-weight: 600; align-self: center;", "Wells:"),
                           radioGroupButtons(
                             inputId = "n_wells_on_plate",
                             label = NULL,
                             choices = c("96" = 96, "6" = 6, "12" = 12, "24" = 24, "48" = 48, "384" = 384, "1536" = 1536),
                             selected = 96,
                             size = "sm",
                             status = "outline-primary"
                           )
                         ),
                         tags$div(
                           class = "element-controls",
                           # style = "display: flex; align-items: center; gap: 10px;",
                           tags$span(style = "font-weight: 600; align-self: center;", "Description Delimiter:"),
                           radioGroupButtons(
                             inputId = "description_delimiter",
                             label = NULL,
                             choices = setNames(c("_", "|", ":", "-"), c("_", "|", ":", "-")),
                             selected = "_",
                             size = "sm",
                             status = "outline-primary"
                           )
                         ),
                         # tags$head(
                         #   tags$style(HTML("
                         #    .element-controls {
                         #      display: flex;
                         #      flex-wrap: wrap;
                         #      gap: 20px;
                         #      padding: 15px;
                         #      background: #f8f9fa;
                         #      border-radius: 8px;
                         #      margin-bottom: 15px;
                         #    }
                         #  "))
                         # ),
                         # Checkboxes to include/exclude optional elements
                         tags$div(
                           class = "element-controls",
                           tags$span(style = "font-weight: 600; align-self: center;", "Include Optional Elements:"),
                           checkboxGroupButtons(
                             inputId = "optional_elements",
                             label = NULL,
                             choices = c("SampleGroupA", "SampleGroupB"),
                             selected = c("SampleGroupA", "SampleGroupB"),
                             status = "outline-primary",
                             checkIcon = list(
                               yes = icon("check"),
                               no = icon("times")
                             )
                           )
                         ),
                         uiOutput("order_input_ui"),
                         uiOutput("bcsorder_input_ui"),
                         # orderInput(
                         #   inputId = "XElementOrder",
                         #   label = "Sample Elements (drag and drop items to change order)",
                         #   items = c('PatientID', 'DilutionFactor', 'TimePeriod', 'SampleGroupA', 'SampleGroupB'),
                         #   width = "100%", item_class = 'primary'
                         # ),
                         # fluidRow(
                         #   column(
                         #     width = 12,
                         #     tags$div(
                         #       style = "display: flex; align-items: center; margin-top: 10px;",
                         #       tags$h5("Current Order:", style = "margin: 0; margin-right: 10px;"),
                         #       textOutput("XElementOrder", inline = TRUE)
                         #     )
                         #   )
                         # ),
                         # fluidRow(
                         #   column(
                         #     width = 12,
                         #     tags$div(
                         #       style = "display: flex; align-items: center; margin-top: 10px;",
                         #       tags$h5("Current Order:", style = "margin: 0; margin-right: 10px;"),
                         #       textOutput("BCSElementOrder", inline = TRUE)
                         #     )
                         #   )
                         # ),
                         downloadButton("blank_layout_file", "Generate a Layout file")
                       ),
                       fileInput("upload_layout_file"
                                 , label="Upload a completed layout file (only accepts xlsx, xls)"
                                 , accept=c(".xlsx",".xls")
                                 , multiple=FALSE)
                     )
                   ),
                   # conditionalPanel(
                   #   condition = "input.xPonentFile == 'Layout Template'",
                   #   fileInput("upload_experiment_files",
                   #             label = "Select all experiment raw data files",
                   #             accept = c(".xlsx",".xls"),
                   #             multiple = TRUE),
                   #   conditionalPanel(
                   #     condition = "output.hasExperimentPath",
                   #      radioButtons(inputId = "n_wells_on_plate", "Select Number of Wells to Generate on Plates",
                   #                choices = c(96, 6, 12, 24, 48, 384, 1536),
                   #                selected = 96,
                   #                inline = T),
                   #     tags$div(
                   #       style = "display: inline-flex; align-items: center; gap: 10px; padding: 10px; background: #f8f9fa; border-radius: 8px;",
                   #       tags$span(style = "font-weight: 600;", "Delimiter:"),
                   #       radioGroupButtons(
                   #         inputId = "description_delimiter",
                   #         label = NULL,
                   #         choices = setNames(c("_", "|", ":"), c("_", "|", ":")),
                   #         selected = "_",
                   #         size = "sm",
                   #         status = "outline-primary"
                   #       )
                   #     ),

                     ),
                     conditionalPanel(
                       condition = "input.upload_layout_file != ''",
                       uiOutput("view_layout_file_ui"),
                       uiOutput("plate_layout_selector"),
                       plotlyOutput("selected_plate_layout_plot"),
                      conditionalPanel(
                         condition = "output.hasExperimentPath",
                      tagList(
                        tags$p("If this batch of plates contains wells without samples use the word the 'Blank' in the description column of the spreadsheet.
            Then assign the two phrases below to indicate if the wells should be treated as blanks
            (e.g. containing PBS) or if the wells should be treated as empty."),

                        selectInput("batch_blank", "Blank and Empty Well Handling",
                                    choices = c("Skip Empty Wells" = "empty_well",
                                                "Use as Blank" = "use_as_blank"))
                      ),
                       uiOutput("batch_validation_status"),
                       tableOutput("batch_invalid_messages"),
                       uiOutput("upload_batch_data_button")
                      ) # end of hasExperimentPath condition
                     )
                     )
                 )
          ),
          column(9,
                 conditionalPanel(
                   condition = "input.xPonentFile == 'xPONENT'",
                   uiOutput("xPonent_fileparse_ui")
                 ),
                 conditionalPanel(
                   condition = "input.xPonentFile == 'RAW'",
                   uiOutput("segment_selector")
                 )
          )
        )
    #   )
    # )
  } else {
    import_plate_data_title<- paste("Choose or create a study for Importing Plate Data")
  }

   }
  #else {
  #   NULL
  # }
})

output$hasExperimentPath <- reactive({
  path_df <- input$upload_experiment_files   # fileInput returns a data frame
  !is.null(path_df) && nrow(path_df) > 0
})
outputOptions(output, "hasExperimentPath", suspendWhenHidden = FALSE)

output$blank_layout_file <- downloadHandler(
  filename = function() {
    paste0(input$readxMap_study_accession, "_", input$readxMap_experiment_accession_import, "_layout_template.xlsx")
  },
  content = function(file) {
    req(input$upload_experiment_files)
    req(bead_array_header_list())

    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║         GENERATING LAYOUT TEMPLATE                      ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")

    # CRITICAL: Use batch_plate_data() instead of reprocessing files
    # This ensures we use the EXACT same data that was uploaded
    all_plates <- batch_plate_data()

    if (is.null(all_plates)) {
      cat("⚠️  ERROR: batch_plate_data() is NULL!\n")
      cat("   Falling back to processing uploaded files...\n")
      all_plates <- process_uploaded_files(input$upload_experiment_files)
    }

    cat("Using plate data:\n")
    cat("  → Rows:", nrow(all_plates), "\n")
    cat("  → Columns:", ncol(all_plates), "\n")
    cat("  → Source files:", paste(unique(all_plates$source_file), collapse=", "), "\n")

    # Extract antigens
    metadata_cols <- c("source_file", "Well", "Type", "Description",
                       "% Agg Beads", "Sampling Errors", "Acquisition Time")
    antigen_cols <- names(all_plates)[!(names(all_plates) %in% metadata_cols)]
    cat("  → Antigens (", length(antigen_cols), "):\n", sep="")
    for (ag in antigen_cols) {
      cat("      •", ag, "\n")
    }

    generate_layout_template(
      all_plates = all_plates,
      study_accession = input$readxMap_study_accession,
      experiment_accession = input$readxMap_experiment_accession_import,
      n_wells = input$n_wells_on_plate,
      header_list = bead_array_header_list(),
      output_file = file
    )

    cat("✓ Layout template generated!\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")
  }
)

output$plate_metadata_info <- renderUI({
  req(inFile()) # ensure new upload triggers updates
  req(type_p_completed())
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  req(input$table_plates)

  # Convert rhandsontable to data frame
  metadata_table <- hot_to_r(input$table_plates)

  # Extract plate_id value removing extension
  plate_id <- metadata_table[metadata_table$variable == "plate_id",]$value
  #plate_id <- sub("\\.[^.]*$", "", metadata_table[metadata_table$variable == "plate_id", ]$value)

  import_plate_id <- metadata_table[metadata_table$variable == "plate", ]$value
  import_plate_number <- as.integer(gsub("\\D", "", import_plate_id))
  updateTextInput(session = session, "read_import_plate_id",  value = plate_id)
  updateTextInput(session = session, "read_import_plate_number",  value = import_plate_number)

  fluidRow(
    column(
      12,
      div(
        style = "margin-bottom: 10px;",
        tags$span(style = "font-weight: bold;", "Study: "), input$readxMap_study_accession,
        " | ",
        tags$span(style = "font-weight: bold;", "Experiment: "), input$readxMap_experiment_accession_import,
        " | ",
        tags$span(style = "font-weight: bold;", "plate_id: "), input$read_import_plate_id,
        " | ",
        tags$span(style = "font-weight: bold;", "Plate Number: "), input$read_import_plate_number
      )
    )
  )
})

output$plate_validated_status <- renderUI({
  req(input$uploaded_sheet)# trigger refresh

  if (!is_valid_plate()) {
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      createValidateBadge(is_validated = is_valid_plate())
    )
  } else {
    createValidateBadge(is_validated = is_valid_plate())
  }
})

output$plate_validation_message_table <- renderTable({
  req(plate_validation_messages())
  messages_table <- data.frame(
    "Message Number" = seq_along(plate_validation_messages()),
    "Please correct the formatting errors in the file" = plate_validation_messages(),
    check.names = FALSE
  )
})

output$ui_optimization <- renderUI({
  all_completed()
  optimization_parsed_boolean()

  if (!all_completed()) {
    return(div(
      " All required plate types must be completed before proceeding to optimization."
    ))
  }
  if (all_completed() && !optimization_parsed_boolean()) {
    fluidRow(
      tagList(
        tags$p("This plate has more than two serum dilutions. To assess dilutional linearity in the QC workflow
               the plate must be treated as different at each dilution. Splitting this optimization plate will
               also create a new experiment that parses the mixed plates into optimization experiments."),
        radioButtons(
          inputId = "decide_split",
          label = "Do you want to Split this optimization plate by serum dilution into a plate per dilution?",
          choices = c("Yes", "No")
        )
      )
    )
  }  #else if (optimization_parsed_boolean()) {
  #   createOptimizedBadge(is_optimized = optimization_parsed_boolean())
  #
  # }
  # )
})

output$ui_split_button <- renderUI({
  if (!is.null(input$decide_split) && input$decide_split == "Yes" &&
      !isTRUE(optimization_parsed_boolean())) {
    actionButton("split_opt_plates", "Split Optimization Plate")
  }
})

output$plate_optimized_status <- renderUI({
  req(input$uploaded_sheet)# trigger refresh
  input$split_opt_plates
  optimization_parsed_boolean()  # dependency

  createOptimizedBadge(is_optimized = optimization_parsed_boolean())

})

# Display basic info about uploaded files
output$file_summary <- renderPrint({
})

output$plate_layout_selector <- renderUI({
  req(plate_layout_plots())
  plots <- plate_layout_plots()

  shinyWidgets::radioGroupButtons(
    inputId = "select_plate_layout_plot",
    label = "Select Plate Layout:",
    choices = names(plots),
    selected = names(plots)[1],
    status = "success"
  )
})

output$selected_plate_layout_plot <- renderPlotly({
  req(input$select_plate_layout_plot)
  req(plate_layout_plots())
  plots <- plate_layout_plots()
  plots[[input$select_plate_layout_plot]]
})

output$batch_validation_status <- renderUI({
  state <- batch_validation_state()
  req(state)  # Ensure state exists
  createValidateBatchBadge(state$is_validated)
})

output$batch_invalid_messages <- renderTable({
  state <- batch_validation_state()
  req(state)

  if (!state$is_validated && !is.null(state$metadata_result) && !is.null(state$bead_array_result)) {
    create_batch_invalid_message_table(state$metadata_result, state$bead_array_result)
  } else {
    NULL
  }
})

output$upload_batch_data_button <- renderUI({
  req(batch_metadata())

  state <- batch_validation_state()
  req(state)

  metadata_batch <- batch_metadata()
  batch_study_accession <- unique(metadata_batch$study_accession)
  plates_to_upload <- unique(metadata_batch$plate_id)

  # Build SQL list
  plate_list_sql <- paste0("'", paste(plates_to_upload, collapse = "', '"), "'")

  query <- sprintf("
    SELECT plate_id
    FROM madi_results.xmap_header
    WHERE study_accession = '%s'
      AND plate_id IN (%s);
  ", batch_study_accession, plate_list_sql)

  existing_plates <- DBI::dbGetQuery(conn, query)
  plates_exist_in_db <- nrow(existing_plates) > 0

  # Determine badge status - use state$is_uploaded OR database check
  show_uploaded_badge <- state$is_uploaded || plates_exist_in_db

  badge <- createUploadedBatchBadge(show_uploaded_badge)

  # Show button only if validated AND not yet uploaded/existing
  button <- if (state$is_validated && !plates_exist_in_db && !state$is_uploaded) {
    actionButton("upload_batch_button", "Upload Batch")
  } else {
    NULL
  }

  tagList(
    badge,
    br(),
    button
  )
})

output$view_layout_file_ui <- renderUI({
  req(layout_template_sheets())
  req(length(layout_template_sheets()) > 0)
  fluidRow(
    column(2, actionButton("view_layout_plate_id_sheet", "View Layout Plate ID")),
    column(2, actionButton("view_layout_subject_group_sheet", "View Layout Subject Map")),
    column(2, actionButton("view_layout_timepoint_sheet", "View Layout Timepoint")),
    column(2, actionButton("view_layout_plates_map_sheet", "View Layout Plate Map")),
    column(2, actionButton("view_layout_antigen_list_sheet", "View Layout Antigen List"))
  )
})

output$current_context_display <- renderText({
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  paste0(
    "Study: ", input$readxMap_study_accession, " | ",
    "Experiment: ", input$readxMap_experiment_accession_import
  )
})


### Observes



# Call on server start
onSessionStart()

# Diagnostic observer to track state changes
observe({
  cat("\n=== BATCH VALIDATION STATE DEBUG ===\n")
  cat("batch_metadata() is NULL:", is.null(batch_metadata()), "\n")
  cat("layout_template_sheets() length:", length(layout_template_sheets()), "\n")
  cat("batch_plate_data() is NULL:", is.null(batch_plate_data()), "\n")

  if (!is.null(batch_metadata())) {
    cat("batch_metadata rows:", nrow(batch_metadata()), "\n")
    cat("batch_metadata study:", unique(batch_metadata()$study_accession), "\n")
  }

  state <- batch_validation_state()
  cat("Validation state - is_validated:", state$is_validated, "\n")
  cat("Validation state - is_uploaded:", state$is_uploaded, "\n")
  cat("=========================================\n")
})

observeEvent(input$optional_elements, {
  # Base elements (always included)
  base_elements <- c("PatientID", "DilutionFactor", "TimePeriod")

  # Reactive to get all active elements
  active_elements <- reactive({
    c(base_elements, input$optional_elements)
  })

  # Render order input dynamically
  output$order_input_ui <- renderUI({
    orderInput(
      inputId = "XElementOrder",
      label = "Description Label: Sample Elements (drag and drop items to change order)",
      items = active_elements(),
      width = "100%",
      item_class = "primary"
    )
  })

  output$bcsorder_input_ui <- renderUI({
    orderInput(
      inputId = "BCSElementOrder",
      label = "Description Label: Blank, Standard or Control Elements (drag and drop items to change order)",
      items = c('Source', 'DilutionFactor'),
      width = "100%",
      item_class = 'info'
    )
  })


})

observeEvent(input$readxMap_study_accession, {
  print(paste("readxMap_study_accession clicked:", input$readxMap_study_accession))
    if (input$readxMap_study_accession != "Click here") {
 # imported_h_study(input$readxMap_study_accession)

    # NEW: RESET ALL BATCH REACTIVES when study changes
    reset_batch_reactives()

    # NEW: Clear any uploaded files from UI
    shinyjs::reset("upload_experiment_files")
    shinyjs::reset("upload_layout_file")

    # NEW: Show notification to user
    showNotification(
      "Study changed. All batch data has been cleared. Please upload new files.",
      type = "warning",
      duration = 5
    )

    # study_exp is a data frame with study and experiment names filtered by user/project workspace
    study_exp <- reactive_df_study_exp()
    # filter study_exp by current study selected in navigation
    filtered_exp <- study_exp[study_exp$study_accession == input$readxMap_study_accession, ]

    print(paste("\n filtered_exp rows:", nrow(filtered_exp)))

    if (nrow(filtered_exp) > 0) {
      expvector <- setNames(filtered_exp$experiment_accession, filtered_exp$experiment_name)
    } else {
      expvector <- character(0)
    }

    experiment_drop <- c("Click OR Create New" = "Click here", expvector)
    print(paste("\n experiment choices:", experiment_drop))

    experiment_choices_rv(experiment_drop)

  }


})

observeEvent(input$readxMap_experiment_accession_import, {
  req(input$readxMap_study_accession)

  print(paste("readxMap_experiment_accession_import changed to:",
              input$readxMap_experiment_accession_import))

  if (input$readxMap_experiment_accession_import != "Click here" &&
      input$readxMap_experiment_accession_import != "") {

    # RESET ALL BATCH REACTIVES when experiment changes
    reset_batch_reactives()

    # Clear any uploaded files from UI
    shinyjs::reset("upload_experiment_files")
    shinyjs::reset("upload_layout_file")

    # Show notification to user
    showNotification(
      paste("Experiment changed to:", input$readxMap_experiment_accession_import,
            "- All batch data has been cleared. Please upload new files."),
      type = "warning",
      duration = 5
    )
  }
})

### read template and create the preview template tab
### Clicks browse to load a plate/batch
observeEvent(input$upload_to_shiny,{

  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)
  availableSheets(NULL)
  # # reset reactive that hold the data
  inFile(NULL)
  plate_data(NULL)
  header_info(NULL)
  plate_validation_messages(NULL)
  is_valid_plate(NULL)
  #imported_h_study(NULL)
  #imported_h_experiment(NULL)
  #imported_h_plate_id(NULL)
  #imported_h_plate_number(NULL)
  type_p_completed(FALSE)
  # type_x_status(list(plate_exists = FALSE, n_record = 0))
  # type_s_status(list(plate_exists = FALSE, n_record = 0))
  # type_c_status(list(plate_exists = FALSE, n_record = 0))
  # type_b_status(list(plate_exists = FALSE, n_record = 0))
  transform_dat <- data.frame()
  # original_df_combined <- reactive({NULL})
  # list_of_dataframes <- reactive({NULL})
  updateSelectInput(session = session, "uploaded_sheet", NULL)


  # type_vector_obs <- c("S", "C", "B", "X")
  #
  # # Reset all rhandsontables
  # for (type in type_vector_obs) {
  #  # if (!is.null(output[[paste0("table_", type)]])) {
  #     cat(paste0("table_", type))
  #       output[[paste0("table_", type)]] <- renderRHandsontable({
  #         NULL
  #         #rhandsontable(data.frame())  # blank placeholder
  #       })
  #   }
  #}

  print("file_uploaded")

  # Store the uploaded file
  inFile(
    input$upload_to_shiny
  )

  if (is.null(inFile())) {
    return(NULL)
  }

  sheets <- readxl::excel_sheets(inFile()$datapath)
  availableSheets(sheets)
  # Dynamically generate UI for sheet selection
  output$sheet_ui <- renderUI({
    sheets <- availableSheets()
    if (!is.null(sheets)) {
      fluidRow(
        selectInput("uploaded_sheet",
                    "Select Sheet",
                    choices = c("Select excel sheet" = "", sheets),  # Combine default with sheets
                    selected = "")
      )
    }

  })

  output$raw_ui <- renderUI({
    sheets <- availableSheets()
    if (!is.null(sheets)) {
      fluidRow(
        if (is.null(plate_validation_messages())) {
          tagList(
            tags$p("If this plate contains wells without samples use the word the 'Blank' in the description column of the spreadsheet.
          Then assign the two phrases below to indicate if the wells should be treated as blanks
          (e.g. containing PBS) or if the wells should be treated as empty."),

            selectInput("blank_keyword", "Blank and Empty Well Handling",
                        choices = c("Skip Empty Wells" = "empty_well",
                                    "Use as Blank" = "use_as_blank"))
          )
        },
        actionButton("view_raw_file", "View Raw File"),
        actionButton("view_raw_header", "View Plate Metadata"),
        tags$div(style = "display:none;",
                 textInput("read_import_plate_id", label = NULL, value = ""),
                 textInput("read_import_plate_number", label = NULL, value = "")
        ),
        uiOutput("plate_metadata_info"),
        uiOutput("plate_validated_status"),
        tableOutput("plate_validation_message_table")
      )
    } else {
      fluidRow(
        tagList(
          tags$p("Select a sheet containing raw bead array data."
          )
        )
      )
    }


  })


})

observe({
  cat("plate_id:", input$read_import_plate_id, "\n")
  cat("Plate Number:",input$read_import_plate_number, "\n" )
})

observeEvent(input$uploaded_sheet,{

  # Require to select sheet to read
  req(input$uploaded_sheet)

  if (input$uploaded_sheet != "") {

  plate_data(
    openxlsx::read.xlsx(inFile()$datapath, startRow = 8, sheet = input$uploaded_sheet)
  )

  header_info(
    openxlsx::read.xlsx(inFile()$datapath, rows = c(1:7), sheet = input$uploaded_sheet, colNames = F)
  )

  transform_dat <- plate_data()
  transform_dat <- data.frame(lapply(transform_dat, function(x) {  gsub("[,]", ".", x) }))
  transform_dat <- data.frame(lapply(transform_dat, function(x) {  gsub("[*]+", "", x) }))
  transform_dat <- data.frame(lapply(transform_dat, function(x) {  gsub("[.]+", ".", x) }))


  # remove trailing rows at the end of file that contain all NA or blank strings
  transform_dat <- transform_dat[!apply(
    transform_dat, 1,
    function(x) all(is.na(x) | trimws(x) == "" | trimws(x) == "NA")
  ), ]


  plate_data(transform_dat)

  # options(max.print = 1000000)
  # print(transform_dat)

  plate_data <- transform_dat
  meta_df <- parse_metadata_df(header_info())

  plate_validation_result <- plate_validation(plate_metadata = meta_df,
                                               plate_data = plate_data,
                                               blank_keyword = input$blank_keyword)

  # cat(plate_validation_result$is_valid)
  # cat(plate_validation_result$messages)
  plate_validation_messages(plate_validation_result$messages)
  is_valid_plate(plate_validation_result$is_valid)
  if (plate_validation_result$is_valid) {
     plate_data(plate_validation_result$updated_plate_data)
  }

  }


})

observeEvent(input$view_raw_file,{
  print("view raw file")
  showModal(
    modalDialog(
      title = "Raw File",
      rhandsontable::rHandsontableOutput("raw_file"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$raw_file <- rhandsontable::renderRHandsontable({
    rhandsontable(plate_data(), readOnly = TRUE)
  })

 # plate_data <<- plate_data()
  print(plate_data())
})

observeEvent(input$view_raw_header,{
  print("view raw header")
  showModal(
    modalDialog(
      title = "Plate Metadata",
      rhandsontable::rHandsontableOutput("raw_header_file"),
      size = "l",
      easyClose = TRUE
    )
  )
  output$raw_header_file <- rhandsontable::renderRHandsontable({
    rhandsontable(header_info(), readOnly = TRUE)
  })
 # platemetadata <<- header_info()
  print(header_info())
})

output$segment_selector <- renderUI({
  #req(inFile()$datapath != )

  req(is_valid_plate()) # Only show if it has a valid plate too

  req(plate_data())  # Ensure that there is data to work with
  # require sheet
  req(input$uploaded_sheet)

  type_p_completed()

  # Find unique types in the dataset
  plate_data() %>%
    #slice(8:n()) %>%
    janitor::clean_names() %>%
    mutate(description = gsub("[^A-Za-z0-9]+", "_", description) %>% trimws(whitespace = "_")) %>%
    mutate(type = str_remove_all(type, "[0-9]")) %>%
    pull(type) %>%
    unique() -> unique_types

  unique_types <- c("P", unique_types)
  # only handle the allowed types
  allowed_types <- c("P", "X", "S", "C", "B","E")
  unique_types <- unique_types[!is.na(unique_types) & unique_types %in% allowed_types]

  unique_plate_types(unique_types)

 if (!type_p_completed()) {
   unique_types <- "P"
 }

  # Create a tab for each unique type
  tabs <- lapply(unique_types, function(type) {
    tabPanel(
      title = paste("Type", type),
      uiOutput(outputId = paste0("ui_", type))  # This will be where we place the table and inputs
    )
  })

  #late_data_in <<- plate_data()

  # add optimization tab if detect multiple sample serum dilutions.
  tabs <- append(tabs, list(uiOutput("optimization_tab")))

  #  is_opt_experiment <- is_optimization_plate(plate_data())
  # if (is_opt_experiment && all_completed()) {
  if (optimization_ready()) {
    tabs <- append(
      tabs,
      list(
        tabPanel(
          title = "Optimization",
          #uiOutput("optimize_plate_info"),
          uiOutput("plate_optimized_status"),
          uiOutput(outputId = "ui_optimization"),
          uiOutput(outputId = "ui_split_button")
        )
      )
    )
  }


  # Make sure to return the tabsetPanel with all tabs included
  do.call(tabsetPanel, tabs)

})

# Observe when data is selected again.
observeEvent(plate_data(), {
  print("New plate data loaded!")
  print(head(plate_data()))
  all_df <- create_list_of_dataframes(plate_data = plate_data(), study_accession = input$readxMap_study_accession,
                            experiment_accession = input$readxMap_experiment_accession_import)
  list_of_dataframes(all_df)
  combined_df <- create_original_df_combined(plate_data = plate_data(), list_of_dataframes = all_df)
  original_df_combined(combined_df)
})

observe({
  cat("type_x_status current value:\n")
  print(type_x_status())
})

observeEvent(all_completed(), {

  # cat("statuses:")
  # print(optimization_parsed_boolean())
  cat("all completed:")
  print(all_completed())
  #if (is_optimization_plate(plate_data()) && all_completed()) {
   #if (optimization_parsed_boolean() && all_completed()) {
  if (all_completed()) {
    optimization_ready(TRUE)
  } else {
    optimization_ready(FALSE)
  }
}, ignoreInit = TRUE)

observeEvent(input$split_opt_plates, {
  split_optimization_single_upload(study_accession = input$readxMap_study_accession, experiment_accession = input$readxMap_experiment_accession_import,
                                   plate_id = input$read_import_plate_id,
                                   plate_number = input$read_import_plate_number)

  # trigger refresh
  optimization_refresh(optimization_refresh() + 1)
})

observeEvent(input$testButton, {

  print("test")

  print(original_df_combined())

  print("unique plate types are")
  print(unique_plate_types())


})

observeEvent(input$savexMapButton, {
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), theads)
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard"), standard_data)
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_control"), control_data)
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_buffer"), buffer_data)
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_sample"), sample_data)
  removeTab(inputId = "body_panel_id", target="previewxMap")
  removeTab(inputId = "body_panel_id", target="headerxMap")
  removeTab(inputId = "body_panel_id", target="readxMap")

  select_query <- glue::glue_sql("
    SELECT DISTINCT
      xmap_header.study_accession,
      xmap_header.experiment_accession,
      xmap_header.study_accession AS study_name,
      xmap_header.experiment_accession AS experiment_name,
      xmap_header.workspace_id,
      xmap_users.project_name
    FROM madi_results.xmap_header
    JOIN madi_results.xmap_users ON xmap_header.workspace_id = xmap_users.workspace_id
    WHERE xmap_header.workspace_id = {userWorkSpaceID()}
  ;", .con = conn)

  query_result <- dbGetQuery(conn, select_query)
  reactive_df_study_exp(query_result)
  print("reactive_df_study_exp:loaded")


  initial_source <- obtain_initial_source(input$readxMap_study_accession)

 # initial_source <- unique(standard_data$source)[1]

  # Initialize study parameters for a user and study
  study_user_params_nrow <- nrow(fetch_study_configuration(study_accession = input$readxMap_study_accession
                                                             , user = currentuser()))
  if (study_user_params_nrow == 0) {
    intitialize_study_configurations(study_accession = input$readxMap_study_accession,
                                     user = currentuser(), initial_source = initial_source)


  }
})

### Layout template import
observeEvent(input$upload_experiment_files, {
  req(input$upload_experiment_files)
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         NEW EXPERIMENT FILES UPLOAD                      ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  cat("Study:", input$readxMap_study_accession, "\n")
  cat("Experiment:", input$readxMap_experiment_accession_import, "\n")

  # CRITICAL: Check if old data exists
  if (!is.null(batch_plate_data())) {
    old_files <- unique(batch_plate_data()$source_file)
    cat("⚠️  Old data detected. Clearing now...\n")
  }

  # Reset ALL batch reactives
  batch_plate_data(NULL)
  batch_metadata(NULL)
  bead_array_header_list(list())
  bead_array_plate_list(list())
  layout_template_sheets(list())
  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL
  ))

  current_exp_files <- input$upload_experiment_files
  cat("Number of NEW files being uploaded:", nrow(current_exp_files), "\n")
  cat("New files:\n")
  for (i in 1:nrow(current_exp_files)) {
    cat("  ", i, ". ", current_exp_files$name[i], "\n", sep="")
  }
  cat("\n")

  batch_data_list <- list()

  for (i in 1:nrow(current_exp_files)) {
    file_path <- current_exp_files$datapath[i]
    file_name <- current_exp_files$name[i]

    cat("Processing:", file_name, "\n")

    # Read full sheet to find where "Well" row is
    bead_array_plate <- openxlsx::read.xlsx(file_path, colNames = FALSE, sheet = 1)
    well_row <- which(bead_array_plate[,1] == "Well")

    # Header block (rows before "Well")
    header_info <- openxlsx::read.xlsx(
      file_path,
      rows = 1:(well_row - 1),
      colNames = FALSE,
      sheet = 1
    )

    # Parse the header metadata
    header_info <- parse_metadata_df(header_info)

    # Plate block (data starting from "Well" row)
    plate_data <- openxlsx::read.xlsx(
      file_path,
      startRow = well_row,
      sheet = 1,
      colNames = TRUE
    )

    names(plate_data) <- gsub("\\.", " ", names(plate_data))

    cat("  → Columns:", ncol(plate_data), "\n")

    # Remove completely empty rows
    plate_data <- plate_data[!apply(
      plate_data, 1,
      function(x) all(is.na(x) | trimws(x) == "" | trimws(x) == "NA")
    ),]

    cat("  → Rows after cleanup:", nrow(plate_data), "\n")

    batch_data_list[[file_name]] <- list(
      header = header_info,
      plate = plate_data
    )
  }

  # Extract header and plate lists
  header_list <- lapply(batch_data_list, `[[`, "header")
  header_list <- add_source_column(header_list)

  # ADD PLATE NUMBERS to headers
  cat("\nAdding plate numbers to headers...\n")
  for (i in seq_along(header_list)) {
    plate_num <- paste0("plate_", i)
    header_list[[i]]$plate <- plate_num

    # If plateid is NA or empty, use plate number
    if (is.na(header_list[[i]]$plateid[1]) || header_list[[i]]$plateid[1] == "") {
      header_list[[i]]$plateid <- plate_num
    }

    cat("  ", names(header_list)[i], "→ plate:", plate_num,
        "| plateid:", header_list[[i]]$plateid[1], "\n")
  }

  plate_list <- lapply(batch_data_list, `[[`, "plate")
  plate_list <- add_source_column(plate_list)

  cat("\nStoring bead array lists...\n")
  cat("  → Header list items:", length(header_list), "\n")
  cat("  → Plate list items:", length(plate_list), "\n")

  bead_array_header_list(header_list)
  bead_array_plate_list(plate_list)

  # Build combined plate data
  cat("\nCombining plate data...\n")
  all_plates <- process_uploaded_files(current_exp_files)

  cat("  → Combined dimensions:", nrow(all_plates), "rows x", ncol(all_plates), "cols\n")
  cat("  → Source files in combined data:",
      paste(unique(all_plates$source_file), collapse=", "), "\n")

  # Extract and log antigens
  metadata_cols <- c("source_file", "Well", "Type", "Description",
                     "% Agg Beads", "Sampling Errors", "Acquisition Time")
  antigen_cols <- names(all_plates)[!(names(all_plates) %in% metadata_cols)]
  cat("  → Antigens detected (", length(antigen_cols), "):\n", sep="")
  for (ag in antigen_cols) {
    cat("      •", ag, "\n")
  }

  batch_plate_data(all_plates)

  cat("\n✓ Upload complete!\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")

  showNotification(
    paste("Successfully uploaded", nrow(current_exp_files), "experiment file(s)"),
    type = "message",
    duration = 3
  )
})

observeEvent(input$upload_layout_file, {
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         UPLOADING LAYOUT FILE                            ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Reset layout-related reactives
  inLayoutFile(NULL)
  avaliableLayoutSheets(NULL)
  layout_template_sheets(list())

  # Reset validation state
  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL
  ))

  input_upload_layout_file <- input$upload_layout_file
  workspace_id <- userWorkSpaceID()
  currentuser <- currentuser()
  bead_array_header_list_data <- bead_array_header_list()

  inLayoutFile(input_upload_layout_file)

  if (is.null(input_upload_layout_file)) {
    cat("⚠️  No layout file provided\n")
    return()
  }

  cat("Layout file:", input_upload_layout_file$name, "\n\n")

  # CRITICAL: Check what data we're validating against
  cat("Checking batch_plate_data()...\n")
  if (is.null(batch_plate_data())) {
    cat("⚠️  ERROR: batch_plate_data() is NULL!\n")
    cat("   Cannot validate without plate data.\n")
    showNotification(
      "Error: No plate data found. Please upload experiment files first.",
      type = "error",
      duration = 10
    )
    return()
  }

  cat("  → Rows:", nrow(batch_plate_data()), "\n")
  cat("  → Source files:", paste(unique(batch_plate_data()$source_file), collapse=", "), "\n")

  # Extract antigens from current batch data
  metadata_cols <- c("source_file", "Well", "Type", "Description",
                     "% Agg Beads", "Sampling Errors", "Acquisition Time")
  current_antigens <- names(batch_plate_data())[!(names(batch_plate_data()) %in% metadata_cols)]
  cat("  → Current batch antigens (", length(current_antigens), "):\n", sep="")
  for (ag in current_antigens) {
    cat("      •", ag, "\n")
  }
  cat("\n")

  # Validate sheet names
  cat("Reading layout file...\n")
  sheets <- readxl::excel_sheets(input_upload_layout_file$datapath)
  validation <- check_sheet_names(input_upload_layout_file$datapath)
  if (!validation$valid) {
    showNotification(
      validation$message,
      type = "error",
      duration = NULL
    )
    return()
  }

  # CORRECTED: Call import_layout_file with only the file_path parameter
  all_sheets <- import_layout_file(input_upload_layout_file$datapath)

  if (!all_sheets$success) {
    showNotification(
      paste(all_sheets$messages, collapse = "\n"),
      type = "error",
      duration = NULL
    )
    return()
  }

  cat("  ✓ Layout sheets read successfully\n")
  cat("  → Available sheets:", paste(names(all_sheets$data), collapse=", "), "\n\n")

  # Assign sheet names
  names(all_sheets$data) <- sheets

  # Check antigens in layout vs current data
  layout_antigens <- unique(all_sheets$data[["antigen_list"]]$antigen_label_on_plate)
  cat("Layout file antigens (", length(layout_antigens), "):\n", sep="")
  for (ag in layout_antigens) {
    cat("  •", ag, "\n")
  }
  cat("\n")

  # Check for mismatches
  extra_in_layout <- setdiff(layout_antigens, current_antigens)
  missing_from_layout <- setdiff(current_antigens, layout_antigens)

  if (length(extra_in_layout) > 0) {
    cat("⚠️  WARNING: Layout has antigens NOT in current batch data:\n")
    for (ag in extra_in_layout) {
      cat("    ✗", ag, "\n")
    }
    cat("\n")
  }

  if (length(missing_from_layout) > 0) {
    cat("⚠️  WARNING: Current batch has antigens NOT in layout:\n")
    for (ag in missing_from_layout) {
      cat("    ✗", ag, "\n")
    }
    cat("\n")
  }

  # Store the layout data
  layout_template_sheets(all_sheets$data)

  # Construct batch header metadata
  batch_header <- construct_batch_upload_metadata(
    plates_map = all_sheets$data[["plates_map"]],
    plate_metadata_list = bead_array_header_list_data,
    workspace_id = workspace_id,
    currentuser = currentuser
  )

  # Prepare metadata
  batch_metadata_data <- combine_plate_metadata(head_list = batch_header)
  batch_metadata_data <- merge(
    batch_metadata_data,
    all_sheets$data[["plate_id"]][, c("plate_filename", "number_of_wells")],
    by.x = "file_name",
    by.y = "plate_filename",
    all.x = TRUE
  )

  names(batch_metadata_data)[names(batch_metadata_data) == "number_of_wells"] <- "n_wells"

  cat("Validating...\n")

  # Validate metadata
  validate_metadata_result <- validate_batch_plate_metadata(
    plate_metadata = batch_metadata_data,
    plate_id_data = all_sheets$data[["plate_id"]]
  )

  # Validate bead array data
  bead_array_validation <- validate_batch_bead_array_data(
    combined_plate_data = batch_plate_data(),
    antigen_import_list = all_sheets$data[["antigen_list"]],
    blank_keyword = input$batch_blank
  )

  # Update validation state
  if (bead_array_validation$is_valid && validate_metadata_result$is_valid) {
    batch_validation_state(list(
      is_validated = TRUE,
      is_uploaded = FALSE,
      validation_time = Sys.time(),
      upload_time = NULL,
      metadata_result = validate_metadata_result,
      bead_array_result = bead_array_validation
    ))
    batch_metadata(batch_metadata_data)

    cat("✓ VALIDATION PASSED!\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")

    showNotification(
      "Layout file validated successfully! Ready to upload to database.",
      type = "message",
      duration = 5
    )
  } else {
    batch_validation_state(list(
      is_validated = FALSE,
      is_uploaded = FALSE,
      validation_time = Sys.time(),
      upload_time = NULL,
      metadata_result = validate_metadata_result,
      bead_array_result = bead_array_validation
    ))

    cat("✗ VALIDATION FAILED\n")
    cat("  Metadata valid:", validate_metadata_result$is_valid, "\n")
    cat("  Bead array valid:", bead_array_validation$is_valid, "\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")

    showNotification(
      "Layout file uploaded but validation failed. Please review error messages.",
      type = "error",
      duration = 10
    )
  }
})

observeEvent(layout_template_sheets(), {
  cat("changed layout sheets")

})

observeEvent(input$view_layout_plate_id_sheet,{
  print("view layout plate id sheet")

  showModal(
    modalDialog(
      title = "Plate ID sheet ",
      rhandsontable::rHandsontableOutput("layout_plate_id_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_plate_id_sheet <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["plate_id"]])
    rhandsontable(layout_template_sheets()[["plate_id"]], readOnly = TRUE)
  })
})

observeEvent(input$view_layout_subject_group_sheet, {
  showModal(
    modalDialog(
      title = "Subject Map",
      rhandsontable::rHandsontableOutput("layout_subject_groups_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_subject_groups_sheet <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["subject_groups"]])
    rhandsontable(layout_template_sheets()[["subject_groups"]], readOnly = TRUE)
  })
})

observeEvent(input$view_layout_timepoint_sheet, {
  showModal(
    modalDialog(
      title = "Timepoint Map",
      rhandsontable::rHandsontableOutput("layout_timepoint_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_timepoint_sheet <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["timepoint"]])
    rhandsontable(layout_template_sheets()[["timepoint"]], readOnly = TRUE)
  })
})

observeEvent(input$view_layout_plates_map_sheet,{
  print("view layout plate map sheet")

  showModal(
    modalDialog(
      title = "Plate map sheet ",
      rhandsontable::rHandsontableOutput("layout_plates_map_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_plates_map_sheet <- rhandsontable::renderRHandsontable({
   req(layout_template_sheets()[["plates_map"]])
    rhandsontable(layout_template_sheets()[["plates_map"]], readOnly = TRUE)
  })

})

observeEvent(input$view_layout_antigen_list_sheet,{
  print("view layout antigen list sheet")

  showModal(
    modalDialog(
      title = "Antigen list sheet ",
      rhandsontable::rHandsontableOutput("layout_antigen_list_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_antigen_list_sheet <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["antigen_list"]])
    rhandsontable(layout_template_sheets()[["antigen_list"]], readOnly = TRUE)
  })

})

## Clear old data from layout when navigate back to layout template
observeEvent(input$xPonentFile, {
  if (input$xPonentFile == "Layout Template") {
    cat("Switched to Layout Template tab — clearing layout data\n")

    # Clear all layout-related reactive values
    inLayoutFile(NULL)
    avaliableLayoutSheets(NULL)
    layout_template_sheets(list())

    # Reset validation state
    batch_validation_state(list(
      is_validated = FALSE,
      is_uploaded = FALSE,
      validation_time = NULL,
      upload_time = NULL,
      metadata_result = NULL,
      bead_array_result = NULL
    ))

    # Remove the dynamically generated buttons
    output$view_layout_file_ui <- renderUI({NULL})

  }
})

observeEvent(input$upload_batch_button, {

  # Get input data
  batch_plates <- batch_plate_data()
  metadata_batch <- batch_metadata()
  layout_sheets <- layout_template_sheets()

  # Debug: Check subject_groups
  cat("\n=== DEBUG: subject_groups (subject_map) ===\n")
  subject_map <- layout_sheets[["subject_groups"]]
  print(head(subject_map))
  cat("Columns:", paste(names(subject_map), collapse = ", "), "\n")

  # Perform upload
  upload_result <- upload_batch_to_database(
    conn = conn,
    batch_plates = batch_plates,
    metadata_batch = metadata_batch,
    layout_sheets = layout_sheets
  )

  if (upload_result$success) {
    # Update validation state to reflect successful upload
    current_state <- batch_validation_state()
    batch_validation_state(list(
      is_validated = current_state$is_validated,
      is_uploaded = TRUE,  # Mark as uploaded
      validation_time = current_state$validation_time,
      upload_time = Sys.time(),
      metadata_result = current_state$metadata_result,
      bead_array_result = current_state$bead_array_result
    ))
  }

  # Handle result
  if (upload_result$already_exists) {
    showNotification(
      upload_result$message,
      type = "warning",
      duration = 5
    )
    return(NULL)
  }

  if (upload_result$success) {
    counts <- upload_result$counts
    detail_message <- sprintf(
      "Uploaded: %d headers, %d samples, %d standards, %d blanks, %d controls, %d antigens, %d visits",
      counts$header,
      counts$samples,
      counts$standards,
      counts$blanks,
      counts$controls,
      counts$antigens,
      counts$visits
    )

    showNotification("Batch Uploaded Successfully", type = "message", duration = 5)
    showNotification(detail_message, type = "message", duration = 10)

  } else {
    # Show main error
    showNotification(
      paste("Upload failed:", upload_result$message),
      type = "error",
      duration = NULL
    )

    # Show specific errors
    for (error_name in names(upload_result$errors)) {
      showNotification(
        paste(error_name, "error:", upload_result$errors[[error_name]]),
        type = "error",
        duration = NULL
      )
    }
  }
})

