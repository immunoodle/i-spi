
# HELPER FUNCTION: Extract plate number from text (filename, plateid, etc.)
# Looks for patterns like "plate_3", "plate 3", "plate.3", "plate3"
# This function is also defined in batch_layout_functions.R but is duplicated
# here to ensure availability in the upload_experiment_files observer.
extract_plate_number <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)

  # Try multiple patterns to extract plate number
  # Pattern 1: "plate" followed by separator and number (plate_3, plate 3, plate.3, plate-3)
  match1 <- regmatches(text, regexpr("[Pp]late[_\\s\\.-]+(\\d+)", text, perl = TRUE))
  if (length(match1) > 0 && nchar(match1) > 0) {
    num <- gsub("[^0-9]", "", match1)
    if (nchar(num) > 0) return(paste0("plate_", num))
  }

  # Pattern 2: Just "plate" followed immediately by number (plate3, plate1IgGtot...)
  match2 <- regmatches(text, regexpr("[Pp]late(\\d+)", text, perl = TRUE))
  if (length(match2) > 0 && nchar(match2) > 0) {
    num <- gsub("[^0-9]", "", match2)
    if (nchar(num) > 0) return(paste0("plate_", num))
  }

  return(NA_character_)
}

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
  # Get layout sheets
  sheets <- layout_template_sheets()

  cat("\n=== plate_layout_plots REACTIVE ===\n")
  cat("  sheets is NULL:", is.null(sheets), "\n")
  cat("  sheets length:", length(sheets), "\n")

  # Return NULL if no sheets
  if (is.null(sheets) || length(sheets) == 0) {
    cat("  → Returning NULL (no sheets)\n")
    return(NULL)
  }

  cat("  Sheet names:", paste(names(sheets), collapse = ", "), "\n")

  # Check for required sheets
  plates_map <- sheets[["plates_map"]]
  plate_id_data <- sheets[["plate_id"]]

  cat("  plates_map is NULL:", is.null(plates_map), "\n")
  cat("  plate_id_data is NULL:", is.null(plate_id_data), "\n")

  if (is.null(plates_map) || is.null(plate_id_data)) {
    cat("  → Returning NULL (missing required sheets)\n")
    return(NULL)
  }

  cat("  plates_map rows:", nrow(plates_map), "\n")
  cat("  plate_id_data rows:", nrow(plate_id_data), "\n")

  # Return NULL if empty data
  if (nrow(plates_map) == 0 || nrow(plate_id_data) == 0) {
    cat("  → Returning NULL (empty data)\n")
    return(NULL)
  }

  # Check required columns exist in plates_map
  required_plates_map_cols <- c("study_name", "experiment_name", "plate_number", "well", "specimen_type")
  missing_cols <- setdiff(required_plates_map_cols, names(plates_map))
  if (length(missing_cols) > 0) {
    cat("  ⚠️ plates_map missing columns:", paste(missing_cols, collapse = ", "), "\n")
    cat("  → Returning NULL\n")
    return(NULL)
  }

  # Check required columns exist in plate_id_data
  required_plate_id_cols <- c("study_name", "experiment_name", "plate_number", "number_of_wells")
  missing_plate_id_cols <- setdiff(required_plate_id_cols, names(plate_id_data))
  if (length(missing_plate_id_cols) > 0) {
    cat("  ⚠️ plate_id_data missing columns:", paste(missing_plate_id_cols, collapse = ", "), "\n")
    cat("  → Returning NULL\n")
    return(NULL)
  }

  cat("  ✓ All required columns present\n")
  cat("  Calling plot_plate_layout()...\n")

  # Call the plotting function with error handling
  result <- tryCatch({
    plots <- plot_plate_layout(plates_map, plate_id_data)
    cat("  ✓ plot_plate_layout() returned", length(plots), "plots\n")
    if (length(plots) > 0) {
      cat("  Plot names:", paste(names(plots), collapse = ", "), "\n")
    }
    plots
  }, error = function(e) {
    cat("  ✗ ERROR in plot_plate_layout():", conditionMessage(e), "\n")
    cat("  Stack trace:\n")
    print(sys.calls())
    return(NULL)
  })

  cat("=================================\n\n")
  return(result)
})

# plate_layout_plots <- reactive({
#   # Get layout sheets
#   sheets <- layout_template_sheets()
#
#   cat("plate_layout_plots: evaluating, sheets length =", length(sheets), "\n")
#
#   # Return NULL if no sheets
#   if (is.null(sheets) || length(sheets) == 0) {
#     cat("plate_layout_plots: returning NULL (no sheets)\n")
#     return(NULL)
#   }
#
#   # Check for required sheets
#   plates_map <- sheets[["plates_map"]]
#   plate_id_data <- sheets[["plate_id"]]
#
#   if (is.null(plates_map) || is.null(plate_id_data)) {
#     return(NULL)
#   }
#
#   # Return NULL if empty data
#   if (nrow(plates_map) == 0 || nrow(plate_id_data) == 0) {
#     cat("plate_layout_plots: returning NULL")
#     return(NULL)
#   }
#
#   # Call the plotting function
#   plot_plate_layout(plates_map, plate_id_data)
# })

# plate_layout_plots <- reactive({
#   req(layout_template_sheets()[["plates_map"]])
#   req(layout_template_sheets()[["plate_id"]])
#
#   plates_map <- layout_template_sheets()[["plates_map"]]
#   plate_id_data <- layout_template_sheets()[["plate_id"]]
#
#   # Call your function
#   plot_plate_layout(plates_map, plate_id_data)
# })


### Outputs
output$upload_path_text <- renderText({
  paste(stri_replace_all_charclass(Sys.getenv("upload_template_path"), "\\p{WHITE_SPACE}", ""))
})

output$fileUploaded <- reactive({
  return(!is.null(getData()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

output$delete_study_ui <- renderUI({
  tabRefreshCounter()$import_tab
  if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab" & input$study_tabs == "delete_study") {
    if (input$readxMap_study_accession != "Click here") {
      import_plate_data_title <- paste("Delete", input$readxMap_study_accession, "Plate Data", sep = " ")
      tagList(
        fluidPage(

        )
      )
    } else {
      import_plate_data_title<- paste("Choose a study for deleting plate data")
    }
  }
})

# UI COMPONENT: Delete Study UI
output$delete_study_ui <- renderUI({
  tabRefreshCounter()$import_tab
  if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab" & input$study_tabs == "delete_study") {
    if (input$readxMap_study_accession != "Click here") {
      selected_study_accession <- input$readxMap_study_accession
      import_plate_data_title <- paste("Delete", selected_study_accession, "Plate Data", sep = " ")

      tagList(
        fluidPage(
          fluidRow(
            column(12,
                   h3(import_plate_data_title, style = "color: #d9534f; margin-bottom: 20px;"),
                   hr()
            )
          ),
          fluidRow(
            column(12,
                   h4("Tables with data for this study:", style = "margin-bottom: 15px;"),
                   div(
                     style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px; border: 1px solid #ddd;",
                     DT::dataTableOutput("delete_study_table")
                   )
            )
          ),
          fluidRow(
            column(12,
                   div(
                     style = "margin-top: 25px; padding: 15px; background-color: #fcf8e3; border: 1px solid #faebcc; border-radius: 5px;",
                     icon("exclamation-triangle", style = "color: #8a6d3b;"),
                     span(
                       "Warning: Deleting study data is permanent and cannot be undone.",
                       style = "color: #8a6d3b; font-weight: bold; margin-left: 10px;"
                     )
                   )
            )
          ),
          fluidRow(
            column(12,
                   div(
                     style = "margin-top: 20px; text-align: center;",
                     actionButton(
                       "delete_study_btn",
                       label = tagList(icon("trash"), "Delete Study Data"),
                       class = "btn-danger btn-lg",
                       style = "padding: 12px 30px; font-size: 16px;"
                     )
                   )
            )
          )
        )
      )
    } else {
      tagList(
        fluidPage(
          fluidRow(
            column(12,
                   div(
                     style = "text-align: center; padding: 50px; color: #777;",
                     icon("hand-pointer", style = "font-size: 48px; margin-bottom: 20px;"),
                     h4("Choose a study for deleting plate data")
                   )
            )
          )
        )
      )
    }
  }
})

# SERVER: Reactive to fetch row counts for selected study
delete_study_data <- reactive({
  req(input$readxMap_study_accession)
  req(input$readxMap_study_accession != "Click here")

  selected_study_accession <- input$readxMap_study_accession

  query <- glue_sql(
    "SELECT * FROM public.count_study_accession_rows({selected_study_accession}) WHERE row_count > 0;",
    selected_study_accession = selected_study_accession,
    .con = conn
  )

  tryCatch({
    result <- dbGetQuery(conn, query)
    result
  }, error = function(e) {
    shinyalert(
      title = "Error",
      text = paste("Error fetching data:", e$message),
      type = "error"
    )
    data.frame(
      schema_name = character(),
      table_name = character(),
      row_count = integer()
    )
  })
})

# SERVER: Render the DataTable
output$delete_study_table <- DT::renderDataTable({
  req(delete_study_data())

  data <- delete_study_data()

  if (nrow(data) == 0) {
    return(
      DT::datatable(
        data.frame(Message = "No data found for this study accession"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    )
  }

  # Add total row
  total_row <- data.frame(
    schema_name = "TOTAL",
    table_name = "",
    row_count = sum(data$row_count)
  )
  data_with_total <- rbind(data, total_row)

  DT::datatable(
    data_with_total,
    colnames = c("Schema", "Table", "Row Count"),
    rownames = FALSE,
    options = list(
      dom = 't',
      paging = FALSE,
      ordering = FALSE,
      columnDefs = list(
        list(className = 'dt-center', targets = 2),
        list(className = 'dt-left', targets = c(0, 1))
      )
    ),
    class = "table table-striped table-bordered"
  ) %>%
    DT::formatStyle(
      columns = c("schema_name", "table_name", "row_count"),
      target = "row",
      backgroundColor = DT::styleEqual("TOTAL", "#f5f5f5"),
      fontWeight = DT::styleEqual("TOTAL", "bold")
    ) %>%
    DT::formatRound(columns = "row_count", digits = 0, mark = ",")
})

output$readxMapData <- renderUI({
 tabRefreshCounter()$import_tab
  if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab" & input$study_tabs == "import_tab") {
  if (input$readxMap_study_accession != "Click here") {
    import_plate_data_title <- paste("Import", input$readxMap_study_accession, "Plate Data", sep = " ")
    tagList(
      fluidPage(
        tagList(
          h3(import_plate_data_title),
          bsCollapsePanel(
            "Instructions for Importing and Uploading batches of plate files.",
            p("This is where we can load plate data from either Raw excel files, the xPONENT format or batches of raw files."),
            # Section A: Before uploading
            h4("A. Before Uploading Batch Plate Files"),
            p("Ensure you have completed the following:"),
            tags$ol(
              tags$li("Created or loaded a Project - Navigate to \"Create, Add, and Load Projects\" in the sidebar."),
              tags$li("Created or selected a Study - Type a new study name (up to 15 characters) or select an existing one."),
              tags$li("Selected the sidebar option to Import Plate Data."),
              tags$li("Created or selected an Experiment - Each experiment represents a specific assay run or feature (e.g., \"IgG_total\", \"FcgR2a\")."),
              tags$li("Selected Layout Template as the file format.")
            ),

            # Section B: Upload Experiment Files
            h4("B. Upload Experiment Files"),
            tags$ol(
              tags$li("Click \"Select all experiment raw data files\"."),
              tags$li("Select all plate files (.xlsx) for this batch."),
              tags$li("Enter a Feature name (up to 15 characters) - this identifies the assay type (e.g., \"IgGtot\", \"FcgR2a\").")
            ),

            # Section C: Configure Plate Settings
            h4("C. Configure Plate Settings"),
            tags$ol(
              tags$li(
                strong("Number of Wells:"),
                p("Select the plate format (6, 12, 24, 48, 96, 384, or 1536 wells).")
              ),
              tags$li(
                strong("Description Delimiter:"),
                p("If your Description field contains data, select the character that separates elements:"),
                tags$ul(
                  tags$li(tags$code("_"), " (underscore) - most common"),
                  tags$li(tags$code("|"), " (pipe)"),
                  tags$li(tags$code(":"), " (colon)"),
                  tags$li(tags$code("-"), " (hyphen)")
                )
              ),
              tags$li(
                strong("Optional Elements:"),
                p("Check/uncheck to include group assignments:"),
                tags$ul(
                  tags$li("☑ SampleGroupA"),
                  tags$li("☑ SampleGroupB")
                )
              ),
              tags$li(
                strong("Element Order:"),
                p("Drag and drop to match your Description field structure:"),
                tags$ul(
                  tags$li("Default order: PatientID → TimePeriod → DilutionFactor"),
                  tags$li("With groups: PatientID → SampleGroupA → SampleGroupB → TimePeriod → DilutionFactor")
                )
              )
            ),

            # Section D: Generate Layout Template
            h4("D. Generate Layout Template"),
            tags$ol(
              tags$li("Click \"Generate a Layout file\"."),
              tags$li("Save the Excel file to your computer."),
              tags$li("The template contains pre-populated sheets based on your plate data.")
            ),

            # Section E: Review and Edit Layout Template
            h4("E. Review and Edit Layout Template"),
            p("Open the generated Excel file and verify/edit each sheet:"),

            tags$h5("Sheet: plate_id"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("study_name"), tags$td("Study identifier")),
                tags$tr(tags$td("experiment_name"), tags$td("Experiment identifier")),
                tags$tr(tags$td("number_of_wells"), tags$td("Plate format (96, 384, etc.)")),
                tags$tr(tags$td("plate_number"), tags$td("Internal plate identifier")),
                tags$tr(tags$td("plateid"), tags$td("Plate ID from instrument")),
                tags$tr(tags$td("plate_filename"), tags$td("Original file path"))
              )
            ),

            tags$h5("Sheet: subject_groups"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("study_name"), tags$td("Study identifier")),
                tags$tr(tags$td("subject_id"), tags$td("Unique patient/subject identifier")),
                tags$tr(tags$td("groupa"), tags$td("First categorical grouping")),
                tags$tr(tags$td("groupb"), tags$td("Second categorical grouping"))
              )
            ),

            tags$h5("Sheet: timepoint"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("study_name"), tags$td("Study identifier")),
                tags$tr(tags$td("timepoint_tissue_abbreviation"), tags$td("Short timepoint code")),
                tags$tr(tags$td("tissue_type"), tags$td("e.g., \"blood\"")),
                tags$tr(tags$td("tissue_subtype"), tags$td("e.g., \"serum\"")),
                tags$tr(tags$td("description"), tags$td("Full timepoint description")),
                tags$tr(tags$td("min_time_since_day_0"), tags$td("Minimum days from baseline")),
                tags$tr(tags$td("max_time_since_day_0"), tags$td("Maximum days from baseline"))
              )
            ),

            tags$h5("Sheet: antigen_list"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("antigen_label_on_plate"), tags$td("Column name from plate file")),
                tags$tr(tags$td("antigen_abbreviation"), tags$td("Short name for analysis")),
                tags$tr(tags$td("antigen_family"), tags$td("Grouping category")),
                tags$tr(tags$td("standard_curve_max_concentration"), tags$td("Upper limit for curve fitting"))
              )
            ),

            tags$h5("Sheet: plates_map"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("study_name"), tags$td("Study identifier")),
                tags$tr(tags$td("plate_number"), tags$td("Plate identifier")),
                tags$tr(tags$td("well"), tags$td("Well position")),
                tags$tr(tags$td("specimen_type"), tags$td("X, S, B, C, or empty")),
                tags$tr(tags$td("specimen_source"), tags$td("Source material identifier")),
                tags$tr(tags$td("specimen_dilution_factor"), tags$td("Numeric dilution")),
                tags$tr(tags$td("subject_id"), tags$td("Links to subject_groups")),
                tags$tr(tags$td("biosample_id_barcode"), tags$td("Sample barcode")),
                tags$tr(tags$td("timepoint_tissue_abbreviation"), tags$td("Links to timepoint sheet"))
              )
            ),

            # Section F: Upload Completed Layout
            h4("F. Upload Completed Layout"),
            tags$ol(
              tags$li("Click \"Upload a completed layout file\"."),
              tags$li("Select your edited layout template."),
              tags$li("Review the plate layout visualization."),
              tags$li(
                "Configure Blank and Empty Well Handling:",
                tags$ul(
                  tags$li("\"Skip Empty Wells\" - removes blank entries"),
                  tags$li("\"Use as Blank\" - treats as background controls")
                )
              )
            ),

            # Section G: Validate and Upload
            h4("G. Validate and Upload"),
            tags$ol(
              tags$li("Check the Batch Validated badge appears (green checkmark)."),
              tags$li("If validation fails, review error messages and correct issues."),
              tags$li("Click \"Upload Batch\" to store data in database."),
              tags$li("Verify Batch Uploaded badge appears.")
            ),

            style = "warning"
          )
        ),
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
                     selected = "Layout Template",
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
                         conditionalPanel(
                           condition = "output.descriptionHasContent",
                           tags$div(
                             class = "element-controls",
                             tags$span(style = "font-weight: 600; align-self: center;", "Description Delimiter:"),
                             radioGroupButtons(
                               inputId = "description_delimiter",
                               label = NULL,
                               choices = setNames(c("_", "|", ":", "-"), c("_", "|", ":", "-")),
                               selected = "_",
                               size = "sm",
                               status = "outline-primary"
                             )
                           )
                           ),
                          conditionalPanel(
                           condition = "output.descriptionHasSufficientElements",
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
                           )
                           ),
                         conditionalPanel(
                           condition = "output.descriptionHasSufficientElements",
                           uiOutput("order_input_ui")
                         ),
                         conditionalPanel(
                           condition = "output.descriptionHasContent",
                           uiOutput("bcsorder_input_ui")
                         )
                         ,
                         downloadButton("blank_layout_file", "Generate a Layout file")
                       ),
                       fileInput("upload_layout_file"
                                 , label="Upload a completed layout file (only accepts xlsx, xls)"
                                 , accept=c(".xlsx",".xls")
                                 , multiple=FALSE)
                     )
                   ),
                     ),
                     conditionalPanel(
                       condition = "output.hasLayoutTemplateSheets",
                       uiOutput("description_warning_ui"),
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
})

output$description_warning_ui <- renderUI({
  # Explicit dependency
  status <- description_status()

  # Also depend on batch_plate_data to clear when data is cleared
  plate_data <- batch_plate_data()

  # Return NULL if no plate data or status not checked
  if (is.null(plate_data) || !status$checked) {
    return(NULL)
  }

  if (!status$has_content) {
    # Warning for completely blank Description
    tags$div(
      style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
        " Description Field is Blank",
        style = "margin-top: 0; color: #856404;"
      ),
      tags$p("The Description field in your plate data is empty. Default values will be used:"),
      tags$ul(
        tags$li("Subject ID: '1'"),
        tags$li("Sample Dilution Factor: 1"),
        tags$li("Timeperiod: 'T0'"),
        tags$li("Groups: 'Unknown'")
      ),
      tags$p(
        tags$strong("You will need to manually update the layout template with correct values before uploading."),
        style = "margin-bottom: 0; color: #856404;"
      )
    )
  } else if (!status$has_sufficient_elements) {
    # Warning for insufficient elements
    tags$div(
      style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
        " Insufficient Description Elements",
        style = "margin-top: 0; color: #856404;"
      ),
      tags$p(sprintf(
        "The Description field has only %d element(s), but at least %d are required.",
        status$min_elements_found,
        status$required_elements
      )),
      tags$p("Missing fields will be filled with default values. Manual update may be required.")
    )
  } else {
    return(NULL)
  }
})

# output$description_warning_ui <- renderUI({
#   status <- description_status()
#
#   if (!status$checked) {
#     return(NULL)
#   }
#
#   if (!status$has_content) {
#     # Warning for completely blank Description
#     tags$div(
#       style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
#       tags$h5(
#         tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
#         " Description Field is Blank",
#         style = "margin-top: 0; color: #856404;"
#       ),
#       tags$p("The Description field in your plate data is empty. Default values will be used:"),
#       tags$ul(
#         tags$li("Subject ID: '1'"),
#         tags$li("Sample Dilution Factor: 1"),
#         tags$li("Timeperiod: 'T0'"),
#         tags$li("Groups: 'Unknown'")
#       ),
#       tags$p(
#         tags$strong("You will need to manually update the layout template with correct values before uploading."),
#         style = "margin-bottom: 0; color: #856404;"
#       )
#     )
#   } else if (!status$has_sufficient_elements) {
#     # Warning for insufficient elements
#     tags$div(
#       style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
#       tags$h5(
#         tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
#         " Insufficient Description Elements",
#         style = "margin-top: 0; color: #856404;"
#       ),
#       tags$p(sprintf(
#         "The Description field has only %d element(s), but at least %d are required.",
#         status$min_elements_found,
#         status$required_elements
#       )),
#       tags$p("Missing fields will be filled with default values. Manual update may be required.")
#     )
#   } else {
#     return(NULL)
#   }
# })

output$hasExperimentPath <- reactive({
  path_df <- input$upload_experiment_files   # fileInput returns a data frame
  !is.null(path_df) && nrow(path_df) > 0
})

outputOptions(output, "hasExperimentPath", suspendWhenHidden = FALSE)

# =============================================================================
# NEW REACTIVE: hasLayoutTemplateSheets
# =============================================================================
# This reactive is used by the conditionalPanel to control visibility of
# view_layout_file_ui, plate_layout_selector, selected_plate_layout_plot,
# and description_warning_ui outputs after layout file is uploaded and processed.
# =============================================================================
output$hasLayoutTemplateSheets <- reactive({
  sheets <- layout_template_sheets()

  # Return TRUE only if sheets exist and contain required data
  has_sheets <- !is.null(sheets) && length(sheets) > 0

  if (has_sheets) {
    # Additional check: verify required sheets exist
    required <- c("plates_map", "plate_id")
    has_required <- all(required %in% names(sheets))

    cat("hasLayoutTemplateSheets: sheets=", length(sheets),
        ", has_required=", has_required, "\n")

    return(has_required)
  }

  return(FALSE)
})

# CRITICAL: Must not suspend when hidden to work with conditionalPanel
outputOptions(output, "hasLayoutTemplateSheets", suspendWhenHidden = FALSE)

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

    desc_status <- description_status()

    generate_layout_template(
      all_plates = all_plates,
      study_accession = input$readxMap_study_accession,
      experiment_accession = input$readxMap_experiment_accession_import,
      n_wells = input$n_wells_on_plate,
      header_list = bead_array_header_list(),
      output_file = file,
      # NEW: Pass description status for handling defaults
      description_status = desc_status,
      delimiter = if (desc_status$has_content) input$description_delimiter else "_",
      element_order = if (desc_status$has_sufficient_elements) input$XElementOrder else c("PatientID", "TimePeriod", "DilutionFactor"),
      bcs_element_order = if (desc_status$has_content) input$BCSElementOrder else c("Source", "DilutionFactor")
    )

    # Show notification if defaults were applied
    if (!desc_status$has_content || !desc_status$has_sufficient_elements) {
      showNotification(
        "Layout template generated with default values. Please review and update before uploading.",
        type = "warning",
        duration = 10
      )
    }

    cat("✓ Layout template generated!\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")
  })

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
  cat("\n=== plate_layout_selector renderUI ===\n")

  # Explicit dependency on layout_template_sheets
  sheets <- layout_template_sheets()

  cat("  sheets length:", length(sheets), "\n")

  # Return NULL if no data
  if (is.null(sheets) || length(sheets) == 0) {
    cat("  → Returning NULL (no sheets)\n")
    return(NULL)
  }

  # Check for required sheets
  if (is.null(sheets[["plates_map"]]) || is.null(sheets[["plate_id"]])) {
    cat("  → Returning NULL (missing plates_map or plate_id)\n")
    return(NULL)
  }

  # Get the plots
  plots <- plate_layout_plots()

  cat("  plots is NULL:", is.null(plots), "\n")
  cat("  plots length:", length(plots), "\n")

  # Return NULL if no plots
  if (is.null(plots) || length(plots) == 0) {
    cat("  → Returning NULL (no plots generated)\n")
    return(NULL)
  }

  cat("  ✓ Rendering radioGroupButtons with", length(plots), "choices\n")
  cat("  Choices:", paste(names(plots), collapse = ", "), "\n")
  cat("=====================================\n\n")

  shinyWidgets::radioGroupButtons(
    inputId = "select_plate_layout_plot",
    label = "Select Plate Layout:",
    choices = names(plots),
    selected = names(plots)[1],
    status = "success"
  )
})

# output$plate_layout_selector <- renderUI({
#   # Explicit dependency on layout_template_sheets
#   sheets <- layout_template_sheets()
#
#   cat("plate_layout_selector: sheets length =", length(sheets), "\n")
#
#   # Return NULL if no data
#   if (is.null(sheets) || length(sheets) == 0) {
#     return(NULL)
#   }
#
#   # Check for required sheets
#   if (is.null(sheets[["plates_map"]]) || is.null(sheets[["plate_id"]])) {
#     return(NULL)
#   }
#
#   # Get the plots
#   plots <- plate_layout_plots()
#
#   # Return NULL if no plots
#   if (is.null(plots) || length(plots) == 0) {
#     return(NULL)
#   }
#
#   shinyWidgets::radioGroupButtons(
#     inputId = "select_plate_layout_plot",
#     label = "Select Plate Layout:",
#     choices = names(plots),
#     selected = names(plots)[1],
#     status = "success"
#   )
# })

# output$plate_layout_selector <- renderUI({
#   req(plate_layout_plots())
#   plots <- plate_layout_plots()
#
#   shinyWidgets::radioGroupButtons(
#     inputId = "select_plate_layout_plot",
#     label = "Select Plate Layout:",
#     choices = names(plots),
#     selected = names(plots)[1],
#     status = "success"
#   )
# })

output$selected_plate_layout_plot <- renderPlotly({
  cat("\n=== selected_plate_layout_plot renderPlotly ===\n")

  # Explicit dependencies
  sheets <- layout_template_sheets()
  cat("  sheets length:", length(sheets), "\n")

  # Return NULL if no layout sheets
  if (is.null(sheets) || length(sheets) == 0) {
    cat("  → Returning NULL (no sheets)\n")
    return(NULL)
  }

  # Get plots FIRST (before requiring input selection)
  plots <- plate_layout_plots()

  cat("  plots is NULL:", is.null(plots), "\n")
  cat("  plots length:", length(plots), "\n")

  # Return NULL if no plots
  if (is.null(plots) || length(plots) == 0) {
    cat("  → Returning NULL (no plots)\n")
    return(NULL)
  }

  # Check if selection input exists and has a value
  selection <- input$select_plate_layout_plot
  cat("  input$select_plate_layout_plot:", selection, "\n")

  # If no selection yet, default to first plot
  if (is.null(selection) || selection == "" || !selection %in% names(plots)) {
    selection <- names(plots)[1]
    cat("  → Using default selection:", selection, "\n")
  }

  cat("  ✓ Rendering plot for:", selection, "\n")
  cat("=============================================\n\n")

  plots[[selection]]
})

# output$selected_plate_layout_plot <- renderPlotly({
#   # Explicit dependencies
#   sheets <- layout_template_sheets()
#
#   cat("selected_plate_layout_plot: sheets length =", length(sheets), "\n")
#
#   # Return NULL if no layout sheets
#   if (is.null(sheets) || length(sheets) == 0) {
#     return(NULL)
#   }
#
#   # Require selection
#   req(input$select_plate_layout_plot)
#
#   # Get plots
#   plots <- plate_layout_plots()
#
#   # Return NULL if no plots or selection not in plots
#   if (is.null(plots) || length(plots) == 0) {
#     cat("selected_plate_layout_plot: returning NULL (no plots)\n")
#     return(NULL)
#   }
#
#   if (is.null(plots) || !input$select_plate_layout_plot %in% names(plots)) {
#     return(NULL)
#   }
#
#   cat("selected_plate_layout_plot: rendering plot for", input$select_plate_layout_plot, "\n")
#   plots[[input$select_plate_layout_plot]]
# })

# output$selected_plate_layout_plot <- renderPlotly({
#   req(input$select_plate_layout_plot)
#   req(plate_layout_plots())
#   plots <- plate_layout_plots()
#   plots[[input$select_plate_layout_plot]]
# })

output$batch_validation_status <- renderUI({
  # Explicit dependency on batch state
  state <- batch_validation_state()

  # Also depend on layout sheets to trigger re-render when cleared
  sheets <- layout_template_sheets()

  # Return "not validated" badge if no state or no sheets
  if (is.null(state) || is.null(sheets) || length(sheets) == 0) {
    return(createValidateBatchBadge(FALSE))
  }

  createValidateBatchBadge(state$is_validated)
})

# output$batch_validation_status <- renderUI({
#   state <- batch_validation_state()
#   req(state)  # Ensure state exists
#   createValidateBatchBadge(state$is_validated)
# })

output$batch_invalid_messages <- renderTable({
  state <- batch_validation_state()

  # Also depend on layout sheets
  sheets <- layout_template_sheets()

  # Return NULL if no data or validation passed
  if (is.null(state) || is.null(sheets) || length(sheets) == 0) {
    return(NULL)
  }

  if (!state$is_validated && !is.null(state$metadata_result) && !is.null(state$bead_array_result)) {
    create_batch_invalid_message_table(state$metadata_result, state$bead_array_result)
  } else {
    NULL
  }
})

# output$batch_invalid_messages <- renderTable({
#   state <- batch_validation_state()
#   req(state)
#
#   if (!state$is_validated && !is.null(state$metadata_result) && !is.null(state$bead_array_result)) {
#     create_batch_invalid_message_table(state$metadata_result, state$bead_array_result)
#   } else {
#     NULL
#   }
# })
output$upload_batch_data_button <- renderUI({
  # Explicit dependencies for proper invalidation
  metadata <- batch_metadata()
  state <- batch_validation_state()
  sheets <- layout_template_sheets()

  # Return NULL if no metadata or sheets (cleared state)
  if (is.null(metadata) || is.null(sheets) || length(sheets) == 0) {
    return(NULL)
  }

  req(state)

  metadata_batch <- metadata
  batch_study_accession <- unique(metadata$study_name)
  batch_experiment_accession <- unique(metadata$experiment_name)

  # FIXED: Use plate_id for checking existing plates (consistent with database column)
  plates_to_upload <- unique(metadata$plate_id)

  # Debug logging
  cat("\n=== UI DUPLICATE CHECK DEBUG ===\n")
  cat("Checking for existing plates with plate_id values:\n")
  cat(paste(plates_to_upload, collapse = ", "), "\n")
  cat("================================\n")

  # FIXED: Use proper parameterized query with glue_sql
  # Do not pre-format the plate list - let glue_sql handle it
  query <- glue_sql("
    SELECT plate_id
    FROM madi_results.xmap_header
    WHERE study_accession = {batch_study_accession}
    AND experiment_accession IN ({batch_experiment_accession*})
    AND plate_id IN ({plates_to_upload*});
  ", .con = conn)
  existing_plates <- DBI::dbGetQuery(conn, query)

  plates_exist_in_db <- nrow(existing_plates) > 0

  cat("Plates already in DB:", plates_exist_in_db, "\n")
  if (plates_exist_in_db) {
    cat("Found existing:", paste(existing_plates$plate_id, collapse = ", "), "\n")
  }

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

# output$upload_batch_data_button <- renderUI({
#   # Explicit dependencies for proper invalidation
#   metadata <- batch_metadata()
#   state <- batch_validation_state()
#   sheets <- layout_template_sheets()
#
#   # Return NULL if no metadata or sheets (cleared state)
#   if (is.null(metadata) || is.null(sheets) || length(sheets) == 0) {
#     return(NULL)
#   }
#
#   req(state)
#
#   metadata_batch <<- metadata
#   batch_study_accession <<- unique(metadata$study_name)
#   batch_experiment_accession <<- unique(metadata$experiment_name)
#   plates_to_upload <<- unique(metadata$plate_id)
#
#   # Build SQL list
#   plate_list_sql <<- paste0("'", paste(plates_to_upload, collapse = "', '"), "'")
#
#   query <- glue_sql("
#     SELECT plate_id
#     FROM madi_results.xmap_header
#     WHERE study_accession = {batch_study_accession}
#     AND experiment_accession IN ({batch_experiment_accession*})
#     AND plate_id IN ({plate_list_sql*});
#   ", .con = conn)
#   existing_plates <- DBI::dbGetQuery(conn, query)
#
#   plates_exist_in_db <- nrow(existing_plates) > 0
#
#   # Determine badge status - use state$is_uploaded OR database check
#   show_uploaded_badge <- state$is_uploaded || plates_exist_in_db
#
#   badge <- createUploadedBatchBadge(show_uploaded_badge)
#
#   # Show button only if validated AND not yet uploaded/existing
#   button <- if (state$is_validated && !plates_exist_in_db && !state$is_uploaded) {
#     actionButton("upload_batch_button", "Upload Batch")
#   } else {
#     NULL
#   }
#
#   tagList(
#     badge,
#     br(),
#     button
#   )
# })

# output$upload_batch_data_button <- renderUI({
#   req(batch_metadata())
#   state <- batch_validation_state()
#   req(state)
#
#   metadata_batch <<- batch_metadata()
#   batch_study_accession <<- unique(metadata_batch$study_name)
#   batch_experiment_accession <<- unique(metadata_batch$experiment_name)
#   plates_to_upload <<- unique(metadata_batch$plate_id)
#
#   # Build SQL list
#   plate_list_sql <<- paste0("'", paste(plates_to_upload, collapse = "', '"), "'")
#
#   query <- glue_sql("
#     SELECT plate_id
#     FROM madi_results.xmap_header
#     WHERE study_accession = {batch_study_accession}
#     AND experiment_accession IN ({batch_experiment_accession*})
#     AND plate_id IN ({plate_list_sql*});
#   ", .con = conn)
#   existing_plates <- DBI::dbGetQuery(conn, query)
#
#   plates_exist_in_db <- nrow(existing_plates) > 0
#
#   # Determine badge status - use state$is_uploaded OR database check
#   show_uploaded_badge <- state$is_uploaded || plates_exist_in_db
#
#   badge <- createUploadedBatchBadge(show_uploaded_badge)
#
#   # Show button only if validated AND not yet uploaded/existing
#   button <- if (state$is_validated && !plates_exist_in_db && !state$is_uploaded) {
#     actionButton("upload_batch_button", "Upload Batch")
#   } else {
#     NULL
#   }
#
#   tagList(
#     badge,
#     br(),
#     button
#   )
# })

output$view_layout_file_ui <- renderUI({
  # Explicit dependency on layout_template_sheets
  sheets <- layout_template_sheets()

  # Return NULL if sheets is NULL or empty
  if (is.null(sheets) || length(sheets) == 0) {
    return(NULL)
  }

  # Verify required sheets exist
  required_sheets <- c("plate_id", "subject_groups", "timepoint", "plates_map", "antigen_list")
  available_sheets <- names(sheets)

  if (!all(required_sheets %in% available_sheets)) {
    return(NULL)
  }

  fluidRow(
    column(2, actionButton("view_layout_plate_id_sheet", "View Layout Plate ID")),
    column(2, actionButton("view_layout_subject_group_sheet", "View Layout Subject Map")),
    column(2, actionButton("view_layout_timepoint_sheet", "View Layout Timepoint")),
    column(2, actionButton("view_layout_plates_map_sheet", "View Layout Plate Map")),
    column(2, actionButton("view_layout_antigen_list_sheet", "View Layout Antigen List"))
  )
})

# output$view_layout_file_ui <- renderUI({
#   req(layout_template_sheets())
#   req(length(layout_template_sheets()) > 0)
#   fluidRow(
#     column(2, actionButton("view_layout_plate_id_sheet", "View Layout Plate ID")),
#     column(2, actionButton("view_layout_subject_group_sheet", "View Layout Subject Map")),
#     column(2, actionButton("view_layout_timepoint_sheet", "View Layout Timepoint")),
#     column(2, actionButton("view_layout_plates_map_sheet", "View Layout Plate Map")),
#     column(2, actionButton("view_layout_antigen_list_sheet", "View Layout Antigen List"))
#   )
# })

output$current_context_display <- renderText({
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  paste0(
    "Study: ", input$readxMap_study_accession, " | ",
    "Experiment: ", input$readxMap_experiment_accession_import
  )
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

    # RESET ALL BATCH REACTIVES
    reset_batch_reactives()

    # RESET ALL BATCH UI ELEMENTS
    reset_batch_ui(
      session = session,
      include_experiment_files = TRUE,
      include_layout_file = TRUE
    )

    # NOTE: Output invalidation is handled by clearing layout_template_sheets()
    # in reset_batch_reactives(). The outputs view_layout_file_ui,
    # plate_layout_selector, selected_plate_layout_plot, and description_warning_ui
    # already check for NULL/empty layout_template_sheets() and return NULL.
    # DO NOT re-assign outputs here as it breaks the reactive chain.

    # RESET EXPERIMENT DROPDOWN
    # Clear the experiment selection when study changes
    updateSelectizeInput(
      session = session,
      inputId = "readxMap_experiment_accession_import",
      selected = "Click here"
    )

    # Show notification to user
    showNotification(
      "The workspace is clear for working with a new Study.",
      type = "warning",
      duration = 5
    )

    # UPDATE EXPERIMENT CHOICES
    study_exp <- reactive_df_study_exp()
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

# observeEvent(input$readxMap_experiment_accession_import, {
#   req(input$readxMap_study_accession)
#
#   print(paste("readxMap_experiment_accession_import changed to:",
#               input$readxMap_experiment_accession_import))
#
#   if (input$readxMap_experiment_accession_import != "Click here" &&
#       input$readxMap_experiment_accession_import != "") {
#
#     # RESET ALL BATCH REACTIVES when experiment changes
#     reset_batch_reactives()
#
#     # Clear any uploaded files from UI
#     shinyjs::reset("upload_experiment_files")
#     shinyjs::reset("upload_layout_file")
#
#     # Show notification to user
#     showNotification(
#       paste("Experiment changed to:", input$readxMap_experiment_accession_import,
#             "- All batch data has been cleared. Please upload new files."),
#       type = "warning",
#       duration = 5
#     )
#   }
# })

observeEvent(input$readxMap_experiment_accession_import, {
  req(input$readxMap_study_accession)

  print(paste("readxMap_experiment_accession_import changed to:",
              input$readxMap_experiment_accession_import))

  if (input$readxMap_experiment_accession_import != "Click here" &&
      input$readxMap_experiment_accession_import != "") {

    # ========================================
    # RESET ALL BATCH REACTIVES
    # ========================================
    reset_batch_reactives()

    # ========================================
    # RESET ALL BATCH UI ELEMENTS
    # ========================================
    reset_batch_ui(
      session = session,
      include_experiment_files = TRUE,
      include_layout_file = TRUE
    )

    # NOTE: Output invalidation is handled by clearing layout_template_sheets()
    # in reset_batch_reactives(). The outputs view_layout_file_ui,
    # plate_layout_selector, selected_plate_layout_plot, and description_warning_ui
    # already check for NULL/empty layout_template_sheets() and return NULL.
    # DO NOT re-assign outputs here as it breaks the reactive chain.

    # cat("  ✓ Cleared dynamic UI outputs\n")

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
    cat("all completed:")
    print(all_completed())

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

  # Reset description status
  description_status(list(
    has_content = FALSE,
    has_sufficient_elements = FALSE,
    min_elements_found = 0,
    required_elements = 3,
    checked = FALSE,
    message = ""
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

  # ============================================================================
  # Extract plate numbers from filename or plateid
  # Priority: 1) plateid from header, 2) filename, 3) fallback to sequential
  # ============================================================================
  cat("\nExtracting plate numbers from filenames/plateids...\n")

  # Track used plate numbers to handle fallbacks without duplicates
  used_plate_nums <- c()

  for (i in seq_along(header_list)) {
    file_name <- names(header_list)[i]
    plate_num <- NA_character_
    extraction_source <- ""

    # PRIORITY 1: Try to extract from plateid in header metadata
    if ("plateid" %in% names(header_list[[i]]) &&
        !is.na(header_list[[i]]$plateid[1]) &&
        header_list[[i]]$plateid[1] != "") {
      plate_num <- extract_plate_number(header_list[[i]]$plateid[1])
      if (!is.na(plate_num)) {
        extraction_source <- paste0("plateid ('", header_list[[i]]$plateid[1], "')")
      }
    }

    # PRIORITY 2: Try to extract from file_name in header metadata
    if (is.na(plate_num) && "file_name" %in% names(header_list[[i]])) {
      plate_num <- extract_plate_number(header_list[[i]]$file_name[1])
      if (!is.na(plate_num)) {
        extraction_source <- paste0("file_name ('", header_list[[i]]$file_name[1], "')")
      }
    }

    # PRIORITY 3: Try to extract from the uploaded filename (source_file)
    if (is.na(plate_num)) {
      plate_num <- extract_plate_number(file_name)
      if (!is.na(plate_num)) {
        extraction_source <- paste0("source_file ('", file_name, "')")
      }
    }

    # FALLBACK: If still NA, assign a sequential number that doesn't conflict
    if (is.na(plate_num)) {
      # Find the next available plate number
      fallback_num <- 1
      while (paste0("plate_", fallback_num) %in% used_plate_nums) {
        fallback_num <- fallback_num + 1
      }
      plate_num <- paste0("plate_", fallback_num)
      extraction_source <- "fallback (sequential)"
      cat("  ⚠️  ", file_name, "→ Could not extract plate number, using fallback:", plate_num, "\n")
    }

    # Check for duplicate plate numbers
    if (plate_num %in% used_plate_nums) {
      original_plate_num <- plate_num
      # Find next available
      base_num <- as.numeric(gsub("plate_", "", plate_num))
      while (paste0("plate_", base_num) %in% used_plate_nums) {
        base_num <- base_num + 1
      }
      plate_num <- paste0("plate_", base_num)
      cat("  ⚠️  Duplicate plate number detected! '", original_plate_num,
          "' already used. Reassigning to: ", plate_num, "\n", sep = "")
    }

    # Track this plate number
    used_plate_nums <- c(used_plate_nums, plate_num)

    # Assign to header
    header_list[[i]]$plate <- plate_num

    # If plateid is NA or empty, use the extracted plate number
    if (is.na(header_list[[i]]$plateid[1]) || header_list[[i]]$plateid[1] == "") {
      header_list[[i]]$plateid <- plate_num
    }

    cat("  ✓ ", file_name, " → ", plate_num, " (from ", extraction_source, ")\n", sep = "")
  }

  cat("\n")

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

  # Check Description field status after loading plate data
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         CHECKING DESCRIPTION FIELD                       ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Get the current delimiter (default to underscore)
  delimiter <- if (!is.null(input$description_delimiter)) input$description_delimiter else "_"

  # Check description elements
  desc_check <- check_description_elements(
    plate_data = all_plates,
    delimiter = delimiter,
    required_elements = 3
  )

  # Update the reactive
  description_status(list(
    has_content = desc_check$has_content,
    has_sufficient_elements = desc_check$has_sufficient_elements,
    min_elements_found = desc_check$min_elements_found,
    required_elements = desc_check$required_elements,
    checked = TRUE,
    message = desc_check$message
  ))

  # Show notifications
  if (!desc_check$has_content) {
    showNotification(
      "Description field is blank. Default values will be used.",
      type = "warning",
      duration = 10
    )
  } else if (!desc_check$has_sufficient_elements) {
    showNotification(
      sprintf("Description has insufficient elements (%d found, %d required).",
              desc_check$min_elements_found, desc_check$required_elements),
      type = "warning",
      duration = 10
    )
  }

  # Extract and log antigens
  metadata_cols <- c("source_file", "Well", "Type", "Description",
                     "% Agg Beads", "Sampling Errors", "Acquisition Time")
  antigen_cols <- names(all_plates)[!(names(all_plates) %in% metadata_cols)]
  cat("  → Antigens detected (", length(antigen_cols), "):\n", sep="")
  for (ag in antigen_cols) {
    cat("      •", ag, "\n")
  }

  batch_plate_data(all_plates)

  # Update description_status (triggers description_warning_ui)
  description_status(list(
    has_content = desc_check$has_content,
    has_sufficient_elements = desc_check$has_sufficient_elements,
    min_elements_found = desc_check$min_elements_found,
    required_elements = desc_check$required_elements,
    checked = TRUE,
    message = desc_check$message
  ))

  # GENERATE LAYOUT TEMPLATE SHEETS (triggers view_layout_file_ui,
  # plate_layout_selector, and selected_plate_layout_plot)
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         GENERATING LAYOUT TEMPLATE SHEETS                ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Generate the layout template sheets from the uploaded data
  # NOTE: You need to implement or call the appropriate function here
  # This is a placeholder - replace with your actual layout generation logic
  generated_sheets <- tryCatch({
    generate_layout_template_from_plates(
      plate_data = all_plates,
      header_list = header_list,
      antigen_cols = antigen_cols
    )
  }, error = function(e) {
    cat("  ⚠️  Error generating layout sheets:", conditionMessage(e), "\n")
    list()
  })

  if (length(generated_sheets) > 0) {
    cat("  ✓ Generated layout sheets:", paste(names(generated_sheets), collapse = ", "), "\n")
    layout_template_sheets(generated_sheets)
  } else {
    cat("  ⚠️  No layout sheets generated\n")
    layout_template_sheets(list())
  }

  # Show notifications
  if (!desc_check$has_content) {
    showNotification(
      "Description field is blank. Default values will be used.",
      type = "warning",
      duration = 10
    )
  } else if (!desc_check$has_sufficient_elements) {
    showNotification(
      sprintf("Description has insufficient elements (%d found, %d required).",
              desc_check$min_elements_found, desc_check$required_elements),
      type = "warning",
      duration = 10
    )
  }

  cat("\n✓ Upload complete!\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")

  showNotification(
    paste("Successfully uploaded", nrow(current_exp_files), "experiment file(s)"),
    type = "message",
    duration = 3
  )
})

observeEvent(input$description_delimiter, {
  req(batch_plate_data())

  all_plates <- batch_plate_data()
  delimiter <- input$description_delimiter

  # Re-check description elements with new delimiter
  desc_check <- check_description_elements(
    plate_data = all_plates,
    delimiter = delimiter,
    required_elements = 3
  )

  # Update the reactive
  description_status(list(
    has_content = desc_check$has_content,
    has_sufficient_elements = desc_check$has_sufficient_elements,
    min_elements_found = desc_check$min_elements_found,
    required_elements = desc_check$required_elements,
    checked = TRUE,
    message = desc_check$message
  ))
}, ignoreInit = TRUE)

# =============================================================================
# REFACTORED observeEvent for input$upload_layout_file
# =============================================================================
# Key fixes:
# 1. Removed the problematic line that overwrites sheet names
# 2. Added explicit logging to track reactive chain
# 3. Added debugging to verify plates_map and plate_id data exist
# 4. Ensured layout_template_sheets() is updated correctly to trigger downstream reactives
# =============================================================================

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
  cat("  → Excel sheets found:", paste(sheets, collapse=", "), "\n")

  validation <- check_sheet_names(input_upload_layout_file$datapath)
  if (!validation$valid) {
    showNotification(
      validation$message,
      type = "error",
      duration = NULL
    )
    return()
  }

  # Call import_layout_file
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
  cat("  → Imported sheet names:", paste(names(all_sheets$data), collapse=", "), "\n\n")

  # =========================================================================
  # FIX: DO NOT overwrite sheet names - import_layout_file returns correct names
  # REMOVED: names(all_sheets$data) <- sheets
  # =========================================================================

  # Verify required sheets exist with correct names
  required_sheets <- c("plates_map", "plate_id", "antigen_list")
  missing_required <- setdiff(required_sheets, names(all_sheets$data))
  if (length(missing_required) > 0) {
    cat("⚠️  ERROR: Missing required sheets:", paste(missing_required, collapse=", "), "\n")
    showNotification(
      paste("Missing required sheets:", paste(missing_required, collapse=", ")),
      type = "error",
      duration = 10
    )
    return()
  }

  # Debug: Verify plates_map and plate_id data for plotting
  cat("\n=== DEBUG: Verifying data for plate layout plots ===\n")
  cat("  plates_map:\n")
  cat("    → Rows:", nrow(all_sheets$data[["plates_map"]]), "\n")
  cat("    → Columns:", paste(names(all_sheets$data[["plates_map"]]), collapse=", "), "\n")
  cat("  plate_id:\n")
  cat("    → Rows:", nrow(all_sheets$data[["plate_id"]]), "\n")
  cat("    → Columns:", paste(names(all_sheets$data[["plate_id"]]), collapse=", "), "\n")

  # Check required columns for plotting
  required_plates_map_cols <- c("study_name", "experiment_name", "plate_number", "well", "specimen_type")
  missing_cols <- setdiff(required_plates_map_cols, names(all_sheets$data[["plates_map"]]))
  if (length(missing_cols) > 0) {
    cat("  ⚠️  plates_map missing columns:", paste(missing_cols, collapse=", "), "\n")
  } else {
    cat("  ✓ plates_map has all required columns\n")
  }

  required_plate_id_cols <- c("study_name", "experiment_name", "plate_number", "number_of_wells")
  missing_plate_id_cols <- setdiff(required_plate_id_cols, names(all_sheets$data[["plate_id"]]))
  if (length(missing_plate_id_cols) > 0) {
    cat("  ⚠️  plate_id missing columns:", paste(missing_plate_id_cols, collapse=", "), "\n")
  } else {
    cat("  ✓ plate_id has all required columns\n")
  }
  cat("================================================\n\n")

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

  # =========================================================================
  # CRITICAL: Store the layout data - triggers downstream reactives
  # =========================================================================
  cat("╔══════════════════════════════════════════════════════════╗\n")
  cat("║  STORING LAYOUT SHEETS (triggers plot generation)       ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  layout_template_sheets(all_sheets$data)

  cat("  ✓ layout_template_sheets() updated\n")
  cat("  → Contains", length(all_sheets$data), "sheets:", paste(names(all_sheets$data), collapse=", "), "\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")

  # Construct batch header metadata
  batch_header <- construct_batch_upload_metadata(
    plates_map = all_sheets$data[["plates_map"]],
    plate_metadata_list = bead_array_header_list_data,
    workspace_id = workspace_id,
    currentuser = currentuser
  )

  # Verify sample_dilution_factor is present
  batch_metadata_data <- batch_header
  cat("\n=== DEBUG: batch_metadata_data ===\n")
  cat("Columns:", paste(names(batch_metadata_data), collapse = ", "), "\n")
  if ("sample_dilution_factor" %in% names(batch_metadata_data)) {
    cat("sample_dilution_factor values:", paste(batch_metadata_data$sample_dilution_factor, collapse = ", "), "\n")
  } else {
    cat("⚠ WARNING: sample_dilution_factor column MISSING!\n")
  }

  all_sheets_v <<- all_sheets

  batch_metadata_data <- merge(
    batch_metadata_data,
    all_sheets$data[["plate_id"]][, c("plateid", "number_of_wells")],
    by.x = "plateid",
    by.y = "plateid",
    all.x = TRUE
  )

  names(batch_metadata_data)[names(batch_metadata_data) == "number_of_wells"] <- "n_wells"

  batch_metadata_data_v <<- batch_metadata_data

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

    showNotification(
      "Layout file uploaded but validation failed. Please review error messages.",
      type = "error",
      duration = 10
    )
  }

  cat("╚══════════════════════════════════════════════════════════╝\n\n")
})

# observeEvent(input$upload_layout_file, {
#   req(input$readxMap_study_accession)
#   req(input$readxMap_experiment_accession_import)
#
#   cat("\n╔══════════════════════════════════════════════════════════╗\n")
#   cat("║         UPLOADING LAYOUT FILE                            ║\n")
#   cat("╚══════════════════════════════════════════════════════════╝\n")
#
#   # Reset layout-related reactives
#   inLayoutFile(NULL)
#   avaliableLayoutSheets(NULL)
#   layout_template_sheets(list())
#
#   # Reset validation state
#   batch_validation_state(list(
#     is_validated = FALSE,
#     is_uploaded = FALSE,
#     validation_time = NULL,
#     upload_time = NULL,
#     metadata_result = NULL,
#     bead_array_result = NULL
#   ))
#
#   input_upload_layout_file <- input$upload_layout_file
#   workspace_id <- userWorkSpaceID()
#   currentuser <- currentuser()
#   bead_array_header_list_data <- bead_array_header_list()
#
#   inLayoutFile(input_upload_layout_file)
#
#   if (is.null(input_upload_layout_file)) {
#     cat("⚠️  No layout file provided\n")
#     return()
#   }
#
#   cat("Layout file:", input_upload_layout_file$name, "\n\n")
#
#   # CRITICAL: Check what data we're validating against
#   cat("Checking batch_plate_data()...\n")
#   if (is.null(batch_plate_data())) {
#     cat("⚠️  ERROR: batch_plate_data() is NULL!\n")
#     cat("   Cannot validate without plate data.\n")
#     showNotification(
#       "Error: No plate data found. Please upload experiment files first.",
#       type = "error",
#       duration = 10
#     )
#     return()
#   }
#
#   cat("  → Rows:", nrow(batch_plate_data()), "\n")
#   cat("  → Source files:", paste(unique(batch_plate_data()$source_file), collapse=", "), "\n")
#
#   # Extract antigens from current batch data
#   metadata_cols <- c("source_file", "Well", "Type", "Description",
#                      "% Agg Beads", "Sampling Errors", "Acquisition Time")
#   current_antigens <- names(batch_plate_data())[!(names(batch_plate_data()) %in% metadata_cols)]
#   cat("  → Current batch antigens (", length(current_antigens), "):\n", sep="")
#   for (ag in current_antigens) {
#     cat("      •", ag, "\n")
#   }
#   cat("\n")
#
#   # Validate sheet names
#   cat("Reading layout file...\n")
#   sheets <- readxl::excel_sheets(input_upload_layout_file$datapath)
#   cat("  → Excel sheets found:", paste(sheets, collapse=", "), "\n")
#
#   validation <- check_sheet_names(input_upload_layout_file$datapath)
#   if (!validation$valid) {
#     showNotification(
#       validation$message,
#       type = "error",
#       duration = NULL
#     )
#     return()
#   }
#
#   # CORRECTED: Call import_layout_file with only the file_path parameter
#   all_sheets <- import_layout_file(input_upload_layout_file$datapath)
#
#   if (!all_sheets$success) {
#     showNotification(
#       paste(all_sheets$messages, collapse = "\n"),
#       type = "error",
#       duration = NULL
#     )
#     return()
#   }
#
#   cat("  ✓ Layout sheets read successfully\n")
#   cat("  → Imported sheet names:", paste(names(all_sheets$data), collapse=", "), "\n\n")
#
#   # =========================================================================
#   # FIX: DO NOT overwrite sheet names with readxl::excel_sheets() result
#   # The import_layout_file function already returns data with correct names:
#   # plate_id, subject_groups, timepoint, antigen_list, plates_map, cell_valid
#   # =========================================================================
#   # REMOVED: names(all_sheets$data) <- sheets  # This was causing the bug!
#
#   # Verify required sheets exist with correct names
#   required_sheets <- c("plates_map", "plate_id", "antigen_list")
#   missing_required <- setdiff(required_sheets, names(all_sheets$data))
#   if (length(missing_required) > 0) {
#     cat("⚠️  ERROR: Missing required sheets:", paste(missing_required, collapse=", "), "\n")
#     showNotification(
#       paste("Missing required sheets:", paste(missing_required, collapse=", ")),
#       type = "error",
#       duration = 10
#     )
#     return()
#   }
#
#   # Debug: Verify plates_map and plate_id data
#   cat("\n=== DEBUG: Verifying data for plate layout plots ===\n")
#   cat("  plates_map:\n")
#   cat("    → Rows:", nrow(all_sheets$data[["plates_map"]]), "\n")
#   cat("    → Columns:", paste(names(all_sheets$data[["plates_map"]]), collapse=", "), "\n")
#   cat("  plate_id:\n")
#   cat("    → Rows:", nrow(all_sheets$data[["plate_id"]]), "\n")
#   cat("    → Columns:", paste(names(all_sheets$data[["plate_id"]]), collapse=", "), "\n")
#
#   # Check if plates_map has the required columns for plotting
#   required_plates_map_cols <- c("study_name", "experiment_name", "plate_number", "well", "specimen_type")
#   missing_cols <- setdiff(required_plates_map_cols, names(all_sheets$data[["plates_map"]]))
#   if (length(missing_cols) > 0) {
#     cat("  ⚠️  plates_map missing columns:", paste(missing_cols, collapse=", "), "\n")
#   } else {
#     cat("  ✓ plates_map has all required columns for plotting\n")
#   }
#
#   # Check if plate_id has the required columns
#   required_plate_id_cols <- c("study_name", "experiment_name", "plate_number", "number_of_wells")
#   missing_plate_id_cols <- setdiff(required_plate_id_cols, names(all_sheets$data[["plate_id"]]))
#   if (length(missing_plate_id_cols) > 0) {
#     cat("  ⚠️  plate_id missing columns:", paste(missing_plate_id_cols, collapse=", "), "\n")
#   } else {
#     cat("  ✓ plate_id has all required columns for plotting\n")
#   }
#   cat("================================================\n\n")
#
#   # Check antigens in layout vs current data
#   layout_antigens <- unique(all_sheets$data[["antigen_list"]]$antigen_label_on_plate)
#   cat("Layout file antigens (", length(layout_antigens), "):\n", sep="")
#   for (ag in layout_antigens) {
#     cat("  •", ag, "\n")
#   }
#   cat("\n")
#
#   # Check for mismatches
#   extra_in_layout <- setdiff(layout_antigens, current_antigens)
#   missing_from_layout <- setdiff(current_antigens, layout_antigens)
#
#   if (length(extra_in_layout) > 0) {
#     cat("⚠️  WARNING: Layout has antigens NOT in current batch data:\n")
#     for (ag in extra_in_layout) {
#       cat("    ✗", ag, "\n")
#     }
#     cat("\n")
#   }
#
#   if (length(missing_from_layout) > 0) {
#     cat("⚠️  WARNING: Current batch has antigens NOT in layout:\n")
#     for (ag in missing_from_layout) {
#       cat("    ✗", ag, "\n")
#     }
#     cat("\n")
#   }
#
#   # =========================================================================
#   # CRITICAL: Store the layout data - this triggers downstream reactives
#   # plate_layout_plots(), plate_layout_selector, selected_plate_layout_plot
#   # =========================================================================
#   cat("Storing layout sheets to trigger plot generation...\n")
#   layout_template_sheets(all_sheets$data)
#   cat("  ✓ layout_template_sheets() updated with", length(all_sheets$data), "sheets\n")
#   cat("  → Sheet names:", paste(names(all_sheets$data), collapse=", "), "\n\n")
#
#   # Construct batch header metadata
#   batch_header <<- construct_batch_upload_metadata(
#     plates_map = all_sheets$data[["plates_map"]],
#     plate_metadata_list = bead_array_header_list_data,
#     workspace_id = workspace_id,
#     currentuser = currentuser
#   )
#
#   # Verify sample_dilution_factor is present
#   batch_metadata_data <- batch_header
#   cat("\n=== DEBUG: batch_metadata_data after combine ===\n")
#   cat("Columns:", paste(names(batch_metadata_data), collapse = ", "), "\n")
#   if ("sample_dilution_factor" %in% names(batch_metadata_data)) {
#     cat("sample_dilution_factor values:", paste(batch_metadata_data$sample_dilution_factor, collapse = ", "), "\n")
#   } else {
#     cat("⚠ WARNING: sample_dilution_factor column MISSING!\n")
#   }
#
#   all_sheets_v <<- all_sheets
#
#   batch_metadata_data <- merge(
#     batch_metadata_data,
#     all_sheets$data[["plate_id"]][, c("plateid", "number_of_wells")],
#     by.x = "plateid",
#     by.y = "plateid",
#     all.x = TRUE
#   )
#
#   names(batch_metadata_data)[names(batch_metadata_data) == "number_of_wells"] <- "n_wells"
#
#   batch_metadata_data_v <<- batch_metadata_data
#
#   cat("Validating...\n")
#
#   # Validate metadata
#   validate_metadata_result <- validate_batch_plate_metadata(
#     plate_metadata = batch_metadata_data,
#     plate_id_data = all_sheets$data[["plate_id"]]
#   )
#
#   # Validate bead array data
#   bead_array_validation <- validate_batch_bead_array_data(
#     combined_plate_data = batch_plate_data(),
#     antigen_import_list = all_sheets$data[["antigen_list"]],
#     blank_keyword = input$batch_blank
#   )
#
#   # Update validation state
#   if (bead_array_validation$is_valid && validate_metadata_result$is_valid) {
#     batch_validation_state(list(
#       is_validated = TRUE,
#       is_uploaded = FALSE,
#       validation_time = Sys.time(),
#       upload_time = NULL,
#       metadata_result = validate_metadata_result,
#       bead_array_result = bead_array_validation
#     ))
#     batch_metadata(batch_metadata_data)
#
#     cat("✓ VALIDATION PASSED!\n")
#     cat("╚══════════════════════════════════════════════════════════╝\n\n")
#
#     showNotification(
#       "Layout file validated successfully! Ready to upload to database.",
#       type = "message",
#       duration = 5
#     )
#   } else {
#     batch_validation_state(list(
#       is_validated = FALSE,
#       is_uploaded = FALSE,
#       validation_time = Sys.time(),
#       upload_time = NULL,
#       metadata_result = validate_metadata_result,
#       bead_array_result = bead_array_validation
#     ))
#
#     cat("✗ VALIDATION FAILED\n")
#     cat("  Metadata valid:", validate_metadata_result$is_valid, "\n")
#     cat("  Bead array valid:", bead_array_validation$is_valid, "\n")
#     cat("╚══════════════════════════════════════════════════════════╝\n\n")
#
#     showNotification(
#       "Layout file uploaded but validation failed. Please review error messages.",
#       type = "error",
#       duration = 10
#     )
#   }
#
#   # =========================================================================
#   # DEBUG: Verify that the reactive chain will fire
#   # =========================================================================
#   cat("\n=== POST-UPLOAD VERIFICATION ===\n")
#   current_sheets <- layout_template_sheets()
#   cat("layout_template_sheets() now contains", length(current_sheets), "sheets\n")
#   if (length(current_sheets) > 0) {
#     cat("  → plates_map present:", !is.null(current_sheets[["plates_map"]]), "\n")
#     cat("  → plate_id present:", !is.null(current_sheets[["plate_id"]]), "\n")
#     if (!is.null(current_sheets[["plates_map"]]) && !is.null(current_sheets[["plate_id"]])) {
#       cat("  ✓ Both required sheets present - plate_layout_plots() should render\n")
#     }
#   }
#   cat("================================\n\n")
# })

# observeEvent(input$upload_layout_file, {
#   req(input$readxMap_study_accession)
#   req(input$readxMap_experiment_accession_import)
#
#   cat("\n╔══════════════════════════════════════════════════════════╗\n")
#   cat("║         UPLOADING LAYOUT FILE                            ║\n")
#   cat("╚══════════════════════════════════════════════════════════╝\n")
#
#   # Reset layout-related reactives
#   inLayoutFile(NULL)
#   avaliableLayoutSheets(NULL)
#   layout_template_sheets(list())
#
#   # Reset validation state
#   batch_validation_state(list(
#     is_validated = FALSE,
#     is_uploaded = FALSE,
#     validation_time = NULL,
#     upload_time = NULL,
#     metadata_result = NULL,
#     bead_array_result = NULL
#   ))
#
#   input_upload_layout_file <- input$upload_layout_file
#   workspace_id <- userWorkSpaceID()
#   currentuser <- currentuser()
#   bead_array_header_list_data <- bead_array_header_list()
#
#   inLayoutFile(input_upload_layout_file)
#
#   if (is.null(input_upload_layout_file)) {
#     cat("⚠️  No layout file provided\n")
#     return()
#   }
#
#   cat("Layout file:", input_upload_layout_file$name, "\n\n")
#
#   # CRITICAL: Check what data we're validating against
#   cat("Checking batch_plate_data()...\n")
#   if (is.null(batch_plate_data())) {
#     cat("⚠️  ERROR: batch_plate_data() is NULL!\n")
#     cat("   Cannot validate without plate data.\n")
#     showNotification(
#       "Error: No plate data found. Please upload experiment files first.",
#       type = "error",
#       duration = 10
#     )
#     return()
#   }
#
#   cat("  → Rows:", nrow(batch_plate_data()), "\n")
#   cat("  → Source files:", paste(unique(batch_plate_data()$source_file), collapse=", "), "\n")
#
#   # Extract antigens from current batch data
#   metadata_cols <- c("source_file", "Well", "Type", "Description",
#                      "% Agg Beads", "Sampling Errors", "Acquisition Time")
#   current_antigens <- names(batch_plate_data())[!(names(batch_plate_data()) %in% metadata_cols)]
#   cat("  → Current batch antigens (", length(current_antigens), "):\n", sep="")
#   for (ag in current_antigens) {
#     cat("      •", ag, "\n")
#   }
#   cat("\n")
#
#   # Validate sheet names
#   cat("Reading layout file...\n")
#   sheets <- readxl::excel_sheets(input_upload_layout_file$datapath)
#   validation <- check_sheet_names(input_upload_layout_file$datapath)
#   if (!validation$valid) {
#     showNotification(
#       validation$message,
#       type = "error",
#       duration = NULL
#     )
#     return()
#   }
#
#   # CORRECTED: Call import_layout_file with only the file_path parameter
#   all_sheets <- import_layout_file(input_upload_layout_file$datapath)
#
#   if (!all_sheets$success) {
#     showNotification(
#       paste(all_sheets$messages, collapse = "\n"),
#       type = "error",
#       duration = NULL
#     )
#     return()
#   }
#
#   cat("  ✓ Layout sheets read successfully\n")
#   cat("  → Available sheets:", paste(names(all_sheets$data), collapse=", "), "\n\n")
#
#   # Assign sheet names
#   names(all_sheets$data) <- sheets
#
#   # Check antigens in layout vs current data
#   layout_antigens <- unique(all_sheets$data[["antigen_list"]]$antigen_label_on_plate)
#   cat("Layout file antigens (", length(layout_antigens), "):\n", sep="")
#   for (ag in layout_antigens) {
#     cat("  •", ag, "\n")
#   }
#   cat("\n")
#
#   # Check for mismatches
#   extra_in_layout <- setdiff(layout_antigens, current_antigens)
#   missing_from_layout <- setdiff(current_antigens, layout_antigens)
#
#   if (length(extra_in_layout) > 0) {
#     cat("⚠️  WARNING: Layout has antigens NOT in current batch data:\n")
#     for (ag in extra_in_layout) {
#       cat("    ✗", ag, "\n")
#     }
#     cat("\n")
#   }
#
#   if (length(missing_from_layout) > 0) {
#     cat("⚠️  WARNING: Current batch has antigens NOT in layout:\n")
#     for (ag in missing_from_layout) {
#       cat("    ✗", ag, "\n")
#     }
#     cat("\n")
#   }
#
#   # Store the layout data
#   layout_template_sheets(all_sheets$data)
#
#   # Construct batch header metadata
#   batch_header <<- construct_batch_upload_metadata(
#     plates_map = all_sheets$data[["plates_map"]],
#     plate_metadata_list = bead_array_header_list_data,
#     workspace_id = workspace_id,
#     currentuser = currentuser
#   )
#
#   # batch_header <- transpose_batch_header(batch_header_wide)
#   #
#   # # Debug: Check what columns are in batch_header
#   # cat("\n=== DEBUG: batch_header columns after construct_batch_upload_metadata ===\n")
#   # for (nm in names(batch_header)) {
#   #   cat("  ", nm, "columns:", paste(names(batch_header[[nm]]), collapse = ", "), "\n")
#   #   if ("sample_dilution_factor" %in% names(batch_header[[nm]])) {
#   #     cat("    sample_dilution_factor values:", paste(batch_header[[nm]]$sample_dilution_factor, collapse = ", "), "\n")
#   #   }
#   # }
#   #
#   # # Prepare metadata
#   # batch_metadata_data <- combine_plate_metadata(head_list = batch_header)
#
#   # Verify sample_dilution_factor is present
#   batch_metadata_data <- batch_header
#   cat("\n=== DEBUG: batch_metadata_data after combine ===\n")
#   cat("Columns:", paste(names(batch_metadata_data), collapse = ", "), "\n")
#   if ("sample_dilution_factor" %in% names(batch_metadata_data)) {
#     cat("sample_dilution_factor values:", paste(batch_metadata_data$sample_dilution_factor, collapse = ", "), "\n")
#   } else {
#     cat("⚠ WARNING: sample_dilution_factor column MISSING!\n")
#   }
#
#   all_sheets_v <<- all_sheets
#
#   batch_metadata_data <- merge(
#     batch_metadata_data,
#     all_sheets$data[["plate_id"]][, c("plateid", "number_of_wells")],
#     by.x = "plateid",
#     by.y = "plateid",
#     all.x = TRUE
#   )
#
#   names(batch_metadata_data)[names(batch_metadata_data) == "number_of_wells"] <- "n_wells"
#
#   batch_metadata_data_v <<- batch_metadata_data
#
#   cat("Validating...\n")
#
#   # Validate metadata
#   validate_metadata_result <- validate_batch_plate_metadata(
#     plate_metadata = batch_metadata_data,
#     plate_id_data = all_sheets$data[["plate_id"]]
#   )
#
#   # Validate bead array data
#   bead_array_validation <- validate_batch_bead_array_data(
#     combined_plate_data = batch_plate_data(),
#     antigen_import_list = all_sheets$data[["antigen_list"]],
#     blank_keyword = input$batch_blank
#   )
#
#   # Update validation state
#   if (bead_array_validation$is_valid && validate_metadata_result$is_valid) {
#     batch_validation_state(list(
#       is_validated = TRUE,
#       is_uploaded = FALSE,
#       validation_time = Sys.time(),
#       upload_time = NULL,
#       metadata_result = validate_metadata_result,
#       bead_array_result = bead_array_validation
#     ))
#     batch_metadata(batch_metadata_data)
#
#     cat("✓ VALIDATION PASSED!\n")
#     cat("╚══════════════════════════════════════════════════════════╝\n\n")
#
#     showNotification(
#       "Layout file validated successfully! Ready to upload to database.",
#       type = "message",
#       duration = 5
#     )
#   } else {
#     batch_validation_state(list(
#       is_validated = FALSE,
#       is_uploaded = FALSE,
#       validation_time = Sys.time(),
#       upload_time = NULL,
#       metadata_result = validate_metadata_result,
#       bead_array_result = bead_array_validation
#     ))
#
#     cat("✗ VALIDATION FAILED\n")
#     cat("  Metadata valid:", validate_metadata_result$is_valid, "\n")
#     cat("  Bead array valid:", bead_array_validation$is_valid, "\n")
#     cat("╚══════════════════════════════════════════════════════════╝\n\n")
#
#     showNotification(
#       "Layout file uploaded but validation failed. Please review error messages.",
#       type = "error",
#       duration = 10
#     )
#   }
# })

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

    # NOTE: view_layout_file_ui will return NULL automatically
    # because layout_template_sheets() was set to list() above.
    # DO NOT re-assign the output here as it breaks the reactive chain.

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

# SERVER: Execute deletion when confirmation is received
observeEvent(input$delete_confirmation, {
  # input$delete_confirmation is TRUE when confirmed, FALSE when cancelled
  req(input$delete_confirmation == TRUE)
  req(input$readxMap_study_accession)
  req(input$readxMap_study_accession != "Click here")

  selected_study_accession <- input$readxMap_study_accession

  cat("\nDelete confirmation received for:", selected_study_accession, "\n")

  # Show busy indicator
  show_modal_spinner(
    spin = "fading-circle",
    color = "#3c8dbc",
    text = "Deleting study data... Please wait."
  )

  # Execute the delete procedure
  tryCatch({
    query <- glue_sql(
      "CALL public.delete_study_accession_rows({selected_study_accession});",
      selected_study_accession = selected_study_accession,
      .con = conn
    )

    cat("\nExecuting query:\n")
    print(query)

    result <- dbExecute(conn, query)

    cat("\ndbExecute result:", result, "\n")

    # Remove busy indicator
    remove_modal_spinner()

    # Show success message
    shinyalert(
      title = "Success!",
      text = paste0(
        "All data for '", selected_study_accession, "' has been successfully deleted."
      ),
      type = "success"
    )

    # Trigger refresh - get the list, modify it, set it back
    current_counters <- tabRefreshCounter()
    current_counters$import_tab <- current_counters$import_tab + 1
    tabRefreshCounter(current_counters)

  }, error = function(e) {
    cat("\nError during deletion:", e$message, "\n")

    # Remove busy indicator
    remove_modal_spinner()

    # Show error message
    shinyalert(
      title = "Deletion Failed",
      text = paste("An error occurred while deleting the data:\n\n", e$message),
      type = "error"
    )
  })
})

# SERVER: Confirmation Modal Dialog
observeEvent(input$delete_study_btn, {
  req(input$readxMap_study_accession)
  req(input$readxMap_study_accession != "Click here")

  data <- delete_study_data()

  if (nrow(data) == 0) {
    shinyalert(
      title = "No Data",
      text = "No data to delete for this study.",
      type = "warning"
    )
    return()
  }

  selected_study_accession <- input$readxMap_study_accession
  total_rows <- sum(data$row_count)
  table_count <- nrow(data)

  shinyalert(
    title = "Confirm Deletion",
    text = paste0(
      "You are about to permanently delete all data for:\n\n",
      selected_study_accession, "\n\n",
      "This will remove ", format(total_rows, big.mark = ","), " rows from ",
      table_count, " table", ifelse(table_count > 1, "s", ""), ".\n\n",
      "THIS ACTION CANNOT BE UNDONE!"
    ),
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonText = "Yes, Delete Permanently",
    cancelButtonText = "Cancel",
    confirmButtonCol = "#d9534f",
    inputId = "delete_confirmation"  # This creates input$delete_confirmation
  )
})

observeEvent(input$execute_delete_btn, {
  req(input$readxMap_study_accession)

  selected_study_accession <- input$readxMap_study_accession

  # Show busy indicator
  show_modal_spinner(
    spin = "fading-circle",
    color = "#3c8dbc",
    text = "Deleting study data... Please wait."
  )

  # Execute the delete procedure
  tryCatch({
    query <- glue_sql(
      "CALL public.delete_study_accession_rows({selected_study_accession});",
      selected_study_accession = selected_study_accession,
      .con = conn
    )
    cat("\n execute_delete_btn: query to delete study: \n")
    print(query)

    dbExecute(conn, query)

    # Remove busy indicator
    remove_modal_spinner()

    # Show success message
    shinyalert(
      title = "Success!",
      text = paste0(
        "All data for '", selected_study_accession, "' has been successfully deleted."
      ),
      type = "success"
    )

    # Trigger refresh of the tab/data
    current_counters <- tabRefreshCounter()
    current_counters$import_tab <- current_counters$import_tab + 1
    tabRefreshCounter(current_counters)

  }, error = function(e) {
    # Remove busy indicator
    remove_modal_spinner()

    # Show error message
    shinyalert(
      title = "Deletion Failed",
      text = paste("An error occurred while deleting the data:\n\n", e$message),
      type = "error"
    )
  })
})

# SERVER: Cancel Deletion - Close Modal
observeEvent(input$cancel_delete_btn, {
  removeModal()
  showNotification(
    "Deletion cancelled.",
    type = "message",
    duration = 2
  )
})

# SERVER: Confirm Deletion - Execute Delete Query
observeEvent(input$confirm_delete_btn, {
  req(input$readxMap_study_accession)

  selected_study_accession <- input$readxMap_study_accession

  # Show progress modal
  showModal(modalDialog(
    title = "Deleting Data...",
    size = "s",
    easyClose = FALSE,
    footer = NULL,
    div(
      style = "text-align: center; padding: 30px;",
      icon("spinner", class = "fa-spin", style = "font-size: 48px; color: #3c8dbc;"),
      p(
        style = "margin-top: 20px; font-size: 14px;",
        "Please wait while the data is being deleted..."
      )
    )
  ))

  # Execute the delete procedure
  tryCatch({
    query <- glue_sql(
      "CALL public.delete_study_accession_rows({selected_study_accession});",
      selected_study_accession = selected_study_accession,
      .con = conn
    )

    dbExecute(conn, query)

    # Close progress modal and show success
    removeModal()

    showModal(modalDialog(
      title = tagList(
        icon("check-circle", style = "color: #5cb85c;"),
        span("Deletion Complete", style = "color: #5cb85c;")
      ),
      size = "s",
      easyClose = TRUE,

      div(
        style = "text-align: center; padding: 20px;",
        icon("check-circle", style = "font-size: 48px; color: #5cb85c;"),
        p(
          style = "margin-top: 20px; font-size: 16px;",
          sprintf(
            "All data for '%s' has been successfully deleted.",
            selected_study_accession
          )
        )
      ),

      footer = tagList(
        actionButton(
          "close_success_modal",
          "Close",
          class = "btn-success",
          style = "padding: 10px 30px;"
        )
      )
    ))

    # Trigger refresh of the tab/data
    current_counters <- tabRefreshCounter()
    current_counters$import_tab <- current_counters$import_tab + 1
    tabRefreshCounter(current_counters)

  }, error = function(e) {
    removeModal()

    showModal(modalDialog(
      title = tagList(
        icon("times-circle", style = "color: #d9534f;"),
        span("Deletion Failed", style = "color: #d9534f;")
      ),
      size = "m",
      easyClose = TRUE,

      div(
        style = "text-align: center; padding: 20px;",
        icon("times-circle", style = "font-size: 48px; color: #d9534f;"),
        p(
          style = "margin-top: 20px; font-size: 14px;",
          "An error occurred while deleting the data:"
        ),
        div(
          style = "background-color: #f2dede; padding: 15px; border-radius: 5px; margin-top: 15px; text-align: left;",
          code(e$message)
        )
      ),

      footer = modalButton("Close")
    ))
  })
})

# SERVER: Close Success Modal
observeEvent(input$close_success_modal, {
  removeModal()
})

