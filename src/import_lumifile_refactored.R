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

# ============================================================================
# NEW: Reactive to track Description field status across all uploaded plates
# ============================================================================
description_status <- reactiveVal(list(
  has_content = TRUE,
  has_sufficient_elements = TRUE,
  min_elements_found = 0,
  required_elements = 3,  # PatientID, TimePeriod, DilutionFactor at minimum
  checked = FALSE,
  message = NULL
))

# ============================================================================
# NEW: Function to check Description field content and element count
# ============================================================================
check_description_elements <- function(plate_data, delimiter = "_", required_elements = 3) {
  # Filter to only sample rows (Type starts with X)
  sample_rows <- plate_data[grepl("^X", plate_data$Type), ]
  
  if (nrow(sample_rows) == 0) {
    return(list(
      has_content = TRUE,  # No samples, so not applicable
      has_sufficient_elements = TRUE,
      min_elements_found = NA,
      required_elements = required_elements,
      message = NULL
    ))
  }
  
  # Check Description column
  descriptions <- sample_rows$Description
  
  # Check if Description column exists
  if (!"Description" %in% names(plate_data)) {
    return(list(
      has_content = FALSE,
      has_sufficient_elements = FALSE,
      min_elements_found = 0,
      required_elements = required_elements,
      message = "Description column is missing from the plate data."
    ))
  }
  
  # Check for blank/empty descriptions
  empty_descriptions <- is.na(descriptions) | trimws(descriptions) == "" | descriptions == "NA"
  
  if (all(empty_descriptions)) {
    return(list(
      has_content = FALSE,
      has_sufficient_elements = FALSE,
      min_elements_found = 0,
      required_elements = required_elements,
      message = "All Description fields are blank. The layout template will need to be manually updated with subject IDs, groups, timepoints, and dilution factors after creation."
    ))
  }
  
  # Count elements in non-empty descriptions
  non_empty_descriptions <- descriptions[!empty_descriptions]
  
  # Escape delimiter for regex if needed
  delimiter_pattern <- switch(delimiter,
    "|" = "\\|",
    "." = "\\.",
    delimiter
  )
  
  element_counts <- sapply(non_empty_descriptions, function(desc) {
    # Split by delimiter and count non-empty elements
    elements <- strsplit(as.character(desc), delimiter_pattern)[[1]]
    elements <- elements[trimws(elements) != ""]
    length(elements)
  })
  
  min_elements <- min(element_counts, na.rm = TRUE)
  
  # Check if sufficient elements exist
  has_sufficient <- min_elements >= required_elements
  
  message <- NULL
  if (!has_sufficient) {
    message <- sprintf(
      "Description field has insufficient elements (found %d, need %d). Some fields will use default values. The layout template may need manual updates for subject IDs, groups, timepoints, and/or dilution factors.",
      min_elements,
      required_elements
    )
  }
  
  return(list(
    has_content = TRUE,
    has_sufficient_elements = has_sufficient,
    min_elements_found = min_elements,
    required_elements = required_elements,
    message = message
  ))
}

# ============================================================================
# NEW: Function to apply default values when Description is insufficient
# ============================================================================
apply_default_values_to_plates_map <- function(plates_map, description_status) {
  if (is.null(plates_map) || nrow(plates_map) == 0) {
    return(plates_map)
  }
  
  # Only apply defaults for sample rows (specimen_type == "X")
  sample_mask <- plates_map$specimen_type == "X"
  
  if (!description_status$has_content || !description_status$has_sufficient_elements) {
    cat("╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Applying default values due to insufficient Description ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
    
    # Set default subject_id to "1"
    if ("subject_id" %in% names(plates_map)) {
      na_subject <- sample_mask & (is.na(plates_map$subject_id) | plates_map$subject_id == "")
      if (any(na_subject)) {
        plates_map$subject_id[na_subject] <- "1"
        cat("  → Set", sum(na_subject), "blank subject_id values to '1'\n")
      }
    }
    
    # Set default specimen_dilution_factor to 1
    if ("specimen_dilution_factor" %in% names(plates_map)) {
      na_dilution <- sample_mask & (is.na(plates_map$specimen_dilution_factor) | plates_map$specimen_dilution_factor == "")
      if (any(na_dilution)) {
        plates_map$specimen_dilution_factor[na_dilution] <- 1
        cat("  → Set", sum(na_dilution), "blank specimen_dilution_factor values to 1\n")
      }
    }
    
    # Set default timeperiod_tissue_abbreviation to "T0"
    if ("timeperiod_tissue_abbreviation" %in% names(plates_map)) {
      na_timeperiod <- sample_mask & (is.na(plates_map$timeperiod_tissue_abbreviation) | plates_map$timeperiod_tissue_abbreviation == "")
      if (any(na_timeperiod)) {
        plates_map$timeperiod_tissue_abbreviation[na_timeperiod] <- "T0"
        cat("  → Set", sum(na_timeperiod), "blank timeperiod_tissue_abbreviation values to 'T0'\n")
      }
    }
    
    # Set default groupa if exists
    if ("groupa" %in% names(plates_map)) {
      na_groupa <- sample_mask & (is.na(plates_map$groupa) | plates_map$groupa == "")
      if (any(na_groupa)) {
        plates_map$groupa[na_groupa] <- "Unknown"
        cat("  → Set", sum(na_groupa), "blank groupa values to 'Unknown'\n")
      }
    }
    
    # Set default groupb if exists
    if ("groupb" %in% names(plates_map)) {
      na_groupb <- sample_mask & (is.na(plates_map$groupb) | plates_map$groupb == "")
      if (any(na_groupb)) {
        plates_map$groupb[na_groupb] <- "Unknown"
        cat("  → Set", sum(na_groupb), "blank groupb values to 'Unknown'\n")
      }
    }
    
    cat("╚══════════════════════════════════════════════════════════╝\n")
  }
  
  return(plates_map)
}

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
                   selectizeInput("readxMap_experiment_accession_import",
                                  label = NULL,
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
                         # ============================================================================
                         # MODIFIED: Conditionally show Description delimiter controls
                         # ============================================================================
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
                         # ============================================================================
                         # MODIFIED: Conditionally show optional elements checkboxes
                         # ============================================================================
                         conditionalPanel(
                           condition = "output.descriptionHasSufficientElements",
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
                         # ============================================================================
                         # MODIFIED: Conditionally show element order inputs
                         # ============================================================================
                         conditionalPanel(
                           condition = "output.descriptionHasSufficientElements",
                           uiOutput("order_input_ui")
                         ),
                         conditionalPanel(
                           condition = "output.descriptionHasContent",
                           uiOutput("bcsorder_input_ui")
                         ),
                         # ============================================================================
                         # NEW: Warning message when Description is blank or insufficient
                         # ============================================================================
                         uiOutput("description_warning_ui"),
                         downloadButton("blank_layout_file", "Generate a Layout file")
                       ),
                       fileInput("upload_layout_file"
                                 , label="Upload a completed layout file (only accepts xlsx, xls)"
                                 , accept=c(".xlsx",".xls")
                                 , multiple=FALSE)
                     )
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
  } else {
    import_plate_data_title<- paste("Choose or create a study for Importing Plate Data")
  }

   }
})

# ============================================================================
# NEW: Output for Description content status (for conditional panels)
# ============================================================================
output$descriptionHasContent <- reactive({
  status <- description_status()
  return(isTRUE(status$has_content))
})
outputOptions(output, "descriptionHasContent", suspendWhenHidden = FALSE)

# ============================================================================
# NEW: Output for Description sufficient elements status (for conditional panels)
# ============================================================================
output$descriptionHasSufficientElements <- reactive({
  status <- description_status()
  return(isTRUE(status$has_sufficient_elements))
})
outputOptions(output, "descriptionHasSufficientElements", suspendWhenHidden = FALSE)

# ============================================================================
# NEW: UI for Description warning message
# ============================================================================
output$description_warning_ui <- renderUI({
  status <- description_status()
  
  if (!status$checked) {
    return(NULL)
  }
  
  if (!status$has_content) {
    # Description is completely blank
    tags$div(
      style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
        " Description Field is Blank",
        style = "margin-top: 0; color: #856404;"
      ),
      tags$p(
        "The Description field in your plate data is empty. The layout template will be generated with default values:",
        style = "margin-bottom: 5px;"
      ),
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
    # Description has content but not enough elements
    tags$div(
      style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
        " Insufficient Description Elements",
        style = "margin-top: 0; color: #856404;"
      ),
      tags$p(
        sprintf(
          "The Description field has only %d element(s), but at least %d are required (PatientID, TimePeriod, DilutionFactor).",
          status$min_elements_found,
          status$required_elements
        ),
        style = "margin-bottom: 5px;"
      ),
      tags$p(
        "Missing fields will be filled with default values. You may need to manually update the layout template before uploading.",
        style = "margin-bottom: 0; color: #856404;"
      )
    )
  } else {
    # All good
    return(NULL)
  }
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

    # ============================================================================
    # MODIFIED: Get description status and pass to generate_layout_template
    # ============================================================================
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
      # Pass delimiter (use default if Description is blank)
      delimiter = if (desc_status$has_content) input$description_delimiter else "_",
      # Pass element order (use defaults if Description is blank/insufficient)
      element_order = if (desc_status$has_sufficient_elements) input$XElementOrder else c("PatientID", "TimePeriod", "DilutionFactor"),
      bcs_element_order = if (desc_status$has_content) input$BCSElementOrder else c("Source", "DilutionFactor")
    )

    cat("✓ Layout template generated!\n")
    
    # Show notification if defaults were applied
    if (!desc_status$has_content || !desc_status$has_sufficient_elements) {
      showNotification(
        "Layout template generated with default values. Please review and update the template before uploading.",
        type = "warning",
        duration = 10
      )
    }
    
    cat("╚══════════════════════════════════════════════════════════╝\n\n")
  }
)

# ... [REST OF THE FILE CONTINUES AS BEFORE - truncated for brevity]
# The key changes are:
# 1. Added description_status reactive
# 2. Added check_description_elements function
# 3. Added apply_default_values_to_plates_map function
# 4. Modified UI to conditionally show/hide description-related controls
# 5. Added warning UI for blank/insufficient descriptions

# ============================================================================
# MODIFIED: Observer for when experiment files are uploaded
# ============================================================================
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
    has_content = TRUE,
    has_sufficient_elements = TRUE,
    min_elements_found = 0,
    required_elements = 3,
    checked = FALSE,
    message = NULL
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
  
  # ============================================================================
  # NEW: Check Description field status after loading plate data
  # ============================================================================
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         CHECKING DESCRIPTION FIELD                       ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  
  # Get the current delimiter (default to underscore)
  delimiter <- if (!is.null(input$description_delimiter)) input$description_delimiter else "_"
  
  # Check description elements
  desc_check <- check_description_elements(
    plate_data = all_plates,
    delimiter = delimiter,
    required_elements = 3  # PatientID, TimePeriod, DilutionFactor
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
  
  cat("Description field status:\n")
  cat("  → Has content:", desc_check$has_content, "\n")
  cat("  → Has sufficient elements:", desc_check$has_sufficient_elements, "\n")
  cat("  → Minimum elements found:", desc_check$min_elements_found, "\n")
  cat("  → Required elements:", desc_check$required_elements, "\n")
  
  if (!is.null(desc_check$message)) {
    cat("  → Message:", desc_check$message, "\n")
  }
  
  cat("╚══════════════════════════════════════════════════════════╝\n")
  
  # Show notification based on description status
  if (!desc_check$has_content) {
    showNotification(
      "Description field is blank in all sample wells. Default values will be used. You will need to manually update the layout template.",
      type = "warning",
      duration = 10
    )
  } else if (!desc_check$has_sufficient_elements) {
    showNotification(
      sprintf(
        "Description field has insufficient elements (%d found, %d required). Some fields will use default values.",
        desc_check$min_elements_found,
        desc_check$required_elements
      ),
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

# ============================================================================
# MODIFIED: Observer for delimiter change to re-check description
# ============================================================================
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

# Continue with rest of the original file...
# The remaining observers and outputs should remain the same
