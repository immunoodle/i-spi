
# create upload pathname
output$upload_path_text <- renderText({
  paste(stri_replace_all_charclass(Sys.getenv("upload_template_path"), "\\p{WHITE_SPACE}", ""))
})

getData <- reactive({
  if(is.null(input$upload_to_shiny)) return(NULL)
})

output$fileUploaded <- reactive({
  return(!is.null(getData()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

### main observe that creates base tabs reading and processing the xMap

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
                     shinyDirButton("experiment_folder", "Select an Experiment Folder", "Please select a folder containing your experiment files."),
                     helpText("In this folder: raw bead array data (excel formatted) and completed layout template (after filling out generated template)"),
                     uiOutput("folderpath"),
                     conditionalPanel(
                       condition = "output.hasExperimentPath",
                        radioButtons(inputId = "n_wells_on_plate", "Select Number of Wells to Generate on Plates",
                                  choices = c(96, 6, 12, 24, 48, 384, 1536),
                                  selected = 96,
                                  inline = T),
                       downloadButton("blank_layout_file", "Generate a Layout file"),
                       fileInput("upload_layout_file"
                                 , label="Upload a completed layout file (only accepts xlsx, xls)"
                                 , accept=c(".xlsx",".xls")
                                 , multiple=FALSE)
                     ),
                     conditionalPanel(
                       condition = "input.upload_layout_file != ''",
                       uiOutput("view_layout_file_ui"),
                       # actionButton("view_layout_plate_id_sheet", "View Layout Plate ID"),
                       # actionButton("view_layout_plates_map_sheet", "View Layout Plate Map"),
                       # actionButton("view_layout_antigen_list_sheet", "View Layout Antigen List")
                     #),
                    # uiOutput("file_summary"),
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
                     # fileInput("upload_batch_bead_array"
                     #           , label="Upload one or more plate/batch files (only accepts xlsx, xls)"
                     #           , accept=c(".xlsx",".xls")
                     #           , multiple=TRUE),
                     uiOutput("batch_validation_status"),
                     tableOutput("batch_invalid_messages"),
                     uiOutput("upload_batch_data_button")
                    )

                     )
                      # actionButton("uplaod_batch", "Upload Batch")
                    # uiOutput("plate_tabs")

                     #rHandsontableOutput("batch_plate_table")



                   #  verbatimTextOutput("layout_sheets")

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
      )
    )
  } else {
    import_plate_data_title<- paste("Choose or create a study for Importing Plate Data")
  }

   }
  #else {
  #   NULL
  # }
})

observeEvent(input$readxMap_study_accession, {
  print(paste("readxMap_study_accession clicked:", input$readxMap_study_accession))

  if (input$readxMap_study_accession != "Click here") {
 # imported_h_study(input$readxMap_study_accession)

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



# observeEvent(input$readxMap_experiment_accession_import, {
#   req(input$readxMap_experiment_accession_import)
#   req(input$readxMap_experiment_accession_import != "Click here")
#
#   print(paste("readxMap_experiment_accession_import observe:", input$readxMap_experiment_accession_import))
#
#   imported_h_experiment(input$readxMap_experiment_accession_import)
#
# })
# observeEvent(input$readxMap_study_accession_import, {
#   print(paste("readxMap_study_accession_import clicked",input$readxMap_study_accession_import))
#   study_exp <- reactive_df_study_exp()
#   filtered_exp <- study_exp[study_exp$study_accession == input$readxMap_study_accession_import, ]
#
#   if (nrow(filtered_exp) > 0) {
#     expvector_imp <<- setNames(filtered_exp$experiment_accession, filtered_exp$experiment_name)
#   } else {
#     expvector_imp <<- character(0)
#   }
#
#   print(paste("expvector_import:", expvector_imp))
#   experiment_drop_imp <<- c(c("Click OR Create New" = "Click here"), expvector_imp)
#   print(paste("experiments:",experiment_drop_imp))
#   updateSelectizeInput(session, inputId = "readxMap_experiment_accession_import",
#                        "Choose Existing Experiment Name OR Create a New Experiment Name (up to 15 characters)",
#                        choices <- experiment_drop_imp,
#                        selected = "Click here")
# })
#
# observeEvent(input$readxMap_study_accession, {
#   print(paste("readxMap_study_accession clicked: ",input$readxMap_study_accession))
#   study_exp <- reactive_df_study_exp()
#   filtered_exp <- study_exp[study_exp$study_accession == input$readxMap_study_accession, ]
#
#   if (nrow(filtered_exp) > 0) {
#     expvector <- setNames(filtered_exp$experiment_accession, filtered_exp$experiment_name)
#   } else {
#     expvector <- character(0)
#   }
#   print(paste("expvector:", expvector))
#   experiment_drop <- c(c("Click here" = "Click here"), expvector)
#   print(paste("experiments:",experiment_drop))
#   updateSelectInput(session, inputId = "readxMap_experiment_accession",
#                     "Choose Experiment Name",
#                     choices <- experiment_drop,
#                     selected = "Click here")
#
# })

### read template and create the preview template tab
## Clicks browse to load a plate/batch
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

# # # rebuild RHandsontable tables
# observeEvent(original_df_combined(), {
#   req(original_df_combined())
#
#   for (type in names(original_df_combined())) {
#     output[[paste0("table_", type)]] <- renderRHandsontable({
#       rhandsontable(original_df_combined()[[type]],
#                     useTypes = FALSE,
#                     overflow = "visible",
#                     horizontal_scroll = TRUE,
#                     vertical_scroll = TRUE,
#                     width = 1024, height = 300) %>%
#         hot_col(
#           col = "feature",
#           type = "dropdown",
#           source = c(
#             "ADCD", "Total_IgG", "IgG1", "IgG2", "IgG3", "IgG4",
#             "IgA", "IgA1", "IgM", "IgM1", "FcgR2a",
#             "FcgR2b", "FcgR3a", "FcgR3b"
#           )
#         )
#     })
#   }
# })








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


  # lapply(names(type_observers), function(nm) {
  #   if (!is.null(type_observers[[nm]])) type_observers[[nm]]$destroy()
  # })
  #
  # # Create fresh ones for S, C, B
  # lapply(type_vector_observes, function(type) {
  #   type_observers[[type]] <- observeEvents(type)
  # })

  }


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
  allowed_types <- c("P", "X", "S", "C", "B")
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
  # type_x_status(list(plate_exists = FALSE, n_record = 0))
  # type_s_status(list(plate_exists = FALSE, n_record = 0))
  # type_c_status(list(plate_exists = FALSE, n_record = 0))
  # type_b_status(list(plate_exists = FALSE, n_record = 0))

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

# output$optimization_tab <- renderUI({
#   req(is_optimization_plate(plate_data()))
#   req(all_completed())
#
#   tabPanel(
#     title = "Optimization",
#     uiOutput("plate_optimized_status"),
#     uiOutput("ui_optimization"),
#     uiOutput("ui_split_button")
#   )
# })
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

# account for Windows and Mac operating systems. Ignores winslash when on Mac
#roots <- c(home = normalizePath("~", winslash = c("/")))
roots <- c(home = normalizePath("~", winslash = "/"), shinyFiles::getVolumes()())


# Initialize folder selection
shinyDirChoose(input, "experiment_folder", roots = roots, session = session)

# folderPath <- reactive({
#   req(input$folder)
#   parseDirPath(roots, input$folder)
# })

# Observe and display the selected folder
# output$folderpath <- renderPrint({
#   req(input$experiment_folder)
#   # Parse the folder path selected
#   experimentPath(parseDirPath(roots, input$experiment_folder))
#   parseDirPath(roots, input$experiment_folder)
# })

output$folderpath <- renderUI({
  req(input$experiment_folder)
  folder <- parseDirPath(roots, input$experiment_folder)
  experimentPath(parseDirPath(roots, input$experiment_folder))
  HTML(paste0("<b>Selected folder:</b>", folder, "</span>"))
})

output$hasExperimentPath <- reactive({
  path <- experimentPath()
  !is.null(path) && nzchar(path)   # nzchar() checks for non-empty string
})
outputOptions(output, "hasExperimentPath", suspendWhenHidden = FALSE)

observeEvent(experimentPath(), {
  cat("changed experiment_folder")
  path <- experimentPath()
  excel_files <- list.files(path, pattern = "\\.xlsx?$", full.names = TRUE)
  # exclude any layout files
  excel_files <- excel_files[!grepl("layout_template", excel_files, ignore.case = TRUE)]
  req(length(excel_files) > 0)



  all_data <- lapply(excel_files, function(file_path) {
    # read in whole dataset
    bead_array_plate <- openxlsx::read.xlsx(file_path, colNames = FALSE, sheet = 1)
    well_row <- which(bead_array_plate[,c(1)] == "Well")

    # Read header (first 7 rows)
    header_info <- openxlsx::read.xlsx(file_path, rows = 1:(well_row - 1), colNames = FALSE, sheet = 1)
    header_info <- parse_metadata_df(header_info)   # Your custom function

    # Read plate data (from row 8 onward)
    plate_data <- openxlsx::read.xlsx(file_path, startRow = well_row, sheet = 1, colNames = TRUE)
    # clean up column names to replace . with a space
    names(plate_data) <- gsub("\\.", " ", names(plate_data))
    # Remove completely empty rows
    plate_data <- plate_data[!apply(
      plate_data, 1,
      function(x) all(is.na(x) | trimws(x) == "" | trimws(x) == "NA")
    ), ]

    list(header = header_info, plate = plate_data)
  })

  names(all_data) <- basename(excel_files)

  # Separate lists (like your upload handler)
  header_list <<- lapply(all_data, `[[`, "header")
  plate_list  <<- lapply(all_data, `[[`, "plate")

  # Save to reactives (assuming you defined these as reactiveVals)
  bead_array_header_list(header_list)
  bead_array_plate_list(plate_list)

  all_plates <- read_bind_xlsx(folder = path, x = "plate")
  batch_plate_data(all_plates)


  showNotification("Data Files Uploaded")
})




output$blank_layout_file <- downloadHandler(
  filename = function() {
    paste0(input$readxMap_study_accession, "_", input$readxMap_experiment_accession_import, "_layout_template.xlsx")
  },
  content = function(file) {
    req(experimentPath())
    req(bead_array_header_list())
    generate_layout_template(
      folder = experimentPath(),
      study_accession = input$readxMap_study_accession,
      experiment_accession = input$readxMap_experiment_accession_import,
      n_wells = input$n_wells_on_plate,
      header_list = bead_array_header_list(),
      output_file = file   # pass Shiny temp file
    )
  }
)

# output$blank_layout_file <- downloadHandler(
#   filename = function() {
#     paste0("layout_template.xlsx")
#   },
#   content = function(file) {
#     file.copy("www/blank_layout_template.xlsx", file)
#   }
# )

# When a file is uploaded, read all sheets and store them
# observeEvent(input$upload_layout_file, {
#   req(input$upload_layout_file)
#
#   file_path <- input$upload_layout_file$datapath
#   sheet_names <- readxl::excel_sheets(file_path)
#
#   # Skip the first 2 rows and use 3rd as header
#   layout_template_sheets$sheets <- purrr::map(sheet_names, ~ read_excel(
#     file_path,
#     sheet = .x,
#     skip = 2,
#     col_names = TRUE
#   ))
#
#   names(layout_template_sheets$sheets) <- sheet_names
#
#
#   print(names(layout_template_sheets$sheets))
# })

observeEvent(input$upload_layout_file, {
 # req(input$upload_layout_file)

  # # reset reactive that hold the data
  inLayoutFile(NULL)
  avaliableLayoutSheets(NULL)

  layout_template_sheets(list())

  # layout_template_sheets$plate_id <- NULL
  # layout_template_sheets$plates_map <- NULL
  # layout_template_sheets$antigen_list <- NULL

  # Store the uploaded file
  inLayoutFile(
    input$upload_layout_file
  )

  if (is.null(inLayoutFile())) {
    return(NULL)
  }

  # # Get file path
  # inFile <- input$upload_layout_file$datapath

  # Get all sheet names
  sheets <- readxl::excel_sheets(inLayoutFile()$datapath)
  cat("\nSheets found:", paste(sheets, collapse = ", "), "\n")

  # Read each sheet, skipping the first 2 rows
  # all_sheets <- lapply(sheets, function(sheet_name) {
  #   skip_rows <- if (sheet_name == "timepoint")  4 else 2
  #   readxl::read_excel(inLayoutFile()$datapath, sheet = sheet_name, skip = skip_rows)
  # })
  all_sheets <- lapply(sheets, function(sheet_name) {
    skip_rows <- if (sheet_name == "timepoint") 4 else 2

    df <- readxl::read_excel(
      inLayoutFile()$datapath,
      sheet = sheet_name,
      skip = skip_rows
    )

    # Remove trailing rows that are all NA
    df <- df[rowSums(!is.na(df)) > 0, ]

    return(df)
  })

  names(all_sheets) <- sheets  # Assign sheet names

  # Now assign to reactive Values object
  layout_template_sheets(all_sheets)

  batch_header <- construct_batch_upload_metadata(plates_map = all_sheets[["plates_map"]],
                                                   plate_metadata_list = bead_array_header_list(),
                                                   workspace_id = userWorkSpaceID(),
                                                   currentuser = currentuser())

  batch_metadata <- combine_plate_metadata(head_list = batch_header)
  # store the number of wells
  batch_metadata <- merge(
           batch_metadata,
           all_sheets[["plate_id"]][, c("plate_filename", "number_of_wells")],
           by.x = "file_name", by.y = "plate_filename",
           all.x = TRUE
  )
names(batch_metadata)[names(batch_metadata) == "number_of_wells"] <- "n_wells"
batch_metadata <- batch_metadata
validate_metadata_result <- validate_batch_plate_metadata(plate_metadata = batch_metadata, plate_id_data = all_sheets[["plate_id"]])

bead_array_validation <- validate_batch_bead_array_data(combined_plate_data = batch_plate_data(), antigen_list = all_sheets[["antigen_list"]],
                                blank_keyword = input$batch_blank)

# bead_array_validation <- validate_batch_bead_array(plate_data_list = bead_array_plate_list(), antigen_list = all_sheets[["antigen_list"]],
#                           blank_keyword = input$batch_blank)

if (bead_array_validation$is_valid && validate_metadata_result$is_valid) {
  batch_validated <- TRUE
  batch_metadata(batch_metadata)
} else {
  batch_validated <- FALSE
}

output$batch_validation_status <- renderUI({
  createValidateBatchBadge(batch_validated)
})

output$batch_invalid_messages <- renderTable({
  if (!batch_validated) {
    create_batch_invalid_message_table(validate_metadata_result, bead_array_validation)
  } else {
    NULL
  }
})

# output$upload_batch_data_button <- renderUI({
#   if (batch_validated) {
#     actionButton("uplaod_batch_button", "Upload Batch")
#   } else {
#     NULL
#   }
# })

output$upload_batch_data_button <- renderUI({

  req(batch_metadata())
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
  plates_exist <- nrow(existing_plates) > 0

  # UI elements
  badge <- createUploadedBatchBadge(plates_exist)
  button <- if (batch_validated && !plates_exist) {
    actionButton("uplaod_batch_button", "Upload Batch")
  } else {
    NULL
  }

  # Combine into UI block
  tagList(
    badge,
    br(),
    button
  )
})


# validate_metadata_result <- validate_batch_plate(plate_metadata = batch_metadata, plate_data_list = bead_array_plate_list(),
#                                          plate_id_data = all_sheets[["plate_id"]])
  # Dynamically generate UI
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
  # if ("plate_id" %in% names(all_sheets)) {
  #   layout_template_sheets$plate_id <- all_sheets[["plate_id"]]
  # }
  # if ("plates_map" %in% names(all_sheets)) {
  #   layout_template_sheets$plates_map <- all_sheets[["plates_map"]]
  # }
  # if ("antigen_list" %in% names(all_sheets)) {
  #   layout_template_sheets$antigen_list <- all_sheets[["antigen_list"]]
  # }
  #
  # cat("\nSheet dimensions:\n")
  # print(lapply(all_sheets, dim))
})


observeEvent(layout_template_sheets(), {
  cat("changed layout sheets")

})

#
# observe({
#   file <- input$upload_layout_file
#   cat("current file: ")
#   print(file)
#   # If no file is selected
#   if (is.null(file)) {
#     cat("No layout file selected — resetting reactive values.\n")
#     inLayoutFile(NULL)
#     avaliableLayoutSheets(NULL)
#     layout_template_sheets(list())
#   }
# })

# observe({
#   if (is.null(input$upload_layout_file)) {
#     print("No file selected yet.")
#   } else {
#     print("File has been selected.")
#     print(input$upload_layout_file) # Shows the file information
#   }
# })

# observe({
#   if (is.null(input$upload_layout_file)) {
#     layout_template_sheets(list())
#     removeModal()
#     output$layout_plate_id_sheet <- rhandsontable::renderRHandsontable(NULL)
#     output$layout_plates_map_sheet <- rhandsontable::renderRHandsontable(NULL)
#     output$layout_antigen_list_sheet <- rhandsontable::renderRHandsontable(NULL)
#   }
# })
# all_sheets <- reactive({
#   req(input$upload_layout_file)
#
#   file_path <- input$upload_layout_file$datapath
#   sheet_names <- excel_sheets(file_path)
#
#   # Read each sheet into a list
#   sheets_list <- map(sheet_names, ~ read_excel(file_path, sheet = .x, skip = 2,))
#   names(sheets_list) <- sheet_names
#   sheets_list
# })

# output$layout_sheets <- renderPrint({
#   req(layout_template_sheets)
#   cat("Sheets loaded:\n")
#   print(names(layout_template_sheets))
#   cat("\n Sheet dimensions:\n")
#   lapply(layout_template_sheets$all_sheets, dim)
#   for(name in names(layout_template_sheets$all_sheets)) {
#     print(head(layout_template_sheets$all_sheets[[name]], 5))
#   }
#
# })
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

  # platemetadata <<- header_info()
  #print(header_info())
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

    # Remove the dynamically generated buttons
    output$view_layout_file_ui <- renderUI({NULL})

  }
})


# observeEvent(input$upload_batch_bead_array, {
#   req(input$upload_batch_bead_array)
#
#   uploaded_files <- input$upload_batch_bead_array
#
#   # Read each Excel file into a list element
#   files_list <- lapply(uploaded_files$datapath, function(file_path) {
#     read_excel(file_path)
#   })
#
#   # Assign filenames as names
#   names(files_list) <- uploaded_files$name
#
#   # Store the list in the reactiveVal
#   batch_data_list(files_list)
# })
# observeEvent(input$upload_batch_bead_array, {
#   req(input$upload_batch_bead_array)
#
#   uploaded_files <- input$upload_batch_bead_array
#
#   # Read only the first sheet from each uploaded file
#   files_list <- lapply(uploaded_files$datapath, function(path) {
#     readxl::read_excel(path, sheet = 1)
#   })
#
#   # Assign filenames as list names
#   names(files_list) <- uploaded_files$name
#
#   # Store in reactiveVal
#   batch_data_list(files_list)
# })
# observeEvent(input$upload_batch_bead_array, {
#   req(input$upload_batch_bead_array)
#   uploaded_files <- input$upload_batch_bead_array
#
#   # Read each file’s first sheet (header + data)
#   files_list <- lapply(uploaded_files$datapath, function(path) {
#     # Read header info (first 7 rows, no column names)
#     header_info <- openxlsx::read.xlsx(path, rows = 1:7, colNames = FALSE, sheet = 1)
#     # Read plate data (starting from row 8)
#     plate_data <- openxlsx::read.xlsx(path, startRow = 8, sheet = 1, colNames = TRUE)
#
#     # Clean up: remove empty rows
#     plate_data <- plate_data[!apply(
#       plate_data, 1,
#       function(x) all(is.na(x) | trimws(x) == "" | trimws(x) == "NA")
#     ), ]
#
#     # Return both header and plate data
#     list(header = header_info, plate = plate_data)
#   })
#
#   # Assign filenames as list names
#   names(files_list) <- uploaded_files$name
#
#   # Store in reactiveVal
#   batch_data_list(files_list)
# })

# observeEvent(input$upload_batch_bead_array, {
#   req(input$upload_batch_bead_array)
#   uploaded_files <- input$upload_batch_bead_array
#
#   # Use lapply to process all uploaded files
#   all_data <- lapply(uploaded_files$datapath, function(path) {
#     # Read header (first 7 rows)
#     header_info <- openxlsx::read.xlsx(path, rows = 1:7, colNames = FALSE, sheet = 1)
#     header_info <- parse_metadata_df(header_info)
#     # Read plate data (from row 8 onward)
#     plate_data <- openxlsx::read.xlsx(path, startRow = 8, sheet = 1, colNames = TRUE)
#
#     # Remove completely empty rows
#     plate_data <- plate_data[!apply(
#       plate_data, 1,
#       function(x) all(is.na(x) | trimws(x) == "" | trimws(x) == "NA")
#     ), ]
#
#     list(header = header_info, plate = plate_data)
#   })
#
#   # Assign names using uploaded filenames
#   names(all_data) <- uploaded_files$name
#
#   # Separate into two named lists
#   header_list <- lapply(all_data, `[[`, "header")
#   plate_list  <- lapply(all_data, `[[`, "plate")
#
#   # Store in reactiveVals
#   bead_array_header_list(header_list)
#   bead_array_plate_list(plate_list)
#
#   plate_names <- sapply(header_list, function(x) x$plate)
#   sample_dilution_plate_df(
#     data.frame(
#       plate = plate_names,
#       sample_dilution_factor = NA_real_,
#       stringsAsFactors = FALSE
#     )
#   )
#
#   batch_header <- construct_batch_upload_metadata(plates_map = layout_template_sheets()[["plates_map"]],
#                                                   plate_metadata_list = bead_array_header_list(),
#                                                   workspace_id = userWorkSpaceID(),
#                                                   currentuser = currentuser())
#
#   batch_metadata <- combine_plate_metadata(head_list = batch_header)
#   validation_result <- validate_batch_plate(plate_metadata = batch_metadata, plate_data_list = bead_array_plate_list(),
#                                              plate_id_data = layout_template_sheets()[["plate_id"]])
#   bead_array_validation <- validate_batch_bead_array(plate_data_list = bead_array_plate_list(),
#                                                       antigen_list = layout_template_sheets()[["antigen_list"]],
#                                                       blank_keyword = input$batch_blank)
#   if (bead_array_validation$is_valid && validation_result$is_valid) {
#     batch_validated <- TRUE
#   } else {
#     batch_validated <- FALSE
#   }
#
#
#
#   output$batch_validation_status <- renderUI({
#     createValidateBatchBadge(batch_validated)
#   })
#
#   output$batch_invalid_messages <- renderTable({
#     if (!batch_validated) {
#       create_batch_invalid_message_table(validation_result, bead_array_validation)
#     } else {
#       NULL
#     }
#   })
#
#   output$upload_batch_data_button <- renderUI({
#     if (batch_validated) {
#       actionButton("uplaod_batch_button", "Upload Batch")
#     } else {
#       NULL
#     }
#   })
#   # sample_dilution_plate_df(
#   #   data.frame(
#   #     plate_name = names(bead_array_plate_list()),
#   #     sample_dilution_factor = NA_real_,
#   #     stringsAsFactors = FALSE
#   #   ))
# })
# Display basic info about uploaded files
output$file_summary <- renderPrint({
 # data_list <- batch_data_list()
 # head_list <<- bead_array_header_list()
 # plate_list <<- bead_array_plate_list()
 # plates_map <<- layout_template_sheets()[["plates_map"]]
 # antigen_list <<- layout_template_sheets()[["antigen_list"]]
 # plate_id_data <<- layout_template_sheets()[["plate_id"]]
 # timepoint_map <<- layout_template_sheets()[["timepoint"]]
 # subject_map <<- layout_template_sheets()[["subject_groups"]]

  # batch_header <- construct_batch_upload_metadata(plates_map = layout_template_sheets()[["plates_map"]],
  #                                                plate_metadata_list = bead_array_header_list(),
  #                                                workspace_id = userWorkSpaceID(),
  #                                                currentuser = currentuser())


  # if (length(data_list) == 0) {
  #   cat("No batch files uploaded yet.")
  # } else {
  #   cat("Uploaded batch files:\n\n")
  #   for (file in names(data_list)) {
  #     df <- data_list[[file]]$plate
  #     cat("•", file, "→", nrow(df), "rows x", ncol(df), "columns\n")
  #   }
  # }
})

plate_layout_plots <- reactive({
  req(layout_template_sheets()[["plates_map"]])
  req(layout_template_sheets()[["plate_id"]])

  plates_map <- layout_template_sheets()[["plates_map"]]
  plate_id_data <- layout_template_sheets()[["plate_id"]]

  # Call your function
  plot_plate_layout(plates_map, plate_id_data)
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


# # Dynamically generate tabs for each plot
# output$plate_tabs <- renderUI({
#   req(plate_layout_plots())
#   plots <- plate_layout_plots()
#
#   tabs <- lapply(names(plots), function(name) {
#     tabPanel(
#       title = name,
#       plotOutput(paste0("plot_", name))
#     )
#   })
#
#   do.call(tabsetPanel, tabs)
# })
#
#
# #  Render each plot
# observe({
#   req(plate_layout_plots())
#   plots <- plate_layout_plots()
#
#   for (name in names(plots)) {
#     local({
#       plot_name <- name
#       output[[paste0("plot_", plot_name)]] <- renderPlot({
#         plots[[plot_name]]
#       })
#     })
#   }
# })

# Join sample dilutions for the plate to the header
construct_batch_upload_metadata <- function(plates_map, plate_metadata_list, currentuser, workspace_id) {

  cat("plates map")
  print(names(plates_map))
  cat("plate metadata list")
  print(plate_metadata_list)

  # get unique sample dilution factors in a dataframe by plate_number
  sample_dilutions_by_plate <- unique(
    plates_map[plates_map$specimen_type == "X",
               c("plate_number", "specimen_type", "specimen_dilution_factor")])

    names(sample_dilutions_by_plate)[names(sample_dilutions_by_plate) == "specimen_dilution_factor"] <- "sample_dilution_factor"

    print(sample_dilutions_by_plate)

    experiment_by_plate <- unique(
      plates_map[, c("plate_number", "experiment_name")]
    )

    # Add dilution factor to each head df in the list
    plate_metadata_list_updated <- lapply(plate_metadata_list, function(df) {

      plate_num <- df$plate[1]


      # Match with dilution table
      match_row <- sample_dilutions_by_plate[sample_dilutions_by_plate$plate_number == plate_num, , drop = FALSE]

      # Add dilution column if match found
      if (nrow(match_row) > 0) {
        df$sample_dilution_factor <- match_row$sample_dilution_factor
      } else {
        df$sample_dilution_factor <- NA_real_
      }

      # add experiment_name
      exp_row <- experiment_by_plate[
        experiment_by_plate$plate_number == plate_num, , drop = FALSE
      ]
      if (nrow(exp_row) > 0) {
        df$experiment_name <- exp_row$experiment_name
      } else {
        df$experiment_name <- NA_character_
      }

      # ## add metadata
       df$study_accession <- unique(plates_map$study_name)
       df$experiment_accession <- unique(plates_map$feature)
       df$workspace_id <- workspace_id
       df$auth0_user <- currentuser

      return(df)
      # ## add the study_name
      # df$study_accession <- unique(plates_map$study_name)
    })

    # add in source file for later join
    for (nm in names(plate_metadata_list_updated)) {
      plate_metadata_list_updated[[nm]]$source_file <- nm
    }

    return(plate_metadata_list_updated)
}

plot_plate_layout <- function(plates_map, plate_id_data) {
  #plates_map_v <<- plates_map
  # Join in number_of_wells info
  plates_map_joined <- merge(
    plates_map,
    plate_id_data[, c("study_name", "experiment_name", "plate_number", "number_of_wells")],
    by = c("study_name", "experiment_name", "plate_number"),
    all.x = TRUE
  )
  specimen_labels <- c(
    "S" = "Standard Curve",
    "C" = "Controls",
    "X" = "Samples",
    "B" = "Blanks"
  )
  plates_map_joined$`Specimen Type` <- specimen_labels[plates_map_joined$specimen_type]

  # Initialize list for storing plots
  plate_plots <- list()
  specimen_palette <-  c("Blanks" = "#F3C300",
                               "Controls" = "#2B3D26",
                               "Standard Curve" = "#A1CAF1",
                               "Samples" = "#8DB600")

  # Get unique combinations of study, experiment, and plate
  unique_combos <- unique(
    plates_map_joined[, c("study_name", "experiment_name", "plate_number")]
  )
  # remove trailing NA rows
  unique_combos <- unique_combos[rowSums(is.na(unique_combos)) != ncol(unique_combos), ]


  # Loop through each combination
  for (i in seq_len(nrow(unique_combos))) {
    study <- unique_combos$study_name[i]
    exp <- unique_combos$experiment_name[i]
    plate <- unique_combos$plate_number[i]

    # Filter data for this combination
    plates_map_joined_filtered <- plates_map_joined[plates_map_joined$study_name == study & plates_map_joined$experiment_name == exp
                                                    & plates_map_joined$plate_number == plate,]
    plates_map_joined_filtered <- plates_map_joined_filtered[rowSums(is.na(plates_map_joined_filtered)) != ncol(plates_map_joined_filtered), ]

    n_wells <- unique(plates_map_joined_filtered$number_of_wells)

    # if (i == 1) {
    #   message("First plate data preview:")
    #   print(head(plates_map_joined_filtered))
    #   assign("first_plate_data", plates_map_joined_filtered, envir = .GlobalEnv)
    # }
    # if (i == 2) {
    #   assign("second_plate", plates_map_joined_filtered, envir = .GlobalEnv)
    # }

    # Build title
    plot_title <- paste0(
      "Study: ", study,
      " | Experiment: ", exp,
      " | Plate: ", plate,
      " (", n_wells, " wells)"
    )

    plate_layout <- plate_plot_plotly2(
      data = plates_map_joined_filtered,
      plate_size = n_wells,
      title = plot_title,
      colour = specimen_palette
    )
    # plate_layout <- plate_plot_plotly(
    #   data = plates_map_joined_filtered,
    #   position = plates_map_joined_filtered$well,
    #   value =  plates_map_joined_filtered$`Specimen Type`,
    #   plate_size = n_wells,
    #   title = plot_title,
    #   colour = specimen_palette
    # )
    # plate_layout <- plate_plot(
    #   data = plates_map_joined_filtered,
    #   position = well,
    #   value = `Specimen Type`,
    #   plate_size = n_wells,
    #    plate_type = "round",
    #   title = plot_title,
    #   colour = specimen_palette,
    #    silent = FALSE,
    #    scale = 1
    # )



    # Add plot to list
    list_name <- paste(study, exp, plate, sep = "_")
    plate_plots[[list_name]] <- plate_layout
  }

  #plate_plots <<- plate_plots

  return(plate_plots)
}
# A plotly version of the plot_plate_function from the ggplate R library
plate_plot_plotly2 <- function(data, plate_size = 96, title = NULL, colour = NULL) {
  #data_v <<- data
  cat("Plate")
  print(unique(data$plate_number))
 cat("Plate Size")
 print(plate_size)
  data <- data |>
    dplyr::mutate(
      row = stringr::str_extract(well, pattern = "[:upper:]+"),
      col = as.numeric(stringr::str_extract(well, pattern = "\\d+")),
      row_num = as.numeric(match(row, LETTERS))
    )

  # determine number of rows and columns
  dims <- switch(
    as.character(plate_size),
    "6" = c(2, 3),
    "12" = c(3, 4),
    "24" = c(4, 6),
    "48" = c(6, 8),
    "96" = c(8, 12),
    "384" = c(16, 24),
    "1536" = c(32, 48)
  )
  n_rows <- dims[1]
  n_cols <- dims[2]

  # make grid of all wells
  plate_layout_grid <- expand.grid(
    Row = LETTERS[1:n_rows],
    Column = 1:n_cols
  )
  plate_layout_grid$well <- paste(plate_layout_grid$Row, plate_layout_grid$Column, sep = "")

  # find empty wells
  empty_wells <- setdiff(plate_layout_grid$well, data$well)
  empty_df <- plate_layout_grid |>
    dplyr::filter(well %in% empty_wells) |>
    dplyr::mutate(
      row_num = as.numeric(match(Row, LETTERS)),
      col = Column,
      `Specimen Type` = "Empty Well"
    )

  # flip y-axis to match lab plate layout
  data$row_num <- abs(data$row_num - (n_rows + 1))
  empty_df$row_num <- abs(empty_df$row_num - (n_rows + 1))

  # Build plot
  p <- plot_ly() %>%
    add_trace(
      data = data,
      x = ~col,
      y = ~row_num,
      type = "scatter",
      mode = "markers",
      color = ~`Specimen Type`,
      colors = colour,
      text = ~paste(
        "Well:", well,
        "<br>Specimen Type:", `Specimen Type`,
        ifelse(`Specimen Type` == "Samples",
               paste0("<br>Patient ID: ", subject_id,
                      "<br>Timepoint: ", timepoint_tissue_abbreviation),
               ""),
        "<br>Biosample Barcode:", biosample_id_barcode,
        "<br>Specimen Dilution Factor:", specimen_dilution_factor,
        "<br>Feature:", feature
      ),
      hoverinfo = "text",
      marker = list(size = 20, line = list(width = 1, color = "black"), symbol = "circle")
    ) %>%
    add_trace(
      data = empty_df,
      x = ~col,
      y = ~row_num,
      name = "Empty Well",
      type = "scatter",
      mode = "markers",
      marker = list(size = 20, color = "white", line = list(width = 1, color = "grey")),
      hoverinfo = "text",
      text = ~paste("Well:", well, "<br>Empty")
    ) %>%
    layout(
      title = title,
      legend = list(title = list(text = "Specimen Type")),
      yaxis = list(
        title = "",
        tickvals = seq(1, n_rows),
        ticktext = rev(LETTERS[1:n_rows]),
        showgrid = FALSE
      ),
      xaxis = list(
        title = "",
        tickvals = seq(1, n_cols),
        showgrid = FALSE
      ),
      showlegend = TRUE,
      shapes = list(
        list(
          type = "rect",
          x0 = 0.5, x1 = n_cols + 0.5,
          y0 = 0.5, y1 = n_rows + 0.5,
          line = list(color = "black", width = 1),
          fillcolor = "rgba(0,0,0,0)"
        )
      )
    )

  return(p)
}

# plate_plot_plotly <- function(data, position, value, plate_size = 96, title = NULL, colour = NULL) {
#   data_v <<- data
#   data <- data |>
#     dplyr::mutate(
#       row = stringr::str_extract({{ position }}, pattern = "[:upper:]+"),
#       col = as.numeric(stringr::str_extract({{ position }}, pattern = "\\d+")),
#       row_num = as.numeric(match(row, LETTERS))
#     )
#
#
#   # determine number of rows and columns
#   dims <- switch(
#     as.character(plate_size),
#     "6" = c(2, 3),
#     "12" = c(3, 4),
#     "24" = c(4, 6),
#     "48" = c(6, 8),
#     "96" = c(8, 12),
#     "384" = c(16, 24),
#     "1536" = c(32, 48)
#   )
#   n_rows <- dims[1]; n_cols <- dims[2]
#
#   plate_layout_grid <- expand.grid(
#     Row = LETTERS[1:n_rows],
#     Column = 1:n_cols
#   )
#   plate_layout_grid$well <- paste(plate_layout_grid$Row, plate_layout_grid$Column, sep = "")
#   empty_wells <- setdiff(plate_layout_grid$well, data$well)
#   empty_df <- plate_layout_grid |>
#     filter(well %in% empty_wells) |>
#     mutate(
#       row_num = as.numeric(match(Row, LETTERS)),
#       col = Column,
#       value = "Empty Well"
#     )
#   # flip y-axis (to match lab plate orientation)
#   data$row_num <- abs(data$row_num - (n_rows + 1))
#   empty_df$row_num <- abs(empty_df$row_num - (n_rows + 1))
#
#   # for (col in setdiff(names(data), names(empty_df))) {
#   #   empty_df[[col]] <- NA
#   # }
#   p <- plot_ly()
#
#   p <- p %>% add_trace(
#     data = data,
#     x = ~col,
#     y = ~row_num,
#     type = "scatter",
#     mode = "markers",
#     color = ~{{ value }},
#     colors = colour,
#     text = ~paste("Well:", {{ position }},
#                   "<br>Specimen Type:", {{ value }},
#                   ifelse({{ value }} == "Samples",
#                          paste0("<br>Patient ID: ", data$subject_id,
#                                 "<br>Timepoint: ", data$timepoint),
#                          ""),
#                   "<br> Biosample Barcode: ", data$biosample_id_barcode,
#                   "<br> Specimen Dilution Factor: ", data$specimen_dilution_factor,
#                   "<br> Feature: ", data$feature
#     ),
#     hoverinfo = "text",
#     marker = list(size = 20, line = list(width = 1, color = "black"), symbol = "circle")
#   ) %>%
#     add_trace(
#       data = empty_df,
#       x = ~col,
#       y = ~row_num,
#       type = "scatter",
#       mode = "markers",
#       marker = list(size = 20, color = "white", line = list(width = 1, color = "grey")),
#       hoverinfo = "text",
#       text = ~paste("Well:", well, "<br>Empty")
#     ) %>%
#     layout(
#       title = title,
#       legend = list(title = list(text = "Specimen Type")),
#       yaxis = list(
#         title = "", tickvals = seq(1, n_rows), ticktext = rev(LETTERS[1:n_rows]),
#         showgrid = F
#         # autorange = "reversed"
#       ),
#       xaxis = list(title = "", tickvals = seq(1, n_cols),
#                    showgrid = F),
#       showlegend = TRUE,
#       shapes = list(
#         list(
#           type = "rect",
#           x0 = 0.5, x1 = n_cols + 0.5,
#           y0 = 0.5, y1 = n_rows + 0.5,
#           line = list(color = "black", width = 1),
#           fillcolor = "rgba(0,0,0,0)"
#         )
#       )
#
#     )
#
#   return(p)
# }

# plate_plot_plotly <- function(data, position, value, plate_size = 96, title = NULL, colour = NULL) {
#   data_v <<- data
#   data <- data |>
#     dplyr::mutate(
#       row = stringr::str_extract({{ position }}, pattern = "[:upper:]+"),
#       col = as.numeric(stringr::str_extract({{ position }}, pattern = "\\d+")),
#       row_num = as.numeric(match(row, LETTERS))
#     )
#
#
#   # determine number of rows and columns
#   dims <- switch(
#     as.character(plate_size),
#     "6" = c(2, 3),
#     "12" = c(3, 4),
#     "24" = c(4, 6),
#     "48" = c(6, 8),
#     "96" = c(8, 12),
#     "384" = c(16, 24),
#     "1536" = c(32, 48)
#   )
#   n_rows <- dims[1]; n_cols <- dims[2]
#
#   plate_layout_grid <- expand.grid(
#     Row = LETTERS[1:n_rows],
#     Column = 1:n_cols
#   )
#   plate_layout_grid$well <- paste(plate_layout_grid$Row, plate_layout_grid$Column, sep = "")
#   empty_wells <- setdiff(plate_layout_grid$well, data$well)
#   empty_df <- plate_layout_grid |>
#     filter(well %in% empty_wells) |>
#     mutate(
#       row_num = as.numeric(match(Row, LETTERS)),
#       col = Column,
#       value = "Empty Well"
#     )
#   # flip y-axis (to match lab plate orientation)
#   data$row_num <- abs(data$row_num - (n_rows + 1))
#   empty_df$row_num <- abs(empty_df$row_num - (n_rows + 1))
#
#   # for (col in setdiff(names(data), names(empty_df))) {
#   #   empty_df[[col]] <- NA
#   # }
#  p <- plot_ly()
#
#   p <- p %>% add_trace(
#     data = data,
#     x = ~col,
#     y = ~row_num,
#     type = "scatter",
#     mode = "markers",
#     color = ~{{ value }},
#     colors = colour,
#     text = ~paste("Well:", {{ position }},
#                   "<br>Specimen Type:", {{ value }},
#                   ifelse({{ value }} == "Samples",
#                          paste0("<br>Patient ID: ", data$subject_id,
#                                 "<br>Timepoint: ", data$timepoint),
#                          ""),
#                   "<br> Biosample Barcode: ", data$biosample_id_barcode,
#                   "<br> Specimen Dilution Factor: ", data$specimen_dilution_factor,
#                   "<br> Feature: ", data$feature
#     ),
#     hoverinfo = "text",
#     marker = list(size = 20, line = list(width = 1, color = "black"), symbol = "circle")
#   ) %>%
#     add_trace(
#       data = empty_df,
#       x = ~col,
#       y = ~row_num,
#       type = "scatter",
#       mode = "markers",
#       marker = list(size = 20, color = "white", line = list(width = 1, color = "grey")),
#       hoverinfo = "text",
#       text = ~paste("Well:", well, "<br>Empty")
#     ) %>%
#     layout(
#       title = title,
#       legend = list(title = list(text = "Specimen Type")),
#       yaxis = list(
#         title = "", tickvals = seq(1, n_rows), ticktext = rev(LETTERS[1:n_rows]),
#         showgrid = F
#         # autorange = "reversed"
#       ),
#       xaxis = list(title = "", tickvals = seq(1, n_cols),
#                    showgrid = F),
#       showlegend = TRUE,
#       shapes = list(
#         list(
#           type = "rect",
#           x0 = 0.5, x1 = n_cols + 0.5,
#           y0 = 0.5, y1 = n_rows + 0.5,
#           line = list(color = "black", width = 1),
#           fillcolor = "rgba(0,0,0,0)"
#         )
#       )
#
#     )
#
#   return(p)
# }



# Helper function to combine plate metadata from batch load
combine_plate_metadata <- function(head_list) {
  plate_metadata <- do.call(rbind, lapply(names(head_list), function(nm) {
         df <- head_list[[nm]]
         df
    }))

  return(plate_metadata)
}


validate_batch_plate <- function(plate_metadata, plate_data_list, plate_id_data) {
  # plate_data_list <<- plate_data_list
  message_list <- c()

  check_uploaded_file_in_layout <- plate_metadata$file_name %in% plate_id_data$plate_filename
  if (!all(check_uploaded_file_in_layout)) {
     message_list <- c("Some uploaded plates are misssing in the layout file.")
  }
  # validate the required columns
  required_cols <- c("file_name", "rp1_pmt_volts", "rp1_target", "acquisition_date")
  missing_cols <- setdiff(required_cols, names(plate_metadata))

  if (length(missing_cols) > 0) {
    message_list <- c(
      message_list,
      paste("The following required plate metadata columns are missing so further parsing cannot be conducted:",
            paste(missing_cols, collapse = ", "))
    )
    # If critical metadata is missing, return early
    return(list(
      is_valid = FALSE,
      messages = message_list
    ))
  }

 # plate_metadata <<- plate_metadata

  # check to see if all files passes file Path
  pass_file_path <- all(looks_like_file_path(plate_metadata$file_name))
  if (!pass_file_path) {
    message_list <- c(message_list, "Ensure that alll file paths have foward or backward slashes based on Mac or Windows")
  }

  pass_rp1_pmt_volts <- all(check_rp1_numeric(plate_metadata$rp1_pmt_volts))
  is_numeric <- check_rp1_numeric(plate_metadata$rp1_pmt_volts)
  if (!pass_rp1_pmt_volts) {
    bad_rp1_pmt_volts <- plate_metadata[!is_numeric, c("plateid", "rp1_pmt_volts")]
    labeled_vals <- paste(bad_rp1_pmt_volts$plateid, bad_rp1_pmt_volts$rp1_pmt_volts, sep = ":")

    message_list <- c(message_list, paste(
      "Ensure that all files have an RP1 PMT (Volts) field that is numeric and if it is a decimal only one period is present. Values by plateid:",
      paste(labeled_vals, collapse = ", "))
    )
  }
  pass_rp1_target <- all(check_rp1_numeric(plate_metadata$rp1_target))
  is_target_numeric <- check_rp1_numeric(plate_metadata$rp1_target)
  if (!pass_rp1_target) {
    invalid_rp1_target <- plate_metadata[!is_target_numeric, c("plateid", "rp1_target")]
    labeled_bad_rp1_target <- paste(invalid_rp1_target$plateid, invalid_rp1_target$rp1_target, sep = ":")
    message_list <- c(message_list, paste("Ensure that the RP1 Target is numeric and if it is a decimal only one period is present. Values by plateid:",
                                          paste(labeled_bad_rp1_target,collapse = ", ")))
  }

  pass_time_format <- all(check_time_format(capitalize_am_pm(plate_metadata$acquisition_date)))
  is_time_format <- check_time_format(capitalize_am_pm(plate_metadata$acquisition_date))
  if (!pass_time_format) {
    invalid_time_format <- plate_metadata[!is_time_format, c("plateid", "aquisition_date")]
    labeled_invalid_time_format <- paste(invalid_time_format$plateid, invalid_time_format$aquisition_date)
    message_list <- c(message_list, paste("Ensure the acquisition date is in the following date time format: DD-MMM-YYYY, HH:MM AM/PM Example: 01-Oct-2025, 12:12 PM  |Current Value by plateid:",
                                          paste(labeled_invalid_time_format, collapse = ", ")))
  }

  # validate main data sets


  # if no invalid messages then it is good to pass
  is_valid <- length(message_list) == 0

  if (is_valid)  {
    return(list(
      is_valid = is_valid,
      messages = message_list,
      updated_plate_data = plate_data
    ))
  } else {
    return(list(
      is_valid = is_valid,
      messages = message_list
    ))
  }
}

validate_batch_bead_array <- function(plate_data_list, antigen_list, blank_keyword) {
  plate_data_list <<- plate_data_list
  message_list <- list()
  plate_names <- names(plate_data_list)

  for (name in plate_names) {
    # Get current dataset
    df <- plate_data_list[[name]]

    # Replace dots with spaces except for "%.Agg.Beads" and "Sampling.Errors"
    col_names <- ifelse(names(df) %in% c("Sampling.Errors", "%.Agg.Beads"),
                        names(df),                 # keep as-is
                        gsub("\\.", " ", names(df)))  # replace . with space

    # Assign new names
    names(df) <- col_names

    # Store updated data set
    plate_data_list[[name]] <- df
  }

  antigens_check <- unique(antigen_list$antigen_label_on_plate)
  check_antigens <- lapply(plate_data_list, function(df) {
    sapply(antigens_check, function(name) name %in% names(df))
  })

  # check on each plate and then all plates results together
  all_antigens_present <- all(sapply(check_antigens, function(x) all(x)))
  if (!all_antigens_present) {
    check_antigens_with_falses <- check_antigens[sapply(check_antigens, function(x) any(!x))]
    if (length(check_antigens_with_falses) > 0) {
      # For each plate with FALSE antigens
      for (plate_name in names(check_antigens_with_falses)) {
        missing_antigens <- names(check_antigens_with_falses[[plate_name]])[!check_antigens_with_falses[[plate_name]]]
        # Create a formatted message
        msg <- paste0(
          "Plate ", plate_name,
          " is missing the following antigens in the layout file: ",
          paste(missing_antigens, collapse = ", ")
        )
        message_list <- c(message_list, msg)
      }
    }
  }

  #pass_agg_bead_check_list <- lapply(plate_list, check_agg_bead_column)
  pass_agg_bead_check_list <- lapply(plate_data_list, check_batch_agg_bead_column)
  all_true <- all(sapply(pass_agg_bead_check_list, function(x) x$result))
  if (!all_true) {
    failed <- names(pass_agg_bead_check_list)[!sapply(pass_agg_bead_check_list, function(x) x$result)]
    agg_bead_message <- paste(
      "The following plates are missing a % Agg Beads column:\n",
      paste("-", failed, collapse = "\n"),
      "\nEnsure each plate has a % Agg Beads column after the last antigen."
    )
    message_list <- c(message_list, agg_bead_message)
  } else {
    pass_bead_count_check_list <- lapply(plate_data_list, check_bead_count)

    # Determine if all passed
    all_true_bead <- all(sapply(pass_bead_count_check_list, function(x) x[[1]]))

    if (!all_true_bead) {
      failed_bead <- names(pass_bead_count_check_list)[!sapply(pass_bead_count_check_list, function(x) x[[1]])]

      # Gather all detailed messages for failed plates
      bead_messages <- sapply(failed_bead, function(plate) {
        paste0("Plate: ", plate, "\n", pass_bead_count_check_list[[plate]][[2]])
      }, USE.NAMES = FALSE)

      # Combine into one final message
      bead_count_message <- paste(
        "Ensure the bead count is present after all MFI values in parentheses for the following plates:\n",
        paste(bead_messages, collapse = "\n\n"),
        sep = ""
      )

      # Add to message list
      message_list <- c(message_list, bead_count_message)
    }

    }


   # examine blanks in type column
  # Loop over all plate data frames
  for (i in seq_along(plate_data_list)) {
    plate_data <- plate_data_list[[i]]
    plate_name <- names(plate_data_list)[i]

    #  Check if blank correction is needed
    procceed_to_blank_check <- check_blank_in_sample_boolean(plate_data)
    if (!procceed_to_blank_check) {
      # Update plate data based on user keyword choice
      plate_data <- check_blank_in_sample(plate_data, blank_keyword = blank_keyword)
    }

    # Check blank description format
    pass_blank_description <- check_blank_description(plate_data)
    if (!pass_blank_description[[1]]) {
      message_list <- c(
        message_list,
        paste("Plate:", plate_name, "-", pass_blank_description[[2]])
      )
    }

    # Update modified plate back into list
    plate_data_list[[i]] <- plate_data
  }

  is_valid <- length(message_list) == 0

  if (is_valid)  {
    return(list(
      is_valid = is_valid,
      messages = message_list
    ))
  } else {
    return(list(
      is_valid = is_valid,
      messages = message_list
    ))
  }

}

  # pass_type_col <- check_type_column(plate_data)
  #   if (!pass_type_col[[1]]) {
  #     message_list <- c(message_list, pass_type_col[[2]])
  #   }

#   # validate main data set
#
#   pass_type_col <- check_type_column(plate_data)
#   if (!pass_type_col[[1]]) {
#     message_list <- c(message_list, pass_type_col[[2]])
#   }
#   pass_description <- check_sample_description(plate_data)
#   if (!pass_description[[1]]) {
#     message_list <- c(message_list, pass_description[[2]])
#   }
#
#   pass_standard_description <- check_standard_description(plate_data)
#   if (!pass_standard_description[[1]]) {
#     message_list <- c(message_list, pass_standard_description[[2]])
#   }
#
#   pass_agg_bead_check <- check_agg_bead_column(plate_data)
#   if (!pass_agg_bead_check[[1]]) {
#     message_list <- c(message_list, pass_agg_bead_check[[2]])
#   } else {
#     # check blanks if aggregate column is present
#     pass_bead_count_check <- check_bead_count(plate_data)
#     if (!pass_bead_count_check[[1]]) {
#       message_list <- c(message_list, paste("Ensure the bead count is present after all MFI values in parentheses for: \n", pass_bead_count_check[[2]], sep = ""))
#     }
#   }
#   # examine blanks in type column
#   procceed_to_blank_check <- check_blank_in_sample_boolean(plate_data)
#   if (!procceed_to_blank_check) {
#     # Update Plate Data based on keyword choice
#     plate_data <- check_blank_in_sample(plate_data, blank_keyword = blank_keyword)
#   }
#
#   # if blanks are processed still check it
#   pass_blank_description <- check_blank_description(plate_data)
#   if (!pass_blank_description[[1]]) {
#     message_list <- c(message_list, pass_blank_description[[2]])
#   }
#
#   # if no invalid messages then it is good to pass
#   is_valid <- length(message_list) == 0
#
#   if (is_valid)  {
#     return(list(
#       is_valid = is_valid,
#       messages = message_list,
#       updated_plate_data = plate_data
#     ))
#   } else {
#     return(list(
#       is_valid = is_valid,
#       messages = message_list
#     ))
#   }
#
# }
#
# plot_plate_layout <- function(plates_map, plate_id_data) {
#   plates_map_joined <- merge(
#     plates_map,
#          plate_id_data[, c("study_name", "experiment_name", "plate_number", "number_of_wells")],
#         by = c("study_name", "experiment_name", "plate_number"),
#          all.x = TRUE
#      )
#
#   plate_layout_plots <- list()
#
#   for (plate in unique(plates_map_joined$plate_number)) {
#      plates_map_joined_filtered <- plates_map_joined[plates_map_joined$plate_number == plate, ]
#      n_wells <- unique(plates_map_joined_filtered$number_of_wells)
#
#      plate_layout <- plate_plot(
#                         data = plates_map_joined_filtered,
#                         position = well,
#                         value = specimen_type,
#                         plate_size = n_wells,
#                         plate_type = "round")
#
#      plate_layout_plots[[plate]] <- plate_layout
#
#   }
#   return(plate_layout_plots)
# }
#
#

create_batch_invalid_message_table <- function(validation_result, bead_array_validation) {
  combined_messages <- c(validation_result$messages, unlist(bead_array_validation$messages))
  message_table <- data.frame(
    "Message Number" = seq_along(combined_messages),
    "Please correct the formatting errors in the file" = combined_messages,
    check.names = F,
    stringsAsFactors = FALSE
  )
  return(message_table)
}

# output$batch_uploded_status <- renderUI({
#   batch_metadata()
#   metadata_batch <- batch_metadata()
#   batch_study_accession <- unique(metadata_batch$study_accession)
#   plates_to_upload <- unique(metadata_batch$plate_id)
#
#   plate_list_sql <- paste0("'", paste(plates_to_upload, collapse = "', '"), "'")
#   query <- sprintf("
#     SELECT
#       xmap_header_id,
#       study_accession,
#       experiment_accession,
#       plate_id,
#       file_name,
#       acquisition_date,
#       reader_serial_number,
#       rp1_pmt_volts,
#       rp1_target,
#       auth0_user,
#       workspace_id,
#       plateid,
#       plate,
#       sample_dilution_factor,
#       n_wells
#     FROM madi_results.xmap_header
#     WHERE study_accession = '%s'
#       AND plate_id IN (%s);
#   ", batch_study_accession, plate_list_sql)
#
#   existing_plates <- DBI::dbGetQuery(conn, query)
#   if (nrow(existing_plates) > 0) {
#     createUploadedBatchBadge(TRUE)
#   } else {
#     createUploadedBatchBadge(FALSE)
#   }
#
# })


observeEvent(input$uplaod_batch_button, {
  batch_plates <- batch_plate_data()
  metadata_batch <- batch_metadata()
  timepoint_map_in <- layout_template_sheets()[["timepoint"]]
  subject_map_in <- layout_template_sheets()[["subject_groups"]]
  antigen_list_in <- layout_template_sheets()[["antigen_list"]]
  plates_map_in <-  layout_template_sheets()[["plates_map"]]

  # # get the study accession of the batch and plate ids that will be uploaded
  batch_study_accession <- unique(metadata_batch$study_accession)
  batch_experiment_accession <- unique(metadata_batch$experiment_name)
  plates_to_upload <- unique(metadata_batch$plate_id)

  plate_list_sql <- paste0("'", paste(plates_to_upload, collapse = "', '"), "'")
  query <- sprintf("
    SELECT
      xmap_header_id,
      study_accession,
      experiment_accession,
      plate_id,
      file_name,
      acquisition_date,
      reader_serial_number,
      rp1_pmt_volts,
      rp1_target,
      auth0_user,
      workspace_id,
      plateid,
      plate,
      sample_dilution_factor,
      n_wells
    FROM madi_results.xmap_header
    WHERE study_accession = '%s'
      AND plate_id IN (%s);
  ", batch_study_accession, plate_list_sql)

  existing_plates <- DBI::dbGetQuery(conn, query)
  if (nrow(existing_plates) > 0) {
    cat("These plates already exist for the study")
  } else {
    cat("These plates do not exist for the study")

    ## Upload header
    #metadata_batch <<- metadata_batch
    upload_metadata_df <- prepare_batch_header(metadata_batch)
    dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"),upload_metadata_df)

    ## join in plates (source file)
    batch_plates_2 <- merge(batch_plates, metadata_batch[, c("source_file", "plate")],
                          by.x = "source_file",
                          all.x = T)
    cat("after join batch ")
    # obtain samples to upload
    sample_map <- plates_map_in[plates_map_in$specimen_type == "X",]
    if (nrow(sample_map) > 0) {
     samples_to_upload <-  prepare_batch_bead_assay_samples(sample_plate_map = sample_map,
                                       combined_plate_data = batch_plates_2, batch_metadata = metadata_batch,
                                       antigen_list = antigen_list_in, subject_map = subject_map_in)

    dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_sample"),samples_to_upload)

      # samples_to_upload <<- prepare_batch_bead_assay_samples(sample_plate_map = sample_map, combined_plate_data = combined_plate_data,
      #                                                     antigen_list = layout_template_sheets()[["antigen_list"]],
      #                                                     subject_map = subject_map)
    }

    standards_map <- plates_map_in[plates_map_in$specimen_type == "S",]
    if (nrow(standards_map) > 0) {
      standards_to_upload <- prepare_batch_bead_assay_standards(standard_plate_map = standards_map, combined_plate_data = batch_plates_2,
                                         antigen_list = antigen_list_in,
                                         batch_metadata = metadata_batch)
      dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard"),standards_to_upload)

    }

    blanks_map <- plates_map_in[plates_map_in$specimen_type == "B",]
    if (nrow(blanks_map) > 0) {
      blanks_to_upload <- prepare_batch_bead_assay_blanks(blanks_plate_map = blanks_map, combined_plate_data = batch_plates_2, antigen_list = antigen_list_in,
                                      batch_metadata = metadata_batch)
      dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_buffer"), blanks_to_upload)
    }


    controls_map <- plates_map_in[plates_map_in$specimen_type == "C",]
    if (nrow(controls_map) > 0) {
    controls_to_uplaod <- prepare_batch_bead_assay_controls(controls_plate_map = controls_map, combined_plate_data = batch_plates_2, antigen_list =
                                        antigen_list_in, batch_metadata = metadata_batch)
    dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_control"),controls_to_uplaod)

    }

    antigen_family_df <- prepare_batch_antigen_family(antigen_list_in)

    existing_antigens <- DBI::dbGetQuery(
      conn,
      sprintf(
        "SELECT study_accession, experiment_accession, antigen FROM madi_results.xmap_antigen_family
     WHERE study_accession = '%s'
        AND experiment_accession = '%s'",
        batch_study_accession,
         batch_experiment_accession

      )
    )

    # Anti-join keeping only rows not already in DB
    antigen_family_to_insert <- dplyr::anti_join(
      antigen_family_df,
      existing_antigens,
      by = c("study_accession", "experiment_accession", "antigen")
    )

    # Append if new rows exist
    if (nrow(antigen_family_to_insert) > 0) {
      DBI::dbAppendTable(
        conn,
        DBI::Id(schema = "madi_results", table = "xmap_antigen_family"),
        antigen_family_to_insert
      )
      cat("Inserted new antigen family entries\n")
    } else {
      cat("No new antigen family entries to insert\n")
    }

    # dbAppendTable(conn,
    #               Id(schema = "madi_results", table = "xmap_antigen_family"),
    #               antigen_family_df)

    planned_visits_df <- prepare_planned_visits(timepoint_map = timepoint_map_in)
    existing_visits <- DBI::dbGetQuery(
      conn,
      sprintf(
        "SELECT study_accession, timepoint_name
     FROM madi_results.xmap_planned_visit
     WHERE study_accession = '%s'",
        batch_study_accession
      )
    )

    planned_visits_to_insert <- dplyr::anti_join(
      planned_visits_df,
      existing_visits,
      by = c("study_accession", "timepoint_name")
    )

    if (nrow(planned_visits_to_insert) > 0) {
      DBI::dbAppendTable(
        conn,
        DBI::Id(schema = "madi_results", table = "xmap_planned_visit"),
        planned_visits_to_insert
      )
      cat("Inserted new planned visit rows\n")
    } else {
      cat("No new planned visits to insert\n")
    }
   }

  showNotification("Batch Uploaded")
})


# observeEvent(input$uplaod_batch_button, {
# data_list <- batch_data_list()
# head_list <<- bead_array_header_list()
# plate_list <<- bead_array_plate_list()
# plates_map <<- layout_template_sheets()[["plates_map"]]
# antigen_list <<- layout_template_sheets()[["antigen_list"]]
# plate_id_data <<- layout_template_sheets()[["plate_id"]]
# timepoint_map <<- layout_template_sheets()[["timepoint"]]
# subject_map <<- layout_template_sheets()[["subject_groups"]]
#
# batch_header <- construct_batch_upload_metadata(plates_map = layout_template_sheets()[["plates_map"]],
#                                                 plate_metadata_list = bead_array_header_list(),
#                                                 workspace_id = userWorkSpaceID(),
#                                                 currentuser = currentuser())
# # metadata to store in the header table
# batch_metadata <- combine_plate_metadata(head_list = batch_header)
# # store the number of wells
# batch_metadata <- merge(
#        batch_metadata,
#        plate_id_data[, c("plate_filename", "number_of_wells")],
#        by.x = "file_name", by.y = "plate_filename",
#        all.x = TRUE
# )
# names(batch_metadata)[names(batch_metadata) == "number_of_wells"] <- "n_wells"
#
# batch_metadata <<- batch_metadata
# # get the study accession of the batch and plate ids that will be uploaded
# batch_study_accession <- unique(batch_metadata$study_accession)
# plates_to_upload <- unique(batch_metadata$plate_id)
#
# plate_list_sql <- paste0("'", paste(plates_to_upload, collapse = "', '"), "'")
# query <- sprintf("
#   SELECT
#     xmap_header_id,
#     study_accession,
#     experiment_accession,
#     plate_id,
#     file_name,
#     acquisition_date,
#     reader_serial_number,
#     rp1_pmt_volts,
#     rp1_target,
#     auth0_user,
#     workspace_id,
#     plateid,
#     plate,
#     sample_dilution_factor,
#     n_wells
#   FROM madi_results.xmap_header
#   WHERE study_accession = '%s'
#     AND plate_id IN (%s);
# ", batch_study_accession, plate_list_sql)
#
# existing_plates <- DBI::dbGetQuery(conn, query)
# if (nrow(existing_plates) > 0) {
#   cat("These plates already exist for the study")
# } else {
#   cat("These plates do not exist for the study")
#
#
#
# plate_data_list2 <- plate_list
# # add experiment name and plate to each raw assay data
# for (i in seq_along(plate_data_list2)) {
#   plate_data_list2[[i]]$plate <- batch_metadata$plate[i]
#   plate_data_list2[[i]]$plate_id <- batch_metadata$plate_id[i]
#   plate_data_list2[[i]]$experiment_name <- batch_metadata$experiment_name[i]
# }
#
# # combine all plates together
# combined_plate_data <- dplyr::bind_rows(plate_data_list2)
#
#
# # obtain samples to upload
# sample_map <- plates_map[plates_map$specimen_type == "X",]
# if (nrow(sample_map) > 0) {
#   samples_to_upload <<- prepare_batch_bead_assay_samples(sample_plate_map = sample_map, combined_plate_data = combined_plate_data,
#                                                       antigen_list = layout_template_sheets()[["antigen_list"]],
#                                                       subject_map = subject_map)
# }
#
#
# ## Standards
# standard_map <- plates_map[plates_map$specimen_type == "S",]
# if (nrow(standard_map) > 0) {
#   standards_to_upload <<- prepare_batch_bead_assay_standards(standard_plate_map = standard_map, combined_plate_data = combined_plate_data,
#                                                           antigen_list = layout_template_sheets()[["antigen_list"]])
# }
# ## Blanks
# blanks_map <- plates_map[plates_map$specimen_type == "B",]
# if (nrow(blanks_map) > 0) {
#   blanks_to_upload <<- prepare_batch_bead_assay_blanks(blanks_plate_map = blanks_map, combined_plate_data = combined_plate_data,
#                                                     antigen_list = layout_template_sheets()[["antigen_list"]])
# }
# # controls upload
# controls_map <- plates_map[plates_map$specimen_type == "C",]
# if (nrow(controls_map) > 0) {
#   controls_to_upload <<- prepare_batch_bead_assay_controls(controls_plate_map = controls_map, combined_plate_data = combined_plate_data,
#                                                           antigen_list = layout_template_sheets()[["antigen_list"]])
# }
#
# ## Antigen family information
# antigen_family_table <<- prepare_batch_antigen_famly(antigen_list = layout_template_sheets()[["antigen_list"]])
# # existing <- dbGetQuery(conn, "SELECT study_accesssion, antigen FROM antigen_family_table")
# #
# # # Filter out rows already present
# # new_rows <<- dplyr::anti_join(antigen_family_table, existing,
# #                              by = c("study_accesssion", "antigen"))
# # for(i in seq_len(nrow(antigen_family_table))) {
# #   row <- antigen_family_table[i, ]
# #   sql <- "
# #     INSERT INTO antigen_family_table (
# #       study_accesssion,
# #       antigen,
# #       antigen_family,
# #       standard_curve_concentration,
# #       antigen_name,
# #       virus_bacterial_strain
# #     )
# #     SELECT ?, ?, ?, ?, ?, ?
# #     WHERE NOT EXISTS (
# #       SELECT 1 FROM antigen_family_table
# #       WHERE study_accesssion = ? AND antigen = ?
# #     );
# #   "
# #  query <-  glue::glue_sql(sql, .con = con,
# #                  row$study_accesssion, row$antigen, row$antigen_family,
# #                  row$standard_curve_concentration, row$antigen_name,
# #                  row$virus_bacterial_strain, row$study_accesssion, row$antigen
# #   )
# #
# # }
#
#
#
# }
# # plate_data_list2 <- plate_data_list
# # # names(plate_data_list2) <- batch_metadata$plate_id
# # names(plate_data_list2) <- head_list$plate_id
# #
# # plate_data_list2 <- imap(plate_data_list, ~ {
# #    plate <- head_list[[.y]]$plate[1]
# #    dplyr::mutate(.x, plate = plate)
# #  })

# sample_joined2 <- sample_joined
# for (i in names(plate_data_list2)) {
#   current_plate <- plate_data_list2[[i]]
#
#  sample_joined2 <- merge(sample_joined2, current_plate,
#                          by.x = c("plate_number", "well"),
#                          by.y = c("plate", "Well"),
#                          all.x = T)
# }

#df <-do.call(rbind, plate_data_list2)

#})










# output$batch_plate_table <- renderRHandsontable({
#   req(sample_dilution_plate_df())
#   rhandsontable(sample_dilution_plate_df(), readOnly = FALSE) %>%
#     hot_col("plate", readOnly = TRUE) %>%
#     hot_col("sample_dilution_factor", type = "numeric", format = "0") %>%
#     hot_cols(
#       renderer = "
#         function (instance, td, row, col, prop, value, cellProperties) {
#           Handsontable.renderers.TextRenderer.apply(this, arguments);
#
#           if (prop === 'sample_dilution_factor') {
#             var num = parseFloat(value);
#
#             if (isNaN(num) || value === null || value === '') {
#               td.style.background = '#f8d7da';   // light red for missing
#               td.style.color = 'black';
#               td.title = 'Please enter a value';
#             } else if (num < 1 || num > 100000) {
#               td.style.background = '#f5c6cb';   // pink for out of range
#               td.style.color = 'black';
#               td.title = 'Value must be between 1 and 100000';
#             } else {
#               td.style.background = '#d4edda';   // light green for valid
#               td.style.color = 'black';
#               td.title = '';
#             }
#           }
#         }
#       "
#     )
# })

# output$batch_plate_table <- renderRHandsontable({
#   req(sample_dilution_plate_df())
#   rhandsontable(sample_dilution_plate_df(), readOnly = FALSE) %>%
#     hot_col("plate_name", readOnly = TRUE) %>%
#     hot_col("sample_dilution_factor", type = "numeric") %>% # placeholder = "Enter a number 1–100000") %>%
#     hot_cols(
#       renderer = "
#             function (instance, td, row, col, prop, value, cellProperties) {
#               Handsontable.renderers.TextRenderer.apply(this, arguments);
#               if (sample_dilution_factor === null || sample_dilution_factor === '' || sample_dilution_factor === 'NA') {
#                 td.style.background = 'lightcoral';   // highlight empty cells
#                 td.innerHTML = 'please fill in';
#                 td.style.fontStyle = 'italic';
#               }
#             }"
#     )
# })
#
# observeEvent(input$batch_plate_table, {
#   df <- hot_to_r(input$batch_plate_table)
#   sample_dilution_plate_df(df)
# })


# ht <-  rhandsontable(df_long, rowHeaders = NULL) %>%
#   hot_col("variable", readOnly = TRUE) %>%  # Disable editing keys
#   hot_cols(
#     renderer = "
#             function (instance, td, row, col, prop, value, cellProperties) {
#               Handsontable.renderers.TextRenderer.apply(this, arguments);
#               if (value === null || value === '' || value === 'NA') {
#                 td.style.background = 'lightcoral';   // highlight empty cells
#                 td.innerHTML = 'please fill in';
#                 td.style.fontStyle = 'italic';
#               }
#             }"
#   )
#
# for (i in seq_len(nrow(df_long))) {
#   if (df_long$variable[i] %in% c("file_name", "study_accession", "experiment_accession", "plate_id", "auth0_user", "workspace_id")) {
#     ht <- hot_cell(ht, row = i, col = "value", readOnly = TRUE)
#   }
# }
# output$file_summary <- renderPrint({
#   files <<- batch_data_list()
#   if (length(files) == 0) {
#     cat("No files uploaded yet.")
#   } else {
#     cat("Uploaded files:\n")
#     print(names(files))
#     cat("\nEach is a data frame with these dimensions:\n")
#     print(sapply(files, dim))
#   }
# })

# observe({
#   print(input$upload_layout_file)
# })

# output$layout_sheets <- renderPrint({
#   cat("Loaded sheets:\n")
#
#   if (!is.null(layout_template_sheets$plate_id)) {
#     cat("\n--- plate_id ---\n")
#     print(dim(layout_template_sheets$plate_id))
#     print(head(layout_template_sheets$plate_id, 3))
#   }
#
#   if (!is.null(layout_template_sheets$plates_map)) {
#     cat("\n--- plates_map ---\n")
#     print(dim(layout_template_sheets$plates_map))
#     print(head(layout_template_sheets$plates_map, 3))
#   }
#
#   if (!is.null(layout_template_sheets$antigen_list)) {
#     cat("\n--- antigen_list ---\n")
#     print(dim(layout_template_sheets$antigen_list))
#     print(head(layout_template_sheets$antigen_list, 3))
#   }
# })



# observeEvent(all_sheets(), {
#   plate_id_sheet <- all_sheets()[["plate_id"]]
#   plates_map_sheet <- all_sheets()[["plates_map"]]
#   antigen_list_sheet <- all_sheets()[["antigen_list"]]
#
#   # # Example: print row counts
#   # cat("plate_id_sheet:", head(plate_id_sheet), "\n")
#   # cat("plates_map_sheet:", head(plates_map_sheet), "\n")
#
# })

