
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
                     downloadButton("blank_layout_file", "Download a blank layout template"),
                     fileInput("upload_layout_file"
                               , label="Upload a layout file (only accepts xlsx, xls)"
                               , accept=c(".xlsx",".xls")
                               , multiple=FALSE),
                     conditionalPanel(
                       condition = "input.upload_layout_file != ''",
                       uiOutput("view_layout_file_ui")
                       # actionButton("view_layout_plate_id_sheet", "View Layout Plate ID"),
                       # actionButton("view_layout_plates_map_sheet", "View Layout Plate Map"),
                       # actionButton("view_layout_antigen_list_sheet", "View Layout Antigen List")
                     ),
                     fileInput("upload_batch_bead_array"
                                , label="Upload one or more plate/batch files (only accepts xlsx, xls)"
                                , accept=c(".xlsx",".xls")
                                , multiple=TRUE),
                     uiOutput("file_summary"),
                     uiOutput("plate_tabs")

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

output$blank_layout_file <- downloadHandler(
  filename = function() {
    paste0("layout_template.xlsx")
  },
  content = function(file) {
    file.copy("www/layout_template.xlsx", file)
  }
)

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
  all_sheets <- lapply(sheets, function(sheet_name) {
    skip_rows <- if (sheet_name == "timepoint")  3 else 2
    readxl::read_excel(inLayoutFile()$datapath, sheet = sheet_name, skip = skip_rows)
  })
  names(all_sheets) <- sheets  # Assign sheet names

  # Now assign to reactive Values object
  layout_template_sheets(all_sheets)

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

observeEvent(input$upload_batch_bead_array, {
  req(input$upload_batch_bead_array)
  uploaded_files <- input$upload_batch_bead_array

  # Use lapply to process all uploaded files
  all_data <- lapply(uploaded_files$datapath, function(path) {
    # Read header (first 7 rows)
    header_info <- openxlsx::read.xlsx(path, rows = 1:7, colNames = FALSE, sheet = 1)
    header_info <- parse_metadata_df(header_info)
    # Read plate data (from row 8 onward)
    plate_data <- openxlsx::read.xlsx(path, startRow = 8, sheet = 1, colNames = TRUE)

    # Remove completely empty rows
    plate_data <- plate_data[!apply(
      plate_data, 1,
      function(x) all(is.na(x) | trimws(x) == "" | trimws(x) == "NA")
    ), ]

    list(header = header_info, plate = plate_data)
  })

  # Assign names using uploaded filenames
  names(all_data) <- uploaded_files$name

  # Separate into two named lists
  header_list <- lapply(all_data, `[[`, "header")
  plate_list  <- lapply(all_data, `[[`, "plate")

  # Store in reactiveVals
  bead_array_header_list(header_list)
  bead_array_plate_list(plate_list)

  plate_names <- sapply(header_list, function(x) x$plate)
  sample_dilution_plate_df(
    data.frame(
      plate = plate_names,
      sample_dilution_factor = NA_real_,
      stringsAsFactors = FALSE
    )
  )

  batch_header <<- construct_batch_upload_metadata(plates_map = layout_template_sheets()[["plates_map"]],
                                                  plate_metadata_list = bead_array_header_list(),
                                                  workspace_id = userWorkSpaceID(),
                                                  currentuser = currentuser())


  # sample_dilution_plate_df(
  #   data.frame(
  #     plate_name = names(bead_array_plate_list()),
  #     sample_dilution_factor = NA_real_,
  #     stringsAsFactors = FALSE
  #   ))
})
# Display basic info about uploaded files
output$file_summary <- renderPrint({
 # data_list <- batch_data_list()
  head_list <<- bead_array_header_list()
  plate_list <<- bead_array_plate_list()
  plates_map <<- layout_template_sheets()[["plates_map"]]
  antigen_list <<- layout_template_sheets()[["antigen_list"]]
  plate_id_data <<- layout_template_sheets()[["plate_id"]]
  timepoint_map <<- layout_template_sheets()[["timepoint"]]
  subject_map <<- layout_template_sheets()[["subject_groups"]]

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


# Dynamically generate tabs for each plot
output$plate_tabs <- renderUI({
  req(plate_layout_plots())
  plots <- plate_layout_plots()

  tabs <- lapply(names(plots), function(name) {
    tabPanel(
      title = name,
      plotOutput(paste0("plot_", name))
    )
  })

  do.call(tabsetPanel, tabs)
})


#  Render each plot
observe({
  req(plate_layout_plots())
  plots <- plate_layout_plots()

  for (name in names(plots)) {
    local({
      plot_name <- name
      output[[paste0("plot_", plot_name)]] <- renderPlot({
        plots[[plot_name]]
      })
    })
  }
})

# Join sample dilutions for the plate to the header
construct_batch_upload_metadata <- function(plates_map, plate_metadata_list, currentuser, workspace_id) {
  sample_dilutions_by_plate <- unique(
    plates_map[plates_map$specimen_type == "X",
               c("plate_number", "specimen_type", "specimen_dilution_factor")])
    names(sample_dilutions_by_plate)[names(sample_dilutions_by_plate) == "specimen_dilution_factor"] <- "sample_dilution_factor"

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



    return(plate_metadata_list_updated)
}

plot_plate_layout <- function(plates_map, plate_id_data) {
  # Join in number_of_wells info
  plates_map_joined <- merge(
    plates_map,
    plate_id_data[, c("study_name", "experiment_name", "plate_number", "number_of_wells")],
    by = c("study_name", "experiment_name", "plate_number"),
    all.x = TRUE
  )

  # Initialize list for storing plots
  plate_plots <- list()

  # Get unique combinations of study, experiment, and plate
  unique_combos <- unique(
    plates_map_joined[, c("study_name", "experiment_name", "plate_number")]
  )

  # Loop through each combination
  for (i in seq_len(nrow(unique_combos))) {
    study <- unique_combos$study_name[i]
    exp <- unique_combos$experiment_name[i]
    plate <- unique_combos$plate_number[i]

    # Filter data for this combination
    plates_map_joined_filtered <- plates_map_joined[plates_map_joined$study_name == study & plates_map_joined$experiment_name == exp
                                                    & plates_map_joined$plate_number == plate,]

    n_wells <- unique(plates_map_joined_filtered$number_of_wells)

    # Build title
    plot_title <- paste0(
      "Study: ", study,
      " | Experiment: ", exp,
      " | Plate: ", plate,
      " (", n_wells, " wells)"
    )


    plate_layout <- plate_plot(
      data = plates_map_joined_filtered,
      position = well,
      value = specimen_type,
      plate_size = n_wells,
      plate_type = "round",
      title = plot_title,
      silent = FALSE
    )



    # Add plot to list
    list_name <- paste(study, exp, plate, sep = "_")
    plate_plots[[list_name]] <- plate_layout
  }

  return(plate_plots)
}


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

