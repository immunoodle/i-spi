
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
  if (input$readxMap_study_accession != "Click here") {
    import_plate_data_title <- paste("Import", input$readxMap_study_accession, "Plate Data", sep = " ")
  } else {
    import_plate_data_title<- paste("No study selected for Importing Plate Data")
  }
  tagList(
    fluidPage(
          tagList(
            h3(import_plate_data_title),
            if (input$readxMap_study_accession != "Click here") {
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
            }
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
       # column(5,
#
#                selectizeInput("readxMap_study_accession_import",
#                               "Choose Existing Study Name OR Create a New Study Name (up to 15 characters)",
#                               choices <- c(c("Click OR Create New" = "Click here"),
#                                            setNames(unique(reactive_df_study_exp()$study_accession),
#                                                     unique(reactive_df_study_exp()$study_name)
#                                            )
#                               ),
#                               selected = "Click here",
#                               multiple = FALSE,
#                               options = list(create = TRUE), width = '500px'
#                )
#         ),
        column(5,
               conditionalPanel(
                 condition = "input.readxMap_study_accession != 'Click here'",
                 selectizeInput("readxMap_experiment_accession_import",
                                "Choose Existing Experiment Name OR Create a New Experiment Name (up to 15 characters)",
                                choices <- c("Click OR Create New" = "Click here"),
                                selected = "Click here",
                                multiple = FALSE,
                                options = list(create = TRUE), width = '700px'
                 )
               )
        )
      )
      ,
      fluidRow(
        column(9,
               conditionalPanel(
                 condition = "input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession_import != 'Click here' && input.readxMap_experiment_accession_import != ''",
                 # shinyWidgets::switchInput("xPonentFile",
                 #                           onLabel = "xPONENT",
                 #                           offLabel = "RAW",
                 #                           onStatus = "success",
                 #                           offStatus = "danger",
                 #                           value = FALSE,
                 #                           size = "large",
                 #                           label = "File Format",
                 #                           labelWidth = "150px",
                 #                           handleWidth = "100px",
                 #                           width = "auto"),
                 shinyWidgets::radioGroupButtons(
                   inputId = "xPonentFile",
                   label = "File Format",
                   choices = c("xPONENT", "RAW"),
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
})

observeEvent(input$readxMap_study_accession, {
  print(paste("readxMap_study_accession clicked:", input$readxMap_study_accession))

 # imported_h_study(input$readxMap_study_accession)

  # study_exp is a data frame with study and experiment names filtered by user/project workspace
  study_exp <- reactive_df_study_exp()
  # filter study_exp by current study selected in navigation
  filtered_exp <- study_exp[study_exp$study_accession == input$readxMap_study_accession, ]


  if (nrow(filtered_exp) > 0) {
    expvector <- setNames(filtered_exp$experiment_accession, filtered_exp$experiment_name)
  } else {
    expvector <- character(0)
  }

  experiment_drop <- c("Click OR Create New" = "Click here", expvector)


  updateSelectizeInput(
    session,
    inputId = "readxMap_experiment_accession_import",
    label = "Choose Existing Experiment Name OR Create a New Experiment Name (up to 15 characters)",
    choices = experiment_drop,
    selected = "Click here"
  )


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
observeEvent(input$upload_to_shiny,{

  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)
  availableSheets(NULL)
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
                    selected = ""),
        actionButton("view_raw_file", "View Raw File"),
        actionButton("view_raw_header", "View Raw Header")
      )
    }


  })


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
  plate_data(transform_dat)


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

  print(plate_data())
})

observeEvent(input$view_raw_header,{
  print("view raw header")

  showModal(
    modalDialog(
      title = "Raw Header",
      rhandsontable::rHandsontableOutput("raw_header_file"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$raw_header_file <- rhandsontable::renderRHandsontable({
    rhandsontable(header_info(), readOnly = TRUE)
  })

  print(header_info())
})

output$segment_selector <- renderUI({

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

  # Make sure to return the tabsetPanel with all tabs included
  do.call(tabsetPanel, tabs)

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
    SELECT
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
