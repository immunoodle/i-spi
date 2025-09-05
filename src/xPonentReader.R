
xPonent_fileinput_ui <- fluidPage(
  fluidRow(
    column(6,
           fileInput("uploaded_xPonent"
                     , label="Upload a plate/batch file (xPONENT format Only)"
                     # , accept=c(".xlsx",".xls", ",csv")
                     , multiple=FALSE)),
    column(6,
           fileInput("uploaded_xPonent_metadata",
                     label = "Upload meta data"),
           multiple = FALSE)

  ),
  # conditionalPanel(
  #   condition (input$uploaded_xPonent_metadata),
  actionButton("parse_xPonent_types", "Parse")
  # )

)

xPonent_fileparse <- fluidPage(
  tabsetPanel(
    id = "xPonent_tabset",
    tabPanel(
      title = "Raw File",
      rHandsontableOutput("xPonentOutput", width = "100%", height = 300)
    )
  )
)



# Exponent files
observeEvent(input$xPonentFile, {

  if(input$xPonentFile == 'xPONENT'){
    removeTab(inputId = "body_panel_id", target="headerxMap")

    output$xPonentReader_fileinput_ui <- renderUI({

      xPonent_fileinput_ui

    })

    output$xPonent_fileparse_ui <- renderUI({

      xPonent_fileparse

    })

  }


})

observeEvent(input$test_xponent, {

  test_test_xponent <<- generated_tabs()

})

observeEvent(input$uploaded_xPonent, {

  xPonent_raw_file <- input$uploaded_xPonent$datapath
  lumcsv <- moach::read_Xponent_csv(xPonent_raw_file)

  # Store file for extraction of plate_id later
  lumcsv_reactive(lumcsv)

  test_lumcsv <<- lumcsv

  # Cleaning the xponent file
  raw_xponent_exprs <- lumcsv$AssayData$Exprs
  raw_xponent_wells <- lumcsv$AssayData$Wells

  raw_xponent_exprs[ , c("Location","Sample","Analyte","Count","Net_MFI")] %>%
    full_join(raw_xponent_wells[ , c("Location","Sample")], by = c("Location", "Sample")) %>%
    # Clean names
    janitor::clean_names() %>%
    # Drop median
    # select(-median, -dilution_factor) %>%
    # Rename cols
    dplyr::rename(antigen = analyte) -> clean_xPonent

  # Storing in the reactiveVal
  xponent_plate_data(clean_xPonent)
  test_xponent_plate_data <<- xponent_plate_data()

  output$xPonentOutput <- renderRHandsontable({
    rhandsontable(xponent_plate_data())
  })


  showNotification("Please upload meta data", type = "message")

})

observeEvent(input$uploaded_xPonent_metadata, {

  # Read in the meta data
  meta_data_raw_file <- input$uploaded_xPonent_metadata$datapath
  meta_data <- read.xlsx(meta_data_raw_file, startRow = 3)

  meta_data %>%
    janitor::clean_names() %>%
    dplyr::rename(location = well_location) -> meta_data_clean

  xponent_meta_data(meta_data_clean)

  test_xponent_meta_data <<- xponent_meta_data()

})

xPonent_combined_data <- reactive({

  req(xponent_plate_data(), xponent_meta_data())

  xponent_plate_data() %>%
    left_join(xponent_meta_data())

})

observe({

  test_combined <<- xPonent_combined_data()

  output$xPonentOutput <- renderRHandsontable({
    rhandsontable(xPonent_combined_data())
  })


})

observeEvent(input$parse_xPonent_types, {
  print("parse xPonent types")
  raw_xPonent_file <- hot_to_r(input$xPonentOutput)

  # raw_xPonent_file <- process_samples_with_class(raw_xPonent_file)

  # Parse different types
  raw_xPonent_file <- raw_xPonent_file %>% mutate(type_check = class, stype = class)

  test_raw_xPonent_file <<- raw_xPonent_file

  xPonent_types <- unique(raw_xPonent_file$type_check)

  # Remove tabs
  for (type in generated_tabs()){
    removeTab(inputId = "xPonent_tabset",target = paste0("xponent_type_tab_",type))
  }

  # Set generated tabs to NULL
  generated_tabs(c())

  # Create subset of different types
  xPonent_subset <- list()
  for(type in xPonent_types){
    print(type)
    xPonent_subset[[type]] <- raw_xPonent_file[raw_xPonent_file$type_check==type,] %>% dplyr::select(-type_check, -class)
  }

  # Insert all tabs
  for(xponent_type in xPonent_types){
    local({
      my_xponent_type <- xponent_type
      generated_tabs(c(generated_tabs(), xponent_type))
      # Insert the tab
      insertTab(
        inputId = "xPonent_tabset",
        tabPanel(
          value = paste0("xponent_type_tab_", my_xponent_type),
          title = paste0("Type ", my_xponent_type),
          # Create the UI
          uiOutput(paste0("ui_xponent_type_", my_xponent_type))
        )
      )

      # Render the UI
      output[[paste0("ui_xponent_type_", my_xponent_type)]] <- renderUI({
        tagList(
          fluidRow(
            actionButton(paste0("upload_xponent_type_",my_xponent_type), "Upload",icon = icon("upload")),
            br(),
            rHandsontableOutput(paste0("xponent_type_", my_xponent_type), width = "100%", height = 300)
          )
        )

      })

      # Render the output
      output[[paste0("xponent_type_", my_xponent_type)]] <- renderRHandsontable({

        display_df <- xPonent_subset[[my_xponent_type]]

        if(my_xponent_type != "X"){

          display_df %>%
            dplyr::select(-c(timepoint, subject_id)) %>%
            dplyr::rename(source = group_name)-> display_df

        }

        rhandsontable(display_df)

      })
    })
  }


})

observeEvent(input$upload_xponent_type_X, {

  print("upload for xponent type X")
  xponent_type_x_table <<- hot_to_r(input$xponent_type_X)
  study_name_import <- as.character(input$readxMap_study_accession_import)
  experiment_name_import <- as.character(input$readxMap_experiment_accession_import)

  # Extract plate_id
  load_plate_id <- file.path("E:",
                             "batch",
                             paste0(gsub("[[:punct:][:blank:]]+", ".", lumcsv_reactive()$BatchHeader$BatchInfo$Batch),".csv"))

  # Fetch existing header info
  load_auth0_user <- current_user_nocompress()
  load_workspace_id <- userWorkSpaceID()
  user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
    filter(study_accession == study_name_import &
           experiment_accession == experiment_name_import &
           plate_id == load_plate_id &
           workspace_id == load_workspace_id)

  if(nrow(user_header_data) == 0){
    xponent_header_upload <- data.frame(study_accession = study_name_import,
                                        experiment_accession = experiment_name_import,
                                        plate_id = load_plate_id,
                                        file_name = load_plate_id,
                                        acquisition_date = lumcsv_reactive()$BatchHeader$BatchInfo$Date,
                                        reader_serial_number = lumcsv_reactive()$BatchHeader$BatchInfo$SN,
                                        rp1_pmt_volts = NA,
                                        rp1_target = NA,
                                        auth0_user = load_auth0_user,
                                        workspace_id = load_workspace_id
    )
    # Map column types to the database

    xponent_header_ready <<- map_type_db("Header", xponent_header_upload)
    # Append table

    DBI::dbAppendTable(conn, Id(schema = "madi_results",
                                table = "xmap_header"),
                       xponent_header_ready)
    showNotification("Uploaded header successfully", type = "message")
      reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

  }

  xponent_type_x_upload <- data.frame(study_accession = study_name_import,
                                      experiment_accession = experiment_name_import,
                                      plate_id = load_plate_id,
                                      timeperiod = xponent_type_x_table$timepoint,
                                      patientid = xponent_type_x_table$subject_id,
                                      well = xponent_type_x_table$location,
                                      stype = xponent_type_x_table$stype,
                                      sampleid = xponent_type_x_table$sample,
                                      id_imi = NA,
                                      agroup = xponent_type_x_table$group_name,
                                      dilution = xponent_type_x_table$dilution_factor,
                                      pctaggbeads = NA,
                                      samplingerrors = NA,
                                      antigen = xponent_type_x_table$antigen,
                                      antibody_mfi = xponent_type_x_table$net_mfi,
                                      antibody_n = xponent_type_x_table$count,
                                      antibody_name = NA,
                                      feature = xponent_type_x_table$feature,
                                      gate_class = NA,
                                      antibody_au = NA,
                                      antibody_au_se = NA,
                                      reference_dilution = NA,
                                      gate_class_dil = NA)


  tryCatch({

    # Map column types to the database
    xponent_type_x_ready <<- map_type_db("Sample", xponent_type_x_upload)

    # Append table
    DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_sample"), xponent_type_x_ready)
    showNotification("Uploaded samples successfully", type = "message")

  },
  error = function(e) {
    showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                     type = "error",
                     closeButton = TRUE,
                     duration = NULL)
  })

})


observeEvent(input$upload_xponent_type_S, {

  print("upload for xponent type S")
  xponent_type_s_table <<- hot_to_r(input$xponent_type_S)
  study_name_import <- as.character(input$readxMap_study_accession_import)
  experiment_name_import <- as.character(input$readxMap_experiment_accession_import)

  # Extract plate_id
  load_plate_id <- file.path("E:",
                             "batch",
                             paste0(gsub("[[:punct:][:blank:]]+", ".", lumcsv_reactive()$BatchHeader$BatchInfo$Batch),".csv"))

  # Fetch existing header info
  load_auth0_user <- current_user_nocompress()
  load_workspace_id <- userWorkSpaceID()
  user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
    filter(study_accession == study_name_import &
           experiment_accession == experiment_name_import &
           plate_id == load_plate_id &
           workspace_id == load_workspace_id)

  if(nrow(user_header_data) == 0){
    xponent_header_upload <- data.frame(study_accession = study_name_import,
                                        experiment_accession = experiment_name_import,
                                        plate_id = load_plate_id,
                                        file_name = load_plate_id,
                                        acquisition_date = lumcsv_reactive()$BatchHeader$BatchInfo$Date,
                                        reader_serial_number = lumcsv_reactive()$BatchHeader$BatchInfo$SN,
                                        rp1_pmt_volts = NA,
                                        rp1_target = NA,
                                        auth0_user = load_auth0_user,
                                        workspace_id = load_workspace_id
    )
    # Map column types to the database
    xponent_header_ready <<- map_type_db("Header", xponent_header_upload)
    # Append table
    DBI::dbAppendTable(conn, Id(schema = "madi_results",
                                table = "xmap_header"),
                       xponent_header_ready)
    showNotification("Uploaded header successfully", type = "message")
      reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

  }


  xponent_type_s_upload <- data.frame(study_accession = study_name_import,
                                      experiment_accession = experiment_name_import,
                                      plate_id = load_plate_id,
                                      well = xponent_type_s_table$location,
                                      stype = xponent_type_s_table$stype,
                                      sampleid = xponent_type_s_table$sample,
                                      source = xponent_type_s_table$source,
                                      dilution = xponent_type_s_table$dilution_factor,
                                      pctaggbeads = NA,
                                      samplingerrors = NA,
                                      antigen = xponent_type_s_table$antigen,
                                      antibody_mfi = xponent_type_s_table$net_mfi,
                                      antibody_n = xponent_type_s_table$count,
                                      antibody_name = NA,
                                      feature = xponent_type_s_table$feature)

  tryCatch({

    # Map column types to the database
    xponent_type_s_ready <<- map_type_db("Standard", xponent_type_s_upload)

    # Append table
    DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard"), xponent_type_s_ready)
    showNotification("Uploaded standards successfully", type = "message")

  },
  error = function(e) {
    showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                     type = "error",
                     closeButton = TRUE,
                     duration = NULL)
  })

})


observeEvent(input$upload_xponent_type_C, {

  print("upload for type C")
  xponent_type_c_table <<- hot_to_r(input$xponent_type_C)
  study_name_import <- as.character(input$readxMap_study_accession_import)
  experiment_name_import <- as.character(input$readxMap_experiment_accession_import)

  # Extract plate_id
  load_plate_id <- file.path("E:",
                             "batch",
                             paste0(gsub("[[:punct:][:blank:]]+", ".", lumcsv_reactive()$BatchHeader$BatchInfo$Batch),".csv"))

  # Fetch existing header info
  load_auth0_user <- current_user_nocompress()
  load_workspace_id <- userWorkSpaceID()
  user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
    filter(study_accession == study_name_import &
           experiment_accession == experiment_name_import &
           plate_id == load_plate_id &
           workspace_id == load_workspace_id)

  if(nrow(user_header_data) == 0){
    xponent_header_upload <- data.frame(study_accession = study_name_import,
                                        experiment_accession = experiment_name_import,
                                        plate_id = load_plate_id,
                                        file_name = load_plate_id,
                                        acquisition_date = lumcsv_reactive()$BatchHeader$BatchInfo$Date,
                                        reader_serial_number = lumcsv_reactive()$BatchHeader$BatchInfo$SN,
                                        rp1_pmt_volts = NA,
                                        rp1_target = NA,
                                        auth0_user = load_auth0_user,
                                        workspace_id = load_workspace_id
    )
    # Map column types to the database
    xponent_header_ready <<- map_type_db("Header", xponent_header_upload)
    # Append table
    DBI::dbAppendTable(conn, Id(schema = "madi_results",
                                table = "xmap_header"),
                       xponent_header_ready)
    showNotification("Uploaded header successfully", type = "message")
      reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

  }

  xponent_type_c_upload <<- data.frame(study_accession = study_name_import,
                                       experiment_accession = experiment_name_import,
                                       plate_id = load_plate_id,
                                       well = xponent_type_c_table$location,
                                       stype = xponent_type_c_table$stype,
                                       sampleid = xponent_type_c_table$sample,
                                       source = xponent_type_c_table$source,
                                       dilution = xponent_type_c_table$dilution_factor,
                                       pctaggbeads = NA,
                                       samplingerrors = NA,
                                       antigen = xponent_type_c_table$antigen,
                                       antibody_mfi = xponent_type_c_table$net_mfi,
                                       antibody_n = xponent_type_c_table$count,
                                       antibody_name = NA,
                                       feature = xponent_type_c_table$feature)

  tryCatch({
    # Map column types to the database
    xponent_type_c_ready <- map_type_db("Control", xponent_type_c_upload)

    # Append table
    DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_control"), xponent_type_c_ready)
    showNotification("Uploaded controls successfully", type = "message")

  },
  error = function(e) {
    showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                     type = "error",
                     closeButton = TRUE,
                     duration = NULL)
  })

})


observeEvent(input$upload_xponent_type_B, {
  print("upload for type B")
  xponent_type_b_table <<- hot_to_r(input$xponent_type_B)
  study_name_import <- as.character(input$readxMap_study_accession_import)
  experiment_name_import <- as.character(input$readxMap_experiment_accession_import)

  # Extract plate_id
  load_plate_id <- file.path("E:",
                             "batch",
                             paste0(gsub("[[:punct:][:blank:]]+", ".", lumcsv_reactive()$BatchHeader$BatchInfo$Batch),".csv"))

  # Fetch existing header info
  load_auth0_user <- current_user_nocompress()
  load_workspace_id <- userWorkSpaceID()
  user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
    filter(study_accession == study_name_import &
           experiment_accession == experiment_name_import &
           plate_id == load_plate_id &
           workspace_id == load_workspace_id)

  if(nrow(user_header_data) == 0){
    xponent_header_upload <- data.frame(study_accession = study_name_import,
                                        experiment_accession = experiment_name_import,
                                        plate_id = load_plate_id,
                                        file_name = load_plate_id,
                                        acquisition_date = lumcsv_reactive()$BatchHeader$BatchInfo$Date,
                                        reader_serial_number = lumcsv_reactive()$BatchHeader$BatchInfo$SN,
                                        rp1_pmt_volts = NA,
                                        rp1_target = NA,
                                        auth0_user = load_auth0_user,
                                        workspace_id = load_workspace_id
    )
    # Map column types to the database
    xponent_header_ready <<- map_type_db("Header", xponent_header_upload)
    # Append table
    DBI::dbAppendTable(conn, Id(schema = "madi_results",
                                table = "xmap_header"),
                       xponent_header_ready)
    showNotification("Uploaded header successfully", type = "message")
      reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

  }

  xponent_type_b_upload <- data.frame(study_accession = study_name_import,
                                      experiment_accession = experiment_name_import,
                                      plate_id = load_plate_id,
                                      well = xponent_type_b_table$location,
                                      stype = xponent_type_b_table$stype,
                                      pctaggbeads = NA,
                                      samplingerrors = NA,
                                      antigen = xponent_type_b_table$antigen,
                                      antibody_mfi = xponent_type_b_table$net_mfi,
                                      antibody_n = xponent_type_b_table$count,
                                      antibody_name = NA,
                                      dilution = xponent_type_b_table$dilution_factor,
                                      feature = xponent_type_b_table$feature
  )

  tryCatch({
    # Map column types to the database
    xponent_type_b_ready <<- map_type_db("Buffer", xponent_type_b_upload)

    # Append table
    DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_buffer"), xponent_type_b_ready)
    showNotification("Uploaded buffers successfully", type = "message")

  },
  error = function(e) {
    showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                     type = "error",
                     closeButton = TRUE,
                     duration = NULL)
  })

})


