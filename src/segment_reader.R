list_of_dataframes <- reactive({

  req(input$uploaded_sheet)

  plate_data() %>%
    janitor::clean_names() %>%
    mutate(description = gsub("[^A-Za-z0-9.]+", "_", description) %>% trimws(whitespace = "_")) %>%
    mutate(type = str_remove_all(type, "[0-9]")) -> type_df

  all_dataframes <- list()

  for(well_type in unique(type_df$type)){

    xprofile <- update_db(operation = "select",
                          schema = "madi_results",
                          table_name = "xmap_profile",
                          select_where = list("concat(study_accession,experiment_accession,stype)" = paste0(input$readxMap_study_accession_import,input$readxMap_experiment_accession_import,well_type))
    )
    if (nrow(xprofile) > 0){
      xprofile <- xprofile[order("study_accession", "experiment_accession", "stype", "-xmap_profile_id"), ]
      xprofile <- distinct(xprofile, study_accession, experiment_accession, stype , .keep_all = TRUE)
      feature <- xprofile$source_stor
    } else {
      feature <- ""
      }

    print(well_type)

    type_df %>%
      filter(type == well_type) -> type_filter

    type_filter %>%
      mutate(split_length = str_count(description, "_")) -> type_length

    if(all(is.na(unique(type_length$split_length)))){

      type_length %>%
        mutate(source = "",
               dilution = 1,
               feature = feature) %>%
        select(source,
               dilution,
               feature) -> all_dataframes[[well_type]]

      next
    }

    n_splits <- max(type_length$split_length, na.rm = T) + 1

    type_filter %>%
      select(description) %>%
      separate(description,
               into = paste0("segment_", 1:n_splits),
               sep = "_"
      ) -> sub_dataframes

    sub_dataframes$feature <- feature

    all_dataframes[[well_type]] <- sub_dataframes
  }

  return(all_dataframes)

})

original_df_combined <- reactive({

  req(list_of_dataframes())
  req(plate_data())

  full_df <- list()

  for(type_filter in unique(names(list_of_dataframes()))){

    plate_data() %>%
      janitor::clean_names() %>%
      select(type, description) %>%
      mutate(type_check = str_remove_all(type, "[0-9]")) %>%
      filter(type_check == type_filter) %>%
      select(-type_check) -> orig_data

    orig_data %>%
      cbind(list_of_dataframes()[[type_filter]]) -> full_df[[type_filter]]

  }

  return(full_df)

})

create_ui_for_type <- function(data_type, study_accession = NULL, experiment_accession = NULL) {

  df <- original_df_combined()[[data_type]]

  col_names <- names(df)
  print(paste(" select where str:",paste0(study_accession,experiment_accession,data_type)))
  xprofile <<- update_db(operation = "select",
                        schema = "madi_results",
                        table_name = "xmap_profile",
                        select_where = list("concat(study_accession,experiment_accession,stype)" = paste0(study_accession,experiment_accession,data_type))
                        )
  if (nrow(xprofile) > 0){
    xprofile <- xprofile[order("study_accession", "experiment_accession", "stype", "-xmap_profile_id"), ]
    xprofile <- distinct(xprofile, study_accession, experiment_accession, stype , .keep_all = TRUE)
    }

  if(nrow(xprofile) > 0 & data_type == 'X'){
      selected_col <- c(xprofile$dilution_stor, xprofile$group_stor, xprofile$timeperiod_stor, xprofile$patientid_stor)
    } else if (nrow(xprofile) > 0) {
      selected_col <- c(xprofile$dilution_stor, xprofile$group_stor)
    } else {
      selected_col <- col_names[1]
    }

    if (nrow(xprofile) > 0) {
      group_exp_val <- strsplit(selected_col[2],",",fixed = TRUE)[[1]]
    } else {
      group_exp_val <- selected_col[1]
    }

    if (nrow(xprofile) > 0 & data_type == 'X') {
      time_exp_val <- strsplit(selected_col[3],",",fixed = TRUE)[[1]]
    } else {
      time_exp_val <- selected_col[1]
    }

  if(data_type == "X"){
    p1 <- p("Select segments corresponding to Dilution, Group name, Timepoint and Patient ID")
  }else{
    p1 <- p("Select segments corresponding to Dilution and Source")
  }

  table_ui <- rHandsontableOutput(paste0("table_", data_type))

  select_ui_dilution <- selectInput(paste0("select_dilution_", data_type),
                                    label = "Dilution",
                                    choices = col_names,
                                    selected = selected_col[1])

  if(data_type == "X"){
    group_label = "Group Name"
  }else{
    group_label = "Source"
  }

  select_ui_group_name <- pickerInput(
    inputId = paste0("select_group_", data_type),
    label = group_label,
    choices = list(colnames = col_names),
    selected = group_exp_val,
    multiple = TRUE
  )

  select_ui_timepoint <- pickerInput(
    inputId = paste0("select_timepoint_", data_type),
    label = "Timepoint",
    choices = list(colnames = col_names),
    selected = time_exp_val,
    multiple = TRUE
  )

  select_ui_patientID <- selectInput(paste0("select_patientID_", data_type),
                                     label = "Patient ID",
                                     choices = col_names,
                                     selected = ifelse(nrow(xprofile) > 0,selected_col[4],selected_col[1])
                                     )

  select_ui3 <- verbatimTextOutput(paste0("print_name_", data_type))

  if(data_type == "X"){

    select_row_ui <- fluidRow(
      column(3, select_ui_dilution),
      column(3,
             fluidRow(select_ui_group_name,
                      select_ui3)
      ),
      column(3,
             select_ui_timepoint),
      column(3,
             select_ui_patientID)
    )
  }else{

    select_row_ui <- fluidRow(
      column(4, select_ui_dilution),
      column(4,
             fluidRow(select_ui_group_name,
                      select_ui3)
      )
    )

  }

  button_ui <- actionButton(paste0("assign_value_", data_type), "Assign")

  submit_ui <- actionButton(paste0("upload_type_", data_type), "Upload", icon = icon("upload"))

  ui_outputs <- tagList(br(),
                        p1,
                        select_row_ui,
                        button_ui,
                        submit_ui,
                        br(),
                        table_ui
                        )

  do.call(tagList, ui_outputs)
}

observe({

  if(!is.null(unique_plate_types())){

    type_vector <- unique_plate_types()

    create_ui_output <- function(type) {
      output_name <- paste0("ui_", type)
      output[[output_name]] <- renderUI({
        create_ui_for_type(type, study_accession = input$readxMap_study_accession_import, experiment_accession = input$readxMap_experiment_accession_import)
      })
    }

    for (type in type_vector) {
      create_ui_output(type)
    }

  }else{
    "hi"
  }
})

output[["table_X1"]] <- renderTable({
  original_df_combined()[["B"]] %>% head()
})

observe({
  types <- unique_plate_types()
  for (type in types) {
    local({
      current_type <- type

      output[[paste0("table_", current_type)]] <- renderRHandsontable({
        original_df_combined()[[current_type]] %>%
          rhandsontable(useTypes = FALSE,
                        overflow = "visible",
                        horizontal_scroll = TRUE,
                        vertical_scroll = TRUE, width = 1024, height = 300) %>%
          hot_col(col = "feature", type = "dropdown", source = c("ADCD", "Total_IgG", "IgG1", "IgG2", "IgG3", "IgG4", "IgA", "IgA1", "IgM", "IgM1", "FcgR2a", "FcgR2b", "FcgR3a", "FcgR3b"))
      })
    })
  }
})

color_renderer <- "
  function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.color = 'green';
  }
"

observeEvent(input$assign_value_X, {

  current_df <- hot_to_r(input$table_X)

  current_df$dilution <- current_df[[input$select_dilution_X]]

  current_df %>%
    mutate(group_name = do.call(paste, c(select(., all_of(input$select_group_X)), sep = "_"))) -> current_df

  current_df %>%
    mutate(timepoint = do.call(paste, c(select(., all_of(input$select_timepoint_X)), sep = "_"))) -> current_df

  current_df$patient_id <- current_df[[input$select_patientID_X]]

  output$table_X <- renderRHandsontable({
    rhandsontable(current_df,
                  overflow = "visible",
                  horizontal_scroll = TRUE,
                  vertical_scroll = TRUE, width = 1024, height = 300) %>%
      hot_validate_numeric(cols = "dilution", min = 1) %>%
      hot_col(c("dilution", "group_name", "timepoint", "patient_id"), renderer = color_renderer) %>%
      hot_col(col = "feature", type = "dropdown", source = c("ADCD", "Total_IgG", "IgG1", "IgG2", "IgG3", "IgG4", "IgA", "IgA1", "IgM", "IgM1", "FcgR2a", "FcgR2b", "FcgR3a", "FcgR3b"))
  })

})

type_vector_observes <- c("S", "C","B")

observeEvents <- function(type) {
  observeEvent(input[[paste0("assign_value_", type)]], {

    table_input_id <- paste0("table_", type)
    select_dilution_id <- paste0("select_dilution_", type)
    select_group_id <- paste0("select_group_", type)

    current_df <- hot_to_r(input[[table_input_id]])

    current_df$dilution <- current_df[[input[[select_dilution_id]]]]

    current_df <- current_df %>%
      mutate(source = do.call(paste, c(select(., all_of(input[[select_group_id]])), sep = "_")))

    highlighted_cols <- c("dilution", "source")

    output[[table_input_id]] <- renderRHandsontable({
      rhandsontable(current_df,
                    overflow = "visible",
                    horizontal_scroll = TRUE,
                    vertical_scroll = TRUE, width = 1024, height = 300) %>%
        hot_validate_numeric(cols = "dilution", min = 1) %>%
        hot_col(highlighted_cols, renderer = color_renderer) %>%
        hot_col(col = "feature", type = "dropdown", source = c("ADCD", "Total_IgG", "IgG1", "IgG2", "IgG3", "IgG4", "IgA", "IgA1", "IgM", "IgM1", "FcgR2a", "FcgR2b", "FcgR3a", "FcgR3b"))
    })

  })
}

lapply(type_vector_observes, observeEvents)

observeEvent(input$upload_type_X, {

  auth0_username <- session$userData$auth0_info$nickname

  print("upload for X")
  print(input$readxMap_study_accession_import)
  print(input$readxMap_experiment_accession_import)

  study_name_import <- as.character(input$readxMap_study_accession_import)
  experiment_name_import <- as.character(input$readxMap_experiment_accession_import)

  if(nchar(study_name_import) > 15){
    showNotification("Study Name should be less than 15 characters", type = "error")
    return(NULL)
  }

  if(nchar(experiment_name_import) > 15){
    showNotification("Experiment Name should be less than 15 characters", type = "error")
    return(NULL)
  }

  x_type_final_table <- hot_to_r(input$table_X)

  full_data <- readxl::read_excel(inFile()$datapath, col_names = FALSE, skip = 0, sheet = input$uploaded_sheet)

  lines_to_skip <- which(grepl("well",readxl::read_excel(inFile()$datapath, col_names = TRUE, sheet = input$uploaded_sheet)[,1][[1]], ignore.case = TRUE)) - 1

  table_data <- as.data.frame(readxl::read_excel(inFile()$datapath, sheet = input$uploaded_sheet ,col_names = TRUE, skip = lines_to_skip))

  full_data <- data.frame(lapply(full_data, function(x) {  gsub("[,]", ".", x) }))
  full_data <- data.frame(lapply(full_data, function(x) {  gsub("[*]+", "", x) }))
  table_data <- data.frame(lapply(table_data, function(x) {  gsub("[,]", ".", x) }))
  table_data <- data.frame(lapply(table_data, function(x) {  gsub("[*]+", "", x) }))

  type_x_ready <- combine_plate_data(full_data,
                                      table_data,
                                      type = "X",
                                      new_col_df = x_type_final_table,
                                      study_accession = study_name_import,
                                      exp_accession = experiment_name_import)
  feature_to_stor <- type_x_ready$feature[1]

  if(is.null(type_x_ready)){
    showNotification("Error in assigned values", type = "error")
    return(NULL)
  }

  tryCatch({

      type_x_ready <- map_type_db("Sample", type_x_ready)
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_sample"), type_x_ready)

      profile_data <- data.frame(study_accession=study_name_import, experiment_accession=experiment_name_import,
                                 stype='X',
                                 dilution_stor = input$select_dilution_X,
                                 group_stor = paste0(input$select_group_X, collapse = ","),
                                 timeperiod_stor = paste0(input$select_timepoint_X, collapse = ","),
                                 patientid_stor = input$select_patientID_X,
                                 source_stor = feature_to_stor
                                 )
      select_profile_row <- paste0(study_name_import,experiment_name_import,"X")
      delete_str <- glue::glue_sql("DELETE FROM madi_results.xmap_profile
      WHERE concat(study_accession,experiment_accession,stype) = {select_profile_row}", .con = conn)
      DBI::dbExecute(conn, delete_str )
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_profile"), profile_data)

      header_data <- generate_header(inFile()$datapath, input$readxMap_study_accession_import, input$readxMap_experiment_accession_import, auth0_username)

      header_data <- data.frame(lapply(header_data, function(x) {  gsub("[,]", ".", x) }))
      header_data <- data.frame(lapply(header_data, function(x) {  gsub("[*]+", "", x) }))

      header_data$workspace_id <- userWorkSpaceID()
      user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
        filter(study_accession == header_data$study_accession & experiment_accession == header_data$experiment_accession & plate_id == header_data$plate_id  & workspace_id == header_data$workspace_id)

      if(nrow(user_header_data) == 0){
        header_data$auth0_user <- current_user_nocompress()
        DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), header_data)
      }

      showNotification("Uploaded successfully", type = "message")

       reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
       print("reactive_df_study_exp:loaded")
})
})


observeEvent(input$upload_type_S, {

  auth0_username <- session$userData$auth0_info$nickname
  print("upload for S")
  print(input$readxMap_study_accession_import)
  print("experiment_accession is")
  print(input$readxMap_experiment_accession_import)

  study_name_import <- as.character(input$readxMap_study_accession_import)
  experiment_name_import <- as.character(input$readxMap_experiment_accession_import)

  if(nchar(study_name_import) > 15){
    showNotification("Study Name should be less than 15 characters", type = "error")
    return(NULL)
  }

  if(nchar(experiment_name_import) > 15){
    showNotification("Experiment Name should be less than 15 characters", type = "error")
    return(NULL)
  }

  s_type_final_table <- hot_to_r(input$table_S)

  full_data <- readxl::read_excel(inFile()$datapath, col_names = FALSE, skip = 0, sheet = input$uploaded_sheet)

  lines_to_skip <- which(grepl("well",readxl::read_excel(inFile()$datapath, col_names = TRUE, sheet = input$uploaded_sheet)[,1][[1]], ignore.case = TRUE)) - 1

  table_data <- as.data.frame(readxl::read_excel(inFile()$datapath, sheet = input$uploaded_sheet ,col_names = TRUE, skip = lines_to_skip))

  full_data <- data.frame(lapply(full_data, function(x) {  gsub("[,]", ".", x) }))
  full_data <- data.frame(lapply(full_data, function(x) {  gsub("[*]+", "", x) }))
  table_data <- data.frame(lapply(table_data, function(x) {  gsub("[,]", ".", x) }))
  table_data <- data.frame(lapply(table_data, function(x) {  gsub("[*]+", "", x) }))

  type_s_ready <- combine_plate_data(full_data,
                                     table_data,
                                     type = "S",
                                     new_col_df = s_type_final_table,
                                     study_accession = study_name_import,
                                     exp_accession = experiment_name_import)
  feature_to_stor <- type_s_ready$feature[1]
  if(is.null(type_s_ready)){
    showNotification("Error in assigned values", type = "error")
    return(NULL)
  }

  tryCatch(
    {
      type_s_ready <- map_type_db("Standard", type_s_ready)
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard"), type_s_ready)

      profile_data <- data.frame(study_accession=study_name_import,
                                 experiment_accession=experiment_name_import,
                                 stype='S',
                                 dilution_stor = input$select_dilution_S,
                                 group_stor = paste0(input$select_group_S, collapse = ","),
                                 source_stor = feature_to_stor
                                 )
      select_profile_row <- paste0(study_name_import,experiment_name_import,"S")
      delete_str <- glue::glue_sql("DELETE FROM madi_results.xmap_profile
      WHERE concat(study_accession,experiment_accession,stype) = {select_profile_row}", .con = conn)
      DBI::dbExecute(conn, delete_str )
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_profile"), profile_data)

      header_data <- generate_header(inFile()$datapath,
                                     input$readxMap_study_accession_import,
                                     input$readxMap_experiment_accession_import, auth0_username)

      header_data <- data.frame(lapply(header_data, function(x) {  gsub("[,]", ".", x) }))
      header_data <- data.frame(lapply(header_data, function(x) {  gsub("[*]+", "", x) }))

      header_data$workspace_id <- userWorkSpaceID()
      user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
        filter(study_accession == header_data$study_accession &
               experiment_accession == header_data$experiment_accession &
               plate_id == header_data$plate_id &
               workspace_id == header_data$workspace_id)

      if(nrow(user_header_data) == 0){
        header_data$auth0_user <- current_user_nocompress()
        DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), header_data)
      }

      showNotification("Uploaded successfully", type = "message")
    },
    error = function(e) {
      showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                       type = "error" ,
                       closeButton = TRUE,
                       duration = NULL)
    }
  )
      reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

})


observeEvent(input$upload_type_C, {

  auth0_username <- session$userData$auth0_info$nickname
  print("upload for C")

  print(input$readxMap_study_accession_import)
  print(input$readxMap_experiment_accession_import)

  study_name_import <- as.character(input$readxMap_study_accession_import)
  experiment_name_import <- as.character(input$readxMap_experiment_accession_import)

  if(nchar(study_name_import) > 15){
    showNotification("Study Name should be less than 15 characters", type = "error")
    return(NULL)
  }

  if(nchar(experiment_name_import) > 15){
    showNotification("Experiment Name should be less than 15 characters", type = "error")
    return(NULL)
  }

  c_type_final_table <- hot_to_r(input$table_C)

  full_data <- readxl::read_excel(inFile()$datapath, col_names = FALSE, skip = 0, sheet = input$uploaded_sheet)

  lines_to_skip <- which(grepl("well", readxl::read_excel(inFile()$datapath, col_names = TRUE, sheet = input$uploaded_sheet)[,1][[1]], ignore.case = TRUE)) - 1

  table_data <- as.data.frame(readxl::read_excel(inFile()$datapath, sheet = input$uploaded_sheet, col_names = TRUE, skip = lines_to_skip))

  full_data <- data.frame(lapply(full_data, function(x) { gsub("[,]", ".", x) }))
  full_data <- data.frame(lapply(full_data, function(x) { gsub("[*]+", "", x) }))
  table_data <- data.frame(lapply(table_data, function(x) { gsub("[,]", ".", x) }))
  table_data <- data.frame(lapply(table_data, function(x) { gsub("[*]+", "", x) }))

  type_c_ready <- combine_plate_data(full_data,
                                     table_data,
                                     type = "C",
                                     new_col_df = c_type_final_table,
                                     study_accession = study_name_import,
                                     exp_accession = experiment_name_import)
  feature_to_stor <- type_c_ready$feature[1]
  if(is.null(type_c_ready)){
    showNotification("Error in assigned values", type = "error")
    return(NULL)
  }

  tryCatch(
    {
      type_c_ready <- map_type_db("Control", type_c_ready)
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_control"), type_c_ready)

      profile_data <- data.frame(study_accession = study_name_import,
                                 experiment_accession = experiment_name_import,
                                 stype = 'C',
                                 dilution_stor = input$select_dilution_C,
                                 group_stor = paste0(input$select_group_C, collapse = ","),
                                 source_stor = feature_to_stor)
      select_profile_row <- paste0(study_name_import, experiment_name_import, "C")
      delete_str <- glue::glue_sql("DELETE FROM madi_results.xmap_profile
      WHERE concat(study_accession, experiment_accession, stype) = {select_profile_row}", .con = conn)
      DBI::dbExecute(conn, delete_str)
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_profile"), profile_data)

      header_data <- generate_header(inFile()$datapath, input$readxMap_study_accession_import, input$readxMap_experiment_accession_import, auth0_username)

      header_data <- data.frame(lapply(header_data, function(x) { gsub("[,]", ".", x) }))
      header_data <- data.frame(lapply(header_data, function(x) { gsub("[*]+", "", x) }))

      header_data$workspace_id <- userWorkSpaceID()
      user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
        filter(study_accession == header_data$study_accession &
               experiment_accession == header_data$experiment_accession &
               plate_id == header_data$plate_id &
               workspace_id == header_data$workspace_id)

      if(nrow(user_header_data) == 0){
        header_data$auth0_user <- current_user_nocompress()
        DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), header_data)
      }

      showNotification("Uploaded successfully", type = "message")
    },
    error = function(e) {
      showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                       type = "error",
                       closeButton = TRUE,
                       duration = NULL)
    }
  )
      reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

})


observeEvent(input$upload_type_B, {

  auth0_username <- session$userData$auth0_info$nickname
  print("upload for B")
  print(input$readxMap_study_accession_import)
  print(input$readxMap_experiment_accession_import)

  study_name_import <- as.character(input$readxMap_study_accession_import)
  experiment_name_import <- as.character(input$readxMap_experiment_accession_import)

  if(nchar(study_name_import) > 15){
    showNotification("Study Name should be less than 15 characters", type = "error")
    return(NULL)
  }

  if(nchar(experiment_name_import) > 15){
    showNotification("Experiment Name should be less than 15 characters", type = "error")
    return(NULL)
  }

  b_type_final_table <- hot_to_r(input$table_B)

  full_data <- readxl::read_excel(inFile()$datapath, col_names = FALSE, skip = 0, sheet = input$uploaded_sheet)

  lines_to_skip <- which(grepl("well", readxl::read_excel(inFile()$datapath, col_names = TRUE, sheet = input$uploaded_sheet)[,1][[1]], ignore.case = TRUE)) - 1

  table_data <- as.data.frame(readxl::read_excel(inFile()$datapath, sheet = input$uploaded_sheet, col_names = TRUE, skip = lines_to_skip))

  full_data <- data.frame(lapply(full_data, function(x) { gsub("[,]", ".", x) }))
  full_data <- data.frame(lapply(full_data, function(x) { gsub("[*]+", "", x) }))
  table_data <- data.frame(lapply(table_data, function(x) { gsub("[,]", ".", x) }))
  table_data <- data.frame(lapply(table_data, function(x) { gsub("[*]+", "", x) }))

  type_b_ready <- combine_plate_data(full_data,
                                     table_data,
                                     type = "B",
                                     new_col_df = b_type_final_table,
                                     study_accession = input$readxMap_study_accession_import,
                                     exp_accession = input$readxMap_experiment_accession_import)
  feature_to_stor <- type_b_ready$feature[1]
  if(is.null(type_b_ready)){
    showNotification("Error in assigned values", type = "error")
    return(NULL)
  }

  tryCatch(
    {
      type_b_ready <- map_type_db("Buffer", type_b_ready)
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_buffer"), type_b_ready)

      group_t_stor <- input$select_group_B
      group_stor <- paste0(group_t_stor, collapse = ",")
      profile_data <- data.frame(study_accession=study_name_import,
                                 experiment_accession=experiment_name_import,
                                 stype='B',
                                 dilution_stor = input$select_dilution_B,
                                 group_stor = group_stor,
                                 source_stor = feature_to_stor)
      select_profile_row <- paste0(study_name_import, experiment_name_import, "B")
      delete_str <- glue::glue_sql("DELETE FROM madi_results.xmap_profile
      WHERE concat(study_accession, experiment_accession, stype) = {select_profile_row}", .con = conn)
      DBI::dbExecute(conn, delete_str)
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_profile"), profile_data)

      header_data <- generate_header(inFile()$datapath, input$readxMap_study_accession_import, input$readxMap_experiment_accession_import, auth0_username)

      header_data <- data.frame(lapply(header_data, function(x) { gsub("[,]", ".", x) }))
      header_data <- data.frame(lapply(header_data, function(x) { gsub("[*]+", "", x) }))

      header_data$workspace_id <- userWorkSpaceID()
      user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
        filter(study_accession == header_data$study_accession &
               experiment_accession == header_data$experiment_accession &
               plate_id == header_data$plate_id &
               workspace_id == header_data$workspace_id)

      if(nrow(user_header_data) == 0){
        header_data$auth0_user <- current_user_nocompress()
        DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), header_data)
      }

      showNotification("Uploaded successfully", type = "message")
    },
    error = function(e) {
      showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                       type = "error",
                       closeButton = TRUE,
                       duration = NULL)
    }
  )
      reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

})
