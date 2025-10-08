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
                          table_name = "xmap_profile", # was import for study
                          select_where = list("concat(study_accession,experiment_accession,stype)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession_import,well_type))
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

  cat("=== ABOUT TO QUERY DATABASE ===\n")
  cat("Function parameters received:\n")
  cat("  study_accession:", study_accession, "\n")
  cat("  experiment_accession:", experiment_accession, "\n")
  cat("  data_type:", data_type, "\n")

  query_string <- paste0(study_accession, experiment_accession, data_type)
  cat("  Full query string:", query_string, "\n")
  cat("===============================\n")


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
      patient_exp_val <- strsplit(selected_col[3],",",fixed = TRUE)[[1]]
    } else {
      time_exp_val <- selected_col[1]
      patient_exp_val <- selected_col[1]
    }

  # imported_h_study(study_accession)
  # imported_h_experiment(experiment_accession)

  header_info <- fluidRow(
    column(
      12,
      div(
        style = "margin-bottom: 10px;",
        tags$span(style = "font-weight: bold;", "Study: "), study_accession,#textOutput("imported_h_studyimported_h_study", inline = TRUE),
        " | ",
        tags$span(style = "font-weight: bold;", "Experiment: "), experiment_accession,#textOutput("imported_h_experiment", inline = TRUE),
        " | ",
        tags$span(style = "font-weight: bold;", "plate_id: "), textOutput("imported_h_plate_id", inline = TRUE)
      )
    )
  )

  if(data_type == "X"){
    p1 <- tagList(header_info, p("Select segments corresponding to Dilution, Group name, Timepoint and Patient ID"))
  }else{
    p1 <- tagList(header_info, p("Select segments corresponding to Dilution and Source"))
  }

  table_ui <- rHandsontableOutput(paste0("table_", data_type))

  select_ui_dilution <- selectInput(paste0("select_dilution_", data_type),
                                    label = "Dilution",
                                    choices = col_names,
                                    selected = selected_col[1])

  if(data_type == "X"){
    group_label = "Group Name (up to 40 characters)"
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
    label = "Timepoint (up to 40 characters)",
    choices = list(colnames = col_names),
    selected = time_exp_val,
    multiple = TRUE
  )

  select_ui_patientID <- pickerInput(
    inputId = paste0("select_patientID_", data_type),
    label = "Patient ID",
    choices = list(colnames = col_names),
    selected = patient_exp_val,
    multiple = TRUE
    )

  select_ui3 <- verbatimTextOutput(paste0("print_name_", data_type))

  if(data_type == "X"){

    select_row_ui <- fluidRow(
      uiOutput("upload_sample_status"),
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
  } else if (data_type == "S"){
    select_row_ui <- fluidRow(
      uiOutput("upload_standards_status"),
      column(4, select_ui_dilution),
      column(4,
             fluidRow(select_ui_group_name,
                      select_ui3)
      )
    )
  } else if (data_type == "C"){
    select_row_ui <- fluidRow(
      uiOutput("upload_control_status"),
      column(4, select_ui_dilution),
      column(4,
             fluidRow(select_ui_group_name,
                      select_ui3)
      )
    )
  } else if (data_type == "B"){
    select_row_ui <- fluidRow(
      uiOutput("upload_buffer_status"),
      column(4, select_ui_dilution),
      column(4,
             fluidRow(select_ui_group_name,
                      select_ui3)
      )
    )
  }  else {

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

  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  if(!is.null(unique_plate_types())){

    type_vector <- unique_plate_types()


    create_ui_output <- function(type) {
      output_name <- paste0("ui_", type)
      output[[output_name]] <- renderUI({
        req(input$readxMap_experiment_accession_import)

        create_ui_for_type(type, study_accession = input$readxMap_study_accession, experiment_accession = input$readxMap_experiment_accession_import)
      })
    }

    for (type in type_vector) {
      if (type != "P") {
       create_ui_output(type)
      } else {
        output$ui_P <- renderUI({
          req(input$readxMap_experiment_accession_import)
          req(header_info())
          tagList(
            p("Edit editable fields in the table to assign plate information. The sample dilution factor must be between 1 and 100,000.
              plate should be in the form plate_n (n = 1–99, optionally followed by a lowercase letter a–z).
              Additional variables in the plate metadata that currently are not stored in the database are automatically ignored in the table for the uploading for Type P."),
            uiOutput("upload_head_status"),
            rHandsontableOutput("table_plates"),
            # conditionalPanel(
            #   condition = "output.type_p_completed_js == 'false'",
            #   #actionButton("assign_header", label = "Assign and Save")
            # )
          )

        })
      }
    }

  }else{
    "hi"
  }
})


# output$type_p_completed_js <- reactive({
#   type_p_completed()
# })

# output$type_p_completed_js <- renderText({
#   if (type_p_completed()) {
#     "true"   # string, not logical
#   } else {
#     "false"
#   }
# })

output$table_plates <- renderRHandsontable({
  req(header_info())

  print("Header info:")
  print(header_info())

  # Parse metadata into key-value df
  meta_df <- parse_metadata_df(header_info())

  # Variables stored in the database
  stored_variables <- c(
    "file_name", "acquisition_date", "reader_serial_number",
    "rp1_pmt_volts", "rp1_target", "plateid", "plate_id", "plate"
  )
 # remove any extra for downstream saving.
  extra_cols <- setdiff(names(meta_df), stored_variables)
  if (length(extra_cols) > 0) {
    meta_df <- meta_df[, setdiff(names(meta_df), extra_cols), drop = FALSE]
  }


                               # study_accession = input$readxMap_study_accession_import,
                               # experiment_accession = input$readxMap_experiment_accession_import,
                               # currentuser = currentuser(),
                               # workspace_id = userWorkSpaceID())

  # meta_df <- cbind(study_accession = input$readxMap_study_accession_import,
  #                  experiment_accession = input$readxMap_experiment_accession_import,
  #                  meta_df,
  #                  auth0_user = currentuser(),
  #                  workspace_id = userWorkSpaceID())
  #
  df_long <- meta_df %>%
    pivot_longer(
      cols = everything(),       # pivot all columns
      names_to = "variable",     # column names go here
      values_to = "value"        # column values go here
    )

  new_rows <- data.frame(
    variable = c( "sample_dilution_factor", "study_accession", "experiment_accession", "auth0_user", "workspace_id"),
    value = c(
      NA,
      input$readxMap_study_accession, # was import
      input$readxMap_experiment_accession_import,
      currentuser(),
      as.character(userWorkSpaceID())
    ),
    stringsAsFactors = FALSE
  )

  df_long <- rbind(df_long, new_rows)
  #df_long$variable as.POSIXct(strptime(gsub(",",":",gsub(" ","",df_long[["acquisition_date"]])),format='%d-%b-%Y:%H:%M%p'), tz = "EST")


  # meta_long <- meta_df %>%
  #   tidyr::pivot_longer(cols = everything(), names_to = "Field", values_to = "Value")
  #
 ht <-  rhandsontable(df_long, rowHeaders = NULL) %>%
    hot_col("variable", readOnly = TRUE) %>%  # Disable editing keys
       hot_cols(
         renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.TextRenderer.apply(this, arguments);
              if (value === null || value === '' || value === 'NA') {
                td.style.background = 'lightcoral';   // highlight empty cells
                td.innerHTML = 'please fill in';
                td.style.fontStyle = 'italic';
              }
            }"
       )

 for (i in seq_len(nrow(df_long))) {
   if (df_long$variable[i] %in% c("file_name", "study_accession", "experiment_accession", "plate_id", "auth0_user", "workspace_id")) {
     ht <- hot_cell(ht, row = i, col = "value", readOnly = TRUE)
   }
 }




 #ht_view <<- ht
 # sample_dil_row <- which(df_long$variable == "sample_dilution_factor")
 # if (length(sample_dil_row) == 1) {
 #   ht <-hot_col(ht, col = "value", type = "numeric") %>%
 #     hot_cell(row = sample_dil_row, col = "value", readOnly = FALSE)
 # }


 # js_validator <- sprintf(
 #   "function(value, row) { if (row === %d) { return value >= 1 && value <= 1000000; } else { return true; } }",
 #   sample_dil_row - 1  # JS is 0-indexed
 # )
 #
 # # Apply to rhandsontable
 # ht <- rhandsontable(df_long, rowHeaders = NULL) %>%
 #   hot_col("variable", readOnly = TRUE) %>%
 #   hot_col("value", type = "numeric", validator = js_validator)


 ht
  # rhandsontable(meta_df, rowHeaders = NULL) %>%
  #   hot_col("study_accession", readOnly = T) %>%
  #   hot_col("experiment_accession", readOnly = T) %>%
  #   hot_col("auth0_user", readOnly = T) %>%
  #   hot_col("workspace_id", readOnly = T)
})

observeEvent(input$table_plates, {

  updated_table <- hot_to_r(input$table_plates)
  current_type_p_tab(updated_table)

  # Update the reactiveVals whenever the table is updated
  import_study <- updated_table[updated_table$variable == "study_accession", ]$value
  #imported_h_study(import_study)

  import_experiment <- updated_table[updated_table$variable == "experiment_accession", ]$value
  #imported_h_experiment(import_experiment)

  import_plate_id <- updated_table[updated_table$variable == "plate_id", ]$value
  imported_h_plate_id(import_plate_id)
})

observeEvent(input$assign_header, {
  req(current_type_p_tab())
  head_p <- current_type_p_tab()
  # head_p_v <<- head_p

  p_wide <- head_p %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    )

  p_wide$acquisition_date <- as.POSIXct(strptime(gsub(",",":",gsub(" ","",p_wide[["acquisition_date"]])),format='%d-%b-%Y:%H:%M%p'), tz = "EST")

  p_wide <- p_wide[, c("study_accession", "experiment_accession", "plate_id", "acquisition_date",
                       "file_name", "reader_serial_number", "rp1_pmt_volts", "rp1_target", "auth0_user",
                       "workspace_id", "plateid", "plate", "sample_dilution_factor")]

   p_wide$sample_dilution_factor <- as.numeric(p_wide$sample_dilution_factor)

  #p_wide_v <<- p_wide

  if (is.na(p_wide$sample_dilution_factor) ||
      p_wide$sample_dilution_factor < 1 ||
      p_wide$sample_dilution_factor > 1000000 ) {
    showNotification("Sample Dilution Factor must be numeric and between 1 and 1,000,000")
  } else if (!grepl("^plate_([1-9]|[1-9][0-9])[a-z]?$", p_wide$plate)) {
  showNotification("The plate must be in the form plate_n (n = 1–99, optionally followed by a lowercase letter a–z) (e.g., plate_1, plate_23)")
  } else {

  # capture values
  import_study <- p_wide$study_accession
  #imported_h_study(import_study)
  import_experiment <- p_wide$experiment_accession
 # imported_h_experiment(import_experiment)
  import_plate_id <- p_wide$plate_id
  #imported_h_plate_id(import_plate_id)

  plate_query <- glue::glue_sql("SELECT *
	FROM madi_results.xmap_header
	WHERE plate_id = {import_plate_id}
	AND study_accession = {import_study}
	AND experiment_accession = {import_experiment}",
                                   .con = conn)
  plate_query_data <- dbGetQuery(conn, plate_query)

  if(nrow(plate_query_data) == 0) {
    cat("procceed")
    tryCatch({
      DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), p_wide)

      showNotification(glue::glue("Plate Header Uploaded"), type = "message")
    }, error = function(e) {
      showNotification(glue::glue("Error inserting Plate Header"), type = "error")
    })

    type_p_completed(TRUE)

  } else {
    cat("header already uploaded")
  }

  }


})

# plate_exists <- reactive({
#   req(current_type_p_tab())
#   head_p <- current_type_p_tab()
#
#   p_wide <- head_p %>%
#     pivot_wider(names_from = variable, values_from = value)
#
#   import_study <- p_wide$study_accession
#   import_experiment <- p_wide$experiment_accession
#   import_plate_id <- p_wide$plate_id
#
#   plate_query <- glue::glue_sql(
#     "SELECT *
#      FROM madi_results.xmap_header
#      WHERE plate_id = {import_plate_id}
#        AND study_accession = {import_study}
#        AND experiment_accession = {import_experiment}",
#     .con = conn
#   )
#
#   nrow(DBI::dbGetQuery(conn, plate_query)) > 0
# })
#
# observe({
#   type_p_completed(plate_exists())
# })
#
# output$upload_head_status <- renderUI({
#   exists <- plate_exists()
#
#   if (!exists) {
#     div(
#       style = "display: flex; align-items: center; gap: 10px;",
#       createUploadStatusBadge(is_uploaded = exists),
#       actionButton("assign_header", "Upload Plate")
#     )
#   } else {
#     createUploadStatusBadge(is_uploaded = exists)
#   }
# })

observe({
  req(current_type_p_tab())  # make sure a plate is selected
  head_p <- current_type_p_tab()
  type_p_completed()
  # pivot to get plate_id, study, experiment
  p_wide <- head_p %>%
    pivot_wider(names_from = variable, values_from = value)

  import_study <- p_wide$study_accession
  import_experiment <- p_wide$experiment_accession
  import_plate_id <- p_wide$plate_id

  # check if plate already exists in DB
  plate_query <- glue::glue_sql(
    "SELECT *
     FROM madi_results.xmap_header
     WHERE plate_id = {import_plate_id}
       AND study_accession = {import_study}
       AND experiment_accession = {import_experiment}",
    .con = conn
  )

  n_row_plate <- nrow(dbGetQuery(conn, plate_query))

  plate_exists <- n_row_plate > 0

  type_p_completed(plate_exists)

  # output$upload_head_status <- renderUI({
  #   createUploadStatusBadge(is_uploaded = plate_exists)
  #
  # })
  output$upload_head_status <- renderUI({
    type_p_completed() # trigger refresh

    if (!plate_exists) {
      div(
        style = "display: flex; align-items: center; gap: 10px;",
        createUploadStatusBadge(is_uploaded = plate_exists, n_record = n_row_plate),
        actionButton("assign_header", "Upload Plate")
      )
    } else {
      createUploadStatusBadge(is_uploaded = plate_exists, n_record = n_row_plate)
    }
  })


  #imported_h_study(input$readxMap_study_accession)
  #imported_h_experiment(input$readxMap_experiment_accession_import)
  # if (plate_exists) {
  #   shinyjs::disable("assign_header")
  #   #showNotification("Header already uploaded, assign button disabled", type = "warning")
  # } else {
  #   shinyjs::enable("assign_header")
  # }
})

createUploadStatusBadge <- function(is_uploaded, n_record) {
  if(n_record >1 ) {
    plural <- "s"
  } else {
    plural <- ""
  }

  if (is_uploaded) {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste(" Completed Upload (", n_record, " record",plural, ")", sep = ""))
    )
  } else {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), " Not Uploaded")
    )
  }
}

# observe({
#   if(type_p_completed()) {
#     shinyjs::disable("assign_header")
#   } else {
#     shinyjs::enable("assign_header")
#   }
# })
# observeEvent(type_p_completed(), {
#   cat("P completed?: ")
#   print(type_p_completed())
#   type_p_status <- type_p_completed()
#   if (type_p_status) {
#     shinyjs::disable("assign_header")
#   } else {
#     shinyjs::enable("assign_header")
#   }
# })


# edited_header_table <- reactive({
#   hot_to_r(input$meta_table_P)
# })


clean_plateids <- function(plates) {
  if (nrow(plates) == 0) return(plates)

  # Always extract basename (last bit of path)
  plates$plateidr <- str_trim(
    str_replace_all(str_split_i(plates$file_name, "\\\\", -1), " ", ""),
    side = "both"
  )

  # "plateidv" is the candidate version
  plates$plateidv <- ifelse(is.na(plates$file_name), plates$plateidr, plates$file_name)
  plates$plateidv <- str_replace_all(plates$plateidv, fixed(".."), "_")
  plates$plateidv <- str_replace_all(plates$plateidv, fixed("."), "_")
  plates$plateidv <- str_replace_all(plates$plateidv, fixed("plate_"), "plate")
  plates$plateidv <- str_remove(plates$plateidv, "\\.rbx$")

  # If plateid already exists, keep it; otherwise use plateidv
  if (str_trim(str_replace_all(plates$file_name, "\\s", ""), side = "both") == plates$plateid) {
    plates$plateid <- str_remove(plates$plateidr, "\\.rbx$")
    plates$plate_id <- str_trim(str_replace_all(plates$file_name, "\\s", ""), side = "both")
  }
  # if (!"plateid" %in% names(plates)) {
  #   cat("no plateid in header")
  #   plates_h <<- plates
  #   plates$plateid <- plates$plateidv
   else {
    plates$plateid <- ifelse(is.na(plates$plateid) | plates$plateid == "",
                             plates$plateidv,
                             plates$plateid)
    plates$plate_id <- str_trim(str_replace_all(plates$file_name, "\\s", ""), side = "both")
  }

  # Lowercase + cleaning for downstream extraction
  plates$plateids <- tolower(plates$plateid)
  plates$plateids <- str_trim(str_replace_all(plates$plateids, "\\s", ""), side = "both")
  plates$plateids <- str_replace_all(plates$plateids, "plaque", "plate")
  plates$plateids <- str_replace_all(plates$plateids, "_pt", "_plate")

  # Extract plate number safely
  plates$plate <- str_split_i(plates$plateids, "plate", -1)
  plates$plate <- paste0("plate_", str_split_i(plates$plate, "_", 1))
  plates$plate <- str_extract(plates$plate, "plate_\\d+")

  # Drop helper cols
  plates <- plates[, !(names(plates) %in% c("plateidr", "plateids", "plateidv"))]

  return(plates)
}


parse_plate_header <- function(plates) {
 plates <<- plates
  plates$plateidr <- str_trim(str_replace_all(str_split_i(plates$file_name, "\\\\", -1), " ", ""), side = "both")
   plates$plateid <- ifelse(is.na(plates$file_name),
                           plates$plateidr,
                           plates$file_name)
  plates$plateid <- str_replace_all(plates$plateid, fixed(".."),"_")
  plates$plateid <- str_replace_all(plates$plateid, fixed("."),"_")
  plates$plateidv <- str_replace_all(plates$plateid, fixed("plate_"),"plate")
  plates$plate_id_proc <- str_trim(str_replace_all(plates$file_name, "\\s", ""), side = "both")
  if (nrow(plates) > 0) {
  #  plates$needs_update <- ifelse(is.na(plates$plate), 1, plates$needs_update)
    plates$plateids <- tolower(plates$plateid)
    plates$plateids <- str_trim(str_replace_all(plates$plateids, "\\s", ""), side = "both")
    plates$plateids <- stringr::str_replace_all(plates$plateids, "plaque", "plate")
    plates$plateids <- stringr::str_replace_all(plates$plateids, "_pt", "_plate")
    plates$plate <- str_split_i(plates$plateids, "plate",-1)
    plates$plate <- paste("plate",str_split_i(plates$plate, "_",1),sep = "_")


    plates$plate <- str_extract(plates$plate, "plate_\\d+")

  }
  # if (plates$plate_id == "") {
  #   plates$plate_id <- plates$plateid
  # }
  # plates$plateid <- plates$plateidr
  # plates$plateid <- str_remove(plates$plateid, "\\.rbx$")

  plates <- plates[,!(names(plates) %in% c("plateidr", "plateids", "plate_id_proc", "plateidv"))]
  return(plates)
}


# clean_plateids <- function(plates) {
#
#   # Extract last part of filename (basename)
#   plates$plateidr <- str_trim(
#     str_replace_all(str_split_i(plates$file_name, "\\\\", -1), " ", ""),
#     side = "both"
#   )
#
#   # If plateid exists, keep it; otherwise use plateidr
#   if (!"plateid" %in% names(plates)) {
#     plates$plateid <- plates$plateidr
#   } else {
#     plates$plateid <- ifelse(is.na(plates$plateid) | plates$plateid == "",
#                              plates$plateidr,
#                              plates$plateid)
#
#
#   # Clean replacements
#   plates$plateid <- str_replace_all(plates$plateid, fixed(".."), "_")
#   plates$plateid <- str_replace_all(plates$plateid, fixed("."), "_")
#   plates$plateid <- str_replace_all(plates$plateid, fixed("plate_"), "plate")
#   plates$plateid <- str_remove(plates$plateid, "\\.rbx$")
#
#   }
#   # Normalize lowercase versions for matching
#   plates$plateids <- tolower(plates$plateid)
#   plates$plateids <- str_trim(str_replace_all(plates$plateids, "\\s", ""), side = "both")
#   plates$plateids <- str_replace_all(plates$plateids, "plaque", "plate")
#   plates$plateids <- str_replace_all(plates$plateids, "_pt", "_plate")
#
#   # Extract plate number
#   plates$plate <- str_split_i(plates$plateids, "plate", -1)
#   plates$plate <- paste0("plate_", str_split_i(plates$plate, "_", 1))
#   plates$plate <- str_extract(plates$plate, "plate_\\d+")
#
#   # Drop helper columns
#   plates <- plates[, !(names(plates) %in% c("plateidr", "plateids"))]
#
#   return(plates)
# }

parse_metadata_df <- function(df) {
  # Extract the column as a character vector
  meta_lines <- df$X1

  # Parse lines into key-value pairs
  meta_df <- do.call(rbind, lapply(meta_lines, function(line) {
    parts <- strsplit(line, ":", fixed = TRUE)[[1]]
    value <- if (length(parts) > 1) trimws(paste(parts[-1], collapse = ":")) else NA
    data.frame(field = trimws(parts[1]), value = value, stringsAsFactors = FALSE)
  }))

 if (meta_df[meta_df$field == "Plate ID",]$value == "") {
    meta_df[meta_df$field == "Plate ID",]$value <- meta_df[meta_df$field == "File Name",]$value
  }

  meta_df[meta_df$field == "Plate ID", "value"] <-
    str_trim(str_replace_all(meta_df[meta_df$field == "Plate ID", "value"], "\\s", ""), side = "both")

  if (meta_df[meta_df$field == "Plate ID",]$value != "") {
    meta_df$field[meta_df$field == "Plate ID"] <- "plateid"

  }
  #meta_df <-  meta_df[meta_df$field != "Plate ID",]
  # meta_df <- rbind(
  #   meta_df,
  #   study_accession = study_accession,
  #   experiment_accession = experiment_accession,
  #   auth0_user = currentuser,
  #   workspace_id = workspace_id
  # )

  # meta_df$study_accession <- study_accession
  # meta_df$experiment_accession <- experiment_accession
  # meta_df$auth0_user <- currentuser
  # meta_df$workspace_id <- workspace_id



  meta_df <- meta_df %>%
               pivot_wider(names_from = field, values_from = value)
  names(meta_df) <- tolower(names(meta_df))
  names(meta_df) <- gsub(" ", "_", names(meta_df))
  #names(meta_df)[names(meta_df) == "plate_id"] <- "plateid"

  names(meta_df) <- gsub("[()]", "", names(meta_df))


  meta_df <- clean_plateids(meta_df)

  # Convert time to this form for db
 #meta_df$acquisition_date <- as.POSIXct(strptime(gsub(",",":",gsub(" ","",meta_df[["acquisition_date"]])),format='%d-%b-%Y:%H:%M%p'), tz = "EST")

 # meta_df <- meta_df

  return(meta_df)
}


# output$imported_h_study <- renderText({
#  # req(input$readxMap_study_accession)
#   input$readxMap_study_accession
#   # req(imported_h_study())
#   # imported_h_study()
# })
#
# output$imported_h_experiment <- renderText({
# #  req(input$readxMap_experiment_accession_import)
#   input$readxMap_experiment_accession_import
#   #imported_h_experiment()
# })

output$imported_h_plate_id <- renderText({
  req(imported_h_plate_id())
  imported_h_plate_id()
})


# observeEvent(input$assign_header, {
#
#   current_df <- hot_to_r(input$table_plates)
#
#
#   # current_df$dilution <- current_df[[input$select_dilution_X]]
#   #
#   # current_df %>%
#   #   mutate(group_name = do.call(paste, c(select(., all_of(input$select_group_X)), sep = "_"))) -> current_df
#   #
#   # current_df %>%
#   #   mutate(timepoint = do.call(paste, c(select(., all_of(input$select_timepoint_X)), sep = "_"))) -> current_df
#   #
#   current_df$auth0_user <- currentuser()
#
#   output$table_plates <- renderRHandsontable({
#     rhandsontable(current_df,
#                   overflow = "visible",
#                   horizontal_scroll = TRUE,
#                   vertical_scroll = TRUE, width = 1024, height = 300) %>%
#      ## hot_validate_numeric(cols = "dilution", min = 1) %>%
#       hot_col(c("auth0_user"), renderer = color_renderer) #%>%
#      # hot_col(col = "feature", type = "dropdown", source = c("ADCD", "Total_IgG", "IgG1", "IgG2", "IgG3", "IgG4", "IgA", "IgA1", "IgM", "IgM1", "FcgR2a", "FcgR2b", "FcgR3a", "FcgR3b"))
#   })
#
# })



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

  current_df %>%
    mutate(patient_id = do.call(paste, c(select(., all_of(input$select_patientID_X)), sep = "_"))) -> current_df

  # current_df$patient_id <- current_df[[input$select_patientID_X]]

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

  req(imported_h_plate_id())

  auth0_username <- session$userData$auth0_info$nickname

  print("upload for X")
  print(input$readxMap_study_accession) # was import
  print(input$readxMap_experiment_accession_import)

  study_name_import <- as.character(input$readxMap_study_accession)
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
  # from what was stored before when updating the header
  type_x_ready$plate_id <- imported_h_plate_id()
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


      ## Header is already saved
      # header_data <- generate_header(inFile()$datapath, input$readxMap_study_accession_import, input$readxMap_experiment_accession_import, auth0_username)
      #
      # header_data <- data.frame(lapply(header_data, function(x) {  gsub("[,]", ".", x) }))
      # header_data <- data.frame(lapply(header_data, function(x) {  gsub("[*]+", "", x) }))
      #
      # header_data$workspace_id <- userWorkSpaceID()
      # user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
      #   filter(study_accession == header_data$study_accession & experiment_accession == header_data$experiment_accession & plate_id == header_data$plate_id  & workspace_id == header_data$workspace_id)
      #
      # if(nrow(user_header_data) == 0){
      #   header_data$auth0_user <- current_user_nocompress()
      #   DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), header_data)
      # }

      showNotification("Uploaded successfully", type = "message")

       #reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
       print("reactive_df_study_exp:loaded")

       # refresh update that it is loaded.
       type_x_status(list(
         plate_exists = TRUE,
         n_record = nrow(type_x_ready)
       ))
})
})

## Observe for type X
observe({
  req(input$readxMap_study_accession) #was _import
  req(input$readxMap_experiment_accession_import)
  req(imported_h_plate_id())

  study_name <- input$readxMap_study_accession
  experiment_name <- input$readxMap_experiment_accession_import
  plate_id <- imported_h_plate_id()

  query <- glue::glue_sql(
    "
    SELECT *
    FROM madi_results.xmap_sample
    WHERE study_accession = {study_name}
      AND experiment_accession = {experiment_name}
      AND plate_id = {plate_id}
    ",
    .con = conn
  )

  sample_data <- DBI::dbGetQuery(conn, query)

  # Update reactive status
  type_x_status(list(
    plate_exists = nrow(sample_data) > 0,
    n_record = nrow(sample_data)
  ))
})

output$upload_sample_status <- renderUI({
  status <- type_x_status()

  div(
    style = "display: flex; align-items: center; gap: 10px;",
    createUploadStatusBadge(is_uploaded = status$plate_exists, n_record = status$n_record)
  )
})



#   output$upload_sample_status <- renderUI({
#     type_x_completed() # trigger refresh
#
#     if (!plate_sample_exists) {
#       div(
#         style = "display: flex; align-items: center; gap: 10px;",
#         createUploadStatusBadge(is_uploaded = plate_sample_exists, n_record = nrow_sample),
#       )
#     } else {
#       createUploadStatusBadge(is_uploaded = plate_sample_exists, n_record = nrow_sample)
#     }
#   })
# })

# observe({
#     status <<- type_x_status()
#
#     # if (status$plate_exists) {
#     #   shinyjs::disable("upload_type_X")
#     # } else {
#     #   shinyjs::enable("upload_type_X")
#     # }
#   })



observeEvent(input$upload_type_S, {
  req(imported_h_plate_id())

  auth0_username <- session$userData$auth0_info$nickname
  print("upload for S")
  print(input$readxMap_study_accession) # was _import
  print("experiment_accession is")
  print(input$readxMap_experiment_accession_import)

  study_name_import <- as.character(input$readxMap_study_accession)
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
  #from what was stored before when updating the header
  type_s_ready$plate_id <- imported_h_plate_id()

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

      # header_data <- generate_header(inFile()$datapath,
      #                                input$readxMap_study_accession_import,
      #                                input$readxMap_experiment_accession_import, auth0_username)
      #
      # header_data <- data.frame(lapply(header_data, function(x) {  gsub("[,]", ".", x) }))
      # header_data <- data.frame(lapply(header_data, function(x) {  gsub("[*]+", "", x) }))
      #
      # header_data$workspace_id <- userWorkSpaceID()
      # user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
      #   filter(study_accession == header_data$study_accession &
      #          experiment_accession == header_data$experiment_accession &
      #          plate_id == header_data$plate_id &
      #          workspace_id == header_data$workspace_id)
      #
      # if(nrow(user_header_data) == 0){
      #   header_data$auth0_user <- current_user_nocompress()
      #   DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), header_data)
      # }

      showNotification("Uploaded successfully", type = "message")
    },
    error = function(e) {
      showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                       type = "error" ,
                       closeButton = TRUE,
                       duration = NULL)
    }
  )
      #reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

      # refresh update that it is loaded.
      type_s_status(list(
        plate_exists = TRUE,
        n_record = nrow(type_s_ready)
      ))
})

## Observe for type S
observe({
  req(input$readxMap_study_accession) # was import
  req(input$readxMap_experiment_accession_import)
  req(imported_h_plate_id())

  study_name <- input$readxMap_study_accession
  experiment_name <- input$readxMap_experiment_accession_import
  plate_id <- imported_h_plate_id()

  query <- glue::glue_sql(
    "
    SELECT *
    FROM madi_results.xmap_standard
    WHERE study_accession = {study_name}
      AND experiment_accession = {experiment_name}
      AND plate_id = {plate_id}
    ",
    .con = conn
  )

  standards_data <- DBI::dbGetQuery(conn, query)

  # Update reactive status
  type_s_status(list(
    plate_exists = nrow(standards_data) > 0,
    n_record = nrow(standards_data)
  ))
})

output$upload_standards_status <- renderUI({
  status <- type_s_status()

  div(
    style = "display: flex; align-items: center; gap: 10px;",
    createUploadStatusBadge(is_uploaded = status$plate_exists, n_record = status$n_record)
  )
})



observeEvent(input$upload_type_C, {
  req(imported_h_plate_id())

  auth0_username <- session$userData$auth0_info$nickname
  print("upload for C")

  print(input$readxMap_study_accession) #was _import
  print(input$readxMap_experiment_accession_import)

  study_name_import <- as.character(input$readxMap_study_accession)
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
  # from what was stored before when updating the header
  type_c_ready$plate_id <- imported_h_plate_id()
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

      # header_data <- generate_header(inFile()$datapath, input$readxMap_study_accession_import, input$readxMap_experiment_accession_import, auth0_username)
      #
      # header_data <- data.frame(lapply(header_data, function(x) { gsub("[,]", ".", x) }))
      # header_data <- data.frame(lapply(header_data, function(x) { gsub("[*]+", "", x) }))
      #
      # header_data$workspace_id <- userWorkSpaceID()
      # user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
      #   filter(study_accession == header_data$study_accession &
      #          experiment_accession == header_data$experiment_accession &
      #          plate_id == header_data$plate_id &
      #          workspace_id == header_data$workspace_id)
      #
      # if(nrow(user_header_data) == 0){
      #   header_data$auth0_user <- current_user_nocompress()
      #   DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), header_data)
      # }

      showNotification("Uploaded successfully", type = "message")
    },
    error = function(e) {
      showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                       type = "error",
                       closeButton = TRUE,
                       duration = NULL)
    }
  )
     # reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

      # refresh update that it is loaded.
      type_c_status(list(
        plate_exists = TRUE,
        n_record = nrow(type_c_ready)
      ))
})

## Observe for type C
observe({
  req(input$readxMap_study_accession) #was _import
  req(input$readxMap_experiment_accession_import)
  req(imported_h_plate_id())

  study_name <- input$readxMap_study_accession
  experiment_name <- input$readxMap_experiment_accession_import
  plate_id <- imported_h_plate_id()

  query <- glue::glue_sql(
    "
    SELECT *
    FROM madi_results.xmap_control
    WHERE study_accession = {study_name}
      AND experiment_accession = {experiment_name}
      AND plate_id = {plate_id}
    ",
    .con = conn
  )

  control_data <- DBI::dbGetQuery(conn, query)

  # Update reactive status
  type_c_status(list(
    plate_exists = nrow(control_data) > 0,
    n_record = nrow(control_data)
  ))
})

output$upload_control_status <- renderUI({
  status <- type_c_status()

  div(
    style = "display: flex; align-items: center; gap: 10px;",
    createUploadStatusBadge(is_uploaded = status$plate_exists, n_record = status$n_record)
  )
})



observeEvent(input$upload_type_B, {
  req(imported_h_plate_id())

  auth0_username <- session$userData$auth0_info$nickname
  print("upload for B")
  print(input$readxMap_study_accession) # was import
  print(input$readxMap_experiment_accession_import)

  study_name_import <- as.character(input$readxMap_study_accession)
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
                                     study_accession = input$readxMap_study_accession, # was import
                                     exp_accession = input$readxMap_experiment_accession_import)
  # from what was stored before when updating the header
  type_b_ready$plate_id <- imported_h_plate_id()
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

      # header_data <- generate_header(inFile()$datapath, input$readxMap_study_accession_import, input$readxMap_experiment_accession_import, auth0_username)
      #
      # header_data <- data.frame(lapply(header_data, function(x) { gsub("[,]", ".", x) }))
      # header_data <- data.frame(lapply(header_data, function(x) { gsub("[*]+", "", x) }))
      #
      # header_data$workspace_id <- userWorkSpaceID()
      # user_header_data <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_header") %>%
      #   filter(study_accession == header_data$study_accession &
      #          experiment_accession == header_data$experiment_accession &
      #          plate_id == header_data$plate_id &
      #          workspace_id == header_data$workspace_id)
      #
      # if(nrow(user_header_data) == 0){
      #   header_data$auth0_user <- current_user_nocompress()
      #   DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), header_data)
      # }

      showNotification("Uploaded successfully", type = "message")
    },
    error = function(e) {
      showNotification(paste0("An error occurred: ", conditionMessage(e), "\n"),
                       type = "error",
                       closeButton = TRUE,
                       duration = NULL)
    }
  )
     # reactive_df_study_exp(reloadReactive(conn, userWorkSpaceID()))
      print("reactive_df_study_exp:loaded")

      # refresh update that it is loaded.
      type_b_status(list(
        plate_exists = TRUE,
        n_record = nrow(type_b_ready)
      ))

})

## Observe for type B
observe({
  req(input$readxMap_study_accession) # was import
  req(input$readxMap_experiment_accession_import)
  req(imported_h_plate_id())

  study_name <- input$readxMap_study_accession
  experiment_name <- input$readxMap_experiment_accession_import
  plate_id <- imported_h_plate_id()

  query <- glue::glue_sql(
    "
    SELECT *
    FROM madi_results.xmap_buffer
    WHERE study_accession = {study_name}
      AND experiment_accession = {experiment_name}
      AND plate_id = {plate_id}
    ",
    .con = conn
  )

  buffer_data <- DBI::dbGetQuery(conn, query)

  # Update reactive status
  type_b_status(list(
    plate_exists = nrow(buffer_data) > 0,
    n_record = nrow(buffer_data)
  ))
})

output$upload_buffer_status <- renderUI({
  status <- type_b_status()

  div(
    style = "display: flex; align-items: center; gap: 10px;",
    createUploadStatusBadge(is_uploaded = status$plate_exists, n_record = status$n_record)
  )
})

