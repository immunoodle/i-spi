
createLink <- function(val) {
  #sprintf(paste0('<a href="', URLdecode(val),'" target="_blank">', substr(val, 1, 25) ,'</a>'))
  sprintf(paste0('<a href="', val,'" target="_blank">', substr(val, 1, 254) ,'</a>'))
}

update_db <- function(..., schema = "madi_results",help = FALSE){

  args <- list(...)

  if(help == T){
    if(length(args) == 0){
      return(cat("This function helps perform select, insert and update operations on tables
                \n It accepts 3 inputs \n - operation : 'select', 'insert', 'update' \n - table_name : name of the table from the db schema madi_results \n - db_data : Dataframe containing values as a single row for insertion or updation. Dataframe should have equal number of columns as the table \n - update_where : named vector (Example - c(program_id = 1)) \n \nUse the help = TRUE for any guidance with the inputs \n \n For example: update_db(table_name = 'program', help = TRUE)"))
    }else if(length(args$table_name) != 0){
      DBI::dbGetQuery(conn,
                      glue_sql(.con = conn,
                               "SELECT table_name, column_name, is_nullable, data_type, character_maximum_length, numeric_precision, numeric_scale FROM information_schema.columns WHERE table_schema = {schema} AND table_name IN ({args$table_name*});")
      )
    }
  } else if(help == F){
    if(length(args$operation) != 0){

      stopifnot(length(args$operation) == 1, length(args$table_name) == 1)

      # Check if schema is entered
      if(!is.null(args$schema)){
        schema <- args$schema
      }

      if(args$operation == "insert"){
        print("inside insert")

        # Get column names for the table requested
        get_column_names <-DBI::dbGetQuery(conn,
                                           glue_sql(.con = conn,
                                                    "SELECT column_name FROM information_schema.columns WHERE table_schema = {schema} AND table_name IN ({args$table_name*});")
        )

        # Constructing the SQL query in 1 parts.
        # "INSERT INTO madi_results.<table_name> (part1) VALUES (part2)"

        part1 <- paste(get_column_names$column_name, collapse = ",")
        part2 <- paste0("{", get_column_names$column_name, "}", collapse = ", ")

        # Using assign instead of attach to create variables locally within the function
        # attach(args$db_data)

        for (col_name in names(args$db_data)) {
          assign(col_name, args$db_data[[col_name]])
        }

        glue_str <- glue(
          "INSERT INTO {schema}.{args$table_name} ({part1}) VALUES ({part2});"
        )


        print(glue_sql(.con = conn,glue_str))

        DBI::dbSendQuery(conn, glue_sql(.con = conn,glue_str))

        print("insert complete")

        # No need to use detach as we are not using attach anymore
        # detach(args$db_data)
      }

      if(args$operation == "select"){

        print("inside select")

        if(is.null(args$select_where)){
          print("selecting all columns_start")
          glue_str <- glue(
            "SELECT * FROM {schema}.{args$table_name};"
          )

          print(glue_sql(.con = conn, glue_str))

          result <- DBI::dbGetQuery(conn, glue_sql(.con = conn, glue_str))

          print("selecting all columns_end")

          return(result)
        }
        else if (length(args$select_where[[1]]) == 1){

          print(names(args$select_where))
          select_where_col <- names(args$select_where)

          if(class(args$select_where[[1]]) %in% c("numeric", "integer")){
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name} WHERE {select_where_col} = {args$select_where};"
            )
          }else if(class(args$select_where[[1]])=="character"){
            print("location HF")
            # Adding an extra parenthesis {} because glue would convert the variable to a character in here and then not have quotes later
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name} WHERE {select_where_col} = {{args$select_where}};"
            )
          }
          print(paste0("selecting where ", names(args$select_where) ,"=",args$select_where))

          print(glue_sql(.con = conn, glue_str))

          print("location HG")
          result <- DBI::dbGetQuery(conn, glue_sql(.con = conn, glue_str))

          # print(glue_sql(.con = conn, glue_str))

          # print(paste0("selecting where ", names(args$select_where) ,"=",args$select_where))

          return(result)
        }
        else{

          select_where_col <- names(args$select_where)

          if(class(args$select_where[[1]]) %in% c("numeric", "integer")){
            select_where_ids <- paste0(args$select_where[[1]], collapse = ",")
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name} WHERE {select_where_col} IN ({select_where_ids});"
            )
          }else if(class(args$select_where[[1]])=="character"){
            select_where_ids <- paste0("'", args$select_where[[1]], "'", collapse = ",")
            # Adding an extra parenthesis {} because glue would convert the variable to a character in here and then not have quotes later
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name} WHERE {select_where_col} IN ({select_where_ids});"
            )
          }
          else {
            glue_str <- glue(
              "SELECT * FROM {schema}.{args$table_name};"
            )
          }
          print("location HH")

          result <- DBI::dbGetQuery(conn, glue_sql(.con = conn, glue_str))

          print(paste0("selecting where ", names(args$select_where) ,"=",args$select_where))

          return(result)

        }



      }

      if(args$operation == "update"){

        print("inside update")

        get_column_names <-DBI::dbGetQuery(conn,
                                           glue_sql(.con = conn,
                                                    "SELECT column_name FROM information_schema.columns WHERE table_schema = {schema} AND table_name IN ({args$table_name*});")
        )

        db_cols <- paste(get_column_names$column_name)
        update_where_col <- names(args$update_where)

        # attach(args$db_data)
        for (col_name in names(args$db_data)) {
          assign(col_name, args$db_data[[col_name]])
        }

        # Use paste() to concatenate strings with values from a and b vectors
        result <- paste(db_cols, "=", "{", db_cols, "}", collapse = ", ")

        if(class(args$update_where) %in% c("numeric", "integer")){
          glue_str <- glue(
            "UPDATE {schema}.{args$table_name} SET {result} WHERE {update_where_col} = {args$update_where};"
          )
        }else if(class(args$update_where)=="character"){
          # Adding an extra parenthesis {} because glue would convert the variable to a character in here and then not have quotes later
          glue_str <- glue(
            "UPDATE {schema}.{args$table_name} SET {result} WHERE {update_where_col} = {{args$update_where}};"
          )
        }
        print(glue_sql(.con = conn, glue_str))

        DBI::dbSendQuery(conn, glue_sql(.con = conn, glue_str))
        print("update complete")

        # detach(args$db_data)

      }


    }
  }

}

combine_plate_data <- function(full_data, table_data, type, new_col_df, study_accession, exp_accession){

  if(exp_accession == ""){
    showNotification("Please select experiment name", type = "error")
    print("exp_accession is ''")
    return(NULL)
  }

  if(study_accession == ""){
    showNotification("Please select study name", type = "error")
    print("study accession is ''")
    return(NULL)
  }

  if(all(new_col_df$feature == "")){
    showNotification("Error: Blank feature column",type = "error")
    print("feature is ''")
    return(NULL)
  }else if(any(new_col_df$feature == "")){
    print("some features are ''")
    showNotification("Warning: Missing values in feature column",type = "warning")
    return(NULL)
  }

  # remove commas and replace with periods - excel from EU uses commas
  # remove *** and replace wiith blanks - default for luminex with too few beads
  full_data <- data.frame(lapply(full_data, function(x) {  gsub("[,]", ".", x) }))
  full_data <- data.frame(lapply(full_data, function(x) {  gsub("[*]+", "", x) }))
  table_data <- data.frame(lapply(table_data, function(x) {  gsub("[,]", ".", x) }))
  table_data <- data.frame(lapply(table_data, function(x) {  gsub("[*]+", "", x) }))

  # Formatting header

  header_data <- as.data.frame(full_data[(1:6), , drop = FALSE])
  heads <- as.data.frame(str_split_fixed(header_data[,1], ':', 2))
  heads$V1 <- gsub(" ","_",heads$V1)
  heads$V1 <- gsub("(Volts)","Volts",heads$V1)
  theads <- as.data.frame(setNames(data.frame(t(heads[,-1])), heads[,1]))
  theads$'Plate_ID' <- tools::file_path_sans_ext(basename(heads[[1,2]]))
  names(theads)[] <- tolower(names(theads)[])
  setnames(theads, gsub("\\([^)(]+\\)", "", gsub(" ", "", gsub("rp1_pmt_(volts)", "rp1_pmt_volts", names(theads), fixed = TRUE))))
  theads$acquisition_date <- as.POSIXct(strptime(gsub(",",":",gsub(" ","",theads[["acquisition_date"]])),format='%d-%b-%Y:%H:%M%p'), tz = "EST")
  # theads$study_accession <- input$readxMap_study_accession
  theads$study_accession <- study_accession
  # theads$experiment_accession <- input$readxMap_experiment_accession
  theads$experiment_accession <- exp_accession
  theads <- theads %>% relocate(study_accession, .before = file_name)
  theads <- theads %>% relocate(experiment_accession, .before = file_name)
  theads <- theads

  # Format table data
  # table_data <<- table_data
  names(table_data)[] <- tolower(names(clean_names(table_data)[]))
  setnames(table_data, gsub("\\([^)(]+\\)", "", gsub(" ", "", gsub("x_agg_beads", "pctaggbeads", gsub("sampling_errors", "samplingerrors", names(table_data), fixed = TRUE)))))

  table_data$type <- factor(table_data$type)
  table_data$pctaggbeads <- as.numeric(table_data$pctaggbeads)
  table_data <- table_data

  # start of import_lumifile script
  table_data <- table_data %>% separate_wider_position(type, c(stype = 1, sampleid = 2), too_few = "align_start")
  table_data$stype <- as.factor(table_data$stype)

  # Filter for type
  table_data %>%
    filter(stype == type) -> table_data

  # Defining new columns to the subset
  if(type == "X"){
    new_cols <- c("dilution", "group_name", "timepoint", "patient_id","feature")
  }else{
    new_cols <- c("source", "dilution","feature")
  }

  # Keeping only the new relevant columns
  new_cols_df <- new_col_df[, new_cols]

  # Bind new columns to the table
  table_data %>%
    cbind(new_cols_df) -> table_data

  long_table_data <- pivot_longer(table_data,
                                  cols = -c("well", "stype","sampleid", "description", "pctaggbeads", "samplingerrors", new_cols)) %>%
    dplyr::rename(antigen = name,
           mfi_n = value)

  long_table_data <- long_table_data %>% separate_wider_delim(mfi_n, " (", names = c("antibody_mfi", "antibody_nx"), cols_remove = TRUE, too_few = "align_start", too_many = "merge")
  long_table_data$antibody_n <- gsub(")","",long_table_data$antibody_nx)
  long_table_data$antibody_mfi <- as.numeric(long_table_data$antibody_mfi)
  long_table_data$antibody_n <- as.numeric(long_table_data$antibody_n)
  long_table_data <- long_table_data[,!(names(long_table_data) %in% c("antibody_nx"))]

  long_table_data$description <- gsub("[[:space:]]","-",long_table_data$description)
  long_table_data$description <- gsub("-+","-",long_table_data$description)
  long_table_data$description <- gsub("_-","_",long_table_data$description)

  # long_table_data$study_accession <- input$readxMap_study_accession
  long_table_data$study_accession <- study_accession

  # long_table_data$experiment_accession <- input$readxMap_experiment_accession
  long_table_data$experiment_accession <- exp_accession

  # long_table_data$feature <- feature

  long_table_data$plate_id <- tools::file_path_sans_ext(basename(heads[[1,2]]))
  # long_table_data <- long_table_data %>% relocate(study_accession, .before=well)
  # long_table_data <- long_table_data %>% relocate(experiment_accession, .before=well)
  # long_table_data <- long_table_data %>% relocate(plate_id, .before=well)

  if(type == "X"){

    # sample_data <<- long_table_data %>% separate_wider_delim(description, "_", names = c("id_imi", "agroup", "biosample", "dilut"), cols_remove = TRUE, too_few = "align_start", too_many = "merge")
    # sample_data$dilut <- str_replace(sample_data[["dilut"]], "1\\\\", "QQ")
    # sample_data$dilution <- as.numeric(gsub('\\D+', '', gsub(',.*', '', sample_data[["dilut"]])))
    # sample_data$timeperiod <- sapply(str_extract_all(sample_data$biosample, "(?<=\\()[^)(]+(?=\\))"), paste0, collapse =",")
    # sample_data$patientid <- gsub("\\([^)(]+\\)", "", sample_data$biosample)
    # sample_data$patientid <- gsub("-","",sample_data$patientid)
    # sample_data = sample_data[,!(names(sample_data) %in% c("biosample"))]

    # sample_data = sample_data[,!(names(sample_data) %in% c("dilut"))]
    sample_data <- long_table_data
    sample_data$id_imi <- "TEST"
    sample_data$dilution <- long_table_data$dilution
    sample_data$timeperiod <- long_table_data$timepoint
    sample_data$patientid <- long_table_data$patient_id
    sample_data$agroup <- long_table_data$group_name

    # sample_data <- sample_data %>% relocate(timeperiod, .before=well)
    # sample_data <- sample_data %>% relocate(patientid, .before=well)

    # Order of cols in the db - Does not have "xmap_sample_id" and "antibody_name"
    col_order <- c("study_accession", "experiment_accession", "plate_id", "timeperiod","patientid", "well", "stype", "sampleid", "id_imi", "agroup", "dilution", "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi",
                   "antibody_n","feature")

    sample_data %>%
      select(all_of(col_order)) -> sample_data

    return(sample_data)
  }

  if(type == "S"){

    # Do we need this line below ?
    # standard_data <<- long_table_data %>% separate_wider_delim(description, "_", names = c("source", "dilut"), cols_remove = TRUE, too_few = "align_start", too_many = "merge")

    standard_data <- long_table_data
    standard_data$dilution <- as.numeric(gsub('[^0-9.]+', '', long_table_data[["dilution"]]))
    # standard_data = standard_data[,!(names(standard_data) %in% c("dilut"))]

    # Order of columns in db - This does not contain "xmap_standard_id" and "antibody_name"
    col_order <- c("study_accession", "experiment_accession", "plate_id", "well", "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi",
                   "antibody_n", "feature")

    standard_data %>%
      select(all_of(col_order)) -> standard_data

    return(standard_data)

  }

  if(type == "C"){

    control_data <- long_table_data
    control_data$dilution <- as.numeric(gsub('[^0-9.]+', '', long_table_data[["dilution"]]))
    # standard_data = standard_data[,!(names(standard_data) %in% c("dilut"))]

    # Order of columns in db - This does not contain "xmap_standard_id" and "antibody_name"
    col_order <- c("study_accession", "experiment_accession", "plate_id", "well", "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi",
                   "antibody_n","feature")

    control_data %>%
      select(all_of(col_order)) -> control_data

    return(control_data)

  }

  if(type == "B"){

    buffer_data <- long_table_data
    buffer_data$dilution <- as.numeric(gsub('[^0-9.]+', '', long_table_data[["dilution"]]))
    # buffer_data$dilution <- as.numeric(gsub('\\D+', '', gsub(',.*', '', long_table_data[["dilution"]])))
    # standard_data = standard_data[,!(names(standard_data) %in% c("dilut"))]

    # Order of columns in db - This does not contain "xmap_standard_id" and "antibody_name"
    col_order <- c("study_accession", "experiment_accession", "plate_id", "well", "stype", "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi","antibody_n","dilution", "feature")

    buffer_data %>%
      select(all_of(col_order)) -> buffer_data

    return(buffer_data)

  }

}

map_type_db <- function(type, df){

  # Function to convert datatype of df to corresponding datatype in db

  if(type == "Sample"){
    table_name_db <- "xmap_sample"
  } else if(type == "Standard"){
    table_name_db <- "xmap_standard"
  } else if(type == "Buffer"){
    table_name_db <- "xmap_buffer"
  } else if(type == "Control"){
    table_name_db <- "xmap_control"
  } else if(type == "Standard_fits"){
    table_name_db <- "xmap_standard_fits"
  } else if(type == "Standard_fit_tab"){
    table_name_db <- "xmap_standard_fit_tab"
  } else if(type == "Standard_preds"){
    table_name_db <- "xmap_standard_preds"
  } else if(type == "Standard_stor"){
    table_name_db <- "xmap_standard_stor"
  } else if(type == "Header"){
    table_name_db <- "xmap_header"
  } else{
    return(NULL)
  }

  # Fetch the datatypes from the db
  update_db(schema = "madi_results", table_name = table_name_db, help = T) %>%
    select(column_name, data_type) -> data_types

  # Create function to map database types to R types
  map_data_types <- function(pg_type) {
    switch(pg_type,
           "integer" = "integer",
           "bigint" = "integer64",
           "numeric" = "numeric",
           "real" = "numeric",
           "double precision" = "numeric",
           "character varying" = "character",
           "text" = "character",
           "boolean" = "logical",
           "date" = "Date",
           "timestamp" = "POSIXct",
           # Add more mappings as needed
           "character") # default type
  }

  # Create a vector of the mapped db types to R types
  mapped_types <- map_chr(data_types$data_type, map_data_types)

  # Remove the first default standard ID column
  mapped_types <- mapped_types[-1]

  # Set names of mapped types to the colnames of the df
  names(mapped_types) <- colnames(df)

  # Take each column and convert to respective types
  for (col_name in names(df)) {
    col_type <- mapped_types[col_name]
    conversion_function <- get(paste("as", col_type, sep = ".")) # Get the correct conversion function
    df[[col_name]] <- conversion_function(df[[col_name]]) # Convert column in place

    if(col_type %in% c("numeric", "integer")){
      df[[col_name]]
    }
  }

  return(df)

}

create_ui_antigen <- function(antigen = NULL, sample_antigen = NULL, standard_antigen = NULL, min_max = c(0,500)){
  sample_slider_id <- paste0("sample_slider_",antigen)
 print(paste("create_ui_antigen _ sample slider: ", sample_slider_id))
  full_ui <- fluidRow(
          br(),
          column(1,
                 fluidRow(
                   numericInput(paste0("sample_slider_upper_textinput_", antigen), "Upper Limit",
                                min = 0,
                                step = 1,
                                max = round_any(max(sample_antigen$mfi), 500, f = ceiling),
                                value = as.numeric(min_max[2])
                   ),
                   noUiSliderInput(
                     inputId =  sample_slider_id, label = "Limits",
                     min = 0, max = round_any(max(sample_antigen$mfi), 500, f = ceiling), step = 1,
                     value = min_max, margin = 1,
                     orientation = "vertical",
                     direction = "rtl",
                     width = "100px", height = "200px"
                   ),
                   numericInput(paste0("sample_slider_lower_textinput_", antigen), "Lower Limit",
                                min = 0,
                                step = 1,
                                max = round_any(max(sample_antigen$mfi), 500, f = ceiling),
                                value = min_max[1]
                   )
                 )
          ),
          column(2,
                 fluidRow(
                         plotlyOutput(paste0("sample_plot_",antigen)),
                         br(),
                         actionButton(paste0("update_gate_class_",antigen), "Save Sample Limits")
                          )
          )
          ,
          column(2, fluidRow(
                              plotOutput(paste0("density_plot_", antigen))
                            )
          ),
          column(3, fluidRow(
                              plotOutput(paste0("buffer_control_plot_", antigen))
                            )
          ),
          column(4, fluidRow(
                 conditionalPanel(
                   condition = "!is.null(standard_antigen)",
                   plotOutput(paste0("standard_plot_", antigen)),

                   # selectInput(paste0("standard_source_si_",antigen),
                   #             "Choose Standard Curve Source",
                   #             choices <- c(unique(standard_antigen$source)),
                   #             # selected = "Click here",
                   #             multiple = FALSE
                   # )

                 )
            )
          )

        # ,
        #   column(1, ""),
        #   column(3, gt_output(outputId = paste0("below_sample_ref_summ_", antigen)), br(), plotOutput(paste0("below_sample_ref_plot_", antigen))),
        #   column(3, gt_output(outputId = paste0("between_sample_ref_summ_", antigen)), br(), plotOutput(paste0("between_sample_ref_plot_", antigen))),
        #   column(5, gt_output(outputId = paste0("above_sample_ref_summ_", antigen)), br(), plotOutput(paste0("above_sample_ref_plot_", antigen)))
  )

  do.call(tagList, full_ui)

}

create_ui_output_antigen <- function(antigen = NULL, sample_antigen = NULL, standard_antigen = NULL, min_max = NULL) {
  output_name <- paste0("ui_", antigen)
  print(paste("create_ui_output_antigen _ output_name: ", output_name))
  output[[output_name]] <- renderUI({
    create_ui_antigen(antigen = antigen, sample_antigen = sample_antigen, standard_antigen = standard_antigen, min_max = min_max)
  })
}

create_dilution_qc_ui_antigen <- function(antigen, plat_sample_dilution_qc){

  sample_dilution_qc_table_id <- paste0("sample_dilution_qc_table_",antigen)
  refresh_button_id <- paste0("dilution_qc_refresh_",antigen)
  select_dilution_db <- paste0("dilution_qc_upload_db_",antigen)
  standardize_dilution <- paste0("standardize_dilution_",antigen)

  boxplot_ui <- fluidRow(
    column(4,
           # noUiSliderInput(
           #   inputId =  sample_dilution_qc_slider_id, label = "Limits",
           #   min = 0, max = round_any(max(plat_sample_dilution_qc[plat_sample_dilution_qc$antigen == antigen,]$antibody_mfi), 500, f = ceiling), step = 1,
           #   value = c(quantile(plat_sample_dilution_qc[plat_sample_dilution_qc$antigen == antigen,]$antibody_mfi)[2], quantile(plat_sample_dilution_qc[plat_sample_dilution_qc$antigen == antigen,]$antibody_mfi)[4]), margin = 1,
           #   orientation = "vertical",
           #   direction = "rtl",
           #   width = "100px", height = "345px"
           # )
           fluidRow(
             rHandsontableOutput(sample_dilution_qc_table_id)
           ),
           fluidRow(
             actionButton(refresh_button_id, label = "Refresh"),
             br(),
             actionButton(standardize_dilution, "Standardize"),
             actionButton(select_dilution_db, label = "Update gate class")
           )),
    column(8,
           plotOutput(paste0("sample_dilution_qc_boxplot_", antigen)))

  )

}

generate_header <- function(path, study_accession, exp_accession, auth0_name){

  full_data <- readxl::read_excel(path, col_names = FALSE, skip = 0)

  header_data <- as.data.frame(full_data[(1:6), , drop = FALSE])
  heads <- as.data.frame(str_split_fixed(header_data[,1], ':', 2))
  heads$V1 <- gsub(" ","_",heads$V1)
  heads$V1 <- gsub("(Volts)","Volts",heads$V1)
  theads <- as.data.frame(setNames(data.frame(t(heads[,-1])), heads[,1]))
  theads$'Plate_ID' <- tools::file_path_sans_ext(basename(heads[[1,2]]))
  names(theads)[] <- tolower(names(theads)[])
  setnames(theads, gsub("\\([^)(]+\\)", "", gsub(" ", "", gsub("rp1_pmt_(volts)", "rp1_pmt_volts", names(theads), fixed = TRUE))))
  theads$acquisition_date <- as.POSIXct(strptime(gsub(",",":",gsub(" ","",theads[["acquisition_date"]])),format='%d-%b-%Y:%H:%M%p'), tz = "EST")

  theads$study_accession <- study_accession
  theads$experiment_accession <- exp_accession
  theads <- theads %>% relocate(study_accession, .before = file_name)
  theads <- theads %>% relocate(experiment_accession, .before = file_name)

  theads$auth0_user <- auth0_name

  return(theads)

}

# Function to process 'sample' column
# Function to process the dataframe and create 'type' column
process_samples_with_class <- function(df) {

  # ----example ----
  # df <- data.frame(sample = c("X1", "X2", "Unknown1", "S1", "S2", "Unknown3", "Unknown4", "B1", "B2", "B3", "Unknown"),
  #                  class = c("X", "X", "X", "S", "S", "S", "X", "B", "B", "B", "B"))

  # Process the dataframe
  # df_processed <- process_samples_with_class(df)
  # ---- example ----

  # Initialize 'type' column
  df$type <- NA

  # Initialize a list to keep track of the max number for each class
  max_nums <- list()

  # Iterate over rows of the dataframe
  for(i in 1:nrow(df)) {
    current_class <- df$class[i]
    sample <- df$sample[i]

    # Check if the sample is known or unknown
    if(grepl("^Unknown", sample)) {
      # If unknown, increment the max number for the current class
      if(!is.null(max_nums[[current_class]])) {
        max_nums[[current_class]] <- max_nums[[current_class]] + 1
      } else {
        # If it's the first time this class is encountered, start with 1
        max_nums[[current_class]] <- 1
      }
      # Assign the new type
      df$type[i] <- paste0(current_class, max_nums[[current_class]])
    } else {
      # If known, directly use the sample as type
      df$type[i] <- sample
      # Update the max number for the current class, if needed
      num <- as.numeric(gsub("[^0-9]", "", sample))
      if(is.null(max_nums[[current_class]]) || num > max_nums[[current_class]]) {
        max_nums[[current_class]] <- num
      }
    }
  }

  return(df)
}

calculate_x_min_max_dynamic <- function(df, dilution_col = "Dilution", span_ratio = 0.25) {

  # Function to create geom_hlines for dilution qc

  # Calculate median of the dilution values
  median_dilution <- median(as.numeric(df[[dilution_col]]), na.rm = TRUE)

  # Define the span as a ratio of the median dilution value
  span <- median_dilution * span_ratio

  # Calculate half of the span
  half_span <- span / 2

  # Calculate x_min and x_max and add them to the dataframe
  df$x_min <- as.numeric(df[[dilution_col]]) - half_span
  df$x_max <- as.numeric(df[[dilution_col]]) + half_span

  return(df)
}

welsch.weight <- function(x, c=2.111){
  w = exp( -(x/c)^2 / 2 )
  return(w)
}

logist.predict <- function(params,newdils){
  preds <- data.frame()
  i <- 1
  for (i in 1:nrow(params)) {
    newdils$study_accession <- params[i,"study_accession"]
    newdils$experiment_accession <- params[i,"experiment_accession"]
    newdils$plateid <- params[i,"plateid"]
    newdils$source <- params[i,"source"]
    newdils$antigen<- params[i,"antigen"]
    newdils$fitted <- params[i,"r_asy"] + (params[i,"l_asy"]-params[i,"r_asy"]/(1 + exp((newdils$log_dilution-params[i,"x_mid"])/params[i,"scale"])))
    preds <- rbind(preds,newdils)
    }

  return(preds)
}

compute_robust_curves <- function(dat, antigen, plate, study_accession, experiment_accession, ssource){

  # print(dat[ , c("mfi","log_dilution")])
  # lymphocytes clump together at high concentrations dragging down the mfi values is the overall dilution is not high enough
  # created a fix based on conversation with Project 3.
  dat$orig <- dat$mfi
  dat$antigen <- NA
  dat$antigen <- antigen
  # 1. identify the highest mfi and corresponding log_dilution
  max_mfi <- max(dat$mfi)
  logd_at_max_mfi <- max(dat[dat$mfi==max_mfi, ]$log_dilution)
  # 2. identify the mfis lower than the max_mfi at higher concentrations
  dat[dat$log_dilution > logd_at_max_mfi, ]$mfi <- max_mfi - ((max_mfi - dat[dat$log_dilution > logd_at_max_mfi, ]$mfi)*0.1/((dat[dat$log_dilution > logd_at_max_mfi, ]$log_dilution-logd_at_max_mfi)*2))
  print(dat[ , c("orig","mfi","log_dilution")])

  init1 <- getInitial(mfi ~ SSfpl(log_dilution, l_asy, r_asy, xmid, scal),
                      data = dat,
                      control=nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE))
  print(init1)


  initfit <- nls(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
                 data = dat,
                 start = init1,
                 control = nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE)
  )
  init2 <- coefficients(initfit)
  print(init2)
  #
  # lower_c <- make_lower(init2)
  # upper_c <- make_upper(init2)

  print("above fit")
  print(max(dat$log_dilution))
  tryCatch({
    print("in catch block")
    fit <- robustbase::nlrob(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
                             data = dat
                             , method = "M"
                             , start = init2
                             , na.action = na.exclude
                             , maxit = 500
                             , doCov = TRUE
                             # , weights = welsch_w
                             # , lower = lower_c
                             # , upper = upper_c
                             # , cntrl = c(nlrob.control("CM", psi = "welsh", tuning.chi = 2.11))
                             , tol = 1e-06
                             , control = nls.control(maxiter = 500,
                                                     tol = 1e-06,
                                                     minFactor = 1/2048,
                                                     printEval = FALSE,
                                                     warnOnly = TRUE,
                                                     scaleOffset = 0,
                                                     nDcentral = FALSE)
                             )
    # fit <<- nls(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
    #            data = dat,
    #            start = init2,
    #            control = nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE)
    # )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(fit$status)
  if (!is.na(fit$status)){
    dat$weights <- fit$rweights
  } else {
     dat$weights <- 1.0
  }


  # print(paste0("above predict => ", "n rows in prediction set: ", nrow(newdils)))
  # fitted <- predict(fit, newdils)
  # preds <- as.data.frame(cbind(newdils,fitted))
  # preds$study_accession <- study_accession
  # preds$experiment_accession <- experiment_accession
  # preds$antigen <- antigen
  # preds$plateid <- plate
  # preds$source <- ssource
  # preds <- as.data.frame(preds)

  print("above glance")
  iter <- c(as.numeric(fit$iter))
  status <- c(fit$status)
  l_asy <- c(as.numeric(fit$coefficients[1]))
  r_asy <- c(as.numeric(fit$coefficients[2]))
  x_mid <- c(as.numeric(fit$coefficients[3]))
  scale <- c(as.numeric(fit$coefficients[4]))
  sigma <- c(as.numeric(fit$coefficients[5]))

  # iter <- 1
  # status <- fit$convInfo$stopMessage
  # l_asy <- c(as.numeric(fit$m$getPars()[1]))
  # r_asy <- c(as.numeric(fit$m$getPars()[2]))
  # x_mid <- c(as.numeric(fit$m$getPars()[3]))
  # scale <- c(as.numeric(fit$m$getPars()[4]))

  coef_a <- as.numeric(fit$m$getPars()[1])
  coef_d <- as.numeric(fit$m$getPars()[2])
  coef_k <- 4.6805
  bendlower <- round(((coef_a - coef_d) / (1 + (1/coef_k))) + coef_d,0)
  bendupper <- round(((coef_a - coef_d) / (1 + coef_k)) + coef_d,0)
  llod <- round(predictNLS(fit,newdata = data.frame(log_dilution = -10), interval = "confidence")$summary$`Sim.97.5%`,0)
  ulod <- round(predictNLS(fit,newdata = data.frame(log_dilution = 0), interval = "confidence")$summary$`Sim.2.5%`,0)
  rsquare_fit <- round(modelr::rsquare(fit,augment(fit)),3)
  glance_fit <- data.frame(study_accession = study_accession,
                           experiment_accession=experiment_accession,
                           plateid = plate,
                           antigen = antigen,
                           iter = iter,
                           status = status,
                           l_asy, r_asy, x_mid, scale,
                           bendlower, bendupper, llod, ulod,
                           as.data.frame(glance(fit)[4:9]),
                           rsquare_fit)
  glance_fit$source <- unique(dat$source)
  # print(names(glance_fit))
  print("after glance is created")

  fit_tab <- rounddf(as.data.frame(broom::tidy(fit)[,c(1:3,5)]), digits = 2, pad = TRUE)
  fit_tab$signif <- stars.pval(c(as.numeric(fit_tab$p.value)))
  fit_tab <- fit_tab[ , c(1:3,5)]
  fit_tab$study_accession <- study_accession
  fit_tab$experiment_accession <- experiment_accession
  fit_tab$antigen <- antigen
  fit_tab$plateid <- plate
  fit_tab$source <- ssource
  names(fit_tab)[names(fit_tab) == "std.error"] <- "std_error"
  fit_tab <- as.list(fit_tab[, c(5:8,1:4,9)])
  # print(names(fit_tab))
  print("after fit_tab is created")

  names(dat)[names(dat) == "n"] <- "nbeads"
  dat$plate_id <- dat$plateid
  print(paste("dat$antigen",antigen))
  # print(names(dat))
  # print(dat)
  dat_stor <- dat[ , c("study_accession", "experiment_accession", "plate_id","stype", "antigen", "mfi",
                       "nbeads","feature","plateid", "dilution", "source", "log_dilution","weights")]
  print("above return")
  # print(names(dat_stor))
  # newlist <- list(fit_tab,glance_fit,preds,dat_stor)
  newlist <- list(fit_tab,glance_fit,dat_stor)
  return(newlist)
}


compute_allplate_robust_curves <- function(dat, antigen, study_accession, experiment_accession, ssource){

  # print(dat[ , c("mfi","log_dilution")])
  # lymphocytes clump together at high concentrations dragging down the mfi values is the overall dilution is not high enough
  # created a fix based on conversation with Project 3.
  dat$orig <- dat$mfi
  dat$antigen <- NA
  dat$antigen <- antigen
  plate <- paste(experiment_accession,"all_plates",sep = ".")
  # 1. identify the highest mfi and corresponding log_dilution
  max_mfi <- max(dat$mfi)
  logd_at_max_mfi <- max(dat[dat$mfi==max_mfi, ]$log_dilution)
  # 2. identify the mfis lower than the max_mfi at higher concentrations
  dat[dat$log_dilution > logd_at_max_mfi, ]$mfi <- max_mfi - ((max_mfi - dat[dat$log_dilution > logd_at_max_mfi, ]$mfi)*0.1/((dat[dat$log_dilution > logd_at_max_mfi, ]$log_dilution-logd_at_max_mfi)*2))
  print(dat[ , c("orig","mfi","log_dilution")])

  init1 <- getInitial(mfi ~ SSfpl(log_dilution, l_asy, r_asy, xmid, scal),
                      data = dat,
                      control=nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE))
  print(init1)


  initfit <- nls(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
                 data = dat,
                 start = init1,
                 control = nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE)
  )
  init2 <- coefficients(initfit)
  print(init2)
  #
  # lower_c <- make_lower(init2)
  # upper_c <- make_upper(init2)

  print("above fit")
  print(max(dat$log_dilution))
  tryCatch({
    print("in catch block")
    fit <- robustbase::nlrob(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
                             data = dat
                             , method = "M"
                             , start = init2
                             , na.action = na.exclude
                             , maxit = 500
                             , doCov = TRUE
                             # , weights = welsch_w
                             # , lower = lower_c
                             # , upper = upper_c
                             # , cntrl = c(nlrob.control("CM", psi = "welsh", tuning.chi = 2.11))
                             , tol = 1e-06
                             , control = nls.control(maxiter = 500,
                                                     tol = 1e-06,
                                                     minFactor = 1/2048,
                                                     printEval = FALSE,
                                                     warnOnly = TRUE,
                                                     scaleOffset = 0,
                                                     nDcentral = FALSE)
    )
    # fit <<- nls(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
    #            data = dat,
    #            start = init2,
    #            control = nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE)
    # )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(fit$status)
  if (!is.na(fit$status)){
    dat$weights <- fit$rweights
  } else {
    dat$weights <- 1.0
  }

  print("above glance")
  iter <- c(as.numeric(fit$iter))
  status <- c(fit$status)
  l_asy <- c(as.numeric(fit$coefficients[1]))
  r_asy <- c(as.numeric(fit$coefficients[2]))
  x_mid <- c(as.numeric(fit$coefficients[3]))
  scale <- c(as.numeric(fit$coefficients[4]))
  sigma <- c(as.numeric(fit$coefficients[5]))

  # iter <- 1
  # status <- fit$convInfo$stopMessage
  # l_asy <- c(as.numeric(fit$m$getPars()[1]))
  # r_asy <- c(as.numeric(fit$m$getPars()[2]))
  # x_mid <- c(as.numeric(fit$m$getPars()[3]))
  # scale <- c(as.numeric(fit$m$getPars()[4]))

  coef_a <- as.numeric(fit$m$getPars()[1])
  coef_d <- as.numeric(fit$m$getPars()[2])
  coef_k <- 4.6805
  bendlower <- round(((coef_a - coef_d) / (1 + (1/coef_k))) + coef_d,0)
  bendupper <- round(((coef_a - coef_d) / (1 + coef_k)) + coef_d,0)
  llod <- round(predictNLS(fit,newdata = data.frame(log_dilution = -10), interval = "confidence")$summary$`Sim.97.5%`,0)
  ulod <- round(predictNLS(fit,newdata = data.frame(log_dilution = 0), interval = "confidence")$summary$`Sim.2.5%`,0)
  rsquare_fit <- round(modelr::rsquare(fit,augment(fit)),3)

  glance_fit <- data.frame(study_accession = study_accession,
                           experiment_accession=experiment_accession,
                           plateid = plate,
                           antigen = antigen,
                           iter = iter,
                           status = status,
                           l_asy, r_asy, x_mid, scale,
                           bendlower, bendupper, llod, ulod,
                           as.data.frame(glance(fit)[4:9]),
                           rsquare_fit)
  glance_fit$source <- unique(dat$source)
  # print(names(glance_fit))
  print("after glance is created")

  fit_tab <- rounddf(as.data.frame(broom::tidy(fit)[,c(1:3,5)]), digits = 2, pad = TRUE)
  fit_tab$signif <- stars.pval(c(as.numeric(fit_tab$p.value)))
  fit_tab <- fit_tab[ , c(1:3,5)]
  fit_tab$study_accession <- study_accession
  fit_tab$experiment_accession <- experiment_accession
  fit_tab$antigen <- antigen
  fit_tab$plateid <- plate
  fit_tab$source <- ssource
  names(fit_tab)[names(fit_tab) == "std.error"] <- "std_error"
  fit_tab <- as.list(fit_tab[, c(5:8,1:4,9)])
  # print(names(fit_tab))
  print("after fit_tab is created")

  newdils <- data.frame(log_dilution = seq(from = -10, to = 0, by = 0.05))
  print(paste0("above predict => ", "n rows in prediction set: ", nrow(newdils)))
  fitted <- predict(fit, newdils)
  preds <- as.data.frame(cbind(newdils,fitted))
  preds$study_accession <- study_accession
  preds$experiment_accession <- experiment_accession
  preds$antigen <- antigen
  preds$plateid <- plate
  preds$source <- ssource
  preds <- as.data.frame(preds)

  names(dat)[names(dat) == "n"] <- "nbeads"
  dat$plate_id <- plate
  print(paste("dat$antigen",antigen))
  # print(names(dat))
  # print(dat)
  dat_stor <- dat[ , c("study_accession", "experiment_accession", "plate_id","stype", "antigen", "mfi",
                       "nbeads","feature","plateid", "dilution", "source", "log_dilution","weights")]
  print("above return")
  # print(names(dat_stor))
  # newlist <- list(fit_tab,glance_fit,preds,dat_stor)
  newlist <- list(fit_tab,glance_fit,dat_stor,preds)
  return(newlist)
} # end allplates robust curves


## Download dataset from plots
createDownloadPlotData <- function(data_component, file_suffix) {
  downloadHandler(
    filename = function() {
      paste(input$readxMap_study_accession, input$readxMap_experiment_accession, file_suffix, ".csv", sep = "_")
    },
    content = function(file) {
      # download data component (data frame)
      write.csv(data_component, file)
    }
  )
}


# create_ui_curves <- function(antigen, standard_curve){
#   full_ui <- fluidRow(
#     fluidRow(
#       # column(3,
#       #        br(),
#       #        # Add your sidebar content here
#       #        h4("Sidebar Panel"),
#       #        selectInput("transform_mfi", "Transform MFI", choices = c("MFI","log_MFI"))
#       # ),
#       column(12,
#              # add_busy_spinner(spin = "circle",
#              #                  color = "#FB6A4A",
#              #                  timeout = 100,
#              #                  position = c("bottom-left"),
#              #                  onstart = TRUE,
#              #                  margins = c(10, 10),
#              #                  height = "150px",
#              #                  width = "150px"),
#              # Add your main panel content here
#              br(),
#              plotOutput(paste0("standard_curves_", antigen))
#       )
#     )
#   )
#
#   do.call(tagList, full_ui)
#
# }

# create_ui_output_curves <- function(antigen, standard_curve) {
#   output_name <- paste0("ui_curv_", antigen)
#   output[[output_name]] <- renderUI({
#     create_ui_curves(antigen, standard_curve)
#   })
# }

# compute_curves <- function(dat){
#   init1 <- getInitial(MFI ~ SSfpl(log_dilution, l_asy, r_asy, xmid, scal),
#                       data = dat,
#                       control=nls.control(maxiter = 150, minFactor=1/2048, warnOnly=TRUE))
#   print(init1)
#   fit <- nls(MFI ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
#              data = dat,
#              start = init1,
#              control = nls.control(maxiter = 150, minFactor=1/10240, warnOnly=TRUE)
#   )
#
#   log_dilution <- seq(from = min_dilute, to = max_dilute, by = 0.1)
#   newdils <- as.data.frame(log_dilution)
#   fitted <- predict(fit, newdils)
#   preds <- cbind(newdils,fitted)
#
#   newlist <- list(fit,preds)
#   return(newlist)
# }
#
# make_lower <- function(init){
#   lower_c <- init
#   for (i in 1:length(init)) {
#     if(init[i] > 0) {
#       print("Positive number")
#       lower_c[i] <- init[i] / 5
#     } else {
#       if(init[i] == 0) {
#         print("Zero")
#         lower_c[i] <- -5
#       } else {
#         print("Negative number")
#         lower_c[i] <- init[i] * 5
#       }
#     }
#   }
#   lower_c <- c(lower_c,sigma = 0)
#   return(lower_c)
# }
#
#
# make_upper <- function(init){
#   upper_c <- init
#   for (i in 1:length(init)) {
#     if(init[i] > 0) {
#       print("Positive number")
#       upper_c[i] <- init[i] * 5
#     } else {
#       if(init[i] == 0) {
#         print("Zero")
#         upper_c[i] <- 5
#       } else {
#         print("Negative number")
#         upper_c[i] <- init[i] / 5
#       }
#     }
#   }
#   upper_c <- c(upper_c,sigma = 1000)
#   return(upper_c)
# }
