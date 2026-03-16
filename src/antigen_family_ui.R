tags$head(
  tags$style(HTML("
    .datatables {
      width: 100% !important;
      margin: 0 !important;
    }
    .dataTables_wrapper {
      width: 100%;
      margin: 0 auto;
      padding: 0 10px;
    }
    .panel-collapse {
      padding: 15px;
    }
  "))
)

standard_curve_table_list <- list()

data_summary <- list()

std_curve_tab_active <- reactiveVal(FALSE)


fetch_antigen_family_table <- function(study_accession, project_id, experiment_accession = NULL, default_family = "All Antigens") {
  
  q_study   <- DBI::dbQuoteString(conn, study_accession)
  q_project <- as.integer(project_id)
  
  # Normalize placeholder
  if (is.null(experiment_accession) || experiment_accession == "Click here") {
    experiment_accession <- NULL
  }
  
  # Base WHERE clause
  where_clause <- paste0("
      study_accession = ", q_study, "
      AND project_id = ", q_project, "
      AND l_asy_constraint_method IS NOT NULL
  ")
  
  # Add experiment filter only if provided
  if (!is.null(experiment_accession) && nzchar(experiment_accession)) {
    q_exp <- DBI::dbQuoteString(conn, experiment_accession)
    where_clause <- paste0(where_clause, "
      AND experiment_accession = ", q_exp)
  }
  
  select_query <- paste0("
    SELECT xmap_antigen_family_id, study_accession, project_id, experiment_accession,
           antigen, feature, antigen_family, standard_curve_concentration,
           antigen_name, virus_bacterial_strain, antigen_source, catalog_number,
           l_asy_min_constraint, l_asy_max_constraint, l_asy_constraint_method,
           model_form_list, pcov_threshold
    FROM madi_results.xmap_antigen_family
    WHERE ", where_clause, ";
  ")
  
  print(select_query)
  
  result_df <- DBI::dbGetQuery(conn, select_query)
  
  # Backfill missing antigen_family
  if (nrow(result_df) > 0 && "antigen_family" %in% names(result_df)) {
    na_or_empty <- is.na(result_df$antigen_family) |
      trimws(as.character(result_df$antigen_family)) == ""
    
    if (any(na_or_empty)) {
      result_df$antigen_family[na_or_empty] <- default_family
    }
  }
  
  return(result_df)
}
# fetch_antigen_family_table <- function(study_accession, project_id, experiment_accession, default_family = "All Antigens") {
#   # if (study_accession == "reset") {
#   #   return(NULL)
#   # }
#   # 
#   # # Quoted values for safe SQL interpolation
#   q_study   <- dbQuoteString(conn, study_accession)
#   q_exp     <- dbQuoteString(conn, experiment_accession)
#   q_family  <- dbQuoteString(conn, default_family)
#   q_project <- as.integer(project_id)
#   
#   print("Q_experiment")
#   print(q_exp)
#   # 
#   # # --- Step 1: Insert any antigens from xmap_sample that are missing in xmap_antigen_family ---
#   # # This handles BOTH the "no rows at all" and "some rows missing" cases in one pass.
#   # insert_query <- paste0("
#   #   INSERT INTO madi_results.xmap_antigen_family
#   #     (study_accession, project_id, experiment_accession, antigen, feature, antigen_family, standard_curve_concentration)
#   #   SELECT DISTINCT
#   #     s.study_accession,
#   #     ", q_project, ",
#   #     ", q_exp, ",
#   #     s.antigen,
#   #     s.feature, 
#   #     ", q_family, ",
#   #     10000
#   #   FROM madi_results.xmap_sample s
#   #   WHERE s.study_accession = ", q_study, "
#   #     AND NOT EXISTS (
#   #       SELECT 1
#   #       FROM madi_results.xmap_antigen_family f
#   #       WHERE f.study_accession = s.study_accession
#   #         AND f.project_id = ", q_project, "
#   #         AND f.experiment_accession = ", q_exp, "
#   #         AND f.antigen = s.antigen
#   #         AND f.feature = s.feature
#   #     );")
#   # 
#   # dbExecute(conn, insert_query)
#   # 
#   # --- Step 2: Fetch the full result set (always exactly one query) ---
#   select_query <- paste0("
#     SELECT xmap_antigen_family_id, study_accession, project_id, experiment_accession,
#            antigen, feature, antigen_family, standard_curve_concentration,
#            antigen_name, virus_bacterial_strain, antigen_source, catalog_number,
#            l_asy_min_constraint, l_asy_max_constraint, l_asy_constraint_method, model_form_list,
#            pcov_threshold
#     FROM madi_results.xmap_antigen_family
#     WHERE study_accession = ", q_study, "
#       AND project_id = ", q_project, "
#       AND experiment_accession = ", q_exp, "
#       AND l_asy_constraint_method is not NULL;")
#   
#   result_df <- dbGetQuery(conn, select_query)
#   
#   # --- Step 3: Backfill empty/NA antigen_family values in the returned data ---
#   if (nrow(result_df) > 0 && "antigen_family" %in% names(result_df)) {
#     na_or_empty <- is.na(result_df$antigen_family) |
#       trimws(as.character(result_df$antigen_family)) == ""
#     if (any(na_or_empty)) {
#       result_df$antigen_family[na_or_empty] <- default_family
#     }
#   }
#   
#   return(result_df)
# }




# fetch_antigen_family_table <- function(study_accession, project_id, experiment_accession, default_family = "All Antigens") {
#   if (study_accession == "reset") {
#     result_df <- NULL
#   } else {
#     # Build the base query
#     query <- paste0("
#       SELECT xmap_antigen_family_id, study_accession, project_id, experiment_accession,
#              antigen, antigen_family, standard_curve_concentration,
#              antigen_name, virus_bacterial_strain, antigen_source, catalog_number,
#              l_asy_min_constraint, l_asy_max_constraint, l_asy_constraint_method
#       FROM madi_results.xmap_antigen_family
#       WHERE study_accession = '", study_accession, "'
#         AND project_id = ", project_id)
#     
#     # Add experiment_accession filter
#     if (!is.null(experiment_accession) && experiment_accession != "" && experiment_accession != "Click here") {
#       query <- paste0(query, "
#         AND experiment_accession = '", experiment_accession, "'")
#     } else {
#       query <- paste0(query, "
#         AND experiment_accession IS NULL")
#     }
#     
#     result_df <- dbGetQuery(conn, query)
#     
#     if (nrow(result_df) == 0 || is.null(result_df)) {
#       # No rows found — insert template rows
#       if (!is.null(experiment_accession) && experiment_accession != "" && experiment_accession != "Click here") {
#         # Insert for a specific experiment
#         insert_query <- paste0("
#           INSERT INTO madi_results.xmap_antigen_family
#             (study_accession, project_id, experiment_accession, antigen, antigen_family, standard_curve_concentration)
#           SELECT DISTINCT
#             '", study_accession, "',
#             ", project_id, ",
#             '", experiment_accession, "',
#             antigen,
#             '", default_family, "',
#             10000
#           FROM madi_results.xmap_sample
#           WHERE study_accession = '", study_accession, "'
#           ORDER BY antigen;")
#       } else {
#         # Insert template rows (experiment_accession IS NULL)
#         insert_query <- paste0("
#           INSERT INTO madi_results.xmap_antigen_family
#             (study_accession, project_id, antigen, antigen_family, standard_curve_concentration)
#           SELECT DISTINCT
#             '", study_accession, "',
#             ", project_id, ",
#             antigen,
#             '", default_family, "',
#             10000
#           FROM madi_results.xmap_sample
#           WHERE study_accession = '", study_accession, "'
#           ORDER BY antigen;")
#       }
#       dbExecute(conn, insert_query)
#       result_df <- dbGetQuery(conn, query)
#     } else {
#       # Rows exist — add any new antigens that are missing
#       if (!is.null(experiment_accession) && experiment_accession != "" && experiment_accession != "Click here") {
#         insert_query <- paste0("
#           INSERT INTO madi_results.xmap_antigen_family
#             (study_accession, project_id, experiment_accession, antigen, antigen_family, standard_curve_concentration)
#           SELECT DISTINCT
#             s.study_accession,
#             ", project_id, ",
#             '", experiment_accession, "',
#             s.antigen,
#             '", default_family, "',
#             10000
#           FROM madi_results.xmap_sample s
#           WHERE s.study_accession = '", study_accession, "'
#             AND NOT EXISTS (
#               SELECT 1
#               FROM madi_results.xmap_antigen_family f
#               WHERE f.study_accession = s.study_accession
#                 AND f.project_id = ", project_id, "
#                 AND f.experiment_accession = '", experiment_accession, "'
#                 AND f.antigen = s.antigen
#             );")
#       } else {
#         insert_query <- paste0("
#           INSERT INTO madi_results.xmap_antigen_family
#             (study_accession, project_id, antigen, antigen_family, standard_curve_concentration)
#           SELECT DISTINCT
#             s.study_accession,
#             ", project_id, ",
#             s.antigen,
#             '", default_family, "',
#             10000
#           FROM madi_results.xmap_sample s
#           WHERE s.study_accession = '", study_accession, "'
#             AND NOT EXISTS (
#               SELECT 1
#               FROM madi_results.xmap_antigen_family f
#               WHERE f.study_accession = s.study_accession
#                 AND f.project_id = ", project_id, "
#                 AND f.experiment_accession IS NULL
#                 AND f.antigen = s.antigen
#             );")
#       }
#       dbExecute(conn, insert_query)
#       result_df <- dbGetQuery(conn, query)
#     }
#     
#     # Handle NULL or empty antigen_family values
#     if (!is.null(result_df) && nrow(result_df) > 0 && "antigen_family" %in% names(result_df)) {
#       na_or_empty <- is.na(result_df$antigen_family) |
#         result_df$antigen_family == "" |
#         trimws(as.character(result_df$antigen_family)) == ""
#       if (any(na_or_empty)) {
#         result_df$antigen_family[na_or_empty] <- default_family
#       }
#     }
#   }
#   return(result_df)
# }
# fetch_antigen_family_table <- function(study_accession, project_id, default_family = "All Antigens") {
#   if (study_accession == "reset") {
#     result_df <- NULL
#   } else {
#     query <- paste0("
#       SELECT xmap_antigen_family_id, study_accession, project_id, experiment_accession,
#              antigen, antigen_family, standard_curve_concentration,
#              antigen_name, virus_bacterial_strain, antigen_source, catalog_number,
#              l_asy_min_constraint, l_asy_max_constraint, l_asy_constraint_method
#       FROM madi_results.xmap_antigen_family
#       WHERE study_accession = '", study_accession, "'
#         AND project_id = ", project_id)
#     
#     result_df <- dbGetQuery(conn, query)
#     
#     if (nrow(result_df) == 0 || is.null(result_df)) {
#       insert_query <- paste0("
#         INSERT INTO madi_results.xmap_antigen_family
#           (study_accession, project_id, antigen, antigen_family, standard_curve_concentration)
#         SELECT DISTINCT
#           '", study_accession, "',
#           ", project_id, ",
#           antigen,
#           '", default_family, "',
#           10000
#         FROM madi_results.xmap_sample
#         WHERE study_accession = '", study_accession, "'
#         ORDER BY antigen;")
#       dbExecute(conn, insert_query)
#       result_df <- dbGetQuery(conn, query)
#     } else {
#       insert_query <- paste0("
#         INSERT INTO madi_results.xmap_antigen_family
#           (study_accession, project_id, antigen, antigen_family, standard_curve_concentration)
#         SELECT DISTINCT
#           s.study_accession,
#           ", project_id, ",
#           s.antigen,
#           '", default_family, "',
#           10000
#         FROM madi_results.xmap_sample s
#         WHERE s.study_accession = '", study_accession, "'
#           AND NOT EXISTS (
#             SELECT 1
#             FROM madi_results.xmap_antigen_family f
#             WHERE f.study_accession = s.study_accession
#               AND f.project_id = ", project_id, "
#               AND f.antigen = s.antigen
#           );")
#       dbExecute(conn, insert_query)
#       result_df <- dbGetQuery(conn, query)
#     }
#     
#     # Handle NULL or empty antigen_family values
#     if (!is.null(result_df) && nrow(result_df) > 0 && "antigen_family" %in% names(result_df)) {
#       na_or_empty <- is.na(result_df$antigen_family) |
#         result_df$antigen_family == "" |
#         trimws(as.character(result_df$antigen_family)) == ""
#       if (any(na_or_empty)) {
#         result_df$antigen_family[na_or_empty] <- default_family
#       }
#     }
#   }
#   return(result_df)
# }
# fetch_antigen_family_table <- function(study_accession, default_family = "All Antigens") {
#   if (study_accession == "reset") {
#     result_df <- NULL
#   } else {
#     query <- paste0("
#       SELECT xmap_antigen_family_id, study_accession, experiment_accession, antigen, antigen_family, standard_curve_concentration,
#       antigen_name, virus_bacterial_strain, antigen_source, catalog_number, l_asy_min_constraint, l_asy_max_constraint, l_asy_constraint_method
#       FROM madi_results.xmap_antigen_family
#       WHERE study_accession = '", study_accession, "'")
# 
#     # Run the query and fetch the result as a data frame
#     result_df <- dbGetQuery(conn, query)
# 
#     if (nrow(result_df) == 0 || is.null(result_df)) {
#       # check if result df is null or 0 rows and if so run additional query insert.
#       insert_query <- paste0("INSERT INTO madi_results.xmap_antigen_family(
#                 	study_accession, antigen, antigen_family, standard_curve_concentration)
#                 	SELECT DISTINCT '",study_accession,"', antigen, '", default_family, "', 10000
#                 	FROM madi_results.xmap_sample
#                 	WHERE study_accession = '", study_accession, "'
#                 	ORDER BY antigen;")
#       dbExecute(conn, insert_query)
#       result_df <- dbGetQuery(conn, query)
#     } else {
#       #}  ## Add new antigens to table
#       insert_query <- paste0("
#       INSERT INTO madi_results.xmap_antigen_family (study_accession, antigen, antigen_family, standard_curve_concentration)
#       SELECT DISTINCT s.study_accession, s.antigen, '", default_family, "', 10000
#       FROM madi_results.xmap_sample s
#       WHERE s.study_accession = '", study_accession, "'
#         AND NOT EXISTS (
#           SELECT 1 FROM madi_results.xmap_antigen_family f
#           WHERE f.study_accession = s.study_accession
#             AND f.antigen = s.antigen
#         );")
#       dbExecute(conn, insert_query)
# 
#       result_df <- dbGetQuery(conn, query)
#     }
# 
#     # Handle NULL or empty antigen_family values in the result
#     # This catches cases where rows exist but antigen_family was never set or was cleared
#     if (!is.null(result_df) && nrow(result_df) > 0 && "antigen_family" %in% names(result_df)) {
#       na_or_empty <- is.na(result_df$antigen_family) |
#         result_df$antigen_family == "" |
#         trimws(as.character(result_df$antigen_family)) == ""
#       if (any(na_or_empty)) {
#         result_df$antigen_family[na_or_empty] <- default_family
#       }
#     }
#   } # end outer else statement
# 
#   return(result_df)
# }

# fetch_antigen_family_table <- function(study_accession) {
#   if (study_accession == "reset") {
#     result_df <- NULL
#   } else {
#     query <- paste0("
#       SELECT xmap_antigen_family_id, study_accession, experiment_accession, antigen, antigen_family, standard_curve_concentration,
#       antigen_name, virus_bacterial_strain, antigen_source, catalog_number, l_asy_min_constraint, l_asy_max_constraint, l_asy_constraint_method
#       FROM madi_results.xmap_antigen_family
#       WHERE study_accession = '", study_accession, "'")
#
#     # Run the query and fetch the result as a data frame
#     result_df <- dbGetQuery(conn, query)
#
#     if (nrow(result_df) == 0 || is.null(result_df)) {
#       # check if result df is null or 0 rows and if so run additional query insert.
#       insert_query <- paste0("INSERT INTO madi_results.xmap_antigen_family(
#                 	study_accession, antigen, antigen_family, standard_curve_concentration)
#                 	SELECT DISTINCT '",study_accession,"', antigen, 'All Antigens', 10000
#                 	FROM madi_results.xmap_sample
#                 	WHERE study_accession = '", study_accession, "'
#                 	ORDER BY antigen;")
#       dbExecute(conn, insert_query)
#       result_df <- dbGetQuery(conn, query)
#     } else {
#   #}  ## Add new antigens to table
#       insert_query <- paste0("
#       INSERT INTO madi_results.xmap_antigen_family (study_accession, antigen, antigen_family, standard_curve_concentration)
#       SELECT DISTINCT s.study_accession, s.antigen, 'All Antigens', 10000
#       FROM madi_results.xmap_sample s
#       WHERE s.study_accession = '", study_accession, "'
#         AND NOT EXISTS (
#           SELECT 1 FROM madi_results.xmap_antigen_family f
#           WHERE f.study_accession = s.study_accession
#             AND f.antigen = s.antigen
#         );")
#       dbExecute(conn, insert_query)
#
#      result_df <- dbGetQuery(conn, query)
#     }
#   } # end outer else statement
#
#   return(result_df)
# }

# create_antigen_family_rows <- function(study, experiment) {
#   conn <- get_db_connection()
# 
#   # Pull table
#   antigen_families <- DBI::dbGetQuery(conn, "
#     SELECT *
#     FROM madi_results.xmap_antigen_family
#   ")
# 
#   # Force character to avoid factor matching issues
#   antigen_families$experiment_accession <- as.character(antigen_families$experiment_accession)
#   antigen_families$study_accession      <- as.character(antigen_families$study_accession)
# 
#   # Template rows: study exists but experiment NULL to copy
#   template_rows <- antigen_families[
#     antigen_families$study_accession == study &
#       is.na(antigen_families$experiment_accession),
#   ]
# 
#   if (nrow(template_rows) == 0) {
#     message(glue::glue(
#       "No template antigen family rows found for study {study}. Nothing to copy."
#     ))
#     return(invisible(NULL))
#   }
# 
#   # check if study and experiment rows already exist
#   study_exp_rows <- antigen_families[
#     antigen_families$study_accession == study &
#       !is.na(antigen_families$experiment_accession) &
#       antigen_families$experiment_accession == experiment,
#   ]
# 
#   if (nrow(study_exp_rows) > 0) {
#     message(glue::glue(
#       "Antigen family rows already exist for {study}:{experiment}. No action taken."
#     ))
#     return(invisible(NULL))
#   }
# 
#   # Create new experiment rows
#   new_rows <- template_rows
#   new_rows$experiment_accession <- experiment
# 
#   new_rows <- new_rows[ , !(names(new_rows) %in% "xmap_antigen_family_id")]
# 
#   query <- glue::glue_sql("
#     SELECT *
#     FROM madi_results.xmap_antigen_family
#     WHERE study_accession = {study}
#       AND experiment_accession = {experiment}
# ", .con = conn)
# 
#   antigen_families_check_study_experiment <- DBI::dbGetQuery(conn, query)
#   if (nrow(antigen_families_check_study_experiment) == 0) {
#       # Insert into DB
#     DBI::dbAppendTable(
#       conn,
#       DBI::Id(schema = "madi_results", table = "xmap_antigen_family"),
#       new_rows
#     )
# 
#       message(glue::glue(
#         "Created {nrow(new_rows)} antigen family rows for {study}:{experiment} from template."
#       ))
# 
#       invisible(new_rows)
# 
#   } else {
#     message(glue::glue(
#       "No antigen family rows for {study}:{experiment} from template."
#     ))
#   }
# }

create_antigen_family_rows <- function(study, experiment, project_id) {
  conn <- get_db_connection()
  
  antigen_families <- DBI::dbGetQuery(conn, "
    SELECT *
    FROM madi_results.xmap_antigen_family
  ")
  
  antigen_families$experiment_accession <- as.character(antigen_families$experiment_accession)
  antigen_families$study_accession      <- as.character(antigen_families$study_accession)
  antigen_families$project_id           <- as.character(antigen_families$project_id)
  
  # Template rows: match study AND project_id, experiment is NULL
  template_rows <- antigen_families[
    antigen_families$study_accession == study &
      as.character(antigen_families$project_id) == as.character(project_id) &
      is.na(antigen_families$experiment_accession),
  ]
  
  if (nrow(template_rows) == 0) {
    message(glue::glue(
      "No template antigen family rows found for study {study} and project {project_id}. Nothing to copy."
    ))
    return(invisible(NULL))
  }
  
  # Check if rows already exist for this study + experiment + project_id
  query <- glue::glue_sql("
    SELECT *
    FROM madi_results.xmap_antigen_family
    WHERE study_accession = {study}
      AND experiment_accession = {experiment}
      AND project_id = {project_id}
  ", .con = conn)
  existing_rows <- DBI::dbGetQuery(conn, query)
  
  if (nrow(existing_rows) > 0) {
    message(glue::glue(
      "Antigen family rows already exist for {study}:{experiment}:{project_id}. No action taken."
    ))
    return(invisible(NULL))
  }
  
  # Create new rows with correct project_id
  new_rows <- template_rows
  new_rows$experiment_accession <- experiment
  new_rows$project_id <- project_id
  new_rows <- new_rows[, !(names(new_rows) %in% "xmap_antigen_family_id")]
  
  DBI::dbAppendTable(
    conn,
    DBI::Id(schema = "madi_results", table = "xmap_antigen_family"),
    new_rows
  )
  
  message(glue::glue(
    "Created {nrow(new_rows)} antigen family rows for {study}:{experiment}:{project_id} from template."
  ))
  invisible(new_rows)
}

# The main rendering logic wrapped in a reactive expression
render_antigen_family <- reactive({
  req(input$readxMap_study_accession)

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  # Fetch data once
  antigen_families_rv(fetch_antigen_family_table(selected_study, userWorkSpaceID(), input$readxMap_experiment_accession))

  # Debug output
  cat("Antigen families data updated for:", selected_study, "\n")
  print(head(antigen_families_rv()))

  # Render the instructions
  output$antigen_family_instructions <- renderUI({
    req(selected_study)
    HTML(paste("<span style='font-size: 1.5em;'>To edit the family of an antigen for",
               selected_study,
               "click on the cell in the Antigen Family column.",
               "After making the change, click anywhere outside the cell to save the update.<br></span>"))
  })

  # Render the data table
  output$antigenFamilyUI <- renderUI({
    req(antigen_families_rv(), selected_study)
    tagList(
      fluidRow(
        div(
          style = "width: 100%; padding: 0 15px;",  # Added container styling
          span(
            div(style = "display:inline-block; margin-bottom: 10px;",
                title = "Info",
                icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                     `data-placement` = "right",
                     title = paste("To edit the family of an antigen for", selected_study,
                                   " click on the cell in the Antigen Family column.",
                                   " After making the change, click anywhere outside the cell to save the update"),
                     `data-html` = "true")
            )
          ),
          br(),
          div(
            style = "width: 100%; overflow-x: auto;",  # Added wrapper for table
            DTOutput("antigen_family_table")
          )
        )
      )
    )
  })

})
#   # And modify your renderDT to be more responsive:
#   output$antigen_family_table <- renderDT({
#     req(antigen_families_rv())
#     cat("Rendering datatable\n")
#     datatable(antigen_families_rv(),
#               options = list(
#                 pageLength = 50,
#                 scrollX = TRUE,
#                 scrollY = "400px",
#                 autoWidth = TRUE,  # Added this
#                 responsive = TRUE, # Added this
#                 order = list(list(0, 'asc')),
#                 columnDefs = list(
#                   list(className = 'dt-center', targets = '_all')
#                 )
#               ),
#               editable = list(
#                 target = 'cell',
#                 disable = list(columns = c(0:3))
#               ),
#               selection = 'none',
#               class = 'cell-border stripe hover'  # Added styling classes
#     ) %>%
#       formatStyle(columns = 1:ncol(antigen_families_rv()),  # Added column styling
#                   backgroundColor = 'white',
#                   borderBottom = '1px solid #ddd')
#   })
# })
#
# # Handle initial page load
# observe({
#   req(input$readxMap_study_accession)
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Initial load of Study Overview tab\n")
#     render_antigen_family()
#   }
# })
#
# # Handle tab changes
# observeEvent(input$study_level_tabs, {
#   req(input$readxMap_study_accession)
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Tab changed to Study Overview\n")
#     render_antigen_family()
#   }
# })
#
# # Handle study selection changes
# observeEvent(input$readxMap_study_accession, {
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Study selection changed\n")
#     render_antigen_family()
#   }
# })
