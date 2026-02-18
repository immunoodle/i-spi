get_db_connection <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("db"),
            host = Sys.getenv("db_host"),
            port = Sys.getenv("db_port"),
            user = Sys.getenv("db_userid_x"),
            password = Sys.getenv("db_pwd_x"),
            sslmode = 'require',
            options = "-c search_path=madi_results"
  )
}

fetch_study_parameters <- function(study_accession, param_user, param_group = "standard_curve_options", project_id = userWorkSpaceID(), conn) {
  query <- glue("
  SELECT study_accession, param_name, param_boolean_value, param_character_value
	FROM madi_results.xmap_study_config
  WHERE project_id = {project_id}
  AND study_accession = '{study_accession}'
  AND param_user = '{param_user}'
  AND param_group = '{param_group}';
")
  study_parameters <- dbGetQuery(conn, query)
  return(list(
    applyProzone = study_parameters[study_parameters$param_name=="applyProzone", "param_boolean_value"],
    blank_option = study_parameters[study_parameters$param_name=="blank_option", "param_character_value"],
    standard_source = study_parameters[study_parameters$param_name=="default_source", "param_character_value"],
    is_log_response = study_parameters[study_parameters$param_name=="is_log_mfi_axis", "param_boolean_value"],
    is_log_independent = TRUE,
    mean_mfi = study_parameters[study_parameters$param_name=="mean_mfi", "param_boolean_value"]
  ))
}

fetch_antigen_parameters <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("
  SELECT
    xmap_antigen_family_id,
    study_accession,
    experiment_accession,
    antigen,
    l_asy_min_constraint,
    l_asy_max_constraint,
    l_asy_constraint_method,
    standard_curve_concentration,
    pcov_threshold
  FROM madi_results.xmap_antigen_family
  WHERE project_id = {project_id}
  AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';
")
  antigen_constraints <- dbGetQuery(conn, query)
  return(antigen_constraints=antigen_constraints)
}


fetch_db_header <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, nominal_sample_dilution,plate_id,
  assay_response_variable, assay_independent_variable, nominal_sample_dilution, project_id
  FROM madi_results.xmap_header
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

fetch_db_header_experiments <- function(study_accession, conn, verbose = TRUE) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, plate_id,
  assay_response_variable, assay_independent_variable
  FROM madi_results.xmap_header
WHERE study_accession = '{study_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

fetch_db_standards <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, feature, plate_id, stype, source, sampleid, well, dilution, antigen, antibody_mfi AS mfi, nominal_sample_dilution
  FROM madi_results.xmap_standard
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  standard_df  <- dbGetQuery(conn, query)
  standard_df <- distinct(standard_df)
  return(standard_df)
}

fetch_db_buffer <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plate_id, stype, well, antigen, dilution, feature, antibody_mfi AS mfi, nominal_sample_dilution FROM madi_results.xmap_buffer
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  blank_data <- dbGetQuery(conn, query)
  blank_data <- distinct(blank_data)
  return(blank_data)
}

fetch_db_controls <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plate_id, well, stype, sampleid,
                    source, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi as MFI, antibody_n
                    feature, project_id, plateid, nominal_sample_dilution, plate
                  	FROM madi_results.xmap_control
              WHERE project_id = {project_id}
              AND study_accession = '{study_accession}'
              AND experiment_accession = '{experiment_accession}';")

  control_data <- dbGetQuery(conn, query)
  control_data <- distinct(control_data)

  return(control_data)
}

fetch_db_samples <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession,
experiment_accession, plate_id, timeperiod, patientid,
well, stype, sampleid,  agroup, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi AS mfi,
antibody_n, nominal_sample_dilution, feature FROM madi_results.xmap_sample
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  sample_data <- dbGetQuery(conn, query)
  sample_data <- distinct(sample_data)
  return(sample_data)
}

pull_data <- function(study_accession, experiment_accession, project_id, conn = conn) {
  plates <- fetch_db_header(study_accession = study_accession,
                            experiment_accession = experiment_accession,
                            project_id = project_id,
                            conn = conn)
  plates$plate_id <- trimws(plates$plate_id)

  plates$plate_nom <- paste(plates$plate, plates$nominal_sample_dilution, sep = "-")


  antigen_constraints <- fetch_antigen_parameters(study_accession = study_accession,
                                                  experiment_accession = experiment_accession,
                                                  project_id = project_id,
                                                  conn = conn)

  standard_curve_data <- fetch_db_standards(study_accession = study_accession,
                                            experiment_accession = experiment_accession,
                                            project_id = project_id,
                                            conn = conn)
  standard_curve_data$plate_id <- trimws(standard_curve_data$plate_id)

  # cat("plates\n")
  # print(names(plates))
  # cat("standard_curve data\n")
  # print(names(standard_curve_data))

  standards <- inner_join(standard_curve_data, plates[, c("study_accession", "experiment_accession" ,"plateid", "plate", "plate_id" , "assay_response_variable" ,"assay_independent_variable"
   ,"project_id")], by = c("study_accession", "experiment_accession","plate_id"))[ ,c("study_accession","experiment_accession","feature","source","plateid",
                                                                                                                        "plate", "stype", "nominal_sample_dilution",
                                                                                                                        "sampleid","well","dilution","antigen","mfi",
                                                                                                                        "assay_response_variable", "assay_independent_variable")]

 standards$plate_nom <- paste(standards$plate, standards$nominal_sample_dilution, sep = "-")

  blanks <- inner_join(fetch_db_buffer(study_accession = study_accession,
                                       experiment_accession = experiment_accession,
                                       project_id = project_id,
                                       conn = conn) %>% dplyr::mutate(plate_id = trimws(as.character(plate_id))),
                       plates[, c("study_accession", "experiment_accession" ,"plateid", "plate", "plate_id" , "assay_response_variable" ,"assay_independent_variable"
                                  ,"project_id")], by = c("study_accession", "experiment_accession","plate_id"))

  blanks$plate_nom <- paste(blanks$plate, blanks$nominal_sample_dilution, sep = "-")

  samples <- inner_join(fetch_db_samples(study_accession = study_accession,
                                         experiment_accession = experiment_accession,
                                         project_id = project_id,
                                         conn = conn) %>% dplyr::mutate(plate_id = trimws(as.character(plate_id))),
                        plates[, c("study_accession", "experiment_accession" ,"plateid", "plate", "plate_id" , "assay_response_variable" ,"assay_independent_variable"
                                   ,"project_id")], by = c("study_accession", "experiment_accession","plate_id"))

  samples$plate_nom <- paste(samples$plate, samples$nominal_sample_dilution, sep = "-")

  response_var = unique(plates$assay_response_variable)
  indep_var = unique(plates$assay_independent_variable)


  return(list(plates=plates, standards=standards,
              blanks=blanks, samples=samples,
              antigen_constraints=antigen_constraints,
              response_var = response_var,
              indep_var = indep_var)
  )
}

shiny_notify <- function(session = shiny::getDefaultReactiveDomain()) {
  function(msg) {
    shiny::showNotification(
      msg,
      type = "message",
      session = session
    )
  }
}

upsert_best_curve <- function(conn,
                              df,
                              schema = "madi_results",
                              table  = "best_plate_all",
                              notify = NULL,
                              quiet  = FALSE,
                              batch_size = 50000,
                              use_copy = TRUE,
                              skip_index_check = FALSE) {

  ##
  ## Notifier
  ##
  if (is.null(notify)) {
    notify <- function(msg) if (!quiet) message(Sys.time(), " - ", msg)
  }
  bail <- function(msg) {
    notify(msg)
    invisible(FALSE)
  }

  ##
  ## Basic checks
  ##
  if (!DBI::dbIsValid(conn)) return(bail("Database connection is not valid."))
  if (!is.data.frame(df) || nrow(df) == 0) return(bail("No data provided."))

  cols <- names(df)
  nk <- get_natural_keys(table)
  pk <- get_primary_key(table)  # New function to get primary key

  if (is.null(nk)) stop("Unknown table: ", table)

  missing_keys <- setdiff(nk, cols)
  if (length(missing_keys) > 0) {
    stop("Missing natural-key columns: ", paste(missing_keys, collapse = ", "))
  }

  nk_has_na <- sapply(df[, nk, drop = FALSE], function(x) any(is.na(x)))

  if (any(nk_has_na)) {
    # Columns that contain any NA
    cols_with_na <- names(nk_has_na)[nk_has_na]

    # How many rows are affected per column (use `colSums` for a quick count)
    rows_per_col <- colSums(is.na(df[, cols_with_na, drop = FALSE]))

  # If you also want to know *which rows* are problematic you can
  # collect the row numbers (limited to the first `max_rows_to_show`)
  problematic_rows <- which(rowSums(is.na(df[, cols_with_na, drop = FALSE])) > 0)

  # Build a helpful message
  msg <- paste0(
    "Natural‑key column(s) contain NA in table `", table, "`:\n",
    paste0(" - ", cols_with_na, ": ", rows_per_col, " NA(s)"),
    collapse = "\n"
  )

  if (length(problematic_rows) > 0) {
    shown <- head(problematic_rows, 3)
    msg <- paste0(
      msg,
      "\nFirst ", length(shown), " problematic row(s): ",
      paste(shown, collapse = ", "),
      if (length(problematic_rows) > 3) " …"
    )
  }

  return(bail(msg))
}

  # if (anyNA(df[, nk, drop = FALSE])) {
  #   return(bail("Natural-key columns contain NA; cannot upsert."))
  # }

  ##
  ## Pre-compute SQL components
  ##
  sql_parts <- build_sql_components_pk(conn, schema, table, cols, nk, pk)

  ##
  ## Batch processing for large datasets
  ##
  n_rows <- nrow(df)

  if (n_rows > batch_size) {
    notif_id <- paste0("upsert_", table)

    showNotification(
      sprintf("Processing %d rows in %d batches", n_rows, ceiling(n_rows / batch_size)),
      id = notif_id,
      duration = 3,
      type = "message"
    )

    batches <- split(df, ceiling(seq_len(n_rows) / batch_size))

    results <- tryCatch({
      vapply(seq_along(batches), function(i) {
        showNotification(
          sprintf("Batch %d/%d (%d rows)", i, length(batches), nrow(batches[[i]])),
          id = notif_id,
          duration = NULL,
          type = "message"
        )
        upsert_batch_pk(conn, batches[[i]], table, sql_parts, use_copy, notify)
      }, logical(1))
    }, error = function(e) {
      showNotification(
        sprintf("Error during batch processing: %s", conditionMessage(e)),
        id = notif_id,
        duration = NULL,
        type = "error"
      )
      return(NULL)
    })

    if (is.null(results)) {
      return(invisible(FALSE))
    }

    if (all(results)) {
      removeNotification(notif_id)
      showNotification(
        sprintf("All %d batches completed successfully", length(batches)),
        duration = 5,
        type = "message"
      )
      return(invisible(TRUE))
    } else {
      showNotification(
        sprintf("%d/%d batches failed", sum(!results), length(batches)),
        id = notif_id,
        duration = NULL,
        type = "error"
      )
      return(invisible(FALSE))
    }
  }

  ## Single batch
  ok <- tryCatch({
    upsert_batch_pk(conn, df, table, sql_parts, use_copy, notify)
  }, error = function(e) {
    showNotification(
      sprintf("Upsert failed: %s", conditionMessage(e)),
      duration = NULL,
      type = "error"
    )
    return(FALSE)
  })

  if (ok) {
    showNotification(
      paste0(table, " upsert completed (", n_rows, " rows)."),
      duration = 5,
      type = "message"
    )
  }

  invisible(ok)
}

## Get primary key for each table
get_primary_key <- function(table) {
  keys <- list(
    best_plate_all = "best_plate_all_id",
    best_glance_all = "best_glance_all_id",
    best_tidy_all = "best_tidy_all_id",
    best_sample_se_all = "best_sample_se_all_id",
    best_standard_all = "best_standard_all_id",
    best_pred_all = "best_pred_all_id"
  )
  keys[[table]]
}

## Build SQL components for primary key based upsert
build_sql_components_pk <- function(conn, schema, table, cols, nk, pk) {
  schema_id <- as.character(DBI::dbQuoteIdentifier(conn, schema))
  table_id <- as.character(DBI::dbQuoteIdentifier(conn, table))

  cols_quoted <- vapply(cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  nk_quoted <- vapply(nk, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  pk_quoted <- as.character(DBI::dbQuoteIdentifier(conn, pk))

  cols_list <- paste(cols_quoted, collapse = ", ")
  nk_list <- paste(nk_quoted, collapse = ", ")

  # Build WHERE clause for natural key matching
  nk_conditions <- vapply(nk, function(x) {
    col_quoted <- as.character(DBI::dbQuoteIdentifier(conn, x))
    paste0("t.", col_quoted, " = tmp.", col_quoted)
  }, character(1), USE.NAMES = FALSE)

  nk_where_clause <- paste(nk_conditions, collapse = " AND ")

  # Build UPDATE SET clause for non-key columns
  update_cols <- setdiff(cols, c(nk, pk))
  if (length(update_cols) > 0) {
    update_quoted <- vapply(update_cols, function(x) {
      as.character(DBI::dbQuoteIdentifier(conn, x))
    }, character(1), USE.NAMES = FALSE)
    set_clause <- paste(
      vapply(update_quoted, function(col) paste0(col, " = tmp.", col), character(1)),
      collapse = ", "
    )
  } else {
    set_clause <- NULL
  }

  # Columns for INSERT (excluding primary key - let it auto-generate)
  insert_cols <- setdiff(cols, pk)
  insert_cols_quoted <- vapply(insert_cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)
  insert_cols_list <- paste(insert_cols_quoted, collapse = ", ")

  list(
    schema_id = schema_id,
    table_id = table_id,
    pk = pk,
    pk_quoted = pk_quoted,
    cols = cols,
    cols_list = cols_list,
    insert_cols = insert_cols,
    insert_cols_list = insert_cols_list,
    nk = nk,
    nk_list = nk_list,
    nk_where_clause = nk_where_clause,
    set_clause = set_clause
  )
}

## Execute single batch using DELETE/INSERT strategy based on natural keys
upsert_batch_pk <- function(conn, df, table, sql_parts, use_copy, notify) {
  tryCatch({
    DBI::dbWithTransaction(conn, {
      tmp_name <- paste0("tmp_", substr(digest::digest(Sys.time()), 1, 8))
      tmp_id <- as.character(DBI::dbQuoteIdentifier(conn, tmp_name))

      # Create temp table (excluding primary key column if present)
      temp_cols <- setdiff(sql_parts$cols, sql_parts$pk)

      # Filter df to exclude primary key column for temp table
      df_temp <- df[, temp_cols, drop = FALSE]

      create_sql <- sprintf(
        "CREATE TEMP TABLE %s (%s) ON COMMIT DROP",
        tmp_id,
        paste(sprintf("%s %s",
                      vapply(temp_cols, function(x) as.character(DBI::dbQuoteIdentifier(conn, x)), character(1)),
                      vapply(df_temp, pg_type_map, character(1))
        ), collapse = ", ")
      )
      DBI::dbExecute(conn, create_sql)

      # Load data into temp table using COPY
      if (use_copy && requireNamespace("RPostgres", quietly = TRUE)) {
        RPostgres::dbWriteTable(
          conn, tmp_name, df_temp,
          append = TRUE,
          row.names = FALSE,
          copy = TRUE
        )
      } else {
        DBI::dbWriteTable(conn, tmp_name, df_temp, append = TRUE, row.names = FALSE)
      }

      # Step 1: DELETE existing rows that match natural keys
      delete_sql <- sprintf(
        "DELETE FROM %s.%s t
         USING %s tmp
         WHERE %s",
        sql_parts$schema_id, sql_parts$table_id,
        tmp_id,
        sql_parts$nk_where_clause
      )
      DBI::dbExecute(conn, delete_sql)

      # Step 2: INSERT all rows from temp table (primary key auto-generates)
      insert_sql <- sprintf(
        "INSERT INTO %s.%s (%s)
         SELECT %s FROM %s",
        sql_parts$schema_id, sql_parts$table_id,
        sql_parts$insert_cols_list,
        sql_parts$insert_cols_list,
        tmp_id
      )
      DBI::dbExecute(conn, insert_sql)
    })
    TRUE
  }, error = function(e) {
    showNotification(
      id = "error_batch",
      paste0("Batch failed for ", table, ": ", conditionMessage(e)),
      duration = NULL,
      closeButton = TRUE,
      type = "error"
    )
    FALSE
  })
}





# upsert_best_curve <- function(conn,
#                               df,
#                               schema = "madi_results",
#                               table  = "best_plate_all",
#                               notify = NULL,
#                               quiet  = FALSE,
#                               batch_size = 50000,
#                               use_copy = TRUE,
#                               skip_index_check = FALSE) {
#
#   ##
#   ## Notifier
#   ##
#   if (is.null(notify)) {
#     notify <- function(msg) if (!quiet) message(Sys.time(), " - ", msg)
#   }
#   bail <- function(msg) {
#     notify(msg)
#     invisible(FALSE)
#   }
#
#   ##
#   ## Basic checks
#   ##
#   if (!DBI::dbIsValid(conn)) return(bail("Database connection is not valid."))
#   if (!is.data.frame(df) || nrow(df) == 0) return(bail("No data provided."))
#
#   cols <- names(df)
#   nk <- get_natural_keys(table)
#   if (is.null(nk)) stop("Unknown table: ", table)
#
#   missing_keys <- setdiff(nk, cols)
#   if (length(missing_keys) > 0) {
#     stop("Missing natural-key columns: ", paste(missing_keys, collapse = ", "))
#   }
#
#   if (anyNA(df[, nk, drop = FALSE])) {
#     return(bail("Natural-key columns contain NA; cannot upsert."))
#   }
#
#   ##
#   ## Cache index check (skip on repeated calls)
#   ##
#   # if (!skip_index_check) {
#   #   cache_key <- paste0(schema, ".", table)
#   #   if (!exists(".upsert_index_cache", envir = .GlobalEnv)) {
#   #     assign(".upsert_index_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
#   #   }
#   #   cache <- get(".upsert_index_cache", envir = .GlobalEnv)
#   #
#   #   if (!isTRUE(cache[[cache_key]])) {
#   #     idx_exists <- DBI::dbGetQuery(conn, glue::glue_sql(
#   #       "SELECT 1 FROM pg_indexes
#   #        WHERE schemaname = {schema} AND tablename = {table}
#   #        AND indexdef LIKE '%UNIQUE%' LIMIT 1",
#   #       .con = conn
#   #     ))
#   #     if (nrow(idx_exists) == 0) {
#   #       return(bail(paste0("No UNIQUE index for ", schema, ".", table)))
#   #     }
#   #     cache[[cache_key]] <- TRUE
#   #   }
#  # }
#
#   ##
#   ## Pre-compute SQL components (avoid repeated quoting)
#   ##
#   sql_parts <- build_sql_components(conn, schema, table, cols, nk)
#
#   ##
#   ## Batch processing for large datasets
#   ##
#   n_rows <- nrow(df)
#
#   if (n_rows > batch_size) {
#     notif_id <- paste0("upsert_", table)
#
#     showNotification(
#       sprintf("Processing %d rows in %d batches", n_rows, ceiling(n_rows / batch_size)),
#       id = notif_id,
#       duration = 3,
#       type = "message"
#     )
#
#     batches <- split(df, ceiling(seq_len(n_rows) / batch_size))
#
#     results <- tryCatch({
#       vapply(seq_along(batches), function(i) {
#         showNotification(
#           sprintf("Batch %d/%d (%d rows)", i, length(batches), nrow(batches[[i]])),
#           id = notif_id,
#           duration = NULL,
#           type = "message"
#         )
#         upsert_batch(conn, batches[[i]], table, sql_parts, use_copy, notify)
#       }, logical(1))
#     }, error = function(e) {
#       showNotification(
#         sprintf("Error during batch processing: %s", conditionMessage(e)),
#         id = notif_id,
#         duration = NULL,
#         type = "error"
#       )
#       return(NULL)
#     })
#
#     # Handle error case (NULL returned from tryCatch)
#     if (is.null(results)) {
#       return(invisible(FALSE))
#     }
#
#     if (all(results)) {
#       removeNotification(notif_id)
#       showNotification(
#         sprintf("All %d batches completed successfully", length(batches)),
#         duration = 5,
#         type = "message"
#       )
#       return(invisible(TRUE))
#     } else {
#       showNotification(
#         sprintf("%d/%d batches failed", sum(!results), length(batches)),
#         id = notif_id,
#         duration = NULL,
#         type = "error"
#       )
#       return(invisible(FALSE))
#     }
#   }
#
#   ## Single batch
#   ok <- tryCatch({
#     upsert_batch(conn, df, table, sql_parts, use_copy, notify)
#   }, error = function(e) {
#     showNotification(
#       sprintf("Upsert failed: %s", conditionMessage(e)),
#       duration = NULL,
#       type = "error"
#     )
#     return(FALSE)
#   })
#
#   if (ok) {
#     showNotification(
#       paste0(table, " upsert completed (", n_rows, " rows)."),
#       duration = 5,
#       type = "message"
#     )
#   }
#
#   invisible(ok)
# }

## Pre-compute SQL components once
#
# build_sql_components <- function(conn, schema, table, cols, nk) {
#   schema_id <- as.character(DBI::dbQuoteIdentifier(conn, schema))
#   table_id <- as.character(DBI::dbQuoteIdentifier(conn, table))
#
#   cols_quoted <- vapply(cols, function(x) {
#     as.character(DBI::dbQuoteIdentifier(conn, x))
#   }, character(1), USE.NAMES = FALSE)
#
#   nk_quoted <- vapply(nk, function(x) {
#     as.character(DBI::dbQuoteIdentifier(conn, x))
#   }, character(1), USE.NAMES = FALSE)
#
#   cols_list <- paste(cols_quoted, collapse = ", ")
#   nk_list <- paste(nk_quoted, collapse = ", ")
#
#   update_cols <- setdiff(cols, nk)
#   if (length(update_cols) > 0) {
#     update_quoted <- vapply(update_cols, function(x) {
#       as.character(DBI::dbQuoteIdentifier(conn, x))
#     }, character(1), USE.NAMES = FALSE)
#     set_clause <- paste(
#       vapply(update_quoted, function(col) paste0(col, " = EXCLUDED.", col), character(1)),
#       collapse = ", "
#     )
#     conflict_action <- paste("DO UPDATE SET", set_clause)
#   } else {
#     conflict_action <- "DO NOTHING"
#   }
#
#   list(
#     schema_id = schema_id,
#     table_id = table_id,
#     cols = cols,
#     cols_list = cols_list,
#     nk_list = nk_list,
#     conflict_action = conflict_action
#   )
# }


## Execute single batch with COPY optimization
#
# upsert_batch <- function(conn, df, table, sql_parts, use_copy, notify) {
#   tryCatch({
#     DBI::dbWithTransaction(conn, {
#       tmp_name <- paste0("tmp_", substr(digest::digest(Sys.time()), 1, 8))
#       tmp_id <- as.character(DBI::dbQuoteIdentifier(conn, tmp_name))
#
#       # Minimal temp table - UNLOGGED equivalent for temp tables
#       create_sql <- sprintf(
#         "CREATE TEMP TABLE %s (%s) ON COMMIT DROP",
#         tmp_id,
#         paste(sprintf("%s %s",
#                       vapply(sql_parts$cols, function(x) as.character(DBI::dbQuoteIdentifier(conn, x)), character(1)),
#                       vapply(df, pg_type_map, character(1))
#         ), collapse = ", ")
#       )
#       DBI::dbExecute(conn, create_sql)
#
#       # Use COPY for bulk loading (much faster than INSERT)
#       if (use_copy && requireNamespace("RPostgres", quietly = TRUE)) {
#         RPostgres::dbWriteTable(
#           conn, tmp_name, df,
#           append = TRUE,
#           row.names = FALSE,
#           copy = TRUE  # Uses PostgreSQL COPY protocol
#         )
#       } else {
#         DBI::dbWriteTable(conn, tmp_name, df, append = TRUE, row.names = FALSE)
#       }
#
#       # Execute upsert
#       upsert_sql <- sprintf(
#         "INSERT INTO %s.%s (%s) SELECT %s FROM %s ON CONFLICT (%s) %s",
#         sql_parts$schema_id, sql_parts$table_id,
#         sql_parts$cols_list, sql_parts$cols_list, tmp_id,
#         sql_parts$nk_list, sql_parts$conflict_action
#       )
#       DBI::dbExecute(conn, upsert_sql)
#     })
#     TRUE
#   }, error = function(e) {
#     showNotification(id = "error_batch" ,paste0("Batch failed for ", table, ":", conditionMessage(e)), duration = NULL, closeButton = T, type = "error")
#     FALSE
#   })
# }


## Map R types to PostgreSQL types

pg_type_map <- function(col) {
  switch(class(col)[1],
         "integer" = "INTEGER",
         "numeric" = "DOUBLE PRECISION",
         "character" = "TEXT",
         "logical" = "BOOLEAN",
         "Date" = "DATE",
         "POSIXct" = "TIMESTAMPTZ",
         "POSIXlt" = "TIMESTAMPTZ",
         "factor" = "TEXT",
         "TEXT"
  )
}

## Helper: Get natural keys for table

get_natural_keys <- function(table) {
  keys <- list(
    # best_plate_all = c(
    #   "study_accession", "experiment_accession",
    #   "plateid", "plate", "sample_dilution_factor", "source"
    # ),
    # best_glance_all = c(
    #   "study_accession", "experiment_accession",
    #   "plateid", "plate", "sample_dilution_factor", "source", "antigen"
    # ),
    # best_tidy_all = c(
    #   "study_accession", "experiment_accession",
    #   "plateid", "plate", "sample_dilution_factor", "source", "antigen", "term"
    # ),
    # best_sample_se_all = c(
    #   "study_accession", "experiment_accession",
    #   "plateid", "plate", "sample_dilution_factor", "source", "antigen",
    #   "patientid", "timeperiod", "sampleid", "dilution",
    #    "uid"
    # ),
    # best_standard_all = c(
    #   "study_accession", "experiment_accession",
    #   "plateid", "plate", "sample_dilution_factor", "source", "antigen", "feature", "well"
    # ), # dilution not included as it can be NA when geometric mean is used
    # best_pred_all = c(
    #   "study_accession", "experiment_accession",
    #   "plateid", "plate", "sample_dilution_factor", "source", "antigen",
    #    "id_match"
    # )
    best_plate_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source"
    ),
    best_glance_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "antigen"
    ),
    best_tidy_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "antigen", "term"
    ),
    best_sample_se_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "antigen",
      "patientid", "timeperiod", "sampleid", "dilution"
    ),
    best_standard_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "antigen", "feature", "well"
    ), # dilution not included as it can be NA when geometric mean is used
    best_pred_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "antigen"
    )
  )

  keys[[table]]
}

## Helper: Build UPSERT SQL using glue_sql

build_upsert_sql_glue <- function(conn, schema, table, tmp_name, cols, nk) {

  ## Pre-quote identifiers to avoid glue_sql conflicts
  schema_id <- DBI::dbQuoteIdentifier(conn, schema)
  table_id <- DBI::dbQuoteIdentifier(conn, table)
  tmp_id <- DBI::dbQuoteIdentifier(conn, tmp_name)

  ## Quote column names
  cols_quoted <- vapply(cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  nk_quoted <- vapply(nk, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  ## Build column list strings

  cols_list <- paste(cols_quoted, collapse = ", ")
  nk_list <- paste(nk_quoted, collapse = ", ")

  ## Build SET clause for non-key columns
  update_cols <- setdiff(cols, nk)

  if (length(update_cols) > 0) {
    update_quoted <- vapply(update_cols, function(x) {
      as.character(DBI::dbQuoteIdentifier(conn, x))
    }, character(1), USE.NAMES = FALSE)

    set_parts <- vapply(update_quoted, function(col) {
      paste0(col, " = EXCLUDED.", col)
    }, character(1), USE.NAMES = FALSE)

    set_clause <- paste(set_parts, collapse = ", ")
    conflict_action <- paste("DO UPDATE SET", set_clause)
  } else {
    conflict_action <- "DO NOTHING"
  }

  ## Build final SQL using glue_sql with DBI::SQL for pre-quoted parts
  glue::glue_sql(
    "INSERT INTO {DBI::SQL(schema_id)}.{DBI::SQL(table_id)} ({DBI::SQL(cols_list)})
     SELECT {DBI::SQL(cols_list)}
     FROM {DBI::SQL(tmp_id)}
     ON CONFLICT ({DBI::SQL(nk_list)})
     {DBI::SQL(conflict_action)}",
    .con = conn
  )
}

select_antigen_plate <- function(loaded_data,
                                 study_accession = study_accession,
                                 experiment_accession = experiment_accession,
                                 source = source,
                                 antigen = antigen,
                                 plate = plate,
                                 antigen_constraints = antigen_constraints){
  print("select antigen plate in batch\n")
  print("plate in\n")
  print(plate)
  print("standards structure\n")
  print(str(loaded_data$standards))
  print("antigens\n")
  print(unique(loaded_data$standards$antigen))
  print("source\n")
  print(unique(loaded_data$standards$source))
  print("plate_nom\n")
  print(unique(loaded_data$standards$plate_nom))

  print("plate\n")
  print(unique(loaded_data$standards$plate))

  plate_standard  <- loaded_data$standards[loaded_data$standards$source == source &
                                             loaded_data$standards$antigen == antigen &
                                             loaded_data$standards$plate_nom == plate ,]
  # Guard against empty plate_standard data
  if (is.null(plate_standard) || nrow(plate_standard) == 0) {
    warning(paste("No standard curve data found for:",
                  "source =", source,
                  ", antigen =", antigen,
                  ", plate =", plate))
    return(NULL)
  }

  plate_blanks <- loaded_data$blanks[loaded_data$blanks$antigen == antigen &
                                       loaded_data$blanks$plate_nom == plate,]

  plate_samples <- loaded_data$samples[loaded_data$samples$antigen == antigen &
                                         loaded_data$samples$plate_nom == plate,]

  # anything after - is removed (nominal sample dilutions)
  plate_c <- sub("-.*$", "", plate)

  antigen_settings <- obtain_lower_constraint(dat = plate_standard,
                                              antigen = antigen,
                                              study_accession = study_accession,
                                              experiment_accession = experiment_accession,
                                              plate = plate_c,

                                              plateid = unique(plate_standard$plateid),

                                              plate_blanks = plate_blanks,
                                              antigen_constraints = antigen_constraints)


  fixed_a_result <- test_fixed_lower_asymptote(antigen_settings)

  std_error_blank <- get_blank_se(antigen_settings = antigen_settings)

  return (list(plate_standard=plate_standard,
               plate_blanks=plate_blanks,
               plate_samples=plate_samples,
               antigen_settings=antigen_settings,
               fixed_a_result = fixed_a_result,
               std_error_blank = std_error_blank))
}

#### Fetch saved results from std_curver
fetch_best_plate_all <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("
SELECT best_plate_all_id, study_accession, experiment_accession, feature, source, plateid, plate, nominal_sample_dilution, assay_response_variable, assay_independent_variable
	FROM madi_results.best_plate_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
	AND experiment_accession = '{experiment_accession}';
")
  best_plate_all <- dbGetQuery(conn, query)
  return(best_plate_all)
}

fetch_best_tidy_all <- function(study_accession,experiment_accession, project_id, conn) {
  query <- glue("SELECT best_tidy_all_id, study_accession, experiment_accession, term, lower, upper, estimate, std_error, statistic, p_value, nominal_sample_dilution, antigen, plateid, plate, source
	FROM madi_results.best_tidy_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}'")
  best_tidy_all <- dbGetQuery(conn, query)
  return(best_tidy_all)
}
fetch_best_pred_all <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT best_pred_all_id, x, model, yhat, overall_se, predicted_concentration, se_x, pcov, study_accession, experiment_accession, nominal_sample_dilution, plateid, plate,
  antigen, source, best_glance_all_id
	FROM madi_results.best_pred_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';")
  best_pred_all <- dbGetQuery(conn, query)
  return(best_pred_all)
}

fetch_best_standard_all <- function(study_accession,experiment_accession, project_id, conn) {
  query <- glue("SELECT best_standard_all_id, study_accession, experiment_accession, feature, source, plateid, plate, stype, nominal_sample_dilution, sampleid, well,
  dilution, antigen, assay_response, assay_response_variable, assay_independent_variable, concentration, g, best_glance_all_id
	FROM madi_results.best_standard_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';")
  best_standard_all <- dbGetQuery(conn, query)
  return(best_standard_all)
}

fetch_best_glance_all <- function(study_accession,experiment_accession, project_id, conn) {
  query <- glue("SELECT best_glance_all_id, study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, antigen, iter, status, crit, a, b, c, d, lloq, uloq, lloq_y, uloq_y, llod, ulod, inflect_x, inflect_y, std_error_blank, dydx_inflect, dfresidual, nobs, rsquare_fit, aic, bic, loglik, mse, cv, source, bkg_method, is_log_response, is_log_x, apply_prozone, formula, g
	FROM madi_results.best_glance_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';")
  best_glance_all <- dbGetQuery(conn, query)
  return(best_glance_all)
}

#' Fetch best_glance_all filtered by user's current study parameters
#' This ensures consistency with fetch_best_pred_all_summary and other summary functions
#' @param study_accession Study accession ID
#' @param experiment_accession Experiment accession ID
#' @param param_user Current user for parameter lookup
#' @param conn Database connection
#' @return Filtered best_glance_all dataframe matching user's current study configuration
fetch_best_glance_all_summary <- function(study_accession, experiment_accession, param_user, project_id, conn) {
  query <- glue_sql("
    WITH params AS (
      SELECT
        study_accession,
        BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
        MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
        BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
      FROM madi_results.xmap_study_config
      WHERE project_id = {project_id}
         AND study_accession = {study_accession}
        AND param_user = {param_user}
        AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
      GROUP BY study_accession
    )
    SELECT
      g.best_glance_all_id, g.study_accession, g.experiment_accession,
      g.plateid, g.plate, g.nominal_sample_dilution, g.antigen, g.iter,
      g.status, g.crit, g.a, g.b, g.c, g.d, g.lloq, g.uloq, g.lloq_y,
      g.uloq_y, g.llod, g.ulod, g.inflect_x, g.inflect_y, g.std_error_blank,
      g.dydx_inflect, g.dfresidual, g.nobs, g.rsquare_fit, g.aic, g.bic,
      g.loglik, g.mse, g.cv, g.source, g.bkg_method, g.is_log_response,
      g.is_log_x, g.apply_prozone, g.formula, g.g
    FROM madi_results.best_glance_all g
    CROSS JOIN params
    WHERE g.project_id = {project_id}
      AND g.study_accession = {study_accession}
      AND g.experiment_accession = {experiment_accession}
      AND g.is_log_response = params.is_log_mfi_axis
      AND g.bkg_method = params.blank_option
      AND g.apply_prozone = params.apply_prozone",
                    .con = conn
  )
  dbGetQuery(conn, query)
}

fetch_best_sample_se_all <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("
SELECT best_sample_se_all_id, raw_predicted_concentration, study_accession, experiment_accession, timeperiod, patientid, well, stype, sampleid,
agroup, pctaggbeads, samplingerrors, antigen,
antibody_n, plateid, plate, nominal_sample_dilution, assay_response_variable, assay_independent_variable, dilution, overall_se, raw_assay_response, assay_response,
se_concentration, final_predicted_concentration, pcov, source, gate_class_loq, gate_class_lod,
gate_class_pcov, best_glance_all_id, feature, norm_assay_response
	FROM madi_results.best_sample_se_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
	AND experiment_accession = '{experiment_accession}';")
  best_sample_se_all <- dbGetQuery(conn, query)
  return(best_sample_se_all)
}


## Specific fetch queries for the summary of standard curves accounting for
# selected study_configuration based on glance_id
fetch_best_pred_all_summary <- function(study_accession, experiment_accession, param_user, project_id, conn) {
  query <- glue_sql("
    WITH params AS (
      SELECT
        study_accession,
        BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
        MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
        BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
      FROM madi_results.xmap_study_config
      WHERE project_id = {project_id}
        AND study_accession = {study_accession}
        AND param_user = {param_user}
        AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
      GROUP BY study_accession
    )
    SELECT
      p.best_pred_all_id, p.x, p.model, p.yhat, p.overall_se,
      p.predicted_concentration, p.se_x, p.pcov,
      p.study_accession, p.experiment_accession, p.nominal_sample_dilution,
      p.plateid, p.plate, p.antigen, p.source, p.best_glance_all_id,
      g.is_log_response, g.is_log_x, g.bkg_method, g.apply_prozone
    FROM madi_results.best_pred_all p
    LEFT JOIN madi_results.best_glance_all g
      ON p.best_glance_all_id = g.best_glance_all_id
    CROSS JOIN params
    WHERE p.project_id = {project_id}
      AND p.study_accession = {study_accession}
      AND p.experiment_accession = {experiment_accession}
      AND g.is_log_response = params.is_log_mfi_axis
      AND g.bkg_method = params.blank_option
      AND g.apply_prozone = params.apply_prozone",
                    .con = conn
  )
  dbGetQuery(conn, query)
}

fetch_best_standard_all_summary <- function(study_accession, experiment_accession, param_user, project_id, conn) {
  query <- glue_sql("
    WITH params AS (
      SELECT
        study_accession,
        BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
        MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
        BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
      FROM madi_results.xmap_study_config
      WHERE project_id = {project_id}
        AND study_accession = {study_accession}
        AND param_user = {param_user}
        AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
      GROUP BY study_accession
    )
    SELECT
      s.best_standard_all_id, s.study_accession, s.experiment_accession,
      s.feature, s.source, s.plateid, s.plate, s.stype, s.nominal_sample_dilution,
      s.sampleid, s.well, s.dilution, s.antigen, s.assay_response,
      s.assay_response_variable, s.assay_independent_variable,
      s.concentration, s.g, s.best_glance_all_id,
      g.is_log_response, g.is_log_x, g.bkg_method, g.apply_prozone
    FROM madi_results.best_standard_all s
    LEFT JOIN madi_results.best_glance_all g
      ON s.best_glance_all_id = g.best_glance_all_id
    CROSS JOIN params
    WHERE s.project_id = {project_id}
      AND s.study_accession = {study_accession}
      AND s.experiment_accession = {experiment_accession}
      AND g.is_log_response = params.is_log_mfi_axis
      AND g.bkg_method = params.blank_option
      AND g.apply_prozone = params.apply_prozone",
                    .con = conn
  )
  dbGetQuery(conn, query)
}

fetch_best_sample_se_all_summary <- function(study_accession, experiment_accession, param_user, project_id, conn) {
  query <- glue_sql("
    WITH params AS (
      SELECT
        study_accession,
        BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
        MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
        BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
      FROM madi_results.xmap_study_config
      WHERE project_id = {project_id}
        AND study_accession = {study_accession}
        AND param_user = {param_user}
        AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
      GROUP BY study_accession
    )
    SELECT
      ss.best_sample_se_all_id, ss.raw_predicted_concentration,
      ss.study_accession, ss.experiment_accession, ss.timeperiod,
      ss.patientid, ss.well, ss.stype, ss.sampleid, ss.agroup,
      ss.pctaggbeads, ss.samplingerrors, ss.antigen, ss.antibody_n,
      ss.plateid, ss.plate, ss.nominal_sample_dilution,
      ss.assay_response_variable, ss.assay_independent_variable,
      ss.dilution, ss.overall_se, ss.assay_response, ss.se_concentration,
      ss.final_predicted_concentration, ss.pcov, ss.source, ss.gate_class_loq, ss.gate_class_lod,
      ss.gate_class_pcov, ss.best_glance_all_id, ss.feature, ss.norm_assay_response,
      g.is_log_response, g.is_log_x, g.bkg_method, g.apply_prozone
    FROM madi_results.best_sample_se_all ss
    LEFT JOIN madi_results.best_glance_all g
      ON ss.best_glance_all_id = g.best_glance_all_id
    CROSS JOIN params
    WHERE ss.project_id = {project_id}
      AND ss.study_accession = {study_accession}
      AND ss.experiment_accession = {experiment_accession}
      AND g.is_log_response = params.is_log_mfi_axis
      AND g.bkg_method = params.blank_option
      AND g.apply_prozone = params.apply_prozone",
                    .con = conn
  )
  dbGetQuery(conn, query)
}

fetch_current_sc_options_wide <- function(currentuser, study_accession, project_id, conn) {
  query <- glue_sql(
    "
SELECT
  study_accession,
  param_user,
  BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
  MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
  BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
FROM madi_results.xmap_study_config
WHERE project_id = {project_id}
  AND study_accession = {study_accession}
  AND param_user = {currentuser}
  AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
GROUP BY study_accession, param_user
",
    currentuser     = currentuser,
    study_accession = study_accession,
    .con = conn
  )

  print(query)
  dbGetQuery(conn, query)
}

attach_antigen_familes <- function(best_pred_all, antigen_families, default_family = "All Antigens") {
  # Handle case where antigen_families is NULL or empty
  if (is.null(antigen_families) || nrow(antigen_families) == 0) {
    # Create antigen_family column with default value for all antigens
    best_pred_all$antigen_family <- default_family
    return(best_pred_all)
  }

  # Ensure antigen_families has required columns
  required_cols <- c("study_accession", "antigen", "antigen_family")
  if (!all(required_cols %in% names(antigen_families))) {
    # If required columns are missing, use default family
    best_pred_all$antigen_family <- default_family
    return(best_pred_all)
  }

  # Perform the merge
  pred_with_antigen_familes <- merge(best_pred_all,
                                     antigen_families[, required_cols],
                                     by = c("study_accession", "antigen"),
                                     all.x = TRUE)

  # Replace NA/NULL antigen_family values with the default
  if ("antigen_family" %in% names(pred_with_antigen_familes)) {
    na_family <- is.na(pred_with_antigen_familes$antigen_family) |
      pred_with_antigen_familes$antigen_family == "" |
      is.null(pred_with_antigen_familes$antigen_family)
    pred_with_antigen_familes$antigen_family[na_family] <- default_family
  } else {
    # Column doesn't exist after merge, add it with default
    pred_with_antigen_familes$antigen_family <- default_family
  }

  return(pred_with_antigen_familes)
}




