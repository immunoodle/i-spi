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

fetch_study_parameters <- function(study_accession, param_user, param_group = "standard_curve_options", conn) {
  query <- glue("
  SELECT study_accession, param_name, param_boolean_value, param_character_value
	FROM madi_results.xmap_study_config
  WHERE study_accession = '{study_accession}'
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

fetch_antigen_parameters <- function(study_accession, experiment_accession, conn) {
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
  WHERE study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';
")
  antigen_constraints <- dbGetQuery(conn, query)
  return(antigen_constraints=antigen_constraints)
}


fetch_db_header <- function(study_accession, experiment_accession, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, sample_dilution_factor, plate_id,
  assay_response_variable, assay_independent_variable
  FROM madi_results.xmap_header
WHERE study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

fetch_db_standards <- function(study_accession, experiment_accession, conn) {
  query <- glue("SELECT study_accession, experiment_accession, feature, plate_id, stype, source, sampleid, well, dilution, antigen, antibody_mfi AS mfi FROM madi_results.xmap_standard
WHERE study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  standard_df  <- dbGetQuery(conn, query)
  standard_df <- distinct(standard_df)
  return(standard_df)
}

fetch_db_buffer <- function(study_accession, experiment_accession, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plate_id, stype, well, antigen, dilution, feature, antibody_mfi AS mfi FROM madi_results.xmap_buffer
WHERE study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  blank_data <- dbGetQuery(conn, query)
  blank_data <- distinct(blank_data)
  return(blank_data)
}

fetch_db_samples <- function(study_accession, experiment_accession, conn) {
  query <- glue("SELECT study_accession,
experiment_accession, plate_id, timeperiod, patientid,
well, stype, sampleid,  agroup, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi AS mfi,
antibody_n FROM madi_results.xmap_sample
WHERE study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  sample_data <- dbGetQuery(conn, query)
  sample_data <- distinct(sample_data)
  return(sample_data)
}

pull_data <- function(study_accession, experiment_accession, conn = conn) {
  plates <- fetch_db_header(study_accession = study_accession,
                            experiment_accession = experiment_accession,
                            conn = conn)
  plates$plate_id <- trimws(plates$plate_id)

  antigen_constraints <- fetch_antigen_parameters(study_accession = study_accession,
                                                  experiment_accession = experiment_accession,
                                                  conn = conn)
  standard_curve_data <- fetch_db_standards(study_accession = study_accession,
                                            experiment_accession = experiment_accession,
                                            conn = conn)
  standard_curve_data$plate_id <- trimws(standard_curve_data$plate_id)

  standards <- inner_join(standard_curve_data, plates, by = c("study_accession", "experiment_accession","plate_id"))[ ,
                                                                                                                      c("study_accession","experiment_accession","feature","source","plateid","plate", "stype","sample_dilution_factor",
                                                                                                                        "sampleid","well","dilution","antigen","mfi",
                                                                                                                        "assay_response_variable", "assay_independent_variable")]
  blanks <- inner_join(fetch_db_buffer(study_accession = study_accession,
                                       experiment_accession = experiment_accession,
                                       conn = conn) %>% dplyr::mutate(plate_id = trimws(as.character(plate_id))),
                       plates, by = c("study_accession", "experiment_accession","plate_id"))
  samples <- inner_join(fetch_db_samples(study_accession = study_accession,
                                         experiment_accession = experiment_accession,
                                         conn = conn) %>% dplyr::mutate(plate_id = trimws(as.character(plate_id))),
                        plates, by = c("study_accession", "experiment_accession","plate_id"))
  response_var = unique(plates$assay_response_variable)
  indep_var = unique(plates$assay_independent_variable)

  return(list(plates=plates, standards=standards,
              blanks=blanks, samples=samples,
              antigen_constraints=antigen_constraints,
              response_var = response_var,
              indep_var = indep_var)
  )
}

# shiny_notify <- function(session = shiny::getDefaultReactiveDomain()) {
#   function(msg, type = c("message","warning","error")) {
#     type <- match.arg(type)
#     shiny::showNotification(msg,
#                             type = switch(type, message = "message", warning = "warning", error = "error"),
#                             session = session
#     )
#   }
# }

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
                              quiet  = FALSE) {

  ## ----------------------------
  ## Notifier
  ## ----------------------------
  if (is.null(notify)) {
    notify <- function(msg) {
      if (!quiet) message(as.character(msg))
    }
  }

  bail <- function(msg) {
    notify(msg)
    invisible(FALSE)
  }

  ## ----------------------------
  ## Basic checks
  ## ----------------------------
  if (!DBI::dbIsValid(conn)) {
    return(bail("Database connection is not valid."))
  }

  if (!is.data.frame(df) || nrow(df) == 0) {
    return(bail("No data provided for upsert."))
  }

  cols <- names(df)

  ## ----------------------------
  ## Natural keys by table
  ## ----------------------------
  nk <- get_natural_keys(table)

  if (is.null(nk)) {
    stop("Unknown table: ", table)
  }

  ## ----------------------------
  ## Validate natural keys
  ## ----------------------------
  missing_keys <- setdiff(nk, cols)
  if (length(missing_keys) > 0) {
    stop("Missing natural-key columns for ", table, ": ",
         paste(missing_keys, collapse = ", "))
  }

  if (anyNA(df[, nk, drop = FALSE])) {
    bad <- which(!complete.cases(df[, nk, drop = FALSE]))
    print(df[bad, nk, drop = FALSE])
    return(bail("Natural-key columns contain NA; cannot upsert."))
  }

  ## ----------------------------
  ## Optional: verify UNIQUE index exists
  ## ----------------------------
  idx_check_sql <- glue::glue_sql(
    "SELECT 1
     FROM pg_index i
     JOIN pg_class t ON t.oid = i.indrelid
     JOIN pg_namespace n ON n.oid = t.relnamespace
     WHERE n.nspname = {schema}
       AND t.relname = {table}
       AND i.indisunique
     LIMIT 1",
    .con = conn
  )

  if (nrow(DBI::dbGetQuery(conn, idx_check_sql)) == 0) {
    return(bail(paste0("No UNIQUE index found for ", schema, ".", table,
                       ". Run schema migrations first.")))
  }

  ## ----------------------------
  ## Transaction
  ## ----------------------------
  ok <- tryCatch({

    DBI::dbWithTransaction(conn, {

      tmp_name <- paste0("tmp_upsert_", table, "_", sample.int(1e9, 1))

      ## Create temp table using glue_sql
      ## Note: Use DBI::SQL() for identifiers that need special handling
      schema_id <- DBI::dbQuoteIdentifier(conn, schema)
      table_id <- DBI::dbQuoteIdentifier(conn, table)
      tmp_id <- DBI::dbQuoteIdentifier(conn, tmp_name)

      create_temp_sql <- glue::glue_sql(
        "CREATE TEMP TABLE {DBI::SQL(tmp_id)}
         (LIKE {DBI::SQL(schema_id)}.{DBI::SQL(table_id)} INCLUDING DEFAULTS)
         ON COMMIT DROP",
        .con = conn
      )

      DBI::dbExecute(conn, create_temp_sql)

      ## Bulk write to temp table
      DBI::dbWriteTable(
        conn,
        name = tmp_name,
        value = df,
        append = TRUE,
        row.names = FALSE
      )

      ## Build and execute UPSERT statement
      upsert_sql <- build_upsert_sql_glue(
        conn = conn,
        schema = schema,
        table = table,
        tmp_name = tmp_name,
        cols = cols,
        nk = nk
      )

      DBI::dbExecute(conn, upsert_sql)
    })

    TRUE

  }, error = function(e) {
    notify(paste0(table, " upsert failed: ", conditionMessage(e)))
    FALSE
  })

  if (ok) {
    notify(paste0(table, " upsert completed (", nrow(df), " rows)."))
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

## ----------------------------
## Helper: Get natural keys for table
## ----------------------------
get_natural_keys <- function(table) {
  keys <- list(
    best_plate_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "sample_dilution_factor", "source"
    ),
    best_glance_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "sample_dilution_factor", "source", "antigen"
    ),
    best_tidy_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "sample_dilution_factor", "source", "antigen", "term"
    ),
    best_sample_se_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "sample_dilution_factor", "source", "antigen",
      "patientid", "timeperiod", "sampleid", "dilution",
       "uid"
    ),
    best_standard_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "sample_dilution_factor", "source", "antigen", "feature", "well"
    ), # dilution not included as it can be NA when geometric mean is used
    best_pred_all = c(
      "study_accession", "experiment_accession",
      "plateid", "plate", "sample_dilution_factor", "source", "antigen",
       "id_match"
    )
  )

  keys[[table]]
}

## ----------------------------
## Helper: Build UPSERT SQL using glue_sql
## ----------------------------
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
                                 sample_dilution_factor = sample_dilution_factor,
                                 antigen_constraints = antigen_constraints)
{
  plate_standard  <- loaded_data$standards[loaded_data$standards$source == source &
                                             loaded_data$standards$antigen == antigen &
                                             loaded_data$standards$plate == plate &
                                             loaded_data$standards$sample_dilution_factor == sample_dilution_factor,]

  plate_blanks <- loaded_data$blanks[loaded_data$blanks$antigen == antigen &
                                       loaded_data$blanks$plate == plate &
                                       loaded_data$blanks$sample_dilution_factor == sample_dilution_factor,]

  plate_samples <- loaded_data$samples[loaded_data$samples$antigen == antigen &
                                         loaded_data$samples$plate == plate &
                                         loaded_data$samples$sample_dilution_factor == sample_dilution_factor,]

  antigen_settings <- obtain_lower_constraint(dat = plate_standard,
                                              antigen = antigen,
                                              study_accession = study_accession,
                                              experiment_accession = experiment_accession,
                                              plate = plate,

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
fetch_best_plate_all <- function(study_accession, conn) {
  query <- glue("
SELECT best_plate_all_id, study_accession, experiment_accession, feature, source, plateid, plate, sample_dilution_factor, assay_response_variable, assay_independent_variable
	FROM madi_results.best_plate_all
	WHERE study_accession = '{study_accession}';
")
  best_plate_all <- dbGetQuery(conn, query)
  return(best_plate_all)
}

fetch_best_tidy_all <- function(study_accession, conn) {
  query <- glue("SELECT best_tidy_all_id, study_accession, experiment_accession, term, lower, upper, estimate, std_error, statistic, p_value, sample_dilution_factor, antigen, plateid, plate, source
	FROM madi_results.best_tidy_all
	WHERE study_accession = '{study_accession}'")
  best_tidy_all <- dbGetQuery(conn, query)
  return(best_tidy_all)
}
fetch_best_pred_all <- function(study_accession, conn) {
  query <- glue("SELECT best_pred_all_id, x, model, yhat, overall_se, predicted_concentration, se_x, pcov, study_accession, experiment_accession, sample_dilution_factor, plateid, plate, antigen, source, id_match
	FROM madi_results.best_pred_all
	WHERE study_accession = '{study_accession}';")
  best_pred_all <- dbGetQuery(conn, query)
  return(best_pred_all)
}

fetch_best_standard_all <- function(study_accession, conn) {
  query <- glue("SELECT best_standard_all_id, study_accession, experiment_accession, feature, source, plateid, plate, stype, sample_dilution_factor, sampleid, well, dilution, antigen, assay_response, assay_response_variable, assay_independent_variable, concentration, g
	FROM madi_results.best_standard_all
	WHERE study_accession = '{study_accession}';")
  best_standard_all <- dbGetQuery(conn, query)
  return(best_standard_all)
}

fetch_best_glance_all <- function(study_accession, conn) {
  query <- glue("SELECT best_glance_all_id, study_accession, experiment_accession, plateid, plate, sample_dilution_factor, antigen, iter, status, crit, a, b, c, d, lloq, uloq, lloq_y, uloq_y, llod, ulod, inflect_x, inflect_y, std_error_blank, dydx_inflect, dfresidual, nobs, rsquare_fit, aic, bic, loglik, mse, cv, source, bkg_method, is_log_response, is_log_x, apply_prozone, formula, g
	FROM madi_results.best_glance_all
	WHERE study_accession = '{study_accession}';")
  best_glance_all <- dbGetQuery(conn, query)
  return(best_glance_all)
}


attach_antigen_familes <- function(best_pred_all, antigen_families) {
  pred_with_antigen_familes <- merge(best_pred_all, antigen_families[, c("study_accession", "antigen", "antigen_family")],
                                     by = c("study_accession", "antigen"), all.x = T)
  return(pred_with_antigen_familes)

}
