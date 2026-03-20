subtract_wavelength_mfi <- function(df, study_accession, experiment_accession, plate,
                                    nominal_sample_dilution, wavelengths, mfi_col = "mfi",
                                    join_keys = NULL) {
  
  # === DIAGNOSTIC — remove once working ===
  cat("--- subtract_wavelength_mfi ---\n")
  cat("study:", study_accession, " experiment:", experiment_accession, 
      " plate:", plate, " dilution:", nominal_sample_dilution, "\n")
  cat("total rows in df:", nrow(df), "\n")
  cat("wavelength distribution in df:\n")
  print(table(df$plate, df$wavelength))
  # === END DIAGNOSTIC ===
  
  
  wl_parts <- strsplit(wavelengths, "\\|")[[1]]
  wl_low   <- as.character(min(as.numeric(wl_parts)))
  wl_high  <- as.character(max(as.numeric(wl_parts)))
  delta_experiment <- paste0(experiment_accession, "|D")
  
  # Convert integer64 to avoid merge issues
  for (col in names(df)) {
    if (inherits(df[[col]], "integer64")) df[[col]] <- as.character(df[[col]])
  }
  
  # Base filter
  base_filter <-
    df$study_accession      == study_accession      &
    df$experiment_accession == experiment_accession &
    df$plate                == plate
  
  if ("nominal_sample_dilution" %in% names(df) &&
      !all(is.na(df$nominal_sample_dilution))) {
    base_filter <- base_filter & df$nominal_sample_dilution == nominal_sample_dilution
  }
  
  df <- df[base_filter & !is.na(base_filter), ]
  cat("nrow after plate filter:", nrow(df), "\n")
  
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  if (!"wavelength" %in% names(df)) return(data.frame())
  
  df_low  <- df[df$wavelength == wl_low,  ]
  df_high <- df[df$wavelength == wl_high, ]
  cat("nrow df_low:", nrow(df_low), " nrow df_high:", nrow(df_high), "\n")
  
  if (nrow(df_low) == 0 || nrow(df_high) == 0) return(data.frame())
  
  # Use explicit join keys if provided, otherwise fall back to setdiff
  # (setdiff works for standard/control/buffer where row IDs are consistent)
  if (!is.null(join_keys)) {
    # Validate all join keys exist in df
    missing_keys <- setdiff(join_keys, names(df_low))
    if (length(missing_keys) > 0) {
      cat("WARNING: missing join keys:", paste(missing_keys, collapse = ", "), "\n")
      return(data.frame())
    }
    key_cols <- join_keys
  } else {
    # Columns that are row-level identifiers — never valid join keys
    # cols_to_exclude <- c(
    #   mfi_col, "wavelength",
    #   "well", "sampleid", "xmap_sample_id", "plate_id", "plateid"
    # )
    cols_to_exclude <- c(
      mfi_col, "wavelength",
      "well", "sampleid",
      "xmap_sample_id", "xmap_buffer_id", "xmap_control_id", "xmap_standard_id",
      "n",
      "plate_id", "plateid"
    )
    
    key_cols <- setdiff(names(df_low), cols_to_exclude)
  }
  
  cat("key_cols:", paste(key_cols, collapse = ", "), "\n")
  cat("duplicate keys df_low:",  sum(duplicated(df_low[,  key_cols])), "\n")
  cat("duplicate keys df_high:", sum(duplicated(df_high[, key_cols])), "\n")
  
  df_merged <- merge(
    df_low,
    df_high[, c(key_cols, mfi_col)],
    by       = key_cols,
    suffixes = c("_low", "_high")
  )
  cat("nrow df_merged:", nrow(df_merged), "\n")
  
  if (nrow(df_merged) == 0) return(data.frame())
  
  df_merged[[mfi_col]]   <- df_merged[[paste0(mfi_col, "_low")]] - df_merged[[paste0(mfi_col, "_high")]]
  df_merged[[paste0(mfi_col, "_low")]]  <- NULL
  df_merged[[paste0(mfi_col, "_high")]] <- NULL
  df_merged$wavelength                  <- "delta"
  df_merged$experiment_accession        <- delta_experiment
  
  return(df_merged)
}


# get_insertable_cols <- function(conn, schema, table_name) {
#   col_sql <- glue::glue("
#     SELECT column_name
#     FROM information_schema.columns
#     WHERE table_schema = '{schema}'
#       AND table_name   = '{table_name}'
#       AND column_default  IS NULL OR NOT LIKE 'nextval%'  -- excludes auto-generated serial/sequence PKs
#     ORDER BY ordinal_position;
#   ")
#   DBI::dbGetQuery(conn, col_sql)$column_name
# }

get_insertable_cols <- function(conn, schema, table_name) {
  col_sql <- glue::glue("
    SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = '{schema}'
      AND table_name   = '{table_name}'
      AND column_name NOT IN (
        SELECT column_name 
        FROM information_schema.columns c
        WHERE c.table_schema = '{schema}'
          AND c.table_name   = '{table_name}'
          AND pg_get_serial_sequence('{schema}.' || '{table_name}', c.column_name) IS NOT NULL
      )
    ORDER BY ordinal_position;
  ")
  DBI::dbGetQuery(conn, col_sql)$column_name
}

insert_delta_sql <- function(conn, schema, table_name, df) {
  
  # Rename R columns back to DB column names
  names(df)[names(df) == "mfi"] <- "antibody_mfi"
  names(df)[names(df) == "n"]   <- "antibody_n"
  
  # Dynamically get insertable columns from DB
  insert_cols <- get_insertable_cols(conn, schema, table_name)
  cat("DB cols for", table_name, ":\n", paste(insert_cols, collapse = ", "), "\n")
  cat("df cols:\n", paste(names(df), collapse = ", "), "\n")
  
  # Only use cols that exist in both DB and dataframe
  use_cols <- intersect(insert_cols, names(df))
  cat("inserting into", table_name, "- cols:", paste(use_cols, collapse = ", "), "\n")
  
  col_list <- paste(use_cols, collapse = ", ")
  
  # Build VALUES rows
  rows <- apply(df[, use_cols, drop = FALSE], 1, function(row) {
    vals <- sapply(row, function(v) {
      if (is.na(v)) "NULL"
      else paste0("'", gsub("'", "''", as.character(v)), "'")
    })
    paste0("(", paste(vals, collapse = ", "), ")")
  })
  
  values_sql <- paste(rows, collapse = ",\n")
  
  sql <- glue::glue("
    INSERT INTO {schema}.{table_name} ({col_list})
    VALUES {values_sql};
  ")
  
  DBI::dbExecute(conn, sql)
}

# get_db_columns <- function(conn, schema, table_name) {
#   query <- glue::glue("
#     SELECT column_name 
#     FROM information_schema.columns 
#     WHERE table_schema = '{schema}' 
#       AND table_name   = '{table_name}'
#       AND column_default NOT LIKE 'nextval%'  -- excludes auto-generated serial/sequence columns
#   ")
#   DBI::dbGetQuery(conn, query)$column_name
# }
# 
# insert_delta <- function(conn, schema, table_name, df) {
#   db_cols  <- get_db_columns(conn, schema, table_name)
#   df_cols  <- intersect(db_cols, names(df))  # only cols that exist in both
#   
#   cat("inserting columns:", paste(df_cols, collapse = ", "), "\n")
#   
#   df_to_insert <- df[, df_cols, drop = FALSE]
#   DBI::dbWriteTable(conn, 
#                     name       = DBI::Id(schema = schema, table = table_name),
#                     value      = df_to_insert, 
#                     append     = TRUE, 
#                     row.names  = FALSE)
# }



# subtract_wavelength_mfi <- function(df, study_accession, experiment_accession, plate, 
#                                     nominal_sample_dilution, wavelengths, mfi_col = "mfi") {
#   print(str(df))
#   wl_parts <- strsplit(wavelengths, "\\|")[[1]]
#   wl_low   <- as.character(min(as.numeric(wl_parts)))
#   wl_high  <- as.character(max(as.numeric(wl_parts)))
#   
#   delta_experiment <- paste0(experiment_accession, "_D")
#   
#   cat("--- subtract_wavelength_mfi ---\n")
#   cat("wl_low:", wl_low, " wl_high:", wl_high, "\n")
#   cat("filtering on plate:", plate, " nominal_sample_dilution:", nominal_sample_dilution, "\n")
#   
#   # Build filter — only apply nominal_sample_dilution if column exists and is populated
#   base_filter <-
#     df$study_accession      == study_accession      &
#     df$experiment_accession == experiment_accession &
#     df$plate                == plate
#   
#   if ("nominal_sample_dilution" %in% names(df)) {
#     base_filter <- base_filter & df$nominal_sample_dilution == nominal_sample_dilution
#   }
#   
#   df <- df[base_filter, ]
#   
#   cat("nrow after plate filter:", nrow(df), "\n")
#   
#   # Empty input — return empty df preserving structure
#   if (is.null(df) || nrow(df) == 0) return(data.frame())
#   
#   if (!"wavelength" %in% names(df)) {
#     cat("no wavelength column found\n")
#     return(data.frame())
#   }
#   
#   cat("unique wavelengths in filtered df:", paste(unique(df$wavelength), collapse = ", "), "\n")
#   
#   df_low  <- df[df$wavelength == wl_low,  ]
#   df_high <- df[df$wavelength == wl_high, ]
#   
#   cat("nrow df_low:", nrow(df_low), " nrow df_high:", nrow(df_high), "\n")
#   
#   # No matching wavelength rows — return empty df
#   if (nrow(df_low) == 0 || nrow(df_high) == 0) return(data.frame())
#   
#   
#   key_cols <- setdiff(names(df_low), c(mfi_col, "wavelength"))
#   # key_cols <- c("study_accession", "experiment_accession", "plate",
#   #               "nominal_sample_dilution", "patientid", "timeperiod",
#   #               "antigen", "feature", "stype")
#   # 
#   # After splitting df_low and df_high, before merge:
#   cat("key_cols:", paste(key_cols, collapse = ", "), "\n")
#   cat("duplicate keys df_low:", sum(duplicated(df_low[, key_cols])), "\n")
#   cat("duplicate keys df_high:", sum(duplicated(df_high[, key_cols])), "\n")
#   
#   # Find wells in 450 but not 620 and vice versa
#   wells_low  <- df_low$well
#   wells_high <- df_high$well
#   cat("wells in 450 only:", paste(setdiff(wells_low, wells_high), collapse = ", "), "\n")
#   cat("wells in 620 only:", paste(setdiff(wells_high, wells_low), collapse = ", "), "\n")
#   
#   # Do sampleids appear at both wavelengths?
#   sampleids_low <- df$sampleid[df$wavelength == wl_low]
#   sampleids_high <- df$sampleid[df$wavelength == wl_high]
#   cat("sampleids in both:", length(intersect(sampleids_low, sampleids_high)), "\n")
#   cat("sampleids in low wl only:", length(setdiff(sampleids_low, sampleids_high)), "\n")
#   cat("sampleids in high wl only:", length(setdiff(sampleids_high, sampleids_low)), "\n")
#   df_merged <- merge(
#     df_low,
#     df_high[, c(key_cols, mfi_col)],
#     by       = key_cols,
#     suffixes = c("_low", "_high")
#   )
#   
#   cat("nrow df_merged:", nrow(df_merged), "\n")
#   
#   # Merge produced nothing — return empty df
#   if (nrow(df_merged) == 0) return(data.frame())
#   
#   df_merged[[mfi_col]]                  <- df_merged[[paste0(mfi_col, "_low")]] - df_merged[[paste0(mfi_col, "_high")]]
#   df_merged[[paste0(mfi_col, "_low")]]  <- NULL
#   df_merged[[paste0(mfi_col, "_high")]] <- NULL
#   df_merged$wavelength                  <- "delta"
#   df_merged$experiment_accession        <- delta_experiment
#   
#   return(df_merged)
# }








# subtract_wavelength_mfi <- function(df, study_accession, experiment_accession, plate, 
#                                     nominal_sample_dilution, wavelengths, mfi_col = "mfi") {
#   
#   # Parse wavelengths — keep as CHARACTER to match how they're stored in the df
#   wl_parts <- strsplit(wavelengths, "\\|")[[1]]
#   wl_low   <- as.character(min(as.numeric(wl_parts)))   # "450"
#   wl_high  <- as.character(max(as.numeric(wl_parts)))   # "620"
#   
#   delta_experiment <- paste0(experiment_accession, "_D")
#   
#   cat("--- subtract_wavelength_mfi ---\n")
#   cat("wl_low:", wl_low, " wl_high:", wl_high, "\n")
#   cat("filtering on plate:", plate, " nominal_sample_dilution:", nominal_sample_dilution, "\n")
#   
#   # Filter to the relevant plate
#   df <- df[
#     df$study_accession           == study_accession        &
#       df$experiment_accession      == experiment_accession   &
#       df$plate                     == plate                  &
#       df$nominal_sample_dilution   == nominal_sample_dilution,
#   ]
#   
#   cat("nrow after plate filter:", nrow(df), "\n")
#   if (is.null(df) || nrow(df) == 0) return(NULL)
#   
#   if (!"wavelength" %in% names(df)) {
#     cat("no wavelength column found\n")
#     return(NULL)
#   }
#   
#   cat("unique wavelengths in filtered df:", paste(unique(df$wavelength), collapse = ", "), "\n")
#   
#   df_low  <- df[df$wavelength == wl_low,  ]
#   df_high <- df[df$wavelength == wl_high, ]
#   
#   cat("nrow df_low:", nrow(df_low), " nrow df_high:", nrow(df_high), "\n")
#   if (nrow(df_low) == 0 || nrow(df_high) == 0) return(NULL)
#   
#   key_cols  <- setdiff(names(df_low), c(mfi_col, "wavelength"))
#   
#   df_merged <- merge(
#     df_low,
#     df_high[, c(key_cols, mfi_col)],
#     by       = key_cols,
#     suffixes = c("_low", "_high")
#   )
#   
#   cat("nrow df_merged:", nrow(df_merged), "\n")
#   if (nrow(df_merged) == 0) return(NULL)
#   
#   df_merged[[mfi_col]]                  <- df_merged[[paste0(mfi_col, "_low")]] -
#     df_merged[[paste0(mfi_col, "_high")]]
#   df_merged[[paste0(mfi_col, "_low")]]  <- NULL
#   df_merged[[paste0(mfi_col, "_high")]] <- NULL
#   df_merged$wavelength                  <- "delta"
#   df_merged$experiment_accession        <- delta_experiment
#   
#   return(df_merged)
# }

# subtract_wavelength_mfi <- function(df, study_accession, experiment_accession, plate, nominal_sample_dilution, wavelengths, mfi_col = "mfi") {
#   
#   # Parse wavelengths inside the function
#   wl_parts   <- strsplit(wavelengths, "\\|")[[1]]
#   wl_numeric <- as.numeric(wl_parts)
#   wl_low     <- min(wl_numeric)   # e.g. 450
#   wl_high    <- max(wl_numeric)   # e.g. 620
#   
#   delta_experiment <- paste0(experiment_accession, "_D")
#   
#   # Filter to the relevant plate
#   df <- df[
#     df$study_accession      == study_accession &
#       df$experiment_accession == experiment_accession &
#       df$plate              == plate &
#       df$nominal_sample_dilution == nominal_sample_dilution, 
#   ]
#   
#   if (is.null(df) || nrow(df) == 0)  return(NULL)
#   if (!"wavelength" %in% names(df))  return(NULL)
#   
#   df_low  <- df[df$wavelength == wl_low,  ]
#   df_high <- df[df$wavelength == wl_high, ]
#   
#   if (nrow(df_low) == 0 || nrow(df_high) == 0) return(NULL)
#   
#   key_cols <- setdiff(names(df_low), c(mfi_col, "wavelength"))
#   
#   df_merged <- merge(
#     df_low,
#     df_high[, c(key_cols, mfi_col)],
#     by       = key_cols,
#     suffixes = c("_low", "_high")
#   )
#   
#   df_merged[[mfi_col]]                  <- df_merged[[paste0(mfi_col, "_low")]] -
#     df_merged[[paste0(mfi_col, "_high")]]
#   df_merged[[paste0(mfi_col, "_low")]]  <- NULL
#   df_merged[[paste0(mfi_col, "_high")]] <- NULL
#   df_merged$wavelength                  <- "delta"
#   df_merged$experiment_accession        <- delta_experiment
#   
#   return(df_merged)
# }