split_plate_nominal_sample_dilution <- function(
    study_accession,
    experiment_accession,
    plateid,
    conn
) {

  showNotification(id = "split_plate_notification", HTML("Splitting plate by nominal sample dilution<span class = 'dots'>"), duration = NULL)
  cat("Splitting plate by nominal sample dilution\n")
  cat("Study:", study_accession, "\n")
  cat("Experiment:", experiment_accession, "\n")
  cat("Plate:", plateid, "\n")

  # -----------------------------
  # Excluded antigens
  # -----------------------------
  exclude_antigens <- c("Well", "Type", "Description", "Region", "Gate", "Total", "% Agg Beads", "Sampling Errors", "Rerun Status",
                        "Device Error", "Plate ID", "Regions Selected", "RP1 Target", "Platform Heater Target", "Platform Temp (°C)",
                        "Bead Map", "Bead Count", "Sample Size (µl)", "Sample Timeout (sec)", "Flow Rate (µl/min)", "Air Pressure (psi)",
                        "Sheath Pressure (psi)", "Original DD Gates", "Adjusted DD Gates", "RP1 Gates", "User", "Access Level", "Acquisition Time",
                        "acquisition_time", "Reader Serial Number", "Platform Serial Number", "Software Version", "LXR Library", "Reader Firmware",
                        "Platform Firmware", "DSP Version", "Board Temp (°C)", "DD Temp (°C)", "CL1 Temp (°C)", "CL2 Temp (°C)", "DD APD (Volts)",
                        "CL1 APD (Volts)", "CL2 APD (Volts)", "High Voltage (Volts)", "RP1 PMT (Volts)", "DD Gain", "CL1 Gain", "CL2 Gain", "RP1 Gain")

  exclude_antigens_sql <- paste0("'", paste(exclude_antigens, collapse = "', '"), "'")

  # -----------------------------
  # Get nominal dilutions
  # -----------------------------
  print("PLate id check nominal\n")
  print(plateid)
  get_nominal <- glue::glue("
    SELECT DISTINCT nominal_sample_dilution
    FROM madi_results.xmap_header
    WHERE study_accession = '{study_accession}'
      AND experiment_accession = '{experiment_accession}'
      AND plateid = '{plateid}'
      AND nominal_sample_dilution IS NOT NULL;
  ")

  nominal_vals <- DBI::dbGetQuery(conn, get_nominal)$nominal_sample_dilution
  nominal_vals <- strsplit(nominal_vals, "\\|")[[1]]
  if (length(nominal_vals) <= 1) {
    showNotification("Plate has one or zero nominal dilutions — nothing to split.")
    return(invisible(NULL))
  }

  # -----------------------------
  # Helper: duplicate rows ONCE
  # -----------------------------
  duplicate_by_nominal <- function(table, extra_where = "") {

    # Get column names dynamically
    col_sql <- glue::glue("
      SELECT column_name
      FROM information_schema.columns
      WHERE table_schema = 'madi_results'
        AND table_name = '{table}'
      ORDER BY ordinal_position;
    ")

    cols <- DBI::dbGetQuery(conn, col_sql)$column_name

    # obtain primary key for the table
      pk_sql <- glue::glue("
    SELECT kcu.column_name
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu
      ON tc.constraint_name = kcu.constraint_name
     AND tc.table_schema = kcu.table_schema
    WHERE tc.constraint_type = 'PRIMARY KEY'
      AND tc.table_schema = 'madi_results'
      AND tc.table_name = '{table}';
  ")

    pk_cols <- DBI::dbGetQuery(conn, pk_sql)$column_name

    base_cols <- setdiff(cols, c(pk_cols,"nominal_sample_dilution"))

    select_cols <- paste(base_cols, collapse = ", ")
    insert_cols <- paste(c(base_cols, "nominal_sample_dilution"), collapse = ", ")

    for (dil in nominal_vals) {

      # on the mixed plate samples at one sample dilution will not be split into other sample dilutions.
      dilution_clause <- if (table == "xmap_sample") {
        glue("AND src.dilution::text = '{dil}'")
      } else {
        ""
      }

      sql <- glue::glue("
  INSERT INTO madi_results.{table} ({insert_cols})
  SELECT
    {select_cols},
    '{dil}' AS nominal_sample_dilution
  FROM madi_results.{table} src
  WHERE src.study_accession = '{study_accession}'
    AND src.experiment_accession = '{experiment_accession}'
    AND src.plateid = '{plateid}'
    AND src.nominal_sample_dilution LIKE '%|%'
    {extra_where}
    {dilution_clause}
    AND NOT EXISTS (
      SELECT 1
      FROM madi_results.{table} tgt
      WHERE tgt.study_accession = src.study_accession
        AND tgt.experiment_accession = src.experiment_accession
        AND tgt.plateid = src.plateid
        AND tgt.nominal_sample_dilution = '{dil}'
    );
")



      DBI::dbExecute(conn, sql)
    }
  }


  duplicate_by_nominal("xmap_header")

  # -----------------------------
  # SAMPLE
  # -----------------------------
  duplicate_by_nominal(
    "xmap_sample",
    glue("AND antigen NOT IN ({exclude_antigens_sql})"))

  # -----------------------------
  # STANDARD
  # -----------------------------
  duplicate_by_nominal(
    "xmap_standard",
    glue("AND antigen NOT IN ({exclude_antigens_sql})"))

  # -----------------------------
  # CONTROL
  # -----------------------------
  duplicate_by_nominal("xmap_control",
                       glue("AND antigen NOT IN ({exclude_antigens_sql})"))

  # -----------------------------
  # BUFFER / BLANK
  # -----------------------------
  duplicate_by_nominal("xmap_buffer",
                       glue("AND antigen NOT IN ({exclude_antigens_sql})"))

  showNotification(id = "split_plate_notification", "Plate successfully split by nominal sample dilution.", duration = NULL)
  removeNotification(id = "split_plate_notification")
  cat("Split completed.\n")
}
