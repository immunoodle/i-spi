# ==============================================================================
# xPonentReader.R - Batch xPONENT file processing
# ==============================================================================
# This module provides batch xPONENT file upload and template generation that
# mirrors the Raw File workflow. Multiple xPONENT CSV files are read, converted
# to the same wide-format data structure used by Raw File processing, and then
# the standard generate_layout_template() function is reused for downstream
# layout generation, validation, and batch upload.
# ==============================================================================

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Convert a single xPONENT file to wide-format plate data
#'
#' Takes the output of moach::read_Xponent_csv() and pivots it from long format
#' (one row per well x analyte) to wide format (one row per well, one column per
#' analyte MFI value), matching the structure produced by raw .xlsx plate files.
#'
#' @param lumcsv Output of moach::read_Xponent_csv()
#' @param file_name Original file name for the source_file column
#' @return Data frame in wide format with columns: source_file, Well, Type,
#'         Description, and one column per analyte (Net MFI values)
#'
xponent_to_wide_plate_data <- function(lumcsv, file_name) {

  # Extract the expression data and well information
  raw_exprs <- lumcsv$AssayData$Exprs
  raw_wells <- lumcsv$AssayData$Wells

  # Join expression data with well metadata
  combined <- raw_exprs[, c("Location", "Sample", "Analyte", "Count", "Net_MFI")] %>%
    dplyr::full_join(
      raw_wells[, c("Location", "Sample")],
      by = c("Location", "Sample")
    ) %>%
    janitor::clean_names()

  # Pivot to wide format: one column per analyte with Net_MFI values
  wide_data <- combined %>%
    dplyr::select(location, sample, analyte, net_mfi) %>%
    tidyr::pivot_wider(
      id_cols = c(location, sample),
      names_from = analyte,
      values_from = net_mfi,
      values_fn = list(net_mfi = dplyr::first)
    )

  # Map xPONENT columns to the standard raw file format
  # The raw format expects: Well, Type, Description, then antigen columns
  plate_data <- wide_data %>%
    dplyr::rename(
      Well = location,
      Description = sample
    )

  # Infer Type from the sample name (xPONENT uses naming conventions)
  # Standards often start with "S", Controls with "C", Blanks with "B"
  # Samples are everything else (Type = "X")
  plate_data$Type <- vapply(plate_data$Description, function(desc) {
    if (is.na(desc) || desc == "") return("X")
    desc_upper <- toupper(trimws(desc))

    # Check for standard patterns
    if (grepl("^S[0-9]", desc_upper) || grepl("^STD", desc_upper, ignore.case = TRUE)) {
      return(paste0("S", gsub("[^0-9]", "", substr(desc_upper, 1, 5))))
    }
    if (grepl("^C[0-9]", desc_upper) || grepl("^CTRL", desc_upper, ignore.case = TRUE) ||
        grepl("^CONTROL", desc_upper, ignore.case = TRUE)) {
      return(paste0("C", gsub("[^0-9]", "", substr(desc_upper, 1, 5))))
    }
    if (grepl("^B[0-9]?$", desc_upper) || grepl("^BLANK", desc_upper, ignore.case = TRUE) ||
        grepl("^BACKGROUND", desc_upper, ignore.case = TRUE)) {
      return("B")
    }
    return("X")
  }, character(1))

  # Add source file column
  plate_data$source_file <- file_name

  # Reorder: source_file, Well, Type, Description, then antigens
  antigen_cols <- setdiff(names(plate_data), c("source_file", "Well", "Type", "Description"))
  plate_data <- plate_data[, c("source_file", "Well", "Type", "Description", antigen_cols)]

  return(plate_data)
}


#' Extract header metadata from an xPONENT file
#'
#' Creates a header data frame matching the structure expected by
#' generate_layout_template() and the batch upload workflow.
#'
#' @param lumcsv Output of moach::read_Xponent_csv()
#' @param file_name Original file name
#' @return Data frame with one row of header metadata
#'
extract_xponent_header <- function(lumcsv, file_name) {

  batch_info <- lumcsv$BatchHeader$BatchInfo

  # Build plate_id from batch name (consistent with existing xPONENT logic)
  plate_id_raw <- file.path("E:", "batch",
                            paste0(gsub("[[:punct:][:blank:]]+", ".", batch_info$Batch), ".csv"))

  # Extract acquisition date and normalize to DB format
  acq_date <- tryCatch({
    raw_date <- as.character(batch_info$Date)
    normalize_acquisition_date(raw_date)
  }, error = function(e) NA_character_)

  # Extract serial number
  serial_number <- tryCatch({
    batch_info$SN
  }, error = function(e) NA_character_)

  data.frame(
    source_file = file_name,
    file_name = file_name,
    plateid = clean_plate_id(plate_id_raw),
    acquisition_date = as.character(acq_date),
    reader_serial_number = as.character(serial_number),
    rp1_pmt_volts = NA_character_,
    rp1_target = NA_character_,
    stringsAsFactors = FALSE
  )
}


#' Build assay_response_long from a single xPONENT CSV
#'
#' Extracts the long-format expression data (Net_MFI + bead count per well per
#' analyte) directly from the parsed xPONENT object, BEFORE any wide pivot.
#' This preserves bead counts that are lost in the wide plate data.
#'
#' @param lumcsv Parsed xPONENT object from moach::read_Xponent_csv()
#' @param file_name Original file name for the source_file column
#' @return Data frame with columns: source_file, well, antigen, feature,
#'         assay_response, assay_bead_count
#'
extract_xponent_assay_response_long <- function(lumcsv, file_name) {

  raw_exprs <- lumcsv$AssayData$Exprs

  # Build long format directly from expression data
  assay_long <- raw_exprs[, c("Location", "Analyte", "Net_MFI", "Count")] %>%
    dplyr::rename(
      well             = Location,
      antigen          = Analyte,
      assay_response   = Net_MFI,
      assay_bead_count = Count
    ) %>%
    dplyr::mutate(
      source_file      = file_name,
      feature          = "Net_MFI",
      assay_response   = as.numeric(assay_response),
      assay_bead_count = as.integer(assay_bead_count)
    )

  return(assay_long)
}


#' Process multiple xPONENT files into the standard batch format
#'
#' This is the xPONENT equivalent of process_experiment_files(). It reads
#' multiple xPONENT CSV files, converts each to wide format, and combines
#' them. The output format matches what process_experiment_files() produces,
#' so all downstream processing (layout generation, validation, upload) works
#' identically.
#'
#' @param upload_df Data frame from Shiny fileInput (columns: name, datapath, etc.)
#' @return List with combined_plates, header_list, plate_list, assay_response_long
#'
process_xponent_files <- function(upload_df) {
  cat("Processing", nrow(upload_df), "xPONENT files...\n")

  results <- lapply(seq_len(nrow(upload_df)), function(i) {
    file_path <- upload_df$datapath[i]
    file_name <- upload_df$name[i]
    cat("  Processing xPONENT:", file_name, "\n")

    tryCatch({
      # Read xPONENT CSV using moach
      lumcsv <- moach::read_Xponent_csv(file_path)

      # Convert to wide plate data
      plate_data <- xponent_to_wide_plate_data(lumcsv, file_name)
      cat("    -> Columns:", ncol(plate_data), ", Rows:", nrow(plate_data), "\n")

      # Extract header metadata
      header_data <- extract_xponent_header(lumcsv, file_name)

      # Extract long-format assay response (preserves bead counts)
      assay_long <- extract_xponent_assay_response_long(lumcsv, file_name)

      list(
        plate = plate_data,
        header = header_data,
        assay_long = assay_long,
        file_name = file_name
      )
    }, error = function(e) {
      cat("    x ERROR:", conditionMessage(e), "\n")
      showNotification(
        paste("Failed to read xPONENT file:", file_name, "-", conditionMessage(e)),
        type = "error", duration = 10
      )
      return(NULL)
    })
  })

  # Remove failed files
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    stop("No valid xPONENT files could be processed")
  }

  # Extract headers and plates
  names(results) <- sapply(results, `[[`, "file_name")
  header_list <- lapply(results, `[[`, "header")
  plate_list <- lapply(results, `[[`, "plate")
  assay_long_list <- lapply(results, `[[`, "assay_long")

  # Assign plate numbers (reuse existing function)
  cat("\nExtracting plate numbers...\n")
  header_list <- assign_plate_numbers(header_list)

  # Combine all plates
  cat("\nCombining xPONENT plate data...\n")
  combined_plates <- dplyr::bind_rows(plate_list)

  # Add plateid from source_file
  if ("source_file" %in% names(combined_plates)) {
    combined_plates$plateid <- clean_plate_id(combined_plates$source_file)
  }

  # Combine assay_response_long across files and add plateid
  cat("Combining xPONENT assay_response_long...\n")
  assay_response_long <- dplyr::bind_rows(assay_long_list)
  if ("source_file" %in% names(assay_response_long)) {
    assay_response_long$plateid <- clean_plate_id(assay_response_long$source_file)
  }
  cat("  -> assay_response_long:", nrow(assay_response_long), "rows x",
      ncol(assay_response_long), "cols\n")

  cat("  -> Combined dimensions:", nrow(combined_plates), "rows x", ncol(combined_plates), "cols\n")
  cat("  -> Source files:", paste(unique(combined_plates$source_file), collapse = ", "), "\n")

  list(
    combined_plates = combined_plates,
    header_list = header_list,
    plate_list = plate_list,
    assay_response_long = assay_response_long
  )
}


# ==============================================================================
# SERVER-SIDE OBSERVERS
# ==============================================================================

# Local reactive to store xPONENT assay_response_long (preserves bead counts
# that are lost in the wide-format batch_plate_data)
xponent_assay_response_long <- reactiveVal(NULL)

# ---- Observer: xPONENT batch file upload ----
observeEvent(input$upload_xponent_experiment_files, {
  req(input$upload_xponent_experiment_files)
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  cat("\n======================================================\n")
  cat("         NEW xPONENT EXPERIMENT FILES UPLOAD\n")
  cat("======================================================\n")
  cat("Study:", input$readxMap_study_accession, "\n")
  cat("Experiment:", input$readxMap_experiment_accession_import, "\n")
  cat("Files to upload:", nrow(input$upload_xponent_experiment_files), "\n\n")

  # RESET ALL BATCH REACTIVES
  if (!is.null(batch_plate_data())) {
    cat("  Clearing existing data...\n")
  }

  batch_plate_data(NULL)
  batch_metadata(NULL)
  bead_array_header_list(list())
  bead_array_plate_list(list())
  layout_template_sheets(list())
  xponent_assay_response_long(NULL)
  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL
  ))
  description_status(list(
    has_content = FALSE,
    has_sufficient_elements = FALSE,
    min_elements_found = 0,
    required_elements = 3,
    checked = FALSE,
    message = ""
  ))

  # PROCESS xPONENT FILES
  tryCatch({
    processed <- process_xponent_files(input$upload_xponent_experiment_files)

    # Store results in the SAME reactives used by Raw File workflow
    bead_array_header_list(processed$header_list)
    bead_array_plate_list(processed$plate_list)
    batch_plate_data(processed$combined_plates)

    # Store xPONENT-specific assay_response_long (preserves bead counts)
    xponent_assay_response_long(processed$assay_response_long)

    # CHECK DESCRIPTION FIELD
    cat("\n--- CHECKING DESCRIPTION FIELD (xPONENT) ---\n")

    delimiter <- input$xponent_description_delimiter %||% "_"
    desc_status <- check_and_report_description(
      processed$combined_plates,
      delimiter,
      required_elements = 3
    )
    description_status(desc_status)

    # Show warnings
    if (!desc_status$has_content) {
      showNotification(
        "Description field is blank. Default values will be used.",
        type = "warning", duration = 10
      )
    } else if (!desc_status$has_sufficient_elements) {
      showNotification(
        sprintf("Description has insufficient elements (%d found, %d required).",
                desc_status$min_elements_found, desc_status$required_elements),
        type = "warning", duration = 10
      )
    }

    # LOG ANTIGENS
    metadata_cols <- c("source_file", "Well", "Type", "Description",
                       "% Agg Beads", "Sampling Errors", "Acquisition Time", "plateid")
    antigen_cols <- setdiff(names(processed$combined_plates), metadata_cols)
    cat("\nAntigens detected (", length(antigen_cols), "):\n", sep = "")
    for (ag in antigen_cols) cat("  ", ag, "\n")

    cat("\n  xPONENT upload complete!\n")
    cat("======================================================\n\n")

    showNotification(
      paste("Successfully processed", nrow(input$upload_xponent_experiment_files), "xPONENT file(s)"),
      type = "message", duration = 3
    )

  }, error = function(e) {
    cat("  xPONENT upload failed:", conditionMessage(e), "\n")
    showNotification(
      paste("xPONENT upload failed:", conditionMessage(e)),
      type = "error", duration = 10
    )
  })
})


# ---- Observer: xPONENT description delimiter change ----
observeEvent(input$xponent_description_delimiter, {
  req(batch_plate_data())
  req(input$xPonentFile == "xPONENT")

  all_plates <- batch_plate_data()
  delimiter <- input$xponent_description_delimiter

  desc_check <- check_description_elements(
    plate_data = all_plates,
    delimiter = delimiter,
    required_elements = 3
  )

  description_status(list(
    has_content = desc_check$has_content,
    has_sufficient_elements = desc_check$has_sufficient_elements,
    min_elements_found = desc_check$min_elements_found,
    required_elements = desc_check$required_elements,
    checked = TRUE,
    message = desc_check$message
  ))
}, ignoreInit = TRUE)


# ---- Download handler: xPONENT layout template generation ----
output$xponent_blank_layout_file <- downloadHandler(
  filename = function() {
    paste0(input$readxMap_study_accession, "_",
           input$readxMap_experiment_accession_import,
           "_xponent_layout_template.xlsx")
  },
  content = function(file) {
    req(input$upload_xponent_experiment_files)
    req(bead_array_header_list())

    cat("\n======================================================\n")
    cat("         GENERATING xPONENT LAYOUT TEMPLATE\n")
    cat("======================================================\n")

    all_plates <- batch_plate_data()

    if (is.null(all_plates)) {
      cat("  ERROR: batch_plate_data() is NULL!\n")
      cat("   Falling back to processing uploaded xPONENT files...\n")
      all_plates <- process_xponent_files(input$upload_xponent_experiment_files)$combined_plates
    }

    cat("Using plate data:\n")
    cat("  -> Rows:", nrow(all_plates), "\n")
    cat("  -> Columns:", ncol(all_plates), "\n")
    cat("  -> Source files:", paste(unique(all_plates$source_file), collapse = ", "), "\n")

    desc_status <- description_status()

    # Reuse the SAME generate_layout_template() function
    generate_layout_template(
      all_plates = all_plates,
      study_accession = input$readxMap_study_accession,
      experiment_accession = input$readxMap_experiment_accession_import,
      n_wells = input$xponent_n_wells_on_plate,
      header_list = bead_array_header_list(),
      output_file = file,
      description_status = desc_status,
      delimiter = if (desc_status$has_content) input$xponent_description_delimiter else "_",
      element_order = if (desc_status$has_sufficient_elements) input$xponent_XElementOrder else c("PatientID", "TimePeriod", "DilutionFactor"),
      bcs_element_order = if (desc_status$has_content) input$xponent_BCSElementOrder else c("Source", "DilutionFactor"),
      assay_response_long_override = xponent_assay_response_long(),
      feature_value = input$xponent_feature_value
    )

    if (!desc_status$has_content || !desc_status$has_sufficient_elements) {
      showNotification(
        "Layout template generated with default values. Please review and update before uploading.",
        type = "warning", duration = 10
      )
    }

    cat("  xPONENT layout template generated!\n")
    cat("======================================================\n\n")
  }
)


# ---- Observer: xPONENT layout file upload ----
# Reuses the SAME import_layout_file() and validation as the Raw File pathway
observeEvent(input$upload_xponent_layout_file, {
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  project_id <- userWorkSpaceID()
  study_accession <- input$readxMap_study_accession
  experiment_accession <- input$readxMap_experiment_accession_import
  workspace_id <- userWorkSpaceID()
  current_user <- currentuser()

  cat("\n======================================================\n")
  cat("     PROCESSING xPONENT LAYOUT FILE UPLOAD\n")
  cat("======================================================\n")
  cat("Study:", study_accession, "\n")
  cat("Experiment:", experiment_accession, "\n")

  # Reset validation state
  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL,
    data_stored = FALSE
  ))

  input_upload_layout_file <- input$upload_xponent_layout_file
  inLayoutFile(input_upload_layout_file)

  if (is.null(input_upload_layout_file)) {
    cat("  No file uploaded\n")
    return(NULL)
  }

  cat("Layout file:", input_upload_layout_file$name, "\n\n")

  tryCatch({

    # ================================================================
    # READ LAYOUT FILE
    # ================================================================
    sheets <- readxl::excel_sheets(input_upload_layout_file$datapath)
    cat("Sheets found:", paste(sheets, collapse = ", "), "\n")

    validation <- check_sheet_names(input_upload_layout_file$datapath, exact_match = FALSE)
    if (!validation$valid) {
      showNotification(validation$message, type = "error", duration = NULL)
      return(NULL)
    }

    all_sheets <- import_layout_file(input_upload_layout_file$datapath)

    if (!all_sheets$success) {
      showNotification(paste(all_sheets$messages, collapse = "\n"), type = "error", duration = NULL)
      return(NULL)
    }

    cat("  ✓ Layout sheets read successfully\n")
    cat("  → Imported sheet names:", paste(names(all_sheets$data), collapse = ", "), "\n\n")


    # ================================================================
    # VALIDATE REQUIRED SHEETS
    # ================================================================
    required_sheets <- c("plates_map", "plate_id", "antigen_list")
    missing_required <- setdiff(required_sheets, names(all_sheets$data))

    if (length(missing_required) > 0) {
      cat("  ⚠  ERROR: Missing required sheets:", paste(missing_required, collapse = ", "), "\n")
      showNotification(
        paste("Missing required sheets:", paste(missing_required, collapse = ", ")),
        type = "error", duration = 10
      )
      return(NULL)
    }


    # ================================================================
    # EXTRACT DATA FROM LAYOUT FILE
    # ================================================================
    plates_map     <- all_sheets$data[["plates_map"]]
    plate_id_sheet <- all_sheets$data[["plate_id"]]
    antigen_list   <- all_sheets$data[["antigen_list"]]
    subject_map    <- all_sheets$data[["subject_groups"]]
    timepoint_map  <- all_sheets$data[["timepoint"]]

    has_assay_response_long <- "assay_response_long" %in% names(all_sheets$data)

    cat("\n=== LAYOUT FILE CONTENTS (xPONENT) ===\n")
    cat("  plate_id:\n")
    cat("    → Rows:", nrow(plate_id_sheet), "\n")
    cat("    → Columns:", paste(names(plate_id_sheet), collapse = ", "), "\n")
    cat("  plates_map:\n")
    cat("    → Rows:", nrow(plates_map), "\n")
    cat("    → Columns:", paste(names(plates_map), collapse = ", "), "\n")
    cat("  antigen_list:\n")
    cat("    → Rows:", nrow(antigen_list), "\n")
    cat("    → Columns:", paste(names(antigen_list), collapse = ", "), "\n")
    cat("  assay_response_long:", if (has_assay_response_long) "PRESENT ✓" else "MISSING ⚠", "\n")
    cat("============================\n\n")


    # ================================================================
    # VALIDATE REQUIRED COLUMNS
    # ================================================================
    cat("Validating required columns...\n")

    required_plate_id_cols <- c("study_name", "experiment_name", "plate_number",
                                "plateid", "plate_id", "number_of_wells")
    missing_plate_id_cols <- setdiff(required_plate_id_cols, names(plate_id_sheet))
    if (length(missing_plate_id_cols) > 0) {
      cat("  ⚠  plate_id missing columns:", paste(missing_plate_id_cols, collapse = ", "), "\n")
    } else {
      cat("  ✓ plate_id has all required columns\n")
    }

    required_plates_map_cols <- c("study_name", "experiment_name", "plate_number",
                                  "well", "specimen_type", "plateid")
    missing_plates_map_cols <- setdiff(required_plates_map_cols, names(plates_map))
    if (length(missing_plates_map_cols) > 0) {
      cat("  ⚠  plates_map missing columns:", paste(missing_plates_map_cols, collapse = ", "), "\n")
    } else {
      cat("  ✓ plates_map has all required columns\n")
    }

    required_antigen_cols <- c("antigen_label_on_plate", "antigen_abbreviation")
    missing_antigen_cols <- setdiff(required_antigen_cols, names(antigen_list))
    if (length(missing_antigen_cols) > 0) {
      cat("  ⚠  antigen_list missing columns:", paste(missing_antigen_cols, collapse = ", "), "\n")
    } else {
      cat("  ✓ antigen_list has all required columns\n")
    }


    # ================================================================
    # ADD PROJECT_ID IF NOT PRESENT
    # ================================================================
    if (!"project_id" %in% names(plates_map)) {
      plates_map$project_id <- project_id
      cat("  → Added project_id to plates_map\n")
    }

    if (!"project_id" %in% names(plate_id_sheet)) {
      plate_id_sheet$project_id <- project_id
      cat("  → Added project_id to plate_id\n")
    }


    # ================================================================
    # CHECK/COMPUTE NOMINAL_SAMPLE_DILUTION
    # ================================================================
    cat("\n  Checking nominal_sample_dilution...\n")

    if ("nominal_sample_dilution" %in% names(plates_map) &&
        "nominal_sample_dilution" %in% names(plate_id_sheet)) {
      cat("    ✓ nominal_sample_dilution already present in layout file\n")
    } else {
      cat("    → Computing nominal_sample_dilution from plates_map...\n")

      sample_rows <- plates_map[
        substr(plates_map$specimen_type, 1, 1) == "X" &
          !is.na(plates_map$specimen_dilution_factor),
      ]

      if (nrow(sample_rows) > 0) {
        nsd_df <- aggregate(
          specimen_dilution_factor ~ project_id + study_name + experiment_name + plate_number,
          data = sample_rows,
          FUN = function(x) paste(sort(unique(x)), collapse = "|")
        )
        names(nsd_df)[names(nsd_df) == "specimen_dilution_factor"] <- "nominal_sample_dilution"
      } else {
        unique_plates <- unique(plates_map[, c("project_id", "study_name", "experiment_name", "plate_number"), drop = FALSE])
        nsd_df <- unique_plates
        nsd_df$nominal_sample_dilution <- "1"
      }

      plates_map <- plates_map[, names(plates_map) != "nominal_sample_dilution", drop = FALSE]
      plate_id_sheet <- plate_id_sheet[, names(plate_id_sheet) != "nominal_sample_dilution", drop = FALSE]

      join_cols <- c("project_id", "study_name", "experiment_name", "plate_number")
      join_cols <- intersect(join_cols, names(plates_map))

      plates_map <- merge(plates_map, nsd_df, by = join_cols, all.x = TRUE)
      plates_map$nominal_sample_dilution[is.na(plates_map$nominal_sample_dilution)] <- "1"

      join_cols_plate_id <- intersect(join_cols, names(plate_id_sheet))
      plate_id_sheet <- merge(plate_id_sheet, nsd_df, by = join_cols_plate_id, all.x = TRUE)
      plate_id_sheet$nominal_sample_dilution[is.na(plate_id_sheet$nominal_sample_dilution)] <- "1"

      cat("    ✓ nominal_sample_dilution computed and added\n")
    }


    # ================================================================
    # CHECK/VALIDATE ASSAY_RESPONSE_LONG
    # ================================================================
    if (has_assay_response_long) {
      assay_response_long <- all_sheets$data[["assay_response_long"]]
      cat("  ✓ assay_response_long loaded from layout file (", nrow(assay_response_long), " rows)\n", sep = "")

      required_arl_cols <- c("plateid", "well", "antigen", "assay_response", "assay_bead_count")
      missing_arl_cols <- setdiff(required_arl_cols, names(assay_response_long))
      if (length(missing_arl_cols) > 0) {
        cat("    ⚠  assay_response_long missing columns:", paste(missing_arl_cols, collapse = ", "), "\n")
      }

    } else if (!is.null(xponent_assay_response_long())) {
      # FALLBACK: Use stored xPONENT assay_response_long reactive
      cat("\n  ⚠  assay_response_long sheet NOT in layout file\n")
      cat("      Using stored xPONENT assay_response_long reactive...\n")

      assay_response_long <- xponent_assay_response_long()
      assay_response_long$project_id      <- project_id
      assay_response_long$study_name      <- study_accession
      assay_response_long$experiment_name <- experiment_accession
      assay_response_long$feature         <- input$xponent_feature_value

      cat("      ✓ Created assay_response_long with", nrow(assay_response_long), "rows\n")

    } else {
      cat("\n  ⚠  assay_response_long NOT available from layout file or reactive\n")
      cat("      Please regenerate the layout template to include assay_response_long sheet\n")
      showNotification(
        "Layout file is missing assay_response_long sheet and no xPONENT data available. Please regenerate the layout template.",
        type = "error", duration = 10
      )
      assay_response_long <- NULL
    }


    # ================================================================
    # ANTIGEN ALIGNMENT CHECK
    # ================================================================
    cat("\n  Checking antigen alignment...\n")

    layout_antigens <- unique(antigen_list$antigen_label_on_plate)
    cat("    → Layout antigens:", length(layout_antigens), "\n")

    if (!is.null(assay_response_long) && "antigen_label_on_plate" %in% names(assay_response_long)) {
      response_antigens <- unique(assay_response_long$antigen_label_on_plate)

      extra_in_layout    <- setdiff(layout_antigens, response_antigens)
      missing_from_layout <- setdiff(response_antigens, layout_antigens)

      if (length(extra_in_layout) > 0) {
        cat("    ⚠  Layout has antigens NOT in response data:\n")
        for (ag in head(extra_in_layout, 5)) cat("        ✗", ag, "\n")
        if (length(extra_in_layout) > 5) cat("        ... and", length(extra_in_layout) - 5, "more\n")
      }

      if (length(missing_from_layout) > 0) {
        cat("    ⚠  Response data has antigens NOT in layout:\n")
        for (ag in head(missing_from_layout, 5)) cat("        ✗", ag, "\n")
        if (length(missing_from_layout) > 5) cat("        ... and", length(missing_from_layout) - 5, "more\n")
      }

      if (length(extra_in_layout) == 0 && length(missing_from_layout) == 0) {
        cat("    ✓ All antigens align\n")
      }
    }


    # ================================================================
    # STORE ENHANCED SHEETS
    # ================================================================
    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║  STORING LAYOUT SHEETS (xPONENT)                        ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")

    all_sheets$data[["plates_map"]] <- plates_map
    all_sheets$data[["plate_id"]]   <- plate_id_sheet

    if (!is.null(assay_response_long)) {
      all_sheets$data[["assay_response_long"]] <- assay_response_long
    }

    layout_template_sheets(all_sheets$data)

    cat("  ✓ layout_template_sheets() updated\n")
    cat("  → Contains", length(all_sheets$data), "sheets:", paste(names(all_sheets$data), collapse = ", "), "\n")


    # ================================================================
    # PREPARE BATCH_METADATA (from plate_id sheet)
    # ================================================================
    batch_metadata_data <- plate_id_sheet
    batch_metadata_data$workspace_id <- workspace_id
    batch_metadata_data$currentuser  <- current_user

    if ("plate_filename" %in% names(batch_metadata_data)) {
      names(batch_metadata_data)[names(batch_metadata_data) == "plate_filename"] <- "file_name"
    }

    cat("\n=== batch_metadata_data (xPONENT) ===\n")
    cat("  Columns:", paste(names(batch_metadata_data), collapse = ", "), "\n")
    cat("  Rows:", nrow(batch_metadata_data), "\n")


    # ================================================================
    # VALIDATION
    # ================================================================
    cat("\nValidating...\n")

    # Metadata validation
    validate_metadata_result <- validate_batch_plate_metadata(
      plate_metadata = batch_metadata_data,
      plate_id_data  = plate_id_sheet
    )

    # Bead array validation - use assay_response_long if available
    if (!is.null(assay_response_long)) {
      bead_array_validation <- validate_assay_response_data(
        assay_response_long = assay_response_long,
        antigen_import_list = antigen_list,
        plates_map          = plates_map
      )
    } else {
      bead_array_validation <- list(
        is_valid = FALSE,
        message  = "No assay response data available for validation"
      )
    }

    # Update validation state
    if (bead_array_validation$is_valid && validate_metadata_result$is_valid) {
      batch_validation_state(list(
        is_validated    = TRUE,
        is_uploaded     = FALSE,
        validation_time = Sys.time(),
        upload_time     = NULL,
        metadata_result = validate_metadata_result,
        bead_array_result = bead_array_validation
      ))
      # Use normalized metadata (dates have been parsed and standardized)
      batch_metadata(
        if (!is.null(validate_metadata_result$normalized_metadata))
          validate_metadata_result$normalized_metadata
        else batch_metadata_data
      )

      cat("✓ VALIDATION PASSED!\n")
      showNotification(
        "xPONENT layout validated successfully! Ready to upload to database.",
        type = "message", duration = 5
      )
    } else {
      batch_validation_state(list(
        is_validated    = FALSE,
        is_uploaded     = FALSE,
        validation_time = Sys.time(),
        upload_time     = NULL,
        metadata_result = validate_metadata_result,
        bead_array_result = bead_array_validation
      ))

      cat("✗ VALIDATION FAILED\n")
      cat("  Metadata valid:", validate_metadata_result$is_valid, "\n")
      cat("  Bead array valid:", bead_array_validation$is_valid, "\n")

      showNotification(
        "xPONENT layout validation failed. Please review error messages.",
        type = "error", duration = 10
      )
    }

    cat("╚══════════════════════════════════════════════════════════╝\n\n")

  }, error = function(e) {
    cat("  Layout import error:", conditionMessage(e), "\n")
    showNotification(
      paste("Error importing layout file:", conditionMessage(e)),
      type = "error", duration = 10
    )
  })
})


# ---- UI output: xPONENT element order drag-and-drop ----
output$xponent_order_input_ui <- renderUI({
  req(description_status()$has_sufficient_elements)

  all_elements <- c("PatientID", "TimePeriod", "DilutionFactor")
  optional <- input$xponent_optional_elements
  if (!is.null(optional)) {
    all_elements <- c("PatientID", optional, "TimePeriod", "DilutionFactor")
  }

  orderInput(
    inputId = "xponent_XElementOrder",
    label = "Description Label: Sample Elements (drag and drop items to change order)",
    items = all_elements,
    width = "100%",
    item_class = "primary"
  )
})

output$xponent_bcsorder_input_ui <- renderUI({
  req(description_status()$has_content)

  orderInput(
    inputId = "xponent_BCSElementOrder",
    label = "Description Label: Blank, Standard or Control Elements (drag and drop items to change order)",
    items = c("Source", "DilutionFactor"),
    width = "100%",
    item_class = "info"
  )
})

# ---- Observer: xPONENT optional elements change ----
observeEvent(input$xponent_optional_elements, {
  req(input$xPonentFile == "xPONENT")

  # Base elements (always included)
  base_elements <- c("PatientID", "DilutionFactor", "TimePeriod")

  # Reactive to get all active elements
  active_elements <- reactive({
    c(base_elements, input$xponent_optional_elements)
  })

  # Render order input dynamically
  output$xponent_order_input_ui <- renderUI({
    orderInput(
      inputId = "xponent_XElementOrder",
      label = "Description Label: Sample Elements (drag and drop items to change order)",
      items = active_elements(),
      width = "100%",
      item_class = "primary"
    )
  })

  output$xponent_bcsorder_input_ui <- renderUI({
    orderInput(
      inputId = "xponent_BCSElementOrder",
      label = "Description Label: Blank, Standard or Control Elements (drag and drop items to change order)",
      items = c("Source", "DilutionFactor"),
      width = "100%",
      item_class = "info"
    )
  })
}, ignoreInit = TRUE, ignoreNULL = TRUE)
