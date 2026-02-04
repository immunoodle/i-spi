
# SECTION 1: COLUMN MAPPING UTILITIES (unchanged from v1)

#' Create the standard column mapping
create_column_mapping <- function() {
  list(
    # Study/Experiment identifiers
    study_name = "study_accession",
    experiment_name = "experiment_accession",

    # Plate identifiers
    plate_number = "plate",
    number_of_wells = "n_wells",

    # Specimen data
    specimen_type = "stype",
    specimen_dilution_factor = "dilution",
    biosample_id_barcode = "sampleid",
    specimen_source = "source",

    # Time/Subject
    timepoint_tissue_abbreviation = "timeperiod",
    subject_id = "patientid",

    # Quality metrics
    `% Agg Beads` = "pctaggbeads",
    `%.Agg.Beads` = "pctaggbeads",
    `Sampling Errors` = "samplingerrors",
    `Sampling.Errors` = "samplingerrors",

    # Antigen
    antigen_abbreviation = "antigen",

    # Assay response
    assay_response = "antibody_mfi",
    assay_bead_count = "antibody_n",

    # Metadata
    currentuser = "auth0_user"
  )
}


#' Apply column mapping to a data frame
apply_column_mapping <- function(df, mapping = NULL) {
  if (is.null(mapping)) {
    mapping <- create_column_mapping()
  }

  for (old_name in names(mapping)) {
    if (old_name %in% names(df)) {
      new_name <- mapping[[old_name]]
      names(df)[names(df) == old_name] <- new_name
    }
  }
  df
}


# SECTION 2: PLATE IDENTIFIER UTILITIES (unchanged from v1)
#' Extract plate number from text (CANONICAL VERSION)
extract_plate_number <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)

  # Pattern 1: "plate" followed by separator and number
  match1 <- regmatches(text, regexpr("[Pp]late[_\\s\\.-]+(\\d+)", text, perl = TRUE))
  if (length(match1) > 0 && nchar(match1) > 0) {
    num <- gsub("[^0-9]", "", match1)
    if (nchar(num) > 0) return(paste0("plate_", num))
  }

  # Pattern 2: "plate" immediately followed by number
  match2 <- regmatches(text, regexpr("[Pp]late(\\d+)", text, perl = TRUE))
  if (length(match2) > 0 && nchar(match2) > 0) {
    num <- gsub("[^0-9]", "", match2)
    if (nchar(num) > 0) return(paste0("plate_", num))
  }

  return(NA_character_)
}


#' Compute all plate identifiers in a single pass
compute_plate_identifiers <- function(header_list) {

  used_plate_nums <- c()
  results <- list()

  for (i in seq_along(header_list)) {
    source_file <- names(header_list)[i]
    header_df <- header_list[[i]]

    # Extract plate number using priority order
    plate_number <- NA_character_
    extraction_source <- NA_character_

    # Priority 1: plateid from header
    if ("plateid" %in% names(header_df) &&
        !is.na(header_df$plateid[1]) &&
        header_df$plateid[1] != "") {
      plate_number <- extract_plate_number(header_df$plateid[1])
      if (!is.na(plate_number)) extraction_source <- "plateid"
    }

    # Priority 2: file_name from header
    if (is.na(plate_number) && "file_name" %in% names(header_df)) {
      plate_number <- extract_plate_number(header_df$file_name[1])
      if (!is.na(plate_number)) extraction_source <- "file_name"
    }

    # Priority 3: source_file
    if (is.na(plate_number)) {
      plate_number <- extract_plate_number(source_file)
      if (!is.na(plate_number)) extraction_source <- "source_file"
    }

    # Fallback: sequential
    if (is.na(plate_number)) {
      fallback_num <- 1
      while (paste0("plate_", fallback_num) %in% used_plate_nums) {
        fallback_num <- fallback_num + 1
      }
      plate_number <- paste0("plate_", fallback_num)
      extraction_source <- "fallback"
    }

    # Handle duplicates
    if (plate_number %in% used_plate_nums) {
      base_num <- as.numeric(gsub("plate_", "", plate_number))
      while (paste0("plate_", base_num) %in% used_plate_nums) {
        base_num <- base_num + 1
      }
      plate_number <- paste0("plate_", base_num)
    }

    used_plate_nums <- c(used_plate_nums, plate_number)

    # Get file_name for plate_id computation
    file_name_val <- if ("file_name" %in% names(header_df)) header_df$file_name[1] else source_file

    results[[i]] <- data.frame(
      source_file = source_file,
      plateid = clean_plate_id(source_file),
      plate_id = clean_plate_id(file_name_val),
      plate_number = plate_number,
      extraction_source = extraction_source,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}



# SECTION 3: NOMINAL SAMPLE DILUTION COMPUTATION
#' Compute nominal_sample_dilution for each plate
#'
#' Creates a pipe-delimited string of unique specimen_dilution_factor values
#' for sample wells (specimen_type = "X") on each plate.
#'
#' @param plates_map The plates_map data frame
#' @param plate_key_cols Columns that define a unique plate
#' @return Data frame with plate keys and nominal_sample_dilution
compute_nominal_sample_dilution <- function(plates_map,
                                             plate_key_cols = c("project_id", "study_name",
                                                                "experiment_name", "plate_id")) {

  cat("\n=== Computing nominal_sample_dilution ===\n")

  # Filter to sample rows only (specimen_type starts with "X")
  sample_rows <- plates_map[
    substr(plates_map$specimen_type, 1, 1) == "X" &
    !is.na(plates_map$specimen_dilution_factor),
  ]

  cat("  Sample rows found:", nrow(sample_rows), "\n")

  if (nrow(sample_rows) == 0) {
    # No samples - return plates with default dilution
    all_plates <- unique(plates_map[, plate_key_cols, drop = FALSE])
    all_plates$nominal_sample_dilution <- "1"
    return(all_plates)
  }

  # Aggregate dilution factors by plate
  # Using base R aggregate to avoid dplyr dependency issues
  agg_formula <- as.formula(
    paste("specimen_dilution_factor ~", paste(plate_key_cols, collapse = " + "))
  )

  result <- aggregate(
    agg_formula,
    data = sample_rows,
    FUN = function(x) paste(sort(unique(x)), collapse = "|")
  )

  names(result)[names(result) == "specimen_dilution_factor"] <- "nominal_sample_dilution"

  # Ensure all plates are represented (some might have only standards/blanks)
  all_plates <- unique(plates_map[, plate_key_cols, drop = FALSE])

  result <- merge(all_plates, result, by = plate_key_cols, all.x = TRUE)
  result$nominal_sample_dilution[is.na(result$nominal_sample_dilution)] <- "1"

  cat("  Plates processed:", nrow(result), "\n")
  cat("  Unique dilution patterns:", length(unique(result$nominal_sample_dilution)), "\n")
  cat("=====================================\n\n")

  return(result)
}


#' Join nominal_sample_dilution to plates_map and plate_id sheets
#'
#' @param plates_map The plates_map data frame
#' @param plate_id The plate_id data frame
#' @param plate_key_cols Columns that define a unique plate
#' @return List with enhanced plates_map and plate_id
add_nominal_sample_dilution_to_sheets <- function(plates_map, plate_id,
                                                   plate_key_cols = c("project_id", "study_name",
                                                                      "experiment_name", "plate_id")) {

  # Compute dilution once
  nsd_df <- compute_nominal_sample_dilution(plates_map, plate_key_cols)

  # Remove existing nominal_sample_dilution if present (to avoid duplicates)
  if ("nominal_sample_dilution" %in% names(plates_map)) {
    plates_map <- plates_map[, names(plates_map) != "nominal_sample_dilution"]
  }
  if ("nominal_sample_dilution" %in% names(plate_id)) {
    plate_id <- plate_id[, names(plate_id) != "nominal_sample_dilution"]
  }

  # Join to plates_map (propagates to all rows for each plate)
  plates_map_enhanced <- merge(
    plates_map,
    nsd_df,
    by = plate_key_cols,
    all.x = TRUE
  )

  # Join to plate_id (one row per plate)
  plate_id_enhanced <- merge(
    plate_id,
    nsd_df,
    by = plate_key_cols,
    all.x = TRUE
  )

  list(
    plates_map = plates_map_enhanced,
    plate_id = plate_id_enhanced
  )
}



# SECTION 4: ASSAY_RESPONSE_LONG SHEET CREATION
#' Create the assay_response_long sheet from raw plate data
#'
#' Pivots wide MFI data to long format with natural key columns.
#' This sheet becomes the single source of truth for assay response data
#' at upload time.
#'
#' @param combined_plates Combined raw plate data from all files
#' @param plate_identifiers Data frame from compute_plate_identifiers()
#' @param antigen_list The antigen_list sheet from layout
#' @param project_id Project identifier
#' @param study_name Study accession
#' @param experiment_name Experiment accession
#' @return Data frame in long format with natural key + assay response
create_assay_response_long <- function(combined_plates,
                                        plate_identifiers,
                                        antigen_list,
                                        project_id,
                                        study_name,
                                        experiment_name) {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         CREATING ASSAY_RESPONSE_LONG SHEET               ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Identify MFI columns (contain bead region number in parentheses)
  all_cols <- names(combined_plates)
  mfi_cols <- grep("\\([0-9]+\\)", all_cols, value = TRUE)

  cat("  → MFI columns found:", length(mfi_cols), "\n")

  if (length(mfi_cols) == 0) {
    cat("  ⚠ WARNING: No MFI columns detected!\n")
    return(NULL)
  }

  # Metadata columns to keep through pivot
  meta_cols <- c("source_file", "Well")
  available_meta <- intersect(meta_cols, all_cols)

  # Subset to relevant columns
  pivot_data <- combined_plates[, c(available_meta, mfi_cols), drop = FALSE]

  cat("  → Starting pivot from wide to long...\n")

  # Pivot to long format using tidyr
  assay_long <- tidyr::pivot_longer(
    pivot_data,
    cols = tidyselect::all_of(mfi_cols),
    names_to = "antigen_label_on_plate",
    values_to = "mfi_bead_combined"
  )

  cat("  → Rows after pivot:", nrow(assay_long), "\n")

  # Clean up and extract values
  assay_long <- assay_long %>%
    dplyr::mutate(
      # Clean antigen label (dots to spaces, as they appear in raw data)
      antigen_label_on_plate = gsub("\\.", " ", antigen_label_on_plate),

      # Extract MFI value (numeric before parentheses)
      assay_response = as.numeric(stringr::str_extract(mfi_bead_combined, "^[0-9.]+")),

      # Extract bead count (number inside parentheses)
      assay_bead_count = as.numeric(stringr::str_extract(mfi_bead_combined, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    dplyr::select(-mfi_bead_combined)

  # Rename Well to well for consistency
  if ("Well" %in% names(assay_long)) {
    names(assay_long)[names(assay_long) == "Well"] <- "well"
  }

  # Join plate identifiers
  cat("  → Joining plate identifiers...\n")
  assay_long <- merge(
    assay_long,
    plate_identifiers[, c("source_file", "plateid", "plate_id", "plate_number")],
    by = "source_file",
    all.x = TRUE
  )

  # Join antigen abbreviation from antigen_list
  cat("  → Joining antigen abbreviations...\n")

  # Get unique antigen lookup
  antigen_lookup <- unique(antigen_list[, c("antigen_label_on_plate", "antigen_abbreviation")])

  assay_long <- merge(
    assay_long,
    antigen_lookup,
    by = "antigen_label_on_plate",
    all.x = TRUE
  )

  # Rename antigen_abbreviation to antigen
  names(assay_long)[names(assay_long) == "antigen_abbreviation"] <- "antigen"

  # Add context columns
  assay_long$project_id <- project_id
  assay_long$study_name <- study_name
  assay_long$experiment_name <- experiment_name

  # Reorder columns - natural key first
  natural_key_cols <- c("project_id", "study_name", "experiment_name", "plateid", "well")
  antigen_cols <- c("antigen_label_on_plate", "antigen")
  response_cols <- c("assay_response", "assay_bead_count")
  other_cols <- c("source_file", "plate_id", "plate_number")

  col_order <- c(natural_key_cols, antigen_cols, response_cols, other_cols)
  available_cols <- intersect(col_order, names(assay_long))

  assay_long <- assay_long[, available_cols, drop = FALSE]

  # Report missing antigens
  missing_antigen <- sum(is.na(assay_long$antigen))
  if (missing_antigen > 0) {
    cat("  ⚠ WARNING:", missing_antigen, "rows have no antigen abbreviation match\n")
    unmatched <- unique(assay_long$antigen_label_on_plate[is.na(assay_long$antigen)])
    cat("    Unmatched labels:", paste(head(unmatched, 5), collapse = ", "),
        if (length(unmatched) > 5) "..." else "", "\n")
  }

  cat("  ✓ assay_response_long created with", nrow(assay_long), "rows\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")

  return(assay_long)
}



# SECTION 5: ENHANCED PLATE_ID SHEET
#' Enhance the plate_id sheet with file mapping columns
#'
#' Adds source_file, plateid, plate_id from plate_identifiers,
#' plus nominal_sample_dilution and project_id.
#'
#' @param plate_id_sheet Original plate_id sheet
#' @param plate_identifiers Data frame from compute_plate_identifiers()
#' @param nominal_sample_dilution_df Data frame with plate-level dilutions
#' @param project_id Project identifier
#' @return Enhanced plate_id data frame
enhance_plate_id_sheet <- function(plate_id_sheet,
                                    plate_identifiers,
                                    nominal_sample_dilution_df = NULL,
                                    project_id) {

  cat("\n=== Enhancing plate_id sheet ===\n")

  # Start with original sheet
  enhanced <- plate_id_sheet

  # Add project_id if not present
  if (!"project_id" %in% names(enhanced)) {
    enhanced$project_id <- project_id
  }

  # Join plate identifiers
  # Match on plate_filename (original file name) to source_file
  if ("plate_filename" %in% names(enhanced)) {
    # Create a lookup from plate_identifiers
    id_lookup <- plate_identifiers[, c("source_file", "plateid", "plate_id", "plate_number")]

    # First try matching plate_filename to source_file
    enhanced <- merge(
      enhanced,
      id_lookup,
      by.x = "plate_filename",
      by.y = "source_file",
      all.x = TRUE,
      suffixes = c("", "_from_id")
    )

    # If plate_id was already present, use it; otherwise use the joined one
    if ("plate_id_from_id" %in% names(enhanced)) {
      if ("plate_id" %in% names(enhanced)) {
        # Keep original, remove duplicate
        enhanced <- enhanced[, names(enhanced) != "plate_id_from_id"]
      } else {
        names(enhanced)[names(enhanced) == "plate_id_from_id"] <- "plate_id"
      }
    }

    # Add source_file column (same as plate_filename for mapping purposes)
    enhanced$source_file <- enhanced$plate_filename
  }

  # Join nominal_sample_dilution if provided
  if (!is.null(nominal_sample_dilution_df)) {
    join_cols <- intersect(
      names(nominal_sample_dilution_df),
      names(enhanced)
    )
    join_cols <- setdiff(join_cols, "nominal_sample_dilution")

    if (length(join_cols) > 0) {
      # Remove existing nominal_sample_dilution if present
      enhanced <- enhanced[, names(enhanced) != "nominal_sample_dilution", drop = FALSE]

      enhanced <- merge(
        enhanced,
        nominal_sample_dilution_df,
        by = join_cols,
        all.x = TRUE
      )
    }
  }

  cat("  → Columns in enhanced plate_id:", paste(names(enhanced), collapse = ", "), "\n")
  cat("  → Rows:", nrow(enhanced), "\n")
  cat("================================\n\n")

  return(enhanced)
}



# SECTION 6: UPLOAD PREPARATION USING ASSAY_RESPONSE_LONG
#' Prepare specimen data for upload using assay_response_long
#'
#' Joins plates_map (specimen metadata) with assay_response_long (MFI data)
#' using the natural key, then applies column mapping for database insert.
#'
#' @param plates_map The plates_map sheet (with nominal_sample_dilution)
#' @param assay_response_long The pre-pivoted MFI data
#' @param subject_map The subject_groups sheet (for samples only)
#' @param specimen_type One of "X", "S", "B", "C"
#' @param col_mapping Column mapping for database
#' @return Data frame ready for database insert
prepare_specimen_for_upload <- function(plates_map,
                                         assay_response_long,
                                         subject_map = NULL,
                                         specimen_type,
                                         col_mapping = NULL) {

  if (is.null(col_mapping)) {
    col_mapping <- create_column_mapping()
  }

  cat("\n  Preparing", specimen_type, "data for upload...\n")

  # Filter plates_map to this specimen type
  specimen_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == specimen_type, ]

  if (nrow(specimen_map) == 0) {
    cat("    → No", specimen_type, "specimens found\n")
    return(NULL)
  }

  cat("    → Specimen rows:", nrow(specimen_map), "\n")

  # For samples, join subject_groups first
  if (specimen_type == "X" && !is.null(subject_map)) {
    specimen_map <- merge(
      specimen_map,
      subject_map,
      by = c("study_name", "subject_id"),
      all.x = TRUE
    )
    # Create combined group
    specimen_map$agroup <- ifelse(
      is.na(specimen_map$groupb),
      specimen_map$groupa,
      paste(specimen_map$groupa, specimen_map$groupb, sep = "_")
    )
  }

  # Define natural key for joining to assay_response
  natural_key <- c("project_id", "study_name", "experiment_name", "plateid", "well")

  # Join assay response data
  specimen_data <- merge(
    specimen_map,
    assay_response_long,
    by = natural_key,
    all.x = TRUE,
    suffixes = c("", "_assay")
  )

  cat("    → After join with assay_response:", nrow(specimen_data), "\n")

  # Handle duplicate columns from merge
  # Prefer the plate_id from plates_map
  if ("plate_id_assay" %in% names(specimen_data)) {
    specimen_data <- specimen_data[, names(specimen_data) != "plate_id_assay"]
  }

  # Apply column mapping
  specimen_data <- apply_column_mapping(specimen_data, col_mapping)

  # Select appropriate columns based on specimen type
  if (specimen_type == "X") {
    final_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "timeperiod",
      "patientid", "well", "stype", "sampleid", "agroup", "dilution",
      "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi", "antibody_n",
      "feature", "nominal_sample_dilution", "project_id", "plateid", "plate"
    )
  } else if (specimen_type == "S") {
    final_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "source", "dilution", "pctaggbeads", "samplingerrors",
      "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
      "plateid", "nominal_sample_dilution", "plate"
    )
  } else if (specimen_type == "B") {
    final_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi",
      "antibody_n", "dilution", "feature", "project_id", "plateid",
      "nominal_sample_dilution", "plate"
    )
  } else if (specimen_type == "C") {
    final_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "sampleid", "source", "dilution", "pctaggbeads",
      "samplingerrors", "antigen", "antibody_mfi", "antibody_n", "feature",
      "project_id", "plateid", "nominal_sample_dilution", "plate"
    )
  }

  available_cols <- intersect(final_cols, names(specimen_data))
  specimen_data <- specimen_data[, available_cols, drop = FALSE]

  cat("    → Final columns:", length(available_cols), "\n")

  return(specimen_data)
}



# SECTION 7: LAYOUT TEMPLATE GENERATION ADDITIONS
#' Add assay_response_long sheet to workbook
#'
#' @param wb An openxlsx workbook object
#' @param assay_response_long Data frame from create_assay_response_long()
#' @return Modified workbook
add_assay_response_long_sheet <- function(wb, assay_response_long) {

  if (is.null(assay_response_long) || nrow(assay_response_long) == 0) {
    cat("  ⚠ No assay_response_long data to write\n")
    return(wb)
  }

  addWorksheet(wb, "assay_response_long")
  writeData(wb, "assay_response_long", assay_response_long, startRow = 1, startCol = 1)

  # Style header
  bold_style <- createStyle(textDecoration = "bold")
  addStyle(wb, "assay_response_long", style = bold_style, rows = 1, cols = 1:ncol(assay_response_long))

  # Set column widths
  setColWidths(wb, "assay_response_long", cols = 1:ncol(assay_response_long), widths = "auto")

  cat("  ✓ Added assay_response_long sheet with", nrow(assay_response_long), "rows\n")

  return(wb)
}


#' Add upload_config sheet to workbook
add_upload_config_sheet <- function(wb, config = list()) {

  default_config <- list(
    upload_version = "2.0",
    description_delimiter = "_",
    description_element_order = "PatientID,TimePeriod,DilutionFactor",
    bcs_element_order = "Source,DilutionFactor",
    default_dilution_factor = "1",
    default_subject_id = "1",
    default_timepoint = "T0",
    generated_timestamp = as.character(Sys.time())
  )

  final_config <- modifyList(default_config, config)

  config_df <- data.frame(
    field_name = names(final_config),
    field_value = unlist(final_config),
    description = c(
      "Schema version for compatibility checking",
      "Delimiter used in Description field parsing",
      "Order of elements in sample Description",
      "Order of elements for Blanks/Controls/Standards",
      "Default value when dilution factor cannot be parsed",
      "Default value when subject ID cannot be parsed",
      "Default value when timepoint cannot be parsed",
      "Timestamp when this template was generated"
    ),
    stringsAsFactors = FALSE
  )
  rownames(config_df) <- NULL

  addWorksheet(wb, "upload_config")
  writeData(wb, "upload_config", config_df, startRow = 1, startCol = 1)

  bold_style <- createStyle(textDecoration = "bold")
  addStyle(wb, "upload_config", style = bold_style, rows = 1, cols = 1:3)
  setColWidths(wb, "upload_config", cols = 1:3, widths = c(30, 50, 50))

  return(wb)
}



# SECTION 8: COMPLETE UPLOAD WORKFLOW
#' Prepare complete upload package from layout sheets
#'
#' This is the main entry point for preparing upload data.
#' Should be called after layout validation succeeds.
#'
#' @param layout_sheets List of layout template sheets
#' @param assay_response_long Pre-created assay response data
#' @param project_id Project identifier
#' @param workspace_id Workspace identifier
#' @param auth0_user Current user
#' @return List of data frames ready for database insert
prepare_upload_package <- function(layout_sheets,
                                    assay_response_long,
                                    project_id,
                                    workspace_id,
                                    auth0_user) {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         PREPARING UPLOAD PACKAGE                         ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Get layout components
  plate_id <- layout_sheets[["plate_id"]]
  plates_map <- layout_sheets[["plates_map"]]
  antigen_list <- layout_sheets[["antigen_list"]]
  subject_map <- layout_sheets[["subject_groups"]]
  timepoint_map <- layout_sheets[["timepoint"]]

  col_mapping <- create_column_mapping()

  # === HEADER ===
  cat("  → Preparing header...\n")
  header_data <- plate_id
  header_data$workspace_id <- workspace_id
  header_data$auth0_user <- auth0_user
  header_data$assay_response_variable <- "mfi"
  header_data$assay_independent_variable <- "concentration"
  header_data <- apply_column_mapping(header_data, col_mapping)

  # Rename for database
  if ("plate_filename" %in% names(header_data)) {
    names(header_data)[names(header_data) == "plate_filename"] <- "file_name"
  }

  # === SPECIMENS ===
  cat("  → Preparing specimens...\n")
  samples_data <- prepare_specimen_for_upload(
    plates_map, assay_response_long, subject_map, "X", col_mapping
  )

  standards_data <- prepare_specimen_for_upload(
    plates_map, assay_response_long, NULL, "S", col_mapping
  )

  blanks_data <- prepare_specimen_for_upload(
    plates_map, assay_response_long, NULL, "B", col_mapping
  )

  controls_data <- prepare_specimen_for_upload(
    plates_map, assay_response_long, NULL, "C", col_mapping
  )

  # === ANTIGENS ===
  cat("  → Preparing antigens...\n")
  antigens_data <- antigen_list[, c(
    "study_name", "experiment_name", "antigen_abbreviation", "antigen_family",
    "standard_curve_max_concentration", "antigen_name", "virus_bacterial_strain",
    "antigen_source", "catalog_number", "l_asy_min_constraint",
    "l_asy_max_constraint", "l_asy_constraint_method"
  ), drop = FALSE]
  names(antigens_data)[names(antigens_data) == "standard_curve_max_concentration"] <- "standard_curve_concentration"
  antigens_data <- apply_column_mapping(antigens_data, col_mapping)

  # === VISITS ===
  cat("  → Preparing planned visits...\n")
  visits_data <- timepoint_map
  names(visits_data)[names(visits_data) == "timepoint_tissue_abbreviation"] <- "timepoint_name"
  names(visits_data)[names(visits_data) == "tissue_type"] <- "type"
  names(visits_data)[names(visits_data) == "tissue_subtype"] <- "subtype"
  names(visits_data)[names(visits_data) == "description"] <- "end_rule"
  names(visits_data)[names(visits_data) == "min_time_since_day_0"] <- "min_start_day"
  names(visits_data)[names(visits_data) == "max_time_since_day_0"] <- "max_start_day"
  visits_data <- apply_column_mapping(visits_data, col_mapping)

  # === METADATA ===
  study_accession <- unique(plates_map$study_name)[1]
  experiment_accession <- unique(plates_map$experiment_name)[1]
  plate_ids <- unique(plate_id$plate_id)

  cat("  ✓ Upload package prepared\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")

  list(
    header = header_data,
    samples = samples_data,
    standards = standards_data,
    blanks = blanks_data,
    controls = controls_data,
    antigens = antigens_data,
    visits = visits_data,
    metadata = list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      project_id = project_id,
      plate_ids = plate_ids
    )
  )
}

