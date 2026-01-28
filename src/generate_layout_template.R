#' Generate Layout Template with Description Validation
#'
#' Creates an Excel layout template for batch plate uploads. Handles cases where
#' the Description field is blank or has insufficient elements by applying
#' default values and notifying the user.
#'
#' @param all_plates Combined plate data from all uploaded files
#' @param study_accession Study accession ID
#' @param experiment_accession Experiment accession ID
#' @param n_wells Number of wells on the plate (96, 384, etc.)
#' @param header_list List of header metadata from each plate file
#' @param output_file Path to save the Excel template
#' @param description_status List from check_description_elements() - if NULL, will be computed
#' @param delimiter Character used to separate Description elements (default: "_")
#' @param element_order Order of elements in Description for samples (default from input$XElementOrder)
#' @param bcs_element_order Order of elements for Blanks/Controls/Standards (default from input$BCSElementOrder)
#'
#' @return Writes Excel file to output_file path
#'
generate_layout_template <- function(all_plates,
                                     study_accession,
                                     experiment_accession,
                                     n_wells,
                                     header_list,
                                     output_file,
                                     project_id = userWorkSpaceID(),
                                     description_status = NULL,
                                     delimiter = NULL,
                                     element_order = NULL,
                                     bcs_element_order = NULL) {

  # all_plates_v <<- all_plates
  # header_list_v <<- header_list

  wb <- createWorkbook()
  bold_style <- createStyle(textDecoration = "bold")
  italic_style  <- createStyle(textDecoration = "italic")


  # Handle description_status - compute if not provided

  if (is.null(description_status)) {
    # Get delimiter from input or use default
    check_delimiter <- if (!is.null(delimiter)) delimiter else
      if (exists("input") && !is.null(input$description_delimiter)) input$description_delimiter else "_"

    description_status <- check_description_elements(
      plate_data = all_plates,
      delimiter = check_delimiter,
      required_elements = 3
    )

    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Description Status (computed)                           ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
    cat("  → Has content:", description_status$has_content, "\n")
    cat("  → Has sufficient elements:", description_status$has_sufficient_elements, "\n")
    cat("  → Min elements found:", description_status$min_elements_found, "\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")
  }


  # Get delimiter - from parameter, input, or default

  description_delimiter <- if (!is.null(delimiter)) {
    delimiter
  } else if (description_status$has_content && exists("input") && !is.null(input$description_delimiter)) {
    input$description_delimiter
  } else {
    "_"  # Default delimiter
  }


  # Get element order - from parameter, input, or defaults

  XElementOrder <- if (!is.null(element_order)) {
    element_order
  } else if (description_status$has_sufficient_elements && exists("input") && !is.null(input$XElementOrder)) {
    input$XElementOrder
  } else {
    # Default order when Description is blank/insufficient
    c("PatientID", "TimePeriod", "DilutionFactor")
  }

  BCSElementOrder <- if (!is.null(bcs_element_order)) {
    bcs_element_order
  } else if (description_status$has_content && exists("input") && !is.null(input$BCSElementOrder)) {
    input$BCSElementOrder
  } else {
    # Default order
    c("Source", "DilutionFactor")
  }

  cat("Using delimiter:", description_delimiter, "\n")
  cat("Using XElementOrder:", paste(XElementOrder, collapse = ", "), "\n")
  cat("Using BCSElementOrder:", paste(BCSElementOrder, collapse = ", "), "\n\n")

  # Combine header list
  plate_id <- do.call(rbind, header_list)

  # plate_id_df_v <<- plate_id
  # all_plates_v <<- all_plates

  # DIAGNOSTIC
  cat("\n=== GENERATE LAYOUT TEMPLATE DEBUG ===\n")
  cat("Header columns before processing:", paste(names(plate_id), collapse=", "), "\n")

  # Add basic fields
  plate_id$study_name <- study_accession
  plate_id$experiment_name <- experiment_accession
  plate_id$number_of_wells <- n_wells

  # plate_id_v <<- plate_id


  if ("source_file" %in% names(plate_id) && !all(is.na(plate_id$source_file))) {
    plate_id$plateid <- clean_plate_id(plate_id$source_file)
  }

  # Handle plate_filename first (needed for plate_number extraction)
  if ("file_name" %in% names(plate_id)) {
    names(plate_id)[names(plate_id) == "file_name"] <- "plate_filename"
    cat("  ✓ Renamed 'file_name' to 'plate_filename'\n")
  } else if ("source_file" %in% names(plate_id)) {
    plate_id$plate_filename <- plate_id$source_file
    cat("  ✓ Created plate_filename from source_file\n")
  } else {
    # Last resort: use the names from header_list
    plate_id$plate_filename <- names(header_list)
    cat("  ✓ Created plate_filename from header_list names\n")
  }

  # # HELPER FUNCTION: Extract plate number from text (filename, plateid, etc.)
  # # Looks for patterns like "plate_3", "plate 3", "plate.3", "plate3"
  # # This function is also defined in batch_layout_functions.R but is duplicated
  # # here to ensure availability in the upload_experiment_files observer.
  # extract_plate_number <- function(text) {
  #   if (is.na(text) || text == "") return(NA_character_)
  #
  #   # Try multiple patterns to extract plate number
  #   # Pattern 1: "plate" followed by separator and number (plate_3, plate 3, plate.3, plate-3)
  #   match1 <- regmatches(text, regexpr("[Pp]late[_\\s\\.-]+(\\d+)", text, perl = TRUE))
  #   if (length(match1) > 0 && nchar(match1) > 0) {
  #     num <- gsub("[^0-9]", "", match1)
  #     if (nchar(num) > 0) return(paste0("plate_", num))
  #   }
  #
  #   # Pattern 2: Just "plate" followed immediately by number (plate3, plate1IgGtot...)
  #   match2 <- regmatches(text, regexpr("[Pp]late(\\d+)", text, perl = TRUE))
  #   if (length(match2) > 0 && nchar(match2) > 0) {
  #     num <- gsub("[^0-9]", "", match2)
  #     if (nchar(num) > 0) return(paste0("plate_", num))
  #   }
  #
  #   return(NA_character_)
  # }

  # # Extract plate_number from filename or plateid
  # # Looks for patterns like "plate_3", "plate 3", "plate.3", "plate3"
  #
  # extract_plate_number <- function(text) {
  #   if (is.na(text) || text == "") return(NA_character_)
  #
  #   # Try multiple patterns to extract plate number
  #   # Pattern 1: "plate" followed by separator and number (plate_3, plate 3, plate.3, plate-3)
  #   match1 <- regmatches(text, regexpr("[Pp]late[_\\s\\.-]+(\\d+)", text, perl = TRUE))
  #   if (length(match1) > 0 && nchar(match1) > 0) {
  #     num <- gsub("[^0-9]", "", match1)
  #     if (nchar(num) > 0) return(paste0("plate_", num))
  #   }
  #
  #   # Pattern 2: Just "plate" followed immediately by number (plate3, plate1IgGtot...)
  #   match2 <- regmatches(text, regexpr("[Pp]late(\\d+)", text, perl = TRUE))
  #   if (length(match2) > 0 && nchar(match2) > 0) {
  #     num <- gsub("[^0-9]", "", match2)
  #     if (nchar(num) > 0) return(paste0("plate_", num))
  #   }
  #
  #   return(NA_character_)
  # }

  # Try to extract plate_number - prioritize plateid and source_file over existing 'plate' column
  # because 'plate' column may have been assigned consecutive numbers incorrectly
  plate_numbers_extracted <- rep(NA_character_, nrow(plate_id))

  cat("  → Extracting plate numbers...\n")

  # FIRST PRIORITY: plateid column (most likely to have correct plate identifiers)
  if ("plateid" %in% names(plate_id)) {
    for (i in 1:nrow(plate_id)) {
      extracted <- extract_plate_number(plate_id$plateid[i])
      if (!is.na(extracted)) {
        plate_numbers_extracted[i] <- extracted
        cat("      Row", i, ": plateid '", plate_id$plateid[i], "' → ", extracted, "\n")
      }
    }
    cat("  → Attempted extraction from 'plateid' column\n")
  }

  # SECOND PRIORITY: source_file column
  if ("source_file" %in% names(plate_id)) {
    for (i in 1:nrow(plate_id)) {
      if (is.na(plate_numbers_extracted[i])) {
        extracted <- extract_plate_number(plate_id$source_file[i])
        if (!is.na(extracted)) {
          plate_numbers_extracted[i] <- extracted
          cat("      Row", i, ": source_file '", plate_id$source_file[i], "' → ", extracted, "\n")
        }
      }
    }
    cat("  → Attempted extraction from 'source_file' column\n")
  }

  # THIRD PRIORITY: plate_filename column
  if ("plate_filename" %in% names(plate_id)) {
    for (i in 1:nrow(plate_id)) {
      if (is.na(plate_numbers_extracted[i])) {
        extracted <- extract_plate_number(plate_id$plate_filename[i])
        if (!is.na(extracted)) {
          plate_numbers_extracted[i] <- extracted
          cat("      Row", i, ": plate_filename '", plate_id$plate_filename[i], "' → ", extracted, "\n")
        }
      }
    }
    cat("  → Attempted extraction from 'plate_filename' column\n")
  }

  # FOURTH PRIORITY: existing 'plate' column (ONLY if still NA - this column may have wrong consecutive values)
  if ("plate" %in% names(plate_id)) {
    for (i in 1:nrow(plate_id)) {
      if (is.na(plate_numbers_extracted[i])) {
        extracted <- extract_plate_number(plate_id$plate[i])
        if (!is.na(extracted)) {
          plate_numbers_extracted[i] <- extracted
          cat("      Row", i, ": plate '", plate_id$plate[i], "' → ", extracted, "\n")
        }
      }
    }
    cat("  → Attempted extraction from 'plate' column (fallback)\n")
  }

  # Fallback: assign consecutive numbers only to those that couldn't be extracted
  na_count <- sum(is.na(plate_numbers_extracted))
  if (na_count > 0) {
    # Find the max number already extracted to continue from there
    existing_nums <- as.numeric(gsub("plate_", "", plate_numbers_extracted[!is.na(plate_numbers_extracted)]))
    if (length(existing_nums) > 0) {
      next_num <- max(existing_nums, na.rm = TRUE) + 1
    } else {
      next_num <- 1
    }

    for (i in 1:nrow(plate_id)) {
      if (is.na(plate_numbers_extracted[i])) {
        plate_numbers_extracted[i] <- paste0("plate_", next_num)
        next_num <- next_num + 1
      }
    }
    cat("  → Assigned fallback plate numbers to", na_count, "plates\n")
  }

  plate_id$plate_number <- plate_numbers_extracted

  cat("  ✓ Plate numbers extracted/assigned:\n")
  for (i in 1:nrow(plate_id)) {
    source_name <- if ("source_file" %in% names(plate_id)) plate_id$source_file[i] else
      if ("plate_filename" %in% names(plate_id)) plate_id$plate_filename[i] else
        paste0("Row ", i)
    cat("      ", source_name, " → ", plate_id$plate_number[i], "\n")
  }

  # # Handle plateid
  # if (!"plateid" %in% names(plate_id)) {
  #   # Try to extract from plate_id column if it exists
  #   if ("plate_id" %in% names(plate_id)) {
  #     plate_id$plateid <- plate_id$plate_id
  #     cat("  ✓ Created plateid from plate_id column\n")
  #   } else {
  #     # Create from plate_number
  #     plate_id$plateid <- plate_id$plate_number
  #     cat("  ✓ Created plateid from plate_number\n")
  #   }
  # }

  # Handle plateid and plate_id columns
  # NOTE: plate_id = full cleaned path, plateid = short identifier

  # First, create plate_id from the full file path using clean_plate_id
  if ("plate_filename" %in% names(plate_id)) {
    plate_id$plate_id <- clean_plate_id(plate_id$plate_filename)
    cat("  ✓ Created plate_id from plate_filename using clean_plate_id\n")
  } else if ("source_file" %in% names(plate_id)) {
    plate_id$plate_id <- clean_plate_id(plate_id$source_file)
    cat("  ✓ Created plate_id from source_file using clean_plate_id\n")
  }

  # Then, set plateid to the short identifier (plate_number or existing plateid)
  if (!"plateid" %in% names(plate_id) || is.na(plate_id$plateid[1]) || plate_id$plateid[1] == "") {
    plate_id$plateid <- plate_id$plate_number
    cat("  ✓ Created plateid from plate_number\n")
  }

  # # Apply clean_plate_id to plateid using the full file path
  # if ("plate_filename" %in% names(plate_id)) {
  #   plate_id$plateid <- clean_plate_id(plate_id$plate_filename)
  #   cat("  ✓ Applied clean_plate_id to plateid from plate_filename\n")
  # }
  cat("Columns after processing:", paste(names(plate_id), collapse=", "), "\n")

  # Now safely subset
  required_cols <- c("study_name", "experiment_name", "number_of_wells",
                     "plate_number", "plateid", "plate_id", "plate_filename")

  # Verify all required columns exist
  missing <- setdiff(required_cols, names(plate_id))
  if (length(missing) > 0) {
    stop("ERROR: Still missing columns after processing: ", paste(missing, collapse=", "))
  }

  plate_id <- plate_id[, required_cols, drop = FALSE]
  cat("Final plate_id dimensions:", nrow(plate_id), "rows x", ncol(plate_id), "cols\n")
  cat("=====================================\n\n")

  antigen_names <- names(all_plates)[!(names(all_plates) %in% c( "source_file", "Well", "Type", "Description", "% Agg Beads", "Sampling Errors", "Acquisition Time"))]

  antigen_df <- tibble(
    study_name = rep(study_accession, length(antigen_names)),
    experiment_name = rep(experiment_accession, length(antigen_names)),
    antigen_label_on_plate = antigen_names,
    # antigen_abbreviation = str_split_i(antigen_names," ",1),
    antigen_abbreviation = clean_antigen_label(antigen_names),
    standard_curve_max_concentration = 100000,
    l_asy_constraint_method = "default",
    l_asy_min_constraint = 0,
    l_asy_max_constraint = 0,
    antigen_family = NA_character_,
    antigen_name = NA_character_,
    virus_bacterial_strain = NA_character_,
    antigen_source = NA_character_,
    catalog_number = NA_character_
  )

  ## Plates Map
  plate_numbers <- unique(plate_id$plate_number)
  input_feature_value <- if (exists("input") && !is.null(input$feature_value)) input$feature_value else NA_character_

  # Get element positions from order vectors
  print(XElementOrder)
  xpatientid <- get_element_position("PatientID", XElementOrder)
  xtimeperiod <- get_element_position("TimePeriod", XElementOrder)
  xdilutionfactor <- get_element_position("DilutionFactor", XElementOrder)
  xgroupa <- get_element_position("SampleGroupA", XElementOrder)
  xgroupb <- get_element_position("SampleGroupB", XElementOrder)

  print(BCSElementOrder)
  bcssource <- get_element_position("Source", BCSElementOrder)
  bcsdilutionfactor <- get_element_position("DilutionFactor", BCSElementOrder)

  well_list <- generate_well_list(n_wells)

  # Create header_df safely
  cat("Creating header_df from header_list...\n")
  header_df_raw <- do.call(rbind, header_list)
  cat("  Available columns:", paste(names(header_df_raw), collapse=", "), "\n")

  # Build header_df with available columns
  header_df_cols <- c("source_file")

  if ("plateid" %in% names(header_df_raw)) {
    header_df_cols <- c(header_df_cols, "plateid")
  } else if ("plate_id" %in% names(header_df_raw)) {
    header_df_raw$plateid <- header_df_raw$plate_id
    header_df_cols <- c(header_df_cols, "plateid")
  } else {
    # Create plateid from extracted plate numbers
    header_df_raw$plateid <- sapply(header_df_raw$source_file, function(sf) {
      extracted <- extract_plate_number(sf)
      if (!is.na(extracted)) extracted else paste0("plate_", which(header_df_raw$source_file == sf)[1])
    })
    header_df_cols <- c(header_df_cols, "plateid")
  }

  # Use the same extraction logic for 'plate' column used in merge
  if ("plate" %in% names(header_df_raw)) {
    # Extract proper plate number from existing plate column
    header_df_raw$plate <- sapply(1:nrow(header_df_raw), function(i) {
      # First try to extract from existing plate value
      extracted <- extract_plate_number(header_df_raw$plate[i])
      if (!is.na(extracted)) return(extracted)
      # Fall back to source_file
      extracted <- extract_plate_number(header_df_raw$source_file[i])
      if (!is.na(extracted)) return(extracted)
      # Last resort
      return(paste0("plate_", i))
    })
    header_df_cols <- c(header_df_cols, "plate")
  } else if ("plate_number" %in% names(header_df_raw)) {
    header_df_raw$plate <- header_df_raw$plate_number
    header_df_cols <- c(header_df_cols, "plate")
  } else {
    # Extract plate number from source_file
    header_df_raw$plate <- sapply(1:nrow(header_df_raw), function(i) {
      extracted <- extract_plate_number(header_df_raw$source_file[i])
      if (!is.na(extracted)) return(extracted)
      return(paste0("plate_", i))
    })
    header_df_cols <- c(header_df_cols, "plate")
    header_df_cols <- c(header_df_cols, "plate")
  }

  header_df <- header_df_raw[, header_df_cols, drop = FALSE]
  cat("  Final header_df columns:", paste(names(header_df), collapse=", "), "\n\n")

  # Now continue with merge
  in_plates <- merge(all_plates, header_df, by = "source_file")
  in_plates$study_name <- study_accession
  in_plates$experiment_name <- experiment_accession

  plate_well_map <- expand_grid(
    study_name = study_accession,
    plate_number = plate_numbers,
    well = well_list)
  plate_well_map$experiment_name <- experiment_accession

  plate_well_map <- dplyr::left_join(plate_well_map, in_plates, by = c("study_name" = "study_name", "experiment_name" = "experiment_name", "plate_number" = "plate", "well" = "Well"))
  plate_well_map$specimen_type <- case_when(
    is.na(plate_well_map$Type) ~ "",
    substr(plate_well_map$Type,1,1) == "X" ~ "X",
    substr(plate_well_map$Type,1,1) == "C" ~ "C",
    substr(plate_well_map$Type,1,1) == "B" ~ "B",
    substr(plate_well_map$Type,1,1) == "S" ~ "S",
    TRUE ~ ""
  )


  # MODIFIED: Use parse_description_with_defaults for sample rows when Description
  # is blank or insufficient

  if (!description_status$has_content || !description_status$has_sufficient_elements) {
    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Using parse_description_with_defaults for samples       ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")

    # Parse each sample row using the function with defaults
    plate_well_map$subject_id <- sapply(1:nrow(plate_well_map), function(i) {
      if (is.na(plate_well_map$Type[i]) || substr(plate_well_map$Type[i], 1, 1) != "X") {
        # Non-sample rows
        type_char <- substr(plate_well_map$Type[i], 1, 1)
        if (is.na(type_char)) return("")
        if (type_char == "C") return(substr(plate_well_map$Type[i], 2, nchar(plate_well_map$Type[i])))
        if (type_char == "B") return("1")
        if (type_char == "S") return(substr(plate_well_map$Type[i], 2, nchar(plate_well_map$Type[i])))
        return("0")
      }
      # Sample rows - use parse_description_with_defaults
      parsed <- parse_description_with_defaults(
        description = plate_well_map$Description[i],
        delimiter = description_delimiter,
        element_order = XElementOrder,
        optional_elements = c("SampleGroupA", "SampleGroupB")
      )
      return(parsed$subject_id)
    })

    plate_well_map$specimen_dilution_factor <- sapply(1:nrow(plate_well_map), function(i) {
      type_char <- substr(plate_well_map$Type[i], 1, 1)
      if (is.na(type_char)) return(1)

      if (type_char == "X") {
        parsed <- parse_description_with_defaults(
          description = plate_well_map$Description[i],
          delimiter = description_delimiter,
          element_order = XElementOrder,
          optional_elements = c("SampleGroupA", "SampleGroupB")
        )
        return(parsed$dilution_factor)
      } else if (type_char %in% c("C", "B", "S")) {
        # Try to parse BCS description
        val <- str_split_i(plate_well_map$Description[i], description_delimiter, bcsdilutionfactor)
        num_val <- suppressWarnings(as.numeric(val))
        return(if (!is.na(num_val)) num_val else 1)
      }
      return(1)
    })

    plate_well_map$timepoint_tissue_abbreviation <- sapply(1:nrow(plate_well_map), function(i) {
      type_char <- substr(plate_well_map$Type[i], 1, 1)
      if (is.na(type_char) || type_char != "X") return("")

      parsed <- parse_description_with_defaults(
        description = plate_well_map$Description[i],
        delimiter = description_delimiter,
        element_order = XElementOrder,
        optional_elements = c("SampleGroupA", "SampleGroupB")
      )
      return(parsed$timeperiod)
    })

    cat("  ✓ Parsed sample descriptions with defaults\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")

  } else {

    # ORIGINAL: Use str_split_i when Description has content

    plate_well_map$subject_id <- case_when(
      is.na(plate_well_map$Type) ~ "",
      substr(plate_well_map$Type,1,1) == "X" ~ str_split_i(plate_well_map$Description,description_delimiter,xpatientid),
      substr(plate_well_map$Type,1,1) == "C" ~ substr(plate_well_map$Type,2,nchar(plate_well_map$Type)),
      substr(plate_well_map$Type,1,1) == "B" ~ "1",
      substr(plate_well_map$Type,1,1) == "S" ~ substr(plate_well_map$Type,2,nchar(plate_well_map$Type)),
      TRUE ~ "0"
    )

    plate_well_map$specimen_dilution_factor <- case_when(
      is.na(plate_well_map$Type) ~ 1,
      substr(plate_well_map$Type,1,1) == "X" ~ as.numeric(str_split_i(plate_well_map$Description,description_delimiter,xdilutionfactor)),
      substr(plate_well_map$Type,1,1) == "C" ~ as.numeric(str_split_i(plate_well_map$Description,description_delimiter,bcsdilutionfactor)),
      substr(plate_well_map$Type,1,1) == "B" ~ as.numeric(str_split_i(plate_well_map$Description,description_delimiter,bcsdilutionfactor)),
      substr(plate_well_map$Type,1,1) == "S" ~ as.numeric(str_split_i(plate_well_map$Description,description_delimiter,bcsdilutionfactor)),
      TRUE ~ 1
    )

    plate_well_map$timepoint_tissue_abbreviation <- case_when(
      is.na(plate_well_map$Type) ~ "",
      substr(plate_well_map$Type,1,1) == "X" ~ str_split_i(plate_well_map$Description,description_delimiter,xtimeperiod),
      substr(plate_well_map$Type,1,1) == "C" ~ "",
      substr(plate_well_map$Type,1,1) == "B" ~ "",
      substr(plate_well_map$Type,1,1) == "S" ~ "",
      TRUE ~ ""
    )
  }


  # Build subject_groups - also using parse_description_with_defaults if needed


  # First, ensure subject_id has no NA values for sample rows before creating subject_groups
  # Replace NA/empty subject_id values with default "1" for sample rows
  sample_mask_pre <- plate_well_map$specimen_type == "X"
  na_subject_pre <- sample_mask_pre & (is.na(plate_well_map$subject_id) | plate_well_map$subject_id == "")
  if (any(na_subject_pre, na.rm = TRUE)) {
    plate_well_map$subject_id[na_subject_pre] <- "1"
    cat("  → Pre-filled", sum(na_subject_pre, na.rm = TRUE), "NA/empty subject_id values with '1'\n")
  }

  # Filter to sample rows and convert to plain data.frame to avoid tibble row name issues
  sample_rows_mask <- plate_well_map$specimen_type == "X"

  # Handle case where there are no sample rows
  if (!any(sample_rows_mask, na.rm = TRUE)) {
    cat("  → No sample rows found, creating default subject_groups\n")
    subject_groups <- data.frame(
      study_name = study_accession,
      subject_id = "1",
      groupa = "Unknown",
      groupb = "Unknown",
      stringsAsFactors = FALSE
    )
  } else {
    # Create subject_id_dat as a fresh data.frame (not a tibble subset)
    subject_id_dat <- data.frame(
      study_name = plate_well_map$study_name[sample_rows_mask],
      subject_id = plate_well_map$subject_id[sample_rows_mask],
      Description = plate_well_map$Description[sample_rows_mask],
      stringsAsFactors = FALSE
    )

    # Reset row names to avoid "row names contain missing values" error
    rownames(subject_id_dat) <- NULL

    cat("  → Created subject_id_dat with", nrow(subject_id_dat), "sample rows\n")

    if (!description_status$has_content || !description_status$has_sufficient_elements) {
      # Parse groups using the function with defaults
      subject_id_dat$groupa <- sapply(subject_id_dat$Description, function(desc) {
        parsed <- parse_description_with_defaults(
          description = desc,
          delimiter = description_delimiter,
          element_order = XElementOrder,
          optional_elements = c("SampleGroupA", "SampleGroupB")
        )
        return(parsed$groupa)
      })

      subject_id_dat$groupb <- sapply(subject_id_dat$Description, function(desc) {
        parsed <- parse_description_with_defaults(
          description = desc,
          delimiter = description_delimiter,
          element_order = XElementOrder,
          optional_elements = c("SampleGroupA", "SampleGroupB")
        )
        return(parsed$groupb)
      })
    } else {
      # Original logic
      if (!is.na(xgroupa)) {
        subject_id_dat$groupa <- str_split_i(subject_id_dat$Description,description_delimiter,xgroupa)
      } else {
        subject_id_dat$groupa <- ""
      }
      if (!is.na(xgroupb)) {
        subject_id_dat$groupb <- str_split_i(subject_id_dat$Description,description_delimiter,xgroupb)
      } else {
        subject_id_dat$groupb <- ""
      }
    }

    # Ensure no NA values in the columns used for distinct
    subject_id_dat$subject_id[is.na(subject_id_dat$subject_id) | subject_id_dat$subject_id == ""] <- "1"
    subject_id_dat$groupa[is.na(subject_id_dat$groupa) | subject_id_dat$groupa == ""] <- "Unknown"
    subject_id_dat$groupb[is.na(subject_id_dat$groupb) | subject_id_dat$groupb == ""] <- "Unknown"

    # Create subject_groups using unique() on a plain data.frame to avoid dplyr issues
    subject_groups_cols <- subject_id_dat[, c("study_name", "subject_id", "groupa", "groupb"), drop = FALSE]
    rownames(subject_groups_cols) <- NULL
    subject_groups <- unique(subject_groups_cols)
    rownames(subject_groups) <- NULL

    cat("  → Created subject_groups with", nrow(subject_groups), "unique subjects\n")
  }

  plate_well_map$specimen_source <- case_when(
    is.na(plate_well_map$Type) ~ "",
    substr(plate_well_map$Type,1,1) == "X" ~ "sample",
    substr(plate_well_map$Type,1,1) == "C" ~ str_split_i(plate_well_map$Description,description_delimiter,bcssource),
    substr(plate_well_map$Type,1,1) == "B" ~ str_split_i(plate_well_map$Description,description_delimiter,bcssource),
    substr(plate_well_map$Type,1,1) == "S" ~ str_split_i(plate_well_map$Description,description_delimiter,bcssource),
    TRUE ~ ""
  )


  plate_well_map$biosample_id_barcode <- case_when(
    is.na(plate_well_map$Type) ~ "",
    substr(plate_well_map$Type,1,1) == "X" ~ substr(plate_well_map$Type,2,nchar(plate_well_map$Type)),
    substr(plate_well_map$Type,1,1) == "C" ~ substr(plate_well_map$Type,2,nchar(plate_well_map$Type)),
    substr(plate_well_map$Type,1,1) == "B" ~ "",
    substr(plate_well_map$Type,1,1) == "S" ~ substr(plate_well_map$Type,2,nchar(plate_well_map$Type)),
    TRUE ~ ""
  )

  plate_well_map$feature <- input_feature_value

  plate_well_map <- plate_well_map[ , c("study_name",	"plate_number",	"well",	"specimen_type",	"specimen_source",
                                        "specimen_dilution_factor",	"experiment_name",	"feature",	"subject_id",	"biosample_id_barcode",	"timepoint_tissue_abbreviation")]


  # NEW: Apply default values to plates_map if Description was insufficient

  # Apply defaults directly here since the column names may differ
  if (!description_status$has_content || !description_status$has_sufficient_elements) {
    sample_mask <- plate_well_map$specimen_type == "X"

    cat("╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Applying default values due to insufficient Description ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")

    # Set default subject_id to "1"
    if ("subject_id" %in% names(plate_well_map)) {
      na_subject <- sample_mask & (is.na(plate_well_map$subject_id) | plate_well_map$subject_id == "")
      if (any(na_subject, na.rm = TRUE)) {
        plate_well_map$subject_id[na_subject] <- "1"
        cat("  → Set", sum(na_subject, na.rm = TRUE), "blank subject_id values to '1'\n")
      }
    }

    # Set default specimen_dilution_factor to 1
    if ("specimen_dilution_factor" %in% names(plate_well_map)) {
      na_dilution <- sample_mask & (is.na(plate_well_map$specimen_dilution_factor) |
                                      plate_well_map$specimen_dilution_factor == "" |
                                      is.nan(plate_well_map$specimen_dilution_factor))
      if (any(na_dilution, na.rm = TRUE)) {
        plate_well_map$specimen_dilution_factor[na_dilution] <- 1
        cat("  → Set", sum(na_dilution, na.rm = TRUE), "blank specimen_dilution_factor values to 1\n")
      }
    }

    # Set default timepoint_tissue_abbreviation to "T0"
    if ("timepoint_tissue_abbreviation" %in% names(plate_well_map)) {
      na_timepoint <- sample_mask & (is.na(plate_well_map$timepoint_tissue_abbreviation) | plate_well_map$timepoint_tissue_abbreviation == "")
      if (any(na_timepoint, na.rm = TRUE)) {
        plate_well_map$timepoint_tissue_abbreviation[na_timepoint] <- "T0"
        cat("  → Set", sum(na_timepoint, na.rm = TRUE), "blank timepoint_tissue_abbreviation values to 'T0'\n")
      }
    }

    cat("╚══════════════════════════════════════════════════════════╝\n")
  }

  timepoints <- unique(plate_well_map[plate_well_map$specimen_type=="X", ]$timepoint_tissue_abbreviation)
  # Remove NA and empty timepoints
  timepoints <- timepoints[!is.na(timepoints) & timepoints != ""]

  # Ensure at least one timepoint exists
  if (length(timepoints) == 0) {
    timepoints <- "T0"
    cat("  → No timepoints found, using default 'T0'\n")
  }

  timepoint <- expand_grid(
    study_name = study_accession,
    timepoint_tissue_abbreviation	= timepoints)
  timepoint$tissue_type	= "blood"
  timepoint$tissue_subtype	= "serum"
  timepoint$description = NA_character_
  timepoint$min_time_since_day_0= NA_character_
  timepoint$max_time_since_day_0	= NA_character_
  timepoint$timepoint_unit= NA_character_


  cell_valid_table <- tibble(
    tibble(
      l_asy_constraint_method = c(
        "default",
        "user_defined",
        "range_of_blanks",
        "geometric_mean_of_blanks"
      )
    )
  )


  # plate_id_before_wb <<- plate_id
  # plate_well_map_before_wb <<- plate_well_map

 # string with all unique sample dilutions separated by a pipe
  plate_id$nominal_sample_dilution <-paste(
    sort(unique(
      plate_well_map[
        plate_well_map$specimen_type == "X",
      ]$specimen_dilution_factor
    )),
    collapse = "|"
  )

 # add project_id
 plate_id$project_id <- project_id

  workbook <- list(
    plate_id = plate_id,
    subject_groups = subject_groups,
    timepoint = timepoint,
    antigen_list = antigen_df,
    plates_map = plate_well_map,
    cell_valid = cell_valid_table
  )

  # Loop over list elements and write each to a sheet
  for (nm in names(workbook)) {
    addWorksheet(wb, nm)
    writeData(wb, nm, "Example:", startRow = 1, startCol = 1)
    if (nm == "plate_id") {
      # Example row (Row 2)
      example_row <- c(
        "Vietnam342",
        "IgG_total",
        96,
        "plate_1",
        "plaque_1_IgG_15092025",
        "C:/Users/d78039e/GeiselMed/vaccineVietnam342/IgG_total/plaque_1_IgG_15092025.xlsx"
      )
      writeData(wb, nm, t(example_row),
                startRow = 2, startCol = 1, colNames = FALSE)

      # Real table: headers at row 3, data starts row 4
      writeData(wb, nm, workbook[[nm]], startRow = 3, startCol = 1)

      # Bold  header row
      addStyle(wb, nm, style = bold_style,
               rows = c(3), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
      # Italicize the examples
      addStyle(wb, nm, style = italic_style,
               rows = c(1:2), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)

    } else if (nm == "subject_groups") {
      # Example row (Row 2)
      example_group_row <- c(
        "Vietnam342",
        "P58732",
        "TdaP",
        "wP"
      )
      writeData(wb, nm, t(example_group_row),
                startRow = 2, startCol = 1, colNames = FALSE)
      # Real table: headers at row 3, data starts row 4
      writeData(wb, nm, workbook[[nm]], startRow = 3, startCol = 1)

      # Bold  header row
      addStyle(wb, nm, style = bold_style,
               rows = c(3), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
      # Italicize the examples
      addStyle(wb, nm, style = italic_style,
               rows = c(1:2), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)

    } else if (nm == "timepoint") {
      ex_row2 <- c("Vietnam342", "T0", "blood", "serum", "initial_visit", 1, 1, "day")
      ex_row3 <- c("Vietnam342", "T1", "blood", "serum", "maternal_serum_at_birth", 3, 6, "month")
      ex_row4 <- c("Vietnam342", "C1", "blood", "cord", "cord blood", 3, 6, "month")
      writeData(wb, nm, t(ex_row2),
                startRow = 2, startCol = 1, colNames = FALSE)
      writeData(wb, nm, t(ex_row3),
                startRow = 3, startCol = 1, colNames = FALSE)
      writeData(wb, nm, t(ex_row4),
                startRow = 4, startCol = 1, colNames = FALSE)

      # Real table: headers at row 4, data starts row 5
      writeData(wb, nm, workbook[[nm]], startRow = 5, startCol = 1)

      # Bold  header row
      addStyle(wb, nm, style = bold_style,
               rows = c(5), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
      # Italicize the examples
      addStyle(wb, nm, style = italic_style,
               rows = c(1:4), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)

    } else if (nm == "antigen_list") {
      writeData(wb, nm, "user defined constraint", startRow = 1, startCol = 7)

      ex_antigen_list <- c("Vietnam342","IgG_total", "FHA (27)","FHA", 100000, "unconstrained", 1, 10, "Pertussis", "Filamentous hemagglutinin adhesin ", "Bordetella pertussis strain Tohama I",
                           "SinoBiological",	"FHA123456789")
      writeData(wb, nm, t(ex_antigen_list),
                startRow = 2, startCol = 1, colNames = FALSE)
      # Real table: headers at row 3, data starts row 4
      writeData(wb, nm, workbook[[nm]], startRow = 3, startCol = 1)

      # Bold  header row
      addStyle(wb, nm, style = bold_style,
               rows = c(3), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
      # Italicize the examples
      addStyle(wb, nm, style = italic_style,
               rows = c(1:2), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
    } else if (nm ==  "plates_map") {
      ex_row_plate_map <- c("Vietnam342", "plate_1", "A1", "X", "sample", 2000, "IgG_total", "IgG", "P58732", "B563429","T0")
      writeData(wb, nm, t(ex_row_plate_map),
                startRow = 2, startCol = 1, colNames = FALSE)
      # Real table: headers at row 3, data starts row 4
      writeData(wb, nm, workbook[[nm]], startRow = 3, startCol = 1)

      # Bold  header row
      addStyle(wb, nm, style = bold_style,
               rows = c(3), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
      # Italicize the examples
      addStyle(wb, nm, style = italic_style,
               rows = c(1:2), cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)

    } else if (nm == "cell_valid") {
      writeData(wb, nm, workbook[[nm]], startRow = 1, startCol = 1)
      addStyle(wb, nm, style = bold_style, rows = 1, cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
    } else {
      writeData(wb, nm, workbook[[nm]])
      addStyle(wb, nm, style = bold_style, rows = 1, cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
    }
  }


  # VALIDATION: Collect all issues for README_IMPORTANT

  validation_issues <- list()
  validation_warnings <- list()

  # Track if we need to show the README

  needs_readme <- FALSE

  # --- Check 1: Description content/sufficiency ---
  if (!description_status$has_content) {
    needs_readme <- TRUE
    validation_issues <- c(validation_issues, list(
      "DESCRIPTION FIELD BLANK:",
      "  All Description fields in your plate data are blank.",
      "  Default values have been applied to ALL sample wells."
    ))
  } else if (!description_status$has_sufficient_elements) {
    needs_readme <- TRUE
    validation_issues <- c(validation_issues, list(
      "DESCRIPTION FIELD INSUFFICIENT:",
      paste0("  Description field has only ", description_status$min_elements_found,
             " elements (expected ", description_status$required_elements, ")."),
      "  Some fields may have incorrect or default values."
    ))
  }

  # --- Check 2: Specimen dilution factor validation for samples ---
  sample_rows <- plate_well_map[plate_well_map$specimen_type == "X", ]
  if (nrow(sample_rows) > 0) {
    # Check for NA, empty, or non-numeric dilution factors
    dilution_values <- sample_rows$specimen_dilution_factor

    # Count issues
    na_dilutions <- sum(is.na(dilution_values), na.rm = TRUE)

    # Check for non-numeric (if stored as character)
    if (is.character(dilution_values)) {
      non_numeric_dilutions <- sum(!grepl("^[0-9]*\\.?[0-9]+$", dilution_values) &
                                     !is.na(dilution_values) &
                                     dilution_values != "", na.rm = TRUE)
    } else {
      non_numeric_dilutions <- 0
    }

    # Check for default value of 1 (which may indicate missing data)
    default_dilutions <- sum(dilution_values == 1, na.rm = TRUE)
    total_samples <- nrow(sample_rows)

    if (na_dilutions > 0) {
      needs_readme <- TRUE
      validation_warnings <- c(validation_warnings, list(
        "SPECIMEN DILUTION FACTOR - MISSING VALUES:",
        paste0("  ", na_dilutions, " of ", total_samples, " sample wells have NA/missing dilution factors."),
        "  These have been set to default value of 1.",
        "  Please verify and update the 'specimen_dilution_factor' column in PLATES_MAP."
      ))
    }

    if (non_numeric_dilutions > 0) {
      needs_readme <- TRUE
      validation_warnings <- c(validation_warnings, list(
        "SPECIMEN DILUTION FACTOR - NON-NUMERIC VALUES:",
        paste0("  ", non_numeric_dilutions, " sample wells had non-numeric dilution factors."),
        "  These have been set to default value of 1.",
        "  Please update with correct numeric values in PLATES_MAP."
      ))
    }

    # Warn if ALL samples have the default dilution (likely not filled in)
    if (default_dilutions == total_samples && total_samples > 0 &&
        (!description_status$has_content || !description_status$has_sufficient_elements)) {
      validation_warnings <- c(validation_warnings, list(
        "SPECIMEN DILUTION FACTOR - ALL DEFAULTS:",
        paste0("  All ", total_samples, " sample wells have dilution factor = 1 (default)."),
        "  This may indicate missing data. Please verify dilution values are correct."
      ))
    }
  }

  # --- Check 3: Timepoint linkage validation ---
  # Get timepoints from plates_map (sample rows only)
  plates_map_timepoints <- unique(plate_well_map[plate_well_map$specimen_type == "X",
                                                 "timepoint_tissue_abbreviation"])
  plates_map_timepoints <- plates_map_timepoints[!is.na(plates_map_timepoints) &
                                                   plates_map_timepoints != ""]

  # Get timepoints from timepoint sheet
  timepoint_sheet_values <- unique(timepoint$timepoint_tissue_abbreviation)
  timepoint_sheet_values <- timepoint_sheet_values[!is.na(timepoint_sheet_values) &
                                                     timepoint_sheet_values != ""]

  # Find mismatches
  in_plates_not_timepoint <- setdiff(plates_map_timepoints, timepoint_sheet_values)
  in_timepoint_not_plates <- setdiff(timepoint_sheet_values, plates_map_timepoints)

  if (length(in_plates_not_timepoint) > 0) {
    needs_readme <- TRUE
    validation_issues <- c(validation_issues, list(
      "TIMEPOINT MISMATCH - MISSING FROM TIMEPOINT SHEET:",
      paste0("  The following timepoints are in PLATES_MAP but not in TIMEPOINT sheet:"),
      paste0("    • ", paste(in_plates_not_timepoint, collapse = ", ")),
      "  Please add these timepoints to the TIMEPOINT sheet or correct PLATES_MAP values."
    ))
  }

  if (length(in_timepoint_not_plates) > 0) {
    needs_readme <- TRUE
    validation_warnings <- c(validation_warnings, list(
      "TIMEPOINT MISMATCH - UNUSED IN PLATES_MAP:",
      paste0("  The following timepoints are in TIMEPOINT sheet but not used in PLATES_MAP:"),
      paste0("    • ", paste(in_timepoint_not_plates, collapse = ", ")),
      "  These may be unused or there may be data entry errors."
    ))
  }

  # --- Check 4: Subject ID validation ---
  if (nrow(sample_rows) > 0) {
    subject_ids <- sample_rows$subject_id
    default_subject_count <- sum(subject_ids == "1", na.rm = TRUE)
    na_subject_count <- sum(is.na(subject_ids) | subject_ids == "", na.rm = TRUE)

    if (na_subject_count > 0) {
      needs_readme <- TRUE
      validation_warnings <- c(validation_warnings, list(
        "SUBJECT ID - MISSING VALUES:",
        paste0("  ", na_subject_count, " sample wells have missing subject IDs."),
        "  Please update the 'subject_id' column in PLATES_MAP."
      ))
    }

    if (default_subject_count == nrow(sample_rows) && nrow(sample_rows) > 1 &&
        (!description_status$has_content || !description_status$has_sufficient_elements)) {
      validation_warnings <- c(validation_warnings, list(
        "SUBJECT ID - ALL DEFAULTS:",
        paste0("  All ", nrow(sample_rows), " sample wells have subject_id = '1' (default)."),
        "  This likely indicates missing data. Please update with actual subject identifiers."
      ))
    }
  }

  # --- Check 5: Group validation ---
  if (nrow(subject_groups) > 0) {
    unknown_groupa <- sum(subject_groups$groupa == "Unknown", na.rm = TRUE)
    unknown_groupb <- sum(subject_groups$groupb == "Unknown", na.rm = TRUE)

    if (unknown_groupa > 0 || unknown_groupb > 0) {
      if (!description_status$has_content || !description_status$has_sufficient_elements) {
        validation_warnings <- c(validation_warnings, list(
          "GROUP ASSIGNMENTS - DEFAULTS APPLIED:",
          paste0("  ", unknown_groupa, " subjects have groupa = 'Unknown'"),
          paste0("  ", unknown_groupb, " subjects have groupb = 'Unknown'"),
          "  Please update group assignments in SUBJECT_GROUPS sheet if needed."
        ))
      }
    }
  }


  # BUILD README_IMPORTANT CONTENT

  if (needs_readme || length(validation_issues) > 0 || length(validation_warnings) > 0) {

    readme_lines <- c(
      "╔══════════════════════════════════════════════════════════════════════════════╗",
      "║  IMPORTANT: LAYOUT TEMPLATE VALIDATION REPORT                               ║",
      "╚══════════════════════════════════════════════════════════════════════════════╝",
      "",
      paste0("Generated: ", Sys.time()),
      paste0("Study: ", study_accession),
      paste0("Experiment: ", experiment_accession),
      ""
    )

    # Add critical issues section
    if (length(validation_issues) > 0) {
      readme_lines <- c(readme_lines,
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "⚠️  CRITICAL ISSUES REQUIRING ATTENTION:",
                        "═══════════════════════════════════════════════════════════════════════════════",
                        ""
      )
      for (issue in validation_issues) {
        readme_lines <- c(readme_lines, issue)
      }
      readme_lines <- c(readme_lines, "")
    }

    # Add warnings section
    if (length(validation_warnings) > 0) {
      readme_lines <- c(readme_lines,
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "⚡ WARNINGS:",
                        "═══════════════════════════════════════════════════════════════════════════════",
                        ""
      )
      for (warning in validation_warnings) {
        readme_lines <- c(readme_lines, warning)
      }
      readme_lines <- c(readme_lines, "")
    }

    # Add default values section if applicable
    if (!description_status$has_content || !description_status$has_sufficient_elements) {
      readme_lines <- c(readme_lines,
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "DEFAULT VALUES APPLIED:",
                        "═══════════════════════════════════════════════════════════════════════════════",
                        "",
                        "The following default values were used where data was missing:",
                        "  • Subject ID: '1'",
                        "  • Sample Dilution Factor: 1",
                        "  • Timepoint: 'T0'",
                        "  • Groups (groupa, groupb): 'Unknown'",
                        ""
      )
    }

    # Add action items
    readme_lines <- c(readme_lines,
                      "═══════════════════════════════════════════════════════════════════════════════",
                      "REQUIRED ACTIONS BEFORE UPLOADING:",
                      "═══════════════════════════════════════════════════════════════════════════════",
                      "",
                      "1. PLATES_MAP sheet:",
                      "   - Verify/update 'subject_id' column with actual patient/subject identifiers",
                      "   - Verify/update 'specimen_dilution_factor' with correct numeric dilution values",
                      "   - Verify/update 'timepoint_tissue_abbreviation' with correct timepoint codes",
                      "",
                      "2. SUBJECT_GROUPS sheet:",
                      "   - Verify/update 'subject_id' column matches values in PLATES_MAP",
                      "   - Update 'groupa' and 'groupb' columns with correct group assignments",
                      "",
                      "3. TIMEPOINT sheet:",
                      "   - Ensure all timepoints used in PLATES_MAP are listed here",
                      "   - Fill in tissue_type, tissue_subtype, description, and time ranges",
                      "",
                      "4. VALIDATION CHECK:",
                      "   - Timepoints in PLATES_MAP must match entries in TIMEPOINT sheet",
                      "   - Subject IDs in PLATES_MAP should match entries in SUBJECT_GROUPS",
                      "   - All specimen_dilution_factor values must be numeric",
                      "",
                      "═══════════════════════════════════════════════════════════════════════════════",
                      "After updating, save this file and upload it as the layout template.",
                      "═══════════════════════════════════════════════════════════════════════════════"
    )

    readme_data <- data.frame(Notes = readme_lines, stringsAsFactors = FALSE)

    addWorksheet(wb, "README_IMPORTANT")
    writeData(wb, "README_IMPORTANT", readme_data, startRow = 1, startCol = 1, colNames = FALSE)

    # Style the README sheet
    warning_style <- createStyle(
      fontColour = "#8B0000",
      textDecoration = "bold"
    )
    addStyle(wb, "README_IMPORTANT", style = warning_style,
             rows = 1:3, cols = 1, gridExpand = TRUE)

    # Style section headers
    section_style <- createStyle(
      fontColour = "#000080",
      textDecoration = "bold"
    )

    # Find and style section header rows (those with ═══)
    section_rows <- which(grepl("^═══", readme_lines))
    if (length(section_rows) > 0) {
      addStyle(wb, "README_IMPORTANT", style = section_style,
               rows = section_rows, cols = 1, gridExpand = TRUE)
    }

    setColWidths(wb, "README_IMPORTANT", cols = 1, widths = 85)

    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Added README_IMPORTANT sheet with validation report     ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
    cat("  Issues found:", length(validation_issues), "\n")
    cat("  Warnings found:", length(validation_warnings), "\n")
  }

  # Save the workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)

  cat("\n✓ Layout template saved to:", output_file, "\n")
}

