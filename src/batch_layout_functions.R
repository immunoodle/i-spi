

reset_batch_reactives <- function() {
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         RESETTING ALL BATCH REACTIVES                    ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Log what's being cleared
  if (!is.null(batch_plate_data())) {
    cat("  ✓ Clearing batch_plate_data:", nrow(batch_plate_data()), "rows\n")
    cat("    Source files:", unique(batch_plate_data()$source_file), "\n")
  }

  if (!is.null(batch_metadata())) {
    cat("  ✓ Clearing batch_metadata:", nrow(batch_metadata()), "rows\n")
  }

  if (length(bead_array_header_list()) > 0) {
    cat("  ✓ Clearing bead_array_header_list:", length(bead_array_header_list()), "items\n")
    cat("    Files:", names(bead_array_header_list()), "\n")
  }

  if (length(bead_array_plate_list()) > 0) {
    cat("  ✓ Clearing bead_array_plate_list:", length(bead_array_plate_list()), "items\n")
  }

  if (length(layout_template_sheets()) > 0) {
    cat("  ✓ Clearing layout_template_sheets:", length(layout_template_sheets()), "sheets\n")
  }

  # Reset batch data reactives
  batch_plate_data(NULL)
  batch_metadata(NULL)

  # Reset layout template
  layout_template_sheets(list())

  # Reset bead array data
  bead_array_header_list(list())
  bead_array_plate_list(list())

  # Reset validation state
  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL
  ))

  cat("  ✓ All reactives reset to NULL/empty\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
}

# Output for Description content status (for conditional panels)
output$descriptionHasContent <- reactive({
  status <- description_status()
  return(isTRUE(status$has_content))
})
outputOptions(output, "descriptionHasContent", suspendWhenHidden = FALSE)

# Output for Description sufficient elements status (for conditional panels)
output$descriptionHasSufficientElements <- reactive({
  status <- description_status()
  return(isTRUE(status$has_sufficient_elements))
})
outputOptions(output, "descriptionHasSufficientElements", suspendWhenHidden = FALSE)

# Clear any leftover temp directories from previous sessions
onSessionStart <- function() {
  tmpdir <- file.path(tempdir(), "uploaded_batch")
  if (dir.exists(tmpdir)) {
    unlink(tmpdir, recursive = TRUE)
    cat("Cleared temp directory from previous session\n")
  }
}

#' Check Description field content and element count
#'
#' This function examines the Description column in plate data to determine
#' if it has content and whether it contains sufficient elements for parsing
#' into subject ID, timeperiod, dilution factor, and optional group fields.
#'
#' @param plate_data Data frame containing plate data with a Description column
#' @param delimiter Character used to separate elements in Description (default: "_")
#' @param required_elements Minimum number of elements required (default: 3 for PatientID, TimePeriod, DilutionFactor)
#'
#' @return List with:
#'   - has_content: TRUE if at least one sample has a non-empty Description
#'   - has_sufficient_elements: TRUE if all non-empty Descriptions have enough elements
#'   - min_elements_found: The minimum number of elements found in any Description
#'   - required_elements: The number of elements required
#'   - message: Warning message if applicable, NULL otherwise
#'
#' @examples
#' # Check a plate with proper descriptions
#' check_description_elements(plate_data, delimiter = "_", required_elements = 3)
#'
check_description_elements <- function(plate_data, delimiter = "_", required_elements = 3) {
  # Filter to only sample rows (Type starts with X)
  sample_rows <- plate_data[grepl("^X", plate_data$Type), ]

  if (nrow(sample_rows) == 0) {
    return(list(
      has_content = TRUE,  # No samples, so not applicable
      has_sufficient_elements = TRUE,
      min_elements_found = NA,
      required_elements = required_elements,
      message = NULL
    ))
  }

  # Check Description column
  descriptions <- sample_rows$Description

  # Check if Description column exists
  if (!"Description" %in% names(plate_data)) {
    return(list(
      has_content = FALSE,
      has_sufficient_elements = FALSE,
      min_elements_found = 0,
      required_elements = required_elements,
      message = "Description column is missing from the plate data."
    ))
  }

  # Check for blank/empty descriptions
  empty_descriptions <- is.na(descriptions) | trimws(descriptions) == "" | descriptions == "NA"

  if (all(empty_descriptions)) {
    return(list(
      has_content = FALSE,
      has_sufficient_elements = FALSE,
      min_elements_found = 0,
      required_elements = required_elements,
      message = "All Description fields are blank. The layout template will need to be manually updated with subject IDs, groups, timepoints, and dilution factors after creation."
    ))
  }

  # Count elements in non-empty descriptions
  non_empty_descriptions <- descriptions[!empty_descriptions]

  # Escape delimiter for regex if needed
  delimiter_pattern <- switch(delimiter,
                              "|" = "\\|",
                              "." = "\\.",
                              delimiter
  )

  element_counts <- sapply(non_empty_descriptions, function(desc) {
    # Split by delimiter and count non-empty elements
    elements <- strsplit(as.character(desc), delimiter_pattern)[[1]]
    elements <- elements[trimws(elements) != ""]
    length(elements)
  })

  min_elements <- min(element_counts, na.rm = TRUE)

  # Check if sufficient elements exist
  has_sufficient <- min_elements >= required_elements

  message <- NULL
  if (!has_sufficient) {
    message <- sprintf(
      "Description field has insufficient elements (found %d, need %d). Some fields will use default values. The layout template may need manual updates for subject IDs, groups, timepoints, and/or dilution factors.",
      min_elements,
      required_elements
    )
  }

  return(list(
    has_content = TRUE,
    has_sufficient_elements = has_sufficient,
    min_elements_found = min_elements,
    required_elements = required_elements,
    message = message
  ))
}


#' Apply default values to plates_map when Description is insufficient
#'
#' This function fills in default values for plates_map columns when the
#' Description field is blank or has insufficient elements to parse.
#'
#' @param plates_map Data frame representing the plates map
#' @param description_status List from check_description_elements function
#'
#' @return Modified plates_map with default values applied where needed
#'
#' @details
#' Default values applied:
#'   - subject_id: "1"
#'   - specimen_dilution_factor: 1
#'   - timeperiod_tissue_abbreviation: "T0"
#'   - groupa: "Unknown"
#'   - groupb: "Unknown"
#'
apply_default_values_to_plates_map <- function(plates_map, description_status) {
  if (is.null(plates_map) || nrow(plates_map) == 0) {
    return(plates_map)
  }

  # Only apply defaults for sample rows (specimen_type == "X")
  sample_mask <- plates_map$specimen_type == "X"

  if (!description_status$has_content || !description_status$has_sufficient_elements) {
    cat("╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Applying default values due to insufficient Description ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")

    # Set default subject_id to "1"
    if ("subject_id" %in% names(plates_map)) {
      na_subject <- sample_mask & (is.na(plates_map$subject_id) | plates_map$subject_id == "")
      if (any(na_subject)) {
        plates_map$subject_id[na_subject] <- "1"
        cat("  → Set", sum(na_subject), "blank subject_id values to '1'\n")
      }
    }

    # Set default specimen_dilution_factor to 1
    if ("specimen_dilution_factor" %in% names(plates_map)) {
      na_dilution <- sample_mask & (is.na(plates_map$specimen_dilution_factor) | plates_map$specimen_dilution_factor == "")
      if (any(na_dilution)) {
        plates_map$specimen_dilution_factor[na_dilution] <- 1
        cat("  → Set", sum(na_dilution), "blank specimen_dilution_factor values to 1\n")
      }
    }

    # Set default timeperiod_tissue_abbreviation to "T0"
    if ("timeperiod_tissue_abbreviation" %in% names(plates_map)) {
      na_timeperiod <- sample_mask & (is.na(plates_map$timeperiod_tissue_abbreviation) | plates_map$timeperiod_tissue_abbreviation == "")
      if (any(na_timeperiod)) {
        plates_map$timeperiod_tissue_abbreviation[na_timeperiod] <- "T0"
        cat("  → Set", sum(na_timeperiod), "blank timeperiod_tissue_abbreviation values to 'T0'\n")
      }
    }

    # Set default groupa if exists
    if ("groupa" %in% names(plates_map)) {
      na_groupa <- sample_mask & (is.na(plates_map$groupa) | plates_map$groupa == "")
      if (any(na_groupa)) {
        plates_map$groupa[na_groupa] <- "Unknown"
        cat("  → Set", sum(na_groupa), "blank groupa values to 'Unknown'\n")
      }
    }

    # Set default groupb if exists
    if ("groupb" %in% names(plates_map)) {
      na_groupb <- sample_mask & (is.na(plates_map$groupb) | plates_map$groupb == "")
      if (any(na_groupb)) {
        plates_map$groupb[na_groupb] <- "Unknown"
        cat("  → Set", sum(na_groupb), "blank groupb values to 'Unknown'\n")
      }
    }

    cat("╚══════════════════════════════════════════════════════════╝\n")
  }

  return(plates_map)
}


#' Parse Description field with fallback to defaults
#'
#' This function attempts to parse the Description field into components.
#' If parsing fails or the field is empty, it returns default values.
#'
#' @param description Character string containing the Description value
#' @param delimiter Character used to separate elements
#' @param element_order Character vector specifying the order of elements
#' @param optional_elements Character vector of optional elements to include
#'
#' @return Named list with parsed values or defaults:
#'   - subject_id: Subject/patient ID (default: "1")
#'   - groupa: First group assignment (default: "Unknown")
#'   - groupb: Second group assignment (default: "Unknown")
#'   - timeperiod: Timepoint/tissue (default: "T0")
#'   - dilution_factor: Specimen dilution (default: 1)
#'
parse_description_with_defaults <- function(description,
                                            delimiter = "_",
                                            element_order = c("PatientID", "SampleGroupA", "SampleGroupB", "TimePeriod", "DilutionFactor"),
                                            optional_elements = c("SampleGroupA", "SampleGroupB")) {

  # Default values
  defaults <- list(
    subject_id = "1",
    groupa = "Unknown",
    groupb = "Unknown",
    timeperiod = "T0",
    dilution_factor = 1
  )

  # Check if description is empty
  if (is.na(description) || trimws(description) == "" || description == "NA") {
    return(defaults)
  }

  # Escape delimiter for regex
  delimiter_pattern <- switch(delimiter,
                              "|" = "\\|",
                              "." = "\\.",
                              delimiter
  )

  # Split description
  elements <- strsplit(as.character(description), delimiter_pattern)[[1]]
  elements <- trimws(elements)
  elements <- elements[elements != ""]

  # If no elements, return defaults
  if (length(elements) == 0) {
    return(defaults)
  }

  # Build result list
  result <- defaults

  # Map elements to fields based on order
  # Filter element_order to only include elements we're expecting
  active_elements <- element_order[element_order %in% c("PatientID", "SampleGroupA", "SampleGroupB", "TimePeriod", "DilutionFactor")]

  # If optional elements are not in the list, remove them from active_elements
  active_elements <- active_elements[active_elements %in% c("PatientID", "TimePeriod", "DilutionFactor") |
                                       active_elements %in% optional_elements]

  for (i in seq_along(active_elements)) {
    if (i <= length(elements)) {
      field <- active_elements[i]
      value <- elements[i]

      if (field == "PatientID") {
        result$subject_id <- value
      } else if (field == "SampleGroupA") {
        result$groupa <- value
      } else if (field == "SampleGroupB") {
        result$groupb <- value
      } else if (field == "TimePeriod") {
        result$timeperiod <- value
      } else if (field == "DilutionFactor") {
        # Try to parse as numeric, fall back to default if not
        numeric_val <- suppressWarnings(as.numeric(value))
        if (!is.na(numeric_val)) {
          result$dilution_factor <- numeric_val
        }
      }
    }
  }

  return(result)
}


#' Validate Description elements for batch upload
#'
#' Validates that the Description field in the batch plate data has
#' sufficient information for creating the layout template.
#'
#' @param batch_plate_data Combined plate data from all uploaded files
#' @param delimiter Delimiter used in Description field
#' @param required_elements Minimum number of elements required
#'
#' @return List with validation result and messages
#'
validate_batch_description <- function(batch_plate_data, delimiter = "_", required_elements = 3) {

  result <- list(
    valid = TRUE,
    has_warnings = FALSE,
    messages = c()
  )

  # Get unique source files
  source_files <- unique(batch_plate_data$source_file)

  for (file in source_files) {
    file_data <- batch_plate_data[batch_plate_data$source_file == file, ]

    desc_check <- check_description_elements(
      plate_data = file_data,
      delimiter = delimiter,
      required_elements = required_elements
    )

    if (!desc_check$has_content) {
      result$has_warnings <- TRUE
      result$messages <- c(result$messages,
                           sprintf("File '%s': Description field is blank. Default values will be used.", file)
      )
    } else if (!desc_check$has_sufficient_elements) {
      result$has_warnings <- TRUE
      result$messages <- c(result$messages,
                           sprintf("File '%s': Description has only %d elements (need %d). Some defaults will be applied.",
                                   file, desc_check$min_elements_found, required_elements)
      )
    }
  }

  return(result)
}


# read in the folder where the files are and the common word
# Usually the common word is 'plate'
read_bind_xlsx <- function (folder, x){
  files <- list.files(folder, pattern=x, full.names=TRUE)
  files <- files[!grepl("layout", basename(files), ignore.case = TRUE)]
  names(files) <- basename(files)

  files %>%
    #set_names(~basename(.))%>%
    map_dfr(read_excel,sheet=1, skip=7, col_types="text",.id="source_file")
}

# FIXED: process_uploaded_files function
# This is the root cause - the temp directory was accumulating files from multiple batches

# REPLACE the existing process_uploaded_files function in batch_layout_functions.R
# (around line 15-23) with this:

process_uploaded_files <- function(upload_df) {
  # Define temp directory for this batch
  tmpdir <- file.path(tempdir(), "uploaded_batch")

  # CRITICAL FIX: Clear the directory if it exists to remove old files
  if (dir.exists(tmpdir)) {
    cat("  → Clearing old temp directory:", tmpdir, "\n")
    old_files <- list.files(tmpdir, full.names = TRUE)
    if (length(old_files) > 0) {
      cat("  → Removing", length(old_files), "old files\n")
      file.remove(old_files)
    }
  } else {
    cat("  → Creating temp directory:", tmpdir, "\n")
    dir.create(tmpdir)
  }

  # Copy new files to temp directory
  cat("  → Copying", nrow(upload_df), "new files to temp directory\n")
  for (i in 1:nrow(upload_df)) {
    file.copy(upload_df$datapath[i],
              file.path(tmpdir, upload_df$name[i]),
              overwrite = TRUE)
  }

  # Verify what files are in the directory
  files_in_dir <- list.files(tmpdir, pattern = "plate", full.names = FALSE)
  cat("  → Files in temp directory:", paste(files_in_dir, collapse = ", "), "\n")

  # Read and combine all Excel files
  result <- read_bind_xlsx(folder = tmpdir, x = "plate")

  cat("  → Combined data dimensions:", nrow(result), "rows x", ncol(result), "cols\n")

  return(result)
}

# ALTERNATIVE: If you want to be extra safe, create a unique temp directory each time
# This ensures complete isolation between batches

process_uploaded_files_v2 <- function(upload_df) {
  # Create a unique temp directory for this batch using timestamp
  tmpdir <- file.path(tempdir(), paste0("uploaded_batch_", format(Sys.time(), "%Y%m%d_%H%M%S")))

  cat("  → Creating unique temp directory:", tmpdir, "\n")
  dir.create(tmpdir, showWarnings = FALSE)

  # Copy new files to temp directory
  cat("  → Copying", nrow(upload_df), "new files\n")
  for (i in 1:nrow(upload_df)) {
    file.copy(upload_df$datapath[i],
              file.path(tmpdir, upload_df$name[i]),
              overwrite = TRUE)
  }

  # Verify what files are in the directory
  files_in_dir <- list.files(tmpdir, pattern = "plate", full.names = FALSE)
  cat("  → Files in temp directory:", paste(files_in_dir, collapse = ", "), "\n")

  # Read and combine all Excel files
  result <- read_bind_xlsx(folder = tmpdir, x = "plate")

  cat("  → Combined data dimensions:", nrow(result), "rows x", ncol(result), "cols\n")

  # Clean up the temp directory after reading
  cat("  → Cleaning up temp directory\n")
  unlink(tmpdir, recursive = TRUE)

  return(result)
}

# RECOMMENDATION: Use the first version (process_uploaded_files) for now
# as it's a minimal change that fixes the issue. The v2 version is more robust
# but requires more testing.


generate_well_list <- function(plate_size) {
  # Matches logic from plate_plot()
  dims <- switch(
    as.character(plate_size),
    "6" = c(2, 3),
    "12" = c(3, 4),
    "24" = c(4, 6),
    "48" = c(6, 8),
    "96" = c(8, 12),
    "384" = c(16, 24),
    "1536" = c(32, 48)
  )

  n_rows <- dims[1]
  n_cols <- dims[2]

  # Row letters support (A–Z, AA–AF to match function
  MORELETTERS <- c(LETTERS, "AA", "AB", "AC", "AD", "AE", "AF")

  rows <- MORELETTERS[seq_len(n_rows)]
  cols <- seq_len(n_cols)

  # Create well labels e.g., A1, B1...
  expand.grid(Row = rows, Col = cols) |>
    dplyr::mutate(
      well = paste0(Row, Col)
    ) |>
    dplyr::pull(well)
}

add_source_column <- function(df_list) {
  # Check if input is a list
  if (!is.list(df_list)) {
    stop("Input must be a list of data frames")
  }

  # Check if list has names
  if (is.null(names(df_list))) {
    stop("List must have names")
  }

  # Get the names of list elements
  list_names <- names(df_list)

  # Add source_file column to each data frame
  result <- mapply(function(df, name) {
    if (!is.data.frame(df)) {
      warning(paste("Element", name, "is not a data frame. Skipping."))
      return(df)
    }
    df$source_file <- name
    return(df)
  }, df_list, list_names, SIMPLIFY = FALSE)

  return(result)
}

get_element_position <- function(element,
                                 order_vector,
                                 ignore_case = FALSE,
                                 default = NA_integer_) {

  # Validate inputs
  if (missing(order_vector)) {
    stop("'order_vector' is required")
  }

  if (!is.character(element)) {
    stop("'element' must be a character vector")
  }

  if (!is.character(order_vector)) {
    stop("'order_vector' must be a character vector")
  }

  if (length(order_vector) == 0) {
    stop("'order_vector' cannot be empty")
  }

  # Find positions
  if (ignore_case) {
    position <- match(tolower(element), tolower(order_vector))
  } else {
    position <- match(element, order_vector)
  }

  # Replace NA with default value
  position[is.na(position)] <- default

  # Name the results if multiple elements
  if (length(element) > 1) {
    names(position) <- element
  }

  return(position)
}

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
                                     description_status = NULL,
                                     delimiter = NULL,
                                     element_order = NULL,
                                     bcs_element_order = NULL) {

  wb <- createWorkbook()
  bold_style <- createStyle(textDecoration = "bold")
  italic_style  <- createStyle(textDecoration = "italic")

  # ============================================================================
  # NEW: Handle description_status - compute if not provided
  # ============================================================================
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

  # ============================================================================
  # NEW: Get delimiter - from parameter, input, or default
  # ============================================================================
  description_delimiter <- if (!is.null(delimiter)) {
    delimiter
  } else if (description_status$has_content && exists("input") && !is.null(input$description_delimiter)) {
    input$description_delimiter
  } else {
    "_"  # Default delimiter
  }

  # ============================================================================
  # NEW: Get element order - from parameter, input, or defaults
  # ============================================================================
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

  # DIAGNOSTIC
  cat("\n=== GENERATE LAYOUT TEMPLATE DEBUG ===\n")
  cat("Header columns before processing:", paste(names(plate_id), collapse=", "), "\n")

  # Add basic fields
  plate_id$study_name <- study_accession
  plate_id$experiment_name <- experiment_accession
  plate_id$number_of_wells <- n_wells

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

  # ============================================================================
  # IMPROVED: Extract plate_number from filename or plateid
  # Looks for patterns like "plate_3", "plate 3", "plate.3", "plate3"
  # ============================================================================
  extract_plate_number <- function(text) {
    if (is.na(text) || text == "") return(NA_character_)

    # Try multiple patterns to extract plate number
    # Pattern 1: "plate" followed by separator and number (plate_3, plate 3, plate.3, plate-3)
    match1 <- regmatches(text, regexpr("[Pp]late[_\\s\\.-]+(\\d+)", text, perl = TRUE))
    if (length(match1) > 0 && nchar(match1) > 0) {
      num <- gsub("[^0-9]", "", match1)
      if (nchar(num) > 0) return(paste0("plate_", num))
    }

    # Pattern 2: Just "plate" followed immediately by number (plate3, plate1IgGtot...)
    match2 <- regmatches(text, regexpr("[Pp]late(\\d+)", text, perl = TRUE))
    if (length(match2) > 0 && nchar(match2) > 0) {
      num <- gsub("[^0-9]", "", match2)
      if (nchar(num) > 0) return(paste0("plate_", num))
    }

    return(NA_character_)
  }

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

  # Handle plateid
  if (!"plateid" %in% names(plate_id)) {
    # Try to extract from plate_id column if it exists
    if ("plate_id" %in% names(plate_id)) {
      plate_id$plateid <- plate_id$plate_id
      cat("  ✓ Created plateid from plate_id column\n")
    } else {
      # Create from plate_number
      plate_id$plateid <- plate_id$plate_number
      cat("  ✓ Created plateid from plate_number\n")
    }
  }

  cat("Columns after processing:", paste(names(plate_id), collapse=", "), "\n")

  # Now safely subset
  required_cols <- c("study_name", "experiment_name", "number_of_wells",
                     "plate_number", "plateid", "plate_filename")

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
    antigen_abbreviation = str_split_i(antigen_names," ",1),
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

  # ============================================================================
  # MODIFIED: Use parse_description_with_defaults for sample rows when Description
  # is blank or insufficient
  # ============================================================================
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
    # ============================================================================
    # ORIGINAL: Use str_split_i when Description has content
    # ============================================================================
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

  # ============================================================================
  # Build subject_groups - also using parse_description_with_defaults if needed
  # ============================================================================

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

  # ============================================================================
  # NEW: Apply default values to plates_map if Description was insufficient
  # ============================================================================
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

  # ============================================================================
  # VALIDATION: Collect all issues for README_IMPORTANT
  # ============================================================================
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

  # ============================================================================
  # BUILD README_IMPORTANT CONTENT
  # ============================================================================
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




#' Check if Excel sheet names match expected names
#'
#' @param file_path Path to the Excel file
#' @param expected_sheets Vector of expected sheet names (optional, uses default if not provided)
#' @param exact_match If TRUE, requires exact match; if FALSE, checks if expected sheets exist
#'
#' @return A list with validation status and details
check_sheet_names <- function(file_path,
                              expected_sheets = c("plate_id", "subject_groups", "timepoint",
                                                  "antigen_list", "plates_map", "cell_valid"),
                              exact_match = TRUE) {

  # Get actual sheet names from file
  actual_sheets <- readxl::excel_sheets(file_path)

  # Check for missing sheets
  missing_sheets <- setdiff(expected_sheets, actual_sheets)

  # Check for extra sheets
  extra_sheets <- setdiff(actual_sheets, expected_sheets)

  # Determine if valid

  if (exact_match) {
    is_valid <- length(missing_sheets) == 0 && length(extra_sheets) == 0
  } else {
    is_valid <- length(missing_sheets) == 0
  }

  # Build result
  result <- list(
    valid = is_valid,
    actual_sheets = actual_sheets,
    expected_sheets = expected_sheets,
    missing_sheets = missing_sheets,
    extra_sheets = extra_sheets,
    message = NULL
  )

  # Create informative message
  if (is_valid) {
    result$message <- "All sheet names are valid."
  } else {
    msg_parts <- c()
    if (length(missing_sheets) > 0) {
      msg_parts <- c(msg_parts, paste("Missing sheets:", paste(missing_sheets, collapse = ", ")))
    }
    if (exact_match && length(extra_sheets) > 0) {
      msg_parts <- c(msg_parts, paste("Unexpected sheets:", paste(extra_sheets, collapse = ", ")))
    }
    result$message <- paste(msg_parts, collapse = ". ")
  }

  return(result)
}

#' Remove trailing rows that are all NA
#'
#' @param df A data frame
#'
#' @return Data frame with trailing all-NA rows removed
remove_trailing_na_rows <- function(df) {


  if (nrow(df) == 0) return(df)

  # Find rows where all values are NA
  all_na_rows <- apply(df, 1, function(row) all(is.na(row)))

  # Find the last non-all-NA row
  if (all(all_na_rows)) {
    # All rows are NA, return empty data frame with same structure
    return(df[0, , drop = FALSE])
  }

  # Find trailing NA rows
  last_valid_row <- max(which(!all_na_rows))

  # Return data frame up to last valid row
  return(df[1:last_valid_row, , drop = FALSE])
}


#' Identify rows to skip at the top of each sheet
#'
#' @param file_path Path to the Excel file
#' @param max_rows_to_search Maximum number of rows to search for header (default: 50)
#'
#' @return A named list with skip values for each sheet
identify_rows_to_skip <- function(file_path, max_rows_to_search = 50) {


  # Define sheet types and their header identifiers
  sheet_config <- list(
    plate_id = "study_name",
    subject_groups = "study_name",
    timepoint = "study_name",
    antigen_list = "study_name",
    plates_map = "study_name",
    cell_valid = "l_asy_constraint_method"
  )

  # Get actual sheets in file
  actual_sheets <- readxl::excel_sheets(file_path)

  # Initialize results
  results <- list()

  for (sheet_name in names(sheet_config)) {

    # Check if sheet exists in file
    if (!sheet_name %in% actual_sheets) {
      results[[sheet_name]] <- list(
        skip = NA,
        found = FALSE,
        message = paste("Sheet", sheet_name, "not found in file")
      )
      next
    }

    # Get the header identifier for this sheet
    header_identifier <- sheet_config[[sheet_name]]

    # Read first column of the sheet (limited rows) to find header
    preview_data <- readxl::read_excel(
      file_path,
      sheet = sheet_name,
      n_max = max_rows_to_search,
      col_names = FALSE,
      col_types = "text"
    )

    # Search for header identifier in all columns
    header_row <- NA

    for (i in seq_len(nrow(preview_data))) {
      row_values <- as.character(preview_data[i, ])
      # Check if any cell in this row contains the header identifier
      if (any(grepl(header_identifier, row_values, ignore.case = TRUE))) {
        header_row <- i
        break
      }
    }

    # Calculate skip value (rows before header row)
    if (!is.na(header_row)) {
      skip_value <- header_row - 1
      results[[sheet_name]] <- list(
        skip = skip_value,
        header_row = header_row,
        found = TRUE,
        header_identifier = header_identifier,
        message = paste("Header found at row", header_row, "- skip", skip_value, "rows")
      )
    } else {
      results[[sheet_name]] <- list(
        skip = NA,
        header_row = NA,
        found = FALSE,
        header_identifier = header_identifier,
        message = paste("Header identifier '", header_identifier, "' not found in first",
                        max_rows_to_search, "rows")
      )
    }
  }

  return(results)
}


#' Get skip values as a simple named vector
#'
#' @param file_path Path to the Excel file
#' @param max_rows_to_search Maximum number of rows to search for header
#'
#' @return Named numeric vector of skip values
get_skip_values <- function(file_path, max_rows_to_search = 50) {


  full_results <- identify_rows_to_skip(file_path, max_rows_to_search)

  skip_values <- sapply(full_results, function(x) x$skip)

  return(skip_values)
}

#' Validate that all headers were found
#'
#' @param skip_results Results from identify_rows_to_skip()
#'
#' @return List with validation status and details
validate_skip_results <- function(skip_results) {


  all_found <- all(sapply(skip_results, function(x) x$found))

  failed_sheets <- names(skip_results)[!sapply(skip_results, function(x) x$found)]

  messages <- sapply(skip_results, function(x) x$message)

  list(
    valid = all_found,
    failed_sheets = failed_sheets,
    messages = messages,
    summary = if (all_found) {
      "All header rows successfully identified."
    } else {
      paste("Failed to find headers in:", paste(failed_sheets, collapse = ", "))
    }
  )
}

#' Read a single sheet with skip value and remove trailing NA rows
#'
#' @param file_path Path to the Excel file
#' @param sheet_name Name of the sheet to read
#' @param skip Number of rows to skip
#'
#' @return Cleaned data frame
read_and_clean_sheet <- function(file_path, sheet_name, skip) {


  df <- readxl::read_excel(
    file_path,
    sheet = sheet_name,
    skip = skip
  )

  # Remove trailing NA rows
  df <- remove_trailing_na_rows(df)

  return(df)
}

#' Read all sheets with correct skip values and clean trailing NA rows
#'
#' @param file_path Path to the Excel file
#' @param max_rows_to_search Maximum rows to search for header
#'
#' @return Named list of cleaned data frames
read_all_sheets <- function(file_path, max_rows_to_search = 50) {


  skip_values <- get_skip_values(file_path, max_rows_to_search)

  # Check for any NA values (headers not found)
  if (any(is.na(skip_values))) {
    missing <- names(skip_values)[is.na(skip_values)]
    stop("Could not find header row for sheets: ", paste(missing, collapse = ", "))
  }

  # Read and clean each sheet
  sheet_data <- lapply(names(skip_values), function(sheet_name) {
    read_and_clean_sheet(
      file_path,
      sheet_name,
      skip_values[[sheet_name]]
    )
  })

  names(sheet_data) <- names(skip_values)

  return(sheet_data)
}

#' Full validation and import pipeline
#'
#' @param file_path Path to the Excel file
#' @param expected_sheets Vector of expected sheet names
#' @param max_rows_to_search Maximum rows to search for header
#'
#' @return List with status, messages, and data
import_layout_file <- function(file_path,
                               expected_sheets = c("plate_id", "subject_groups", "timepoint",
                                                   "antigen_list", "plates_map", "cell_valid"),
                               max_rows_to_search = 50) {


  result <- list(
    success = FALSE,
    messages = c(),
    data = NULL,
    row_counts = NULL
  )

  # Step 1: Check sheet names exist
  actual_sheets <- readxl::excel_sheets(file_path)
  missing_sheets <- setdiff(expected_sheets, actual_sheets)

  if (length(missing_sheets) > 0) {
    result$messages <- c(result$messages,
                         paste("Missing sheets:", paste(missing_sheets, collapse = ", ")))
    return(result)
  }

  # Step 2: Identify skip values
  skip_results <- identify_rows_to_skip(file_path, max_rows_to_search)
  validation <- validate_skip_results(skip_results)

  if (!validation$valid) {
    result$messages <- c(result$messages, validation$summary)
    return(result)
  }

  # Step 3: Read and clean all sheets
  tryCatch({
    sheet_data <- read_all_sheets(file_path, max_rows_to_search)

    result$success <- TRUE
    result$data <- sheet_data
    result$row_counts <- sapply(sheet_data, nrow)
    result$messages <- c(result$messages, "All sheets imported successfully.")

  }, error = function(e) {
    result$messages <- c(result$messages, paste("Error reading sheets:", e$message))
  })

  return(result)
}

#' Extract plate identification columns from plate metadata list
#'
#' @param plate_metadata_list A named list of tibbles/data frames, each containing
#'   plate metadata with columns: plate, plateid, plate_id, source_file
#'
#' @return A data frame with one row per plate and columns: plate, plateid, plate_id, source_file
#'
extract_plate_identifiers <- function(plate_metadata_list) {

  # Define columns to extract
  cols_to_extract <- c("plate", "plateid", "plate_id", "source_file", "reader_serial_number",
                       "file_name", "rp1_pmt_volts", "rp1_target", "acquisition_date")

  # Extract and combine
  result <- do.call(rbind, lapply(names(plate_metadata_list), function(nm) {
    df <- plate_metadata_list[[nm]]

    # Check which columns exist
    available_cols <- intersect(cols_to_extract, names(df))
    missing_cols <- setdiff(cols_to_extract, names(df))

    # Extract available columns (take first row if multiple)
    extracted <- df[1, available_cols, drop = FALSE]

    # Add NA for any missing columns
    for (col in missing_cols) {
      extracted[[col]] <- NA_character_
    }

    # Ensure column order is consistent
    extracted[, cols_to_extract, drop = FALSE]
  }))

  # Convert to plain data.frame and reset row names
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  rownames(result) <- NULL

  return(result)
}

#' Transpose batch header data frame
#'
#' @param batch_header A data frame with a source_file column to use as column names
#' @param id_column The column to use for naming the transposed columns (default: "source_file")
#'
#' @return A transposed data frame where original rows become columns named by source_file values
#'
transpose_batch_header <- function(batch_header, id_column = "source_file") {

  # Get the column names to use for new columns
  new_col_names <- batch_header[[id_column]]

  # Remove the id column from the data before transposing
  data_to_transpose <- batch_header[, names(batch_header) != id_column, drop = FALSE]

  # Transpose the data
  transposed <- as.data.frame(t(data_to_transpose), stringsAsFactors = FALSE)

  # Set column names to the source_file values
  names(transposed) <- new_col_names

  # The row names are now the original column names - convert to a column
  transposed$variable <- rownames(transposed)
  rownames(transposed) <- NULL

  # Reorder so variable is first
  transposed <- transposed[, c("variable", new_col_names)]

  return(transposed)
}

# Join sample dilutions for the plate to the header

# In batch_layout_functions.R - Replace the construct_batch_upload_metadata function

construct_batch_upload_metadata <- function(plates_map, plate_metadata_list, currentuser, workspace_id) {

  cat("\n=== CONSTRUCT BATCH UPLOAD METADATA ===\n")
  cat("plates_map columns:", paste(names(plates_map), collapse=", "), "\n")
  cat("Number of plate metadata items:", length(plate_metadata_list), "\n")
  cat("Plate metadata names:", paste(names(plate_metadata_list), collapse=", "), "\n")

  # Check if plate_metadata_list items have 'plate' column
  first_item <- plate_metadata_list[[1]]
  cat("First metadata item columns:", paste(names(first_item), collapse=", "), "\n")

  # ============================================================================
  # FIX: Get sample dilution factors - use the MOST COMMON value per plate
  # or the minimum if there are multiple dilutions (for mixed dilution plates)
  # ============================================================================
  sample_dilutions_raw <- plates_map[plates_map$specimen_type == "X" &
                                       !is.na(plates_map$plate_number),
                                     c("plate_number", "specimen_dilution_factor")]

  # Remove NA dilution factors before aggregation
  sample_dilutions_raw <- sample_dilutions_raw[!is.na(sample_dilutions_raw$specimen_dilution_factor), ]

  # FIX: Use a custom aggregation that picks the most frequent dilution factor
  # If ties, use the minimum value
  sample_dilutions_by_plate <- do.call(rbind, lapply(
    split(sample_dilutions_raw, sample_dilutions_raw$plate_number),
    function(df) {
      if (nrow(df) == 0) return(NULL)

      # Get frequency table
      freq_table <- table(df$specimen_dilution_factor)

      # Get the most frequent value (or min if tied)
      max_freq <- max(freq_table)
      most_common <- as.numeric(names(freq_table)[freq_table == max_freq])
      representative_dilution <- min(most_common)  # Use min if there's a tie

      data.frame(
        plate_number = unique(df$plate_number),
        sample_dilution_factor = representative_dilution,
        stringsAsFactors = FALSE
      )
    }
  ))
  rownames(sample_dilutions_by_plate) <- NULL

  cat("Sample dilutions by plate:\n")
  print(sample_dilutions_by_plate)
  cat("\n")

  # ============================================================================
  # FIX: Get experiment by plate - ensure TRUE uniqueness by plate_number
  # Remove 'feature' from distinct to prevent multiplication
  # ============================================================================
  experiment_by_plate <- unique(
    plates_map[!is.na(plates_map$plate_number),
               c("study_name", "plate_number", "experiment_name"),
               drop = FALSE]
  )

  # CRITICAL FIX: Ensure only ONE row per plate_number
  experiment_by_plate <- experiment_by_plate[!duplicated(experiment_by_plate$plate_number), ]

  experiment_by_plate$workspace_id <- workspace_id
  experiment_by_plate$auth0_user <- currentuser

  cat("Experiment by plate (after dedup):\n")
  print(experiment_by_plate)
  cat("Number of rows:", nrow(experiment_by_plate), "\n")
  cat("\n")

  source_file_by_plate <- extract_plate_identifiers(plate_metadata_list)
  source_file_by_plate$plate_number <- source_file_by_plate$plate

  cat("Source file by plate:\n")
  print(source_file_by_plate)
  cat("Number of rows:", nrow(source_file_by_plate), "\n")
  cat("\n")

  # ============================================================================
  # FIX: Perform joins carefully with validation
  # ============================================================================

  # First join
  plate_metadata_list_updated <- merge(
    experiment_by_plate,
    sample_dilutions_by_plate,
    by = "plate_number",
    all.x = TRUE
  )

  cat("After first join - rows:", nrow(plate_metadata_list_updated), "\n")

  # Check for unexpected row multiplication
  if (nrow(plate_metadata_list_updated) > nrow(experiment_by_plate)) {
    cat("⚠️  WARNING: Row multiplication detected after first join!\n")
    cat("   Expected:", nrow(experiment_by_plate), "Got:", nrow(plate_metadata_list_updated), "\n")
    # Deduplicate
    plate_metadata_list_updated <- plate_metadata_list_updated[
      !duplicated(plate_metadata_list_updated$plate_number), ]
    cat("   After dedup:", nrow(plate_metadata_list_updated), "\n")
  }

  # Second join
  plate_metadata_list_updated <- merge(
    plate_metadata_list_updated,
    source_file_by_plate,
    by = "plate_number",
    all.x = TRUE
  )

  cat("After second join - rows:", nrow(plate_metadata_list_updated), "\n")

  # Check for unexpected row multiplication again
  if (nrow(plate_metadata_list_updated) > length(plate_metadata_list)) {
    cat("⚠️  WARNING: Row multiplication detected after second join!\n")
    cat("   Expected:", length(plate_metadata_list), "Got:", nrow(plate_metadata_list_updated), "\n")
    # Deduplicate by plate_number (the true unique key)
    plate_metadata_list_updated <- plate_metadata_list_updated[
      !duplicated(plate_metadata_list_updated$plate_number), ]
    cat("   After dedup:", nrow(plate_metadata_list_updated), "\n")
  }

  cat("plate_metadata_list_updated:\n")
  print(plate_metadata_list_updated)
  cat("\n")

  cat("✓ Batch metadata construction complete\n")
  cat("=====================================\n\n")

  return(plate_metadata_list_updated)
}
# construct_batch_upload_metadata <- function(plates_map, plate_metadata_list, currentuser, workspace_id) {
#
#   cat("\n=== CONSTRUCT BATCH UPLOAD METADATA ===\n")
#   cat("plates_map columns:", paste(names(plates_map), collapse=", "), "\n")
#   cat("Number of plate metadata items:", length(plate_metadata_list), "\n")
#   cat("Plate metadata names:", paste(names(plate_metadata_list), collapse=", "), "\n")
#
#   # Check if plate_metadata_list items have 'plate' column
#   first_item <- plate_metadata_list[[1]]
#   cat("First metadata item columns:", paste(names(first_item), collapse=", "), "\n")
#
#   # # Get unique sample dilution factors by plate_number
#   # sample_dilutions_by_plate <- as.data.frame(dplyr::distinct(
#   #   plates_map[plates_map$specimen_type == "X",
#   #              c("plate_number", "specimen_type", "specimen_dilution_factor")])
#   # )
#   #
#   # sample_dilutions_by_plate <- sample_dilutions_by_plate[!is.na(sample_dilutions_by_plate$plate_number), ]
#   #
#   # names(sample_dilutions_by_plate)[names(sample_dilutions_by_plate) == "specimen_dilution_factor"] <- "sample_dilution_factor"
#   #
#   # cat("Sample dilutions by plate:\n")
#   # print(sample_dilutions_by_plate)
#   #
#   # # Get experiment by plate
#   # experiment_by_plate <- as.data.frame(dplyr::distinct(
#   #   plates_map[, c("study_name", "plate_number", "experiment_name", "feature")])
#   # )
#
#   # Get sample dilution factors - aggregate to ONE per plate
#   sample_dilutions_raw <- plates_map[plates_map$specimen_type == "X" &
#                                        !is.na(plates_map$plate_number),
#                                      c("plate_number", "specimen_dilution_factor")]
#
#   # Aggregate to get ONE dilution factor per plate (use minimum as representative)
#   sample_dilutions_by_plate <- aggregate(
#     specimen_dilution_factor ~ plate_number,
#     data = sample_dilutions_raw,
#     FUN = function(x) min(x, na.rm = TRUE)
#   )
#   names(sample_dilutions_by_plate)[names(sample_dilutions_by_plate) ==
#                                      "specimen_dilution_factor"] <- "sample_dilution_factor"
#
#   # Get experiment by plate - exclude 'feature' to avoid duplicates
#   experiment_by_plate <- as.data.frame(dplyr::distinct(
#     plates_map[!is.na(plates_map$plate_number),
#                c("study_name", "plate_number", "experiment_name")]
#   ))
#
#   # Remove any remaining duplicates (defensive)
#   experiment_by_plate <- experiment_by_plate[!duplicated(experiment_by_plate$plate_number), ]
#   experiment_by_plate$workspace_id <- workspace_id
#   experiment_by_plate$auth0_user <- currentuser
#
#   cat("Experiment by plate:\n")
#   print(experiment_by_plate)
#   cat("\n")
#
#   source_file_by_plate <- extract_plate_identifiers(plate_metadata_list)
#   source_file_by_plate$plate_number <- source_file_by_plate$plate
#
#   cat("Source file by plate:\n")
#   print(source_file_by_plate)
#   cat("\n")
#
#   plate_metadata_list_updated <- inner_join(experiment_by_plate, sample_dilutions_by_plate, by = "plate_number")
#   plate_metadata_list_updated <- inner_join(plate_metadata_list_updated, source_file_by_plate, by = "plate_number")
# #
# #   # Restore names
# #   names(plate_metadata_list_updated) <- names(plate_metadata_list)
#
#   cat("plate_metadata_list_updated:\n")
#   print(plate_metadata_list_updated)
#   cat("\n")
#
#   cat("✓ Batch metadata construction complete\n")
#   cat("=====================================\n\n")
#
#   return(plate_metadata_list_updated)
# }

plot_plate_layout <- function(plates_map, plate_id_data) {
  #plates_map_v <<- plates_map
  # Join in number_of_wells info
  plates_map_joined <- merge(
    plates_map,
    plate_id_data[, c("study_name", "experiment_name", "plate_number", "number_of_wells")],
    by = c("study_name", "experiment_name", "plate_number"),
    all.x = TRUE
  )
  plates_map_joined$specimen_type <- ifelse(is.na(plates_map_joined$specimen_type),"E",plates_map_joined$specimen_type)
  specimen_labels <- c(
    "S" = "Standard Curve",
    "C" = "Controls",
    "X" = "Samples",
    "B" = "Blanks",
    "E" = "Empty"
  )
  plates_map_joined$`Specimen Type` <- specimen_labels[plates_map_joined$specimen_type]

  # Initialize list for storing plots
  plate_plots <- list()
  specimen_palette <-  c("Blanks" = "#F3C300",
                         "Controls" = "#2B3D26",
                         "Standard Curve" = "#A1CAF1",
                         "Samples" = "#8DB600",
                         "Empty" = "white"
  )

  # Get unique combinations of study, experiment, and plate
  unique_combos <- unique(
    plates_map_joined[, c("study_name", "experiment_name", "plate_number")]
  )
  # remove trailing NA rows
  unique_combos <- unique_combos[rowSums(is.na(unique_combos)) != ncol(unique_combos), ]


  # Loop through each combination
  for (i in seq_len(nrow(unique_combos))) {
    study <- unique_combos$study_name[i]
    exp <- unique_combos$experiment_name[i]
    plate <- unique_combos$plate_number[i]

    plates_map_joined_filtered <- plates_map_joined[plates_map_joined$study_name == study & plates_map_joined$experiment_name == exp
                                                    & plates_map_joined$plate_number == plate,]
    plates_map_joined_filtered <- plates_map_joined_filtered[rowSums(is.na(plates_map_joined_filtered)) != ncol(plates_map_joined_filtered), ]

    n_wells <- unique(plates_map_joined_filtered$number_of_wells)

    plot_title <- paste0(
      "Study: ", study,
      " | Experiment: ", exp,
      " | Plate: ", plate,
      " (", n_wells, " wells)"
    )

    plate_layout <- plate_plot_plotly2(
      data = plates_map_joined_filtered,
      plate_size = n_wells,
      title = plot_title,
      colour = specimen_palette
    )

    list_name <- paste(study, exp, plate, sep = "_")
    plate_plots[[list_name]] <- plate_layout
  }

  #plate_plots <<- plate_plots

  return(plate_plots)
}

plate_plot_plotly2 <- function(data, plate_size = 96, title = NULL, colour = NULL) {
  #data_v <<- data
  cat("Plate")
  print(unique(data$plate_number))
  cat("Plate Size")
  print(plate_size)
  data <- data |>
    dplyr::mutate(
      row = stringr::str_extract(well, pattern = "[:upper:]+"),
      col = as.numeric(stringr::str_extract(well, pattern = "\\d+")),
      row_num = as.numeric(match(row, LETTERS))
    )

  # determine number of rows and columns
  dims <- switch(
    as.character(plate_size),
    "6" = c(2, 3),
    "12" = c(3, 4),
    "24" = c(4, 6),
    "48" = c(6, 8),
    "96" = c(8, 12),
    "384" = c(16, 24),
    "1536" = c(32, 48)
  )
  n_rows <- dims[1]
  n_cols <- dims[2]

  # make grid of all wells
  plate_layout_grid <- expand.grid(
    Row = LETTERS[1:n_rows],
    Column = 1:n_cols
  )
  plate_layout_grid$well <- paste(plate_layout_grid$Row, plate_layout_grid$Column, sep = "")

  # # find empty wells
  # empty_wells <- setdiff(plate_layout_grid$well, data$well)
  # empty_df <- plate_layout_grid |>
  #   dplyr::filter(well %in% empty_wells) |>
  #   dplyr::mutate(
  #     row_num = as.numeric(match(Row, LETTERS)),
  #     col = Column,
  #     `Specimen Type` = "Empty Well"
  #   )

  # flip y-axis to match lab plate layout
  data$row_num <- abs(data$row_num - (n_rows + 1))
  # empty_df$row_num <- abs(empty_df$row_num - (n_rows + 1))

  # Build plot
  p <- plot_ly() %>%
    add_trace(
      data = data,
      x = ~col,
      y = ~row_num,
      type = "scatter",
      mode = "markers",
      color = ~`Specimen Type`,
      colors = colour,
      text = ~paste(
        "Well:", well,
        "<br>Specimen Type:", `Specimen Type`,
        ifelse(`Specimen Type` == "Samples",
               paste0("<br>Patient ID: ", subject_id,
                      "<br>Timepoint: ", timepoint_tissue_abbreviation),
               ""),
        "<br>Biosample Barcode:", biosample_id_barcode,
        "<br>Specimen Dilution Factor:", specimen_dilution_factor,
        "<br>Feature:", feature
      ),
      hoverinfo = "text",
      marker = list(size = 20, line = list(width = 1, color = "black"), symbol = "circle")
    ) %>%
    # add_trace(
    #   data = empty_df,
    #   x = ~col,
    #   y = ~row_num,
    #   name = "Empty Well",
    #   type = "scatter",
    #   mode = "markers",
    #   marker = list(size = 20, color = "white", line = list(width = 1, color = "grey")),
    #   hoverinfo = "text",
    #   text = ~paste("Well:", well, "<br>Empty")
    # ) %>%
    layout(
      title = title,
      legend = list(title = list(text = "Specimen Type")),
      yaxis = list(
        title = "",
        tickvals = seq(1, n_rows),
        ticktext = rev(LETTERS[1:n_rows]),
        showgrid = FALSE
      ),
      xaxis = list(
        title = "",
        tickvals = seq(1, n_cols),
        showgrid = FALSE
      ),
      showlegend = TRUE,
      shapes = list(
        list(
          type = "rect",
          x0 = 0.5, x1 = n_cols + 0.5,
          y0 = 0.5, y1 = n_rows + 0.5,
          line = list(color = "black", width = 1),
          fillcolor = "rgba(0,0,0,0)"
        )
      )
    )

  return(p)
}

# Helper function to combine plate metadata from batch load
combine_plate_metadata <- function(head_list) {
  plate_metadata <- do.call(rbind, lapply(names(head_list), function(nm) {
    df <- head_list[[nm]]
    # Ensure sample_dilution_factor column exists
    if (!"sample_dilution_factor" %in% names(df)) {
      df$sample_dilution_factor <- 1
      cat("  ⚠ Added default sample_dilution_factor for:", nm, "\n")
    }
    df
  }))

  return(plate_metadata)
}

## this function merges layout info and raw file info for uploading samples
prepare_batch_bead_assay_samples <- function(sample_plate_map, combined_plate_data, batch_metadata, antigen_import_list, subject_map) {
  sample_joined <- merge(sample_plate_map, subject_map,
                         by = c("study_name", "subject_id"),
                         all.x = TRUE
  )
  sample_joined$agroup <-ifelse(is.na(sample_joined$groupb),
                                sample_joined$groupa,
                                paste(sample_joined$groupa, sample_joined$groupb, sep = "_"))
  # sample_j <- merge(
  #   sample_joined,
  #   combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
  #   by.x = c("experiment_name", "plate_number", "well"),
  #   by.y = c("experiment_name", "plate", "Well")
  # )
  sample_j <- merge(
    sample_joined,
    combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
    by.x = c("plate_number", "well"),
    by.y = c("plate", "Well")
  )

  sample_long <- sample_j %>%
    pivot_longer(
      cols = matches("\\([0-9]+\\)"), # all columns containing bead count
      names_to = "antigen",
      values_to = "mfi_bead"
    ) %>%
    mutate(
      # Remove the bead number from column name to get clean antigen name
      antigen = gsub("\\.", " ", antigen),#str_remove(antigen, "\\.\\([0-9]+\\)"),
      # Split MFI and bead count from the value column
      antibody_mfi = as.numeric(str_extract(mfi_bead, "^[0-9.]+")),
      antibody_n = as.numeric(str_extract(mfi_bead, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    select(-mfi_bead) %>%
    relocate(antigen, antibody_mfi, antibody_n, .after = biosample_id_barcode)


  # join in the antigen abbreviation
  sample_long2 <- merge(sample_long, antigen_import_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
                        by.x = c("study_name", "experiment_name", "antigen"),
                        by.y =  c("study_name", "experiment_name", "antigen_label_on_plate"),
                        all.x = T)

  # join in the plate_id from the metadata
  sample_long2 <- merge(sample_long2,
                        batch_metadata[, c("source_file", "plate_id")],
                        by = "source_file",
                        all.x = T)

  sample_long2 <- sample_long2[, !names(sample_long2) %in% c("antigen")]
  names(sample_long2)[names(sample_long2) == "antigen_abbreviation"] <- "antigen"
  names(sample_long2)[names(sample_long2) == "study_name"] <- "study_accession"
  names(sample_long2)[names(sample_long2) == "experiment_name"] <- "experiment_accession"
  names(sample_long2)[names(sample_long2) == "specimen_type"] <- "stype"
  names(sample_long2)[names(sample_long2) == "specimen_dilution_factor"] <- "dilution"
  names(sample_long2)[names(sample_long2) == "% Agg Beads"] <- "pctaggbeads"
  names(sample_long2)[names(sample_long2) == "Sampling Errors"] <- "samplingerrors"
  names(sample_long2)[names(sample_long2) == "timepoint_tissue_abbreviation"] <- "timeperiod"
  names(sample_long2)[names(sample_long2) == "subject_id"] <- "patientid"
  names(sample_long2)[names(sample_long2) == "biosample_id_barcode"] <- "sampleid"

  samples_to_upload <- sample_long2[, c("study_accession", "experiment_accession", "plate_id", "timeperiod", "patientid", "well",
                                        "stype", "sampleid", "agroup", "dilution", "pctaggbeads", "samplingerrors", "antigen",
                                        "antibody_mfi", "antibody_n", "feature")]
  # Before returning, add debug:
  cat("\n=== DEBUG: Inside prepare_batch_bead_assay_samples ===\n")
  cat("Result columns:", paste(names(samples_to_upload), collapse = ", "), "\n")

  if ("sampleid" %in% names(samples_to_upload)) {
    cat("sampleid present, null count:", sum(is.na(samples_to_upload$sampleid)), "\n")
  } else {
    cat("WARNING: sampleid column NOT FOUND\n")
    cat("Looking for similar columns:",
        paste(grep("sample", names(samples_to_upload), value = TRUE, ignore.case = TRUE), collapse = ", "), "\n")
  }

  # Check subject_map for the source of sampleid
  cat("\nsubject_map columns:", paste(names(subject_map), collapse = ", "), "\n")
  if ("sampleid" %in% names(subject_map)) {
    cat("sampleid in subject_map, null count:", sum(is.na(subject_map$sampleid)), "\n")
  }

  return(samples_to_upload)
}

prepare_batch_bead_assay_standards <- function(standard_plate_map, combined_plate_data, antigen_import_list, batch_metadata) {
  # standard_j <- merge(
  #   standard_plate_map,
  #   combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
  #   by.x = c("experiment_name", "plate_number", "well"),
  #   by.y = c("experiment_name", "plate", "Well")
  # )
  standard_j <- merge(
    standard_plate_map,
    combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
    by.x = c("plate_number", "well"),
    by.y = c("plate", "Well")
  )

  standard_long <- standard_j %>%
    pivot_longer(
      cols = matches("\\([0-9]+\\)"), # all columns containing bead count
      names_to = "antigen",
      values_to = "mfi_bead"
    ) %>%
    mutate(
      # Remove the bead number from column name to get clean antigen name
      antigen = gsub("\\.", " ", antigen),#str_remove(antigen, "\\.\\([0-9]+\\)"),
      # Split MFI and bead count from the value column
      antibody_mfi = as.numeric(str_extract(mfi_bead, "^[0-9.]+")),
      antibody_n = as.numeric(str_extract(mfi_bead, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    select(-mfi_bead) %>%
    relocate(antigen, antibody_mfi, antibody_n, .after = biosample_id_barcode)

  standard_long2 <- merge(standard_long, antigen_import_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
                          by.x = c("study_name", "experiment_name", "antigen"),
                          by.y =  c("study_name", "experiment_name", "antigen_label_on_plate"),
                          all.x = T)

  # join in the plate_id from the metadata
  standard_long2 <- merge(standard_long2,
                          batch_metadata[, c("source_file", "plate_id")],
                          by = "source_file",
                          all.x = T)

  standard_long2 <- standard_long2[, !names(standard_long2) %in% c("antigen")]
  names(standard_long2)[names(standard_long2) == "antigen_abbreviation"] <- "antigen"
  names(standard_long2)[names(standard_long2) == "study_name"] <- "study_accession"
  names(standard_long2)[names(standard_long2) == "experiment_name"] <- "experiment_accession"
  names(standard_long2)[names(standard_long2) == "specimen_type"] <- "stype"
  names(standard_long2)[names(standard_long2) == "specimen_source"] <- "source"
  names(standard_long2)[names(standard_long2) == "specimen_dilution_factor"] <- "dilution"
  names(standard_long2)[names(standard_long2) == "biosample_id_barcode"] <- "sampleid"
  names(standard_long2)[names(standard_long2) == "% Agg Beads"] <- "pctaggbeads"
  names(standard_long2)[names(standard_long2) == "Sampling Errors"] <- "samplingerrors"

  standards_to_upload <- standard_long2[, c("study_accession", "experiment_accession", "plate_id","well",
                                            "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors", "antigen",
                                            "antibody_mfi", "antibody_n", "feature")]
  return(standards_to_upload)
}

prepare_batch_bead_assay_blanks <- function(blanks_plate_map, combined_plate_data, antigen_import_list, batch_metadata) {
  # blanks_j <- merge(blanks_plate_map, combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
  #                   by.x = c("experiment_name", "plate_number", "well"),
  #                   by.y = c("experiment_name", "plate", "Well"))
  blanks_j <- merge(blanks_plate_map, combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
                    by.x = c("plate_number", "well"),
                    by.y = c("plate", "Well"))

  blanks_long <- blanks_j %>%
    pivot_longer(
      cols = matches("\\([0-9]+\\)"), # all columns containing bead count
      names_to = "antigen",
      values_to = "mfi_bead"
    ) %>%
    mutate(
      # Remove the bead number from column name to get clean antigen name
      antigen = gsub("\\.", " ", antigen),#str_remove(antigen, "\\.\\([0-9]+\\)"),
      # Split MFI and bead count from the value column
      antibody_mfi = as.numeric(str_extract(mfi_bead, "^[0-9.]+")),
      antibody_n = as.numeric(str_extract(mfi_bead, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    select(-mfi_bead) %>%
    relocate(antigen, antibody_mfi, antibody_n, .after = biosample_id_barcode)

  blanks_long2 <- merge(blanks_long, antigen_import_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
                        by.x = c("study_name", "experiment_name", "antigen"),
                        by.y =  c("study_name", "experiment_name", "antigen_label_on_plate"),
                        all.x = T)

  blanks_long2 <- merge(blanks_long2,
                        batch_metadata[, c("source_file", "plate_id")],
                        by = "source_file",
                        all.x = T)

  blanks_long2 <- blanks_long2[, !names(blanks_long2) %in% c("antigen")]
  names(blanks_long2)[names(blanks_long2) == "antigen_abbreviation"] <- "antigen"
  names(blanks_long2)[names(blanks_long2) == "study_name"] <- "study_accession"
  names(blanks_long2)[names(blanks_long2) == "experiment_name"] <- "experiment_accession"
  names(blanks_long2)[names(blanks_long2) == "specimen_type"] <- "stype"
  names(blanks_long2)[names(blanks_long2) == "specimen_dilution_factor"] <- "dilution"
  names(blanks_long2)[names(blanks_long2) == "% Agg Beads"] <- "pctaggbeads"
  names(blanks_long2)[names(blanks_long2) == "Sampling Errors"] <- "samplingerrors"

  blanks_to_upload <- blanks_long2[, c("study_accession", "experiment_accession", "plate_id","well",
                                       "stype", "pctaggbeads", "samplingerrors", "antigen",
                                       "antibody_mfi", "antibody_n", "dilution", "feature")]

  return(blanks_to_upload)

}

prepare_batch_bead_assay_controls <- function(controls_plate_map, combined_plate_data, antigen_import_list, batch_metadata) {
  # controls_j <- merge(controls_plate_map, combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
  #                     by.x = c("experiment_name", "plate_number", "well"),
  #                     by.y = c("experiment_name", "plate", "Well"))
  controls_j <- merge(controls_plate_map, combined_plate_data[, !(names(combined_plate_data) %in% c("Type", "Description"))],
                      by.x = c("plate_number", "well"),
                      by.y = c("plate", "Well"))

  controls_long <- controls_j %>%
    pivot_longer(
      cols = matches("\\([0-9]+\\)"), # all columns containing bead count
      names_to = "antigen",
      values_to = "mfi_bead"
    ) %>%
    mutate(
      # Remove the bead number from column name to get clean antigen name
      antigen = gsub("\\.", " ", antigen),#str_remove(antigen, "\\.\\([0-9]+\\)"),
      # Split MFI and bead count from the value column
      antibody_mfi = as.numeric(str_extract(mfi_bead, "^[0-9.]+")),
      antibody_n = as.numeric(str_extract(mfi_bead, "(?<=\\()[0-9]+(?=\\))"))
    ) %>%
    select(-mfi_bead) %>%
    relocate(antigen, antibody_mfi, antibody_n, .after = biosample_id_barcode)

  # merge in tehe antigen name
  controls_long2 <- merge(controls_long, antigen_import_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
                          by.x = c("study_name", "experiment_name", "antigen"),
                          by.y =  c("study_name", "experiment_name", "antigen_label_on_plate"),
                          all.x = T)

  # merge in the plate_id
  controls_long2 <- merge(controls_long2,
                        batch_metadata[, c("source_file", "plate_id")],
                        by = "source_file",
                        all.x = T)


  controls_long2 <- controls_long2[, !names(controls_long2) %in% c("antigen")]
  names(controls_long2)[names(controls_long2) == "antigen_abbreviation"] <- "antigen"
  names(controls_long2)[names(controls_long2) == "study_name"] <- "study_accession"
  names(controls_long2)[names(controls_long2) == "experiment_name"] <- "experiment_accession"
  names(controls_long2)[names(controls_long2) == "specimen_type"] <- "stype"
  names(controls_long2)[names(controls_long2) == "biosample_id_barcode"] <- "sampleid"
  names(controls_long2)[names(controls_long2) == "specimen_source"] <- "source"

  names(controls_long2)[names(controls_long2) == "specimen_dilution_factor"] <- "dilution"
  names(controls_long2)[names(controls_long2) == "%.Agg.Beads"] <- "pctaggbeads"
  names(controls_long2)[names(controls_long2) == "Sampling.Errors"] <- "samplingerrors"

  controls_to_upload <- controls_long2[, c("study_accession", "experiment_accession", "plate_id","well",
                                           "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors", "antigen",
                                           "antibody_mfi", "antibody_n", "feature")]
  return(controls_to_upload)
}

prepare_batch_antigen_family <- function(antigen_import_list) {
  antigen_import_list_df <- antigen_import_list[, c("study_name", "experiment_name", "antigen_abbreviation", "antigen_family", "standard_curve_max_concentration", "antigen_name", "virus_bacterial_strain", "antigen_source",
                                      "catalog_number", "l_asy_min_constraint", "l_asy_max_constraint", "l_asy_constraint_method")]
  names(antigen_import_list_df)[names(antigen_import_list_df) == "study_name"] <- "study_accession"
  names(antigen_import_list_df)[names(antigen_import_list_df) == "experiment_name"] <- "experiment_accession"
  names(antigen_import_list_df)[names(antigen_import_list_df) == "standard_curve_max_concentration"] <- "standard_curve_concentration"
  names(antigen_import_list_df)[names(antigen_import_list_df) == "antigen_abbreviation"] <- "antigen"

  return(antigen_import_list_df)
}

prepare_planned_visits <- function(timepoint_map) {
  names(timepoint_map)[names(timepoint_map) == "study_name"] <- "study_accession"
  names(timepoint_map)[names(timepoint_map) == "timepoint_tissue_abbreviation"] <- "timepoint_name"
  names(timepoint_map)[names(timepoint_map) == "tissue_type"] <- "type"
  names(timepoint_map)[names(timepoint_map) == "tissue_subtype"] <- "subtype"
  names(timepoint_map)[names(timepoint_map) == "description"] <- "end_rule"
  names(timepoint_map)[names(timepoint_map) == "min_time_since_day_0"] <- "min_start_day"
  names(timepoint_map)[names(timepoint_map) == "max_time_since_day_0"] <- "max_start_day"



  return(timepoint_map)
}

prepare_batch_header <- function(metadata_batch) {

  # Ensure sample_dilution_factor exists and has no NA values
  if (!"sample_dilution_factor" %in% names(metadata_batch)) {
    metadata_batch$sample_dilution_factor <- 1
    cat("  ⚠ Added default sample_dilution_factor column\n")
  } else {
    # Replace NA values with default
    na_count <- sum(is.na(metadata_batch$sample_dilution_factor))
    if (na_count > 0) {
      metadata_batch$sample_dilution_factor[is.na(metadata_batch$sample_dilution_factor)] <- 1
      cat("  ⚠ Replaced", na_count, "NA sample_dilution_factor values with default 1\n")
    }
  }

  metadata_batch <- metadata_batch[,c("study_name", "experiment_name", "plate_id", "file_name", "acquisition_date",
                                      "reader_serial_number", "rp1_pmt_volts", "rp1_target",
                                      "auth0_user", "workspace_id", "plateid", "plate",
                                      "sample_dilution_factor",
                                      "n_wells")]
  names(metadata_batch)[names(metadata_batch) == "study_name"] <- "study_accession"
  names(metadata_batch)[names(metadata_batch) == "experiment_name"] <- "experiment_accession"
  metadata_batch$assay_response_variable <- "mfi"
  metadata_batch$assay_independent_variable <- "concentration"

  nk_cols <- c("study_accession", "experiment_accession", "plate_id")
  n_before <- nrow(metadata_batch)
  metadata_batch <- metadata_batch[!duplicated(metadata_batch[, nk_cols, drop = FALSE]), ]
  n_after <- nrow(metadata_batch)

  if (n_before != n_after) {
    cat("  ⚠ Removed", n_before - n_after, "duplicate header rows\n")
  }

  # Debug output
  cat("\n=== prepare_batch_header DEBUG ===\n")
  cat("Columns in prepared header:", paste(names(metadata_batch), collapse = ", "), "\n")
  cat("Number of plates:", nrow(metadata_batch), "\n")
  cat("sample_dilution_factor values:", paste(metadata_batch$sample_dilution_factor, collapse = ", "), "\n")
  cat("===================================\n")

  return(metadata_batch)
}

parse_metadata_df <- function(header_matrix) {
  # header_matrix is a matrix/data.frame from reading Excel header rows
  # It contains rows like "File Name: C:\\path\\file.xlsx"

  # Convert to data frame if needed
  if (!is.data.frame(header_matrix)) {
    header_matrix <- as.data.frame(header_matrix, stringsAsFactors = FALSE)
  }

  # Initialize result
  result <- data.frame(
    file_name = NA_character_,
    acquisition_date = NA_character_,
    rp1_pmt_volts = NA_character_,
    rp1_target = NA_character_,
    plateid = NA_character_,
    plate = NA_character_,
    stringsAsFactors = FALSE
  )

  # Parse each row
  for (i in 1:nrow(header_matrix)) {
    # Get the first column value
    cell_value <- as.character(header_matrix[i, 1])

    # Extract based on label
    if (grepl("File Name:", cell_value, ignore.case = TRUE)) {
      result$file_name <- trimws(gsub("^.*File Name:\\s*", "", cell_value))
    } else if (grepl("Acquisition Date:", cell_value, ignore.case = TRUE)) {
      result$acquisition_date <- trimws(gsub("^.*Acquisition Date:\\s*", "", cell_value))
    } else if (grepl("RP1 PMT.*Volts", cell_value, ignore.case = TRUE)) {
      result$rp1_pmt_volts <- trimws(gsub("^.*RP1 PMT.*Volts.*:\\s*", "", cell_value))
    } else if (grepl("RP1 Target:", cell_value, ignore.case = TRUE)) {
      result$rp1_target <- trimws(gsub("^.*RP1 Target:\\s*", "", cell_value))
    } else if (grepl("Plate ID:", cell_value, ignore.case = TRUE)) {
      extracted <- trimws(gsub("^.*Plate ID:\\s*", "", cell_value))
      result$plateid <- ifelse(extracted == "" || is.na(extracted), NA_character_, extracted)
    }
  }

  return(result)
}

