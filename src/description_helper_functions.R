# ============================================================================
# Description Validation Helper Functions
# Add these functions to plate_validator_functions.R or batch_layout_functions.R
# ============================================================================

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
