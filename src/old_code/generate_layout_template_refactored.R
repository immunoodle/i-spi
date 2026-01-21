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

  # Handle plate_number
  if ("plate" %in% names(plate_id)) {
    names(plate_id)[names(plate_id) == "plate"] <- "plate_number"
    cat("  ✓ Renamed 'plate' to 'plate_number'\n")
  } else if (!"plate_number" %in% names(plate_id)) {
    # Create plate_number from row index
    plate_id$plate_number <- paste0("plate_", 1:nrow(plate_id))
    cat("  ✓ Created plate_number from index\n")
  }

  # Handle plate_filename
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
    # Create plateid
    header_df_raw$plateid <- paste0("plate_", 1:nrow(header_df_raw))
    header_df_cols <- c(header_df_cols, "plateid")
  }

  if ("plate" %in% names(header_df_raw)) {
    header_df_cols <- c(header_df_cols, "plate")
  } else if ("plate_number" %in% names(header_df_raw)) {
    header_df_raw$plate <- header_df_raw$plate_number
    header_df_cols <- c(header_df_cols, "plate")
  } else {
    # Create plate from index
    header_df_raw$plate <- paste0("plate_", 1:nrow(header_df_raw))
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
  subject_id_dat <- plate_well_map[plate_well_map$specimen_type=="X", c("study_name","subject_id","Description")]
  
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
  
  subject_groups <- dplyr::distinct(subject_id_dat[ , c("study_name","subject_id","groupa", "groupb")])

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
  plate_well_map <- apply_default_values_to_plates_map(plate_well_map, description_status)

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
  # NEW: Add README sheet if defaults were applied due to blank/insufficient Description
  # ============================================================================
  if (!description_status$has_content || !description_status$has_sufficient_elements) {
    readme_data <- data.frame(
      Notes = c(
        "╔══════════════════════════════════════════════════════════════════════════════╗",
        "║  IMPORTANT: This layout template was generated with DEFAULT VALUES          ║",
        "╚══════════════════════════════════════════════════════════════════════════════╝",
        "",
        "The Description field in your plate data was blank or had insufficient elements.",
        "",
        "Default values applied:",
        "  • Subject ID: '1'",
        "  • Sample Dilution Factor: 1",
        "  • Timeperiod: 'T0'",
        "  • Groups (groupa, groupb): 'Unknown'",
        "",
        "═══════════════════════════════════════════════════════════════════════════════",
        "REQUIRED ACTIONS BEFORE UPLOADING:",
        "═══════════════════════════════════════════════════════════════════════════════",
        "",
        "1. PLATES_MAP sheet:",
        "   - Update 'subject_id' column with actual patient/subject identifiers",
        "   - Update 'specimen_dilution_factor' with correct dilution values",
        "   - Update 'timepoint_tissue_abbreviation' with correct timepoint codes",
        "",
        "2. SUBJECT_GROUPS sheet:",
        "   - Update 'subject_id' column with actual patient/subject identifiers",
        "   - Update 'groupa' and 'groupb' columns with correct group assignments",
        "",
        "3. TIMEPOINT sheet:",
        "   - Update 'timepoint_tissue_abbreviation' with correct timepoint codes",
        "   - Fill in tissue_type, tissue_subtype, description, and time ranges",
        "",
        "═══════════════════════════════════════════════════════════════════════════════",
        "After updating, save this file and upload it as the layout template.",
        "═══════════════════════════════════════════════════════════════════════════════"
      ),
      stringsAsFactors = FALSE
    )
    
    addWorksheet(wb, "README_IMPORTANT")
    writeData(wb, "README_IMPORTANT", readme_data, startRow = 1, startCol = 1, colNames = FALSE)
    
    # Style the README sheet
    warning_style <- createStyle(
      fontColour = "#8B0000",
      textDecoration = "bold"
    )
    addStyle(wb, "README_IMPORTANT", style = warning_style, 
             rows = 1:3, cols = 1, gridExpand = TRUE)
    
    setColWidths(wb, "README_IMPORTANT", cols = 1, widths = 85)
    
    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║  Added README_IMPORTANT sheet with instructions          ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
  }

  # Save the workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("\n✓ Layout template saved to:", output_file, "\n")
}
