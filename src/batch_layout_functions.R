

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

# Clear any leftover temp directories from previous sessions
onSessionStart <- function() {
  tmpdir <- file.path(tempdir(), "uploaded_batch")
  if (dir.exists(tmpdir)) {
    unlink(tmpdir, recursive = TRUE)
    cat("Cleared temp directory from previous session\n")
  }
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

generate_layout_template <- function(all_plates, study_accession, experiment_accession, n_wells, header_list, output_file) {
  wb <- createWorkbook()
  bold_style <- createStyle(textDecoration = "bold")
  italic_style  <- createStyle(textDecoration = "italic")

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
    l_asy_constraint_method = "unconstrained",
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
  input_feature_value <- input$feature_value
  description_delimiter <- input$description_delimiter
  XElementOrder <- input$XElementOrder
  print(XElementOrder)
  xpatientid <- get_element_position("PatientID", XElementOrder)
  xtimeperiod <- get_element_position("TimePeriod", XElementOrder)
  xdilutionfactor <- get_element_position("DilutionFactor", XElementOrder)
  xgroupa <- get_element_position("SampleGroupA", XElementOrder)
  xgroupb <- get_element_position("SampleGroupB", XElementOrder)

  BCSElementOrder <- input$BCSElementOrder
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


  plate_well_map$subject_id <- case_when(
    is.na(plate_well_map$Type) ~ "",
    substr(plate_well_map$Type,1,1) == "X" ~ str_split_i(plate_well_map$Description,description_delimiter,xpatientid),
    substr(plate_well_map$Type,1,1) == "C" ~ substr(plate_well_map$Type,2,length(plate_well_map$Type)-1),
    substr(plate_well_map$Type,1,1) == "B" ~ "1",
    substr(plate_well_map$Type,1,1) == "S" ~ substr(plate_well_map$Type,2,length(plate_well_map$Type)-1),
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

  subject_id_dat <- plate_well_map[plate_well_map$specimen_type=="X", c("study_name","subject_id","Description")]
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
    substr(plate_well_map$Type,1,1) == "X" ~ substr(plate_well_map$Type,2,length(plate_well_map$Type)-1),
    substr(plate_well_map$Type,1,1) == "C" ~ substr(plate_well_map$Type,2,length(plate_well_map$Type)-1),
    substr(plate_well_map$Type,1,1) == "B" ~ "",
    substr(plate_well_map$Type,1,1) == "S" ~ substr(plate_well_map$Type,2,length(plate_well_map$Type)-1),
    TRUE ~ ""
  )

  plate_well_map$feature <- input_feature_value

  plate_well_map <- plate_well_map[ , c("study_name",	"plate_number",	"well",	"specimen_type",	"specimen_source",
                                        "specimen_dilution_factor",	"experiment_name",	"feature",	"subject_id",	"biosample_id_barcode",	"timepoint_tissue_abbreviation")]

  timepoints <- unique(plate_well_map[plate_well_map$specimen_type=="X", ]$timepoint_tissue_abbreviation)
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
        "unconstrained",
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
      # # Example label (Row 1)
      # writeData(wb, nm, "Example:", startRow = 1, startCol = 1)

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

    # writeData(wb, nm, workbook[[nm]])
    #
    # # Bold a specific row, for example row 1
    # addStyle(wb, sheet = nm, style = bold_style, rows = 1, cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
  }

  # Save the workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
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

# Join sample dilutions for the plate to the header
construct_batch_upload_metadata <- function(plates_map, plate_metadata_list, currentuser, workspace_id) {

  cat("\n=== CONSTRUCT BATCH UPLOAD METADATA ===\n")
  cat("plates_map columns:", paste(names(plates_map), collapse=", "), "\n")
  cat("Number of plate metadata items:", length(plate_metadata_list), "\n")
  cat("Plate metadata names:", paste(names(plate_metadata_list), collapse=", "), "\n")

  # Check if plate_metadata_list items have 'plate' column
  first_item <- plate_metadata_list[[1]]
  cat("First metadata item columns:", paste(names(first_item), collapse=", "), "\n")

  # Get unique sample dilution factors by plate_number
  sample_dilutions_by_plate <- unique(
    plates_map[plates_map$specimen_type == "X",
               c("plate_number", "specimen_type", "specimen_dilution_factor")]
  )

  names(sample_dilutions_by_plate)[names(sample_dilutions_by_plate) == "specimen_dilution_factor"] <- "sample_dilution_factor"

  cat("Sample dilutions by plate:\n")
  print(sample_dilutions_by_plate)

  # Get experiment by plate
  experiment_by_plate <- unique(
    plates_map[, c("plate_number", "experiment_name")]
  )

  cat("Experiment by plate:\n")
  print(experiment_by_plate)
  cat("\n")

  # Add dilution factor to each header df in the list
  plate_metadata_list_updated <- lapply(names(plate_metadata_list), function(plate_name) {
    df <- plate_metadata_list[[plate_name]]

    # Get plate number from the df
    if ("plate" %in% names(df) && !is.na(df$plate[1])) {
      plate_num <- df$plate[1]
    } else {
      # Extract from name or create from index
      plate_num <- paste0("plate_", which(names(plate_metadata_list) == plate_name))
      df$plate <- plate_num
      cat("  Created plate number", plate_num, "for", plate_name, "\n")
    }

    cat("  Processing:", plate_name, "| plate_num:", plate_num, "\n")

    # Match with dilution table
    match_row <- sample_dilutions_by_plate[
      sample_dilutions_by_plate$plate_number == plate_num, , drop = FALSE
    ]

    # Add dilution column if match found
    if (nrow(match_row) == 1) {
      df$sample_dilution_factor <- match_row$sample_dilution_factor
      cat("    ✓ Added dilution factor:", match_row$sample_dilution_factor, "\n")
    } else if (nrow(match_row) > 1) {
      df$sample_dilution_factor <- match_row$sample_dilution_factor[1]
      cat("    ⚠ Multiple dilution factors, using first:", match_row$sample_dilution_factor[1], "\n")
    } else {
      df$sample_dilution_factor <- NA_real_
      cat("    ⚠ No dilution factor found for plate_number:", plate_num, "\n")
    }

    # Add experiment_name
    exp_row <- experiment_by_plate[
      experiment_by_plate$plate_number == plate_num, , drop = FALSE
    ]

    if (nrow(exp_row) > 0) {
      df$experiment_name <- exp_row$experiment_name[1]
      cat("    ✓ Added experiment:", exp_row$experiment_name[1], "\n")
    } else {
      df$experiment_name <- NA_character_
      cat("    ⚠ No experiment found for plate_number:", plate_num, "\n")
    }

    # Add metadata
    df$study_accession <- unique(plates_map$study_name)[1]
    df$experiment_accession <- unique(plates_map$feature)[1]
    df$workspace_id <- workspace_id
    df$auth0_user <- currentuser

    return(df)
  })

  # Restore names
  names(plate_metadata_list_updated) <- names(plate_metadata_list)

  # Add source file for later join
  for (nm in names(plate_metadata_list_updated)) {
    plate_metadata_list_updated[[nm]]$source_file <- nm
  }

  cat("✓ Batch metadata construction complete\n")
  cat("=====================================\n\n")

  return(plate_metadata_list_updated)
}

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
  metadata_batch <- metadata_batch[,c("study_accession", "experiment_name", "plate_id", "file_name", "acquisition_date",
                                      "reader_serial_number", "rp1_pmt_volts", "rp1_target", "auth0_user", "workspace_id", "plateid", "plate", "sample_dilution_factor",
                                      "n_wells")]
  names(metadata_batch)[names(metadata_batch) == "experiment_name"] <- "experiment_accession"
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

