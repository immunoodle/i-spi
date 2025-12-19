

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

process_uploaded_files <- function(upload_df) {
  tmpdir <- file.path(tempdir(), "uploaded_batch")
  if (!dir.exists(tmpdir)) dir.create(tmpdir)

  for (i in 1:nrow(upload_df)) {
    file.copy(upload_df$datapath[i], file.path(tmpdir, upload_df$name[i]), overwrite = TRUE)
  }

  read_bind_xlsx(folder = tmpdir, x = "plate")
}


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


generate_layout_template <- function(all_plates, study_accession, experiment_accession, n_wells, header_list, output_file) {
  wb <- createWorkbook()
  bold_style <- createStyle(textDecoration = "bold")
  italic_style  <- createStyle(textDecoration = "italic")

  plate_id <- do.call(rbind,header_list)
  plate_id$study_name <- study_accession
  plate_id$experiment_name <- experiment_accession
  plate_id$number_of_wells <- n_wells
  if ("plate" %in% names(plate_id)) {
    names(plate_id)[names(plate_id) == "plate"] <- "plate_number"
  }
  if ("file_name" %in% names(plate_id)) {
    names(plate_id)[names(plate_id) == "file_name"] <- "plate_filename"
  }


  plate_id <- plate_id[, c("study_name", "experiment_name", "number_of_wells", "plate_number", "plateid", "plate_filename"),]

  # combine all plates together
  # all_plates <- read_bind_xlsx(folder = folder, x = "plate")
  antigen_names <- names(all_plates)[!(names(all_plates) %in% c( "source_file", "Well", "Type", "Description", "% Agg Beads", "Sampling Errors"))]

  antigen_df <- tibble(
    study_name = rep(study_accession, length(antigen_names)),
    experiment_name = rep(experiment_accession, length(antigen_names)),
    antigen_label_on_plate = antigen_names,
    antigen_abbreviation = NA_character_,
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
  well_list <- generate_well_list(n_wells)
  plate_well_map <- expand_grid(
    study_name = study_accession,
    plate_number = plate_numbers,
    well = well_list
  ) %>%
    mutate(specimen_type = NA_character_,
           specimen_source = NA_character_,
           specimen_dilution_factor	 = NA_real_,
           experiment_name = experiment_accession,
           feature = NA_character_,
           subject_id	 = NA_character_,
           biosample_id_barcode = NA_character_,
           timepoint_tissue_abbreviation = NA_character_)

  timepoint <- tibble(
    study_name = study_accession,
    timepoint_tissue_abbreviation	= NA_character_,
    tissue_type	= NA_character_,
    tissue_subtype	= NA_character_,
    description = NA_character_,
    min_time_since_day_0= NA_character_,
    max_time_since_day_0	= NA_character_,
    timepoint_unit= NA_character_
  )
  subject_groups  <- tibble(
    study_name = study_accession,
    subject_id = NA_character_,
    groupa = NA_character_,
    groupb = NA_character_
  )

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

        ex_antigen_list <- c("Vietnam342","IgG_total", "FHA (27)","FHA", 100000, "default", 1, 10, "Pertussis", "Filamentous hemagglutinin adhesin ", "Bordetella pertussis strain Tohama I",
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

  validation_range <- paste0("cell_valid!$A$2:$A$", nrow(cell_valid_table) + 1)

  # Find which column in antigen_list has l_asy_constraint_method
  constraint_col <- which(names(antigen_df) == "l_asy_constraint_method")

  # Apply data validation to the entire column (for all rows of antigen_df)
  dataValidation(
    wb,
    sheet = "antigen_list",
    cols = constraint_col,
    rows = 2:(nrow(antigen_df) + 1),  # skip header row
    type = "list",
    value = validation_range,
    allowBlank = TRUE
  )


  saveWorkbook(wb, file = output_file, overwrite = TRUE)

  #saveWorkbook(wb, file = paste(study_accession, experiment_accession, "layout_template.xlsx", sep = "_"), overwrite = TRUE)

  #write.xlsx(workbook, file = paste(study_accession, experiment_accession, "layout_template.xlsx", sep = "_"))
}

# layout_template <- generate_layout_template(folder = "/Users/f006yd8/Desktop/Gambia123/IgG_total",
#                                             study_accession = "Gambia123",
#                                             experiment_accession = "IgG_total",
#                                             n_wells = 96,
#                                             header_list = header_list)

validate_batch_plate_metadata <- function(plate_metadata, plate_id_data) {
  message_list <- c()

  check_uploaded_file_in_layout <- plate_metadata$file_name %in% plate_id_data$plate_filename
  if (!all(check_uploaded_file_in_layout)) {
    message_list <- c("Some uploaded plates are misssing in the layout file.")
  }
  # validate the required columns
  required_cols <- c("file_name", "rp1_pmt_volts", "rp1_target", "acquisition_date")
  missing_cols <- setdiff(required_cols, names(plate_metadata))

  if (length(missing_cols) > 0) {
    message_list <- c(
      message_list,
      paste("The following required plate metadata columns are missing so further parsing cannot be conducted:",
            paste(missing_cols, collapse = ", "))
    )
    # If critical metadata is missing, return early
    return(list(
      is_valid = FALSE,
      messages = message_list
    ))
  }

  # plate_metadata <<- plate_metadata

  # check to see if all files passes file Path
  pass_file_path <- all(looks_like_file_path(plate_metadata$file_name))
  if (!pass_file_path) {
    message_list <- c(message_list, "Ensure that alll file paths have foward or backward slashes based on Mac or Windows")
  }

  pass_rp1_pmt_volts <- all(check_rp1_numeric(plate_metadata$rp1_pmt_volts))
  is_numeric <- check_rp1_numeric(plate_metadata$rp1_pmt_volts)
  if (!pass_rp1_pmt_volts) {
    bad_rp1_pmt_volts <- plate_metadata[!is_numeric, c("plateid", "rp1_pmt_volts")]
    labeled_vals <- paste(bad_rp1_pmt_volts$plateid, bad_rp1_pmt_volts$rp1_pmt_volts, sep = ":")

    message_list <- c(message_list, paste(
      "Ensure that all files have an RP1 PMT (Volts) field that is numeric and if it is a decimal only one period is present. Values by plateid:",
      paste(labeled_vals, collapse = ", "))
    )
  }
  pass_rp1_target <- all(check_rp1_numeric(plate_metadata$rp1_target))
  is_target_numeric <- check_rp1_numeric(plate_metadata$rp1_target)
  if (!pass_rp1_target) {
    invalid_rp1_target <- plate_metadata[!is_target_numeric, c("plateid", "rp1_target")]
    labeled_bad_rp1_target <- paste(invalid_rp1_target$plateid, invalid_rp1_target$rp1_target, sep = ":")
    message_list <- c(message_list, paste("Ensure that the RP1 Target is numeric and if it is a decimal only one period is present. Values by plateid:",
                                          paste(labeled_bad_rp1_target,collapse = ", ")))
  }

  pass_time_format <- all(check_time_format(capitalize_am_pm(plate_metadata$acquisition_date)))
  is_time_format <- check_time_format(capitalize_am_pm(plate_metadata$acquisition_date))
  if (!pass_time_format) {
    invalid_time_format <- plate_metadata[!is_time_format, c("plateid", "aquisition_date")]
    labeled_invalid_time_format <- paste(invalid_time_format$plateid, invalid_time_format$aquisition_date)
    message_list <- c(message_list, paste("Ensure the acquisition date is in the following date time format: DD-MMM-YYYY, HH:MM AM/PM Example: 01-Oct-2025, 12:12 PM  |Current Value by plateid:",
                                          paste(labeled_invalid_time_format, collapse = ", ")))
  }
  is_valid <- length(message_list) == 0

  if (is_valid)  {
    return(list(
      is_valid = is_valid,
      messages = message_list,
      updated_plate_data = plate_data
    ))
  } else {
    return(list(
      is_valid = is_valid,
      messages = message_list
    ))
  }
}

validate_batch_bead_array_data <- function(combined_plate_data, antigen_list, blank_keyword) {
  cat("in validate_batch_bead_array")
  # combined_plate_data <<- combined_plate_data
  # antigen_list_in <<- antigen_list
  # blank_keyword <<- blank_keyword

  message_list <- c()
  unique_plates <- unique(combined_plate_data$source_file)
  # obtain antigens from the layout file that are labeled on the plate
  valid_antigens <- unique(antigen_list$antigen_label_on_plate)
  check_antigens <- all(valid_antigens %in% names(combined_plate_data))
  if (!check_antigens) {
    missing_antigens <- valid_antigens[!valid_antigens %in% names(combined_plate_data)]
    message_list <- c(message_list,  paste(
      "The following antigens are missing in the layout/data file:",
      paste(missing_antigens, collapse = ", ")
    ))
  }

  cat("after antigens")

  pass_agg_bead_check <- check_batch_agg_bead_column(combined_plate_data)
  if (!pass_agg_bead_check$result) {
    message_list <- c(message_list, pass_agg_bead_check$message)
  } else {
    for (plate in unique_plates) {
      pass_bead_count_check <- check_bead_count(combined_plate_data[combined_plate_data$source_file == plate,])
      if (!pass_bead_count_check[[1]]) {
        message_list <- c(message_list, paste(plate, pass_bead_count_check$message, sep = ": "))
      }
    }
    cat("after bead count check")
  }

  # examine blanks in type column
  for (plate in unique_plates) {
    plate_data <- combined_plate_data[combined_plate_data$source_file == plate,]
    procceed_to_blank_check <- check_blank_in_sample_boolean(df = plate_data)
    cat("blank_check")
    if (!procceed_to_blank_check) {
      plate_data <- check_blank_in_sample(plate_data, blank_keyword = blank_keyword)
    }
    pass_blank_description <- check_blank_description_batch(plate_data)

    if (!pass_blank_description[[1]]) {
      message_list <- c(
        message_list,
        paste("Plate:", plate, "-", pass_blank_description[[2]])
      )
    }

    # 3. Write updated rows back into the combined df
    combined_plate_data[combined_plate_data$source_file == plate, ] <- plate_data

  }

  is_valid <- length(message_list) == 0

  cat("after is valid bead array")
  if (is_valid)  {
    return(list(
      is_valid = is_valid,
      messages = message_list
    ))
  } else {
    return(list(
      is_valid = is_valid,
      messages = message_list
    ))
  }

}

## this function merges layout info and raw file info for uploading samples
prepare_batch_bead_assay_samples <- function(sample_plate_map, combined_plate_data, batch_metadata, antigen_list, subject_map) {
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
  sample_long2 <- merge(sample_long, antigen_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
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

  return(samples_to_upload)
}


prepare_batch_bead_assay_standards <- function(standard_plate_map, combined_plate_data, antigen_list, batch_metadata) {
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

  standard_long2 <- merge(standard_long, antigen_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
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

prepare_batch_bead_assay_blanks <- function(blanks_plate_map, combined_plate_data, antigen_list, batch_metadata) {
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

  blanks_long2 <- merge(blanks_long, antigen_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
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

prepare_batch_bead_assay_controls <- function(controls_plate_map, combined_plate_data, antigen_list, batch_metadata) {
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
  controls_long2 <- merge(controls_long, antigen_list[, c("study_name", "experiment_name", "antigen_label_on_plate", "antigen_abbreviation")],
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


prepare_batch_antigen_family <- function(antigen_list) {
  antigen_list_df <- antigen_list[, c("study_name", "experiment_name", "antigen_abbreviation", "antigen_family", "standard_curve_max_concentration", "antigen_name", "virus_bacterial_strain", "antigen_source",
                                      "catalog_number", "l_asy_min_constraint", "l_asy_max_constraint", "l_asy_constraint_method")]
  names(antigen_list_df)[names(antigen_list_df) == "study_name"] <- "study_accession"
  names(antigen_list_df)[names(antigen_list_df) == "experiment_name"] <- "experiment_accession"
  names(antigen_list_df)[names(antigen_list_df) == "standard_curve_max_concentration"] <- "standard_curve_concentration"
  names(antigen_list_df)[names(antigen_list_df) == "antigen_abbreviation"] <- "antigen"

  return(antigen_list_df)
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

