
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


generate_layout_template <- function(folder, study_accession, experiment_accession, n_wells, header_list, output_file) {
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
  all_plates <- read_bind_xlsx(folder = folder, x = "plate")
  antigen_names <- names(all_plates)[!(names(all_plates) %in% c( "source_file", "Well", "Type", "Description", "% Agg Beads", "Sampling Errors"))]

  antigen_df <- tibble(
    study_name = rep(study_accession, length(antigen_names)),
    experiment_name = rep(experiment_accession, length(antigen_names)),
    antigen_label_on_plate = antigen_names,
    antigen_abbreviation = NA_character_,
    standard_curve_max_concentration = 100000,
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


  workbook <- list(
    plate_id = plate_id,
    subject_groups = subject_groups,
    timepoint = timepoint,
    antigen_list = antigen_df,
    plates_map = plate_well_map
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
        ex_antigen_list <- c("Vietnam342","IgG_total", "FHA (27)","FHA", 100000, "Pertussis", "Filamentous hemagglutinin adhesin ", "Bordetella pertussis strain Tohama I",
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

      }else {
      writeData(wb, nm, workbook[[nm]])
      addStyle(wb, nm, style = bold_style, rows = 1, cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
    }

    # writeData(wb, nm, workbook[[nm]])
    #
    # # Bold a specific row, for example row 1
    # addStyle(wb, sheet = nm, style = bold_style, rows = 1, cols = 1:ncol(workbook[[nm]]), gridExpand = TRUE)
  }

  saveWorkbook(wb, file = output_file, overwrite = TRUE)

  #saveWorkbook(wb, file = paste(study_accession, experiment_accession, "layout_template.xlsx", sep = "_"), overwrite = TRUE)

  #write.xlsx(workbook, file = paste(study_accession, experiment_accession, "layout_template.xlsx", sep = "_"))
}

# layout_template <- generate_layout_template(folder = "/Users/f006yd8/Desktop/Gambia123/IgG_total",
#                                             study_accession = "Gambia123",
#                                             experiment_accession = "IgG_total",
#                                             n_wells = 96,
#                                             header_list = header_list)

