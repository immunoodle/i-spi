# ==============================================================================
# ELISA Layout Template Generation - Diagnostic Script
# ==============================================================================
# Run this in the Shiny app console or source it to isolate errors.
# It tests the ELISA parsing and layout template generation in isolation.
#
# Usage:
#   1. Place this file alongside your app.R
#   2. In RStudio console (while app is NOT running):
#        source("global.R")
#        source("elisa_diagnostic.R")
#        run_elisa_diagnostic("path/to/FHA_Fc2a_ELISA_RAW_20260213.xlsx")
#
#   OR, inside a running Shiny session (e.g., from a debug button):
#        source("elisa_diagnostic.R", local = TRUE)
#        run_elisa_diagnostic("path/to/FHA_Fc2a_ELISA_RAW_20260213.xlsx",
#                             project_id = userWorkSpaceID(),
#                             study = input$readxMap_study_accession,
#                             experiment = input$readxMap_experiment_accession_import)
# ==============================================================================

run_elisa_diagnostic <- function(file_path,
                                 project_id = "test_project",
                                 study = "test_study",
                                 experiment = "test_experiment",
                                 n_wells = 96,
                                 delimiter = "_") {

  cat("\n")
  cat("================================================================\n")
  cat("  ELISA DIAGNOSTIC - START\n")
  cat("================================================================\n")
  cat("  File:", file_path, "\n")
  cat("  project_id:", deparse(project_id), " class:", class(project_id),
      " length:", length(project_id), "\n")
  cat("  study:", deparse(study), " class:", class(study), "\n")
  cat("  experiment:", deparse(experiment), " class:", class(experiment), "\n")
  cat("  n_wells:", deparse(n_wells), " class:", class(n_wells), "\n\n")

  # ---- Step 1: Check file exists and has required sheets ----
  cat("--- STEP 1: File validation ---\n")
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  sheets <- readxl::excel_sheets(file_path)
  cat("  Sheets found:", paste(sheets, collapse = ", "), "\n")

  has_results <- any(grepl("^results$", sheets, ignore.case = TRUE))
  has_plate_map <- any(grepl("^plate_map$", sheets, ignore.case = TRUE))
  cat("  Has results sheet:", has_results, "\n")
  cat("  Has plate_map sheet:", has_plate_map, "\n")
  if (!has_results || !has_plate_map) stop("Missing required sheet(s)")

  # ---- Step 2: Parse results sheet ----
  cat("\n--- STEP 2: Parse results sheet ---\n")
  parsed_results <- tryCatch({
    parse_elisa_results_sheet(file_path, "results")
  }, error = function(e) {
    cat("  ERROR in parse_elisa_results_sheet:", conditionMessage(e), "\n")
    cat("  Traceback:\n")
    traceback()
    stop(e)
  })
  cat("  OK - parsed", length(parsed_results$parsed_plates), "plates\n")
  cat("  Plates:", paste(names(parsed_results$parsed_plates), collapse = ", "), "\n")
  cat("  Autoloading range:", parsed_results$autoloading_range, "\n")

  # ---- Step 3: Parse plate_map sheet ----
  cat("\n--- STEP 3: Parse plate_map sheet ---\n")
  plate_map <- tryCatch({
    parse_elisa_plate_map(file_path, "plate_map")
  }, error = function(e) {
    cat("  ERROR in parse_elisa_plate_map:", conditionMessage(e), "\n")
    stop(e)
  })
  cat("  OK -", nrow(plate_map), "rows\n")
  cat("  Columns:", paste(names(plate_map), collapse = ", "), "\n")
  cat("  Has Description:", "Description" %in% names(plate_map), "\n")
  cat("  Has ID:", "ID" %in% names(plate_map), "\n")
  cat("  Has Specimen_id:", "Specimen_id" %in% names(plate_map), "\n")
  cat("  Plate values:", paste(sort(unique(plate_map$Plate)), collapse = ", "), "\n")
  cat("  SType values:", paste(sort(unique(plate_map$SType)), collapse = ", "), "\n")

  # ---- Step 4: Combine data ----
  cat("\n--- STEP 4: Combine ELISA data ---\n")
  combined <- tryCatch({
    combine_elisa_data(parsed_results, plate_map, basename(file_path))
  }, error = function(e) {
    cat("  ERROR in combine_elisa_data:", conditionMessage(e), "\n")
    stop(e)
  })
  cat("  OK -", nrow(combined), "rows\n")
  cat("  Class:", paste(class(combined), collapse = ", "), "\n")
  cat("  Columns:", paste(names(combined), collapse = ", "), "\n")
  cat("  Unique plateids:", paste(unique(combined$plateid), collapse = ", "), "\n")

  # ---- Step 4a: Test description status (BUG CHECK) ----
  cat("\n--- STEP 4a: Description status diagnostic ---\n")
  if ("Description" %in% names(plate_map)) {
    all_descs <- plate_map$Description[!is.na(plate_map$Description)]
    cat("  Total descriptions:", length(all_descs), "\n")
    
    all_n <- sapply(all_descs, function(d) length(strsplit(d, delimiter, fixed = TRUE)[[1]]))
    cat("  Min elements (ALL rows):", min(all_n), "\n")
    cat("  Max elements (ALL rows):", max(all_n), "\n")
    
    x_rows <- plate_map[!is.na(plate_map$SType) & substr(plate_map$SType, 1, 1) == "X", ]
    if (nrow(x_rows) > 0 && "Description" %in% names(x_rows)) {
      x_descs <- x_rows$Description[!is.na(x_rows$Description) & trimws(x_rows$Description) != ""]
      x_n <- sapply(x_descs, function(d) length(strsplit(d, delimiter, fixed = TRUE)[[1]]))
      cat("  Min elements (X-type only):", min(x_n), "\n")
      cat("  Max elements (X-type only):", max(x_n), "\n")
    }
    
    bsc_rows <- plate_map[!is.na(plate_map$SType) & substr(plate_map$SType, 1, 1) %in% c("B","S","C"), ]
    if (nrow(bsc_rows) > 0 && "Description" %in% names(bsc_rows)) {
      bsc_descs <- bsc_rows$Description[!is.na(bsc_rows$Description) & trimws(bsc_rows$Description) != ""]
      bsc_n <- sapply(bsc_descs, function(d) length(strsplit(d, delimiter, fixed = TRUE)[[1]]))
      cat("  Min elements (B/S/C-type):", min(bsc_n), "\n")
      cat("  Max elements (B/S/C-type):", max(bsc_n), "\n")
      cat("  *** If B/S/C min < 3, that was causing UI controls to disappear ***\n")
    }
  }

  # ---- Step 4b: Test aggregate sorting (BUG CHECK) ----
  cat("\n--- STEP 4b: Aggregate plateid sorting diagnostic ---\n")
  combined_df_test <- as.data.frame(combined, stringsAsFactors = FALSE)
  agg_test <- aggregate(wavelength ~ plateid + source_file, data = combined_df_test,
                        FUN = function(x) paste(sort(unique(x)), collapse = "|"))
  cat("  Aggregate output order: ", paste(agg_test$plateid, collapse = ", "), "\n")
  cat("  Header plateid order:   ", paste(unique(combined_df_test$plateid), collapse = ", "), "\n")
  if (!identical(agg_test$plateid, unique(combined_df_test$plateid))) {
    cat("  *** MISMATCH! Aggregate sorts lexicographically ***\n")
    cat("  *** This was causing $<-.data.frame error when columns not pre-initialized ***\n")
  }

  # ---- Step 5: Build header_list ----
  cat("\n--- STEP 5: Build header list ---\n")
  header <- tryCatch({
    n_unique_plates <- length(unique(combined$plateid))
    cat("  Number of unique plates:", n_unique_plates, "\n")
    cat("  unique(combined$plateid) class:", class(unique(combined$plateid)), "\n")
    cat("  unique(combined$plateid) length:", length(unique(combined$plateid)), "\n")

    # Test the original approach
    cat("  Testing data.frame construction with recycling...\n")
    test_df <- data.frame(
      source_file = basename(file_path),
      file_name = basename(file_path),
      plateid = unique(combined$plateid),
      acquisition_date = parsed_results$file_metadata$acquisition_date %||% NA_character_,
      reader_serial_number = NA_character_,
      rp1_pmt_volts = NA_character_,
      rp1_target = NA_character_,
      autoloading_range = parsed_results$autoloading_range %||% NA_character_,
      absorbance_id = parsed_results$file_metadata$absorbance_id %||% NA_character_,
      original_filename = parsed_results$file_metadata$filename %||% basename(file_path),
      stringsAsFactors = FALSE
    )
    cat("  Header data.frame OK:", nrow(test_df), "rows x", ncol(test_df), "cols\n")
    test_df
  }, error = function(e) {
    cat("  ERROR building header:", conditionMessage(e), "\n")
    cat("  Attempting element-by-element diagnosis...\n")
    cat("    basename(file_path):", deparse(basename(file_path)), "length:", length(basename(file_path)), "\n")
    cat("    unique(combined$plateid):", deparse(unique(combined$plateid)), "length:", length(unique(combined$plateid)), "\n")
    cat("    acquisition_date:", deparse(parsed_results$file_metadata$acquisition_date), "\n")
    cat("    autoloading_range:", deparse(parsed_results$autoloading_range), "\n")
    cat("    absorbance_id:", deparse(parsed_results$file_metadata$absorbance_id), "\n")
    cat("    original_filename:", deparse(parsed_results$file_metadata$filename), "\n")
    stop(e)
  })

  header_list <- list()
  header_list[[basename(file_path)]] <- header

  # ---- Step 6: Test build_elisa_plate_id ----
  cat("\n--- STEP 6: Test build_elisa_plate_id ---\n")

  # First, manually inspect the inputs
  combined_df <- as.data.frame(combined, stringsAsFactors = FALSE)
  cat("  combined_df class:", paste(class(combined_df), collapse = ", "), "\n")

  unique_plates <- unique(combined_df[, c("plateid", "source_file", "plate"), drop = FALSE])
  cat("  unique_plates:", nrow(unique_plates), "rows\n")
  cat("  unique_plates$plateid:", paste(unique_plates$plateid, collapse = ", "), "\n")
  cat("  unique_plates$plateid class:", class(unique_plates$plateid), "\n")
  cat("  unique_plates$plateid length:", length(unique_plates$plateid), "\n")

  cat("  project_id:", deparse(project_id), " length:", length(project_id), " class:", class(project_id), "\n")
  cat("  study:", deparse(study), " length:", length(study), "\n")
  cat("  experiment:", deparse(experiment), " length:", length(experiment), "\n")
  cat("  n_wells:", deparse(n_wells), " length:", length(n_wells), "\n")

  # Test the data.frame construction directly
  cat("\n  Testing plate_id data.frame construction...\n")
  tryCatch({
    test_plate_id <- data.frame(
      project_id = project_id,
      study_name = study,
      experiment_name = experiment,
      number_of_wells = n_wells,
      plate_number = unique_plates$plateid,
      plateid = unique_plates$plateid,
      plate_id = paste0(unique_plates$source_file, "_", unique_plates$plateid),
      plate_filename = unique_plates$source_file,
      stringsAsFactors = FALSE
    )
    cat("  OK:", nrow(test_plate_id), "rows x", ncol(test_plate_id), "cols\n")
    print(str(test_plate_id))
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    cat("  --- Diagnosing individual argument lengths ---\n")
    cat("    project_id length:", length(project_id), "class:", class(project_id), "value:", deparse(project_id), "\n")
    cat("    study length:", length(study), "class:", class(study), "\n")
    cat("    experiment length:", length(experiment), "class:", class(experiment), "\n")
    cat("    n_wells length:", length(n_wells), "class:", class(n_wells), "\n")
    cat("    plate_number length:", length(unique_plates$plateid), "\n")
    cat("    plate_id length:", length(paste0(unique_plates$source_file, "_", unique_plates$plateid)), "\n")
    cat("    plate_filename length:", length(unique_plates$source_file), "\n")
  })

  # ---- Step 7: Test nsd merge collision (BUG CHECK) ----
  cat("\n--- STEP 7: Test nominal_sample_dilution merge ---\n")
  tryCatch({
    # Simulate what generate_elisa_layout_template does
    test_pm <- data.frame(
      plateid = c("plate_6", "plate_7"),
      project_id = project_id,
      study_name = study,
      experiment_name = experiment,
      nominal_sample_dilution = "1",
      well = c("A1", "A1"),
      specimen_type = c("X", "S"),
      specimen_dilution_factor = c(1, 100),
      stringsAsFactors = FALSE
    )
    test_nsd <- data.frame(
      project_id = project_id,
      study_name = study,
      experiment_name = experiment,
      plateid = c("plate_6", "plate_7"),
      nominal_sample_dilution = c("1", "100"),
      stringsAsFactors = FALSE
    )
    cat("  test_pm cols:", paste(names(test_pm), collapse=", "), "\n")
    cat("  test_nsd cols:", paste(names(test_nsd), collapse=", "), "\n")
    
    # Merge WITHOUT dropping the duplicate column (reproduces the bug)
    test_merged_bad <- merge(test_pm, test_nsd,
                             by = c("project_id", "study_name", "experiment_name", "plateid"),
                             all.x = TRUE)
    cat("  After merge (without drop): cols =", paste(names(test_merged_bad), collapse=", "), "\n")
    cat("  Has 'nominal_sample_dilution':", "nominal_sample_dilution" %in% names(test_merged_bad), "\n")
    cat("  Has '.x' suffix:", "nominal_sample_dilution.x" %in% names(test_merged_bad), "\n")
    cat("  Has '.y' suffix:", "nominal_sample_dilution.y" %in% names(test_merged_bad), "\n")
    if ("nominal_sample_dilution.x" %in% names(test_merged_bad)) {
      cat("  *** This confirms the merge collision bug! ***\n")
    }
    
    # Merge WITH dropping first (the fix)
    test_pm2 <- test_pm
    test_pm2$nominal_sample_dilution <- NULL
    test_merged_good <- merge(test_pm2, test_nsd,
                              by = c("project_id", "study_name", "experiment_name", "plateid"),
                              all.x = TRUE)
    cat("  After merge (with drop): cols =", paste(names(test_merged_good), collapse=", "), "\n")
    cat("  Has 'nominal_sample_dilution':", "nominal_sample_dilution" %in% names(test_merged_good), "\n")
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
  })

  # ---- Step 8: Test full layout template generation ----
  cat("\n--- STEP 8: Test generate_elisa_layout_template ---\n")
  output_file <- tempfile(fileext = ".xlsx")
  tryCatch({
    generate_elisa_layout_template(
      combined_data = combined_df,
      plate_map = plate_map,
      header_list = header_list,
      study_accession = study,
      experiment_accession = experiment,
      n_wells = n_wells,
      output_file = output_file,
      project_id = project_id,
      description_status = list(
        has_content = TRUE,
        has_sufficient_elements = TRUE,
        min_elements_found = 3,
        required_elements = 3,
        checked = FALSE
      ),
      delimiter = delimiter,
      element_order = c("PatientID", "TimePeriod", "DilutionFactor"),
      bcs_element_order = c("Source", "DilutionFactor")
    )
    cat("  OK - Layout template saved to:", output_file, "\n")

    # Verify the output
    out_sheets <- readxl::excel_sheets(output_file)
    cat("  Output sheets:", paste(out_sheets, collapse = ", "), "\n")
    for (s in out_sheets) {
      d <- readxl::read_excel(output_file, sheet = s)
      cat("    ", s, ":", nrow(d), "rows x", ncol(d), "cols\n")
    }
  }, error = function(e) {
    cat("  ERROR in generate_elisa_layout_template:", conditionMessage(e), "\n")
    cat("  Call stack:\n")
    traceback()
  })

  cat("\n================================================================\n")
  cat("  ELISA DIAGNOSTIC - COMPLETE\n")
  cat("================================================================\n")

  invisible(list(
    combined = combined,
    plate_map = plate_map,
    header_list = header_list,
    parsed_results = parsed_results
  ))
}
