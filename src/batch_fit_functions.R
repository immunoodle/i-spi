fetch_db_header_experiments <- function(study_accession, conn, verbose = TRUE) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, sample_dilution_factor, plate_id,
  assay_response_variable, assay_independent_variable
  FROM madi_results.xmap_header
WHERE study_accession = '{study_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

build_antigen_list <- function(exp_list, loaded_data_list, study_accession, verbose = TRUE) {
  antigen_list <- list()
  antigen_list_ids <- c()
  for (experiment_accession in exp_list) {
    loaded_data <- loaded_data_list[[experiment_accession]]
    standards <- loaded_data$standards
    valid_combos <- unique(standards[, c("experiment_accession","plate", "source", "sample_dilution_factor")])
    print(valid_combos)
    for (i in seq_len(nrow(valid_combos))) {
      current_combo <- valid_combos[i, ]
      exp_standards <- standards[standards$experiment_accession == current_combo$experiment_accession &
                                   standards$plate == current_combo$plate & standards$source == current_combo$source &
                                   standards$sample_dilution_factor == current_combo$sample_dilution_factor, ]

      antigens <- unique(exp_standards$antigen)
      antigens <- antigens[!is.na(antigens) & antigens != ""]

      if (length(antigens) > 0) {
        id <- paste0(experiment_accession, "_",
                     current_combo$plate, "_",
                     current_combo$sample_dilution_factor, "_",
                     current_combo$source)
        antigen_list[[id]] <- list(
          antigens = unique(exp_standards$antigen),
          study_accession = study_accession,
          experiment_accession = experiment_accession,
          plate = current_combo$plate,
          sample_dilution_factor = current_combo$sample_dilution_factor,
          source = current_combo$source
        )

        antigen_list_ids <- c(antigen_list_ids, id)
      }
    }
  }

  return(list(antigen_list = antigen_list,
              antigen_list_ids = antigen_list_ids))

}

build_antigen_plate_list <- function(antigen_list_result, loaded_data_list, verbose = TRUE) {
  antigen_list <- antigen_list_result$antigen_list
  antigen_list_ids <- antigen_list_result$antigen_list_ids

  antigen_plate_list <- list()
  antigen_plate_list_ids <- c()
  for (antigen_id in antigen_list_ids) {
    info <- antigen_list[[antigen_id]]
    for (antigen in info$antigens) {
      print(antigen)
      antigen_constraints <- loaded_data_list[[info$experiment_accession]]$antigen_constraints[
        loaded_data_list[[info$experiment_accession]]$antigen_constraints$antigen == antigen,
      ]

      current_unique_id <- paste0(
        info$study_accession, "_",
        info$experiment_accession, "_",
        info$plate, "_",
        info$source, "_",
        info$sample_dilution_factor, "_",
        antigen
      )

      antigen_plate_list[[current_unique_id]] <- select_antigen_plate(
        loaded_data = loaded_data_list[[info$experiment_accession]],
        study_accession = info$study_accession,
        experiment_accession = info$experiment_accession,
        source = info$source,
        antigen = antigen,
        plate = info$plate,
        sample_dilution_factor = info$sample_dilution_factor,
        antigen_constraints = antigen_constraints
      )

      antigen_plate_list_ids <- c(antigen_plate_list_ids, current_unique_id)
    }
  }

  return(list(antigen_plate_list = antigen_plate_list,
              antigen_plate_list_ids = antigen_plate_list_ids)
  )

}

prep_plate_data_batch <- function(antigen_plate_list_res, study_params, verbose = TRUE) {
  antigen_plate_list <- antigen_plate_list_res$antigen_plate_list
  antigen_plate_list_ids <-  antigen_plate_list_res$antigen_plate_list_ids

  prepped_data_list <- list()
  formula_list <- list()

  antigen_plate_name_list <- list()

  for (id in antigen_plate_list_ids) {
    antigen_plate_current <- antigen_plate_list[[id]]
    if (verbose) print(id)
    prepped_data_list[[id]] <- preprocess_robust_curves(data = antigen_plate_current$plate_standard,
                                                        antigen_settings = antigen_plate_current$antigen_settings,
                                                        response_variable = unique(antigen_plate_current$plate_standard$assay_response_variable),
                                                        independent_variable = unique(antigen_plate_current$plate_standard$assay_independent_variable),
                                                        is_log_response = study_params$is_log_response,
                                                        blank_data = antigen_plate_current$plate_blanks,
                                                        blank_option = study_params$blank_option,
                                                        is_log_independent = study_params$is_log_independent,
                                                        apply_prozone = study_params$applyProzone,
                                                        verbose = TRUE)
    if (nrow(prepped_data_list[[id]]$data) < 6) {
      prepped_data_list[[id]] <- NULL
      next
    }

    formula_list[[id]] <- select_model_formulas(fixed_constraint = antigen_plate_current$fixed_a_result,
                                                response_variable = unique(antigen_plate_current$plate_standard$assay_response_variable),
                                                is_log_response = study_params$is_log_response)

    antigen_plate_name_list[[id]] <- id

  }

  return(list(prepped_data_list = prepped_data_list,
              formula_list = formula_list,
              antigen_plate_name_list = antigen_plate_name_list))
}

fit_experiment_plate_batch <- function(prepped_data_list_res,
                                       antigen_plate_list_res,
                                       model_names,
                                       study_params,
                                       se_antigen_table = NULL,
                                       verbose = TRUE) {
  prepped_data_list <- prepped_data_list_res$prepped_data_list
  formula_list <- prepped_data_list_res$formula_list
  antigen_plate_list <- antigen_plate_list_res$antigen_plate_list

  plate_model_constraints_list <- list()
  plate_start_lists <- list()
  plate_robust_fit_list <- list()
  fit_summary_list <- list()
  fit_params_list <- list()
  plot_data_list <- list()
  candidate_best_fit_list <- list()
  best_fit_list <- list()
  for(prep_dat_name in names(prepped_data_list)) {
    showNotification(id = "batch_sc_fit_notify", paste("processing", prep_dat_name), duration = NULL)
    if (verbose) print(prep_dat_name)
    plate_prepped_data <- prepped_data_list[[prep_dat_name]]
    formulas <- formula_list[[prep_dat_name]]
    response_variable <- unique(antigen_plate_list[[prep_dat_name]]$plate_standard$assay_response_variable)
    independent_variable <- unique(antigen_plate_list[[prep_dat_name]]$plate_standard$assay_independent_variable)
    fixed_a_result <- antigen_plate_list[[prep_dat_name]]$fixed_a_result
    antigen_settings <- antigen_plate_list[[prep_dat_name]]$antigen_settings
    if (verbose) print(independent_variable)
    plate_model_constraints_list[[prep_dat_name]] <- obtain_model_constraints(data = plate_prepped_data$data,
                                                                              formulas = formulas,
                                                                              independent_variable = independent_variable,
                                                                              response_variable = response_variable,
                                                                              is_log_response = TRUE,
                                                                              is_log_concentration = TRUE,
                                                                              antigen_settings = antigen_settings,
                                                                              max_response = max(plate_prepped_data$data[[response_variable]], na.rm = TRUE),
                                                                              min_response = min(plate_prepped_data$data[[response_variable]], na.rm = TRUE),
                                                                              verbose = verbose)


    plate_start_lists[[prep_dat_name]] <- make_start_lists(model_constraints = plate_model_constraints_list[[prep_dat_name]],
                                                           frac_generate = 0.8,
                                                           quants = c(low = 0.2, mid = 0.5, high = 0.8))

    plate_robust_fit_list[[prep_dat_name]] <- compute_robust_curves(prepped_data = plate_prepped_data$data,
                                                                    response_variable = response_variable,
                                                                    independent_variable = independent_variable,
                                                                    formulas = formulas,
                                                                    model_constraints = plate_model_constraints_list[[prep_dat_name]],
                                                                    start_lists =  plate_start_lists[[prep_dat_name]],
                                                                    verbose = verbose)

    fit_summary_list[[prep_dat_name]] <- summarize_model_fits(plate_robust_fit_list[[prep_dat_name]], verbose = verbose)

    fit_params_list[[prep_dat_name]] <- summarize_model_parameters(models_fit_list = plate_robust_fit_list[[prep_dat_name]],
                                                                   level = 0.95,
                                                                   model_names = model_names,
                                                                   verbose = verbose)

    plot_data_list[[prep_dat_name]] <- get_plot_data(models_fit_list =  plate_robust_fit_list[[prep_dat_name]],
                                                     prepped_data = plate_prepped_data$data,
                                                     fit_params = fit_params_list[[prep_dat_name]],
                                                     fixed_a_result = fixed_a_result,
                                                     model_names = model_names,
                                                     x_var = independent_variable,
                                                     y_var = response_variable,
                                                     verbose = verbose)


    candidate_best_fit_list[[prep_dat_name]]<- select_model_fit_AIC(fit_summary = fit_summary_list[[prep_dat_name]],
                                                                    fit_robust_lm = plate_robust_fit_list[[prep_dat_name]],
                                                                    fit_params = fit_params_list[[prep_dat_name]],
                                                                    plot_data = plot_data_list[[prep_dat_name]],
                                                                    verbose = verbose)
    # add the glance for the best fit
    # print(prep_dat_name)
    # print(length(candidate_best_fit_list[[prep_dat_name]]$best_model_name) == 1)


    candidate_best_fit_list[[prep_dat_name]] <- fit_qc_glance(best_fit = candidate_best_fit_list[[prep_dat_name]],
                                                              response_variable = response_variable,
                                                              independent_variable = independent_variable,
                                                              fixed_a_result = fixed_a_result,
                                                              antigen_settings = antigen_settings,
                                                              antigen_fit_options = prepped_data_list[[prep_dat_name]]$antigen_fit_options,
                                                              verbose = verbose)


    # # ## add the tidy to the best fit object
    candidate_best_fit_list[[prep_dat_name]] <- tidy.nlsLM(best_fit = candidate_best_fit_list[[prep_dat_name]],
                                                           fixed_a_result = fixed_a_result,
                                                           model_constraints = plate_model_constraints_list[[prep_dat_name]],
                                                           antigen_settings = antigen_settings,
                                                           antigen_fit_options = prepped_data_list[[prep_dat_name]]$antigen_fit_options,
                                                           verbose = verbose)

    # Extract identifiers for SE lookup
    current_plate <- antigen_plate_list[[prep_dat_name]]
    current_se <- if (!is.null(se_antigen_table)) {
      lookup_antigen_se(
        se_table = se_antigen_table,
        study_accession = unique(current_plate$plate_standard$study_accession),
        experiment_accession = unique(current_plate$plate_standard$experiment_accession),
        source = unique(current_plate$plate_standard$source),
        antigen = unique(current_plate$plate_standard$antigen)
      )
    } else {
      NA_real_
    }
    candidate_best_fit_list[[prep_dat_name]]  <- predict_and_propagate_error(best_fit = candidate_best_fit_list[[prep_dat_name]],
                                                                             response_var = "mfi",
                                                                             antigen_plate = antigen_plate_list[[prep_dat_name]],
                                                                             study_params = study_params,
                                                                             se_std_response = current_se,
                                                                             verbose = verbose)

    candidate_best_fit_list[[prep_dat_name]] <- gate_samples(best_fit = candidate_best_fit_list[[prep_dat_name]],
                                                             response_variable = "mfi",
                                                             pcov_threshold = antigen_settings$pcov_threshold, verbose = verbose
    )


  }

  return(candidate_best_fit_list)

}



create_batch_fit_outputs <- function(batch_fit_res, antigen_plate_list_res) {

  best_tidy_all <- do.call(
    rbind,
    lapply(batch_fit_res, function(x) x$best_tidy)
  )
  rownames(best_tidy_all) <- NULL


  best_glance_all <- dplyr::bind_rows(
    lapply(batch_fit_res, function(x) {
      bg <- x$best_glance
      if (is.null(bg)) return(NULL)

      bg$g <- if ("g" %in% names(bg)) dplyr::coalesce(bg$g, 1) else 1
      bg
    })
  )

  best_pred_all <- dplyr::bind_rows(
    lapply(batch_fit_res, function(x) x$best_pred)
  )

  best_sample_se_all <- dplyr::bind_rows(
    lapply(batch_fit_res, function(x) x$sample_se)
  )


  best_standard_all <- dplyr::bind_rows(
    lapply(batch_fit_res, function(x) {
      bg <- x$best_data
      if (is.null(bg)) return(NULL)
      bg$g <- if ("g" %in% names(bg)) dplyr::coalesce(bg$g, 1) else 1
      bg
    })
  )

  best_plate_all <- dplyr::distinct(best_standard_all[ , c("study_accession","experiment_accession","feature","source","plateid","plate","sample_dilution_factor","assay_response_variable","assay_independent_variable")])

  return(list(
    best_tidy_all     = best_tidy_all,
    best_glance_all   = best_glance_all,
    best_pred_all     = best_pred_all,
    best_sample_se_all = best_sample_se_all,
    best_standard_all = best_standard_all,
    best_plate_all    = best_plate_all,
    antigen_plate_list = antigen_plate_list_res
    )
    )
}


# add unique identifiers and rename the response variable to be generic for saving
process_batch_outputs <- function(batch_outputs, response_var) {

  batch_outputs$best_pred_all <- batch_outputs$best_pred_all %>%
    dplyr::group_by(
      study_accession,
      experiment_accession,
      plateid,
      plate,
      sample_dilution_factor,
      source,
      antigen
    ) %>%
    dplyr::mutate(id_match = dplyr::row_number()) %>%
    dplyr::ungroup()


  batch_outputs$best_sample_se_all <- batch_outputs$best_sample_se_all %>%
    dplyr::rename(assay_response = all_of(response_var)) %>%
    dplyr::group_by(
      study_accession, experiment_accession,
      plateid, plate, sample_dilution_factor, source, antigen,
      patientid, timeperiod, sampleid, dilution
    ) %>%
    dplyr::mutate(uid = dplyr::row_number()) %>%
    dplyr::ungroup()

  batch_outputs$best_standard_all <- batch_outputs$best_standard_all %>%
    dplyr::rename(assay_response = all_of(response_var))

  return(batch_outputs)
}

