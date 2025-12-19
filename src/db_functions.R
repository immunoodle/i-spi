get_db_connection <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("db"),
            host = Sys.getenv("db_host"),
            port = Sys.getenv("db_port"),
            user = Sys.getenv("db_userid_x"),
            password = Sys.getenv("db_pwd_x"),
            sslmode = 'require',
            options = "-c search_path=madi_results"
  )
}

fetch_study_parameters <- function(study_accession, param_user, param_group = "standard_curve_options", conn) {
  query <- glue("
  SELECT study_accession, param_name, param_boolean_value, param_character_value
	FROM madi_results.xmap_study_config
  WHERE study_accession = '{study_accession}'
  AND param_user = '{param_user}'
  AND param_group = '{param_group}';
")
  study_parameters <- dbGetQuery(conn, query)
  return(list(
    applyProzone = study_parameters[study_parameters$param_name=="applyProzone", "param_boolean_value"],
    blank_option = study_parameters[study_parameters$param_name=="blank_option", "param_character_value"],
    standard_source = study_parameters[study_parameters$param_name=="default_source", "param_character_value"],
    is_log_response = study_parameters[study_parameters$param_name=="is_log_mfi_axis", "param_boolean_value"],
    is_log_independent = TRUE,
    mean_mfi = study_parameters[study_parameters$param_name=="mean_mfi", "param_boolean_value"]
  ))
}

fetch_antigen_parameters <- function(study_accession, experiment_accession, conn) {
  query <- glue("
  SELECT
    xmap_antigen_family_id,
    study_accession,
    experiment_accession,
    antigen,
    l_asy_min_constraint,
    l_asy_max_constraint,
    l_asy_constraint_method,
    standard_curve_concentration,
    pcov_threshold
  FROM madi_results.xmap_antigen_family
  WHERE study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';
")
  antigen_constraints <- dbGetQuery(conn, query)
  return(antigen_constraints=antigen_constraints)
}


fetch_db_header <- function(study_accession, experiment_accession, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, sample_dilution_factor, plate_id,
  assay_response_variable, assay_independent_variable
  FROM madi_results.xmap_header
WHERE study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

fetch_db_standards <- function(study_accession, experiment_accession, conn) {
  query <- glue("SELECT study_accession, experiment_accession, feature, plate_id, stype, source, sampleid, well, dilution, antigen, antibody_mfi AS mfi FROM madi_results.xmap_standard
WHERE study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  standard_df  <- dbGetQuery(conn, query)
  standard_df <- distinct(standard_df)
  return(standard_df)
}

fetch_db_buffer <- function(study_accession, experiment_accession, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plate_id, stype, well, antigen, dilution, feature, antibody_mfi AS mfi FROM madi_results.xmap_buffer
WHERE study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  blank_data <- dbGetQuery(conn, query)
  blank_data <- distinct(blank_data)
  return(blank_data)
}

fetch_db_samples <- function(study_accession, experiment_accession, conn) {
  query <- glue("SELECT study_accession,
experiment_accession, plate_id, timeperiod, patientid,
well, stype, sampleid,  agroup, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi AS mfi,
antibody_n FROM madi_results.xmap_sample
WHERE study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  sample_data <- dbGetQuery(conn, query)
  sample_data <- distinct(sample_data)
  return(sample_data)
}

pull_data <- function(study_accession, experiment_accession, conn = conn) {
  plates <- fetch_db_header(study_accession = study_accession,
                            experiment_accession = experiment_accession,
                            conn = conn)
  plates$plate_id <- trimws(plates$plate_id)

  antigen_constraints <- fetch_antigen_parameters(study_accession = study_accession,
                                                  experiment_accession = experiment_accession,
                                                  conn = conn)
  standard_curve_data <- fetch_db_standards(study_accession = study_accession,
                                            experiment_accession = experiment_accession,
                                            conn = conn)
  standard_curve_data$plate_id <- trimws(standard_curve_data$plate_id)

  standards <- inner_join(standard_curve_data, plates, by = c("study_accession", "experiment_accession","plate_id"))[ ,
                                                                                                                      c("study_accession","experiment_accession","feature","source","plateid","plate", "stype","sample_dilution_factor",
                                                                                                                        "sampleid","well","dilution","antigen","mfi",
                                                                                                                        "assay_response_variable", "assay_independent_variable")]
  blanks <- inner_join(fetch_db_buffer(study_accession = study_accession,
                                       experiment_accession = experiment_accession,
                                       conn = conn) %>% dplyr::mutate(plate_id = trimws(as.character(plate_id))),
                       plates, by = c("study_accession", "experiment_accession","plate_id"))
  samples <- inner_join(fetch_db_samples(study_accession = study_accession,
                                         experiment_accession = experiment_accession,
                                         conn = conn) %>% dplyr::mutate(plate_id = trimws(as.character(plate_id))),
                        plates, by = c("study_accession", "experiment_accession","plate_id"))
  response_var = unique(plates$assay_response_variable)
  indep_var = unique(plates$assay_independent_variable)

  return(list(plates=plates, standards=standards,
              blanks=blanks, samples=samples,
              antigen_constraints=antigen_constraints,
              response_var = response_var,
              indep_var = indep_var)
  )
}

save_best_fit_glances <- function(batch_outputs) {
  batch_outputs1 <- batch_outputs
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "is_log_response"] <- "is_log_mfi_axis"
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "a"] <- "l_asy"
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "d"] <- "r_asy"
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "c"] <- "x_mid"
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "b"] <- "scale"
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "lloq_y"] <- "bendlower"
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "uloq_y"] <- "bendupper"
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "inflect_x"] <- "x_inflection"
  names(batch_outputs$best_glance_all)[names(batch_outputs$best_glance_all) == "inflect_y"] <- "y_inflection"

}


save_best_fit_tidys <- function(batch_outputs) {
  names(batch_outputs$best_tidy_all)
}

save_best_fit_samples <- function(batch_outputs) {
  names(batch_outputs$best_sample_se_all)[names(batch_outputs$best_sample_se_all) == "gate_class_pcov"] <- "gate_class_linear_region"
  names(batch_outputs$best_sample_se_all)[names(batch_outputs$best_sample_se_all) == "pcov"] <- "quality_score"
  names(batch_outputs$best_sample_se_all)[names(batch_outputs$best_sample_se_all) == "gate_class_lod"] <- "gate_class"
  names(batch_outputs$best_sample_se_all)[names(batch_outputs$best_sample_se_all) == "se_x"] <- "antibody_au_se"

  batch_outputs$best_sample_se_all$predicted_concentration[is.nan(batch_outputs$best_sample_se_all$predicted_concentration)] <- NA_real_
  batch_outputs$best_sample_se_all$antibody_au_se[is.nan(batch_outputs$best_sample_se_all$antibody_au_se)] <- NA_real_
  batch_outputs$best_sample_se_all$au[is.nan(batch_outputs$best_sample_se_all$au)] <- NA_real_
  cols <- c(
    "gate_class_loq",
    "gate_class",
    "gate_class_linear_region"
  )

  batch_outputs$best_sample_se_all[cols] <-
    lapply(batch_outputs$best_sample_se_all[cols],
           function(x) ifelse(is.na(x), "Not Evaluated", x))

  batch_outputs$best_sample_se_all <- batch_outputs$best_sample_se_all[,!(names(batch_outputs$best_sample_se_all) %in% c("y_new"))]

}

save_best_preds <- function(batch_outputs) {
  names(batch_outputs$best_pred_all)[names(batch_outputs$best_pred_all) == "yhat"] <- "fitted"
  batch_outputs$best_pred_all <- batch_outputs$best_pred_all[,!(names(batch_outputs$best_pred_all) %in% c("x"))]
}




select_antigen_plate <- function(loaded_data,
                                 study_accession = study_accession,
                                 experiment_accession = experiment_accession,
                                 source = source,
                                 antigen = antigen,
                                 plate = plate,
                                 sample_dilution_factor = sample_dilution_factor,
                                 antigen_constraints = antigen_constraints)
{
  plate_standard  <- loaded_data$standards[loaded_data$standards$source == source &
                                             loaded_data$standards$antigen == antigen &
                                             loaded_data$standards$plate == plate &
                                             loaded_data$standards$sample_dilution_factor == sample_dilution_factor,]

  plate_blanks <- loaded_data$blanks[loaded_data$blanks$antigen == antigen &
                                       loaded_data$blanks$plate == plate &
                                       loaded_data$blanks$sample_dilution_factor == sample_dilution_factor,]

  plate_samples <- loaded_data$samples[loaded_data$samples$antigen == antigen &
                                         loaded_data$samples$plate == plate &
                                         loaded_data$samples$sample_dilution_factor == sample_dilution_factor,]

  antigen_settings <- obtain_lower_constraint(dat = plate_standard,
                                              antigen = antigen,
                                              study_accession = study_accession,
                                              experiment_accession = experiment_accession,
                                              plate = plate,

                                              plateid = unique(plate_standard$plateid),

                                              plate_blanks = plate_blanks,
                                              antigen_constraints = antigen_constraints)


  fixed_a_result <- test_fixed_lower_asymptote(antigen_settings)

  std_error_blank <- get_blank_se(antigen_settings = antigen_settings)

  return (list(plate_standard=plate_standard,
               plate_blanks=plate_blanks,
               plate_samples=plate_samples,
               antigen_settings=antigen_settings,
               fixed_a_result = fixed_a_result,
               std_error_blank = std_error_blank))
}



