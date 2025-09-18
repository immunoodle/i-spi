# study configuration settings

## render the study parameters checking the db for rows for user
check_and_render_study_parameters <- function(study_accession, user, max_attempts = 5, delay = 1, attempt = 1) {
  later::later(function() {
    nrow_params <- nrow(fetch_study_configuration(study_accession, user))

    if (nrow_params > 0) {
      # set reactive flag to trigger rendering
      study_params_ready(TRUE)
    } else if (attempt < max_attempts) {
      check_and_render_study_parameters(study_accession, user, max_attempts, delay, attempt + 1)
    } else {
      cat("Study parameters not found after", max_attempts, "attempts\n")
    }
  }, delay = delay)
}


obtain_initial_source <- function(study_accession) {
  study_sources_query <- paste0("SELECT DISTINCT  source
	FROM madi_results.xmap_standard
	WHERE study_accession = '",study_accession ,"'")

  study_sources_df <- dbGetQuery(conn, study_sources_query)

  initial_source <- unique(study_sources_df$source)[1]
  return(initial_source)

}

obtain_all_sc_source <- function(study_accession) {
  study_sources_query <- paste0("SELECT DISTINCT  source
	FROM madi_results.xmap_standard
	WHERE study_accession = '",study_accession ,"'")

  study_sources_df <- dbGetQuery(conn, study_sources_query)

  initial_source <- unique(study_sources_df$source)
  return(initial_source)

}


intitialize_study_configurations <- function(study_accession, user, initial_source) {

  ## Dilution Analysis Parameters
  node_order_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type, param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('", study_accession,"', 'dilution_analysis', 'node_order' , 'Type of Sample Limit', 'string', 20, 'selectInput, multiple', 'limits_of_detection,limits_of_quantification,linear_region' , NULL, NULL, 'linear_region', '", user, "');
")

 dbExecute(conn, node_order_insert)

 passing_limit_of_detection_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type, param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('", study_accession, "', 'dilution_analysis','valid_gate_class' , 'Passing Limit of Detection', 'string',
        20, 'selectInput', 'Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_of_Detection' , NULL, NULL, 'Between_Limits_of_Detection', '", user,"');
")
 dbExecute(conn, passing_limit_of_detection_insert)

 binary_gc_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('", study_accession, "', 'dilution_analysis','is_binary_gc' , 'Use Passing Limit of Detection as binary', 'boolean',9, 'checkbox',
        'TRUE,FALSE' , NULL, FALSE, NULL, '", user, "');")

 dbExecute(conn, binary_gc_insert)

 # AU treatment on status
  diluted_Tx_insert <- paste0("
INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'dilution_analysis','zero_pass_diluted_Tx' , 'Zero Passing Dilutions Too Diluted AU Treatment:', 'string',20, 'radioButtons',
        'all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au' , NULL, NULL, 'all_au', '", user,"');")

  dbExecute(conn, diluted_Tx_insert)

  concentrated_zero_Tx_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('", study_accession, "', 'dilution_analysis','zero_pass_concentrated_Tx' , 'Zero Passing Dilutions Too Concentrated AU Treatment:', 'string',20, 'radioButtons',
        'all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au' , NULL, NULL, 'all_au', '",user, "');")

  dbExecute(conn, concentrated_zero_Tx_insert)

  concentrated_diluted_Tx_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'dilution_analysis','zero_pass_concentrated_diluted_Tx' , 'Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment:', 'string',20, 'radioButtons',
        'all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au' , NULL, NULL, 'all_au', '",user, "');")

  dbExecute(conn, concentrated_diluted_Tx_insert)

  one_pass_acceptable_Tx_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'dilution_analysis','one_pass_acceptable_Tx' , 'One Passing Dilution AU Treatment:', 'string',20, 'radioButtons',
        'all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au' , NULL, NULL, 'geom_passing_au', '",user, "');")

  dbExecute(conn, one_pass_acceptable_Tx_insert)

  two_plus_acceptable_Tx_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'dilution_analysis','two_plus_pass_acceptable_Tx' , 'Two or More Passing Dilution AU Treatment:', 'string',20, 'radioButtons',
        'all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au' , NULL, NULL, 'geom_passing_au', '", user, "');")

  dbExecute(conn, two_plus_acceptable_Tx_insert)


  ## Bead Count Parameters
 lower_bc_threshold_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'bead_count','lower_bc_threshold' , 'Lower Threshold', 'numeric',9, 'numericInput',
        'NA,Inf' , 35, NULL, NULL, '", user, "');")

 dbExecute(conn, lower_bc_threshold_insert)

 upper_bead_count_threshold_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession, "', 'bead_count','upper_bc_threshold' , 'Upper Threshold', 'numeric',9, 'numericInput',
        'NA,Inf' , 50, NULL, NULL, '", user,"');")

 dbExecute(conn, upper_bead_count_threshold_insert)

 failed_well_criteria_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'bead_count','failed_well_criteria' , 'Failed Well Criteria', 'categorical',35, 'radioButtons',
        'Below_Upper_Threshold,Below_Lower_Threshold' , NULL, NULL, 'Below_Lower_Threshold', '",user,"');")

 dbExecute(conn, failed_well_criteria_insert)

 aggregate_beads_threshold_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
	study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
	param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
	VALUES ('",study_accession,"','bead_count','pct_agg_threshold','Aggregate Beads Threshold','numeric',9,'numericInput',
                                     'NA,Inf',30,NULL,NULL, '", user, "');")

 dbExecute(conn, aggregate_beads_threshold_insert)

 ## Standard Curve Options
 mean_mfi_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession, "', 'standard_curve_options','mean_mfi' , 'Compute Mean MFI at each Dilution Factor', 'boolean',35, 'switchInput',
        'TRUE,FALSE' , NULL, TRUE, NULL, '",user,"');")

 dbExecute(conn, mean_mfi_insert)

 blank_options_insert <- paste0(" INSERT INTO madi_results.xmap_study_config(
   study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
   param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
 VALUES ('",study_accession,"', 'standard_curve_options','blank_option' , 'Blank Control:', 'string',35, 'radioButtons',
         'ignored,included,subtracted,subtracted_3x,subtracted_10x' , NULL, NULL, 'ignored', '",user,"');")

 dbExecute(conn, blank_options_insert)

#  default_source_input <- paste0("INSERT INTO madi_results.xmap_study_config(
#   study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
#   param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
# VALUES ('",study_accession,"', 'standard_curve_options','default_source' , 'Source:', 'string',35, 'radioButtons',
#         'NULL' , NULL, NULL,", initial_source,",'",user,"');")
 cat("before source insert")
 default_source_input <- paste0(
   "INSERT INTO madi_results.xmap_study_config(
    study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
    param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
   VALUES ('", study_accession, "', 'standard_curve_options', 'default_source', 'Source:', 'string', 35, 'radioButtons',
           'NULL', NULL, NULL, '", initial_source, "', '", user, "');")




 dbExecute(conn, default_source_input)
 cat("after source insert")

 log_mfi_axis_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'standard_curve_options','is_log_mfi_axis' , 'Use Log Units for MFI', 'boolean',35, 'switchInput',
        'TRUE,FALSE' , NULL, TRUE, NULL, '",user,"');")

 dbExecute(conn, log_mfi_axis_insert)

 apply_prozone_correction_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession, "', 'standard_curve_options','applyProzone' , 'Apply Prozone Correction', 'boolean',35, 'switchInput',
        'TRUE,FALSE' , NULL, TRUE, NULL, '",user,"');")

 dbExecute(conn, apply_prozone_correction_insert)

 ## Subgroup Settings - wil be NULL and fill in with data on plate loading
 reference_level_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'subgroup_settings','reference_arm' , 'Reference Level:', 'string', 128, 'radioButtons',
        'NULL' , NULL, NULL, NULL, '",user,"');
")
 dbExecute(conn, reference_level_insert)

 timeperiod_order_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
   study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
   param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
 VALUES ('", study_accession, "', 'subgroup_settings','timeperiod_order' , 'Time Period Order:', 'string', 128, 'orderInput',
         'NULL' , NULL, NULL, NULL, '",user,"');")

 dbExecute(conn, timeperiod_order_insert)


 primary_timeperiod_comparison_insert <- paste0("INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession,"', 'subgroup_settings','primary_timeperiod_comparison' , ' Compare Two Time Periods:', 'string', 128, 'selectInput,Multiple',
        'NULL' , NULL, NULL, NULL, '",user,"');")

 dbExecute(conn, primary_timeperiod_comparison_insert)

 ## Antigen Family Settings
 antigen_family_order_insert <- paste0("
INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('", study_accession,"', 'antigen_family','antigen_family_order' , 'Antigen Family Order:', 'string', 128, 'orderInput',
        'NULL' , NULL, NULL, NULL, '",user,"');")
 dbExecute(conn, antigen_family_order_insert)


 antigen_order <- paste0("
INSERT INTO madi_results.xmap_study_config(
  study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type,
  param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user)
VALUES ('",study_accession, "', 'antigen_family','antigen_order' , 'Antigen Order:', 'string', 128, 'orderInput',
        'NULL' , NULL, NULL, NULL, '",user,"');
")

 dbExecute(conn, antigen_order)

}


## Fetch study parameters for a given study and user
fetch_study_configuration <- function(study_accession, user) {
  query <- paste0("SELECT xmap_study_config_id, study_accession, param_group, param_name, param_label, param_data_type, param_char_len, param_control_type, param_choices_list, param_integer_value, param_boolean_value, param_character_value, param_user
	FROM madi_results.xmap_study_config
	WHERE study_accession = '", study_accession,"'
	AND param_user = '", user,"';
")
  study_configuration_df  <- dbGetQuery(conn, query)
  return(study_configuration_df)
}

## Download user parameters
download_user_parameters <- function(study_accession, user) {
  query <- paste0("SELECT study_accession, param_group, param_label, param_integer_value, param_boolean_value, param_character_value, param_user
	FROM madi_results.xmap_study_config
	WHERE study_accession = '", study_accession,"'
	AND param_user = '", user,"';")
  downlaod_df <- dbGetQuery(conn, query)

  return(downlaod_df)
# 	download_plot_data <- download_this(
# 	  downlaod_df,
#     output_name = paste0(study_accession, "_study_config", user),
#     output_extension = ".xlsx",
#     button_label = paste0("Download ", study_accession, " Configuration for ", user),
#     button_type = "warning",
#     icon = "fa fa-save",
#     class = "hvr-sweep-to-left"
#   )

}

fetch_study_sources <- function(study_accession) {
  query <- paste0("SELECT DISTINCT study_accession, source
	FROM madi_results.xmap_standard
	WHERE study_accession = '", study_accession,"';")

  study_sources_df <- dbGetQuery(conn, query)
  return(study_sources_df)
}

fetch_study_arms <- function(study_accession) {
  query <- paste0("SELECT DISTINCT study_accession, agroup
	FROM madi_results.xmap_sample
	WHERE study_accession = '",study_accession,"';")
  study_arms_df <- dbGetQuery(conn, query)
  return(study_arms_df)
}

fetch_study_timeperiods <- function(study_accession) {
  query <- paste0("SELECT DISTINCT study_accession, timeperiod
	FROM madi_results.xmap_sample
	WHERE study_accession = '",study_accession,"';")
  study_timeperiods_df <- dbGetQuery(conn, query)
  return(study_timeperiods_df)
}


# Fetch Antigen Family Table
fetch_antigen_family_table <- function(study_accession) {
  if (study_accession == "reset") {
    result_df <- NULL
  } else {
    query <- paste0("
      SELECT xmap_antigen_family_id, study_accession, antigen, antigen_family
      FROM madi_results.xmap_antigen_family
      WHERE study_accession = '", study_accession, "'")

    # Run the query and fetch the result as a data frame
    result_df <- dbGetQuery(conn, query)

    if (nrow(result_df) == 0 || is.null(result_df)) {
      # check if result df is null or 0 rows and if so run additional query insert.
      insert_query <- paste0("INSERT INTO madi_results.xmap_antigen_family(
                	study_accession, antigen, antigen_family)
                	SELECT DISTINCT '",study_accession,"', antigen, 'All Antigens'
                	FROM madi_results.xmap_sample
                	WHERE study_accession = '", study_accession, "'
                	ORDER BY antigen;")
      dbExecute(conn, insert_query)
      result_df <- dbGetQuery(conn, query)
    } else {
      #}  ## Add new antigens to table
      insert_query <- paste0("
      INSERT INTO madi_results.xmap_antigen_family (study_accession, antigen, antigen_family)
      SELECT DISTINCT s.study_accession, s.antigen, 'All Antigens'
      FROM madi_results.xmap_sample s
      WHERE s.study_accession = '", study_accession, "'
        AND NOT EXISTS (
          SELECT 1 FROM madi_results.xmap_antigen_family f
          WHERE f.study_accession = s.study_accession
            AND f.antigen = s.antigen
        );")
      dbExecute(conn, insert_query)

      result_df <- dbGetQuery(conn, query)
    }
  } # end outer else statement

  return(result_df)
}


## Delete Plate from the SQL database and all associated data
delete_plate <- function(conn, selected_study, selected_plate_id, selected_plateid) {
  #conn <- get_db_connection()

  delete_header_str <- glue::glue_sql("DELETE FROM madi_results.xmap_header
    WHERE study_accession = {selected_study} AND plate_id = {selected_plate_id};", .con = conn)
  RPostgres::dbExecute(conn, delete_header_str)

  delete_buffer_str <- glue::glue_sql("DELETE FROM madi_results.xmap_buffer
    WHERE study_accession = {selected_study} AND plate_id = {selected_plate_id};", .con = conn)
  RPostgres::dbExecute(conn, delete_buffer_str)

  delete_control_str <- glue::glue_sql("DELETE FROM madi_results.xmap_control
    WHERE study_accession = {selected_study} AND plate_id = {selected_plate_id};", .con = conn)
  RPostgres::dbExecute(conn, delete_control_str)

  delete_sample_str <- glue::glue_sql("DELETE FROM madi_results.xmap_sample
    WHERE study_accession = {selected_study} AND plate_id = {selected_plate_id};", .con = conn)
  RPostgres::dbExecute(conn, delete_sample_str)

  delete_standard_str <- glue::glue_sql("DELETE FROM madi_results.xmap_standard
    WHERE study_accession = {selected_study} AND plate_id = {selected_plate_id};", .con = conn)
  RPostgres::dbExecute(conn, delete_standard_str)

  delete_fits_str <- glue::glue_sql("DELETE FROM madi_results.xmap_standard_fits
    WHERE study_accession = {selected_study} AND plateid = {selected_plateid};", .con = conn)
  RPostgres::dbExecute(conn, delete_fits_str)

  delete_fit_tab_str <- glue::glue_sql("DELETE FROM madi_results.xmap_standard_fit_tab
    WHERE study_accession = {selected_study} AND plateid = {selected_plateid};", .con = conn)
  RPostgres::dbExecute(conn, delete_fit_tab_str)

# dbDisconnect(conn)
}



