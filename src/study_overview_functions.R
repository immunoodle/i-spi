## study overview functions

gmean <- function(x) {
  return(exp(mean(log(x))))
}
# Function to calculate geometric std deviation
gsd <- function(x) {
  return(exp(sd(log(x))))
}

gn <- function(x) {
  return(nrow(x))
}

pull_standard <- function(conn, selected_study, current_user) {
  standard_query <- glue::glue_sql("
  SELECT DISTINCT study_accession, experiment_accession AS Analyte, plate_id, well, antigen,
        		antibody_mfi AS MFI, antibody_n AS bead_count, pctaggbeads
  	FROM madi_results.xmap_standard
  	WHERE study_accession = {selected_study}
  	ORDER BY experiment_accession, antigen, plate_id",
                                   .con = conn)
  standard_data <- dbGetQuery(conn, standard_query)
  standard_data$plate_id <- str_trim(str_replace_all(standard_data$plate_id, "\\s", ""), side = "both")

  return(standard_data)
}
pull_blank <- function(conn, selected_study, current_user) {
  buffer_query <- glue::glue_sql("
  SELECT DISTINCT study_accession, experiment_accession AS Analyte, plate_id, well, antigen,
        		antibody_mfi AS MFI, antibody_n AS bead_count, pctaggbeads
  	FROM madi_results.xmap_buffer
  	WHERE study_accession = {selected_study}
  	ORDER BY experiment_accession, antigen, plate_id",
                                 .con = conn)
  blank_data <- dbGetQuery(conn, buffer_query)
  blank_data$plate_id <- str_trim(str_replace_all(blank_data$plate_id, "\\s", ""), side = "both")
  return(blank_data)
}
pull_control <- function(conn, selected_study, current_user){
  control_query <- glue::glue_sql("
  SELECT DISTINCT study_accession, experiment_accession AS Analyte, plate_id, well, antigen,
        		antibody_mfi AS MFI, antibody_n AS bead_count, pctaggbeads
  	FROM madi_results.xmap_control
  	WHERE study_accession = {selected_study}
  	ORDER BY experiment_accession, antigen, plate_id",
                                  .con = conn)
  control_data <- dbGetQuery(conn, control_query)
  control_data$plate_id <- str_trim(control_data$plate_id, side = "both")
  control_data$plate_id <- str_trim(str_replace_all(control_data$plate_id, "\\s", ""), side = "both")

  return(control_data)
}
pull_samples <- function(conn, selected_study, current_user) {
  select_query <- glue::glue_sql("
      		SELECT DISTINCT xmap_sample.study_accession, experiment_accession, plate_id,
      		  well, antigen,
        		antibody_mfi AS MFI, antibody_au AS AU,
        		dilution AS sample_dilution_factor,
        		CASE
        		  WHEN gate_class IN ('Between_Limits','Acceptable') THEN 'Acceptable'
              WHEN gate_class IN ('Below_Lower_Limit','Too Diluted') THEN 'Too Diluted'
      		    WHEN gate_class IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
              WHEN gate_class IN ('Not Evaluated') OR gate_class IS NULL THEN 'Not Evaluated' END AS gclod,
            CASE
              WHEN gate_class_linear_region IN ('Between_Limits','Acceptable') THEN 'Acceptable'
              WHEN gate_class_linear_region IN ('Below_Lower_Limit','Too Diluted') THEN 'Too Diluted'
              WHEN gate_class_linear_region IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
              WHEN gate_class_linear_region IN ('Not Evaluated') OR gate_class IS NULL THEN 'Not Evaluated' END AS gclin,
            CASE
              WHEN gate_class_loq IN ('Between_Limits','Acceptable') THEN 'Acceptable'
              WHEN gate_class_loq IN ('Below_Lower_Limit','Too Diluted') THEN 'Too Diluted'
              WHEN gate_class_loq IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
              WHEN gate_class_loq IN ('Not Evaluated') OR gate_class IS NULL THEN 'Not Evaluated' END AS gcloq,
            CASE WHEN antibody_n < lower_bc_threshold THEN 'LowBeadN' ELSE 'Acceptable' END AS lowbeadn,
            CASE WHEN pctaggbeads > pct_agg_threshold THEN 'PctAggBeads' ELSE 'Acceptable' END AS highbeadagg
      		FROM madi_results.xmap_sample
          INNER JOIN (
            SELECT study_accession, param_integer_value AS lower_bc_threshold
            FROM madi_results.xmap_study_config
    		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'lower_bc_threshold'
  		    ) AS bct ON bct.study_accession = xmap_sample.study_accession
          INNER JOIN (
            SELECT study_accession, param_integer_value AS pct_agg_threshold
            FROM madi_results.xmap_study_config
    		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'pct_agg_threshold'
  		    ) AS pab ON pab.study_accession = xmap_sample.study_accession
  		    WHERE xmap_sample.study_accession = {selected_study}
  ",
                                 .con = conn)
  # antibody_n AS bead_count, lower_bc_threshold,
  # pctaggbeads, pct_agg_threshold,
  active_samples <- dbGetQuery(conn, select_query)
  active_samples$analyte <- factor(active_samples$experiment_accession)
  active_samples$plate_id <- str_trim(str_replace_all(active_samples$plate_id, "\\s", ""), side = "both")
  return(active_samples)
}

# function to summarize data with gmean, gsd and count (n)
summarise_data <- function(df) {
  df %>%
    group_by(analyte, antigen, plate_id) %>%
    dplyr::summarise(
      gmean = gmean(mfi),
      gsd = gsd(mfi),
      n = dplyr::n(),
      cv_mfi = (sd(mfi)/mean(mfi)) * 100,
      mp_mfi = mean(mfi),
      .groups = "drop"
    )
}
# mp = mean plate
interplate_summarize <- function(df) {
  df %>%
    group_by(analyte, antigen) %>%
    dplyr::summarise(
      gmean = gmean(mp_mfi),
      gsd = gsd(mp_mfi),
      n = dplyr::n(),
      cv_mfi = (sd(mp_mfi)/mean(mp_mfi)) * 100,
      .groups = "drop"
    )
}
# Further summarise counts for specific conditions within active_samples
get_condition_counts <- function(data, condition_col, condition_val, count_col_name, sample_summ) {
  filtered <- data %>% dplyr::filter((!!sym(condition_col)) == condition_val)
  if (nrow(filtered) > 0) {
    filtered %>%
      dplyr::group_by(analyte, antigen, plate_id) %>%
      dplyr::summarise(!!count_col_name := dplyr::n(), .groups = "drop")
  } else {
    # if no rows match, create empty data.frame with zeros for all groups in sample_summ
    sample_summ %>%
      dplyr::select(analyte, antigen, plate_id) %>%
      dplyr::mutate(!!count_col_name := 0)
  }
}



check_plate <- function(selected_study){
 # conn <- get_db_connection()
  query_sample_dilution_factor <- glue::glue_sql("SELECT experiment_accession, plate_id, feature, dilution AS sample_dilution_factord
  	FROM madi_results.xmap_sample
  	WHERE study_accession = {selected_study};", .con = conn)
  dilutions <- distinct(dbGetQuery(conn, query_sample_dilution_factor))
  query_plates <- glue::glue_sql("SELECT xmap_header_id, experiment_accession, plate_id, plateid,
  plate, sample_dilution_factor
  	FROM madi_results.xmap_header
  	WHERE study_accession = {selected_study};", .con = conn)
  plates <- dbGetQuery(conn, query_plates)
  plates <- merge(plates, dilutions, by = c("plate_id","experiment_accession"), all.x = TRUE)
  #rm(dilutions)
  plates$needs_update <- ifelse(is.na(plates$sample_dilution_factord), 1, 0)
  plates$sample_dilution_factor <- ifelse(is.na(plates$sample_dilution_factord),
                                          plates$sample_dilution_factor,
                                          plates$sample_dilution_factord)
  plates$plateidr <- str_trim(str_replace_all(str_split_i(plates$plate_id, "\\\\", -1), " ", ""), side = "both")
  plates$needs_update <- ifelse(is.na(plates$plateid), 1, plates$needs_update)
  plates$plateid <- ifelse(is.na(plates$plateid),
                           plates$plateidr,
                           plates$plateid)
  plates$plateid <- str_replace_all(plates$plateid, fixed(".."),"_")
  plates$plateid <- str_replace_all(plates$plateid, fixed("."),"_")
  plates$plateid <- str_replace_all(plates$plateid, fixed("plate_"),"plate")
  plates$plate_id <- str_trim(str_replace_all(plates$plate_id, "\\s", ""), side = "both")
  if (nrow(plates) > 0) {
    plates$needs_update <- ifelse(is.na(plates$plate), 1, plates$needs_update)
    plates$plateids <- tolower(plates$plateid)
    plates$plateids <- str_trim(str_replace_all(plates$plateids, "\\s", ""), side = "both")
    plates$plateids <- stringr::str_replace_all(plates$plateids, "plaque", "plate")
    plates$plateids <- stringr::str_replace_all(plates$plateids, "_pt", "_plate")
    plates$plate <- str_split_i(plates$plateids, "plate",-1)
    plates$plate <- paste("plate",str_split_i(plates$plate, "_",1),sep = "_")
    plates <- distinct(plates[ , c("xmap_header_id","experiment_accession","plate_id","plateid","plate","sample_dilution_factor","needs_update")])
  }


  # does it need updating?
  plates_update <- plates[plates$needs_update == 1, c("xmap_header_id","experiment_accession","plate_id","plateid","plate","sample_dilution_factor")]

  #update
  if (nrow(plates_update)>0){
    for(i in seq_len(nrow(plates_update))) {
      this_row <- plates_update[i, ]

      sql <- glue_sql(
        "UPDATE xmap_header
     SET plateid = {this_row$plateid}, plate = {this_row$plate},
         sample_dilution_factor = {this_row$sample_dilution_factor}
     WHERE xmap_header_id = {this_row$xmap_header_id};",
        .con = conn
      )
      dbExecute(conn, sql)
    }
  }
  #dbDisconnect(conn)

  plates$analyte <- paste(plates$experiment_accession,plates$sample_dilution_factor,sep = "_")
  plates$feature <- plates$experiment_accession
  plates$plate_id <- toupper(plates$plate_id)
  plates <- plates[ , c("plate_id", "plateid", "plate", "feature", "analyte", "sample_dilution_factor")]
  return(plates)
}

load_specimens <- function(current_user, selected_study) {
  #conn <- get_db_connection()
  standard_data <- pull_standard(conn, selected_study, current_user)
  standard_data$specimen_type <- "standard"
  blank_data <- pull_blank(conn, selected_study, current_user)
  blank_data$specimen_type <- "blank"
  control_data <- pull_control(conn, selected_study, current_user)
  control_data$specimen_type <- "control"
  active_samples <- pull_samples(conn, selected_study, current_user)
  active_samples$specimen_type <- "sample"

  #dbDisconnect(conn)
  return(list(standard_data, blank_data, control_data, active_samples))
}

make_summspec <- function(standard_data, blank_data, control_data, active_samples, plates) {

  # Summarise buffer data and add specimen_type
  buffer_summ <- summarise_data(blank_data) %>%
    mutate(specimen_type = "blank")

  cat("aftr summarise_data blank")
  # Summarize control data and add specimen_type
  control_summ <- summarise_data(control_data) %>%
    mutate(specimen_type = "control")

  cat("aftr summarise_data control")
  # Summarize standard data and add specimen_type
  standard_summ <- summarise_data(standard_data) %>%
    mutate(specimen_type = "standard")

  cat("aftr summarise_data standard")
  # Summarize active_samples (sample data)
  sample_summ <- summarise_data(active_samples) %>%
    mutate(specimen_type = "sample")

  cat("after summarise_data sample")

  sample_lowbead <- get_condition_counts(active_samples, "lowbeadn", "LowBeadN", "nlowbead", sample_summ)
  cat("after sample low bead")
  print(class(sample_lowbead))
  sample_highbeadagg <- get_condition_counts(active_samples, "highbeadagg", "PctAggBeads", "nhighbeadagg", sample_summ)
  sample_gclin <- get_condition_counts(active_samples, "gclin", "Acceptable", "nlinear", sample_summ)
  sample_gcconc <- get_condition_counts(active_samples, "gclin", "Too Concentrated", "ntooconc", sample_summ)
  sample_gcdilut <- get_condition_counts(active_samples, "gclin", "Too Diluted", "ntoodilut", sample_summ)
  sample_gcaulod <- get_condition_counts(active_samples, "gclod", "Too Concentrated", "nabovelod", sample_summ)
  sample_gcbllod <- get_condition_counts(active_samples, "gclod", "Too Diluted", "nbelowlod", sample_summ)

  sample_summ <- sample_summ %>%
    left_join(sample_gclin, by = c("analyte", "antigen", "plate_id")) %>%
    left_join(sample_highbeadagg, by = c("analyte", "antigen", "plate_id")) %>%
    left_join(sample_lowbead, by = c("analyte", "antigen", "plate_id")) %>%
    left_join(sample_gcconc, by = c("analyte", "antigen", "plate_id")) %>%
    left_join(sample_gcdilut, by = c("analyte", "antigen", "plate_id")) %>%
    left_join(sample_gcaulod, by = c("analyte", "antigen", "plate_id")) %>%
    left_join(sample_gcbllod, by = c("analyte", "antigen", "plate_id")) %>%
    # Replace NAs in the new count columns with zeros
    replace_na(list(
      nlinear = 0,
      nhighbeadagg = 0,
      nlowbead = 0,
      ntooconc = 0,
      ntoodilut = 0,
      nabovelod = 0,
      nbelowlod = 0
    ))

  summ_spec <- bind_rows(buffer_summ, control_summ, standard_summ, sample_summ)

  summ_spec$plate_id <- toupper(summ_spec$plate_id)
  plates$plate_id <- toupper(plates$plate_id)
  summ_spec <- merge(summ_spec, plates, by="plate_id", all.x = TRUE)

  cat("Sum Spec:\n")
  print(head(summ_spec))
  if ("analyte.y" %in% names(summ_spec)) {
    names(summ_spec)[names(summ_spec) == "analyte.y"] <- "analyte"
  }
  return(summ_spec)
}

make_interplate_summ_spec <- function(summ_spec) {
  interplate_summ_spec <- interplate_summarize(summ_spec)
  return(interplate_summ_spec)
}

pivot_by_plate <- function(df, value_col) {
  df %>%
    select(analyte, antigen, plate_id, all_of(value_col)) %>%
    pivot_wider(names_from = plate_id, values_from = all_of(value_col), values_fill = 0)
}

pivot_sample_col <- function(df,colname) {
  df %>%
    select(analyte, antigen, plate_id, all_of(colname)) %>%
    pivot_wider(names_from = plate_id, values_from = all_of(colname), values_fill = 0)
}

# download report
report_vars <- function(summ_spec) {
  summ_spec <- distinct(summ_spec, analyte, antigen, plate, specimen_type, .keep_all = TRUE)
  summ_spec$nlowbead <- ifelse(is.na(summ_spec$nlowbead),0,summ_spec$nlowbead)
  summ_spec$nlinear <- ifelse(is.na(summ_spec$nlinear),0,summ_spec$nlinear)
  summ_spec$nhighbeadagg <- ifelse(is.na(summ_spec$nhighbeadagg),0,summ_spec$nhighbeadagg)
  summ_spec$ntooconc <- ifelse(is.na(summ_spec$ntooconc),0,summ_spec$ntooconc)
  summ_spec$ntoodilut <- ifelse(is.na(summ_spec$ntoodilut),0,summ_spec$ntoodilut)
  summ_spec$nabovelod <- ifelse(is.na(summ_spec$nabovelod),0,summ_spec$nabovelod)
  summ_spec$nbelowlod <- ifelse(is.na(summ_spec$nbelowlod),0,summ_spec$nbelowlod)
  summ_spec$pct_lin <- ifelse(summ_spec$n > 0, round(summ_spec$nlinear / summ_spec$n * 100, digits = 0), NULL)

  return(summ_spec)
}

convert_vars <- function(summ_spec) {
  x_vals <- seq(-5, 0, length.out = 1000)
  summ_spec_dup <- distinct(summ_spec, analyte, antigen, plate, specimen_type, .keep_all = TRUE)
  cat("summ spec_dup/n")
 print(head(summ_spec_dup))
  # print(unique(summ_spec_dup$plateid))
  sample_spec <- summ_spec_dup[summ_spec_dup$specimen_type=='sample', ]
  cat("sample spec\n")
  sample_spec$nlowbead <- ifelse(is.na(sample_spec$nlowbead),0,sample_spec$nlowbead)
  sample_spec$nlinear <- ifelse(is.na(sample_spec$nlinear),0,sample_spec$nlinear)
  sample_spec$nhighbeadagg <- ifelse(is.na(sample_spec$nhighbeadagg),0,sample_spec$nhighbeadagg)
  sample_spec$ntooconc <- ifelse(is.na(sample_spec$ntooconc),0,sample_spec$ntooconc)
  sample_spec$ntoodilut <- ifelse(is.na(sample_spec$ntoodilut),0,sample_spec$ntoodilut)
  sample_spec$nabovelod <- ifelse(is.na(sample_spec$nabovelod),0,sample_spec$nabovelod)
  sample_spec$nbelowlod <- ifelse(is.na(sample_spec$nbelowlod),0,sample_spec$nbelowlod)
  sample_spec$pct_lin <- ifelse(sample_spec$n > 0, round(sample_spec$nlinear / sample_spec$n * 100, digits = 0), NULL)

 # sample_spec$analyte <- paste(sample_spec$analyte, sample_spec$sample_dilution_factor, sep = "_")
  print(head(sample_spec))

  # sample_spec$plaque_info <- str_extract(sample_spec$plateid, regex("plaque[_]?\\d+[a-zA-Z]*", ignore_case = TRUE))
  # sample_spec$plate_number <- str_replace(sample_spec$plaque_info, regex("(?i)plaque[_]?", ""), "")
  # if (all(sample_spec$plate == "plate_")) {
  #   sample_spec$plate <- paste0(sample_spec$plate, sample_spec$plate_number)
  # }

 filter_res <- summ_spec_dup %>%
    dplyr::summarise(n = dplyr::n(), .by = c(analyte, antigen, plate, sample_dilution_factor, specimen_type)) |>
    dplyr::filter(n > 1L)

 cat("duplicated\n")
 print(filter_res)

  tsumm_spec <- pivot_wider(summ_spec_dup, id_cols = c("analyte", "antigen", "plate", "sample_dilution_factor"), names_from = "specimen_type", values_from = "n")
  cat("after tsum pivot")
  tsumm_spec$sample <- ifelse(is.na(tsumm_spec$sample), 0, tsumm_spec$sample)
  tsumm_spec$standard <- ifelse(is.na(tsumm_spec$standard), 0, tsumm_spec$standard)
  tsumm_spec$control <- ifelse(is.na(tsumm_spec$control), 0, tsumm_spec$control)
  tsumm_spec$blank <- ifelse(is.na(tsumm_spec$blank), 0, tsumm_spec$blank)
  #tsumm_spec$analyte <- paste(tsumm_spec$analyte, tsumm_spec$sample_dilution_factor, sep = "_")

  tsample <- as.data.frame(pivot_wider(tsumm_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "sample"))
  tstandard <- as.data.frame(pivot_wider(tsumm_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "standard"))
  tblank <- as.data.frame(pivot_wider(tsumm_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "blank"))
  tcontrol <- as.data.frame(pivot_wider(tsumm_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "control"))
  tsample_lin <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "pct_lin"))
  tsample_lobead <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "nlowbead"))
  tsample_hiagg <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "nhighbeadagg"))
  tsample_conc <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "ntooconc"))
  tsample_dilut <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "ntoodilut"))
  tsample_abovelod <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "nabovelod"))
  tsample_belowlod <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "nbelowlod"))
  return(list(tsample, tstandard, tblank, tcontrol, tsample_lin, tsample_lobead, tsample_hiagg, tsample_conc, tsample_dilut, tsample_abovelod, tsample_belowlod))
}

get_bg_color <- function(pctlin) {
  norm_val <- pctlin / 100
  colors <- viridisLite::viridis(100, begin=0.01, end=0.95, option = "E")
  colors[ceiling(norm_val * 99) + 1]
}

preprocess_plate_data <- function(current_user, selected_study){
  plates <- check_plate(selected_study)
  cat("before load specimens\n")
  loaded_data <- load_specimens(current_user, selected_study)
  cat("after loading specimens\n")
  standard_data <- as.data.frame(loaded_data[[1]])
  blank_data <- as.data.frame(loaded_data[[2]])
  control_data <- as.data.frame(loaded_data[[3]])
  active_samples <- as.data.frame(loaded_data[[4]])
  cat("before make summspec\n")
  summ_spec <- make_summspec(standard_data, blank_data, control_data, active_samples, plates)
  cat("after make spec\n")
  count_set <- convert_vars(summ_spec)
  plates$plate <- paste(plates$plate, plates$sample_dilution_factor, sep = "_")
  return(list(count_set, plates))
}
