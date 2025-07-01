library(RPostgres); library(glue); library(DBI);
library(plotly);
library(ggplot2);
library(shiny);
library(tidyr)
library(stringr)
library(dplyr)
library(strex)
library(gt)
library(gtExtras)
library(ggridges)

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

geom_vdensity <- function(data, at, ...) {
  ggplot2::geom_segment(
    data = dplyr::filter(as.data.frame(density(data,na.rm = TRUE)[1:2]),
                         seq_along(x) == which.min(abs(x - at))),
    ggplot2::aes(x, 0, xend = x, yend = y), ...)
}


# Define database connection function
get_db_connection <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("db"),
            host = Sys.getenv("db_host"),
            port = Sys.getenv("db_port"),
            user = Sys.getenv("db_userid_x"),
            password = Sys.getenv("db_pwd_x"),
            options = "-c search_path=madi_results"
  )
}

conn <- get_db_connection()

current_user <- "mscotzens"
selected_study <- "MADI_01"

fit_query <- glue::glue_sql("
SELECT experiment_accession AS Analyte, antigen, plateid,
bkg_method AS buffer_treatment, is_log_mfi_axis AS logMFI, crit, cv, llod, ulod, bendlower AS llin, bendupper AS ulin, lloq, uloq
	FROM madi_results.xmap_standard_fits
	INNER JOIN madi_results.xmap_study_config ON xmap_standard_fits.study_accession = xmap_study_config.study_accession
	WHERE xmap_standard_fits.study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'default_source' AND source = param_character_value
	ORDER BY experiment_accession, antigen, plateid",
                               .con = conn)
standard_fit <- dbGetQuery(conn, fit_query)
standard_fit$plateid <- str_replace_all(standard_fit$plateid, fixed(".."),"_")
standard_fit$plateid <- str_replace_all(standard_fit$plateid, fixed("."),"_")



buffer_query <- glue::glue_sql("
SELECT DISTINCT study_accession, experiment_accession AS Analyte, REPLACE(split_part(plate_id,'\',-1),' ','.') AS plateid, well, antigen,
      		antibody_mfi AS MFI, antibody_n AS bead_count, pctaggbeads
	FROM madi_results.xmap_buffer
	WHERE study_accession = {selected_study}
	ORDER BY experiment_accession, antigen, plateid",
                            .con = conn)
buffer_data <- dbGetQuery(conn, buffer_query)
buffer_data$plateid <- str_split_i(buffer_data$plateid, "\\\\",-1)

buffer_data$plateid <- str_replace_all(buffer_data$plateid, fixed(".."),"_")
buffer_data$plateid <- str_replace_all(buffer_data$plateid, fixed("."),"_")
buffer_data$plate  <- ifelse(regexpr('plate', buffer_data$plateid) > 0,
                                str_replace_all(buffer_data$plateid, fixed("plate_"),"plate"),
                                ifelse(regexpr('_pt', buffer_data$plateid) > 0,
                                       str_replace_all(buffer_data$plateid, fixed("_pt"),"_plate"),
                                       buffer_data$plateid
                                )
)
buffer_data$plate <- ifelse(regexpr('plate', buffer_data$plate) < str_locate_last(buffer_data$plate,"_")[ , 1],
                               substr(buffer_data$plate,
                                      regexpr('plate', buffer_data$plate),
                                      str_locate_last(buffer_data$plate,"_")[ , 1]-1
                               ), substr(buffer_data$plate,
                                         regexpr('plate', buffer_data$plate),str_length(buffer_data$plate)-regexpr('plate', buffer_data$plate)+7
                               )
)
buffer_data$plateid <- str_replace_all(buffer_data$plateid, fixed(".."),"_")
buffer_data$plateid <- str_replace_all(buffer_data$plateid, fixed("."),"_")
buffer_gmean <- aggregate(mfi ~ analyte + antigen + plateid,
                         data = buffer_data,
                         FUN = gmean
                         )
buffer_gsd <- aggregate(mfi ~ analyte + antigen + plateid,
                         data = buffer_data,
                         FUN = gsd
)
buffer_n <- aggregate(mfi ~ analyte + antigen + plateid,
                        data = buffer_data,
                        FUN = length
)
buffer_summ <-  merge(buffer_gmean, buffer_gsd, by=c("analyte", "antigen", "plateid"))
colnames(buffer_summ)[4:5] <- c("blank_gmean", "blank_gsd")
buffer_summ <-  merge(buffer_summ, buffer_n, by=c("analyte", "antigen", "plateid"))
colnames(buffer_summ)[6] <- c("blank_n")

control_query <- glue::glue_sql("
SELECT DISTINCT study_accession, experiment_accession AS Analyte, REPLACE(split_part(plate_id,'\',-1),' ','.') AS plateid, well, antigen,
      		antibody_mfi AS MFI, antibody_n AS bead_count, pctaggbeads
	FROM madi_results.xmap_control
	WHERE study_accession = {selected_study}
	ORDER BY experiment_accession, antigen, plateid",
                               .con = conn)
control_data <- dbGetQuery(conn, control_query)
control_data$plateid <- str_split_i(control_data$plateid, "\\\\",-1)

control_data$plateid <- str_replace_all(control_data$plateid, fixed(".."),"_")
control_data$plateid <- str_replace_all(control_data$plateid, fixed("."),"_")
control_data$plate  <- ifelse(regexpr('plate', control_data$plateid) > 0,
                             str_replace_all(control_data$plateid, fixed("plate_"),"plate"),
                             ifelse(regexpr('_pt', control_data$plateid) > 0,
                                    str_replace_all(control_data$plateid, fixed("_pt"),"_plate"),
                                    control_data$plateid
                             )
)
control_data$plate <- ifelse(regexpr('plate', control_data$plate) < str_locate_last(control_data$plate,"_")[ , 1],
                            substr(control_data$plate,
                                   regexpr('plate', control_data$plate),
                                   str_locate_last(control_data$plate,"_")[ , 1]-1
                            ), substr(control_data$plate,
                                      regexpr('plate', control_data$plate),str_length(control_data$plate)-regexpr('plate', control_data$plate)+7
                            )
)
control_data$plateid <- str_replace_all(control_data$plateid, fixed(".."),"_")
control_data$plateid <- str_replace_all(control_data$plateid, fixed("."),"_")
control_gmean <- aggregate(mfi ~ analyte + antigen + plateid,
                          data = control_data,
                          FUN = gmean
)
control_gsd <- aggregate(mfi ~ analyte + antigen + plateid,
                        data = control_data,
                        FUN = gsd
)
control_n <- aggregate(mfi ~ analyte + antigen + plateid,
                      data = control_data,
                      FUN = length
)
control_summ <- merge(control_gmean, control_gsd, by=c("analyte", "antigen", "plateid"))
colnames(control_summ)[4:5] <- c("poscntrl_gmean", "poscntrl_gsd")
control_summ <-  merge(control_summ, control_n, by=c("analyte", "antigen", "plateid"))
colnames(control_summ)[6] <- c("poscntrl_n")

standard_query <- glue::glue_sql("
SELECT DISTINCT study_accession, experiment_accession AS Analyte, REPLACE(split_part(plate_id,'\',-1),' ','.') AS plateid, well, antigen,
      		antibody_mfi AS MFI, antibody_n AS bead_count, pctaggbeads
	FROM madi_results.xmap_standard
	WHERE study_accession = {selected_study}
	ORDER BY experiment_accession, antigen, plateid",
                                .con = conn)
standard_data <- dbGetQuery(conn, standard_query)
standard_data$plateid <- str_split_i(standard_data$plateid, "\\\\",-1)

standard_data$plateid <- str_replace_all(standard_data$plateid, fixed(".."),"_")
standard_data$plateid <- str_replace_all(standard_data$plateid, fixed("."),"_")
standard_data$plate  <- ifelse(regexpr('plate', standard_data$plateid) > 0,
                              str_replace_all(standard_data$plateid, fixed("plate_"),"plate"),
                              ifelse(regexpr('_pt', standard_data$plateid) > 0,
                                     str_replace_all(standard_data$plateid, fixed("_pt"),"_plate"),
                                     standard_data$plateid
                              )
)
standard_data$plate <- ifelse(regexpr('plate', standard_data$plate) < str_locate_last(standard_data$plate,"_")[ , 1],
                             substr(standard_data$plate,
                                    regexpr('plate', standard_data$plate),
                                    str_locate_last(standard_data$plate,"_")[ , 1]-1
                             ), substr(standard_data$plate,
                                       regexpr('plate', standard_data$plate),str_length(standard_data$plate)-regexpr('plate', standard_data$plate)+7
                             )
)
standard_data$plateid <- str_replace_all(standard_data$plateid, fixed(".."),"_")
standard_data$plateid <- str_replace_all(standard_data$plateid, fixed("."),"_")
standard_gmean <- aggregate(mfi ~ analyte + antigen + plateid,
                           data = standard_data,
                           FUN = gmean
)
standard_gsd <- aggregate(mfi ~ analyte + antigen + plateid,
                         data = standard_data,
                         FUN = gsd
)
standard_n <- aggregate(mfi ~ analyte + antigen + plateid,
                       data = standard_data,
                       FUN = length
)
standard_summ <- merge(standard_gmean, standard_gsd, by=c("analyte", "antigen", "plateid"))
colnames(standard_summ)[4:5] <- c("standard_gmean", "standard_gsd")
standard_summ <-  merge(standard_summ, standard_n, by=c("analyte", "antigen", "plateid"))
colnames(standard_summ)[6] <- c("standard_n")

pull_samples <- function(conn, selected_study, current_user) {
  select_query <- glue::glue_sql("
    		SELECT DISTINCT xmap_sample.study_accession, experiment_accession, REPLACE(split_part(plate_id,'\',-1),' ','.') AS plateid, well, antigen,
      		antibody_mfi AS MFI, antibody_au AS AU,
      		antibody_n AS bead_count, lower_bc_threshold,
      		pctaggbeads, pct_agg_threshold, dilution AS serum_dilution,
      		CASE
      		  WHEN gate_class IN ('Between_Limits','Acceptable') THEN 'Acceptable'
            WHEN gate_class IN ('Below_Lower_Limit','Too Diluted') THEN 'Too Diluted'
    		    WHEN gate_class IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
            WHEN gate_class IN ('Not Evaluated') OR gate_class IS NULL THEN 'Not Evaluated' END AS gate_class_lod,
          gate_class_linear_region, gate_class_loq
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
active_samples <- dbGetQuery(conn, select_query)
active_samples$Analyte <- factor(active_samples$experiment_accession)
active_samples$plateid <- str_split_i(active_samples$plateid, "\\\\",-1)

active_samples$plateid <- str_replace_all(active_samples$plateid, fixed(".."),"_")
active_samples$plateid <- str_replace_all(active_samples$plateid, fixed("."),"_")
active_samples$plate  <- ifelse(regexpr('plate', active_samples$plateid) > 0,
                             str_replace_all(active_samples$plateid, fixed("plate_"),"plate"),
                             ifelse(regexpr('_pt', active_samples$plateid) > 0,
                                    str_replace_all(active_samples$plateid, fixed("_pt"),"_plate"),
                                    active_samples$plateid
                             )
  )
active_samples$plate <- ifelse(regexpr('plate', active_samples$plate) < str_locate_last(active_samples$plate,"_")[ , 1],
                           substr(active_samples$plate,
                                  regexpr('plate', active_samples$plate),
                                  str_locate_last(active_samples$plate,"_")[ , 1]-1
                           ), substr(active_samples$plate,
                                     regexpr('plate', active_samples$plate),str_length(active_samples$plate)-regexpr('plate', active_samples$plate)+7
                           )
)
return(active_samples)
}

active_samples <- pull_samples(conn, selected_study, current_user)

table(active_samples$plateid, active_samples$plate)
table(active_samples$plate, active_samples$antigen, active_samples$Analyte)
table(active_samples$gate_class_lod)
table(active_samples$gate_class_linear_region)
table(active_samples$gate_class_linear_region, active_samples$antigen, active_samples$Analyte)

sample_gmean <- aggregate(mfi ~ antigen + plateid,
                            data = active_samples,
                            FUN = gmean
)
sample_gsd <- aggregate(mfi ~ antigen + plateid,
                          data = active_samples,
                          FUN = gsd
)
sample_n <- aggregate(mfi ~ antigen + plateid,
                        data = active_samples,
                        FUN = length
)


sample_summ <- merge(sample_gmean, sample_gsd, by=c("antigen", "plateid"), all.x = TRUE)
colnames(sample_summ)[3:4] <- c("sample_gmean", "sample_gsd")
sample_summ <-  merge(sample_summ, sample_n, by=c("antigen", "plateid"), all.x = TRUE)
colnames(sample_summ)[5] <- c("sample_n")

if (nrow(active_samples[active_samples$pctaggbeads >= active_samples$pct_agg_threshold, ]) > 0) {
  highaggbeadn <- aggregate(mfi ~ antigen + plateid,
                            data = active_samples[active_samples$pctaggbeads >= active_samples$pct_agg_threshold, ],
                            FUN = length
  )
  sample_summ <-  merge(sample_summ, highaggbeadn, by=c("antigen", "plateid"), all.x = TRUE)
  colnames(sample_summ)[5] <- c("highaggbeadn")
} else {
  sample_summ$highaggbeadn <- 0
}

if (nrow(active_samples[active_samples$beadcount <= active_samples$lower_bc_threshold, ]) > 0) {
  lowbead_n <- aggregate(mfi ~ antigen + plateid,
                         data = active_samples[active_samples$beadcount <= active_samples$lower_bc_threshold, ],
                         FUN = length
  )
  sample_summ <-  merge(sample_summ, lowbead_n, by=c("antigen", "plateid"), all.x = TRUE)
  colnames(sample_summ)[5] <- c("lowbead_n")
} else {
  sample_summ$lowbead_n <- 0
}


x_summ <- merge(sample_summ[, c("antigen", "plateid","sample_n")], buffer_summ[, c("antigen", "plateid","blank_n")], by=c("antigen", "plateid"), all.x = TRUE)
x_summ <- merge(x_summ, control_summ[, c("antigen", "plateid","poscntrl_n")], by=c("antigen", "plateid"), all.x = TRUE)
x_summ <- merge(x_summ, standard_summ[, c("antigen", "plateid","standard_n")], by=c("antigen", "plateid"), all.x = TRUE)
x_summ$poscntrl_n <- ifelse(is.na(x_summ$poscntrl_n),0,x_summ$poscntrl_n)


active_samples$Analyte <- paste(active_samples$Analyte, active_samples$serum_dilution, sep = "_")
active_samples$Feature <- paste(active_samples$Analyte, active_samples$antigen, sep = "_")

active_samples <- merge(active_samples, buffer_summ[ , c("plateid", "antigen", "blank_gmean","blank_gsd")], by = c("plateid","antigen"), all.x = TRUE)
active_samples <- merge(active_samples, control_summ[ , c("plateid", "antigen", "poscntrl_gmean","poscntrl_gsd")], by = c("plateid","antigen"), all.x = TRUE)
active_samples <- merge(active_samples, standard_fit[standard_fit$logmfi==TRUE , c("plateid", "antigen", "logmfi", "crit","llod", "ulod","llin","ulin")], by = c("plateid","antigen"), all.x = TRUE)
active_samples <- merge(active_samples, x_summ, by = c("plateid","antigen"), all.x = TRUE)

active_samples$llod <- ifelse(is.na(active_samples$llod),active_samples$blank_gmean + (3*active_samples$blank_gsd), active_samples$llod)
active_samples$ulod <- ifelse(is.na(active_samples$ulod),active_samples$poscntrl_gmean + (3*active_samples$poscntrl_gsd), active_samples$ulod)
active_samples$Detection <- ifelse(active_samples$gate_class_lod == "Not Evaluated",
                                   ifelse(active_samples$mfi > active_samples$blank_gmean + (3*active_samples$blank_gsd),"Acceptable","Too Diluted"),
                                   active_samples$gate_class_lod)
active_samples$Detection <- factor(active_samples$Detection)
active_samples$Linear_Region <- factor(active_samples$gate_class_linear_region)
table(active_samples$Detection)
table(active_samples$Linear_Region)
active_samples$log10_mfi <- log10(active_samples$mfi+1)
active_samples <- active_samples %>%
  mutate(qual_class=case_when(
    log10_mfi < llod ~ "Too.Diluted",
    log10_mfi >= llod & log10_mfi < llin ~ "Detect.Diluted",
    log10_mfi >= llin & log10_mfi < ulin ~ "Linear.Region",
    log10_mfi >= ulin & log10_mfi < ulod ~ "Detect.Concentrated",
    log10_mfi > ulod ~ "Too.Concentrated"))
active_samples$qual_class <- factor(active_samples$qual_class)
active_samples$Feature <- factor(active_samples$Feature)


# Create frequency table
active_samples <- distinct(active_samples, Analyte,antigen,plate,Feature,mfi,.keep_all = TRUE)
freq_table <- as.data.frame(table(
  Analyte = active_samples$Analyte,
  antigen = active_samples$antigen,
  plate = active_samples$plate,
  qual_class = active_samples$qual_class
  # Detection = active_samples$Detection,
  # Linear_Region = active_samples$Linear_Region
))
# freq_table$Quality <- paste0(str_pad(freq_table$Freq,3,"left"),": ",freq_table$qual_class)
freq_table$Quality <- paste0(str_pad(freq_table$Freq,3,"left"))
freq_table$orderq <- case_when(
                              freq_table$qual_class == "Too.Diluted" ~ 5,
                              freq_table$qual_class == "Detect.Diluted" ~ 4,
                              freq_table$qual_class == "Linear.Region" ~ 3,
                              freq_table$qual_class == "Detect.Concentrated" ~ 2,
                              freq_table$qual_class == "Too.Concentrated" ~ 1
                               )
freq_table <- freq_table[with(freq_table,order(Analyte,antigen,plate,orderq)),]
freq_table <- pivot_wider(freq_table, id_cols = c("Analyte","antigen","plate"), names_from = "qual_class", values_from = "Quality")
active_samples <- merge(active_samples, freq_table[ , c("Analyte","antigen","plate","Too.Concentrated","Detect.Concentrated","Linear.Region","Detect.Diluted","Too.Diluted")],
                        by = c("Analyte","antigen","plate"),
                        all.x = TRUE)
active_samples <- distinct(active_samples, Analyte,antigen,plate,Feature,mfi,qual_class,llod,ulod,.keep_all = TRUE)
# p <- ggplot(active_samples[active_samples$antigen=='a_darwin_14' & active_samples$Analyte %in% c("ADCD_150","ADCD_450","ADCD_50"),],

# p <- ggplot(active_samples[active_samples$antigen=='a_darwin_14' &
#                              active_samples$Analyte %in% c("IgG_1000","IgG1_1000","IgG2_500","IgG3_500","IgG4_250"),],

p <- ggplot(active_samples[active_samples$antigen=='a_darwin_14' &
                           active_samples$Analyte %in% c("FcR2A_10000","FcR2B_5000","FcR3A_5000","FcR3B_5000"),],
      # aes(y = log10_mfi, x= plate)) +
      # geom_violin(trim = FALSE, fill = "#C7EAE5", color = "darkblue") +
      aes(y=log10_mfi, x = ..scaled..)) +
  xlim(0,3) +
      geom_density(stat = "density", position = "identity",fill = "#C7EAE5", color = "darkblue") +
      geom_segment(aes(x = 0, xend = 0.5, y = llod, yend = llod), color = "#D31E00", size = 1) +
      geom_segment(aes(x = 0, xend = 0.5, y = llin, yend = llin), color = "#4E79A7", size = 1) +
      geom_segment(aes(x = 0, xend = 0.5, y = ulin, yend = ulin), color = "#4E79A7", size = 1) +
      geom_segment(aes(x = 0, xend = 0.5, y = ulod, yend = ulod), color = "#D31E00", size = 1) +

      # scale_y_discrete(expand = c(0.01, 0)) +
      # scale_x_continuous(expand = c(0.001, 2)) +
      labs(y = "MFI Measurement (log10)", x="Proportion of samples") +
      facet_grid(Analyte ~ plate, scales = "free") +
  geom_text(aes(x = 0.05, y = (llod - 0.3), label = Too.Diluted), hjust = "left", color = "#D31E00", size = 3) +
  geom_text(aes(x = 0.05, y = (llin - 0.1), label = Detect.Diluted), hjust = "left", color = "#4E79A7", size = 3) +
  geom_text(aes(x = 0.05, y = ((llin + ulin)/2), label = paste0(Linear.Region,": LinR")), hjust = "left", color = "#187A51", size = 3) +
  geom_text(aes(x = 0.05, y = (ulin + 0.1), label = Detect.Concentrated), hjust = "left", color = "#4E79A7", size = 3) +
  geom_text(aes(x = 0.05, y = (ulod + 0.3), label = Too.Concentrated), hjust = "left", color = "#D31E00", size = 3) +

  geom_text(aes(x = 3, y = 4.5, label = paste0("Samples: ",sample_n)), hjust = "right", color = "darkgrey", size = 3) +
  geom_text(aes(x = 3, y = 3.5, label = paste0("Blanks: ",blank_n)), hjust = "right", color = "darkgrey", size = 3) +
  geom_text(aes(x = 3, y = 2.5, label = paste0("Standards: ",standard_n)), hjust = "right", color = "darkgrey", size = 3) +
  geom_text(aes(x = 3, y = 1.5, label = paste0("PositiveC: ",poscntrl_n)), hjust = "right", color = "darkgrey", size = 3) +
      theme_bw() +
      theme(axis.text.x = element_text()
      )
print(p)


pull_study_overview <- function(conn, selected_study, current_user) {
  select_query <- glue::glue_sql("
    SELECT tot.experiment_accession, headr.uploader, tot.plateid,
    headr.serum_dilution, tot.antigen, tot.record_type, n_records,
    CASE WHEN n_below_bead_count_threshold IS NULL THEN 0 ELSE n_below_bead_count_threshold END AS n_below_bead_count_threshold,
    CASE WHEN n_above_aggregate_bead_threshold IS NULL THEN 0 ELSE n_above_aggregate_bead_threshold END AS n_above_aggregate_bead_threshold,
    CASE WHEN Acceptable_LOD IS NULL THEN 0 ELSE Acceptable_LOD END AS Acceptable_LOD,
    CASE WHEN Too_Diluted_LOD IS NULL THEN 0 ELSE Too_Diluted_LOD END AS Too_Diluted_LOD,
    CASE WHEN Too_Concentrated_LOD IS NULL THEN 0 ELSE Too_Concentrated_LOD END AS Too_Concentrated_LOD,
    CASE WHEN Not_Evaluated_LOD IS NULL THEN 0 ELSE Not_Evaluated_LOD END AS Not_Evaluated_LOD,
    CASE WHEN Acceptable_LIN IS NULL THEN 0 ELSE Acceptable_LIN END AS Acceptable_LIN,
    CASE WHEN Too_Diluted_LIN IS NULL THEN 0 ELSE Too_Diluted_LIN END AS Too_Diluted_LIN,
    CASE WHEN Too_Concentrated_LIN IS NULL THEN 0 ELSE Too_Concentrated_LIN END AS Too_Concentrated_LIN,
    CASE WHEN Not_Evaluated_LIN IS NULL THEN 0 ELSE Not_Evaluated_LIN END AS Not_Evaluated_LIN,
    CASE WHEN Acceptable_LOQ IS NULL THEN 0 ELSE Acceptable_LOQ END AS Acceptable_LOQ,
    CASE WHEN Too_Diluted_LOQ IS NULL THEN 0 ELSE Too_Diluted_LOQ END AS Too_Diluted_LOQ,
    CASE WHEN Too_Concentrated_LOQ IS NULL THEN 0 ELSE Too_Concentrated_LOQ END AS Too_Concentrated_LOQ,
    CASE WHEN Not_Evaluated_LOQ IS NULL THEN 0 ELSE Not_Evaluated_LOQ END AS Not_Evaluated_LOQ
    FROM (
    SELECT experiment_accession, plate_id, REPLACE(split_part(plate_id,'\',-1),' ','.') AS plateid,
    	antigen, record_type, COUNT(*) AS n_records
    	FROM (
    		SELECT DISTINCT study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, 'sample'  AS record_type
    		FROM madi_results.xmap_sample WHERE study_accession = {selected_study}
    		UNION
    		SELECT DISTINCT xmap_standard.study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, 'standard'  AS record_type
    		FROM madi_results.xmap_standard
    		INNER JOIN madi_results.xmap_study_config ON xmap_standard.study_accession = xmap_study_config.study_accession
    		WHERE xmap_standard.study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'default_source' AND source = param_character_value
    		UNION
    		SELECT DISTINCT study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, 'buffer'  AS record_type
    		FROM madi_results.xmap_buffer WHERE study_accession = {selected_study}
    		UNION
    		SELECT DISTINCT study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, 'control'  AS record_type
    		FROM madi_results.xmap_control WHERE study_accession = {selected_study}
    	) AS a
    	WHERE study_accession = {selected_study}
    	GROUP BY experiment_accession, plate_id, antigen, record_type
    	) AS tot
    	LEFT OUTER JOIN (
    SELECT xsample.experiment_accession,
    REPLACE(split_part(xmap_header.plate_id,'\',-1),' ','_') AS plateid,
    serum_dilution,
    auth0_user AS uploader,xmap_header.plate_id
    	FROM madi_results.xmap_header
    	INNER JOIN (SELECT DISTINCT study_accession, experiment_accession, plate_id, REPLACE(split_part(plate_id,'\',-1),' ','.') AS plateid, dilution AS serum_dilution
    		FROM madi_results.xmap_sample WHERE study_accession = {selected_study}
    		GROUP BY study_accession, experiment_accession, plate_id, REPLACE(split_part(plate_id,'\',-1),' ','.'), dilution
    		) AS xsample ON xsample.study_accession = xmap_header.study_accession
    		AND xsample.experiment_accession = xmap_header.experiment_accession
    		AND xsample.plate_id = xmap_header.plate_id
    ) AS headr ON headr.experiment_accession = tot.experiment_accession AND headr.plate_id = tot.plate_id
    LEFT OUTER JOIN (
  	SELECT experiment_accession, plate_id, antigen, record_type,
  		MAX(CASE WHEN gate_class IN ('Between_Limits','Acceptable') THEN n_LOD END) AS Acceptable_LOD,
  		MAX(CASE WHEN gate_class IN ('Below_Lower_Limit','Too Diluted') THEN n_LOD END) AS Too_Diluted_LOD,
  		MAX(CASE WHEN gate_class IN ('Above_Upper_Limit','Too Concentrated') THEN n_LOD END) AS Too_Concentrated_LOD,
  		MAX(CASE WHEN gate_class IN ('Not Evaluated') OR gate_class IS NULL THEN n_LOD END) AS Not_Evaluated_LOD
  		FROM (
  			SELECT experiment_accession, plate_id,
  			antigen, 'sample' AS record_type, gate_class, COUNT(*) AS n_LOD
  				FROM (
  				SELECT DISTINCT study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, gate_class, 'sample'  AS record_type
  				FROM madi_results.xmap_sample WHERE study_accession = {selected_study}
  				) AS xsample
  				GROUP BY experiment_accession,
  				plate_id,
  				antigen,
  				record_type, gate_class
  			) AS a
  	GROUP BY experiment_accession, plate_id, antigen, record_type, gate_class
  ) AS lodr ON lodr.experiment_accession = tot.experiment_accession
  AND lodr.antigen = tot.antigen
  AND lodr.plate_id = tot.plate_id
  AND lodr.record_type = tot.record_type
  LEFT OUTER JOIN (
  	SELECT experiment_accession, plate_id, antigen, record_type,
  		MAX(CASE WHEN gate_class_linear_region = 'Acceptable' THEN n_LIN END) AS Acceptable_LIN,
  		MAX(CASE WHEN gate_class_linear_region = 'Too Diluted' THEN n_LIN END) AS Too_Diluted_LIN,
  		MAX(CASE WHEN gate_class_linear_region = 'Too Concentrated' THEN n_LIN END) AS Too_Concentrated_LIN,
  		MAX(CASE WHEN gate_class_linear_region = 'Not Evaluated' OR gate_class_linear_region IS NULL THEN n_LIN END) AS Not_Evaluated_LIN
  		FROM (
  		SELECT experiment_accession, plate_id,
  		antigen, 'sample' AS record_type, gate_class_linear_region, COUNT(*) AS n_LIN
  			FROM (
  			SELECT DISTINCT study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, gate_class_linear_region, 'sample'  AS record_type
  			FROM madi_results.xmap_sample WHERE study_accession = {selected_study}
  			) AS xsample
  			GROUP BY experiment_accession,
  			plate_id,
  			antigen,
  			record_type, gate_class_linear_region
  			) AS a
  	GROUP BY experiment_accession, plate_id, antigen, record_type, gate_class_linear_region
  ) AS linr ON linr.experiment_accession = tot.experiment_accession
  AND linr.antigen = tot.antigen
  AND linr.plate_id = tot.plate_id
  AND linr.record_type = tot.record_type
  LEFT OUTER JOIN (
  	SELECT experiment_accession, plate_id, antigen, record_type,
  		MAX(CASE WHEN gate_class_loq = 'Acceptable' THEN n_LOQ END) AS Acceptable_LOQ,
  		MAX(CASE WHEN gate_class_loq = 'Too Diluted' THEN n_LOQ END) AS Too_Diluted_LOQ,
  		MAX(CASE WHEN gate_class_loq = 'Too Concentrated' THEN n_LOQ END) AS Too_Concentrated_LOQ,
  		MAX(CASE WHEN gate_class_loq = 'Not Evaluated' OR gate_class_loq IS NULL THEN n_LOQ END) AS Not_Evaluated_LOQ
  		FROM (
  		SELECT experiment_accession, plate_id,
  		antigen, 'sample' AS record_type, gate_class_loq, COUNT(*) AS n_LOQ
  			FROM (
  			SELECT DISTINCT study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, gate_class_loq, 'sample'  AS record_type
  			FROM madi_results.xmap_sample WHERE study_accession = {selected_study}
  			) AS xsample
  			GROUP BY experiment_accession,
  			plate_id,
  			antigen,
  			record_type, gate_class_loq
  			) AS a
  	GROUP BY experiment_accession, plate_id, antigen, record_type, gate_class_loq
  ) AS loqr ON loqr.experiment_accession = tot.experiment_accession
  AND loqr.antigen = tot.antigen
  AND loqr.plate_id = tot.plate_id
  AND loqr.record_type = tot.record_type
  LEFT OUTER JOIN (
	SELECT experiment_accession, plate_id,
	antigen, record_type, COUNT(*) AS n_below_bead_count_threshold
		FROM (
		SELECT DISTINCT study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, antibody_n, 'sample'  AS record_type
		FROM madi_results.xmap_sample WHERE study_accession = {selected_study}
		) AS xsample
		INNER JOIN madi_results.xmap_study_config ON xsample.study_accession = xmap_study_config.study_accession
		WHERE xsample.study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'lower_bc_threshold' AND antibody_n < param_integer_value
		GROUP BY experiment_accession,
		plate_id,
		antigen,
		record_type
) AS bclow ON bclow.experiment_accession = tot.experiment_accession AND bclow.plate_id = tot.plate_id AND bclow.antigen = tot.antigen AND bclow.record_type = tot.record_type
LEFT OUTER JOIN (
	SELECT experiment_accession, plate_id,
	antigen, 'sample' AS record_type, COUNT(*) AS n_above_aggregate_bead_threshold
		FROM (
		SELECT DISTINCT study_accession, experiment_accession, plate_id, well, antigen, antibody_mfi, pctaggbeads, 'sample'  AS record_type
		FROM madi_results.xmap_sample WHERE study_accession = {selected_study}
		) AS xsample
		INNER JOIN madi_results.xmap_study_config ON xsample.study_accession = xmap_study_config.study_accession
		WHERE xsample.study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'pct_agg_threshold' AND pctaggbeads > param_integer_value
		GROUP BY experiment_accession,
		plate_id,
		antigen,
		record_type
) AS pabhigh ON pabhigh.experiment_accession = pabhigh.experiment_accession AND pabhigh.plate_id = tot.plate_id AND pabhigh.antigen = tot.antigen AND pabhigh.record_type = tot.record_type

    ORDER BY tot.experiment_accession, tot.plate_id, tot.antigen, tot.record_type;",
  .con = conn)
  query_result <- dbGetQuery(conn, select_query)
  query_result
}

query_result <- pull_study_overview(conn, selected_study, current_user)

query_result$Analyte <- factor(query_result$experiment_accession)
query_result$plateid <- str_split_i(query_result$plateid, "\\\\",-1)

query_result$plateid <- str_replace_all(query_result$plateid, fixed(".."),"_")
query_result$plateid <- str_replace_all(query_result$plateid, fixed("."),"_")
query_result <- distinct(query_result)

all_wells <- distinct(query_result[query_result$record_type != "control" ,c("Analyte","serum_dilution","plateid","antigen","record_type","n_records")])
tall_wells <- pivot_wider(all_wells, id_cols = c("Analyte","serum_dilution","plateid","antigen"), names_from = "record_type", values_from = "n_records")
tall_wells$"sample buffer standard" <- paste(tall_wells$sample, tall_wells$buffer, tall_wells$standard)
tall_wells <- tall_wells[ ,c("Analyte","serum_dilution","plateid","antigen","sample buffer standard", "sample")]

bead_wells <- distinct(query_result[query_result$record_type == "sample"  ,c("Analyte","serum_dilution","plateid","antigen","n_below_bead_count_threshold")])
# tbead_wells <- pivot_wider(bead_wells, id_cols = c("Analyte","serum_dilution","plateid","antigen"), names_prefix = "lobead",names_from = "record_type", values_from = "n_below_bead_count_threshold")

agg_wells <- distinct(query_result[query_result$record_type == "sample"  ,c("Analyte","serum_dilution","plateid","antigen","n_above_aggregate_bead_threshold")])
# tagg_wells <- pivot_wider(agg_wells , id_cols = c("Analyte","serum_dilution","plateid","antigen"), names_prefix="hiagg",names_from = "record_type", values_from = "n_above_aggregate_bead_threshold")

tall_wells <- merge(tall_wells,bead_wells,by = c("Analyte","serum_dilution","plateid","antigen"), all.x = TRUE)
tall_wells <- merge(tall_wells,agg_wells,by = c("Analyte","serum_dilution","plateid","antigen"), all.x = TRUE)

tall_wells$tally <- ifelse(tall_wells$n_below_bead_count_threshold > 0,
                          ifelse(tall_wells$n_above_aggregate_bead_threshold > 0,
                                 paste0("low_beadcount=",tall_wells$n_below_bead_count_threshold," high_pct_agg=",tall_wells$n_above_aggregate_bead_threshold),
                                 paste0(" low_beadcount=",tall_wells$n_below_bead_count_threshold)
                          ),
                          ifelse(tall_wells$n_above_aggregate_bead_threshold > 0,
                                 paste0(" high_pct_agg=",tall_wells$n_above_aggregate_bead_threshold),
                                 ""
                          )

                    )
tall_wells <- tall_wells[ , c("Analyte","serum_dilution","plateid","antigen","sample buffer standard","tally", "sample") ]


samp_wells_lod <- query_result[query_result$record_type == "sample" , c("Analyte","serum_dilution","plateid","antigen",
                                                                    "acceptable_lod","too_diluted_lod","too_concentrated_lod","not_evaluated_lod")]
samp_wells_lod <- distinct(samp_wells_lod)
samp_wells_lin <- query_result[query_result$record_type == "sample" , c("Analyte","serum_dilution","plateid","antigen",
                                                                        "acceptable_lin","too_diluted_lin","too_concentrated_lin","not_evaluated_lin")]
samp_wells_lin <- distinct(samp_wells_lin)
samp_wells_loq <- query_result[query_result$record_type == "sample" , c("Analyte","serum_dilution","plateid","antigen",
                                                                        "acceptable_loq","too_diluted_loq","too_concentrated_loq","not_evaluated_loq")]
samp_wells_loq <- distinct(samp_wells_loq)

x_wells_lod <-  samp_wells_lod %>%
  group_by(Analyte,serum_dilution,plateid,antigen) %>%
  summarise(Acceptable_LOD = sum(acceptable_lod),Too_Diluted_LOD = sum(too_diluted_lod),Too_Concentrated_LOD = sum(too_concentrated_lod), NEv_LOD = sum(not_evaluated_lod))
x_wells_lod <- x_wells_lod[ , c("plateid","antigen", "Acceptable_LOD","Too_Diluted_LOD","Too_Concentrated_LOD")]
x_wells_lin <-  samp_wells_lin %>%
  group_by(Analyte,serum_dilution,plateid,antigen) %>%
  summarise(Acceptable_LIN = sum(acceptable_lin),Too_Diluted_LIN = sum(too_diluted_lin),Too_Concentrated_LIN = sum(too_concentrated_lin))
x_wells_lin <- x_wells_lin[ , c("plateid","antigen", "Acceptable_LIN","Too_Diluted_LIN","Too_Concentrated_LIN")]
x_wells_loq <-  samp_wells_loq %>%
  group_by(Analyte,serum_dilution,plateid,antigen) %>%
  summarise(Acceptable_LOQ = sum(acceptable_loq),Too_Diluted_LOQ = sum(too_diluted_loq),Too_Concentrated_LOQ = sum(too_concentrated_loq))
x_wells_loq <- x_wells_loq[ , c("plateid","antigen", "Acceptable_LOQ","Too_Diluted_LOQ","Too_Concentrated_LOQ")]

wellsm <- merge(tall_wells,x_wells_lod,by = c("plateid","antigen"),all.x = TRUE)
wellsm <- merge(wellsm,x_wells_lin,by = c("plateid","antigen"),all.x = TRUE)
wellsm <- merge(wellsm,x_wells_loq,by = c("plateid","antigen"),all.x = TRUE)
names(wellsm)
unique(wellsm$plateid)
names(standard_fit)
unique(standard_fit$plateid)
wellsm_lod <- merge(wellsm,standard_fit,by = c("plateid","antigen"),all.x = TRUE)
wellsm_lod$propLODaccept <- wellsm_lod$Acceptable_LOD/wellsm_lod$sample
wellsm_lod$propLINaccept <- wellsm_lod$Acceptable_LIN/wellsm_lod$sample
wellsm_lod$propLOQaccept <- wellsm_lod$Acceptable_LOQ/wellsm_lod$sample
wellsm_lod$plate <- ifelse(regexpr('plate', wellsm_lod$plateid) > 0,
                           str_replace_all(wellsm_lod$plateid, fixed("plate_"),"plate"),
                           ifelse(regexpr('_pt', wellsm_lod$plateid) > 0,
                                  str_replace_all(wellsm_lod$plateid, fixed("_pt"),"_plate"),
                                  wellsm_lod$plateid
                                  )
                           )
wellsm_lod$plate <- ifelse(regexpr('plate', wellsm_lod$plate) < str_locate_last(wellsm_lod$plate,"_")[ , 1],
                           substr(wellsm_lod$plate,
                                  regexpr('plate', wellsm_lod$plate),
                                  str_locate_last(wellsm_lod$plate,"_")[ , 1]-1
                           ), substr(wellsm_lod$plate,
                                     regexpr('plate', wellsm_lod$plate),str_length(wellsm_lod$plate)-regexpr('plate', wellsm_lod$plate)+7
                           )
                    )
table(wellsm_lod$plateid, wellsm_lod$plate)
table(wellsm_lod$crit, wellsm_lod$antigen)


