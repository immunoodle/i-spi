# Functions for Bead Count Analysis

# Calculate Log Dilution Factor and clean up plate names
calculate_log_dilution <- function(dat){
  dat <- dat %>% mutate(dilution_factor = 1/dat$dilution,
                        log_dilution = log10(dilution_factor))

  # dat$mfi <- log10(dat$mfi)
  #dat_v <<- dat
  return(dat)
}

# function takes in df which can be sample data or standard curve data with
#well column and converts character well to a well number in 96 well multiplex bead assay
#plate
obtain_well_number <- function(df, well_col) {
  # Define row and column indices of a 96-well plate
  rows <- LETTERS[1:8]  # A to H
  cols <- 1:12          # 1 to 12

  # Extract the well values from the specified column
  all_wells <- df[[well_col]]

  # Compute well numbers
  well_vector <- sapply(all_wells, function(well) {
    row_letter <- substr(well, 1, 1)
    col_number <- as.numeric(substr(well, 2, nchar(well)))

    # Check validity of row and column
    if (!(row_letter %in% rows) || is.na(col_number) || !(col_number %in% cols)) {
      return(NA)
    }

    # Compute well number
    row_index <- match(row_letter, rows)  # Convert row letter to index
    well_number <- (col_number - 1) * 8 + row_index

    return(ifelse(well_number >= 1 & well_number <= 96, well_number, NA))
  })

  # Add the new column to the original data frame
  df$well_number <- well_vector
  df_view <- df
  return(df)
}

# plot bead count with df with well, thresholds and criteria.
# Plotly outputs.
plot_bead_count <- function(df_well, lower_threshold, upper_threshold, failed_well_criteria) {
  if (failed_well_criteria == "Below_Upper_Threshold" && !is.na(upper_threshold)) {
    bead_count_threshold <- upper_threshold
  } else if (failed_well_criteria == "Below_Lower_Threshold" && !is.na(lower_threshold)) {
    bead_count_threshold <- lower_threshold
  } else if (is.na(upper_threshold) && is.na(lower_threshold)) {
    bead_count_threshold <- NA
  } else if (failed_well_criteria == "Below_Upper_Threshold" && is.na(upper_threshold)) {
    bead_count_threshold <- NA
  } else if (failed_well_criteria == "Below_Lower_Threshold" && is.na(lower_threshold)) {
    bead_count_threshold <- NA
  }
  # Add indicator of bead count gate class
  if (!is.na(bead_count_threshold)) {
    df_well$bead_count_gc <- ifelse(df_well$n >= bead_count_threshold,
                                             "Sufficient Bead Count", "Low Bead Count")
  } else {
    df_well$bead_count_gc <- "Sufficient Bead Count"
  }

  df_well$bead_count_gc <- factor(df_well$bead_count_gc,
                                           levels = c("Sufficient Bead Count", "Low Bead Count"))

  # Order the data by well number
  df_well <- df_well[order(df_well$well_number), ]
  df_well$well_number <- as.numeric(as.character(df_well$well_number))

  # calculate the average bead count to whole number
  mean_bead_count <- round(mean(df_well$n, na.rm = T),0)
  # hover text condition for sample data or standards
  # df_well <- df_well
  df_well$hover_text <- with(df_well, ifelse(bead_count_gc == "Sufficient Bead Count" & stype == "X",
                                             paste0("<br>Well: ", well,
                                                    "<br>Bead Count: ", n,
                                                    "<br>Subject: ", subject_accession
                                             ),
                                             paste0("<br>Well: ", well,
                                                    "<br>Bead Count: ", n)
                                      ))
  # for standards or sample
  data_type <- ifelse(unique(df_well$stype) == "S", "Standards", "Samples")

  # Create plot
  p <- plot_ly() %>%
    add_trace(
      data = df_well,
      x = ~well_number,
      y = ~n,
      type = "scatter",
      mode = "lines",
      line = list(color = 'lightgrey'),
      showlegend = F
    ) %>%
    # Points for Sufficient Bead Count
    add_trace(
      data = subset(df_well, bead_count_gc == "Sufficient Bead Count"),
      x = ~well_number,
      y = ~n,
      type = "scatter",
      mode = "markers",
      marker = list(color = "#0067a5"),
      showlegend = TRUE,
      name = "Sufficient Bead Count",
      text = ~hover_text,
      hoverinfo = "text"
    ) %>%
    # Points for Low Bead Count
    add_trace(
      data = subset(df_well, bead_count_gc == "Low Bead Count"),
      x = ~well_number,
      y = ~n,
      type = "scatter",
      mode = "markers",
      marker = list(color = "#be0032"),
      showlegend = TRUE,
      name = "Low Bead Count",
      text = ~hover_text,
      hoverinfo = "text"
    )
    # horizontal line at threshold
    # add_trace(
    #   x = c(0, 100), # 96 wells expected. Add a little buffer room on left and right
    #   y = bead_count_threshold,
    #   type = "scatter",
    #   mode = "lines",
    #   line = list(color = "#be0032", dash = "dash"),
    #   name = "Bead Count Threshold",
    #   text = ~paste0("Bead Count Threhold: ", bead_count_threshold),
    #   hoverinfo = "text"
    #
    # ) %>%
    if (!is.na(lower_threshold)) {
       p <- p %>%   add_trace(
          x = c(0, 100), # 96 wells expected. Add a little buffer room on left and right
          y = lower_threshold,
          type = "scatter",
          mode = "lines",
          line = list(color = "#be0032", dash = "dash"),
          name = "Lower Bead Count Threshold",
          text = ~paste0("Lower Bead Count Threhold: ", lower_threshold),
          hoverinfo = "text"

        )
    }
  if (!is.na(upper_threshold)) {
   p <- p %>%  add_trace(
      x = c(0, 100), # 96 wells expected. Add a little buffer room on left and right
      y = upper_threshold,
      type = "scatter",
      mode = "lines",
      line = list(color = "#0067a5", dash = "dash"),
      name = "Upper Bead Count Threshold",
      text = ~paste0("Upper Bead Count Threhold: ", upper_threshold),
      hoverinfo = "text"

    )
  }
    # average bead count
   p <- p %>%  add_trace(
      x = c(0, 100), # add a little buffer room on left and right. Expecting 96 wells.
      y = mean_bead_count,
      type = "scatter",
      mode = "lines",
      line = list(color = "black", dash = "solid"),
      name = "Average Bead Count",
      text = ~paste0("Average Bead Count: ", mean_bead_count),
      hoverinfo = "text"
    ) %>%
    layout(title  = paste("Bead Count Analysis for", data_type ),
           xaxis = list(title = "Well order"),
           yaxis = list(title = "Bead count"),
           font = list(size = 12))


  return(p)
}

# compute gate class of bead count
# bead_count_gc <- function(df_well, lower_threshold, upper_threshold, failed_well_criteria) {
#   if (failed_well_criteria == "upper") {
#     bead_count_threshold <- upper_threshold
#   } else if (failed_well_criteria == "lower") {
#     bead_count_threshold <- lower_threshold
#   }
#   # Add indicator of bead count gate class
#   df_well$bead_count_gc <- ifelse(df_well$n >= bead_count_threshold,
#                                   "Sufficient Bead Count", "Low Bead Count")
#   df_well$bead_count_gc <- factor(df_well$bead_count_gc,
#                                   levels = c("Sufficient Bead Count", "Low Bead Count"))
#
#   # boolean indicator of if it is low or not.
#   df_well$is_low_bead_count <- ifelse(df_well$n >= bead_count_threshold, F, T)
#   # Order the data by well number
#   df_well <- df_well[order(df_well$well_number), ]
#
#   return(df_well)
# }

bead_count_gc <- function(df_well, lower_threshold, upper_threshold, failed_well_criteria) {
  # Determine which threshold to use
  if (failed_well_criteria == "Below_Upper_Threshold" && !is.na(upper_threshold)) {
    bead_count_threshold <- upper_threshold
  } else if (failed_well_criteria == "Below_Lower_Threshold" && !is.na(lower_threshold)) {
    bead_count_threshold <- lower_threshold
  } else if (is.na(upper_threshold) && is.na(lower_threshold)) {
    bead_count_threshold <- NA
    return(df_well[0, ])
  } else if (failed_well_criteria == "Below_Upper_Threshold" && is.na(upper_threshold)) {
    bead_count_threshold <- NA
    return(df_well[0, ])
   } else if (failed_well_criteria == "Below_Lower_Threshold" && is.na(lower_threshold)) {
     bead_count_threshold <- NA
     return(df_well[0, ])
   } else {
    warning("Invalid or missing threshold value. Skipping bead count classification.")
    df_well$bead_count_gc <- NA
    df_well$is_low_bead_count <- NA
    df_well <- df_well[order(df_well$well_number), ]
    return(df_well)
  }

  # Add indicator of bead count gate class
  df_well$bead_count_gc <- ifelse(
    is.na(df_well$n),
    NA,
    ifelse(df_well$n >= bead_count_threshold, "Sufficient Bead Count", "Low Bead Count")
  )
  df_well$bead_count_gc <- factor(
    df_well$bead_count_gc,
    levels = c("Sufficient Bead Count", "Low Bead Count")
  )

  # Boolean indicator
  df_well$is_low_bead_count <- ifelse(
    is.na(df_well$n),
    NA,
    ifelse(df_well$n >= bead_count_threshold, F, T)
  )

  # Order by well number
  df_well <- df_well[order(df_well$well_number), ]
  df_well <- df_well[, c("study_accession", "experiment_accession", "plateid", "visit_name", "patientid",
                         "well", "stype", "sampleid", "arm_name", "dilution", "pctaggbeads", "samplingerrors",
                         "antigen",	"value_reported",	"n", "nominal_sample_dilution", "plate", "plate_nom",
                         "subject_accession"	,"bead_count_gc","is_low_bead_count")]
  return(df_well)
}

# download bead count gate class data
download_bead_count_data <- function(download_df, selected_study, selected_experiment) {
  download_df <- download_df[download_df$experiment_accession == selected_experiment,]
  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_study, "_",selected_experiment, "_bead_count_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download Bead Count Data for ",selected_experiment, " in ", selected_study),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}

