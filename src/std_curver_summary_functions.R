aggregate_params <- function(df) {
  list(
    a = median(df$a, na.rm = TRUE),
    b = median(df$b, na.rm = TRUE),
    c = median(df$c, na.rm = TRUE),
    d = median(df$d, na.rm = TRUE),
    g     = if ("g" %in% names(df)) median(df$g, na.rm = TRUE) else 1
  )
}

calculate_cv_dilution_platewise <- function(best_standard, antigen_settings) {

  df_cv_dilution_factor <- data.frame(
    experiment_accession = character(),
    source = character(),

    antigen = character(),
    dilution = numeric(),
    #  log_dilution_mean_antigen = numeric(),
    mean_assay_response_at_dilution = numeric(),
    sd_assay_response_at_dilution = numeric(),
    cv_assay_response_pct_at_dilution = numeric(),
    stringsAsFactors = FALSE)

  # unique combinations of antigens and dilutions
  for (experiment in unique(best_standard$experiment_accession)) {
    for (source in unique(best_standard$source)) {
      for (antigen in unique(best_standard$antigen)) {
        for (dilution in unique(best_standard$dilution)) {

          # subset data for the current antigen and dilution across all plates
          subset_data_dilution_series <- best_standard[best_standard$antigen == antigen & best_standard$dilution == dilution
                                                       & best_standard$source == source & best_standard$experiment_accession == experiment,]
          # print(subset_data_dilution_series$mfi)
          # check if there is more than 1 row for the current dilution level
          if (nrow(subset_data_dilution_series) > 1) {
            mean_assay_response_at_dilution <- mean(subset_data_dilution_series$assay_response, na.rm = TRUE)
            sd_assay_response_at_dilution <- sd(subset_data_dilution_series$assay_response, na.rm = TRUE)
            cv_assay_response_pct_at_dilution <- (sd_assay_response_at_dilution / mean_assay_response_at_dilution) * 100

            # Add the original log_dilution along with calculated mean, sd, and CV
            current_combination <- data.frame(experiment_accession = experiment,
                                              source = source,
                                              antigen = antigen,
                                              dilution = dilution,
                                              mean_assay_response_at_dilution = mean_assay_response_at_dilution,
                                              sd_assay_response_at_dilution = sd_assay_response_at_dilution,
                                              cv_assay_response_pct_at_dilution = cv_assay_response_pct_at_dilution)

            df_cv_dilution_factor <- rbind(df_cv_dilution_factor, current_combination)
          }
        }
      }
    }
  }

  df_cv_dilution_factor <- df_cv_dilution_factor[
    is.finite(df_cv_dilution_factor$cv_assay_response_pct_at_dilution), ]

  # compute antigen-specific concentrations and attach to cv dataframe
  cv_df <- data.frame()
  for (antigen in unique(df_cv_dilution_factor$antigen)) {
    antigen_undiluted_sc_concentration <- get_study_exp_antigen_plate_param(antigen_settings[antigen_settings$antigen == antigen,])
    cv_df <- rbind(cv_df,compute_concentration(data = df_cv_dilution_factor[df_cv_dilution_factor$antigen == antigen, ],
                                               undiluted_sc_concentration = antigen_undiluted_sc_concentration,
                                               independent_variable = "concentration",
                                               is_log_concentration = TRUE))

  }

  return(cv_df)

}

aggregate_standard_curves <- function(best_pred_all,
                                      experiment_accession,
                                      best_glance_all,
                                      antigen,
                                      source,
                                      indep_var,
                                      response_var,
                                      antigen_settings) {

  best_glance_experiment_antigen <- best_glance_all[
    best_glance_all$experiment_accession == experiment_accession &
      best_glance_all$antigen == antigen &
      best_glance_all$source == source,
  ]


  antigen_settings_specific <- antigen_settings[antigen_settings$antigen == antigen, ]

  ## ---------------------------
  ## Filter experiment / antigen / source
  ## ---------------------------
  best_pred_experiment_antigen <- best_pred_all[
    best_pred_all$experiment_accession == experiment_accession &
      best_pred_all$antigen == antigen &
      best_pred_all$source == source,
  ]
  if (nrow(best_pred_experiment_antigen) == 0) {
    warning("No data found for specified experiment / antigen / source")
    return(list(refit_fit_df = data.frame(),
                predicted_agg_df = data.frame()))
  }


  ## ---------------------------
  ## X grid (original concentration scale)
  ## ---------------------------
  x_values <- seq(
    min(best_pred_experiment_antigen$predicted_concentration, na.rm = TRUE),
    max(best_pred_experiment_antigen$predicted_concentration, na.rm = TRUE),
    length.out = 1000
  )

  ## ---------------------------
  ## Initialize outputs
  ## ---------------------------
  refit_fit_df   <- data.frame()
  predicted_agg_df <- data.frame()

  y <- best_pred_experiment_antigen$yhat
  x <- best_pred_experiment_antigen$predicted_concentration
  data <- data.frame(concentration = x,
                     mfi = y)


  formulas <- select_model_formulas(fixed_constraint = NULL, response_variable = response_var,
                                    is_log_response = study_params$is_log_response)

  params_by_model <- split(best_glance_experiment_antigen, best_glance_experiment_antigen$crit)

  model_counts <- table(best_glance_experiment_antigen$crit)
  agg_model <- names(which.max(model_counts))



  agg_params_by_model <- lapply(params_by_model, aggregate_params)
  agg_params <- agg_params_by_model[[agg_model]]

  if (agg_model == "Yd5") {

    predicted_mfi_agg <- Yd5(
      x_values,
      a = as.numeric(agg_params["a"]),
      b = as.numeric(agg_params["b"]),
      c = as.numeric(agg_params["c"]),
      d = as.numeric(agg_params["d"]),
      g = as.numeric(agg_params["g"])
    )

  } else if (agg_model == "Y5") {

    predicted_mfi_agg <- Y5(
      x_values,
      a = as.numeric(agg_params["a"]),
      b = as.numeric(agg_params["b"]),
      c = as.numeric(agg_params["c"]),
      d = as.numeric(agg_params["d"]),
      g = as.numeric(agg_params["g"])
    )

  } else if (agg_model == "Y4") {

    predicted_mfi_agg <- Y4(
      x_values,
      a = as.numeric(agg_params["a"]),
      b = as.numeric(agg_params["b"]),
      c = as.numeric(agg_params["c"]),
      d = as.numeric(agg_params["d"])
    )

  } else if (agg_model == "Yd4") {

    predicted_mfi_agg <- Yd4(
      x_values,
      a = as.numeric(agg_params["a"]),
      b = as.numeric(agg_params["b"]),
      c = as.numeric(agg_params["c"]),
      d = as.numeric(agg_params["d"])
    )

  } else if (agg_model == "Ygomp4") {

    predicted_mfi_agg <- Ygomp4(
      x_values,
      a = as.numeric(agg_params["a"]),
      b = as.numeric(agg_params["b"]),
      c = as.numeric(agg_params["c"]),
      d = as.numeric(agg_params["d"])
    )

  } else {
    stop("Unsupported aggregated model: ", agg_model)
  }

  predicted_agg_df <- data.frame(
    plateid = "aggregated",
    mod_class = agg_model,
    experiment_accession = experiment_accession,
    antigen = antigen,
    source = source,
    predicted_concentration = x_values,
    yhat = predicted_mfi_agg
  )




  return(predicted_agg_df)

}

summarize_sc_fits_plotly <- function(best_pred_all, cv_df, best_plate_all, study_params, experiment_accession,
                                     aggregated_fit, antigen, source) {

  best_plates_exp_source <-  best_plate_all[
    best_plate_all$experiment_accession == experiment_accession &
      best_plate_all$source == source,
  ]
  assay_response_variable <- unique(best_plates_exp_source$assay_response_variable)[1]
  assay_independent_variable <- unique(best_plates_exp_source$assay_independent_variable)[1]
  if (study_params$is_log_response) {
    y_axis_label <- paste("log<sub>10</sub>", assay_response_variable)
  } else {
    y_axis_label <- assay_response_variable
  }

  if (study_params$is_log_independent) {
    x_axis_label <- paste("log<sub>10</sub>", assay_independent_variable)
  } else {
    x_axis_label <- assay_independent_variable
  }


  model_linetype <- c(
    "Y5" = "solid",
    "Yd5" = "dash",
    "Y4" = "dot",
    "Yd4" = "dashdot",
    "Ygomp4"  = "longdash"
  )

  antigen_source_exp_fits <- best_pred_all[
    best_pred_all$experiment_accession == experiment_accession &
      best_pred_all$antigen == antigen &
      best_pred_all$source == source,
  ]

  cv_df_antigen <- cv_df[cv_df$antigen == antigen & cv_df$experiment_accession == experiment_accession & cv_df$source == source,]

  if (nrow(antigen_source_exp_fits) == 0) {
    stop("No data found for this experiment / antigen / source")
  }

  # Create grouping variable
  antigen_source_exp_fits$group_id <- interaction(
    antigen_source_exp_fits$plateid,
    antigen_source_exp_fits$sample_dilution_factor,
    antigen_source_exp_fits$model,
    drop = TRUE
  )

  # Ensure proper ordering for lines
  antigen_source_exp_fits <- antigen_source_exp_fits[
    order(antigen_source_exp_fits$group_id,
          antigen_source_exp_fits$predicted_concentration),
  ]

  p <- plot_ly()

  for (grp in unique(antigen_source_exp_fits$group_id)) {

    df <- antigen_source_exp_fits[antigen_source_exp_fits$group_id == grp, ]

    dash_type  <- model_linetype[as.character(df$model[1])]

    p <- add_lines(
      p,
      data = df,
      x = ~predicted_concentration,
      y = ~yhat,
      name = as.character(grp),
      line = list(dash = dash_type),
      hoverinfo = "text",
      text = ~paste(
        "Plate:", plateid,
        "<br>Sample Dilution Factor:", sample_dilution_factor,
        "<br> Model Type:", model
      )
    )
  }


  agg_dash_type  <- model_linetype[as.character(aggregated_fit$mod_class[1])]
  p <- add_lines(
    p,
    data = aggregated_fit,
    x = ~predicted_concentration,
    y = ~yhat,
    name = paste("Aggregated Fit", unique(aggregated_fit$mod_class)),
    line = list(
      dash = agg_dash_type,
      width = 4,
      color = "black"
    ),
    hoverinfo = "text",
    text = ~paste(
      "Aggregated Standard Curve",
      "<br>Model:", mod_class
    ),
    showlegend = TRUE
  )

  p <- p %>%
    add_trace(
      data = cv_df_antigen,
      x = ~concentration,  # or log10(dilution) if needed
      y = ~cv_assay_response_pct_at_dilution,
      type = "scatter",
      mode = "markers",
      marker = list(color = "#8C70FF", size = 8),
      yaxis = "y2",
      name = "Coefficient of Variation",
      hoverinfo = "text",
      text = ~paste0(
        "<br>Dilution Factor: ", dilution,
        "<br> Concentration: ", round(concentration, 2),
        "<br>CV (%): ", round(cv_assay_response_pct_at_dilution, 2)
      )
    )

  p <- p %>%
    add_trace(
      data = cv_df_antigen,
      x = ~concentration,
      y = rep(NA, nrow(cv_df_antigen)),
      type = "scatter",
      mode = "lines",
      xaxis = "x2",
      showlegend = FALSE,
      hoverinfo = "none",
      line = list(color = "rgba(0,0,0,0)")
    )

  x_range_raw <- range(
    antigen_source_exp_fits$predicted_concentration,
    na.rm = TRUE
  )
  pad <- diff(x_range_raw) * 0.05  # 5% padding

  x_range <- c(
    x_range_raw[1] - pad,
    x_range_raw[2] + pad
  )

  ax <- list(
    overlaying = "x",
    side = "top",
    title = "Dilution Factor",
    tickmode = "array",
    tickvals = cv_df_antigen$concentration,
    ticktext = cv_df_antigen$dilution,
    fixedrange = TRUE,
    range = x_range,
    tickfont = list(color = "#8db600")
  )



  p <- layout(
    p,
    xaxis = list(
      title = x_axis_label,
      fixedrange = TRUE,
      range = x_range
    ),
    xaxis2 = ax,
    yaxis = list(
      title = y_axis_label
    ),
    yaxis2 = list(
      title = "Coefficient of Variation (%)",
      overlaying = "y",
      side = "right",
      showgrid = FALSE,
      zeroline = FALSE,
      tickfont = list(color = "#8C70FF")
    ),
    title = list(
      text = paste("Standard Curves for", antigen, "by Plate and Model Class"),
      y = 1.15,
      yref = "paper",
      x = 0.5
      # xanchor = "center"
    ),
    legend = list(title = list(text = "Plate", x = 1.2, y = 1)),
    font = list(size = 12),
    margin = list(t = 150, b = 80),
    autosize = T
  )





  return(p)
}
