
get_plot_data <- function(models_fit_list,
                          prepped_data,
                          fit_params,
                          fixed_a_result,
                          model_names = c("Y5","Yd5","Y4","Yd4","Ygomp4"),
                          x_var = "concentration",
                          y_var = "mfi",
                          verbose = TRUE) {
  # Extract data
  dat <- prepped_data
  dat$x <- dat[[x_var]]
  dat$y <- dat[[y_var]]
  x <- dat[[x_var]]
  y <- dat[[y_var]]

  ## 1. Build prediction data for all models
  # A fine grid over the range of x for smooth curves
  x_new <- seq(min(x, na.rm = TRUE),
               max(x, na.rm = TRUE),
               length.out = 200)

  pred_list <- list()
  resid_list <- list()
  d2xy_list <- list()
  dydx_list <- list()

  for (mname in model_names) {
    entry <- models_fit_list[[mname]]
    fit_obj <- if (is.list(entry)) entry$fit else entry

    if (is.null(fit_obj) || !inherits(fit_obj, "nls")) {
      next
    }

    # prediction data.frame must have same column name as x_var
    newdata <- data.frame(x_new)
    names(newdata) <- x_var

    # Predictions + residuals
    y_pred <- tryCatch({
      predict(fit_obj, newdata = newdata)
    }, error = function(e) rep(NA_real_, length(x_new)))

    if (is.null(fixed_a_result)) {
      fit_obj_coef <- coef(fit_obj)
    } else {
      fit_obj_coef <- c(a = fixed_a_result, coef(fit_obj))
    }

    d2x_y <- tryCatch({
      if (mname == "Y5") {
        do.call(d2xY5, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Y4") {
        do.call(d2xY4, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Yd5") {
        do.call(d2xYd5, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Yd4") {
        do.call(d2xYd4, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Ygomp4") {
        do.call(d2xYgomp4, c(list(x = x_new), fit_obj_coef))
      }
    }, error = function(e) rep (NA_real_, length(x_new)))

    dydx <- tryCatch({
      if (mname == "Y5") {
        do.call(dydxY5, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Y4") {
        do.call(dydxY4, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Yd5") {
        do.call(dydxYd5, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Yd4") {
        do.call(dydxYd4, c(list(x = x_new), fit_obj_coef))
      } else if (mname == "Ygomp4") {
        do.call(dydxYgomp4, c(list(x = x_new), fit_obj_coef))
      }
    }, error = function(e) rep (NA_real_, length(x_new)))

    y_fit <- tryCatch({
      fitted(fit_obj)
    }, error = function(e) rep(NA_real_, length(x)))

    resid <- tryCatch({
      residuals(fit_obj)
    }, error = function(e) rep(NA_real_, length(x)))

    pred_list[[mname]] <- data.frame(
      model = mname,
      x     = x_new,
      yhat  = y_pred
    )

    resid_list[[mname]] <- data.frame(
      model     = mname,
      fitted    = y_fit,
      residuals = resid
    )

    d2xy_list[[mname]] <- data.frame(
      model = mname,
      x = x_new,
      d2x_y = d2x_y
    )

    dydx_list[[mname]] <- data.frame(
      model = mname,
      x = x_new,
      dydx = dydx
    )
  }


  pred_df  <- do.call(rbind, pred_list)
  pred_df$yhat <- as.numeric(pred_df$yhat)
  resid_df <- do.call(rbind, resid_list)
  d2xy_df <- do.call(rbind, d2xy_list)
  dydx_df <- do.call(rbind, dydx_list)


  fit_summary <- summarize_model_fits(models_fit_list, model_names)
  fit_summary_long <- reshape2::melt(
    fit_summary,
    id.vars = c("model", "converged"),
    measure.vars = c("AIC"),
    variable.name = "criterion",
    value.name = "value"
  )
  fit_summary_long <- subset(fit_summary_long, converged & is.finite(value))
  names(fit_summary_long)[names(fit_summary_long) == "criterion"] <- "parameter"
  names(fit_summary_long)[names(fit_summary_long) == "value"] <- "estimate"
  fit_summary_long$conf.low <- fit_summary_long$estimate
  fit_summary_long$conf.high <- fit_summary_long$estimate

  fit_params_aic <- rbind(fit_summary_long, fit_params)

  if (verbose) {
    message("Plot Data Completed")
  }
  return(list(dat = dat, pred_df = pred_df,
              resid_df = resid_df,
              fit_params = fit_params,
              d2xy_df = d2xy_df,
              dydx_df = dydx_df,
              fit_params_aic = fit_params_aic))
}

plot_model_comparisons <- function(plot_data,
                                   model_names = c("Y5","Yd5","Y4","Yd4","Ygomp4"),
                                   x_var = "concentration",
                                   y_var = "mfi",
                                   use_patchwork = TRUE) {

  ## 1.Extract data
  pred_df <- plot_data$pred_df
  resid_df <- plot_data$resid_df
  fit_params_df <- plot_data$fit_params
  fit_params_aic <- plot_data$fit_params_aic
  dat <- plot_data$dat
  x <- dat[[x_var]]
  y <- dat[[y_var]]


  ## 2. Plot: data + fitted curves
  p_data_fit <- ggplot(dat[ , c("x","y")], aes(x = x, y = y)) +
    geom_point(alpha = 0.7) +
    geom_line(data = pred_df, aes(x = x, y = yhat, color = model, group = model)) +
    labs(title = "Observed data with fitted curves",
         x = x_var,
         y = y_var,
         color = "Model") +
    theme_bw()

  ## 3. Residual vs fitted
  p_resid <- ggplot(resid_df,
                    aes(x = fitted, y = residuals, color = model)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(alpha = 0.6) +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values",
         y = "Residuals",
         color = "Model") +
    theme_bw()

  # ## 4. QQ plot of residuals
  # # Make a combined QQ plot by model (facets)
  # p_qq <- ggplot(resid_df, aes(sample = residuals)) +
  #   stat_qq(alpha = 0.6) +
  #   stat_qq_line() +
  #   facet_wrap(~ model, scales = "free") +
  #   labs(title = "Normal Q-Q plots of residuals") +
  #   theme_bw()



  p_ci_params <- ggplot(fit_params_aic) +
    geom_point(aes(as.factor(model), estimate), color = "black") +
    facet_wrap(~ parameter, scale = 'free_x', ncol = 6) +
    geom_linerange(aes(as.factor(model), ymin = conf.low, ymax = conf.high)) +
    coord_flip() +
    # scale_color_manual(values = c('green4', 'black')) +
    theme_bw(base_size = 12) +
    theme(legend.position = 'top') +
    xlab('Model') +
    ylab('Parameter Estimate')

  ## 5. Optional: AIC/BIC barplot
  # fit_summary <- summarize_model_fits(models_fit_list, model_names)
  # fit_summary_long <- reshape2::melt(
  #   fit_summary,
  #   id.vars = c("model", "converged"),
  #   measure.vars = c("AIC", "BIC"),
  #   variable.name = "criterion",
  #   value.name = "value"
  # )

  # p_info <- ggplot(
  #   subset(fit_summary_long, converged & is.finite(value)),
  #   aes(x = model, y = value, fill = criterion)
  # ) +
  #   geom_col(position = position_dodge(width = 0.7)) +
  #   labs(title = "Information criteria (lower is better)",
  #        x = "Model",
  #        y = "Criterion value") +
  #   theme_bw()

  if (!use_patchwork) {
    # Return a list of plots so caller can arrange as desired
    return(list(
      data_fit = p_data_fit,
      resid    = p_resid,
      p_ci_params = p_ci_params
      # info     = p_info
    ))
  }

  # Use patchwork for a single panel display
  # install.packages("patchwork") if needed
  library(patchwork)

  # Arrange: top row = data+fits, info; bottom row = resid, qq
  combined <- (p_data_fit) /
    (p_resid)/
    (p_ci_params)

  combined
}


# format terms such as mfi and concentration
format_assay_terms <- function(x) {
  lookup <- c(
    MFI = "MFI",
    Concentration = "Concentration"
  )

  # normalize lookup keys for matching
  names(lookup) <- tolower(names(lookup))

  sapply(x, function(v) {
    key <- tolower(v)
    if (key %in% names(lookup)) lookup[[key]] else v
  }, USE.NAMES = FALSE)
}

# best fit must contain sample_se
plot_standard_curve <- function(best_fit,
                                is_display_log_response,
                                is_display_log_independent,
                                pcov_threshold,
                                response_variable = "mfi",
                                independent_variable = "concentration") {
  p <- plotly::plot_ly()

  samples_predicted_conc <- best_fit$sample_se
  samples_predicted_conc <- samples_predicted_conc[!is.nan(samples_predicted_conc$predicted_concentration),]
  best_fit$best_pred$pcov_threshold <- pcov_threshold
  ### -------------------------
  ### 1. RESPONSE VARIABLE (Y)
  ### -------------------------
  log_response_status <- best_fit$best_glance$is_log_response

  if (log_response_status && !is_display_log_response) {
    best_fit$best_data[[response_variable]] <- 10^best_fit$best_data[[response_variable]]
    best_fit$best_pred$yhat               <- 10^best_fit$best_pred$yhat
    best_fit$best_glance$llod             <- 10^best_fit$best_glance$llod
    best_fit$best_glance$ulod             <- 10^best_fit$best_glance$ulod
    best_fit$best_glance$lloq_y           <- 10^best_fit$best_glance$lloq_y
    best_fit$best_glance$uloq_y           <- 10^best_fit$best_glance$uloq_y
    best_fit$best_glance$inflect_y        <- 10^best_fit$best_glance$inflect_y
    best_fit$best_d2xy$d2x_y              <- 10^best_fit$best_d2xy$d2x_y

    samples_predicted_conc[[response_variable]] <- 10^samples_predicted_conc[[response_variable]]
  }

  ### -----------------------------
  ### 2. INDEPENDENT VARIABLE (X)
  ### -----------------------------
  log_independent_variable_status <- best_fit$best_glance$is_log_x

  if (log_independent_variable_status && !is_display_log_independent) {
    best_fit$best_data$concentration              <- 10^best_fit$best_data$concentration
    best_fit$best_pred$x                          <- 10^best_fit$best_pred$x
    best_fit$best_glance$lloq                     <- 10^best_fit$best_glance$lloq
    best_fit$best_glance$uloq                     <- 10^best_fit$best_glance$uloq
    best_fit$best_glance$inflect_x                <- 10^best_fit$best_glance$inflect_x
    best_fit$best_d2xy$x                          <- 10^best_fit$best_d2xy$x
    samples_predicted_conc$predicted_concentration <- 10^samples_predicted_conc$predicted_concentration
  }
  y3_label <- paste(stringr::str_to_title(independent_variable), "Uncertainty (pCoV)")
  if (is_display_log_response) {
    y_label <- paste("log<sub>10</sub>", format_assay_terms(response_variable))

  } else {
    y_label <- format_assay_terms(response_variable)
  }

  if (is_display_log_independent) {
    x_label <- paste("log<sub>10</sub>", format_assay_terms(independent_variable))
  } else {
    x_label <- format_assay_terms(independent_variable)
  }
  ### ------------------------
  ### 3. MODEL NAME
  ### ------------------------
  model_name <- best_fit$best_model_name
  title_model_name <- switch(
    model_name,
    "Y4" = "4-parameter Logistic",
    "Yd4" = "4-parameter Log-Logistic",
    "Ygomp4" = "4-parameter Gompertz type",
    "Y5" = "5-parameter Logistic",
    "Yd5" = "5-parameter Log-Logistic",
    model_name
  )

  ## -----------------------------------------------------------------
  ## 3b.  PREPARE SAMPLE‑UNCERTAINTY (single scaling) -------------------
  ## -----------------------------------------------------------------
  ##   • Convert everything to log10(SE) *once*.
  ##   • Combine model‑derived SE and sample‑specific SE to get a
  ##     common axis range.
  ## -----------------------------------------------------------------
  print(names(best_fit$best_pred))
  se_model   <- best_fit$best_pred$se_concentration
  se_samples <- samples_predicted_conc$se_concentration
  se_all <- c(best_fit$best_pred$pcov,  samples_predicted_conc$pcov)
  se_range      <- range(se_all, na.rm = TRUE)

  # se_max <- min(ceiling(quantile(se_samples ^ 2, probs = 0.95, na.rm = TRUE) * 1000), 100)


  # se_margin <- (se_max - 0) * 0.02
  # # se_margin <- (100 - se_range[1]) * 0.05
  # se_axis_limits <- c(1 - se_margin, se_max + se_margin)
  # # se_axis_limits <- c(se_range[1] - se_margin, 100 + se_margin)

  se_max <- min(ceiling(quantile(se_samples ^ 2, probs = 0.95, na.rm = TRUE) * 1000), 100)
  se_min <- max(min(se_all, na.rm = TRUE), 0.1)  # Ensure positive minimum for log scale

  # For log scale, work with the actual values (Plotly will handle the log transform)
  se_axis_limits <- c(se_min * 0.8, se_max * 1.2)  # Add some padding
  dtick <- ifelse(se_max > 19, ifelse(se_max > 35,10, 5), 1)
  # dtick <- 0.301

  ### ------------------------
  ### 4. RAW POINTS (standards + blanks)
  ### ------------------------
  p <- p %>% add_trace(
    data = best_fit$best_data,
    x = best_fit$best_data[[independent_variable]],
    y = best_fit$best_data[[response_variable]],
    type = "scatter",
    mode = "markers",
    name = ~ifelse(stype == "S", "Standards",
                   ifelse(stype == "B", "Geometric Mean of Blanks", stype)),
    color = ~stype,
    colors = c("B" = "#1f77b4", "S" = "black"),
    text = ~paste0(
      "<br>", independent_variable, ": ", best_fit$best_data[[independent_variable]],
      "<br>Dilution Factor: ", dilution,
      "<br>", response_variable, ": ", best_fit$best_data[[response_variable]]
    ),
    hoverinfo = "text"
  )

  ### ------------------------
  ### 5. FITTED CURVE
  ### ------------------------
  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_pred$yhat,
    name = "Fitted Curve",
    line = list(color = "blue")
  )

  ### ------------------------
  ### 6. LOD lines (horizontal)
  ### ------------------------
  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_glance$ulod,
    name = paste("Upper LOD:", round(best_fit$best_glance$ulod, 3)),
    line = list(color = "orangered", dash = "dash")
  )

  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_glance$llod,
    name = paste("Lower LOD:", round(best_fit$best_glance$llod, 3)),
    line = list(color = "orangered", dash = "dash")
  )

  ### ------------------------
  ### 7. LOQ (vertical + horizontal)
  ### ------------------------
  y_min <- min(best_fit$best_data[[response_variable]], na.rm = TRUE)
  y_max <- max(best_fit$best_data[[response_variable]], na.rm = TRUE)

  ### LLOQ – vertical line
  p <- p %>% add_lines(
    x = c(best_fit$best_glance$lloq),
    y = c(y_min, y_max),
    name = paste(
      "Lower LOQ: (",
      round(best_fit$best_glance$lloq, 3), ",",
      round(best_fit$best_glance$lloq_y, 3), ")"
    ),
    line = list(color = "purple"),
    legendgroup = "linked_lloq",
    hoverinfo = "text"
  )

  ### ULOQ – vertical line
  p <- p %>% add_lines(
    x = c(best_fit$best_glance$uloq),
    y = c(y_min, y_max),
    name = paste(
      "Upper LOQ: (",
      round(best_fit$best_glance$uloq, 3), ",",
      round(best_fit$best_glance$uloq_y, 3), ")"
    ),
    line = list(color = "purple"),
    legendgroup = "linked_uloq",
    hoverinfo = "text"
  )

  ### Horizontal LOQ lines
  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_glance$uloq_y,
    name = "",
    legendgroup = "linked_uloq", showlegend = FALSE,
    line = list(color = "purple")
  )

  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_glance$lloq_y,
    name = "",
    legendgroup = "linked_lloq", showlegend = FALSE,
    line = list(color = "purple")
  )


  ### ------------------------
  ### 8a. SECOND DERIVATIVE (y2 axis)
  ### ------------------------
  p <- p %>% add_lines(
    x = best_fit$best_d2xy$x,
    y = best_fit$best_d2xy$d2x_y,
    name = "2nd Derivative of x given y",
    yaxis = "y2",
    line = list(color = "#f3c300"),
    visible = "legendonly"
  )

  ### ------------------------
  ### 8b. Sample uncertainty (y3 axis)
  ### ------------------------
  unc_col <- list(color = "orange")
  p <- p %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_pred$pcov,
    name = "Measurement Uncertainty",
    yaxis = "y3",
    line = unc_col,
    legendgroup = "linked_uncertainty"
  ) %>% add_trace(
    data = samples_predicted_conc,
    x = ~predicted_concentration,
    y = ~pcov,
    type = "scatter",
    mode = "markers",
    name = "",                         ## no extra legend entry
    marker = list(color = "red", symbol = "circle"),
    text = ~paste("Predicted", x_label, ":", predicted_concentration,
                  "<br>Coefficient of Variation (pCoV):", round(pcov,2), "%"),
    yaxis = "y3",
    legendgroup = "linked_uncertainty",
    showlegend = FALSE,
    hovertemplate = "%{text}<extra></extra>"
  ) %>% add_lines(
    x = best_fit$best_pred$x,
    y = best_fit$best_pred$pcov_threshold,
    name = paste0("pCoV Threshold: ",best_fit$best_pred$pcov_threshold,"%"),
    yaxis = "y3",
    line = list(color = "orange", dash = "dash"),
    legendgroup = "linked_uncertainty"
  )

  ### ------------------------
  ### 9a. SAMPLES
  ### ------------------------
  p <- p %>% add_trace(
    data = samples_predicted_conc,
    x = ~predicted_concentration,
    y = samples_predicted_conc[[response_variable]],
    type = "scatter",
    mode = "markers",
    name = "Samples",
    marker = list(color = "green", symbol = "circle"),
    text = ~paste("Predicted", x_label, ":", predicted_concentration,
                  "<br>",y_label , ":", samples_predicted_conc[[response_variable]],
                  "<br>Patient ID:", patientid,
                  "<br>Well:", well,
                  "<br>LOQ Gate Class:", samples_predicted_conc$gate_class_loq,
                  "<br>LOD Gate Class:", samples_predicted_conc$gate_class_lod,
                  "<br> PCOV Gate Class:",samples_predicted_conc$gate_class_pcov ),
    #"<br>Timeperiod:", timeperiod),
    hovertemplate = "%{text}<extra></extra>"
  )

  ### ------------------------
  ### 10. INFLECTION POINT
  ### ------------------------
  p <- p %>% add_trace(
    x = best_fit$best_glance$inflect_x,
    y = best_fit$best_glance$inflect_y,
    type = "scatter",
    mode = "markers",
    name = paste(
      "Inflection Point: (",
      round(best_fit$best_glance$inflect_x, 3), ",",
      round(best_fit$best_glance$inflect_y, 3), ")"
    ),
    marker = list(color = "#e377c2", size = 8)
  )

  ### ------------------------
  ### 11. LAYOUT
  ### ------------------------
  p <- p %>% layout(
    title = paste(
      "Fitted", title_model_name, "Model (",
      unique(best_fit$best_data$plate), ",",
      unique(best_fit$best_data$antigen), ")"
    ),
    xaxis = list(title = x_label,
                 showgrid = TRUE,
                 zeroline = FALSE),
    yaxis = list(title = y_label,
                 showgrid = TRUE,
                 zeroline = TRUE),
    legend = list(x = 1.1,
                  y = 1,
                  xanchor = "left"),
    font = list(size = 12),

    yaxis2 = list(
      showticklabels = FALSE,
      title = "",
      tickmode = "linear",
      dtick = 10,
      overlaying = "y",
      side = "right",
      showgrid = FALSE,
      zeroline = FALSE
    ),

    yaxis3 = list(
      overlaying = "y",
      side = "right",
      title = y3_label,
      range = se_axis_limits,
      tickmode = "linear",
      type = "linear",
      # range = log10(se_axis_limits),
      dtick = dtick,
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = TRUE
    )
  )

  return(p)
}

glance_plot_data <- function(best_glance_all,
                             best_plate_all,
                             verbose = TRUE) {
  datf <- dplyr::inner_join(best_glance_all[ , c("study_accession", "experiment_accession","antigen","plateid","source","is_log_x","status","crit","lloq","uloq","inflect_x","dydx_inflect")],
                            best_plate_all[, c("feature","plateid","plate","sample_dilution_factor","assay_response_variable","assay_independent_variable")], by = "plateid"
  )
  datf$lloq <- ifelse(datf$is_log_x, 10^datf$lloq, datf$lloq)
  datf$uloq <- ifelse(datf$is_log_x, 10^datf$uloq, datf$uloq)
  datf$inflect_x <- ifelse(datf$is_log_x, 10^datf$inflect_x, datf$inflect_x)
  datf$lloq <- ifelse(is.na(datf$lloq), 0, datf$lloq)
  datf$uloq <- ifelse(is.na(datf$uloq), 2* datf$inflect_x, datf$uloq)
  datf$feature <- ifelse(datf$experiment_accession == "ADCD", paste0(datf$experiment_accession, "_", datf$sample_dilution_factor, "x"), datf$feature)
  datf$feature <- factor(datf$feature)
  datf$plate <- factor(datf$plate)
  datf$experiment_accession <- factor(datf$experiment_accession)

  datf$source <- factor(datf$source)
  datf$sample_dilution_factor <- factor(datf$sample_dilution_factor)
  datf$antigen <- factor(datf$antigen)
  datf$status <- factor(datf$status)
  datf$crit <- factor(datf$crit)
  return(datf)
}


make_feature_plot <- function(datf, feature_name) {

  df <- datf %>%
    filter(feature == feature_name)

  # Define hover text
  df$hover <- paste0(
    "experiment_accession: ", df$experiment_accession, "<br>",
    "feature: ", df$feature, "<br>",
    "antigen: ", df$antigen, "<br>",
    "plate: ", df$plate, "<br>",
    "source: ", df$source, "<br>",
    "sample_dilution_factor: ", df$sample_dilution_factor, "<br>",
    "best_fit_model: ", df$crit, "<br>",
    "range of quantification: ", round(df$lloq,2), "-", round(df$uloq,2),  "<br>",
    "sensitivity (slope @inflection): ", round(df$dydx_inflect,2)
  )

  p <- plotly::plot_ly()

  # Add horizontal line segments
  antigens <- unique(df$antigen)
  pal <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(antigens))
  names(pal) <- antigens

  p <- p %>% add_segments(
    data = df,
    x = ~lloq,
    xend = ~uloq,
    y = ~dydx_inflect,
    yend = ~dydx_inflect,
    color = ~antigen,
    colors = pal,
    text = ~hover,
    hoverinfo = "text",
    line = list(width = 3)
  )

  # Layout for Nature‑style clarity
  p <- p %>% layout(
    title = list(
      title = paste(
        "Feature:", feature_name
      )
    ),
    xaxis = list(
      title = "Concentration",
      type = "log",
      tickvals = c(1,10,100,1000,10000,100000),
      ticktext = c("1","10","100","1e3","1e4","1e5"),
      showgrid = TRUE,
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Sensitivity (slope at inflection)",
      type = "log",
      showgrid = TRUE,
      zeroline = FALSE
    ),
    legend = list(
      title = list(text = "Antigen"),
      orientation = "v"
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  )

  return(p)
}

