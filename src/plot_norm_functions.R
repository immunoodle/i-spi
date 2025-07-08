
# Functions to populate data frames
produce_buffer_data <- function(stored_plates_data){
  norm_plate_data <- data.frame()
  buffers <- stored_plates_data$stored_buffer[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution","feature", "plateid")]
  norm_plate_data <- buffers

  return(norm_plate_data)
}

produce_stand_data <- function(stored_plates_data){
  norm_plate_data <- data.frame()
  standards <- stored_plates_data$stored_standard[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  norm_plate_data <- standards
  return(norm_plate_data)
}

produce_buffer_standards_data <- function(stored_plates_data) {
  norm_plate_data <- data.frame()
  buffers <- stored_plates_data$stored_buffer[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  standards <- stored_plates_data$stored_standard[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  norm_plate_data <- rbind(buffers, standards)
  return(norm_plate_data)
}

produce_controls_standards_data <- function(stored_plates_data) {
  norm_plate_data <- data.frame()
  controls <- stored_plates_data$stored_control[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  standards <- stored_plates_data$stored_standard[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  norm_plate_data <- rbind(controls, standards)

  return(norm_plate_data)
}

produce_buffer_controls_data <- function(stored_plates_data){
  norm_plate_data <- data.frame()
  buffers <- stored_plates_data$stored_buffer[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  controls <- stored_plates_data$stored_control[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  norm_plate_data <- rbind(buffers, controls)

  return(norm_plate_data)
}

produce_buffer_controls_standards_data <- function(stored_plates_data) {
  norm_plate_data <- data.frame()
  buffers <- stored_plates_data$stored_buffer[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  controls <- stored_plates_data$stored_control[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  standards <- stored_plates_data$stored_standard[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  norm_plate_data <- rbind(buffers, controls, standards)
  return(norm_plate_data)
}

produce_controls_data <- function(stored_plates_data){
  norm_plate_data <- data.frame()
  norm_plate_data <- stored_plates_data$stored_control[ , c("study_accession", "experiment_accession", "antigen", "mfi", "n", "dilution", "feature", "plateid")]
  return(norm_plate_data)
}

# function for loading plate data
load_norm_plate_data <- function(stored_plates_data, normset_str = NULL, selected_antigen = NULL, selected_feature = NULL){
  # If normset_str is null return message
  if (is.null(normset_str)){
    errorMessage <- "A combination of buffers, standards, and controls must be selected"
    return(errorMessage)
  }


  if (normset_str == "standards"){
    norm_plate_data <- produce_stand_data(stored_plates_data)
  } else if (normset_str == "buffers"){
    norm_plate_data <- produce_buffer_data(stored_plates_data)
  } else if (normset_str == "buffers, standards") {
    norm_plate_data <- produce_buffer_standards_data(stored_plates_data)
  } else if (normset_str == "controls, standards"){
    norm_plate_data <- produce_controls_standards_data(stored_plates_data)
  } else if (normset_str == "buffers, controls, standards") {
    norm_plate_data <- produce_buffer_controls_standards_data(stored_plates_data)
  }

  # select antigen and feature from input
  antigen <- selected_antigen
  feature <- selected_feature

  # subset data based on antigen and feature
  norm_plate_data <- norm_plate_data[norm_plate_data$antigen == selected_antigen & norm_plate_data$feature == selected_feature, ]
  norm_plate_data$raw_value <- norm_plate_data$mfi
  norm_plate_data$plateid <- as.factor(norm_plate_data$plateid)
  norm_plate_data <- as.data.frame(norm_plate_data)

  return(norm_plate_data)
}

# function for loading sample data
load_sample_data <- function(stored_plates_data, selected_antigen, selected_feature){

  sample_data <- stored_plates_data$stored_sample
  sample_data$selected_str <- paste0(sample_data$study_accession,sample_data$experiment_accession)
  names(sample_data)[names(sample_data) == "xmap_sample_id"] <- "tid"
  sample_data <- sample_data[sample_data$antigen == selected_antigen & sample_data$feature== selected_feature, ]
  sample_data$raw_value <- sample_data$mfi
  sample_data$plateid <- as.factor(sample_data$plateid)


  sample_data <- as.data.frame(sample_data[ ,
                                            c("study_accession", "experiment_accession", "tid", "plateid", "dilution", "antigen", "feature", "raw_value")]
  )
  return(sample_data)
}

# This function fits a cgam, gam, polynomial or linear model to the plate data
fit_models <- function(norm_plate_data){
  fitted_models <- list()
  model_types <- c()
  fit_status <- c()

  for(plate in unique(norm_plate_data$plateid)){
    norm_plate_data_subset <- norm_plate_data[norm_plate_data$plateid == plate,]

    # ### prozone fix
    # datlstda <- norm_plate_data_subset
    # max_raw_value <- max(datlstda$raw_value)
    # logd_at_max_raw_value <- max(datlstda[datlstda$raw_value==max_raw_value, ]$log_dilution)
    # # 2. identify the raw_values lower than the max_raw_value at higher concentrations
    # datlstda[datlstda$log_dilution > logd_at_max_raw_value, ]$raw_value <- max_raw_value + ((max_raw_value -
    #                                                                          datlstda[datlstda$log_dilution > logd_at_max_raw_value, ]$raw_value)*0.02 /
    #                                                                         ((datlstda[datlstda$log_dilution > logd_at_max_raw_value, ]$log_dilution-logd_at_max_raw_value)*2))
    # norm_plate_data_subset <- datlstda

    # # fit a cgam model
    # tryCatch({
    #   cgam_model <-cgam(log10(normal_reference) ~ s.incr(log10(raw_value),
    #                                                      numknots = 4,
    #                                                      knots = 0, var.knots = 0,
    #                                                      space = "Q", db.exp = T),
    #                                                     data = norm_plate_data_subset,
    #                                                     cic = FALSE, nsim = 100)
    #   # cgam_model <-cgam(normal_reference ~ s.incr(raw_value, numknots = 4, knots = 0, var.knots = 0, space = "Q", db.exp = T),
    #   #                   data = norm_plate_data_subset, cic = FALSE, nsim = 100)
    #   # Glance info
    #   cgam_summary <- summary(cgam_model)
    #   # check deviance
    #   if (cgam_summary$deviance > 0){
    #     fitted_models[[plate]] <- list(status = paste("Plate:",plate,"fit: cgam"), type = "cgam", model = cgam_model, summary = cgam_summary)
    #     model_types <- c(model_types, "cgam")
    #     fit_status <- c(fit_status, paste("Plate:",plate,"fit: cgam"))
    #     # cat("CGAM fitted for plate ", plate, "\n")
    #   }else{
    #     # cat("CGAM deviance is 0 for plate ", plate, "\n")
    #     fitted_models[[plate]] <- NULL
    #   }
    # }, error = function(e) {
    #   fitted_models[[plate]] <- NULL
    #   cat("CGAM failed for plate", plate, "\n")
    # }
    # )

    # Try fitting a GAM model
    if (is.null(fitted_models[[plate]])) {
      tryCatch({
        gam_model <- gam(log10(normal_reference) ~ s(log10(raw_value)), data = norm_plate_data_subset)
        gam_glance <- broom::glance(gam_model)
        # check deviance
        if(gam_glance$deviance > 0){
          fitted_models[[plate]] <- list(status = paste("Plate:",plate,"fit: gam"), type = "gam", model = gam_model, glance = gam_glance)
          model_types <- c(model_types,"gam")
          fit_status <- c(fit_status, paste("Plate:",plate,"fit: gam"))
        } else{
          # cat("GAM deviance is 0 for plate ", plate, "\n")
          fitted_models[[plate]] <- NULL
        }
      }, error = function(e){
        fitted_models[[plate]] <- NULL
        cat("GAM failed for plate ", plate, "\n")
      })
    }

    # Try fitting linear model if CGAM or GAM fails
    if (is.null(fitted_models[[plate]])){
      tryCatch({
        lm_poly_model <- lm(log10(normal_reference) ~ poly(log10(raw_value), 3, raw = TRUE), data = norm_plate_data_subset)
        # lm_poly_model <- lm(normal_reference ~ poly(raw_value, 3, raw = TRUE), data = norm_plate_data_subset)
        lm_glance <- broom::glance(lm_poly_model)
        # check deviance
        #  if(lm_glance$deviance > 0){
        fitted_models[[plate]] <- list(status = paste("Plate:",plate,"fit: polynomial"), type = "lm_poly", model = lm_poly_model, glance = lm_glance)
        model_types <- c(model_types, "lm_poly")
        fit_status <- c(fit_status, paste("Plate:",plate,"fit: polynomial"))
        # cat("LM Poly fitted for plate ", plate, "\n")
        # } else{
        # cat("Polynomial Linear deviance is 0 for plate ", plate)
        # }
      }
      , error = function(e){
        fitted_models[[plate]] <- NULL
        cat("Polynomial Model failed for plate", plate)
      })
    }

    # Try fitting a linear model if poly linear model fails
    if(is.null(fitted_models[[plate]])) {
      tryCatch({
        lm_model <-  lm(log10(normal_reference) ~ log10(raw_value), data = norm_plate_data_subset)
        # lm_model <-  lm(normal_reference ~ raw_value, data = norm_plate_data_subset)
        lm_glance <- broom:glance(lm_model)
        fitted_models[[plate]] <- list(status = paste("Plate:",plate,"fit: linear"), type = "lm", model = lm_model, glance = lm_glance)
        model_types <- c(model_types, "lm")
        fit_status <- c(fit_status, paste("Plate:",plate,"fit: linear"))
      }, error = function(e){
        fitted_models[[plate]] <- NULL
        cat("Linear Model failed for plate", plate)
      })
    }
  } # end for loop

  return(list(models = fitted_models, model_types = model_types, fit_status = fit_status))

}

# Predictions on data
predict_plate <- function(in_data, model, modtype){
  in_data <- as.data.frame(in_data)
  plate_id <- unique(in_data$plateid)
  # print(plate_id)
  #
  # cat("In Data")
  # print(class(in_data))
  # print(nrow(in_data))
  # print(names(in_data))
  #
  # cat("Model")
  # print(model)

  # cat("Predictions: \n")
  if (modtype=="cgam") {
    predictions <- predict(model, in_data, interval = "none")
    # print(class(predictions))
    # print(predictions)
    plate_fitted <- as.data.frame(cbind(in_data, predictions$fit))
    names(plate_fitted)[names(plate_fitted) == "predictions$fit"] <- "log_normalized_value"
    # names(plate_fitted)[names(plate_fitted) == "predictions$fit"] <- "normalized_value"
  } else {
    predictions <- predict(model, in_data, interval = "none")
    # print(class(predictions))
    # print(predictions)
    predictions <- as.data.frame(predictions)
    plate_fitted <- as.data.frame(cbind(in_data, predictions[, c(1)]))
    names(plate_fitted)[names(plate_fitted) == "predictions[, c(1)]"] <- "log_normalized_value"
    # names(plate_fitted)[names(plate_fitted) == "predictions[, c(1)]"] <- "normalized_value"
  }
  # cat("Plate Fitted\n")
  plate_fitted$normalized_value <- exp10(plate_fitted$log_normalized_value)
  # plate_fitted$log_normalized_value <- log10(plate_fitted$normalized_value)
  # cat("plate fitted after creation")
  # print(names(plate_fitted))
  # print(class(plate_fitted))
  # print(dim(plate_fitted))
  return(plate_fitted)
}

# function to predict fitted values
predict_fitted <- function(setdata, fitted_models){
  plate_list <- unique(setdata$plateid)
  # print(plate_list)
  # empty dataframe to store results
  norm_plate_data_fitted <- data.frame()

  for (platid in 1:length(plate_list)){
    # print(platid)
    model <- fitted_models[platid][[1]]$model
    modtype <- fitted_models[platid][[1]]$type
    in_data <- setdata[setdata$plateid == plate_list[platid], ]
    result <- predict_plate(in_data = in_data, model = model, modtype = modtype)
    # print(nrow(result))
    norm_plate_data_fitted <- rbind(norm_plate_data_fitted,result)
  }
  # cat("plate data_fitted after creation")
  # print(names(norm_plate_data_fitted))
  # print(class(norm_plate_data_fitted))
  # print(dim(norm_plate_data_fitted))
  return(norm_plate_data_fitted)
}

make_scatter_plotly <- function(plot_ndat=NULL,
                                xvar = NULL,
                                yvar = NULL,
                                byvar = NULL,
                                idvar = NULL,
                                subjectid_marker_prefix = "Subject:",
                                xvar_marker_prefix = "x value",
                                yvar_marker_prefix = "y value",
                                xlabel_txt = "x value",
                                ylabel_txt = "y value",
                                scaling = c("linear","log")
) {
  plot_ndat <- plot_ndat[ , c(idvar,byvar,xvar,yvar)]
  names(plot_ndat)[names(plot_ndat) == xvar] <- "xvar"
  names(plot_ndat)[names(plot_ndat) == yvar] <- "yvar"
  names(plot_ndat)[names(plot_ndat) == byvar] <- "byvar"
  names(plot_ndat)[names(plot_ndat) == idvar] <- "SubjectID"
  plot_ndat <- plot_ndat[order(plot_ndat$byvar,plot_ndat$xvar), ]
  ncolors_needed <- length(unique(plot_ndat$byvar))
  if (ncolors_needed < 37) {
    ## Create a color palette for up to 36 different plates.
    color_normalize <- as.vector(palette36.colors(n = ncolors_needed))
  } else {
    ## Create a color palette for more than 36 different plates.
    ### need to fix this later
    color_normalize <- as.vector(palette36.colors(n = ncolors_needed))
  }

  norm_plot_component <-  plot_ly(data = plot_ndat,
                                  x=~xvar,
                                  y=~yvar,
                                  mode='lines+markers',
                                  type='scatter',
                                  color=~byvar,
                                  colors = color_normalize,
                                  hoverinfo = "text",
                                  showlegend = TRUE,
                                  text = paste(subjectid_marker_prefix, plot_ndat$SubjectID,
                                               "<br>",
                                               xvar_marker_prefix, round(as.numeric(plot_ndat$xvar),2),
                                               "<br>",
                                               yvar_marker_prefix, round(as.numeric(plot_ndat$yvar),2)
                                  )
  ) %>% layout( dragmode = "lasso",
                xaxis=list(title=xlabel_txt, type = scaling[1]),
                yaxis=list(title=ylabel_txt, type = scaling[1]),
                plot_bgcolor  = "transparent",
                paper_bgcolor = "transparent",
                updatemenus = list(
                  list(
                    type = "dropdown",
                    direction = "down",
                    y = 0.8,
                    x = -0.1,
                    buttons = list(
                      list(method = "relayout",
                           args = list("yaxis", list(type = scaling[1], title=ylabel_txt)),
                           label = scaling[1]),
                      list(method = "relayout",
                           args = list("yaxis", list(type = scaling[2], title=ylabel_txt)),
                           label = scaling[2])
                    ) # end buttons
                  ),
                  list(
                    type = "dropdown",
                    direction = "right",
                    y = -0.1,
                    x = 0.05,
                    buttons = list(
                      list(method = "relayout",
                           args = list("xaxis", list(type = scaling[1], title=xlabel_txt)),
                           label = scaling[1]),
                      list(method = "relayout",
                           args = list("xaxis", list(type = scaling[2], title=xlabel_txt)),
                           label = scaling[2])
                    ) # end buttons
                  ) # end sublist
                ) # end list
  ) # end layout

  norm_plotly <- ggplotly(norm_plot_component)
  return(norm_plotly)
}


make_density_plotly <- function(plot_ndat=NULL,
                                xvar = NULL,
                                byvar = NULL,
                                idvar = NULL,
                                xlabel_txt = "x value",
                                xmax = xmax
) {
  plot_ndat <- plot_ndat[ , c(idvar,byvar,xvar)]
  names(plot_ndat)[names(plot_ndat) == xvar] <- "xvar"
  names(plot_ndat)[names(plot_ndat) == byvar] <- "byvar"
  names(plot_ndat)[names(plot_ndat) == idvar] <- "SubjectID"
  plot_ndat <- plot_ndat[order(plot_ndat$byvar,plot_ndat$xvar), ]
  ncolors_needed <- length(unique(plot_ndat$byvar))
  if (ncolors_needed < 37) {
    ## Create a color palette for up to 36 different plates.
    color_normalize <- as.vector(palette36.colors(n = ncolors_needed))
  } else {
    ## Create a color palette for more than 36 different plates.
    ### need to fix this later
    color_normalize <- as.vector(palette36.colors(n = ncolors_needed))
  }

  raw_density <- ggplot(data=plot_ndat, aes(x = xvar)) +
    geom_density(alpha = 0.3, aes(color = byvar), show.legend = FALSE) +
    scale_color_manual(name = "Plate ID", values = color_normalize )+
    xlim(10, xmax) +
    labs(x=xlabel_txt, y="Density")
  theme_bw() +
    theme(text = element_text(size=8),
          legend.title = element_text(size=8),
          plot.background=element_blank(),
          legend.position = "none"
    )

  density_plotly <- ggplotly(raw_density)  %>%
    layout( dragmode = "lasso",
            xaxis=list(title=xlabel_txt, type = "linear", range=c(10,xmax)),
            yaxis=list(title="Density", type = "linear"),
            plot_bgcolor  = "transparent",
            paper_bgcolor = "transparent",
            updatemenus = list(
              list(
                type = "dropdown",
                direction = "right",
                y = -0.1,
                x = 0.05,
                buttons = list(
                  list(method = "relayout",
                       args = list("xaxis", list(type = "linear")),
                       label = "linear"),
                  list(method = "relayout",
                       args = list("xaxis", list(type = "log")),
                       label = "log")
                ) # end buttons
              ) # end list
            ) # end list
    ) # end layout
  return(density_plotly)
}

## produces table of fits to each plate. n rows = n plates.
fit_status_table <- function(modfitstatus=NULL) {
  fit_status_df <- as.data.frame(modfitstatus)
  names(fit_status_df)[names(fit_status_df) == "model_results$fit_status"] <- "Fit Status by Plate"
  return(fit_status_df)
}

## produces table of fits to each plate. n rows = n plates
produce_fit_status_tab <- function(model_results){
  fit_stat_tab_df <- fit_status_table(modfitstatus=model_results$fit_status)
  names(fit_stat_tab_df)[names(fit_stat_tab_df) == "modfitstatus"] <- "Fit Status by Plate"
  fit_status_tab <- gt(fit_stat_tab_df)
  return(fit_status_tab)
}

### create two pairs of figures with only one legend for all plots
# get predictions from models
# norm_plate_data predictions
plot_plate_fits <- function(norm_plate_data, fitted_models){
  norm_plate_data_fitted <- predict_fitted(setdata=norm_plate_data, fitted_models=fitted_models)



  plat_dat <- norm_plate_data_fitted

 # plat_dat <- norm_plate_data_fitted[ norm_plate_data_fitted$study_accession == selected_study &
                                  # norm_plate_data_fitted$experiment_accession == selected_experiment &
                                   #norm_plate_data_fitted$antigen == selected_antigen &
                                  # norm_plate_data_fitted$feature == selected_feature,  ]

  plat_dat$standard_id <- rownames(plat_dat)
  xmax <- max(plat_dat$raw_value)


  plate_raw_scatter <- make_scatter_plotly(plot_ndat=plat_dat,
                                           xvar = "raw_value",
                                           yvar = "normal_reference",
                                           byvar = "plateid",
                                           idvar = "standard_id",
                                           subjectid_marker_prefix = "Standard: ",
                                           xvar_marker_prefix = "Raw value: ",
                                           yvar_marker_prefix = "Normal Reference: ",
                                           xlabel_txt = "Raw Values [MFI]",
                                           ylabel_txt = "Normal Reference [mean(MFI)]",
                                           scaling=c("linear","log")
  ) %>% layout(showlegend=TRUE,
               xaxis = list(side="right", showgrid=FALSE, title="Raw Values [MFI]"),
               yaxis = list(showgrid=FALSE, title="Normal Reference [mean(MFI)]"))


  plate_raw_density <- make_density_plotly(plot_ndat=plat_dat,
                                           xvar = "raw_value",
                                           byvar = "plateid",
                                           idvar = "standard_id",
                                           xlabel_txt = "Raw Values [MFI]",
                                           xmax = xmax
  ) %>% layout(showlegend=FALSE,
               xaxis = list(side="right", showgrid=FALSE, title="Raw MFI"),
               yaxis = list(showgrid=FALSE, title="Density"))


  plate_normalized_scatter <- make_scatter_plotly(plot_ndat=plat_dat,
                                                  xvar = "raw_value",
                                                  yvar = "normalized_value",
                                                  byvar = "plateid",
                                                  idvar = "standard_id",
                                                  subjectid_marker_prefix = "Standard: ",
                                                  xvar_marker_prefix = "Raw value: ",
                                                  yvar_marker_prefix = "Normalized: ",
                                                  xlabel_txt = "Raw Values [MFI]",
                                                  ylabel_txt = "Normalized [normalized MFI]",
                                                  scaling=c("log","linear")
  ) %>% layout(showlegend=FALSE,
               xaxis = list(side="right", showgrid=FALSE, title="Normalized Values [normalized MFI]"),
               yaxis = list(showgrid=FALSE, title="Normal Reference [mean(MFI)]"))

  plate_normalized_density <- make_density_plotly(plot_ndat=plat_dat,
                                                  xvar = "normalized_value",
                                                  byvar = "plateid",
                                                  idvar = "standard_id",
                                                  xlabel_txt = "Normalized Values [normalized MFI]",
                                                  xmax = xmax
  ) %>% layout(showlegend=TRUE,
               xaxis = list(side="right", showgrid=FALSE, title="Normalized"),
               yaxis = list(showgrid=FALSE, title="Density"))

  # fig_one_list <- c(fig_one_list[1], fig_one_list[2], fig_one_list[3], fig_one_list[4])
  # list(plate_raw_scatter,plate_raw_density,plate_normalized_scatter,plate_normalized_density)

  fig_one <- subplot(plate_raw_scatter,
                     plate_raw_density,
                     # plate_normalized_scatter,
                     # plate_normalized_density,
                     nrows = 1,
                     widths = c(0.8,0.2)
  ) %>%
    layout(
      showlegend=TRUE,
      xaxis = list(side="right", showgrid=FALSE, title="Raw Values [MFI]"),
      yaxis = list(showgrid=FALSE, title="Normal Reference [mean(MFI)]"), plot_bgcolor  = "transparent",
      paper_bgcolor = "transparent"
    ) # end layout

  fig_two <- subplot(plate_normalized_scatter,
                     plate_normalized_density,
                     nrows = 1,
                     widths = c(0.8,0.2)
  ) %>%
    layout(
      showlegend=TRUE,
      xaxis = list(side="right", showgrid=FALSE, title="Normalized Values [normalized MFI]"),
      yaxis = list(showgrid=FALSE, title="Normal Reference [mean(MFI)]",plot_bgcolor  = "transparent",
                   paper_bgcolor = "transparent")
    ) # end layout


  return(list(fig_one,fig_two))

}

plot_sample_fits <- function(fitted_models,stored_plates_data, ant, feat){
  # sample_data predictions
  sample_data <- load_sample_data(stored_plates_data = stored_plates_data,
                                  selected_antigen = ant,
                                  selected_feature = feat)

  sample_data_fitted <- predict_fitted(setdata=sample_data, fitted_models=fitted_models)

  samp_dat <- sample_data_fitted

 # samp_dat <- sample_data_fitted[sample_data_fitted$study_accession == selected_study &
                                  # sample_data_fitted$experiment_accession == selected_experiment &
                                  # sample_data_fitted$antigen == selected_antigen &
                                  # sample_data_fitted$feature == selected_feature,  ]

  sample_plotly <- make_scatter_plotly(plot_ndat=samp_dat,
                                       xvar = "raw_value",
                                       yvar = "normalized_value",
                                       byvar = "plateid",
                                       idvar = "tid",
                                       subjectid_marker_prefix = "Subject: ",
                                       xvar_marker_prefix = "Raw value: ",
                                       yvar_marker_prefix = "Normalized: ",
                                       xlabel_txt = "Raw Values [MFI]",
                                       ylabel_txt = "Normalized [MFI]",
                                       scaling=c("linear","log")
  ) %>%
    layout(
      showlegend=TRUE,
      xaxis = list(side="right", showgrid=FALSE, title="Raw Sample Values [MFI]"),
      yaxis = list(showgrid=FALSE, title="Normalized Sample Values [normalized MFI]")
    ) # end layout

  sample_plotly
}
