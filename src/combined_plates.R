create_standard_plot_allplate <- function() {
  print("in create standard_plot")
  # local_antigen <- input$gaterxMap_antigen
  antigen_list <- unique(all_standard$antigen)
  local_antigen <- antigen_list[2]
  # local_source <- input$gaterxMap_source
  source_list <- unique(all_standard$source)
  local_source <- source_list[1]
  local_feature <- "FcgR2a"
  local_study_accession <- unique(all_standard$study_accession)
  local_experiment_accession <- unique(all_standard$experiment_accession)

  local_standard_antigen <- all_standard[all_standard$antigen == local_antigen &
                                         all_standard$feature == local_feature &
                                         all_standard$source == local_source, ]

  local_preds_antigen <- all_preds[all_preds$antigen == local_antigen &
                                           all_preds$source == local_source, ]
  local_preds_antigen$linethick <- 0.7

  local_fits_antigen <- all_fits[all_fits$antigen == local_antigen &
                                   all_fits$source == local_source,
                                   c("study_accession", "experiment_accession", "plateid", "antigen",
                                     "l_asy", "r_asy", "x_mid", "scale",
                                     "bendlower", "bendupper", "llod", "ulod","source"
                                     )]
#
#   chek <- local_fits_antigen %>%
#     group_by(study_accession, experiment_accession, plateid, antigen, source) %>%
#     summarise(across(l_asy, r_asy, x_mid,scale, bendlower, bendupper, llod, ulod), ~ mean(.x, na.rm = TRUE))

  lfa_tibb <- as.data.frame(local_fits_antigen)
  mean_plates_fits <- as.data.frame(dplyr::reframe(.data = lfa_tibb, across(c(l_asy, r_asy, x_mid,scale, bendlower, bendupper, llod, ulod),mean), .by = c(study_accession, experiment_accession, plateid, antigen, source) )
  )
  mean_pplate_fits <- dplyr::reframe(.data = mean_plates_fits, across(c(l_asy, r_asy, x_mid,scale, bendlower, bendupper, llod, ulod),mean), .by = c(study_accession, experiment_accession, antigen, source) )
  mean_pplate_fits$plateid <- paste(mean_pplate_fits$experiment_accession,"mean_parms","16052024", sep = ".")

  local_fits_antigen <- rbind(local_fits_antigen,mean_pplate_fits)

  fit_list <<- compute_allplate_robust_curves(local_standard_antigen, local_antigen, local_study_accession, local_experiment_accession, local_source)

  # newdils <- data.frame(log_dilution = seq(from = -10, to = 0, by = 0.05))
  # stored_preds <- logist.predict(stored_fits,newdils)

  allplate_preds <<- as.data.frame(fit_list[4])
  allplate_preds$focal_group <- "All others"
  allplate_preds$linethick <- 1

  allplate_fits <<- as.data.frame(fit_list[2])
  local_hline_fit <- as.data.frame(fit_list[2])

  local_preds_antigen <- rbind(local_preds_antigen, allplate_preds)


  # local_standard_antigen <- pa_data$standard_antigen[pa_data$standard_antigen$source==local_source, ]
  # local_preds_antigen <- pa_data$preds_antigen[pa_data$preds_antigen$source==local_source, ]

  if(nrow(local_standard_antigen) == 0){
    return(blank_plot("Standard"))
  } else if (is.null(nrow(local_preds_antigen)) | nrow(local_preds_antigen)==0) {
    # return(blank_plot("Standard"))
    # local_standard_antigen <- pa_data$standard_antigen[pa_data$standard_antigen$source==local_source, ]
    output[[paste0("standard_plot")]] <- renderPlot({
      print(paste("renderPlot_standard_plot_antigen:",paste0("standard_plot_raw_",local_antigen)))
      ggplot(data = local_standard_antigen,
             aes(x = log_dilution, y = mfi)) +
        geom_point() +
        ylim(0, max(local_preds_antigen$fitted)) +
        xlim(min(local_standard_antigen$log_dilution), 0) +
        xlab("Dilution (log units)") +
        ylab("MFI") +
        ggtitle("Standard Data - Not Fitted") +
        theme_bw() +
        theme(text = element_text(size=8) )
    })
  } else {
    # local_hline_fit <- pa_data$hline_fit[pa_data$hline_fit$source==local_source, ]
    hline_data <- data.frame( source = c(local_hline_fit$source,
                                         local_hline_fit$source,
                                         local_hline_fit$source,
                                         local_hline_fit$source),
                              y = c(local_hline_fit$llod,
                                    local_hline_fit$bendlower,
                                    local_hline_fit$bendupper,
                                    local_hline_fit$ulod),
                              type = factor(c(1, 2, 2, 1)),
                              ctype = factor(c(1, 2, 3, 4)),
                              labels = c(paste0("Lower LOD: ",local_hline_fit$llod),
                                         paste0("Lower bend: ",local_hline_fit$bendlower),
                                         paste0("Upper bend: ",local_hline_fit$bendupper),
                                         paste0("Upper LOD: ",local_hline_fit$ulod)),
                              stringsAsFactors = FALSE)
    # pa_data$hline_data <- hline_data
    # local_fit_tab <- pa_data$fit_tab_antigen[pa_data$fit_tab_antigen$source==local_source,]
    local_fit_tab <- as.data.frame(fit_list[1])[ , c("term", "estimate", "std_error", "signif")]
    print("assembling the fit table")
    t1 <- tableGrob(local_fit_tab, theme = ttheme_minimal(base_size = 9, padding = unit(c(2, 2), "mm")),
                    rows = NULL,
                    vp = viewport(x = 0.1, y = 0.1))
    # title_standard <- paste("Rsquare: ",
    #                         round(local_hline_fit$rsquare_fit,3),
    #                         "Residual[df]:",
    #                         local_hline_fit$df.residual
    # )
    title_standard <- paste(local_hline_fit$study_accession,
                            local_hline_fit$experiment_accession,
                            local_hline_fit$antigen,
                            local_hline_fit$plateid,
                            sep = " - ")
    title_text_t1 <- bquote(R^2 * ": " * .(sprintf("%.3f", round(local_hline_fit$rsquare_fit,3))) * ", " * Residual[df] * ": " *  .(sprintf("%.0f", local_hline_fit$df.residual)))
    title_t1 <- textGrob(title_text_t1,gp=gpar(fontsize=9))
    padding <- unit(2,"mm")
    fit_table <- gtable_add_rows(
      t1,
      heights = grobHeight(title_t1) + padding,
      pos = 0)
    fit_table <- gtable_add_grob(
      fit_table,
      title_t1,
      1, 1, 1, ncol(fit_table))
    print(paste("antigen:",local_antigen,"fit_table",fit_table))

    # output[[paste0("standard_plot_", local_antigen)]] <<- renderPlot({
    output[[paste0("standard_plot")]] <- renderPlot({
      print(paste("renderPlot_standard_plot_antigen:",paste0("standard_plot_",local_antigen)))

      ggplot(data = local_standard_antigen,
             aes(x = log_dilution, y = mfi, colour = plateid)) +
        geom_point() +
        geom_line(data = local_preds_antigen[local_preds_antigen$linethick==1,],
                  aes(x = log_dilution,
                      y = fitted,
                      colour = plateid),
                  show.legend = FALSE) +
        ylim(0, max(local_preds_antigen$fitted)) +
        xlim(min(local_standard_antigen$log_dilution), 0) +
        xlab("Dilution (log units)") +
        ylab("MFI") +
        theme_bw() +
        theme(text = element_text(size=8)
              # , line = element_line(linewidth = 1)
              ) +

        geom_hline(data = hline_data,
                   aes(yintercept = y, linetype = ctype)) +


        # scale_colour_manual(values = color_features,
        #                     labels = as.list(hline_data$labels),
        #                     name = "Key") +
        scale_linetype_manual(values = 1:4,
                              labels = as.list(hline_data$labels),
                              name = "Key") +

        ggtitle(title_standard) +

        annotation_custom(fit_table,
                          xmin=(min(local_standard_antigen$log_dilution)),
                          xmax=(0.5*max(local_standard_antigen$log_dilution)),
                          ymin=(0.88*max(local_preds_antigen$fitted)),
                          ymax=(0.96*max(local_preds_antigen$fitted))
        )
    })
  }
  # })
}
