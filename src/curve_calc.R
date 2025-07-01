### code to compute standard curves and store the fits, the parameters and the predicted values for each set of curve data

# observeEvent(input$btn_calc_standard_curves, {
#     print("in btn calc standard curves")
#     removeTab(inputId = "body_panel_id", target="curverxMap")
#     show_modal_progress_circle(
#       value = 0,
#       text = "Starting robust regression",
#       color = "#112446",
#       stroke_width = 4,
#       easing = "easeOut",
#       duration = 1000,
#       trail_color = "#eee",
#       trail_width = 1,
#       height = "200px",
#       session = shiny::getDefaultReactiveDomain()
#     )
#     select_study_accession <- input$readxMap_study_accession
#     select_experiment_accession <- input$readxMap_experiment_accession
#     print(paste("Selected Experiment: ",select_study_accession, "-", select_experiment_accession))
#
#                 print(paste("writing plf:", local_antigen, platey))
#                 plf <- fit_list[[3]][ , c("study_accession", "experiment_accession", "antigen", "plateid", "fitted", "log_dilution", "source")]
#                 # plf$source <- unique(fit_list[[4]]$source)
#                 plf <- map_type_db("Standard_preds", plf)
#                 DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_preds"), plf)

                print(paste("writing slf:", cc_antigen, platey))
                slf <- as.data.frame(fit_list[[3]])
                # slf$antigen <- NA
                names(slf)[names(slf) == "n"] <- "nbeads"

                # print(names(slf))

                slf <- slf[, c("study_accession", "experiment_accession", "plate_id", "stype", "dilution", "antigen", "mfi", "nbeads", "feature", "log_dilution", "plateid","weights", "source")]
                # slf$antigen <- cc_antigen
                slf <- map_type_db("Standard_stor", slf)
                # print(paste("slf:", slf$anigen))
                # print(names(slf))
                DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_stor"), slf)

              }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

          } # close antigen loop
        # } else {
        #   print(paste0("---- no data in dat_standard ---- : ", local_antigen))
        #   plate_standard <- NULL
        # }
      } # close source loop
      print(paste0(platey,": after fit, predictions and dataset is assembled and stored"))

    } # close plate loop
    remove_modal_progress(session = getDefaultReactiveDomain())

    })
    ## update the header table
    updateSelectInput(session = getDefaultReactiveDomain(),
                      inputId = "readxMap_experiment_accession",
                      label = "Choose Experiment Name",
                      choices = c(c("Click here" = "Click here"),
                                  setNames(reactive_df_study_exp()$experiment_accession,
                                           reactive_df_study_exp()$experiment_name)
                      ),
                      selected = NULL)
    updateSelectInput(session = getDefaultReactiveDomain(),
                      inputId = "readxMap_experiment_accession",
                      label = "Choose Experiment Name",
                      choices = c(c("Click here" = "Click here"),
                                  setNames(reactive_df_study_exp()$experiment_accession,
                                           reactive_df_study_exp()$experiment_name)
                      ),
                      selected = select_experiment_accession)
}) # end observeEvent for input$btn_calc_standard_curves


observeEvent(input$btn_calc_select_standard_curves, {
  print("in btn calc select standard curves")
  print(names(input$stored_header_rows_selected))
  print(selected_studyexpplate$plateid)
  removeTab(inputId = "body_panel_id", target="curverxMap")
  show_modal_progress_circle(
    value = 0,
    text = "Starting robust regression",
    color = "#112446",
    stroke_width = 4,
    easing = "easeOut",
    duration = 1000,
    trail_color = "#eee",
    trail_width = 1,
    height = "200px",
    session = shiny::getDefaultReactiveDomain()
  )
  select_study_accession <- input$readxMap_study_accession
  select_experiment_accession <- input$readxMap_experiment_accession
  select_plate <- stored_header[input$stored_header_rows_selected, ]$plateid
  print(paste("Selected Experiment: ",select_study_accession, "-", select_experiment_accession))
  print(paste("Selected Plate: ", select_plate))

  # Check to make sure that all the standard curve plate data have been copied from the control table
  if (select_study_accession=='MADI_P3_GAPS') {
    local({
      dat_stand <- update_db(operation = "select",
                             schema = "madi_results",
                             table_name = "xmap_standard",
                             select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
      )
      plat_list <- unique(dat_stand$plateid)
      print(paste("plates in plat_list:",length(plat_list)))
      for (plat in 1:length(plat_list)) {
        px_id <- plat_list[plat]
        soc_list <- unique(dat_stand[dat_stand$plateid==px_id,]$source)
        print(paste(plat, "copy plateid:", px_id, "soc_list:",soc_list))
        if (length(soc_list)==1) {
          dat_control <- update_db(operation = "select",
                                   schema = "madi_results",
                                   table_name = "xmap_control",
                                   select_where = list("concat(study_accession,experiment_accession,plateid)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession,px_id))
          )
          dat_control$stype <- "S"
          dat_control$antibody_name <- "from_control"
          dat_control <- dat_control[ , c("study_accession", "experiment_accession", "antigen", "source", "plateid", "well", "stype", "sampleid", "dilution", "pctaggbeads", "samplingerrors", "antibody_mfi", "antibody_n", "antibody_name", "feature")]
          dat_control <- dat_control
          print(paste("copy plateid dat_control:", unique(dat_control$plateid), unique(dat_control$source), nrow(dat_control)))
          DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard"), dat_control)
        }
      }
    })
    # table(dat_stand$plateid,dat_stand$source)
  }

  local({
    dat_standard_full <- storedlong_plates_data$stored_standard



    fit_standard <- update_db(operation = "select",
                              schema = "madi_results",
                              table_name = "xmap_standard_fits",
                              select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
    )
    if (is.null(fit_standard)) {
      fit_standard_dedup <- distinct(fit_standard, study_accession, experiment_accession, antigen, plateid, source, status, .keep_all = FALSE)
      dat_standard_merge <- merge(dat_standard_full, fit_standard_dedup, by.x = c("study_accession", "experiment_accession", "antigen", "plateid", "source"), by.y = c("study_accession", "experiment_accession", "antigen", "plateid", "source"), all.x = TRUE)
      print("all the fields of the dat_standard_merge")
      dat_standard <- dat_standard_merge[is.na(dat_standard_merge$status), c("study_accession","experiment_accession","plateid","stype","antigen","antibody_mfi","antibody_n","feature","status","plateid","dilution","source")]
    } else {
      dat_standard <- dat_standard_full
    }

    names(dat_standard)[names(dat_standard) == "antibody_mfi"] <- "MFI"
    names(dat_standard)[names(dat_standard) == "antibody_n"] <- "nbeads"
    dat_standard$dilution <- 1 / dat_standard$dilution
    dat_standard$log_dilution <- log10(dat_standard$dilution + 0.00001)
    print("all the fields of the dat_standard")
    # print(names(dat_standard))
    # dat_standard <- subset(dat_standard,select = -c(antibody_name,well,sampleid,pctaggbeads,samplingerrors))

    # dat_standard <- dat_standard

    plates_list <<- unique(dat_standard$plateid)
    print(paste0("plates_list: ",plates_list))
    m=0
    for (i in 1:length(plates_list)) {
      platey <- plates_list[i]

      source_list <- unique(dat_standard[dat_standard$plateid==platey,]$source)
      print(paste0("source_list: ",source_list))

      for (j in 1:length(source_list)){
        local_source <- source_list[j]
        #       if(nrow(dat_standard[dat_standard$plateid==platey & dat_standard$source==local_source, ]) > 0) {
        print(paste("Plate:",platey," Source:",local_source))
        plate_standard <<- dat_standard[dat_standard$plateid==platey & dat_standard$source==local_source, ]
        max_mfi <- round_any(max(plate_standard$mfi), 500, f = ceiling)
        max_dilute <- max(plate_standard$log_dilution)
        min_dilute <- min(plate_standard$log_dilution)
        # log_dilution <- seq(from = -10, to = 0, by = 0.05)
        # newdils <- as.data.frame(log_dilution)
        antigen_list <- unique(plate_standard$antigen)
        # print(paste0("antigen_list: ",antigen_list))

        for (k in 1:length(antigen_list)) {
          cc_antigen <- antigen_list[k]
          plate_txt <- paste("Plate:",platey," Source:",local_source," Antigen:",cc_antigen)
          m <- m+1
          update_modal_progress(
            value = m / (length(plates_list)*length(source_list)*length(antigen_list)),
            text = plate_txt,
            session = shiny::getDefaultReactiveDomain()
          )
          antigen_standard <- plate_standard[plate_standard$antigen==cc_antigen, ]

          print(paste(m, "in loop:", plate_txt))
          # for iteratively reweighted nonlinear constrained regression with welsch weights
          tryCatch(
            {
              print(paste("creating fits - source:",local_source))
              fit_list <- compute_robust_curves(antigen_standard, cc_antigen, platey, select_study_accession, select_experiment_accession, local_source)
              # store fits
              print(paste("writing tlf:", cc_antigen, platey))
              tlf <- as.data.frame(fit_list[[1]])
              # tlf$source <- unique(fit_list[[4]]$source)
              tlf <- map_type_db("Standard_fit_tab", tlf)
              # print(names(tlf))
              # print(tlf)
              DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_fit_tab"), tlf)

              print(paste("writing glf:", cc_antigen, platey, local_source))
              glf <- as.data.frame(fit_list[[2]])
              # glf$source <- NA
              names(glf)[names(glf) == "df.residual"] <- "dfresidual"
              names(glf)[names(glf) == "logLik"] <- "loglik"
              names(glf)[names(glf) == "AIC"] <- "aic"
              names(glf)[names(glf) == "BIC"] <- "bic"
              glf$source <- local_source
              glf$crit <- NA
              glf <- glf[ , c("study_accession", "experiment_accession", "plateid", "antigen", "iter", "status",
                              "crit", "l_asy", "r_asy", "x_mid", "scale", "bendlower", "bendupper",
                              "llod", "ulod", "loglik", "aic", "bic", "deviance", "dfresidual",
                              "nobs", "rsquare_fit", "source")]
              glf <- map_type_db("Standard_fits", glf)
              # print(names(glf))
              # print(glf)
              DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_fits"), glf)
              #
              #                 print(paste("writing plf:", local_antigen, platey))
              #                 plf <- fit_list[[3]][ , c("study_accession", "experiment_accession", "antigen", "plateid", "fitted", "log_dilution", "source")]
              #                 # plf$source <- unique(fit_list[[4]]$source)
              #                 plf <- map_type_db("Standard_preds", plf)
              #                 DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_preds"), plf)

              print(paste("writing slf:", cc_antigen, platey))
              slf <- as.data.frame(fit_list[[3]])
              # slf$antigen <- NA
              names(slf)[names(slf) == "n"] <- "nbeads"

              # print(names(slf))

              slf <- slf[, c("study_accession", "experiment_accession", "plate_id", "stype", "dilution", "antigen", "mfi", "nbeads", "feature", "log_dilution", "plateid","weights", "source")]
              # slf$antigen <- cc_antigen
              slf <- map_type_db("Standard_stor", slf)
              # print(paste("slf:", slf$anigen))
              # print(names(slf))
              DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_stor"), slf)

            }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

        } # close antigen loop
        # } else {
        #   print(paste0("---- no data in dat_standard ---- : ", local_antigen))
        #   plate_standard <- NULL
        # }
      } # close source loop
      print(paste0(platey,": after fit, predictions and dataset is assembled and stored"))

    } # close plate loop
    remove_modal_progress(session = getDefaultReactiveDomain())

  })
  ## update the header table
  updateSelectInput(session = getDefaultReactiveDomain(),
                    inputId = "readxMap_experiment_accession",
                    label = "Choose Experiment Name",
                    choices = c(c("Click here" = "Click here"),
                                setNames(reactive_df_study_exp()$experiment_accession,
                                         reactive_df_study_exp()$experiment_name)
                    ),
                    selected = NULL)
  updateSelectInput(session = getDefaultReactiveDomain(),
                    inputId = "readxMap_experiment_accession",
                    label = "Choose Experiment Name",
                    choices = c(c("Click here" = "Click here"),
                                setNames(reactive_df_study_exp()$experiment_accession,
                                         reactive_df_study_exp()$experiment_name)
                    ),
                    selected = select_experiment_accession)
}) # end observeEvent for input$btn_calc_select_standard_curves

