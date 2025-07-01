# use to plot stored standard_curves for an individual plate

observeEvent(input$stored_header_rows_selected, {
    print("In curver plotter code")
    removeTab(inputId = "body_panel_id", target="curverxMap")

    select_study_accession <- stored_plates_data$stored_header[input$stored_header_rows_selected,2]
    select_experiment_accession <- stored_plates_data$stored_header[input$stored_header_rows_selected,3]
    print(paste("Selected Experiment: ",select_study_accession, "-", select_experiment_accession))
    print("selected study and experiment")

#     dat_standard_full <- update_db(operation = "select",
#                                schema = "madi_results",
#                                table_name = "xmap_standard",
#                                select_where = list("concat(study_accession,experiment_accession)" = paste0(select_study_accession,select_experiment_accession))
#                                )
#     dat_standard_full$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(dat_standard_full$plate_id))
#     # dat_standard_full_dedup <<- distinct(dat_standard_full, study_accession, experiment_accession, antigen, plateid, antibody_mfi, dilution, .keep_all = TRUE)
#     # dat_standard_full_dedup$study_accession <- factor(dat_standard_full_dedup$study_accession)
#     # dat_standard_full_dedup$experiment_accession <- factor(dat_standard_full_dedup$experiment_accession)
#     # dat_standard_full_dedup$antigen <- factor(dat_standard_full_dedup$antigen)
#     # dat_standard_full_dedup$plateid <- factor(dat_standard_full_dedup$plateid)
#     fit_standard <- update_db(operation = "select",
#                               schema = "madi_results",
#                               table_name = "xmap_standard_fits",
#                               select_where = list("concat(study_accession,experiment_accession)" = paste0(select_study_accession,select_experiment_accession))
#                               )
#     fit_standard_dedup <<- distinct(fit_standard, study_accession, experiment_accession, antigen, plateid, .keep_all = TRUE)
#     # fit_standard_dedup$study_accession <- factor(fit_standard_dedup$study_accession)
#     # fit_standard_dedup$experiment_accession <- factor(fit_standard_dedup$experiment_accession)
#     # fit_standard_dedup$antigen <- factor(fit_standard_dedup$antigen)
#     # fit_standard_dedup$plateid <- factor(fit_standard_dedup$plateid)
#     dat_standard_merge <<- merge(dat_standard_full, fit_standard_dedup, by.x = c("study_accession", "experiment_accession", "antigen", "plateid"), by.y = c("study_accession", "experiment_accession", "antigen", "plateid"), all.x = TRUE)
#     dat_standard <<- dat_standard_merge[is.na(dat_standard_merge$status), c(5,1,2,4,8,11,3,6,7,9,10,12:17)]
#
#     print("after dat_standard is called once")
#     names(dat_standard)[names(dat_standard) == "antibody_mfi"] <- "MFI"
#     names(dat_standard)[names(dat_standard) == "antibody_n"] <- "nbeads"
#     dat_standard$dilution <- 1 / dat_standard$dilution
#     dat_standard$log_dilution <- log10(dat_standard$dilution + 0.00001)
#     dat_standard <- subset(dat_standard,select = -c(antibody_name,well,sampleid,source,pctaggbeads,samplingerrors))
#
#
#     print("line 160")
#     antigen_list <<- unique(dat_standard$antigen)
#     ## antigen_list <<- c("fha","prn","pt","pentamer")
#     print(paste0("antigen_list: ",antigen_list))
#
#     for (local_antigen in antigen_list){
#       # Assigning standard values
#       if(!is.null(dat_standard)){
#         print(paste0("found data in dat_standard: ", local_antigen))
#         standard_curve <<- dat_standard[dat_standard$antigen==local_antigen, ]
#         max_mfi <- round_any(max(standard_curve$MFI), 500, f = ceiling)
#         max_dilute <- max(standard_curve$log_dilution)
#         min_dilute <- min(standard_curve$log_dilution)
#         log_dilution <- seq(from = -20, to = 0, by = 0.1)
#         newdils <- as.data.frame(log_dilution)
#       } else {
#         print(paste0("---- no data in dat_standard ---- : ", local_antigen))
#         standard_curve <- NULL
#       }
#
#       # curv_fits <- data.frame()
#       # curv_coefs <- data.frame()
#       # curv_preds <- data.frame()
#       # curv_stor <- data.frame()
#
#       plates_list <<- unique(standard_curve$plateid)
#
#       print(paste0("plates_list: ",plates_list))
#
#       for (plate in plates_list) {
#         plate_standard <<- standard_curve[standard_curve$plateid==plate, ]
#         # for iteratively reweighted nonlinear constrained regression with welsch weights
#         tryCatch({
#
#
#         fit_list <<- compute_robust_curves(plate_standard, newdils, local_antigen, plate, select_study_accession, select_experiment_accession)
#         fit <- fit_list[[1]]
#         # store fits
#         glf <- fit_list[[2]]
#         names(glf)[names(glf) == "df.residual"] <- "dfresidual"
#         names(glf)[names(glf) == "logLik"] <- "loglik"
#         names(glf)[names(glf) == "AIC"] <- "aic"
#         names(glf)[names(glf) == "BIC"] <- "bic"
#
#         plf <- fit_list[[3]][ , c("study_accession", "experiment_accession", "antigen", "plateid", "fitted", "log_dilution")]
#
#         slf <- fit_list[[4]][ ,-c(1)]
#         # names(slf)[names(slf) == "PlateID"] <- "plateid"
#         names(slf)[names(slf) == "MFI"] <- "mfi"
# #         insert_str <- "
# #         INSERT INTO madi_results.xmap_standard_fits(
# # 	study_accession, experiment_accession, plateid, antigen, iter, status,
# # 	l_asy, r_asy, x_mid, scale, bendlower, bendupper, llod, ulod,
# # 	loglik, aic, bic, deviance, dfresidual, nobs)
# # 	VALUES (glf[1], glf[2], glf[3], glf[4], glf[5], glf[6], glf[7], glf[8],
# # 	glf[9], glf[10], glf[11], glf[12], glf[13], glf[14], glf[15], glf[16],
# # 	glf[17], glf[18], glf[19], glf[20], glf[21]);
# #         "
#         print("writing glf")
#         glf <<- map_type_db("Standard_fits", glf)
#         DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_fits"), glf)
#
#         print("writing plf")
#         plf <<- map_type_db("Standard_preds", plf)
#         DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_preds"), plf)
#
#         print("writing slf")
#         slf <<- map_type_db("Standard_stor", slf)
#         DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_stor"), slf)
#
#         # print(paste0(local_antigen,"=> start accumulating plates: ", plate))
#         # curv_fits <- rbind(curv_fits,fit_list[[2]])
#         # # print(curv_fits)
#         # curv_preds <- rbind(curv_preds,fit_list[[3]])
#         # list("concat(study_accession,experiment_accession)" = paste0(select_study_accession,select_experiment_accession))
#         # # print(curv_preds)
#         # curv_stor <- rbind(curv_stor,fit_list[[4]])
#         # # print(curv_stor)
#         }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#       }
#       # print(paste0("start accumulating antigens: ", local_antigen))
#       # accum_curv_fits <<- rbind(accum_curv_fits, curv_fits)
#       # accum_curv_preds <<- rbind(accum_curv_preds, curv_preds)
#       # accum_standard_curve <<- rbind(accum_standard_curve, curv_stor)
#       print(paste0(local_antigen,": after fit_table is assembled"))
#     }

    dat_standard_merge <<- merge(dat_standard_full, fit_standard_dedup, by.x = c("study_accession", "experiment_accession", "antigen", "plateid"), by.y = c("study_accession", "experiment_accession", "antigen", "plateid"), all.x = TRUE)
    dat_standard <<- dat_standard_merge[!is.na(dat_standard_merge$status), ]
    print("after dat_standard is called once")
    names(dat_standard)[names(dat_standard) == "antibody_mfi"] <- "MFI"
    names(dat_standard)[names(dat_standard) == "antibody_n"] <- "nbeads"
    dat_standard$dilution <- 1 / dat_standard$dilution
    dat_standard$log_dilution <- log10(dat_standard$dilution + 0.00001)
    dat_standard <- subset(dat_standard,select = -c(antibody_name,well,sampleid,source,pctaggbeads,samplingerrors))
    print("line 155")
    antigen_list <<- unique(dat_standard$antigen)
    ## antigen_list <<- c("fha","prn","pt","pentamer")
    print(paste0("antigen_list: ",antigen_list))

    for (local_antigen in antigen_list){
      # Assigning standard values
      if(!is.null(dat_standard)){
        print(paste0("found data in dat_standard: ", local_antigen))
        standard_curve <<- dat_standard[dat_standard$antigen==local_antigen, ]
        max_mfi <- round_any(max(standard_curve$MFI), 500, f = ceiling)
        max_dilute <- max(standard_curve$log_dilution)
        min_dilute <- min(standard_curve$log_dilution)
        log_dilution <- seq(from = -20, to = 0, by = 0.1)
        newdils <- as.data.frame(log_dilution)
      } else {
        print(paste0("---- no data in dat_standard ---- : ", local_antigen))
        standard_curve <- NULL
      }

      # curv_fits <- data.frame()
      # curv_coefs <- data.frame()
      # curv_preds <- data.frame()
      # curv_stor <- data.frame()

      plates_list <<- unique(standard_curve$plateid)

      print(paste0("plates_list: ",plates_list))

      for (plate in plates_list) {
        plate_standard <<- standard_curve[standard_curve$plateid==plate, ]
      }

    }
    # insertTab(inputId = "body_panel_id",
    #           tabPanel(value = "curverxMap",
    #                    title = "Review all standard curves",
    #                    add_busy_spinner(spin = "circle",
    #                                     color = "#FB6A4A",
    #                                     timeout = 100,
    #                                     position = c("bottom-left"),
    #                                     onstart = TRUE,
    #                                     margins = c(10, 10),
    #                                     height = "200px",
    #                                     width = "200px"),
    #                    ##add_busy_bar(color = "red", height = "12px"),
    #                    h3("MADI Lumi Reader - Examine the standard curves from all plates"),
    #                    p(paste0("Study Name: ", input$readxMap_study_accession)),
    #                    p(paste0("Experiment Name: ", input$readxMap_experiment_accession)),
    #                    br(),
    #                    p("This is where we can REVIEW and CHOOSE the LOD manually from examining data from all plates"),
    #                    p("On the Standard Curve figures:"),
    #                    p(style = "padding-left: 15px;","a. Nonlinear fitting matches the methods of Rajam et. al. Biologicals. 2019 January ; 57: 9–20. (DOI:10.1016/j.biologicals.2018.11.001)"),
    #                    p(style = "padding-left: 15px;","b. Bend limits from logistic functions follow the methods of Sebaugh and McCray. Pharmaceut. Statist. 2003; 2: 167–174 (DOI:10.1002/pst.062) "),
    #                    br(),
    #                    uiOutput("dynamic_tabset_curve")
    #           )
    # )

#       ui_output_main <- paste0("ui_curv_", local_antigen)
#
#       output[[paste0("standard_curves_", local_antigen)]] <- renderPlot({
#
#           if(is.null(standard_curve)){
#             return(blank_plot("Standard"))
#           }
#           ps <- ggplot(data = standard_curve, aes(x = log_dilution, y = MFI, group = PlateID, color = PlateID)) +
#             geom_point() +
#             geom_line(data = curv_preds, aes(x = log_dilution, y = fitted, group = PlateID, color = PlateID), linewidth = 1) +
#             # scale_colour_manual(values = color_typ,
#             #                     labels = hline_labels,
#             #                     name = "Key") +
#             ylim(0, max_mfi) +
#             xlab("Dilution (log units)") +
#             ylab("MFI") +
#             ggtitle("Standard Curve") +
#             theme_bw() +
#             theme( text = element_text(size=8) )
#             ps
#
#       })
#
#
#     }
#
#     tabs <- list()
#
#     # Generate tabsets
#     for (antigen in antigen_list) {
# #      standard_curve <- dat_standard[dat_standard$antigen==antigen, ]
#       # Instead of rendering here, just collect the tabPanels
#       tabs[[length(tabs) + 1]] <- tabPanel(
#         title = paste("Type", antigen),
#         uiOutput(outputId = paste0("ui_curv_", antigen))  # Placeholder for dynamic content
#       )
#     }
#     print("line 224")
#
#     # Now render all tabs at once outside of the loop
#     output$dynamic_tabset_curve <- renderUI({
#       do.call(tabsetPanel, args = tabs)
#     })
#
#     ### create ui_output_antigen for each antigen
#     for (antigen in antigen_list) {
#       local({
#         local_antigen <- antigen
#         create_ui_output_curves(local_antigen, dat_standard[dat_standard$antigen == local_antigen, ])
#       })
#     }
#     print("line 245")
#
#     ### fill ui_ouput
#     for (local_antigen in antigen_list){
#
#       antigen_curv_preds <<- accum_curv_preds[accum_curv_preds$antigen == local_antigen, ]
#       antigen_standard_curve <<- accum_standard_curve[accum_standard_curve$antigen == local_antigen, ]
#
#       # ui_output_main <- paste0("ui_curv_", local_antigen)
#       #
#       # output[[paste0("standard_curves_", local_antigen)]] <- renderPlot({
#       #
#       #     if(is.null(antigen_standard_curve)){
#       #       return(blank_plot("Standard"))
#       #     }
#       #     ps <- ggplot(data = antigen_standard_curve, aes(x = log_dilution, y = MFI, group = PlateID, color = PlateID)) +
#       #       geom_point() +
#       #       geom_line(data = antigen_curv_preds, aes(x = log_dilution, y = fitted, group = PlateID, color = PlateID), linewidth = 1) +
#       #       # scale_colour_manual(values = color_typ,
#       #       #                     labels = hline_labels,
#       #       #                     name = "Key") +
#       #       ylim(0, max_mfi) +
#       #       xlab("Dilution (log units)") +
#       #       ylab("MFI") +
#       #       ggtitle("Standard Curve") +
#       #       theme_bw() +
#       #       theme( text = element_text(size=8) )
#       #       ps
#       #
#       # })
#       }
})

