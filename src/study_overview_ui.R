observeEvent(input$study_level_tabs, {

  req(input$readxMap_study_accession != "Click here",
      input$study_level_tabs == "Study Overview",
      input$main_tabs == "view_files_tab")

  if (input$study_level_tabs == "Study Overview") {
    cat("before preproccess data")
    showNotification(id = "load_overview_notification", "Loading Study Overview...", duration = NULL)
    pppd <- preprocess_plate_data(conn, currentuser(),input$readxMap_study_accession)
    cat("after preproccessing data")
    removeNotification("load_overview_notification")

    count_set <- pppd[[1]]
    plates <- pppd[[2]]
    # load the sample specimen
    sample_specimen <- pppd[[3]]
    # Load the summspec
    summ_spec <- pppd[[4]]
    standard_fit_data <- pppd[[5]]
    preped_data <- prep_analyte_fit_summary(summ_spec_in = summ_spec, standard_fit_res = standard_fit_data)

    pct_linn <- count_set[[5]]
    plate_cols <- grep("^plate_", names(pct_linn), value = TRUE)
    pct_linn$pct_accept <- rowMeans(pct_linn[, plate_cols], na.rm = TRUE)
    pct_linn <- pct_linn[, c("analyte", "antigen", "pct_accept")]

    n_conc <- count_set[[8]]
    plate_cols <- grep("^plate_", names(n_conc), value = TRUE)
    n_conc$sum_too_conc <- rowSums(n_conc[, plate_cols], na.rm = TRUE)
    n_conc <- n_conc[, c("analyte", "antigen", "sum_too_conc")]

    n_dilut <- count_set[[9]]
    plate_cols <- grep("^plate_", names(n_dilut), value = TRUE)
    n_dilut$sum_too_dilut <- rowSums(n_dilut[, plate_cols], na.rm = TRUE)
    n_dilut <- n_dilut[, c("analyte", "antigen", "sum_too_dilut")]

    aa_stats <- merge(pct_linn , n_conc, by = c("analyte","antigen"))
    aa_stats <- merge(aa_stats , n_dilut, by = c("analyte","antigen"))
    aa_stats$cell_text <- paste(
      paste0("%QLR:", round(aa_stats$pct_accept,digits = 1)),
      paste0("NTC:", aa_stats$sum_too_conc),
      paste0("NTD:", aa_stats$sum_too_dilut), sep = " "
    )
    aa_stats$bg_color <- get_bg_color(aa_stats$pct_accept)

    tcell_text <- pivot_wider(aa_stats, id_cols = "analyte", names_from = "antigen", values_from = "cell_text")
    tcell_bg <- pivot_wider(aa_stats, id_cols = "analyte", names_from = "antigen", values_from = "bg_color")

    output$study_overview_page <- renderUI({
      fluidPage(
      tagList(
      tabsetPanel(
        tabPanel(
          "Blanks, Controls, and Standards",
          br(),
          plotOutput("analyte_plate_specimen", height = "800px"),
          downloadButton("download_analyte_plate_specimen_plot", "Download Plot"),
          downloadButton("download_analyte_plate_specimen_data", "Downlaod Samples Summarized by Analyte, Plate, and Specimen"),
          DTOutput("sample_spec_plate_summary_table")
          #  plotlyOutput("study_arm_plot", width = "75vw")
        ),
        tabPanel(
          "High Aggregate and Low Bead Count",
          uiOutput("analyte_selector_beadUI"),
          uiOutput("specimen_selector_beadUI"),
          plotOutput("bead_count_summary_plot", height = "800px")
        ),
        # tabPanel(
        #   "Overall Sample Quality",
        #   div(
        #     style = "width: 75vw; overflow-x: auto;",
        #     plotOutput("plate_legend_plotb", height = 60)
        #   ),
        #   div(
        #     style = "width: 75vw; overflow-x: auto;",
        #     uiOutput("analyte_antigen_heatmap", width = "75vw")
        #   )
        # ),
        tabPanel(
          "Samples by Arm",
          plotOutput("arm_balance"),
          downloadButton("download_arm_balance_plot", "Download Plot"),
          DTOutput("arm_balance_table"),
          downloadButton("download_arm_balance_sample", "Download Sample Proportions Across Study Arms, Analyte, and Plate")
        ),
        tabPanel(
          "Samples by Timeperiod",
          plotOutput("analyte_antigen_timeperiod", height = "800px"),
          downloadButton("download_analyte_antigen_plot", "Download Plot"),
          downloadButton("download_analyte_antigen_plot_data", "Download Samples Summarized by Timeperiod"),
          br(),
          DTOutput("sample_spec_timeperiod_summary_table")
          #downloadablePlotUI("analyte_antigen_timeperiod"),
                             # downloadtypes = c("png"),
                             # download_hovertext = "Download analyte by antigen and timeperiod plot",
                             # height = "800px",
                             # btn_halign = "left"),
        ),


        # tabPanel(
        #   "Intraplate vs Interplate CV",
        #   plotOutput("inter_intra_cv", height = "800px"),
        #   downloadButton("download_inter_intra_cv_plot", "Downlaod Plot"),
        #   downloadButton("download_inter_intra_cv_data", "Download Intraplate and Interplate CV Data")
        # ),
        tabPanel(
          "Sample Estimate Quality by Plate and Antigen",
          uiOutput("fit_source_selectorUI"),
          uiOutput("analyte_selectorUI"),
          plotOutput("analyte_dilution_assessment", height = "800px"),#, height = "800px"),
          downloadButton("download_plot_dilution_assessment", "Download Plot"),
          downloadButton("download_plot_dilution_assessment_data", "Download Sample Estimate Quality by Plate and Antigen"),
         # hr(style = "border: none; height: 1px; background-color: black; margin: 20px 0;"),
          # fluidRow(
          # column(6, uiOutput("plate_selectorUI")),
          # column(6, uiOutput("plate_id_selectorUI"))),
          # #fluidRow(
          #   column(6, uiOutput("plateid_selectorUI")),
          #  # column(6, br(), actionButton("delete_plate", label = "Delete Selected Plate"))
          #  ),
          # hr(style = "border: none; height: 1px; background-color: black; margin: 20px 0;"),
          DTOutput("proportion_analyte_fit"),
          downloadButton("download_proportion_dilution_assessment_data", "Download Proportion Summary")
      ),
      # tabPanel(
      #   "Sample Quality by Plate",
      #   uiOutput("antigenSelectorUI"),
      #   div(
      #     style = "width: 75vw; overflow-x: auto;",
      #     plotOutput("plate_legend_plot", height = 60, width = "75vw")),
      #   div(
      #     style = "width: 75vw; overflow-x: auto;",
      #     uiOutput("plate_analyte_table"))
      # ),

      )
    ))})

    # Reactive values to store counts
    counts <- reactiveVal(count_set[[1]])
    stdcounts <- reactiveVal(count_set[[2]])
    blankcounts <- reactiveVal(count_set[[3]])
    cntrlcounts <- reactiveVal(count_set[[4]])
    linn <- reactiveVal(count_set[[5]])
    lowbeadn <- reactiveVal(count_set[[6]])
    hiaggn <- reactiveVal(count_set[[7]])
    concn <- reactiveVal(count_set[[8]])
    dilutn <- reactiveVal(count_set[[9]])
    abovelod <- reactiveVal(count_set[[10]])
    belowlod <- reactiveVal(count_set[[11]])

    pct_text <- reactiveVal(tcell_text)
    pct_bg_color <- reactiveVal(tcell_bg)

    output$antigenSelectorUI <- renderUI({
      req(counts())
      shinyWidgets::radioGroupButtons(
        inputId = "antigenSelector",
        label = "Select Antigen:",
        choices = unname(unlist(as.vector(unique(count_set[[1]]["antigen"])))),
        selected = unname(unlist(as.vector(unique(count_set[[1]]["antigen"]))))[1],
        status = "success"
      )
    })







    output$plate_analyte_table <- renderUI({
      req(input$antigenSelector)

      current_counts <- counts()[counts()$antigen == input$antigenSelector, ]
      rownames(current_counts) <- current_counts$analyte
      current_counts <- current_counts[ , ! names(current_counts) %in% c("analyte","antigen")]

      current_stdcounts <- stdcounts()[stdcounts()$antigen == input$antigenSelector, ]
      rownames(current_stdcounts) <- current_stdcounts$analyte
      current_stdcounts <- current_stdcounts[ , ! names(current_stdcounts) %in% c("analyte","antigen")]

      current_blankcounts <- blankcounts()[blankcounts()$antigen == input$antigenSelector, ]
      rownames(current_blankcounts) <- current_blankcounts$analyte
      current_blankcounts <- current_blankcounts[ , ! names(current_blankcounts) %in% c("analyte","antigen")]

      current_cntrlcounts <- cntrlcounts()[cntrlcounts()$antigen == input$antigenSelector, ]
      rownames(current_cntrlcounts) <- current_cntrlcounts$analyte
      current_cntrlcounts <- current_cntrlcounts[ , ! names(current_cntrlcounts) %in% c("analyte","antigen")]

      current_linn <- linn()[linn()$antigen == input$antigenSelector, ]
      rownames(current_linn) <- current_linn$analyte
      current_linn <- current_linn[ , ! names(current_linn) %in% c("analyte","antigen")]

      current_lowbeadn <- lowbeadn()[lowbeadn()$antigen == input$antigenSelector, ]
      rownames(current_lowbeadn) <- current_lowbeadn$analyte
      current_lowbeadn <- current_lowbeadn[ , ! names(current_lowbeadn) %in% c("analyte","antigen")]

      current_hiaggn <- hiaggn()[hiaggn()$antigen == input$antigenSelector, ]
      rownames(current_hiaggn) <- current_hiaggn$analyte
      current_hiaggn <- current_hiaggn[ , ! names(current_hiaggn) %in% c("analyte","antigen")]

      current_concn <- concn()[concn()$antigen == input$antigenSelector, ]
      rownames(current_concn) <- current_concn$analyte
      current_concn <- current_concn[ , ! names(current_concn) %in% c("analyte","antigen")]

      current_dilutn <- dilutn()[dilutn()$antigen == input$antigenSelector, ]
      rownames(current_dilutn) <- current_dilutn$analyte
      current_dilutn <- current_dilutn[ , ! names(current_dilutn) %in% c("analyte","antigen")]

      current_abovelod <- abovelod()[abovelod()$antigen == input$antigenSelector, ]
      rownames(current_abovelod) <- current_abovelod$analyte
      current_abovelod <- current_abovelod[ , ! names(current_abovelod) %in% c("analyte","antigen")]

      current_belowlod <- belowlod()[belowlod()$antigen == input$antigenSelector, ]
      rownames(current_belowlod) <- current_belowlod$analyte
      current_belowlod <- current_belowlod[ , ! names(current_belowlod) %in% c("analyte","antigen")]

      analyte_list <- rownames(current_counts)
      plate_list <- colnames(current_counts)

      # Build the table as a tagList
      table_rows <- list()

      # Header row (blank corner + plate names)
      header_cells <- list(tags$th("Analyte / Plate"))

      # Add plate headers
      for (j in 1:length(plate_list)) {
        header_cells[[length(header_cells) + 1]] <- tags$th(plate_list[j])
      }
      table_rows[[1]] <- tags$tr(header_cells)

      # For each analyte (row)
      for (i in 1:length(analyte_list)) {
        row_cells <- list()

        # Analyte name cell
        row_cells[[1]] <- tags$td(analyte_list[i])

        # For each plate (column)
        for (j in 1:length(plate_list)) {
          # Build button id using analyte and plate indices
          btn_id <- paste0("delete_", i, "_", j)
          count_val <- current_counts[i, j]
          count_std <- current_stdcounts[i, j]
          count_blank <- current_blankcounts[i, j]
          count_cntrl <- current_cntrlcounts[i, j]
          count_linn <- current_linn[i, j]
          count_lowbeadn <- current_lowbeadn[i, j]
          count_hiaggn <- current_hiaggn[i, j]
          count_concn <- current_concn[i, j]
          count_dilutn <- current_dilutn[i, j]
          count_abovelod <- current_abovelod[i, j]
          count_belowlod <- current_belowlod[i, j]

          bg_color <- get_bg_color(count_linn)
          text_nlowbead_color <- ifelse(count_lowbeadn > 0, "red", "white")
          text_nhighagg_color <- ifelse(count_hiaggn > 0, "red", "white")
          text_color <- "white"
          cell_text <- paste(paste0("X:",count_val),
                             paste0("S:",count_std),
                             paste0("B:",count_blank),
                             paste0("C:",count_cntrl),
                             paste0("NTC:", count_concn),
                             paste0("NTD:", count_dilutn),
                             paste0("aULOD:", count_abovelod),
                             paste0("bLLOD:", count_belowlod),
                             sep = " "
          )

          # Add delete button using actionButton
          delete_button <- actionButton(
            inputId = btn_id,
            label = "Delete",
            style = "margin-top: 5px; font-size: small; padding: 4px 8px;"
          )

          row_cells[[length(row_cells) + 1]] <- tags$td(
            style = paste0(
              "background-color:", bg_color, ";",
              "padding: 8px; text-align: center; vertical-align: middle;margin-right: 10px; padding: 8px; border: 1px solid red;"
            ),
            tags$div(
              style = paste0("color:", text_color, ";"),
              cell_text
            ),
            tags$div(
              style = paste0("color:", text_nlowbead_color, ";"),
              paste0("nLowBead:", count_lowbeadn)
            ),
            tags$div(
              style = paste0("color:", text_nhighagg_color, ";"),
              paste0("nHighAgg:",count_hiaggn)
            ),
            delete_button
          )
        }
        table_rows[[length(table_rows) + 1]] <- tags$tr(row_cells)
      }

      # Render the table
      tags$table(
        tags$caption(style="caption-side: bottom; text-align: left;", HTML("Key to the abbreviations:<br>
  X:test samples S:standard curve samples<br>
  B:blanks C:positive controls<br>
  NTC:too concentrated - above the quantifiable linear region<br>
  NTD:too diluted - below the quantifiable linear region<br>
  aULOD:above the upper limit of detection<br>
  bLLOD:below the lower limit of detection<br>
  nLowBead:bead count below low threshold<br>
  nHighAgg:% aggregate beads greater than threshold<br>
  NTC, NTD, aULOD and bLLOD are all derived from standard curve fitting.")
        ),
        style = "border-collapse: collapse; width: 80%;",
        border = 1,
        tags$style(
          HTML("
          table, th, td { border: 1px solid black; padding: 8px; text-align: center; }
          th { background-color: #f2f2f2; }
        ")
        ),
        table_rows
      )

    })

    output$plate_legend_plot <- renderPlot({
      gradient <- viridisLite::viridis(100, begin=0.01, end=0.95, option = "E")
      df <- data.frame(x = seq(0, 100, length.out=100), y = 1)
      ggplot(df, aes(x=x, y=y, fill=x)) +
        geom_tile() +
        scale_fill_gradientn(colors = gradient, limits = c(0,100), name = "count_linn") +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size=12),
          legend.position = "none"
        ) +
        xlab("% Quantifiable (Linear) Region") +
        ylab("")
    })

    output$plate_legend_plotb <- renderPlot({
      gradient <- viridisLite::viridis(100, begin=0.01, end=0.95, option = "E")
      df <- data.frame(x = seq(0, 100, length.out=100), y = 1)
      ggplot(df, aes(x=x, y=y, fill=x)) +
        geom_tile() +
        scale_fill_gradientn(colors = gradient, limits = c(0,100), name = "count_linn") +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size=12),
          legend.position = "none"
        ) +
        xlab("% Quantifiable (Linear) Region") +
        ylab("")
    })

    output$analyte_antigen_heatmap <- renderUI({

      current_pcttext <- pct_text()
      analyte_list <- unique(current_pcttext$analyte)
      current_pcttext <- current_pcttext[ , ! names(current_pcttext) %in% c("analyte")]
      antigen_list <- colnames(current_pcttext)

      current_bg <- pct_bg_color()
      current_bg <- current_bg[ , ! names(current_bg) %in% c("analyte")]

      # Build the table as a tagList
      table_rows <- list()

      # Header row (blank corner + plate names)
      header_cells <- list(tags$th("Analyte / Antigen"))

      # Add antigen headers
      for (j in 1:length(antigen_list)) {
        header_cells[[length(header_cells) + 1]] <- tags$th(antigen_list[j])
      }
      table_rows[[1]] <- tags$tr(header_cells)

      # For each analyte (row)
      for (i in 1:length(analyte_list)) {
        row_cells <- list()

        # Analyte name cell
        row_cells[[1]] <- tags$td(analyte_list[i])

        # For each antigen (column)
        for (j in 1:length(antigen_list)) {
          # Build button id using analyte and plate indices
          btn_id <- paste0("delete_", i, "_", j)
          cell_text <- current_pcttext[i, j]
          bg_color <- current_bg[i, j]
          text_color <- "white"
          row_cells[[length(row_cells) + 1]] <- tags$td(
            style = paste0(
              "background-color:", bg_color, ";",
              "padding: 8px; text-align: center; vertical-align: middle;margin-right: 10px; padding: 8px; border: 1px solid red;"
            ),
            tags$div(
              style = paste0("color:", text_color, ";"),
              cell_text
            )
          )
        }
        table_rows[[length(table_rows) + 1]] <- tags$tr(row_cells)
      }

      # Render the table
      tags$table(
        tags$caption(style="caption-side: bottom; text-align: left;", HTML("Key to the abbreviations:<br>
  %QLR:percent of samples in quantifiable linear region<br>
  NTC:too concentrated - above the quantifiable linear region<br>
  NTD:too diluted - below the quantifiable linear region<br>
  NTC, and NTD are all derived from standard curve fitting.")
        ),
        style = "border-collapse: collapse; width: 80%;",
        border = 1,
        tags$style(
          HTML("
          table, th, td { border: 1px solid black; padding: 8px; text-align: center; }
          th { background-color: #f2f2f2; }
        ")
        ),
        table_rows
      )
    })

    observe({
      # req(input$antigenSelector)

      current_counts <- counts()[counts()$antigen == input$antigenSelector, ]
      # current_counts <- counts()
      analyte_list <- unique(current_counts$analyte)
      rownames(current_counts) <- current_counts$analyte
      current_counts <- current_counts[ , ! names(current_counts) %in% c("analyte","antigen")]
      plate_list <- colnames(current_counts)

      # Loop through all buttons
      for (i in 1:length(analyte_list)) {
        for (j in 1:length(plate_list)) {
          local({
            row_idx <- i
            col_idx <- j
            btn_id <- paste0("delete_", row_idx, "_", col_idx)
            confirm_btn_id <- paste0("confirm_delete_", row_idx, "_", col_idx)
            cancel_btn_id <- paste0("cancel_delete_", row_idx, "_", col_idx)

            observeEvent(input[[btn_id]], {
              # showNotification(paste("Delete clicked for analyte", analyte_list[row_idx], "plate", plate_list[col_idx]))
              showModal(
                modalDialog(
                  title = "Confirm Delete",
                  paste("Are you sure you want to delete count for analyte",
                        analyte_list[row_idx], "and plate", plate_list[col_idx], "?"),
                  footer = tagList(
                    actionButton(confirm_btn_id, "Confirm"),
                    modalButton("Cancel")
                  ),
                  easyClose = TRUE
                )
              )


            }, ignoreInit = TRUE)

            observeEvent(input[[confirm_btn_id]], {
              removeModal()
              showNotification(paste("Deleting records for analyte", analyte_list[row_idx], "plate", plate_list[col_idx]))

              # Your delete logic here:
              # new_counts <- counts()
              # # Find the row and column indices in new_counts that match analyte and plate
              # row_to_modify <- which(new_counts$analyte == analyte_list[row_idx] & new_counts$antigen == input$antigenSelector)
              # if (length(row_to_modify) == 1) {
              #   new_counts[row_to_modify, plate_list[col_idx]] <- 0
              #   counts(new_counts)
              # }

              # reload query
              cat("\n\nDeleted count for analyte:", analyte_list[row_idx], "plate:", plate_list[col_idx], "\n\n")
              #           pull_delete_set_query <- glue::glue_sql("
              # SELECT DISTINCT study_accession, experiment_accession AS analyte, REPLACE(split_part(plate_id,'\',-1),' ','.') AS plateid
              # 	FROM madi_results.xmap_header
              # 	WHERE study_accession = {selected_study}
              # 	ORDER BY experiment_accession, antigen, plateid",
              #                                                   .con = conn)
              #           delete_set <- dbGetQuery(conn, pull_delete_set_query)
            })
          }) #end local
        }
      }

    })

    output$analyte_antigen_timeperiod <- renderPlot({
      plot_analyte_antigen_timeperiod()
    })

    plot_analyte_antigen_timeperiod <- function() {
      req(input$readxMap_study_accession)
      req(sample_specimen)
      microviz_kelly_pallete <-  c("#f3c300","#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
                                   "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
                                   "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")
      #sample_specimen_v <- sample_specimen
      study_configuration <- fetch_study_configuration(study_accession = input$readxMap_study_accession , user = currentuser())
      timeperiod_order <- strsplit(study_configuration[study_configuration$param_name == "timeperiod_order",]$param_character_value, ",")[[1]]
      index_named_vector <- setNames(seq_along(timeperiod_order), timeperiod_order)
      sample_spec_timeperiod <- summarise_by_timeperiod(sample_specimen)

      # Map the number of the timeperiods
      sample_spec_timeperiod$timeperiod_order <- purrr::map_int(
        sample_spec_timeperiod$timeperiod,
        ~ index_named_vector[[.x]]
      )

      make_timeperiod_grid(df = sample_spec_timeperiod, x_var = "analyte",
                           y_var = "antigen",
                           time_var = "timeperiod",
                           count_var = "n",
                           title_var = "Number of Samples by Analyte, Antigen, and Timeperiod",
                           time_var_order = "timeperiod_order",
                           time_var_palette = microviz_kelly_pallete)
    }

    output$sample_spec_timeperiod_summary_table <- renderDT({
      req(input$readxMap_study_accession)
      req(sample_specimen)

      study_configuration <- fetch_study_configuration(study_accession = input$readxMap_study_accession , user = currentuser())
      timeperiod_order <- strsplit(study_configuration[study_configuration$param_name == "timeperiod_order",]$param_character_value, ",")[[1]]
      index_named_vector <- setNames(seq_along(timeperiod_order), timeperiod_order)
      sample_spec_timeperiod <- summarise_by_timeperiod(sample_specimen)

      # Map the number of the timeperiods
      sample_spec_timeperiod$timeperiod_order <- purrr::map_int(
        sample_spec_timeperiod$timeperiod,
        ~ index_named_vector[[.x]]
      )
      dt <- create_timeperiod_table(sample_spec_timeperiod)
      datatable(dt, caption = "Number of Samples by Analyte, Antigen, and Timeperiod", filter = "top")
    })

    output$download_analyte_antigen_plot <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "analyte_antigen_timeperiod_plot.pdf", sep = "_")
      },
      content = function(file) {
        # Save the plot to the specified file
        ggsave(file, plot = plot_analyte_antigen_timeperiod(),
               device = "pdf",
               width = 20,
               height = 10,
               units = "in") # Specify device type
      }
    )

    output$download_analyte_antigen_plot_data <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "summarise_by_timeperiod.csv", sep = "_")
      }, content = function(file) {
        req(input$readxMap_study_accession)
        req(sample_specimen)
        #sample_specimen_v <- sample_specimen
        sample_spec_timeperiod <- summarise_by_timeperiod(sample_specimen)

        write.csv(sample_spec_timeperiod, file)
      }
    )


    output$analyte_plate_specimen <- renderPlot({
      plot_analyte_plate_specimen()
      # req(input$readxMap_study_accession)
      # req(summ_spec)
      # kelly_specimen_palette <-  c("blank" = "#f3c300",
      #                              "control" = "#2b3d26",
      #                              "standard" = "#a1caf1",
      #                              "sample" = "#8db600",
      #                               "low_bead_count" = "#f6a600",
      #                                "high_aggregate_beads" = "#be0032")
      #                              #   "#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
      #                              # "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
      #                              # "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")
      # #preped_data <- prep_plate_content_summary(summ_spec_df = summ_spec)
      # make_timeperiod_grid(df = summ_spec,
      #                      x_var = "analyte", y_var = "plate",
      #                      time_var = "specimen_type", count_var = "n",
      #                      time_var_order = "specimen_type_order",
      #                      time_var_palette = kelly_specimen_palette,
      #                      title_var = "Number of Samples by Analyte, Plate and Specimen Type")
    })

    plot_analyte_plate_specimen <- function() {
      req(input$readxMap_study_accession)
      req(summ_spec)

      # Only show the blanks standard and controls
      summ_spec <- summ_spec[summ_spec$specimen_type %in% c("blank", "standard", "control"), ]

      kelly_specimen_palette <-  c("blank" = "#f3c300",
                                   "control" = "#2b3d26",
                                   "standard" = "#a1caf1",
                                   "sample" = "#8db600",
                                   "low_bead_count" = "#f6a600",
                                   "high_aggregate_beads" = "#be0032")
      #   "#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
      # "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
      # "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")
      #preped_data <- prep_plate_content_summary(summ_spec_df = summ_spec)
      make_timeperiod_grid(df = summ_spec,
                           x_var = "analyte", y_var = "plate",
                           time_var = "specimen_type", count_var = "n",
                           time_var_order = "specimen_type_order",
                           time_var_palette = kelly_specimen_palette,
                           title_var = "Summary of Non-Sample Specimen Types by Plate and Analyte")#"Number of Samples by Analyte, Plate and Specimen Type")

    }

    output$sample_spec_plate_summary_table <- renderDT({
      req(input$readxMap_study_accession)
      req(summ_spec)
     # Only show the blanks standard and controls
      summ_spec <- summ_spec[summ_spec$specimen_type %in% c("blank", "standard", "control"), ]
      summ_spec <- summ_spec[, c("analyte", "plate", "specimen_type", "n")]
      datatable(summ_spec, caption = "Number of Non-Sample Specimen Types by Plate and Analyte", filter = "top")
    })

    output$download_analyte_plate_specimen_plot <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "analyte_plate_specimen_plot.pdf", sep = "_")
      },
      content = function(file) {
        # Save the plot to the specified file
        ggsave(file, plot = plot_analyte_plate_specimen(),
               device = "pdf",
               width = 20,
               height = 10,
               units = "in") # Specify device type
      }
    )

    output$download_analyte_plate_specimen_data <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "summarize_by_analyte_plate_specimen.csv", sep = "_")
      }, content = function(file) {
        req(input$readxMap_study_accession)
        req(summ_spec)


        write.csv(summ_spec, file)
      }
    )



 plot_inter_intra_cv <- function() {
   req(input$readxMap_study_accession)
   req(summ_spec)

   kelly_specimen_palette <-  c("blank" = "#f3c300",
                                "control" = "#2b3d26",
                                "standard" = "#a1caf1",
                                "sample" = "#8db600",
                                "low_bead_count" = "#f6a600",
                                "high_aggregate_beads" = "#be0032")

   interplate_cv_data <- make_interplate_summ_spec(summ_spec)

   summ_spec <- summ_spec[summ_spec$n > 1,]
   interplate_cv_data <- interplate_cv_data[!is.na(interplate_cv_data$interplate_cv_mfi), ]

   summ_spec_d <- summ_spec


   interplate_cv_data_v <- interplate_cv_data
   # missing_matches <- anti_join(summ_spec_d, interplate_cv_data_v, by = c("analyte", "antigen"))
   combined_summary <- dplyr::left_join(
     summ_spec,
     interplate_cv_data,
     by = c("analyte.x" = "analyte", "antigen" = "antigen")#by = c("analyte", "antigen")
   )

   combined_summary$specimen_type <- factor(combined_summary$specimen_type, levels = names(kelly_specimen_palette))

   # combined_summary_v <- combined_summary
   make_cv_scatterplot(
     df = combined_summary,
     x_var = "intraplate_cv_mfi",
     y_var = "interplate_cv_mfi",
     facet_var1 = "antigen",
     facet_var2 = "analyte",
     color_var = "specimen_type",
     title_var = "Intra vs Interplate CV by Antigen and Analyte",
     color_palette = kelly_specimen_palette
   )

 }
    output$inter_intra_cv <- renderPlot({
      plot_inter_intra_cv()
    })

    output$download_inter_intra_cv_plot <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "iner_intra_cv_plot.pdf", sep = "_")
      },
      content = function(file) {
        # Save the plot to the specified file
        ggsave(file, plot = plot_inter_intra_cv(),
               device = "pdf",
               width = 20,
               height = 10,
               units = "in") # Specify device type
      }
    )

    output$download_inter_intra_cv_data <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "iner_intra_cv_data.csv", sep = "_")
      },
      content = function(file) {
        req(input$readxMap_study_accession)
        req(summ_spec)

        kelly_specimen_palette <-  c("blank" = "#f3c300",
                                     "control" = "#2b3d26",
                                     "standard" = "#a1caf1",
                                     "sample" = "#8db600",
                                     "low_bead_count" = "#f6a600",
                                     "high_aggregate_beads" = "#be0032")

        interplate_cv_data <- make_interplate_summ_spec(summ_spec)

        summ_spec <- summ_spec[summ_spec$n > 1,]
        interplate_cv_data <- interplate_cv_data[!is.na(interplate_cv_data$interplate_cv_mfi), ]

        summ_spec_d <- summ_spec


        interplate_cv_data_v <- interplate_cv_data
        combined_summary <- dplyr::left_join(
          summ_spec,
          interplate_cv_data,
          by = c("analyte.x" = "analyte", "antigen" = "antigen")#by = c("analyte", "antigen")
        )

        combined_summary$specimen_type <- factor(combined_summary$specimen_type, levels = names(kelly_specimen_palette))

        write.csv(combined_summary, file)
      }
    )


    output$fit_source_selectorUI <- renderUI({
      study_sources <- fetch_study_sources(study_accession = input$readxMap_study_accession)
      radioButtons(
        inputId = "pull_fit_source",
        label = "Select Source:",
        choices = study_sources$source,
        selected = study_sources$source[1]
      )
    })


    output$analyte_selectorUI <- renderUI({
      req(preped_data)
      req(input$pull_fit_source)
      preped_data <- preped_data[preped_data$source == input$pull_fit_source,]
      analyte_choices <- unique(preped_data$analyte[!is.na(preped_data$analyte)])

        shinyWidgets::radioGroupButtons(
          inputId = "analyte_selector",
          label = "Select Analyte:",
          choices = analyte_choices,
          selected = analyte_choices[1],
          status = "success"
        )
    })

    output$analyte_selector_beadUI <- renderUI({
      req(preped_data)

      analyte_choices <- unique(preped_data$analyte[!is.na(preped_data$analyte)])

      shinyWidgets::radioGroupButtons(
        inputId = "analyte_selector_bead",
        label = "Select Analyte:",
        choices = analyte_choices,
        selected = analyte_choices[1],
        status = "success"
      )
    })
#
    output$specimen_selector_beadUI <- renderUI({
      req(preped_data)

      specimen_choices <- unique(preped_data[preped_data$specimen_type %in% c("blank", "control", "standard", "sample"), "specimen_type"])

      shinyWidgets::radioGroupButtons(
        inputId = "specimen_selector_bead",
        label = "Select Specimen:",
        choices = specimen_choices,
        selected = specimen_choices[1],
        status = "success"
      )
    })

    output$bead_count_summary_plot <- renderPlot({
      req(preped_data)
      req(input$analyte_selector_bead)
      req(input$specimen_selector_bead)
      bead_data <- preped_data[preped_data$specimen_type %in% c("blank", "control", "standard", "sample"), ]
      bead_data <- bead_data[, c("analyte","antigen", "plate","specimen_type","nhighbeadagg","nlowbead")]

      names(bead_data)[names(bead_data) == "nhighbeadagg"] <- "HighAggregates"
      names(bead_data)[names(bead_data) == "nlowbead"]  <- "LowBeads"

       bead_data <- bead_data %>%
        group_by(analyte, antigen, plate, specimen_type) %>%
        dplyr::summarise(
          LowBeads = sum(LowBeads, na.rm = TRUE),
          HighAggregates = sum(HighAggregates, na.rm = TRUE),
          .groups = "keep"
        )



      lbead_data <- pivot_longer(bead_data, cols = c("HighAggregates","LowBeads"),
                                 names_to = "Type", values_to = "N_wells")
      lbead_data$antigen <- factor(lbead_data$antigen)
      lbead_data$plate <- factor(lbead_data$plate)

      specimen_type = input$specimen_selector_bead
      analyte = input$analyte_selector_bead
      title = paste("Bead counts failing thresholds:",analyte, specimen_type)

      plot <- make_antigen_plate_bead(data=lbead_data,
                                    specimen_type = specimen_type,
                                    analyte = analyte,
                                    title = title)

      return(plot)
      # summary_totals <- bead_data %>%
      #   group_by(plate, antigen, specimen_type) %>%
      #   dplyr::summarise(
      #     HighAggregates = sum(HighAggregates),
      #     LowBeads = sum(LowBeads),
      #     .groups = "drop"
      #   )
      #
      #
      # plot_data <- summary_totals %>%
      #   pivot_longer(
      #     cols = c(HighAggregates, LowBeads),
      #     names_to = "type",
      #     values_to = "count"
      #   )
      #
      # #plot_data <- plot_data[plot_data$count != 0,]
      #
      # plot <- ggplot(plot_data, aes(x = plate, y = antigen, fill = count)) +
      #   geom_tile(color = "white") +
      #   facet_grid(specimen_type ~type) +
      #   #geom_point(aes(shape = specimen_type), size = 2) +
      #   #facet_wrap(~type) +
      #   scale_fill_viridis_c(option = "viridis",
      #                        limits = c(min(plot_data$count, na.rm = TRUE), max(plot_data$count, na.rm = TRUE)),
      #                        breaks = c(min(plot_data$count, na.rm = TRUE), round(max(plot_data$count)/2), max(plot_data$count))) +
      #   theme_minimal() +
      #   theme(
      #     strip.text = element_text(face = "bold")
      #   ) +
      #   # theme(
      #   #   panel.grid = element_blank()
      #   # ) +
      #   labs(title = "High Aggregate and Low Bead Counts per Plate, Antigen, and Specimen Type",
      #        x = "Plate", y = "Antigen", fill = "Count")
      #
      #
      # # plot <- ggplot(plot_data, aes(x = antigen, y = plate, fill = count)) +
      # #   geom_tile(data = subset(plot_data, count > 0), color = "white") +
      # #   facet_grid(specimen_type ~ type) +
      # #   scale_fill_viridis_c(
      # #     option = "viridis",
      # #     limits = c(min(plot_data$count[plot_data$count > 0], na.rm = TRUE),
      # #                max(plot_data$count, na.rm = TRUE)),
      # #     breaks = c(min(plot_data$count[plot_data$count > 0], na.rm = TRUE),
      # #                round(max(plot_data$count, na.rm = TRUE)/2),
      # #                max(plot_data$count, na.rm = TRUE))
      # #   ) +
      # #   theme_minimal() +
      # #   labs(
      # #     title = "High Aggregate and Low Bead Counts per Plate and Antigen",
      # #     x = "Antigen", y = "Plate", fill = "Count"
      # #   )
      #
      #
      # #summary_totals <- summary_totals[summary_totals$HighAggregates > 0 | summary_totals$LowBeads > 0,]
      # # summary_wide <- summary_totals %>%
      # #   pivot_wider(
      # #     names_from = antigen,
      # #     values_from = c(HighAggregates, LowBeads)
      # #   )
      #
      #
      # # names(bead_data)[names(bead_data) == "nhighbeadagg"] <- "HighAggregates"
      # # names(bead_data)[names(bead_data) == "nlowbead"]     <- "LowBeads"
      # # antigens <- sort(unique(bead_data$antigen))
      # # plates <- sort(unique(bead_data$plate))
      # # table_cells <- matrix(vector("list", length(antigens) * length(plates)),
      # #                       nrow = length(antigens), ncol = length(plates),
      # #                       dimnames = list(antigens, plates))
      # #
      # # for (a in antigens) {
      # #   for (p in plates) {
      # #     sub <- bead_data %>%
      # #       filter(antigen == a, plate == p) %>%
      # #       mutate(total = HighAggregates + LowBeads) %>%
      # #       filter(total > 0) %>%
      # #       select(specimen_type, HighAggregates, LowBeads)
      # #     # If there are rows, convert to a data.frame with specimen_type as row names
      # #     if (nrow(sub) > 0) {
      # #       mat <- as.data.frame(sub[, c("HighAggregates", "LowBeads")],
      # #                            row.names = sub$specimen_type,
      # #                            stringsAsFactors = FALSE)
      # #       table_cells[a, p][[1]] <- mat
      # #     } else {
      # #       table_cells[a, p][[1]] <- NULL
      # #     }
      # #   }
      # # }
      # #
      # #
      # # for (a in antigens) {
      # #   cat("Antigen:", a, "\n")
      # #   for (p in plates) {
      # #     cat(" Plate:", p, "\n")
      # #     cell <- table_cells[a, p][[1]]
      # #     if (is.null(cell)) {
      # #       cat("  (no specimen_types with total > 0)\n")
      # #     } else {
      # #       print(cell)
      # #     }
      # #   }
      # #   cat("\n")
      # # }
      # return(plot)
    })



    output$plate_selectorUI <- renderUI({
      req(preped_data)
      preped_data_v <- preped_data
      shinyWidgets::radioGroupButtons(
        inputId = "delete_plate_selector",
        label = "Select Plate to Delete:",
        choices = unique(preped_data$plate),
        selected = unique(preped_data$plate)[1],
        status = "success"
      )
    })



    output$plate_id_selectorUI <- renderUI({
      req(preped_data)
      preped_data <- preped_data[preped_data$analyte == input$analyte_selector & preped_data$plate == input$delete_plate_selector, ]
      shinyWidgets::radioGroupButtons(
        inputId = "delete_plate_id_selector",
        label = "plate_id to Delete:",
        choices = unique(preped_data$plate_id),
        selected = unique(preped_data$plate_id)[1],
        status = "success"
      )
    })

    output$plateid_selectorUI <- renderUI({
      req(preped_data)
      preped_data <- preped_data[preped_data$analyte == input$analyte_selector & preped_data$plate == input$delete_plate_selector, ]
      shinyWidgets::radioGroupButtons(
        inputId = "delete_plateid_selector",
        label = "plateid to Delete:",
        choices = unique(preped_data$plateid),
        selected = unique(preped_data$plateid)[1],
        status = "success"
      )
    })

    # observeEvent(input$delete_plate, {
    #   # showNotification(paste("Delete clicked for analyte", analyte_list[row_idx], "plate", plate_list[col_idx]))
    #   showModal(
    #     modalDialog(
    #       title = "Confirm Delete",
    #       paste("Are you sure you want to delete count for analyte",
    #             input$analyte_selector, "and plate", input$delete_plate_selector, "?. This will delete the header,
    #             buffers, controls, standards, and standard fits."),
    #       footer = tagList(
    #         #actionButton("confirm_plate_delete", "Confirm Deletion"),
    #         modalButton("Cancel")
    #       ),
    #       easyClose = TRUE
    #     )
    #   )
    #
    #
    # }, ignoreInit = TRUE)


    # observe({
    #   shinyjs::disable("confirm_plate_delete")
    # })




    plot_analyte_plate_model <- function() {
      req(preped_data)
      req(input$analyte_selector)

     analyte_summary_plot <- plot_preped_analyte_fit_summary(preped_data =preped_data , analyte_selector = input$analyte_selector)

     return(analyte_summary_plot[[1]])

    }

    get_analyte_plate_proportion <- function() {
      req(preped_data)
      req(input$analyte_selector)

      analyte_summary_plot <- plot_preped_analyte_fit_summary(preped_data = preped_data , analyte_selector = input$analyte_selector)

      return(analyte_summary_plot[[2]])

    }

    output$analyte_dilution_assessment <- renderPlot({
      plot_analyte_plate_model()

    })

    output$download_plot_dilution_assessment <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "dilution_assessment.pdf", sep = "_")
      },
      content = function(file) {
        # Save the plot to the specified file
        ggsave(file, plot = plot_analyte_plate_model(),
               device = "pdf",
               width = 20,
               height = 10,
               units = "in") # Specify device type
      }
    )

    output$download_plot_dilution_assessment_data <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "analyte_fit_specimen_data.csv", sep = "_")
      },
      content = function(file) {
        req(preped_data)
        write.csv(preped_data, file)

      }
    )

    output$download_proportion_dilution_assessment_data <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "analyte_fit_proportion_specimen_data.csv", sep = "_")
      },
      content = function(file) {

        proportion_df <- get_analyte_plate_proportion()

        write.csv(proportion_df, file)

      }
    )

    output$proportion_analyte_fit <- renderDT({

      proportion_df <- get_analyte_plate_proportion()
      proportion_df <- proportion_df[, c("plate", "antigen", "analyte", "model_class", "crit", "fit_category", "count", "proportion")]
      datatable(proportion_df, caption = "Sample Estimate Quality by Plate and Antigen", filter = "top")
    })



    plot_arm_balance  <- function() {
      req(input$readxMap_study_accession)
      microviz_kelly_pallete <-  c( "#a1caf1","#f38400", "#f3c300","#875692","#be0032","#c2b280","#848482",
                                   "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
                                   "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")

      study_configuration <- fetch_study_configuration(study_accession = input$readxMap_study_accession , user = currentuser())
      reference_arm <- strsplit(study_configuration[study_configuration$param_name == "reference_arm",]$param_character_value, ",")[[1]]
      if (is.null(reference_arm)) {
        reference_arm <- fetch_study_arms(study_accession =input$readxMap_study_accession)$agroup[1]
      }
      all_arms <- fetch_study_arms(study_accession = input$readxMap_study_accession)$agroup
      other_arms <- all_arms[all_arms != reference_arm]

      sorted_arms <- c(reference_arm, other_arms)

      prepared_arm_data <- prepare_arm_balance_data(sample_specimen, sorted_arms)

     plot <- make_timeperiod_grid_stacked(
        df = prepared_arm_data,
        x_var = "analyte",
        y_var = "plate",
        time_var = "arm",
        count_var = "proportion",
        time_var_order = "agroup_order",
        time_var_palette = microviz_kelly_pallete,
        title_var = paste(input$readxMap_study_accession, "- Proportion of Samples by Study Arms, Plate, and Analyte")
      )

     return(plot)
    }

    output$arm_balance <- renderPlot({
      plot_arm_balance()

    })

    output$download_arm_balance_plot <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "arm_balance.pdf", sep = "_")
      },
      content = function(file) {
        # Save the plot to the specified file
        ggsave(file, plot = plot_arm_balance(),
               device = "pdf",
               width = 20,
               height = 10,
               units = "in") # Specify device type
      }
    )

    output$arm_balance_table <- renderDT({
      req(input$readxMap_study_accession)

      study_configuration <- fetch_study_configuration(study_accession = input$readxMap_study_accession , user = currentuser())
      reference_arm <- strsplit(study_configuration[study_configuration$param_name == "reference_arm",]$param_character_value, ",")[[1]]
      if (is.null(reference_arm)) {
        reference_arm <- fetch_study_arms(study_accession =input$readxMap_study_accession)$agroup[1]
      }
      all_arms <- fetch_study_arms(study_accession = input$readxMap_study_accession)$agroup
      other_arms <- all_arms[all_arms != reference_arm]

      sorted_arms <- c(reference_arm, other_arms)

      prepared_arm_data <- prepare_arm_balance_data(sample_specimen, sorted_arms)

      # initial order
      prepared_arm_data <- prepared_arm_data[order(prepared_arm_data$agroup_order),]
      prepared_arm_data <- prepared_arm_data[, c("plate", "analyte", "agroup", "proportion")]

     names(prepared_arm_data)[names(prepared_arm_data) == "agroup"] <- "arm"

      datatable(prepared_arm_data, caption = "Sample Proportions Across Study Arms Stratified by Analyte and Plate", filter = "top")

    })

    output$download_arm_balance_sample <- downloadHandler(
      filename = function() {
        paste(input$readxMap_study_accession, "analyte_arm_balance_sample_proportion.csv", sep = "_")
      },
      content = function(file) {
        req(input$readxMap_study_accession)

        study_configuration <- fetch_study_configuration(study_accession = input$readxMap_study_accession , user = currentuser())
        reference_arm <- strsplit(study_configuration[study_configuration$param_name == "reference_arm",]$param_character_value, ",")[[1]]
        if (is.null(reference_arm)) {
          reference_arm <- fetch_study_arms(study_accession =input$readxMap_study_accession)$agroup[1]
        }
        all_arms <- fetch_study_arms(study_accession = input$readxMap_study_accession)$agroup
        other_arms <- all_arms[all_arms != reference_arm]

        sorted_arms <- c(reference_arm, other_arms)

        prepared_arm_data <- prepare_arm_balance_data(sample_specimen, sorted_arms)

        # initial order
        prepared_arm_data <- prepared_arm_data[order(prepared_arm_data$agroup_order),]

        write.csv(prepared_arm_data, file)

      }
    )

    output$study_arm_plot <- renderPlotly({
      req(input$readxMap_study_accession)

      study_arm_df <- fetch_study_participant_arms(study_accession = input$readxMap_study_accession)
      study_arm_distribution <- plot_study_arm_distribution(patients_arm = study_arm_df )
      return(study_arm_distribution)
    })


  } # end in study overview

})
