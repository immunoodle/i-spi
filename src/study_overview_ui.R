library(RPostgres)
library(glue)
library(DBI)
library(plotly)
library(ggplot2)
library(shiny)
library(tidyr)
library(extras)
library(stringr)
library(stringi)
library(dplyr)
library(here)
library(viridis)
library(shinyWidgets)
library(htmltools)

source(here("./src/study_overview_functions.R"), local = TRUE)

current_user <- "mscotzens@gmail.com"
selected_study <- "MADI_01"

pppd <- preprocess_plate_data(current_user, selected_study)
count_set <<- pppd[[1]]
plates <- pppd[[2]]

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

ui <- fluidPage(
  titlePanel("Loaded data - % in Quantifiable Region"),
  tabsetPanel(
    tabPanel(
      "Analyte-Plate by Antigen",
      uiOutput("antigenSelectorUI"),
      plotOutput("plate_legend_plot", height = 60),
      uiOutput("plate_analyte_table")
    ),
    tabPanel(
      "Analyte-Antigen",
      plotOutput("plate_legend_plotb", height = 60),
      uiOutput("analyte_antigen_heatmap")
    )
  )
)

server <- function(input, output, session) {

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

    current_counts <<- counts()[counts()$antigen == input$antigenSelector, ]
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

    current_linn <<- linn()[linn()$antigen == input$antigenSelector, ]
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

    analyte_list <<- rownames(current_counts)
    plate_list <<- colnames(current_counts)

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
        count_linn <<- current_linn[i, j]
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

}

shinyApp(ui, server)
