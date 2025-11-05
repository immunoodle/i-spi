tags$head(
  tags$style(HTML("
    .datatables {
      width: 100% !important;
      margin: 0 !important;
    }
    .dataTables_wrapper {
      width: 100%;
      margin: 0 auto;
      padding: 0 10px;
    }
    .panel-collapse {
      padding: 15px;
    }
  "))
)

standard_curve_table_list <- list()

data_summary <- list()

std_curve_tab_active <- reactiveVal(FALSE)


fetch_antigen_family_table <- function(study_accession) {
  if (study_accession == "reset") {
    result_df <- NULL
  } else {
    query <- paste0("
      SELECT xmap_antigen_family_id, study_accession, experiment_accession, antigen, antigen_family, standard_curve_concentration,
      antigen_name, virus_bacterial_strain, antigen_source, catalog_number, l_asy_min_constraint, l_asy_max_constraint, l_asy_constraint_method
      FROM madi_results.xmap_antigen_family
      WHERE study_accession = '", study_accession, "'")

    # Run the query and fetch the result as a data frame
    result_df <- dbGetQuery(conn, query)

    if (nrow(result_df) == 0 || is.null(result_df)) {
      # check if result df is null or 0 rows and if so run additional query insert.
      insert_query <- paste0("INSERT INTO madi_results.xmap_antigen_family(
                	study_accession, antigen, antigen_family, standard_curve_concentration)
                	SELECT DISTINCT '",study_accession,"', antigen, 'All Antigens', 10000
                	FROM madi_results.xmap_sample
                	WHERE study_accession = '", study_accession, "'
                	ORDER BY antigen;")
      dbExecute(conn, insert_query)
      result_df <- dbGetQuery(conn, query)
    } else {
  #}  ## Add new antigens to table
      insert_query <- paste0("
      INSERT INTO madi_results.xmap_antigen_family (study_accession, antigen, antigen_family, standard_curve_concentration)
      SELECT DISTINCT s.study_accession, s.antigen, 'All Antigens', 10000
      FROM madi_results.xmap_sample s
      WHERE s.study_accession = '", study_accession, "'
        AND NOT EXISTS (
          SELECT 1 FROM madi_results.xmap_antigen_family f
          WHERE f.study_accession = s.study_accession
            AND f.antigen = s.antigen
        );")
      dbExecute(conn, insert_query)

     result_df <- dbGetQuery(conn, query)
    }
  } # end outer else statement

  return(result_df)
}

# The main rendering logic wrapped in a reactive expression
render_antigen_family <- reactive({
  req(input$readxMap_study_accession)

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  # Fetch data once
  antigen_families_rv(fetch_antigen_family_table(selected_study))

  # Debug output
  cat("Antigen families data updated for:", selected_study, "\n")
  print(head(antigen_families_rv()))

  # Render the instructions
  output$antigen_family_instructions <- renderUI({
    req(selected_study)
    HTML(paste("<span style='font-size: 1.5em;'>To edit the family of an antigen for",
               selected_study,
               "click on the cell in the Antigen Family column.",
               "After making the change, click anywhere outside the cell to save the update.<br></span>"))
  })

  # Render the data table
  output$antigenFamilyUI <- renderUI({
    req(antigen_families_rv(), selected_study)
    tagList(
      fluidRow(
        div(
          style = "width: 100%; padding: 0 15px;",  # Added container styling
          span(
            div(style = "display:inline-block; margin-bottom: 10px;",
                title = "Info",
                icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                     `data-placement` = "right",
                     title = paste("To edit the family of an antigen for", selected_study,
                                   " click on the cell in the Antigen Family column.",
                                   " After making the change, click anywhere outside the cell to save the update"),
                     `data-html` = "true")
            )
          ),
          br(),
          div(
            style = "width: 100%; overflow-x: auto;",  # Added wrapper for table
            DTOutput("antigen_family_table")
          )
        )
      )
    )
  })

})
#   # And modify your renderDT to be more responsive:
#   output$antigen_family_table <- renderDT({
#     req(antigen_families_rv())
#     cat("Rendering datatable\n")
#     datatable(antigen_families_rv(),
#               options = list(
#                 pageLength = 50,
#                 scrollX = TRUE,
#                 scrollY = "400px",
#                 autoWidth = TRUE,  # Added this
#                 responsive = TRUE, # Added this
#                 order = list(list(0, 'asc')),
#                 columnDefs = list(
#                   list(className = 'dt-center', targets = '_all')
#                 )
#               ),
#               editable = list(
#                 target = 'cell',
#                 disable = list(columns = c(0:3))
#               ),
#               selection = 'none',
#               class = 'cell-border stripe hover'  # Added styling classes
#     ) %>%
#       formatStyle(columns = 1:ncol(antigen_families_rv()),  # Added column styling
#                   backgroundColor = 'white',
#                   borderBottom = '1px solid #ddd')
#   })
# })
#
# # Handle initial page load
# observe({
#   req(input$readxMap_study_accession)
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Initial load of Study Overview tab\n")
#     render_antigen_family()
#   }
# })
#
# # Handle tab changes
# observeEvent(input$study_level_tabs, {
#   req(input$readxMap_study_accession)
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Tab changed to Study Overview\n")
#     render_antigen_family()
#   }
# })
#
# # Handle study selection changes
# observeEvent(input$readxMap_study_accession, {
#   if (input$study_level_tabs == "Study Overview") {
#     cat("Study selection changed\n")
#     render_antigen_family()
#   }
# })
