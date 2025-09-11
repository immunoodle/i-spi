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

# antigen family table editable
observeEvent(input$antigen_family_table_cell_edit, {
  info <- input$antigen_family_table_cell_edit
  row_num <- info$row
  col_num <- info$col
  new_value <- info$value

  current_data <- antigen_families_rv()
  col_name <- colnames(current_data)[col_num]
  row_id <- current_data$xmap_antigen_family_id[row_num]

  message("Edited column:", col_name, " | New value:", new_value)

  if (col_name == "antigen_family") {
    new_value <- as.character(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET antigen_family = $1 WHERE xmap_antigen_family_id = $2"

    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$antigen_family[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("antigen_family updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating antigen_family:", e$message), type = "error")
    })

  } else if (col_name == "standard_curve_concentration") {
    new_value <- as.numeric(new_value)
    update_query <- "UPDATE madi_results.xmap_antigen_family
                     SET standard_curve_concentration = $1 WHERE xmap_antigen_family_id = $2"

    tryCatch({
      dbExecute(conn, update_query, params = list(new_value, row_id))
      current_data$standard_curve_concentration[row_num] <- new_value
      antigen_families_rv(current_data)
      showNotification("standard_curve_concentration updated successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating standard_curve_concentration:", e$message), type = "error")
    })
  }
})

# observeEvent(input$antigen_family_table_cell_edit, {
#   info <- input$antigen_family_table_cell_edit
#   row_num <- info$row
#   col_num <- info$col
#   new_value <- as.character(info$value)
#   current_data <- antigen_families_rv()
#   row_id <- current_data$xmap_antigen_family_id[row_num]
#
#   update_query <- "UPDATE madi_results.xmap_antigen_family
#                       SET antigen_family = $1
#                       WHERE xmap_antigen_family_id = $2"
#
#   tryCatch({
#     dbExecute(conn, update_query, params = list(new_value, row_id))
#
#     # Update the reactive value with new data
#     current_data$antigen_family[row_num] <- new_value
#     antigen_families_rv(current_data)
#
#     showNotification(
#       "Antigen Family updated successfully",
#       type = "message"
#     )
#   }, error = function(e) {
#     showNotification(
#       paste("Error updating antigen family:", e$message),
#       type = "error"
#     )
#   })
# })

render_study_parameters <- reactive({

#  req(input$readxMap_study_accession)
  #req(study_config)
  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_sources <- fetch_study_sources(study_accession = selected_study)
  study_arms <- fetch_study_arms(study_accession = selected_study)
  study_timeperiods <- fetch_study_timeperiods(study_accession = selected_study)

  # study_config <- study_config[study_config$param_group == "antigen_family",]
  # antigen_family_order_params <- study_config[study_config$param_name == "antigen_family_order",]
  # antigen_order_params <- study_config[study_config$param_name == "antigen_order",]
  #
  # antigen_family_order <- strsplit(antigen_family_order_params$param_character_value, ",")[[1]]
  # antigen_order <- strsplit(antigen_order_params$param_character_value, ",")[[1]]
  #
  # antigen_family_df <- fetch_antigen_family_table(selected_study)
  # antigen_family_df$antigen_family <- factor(antigen_family_df$antigen_family, levels = antigen_family_order)
  # antigen_family_df <- antigen_family_df[order(antigen_family_df$antigen_family), , drop = FALSE]
  #
  # antigen_family_df$antigen <- factor(antigen_family_df$antigen, levels = antigen_order)
  # antigen_family_df <- antigen_family_df[order(antigen_family_df$antigen), , drop = FALSE]
  # # Fetch data once
  # antigen_families_rv(antigen_family_df)

  # Fetch data once
  antigen_families_rv(fetch_antigen_family_table(selected_study))

  # Debug output
  #cat("Antigen families data updated for:", selected_study, "\n")

 study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())
  output$studyParameters_UI <- renderUI({
    tagList(
    conditionalPanel(
      condition = "input.readxMap_study_accession == 'Click here'",
      HTML("<h3>No study selected. Choose or create a study to change study settings.</h3>")
    ),

  #  req(selected_study, currentuser())
    conditionalPanel(condition = "input.readxMap_study_accession != 'Click here'",
    tagList(
      HTML(paste0("<h3>Change ", selected_study, " study settings for ", currentuser(), "</h3>")),
      tabsetPanel(
        id = "study_params_section_tab",
        tabPanel(
          "QC Basic Parameters",
          radioGroupButtons(
            inputId = "basic_qc_params",
            label = "",
            choices = c("Plate Label Editor", "Bead Count Parameters", "Standard Curve Parameters" ),
            selected = "Plate Label Editor"
          ),
          conditionalPanel(
            condition = "input.basic_qc_params == 'Plate Label Editor'",
            uiOutput("plate_management_UI")
          ),
          conditionalPanel(
            condition = "input.basic_qc_params == 'Bead Count Parameters'",
            uiOutput("bead_count_config")
          ),
          conditionalPanel(
            condition = "input.basic_qc_params == 'Standard Curve Parameters'",
            uiOutput("standard_curve_config")
          )
        ),
        tabPanel(
          "Advanced Parameters",
          radioGroupButtons(
            inputId = "advanced_qc_params",
            label = "",
            choices = c("Antigen Family Parameters", "Dilution Analysis Parameters", "Subgroup Parameters"),
            selected = "Antigen Family Parameters"
          ),

          conditionalPanel(
            condition = "input.advanced_qc_params == 'Antigen Family Parameters'",
            uiOutput("antigen_family_config")),

          conditionalPanel(
            condition = "input.advanced_qc_params == 'Dilution Analysis Parameters'",
            uiOutput("dilution_analysis_config")),

          conditionalPanel(
            condition = "input.advanced_qc_params == 'Subgroup Parameters'",
            uiOutput("subgroup_config"))

          )
        ),
        #)
      #uiOutput("plate_management_UI"),
      # uiOutput("bead_count_config"),
      # uiOutput("dilution_analysis_config"),
      # uiOutput("standard_curve_config"),


      # bsCollapse(
      #   id = "advanced_parameters",
      #   bsCollapsePanel(
      #   title = "Advanced Parameters",
      #   uiOutput("antigen_family_config"),
      #   uiOutput("dilution_analysis_config"),
      #   uiOutput("subgroup_config"),
      #   style = "primary")
      # ),
      conditionalPanel(
        condition = "!(input.basic_qc_params == 'Plate Label Editor' && input.study_params_section_tab == 'QC Basic Parameters')",
        actionButton(inputId = "reset_user_config", label = "Reset Study Parameters"),
        uiOutput("user_parameter_download")
      ),
    )
    )
    )
  })



  output$antigen_family_config <- renderUI({
    req(study_config)
    req(input$advanced_qc_params == 'Antigen Family Parameters')
    #study_config <- study_config_rv()
    study_config <- study_config[study_config$param_group == "antigen_family",]
    antigen_family_order_params <- study_config[study_config$param_name == "antigen_family_order",]
    antigen_order_params <- study_config[study_config$param_name == "antigen_order",]

    antigen_family_choices <- unique(antigen_families_rv()$antigen_family)
    antigen_choices <- unique(antigen_families_rv()$antigen)

    antigen_family_order_val <- antigen_family_order_params$param_character_value
    if (!is.null(antigen_family_order_val) && length(antigen_family_order_val) > 0 && !all(is.na(antigen_family_order_val))) {
      default_db_antigen_family_order <- strsplit(antigen_family_order_val, ",")[[1]]
    } else {
      default_db_antigen_family_order <- antigen_family_choices

    }
    # default_db_antigen_family_order <- strsplit(antigen_family_order_params$param_character_value, ",")[[1]]
    # if (!all(default_db_antigen_family_order %in% antigen_family_choices)) {
    #   #if (!default_db_timeperiod_order %in% timeperiod_choices) {
    #   default_db_antigen_family_order <- antigen_family_choices# fallback
    # }
    db_antigen_order_val <- antigen_order_params$param_character_value
    if (!is.null(db_antigen_order_val) && length(db_antigen_order_val) > 0 && !all(is.na(db_antigen_order_val))) {
      default_db_antigen_order <- strsplit(db_antigen_order_val, ",")[[1]]
    } else {
      default_db_antigen_order <- antigen_choices# fallback
    }
    # default_db_antigen_order <- strsplit(antigen_order_params$param_character_value, ",")[[1]]
    # if (!all(default_db_antigen_order %in% antigen_choices)) {
    #   #if (!default_db_timeperiod_order %in% timeperiod_choices) {
    #   default_db_antigen_order <- antigen_choices# fallback
    # }
    mainPanel(
    # bsCollapse(
    #   id = "antigen_config_collapse",
    #   bsCollapsePanel(
    #     title = "Antigen Family Parameters",

      #HTML("<h4><strong>Antigen Family</strong></h4>"),
      HTML(paste0("<h4>Order the antigen family and antigens from most important antigen family and antigen from left to right. </h4>")),
      orderInput(
        inputId = antigen_family_order_params$param_name,
        label = antigen_family_order_params$param_label,
        items = default_db_antigen_family_order#unique(antigen_families_rv()$antigen_family)
      ),
      uiOutput("antigen_family_order_warning"),
      orderInput(
        inputId = antigen_order_params$param_name,
        label = antigen_order_params$param_label,
        items = default_db_antigen_order# unique(antigen_families_rv()$antigen)
      ),
      uiOutput("antigen_order_warning"),
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
        )
      ),
      fluidRow(
        div(
          style = "width: 100%; overflow-x: auto;",
          DTOutput("antigen_family_table")
        )

      ),
      actionButton(inputId = "save_antigen_family_settings",
                   "Save")

    #   style = "primary"
    # ) # end panel
    )
  })

  output$antigen_family_table <- renderDT({
    req(antigen_families_rv())
    #req(study_config)
    # study_config <- study_config[study_config$param_group == "antigen_family",]
    # antigen_family_order_params <- study_config[study_config$param_name == "antigen_family_order",]
    # antigen_order_params <- study_config[study_config$param_name == "antigen_order",]
    #
    # antigen_family_order <- strsplit(antigen_family_order_params$param_character_value, ",")[[1]]
    # antigen_order <- strsplit(antigen_order_params$param_character_value, ",")[[1]]
    #
    #
     cat("Rendering datatable\n")
    # antigen_family_df <- antigen_families_rv()
    # antigen_family_df$antigen_family <- factor(antigen_family_df$antigen_family, levels = antigen_family_order)
    # antigen_family_df <- antigen_family_df[order(antigen_family_df$antigen_family), , drop = FALSE]
    #
    # antigen_family_df$antigen <- factor(antigen_family_df$antigen, levels = antigen_order)
    # antigen_family_df <- antigen_family_df[order(antigen_family_df$antigen), , drop = FALSE]
    # antigen_families_rv(antigen_family_df)

    datatable(antigen_families_rv(),
              options = list(
                pageLength = 50,
                scrollX = TRUE,
                scrollY = "400px",
                autoWidth = TRUE,  # Added this
                responsive = TRUE, # Added this
                order = list(list(0, 'asc')),
                columnDefs = list(
                  list(className = 'dt-center', targets = '_all')
                )
              ),
              editable = list(
                target = 'cell',
                disable = list(columns = c(0:3))
              ),
              selection = 'none',
              class = 'cell-border stripe hover'  # Added styling classes
    ) %>%
      formatStyle(columns = 1:ncol(antigen_families_rv()),  # Added column styling
                  backgroundColor = 'white',
                  borderBottom = '1px solid #ddd')
  })

  output$bead_count_config <- renderUI({
   req(study_config)
   req(input$basic_qc_params == 'Bead Count Parameters')
    # req(study_config_rv())
    # study_config <- study_config_rv()
    min_val_lower_bc <- strsplit(study_config[study_config$param_name == "lower_bc_threshold",]$param_choices_list, ",")[[1]][1]
    min_val_upper_bc <- strsplit(study_config[study_config$param_name == "upper_bc_threshold",]$param_choices_list, ",")[[1]][1]
    failed_well_params <-  study_config[study_config$param_name == "failed_well_criteria",]
    failed_well_params_choices <- strsplit(failed_well_params$param_choices_list, ",")[[1]]
    min_val_pct_agg_threshold <- strsplit(study_config[study_config$param_name == "pct_agg_threshold",]$param_choices_list, ",")[[1]][1]
#

    mainPanel(
#     bsCollapse(
#       id = "bead_count_config_collapse",
#       bsCollapsePanel(
#         title = "Bead Count Parameters",
     # HTML("<h4><strong>Bead Count</strong></h4>"),
      numericInput(inputId = "lower_bc_threshold",
                   label =  study_config[study_config$param_name == "lower_bc_threshold",]$param_label,
                   value = study_config[study_config$param_name == "lower_bc_threshold",]$param_integer_value,
                   min = if (min_val_lower_bc == "NA") NA else min_val_lower_bc),
      numericInput(inputId = "upper_bc_threshold",
                   label =  study_config[study_config$param_name == "upper_bc_threshold",]$param_label,
                   value = study_config[study_config$param_name == "upper_bc_threshold",]$param_integer_value,
                   min = if (min_val_upper_bc == "NA") NA else min_val_upper_bc),

     numericInput(inputId = "pct_agg_threshold",
                  label = study_config[study_config$param_name == "pct_agg_threshold",]$param_label,
                  value = study_config[study_config$param_name == "pct_agg_threshold",]$param_integer_value,
                  min = if (min_val_pct_agg_threshold == "NA") NA else min_val_pct_agg_threshold),


      # failed well criteria
      radioButtons(failed_well_params$param_name,
                   label = failed_well_params$param_label,
                   choices = failed_well_params_choices,
                   selected = study_config[study_config$param_name == "failed_well_criteria",]$param_character_value),

     uiOutput("failed_well_warning"),

     actionButton(inputId = "save_bead_count_params",
                  label = "Save"),


      # style = "primary"
      # )
    )
  })
#
  output$dilution_analysis_config <- renderUI({
    req(study_config)
    req(input$advanced_qc_params == 'Dilution Analysis Parameters')
    # req(study_config_rv())
    # study_config <- study_config_rv()

    study_config <- study_config[study_config$param_group == "dilution_analysis",]
    node_order_params <-  study_config[study_config$param_name == "node_order",]
    node_order_params_choices <- strsplit(node_order_params$param_choices_list, ",")[[1]]
    valid_gate_class_params <- study_config[study_config$param_name == "valid_gate_class",]
    valid_gate_class_choices <- strsplit(valid_gate_class_params$param_choices_list, ",")[[1]]
    is_binary_gc_params <-  study_config[study_config$param_name == "is_binary_gc",]
    zero_pass_too_diluted_Tx_params <- study_config[study_config$param_name == "zero_pass_diluted_Tx",]
    zero_pass_too_diluted_Tx_choices <- strsplit(zero_pass_too_diluted_Tx_params$param_choices_list, ",")[[1]]
    zero_pass_concentrated_Tx_params <- study_config[study_config$param_name == "zero_pass_concentrated_Tx",]
    zero_pass_concentrated_Tx_choices <- strsplit(zero_pass_concentrated_Tx_params$param_choices_list, ",")[[1]]
    zero_pass_concentrated_diluted_Tx_params <- study_config[study_config$param_name == "zero_pass_concentrated_diluted_Tx",]
    zero_pass_concentrated_diluted_Tx_choices <- strsplit(zero_pass_concentrated_diluted_Tx_params$param_choices_list, ",")[[1]]
    one_pass_acceptable_Tx_params <- study_config[study_config$param_name == "one_pass_acceptable_Tx",]
    one_pass_acceptable_Tx_params_choices <- strsplit(one_pass_acceptable_Tx_params$param_choices_list, ",")[[1]]
    two_plus_pass_acceptable_Tx_params <- study_config[study_config$param_name == "two_plus_pass_acceptable_Tx",]
    two_plus_pass_acceptable_Tx_choices <- strsplit(two_plus_pass_acceptable_Tx_params$param_choices_list, ",")[[1]]

    au_treatment_choices_names = c("Keep all AU measurements",
                "Keep passing AU measurements",
                "Geometric mean of all AU measurements",
                "Geometric mean of passing AU measurements",
                "Replace AU measurements with geometric mean of blank AUs",
                "Exclude AU measurements")
    zero_pass_too_diluted_Tx_choices <- setNames(zero_pass_too_diluted_Tx_choices, au_treatment_choices_names)
    zero_pass_concentrated_Tx_choices <- setNames(zero_pass_concentrated_Tx_choices, au_treatment_choices_names)
    zero_pass_concentrated_diluted_Tx_choices <- setNames(zero_pass_concentrated_diluted_Tx_choices, au_treatment_choices_names)
    one_pass_acceptable_Tx_params_choices <- setNames(one_pass_acceptable_Tx_params_choices, au_treatment_choices_names)
    two_plus_pass_acceptable_Tx_choices <- setNames(two_plus_pass_acceptable_Tx_choices, au_treatment_choices_names)


   default_db_node_order <- strsplit(node_order_params$param_character_value, ",")[[1]]
   ordered_db_node_order_choices <- c(
     default_db_node_order,
     setdiff(node_order_params_choices, default_db_node_order)
   )

   default_valid_gate_class_order <- strsplit(valid_gate_class_params$param_character_value,",")[[1]]
   ordered_db_gate_class_choices <- c(default_valid_gate_class_order,
                                      setdiff(valid_gate_class_choices,default_valid_gate_class_order))
   # if (is.null(default_db_node_order) || identical(default_db_node_order, "null")) {
   #   default_db_node_order <- "linear"
   # }
    # if (!identical(default_db_node_order,node_order_params_choices )) {
    #   default_db_node_order <- node_order_params_choices #strsplit(node_order_params$param_character_value, ",")[[1]] # fallback
    # }

   mainPanel(
    # bsCollapse(
    #   id = "dilution_analysis_config",
    #   bsCollapsePanel(
    #     title = "Dilution Analysis Parameters",
    #  HTML("<h4><strong>Dilution Analysis</strong></h4>"),
      HTML(paste0("<h4>For the decision tree, select the order in which the decisions (and thus the nodes) are created from the type of sample limit selector.
                  Set the limit of detection that is considered passing in the decision tree classification and how the final arbritary units are calculated
                  for the combinations of the number of dilutions that are passing and the sample's concentration status. </h4>")),

     grVizOutput("decision_tree_diagram"),

      selectInput(inputId = "node_order",
                  label = node_order_params$param_label,
                  choices = ordered_db_node_order_choices,#node_order_params_choices,
                  selected = strsplit(node_order_params$param_character_value, ",")[[1]],
                  multiple = TRUE),

      # Valid gate class parameters
      selectInput(inputId = valid_gate_class_params$param_name,
                  label = valid_gate_class_params$param_label,
                  choices = ordered_db_gate_class_choices,#alid_gate_class_choices,
                  selected = strsplit(valid_gate_class_params$param_character_value,",")[[1]],
                  multiple = TRUE),

      # Binary Passing or not for Limits in tree
      checkboxInput(inputId = is_binary_gc_params$param_name,
                    label = is_binary_gc_params$param_label,
                    value = as.logical(toupper(is_binary_gc_params$param_boolean_value))),

    uiOutput("is_bnary_gc_warning"),

      HTML(paste0("<h4> Set decisions on how to treat the final arbritary unit calculations for
                  the associated number of passing dilutions and concentration status.</h4>")),
      fluidRow(
        column(6,
      radioButtons(inputId = zero_pass_too_diluted_Tx_params$param_name,
                   label = zero_pass_too_diluted_Tx_params$param_label,
                   choices = zero_pass_too_diluted_Tx_choices,
                   selected = zero_pass_too_diluted_Tx_params$param_character_value),

      uiOutput("zero_pass_diluted_Tx_warning"),

      radioButtons(inputId = zero_pass_concentrated_Tx_params$param_name,
                  label = zero_pass_concentrated_Tx_params$param_label,
                  choices = zero_pass_concentrated_Tx_choices,
                  selected = zero_pass_concentrated_Tx_params$param_character_value),
      uiOutput("zero_pass_concentrated_Tx_warning"),

      radioButtons(inputId = zero_pass_concentrated_diluted_Tx_params$param_name,
                   label = zero_pass_concentrated_diluted_Tx_params$param_label,
                   choices = zero_pass_concentrated_diluted_Tx_choices,
                   selected = zero_pass_concentrated_diluted_Tx_params$param_character_value),
      uiOutput("zero_pass_concentrated_diluted_Tx_warning")
        ),
      column(6,
      radioButtons(inputId = one_pass_acceptable_Tx_params$param_name,
                   label = one_pass_acceptable_Tx_params$param_label,
                   choices = one_pass_acceptable_Tx_params_choices,
                   selected = one_pass_acceptable_Tx_params$param_character_value),
      uiOutput("one_pass_acceptable_Tx_warning"),

      radioButtons(inputId = two_plus_pass_acceptable_Tx_params$param_name,
                   label = two_plus_pass_acceptable_Tx_params$param_label,
                   choices = two_plus_pass_acceptable_Tx_choices,
                   selected = two_plus_pass_acceptable_Tx_params$param_character_value
                   ),
     uiOutput("two_plus_pass_acceptable_Tx_warning")
      )

    ),
    actionButton(inputId = "save_dilution_analysis_config",
                 label = "Save")

  #   style = "primary"
  # )
    )
})


  output$standard_curve_config <- renderUI({
    req(study_config)
    req(input$basic_qc_params == 'Standard Curve Parameters')
    # req(study_config_rv())
    # study_config <- study_config_rv()
    req(study_sources)
    # get study config parameters
    study_config <- study_config[study_config$param_group == "standard_curve_options",]
    mean_mfi_params <- study_config[study_config$param_name == "mean_mfi",]
    log_mfi_axis_params <- study_config[study_config$param_name == "is_log_mfi_axis",]
    prozone_correction_params <- study_config[study_config$param_name == "applyProzone",]
    blank_options_params <- study_config[study_config$param_name == "blank_option",]
    blank_options_choices <- strsplit(blank_options_params$param_choices_list, ",")[[1]]
    source_options_choices <- study_sources$source
    default_source_params <- study_config[study_config$param_name == "default_source",]

    blank_control_choices_names = c("Ignored" = "ignored", "Included" = "included", "Subtracted 1 x Geometric mean" = "subtracted",
                "Subtract 3 x Geometric Mean" = "subtracted_3x", "Subtracted 10 x Geometric Mean" = "subtracted_10x")
    blank_options_choices <- setNames(blank_options_choices, names(blank_control_choices_names))
    # default_source_choices <- strsplit(default_source_params$param_choices_list, ",")[[1]]

     # check if source has been saved (initially it is not)
    default_db_source <- default_source_params$param_character_value
    cat("Default db source\n")
    print(default_db_source)

    # if (!default_db_source %in% source_options_choices) {
    #   default_db_source <- source_options_choices[1]  # fallback
    # }



    # default_blank_option <- blank_options_params$param_character_value
    # if (!default_blank_option %in% blank_options_choices) {
    #   default_blank_option <- blank_options_params$param_character_value
    # }

    mainPanel(
  # bsCollapse(
  #   id = "standard_curve_config",
  #   bsCollapsePanel(
  #     title = "Standard Curve Parameters",

      # bsCollapse(
      #   id = "standard_curve_parameters_info",
      #   bsCollapsePanel(
      #     title = "Standard Curve Parameters Methods",
      #     tagList(
      #      tags$p("The Prozone correction which is recomended, accounts for the prozone effect in stndard curve data in which
      #             '...the concentration of the analyte becomes so high that it exceeds the capacity of the antibodies in the assay' (Bradley and Bhalla)"),
      #      tags$p("Including the geometric mean of the MFI of the blanks and subtracting the geometric mean of the blanks from each standard are adapted from Sanz et al.")
      #
      #     ), # end taglist
      #     style = "success"
      #   )),


      tags$table(
        border = 1,

        # Row 1: Mean MFI
        tags$tr(
          tags$td(
            switchInput(mean_mfi_params$param_name,
                        label = mean_mfi_params$param_label,
                        value = as.logical(toupper(mean_mfi_params$param_boolean_value))),
            uiOutput("mean_mfi_warning")
          ),
          tags$td(
            "Take the mean of multiple MFI measurements in the standards at same concentration. This is especially helpful if there are repeated MFI measures."
          )
        ),

        # Row 2: Log MFI
        tags$tr(
          tags$td(
            switchInput(log_mfi_axis_params$param_name,
                        label = log_mfi_axis_params$param_label,
                        value = as.logical(toupper(log_mfi_axis_params$param_boolean_value))),
            uiOutput("is_log_mfi_warning")
          ),
          tags$td(
            "Choose whether or not to log transform MFI when fitting standard curves."
          )
        ),

        # Row 3: Prozone correction
        tags$tr(
          tags$td(
            switchInput(prozone_correction_params$param_name,
                        label = prozone_correction_params$param_label,
                        value = as.logical(toupper(prozone_correction_params$param_boolean_value))),
            uiOutput("apply_prozone_warning")
          ),
          tags$td(
            "The Prozone Correction, which is recomended, accounts for the prozone effect in stndard curve data in which
                  '...the concentration of the analyte becomes so high that it exceeds the capacity of the antibodies in the assay' (Bradley and Bhalla)"
          )
        ),

        # Row 4: Blank control options
        tags$tr(
          tags$td(
            radioButtons(blank_options_params$param_name,
                         label = blank_options_params$param_label,
                         choices = blank_options_choices,
                         selected = blank_options_params$param_character_value),
            uiOutput("blank_option_warning")
          ),
          tags$td(
            "Select how blank controls are handled in the standard curve estimation. Options include Ignore, Include geometric mean of blank, or Subtract multiples of the geometric mean.",
            div(style = "display:inline-block; margin-bottom: 10px;",
                title = "Info",
                icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                     `data-placement` = "right",
                     title = paste("To select a method for which the blanks are to be treated for the selected study click one of the methods.
                                 The avaliable methods are 'Ignore', 'Include', 'Subtract Geometric Mean', 'Subtract three times the Geometric Mean' and subtract 10 times the geometric mean.
                                 When Ignore is selected, the blanks are not considered in the standard curve.
                                 When Included is selected, the stimation of the standard curve takes into account the mean of the background
of the values as another point of the standard curve. The median fluorescence intensity and the expected concentration for this new point by analyte is estimated as follows:
                                 MFI: geometric mean of the blank controls.
                                 log dilution: The mininum log dilution - log10(2). This corresponds to the the minimum expected concentration value of the standard points divided by 2 as in the drLumi package.
                                 When subtracted is selected, the geometric mean of the blank controls is subracted from all the standard points. Depending on what level of subtraction is selected,
                                 the geometric mean is multiplied by that factor (1,3, or 10) before the subtraction is applied. After subtraction, if any MFI is below 0 it is set to 0."),
                     `data-html` = "true")
            )
          )
        ),

        # Row 5: Source
        tags$tr(
          tags$td(
            radioButtons(default_source_params$param_name,
                         label = default_source_params$param_label,
                         choices  = source_options_choices,
                         selected = default_db_source),
            uiOutput("default_source_warning")
          ),
          tags$td(
            "Select the default source to use for standard curve calculation."
          )
        )
      ),

  #  HTML("<h4><strong>Standard Curve</strong></h4>"),
    # Mean MFI at each dilution factor
#     switchInput(mean_mfi_params$param_name,
#                 label = mean_mfi_params$param_label,
#                 value = as.logical(toupper(mean_mfi_params$param_boolean_value))),
#
#     uiOutput("mean_mfi_warning"),
#    # Use MFI as log or not
#    switchInput(log_mfi_axis_params$param_name,
#               label = log_mfi_axis_params$param_label,
#               value = as.logical(toupper(log_mfi_axis_params$param_boolean_value))),
#
#    uiOutput("is_log_mfi_warning"),
#
#   # Prozone correction or not
#   switchInput(prozone_correction_params$param_name,
#               label = prozone_correction_params$param_label,
#               value = as.logical(toupper(prozone_correction_params$param_boolean_value))),
#
#   uiOutput("apply_prozone_warning"),
#
#
#
#     # Blank Control Options
#     div(style = "display:inline-block; margin-bottom: 10px;",
#         title = "Info",
#         icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
#              `data-placement` = "right",
#              title = paste("To select a method for which the buffers are to be treated for the selected study click one of the methods.
#                                  The avaliable methods are 'Ignore', 'Include', 'Subtract Geometric Mean', 'Subtract three times the Geometric Mean' and subtract 10 times the geometric mean.
#                                  When Ignore is selected, the buffers are not considered in the standard curve.
#                                  When Included is selected, the stimation of the standard curve takes into account the mean of the background
# of the values as another point of the standard curve. The median fluorescence intensity and the expected concentration for this new point by analyte is estimated as follows:
#                                  MFI: geometric mean of the blank controls.
#                                  log dilution: The mininum log dilution - log10(2). This corresponds to the the minimum expected concentration value of the standard points divided by 2 as in the drLumi package.
#                                  When subtracted is selected, the geometric mean of the blank controls is subracted from all the standard points. Depending on what level of subtraction is selected,
#                                  the geometric mean is multiplied by that factor (1,3, or 10) before the subtraction is applied. After subtraction, if any MFI is below 0 it is set to 0."),
#              `data-html` = "true")
#     ),
#     radioButtons(blank_options_params$param_name,
#                  label = blank_options_params$param_label,
#                  choices = blank_options_choices,
#                  selected = blank_options_params$param_character_value),
#
#     uiOutput("blank_option_warning"),
#     # Source - get from loaded data - default to first source
#      radioButtons(default_source_params$param_name,
#                   label = default_source_params$param_label,
#                   choices  = source_options_choices,
#                   selected = default_db_source), # source_options_choices[1]),
#   uiOutput("default_source_warning"),

  actionButton(inputId = "save_standard_curve_config",
               label = "Save")

#   style = "primary")
# )
)

  })

  output$subgroup_config <- renderUI({
    req(study_config)
    req(input$advanced_qc_params == 'Subgroup Parameters')
    # req(study_config_rv())
    # study_config <- study_config_rv()
    req(study_arms)
    req(study_timeperiods)

    # get study config parameters
    study_config <- study_config[study_config$param_group == "subgroup_settings",]
    reference_arm_params <- study_config[study_config$param_name == "reference_arm",]
    reference_arm_choices <- study_arms$agroup
    timeperiod_order_params <- study_config[study_config$param_name == "timeperiod_order",]
    timeperiod_choices <- study_timeperiods$timeperiod
    primary_timeperiod_comparison_params <- study_config[study_config$param_name == "primary_timeperiod_comparison",]

    # Decide to load database values if they are saved after first time it is run and provide fall back
    default_db_reference_arm <- reference_arm_params$param_character_value
    if (is.null(default_db_reference_arm)) {
      default_db_reference_arm <- reference_arm_choices[1]
    } else {
      if (!default_db_reference_arm %in% reference_arm_choices) {
        default_db_reference_arm <- reference_arm_choices[1]  # fallback
      }
    }
   # default_db_timeperiod_order <- timeperiod_order_params$param_character_value
    if(is.null(timeperiod_order_params$character_value)) {
        default_db_timeperiod_order <- timeperiod_choices# fallback
    } else {
      default_db_timeperiod_order <- strsplit(timeperiod_order_params$param_character_value, ",")[[1]]
      if (!all(default_db_timeperiod_order %in% timeperiod_choices)) {
        #if (!default_db_timeperiod_order %in% timeperiod_choices) {
          default_db_timeperiod_order <- timeperiod_choices# fallback
      }
    }

  if (is.null(primary_timeperiod_comparison_params$param_character_value)) {
      default_db_primary_timeperiod_comparison <- timeperiod_choices[1:2]# fallback
  } else {
    default_db_primary_timeperiod_comparison <- strsplit(primary_timeperiod_comparison_params$param_character_value, ",")[[1]]
      #default_db_primary_timeperiod_comparison <- primary_timeperiod_comparison_params$param_character_value
      #if (!default_db_primary_timeperiod_comparison %in% timeperiod_choices) {
    if (!all(default_db_primary_timeperiod_comparison %in% timeperiod_choices)) {
        default_db_primary_timeperiod_comparison <- timeperiod_choices[1:2]# fallback
    }
  }

    mainPanel(
    # bsCollapse(
    #   id = "subgroup_config",
    #   bsCollapsePanel(
    #     title = "Subgroup Parameters",
    #HTML("<h4><strong>Subgroup Parameters</strong></h4>"),
    # Arm control
    radioButtons(inputId = reference_arm_params$param_name,
                 label = reference_arm_params$param_label,
                 choices = reference_arm_choices,
                 selected = default_db_reference_arm),#reference_arm_choices[1]),

    uiOutput("reference_arm_warning"),
    # Order the timeperiods
    orderInput(inputId = timeperiod_order_params$param_name,
               label = timeperiod_order_params$param_label,
               items = default_db_timeperiod_order), #timeperiod_choices),

    uiOutput("timeperiod_order_warning"),

    selectInput(inputId = primary_timeperiod_comparison_params$param_name,
                label = primary_timeperiod_comparison_params$param_label,
                choices = timeperiod_choices,
                selected = default_db_primary_timeperiod_comparison,#timeperiod_choices[1:2],
                multiple = T),

    actionButton(inputId = "save_subgroup_config",
                 label = "Save"),

    # style = "primary")
    # )
    )

  })

}) # end render



observe({
  req(input$main_tabs == "study_settings")
 # req(study_level_tabs == "Study Parameters")
  req(input$readxMap_study_accession)
  req(currentuser())
  # capture reactive inputs *outside* later callback
  study_accession <- isolate(input$readxMap_study_accession)
  user <- isolate(currentuser())

  # start async polling
  check_and_render_study_parameters(study_accession, user)
})

study_params_ready <- reactiveVal(FALSE)

# check if ready on database side
observeEvent(study_params_ready(), {
 # req(input$main_tabs == "view_files_tab")
  #req(input$study_level_tabs == "Study Parameters")
  if (study_params_ready()) {
    render_study_parameters()  # safe here, reactive context
    study_params_ready(FALSE)  # reset flag if needed
  }
})

# observe({
#   req(input$readxMap_study_accession)
#   req(currentuser())
#   if (input$study_level_tabs == "Study Parameters") {
#     cat("Load Study Parameters tab\n")
#
#     study_user_params_nrow <- nrow(fetch_study_configuration(
#       study_accession = input$readxMap_study_accession,
#       user = currentuser()
#     ))
#
#
#     study_accession <-  input$readxMap_study_accession
#     user <-   currentuser()
#
#     check_and_render_study_parameters(study_accession, user)
#
#
#     if (study_user_params_nrow > 0) {
#       render_study_parameters()
#     }
#
#     # config <- fetch_study_configuration(study_accession = input$readxMap_study_accession, user = currentuser())
#     # study_config_rv(config)
#    # render_study_parameters()
#   }
# })

# Detect Changed Values antigen

output$antigen_family_order_warning <- renderUI({
  input$save_antigen_family_settings # once save happens -reset validity of changes
  input$reset_user_config
  antigen_family_choices <- unique(antigen_families_rv()$antigen_family)


  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  antigen_study_config <- study_config[study_config$param_group == "antigen_family",]
  antigen_family_order_params <-  antigen_study_config[antigen_study_config$param_name == "antigen_family_order",]
  antigen_family_order_database <- antigen_family_order_params$param_character_value

  if (!is.null(antigen_family_order_database) && length(antigen_family_order_database) > 0) {
    antigen_family_order_database_comparison <- strsplit(antigen_family_order_database, ",")[[1]]
  } else {
    antigen_family_order_database_comparison <- antigen_family_choices

  }
  #antigen_family_order_database_comparison <- strsplit(antigen_family_order_database, ",")[[1]]

  if (!identical(input$antigen_family_order, antigen_family_order_database_comparison)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$antigen_order_warning <- renderUI({
  input$save_antigen_family_settings # once save happens -reset validity of changes
  input$reset_user_config
  antigen_choices <- unique(antigen_families_rv()$antigen)
  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  antigen_study_config <- study_config[study_config$param_group == "antigen_family",]
  antigen_order_params <-  antigen_study_config[antigen_study_config$param_name == "antigen_order",]
  antigen_order_database <- antigen_order_params$param_character_value

  if (!is.null(antigen_order_database) && length(antigen_order_database) > 0) {
    antigen_order_database_comparison <- strsplit(antigen_order_database, ",")[[1]]
  } else {
    antigen_order_database_comparison <- antigen_choices

  }
 # antigen_order_database_comparison <- strsplit(antigen_order_database, ",")[[1]]
  if (!identical(input$antigen_order,antigen_order_database_comparison)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})
# observeEvent({
#   req(input$readxMap_study_accession)
#   # Get selected study
#   selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)
#
#   study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())
#
#   study_config <- study_config[study_config$param_group == "antigen_family",]
#   antigen_family_order_params <- study_config[study_config$param_name == "antigen_family_order",]
#   antigen_order_params <- study_config[study_config$param_name == "antigen_order",]
#
#   print(input$antigen_order)
# if (!identical(input$antigen_order, antigen_order_params$param_character_value)) {
#   cat("unsaved changes")
#   showFeedbackWarning(session = session, "antigen_order", text = "Unsaved changes")
# } else {
#   hideFeedback(session = session, "antigen_order")
# }
# })

# Bead Count  Detect Changed Values
## Observe for non radio buttons and orderInputs
observe({
  req(input$main_tabs == "view_files_tab")
#  req(study_level_tabs == "Study Parameters")
  req(input$readxMap_study_accession)
   input$save_bead_count_params # once save happens -reset validity of changes
   input$reset_user_config
    # Get selected study
    selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

    study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

    bead_count_study_config <- study_config[study_config$param_group == "bead_count",]
    lower_threshold_param <- bead_count_study_config[bead_count_study_config$param_name == "lower_bc_threshold",]
    bead_count_database_lower_threshold_val <- lower_threshold_param$param_integer_value

    if (!identical(input$lower_bc_threshold, bead_count_database_lower_threshold_val)) {
      showFeedbackWarning("lower_bc_threshold", text = "Unsaved Changes")
    } else {
      hideFeedback("lower_bc_threshold")
    }

    bead_count_upper_threshold_param <- bead_count_study_config[bead_count_study_config$param_name == "upper_bc_threshold",]
    bead_count_database_upper_threshold_val <- bead_count_upper_threshold_param$param_integer_value
    if (!identical(input$upper_bc_threshold, bead_count_database_upper_threshold_val)) {
      showFeedbackWarning("upper_bc_threshold", text = "Unsaved Changes")
    } else {
      hideFeedback("upper_bc_threshold")
    }

    bead_count_pct_agg_threshold_param <- bead_count_study_config[bead_count_study_config$param_name == "pct_agg_threshold",]
    bead_count_database_pct_agg_threshold_val <- bead_count_pct_agg_threshold_param$param_integer_value
    if (!identical(input$pct_agg_threshold, bead_count_database_pct_agg_threshold_val)) {
      showFeedbackWarning("pct_agg_threshold", text = "Unsaved Changes")
    } else {
      hideFeedback("pct_agg_threshold")
    }

    # bead_count_failed_well_crieria <-  bead_count_study_config[bead_count_study_config$param_name == "failed_well_criteria",]
    # bead_count_database_failed_well_criteria <- bead_count_failed_well_crieria$param_character_value
    # if (!identical(input$failed_well_criteria, bead_count_database_failed_well_criteria)) {
    #   showFeedbackWarning("failed_well_criteria", text = "Unsaved Changes")
    # } else {
    #   hideFeedback("failed_well_criteria")
    # }

})

## Bead Count radioButtons observe Changed Values
output$failed_well_warning <- renderUI({
  input$save_bead_count_params # once save happens -reset validity of changes
  input$reset_user_config
  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  bead_count_study_config <- study_config[study_config$param_group == "bead_count",]
  bead_count_failed_well_crieria <-  bead_count_study_config[bead_count_study_config$param_name == "failed_well_criteria",]
  bead_count_database_failed_well_criteria <- bead_count_failed_well_crieria$param_character_value
  if (input$failed_well_criteria != bead_count_database_failed_well_criteria) {
    span(style = "color: #F89406;",
         "Unsaved Chamges")
  }
})

## Dilution Analysis Diagrams
decision_tree_reactive_diagram <- reactive({
  input$save_dilution_analysis_config # refresh when the dilution analysis params are saved.
  input$reset_user_config

  req(input$readxMap_study_accession)
  req(currentuser())

  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())


  is_binary <- study_config[study_config$param_name == "is_binary_gc",]$param_boolean_value
  node_order <- strsplit(study_config[study_config$param_name == "node_order",]$param_character_value, ",")[[1]]
  sufficient_gc <- strsplit(study_config[study_config$param_name == "valid_gate_class",]$param_character_value, ",")[[1]]

  replacements <- c(limits_of_detection = "gate", linear_region = "linear", limits_of_quantification = "quantifiable")
  node_order <- ifelse(node_order %in% names(replacements), replacements[node_order], node_order)

  # replacements_gate <- c(Between_Limits_of_Detection = "Between_Limits", Above_Upper_Limit_of_Detection = "Above_Upper_Limit", Below_Lower_Limit_of_Detection = "Below_Lower_Limit")
  # sufficient_gc <- ifelse(sufficient_gc %in% names(replacements_gate), replacements_gate[sufficient_gc], sufficient_gc)

  # Create truth table inline
  truth_table <- create_truth_table(
    binary_gate = is_binary,
    exclude_linear = FALSE,
    exclude_quantifiable = FALSE,
    exclude_gate = FALSE
  )

  # Create decision tree
  decision_tree <- create_decision_tree_tt(
    truth_table = truth_table,
    binary_gate = is_binary,
    sufficient_gc_vector = sufficient_gc,
    node_order = node_order
  )

  decision_tree
})


# decision_tree_reactive_diagram <- reactive({
#   req(decision_tree_cache())
#   decision_tree_cache()
# })

## Decision Tree Plot
output$decision_tree_diagram <- renderGrViz({
  req(decision_tree_reactive_diagram())

  decision_tree <- decision_tree_reactive_diagram()

  dot_string <- paste(
    "digraph tree {",
    paste(get_edges(decision_tree), collapse = "; "),
    "}",
    sep = "\n"
  )
  grViz(dot_string)
  #decision_tree$ToDiagrammeRGraph()
})

## Dilution Analysis - Detect Change Parameters
observe({
  req(input$readxMap_study_accession != "Click here")
 # req(input$main_tabs == "view_files_tab")
  #req(input$study_level_tabs)
  #req(input$study_level_tabs == "Study Parameters")
  req(input$readxMap_study_accession)
  req(input$node_order)
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())
  study_config_v <- study_config

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  node_order_param <- dilution_analysis_study_config[dilution_analysis_study_config$param_name == "node_order",]
  node_order_param_val <- node_order_param$param_character_value

  node_order_vector <- paste(input$node_order, collapse = ",")

  #node_order_param_val_vector <- strsplit(node_order_param_val, ",")[[1]]
 #check <<- identical(strsplit(node_order_param_val, ",")[[1]], node_order)

  if (!identical(node_order_vector, node_order_param_val)) {
    showFeedbackWarning("node_order", text = "Unsaved Changes")
  } else {
    hideFeedback("node_order")
  }
  # Passing Limit of Detection
  valid_gate_class_param <- dilution_analysis_study_config[dilution_analysis_study_config$param_name == "valid_gate_class",]
  valid_gate_class_param_val <- valid_gate_class_param$param_character_value
  valid_gate_class_param_val_vector <- strsplit(valid_gate_class_param_val, ",")[[1]]

  if (!identical(input$valid_gate_class, valid_gate_class_param_val_vector)) {
    showFeedbackWarning("valid_gate_class", text = "Unsaved Changes")
  } else {
    hideFeedback("valid_gate_class")
  }
})

## Dilution Analysis monitor changes for non select Inputs
output$is_bnary_gc_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  dilution_analysis_passing_limit_of_d <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "is_binary_gc",]
  dilution_analysis_passing_limit_of_d_val <- as.logical(toupper(dilution_analysis_passing_limit_of_d$param_boolean_value))
  if (input$is_binary_gc != dilution_analysis_passing_limit_of_d_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})
# passing dilution - concentration status
output$zero_pass_diluted_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config
  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  zero_pass_diluted_TX <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "zero_pass_diluted_Tx",]
  dilution_analysis_zero_pass_diluted_TX_val<- zero_pass_diluted_TX$param_character_value
  if (input$zero_pass_diluted_Tx != dilution_analysis_zero_pass_diluted_TX_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$zero_pass_concentrated_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  zero_pass_concentrated_Tx <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "zero_pass_concentrated_Tx",]
  dilution_analysis_zero_pass_concentrated_Tx_val<- zero_pass_concentrated_Tx$param_character_value
  if (input$zero_pass_concentrated_Tx != dilution_analysis_zero_pass_concentrated_Tx_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$zero_pass_concentrated_diluted_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  zero_pass_concentrated_diluted_Tx <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "zero_pass_concentrated_diluted_Tx",]
  dilution_analysis_zero_pass_concentrated_diluted_Tx_val <- zero_pass_concentrated_diluted_Tx$param_character_value
  if (input$zero_pass_concentrated_diluted_Tx != dilution_analysis_zero_pass_concentrated_diluted_Tx_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$one_pass_acceptable_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  one_pass_acceptable_Tx <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "one_pass_acceptable_Tx",]
  dilution_analysis_one_pass_acceptable_Tx_val <- one_pass_acceptable_Tx$param_character_value
  if (input$one_pass_acceptable_Tx != dilution_analysis_one_pass_acceptable_Tx_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$two_plus_pass_acceptable_Tx_warning <- renderUI({
  input$save_dilution_analysis_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  dilution_analysis_study_config <- study_config[study_config$param_group == "dilution_analysis",]
  two_plus_pass_acceptable_Tx <-  dilution_analysis_study_config[dilution_analysis_study_config$param_name == "two_plus_pass_acceptable_Tx",]
  dilution_analysis_two_plus_pass_acceptable_Tx_val <- two_plus_pass_acceptable_Tx$param_character_value
  if (input$two_plus_pass_acceptable_Tx != dilution_analysis_two_plus_pass_acceptable_Tx_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

## Monitor changes for Standard Curve Parameters
# mean mfi (aggrigate or not)
output$mean_mfi_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  mean_mfi_param <-  standard_curve_study_config[standard_curve_study_config$param_name == "mean_mfi",]
  mean_mfi_param_val <- as.logical(toupper(mean_mfi_param$param_boolean_val))
  if (input$mean_mfi != mean_mfi_param_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$is_log_mfi_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  log_mfi_axis_param <-  standard_curve_study_config[standard_curve_study_config$param_name == "is_log_mfi_axis",]
  log_mfi_axis_param_val <- as.logical(toupper(log_mfi_axis_param$param_boolean_val))
  if (input$is_log_mfi_axis != log_mfi_axis_param_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$apply_prozone_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  prozone_correction_param <-  standard_curve_study_config[standard_curve_study_config$param_name == "applyProzone",]
  prozone_correction_param_val <- as.logical(toupper(prozone_correction_param$param_boolean_val))
  if (input$applyProzone != prozone_correction_param_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})


#blank control options
output$blank_option_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  blank_controls_param <-  standard_curve_study_config[standard_curve_study_config$param_name == "blank_option",]
  blank_option_val <- blank_controls_param$param_character_val
  if (input$blank_option != blank_option_val) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$default_source_warning <- renderUI({
  input$save_standard_curve_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  standard_curve_study_config <- study_config[study_config$param_group == "standard_curve_options",]
  source_params <-  standard_curve_study_config[standard_curve_study_config$param_name == "default_source",]
  source_params_val <- source_params$param_character_val
  if (!identical(input$default_source, source_params_val)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

### Detect changes for subgroup parameters
observe({
 # req(input$main_tabs == "view_files_tab")
  req(input$readxMap_study_accession)
  req(input$primary_timeperiod_comparison)
  input$save_subgroup_config # once save happens -reset validity of changes
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  subgroup_study_config <- study_config[study_config$param_group == "subgroup_settings",]
  primary_timeperiod_comparison_param <- subgroup_study_config[subgroup_study_config$param_name == "primary_timeperiod_comparison",]
  primary_timeperiod_comparison_val <- primary_timeperiod_comparison_param$param_character_value
  #comparison <<-  input$primary_timeperiod_comparison
 # primary_timeperiod_order_database_comparison <<- strsplit(primary_timeperiod_comparison_val, ",")[[1]]
  #primary_timeperiod_order_database_comparison <-   paste(input$primary_timeperiod_comparison_val, collapse = ",")

  primary_timeperiod_comparison_vector <- paste(input$primary_timeperiod_comparison, collapse = ",")

  if (!identical(primary_timeperiod_comparison_vector,primary_timeperiod_comparison_val)) {
    showFeedbackWarning("primary_timeperiod_comparison", text = "Unsaved Changes")
  } else {
    hideFeedback("primary_timeperiod_comparison")
  }
})

output$reference_arm_warning <- renderUI({
  input$save_subgroup_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  subgroup_study_config <- study_config[study_config$param_group == "subgroup_settings",]
  reference_arm_params <-  subgroup_study_config[subgroup_study_config$param_name == "reference_arm",]
  reference_arm_params_val <- reference_arm_params$param_character_value
  if (!identical(input$reference_arm, reference_arm_params_val)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

output$timeperiod_order_warning <- renderUI({
  input$save_subgroup_config # monitor save button
  input$reset_user_config

  # Get selected study
  selected_study <- ifelse(input$readxMap_study_accession == "Click here", "reset", input$readxMap_study_accession)

  study_config <- fetch_study_configuration(study_accession = selected_study, user = currentuser())

  subgroup_study_config <- study_config[study_config$param_group == "subgroup_settings",]
  timeperiod_order_params <-  subgroup_study_config[subgroup_study_config$param_name == "timeperiod_order",]
  timeperiod_order_params_val <- timeperiod_order_params$param_character_value
  if (!is.null(timeperiod_order_params_val) && length(timeperiod_order_params_val) > 0) {
    timeperiod_order_database_comparison <- strsplit(timeperiod_order_params_val, ",")[[1]]
  } else {
    timeperiod_order_database_comparison <- input$timeperiod_order
  }

  if (!identical(input$timeperiod_order, timeperiod_order_database_comparison)) {
    span(style = "color: #F89406;",
         "Unsaved Changes")
  }
})

# Export last saved user settings from the database
output$user_parameter_download <- renderUI({
  req(input$main_tabs == "view_files_tab")
  req(input$readxMap_study_accession)
  req(currentuser())
  download_user_parameters(study_accession = input$readxMap_study_accession, user = currentuser())
})

output$user_parameter_download <- renderUI({
  req(input$readxMap_study_accession)
  req(currentuser())
  button_label <- paste0("Download ", input$readxMap_study_accession, " Configuration for ", currentuser())
  downloadButton("user_parameter_download_handle", button_label)
})


output$user_parameter_download_handle <-  downloadHandler(
  filename = function() {
    paste(input$readxMap_study_accession, "study_config", currentuser(), ".csv", sep = "_")
  },
  content = function(file) {
    req(input$main_tabs == "view_files_tab")
    req(input$readxMap_study_accession)
    req(currentuser())

    user_config_table <- download_user_parameters(study_accession = input$readxMap_study_accession, user = currentuser())


    # download data component (data frame)
    write.csv(user_config_table, file)
  }
)


### Update Database Buttons
observeEvent(input$save_bead_count_params, {
  req(currentuser())
  req(input$readxMap_study_accession)
  req(input$lower_bc_threshold)
  req(input$upper_bc_threshold)
  req(input$failed_well_criteria)
  cat("updating bead count params")
  cat(currentuser())
  cat(input$readxMap_study_accession)
  cat(input$lower_bc_threshold)
  cat(input$upper_bc_threshold)
  cat(input$failed_well_criteria)

# Update the lower bead count
  update_query <- paste0("UPDATE madi_results.xmap_study_config
  SET param_integer_value = ", as.numeric(input$lower_bc_threshold), "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'lower_bc_threshold'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_query)

  update_upper_threshold_query <- paste0("UPDATE madi_results.xmap_study_config
  SET param_integer_value = ", as.numeric(input$upper_bc_threshold), "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'upper_bc_threshold'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_upper_threshold_query)

  update_pct_agg_threshold_query <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_integer_value = ", as.numeric(input$pct_agg_threshold), "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'pct_agg_threshold'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_pct_agg_threshold_query)

  update_failed_well_query <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$failed_well_criteria, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'failed_well_criteria'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_failed_well_query)

  showNotification("Bead Count parameters updated successfully", type = "message")

})

observeEvent(input$save_antigen_family_settings, {
  req(currentuser())
  req(input$readxMap_study_accession)
  cat(input$antigen_family_order)
  cat(input$antigen_order)

  req(input$antigen_family_order)
  req(input$antigen_order)
  cat("Antigen Family settings saved")

  antigen_family_order_str <- paste0("'", paste(input$antigen_family_order, collapse = ","), "'")
  antigen_order_str <- paste0("'", paste(input$antigen_order, collapse = ","), "'")

  update_query <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", antigen_family_order_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'antigen_family_order'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_query)

  update_antigen_order <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", antigen_order_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'antigen_order'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_antigen_order)

  showNotification("Antigen Family Parameters updated successfully", type = "message")

})

observeEvent(input$save_dilution_analysis_config, {
  req(currentuser())
  req(input$readxMap_study_accession)
  req(input$node_order)
  req(input$valid_gate_class)
  req(input$zero_pass_diluted_Tx)
  req(input$zero_pass_concentrated_Tx)
  req(input$zero_pass_concentrated_diluted_Tx)
  req(input$one_pass_acceptable_Tx)
  req(input$two_plus_pass_acceptable_Tx)
  cat("Saving dilution analysis configurations")

  node_order_str <- paste0("'", paste(input$node_order, collapse = ","), "'")

  valid_gc_str <- paste0("'", paste(input$valid_gate_class, collapse = ","), "'")

  update_sample_limit <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", node_order_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'node_order'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_sample_limit)

  # passing limit of detection
  update_valid_gate_class <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", valid_gc_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'valid_gate_class'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_valid_gate_class)

  update_is_binary_gc <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_boolean_value = ", input$is_binary_gc, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'is_binary_gc'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_is_binary_gc)

  update_zero_pass_diluted_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$zero_pass_diluted_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'zero_pass_diluted_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_zero_pass_diluted_Tx)

  update_zero_pass_concentrated_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$zero_pass_concentrated_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'zero_pass_concentrated_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_zero_pass_concentrated_Tx)

  update_zero_pass_concentrated_diluted_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$zero_pass_concentrated_diluted_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'zero_pass_concentrated_diluted_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn,update_zero_pass_concentrated_diluted_Tx )

  update_one_pass_acceptable_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$one_pass_acceptable_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'one_pass_acceptable_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn,update_one_pass_acceptable_Tx)

  update_two_plus_pass_acceptable_Tx <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$two_plus_pass_acceptable_Tx, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'two_plus_pass_acceptable_Tx'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn,update_two_plus_pass_acceptable_Tx)

  showNotification("Dilution Analysis Parameters updated successfully", type = "message")


})

observeEvent(input$save_standard_curve_config, {
  req(currentuser())
  req(input$readxMap_study_accession)
  req(input$blank_option)
  req(input$default_source)
  cat("in save standard curve")

  update_mean_mfi <- paste0("UPDATE madi_results.xmap_study_config
  SET param_boolean_value = ", input$mean_mfi, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'mean_mfi'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_mean_mfi)

  update_blank_option <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$blank_option, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'blank_option'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_blank_option)

  update_is_log_mfi_axis <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_boolean_value = ", input$is_log_mfi_axis, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'is_log_mfi_axis'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_is_log_mfi_axis)

  update_prozone_correction <-  paste0("UPDATE madi_results.xmap_study_config
  SET param_boolean_value = ", input$applyProzone, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'applyProzone'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_prozone_correction)


  update_source <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$default_source, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'default_source'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_source)

  showNotification("Standard Curve Parameters updated successfully", type = "message")

})

observeEvent(input$save_subgroup_config, {
  req(currentuser())
  req(input$readxMap_study_accession)
  req(input$reference_arm)
  print(input$timeperiod_order)
  req(input$primary_timeperiod_comparison)
  cat("Saving Subgroup configurations")

  timeperiod_order_str <- paste0("'", paste(input$timeperiod_order, collapse = ","), "'")
  primary_timeperiod_comparison_str <- paste0("'", paste(input$primary_timeperiod_comparison, collapse = ","), "'")

  update_reference_arm <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = '", input$reference_arm, "'
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'reference_arm'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_reference_arm)

  update_timeperiod_order <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", timeperiod_order_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'timeperiod_order'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_timeperiod_order)

  update_timeperiod_comparsion <- paste0("UPDATE madi_results.xmap_study_config
  SET param_character_value = ", primary_timeperiod_comparison_str, "
    WHERE study_accession = '",input$readxMap_study_accession,"'
  AND param_name = 'primary_timeperiod_comparison'
  AND param_user = '", currentuser(), "';")

  dbExecute(conn, update_timeperiod_comparsion)

  showNotification("Subgroup Parameters updated successfully", type = "message")


})

# Reset the configuration for a user for the selected study
observeEvent(input$reset_user_config, {
  req(currentuser())
  req(input$readxMap_study_accession)
  # 1st delete user's configuration
  delete_user_config <- paste0("DELETE FROM madi_results.xmap_study_config
  WHERE study_accession = '", input$readxMap_study_accession, "'
  AND param_user = '", currentuser(),"';")

  dbExecute(conn, delete_user_config)

  initial_source <- unique(stored_plates_data$stored_standard$source)[1]

  # Add back fresh study configuration
  intitialize_study_configurations(study_accession = input$readxMap_study_accession, user = currentuser(), initial_source = initial_source)

  #study_config <- fetch_study_configuration(study_accession = input$readxMap_study_accession, user = currentuser())


  showNotification(paste0("Successfully reset study configuration for ", currentuser()), type = "message")

})


## Clear Study Configuration
# observeEvent(input$main_tabs, {
#   if (!is.null(input$main_tabs) && input$main_tabs != "view_files_tab") {
#     message("Not Viewing stored files")
#     cat("xmap experiment: ")
#     print(input$readxMap_experiment_accession)
#    updateSelectInput(session, "readxMap_experiment_accession", selected = "Click here")
#    cat("xmap experiment after update:\n ")
#    print(input$readxMap_experiment_accession)
#     # clear outputs
#     output$studyParameters_UI <- NULL
#     output$antigen_family_config <- NULL
#     output$antigen_family_table <- NULL
#     output$bead_count_config <- NULL
#     output$dilution_analysis_config <- NULL
#     output$standard_curve_config <- NULL
#     output$subgroup_config <- NULL
#     output$antigen_family_order_warning <- NULL
#     output$antigen_order_warning <- NULL
#     output$failed_well_warning <- NULL
#     output$decision_tree_diagram <- NULL
#     output$is_bnary_gc_warning <- NULL
#     output$zero_pass_diluted_Tx_warning <- NULL
#     output$zero_pass_concentrated_Tx_warning <- NULL
#     output$zero_pass_concentrated_diluted_Tx_warning <- NULL
#     output$one_pass_acceptable_Tx_warning <- NULL
#     output$two_plus_pass_acceptable_Tx_warning <- NULL
#     output$mean_mfi_warning <- NULL
#     output$is_log_mfi_warning <- NULL
#     output$apply_prozone_warning <- NULL
#     output$blank_option_warning <- NULL
#     output$default_source_warning <- NULL
#     output$reference_arm_warning <- NULL
#     output$timeperiod_order_warning <- NULL
#     #output$user_parameter_download <- NULL
#
#
#
#   }
# })
