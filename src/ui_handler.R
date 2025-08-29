# ui handling

reset_import_values <- function() {
  # Reset all reactive values
  isolate({
    # Single values
    upload_state_value$upload_state <- NULL
    rv_value_button$valueButton <- 0

    # Clear reactiveValues objects
    # header_rvdata$data <- NULL
    # standard_rvdata$data <- NULL
    # sample_rvdata$data <- NULL
    # control_rvdata$data <- NULL
    # buffer_rvdata$data <- NULL

    # Clear reactiveVal objects
    plate_data(NULL)
    unique_plate_types(NULL)
    availableSheets(NULL)
    inFile(NULL)

    # Reset xPonent specific values
    xponent_plate_data(NULL)
    xponent_meta_data(NULL)
    generated_tabs(NULL)
    lumcsv_reactive(NULL)

  })
}




get_user_projects <- function(conn, current_user) {
  query <- glue::glue("
    SELECT pu.project_id, p.project_name, pak.access_key
    FROM madi_lumi_users.project_users pu
    JOIN madi_lumi_users.projects p ON pu.project_id = p.project_id
    JOIN madi_lumi_users.project_access_keys pak ON pu.project_id = pak.project_id
    WHERE pu.user_id = {dbQuoteLiteral(conn, current_user)} AND pu.is_owner = TRUE
  ")
  dbGetQuery(conn, query)
}

get_user_projects_non_owner <- function(conn, current_user) {
  query <- glue::glue("
    SELECT pu.project_id, p.project_name
    FROM madi_lumi_users.project_users pu
    JOIN madi_lumi_users.projects p ON pu.project_id = p.project_id
    JOIN madi_lumi_users.project_access_keys pak ON pu.project_id = pak.project_id
    WHERE pu.user_id = {dbQuoteLiteral(conn, current_user)} AND pu.is_owner = FALSE
  ")
  dbGetQuery(conn, query)
}

getProjectName <- function(conn, current_user){
  query <- glue::glue(" SELECT project_name, workspace_id FROM madi_results.xmap_users pu WHERE pu.auth0_user = {dbQuoteLiteral(conn, current_user)} ")
  result <- dbGetQuery(conn, query)

  if(nrow(result) > 0){
    name <- result[1, "project_name"]
    id <- result[1, "workspace_id"]
  } else {
    name <- "unknown"
    id <- -1
  }

  return(list(name = name, id = id))
}




output$load_ui <- renderUI({
  current_user <- currentuser()
  tabRefreshCounter()$manage_project_tab
  fluidPage(
    tagList(
      #h3("You are in workspace/project:", userProjectName(), "ID: ", userWorkSpaceID() ),
      # br(),
      bsCollapse(
        id = "loadProjectCollapse",
        bsCollapsePanel(
          title = " Load Existing Project Documentation",
          style = "success",
          tagList(
            tags$p("To load a project you own, in the table labeled 'Projects you own: (your username)' select a project
                   and press the button labed 'Load Selected Project' under the table."),
            tags$p("To load a project you have access to, in the table labeled 'Projects you have access to: (your username)' select a project
                   and press the button labed 'Load Selected Project' under the table.")
          )
        )
      ),
      h3(glue::glue("Projects you own: {current_user}")),
      DT::dataTableOutput("userProjectsTable"),
      actionButton("execute_project_button", "Load Selected Project"),
      br(),
      h3(glue::glue("Projects you have access to: {current_user}")),
      DT::dataTableOutput("userProjectsTableNonOwner"),
      actionButton("execute_project_button1", "Load Selected Project")
    )
  )
})

observeEvent(input$execute_project_button, {
  selected_project_id <- input$userProjectsTable_rows_selected
  if (length(selected_project_id) == 1) {
    # Retrieve the project ID of the first selected row
    selected_project_id <- selected_project_id[1]
    # Perform action based on the selected project ID
    user_projects <- get_user_projects(conn, currentuser())
    project_id <- user_projects[selected_project_id, "project_id"]
    # Call your function to execute the project
    load_project(conn, project_id, currentuser())
    current_project_details <- getProjectName(conn, currentuser())
    userWorkSpaceID(current_project_details$id)
    userProjectName(current_project_details$name)
  } else {
    # Notify the user if no project or multiple projects are selected
    showModal(modalDialog(
      title = "Error",
      "Please select one project to execute.",
      easyClose = TRUE
    ))
  }
})



observeEvent(input$execute_project_button1, {
  selected_project_id <- input$userProjectsTableNonOwner_rows_selected
  if (length(selected_project_id) == 1) {
    # Retrieve the project ID of the first selected row
    selected_project_id <- selected_project_id[1]
    # Perform action based on the selected project ID
    user_projects_non_owner <- get_user_projects_non_owner(conn, currentuser())
    project_id <- user_projects_non_owner[selected_project_id, "project_id"]
    # Call your function to execute the project
    load_project(conn, project_id, currentuser())
    current_project_details <- getProjectName(conn, currentuser())
    userWorkSpaceID(current_project_details$id)
    userProjectName(current_project_details$name)
  } else {
    # Notify the user if no project or multiple projects are selected
    showModal(modalDialog(
      title = "Error",
      "Please select one project to execute.",
      easyClose = TRUE
    ))
  }
})




output$userProjectsTable <- DT::renderDataTable({
  tabRefreshCounter()$manage_project_tab
  user_projects <- get_user_projects(conn, currentuser())
  DT::datatable(user_projects, options = list(pageLength = 5))
})

output$userProjectsTableNonOwner <- DT::renderDataTable({
  tabRefreshCounter()$manage_project_tab
  user_projects_non_owner <- get_user_projects_non_owner(conn, currentuser())
  DT::datatable(user_projects_non_owner, options = list(pageLength = 5))
})

output$view_stored_experiments_ui <- renderUI({
  tabRefreshCounter()$view_files_tab
  req(reactive_df_study_exp())

  # Get data
  df <- reactive_df_study_exp()
  df <- df[df$study_accession != "Click here", ]


  # Build choices safely
  study_choices <- c("Click here" = "Click here",
                     setNames(unique(df$study_accession),
                              unique(df$study_name)))


  tagList(
    fluidPage(
      h3("Interactive Serology Plate Inspector - Stored Plate Data"),

      # Study Selection
      fluidRow(
        column(5,
               selectInput("readxMap_study_accession",
                           "Choose Study Name",
                           choices = study_choices,
                           # choices = c("Click here" = "Click here",
                           #             setNames(unique(reactive_df_study_exp()$study_accession),
                           #                      unique(reactive_df_study_exp()$study_name))),
                           selected = "Click here",
                           multiple = FALSE
               )
        )
      ),

      # Study Level Content
      conditionalPanel(
        condition = "input.readxMap_study_accession != 'Click here'",
        tabsetPanel(
          id = "study_level_tabs",
          tabPanel("Study Parameters",
                   id = "study_parameters_tab",
                   uiOutput("studyParameters_UI")
          ),
          # tabPanel("Plate Management",
          #          id = "plate_management_tab",
          #          uiOutput("plate_management_UI")),




          # Study Overview Tab
          tabPanel("Study Overview",
                   id = "study_overview_tab",
                   uiOutput("study_overview_page")
          ),
          # Experiment Level Tab
          tabPanel("Experiments",
                   fluidRow(
                    # column(6,
                            selectInput("readxMap_experiment_accession",
                                        "Choose Experiment Name",
                                        choices = c("Click here" = "Click here",
                                                    setNames(reactive_df_study_exp()$experiment_accession,
                                                             reactive_df_study_exp()$experiment_name)),
                                        selected = "Click here",
                                        multiple = FALSE
                            ),
                    # ),
                     #column(6,
                     conditionalPanel(
                       condition = "input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",

                      # tabsetPanel(
                      #   id = "basic_advance_tabs",
                      #   tabPanel(id = "basic_qc",
                      #     title = "Quality Control",
                      #     radioGroupButtons(
                      #       inputId = "qc_component",
                      #       label = "QC Phase",
                      #       choices = c("Data", "Bead Count", "Standard Curve","Standard Curve Summary", "Dilution Analysis", "Dilutional Linearity",
                      #                   # "Plate Normalization",
                      #                   "Outliers", "Subgroup Detection", "Subgroup Detection Summary"),
                      #       selected = "Data",
                      #       #  justified = TRUE,
                      #       #  multiple = FALSE
                      #     )
                      #   ),
                      #   tabPanel(id = "advance_qc",
                      #            title = "Advanced Diagnostics",
                      #            radioGroupButtons(
                      #              inputId = "advanced_qc_component",
                      #              label = "Advanced QC Phase",
                      #              choices = c("Dilution Analysis", "Dilutional Linearity",
                      #                          # "Plate Normalization",
                      #                          "Outliers", "Subgroup Detection", "Subgroup Detection Summary"),
                      #              selected = "Dilution Analysis",
                      #              #  justified = TRUE,
                      #              #  multiple = FALSE
                      #            ))
                      #
                      # ),

                      tabsetPanel(
                        id = "basic_advance_tabs",
                        tabPanel(
                         # id = "data_view",
                          title = "Data",
                          # conditionalPanel(
                          #   condition = "input.basic_advance_tabs == 'data_view'",
                            uiOutput("dynamic_data_ui")
                          #)
                        ),
                        tabPanel(
                          id = "basic_qc",
                          title = "Quality Control - Basic",
                          radioGroupButtons(
                            inputId = "qc_component",
                            label = "",
                            choices = c("Bead Count", "Standard Curve","Standard Curve Summary"),
                            selected = "Bead Count"
                          ),

                          # Conditional panels for basic QC only
                          # conditionalPanel(
                          #   condition = "input.qc_component == 'Data'",
                          #   uiOutput("dynamic_data_ui")
                          # ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Bead Count'",
                            uiOutput("bead_count_module_ui")
                          ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Standard Curve'",
                            uiOutput("sc_fit_module_ui")
                          ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Standard Curve Summary'",
                            uiOutput("standardCurveSummaryUI")
                          )
                        ),

                        tabPanel(
                          id = "advance_qc",
                          title = "Advanced Diagnostics",
                          radioGroupButtons(
                            inputId = "advanced_qc_component",
                            label = "Advanced QC Phase",
                            choices = c("Dilution Analysis", "Dilutional Linearity",
                                        "Outliers", "Subgroup Detection", "Subgroup Detection Summary"),
                            selected = "Dilution Analysis"
                          ),

                          # Conditional panels for advanced QC only
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Dilution Analysis'",
                            uiOutput("dilutionAnalysisUI")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Dilutional Linearity'",
                            uiOutput("dilutional_linearity_mod_ui")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Outliers'",
                            uiOutput("outlierTab")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Subgroup Detection'",
                            uiOutput("subgroupDetectionUI")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Subgroup Detection Summary'",
                            uiOutput("subgroup_summary_UI")
                          )
                        )
                      ),





                       # radioGroupButtons(
                       #   inputId = "qc_component",
                       #   label = "QC Phase",
                       #   choices = c("Data", "Bead Count", "Standard Curve","Standard Curve Summary", "Dilution Analysis", "Dilutional Linearity",
                       #              # "Plate Normalization",
                       #               "Outliers", "Subgroup Detection", "Subgroup Detection Summary"),
                       #   selected = "Data",
                       # #  justified = TRUE,
                       # #  multiple = FALSE
                       # )
                      # verbatimTextOutput(paste(input$qc_component, input$readxMap_study_accession, input$readxMap_experiment_accession, input$study_level_tabs, input$main_tabs, sep = ", "))


                    )
                   #)
                   ),

                   # Experiment Level Content
                   # conditionalPanel(
                   #   condition = "input.readxMap_experiment_accession != 'Click here'",
                   #   uiOutput("stored_plates_ui")
                   # )
                   # tabPanel(
                   #   id = "check_conditions",
                   #   verbatimTextOutput(paste(input$qc_component, input$readxMap_study_accession, input$readxMap_experiment_accession, input$study_level_tabs, input$main_tabs, sep = ", "))
                   # ),


                   # OLD conditions
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Data' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #   uiOutput("dynamic_data_ui")
                   #
                   #   #render_dynamic_ui_content()
                   # ),
                   # conditionalPanel(
                   #   #condition = "input.qc_component == 'Bead Count' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #  condition = "input.qc_component == 'Bead Count'",
                   #  uiOutput("bead_count_module_ui")
                   # ),
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Standard Curve' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #   uiOutput("sc_fit_module_ui")
                   # ),
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Standard Curve Summary' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   # #  uiOutput("sc_summary_module_ui")
                   # uiOutput("standardCurveSummaryUI")
                   # ),
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Dilution Analysis' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #   uiOutput("dilutionAnalysisUI")
                   # ),
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Dilutional Linearity' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #   uiOutput("dilutional_linearity_mod_ui")
                   # ),
                   #
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Outliers'  && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #   uiOutput("outlierTab")
                   # ),
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Subgroup Detection' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #   # uiOutput("dynamic_data_ui"),
                   #   # uiOutput("beadCountAnalysisUI"),
                   #   # uiOutput("standard_curve_section"),
                   #   # uiOutput("dilution_analysis_section"),
                   #   # uiOutput("outlierTab"),
                   #   uiOutput("subgroupDetectionUI")
                   #  # uiOutput("subgroup_detection_section")
                   # ),
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Subgroup Detection Summary' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #    uiOutput("subgroup_summary_UI")
                   #   )
                   # conditionalPanel(
                   #   condition = "input.qc_component == 'Test' && input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
                   #   uiOutput("sg_module_ui")
                   # )

          ) # end TabsetPanel
        ) # end study level tabs
      ) # end  Study Level Content
      ) # end fluidPage
    ) #end tagList
})

# observeEvent({
#   list(input$qc_component,
#   input$readxMap_study_accession,
#   input$readxMap_experiment_accession,
#   input$study_level_tabs,
#   input$main_tabs)
# }, {
#   if (
#     input$qc_component == "Data" &&
#     input$readxMap_study_accession != "Click here" &&
#     input$readxMap_experiment_accession != "Click here" &&
#     input$study_level_tabs == "Experiments" &&
#     input$main_tabs == "view_files_tab"
#   ) {
#     cat("Re-entered Data panel — rerendering dynamic_data_ui\n")
#     rerender_trigger(rerender_trigger() + 1)
#   }
# })

# Data Contents
output$dynamic_data_ui <- renderUI({
  req(input$basic_advance_tabs)
  if (
    input$basic_advance_tabs == "Data" &&
    input$readxMap_study_accession != "Click here" &&
    input$readxMap_experiment_accession != "Click here" &&
    input$study_level_tabs == "Experiments" &&
    input$main_tabs == "view_files_tab"
  ) {
    tabsetPanel(
      id = "dataCollapse",
      tabPanel(
        title = "Plates",
        DT::dataTableOutput("stored_header"),
        downloadButton("download_stored_header"),
        uiOutput("header_actions")
      ),
      tabPanel(
        title = "Standards",
        DT::dataTableOutput("swide_standard"),
        downloadButton("download_stored_standard")
      ),
      tabPanel(
        title = "Controls",
        DT::dataTableOutput("swide_control"),
        downloadButton("download_stored_control")
      ),
      tabPanel(
        title = "Blanks",
        DT::dataTableOutput("swide_buffer"),
        downloadButton("download_stored_buffer")
      ),
      tabPanel(
        title = "Samples",
        DT::dataTableOutput("swide_sample"),
        downloadButton("download_stored_sample")
      )
    )

    # bsCollapse(
    #   id = "dataCollapse",
    #   multiple = FALSE,
    #   bsCollapsePanel(
    #     title = "Header",
    #     DT::dataTableOutput("stored_header"),
    #     downloadButton("download_stored_header"),
    #     uiOutput("header_actions"),
    #     style = "primary"
    #   ),
    #   bsCollapsePanel(
    #     title = "Standards",
    #     DT::dataTableOutput("swide_standard"),
    #     downloadButton("download_stored_standard"),
    #     style = "primary"
    #   ),
    #   bsCollapsePanel(
    #     title = "Controls",
    #     DT::dataTableOutput("swide_control"),
    #     downloadButton("download_stored_control"),
    #     style = "primary"
    #   ),
    #   bsCollapsePanel(
    #     title = "Buffer",
    #     DT::dataTableOutput("swide_buffer"),
    #     downloadButton("download_stored_buffer"),
    #     style = "primary"
    #   ),
    #   bsCollapsePanel(
    #     title = "Sample",
    #     DT::dataTableOutput("swide_sample"),
    #     downloadButton("download_stored_sample"),
    #     style = "primary"
    #   )
    # )
  } else {
    NULL  # Removes the bsCollapse completely
  }
})

observeEvent(input$advanced_qc_component, {
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession)

  if (input$advanced_qc_component == "Dilutional Linearity" &&
      input$readxMap_study_accession != "" &&
      input$readxMap_study_accession != "Click here" &&
      input$readxMap_experiment_accession != "" &&
      input$readxMap_experiment_accession != "Click here") {

    # Destroy previous module (if exists)
    prev_dil_lin_id <- paste0("dil_lin_mod_", reload_dil_lin_count)
    try(destroyModule(prev_dil_lin_id), silent = TRUE)

    # Increment counter and build new ID
    reload_dil_lin_count <<- reload_dil_lin_count + 1
    new_dil_lin_id <- paste0("dil_lin_mod_", reload_dil_lin_count)

    # Render UI and load module
    output$dilutional_linearity_mod_ui <- renderUI({
      destroyableDilutionalLinearityModuleUI(new_dil_lin_id)
    })

    destroyableDilutionalLinearityServer(
      id = new_dil_lin_id,
      selected_study = reactive(input$readxMap_study_accession),
      selected_experiment = reactive(input$readxMap_experiment_accession),
      currentuser()
    )

  } else {
    # If switching away, destroy any existing SC a module
    try(destroyModule(paste0("dil_lin_mod_", reload_dil_lin_count)), silent = TRUE)
    output$dilutional_linearity_mod_ui <- renderUI({ NULL })
  }
})

observeEvent(input$qc_component, {
  cat("QC component selected:\n")
  print(input$qc_component)

  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession)

 # cat("Current open panel(s):", input$standardCurveCollapse, "\n")
  # ----- Bead Count Module -----
  if (input$qc_component == "Bead Count" &&
      input$readxMap_study_accession != "" &&
      input$readxMap_study_accession != "Click here" &&
      input$readxMap_experiment_accession != "" &&
      input$readxMap_experiment_accession != "Click here") {

    # Destroy previous module (if exists)
    prev_bead_mod_id <- paste0("bead_count_mod_", reload_bead_count)
    try(destroyModule(prev_bead_mod_id), silent = TRUE)

    # Increment counter and build new ID
    reload_bead_count <<- reload_bead_count + 1
    new_bead_mod_id <- paste0("bead_count_mod_", reload_bead_count)

    # Render UI and load module
    output$bead_count_module_ui <- renderUI({
      destroyableBeadCountModuleUI(new_bead_mod_id)
    })

    destroyableBeadCountModuleServer(
      id = new_bead_mod_id,
      selected_study = reactive(input$readxMap_study_accession),
      selected_experiment = reactive(input$readxMap_experiment_accession),
      currentuser()
    )

  } else {
    # If switching away, destroy any existing bead module
    try(destroyModule(paste0("bead_count_mod_", reload_bead_count)), silent = TRUE)
    output$bead_count_module_ui <- renderUI({ NULL })
  #  gc(verbose = TRUE)
  }

  # ----- Standard Curve Module -----
  if (input$qc_component == "Standard Curve" &&
      input$readxMap_study_accession != "" &&
      input$readxMap_study_accession != "Click here" &&
      input$readxMap_experiment_accession != "" &&
      input$readxMap_experiment_accession != "Click here") {

    # Destroy previous module (if exists)
    prev_sc_mod_id <- paste0("standard_curve_fit_mod_", reload_sc_fit_mod_count)
    try(destroyModule(prev_sc_mod_id), silent = TRUE)

    # Increment counter and build new ID
    reload_sc_fit_mod_count <<- reload_sc_fit_mod_count + 1
    new_sc_mod_id <- paste0("standard_curve_fit_mod_", reload_sc_fit_mod_count)

    # Render UI and load module
    output$sc_fit_module_ui <- renderUI({
      destroyableStandardCurveFittingModuleUI(new_sc_mod_id)
    })

    destroyableStandardCurveFittingServer(
      id = new_sc_mod_id,
      selected_study = reactive(input$readxMap_study_accession),
      selected_experiment = reactive(input$readxMap_experiment_accession),
      currentuser()
    )

  } else {
    # If switching away, destroy any existing SC module
    try(destroyModule(paste0("standard_curve_fit_mod_", reload_sc_fit_mod_count)), silent = TRUE)
    output$sc_fit_module_ui <- renderUI({ NULL })
   # gc(verbose = TRUE)
  }
  # ----- Standard Curve Summary Module -----
  # if (input$qc_component == "Standard Curve Summary" &&
  #     input$readxMap_study_accession != "" &&
  #     input$readxMap_study_accession != "Click here" &&
  #     input$readxMap_experiment_accession != "" &&
  #     input$readxMap_experiment_accession != "Click here") {
  #
  #   # Destroy previous module (if exists)
  #   prev_sc_summary_mod_id <- paste0("standard_curve_sum_mod_", reload_sc_summary_mod_count)
  #   try(destroyModule(prev_sc_summary_mod_id), silent = TRUE)
  #
  #   # Increment counter and build new ID
  #   reload_sc_summary_mod_count <<- reload_sc_summary_mod_count + 1
  #   new_sc_sum_mod_id <- paste0("standard_curve_sum_mod_", reload_sc_summary_mod_count)
  #
  #   # Render UI and load module
  #   output$sc_summary_module_ui <- renderUI({
  #     destroyableStandardCurveSummaryModuleUI(new_sc_sum_mod_id)
  #   })
  #
  #   destroyableStandardCurveSummaryModuleServer(
  #     id = new_sc_sum_mod_id,
  #     selected_study = reactive(input$readxMap_study_accession),
  #     selected_experiment = reactive(input$readxMap_experiment_accession),
  #     currentuser()
  #   )
  #
  # } else {
  #   # If switching away, destroy any existing SC a module
  #   try(destroyModule(paste0("standard_curve_sum_mod_", reload_sc_summary_mod_count)), silent = TRUE)
  #   output$sc_summary_module_ui <- renderUI({ NULL })
  #   gc(verbose = TRUE)
  # }

  # if (input$qc_component == "Test" &&
  #     input$readxMap_study_accession != "" &&
  #     input$readxMap_study_accession != "Click here" &&
  #     input$readxMap_experiment_accession != "" &&
  #     input$readxMap_experiment_accession != "Click here") {
  #
  #   # Destroy previous module (if exists)
  #   prev_sc_summary_mod_id <- paste0("subgroup_mod_", reload_sg_count)
  #   try(destroyModule(prev_sc_summary_mod_id), silent = TRUE)
  #
  #   # Increment counter and build new ID
  #   reload_sg_count <<- reload_sg_count + 1
  #   new_sc_sum_mod_id <- paste0("subgroup_mod_", reload_sg_count)
  #
  #   # Render UI and load module
  #   output$sg_module_ui <- renderUI({
  #     destroyableSubgroupDetectionModuleUI(new_sc_sum_mod_id)
  #   })
  #
  #   destroyableSubgroupDetectionServer(
  #     id = new_sc_sum_mod_id,
  #     selected_study = reactive(input$readxMap_study_accession),
  #     selected_experiment = reactive(input$readxMap_experiment_accession),
  #     currentuser()
  #   )
  #
  # } else {
  #   # If switching away, destroy any existing SC a module
  #   try(destroyModule(paste0("subgroup_mod_", reload_sg_count)), silent = TRUE)
  #   output$sg_module_ui <- renderUI({ NULL })
  #   gc(verbose = TRUE)
  # }

  # if (input$advanced_qc_component == "Dilutional Linearity" &&
  #     input$readxMap_study_accession != "" &&
  #     input$readxMap_study_accession != "Click here" &&
  #     input$readxMap_experiment_accession != "" &&
  #     input$readxMap_experiment_accession != "Click here") {
  #
  #   # Destroy previous module (if exists)
  #   prev_dil_lin_id <- paste0("dil_lin_mod_", reload_dil_lin_count)
  #   try(destroyModule(prev_dil_lin_id), silent = TRUE)
  #
  #   # Increment counter and build new ID
  #   reload_dil_lin_count <<- reload_dil_lin_count + 1
  #   new_dil_lin_id <- paste0("dil_lin_mod_", reload_dil_lin_count)
  #
  #   # Render UI and load module
  #   output$dilutional_linearity_mod_ui <- renderUI({
  #     destroyableDilutionalLinearityModuleUI(new_dil_lin_id)
  #   })
  #
  #   destroyableDilutionalLinearityServer(
  #     id = new_dil_lin_id,
  #     selected_study = reactive(input$readxMap_study_accession),
  #     selected_experiment = reactive(input$readxMap_experiment_accession),
  #     currentuser()
  #   )
  #
  # } else {
  #   # If switching away, destroy any existing SC a module
  #   try(destroyModule(paste0("dil_lin_mod_", reload_dil_lin_count)), silent = TRUE)
  #   output$dilutional_linearity_mod_ui <- renderUI({ NULL })
  # }

  gc(verbose = TRUE)
})
#
# observeEvent(input$qc_component, {
#  # req(input$qc_component)
#  # req(input$qc_component != "Select Phase")
#   cat("QC component\n")
#   req(input$readxMap_study_accession)
#   req(input$readxMap_experiment_accession)
#   print(input$qc_component)
#
#  # cat("Standard Curve Collapse Status\n")
#  # print(input$StandardCurveCollapse)
#  # if (input$qc == "Bead Count") {
#   # if (input$qc_component == "Bead Count" &&  !is.null(selected_studyexpplate$study_accession) &&
#   #     selected_studyexpplate$study_accession != "" &&  !is.null(selected_studyexpplate$experiment_accession) &&  selected_studyexpplate$experiment_accession != ""
#   #     && selected_studyexpplate$study_accession != "Click here" &&  selected_studyexpplate$experiment_accession != "Click here") {
#   if (input$qc_component == "Bead Count" && input$readxMap_study_accession != "" && input$readxMap_study_accession != "Click here" &&
#       input$readxMap_experiment_accession != "" && input$readxMap_experiment_accession != "Click here") {
#        try(destroyModule("bead_count_mod"), silent = TRUE)
#        #reload_bead_count(reload_bead_count() + 1)
#         reload_bead_count <<- reload_bead_count + 1 # global to source counter outside of observeEvent
#       output$bead_count_module_ui <- renderUI({
#         #destroyableBeadCountModuleUI("bead_count_mod")
#         destroyableBeadCountModuleUI(paste0("bead_count_mod_", reload_bead_count))
#
#       })
#     destroyableBeadCountModuleServer(id = paste0("bead_count_mod_",  reload_bead_count), selected_study = reactive(input$readxMap_study_accession),
#                                      selected_experiment = reactive(input$readxMap_experiment_accession), currentuser())
#   } else {
#     destroyModule("bead_count_mod")
#     output$bead_count_module_ui <- renderUI({ NULL })  # Remove UI
#     gc(verbose = TRUE)
#   }
#
#   if (input$qc_component == "Standard Curve" && input$readxMap_study_accession != "" && input$readxMap_study_accession != "Click here" &&
#       input$readxMap_experiment_accession != "" && input$readxMap_experiment_accession != "Click here") {
#     try(destroyModule("standard_curve_fit_mod"), silent = TRUE)
#     #reload_bead_count(reload_bead_count() + 1)
#     reload_sc_fit_mod_count <<- reload_sc_fit_mod_count + 1 # global to source counter outside of observeEvent
#     output$sc_fit_module_ui <- renderUI({
#       #destroyableBeadCountModuleUI("bead_count_mod")
#       destroyableStandardCurveFittingModuleUI(paste0("standard_curve_fit_mod_", reload_sc_fit_mod_count))
#
#     })
#     destroyableStandardCurveFittingServer(id = paste0("standard_curve_fit_mod_",  reload_sc_fit_mod_count), selected_study = reactive(input$readxMap_study_accession),
#                                      selected_experiment = reactive(input$readxMap_experiment_accession), currentuser())
#   } else {
#     destroyModule("standard_curve_fit_mod")
#     output$sc_fit_module_ui <- renderUI({ NULL })  # Remove UI
#     gc(verbose = TRUE)
#   }
# })
  # all_outputs <- outputOptions(NULL)
  # all_outputs_v <<- all_outputs
  # output_ids <- ls(session$output)
  # output_objects <- output
  #
  # output_ids <- ls(output)
  #
  # output_list <- lapply(output_ids, function(id) {
  #   output[[id]]
  # })
  # output_list <<- output_list

  # session_output <- session$output
  #
  # temp_outputs_v <<- output_ids
  # session_output_v <<- session_output
  # if (input$qc_component != "Data") {
  #   cat("removing non related data output")
  #   removeOutput("stored_header", session = session)
  #   removeOutput("swide_standard", session = session)
  #   removeOutput("swide_control", session = session)
  #   removeOutput("swide_buffer", session = session)
  #   removeOutput("swide_sample", session = session)
  #   removeOutput("dynamic_data_ui", session = session)
  #   # output$stored_header <- NULL
  #   # output$swide_standard <- NULL
  #   # output$swide_control <- NULL
  #   # output$swide_buffer <- NULL
  #   # output$swide_sample <- NULL
  #   # output$dynamic_data_ui <- NULL
  #
  #   gc()
  # }
  # if (input$qc_component != "Bead Count") {
  #   removeOutput("beadCountAnalysisUI", session = session)
  #   removeOutput("plateSelection_bead_count_UI", session = session)
  #   removeOutput("sample_data_antigenUI", session = session)
  #   removeOutput("beadCountPlot", session = session)
  #   removeOutput("sample_low_bead_count_table", session = session)
  #   removeOutput("download_bead_gating", session = session)
  #
  #   gc(verbose = T)
  # }

#})


# my_session_info <- reactive({
#   session  # or any other relevant part
# })

# observeEvent(input$qc_component, {
#   req(input$readxMap_experiment_accession)
#   req(stored_plates_data)
#   req(input$qc_component != "Select Phase")
#   cat("QC component")
#   print(input$qc_component)
#
#   # m <<- m + 1
#   # print(paste("plate_selected", plate_selected))
#   # update_modal_progress(
#   #   value = m / 8,
#   #   text = "Rendering wide tables",
#   #   session = shiny::getDefaultReactiveDomain()
#   # )
#
#
# #
# #   tabsetPanel(id = "inLoadedData",
# #               tabPanel(
# #                 title = "Data",
# #                 br(),
# #                 bsCollapse(
# #                   id = "dataCollapse",
# #                   multiple = FALSE,
# #                   bsCollapsePanel(
# #                     title = "Header",
# #                     DT::dataTableOutput("stored_header"),
# #                     downloadButton("download_stored_header"),
# #                     uiOutput("header_actions"),
# #                     style = "primary"
# #                   ),
# #                   bsCollapsePanel(
# #                     title = "Standards",
# #                     DT::dataTableOutput("swide_standard"),
# #                     downloadButton("download_stored_standard"),
# #                     style = "primary"
# #                   ),
# #                   bsCollapsePanel(
# #                     title = "Controls",
# #                     DT::dataTableOutput("swide_control"),
# #                     downloadButton("download_stored_control"),
# #                     style = "primary"
# #                   ),
# #                   bsCollapsePanel(
# #                     title = "Buffer",
# #                     DT::dataTableOutput("swide_buffer"),
# #                     downloadButton("download_stored_buffer"),
# #                     style = "primary"
# #                   ),
# #                   bsCollapsePanel(
# #                     title = "Sample",
# #                     DT::dataTableOutput("swide_sample"),
# #                     downloadButton("download_stored_sample"),
# #                     style = "primary" # set to open initially
# #                   ),
# #                   open = "Header"
# #                 )
# #               ))
# }, ignoreInit = FALSE)

observeEvent(input$study_level_tabs, {
  if (input$study_level_tabs == "Experiments") {
    updateRadioGroupButtons(session, "qc_component", selected = "Data")
  }
})

load_project <- function(conn, project_id, current_user){
    if (project_id != "") {
      dbBegin(conn)
      tryCatch({
        result <- dbGetQuery(conn, glue::glue("SELECT COUNT(*) as user_count FROM madi_lumi_users.project_users WHERE project_id = {project_id} AND user_id = '{current_user}'"))
        if (result$user_count > 0) {
          # Retrieve the project name for the given project_id
          project_name_query <- glue::glue("SELECT project_name FROM madi_lumi_users.projects WHERE project_id = {project_id}")
          project_name_result <- dbGetQuery(conn, project_name_query)

          if (nrow(project_name_result) > 0) {
            project_name <- project_name_result$project_name[1]

            query_check <- sprintf("SELECT 1 FROM madi_results.xmap_users WHERE auth0_user = %s", dbQuoteLiteral(conn, current_user))
            exists <- dbGetQuery(conn, query_check)

            # Insert or update the user record
            if (nrow(exists) == 0) {

              query_insert <- glue::glue("INSERT INTO madi_results.xmap_users (auth0_user, project_name, workspace_id) VALUES ({dbQuoteLiteral(conn, current_user)}, {dbQuoteLiteral(conn, project_name)}, {project_id})")
              dbExecute(conn, query_insert)
              message("New user inserted in xmap.")
            } else {

              query_update <- glue::glue("UPDATE madi_results.xmap_users SET project_name = {dbQuoteLiteral(conn, project_name)}, workspace_id = {project_id} WHERE auth0_user = {dbQuoteLiteral(conn, current_user)}")
              dbExecute(conn, query_update)

              message("User updated in xmap.")
            }

            dbCommit(conn)
            showNotification(glue::glue("Project {project_id} loaded successfully!"), type = "message")
          } else {
            dbRollback(conn)
            showNotification("Project not found.", type = "error")
          }
        } else {
          dbRollback(conn)
          showNotification("You do not have access to the project you are trying to load.", type = "error")
        }
      }, error = function(e) {
        dbRollback(conn)
        showNotification(glue::glue("Failed to load project '{project_id}'. Error: {e$message}"), type = "error")
      })
    }
  source("import_lumifile.R", local=TRUE)
}

observeEvent(userWorkSpaceID(), {
  reset_import_values()
})


refreshTabUI <- function(tabName) {
  # Get current counters
  current_counters <- tabRefreshCounter()
  # Increment counter for specific tab
  current_counters[[tabName]] <- current_counters[[tabName]] + 1
  # Update counters
  tabRefreshCounter(current_counters)
}


observeEvent(input$main_tabs, {
  currentTab <- input$main_tabs

  # Reset values when moving away from Import or View Files tabs
  if (!is.null(previousTab())) {
    if (previousTab() == "import_tab" && currentTab != "import_tab") {
      reset_import_values()
    }
    if (previousTab() == "view_files_tab" && currentTab != "view_files_tab") {
      reset_view_values()
    }
  }

  # Refresh current tab
  refreshTabUI(currentTab)
  previousTab(currentTab)
}, ignoreInit = TRUE)

# Create a reset function for view values
reset_view_values <- function() {
  updateSelectInput(session, "readxMap_study_accession", selected = "Click here")
  updateSelectInput(session, "readxMap_experiment_accession", selected = "Click here")
}

output$manage_project_ui <- renderUI({
  fluidRow(
    column(12,
           # Create Project Section
           wellPanel(
             h4("Create New Project"),
             bsCollapse(
               id = "createNewProjectCollapse",
               bsCollapsePanel(
                 title = "Create New Project Documentation",
                 style = "success",
                 tagList(
                   tags$p("To create a new project in the 'Enter Project Name:' field
                          type the name of the new project and press 'Enter'.")
                 )
               )
             ),
             textInput("project_name", "Enter Project Name:"),
             actionButton("create_project", "Enter", class = "btn-success")
           ),
           hr(),
           # Add Project Section
           wellPanel(
             h4("Add New Project"),
             bsCollapse(
               id = "addProjectDocumentation",
               bsCollapsePanel(
                 title = "Create New Project Documentation",
                 style = "success",
                 tagList(
                   tags$p("To add a new project given an existing project ID and access ID,
                          type the project ID and access ID in the respective fields and press 'Add Project'")
                 )
               )
             ),
             textInput("project_id", "Enter Project ID:"),
             textInput("access_id", "Enter Access ID:"),
             actionButton("add_project", "Add Project", class = "btn-success")
           ),
           hr(),
           # Load Project Section
           wellPanel(
             h4("Load Existing Project"),
             uiOutput("load_ui")
           )
    )
  )
})

