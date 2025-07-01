# ui handling

reset_import_values <- function() {
  # Reset all reactive values
  isolate({
    # Single values
    upload_state_value$upload_state <- NULL
    rv_value_button$valueButton <- 0

    # Clear reactiveValues objects
    header_rvdata$data <- NULL
    standard_rvdata$data <- NULL
    sample_rvdata$data <- NULL
    control_rvdata$data <- NULL
    buffer_rvdata$data <- NULL

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
  tagList(
    fluidPage(
      h3("Interactive Serology Plate Inspector - Stored Plate Data"),

      # Study Selection
      fluidRow(
        column(5,
               selectInput("readxMap_study_accession",
                           "Choose Study Name",
                           choices = c("Click here" = "Click here",
                                       setNames(unique(reactive_df_study_exp()$study_accession),
                                                unique(reactive_df_study_exp()$study_name))),
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




          # Study Overview Tab
          tabPanel("Study Overview",
                   id = "study_overview_tab",
                   bsCollapse(
                     id = "studyOverviewCollapse",
                       bsCollapsePanel(
                         title = "Antigen Family",
                         div(
                           style = "padding: 15px; width: 100%; max-width: 100%; overflow-x: auto;",  # Added styling
                           uiOutput("antigenFamilyUI")
                         ),
                         style = "primary"
                       ),
                     bsCollapsePanel(
                       title = "Bead Count Options",
                       div(
                         style = "padding: 15px; width: 100%; max-width: 100%; overflow-x: auto;",  # Added styling
                         uiOutput("bead_count_controls_UI")
                       ),
                       style = "primary"
                     ),
                     bsCollapsePanel(
                       title = "Standard Curve Dilution Options",
                       div(
                         style = "padding: 15px; width: 100%; max-width: 100%; overflow-x: auto;",  # Added styling
                          uiOutput("dilution_standards_controls")
                       ),
                       style = "primary"
                     ),
                       bsCollapsePanel(
                         title = "Blank Controls",
                         div(
                           style = "padding: 15px; width: 100%; max-width: 100%; overflow-x: auto;",  # Added styling
                           uiOutput("blankControlOptionUI")
                         ),
                         style = "primary"
                       ),
                       bsCollapsePanel(
                         title = "Dilution Analysis Parameters",
                         div(
                           style = "padding: 15px; width: 100%; max-width: 100%; overflow-x: auto; overflow-y: auto;",
                           uiOutput("dilution_paramatersUI")
                         ),
                         style = "primary"
                       ),
                       bsCollapsePanel(
                         title = "Set Reference Arm",
                         div(
                           style = "padding: 15px; width: 100%; max-width: 100%; overflow-x: auto;",
                           uiOutput("reference_arm_UI")
                         ),
                         style = "primary"
                       )
                   )
          ),

          # Experiment Level Tab
          tabPanel("Experiments",
                   fluidRow(
                     column(5,
                            selectInput("readxMap_experiment_accession",
                                        "Choose Experiment Name",
                                        choices = c("Click here" = "Click here",
                                                    setNames(reactive_df_study_exp()$experiment_accession,
                                                             reactive_df_study_exp()$experiment_name)),
                                        selected = "Click here",
                                        multiple = FALSE
                            )
                     )
                   ),

                   # Experiment Level Content
                   conditionalPanel(
                     condition = "input.readxMap_experiment_accession != 'Click here'",
                     uiOutput("stored_plates_ui")
                   )
          )
        )
      )
    )
  )
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

