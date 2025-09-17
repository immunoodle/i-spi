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


# output$sidebar_tabs <- renderMenu({
#   current_count <- tab_counter()
#   dynamic_items <- lapply(seq_len(current_count), function(i) {
#     menuItem(paste("Dynamic Tab", i),
#              tabName = paste0("dynamic_tab_", i), icon = icon("folder"))
#   })
#
#   sidebarMenu(id = "sidebar_tabs",
#               menuItem("View, Process, and Export Data", tabName = "view_files_tab", icon = icon("dashboard")),
#               menuItem("Import Plate Data", tabName = "import_tab", icon = icon("file")),
#               menuItem("Create, Add, and Load Projects", tabName = "manage_project_tab", icon = icon("chart-line")),
#               dynamic_items
#   )
# })
#
# output$body_tabs <- renderUI({
#   current_count <- tab_counter()
#   dynamic_tabs <- lapply(seq_len(current_count), function(i) {
#     tabItem(tabName = paste0("dynamic_tab_", i),
#             uiOutput(paste0("dynamic_content_", i)))
#   })
#
#   do.call(tabItems, c(
#     list(
#       tabItem(tabName = "view_files_tab", uiOutput("view_stored_experiments_ui")),
#       tabItem(tabName = "import_tab", uiOutput("view_stored_experiments_ui")),
#       tabItem(tabName = "manage_project_tab", uiOutput("manage_project_ui"))
#     ),
#     dynamic_tabs
#   ))
# })


# output$sidebar_tabs <- renderMenu({
#   current_count <- tab_counter()
#   dynamic_items <- lapply(seq_len(current_count), function(i) {
#     menuItem(paste("Dynamic Tab", i),
#              tabName = paste0("dynamic_tab_", i), icon = icon("folder"))
#   })
#
#   do.call(sidebarMenu, c(
#     list(id = "sidebar_tabs",selected = "view_files_tab",
#          menuItem("View, Process, and Export Data", tabName = "view_files_tab", icon = icon("dashboard")),
#          menuItem("Import Plate Data", tabName = "import_tab", icon = icon("file")),
#          menuItem("Create, Add, and Load Projects", tabName = "manage_project_tab", icon = icon("chart-line"))
#     ),
#     dynamic_items
#   ))
# })

# output$sidebar_tabs <- renderMenu({
#   current_count <- tab_counter()
#
#   # dynamic_items <- lapply(seq_len(current_count), function(i) {
#   #   menuItem(
#   #     paste("Dynamic Tab", i),
#   #     tabName = paste0("dynamic_tab_", i),
#   #     icon = icon("folder")
#   #   )
#   # })
#
#   do.call(sidebarMenu, c(
#     list(
#       id = "main_tabs",
#     #  selected = "view_files_tab",  # initial selected tab
#       menuItem("Home", tabName = "home_page", icon = icon("home")),      # landing page content
#       menuItem("Create, Add, and Load Projects", tabName = "manage_project_tab", icon = icon("chart-line")),
#       menuItem("Import Plate Data", tabName = "import_tab", icon = icon("file")),
#       menuItem("Change Study Settings", tabName = "study_settings", icon = icon("cog")),
#       menuItem("View, Process, and Export Data", tabName = "view_files_tab", icon = icon("dashboard"))
#
#     )
#     #dynamic_items  # add dynamic tabs at the end
#   ))
# })

output$project_info <- renderUI({
  tagList(
    div(
      style = paste0(
        "background-color: #666666; padding: 15px; border-radius: 8px; ",
        "margin: 0 auto 20px auto; border: 2px solid #2c3e50; ",
        "width: 90%; max-width: 400px; box-sizing: border-box;"),
  p(
    strong("Project: ", style = "color: white;"),
    span(userProjectName(), style = "color: white;"), br(),
    strong("Project ID: ", style = "color: white;"),
    span(userWorkSpaceID(), style = "color: white;")
  )
  )
  )
})

output$main_study_selector <- renderUI({
  req(reactive_df_study_exp())
  req(study_choices_rv())
  # Get data
  df <- reactive_df_study_exp()
  df <- df[df$study_accession != "Click here", ]


  # Build choices safely
  # study_choices <- c("Click here" = "Click here",
  #                    setNames(unique(df$study_accession),
  #                             unique(df$study_name)))
  #

  selectizeInput("readxMap_study_accession",
               "Choose Existing Study Name OR Create a New Study Name (by typing up to 15 characters)",
               # choices <- c(c("Click OR Create New" = "Click here"),
               #              setNames(unique(reactive_df_study_exp()$study_accession),
               #                       unique(reactive_df_study_exp()$study_name)
               #              )
               # ),
               choices = study_choices_rv(),
               #selected = "Click here",
               multiple = FALSE,
               options = list(create = TRUE), width = '500px'
)
})

# add value to manual studies when new study is added
# observeEvent(input$readxMap_study_accession, {
#   val <- input$readxMap_study_accession
#   # ignore initial "Click here"
#   if (is.null(val) || val == "Click here") return()
#   # get source list
#   src_df <- reactive_df_study_exp()
#   src_acc <- if (!is.null(src_df)) unique(src_df$study_accession) else character(0)
#
#   if (!(val %in% src_acc) && !(val %in% manual_studies$entries)) {
#     manual_studies$entries <- c(manual_studies$entries, val)
#   }
# }, ignoreNULL = TRUE)


# observe({
#   req(reactive_df_study_exp())   # wait until source is available
#   df <- reactive_df_study_exp()
#
#   # remove any rows with "Click here"
#   df <- df[!is.na(df$study_accession) & df$study_accession != "Click here", , drop = FALSE]
#
#   # unique mapping: study_accession -> study_name for labeling
#   uniq <- df[!duplicated(df$study_accession), c("study_accession", "study_name"), drop = FALSE]
#
#   # named vector: value = accession, name = study_name
#   named_src <- if (nrow(uniq) > 0) setNames(as.character(uniq$study_accession),
#                                             as.character(uniq$study_name)) else character(0)
#
#   # combine: Click here first, then source, then manual typed entries
#   named_choices <- c("Click here" = "Click here")
#
#   if (length(named_src) > 0) named_choices <- c(named_choices, named_src)
#
#   if (length(manual_studies$entries) > 0) {
#     # add manual entries (label == value)
#     named_choices <- c(named_choices,
#                        setNames(manual_studies$entries, manual_studies$entries))
#   }
#
#   # keep user's current selection if possible; otherwise fall back to "Click here"
#   current <- input$readxMap_study_accession
#   if (is.null(current) || !(current %in% c("Click here", names(named_choices), as.character(named_choices)))) {
#     current <- "Click here"
#   }
#
#   # update the UI — don't rebuild the widget, just change choices/selection
#   updateSelectizeInput(
#     session,
#     "readxMap_study_accession",
#     choices = named_choices,
#     selected = current,
#     options = list(create = TRUE)   # keep create enabled
#   )
# })




# observe({
#   req(reactive_df_study_exp())
#
#   # Get data
#   df <- reactive_df_study_exp()
#   df <- df[df$study_accession != "Click here", ]
#
#   # Build updated choices
#   study_choices <- c("Click here" = "Click here",
#                      setNames(unique(df$study_accession),
#                               unique(df$study_name)))
#
#   # # If user typed a new value, make sure it’s kept in the dropdown
#   current <- input$readxMap_study_accession
#   if (!is.null(current) && !(current %in% study_choices)) {
#     study_choices <- c(study_choices, setNames(current, current))
#   }
#   #
#   # Update the selectize input (keeps selection unless you override)
#   updateSelectizeInput(
#     session,
#     "readxMap_study_accession",
#     choices = study_choices,
#     selected = current
#   )
# })

output$landing_page_ui <- renderUI({
  cat(input$main_tabs)
  if (is.null(input$main_tabs) || input$main_tabs == "home_page") {   # nothing selected yet
    fluidRow(
      div(style = "padding-left: 50px; padding-right: 50px;",
      tagList(
       # img(src = "I_SPI_logo.png"),
        img(src = "I_SPI_logo_transparent.png",
            style = "max-width:40%; height:auto;"),
        br(),
        br(),
        p(strong("Overview")),
        p("The Interactive Serology Plate Inspector, also known as I-SPI, is an open-source web application designed to streamline quality control
        (QC) and quality assurance (QA) for multiplex immunoassays.
          It provides a unified workflow that supports both automation and user decision making while remaining grounded in
          objective statistical algorithms."),
        p(strong("Quick Start Guide")),
        tags$ol(
          tags$li(
            tags$p("Create or Load a Project"),
            tags$p("To get started, the first step is to create a new project or load an existing project.
            To do this, click Create, Add, and Load Projects in the sidebar.
            Project access keys allowing  you to share projects with others can be found here.")
          ),
          tags$li(
            tags$p("Create or Load a Study"),
            tags$p("To import your data into a study, click Import Plate Data in the sidebar. Select an existing study or create a new study by typing a new name into the study field in the sidebar.
                   Plate data can be imported into I-SPI once a study is selected.")
          ),
          tags$li(
            tags$p("Change Study Settings"),
            tags$p("If you need to change a study’s settings, click Change Study Settings in the sidebar.")
          ),
          tags$li(
            tags$p("Conduct Analyses"),
            tags$p("To conduct QA and QC analyses on your study data, click View, Process, and Export Data in the sidebar.")
          )
        ),
        p("The organization of projects, studies and experiments and how data and QC/QA results can be shared is outlined in the figure below."),
        img(src = "research_ISPI_organization.png", style = "max-width: 80%;"),
        br(),
        br(),
        HTML(
          'For more detailed documentation on I-SPI please visit
   <a href="https://immunoodle.org/" target="_blank">Immunoodle</a>.
   All the Immunoodle tools are available through
   <a href="https://github.com/immunoodle/deployment" target="_blank">GitHub</a>.
          To download the source code for I-SPI please visit the <a href = "https://github.com/immunoodle/i-spi", target="_blank"> I-SPI repository</a>.
          We welcome feedback which can be given on the <a href = "https://github.com/immunoodle/i-spi/issues", target ="_blank"> issues page </a> of our repository for I-SPI.'
        ),
        br(),
        br(),
        p(strong("Citing I-SPI")),
        p("Citation information is coming soon.")
        # p("For more detailed documentation on I-SPI please visit ",
        # a("Immunoodle", href = "https://immunoodle.org/", target = "_blank"), ".All the Immunoodle tools are available through",
        # a("GitHub", href = "https://github.com/immunoodle/deployment"), ".", target = "_blank")


      )
    )
    )
  }
})



output$body_tabs <- renderUI({
  current_count <- tab_counter()

  dynamic_tabs <- lapply(seq_len(current_count), function(i) {
    tabItem(
      tabName = paste0("dynamic_tab_", i),
      uiOutput(paste0("dynamic_content_", i))  # placeholder for dynamic content
    )
  })

  do.call(tabItems, c(
    list(
      tabItem(tabName = "view_files_tab", uiOutput("view_stored_experiments_ui")),
      tabItem(tabName = "study_settings", uiOutput("studyParameters_UI")),
      tabItem(tabName = "import_tab", uiOutput("readxMapData")),
      tabItem(tabName = "manage_project_tab", uiOutput("manage_project_ui"))
    )
   # dynamic_tabs  # append dynamic tabs here
  ))

})


#
# observe({
#   req(input$main_tabs)
#   if (input$main_tabs == "home_tab") {
#     output$body_tabs <- renderUI({
#       h2("Welcome to I-SPI")
#     })
#   }
# })
# observe({
#   req(input$main_tabs)  # make sure it exists
#   if (input$main_tabs == "home_tab") {
#     output$home_tab <- renderUI({
#       h2("Welcome to I-SPI")
#       # You can add more landing content here
#     })
#   } else {
#     output$home_tab <- renderUI({
#       NULL  # hide content when not on Home tab
#     })
#   }
# })

# output$body_tabs <- renderUI({
#   current_count <- tab_counter()
#   dynamic_tabs <- lapply(seq_len(current_count), function(i) {
#     tabItem(tabName = paste0("dynamic_tab_", i),
#             uiOutput(paste0("dynamic_content_", i)))
#   })
#
#   do.call(tabItems, c(
#     list(
#       tabItem(tabName = "view_files_tab", uiOutput("view_stored_experiments_ui")),
#       tabItem(tabName = "import_tab", uiOutput("readxMapData")),
#       tabItem(tabName = "manage_project_tab", uiOutput("manage_project_ui"))
#     ),
#     dynamic_tabs
#   ))
# })




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
  if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab" & input$study_tabs == "view_files_tab") {
    if (input$readxMap_study_accession != "Click here") {

 # tabRefreshCounter()$view_files_tab
  req(reactive_df_study_exp())
  # Get data
  df <- reactive_df_study_exp()
  #df <- df[df$study_accession != "Click here", ]
  #df_v <<- df
  # Build choices safely
  # study_choices <- c("Click here" = "Click here",
  #                    setNames(unique(df$study_accession),
  #                             unique(df$study_name)))
  # experiment_choices <- c("Click here" = "Click here",
  #                   setNames(unique(df$experiment_accession),
  #                            unique(df$experiment_name)))

 # if (!is.null(input$readxMap_study_accession)) {
    df_filtered <- df[df$study_accession == input$readxMap_study_accession, ]
    experiment_choices <- c(
      "Click here" = "Click here",
      setNames(df_filtered$experiment_accession, df_filtered$experiment_name)
    )
  #}

# if (input$readxMap_study_accession != "Click here") {
   stored_plate_title <- paste("View, Process, and Export", input$readxMap_study_accession, "Data", sep = " ")

 # } else if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab") {
 #   stored_plate_title <- paste("Choose or create a study to View, Process, and Export Data")
 # } else {
 #   stored_plate_title <- ""
 # }
  tagList(
    fluidPage(
      h3(stored_plate_title),

      # Study Level Content
        tabsetPanel(
          id = "study_level_tabs",
          # Experiment Level Tab
          tabPanel("Experiments",
                   fluidRow(
                    # column(6,
                            selectInput("readxMap_experiment_accession",
                                        "Choose Experiment Name",
                                        choices = experiment_choices,
                                        # choices = c("Click here" = "Click here",
                                        #             setNames(df$experiment_accession,
                                        #                      df()$experiment_name)),
                                        selected = "Click here",
                                        multiple = FALSE
                            ),
                    # ),
                     #column(6,
                     conditionalPanel(
                       condition = "input.readxMap_experiment_accession != 'Click here' &&
                       input.study_level_tabs == 'Experiments' && input.study_tabs== 'view_files_tab'",

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
                            selected = character(0)
                          ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Bead Count'",
                            textOutput("Bead Count")
                            #uiOutput("bead_count_module_ui")
                          ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Standard Curve'",
                            textOutput("Standard Curve Fitting")
                           # uiOutput("sc_fit_module_ui")
                          ),
                          conditionalPanel(
                            condition = "input.qc_component == 'Standard Curve Summary'",
                          #  uiOutput("standardCurveSummaryUI")
                          textOutput("Standard Curve Summary")
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
                            selected = character(0)
                          ),

                          # Conditional panels for advanced QC only
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Dilution Analysis'",
                            textOutput("Dilution Analysis")
                          #  uiOutput("dilutionAnalysisUI")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Dilutional Linearity'",
                            textOutput("Dilution Linearity")
                           # uiOutput("dilutional_linearity_mod_ui")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Outliers'",
                            textOutput("Outliers")
                          #  uiOutput("outlierTab")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Subgroup Detection'",
                            textOutput("Subgroup Detection")
                           # uiOutput("subgroupDetectionUI")
                          ),
                          conditionalPanel(
                            condition = "input.advanced_qc_component == 'Subgroup Detection Summary'",
                            textOutput("Subgroup Summary")
                          #  uiOutput("subgroup_summary_UI")
                          )
                        )
                      ),
                    )
                   #)
                   ),
          ),
          # Study Overview Tab
          tabPanel("Study Overview",
                   id = "study_overview_tab",
                   textOutput("Study Overview")
                  # uiOutput("study_overview_page")
          ),# end TabsetPanel
        #) # end study level tabs
      ) # end  Study Level Content
      ) # end fluidPage
    ) #end tagList
    } # end test for click here
    else {
      stored_plate_title <- paste("Choose or create a study to View, Process, and Export Data")
    }
  } # end outer if for nav
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


# deselect whatever was selected for when you come back to the options.
# observeEvent(input$basic_advance_tabs, {
#   if (input$basic_advance_tabs != "basic_qc") {
#     updateRadioGroupButtons(session, "qc_component", selected = character(0))
#   }
#   if (input$basic_advance_tabs != "advance_qc") {
#     updateRadioGroupButtons(session, "advanced_qc_component", selected = character(0))
#   }
# })
observeEvent(input$basic_advance_tabs, {
  if (input$basic_advance_tabs == "basic_qc") {
    updateRadioGroupButtons(session, "advanced_qc_component", selected = character(0))
  }
  # if (input$basic_advance_tabs == "advance_qc") {
  #   updateRadioGroupButtons(session, "qc_component", selected = character(0))
  # }
  # if (input$basic_advance_tabs == "Data") {
  #   updateRadioGroupButtons(session, "qc_component", selected = character(0))
  #   updateRadioGroupButtons(session, "advanced_qc_component", selected = character(0))
  # }
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
  if (input$main_tabs != "home_page") {
  fluidRow(
    column(12,
           # Create Project Section
           h3("Project Management"),
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
  } else {
     NULL
  }
})

