# user.R

currentuser <- reactiveVal("unknown user")
current_user_compress <- reactiveVal("unknown user")
current_user_nocompress <- reactiveVal("unknown user")
userData_upload <- reactiveVal() # to fetch user data from the madi_track.user table

output$auth0_user_info <- renderPrint({
  session$userData$auth0_info
})

# User loading
observe({
  if (session$userData$app_logic_initialized == FALSE) {
    current_user <- 'hardik.gupta.th'
    current_login <- "Local as "
    cat("Debug output:", "Auth0 not working or not logged in.", "\n", file = stderr())
  } else {
    current_user <- toString(user_data()$email)
    current_login <- "Welcome"

    cat("Debug output:", "Auth0 working.", "\n", file = stderr())
  }

  usersession <- paste0(sep = " ", "user:", current_user)
  print(paste("usersession:", usersession))
  current_user_compress(gsub("[[:punct:][:blank:]]+", "", current_user))
  current_user_nocompress(current_user)
  print(paste("current_user_compress:", current_user_compress()))

  removeTab(inputId = "body_panel_id", target = "userData")

  currentuser(current_user)
  current_project_details <- getProjectName(conn, currentuser())
  userWorkSpaceID(current_project_details$id)
  userProjectName(current_project_details$name)

  select_query <- glue::glue_sql("
    SELECT
      xmap_header.study_accession,
      xmap_header.experiment_accession,
      xmap_header.study_accession AS study_name,
      xmap_header.experiment_accession AS experiment_name,
      xmap_header.workspace_id,
      xmap_users.project_name
    FROM madi_results.xmap_header
    JOIN madi_results.xmap_users ON xmap_header.workspace_id = xmap_users.workspace_id
    WHERE xmap_header.workspace_id = {userWorkSpaceID()}
  ;", .con = conn)

  query_result <- dbGetQuery(conn, select_query)
  reactive_df_study_exp(query_result)
  print("reactive_df_study_exp:loaded")

  output$main_tab_selector <- renderUI({
    tagList(
      div(
      style = paste0(
        "background-color: #ffffff; padding: 15px; border-radius: 8px; ",
        "margin: 0 auto 20px auto; border: 2px solid #2c3e50; ",
        "width: 90%; max-width: 400px; box-sizing: border-box;"
      ),#paste0("background-color: #ffffff ; padding: 15px; border-radius: 8px; margin-bottom: 20px; border: 2px solid #2c3e50;"),
      div(
        style = "display: flex; align-items: center; margin-bottom: 8px;",
        sidebarUserPanel(
          selectInput("dataset", "Choose a dataset:",
                      choices = c("rock", "pressure", "cars")),
          radioButtons("main_tabs_rb", NULL,
                     choiceNames = list(
                       div(
                         style = "display: flex; align-items: center; padding: 8px; background-color: #ffffff;",
                         tags$i(class = "fa fa-eye", style = "color: #e74c3c; margin-right: 10px; font-size: 1.1em;"),
                         span("View, Process, and Export Data", style = "color: #2c3e50; font-weight: 600;")
                       ),
                       div(
                         style = "display: flex; align-items: center; padding: 8px; background-color: #ffffff;",
                         tags$i(class = "fa fa-cloud-upload", style = "color: #e74c3c; margin-right: 10px; font-size: 1.1em;"),
                         span("Import Plate Data", style = "color: #2c3e50; font-weight: 600;")
                       ),
                       div(
                         style = "display: flex; align-items: center; padding: 8px; background-color: #ffffff;",
                         tags$i(class = "fa fa-users", style = "color: #e74c3c; margin-right: 10px; font-size: 1.1em;"),
                         span("Create, Add, and Load Projects", style = "color: #2c3e50; font-weight: 600;")
                       )
                     ),
                     choiceValues = list("view_files_tab", "import_tab", "manage_project_tab"),
                     selected = "view_files_tab"
        )
      ))
      )
    )
  })
  output$userpanel <- renderUI({
    #req(input$main_tabs)
    tagList(
      div(style = "padding: 15px;"),
      div(
        style = paste0(
          "background-color: #666666; padding: 15px; border-radius: 8px; ",
          "margin: 0 auto 20px auto; border: 2px solid #2c3e50; ",
          "width: 90%; max-width: 400px; box-sizing: border-box;"),
     # sidebarUserPanel(
      span(current_login, currentuser(), style = "color: white"),
        # div(
        #   span(current_login, style = "display: block;"),
        #   span(currentuser(), style = "display: block;")
        # ),
        actionButton(
          inputId = "logout_button", # Must match the observeEvent input
          label = "Logout",
          icon = icon("sign-out-alt"), # Optional: FontAwesome icon
          class = "btn-danger",        # Optional: Styling class
          width = "80%"                # Optional: Styling
        )
      #)
      )

      # div(style = "padding: 15px; color: #FFFFFF;",
      #     # Project Info
      #     h4("Current Project",
      #        style = "margin-top: 0; color: #00FF00; font-weight: bold;"),
      #     p(
      #       strong("Project: ", style = "color: #90EE90;"),
      #       span(userProjectName(), style = "color: #FFFFFF;"), br(),
      #       strong("Project ID: ", style = "color: #90EE90;"),
      #       span(userWorkSpaceID(), style = "color: #FFFFFF;")
      #     ),
      #     # Session Info
      #     hr(style = "margin: 10px 0; border-color: #90EE90;"),
      #     # div(
      #     #   style = paste0(
      #     #     "background-color: #E1F5F9; padding: 15px; border-radius: 8px; ",
      #     #     "margin: 0 auto 20px auto; border: 2px solid #2c3e50; ",
      #     #     "width: 90%;  box-sizing: border-box;"
      #     #   ),
      #     #
      #     #   # Header
      #     #   h4("Current Session",
      #     #      style = "color: black; font-weight: bold; margin-bottom: 10px;"
      #     #   ),
      #     #
      #     #   # Dynamic section based on current tab
      #     #   p(
      #     #     strong(
      #     #       if (input$main_tabs == "import_tab") "Importing:"
      #     #       else if (input$main_tabs == "view_files_tab") "Viewing Files:"
      #     #       else if (input$main_tabs == "manage_project_tab") "Managing Project"
      #     #       else "",
      #     #       style = "color: black;"
      #     #     )
      #     #   ),
      #     #
      #     #   # Show study/experiment details only for import and view_files tabs
      #     #   if (input$main_tabs %in% c("import_tab", "view_files_tab")) {
      #     #     div(
      #     #       style = "padding-left: 10px;",
      #     #
      #     #       # Study
      #     #       p(
      #     #         strong("Study: ", style = "color: black;"),
      #     #         if (!is.null(
      #     #           if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #     #           else input$readxMap_study_accession
      #     #         ) &&
      #     #         (if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #     #          else input$readxMap_study_accession) != "Click here") {
      #     #           span(
      #     #             if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #     #             else input$readxMap_study_accession,
      #     #             style = "color: black;"
      #     #           )
      #     #         } else {
      #     #           em("None selected", style = "color: black;")
      #     #         }
      #     #       ),
      #     #
      #     #       # Experiment
      #     #       p(
      #     #         strong("Experiment: ", style = "color: black;"),
      #     #         if (!is.null(
      #     #           if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #     #           else input$readxMap_experiment_accession
      #     #         ) &&
      #     #         (if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #     #          else input$readxMap_experiment_accession) != "Click here") {
      #     #           span(
      #     #             if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #     #             else input$readxMap_experiment_accession,
      #     #             style = "color: black;"
      #     #           )
      #     #         } else {
      #     #           em("None selected", style = "color: black;")
      #     #         }
      #     #       )
      #     #     )
      #     #   }
      #     # )
      #
      #     h4("Current Session",
      #        style = "color: #00FF00; font-weight: bold;"),
      #     # Dynamic Section based on current tab
      #     p(
      #       strong(
      #         if (input$main_tabs == "import_tab") "Importing:"
      #         else if (input$main_tabs == "view_files_tab") "Viewing Files:"
      #         else if (input$main_tabs == "manage_project_tab") "Managing Project"
      #         else "",
      #         style = "color: #90EE90;"
      #       )
      #     ),
      #    # Show study/experiment details only for import and view_files tabs
      #     if (input$main_tabs %in% c("import_tab", "view_files_tab")) {
      #       p(
      #         style = "padding-left: 10px;",
      #         strong("Study: ", style = "color: #90EE90;"),
      #         if (!is.null(
      #           if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #           else input$readxMap_study_accession
      #         ) &&
      #         (if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #          else input$readxMap_study_accession) != "Click here") {
      #           span(
      #             if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #             else input$readxMap_study_accession,
      #             style = "color: #FFFFFF;"
      #           )
      #         } else {
      #           em("None selected", style = "color: #98FB98;")
      #         }, br(),
      #         strong("Experiment: ", style = "color: #90EE90;"),
      #         if (!is.null(
      #           if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #           else input$readxMap_experiment_accession
      #         ) &&
      #         (if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #          else input$readxMap_experiment_accession) != "Click here") {
      #           span(
      #             if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #             else input$readxMap_experiment_accession,
      #             style = "color: #FFFFFF;"
      #           )
      #         } else {
      #           em("None selected", style = "color: #98FB98;")
      #         }, #br()
      #        #  strong("Reference Arm: ", style = "color: #90EE90;"),
      #        #  reference_arm(),
      #        #  br(),
      #        # strong("Blank Control: ", style = "color: #90EE90;"),
      #        # if(background_control_rv() == "ignored") {
      #        #   "Ignored"
      #        # } else if (background_control_rv() == "included"){
      #        #   "Included"
      #        # } else if (background_control_rv() == "subtracted") {
      #        #   "Subtracted 1 x Geometric mean"
      #        # } else if (background_control_rv() == "subtracted_3x") {
      #        #   "Subtract 3 x Geometric Mean"
      #        # }  else if (background_control_rv() == "subtracted_10x") {
      #        #   "Subtracted 10 x Geometric Mean"
      #        # }, br(),
      #        # strong("Failed Well Criteria: ", style = "color: #90EE90;"),
      #        # if (failed_well_criteria() == "lower") {
      #        #   "Below Lower Threshold"
      #        # } else if (failed_well_criteria() == "upper") {
      #        #   "Below Upper Threshold"
      #        # }, br(),
      #        # strong("Bead Count Upper Threshold: ", style = "color: #90EE90;"),
      #        # upper_threshold_rv(),
      #        # br(),
      #        # strong("Bead Count Lower Threshold: ", style = "color: #90EE90;"),
      #        # lower_threshold_rv(),
      #        # br()
      #        #strong("Treatment of MFI values in Standard Curves: ",style = "color: #90EE90;"),
      #          # if (aggrigate_mfi_dilution()) {
      #          #   "Mean MFI at each Dilution Factor"
      #          # } else {
      #          #   "Raw MFI at each Dilution Factor"
      #          # }
      #
      #
      #
      #
      #       )
      #     }
      # ),


      # div(
      #   style = paste0(
      #     "background-color: #ffffff; padding: 15px; border-radius: 8px; ",
      #     "margin: 0 auto 20px auto; border: 2px solid #2c3e50; ",
      #     "width: 90%; max-width: 400px; box-sizing: border-box;"),
      #         h4("Current Project",
      #            style = "margin-top: 0; color: black; font-weight: bold;"),
      #         p(
      #           strong("Project: ", style = "color: black;"),
      #           span(userProjectName(), style = "color: black;"), br(),
      #           strong("Project ID: ", style = "color: black;"),
      #           span(userWorkSpaceID(), style = "color: black;")
      #         )
      # )
      # div(
      #   style = paste0(
      #     "background-color: #ffffff; padding: 15px; border-radius: 8px; ",
      #     "margin: 0 auto 20px auto; border: 2px solid #2c3e50; ",
      #     "width: 90%; max-width: 400px; box-sizing: border-box;"
      #   ),#paste0("background-color: #ffffff ; padding: 15px; border-radius: 8px; margin-bottom: 20px; border: 2px solid #2c3e50;"),
      #   h4("Current Session",
      #      style = "color: black; font-weight: bold;"
      #   ),
      #
      #   p(
      #     strong(
      #       if (input$main_tabs == "import_tab") "Importing:"
      #       else if (input$main_tabs == "view_files_tab") "Viewing Files:"
      #       else if (input$main_tabs == "manage_project_tab") "Managing Project"
      #       else "",
      #       style = "color: black;"
      #     )
      #   ),
      #
      #   # Only show study/experiment details for import/view tabs
      #   if (input$main_tabs %in% c("import_tab", "view_files_tab")) {
      #     p(
      #       style = "padding-left: 10px;",
      #       strong("Study: ", style = "color: black;"),
      #       if (!is.null(
      #         if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #         else input$readxMap_study_accession
      #       ) &&
      #       (if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #        else input$readxMap_study_accession) != "Click here") {
      #         span(
      #           if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
      #           else input$readxMap_study_accession,
      #           style = "color: black;"
      #         )
      #       } else {
      #         em("None selected", style = "color: black;")
      #       },
      #       br(),
      #       strong("Experiment: ", style = "color: black;"),
      #       if (!is.null(
      #         if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #         else input$readxMap_experiment_accession
      #       ) &&
      #       (if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #        else input$readxMap_experiment_accession) != "Click here") {
      #         span(
      #           if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
      #           else input$readxMap_experiment_accession,
      #           style = "color: black;"
      #         )
      #       } else {
      #         em("None selected", style = "color: black;")
      #       }
      #     )
      #   }
      #   )

    #   div(style = "padding: 15px; color: #FFFFFF;",
    #       hr(style = "margin: 10px 0; border-color: #90EE90;"),
    #       conditionalPanel(
    #         condition = "input.readxMap_study_accession != 'Click here' && input.readxMap_experiment_accession != 'Click here' && input.study_level_tabs == 'Experiments' && input.main_tabs == 'view_files_tab'",
    #
    #       selectInput(
    #         inputId = "qc_component",
    #         label = "QC Phase",
    #         choices = c("Data", "Bead Count", "Standard Curve", "Dilution Analysis", "Plate Normalization", "Outliers", "Subgroup Detection"),
    #         selected = "Data",
    #         multiple = FALSE
    #       )))
     )
  })

  # Sync radio buttons with tabsetPanel
  # observeEvent(input$main_tabs_panel, {
  #   updateTabsetPanel(session, "main_tabs", selected = input$main_tabs_panel)
  # })

  # observeEvent(input$main_tabs_panel, {
  #   # Only update if selected tab is different
  #   if (input$main_tabs != input$main_tabs_panel) {
  #     updateTabsetPanel(session, "main_tabs", selected = input$main_tabs_panel)
  #   }
  # })


  # observeEvent(input$main_tabs, {
  #   cat(paste("Switched to",input$main_tabs))
  #
  #   conditionalPanel(
  #     "input"
  #   )
  #   if (input$main_tabs == "view_files_tab") {
  #     mainPanel(
  #       uiOutput("view_stored_experiments_ui")
  #     )
  #   } else if (input$main_tabs == "import_tab") {
  #     mainPanel(
  #      uiOutput("readxMapData")
  #     )
  #   } else {
  #     mainPanel(
  #       uiOutput("manage_project_ui")
  #     )
  #   }
  #
  #
  # })

  output$primary_body_panel <- renderUI({
    # req(input$main_tabs)
    # if (input$main_tabs == "view_files_tab") {
    #   # insertTab(
    #   #   inputId = "body_panel_id",
    #   #   tabPanel(
    #   #     value = "userData",
    #   #     title = "Connection User Data",
    #   #
    #     )
    #   )
    # }
  })


  output$primarysidepanel <- renderUI({
    # If you need any additional sidebar content
    # Otherwise, you can remove this completely
  })

  insertTab(
    inputId = "body_panel_id",
    tabPanel(
      value = "userData",
      title = "Connection User Data",
      shinydashboard::box(
        title = "For users needing help logging in",
        width = 14,
        status = "primary",
        solidHeader = TRUE,
        h3("Connection Data"),
        verbatimTextOutput("auth0_user_info")
      )
    )
  )
})
