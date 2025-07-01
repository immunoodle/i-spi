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

  output$userpanel <- renderUI({
    tagList(
      sidebarUserPanel(
      span(current_login, currentuser()),
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
      ),
      div(style = "padding: 15px; color: #FFFFFF;",
          # Project Info
          h4("Current Project",
             style = "margin-top: 0; color: #00FF00; font-weight: bold;"),
          p(
            strong("Project: ", style = "color: #90EE90;"),
            span(userProjectName(), style = "color: #FFFFFF;"), br(),
            strong("Project ID: ", style = "color: #90EE90;"),
            span(userWorkSpaceID(), style = "color: #FFFFFF;")
          ),
          # Session Info
          hr(style = "margin: 10px 0; border-color: #90EE90;"),
          h4("Current Session",
             style = "color: #00FF00; font-weight: bold;"),
          # Dynamic Section based on current tab
          p(
            strong(
              if (input$main_tabs == "import_tab") "Importing:"
              else if (input$main_tabs == "view_files_tab") "Viewing Files:"
              else if (input$main_tabs == "manage_project_tab") "Managing Project"
              else "",
              style = "color: #90EE90;"
            )
          ),
          # Show study/experiment details only for import and view_files tabs
          if (input$main_tabs %in% c("import_tab", "view_files_tab")) {
            p(
              style = "padding-left: 10px;",
              strong("Study: ", style = "color: #90EE90;"),
              if (!is.null(
                if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
                else input$readxMap_study_accession
              ) &&
              (if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
               else input$readxMap_study_accession) != "Click here") {
                span(
                  if (input$main_tabs == "import_tab") input$readxMap_study_accession_import
                  else input$readxMap_study_accession,
                  style = "color: #FFFFFF;"
                )
              } else {
                em("None selected", style = "color: #98FB98;")
              }, br(),
              strong("Experiment: ", style = "color: #90EE90;"),
              if (!is.null(
                if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
                else input$readxMap_experiment_accession
              ) &&
              (if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
               else input$readxMap_experiment_accession) != "Click here") {
                span(
                  if (input$main_tabs == "import_tab") input$readxMap_experiment_accession_import
                  else input$readxMap_experiment_accession,
                  style = "color: #FFFFFF;"
                )
              } else {
                em("None selected", style = "color: #98FB98;")
              }, br(),
              strong("Reference Arm: ", style = "color: #90EE90;"),
              reference_arm(),
              br(),
             strong("Blank Control: ", style = "color: #90EE90;"),
             if(background_control_rv() == "ignored") {
               "Ignored"
             } else if (background_control_rv() == "included"){
               "Included"
             } else if (background_control_rv() == "subtracted") {
               "Subtracted 1 x Geometric mean"
             } else if (background_control_rv() == "subtracted_3x") {
               "Subtract 3 x Geometric Mean"
             }  else if (background_control_rv() == "subtracted_10x") {
               "Subtracted 10 x Geometric Mean"
             }, br(),
             strong("Failed Well Criteria: ", style = "color: #90EE90;"),
             if (failed_well_criteria() == "lower") {
               "Below Lower Threshold"
             } else if (failed_well_criteria() == "upper") {
               "Below Upper Threshold"
             }, br(),
             strong("Bead Count Upper Threshold: ", style = "color: #90EE90;"),
             upper_threshold_rv(),
             br(),
             strong("Bead Count Lower Threshold: ", style = "color: #90EE90;"),
             lower_threshold_rv(),
             br()
             #strong("Treatment of MFI values in Standard Curves: ",style = "color: #90EE90;"),
               # if (aggrigate_mfi_dilution()) {
               #   "Mean MFI at each Dilution Factor"
               # } else {
               #   "Raw MFI at each Dilution Factor"
               # }




            )
          }
      )
    )
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
