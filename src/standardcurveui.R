standard_curve_table_list <- list()

data_summary <- list()

std_curve_tab_active <- reactiveVal(FALSE)

standardCurveFittingModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
    .standard-curve_model-fit-collapse-container {
      width: 75vw;
      overflow-x: auto;
    }
    .gated-samples-collapse-container {
      width: 75vw;
      overflow-x: auto;
    }
  "))
    ),
    fluidRow(
      column(12,
             bsCollapse(
               id = ns("methodsCollapse"),
               bsCollapsePanel(
                 title = "Standard Curve Fitting Methods",
                 tagList(
                   tags$p("Use the dropdown menus titled 'Plate' and 'Antigen' to select the plate and antigen on the plate of interest. If there are more than one source, select the desired source from the radio buttons."),
                   tags$p("
We have created an unsupervised algorithm to fit a standard curve to the dilution series of the antigen on the selected plate in the selected study with the Median Fluorescence Index (MFI) as the outcome. The unsupervised algorithm attempts to fit a cascade of models to find the best fit. It stops on a model if the model converges and attempts the next model in the cascade if not. This cascade begins with a 5 parameter logistic regression with the Nonlinear Least Squares (nls) method. (Gottschalk and Dunn, 2004).
This is followed by a 5 parameter logistic regression model from the Dose-response Data Analysis (drda) library (Malyutina,Tang and Pessi). If the previous two models fail to find a fit, we attempt a 4 parameter logistic regression with the nonlinear least squares method following the method developed by (Rajam et. al. 2019).
If a 4 parameter model fails we turn to a nls exponential model before fitting no model to the dilution series (Watson, 2018).
"),

                   tags$p("The figure depicts the raw dilution series, the fitted curve, the lower and upper limit of detection (if estimated), bend lines and the predicted dilution. To view the predicted dilution, click 'Predicted Dilution' in the legend.
The information icon below the figure provides definitions for terminology and acronyms used in the figure including the following:"),
                   tags$ul(tags$li("LOD: Limit of Detection"),
                           tags$li("AU: Arbitrary Unit"),
                           tags$li("Bend Lines: The region between the bend lines provide a line segment of the standard curve which is linear."))
                   ,
                   tags$p("The first table below the figure provides the table of coefficients of the fitted model and includes the terms’ estimate, standard error, and p value. The table below the coefficient table provides model fit statistics including the bendlines, limits of detection, model parameter estimates, an indicator of what type of model is fit (crit), and the following model fit statistics:"),
                   tags$ul(tags$li("iter: number of iterations to convergence"),
                           tags$li("status: convergence status. Either converged or not converged"),
                           tags$li("loglik: the log likelihood"),
                           tags$li("dfresidual: residual degrees of freedom"),
                           tags$li("nobs: Number of observations fitted from the dilution series"),
                           tags$li("rsquare_fit: The model’s R squared value"),
                           tags$li("mse: mean squared error"),
                           tags$li("source: the source"),
                           tags$li("cv: Coefficient of Variation (%)")
                   ), # end ul
                   tags$p("Below the model fit statistics there are two tables that list sample values above the upper limit of detection and below the lower limit of detection.
"),
                   tags$p("At the bottom of the screen, if a model exists, there are three ways to save the results before later retrieval. The buttons are labeled as follows:"),
                   tags$ol(tags$li(HTML("<b>Save Model Fit: </b> When clicked the model fit and the predicted dilution fraction are saved to the database for the current model for the antigen on the plate that is currently in view.
")),
                           tags$li(HTML("<b>Save Models for All Antigens on Current Plate: </b> When clicked all the antigens on the selected plate are fit to standard curves and model fits and predicted dilution fractions are saved to the database.
")),
                           tags$li(HTML("<b>Save Models for all Antigens on all Plates in Current Experiment:</b>  When clicked all the plates and all the antigens on those plates are fit to standard curves and model fits and predicted dilution fractions are saved to the database.
"))), #end ordered list
                   tags$h3("References"),
                   tags$p(" Alina Malyutina, Jing Tang, Alberto Pessia (2023). drda: An R Package for Dose-Response Data
  Analysis Using Logistic Functions. Journal of Statistical Software, 106(4), 1-26.
  doi:10.18637/jss.v106.i04"),
                   tags$p("Gottschalk PG and Dunn JR II, Fitting Brenden’s five-parameter logistic curve, Bio-Rad bulletin 3022 (2004)."),
                   tags$p("Rajam, G., Carlone, G., Kim, E., Choi, J., Paulos, S., Park, S., Jeyachandran, A., Gorantla, Y., Wong, E., Sabnis, A., Browning, P., Desai, R., Quinn, C. P., & Schiffer, J. (2019). Development and validation of a robust multiplex serological assay to quantify antibodies specific to pertussis antigens. Biologicals : journal of the International Association of Biological Standardization, 57, 9–20. https://doi.org/10.1016/j.biologicals.2018.11.001
"),
                   tags$p("Watson, D. (n.d.). Fitting exponential decays in R, the easy way. Retrieved January 6, 2025, from https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/")

                 ),# end tagList,
                 style = "success"
               )
             ),
             # fluidRow(
             #   uiOutput("toggle_aggrigate_mfi_dilution"),
             #   textOutput("status")
             # ),
             fluidRow(
               # uiOutput("studySelectionUI", style = "margin: 0;"),
               # uiOutput("expirementSelectionUI",style = "margin: 0;"),
               column(4,uiOutput(ns("plateSelectionUI"))),
               column(4,uiOutput(ns("antigenSelectionUI"))),
               column(4,uiOutput(ns("sourceSelectionUI")))
             ) ,
             mainPanel(

               # conditional logic in rendering
               uiOutput(ns("dilution_message")),
               uiOutput(ns("dilution_plot_ui")),
               conditionalPanel(condition = "!is.null(mod)",
                                plotlyOutput(ns("standardFitUI"), width = "75vw")),
              # plotlyOutput(ns("standardFitUI"), width = "75vw"),

               #uiOutput(ns("standardFitUIContainer")),

               div(span(
                 div(style = "display:inline-block;",
                     title = "Info",
                     icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                          `data-placement` = "right", title = "LOD: Limit of Detection<br>AU: Arbitrary Unit<br>Bend Lines:
                                                        The region between the bend lines provide a line segment of the standard curve which is linear",
                          `data-html` = "true")
                 )
               ),
               br()
               ),
               # standard_plot data download
               downloadButton(ns("download_standard_plot_data"),"Download Current Standard Curve Data"),
               br(),
               downloadButton(ns("download_sample_plot_data"), "Download Current Sample Data"),
               br(),
               # Save Model Fits
               uiOutput(ns("saveButtonsUI")),
               br(),
              div(class = "table-container", tableOutput(ns("parameterFit"))),
              div(class = "table-container", tableOutput(ns("modelFit"))),
              # div(class = "standard-curve_model-fit-collapse-container",
              #  bsCollapse(
              #    id = ns("standard_curve_model_fit"),
              #    bsCollapsePanel(
              #      title = "Parameter Estimates and Summary Statistics",
              #      div(class = "table-container", tableOutput(ns("parameterFit"))),
              #      div(class = "table-container", tableOutput(ns("modelFit"))),
              #      style = "primary"
              #    )
              #  )
              #  ),
              div(class = "table-container",tableOutput(ns("above_ulod_table"))),
              div(class = "table-container",tableOutput(ns("below_limit_table")))
              #div(class = "gated-samples-collapse-container",
               # bsCollapse(
               #   id = ns("gated_samples"),
               #   bsCollapsePanel(
               #     title = "Gated Samples",
               #     div(class = "table-container",tableOutput(ns("above_ulod_table"))),
               #     div(class = "table-container",tableOutput(ns("below_limit_table"))),
               #     style = "primary"
               #   )
               # )
              #)
             )
      )
    )
  ) # end tagList
}

standardCurveFittingServer <- function(id, selected_study, selected_experiment, currentuser) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    std_curve_data_model_fit <- reactiveVal()
    sample_data_model_fit <- reactiveVal()
    buffer_data_model_fit <- reactiveVal()

    sample_data_sc <- fetch_db_samples(study_accession = selected_study(), experiment_accession = selected_experiment())
    standard_data_curve_sc <- fetch_db_standards(study_accession = selected_study(), experiment_accession = selected_experiment())
    buffer_data_sc <- fetch_db_buffer(study_accession = selected_study(), experiment_accession = selected_experiment())

    if (!is.null(selected_study()) && length(selected_study()) > 0 &&
        !is.null(selected_experiment()) && length(selected_experiment()) > 0 &&
        !is.null(sample_data_sc) && length(sample_data_sc > 0)){

      # Filter sample data
      sample_data_sc$selected_str <- paste0(sample_data_sc$study_accession, sample_data_sc$experiment_accession)
      sample_data_sc <- sample_data_sc[sample_data_sc$selected_str == paste0(selected_study(), selected_experiment()), ]

      # Summarize sample data
      cat("Viewing sample data fitting module")
      print(names(sample_data_sc))
      print(table(sample_data_sc$plateid))
      print(table(sample_data_sc$antigen))
      cat("After summarizing sample data fitting module")


      # Rename columns

      sample_data_sc <- dplyr::rename(sample_data_sc, arm_name = agroup)
      sample_data_sc <- dplyr::rename(sample_data_sc, visit_name = timeperiod)


      sample_data_sc$subject_accession <- sample_data_sc$patientid

      sample_data_sc <- dplyr::rename(sample_data_sc, value_reported = antibody_mfi)

      arm_choices <- unique(sample_data_sc$arm_name)
      visits <- unique(sample_data_sc$visit_name)

      sample_data_model_fit(sample_data_sc)
    }


    if (!is.null(selected_study()) && length(selected_study()) > 0 &&
        !is.null(selected_experiment()) && length(selected_experiment()) > 0 &&
        !is.null(standard_data_curve_sc) && length(standard_data_curve_sc) > 0){

      # Filter sample data
      standard_data_curve_sc$selected_str <- paste0(standard_data_curve_sc$study_accession, standard_data_curve_sc$experiment_accession)
      standard_data_curve_sc <- standard_data_curve_sc[standard_data_curve_sc$selected_str == paste0(selected_study(), selected_experiment()), ]

      # Summarize std curve data data
      cat("View Standard Curve data plateid")
      print(table(standard_data_curve_sc$plateid))
      cat("View Standard Curve data antigen")
      print(table(standard_data_curve_sc$antigen))

      std_curve_data_sc <- standard_data_curve_sc


      # Rename columns

      # data <- dplyr::rename(data, arm_name = agroup)
      #data <- dplyr::rename(data, visit_name = timeperiod)


      std_curve_data_sc$subject_accession <- std_curve_data_sc$patientid

      std_curve_data_sc <- calculate_log_dilution(std_curve_data_sc)
      cat("Standard Curve data after calculating log dilutions")
      print(names(std_curve_data_sc))
      std_curve_data_model_fit(std_curve_data_sc)

    }

    # Load Buffer Data
    if (!is.null(selected_study()) && length(selected_study()) >0 &&
        !is.null(selected_experiment()) && length(selected_experiment()) > 0 &&
        !is.null(buffer_data_sc) && length(buffer_data_sc) > 0) {

      buffer_data_sc$selected_str <- paste0(buffer_data_sc$study_accession, buffer_data_sc$experiment_accession)
      buffer_data_sc <- buffer_data_sc[buffer_data_sc$selected_str == paste0(selected_study(), selected_experiment()), ]
      # buffer_data_view <<- buffer_data
      cat("View buffer data names")
      print(names(buffer_data_sc))
      buffer_data_model_fit(buffer_data_sc)
    }

    ## Load study configuration for the user
    study_configuration <- fetch_study_configuration(study_accession = selected_study() , user = currentuser())
    # Determine if standard curve data is available for study and experiment
    is_standard_available <- checkStandardCurves(std_curve_data_sc, selected_study(), selected_experiment())
    cat("Standard Curve avaliablilty")
    print(is_standard_available)

    if (is_standard_available) {

      output$plateSelectionUI <- renderUI({
        #req(input$readxMap_study_accession, input$readxMap_experiment_accession)
        req(std_curve_data_sc$study_accession, std_curve_data_sc$experiment_accession)
        updateSelectInput(session, ns("plateSelection"), selected = NULL)  # Reset the plateSelection
        sc_plate_data <- std_curve_data_sc[std_curve_data_sc$study_accession %in% selected_study() &
                                          std_curve_data_sc$experiment_accession %in% selected_experiment(), ]

        req(nrow(sc_plate_data) > 0)

        selectInput(ns("plateSelection"),
                    label = "Plate",
                    choices = unique(sc_plate_data$plateid))
      })

      output$antigenSelectionUI <- renderUI({
     #   req(input$readxMap_study_accession, input$readxMap_experiment_accession)
        req(std_curve_data_sc$study_accession, std_curve_data_sc$experiment_accession)
        updateSelectInput(session, ns("antigenSelection"), selected = NULL)

        dat_antigen <- std_curve_data_sc[std_curve_data_sc$study_accession %in% selected_study() &
                                           std_curve_data_sc$experiment_accession %in% selected_experiment() &
                                           std_curve_data_sc$plateid %in% input$plateSelection, ]

        dat_antigen <- dat_antigen[!is.na(dat_antigen$mfi),]
        req(nrow(dat_antigen) > 0)


        selectInput(ns("antigenSelection"),
                    label = "Antigen",
                    choices = unique(dat_antigen$antigen)) #unique(dat_antigen$antigen)
      })


    source_rv <- reactiveVal(NULL)
    # output$sourceSelectionUI <- renderUI({
    #   req(std_curve_data_sc$study_accession, std_curve_data_sc$experiment_accession)
    #  # req(input$readxMap_study_accession, input$readxMap_experiment_accession,
    #   req(input$plateSelection)
    #   req(study_configuration)
    #   # initial_source <- study_configuration[study_configuration$param_name == "default_source",]$param_character_value
    #   # cat("\nInitial Source:\n")
    #   # print(initial_source)
    #
    #
    #
    #   # if (is.null(initial_source) || is.na(initial_source)) {
    #   #   initial_source <- obtain_initial_source(selected_study())
    #   # } else {
    #   #   initial_source <- initial_source
    #   # }
    #
    #
    #   # if (is.null(initial_source) || is.na(initial_source)) {
    #   #    # <- obtain_initial_source(selected_study())
    #   #   updateRadioButtons(session, "sourceSelection", selected = NULL)
    #   # } else {
    #   #   updateRadioButtons(session, "sourceSelection", selected = initial_source)
    #   # }
    #
    #   #  updateRadioButtons(session, "sourceSelection", selected = NULL)
    #
    #   dat_source <- std_curve_data_sc[std_curve_data_sc$study_accession %in% selected_study() &
    #                                     std_curve_data_sc$experiment_accession %in% selected_experiment() &
    #                                     std_curve_data_sc$plateid %in% input$plateSelection &
    #                                     std_curve_data_sc$antigen %in% input$antigenSelection, ]
    #   req(nrow(dat_source) > 0)
    #
    #   #if (!(initial_source %in% unique(dat_source$source))) {
    #     initial_source <- unique(dat_source$source)[1]
    # #  }
    #
    #   source_rv(initial_source)
    #
    #   radioButtons("sourceSelection",
    #                label = "Source",
    #                choices = unique(dat_source$source),
    #                selected = initial_source
    #               )
    #
    # })
    output$sourceSelectionUI <- renderUI({
      req(std_curve_data_sc, input$plateSelection, input$antigenSelection)

      dat_source <- std_curve_data_sc[
        std_curve_data_sc$study_accession %in% selected_study() &
          std_curve_data_sc$experiment_accession %in% selected_experiment() &
          std_curve_data_sc$plateid %in% input$plateSelection &
          std_curve_data_sc$antigen %in% input$antigenSelection, ]

      req(nrow(dat_source) > 0)

      radioButtons(
        ns("sourceSelection"),
        label = "Source",
        choices = unique(dat_source$source),
        selected = unique(dat_source$source)[1]
      )
    })




    # This was for souurce in parameters
    # observeEvent(input$sourceSelection, {
    #   if (is.null(input$sourceSelection)) {
    #     req(study_configuration)
    #     initial_source <- study_configuration[study_configuration$param_name == "default_source",]$param_character_value
    #     cat("\nInitial Source:\n")
    #     print(initial_source)
    #
    #     if (is.null(initial_source) || is.na(initial_source)) {
    #       initial_source <- obtain_initial_source(selected_study())
    #     } else {
    #       initial_source <- initial_source
    #     }
    #     source_rv(initial_source)
    #   } else {
    #     source_rv(input$sourceSelection)
    #   }
    # })
    ### end above comment

      # output$sourceSelectionUI <- renderUI({
      #   req(std_curve_data_sc$study_accession, std_curve_data_sc$experiment_accession)
      #   req(input$plateSelection)
      #   req(study_configuration)
      #
      #   updateSelectInput(session, ns("sourceSelection"), selected = NULL)
      #   dat_source <- std_curve_data_sc[
      #     std_curve_data_sc$study_accession %in% selected_study() &
      #       std_curve_data_sc$experiment_accession %in% selected_experiment() &
      #       std_curve_data_sc$plateid %in% input$plateSelection &
      #       std_curve_data_sc$antigen %in% input$antigenSelection, ]
      #   req(nrow(dat_source) > 0)
      #
      #   selectInput("sourceSelection",
      #                label = "Source",
      #                choices = unique(dat_source$source))
      # })
      #
      #
      # # observe({
      # #   req(study_configuration)
      # #   initial_source <- study_configuration[study_configuration$param_name == "default_source", ]$param_character_value
      # #
      # #   # fallback: try to obtain from DB if not in config
      # #   if (is.null(initial_source) || is.na(initial_source)) {
      # #     initial_source <- obtain_initial_source(selected_study())
      # #   }
      # #
      # #   # only update if the input exists
      # #   if ("sourceSelection" %in% names(input)) {
      # #     updateRadioButtons(session, "sourceSelection", selected = initial_source)
      # #   }
      # # })


      # Main Panel Output
      filtered_data_rv <- reactive({
        req(std_curve_data_sc)
        req(study_configuration)
        req(input$sourceSelection)

        bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
        apply_prozone <- as.logical(toupper(study_configuration[study_configuration$param_name == "applyProzone",]$param_boolean_value))
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
        aggrigate_mfi_dilution <- as.logical(toupper(study_configuration[study_configuration$param_name == "mean_mfi",]$param_boolean_value))

        # initial_source <- study_configuration[study_configuration$param_name == "default_source",]$param_character_value
        # if (is.null(initial_source) || is.na(initial_source)) {
        #   initial_source <- obtain_initial_source(selected_study())
        # } else {
        #   initial_source <- initial_source
        # }
        #  req(background_control_rv())

        cat("before antigen val filtered data")
        antigen_val <-  if (is.null(selected_antigen())) {
          input$antigenSelection
        } else{
          selected_antigen()
        }
        cat("after antigen_val_ filtered data")
        print(antigen_val)


        plate_val <- if(is.null(selected_plate())) {
          input$plateSelection
        } else {
          selected_plate()
        }

        cat("before filtering")
        print(head(std_curve_data_sc))
        print(plate_val)
        print(antigen_val)
        print(selected_experiment())
        print(input$sourceSelection)
       # updateRadioButtons(session = session, "sourceSelection", initial_source)
        #std_curve_data_s <<- std_curve_data_sc
        filtered_data <- std_curve_data_sc[std_curve_data_sc$source == input$sourceSelection & std_curve_data_sc$antigen == antigen_val
                                        & std_curve_data_sc$experiment_accession == selected_experiment() &
                                          std_curve_data_sc$plateid == plate_val,]

        cat("after filtering")
        print(head(filtered_data))

        req(nrow(filtered_data) > 0)

        # Apply prozone correction if selected
        if (apply_prozone) {
          filtered_data <- correct_prozone(filtered_data, prop_diff = 0.1, dil_scale = 2)
        }
        ## Take log MFI if selected in configuration
        if (is_log_mfi) {
          filtered_data$mfi <- log10(filtered_data$mfi)
          buffer_data_sc$mfi <- log10(buffer_data_sc$mfi)

        }

        # Take average of MFI at each dilution factor if option is selected from dilution options. from db not reactive
        if (aggrigate_mfi_dilution) {
          filtered_data <- aggregate_mfi_dilution_factor(filtered_data)
        }
        # apply buffer/blank methods if selected included
        # bkg_method <- background_control_rv()
        if (bkg_method == "included") {
          filtered_data <- include_blanks(buffer_data_sc, filtered_data, plateid = plate_val, antigen = antigen_val)
        }
        if (bkg_method == "subtracted") {
          filtered_data <- subtract_blanks(buffer_data = buffer_data_sc, data = filtered_data, stype = "standards",
                                           plateid = plate_val, antigen = antigen_val, multiplier = 1)
        } else if (bkg_method == "subtracted_3x") {
          filtered_data <- subtract_blanks(buffer_data = buffer_data_sc, data = filtered_data, stype = "standards",
                                           plateid = plate_val, antigen = antigen_val, multiplier = 3)
        } else if (bkg_method == "subtracted_10x") {
          filtered_data <- subtract_blanks(buffer_data = buffer_data_sc, data = filtered_data, stype = "standards",
                                           plateid = plate_val, antigen = antigen_val, multiplier = 10)
        } else {
          # ignoring buffer
          filtered_data <- filtered_data
        }

        print(head(filtered_data))
        return(filtered_data)

      })

      sample_data_rv <- reactive({
        req(sample_data_sc)
        req(study_configuration)
        bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))

        # req(background_control_rv())

        antigen_val <-  if (is.null(selected_antigen())) {
          input$antigenSelection
        } else{
          selected_antigen()
        }

        plate_val <- if(is.null(selected_plate())) {
          input$plateSelection
        } else {
          selected_plate()
        }
        # Take log of MFI
        #sample_data_v <<- sample_data
        if (is_log_mfi) {
          sample_data_sc$value_reported <- log10(sample_data_sc$value_reported)
          sample_data_sc$antibody_mfi <- sample_data_sc$value_reported
          sample_data_sc$mfi <- sample_data_sc$value_reported
          buffer_data_sc$mfi <- log10(buffer_data_sc$mfi)
        } else {
          if ("value_reported" %in% names(sample_data_sc) & !("antibody_mfi" %in% names(sample_data_sc))) {
            sample_data_sc$antibody_mfi <- sample_data_sc$value_reported
          }
        }

        # bkg_method <- background_control_rv()
        if (bkg_method == "subtracted") {
          sample_data_sc <- subtract_blanks(buffer_data = buffer_data_sc, data = sample_data_sc, stype = "sample",
                                         plateid = plate_val, antigen = antigen_val, multiplier = 1)
        } else if (bkg_method == "subtracted_3x") {
          sample_data_sc <- subtract_blanks(buffer_data = buffer_data_sc, data = sample_data_sc, stype = "sample",
                                         plateid = plate_val, antigen = antigen_val, multiplier = 3)
        } else if (bkg_method == "subtracted_10x") {
          sample_data_sc <- subtract_blanks(buffer_data = buffer_data_sc, data = sample_data_sc, stype = "sample",
                                         plateid = plate_val, antigen = antigen_val, multiplier = 10)
        } else {
          # ignoring buffer
          sample_data_sc
        }

        return(sample_data_sc)
      })

      model <- reactive({
        req(study_configuration)
        cat("In model")
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
        bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
        print(bkg_method)
        #Access the reactive filtered_data standard
        filtered_data_val <- filtered_data_rv()
        #filtered_standard_curve_data <<- filtered_data_val
        # antigen value based on button condition being pressed
        cat("before antigen_val")
        antigen_val <-  if (is.null(selected_antigen())) {
          input$antigenSelection
        } else{
          selected_antigen()
        }
        cat("after antigen val")
        req(antigen_val)

        # plate value based on button condition being pressed
        plate_val <- if (is.null(selected_plate())) {
          input$plateSelection
        } else {
          selected_plate()
        }
        req(plate_val)

        # ensure that there are >= 6 standard curve points
        n_valid_points <- sum(!is.na(filtered_data_val$mfi))
        if (n_valid_points < 6) {
          output$dilution_message <- renderUI({
            HTML(paste("<span style='font-size:20px;'> There are less than 6 points in the dilution series.
                       Cannot fit a Standard Curve  <br></span>"))

          })

          output$dilution_plot_ui <- renderUI({
            sample_data_val <- sample_data_rv()
            req(study_configuration)
            is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))

            # standard curve data
            filtered_data_val <- filtered_data_rv()

            plot_curve_plotly(NULL, filtered_data_val, source_filter = input$sourceSelection,
                              antigen = input$antigenSelection, plate = input$plateSelection,
                              sample_data_au = NULL,
                              is_log_mfi_axis = is_log_mfi)
          })
          # save buttons to not appear
          output$saveButtonsUI <- renderUI({
            NULL
          })
          # return a model of value null
          return(NULL)

        } else {
         # filtered_data_val_view <<- filtered_data_val
          mod <- tryCatch({
            #set seed for reproducibility
            set.seed(11262024)
            compute_robust_curves_5_param(
              dat = filtered_data_val,
              antigen = antigen_val,#input$antigenSelection,
              plate = plate_val,#input$plateSelection,
              study_accession = selected_study(),
              experiment_accession = selected_experiment(),
              source = source_rv(),#input$sourceSelection, was source_rv()
              bkg =  bkg_method,
              is_log_mfi_axis = is_log_mfi,
              g_value = 0.5
            )
          }, error = function(e){
            print(e)
            return(NULL)
          })

          # render message and dilution series if no models were fit
          if (is.null(mod)) {
            print("model null")
            output$dilution_message <- renderUI({
              HTML(paste("<span style='font-size:20px;'> No Standard Curve Avaliable: All Models Failed to Converge <br></span>"))

            })

            output$dilution_plot_ui <- renderUI({
              sample_data_val <- sample_data_rv()
              req(study_configuration)
              is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))


              # standard curve data
              filtered_data_val <- filtered_data_rv()

              plot_curve_plotly(NULL, filtered_data_val, source_filter = input$sourceSelection,
                                antigen = input$antigenSelection, plate = input$plateSelection,
                                sample_data_au = NULL,
                                is_log_mfi_axis = is_log_mfi)
            })
            # save buttons to not appear
            output$saveButtonsUI <- renderUI({
              NULL
            })
            # return a model of value null
            return(mod)
          } else {
            print("model not null")
            #mod <- compute_linear_center(mod, filtered_data_val)
            # do not show message and raw dilution series if there is a model.
            output$dilution_message <- renderUI({
              NULL
            })
            output$dilution_plot_ui <- renderUI({
              NULL
            })
            # show the save buttons
            output$saveButtonsUI <- renderUI({
              div(class = "button-container",
                  actionButton(ns("updateModelFit"), "Save Standard Curves for the Current Experiment"))
            })

            return(mod)
          }
        }
        # } # end else

      })

      sample_data_au <- reactive({
        req(model())
        req(sample_data_rv())
        req(filtered_data_rv())

        cat("Model in sample_data_au")
        print(model())
        cat("Before compute gate class names of sample data")
        print(names(sample_data_rv()))
        print(nrow(sample_data_rv()))

        cat("Model status")
        # if(is.null(model())) {
        #   cat("model is null")
        # }
        # value <- model()
        # print(value)
        # if (is.null(value)) {
        #   cat("value is null")
        # }
        # print(str(model()))
        # if (is.null(model())) {
        #   cat("model is null")
        #   return(NULL)
        #
        # } else {
        # gc_sample_data <- compute_gate_class(model(), sample_data_rv())
        # cat("After compute gate class names of sample data")
        # print(names(gc_sample_data))

        antigen_val <-  if (is.null(selected_antigen())) {
          input$antigenSelection
        } else{
          selected_antigen()
        }

        plate_val <- if(is.null(selected_plate())) {
          input$plateSelection
        } else {
          selected_plate()
        }


        sample_data_static <- sample_data_rv()

        sample_data_filtered <- sample_data_static[sample_data_static$antigen == antigen_val &
                                                     sample_data_static$plateid == plate_val,]
        sample_data_filtered_v <- sample_data_filtered
        # sample_data_val_filtered <- sample_data[sample_data$antigen == antigen &
        #                                           sample_data$plateid == plate,]



        sample_data_au <- backsub_true_dilution_sample(fitted_model = model(), sample_data = sample_data_filtered, dat = filtered_data_rv())[[1]]
        #sample_data_au_v_r <<- sample_data_au
        std_data_au <- backsub_true_dilution_sample(fitted_model = model(), sample_data = sample_data_filtered, dat = filtered_data_rv())[[2]]
        names(std_data_au)[names(std_data_au) == "antibody_au"] <- "predicted_log_dilution"
        # std_data_au_v <<- std_data_au
        gc_sample_data <- compute_gate_class(model(), sample_data_au)
        cat("After compute gate class names of sample data")
        print(names(gc_sample_data))

        # add gating for limits of quantification
        sample_data_au <- compute_loq_gate_class(model_list = model(), sample_data_au = gc_sample_data)

        #sample_data_au_g <<- sample_data_au

        return(sample_data_au)

      })

      # output$standardFitUIContainer <- renderUI({
      #   req(model())  # only show when model() is not NULL
      #   plotlyOutput(ns("standardFitUI"), width = "75vw")
      # })

      # UI for Standard Curve plot
      output$standardFitUI <- renderPlotly({
        req(model())
        req(study_configuration)
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))


        if (is.null(model())) {
          cat("in standard fit UI: model is NULL")
          NULL
        } else {
          req(filtered_data_rv())
          req(sample_data_au())
          cat("in standard fit UI: model present")
          print(nrow(sample_data_au()))
          print(sample_data_au())

          plot_curve_plotly(model(), filtered_data_rv(), source_filter = input$sourceSelection,
                            antigen = input$antigenSelection, plate = input$plateSelection,
                            sample_data_au = sample_data_au(),
                            is_log_mfi_axis = is_log_mfi)
        }
      })

      output$download_standard_plot_data <- downloadHandler(
        filename = function() {
          paste(selected_study(), selected_experiment(), "plot_standard_data.csv", sep = "_")
        },
        content = function(file) {
          # download data component (data frame)
          write.csv(filtered_data_rv(), file)
        }
      )

      output$download_sample_plot_data <-  downloadHandler(
        filename = function() {
          paste(selected_study(), selected_experiment(), "plot_sample_data.csv", sep = "_")
        },
        content = function(file) {
          # download data component (data frame)
          write.csv(sample_data_au(), file)
        }
      )



      # UI for dilution series with no fit
      output$noFitUI <- renderUI({
        mod <- model()
        req(mod)
        req(study_configuration)
        bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))


        sample_data_val <- sample_data_rv()

        # standard curve data
        filtered_data_val <- filtered_data_rv()

        plot_curve_plotly(mod, filtered_data_val, source_filter = input$sourceSelection,
                          antigen = input$antigenSelection, plate = input$plateSelection,
                          sample_data_au = sample_data_au,
                          is_log_mfi_axis = is_log_mfi)
      })

      # Parameter table from model
      output$parameterFit <- renderTable({
        mod <- model()
        req(mod)

        fit_table <- mod[1]
        fit_table <- as.data.frame(fit_table)

        fit_table
      }, caption= "Parameter Estimates",
      caption.placement = getOption("xtable.caption.placement", "top"))

      # Model Fit for saving UI
      output$modelFitUI <- renderUI({
        mod <- model()
        req(mod)

        model_fit_tab <- mod[2]
        model_fit_tab <- as.data.frame(model_fit_tab)

        # display table
        tableOutput("modelFit")
      })

      # Table for model fit
      output$modelFit <- renderTable({
        mod <- model()
        req(mod)
        model_fit_tab <- mod[2]
        model_fit_tab <- as.data.frame(model_fit_tab)
        model_fit_tab
      },caption= "Summary Statistics",
      caption.placement = getOption("xtable.caption.placement", "top"))


      # Sample values above ULOD table
      output$above_ulod_table <- renderTable({
        req(model())
        req(sample_data_au())
        cat("Printing in above ulod table ")
        filtered_sample_data_df <- sample_data_au()
        above_ulod_table <- filtered_sample_data_df[filtered_sample_data_df$gc== "Too Concentrated",]
        names(above_ulod_table)[names(above_ulod_table) == "value_reported"] <- "antibody_mfi"
        above_ulod_table <- above_ulod_table[, c("xmap_sample_id", "subject_accession", "antigen",
                                                 "plateid", "antibody_mfi", "gc", "in_linear_region")]
        above_ulod_table$xmap_sample_id <- as.character(above_ulod_table$xmap_sample_id)

        return(above_ulod_table)

      }, caption="Sample Values above the Upper Limit of Detection",
      caption.placement = getOption("xtable.caption.placement", "top"))

      # Sample values below the LLOD table
      output$below_limit_table <- renderTable({
        req(model())
        cat("Structure of filtered sample data ")
        str(sample_data_au())
        filtered_sample_data_df <- sample_data_au()
        cat("filtered_sample_data_df info")
        print(class(filtered_sample_data_df))
        print(dim(filtered_sample_data_df))
        below_llod <- filtered_sample_data_df[filtered_sample_data_df$gc == "Too Diluted",]


        names(below_llod)[names(below_llod) == "value_reported"] <- "antibody_mfi"
        cat("Below llod class")
        print(class(below_llod))
        print(dim(below_llod))
        print(names(below_llod))
        below_llod <- below_llod[, c("xmap_sample_id", "subject_accession", "antigen",
                                     "plateid", "antibody_mfi", "gc", "in_linear_region")]
        below_llod$xmap_sample_id <- as.character(below_llod$xmap_sample_id)

        return(below_llod)
      },caption="Sample Values below the Lower Limit of Detection",
      caption.placement = getOption("xtable.caption.placement", "top"))


      # output$above_uloq_table <- renderTable({
      #   req(model())
      #   cat("Structure of filtered sample data ")
      #   str(sample_data_au())
      #   filtered_sample_data_df <- sample_data_au()
      #   filtered_sample_data_df <- compute_loq_gate_class(model() ,filtered_sample_data_df)
      # },caption="Sample Values above the Lower Limit of Quantification",
      # caption.placement = getOption("xtable.caption.placement", "top"))


      ## Save the model fit message including NA or value for Asymmetry Parameter
      output$saveModelFitMessage <- renderText({
        NULL
      })



      # Save model fit to database via button
      observeEvent(input$updateModelFit, {
        #if (input$updateModelFit == 0) return(NULL)

        cat("\nin update model fit\n")
        #req(std_curve_data)
        #req(sample_data)
        req(std_curve_data_model_fit())
        req(sample_data_model_fit())
        req(buffer_data_model_fit())
        cat("\after req data\n")
        std_curve_data <- std_curve_data_model_fit()
        sample_data <- sample_data_model_fit()
        buffer_data <- buffer_data_model_fit()


        #req(background_control_rv())
        #req(aggrigate_mfi_dilution())
        #   req(buffer_data)
        req(study_configuration)
        cat("\after req config\n")
        bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
        is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
        aggregate_mfi_dilution <- study_configuration[study_configuration$param_name == "mean_mfi",]$param_boolean_value
        apply_prozone_correction <- study_configuration[study_configuration$param_name == "applyProzone",]$param_boolean_value
       # config_source <- study_configuration[study_configuration$param_name == "default_source",]$param_character_value



        reset_cols <- c("gc", "au", "au_se", "gate_class_loq", "gc", "gate_class_linear_region", "in_linear_region", "in_quantifiable_range", "quality_score")
        sample_data_in <- sample_data[,!names(sample_data) %in% reset_cols]

        cat("\nbefore save fit au\n")
        save_fit_au(dat = std_curve_data, sample_data = sample_data_in, selectedExperiment = selected_experiment(),
                    #selectedSource = config_source, source is looped over in the function
                    #plate_list = input$plateSelection, antigen_list_input = input$antigenSelection,
                    buffer_data = buffer_data, bkg = bkg_method,
                    aggregate_mfi_dilution = aggregate_mfi_dilution,
                    apply_prozone_correction = apply_prozone_correction,
                    is_log_mfi_axis = is_log_mfi )

      }, ignoreInit = TRUE)
      #


      # Antigens on a single plate
      selected_antigen <- reactiveVal(NULL)

      # Update Plates in an entire Experiment NEED This
      selected_plate <- reactiveVal(NULL)



    } else {
       output$standardCurveUI <- renderUI({
              HTML(paste("<span style='font-size:20px;'> There are no standards data available for", selected_experiment(), "in", selected_study(),  "</span>"))
            })
    }




  }) # end moduleServer
}

# --- Destroyable wrappers ---
destroyableStandardCurveFittingModuleUI <- makeModuleUIDestroyable(standardCurveFittingModuleUI)
destroyableStandardCurveFittingServer <- makeModuleServerDestroyable(standardCurveFittingServer)



# observeEvent(list(
#   input$readxMap_experiment_accession,
#   input$readxMap_study_accession,
#   input$qc_component,
#   input$study_level_tabs,
#   input$main_tabs), {
#
#     req(input$qc_component == "Standard Curve",
#         input$readxMap_study_accession != "Click here",
#         input$readxMap_experiment_accession != "Click here",
#         input$study_level_tabs == "Experiments",
#         input$main_tabs == "view_files_tab")
# #observeEvent(input$inLoadedData, {
#   if (input$qc_component == "Standard Curve") {
#     selected_study <- selected_studyexpplate$study_accession
#     selected_experiment <- selected_studyexpplate$experiment_accession
#
#     sample_data <- stored_plates_data$stored_sample
#     standard_data_curve <- stored_plates_data$stored_standard
#     buffer_data <- stored_plates_data$stored_buffer
#
#     # Check if selected study, experiment, and sample data are available
#     if (!is.null(selected_study) && length(selected_study) > 0 &&
#         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
#         !is.null(sample_data) && length(sample_data) > 0){
#
#       # Filter sample data
#       sample_data$selected_str <- paste0(sample_data$study_accession, sample_data$experiment_accession)
#       sample_data <- sample_data[sample_data$selected_str == paste0(selected_study, selected_experiment), ]
#
#       # Summarize sample data
#       cat("Viewing sample data")
#       print(names(sample_data))
#       print(table(sample_data$plateid))
#       print(table(sample_data$antigen))
#       cat("After summarizing sample data")
#
#
#       # Rename columns
#
#       sample_data <- dplyr::rename(sample_data, arm_name = agroup)
#       sample_data <- dplyr::rename(sample_data, visit_name = timeperiod)
#
#
#       sample_data$subject_accession <- sample_data$patientid
#
#       sample_data <- dplyr::rename(sample_data, value_reported = mfi)
#
#       arm_choices <- unique(sample_data$arm_name)
#       visits <- unique(sample_data$visit_name)
#
#       sample_data_model_fit(sample_data)
#     }
#
#     # Check if selected study, experiment, and standard data are available
#     if (!is.null(selected_study) && length(selected_study) > 0 &&
#         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
#         !is.null(standard_data_curve) && length(standard_data_curve) > 0){
#
#       # Filter sample data
#       standard_data_curve$selected_str <- paste0(standard_data_curve$study_accession, standard_data_curve$experiment_accession)
#       standard_data_curve <- standard_data_curve[standard_data_curve$selected_str == paste0(selected_study, selected_experiment), ]
#
#       # Summarize std curve data data
#       cat("View Standard Curve data plateid")
#       print(table(standard_data_curve$plateid))
#       cat("View Standard Curve data antigen")
#       print(table(standard_data_curve$antigen))
#
#       std_curve_data <- standard_data_curve
#
#
#       # Rename columns
#
#       # data <- dplyr::rename(data, arm_name = agroup)
#       #data <- dplyr::rename(data, visit_name = timeperiod)
#
#
#       std_curve_data$subject_accession <- std_curve_data$patientid
#
#       std_curve_data <- calculate_log_dilution(std_curve_data)
#       cat("Standard Curve data after calculating log dilutions")
#       print(names(std_curve_data))
#       std_curve_data_model_fit(std_curve_data)
#
#     }
#
#     # Load Buffer Data
#     if (!is.null(selected_study) && length(selected_study) >0 &&
#         !is.null(selected_experiment) && length(selected_experiment) > 0 &&
#         !is.null(buffer_data) && length(buffer_data) > 0) {
#
#       buffer_data$selected_str <- paste0(buffer_data$study_accession, buffer_data$experiment_accession)
#       buffer_data <- buffer_data[buffer_data$selected_str == paste0(selected_study, selected_experiment), ]
#      # buffer_data_view <<- buffer_data
#       cat("View buffer data names")
#       print(names(buffer_data))
#       buffer_data_model_fit(buffer_data)
#     }
#
#     ## Load study configuration for the user
#     study_configuration <- fetch_study_configuration(study_accession = selected_study , user = currentuser())
#
#     # Determine if standard curve data is available for study and experiment
#     is_standard_avaliable <- checkStandardCurves(standard_data_curve, selected_study, selected_experiment)
#     cat("Standard Curve avaliablilty")
#     print(is_standard_avaliable)
#
#
#
#     #std_curve_data <<- dplyr::rename(std_curve_data, value_reported = mfi)
#     # std_curve_data$antibody_mfi <- std_curve_data$value_reported
#     # std_curve_data$mfi <- std_curve_data$value_reported
#     # data$antibody_mfi <- data$value_reported
#     # data$mfi <- data$value_reported
#     #arm_choices <- unique(data$arm_name)
#     #visits <- unique(data$visit_name)
#     if (is_standard_avaliable) {
#       # bsCollapse(
#       #   id = "StandardCurveCollapse",
#       #   bsCollapsePanel(
#       #     title = "Standard Curve Fitting",
#       #     uiOutput("standardCurveUI"),
#       #     style = "primary"
#       #   )
#       # )
#
#       output$standardCurveUI <- renderUI({
#         req(study_configuration)
#
#         tagList(
#           fluidRow(
#             column(12,
#                    bsCollapse(
#                      id = "methodsCollapse",
#                      bsCollapsePanel(
#                        title = "Standard Curve Fitting Methods",
#                        tagList(
#                          tags$p("Use the dropdown menus titled 'Plate' and 'Antigen' to select the plate and antigen on the plate of interest. If there are more than one source, select the desired source from the radio buttons."),
#                          tags$p("
# We have created an unsupervised algorithm to fit a standard curve to the dilution series of the antigen on the selected plate in the selected study with the Median Fluorescence Index (MFI) as the outcome. The unsupervised algorithm attempts to fit a cascade of models to find the best fit. It stops on a model if the model converges and attempts the next model in the cascade if not. This cascade begins with a 5 parameter logistic regression with the Nonlinear Least Squares (nls) method. (Gottschalk and Dunn, 2004).
# This is followed by a 5 parameter logistic regression model from the Dose-response Data Analysis (drda) library (Malyutina,Tang and Pessi). If the previous two models fail to find a fit, we attempt a 4 parameter logistic regression with the nonlinear least squares method following the method developed by (Rajam et. al. 2019).
# If a 4 parameter model fails we turn to a nls exponential model before fitting no model to the dilution series (Watson, 2018).
# "),
#
#                          tags$p("The figure depicts the raw dilution series, the fitted curve, the lower and upper limit of detection (if estimated), bend lines and the predicted dilution. To view the predicted dilution, click 'Predicted Dilution' in the legend.
# The information icon below the figure provides definitions for terminology and acronyms used in the figure including the following:"),
#                          tags$ul(tags$li("LOD: Limit of Detection"),
#                                  tags$li("AU: Arbitrary Unit"),
#                                  tags$li("Bend Lines: The region between the bend lines provide a line segment of the standard curve which is linear."))
#                          ,
#                          tags$p("The first table below the figure provides the table of coefficients of the fitted model and includes the terms’ estimate, standard error, and p value. The table below the coefficient table provides model fit statistics including the bendlines, limits of detection, model parameter estimates, an indicator of what type of model is fit (crit), and the following model fit statistics:"),
#                          tags$ul(tags$li("iter: number of iterations to convergence"),
#                                  tags$li("status: convergence status. Either converged or not converged"),
#                                  tags$li("loglik: the log likelihood"),
#                                  tags$li("dfresidual: residual degrees of freedom"),
#                                  tags$li("nobs: Number of observations fitted from the dilution series"),
#                                  tags$li("rsquare_fit: The model’s R squared value"),
#                                  tags$li("mse: mean squared error"),
#                                  tags$li("source: the source"),
#                                  tags$li("cv: Coefficient of Variation (%)")
#                          ), # end ul
#                          tags$p("Below the model fit statistics there are two tables that list sample values above the upper limit of detection and below the lower limit of detection.
# "),
#                          tags$p("At the bottom of the screen, if a model exists, there are three ways to save the results before later retrieval. The buttons are labeled as follows:"),
#                          tags$ol(tags$li(HTML("<b>Save Model Fit: </b> When clicked the model fit and the predicted dilution fraction are saved to the database for the current model for the antigen on the plate that is currently in view.
# ")),
#                                  tags$li(HTML("<b>Save Models for All Antigens on Current Plate: </b> When clicked all the antigens on the selected plate are fit to standard curves and model fits and predicted dilution fractions are saved to the database.
# ")),
#                                  tags$li(HTML("<b>Save Models for all Antigens on all Plates in Current Experiment:</b>  When clicked all the plates and all the antigens on those plates are fit to standard curves and model fits and predicted dilution fractions are saved to the database.
# "))), #end ordered list
#                          tags$h3("References"),
#                          tags$p(" Alina Malyutina, Jing Tang, Alberto Pessia (2023). drda: An R Package for Dose-Response Data
#   Analysis Using Logistic Functions. Journal of Statistical Software, 106(4), 1-26.
#   doi:10.18637/jss.v106.i04"),
#                          tags$p("Gottschalk PG and Dunn JR II, Fitting Brenden’s five-parameter logistic curve, Bio-Rad bulletin 3022 (2004)."),
#                          tags$p("Rajam, G., Carlone, G., Kim, E., Choi, J., Paulos, S., Park, S., Jeyachandran, A., Gorantla, Y., Wong, E., Sabnis, A., Browning, P., Desai, R., Quinn, C. P., & Schiffer, J. (2019). Development and validation of a robust multiplex serological assay to quantify antibodies specific to pertussis antigens. Biologicals : journal of the International Association of Biological Standardization, 57, 9–20. https://doi.org/10.1016/j.biologicals.2018.11.001
# "),
#                          tags$p("Watson, D. (n.d.). Fitting exponential decays in R, the easy way. Retrieved January 6, 2025, from https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/")
#
#                        ),# end tagList,
#                        style = "success"
#                      )
#                    ),
#                    # fluidRow(
#                    #   uiOutput("toggle_aggrigate_mfi_dilution"),
#                    #   textOutput("status")
#                    # ),
#                    fluidRow(
#                     # uiOutput("studySelectionUI", style = "margin: 0;"),
#                     # uiOutput("expirementSelectionUI",style = "margin: 0;"),
#                      column(4,uiOutput("plateSelectionUI")),
#                      column(4,uiOutput("antigenSelectionUI")),
#                      column(4,uiOutput("sourceSelectionUI"))
#
#                      # View Methods Information
#                      #actionButton("showMethods", "View Methods"),
#                    #  uiOutput("asymetrySliderUI"),
#                     # uiOutput("asymmetryResetUI"),
#
#                    ),
#                    mainPanel(
#                        # uiOutput("studySelectionUI", style = "margin: 0;"),
#                        # uiOutput("expirementSelectionUI", style = "margin: 0;"),
#                        # uiOutput("plateSelectionUI")),
#                        # uiOutput("antigenSelectionUI"),
#                        # uiOutput("sourceSelection"),
#
#                      # conditional logic in rendering
#                      uiOutput("dilution_message"),
#                      uiOutput("dilution_plot_ui"),
#                      conditionalPanel(condition = "!is.null(mod)",
#                                       plotlyOutput("standardFitUI", width = "75vw")),
#                      div(span(
#                        div(style = "display:inline-block;",
#                            title = "Info",
#                            icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
#                                 `data-placement` = "right", title = "LOD: Limit of Detection<br>AU: Arbitrary Unit<br>Bend Lines:
#                                                         The region between the bend lines provide a line segment of the standard curve which is linear",
#                                 `data-html` = "true")
#                        )
#                      ),
#                      br()
#                      ),
#                      # div(style = "overflow-x:auto; width: 100%",
#                      #     tableOutput("parameterFit")
#                      # ),
#                      # standard_plot data download
#                      downloadButton("download_standard_plot_data","Download Current Standard Curve Data"),
#                      br(),
#                      downloadButton("download_sample_plot_data", "Download Current Sample Data"),
#                      br(),
#                      # Save Model Fits
#                      uiOutput("saveButtonsUI"),
#                      br(),
#
#                      bsCollapse(
#                        id = "standard_curve_model_fit",
#                        bsCollapsePanel(
#                          title = "Parameter Estimates and Summary Statistics",
#                          div(class = "table-container", tableOutput("parameterFit")),
#                          div(class = "table-container", tableOutput("modelFit")),
#                          style = "primary"
#                        )
#                      ),
#                      # div(class = "table-container", tableOutput("parameterFit")),
#                      # div(class = "table-container", tableOutput("modelFit")),
#                      # div(style = "overflow-x: auto; width: 100%;",
#                      #     tableOutput("modelFit")),
#
#                      #h4("Sample Values above the Upper Limit of Detection"),
#                      # div(style = "overflow-x: auto; width: 100%;",
#                      #     tableOutput("above_ulod_table")),
#                      bsCollapse(
#                        id = "gated_samples",
#                        bsCollapsePanel(
#                          title = "Gated Samples",
#                          div(class = "table-container",tableOutput("above_ulod_table")),
#                          div(class = "table-container",tableOutput("below_limit_table")),
#                          style = "primary"
#                        )
#                      )
#                      #div(class = "table-container",tableOutput("above_ulod_table")),
#
#                      # h4("Sample Values below the Lower Limit of Detection"),
#                     # div(style = "overflow-x: auto; width: 100%",
#
#                     # div(class = "table-container",tableOutput("below_limit_table")),
#
#                    )
#             )
#           )
#         )
#       })
#
#       # the method for the blanks
#       # output$blank_method <- renderUI({
#       #   req(background_control_rv())
#       #   background_control_rv()
#       # })
#
#       output$plateSelectionUI <- renderUI({
#         req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#         req(std_curve_data$study_accession, std_curve_data$experiment_accession)
#         updateSelectInput(session, "plateSelection", selected = NULL)  # Reset the plateSelection
#         sc_plate_data <- std_curve_data[std_curve_data$study_accession %in% input$readxMap_study_accession &
#                                        std_curve_data$experiment_accession %in% input$readxMap_experiment_accession, ]
#
#         req(nrow(sc_plate_data) > 0)
#
#         selectInput("plateSelection",
#                     label = "Plate",
#                     choices = unique(sc_plate_data$plateid))
#       })
#
#
#       output$antigenSelectionUI <- renderUI({
#         req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#         req(std_curve_data$study_accession, std_curve_data$experiment_accession)
#         updateSelectInput(session, "antigenSelection", selected = NULL)
#
#         dat_antigen <- std_curve_data[std_curve_data$study_accession %in% input$readxMap_study_accession &
#                                         std_curve_data$experiment_accession %in% input$readxMap_experiment_accession &
#                                         std_curve_data$plateid %in% input$plateSelection, ]
#
#         dat_antigen <- dat_antigen[!is.na(dat_antigen$mfi),]
#         req(nrow(dat_antigen) > 0)
#
#
#         selectInput("antigenSelection",
#                     label = "Antigen",
#                     choices = unique(dat_antigen$antigen)) #unique(dat_antigen$antigen)
#       })
#
#       output$sourceSelectionUI <- renderUI({
#         req(std_curve_data$study_accession, std_curve_data$experiment_accession)
#         req(input$readxMap_study_accession, input$readxMap_experiment_accession, input$plateSelection)
#         req(study_configuration)
#         initial_source <- study_configuration[study_configuration$param_name == "default_source",]$param_character_value
#         cat("\nInitial Source:\n")
#         print(initial_source)
#
#         if (is.null(initial_source) || is.na(initial_source)) {
#           updateRadioButtons(session, "sourceSelection", selected = NULL)
#         } else {
#           updateRadioButtons(session, "sourceSelection", selected = initial_source)
#         }
#
#       #  updateRadioButtons(session, "sourceSelection", selected = NULL)
#
#         dat_source <- std_curve_data[std_curve_data$study_accession %in% input$readxMap_study_accession &
#                                        std_curve_data$experiment_accession %in% input$readxMap_experiment_accession &
#                                        std_curve_data$plateid %in% input$plateSelection &
#                                        std_curve_data$antigen %in% input$antigenSelection, ]
#         req(nrow(dat_source) > 0)
#
#         radioButtons("sourceSelection",
#                      label = "Source",
#                      choices = unique(dat_source$source))
#
#       })
#
#
#
#       # Main Panel Output
#       filtered_data_rv <- reactive({
#         req(std_curve_data)
#         req(study_configuration)
#         bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
#         apply_prozone <- as.logical(toupper(study_configuration[study_configuration$param_name == "applyProzone",]$param_boolean_value))
#         is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#         aggrigate_mfi_dilution <- as.logical(toupper(study_configuration[study_configuration$param_name == "mean_mfi",]$param_boolean_value))
#       #  req(background_control_rv())
#
#         antigen_val <-  if (is.null(selected_antigen())) {
#           input$antigenSelection
#         } else{
#           selected_antigen()
#         }
#
#         plate_val <- if(is.null(selected_plate())) {
#           input$plateSelection
#         } else {
#           selected_plate()
#         }
#
#         filtered_data <- std_curve_data[std_curve_data$source == input$sourceSelection &std_curve_data$antigen == antigen_val
#                                         & std_curve_data$experiment_accession == input$readxMap_experiment_accession &
#                                           std_curve_data$plateid == plate_val,]
#
#         req(nrow(filtered_data) > 0)
#
#         # Apply prozone correction if selected
#         if (apply_prozone) {
#           filtered_data <- correct_prozone(filtered_data, prop_diff = 0.1, dil_scale = 2)
#         }
#         ## Take log MFI if selected in configuration
#         if (is_log_mfi) {
#           filtered_data$mfi <- log10(filtered_data$mfi)
#           buffer_data$mfi <- log10(buffer_data$mfi)
#
#         }
#
#         # Take average of MFI at each dilution factor if option is selected from dilution options. from db not reactive
#         if (aggrigate_mfi_dilution) {
#           filtered_data <- aggregate_mfi_dilution_factor(filtered_data)
#         }
#         # apply buffer/blank methods if selected included
#        # bkg_method <- background_control_rv()
#         if (bkg_method == "included") {
#           filtered_data <- include_blanks(buffer_data, filtered_data, plateid = plate_val, antigen = antigen_val)
#         }
#         if (bkg_method == "subtracted") {
#           filtered_data <- subtract_blanks(buffer_data = buffer_data, data = filtered_data, stype = "standards",
#                                            plateid = plate_val, antigen = antigen_val, multiplier = 1)
#         } else if (bkg_method == "subtracted_3x") {
#           filtered_data <- subtract_blanks(buffer_data = buffer_data, data = filtered_data, stype = "standards",
#                                           plateid = plate_val, antigen = antigen_val, multiplier = 3)
#         } else if (bkg_method == "subtracted_10x") {
#           filtered_data <- subtract_blanks(buffer_data = buffer_data, data = filtered_data, stype = "standards",
#                                            plateid = plate_val, antigen = antigen_val, multiplier = 10)
#         } else {
#           # ignoring buffer
#           filtered_data <- filtered_data
#         }
#
#         return(filtered_data)
#
#       })
#
#
#
#       sample_data_rv <- reactive({
#         req(sample_data)
#         req(study_configuration)
#         bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
#         is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#
#        # req(background_control_rv())
#
#         antigen_val <-  if (is.null(selected_antigen())) {
#           input$antigenSelection
#         } else{
#           selected_antigen()
#         }
#
#         plate_val <- if(is.null(selected_plate())) {
#           input$plateSelection
#         } else {
#           selected_plate()
#         }
#         # Take log of MFI
#         #sample_data_v <<- sample_data
#         if (is_log_mfi) {
#            sample_data$value_reported <- log10(sample_data$value_reported)
#            sample_data$antibody_mfi <- sample_data$value_reported
#            sample_data$mfi <- sample_data$value_reported
#            buffer_data$mfi <- log10(buffer_data$mfi)
#         } else {
#           if ("value_reported" %in% names(sample_data) & !("antibody_mfi" %in% names(sample_data))) {
#             sample_data$antibody_mfi <- sample_data$value_reported
#           }
#         }
#
#        # bkg_method <- background_control_rv()
#         if (bkg_method == "subtracted") {
#           sample_data <- subtract_blanks(buffer_data = buffer_data, data = sample_data, stype = "sample",
#                                          plateid = plate_val, antigen = antigen_val, multiplier = 1)
#         } else if (bkg_method == "subtracted_3x") {
#           sample_data <- subtract_blanks(buffer_data = buffer_data, data = sample_data, stype = "sample",
#                                          plateid = plate_val, antigen = antigen_val, multiplier = 3)
#         } else if (bkg_method == "subtracted_10x") {
#           sample_data <- subtract_blanks(buffer_data = buffer_data, data = sample_data, stype = "sample",
#                                          plateid = plate_val, antigen = antigen_val, multiplier = 10)
#         } else {
#           # ignoring buffer
#           sample_data
#         }
#
#         return(sample_data)
#       })
#
#
#       g_value_5_param <- reactiveVal(NULL)
#
#
#
#       model <- reactive({
#         req(study_configuration)
#         is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#
#         #Access the reactive filtered_data standard
#         filtered_data_val <- filtered_data_rv()
#         #filtered_standard_curve_data <<- filtered_data_val
#         # antigen value based on button condition being pressed
#         antigen_val <-  if (is.null(selected_antigen())) {
#           input$antigenSelection
#         } else{
#           selected_antigen()
#         }
#         req(antigen_val)
#
#         # plate value based on button condition being pressed
#         plate_val <- if (is.null(selected_plate())) {
#           input$plateSelection
#         } else {
#           selected_plate()
#         }
#         req(plate_val)
#
#         # ensure that there are >= 6 standard curve points
#         n_valid_points <- sum(!is.na(filtered_data_val$mfi))
#         if (n_valid_points < 6) {
#           output$dilution_message <- renderUI({
#             HTML(paste("<span style='font-size:20px;'> There are less than 6 points in the dilution series.
#                        Cannot fit a Standard Curve  <br></span>"))
#
#           })
#
#           output$dilution_plot_ui <- renderUI({
#             sample_data_val <- sample_data_rv()
#             req(study_configuration)
#             is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#
#             # standard curve data
#             filtered_data_val <- filtered_data_rv()
#
#             plot_curve_plotly(NULL, filtered_data_val, source_filter = input$sourceSelection,
#                               antigen = input$antigenSelection, plate = input$plateSelection,
#                               sample_data_au = NULL,
#                               is_log_mfi_axis = is_log_mfi)
#           })
#           # save buttons to not appear
#           output$saveButtonsUI <- renderUI({
#             NULL
#           })
#           # return a model of value null
#           return(NULL)
#
#         } else {
#           #filtered_data_val_view <<- filtered_data_val
#           mod <- tryCatch({
#             #set seed for reproducibility
#             set.seed(11262024)
#             compute_robust_curves_5_param(
#               dat = filtered_data_val,
#               antigen = antigen_val,#input$antigenSelection,
#               plate = plate_val,#input$plateSelection,
#               study_accession = input$readxMap_study_accession,
#               experiment_accession = input$readxMap_experiment_accession,
#               source = input$sourceSelection,
#               bkg =  background_control_rv(),
#               is_log_mfi_axis = is_log_mfi,
#               g_value = 0.5
#             )
#           }, error = function(e){
#             print(e)
#             return(NULL)
#           })
#
#           # render message and dilution series if no models were fit
#           if (is.null(mod)) {
#             output$dilution_message <- renderUI({
#               HTML(paste("<span style='font-size:20px;'> No Standard Curve Avaliable: All Models Failed to Converge <br></span>"))
#
#             })
#
#             output$dilution_plot_ui <- renderUI({
#               sample_data_val <- sample_data_rv()
#               req(study_configuration)
#               is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#
#
#               # standard curve data
#               filtered_data_val <- filtered_data_rv()
#
#               plot_curve_plotly(NULL, filtered_data_val, source_filter = input$sourceSelection,
#                                 antigen = input$antigenSelection, plate = input$plateSelection,
#                                 sample_data_au = NULL,
#                                 is_log_mfi_axis = is_log_mfi)
#             })
#             # save buttons to not appear
#             output$saveButtonsUI <- renderUI({
#               NULL
#             })
#             # return a model of value null
#             return(mod)
#           } else {
#               mod <- compute_linear_center(mod, filtered_data_val)
#             # do not show message and raw dilution series if there is a model.
#             output$dilution_message <- renderUI({
#               NULL
#             })
#             output$dilution_plot_ui <- renderUI({
#               NULL
#             })
#             # show the save buttons
#             output$saveButtonsUI <- renderUI({
#               # tagList(
#               #   fluidRow(actionButton("updateModelFit", "Save Model Fit"), style = 'display: inline-block; margin-right: 15px;'),
#               #
#               #   fluidRow(actionButton("updateFitAntigensSinglePlate", "Save Models for all Antigens on Current Plate"),
#               #            style = 'display: inline-block; margin-right: 15px;'),
#               #
#               #   fluidRow(actionButton("updateFitPlatesExp", "Save Models for all Antigens on all Plates in Current Experiment")
#               #            , style = 'display: inline-block; margin-right: 15px;')
#               #) # end tag list
#               # fluidRow(column(4, actionButton("updateModelFit", "Save Model Fit")),
#               #          column(4, actionButton("updateFitAntigensSinglePlate", "Save Models for all Antigens on Current Plate")),
#               #          column(4, actionButton("updateFitPlatesExp", "Save Models for all Antigens on all Plates in Current Experiment")))
#               div(class = "button-container",
#                   actionButton("updateModelFit", "Save Standard Curves for the Current Experiment"))
#                  # actionButton("updateFitAntigensSinglePlate", "Save Models for all Antigens on Current Plate"),
#                   #actionButton("updateFitPlatesExp", "Save Models for all Antigens on all Plates in Current Experiment"))
#
#             })
#
#             return(mod)
#           }
#         }
#         # } # end else
#
#       })
#
#       # Merge and filter the sample data
#       sample_data_au <- reactive({
#         req(model())
#         req(sample_data_rv())
#         req(filtered_data_rv())
#
#         cat("Model in sample_data_au")
#         print(model())
#         cat("Before compute gate class names of sample data")
#         print(names(sample_data_rv()))
#         print(nrow(sample_data_rv()))
#
#         cat("Model status")
#         # if(is.null(model())) {
#         #   cat("model is null")
#         # }
#         # value <- model()
#         # print(value)
#         # if (is.null(value)) {
#         #   cat("value is null")
#         # }
#         # print(str(model()))
#         # if (is.null(model())) {
#         #   cat("model is null")
#         #   return(NULL)
#         #
#         # } else {
#         # gc_sample_data <- compute_gate_class(model(), sample_data_rv())
#         # cat("After compute gate class names of sample data")
#         # print(names(gc_sample_data))
#
#         antigen_val <-  if (is.null(selected_antigen())) {
#           input$antigenSelection
#         } else{
#           selected_antigen()
#         }
#
#         plate_val <- if(is.null(selected_plate())) {
#           input$plateSelection
#         } else {
#           selected_plate()
#         }
#
#
#         sample_data_static <- sample_data_rv()
#
#         sample_data_filtered <- sample_data_static[sample_data_static$antigen == antigen_val &
#                                                      sample_data_static$plateid == plate_val,]
#        sample_data_filtered_v <- sample_data_filtered
#         # sample_data_val_filtered <- sample_data[sample_data$antigen == antigen &
#         #                                           sample_data$plateid == plate,]
#
#
#
#         sample_data_au <- backsub_true_dilution_sample(fitted_model = model(), sample_data = sample_data_filtered, dat = filtered_data_rv())[[1]]
#         #sample_data_au_v_r <<- sample_data_au
#         std_data_au <- backsub_true_dilution_sample(fitted_model = model(), sample_data = sample_data_filtered, dat = filtered_data_rv())[[2]]
#         names(std_data_au)[names(std_data_au) == "antibody_au"] <- "predicted_log_dilution"
#        # std_data_au_v <<- std_data_au
#         gc_sample_data <- compute_gate_class(model(), sample_data_au)
#         cat("After compute gate class names of sample data")
#         print(names(gc_sample_data))
#
#         # add gating for limits of quantification
#         sample_data_au <- compute_loq_gate_class(model_list = model(), sample_data_au = gc_sample_data)
#
#         #sample_data_au_g <<- sample_data_au
#
#         return(sample_data_au)
#
#       })
#
#       # UI for Standard Curve plot
#       output$standardFitUI <- renderPlotly({
#         req(model())
#         req(study_configuration)
#         is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#
#
#         if (is.null(model())) {
#           cat("in standard fit UI: model is NULL")
#           NULL
#         } else {
#           req(filtered_data_rv())
#           req(sample_data_au())
#           cat("in standard fit UI: model present")
#           print(nrow(sample_data_au()))
#           print(sample_data_au())
#
#           plot_curve_plotly(model(), filtered_data_rv(), source_filter = input$sourceSelection,
#                             antigen = input$antigenSelection, plate = input$plateSelection,
#                             sample_data_au = sample_data_au(),
#                             is_log_mfi_axis = is_log_mfi)
#         }
#       })
#
#       ## download plot standard data
#       # output$download_standard_plot_data <- renderUI({
#       #   req(filtered_data_rv())
#       #   filtered_data_static <- filtered_data_rv()
#       #
#       #  download_button <-  createDownloadPlotData(data_component = filtered_data_static, file_suffix = "plot_standard_data")
#       #  return(download_button)
#       # })
#
#       output$download_standard_plot_data <- downloadHandler(
#         filename = function() {
#           paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "plot_standard_data.csv", sep = "_")
#         },
#         content = function(file) {
#           # download data component (data frame)
#           write.csv(filtered_data_rv(), file)
#         }
#       )
#
#       output$download_sample_plot_data <-  downloadHandler(
#           filename = function() {
#             paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "plot_sample_data.csv", sep = "_")
#           },
#           content = function(file) {
#             # download data component (data frame)
#             write.csv(sample_data_au(), file)
#           }
#         )
#
#
#
#       # UI for dilution series with no fit
#       output$noFitUI <- renderUI({
#         mod <- model()
#         req(mod)
#         req(study_configuration)
#         bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
#         is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#
#
#         sample_data_val <- sample_data_rv()
#
#         # standard curve data
#         filtered_data_val <- filtered_data_rv()
#
#         plot_curve_plotly(mod, filtered_data_val, source_filter = input$sourceSelection,
#                           antigen = input$antigenSelection, plate = input$plateSelection,
#                           sample_data_au = sample_data_au,
#                           is_log_mfi_axis = is_log_mfi)
#       })
#
#       # Parameter table from model
#       output$parameterFit <- renderTable({
#         mod <- model()
#         req(mod)
#
#         fit_table <- mod[1]
#         fit_table <- as.data.frame(fit_table)
#
#         fit_table
#       })
#
#       # Model Fit for saving UI
#       output$modelFitUI <- renderUI({
#         mod <- model()
#         req(mod)
#
#         model_fit_tab <- mod[2]
#         model_fit_tab <- as.data.frame(model_fit_tab)
#
#         # display table
#         tableOutput("modelFit")
#       })
#
#       # Table for model fit
#       output$modelFit <- renderTable({
#         mod <- model()
#         req(mod)
#         model_fit_tab <- mod[2]
#         model_fit_tab <- as.data.frame(model_fit_tab)
#         model_fit_tab
#       })
#
#
#       # Sample values above ULOD table
#       output$above_ulod_table <- renderTable({
#         req(model())
#         req(sample_data_au())
#         cat("Printing in above ulod table ")
#         filtered_sample_data_df <- sample_data_au()
#         above_ulod_table <- filtered_sample_data_df[filtered_sample_data_df$gc== "Above_Upper_Limit",]
#         names(above_ulod_table)[names(above_ulod_table) == "value_reported"] <- "antibody_mfi"
#         above_ulod_table <- above_ulod_table[, c("xmap_sample_id", "subject_accession", "antigen",
#                                                  "plateid", "antibody_mfi", "gc", "in_linear_region")]
#         above_ulod_table$xmap_sample_id <- as.character(above_ulod_table$xmap_sample_id)
#
#         return(above_ulod_table)
#
#       }, caption="Sample Values above the Upper Limit of Detection",
#       caption.placement = getOption("xtable.caption.placement", "top"))
#
#       # Sample values below the LLOD table
#       output$below_limit_table <- renderTable({
#         req(model())
#         cat("Structure of filtered sample data ")
#         str(sample_data_au())
#         filtered_sample_data_df <- sample_data_au()
#         cat("filtered_sample_data_df info")
#         print(class(filtered_sample_data_df))
#         print(dim(filtered_sample_data_df))
#         below_llod <- filtered_sample_data_df[filtered_sample_data_df$gc == "Below_Lower_Limit",]
#
#
#         names(below_llod)[names(below_llod) == "value_reported"] <- "antibody_mfi"
#         cat("Below llod class")
#         print(class(below_llod))
#         print(dim(below_llod))
#         print(names(below_llod))
#         below_llod <- below_llod[, c("xmap_sample_id", "subject_accession", "antigen",
#                                      "plateid", "antibody_mfi", "gc", "in_linear_region")]
#         below_llod$xmap_sample_id <- as.character(below_llod$xmap_sample_id)
#
#         return(below_llod)
#       },caption="Sample Values below the Lower Limit of Detection",
#       caption.placement = getOption("xtable.caption.placement", "top"))
#
#
#       # output$above_uloq_table <- renderTable({
#       #   req(model())
#       #   cat("Structure of filtered sample data ")
#       #   str(sample_data_au())
#       #   filtered_sample_data_df <- sample_data_au()
#       #   filtered_sample_data_df <- compute_loq_gate_class(model() ,filtered_sample_data_df)
#       # },caption="Sample Values above the Lower Limit of Quantification",
#       # caption.placement = getOption("xtable.caption.placement", "top"))
#
#
#       ## Save the model fit message including NA or value for Asymmetry Parameter
#       output$saveModelFitMessage <- renderText({
#         NULL
#       })
#
#
#       # model_fit_requested <- reactiveVal(FALSE)
#       #
#       # observeEvent(input$updateModelFit, {
#       #   model_fit_requested(TRUE)
#       # })
#       #
#       # observe({
#       #   req(model_fit_requested())
#       #
#       #   # Only proceed when required inputs are available
#       #   req(std_curve_data, sample_data, buffer_data, study_configuration)
#       #
#       #   bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
#       #   is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#       #   aggregate_mfi_dilution <- study_configuration[study_configuration$param_name == "mean_mfi",]$param_boolean_value
#       #   apply_prozone_correction <- study_configuration[study_configuration$param_name == "applyProzone",]$param_boolean_value
#       #   config_source <- study_configuration[study_configuration$param_name == "default_source",]$param_character_value
#       #
#       #   reset_cols <- c("gc", "au", "au_se", "gate_class_loq", "gc", "gate_class_linear_region", "in_linear_region", "in_quantifiable_range", "quality_score")
#       #   sample_data_in <- sample_data[,!names(sample_data) %in% reset_cols]
#       #
#       #   save_fit_au(
#       #     dat = std_curve_data,
#       #     sample_data = sample_data_in,
#       #     selectedExperiment = input$readxMap_experiment_accession,
#       #     selectedSource = config_source,
#       #     buffer_data = buffer_data,
#       #     bkg = bkg_method,
#       #     aggregate_mfi_dilution = aggregate_mfi_dilution,
#       #     apply_prozone_correction = apply_prozone_correction,
#       #     is_log_mfi_axis = is_log_mfi
#       #   )
#       #
#       #   # reset the trigger so it won't fire again unless button is clicked again
#       #   model_fit_requested(FALSE)
#       # })
#
#
#       # Save model fit to database via button
#       observeEvent(input$updateModelFit, {
#         #if (input$updateModelFit == 0) return(NULL)
#
#         #req(std_curve_data)
#         #req(sample_data)
#         req(std_curve_data_model_fit())
#         req(sample_data_model_fit())
#         req(buffer_data_model_fit())
#         std_curve_data <- std_curve_data_model_fit()
#         sample_data <- sample_data_model_fit()
#         buffer_data <- buffer_data_model_fit()
#
#
#         #req(background_control_rv())
#         #req(aggrigate_mfi_dilution())
#      #   req(buffer_data)
#         req(study_configuration)
#         bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
#         is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#         aggregate_mfi_dilution <- study_configuration[study_configuration$param_name == "mean_mfi",]$param_boolean_value
#         apply_prozone_correction <- study_configuration[study_configuration$param_name == "applyProzone",]$param_boolean_value
#         config_source <- study_configuration[study_configuration$param_name == "default_source",]$param_character_value
#
#
#         reset_cols <- c("gc", "au", "au_se", "gate_class_loq", "gc", "gate_class_linear_region", "in_linear_region", "in_quantifiable_range", "quality_score")
#         sample_data_in <- sample_data[,!names(sample_data) %in% reset_cols]
#         #sample_data_in_button <<- sample_data_in
#
#        # sample_data_au_v_save <<- sample_data_au()
#
#
#         # Reset gated columns if they are already gated
#         # reset_cols <- c("gc", "au", "au_se", "gate_class_loq", "gc", "in_linear_region", "in_quantifiable_range")
#         # for (col in reset_cols) {
#         #   if (any(!is.na(sample_data[[col]]))) {
#         #     sample_data[[col]] <- NA
#         #   }
#         # }
#         #std_curve_data_view <<- std_curve_data
#
#         save_fit_au(dat = std_curve_data, sample_data = sample_data_in, selectedExperiment = input$readxMap_experiment_accession,
#                     selectedSource = config_source,
#                     #plate_list = input$plateSelection, antigen_list_input = input$antigenSelection,
#                     buffer_data = buffer_data, bkg = bkg_method,
#                     aggregate_mfi_dilution = aggregate_mfi_dilution,
#                     apply_prozone_correction = apply_prozone_correction,
#                     is_log_mfi_axis = is_log_mfi )
#
#       }, ignoreInit = TRUE)
#       #
#
#
#       # Antigens on a single plate
#       selected_antigen <- reactiveVal(NULL)
#
#       # Update Plates in an entire Experiment NEED This
#       selected_plate <- reactiveVal(NULL)
#
#
#       # Save all fits on a single plate
#       # observeEvent(input$updateFitAntigensSinglePlate, {
#       #   req(std_curve_data)
#       #   req(sample_data)
#       #  # req(background_control_rv())
#       #   req(aggrigate_mfi_dilution())
#       #   req(buffer_data)
#       #   req(study_configuration)
#       #   bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
#       #   is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#       #
#       #
#       #   # All antigens for the selected plate
#       #   plate_data <- std_curve_data[std_curve_data$plateid %in% input$plateSelection, ]
#       #   antigen_list <- unique(plate_data$antigen)
#       #
#       #   # Reset gated columns if they are already gated
#       #   reset_cols <- c("gc", "au", "au_se", "gate_class_loq", "gc", "in_linear_region", "in_quantifiable_range")
#       #   for (col in reset_cols) {
#       #     if (any(!is.na(sample_data[[col]]))) {
#       #       sample_data[[col]] <- NA
#       #     }
#       #   }
#       #   cat("Antigen List in observe event")
#       #   print(antigen_list)
#       #   cat("Calling save fit au")
#       #   save_fit_au(
#       #     dat = std_curve_data,
#       #     sample_data = sample_data,
#       #     selectedExperiment = input$readxMap_experiment_accession,
#       #     selectedSource = input$sourceSelection,
#       #     plate_list = input$plateSelection,
#       #     antigen_list_input = antigen_list,
#       #     buffer_data = buffer_data,
#       #     bkg = bkg_method,
#       #     aggrigate_mfi_dilution = aggrigate_mfi_dilution(),
#       #     is_log_mfi_axis = is_log_mfi
#       #   )
#       # })
#
#       # Save all fits on all plates in current experiment
#       # observeEvent(input$updateFitPlatesExp, {
#       #   req(std_curve_data)
#       #   req(sample_data)
#       #   req(background_control_rv())
#       #   req(aggrigate_mfi_dilution())
#       #   req(buffer_data)
#       #   req(study_configuration)
#       #   bkg_method <- study_configuration[study_configuration$param_name == "blank_option",]$param_character_value
#       #   is_log_mfi <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))
#       #
#       #
#       #   experiment_data <- std_curve_data[std_curve_data$experiment_accession %in% input$readxMap_experiment_accession, ]
#       #
#       #   plate_list_input <- unique(experiment_data$plateid)
#       #   antigen_list <- unique(experiment_data$antigen)
#       #
#       #   print(plate_list_input)
#       #   print(antigen_list)
#       #
#       #   # Reset gated columns if they are already gated
#       #   reset_cols <- c("gc", "au", "au_se", "gate_class_loq", "gc", "in_linear_region", "in_quantifiable_range")
#       #   for (col in reset_cols) {
#       #     if (any(!is.na(sample_data[[col]]))) {
#       #       sample_data[[col]] <- NA
#       #     }
#       #   }
#       #
#       #   save_fit_au(
#       #     dat = std_curve_data,
#       #     sample_data = sample_data,
#       #     selectedExperiment = input$readxMap_experiment_accession,
#       #     selectedSource = input$sourceSelection,
#       #     plate_list = plate_list_input,
#       #     antigen_list_input = antigen_list,
#       #     buffer_data = buffer_data,
#       #     bkg = background_control_rv(),
#       #     aggrigate_mfi_dilution = aggrigate_mfi_dilution(),
#       #     is_log_mfi_axis = is_log_mfi
#       #   )
#       # })
#
#     } else {
#       output$standardCurveUI <- renderUI({
#         HTML(paste("<span style='font-size:20px;'> There are no standards data available for", selected_experiment, "in", selected_study,  "</span>"))
#       })
#
#     } # end if else for checking if standards are available.
#
#
#   } # end in Standard Curve tab
#   else {
#     output$standardCurveUI <- renderUI(NULL)
#     output$plateSelectionUI <- renderUI(NULL)
#     output$antigenSelectionUI <- renderUI(NULL)
#     output$sourceSelectionUI <- renderUI(NULL)
#     output$dilution_message <- renderUI(NULL)
#     output$dilution_plot_ui <- renderUI(NULL)
#     output$standardFitUI <- renderPlotly(NULL)
#     output$download_standard_plot_data <- downloadHandler(
#       filename = function() { NULL },
#       content = function(file) {}
#     )
#     output$download_sample_plot_data <- downloadHandler(
#       filename = function() { NULL },
#       content = function(file) {}
#     )
#     output$saveButtonsUI <- renderUI(NULL)
#     output$parameterFit <- renderTable(NULL)
#     output$modelFit <- renderTable(NULL)
#     output$above_ulod_table <- renderTable(NULL)
#     output$below_limit_table <- renderTable(NULL)
#
#   }
# })
#
#
# #
#
#
