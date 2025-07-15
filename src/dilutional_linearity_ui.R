
dilutionalLinearityModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
    .dilutional_linearity-collapse-container {
      width: 75vw;
      overflow-x: auto;
    }
    .dilution-linearity-plot-container {
      width: 75vw;
    }
  "))),
    fluidRow(
      bsCollapse(
        id = ns("dilutional_linearity_methods"),
        bsCollapsePanel(
          title = "Dilutional Lineaerity Methods",
    tags$p("To assess dilutional linearity for an antigen select the antigen and the response type which is either Arbitrary Units or MFI (Median Fluorescence Intensity).
                           A linear model for the selected antigen is fitted for each plate with the outcome being the response at one dilution and the predictor is the response at another dilution. The predictor is taken to be the response at the middle dilution in the dilution series and the responses are the other dilutions (one at a time).
                           A correction is applied to adjust the response by fitting an initial linear model and subtracting the intercept of that model from the response and dividing by the slope of the model. A new linear model is then fit with the corrected response values.
                           If the option is not selected the initial linear model is used. Model fit statistics are provided and can be downloaded with and without the correction and the processed data with the corrected response (new_y) can be downloaded To switch between plates, use the tabs associated with each plate.
                           The figure depicts the responses from each dilution plotted against each other. Points are colored by the gate class of the linear region at each dilution by taking the combination of that sample at the x-axis serum dilution then the y-axis serum dilution.
                           The first corresponds to the x axis serum dilution and the second part of the status corresponds to the serum dilution on the y-axis and is labeled in the legend as (x-concentration status / y-concentration status)"),
    style = "success"
        )),
    mainPanel(
      fluidRow(column(3, uiOutput(ns("da_lin_antigenUI"))),
               column(3,
                      uiOutput(ns("response_selectionUI"))),
               column(3, uiOutput(ns("exclude_concentrated_samples_UI"))),
               column(3,uiOutput(ns("linear_correction_UI")))),
      uiOutput(ns("facet_tabs_ui")),
      div(class = "dilution-linearity-plot-container",
        plotlyOutput(ns("selected_facet"))
      ),
      br(),
      downloadButton(ns("download_processed_lm_fit_data"), "Download Processed Linear Model Data"),
      br(),
      div(class = "dilutional_linearity-collapse-container",
      bsCollapse(
        id = ns("linearity_stats"),
        bsCollapsePanel(
          title = "Summary Statistics",
          div(class = "table-container", tableOutput(ns("facet_model_glance"))),
          style = "primary"
        )
      )
      ),
      fluidRow(column(6,downloadButton(ns("download_model_correction_glance"), "Download Model Statistics with Linear Correction")),
               column(6,downloadButton(ns("download_model_uncorrected_glance"), "Download Model Statistics without Linear Correction")))
    )

    )
  ) # end tagList
}

dilutionalLinearityServer <- function(id, selected_study, selected_experiment, currentuser) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sample_data_dl <- fetch_db_samples(study_accession = selected_study(), experiment_accession = selected_experiment())

      if (!is.null(selected_study()) && length(selected_study()) > 0 &&
        !is.null(selected_experiment()) && length(selected_experiment()) > 0 &&
        !is.null(sample_data_dl) && length(sample_data_dl > 0)){

      # Filter sample data
      sample_data_dl$selected_str <- paste0(sample_data_dl$study_accession, sample_data_dl$experiment_accession)
      sample_data_dl <- sample_data_dl[sample_data_dl$selected_str == paste0(selected_study(), selected_experiment()), ]

      # Summarize sample data
      cat("Viewing sample data dilution linearity module")
      print(names(sample_data_dl))
      print(table(sample_data_dl$plateid))
      print(table(sample_data_dl$antigen))
      cat("After summarizing sample data dilution linearity module")


      # Rename columns

      sample_data_dl <- dplyr::rename(sample_data_dl, arm_name = agroup)
      sample_data_dl <- dplyr::rename(sample_data_dl, visit_name = timeperiod)


      sample_data_dl$subject_accession <- sample_data_dl$patientid

      sample_data_dl <- dplyr::rename(sample_data_dl, value_reported = antibody_mfi)

      arm_choices <- unique(sample_data_dl$arm_name)
      visits <- unique(sample_data_dl$visit_name)

      }

    ## Load study configuration for the user
    study_configuration <- fetch_study_configuration(study_accession = selected_study() , user = currentuser())
    is_log_mfi_axis <- as.logical(toupper(study_configuration[study_configuration$param_name == "is_log_mfi_axis",]$param_boolean_value))

    output$da_lin_antigenUI <- renderUI({
      req(study_configuration)
      req(nrow(study_configuration) > 0)
      cat("in antigen lin selector")
      antigen_choices <- strsplit(study_configuration[study_configuration$param_name == "antigen_order",]$param_character_value, ",")[[1]]
      if (all(is.na(antigen_choices))) {
        node_order_in <- strsplit(study_configuration[study_configuration$param_name == "node_order",]$param_character_value, ",")[[1]]

        gated_data <- calculate_sample_concentration_status(study_accession = selected_study(), experiment_accession = selected_experiment(), node_order = node_order_in)

        antigen_choices <-  unique(gated_data$antigen)
      }

      selectInput(ns("antigen_da_lin_selector"),
                  label = "Select Antigen",
                  choices = antigen_choices,
                  multiple = F)
    })

    output$response_selectionUI <- renderUI({
      selectInput(
        inputId = ns("dil_lin_response"),
        label = "Response type",
        choices = c("Arbritary Units" = "au",
                    "MFI" = "mfi")
      )
    })

    output$linear_correction_UI <- renderUI({
      checkboxInput(inputId = ns("apply_lm_corr"),
                    label = "Apply Linear Correction",
                    value = T)
    })

    output$exclude_concentrated_samples_UI <- renderUI({
      checkboxInput(inputId = ns("exclude_conc_samples"),
                    label = "Exclude Concentrated Samples from Model Fittting",
                    value = T)
    })

    plate_lm_facets <- reactive({
    #  req(input$qc_component == "Dilutional Linearity")
      req(study_configuration)
      # req(selected_study)
      # req(selected_experiment)
      req(input$dil_lin_response)
      req(input$antigen_da_lin_selector)

      distinct_samples <- prepare_lm_sample_data(
        study_accession = selected_study(),
        experiment_accession = selected_experiment(),
        is_log_mfi_axis = is_log_mfi_axis,
        response_type = input$dil_lin_response
      )


      dil_lin_regress_list <- dil_lin_regress(
        distinct_samples,
        response_type = input$dil_lin_response,
        exclude_conc_samples = input$exclude_conc_samples
      )



      produce_all_plate_facets(
        distinct_samples = distinct_samples,
        dil_lin_regress_list = dil_lin_regress_list,
        selected_antigen = input$antigen_da_lin_selector,
        is_dil_lin_corr = input$apply_lm_corr,
        response_type = input$dil_lin_response,
        is_log_mfi_axis = is_log_mfi_axis
      )

    })

    #cached_facets <- reactiveVal(NULL)

    # observeEvent(plate_lm_facets(), {
    #   cached_facets(plate_lm_facets())
    # })

    observe({
     req(plate_lm_facets())
      plot_list <- plate_lm_facets()

      for (i in seq_along(plot_list)) {
        local({
          my_i <- i
          output_id <- ns(paste0("facet_lm_plot_", my_i))

          #cat("Registering renderPlotly for", output_id, "\n")

          output[[output_id]] <- renderPlotly({
           # cat("Rendering plot for", output_id, "\n")
            p <- plot_list[[my_i]]
          #  cat("  --> trace count:", length(p$x$data), "\n")
            p

          })


         outputOptions(output, output_id, suspendWhenHidden = FALSE)
        })
      }
    })

    #
    output$facet_tabs_ui <- renderUI({
      req(plate_lm_facets())
      plot_list <- plate_lm_facets()

      tab_list <- lapply(seq_along(plot_list), function(i) {
        if (!is.null(plot_list[[i]])) {
          tabPanel(
            title = paste("Plate", i),
            value = paste0("plate", i),
            NULL # Initially nothing so plots have time to render
            #cat("Output plot for", ns(paste0("facet_lm_plot_", i)), "\n"),

            #plotlyOutput(outputId = ns(paste0("facet_lm_plot_", i))) #, width = "75vw", height = "500px")

          )
        } else {
          NULL
        }
      })

    #  print(tab_list)
      tab_list <- tab_list[!sapply(tab_list, is.null)]
      do_call_result <- do.call(tabsetPanel, c(tab_list, id = ns("facet_tabs_ui")))
      do_call_result
    })


    # The one currently selected
    output$selected_facet <- renderPlotly({
      req(input$facet_tabs_ui)
      facets <- plate_lm_facets()

      req(facets[[input$facet_tabs_ui]])

      facets[[input$facet_tabs_ui]]
    })
    #

    output$facet_model_glance <- renderTable({
      # req(input$qc_component == "Dilutional Linearity") Dilutional Linearity
      # req(selected_study)
      # req(selected_experiment)
      req(input$dil_lin_response)
      #req(is_log_mfi_axis)
      req(input$antigen_da_lin_selector)


      distinct_samples <- prepare_lm_sample_data(study_accession = selected_study(), experiment_accession = selected_experiment(), is_log_mfi_axis = is_log_mfi_axis, response_type = input$dil_lin_response)
      dil_lin_regress_list <- dil_lin_regress(distinct_samples, response_type = input$dil_lin_response, exclude_conc_samples = input$exclude_conc_samples)

      if (input$apply_lm_corr) {
        glance_df <- dil_lin_regress_list$model_corr_glance_df
      } else {
        glance_df <- dil_lin_regress_list$glance_uncorrect_df
      }

      glance_df <- glance_df[glance_df$antigen == input$antigen_da_lin_selector,]

      return(glance_df)
    }, caption = "Antigen Model Fit Statistics",
    caption.placement = getOption("xtable.caption.placement", "top"))

    # Download associated data for processed lm fits has corrected y value
    output$download_processed_lm_fit_data <-  downloadHandler(
      filename = function() {
        paste(selected_study(), selected_experiment(), "processed_lm_fit_data.csv", sep = "_")
      },
      content = function(file) {
        # req(selected_study)
        # req(selected_experiment)
        req(input$dil_lin_response)
      #  req(is_log_mfi_axis)
        req(input$antigen_da_lin_selector)

        distinct_samples <- prepare_lm_sample_data(study_accession = selected_study(), experiment_accession = selected_experiment(), is_log_mfi_axis = is_log_mfi_axis, response_type = input$dil_lin_response)
        dil_lin_regress_list <- dil_lin_regress(distinct_samples, response_type = input$dil_lin_response, exclude_conc_samples = input$exclude_conc_samples)
        proccessed_lm_fit_data <- dil_lin_regress_list$observed_data
        # download data component (data frame)
        write.csv(proccessed_lm_fit_data, file)
      }
    )

    output$download_model_uncorrected_glance <- downloadHandler(
      filename = function() {
        paste(selected_study(), selected_experiment(), "lm_uncorrected_glance.csv", sep = "_")
      },
      content = function(file) {
        # req(selected_study)
        # req(selected_experiment)
        req(input$dil_lin_response)
      #  req(is_log_mfi_axis)
        req(input$antigen_da_selector)


        distinct_samples <- prepare_lm_sample_data(study_accession = selected_study(), experiment_accession = selected_experiment(), is_log_mfi_axis = is_log_mfi_axis, response_type = input$dil_lin_response)
        dil_lin_regress_list <- dil_lin_regress(distinct_samples, response_type = input$dil_lin_response, exclude_conc_samples = input$exclude_conc_samples)

        glance_df <- dil_lin_regress_list$glance_uncorrect_df


        # download data component (data frame)
        write.csv(glance_df, file)
      }
    )

    output$download_model_correction_glance <- downloadHandler(
      filename = function() {
        paste(selected_study(), selected_experiment(), "lm_correction_glance.csv", sep = "_")
      },
      content = function(file) {
        # req(selected_study)
        # req(selected_experiment)
        req(input$dil_lin_response)
       # req(is_log_mfi_axis)
        req(input$antigen_da_selector)


        distinct_samples <- prepare_lm_sample_data(study_accession = selected_study(), experiment_accession = selected_experiment(), is_log_mfi_axis = is_log_mfi_axis, response_type = input$dil_lin_response)
        dil_lin_regress_list <- dil_lin_regress(distinct_samples, response_type = input$dil_lin_response, exclude_conc_samples = input$exclude_conc_samples)

        glance_df <- dil_lin_regress_list$model_corr_glance_df


        # download data component (data frame)
        write.csv(glance_df, file)
      }
    )





  }) # end ModuleServer
}

#--- Destroyable wrappers ---
destroyableDilutionalLinearityModuleUI <- makeModuleUIDestroyable(dilutionalLinearityModuleUI)
destroyableDilutionalLinearityServer <- makeModuleServerDestroyable(dilutionalLinearityServer)
