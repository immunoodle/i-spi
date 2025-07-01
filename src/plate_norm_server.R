plateNormalizationUI <- function(id) {
    ns <- NS(id)
    
    tabPanel(
      title = "Plate Normalization",
      fluidPage(
        # Dropdowns organized in rows
        fluidRow(
          column(4, selectInput(ns("antigen"), "Select Antigen", choices = NULL)),
          column(4, uiOutput(ns("featureSelectorUI"))),
          column(4, selectInput(ns("buffer"), "Select Buffer", choices = NULL)),
          column(4, actionButton(ns("normalize"), "Normalize Plates")),
          column(4, checkboxInput(ns("check1"), "Sample Fit", value = NULL))
        ),
        fluidRow(
          column(12, 
                 plotlyOutput(ns("plot1"), width = "100%", height = "400px")
          ),
          hr(),  # Line between plot 1 and plot 2
          column(12, 
                 plotlyOutput(ns("plot2"), width = "100%", height = "400px")
          ),
          hr(),  # Line between plot 2 and plot 3
          column(12, 
                 conditionalPanel(
                   condition = paste0("input['", ns("check1"), "'] == true"),  # Condition to check if checkbox is checked
                   plotlyOutput(ns("plot3"), width = "100%", height = "400px")
                 )
          )
        )
        
      )
    )
  }

plateNormalizationServer <- function(id, stored_plates_data, selected_studyexpplate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("plot_norm_functions.R", local = TRUE)
    saved_plot <- reactiveVal()
    
    
    selected_study <- reactive({
      req(selected_studyexpplate$study_accession)
      selected_studyexpplate$study_accession
    })
    
    selected_experiment <- reactive({
      req(selected_studyexpplate$experiment_accession)
      selected_studyexpplate$experiment_accession
    })
    
    sample_data_outlier <- reactive({
      req(stored_plates_data$stored_sample)
      stored_plates_data$stored_sample
    })
    
    data <- reactive({
      req(sample_data_outlier(), selected_study(), selected_experiment())
      sample_data_outlier() %>%
        mutate(selected_str = paste0(study_accession, experiment_accession)) %>%
        filter(selected_str == paste0(selected_study(), selected_experiment()))
    })
    
    
    # Antigen selector
    antigen_choices <- reactive({
      req(data())
      unique(data()$antigen)
    })
    
    observe({
      updateSelectInput(session, "antigen", choices = antigen_choices())
    })
    
    # Feature selector
    output$featureSelectorUI <- renderUI({
      req(input$antigen)
      selectInput(ns("feature"), "Select Feature:", choices = unique(data()[data()$antigen == input$antigen, ]$feature))
    })
    
    # Buffer selector
    buffer_choices <- reactive({
      req(data())
      normset_str_choices <- c("standards", "buffers", "buffers, standards", "controls, standards", "buffers, controls, standards")
      normset_str_choices
    })
    
    observe({
      updateSelectInput(session, "buffer", choices = buffer_choices())
    })
    
 
    
    observeEvent(input$normalize, {
      
      
      req(input$antigen, input$feature, input$buffer)
      
      
      
      # Get selected values
      selected_antigen <- input$antigen
      selected_feature <- input$feature
      normset_str <- input$buffer
      
      
      
      # Load plate data
      plate_data <- load_plate_data(
        stored_plates_data = stored_plates_data,
        normset_str = normset_str,
        selected_antigen = selected_antigen,
        selected_feature = selected_feature
      )
      
      
      if (nrow(plate_data) == 0) {
        showNotification("No data available for the selected options.", type = "warning")
        return()  # Stop execution of the rest of the observe function
      }
      #careful here
      
      
     # plate_data <- load_plate_data(stored_plates_data = stored_plates_data, normset_str = normset_str, selected_antigen = selected_antigen, selected_feature = selected_feature)
      ## calculate normal plate as average of plates by c("study_accession", "experiment_accession", "dilution", "antigen", "feature")
      plate_group <- aggregate(x = plate_data$raw_value,
                               by = list(plate_data$study_accession,plate_data$experiment_accession,plate_data$dilution,plate_data$antigen,plate_data$feature),
                               FUN = mean
      )
      names(plate_group)[names(plate_group) == "x"] <- "normal_reference"
      
      plate_data <- merge(plate_data,
                          plate_group,
                          by.x = c("study_accession","experiment_accession","dilution","antigen","feature"),
                          by.y = c("Group.1","Group.2","Group.3","Group.4","Group.5"),
                          all.x = TRUE
      )
      
      # order the normal and raw values 
      
      #sorts data
      plate_data <- plate_data[order(plate_data$plateid,plate_data$normal_reference),]
      
      # get fitted models
      model_results <- fit_models(plate_data)
      #fits the model that are used to norm data
      
      fitted_models <- model_results$models
      #pulls info
      
      model_types <- model_results$model_types
      #just pulls the type
      
      
      #this normalize
      produce_fit_status_tab(model_results = model_results)
      
      #this generates 4 plots
      plots <- plot_plate_fits(plate_data = plate_data, fitted_models = fitted_models)
      
      
      
      output$plot1 <- renderPlotly({
        plots[[1]]  # Make sure you use double square brackets for list access
      })
      
      output$plot2 <- renderPlotly({
        plots[[2]]
      })
      
      
      sample_fit <- plot_sample_fits(
        fitted_models = fitted_models, 
        stored_plates_data = stored_plates_data,
        ant = selected_antigen, 
        feat = selected_feature
      )
      
      
      output$plot3 <- renderPlotly({
        
        sample_fit  # Render the plot stored in the reactiveVal
        
      })
      
      
    })
    
  
    
    
    
  
  })
  
 
}