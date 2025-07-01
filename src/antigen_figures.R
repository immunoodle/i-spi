# Figures sample script
library(tidyverse)
library(shiny)
library(shinyWidgets)

# For the jitter to remain same every time - THIS DOES NOT WORK 
set.seed(1234)
options(scipen = 999)

sample <- readRDS("sample.rds")
sample <- sample %>% 
  select(dilution, antigen, antibody_mfi) %>% 
  filter(antigen == "gb")

buffer <- readRDS("buffer.rds")
buffer <- buffer %>% 
  select(dilution, antigen, antibody_mfi) %>% 
  filter(antigen == "gb") 
buffer$type <- "buffer"

standard <- readRDS("standard.rds")
standard <- standard %>% 
  select(dilution, antigen, antibody_mfi) %>% 
  filter(antigen == "gb")

control <- readRDS("control.rds")
control <- control %>% 
  select(dilution, antigen, antibody_mfi) %>% 
  filter(antigen == "gb")
control$type <- "control"

# Define UI
ui <- fluidPage(
  titlePanel("Shiny App for Figures"),
  
  # Create a tabset panel
  tabsetPanel(
    tabPanel("Tab 1", 
             uiOutput("figure_customisation"))
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observe({
    
    output$figure_customisation <- renderUI({
      fluidRow(
        fluidRow(
        column(3, 
               br(),
                 # Add your sidebar content here
                 h4("Sidebar Panel"), 
                 selectInput("test", "Test", choices = "test")
               ),
        column(9,
                 # Add your main panel content here
                br(),
               column(
                 1, 
                 fluidRow(
                   noUiSliderInput(
                     inputId = "sample_slider", label = "Sample Limits",
                     min = min(sample$antibody_mfi), max = max(sample$antibody_mfi), step = 1,
                     value = c(quantile(sample$antibody_mfi)[2], quantile(sample$antibody_mfi)[4]), margin = 1,
                     orientation = "vertical",
                     direction = "rtl",
                     width = "100px", height = "300px"
                   ) 
                 )
               ), 
               column(3, 
                      plotOutput("sample_plot")), 
               column(3,
                      plotOutput("buffer_control_plot")), 
               column(4, 
                      plotOutput("standard_plot"))
               )
      ), 
      fluidRow(
        column(2, ""),
        column(3, plotOutput("below_sample_ref_plot")),
        column(3, plotOutput("between_sample_ref_plot")),
        column(3, plotOutput("above_sample_ref_plot"))
      )
    )
    })
    
  })

  output$sample_plot <- renderPlot({
    
    ggplot(sample, aes(x = factor(1), y = antibody_mfi)) +
      geom_boxplot() +
      geom_point(aes (x= 1, y = antibody_mfi)) + 
      # geom_jitter(width = 0.2, color = "blue", alpha = 0.5) + 
      geom_hline(yintercept = input$sample_slider[1], linetype = "dashed", color = "red") +  # 25th quantile
      geom_hline(yintercept = input$sample_slider[2], linetype = "dashed", color = "green") + # 75th quantile
      xlab("Antibody MFI") +
      ylab("MFI") +
      ggtitle("Samples") +
      theme_bw()
    
  })
  
  output$buffer_control_plot <- renderPlot({
    
    buffer %>% 
      rbind(control) %>% 
      ggplot(aes(x = factor(1), y = antibody_mfi)) +
      geom_boxplot() +
      geom_point(aes (x= 1, y = antibody_mfi, color = type)) +
      # geom_jitter(width = 0.2, color = "blue", alpha = 0.5) +
      xlab("Antibody MFI") +
      ylab("MFI") +
      ggtitle("Buffers and Controls") +
      theme_bw() +
      theme(legend.title = element_blank())
    
    
  })
  
  output$standard_plot <- renderPlot({
    
    standard %>%
      ggplot(aes(x = dilution, y = antibody_mfi)) +
      geom_point() + 
      xlab("Dilution") +
      ylab("MFI") +
      ggtitle("Standards") +
      theme_bw()
    
  })
  
  output$below_sample_ref_plot <- renderPlot({
    
    sample <- sample %>% filter(antibody_mfi < input$sample_slider[1])
    
    ggplot(sample, aes(x = antibody_mfi)) +
      geom_density() +
      theme_bw()
    
    
  })
  
  output$between_sample_ref_plot <- renderPlot({
    
    sample <- sample %>% filter(antibody_mfi >= input$sample_slider[1] & antibody_mfi <= input$sample_slider[2])
    
    ggplot(sample, aes(x = antibody_mfi)) +
      geom_density() +
      theme_bw()
    
  })
  
  output$above_sample_ref_plot <- renderPlot({
    
    sample <- sample %>% filter(antibody_mfi > input$sample_slider[2])
    
    ggplot(sample, aes(x = antibody_mfi)) +
      geom_density() +
      theme_bw()
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
