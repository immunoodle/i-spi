
data_summary <- list()

std_curve_tab_active <- reactiveVal(FALSE)


output$bead_count_controls_UI <- renderUI({
  tagList(
    fluidRow(
      div(
        style = "width: 100%; padding: 0 15px;",  # Added container styling
        span(
          div(style = "display:inline-block; margin-bottom: 10px;",
              title = "Info",
              icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                   `data-placement` = "right",
                   title = paste("To set the lower and upper thresholds for bead count analysis, use the up and down arrows to adjust the values,
                                 or type a number directly into the fields. If no number is provided, the default values of 35 for the lower threshold
                                 and 50 for the upper threshold will be used. To indicate a failed well, select either 'Below Upper Threshold' or 'Below Lower Threshold'
                                 from the radio buttons titled 'Failed Well Criteria'."),


                   `data-html` = "true")
          )
        )
      )
    ),

    uiOutput("lower_threshold"),
    uiOutput("upper_threshold_ui"),
    uiOutput("failed_well_criteria_ui")
)
})

# Lower threshold
output$lower_threshold <- renderUI({
 #req(lower_threshold_rv())
  numericInput(inputId = "lower_threshold_val",
               label = "Lower Threshold",
               value = lower_threshold_rv(),
               min = NA)
})
# upper threshold
output$upper_threshold_ui <- renderUI({
 # req(upper_threshold_rv())
  numericInput(inputId = "upper_threshold_val",
               label = "Upper Threshold",
               value = upper_threshold_rv(),
               min = NA)
})

output$failed_well_criteria_ui <- renderUI({
  req(failed_well_criteria())
  radioButtons("thresholdCriteria_val",
               label = "Failed Well Criteria",
               choices = c("Below Upper Threshold" = "upper",
                           "Below Lower Threshold" = "lower"),
               selected = failed_well_criteria())
})

observeEvent(input$lower_threshold_val, {
  lower_threshold_rv(input$lower_threshold_val)
})

# Check if the input is NA
# observe({
#   if (is.na(lower_threshold_rv())) {
#     # Reset to default value if NA
#     updateNumericInput(session, "lower_threshold_val", value = 35)
#   }
# })


observeEvent(input$upper_threshold_val, {
  upper_threshold_rv(input$upper_threshold_val)
})

#Check if the input is NA
# observe({
#   if (is.na(upper_threshold_rv())) {
#     # Reset to default value if NA
#     updateNumericInput(session, "upper_threshold_val", value = 50)
#   }
# })

observeEvent(input$thresholdCriteria_val, {
  failed_well_criteria(input$thresholdCriteria_val)
})

