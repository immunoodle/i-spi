standard_curve_table_list <- list()

data_summary <- list()

std_curve_tab_active <- reactiveVal(FALSE)


output$blankControlOptionUI <- renderUI({
  tagList(
    fluidRow(
      div(
        style = "width: 100%; padding: 0 15px;",  # Added container styling
        span(
          div(style = "display:inline-block; margin-bottom: 10px;",
              title = "Info",
              icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                   `data-placement` = "right",
                   title = paste("To select a method for which the buffers are to be treated for the selected study click one of the methods.
                                 The avaliable methods are 'Ignore', 'Include', 'Subtract Geometric Mean', 'Subtract three times the Geometric Mean' and subtract 10 times the geometric mean.
                                 When Ignore is selected, the buffers are not considered in the standard curve.
                                 When Included is selected, the stimation of the standard curve takes into account the mean of the background
of the values as another point of the standard curve. The median fluorescence intensity and the expected concentration for this new point by analyte is estimated as follows:
                                 MFI: geometric mean of the blank controls.
                                 log dilution: The mininum log dilution - log10(2). This corresponds to the the minimum expected concentration value of the standard points divided by 2 as in the drLumi package.
                                 When subtracted is selected, the geometric mean of the blank controls is subracted from all the standard points. Depending on what level of subtraction is selected,
                                 the geometric mean is multiplied by that factor (1,3, or 10) before the subtraction is applied. After subtraction, if any MFI is below 0 it is set to 0."),
                   `data-html` = "true")
          )
        )
        )
      ),
    fluidRow(
      uiOutput("blank_option_UI")
    )
  )
})

output$blank_option_UI <- renderUI({
  req(background_control_rv())
  radioButtons("blank_option", "Blank Control:",
               choices = c("Ignored" = "ignored", "Included" = "included", "Subtracted 1 x Geometric mean" = "subtracted",
                           "Subtract 3 x Geometric Mean" = "subtracted_3x", "Subtracted 10 x Geometric Mean"  = "subtracted_10x"),
               selected = background_control_rv())
})


observeEvent(input$blank_option, {
  background_control_rv(input$blank_option)
})
