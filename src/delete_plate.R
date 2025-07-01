
observeEvent(input$stored_header_rows_selected, {
        if (delete_button) {
          select_study_accession <<- stored_plates_data$stored_header[input$stored_header_rows_selected,2]
          select_experiment_accession <<- stored_plates_data$stored_header[input$stored_header_rows_selected,3]
          print(paste("Selected Experiment: ",select_study_accession, "-", select_experiment_accession))
          select_plate_id <- stored_plates_data$stored_header[input$stored_header_rows_selected,4]
          print(paste("Selected Plate ID: ", select_plate_id))
          selected_auth0_user <- stored_plates_data$stored_header[input$stored_header_rows_selected,4]
        }
      })


