# Dilution QC for Analysis

# Observe the reactive expression for changes
observeEvent(input$stored_header_rows_selected,{

    removeTab(inputId = "body_panel_id", target="dilution_qc_tab")

    # select_plate_id <- stored_plates_data$stored_header[input$stored_header_rows_selected,4]
    # plat_sample_dilution_qc <- update_db(operation = "select", schema = "madi_results", table_name = "xmap_sample", select_where = list("plate_id" = select_plate_id))
    #
    select_study_accession <- stored_plates_data$stored_header[input$stored_header_rows_selected,2]
    select_experiment_accession <- stored_plates_data$stored_header[input$stored_header_rows_selected,3]
    sample_standard_full <- update_db(operation = "select",
                                      schema = "madi_results",
                                      table_name = "xmap_sample",
                                      select_where = list("concat(study_accession,experiment_accession)" = paste0(select_study_accession,select_experiment_accession))
                                    )
    plat_sample_dilution_qc <- sample_standard_full

    insertTab(inputId = "body_panel_id",
              tabPanel(value = "dilution_qc_tab",
                       title = "Dilution QC for Experiment",
                       br(),
                       h4("Fine tune the data for subsequent analysis across plates"),
                       uiOutput("dilution_qc_ui")
              )
    )

    output$dilution_qc_ui <- renderUI({

      fluidPage(
        fluidRow(dataTableOutput("dilution_table"),
                 uiOutput("diluton_tabset_ui"))
      )

    })

    output$dilution_table <- renderDataTable({

      plat_sample_dilution_qc %>%
        select(study_accession, experiment_accession, plate_id, dilution) %>%
        distinct()

    })

    antigen_list_dilution_qc <- unique(plat_sample_dilution_qc$antigen)

    tabs_dilution_qc <- list()

    for (antigen in antigen_list_dilution_qc) {

      sample_antigen <- plat_sample_dilution_qc[plat_sample_dilution_qc$antigen==antigen, ]

      # Instead of rendering here, just collect the tabPanels
      tabs_dilution_qc[[length(tabs_dilution_qc) + 1]] <- tabPanel(
        title = paste("Type", antigen),
        uiOutput(outputId = paste0("ui_dilution_qc_", antigen))  # Placeholder for dynamic content
      )


    }

    output$diluton_tabset_ui <- renderUI({
      do.call(tabsetPanel, args = tabs_dilution_qc)
    })

    create_dilution_qc_ui_output_antigen <- function(antigen, plat_sample_dilution_qc) {
      output_name <- paste0("ui_dilution_qc_", antigen)
      output[[output_name]] <- renderUI({
        create_dilution_qc_ui_antigen(antigen, plat_sample_dilution_qc)
      })
    }

    for (antigen in antigen_list_dilution_qc) {
      local({
        local_antigen <- antigen
        create_dilution_qc_ui_output_antigen(local_antigen, plat_sample_dilution_qc)
      })
    }

    # Output the UIs for each antigen
    for (antigen in antigen_list_dilution_qc) {
      local({
        table_data_reactive <- reactiveVal()
        local_antigen <- antigen
        sample_dilution_qc_table_id <- paste0("sample_dilution_qc_table_",local_antigen)
        refresh_button_id <- paste0("dilution_qc_refresh_",local_antigen)
        select_dilution_db <- paste0("dilution_qc_upload_db_",local_antigen)
        standardize_dilution <- paste0("standardize_dilution_",local_antigen)
        hlines_table_reactive <- reactiveVal()

        # Plot data
        processed_data <- plat_sample_dilution_qc %>%
          filter(antigen == local_antigen) %>%
          select(antibody_mfi, dilution, gate_class, patientid) %>%
          mutate(dilution = factor(dilution))

        processed_data <- calculate_x_min_max_dynamic(processed_data, "dilution", 0.25)

        # Table geom_hline data

        # Checking if gate classes exist:
        # if yes assign hlines to max and min of "between" gate classes
        # if no, assign to 25th and 75th percentile

        if(all(is.na(plat_sample_dilution_qc[plat_sample_dilution_qc$antigen == local_antigen,"gate_class"]))){

          plat_sample_dilution_qc %>%
            dplyr::filter(antigen == local_antigen) %>%
            dplyr::group_by(dilution) %>%
            dplyr::summarise(lower_limit = quantile(antibody_mfi, na.rm = TRUE)[2],
                             upper_limit = quantile(antibody_mfi, na.rm = TRUE)[4]) %>%
            ungroup() %>%
            mutate(dilution = factor(dilution)) -> table_data

        } else {

          plat_sample_dilution_qc %>%
            dplyr::filter(antigen == local_antigen) %>%
            dplyr::filter(gate_class == "Between_Limits") %>%
            dplyr::group_by(dilution) %>%
            dplyr::summarise(lower_limit = min(antibody_mfi),
                             upper_limit = max(antibody_mfi)) %>%
            ungroup() %>%
            mutate(dilution = factor(dilution)) -> table_data


        }

        # Add reference dilution for table
        table_data$reference_dilution = c(TRUE, rep(FALSE, nrow(table_data)-1))

        table_data_reactive(table_data)

        processed_data %>%
          select(dilution, x_min, x_max) %>%
          distinct() %>%
          left_join(table_data, by = "dilution") -> hlines_table

        hlines_table$gate_class <- NA

        hlines_table_reactive(hlines_table)

        # Check if there are any non-NA values in gate_class
        if(any(!is.na(processed_data$gate_class))) {
          # If there are non-NA values, plot with color aesthetic
          p <- ggplot(processed_data, aes(dilution, antibody_mfi, color = gate_class)) +
            geom_point()
        } else {
          # If all values are NA, plot without color aesthetic
          p <- ggplot(processed_data, aes(dilution, antibody_mfi)) +
            geom_point()
          showNotification(paste0("Gate class not found in ", local_antigen), type = "warning")
        }

        p <- p + geom_line(aes(group = patientid), show.legend = FALSE)

        p <- p + geom_segment(data = hlines_table_reactive(), aes(x = x_min, xend = x_max, y = upper_limit, yend = upper_limit))
        p <- p + geom_segment(data = hlines_table_reactive(), aes(x = x_min, xend = x_max, y = lower_limit, yend = lower_limit))

        p <- p +
          theme_minimal() +
          labs(x = "Dilution", y = "MFI") +
          theme(legend.position = "bottom")

        # Create density plot
        p2 <- ggplot(processed_data, aes(x = antibody_mfi)) +
          geom_density(fill = "blue", alpha = 0.5) + # Use ..scaled.. for density scaling
          coord_flip() + # Flip to make it vertical
          theme_minimal() +
          theme(axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor = element_blank())

        output[[sample_dilution_qc_table_id]] <- renderRHandsontable({
          rhandsontable(table_data_reactive())
        })

        output[[paste0("sample_dilution_qc_boxplot_", local_antigen)]] <- renderPlot({
          grid.arrange(p, p2, ncol = 2, widths = c(3, 1))
        })

        observeEvent(input[[refresh_button_id]],{

          table_data_refresh <- hot_to_r(input[[sample_dilution_qc_table_id]])

          # Make sure only 1 reference dilution is selected
          if(sum(table_data_refresh$reference_dilution) != 1){
            if(sum(table_data_refresh$reference_dilution) > 1) error_msg <- "Please select only 1 reference dilution"
            if(sum(table_data_refresh$reference_dilution) < 1) error_msg <- "Please select atleast 1 reference dilution"
            showNotification(error_msg, type = "error")
            return()
          }

          # Assuming test_table_data_refresh$dilution is your variable
          table_data_refresh$dilution <- as.factor(levels(table_data_refresh$dilution))

          print(paste0("clicked refresh button for antigen : ", local_antigen))

          # Plot data
          processed_data <- plat_sample_dilution_qc %>%
            filter(antigen == local_antigen) %>%
            select(antibody_mfi, dilution, gate_class, patientid) %>%
            mutate(dilution = factor(dilution))

          # Generating xmin and xmax for segments
          processed_data <- calculate_x_min_max_dynamic(processed_data, "dilution", 0.25)

          processed_data %>%
            select(dilution, x_min, x_max) %>%
            distinct() %>%
            left_join(table_data_refresh, by = "dilution") -> hlines_table

          hlines_table$gate_class <- NA

          hlines_table_reactive(hlines_table)

          # Check if there are any non-NA values in gate_class
          if(any(!is.na(processed_data$gate_class))) {
            # If there are non-NA values, plot with color aesthetic
            p <- ggplot(processed_data, aes(dilution, antibody_mfi, color = gate_class)) +
              geom_point()
          } else {
            # If all values are NA, plot without color aesthetic
            p <- ggplot(processed_data, aes(dilution, antibody_mfi)) +
              geom_point()
          }

          p <- p + geom_line(aes(group = patientid), show.legend = FALSE)

          p <- p + geom_segment(data = hlines_table_reactive(), aes(x = x_min, xend = x_max, y = upper_limit, yend = upper_limit))
          p <- p + geom_segment(data = hlines_table_reactive(), aes(x = x_min, xend = x_max, y = lower_limit, yend = lower_limit))

          p <- p +
            theme_minimal() +
            labs(x = "Dilution", y = "MFI") +
            theme(legend.position = "bottom")

          p2 <- ggplot(processed_data, aes(x = antibody_mfi)) +
            geom_density(fill = "blue", alpha = 0.5) + # Use ..scaled.. for density scaling
            coord_flip() + # Flip to make it vertical
            theme_minimal() +
            theme(axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor = element_blank())

          output[[paste0("sample_dilution_qc_boxplot_", local_antigen)]] <- renderPlot({
            grid.arrange(p, p2, ncol = 2, widths = c(3, 1))
          })

        })

        observeEvent(input[[standardize_dilution]],{


          table_data_refresh <- hot_to_r(input[[sample_dilution_qc_table_id]])

          # Make sure only 1 reference dilution is selected
          if(sum(table_data_refresh$reference_dilution) != 1){
            if(sum(table_data_refresh$reference_dilution) > 1) error_msg <- "Please select only 1 reference dilution"
            if(sum(table_data_refresh$reference_dilution) < 1) error_msg <- "Please select atleast 1 reference dilution"
            showNotification(error_msg, type = "error")
            return()
          }

          # Data
          sample_data <- plat_sample_dilution_qc %>%
            filter(antigen == local_antigen) %>%
            select(antibody_mfi, dilution, gate_class, patientid) %>%
            mutate(dilution = factor(dilution)) %>%
            arrange(antibody_mfi)

          # Create new columns in the sample variable
          sample_data$antibody_au <- NA
          sample_data$antibody_au_se <- NA
          sample_data$reference_dilution <- NA
          sample_data$gate_class_dil <- NA

          all_dilutions <- unique(sample_data$dilution)

          # Set reference dilution from table
          reference_dilution <- as.character(table_data_refresh[table_data_refresh$reference_dilution == TRUE, ]$dilution)
          sample_data$reference_dilution <- reference_dilution

          remaining_dilutions <- setdiff(all_dilutions, reference_dilution)

          for(dilution_pair in remaining_dilutions){

            print(dilution_pair)

            # Filter Between_limits points
            df <- sample_data %>%
              filter(dilution %in% c(reference_dilution, dilution_pair)) %>%
              group_by(patientid) %>%
              # Filter all patients where gate class is between limits
              filter(all(gate_class == "Between_Limits")) %>%
              ungroup()

            # Filter Between_limit points for reference dilution
            df %>%
              filter(dilution == reference_dilution) -> in_between_reference

            # Filter Between_limit points for corresponding pair
            df %>%
              filter(dilution == dilution_pair) -> in_between_dilution_pair

            # Filter all points for reference variable
            sample_data %>%
              filter(dilution == reference_dilution) -> sampdat_reference

            # Filter all points for dilution pair
            sample_data %>%
              filter(dilution == dilution_pair) -> sampdat_dilution_pair

            # Setting up x and y
            y <- in_between_reference$antibody_mfi
            x <- in_between_dilution_pair$antibody_mfi
            x_new <- sampdat_dilution_pair$antibody_mfi

            summary(y)[c(1,6)]
            summary(x)[c(1,6)]

            # Change to polynomial fit
            fm <- lmrob(y ~ poly(x, 3, raw = T))

            new <- data.frame(x = x_new)
            str(predict(fm, new, se.fit = TRUE))

            y_pred <-  predict(fm, new, se.fit = TRUE)

            df_pred <- tibble(dilution = dilution_pair,
                              patientid = sampdat_dilution_pair$patientid,
                              antibody_au_dilution_pair = y_pred$fit,
                              antibody_au_se_dilution_pair = y_pred$se.fit)

            # Join the new dataframe with predicted values to the original
            sample_data %>%
              dplyr::left_join(df_pred, by = c("dilution", "patientid")) -> sample_data

            sample_data %>%
              # If values are present in antibody_au_dilution_pair keep those
              mutate(antibody_au = ifelse(!is.na(antibody_au_dilution_pair), antibody_au_dilution_pair, antibody_au),
                     antibody_au_se = ifelse(!is.na(antibody_au_se_dilution_pair), antibody_au_se_dilution_pair, antibody_au_se)) %>%
              select(-c(antibody_au_dilution_pair, antibody_au_se_dilution_pair)) -> sample_data

          }


          # Filling the reference pair antibody_au
          sample_data %>%
            mutate(antibody_au = ifelse(is.na(antibody_au), antibody_mfi, antibody_au),
                   antibody_au_se = ifelse(is.na(antibody_au_se), antibody_mfi, antibody_au)) -> sample_data

          # Plot figure
          plot_data <- sample_data

          plot_data %>%
            mutate(dilution = as.numeric(dilution)) %>%
            arrange(dilution) %>%
            mutate(dilution = dilution %>% as.factor() %>% fct_inorder()) -> plot_data

          p_standard <- ggplot(plot_data, aes(dilution, antibody_au, color = gate_class)) +
            geom_point()

          p_standard <- p_standard + geom_line(aes(group = patientid), show.legend = FALSE)

          p_standard <- p_standard + geom_segment(data = hlines_table_reactive(), aes(x = x_min, xend = x_max, y = upper_limit, yend = upper_limit))
          p_standard <- p_standard + geom_segment(data = hlines_table_reactive(), aes(x = x_min, xend = x_max, y = lower_limit, yend = lower_limit))

          p_standard <- p_standard +
            theme_minimal() +
            labs(x = "Dilution", y = "Augmented MFI") +
            theme(legend.position = "bottom")

          p2_standard <- ggplot(plot_data, aes(x = antibody_au)) +
            geom_density(fill = "blue", alpha = 0.5) + # Use ..scaled.. for density scaling
            coord_flip() + # Flip to make it vertical
            theme_minimal() +
            theme(axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor = element_blank())


          output[[paste0("sample_dilution_qc_boxplot_", local_antigen)]] <- renderPlot({
            grid.arrange(p_standard, p2_standard, ncol = 2, widths = c(3, 1))
          })


        })

        observeEvent(input[[select_dilution_db]], {

          print(paste0("clicked upload dilution db for antigen: ", local_antigen))

          table_data_refresh <- hot_to_r(input[[sample_dilution_qc_table_id]])

          # Plot data
          processed_data <- plat_sample_dilution_qc %>%
            filter(antigen == local_antigen) %>%
            select(antibody_mfi, dilution, gate_class, patientid) %>%
            mutate(dilution = factor(dilution))

          # Add a new column gate_class_dilution_qc with limits from the dilution_qc tab
          processed_data$gate_class_dilution_qc <- NA

          all_dilutions <- unique(table_data_refresh$dilution)

          for(dil in all_dilutions){

            # Extract upper and lower limit selected from the table
            lower_limit <- table_data_refresh[table_data_refresh$dilution == dil, ]$lower_limit
            upper_limit <- table_data_refresh[table_data_refresh$dilution == dil, ]$upper_limit

            print(paste0("lower limit for dilution : ",dil, " is ", lower_limit))
            print(paste0("upper limit for dilution : ",dil, " is ", upper_limit))

            processed_data %>%
              mutate(gate_class_dilution_qc = case_when(
                (dilution == dil) & (antibody_mfi < lower_limit) ~ "Below_Lower_Limit",
                (dilution == dil) & (antibody_mfi > upper_limit) ~ "Above_Upper_Limit",
                (dilution == dil) & (antibody_mfi >= lower_limit) & (antibody_mfi <= upper_limit) ~ "Between_Limits",
                TRUE ~ gate_class_dilution_qc
              )) -> processed_data
          }

          processed_data


        })


      })
    }




})


