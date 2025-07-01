# function to pull all data needed for the sample gating figure


create_plot_dat_plate <- function(select_study_accession = NULL, select_experiment_accession = NULL, select_plateid = NULL) {

  print("pulling all_sample")
  all_sample <- subset(storedlong_plates_data$stored_sample,select = -c(well,timeperiod,sampleid,id_imi,agroup,pctaggbeads,samplingerrors))
  names(all_sample)[names(all_sample) == "n"] <- "nbeads"
  all_sample$focal_group <- factor(ifelse(all_sample$plateid==select_plateid,"Focal plate","All others"))
  all_sample <- all_sample %>% mutate(focal_group = fct_relevel(focal_group, "Focal plate", after = Inf ))
  all_sample <<- all_sample[order(all_sample$antigen, decreasing = FALSE), ]
  p_data$all_sample <- all_sample

  check_buffer <- storedlong_plates_data$stored_buffer
  if (exists("check_buffer")&&is.data.frame(get("check_buffer"))) {
    nrows_buffer <- nrow(storedlong_plates_data$stored_buffer)
  } else {
    nrows_buffer <- 0
  }
  if (nrows_buffer > 0) {
    print("at loading all_buffer")
    all_buffer <- subset(storedlong_plates_data$stored_buffer,select = -c(well,pctaggbeads,samplingerrors))
    names(all_buffer)[names(all_buffer) == "n"] <- "nbeads"
    all_buffer$focal_group <- ifelse(all_buffer$plateid==select_plateid,"Focal plate","All others")
    all_buffer_check <<- all_buffer
    print(paste("nrows p_data all buffer:",nrow(p_data$all_buffer)))
    p_data$all_buffer <- all_buffer
  }

  check_control <- storedlong_plates_data$stored_control
  if (exists("check_control")&&is.data.frame(get("check_control"))) {
    nrows_control <- nrow(storedlong_plates_data$stored_control)
  } else {
    nrows_control <- 0
  }
  if (nrows_control > 0) {
    print("at loading all_control")
    all_control <- subset(storedlong_plates_data$stored_control,select = -c(well,sampleid,source,pctaggbeads,samplingerrors))
    names(all_control)[names(all_control) == "n"] <- "nbeads"
    all_control$focal_group <- ifelse(all_control$plateid==select_plateid,"Focal plate","All others")
    p_data$all_control <- all_control
    all_control_check <<- all_control
  }

  check_standard <- storedlong_plates_data$stored_standard
  ### create standard curve sets even in the presence of missing data
  if (is.null(nrow(storedlong_plates_data$stored_standard))) {
    nrows_standard_raw <- 0
    pa_data$standard_status <- 'No Standard Data'
  } else if (nrow(storedlong_plates_data$stored_standard) == 0) {
    nrows_standard_raw <- 0
    pa_data$standard_status <- 'No Standard Data'
  } else {
    nrows_standard_raw <- nrow(storedlong_plates_data$stored_standard)
    if (is.null(nrow(storedlong_plates_data$stored_fitstor))){
      nrows_fitstor <- 0
      pa_data$standard_status <- 'No Standard Curve Fit'
    } else if (nrow(storedlong_plates_data$stored_fitstor) == 0) {
      nrows_fitstor <- 0
      pa_data$standard_status <- 'No Standard Curve Fit'
    } else {
      nrows_fitstor <- nrow(storedlong_plates_data$stored_fitstor)
      pa_data$standard_status <- 'Standard Curve Fit'
    }
  }

  if (exists("check_standard")&&is.data.frame(get("check_standard"))) {
    nrows_standard_raw <- nrow(storedlong_plates_data$stored_standard)
  } else {
    nrows_standard_raw <- 0
  }
  if (nrows_standard_raw > 0) {
    print("at loading all_standard_raw")
    all_standard_raw <- subset(storedlong_plates_data$stored_standard,select = -c(well,sampleid,source,pctaggbeads,samplingerrors))
    names(all_standard_raw)[names(all_standard_raw) == "n"] <- "nbeads"
    all_standard_raw$focal_group <- ifelse(all_standard_raw$plateid==select_plateid,"Focal plate","All others")
    all_standard_raw <- all_standard_raw
    p_data$all_standard_raw <- all_standard_raw
    nrows_fitstor <- nrow(storedlong_plates_data$stored_fitstor)
    if (nrows_fitstor > 0) {
      print("create all_standard_corrected")
      all_standard <- storedlong_plates_data$stored_fitstor
      names(all_standard)[names(all_standard) == "n"] <- "nbeads"
      all_standard$focal_group <- factor(ifelse(all_standard$plateid==select_plateid,"Focal plate","All others"))
      p_data$all_standard <- all_standard
      all_standard <<- all_standard

      print("create all_fits")
      all_fits <- storedlong_plates_data$stored_fits
      all_fits$focal_group <- factor(ifelse(all_fits$plateid==select_plateid,"Focal plate","All others"))
      p_data$all_fits <- all_fits
      all_fits <<- all_fits

      print("create all_fit_note")
      all_fit_note <- data.frame(plateid = storedlong_plates_data$stored_fits$plateid, antigen = storedlong_plates_data$stored_fits$antigen, source = storedlong_plates_data$stored_fits$source, fit_note = paste0("R-Square: ",round(storedlong_plates_data$stored_fits$rsquare_fit,3),", Residual degrees of freedom: ",storedlong_plates_data$stored_fits$dfresidual))
      all_fit_note$focal_group <- factor(ifelse(all_fit_note$plateid==select_plateid,"Focal plate","All others"))
      p_data$all_fit_note <- all_fit_note
      all_fit_note <<- all_fit_note

      print("create all_preds")
      all_preds <- storedlong_plates_data$stored_preds
      all_preds$focal_group <- factor(ifelse(all_preds$plateid==select_plateid,"Focal plate","All others"))
      p_data$all_preds <- all_preds
      all_preds <<- all_preds

      print("create all_fit_tab")
      all_fit_tab <- storedlong_plates_data$stored_fit_tab
      all_fit_tab$focal_group <- factor(ifelse(all_fit_tab$plateid==select_plateid,"Focal plate","All others"))
      p_data$all_fit_tab <- all_fit_tab
      all_fit_tab <<- all_fit_tab
    }
  }
  print(paste("loaded study:",select_study_accession,"experiment:",select_experiment_accession,"plate:",select_plateid))
}

create_plot_dat_plate_antigen <- function(antigen = NULL) {
    req(input$gaterxMap_plateid)
    select_plateid <- input$gaterxMap_plateid
    print(paste("filling pa reactive:",select_plateid,"Antigen:",antigen))

    print(paste("at creating all_antigen:", antigen))
    all_antigen <- p_data$all_sample[p_data$all_sample$antigen==antigen, ]
    pa_data$all_antigen <- all_antigen
    print(pa_data$all_antigen[3,])

    pa_data$sample_antigen <- pa_data$all_antigen[all_antigen$focal_group=="Focal plate",]
    pa_data$sample_antigen$sample_type <- "Sample"

    print(paste("at creating ldata_antigen:", antigen))
    ldata_antigen <- data.frame(focal_group=c("Focal plate","All others"),
                        median = c(quantile(all_antigen[all_antigen$focal_group=="Focal plate",]$mfi, na.rm = TRUE)[3],
                                   quantile(all_antigen[all_antigen$focal_group=="All others",]$mfi, na.rm = TRUE)[3]))
    pa_data$ldata_antigen <- ldata_antigen
    max_sample <- max(pa_data$sample_antigen$mfi)
    print(pa_data$ldata_antigen)

    print(paste("at creating buffer_antigen:", antigen))
    check_buffer <- storedlong_plates_data$stored_buffer
    if (exists("check_buffer")&&is.data.frame(get("check_buffer"))) {
      nrows_buffer <- nrow(storedlong_plates_data$stored_buffer)
    } else {
      nrows_buffer <- 0
    }
    print(paste("nrows_buffer:",nrows_buffer))
    if (nrows_buffer > 0) {
      check_pa_buffer <- p_data$all_buffer[p_data$all_buffer$focal_group=="Focal plate" & p_data$all_buffer$antigen==antigen,]
      nrows_pa_buffer <- nrow(check_pa_buffer)
      print(paste("nrows:",nrows_pa_buffer))
      if (nrows_pa_buffer > 0) {
        buffer_a <- p_data$all_buffer[p_data$all_buffer$focal_group=="Focal plate" &
                                          p_data$all_buffer$antigen==antigen, c("study_accession","experiment_accession","plateid","antigen","feature", "nbeads","mfi","focal_group")]
        buffer_a$sample_type <- "Buffer"
        pa_data$buffer_a <- buffer_a
        max_buffer <- max(pa_data$buffer_a$mfi)
      } else {
        max_buffer <- max(pa_data$sample_antigen$mfi)
        nrows_pa_buffer <- 0
      }
    } else {
      max_buffer <- max(pa_data$sample_antigen$mfi)
      nrows_pa_buffer <- 0
    }
    pa_data$nrows_pa_buffer <- nrows_pa_buffer
    print(paste("max:", max_buffer))

    print(paste("at creating control_antigen:", antigen))
    check_control <- storedlong_plates_data$stored_control
    if (exists("check_control")&&is.data.frame(get("check_control"))) {
      nrows_control <- nrow(storedlong_plates_data$stored_control)
    } else {
      nrows_control <- 0
    }
    print(paste("nrows_control:",nrows_control))
    if (nrows_control > 0) {
      check_pa_control <- p_data$all_control[p_data$all_control$focal_group=="Focal plate" & p_data$all_control$antigen==antigen,]
      nrows_pa_control <- nrow(check_pa_control)
      print(paste("nrows:",nrows_pa_control))
      if (nrows_pa_control > 0) {
        control_a <- p_data$all_control[p_data$all_control$focal_group=="Focal plate" &
                                          p_data$all_control$antigen==antigen, c("study_accession","experiment_accession","plateid","antigen","feature", "nbeads","mfi","focal_group")]
        control_a$sample_type <- "Control"
        pa_data$control_a <- control_a
        max_control <- max(pa_data$control_a$mfi)
      } else {
        max_control <- max(pa_data$sample_antigen$mfi)
        nrows_pa_control <- 0
      }
    } else {
      max_control <- max(pa_data$sample_antigen$mfi)
      nrows_pa_control <- 0
    }
    pa_data$nrows_pa_control <- nrows_pa_control
    print(paste("max:", max_control))

    max_common <- round_any(max(max_buffer,max_control,max_sample), 500, f = ceiling)

    ### create a reference line table for buffer and control that matches
    ### the buffers and controls that are identified in each plate
    print(paste(nrows_pa_buffer, nrows_pa_control))
    if(nrows_pa_buffer > 0 & nrows_pa_control > 0){
      # print("in both buffer and control")
      pa_data$samp_base <- do.call("rbind", list(pa_data$sample_antigen[ , c("mfi","nbeads","sample_type")],
                                                 pa_data$buffer_a[ , c("mfi","nbeads","sample_type")],
                                                 pa_data$control_a[ , c("mfi","nbeads","sample_type")]))
      # print(names(control_a))
      # print(names(buffer_a))
      control_antigen <- rbind(control_a, buffer_a)
      pa_data$control_antigen <- control_antigen
      print(paste("control_antigen nrows:",nrow(pa_data$control_antigen)))
      reference_lines <- data.frame(reference_line=c('Buffer Mean','Buffer Max','3SD Above Buffer','10SD Above Buffer','Control Mean','Control Min', 'Control Max'),
                                    yinterc=c(round(mean(buffer_a$mfi),1)
                                                 ,round(max(buffer_a$mfi),1)
                                                 ,round(3 * sd(buffer_a$mfi) + mean(buffer_a$mfi),1)
                                                 ,round(10 * sd(buffer_a$mfi) + mean(buffer_a$mfi),1)
                                                 ,round(mean(control_a$mfi),1)
                                                 ,round(min(control_a$mfi),1)
                                                 ,round(max(control_a$mfi),1)
                                    ),
                                    control_type=c(1,1,1,1,2,2,2),
                                    reflintype=c(1,2,3,4,5,6,7),
                                    type_labels=c(paste('Buffer Mean:', round(mean(buffer_a$mfi),1)),
                                                  paste('Buffer Max:', round(max(buffer_a$mfi),1)),
                                                  paste('3SD Above Buffer:', round(3 * sd(buffer_a$mfi) + mean(buffer_a$mfi),1)),
                                                  paste('10SD Above Buffer:', round(10 * sd(buffer_a$mfi) + mean(buffer_a$mfi),1)),
                                                  paste('Control Mean: ', round(mean(control_a$mfi),1)),
                                                  paste('Control Min:', round(min(control_a$mfi),1)),
                                                  paste('Control Max:', round(max(control_a$mfi),1))
                                    )
      )
      reference_lines$reference_line <- factor(reference_lines$reference_line)
      reference_lines$control_type <- factor(reference_lines$control_type)
      reference_lines$reflintypetype <- factor(reference_lines$reflintype)
    } else if (nrows_pa_buffer > 0 & nrows_pa_control < 1){
      # print("in buffer no control")
      # print(names(pa_data$sample_antigen))
      # print(names(pa_data$buffer_a))
      pa_data$samp_base <- do.call("rbind", list(pa_data$sample_antigen[ , c("mfi","nbeads","sample_type")],
                                                 pa_data$buffer_a[ , c("mfi","nbeads","sample_type")]))
      control_antigen <- buffer_a
      reference_lines <- data.frame(reference_line=c('Buffer Mean','Buffer Max','3SD Above Buffer','10SD Above Buffer'),
                                    yinterc=c(round(mean(buffer_a$mfi),1)
                                                 ,round(max(buffer_a$mfi),1)
                                                 ,round(3 * sd(buffer_a$mfi) + mean(buffer_a$mfi),1)
                                                 ,round(10 * sd(buffer_a$mfi) + mean(buffer_a$mfi),1)

                                    ),
                                    control_type=c(1,1,1,1),
                                    reflintype=c(1,2,3,4),
                                    type_labels=c(paste('Buffer Mean:', round(mean(buffer_a$mfi),1)),
                                                  paste('Buffer Max:', round(max(buffer_a$mfi),1)),
                                                  paste('3SD Above Buffer:', round(3 * sd(buffer_a$mfi) + mean(buffer_a$mfi),1)),
                                                  paste('10SD Above Buffer:', round(10 * sd(buffer_a$mfi) + mean(buffer_a$mfi),1))
                                    )
      )
      reference_lines$reference_line <- factor(reference_lines$reference_line)
      reference_lines$control_type <- factor(reference_lines$control_type)
      reference_lines$reflintype <- factor(reference_lines$reflintype)
    } else if (nrows_pa_buffer < 1 & nrows_pa_control > 0){
      # print("in control no buffer")
      pa_data$samp_base <- do.call("rbind", list(pa_data$sample_antigen[ , c("mfi","nbeads","sample_type")],
                                                 pa_data$control_a[ , c("mfi","nbeads","sample_type")]))
      control_antigen <- control_a
      reference_lines <- data.frame(reference_line=c('Control Mean','Control Min','Control Max'),
                                    yinterc=c(round(mean(control_a$mfi),1)
                                                 ,round(min(control_a$mfi),1)
                                                 ,round(max(control_a$mfi),1)
                                    ),
                                    control_type=c(2,2,2),
                                    reflintype=c(5,6,7),
                                    type_labels=c(paste('Control Mean: ', round(mean(control_a$mfi),1)),
                                                  paste('Control Min:', round(min(control_a$mfi),1)),
                                                  paste('Control Max:', round(max(control_a$mfi),1))
                                    )
      )
      reference_lines$reference_line <- factor(reference_lines$reference_line)
      reference_lines$control_type <- factor(reference_lines$control_type)
      reference_lines$reflintype <- factor(reference_lines$reflintype)
    } else {
      control_antigen <- NULL
      reference_lines$reference_line <- factor(reference_lines$reference_line)
      reference_lines$control_type <- factor(reference_lines$control_type)
      reference_lines$reflintype <- factor(reference_lines$reflintype)
    }
    reference_lines <- reference_lines
    # class(reference_lines)
    print(reference_lines)
    pa_data$reference_lines <- reference_lines
    pa_data$control_antigen <- control_antigen

    ### create standard curve sets even in the presence of missing data
    if (is.null(nrow(p_data$all_standard_raw))) {
      nrows_standard_raw <- 0
      pa_data$standard_status <- 'No Standard Data'
    } else if (nrow(p_data$all_standard_raw) == 0) {
      nrows_standard_raw <- 0
      pa_data$standard_status <- 'No Standard Data'
    } else {
      nrows_standard_raw <- nrow(p_data$all_standard_raw)
      if (is.null(nrow(p_data$all_standard))){
        nrows_fitstor <- 0
        pa_data$standard_status <- 'No Standard Curve Fit'
      } else if (nrow(p_data$all_standard) == 0) {
        nrows_fitstor <- 0
        pa_data$standard_status <- 'No Standard Curve Fit'
      } else {
        nrows_fitstor <- nrow(p_data$all_standard)
        pa_data$standard_status <- 'Standard Curve Fit'
      }
    }
    if (pa_data$standard_status == 'Standard Curve Fit') {
      standard_antigen <- p_data$all_standard[p_data$all_standard$focal_group=="Focal plate" & p_data$all_standard$antigen==antigen, ]
      pa_data$standard_antigen <- standard_antigen
      preds_antigen <- p_data$all_preds[p_data$all_preds$focal_group=="Focal plate" & p_data$all_preds$antigen==antigen, ]
      pa_data$preds_antigen <- preds_antigen
      fit_tab_antigen <- p_data$all_fit_tab[p_data$all_fit_tab$focal_group=="Focal plate" & p_data$all_fit_tab$antigen==antigen, c(6:10)]
      pa_data$fit_tab_antigen <- fit_tab_antigen
      fit_note <- p_data$all_fit_note[p_data$all_fit_note$focal_group=="Focal plate" & p_data$all_fit_note$antigen==antigen, ]
      pa_data$fit_note <- fit_note
      hline_fit <- p_data$all_fits[p_data$all_fits$focal_group=="Focal plate" & p_data$all_fits$antigen==antigen, ]
      pa_data$hline_fit <- hline_fit
      max_standpoint <- max(standard_antigen$mfi)*1.2
      max_preds <- max(preds_antigen$fitted)*1.2
      max_standard <- max(max_standpoint,max_preds)
      min_max <- c(hline_fit$llod[1],hline_fit$ulod[1])
      pa_data$min_max <- min_max
    } else if (pa_data$standard_status == 'No Standard Curve Fit') {
      standard_antigen <- p_data$all_standard_raw[p_data$all_standard_raw$focal_group=="Focal plate" & p_data$all_standard_raw$antigen==antigen, ]
      pa_data$standard_antigen <- standard_antigen
      max_standard <- max(standard_antigen$mfi)*1.2
      min_max <- c(quantile(sample_antigen$mfi, na.rm = TRUE)[2],quantile(sample_antigen$mfi, na.rm = TRUE)[4])
      pa_data$min_max <- min_max
    } else {
      pa_data$standard_antigen <- NULL
      max_standard <- max(sample_antigen$mfi)*1.2
      min_max <- c(quantile(sample_antigen$mfi, na.rm = TRUE)[2],quantile(sample_antigen$mfi, na.rm = TRUE)[4])
      pa_data$min_max <- min_max
    }
    print(paste("antigen:",antigen,"gate start min_max:",pa_data$min_max))
    ### Create a common axis max
    max_common <- round_any(max(max_standard,max_buffer,max_control,max_sample), 500, f = ceiling)
    pa_data$max_common <- max_common
    print(paste("antigen:",antigen,"max_common:",pa_data$max_common))
    pa_data$nrows_pa_standard <- nrow(pa_data$standard_antigen)
  }# end function

# ui_output_main <- paste0("ui_", local_antigen)
sample_slider_id <- paste0("sample_slider")
slider_upper_text_id <- paste0("sample_slider_upper_textinput")
slider_lower_text_id <- paste0("sample_slider_lower_textinput")

debouncedInput_upper_limit <- reactive(input[[slider_upper_text_id]]) %>% debounce(1000)

debouncedInput_lower_limit <- reactive(input[[slider_lower_text_id]]) %>% debounce(1000)

# Slider input observe events
observeEvent(debouncedInput_upper_limit(),{
  # print("upper limit input")
  sliderVal_upper <- c(as.numeric(input[[slider_lower_text_id]]), as.numeric(input[[slider_upper_text_id]]))
  test_sliderVal_upper <- sliderVal_upper
  updateNoUiSliderInput(session, sample_slider_id, value = sliderVal_upper)
})

observeEvent(debouncedInput_lower_limit(),{
  # print("lower limit input")
  sliderVal_lower <- c(as.numeric(input[[slider_lower_text_id]]), as.numeric(input[[slider_upper_text_id]]))
  test_sliderVal_lower <- sliderVal_lower

  updateNoUiSliderInput(session, sample_slider_id, value = sliderVal_lower)
})

observeEvent(input[[sample_slider_id]],{

  upper_value <- input[[sample_slider_id]][2]
  lower_value <- input[[sample_slider_id]][1]

  updateTextInput(session, slider_upper_text_id, value = upper_value)
  updateTextInput(session, slider_lower_text_id, value = lower_value)
})

observeEvent(input$stored_header_rows_selected, {
    removeTab(inputId = "body_panel_id", target="gaterxMap")
    color_buffer <- c("#DD4444", "#555599")
    color_control <- c("#DD4444", "#555599")
    select_study_accession <- input$readxMap_study_accession
    select_experiment_accession <- input$readxMap_experiment_accession
    select_plateid <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("plateid")]
    print(paste("Selected Study: ",
                select_study_accession,
                "Selected Experiment: ",
                select_experiment_accession,
                "Selected Plate:
                ",select_plateid)
          )
    output$studytxtOutput = renderText({
      paste0("Study Name: ", input$readxMap_study_accession)
    })
    output$experimenttxtOutput = renderText({
      paste0("Experiment Name: ", input$readxMap_experiment_accession)
    })
    output$selected_plate_text = renderText({
      paste0("Selected Plate: ", select_plateid)
    })
    # p_data_all_sample <<- p_data$all_sample
    # p_data_all_standard_raw <<- p_data$all_standard_raw

    create_plot_dat_plate(select_study_accession = select_study_accession,
                          select_experiment_accession = select_experiment_accession,
                          select_plateid = select_plateid
                          )
    print("observe header row")
    print(pa_data$standard_status)
    if (pa_data$standard_status == 'Standard Curve Fit') {
      make_standard_plot <- c("Standard")
      standard_source <- c(unique(p_data$all_standard$source))
    } else if (pa_data$standard_status == 'No Standard Curve Fit') {
      make_standard_plot <- c("Standard")
      standard_source <- c(unique(p_data$all_standard_raw$source))
    } else {
      make_standard_plot <- c("NoStandard")
      standard_source <- c("NoStandard")
    }

    print("after standard plot made in select header row - before tab")
    print(pa_data$standard_status)

    antigen_list <- unique(p_data$all_sample[p_data$all_sample$focal_group=="Focal plate", "antigen"])
    ## set_initial min max
    pa_data$min_max <-  c(quantile(p_data$all_sample[p_data$all_sample$focal_group=="Focal plate" & p_data$all_sample$antigen==antigen_list[1], "mfi"], na.rm = TRUE)[2],
                 quantile(p_data$all_sample[p_data$all_sample$focal_group=="Focal plate" & p_data$all_sample$antigen==antigen_list[1], "mfi"], na.rm = TRUE)[4]
                  )
    print(names(p_data$all_sample))
    print(pa_data$min_max)
    print(nrow(p_data$all_sample))
    pa_data$max_common <- (round_any(max(p_data$all_sample[p_data$all_sample$focal_group=="Focal plate", "mfi"]), 500, f = ceiling))

    insertTab(inputId = "body_panel_id",
        tabPanel(value = "gaterxMap",
          title = "Individual Plate QA Analysis",
          h3("MADI Lumi Reader - Curate data from an individual plate"),
          bsCollapsePanel(
            "Instructions",
           p("This is where we can REVIEW and CURATE the data from an existing individual plate"),
           p("1. At the start it takes some time for the figures to render. More time is taken if there is a standard curve to process."),
           p("2. On the Samples figure:"),
           p(style = "padding-left: 15px;","a. Hover over points to see both the individual MFI and number of beads."),
           p(style = "padding-left: 15px;","b. Upper and lower limits are defined as cut points between classification of MFI values."),
           p(style = "padding-left: 15px;","c. Inital upper and lower limits coincide with the 75th and 25th quantiles of the MFI distribution"),
           p(style = "padding-left: 15px;","d. Users can change the upper and lower limits using either the limits-input boxes or the slider."),
           p(style = "padding-left: 15px;","e. After the limits are updated and the Sample figure renders, the classification is updated in the density figured at the bottom of the page."),
           p(style = "padding-left: 15px;","f. When satisfied with the classification, Save the classification to the data for this individual plate."),
           p("2. On the Standard Curve figure:"),
           p(style = "padding-left: 15px;","a. Nonlinear fitting and LODs match the methods of Rajam et. al. Biologicals. 2019 January ; 57: 9–20. (DOI:10.1016/j.biologicals.2018.11.001)"),
           p(style = "padding-left: 15px;","b. Bend limits from logistic functions follow the methods of Sebaugh and McCray. Pharmaceut. Statist. 2003; 2: 167–174 (DOI:10.1002/pst.062) "),
           style = "success"),
           fluidRow(
               column(3,textOutput("studytxtOutput")),
               column(3,textOutput("experimenttxtOutput"))
            ),
            fluidRow(
              # p(paste0("Selected plate: ", select_plateid)),
              column(3,selectInput("gaterxMap_plateid",
                          "Choose Plate Name",
                          choices <- c(setNames(unique(p_data$all_sample$plateid), unique(p_data$all_sample$plateid))),
                          selected = select_plateid,
                          multiple = FALSE
              )),
              column(9,radioGroupButtons(
                inputId = "gaterxMap_antigen",
                label = "Choose Antigen",
                choices = antigen_list,
                status = "success"
              ))
            ),
         # plot row
            fluidRow(
              # slider
              column(1,
                     fluidRow(
                       numericInput(paste0("sample_slider_upper_textinput"), "Upper Limit",
                                    min = 0,
                                    step = 1,
                                    max = as.numeric(pa_data$max_common),
                                    value = as.numeric(pa_data$min_max[2])
                       ),
                       noUiSliderInput(
                         inputId =  sample_slider_id, label = "Limits",
                         min = 0, max = as.numeric(pa_data$max_common), step = 1,
                         value = pa_data$min_max[1:2], margin = 1,
                         orientation = "vertical",
                         direction = "rtl",
                         width = "100px", height = "200px"
                       ),
                       numericInput(paste0("sample_slider_lower_textinput"), "Lower Limit",
                                    min = 0,
                                    step = 1,
                                    max = as.numeric(pa_data$max_common),
                                    value = pa_data$min_max[1]
                       )
                     )
              ),
              # sample_plot
              column(3,
                     fluidRow(
                       plotlyOutput(paste0("sample_plot")),
                       br(),
                       actionButton(paste0("update_gate_class"), "Save Sample Limits")
                     )                              ),
              # density_plot
              column(4,
                     fluidRow(
                       plotOutput(paste0("density_plot"))
                     )
              ),
              # #buffer_control_plot
              # column(3,
              #        fluidRow(
              #          plotOutput(paste0("buffer_control_plot"))
              #        )
              # ),
              #standard_plot
              column(4, fluidRow(
                conditionalPanel(
                  condition = "make_standard_plot=='Standard'",
                  plotOutput(paste0("standard_plot")),
                  selectInput("gaterxMap_source",
                              "Choose Standard Curve Source",
                              choices <- standard_source,
                              # selected = "Click here",
                              multiple = FALSE
                              )
                  ) # close conditionalPanel
                )
              ) #close column
            ) # close row of plots
        ) # close tabpanel
    )# close insert tab
})

observeEvent(input$gaterxMap_plateid, {
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession)
  local({
    select_study_accession <- input$readxMap_study_accession
    select_experiment_accession <- input$readxMap_experiment_accession
    select_plateid <- input$gaterxMap_plateid
    local_antigen <- input$gaterxMap_antigen
    print(paste("gaterxMap_plateid",select_plateid,local_antigen))
    create_plot_dat_plate(select_study_accession = select_study_accession,
                          select_experiment_accession = select_experiment_accession,
                          select_plateid = select_plateid
    )
    all_antigen <- p_data$all_sample[p_data$all_sample$antigen==local_antigen, ]
    create_plot_dat_plate_antigen(antigen = local_antigen)
    tryCatch({
      create_sample_plot()
      create_density_plot()
      print(pa_data$standard_status)
      if (pa_data$standard_status == 'Standard Curve Fit') {
        make_standard_plot <- "fitted"
        standard_source <- c(unique(pa_data$standard_antigen$source))
        pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
        print(pa_data$max_common)
        print(paste("before create plot"))
        create_standard_plot()
        pa_data$min_max <- c(pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("llod")],
                             pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("ulod")]
        )
        print(pa_data$min_max)
      } else if (pa_data$standard_status == 'No Standard Curve Fit') {
        make_standard_plot <- "raw"
        standard_source <- c(unique(pa_data$standard_antigen$source))
        pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
        create_standard_plot_raw()
        print(pa_data$min_max)
      } else {
        make_standard_plot <- "none"
        standard_source <- c("NoStandard")
        create_standard_plot_blank()
        print(pa_data$min_max)
      }

      # nrows_fit_antigen <<- nrow(pa_data$hline_fit)
      # nrows_standard_antigen <<- nrow(pa_data$standard_antigen)
      # if (!is.null(nrows_fit_antigen) & !is.na(nrows_fit_antigen)) {
      #   if (nrows_fit_antigen > 0) {
      #     make_standard_plot <- "fitted"
      #     standard_source <- c(unique(pa_data$standard_antigen$source))
      #     pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
      #     create_standard_plot()
      #     pa_data$min_max <- c(pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("llod")],
      #                          pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("ulod")]
      #     )
      #   } else if (nrows_standard_antigen > 0) {
      #     make_standard_plot <- "raw"
      #     standard_source <- c(unique(pa_data$standard_antigen$source))
      #     pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
      #     create_standard_plot_raw()
      #   } else {
      #     make_standard_plot <- "none"
      #     standard_source <- c("NoStandard")
      #     create_standard_plot_blank()
      #   }
        print("updating the sliders - gaterxmap_plateid")
        updateTextInput(session, slider_lower_text_id, value = pa_data$min_max[1])
        updateTextInput(session, slider_upper_text_id, value = pa_data$min_max[2])
        sliderVal <- c(as.numeric(input[[slider_lower_text_id]]), as.numeric(input[[slider_upper_text_id]]))
        updateNoUiSliderInput(session, sample_slider_id, value = sliderVal, range = c(0,pa_data$max_common))
      # }
    }) ### after tryCatch
  }) # after local
})

observeEvent(input$gaterxMap_antigen, {
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession)
  local({
    select_study_accession <- input$readxMap_study_accession
    select_experiment_accession <- input$readxMap_experiment_accession
    select_plateid <- input$gaterxMap_plateid
    local_antigen <- input$gaterxMap_antigen
    print(paste("gaterxMap_antigen",select_plateid,local_antigen))
    create_plot_dat_plate(select_study_accession = select_study_accession,
                          select_experiment_accession = select_experiment_accession,
                          select_plateid = select_plateid
    )
    create_plot_dat_plate_antigen(antigen = local_antigen)
    tryCatch({
      create_sample_plot()
      create_density_plot()
      print("observe gater_xmap_antigen")
      print(pa_data$standard_status)
      if (pa_data$standard_status == 'Standard Curve Fit') {
        make_standard_plot <- "fitted"
        standard_source <- c(unique(pa_data$standard_antigen$source))
        pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
        create_standard_plot()
        pa_data$min_max <- c(pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("llod")],
                             pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("ulod")]
        )
      } else if (pa_data$standard_status == 'No Standard Curve Fit') {
        make_standard_plot <- "raw"
        standard_source <- c(unique(pa_data$standard_antigen$source))
        pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
        create_standard_plot_raw()
      } else {
        make_standard_plot <- "none"
        standard_source <- c("NoStandard")
        create_standard_plot_blank()
      }
        updateTextInput(session, slider_lower_text_id, value = pa_data$min_max[1])
        updateTextInput(session, slider_upper_text_id, value = pa_data$min_max[2])
        sliderVal <- c(as.numeric(input[[slider_lower_text_id]]), as.numeric(input[[slider_upper_text_id]]))
        updateNoUiSliderInput(session, sample_slider_id, value = sliderVal, range = c(0,pa_data$max_common))
      # }
    }) ### after tryCatch
  }) # after local
})

observeEvent(input$gaterxMap_source, {
  tryCatch({
    print("observe_gaterxMap_source")
    print(pa_data$standard_status)
    if (pa_data$standard_status == 'Standard Curve Fit') {
      make_standard_plot <- "fitted"
      standard_source <- c(unique(pa_data$standard_antigen$source))
      pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
      pa_data$min_max <- c(pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("llod")],
                           pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("ulod")]
      )
      updateTextInput(session, slider_lower_text_id, value = pa_data$min_max[1])
      updateTextInput(session, slider_upper_text_id, value = pa_data$min_max[2])
      sliderVal <- c(as.numeric(input[[slider_lower_text_id]]), as.numeric(input[[slider_upper_text_id]]))
      updateNoUiSliderInput(session, sample_slider_id, value = sliderVal, range = c(0,pa_data$max_common))
      create_standard_plot()
    } else if (pa_data$standard_status == 'No Standard Curve Fit') {
      make_standard_plot <- "raw"
      standard_source <- c(unique(pa_data$standard_antigen$source))
      pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
      create_standard_plot_raw()
    } else {
      make_standard_plot <- "none"
      standard_source <- c("NoStandard")
      create_standard_plot_blank()
    }
    # nrows_fit_antigen <- nrow(pa_data$hline_fit)
    # nrows_standard_antigen <- nrow(pa_data$standard_antigen)
    # if (!is.null(nrows_fit_antigen) & !is.na(nrows_fit_antigen)) {
    #   if (nrows_fit_antigen > 0) {
    #     make_standard_plot <- "fitted"
    #     standard_source <- c(unique(pa_data$standard_antigen$source))
    #     pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
    #     pa_data$min_max <- c(pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("llod")],
    #                          pa_data$hline_fit[pa_data$hline_fit$source==input$gaterxMap_source, c("ulod")]
    #                          )
    #     updateTextInput(session, slider_lower_text_id, value = pa_data$min_max[1])
    #     updateTextInput(session, slider_upper_text_id, value = pa_data$min_max[2])
    #     sliderVal <- c(as.numeric(input[[slider_lower_text_id]]), as.numeric(input[[slider_upper_text_id]]))
    #     updateNoUiSliderInput(session, sample_slider_id, value = sliderVal, range = c(0,pa_data$max_common))
    #     create_standard_plot()
    #   } else if (nrows_standard_antigen > 0) {
    #     make_standard_plot <- "raw"
    #     standard_source <- c(unique(pa_data$standard_antigen$source))
    #     pa_data$max_common <- (round_any(max(max(pa_data$sample_antigen$mfi),max(pa_data$control_antigen$mfi),max(pa_data$standard_antigen$mfi)), 500, f = ceiling))
    #     create_standard_plot_raw()
    #   } else {
    #     make_standard_plot <- "none"
    #     standard_source <- c("NoStandard")
    #     create_standard_plot_blank()
    #   }
    # }
  }) ### after tryCatch
})


observeEvent(input[[paste0("update_gate_class")]], {
  local_antigen <- input$gaterxMap_antigen
  print("before initialize update_gate_class")
  update_gate_class <- pa_data$sample_antigen[ , c("xmap_sample_id", "mfi")]
  update_gate_class$gate_class <- NULL
  print("after initialize update_gate_class")
  print(paste0("update_gate_class_", local_antigen))
  if (nrow(update_gate_class[update_gate_class$mfi < input[[sample_slider_id]][1], ]) > 0) {
    update_gate_class[update_gate_class$mfi < input[[sample_slider_id]][1], c("gate_class")] <- "Below_Lower_Limit"
  }
  if (nrow(update_gate_class[update_gate_class$mfi >= input[[sample_slider_id]][1] & update_gate_class$mfi <= input[[sample_slider_id]][2], ]) > 0) {
    update_gate_class[update_gate_class$mfi >= input[[sample_slider_id]][1] & update_gate_class$mfi <= input[[sample_slider_id]][2], c("gate_class")] <- "Between_Limits"
  }
  if (nrow(update_gate_class[update_gate_class$mfi > input[[sample_slider_id]][2], ]) > 0) {
    update_gate_class[update_gate_class$mfi > input[[sample_slider_id]][2], c("gate_class")] <- "Above_Upper_Limit"
  }
  if (dbExistsTable(conn, Id(schema = "madi_results", table = "xmap_tmp_gate_class"))) {
    DBI::dbRemoveTable(conn, Id(schema = "madi_results", table = "xmap_tmp_gate_class"))
  }
  print("store update_gate_class")
  update_gate_class <- update_gate_class
  print("write update_gate_class")
  DBI::dbWriteTable(conn, Id(schema = "madi_results", table = "xmap_tmp_gate_class"), update_gate_class)
  DBI::dbExecute(conn, "UPDATE madi_results.xmap_sample SET gate_class = xmap_tmp_gate_class.gate_class FROM madi_results.xmap_tmp_gate_class WHERE xmap_tmp_gate_class.xmap_sample_id = xmap_sample.xmap_sample_id;")
  showNotification("UPDATED sample gate_class successfully", type = "message")
  if (dbExistsTable(conn, Id(schema = "madi_results", table = "xmap_tmp_gate_class"))) {
    DBI::dbRemoveTable(conn, Id(schema = "madi_results", table = "xmap_tmp_gate_class"))
  }
  # removeTab(inputId = "body_panel_id", target="gaterxMap")
})

create_sample_plot <- function() {
  # output[[paste0("sample_plot_",local_antigen)]] <<- renderPlotly({
  local_antigen <- input$gaterxMap_antigen
  output[[paste0("sample_plot")]] <- renderPlotly({
    # print(paste("renderPlotly_sample_plot_antigen:",paste0("sample_plot_",local_antigen)))
    # print(input[[sample_slider_id]])
    # pa_data$samp_base <- do.call("rbind", list(pa_data$sample_antigen[ , c("mfi","nbeads","sample_type")],
    #       pa_data$buffer_antigen[ , c("mfi","nbeads","sample_type")],
    #       pa_data$control_antigen[ , c("mfi","nbeads","sample_type")]))
    ggplot(pa_data$samp_base, aes(x = factor(sample_type), y = mfi, text = paste0("MFI: ", mfi, "\nnBeads: ", nbeads))) +
      # geom_boxplot(outlier.shape = NA, outlier.size = NA, outlier.colour = NA) +
      # geom_point(aes (x= 1, y = mfi)) +
      geom_jitter(aes(color = nbeads >= 50), shape=21, width = 0.2, alpha = 0.5, show.legend = FALSE) +
      geom_hline(yintercept = input[[sample_slider_id]][1], linetype = "dashed", color = "red") +
      geom_hline(yintercept = input[[sample_slider_id]][2], linetype = "dashed", color = "green") +
      ylim(0, pa_data$max_common) +
      xlab(toupper(local_antigen)) +
      ylab("MFI") +
      theme_bw() +
      theme(text = element_text(size=8),
            legend.title = element_blank(),
            legend.position = "none"
      ) -> p
    ## p<- ggMarginal(p, type="histogram", groupColour = TRUE, groupFill = TRUE)

    sample_plot <- ggplotly(p,
                            height = 400,
                            tooltip = "text"
    ) %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(text = "Sample Values", pad = list(b = -50, t = -50)),
             xaxis=list(title=list(standoff=0))
      )

    sample_plot
  })

}

create_density_plot <- function() {
  # Create density plot
  local_antigen <- input$gaterxMap_antigen
  output[[paste0("density_plot")]] <- renderPlot({
    print(paste0("renderPlot_density_plot_", local_antigen))
    ggplot(pa_data$all_antigen, aes(x = mfi)) +
      geom_density(alpha = 0.3, aes(fill = focal_group)) +
      xlim(0, pa_data$max_common) +
      ylab(toupper(local_antigen)) +
      # geom_vline(xintercept = quantile(sample_antigen$mfi)[2], linetype = "dashed") +
      geom_vline(data = pa_data$ldata_antigen, aes(xintercept = median, color=focal_group), linetype = "solid") +
      geom_vline(data = pa_data$reference_lines, aes(xintercept = yinterc, linetype = factor(reflintype), color = factor(control_type))) +
      # geom_vline(aes(color = focal_group), xintercept = quantile(sample_antigen$mfi)[4], linetype = "dashed") +
      scale_colour_manual(values = color_groups,
                          labels = c('Buffer','Control','Median:Focal plate','Median:Other plates'),
                          name = "Key") +
      scale_linetype_manual(values = 1:7,
                            labels = as.list(pa_data$reference_lines$type_labels),
                            name = "Type") +
      scale_fill_manual(values = focal_plate_color,
                        name = "Plate Density"
                        # labels = c('Focal plate','Other plates')
                        ) +
      coord_flip() +
      theme_minimal() +
      theme(text = element_text(size=8),
            legend.title = element_text(size=8),
            legend.position = "right"
      )
  }, height = 400)

  # output[[paste0("buffer_control_plot_", local_antigen)]] <<- renderPlot({
  # output[[paste0("buffer_control_plot")]] <<- renderPlot({
  #   print(paste("renderPlot_buffer_control_plot_antigen:",paste0("buffer_control_plot_",local_antigen)))
    # if(nrow(pa_data$control_antigen) == 0){
    #   return(blank_plot("Buffer/Control"))
    # }
    # # pa_ref_line <<- pa_data$reference_lines
    # ggplot(pa_data$control_antigen, aes(x = mfi, fill = focal_group)) +
    #   geom_density(alpha = 0.3) +
    #   xlim(0, pa_data$max_common) +
    #   ylab(toupper(local_antigen))
    # ggplot(data = pa_ref_line, aes(x = factor(1), y = yinterc, color = control_type)) +
    #   geom_hline(aes(yintercept = yinterc, linetype = type)) +
    #   geom_point()
    #+
    # scale_colour_manual(values = c("#DD4444", "#555599"),
    #                     labels = c('Buffer','Control'),
    #                     name = "Key") +
    # scale_linetype_manual(values = 1:6,
    #                       labels = as.list(pa_data$reference_lines$type_labels),
    #                       name = "Type") +
    # ylim(0, pa_data$max_common) +
    # xlab(toupper(local_antigen)) +
    # ylab("MFI") +
    # ggtitle("Buffers and Controls") +
    # theme_bw() +
    # theme(
    #   text = element_text(size=10),
    #   legend.title = element_blank()
    #   )
  # }, height = 400)
}

blank_plot <- function(plot_type = NULL) {
  local_antigen <- input$gaterxMap_antigen
  local_source <- input$gaterxMap_source
  local_standard_antigen <- pa_data$standard_antigen[pa_data$standard_antigen$source==local_source, ]
  output[[paste0("standard_plot")]] <- renderPlot({
    print(paste("renderPlot_standard_plot_antigen:",paste0("standard_plot_",local_antigen)))
    ggplot(data = local_standard_antigen,
           aes(x = log_dilution, y = mfi)) +
      geom_blank()+
      ylim(0, pa_data$max_common) +
      xlim(-10, 0) +
      xlab("Dilution (log units)") +
      ylab("MFI") +
      ggtitle("No Standard Curve") +
      theme_bw() +
      theme(text = element_text(size=8) )
  })
}

create_standard_plot <- function() {
  print("in create standard_plot")
  local_antigen <- input$gaterxMap_antigen
  local_source <- input$gaterxMap_source
  local_standard_antigen <- pa_data$standard_antigen[pa_data$standard_antigen$source==local_source, ]
  local_preds_antigen <- pa_data$preds_antigen[pa_data$preds_antigen$source==local_source, ]

  if(nrow(local_standard_antigen) == 0){
    return(blank_plot("Standard"))
  } else if (is.null(nrow(local_preds_antigen)) | nrow(local_preds_antigen)==0) {
    # return(blank_plot("Standard"))
    # local_standard_antigen <- pa_data$standard_antigen[pa_data$standard_antigen$source==local_source, ]
    output[[paste0("standard_plot")]] <- renderPlot({
      print(paste("renderPlot_standard_plot_antigen:",paste0("standard_plot_raw_",local_antigen)))
      ggplot(data = local_standard_antigen,
             aes(x = log_dilution, y = mfi)) +
        geom_point() +
        ylim(0, pa_data$max_common) +
        xlim(min(local_standard_antigen$log_dilution), 0) +
        xlab("Dilution (log units)") +
        ylab("MFI") +
        ggtitle("Standard Data - Not Fitted") +
        theme_bw() +
        theme(text = element_text(size=8) )
    })
  } else {
  local_hline_fit <- pa_data$hline_fit[pa_data$hline_fit$source==local_source, ]
  hline_data <- data.frame( source = c(local_hline_fit$source,
                                       local_hline_fit$source,
                                       local_hline_fit$source,
                                       local_hline_fit$source),
                            y = c(local_hline_fit$llod,
                                  local_hline_fit$bendlower,
                                  local_hline_fit$bendupper,
                                  local_hline_fit$ulod),
                            type = factor(c(1, 2, 2, 1)),
                            ctype = factor(c(1, 2, 3, 4)),
                            labels = c(paste0("Lower LOD: ",local_hline_fit$llod),
                                       paste0("Lower bend: ",local_hline_fit$bendlower),
                                       paste0("Upper bend: ",local_hline_fit$bendupper),
                                       paste0("Upper LOD: ",local_hline_fit$ulod)),
                            stringsAsFactors = FALSE)
  pa_data$hline_data <- hline_data
  local_fit_tab <- pa_data$fit_tab_antigen[pa_data$fit_tab_antigen$source==local_source,]
  print("assembling the fit table")
  t1 <- tableGrob(pa_data$fit_tab_antigen[pa_data$fit_tab_antigen$source==local_source,], theme = ttheme_minimal(base_size = 9, padding = unit(c(2, 2), "mm")),
                  rows = NULL,
                  vp = viewport(x = 0.1, y = 0.1))
  title_standard <- paste("Rsquare: ",
                          round(pa_data$hline_fit[pa_data$hline_fit$source==local_source, ]$rsquare_fit,3),
                          "Residual[df]:",
                          pa_data$hline_fit[pa_data$hline_fit$source==local_source, ]$dfresidual
                          )
  title_text_t1 <- bquote(R^2 * ": " * .(sprintf("%.3f", round(pa_data$hline_fit[pa_data$hline_fit$source==local_source, ]$rsquare_fit,3))) * ", " * Residual[df] * ": " *  .(sprintf("%.0f", pa_data$hline_fit[pa_data$hline_fit$source==local_source, ]$dfresidual)))
  title_t1 <- textGrob(title_text_t1,gp=gpar(fontsize=9))
  padding <- unit(2,"mm")
  fit_table <- gtable_add_rows(
    t1,
    heights = grobHeight(title_t1) + padding,
    pos = 0)
  fit_table <- gtable_add_grob(
    fit_table,
    title_t1,
    1, 1, 1, ncol(fit_table))
  print(paste("antigen:",local_antigen,"fit_table",fit_table))

  # output[[paste0("standard_plot_", local_antigen)]] <<- renderPlot({
  output[[paste0("standard_plot")]] <- renderPlot({
    print(paste("renderPlot_standard_plot_antigen:",paste0("standard_plot_",local_antigen)))

    ggplot(data = local_standard_antigen,
           aes(x = log_dilution, y = mfi)) +
      geom_point() +
      geom_line(data = local_preds_antigen,
                aes(x = log_dilution, y = fitted)) +
      geom_hline(data = pa_data$hline_data,
                 aes(yintercept = y, linetype = type, color = ctype)) +
      scale_colour_manual(values = color_typ,
                          labels = as.list(pa_data$hline_data$labels),
                          name = "Key") +
      scale_linetype_manual(values = 1:2,
                            labels = c("LOD", "Bend"),
                            name = "Type") +
      ylim(0, pa_data$max_common) +
      xlim(min(local_standard_antigen$log_dilution), 0) +
      xlab("Dilution (log units)") +
      ylab("MFI") +
      ggtitle(title_standard) +
      theme_bw() +
      theme(text = element_text(size=8) ) +
      annotation_custom(fit_table,
          xmin=(min(local_standard_antigen$log_dilution)),
          xmax=(0.2*max(local_standard_antigen$log_dilution)),
          ymin=(0.88*pa_data$max_common),
          ymax=(0.96*pa_data$max_common)
          )
  })
  }
  # })
}

create_standard_plot_raw <- function() {
  local_antigen <- input$gaterxMap_antigen
  local_source <- input$gaterxMap_source
  local_standard_antigen <- pa_data$standard_antigen[pa_data$standard_antigen$source==local_source, ]
  output[[paste0("standard_plot")]] <- renderPlot({
    print(paste("renderPlot_standard_plot_antigen:",paste0("standard_plot_raw_",local_antigen)))
    if(nrow(local_standard_antigen) == 0){
      return(blank_plot("Standard"))
    }
    ggplot(data = local_standard_antigen,
           aes(x = log_dilution, y = mfi)) +
      geom_point() +
      ylim(0, pa_data$max_common) +
      xlim(min(local_standard_antigen$log_dilution), 0) +
      xlab("Dilution (log units)") +
      ylab("MFI") +
      ggtitle("Standard Data - Not Yet Fitted") +
      theme_bw() +
      theme(text = element_text(size=8) )
  })
}

create_standard_plot_blank <- function() {
  local_antigen <- input$gaterxMap_antigen
  local_source <- input$gaterxMap_source
  local_standard_antigen <- pa_data$standard_antigen[pa_data$standard_antigen$source==local_source, ]
  output[[paste0("standard_plot")]] <- renderPlot({
    print(paste("renderPlot_standard_plot_antigen:",paste0("standard_plot_blank_",local_antigen)))
      return(blank_plot("Standard"))
  })
}

create_sample_plate_plots <- function () {
  # output[[paste0("sample_plot_",local_antigen)]] <<- renderPlot({
  #   print(paste("sample_plot_",local_antigen,"sample_slider_id:",start_llod,start_ulod))
  #   ggMarginal(ggplot(all_antigen, aes(x = factor(1), y = MFI, color=focal_group)) +
  #     # geom_boxplot(outlier.shape = NA, outlier.size = NA, outlier.colour = NA) +
  #     # geom_point(aes (x= 1, y = MFI)) +
  #     geom_jitter(aes(color = focal_group, stroke = focal_stroke), alpha = 1, shape=21, width = 0.4, show.legend = TRUE) +
  #     geom_hline(data = ldata, aes(yintercept = median, color=focal_group), linetype = "solid") +
  #     geom_hline(yintercept = start_llod, linetype = "dashed", color = "red") +
  #     geom_hline(yintercept = start_ulod, linetype = "dashed", color = "green") +
  #     scale_color_manual(name = "Plate", values = c(focal_plate_color), labels = c("All others","Focal plate")) +
  #     # scale_color_identity(labels = c(red = "Focal plate", gray90 = "Other plates"), guide = "legend")
  #   ylim(0, max_common) +
  #     xlab(toupper(local_antigen)) +
  #     ylab("MFI") +
  #     theme_bw() +
  #     theme(text = element_text(size=8),
  #           legend.title = element_blank(),
  #           legend.position = "top"
  #     ), type="density", groupColour = TRUE, groupFill = TRUE)
  # })

}

create_gated_sample_summary <- function() {
  # output[[paste0("below_sample_ref_plot_", local_antigen)]] <- renderPlot({
  #   if(is.null(sample_antigen)){
  #     return(blank_plot("Sample"))
  #   } else if (nrow(sample_antigen %>% dplyr::filter(mfi < input[[sample_slider_id]][1])) > 0) {
  #     sample <- sample_antigen %>% filter(mfi < input[[sample_slider_id]][1])
  #     ggplot(sample, aes(x = mfi)) +
  #       geom_density() +
  #       theme_bw()
  #   }
  # })
  #
  # output[[paste0("below_sample_ref_summ_", local_antigen)]] <- render_gt({
  #   if(is.null(sample_antigen)){
  #     return(gt::gt(data.frame("Data"="No Data")))
  #   } else if (nrow(sample_antigen %>% dplyr::filter(mfi < input[[sample_slider_id]][1])) > 0) {
  #     sample <- sample_antigen %>% dplyr::filter(mfi < input[[sample_slider_id]][1])
  #     tbl_summary(sample, include = c(mfi), type = list(mfi ~ "continuous")) %>%
  #       add_stat_label() %>%
  #       add_n() %>%
  #       bold_labels() %>%
  #       as_gt() %>%
  #       tab_header(md("Below Sample Limit")) %>%
  #       gt::tab_options(table.font.size = "10px")
  #   }
  # })
  #
  # output[[paste0("between_sample_ref_plot_", local_antigen)]] <- renderPlot({
  #   if(is.null(sample_antigen)){
  #     return(blank_plot("Sample"))
  #   } else if (nrow(sample_antigen %>% filter(mfi >= input[[sample_slider_id]][1] & mfi <= input[[sample_slider_id]][2])) > 0) {
  #     sample <- sample_antigen %>% filter(mfi >= input[[sample_slider_id]][1] & mfi <= input[[sample_slider_id]][2])
  #     ggplot(sample, aes(x = mfi)) +
  #       geom_density() +
  #       theme_bw()
  #   }
  # })
  #
  # output[[paste0("between_sample_ref_summ_", local_antigen)]] <- render_gt({
  #   if(is.null(sample_antigen)){
  #     return(gt::gt(data.frame("Data"="No Data")))
  #   } else if (nrow(sample_antigen %>% filter(mfi >= input[[sample_slider_id]][1] & mfi <= input[[sample_slider_id]][2])) > 0) {
  #     sample <- sample_antigen %>% filter(mfi >= input[[sample_slider_id]][1] & mfi <= input[[sample_slider_id]][2])
  #     tbl_summary(sample, include = c(mfi), type = list(mfi ~ "continuous")) %>%
  #       add_stat_label() %>%
  #       add_n() %>%
  #       bold_labels() %>%
  #       as_gt() %>%
  #       tab_header(md("Between Sample Limits")) %>%
  #       gt::tab_options(table.font.size = "10px")
  #   }
  # })
  #
  # output[[paste0("above_sample_ref_plot_", local_antigen)]] <- renderPlot({
  #   if(is.null(sample_antigen)){
  #     return(blank_plot("Sample"))
  #   } else if (nrow(sample_antigen %>% filter(mfi > input[[sample_slider_id]][2])) > 0) {
  #     sample <- sample_antigen %>% filter(mfi > input[[sample_slider_id]][2])
  #     ggplot(sample, aes(x = mfi)) +
  #       geom_density() +
  #       theme_bw()
  #   }
  # })
  #
  # output[[paste0("above_sample_ref_summ_", local_antigen)]] <- render_gt({
  #   if(is.null(sample_antigen)){
  #     return(gt::gt(data.frame("Data"="No Data")))
  #   } else if (nrow(sample_antigen %>% filter(mfi > input[[sample_slider_id]][2])) > 0) {
  #     sample <- sample_antigen %>% filter(mfi > input[[sample_slider_id]][2])
  #     tbl_summary(sample, include = c(mfi), type = list(mfi ~ "continuous")) %>%
  #       add_stat_label() %>%
  #       add_n() %>%
  #       bold_labels() %>%
  #       as_gt() %>%
  #       tab_header(md("Above Sample Limit")) %>%
  #       gt::tab_options(table.font.size = "10px")
  #   }
  # })

}
