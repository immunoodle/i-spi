### Functions for nonlinear standard curve fitting and visualization
## R package by Seamus, Scot, and Annie

# This function returns the lower and upper constraints for an antigen given its method
# methods are  ['default','user_defined','range_of_blanks', 'geometric_mean_of_blanks']

### The Standard Curve MFI is already logged transformed but the blanks are not
obtain_lower_constraint <- function(dat, antigen, study_accession, experiment_accession, plate, plateid, plate_blanks, antigen_constraints) {
  antigen_constraints$l_asy_constraint_method <- trimws(antigen_constraints$l_asy_constraint_method)
  # blank_data_plate <- blank_data[blank_data$plate == plate & blank_data$antigen == antigen,]
  if (nrow(plate_blanks) > 1) {
    se_blank_mfi <- sd(plate_blanks$mfi, na.rm = TRUE) / sqrt(sum(!is.na(plate_blanks$mfi)))
  } else {
    se_blank_mfi <- 0
  }

  if (antigen_constraints$l_asy_constraint_method == "user_defined") {
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = antigen_constraints$l_asy_min_constraint,
      l_asy_max_constraint = antigen_constraints$l_asy_max_constraint,
      l_asy_constraint_method = antigen_constraints$l_asy_constraint_method,
      std_error_blank = se_blank_mfi,
      standard_curve_concentration =  antigen_constraints$standard_curve_concentration,
      pcov_threshold = antigen_constraints$pcov_threshold
    )
  } else if (antigen_constraints$l_asy_constraint_method == "default") {
    l_asy_max_constraint_dat <- max(dat$mfi, na.rm = TRUE)
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = 0, # lower bound is set to 0
      l_asy_max_constraint = max(dat$mfi, na.rm = T),
      l_asy_constraint_method = antigen_constraints$l_asy_constraint_method,
      std_error_blank = se_blank_mfi,
      standard_curve_concentration =  antigen_constraints$standard_curve_concentration,
      pcov_threshold = antigen_constraints$pcov_threshold
    )
  } else if (antigen_constraints$l_asy_constraint_method == "range_of_blanks") {
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = min(plate_blanks$mfi),
      l_asy_max_constraint = max(plate_blanks$mfi),
      l_asy_constraint_method = antigen_constraints$l_asy_constraint_method,
      std_error_blank =  se_blank_mfi,
      standard_curve_concentration =  antigen_constraints$standard_curve_concentration,
      pcov_threshold = antigen_constraints$pcov_threshold
    )
  } else if (antigen_constraints$l_asy_constraint_method == 'geometric_mean_of_blanks') {
    geometric_mean <- exp(mean(log(plate_blanks$mfi), na.rm = TRUE))
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      #plateid = plateid,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = geometric_mean,
      l_asy_max_constraint = geometric_mean,
      l_asy_constraint_method = antigen_constraints$l_asy_constraint_method,
      std_error_blank = se_blank_mfi,
      standard_curve_concentration =  antigen_constraints$standard_curve_concentration,
      pcov_threshold = antigen_constraints$pcov_threshold
    )
  } else {
    return(NULL)
  }
  return(l_asy_constraints)
}

# 1.	get_study_exp_antigen_plate_params return standard curve concentration for undiluted standard curve sample.
# Result is a number (10000 for example) and is passed in to compute concentration (undiluted_sc_concentration).
get_study_exp_antigen_plate_param <- function(l_asy_constraints) {
  undiluted_sc_concentration <- l_asy_constraints$standard_curve_concentration
  return(undiluted_sc_concentration)
}

## get the standard error of the blanks for later use
get_blank_se <- function(antigen_settings) {
  std_error_blank <- antigen_settings$std_error_blank
  return(std_error_blank)
}

# 2. get_study_params return study parameters, such as the blank options, prozone correction, aggrigate repeated measures etc


# 3. compute concentration column  with an option to log10 the concentration
# the independent variable is given to be a string (which is usually concentration)
# read in the undiluted standard curve sample's concentration value
compute_concentration <- function(data,
                                  undiluted_sc_concentration,
                                  independent_variable,
                                  is_log_concentration = TRUE) {
  independent_variable <- unique(independent_variable)
  data[[independent_variable]] <- (1 / data$dilution) * undiluted_sc_concentration

  if (is_log_concentration) {
    data[[independent_variable]] <- log10(data[[independent_variable]])
  }

  return(data)
}

##  4. Correct for the Prozone Effect
### Prozone Correction function
correct_prozone <- function(stdframe = NULL, prop_diff = NULL, dil_scale = NULL,
                            response_variable = "mfi",
                            independent_variable = "concentration",
                            verbose = TRUE) {
  ## stdframe must contain the columns labelled mfi and log_dilution for one set of standard curve data i.e. one dilution series
  ##
  ### correct for prozone effect by correcting the values past the peak by raising them to a neutral asymptote based on an assumed measured C90.
  ## ACS Meas. Sci. Au 2024, 4, 4, 452–458
  ## https://pubs.acs.org/doi/10.1021/acsmeasuresciau.4c00010
  ## The hook effect, also known as the prozone effect, is a phenomenon that commonly occurs in
  ## antibody-based sandwich immunoassay biosensors. (1,2) In a typical immunoassay, the binding
  ## of antibodies to the analyte leads to the formation of a visible signal, such as a color
  ## change or a fluorescent signal where the intensity of this signal is directly proportional
  ## to the concentration of the analyte in the sample being tested. (3,4) However, the prozone
  ## effect happens when the concentration of the analyte becomes so high that it exceeds the
  ## capacity of the antibodies in the assay. (5) In this situation, the excess analyte can
  ## saturate or overwhelm the binding sites on the antibodies, and as a result, the sensor
  ## response is inhibited, leading to a false-low or even false-negative test result. (6)
  ##
  ## 1 Chen, W.; Shan, S.; Peng, J.; Liu, D.; Xia, J.; Shao, B.; Lai, W. Sensitive and hook effect-free lateral flow assay integrated with cascade signal transduction system. Sens. Actuators, B 2020, 321, 128465,  DOI: 10.1016/j.snb.2020.128465
  ## 2 Selby, C. Interference in immunoassay. Ann. Clin. Biochem. 1999, 36, 704– 721,  DOI: 10.1177/000456329903600603
  ## 3 Hessick, E. R.; Dannemiller, K.; Gouma, P. Development of a Novel Lateral Flow Immunoassay for Detection of Harmful Allergens Found in Dust. Meet. Abstr. 2022, 241, 2333,  DOI: 10.1149/ma2022-01552333mtgabs
  ## 4 Poudineh, M.; Maikawa, C. L.; Ma, E. Y.; Pan, J.; Mamerow, D.; Hang, Y.; Baker, S. W.; Beirami, A.; Yoshikawa, A.; Eisenstein, M. A fluorescence sandwich immunoassay for the real-time continuous detection of glucose and insulin in live animals. Nat. Biomed. Eng. 2020, 5, 53– 63,  DOI: 10.1038/s41551-020-00661-1
  ## 5 Bravin, C.; Amendola, V. Wide range detection of C-Reactive protein with a homogeneous immunofluorimetric assay based on cooperative fluorescence quenching assisted by gold nanoparticles. Biosens. Bioelectron. 2020, 169, 112591,  DOI: 10.1016/j.bios.2020.112591
  ## 6 Raverot, V.; Perrin, P.; Chanson, P.; Jouanneau, E.; Brue, T.; Raverot, G. Prolactin immunoassay: does the high-dose hook effect still exist?. Pituitary 2022, 25, 653– 657,  DOI: 10.1007/s11102-022-01246-8
  ##

  ## following modelling of prozone effects in:
  ## Development of an experimental method to overcome the hook effect in sandwich-type lateral flow immunoassays guided by computational modelling
  ## Sensors and Actuators B: Chemical Volume 324, 1 December 2020, 128756
  ## and
  ## Hook effect detection and detection-range-controllable one-step immunosensor for inflammation monitoring
  ## Sensors and Actuators B: Chemical Volume 304, 1 February 2020, 127408

  # 0. Filter out NA mfi and log dilution
  response_variable <- unique(response_variable)
  stdframe <- stdframe[!is.na(stdframe[[response_variable]]) & !is.na(stdframe[[independent_variable]]),]


  # 1. identify the highest mfi and corresponding log_dilution in stdframe
  max_response <- max(stdframe[[response_variable]], na.rm = TRUE)
  logc_at_max_response <- max(stdframe[stdframe[[response_variable]]==max_response, ][[independent_variable]])
  if (verbose) cat("Peak MFI =", max_response, "at concentration =", logc_at_max_response, "\n")

  post_peak <- stdframe[[independent_variable]] > logc_at_max_response
  if (verbose) cat("Number of points beyond the peak:", sum(post_peak), "\n")
  # 2. identify the mfis lower than the max_response at higher concentrations and dampen the delta mfis to compensate for
  stdframe[stdframe[[independent_variable]] > logc_at_max_response, ][[response_variable]] <- max_response +
    (
      (max_response - stdframe[stdframe[[independent_variable]] > logc_at_max_response, ][[response_variable]]) * prop_diff /
        ((stdframe[stdframe[[independent_variable]] > logc_at_max_response, ][[independent_variable]]-logc_at_max_response) * dil_scale)
    )
  return(stdframe)
  ### end correct for prozone effect
}

## 5. Blank Handling
#helper function to compute geometric mean
geom_mean <- function (x, na.rm = TRUE) {
  ans <- exp(mean(log(x), na.rm = TRUE))
  ans
}

# Include Blanks as an extra point in the standard curve data
# Estimation of the standard curve takes into account the mean of the background
# of the values as another point of the standard curve. The median fluorescence intensity and the
# expected concentration for this new point by analyte is estimated as follows:
# MFI: geometric mean value of the blank controls.
# EC: the minimum expected concentration value of the standard points divided by 2.
# On the log dilution scale we subtract log10(2) which is equivalent to dividing by 2.
include_blanks_conc <- function(blank_data, data, response_variable, independent_variable = "concentration") {
  data <- data[, !(names(data) %in% c("dilution_factor", "log_dilution"))]

  #plateid, antigen, response_variable, independent_variable = "concentration") {
  # # filter the plate and antigen from the buffer and standard curve data
  # buffer_data_filtered <- buffer_data[buffer_data$plateid == plateid & buffer_data$antigen == antigen, ]
  # std_curve_data_filtered <- std_curve_data[std_curve_data$plateid == plateid & std_curve_data$antigen == antigen,]
  #
  # # calculate the geometric mean of the buffer/blanks by analyte
  # if (is_log_response) {
  #   response_blank <- log10(geom_mean(blank_data[[response_variable]]))
  # } else {
  response_blank <- geom_mean(blank_data[[response_variable]])

  # }



  # calculate the log dilution of the buffer (Dr Lumi uses (1/(min(dilution_factor))/2)
  # cat("\nIn include blanks\n")
  # print(head(std_curve_data_filtered))
  # the minimum expected concentration value of the standard points divided by 2.
  min_concentration <- min(data[[independent_variable]], na.rm = T)
  conc_blank <- min_concentration - log10(2)

  #conc_blank <- min_concentration / 2
  #
  # min_log_dilution <- min(std_curve_data_filtered$log_dilution)
  # log_dilution_buffer <- min_log_dilution - log10(2)
  # min_dilution_factor <- min(std_curve_data_filtered$dilution)
  #
  #data$antibody_mfi <- data$mfi

  # Create new blank/mean point
  new_point <- tibble::tibble(
    study_accession = unique(data$study_accession),
    experiment_accession = unique(data$experiment_accession),
    feature = unique(data$feature),
    source = unique(data$source),
    plateid = unique(data$plateid),
    plate  =  unique(data$plate),
    stype = "B", # blanks are B and recognized in standard curve plot as such
    sample_dilution_factor = unique(data$sample_dilution_factor),
    sampleid = "blank_mean",
    well = "geometric_mean_blank",
    dilution = NA_real_,
    antigen = unique(data$antigen),
    !!response_variable := response_blank,
    assay_response_variable = unique(data$assay_response_variable),
    assay_independent_variable = unique(data$assay_independent_variable),
    concentration = conc_blank

  )



  # new_point <- data.frame(
  #   plateid = unique(data$plateid),
  #   antigen = unique(data$antigen),
  #   mfi = response_blank,
  #   study_accession = unique(data$study_accession),
  #   experiment_accession = unique(data$experiment_accession),
  #   well = "geometric_mean_buffer",
  #   stype = unique(blank_data$stype),
  #   sampleid = "buffer_mean",
  #   source = unique(data$source),
  #   dilution = NA_real_,
  #   pctaggbeads = NA_real_,
  #   samplingerrors = NA_character_,
  #   n = NA_integer_,
  #   feature = unique(data$feature),
  #   predicted_mfi = response_blank,
  #   #selected_str = unique(data$selected_str),
  #   concentration = conc_blank
  # )


  # cat("\nnames of standard curve filtered\n")
  # print(names(std_curve_data_filtered))
  # cat("\n names of  new point\n")
  # print(names(new_point))
  #
  data_with_blank <- rbind(data, new_point)
  return(data_with_blank)

}

# The geometric mean or a multiple of the geometric mean of the blank controls is subtracted from all the standard points if that options are selected.
# pass in the buffer data, standards as data
# blank_option: ignored,included,subtracted,subtracted_3x,subtracted_10x
perform_blank_operation <- function(blank_data, data, response_variable, independent_variable, is_log_response, blank_option = "ignored", verbose = TRUE) {
  if (verbose) {
    message("Blank Option Used: ", blank_option)
  }
  valid_options <- c("ignored","included","subtracted","subtracted_3x","subtracted_10x")

  if (!(blank_option %in% valid_options)) {
    message("Invalid value for blank_option. Must be one of: 'ignored', 'included', 'subtracted', 'subtracted_3x', or 'subtracted_10x'.")
    return(data)
  }

  if (blank_option != "ignored" && (is.null(blank_data) || nrow(blank_data) == 0)) {
    message("Blank data must be supplied when blank_option is not 'ignored'.")
    return(data)
  }
  if (blank_option == "included") {
    data <- include_blanks_conc(blank_data = blank_data, data = data, response_variable = response_variable,
                                independent_variable = independent_variable)

    if (verbose) message("Geometric mean of blanks included as an extra point in the standard curve.")

  }
  if (blank_option %in% c("subtracted", "subtracted_3x", "subtracted_10x")) {

    factor <- switch(blank_option,
                     "subtracted" = 1,
                     "subtracted_3x" = 3,
                     "subtracted_10x" = 10)

    # if (is_log_response) {
    #   data_linear <- 10^(data[[response_variable]])
    #   blank_linear <- geom_mean(blank_data[[response_variable]])
    #
    #   adjusted_lin <- data_linear - factor * blank_linear
    #
    #   smallest_pos <- min(data_linear[data_linear > 0], na.rm = TRUE)
    #   floor_val <- smallest_pos * 0.1   # e.g. 10% of smallest positive observed
    #
    #   adjusted_lin[adjusted_lin <= 0] <- floor_val
    #   # if (any(adjusted_lin <= 0, na.rm = TRUE)) {
    #   #   message("Setting values <= 0 after subtraction to a small positive number before log-transforming.")
    #   #   adjusted_lin[adjusted_lin <= 0] <- min(adjusted_lin[adjusted_lin > 0], na.rm = TRUE) * 1e-3
    #   # }
    #   # adjusted_lin[adjusted_lin < 0] <- 0  # prevent negatives
    #
    #   # re-log after subtraction
    #   data[[response_variable]] <- log10(adjusted_lin)
    #   if (verbose) {
    #     message("Performed blank subtraction (×", factor, ") in linear space, then log-transformed back.")
    #   }
    # } else {
    # Direct subtraction in linear space
    blank_mean <- geom_mean(blank_data[[response_variable]])
    dat <- data
    data[[response_variable]] <- data[[response_variable]] - factor * blank_mean

    if (is_log_response) {
      data[data[[response_variable]] < 0, response_variable] <- 1
    } else {
      data[data[[response_variable]] < 0, response_variable] <- 0
    }
    if (verbose) {
      message("Performed blank subtraction (×", factor, ") in linear space.")
    }
    # }



  }


  return(data)
}

# 6.  compute_log_response logs the response variable for fitting if flag is set to true
# the response variable is set to be a given string such as mfi
# is log response is a boolean flag to decide to take log response if true or not if false.
compute_log_response <- function(data, response_variable, is_log_response = TRUE) {
  if (is_log_response) {
    data[[response_variable]] <- log10(data[[response_variable]])
  }

  return(data)
}

##### Model Fitting
# determine if it is a fixed constraint
# pass in constraint list on the lower asymptote and return the value of the constraint if it is fixed else NULL
test_fixed_lower_asymptote <- function(l_asy_constraints) {
  if (l_asy_constraints$l_asy_min_constraint == l_asy_constraints$l_asy_max_constraint) {
    fixed_constraint <- l_asy_constraints$l_asy_min_constraint
    return(fixed_constraint)
  }
  else  {
    return(NULL)
  }
}

generate_start <- function(bounds, frac = 0.90) {
  start_offset <- (1-frac)/2 # 0.05 by default
  lower <- bounds$lower
  upper <- bounds$upper
  if (!all(names(lower) == names(upper))) {
    stop("Lower and upper bounds must have identical parameter names")
  }

  # start_lower <-  lower  * (1 + start_offset) #frac * (upper - lower)
  # start_upper <- upper * (1- start_offset) #(1- frac) * (upper - lower)

  width <- upper - lower

  start_lower <- lower + start_offset * width
  start_upper <- upper - start_offset * width

  start_list <- list(
    start_lower = start_lower,
    start_upper = start_upper

  )
  return(start_list)

}

## Functions for upper and lower bounds in fitting
.make_bounds <- function(param_names, lower_vals, upper_vals) {
  if (length(param_names) != length(lower_vals) || length(param_names) != length(upper_vals)) {
    stop("Lengths must match")
  }
  # lower <- setNames(as.numeric(lower_vals), param_names)
  # upper <- setNames(as.numeric(upper_vals), param_names)
  list(lower = lower_vals, upper = upper_vals)
}

#Utility to compute middle-90% bounds of y
.y_mid_bounds <- function(ymin, ymax) {
  span <- ymax - ymin
  low <- ymin + 0.05 * span
  high <- ymin + 0.95 * span
  c(low = low, high = high)
}

# obtain the free parameters names in each model
# dependent variable is mfi, independent variable is concentration
# free variables are returned in alphabetical order
obtain_free_variables <- function(formulas, dep = "mfi", indep = "concentration") {
  lapply(formulas, function(f) {
    vars <- all.vars(f)
    sort(setdiff(vars, c(dep, indep)))
  })
}

#obtain the response variable name for all the models.
# all formulas should have the same response variable
obtain_response_variable <- function(formulas) {
  response_vars <- sapply(formulas, function(f) {
    vars <- all.vars(f)
    # response_idx <- attr(stats::terms(f), "response")
    # vars[response_idx]
    as.character(f[[2]])
  },
  USE.NAMES = TRUE)

  response_variable <- unique(response_vars)
  return(response_variable)
}

# response variable based on formula, researcher provides independent variable for obtain_free_variables called internally
obtain_model_constraints <- function(data, formulas,
                                     response_variable,
                                     independent_variable,
                                     is_log_response,
                                     is_log_concentration,
                                     antigen_settings,
                                     max_response,
                                     min_response,
                                     verbose = TRUE) {
  free_variables <- obtain_free_variables(formulas = formulas, dep = response_variable , indep = independent_variable)
  # nls 5
  Y5_nls_constraint <- Y5_safe_constraint(data = data,
                                          y_min = min_response,
                                          y_max = max_response,
                                          Y5_formula = formulas$Y5,
                                          Y5_free_vars = free_variables$Y5,
                                          is_log_response = is_log_response,
                                          is_log_concentration = is_log_concentration,
                                          antigen_settings = antigen_settings
  )
  # drda_5 Hill
  Yd5_constraint <- Yd5_safe_constraint(data = data, y_min = min_response, y_max = max_response, Yd5_formula = formulas$Yd5,
                                        Yd5_free_vars = free_variables$Yd5, is_log_response = is_log_response, is_log_concentration = is_log_concentration,
                                        antigen_settings = antigen_settings)
  # nls_4
  Y4_nls_constraint <- Y4_safe_constraint(data = data, y_min = min_response, y_max = max_response, Y4_formula = formulas$Y4,
                                          Y4_free_vars = free_variables$Y4, is_log_response = is_log_response, is_log_concentration = is_log_concentration,
                                          antigen_settings = antigen_settings)
  #nlslm_4 Hill
  Yd4_constraint <- Yd4_safe_constraint(data = data, y_min = min_response, y_max = max_response, Yd4_formula = formulas$Yd4,
                                        Yd4_free_vars = free_variables$Yd4, is_log_response = is_log_response, is_log_concentration = is_log_concentration,
                                        antigen_settings = antigen_settings)
  #Ygomp4
  Ygomp4_constraint <- Ygomp4_safe_constraint(data = data, y_min = min_response, y_max = max_response, Ygomp4_formula = formulas$Ygomp4,
                                              Ygomp4_free_vars = free_variables$Ygomp4, is_log_response = is_log_response, is_log_concentration = is_log_concentration,
                                              antigen_settings = antigen_settings)
  constraint_models <- list(
    Y5 = Y5_nls_constraint,
    Yd5 = Yd5_constraint,
    Y4 = Y4_nls_constraint,
    Yd4 = Yd4_constraint,
    Ygomp4 = Ygomp4_constraint
  )
  if (verbose) {
    print(constraint_models)
  }

  return(constraint_models)
}

make_start_lists <- function(model_constraints,
                             frac_generate = 0.8,
                             quants = c(low = 0.2, mid = 0.5, high = 0.8)) {
  # base starting values from the constraints
  start_values <- lapply(model_constraints, function(x) generate_start(x, frac = frac_generate))

  # set $start values based on a quantile
  average_start_list <- function(start_values, quantile = 0.5) {
    lapply(start_values, function(x) {
      x$start <- x$start_lower + quantile *(x$start_upper - x$start_lower)
      x
    })
  }

  nlsLMstart_low <- average_start_list(start_values = start_values, quantile = as.numeric(quants["low"]))
  nlsLMstart_mid <- average_start_list(start_values = start_values, quantile = as.numeric(quants["mid"]))
  nlsLMstart_high <- average_start_list(start_values = start_values, quantile = as.numeric(quants["high"]))

  start_list <- list(
    low  = nlsLMstart_low,
    mid  = nlsLMstart_mid,
    high = nlsLMstart_high
  )

  return(start_list)
}


compute_robust_curves <- function(prepped_data,
                                  response_variable,
                                  independent_variable,
                                  formulas,
                                  model_constraints,
                                  start_lists,
                                  verbose = TRUE) {
  models_fit_list <- list()
  for(formula in names(formulas)) {
    name <- formula
    if (verbose) {
      cat("\n model name:")
      print(name)
      cat("\n\n")
    }
    if (verbose) message("\n Trying model: ", name)
    lower_model_constraints <- model_constraints[[name]]$lower
    upper_model_constraints <- model_constraints[[name]]$upper
    # extract starts for current formula
    nlsLM_start_lists <- list(
      low  = start_lists$low[[name]]$start,
      mid  = start_lists$mid[[name]]$start,
      high = start_lists$high[[name]]$start
    )
    best_fit <- select_nlsLM_aic(prepped_data = prepped_data,
                                 response_variable = response_variable,
                                 independent_variable = independent_variable,
                                 formula = formulas[[name]],
                                 lower_model_constraints = lower_model_constraints,
                                 upper_model_constraints = upper_model_constraints,
                                 start_lists = nlsLM_start_lists,
                                 verbose = verbose
    )
    if (!is.null(best_fit)) {
      models_fit_list[[name]] <- list(fit = best_fit, data = prepped_data)
    } else {
      if (verbose) message("Model ", name, " failed; trying next formula.")
    }

  }
  # if (verbose) message("All formulas failed. Returning NULL.")
  return(models_fit_list)
}

select_nlsLM_aic <- function(prepped_data,
                             response_variable,
                             independent_variable,
                             formula,
                             lower_model_constraints,
                             upper_model_constraints,
                             start_lists,  verbose = TRUE) {


  fits <- lapply(names(start_lists), function(nm) {
    if (verbose) message("Fitting with start: ", nm)

    fit_obj <- tryCatch(
      {
        nlsLM_fit(
          formula      = formula,
          data         = prepped_data,
          start_values = start_lists[[nm]],
          lower        = lower_model_constraints,
          upper        = upper_model_constraints,
          verbose      = verbose
        )
      },
      error = function(e) {
        if (verbose) message("  Start '", nm, "' failed: ", e$message)
        NULL
      }
    )

    fit_obj
  })

  names(fits) <- names(start_lists)

  #fits_v <<- fits

  fits <- Filter(Negate(is.null), fits)

  if (length(fits) == 0) {
    if (verbose) message("  All starts failed for this formula.")
    return(NULL)
  }

  aic_vals <- sapply(fits, function(x) AIC(x))

  # 2. Find the best (lowest AIC)
  best_name <- names(which.min(aic_vals))
  if (verbose) {
    message("Best fit")
    print(best_name)
    print(aic_vals)
  }
  best_fit  <- fits[[best_name]]


  return(best_fit)


}

nlsLM_fit <- function(formula, data, start_values, lower = -Inf, upper = Inf, verbose = TRUE) {
  library(minpack.lm)
  if (verbose) {
    message("nlsLM lower constraints")
    print(lower)
    message("nlsLM start values")
    print(start_values)
    message("nlsLM upper constraints")
    print(upper)
  }
  # start_v <<- start_values
  fit <- tryCatch({
    minpack.lm::nlsLM(
      formula = formula,
      data    = data,
      start   = start_values,
      lower   = lower,
      upper   = upper,
      control = nls.lm.control(maxiter = 200)
    )
  }, error = function(e) {
    if (verbose) message("nlsLM failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(fit) && verbose)  {
    message("Fit successful.")
    print(fit)
  }
  return(fit)

}

summarize_model_fits <- function(models_fit_list,
                                 model_names = c("Y5","Yd5","Y4","Yd4","Ygomp4"),
                                 verbose = TRUE) {
  # Ensure all 5 models appear in the summary, even if not fit
  all_models <- unique(c(model_names, names(models_fit_list)))

  summary_list <- lapply(all_models, function(mname) {
    fit_obj <- models_fit_list[[mname]]$fit %||% models_fit_list[[mname]]  # just in case stored differently

    if (is.null(fit_obj) || !inherits(fit_obj, "nls")) {
      return(data.frame(
        model       = mname,
        converged   = FALSE,
        rss         = NA_real_,
        df_resid    = NA_integer_,
        n_params    = NA_integer_,
        AIC         = NA_real_,
        BIC         = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    # Residual sum of squares
    rss_val <- tryCatch({
      sum(residuals(fit_obj)^2)
    }, error = function(e) NA_real_)

    # Number of parameters: length of coefficient vector
    n_params <- tryCatch({
      length(coef(fit_obj))
    }, error = function(e) NA_integer_)

    # Degrees of freedom (n - p); nls objects have df.residual
    df_resid <- tryCatch({
      df.residual(fit_obj)
    }, error = function(e) NA_integer_)

    aic_val <- tryCatch(AIC(fit_obj), error = function(e) NA_real_)
    bic_val <- tryCatch(BIC(fit_obj), error = function(e) NA_real_)

    data.frame(
      model       = mname,
      converged   = TRUE,
      rss         = rss_val,
      df_resid    = df_resid,
      n_params    = n_params,
      AIC         = aic_val,
      BIC         = bic_val,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summary_list)
}

# small helper (if not already defined)
`%||%` <- function(a, b) if (!is.null(a)) a else b

summarize_model_parameters <- function(models_fit_list,
                                       level = 0.95,
                                       model_names = c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4"),
                                       verbose = TRUE) {
  # level: confidence level for intervals (default 95%)
  # Ensure all 5 models appear in the summary, even if not fit
  all_models <- unique(names(models_fit_list))

  summary_list <- lapply(all_models, function(mname) {
    fit_obj <- models_fit_list[[mname]]$fit %||% models_fit_list[[mname]]
    # just in case stored differently

    if (is.null(fit_obj) || !inherits(fit_obj, "nls")) {
      return(data.frame(
        model     = mname,
        parameter = NA,
        estimate  = NA_real_,
        conf.low  = NA_real_,
        conf.high = NA_real_,
        converged   = FALSE,
        stringsAsFactors = FALSE
      ))
    }

    # Safely try to get confidence intervals
    ci <- tryCatch(
      {
        nlstools::confint2(fit_obj, level = level)
      },
      error = function(e) {
        # If confint fails (e.g., non-converged Hessian), fall back to NA CIs
        NULL
      }
    )

    if (verbose) {
      cat("confint2 output:")
      print(mname)
      print(ci)
      cat("\n\n")
    }


    coefs <- coef(fit_obj)
    par_names <- names(coefs)

    if (!is.null(ci)) {
      # confint returns a matrix with rows as parameters
      # Ensure rownames align with coefs
      ci <- ci[par_names, , drop = FALSE]
      conf.low  <- ci[, 1]
      conf.high <- ci[, 2]
    } else {
      conf.low  <- rep(NA_real_, length(coefs))
      conf.high <- rep(NA_real_, length(coefs))
    }

    data.frame(
      model     = mname,
      parameter = par_names,
      estimate  = as.numeric(coefs),
      conf.low  = as.numeric(conf.low),
      conf.high = as.numeric(conf.high),
      converged = TRUE,
      stringsAsFactors = FALSE
    )
  })

  if (verbose) {
    message("Summarized Parameters completed")
  }
  do.call(rbind, summary_list)

}

select_model_fit_AIC <- function(fit_summary,
                                 fit_robust_lm,
                                 fit_params,
                                 plot_data,
                                 verbose = TRUE) {
  selected_model_name <- fit_summary[which.min(fit_summary$AIC),]$model
  selected_fit <- fit_robust_lm[[selected_model_name]]$fit
  selected_data <-  fit_robust_lm[[selected_model_name]]$data
  selected_params <- fit_params[fit_params$model == selected_model_name,]
  pred_df <- plot_data$pred_df[plot_data$pred_df$model == selected_model_name,]
  d2xy_df <- plot_data$d2xy_df[plot_data$d2xy_df$model == selected_model_name,]
  dydx_df <- plot_data$dydx_df[plot_data$dydx_df$model == selected_model_name,]

  return(list(best_model_name =selected_model_name, best_fit = selected_fit, best_data = selected_data,
              best_ci = selected_params, best_pred = pred_df,
              best_d2xy = d2xy_df, best_dydx = dydx_df))

}

# This function depends on get_loqs, generate_inflection_point, generate_lods as it is a wrapper and calls them
fit_qc_glance <- function(best_fit,
                          response_variable,
                          independent_variable,
                          fixed_a_result,
                          antigen_settings,
                          antigen_fit_options,
                          verbose = TRUE) {


  if (antigen_settings$l_asy_constraint_method == "range_of_blanks" && antigen_fit_options$is_log_response && !is.null(fixed_a_result)) {
    .eps = 0.00005
    fixed_a_result <- log10(fixed_a_result + .eps)
  }
  # obtain qc metrics
  loqs <- get_loqs(best_d2xy = best_fit$best_d2xy, fit = best_fit$best_fit, independent_variable = independent_variable)

  std_error_blank <- list(std_error_blank = get_blank_se(antigen_settings = antigen_settings))

  lods <- generate_lods(best_fit = best_fit, fixed_a_result = fixed_a_result, std_error_blank = std_error_blank)

  inflection_point <- generate_inflection_point(model_name = best_fit$best_model_name, fit = best_fit$best_fit, fixed_a_result = fixed_a_result,independent_variable = independent_variable)


  theta <- coef(best_fit$best_fit)
  theta["a"] <- ifelse(!is.null(fixed_a_result), fixed_a_result, theta["a"])
  yi <- as.numeric(inflection_point$inflect_y)
  dydx_inflect <- as.numeric(switch(best_fit$best_model_name,
                                    Y4      = dydxY4(yi, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                                    Yd4     = dydxYd4(yi, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                                    Ygomp4  = dydxYgomp4(yi, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                                    Y5      = dydxY5(yi, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]),
                                    Yd5     = dydxYd5(yi, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]),
                                    stop("Unsupported model")
  )
  )

  combined_qc_list <- c(loqs, lods, inflection_point, std_error_blank, dydx_inflect = dydx_inflect)

  # Ensure all values in the list are scalar numerics before creating dataframe
  combined_qc_list <- lapply(combined_qc_list, function(x) {
    if (is.list(x)) as.numeric(unlist(x))[1] else as.numeric(x)[1]
  })

  qc_glance <- as.data.frame(combined_qc_list, stringsAsFactors = FALSE)

  # Ensure scalar comparisons
  uloq_val <- as.numeric(qc_glance$uloq)[1]
  inflect_val <- as.numeric(qc_glance$inflect_x)[1]

  if (!is.na(uloq_val) && !is.na(inflect_val) && uloq_val < inflect_val) {
    qc_glance$uloq <- NA_real_
    qc_glance$uloq_y <- NA_real_
  }
  # obtain fit statistics and parameter estimates
  s <-  summary(best_fit$best_fit)
  coefs <- coef(s)
  coef_df <- as.data.frame(t(coefs[, "Estimate"]))

  if (!("a" %in% names(coef_df))) {
    coef_df$a <- fixed_a_result
  }

  # Residual stats
  sigma <- s$sigma
  df_resid <- s$df[2]

  # Compute R-squared
  response <- best_fit$best_data[[response_variable]]

  rss <- sum(residuals(best_fit$best_fit)^2)
  tss <- sum((response - mean(response))^2)
  r_squared <- 1 - rss / tss

  # AIC and BIC
  aic <- AIC(best_fit$best_fit)
  bic <- BIC(best_fit$best_fit)

  logLik_val <- as.numeric(logLik(best_fit$best_fit))
  converged <- best_fit$best_fit$convInfo$isConv
  iter <- best_fit$best_fit$convInfo$finIter

  crit <- best_fit$best_model_name

  model_formula <- paste(deparse(formula(best_fit$best_fit)), collapse = " ")
  model_formula <- gsub("I\\((.*)\\)", "\\1", model_formula)

  n_obs <- length(residuals(best_fit$best_fit))

  # calculate mse
  mse <- mean(resid(best_fit$best_fit)^2, na.rm = T)
  # mean of observed response
  mean_obs_mfi<- mean(response, na.rm = TRUE)
  # coefficient of variation.
  cv <- (sqrt(mse) / mean_obs_mfi) * 100

  glance_df <-  data.frame(
    study_accession = unique(best_fit$best_data$study_accession),
    experiment_accession = unique(best_fit$best_data$experiment_accession),
    plateid = unique(best_fit$best_data$plateid),
    plate = unique(best_fit$best_data$plate),
    sample_dilution_factor = unique(best_fit$best_data$sample_dilution_factor),
    antigen = unique(best_fit$best_data$antigen),
    iter = iter,
    status = converged,
    crit = crit,
    coef_df,
    qc_glance,
    dfresidual = df_resid,
    nobs = n_obs,
    rsquare_fit = r_squared,
    aic = aic,
    bic = bic,
    loglik = logLik_val,
    mse = mse,
    cv = cv,
    source = unique(best_fit$best_data$source),
    bkg_method =  antigen_fit_options$blank_option, #blank_option,
    is_log_response = antigen_fit_options$is_log_response,
    is_log_x = antigen_fit_options$is_log_concentration,
    apply_prozone  = antigen_fit_options$apply_prozone,
    formula = model_formula)

  best_fit$best_glance <- glance_df
  return(best_fit)

}

get_loqs <- function(best_d2xy, fit, independent_variable,  verbose = TRUE) {
  y <- as.numeric(best_d2xy$d2x_y)
  x <- as.numeric(best_d2xy$x)

  n <- length(y)
  if (n < 3) stop("Need at least 3 points to detect local extrema.")

  # first differences
  dy <- diff(y)

  # candidate interior indices where slope changes sign
  idx_max <- which(dy[-1] < 0 & dy[-length(dy)] > 0) + 1  # local max neighborhood
  idx_min <- which(dy[-1] > 0 & dy[-length(dy)] < 0) + 1  # local min neighborhood

  interpolate_vertex <- function(i) {
    # use points (i-1, i, i+1)
    xi <- x[(i-1):(i+1)]
    yi <- y[(i-1):(i+1)]

    # fit quadratic: y = a*x^2 + b*x + c
    X <- cbind(xi^2, xi, 1)
    coef <- solve(t(X) %*% X, t(X) %*% yi)  # least squares for robustness
    a <- coef[1]; b <- coef[2]; c <- coef[3]

    if (a == 0) {
      # fallback: no curvature, just return middle point
      return(list(x = xi[2], y = yi[2]))
    }

    # vertex of parabola: x* = -b / (2a)
    xv <- -b / (2 * a)

    # clamp to local interval [x_{i-1}, x_{i+1}] to avoid nonsense extrapolation
    xv <- max(min(xv, max(xi)), min(xi))

    yv <- a * xv^2 + b * xv + c
    list(x = xv, y = yv)
  }

  # interpolate for each candidate
  if (length(idx_max) > 0) {
    max_list <- lapply(idx_max, interpolate_vertex)
    max_df <- data.frame(
      x = vapply(max_list, `[[`, numeric(1), "x"),
      y = vapply(max_list, `[[`, numeric(1), "y"),
      i_center = idx_max
    )
  } else {
    max_df <- data.frame(x = numeric(0), y = numeric(0), i_center = integer(0))
  }

  if (length(idx_min) > 0) {
    min_list <- lapply(idx_min, interpolate_vertex)
    min_df <- data.frame(
      x = vapply(min_list, `[[`, numeric(1), "x"),
      y = vapply(min_list, `[[`, numeric(1), "y"),
      i_center = idx_min
    )
  } else {
    min_df <- data.frame(x = numeric(0), y = numeric(0), i_center = integer(0))
  }

  # # also give global (approximate) max/min among these
  # global_max <- if (nrow(max_df) > 0) max_df[which.max(max_df$y), ] else NULL

  # all_x <- c(max_df$x,min_df$x)
  # Handle empty dataframes and ensure scalar values are returned
  if (nrow(max_df) > 0) {
    lloq_x <- as.numeric(max_df[which.max(max_df$y), "x"][1])
  } else {
    lloq_x <- NA_real_
  }

  if (nrow(min_df) > 0) {
    uloq_x <- as.numeric(min_df[which.min(min_df$y), "x"][1])
  } else {
    uloq_x <- NA_real_
  }

  # global_min <- if (nrow(min_df) > 0) min_df[which.min(min_df$y), ] else NULL

  y_loq <- tryCatch({
    predict(fit, newdata = setNames(data.frame(x = c(lloq_x, uloq_x)), independent_variable))
  }, error = function(e) rep(NA_real_, 2))

  # Ensure lloq_y and uloq_y are scalar numeric values
  lloq_y <- if (length(y_loq) >= 1 && !all(is.na(y_loq))) as.numeric(min(y_loq, na.rm = TRUE)) else NA_real_
  uloq_y <- if (length(y_loq) >= 1 && !all(is.na(y_loq))) as.numeric(max(y_loq, na.rm = TRUE)) else NA_real_

  # print(inflection_point$inflect_x)
  # if (uloq_x < inflection_point$inflect_x) {
  #   uloq_x <- NA_real_
  #   uloq_y <- NA_real_
  # } else {
  #   uloq_y <- max(y_loq)
  # }
  return(list(
    lloq = lloq_x,
    uloq = uloq_x,
    lloq_y = lloq_y,
    uloq_y = uloq_y
  ))
}

generate_inflection_point <- function(model_name, fit, fixed_a_result, independent_variable,  verbose = TRUE) {
  params <- coef(fit)
  g <- as.numeric(if ("g" %in% names(params)) params["g"] else 1)# auto default
  a <-  as.numeric(ifelse(!is.null(fixed_a_result), fixed_a_result, params["a"]))
  b <- as.numeric(params["b"])
  c <- as.numeric(params["c"])
  d <- as.numeric(params["d"])


  inflect_x  <- tryCatch({
    if (model_name == "Y5") {
      # For 5 parameter model
      # x inflection point is c - b*ln(g)
      c - b * log(g)
    } else if (model_name == "Y4") {
      c
    } else if (model_name == "Yd5") {
      c + (log(g) / b)
    } else if (model_name == "Yd4") {
      c
    } else if (model_name == "Ygomp4") {
      c
    }
  }, error = function(e) NA)

  inflect_x <- as.numeric(inflect_x)

  inflect_y <- tryCatch({
    if (model_name == "Y5") {
      # For 5 parameter model
      # d + ((a - d) / (1 + g)^g)
      x_inf <- c - b * log(g)
      d + (a - d) / (1 + exp((x_inf - c) / b))^g
    } else if (model_name == "Y4") {
      (a + d) / 2
    } else if (model_name == "Yd5") {
      a + (d - a) * 2^(-1/g)
    } else if (model_name == "Yd4") {
      (a + d) / 2
    } else if (model_name == "Ygomp4") {
      a + (d - a) * exp(-1)
    }

    #predict(fit, newdata = setNames(data.frame(x = inflect_x), independent_variable))
  }, error = function(e) NA)

  inflect_y <- as.numeric(inflect_y)

  return(list(inflect_x = inflect_x, inflect_y = inflect_y))
}

generate_lods <- function(best_fit, fixed_a_result, std_error_blank,  verbose = TRUE) {

  best_ci <- best_fit$best_ci
  best_data <- best_fit$best_data

  ulod <- best_ci[best_ci$parameter == "d",]$conf.low

  if (!is.null(fixed_a_result)) {
    if (is.na(std_error_blank)) {
      std_error_blank <- 0
    } else {
      std_error_blank <- as.numeric(std_error_blank)
    }
    critical_value <- qt(0.975, df = nrow(best_data) - length(best_ci$parameter))
    cat("critical value:\n")
    print(critical_value)
    cat("Blank SE:\n")
    print(std_error_blank)
    margin_of_error <- critical_value * std_error_blank
    llod <- fixed_a_result + margin_of_error
  } else {
    llod <- best_ci[best_ci$parameter == "a",]$conf.high
  }

  if (ulod < 0 || ulod < llod) {
    ulod  <- NA_real_
  }
  return(list(llod = llod, ulod = ulod))

}

tidy.nlsLM <- function(best_fit, fixed_a_result, model_constraints, antigen_settings, antigen_fit_options,  verbose = TRUE) {

  if (antigen_settings$l_asy_constraint_method == "range_of_blanks" && antigen_fit_options$is_log_response) {
    if (!is.null(fixed_a_result)) {
      .eps <- 0.000005
      fixed_a_result <- log10(fixed_a_result + .eps)
    }
    antigen_settings$l_asy_min_constraint <- log10(antigen_settings$l_asy_min_constraint)
    antigen_settings$l_asy_max_constraint <- log10(antigen_settings$l_asy_max_constraint)

  }
  m_constraints <- model_constraints[[best_fit$best_model_name]]
  m_constraints_df <- as.data.frame(m_constraints)
  m_constraints_df$term <- rownames(m_constraints_df)
  rownames(m_constraints_df) <- NULL
  m_constraints_df <- m_constraints_df[, c("term", "lower", "upper")]
  if (!is.null(fixed_a_result)) {
    a_fixed_constraint <- tibble::tibble(
      term = "a",
      lower = antigen_settings$l_asy_min_constraint,
      upper = antigen_settings$l_asy_max_constraint,
    )
    m_constraints_df <-  rbind(a_fixed_constraint, m_constraints_df)
  }

  s <- summary(best_fit$best_fit)
  out <- as.data.frame(s$coefficients)
  tidy_df <- tibble::tibble(
    term = rownames(out),
    estimate = out[, "Estimate"],
    std.error = out[, "Std. Error"],
    statistic = out[, "t value"],
    p.value = out[, "Pr(>|t|)"]
  )

  tidy_df$study_accession <- unique(best_fit$best_data$study_accession)
  tidy_df$experiment_accession <- unique(best_fit$best_data$experiment_accession)
  tidy_df$sample_dilution_factor <- unique(best_fit$best_data$sample_dilution_factor)
  tidy_df$antigen <- unique(best_fit$best_data$antigen)
  tidy_df$plateid <- unique(best_fit$best_data$plateid)
  tidy_df$plate <- unique(best_fit$best_data$plate)
  tidy_df$source <- unique(best_fit$best_data$source)

  if (!is.null(fixed_a_result)) {
    a_fixed <- tibble::tibble(
      term = "a",
      estimate = fixed_a_result,
      std.error = 0,
      statistic = NA_real_,
      p.value = NA_real_,
      study_accession = unique(best_fit$best_data$study_accession),
      experiment_accession = unique(best_fit$best_data$experiment_accession),
      sample_dilution_factor = unique(best_fit$best_data$sample_dilution_factor),
      antigen = unique(best_fit$best_data$antigen),
      plateid = unique(best_fit$best_data$plateid),
      plate = unique(best_fit$best_data$plate),
      source = unique(best_fit$best_data$source)
    )
    tidy_df <- rbind(a_fixed, tidy_df)
  }

  # rename standard error column And p-value column
  names(tidy_df)[names(tidy_df) == "std.error"] <- "std_error"
  names(tidy_df)[names(tidy_df) == "p.value"] <- "p_value"


  tidy_df <- merge(tidy_df, m_constraints_df, by = "term", all.x = TRUE)
  other_cols <- setdiff(colnames(tidy_df), c("term", c("lower", "upper")))

  # New order: term → lower, upper → rest
  tidy_df <- tidy_df[, c("term", "lower", "upper", other_cols)]


  best_fit$best_tidy <- tidy_df
  if (verbose) {
    message("Finished tidy.nlsLM")
  }
  return(best_fit)
}

calculate_predicted_concentration <- function(model_name,fit,
                                              plate_samples,
                                              fixed_constraint,
                                              response_variable,
                                              is_log_response,
                                              verbose = TRUE) {
  if (is_log_response) {
    plate_samples[[response_variable]] <- log10(plate_samples[[response_variable]])
  }

  params <- coef(fit)
  g <- if ("g" %in% names(params)) params["g"] else 1  # auto default
  b <- params["b"]
  c <- params["c"]
  d <- params["d"]

  if (!is.null(fixed_constraint)){
    message("Lower asymptote is fixed at", fixed_constraint)
    fixed_value <- fixed_constraint
    a  <-  fixed_value
    plate_samples$predicted_concentration  <- tryCatch({
      if (model_name == "Y5") {
        inv_Y5_fixed(y = plate_samples[[response_variable]] , fixed_a = a, b = b, c = c, d = d , g = g)
      } else if (model_name == "Yd5") {
        inv_Yd5_fixed(y = plate_samples[[response_variable]], fixed_a = a, b = b, c = c, d = d, g = g)
      } else if (model_name == "Y4") {
        inv_Y4_fixed(y = plate_samples[[response_variable]], fixed_a = a, b = b, c = c, d = d)
      } else if (model_name == "Yd4") {
        inv_Yd4_fixed(y = plate_samples[[response_variable]], fixed_a = a, b = b, c = c, d = d)
      } else if (model_name == "Ygomp4") {
        inv_Ygomp4_fixed(plate_samples[[response_variable]], fixed_a = a, b = b, c = c, d = d)
      }
    }, error = function(e)  {
      message("Error: ", e$message)
      rep(NA_real_, nrow(plate_samples))
    }
    )

  } else {
    a <- params["a"]
    plate_samples$predicted_concentration  <- tryCatch({
      if (model_name == "Y5") {
        inv_Y5(y = plate_samples[[response_variable]] , a = a, b = b, c = c, d = d , g = g)
      } else if (model_name == "Yd5") {
        inv_Yd5(y = plate_samples[[response_variable]], a = a, b = b, c = c, d = d, g = g)
      } else if (model_name == "Y4") {
        inv_Y4(y = plate_samples[[response_variable]], a = a, b = b, c = c, d = d)
      } else if (model_name == "Yd4") {
        inv_Yd4(y = plate_samples[[response_variable]], a = a, b = b, c = c, d = d)
      } else if (model_name == "Ygomp4") {
        message("Ygomp4 predicted")
        inv_Ygomp4(plate_samples[[response_variable]], a = a, b = b, c = c, d = d)
      }
    }, error = function(e)  {
      message("Error: ", e$message)
      rep(NA_real_, nrow(plate_samples))
    }
    )

  }

  return(plate_samples)
}

# For a plate and and antigen in a study prepare data for compute_robust_curves
preprocess_robust_curves <- function(data, antigen_settings, response_variable,
                                     independent_variable,
                                     is_log_response,
                                     blank_data = NULL,
                                     blank_option = "ignored",
                                     is_log_independent = TRUE,
                                     apply_prozone = TRUE,
                                     verbose = TRUE) {

  ## compute standard curve concentration for undiluted standard curve sample.
  undiluted_sc_concentration <- get_study_exp_antigen_plate_param(antigen_settings)

  data <- compute_concentration(data = data,
                                undiluted_sc_concentration = undiluted_sc_concentration,
                                independent_variable = independent_variable,
                                is_log_concentration = TRUE)

  if (apply_prozone) {
    if (verbose) {
      message("applying prozone correction")
    }
    data <- correct_prozone(stdframe = data,
                            prop_diff = 0.1,
                            dil_scale = 2,
                            response_variable = response_variable,
                            independent_variable = independent_variable,
                            verbose = verbose
    )

  }

  ## Blank Operations (in linear space)
  data <- perform_blank_operation(
    blank_data = blank_data,
    data = data,
    response_variable = response_variable,
    independent_variable = independent_variable,
    is_log_response = is_log_response,
    blank_option = blank_option,
    verbose = verbose
  )

  ## Log transform the response after all other prepossessing steps
  if (is_log_response) {
    if (verbose) {
      message("Applying log10 to the response variable after blank operation")
    }
    # set any response 0 or less to 1 so log10(1) is 0
    data[[response_variable]][data[[response_variable]] <= 0] <- 1
    data[[response_variable]] <- log10(data[[response_variable]])
  }

  antigen_fit_options <- list(is_log_response = is_log_response,
                              blank_option = blank_option,
                              is_log_concentration = is_log_independent,
                              apply_prozone = apply_prozone)

  return(list(data = data, antigen_fit_options = antigen_fit_options))
}

propagate_error_analytic <- function(model,         # character: "Y4","Yd4","Ygomp4","Y5","Yd5"
                                     fit,           # nlsLM object (already fitted)
                                     y,             # observed response
                                     se_y = 0,     # standard error of y (0 if unknown)
                                     fixed_a,  # the constrained lower asymptote
                                     verbose = TRUE
) {
  # ----- 1. Extract coefficients & covariance -------------------------------
  theta    <- coef(fit)          # named vector
  vcov_mat <- vcov(fit)          # covariance matrix

  if(!is.null(fixed_a)) {


    # ----- 2. Analytic gradient w.r.t. parameters & y -----------------------
    inv_and_grad <- make_inv_and_grad_fixed(model, y, fixed_a)
    # ----- 3. Evaluate inverse (point estimate) ------------------------------
    x_hat <- inv_and_grad$inv(theta)
    grad_theta <- inv_and_grad$grad(theta)   # named numeric vector
    grad_y <- inv_and_grad$grad_y(theta)    # scalar
  } else {
    # ----- 2. Evaluate inverse (point estimate) ------------------------------
    x_hat <- switch(model,
                    Y4      = inv_Y4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Yd4     = inv_Yd4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Ygomp4  = inv_Ygomp4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Y5      = inv_Y5(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]),
                    Yd5     = inv_Yd5(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]),
                    stop("Unsupported model name"))

    # ----- 3. Analytic gradient w.r.t. parameters & y -----------------------
    grads <- switch(model,
                    Y4     = grad_Y4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Yd4    = grad_Yd4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Ygomp4 = grad_Ygomp4(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"]),
                    Y5     = grad_Y5(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]),
                    Yd5    = grad_Yd5(y, a = theta["a"], b = theta["b"], c = theta["c"], d = theta["d"], g = theta["g"]))

    grad_theta <- grads$grad_theta   # named vector (same order as theta)
    grad_y     <- grads$grad_y
  }


  # ----- 4. Delta‑method variance -----------------------------------------
  var_par <- as.numeric(t(grad_theta) %*% vcov_mat %*% grad_theta)
  var_y   <- (grad_y^2) * (se_y^2)
  var_x   <- var_par + var_y
  se_x    <- sqrt(var_x)

  # ----- 5. Return ---------------------------------------------------------
  list(x_est      = x_hat,
       se_x       = se_x,
       var_x      = var_x,
       grad_theta = grad_theta,
       grad_y     = grad_y)
}

#  Propagation for a whole data‑frame
#' Propagate the error of a fitted sigmoid model to many new samples
#'
#' @param pred_df   data‑frame that contains the new measurements.
#' @param fit       an **nlsLM** object that was used to fit the
#'                  chosen sigmoid model (already has coef() and vcov()).
#' @param model     character, one of: "Y4","Yd4","Ygomp4","Y5","Yd5".
#' @param y_col     name (character) of the column that stores the observed response.
#' @param se_col    name (character) of the column that stores the standard error of the response.
#' @param quiet     logical. If TRUE suppresses the progress bar.
#'
#' @return the input data‑frame with two extra columns:
#'         * `x_est` – inverse‑predicted concentration,
#'         * `se_x`  – propagated standard error of that concentration.
#' @examples
#' ##  simulate a small example (3 rows) -----------------
#' library(minpack.lm)
#' ## (fit a 4‑parameter logistic first)
#' set.seed(123)
#' x_cal  <- seq(0,10,length.out=25)
#' a<-0.2; b<-0.9; c<-4.5; d<-9.8
#' y_cal  <- a + (d-a)/(1+exp((x_cal-c)/b)) + rnorm(length(x_cal),sd=0.08)
#' fit4   <- nlsLM(y ~ a + (d-a)/(1+exp((x-c)/b)),
#'                data=data.frame(x=x_cal,y=y_cal),
#'                start=list(a=0,b=1,c=5,d=10))
#'
#' pred_df <- data.frame(
#'   response_var = c(6.7,7.2,5.9),
#'   se_std_response = c(0.09,0.12,0.07)
#' )
#' out <- propagate_error_dataframe(pred_df, fit4,
#'                                  model = "Y4",
#'                                  y_col = "response_var",
#'                                  se_col = "se_std_response")
#' print(out)
propagate_error_dataframe <- function(pred_df,
                                      fit,
                                      model = c("Y4","Yd4","Ygomp4","Y5","Yd5"),
                                      y_col,
                                      se_col,
                                      fixed_a,
                                      quiet = FALSE) {
  ## 0. sanity checks
  if (!is.data.frame(pred_df))
    stop("`pred_df` must be a data.frame.", call. = FALSE)

  model <- match.arg(model)

  ## 1. tidy‑eval column selectors
  y_q  <- enquo(y_col)
  se_q <- enquo(se_col)

  # evaluate the selectors – they may be a column name, a character string,
  # or a plain numeric vector (for SEs)
  y_val  <- rlang::eval_tidy(y_q, pred_df, env = parent.frame())
  se_val <- rlang::eval_tidy(se_q, pred_df, env = parent.frame())

  ## 1a. Resolve response column --------------------------------------
  if (is.character(y_val) && length(y_val) == 1) {
    y_name <- y_val
  } else if (is.name(y_q) && length(y_val) == 1) {
    y_name <- as.character(y_val)
  } else {
    stop("`y_col` must resolve to a single column name (character).",
         call. = FALSE)
  }
  if (!y_name %in% names(pred_df))
    stop(sprintf("Column '%s' not found in `pred_df`.", y_name), call. = FALSE)

  ## 1b. Resolve SE column / vector ------------------------------------
  if (is.numeric(se_val) && length(se_val) == nrow(pred_df)) {
    se_vec <- se_val                     # a raw numeric vector
  } else if (is.character(se_val) && length(se_val) == 1) {
    se_name <- se_val
    if (!se_name %in% names(pred_df))
      stop(sprintf("Column '%s' not found in `pred_df`.", se_name),
           call. = FALSE)
    se_vec <- pred_df[[se_name]]
  } else if (is.name(se_q) && length(se_val) == 1) {
    se_name <- as.character(se_val)
    if (!se_name %in% names(pred_df))
      stop(sprintf("Column '%s' not found in `pred_df`.", se_name),
           call. = FALSE)
    se_vec <- pred_df[[se_name]]
  } else {
    stop("`se_col` must be a column name (quoted/unquoted) or a numeric ",
         "vector of length = nrow(pred_df).", call. = FALSE)
  }

  ## 2.  Vectorised evaluation -----------------------------------------
  # The anonymous function returns a *list* with components x_est and se_x.
  # Using `mapply(..., SIMPLIFY = FALSE)` keeps the list structure.
  # res <- mapply(
  #   FUN = function(y_i, se_i) {
  #     if (is.na(y_i) || is.na(se_i)) {
  #       return(list(x_est = NA_real_, se_x = NA_real_))
  #     }
  #     out <- propagate_error_analytic(model, fit, y_i, se_i, fixed_a)
  #     list(x_est = out$x_est, se_x = out$se_x)
  #   },
  #   y_i = pred_df[[y_name]],
  #   se_i = se_vec,
  #   SIMPLIFY = FALSE
  # )

    res <- mapply(
    FUN = function(y_i, se_i) {
      if (is.na(y_i) || is.na(se_i)) {
        return(list(x_est = NA_real_, se_x = NA_real_))
      }
      # This is where se_i (from se_col) is used
      out <- propagate_error_analytic(model, fit, y_i, se_i, fixed_a)
      list(x_est = out$x_est, se_x = out$se_x)
    },
    y_i = pred_df[[y_name]],
    se_i = se_vec,  # ← This needs to be non-zero!
    SIMPLIFY = FALSE
  )

  ## 3. Attach results -------------------------------------------------
  pred_df$predicted_concentration <- sapply(res, `[[`, "x_est")
  pred_df$se_x  <- sapply(res, `[[`, "se_x")

  pred_df
}

#' Build a list of assay SE estimates for each plate in batch processing
#'
#' @param antigen_plate_list_res Result from build_antigen_plate_list()
#' @param loaded_data_list List of loaded data per experiment
#' @param response_var Name of response variable (e.g., "mfi")
#' @param dilution_col Name of dilution column
#' @return Named list of se_std_response objects keyed by plate ID
build_se_std_response_list <- function(antigen_plate_list_res,
                                       loaded_data_list,
                                       response_var,
                                       dilution_col = "dilution") {

  antigen_plate_list <- antigen_plate_list_res$antigen_plate_list
  antigen_plate_list_ids <- antigen_plate_list_res$antigen_plate_list_ids

  se_list <- list()

  for (plate_id in antigen_plate_list_ids) {
    plate_info <- antigen_plate_list[[plate_id]]

    if (is.null(plate_info) || is.null(plate_info$plate_standard)) {
      se_list[[plate_id]] <- NULL
      next
    }

    # Calculate SE for this plate's standards
    se_list[[plate_id]] <- tryCatch({
      assay_se(
        data = plate_info$plate_standard,
        dilution_col = dilution_col,
        response_col = response_var,
        method = "pooled_within"  # Use the refactored method
      )
    }, error = function(e) {
      warning(paste("Could not calculate SE for plate:", plate_id, "-", e$message))
      NULL
    })
  }

  return(se_list)
}


#' Estimate Assay Response Standard Error from Standard Curve Replicates
#'
#' Computes an estimate of the standard error in assay response (y) by
#' aggregating within-dilution variability from standard samples collected
#' across multiple plates. Within each dilution level, standard samples
#' are treated as replicates for estimating measurement error.
#'
#' @param data        data.frame or tibble containing standard curve data
#' @param dilution_col name (character) of the column identifying dilution level
#'                     (default = "dilution")
#' @param response_col name (character) of the response column (e.g., "mfi")
#' @param plate_col   name (character) of the column identifying plate
#'                     (default = "plate"). If NULL, assumes single plate.
#' @param method      character, one of:
#'                     - "pooled_within" (default): pools within-dilution variance
#'                       weighted by degrees of freedom (recommended)
#'                     - "median_se": median of per-dilution SEs (robust)
#'                     - "mean_se": mean of per-dilution SEs
#' @param min_reps    minimum replicates per dilution to include in estimate
#'                     (default = 2)
#' @param na.rm       logical – drop NA values (default = TRUE)
#'
#' @return A list with:
#'   \item{overall_se}{single numeric: pooled SE of response measurement}
#'   \item{by_dilution}{tibble with per-dilution statistics}
#'   \item{pooling_method}{character: method used for pooling}
#'   \item{total_df}{total degrees of freedom in the pooled estimate
#'   \item{n_dilutions_used}{number of dilution levels contributing to estimate}
#'
#' @details
#' The pooled within-dilution method computes:
#' \deqn{s_{pooled}^2 = \frac{\sum_{i} (n_i - 1) s_i^2}{\sum_{i} (n_i - 1)}}
#' where \eqn{s_i^2} is the variance at dilution level \eqn{i} with \eqn{n_i}
#' replicates. The pooled SE is then \eqn{s_{pooled} / \sqrt{\bar{n}}} where
#' \eqn{\bar{n}} is the harmonic mean of replicate counts.
#'
#' This approach is appropriate because:
#' 1. Within each dilution, samples have the same expected concentration
#' 2. Variability within dilution reflects measurement error, not signal
#' 3. Pooling across dilutions increases precision of the SE estimate
#'
#' @examples
#' # Example with standard curve data
#' se_result <- assay_se(
#'   data = standards_df,
#'   dilution_col = "dilution",
#'   response_col = "mfi",
#'   plate_col = "plate",
#'   method = "pooled_within"
#' )
#' print(se_result$overall_se)
#'
#' @export
assay_se <- function(data,
                     dilution_col = "dilution",
                     response_col = "mfi",
                     plate_col    = "plate",
                     method       = c("pooled_within", "median_se", "mean_se"),
                     min_reps     = 2,
                     na.rm        = TRUE) {



  ## 0. Input validation

  method <- match.arg(method)
  data <- dplyr::as_tibble(data)

  required_cols <- c(dilution_col, response_col)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  ## 1. Prepare data: group by dilution (and optionally plate within dilution)
  #    Each dilution level contains replicates that should have same true value

  # Remove NA responses

  if (na.rm) {
    data <- data[!is.na(data[[response_col]]), ]
  }

  if (nrow(data) == 0) {
    warning("No valid response data after removing NAs")
    return(list(
      overall_se = NA_real_,
      by_dilution = NULL,
      pooling_method = method,
      total_df = 0,
      n_dilutions_used = 0
    ))
  }

  ## 2. Compute within-dilution statistics
  #    These are replicates at the same concentration level
  by_dilution <- data %>%
    dplyr::group_by(!!rlang::sym(dilution_col)) %>%
    dplyr::summarise(
      n        = dplyr::n(),
      mean     = mean(!!rlang::sym(response_col), na.rm = TRUE),
      sd       = stats::sd(!!rlang::sym(response_col), na.rm = TRUE),
      variance = stats::var(!!rlang::sym(response_col), na.rm = TRUE),
      se       = sd / sqrt(n),
      df       = n - 1,  # degrees of freedom for this dilution
      .groups  = "drop"
    ) %>%
    dplyr::arrange(!!rlang::sym(dilution_col))

  ## 3. Filter to dilutions with sufficient replicates
  by_dilution_valid <- by_dilution %>%
    dplyr::filter(n >= min_reps, !is.na(variance), variance > 0)

  n_dilutions_used <- nrow(by_dilution_valid)

  if (n_dilutions_used == 0) {
    warning("No dilution levels with sufficient replicates (min_reps = ", min_reps, ")")
    return(list(
      overall_se = NA_real_,
      by_dilution = by_dilution,
      pooling_method = method,
      total_df = 0,
      n_dilutions_used = 0
    ))
  }

  ## 4. Compute pooled SE estimate based on method
  overall_se <- switch(method,

                       # Method 1: Pooled within-group variance (recommended)
                       # This is the statistically proper way to pool variances
                       "pooled_within" = {
                         # Pooled variance: sum of (df_i * var_i) / sum(df_i)
                         total_df <- sum(by_dilution_valid$df)
                         pooled_var <- sum(by_dilution_valid$df * by_dilution_valid$variance) / total_df
                         pooled_sd <- sqrt(pooled_var)

                         # SE for a single observation
                         # Use harmonic mean of n for typical replicate count
                         n_harmonic <- n_dilutions_used / sum(1 / by_dilution_valid$n)
                         pooled_sd / sqrt(n_harmonic)
                       },

                       # Method 2: Median of per-dilution SEs (robust to outliers)
                       "median_se" = {
                         stats::median(by_dilution_valid$se, na.rm = TRUE)
                       },

                       # Method 3: Mean of per-dilution SEs
                       "mean_se" = {
                         mean(by_dilution_valid$se, na.rm = TRUE)
                       }
  )

  ## 5. Compute total degrees of freedom (for confidence intervals if needed)
  total_df <- sum(by_dilution_valid$df)

  ## 6. Return results
  list(
    overall_se       = overall_se,
    by_dilution      = by_dilution,
    pooling_method   = method,
    total_df         = total_df,
    n_dilutions_used = n_dilutions_used
  )
}

#' Replace ∞ with the largest finite value and rescale to [1, max_finite]
#'
#' @param x A numeric vector (e.g. a data‑frame column).  `NA`s are kept as `NA`.
#'
#' @return A numeric vector of the same length as `x`.  All `Inf` values are
#'   replaced by the largest finite value that occurs in `x`.  Afterwards the
#'   whole vector is linearly rescaled so that the smallest (non‑NA) value becomes
#'   1 and the largest (the former `max_finite`) becomes `max_finite`.
#'
#' @details
#'   * Only positive `Inf` is recoded – any `-Inf` is left untouched (or you can
#'     change the code if you want a different behaviour).
#'   * If **no** finite value is present the function returns a vector of `NA`s
#'     and throws a warning.
#'   * When all (finite) values are identical the function returns a vector of
#'     `1`s (the range collapses to a single point).
#'   * `NA` and `NaN` are preserved, they are **not** used when computing the
#'     max/min.
#'
#' @examples
#' # Example 1 – a simple column with a few infinities
#' vec <- c(2, 5, Inf, 8, 3, Inf, NA)
#' condition_inf_rescale(vec)
#'
#' # Example 2 – a column that already has a range 1 … 10
#' vec2 <- c(1, 4, 7, 10)
#' condition_inf_rescale(vec2)   # unchanged (apart from possible rounding)
#'
#' # Example 3 – all values are Inf  (returns NAs)
#' condition_inf_rescale(rep(Inf, 5))
#'
#' @export
condition_inf_rescale <- function(x) {
  ## 0. Input checks
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector (or a numeric data‑frame column).")
  }
  ## 1. Identify finite (i.e. non‑NA, non‑NaN, non‑Inf) values
  finite_idx <- is.finite(x)               # TRUE for numbers that are not NA/NaN/Inf
  if (!any(finite_idx)) {
    warning("No finite values found – returning a vector of NA.")
    return(rep(NA_real_, length(x)))
  }
  ## 2. Replace positive Inf with the largest finite value
  max_finite <- max(x[finite_idx], na.rm = TRUE)   # the target for Inf
  inf_pos_idx <- is.infinite(x) & (x > 0)          # only +Inf
  if (any(inf_pos_idx)) {
    x[inf_pos_idx] <- max_finite
  }
  ## 3. Rescale to the interval [1, max_finite]
  # Re‑compute the finite range after the replacement (NAs are ignored)
  finite_after <- is.finite(x)
  min_val <- min(x[finite_after], na.rm = TRUE)
  max_val <- max(x[finite_after], na.rm = TRUE)   # should equal max_finite
  # If the range collapses to a single point, just return 1 for every non‑NA
  if (abs(max_val - min_val) < .Machine$double.eps) {
    out <- rep(1, length(x))
    out[!finite_after] <- NA_real_   # keep original NAs/NaNs
    return(out)
  }
  # Linear rescaling:  (x - min) / (max - min)  ->  [0,1]
  # then stretch to [1, max_finite]
  scale_factor <- (max_finite - 1) / (max_val - min_val)
  out <- (x - min_val) * scale_factor + 1
  # Preserve original NA/NaN positions
  out[!finite_after] <- NA_real_
  return(list(out=out, scale_factor=scale_factor, min_val=min_val))
}

scale_pcov <- function(data, pcov_at_inflect, pcov_at_loq) {
  # Scale pcov from [pcov_at_inflect, pcov_at_loq] to [pcov_at_inflect, 50]
  data$pcov_scaled <- pcov_at_inflect +
    (data$pcov - pcov_at_inflect) * (50 - pcov_at_inflect) / (pcov_at_loq - pcov_at_inflect)

  # data$pcov_scaled <- 1 +
  #   (data$pcov - 1) * (70 - 1) / (pcov_at_loq - pcov_at_inflect)

  return(data)
}

scale_1_100 <- function(x, pcov_at_inflect, pcov_at_loq) {
  # Calculate the minimum and maximum of the input vector, ignoring NAs
  # min_val <- min(x, na.rm = TRUE)
  # max_val <- max(x, na.rm = TRUE)

  min_val <- pcov_at_inflect
  max_val <- pcov_at_loq

  # Apply the scaling formula to range from 1 to 100
  scaled_x <- ((x - min_val) / (max_val - min_val)) * 99 + 1
  # Apply the scaling formula to range from 1 to 30
  scaled_x <- ((x - min_val) / (max_val - min_val)) * 29 + 1

  return(scaled_x)
}

pcov_calc <- function(data,
                      dydx_inflect = dydx_inflect,
                      lloq,
                      uloq,
                      se_var,
                      x_var,
                      is.samples=TRUE,
                      scale_factor = NULL,
                      pcov_at_loq = NULL,
                      pcov_at_inflect = NULL,
                      min_val = NULL,
                      verbose = TRUE) {
  if (nrow(data) == 0) {
    stop("Both 'x' and 'se' must be numeric.")
  }
  data$pcov <- rep(NA_real_, nrow(data))
  x_cond <- list()
  if (is.samples) {
    x_cond$scale_factor <- scale_factor
    x_cond$min_val <- min_val
  } else {
    x_cond <- condition_inf_rescale(data[!is.na(as.numeric(data[[x_var]])) & !is.na(as.numeric(data[[se_var]])), ][[x_var]])
  }
  x_cond$out <- data[[x_var]]
  data[!is.na(as.numeric(data[[x_var]])) & !is.na(as.numeric(data[[se_var]])), ]$pcov <-
    data[!is.na(as.numeric(data[[x_var]])) & !is.na(as.numeric(data[[se_var]])), ][[se_var]] ^ 2

  # Calculate pcov_at_loq if not provided

  if (is.null(pcov_at_loq)) {
    if (!is.na(lloq)) {
      pcov_subset_lloq <- data[data[[x_var]] >= lloq & data[[x_var]] < dydx_inflect, ]$pcov
      pcov_at_lloq <- if (length(pcov_subset_lloq) > 0 && !all(is.na(pcov_subset_lloq))) max(pcov_subset_lloq, na.rm = TRUE) else 50
    } else {
      pcov_at_lloq <- 50
    }
    if (!is.na(uloq)) {
      pcov_subset_uloq <- data[data[[x_var]] <= uloq & data[[x_var]] > dydx_inflect, ]$pcov
      pcov_at_uloq <- if (length(pcov_subset_uloq) > 0 && !all(is.na(pcov_subset_uloq))) max(pcov_subset_uloq, na.rm = TRUE) else 50
    } else {
      pcov_at_uloq <- 50
    }
    pcov_at_loq <- max(pcov_at_lloq, pcov_at_uloq)
  }

  # Calculate pcov_at_inflect if not provided
  if (is.null(pcov_at_inflect)) {
    pcov_at_inflect <- min(data$pcov, na.rm = TRUE)
  }

  # Handle edge case where pcov_at_loq equals pcov_at_inflect (would cause division by zero)
  if (pcov_at_loq == pcov_at_inflect) {
    pcov_at_loq <- pcov_at_inflect + 1  # Avoid division by zero
  }

  # Scale pcov values and cap at 100
  data$pcov <- scale_1_100(data$pcov, pcov_at_inflect = pcov_at_inflect, pcov_at_loq = pcov_at_loq)
  # scaled_data <- scale_pcov(data, pcov_at_inflect = pcov_at_inflect, pcov_at_loq = pcov_at_loq)
  # data$pcov <- scaled_data$pcov_scaled
  # data$pcov <- ifelse(data$pcov > 100, 100, data$pcov)

  return(list(data=data,
              scale_factor=1,
              min_val=x_cond$min_val,
              pcov_at_loq = pcov_at_loq,
              pcov_at_inflect = pcov_at_inflect))
}

## The best fit must contain best_pred and antigen_plate containing plate_samples
predict_and_propagate_error <- function(best_fit,
                                        response_var,
                                        antigen_plate,
                                        study_params,
                                        se_std_response = NULL,
                                        verbose = TRUE) {
  if (study_params$is_log_response) {
    if(is.null(antigen_plate$fixed_a_result)) {
      fixed_a_result <- NULL
    } else {
      .eps <- 0.000005
      fixed_a_result <- log10(antigen_plate$fixed_a_result + .eps)
    }
    log_plate_samples <- log10(antigen_plate$plate_samples[[response_var]])
  }

  pred_se <- best_fit$best_pred
  if (verbose) {
    if (nrow(pred_se) == 0) {
      message("pred_se has 0 rows")
    } else {
      message(paste("pred_se has", nrow(pred_se), "row(s)"))
    }
  }

  z    <- qnorm(0.975)
  max_conc_standard <- ifelse(study_params$is_log_independent,log10(antigen_plate$antigen_settings$standard_curve_concentration),antigen_plate$antigen_settings$standard_curve_concentration)

  # Ensure scalar numeric values from best_glance
  dydx_inflect <- as.numeric(best_fit$best_glance$dydx_inflect)[1]
  lloq <- as.numeric(best_fit$best_glance$lloq)[1]
  uloq <- as.numeric(best_fit$best_glance$uloq)[1]

  # se_std_response <- assay_se(
  #   data = loaded_data$standards,
  #   dilution_col = "dilution",
  #   response_col = response_var,
  #   plate_col = "plate",
  #   method = "pooled_within"
  # )

  # For standards prediction curve
  pred_se$overall_se <- if (!is.null(se_std_response)) {
    se_std_response$overall_se
  } else {
    0
  }

  if (verbose) {
    message("after assigning pred_se$overall_se")
  }

  pred_se <- propagate_error_dataframe(pred_df = pred_se,
                                       fit        = best_fit$best_fit,
                                       model      = best_fit$best_model_name,
                                       y_col      = "yhat",
                                       se_col     = "overall_se",
                                       fixed_a = fixed_a_result
  )

  pred_se$predicted_concentration <- ifelse(!is.infinite(pred_se$predicted_concentration), pred_se$predicted_concentration,
                                            ifelse(pred_se$predicted_concentration > 0, max_conc_standard, 0)
  )

  pcov_list <- pcov_calc(data = pred_se,
                         dydx_inflect = dydx_inflect,
                         lloq = lloq,
                         uloq = uloq,
                         se_var = "se_x",
                         x_var = "predicted_concentration",
                         is.samples = FALSE)

  pcov_data <- pcov_list$data[, c("x","pcov")]
  pred_se <- merge(pred_se, pcov_data, by = "x", all.x = TRUE)
  pred_se$study_accession <- unique(best_fit$best_data$study_accession)
  pred_se$experiment_accession <- unique(best_fit$best_data$experiment_accession)
  pred_se$sample_dilution_factor <- unique(best_fit$best_data$sample_dilution_factor)
  pred_se$plateid <- unique(best_fit$best_data$plateid)
  pred_se$plate <- unique(best_fit$best_data$plate)
  pred_se$antigen <- unique(best_fit$best_data$antigen)
  pred_se$source <- unique(best_fit$best_data$source)
  best_fit$best_pred <- pred_se

  if (verbose) {
    print(head(antigen_plate$plate_samples))
  }

  sample_se <- data.frame(y_new = log_plate_samples,
                          dilution = antigen_plate$plate_samples$dilution,
                          well = antigen_plate$plate_samples$well)
  if (verbose) {
    if (nrow(sample_se) == 0) {
      message("sample_se has 0 rows")
    } else {
      message(paste("sample_se has", nrow(sample_se), "row(s)"))
    }
  }

  sample_se$overall_se <- if (!is.null(se_std_response)) {
    se_std_response$overall_se
  } else {
    0
  }
  if (verbose) {
    message("after assigning sample_se$overall_se")
  }

  sample_se[[response_var]] <- sample_se$y_new
  sample_se <- propagate_error_dataframe(pred_df = sample_se,
                                         fit        = best_fit$best_fit,
                                         model      = best_fit$best_model_name,
                                         y_col      = response_var,
                                         se_col     = "overall_se",
                                         fixed_a =  fixed_a_result
  )
  # predicted coefficient of variation: 100 * se_x / predicted concentration
  # sample_se$pcov <- 50 * (10^(z * sample_se$se_x) - 10^(-z * sample_se$se_x))
  # sample_se$pcov <- 100 * log(10) * sample_se$se_x

  if(study_params$is_log_independent) {
    sample_se$au <- 10^sample_se$predicted_concentration * sample_se$dilution
  } else {
    sample_se$au <- sample_se$predicted_concentration * sample_se$dilution
  }
  sample_se <- dplyr::inner_join(
    antigen_plate$plate_samples[, !(names(antigen_plate$plate_samples) %in% c("mfi", "dilution"))],
    sample_se,
    by = "well"
  )

  pcov_sample_list <- pcov_calc(data = sample_se,
                                dydx_inflect = dydx_inflect,
                                lloq = lloq,
                                uloq = uloq,
                                se_var = "se_x",
                                x_var = "predicted_concentration",
                                is.samples = TRUE,
                                scale_factor=pcov_list$scale_factor,
                                pcov_at_loq = pcov_list$pcov_at_loq,
                                pcov_at_inflect = pcov_list$pcov_at_inflect,
                                min_val=pcov_list$min_val)

  pcov_sample_data <- pcov_sample_list$data[, c("predicted_concentration","pcov")]
  names(pcov_sample_data)[which(names(pcov_sample_data) == "predicted_concentration")] <- "x"
  sample_se <- merge(sample_se, pcov_sample_data, by.x = "predicted_concentration", by.y = "x", all.x = TRUE)
  sample_se$source <- unique(best_fit$best_data$source)
  # remove plate_id and y_new; rename se_x to se_concentration for later use.
  sample_se <- sample_se[, !names(sample_se) %in% c("plate_id", "y_new")]
  names(sample_se)[names(sample_se) == "se_x"] <- "se_concentration"
  best_fit$sample_se <- sample_se

  if (verbose) {
    message("Finished predict_and_propagate_error")
  }

  # best_fit_v <<- best_fit
  return(best_fit)
}

# Test that se_x is minimum near inflection point
test_se_at_inflection <- function(best_fit) {
  pred <- best_fit$best_pred
  inflect_x <- best_fit$best_glance$inflect_x

  # Find the x value closest to inflection point
  closest_idx <- which.min(abs(pred$x - inflect_x))
  se_at_inflect <- pred$se_x[closest_idx]

  # SE should be minimum (or very close to minimum) at inflection
  min_se <- min(pred$se_x, na.rm = TRUE)

  cat("SE at inflection point:", se_at_inflect, "\n")
  cat("Minimum SE in curve:", min_se, "\n")
  cat("Ratio (should be ~1.0):", se_at_inflect / min_se, "\n")

  # Visual check
  plot(pred$x, pred$se_x, type = "l",
       xlab = "Concentration", ylab = "SE(x)")
  abline(v = inflect_x, col = "red", lty = 2)
  points(pred$x[closest_idx], se_at_inflect, col = "red", pch = 19)
}

gate_samples <- function(best_fit,
                         response_variable,
                         pcov_threshold,
                         verbose = TRUE) {
  sample_se <- best_fit$sample_se

  # Ensure all threshold values are scalar numerics (not lists)
  lloq_x <- as.numeric(best_fit$best_glance$lloq)[1]
  uloq_x <- as.numeric(best_fit$best_glance$uloq)[1]
  ulod <- as.numeric(best_fit$best_glance$ulod)[1]
  llod <- as.numeric(best_fit$best_glance$llod)[1]
  inflect_x <- as.numeric(best_fit$best_glance$inflect_x)[1]

  sample_se$gate_class_loq <- ifelse(sample_se$predicted_concentration >= lloq_x &sample_se$predicted_concentration <= uloq_x,
                                     "Acceptable",
                                     ifelse(sample_se$predicted_concentration > uloq_x, "Too Concentrated", "Too Diluted"))

  sample_se$gate_class_lod <- ifelse(sample_se[[response_variable]] >= llod & sample_se[[response_variable]] <= ulod,
                                     "Acceptable",
                                     ifelse(sample_se[[response_variable]] > ulod, "Too Concentrated", "Too Diluted"))

  sample_se$gate_class_pcov <- ifelse(sample_se$pcov <= pcov_threshold,
                                      "Acceptable",
                                      ifelse(sample_se$predicted_concentration < inflect_x, "Too Diluted", "Too Concentrated"))

  best_fit$sample_se <- sample_se

  return(best_fit)


}
