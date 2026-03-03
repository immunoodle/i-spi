### Functions for nonlinear standard curve fitting and visualization
## R package by Seamus, Scot, and Annie

# This function returns the lower and upper constraints for an antigen given its method
# methods are  ['default','user_defined','range_of_blanks', 'geometric_mean_of_blanks']

### The Standard Curve MFI is already logged transformed but the blanks are not
obtain_lower_constraint <- function(dat, antigen, study_accession, experiment_accession, plate, plateid, plate_blanks, antigen_constraints) {

  # Handle case where antigen_constraints is a dataframe with multiple rows
  # Take the first row to ensure scalar values for all constraint parameters
  if (is.data.frame(antigen_constraints) && nrow(antigen_constraints) > 1) {
    warning(paste("Multiple constraint rows found for antigen:", antigen,
                  "- using first row. Consider deduplicating antigen_constraints."))
    antigen_constraints <- antigen_constraints[1, , drop = FALSE]
  }

  # Extract scalar values from antigen_constraints to avoid "condition has length > 1" errors
  # Use helper function to safely extract first non-NA value
  safe_extract <- function(x, default = NA) {
    if (is.null(x) || length(x) == 0) return(default)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(default)
    return(x[1])
  }

  constraint_method <- safe_extract(trimws(antigen_constraints$l_asy_constraint_method), "default")
  l_asy_min <- safe_extract(antigen_constraints$l_asy_min_constraint, 0)
  l_asy_max <- safe_extract(antigen_constraints$l_asy_max_constraint, NA)
  std_curve_conc <- safe_extract(antigen_constraints$standard_curve_concentration, 10000)
  pcov_thresh <- safe_extract(antigen_constraints$pcov_threshold, 20)

  # blank_data_plate <- blank_data[blank_data$plate == plate & blank_data$antigen == antigen,]
  if (nrow(plate_blanks) > 1) {
    se_blank_mfi <- sd(plate_blanks$mfi, na.rm = TRUE) / sqrt(sum(!is.na(plate_blanks$mfi)))
  } else {
    se_blank_mfi <- 0
  }

  if (constraint_method == "user_defined") {
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = l_asy_min,
      l_asy_max_constraint = l_asy_max,
      l_asy_constraint_method = constraint_method,
      std_error_blank = se_blank_mfi,
      standard_curve_concentration = std_curve_conc,
      pcov_threshold = pcov_thresh
    )
  } else if (constraint_method == "default") {
    l_asy_max_constraint_dat <- max(dat$mfi, na.rm = TRUE)
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = 0, # lower bound is set to 0
      l_asy_max_constraint = max(dat$mfi, na.rm = T),
      l_asy_constraint_method = constraint_method,
      std_error_blank = se_blank_mfi,
      standard_curve_concentration = std_curve_conc,
      pcov_threshold = pcov_thresh
    )
  } else if (constraint_method == "range_of_blanks") {
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = min(plate_blanks$mfi),
      l_asy_max_constraint = max(plate_blanks$mfi),
      l_asy_constraint_method = constraint_method,
      std_error_blank =  se_blank_mfi,
      standard_curve_concentration = std_curve_conc,
      pcov_threshold = pcov_thresh
    )
  } else if (constraint_method == 'geometric_mean_of_blanks') {
    geometric_mean <- exp(mean(log(plate_blanks$mfi), na.rm = TRUE))
    l_asy_constraints <- list(
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      #plateid = plateid,
      plate = plate,
      antigen = antigen,
      l_asy_min_constraint = geometric_mean,
      l_asy_max_constraint = geometric_mean,
      l_asy_constraint_method = constraint_method,
      std_error_blank = se_blank_mfi,
      standard_curve_concentration = std_curve_conc,
      pcov_threshold = pcov_thresh
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
    nominal_sample_dilution = unique(data$nominal_sample_dilution),
    sampleid = "blank_mean",
    well = "geometric_mean_blank",
    dilution = NA_real_,
    antigen = unique(data$antigen),
    !!response_variable := response_blank,
    assay_response_variable = unique(data$assay_response_variable),
    assay_independent_variable = unique(data$assay_independent_variable),
    concentration = conc_blank

  )

  # if plate nom is present place in correct spot
  if ("plate_nom" %in% names(data)) {
    new_point$plate_nom <- unique(data$plate_nom)[1]

    nm <- names(new_point)
    i  <- match("assay_independent_variable", nm)

    new_point <- new_point[
      , c(nm[1:i], "plate_nom", nm[(i + 1):(length(nm) - 1)])
    ]
  }


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
  pred_df    <- plot_data$pred_df[plot_data$pred_df$model == selected_model_name,]
  d2xy_df    <- plot_data$d2xy_df[plot_data$d2xy_df$model == selected_model_name,]
  dydx_df    <- plot_data$dydx_df[plot_data$dydx_df$model == selected_model_name,]
  curve_ci_df <- if (!is.null(plot_data$ci_df))
    plot_data$ci_df[plot_data$ci_df$model == selected_model_name,]
  else NULL

  return(list(best_model_name = selected_model_name, best_fit = selected_fit, best_data = selected_data,
              best_ci = selected_params, best_pred = pred_df,
              best_d2xy = d2xy_df, best_dydx = dydx_df, best_curve_ci = curve_ci_df))

}

# Helper: dispatch to the correct dydx<model> function by name
dispatch_dydx <- function(model_name, x, theta) {
  fn <- match.fun(paste0("dydx", model_name))
  args <- c(list(x = x), as.list(theta[intersect(names(theta), c("a","b","c","d","g"))]))
  do.call(fn, args)
}

# .compute_fda2018_scalars()
#
# Internal helper – all FDA-2018 LOQ arithmetic lives here.
# Returns a plain named list of exactly 6 scalars (NA_real_ on failure).
#
# Arguments mirror the relevant subset of fit_fda2018_loq(); see that
# function's header for full protocol description.
.compute_fda2018_scalars <- function(fit,
                                     best_data,
                                     model_name,
                                     plate_blanks,
                                     response_variable,
                                     independent_variable,
                                     fixed_a_result    = NULL,
                                     is_log_response   = FALSE,
                                     cv_threshold      = 20,
                                     lloq_cv_threshold = 25,
                                     accuracy_lo       = 80,
                                     accuracy_hi       = 120,
                                     verbose           = TRUE) {

  # ------------------------------------------------------------------
  # 1. Blank statistics (raw linear-scale MFI from plate_blanks)
  # ------------------------------------------------------------------
  if (!is.null(plate_blanks) && nrow(plate_blanks) > 0) {
    blank_vals <- plate_blanks$mfi
    Blank_mean <- mean(blank_vals, na.rm = TRUE)
    Blank_SD   <- sd(blank_vals,   na.rm = TRUE)
    if (is.na(Blank_SD)) Blank_SD <- 0
  } else {
    Blank_mean <- NA_real_
    Blank_SD   <- NA_real_
  }

  # Convenience: the six-scalar NA result used for early returns
  na_scalars <- list(
    LLOQ_FDA2018_response      = NA_real_,
    LLOQ_FDA2018_concentration = NA_real_,
    ULOQ_FDA2018_response      = NA_real_,
    ULOQ_FDA2018_concentration = NA_real_,
    Blank_mean                 = Blank_mean,
    Blank_SD                   = Blank_SD
  )

  # ------------------------------------------------------------------
  # 2. Strip the geometric-mean blank row (stype == "B")
  # ------------------------------------------------------------------
  if ("stype" %in% names(best_data)) {
    std_data <- best_data[is.na(best_data$stype) | best_data$stype != "B", ]
  } else {
    std_data <- best_data
  }

  if (nrow(std_data) == 0) return(na_scalars)

  # ------------------------------------------------------------------
  # 3. Back-calculate concentration for every standard well
  # ------------------------------------------------------------------
  std_data_bc <- tryCatch(
    calculate_predicted_concentration(
      model_name        = model_name,
      fit               = fit,
      plate_samples     = std_data,
      fixed_constraint  = fixed_a_result,
      response_variable = response_variable,
      is_log_response   = is_log_response,
      verbose           = verbose
    ),
    error = function(e) {
      if (verbose) message("[FDA2018] back-calculation failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(std_data_bc)) return(na_scalars)

  # ------------------------------------------------------------------
  # 4. Per-level %CV and % recovery
  # ------------------------------------------------------------------
  nominal_concs <- sort(unique(std_data_bc[[independent_variable]]))

  loq_rows <- lapply(nominal_concs, function(nom_x) {
    rows <- std_data_bc[std_data_bc[[independent_variable]] == nom_x, ]
    bc   <- rows$predicted_concentration          # log10-scale
    bc   <- bc[!is.na(bc)]
    n    <- length(bc)
    if (n < 1) return(NULL)

    mean_bc      <- mean(bc)
    sd_bc        <- if (n > 1) sd(bc) else NA_real_
    cv_pct       <- if (!is.na(sd_bc) && mean_bc != 0)
      (sd_bc / abs(mean_bc)) * 100
    else NA_real_
    pct_recovery <- (10^mean_bc / 10^nom_x) * 100  # both on linear scale

    data.frame(
      nominal_log_x = nom_x,
      nominal_conc  = 10^nom_x,
      n_reps        = n,
      mean_log_bc   = mean_bc,
      sd_log_bc     = sd_bc,
      cv_pct        = cv_pct,
      pct_recovery  = pct_recovery,
      stringsAsFactors = FALSE
    )
  })

  loq_table <- do.call(rbind, Filter(Negate(is.null), loq_rows))
  if (is.null(loq_table) || nrow(loq_table) == 0) return(na_scalars)

  # ------------------------------------------------------------------
  # 5. Flag passing levels
  # ------------------------------------------------------------------
  passes_accuracy <- function(r) !is.na(r) & r >= accuracy_lo & r <= accuracy_hi

  loq_table$passes_std  <- !is.na(loq_table$cv_pct) &
    loq_table$cv_pct <= cv_threshold &
    passes_accuracy(loq_table$pct_recovery)

  loq_table$passes_lloq <- !is.na(loq_table$cv_pct) &
    loq_table$cv_pct <= lloq_cv_threshold &
    passes_accuracy(loq_table$pct_recovery)

  lloq_candidates <- which(loq_table$passes_lloq)
  passing_std     <- which(loq_table$passes_std)

  # ------------------------------------------------------------------
  # 6. Fitted response (linear scale) at a given log10-concentration
  # ------------------------------------------------------------------
  pred_response_at <- function(log_x) {
    nd   <- setNames(data.frame(log_x), independent_variable)
    yhat <- as.numeric(predict(fit, newdata = nd))
    if (is_log_response) 10^yhat else yhat
  }

  lloq_conc     <- NA_real_
  lloq_response <- NA_real_
  uloq_conc     <- NA_real_
  uloq_response <- NA_real_

  if (length(lloq_candidates) > 0) {
    lloq_row      <- loq_table[min(lloq_candidates), ]
    lloq_conc     <- lloq_row$nominal_conc
    lloq_response <- pred_response_at(lloq_row$nominal_log_x)
  }

  if (length(passing_std) > 0) {
    uloq_row      <- loq_table[max(passing_std), ]
    uloq_conc     <- uloq_row$nominal_conc
    uloq_response <- pred_response_at(uloq_row$nominal_log_x)
  }

  if (verbose) {
    message(sprintf(
      "[FDA2018] LLOQ: conc=%s, response=%s | ULOQ: conc=%s, response=%s",
      format(lloq_conc,     digits = 4),
      format(lloq_response, digits = 4),
      format(uloq_conc,     digits = 4),
      format(uloq_response, digits = 4)
    ))
  }

  list(
    LLOQ_FDA2018_response      = lloq_response,
    LLOQ_FDA2018_concentration = lloq_conc,
    ULOQ_FDA2018_response      = uloq_response,
    ULOQ_FDA2018_concentration = uloq_conc,
    Blank_mean                 = Blank_mean,
    Blank_SD                   = Blank_SD
  )
}

# This function depends on get_loqs, generate_inflection_point, generate_lods as it is a wrapper and calls them
# fit_qc_glance()
#
# Wrapper that assembles the QC-glance data-frame for a single plate fit.
# Optionally computes the six FDA-2018 LOQ scalars in the same pass so that
# no second call is required.
#
# New / changed arguments vs. the original:
#   plate_blanks    – data.frame of blank wells with column `mfi` (raw,
#                     linear scale). Pass NULL to skip FDA-2018 computation.
#   fda2018_options – named list with any of:
#                       cv_threshold      (default 20)
#                       lloq_cv_threshold (default 25)
#                       accuracy_lo       (default 80)
#                       accuracy_hi       (default 120)
#                     Pass NULL (default) to use all defaults.
#
# All other arguments are unchanged from the original.
fit_qc_glance <- function(best_fit,
                          response_variable,
                          independent_variable,
                          fixed_a_result,
                          antigen_settings,
                          antigen_fit_options,
                          plate_blanks     = NULL,
                          fda2018_options  = NULL,
                          verbose          = TRUE) {

  # ------------------------------------------------------------------
  # Unpack frequently-used members once
  # ------------------------------------------------------------------
  fit        <- best_fit$best_fit
  best_data  <- best_fit$best_data
  model_name <- best_fit$best_model_name

  if (antigen_settings$l_asy_constraint_method == "range_of_blanks" &&
      antigen_fit_options$is_log_response &&
      !is.null(fixed_a_result)) {
    .eps           <- 0.00005
    fixed_a_result <- log10(fixed_a_result + .eps)
  }

  # ------------------------------------------------------------------
  # QC metrics (unchanged logic)
  # ------------------------------------------------------------------
  loqs <- get_loqs(
    best_d2xy           = best_fit$best_d2xy,
    fit                 = fit,
    independent_variable = independent_variable
  )

  blank_se_value <- get_blank_se(antigen_settings = antigen_settings)

  lods <- generate_lods(
    best_fit        = best_fit,
    fixed_a_result  = fixed_a_result,
    std_error_blank = blank_se_value
  )

  inflection_point <- generate_inflection_point(
    model_name           = model_name,
    fit                  = fit,
    fixed_a_result       = fixed_a_result,
    independent_variable = independent_variable
  )

  mdc_rdl <- generate_mdc_rdl(
    best_fit             = best_fit,
    lods                 = lods,
    independent_variable = independent_variable,
    verbose              = verbose
  )

  theta      <- coef(fit)
  theta["a"] <- ifelse(!is.null(fixed_a_result), fixed_a_result, theta["a"])

  xi            <- as.numeric(inflection_point$inflect_x)
  dydx_inflect  <- as.numeric(dispatch_dydx(model_name, xi, theta))

  # ------------------------------------------------------------------
  # FDA-2018 six scalars  (NA when plate_blanks is NULL)
  # ------------------------------------------------------------------
  fda_opts <- if (is.null(fda2018_options)) list() else fda2018_options

  fda2018_scalars <- .compute_fda2018_scalars(
    fit                  = fit,
    best_data            = best_data,
    model_name           = model_name,
    plate_blanks         = plate_blanks,
    response_variable    = response_variable,
    independent_variable = independent_variable,
    fixed_a_result       = fixed_a_result,
    is_log_response      = antigen_fit_options$is_log_response,
    cv_threshold         = if (!is.null(fda_opts$cv_threshold))      fda_opts$cv_threshold      else 20,
    lloq_cv_threshold    = if (!is.null(fda_opts$lloq_cv_threshold)) fda_opts$lloq_cv_threshold else 25,
    accuracy_lo          = if (!is.null(fda_opts$accuracy_lo))       fda_opts$accuracy_lo       else 80,
    accuracy_hi          = if (!is.null(fda_opts$accuracy_hi))       fda_opts$accuracy_hi       else 120,
    verbose              = verbose
  )

  # ------------------------------------------------------------------
  # Merge all scalars into one flat list, then coerce to data.frame
  # ------------------------------------------------------------------
  combined_qc_list <- c(
    loqs,
    lods,
    inflection_point,
    std_error_blank = blank_se_value,
    dydx_inflect    = dydx_inflect,
    mdc_rdl,
    fda2018_scalars          # <-- six new columns land here
  )

  combined_qc_list <- lapply(combined_qc_list, function(x) {
    if (is.list(x)) as.numeric(unlist(x))[1] else as.numeric(x)[1]
  })

  qc_glance <- as.data.frame(combined_qc_list, stringsAsFactors = FALSE)

  # ULOQ sanity-check (unchanged)
  uloq_val   <- as.numeric(qc_glance$uloq)[1]
  inflect_val <- as.numeric(qc_glance$inflect_x)[1]
  if (!is.na(uloq_val) && !is.na(inflect_val) && uloq_val < inflect_val) {
    qc_glance$uloq   <- NA_real_
    qc_glance$uloq_y <- NA_real_
  }

  # ------------------------------------------------------------------
  # Fit statistics and parameter estimates (unchanged)
  # ------------------------------------------------------------------
  s       <- summary(fit)
  coefs   <- coef(s)
  coef_df <- as.data.frame(t(coefs[, "Estimate"]))
  if (!("a" %in% names(coef_df))) coef_df$a <- fixed_a_result

  sigma    <- s$sigma
  df_resid <- s$df[2]

  response  <- best_data[[response_variable]]
  rss       <- sum(residuals(fit)^2)
  tss       <- sum((response - mean(response))^2)
  r_squared <- 1 - rss / tss

  aic         <- AIC(fit)
  bic         <- BIC(fit)
  logLik_val  <- as.numeric(logLik(fit))
  converged   <- fit$convInfo$isConv
  iter        <- fit$convInfo$finIter
  crit        <- model_name
  model_formula <- gsub(
    "I\\((.*)\\)", "\\1",
    paste(deparse(formula(fit)), collapse = " ")
  )
  n_obs        <- length(residuals(fit))
  mse          <- mean(resid(fit)^2, na.rm = TRUE)
  mean_obs_mfi <- mean(response,     na.rm = TRUE)
  cv           <- (sqrt(mse) / mean_obs_mfi) * 100

  # ------------------------------------------------------------------
  # Assemble glance_df  (qc_glance already contains the 6 FDA columns)
  # ------------------------------------------------------------------
  glance_df <- data.frame(
    study_accession        = unique(best_data$study_accession),
    experiment_accession   = unique(best_data$experiment_accession),
    plateid                = unique(best_data$plateid),
    plate                  = unique(best_data$plate),
    nominal_sample_dilution = unique(best_data$nominal_sample_dilution),
    antigen                = unique(best_data$antigen),
    iter                   = iter,
    status                 = converged,
    crit                   = crit,
    coef_df,
    qc_glance,             # includes LLOQ/ULOQ FDA2018 + Blank_mean/SD
    dfresidual             = df_resid,
    nobs                   = n_obs,
    rsquare_fit            = r_squared,
    aic                    = aic,
    bic                    = bic,
    loglik                 = logLik_val,
    mse                    = mse,
    cv                     = cv,
    source                 = unique(best_data$source),
    bkg_method             = antigen_fit_options$blank_option,
    is_log_response        = antigen_fit_options$is_log_response,
    is_log_x               = antigen_fit_options$is_log_concentration,
    apply_prozone          = antigen_fit_options$apply_prozone,
    formula                = model_formula
  )

  best_fit$best_glance <- glance_df
  return(best_fit)
}

# fit_qc_glance <- function(best_fit,
#                           response_variable,
#                           independent_variable,
#                           fixed_a_result,
#                           antigen_settings,
#                           antigen_fit_options,
#                           verbose = TRUE) {
#
#
#   # Refactor 3: Extract frequently-used members once
#   fit        <- best_fit$best_fit
#   best_data  <- best_fit$best_data
#   model_name <- best_fit$best_model_name
#
#   if (antigen_settings$l_asy_constraint_method == "range_of_blanks" && antigen_fit_options$is_log_response && !is.null(fixed_a_result)) {
#     .eps = 0.00005
#     fixed_a_result <- log10(fixed_a_result + .eps)
#   }
#   # obtain qc metrics
#   loqs <- get_loqs(best_d2xy = best_fit$best_d2xy, fit = fit, independent_variable = independent_variable)
#
#   # Refactor 5: keep blank SE as plain scalar until final assembly
#   blank_se_value <- get_blank_se(antigen_settings = antigen_settings)
#
#   lods <- generate_lods(best_fit = best_fit, fixed_a_result = fixed_a_result, std_error_blank = blank_se_value)
#
#   inflection_point <- generate_inflection_point(model_name = model_name, fit = fit, fixed_a_result = fixed_a_result, independent_variable = independent_variable)
#
#   # Refactor 1: pass pre-computed lods into generate_mdc_rdl
#   mdc_rdl <- generate_mdc_rdl(best_fit = best_fit, lods = lods,
#                                independent_variable = independent_variable,
#                                verbose = verbose)
#
#   theta <- coef(fit)
#   theta["a"] <- ifelse(!is.null(fixed_a_result), fixed_a_result, theta["a"])
#   # Use inflect_x (concentration) for the derivative, not inflect_y (response)
#   # The dydx functions take x (concentration) as the first argument
#   xi <- as.numeric(inflection_point$inflect_x)
#   # Refactor 4: use dispatch_dydx helper instead of switch block
#   dydx_inflect <- as.numeric(dispatch_dydx(model_name, xi, theta))
#
#   combined_qc_list <- c(loqs, lods, inflection_point, std_error_blank = blank_se_value, dydx_inflect = dydx_inflect, mdc_rdl)
#
#   # Ensure all values in the list are scalar numerics before creating dataframe
#   combined_qc_list <- lapply(combined_qc_list, function(x) {
#     if (is.list(x)) as.numeric(unlist(x))[1] else as.numeric(x)[1]
#   })
#
#   qc_glance <- as.data.frame(combined_qc_list, stringsAsFactors = FALSE)
#
#   # Ensure scalar comparisons
#   uloq_val <- as.numeric(qc_glance$uloq)[1]
#   inflect_val <- as.numeric(qc_glance$inflect_x)[1]
#
#   if (!is.na(uloq_val) && !is.na(inflect_val) && uloq_val < inflect_val) {
#     qc_glance$uloq <- NA_real_
#     qc_glance$uloq_y <- NA_real_
#   }
#   # obtain fit statistics and parameter estimates
#   s <-  summary(fit)
#   coefs <- coef(s)
#   coef_df <- as.data.frame(t(coefs[, "Estimate"]))
#
#   if (!("a" %in% names(coef_df))) {
#     coef_df$a <- fixed_a_result
#   }
#
#   # Residual stats
#   sigma <- s$sigma
#   df_resid <- s$df[2]
#
#   # Compute R-squared
#   response <- best_data[[response_variable]]
#
#   rss <- sum(residuals(fit)^2)
#   tss <- sum((response - mean(response))^2)
#   r_squared <- 1 - rss / tss
#
#   # AIC and BIC
#   aic <- AIC(fit)
#   bic <- BIC(fit)
#
#   logLik_val <- as.numeric(logLik(fit))
#   converged <- fit$convInfo$isConv
#   iter <- fit$convInfo$finIter
#
#   crit <- model_name
#
#   model_formula <- paste(deparse(formula(fit)), collapse = " ")
#   model_formula <- gsub("I\\((.*)\\)", "\\1", model_formula)
#
#   n_obs <- length(residuals(fit))
#
#   # calculate mse
#   mse <- mean(resid(fit)^2, na.rm = T)
#   # mean of observed response
#   mean_obs_mfi<- mean(response, na.rm = TRUE)
#   # coefficient of variation.
#   cv <- (sqrt(mse) / mean_obs_mfi) * 100
#
#
#   glance_df <-  data.frame(
#     study_accession = unique(best_data$study_accession),
#     experiment_accession = unique(best_data$experiment_accession),
#     plateid = unique(best_data$plateid),
#     plate = unique(best_data$plate),
#     nominal_sample_dilution = unique(best_data$nominal_sample_dilution),
#     antigen = unique(best_data$antigen),
#     iter = iter,
#     status = converged,
#     crit = crit,
#     coef_df,
#     qc_glance,
#     dfresidual = df_resid,
#     nobs = n_obs,
#     rsquare_fit = r_squared,
#     aic = aic,
#     bic = bic,
#     loglik = logLik_val,
#     mse = mse,
#     cv = cv,
#     source = unique(best_data$source),
#     bkg_method =  antigen_fit_options$blank_option, #blank_option,
#     is_log_response = antigen_fit_options$is_log_response,
#     is_log_x = antigen_fit_options$is_log_concentration,
#     apply_prozone  = antigen_fit_options$apply_prozone,
#     formula = model_formula
#     )
#
#   best_fit$best_glance <- glance_df
#   return(best_fit)
#
# }

# Compute FDA (2018) LLOQ, ULOQ, Blank_mean, and Blank_SD for a single plate.
#
# Protocol (FDA Bioanalytical Method Validation Guidance, 2018 / DeSilva 2003):
#   LLOQ: lowest nominal concentration at which %CV ≤ lloq_cv_threshold (25%)
#         AND % recovery is within [accuracy_lo, accuracy_hi] (80–120%).
#   ULOQ: highest nominal concentration at which %CV ≤ cv_threshold (20%)
#         AND % recovery is within [accuracy_lo, accuracy_hi].
#   Blank_mean / Blank_SD: mean and SD of raw blank (PBS) MFI values.
#
# Arguments:
#   best_fit           – list returned by select_model_fit_AIC() (and fit_qc_glance()).
#   plate_blanks       – data.frame of blank wells with column `mfi` (raw, linear scale).
#   response_variable  – character name of the MFI column in best_data.
#   independent_variable – character name of the log10-concentration column in best_data.
#   fixed_a_result     – fixed lower-asymptote value (NULL if not fixed).
#   is_log_response    – logical; TRUE if the response was log10-transformed before fitting.
#   cv_threshold       – %CV acceptance limit for all levels except LLOQ (default 20).
#   lloq_cv_threshold  – %CV acceptance limit at the LLOQ level (default 25, per FDA 2018).
#   accuracy_lo / accuracy_hi – % recovery acceptance window (default 80–120%).
#
# Returns:
#   best_fit with a new element $fda2018 containing a named list of the six scalars
#   plus a fda2018_loq_table data.frame with per-level diagnostics.
# fit_fda2018_loq <- function(best_fit,
#                              plate_blanks,
#                              response_variable,
#                              independent_variable,
#                              fixed_a_result    = NULL,
#                              is_log_response   = FALSE,
#                              cv_threshold      = 20,
#                              lloq_cv_threshold = 25,
#                              accuracy_lo       = 80,
#                              accuracy_hi       = 120,
#                              verbose           = TRUE) {
#
#   fit        <- best_fit$best_fit
#   best_data  <- best_fit$best_data
#   model_name <- best_fit$best_model_name
#
#   # --- 1. Blank statistics from raw plate_blanks (mfi column, linear scale) ---
#   if (!is.null(plate_blanks) && nrow(plate_blanks) > 0) {
#     blank_vals <- plate_blanks$mfi
#     Blank_mean <- mean(blank_vals, na.rm = TRUE)
#     Blank_SD   <- sd(blank_vals,   na.rm = TRUE)
#     if (is.na(Blank_SD)) Blank_SD <- 0
#   } else {
#     Blank_mean <- NA_real_
#     Blank_SD   <- NA_real_
#   }
#
#   # --- 2. Strip the geometric-mean blank row (stype == "B") from best_data ---
#   if ("stype" %in% names(best_data)) {
#     std_data <- best_data[is.na(best_data$stype) | best_data$stype != "B", ]
#   } else {
#     std_data <- best_data
#   }
#
#   na_result <- list(
#     LLOQ_FDA2018_response      = NA_real_,
#     LLOQ_FDA2018_concentration = NA_real_,
#     ULOQ_FDA2018_response      = NA_real_,
#     ULOQ_FDA2018_concentration = NA_real_,
#     Blank_mean                 = Blank_mean,
#     Blank_SD                   = Blank_SD,
#     fda2018_loq_table          = NULL
#   )
#
#   if (nrow(std_data) == 0) {
#     best_fit$fda2018 <- na_result
#     return(best_fit)
#   }
#
#   # --- 3. Back-calculate concentration for every standard well ---
#   std_data_bc <- calculate_predicted_concentration(
#     model_name        = model_name,
#     fit               = fit,
#     plate_samples     = std_data,
#     fixed_constraint  = fixed_a_result,
#     response_variable = response_variable,
#     is_log_response   = is_log_response,
#     verbose           = verbose
#   )
#   # predicted_concentration is on the log10 concentration scale
#
#   # --- 4. Per-level %CV and % recovery ---
#   nominal_concs <- sort(unique(std_data_bc[[independent_variable]]))
#
#   loq_rows <- lapply(nominal_concs, function(nom_x) {
#     rows <- std_data_bc[std_data_bc[[independent_variable]] == nom_x, ]
#     bc   <- rows$predicted_concentration   # log10-scale
#     bc   <- bc[!is.na(bc)]
#     n    <- length(bc)
#     if (n < 1) return(NULL)
#
#     mean_bc      <- mean(bc)
#     sd_bc        <- if (n > 1) sd(bc) else NA_real_
#     cv_pct       <- if (!is.na(sd_bc) && mean_bc != 0) (sd_bc / abs(mean_bc)) * 100 else NA_real_
#     pct_recovery <- (10^mean_bc / 10^nom_x) * 100   # both converted to linear scale
#
#     data.frame(
#       nominal_log_x = nom_x,
#       nominal_conc  = 10^nom_x,
#       n_reps        = n,
#       mean_log_bc   = mean_bc,
#       sd_log_bc     = sd_bc,
#       cv_pct        = cv_pct,
#       pct_recovery  = pct_recovery,
#       stringsAsFactors = FALSE
#     )
#   })
#
#   loq_table <- do.call(rbind, Filter(Negate(is.null), loq_rows))
#
#   if (is.null(loq_table) || nrow(loq_table) == 0) {
#     best_fit$fda2018 <- na_result
#     return(best_fit)
#   }
#
#   # --- 5. Flag passing levels ---
#   passes_accuracy <- function(pct_rec) !is.na(pct_rec) & pct_rec >= accuracy_lo & pct_rec <= accuracy_hi
#
#   loq_table$passes_std  <- !is.na(loq_table$cv_pct) & loq_table$cv_pct <= cv_threshold      & passes_accuracy(loq_table$pct_recovery)
#   loq_table$passes_lloq <- !is.na(loq_table$cv_pct) & loq_table$cv_pct <= lloq_cv_threshold & passes_accuracy(loq_table$pct_recovery)
#
#   lloq_candidates <- which(loq_table$passes_lloq)
#   passing_std     <- which(loq_table$passes_std)
#
#   # Helper: fitted response (linear scale) at a given log10-concentration
#   pred_response_at <- function(log_x) {
#     nd   <- setNames(data.frame(log_x), independent_variable)
#     yhat <- as.numeric(predict(fit, newdata = nd))
#     if (is_log_response) 10^yhat else yhat
#   }
#
#   lloq_conc     <- NA_real_
#   lloq_response <- NA_real_
#   uloq_conc     <- NA_real_
#   uloq_response <- NA_real_
#
#   if (length(lloq_candidates) > 0) {
#     lloq_row      <- loq_table[min(lloq_candidates), ]
#     lloq_conc     <- lloq_row$nominal_conc
#     lloq_response <- pred_response_at(lloq_row$nominal_log_x)
#   }
#
#   if (length(passing_std) > 0) {
#     uloq_row      <- loq_table[max(passing_std), ]
#     uloq_conc     <- uloq_row$nominal_conc
#     uloq_response <- pred_response_at(uloq_row$nominal_log_x)
#   }
#
#   if (verbose) {
#     message(sprintf(
#       "FDA2018 LOQ — LLOQ: conc=%s, response=%s | ULOQ: conc=%s, response=%s",
#       format(lloq_conc, digits = 4), format(lloq_response, digits = 4),
#       format(uloq_conc, digits = 4), format(uloq_response, digits = 4)
#     ))
#   }
#
#   best_fit$fda2018 <- list(
#     LLOQ_FDA2018_response      = lloq_response,
#     LLOQ_FDA2018_concentration = lloq_conc,
#     ULOQ_FDA2018_response      = uloq_response,
#     ULOQ_FDA2018_concentration = uloq_conc,
#     Blank_mean                 = Blank_mean,
#     Blank_SD                   = Blank_SD
#     # ,
#     # fda2018_loq_table          = loq_table
#   )
#
#   return(best_fit)
# }

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

  # Calculate x-coordinate of inflection point analytically
  # The inflection point is where the second derivative equals zero
  inflect_x  <- tryCatch({
    if (model_name == "Y5") {
      # For 5 parameter model: x_inflect = c - b*ln(g)
      c - b * log(g)
    } else if (model_name == "Y4") {
      # For 4 parameter logistic: x_inflect = c
      c
    } else if (model_name == "Yd5") {
      # For 5 parameter decreasing logistic: x_inflect = c + ln(g)/b
      c + (log(g) / b)
    } else if (model_name == "Yd4") {
      # For 4 parameter log-logistic: x_inflect = c
      c
    } else if (model_name == "Ygomp4") {
      # For Gompertz: x_inflect = c (where second derivative = 0)
      c
    }
  }, error = function(e) NA)

  inflect_x <- as.numeric(inflect_x)

  # Calculate y-coordinate by evaluating the fitted model at inflect_x
  # This ensures the inflection point lies exactly on the fitted curve
  inflect_y <- tryCatch({
    # Use predict() to evaluate the fitted model at the inflection point
    # This is more robust than analytical formulas as it uses the actual fitted model
    newdata <- setNames(data.frame(x = inflect_x), independent_variable)
    predicted_y <- predict(fit, newdata = newdata)
    as.numeric(predicted_y)
  }, error = function(e) {
    # Fallback to analytical calculation if predict fails
    if (verbose) message("predict() failed, using analytical formula for inflect_y")
    tryCatch({
      switch(model_name,
             "Y5" = d + (a - d) / (1 + exp((inflect_x - c) / b))^g,
             "Y4" = d + (a - d) / (1 + exp((inflect_x - c) / b)),
             "Yd5" = a + (d - a) * (1 + g * exp(-b * (inflect_x - c)))^(-1/g),
             "Yd4" = a + (d - a) / (1 + exp(-b * (inflect_x - c))),
             "Ygomp4" = a + (d - a) * exp(-exp(-b * (inflect_x - c))),
             NA
      )
    }, error = function(e2) NA)
  })

  inflect_y <- as.numeric(inflect_y)

  return(list(inflect_x = inflect_x, inflect_y = inflect_y))
}

generate_lods <- function(best_fit, fixed_a_result, std_error_blank,  verbose = TRUE) {

  best_ci <- best_fit$best_ci
  best_data <- best_fit$best_data

  ulod <- best_ci[best_ci$parameter == "d",]$conf.low

  if (!is.null(fixed_a_result)) {
    if (is.null(std_error_blank) || is.na(std_error_blank)) {
      std_error_blank <- 0
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

generate_mdc_rdl <- function(best_fit, lods,
                             independent_variable, verbose = TRUE) {

  # Refactor 1: accept pre-computed lods instead of recomputing
  llod <- as.numeric(lods$llod)
  ulod <- as.numeric(lods$ulod)

  fit        <- best_fit$best_fit
  best_data  <- best_fit$best_data

  # x-range of the standards (search bounds for uniroot)
  x_data <- best_data[[independent_variable]]
  x_lo   <- min(x_data, na.rm = TRUE)
  x_hi   <- max(x_data, na.rm = TRUE)

  # Degrees of freedom for t-quantile (used by CI calculations)
  n_params <- length(coef(fit))
  n_obs    <- nrow(best_data)
  t_crit   <- qt(0.975, df = n_obs - n_params)

  # Variance-covariance matrix of fitted parameters
  V <- vcov(fit)

  # Refactor 2: hoist invariants out of pred_se inner loop
  theta <- coef(fit)
  p     <- length(theta)
  rhs   <- as.list(formula(fit))[[3]]

  # --- helper: predicted y at a single x ----------------------------------
  pred_y <- function(x_val) {
    nd <- setNames(data.frame(x_val), independent_variable)
    as.numeric(predict(fit, newdata = nd))
  }

  # --- helper: SE of predicted y via delta method -------------------------
  #     grad(theta) evaluated numerically; se = sqrt(grad' V grad)
  #     theta, p, rhs, V are captured from enclosing scope (hoisted)
  pred_se <- function(x_val, eps = 1e-6) {
    y0   <- pred_y(x_val)
    nd   <- setNames(data.frame(x_val), independent_variable)
    grad <- vapply(seq_len(p), function(j) {
      theta_j    <- theta
      theta_j[j] <- theta[j] + eps
      env <- c(as.list(theta_j), as.list(nd))
      (as.numeric(eval(rhs, envir = env)) - y0) / eps
    }, numeric(1))
    sqrt(as.numeric(crossprod(grad, V %*% grad)))
  }

  # --- helper: safe uniroot wrapper --------------------------------------
  safe_uniroot <- function(f, lower, upper) {
    tryCatch({
      f_lo <- f(lower)
      f_hi <- f(upper)
      if (is.na(f_lo) || is.na(f_hi)) return(NA_real_)
      if (sign(f_lo) == sign(f_hi)) return(NA_real_)
      uniroot(f, lower = lower, upper = upper, tol = .Machine$double.eps^0.5)$root
    }, error = function(e) NA_real_)
  }

  # --- 1. mindc: fitted curve == llod -------------------------------------
  mindc <- NA_real_
  if (!is.na(llod)) {
    mindc <- safe_uniroot(function(x_val) pred_y(x_val) - llod,
                          lower = x_lo, upper = x_hi)
  }

  # --- 2. maxdc: fitted curve == ulod -------------------------------------
  maxdc <- NA_real_
  if (!is.na(ulod)) {
    maxdc <- safe_uniroot(function(x_val) pred_y(x_val) - ulod,
                          lower = x_lo, upper = x_hi)
  }

  # --- 3. minrdl: 2.5% CI of fitted curve == llod ------------------------
  #        lower CI = pred_y(x) - t * se(x)
  minrdl <- NA_real_
  if (!is.na(llod)) {
    minrdl <- safe_uniroot(
      function(x_val) (pred_y(x_val) - t_crit * pred_se(x_val)) - llod,
      lower = x_lo, upper = x_hi
    )
  }

  # --- 4. maxrdl: 97.5% CI of fitted curve == ulod -----------------------
  #        upper CI = pred_y(x) + t * se(x)
  maxrdl <- NA_real_
  if (!is.na(ulod)) {
    maxrdl <- safe_uniroot(
      function(x_val) (pred_y(x_val) + t_crit * pred_se(x_val)) - ulod,
      lower = x_lo, upper = x_hi
    )
  }

  if (verbose) {
    message(sprintf("MDC/RDL — mindc: %s, maxdc: %s, minrdl: %s, maxrdl: %s",
                    format(mindc, digits = 4), format(maxdc, digits = 4),
                    format(minrdl, digits = 4), format(maxrdl, digits = 4)))
  }

  list(
    mindc  = as.numeric(mindc),
    maxdc  = as.numeric(maxdc),
    minrdl = as.numeric(minrdl),
    maxrdl = as.numeric(maxrdl)
  )
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
  tidy_df$nominal_sample_dilution <- unique(best_fit$best_data$nominal_sample_dilution)
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
      nominal_sample_dilution = unique(best_fit$best_data$nominal_sample_dilution),
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

# ── Run this BEFORE calling propagate_error_dataframe ──────────────────────
diagnose_propagation_inputs <- function(fit, model, fixed_a, y_test = NULL) {

  params <- coef(fit)
  Sigma  <- vcov(fit)

  cat("\n═══ Propagation Input Diagnosis ═══\n")
  cat("Model         :", model, "\n")
  cat("coef(fit)     :", paste(names(params), "=", round(params, 5), collapse = ", "), "\n")
  cat("vcov dim      :", paste(dim(Sigma), collapse = " x "), "\n")
  cat("vcov rownames :", paste(rownames(Sigma), collapse = ", "), "\n")
  cat("fixed_a       :", if (is.null(fixed_a)) "NULL" else round(fixed_a, 6), "\n")

  # If fixed_a is supplied, 'a' should NOT be in coef(fit)
  if (!is.null(fixed_a) && "a" %in% names(params)) {
    cat("⚠️  WARNING: fixed_a is supplied BUT 'a' is ALSO in coef(fit).\n")
    cat("   This causes the augmented-Sigma path to corrupt the gradient alignment.\n")
    cat("   Solution: fit the model WITHOUT 'a' as a free parameter when fixed_a is used.\n")
  }

  # Test inv and grad at a sample y value
  if (!is.null(y_test)) {
    cat("\nTesting inv/grad at y =", y_test, "\n")
    fns <- tryCatch(
      make_inv_and_grad_fixed(model = model, y = y_test,
                              fixed_a = if (!is.null(fixed_a)) fixed_a else params["a"]),
      error = function(e) { cat("make_inv_and_grad_fixed ERROR:", e$message, "\n"); NULL }
    )
    if (!is.null(fns)) {
      x_est <- tryCatch(fns$inv(params),    error = function(e) { cat("inv() ERROR:", e$message,"\n"); NA })
      g_t   <- tryCatch(fns$grad(params),   error = function(e) { cat("grad() ERROR:", e$message,"\n"); NULL })
      g_y   <- tryCatch(fns$grad_y(params), error = function(e) { cat("grad_y() ERROR:", e$message,"\n"); NA })

      cat("  x_est    :", x_est, "\n")
      cat("  grad_t   :", if (!is.null(g_t)) paste(names(g_t), "=", round(g_t,5), collapse=", ") else "NULL", "\n")
      cat("  grad_y   :", g_y, "\n")

      # Check alignment
      if (!is.null(g_t)) {
        common <- intersect(names(g_t), rownames(Sigma))
        cat("  grad_t names  :", paste(names(g_t),   collapse=", "), "\n")
        cat("  Sigma rownames:", paste(rownames(Sigma), collapse=", "), "\n")
        cat("  Common names  :", paste(common, collapse=", "), "\n")
        if (length(common) == length(g_t) && length(common) == nrow(Sigma)) {
          g_vec  <- g_t[common]
          S_sub  <- Sigma[common, common, drop=FALSE]
          var_x  <- as.numeric(t(g_vec) %*% S_sub %*% g_vec)
          cat("  var_x (param contribution) :", var_x, "\n")
          cat("  se_x  (param contribution) :", sqrt(max(var_x, 0)), "\n")
        } else {
          cat("  ⚠️  NAME MISMATCH — this is the root cause of NA se_x!\n")
        }
      }
    }
  }
  cat("═══════════════════════════════════\n\n")
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
#'
propagate_error_dataframe <- function(pred_df,
                                      fit,
                                      model = c("Y4","Yd4","Ygomp4","Y5","Yd5"),
                                      y_col,
                                      se_col,
                                      fixed_a,
                                      cv_x_max = 125,
                                      is_log_x  = TRUE,   # is x_est on log10 scale?
                                      quiet = FALSE) {
  model <- match.arg(model)

  # Validate is_log_x
  is_log_x <- isTRUE(is_log_x)

  if (!quiet) {
    message("[propagate] CV formula   : ",
            if (is_log_x) "LINEAR-scale (se_x * ln(10) * 100) — avoids /0 at log10(conc)=0"
            else           "LOG-scale    (se_x / |x_est| * 100)")
  }

  # ── 1. Validate cv_x_max ────────────────────────────────────
  cv_x_max <- if (isTRUE(is.finite(cv_x_max)) && cv_x_max > 0) {
    as.numeric(cv_x_max)[1]
  } else {
    message("[propagate] cv_x_max invalid; defaulting to 125.")
    125
  }

  # ── 2. Extract params and Sigma from fit ─────────────────────
  params <- coef(fit)
  Sigma  <- vcov(fit)

  if (!quiet) {
    message("[propagate] Model       : ", model)
    message("[propagate] Free params : ", paste(names(params), collapse = ", "))
    message("[propagate] Sigma rows  : ", paste(rownames(Sigma), collapse = ", "))
    message("[propagate] fixed_a     : ",
            if (is.null(fixed_a)) "NULL (a is free in coef)" else round(as.numeric(fixed_a), 6))
  }

  # ── 3. Decide which branch make_inv_and_grad_fixed will use ──
  #
  #  fixed_a supplied as a real scalar → truly fixed, pass as-is (as.numeric)
  #  fixed_a is NULL                   → 'a' is free, pass NULL
  #
  #  We do NOT read 'a' from params and pass it as fixed_a here.
  #  When fixed_a = NULL, make_inv_and_grad_fixed Branch B reads
  #  p["a"] internally from coef(fit) and uses the full grad_* functions.

  use_fixed_a <- !is.null(fixed_a) && isTRUE(is.finite(as.numeric(fixed_a)))
  fna         <- if (use_fixed_a) as.numeric(fixed_a) else NULL

  # Sanity: when a is free it must be in coef(fit)
  if (!use_fixed_a && !"a" %in% names(params)) {
    stop("[propagate] fixed_a is NULL but 'a' not found in coef(fit).")
  }
  # Sanity: when a is fixed it must NOT be in coef(fit)
  if (use_fixed_a && "a" %in% names(params)) {
    warning("[propagate] fixed_a supplied but 'a' also in coef(fit). ",
            "The coef 'a' will be ignored; fixed_a will be used.")
    params <- params[names(params) != "a"]
    Sigma  <- Sigma[rownames(Sigma) != "a", colnames(Sigma) != "a", drop = FALSE]
  }

  # ── 4. Loop ──────────────────────────────────────────────────
  n   <- nrow(pred_df)
  res <- vector("list", n)
  pb  <- if (!quiet) txtProgressBar(min = 0, max = n, style = 3) else NULL

  n_na_inv  <- 0L
  n_na_grad <- 0L
  n_na_vpar <- 0L
  n_capped  <- 0L
  n_ok      <- 0L

  for (i in seq_len(n)) {

    y_i    <- pred_df[[y_col]][i]
    se_y_i <- if (isTRUE(is.finite(pred_df[[se_col]][i]))) pred_df[[se_col]][i] else 0

    # Scale sanity check (log10 response only — warn once)
    if (i == 1 && !quiet) {
      y_range <- diff(range(pred_df[[y_col]], na.rm = TRUE))
      se_median <- median(pred_df[[se_col]], na.rm = TRUE)
      if (isTRUE(is.finite(se_median)) && isTRUE(is.finite(y_range)) && y_range > 0) {
        se_to_range_ratio <- se_median / y_range
        if (se_to_range_ratio > 0.5) {
          message(sprintf(paste0(
            "\n[propagate] WARNING: median se_col (%.4f) is %.1f%% of the y_col range (%.4f).\n",
            "  This suggests se_col may be on a different scale than y_col.\n",
            "  If y_col is log10-transformed, se_col must also be in log10 units.\n",
            "  Convert: se_log10 = se_raw / (raw_value * log(10))"),
            se_median, se_to_range_ratio * 100, y_range
          ))
        }
      }
    }

    # Guard: skip non-finite y
    if (!isTRUE(is.finite(y_i))) {
      res[[i]] <- list(x_est = NA_real_, se_x = NA_real_, cv_x = cv_x_max)
      n_na_inv <- n_na_inv + 1L
      if (!quiet) setTxtProgressBar(pb, i)
      next
    }

    # Build closures — pass fna (NULL or scalar)
    fns <- tryCatch(
      make_inv_and_grad_fixed(model = model, y = y_i, fixed_a = fna),
      error = function(e) {
        if (!quiet) message(sprintf("[propagate] closure failed row %d: %s", i, e$message))
        NULL
      }
    )
    if (is.null(fns)) {
      res[[i]] <- list(x_est = NA_real_, se_x = NA_real_, cv_x = cv_x_max)
      n_na_inv <- n_na_inv + 1L
      if (!quiet) setTxtProgressBar(pb, i)
      next
    }

    # Inverse prediction
    x_est <- tryCatch(fns$inv(params), error = function(e) NA_real_)
    if (!isTRUE(is.finite(x_est))) {
      res[[i]] <- list(x_est = x_est, se_x = NA_real_, cv_x = cv_x_max)
      n_na_inv <- n_na_inv + 1L
      if (!quiet) setTxtProgressBar(pb, i)
      next
    }

    # Gradient w.r.t. free parameters ∂x/∂θ
    grad_t <- tryCatch(
      fns$grad(params),
      error = function(e) { n_na_grad <<- n_na_grad + 1L; rep(NA_real_, length(params)) }
    )

    # Gradient w.r.t. response ∂x/∂y
    grad_y_val <- tryCatch(fns$grad_y(params), error = function(e) NA_real_)

    # ── Delta-method variance ─────────────────────────────────
    # Align grad_t names with Sigma — they MUST match now that Branch B
    # returns a,b,c,d and Branch A returns b,c,d.
    var_par <- NA_real_

    if (all(is.finite(grad_t))) {
      common <- intersect(names(grad_t), rownames(Sigma))

      if (length(common) > 0) {
        if (length(common) < length(grad_t) && !quiet && i == 1) {
          message("[propagate] NOTE: grad_t has ", length(grad_t),
                  " names but only ", length(common),
                  " align with Sigma. Using: ", paste(common, collapse = ", "))
        }
        g_sub  <- grad_t[common]
        S_sub  <- Sigma[common, common, drop = FALSE]
        var_par <- tryCatch(
          as.numeric(t(g_sub) %*% S_sub %*% g_sub),
          error = function(e) NA_real_
        )
      } else {
        # Still a mismatch — emit a clear one-time diagnostic
        if (!quiet && i == 1) {
          message("[propagate] WARNING: zero common names between grad_t and Sigma!")
          message("  grad_t names : ", paste(names(grad_t), collapse = ", "))
          message("  Sigma rows   : ", paste(rownames(Sigma), collapse = ", "))
          message("  This means 'a' is neither in Sigma (fixed) nor returned by grad_t (free).")
          message("  Check that fixed_a is correctly NULL or a scalar.")
        }
      }
    } else {
      n_na_grad <- n_na_grad + 1L
    }

    if (!isTRUE(is.finite(var_par))) n_na_vpar <- n_na_vpar + 1L

    # Measurement-error contribution  (∂x/∂y)^2 * se_y^2
    var_y <- if (isTRUE(is.finite(grad_y_val)) && se_y_i > 0)
      (grad_y_val^2) * (se_y_i^2) else 0

    var_x <- if (isTRUE(is.finite(var_par))) var_par + var_y else NA_real_
    se_x  <- if (isTRUE(is.finite(var_x)) && var_x >= 0) sqrt(var_x) else NA_real_

    # CV_x
    # ── CV_x computation ─────────────────────────────────────────────────────
    #
    # STRATEGY: when x_est is on the log10 concentration scale, the standard
    # ratio cv = (se_x / |x_est|) * 100 diverges as x_est -> 0 (i.e. conc -> 1).
    # This is a mathematical artefact of the log scale passing through zero —
    # NOT a real increase in uncertainty.
    #
    # Correct approach: propagate se_x to the LINEAR concentration scale first,
    # then compute the CV there.
    #
    #   x_linear     = 10^x_est
    #   se_x_linear  = se_x * 10^x_est * log(10)      [delta method]
    #   cv_x_linear  = (se_x_linear / x_linear) * 100
    #                = se_x * log(10) * 100
    #                = se_x * 230.259...
    #
    # This is independent of x_est, so it never diverges at x_est = 0.
    # The is_log_x flag controls which formula is used.
    #
    cv_x <- if (isTRUE(is.finite(se_x))) {

      if (is_log_x) {
        # Log10-scale x_est: use linear-scale CV (avoids /0 at x_est=0)
        raw_cv <- se_x * log(10) * 100   # = se_x * 230.26
      } else {
        # Linear-scale x_est: use standard ratio CV
        if (isTRUE(abs(x_est) > 1e-10)) {
          raw_cv <- (se_x / abs(x_est)) * 100
        } else {
          raw_cv <- Inf
        }
      }

      if (isTRUE(is.finite(raw_cv))) {
        if (raw_cv < cv_x_max) {
          n_ok <- n_ok + 1L
        } else {
          n_capped <- n_capped + 1L
        }
        min(raw_cv, cv_x_max)
      } else {
        n_capped <- n_capped + 1L
        cv_x_max
      }

    } else {
      n_capped <- n_capped + 1L
      cv_x_max
    }

    res[[i]] <- list(x_est = x_est, se_x = se_x, cv_x = cv_x)
    if (!quiet) setTxtProgressBar(pb, i)
  }

  if (!quiet) close(pb)

  # ── 5. Unpack ─────────────────────────────────────────────────
  pred_df$predicted_concentration <- sapply(res, `[[`, "x_est")
  pred_df$se_x                    <- sapply(res, `[[`, "se_x")
  pred_df$cv_x                    <- sapply(res, `[[`, "cv_x")
  pred_df$cv_x[!is.finite(pred_df$cv_x)] <- cv_x_max

  # ── 6. Summary ────────────────────────────────────────────────
  if (!quiet) {
    message("\n── propagate_error_dataframe summary ──────────────────")
    message(sprintf("  Rows processed     : %d", n))
    message(sprintf("  x_est finite       : %d", sum(is.finite(pred_df$predicted_concentration))))
    message(sprintf("  x_est NA           : %d", sum(!is.finite(pred_df$predicted_concentration))))
    message(sprintf("  se_x finite        : %d", sum(is.finite(pred_df$se_x))))
    message(sprintf("  se_x NA            : %d  (grad or var_par issue)", sum(!is.finite(pred_df$se_x))))
    message(sprintf("  cv_x < cap         : %d", n_ok))
    message(sprintf("  cv_x at cap (%3.0f) : %d", cv_x_max, n_capped))
    message(sprintf("  grad_t NA rows     : %d", n_na_grad))
    message(sprintf("  var_par NA rows    : %d", n_na_vpar))

    xv <- pred_df$predicted_concentration[is.finite(pred_df$predicted_concentration)]
    sv <- pred_df$se_x[is.finite(pred_df$se_x)]
    cv <- pred_df$cv_x[is.finite(pred_df$cv_x) & pred_df$cv_x < cv_x_max]

    if (length(xv)) message(sprintf("  x_est range        : [%.4f, %.4f]", min(xv), max(xv)))
    if (length(sv)) message(sprintf("  se_x  range        : [%.4f, %.4f]", min(sv), max(sv)))
    if (length(cv)) message(sprintf("  cv_x  range (excl cap): [%.2f, %.2f]", min(cv), max(cv)))
    message("───────────────────────────────────────────────────────")
  }

  # pred_df_v <<- pred_df
  pred_df
}



diagnose_cv_x <- function(df, label = "pred_se",
                          lloq = NULL, uloq = NULL,
                          cv_x_max = 125,     # cap parameter
                          verbose = TRUE) {
  if (!verbose) return(invisible(NULL))
  if (!"cv_x" %in% names(df)) {
    message(sprintf("[cv_x diagnostic] '%s': cv_x column not found.", label))
    return(invisible(NULL))
  }

  cv <- df$cv_x
  xc <- df$predicted_concentration

  finite_mask <- is.finite(cv) & is.finite(xc)
  cv_f  <- cv[finite_mask]
  xc_f  <- xc[finite_mask]

  if (length(cv_f) == 0) {
    message(sprintf("[cv_x diagnostic] '%s': no finite cv_x values.", label))
    return(invisible(NULL))
  }

  min_idx  <- which.min(cv_f)
  min_cv   <- cv_f[min_idx]
  min_x    <- xc_f[min_idx]
  max_cv   <- max(cv_f, na.rm = TRUE)
  mean_cv  <- mean(cv_f, na.rm = TRUE)

  # Count rows that hit the cap exactly (were clamped) vs genuinely > 20
  n_at_cap <- sum(cv_f >= cv_x_max, na.rm = TRUE)
  n_gt_20  <- sum(cv_f >  20,       na.rm = TRUE)
  n_na_raw <- sum(!is.finite(df$cv_x))   # residual NAs before cap applied

  message(sprintf(
    "\n[cv_x diagnostic] --- %s ---
  cv_x_max (cap)   : %.1f
  N total          : %d
  N finite cv_x    : %d
  N non-finite raw : %d  (replaced with cap)
  Min  cv_x        : %.3f  at predicted_concentration = %.4f
  Max  cv_x        : %.3f
  Mean cv_x        : %.3f
  N cv_x > 20      : %d
  N cv_x at cap    : %d",
    label, cv_x_max,
    nrow(df), length(cv_f), n_na_raw,
    min_cv, min_x, max_cv, mean_cv,
    n_gt_20, n_at_cap
  ))

  if (!is.null(lloq) && !is.null(uloq) && isTRUE(is.finite(lloq)) && isTRUE(is.finite(uloq))) {
    in_loq    <- finite_mask &
      df$predicted_concentration >= lloq &
      df$predicted_concentration <= uloq
    cv_in_loq <- df$cv_x[in_loq]
    n_loq_cap <- sum(cv_in_loq >= cv_x_max, na.rm = TRUE)

    message(sprintf(
      "  Within [lloq=%.4f, uloq=%.4f]:
    N            = %d
    mean cv_x    = %.3f
    max  cv_x    = %.3f
    N at cap     = %d  %s",
      lloq, uloq,
      length(cv_in_loq),
      mean(cv_in_loq, na.rm = TRUE),
      max(cv_in_loq,  na.rm = TRUE),
      n_loq_cap,
      if (n_loq_cap > 0)
        "[WARNING] capped values inside LOQ window — check curve fit near limits"
      else ""
    ))
  }

  if (n_at_cap > 0) {
    message(sprintf(
      "  [INFO] %d point(s) capped at cv_x_max=%.1f (asymptote proximity or failed propagation).",
      n_at_cap, cv_x_max
    ))
  }

  invisible(list(
    min_cv   = min_cv,
    min_x    = min_x,
    max_cv   = max_cv,
    mean_cv  = mean_cv,
    n_gt_20  = n_gt_20,
    n_at_cap = n_at_cap
  ))
}

#' Estimate Assay Response Standard Error from Standard Curve Replicates
#'
#' Computes an estimate of the standard error in assay response (y) by
#' aggregating within-dilution variability from standard samples collected
#' across multiple plates. Within each dilution level, standard samples
#' from different plates are treated as replicates for estimating measurement error.
#'
#' @param data        data.frame or tibble containing standard curve data
#' @param dilution_col name (character) of the column identifying dilution level
#'                     (default = "dilution")
#' @param response_col name (character) of the response column (e.g., "mfi")
#' @param plate_col   name (character) of the column identifying plate
#'                     (default = "plate"). Used to pool across plates when
#'                     single replicates per plate.
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
#'   \item{pooling_strategy}{character: "within_plate" or "across_plates"}
#'   \item{total_df}{total degrees of freedom in the pooled estimate}
#'   \item{n_dilutions_used}{number of dilution levels contributing to estimate}
#'   \item{n_plates}{number of plates in the data}
#'
#' @details
#' The function automatically detects whether replicates exist within plates
#' or whether pooling across plates is needed:
#'
#' **Within-plate replicates**: When multiple measurements exist at the same
#' dilution on the same plate, variance is computed within each plate-dilution
#' combination and then pooled.
#'
#' **Across-plate pooling**: When only single measurements exist per dilution
#' per plate (common in many assay designs), the function pools measurements
#' from different plates at each dilution level to estimate variability.
#'
#' The pooled within-dilution method computes:
#' \deqn{s_{pooled}^2 = \frac{\sum_{i} (n_i - 1) s_i^2}{\sum_{i} (n_i - 1)}}
#' where \eqn{s_i^2} is the variance at dilution level \eqn{i} with \eqn{n_i}
#' replicates (either within-plate or across-plates).
#'
#' @examples
#' # Example with standard curve data (single replicate per plate per dilution)
#' se_result <- assay_se(
#'   data = standards_df,
#'   dilution_col = "dilution",
#'   response_col = "mfi",
#'   plate_col = "plate",
#'   method = "pooled_within"
#' )
#' print(se_result$overall_se)
#' print(se_result$pooling_strategy)
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

  # Check if plate column exists

  has_plate_col <- !is.null(plate_col) && plate_col %in% colnames(data)

  ## 1. Remove NA responses
  if (na.rm) {
    data <- data[!is.na(data[[response_col]]), ]
  }

  if (nrow(data) == 0) {
    warning("No valid response data after removing NAs")
    return(list(
      overall_se       = NA_real_,
      by_dilution      = NULL,
      pooling_method   = method,
      pooling_strategy = NA_character_,
      total_df         = 0,
      n_dilutions_used = 0,
      n_plates         = 0
    ))
  }

  ## 2. Determine pooling strategy
  #    Check if we have within-plate replicates or need to pool across plates

  if (has_plate_col) {
    # Count replicates per plate-dilution combination
    rep_counts <- data %>%
      dplyr::group_by(
        !!rlang::sym(plate_col),
        !!rlang::sym(dilution_col)
      ) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    # Determine if we have within-plate replicates
    # If median reps per plate-dilution is > 1, use within-plate strategy
    median_reps_per_plate_dilution <- stats::median(rep_counts$n)
    has_within_plate_reps <- median_reps_per_plate_dilution >= min_reps

    n_plates <- length(unique(data[[plate_col]]))
  } else {
    has_within_plate_reps <- TRUE  # No plate info, treat all as one group
    n_plates <- 1
  }

  ## 3. Compute within-dilution statistics based on strategy

  if (has_within_plate_reps || !has_plate_col) {
    # STRATEGY A: Within-plate replicates exist
    # Pool variance within each dilution level (original approach)
    pooling_strategy <- "within_plate"

    by_dilution <- data %>%
      dplyr::group_by(!!rlang::sym(dilution_col)) %>%
      dplyr::summarise(
        n        = dplyr::n(),
        mean     = mean(!!rlang::sym(response_col), na.rm = TRUE),
        sd       = stats::sd(!!rlang::sym(response_col), na.rm = TRUE),
        variance = stats::var(!!rlang::sym(response_col), na.rm = TRUE),
        se       = sd / sqrt(n),
        df       = n - 1,
        .groups  = "drop"
      ) %>%
      dplyr::arrange(!!rlang::sym(dilution_col))

  } else {
    # STRATEGY B: Single replicate per plate per dilution
    # Pool ACROSS plates at each dilution level
    pooling_strategy <- "across_plates"

    # Each plate provides one measurement per dilution
    # Treat different plates as replicates at each dilution level
    by_dilution <- data %>%
      dplyr::group_by(!!rlang::sym(dilution_col)) %>%
      dplyr::summarise(
        n        = dplyr::n(),  # Number of plates with this dilution
        n_plates_at_dilution = dplyr::n_distinct(!!rlang::sym(plate_col)),
        mean     = mean(!!rlang::sym(response_col), na.rm = TRUE),
        sd       = stats::sd(!!rlang::sym(response_col), na.rm = TRUE),
        variance = stats::var(!!rlang::sym(response_col), na.rm = TRUE),
        se       = sd / sqrt(n),
        df       = n - 1,
        .groups  = "drop"
      ) %>%
      dplyr::arrange(!!rlang::sym(dilution_col))
  }

  ## 4. Filter to dilutions with sufficient replicates
  by_dilution_valid <- by_dilution %>%
    dplyr::filter(n >= min_reps, !is.na(variance), variance > 0)

  n_dilutions_used <- nrow(by_dilution_valid)

  if (n_dilutions_used == 0) {
    # Provide more informative warning
    if (pooling_strategy == "across_plates") {
      warning(
        "No dilution levels with sufficient replicates (min_reps = ", min_reps, "). ",
        "You have ", n_plates, " plates. Need at least ", min_reps,
        " plates with measurements at the same dilution to estimate SE."
      )
    } else {
      warning("No dilution levels with sufficient replicates (min_reps = ", min_reps, ")")
    }

    return(list(
      overall_se       = NA_real_,
      by_dilution      = by_dilution,
      pooling_method   = method,
      pooling_strategy = pooling_strategy,
      total_df         = 0,
      n_dilutions_used = 0,
      n_plates         = n_plates
    ))
  }

  ## 5. Compute pooled SE estimate based on method
  overall_se <- switch(method,

                       # Method 1: Pooled within-group variance (recommended)
                       "pooled_within" = {
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

  ## 6. Compute total degrees of freedom
  total_df <- sum(by_dilution_valid$df)

  ## 7. Return results
  list(
    overall_se       = overall_se,
    by_dilution      = by_dilution,
    pooling_method   = method,
    pooling_strategy = pooling_strategy,
    total_df         = total_df,
    n_dilutions_used = n_dilutions_used,
    n_plates         = n_plates
  )
}

#' Compute Assay SE for Each Antigen Across All Plates
#'
#' Computes the standard error of assay response for each unique combination
#' of study_accession, experiment_accession, source, and antigen by pooling
#' standard curve data across all plates. This SE can then be reused for
#' error propagation on each individual plate.
#'
#' @param standards_data data.frame containing all standard curve data
#' @param response_col name of the response column (e.g., "mfi")
#' @param dilution_col name of the dilution column (default = "dilution")
#' @param plate_col name of the plate column (default = "plate")
#' @param grouping_cols character vector of columns defining the grouping
#'        (default = c("study_accession", "experiment_accession", "source", "antigen"))
#' @param method pooling method for assay_se (default = "pooled_within")
#' @param min_reps minimum replicates required (default = 2)
#'
#' @return A data.frame with one row per antigen grouping containing:
#'   - grouping columns (study_accession, experiment_accession, source, antigen)
#'   - overall_se: the pooled SE for that antigen
#'   - pooling_strategy: "within_plate" or "across_plates"
#'   - n_plates: number of plates contributing
#'   - n_dilutions_used: number of dilution levels used
#'   - total_df: total degrees of freedom
#'
#' @export
compute_antigen_se_table <- function(standards_data,
                                     response_col = "mfi",
                                     dilution_col = "dilution",
                                     plate_col = "plate",
                                     grouping_cols = c("study_accession",
                                                       "experiment_accession",
                                                       "source",
                                                       "antigen"),
                                     method = "pooled_within",
                                     min_reps = 2,
                                     verbose = FALSE) {


  # Validate inputs
  required_cols <- c(grouping_cols, response_col, dilution_col, plate_col)
  missing_cols <- setdiff(required_cols, colnames(standards_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Get unique antigen groupings
  unique_groupings <- unique(standards_data[, grouping_cols, drop = FALSE])

  if (verbose) {
    message(sprintf("Computing SE for %d antigen groupings", nrow(unique_groupings)))
  }

  # Compute SE for each grouping
  se_results <- lapply(seq_len(nrow(unique_groupings)), function(i) {
    grouping <- unique_groupings[i, , drop = FALSE]

    # Filter standards to this grouping (across all plates)
    filter_expr <- rep(TRUE, nrow(standards_data))
    for (col in grouping_cols) {
      filter_expr <- filter_expr & (standards_data[[col]] == grouping[[col]])
    }
    grouped_standards <- standards_data[filter_expr, ]

    if (nrow(grouped_standards) == 0) {
      return(data.frame(
        grouping,
        overall_se = NA_real_,
        pooling_strategy = NA_character_,
        n_plates = 0L,
        n_dilutions_used = 0L,
        total_df = 0L,
        stringsAsFactors = FALSE
      ))
    }

    # Compute SE using all plates for this antigen
    se_result <- tryCatch({
      assay_se(
        data = grouped_standards,
        dilution_col = dilution_col,
        response_col = response_col,
        plate_col = plate_col,
        method = method,
        min_reps = min_reps
      )
    }, error = function(e) {
      if (verbose) {
        warning(sprintf("SE calculation failed for %s: %s",
                        paste(grouping, collapse = "/"), e$message))
      }
      list(
        overall_se = NA_real_,
        pooling_strategy = NA_character_,
        n_plates = 0L,
        n_dilutions_used = 0L,
        total_df = 0L
      )
    })

    data.frame(
      grouping,
      overall_se = se_result$overall_se,
      pooling_strategy = se_result$pooling_strategy,
      n_plates = se_result$n_plates,
      n_dilutions_used = se_result$n_dilutions_used,
      total_df = se_result$total_df,
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  se_table <- do.call(rbind, se_results)
  rownames(se_table) <- NULL

  if (verbose) {
    message(sprintf("SE table computed: %d groupings, %d with valid SE",
                    nrow(se_table), sum(!is.na(se_table$overall_se))))
  }

  return(se_table)
}


#' Look Up SE for a Specific Antigen from the SE Table
#'
#' @param se_table data.frame from compute_antigen_se_table()
#' @param study_accession study identifier
#' @param experiment_accession experiment identifier
#' @param source source identifier
#' @param antigen antigen identifier
#'
#' @return numeric SE value, or NA if not found
#' @export
lookup_antigen_se <- function(se_table,
                              study_accession,
                              experiment_accession,
                              source,
                              antigen) {

  if (is.null(se_table) || nrow(se_table) == 0) {
    return(NA_real_)
  }

  idx <- which(
    se_table$study_accession == study_accession &
      se_table$experiment_accession == experiment_accession &
      se_table$source == source &
      se_table$antigen == antigen
  )

  if (length(idx) == 0) {
    return(NA_real_)
  }

  return(se_table$overall_se[idx[1]])
}


## The best fit must contain best_pred and antigen_plate containing plate_samples
predict_and_propagate_error <- function(best_fit,
                                        response_var,
                                        antigen_plate,
                                        study_params,
                                        se_std_response,
                                        cv_x_max = 150,
                                        verbose = TRUE) {

  if (study_params$is_log_response) {
    if (is.null(antigen_plate$fixed_a_result)) {
      fixed_a_result <- NULL
    } else {
      .eps <- 0.000005
      fixed_a_result <- log10(antigen_plate$fixed_a_result + .eps)
    }
    log_plate_samples <- log10(antigen_plate$plate_samples[[response_var]])
  }

  # ── Compute overall_se_value ────────────────────────────────────────────
  # se_std_response is on the RAW response scale (e.g. MFI units).
  # When is_log_response is TRUE the model and all y values are on the
  # log10 scale.  The delta method needs se_y on the SAME scale as y.
  #
  # Conversion via the log10 derivative:
  #   if Y = log10(Z)  then  dY = dZ / (Z * ln(10))
  #   => se_log10 = se_mfi / (mean_mfi * ln(10))
  #
  # We use the geometric mean of the raw responses as the reference MFI
  # for the conversion, which is appropriate for a log-transformed variable.

  if (isTRUE(is.finite(se_std_response)) && se_std_response > 0) {

    if (study_params$is_log_response) {
      # Estimate mean raw MFI from standards (geometric mean on raw scale)
      raw_standards <- antigen_plate$plate_standard[[response_var]]
      raw_standards <- raw_standards[is.finite(raw_standards) & raw_standards > 0]

      if (length(raw_standards) > 0) {
        ref_mfi <- exp(mean(log(raw_standards)))   # geometric mean
      } else {
        # Fallback: back-transform from the log10 mean of plate samples
        raw_plate <- antigen_plate$plate_samples[[response_var]]
        raw_plate  <- raw_plate[is.finite(raw_plate) & raw_plate > 0]
        ref_mfi    <- if (length(raw_plate) > 0) exp(mean(log(raw_plate))) else 1
      }

      # Convert SE from raw MFI units to log10 units
      overall_se_value <- sqrt(se_std_response / (ref_mfi * log(10) * 10))

      if (verbose) {
        message(sprintf(
          "[predict_and_propagate] SE conversion: se_mfi=%.4f, ref_mfi=%.4f -> se_log10=%.6f",
          se_std_response, ref_mfi, overall_se_value
        ))
      }

    } else {
      # Response is NOT log-transformed — use se_std_response directly
      overall_se_value <- sqrt(se_std_response / 10)
    }

  } else {
    # se_std_response is NA / non-finite / zero — use a small fallback
    # based on the spread of the log-scale standards if available
    if (study_params$is_log_response) {
      log_stds <- log10(antigen_plate$plate_standard[[response_var]])
      log_stds <- log_stds[is.finite(log_stds)]
      overall_se_value <- if (length(log_stds) > 1) sd(log_stds) * 0.01 else 0.01
    } else {
      overall_se_value <- 0
    }

    if (verbose) {
      message(sprintf(
        "[predict_and_propagate] se_std_response not usable (%.4f); using fallback se=%.6f",
        if (is.finite(se_std_response)) se_std_response else NA_real_,
        overall_se_value
      ))
    }
  }

  # ── Validate that overall_se_value is now on the right scale ────────────
  # On log10 scale, a realistic se_y is << 1 (typically 0.01 – 0.15).
  # Warn loudly if it still looks like raw MFI units.
  if (study_params$is_log_response && overall_se_value > 1) {
    warning(sprintf(
      "[predict_and_propagate] overall_se_value=%.4f is > 1 on the log10 scale. ",
      overall_se_value,
      "This will inflate se_x. Check that se_std_response is in raw response units."
    ))
  }

  lloq  <- if (!is.null(best_fit$best_glance$lloq)) as.numeric(best_fit$best_glance$lloq)[1] else NA_real_
  uloq  <- if (!is.null(best_fit$best_glance$uloq)) as.numeric(best_fit$best_glance$uloq)[1] else NA_real_

  # ── Standards prediction curve ──────────────────────────────────────────
  pred_se <- best_fit$best_pred

  if (verbose) {
    message(paste("pred_se has", nrow(pred_se), "row(s)"))
  }

  z <- qnorm(0.975)
  max_conc_standard <- ifelse(
    study_params$is_log_independent,
    log10(antigen_plate$antigen_settings$standard_curve_concentration),
    antigen_plate$antigen_settings$standard_curve_concentration
  )

  pred_se$overall_se <- overall_se_value

  diagnose_propagation_inputs(
    fit     = best_fit$best_fit,
    model   = best_fit$best_model_name,
    fixed_a = fixed_a_result,
    y_test  = NULL
  )

  pred_se <- propagate_error_dataframe(
    pred_df  = pred_se,
    fit      = best_fit$best_fit,
    model    = best_fit$best_model_name,
    y_col    = "yhat",
    se_col   = "overall_se",
    fixed_a  = fixed_a_result,
    cv_x_max = cv_x_max,
    is_log_x = study_params$is_log_independent   # TRUE when x is log10(conc)
  )

  diagnose_cv_x(
    df       = pred_se,
    label    = "standards pred_se",
    lloq     = lloq,
    uloq     = uloq,
    cv_x_max = cv_x_max,
    verbose  = verbose
  )

  pred_se$predicted_concentration <- ifelse(
    !is.infinite(pred_se$predicted_concentration),
    pred_se$predicted_concentration,
    ifelse(pred_se$predicted_concentration > 0, max_conc_standard, 0)
  )

  pred_se$pcov                     <- pred_se$cv_x
  pred_se$study_accession          <- unique(best_fit$best_data$study_accession)
  pred_se$experiment_accession     <- unique(best_fit$best_data$experiment_accession)
  pred_se$nominal_sample_dilution  <- unique(best_fit$best_data$nominal_sample_dilution)
  pred_se$plateid                  <- unique(best_fit$best_data$plateid)
  pred_se$plate                    <- unique(best_fit$best_data$plate)
  pred_se$antigen                  <- unique(best_fit$best_data$antigen)
  pred_se$source                   <- unique(best_fit$best_data$source)

  # pred_se_v <<- pred_se
  best_fit$best_pred <- pred_se

  # ── Sample propagation ──────────────────────────────────────────────────
  raw_assay_response <- antigen_plate$plate_samples[[response_var]]

  if (verbose) print(head(antigen_plate$plate_samples))

  sample_se <- data.frame(
    y_new              = log_plate_samples,
    raw_assay_response = raw_assay_response,
    dilution           = antigen_plate$plate_samples$dilution,
    well               = antigen_plate$plate_samples$well
  )

  if (verbose) {
    message(paste("sample_se has", nrow(sample_se), "row(s)"))
  }

  sample_se$overall_se      <- overall_se_value   # already on log10 scale
  sample_se[[response_var]] <- sample_se$y_new

  diagnose_propagation_inputs(
    fit     = best_fit$best_fit,
    model   = best_fit$best_model_name,
    fixed_a = fixed_a_result,
    y_test  = NULL
  )

  sample_se <- propagate_error_dataframe(
    pred_df  = sample_se,
    fit      = best_fit$best_fit,
    model    = best_fit$best_model_name,
    y_col    = response_var,
    se_col   = "overall_se",
    fixed_a  = fixed_a_result,
    cv_x_max = cv_x_max,
    is_log_x = study_params$is_log_independent
  )

  diagnose_cv_x(
    df       = sample_se,
    label    = "samples sample_se",
    lloq     = lloq,
    uloq     = uloq,
    cv_x_max = cv_x_max,
    verbose  = verbose
  )

  if (study_params$is_log_independent) {
    sample_se$final_predicted_concentration <-
      10^sample_se$predicted_concentration * sample_se$dilution
  } else {
    sample_se$final_predicted_concentration <-
      sample_se$predicted_concentration * sample_se$dilution
  }

  sample_se <- dplyr::inner_join(
    antigen_plate$plate_samples[
      , !(names(antigen_plate$plate_samples) %in% c("mfi", "dilution"))
    ],
    sample_se,
    by = "well"
  )

  sample_se$pcov    <- sample_se$cv_x
  sample_se$source  <- unique(best_fit$best_data$source)
  sample_se$feature <- unique(best_fit$best_data$feature)

  # Remove intermediate columns, rename se_x
  sample_se <- sample_se[, !names(sample_se) %in% c("y_new")]
  names(sample_se)[names(sample_se) == "se_x"] <- "se_concentration"

  names(sample_se)[names(sample_se) == "predicted_concentration"] <-
    "raw_predicted_concentration"

  # sample_se_v  <<- sample_se
  best_fit$sample_se <- sample_se

  if (verbose) message("Finished predict_and_propagate_error")

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

test_cv_x_at_inflection <- function(best_fit, verbose = TRUE) {
  pred       <- best_fit$best_pred
  inflect_x  <- best_fit$best_glance$inflect_x

  if (!"cv_x" %in% names(pred)) {
    if (verbose) message("[test_cv_x] cv_x not found in best_pred.")
    return(invisible(NULL))
  }

  finite_mask  <- is.finite(pred$cv_x) & is.finite(pred$predicted_concentration)
  cv_f  <- pred$cv_x[finite_mask]
  xc_f  <- pred$predicted_concentration[finite_mask]

  min_cv      <- min(cv_f, na.rm = TRUE)
  min_idx     <- which.min(cv_f)
  x_at_min_cv <- xc_f[min_idx]

  closest_idx    <- which.min(abs(xc_f - inflect_x))
  cv_at_inflect  <- cv_f[closest_idx]

  ratio <- cv_at_inflect / min_cv

  if (verbose) {
    cat("\n--- cv_x inflection test ---\n")
    cat("Inflection point (x)        :", inflect_x, "\n")
    cat("cv_x at inflection point    :", cv_at_inflect, "\n")
    cat("Minimum cv_x in curve       :", min_cv, "\n")
    cat("x at minimum cv_x           :", x_at_min_cv, "\n")
    cat("Ratio cv_at_inflect/min_cv  :", ratio,
        " (expected ~1.0 if inflection == minimum cv)\n")

    # Visual check
    plot(xc_f, cv_f, type = "l",
         xlab = "Predicted Concentration (log10)", ylab = "CV_x (%)",
         main = "cv_x across concentration range")
    abline(h  = 20,         col = "orange", lty = 2)
    abline(h  = 125,        col = "red",    lty = 2)
    abline(v  = inflect_x,  col = "blue",   lty = 2)
    points(x_at_min_cv, min_cv, col = "darkgreen", pch = 19, cex = 1.5)
    points(xc_f[closest_idx], cv_at_inflect, col = "blue", pch = 19, cex = 1.5)
    legend("topright",
           legend = c("cv_x = 20 threshold", "cv_x = 125 ceiling",
                      "Inflection point", "Min cv_x"),
           col    = c("orange", "red", "blue", "darkgreen"),
           lty    = c(2, 2, 2, NA), pch = c(NA, NA, NA, 19))
  }

  invisible(list(
    cv_at_inflect  = cv_at_inflect,
    min_cv         = min_cv,
    x_at_min_cv    = x_at_min_cv,
    ratio          = ratio
  ))
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

  sample_se$gate_class_loq <- ifelse(sample_se$raw_predicted_concentration >= lloq_x &sample_se$raw_predicted_concentration <= uloq_x,
                                     "Acceptable",
                                     ifelse(sample_se$raw_predicted_concentration > uloq_x, "Too Concentrated", "Too Diluted"))

  sample_se$gate_class_lod <- ifelse(sample_se[[response_variable]] >= llod & sample_se[[response_variable]] <= ulod,
                                     "Acceptable",
                                     ifelse(sample_se[[response_variable]] > ulod, "Too Concentrated", "Too Diluted"))

  sample_se$gate_class_pcov <- ifelse(sample_se$pcov <= pcov_threshold,
                                      "Acceptable",
                                      ifelse(sample_se$raw_predicted_concentration < inflect_x, "Too Diluted", "Too Concentrated"))

  best_fit$sample_se <- sample_se

  return(best_fit)


}
