# This file contains functions supporting the 5, 4 parameter logistic regression, and exponential models

# return a Boolean to report if standard curve data exists for a given/selected experiment to preform Logistic Regression
checkStandardCurves <- function(standard_curve_data, selectedStudy, selectedExperiment){

  print(paste("selectedStudy:", selectedStudy))
  print(paste("selectedExperiment:", selectedExperiment))

  if(nrow(standard_curve_data) == 0 || is.null(standard_curve_data)) {
    cat("No standard data avaliable")
    return(FALSE)
  }

  if (is.null(selectedStudy) || is.null(selectedExperiment)) {
    cat("No selected study or experiment in standard data")
    return(FALSE)
  }

  experimentData <- standard_curve_data[standard_curve_data$study_accession == selectedStudy & standard_curve_data$experiment_accession == selectedExperiment,]
  return(nrow(experimentData) > 0)
}


# Calculate Log Dilution Factor and clean up plate names
calculate_log_dilution <- function(dat){
  dat <- dat %>% mutate(dilution_factor = 1/dat$dilution,
                        log_dilution = log10(dilution_factor))

 # dat$mfi <- log10(dat$mfi)
  #dat_v <<- dat
  return(dat)
}

## Aggregate standard curve data to ensure one point per dilution factor before running models.
## Read in filtered standard curve data for 1 dilution series
aggregate_mfi_dilution_factor <- function(standard_curve_data) {
  avg_mfi_standard <- aggregate(mfi ~ dilution_factor + plateid + antigen, data = standard_curve_data, FUN = mean, na.rm = TRUE)
  avg_standard_curve_data <- merge(avg_mfi_standard, standard_curve_data[, !names(standard_curve_data) %in% "mfi"],
                                   by = c("dilution_factor", "plateid", "antigen"), all.x = TRUE)
  avg_standard_curve_data <- distinct(avg_standard_curve_data, dilution_factor, plateid, antigen, .keep_all = TRUE)

  # avg_standard_curve_data[!(duplicated(avg_standard_curve_data$dilution_factor) &
  #                                                        duplicated(avg_standard_curve_data$antigen) &
  #                                                        duplicated(avg_standard_curve_data$plateid)), ]
  return(avg_standard_curve_data)
}
## Blank functions
# Geometric mean helper function
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
# On the log dilution scale we subtract log10(2) which is equivlent to dividing by 2.

include_blanks <- function(buffer_data, std_curve_data, plateid, antigen) {
  # filter the plate and antigen from the buffer and standard curve data
  buffer_data_filtered <- buffer_data[buffer_data$plateid == plateid & buffer_data$antigen == antigen, ]
  std_curve_data_filtered <- std_curve_data[std_curve_data$plateid == plateid & std_curve_data$antigen == antigen,]

  # calculate the geometric mean of the buffer/blanks by analyte
  mfi_buffer <- geom_mean(buffer_data_filtered$mfi)

  # calculate the log dilution of the buffer (Dr Lumi uses (1/(min(dilution_factor))/2)
  cat("\nIn include blanks\n")
  print(head(std_curve_data_filtered))

  min_log_dilution <- min(std_curve_data_filtered$log_dilution)
  log_dilution_buffer <- min_log_dilution - log10(2)
  min_dilution_factor <- min(std_curve_data_filtered$dilution)

  std_curve_data_filtered$antibody_mfi <- std_curve_data_filtered$mfi


  new_point <- data.frame(
    study_accession = unique(std_curve_data_filtered$study_accession),
    experiment_accession = unique(std_curve_data_filtered$experiment_accession),
    well = "geometric_mean_buffer",
    stype = unique(buffer_data_filtered$stype),
    sampleid = "buffer_mean",
    source =  unique(std_curve_data_filtered$source),
    dilution = as.numeric(NA),
    pctaggbeads = as.numeric(NA),
    samplingerrors = as.character(NA),
    # what is calculated
    antigen = antigen,
    mfi = mfi_buffer,
    predicted_mfi = mfi_buffer,
    n = as.integer(NA),
    # for consistency
    feature = unique(std_curve_data_filtered$feature),
    # read in from function
    plateid = plateid,
    selected_str = unique(std_curve_data_filtered$selected_str),
    dilution_factor = min_dilution_factor,
    log_dilution = log_dilution_buffer,
    antibody_mfi = mfi_buffer
  )

  cat("\nnames of standard curve filtered\n")
  print(names(std_curve_data_filtered))
  cat("\n names of  new point\n")
  print(names(new_point))

  std_curve_data_filtered <- rbind(std_curve_data_filtered, new_point)
  return(std_curve_data_filtered)

}

# The geometric mean of the blank controls is subtracted from all the standard points.
# pass in the buffer data, standards/sample data  as data
#stype is indicator of if it is standard/sample
# multiplier = number of times the geometric mean to subtract, default is 1

subtract_blanks <- function(buffer_data, data, stype, plateid, antigen, multiplier = 1) {
  if (stype == "standards") {
      std_curve_data <- data
      # filter the plate and antigen from the buffer and standard curve data
      buffer_data_filtered <- buffer_data[buffer_data$plateid == plateid & buffer_data$antigen == antigen, ]
      std_curve_data_filtered <- std_curve_data[std_curve_data$plateid == plateid & std_curve_data$antigen == antigen,]

      # calculate the geometric mean of the buffer/blanks by analyte
      mfi_buffer <- geom_mean(buffer_data_filtered$mfi)
      #cat(mfi_buffer)
      # subtract geometric mean of buffer from all the standard points.
      std_curve_data_filtered$mfi <- std_curve_data_filtered$mfi - (multiplier * mfi_buffer)

      # if any mfi is < 0 after subtraction, exclude it from further analysis by setting filtering it out.
      if (any(std_curve_data_filtered[,"mfi"]< 0, na.rm=TRUE)) {
        # cat("below 0 ")
        std_curve_data_filtered[which(std_curve_data_filtered[,"mfi"]< 0 ),"mfi"] <- 0
        #std_curve_data_filtered <- std_curve_data_filtered[std_curve_data_filtered[,"mfi"] >= 0, ]
      }

    return(std_curve_data_filtered)
  } else if (stype == "sample") {
    sample_data <- data

    # filter the plate and antigen from the buffer and standard curve data
    buffer_data_filtered <- buffer_data[buffer_data$plateid == plateid & buffer_data$antigen == antigen, ]
    sample_data_filtered <- sample_data[sample_data$plateid == plateid & sample_data$antigen == antigen,]

    mfi_buffer <- geom_mean(buffer_data_filtered$mfi)
    #cat(mfi_buffer)
    # subtract geometric mean of buffer from all the standard points.
    sample_data_filtered$value_reported <- sample_data_filtered$value_reported - (multiplier * mfi_buffer)

    if (any(sample_data_filtered[,"value_reported"]< 0, na.rm=TRUE)) {
      # cat("below 0 ")
      sample_data_filtered[which(sample_data_filtered[,"value_reported"]< 0 ),"value_reported"] <- 0
      #std_curve_data_filtered <- std_curve_data_filtered[std_curve_data_filtered[,"mfi"] >= 0, ]
    }
    sample_data_filtered <- sample_data_filtered
    return(sample_data_filtered)

  }

}
# Function to compute lower and upper limits of quantification via derivatives
# loq_derivitives <- function(fit, glance_fit) {
#   if (glance_fit$crit == "drda_5") {
#     cat("loq drda")
#     ndf <- fit$model
#     rhs <- "log_dilution"
#   } else {
#     #  if (model$status == "converged") {
#     lhs <- as.character(fit$m$formula()[[2]])
#     parameters <- names(fit$m$getPars())
#     allobj <- ls(fit$m$getEnv())
#     rhs <- "log_dilution" # predictor
#     ndf <- data.frame(get(lhs, fit$m$getEnv()),
#                       get(rhs, fit$m$getEnv()))
#     names(ndf) <- c(lhs, rhs)
#   }
#
#   if(glance_fit$crit =="nls_4") {
#     cat("loq nls 4")
#     FUN <- d3SSl4
#   } else if (glance_fit$crit =="nls_5") {
#     cat("loq nls 5")
#     FUN <- d3SSl5
#   } else if (glance_fit$crit == "nls_exp") {
#     cat("loq nls exp")
#     FUN <- d3SSexp
#   } else if (glance_fit$crit == "drda_5") {
#     cat("loq drda ")
#     FUN <- d3SSdrda
#   }
#
#   ff <- function(x){
#     ans <- do.call(FUN, as.list(c(x,arguments)))
#     if (is.list(ans)) {
#       ans <- unlist(ans)
#     }
#     ans
#   }
#
#   arguments <- as.numeric(coef(fit))
#   ff <- Vectorize(ff)
#   roots <<- try(uniroot.all(ff, c(min(ndf[,rhs]),max(ndf[,rhs]))))
#   msg <- "solution_found"
#   if(inherits(roots,"try-error")){
#     roots <- c(NA, NA)
#     msg <- " (no estimated, error in 'uniroot.all' function)"
#   }
#
#   # no roots found
#   if (length(roots) == 0) {
#     roots <- c(NA, NA)
#     msg <- "No Roots Found"
#   }
#   #  if(length(roots)>2L){
#   #         roots <- c(NA,NA)
#   #         msg <- " (no estimated, >2 roots in third derivative)"
#   # }
#   # if(length(roots) < 2L){
#   #    roots <- c(NA,NA)
#   #    msg <- " (no estimated, <2 roots in third derivative)"
#   #   }
#   # The tangent finds the lloq and uloq of the 3rd derivative of local min and max
#   lloq <- min(roots)
#   uloq <- max(roots)
#
#   limits_of_quantifcation <- list(lloq = lloq, uloq = uloq,
#                                   method = paste("derivitive", msg))
#   return(limits_of_quantifcation)
# }

loq_derivitives <- function(fit, glance_fit) {
  # Identify model type and extract the dataset
  if (glance_fit$crit == "drda_5") {
    ndf <- fit$model
    rhs <- "log_dilution"
  } else {
    lhs <- as.character(fit$m$formula()[[2]])
    rhs <- "log_dilution"
    ndf <- data.frame(
      get(lhs, fit$m$getEnv()),
      get(rhs, fit$m$getEnv())
    )
    names(ndf) <- c(lhs, rhs)
  }


  # Define range of log_dilution values to evaluate the second derivative
  if (glance_fit$crit == "drda_5") {
    dilution_range <- seq(min(ndf[, rhs]), 0 , length.out = 500)
  } else {
    dilution_range <- seq(min(ndf[, rhs]), max(ndf[, rhs]) , length.out = 500)
  }

  # Extract model parameters
  if (glance_fit$crit == "drda_5") {
    drda_coefs <- coef(fit)
    alpha <- as.numeric(drda_coefs["alpha"])
    delta <- as.numeric(drda_coefs["delta"])
    eta <- as.numeric(drda_coefs["eta"])
    phi <- as.numeric(drda_coefs["phi"])
    nu <- as.numeric(drda_coefs["nu"])
    r_asy <- delta + alpha

    model_params <- c(alpha, delta, eta, phi, nu)
    fitted_values <- drda::logistic5_fn(dilution_range, model_params)
    bendlower <- as.numeric(glance_fit$bendlower)
    bendupper <- as.numeric(glance_fit$bendupper)

    if (is.na(bendlower) || is.null(bendlower)) {
      lloq <- NA
    }
    if (is.na(bendupper) || is.null(bendupper)) {
      lloq <- NA
    }

    if (!is.na(bendlower) && !is.null(bendlower)) {
      # LLOQ based on bend lines
      diffs <- fitted_values - bendlower
      # Find where the sign changes
      intersection_indices <- which(sign(diffs[-1]) != sign(diffs[-length(diffs)]))
      # Get the x-values where intersections occur
      lloq <- dilution_range[intersection_indices]
      if (length(lloq) == 0) {
        if (min(fitted_values) >= bendlower & min(fitted_values) <= bendupper) {
          lloq <- min(dilution_range)
        }
      }
    }

    if (!is.na(bendupper) && !is.null(bendupper)) {
      # ULOQ based on bend lines
      diffs_upper <- fitted_values - bendupper
      intersection_indices_upper <- which(sign(diffs_upper[-1]) != sign(diffs_upper[-length(diffs_upper)]))
      uloq <- dilution_range[intersection_indices_upper]

      if (length(uloq) == 0) {
        # in the linear region concentrated fitted value
        if(max(fitted_values) >= bendlower & max(fitted_values) <= bendupper) {
          uloq <- 0
        }
      }
      # bend_intercetion_lloq <<- which(fitted_values == bendlower)
      # bend_intercetion_uloq <<- which(fitted_values == bendupper)
      method <- "bendline_intersection"
      second_derivative_values <- d2SSdrda(dilution_range, nu, eta, phi, delta, r_asy)
    }

  } else if (glance_fit$crit == "nls_exp") {
      lloq <- NA
      uloq <- NA
      method <- "no local min/max or bend lines for exponential fit"
  } else {
    # Select the appropriate second derivative function
    FUN <- switch(glance_fit$crit,
                  "nls_4" = d2SS4pl,
                  "nls_5" = d2SS5pl,
                  "nls_exp" = d2SSexp
                  #stop("Unknown model type for second derivative")
    )
  # Extract model parameters
   arguments <- as.numeric(coef(fit))

   # Define the function for the second derivative
      ff <- Vectorize(function(x) {
        ans <- do.call(FUN, as.list(c(x, arguments)))
        if (is.list(ans)) unlist(ans) else ans
      })


  # Define range of log_dilution values to evaluate the second derivative
 # dilution_range <- seq(min(ndf[, rhs]), max(ndf[, rhs]), length.out = 500)

  # Compute second derivative values
  second_derivative_values <- ff(dilution_range)

  # Find the max and min values of the second derivative
  max_value <- max(second_derivative_values)
  min_value <- min(second_derivative_values)

  # Find the corresponding log dilution
  uloq <- dilution_range[which.min(second_derivative_values)]
  lloq <- dilution_range[which.max(second_derivative_values)]

  method <- "Derivitive"
  }

  cat("before assign NA")

  if (length(lloq) == 0) {
    lloq <- NA
  }
  if (length(uloq) == 0) {
    uloq <- NA
  }


  # Return results
  return(list(
    lloq = lloq,
    uloq = uloq,
    method = method
  ))
}

## Self-start functions 2nd derivitives
# 4 parameter
d2SS4pl <- function(log_dilution, l_asy, r_asy, xmid, scal) {
  exp_term <- exp((log_dilution - xmid) / scal)
  numerator <- exp_term * (exp_term - 1)
  denominator <- scal^2 * (1 + exp_term)^3

  (l_asy - r_asy) * (numerator / denominator)
}

# 5 parameter
d2SS5pl <- function(log_dilution, l_asy, r_asy, xmid, scal, g) {
  exp_term <- exp((log_dilution - xmid) / scal)
  base_term <- 1 + exp_term

  # Compute the numerator
  numerator <- g * exp_term * (exp_term - 1) * (base_term^(-g - 1))

  # Compute the denominator
  denominator <- scal^2

  # Compute second derivative
  (l_asy - r_asy) * (numerator / denominator)
}
# drda 5 parameter

#logistic5_hessian(x, theta)
#alpha, delta, eta, phi, nu


d2SSdrda <- function(x, nu, eta, phi, delta, r_asy) {

 # x_val <<- x
  alpha <- r_asy - delta
  #theta <- c()
  #logistic5_hessian(x, )
  # if (eta > 0 & nu > 0) {
  #   theta <- c(alpha, delta, eta, phi, nu)
  #   second_derivitive <<- drda::logistic5_hessian(x, theta)
  # } else {
  #   theta <- NA
  #   second_derivitive <- NA
  # }
 # theta_view <<- theta
  theta <- ifelse(eta >0 & nu >0 ,c(alpha, delta, eta, phi, nu), NA)
  second_derivitive <- drda::logistic5_hessian(x, theta)
  return(second_derivitive)
  # # Intermediate terms
  # exp_term <- exp(-eta * (x - phi))  # exp(-eta * (x - phi))
  # denom <- 1 + nu * exp_term  # 1 + nu * exp_term
  #
  # # First derivative of g(x)
  # g_prime <- -eta * exp_term / (denom^(1/nu + 1))
  #
  # # Second derivative of g(x)
  # g_prime2 <- (eta^2 * exp_term * (nu * exp_term - 1)) / (denom^(1/nu + 2))
  #
  # # Complete second derivative of f(x)
  # f_prime2 <- delta * g_prime2  # Use second derivative
  #
  # return(f_prime2)
}

# exponential
d2SSexp <- function(x, scal, xmid, l_asy) {
  ans <- (scal - l_asy) * (xmid^3) * (xmid^2) * exp(xmid * x)
  return(ans)
}

# ## Self-start functions for 3rd derivitives for the limits of quantification function via derivitives'
# # DrLumi Package
# # Third order derivative of the SSl4 function
# # @param x a numeric vector of values at which to evaluate the model
# # @param scal a numeric parameter representing the \code{-scal} of the
# # function at the inflection point
# # @param l_asy a numeric parameter representing the lower asymptote when
# # \code{x} trend to \code{-Inf}
# # @param r_asy a numeric parameter representing the higher asymptote when
# # \code{x} trend to \code{Inf}
# # @param x_mid is the x value corresponding to the inflection point
# d3SSl4 <- function(x, scal, l_asy, r_asy, x_mid){
#     ans <- -((r_asy - l_asy) * (10^(scal * (x - x_mid)) * (log(10) * scal) *
#             (log(10) * scal) * (log(10) * scal))/(1 + 10^(scal *
#             (x - x_mid)))^2 -
#             (r_asy - l_asy) * (10^(scal * (x - x_mid)) * (log(10) * scal) *
#             (log(10) * scal)) * (2 * (10^(scal *
#             (x - x_mid)) * (log(10) * scal) * (1 + 10^(scal *
#             (x - x_mid)))))/((1 +
#             10^(scal * (x - x_mid)))^2)^2 - (((r_asy - l_asy) * (10^(scal *
#             (x - x_mid)) * (log(10) * scal) * (log(10) * scal)) * (2 *
#             (10^(scal * (x - x_mid)) * (log(10) * scal) * (1 + 10^(scal *
#             (x - x_mid))))) + (r_asy - l_asy) * (10^(scal * (x -
#             x_mid)) * (log(10) * scal)) * (2 * (10^(scal * (x - x_mid)) *
#             (log(10) * scal) * (log(10) * scal) * (1 + 10^(scal *
#             (x - x_mid))) + 10^(scal * (x - x_mid)) * (log(10) * scal) *
#             (10^(scal * (x - x_mid)) * (log(10) * scal)))))/((1 + 10^(scal *
#             (x - x_mid)))^2)^2 - (r_asy - l_asy) * (10^(scal * (x - x_mid)) *
#             (log(10) * scal)) * (2 * (10^(scal * (x - x_mid)) * (log(10) *
#             scal) * (1 + 10^(scal * (x - x_mid))))) * (2 * (2 * (10^(scal *
#             (x - x_mid)) * (log(10) * scal) * (1 + 10^(scal * (x - x_mid)))) *
#             ((1 + 10^(scal * (x - x_mid)))^2)))/(((1 + 10^(scal * (x -
#             x_mid)))^2)^2)^2))
#     ans
# }
#
# # Third order derivative of the SSl5 function
# #
# # @param x a numeric vector of values at which to evaluate the model
# # @param scal is a numeric parameter representing the \code{-scal} of the
# # function at the inflection point
# # @param l_asy a numeric parameter representing the lower asymptote when
# # \code{x} trend to \code{-Inf}
# # @param r_asy a numeric parameter representing the higher asymptote when
# # \code{x} trend to \code{Inf}
# # @param x_mid is the x value corresponding to the inflection point
# # @param g is a numeric parameter representing the assymetry around the
# # inflection point
# # Dr. Lumi Package
# d3SSl5 <- function(x, scal, l_asy, r_asy, x_mid, g){
#     ans <-  -((r_asy - l_asy) * (((1 + 10^(scal * (x - x_mid)))^(((g -
#             1) - 1) - 1) * (((g - 1) - 1) * (10^(scal * (x -
#             x_mid)) * (log(10) * scal))) * ((g - 1) * (10^(scal *
#             (x - x_mid)) * (log(10) * scal))) + (1 + 10^(scal * (x -
#             x_mid)))^((g - 1) - 1) * ((g - 1) * (10^(scal *
#             (x - x_mid)) * (log(10) * scal) * (log(10) * scal)))) *
#             (g * (10^(scal * (x - x_mid)) * (log(10) * scal))) +
#             (1 + 10^(scal * (x - x_mid)))^((g - 1) - 1) * ((g -
#             1) * (10^(scal * (x - x_mid)) * (log(10) * scal))) *
#             (g * (10^(scal * (x - x_mid)) * (log(10) * scal) *
#             (log(10) * scal))) + ((1 + 10^(scal * (x - x_mid)))^((g -
#             1) - 1) * ((g - 1) * (10^(scal * (x - x_mid)) * (log(10) *
#             scal))) * (g * (10^(scal * (x - x_mid)) * (log(10) *
#             scal) * (log(10) * scal))) + (1 + 10^(scal *
#             (x - x_mid)))^(g -
#             1) * (g * (10^(scal * (x - x_mid)) * (log(10) * scal) *
#             (log(10) * scal) * (log(10) * scal)))))/((1 + 10^(scal *
#             (x - x_mid)))^g)^2 - (r_asy - l_asy) * ((1 + 10^(scal *
#             (x - x_mid)))^((g - 1) - 1) * ((g - 1) * (10^(scal *
#             (x - x_mid)) * (log(10) * scal))) * (g * (10^(scal *
#             (x - x_mid)) * (log(10) * scal))) + (1 + 10^(scal * (x -
#             x_mid)))^(g - 1) * (g * (10^(scal * (x - x_mid)) *
#             (log(10) * scal) * (log(10) * scal)))) * (2 * ((1 + 10^(scal *
#             (x - x_mid)))^(g - 1) * (g * (10^(scal * (x -
#             x_mid)) * (log(10) * scal))) *
#             ((1 + 10^(scal * (x - x_mid)))^g)))/(((1 +
#             10^(scal * (x - x_mid)))^g)^2)^2 - (((r_asy - l_asy) *
#             ((1 + 10^(scal * (x - x_mid)))^((g - 1) - 1) * ((g -
#             1) * (10^(scal * (x - x_mid)) * (log(10) * scal))) *
#             (g * (10^(scal * (x - x_mid)) * (log(10) * scal))) +
#             (1 + 10^(scal * (x - x_mid)))^(g - 1) * (g *
#             (10^(scal * (x - x_mid)) * (log(10) * scal) * (log(10) *
#             scal)))) * (2 * ((1 + 10^(scal * (x - x_mid)))^(g -
#             1) * (g * (10^(scal * (x - x_mid)) * (log(10) * scal))) *
#             ((1 + 10^(scal * (x - x_mid)))^g))) + (r_asy - l_asy) *
#             ((1 + 10^(scal * (x - x_mid)))^(g - 1) * (g *
#             (10^(scal * (x - x_mid)) * (log(10) * scal)))) * (2 *
#             (((1 + 10^(scal * (x - x_mid)))^((g - 1) - 1) * ((g -
#             1) * (10^(scal * (x - x_mid)) * (log(10) * scal))) *
#             (g * (10^(scal * (x - x_mid)) * (log(10) * scal))) +
#             (1 + 10^(scal * (x - x_mid)))^(g - 1) * (g *
#             (10^(scal * (x - x_mid)) * (log(10) * scal) * (log(10) *
#             scal)))) * ((1 + 10^(scal * (x - x_mid)))^g) +
#             (1 + 10^(scal * (x - x_mid)))^(g - 1) * (g *
#             (10^(scal * (x - x_mid)) * (log(10) * scal))) *
#             ((1 + 10^(scal * (x - x_mid)))^(g - 1) * (g *
#             (10^(scal * (x - x_mid)) * (log(10) * scal)))))))/(((1 +
#             10^(scal * (x - x_mid)))^g)^2)^2 - (r_asy - l_asy) *
#             ((1 + 10^(scal * (x - x_mid)))^(g - 1) * (g *
#             (10^(scal * (x - x_mid)) * (log(10) * scal)))) * (2 *
#             ((1 + 10^(scal * (x - x_mid)))^(g - 1) * (g *
#             (10^(scal * (x - x_mid)) * (log(10) * scal))) * ((1 +
#             10^(scal * (x - x_mid)))^g))) * (2 * (2 * ((1 +
#             10^(scal * (x - x_mid)))^(g - 1) * (g * (10^(scal *
#             (x - x_mid)) * (log(10) * scal))) * ((1 + 10^(scal * (x -
#             x_mid)))^g)) * (((1 + 10^(scal *
#             (x - x_mid)))^g)^2)))/((((1 +
#             10^(scal * (x - x_mid)))^g)^2)^2)^2))
#     ans
# }
#
# # Third order derivative for exponential fit
# d3SSexp <- function(x, scal, xmid, l_asy) {
#   ans <- (scal - l_asy) * (xmid^3) * exp(xmid*x)
#   ans
# }
#
# # Third derivative of the drda 5 parameter logistic function
# d3SSdrda <- function(x, nu, eta, phi, delta, r_asy) {
#
#   # Intermediate terms
#   exp_term <- exp(-eta * (x - phi))  # exp(-eta * (x - phi))
#   denom <- 1 + nu * exp_term  # 1 + nu * exp_term
#
#   # First derivative of g(x)
#   g_prime <- -eta * exp_term / (denom^(1/nu + 1))
#
#   # Second derivative of g(x)
#   g_prime2 <- (eta^2 * exp_term * (nu * exp_term - 1)) / (denom^(1/nu + 2))
#
#   # Third derivative of g(x)
#   g_prime3 <- (eta^3 * exp_term * (nu * exp_term - 1)^2 - eta^2 * exp_term * (nu * exp_term - 1)) / (denom^(1/nu + 3))
#
#   # Complete third derivative of f(x)
#   f_prime3 <- (delta) * g_prime3
#
#   return(f_prime3)
# }

# Function to compute robust 5 parameter nls model
# bkg is the blank_method
compute_nls_5 <- function(dat, g_value, bkg, is_log_mfi_axis){
  init1 <- getInitial(mfi ~ SSlogis5(log_dilution, l_asy, r_asy,xmid, scal, g_value),
                      data = dat)
  cat("Init1 Parameters \n")
  print(init1)

  l_asy_init <- ifelse(init1["l_asy"] < 0, 0, init1["l_asy"])
  # l_asy_init <- init1["l_asy"]

  # Extract initial values for initial fit.
  manual_init <- list(
    l_asy = l_asy_init,
    r_asy = init1["r_asy"],
    xmid = init1["xmid"],
    scal = init1["scal"],
    g = g_value              # g from input.
  )

  lower_constraints <- list(
    l_asy = 0,
    r_asy = 10,
    xmid = -5,
    scal = 0.01,
    g = 0.1
  )

  print(manual_init)
  # Initial asymmetry factor g estimate of 1,if not provided for 4 Parameter logistic regression
  # init_g <- ifelse(missing(g_value), 1, g_value)
  cat("initial g: ", g_value, "\n")
  #init_g <- g_value
  print(paste(dat$mfi, dat$log_dilution, sep = "__"))
  # we commented this out because it was too constrained
  # initfit <- nls(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))^g),
  #                 data = dat,
  #                 #tart = c(init1,g = init_g),  # Do I need starting estimates for each parameter what values make sense?
  #                 start = manual_init,
  #                 control = nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE)
  # )
  # if (initfit$convInfo$isConv == FALSE) {
  #   cat("NLS initfit failed. Switching to DRDA ")
  #   return(NULL)
  # }
  # cat("Before init2 ")
  # init2 <- coefficients(initfit)
  # print(init2)
  # #variance-covariance matrix
  # vcov_matrix <- vcov(initfit)

  # if (coefficients(initfit)[1] < 0) {
  #  print(init2)
  nls_mfi <- max(dat$mfi) #upper bound for l_asy, 2* for upper

  init_con <- nls(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))^g),
                  data = dat,
                  #tart = c(init1,g = init_g),  # Do I need starting estimates for each parameter what values make sense?
                  lower = lower_constraints, # scale 0.18
                  upper = c(Inf, Inf, Inf, Inf, Inf), # largest xmid is 11 in dataset scale 1.58
                  start = manual_init,
                  algorithm = "port",
                  control = nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE)
  )
  if (init_con$convInfo$isConv == FALSE) {
    cat("NLS init con failed. Switching to DRDA ")
    return(NULL)
  }
  init2 <- coefficients(init_con)
  #variance-covariance matrix
  vcov_matrix <- vcov(init_con)
  # }



  cat("Init2 Parameters \n")
  print(init2)


  #variance-covariance matrix
  # vcov_matrix <- vcov(initfit)
  # convert to correlation matrix
  cor_matrix <- cov2cor(vcov_matrix)
  cat("Correlation Matrix\n")
  print(cor_matrix)

  print("above fit")

  print("Max log dilution:")
  print(max(dat$log_dilution))

  #fit <- NULL
  tryCatch({
    print("in catch block 5 parameters")
    nls_mfi <- max(dat$mfi)
    # original
    lower_c <- c(10,0,-5, 0.01,0.1)
    upper_c <- c(2*nls_mfi, nls_mfi, 15, 2, 2)

    fit <- robustbase::nlrob(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))^g), # assign global for debug
                             data = dat
                             , method = "M"
                             , algorithm = "port"
                             , start = init2 # This has the 5th parameter g.
                             , na.action = na.exclude
                             , maxit = 500
                             , doCov = TRUE
                             # , weights = welsch_w
                             ,lower = lower_constraints
                             ,upper = upper_c
                             # , cntrl = c(nlrob.control("CM", psi = "welsh", tuning.chi = 2.11))
                             , tol = 1e-06
                             , control = nls.control(maxiter = 500,
                                                     tol = 1e-06,
                                                     minFactor = 1/2048,
                                                     printEval = FALSE,
                                                     warnOnly = TRUE,
                                                     scaleOffset = 0,
                                                     nDcentral = FALSE)
    )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # end tryCatch statement

  #fit_view <<- fit

  if (!is.null(fit)) {
    print("above glance")
    iter <- c(as.numeric(fit$iter))
    status <- c(fit$status)

    print("printing fit")
    print(fit$status)
    if (!is.na(fit$status)){
      dat$weights <- fit$rweights # add model weights  or set them to 1.0 if status is NA.
    } else {
      dat$weights <- 1.0
    }


    cat("RENAMED Coefficents")
    #browser()
    # set the names of the parameters in
    params <- fit$m$getPars()
    names(params) <- c("l_asy", "r_asy", "xmid", "scal")
    fit$m$setPars(params)

    l_asy <- c(as.numeric(fit$coefficients[1]))
    r_asy <- c(as.numeric(fit$coefficients[2]))
    x_mid <- c(as.numeric(fit$coefficients[3]))
    scale <- c(as.numeric(fit$coefficients[4]))
    sigma <- c(as.numeric(fit$coefficients[5])) # Sigma is g.

    has_l_asy <- "l_asy" %in% names(fit$coefficients)
    has_r_asy <- "r_asy" %in% names(fit$coefficients)
    has_x_mid <- "xmid" %in% names(fit$coefficients)
    has_scal <- "scal" %in% names(fit$coefficients)


    cat("R_asy beefore limits", r_asy, "\n")
    cat("l_asy before limits", l_asy, "\n")
    cat("x_mid before limits", x_mid, "\n")
    cat("scal before limits", scale, "\n")
    cat("g before limits", sigma, "\n")

    params <- fit$m$getPars()
    cat("params", params)
    print(class(params))
    # Coefficients
    coef_a <- as.numeric(fit$m$getPars()[1]) # left asymptote
    cat("coef a", coef_a)
    coef_d <- as.numeric(fit$m$getPars()[2]) # right asymptote
    cat("coef_d", coef_d)
    coef_k <- 4.6805  # ask
    if (is_log_mfi_axis) {
      bendlower <- round(((coef_a - coef_d) / (1 + (1/coef_k))) + coef_d,3)
      bendupper <- round(((coef_a - coef_d) / (1 + coef_k)) + coef_d,3)
    } else {
      bendlower <- round(((coef_a - coef_d) / (1 + (1/coef_k))) + coef_d,0)
      bendupper <- round(((coef_a - coef_d) / (1 + coef_k)) + coef_d,0)
    }
    cat("\nbendlower", bendlower, "bendupper", bendupper)


    # set r_asy and l_asy again to make sure it is in scope
    r_asy <- r_asy
    l_asy <- l_asy

    # Lower Limit of Detection predictNLS
    predictions <- predict_nls(fit, newdata = data.frame(log_dilution = -10), interval = "confidence") # estimate, est error, 2.5%, 97.5%
    cat("\nclass of predictions", class(predictions))
    cat(colnames(predictions))
    cat("\npredictions", predictions)
    if (is_log_mfi_axis) {
      llod <- round(predictions[4],3)
    } else {
      llod <- round(predictions[4],0) # nearest whole number  2.5%
    }
    # #

    # if (!exists("coef_d")){
    #   print("right asymptote not found")
    #   r_asy <- NA
    #   ulod <- NA
    # }
    # else {
    predictions_ulod <- predict_nls(fit, newdata = data.frame(log_dilution = -0.01, r_asy = coef_d), interval = "confidence") # estimate, est error, 2.5%, 97.5%
    cat("\npredictions ulod", predictions_ulod)
   # cat("\npredictions ulod", predictions_ulod)
    if (is_log_mfi_axis) {
      ulod <- round(predictions_ulod[3],3)
    } else {
      ulod <- round(predictions_ulod[3],0)
    }

    # if the ulod < lower bend, estimate from t distribution
    # if (predictions_ulod[3] < bendlower) {
    #   se <- as.data.frame(summary(fit)$coefficients)["r_asy", "Std. Error"]
    #   ulod <- round(coef_d - 1.96*se, 0)
    # }

    if (predictions_ulod[3] < llod) {
      cat("ulod is less than llod")

      se <- as.data.frame(summary(fit)$coefficients)["r_asy", "Std. Error"]
      if (is_log_mfi_axis) {
        ulod <- round(coef_d - 1.96*se, 3)
      } else {
        ulod <- round(coef_d - 1.96*se, 0)
      }
      # If a negative ULOD, transition to next model
      if (ulod < 0) {

        cat("Negative ULOD. Switch to attempt DRDA fitting")
        return(NULL)
      }
    }


    rsquare_fit <- round(modelr::rsquare(fit,augment(fit)),3) # need broom package
    # calculate mse
    mse <- mean(resid(fit)^2, na.rm = T)
    # mean of observed mfi
    mean_obs_mfi<- mean(dat$mfi, na.rm = TRUE)
    # coefficient of variation.
    cv <- (sqrt(mse) / mean_obs_mfi) * 100

    # CV for log dilution
    # sd_log_dilution <- sd(dat$log_dilution, na.rm = TRUE)
    # mean_log_dilution <- mean(dat$log_dilution, na.rm = TRUE)
    # cv_log_dilution <- (sd_log_dilution / mean_log_dilution) * 100
    # add g estimate or NA
    g_est <- ifelse(length(fit$m$getPars()) == 5, as.numeric(fit$m$getPars()[5]), NA)

    # cat("\n llod: ", llod)
    # cat("\nulod", ulod)

    #x_mid <- xmid

    glance_fit <- data.frame(study_accession = unique(dat$study_accession),
                             experiment_accession = unique(dat$experiment_accession),
                             plateid = unique(dat$plateid),
                             antigen = unique(dat$antigen),
                             iter = iter,
                             status = status,
                             l_asy, r_asy , x_mid, scale,
                             bendlower, bendupper, llod, ulod,
                             as.data.frame(glance(fit)[4:9]),
                             rsquare_fit, mse, cv)
    glance_fit$source <- unique(dat$source) # column is now source not ssource

    # check for high mse
    if (glance_fit$mse > 100000) {
      cat("mse is > 100000")
      return(NULL)
    }

    # add g estimate
    glance_fit$g <- g_est
    if (is.na(glance_fit$g)) {
      glance_fit$crit <- "nls_4"
    }else {
      glance_fit$crit <- "nls_5"
    }

    # Limits of Quantification
    loq_der <- loq_derivitives(fit = fit, glance_fit = glance_fit)
    glance_fit$lloq <- loq_der$lloq
    glance_fit$uloq <- loq_der$uloq
    glance_fit$loq_method <- loq_der$method

    # Background Method
    glance_fit$bkg_method <- bkg
    glance_fit$is_log_mfi_axis <- is_log_mfi_axis

    # model formula
    glance_fit$formula <- "Y ~ d + ((a-d)/(1 + exp((X-c)/b))^g"
    # rename columns to prepare save
    names(glance_fit)[names(glance_fit) == "AIC"] <- "aic"
    names(glance_fit)[names(glance_fit) == "BIC"] <- "bic"
    names(glance_fit)[names(glance_fit) == "logLik"] <- "loglik"
    names(glance_fit)[names(glance_fit) == "df.residual"] <- "dfresidual"

    cat("Glance Fit Source: \n")
    cat(glance_fit$source)

    print(head(glance_fit))

    print("\n after glance is created")
    # round summary table to 2 decimal places
    fit_tab <- socviz::round_df(as.data.frame(broom::tidy(fit)[,c(1:3,5)]), dig = 2)
    #fit_tab <- rounddf(as.data.frame(broom::tidy(fit)[,c(1:3,5)]), digits = 2, pad = TRUE)
    fit_tab$signif <- stars.pval(c(as.numeric(fit_tab$p.value)))
    #print(fit_tab) # with the p-value

    # fit_tab <- fit_tab[ , c(1:3,5)]
    #print(fit_tab)
    fit_tab$study_accession <- unique(dat$study_accession)
    fit_tab$experiment_accession <- unique(dat$experiment_accession)
    fit_tab$antigen <- unique(dat$antigen)
    fit_tab$plateid <- unique(dat$plateid)
    fit_tab$source <- unique(dat$source) # 1 s

    # rename standard error column And p-value column
    names(fit_tab)[names(fit_tab) == "std.error"] <- "std_error"
    names(fit_tab)[names(fit_tab) == "p.value"] <- "p_value"

    # Fix term column
    #fit_tab$term <- gsub("\\.\\w+", "", fit_tab$term)  # Remove any suffixes
    cat("Model Fit Tab")
    print(fit_tab)

    #THIS line removes the p-value
    fit_tab <- as.list(fit_tab[, c(5:8,1:4,9)])
    print(names(fit_tab))
    print("after fit_tab is created")

    # names(dat)[names(dat) == "n"] <- "nbeads" # n beads fluorescing antibody_n
    #
    # # dat$plate_id <- fit_tab$plateid # fittab instead of dat BEFORE with no multiple plates.
    #
    # #dat$plate_id <- rep(fit_tab$plateid, nrow(dat))  #TEST
    # dat$plate_id <- fit_tab$plateid[1]
    #
    # # print(paste("dat$antigen",antigen))
    #
    # cat("Dat names: ",names(dat))
    # print(dat)
    # # dat <<-dat
    # # plate is not a column here and ssource is now source.
    # dat_stor <- dat[ , c("study_accession", "experiment_accession", "antigen", "mfi",
    #                      "nbeads", "plate_id",  "source", "log_dilution","weights")]
    # print("above return")
    # print(names(dat_stor))

    newlist <-  list(fit_tab, glance_fit, fit)
    return(newlist)
  } else {
    print(summary(fit))
    return(NULL)
  }

}


# function to compute robust 4 parameter nls model
compute_nls_4 <- function(dat, bkg, is_log_mfi_axis) {
  cat("starting nls 4 compute")
  init1 <- tryCatch({
    getInitial(mfi ~ SSfpl(log_dilution, l_asy, r_asy, xmid, scal),
               data = dat,
               control=nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE))
  }, warning = function(w){
    cat("warning:", conditionMessage(w), "\n")
    NULL  # Return NULL if an error occurs
  }, error = function(e) {
    cat("error:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(init1)) {
    return(NULL)
  }

  cat("Init1 Parameters \n")
  print(init1)

  l_asy_init <- ifelse(init1["l_asy"] < 0, 0, init1["l_asy"])
  # Extract 4 initial values for initial fit.
  manual_4_par_init <- list(
    l_asy = l_asy_init,
    r_asy = init1["r_asy"],
    xmid = init1["xmid"],
    scal = init1["scal"]
  )
  lower_constraints <- list(
    l_asy = 0,
    r_asy = 10,
    xmid = -5,
    scal = 0.01
  )
  #  c(r_asy = 10,l_asy = 0, xmid = -5, scal =  0.01)
  #
  cat("Before initfit in compute 4 parameter")
  #fit a 4 parameter model
  initfit <- tryCatch({nls(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
                           data = dat,
                           start = manual_4_par_init,
                           control = nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE))}
                      ,
                      error = function(e){
                        cat("error:", conditionMessage(e), "\n")
                        NULL  # Return NULL if an error occurs
                      })

  cat("Initfit")
  print(initfit)

  if (is.null(initfit)) {
    cat("NLS 4 Parameter initfit failed. Switching to Exponential ")
    return(NULL)
  }
  if (initfit$convInfo$isConv == FALSE) {
    cat("NLS 4 Parameter initfit failed. Switching to Exponential ")
    return(NULL)
  }

  init2 <- coefficients(initfit)

  if (as.numeric(init2[1]) < 0) {
    init2[1] <- 0
  }

  cat("Init2 Parameters \n")

  print(init2)

  #variance-covariance matrix
  vcov_matrix_4p <- vcov(initfit)


  if (coefficients(initfit)[1] < 0) {
    print(init2)
    nls_mfi <- max(dat$mfi) #upper bound for l_asy, 2* for upper

    init_con <- nls(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
                    data = dat,
                    #tart = c(init1,g = init_g),  # Do I need starting estimates for each parameter what values make sense?
                    lower = lower_constraints, # scale 0.18
                    upper = c(2 *nls_mfi, 2 * nls_mfi, Inf, Inf), # largest xmid is 11 in dataset scale 1.58
                    start = manual_4_par_init,
                    algorithm = "port",
                    control = nls.control(maxiter = 500, minFactor=1/2048, warnOnly=TRUE)
    )

    if (init_con$convInfo$isConv == FALSE) {
      cat("NLS 4 param init con failed. Switching to exponential")
      return(NULL)
    }
    init2 <- coefficients(init_con)
    #variance-covariance matrix
    vcov_matrix_4p <- vcov(init_con)
  }

  # print(fit$status)
  # if (!is.na(fit$status)){
  #   dat$weights <- fit$rweights # add model weights  or set them to 1.0 if status is NA.
  # } else {
  #   dat$weights <- 1.0
  # }
  #


  #variance-covariance matrix
  # vcov_matrix <- vcov(initfit)
  # convert to correlation matrix
  # cor_matrix_4p <- cov2cor(vcov_matrix_4p)
  # cat("Correlation Matrix 4 parameters \n")
  # print(cor_matrix_4p)

  print("above fit")

  print("Max log dilution:")
  print(max(dat$log_dilution))

  tryCatch({
    print("in catch block 4 params")
    nls_mfi <- max(dat$mfi)

    # lower_c_4p <- c(10,0,-5, 0.01)
    # upper_c_4p <- c(2*nls_mfi, nls_mfi, 15, 2)

    fit <- tryCatch({
      print("Attempting 4-parameter fit")

      nls_mfi <- max(dat$mfi)
      # lower_c_4p <- c(10, 0, -5, 0.01)
      upper_c_4p <- c(2 *nls_mfi, 2 *nls_mfi, Inf, Inf)

      # check if initial constraints are violatted
      # if (manual_4_par_init$l_asy < lower_constraints$l_asy || manual_4_par_init$l_asy > upper_c_4p$l_asy ||
      #     manual_4_par_init$r_asy < lower_constraints$r_asy || manual_4_par_init$r_asy > upper_c_4p$r_asy ||
      #     manual_4_par_init$xmid < lower_constraints$xmid || manual_4_par_init$xmid > upper_c_4p$xmid ||
      #     manual_4_par_init$scal < lower_constraints$scal || manual_4_par_init$scal > upper_c_4p$scal) {
      #
      #   cat("ERROR: Initial parameters violate constraints. Setting fit to NULL.\n")
      #   NULL
      # } else {
      robustbase::nlrob(
        mfi ~ r_asy + ((l_asy - r_asy) / (1 + exp((log_dilution - xmid) / scal))),
        data = dat,
        method = "M",
        algorithm = "port",
        start = manual_4_par_init,
        na.action = na.exclude,
        maxit = 500,
        doCov = TRUE,
        lower = lower_constraints,
        upper = upper_c_4p,
        tol = 1e-06,
        control = nls.control(
          maxiter = 500,
          tol = 1e-06,
          minFactor = 1 / 2048,
          printEval = FALSE,
          warnOnly = TRUE,
          scaleOffset = 0,
          nDcentral = FALSE
        )
      )
    }, error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
      NULL  # Return NULL if an error occurs
    })
    #   fit <<- robustbase::nlrob(mfi ~ r_asy + ((l_asy-r_asy)/(1 + exp((log_dilution-xmid)/scal))),
    #                            data = dat
    #                            , method = "M"
    #                            , algorithm = "port"
    #                            , start = manual_4_par_init#init2
    #                            , na.action = na.exclude
    #                            , maxit = 500
    #                            , doCov = TRUE
    #                            # , weights = welsch_w
    #                            , lower = lower_c_4p
    #                            , upper = upper_c_4p
    #                            # , cntrl = c(nlrob.control("CM", psi = "welsh", tuning.chi = 2.11))
    #                            , tol = 1e-06
    #                            , control = nls.control(maxiter = 500,
    #                                                    tol = 1e-06,
    #                                                    minFactor = 1/2048,
    #                                                    printEval = FALSE,
    #                                                    warnOnly = TRUE,
    #                                                    scaleOffset = 0,
    #                                                    nDcentral = FALSE)
    #   )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  # print(fit)
  # print(summary(fit))
  # print(summary(fit)$coef)
  #if (!is.null(fit)) {
 # check if fit is null before proceeding.
  if (is.null(fit)) {
    return(NULL)
  }
  # Check if r_asy SE will cause model to not fit well and if so, go to exponential
 # fit_view <<-fit
  summary_robust <- broom::tidy(fit)
  print(summary_robust)
  # print(summary_robust$term)

  if (all(summary_robust$term == c("l_asy.l_asy", "r_asy.r_asy", "xmid.xmid", "scal.scal"))) {
    summary_robust[summary_robust$term == "l_asy.l_asy",]$term <- "l_asy"
    summary_robust[summary_robust$term == "r_asy.r_asy",]$term <- "r_asy"
  }

  r_asy_std_error <- summary_robust[summary_robust$term == "r_asy",]$std.error
  l_asy_est <- summary_robust[summary_robust$term == "l_asy",]$estimate
  r_asy_est <- summary_robust[summary_robust$term == "r_asy",]$estimate
  # if (is.na(r_asy_std_error)) {
  #   return(NULL)
  # } else
  if (r_asy_std_error * 1.96 > (r_asy_est - l_asy_est)){
    return(NULL)
  }
  #  }

  if (!is.null(fit)) {


    print("above glance")
    iter <- c(as.numeric(fit$iter))
    status <- c(fit$status)

    print("printing fit")
    print(fit$status)
    if (!is.na(fit$status)){
      dat$weights <- fit$rweights # add model weights  or set them to 1.0 if status is NA.
    } else {
      dat$weights <- 1.0
    }

    cat("RENAMED Coefficents")

    # Extract parameter estimates
    # set the names of the parameters in
    params <- fit$m$getPars()
    names(params) <- c("l_asy", "r_asy", "xmid", "scal")
    fit$m$setPars(params)

    l_asy <- c(as.numeric(fit$coefficients[1]))
    r_asy <- c(as.numeric(fit$coefficients[2]))
    x_mid <- c(as.numeric(fit$coefficients[3]))
    scale <- c(as.numeric(fit$coefficients[4]))
    sigma <- c(as.numeric(fit$coefficients[5])) # Sigma is g.

    has_l_asy <- "l_asy" %in% names(fit$coefficients)
    has_r_asy <- "r_asy" %in% names(fit$coefficients)
    has_x_mid <- "xmid" %in% names(fit$coefficients)
    has_scal <- "scal" %in% names(fit$coefficients)


    cat("R_asy beefore limits", r_asy, "\n")
    cat("l_asy before limits", l_asy, "\n")
    cat("x_mid before limits", x_mid, "\n")
    cat("scal before limits", scale, "\n")
    cat("g before limits", sigma, "\n")

    params <- fit$m$getPars()
    cat("params", params)
    print(class(params))
    # Coefficients
    coef_a <- as.numeric(fit$m$getPars()[1]) # left asymptote
    cat("coef a", coef_a)
    coef_d <- as.numeric(fit$m$getPars()[2]) # right asymptote
    cat("coef_d", coef_d)
    coef_k <- 4.6805  # ask

    if (is_log_mfi_axis) {
      bendlower <- round(((coef_a - coef_d) / (1 + (1/coef_k))) + coef_d,3)
      bendupper <- round(((coef_a - coef_d) / (1 + coef_k)) + coef_d,3)
    } else {
      bendlower <- round(((coef_a - coef_d) / (1 + (1/coef_k))) + coef_d,0)
      bendupper <- round(((coef_a - coef_d) / (1 + coef_k)) + coef_d,0)
    }
    cat("\nbendlower", bendlower, "bendupper", bendupper)


    # set r_asy and l_asy again to make sure it is in scope
    r_asy <- r_asy
    l_asy <- l_asy

    # Lower Limit of Detection
    predictions <- predict_nls(fit, newdata = data.frame(log_dilution = min(dat$log_dilution) *1.1), interval = "confidence") # estimate, est error, 2.5%, 97.5%
    cat("\nclass of llod predictions", class(predictions))
    cat("\n\n")
    cat(colnames(predictions))
    cat("\npredictions", predictions)
    cat("\n\n")
    if (is_log_mfi_axis) {
      llod <- round(predictions[4],3)
    } else {
       llod <- round(predictions[4],0) # nearest whole number  2.5%
    }
    # # if the llod is > 90% of the r_asy
    if (llod >= (0.90*r_asy)) {
      #test <<- "test"
      cat("before predictNLS 5 param")
     #prediction_ci <- predictNLS(fit, newdata = data.frame(log_dilution = min(dat$log_dilution) *1.1), interval = "confidence")
      prediction_ci <- tryCatch({
        predictNLS(fit, newdata = data.frame(log_dilution = min(dat$log_dilution) * 1.1),
                   interval = "confidence")
      }, error = function(e) {
        message("Prediction failed: ", e$message)
        return(NULL)
      })
      if (is.null(prediction_ci)){
        return(NULL)
      }

      if (is_log_mfi_axis) {
        llod <- round(prediction_ci$summary$`Prop.97.5%`,3)
      } else {
        llod <- round(prediction_ci$summary$`Prop.97.5%`,0)
      }
    }

    # if (!exists("coef_d")){
    #   print("right asymptote not found")
    #   r_asy <- NA
    #   ulod <- NA
    # }
    # else {
    predictions_ulod <- predict_nls(fit, newdata = data.frame(log_dilution = -0.01, r_asy = coef_d), interval = "confidence") # estimate, est error, 2.5%, 97.5%
    cat("\nclass of ulod predictions", class(predictions_ulod))
    cat("\n\n")
    cat(colnames(predictions_ulod))
    cat("\npredictions", predictions_ulod)
    cat("\n\n")
    if (is_log_mfi_axis) {
      ulod <- round(predictions_ulod[3],3)
    } else {
       ulod <- round(predictions_ulod[3],0)
    }
    if (predictions_ulod[3] < llod) {
      se <- as.data.frame(summary(fit)$coefficients)["r_asy", "Std. Error"]
      if (is_log_mfi_axis) {
        ulod <- round(coef_d - 1.96*se, 3)
      } else {
         ulod <- round(coef_d - 1.96*se, 0)
      }
    }

    if (llod >= (0.90*r_asy)) {
       #test <<- "test"
      cat("before predictNLS 4 param")
       prediction_ci <- predictNLS(fit, newdata = data.frame(log_dilution = min(dat$log_dilution) *1.1), interval = "confidence")
       if (is_log_mfi_axis) {
         ulod <- round(prediction_ci$summary$`Prop.2.5%`,3)
       } else {
         ulod <- round(prediction_ci$summary$`Prop.2.5%`,0)
       }
     }

    rsquare_fit <- round(modelr::rsquare(fit,augment(fit)),3) # need broom package
    # calculate mse
    mse <- mean(resid(fit)^2, na.rm = T)
    # mean of observed mfi
    mean_obs_mfi<- mean(dat$mfi, na.rm = TRUE)
    # coefficient of variation.
    cv <- (sqrt(mse) / mean_obs_mfi) * 100


    # # CV for log dilution
    # sd_log_dilution <- sd(dat$log_dilution, na.rm = TRUE)
    # mean_log_dilution <- mean(dat$log_dilution, na.rm = TRUE)
    # cv_log_dilution <- (sd_log_dilution / mean_log_dilution) * 100

    # add g estimate or NA
    g_est <- ifelse(length(fit$m$getPars()) == 5, as.numeric(fit$m$getPars()[5]), NA)

    # cat("\n llod: ", llod)
    # cat("\nulod", ulod)

    #x_mid <- xmid

    glance_fit <- data.frame(study_accession = unique(dat$study_accession),
                             experiment_accession = unique(dat$experiment_accession),
                             plateid = unique(dat$plateid),
                             antigen = unique(dat$antigen),
                             iter = iter,
                             status = status,
                             l_asy, r_asy , x_mid, scale,
                             bendlower, bendupper, llod, ulod,
                             as.data.frame(glance(fit)[4:9]),
                             rsquare_fit, mse, cv)
    glance_fit$source <- unique(dat$source) # column is now source not ssource

    # add g estimate
    glance_fit$g <- g_est
    if (is.na(glance_fit$g)) {
      glance_fit$crit <- "nls_4"
    }else {
      glance_fit$crit <- "nls_5"
    }

    # Limits of Quantification
    loq_der <- loq_derivitives(fit = fit, glance_fit = glance_fit)
    glance_fit$lloq <- loq_der$lloq
    glance_fit$uloq <- loq_der$uloq
    glance_fit$loq_method <- loq_der$method

    # background/blank method
    glance_fit$bkg_method <- bkg
    # log mfi indicator
    glance_fit$is_log_mfi_axis <- is_log_mfi_axis

    # rename columns to prepare save
    names(glance_fit)[names(glance_fit) == "AIC"] <- "aic"
    names(glance_fit)[names(glance_fit) == "BIC"] <- "bic"
    names(glance_fit)[names(glance_fit) == "logLik"] <- "loglik"
    names(glance_fit)[names(glance_fit) == "df.residual"] <- "dfresidual"

    cat("Glance Fit Source: \n")
    cat(glance_fit$source)

    glance_fit$formula <- "Y ~ d + ((a-d)/(1 + exp((X-c)/b)"

    print(head(glance_fit))

    print("\n after glance is created")
    # round summary table to 2 decimal places
    fit_tab <- socviz::round_df(as.data.frame(broom::tidy(fit)[,c(1:3,5)]), dig = 2)
    #fit_tab <- rounddf(as.data.frame(broom::tidy(fit)[,c(1:3,5)]), digits = 2, pad = TRUE)
    fit_tab$signif <- stars.pval(c(as.numeric(fit_tab$p.value)))
    #print(fit_tab) # with the p-value

    # fit_tab <- fit_tab[ , c(1:3,5)]
    #print(fit_tab)
    fit_tab$study_accession <- unique(dat$study_accession)
    fit_tab$experiment_accession <- unique(dat$experiment_accession)
    fit_tab$antigen <- unique(dat$antigen)
    fit_tab$plateid <- unique(dat$plateid)
    fit_tab$source <- unique(dat$source) # 1 s


    # rename standard error column And p-value column
    names(fit_tab)[names(fit_tab) == "std.error"] <- "std_error"
    names(fit_tab)[names(fit_tab) == "p.value"] <- "p_value"

    # Fix term column
    #fit_tab$term <- gsub("\\.\\w+", "", fit_tab$term)  # Remove any suffixes
    cat("Model Fit Tab")
    print(fit_tab)

    #THIS line removes the p-value
    fit_tab <- as.list(fit_tab[, c(5:8,1:4,9)])
    print(names(fit_tab))
    print("after fit_tab is created")

    # names(dat)[names(dat) == "n"] <- "nbeads" # n beads fluorescing antibody_n
    #
    # # dat$plate_id <- fit_tab$plateid # fittab instead of dat BEFORE with no multiple plates.
    #
    # #dat$plate_id <- rep(fit_tab$plateid, nrow(dat))  #TEST
    # dat$plate_id <- fit_tab$plateid[1]
    #
    # # print(paste("dat$antigen",antigen))
    #
    # cat("Dat names: ",names(dat))
    # print(dat)
    # # dat <<-dat
    # # plate is not a column here and ssource is now source.
    # dat_stor <- dat[ , c("study_accession", "experiment_accession", "antigen", "mfi",
    #                      "nbeads", "plate_id",  "source", "log_dilution","weights")]
    # print("above return")
    # print(names(dat_stor))

    newlist <-  list(fit_tab, glance_fit, fit)
    return(newlist)
  } else {
    return(NULL)
  }
}


## Function to compute power four parameter function
compute_power_4 <- function(dat, bkg, is_log_mfi_axis) {
cat("\ncalculating power four param\n")
  start_vals <- list(
    l_asy = min(dat$mfi),       # lower asymptote
    r_asy = max(dat$mfi),       # upper asymptote
    xmid = median(dat$log_dilution), # inflection point
    scal = 1                # slope (Hill slope)
  )

  #start_vals_v <- start_vals
  dat <- dat[, !duplicated(names(dat))]
  # dat_v <<- dat
  # bkg_v <<- bkg
  # is_log_mfi_axis_v <<- is_log_mfi_axis
  # Y4 <- function(x,a,b,c,d) {
  #   q <- exp((x - c) / b)
  #   d + (a - d) / (1 + q)
  # }

  # Fit model with nlsLM (more robust than nls)
  fit <- tryCatch({
    minpack.lm::nlsLM(mfi ~ r_asy + (l_asy - r_asy) / (1 + (log_dilution / xmid)^scal),
                      data = dat,
                      start = start_vals,
                      control = nls.lm.control(maxiter = 1000)) },
    error = function(e) {
      message("Error in nlsLM: ", e$message)
      NULL  # Return NULL on error
    })
  # fit <- minpack.lm::nlsLM(mfi ~ r_asy + (l_asy - r_asy) / (1 + (log_dilution / xmid)^scal),
  #              data = dat,
  #              start = start_vals,
  #              control = nls.lm.control(maxiter = 1000))
#fit_v <<- fit
    if (!is.null(fit)) {
     glance_fit <-  nlsLM_glance(model = fit, data = dat, bkg = bkg, is_log_mfi_axis = is_log_mfi_axis)
     fit_tab <- tidy.nlsLM(model = fit, dat = dat)


    # glance_fit_v <<- glance_fit
     # create copy to put into inflection point code
     glance_fit_2 <- glance_fit
     names(glance_fit_2)[names(glance_fit_2) == "l_asy"] <- "a"
     names(glance_fit_2)[names(glance_fit_2) == "scale"] <- "b"
     names(glance_fit_2)[names(glance_fit_2) == "x_mid"] <- "c"
     names(glance_fit_2)[names(glance_fit_2) == "r_asy"] <- "d"

     # obtain limits of detection and limits of quantification
     inflect_glance <- inflection_point(glance_fit_2)
     bendlower <- inflect_glance$y_inflectionL_3
     bendupper <- inflect_glance$y_inflectionU_3

     if (is_log_mfi_axis) {
       bendlower <- round(bendlower,3)
       bendupper <- round(bendupper,3)
     } else {
       bendlower <- round(bendlower,0)
       bendupper <- round(bendupper,0)
     }

     uloq <- inflect_glance$x_inflectionU_3
     lloq <- inflect_glance$x_inflectionL_3


    # predictions <-  predictNLS(fit, newdata = data.frame(log_dilution = min(dat$log_dilution) * 1.1),
    #             interval = "confidence")
    # ulod <-  predictions$summary$`Prop.2.5%`
    # llod <- predictions$summary$`Prop.97.5%`

     llod_pred <- predictNLS(fit, newdata = data.frame(log_dilution = min(dat$log_dilution) * 1.1),
                             interval = "confidence")
     if (is_log_mfi_axis) {
       llod <- round(llod_pred$summary$`Prop.97.5%`, 3)
     } else {
       llod <- round(llod_pred$summary$`Prop.97.5%`, 0)
     }


     # ULOD — predict at the highest dilution
     ulod_pred <- predictNLS(fit, newdata = data.frame(log_dilution = max(dat$log_dilution) * 0.9),
                             interval = "confidence")
     if (is_log_mfi_axis) {
       ulod <- round(ulod_pred$summary$`Prop.2.5%`,3)
     } else {
       ulod <- round(ulod_pred$summary$`Prop.2.5%`,0)
     }


     glance_fit$bendlower <- bendlower
     glance_fit$bendupper <- bendupper
     glance_fit$lloq <- lloq
     glance_fit$uloq <- uloq
     glance_fit$ulod <- ulod
     glance_fit$llod <- llod
     glance_fit$loq_method <- inflect_glance$used


     final_list <- list(fit_tab, glance_fit, fit)

     return(final_list)
    } else {
      return(NULL)
    }
}

nlsLM_glance <- function(model, data, bkg, is_log_mfi_axis) {

  s <- summary(model)

  # Extract parameter estimates
  coefs <- coef(s)

  # Named vector to data frame row
  coef_df <- as.data.frame(t(coefs[, "Estimate"]))
 # names(coef_df) <- paste0("param_", names(coef_df))

  # Residual stats
  sigma <- s$sigma
  df_resid <- s$df[2]

  # Compute R-squared
  response <- data$mfi
  rss <- sum(residuals(model)^2)
  tss <- sum((response - mean(response))^2)
  r_squared <- 1 - rss / tss

  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  logLik_val <- as.numeric(logLik(model))
  df  <- attr(logLik(model), "df")
  converged <- model$convInfo$isConv
  iter <- model$convInfo$finIter

  crit <- "nlslm_4"

  n_obs <- length(residuals(model))

  # calculate mse
  mse <- mean(resid(model)^2, na.rm = T)
  # mean of observed mfi
  mean_obs_mfi<- mean(data$mfi, na.rm = TRUE)
  # coefficient of variation.
  cv <- (sqrt(mse) / mean_obs_mfi) * 100


  # Combine everything
 glance_df <-  data.frame(
   study_accession = unique(data$study_accession),
   experiment_accession = unique(data$experiment_accession),
   plateid = unique(data$plateid),
   antigen = unique(data$antigen),
    coef_df,
    sigma = sigma,
    dfresidual = df_resid,
    r_squared = r_squared,
    aic = aic,
    bic = bic,
    loglik = logLik_val,
    df = df,
    mse = mse,
    cv = cv,
    nobs = n_obs,
    converged = converged,
    iter = iter,
    source = unique(data$source),
    crit = crit,
    bkg_method =  bkg,
    is_log_mfi_axis = is_log_mfi_axis,
    formula = " Y ~ d + (a - d) / (1 + (x / c)^b)"
  )

 names(glance_df)[names(glance_df) == "scal"] <- "scale"
 names(glance_df)[names(glance_df) == "xmid"] <- "x_mid"

 return(glance_df)
}

tidy.nlsLM <- function(model, dat) {
  s <- summary(model)
  out <- as.data.frame(s$coefficients)
  tidy_df <- tibble::tibble(
    term = rownames(out),
    estimate = out[, "Estimate"],
    std.error = out[, "Std. Error"],
    statistic = out[, "t value"],
    p.value = out[, "Pr(>|t|)"]
  )

  tidy_df$signif <- stars.pval(c(as.numeric(tidy_df$p.value)))
  tidy_df$study_accession <- unique(dat$study_accession)
  tidy_df$experiment_accession <- unique(dat$experiment_accession)
  tidy_df$antigen <- unique(dat$antigen)
  tidy_df$plateid <- unique(dat$plateid)
  tidy_df$source <- unique(dat$source)


  # rename standard error column And p-value column
  names(tidy_df)[names(tidy_df) == "std.error"] <- "std_error"
  names(tidy_df)[names(tidy_df) == "p.value"] <- "p_value"

  return(tidy_df)
}

## Helper functions to get derivatives for Bend limits and quantification
# functional form to get outcome
Ylm4 <- function(log_dilution,l_asy,scal,xmid,r_asy) {
  r_asy + (l_asy - r_asy) / (1 + (log_dilution / xmid)^scal)
}

dYlm4 <- Deriv(Ylm4, "log_dilution", nderiv = 1)
d2Ylm4 <- Deriv(Ylm4, "log_dilution", nderiv = 2)
d3Ylm4 <- Deriv(Ylm4, "log_dilution", nderiv = 3)

get_functions <- function(crit) {
  switch(crit,
         "nlslm_4" = list(
           Y = Ylm4,
           dY = dYlm4,
           d2Y = d2Ylm4
         ),

         "nls_4" = list(
           Y = Y4,
           dY = Y41,
           d2Y = Y42
         ),

         "drda_5" = list(
           Y = Y5,
           dY = Y51,
           d2Y = Y52
         ),

         "nls_5" = list(
           Y = Y5,
           dY = Y51,
           d2Y = Y52
         ),

         "nls_exp" = list(
           Y = Ye,
           dY = Ye1,
           d2Y = Ye2
         ),

         NULL
  )
}


# linear root estimation between two points
get_root <- function(xvec, yvec, index) {
  x0 <- xvec[index]
  x1 <- xvec[index + 1]
  y0 <- yvec[index]
  y1 <- yvec[index + 1]
  x0 - y0 * (x1 - x0) / (y1 - y0)
}

# ULOQ = x_inflectionL_3
# UpperBendLine = y_inflectionL_3
# LLOQ = x_inflectionU_3
# LowerBendLine = y_inflectionU_3
# PASS in glance_fit
inflection_point <- function(data, n_points = 1000, x_min = -6, x_max = -0.0001) {
  results <- data.frame()
  if (nrow(data) < 1) {
    return(data)
  }

  x_seq <- seq(x_min, x_max, length.out = n_points)

  for(i in seq_len(nrow(data))) {
    row <- data[i, ]
    crit <- as.character(row$crit)
    fcts <- get_functions(crit)
    if (is.null(fcts)) next

    # Parameters (some models may not have g)
    a <- row$a; b <- row$b; cpar <- row$c; d <- row$d
    g <- if ("g" %in% names(row)) row$g else NA

    # Compute derivatives and original function values across x_seq
    if (crit %in% c("nlslm_4")) {
      dy_values <- fcts$dY(x_seq, a, b, cpar, d)
      d2y_values <- fcts$d2Y(x_seq, a, b, cpar, d)
      y_values <- fcts$Y(x_seq, a, b, cpar, d)

      # compute third derivative using Deriv for this model
      d3Y_fn <- Deriv(fcts$Y, "log_dilution", nderiv = 3)
      d3y_values <- d3Y_fn(x_seq, a, b, cpar, d)

    } else if (crit == "nls_4") {
      dy_values <- fcts$dY(x_seq, a, b, cpar, d)
      d2y_values <- fcts$d2Y(x_seq, a, b, cpar, d)
      y_values <- fcts$Y(x_seq, a, b, cpar, d)

      # use your supplied functions for third derivative Y43
      d3y_values <- Y43(x_seq, a, b, cpar, d)

    } else if (crit %in% c("nls_5", "drda_5")) {
      dy_values <- fcts$dY(x_seq, a, b, cpar, d, g)
      d2y_values <- fcts$d2Y(x_seq, a, b, cpar, d, g)
      y_values <- fcts$Y(x_seq, a, b, cpar, d, g)

      # Third derivative: your Y53 function
      d3y_values <- Y53(x_seq, a, b, cpar, d, g)

    } else if (crit == "nls_exp") {
      dy_values <- fcts$dY(x_seq, a, b, cpar)
      d2y_values <- fcts$d2Y(x_seq, a, b, cpar)
      y_values <- fcts$Y(x_seq, a, b, cpar)

      # Third derivative using your Ye3
      d3y_values <- Ye3(x_seq, a, b, cpar)

    } else {
      next
    }

    ## 1. Find zero crossings in the second derivative (d2y)
    signs_2 <- sign(d2y_values)
    sign_change_2 <- diff(signs_2)
    zero_crossing_indices_2 <- which(sign_change_2 != 0)
    roots_2 <- if (length(zero_crossing_indices_2) > 0) {
      sapply(zero_crossing_indices_2, function(idx) get_root(x_seq, d2y_values, idx))
    } else NA

    ## 2. Find zero crossings in the third derivative (d3y)
    signs_3 <- sign(d3y_values)
    sign_change_3 <- diff(signs_3)
    zero_crossing_indices_3 <- which(sign_change_3 != 0)
    roots_3 <- if (length(zero_crossing_indices_3) > 0) {
      sapply(zero_crossing_indices_3, function(idx) get_root(x_seq, d3y_values, idx))
    } else NA

    # 3. Pick inflection point candidate from d2y roots
    inflect_x_2 <- NA_real_
    inflect_y_2 <- NA_real_
    if (length(zero_crossing_indices_2) > 0) {
      # Select the root closest to peak first derivative (max slope)
      max_dy_pos <- x_seq[which.max(dy_values)]
      diffs_2 <- abs(roots_2 - max_dy_pos)
      idx_2 <- which.min(diffs_2)
      inflect_x_2 <- roots_2[idx_2]
      inflect_y_2 <- approx(x_seq, y_values, xout = inflect_x_2)$y
    }

    # 4. Pick inflection point candidate from d3y roots (peak/valley of second derivative)
    inflectmax_x_3 <- NA_real_
    inflectmax_y_3 <- NA_real_
    inflectmin_x_3 <- NA_real_
    inflectmin_y_3 <- NA_real_
    if (length(zero_crossing_indices_3) > 0) {
      # Use root closest to x position of max of second derivative value (peak d2y)
      max_d2y_pos <- x_seq[which.max(d2y_values)]
      diffs_3 <- abs(roots_3 - max_d2y_pos)
      idx_3 <- which.min(diffs_3)
      inflectmax_x_3 <- roots_3[idx_3]
      # Evaluate y-value at this x
      inflectmax_y_3 <- approx(x_seq, y_values, xout = inflectmax_x_3)$y

      # Use root closest to x position of min of second derivative value (valley d2y)
      min_d2y_pos <- x_seq[which.min(d2y_values)]
      diffs_3 <- abs(roots_3 - min_d2y_pos)
      idx_3 <- which.min(diffs_3)
      inflectmin_x_3 <- roots_3[idx_3]
      # Evaluate y-value at this x
      inflectmin_y_3 <- approx(x_seq, y_values, xout = inflectmin_x_3)$y
    }

    # 5. Peak of first derivative (max slope)
    max_dy_index <- which.max(dy_values)
    peak_dy_x <- x_seq[max_dy_index]
    peak_dy_y <- y_values[max_dy_index]

    # 6. Decide which inflection point to return with priority:
    #   Prefer zero crossing of second derivative (usual inflection),
    #   else zero crossing of third derivative (peak/valley of second derivative),
    #   else fallback to slope peak
    # ULOQ = x_inflectionL_3
    # UpperBendLine = y_inflectionL_3
    # LLOQ = x_inflectionU_3
    # LowerBendLine = y_inflectionU_3
    # USE
    # geom_vline(data = inflect_points[inflect_points$analyte==analyte_selected & inflect_points$plate==plate_selected, ], aes(xintercept = exp10(x_inflection)*undiluted_concentration), color = "orangered", size = 1.2) +
    #   geom_hline(data = inflect_points[inflect_points$analyte==analyte_selected & inflect_points$plate==plate_selected, ], aes(yintercept = exp10(y_inflection)), color = "orangered", size = 1.2) +
    #   geom_rect(data = inflect_points[inflect_points$analyte==analyte_selected & inflect_points$plate==plate_selected, ], aes(
    #     xmin = exp10(x_inflectionL_3) * undiluted_concentration,
    #     xmax = exp10(x_inflectionU_3) * undiluted_concentration,
    #     ymin = exp10(y_inflectionL_3),
    #     ymax = exp10(y_inflectionU_3)

    if (!is.na(inflect_x_2)) {
      chosen <- "inflection_d2_zero"
      result_row <- data.frame(
        crit = crit,
        index = i,
        x_inflection = inflect_x_2,
        y_inflection = inflect_y_2,
        x_inflectionL_3 = inflectmax_x_3,
        y_inflectionL_3 = inflectmax_y_3,
        x_inflectionU_3 = inflectmin_x_3,
        y_inflectionU_3 = inflectmin_y_3,
        x_peak_dy = peak_dy_x,
        y_peak_dy = peak_dy_y,
        used = chosen
      )
    } else if (!is.na(inflect_x_3)) {
      chosen <- "inflection_d3_zero"
      result_row <- data.frame(
        crit = crit,
        index = i,
        x_inflection = NA_real_,
        y_inflection = NA_real_,
        x_inflectionL_3 = inflectmax_x_3,
        y_inflectionL_3 = inflectmax_y_3,
        x_inflectionU_3 = inflectmin_x_3,
        y_inflectionU_3 = inflectmin_y_3,
        x_peak_dy = peak_dy_x,
        y_peak_dy = peak_dy_y,
        used = chosen
      )
    } else {
      chosen <- "peak_dy"
      result_row <- data.frame(
        crit = crit,
        index = i,
        x_inflection = NA_real_,
        y_inflection = NA_real_,
        x_inflectionL_3 = NA_real_,
        y_inflectionL_3 = NA_real_,
        x_inflectionU_3 = NA_real_,
        y_inflectionU_3 = NA_real_,
        x_peak_dy = peak_dy_x,
        y_peak_dy = peak_dy_y,
        used = chosen
      )
    }

    results <- rbind(results, result_row)
  }

  # Add temporary index to merge correctly
  data$id_tmp <- seq_len(nrow(data))
  results$id_tmp <- data$id_tmp[results$index]
  out <- left_join(data, results, by = c("crit", "id_tmp"))
  out$id_tmp <- NULL

  return(out)
}

predict_log10_dilution_fraction <- function(log10_mfi_obs, log_dilution_vals, predicted_log10_mfi_vals) {
  # Make sure inputs have at least 2 points and no NA
  valid_idx <- which(!is.na(log_dilution_vals) & !is.na(predicted_log10_mfi_vals))
  if(length(valid_idx) < 2) return(NA_real_)

  approx_x <- approx(x = predicted_log10_mfi_vals[valid_idx],
                     y = log_dilution_vals[valid_idx],
                     xout = log10_mfi_obs,
                     rule = 2)$y
  return(approx_x)
}

# Compute 5/4 Parameter models
compute_robust_curves_5_param <- function(dat, antigen, plate = "all", study_accession, experiment_accession, source, bkg, is_log_mfi_axis, g_value = 1){
  #dat_view <<- dat
  ## Deal with buffer noise
  # remove any NA mfi after subtracting in bkg method
  if (any(is.na(dat$mfi))) {
    dat<- dat[!is.na(dat$mfi), ]
  }
  # check to see if there is still standard curve data after subtracting.
  if (nrow(dat) == 0 || is.null(dat)) {
    return(NULL)
  }

  #dat <- calculate_log_dilution(dat)
  #print(names(dat))

  # check validity of g
  if (g_value <= 0){
    errorMessage <- paste("The value of g must be positive. ", g_value, " was entered")
    return(errorMessage)
  }

  # filter the source
  if (!is.null(source)) {
    dat <- dat[dat$source %in% source, ]
    if (nrow(dat) == 0){
      errorMessage <- paste("No data in source: ", source)
      return(errorMessage)
    }
  }

  # check if antigen is in the data
  if (!antigen %in% dat$antigen) {
    errorMessage <- paste("Antigen", antigen, "not found in the dataset.")
    return(errorMessage)
  }

  # filter the antigen
  if (!is.null(antigen)) {
    dat <- dat[dat$antigen %in% antigen, ]
    if(nrow(dat) == 0) {
      errorMessage <- paste("No data for antigen: ", source)
      return(errorMessage)
    }
  }

  # Check to see if plate is in the data set
  if (!plate %in% dat$plateid){
    errorMessage <- paste("Plate", plate, "not found in the dataset.")
    return(errorMessage)
  }

  # filter by plate
  if(plate != "all"){
    dat <- dat[dat$plateid %in% plate,]
    if (nrow(dat) == 0){
      errorMessagePlate <- paste("No data found on plate", plate)
      return(errorMessagePlate)
    }
  }

  #print(dat[ , c("mfi","log_dilution")])

  #lymphocytes clump together at high concentrations dragging down the mfi values is the overall dilution is not high enough. created a fix based on conversation with Project 3.

  # rename mfi
  names(dat)[names(dat) == "antibody_mfi"] <- "mfi"
  print(names(dat))

  # create new columns
  dat$orig <- dat$mfi
  dat$antigen <- NA
  dat$antigen <- antigen

  #SCALE log_dilution -- Check if this fixes convergence
  #dat$log_diluition <- scale(dat$log_dilution)

  #1 Identify the maximum mfi and its corresponding log of dilution.
  max_mfi <- max(dat$mfi)
  logd_at_max_mfi <- max(dat[dat$mfi == max_mfi, ]$log_dilution)
  # cat(max_mfi, logd_at_max_mfi)

  #2 Identify the mfis lower than the max_mfi at higher concentrations
  dat[dat$log_dilution > logd_at_max_mfi, ]$mfi <- max_mfi - ((max_mfi - dat[dat$log_dilution > logd_at_max_mfi,]$mfi) * 0.1/((dat[dat$log_dilution > logd_at_max_mfi,]$log_dilution-logd_at_max_mfi)*2))

  # print(dat[ , c("orig","mfi","log_dilution")])
  #browser()
  # 5 parameter initial parameter estimates
  cat("g = ", g_value, "\n")
  #browser()
  # If else statement for a 4Parameter vs a 5 Parameter model
  if (g_value != 1) {

    cat("SOURCE = ", source)

    # fit the 5 Parameter model
    mod_nls_5 <- compute_nls_5(dat, g_value, bkg, is_log_mfi_axis)

    # Fit alternate 5 parameter model with drda.
    #if (any(abs(rounded_g_cor) >= 0.999)) {
    if (!is.null(mod_nls_5)) {
      return(mod_nls_5)
    } else {
      cat("Fitting 5 Parameter Model DRDA")
      fit_l5 <- compute_drda_5_param(dat, bkg, is_log_mfi_axis)
      if (!is.null(fit_l5)) {
        return(fit_l5)
      } else {
        cat("Fitting 4 Parameter NLS")
        mod_nls_4 <- compute_nls_4(dat, bkg, is_log_mfi_axis)
        if (!is.null(mod_nls_4)) {
          return(mod_nls_4)
        } else {
          cat("Fitting 4 paramater power function")
          mod_power_4 <- compute_power_4(dat, bkg, is_log_mfi_axis)
          if (!is.null(mod_power_4)) {
            return(mod_power_4)
          } else {
          cat("Fitting nls exponential")
          mod_exponential <- compute_exponential_fit(dat, bkg, is_log_mfi_axis)
          if (!is.null(mod_exponential)) {
            return(mod_exponential)
          }  else {
            return(NULL)
          }
          } # end else exp3
        } # end else nls 4
      } #end else drda 5
    } # end else nls 5
  } # end if g is not 1
}

# compute exponential fit and returns model or NULL
compute_exp_3 <- function(data) {
  mod_exponential <- tryCatch({
    scal_initial <- max(data$mfi, na.rm = T)
    xmid_initial <- 0.1
    l_asy_initial <- min(data$mfi, na.rm = T)
    if (l_asy_initial < 0) {
      l_asy_initial <- 0
    }

    minpack.lm::nlsLM(mfi ~ l_asy + (scal - l_asy) * exp(xmid * log_dilution),
                      data = data,
                      start = list(scal = scal_initial, xmid = xmid_initial, l_asy = l_asy_initial),
                      lower = c(-Inf, -Inf, 0),  # Set lower bound for l_asy to 0
                      control = minpack.lm::nls.lm.control(maxiter = 500, ftol = 1e-9, ptol = 1e-6))
  }, error = function(e) {
    print(paste("Exponential Model Failed:", e))
    return(NULL)
  })

  return(mod_exponential)
}

# Produce fit table from exponential model. Uses compute_exp_3
compute_exponential_fit <- function(data, bkg, is_log_mfi_axis) {
  mod_exponential <- compute_exp_3(data)

  if(!is.null(mod_exponential)) {
    # Extract coefficients from the fitted model
    parameters <- coef(mod_exponential)
    model_summary <- summary(mod_exponential)
    coefficient <- model_summary$coefficients


    # Create a data frame for the parameters
    fit_table <- data.frame(term = rownames(coefficient),
                            estimate = coefficient[,"Estimate"],
                            std_error = coefficient[,"Std. Error"],
                            p_value = coefficient[, "Pr(>|t|)"])

    l_asy_p_value <- fit_table["l_asy",]$p_value
    if (l_asy_p_value > 0.05) {
      cat("p-value > 0.05")
      return(NULL)
    }

    fit_table$signif <- stars.pval(c(as.numeric(fit_table$p_value)))
    fit_table$study_accession <- unique(data$study_accession)
    fit_table$experiment_accession <- unique(data$experiment_accession)
    fit_table$plateid <- unique(data$plateid)
    fit_table$antigen <- unique(data$antigen)
    fit_table$source <- unique(data$source)

    # Create Glance
    glance_fit_exp <- mod_exponential[2]
    glance_fit_exp <- as.data.frame(glance_fit_exp)


    print("Glance Fit Tab Names")
    print(names(glance_fit_exp))
    if (identical(names(glance_fit_exp), c("convInfo.isConv","convInfo.finIter","convInfo.finTol","convInfo.stopCode","convInfo.stopMessage"))) {
      names(glance_fit_exp)[names(glance_fit_exp) == "convInfo.finIter"] <- "iter"
      names(glance_fit_exp)[names(glance_fit_exp) == "convInfo.stopMessage"] <- "status"
      glance_fit_exp <- glance_fit_exp[, c("iter", "status")]
    }
    glance_fit_stat <- broom::glance(mod_exponential)[c(4:9)]
    names(glance_fit_stat)[names(glance_fit_stat) == 'AIC'] <- 'aic'
    names(glance_fit_stat)[names(glance_fit_stat) == 'BIC'] <- 'bic'
    names(glance_fit_stat)[names(glance_fit_stat) == 'df.residual'] <- 'dfresidual'
    names(glance_fit_stat)[names(glance_fit_stat) == 'logLik'] <- 'loglik'

    # account for constrained at 0
    if (fit_table["l_asy", "estimate"] == 0) {
      cat("Estimate of l_asy is 0")

      predictions <- predict(mod_exponential, newdata = seq(-10, 0, length.out = 100), interval = "confidence")
      threshold <- 0.005
      # the first one above the threshold
      llod_index <- which(predictions > threshold)[1]
      # concentration at index
      llod_concentration <- seq(-10, 0, length.out = 100)[llod_index]
      llod <- predictions[llod_index]
      if (is_log_mfi_axis) {
        llod <- round(llod, 3)
      } else {
        llod <- round(llod, 0)
      }
    } else {
      predictions <- predict_nls(mod_exponential, newdata = data.frame(log_dilution = -10), interval = "confidence")
      print(predictions)
      if (is_log_mfi_axis) {
        llod <- round(predictions[4],3)
      } else {
        llod <- round(predictions[4],0)
      }
    }

    rsquare_fit <- round(modelr::rsquare(mod_exponential,augment(mod_exponential)),3)
    # calculate mse
    mse <- mean(resid(mod_exponential)^2, na.rm = T)
    # mean of observed mfi
    mean_obs_mfi<- mean(data$mfi, na.rm = TRUE)
    # coefficient of variation.
    cv <- (sqrt(mse) / mean_obs_mfi) * 100

    # CV for log dilution
    # sd_log_dilution <- sd(data$log_dilution, na.rm = TRUE)
    # mean_log_dilution <- mean(data$log_dilution, na.rm = TRUE)
    # cv_log_dilution <- (sd_log_dilution / mean_log_dilution) * 100



    glance_fit_exp$study_accession <- unique(fit_table$study_accession)
    glance_fit_exp$experiment_accession <- unique(fit_table$experiment_accession)
    glance_fit_exp$plateid <- unique(fit_table$plateid)
    glance_fit_exp$antigen <- unique(fit_table$antigen)
    glance_fit_exp$source <- unique(fit_table$source)
    glance_fit_exp$scale <- as.data.frame(t(fit_table))["estimate", "scal"]
    glance_fit_exp$x_mid <- as.data.frame(t(fit_table))["estimate", "xmid"]
    glance_fit_exp$l_asy <- as.data.frame(t(fit_table))["estimate", "l_asy"]
    glance_fit_exp$crit <- "nls_exp"

    glance_fit_exp <- cbind(glance_fit_exp, glance_fit_stat,rsquare_fit, mse, cv, llod)
    glance_fit_exp

    # Limits of quantification
    loq_der <- loq_derivitives(fit = mod_exponential, glance_fit = glance_fit_exp)
    glance_fit_exp$lloq <- loq_der$lloq
    glance_fit_exp$uloq <- loq_der$uloq
    glance_fit_exp$loq_method <- loq_der$method

    # background/blank method
    glance_fit_exp$bkg_method <- bkg
    glance_fit_exp$is_log_mfi_axis <- is_log_mfi_axis

    glance_fit_exp$formula <- "Y ~ a + (b - a) *e^(c*X)"
    newlist <- list(fit_table, glance_fit_exp, mod_exponential)
    return(newlist)
  } else {
    return(NULL)
  }

}

# Compute robust SE for DRDA model.
compute_robust_se <- function(data, mod){
  # Calculate Gradients
  x <- data$log_dilution
  theta <- coef(mod)

  gradients <- logistic5_gradient(x = x, theta = theta)
  #gradients <- apply(gradients, 2, function(col) smooth.spline(1:length(col), col)$y)
  #gradients <- apply(gradients, 2, function(col) predict(loess(col ~ seq_along(col), span = 1)))


  residuals <- mod$residuals
  #X'*diag*X

  m <- t(gradients) %*% diag(residuals^2) %*% gradients
  #m <- tcrossprod(residuals^2 * gradients, gradients)

  #calculate the robust covariance matrix (sandwich estimator)
  #X'X
  #b <- MASS::ginv(t(gradients) %*% gradients)
  hessian_matrix <- t(gradients) %*% gradients
  # if (kappa(hessian_matrix) > 1) {
  #   cat("Condition number > 1. Preforming regularization")
  #cond_number <- kappa(hessian_matrix)
  # if (kappa(hessian_matrix) > 10^6) {
  #   lambda <- 1 / kappa(hessian_matrix)  # Adaptive regularization
  #   hessian_matrix <<- hessian_matrix + lambda * diag(ncol(hessian_matrix))
  # }
  #   lambda <- 1e-6
  #   hessian_matrix <- hessian_matrix + lambda * diag(ncol(hessian_matrix))
  # }
  b <- tryCatch(
    chol2inv(chol(hessian_matrix)),
    error = function(e) MASS::ginv(hessian_matrix)
  )

  robust_cov <- b %*% m %*% b

  diag_values <- diag(robust_cov)
  # if negative diagonals, switch to nls 4
  if (any(diag_values < 0)) {
    return(NULL)
  }
  # Calculate SE of parameters
  robust_se <- sqrt(diag(robust_cov))
  # add NA for sigma
  robust_se <- c(robust_se, NA)

  newlist <- list(robust_cov, robust_se)
  return(newlist)
}

# Calculates and returns LLOD for DRDA model.
calculate_llod_drda <- function(model, is_log_mfi_axis, data_plot) {
  theta <- coef(model)
  log_dil_value <- -10
  new_data <- data.frame(log_dil_value = log_dil_value)
  gradient <- logistic5_gradient(log_dil_value, theta = theta)

  robust_cov <- compute_robust_se(data_plot, model)[[1]]
  prediction <- predict(model, newdata = new_data)
  prediction_var <- gradient %*% robust_cov %*% t(gradient)

  # calculate SE
  prediction_se <- sqrt(prediction_var)

  # get left asymptote
  l_asy <- as.numeric(model$coefficients["alpha"])
  # calculate LLOD
  if (is_log_mfi_axis) {
    llod <- as.numeric(round(l_asy + 1.96*prediction_se,3))
  } else {
    llod <- as.numeric(round(l_asy + 1.96*prediction_se,0))
  }
  return(llod)
}

# Calculates and returns ULOD for DRDA model.
calculate_ulod_drda <- function(model, is_log_mfi_axis, data_plot){
  theta <- coef(model)
  log_dil_value <- -0.001
  new_data <- data.frame(log_dil_value = log_dil_value)
  gradient <- logistic5_gradient(log_dil_value, theta = theta)


  robust_cov <- compute_robust_se(data_plot, model)[[1]]
  prediction <- predict(model, newdata = new_data)
  prediction_var <- gradient %*% robust_cov %*% t(gradient)

  prediction_se <- sqrt(prediction_var)

  # calculate r_asy
  r_asy <- as.numeric(model$coefficients["alpha"]) + as.numeric(model$coefficients["delta"])
  if (is_log_mfi_axis) {
    ulod <- as.numeric(round(r_asy - 1.96* prediction_se, 3))
  } else {
    ulod <- as.numeric(round(r_asy - 1.96* prediction_se, 0))
  }
  return(ulod)
}

# calculates bend lines for DRDA model.
calculate_bend_lines_drda <- function(model, is_log_mfi_axis) {
  # Coefficients
  coef_a <- as.numeric(model$coefficients["alpha"]) # left asymptote

  coef_d <- as.numeric(model$coefficients["alpha"]) + as.numeric(model$coefficients["delta"]) # right asymptote
  coef_k <- 4.6805

  if (is_log_mfi_axis) {
    bendlower <- round(((coef_a - coef_d) / (1 + (1/coef_k))) + coef_d,3)
    bendupper <- round(((coef_a - coef_d) / (1 + coef_k)) + coef_d,3)
  } else {
    bendlower <- round(((coef_a - coef_d) / (1 + (1/coef_k))) + coef_d,0)
    bendupper <- round(((coef_a - coef_d) / (1 + coef_k)) + coef_d,0)
  }

  bend_lines <- c(bendlower, bendupper)
  return(bend_lines)
}

# Computes DRDA model with constraints of lower bound l_asy = 0 and returns tables of fit
compute_drda_5_param <- function(data, bkg, is_log_mfi_axis ) {
  fit_l5 <- tryCatch({
    # Constrain the model alpha/l_asy by 0 min
    lb <- c(0, -Inf, -Inf, -Inf, -Inf)
    ub <- c(Inf, Inf, Inf,  Inf, Inf)
    drda::drda(mfi ~ log_dilution, data = data, mean_function = "logistic5",
               lower_bound = lb,
               upper_bound = ub)
  }, error = function(e){
    print("DRDA 5 Parameter failed. Returning NULL")
    return(NULL)
  })

  if (!is.null(fit_l5)){
    sigma_est <- as.data.frame(summary(fit_l5)$param)["sigma", "Estimate"]
    range_mfi <- range(data$mfi, na.rm = TRUE)
    max_mfi <- max(data$mfi, na.rm = TRUE)
    rse_ratio <- abs(sigma_est) / (range_mfi[2] - range_mfi[1])
    cat("RSE Ratio:")
    print(rse_ratio)
    if (rse_ratio >= 0.03) {
      cat("RSE ratio >= 3% switch to 4 parameters")
      return(NULL)
    }

    # extract coefficients from the fitted model
    #parameters <- coef(fit_l5)
    model_summary <- summary(fit_l5)$param
    # check to see if it is an exponential like curve and if so return NULL
    if (max_mfi < 0.25 * (as.numeric(model_summary["alpha", "Estimate"]) + as.numeric(model_summary["delta", "Estimate"]))) {
      return(NULL)
    }

    # r_asy_val <- model_summary["alpha", "Estimate"] + model_summary["delta", "Estimate"]
    # r_asy<- c(r_asy_val, NA, NA, NA)
    # rbind(model_summary, r_asy)
    #
    coefficient <- model_summary[,"Estimate"]

    fit_table <- data.frame(term = rownames(model_summary),
                            estimate = coefficient)
    # Compute Robust SEs
    na_in_std_error <- any(is.na(model_summary[, "Std. Error"]))
    if (na_in_std_error){
      robust_se <- compute_robust_se(data, fit_l5)
      # switch to nls 4 parameter model
      if (is.null(robust_se)) {
        return(NULL)
      } else {
        fit_table$std_error <- compute_robust_se(data, fit_l5)[[2]]
      }
    } else{
      fit_table$std_error <- model_summary[, "Std. Error"]
    }

    # Compute Right Asymptote and SE
    r_asy_val <- model_summary["alpha", "Estimate"] + model_summary["delta", "Estimate"]
    SE_r_asy <- sqrt(fit_table["alpha","std_error"]^2 + fit_table["delta", "std_error"]^2)

    fit_table_row <- data.frame(term = "r_asy", estimate = r_asy_val, std_error = SE_r_asy)
    fit_table <- rbind(fit_table, fit_table_row)
    #rbind(model_summary, r_asy)

    # Compute P-values
    df <- nrow(data) - 5
    fit_table_clean <- na.omit(fit_table[, c("term","estimate", "std_error")])
    t_stats <- fit_table_clean$estimate / fit_table_clean$std_error
    p_values <- 2 * pt(-abs(t_stats), df = df)

    fit_table_clean$p_value <- p_values
    fit_table_clean$signif <- stars.pval(c(as.numeric(fit_table_clean$p_value)))
    fit_table_clean$study_accession <- unique(data$study_accession)
    fit_table_clean$experiment_accession <- unique(data$experiment_accession)
    fit_table_clean$antigen <- unique(data$antigen)
    fit_table_clean$plateid <- unique(data$plateid)

    fit_table_clean <- fit_table_clean[, c("study_accession", "experiment_accession","plateid", "antigen",
                                           "term", "estimate", "std_error", "p_value", "signif")]




    ### Produce Glance Fit
    glance_fit <-  data.frame(study_accession = unique(fit_table_clean$study_accession),
                              experiment_accession = unique(fit_table_clean$experiment_accession),
                              plateid = unique(fit_table_clean$plateid),
                              antigen = unique(fit_table_clean$antigen),
                              iter = fit_l5$iterations,
                              status = ifelse(fit_l5$converged, "converged", "not_converged"),
                              alpha = fit_table_clean$estimate[fit_table_clean$term == "alpha"],
                              delta = fit_table_clean$estimate[fit_table_clean$term == "delta"],
                              eta = fit_table_clean$estimate[fit_table_clean$term == "eta"],
                              phi = fit_table_clean$estimate[fit_table_clean$term == "phi"],
                              nu = fit_table_clean$estimate[fit_table_clean$term == "nu"],
                              r_asy = fit_table_clean$estimate[fit_table_clean$term == "r_asy"],
                              bendlower = calculate_bend_lines_drda(fit_l5, is_log_mfi_axis)[1],
                              bendupper = calculate_bend_lines_drda(fit_l5, is_log_mfi_axis)[2],
                              llod = calculate_llod_drda(fit_l5, is_log_mfi_axis, data_plot = data),
                              ulod = calculate_ulod_drda(fit_l5, is_log_mfi_axis, data_plot = data),
                              loglik = fit_l5$loglik,
                              dfresidual = fit_l5$df.residual,
                              nobs = fit_l5$n,
                              rsquare_fit =  1 - fit_l5$rss/ sum((data$mfi - mean(data$mfi))^2),
                              mse = as.numeric(fit_l5$rss) / (as.numeric(fit_l5$n) - 5),
                              source = unique(data$source),
                              crit = "drda_5"
    )

    # Coefficient of variation.
    glance_fit$cv <- as.numeric((sqrt(as.numeric(glance_fit$mse))/mean(data$mfi, na.rm = TRUE)) * 100)

    # Limits of Quantification
    loq_der <- loq_derivitives(fit = fit_l5, glance_fit = glance_fit)
    glance_fit$lloq <- loq_der$lloq
    glance_fit$uloq <- loq_der$uloq
    glance_fit$loq_method <- loq_der$method

    # Blank Method
    glance_fit$bkg_method <- bkg
    glance_fit$is_log_mfi_axis <- is_log_mfi_axis

    glance_fit$formula <- "Y ~ d + ((a-d)/(1 + exp((X-c)/b))^g"


    # sd_log_dilution <- sd(data$log_dilution, na.rm = TRUE)
    # mean_log_dilution <- mean(data$log_dilution, na.rm = TRUE)
    # glance_fit$cv_log_dilution <- (sd_log_dilution / mean_log_dilution) * 100

    newlist <- list(fit_table_clean, glance_fit, fit_l5)
    return(newlist)
  }
  else {
    return(NULL)
  }
}


# Helper function for plot 5 parameter logistic regression with formula
five_param_logistic <- function(log_dilution, l_asy, r_asy, xmid, scal, g) {
  r_asy + (l_asy - r_asy) / (1 + exp((log_dilution - xmid) / scal))^g
}

# Helper function for exponential fit formula
exponential_fit <- function(log_dilution, scal, xmid, l_asy) {
  l_asy + (scal - l_asy) * exp(xmid * log_dilution)
}

obtain_fitted_curve <- function() {

}

# Main Plotting code. Handles different model fits and fitted values and AUs.
plot_curve_plotly <- function(model_list, dat, source_filter, antigen, plate, sample_data_au, is_log_mfi_axis){

  # Calculate dilution factor to display in dilution series trace.
  dat$dilution_factor_denominator <- exp10(dat$log_dilution)
  frac_parts <- frac_mat(dat$dilution_factor_denominator)
  dat$dilution_factor_denominator <- frac_parts["denominator",]

  model_list <- model_list

  mfi_text <- if (is_log_mfi_axis) {
    "log<sub>10</sub> MFI"
  } else {
    "MFI"
  }

  # check if there is no model and return plot of dilution series if true
  if (is.null(model_list) || is.null(sample_data_au)) {
    p <- plot_ly()
    p <- p %>% add_trace(data = dat,
                         x = ~log_dilution,
                         y = ~mfi,
                         type = "scatter",
                         mode = "markers",
                         name = "Dilution Series",
                         color = ~stype,
                         colors = c("B" = "#1f77b4", "S" = "black"),
                         #marker = list(color = ~ifelse(stype == "S", "black" , "#1f77b4")),
                       #  color = ~ifelse(stype == "B", "black" , "#1f77b4"),
                         text = ~paste0(
                           "<br>Log Dilution: ", dat$log_dilution,
                           "<br> Dilution Factor: ", dat$dilution_factor_denominator,
                           "<br>", mfi_text,":", dat$mfi
                         ),
                         hoverinfo = "text")%>%
      layout(title  = paste("Dilution Series for", antigen),
             xaxis = list(title = "Dilution log<sub>10</sub>"),
             yaxis = list(title = mfi_text),
             font = list(size = 12))


    return(p)
  } else {
    x_values <- seq(min(dat$log_dilution), 0 , length.out = 100)

    if (model_list[[2]]$crit == "nls_5") {
      cat("In plot: Fitting nls 5 model")
      r_asy <- as.numeric(model_list[[2]]$r_asy)
      l_asy <- as.numeric(model_list[[2]]$l_asy)
      xmid <- as.numeric(model_list[[2]]$x_mid)
      scal <- as.numeric(model_list[[2]]$scale)
      g <- as.numeric(model_list[[2]]$g)

      bend_lower <- as.numeric(model_list[[2]]$bendlower)
      bend_upper <- as.numeric(model_list[[2]]$bendupper)
      ulod <- as.numeric(model_list[[2]]$ulod)
      llod <- as.numeric(model_list[[2]]$llod)
      # limit of quantification
      lloq <- as.numeric(model_list[[2]]$lloq)
      uloq <- as.numeric(model_list[[2]]$uloq)
      # g is not NA
      fitted_y <- five_param_logistic(x_values, l_asy, r_asy, xmid, scal, g)
      model_type <-"5-Parameter"
    } else if (model_list[[2]]$crit == "drda_5") {
      cat("In plot: Fitting drda 5 model")
      # Obtain the DRDA model statistics if it is a drda model
      r_asy <- as.numeric(model_list[[2]]$r_asy)
      alpha <-  as.numeric(model_list[[2]]$alpha)
      delta <- as.numeric(model_list[[2]]$delta)
      eta <- as.numeric(model_list[[2]]$eta)
      phi <-  as.numeric(model_list[[2]]$phi)
      nu <- as.numeric(model_list[[2]]$nu)

      cat("getting values from drda fit")
      ulod <- as.numeric(model_list[[2]]$ulod)
      llod <- as.numeric(model_list[[2]]$llod)
      bend_lower <- as.numeric(model_list[[2]]$bendlower)
      bend_upper <- as.numeric(model_list[[2]]$bendupper)
      # limit of quantification
      lloq <- as.numeric(model_list[[2]]$lloq)
      uloq <- as.numeric(model_list[[2]]$uloq)

      predictions <- predict(model_list[[3]], newdata = data.frame(log_dilution = x_values)) #p5_mod[3][[1]]
      theta <- coef(model_list[[3]])
      fitted_y <- logistic5_fn(x_values, theta)
      model_type <- "5-Parameter"
    } else if (model_list[[2]]$crit == "nls_4") {
      cat("In plot: Fitting nls 4")
      l_asy <- as.numeric(model_list[[2]]$l_asy)
      r_asy <- as.numeric(model_list[[2]]$r_asy)
      xmid <- as.numeric(model_list[[2]]$x_mid)
      scal <- as.numeric(model_list[[2]]$scale)
      bend_lower <- as.numeric(model_list[[2]]$bendlower)
      bend_upper <- as.numeric(model_list[[2]]$bendupper)
      ulod <- as.numeric(model_list[[2]]$ulod)
      llod <- as.numeric(model_list[[2]]$llod)
      # limit of quantification
      lloq <- as.numeric(model_list[[2]]$lloq)
      uloq <- as.numeric(model_list[[2]]$uloq)

      # g is NA in 4 parameter form so we set to 1 (NO Asymmetry)
      g <- 1
      # Calculate the fitted y values
      fitted_y <- five_param_logistic(x_values, l_asy, r_asy, xmid, scal, g)
      model_type <-"4-Parameter"
    } else if (model_list[[2]]$crit == "nlslm_4") {
      l_asy <- as.numeric(model_list[[2]]$l_asy)
      r_asy <- as.numeric(model_list[[2]]$r_asy)
      xmid <- as.numeric(model_list[[2]]$x_mid)
      scal <- as.numeric(model_list[[2]]$scale)
      bend_lower <- as.numeric(model_list[[2]]$bendlower)
      bend_upper <- as.numeric(model_list[[2]]$bendupper)
      ulod <- as.numeric(model_list[[2]]$ulod)
      llod <- as.numeric(model_list[[2]]$llod)
      # limit of quantification
      lloq <- as.numeric(model_list[[2]]$lloq)
      uloq <- as.numeric(model_list[[2]]$uloq)

      g <- 1
      # Calculate the fitted y values
      fitted_y <- Ylm4(x_values,l_asy,scal,xmid,r_asy)
      model_type <- "4-Paramater Power"

    } else if (model_list[[2]]$crit == "nls_exp") {
      cat("In plot: Fitting nls exponential model")
      l_asy <- as.numeric(model_list[[2]]$l_asy)
      scal <- as.numeric(model_list[[2]]$scale)
      xmid <- as.numeric(model_list[[2]]$x_mid)
      llod <- as.numeric(model_list[[2]]$llod)

      fitted_y <- exponential_fit(x_values, scal, xmid, l_asy)
      model_type <- "Exponential"
    } else {
      fitted_y <- NULL
    }

    # Printing values to the console.
    if (model_list[[2]]$crit != "drda_5") {
      cat("l_asy in plot")
      print(l_asy)
    }

    if (model_list[[2]]$crit != "nls_exp") {
      cat("r_asy in plot")
      print(r_asy)
    }
    if (model_list[[2]]$crit != "drda_5") {
      cat("xmid in plot")
      print(xmid)
    }

    if (model_list[[2]]$crit != "drda_5") {
      cat("scal in plot")
      print(scal)
    }

    if (model_list[[2]]$crit == "nls_4" || model_list[[2]]$crit == "nls_5") {
      cat("g in plot")
      print(g)
    }


    plot_data <- dat
    filtered_sample_data_au <- sample_data_au

    if (is.null(fitted_y)) {
      p <- plot_ly()

      p <- p %>% add_trace(data = plot_data,
                           x = ~log_dilution,
                           y = ~mfi,
                           type = "scatter",
                           mode = "markers",
                           name = ~paste(ifelse(stype == "S", "Standard",
                                          ifelse(stype == "B", "Buffer", stype)), "Dilution Series"),
                           color = ~stype,
                           colors = c("B" = "#1f77b4", "S" = "black"),
                          # marker = list(color = ~ifelse(stype == "S", "black" , "#1f77b4")),
                          # color = ~ifelse(stype == "B", "black" , "#1f77b4"),
                           text = ~paste0(
                             "<br>Log Dilution: ", plot_data$log_dilution,
                             "<br> Dilution Factor: ", plot_data$dilution_factor_denominator,
                             "<br>",mfi_text,":", plot_data$mfi
                           ),
                           hoverinfo = "text")
    } else if (model_type == "Exponential") {
      p <- plot_ly()
      # p <- plot_ly(data = plot_data,
      p <- p %>% add_trace(data = plot_data,
                           x = ~log_dilution,
                           y = ~mfi,
                           type = "scatter",
                           mode = "markers",
                           name = ~paste(ifelse(stype == "S", "Standard",
                                                ifelse(stype == "B", "Buffer", stype)), "Dilution Series"),
                           color = ~stype,
                           colors = c("B" = "#1f77b4", "S" = "black"),
                          # marker = list(color = ~ifelse(stype == "S", "black" , "#1f77b4")),
                           text = ~paste0(
                             "<br>Log Dilution: ", plot_data$log_dilution,
                             "<br>Dilution Factor: ", plot_data$dilution_factor_denominator,
                             "<br>",mfi_text,":", plot_data$mfi
                           ),
                           hoverinfo = "text") %>%
        add_lines(x = x_values, y = fitted_y, name = 'Fitted Curve', line = list(color = 'blue')) %>%
        # Limits of Detection
        add_lines(x = x_values, y = llod, name = paste("Lower LOD:", llod), line = list(color = "orangered", dash = "dash")) %>%
        add_trace(data = filtered_sample_data_au,
                  x = ~antibody_au,
                  y = ~antibody_mfi,
                  type = "scatter",
                  mode = "markers",
                  name = "Predicted Dilution",
                  marker = list(color = 'green', symbol = 'circle'),
                 # visible= "legendonly",
                  text = ~paste("Predicted Dilution: ", antibody_au, "<br>", mfi_text, ":", antibody_mfi,
                                "<br>Subject:", subject_accession),
                                #"<br>Timeperiod:", timeperiod),
                  hovertemplate = "%{text}<extra></extra>") %>%
        # Axis Labels
        layout(
          title  = paste("Fitted", model_type, "Model"),#\nAntigen:", antigen, "Source:", source_filter, "Plate:", plate),
          xaxis = list(title = "Dilution log<sub>10</sub>"),
          yaxis = list(title = mfi_text),
          font = list(size = 12)
        )
    } else {
      p <- plot_ly()
      # p <- plot_ly(data = plot_data,
      p <- p %>% add_trace(data = plot_data,
                           x = ~log_dilution,
                           y = ~mfi,
                           type = "scatter",
                           mode = "markers",
                           name = ~paste(ifelse(stype == "S", "Standard",
                                                ifelse(stype == "B", "Buffer", stype)), "Dilution Series"),
                           color = ~stype,
                           colors = c("B" = "#1f77b4", "S" = "black"),
                           #marker = list(color = ~ifelse(stype == "S", "black" , "#1f77b4")),
                          # color = ~ifelse(stype == "B", "black" , "#1f77b4"),
                           text = ~paste0(
                             "<br>Log Dilution: ", plot_data$log_dilution,
                             "<br>Dilution Factor: ", plot_data$dilution_factor_denominator,
                             "<br>",mfi_text, ":", plot_data$mfi
                           ),
                           hoverinfo = "text") %>%
        add_lines(x = x_values, y = fitted_y, name = 'Fitted Curve', line = list(color = 'blue')) %>%
        # Limits of Detection
        add_lines(x = x_values, y = ulod, name = paste("Upper LOD:", ulod), line = list(color = "orangered", dash = "dash"))%>%
        add_lines(x = x_values, y = llod, name = paste("Lower LOD:", llod), line = list(color = "orangered", dash = "dash")) %>%
        # bend lines
        add_lines(x = x_values, y = bend_upper, name = paste("Upper Bend", bend_upper), line = list(color = "purple")) %>%
        add_lines(x = x_values, y = bend_lower, name = paste("Lower Bend", bend_lower), line = list(color = "purple"))
        # LOQs
        if (!is.na(lloq)) {
          if (is_log_mfi_axis) {
            p <- p %>%
              add_lines(x = c(lloq), y = c(min(plot_data$mfi, na.rm = T), max(plot_data$mfi, na.rm = T)),
                        name = paste("Lower LOQ", round(lloq, 3)),
                        line = list(color = "#f3c300"),
                        hoverinfo = "text",
                        hovertext = paste("Lower Limit of Quantification (LLOQ):", round(lloq,2)))
          } else {
            p <- p %>%
              add_lines(x = c(lloq), y = c(0, max(plot_data$mfi, na.rm = T)),
                        name = paste("Lower LOQ", round(lloq, 2)),
                        line = list(color = "#f3c300"),
                        hoverinfo = "text",
                        hovertext = paste("Lower Limit of Quantification (LLOQ):", round(lloq,2)))
          }
         }

          if (!is.na(uloq)) {
            if (is_log_mfi_axis) {
            p <- p %>%
              add_lines(x = c(uloq), y = c(min(plot_data$mfi, na.rm = T), max(plot_data$mfi, na.rm = T)),
                        name = paste("Upper LOQ", round(uloq, 3)),
                        line = list(color = "#f3c300"),
                        hoverinfo = "text",
                        hovertext = paste("Upper Limit of Quantification (ULOQ):", round(uloq,2)))
            } else {
              p <- p %>%
                add_lines(x = c(uloq), y = c(0, max(plot_data$mfi, na.rm = T)),
                          name = paste("Upper LOQ", round(uloq,2)),
                          line = list(color = "#f3c300"),
                          hoverinfo = "text",
                          hovertext = paste("Upper Limit of Quantification (ULOQ):", round(uloq,2)))
            }
          }
      # if (!is.na(lloq)) {
      #   p <- p %>%
      #     add_segments(x = lloq, xend = lloq, y = 0, yend = max(plot_data$mfi),
      #                  name = paste("LLOQ", round(lloq,2)),
      #                  line = list(color = "#882d17"),
      #                  hoverinfo = "text",
      #                  hovertext = paste("Lower Limit of Quantification (LLOQ):", lloq))
      # }
      # if (!is.na(uloq)) {
      #   p <- p %>%
      #     add_segments(x = uloq, xend = uloq, y = 0, yend = max(plot_data$mfi),
      #                  name = paste("ULOQ", round(uloq, 2)),
      #                  line = list(color = "#882d17"),
      #                  hoverinfo = "text",
      #                  hovertext = paste("Upper Limit of Quantification (ULOQ):", uloq))
      # }

        p <- p %>% add_trace(data = filtered_sample_data_au,
                  x = ~antibody_au,
                  y = ~antibody_mfi,
                  type = "scatter",
                  mode = "markers",
                  name = "Predicted Dilution",
                  marker = list(color = 'green', symbol = 'circle'),
                #  visible = "legendonly",
                  text = ~paste("Predicted Dilution: ", antibody_au, "<br>", mfi_text, ":", antibody_mfi, # was Antibody MFI
                                "<br>Subject:", subject_accession,
                                "<br>In Linear Region:", in_linear_region,
                                "<br>In Quantifiable Range:",in_quantifiable_range),
                                 #"<br>Timeperiod:", timeperiod),
                  hovertemplate = "%{text}<extra></extra>") %>%
        # Axis Labels
        layout(
          title  = paste("Fitted", model_type, "Logistic Model"),#\nAntigen:", antigen, "Source:", source_filter, "Plate:", plate),
          xaxis = list(title = "Dilution log<sub>10</sub>"),
          yaxis = list(title = mfi_text),
          font = list(size = 12)
        )
    }

    return(p)
  }

}

# Plot residuals if intrested in residuals of model. Takes in model and data
plot_residuals <- function(model, data) {
  # Ensure the MFI values are numeric
  data$mfi <- as.numeric(data$mfi)

  # Calculate fitted values using the model
  data$fitted_values <- predict(model)

  # Calculate residuals
  data$residuals <- data$mfi - data$fitted_values

  # Create the plot
  plot <- plot_ly(data) %>%
    add_trace(x = ~fitted_values, y = ~residuals, type = "scatter", mode = "markers",
              name = "Residuals vs. Fitted",
              marker = list(color = "red")) %>%
    add_trace(x = ~fitted_values, y = ~rep(0, nrow(data)), type = "scatter", mode = "lines",
              line = list(color = "blue", dash = "dash"), name = "Zero Line") %>%
    layout(
      title = "Residuals vs. Fitted Values",
      xaxis = list(title = "Fitted Values (MFI)"),
      yaxis = list(title = "Residuals"),
      showlegend = TRUE
    )

  return(plot)
}

# Compute gate class and Linear region gating. Account for exponential model. Takes in our model object and sample data
compute_gate_class <- function(model_list, sample_data) {
#sample_data_gc <<- sample_data

 model_test <- model_list
  # if (is.null(model_list)) {
  #   cat("model list is null")
  #   return(NULL)
  # }
  # if (is.null(sample_data)) {
  #   cat("sample data is null")
  #   return(NULL)
  # }

  model_list_df <- model_list[[2]]
  cat("Names of sample data in compute gate class")
  print(names(sample_data))
  print(head(sample_data))

  cat("Model List df")
  print(model_list_df)
  ulod <- model_list[[2]]$ulod
  llod <- model_list[[2]]$llod
  bendlower <- model_list[[2]]$bendlower
  bendupper <- model_list[[2]]$bendupper

  cat("ULOD from gate class")
  print(ulod)
  cat("LLOD from gate class")
  print(llod)
  cat("bendlower from gate class")
  print(bendlower)
  cat("bendupper from gate class")
  print(bendupper)


  # Account for exponential model
  if(model_list_df$crit == "nls_exp") {
    cat("In exponential merge")
    sample_data_mod <- merge(sample_data, model_list_df[, c("plateid", "antigen", "llod")],
                             by = c("plateid", "antigen"), all.y = T)

    cat("after exponential merge")
    sample_data_mod$gc <- ifelse(sample_data_mod$antibody_mfi < sample_data_mod$llod & !is.null(sample_data_mod$llod), "Too Diluted",
                                 "Acceptable")
    # if exponential model then there are no bend lines.
    sample_data_mod$in_linear_region <- ifelse(sample_data_mod$gc == "Acceptable", TRUE, FALSE)
    sample_data_mod$gate_class_linear_region <- sample_data_mod$gc

  } else {
    sample_data_mod <- merge(sample_data, model_list_df[, c("plateid", "antigen", "llod", "ulod")],
                             by = c("plateid", "antigen"), all.y = T)

   # sample_data_mod_view <<- sample_data_mod


    cat("after merge non exponential")

    # value reported is the mfi
    sample_data_mod$gc <- ifelse(sample_data_mod$antibody_mfi < sample_data_mod$llod & !is.null(sample_data_mod$llod), "Too Diluted",
                                 ifelse(sample_data_mod$antibody_mfi > sample_data_mod$ulod & !is.null(sample_data_mod$ulod),  "Too Concentrated",
                                        "Acceptable" ))
    # compute in linear region gating.
    sample_data_mod$in_linear_region <- ifelse(sample_data_mod$antibody_mfi >= model_list_df$bendlower & sample_data_mod$antibody_mfi <=  model_list_df$bendupper,
                                                                                                T, F)


    # sample_data_mod$gate_class_linear_region <- ifelse(sample_data_mod$antibody_mfi < model_list_df$bendlower, "Too Diluted",
    #                                            ifelse(sample_data_mod$antibody_mfi > model_list_df$bendupper, "Too Concentrated", "Acceptable"))

    sample_data_mod$gate_class_linear_region <- ifelse(sample_data_mod$in_linear_region, "Acceptable",
                                                       ifelse(sample_data_mod$antibody_mfi < model_list_df$bendlower,
                                                              "Too Diluted", "Too Concentrated"))

  }

 # sample_data_mod$antibody_mfi <- sample_data_mod$value_reported

  #sample_data_mod_v <<- sample_data_mod
  return(sample_data_mod)
}

  ## Limit of Quantification gating. Use model and AUs that are calculated
  compute_loq_gate_class <- function(model_list, sample_data_au) {
    uloq <- model_list[[2]]$uloq
    lloq <- model_list[[2]]$lloq

    cat("LLOQ for gating")
    print(lloq)
    cat("ULOQ for gating")
    print(uloq)


    if(model_list[[2]]$crit == "nls_exp") {
      sample_data_mod <- merge(sample_data_au, model_list[[2]][, c("plateid", "antigen", "lloq", "uloq")],
                               by = c("plateid", "antigen"), all.y = T)

      sample_data_mod$in_quantifiable_range <- ifelse(sample_data_mod$gc == "Acceptable", TRUE, FALSE)

     # sample_data_mod$gate_class_loq <- sample_data_mod$gc
      sample_data_mod$gate_class_loq  <-  "Not Evaluated"

    } else {
      sample_data_mod <- merge(sample_data_au, model_list[[2]][, c("plateid", "antigen", "lloq", "uloq")],
                               by = c("plateid", "antigen"), all.y = T)

      # Below_Lower_Limit is too diluted,Above_Upper_Limit  is too concentrated, and Between_Limits is acceptable
      sample_data_mod$gate_class_loq <- ifelse(sample_data_mod$antibody_au < sample_data_mod$lloq & !is.null(sample_data_mod$lloq), "Too Diluted",
                                   ifelse(sample_data_mod$antibody_au > sample_data_mod$uloq & !is.null(sample_data_mod$uloq),  "Too Concentrated",
                                          "Acceptable"))
      # add Boolean in quantifiable range
      sample_data_mod$in_quantifiable_range <- ifelse(sample_data_mod$antibody_au >= lloq & sample_data_mod$antibody_au <= uloq, T, F)
    }

    return(sample_data_mod)
  }

  # sample_data_view <<- sample_data_mod
  # model_list_view <<- model_list[[2]]
  #sample_data_view <<- sample_data

  # sample_data$gc <- ifelse(!is.null(llod) & sample_data$value_reported < llod, "Below_Lower_Limit",
  #                          ifelse(!is.null(ulod) & sample_data$value_reported > ulod, "Above_Upper_Limit",
  #                                 ifelse(is.null(ulod), "Unclassified",
  #                                 "Between_Limits")))

  # sample_data$gc <- ifelse(!is.null(llod) & sample_data$value_reported < llod, "Below_Lower_Limit",
  #                          ifelse(!is.null(ulod) & sample_data$value_reported > ulod, "Above_Upper_Limit",
  #                                 ifelse(!is.null(llod) & sample_data$value_reported >= llod, "Between_Limits",
  #                                        "Between_Limits")))





# Compute in linear region
# compute_in_linear_region <- function(model_list, sample_data) {
#
#   model_list_df <- model_list[[2]]
#   # Account for exponential model
#   if(model_list_df$crit == "nls_exp") {
#     cat("In exponential merge linear region")
#     sample_data_mod <- merge(sample_data, model_list_df[, c("plateid", "antigen", "llod")],
#                              by = c("plateid", "antigen"), all.y = T)
#
#     cat("after exponential merge linear region")
#
#   } else {
#     sample_data_mod <- merge(sample_data, model_list_df[, c("plateid", "antigen", "llod", "ulod")],
#                              by = c("plateid", "antigen"), all.y = T)
#
#
#     # value reported is the mfi
#     sample_data_mod$in_linear_region <- ifelse(sample_data_mod$value_reported >= model_list_view$bendlower & sample_data_mod$value_reported <=  model_list_view$bendupper,
#                                                 T, F)
#   }
#   return(sample_data_mod)
# }

### Functions for Back-substitution Logic

# 4 Parameter function solve for log dilution
four_param_logistic_log_dilution <- function(sample_data, l_asy, r_asy, xmid, scal, min_log_dilution, max_log_dilution, stype_val, isPower) {
  cat("Min Log dilution in 4 parameter")
  print(min_log_dilution)

  print("printing parameter values ")
  print(l_asy)
  print(r_asy)
  print(xmid)
  print(scal)

  cat("\n sample data 4 param logistic names\n")
  print(names(sample_data))
  print(head(sample_data))

  if (stype_val == "sample") {
   dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id", "dilution",
                                                                      "subject_accession", "antibody_mfi",
                                                                      "antigen", "plateid")]
  } else if (stype_val == "standard") {
    dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id",
                                                                        "subject_accession", "antibody_mfi",
                                                                        "antigen", "plateid")]
  }
  # dilution_mfi_df$au_class <- ifelse(dilution_mfi_df$antibody_mfi > l_asy & dilution_mfi_df$antibody_mfi >= r_asy,
  #                                    "above_r_asy",
  #                                    ifelse(dilution_mfi_df$antibody_mfi <= l_asy & dilution_mfi_df$antibody_mfi < r_asy,
  #                                           "below_l_asy"
  #                                           , "normal"))
  # Calculate log dilution only for valid MFI values
  #xmid - scal * log(((r_asy - l_asy) / (dilution_mfi_df$antibody_mfi - l_asy)) - 1)



  dilution_mfi_df$au_prep <- ifelse(dilution_mfi_df$antibody_mfi >= r_asy,
                                        max_log_dilution,
                                        ifelse(dilution_mfi_df$antibody_mfi <= l_asy,
                                               min_log_dilution,
                                               1))

 # dilution_mfi_df_v <<- dilution_mfi_df

  if (!"antibody_au" %in% names(dilution_mfi_df)) {
    dilution_mfi_df$antibody_au <- NA_real_
  }

  if (isPower) {
    cat("4 param power func")
    # range_width <- 4 * abs(scal)
    # new_min <- max(min_log_dilution, xmid - range_width)
    # new_max <- min(max_log_dilution, xmid + range_width)

    log_dilution_vals <- seq(min_log_dilution, max_log_dilution, length.out = 500)
    predicted_mfi <- Ylm4(log_dilution_vals, l_asy, scal, xmid, r_asy)
    predicted_mfi <- predicted_mfi

    dilution_mfi_df <- dilution_mfi_df

    idx_au_prep <- which(dilution_mfi_df$au_prep == 1)

    # Extract observed MFI values only for these rows
    observed_mfi_subset <- dilution_mfi_df$antibody_mfi[idx_au_prep]

    predicted_au_subset <- sapply(observed_mfi_subset, function(mfi) {
      predict_log10_dilution_fraction(mfi, log_dilution_vals, predicted_mfi)
    })

    dilution_mfi_df$antibody_au[idx_au_prep] <- predicted_au_subset





   # dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_au <-  xmid * (((l_asy - r_asy) / (dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_mfi - r_asy) - 1)^(1 / scal))
  } else {
    dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_au <- xmid - scal * log(((r_asy - l_asy) / (dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_mfi - l_asy)) - 1)
  }

  dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$au_prep == 1,
                                        dilution_mfi_df$antibody_au,
                                        dilution_mfi_df$au_prep)
  # dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$antibody_mfi >= r_asy,
  #                                       0,
  #                                       ifelse(dilution_mfi_df$antibody_mfi <= l_asy,
  #                                              min_log_dilution,
  #                                              xmid - scal * log((r_asy - l_asy) / (dilution_mfi_df$antibody_mfi - l_asy + (r_asy *0.01)) - 1)))


 # print(paste(dilution_mfi_df$au_class, dilution_mfi_df$antibody_mfi, dilution_mfi_df$antibody_au), sep = "__")

  dilution_mfi_df$antibody_au_se <- ifelse(!is.na(dilution_mfi_df$antibody_au),
                                           sd(dilution_mfi_df$antibody_au, na.rm = TRUE) / sqrt(sum(!is.na(dilution_mfi_df$antibody_au))),
                                           NA)

  dilution_mfi_df <- dilution_mfi_df[,!names(dilution_mfi_df) %in% c("au_prep")]

#  dilution_mfi_df_f <<- dilution_mfi_df
  return(dilution_mfi_df)
}




# # 5 parameter function solve for log dilution
five_param_logistic_log_dilution <- function(sample_data, l_asy, r_asy, xmid, scal, g, min_log_dilution, max_log_dilution, stype_val) {

 if (stype_val == "sample") {
  dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id", "dilution", "subject_accession", "antibody_mfi", "antigen", "plateid")]
 } else if (stype_val == "standard") {
   dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id",
                                                                       "subject_accession", "antibody_mfi",
                                                                       "antigen", "plateid")]
 }
  #dilution_mfi_df_view <<- dilution_mfi_df

  if (!"antibody_au" %in% names(dilution_mfi_df)) {
    dilution_mfi_df$antibody_au <- NA_real_
  }
   cat("\nIn 5 parameter logistic log dilution\n")
  # dilution_mfi_df_v <<- dilution_mfi_df
   print(dilution_mfi_df)

   # dilution_mfi_df$au_prep <- ifelse(dilution_mfi_df >= r_asy,
   #                                       max_log_dilution,
   #                                       ifelse(dilution_mfi_df$antibody_mfi <= l_asy,
   #                                              min_log_dilution),
   #                                       1)

   dilution_mfi_df$au_prep <- ifelse(dilution_mfi_df$antibody_mfi >= r_asy,
                                max_log_dilution,
                                 ifelse(
                                   dilution_mfi_df$antibody_mfi <= l_asy,
                                   min_log_dilution,
                                   1
                                 )
                               )

   dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_au <- xmid + scal * log(((l_asy - r_asy) / (dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_mfi - r_asy))^(1/g) - 1)

   dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$au_prep == 1,
                                         dilution_mfi_df$antibody_au,
                                         dilution_mfi_df$au_prep)


  # dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$antibody_mfi > r_asy,
  #                                       0,
  #                                       ifelse(dilution_mfi_df$antibody_mfi < l_asy,
  #                                              min_log_dilution,
  #                                              xmid + scal * log(((l_asy - r_asy) / (dilution_mfi_df$antibody_mfi - r_asy + (abs(l_asy - r_asy) * 0.001)))^(1/g) - 1)))


  dilution_mfi_df$antibody_au_se <- ifelse(!is.na(dilution_mfi_df$antibody_au),
                                           sd(dilution_mfi_df$antibody_au, na.rm = TRUE) / sqrt(sum(!is.na(dilution_mfi_df$antibody_au))),
                                           NA)

  dilution_mfi_df <- dilution_mfi_df[,!names(dilution_mfi_df) %in% c("au_prep")]

  return(dilution_mfi_df)
}

# DRDA solve for log dilution with 5 parameters
five_param_drda_logistic_log_dilution <- function(sample_data, alpha, delta, eta, phi, nu, min_log_dilution, max_log_dilution, stype_val) {
  # alpha_v <<- alpha
  # delta_v<<- delta
  # eta_v <<- eta
  # phi_v <<- phi
  # nu_v <<- nu
  # min_log_dilution_v <<- min_log_dilution
  # max_log_dilution_c <<- max_log_dilution
  # sample_data <<- sample_data
  # Ensure that the input data has the necessary columns
  if (stype_val == "sample") {
    cat("\nsample_data_names\n")
    print(names(sample_data))
   # sample_data_v <<-sample_data

    # Take log10 of the antibody_MFI
    #sample_data$antibody_mfi <- log10(sample_data$antibody_mfi)


    dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi),
                                 c("xmap_sample_id", "dilution", "subject_accession", "antibody_mfi", "antigen", "plateid")]
  } else if (stype_val == "standard") {
   # standard  <<- sample_data
    print("\nstandard_data_names")
    print(names(sample_data))
    dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id",
                                                                        "subject_accession", "antibody_mfi",
                                                                        "antigen", "plateid")]
  }

  if (!"antibody_au" %in% names(dilution_mfi_df)) {
    dilution_mfi_df$antibody_au <- NA_real_
  }

  dilution_mfi_df$au_prep <- ifelse(dilution_mfi_df$antibody_mfi < alpha & eta >= 0 & nu >= 0,
                                    min_log_dilution,
                                    ifelse(dilution_mfi_df$antibody_mfi > (delta + alpha),
                                           0,
                                           1))
  dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_au <-  phi - (1 / eta) * log(((delta / (dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_mfi - alpha))^nu - 1) / nu)

  dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$au_prep == 1,
                                        dilution_mfi_df$antibody_au,
                                        dilution_mfi_df$au_prep)

  # dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$antibody_mfi < alpha & eta >= 0 & nu >= 0,
  #                                       min_log_dilution,
  #                                       ifelse(dilution_mfi_df$antibody_mfi > (delta + alpha),
  #                                              0,
  #                                              phi - (1 / eta) * log(((delta / (dilution_mfi_df$antibody_mfi - alpha))^nu - 1) / nu +  (delta *0.01))))


  # upper_threhold <<- quantile(dilution_mfi_df$antibody_mfi, 0.995, na.rm = TRUE) + 0.05
  #
  # epsilon <- 1e-8
  # safe_term <- ((delta / (dilution_mfi_df$antibody_mfi - alpha))^nu - 1) / nu
  # safe_term <- pmax(safe_term, epsilon)
  # dilution_mfi_df$antibody_au <- ifelse(
  #   dilution_mfi_df$antibody_mfi < alpha & eta >= 0 & nu >= 0,
  #   min_log_dilution,
  #   ifelse(
  #     dilution_mfi_df$antibody_mfi > upper_threhold,
  #     0,
  #     phi - (1 / eta) * log(safe_term)
  #   )
  # )
  #term_raw <- ((delta / (dilution_mfi_df$antibody_mfi - alpha))^nu - 1) / nu
#
# # Only apply epsilon where the term is zero or negative
# term_safe <- ifelse(term_raw <= 0, epsilon, term_raw)
#
# dilution_mfi_df$antibody_au <- ifelse(
#   dilution_mfi_df$antibody_mfi < alpha & eta >= 0 & nu >= 0,
#   min_log_dilution,
#   ifelse(
#     dilution_mfi_df$antibody_mfi > upper_threhold,
#     0,
#     phi - (1 / eta) * log(term_safe)
#   )
# )
  # dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$antibody_mfi < alpha & eta >= 0 & nu >= 0,
  #                                       min_log_dilution,
  #                                       ifelse(dilution_mfi_df$antibody_mfi > upper_threhold,#(delta *1.05) + alpha,
  #                                              0,
  #                                              phi - (1 / eta) * log(((delta / (dilution_mfi_df$antibody_mfi - alpha))^nu - 1) / nu)))



  dilution_mfi_df$antibody_au_se <- ifelse(!is.na(dilution_mfi_df$antibody_au),
                                           sd(dilution_mfi_df$antibody_au, na.rm = TRUE) / sqrt(sum(!is.na(dilution_mfi_df$antibody_au))),
                                           NA)

  dilution_mfi_df <- dilution_mfi_df[,!names(dilution_mfi_df) %in% c("au_prep")]

  # Return the modified dataframe with antibody_au
  return(dilution_mfi_df)
}


# Exponential fit, solves for log dilution
exponential_fit_log_dilution <- function(sample_data, l_asy, xmid, scal, min_log_dilution, stype_val) {
  if (stype_val == "sample") {
    dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id", "dilution", "subject_accession", "antibody_mfi", "antigen", "plateid")]
  } else if (stype_val == "standard") {
    dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id",
                                                                        "subject_accession", "antibody_mfi",
                                                                        "antigen", "plateid")]
  }
 # offset <- (scal - l_asy) * 0.001
  # Calculate log dilution only for valid MFI values
  if (!"antibody_au" %in% names(dilution_mfi_df)) {
    dilution_mfi_df$antibody_au <- NA_real_
  }

  dilution_mfi_df$au_prep <- ifelse(dilution_mfi_df$antibody_mfi <= l_asy,
                                    min_log_dilution,
                                    1)

  dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_au <- log((dilution_mfi_df[dilution_mfi_df$au_prep == 1,]$antibody_mfi - l_asy) / (scal - l_asy )) / xmid

  dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$au_prep == 1,
                                        dilution_mfi_df$antibody_au,
                                        dilution_mfi_df$au_prep)

  # dilution_mfi_df$antibody_au <- ifelse(dilution_mfi_df$antibody_mfi <= l_asy,
  #                                       min_log_dilution,
  #                                       log((dilution_mfi_df$antibody_mfi - l_asy + offset) / (scal - l_asy )) / xmid)



  dilution_mfi_df$antibody_au_se <- ifelse(!is.na(dilution_mfi_df$antibody_au),
                                           sd(dilution_mfi_df$antibody_au, na.rm = TRUE) / sqrt(sum(!is.na(dilution_mfi_df$antibody_au))),
                                           NA)

  dilution_mfi_df <- dilution_mfi_df[,!names(dilution_mfi_df) %in% c("au_prep")]

  return(dilution_mfi_df)
}

obtain_standard_curve_concentration <- function(study_accession, antigen) {
  query_concentration <- paste0("SELECT study_accession, antigen, antigen_family, standard_curve_concentration
	FROM madi_results.xmap_antigen_family
	WHERE study_accession = '",study_accession,"'
	AND antigen = '", antigen,"';")

  concentration_df <- dbGetQuery(conn, query_concentration)
  standard_curve_antigen_concentration <- as.numeric(concentration_df[["standard_curve_concentration"]])
  return(standard_curve_antigen_concentration)
}

# Preform backsubsitution to find true dilution, append to sample_data
backsub_true_dilution_sample <- function(fitted_model, sample_data, dat){
  cat("\nnames in backsub true dilution sample\n")
  print(names(sample_data))
  cat("\nnames in backsub true dilution dat\n")
  print(names(dat))
  min_log_dilution <- min(dat$log_dilution)
  max_log_dilution <- max(dat$log_dilution) + abs(0 - max(dat$log_dilution))/2
  # sample_data$MFI <- sample_data$value_reported # add
  #sample_data$antibody_mfi <- sample_data$value_reported
  #Extract fitted parameter estimates from model
  #if (is.null(fitted_model[[1]]$signif))
 # fitted_model_v <<- fitted_model
  cat("Before if else blocks")
  print(fitted_model[[2]]$crit)
  if (fitted_model[[2]]$crit == "nls_4" || fitted_model[[2]]$crit == "nlslm_4") {
    cat("in nls4/ nlslm_4 ")
    parameter_table <- as.data.frame(summary(fitted_model[[3]])$coefficients)
    parameter_table$term <- rownames(parameter_table) # 4
    parameter_table <- parameter_table[, c("term", "Estimate")]
    n_terms <- nrow(parameter_table)
  } else if (fitted_model[[2]]$crit == "nls_exp") { # 3
    cat("In expnls parameter table")
    parameter_table <- fitted_model[[1]]
    parameter_table <- as.data.frame(parameter_table)
    parameter_table <- parameter_table[, c("term", "estimate")]
    n_terms <- nrow(parameter_table)
  } else if (fitted_model[[2]]$crit == "drda_5") {
    cat("in drda_5")
    parameter_table <- fitted_model[[1]]
    parameter_table <- as.data.frame(parameter_table)
    parameter_table <- parameter_table[, c("term", "estimate")] # 6 rows with calculated drda r_asy
    n_terms <- nrow(parameter_table)
  } else if (fitted_model[[2]]$crit == "nls_5") {
    cat("In nls 5")
    parameter_table <- as.data.frame(summary(fitted_model[[3]])$coefficients)
    parameter_table$term <- rownames(parameter_table)
    parameter_table <- parameter_table[, c("term", "Estimate")] # 5 rows (g)
    n_terms <- nrow(parameter_table)
  } else {
    parameter_table <- NULL
  }

  cat("N Terms")
  print(n_terms)

 # parameter_table <- parameter_table

  std_dat <- dat
  std_dat$xmap_sample_id <- std_dat$sampleid
  std_dat$subject_accession <- std_dat$sampleid
  std_dat$value_reported <- std_dat$mfi
  std_dat$antibody_mfi <- std_dat$mfi
  cat("\n std dat in back function\n")
  print(std_dat)
  if (n_terms == 4) {
    # crit <<- fitted_model[[2]]$crit
    # parameter_table_v <<- parameter_table
    # sample_data_v <<- sample_data
    # std_dat_v <<- std_dat
    # min_log_dilution <<- min_log_dilution
    # max_log_dilution <<- max_log_dilution

    l_asy_est <- parameter_table$Estimate[parameter_table$term == "l_asy"]
    r_asy_est <- parameter_table$Estimate[parameter_table$term == "r_asy"]
    xmid_est <- parameter_table$Estimate[parameter_table$term == "xmid"]
    scal_est <- parameter_table$Estimate[parameter_table$term == "scal"]

    if (fitted_model[[2]]$crit == "nls_4") {
      log_dilution <- four_param_logistic_log_dilution(sample_data, l_asy_est, r_asy_est, xmid_est, scal_est, min_log_dilution, max_log_dilution, "sample", isPower = F)

      log_dilution_std <- four_param_logistic_log_dilution(std_dat, l_asy_est, r_asy_est, xmid_est, scal_est, min_log_dilution, max_log_dilution, "standard", isPower = F)

      log_dilution_std$predicted_mfi <- fitted_model[[3]]$fitted.values
    } else {
      log_dilution <- four_param_logistic_log_dilution(sample_data, l_asy_est, r_asy_est, xmid_est, scal_est, min_log_dilution, max_log_dilution, "sample", isPower = T)

      log_dilution_std <- four_param_logistic_log_dilution(std_dat, l_asy_est, r_asy_est, xmid_est, scal_est, min_log_dilution, max_log_dilution, "standard", isPower = T)

      cat("before adding predicted mfi")
      log_dilution_std$predicted_mfi <-  as.numeric(fitted_model[[3]]$m$fitted())
      cat("after adding predicted mfi")
    }
  } else if (n_terms == 3) {
    cat("3 terms")
    scal_est <- parameter_table$estimate[parameter_table$term == "scal"]
    l_asy_est <- parameter_table$estimate[parameter_table$term == "l_asy"]
    xmid_est <-  parameter_table$estimate[parameter_table$term == "xmid"]

    log_dilution <- exponential_fit_log_dilution(sample_data, l_asy = l_asy_est, xmid = xmid_est, scal = scal_est, min_log_dilution, "sample")
    log_dilution_std <- exponential_fit_log_dilution(std_dat, l_asy = l_asy_est, xmid = xmid_est, scal = scal_est, min_log_dilution, "standard")

    log_dilution_std$predicted_mfi <- as.vector(fitted_model[[3]]$m$fitted())
  } else if (n_terms == 5) {
    cat("NLS 5 terms ")
    l_asy_est <- parameter_table$Estimate[parameter_table$term == "l_asy"]
    r_asy_est <- parameter_table$Estimate[parameter_table$term == "r_asy"]
    xmid_est <- parameter_table$Estimate[parameter_table$term == "xmid"]
    scal_est <- parameter_table$Estimate[parameter_table$term == "scal"]
    g_est <- parameter_table$Estimate[parameter_table$term == "g"]

    log_dilution <- five_param_logistic_log_dilution(sample_data, l_asy = l_asy_est, r_asy = r_asy_est, xmid = xmid_est, scal = scal_est, g = g_est,
                                                     min_log_dilution = min_log_dilution, max_log_dilution = max_log_dilution, "sample")
    log_dilution_std <- five_param_logistic_log_dilution(std_dat, l_asy = l_asy_est, r_asy = r_asy_est, xmid = xmid_est, scal = scal_est, g = g_est,
                                                     min_log_dilution = min_log_dilution, max_log_dilution = max_log_dilution, "standard")

    log_dilution_std$predicted_mfi <- fitted_model[[3]]$fitted.values
  } else {
    cat("DRDA 5 terms")
    alpha_est <- parameter_table$estimate[parameter_table$term == "alpha"]
    delta_est <- parameter_table$estimate[parameter_table$term == "delta"]
    eta_est <- parameter_table$estimate[parameter_table$term == "eta"]
    phi_est <-  parameter_table$estimate[parameter_table$term == "phi"]
    nu_est <- parameter_table$estimate[parameter_table$term == "nu"]

    log_dilution <- five_param_drda_logistic_log_dilution(sample_data, alpha = alpha_est, delta = delta_est,
                                                          eta = eta_est, phi = phi_est,
                                                          nu = nu_est,
                                                          min_log_dilution = min_log_dilution, max_log_dilution = max_log_dilution, stype_val = "sample")

    cat("\nafter sample log dilution\n")

    log_dilution_std <- five_param_drda_logistic_log_dilution(std_dat, alpha = alpha_est, delta = delta_est,
                                                          eta = eta_est, phi = phi_est,
                                                          nu = nu_est,
                                                          min_log_dilution = min_log_dilution, max_log_dilution = max_log_dilution, stype_val = "standard")
    cat("\nafter log_dilution_std \n")

      print(fitted_model[[3]]$fitted.values)
     log_dilution_std$predicted_mfi <- fitted_model[[3]]$fitted.values

     cat("\nafter adding predicted mfi ")
  }




  names(dat)[names(dat) == "dilution_factor"] <- "dilution_fraction"
  #dat_v <<- dat
  #log_dilution_std_v <- log_dilution_std
  names(log_dilution_std)[names(log_dilution_std) == "xmap_sample_id"] <- "sampleid"


  log_dilution_std <- merge(log_dilution_std[, !(names(log_dilution_std) %in% c("xmap_sample_id", "subject_accession"))], dat[,c("sampleid", "source", "dilution_fraction")], by = c("sampleid"), all.x = TRUE)

  # log_dilution_std <<- log_dilution_std
  # log_dilution <<- log_dilution
  # log_dilution_v <<- log_dilution
  # fitted_model <<- fitted_model

  log_dilution <- log_dilution[!is.na(log_dilution$antibody_au), ]
  log_dilution$antibody_au_se <- as.numeric(log_dilution$antibody_au_se)

  log_dilution$dilution_fraction <- exp(log_dilution$antibody_au)

  # Clip  the column between 0 and 1
  log_dilution$dilution_fraction <- replace(log_dilution$dilution_fraction, log_dilution$dilution_fraction < 0, 0)
  log_dilution$dilution_fraction <- replace(log_dilution$dilution_fraction, log_dilution$dilution_fraction > 1, 1)


  # log_dilution_v <<- log_dilution

  current_antigen <- unique(log_dilution$antigen)
  current_study <- as.character(fitted_model[[2]]$study_accession)
  antigen_standard_curve_concentration <- obtain_standard_curve_concentration(study_accession = current_study, antigen = current_antigen)

  # serum dilution * concentration in arbitrary units * 1000 for readability
  log_dilution$rau <- (log_dilution$dilution * log_dilution$dilution_fraction) * antigen_standard_curve_concentration

   # log_dilution$rau <- (log_dilution$dilution * log_dilution$dilution_fraction) *1000
 # log_dilution$rau <- log_dilution$dilution_fraction *1000

  # log_dilution_v <<- log_dilution
  # fitted_model <<- fitted_model
  # Calculate the scaled distance of MFI to linear center
  log_dilution$quality_score <- scale(log_dilution$antibody_mfi - as.numeric(fitted_model[[2]]$linear_center))

  # log_dilution_std <<- log_dilution_std
  # log_dilution_v <<- log_dilution_std
  # crit <<- fitted_model[[2]]$crit

cat("\nat end of backsub\n")
  return(list(log_dilution, log_dilution_std))
}

# When no standard curve is found add indicators and nulls to sample data
# for the antigen on the plate
prepare_null_model_samples <- function(plate_samples) {
  plate_samples$antibody_au <- NA_real_
  plate_samples$rau <- NA_real_ # this is what is saved in antibody_au column
  plate_samples$antibody_au_se <- NA_real_
  plate_samples$gc <- "Not Evaluated"
  plate_samples$in_linear_region <- NA
  plate_samples$gate_class_linear_region <- "Not Evaluated"
  plate_samples$in_quantifiable_range <- NA
  plate_samples$gate_class_loq <- "Not Evaluated"
  plate_samples$quality_score <- NA_real_

  return(plate_samples)

}


# Returns the table of MFI and its corresponding dilution
backsub_true_dilution_tab <- function(fitted_model, sample_data){
  #Extract fitted parameter estimates from model
  parameter_table <- fitted_model[1]
  parameter_table <- as.data.frame(parameter_table)
  parameter_table <- parameter_table[, c("term", "estimate")]

  n_terms <- nrow(parameter_table)

  # create new au column
  sample_data$antibody_au <- NA
  # 4 parameters
  if (n_terms == 4){
    # Extract necessary parameters for the 4-parameter logistic model
    l_asy_est <- parameter_table$estimate[parameter_table$term == "l_asy"]
    r_asy_est <- parameter_table$estimate[parameter_table$term == "r_asy"]
    xmid_est <- parameter_table$estimate[parameter_table$term == "xmid"]
    scal_est <- parameter_table$estimate[parameter_table$term == "scal"]

    # Backsubsitution for Log Dilution
    log_dilution <- four_param_logistic_log_dilution(sample_data$antibody_mfi, l_asy_est, r_asy_est, xmid_est, scal_est, "sample")
    return(log_dilution)

  } else if (n_terms == 5) {
    # Extract necessary parameters for the 5-parameter logistic model
    l_asy_est <- parameter_table$estimate[parameter_table$term == "l_asy"]
    r_asy_est <- parameter_table$estimate[parameter_table$term == "r_asy"]
    xmid_est <- parameter_table$estimate[parameter_table$term == "xmid"]
    scal_est <- parameter_table$estimate[parameter_table$term == "scal"]
    g_est <- parameter_table$estimate[parameter_table$term == "g"]

    # Backsubsitution for Log Dilution
    log_dilution <- five_param_logistic_log_dilution(MFI = sample_data$antibody_mfi, l_asy = l_asy_est, r_asy = r_asy_est, xmid = xmid_est, scal = scal_est, g = g_est, "sample")
    return(log_dilution)

  } else{ # It failed
    warning("The fitted model does not have 4 or 5 parameters")
    return(NA)
  }


}

### Prozone Correction
correct_prozone <- function(stdframe = NULL, prop_diff = NULL, dil_scale = NULL, mfi_var = "mfi", log_dilution_var = "log_dilution") {
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
  stdframe <- stdframe[!is.na(stdframe[[mfi_var]]) & !is.na(stdframe[[log_dilution_var]]),]


  # 1. identify the highest mfi and corresponding log_dilution in stdframe
  max_mfi <- max(stdframe[[mfi_var]])
  logd_at_max_mfi <- max(stdframe[stdframe[[mfi_var]]==max_mfi, ][[log_dilution_var]])
  # 2. identify the mfis lower than the max_mfi at higher concentrations and dampen the delta mfis to compensate for
  stdframe[stdframe[[log_dilution_var]] > logd_at_max_mfi, ][[mfi_var]] <- max_mfi +
    (
      (max_mfi - stdframe[stdframe[[log_dilution_var]] > logd_at_max_mfi, ][[mfi_var]]) * prop_diff /
        ((stdframe[stdframe[[log_dilution_var]] > logd_at_max_mfi, ][[log_dilution_var]]-logd_at_max_mfi) * dil_scale)
    )
  return(stdframe)
  ### end correct for prozone effect
}

delete_model_fit <- function(conn, study_accession_in, experiment_accession_in, antigen_in, plateid_in, source_in, is_log_mfi_axis_in) {
  delete_query <- glue::glue("DELETE
    FROM madi_results.xmap_standard_fits
    WHERE xmap_standard_fits IN (
      SELECT xmap_standard_fits
      FROM madi_results.xmap_standard_fits
      WHERE study_accession = '{study_accession_in}'
      AND experiment_accession = '{experiment_accession_in}'
      AND antigen = '{antigen_in}'
      AND plateid = '{plateid_in}'
      AND source = '{source_in}'
      AND is_log_mfi_axis = {is_log_mfi_axis_in}
    );"
  )
  cat("after delete query")
  delete_outcome <- DBI::dbExecute(conn, delete_query)
  cat("after delete outcome")
  print(delete_outcome)
  print(delete_query)
}

## Account for bend lower to bend lower missing
compute_linear_center <- function(mod_antigen, filtered_sc_data) {

  if (!"antibody_mfi" %in% names(filtered_sc_data)) {
    filtered_sc_data$antibody_mfi <- filtered_sc_data$mfi
  }

  #filtered_sc_data_v <<- filtered_sc_data

  #mod_antigen_v <<- mod_antigen
  model_fit_tab <- mod_antigen[[2]]
  cat("names of filtered sc data linear center\n")
  print(names(filtered_sc_data))
  cat("Model fit tab linear center")
  print(model_fit_tab[[2]])



#  if (is.na(model_fit_tab["bendlower"])) {
  if (is.na(model_fit_tab[["bendlower"]]) || is.null(model_fit_tab[["bendlower"]])) {
    lower_bound <- min(filtered_sc_data$antibody_mfi) * 1.1
  } else {
    lower_bound <- as.numeric(model_fit_tab[["bendlower"]])
  }

#  if (is.na(model_fit_tab["bendupper"])) {
  if (is.na(model_fit_tab[["bendupper"]]) || is.null(model_fit_tab[["bendupper"]])) {
    upper_bound <- max(filtered_sc_data$antibody_mfi) * 0.9
  } else {
    upper_bound <- as.numeric(model_fit_tab[["bendupper"]])
  }
  model_fit_tab$linear_center <- (lower_bound + upper_bound)/2

  mod_antigen[[2]] <- model_fit_tab

  return(mod_antigen)
}

### Function for saving the model fit when button is clicked
saveModelFit <- function(conn, mod, filtered_sc_data) {
  req(mod)

  # if (is.null(mod)) {
  #   showNotification("Error: Model object is NULL", type = "error")
  #   return()
  # }
  model_fit_tab <- mod[[2]]
  model_fit_tab <- as.data.frame(model_fit_tab)

  if (is.null(model_fit_tab["crit"])) {
    model_fit_tab["l_asy"] <- NULL
    model_fit_tab["r_asy"] <- NULL
    model_fit_tab["x_mid"] <- NULL
    model_fit_tab["scale"] <- NULL
    model_fit_tab["bendlower"] <- NULL
    model_fit_tab["bendupper"] <- NULL
    model_fit_tab["llod"] <- NULL
    model_fit_tab["ulod"] <- NULL
    model_fit_tab["loglik"] <- NULL
    model_fit_tab["aic"] <- NULL
    model_fit_tab["bic"] <- NULL
    model_fit_tab["deviance"] <- NULL
    model_fit_tab["dfresidual"] <- NULL
    model_fit_tab["rsquare_fit"] <- NULL
    model_fit_tab["g"] <- NULL
    model_fit_tab["mse"] <- NULL
    model_fit_tab["cv"] <- NULL
    model_fit_tab["lloq"] <- NULL
    model_fit_tab["uloq"] <- NULL
    model_fit_tab["loq_method"] <- NULL
    model_fit_tab["bkg_method"] <- NULL
    model_fit_tab["is_log_mfi_axis"] <- NULL
    model_fit_tab["linear_center"] <- NULL
    model_fit_tab["formula"] <- NULL
  }
  if (as.character(model_fit_tab["crit"]) == "drda_5") {
    model_fit_tab <- model_fit_tab[,c("study_accession", "experiment_accession","plateid","antigen","iter",
                                      "status","alpha","eta","phi",
                                      "nu","r_asy","bendlower","bendupper","llod",
                                      "ulod","loglik","dfresidual","nobs","rsquare_fit",
                                      "source", "crit", "mse", "cv", "lloq", "uloq", "loq_method", "bkg_method", "is_log_mfi_axis", "linear_center", "formula")]

    names(model_fit_tab)[names(model_fit_tab) == "alpha"] <- "l_asy"
    names(model_fit_tab)[names(model_fit_tab) == "phi"] <- "x_mid"
    names(model_fit_tab)[names(model_fit_tab) == "eta"] <- "scale"
    names(model_fit_tab)[names(model_fit_tab) == "nu"] <- "g"
  }

  cat("Before delete query")
  delete_query <- glue::glue("DELETE
    FROM madi_results.xmap_standard_fits
    WHERE xmap_standard_fits IN (
      SELECT xmap_standard_fits
      FROM madi_results.xmap_standard_fits
      WHERE study_accession = '{model_fit_tab$study_accession}'
      AND experiment_accession = '{model_fit_tab$experiment_accession}'
      AND antigen = '{model_fit_tab$antigen}'
      AND plateid = '{model_fit_tab$plateid}'
      AND source = '{model_fit_tab$source}'
      AND is_log_mfi_axis = {model_fit_tab$is_log_mfi_axis}
      AND linear_center = {model_fit_tab$linear_center}
    );"
  )
  cat("after delete query")
  delete_outcome <- DBI::dbExecute(conn, delete_query)
  cat("after delete outcome")
  print(delete_outcome)
  print(delete_query)

  tryCatch({
    DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard_fits"), model_fit_tab)

    showNotification(glue::glue("Model fit row inserted successfully for antigen: {model_fit_tab$antigen} on plate: {model_fit_tab$plateid}"), type = "message")
  }, error = function(e) {
    showNotification(glue::glue("Error inserting model fit row for antigen: {model_fit_tab$antigen}: {e$message}"), type = "error")
  })
}

#
# SELECT study_accession, experiment_accession, plateid, antigen, iter, status, crit, l_asy, r_asy, x_mid, scale, bendlower, bendupper, llod, ulod, loglik, aic, bic, deviance, dfresidual, nobs, rsquare_fit, source, g, mse, cv, lloq, uloq, loq_method, bkg_method
# FROM madi_results.xmap_standard_fits
# WHERE study_accession = 'BK study' AND experiment_accession = 'IgGtot' AND antigen = 'vp1' and plateid = '20241024.IggtotVP1.1.500.Plate7';

# Save arbritary units and in linear region
save_au <- function(sample_data_au) {
  print("Before updating antibody values")
#  sample_data_au_v <<- sample_data_au
  print(head(sample_data_au))
  # update_antibody_values <- sample_data_au[, c("xmap_sample_id", "dilution", "antibody_au", "rau", "antibody_au_se", "gc", "in_linear_region", "gate_class_loq", "in_quantifiable_range")]
  #
  # update_antibody_values_v <- update_antibody_values

  print(summary(sample_data_au$rau))

  cat("After initialization of update_antibody_values")

  tmp_table_name <- "xmap_tmp_antibody_values"
  if (dbExistsTable(conn, Id(schema = "madi_results", table = tmp_table_name))) {
    DBI::dbRemoveTable(conn, Id(schema = "madi_results", table = tmp_table_name))
  }

  cat("Write update_antibody_values to database")
  DBI::dbWriteTable(conn, Id(schema = "madi_results", table = tmp_table_name), sample_data_au)
  DBI::dbExecute(conn, "UPDATE madi_results.xmap_sample
                   SET antibody_au = xmap_tmp_antibody_values.rau,
                       antibody_au_se = xmap_tmp_antibody_values.antibody_au_se,
                       gate_class = xmap_tmp_antibody_values.gc,
                       in_linear_region = xmap_tmp_antibody_values.in_linear_region,
                       gate_class_linear_region = xmap_tmp_antibody_values.gate_class_linear_region,
                       gate_class_loq = xmap_tmp_antibody_values.gate_class_loq,
                       in_quantifiable_range = xmap_tmp_antibody_values.in_quantifiable_range,
                       quality_score = xmap_tmp_antibody_values.quality_score
                   FROM madi_results.xmap_tmp_antibody_values
                   WHERE xmap_tmp_antibody_values.xmap_sample_id = xmap_sample.xmap_sample_id;")
  showNotification("Updated antibody_au, antibody_au_se, gate class, in linear region, and in quantifiable range successfully", type = "message")

  # Clean up by removing temporary table
  if(dbExistsTable(conn, Id(schema = "madi_results", table = tmp_table_name))) {
    DBI::dbRemoveTable(conn, Id(schema = "madi_results", table = tmp_table_name))
  }
}

# save_loq_gating <- function(sample_data_au) {
#   print("Before updating loq gating values")
#   update_loq_gating_values <- sample_data_au[, c("xmap_sample_id", "gate_class_loq", "in_quantifiable_range")]
#
#   cat("After initialization of update_loq_gating_values")
#
#   tmp_table_name <- "xmap_tmp_loq_gating_values"
#   if (dbExistsTable(conn, Id(schema = "madi_results", table = tmp_table_name))) {
#     DBI::dbRemoveTable(conn, Id(schema = "madi_results", table = tmp_table_name))
#   }
#
#   cat("Write update_loq_gating_values to database")
#   DBI::dbWriteTable(conn, Id(schema = "madi_results", table = tmp_table_name), update_loq_gating_values)
#   DBI::dbExecute(conn, "UPDATE madi_results.xmap_sample
#                    SET gate_class_loq = xmap_tmp_loq_gating_values.gate_class_loq,
#                        in_quantifiable_range = xmap_tmp_loq_gating_values.in_quantifiable_range
#                    FROM madi_results.xmap_tmp_loq_gating_values
#                    WHERE xmap_tmp_loq_gating_values.xmap_sample_id = xmap_sample.xmap_sample_id;")
#   showNotification("Updated loq gate class and in quantifiable range successfully", type = "message")
#
#   # Clean up by removing temporary table
#   if(dbExistsTable(conn, Id(schema = "madi_results", table = tmp_table_name))) {
#     DBI::dbRemoveTable(conn, Id(schema = "madi_results", table = tmp_table_name))
#   }
# }



compute_model <- function(std_curve_data) {

  #Access the reactive filtered_data standard
  filtered_data_val <- std_curve_data[std_curve_data$source == source & std_curve_data$experiment_accession == experiment &
                                        std_curve_data$plateid == plate &
                                        std_curve_data$antigen == antigen,]

  # apply prozone correction
  filtered_data_val <- correct_prozone(filtered_data_val, prop_diff =  0.1, dil_scale = 2)

  # Account for log MFI user setting
  # if (is_log_mfi_axis) {
  #   filtered_data_val$mfi <- log10(filtered_data_val$mfi)
  #   buffer_data$mfi <- log10(buffer_data$mfi)
  # }
  #
  # if (aggrigate_mfi_dilution) {
  #   filtered_data_val <- aggregate_mfi_dilution_factor(filtered_data_val)
  # }
  # # Account for the Blanks
  # if (bkg == "included") {
  #   filtered_data_val <- include_blanks(buffer_data, filtered_data_val, plateid = plate, antigen = antigen)
  # }
  # if (bkg == "subtracted") {
  #   filtered_data_val <- subtract_blanks(buffer_data = buffer_data, data = filtered_data_val, stype = "standards",
  #                                    plateid = plate, antigen = antigen, multiplier = 1)
  # } else if (bkg == "subtracted_3x") {
  #   filtered_data_val <- subtract_blanks(buffer_data = buffer_data, data = filtered_data_val, stype = "standards",
  #                                    plateid = plate, antigen = antigen, multiplier = 3)
  # } else if (bkg == "subtracted_10x") {
  #   filtered_data_val <- subtract_blanks(buffer_data = buffer_data, data = filtered_data_val, stype = "standards",
  #                                    plateid = plate, antigen = antigen, multiplier = 10)
  # } else {
  #   # ignoring buffer
  #   filtered_data_val <- filtered_data_val
  # }

  studySelection <- unique(std_curve_data$study_accession)

  mod <- tryCatch({
    # set seed for reproducibility
    set.seed(11262024)
    compute_robust_curves_5_param(
      dat = filtered_data_val,
      antigen = antigen,
      plate = plate,
      study_accession = studySelection,
      experiment_accession = experiment,
      source = source,
      bkg = bkg,
      is_log_mfi_axis = is_log_mfi_axis,
      g_value = 0.5
    )
  }, error = function(e){
    print(e)
    return(NULL)
  })

  return(mod)
}

# Compute the arbitrary units and limits of quantification range for sample data.
# compute_au <- function(model, sample_data, std_curve_data) {
#
#   # cat("Before compute gate class names of sample data")
#   # print(names(sample_data))
#
#   gc_sample_data <- compute_gate_class(model, sample_data)
#   # if log the value reported is not assigned correctly otherwise.
#   gc_sample_data$value_reported <- gc_sample_data$antibody_mfi
#   # if ("antibody_mfi" %in% names(gc_sample_data)) {
#   #   gc_sample_data$antibody_mfi <- gc_sample_data$value_reported
#   # }
#   # cat("After compute gate class names of sample data")
#   # print(names(gc_sample_data))
#
#   #gc_sample_data_v <<- gc_sample_data
#   # filter the standard data value
#   # sample_data_val <- sample_data_val[sample_data_val$antigen == input$antigenSelection &
#   #                                      sample_data_val$plateid == input$plateSelection,] #plate
#
#   sample_data_au <- backsub_true_dilution_sample(fitted_model = model, sample_data = gc_sample_data, dat = std_curve_data)[[1]]
#   # gate the sample data with limits of quantification
#   sample_data_au_view <- sample_data_au
#   # model_view <<- model
#
#   return(sample_data_au)
# }
blank_treatment <- function(buffer_data, filtered_sc_data, plate, antigen, bkg) {
if (bkg == "included") {
  filtered_sc_data <- include_blanks(buffer_data, filtered_sc_data, plateid = plate, antigen = antigen)
}
if (bkg == "subtracted") {
  filtered_sc_data <- subtract_blanks(buffer_data = buffer_data, data = filtered_sc_data, stype = "standards",
                                      plateid = plate, antigen = antigen, multiplier = 1)
} else if (bkg == "subtracted_3x") {
  filtered_sc_data <- subtract_blanks(buffer_data = buffer_data, data = filtered_sc_data, stype = "standards",
                                      plateid = plate, antigen = antigen, multiplier = 3)
} else if (bkg == "subtracted_10x") {
  filtered_sc_data <- subtract_blanks(buffer_data = buffer_data, data = filtered_sc_data, stype = "standards",
                                      plateid = plate, antigen = antigen, multiplier = 10)
} else {
  # ignoring buffer
  filtered_sc_data <- filtered_sc_data
}

  return(filtered_sc_data)
}


save_fit_au <- function(dat, sample_data, selectedExperiment, selectedSource, buffer_data, bkg, aggregate_mfi_dilution, apply_prozone_correction, is_log_mfi_axis) {

  # Account for log MFI user setting
  if (is_log_mfi_axis) {
    dat$mfi <- log10(dat$mfi)
    dat$antibody_mfi <- dat$mfi
    buffer_data$mfi <- log10(buffer_data$mfi)

    sample_data$value_reported <- log10(sample_data$value_reported)
    sample_data$antibody_mfi <- sample_data$value_reported#log10(sample_data$value_reported)
    sample_data$mfi <- sample_data$value_reported #log10(sample_data$value_reported)
  }


 # dat_v <<- dat

  if (aggregate_mfi_dilution) {
    dat <- aggregate_mfi_dilution_factor(dat)
  }

  plate_list <- unique(sample_data$plateid)

  req(nrow(dat) > 0)
  model_count <- 0

  failed_fits <- list()
  pass_fits <- list()

  for (plate in plate_list) {

    cat("Before plate filter")
    print(plate)
    # sc is standard curve
   # plate_data <- dat[dat$plateid == plate & dat$antigen %in% antigen_list_input,]
   # dat_v <<- dat
    plate_sc_data <- dat[dat$plateid == plate, ]
    plate_source <- unique(plate_sc_data$source)

  # plate_sc_data_view <<- plate_sc_data
  # source_v <<- selectedSource
  # experiment_v <<- selectedExperiment


    cat("Plateid and antigen list input in save_fit_au")
    print(unique(plate_sc_data$plateid))

    antigen_list <- unique(plate_sc_data$antigen)
    print(antigen_list)
    #antigen_list_v <<- antigen_list

    for(antigen in antigen_list) {

      # filter plate and antigen
      filtered_sc_data <- plate_sc_data[plate_sc_data$source == plate_source & plate_sc_data$antigen == antigen
                                  & plate_sc_data$experiment_accession == selectedExperiment,]

      filtered_sc_data_view_1 <<- filtered_sc_data

      filtered_sc_data <- blank_treatment(buffer_data, filtered_sc_data, plate, antigen, bkg)

      cat("after save fit au apply background")
      print(filtered_sc_data)

      if (nrow(filtered_sc_data) > 0) {
      cat(paste("Running model for antigen", antigen, "on plate", plate))

      #mod_antigen <- compute_model(filtered_sc_data, antigen, plate, selectedExperiment, selectedSource, buffer_data, bkg, aggregate_mfi_dilution, is_log_mfi_axis)

       # apply prozone correction
      if (apply_prozone_correction) {
        filtered_sc_data <- correct_prozone(filtered_sc_data, prop_diff =  0.1, dil_scale = 2)
      }

      # ensure variability in the model
       mfi_sd <- sd(filtered_sc_data$mfi, na.rm = TRUE)
       mfi_range <- range(filtered_sc_data$mfi, na.rm = TRUE)
       if (mfi_sd < 0.01 || diff(mfi_range) < 0.05) {
         mod_antigen <- NULL
       } else {
      #filtered_sc_data_view_2 <<- filtered_sc_data

      mod_antigen <- tryCatch({
        # set seed for reproducibility
        set.seed(11262024)
        compute_robust_curves_5_param(
          dat = filtered_sc_data,
          antigen = antigen,
          plate = plate,
          study_accession =  unique(filtered_sc_data$study_accession),
          experiment_accession = unique(filtered_sc_data$experiment_accession),
          source = plate_source,
          bkg = bkg,
          is_log_mfi_axis = is_log_mfi_axis,
          g_value = 0.5
        )
      }, error = function(e){
        print(e)
        return(NULL)
      })

      }




      cat("\nafter mod antigen with fitted values\n")
      print(filtered_sc_data)
      print(mod_antigen[[3]]$fitted.values)

      if (!is.null(mod_antigen)){

        # standard curve fits
        key <- paste(plate, antigen, sep = "_")
        pass_fits[[key]] <- list(
          plate = plate,
          antigen = antigen,
          model = mod_antigen,
          data = filtered_sc_data
        )


        cat("Before Save")
        print(length(mod_antigen))
        #print(mod_antigen[[2]])
        mod_antigen <- compute_linear_center(mod_antigen, filtered_sc_data)

        saveModelFit(conn, mod_antigen, filtered_sc_data)
        model_count <- model_count + 1
        cat("Model count")
        print(model_count)

        sample_data_val_filtered <- sample_data[sample_data$antigen == antigen &
                                                  sample_data$plateid == plate,]

        #sample_data_val_filtered_view <<- sample_data_val_filtered
        #mod_antigen_view <<- mod_antigen
        #filtered_sc_data_view <<- filtered_sc_data

        # filtered_data <- filtered_data
        # std curve data
        #filtered_data_v <<- filtered_data
        cat("before gc calculation in saving ")


      # Gating the lods.
       #sample_data_val_filtered <- compute_gate_class(mod_antigen, sample_data_val_filtered)

       sample_data_au <- backsub_true_dilution_sample(fitted_model = mod_antigen, sample_data = sample_data_val_filtered, dat = filtered_sc_data)[[1]]

       # cat("Critical Value : \n")
       # print(antigen)
       # print(plate)
       # cat(":\n")
      # model_class <- backsub_true_dilution_sample(fitted_model = mod_antigen, sample_data = sample_data_val_filtered, dat = filtered_sc_data)[[3]]
      # print(model_class)
     #  sample_data_au_static <<- sample_data_au
       # gate loq
        sample_data_au <- compute_gate_class(mod_antigen, sample_data_au)
       # sample_data_au_gc_view <<- sample_data_au
       sample_data_loq_gated <- compute_loq_gate_class(model_list = mod_antigen, sample_data_au = sample_data_au)
     #  sample_data_loq_gated_view <<- sample_data_loq_gated


         # Save Arbitrary Units
        save_au(sample_data_loq_gated)
        cat("Finished Saving AU for current fit")
      } # test on mod antigen not null
      else {
        cat("NO Model Fitted: Gating Not Evaluated")
        key <- paste(plate, antigen, sep = "_")
        failed_fits[[key]] <- list(
          plate = plate,
          antigen = antigen
        )
        sample_data_null_mod <- sample_data[sample_data$antigen == antigen &
                                                  sample_data$plateid == plate,]

        #sample_data_null_mod_v <<- sample_data_null_mod

        gated_samples_null_mod <- prepare_null_model_samples(plate_samples = sample_data_null_mod)

        #gated_samples_null_mod_v <<- gated_samples_null_mod

        save_au(gated_samples_null_mod)

        study_accession <- unique(filtered_sc_data$study_accession)
        experiment_accession <- unique(filtered_sc_data$experiment_accession)

        delete_model_fit(conn, study_accession_in = study_accession,
                         experiment_accession_in = experiment_accession ,
                         antigen_in = antigen,
                         plateid_in = plate,
                         source  = selectedSource,
                         is_log_mfi_axis_in = is_log_mfi_axis)


        } # end else for null model
      } # test nrow > 0
    } # end antigen loop
    # selected_antigen(NULL)
  } # end plate loop

 #failed_fits_view <<- failed_fits
# failed_fits_view[["MADI01.ADCD.pt1.450x_pentamer_15"]]
 # pass_fits_view <<- pass_fits
} # end function


# Returns a boolean indicating if the plate is too diluted or not.
is_plate_diluted <- function(df) {
  df <- df[order(df$mfi),]
  df <- df[!is.na(df$mfi),]

  if (nrow(df) < 2) {
    warning("Not enough data after removing NA values in MFI")
    return(FALSE)
  }

  range_mfi <- range(df$mfi, na.rm = T)
  range_mfi <- range_mfi[2] - range_mfi[1]
  print("MFI Range")
  print(range_mfi)

  range_mfi_concentrated <- range(df$mfi[1:2], na.rm = T)
  range_mfi_concentrated <- range_mfi_concentrated[2] - range_mfi_concentrated[1]
  print("Range")
  print(range_mfi_concentrated)

  least_diluted <- range_mfi_concentrated  < (0.20 * range_mfi)
  return(least_diluted)
}

is_concentrated <- function(df){
  df <- df[order(df$mfi),]

  range_mfi <- range(df$mfi, na.rm = T)
  range_mfi <- range_mfi[2] - range_mfi[1]
  print("is Concentrated: Range MFI")
  print(range_mfi)
  range_mfi_diluted <- range(df$mfi[nrow(df)-1:nrow(df)], na.rm = T)
  range_mfi_diluted <- range_mfi_diluted[2] - range_mfi_diluted[1]
  print("Range concentrated")
  print(range_mfi_diluted)

  most_diluted <- range_mfi_diluted > (0.20 * range_mfi)
  return(most_diluted)
}


### For Standard Curve Summary Tab

fetch_standard_curves <- function(study_accession, experiment_accession, bkg_method, is_log_mfi_axis ) {
  query <- paste0("
    SELECT xmap_standard_fits, study_accession, experiment_accession, plateid,
           antigen, iter, status, crit, l_asy, r_asy, x_mid, scale, bendlower, bendupper,
           llod, ulod, loglik, aic, bic, deviance, dfresidual, nobs, rsquare_fit, source, g, lloq, uloq, loq_method, bkg_method, is_log_mfi_axis
    FROM madi_results.xmap_standard_fits
    WHERE study_accession = '", study_accession, "'
    AND experiment_accession = '", experiment_accession, "'
    AND bkg_method = '", bkg_method, "'
    AND is_log_mfi_axis = '", is_log_mfi_axis,"';")


  # Run the query and fetch the result as a data frame
  result_df <- dbGetQuery(conn, query)

}


fetch_sample_data_feature <- function(study_accession, experiment_accession) {
  query <- paste0("
  SELECT *
	FROM madi_results.xmap_sample
	WHERE study_accession = '",study_accession, "'
	AND experiment_accession = '",experiment_accession, "'")

  # Run the query and fetch the result as a data frame
  result_df <- dbGetQuery(conn, query)
}


fetch_standard_curves_mse_feature <- function(study_accession, bkg_method) {
  query <- paste0("
    SELECT xmap_standard_fits, study_accession, experiment_accession, plateid,
           antigen, iter, status, crit, l_asy, r_asy, x_mid, scale, bendlower, bendupper,
           llod, ulod, loglik, aic, bic, deviance, dfresidual, nobs, rsquare_fit, source, g, mse, cv, lloq, uloq, loq_method, bkg_method
    FROM madi_results.xmap_standard_fits
    WHERE study_accession = '", study_accession, "' AND  bkg_method = '", bkg_method,"'")

  # Run the query and fetch the result as a data frame
  result_df <- dbGetQuery(conn, query)

}


# Calculate fitted values for drda
calculate_fitted_drda5 <- function(x, l_asy, r_asy, x_mid, scale, g) {
  delta <- r_asy - l_asy
  # cat("Model parameters: l_asy =", l_asy, ", r_asy =", r_asy, "delta = ", delta, "x_mid =", x_mid, ", scale =", scale, ", g =", g, "\n")
  model_params <- c(l_asy, delta, scale, x_mid, g)
  drda::logistic5_fn(x, model_params)
}

# Assign antigens to antigen families based on biology and add column to df
# assign_antigen_families <- function(standard_curve_study_data) {
#   antigen_families <- list(
#     "H1N1" = c("guangdong_53", "victoria_57"),
#     "H3N2" = c("a_darwin_14","hong_kong_55", "cambodia_63"),
#     "B/Victoria" = c("b_austria_18", "washington_72"),
#     "B/Yamagata" = c("phuket_78"),
#     "CMV" = c("g_b_12", "pentamer_15", "cg1_20", "cg2_61"),
#     "Pertussis" = c("pt_25", "fha_27", "prn_30"),
#     "Polio" = c("ipv1_38", "ipv2_51", "ipv3_67", "ipv_123_65"),
#     "SARS-CoV-2" = c("rbd_42", "s1_48","s2_45"),
#     "TT" = c("tt_21"))
#
#     standard_curve_study_data$antigen_family <- sapply(standard_curve_study_data$antigen, function(antigen) {
#       # Find the corresponding family for each antigen
#       family <- "No Family Specified"  # Default value if no match is found
#       for (fam in names(antigen_families)) {
#         if (antigen %in% antigen_families[[fam]]) {
#           family <- fam
#           break
#         }
#       }
#       return(family)
#     })
#
#     return(standard_curve_study_data)
# }

assign_antigen_families <- function(standard_curve_study_data, antigen_family_lookup) {
  cat("In assign antigen families begining")
  # antigen_family_lookup <<- antigen_family_lookup
  if (!all(c("antigen", "antigen_family") %in% colnames(antigen_family_lookup))) {
    stop("The antigen_family_lookup data frame must contain antigen and antigen_family columns.")
  }
  standard_curve_study_data_with_antigens <- merge(
    standard_curve_study_data,
    antigen_family_lookup[,c("antigen", "antigen_family")],
    by = "antigen",
    all.x = T
  )
  return(standard_curve_study_data_with_antigens)
}

# Takes in standard curve data from study and computes fitted df
compute_fitted_df <- function(standard_curve_study_data, min_log_dilution) {
 # standard_curve_study_data_v <<- standard_curve_study_data
  x_values <- seq(min_log_dilution, 0, length.out = 100)
  # x_values <- seq(-5, 0, by = 0.0001)
  # x_values <- round(x_values, 6)

  fitted_df <- data.frame()

  param_df <- standard_curve_study_data[,c("study_accession","experiment_accession", "source", "antigen","plateid", "crit", "l_asy", "r_asy", "x_mid", "scale", "g")]

  for (i in 1:nrow(param_df)) {
    if (param_df$crit[i] == "drda_5") {
      params <- param_df[i,]
      #print(params)

      fitted <- numeric(length(x_values))
      for (j in seq_along(x_values)) {
        fitted[j] <- calculate_fitted_drda5(x_values[j], params$l_asy, params$r_asy, params$x_mid, params$scale, params$g)
      }

      row_results <- data.frame(study_accession = param_df$study_accession[i], experiment_accession = param_df$experiment_accession[i],
                                antigen = param_df$antigen[i], model_id = param_df$plateid[i], source = param_df$source[i], x = x_values, fitted = fitted, crit = "drda_5")
      #
      # Append to the overall results
      fitted_df <- rbind(fitted_df, row_results)
    }  else if (param_df$crit[i] == "nls_5") {
      # Extract parameters from the current row
      params <- param_df[i, ]

      # Calculate fitted values for nls_5
      fitted <- numeric(length(x_values))
      for (j in seq_along(x_values)) {
        fitted[j] <- five_param_logistic(x_values[j], params$l_asy, params$r_asy, params$x_mid, params$scale, params$g)
      }

      # Create a data frame for the current row's results
      row_results <- data.frame(study_accession = param_df$study_accession[i], experiment_accession = param_df$experiment_accession[i],
                                antigen = param_df$antigen[i],model_id = param_df$plateid[i], source = param_df$source[i], x = x_values, fitted = fitted, crit = "nls_5")

      # Append to the overall results
      fitted_df <- rbind(fitted_df, row_results)
    } else if (param_df$crit[i] == "nls_4") {
      params <- param_df[i, ]

      # Calculate fitted values for nls 4
      fitted <- numeric(length(x_values))
      for (j in seq_along(x_values)) {
        fitted[j] <- five_param_logistic(x_values[j], params$l_asy, params$r_asy, params$x_mid, params$scale, 1)
      }

      # Create a data frame for the current row's results
      row_results <- data.frame(study_accession = param_df$study_accession[i], experiment_accession = param_df$experiment_accession[i],
                                antigen = param_df$antigen[i], model_id = param_df$plateid[i], source = param_df$source[i], x = x_values, fitted = fitted, crit = "nls_4")

      # Append to the overall results
      fitted_df <- rbind(fitted_df, row_results)
    } else if (param_df$crit[i] == "nls_exp") {
      params <- param_df[i, ]

      # Calculate fitted values for exp
      fitted <- numeric(length(x_values))
      for (j in seq_along(x_values)) {
        fitted[j] <- exponential_fit(x_values[j], params$scale, params$x_mid, params$l_asy)
      }

      # Create a data frame for the current row's results
      row_results <- data.frame(study_accession = param_df$study_accession[i], experiment_accession = param_df$experiment_accession[i],
                                antigen = param_df$antigen[i], model_id = param_df$plateid[i], source = param_df$source[i], x = x_values, fitted = fitted, crit = "nls_exp")

      # Append to the overall results
      fitted_df <- rbind(fitted_df, row_results)
    } else {
      fitted <- numeric(length(x_values))
      for (j in seq_along(x_values)) {
        fitted[j] <- NA
      }
      row_results <- data.frame(study_accession = param_df$study_accession[i], experiment_accession = param_df$experiment_accession[i],
                                antigen = param_df$antigen[i],model_id = param_df$plateid[i], source = param_df$source[i], x = x_values, fitted = fitted, crit = "null")

      # Append to the overall results
      fitted_df <- rbind(fitted_df, row_results)
    }
  }

 # fitted_df_v <<- fitted_df
  return(fitted_df)
}

# Calculate fits on fitted data aggregation, fitted = y, x = log_dilution
aggregate_standard_curves <- function(experiment_fitted_data, antigen_selection, min_log_dilution, g_value, bkg, is_log_mfi_axis) {
  x_values <- seq(min_log_dilution, 0, length.out = 1000)

  # Filter by antigen and rename columns
  experiment_fitted_data <- experiment_fitted_data[experiment_fitted_data$antigen == antigen_selection,]
  names(experiment_fitted_data)[names(experiment_fitted_data) == 'x'] <- 'log_dilution'
  names(experiment_fitted_data)[names(experiment_fitted_data) == 'fitted'] <- 'mfi'
  names(experiment_fitted_data)[names(experiment_fitted_data) == 'model_id'] <- 'plateid'
  experiment_fitted_data$plateid <- "aggregated"


  # Initialize dataframes
  refit_fit_df <- data.frame()
  predicted_agg_df <- data.frame()

  cat("before nls 5 compute in aggregate\n")
  print(names(experiment_fitted_data))

  nls_5_models <- compute_nls_5(experiment_fitted_data, g_value = g_value, bkg = bkg, is_log_mfi_axis = is_log_mfi_axis)

  if (length(nls_5_models) == 0) {
    cat("nls_5 models are null\n")

    drda_5_models <- compute_drda_5_param(experiment_fitted_data, bkg, is_log_mfi_axis = is_log_mfi_axis)
   # drda_5_models_v <<- drda_5_models
    refit_fit_df <- rbind(refit_fit_df, drda_5_models[[2]])

    if (nrow(refit_fit_df) == 1) {
      predicted_mfi_agg <- calculate_fitted_drda5(
        x_values,
        l_asy = as.numeric(refit_fit_df["alpha"]),
        r_asy = as.numeric(refit_fit_df["r_asy"]),
        x_mid = as.numeric(refit_fit_df["phi"]),
        scale = as.numeric(refit_fit_df["eta"]),
        g = as.numeric(refit_fit_df["nu"])
      )

      predicted_agg_df <- data.frame(
        mod_class = "drda_5",
        study_accesssion = rep(unique(experiment_fitted_data$study_accession), length(x_values)),
        experiment_accession = rep(unique(experiment_fitted_data$experiment_accession), length(x_values)),
        antigen = rep(unique(experiment_fitted_data$antigen), length(x_values)),
        log_dilution = x_values,
        norm_mfi = predicted_mfi_agg
      )
    }

    if (length(drda_5_models) == 0) {
      nls_4_models <- compute_nls_4(experiment_fitted_data, bkg = bkg, is_log_mfi_axis = is_log_mfi_axis)
      refit_fit_df <- rbind(refit_fit_df, nls_4_models[[2]])

      if (nrow(refit_fit_df) == 1) {
        predicted_mfi_agg <- five_param_logistic(
          x_values,
          l_asy = as.numeric(refit_fit_df["l_asy"]),
          r_asy = as.numeric(refit_fit_df["r_asy"]),
          xmid = as.numeric(refit_fit_df["x_mid"]),
          scal = as.numeric(refit_fit_df["scale"]),
          g = 1
        )

        predicted_agg_df <- data.frame(
          mod_class = "nls_4",
          study_accesssion = rep(unique(experiment_fitted_data$study_accession), length(x_values)),
          experiment_accession = rep(unique(experiment_fitted_data$experiment_accession), length(x_values)),
          antigen = rep(unique(experiment_fitted_data$antigen), length(x_values)),
          log_dilution = x_values,
          norm_mfi = predicted_mfi_agg
        )
      }

      if (length(nls_4_models) == 0) {
        cat("nls_4 models are null\n")
        exp_models <- compute_exponential_fit(experiment_fitted_data, bkg = bkg, is_log_mfi_axis = is_log_mfi_axis )
        refit_fit_df <- rbind(refit_fit_df, exp_models[[2]])

        if (nrow(refit_fit_df) == 1) {
          predicted_mfi_agg <- exponential_fit(
            x_values,
            scal = as.numeric(refit_fit_df["scale"]),
            xmid = as.numeric(refit_fit_df["x_mid"]),
            l_asy = as.numeric(refit_fit_df["l_asy"])
          )

          predicted_agg_df <- data.frame(
            mod_class = "nls_exp",
            study_accesssion = rep(unique(experiment_fitted_data$study_accession), length(x_values)),
            experiment_accession = rep(unique(experiment_fitted_data$experiment_accession), length(x_values)),
            antigen = rep(unique(experiment_fitted_data$antigen), length(x_values)),
            log_dilution = x_values,
            norm_mfi = predicted_mfi_agg
          )
        }

        if (length(exp_models) == 0) {
          predicted_agg_df <- data.frame()
        }
      }
    }
  } else {
    cat("5-parameter nls model fitted\n")
    refit_fit_df <- rbind(refit_fit_df, nls_5_models[[2]])

    if (nrow(refit_fit_df) == 1) {
      predicted_mfi_agg <- five_param_logistic(
        x_values,
        l_asy = as.numeric(refit_fit_df["l_asy"]),
        r_asy = as.numeric(refit_fit_df["r_asy"]),
        xmid = as.numeric(refit_fit_df["x_mid"]),
        scal = as.numeric(refit_fit_df["scale"]),
        g = as.numeric(refit_fit_df["g"])
      )

      predicted_agg_df <- data.frame(
        mod_class = "nls_5",
        study_accesssion = rep(unique(experiment_fitted_data$study_accession), length(x_values)),
        experiment_accession = rep(unique(experiment_fitted_data$experiment_accession), length(x_values)),
        antigen = rep(unique(experiment_fitted_data$antigen), length(x_values)),
        log_dilution = x_values,
        norm_mfi = predicted_mfi_agg
      )
    }
  }

  return(list(refit_fit_df, predicted_agg_df))
}


find_closest <- function(target, candidates) {
  # Find the index of the closest candidate
  idx <- which.min(abs(candidates - target))
  return(idx)
}

## Add normalized mfi to sample data for the antigens at the corresponding log dilution
preform_linear_interpolation <- function(sample_data, aggrigated_norm_mfi) {
  sample_data$log_dilution <- log10(sample_data$au)
  view_norm_mfi <- aggrigated_norm_mfi[[2]]

  interprolated_mfi_dilution_df <- approx(
    x = aggrigated_norm_mfi[[2]]$log_dilution,
    y = aggrigated_norm_mfi[[2]]$norm_mfi,
    xout = sample_data$log_dilution,
    method = "linear"
  )

  sample_data$norm_mfi <- interprolated_mfi_dilution_df$y

  view_sample_data <- sample_data

  return(sample_data)
}

# Save normalized MFI to the database
save_normalized_mfi <- function(sample_data_norm_mfi) {
  print("Before updating norm mfi")
  update_norm_mfi_df <- sample_data_norm_mfi[, c("xmap_sample_id", "experiment_accession", "norm_mfi")]

  cat("After initialization of update_norm_mfi")

  tmp_table_name <- "xmap_tmp_norm_mfi"
  if (dbExistsTable(conn, Id(schema = "madi_results", table = tmp_table_name))) {
    DBI::dbRemoveTable(conn, Id(schema = "madi_results", table = tmp_table_name))
  }

  cat("Write update_norm_mfi to database")
  DBI::dbWriteTable(conn, Id(schema = "madi_results", table = tmp_table_name), update_norm_mfi_df)
  DBI::dbExecute(conn, "UPDATE madi_results.xmap_sample
                   SET norm_mfi = xmap_tmp_norm_mfi.norm_mfi
                   FROM madi_results.xmap_tmp_norm_mfi
                   WHERE xmap_tmp_norm_mfi.xmap_sample_id = xmap_sample.xmap_sample_id;")
  showNotification(paste0("Updated normalized mfi values successfully for ", unique(update_norm_mfi_df$experiment_accession)),
                   type = "message")

  # Clean up by removing temporary table
  if(dbExistsTable(conn, Id(schema = "madi_results", table = tmp_table_name))) {
    DBI::dbRemoveTable(conn, Id(schema = "madi_results", table = tmp_table_name))
  }
}
#  sample_data$normalized_mfi <- NA
#  #result_df <<- merge(sample_data, aggrigated_norm_mfi[[2]], by = c("antigen", "log_dilution"), all.x = TRUE)
# sample_data$normalized_mfi <- with(sample_data, ifelse(
#   paste(antigen, log_dilution) %in% paste(aggrigated_norm_mfi[[2]]$antigen, aggrigated_norm_mfi[[2]]$log_dilution),
#   aggrigated_norm_mfi[[2]]$normalized_mfi[match(paste(antigen, log_dilution), paste(aggrigated_norm_mfi[[2]]$antigen, aggrigated_norm_mfi[[2]]$log_dilution))],
#   normalized_mfi
# ))


# result_df <<- sample_data

# return(sample_data)


# calculate r_asy
#r_asy <- as.numeric(model$coefficients["alpha"]) + as.numeric(model$coefficients["delta"])

# # Compute the normalized mfi values in the sample data for the experiment in the study from aggregated fit
# compute_normalizezd_mfi <- function(aggrigated_fit_in, sample_data, dat) {
#   min_log_dilution <- min(dat$log_dilution)
#   max_log_dilution <- max(dat$log_dilution) + abs(0 - max(dat$log_dilution))/2
#   sample_data$antibody_mfi <- sample_data$value_reported
#
#   aggrigated_fit_glance <- aggrigated_fit_in[1][[1]]
#   if (aggrigated_fit_glance["crit"] == "drda_5") {
#     sample_data_norm_mfi <- five_param_drda_logistic_norm_mfi(sample_data = sample_data, alpha = as.numeric(aggrigated_fit_glance["alpha"]),
#                                       delta = as.numeric(aggrigated_fit_glance["delta"]), eta = as.numeric(aggrigated_fit_glance["eta"]),
#                                       phi = as.numeric(aggrigated_fit_glance["phi"]),
#                                       nu = as.numeric(aggrigated_fit_glance["nu"]),
#                                       min_log_dilution = min_log_dilution,
#                                       max_log_dilution = max_log_dilution)
#   } else if (aggrigated_fit_glance["crit"] == "nls_5") {
#    sample_data_norm_mfi <- five_param_logistic_norm_mfi(sample_data = sample_data, l_asy = as.numeric(aggrigated_fit_glance["l_asy"]),
#                                                         r_asy = as.numeric(aggrigated_fit_glance["r_asy"]),
#                                                         xmid = as.numeric(aggrigated_fit_glance["x_mid"]),
#                                                         scal = as.numeric(aggrigated_fit_glance["scale"]),
#                                                         g = as.numeric(aggrigated_fit_glance["g"]),
#                                                         min_log_dilution = min_log_dilution, max_log_dilution = max_log_dilution)
#   } else if (aggrigated_fit_glance["crit"] == "nls_4") {
#     sample_data_norm_mfi <- four_param_logistic_norm_mfi(sample_data = sample_data, l_asy = as.numeric(aggrigated_fit_glance["l_asy"]),
#                                                          r_asy = as.numeric(aggrigated_fit_glance["r_asy"]),
#                                                          xmid = as.numeric(aggrigated_fit_glance["x_mid"]), scal = as.numeric(aggrigated_fit_glance["scale"]),
#                                                          min_log_dilution = min_log_dilution,
#                                                          max_log_dilution = max_log_dilution)
#   } else if (aggrigated_fit_glance["crit"] == "nls_exp") {
#     sample_data_norm_mfi <- exponential_fit_norm_mfi(sample_data = sample_data, l_asy = as.numeric(aggrigated_fit_glance["l_asy"]),
#                              xmid = as.numeric(aggrigated_fit_glance["x_mid"]), scal = as.numeric(aggrigated_fit_glance["scale"]),
#                              min_log_dilution = min_log_dilution)
#   }
#   return(sample_data_norm_mfi)
# }
#
# five_param_drda_logistic_norm_mfi <- function(sample_data, alpha, delta, eta, phi, nu, min_log_dilution, max_log_dilution) {
#   # Ensure that the input data has the necessary columns
#   dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi),
#                                  c("xmap_sample_id", "subject_accession", "antibody_mfi", "antigen", "plateid", "gc")]
#
#
#   dilution_mfi_df$norm_mfi <- ifelse(dilution_mfi_df$antibody_mfi < alpha & eta >= 0 & nu >= 0,
#                                         min_log_dilution,
#                                         ifelse(dilution_mfi_df$antibody_mfi > (delta + alpha),
#                                                0,
#                                                phi - (1 / eta) * log(((delta / (dilution_mfi_df$antibody_mfi - alpha))^nu - 1) / nu)))
#   # Return the modified dataframe with norm_mfi
#   return(dilution_mfi_df)
# }
#
#
# four_param_logistic_norm_mfi <- function(sample_data, l_asy, r_asy, xmid, scal, min_log_dilution, max_log_dilution) {
#   cat("Min Log dilution in 4 parameter")
#   print(min_log_dilution)
#
#   print("printing parameter values ")
#   print(l_asy)
#   print(r_asy)
#   print(xmid)
#   #print(scal)
#   print("Sample data names")
#   print(names(sample_data))
#
#   dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id","subject_accession", "antibody_mfi", "antigen", "plateid", "gc")]
#
#   # Calculate log dilution only for valid MFI values
#   dilution_mfi_df$norm_mfi <- ifelse(dilution_mfi_df$antibody_mfi >= r_asy,
#                                         0,
#                                         ifelse(dilution_mfi_df$antibody_mfi <= l_asy,
#                                                min_log_dilution,
#                                                xmid - scal * log((r_asy - l_asy) / (dilution_mfi_df$antibody_mfi - l_asy) - 1)))
#   return(dilution_mfi_df)
# }
#
# five_param_logistic_norm_mfi <- function(sample_data, l_asy, r_asy, xmid, scal, g, min_log_dilution, max_log_dilution) {
#
#   dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id","subject_accession", "antibody_mfi", "antigen", "plateid", "gc")]
#   cat("In 5 parameter logistic norm mfi")
#   print(head(dilution_mfi_df))
#
#
#   dilution_mfi_df$norm_mfi <- ifelse(dilution_mfi_df$antibody_mfi > r_asy,
#                                         0,
#                                         ifelse(dilution_mfi_df$antibody_mfi < l_asy,
#                                                 min_log_dilution,
#                                                xmid + scal * log(((l_asy - r_asy) / (dilution_mfi_df$antibody_mfi - r_asy))^(1/g) - 1)))
#
#   return(dilution_mfi_df)
# }
#
#
# # Exponential fit, solves for log dilution
# exponential_fit_norm_mfi <- function(sample_data, l_asy, xmid, scal, min_log_dilution) {
#   dilution_mfi_df <- sample_data[!is.na(sample_data$antibody_mfi) , c("xmap_sample_id","subject_accession", "antibody_mfi", "antigen", "plateid", "gc")]
#
#   # Calculate log dilution only for valid MFI values
#   dilution_mfi_df$norm_mfi <- ifelse(dilution_mfi_df$antibody_mfi <= l_asy,
#                                         min_log_dilution,
#                                         log((dilution_mfi_df$antibody_mfi - l_asy) / (scal - l_asy)) / xmid)
#
#   return(dilution_mfi_df)
# }

# Overlay Standard Curves
plot_standard_curves <- function(fitted_df_in, cv_dilutions, aggrigated_fit_df, antigen_selection, is_log_mfi_axis) {
  microviz_kelly_pallete <-  c("#f3c300","#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
                               "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
                               "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")

  # filter the fitted data and get the aggregated fit df
  fitted_df <- fitted_df_in[fitted_df_in$antigen == antigen_selection, ]
  aggrigated_fit_glance <- aggrigated_fit_df[1][[1]]
  aggrigated_fit_df <- aggrigated_fit_df[2][[1]] # contains the fitted norm mfi values to plot (x, predicted_mfi_agg)

  # add dash type based on model type for fitted df
  fitted_df$dash <- sapply(fitted_df$crit, function(crit) {
    switch(crit,
           "drda_5" = "solid",
           "nls_exp" = "dash",
           "nls_5" = "dot",
           "nls_4" = "dashdot",
           "longdash") # Default case
  })
  # add dash type for model type based on the aggregated fit
  aggrigated_fit_glance$dash <- sapply(aggrigated_fit_glance$crit, function(crit) {
    switch(crit,
           "drda_5" = "solid",
           "nls_exp" = "dash",
           "nls_5" = "dot",
           "nls_4" = "dashdot",
           "longdash") # Default case
  })

  # filter the cv dilutions to be just to the selected antigen
  cv_dilutions_antigen <- cv_dilutions[cv_dilutions$antigen == antigen_selection,]

  cv_dilutions_antigen$dilution_factor <- exp10(cv_dilutions_antigen$log_dilution)
  frac_parts <- frac_mat(cv_dilutions_antigen$dilution_factor)
  cv_dilutions_antigen$dilution_factor <- frac_parts["denominator",]

  # get axis text if log MFI or not
  mfi_text <- if (is_log_mfi_axis) {
    "log<sub>10</sub> MFI"
  } else {
    "MFI"
  }

  p <- plot_ly()

  for (plate in unique(fitted_df$model_id)) {
    sc_plate_data <- fitted_df[fitted_df$model_id == plate,]
    sc_plate_data <- sc_plate_data[order(sc_plate_data$x), ]

    # print(plate_data$dash)
    p <- p %>%
      add_trace(
        x = sc_plate_data$x,
        y = sc_plate_data$fitted,
        type = "scatter",
        mode = "lines",
        line = list(dash = unique(sc_plate_data$dash)),
        color = sc_plate_data$model_id, # color by plateid
        colors = microviz_kelly_pallete,
        name = paste(plate, sc_plate_data$crit) # Name trace by plateid and model class.
      )
  }

  p <- p %>%
    add_trace(
      x = aggrigated_fit_df$log_dilution,
      y = aggrigated_fit_df$norm_mfi,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "black",
        dash = unique(aggrigated_fit_glance$dash)
      ),
      name = paste("Aggregated Standard Curve", aggrigated_fit_glance$crit),
      text = ~paste0("Log Dilution: ", aggrigated_fit_df$log_dilution,
                     "<br>Normalized ", mfi_text, ": ", round(as.numeric(aggrigated_fit_df$norm_mfi), 4)),
      hoverinfo = "text"
    )

  # add cv dilutions to the plot as a trace
  p <- p %>%
    add_trace(
      x = cv_dilutions_antigen$log_dilution,
      y = cv_dilutions_antigen$cv_log_dilution,
      type = "scatter",
      mode = "markers",
      marker = list(color = "#8C70FF"),
      yaxis = "y2",
      xaxis = "x",#"x2", # x1 previously no second axis
      name = "Coefficient of Variation",
      text = ~paste0(
        #"<br>Antigen: ", cv_dilutions_antigen$antigen,
        "<br>Log Dilution: ", cv_dilutions_antigen$log_dilution,
        "<br>CV Log Dilution: ",round(as.numeric(cv_dilutions_antigen$cv_log_dilution),2), "%",
        "<br> Dilution Factor: ", cv_dilutions_antigen$dilution_factor
      ),
      hoverinfo = "text"
    )

  # Dummy invisible trace to activate xaxis2 (Dilution Factor)
  p <- p %>%
    add_trace(
      x = cv_dilutions_antigen$log_dilution,
      y = rep(NA, length(cv_dilutions_antigen$log_dilution)),  # invisible
      type = "scatter",
      mode = "lines",
      xaxis = "x2",
      showlegend = FALSE,
      hoverinfo = "none",
      line = list(color = 'rgba(0,0,0,0)')
    )

  # add range to align the x values of the dilution factor labels
  x_range <- range(fitted_df$x, na.rm = TRUE)  # full log_dilution range

  ax <- list(
    tickfont = list(color = "#8db600"),
    autorange = "reversed",
    overlaying = "x",
    side = "top",
    title = "Dilution Factor",
    type = "linear",  # linear because it's just a relabeling of the same log scale
  #  anchor = "y",
    tickmode = "array",
    tickvals = cv_dilutions_antigen$log_dilution,  # same positions as x
    ticktext = cv_dilutions_antigen$dilution_factor,
    fixedrange = TRUE,
    range = x_range
  )

  # ax <- list(
  #   tickfont = list(color = "#8db600"),
  #   autorange = "reversed",
  #   overlaying = "x",
  #   side = "top",
  #   title = "Dilution Factor",
  #   type = "log",
  #   fixedrange = T)


  # tickvals = cv_dilutions_antigen$dilution_factor,
  # ticktext = cv_dilutions_antigen$dilution_factor)

  ay <- list(
    tickfont = list(color = "#8C70FF"),
    overlaying = "y",
    side = "right",
    title = "Coefficient of Variation (%)",
    fixedrange = T)
  #range = c(0, max(cv_dilutions_antigen$cv_log_dilution) + 1))
  # scaleratio = 1, scaleanchor = "y",
  # constraintoward = "bottom")


  p <- p %>% layout(
    title  = list(text = paste0("Standard Curves for ", unique(fitted_df$antigen), " by Plate and Model Class"),y = 1),
    xaxis2 = ax,
    yaxis2 = ay,
    xaxis = list(title = "Dilution log<sub>10</sub>", fixedrange = T, range = x_range),
    yaxis = list(title = mfi_text, fixedrange = T),
    font = list(size = 12),
    legend = list(x = 1.2, y = 1),
    margin = list(t = 90, b = 80),
    autosize = T
  )
  #%>%
  # layout(
  # layout(plot_bgcolor='#e5ecf6',
  #         xaxis = list(
  #           zerolinecolor = '#FFFFFF',
  #           zerolinewidth = 2
  #        ))
  #        yaxis = list(
  #          zerolinecolor = '#ffff',
  #          zerolinewidth = 2,
  #          gridcolor = 'ffff')
  # )

  p
}

# Obtain mse and cv for models that are saved.
obtain_mse_cv <- function(standard_curve_fits){
  standard_curve_fits <- standard_curve_fits[!(is.na(standard_curve_fits$mse) | is.na(standard_curve_fits$cv)), ]
  df <- data.frame()
  for (row in 1:nrow(standard_curve_fits)) {
    model <- standard_curve_fits[row, c("experiment_accession", "plateid", "antigen", "mse", "cv")]
    df <- rbind(df, model)
  }

  return(df)

}

plot_mse_cv <- function(mse_cv_df) {
  microviz_kelly_pallete <-  c("#f3c300","#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
                               "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
                               "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")

  p <- plot_ly()

  for (plate in unique(mse_cv_df$plateid)) {
    sc_plate_data <- mse_cv_df[mse_cv_df$plateid == plate,]

    p <- p %>%
      add_trace(
        data = sc_plate_data,
        x = ~cv,
        y = ~mse,
        type = "scatter",
        mode = "markers",
        color = ~plateid,
        colors = microviz_kelly_pallete,
        text = ~paste0(
          "Feature: ",experiment_accession,
          "<br>Plate: ", plateid,
          "<br>Antigen: ", antigen,
          "<br>CV: ", cv,
          "<br>MSE: ", mse
        ),
        hoverinfo = "text",
        name = plate
      )
  }

  p <- p %>% layout(
    title  = "MSE vs Coefficent of Variation across Plates and Features",
    xaxis = list(title = "CV"),
    yaxis = list(title = "Mean Squared Error (MSE)"),
    font = list(size = 12)
  )

  p
}


# download fits for experiment
download_fits_experiment <- function(download_df, selected_study, selected_experiment) {
  download_df <- download_df[download_df$experiment_accession == selected_experiment,]
  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_study, "_",selected_experiment, "_fits_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download Standard Curve Fits Data for ",selected_experiment, " in ", selected_study),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}

# download sample data for experiment
download_sample_data_experiment <- function(download_df, selected_study, selected_experiment) {
  download_df <- download_df[download_df$experiment_accession == selected_experiment,]
  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_study, "_",selected_experiment, "_sample_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download Sample Data for ",selected_experiment, " in ", selected_study),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}

calculate_cv_log_dilution_platewise <- function(df) {
  df_log_dilution_cv <- data.frame(antigen = character(),
                                   log_dilution = numeric(),
                                   #  log_dilution_mean_antigen = numeric(),
                                   mean_log_dilution = numeric(),
                                   sd_log_dilution = numeric(),
                                   cv_log_dilution = numeric(),
                                   stringsAsFactors = FALSE)

  # unique combinations of antigens and dilutions
  for (antigen in unique(df$antigen)) {
    for (dilution in unique(df$log_dilution)) {

      # subset data for the current antigen and dilution across all plates
      subset_data_dilution_series <- df[df$antigen == antigen & df$log_dilution == dilution,]
      print(subset_data_dilution_series$mfi)
      # check if there is more than 1 row for the current dilution level
      if (nrow(subset_data_dilution_series) > 1) {
        mean_log_dilution <- mean(subset_data_dilution_series$mfi, na.rm = TRUE)
        sd_log_dilution <- sd(subset_data_dilution_series$mfi, na.rm = TRUE)
        cv_log_dilution <- (sd_log_dilution / mean_log_dilution) * 100

        # Add the original log_dilution along with calculated mean, sd, and CV
        current_combination <- data.frame(antigen = antigen,
                                          log_dilution = dilution,
                                          mean_log_dilution = mean_log_dilution,
                                          sd_log_dilution = sd_log_dilution,
                                          cv_log_dilution = cv_log_dilution)

        df_log_dilution_cv <- rbind(df_log_dilution_cv, current_combination)
      }
    }
  }

  return(df_log_dilution_cv)
}

plot_log_dilution_cv <- function(cv_dilutions, antigen_selection) {
  if (!is.null(antigen_selection)) {
    #cv_dilutions <- cv_dilutions[cv_dilutions$antigen == antigen_selection,]
    cv_dilutions <- subset(cv_dilutions, antigen %in% antigen_selection)
  }

  microviz_kelly_pallete <-  c("#f3c300","#875692","#f38400","#a1caf1","#be0032","#c2b280","#848482",
                               "#008856","#e68fac","#0067a5","#f99379","#604e97", "#f6a600",  "#b3446c" ,
                               "#dcd300","#882d17","#8db600", "#654522", "#e25822","#2b3d26","lightgrey")

  cv_dilutions$dilution_factor <- exp10(cv_dilutions$log_dilution)
  frac_parts <- frac_mat(cv_dilutions$dilution_factor)
  cv_dilutions$dilution_factor <- frac_parts["denominator",]

  p <- plot_ly()

  for (antigen in unique(cv_dilutions$antigen)) {
    antigen_data <- cv_dilutions[cv_dilutions$antigen == antigen,]

    p <- p %>%
      add_trace(
        data = antigen_data,
        x = ~log_dilution,
        y = ~cv_log_dilution,
        type = "scatter",
        mode = "markers",
        color = ~antigen,
        colors = microviz_kelly_pallete,
        text = ~paste0(
          "<br>Antigen: ", antigen,
          "<br>Log Dilution: ", log_dilution,
          "<br>CV Log Dilution: ", round(cv_log_dilution,2), "%",
          "<br> Dilution Factor: ", dilution_factor
        ),
        hoverinfo = "text"#,
        #  xaxis = "x1"
        # name = plate
      )
    # p <- p %>%
    #     add_trace(
    #       data = antigen_data,
    #       x = ~dilution_factor,
    #       y = numeric(0),  # Empty y data
    #       type = "scatter",
    #       mode = "markers",
    #       hoverinfo = "none",  # No hover info as there's no data
    #       xaxis = "x2"  # Use the second x-axis (for dilution_factor)
    #     )
  }

  p <- p %>% layout(
    title  = ifelse(is.null(antigen_selection),paste0("Coefficient of Variation by Log Dilution"),
                    paste0("Coefficient of Variation by Log Dilution for ", paste(antigen_selection, collapse = ", "))),
    xaxis = list(title = "Dilution log<sub>10</sub>",side = "bottom"),
    yaxis = list(title = "Coefficient of Variation (%)"),
    font = list(size = 12)
  )

  p

}


# cv log dilution data download
download_cv_log_dilution_data <- function(download_df, selected_study, selected_experiment, selected_antigen) {
  if (!is.null(selected_antigen)) {
    download_df <- download_df[download_df$antigen == selected_antigen,]
  }
  download_plot_data <- download_this(
    download_df,
    output_name = ifelse(is.null(selected_antigen),paste0(selected_study, "_",selected_experiment, "_cv_log_dilution_data"),
                         paste0(selected_study, "_",selected_experiment, "_", selected_antigen, "_cv_log_dilution_data")),
    output_extension = ".xlsx",
    button_label = ifelse(is.null(selected_antigen),
                          paste0("Download Coefficent of Variation by Log Dilution and Log Dilution Data for ", selected_experiment, " in ", selected_study),
                          paste0("Download Coefficent of Variation by Log Dilution and Log Dilution Data for ", selected_antigen, " ", selected_experiment, " in ", selected_study)),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}



