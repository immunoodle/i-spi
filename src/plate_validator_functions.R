looks_like_file_path <- function(x) {
  # TRUE if:
  # - contains a directory separator ("/" for Unix/Mac, "\\" for Windows), OR
  # - starts with a drive letter on Windows (e.g. "C:/"), AND
  # - ends with a file extension (e.g. ".csv", ".txt")
  (grepl("[/\\\\]", x) | grepl("^[A-Za-z]:[\\\\/]", x)) ##&& grepl("\\.[A-Za-z0-9]+$", x)
}

#looks_like_file_path(meta_df_view$file_name)
# looks_like_file_path("Untitled")

# Validate the RP1 Volts and RP1 Target
# Only one decimal point is allowed
check_rp1_numeric <- function(x) {
  grepl("^\\d+(\\.\\d+)?$", x)
}

## Validate Time is in correct format to store in database
#DD-MMM-YYYY, HH:MM AM/PM
check_time_format <- function(x) {
  grepl("^\\d{2}-[A-Za-z]{3}-\\d{4}, \\d{2}:\\d{2} (AM|PM)$", x)
}

# Capitalize AM /PM
capitalize_am_pm <- function(text) {
  matches <- gregexpr("\\b(am|pm)\\b", text, ignore.case = TRUE, perl = TRUE)
  regmatches(text, matches) <- lapply(regmatches(text, matches), toupper)
  return(text)
}
## Validate the required variables present in metadata
# validate_metadata_variables <- function(df) {
#   required_vars <- c(
#     "file_name", "acquisition_date", "plateid", "plate_id", "plate"
#   )
#
#   missing_vars <- setdiff(required_vars, names(df))
#
#   if (length(missing_vars) > 0) {
#     message <- paste("The following required variables are missing from the plate metadata:",
#                paste(missing_vars, collapse = ", "))
#    return(list(FALSE, message))
#   } else {
#     return(list(TRUE))
#   }
# }

## Primary dataset
# Type column must be in correct format
check_type_column <- function(df) {
bad_rows <- df[!grepl("^[BXCS][0-9]*$", df$Type), ]

if (nrow(bad_rows) > 0) {
  message <-  paste(
    "Need to correct the type column for the following entries: Well",
    paste(bad_rows$Well, "| Value:", bad_rows$Type, collapse = ", ")
  )
  #cat("need to correct type column for the following entries:", paste(bad_rows$Type, collapse = ","))
  return(list(FALSE, message))
} else {
  return(list(TRUE))
}

}

#check_type_column(plte_data_v)


# if false procceed
check_blank_in_sample_boolean <- function(df) {
bad_rows <- which(
  grepl("Blank", df$Description, ignore.case = TRUE) & !grepl("^B", df$Type)
)

if (length(bad_rows) > 0) {
  #cat("Upload blocked: Found 'Blank' in description with type starting not with 'B'.\n")
  return(FALSE)
} else {
  return(TRUE)
}
}

#check_blank_in_sample_boolean(plte_data_v)

# check general description pattern for datatypes
check_sample_description <- function(df) {

 # check description pattern for the samples
  df <- df[grepl("^X", df$Type) &
             !grepl("Blank", df$Description) &
             !grepl("[A-Za-z0-9]+[ _/\\\\:;|\\-][A-Za-z0-9]+[ _/\\\\:;|\\-][A-Za-z0-9]+", df$Description), ]
  # df <- df[grepl("^X", df$Type) & !grepl("^\\[A-Za-z0-9_]+_[A-Za-z0-9_]+_\\[A-Za-z0-9_]+$", df$Description), ]
  if (nrow(df) > 0) {
     sample_message <- paste(
      "Need to modify the Sample description column to include a minimum of [ID]_[timeperiod]_[dilution_factor]: Well",
      paste(df$Well, "| Value:", df$Description, collapse = ", ")
    )
     return(list(FALSE, sample_message))
  } else {
    return(TRUE)
  }
}

check_standard_description <- function(df) {
  # check description pattern for the standards
  df <- df[grepl("^S", df$Type) &
             !grepl("Blank", df$Description) &
             !grepl("^[A-Za-z0-9_]+_\\d+$", df$Description), ]
  if (nrow(df) > 0) {
    standards_message <- paste(
      "Need to modify the Standard description column to be [source]_[dilution_factor] e.g. NIBSC_40: Well",
      paste(df$Well, "| Value:", df$Description, collapse = ", ")
    )
    return(list(FALSE, standards_message))
  } else {
    return(TRUE)
  }
}

# check description of the controls
# check_control_description <- function(df){
#   df <- df[grepl("^C", df$Type) & !grepl("", df$Description).]
# }

# check description of the blank
check_blank_description <- function(df) {
  df <- df[grepl("^B", df$Type) &
             !grepl("Blank", df$Description) &
             !grepl("^[A-Za-z0-9]+_\\d+$", df$Description),]
  if(nrow(df) > 0) {
    blank_message <- paste("Need to modify the Blank description column to be like [source]_[dilution_factor] e.g. PBS_1: Well",
                           paste(df$Well, "| Value:", df$Description, collapse = ", ")
                           )
    return(list(FALSE, blank_message))
  } else {
    return(TRUE)
  }
}

# return type differs
check_blank_description_batch <- function(df) {
  df <- df[grepl("^B", df$Type) &
             !grepl("Blank", df$Description) &
             !grepl("^[A-Za-z0-9]+_\\d+$", df$Description),]
  if(nrow(df) > 0) {
    blank_message <- paste("Need to modify the Blank description column to be like [source]_[dilution_factor] e.g. PBS_1: Well",
                           paste(df$Well, "| Value:", df$Description, collapse = ", ")
    )
    return(list(FALSE, blank_message))
  } else {
    return(list(TRUE))
  }

}


# blank keyword can either be 'empty_well' or 'use_as_blank'
check_blank_in_sample <- function(df, blank_keyword) {
  # Find rows where description contains "Blank" and type starts with not B
  bad_rows <- which(
    grepl("Blank", df$Description, ignore.case = TRUE) & !grepl("^B", df$Type)
  )

  if (length(bad_rows) > 0) {
    #cat("Upload blocked: Found 'Blank' in description with type starting not with 'B'.\n")
    # print(df[bad_rows, ])
    # Ask user to choose replacement keyword
    # repeat {
    #   choice <- readline(
    #     "Replace 'Blank' with one of: 'empty_well' or 'use_as_blank': "
    #   )
    #   if (choice %in% c("empty_well", "use_as_blank")) break
    #   cat("Invalid choice. Please type 'empty_well' or 'use_as_blank'.\n")
    # }

    # df$description[bad_rows] <- choice

    # If use_as_blank -> set type = "B"
    if (blank_keyword == "use_as_blank") {
      df$Type[bad_rows] <- "B"
    }

    # If empty_well -> drop rows
    if (blank_keyword == "empty_well") {
      #  df <- df[-bad_rows, ]
      #df <- df[df$description != "Blank" & df$type != "B",]
      df <- df[!(grepl("^Blank", df$Description, ignore.case = TRUE) & !grepl("^B", df$Type)),]

    }

    return(df)
  } else {

    cat("Check passed: No invalid 'Blank' entries with type 'B'.\n")
    #invisible(TRUE)
    return(df)
  }
}

#skip_empty_well <- check_blank_in_sample(plte_data_v, blank_keyword = "empty_well")
#replace_blank <- check_blank_in_sample(plte_data_v, blank_keyword = "use_as_blank")

check_agg_bead_column <- function(df) {
  required_cols <- c("X..Agg.Beads")
  result <- required_cols %in% names(df)
  if (!result) {
    message <- "Ensure there is a % Agg Beads column after the last antigen."
    return(list(result, message))
  } else {
    return(result)
  }
}

check_batch_agg_bead_column <- function(df) {
  required_cols <- c("% Agg Beads", "X..Agg.Beads", "%.Agg.Beads")
  result <- any(required_cols %in% names(df))

  if (!result) {
    message <- "Ensure there is a % Agg Beads column after the last antigen."
    return(list(result = result, message = message))
  } else {
    return(list(result = result, message = NULL))
  }
}

check_bead_count <- function(df) {

start_col <- which(names(df) == "Description")
possible_end_names <- c("% Agg Beads", "X..Agg.Beads", "%.Agg.Beads")
end_col <- which(names(df) %in% possible_end_names)
# end_col <- which(names(df) == "X..Agg.Beads")

# 2. Subset the columns of interest
subset_df <- df[, (start_col + 1):(end_col-1)]

# 3. Apply regex check to all cells in those columns space care about ^\\d+(\\.\\d+)? \\(\\d+\\)$
#    This creates a logical matrix (same size as subset_df)
match_matrix <- apply(subset_df, 2, function(col) {
  grepl("^\\d+(\\.\\d+)?\\s*\\(\\d+\\)$", col)
})

if (all(match_matrix)) {
  return(list(TRUE))
} else {
  failed_positions <- which(!match_matrix, arr.ind = TRUE)

  # 2. Get the actual failed values
  failed_values <- subset_df[failed_positions]

  # 3. Combine row, column, and value into a data frame
  failed_report <- data.frame(
    row = failed_positions[, "row"],
    antigen = sub("\\..*", "", colnames(subset_df)[failed_positions[, "col"]]),
    value = failed_values,
    stringsAsFactors = FALSE
  )

  msg <- apply(failed_report, 1, function(x) {
    paste0("Row ", x["row"],
           " | Antigen: ", x["antigen"],
           " | Value: ", x["value"])
  })

  final_message <- paste(msg, collapse = "\n")

  return(list(all(match_matrix), final_message))
}

}



# check_bead_count(plte_data_v)
#
# plate_modified_data <- plte_data_v[1,]
#  plate_modified_data$PT..75. <- "32"
#  plate_modified_data <- rbind(plate_modified_data, plte_data_v)
#
# check_bead_count(plate_modified_data)

# Overall function to call plate validation
plate_validation <- function(plate_metadata, plate_data, blank_keyword) {
  message_list <- c()

  # validate the required columns
  required_cols <- c("file_name", "rp1_pmt_volts", "rp1_target", "acquisition_date")
  missing_cols <- setdiff(required_cols, names(plate_metadata))

  if (length(missing_cols) > 0) {
    message_list <- c(
      message_list,
      paste("The following required plate metadata columns are missing so further parsing cannot be conducted:",
            paste(missing_cols, collapse = ", "))
    )
    # If critical metadata is missing, return early
    return(list(
      is_valid = FALSE,
      messages = message_list
    ))
  }

  # pass_required_metadata_variables <- validate_metadata_variables(plate_metadata)
  # if (!pass_required_metadata_variables[[1]]) {
  #   message_list <- c(message_list, pass_required_metadata_variables[[2]])
  # }

  # check to see if it passes file Path
  pass_file_path <- looks_like_file_path(plate_metadata$file_name)
  if (!pass_file_path) {
     message_list <- c(message_list, "Ensure the file path has foward or backward slashes based on Mac or Windows")
  }

  pass_rp1_pmt_volts <- check_rp1_numeric(plate_metadata$rp1_pmt_volts)
  if (!pass_rp1_pmt_volts) {
    message_list <- c(message_list, paste("Ensure that the RP1 PMT (Volts) field is numeric and if it is a decimal only one period is present. Value:",plate_metadata$rp1_pmt_volts, sep = " "))
  }

  pass_rp1_target <- check_rp1_numeric(plate_metadata$rp1_target)
  if (!pass_rp1_target) {
    message_list <- c(message_list, paste("Ensure that the RP1 Target is numeric and if it is a decimal only one period is present. Value:", plate_metadata$rp1_target, sep = " "))
  }

  pass_time_format <- check_time_format(capitalize_am_pm(plate_metadata$acquisition_date))
  if (!pass_time_format) {
    message_list <- c(message_list, paste("Ensure the acquisition date is in the following date time format: DD-MMM-YYYY, HH:MM AM/PM Example: 01-Oct-2025, 12:12 PM  |Current Value:",
                                          plate_metadata$acquisition_date, sep = " "))
  }


  # validate main data set

  pass_type_col <- check_type_column(plate_data)
  if (!pass_type_col[[1]]) {
    message_list <- c(message_list, pass_type_col[[2]])
  }
  pass_description <- check_sample_description(plate_data)
  if (!pass_description[[1]]) {
    message_list <- c(message_list, pass_description[[2]])
  }

  pass_standard_description <- check_standard_description(plate_data)
  if (!pass_standard_description[[1]]) {
    message_list <- c(message_list, pass_standard_description[[2]])
  }

  pass_agg_bead_check <- check_agg_bead_column(plate_data)
  if (!pass_agg_bead_check[[1]]) {
    message_list <- c(message_list, pass_agg_bead_check[[2]])
  } else {
    # check blanks if aggregate column is present
    pass_bead_count_check <- check_bead_count(plate_data)
    if (!pass_bead_count_check[[1]]) {
      message_list <- c(message_list, paste("Ensure the bead count is present after all MFI values in parentheses for: \n", pass_bead_count_check[[2]], sep = ""))
    }
  }
 # examine blanks in type column
 procceed_to_blank_check <- check_blank_in_sample_boolean(plate_data)
 if (!procceed_to_blank_check) {
    # Update Plate Data based on keyword choice
    plate_data <- check_blank_in_sample(plate_data, blank_keyword = blank_keyword)
 }

 # if blanks are processed still check it
 pass_blank_description <- check_blank_description(plate_data)
 if (!pass_blank_description[[1]]) {
   message_list <- c(message_list, pass_blank_description[[2]])
 }

 # if no invalid messages then it is good to pass
 is_valid <- length(message_list) == 0

 if (is_valid)  {
   return(list(
     is_valid = is_valid,
     messages = message_list,
     updated_plate_data = plate_data
   ))
} else {
  return(list(
    is_valid = is_valid,
    messages = message_list
  ))
}
 # if (pass_file_path && pass_bead_count_check[[1]] && procceed_to_blank_check) {
 #   return(list(
 #     is_valid = TRUE
 #   )
 #   )
 # } else {
 #
 # return(list(
 #   is_valid = FALSE,
 #   messages = message_list,
 #   updated_plate_data = plate_data
 # ))
 #
 # }
}

#  validation_result <- plate_validation(plate_metadata = meta_df_view, plate_data = plte_data_v, blank_keyword = "empty_well")
#  validation_result
# #validation_result$is_valid
# # is.null(validation_result$messages)
# #
# # validation_result <- plate_validation(plate_metadata = meta_df_view, plate_data = plte_data_v, blank_keyword = "use_as_blank")
# # validation_result
# #
#  validation_result <- plate_validation(plate_metadata = meta_df_view, plate_data = plate_modified_data, blank_keyword = "use_as_blank")
#  validation_result # no updated_plate_data in list


createValidateBadge <- function(is_validated) {

  if (is_validated) {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste("Plate Validated", sep = ""))
    )
  } else {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), "Plate Not Validated")
    )
  }
}

createValidateBatchBadge <- function(is_validated) {

  if (is_validated) {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste("Batch Validated", sep = ""))
    )
  } else {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), "Batch Not Validated")
    )
  }
}
createUploadedBatchBadge <- function(is_uploded) {

  if (is_uploded) {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste("Batch Uploaded", sep = ""))
    )
  } else {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), "Batch Not Uploaded")
    )
  }
}

createOptimizedBadge <- function(is_optimized) {

  if (is.null(is_optimized) || length(is_optimized) == 0 || !isTRUE(is_optimized)) {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), "Plate Not Optimized")
    )
  } else {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste("Plate Optimized", sep = ""))
    )
  }
  # if (isTRUE(is_optimized)) {
  #   # Completed Upload badge (green)
  #   span(
  #     class = "badge",
  #     style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
  #              background-color: #28a745; color: white;",
  #     tagList(tags$i(class = "fa fa-check"), paste("Plate Optimized", sep = ""))
  #   )
  # } else {
  #   # Not Uploaded badge (grey)
  #   span(
  #     class = "badge",
  #     style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
  #              background-color: #6c757d; color: white;",
  #     tagList(tags$i(class = "fa fa-exclamation-circle"), "Plate Not Optimized")
  #   )
  # }
}
