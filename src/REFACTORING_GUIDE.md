# Description Field Validation Refactoring Guide
# ================================================

## Overview

This document describes the refactoring needed to handle cases where the Description 
field in plate files is blank or has insufficient elements. The refactoring ensures
the app doesn't crash and instead:

1. Notifies the user about insufficient Description content
2. Hides irrelevant UI controls when Description is blank
3. Applies default values for required fields
4. Allows manual update of the layout template after creation

## Files Requiring Changes

### 1. app.R - Add new reactive value

Add this reactive value in the server function, near line 868 (after `sample_dilution_plate_df`):

```r
# NEW: Track Description field status across uploaded plates
description_status <- reactiveVal(list(
  has_content = TRUE,
  has_sufficient_elements = TRUE,
  min_elements_found = 0,
  required_elements = 3,  # PatientID, TimePeriod, DilutionFactor at minimum
  checked = FALSE,
  message = NULL
))
```

### 2. plate_validator_functions.R or batch_layout_functions.R - Add helper functions

Add the following functions from `description_helper_functions.R`:

- `check_description_elements()` - Checks if Description has content and sufficient elements
- `apply_default_values_to_plates_map()` - Applies defaults when Description is insufficient  
- `parse_description_with_defaults()` - Parses Description with fallback to defaults
- `validate_batch_description()` - Validates Description across all batch files

### 3. import_lumifile.R - Major changes required

#### A. Add output conditionals for UI visibility (around line 354)

```r
# Output for Description content status (for conditional panels)
output$descriptionHasContent <- reactive({
  status <- description_status()
  return(isTRUE(status$has_content))
})
outputOptions(output, "descriptionHasContent", suspendWhenHidden = FALSE)

# Output for Description sufficient elements status (for conditional panels)
output$descriptionHasSufficientElements <- reactive({
  status <- description_status()
  return(isTRUE(status$has_sufficient_elements))
})
outputOptions(output, "descriptionHasSufficientElements", suspendWhenHidden = FALSE)
```

#### B. Modify UI in renderUI for "readxMapData" (around line 206-275)

Wrap the Description delimiter controls in a conditionalPanel:

```r
conditionalPanel(
  condition = "output.descriptionHasContent",
  tags$div(
    class = "element-controls",
    tags$span(style = "font-weight: 600; align-self: center;", "Description Delimiter:"),
    radioGroupButtons(
      inputId = "description_delimiter",
      # ... rest of control
    )
  )
)
```

Similarly wrap the optional elements checkboxes:

```r
conditionalPanel(
  condition = "output.descriptionHasSufficientElements",
  tags$div(
    class = "element-controls",
    tags$span(style = "font-weight: 600; align-self: center;", "Include Optional Elements:"),
    checkboxGroupButtons(
      inputId = "optional_elements",
      # ... rest of control
    )
  )
)
```

And wrap the element order inputs:

```r
conditionalPanel(
  condition = "output.descriptionHasSufficientElements",
  uiOutput("order_input_ui")
),
conditionalPanel(
  condition = "output.descriptionHasContent",
  uiOutput("bcsorder_input_ui")
)
```

#### C. Add warning UI output (around line 245)

```r
output$description_warning_ui <- renderUI({
  status <- description_status()
  
  if (!status$checked) {
    return(NULL)
  }
  
  if (!status$has_content) {
    # Warning for completely blank Description
    tags$div(
      style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
        " Description Field is Blank",
        style = "margin-top: 0; color: #856404;"
      ),
      tags$p("The Description field in your plate data is empty. Default values will be used:"),
      tags$ul(
        tags$li("Subject ID: '1'"),
        tags$li("Sample Dilution Factor: 1"),
        tags$li("Timeperiod: 'T0'"),
        tags$li("Groups: 'Unknown'")
      ),
      tags$p(
        tags$strong("You will need to manually update the layout template with correct values before uploading."),
        style = "margin-bottom: 0; color: #856404;"
      )
    )
  } else if (!status$has_sufficient_elements) {
    # Warning for insufficient elements
    tags$div(
      style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
        " Insufficient Description Elements",
        style = "margin-top: 0; color: #856404;"
      ),
      tags$p(sprintf(
        "The Description field has only %d element(s), but at least %d are required.",
        status$min_elements_found,
        status$required_elements
      )),
      tags$p("Missing fields will be filled with default values. Manual update may be required.")
    )
  } else {
    return(NULL)
  }
})
```

Add `uiOutput("description_warning_ui")` in the UI around line 273.

#### D. Modify upload_experiment_files observer (around line 1111)

After building combined plate data, add Description check:

```r
# Check Description field status after loading plate data
cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║         CHECKING DESCRIPTION FIELD                       ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")

# Get the current delimiter (default to underscore)
delimiter <- if (!is.null(input$description_delimiter)) input$description_delimiter else "_"

# Check description elements
desc_check <- check_description_elements(
  plate_data = all_plates,
  delimiter = delimiter,
  required_elements = 3
)

# Update the reactive
description_status(list(
  has_content = desc_check$has_content,
  has_sufficient_elements = desc_check$has_sufficient_elements,
  min_elements_found = desc_check$min_elements_found,
  required_elements = desc_check$required_elements,
  checked = TRUE,
  message = desc_check$message
))

# Show notifications
if (!desc_check$has_content) {
  showNotification(
    "Description field is blank. Default values will be used.",
    type = "warning",
    duration = 10
  )
} else if (!desc_check$has_sufficient_elements) {
  showNotification(
    sprintf("Description has insufficient elements (%d found, %d required).",
            desc_check$min_elements_found, desc_check$required_elements),
    type = "warning",
    duration = 10
  )
}
```

#### E. Add observer for delimiter changes (after line 1256)

```r
observeEvent(input$description_delimiter, {
  req(batch_plate_data())
  
  all_plates <- batch_plate_data()
  delimiter <- input$description_delimiter
  
  # Re-check description elements with new delimiter
  desc_check <- check_description_elements(
    plate_data = all_plates,
    delimiter = delimiter,
    required_elements = 3
  )
  
  # Update the reactive
  description_status(list(
    has_content = desc_check$has_content,
    has_sufficient_elements = desc_check$has_sufficient_elements,
    min_elements_found = desc_check$min_elements_found,
    required_elements = desc_check$required_elements,
    checked = TRUE,
    message = desc_check$message
  ))
}, ignoreInit = TRUE)
```

#### F. Modify downloadHandler for blank_layout_file (around line 360-407)

Pass description_status to generate_layout_template:

```r
desc_status <- description_status()

generate_layout_template(
  all_plates = all_plates,
  study_accession = input$readxMap_study_accession,
  experiment_accession = input$readxMap_experiment_accession_import,
  n_wells = input$n_wells_on_plate,
  header_list = bead_array_header_list(),
  output_file = file,
  # NEW: Pass description status for handling defaults
  description_status = desc_status,
  delimiter = if (desc_status$has_content) input$description_delimiter else "_",
  element_order = if (desc_status$has_sufficient_elements) input$XElementOrder else c("PatientID", "TimePeriod", "DilutionFactor"),
  bcs_element_order = if (desc_status$has_content) input$BCSElementOrder else c("Source", "DilutionFactor")
)

# Show notification if defaults were applied
if (!desc_status$has_content || !desc_status$has_sufficient_elements) {
  showNotification(
    "Layout template generated with default values. Please review and update before uploading.",
    type = "warning",
    duration = 10
  )
}
```

### 4. batch_layout_functions.R - Modify generate_layout_template

The generate_layout_template function needs to:

1. Accept new parameters: `description_status`, `delimiter`, `element_order`, `bcs_element_order`
2. Use `parse_description_with_defaults()` when parsing Description
3. Call `apply_default_values_to_plates_map()` after creating plates_map
4. Handle cases where Description is blank gracefully

## Default Values Applied

When Description is blank or insufficient:

| Field | Default Value |
|-------|---------------|
| subject_id | "1" |
| specimen_dilution_factor | 1 |
| timeperiod_tissue_abbreviation | "T0" |
| groupa | "Unknown" |
| groupb | "Unknown" |

## Testing Scenarios

1. **Normal case**: Description has all required elements (5 elements for full parsing)
   - UI shows all controls
   - No warnings displayed
   - All values parsed from Description

2. **Partial case**: Description has some elements (1-2 elements)
   - UI shows delimiter controls only
   - Warning about insufficient elements
   - Some values parsed, others get defaults

3. **Empty case**: Description is completely blank
   - UI hides all Description-related controls
   - Warning about blank Description
   - All sample wells get default values

4. **Mixed case**: Some wells have Description, others don't
   - UI shows controls (at least some content exists)
   - Warning if minimum elements across all wells is insufficient
   - Individual wells with blank Description get defaults

## User Workflow

1. User uploads plate files
2. App checks Description field automatically
3. If Description is blank/insufficient:
   - Warning message appears
   - Irrelevant UI controls are hidden
4. User generates layout template (with defaults applied)
5. User downloads template
6. User manually updates template with correct values
7. User uploads completed template
8. Validation proceeds normally
