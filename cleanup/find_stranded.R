#!/usr/bin/env Rscript
# find_stranded.R - Static analysis for dead code detection
# Works with complex Shiny apps without requiring sourcing

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Usage: Rscript find_stranded.R <path-to-app>")
}
app_dir <- normalizePath(args[1], mustWork = TRUE)

cat("[find_stranded] Scanning app at", app_dir, "\n")

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

# ============================================================================
# 1. Find all R files
# ============================================================================
src_dir <- file.path(app_dir, "src")
r_files <- list.files(src_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

# Also check app root for app.R, server.R, ui.R, global.R
root_r_files <- list.files(app_dir, pattern = "\\.(R|r)$", full.names = TRUE, recursive = FALSE)
r_files <- unique(c(r_files, root_r_files))

cat("[static] Found", length(r_files), "R source files\n")

# ============================================================================
# 2. Collect function DEFINITIONS using regex
# ============================================================================
collect_function_definitions <- function(file) {
  lines <- tryCatch(readLines(file, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) NULL)
  if (is.null(lines) || length(lines) == 0) return(NULL)

  # Pattern: name <- function(...) or name = function(...)
  # Handles optional whitespace and different assignment operators
  pattern <- "^\\s*([a-zA-Z][a-zA-Z0-9_.]*)\\s*(<-|=)\\s*function\\s*\\("

  matches <- grep(pattern, lines, perl = TRUE)
  if (length(matches) == 0) return(NULL)

  # Extract function names properly

  fn_names <- sapply(lines[matches], function(line) {
    m <- regexec(pattern, line, perl = TRUE)
    match_result <- regmatches(line, m)[[1]]
    if (length(match_result) >= 2) {
      return(match_result[2])
    }
    return(NA_character_)
  }, USE.NAMES = FALSE)

  valid_idx <- !is.na(fn_names) & fn_names != ""
  if (!any(valid_idx)) return(NULL)

  tibble(
    file = file,
    name = fn_names[valid_idx],
    line = matches[valid_idx]
  )
}

# Collect all function definitions
fn_defs <- map_dfr(r_files, collect_function_definitions)

if (is.null(fn_defs) || nrow(fn_defs) == 0) {
  cat("[static] ERROR: No functions found. Check your src/ directory.\n")
  quit(status = 1)
}

cat("[static] Discovered", nrow(fn_defs), "function definitions\n")

# Get unique function names
all_defined_functions <- unique(fn_defs$name)
cat("[static] Unique function names:", length(all_defined_functions), "\n")

# ============================================================================
# 3. Collect function CALLS across all files
# ============================================================================
collect_function_calls <- function(file, known_functions) {
  lines <- tryCatch(readLines(file, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) NULL)
  if (is.null(lines) || length(lines) == 0) return(character())

  # Combine all lines into one text block for analysis
  full_text <- paste(lines, collapse = "\n")

  # Remove comments (single line)
  full_text <- gsub("#[^\n]*", "", full_text)

  # Remove string literals to avoid false positives
  full_text <- gsub('"[^"]*"', '""', full_text)
  full_text <- gsub("'[^']*'", "''", full_text)

  # Find all potential function calls: word followed by (
  # Pattern matches: function_name( but not things like if(, for(, while(
  call_pattern <- "\\b([a-zA-Z][a-zA-Z0-9_.]*)\\s*\\("

  all_matches <- gregexpr(call_pattern, full_text, perl = TRUE)
  matched_texts <- regmatches(full_text, all_matches)[[1]]

  # Extract just the function names (remove the parenthesis)
  called_names <- gsub("\\s*\\($", "", matched_texts)

  # Filter to only known functions (the ones we defined)
  called_known <- intersect(called_names, known_functions)

  return(unique(called_known))
}

# Collect all calls from all files
all_calls_list <- map(r_files, ~collect_function_calls(.x, all_defined_functions))
all_called_functions <- unique(unlist(all_calls_list))

cat("[static] Found calls to", length(all_called_functions), "of our defined functions\n")

# ============================================================================
# 4. Identify Shiny Entry Points
# ============================================================================
# These patterns indicate functions that Shiny calls automatically
shiny_entry_patterns <- c(
  "^server$", "^ui$", "^app$",
  "^render", "^observe", "^reactive",
  "^module.*Server$", "^module.*UI$",
  "Server$", "UI$", "Input$", "Output$"
)

# UI constructors that indicate entry points
ui_constructors <- c(
  "fluidPage", "navbarPage", "dashboardPage", "sidebarLayout",
  "tagList", "tabsetPanel", "tabPanel", "shinyUI", "shinyServer",
  "shinyApp", "runApp"
)

# Find functions used in reactive contexts (these are entry points)
find_reactive_entries <- function(file) {
  lines <- tryCatch(readLines(file, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) NULL)
  if (is.null(lines)) return(character())

  text <- paste(lines, collapse = "\n")

  # Functions assigned to output$ are entry points
  output_pattern <- "output\\$[a-zA-Z0-9_]+\\s*<-\\s*render[A-Za-z]+\\s*\\(\\s*\\{[^}]*\\b([a-zA-Z][a-zA-Z0-9_.]*)\\s*\\("

  # Functions inside observeEvent, observe, reactive are entry points
  reactive_patterns <- c(
    "observeEvent\\s*\\([^,]+,\\s*\\{[^}]*\\b([a-zA-Z][a-zA-Z0-9_.]*)\\s*\\(",
    "observe\\s*\\(\\s*\\{[^}]*\\b([a-zA-Z][a-zA-Z0-9_.]*)\\s*\\(",
    "reactive\\s*\\(\\s*\\{[^}]*\\b([a-zA-Z][a-zA-Z0-9_.]*)\\s*\\("
  )

  entries <- character()
  for (pat in reactive_patterns) {
    m <- gregexpr(pat, text, perl = TRUE)
    if (m[[1]][1] != -1) {
      matches <- regmatches(text, m)[[1]]
      # This is simplified - in practice we'd need more sophisticated parsing
    }
  }

  entries
}

# Mark potential entry points
is_likely_entry_point <- function(fn_name) {
  # Check against Shiny patterns
  for (pat in shiny_entry_patterns) {
    if (grepl(pat, fn_name, ignore.case = TRUE)) return(TRUE)
  }
  FALSE
}

entry_point_functions <- all_defined_functions[sapply(all_defined_functions, is_likely_entry_point)]
cat("[static] Identified", length(entry_point_functions), "likely Shiny entry points\n")

# ============================================================================
# 5. Build Call Graph and Walk It
# ============================================================================
# For each file, map which functions it defines and which functions it calls
build_call_graph <- function() {
  # Start with functions that are:
  # 1. Called from somewhere
  # 2. Are entry points (server, ui, render*, observe*, etc.)
  # 3. Called from app.R, server.R, ui.R, global.R

  live_functions <- unique(c(all_called_functions, entry_point_functions))

  # Also mark as live any function that matches common callback patterns
  callback_patterns <- c(
    "_ui$", "_server$", "_module$",
    "^create_", "^render_", "^update_",
    "^on_", "^handle_"
  )

  for (pat in callback_patterns) {
    matches <- all_defined_functions[grepl(pat, all_defined_functions, ignore.case = TRUE)]
    live_functions <- unique(c(live_functions, matches))
  }

  live_functions
}

live_functions <- build_call_graph()

# ============================================================================
# 6. Identify Dead Functions
# ============================================================================
dead_functions <- setdiff(all_defined_functions, live_functions)

cat("\n=== STATIC CALL-GRAPH RESULT ==============================\n")
cat("Total defined functions :", length(all_defined_functions), "\n")
cat("Functions with calls    :", length(all_called_functions), "\n")
cat("Entry point functions   :", length(entry_point_functions), "\n")
cat("Live functions          :", length(live_functions), "\n")
cat("Potentially dead        :", length(dead_functions), "\n")

if (length(dead_functions) > 0) {
  cat("\n=== POTENTIALLY STRANDED FUNCTIONS ========================\n")

  dead_details <- fn_defs %>%
    filter(name %in% dead_functions) %>%
    arrange(file, line)

  # Group by file for cleaner output
  for (f in unique(dead_details$file)) {
    cat("\nFile:", basename(f), "\n")
    file_dead <- dead_details %>% filter(file == f)
    for (i in seq_len(nrow(file_dead))) {
      cat(sprintf("  Line %4d: %s()\n", file_dead$line[i], file_dead$name[i]))
    }
  }

  # ============================================================================
  # 7. Verify: Double-check for calls we might have missed
  # ============================================================================
  cat("\n=== VERIFICATION ==========================================\n")
  cat("Double-checking each 'dead' function for any references...\n\n")

  truly_dead <- character()
  false_positives <- character()

  for (fn in dead_functions) {
    # Search for this function name followed by ( in all files
    # Use word boundary to avoid partial matches
    search_pattern <- paste0("\\b", fn, "\\s*\\(")

    found_in <- character()
    for (f in r_files) {
      lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
      if (is.null(lines)) next

      # Skip the definition line itself
      def_info <- fn_defs %>% filter(name == fn, file == f)

      for (i in seq_along(lines)) {
        # Skip if this is the definition line
        if (nrow(def_info) > 0 && i %in% def_info$line) next

        if (grepl(search_pattern, lines[i], perl = TRUE)) {
          found_in <- c(found_in, paste0(basename(f), ":", i))
        }
      }
    }

    if (length(found_in) > 0) {
      cat("FALSE POSITIVE:", fn, "- found calls in:\n")
      for (loc in found_in[1:min(3, length(found_in))]) {
        cat("  ", loc, "\n")
      }
      if (length(found_in) > 3) cat("  ... and", length(found_in) - 3, "more\n")
      false_positives <- c(false_positives, fn)
    } else {
      truly_dead <- c(truly_dead, fn)
    }
  } # end dead function loop

  cat("\n=== FINAL RESULTS =========================================\n")
  cat("Initially flagged as dead:", length(dead_functions), "\n")
  cat("Confirmed false positives:", length(false_positives), "\n")
  cat("Truly stranded functions :", length(truly_dead), "\n")

  if (length(truly_dead) > 0) {
    cat("\nTRULY STRANDED FUNCTIONS:\n")

    truly_dead_details <- fn_defs %>%
      filter(name %in% truly_dead) %>%
      arrange(file, line)

    for (i in seq_len(nrow(truly_dead_details))) {
      cat(sprintf("  - %s  (%s:%d)\n",
                  truly_dead_details$name[i],
                  basename(truly_dead_details$file[i]),
                  truly_dead_details$line[i]))
    }
  } else {
    cat("\nNo truly stranded functions found!\n")
  } # end truly dead if block

} else {
  cat("\nNo potentially dead functions detected.\n")
}


cat("\nDone.\n")

