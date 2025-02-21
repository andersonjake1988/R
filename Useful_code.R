# check R version vs releases
# source("C:/Users/ander/Desktop/Gilead/rversion test.R")
# installr::updateR() # Update if out-of-date

# glimpse a list of tables
glimpse_list <- function(list){
  iwalk(list, function(x,y){
    cat(glue("\n\n{y}\n\n"))
    glimpse(x)
  })
}

# Regex for finding strings between strings
# '(?<=STR1)(.*)(?=STR2)'

# create automated task based on script
# taskscheduleR::taskscheduler_create(taskname = "Update_vstm_close",
#                                     rscript = "C:/Users/ander/Documents/GitHub/investbot/inst/scripts/update_close.R",
#                                     schedule = "WEEKLY",
#                                     starttime = "13:30",
#                                     days = c('MON', 'TUE', 'WED', 'THU', 'FRI'))

make_section <- function(.title, comment = TRUE, symbol = "-", indent = 0, underline_sym = "", .length = 70) {
  # Load required libraries
  library(stringr)
  library(rstudioapi)

  # Create the comment section
  title_len <- str_length(str_flatten(c(rep("  ", indent), ifelse(comment, "# ", ""), .title, " ")))
  section <- paste0(str_flatten(rep("  ", indent)), ifelse(comment, "# ", ""), .title, " ", str_flatten(rep(symbol, .length - title_len)))
  under <- str_flatten(rep(underline_sym, .length))
  # Ensure RStudio API is ready
  if (rstudioapi::isAvailable()) {
    # Insert the section above the function call
    rstudioapi::insertText(location = rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start-2,
                           text = paste0(section, "\n", under, "\n"))

  } else {
    warning("RStudio API is not available. Run this in RStudio.")
  }

  # Return the section as feedback
  return(section)
}

find_list_depth <- function(lst) {
  if (!is.list(lst)) {
    return(0)  # Atomic elements have no depth
  }
  return(1 + max(sapply(lst, find_list_depth, simplify = TRUE, USE.NAMES = FALSE), na.rm = TRUE))
}


create_module <- function(name){
  file_name <- glue::glue('modules/{name}_module.R')
  # Check if the directory exists, and create it if it doesn't
  if (!dir.exists('modules')) {
    dir.create('modules')
    message("Directory created: ", 'modules')
  }
  # UI -----------------------------------------------------------------
  ui <- glue::glue("\t{glue('{name}UI')} <- function(id) {{
              ns <- NS(id)
              # Define module UI
            }}")
  # Server -------------------------------------------------------------
  server <- glue::glue("{glue('{name}Server')} <- function(id) {{
                  moduleServer(id, function(input, output, session) {{
                    # Define module Server
                  }})
                }}")
  # Output -------------------------------------------------------------
  output <- stringr::str_c(
    '# UI -----------------------------------------------------------------', "\n",
    ui, "\n\n",
    '# Server -------------------------------------------------------------', "\n",
    server
  )
  write_lines(output, file = file_name)
}


joplin_to_rmd <- function(md_dir, file_name, output_to = getwd(), format = '.html', title = "Joplin Notes"){
  # List all .md files in the folder
  md_files <- list.files(md_dir, pattern = "\\.md$", full.names = TRUE)
  
  # Read and combine the contents of all .md files
  combined_md <- sapply(md_files, function(file) {
    paste0("\n\n", readLines(file), collapse = "\n")
  }, USE.NAMES = FALSE)
  
  # Combine all content into a single character string
  combined_text <- paste(combined_md, collapse = "\n\n")
  
  # Optionally add YAML header for RMarkdown if you need it
  yaml_header <- glue::glue("---
title: \"{title}\"
output: 
  html_document:
    theme: cerulean
    toc: yes
    toc_float:
      collapsed: true
---\n\n")
  
  # Define paths
  if(format == '.html'){
    # Create a temporary directory
    temp_dir <- tempdir()
    rmd_path <- file.path(temp_dir, paste0(file_name, ".Rmd"))
    html_path <- file.path(output_to, paste0(file_name, ".html"))
    writeLines(c(yaml_header, combined_text), rmd_path)
    rmarkdown::render(
      input = rmd_path,
      output_file = html_path
    )
    cli::cli_alert_success(glue::glue("File output: {html_path}"))
  } else {
    rmd_path <- file.path(output_to, paste0(file_name, ".Rmd"))
    writeLines(c(yaml_header, combined_text), rmd_path)
    cli::cli_alert_success(glue::glue("File output: {rmd_path}"))
  }
}










