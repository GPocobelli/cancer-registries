
# ______________________________________________________
#
## ----------- Helper Functions -----------------------
# ______________________________________________________



#' Returns the path of the folder `global_tools`
#'
#' @description
#' Helper Funktion um zu global tools Pfad zu gelangen
#'
#' @param filename       String; Access to the file in the `global_tools` folder
#'
#' @returns              Path to the file. Example use: `get_global_tool_path("helper_functions.R")`
#' @export
get_global_tool_path <- function(filename) {
  here("global_tools", filename)
}













#' Creates WD for generated results
#'
#' @description
#' To save the results with the current date and name in the correct directory `Projects/Projectname/results`.
#'
#' @param analysis_type     String; defines the type of the Output.
#'
#' @returns                 Directory of `Projects/Projectname/results` to safe the results here.
#' Example use: `create_results_wd("cleaning")`.
#' `openxlsx::write.xlsx(data, file = file.path(results_dir, "result_1_data.csv"))`.
#'
#' @export

create_results_wd <- function(analysis_type){
  today <- format(Sys.Date(), "%Y-%m-%d")
  results_dir <- here::here("results", paste0(today, "_", analysis_type))
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  return(results_dir)
}













# # Erstellt eine neue Testdatei
# # Nur bei Bedarf notwendig, Initiale Datei reicht auch vielleicht
# create_tests_wd <- function(analysis_type){
#   today <- format(Sys.Date(), "%Y-%m-%d")
#   results_dir <- here::here("tests", paste0(today, "_", analysis_type))
#   if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
#   return(results_dir)
# }













#' Create new notebook
#'
#' @description
#' Creates a new note file with the correct path in `Projects/Projectname/notebook` and date automatically.
#'
#' @param topic        String; name of the new note file to be inserted.
#'
#' @returns            Path of the new created notebook file.
#'
#' @examples           `create_notebook_wd("notes")`
#'
#' @export
create_notebook_wd <- function(topic = "notes") {
  # Defining date, name and path
  today <- format(Sys.Date(), "%Y-%m-%d")
  notebook_name <- paste0(today, "_", topic, ".qmd")
  notebook_path <- here("notebooks", notebook_name)
  project_name <- basename(here())
  user = Sys.info()["user"]

  # Check if directory exists already
  if (!dir.exists(here("notebooks"))) dir.create(here("notebooks"), recursive = TRUE)

  if (!file.exists(notebook_path)) {

    # Create notebook template
    # Adjustable if necessary
    writeLines(
      c(
        "---",
        paste0('title: "', project_name, ' – Notebook"'),
        "format: html",
        "execute-dir: project",
        "---",
        paste0("**User:** ", user),
        paste0("# Note – ", today),
        paste0("**Session:** ", R.version.string),
        "", "",
        "## Purpose:",
        "", "",
        "## Observation:",
        ""
      ),
      con = notebook_path
    )
    cli_alert_success(paste0("notebook.qmd has been created: ", notebook_path))
  } else {
    cli_alert_warning(paste0("Notebook already exists: ", notebook_path))
  }

}













#' Save recent cleaned data
#'
#' @description
#' `save_cleaned_result()` saves results in 3 different data formats (csv, xlsx, rds)
#' in the directory `data/cleaned/`.
#'
#' @param data                 Data object to be transformed and stored.
#' @param filename_prefix      String; represents the name of the direcotry to be saved.
#' @param cleaned_dir          Default: `data/cleaned/` path
#' @param write_csv2           If = TRUE: CSV with seperator ";"
#' @param create_latest_copy   If = TRUE: Creates a copy with the name: `latest_NAME.rds`
#'
#' @returns                    Creates 3 data formats (xlsx, csv, rds) + `latest_NAME.rds`
#'
#' @examples                   `save_cleaned_result(data, filename_prefix = "therapy_lines")`
#'
#' @export

save_cleaned_result <- function(data,
                                filename_prefix = "cleaned_data",
                                cleaned_dir = "data/cleaned",
                                write_csv2 = FALSE,
                                create_latest_copy = TRUE) {

  # Directory already exisits?
  if (!dir.exists(cleaned_dir)) dir.create(cleaned_dir, recursive = TRUE)

  # Date
  today <- format(Sys.Date(), "%Y-%m-%d")

  # Names
  base <- paste0(filename_prefix, "_", today)
  path_rds   <- file.path(cleaned_dir, paste0(base, "_data.rds"))
  path_csv   <- file.path(cleaned_dir, paste0(base, "_data.csv"))
  path_xlsx  <- file.path(cleaned_dir, paste0(base, "_data.xlsx"))

  # Save
  saveRDS(data, path_rds)

  if (write_csv2) {
    readr::write_csv2(data, path_csv)  # with ";" as a separator (i.e. Excel)
  } else {
    readr::write_csv2(data, path_csv)   # with "," as separator
  }
  openxlsx::write.xlsx(data, path_xlsx)

  if (create_latest_copy) {
    latest_rds_path <- file.path(cleaned_dir, paste0("latest_", filename_prefix, "_data.rds"))
    file.copy(path_rds, latest_rds_path, overwrite = TRUE)
  }

  message("✅ saved as: ", basename(path_rds), ", ", basename(path_csv), ", ", basename(path_xlsx))

  invisible(list(rds = path_rds, csv = path_csv, xlsx = path_xlsx))
}














#' Import latest cleaned data
#'
#' @description
#' `read_latest_cleaned_data()` returns a list of all cleaned data sets from the directory
#' `data/cleaned/` making it easier to import them into the central pipeline or report file.
#'
#'
#' @param path        Default path: `data/cleaned`
#' @param detailed    If TRUE, detailed infos are displayed
#'
##' @returns          List with all data sets; Access via: `cleaned_data_list[["__NAME__"]]`
#'                                                     or `cleaned_data_list$__NAME__`.
#'                                                     Example use: `result_list <- read_latest_cleaned_data()`
#'
#' @import tools
#' @export
read_latest_cleaned_data <- function(path = file.path("data", "cleaned"), detailed = FALSE) {

  if (!dir.exists(path)) {
    warning("⚠️ Directory not found: ", path)
    return(list())
  }


  latest_files <- list.files(
    path = path,
    pattern = "^latest_.*\\.rds$",
    full.names = TRUE
  )


  if (length(latest_files) == 0) {
    warning("⚠️ No file with the pattern: 'latest_*.rds' found in ", path)
    return(list())
  }


  data_list <- lapply(latest_files, readRDS)
  #names(data_list) <- gsub("^latest_.*\\.rds$", "\\1", basename(latest_files))
  names(data_list) <- basename(latest_files) %>%
    tools::file_path_sans_ext() %>%
    gsub("^latest_", "", .) %>%
    gsub("_data$", "", .)


  if (detailed) {
    message("✅ Imported data sets:")
    for (i in seq_along(data_list)) {
      message(" - ", basename(latest_files[i]), " → ", names(data_list)[i])
    }
  }

  else {
    message("✅ Imported data sets:", paste(names(data_list), collapse = ", "))
  }


  return(data_list)
}
