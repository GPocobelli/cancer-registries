




### ____________________________________________________________________________
#
#
# -------------------- Project scaffold ----------------------------------------
#
### ____________________________________________________________________________



#' This script provides a comprehensive function `create_new_project()`.
#' The Idea is to create a reproducible project.
#' Oriented on:
#'    Noble WS (2009) A Quick Guide to Organizing Computational Biology Projects.
#'    PLoS Comput Biol 5(7): e1000424. doi:10.1371/journal.pcbi.1000424
#'
#' * **renv**: Is used to initialize isolated and mobile packages.
#'   Initialization via: `renv::init()` and `renv::snapshot()` to store the
#'   version of the installed packages.
#'   `sessionInfo()` is used to see all used packages and versions.
#'
#' * **Targets‑Outputs**: a `_targets.R`‑Skeleton is created (optional),
#'   supported by `gittargets`.
#'   Example: `tar_git_init()` and `tar_git_snapshot()`
#'
#' * **Modules**: The function `create_new_project()` uses small helper functions.
#'   Each function has a roxgen2 comment block.
#'
#' * **Documentation**: README and Notebook are created, which can be used for
#'   important documentation, like references, research questions, objectives,
#'   (statistical) methods, without a specific presetting to keep it more general.
#'
#' * **Tests**: A testthat skeleton is created. Here, data checks & pipeline tests
#'   can be done.
#'
#'
#'
#'
#'
#'
#' * **Project structure**:
#'
#'   Projects/                        # Parent directory for all projects
#'    ├── Projectname_1/
#'         ├── data/
#'│            ├── raw/                        # Original data (do not modify)
#'│            └── cleaned/                    # Processed data ready for analysis
#'         ├── doc/
#'         ├── notebooks/
#'│            └── YYYY-MM-DD_notes.md         # Project notes and observations
#'         ├── results/
#'│            ├── YYYY-MM-DD_cleaning/
#'│            └── YYYY-MM-DD_analysis/
#'         ├── reports/
#'│            ├── Projectname_1_report.qmd
#'         ├── R/
#'│            ├── cleaning_script.R
#'│            ├── load_lib.R                  # Load required libraries
#'│            ├── read_config.R               # Project-specific configurations
#'│            ├── runall_script.R             # Optional - Central analysis pipeline
#'│            └── other_scripts.R
#'         ├── tests/
#'│            └── testthat/ # Rscripte die alles jeweils testen.
#'         ├── tmp/
#'         ├── .gitignore
#'         ├── .Renviron
#'         ├── _targets.R
#'         ├── README.qmd
#'│        └── Projectname_1.Rproj
#'
#'    ├── Projectname_2/
#'    ├── ...
#'    ├── global_tools/                      # Important!
#'    │    ├── targets_project_setup.R
#'    │    ├── helper_functions.R            # Important!
#'    │    └── stats_functions.R             # other global functions
#'
#'
#'
#'
#'
#'
#' * **Procedure** -------------------------------------------------------------
#'   1. Create a parent folder called `Path/to/Projects` & a
#'      directory `Path/to/Projects/globaltools`.
#'
#'   2. Save this file in the directory `Path/to/Projects/globaltools/targets_project_setup.R`.
#'
#'   3. In a fresh RStudio session run: `source("Path/to/Projects/globaltools/targets_project_setup.R")`
#'      to import the function `create_new_project()`.
#'
#'   4. You also have to save the helper functions into `Path/to/Projects/globaltools`
#'      and access it via `source("Path/to/Projects/global_tools/helper_functions.R")`.
#'
#'   5. To start a new project, run: `create_new_project("Projectname_1")`
#'
#'   6. Open the newly created *.Rproj, then:
#'      targets::tar_make()
#'
#'
#'   7. More targets commandos:
#'       | Commandos               | Explanation                               |
#        | ----------------------- | ----------------------------------------- |
#        | `tar_make()`            | Run pipeline                              |
#        | `tar_visnetwork()`      | Visualization of dependencies             |
#        | `tar_read(target_name)` | Read single target                        |
#        | `tar_load(target_name)` | Load target in workspace                  |
#        | `tar_git_snapshot()`    | Save Git snapshot                         |
#        | `tar_destroy()`         | Delete everything (`_targets/` folder)    |
#'
#'
#'
#'
#'
#'
#'
#' Prerequisite:
#' Needed packages: **fs**, **here**, **renv**, **yaml**, **withr**,
#'                  **gittargets**, **targets** and **tarchetypes**.
#' If missing it will be installed automatically.
#
#
#'
#' @seealso
#' Noble, W., (2009) "A Quick Guide to Organizing Computational Biology Projects"
#' – PLoS Computational Biology 5(7):e1000424. https://doi.org/10.1371/journal.pcbi.1000424
# ______________________________________________________________________________



















### ____________________________________________________________________________
#
#
# --------------------- Code block ---------------------------------------------
#
### ____________________________________________________________________________



## Install and load packages ----

required_setup_packages <- c("fs", "here", "renv", "yaml", "withr",
                             "gittargets", "targets", "tarchetypes")
for (pkg in required_setup_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

if (!requireNamespace("globaltools", quietly = TRUE)) {
  remotes::install_local("M:/Projects/globaltools")
}

library(fs)
library(here)
library(yaml)
library(renv)
library(withr)
library(gittargets)
library(targets)
library(tarchetypes)
library(cli)















## -------------------- Core function ------------------------------------------

#' Core function for creating a new project
#'
#' This function uses helper functions to create an complete project setup.
#' It can be used as a function via a R-script. It uses renv & targets to
#' set up a reproducible workflow.
#'
#' @param project_name
#' String; Name of the project. Is used as file name.
#'
#' @param base_path
#' Base path where the project will be.
#' Has to be defined with `set_wd(__your_path__/projects/)` before.
#'
#' @param include_renv
#' Logical; Whether a renv environment should be initialized. Default: `FALSE`.
#'
#' @param include_gitignore
#' Logical; Whether a gitignore file should be initialized. Default: `TRUE`.
#'
#' @param create_readme
#' Logical; Whether a README file should be initialized. Default: `TRUE`.
#'
#' @param user
#' Name of the user. Will be used in README, config.yaml & cleaning_data_pipeline.R
#' @param use_targets
#'
#' @return Path to the created zum erstellten Projektverzeichnis
#'
#' @examples
#' set_wd("C:/Users/path/to/Projects")
#' create_new_project("Projekt1", include_renv = FALSE)
#' @export
create_new_project <- function(project_name,
                               base_path = getwd(),
                               include_renv = FALSE,
                               use_targets = TRUE,
                               include_gitignore = TRUE,
                               create_readme = TRUE,
                               user = Sys.info()["user"]) {



  # create project path
  project_path <- file.path(base_path, project_name)











  ### -------------------- Directories -----------------------------------------

  # This helper function creates all directories for the project.
  #
  # Base paths are stored in the `project_path`.
  #
  # @param project_path
  # String with the path to the project.
  #
  # @param dirs
  # Vector of all files which will be created for the project.
  #
  # @return invisible `NULL`.

  create_project_dirs <- function(project_path, dirs) {
    for (d in dirs) {
      dir_create(path(project_path, d))
    }
    invisible(NULL)
  }

  if (dir_exists(project_path)) {
     cli_alert_danger("Project directory already exists: {project_path}")
     stop("Abort")
  }

  dir_create(project_path, recurse = TRUE)
  # Verzeichnisse erzeugen
  dirs <- c(
    "data/raw",
    "data/cleaned",
    "R",
    "results",
    "reports",
    "notebook",
    "doc",
    "tests",
    "tmp"
  )

  create_project_dirs(project_path, dirs)
  cli_alert_success("Project directory has been created.")










  ### -------------------- .Rproj ----------------------------------------------

  # Creates the R-project file (.Rproj).
  #
  # Default options are set, like: `RestoreWorkspace: No` and `Encoding: UTF-8`.
  #
  # @param project_name
  # String; Name of the project.
  #
  # @param project_path
  # String with the path to the project.

  write_rproj_file <- function(project_name, project_path) {
    rproj_file <- path(project_path, paste0(project_name, ".Rproj"))

    if (!file_exists(rproj_file)) {
      file_create(rproj_file)

      writeLines(
        c(
          "Version: 1.0",
          "",
          "RestoreWorkspace: No",
          "SaveWorkspace: No",
          "AlwaysSaveHistory: Default",
          "",
          "EnableCodeIndexing: Yes",
          "UseSpacesForTab: Yes",
          "NumSpacesForTab: 2",
          "Encoding: UTF-8",
          "",
          "RnwWeave: knitr",
          "LaTeX: pdfLaTeX"
        ),
        con = rproj_file
      )
    }
    invisible(rproj_file)
  }

  write_rproj_file(project_name, project_path)
  cli_alert_success(".Rproj file has been created")











  ### -------------------- config.yaml -----------------------------------------

  # Creates the configuration file: `config.yaml`
  #
  # Metadata about the project (name, date, user) are saved in the YAML‑file.
  # These infos can be used later in reports or scripts.
  # The file is placed in `reports/config.yaml`.
  #
  # @param project_name
  # String; Name of the project.
  #
  # @param project_path
  # String with the path to the project.
  #
  # @param user
  # Name of the user

  write_config_yaml <- function(project_name, project_path, user) {
    today <- format(Sys.Date(), "%Y-%m-%d")
    config <- list(
      project_name = project_name,
      created = today,
      user = user
    )
    write_yaml(config, file.path(project_path, "reports", "config.yaml"))
    invisible(NULL)
  }

  write_config_yaml(project_name, project_path, user)
  # cli_alert_success("config.yaml file has been created")











  ### -------------------- read_config.R ---------------------------------------

  # Creates a helper script to load the config.yaml informations.
  #
  # The script `R/read_config.R` defines a function `read_project_config()`,
  # which returns the confi.yaml Info like `project_name`,
  # `created` and `user`.
  #
  # @param project_path
  # String with the path to the project.

  write_read_config_script <- function(project_path) {
    read_config_file <- path(project_path, "R", "read_config.R")
    if (!file_exists(read_config_file)) {
      writeLines(
        c(
          "# R/read_config.R",
          "",
          "#' Read project configuration from config.yaml",
          "#'",
          "#' This function attemts to read the confi.yaml file in `reports/config.yaml`",
          "#' It can be used in reports or scripts to reference metadata such as",
          "#' project name, date or user.",
          "#'",
          "#' @param config_path Path to config.yaml; ",
          "#' Default: `here('reports', 'config.yaml')` ",
          "#' @return List including projekt_name, date and user",
          "read_project_config <- function(config_path = here::here('reports', 'config.yaml')) {",
          "  if (!file.exists(config_path)) {",
          "    cli_alert_warning('config.yaml not found, default info will be used.')",
          "    return(list(project_name = 'Unknown Project', created = Sys.Date(), user = Sys.info()['user']))",
          "  }",
          "  yaml::read_yaml(config_path)",
          "}",
          ""
        ),
        con = read_config_file
      )
    }
    invisible(NULL)
  }

  write_read_config_script(project_path)
  # cli_alert_success("read_config.R file has been created")











  ### -------------------- quarto config ---------------------------------------

  # Creates a quatro project configuration file.
  #
  # The file `_quarto.yml` defines basic settings for Quarto reports.
  #
  # @param project_path
  # String with the path to the project.

  write_quarto_config <- function(project_path) {
    quarto_config <- list(
      project = list(
        type = "default",
        `execute-dir` = "project"
      )
    )
    write_yaml(quarto_config, file.path(project_path, "reports", "_quarto.yml"))
    invisible(NULL)
  }

  write_quarto_config(project_path)
  # cli_alert_success("_quarto.yml file has been created")











  ## -------------------- Report template (Quarto) -----------------------------

  # Creates a Quarto report template (.qmd-file) in the `reports/`-file.
  # It is used as the central report file for the project.
  # The template uses the project configurations & library automatically.
  # Also `sessionInfo()` is used to save the relevant Info of R.
  #
  # @param project_name
  # String; Name of the project.
  #
  # @param project_path
  # String with the path to the project.

  write_report_template <- function(project_name, project_path) {
    report_file <- path(project_path, "reports", paste0(project_name, "_report.qmd"))
    if (!file_exists(report_file)) {
      writeLines(
        c(
          "---",
          paste0('title: \"', project_name, ' – Report\"'),
          "format:",
          "  html:",
          "    self-contained: true",
          paste0('    output-file: \"', project_name, '_report.html\"'),
          "output-dir: null",
          "execute-dir: project",
          "---",
          "",
          "```{r}",
          "#| include: false",
          "library(here)",
          "library(yaml)",
          "source(here('R', 'read_config.R'))",
          "config <- read_project_config()",
          "# Loading the package library; when using renv, the library is disabled",
          "source(here('R', 'load_lib.R'))",
          "```",
          "",
          "# Overview",
          "",
          "Project: `r config$project_name`  ",
          "Created on: `r config$created`  ",
          "Author: `r config$user`",
          "",
          "",
          "```{r}",
          "#| include: false",
          "",
          "library(targets)",
          "tar_load(cleaned_data)",
          "library(globaltools)",
          "```",
          "",
          "## Analysis",
          "",
          "```{r}",
          "#| include: false",
          "",
          "cleaned_data <- read_latest_cleaned_data(path = here::here('data', 'cleaned'))",
          "```",
          "",
          "",
          "",
          "Session Information: ",
          "```{r}",
          "sessionInfo()",
          "```",
          ""

        ),
        con = report_file
      )
    }
    invisible(NULL)
  }

  # Reporttemplate
  write_report_template(project_name, project_path)
  # cli_alert_success(project_name, "_report.qmd has been created")










  ## -------------------- R/load_lib.R -----------------------------------------

  ### Adjustable relevant packages ----
  # load_lib.R – List of default packages
  default_pkgs <- c(
    "boot",
    "car", "conflicted", "cli",
    "DataExplorer",
    "fs",
    "ggdist", "ggforce", "gghalves", "ggpp", "ggpubr", "ggrain", "ggsurvfit",
    "gittargets", "gtsummary", "globaltools",
    "here", "Hmisc", "httr",
    "kableExtra", "knitr",
    "magrittr", "MASS",
    "openxlsx",
    "pacman", "patchwork", "powerSurvEpi",
    "quarto",
    "readr", "readxl", "REDCapR", "REDCapTidieR", "rmarkdown",
    "skimr", "stringi", "survival", "survminer",
    "targets", "tarchetypes", "testthat", "tidyverse", "tidymodels",
    "yaml"
  )
  optional_pkgs <- c("colorblindr")







  # Creates a script to load the required R packages.
  #
  # The script `R/load_lib.R` is loading a list of required packages.
  # The list can be ajusted by the user.
  #
  # @param project_path
  # String with the path to the project.
  #
  # @param default_pkgs
  # Vector containing the most important packages to be loaded.
  #
  # @param optional_pkgs
  # Vector containing optional packages, which are ignored if problems occur.

  write_load_lib_script <- function(project_path,
                                    default_pkgs,
                                    optional_pkgs) {

    lib_file <- path(project_path, "R", "load_lib.R")
    if (!file_exists(lib_file)) {
      writeLines(
        c(
          "# R/load_lib.R",
          "#' Installs and loads required packages.",
          "",
          "required_packages <- c(",
          paste0('  \"', default_pkgs, '\"', collapse = ",\n"),
          ")",
          "",
          "optional_packages <- c(",
          paste0('  \"', optional_pkgs, '\"', collapse = ",\n"),
          ")",
          "",
          "install_if_missing <- function(pkg) {",
          "  if (!requireNamespace(pkg, quietly = TRUE)) {",
          "    tryCatch({",
          "      install.packages(pkg)",
          "    }, error = function(e) {",
          "      cli_alert_warning('Package could not be installed: ', pkg)",
          "    })",
          "  }",
          "}",
          "",
          "# Install & load main packages",
          "invisible(lapply(required_packages, install_if_missing))",
          "invisible(lapply(required_packages, function(pkg) library(pkg, character.only = TRUE)))",
          "cli_alert_success('Alle Bibliotheken wurden geladen.')",
          "",
          "# Install & load optional packages",
          "invisible(lapply(optional_packages, install_if_missing))",
          ""

        ),
        con = lib_file
      )
    }
    invisible(NULL)
  }

  write_load_lib_script(project_path, default_pkgs, optional_pkgs)
  # cli_alert_success('load_lib.R was created')













  ## -------------------- README (Quarto) --------------------------------------

  # Creates a README file.
  # README describes all the relevant project information, like workflow,
  # documentation, background info, research questions, methods, etc.
  # Can be adjusted
  #
  # @param project_name
  # String; Name of the project.
  #
  # @param project_path
  # String with the path to the project.
  #
  # @param user
  # Name of the user

  write_readme <- function(project_name, project_path, user) {
    readme_file <- path(project_path, "README.qmd")
    today <- format(Sys.Date(), "%Y-%m-%d")
    writeLines(
      c(
        "---",
        paste0('title: \"', project_name, ' – README\"'),
        "format:",
        "  html:",
        "    toc: true",
        "    toc-depth: 2",
        "    number-sections: true",
        "    self-contained: true",
        "    output-file: README.html",
        "---",
        "",
        paste0("# Project: ", project_name),
        paste0("- Author: ", user),
        paste0("- Created on: ", today),
        "<br>",
        "## Directory overview",
        "- `data/`:       Raw & clean Data",
        "- `R/:`          all R scripts",
        "- `results/`:    Results & Outputs",
        "- `reports/`:    Reports",
        "- `notebook/`:  Notes",
        "- `tests/`:      Tests",
        "<br>",
        "<br>",
        "## Workflow",
        "<br>",
        "### Using {targets}",
        "- Data cleaning, analysis & reports run via the package {targets}",
        "- The pipeline is saved in: `_targets.R`",
        "<br>",

        "### Important commands",
        "- `tar_make()` – starting the pipeline",
        "- `tar_visnetwork()` – isplay dependency graph",
        "- `tar_load(cleaned_data)` – Load result objects into the workspace",
        "<br>",
        "### Update Quarto Report",
        "  Automatically when `cleaned_data` changes (via `tar_quarto(...)`)",
        "<br>",

        "### Note",
        "- Intermediate results are saved in `data/cleaned/`",
        "- Raw data remains unchanged in `data/raw/`",
        "- Functions and helpers are saved in `R/`",
        "<br>",

        "## Documentation",
        "### Preliminary information on relevant formulas/scores etc.",
        "<br>",

        "## Register planning",
        "<br>",
        "",

        "### 01 – Background & literature",
        "    Study situation, publications",
        "<br>",
        "<br>",


        "### 02 – Research questions",
        "    - Objectives and hypotheses",
        "    - Planned endpoints",
        "      (OS, PFS, ORR, CR, etc.)",
        "    - Planned subgroup comparisons",
        "<br>",
        "<br>",


        "### 03 – Sample & Expected Distributions",
        "    - Estimation of sample, power, HR, event rates",
        "    - library(powerSurvEpi)", "powerCT(), ssizeCT(), Precision, Bootstrap",
        "<br>",
        "<br>",


        "### 04 – Statistical methods",
        "<br>",
        "<br>",
        "",

        "### 06 – Publication plan",
        "authors, deadlines",
        "<br>",
        "<br>"
      ),
      con = readme_file
    )
    invisible(NULL)
  }
  # README
  if (create_readme) write_readme(project_name, project_path, user)
  # cli_alert_success("README.qmd file has been created")













  ## -------------------- notebook ---------------------------------------------

  # Creates an empty notebook quarto file, where comments, observations,
  # important hints, etc. can be recorded.
  # The notebook will be saved in the file `notebook/` with today's date.
  #
  # @param project_name
  # String; Name of the project.
  #
  # @param project_path
  # String with the path to the project.
  #
  # @param user
  # Name of the user

  create_notebook <- function(project_name, project_path, user) {
    today <- format(Sys.Date(), "%Y-%m-%d")
    notebook_today <- path(project_path, "notebook", paste0(today, "_notes.qmd"))
    results_today <- path(project_path, "results", paste0(today, "_initial"))

    if (!dir_exists(results_today)) dir_create(results_today)
    if (!file_exists(notebook_today)) {
      writeLines(
        c(
          "---",
          paste0('title: \"', project_name, ' – Notebook\"'),
          "format:",
          "  html:",
          "    self-contained: true",
          "execute-dir: project",
          "---",
          "",
          "",
          paste0("**Project:** ", project_name),
          paste0("**Date:** ", today),
          paste0("**User:** ", user),
          paste0("**Session:** ", R.version.string),
          "",
          "## Objective:",
          "",
          "",
          "## Comments:",
          "",
          ""
        ),
        con = notebook_today
      )
    }
    invisible(NULL)
  }

  create_notebook(project_name, project_path, user)
  # cli_alert_success("notebook file has been created")














  ## -------------------- renv (optional) --------------------------------------

  # Initializes a `renv` environment.
  #
  # The function calls `renv::init()` within the project structure, to create
  # a lokal pproject library.
  # Then, packages can be installed via `renv::snapshot()` and
  # will be stored in `renv.lock`.
  # If `renv` cannot be initialized, a warning will occur & the project will be
  # created without `renv`.
  #
  # @param project_path
  # String with the path to the project.
  #
  # @param include_renv
  # Logical; whether `renv` should be initialized.
  #
  # @return invisible `NULL`

  initialize_renv_env <- function(project_path, include_renv = TRUE) {
    if (!include_renv) {
       cli_alert_success("Project was created without using renv.")
      return(invisible(NULL))
    }
    if (!requireNamespace("renv", quietly = TRUE)) {
       cli_alert_danger("renv was not instalized. Project will be created without renv.")
      return(invisible(NULL))
    }
    message("🔒 Initialize renv for the project")

    # Run renv::init() in the project directory
    withr::with_dir(project_path, {
      # bare = TRUE avoids copying the global library
      renv::init(bare = TRUE, force = TRUE)
    })
    invisible(NULL)
  }

  initialize_renv_env(project_path, include_renv = include_renv)
  cli_alert_success("renv was initialied. Please use `renv::snapshot()` after a package installation.")














  ## -------------------- R/runall.R (optional) --------------------------------

  # Creates a central script pipeline for reproducible purposes.
  #
  # The script `R/runall.R` loads all packages and helper functions, runs cleaning process,
  # analysis scripts, renders the quarto report & creates a snapshot version of the target stores,
  # if gittargets is used.
  #
  # @param project_name
  # String; Name of the project.
  #
  # @param project_path
  # String with the path to the project.

  write_runall_script <- function(project_name, project_path) {
    runall_file <- path(project_path, "R", "runall.R")

    if (use_targets == FALSE) {

      if (!file_exists(runall_file)) {
        writeLines(
          c(
            "# runall.R – Central pipeline",
            "",
            "# 1. Setup",
            "set.seed(1234)",
            "source('R/load_lib.R')",
            "library(globaltools)",
            "source('R/read_config.R')",
            "",

            "# 2. Data cleaning",
            "source('R/cleaning_data_pipeline.R')",
            "",

            "# 3. Loading the cleaned data",
            "# cleaned_data <- read_latest_cleaned_data()",
            "",
            "# 4. Run analysis",
            "# source('R/run_deskriptiv.R')",
            "# source('R/run_survival.R')",
            "",
            "# 5. Generate report",
            "if (!requireNamespace('quarto', quietly = TRUE)) install.packages('quarto')",
            paste0("quarto::quarto_render('reports/", project_name, "_report.qmd')"),
            "",
            "# 6. Create targets-Snapshot (optional)",
            "if (requireNamespace('gittargets', quietly = TRUE)) {",
            "  try({ gittargets::tar_git_snapshot() }, silent = TRUE)",
            "}",
            "", ""
          ),
          con = runall_file
        )
      }

    invisible(NULL)
    cli_alert_success("runall.R file has been created")
    }
  }

  # runall.R
  write_runall_script(project_name, project_path)














  ## -------------------- R/cleaning_data_pipeline.R ------------------------------------

  # Function to clean raw data and export as a cleaned file.
  # Included by targets
  #
  # @param project_name
  # String; Name of the project.
  #
  # @param project_path
  # String with the path to the project.
  #
  # @param user
  # Name of the user

  write_cleaning_data_script <- function(project_name, project_path, user) {

  cleaning_file <- file.path(project_path, "R", "cleaning_data_pipeline.R")
  today <- format(Sys.Date(), "%Y-%m-%d")

  if (!file_exists(cleaning_file)) {
    writeLines(
      c(
        "",
        "# cleaning_data_pipeline.R",
        "",
        paste0("# User: ", user),
        paste0("# Notes – ", today),
        paste0("# Session: ", R.version.string),
        "", "", ""
    ),

    con = cleaning_file
    )
  }

  invisible(NULL)
  }

  write_cleaning_data_script(project_name, project_path, user)
  # cli_alert_success("cleaning_data_pipeline.R script has been created")















  ## -------------------- tests skeleton ---------------------------------------

  # Creates a test skeleton in `tests/testthat/`.
  # Here it can be tested, if there are missing values, or if the targets‑pipeline
  # runs without errors.
  #
  # @param project_path
  # String with the path to the project.

  write_test_structure <- function(project_path) {
    testthat_dir <- path(project_path, "tests", "testthat")
    dir_create(testthat_dir)
    # Simple datacheck
    test_data_file <- path(testthat_dir, "test_data_checks.R")
    if (!file_exists(test_data_file)) {
      writeLines(
        c(
          "# tests/test_data_checks.R",
          "library(testthat)",
          "test_that('Data does not contain any missing values', {",
          "  raw_path <- file.path('data', 'raw', 'data.csv')",
          "  skip_if_not(file.exists(raw_path))",
          "  data <- read.csv(raw_path)",
          "  expect_true(all(complete.cases(data)))",
          "})",
          ""
        ),
        con = test_data_file
      )
    }

    # Pipeline-Test
    pipeline_test_file <- path(testthat_dir, "test_pipeline.R")
    if (!file_exists(pipeline_test_file)) {
      writeLines(
        c(
          "# tests/test_pipeline.R",
          "library(testthat)",
          "library(targets)",
          "test_that('Target pipeline can be executed without errors', {",
          "  skip_if_not(file.exists('_targets.R'))",
          "  # We perform a dry run to validate the pipeline.",
          "  expect_error(targets::tar_make(callr_function = NULL), NA)",
          "})",
          ""
        ),
        con = pipeline_test_file
      )
    }
    invisible(NULL)
  }

  write_test_structure(project_path)
  # cli_alert_success("Test that skeleton has been created")












  ## -------------------- .gitignore -------------------------------------------

  # Creates a .gitignore template.
  #
  # It protects sensitive data.`renv.lock` is **not** ignored, as it is necessary
  # for the reproducible environment.
  #
  # @param project_path
  # String with the path to the project.

  write_gitignore <- function(project_path) {
    gitignore_file <- path(project_path, ".gitignore")
    gitignore_content <- c(
      "# R",
      ".Rhistory",
      ".RData",
      ".Rproj.user/",
      "",
      "# renv",
      "renv/library/",
      "renv/python/",
      "renv/staging/",
      "",
      "# Data",
      "data/raw/",
      "*.xlsx",
      "*.csv",
      "*.rds",
      "",
      "# Results",
      "results/",
      "",
      "# HTML/PDF",
      "*.html",
      "*.pdf",
      "",
      "# Sensitive data",
      "R/config.R",
      ".Renviron",
      ".env"
    )
    writeLines(gitignore_content, gitignore_file)
    invisible(NULL)
  }
  # .gitignore
  if (include_gitignore) write_gitignore(project_path)
  # cli_alert_success(".gitignore file has been created")











  ## -------------------- targets pipeline -------------------------------------

  # Creates a _targets.R‑Skeleton
  #
  # This skeleton defines a targets pipeline inclusive the integration of `gittargets`.
  # The list os targets is expandable and can specify new packages with
  # `tar_option_set()`. The function `tar_git_init()`and `tar_git_snapshot()`
  # are used to connect with Git.
  #
  # @param project_path
  # String with the path to the project.

  write_targets_skeleton <- function(project_path, project_name) {

    if (use_targets) {
      targets_file <- path(project_path, "_targets.R")

      if (!file_exists(targets_file)) {
        writeLines(
          c(
            "### ____________________________________________________________________________",
            "#",
            "#",
            "# -------------------- Targets-Pipeline ----------------------------------------",
            "#",
            "### ____________________________________________________________________________",
            "",
            "",
            "library(targets)",
            "library(gittargets)",
            "library(tarchetypes)",
            "",
            "",
            "",
            "",
            "",
            "## ------- Global options -------------------------------------------------------",
            "tar_option_set(",
              "packages = c('tidyverse', 'here'),",
              "format = 'rds'",
            ")",
            "",
            "",
            "",
            "",
            "",
            "",
            "## ------- Target list - Pipeline -----------------------------------------------",
            "",
            "list(",
            "",
            "",
            "### ------ load_lib.R -----------------------------------",
            "tar_target(",
            "",
            "  load_packages, {",
            "    source('R/load_lib.R')",
            "  },",
            "",
            "  cue = tar_cue(mode = 'always')",
            "",
            "),",
            "### ____________________________________________________",
            "",
            "",
            "",
            "",
            "",
            "",
            "### ------ cleaning script -----------------------------",
            "tar_target(",

            "  cleaning_data_pipeline.R, {",
            "    source('R/cleaning_data_pipeline.R')",
            "  },",
            "",
            "  deps = load_packages,",
            "  cue = tar_cue(mode = 'always')",
            "",
            "),",
            "",
            "### ____________________________________________________",
            "",
            "",
            "",
            "",
            "",
            "",
            "### ------ import cleaned data -------------------------",
            "tar_target(",
            "",
            "  cleaned_data, {",
            "    source('R/load_lib.R')",
            "    read_latest_cleaned_data(detailed = TRUE)",
            "  },",
            "",
            "  deps = cleaning_data,",
            "),",
            "### ____________________________________________________",
            "",
            "",
            "",
            "",
            "",
            "",
            "### ------ Report (Quarto) -----------------------------",
            "tarchetypes::tar_(",
            "",
            "  registry_report,",
            "  path = paste0('reports/', project_name, '_report.qmd'),",
            "  execute_params = list(),",
            "  deps = cleaned_data",
            ")",
            "### ____________________________________________________",
            "",
            "",
            "",
            "### ------ Git -----------------------------------------",
            "",
            "# Initialization of gittargets. Run this code:",
            "# tar_target(",
            "#   snapshot,",
            "#   gittargets::tar_git_snapshot(),",
            "#   deps = registry_report",
            "# )",
            "",
            "### ____________________________________________________",
            ")",
            "",
            ""

          ),
          con = targets_file
        )
      }

      invisible(NULL)
      cli_alert_success("_targets.R file has been created")
    }
  }


  write_targets_skeleton(project_path)













  ## -------------------- Sensitive keys / tokens ------------------------------

  # .Renviron for tokens. It can be accessed via `api_token <- Sys.getenv("api-token")`
  # Important: No empty space (" ") between api_token=___token___ or api_url='___'
  #
  # @param project_path
  # String with the path to the project.

  renviron_path <- file.path(project_path, ".Renviron")
  if (!file.exists(renviron_path)) {
    writeLines(
      c(
        "# Project specific key / tokens",
        "# No empty space between between api_token='01234'",
        "",
        "api_token='___your_token___'",
        "api_url='https:// ...'"

      ),
      con = renviron_path
    )
    # cli_alert_success(".Renviron file was created for keys and tokens")
  }













cli_alert_success("Project '", project_name, "' was created  in: ", project_path)
return(invisible(project_path))
}









