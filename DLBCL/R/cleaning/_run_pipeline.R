


# scripts/cleaning/_run_pipeline.R




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------- Loading Required Packages -------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("scripts/load_lib.R")
source(".Renviron")

library(globaltools)
library(jsonlite)
library(httr)
library(cli)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------- Loading Data Handling Files -----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

files <- list.files(
  "scripts/cleaning",
  pattern = "^[0-9]+.*\\.R$",
  full.names = TRUE
)

# _run_pipeline.R selbst nicht erneut sourcen
files <- files[basename(files) != "_run_pipeline.R"]
purrr::walk(files, source)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------- >> START PIPELINE HERE << ------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- run_cleaning_pipeline(
  api_url   = api_url,
  api_token = api_token
)



base <- out$base
thx  <- out$thx
sv   <- out$sv
pat  <- out$patient_level
tot  <- out$therapy_lines






