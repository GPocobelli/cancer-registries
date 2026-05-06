
# scripts/cleaning/01_data_import.R

  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# --------------- Data Import - REDCAP ---------------------
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



  
  
import_data <- function(api_url, api_token, folder_path = "doc/",
                        dd_source = c("api", "local")) {
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ------------ 1. SLL-Certificate for Safety -------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # API token created in REDCAP and used in .Renviron. 
  
  response <- POST(
    url = api_url, body = list(token = api_token, content = "record", format = "json", type = "flat"),
    encode = "form",
    config = config(ssl_verifypeer = TRUE) # SSL-Certificate activation
  )
  
  if (http_error(response)) {
    print("Error for API-request")
  } else {
    print("API-request was successful")
  }
  
  
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ------------ 2. Prepare Relevant Variables  ------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To avoid unwanted data transformation of the blood marker.
# Ensure no convertion of numeric values: load as character
  
  blood_marker <- c(
    "diag_ldh_diag", "albumin", "b2mg", "bilirubin", "crp",
    "fibrinogen", "ggt", "hemoglobine", "neutrophils", "platelet", "lymphocytes",
    "kreatinin", "leukocytes", "harnsaeure", "harnstoff", "monocytes"
  )
  
  col_spec <- do.call(readr::cols, c(
    setNames(rep(list(readr::col_character()), length(blood_marker)), blood_marker),
    list(.default = readr::col_guess())
  ))
  
  
  
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
## ------------- Data import -------------------------------


# Import raw data from redcap via `REDCapR::redcap_read_oneshot()`.
# Also necessary: Data Dictionary of the Registry for data transformation later.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ------------ 1. Raw data from RedCap   -----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  d <- REDCapR::redcap_read_oneshot(api_url, api_token, guess_max = 10000, col_types = col_spec)$data
  
  
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ------------ 2. Data Dictionary ------------------------ 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Automation: Code to detect the latest Data Dictionary version.

# current folder where Data Dictionary is stored: `doc/...`

  
  
  
  
  
  get_dd_from_api <- function(api_url, api_token) {
    res <- httr::POST(
      url = api_url,
      body = list(
        token        = api_token,
        content      = "metadata",
        format       = "csv",
        returnFormat = "csv"
      ),
      encode = "form",
      httr::config(ssl_verifypeer = TRUE)
    )
    
    if (httr::http_error(res)) {
      stop("REDCap metadata API request failed: ", httr::status_code(res))
    }
    
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    
    # REDCap liefert je nach System ; oder , als Delimiter in CSV-Exports.
    # Heuristik: nimm den Delimiter, der häufiger vorkommt in der Header-Zeile.
    header <- strsplit(txt, "\n", fixed = TRUE)[[1]][1]
    delim  <- if (stringr::str_count(header, ";") > stringr::str_count(header, ",")) ";" else ","
    
    dd <- readr::read_delim(
      file = I(txt),
      delim = delim,
      show_col_types = FALSE,
      guess_max = 4000
    ) %>%
      janitor::clean_names()
    
    # In REDCap heißt die Spalte typischerweise field_name
    if (!"field_name" %in% names(dd)) {
      # falls jemandes Export 1. Spalte Field Name ist, aber umbenannt wurde
      names(dd)[1] <- "field_name"
    }
    
    dd
  }
  

  get_dd_from_local <- function(folder_path) {
    files <- list.files(folder_path, full.names = TRUE,
                        pattern = "LymphomRegister_DataDictionary_.*\\.csv")
    if (length(files) == 0) stop("No local data dictionary files found in ", folder_path)
    
    dates <- as.Date(gsub(".*_(\\d{4}-\\d{2}-\\d{2})\\.csv", "\\1", files), format = "%Y-%m-%d")
    latest_file <- files[which.max(dates)]
    
    readr::read_delim(latest_file, delim = ";", guess_max = 4000, show_col_types = FALSE) %>%
      janitor::clean_names() %>%
      dplyr::rename(field_name = 1)
  }
  
  
  
  
  
  dd_source <- match.arg(dd_source)
  
  
  
  dd <- if (dd_source == "api") {
    tryCatch(
      get_dd_from_api(api_url, api_token),
      error = function(e) {
        message("⚠️ DD via API failed: ", conditionMessage(e), " -> using local DD.")
        get_dd_from_local(folder_path)
      }
    )
  } else {
    get_dd_from_local(folder_path)
  }
  message("✅ done: Raw data & Data Dictionary has been loaded.")
  
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
## ------------- Prepare Relevant Variables ----------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handle wrong number entries:
# Convert comma to point and change laboratory values to numeric


  d <- d %>%
    mutate(
      across(all_of(blood_marker), ~ as.numeric(gsub(",", ".", ., fixed = TRUE)))
    ) %>%
    mutate(
      across(all_of(blood_marker), ~ ifelse(. == 999, NA, .))
    )

  
  
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
## -------------- Data Splitting ----------------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  # Fill all empty rows with characteristic data 
  d_ <- d %>%
    group_by(record_id) %>%
    fill(last_name:platelet, .direction = "down") %>%
    fill() %>%
    fill(date_of_update:cause_of_death_other, .direction = "up") %>%
    dplyr::filter(diagnosis == "dlbcl") %>%
    ungroup() %>%
    dplyr::filter(stringr::str_detect(record_id, "^\\d+$")) %>%
    dplyr::mutate(record_id_num = as.numeric(record_id)) #%>%
    #dplyr::filter(record_id_num < 760) 
  
  
  
  ## Baseline date
  base <- d_ %>% filter(is.na(redcap_repeat_instance) & diagnosis == "dlbcl")
  
  
  ## Therapy data
  thx <- d_ %>%
    dplyr::filter(redcap_repeat_instrument == "medical_treatment" & record_id %in% base$record_id) %>%
    dplyr::select(record_id, last_name, first_name, datum_geb, 
           diagnosis, diagnosis_date, diagnosis_date_month, diagnosis_date_year, 
           cycle = redcap_repeat_instance, treat_y_n:medical_treatment_complete)
  
  
  ## Survival status data
  sv <- d_ %>%
    dplyr::filter(redcap_repeat_instrument == "survival_status" & record_id %in% base$record_id) %>%
    dplyr::select(record_id, cycle = redcap_repeat_instance, starts_with("dod_ic_date"), date_of_update:cause_of_death_other)



  return(list(d = d, dd = dd, base = base, thx = thx, sv = sv))
}
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

