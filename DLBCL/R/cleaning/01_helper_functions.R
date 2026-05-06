# scripts/cleaning/helper_functions.R



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# --------------- Helper Functions -----------------------------
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# Reproducable Code
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Date Imputations -----------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### --------------- 1. Straight Forward ---------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# Hilfsfunktion: macht aus einem Datum (voll oder Monat/Jahr) einen "nk/mm/yyyy" bzw. "dd/mm/yyyy"-String
#' Title
#'
#' @param date 
#' @param month 
#' @param year 
#'
#' @returns
#' @export
#'
#' @examples
combine_partial_full_date_str <- function(date, month, year) {
  # date kann Date oder character ("YYYY-mm-dd") sein
  date_parsed <- as.Date(date)  # robust für Date + ISO-Strings; NA bleibt NA
  
  dplyr::case_when(!is.na(month) & !is.na(year) & is.na(date_parsed) ~ paste("nk", month, year, sep = "/"),
                   !is.na(date_parsed)                               ~ format(date_parsed, "%d/%m/%Y"),
                   TRUE                                              ~ NA_character_
  )
}








# Hauptfunktion: kombiniert + imputiert (01/15/28) + Flag
#' Title
#'
#' @param data 
#' @param prefix 
#' @param day_values 
#' @param after 
#'
#' @returns
#' @export
#'
#' @examples
create_imputed_dates <- function(data, prefix,
                                   day_values = c("01","15","28"),
                                   after = NULL,
                                   condition = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(prefix), length(prefix) == 1)
  
  date_col  <- prefix
  month_col <- paste0(prefix, "_month")
  year_col  <- paste0(prefix, "_year")
  
  combined_str_col <- paste0(prefix)
  flag_col         <- paste0(prefix, "_imputed_day")
  
  date_sym  <- rlang::sym(date_col)
  month_sym <- rlang::sym(month_col)
  year_sym  <- rlang::sym(year_col)
  
  # condition -> logischer Vektor (Länge = nrow(data))
  if (is.null(condition)) {
    cond_vec <- rep(TRUE, nrow(data))
  } else {
    cond_q <- rlang::as_quosure(condition, env = rlang::caller_env())
    cond_vec <- rlang::eval_tidy(cond_q, data = data)
    if (!is.logical(cond_vec) || length(cond_vec) != nrow(data)) {
      stop("condition must evaluate to a logical vector of length nrow(data).", call. = FALSE)
    }
  }
  
  out <- data %>%
    dplyr::mutate(
      "{combined_str_col}" := {
        x <- combine_partial_full_date_str(
          date  = !!date_sym,
          month = !!month_sym,
          year  = !!year_sym
        )
        dplyr::if_else(cond_vec, x, NA_character_)
      }
    )
  
  if (is.null(after)) after <- date_col
  
  out <- out %>%
    dplyr::mutate(
      "{flag_col}" := dplyr::if_else(
        cond_vec,
        stringr::str_detect(.data[[combined_str_col]], "^nk/"),
        NA
      ),
      .after = dplyr::all_of(after)
    )
  
  for (dv in day_values) {
    new_col <- paste0(prefix, "_", dv)
    out <- out %>%
      dplyr::mutate(
        "{new_col}" := dplyr::if_else(
          cond_vec,
          as.Date(
            stringr::str_replace(.data[[combined_str_col]], "^nk/", paste0(dv, "/")),
            format = "%d/%m/%Y"
          ),
          as.Date(NA)
        )
      )
  }
  
  out
}







# Convenience-Wrapper: wende es gleich auf mehrere Prefixe an
#' Title
#'
#' @param data 
#' @param prefixes 
#' @param day_values 
#'
#' @returns
#' @export
#'
#' @examples
add_imputed_dates <- function(data, values, day_values = c("01","15","28")) {

  out <- data
  
  if (is.character(values)) {
    values <- lapply(values, \(x) list(prefix = x))
  }
  
  
  for (v in values) {
    out <- create_imputed_dates(
      out,
      prefix     = v$prefix,
      day_values = day_values,
      after      = v$after %||% NULL,
      condition  = v$condition %||% NULL
    )
  }
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x















# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### --------------- 2. Empirical Imputations ----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


safe_parse_partial_date <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "na", "n/a", "NULL", "null")] <- NA_character_
  x[grepl("^nk/", x)] <- NA_character_
  
  out <- rep(as.Date(NA), length(x))
  fmts <- c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y", "%Y/%m/%d")
  
  for (i in seq_along(x)) {
    xi <- x[i]
    if (is.na(xi)) next
    
    parsed <- as.Date(NA)
    for (fmt in fmts) {
      tmp <- suppressWarnings(as.Date(xi, format = fmt))
      if (!is.na(tmp)) {
        parsed <- tmp
        break
      }
    }
    out[i] <- parsed
  }
  
  out
}

















#' Title
#'
#' @param df 
#' @param ldh 
#' @param rchop 
#' @param fallback 
#'
#' @returns
#' @export
#'
#' @examples
safe_density <- function(df, ldh, rchop, max_delta = 150, fallback = NULL) {
  tryCatch(
    {
      dfiltered <- create_filtered_dataset(
        df,
        ldh_group_value = ldh,
        rchop = rchop,
        max_delta = max_delta
      )
      fit_empirical_density(dfiltered, max_delta = max_delta)
    },
    error = function(e) {
      message("safe_density failed for ldh=", ldh, ", rchop=", rchop, ": ", conditionMessage(e))
      fallback
    }
  )
}









#' Makes sure, that the estimated date is in the given month/year 
#'
#' @param date      estimated date
#' @param m         given month from: *_date_month
#' @param y         given month from: *_date_year
#'
#' @export
in_month_year <- function(date, m, y) {
  
  !is.na(date) && !is.na(m) && !is.na(y) &&
    lubridate::month(date) == as.integer(m) &&
    lubridate::year(date)  == as.integer(y)
}








#' Title
#'
#' @param dfiltered 
#' @param max_delta 
#' @param n 
#'
#' @returns
#' @export
fit_empirical_density <- function(dfiltered, max_delta = 150, n = 2048) {
  
  density(dfiltered, from = 0, to = max_delta, n = n, na.rm = TRUE)
}








#' Title
#'
#' @param dens 
#'
#' @returns
#' @export
sample_from_density <- function(dens) {
  
  probs <- dens$y
  probs[!is.finite(probs)] <- 0
  
  if (sum(probs) <= 0) stop("Density is not positive!")
  
  as.integer(round(sample(dens$x, size = 1, replace = TRUE, prob = probs)))
}











#'  Filtering from raw data set: just **non-missing** data, to create a density to estimate missing diagnosis_date
#'  or therapy_start_date data. 
#'  From literature: R-CHOP therapy & ldh (high/low -> from `diag_ldh_ratio`) has a relevant difference for 
#'
#' @param df            raw data frame
#' @param ldh_group     which ldh-group is used? (low / high)
#' @param rchop         including/excluding R-CHOP therapy 
#' @param max_delta     maximum x-achsis
#'
#' @returns
#' @export
create_filtered_dataset <- function(df,
                                    ldh_group_value  = NULL,
                                    rchop = c("any", "include", "exclude"),
                                    max_delta = 150) {
  
  rchop <- match.arg(rchop)
  
  out <- df %>%
    filter(!is.na(Time_diag_to_txstart), Time_diag_to_txstart >= 0, Time_diag_to_txstart < max_delta)
  
  if (!is.null(ldh_group_value )) {
    out <- out %>% filter(ldh_group == ldh_group_value )
  }
  
  if (rchop == "exclude") {
    out <- out %>%
      dplyr::filter(is.na(drug_med_all) | !stringr::str_detect(drug_med_all, "\\bR-CHOP\\b"))
  } 
  else if (rchop == "include") {
    out <- out %>%
      dplyr::filter(!is.na(drug_med_all) & stringr::str_detect(drug_med_all, "\\bR-CHOP\\b"))
  }
  
  
  dfiltered <- out$Time_diag_to_txstart
  dfiltered <- dfiltered[is.finite(dfiltered)]
  
  if (length(dfiltered) == 0) {
    stop("dfiltered is empty. Check: ldh_group/rchop/max_delta and data")
  }
  
  if (length(dfiltered) < 5) {
    stop("dfiltered is to small to build a density.")
  }
  
  dfiltered
}
















month_days <- function(year, month) {
  first <- as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
  last  <- seq(first, by = "1 month", length.out = 2)[2] - 1
  seq(first, last, by = "1 day")
}

condition_density_on_interval <- function(dens, lo, hi) {
  keep <- dens$x >= lo & dens$x <= hi
  if (!any(keep)) return(NULL)
  x <- dens$x[keep]
  y <- dens$y[keep]
  s <- sum(y)
  if (!is.finite(s) || s <= 0) return(NULL)
  dens2 <- dens
  dens2$x <- x
  dens2$y <- y / s
  dens2
}











first_day_of_month <- function(y, m) {
  as.Date(sprintf("%04d-%02d-01", as.integer(y), as.integer(m)))
}









check_rules <- function(diag, therapy_start, therapy_end,
                        therapy_end_month = NA, therapy_end_year = NA) {
  
  diag     <- as.Date(diag)
  therapy_start <- as.Date(therapy_start)
  therapy_end   <- as.Date(therapy_end)
  
  # Wenn Enddatum fehlt, aber Monat/Jahr vorhanden: Proxy = 01.<Monat>.<Jahr>
  if (is.na(therapy_end) && !is.na(therapy_end_month) && !is.na(therapy_end_year)) {
    therapy_end <- first_day_of_month(therapy_end_year, therapy_end_month)
  }
  
  ok <- TRUE
  if (!is.na(diag) && !is.na(therapy_start)) ok <- ok && (diag <= therapy_start)
  if (!is.na(therapy_start) && !is.na(therapy_end)) ok <- ok && (therapy_start <= therapy_end)
  ok
}










#' Impute candidates from empirical density function for missing `diagnosis_date` or `therapy_start_date`
#' Idea: we know roughly how much time is between diagnosis_date & therapy_start_date. 
#' So we can estimate from the given data (empirical density).
#'
#' @param therapy_start_date 
#' @param therapy_start_month 
#' @param therapy_start_year 
#' @param diagnosis_date 
#' @param diagnosis_month 
#' @param diagnosis_year 
#' @param dfiltered              Filterd data set. 
#'                               From literature: R-CHOP therapy & ldh (high/low -> from `diag_ldh_ratio`) 
#'                               has a relevant difference for time between diagnosis_date & therapy_start_date 
#' @param max_iter 
#'
#' @returns
#' @export
impute_from_empirical_density <- function(therapy_start_date, therapy_start_month, therapy_start_year,
                                          diagnosis_date, diagnosis_date_month, diagnosis_date_year,
                                          density,
                                          therapy_end_date = NA, therapy_end_month = NA, therapy_end_year = NA,
                                          rchop_flag = FALSE,
                                          drug1_end_reason = NA_character_,
                                          planned_days = c(100L, 150L),
                                          max_iter = 200L,
                                          enforce_rules = TRUE) {
  
  

  
  therapy_start_date <- as.Date(therapy_start_date)
  diagnosis_date     <- as.Date(diagnosis_date)
  therapy_end_date   <- as.Date(therapy_end_date)
  
  
  
  
  # Effektives Enddatum für Checks/Clamping (Proxy falls nötig)
  therapy_end_eff <- therapy_end_date
  if (is.na(therapy_end_eff) && !is.na(therapy_end_month) && !is.na(therapy_end_year)) {
    therapy_end_eff <- first_day_of_month(therapy_end_year, therapy_end_month)
  }
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Case 1: therapy known, diagnosis day missing but month/year known
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.na(therapy_start_date) &&
      is.na(diagnosis_date) &&
      !is.na(diagnosis_date_month) && !is.na(diagnosis_date_year)) {
    
    cand_days <- month_days(diagnosis_date_year, diagnosis_date_month)
    lo <- as.numeric(therapy_start_date - max(cand_days))
    hi <- as.numeric(therapy_start_date - min(cand_days))
    
    dens_c <- condition_density_on_interval(density, lo, hi)
    
    if (is.null(dens_c)) stop("No feasible density mass for Case 1 (conditioning interval empty).")
    
    if (!is.na(therapy_end_eff) && therapy_start_date > therapy_end_eff) {
      stop("Inconsistent data: therapy_start_date is after (effective) therapy_end_date.")
    }
    
    
    for (i in seq_len(max_iter)) {
      delta <- sample_from_density(dens_c)
      diag_date <- therapy_start_date - delta
      
      if (!(diag_date %in% cand_days)) {
        diag_date <- cand_days[which.min(abs(as.numeric(cand_days - diag_date)))]
        delta <- as.integer(therapy_start_date - diag_date)
      }
      
      if (!enforce_rules || check_rules(
        diag = diag_date,
        therapy_start = therapy_start_date,
        therapy_end = therapy_end_date,
        therapy_end_month = therapy_end_month,
        therapy_end_year  = therapy_end_year
      )) {
        return(list(
          delta = as.integer(delta),
          diagnosis_date = diag_date,
          therapy_start_date = therapy_start_date,
          fallback = FALSE
        ))
      }
    }
    
    stop("No valid imputation found within max_iter for Case 1.")
  }
  
  

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Case 2: diagnosis known, therapy day missing but month/year known
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.na(diagnosis_date) &&
      is.na(therapy_start_date) &&
      !is.na(therapy_start_month) && !is.na(therapy_start_year)) {
    
    # Monat als HARTE Priorität
    cand_days <- month_days(therapy_start_year, therapy_start_month)
    
    # Hard rule: diag <= start
    cand_days <- cand_days[cand_days >= diagnosis_date]
    
    # Hard rule: start <= end_eff (falls vorhanden)
    if (!is.na(therapy_end_eff)) {
      cand_days <- cand_days[cand_days <= therapy_end_eff]
    }
    
    if (length(cand_days) == 0) {
      stop("No feasible start day left in therapy_start month after hard constraints.")
    }
    
    # Soft constraint: R-CHOP planned duration window (nur wenn end_eff vorhanden)
    use_planned <- isTRUE(rchop_flag) &&
      identical(drug1_end_reason, "Planned end") &&
      !is.na(therapy_end_eff)
    
    if (use_planned) {
      dur <- as.integer(therapy_end_eff - cand_days)
      cand_days_planned <- cand_days[dur >= planned_days[1] & dur <= planned_days[2]]
      if (length(cand_days_planned) > 0) cand_days <- cand_days_planned
    }
    
    lo <- as.numeric(min(cand_days) - diagnosis_date)
    hi <- as.numeric(max(cand_days) - diagnosis_date)
    
    dens_c <- condition_density_on_interval(density, lo, hi)
    if (is.null(dens_c)) stop("No feasible density mass for Case 2 (conditioning interval empty).")
    
    for (i in seq_len(max_iter)) {
      delta <- sample_from_density(dens_c)
      tx_date <- diagnosis_date + delta
      
      if (!(tx_date %in% cand_days)) {
        tx_date <- cand_days[which.min(abs(as.numeric(cand_days - tx_date)))]
        delta <- as.integer(tx_date - diagnosis_date)
      }
      
      if (!enforce_rules || check_rules(
        diag = diagnosis_date,
        therapy_start = tx_date,
        therapy_end = therapy_end_date,
        therapy_end_month = therapy_end_month,
        therapy_end_year  = therapy_end_year
      )) {
        return(list(
          delta = as.integer(delta),
          diagnosis_date = diagnosis_date,
          therapy_start_date = tx_date,
          fallback = FALSE
        ))
      }
    }
    
    stop("No valid imputation found within max_iter for Case 2.")
  }
  
  list(
    delta = NA_integer_,
    diagnosis_date = diagnosis_date,
    therapy_start_date = therapy_start_date,
    fallback = NA
  )
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Therapy - Flags for Analysis later -------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



create_treatment_flags <- function(data) {
  
  library(dplyr)
  library(stringr)
  
  data %>%
    mutate(
      drug_med_all_line = coalesce(drug_med_all_line, ""),
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 1) Normalisierung
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      drug_clean = drug_med_all_line %>%
        str_replace_all(regex("non-pegylated liposomalem", ignore_case = TRUE),
                        "non-pegylated liposomal") %>%
        str_replace_all(regex("pola-r-cmp", ignore_case = TRUE), "pola-r-chp") %>%
        str_replace_all(regex("obi-bendamustin", ignore_case = TRUE), "obinutuzumab, bendamustine") %>%
        str_replace_all(regex("revlimid", ignore_case = TRUE), "lenalidomide") %>%
        str_squish(),
      
      drug_upper = str_to_upper(drug_clean),
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 2) Basisflags
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      empty_flag = drug_upper == "",
      
      bsc_any_flag = str_detect(
        drug_upper,
        regex("\\bBEST SUPPORTIVE CARE\\b|\\bBSC\\b", ignore_case = TRUE)
      ),
      
      rt_flag = str_detect(
        drug_upper,
        regex("\\bRADIOTHERAPY\\b", ignore_case = TRUE)
      ),
      
      intrathecal_flag = str_detect(
        drug_upper,
        regex("\\bINTRATHECAL THERAPY\\b", ignore_case = TRUE)
      ),
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 3) Frontline-Standardregime
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # rchop_flag = str_detect(
      #   drug_upper,
      #   regex("\\bR-CHOP\\b", ignore_case = TRUE)
      # ),
      anthracyclin_flag = str_detect(
        drug_med_all_line,
        regex(
          paste(
            # Einzelsubstanzen
            "\\bDoxorubicin\\b",
            "\\bEpirubicin\\b",
            "\\bIdarubicin\\b",
            "\\bNon-pegylated liposomal(?:em)? Doxorubicin\\b",
            "\\bLiposomal Doxorubicin\\b",
            
            # Kombiregime
            "\\bR-?CHOP\\b",
            "\\bCHOP\\b",
            "\\bR-?CHP\\b",
            "\\bCHP\\b",
            "\\bR-?EPOCH\\b",
            "\\bDA-?R-?EPOCH\\b",
            "\\bR-?COMP\\b",
            "\\bCOMP\\b",
            "\\bPOLA-?R-?CHP\\b",
            "\\bPOLA-?R-?CMP\\b",
            sep = "|"
          ),
          ignore_case = TRUE
        )
      ),
      
      rcomp_flag = str_detect(
        drug_upper,
        regex("\\bR-COMP\\b", ignore_case = TRUE)
      ),
      
      repoch_flag = str_detect(
        drug_upper,
        regex("\\bDA-R-EPOCH\\b|\\bR-DA-EPOCH\\b|\\bR-EPOCH\\b|\\bEPOCH\\b", ignore_case = TRUE)
      ),
      
      pola_rchp_flag = str_detect(
        drug_upper,
        regex("\\bPOLA-R-CHP\\b|\\bPOLA-R-CHOP\\b", ignore_case = TRUE)
      ),
      
      chop_flag = str_detect(
        drug_upper,
        regex("\\bCHOP\\b", ignore_case = TRUE)
      ) & !rchop_flag,
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 4) Transplant / intensivere Verfahren
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      asct_flag = str_detect(
        drug_upper,
        regex("\\bAUTOLOGOUS-HCT\\b|\\bAUTOLOGOUS HCT\\b|\\bHDT\\b|\\bASCT\\b", ignore_case = TRUE)
      ),
      
      allo_flag = str_detect(
        drug_upper,
        regex("\\bALLOGENEIC-HCT\\b|\\bALLOGENEIC HCT\\b", ignore_case = TRUE)
      ),
      
      beam_flag = str_detect(
        drug_upper,
        regex("\\bBEAM\\b", ignore_case = TRUE)
      ),
      
      salvage_gdp_flag = str_detect(
        drug_upper,
        regex("\\bGDP\\b|\\bR-GDP\\b", ignore_case = TRUE)
      ),
      
      salvage_dhap_flag = str_detect(
        drug_upper,
        regex("\\bDHAP\\b|\\bR-DHAP\\b", ignore_case = TRUE)
      ),
      
      salvage_ice_flag = str_detect(
        drug_upper,
        regex("\\bICE\\b|\\bR-ICE\\b|\\bESHAP\\b", ignore_case = TRUE)
      ),
      
      matrix_flag = str_detect(
        drug_upper,
        regex("\\bMATRIX\\b", ignore_case = TRUE)
      ),
      
      gemox_flag = str_detect(
        drug_upper,
        regex("\\bGEMOX\\b|\\bR-GEMOX\\b", ignore_case = TRUE)
      ),
      
      # intensive Salvage-Chemotherapie
      intensive_chemo_flag = salvage_gdp_flag |
        salvage_dhap_flag |
        salvage_ice_flag |
        matrix_flag |
        repoch_flag,
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 5) Chemotherapie / palliativ
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      benda_flag = str_detect(
        drug_upper,
        regex("\\bBENDAMUSTINE\\b|\\bR-BENDAMUSTIN\\b|\\bR-BENDAMUSTINE\\b|\\bBR\\b", ignore_case = TRUE)
      ) & !pola_rchp_flag,
      
      gemcitabine_flag = str_detect(
        drug_upper,
        regex("\\bGEMCITABINE\\b", ignore_case = TRUE)
      ),
      
      pixantrone_flag = str_detect(
        drug_upper,
        regex("\\bPIXANTRONE\\b", ignore_case = TRUE)
      ),
      
      paclitaxel_flag = str_detect(
        drug_upper,
        regex("\\bPACLITAXEL\\b", ignore_case = TRUE)
      ),
      
      nonintensive_chemo_flag = benda_flag |
        gemox_flag |
        pixantrone_flag |
        gemcitabine_flag |
        paclitaxel_flag,
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 6) Moderne / zielgerichtete Therapien
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      pola_flag = str_detect(
        drug_upper,
        regex("\\bPOLATUZUMAB\\b|\\bPOLA\\b", ignore_case = TRUE)
      ),
      
      tafa_flag = str_detect(
        drug_upper,
        regex("\\bTAFASITAMAB\\b", ignore_case = TRUE)
      ),
      
      glofi_flag = str_detect(
        drug_upper,
        regex("\\bGLOFITAMAB\\b", ignore_case = TRUE)
      ),
      
      epco_flag = str_detect(
        drug_upper,
        regex("\\bEPCORITAMAB\\b", ignore_case = TRUE)
      ),
      
      mosun_flag = str_detect(
        drug_upper,
        regex("\\bMOSUNETUZUMAB\\b", ignore_case = TRUE)
      ),
      
      bispecific_flag = glofi_flag | epco_flag | mosun_flag,
      
      cart_flag = str_detect(
        drug_upper,
        regex("\\bAXI-CEL\\b|\\bTISA-CEL\\b|\\bLISO-CEL\\b|\\bBREXU-CEL\\b|\\bCAR-T\\b|\\bCAR T\\b", ignore_case = TRUE)
      ),
      
      acala_flag = str_detect(
        drug_upper,
        regex("\\bACALABRUTINIB\\b", ignore_case = TRUE)
      ),
      
      ibrutinib_flag = str_detect(
        drug_upper,
        regex("\\bIBRUTINIB\\b", ignore_case = TRUE)
      ),
      
      btk_flag = acala_flag | ibrutinib_flag,
      
      veneto_flag = str_detect(
        drug_upper,
        regex("\\bVENETOCLAX\\b", ignore_case = TRUE)
      ),
      
      len_flag = str_detect(
        drug_upper,
        regex("\\bLENALIDOMIDE\\b|\\bR-LENALIDOMIDE\\b|\\bPOMALIDOMID\\b", ignore_case = TRUE)
      ),
      
      checkpoint_flag = str_detect(
        drug_upper,
        regex("\\bPEMBROLIZUMAB\\b|\\bNIVOLUMAB\\b|\\bDURVALUMAB\\b|\\bSPARTALIZUMAB\\b|\\bTAMINADENANT\\b", ignore_case = TRUE)
      ),
      
      other_targeted_flag = str_detect(
        drug_upper,
        regex("\\bSELINEXOR\\b|\\bIBRITUMOMAB\\b|\\bZEVALIN\\b|\\bEVUSHELD\\b", ignore_case = TRUE)
      ),
      
      targeted_flag = cart_flag |
        pola_flag |
        tafa_flag |
        bispecific_flag |
        btk_flag |
        veneto_flag |
        len_flag |
        checkpoint_flag |
        other_targeted_flag,
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 7) Monotherapien / Antikörper
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ritux_only_flag = str_detect(
        drug_upper,
        regex("^\\s*RITUXIMAB\\s*$", ignore_case = TRUE)
      ),
      
      antibody_only_flag = str_detect(
        drug_upper,
        regex("^\\s*(RITUXIMAB|OBINUTUZUMAB|OFATUMUMAB)\\s*$", ignore_case = TRUE)
      ),
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 8) BSC / keine Therapie
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      any_systemic_flag = rchop_flag | rcomp_flag | repoch_flag | pola_rchp_flag |
        asct_flag | allo_flag | intensive_chemo_flag | nonintensive_chemo_flag |
        targeted_flag | ritux_only_flag | antibody_only_flag | chop_flag,
      
      no_treatment_bsc_flag = empty_flag |
        ((bsc_any_flag | treat_y_n == "no") & !any_systemic_flag)
    ) %>%
    
    mutate(
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 9) Hauptgruppe für Tabellen
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      therapy_group = case_when(
        no_treatment_bsc_flag ~ "No treatment/BSC",
        
        treat_line == 1 & (rchop_flag | rcomp_flag | repoch_flag | pola_rchp_flag | asct_flag | chop_flag) ~ 
          "Any treatment",
        
        treat_line >= 2 & asct_flag ~ "Any treatment",
        treat_line >= 2 & intensive_chemo_flag ~ "Any treatment",
        treat_line >= 2 & nonintensive_chemo_flag ~ "Any treatment",
        treat_line >= 2 & targeted_flag ~ "Any treatment",
        treat_line >= 2 & ritux_only_flag ~ "Any treatment",
        
        TRUE ~ "Other"
      ),
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 10) Untergruppe für spätere Tabellenausgabe
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      therapy_subgroup = case_when(
        no_treatment_bsc_flag ~ "No treatment/BSC",
        
        # ---------- First line ----------
        treat_line == 1 & pola_rchp_flag ~ "Pola-R-CHP",
        treat_line == 1 & repoch_flag ~ "R-DA-EPOCH/EPOCH",
        treat_line == 1 & rchop_flag ~ "R-CHOP",
        treat_line == 1 & rcomp_flag ~ "R-COMP",
        treat_line == 1 & asct_flag ~ "HDT + ASCT consolidation",
        treat_line == 1 & chop_flag ~ "CHOP",
        treat_line == 1 ~ "Other",
        
        # ---------- 2L+ intensive ----------
        treat_line >= 2 & asct_flag ~ "HDT + ASCT consolidation",
        treat_line >= 2 & salvage_gdp_flag ~ "GDP/R-GDP",
        treat_line >= 2 & salvage_dhap_flag ~ "DHAP/R-DHAP",
        treat_line >= 2 & salvage_ice_flag ~ "ICE/R-ICE/ESHAP",
        treat_line >= 2 & matrix_flag ~ "MATRIX-based intensive chemotherapy",
        treat_line >= 2 & repoch_flag ~ "R-DA-EPOCH/EPOCH",
        
        # ---------- 2L+ non-intensive chemo ----------
        treat_line >= 2 & benda_flag ~ "R-Bendamustine/BR",
        treat_line >= 2 & gemox_flag ~ "GEMOX/R-GEMOX",
        treat_line >= 2 & pixantrone_flag ~ "Pixantrone-based",
        treat_line >= 2 & gemcitabine_flag ~ "Gemcitabine-based",
        treat_line >= 2 & paclitaxel_flag ~ "Paclitaxel-based",
        
        # ---------- 2L+ targeted / modern ----------
        treat_line >= 2 & cart_flag ~ "CAR T-cell therapy",
        treat_line >= 2 & tafa_flag ~ "Tafasitamab-based",
        treat_line >= 2 & glofi_flag ~ "Glofitamab-based",
        treat_line >= 2 & epco_flag ~ "Epcoritamab-based",
        treat_line >= 2 & mosun_flag ~ "Mosunetuzumab-based",
        treat_line >= 2 & pola_flag ~ "Polatuzumab-based",
        treat_line >= 2 & btk_flag ~ "BTK inhibitor-based",
        treat_line >= 2 & veneto_flag ~ "Venetoclax-based",
        treat_line >= 2 & len_flag ~ "IMiD-based",
        treat_line >= 2 & checkpoint_flag ~ "Checkpoint inhibitor-based",
        treat_line >= 2 & other_targeted_flag ~ "Other targeted/novel therapy",
        
        # ---------- antibody / mono ----------
        treat_line >= 2 & ritux_only_flag ~ "Rituximab only",
        treat_line >= 2 & antibody_only_flag ~ "Antibody only",
        
        TRUE ~ "Other"
      ),
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 11) Überkategorie für 2L+ wie in deiner Tabelle
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      therapy_category = case_when(
        no_treatment_bsc_flag ~ "No treatment/BSC",
        
        treat_line == 1 ~ "Frontline treatment",
        
        treat_line >= 2 & asct_flag ~ "Intensive chemotherapy (curative)",
        treat_line >= 2 & intensive_chemo_flag ~ "Intensive chemotherapy (curative)",
        
        treat_line >= 2 & nonintensive_chemo_flag ~ "Non-intensive chemotherapy (palliative)",
        
        treat_line >= 2 & (
          cart_flag | pola_flag | tafa_flag | bispecific_flag |
            btk_flag | veneto_flag | len_flag | checkpoint_flag |
            other_targeted_flag
        ) ~ "Targeted / novel therapy",
        
        treat_line >= 2 & ritux_only_flag ~ "Antibody / low-intensity therapy",
        
        TRUE ~ "Other"
      ),
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 12) Intention
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      treatment_intention = case_when(
        treat_line == 1 ~ "-",
        
        therapy_category == "Intensive chemotherapy (curative)" ~ "Curative",
        therapy_category == "Non-intensive chemotherapy (palliative)" ~ "Palliative",
        therapy_category == "Targeted / novel therapy" ~ "Palliative",
        no_treatment_bsc_flag ~ "Palliative",
        
        TRUE ~ NA_character_
      )
    )
}
