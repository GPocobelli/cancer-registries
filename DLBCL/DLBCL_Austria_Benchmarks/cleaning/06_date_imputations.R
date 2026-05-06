

# scripts/06_date_imputations.R



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# --------------- Missing Date Variables ---------------------
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# 
#' Sections:
#'    > Imputation Methods:
#'             - Straight forward: 
#'                    Specific Days, 1, 15, 28 -> adjustable for the respective date variables
#'             
#'             - Likelihood function fitting combined with rule-based:
#'                    Distribution fitting for the available not missing date-data.
#'                    Rules: Diagnosis date comes after Therapy start, 
#'                           If day of diagnosis is missing: Therapy start - estimated time between Diag & Thx-start
#'                           Diagnosis month and Diagnosis Year is known: it has to be in this month and
#'                               in a specific estimated timespan.
#'
#'    > Flags, whether there were an imputation 
#'    
#' 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------------- 1. Straight Forward -----------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Diagnosis Date --------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#' Combine and impute diagnosis-related dates
#'
#' Combines full and partial (month/year) diagnosis dates and creates
#' imputed date variants using days 01, 15, and 28.
#'
#' @param base_ A data frame containing diagnosis date variables
#' (`*_date`, `*_date_month`, `*_date_year`).
#'
#' @return The input data frame with additional combined and imputed date columns.
#' @export
combine_diagnosis_dates <- function(base_) {
  
  
  required_cols <- c(
    "diagnosis_date", "diagnosis_date_month", "diagnosis_date_year",
    "diagnosis_transformation_date", "diagnosis_transformation_date_month",
    "diagnosis_transformation_date_year"
  )
  
  missing <- setdiff(required_cols, names(base_))
  if (length(missing) > 0) {
    stop(
      "combine_diagnosis_dates(): Missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  
  base1 <- base_ %>%
    
    add_imputed_dates(
        values = c("diagnosis_date", "diagnosis_transformation_date"),
        day_values = c("01","15","28")
      )
    

  
  
    base1
  
  
}














# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Therapy Dates ---------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#' Imputation of missing day values.
#' For sensitivity analysis & compare imputations (not known day for 01. / 15. / 28.).
#'
#' @param data
#' thx2 data from before
#'
#' @returns
#' Date columns with imputed versions if day value is missing: *_01, *_15, *_28 + *_imputed_day Flags.

impute_therapy_dates_fixed_day <- function(input) {
  
  spec <- list(
    list(prefix = "therapy_start_date",       condition = rlang::expr(treat_y_n == 1)),
    list(prefix = "therapy_end_date",         condition = rlang::expr(treat_y_n == 1)),
    list(prefix = "therapy_relapse_date",     condition = rlang::expr(therapy_relapse_y_n == "yes")),
    list(prefix = "progression_before_date",  condition = NULL),
    list(prefix = "treat_best_response_date", condition = NULL),
    list(prefix = "treat_trans_date",         condition = NULL),
    list(prefix = "progression_date",         condition = NULL),
    list(prefix = "treat_best_response_petct_date", condition = NULL)
    
  )
  
  output <- input
  for (s in spec) {
    output <- create_imputed_dates(
      data       = output,
      prefix     = s$prefix,
      day_values = c("01","15","28"),
      condition  = s$condition
    )
  }
  
  output
}




















# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------------- 2. Empirical Imputations ------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Build density function and pick one date for the missing ones. 
# Imputed date has to be within the given month (just day-date missing) 


#' Empirische Imputation for missing day values in `diagnosis_date` or `therapy_start_date`.
#'
#' Expected cols in `final`:
#'   - diagnosis_date, diagnosis_date_month, diagnosis_date_year
#'   - therapy_start_date, therapy_start_date_month, therapy_start_date_year
#'   - diag_ldh_ratio
#'   - drug_med_all
#'
#' Output:
#'   - diagnosis_date_imp, therapy_start_date_imp, delta_imp
#'   - sowie Hilfsvariablen: rchop_flag, ldh_group, Time_diag_to_txstart
#'
#' @param final       Dataframe (Join of base + thx + ggf. sv).
#' @param max_delta   Maximum delta value (days) for the density.
#' @param seed        Optional: set seed für reproducibility.
#'
#' @return Dataframe with imputed date columns.
#' @export
impute_dates_empirical <- function(final, max_delta = 365, seed = NULL,
                                   use_groups = TRUE) {
  
  if (!is.null(seed)) set.seed(seed)
  
  final2 <- final %>%
    dplyr::mutate(
      rchop_flag = !is.na(drug_med_all) & stringr::str_detect(drug_med_all, "\\bR-CHOP\\b"),
      therapy_start_date_ = safe_parse_partial_date(therapy_start_date),
      diagnosis_date_     = safe_parse_partial_date(diagnosis_date),
      therapy_end_date_   = safe_parse_partial_date(therapy_end_date),
      Time_diag_to_txstart = dplyr::case_when(
        !is.na(diagnosis_date_) & !is.na(therapy_start_date_) ~ as.numeric(therapy_start_date_ - diagnosis_date_),
        TRUE ~ NA_real_
      ),
      ldh_group = dplyr::case_when(
        !is.na(diag_ldh_ratio) & diag_ldh_ratio <= 1 ~ "low",
        !is.na(diag_ldh_ratio) & diag_ldh_ratio >  1 ~ "high",
        TRUE ~ NA_character_
      ),
      
      therapy_end_date_month = dplyr::if_else(is.na(therapy_end_date_) & !is.na(therapy_end_date_month), 
                                              therapy_end_date_month, NA_integer_)
      )
  
  # ---- Density basis: ONLY first line
  base_for_density <- final2 %>% dplyr::filter(treat_line == "1")
  
  dens_all <- safe_density(base_for_density, ldh = NULL, rchop = "any", fallback = NULL, max_delta = max_delta)
  if (is.null(dens_all)) stop("...")
  
  if (use_groups) {
    dens_high       <- safe_density(base_for_density, "high", "exclude", max_delta = max_delta, fallback = dens_all)
    dens_low        <- safe_density(base_for_density, "low",  "exclude", max_delta = max_delta, fallback = dens_all)
    dens_rchop_low  <- safe_density(base_for_density, "low",  "include", max_delta = max_delta, fallback = dens_all)
    dens_rchop_high <- safe_density(base_for_density, "high", "include", max_delta = max_delta, fallback = dens_all)
  } else {
    dens_high <- dens_low <- dens_rchop_low <- dens_rchop_high <- dens_all
  }
  
  final_imp <- final2 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # ---- Impute ONLY line 1
      needs_imp = treat_line == "1" && (
        (!is.na(therapy_start_date_) && is.na(diagnosis_date_) &&
           !is.na(diagnosis_date_month) && !is.na(diagnosis_date_year)) ||
          (!is.na(diagnosis_date_) && is.na(therapy_start_date_) &&
             !is.na(therapy_start_date_month) && !is.na(therapy_start_date_year))
      ),
      .dens = list({
        if (!use_groups) dens_all
        else if (isTRUE(rchop_flag)  && identical(ldh_group, "high")) dens_rchop_high
        else if (isTRUE(rchop_flag)  && identical(ldh_group, "low")) dens_rchop_low
        else if (!isTRUE(rchop_flag) && identical(ldh_group, "high")) dens_high
        else if (!isTRUE(rchop_flag) && identical(ldh_group, "low")) dens_low
        else dens_all
      }),
      imp = list(if (needs_imp) {
        impute_from_empirical_density(
          therapy_start_date    = therapy_start_date_,
          therapy_start_month   = therapy_start_date_month,
          therapy_start_year    = therapy_start_date_year,
          diagnosis_date        = diagnosis_date_,
          diagnosis_date_month  = diagnosis_date_month,
          diagnosis_date_year   = diagnosis_date_year,
          density               = .dens,
          therapy_end_date      = therapy_end_date_,
          therapy_end_month     = therapy_end_date_month,
          therapy_end_year      = therapy_end_date_year,
          max_iter              = 200L,
          enforce_rules         = TRUE,
          rchop_flag            = rchop_flag, 
          drug1_end_reason      = drug1_end_reason,
          planned_days          = c(100L, 150L)
        )
      } else {
        list(delta = NA_integer_, diagnosis_date = diagnosis_date_, therapy_start_date = therapy_start_date_, fallback = NA)
      }),
      diagnosis_date_density_imp     = imp$diagnosis_date,
      therapy_start_date_density_imp = imp$therapy_start_date,
      delta_imp                      = imp$delta,
      empirical_fallback             = imp$fallback
    ) %>%
    dplyr::ungroup() %>%
    
    # fill estimated diagnosis date for all lines, to properly calculate OS later
    dplyr::group_by(record_id) %>%
    tidyr::fill(
      diagnosis_date_density_imp,
      .direction = "downup"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-imp, -.dens, -needs_imp)
    
  
  final_imp
}



# 
# 
# final <- final %>%
#   dplyr::mutate(
#     rchop_flag = !is.na(drug_med_all) & stringr::str_detect(drug_med_all, "\\bR-CHOP\\b")
#   ) %>%
#   dplyr::mutate(
#     therapy_start_date = as.Date(therapy_start_date),
#     diagnosis_date     = as.Date(diagnosis_date),
#     Time_diag_to_txstart = dplyr::case_when(
#       !is.na(diagnosis_date) & !is.na(therapy_start_date) ~ as.numeric(therapy_start_date - diagnosis_date),
#       TRUE ~ NA_real_),
#     ldh_group = dplyr::case_when(
#       !is.na(diag_ldh_ratio) & diag_ldh_ratio <= 1 ~ "low",
#       !is.na(diag_ldh_ratio) & diag_ldh_ratio >  1 ~ "high",
#       TRUE ~ NA_character_
#     )
#   )  
#   
# 
# 
# 
# df_high <- create_filtered_dataset(final, ldh_group_value = "high", rchop = "exclude", max_delta = 150)
# dens_high <- fit_empirical_density(df_high, max_delta = 150)
# 
# df_low  <- create_filtered_dataset(final, ldh_group_value = "low",  rchop = "exclude", max_delta = 150)
# dens_low <- fit_empirical_density(df_low, max_delta = 150)
# 
# 
# 
# 
# df_rchop_low  <- create_filtered_dataset(final, ldh_group_value = "low",  rchop = "include", max_delta = 150)
# dens_rchop_low <- fit_empirical_density(df_rchop_low, max_delta = 150)
# 
# df_rchop_high <- create_filtered_dataset(final, ldh_group_value = "high", rchop = "include", max_delta = 150)
# dens_rchop_high <- fit_empirical_density(df_rchop_high, max_delta = 150)
# 
# 
# 
# 
# final_imp <- final %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(
#     .dens = list(dplyr::case_when(
#       rchop_flag & ldh_group == "high" ~ dens_rchop_high,
#       rchop_flag & ldh_group == "low"  ~ dens_rchop_low,
#       !rchop_flag & ldh_group == "high" ~ dens_high,
#       !rchop_flag & ldh_group == "low"  ~ dens_low,
#       TRUE ~ dens_high
#     )),
#     imp = list(
#       impute_from_empirical_density(
#         therapy_start_date  = therapy_start_date,
#         therapy_start_month = therapy_start_date_month,
#         therapy_start_year  = therapy_start_date_year,
#         diagnosis_date      = diagnosis_date,
#         diagnosis_month     = diagnosis_date_month,
#         diagnosis_year      = diagnosis_date_year,
#         density             = .dens[[1]]
#       )
#     ),
#     diagnosis_date_imp     = imp$diagnosis_date,
#     therapy_start_date_imp = imp$therapy_start_date,
#     delta_imp              = imp$delta
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-imp, -.dens)
# 
# 
