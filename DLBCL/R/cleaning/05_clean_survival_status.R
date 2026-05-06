

# scripts/cleaning/04_clean_survival_status.R




# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# --------------- Clean Survival Status -----------------------------
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# Sections:
#    > 
#    
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>















# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Survival Status -------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Preparation of the survival status data.
#'
#' @param sv 
#' RedCap data set of the filtered version `redcap_repeat_instrument == "survival_status" & record_id %in% base$record_id`
#'
#' @returns
#' Same data set with transformed survival status colums `surv_stat`, `status_date`, `surv_lost2fu_y_n`,
#' @export
prepare_survival_status <- function(sv) {
  
  sv1 <- sv %>%
    dplyr::mutate(
      surv_dod_date = as.Date(surv_dod_date),
      dod_ic_date   = as.Date(dod_ic_date),
      last_contact_date = as.Date(last_contact_date),
      progression_sv_last_date = as.Date(progression_sv_last_date)
    ) %>%
    # combine date of death from patient characteristics and survival status
    dplyr::mutate(
      surv_dod_date = dplyr::coalesce(surv_dod_date, dod_ic_date),
      progression_sv_last_date_month = dplyr::coalesce(progression_sv_last_date_month, dod_ic_date_month),
      progression_sv_last_date_year  = dplyr::coalesce(progression_sv_last_date_year,  dod_ic_date_year)
    ) %>%
    # surv_stat harmonisieren (Lost2FU)
    dplyr::mutate(
      surv_stat = dplyr::case_when(
        surv_stat %in% c("nd", "unknown") ~ "Lost2FU",
        dplyr::coalesce(surv_lost2fu_y_n == 1, FALSE) ~ "Lost2FU",
        TRUE ~ surv_stat
      ),
      .after = surv_stat
    ) %>%
    # status_date + month/year konsistent bauen
    dplyr::mutate(
      status_date = dplyr::case_when(
        surv_stat %in% c("alive","Lost2FU") & !is.na(last_contact_date) ~ last_contact_date,
        surv_stat == "deceased"             & !is.na(surv_dod_date)     ~ surv_dod_date,
        TRUE ~ as.Date(NA)
      ),
      status_date_month = dplyr::case_when(
        surv_stat %in% c("alive","Lost2FU") & is.na(last_contact_date) &
          !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_month,
        surv_stat == "deceased" & is.na(surv_dod_date) &
          !is.na(surv_dod_date_month) & !is.na(surv_dod_date_year) ~ surv_dod_date_month,
        TRUE ~ NA_real_
      ),
      status_date_year = dplyr::case_when(
        surv_stat %in% c("alive","Lost2FU") & is.na(last_contact_date) &
          !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_year,
        surv_stat == "deceased" & is.na(surv_dod_date) &
          !is.na(surv_dod_date_month) & !is.na(surv_dod_date_year) ~ surv_dod_date_year,
        TRUE ~ NA_real_
      )
    )
  
  # Jetzt: status_date_ und _01/_15/_28 generisch erzeugen
  # (prefix = "status_date" erwartet status_date_month/status_date_year existieren -> tun sie)
  sv1 <- create_imputed_dates(
    data   = sv1,
    prefix = "status_date",
    day_values = c("01","15","28"),
    after = "status_date"
  )
  
  # letztes Update pro record_id anhand status_date_15
  sv_latest <- sv1 %>%
    dplyr::arrange(record_id, dplyr::desc(cycle)) %>%
    dplyr::group_by(record_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # Patienten ohne status_date_15 behalten (wie bei dir)
  sv2 <- dplyr::bind_rows(
    sv_latest,
    sv1 %>% dplyr::filter(!record_id %in% sv_latest$record_id)
  )
  
  # optionaler Check (wie bei dir)
  if (length(unique(sv$record_id)) == length(unique(sv2$record_id))) {
    message("All patient included")
  } else {
    warning("Missing patients, please check", call. = FALSE)
  }
  
  # progression_sv_last_date ebenfalls generisch (nur String/Imputationen)
  # Dafür brauchen wir: progression_sv_last_date_month/year existieren und progression_sv_last_date ist Date
  sv3 <- create_imputed_dates(
    data   = sv2,
    prefix = "progression_sv_last_date",
    day_values = c("01","15","28"),
    after = "progression_sv_last_date"
  
  ) %>%
    
    
    dplyr::mutate(record_id = as.numeric(record_id), .after = record_id) %>%
    dplyr::select(-cycle)
  
  sv3
}






















































# prepare_survival_status <- function(sv) {
#   
#   sv1 <- sv %>%
#     
#     mutate(
#       surv_dod_date = as.Date(surv_dod_date),      # assuming "YYYY-MM-DD" from REDCap
#       dod_ic_date   = as.Date(dod_ic_date)
#     ) %>%
#     
#     # combine date of death from patient characteristics and survival status
#     mutate(
#       surv_dod_date = coalesce(surv_dod_date, dod_ic_date),
#       progression_sv_last_date_month = coalesce(progression_sv_last_date_month, dod_ic_date_month),
#       progression_sv_last_date_year = coalesce(progression_sv_last_date_year, dod_ic_date_year),
#     ) %>%
#     
#     # prepare date of contact and death in one variable
#     mutate(
#       
#       status_date = case_when(
#         surv_stat == "alive" ~ case_when(!is.na(last_contact_date) ~ last_contact_date),
#         surv_stat == "deceased" ~ case_when(!is.na(surv_dod_date) ~ surv_dod_date),
#         surv_lost2fu_y_n == 1 ~ case_when(!is.na(last_contact_date) ~ last_contact_date),
#         TRUE ~ NA
#       ),
#       
#       status_date_month = case_when(
#         surv_stat == "alive" ~ case_when(is.na(last_contact_date) & !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_month),
#         surv_stat == "deceased" ~ case_when(is.na(surv_dod_date) & !is.na(surv_dod_date_month) & !is.na(surv_dod_date_year) ~ surv_dod_date_month),
#         surv_lost2fu_y_n == 1 ~ case_when(is.na(last_contact_date) & !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_month),
#         TRUE ~ NA
#       ),
#       
#       status_date_year = case_when(
#         surv_stat == "alive" ~ case_when(is.na(last_contact_date) & !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_year),
#         surv_stat == "deceased" ~ case_when(is.na(surv_dod_date) & !is.na(surv_dod_date_month) & !is.na(surv_dod_date_year) ~ surv_dod_date_year),
#         surv_lost2fu_y_n == 1 ~ case_when(is.na(last_contact_date) & !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_year),
#         TRUE ~ NA
#       ),
#       
#       
#       
#       status_date_ = case_when(
#         surv_stat == "alive" ~ ifelse(
#           !is.na(last_contact_date_month) & !is.na(last_contact_date_year) & is.na(last_contact_date),
#           paste("nk", last_contact_date_month, last_contact_date_year, sep = "/"),
#           format(as.Date(last_contact_date, "%Y-%m-%d"), "%d/%m/%Y")
#         ),
#         surv_stat == "deceased" ~ ifelse(
#           !is.na(surv_dod_date_month) & !is.na(surv_dod_date_year) & is.na(surv_dod_date),
#           paste("nk", surv_dod_date_month, surv_dod_date_year, sep = "/"),
#           format(as.Date(surv_dod_date, "%Y-%m-%d"), "%d/%m/%Y")
#         ),
#         surv_lost2fu_y_n == 1 ~ ifelse(
#           !is.na(last_contact_date_month) & !is.na(last_contact_date_year) & is.na(last_contact_date),
#           paste("nk", last_contact_date_month, last_contact_date_year, sep = "/"),
#           format(as.Date(last_contact_date, "%Y-%m-%d"), "%d/%m/%Y")
#         ),
#         TRUE ~ NA_character_
#       ),
#       surv_stat = ifelse((surv_stat == "nd" | surv_stat == "unknown") & surv_lost2fu_y_n == 1, "Lost2FU", surv_stat),
#       .after = surv_stat
#     )
#   
#   
#   
#   
#   sv_ <- sv1 %>%
#     mutate(status_date_01 = as.Date(stringr::str_replace(status_date_, "nk", "01"), "%d/%m/%Y"),
#            status_date_15 = as.Date(stringr::str_replace(status_date_, "nk", "15"), "%d/%m/%Y"),
#            status_date_28 = as.Date(stringr::str_replace(status_date_, "nk", "28"), "%d/%m/%Y")
#     ) %>%
#     
#     # filter last update
#     filter(!is.na(status_date_15)) %>%
#     filter(status_date_15 == max(status_date_15), .by = record_id) %>%
#     group_by(record_id) %>%
#     slice_tail()
#   
#   
#   
#   # combine dataset with patient with no status date
#   sv2 <- bind_rows(
#     sv_,
#     sv1 %>% filter(!record_id %in% sv_$record_id)
#   )
#   
#   # check for missing patients
#   if (length(unique(sv$record_id)) == length(unique(sv2$record_id))) {
#     print("All patient included")
#   } else {
#     print("Missing patients, please check")
#   }
#   
#   
#   
#   # prepare dataset
#   sv3 <- sv2 %>%
#     mutate(
#       # combine partial and full date of last progression
#       progression_sv_last_date = ifelse(
#         !is.na(progression_sv_last_date_month) & !is.na(progression_sv_last_date_year) & is.na(progression_sv_last_date),
#         paste("nk", progression_sv_last_date_month, progression_sv_last_date_year, sep = "/"),
#         format(as.Date(progression_sv_last_date, "%Y-%m-%d"), "%d/%m/%Y")
#       )
#     ) %>%
#     
#     mutate(
#       progression_sv_last_date_01 = as.Date(stringr::str_replace(progression_sv_last_date, "nk", "01"), "%d/%m/%Y"),
#       progression_sv_last_date_15 = as.Date(stringr::str_replace(progression_sv_last_date, "nk", "15"), "%d/%m/%Y"),
#       progression_sv_last_date_28 = as.Date(stringr::str_replace(progression_sv_last_date, "nk", "28"), "%d/%m/%Y")
#     ) %>%
#         
#     dplyr::mutate(
#       record_id = as.numeric(record_id), .after = record_id
#     ) %>%
# 
# 
#     select(-cycle)
# 
# 
#   return(sv3)
#   
# }






















#' Preparation of the survival status data.
#'
#' @param sv 
#' RedCap data set of the filtered version `redcap_repeat_instrument == "survival_status" & record_id %in% base$record_id`
#'
#' @returns
#' Same data set with transformed survival status colums `surv_stat`, `status_date`, `surv_lost2fu_y_n`,
#' @export
# prepare_survival_status <- function(sv) {
#   
#   sv1 <- sv %>%
#     
#     dplyr::mutate(
#       surv_dod_date = as.Date(surv_dod_date),      # assuming "YYYY-MM-DD" from REDCap
#       dod_ic_date   = as.Date(dod_ic_date),
#     
#       # combine date of death from patient characteristics and survival status
#       surv_dod_date = coalesce(surv_dod_date, dod_ic_date),
#       progression_sv_last_date_month = coalesce(progression_sv_last_date_month, dod_ic_date_month),
#       progression_sv_last_date_year = coalesce(progression_sv_last_date_year, dod_ic_date_year),
#       
#       
#     
#       # prepare date of contact and death in one variable
#       status_date = case_when(
#         surv_stat == "alive" ~ case_when(!is.na(last_contact_date) ~ last_contact_date),
#         surv_stat == "deceased" ~ case_when(!is.na(surv_dod_date) ~ surv_dod_date),
#         surv_lost2fu_y_n == 1 ~ case_when(!is.na(last_contact_date) ~ last_contact_date),
#         TRUE ~ NA
#       ),
#       
#       status_date_month = case_when(
#         surv_stat == "alive" ~ case_when(is.na(last_contact_date) & !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_month),
#         surv_stat == "deceased" ~ case_when(is.na(surv_dod_date) & !is.na(surv_dod_date_month) & !is.na(surv_dod_date_year) ~ surv_dod_date_month),
#         surv_lost2fu_y_n == 1 ~ case_when(is.na(last_contact_date) & !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_month),
#         TRUE ~ NA
#       ),
#       
#       status_date_year = case_when(
#         surv_stat == "alive" ~ case_when(is.na(last_contact_date) & !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_year),
#         surv_stat == "deceased" ~ case_when(is.na(surv_dod_date) & !is.na(surv_dod_date_month) & !is.na(surv_dod_date_year) ~ surv_dod_date_year),
#         surv_lost2fu_y_n == 1 ~ case_when(is.na(last_contact_date) & !is.na(last_contact_date_month) & !is.na(last_contact_date_year) ~ last_contact_date_year),
#         TRUE ~ NA
#       )
#     ) %>%
#       
#       
#     dplyr::mutate(
#       surv_stat = ifelse((surv_stat == "nd" | surv_stat == "unknown") & surv_lost2fu_y_n == 1, "Lost2FU", surv_stat)
#     ) %>%    
#     
#     dplyr::mutate(
#       record_id = as.factor(as.numeric(record_id)), .after = record_id
#     ) %>%
#     
#     dplyr::arrange(record_id)
#     
#   
#   return(sv1)
#   
# }