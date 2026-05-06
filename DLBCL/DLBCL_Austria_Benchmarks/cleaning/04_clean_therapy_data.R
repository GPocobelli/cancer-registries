

# scripts/cleaning/03_clean_therapy_data.R



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# --------------- Clean Therapy Data -----------------------------
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# Sections:
#    > 
#    
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Therapy Variables -----------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Preparation of the therapy data. 
#'
#' @param thx
#' RedCap data set of the filtered version `redcap_repeat_instrument == "medical_treatment" & record_id %in% base$record_id`
#'
#' @returns
#' Same data set with transformed therapy data columns `drug_med_all`, `progression_before_type`, `treat_line_ecog`

cleaning_therapy_variables <- function(thx){
  
  # gather all variables with medical treatment for unite
  med_vars_base <- thx %>%
    dplyr::select(
      starts_with("drug_med"), 
      -contains("check"), 
      -contains("cycle"),
      -contains("end"), drug_car_type, treat_trans_type
    ) %>%
    names()
  # gather all date variables for reformatting
  date_vars <- thx %>%
    dplyr::select(contains("date"),
           -contains("month"),
           -contains("year")) %>%
    names()
  
  
  # Define selected spelling mistakes
  med_corrections <- c("Cladribin"         = "Cladribine",
                       "Fludarabin"        = "Fludurabine",
                       "Fludarabine"       = "Fludurabine",
                       "Ifosfamid"         = "Ifosfamide",
                       "Ibrutinib/Placebo" = "Ibrutinib / Placebo",
                       "Interferon alpha"  = "Interferone-alpha",
                       "Interferon-alpha"  = "Interferone-alpha",
                       "Interferon"        = "Interferone",
                       "Interleukin 2"     = "Interleukin-2",
                       "Mitoxantron"       = "Mitoxantrone",
                       "Oxiplatin"         = "Oxaliplatin",
                       "Pixantron"         = "Pixantrone",
                       "Procarbazin"       = "Procarbazine",
                       "Thiothepa"         = "Thiotepa",
                       "Vincristin"        = "Vincristine",
                       "R-BENDAMUSTIN"     = "R-BENDAMUSTINE",
                       "OBI-BENDAMUSTIN"   = "OBI-BENDAMUSTINE",
                       "R-LENALIDOMID"     = "R-LENALIDOMIDE",
                       "lenalidomid"       = "Lenalidomide",
                       "Lenalidomid"       = "Lenalidomide",
                       "Methotrexat"       = "Methotrexate",
                       "Pixatrone"         = "Pixantrone"
  )
  
  med_corrections_regex <- med_corrections %>%
    purrr::set_names(nm = str_c("\\b", names(.), "\\b"))
  

  
  thx2 <- thx %>%
    
    
    
    dplyr::mutate(dplyr::across(
      dplyr::all_of(med_vars_base), ~ stringr::str_replace_all(.x, med_corrections_regex))
    ) %>%
    
    
    # Clean logical to character data
    dplyr::mutate(
      drug_med_comb = toupper(drug_med_comb),
      drug_intrathecal = ifelse(drug_intrathecal_check___1 == 1, "Intrathecal therapy", NA),
      drug_radiotherapy = ifelse(drug_med_check_radio___1 == 1, "Radiotherapy", NA),
      drug_bsc = ifelse(drug_best_supportive_check___1 == 1, "Best supportive care", NA)
    ) %>%
    
    
    # Clean logical to character data
    dplyr::mutate(
      progression_before_type = dplyr::case_when(
        progression_before_type___cns == 1 & progression_before_type___periphery == 1 ~ "CNS, Periphery",
        progression_before_type___cns == 1 ~ "CNS",
        progression_before_type___periphery == 1 ~ "Periphery",
        # progression_before_type___nd == 1 ~ "Not done",
        TRUE ~ NA_character_
      ),
      
      progression_type = dplyr::case_when(
        progression_type___cns == 1 & progression_type___periphery == 1 ~ "CNS, Periphery",
        progression_type___cns == 1 ~ "CNS",
        progression_type___periphery == 1 ~ "Periphery",
        # progression_type___nd == 1 ~ "Not done",
        TRUE ~ NA_character_
      )) %>%
    
    
    # Clean logical to character data
    dplyr::mutate(
      .ecog_num = dplyr::if_else(
        stringr::str_detect(as.character(treat_line_ecog), "^[0-4]$"),
        as.numeric(treat_line_ecog),
        NA_real_), 
      
      treat_line_ecog = dplyr::case_when(!is.na(.ecog_num) & .ecog_num %in% 0:4 ~ paste0("Grade ", .ecog_num),
                                         TRUE ~ NA_character_
      )
    ) %>%
    
    dplyr::select(-.ecog_num)
  
  
  
  # combine variables that will be created to include all therapies
  med_vars <- c(med_vars_base, "drug_intrathecal", "drug_radiotherapy", "drug_bsc")
  
  
  return(list(
    data = thx2,
    med_vars = med_vars,
    date_vars = date_vars
  ))    
}








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Combine treatment columns --------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Combine treatment columns. 
#'
#' @param thx2
#' thx2 data from before
#'
#' @returns
#' Same data set with combined ttreatment columns `drug_med_all`, `drug_med_chemo`, `drug_med_immune`,
#' `drug_med_target`, `drug_med_other`.

combine_treatment_columns <- function(res){
  thx2 <- res$data
  med_vars <- res$med_vars
  date_vars <- res$date_vars
  
  
  med_vars_to_label <- med_vars[
    grepl("^drug_med", med_vars) &
      med_vars != "drug_med_comb" &
      !grepl("_other$", med_vars) &
      !grepl("^drug_medother[123]$", med_vars)
  ]
  med_vars_to_label <- intersect(med_vars_to_label, names(thx2))
  
  
  # values_to_labels for all med_vars
  thx2 <- purrr::reduce(
    med_vars_to_label,
    ~ values_to_labels(.x, !!rlang::sym(.y)),
    .init = thx2
  )
  
  
  
  thx2 <- thx2 %>%
    
    values_to_labels(drug_medother1) %>%
    values_to_labels(drug_medother2) %>%
    values_to_labels(drug_medother3) %>%

    mutate(across(starts_with("drug_medother"),
                  ~ sub("\\s*\\(.*$", "", .x))
    ) %>%
    
    
    # create a column with all therapies combined 
    tidyr::unite("drug_med_all", all_of(med_vars), sep = ", ", na.rm = T, remove = F) %>%
    
    dplyr::mutate(
      drug_med_all = capitalize_words(stringr::str_replace_all(drug_med_all, c("other, " = "", "Other, " = "", ", other" = "", ", Other " = "")))
    ) %>%
    
    tidyr::unite("drug_med_chemo", c(drug_medchemo1, drug_medchemo2, drug_medchemo3, drug_medchemo_other), sep = ", ", na.rm = T, remove = F) %>%
    tidyr::unite("drug_med_immune", c(drug_medimmune1, drug_medimmune2, drug_medimmune3, drug_medimmune_other), sep = ", ", na.rm = T, remove = F) %>%
    tidyr::unite("drug_med_target", c(drug_medtarget1, drug_medtarget2, drug_medtarget3, drug_medtarget_other), sep = ", ", na.rm = T, remove = F) %>%
    tidyr::unite("drug_med_other", c(drug_medother1, drug_medother2, drug_medother3, drug_medother_other), sep = ", ", na.rm = T, remove = F) %>%
    
    # reformat_dates(date_vars) %>%

    # Correct spelling mistakes
    dplyr::mutate(across(c(drug_med_all, drug_med_chemo), ~ stringr::str_replace(.x, "myocet|Myocet", "Non-pegylated liposomalem Doxorubicin"))
    )
  
  return(thx2)
}








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Transform values to labels -------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Transform treatment values to labels for more readable purpose.
#'
#' @param data
#' thx2 data from before
transform_treatment_to_labels <- function(thx2){
  dd <- dd
  
  thx2 <- thx2 %>%
    
    dplyr::mutate(
      treat_no_reason = case_when(treat_no_reason == "no_sympt" ~ paste0("No symptoms"),
                                              treat_no_reason == "pat_dec" ~ paste0("Patient decision"),
                                              treat_no_reason == "inv_dec" ~ paste0("Doctor's decision"),
                                              treat_no_reason == "other" ~ paste0("Other"),
                                              TRUE ~ NA),
      
      treat_no_reason_temp = if_else(treat_no_reason == "Other", 
                                     treat_no_reason_other, 
                                     treat_no_reason),
      
      treat_no_reason = if_else(treat_no_reason == "Other", 
                                treat_no_reason_other, 
                                treat_no_reason),
      
      treat_no_reason = if_else(is.na(treat_no_reason), treat_no_reason_temp, treat_no_reason)
      
      ) %>%
    
    
    values_to_labels(drug1_start_reason) %>%
    values_to_labels(drug1_end_reason) %>%
    values_to_labels(therapy_status) %>%
    values_to_labels(treat_rebiopsy) %>%
    
    
    # Cleaning
    dplyr::mutate(treat_best_response = case_when(treat_best_response == "99" ~ "Unknown", 
                                                  treat_best_response == "nd" ~ "Not done", 
                                                  treat_best_response == "3" ~ "CR", 
                                                  treat_best_response == "2" ~ "PR", 
                                                  treat_best_response == "1" ~ "SD", 
                                                  treat_best_response == "4" ~ "PD",
                                                  treat_best_response == "5" ~ "NE",
                                                  TRUE ~ treat_best_response),
                  
                  across(c(drug_med_all, drug_med_comb, drug_med_chemo, 
                    drug_med_immune, drug_med_target, drug_med_other), ~ ifelse(.x == "", NA, .x)),
                  
                  across(c(drug_med_all, drug_med_comb, drug_med_chemo, 
                    drug_med_immune, drug_med_target, drug_med_other), ~ str_replace(.x, "other, ", "other - ")),
                  
                  treat_line = as.integer(treat_line)
    ) %>%
    
    
    
  ### -------------- Select and order specific columns ---------------------
  
  
  
  
  dplyr::select(
    
    record_id, cycle, treat_study, treat_study_text, treat_y_n, treat_line,
    
    # Therapy & progression data
    therapy_relapse_y_n, therapy_relapse_date, therapy_relapse_date_month, therapy_relapse_date_year,
    drug1_start_reason, 
    progression_before_date, progression_before_date_month, progression_before_date_year,
    progression_before_type, therapy_status, 
    therapy_start_date, therapy_start_date_month, therapy_start_date_year,
    therapy_end_date, therapy_end_date_month, therapy_end_date_year,
    drug1_end_reason, 
    progression_date, progression_date_month, progression_date_year, progression_type, 
    
    # Best response
    treat_best_response, treat_best_response_date, treat_best_response_date_month, treat_best_response_date_year,
    treat_best_response_petct, 
    treat_best_response_petct_date, treat_best_response_petct_date_month, treat_best_response_petct_date_year,
    treat_rebiopsy,
    
    # treatment data
    drug_med_all, drug_med_comb, drug_med_comb_cycle,
    drug_med_check_chemo___1, drug_med_chemo, drug_medchemo_cycles, drug_medchemo_end_y_n, drug_medchemo_end_date,
    drug_med_immune, drug_medimmune_cycles, drug_medimmune_end_y_n, drug_medimmune_end_date,
    drug_med_target, drug_medtarget_cycles, drug_medtarget_end_y_n, drug_medtarget_end_date,
    drug_med_other, drug_medother_cycles, drug_medother_end_y_n, drug_medother_end_date,
    drug_car_type, drug_car_text, drug_car_date, drug_car_date_month, drug_car_date_year, 
    drug_car_bridge, drug_car_bridge_date, drug_car_bridge_date_month, drug_car_bridge_date_year, 
    drug_car_bridge_text, drug_car_bridge_chemo, drug_car_bridge_chemo_other, comb_med_bridging,
    drug_hold_therapy, comb_med_holding, drug_car_holding_date, drug_car_holding_date_month, drug_car_holding_date_year,
    drug_lympho_therapy, comb_med_lympho,
    drug_lympho_therapy, comb_med_lympho,
    drug_intrathecal_check___1,
    drug_med_check_radio___1,
    drug_med_check_trans___1, treat_trans_type, treat_trans_date, treat_trans_date_month, treat_trans_date_year,
    drug_best_supportive_check___1
    
  ) %>%
    dplyr::mutate(
      record_id = as.numeric(record_id), .after = record_id
           
    ) %>%
    
    dplyr::arrange(record_id, cycle, treat_line)     # !!!!!!!
  
  
  
  return(thx2)
  
}













#' Transform variables to labels of progression specific columns. 
#'
#' @param final_    Data frame with imputed progression dates
#'
#' @returns
#' prepare dataset to go from therapy level to therapy line level
#' transform variables to span therapy lines
#' @export

transform_progression_to_labels <- function(final_) {
  
  final2 <- final_ %>%
    filter(!is.na(record_id)) %>%
    ungroup() %>%
    mutate(pat_id = dense_rank(record_id)) %>%
    select(
      # characteristics
      record_id, first_name, last_name, datum_geb, yob, gender, registry_ic, registry_ic_date, diagnosis_age,
      
      # 1. diagnosis variables
      diagnosis, 
      diagnosis_date, diagnosis_date_imputed_day, diagnosis_date_month, diagnosis_date_year, 
      diagnosis_date_01, diagnosis_date_15, diagnosis_date_28, 
      diagnosis_age,
      
      diagnosis_subtype_coo, diagnosis_subtype, diag_stage, diag_ecog,
      
      diagnosis_transformation_date, diagnosis_transformation_date_imputed_day, 
      diagnosis_transformation_date_01, diagnosis_transformation_date_15, diagnosis_transformation_date_28, 
      diagnosis_transformation_before,
      
      
      
      # 2. IPI / NCCN
      #diag_site, 
      diag_extranodal_site_n_tot, diag_ipi_n, diag_ipi_score_standard, diag_ipi_score_revised, diag_ipi_score_aa, 
      diag_ipi_score_cns, 
      
      diag_nccn_n, diag_nccn_score, 
      
      diag_secondmalign_y_n,

      
      # Laboratory
      diag_ldh_baseline, diag_ldh_diag, diag_ldh_ratio, diag_ldh_elevated_y_n,
      albumin, b2mg, bilirubin, crp, fibrinogen, ggt, harnsaeure, harnstoff, hemoglobine, 
      kreatinin, ldh, leukocytes, lymphocytes, monocytes, neutrophils, platelet,
      
      
      # Treat line characteristics
      treat_y_n, cycle, treat_line,
      
      ## Treat Line & Therapies
      treat_study, treat_study_text,
      therapy_relapse_y_n, therapy_relapse_date, therapy_relapse_date_imputed_day, therapy_relapse_date_01, therapy_relapse_date_15, therapy_relapse_date_28, 
      drug1_start_reason, 
      
      # Porgession before
      progression_before_date, progression_before_date_imputed_day, 
      progression_before_date_01, progression_before_date_15, progression_before_date_28, progression_before_type,
      
      # Therapystart & end 
      therapy_status, 
      therapy_start_date, therapy_start_date_month, therapy_start_date_year, therapy_start_date_imputed_day, therapy_start_date_01, therapy_start_date_15, therapy_start_date_28, 
      
      therapy_end_date, therapy_end_date_month, therapy_end_date_year, therapy_end_date_imputed_day, therapy_end_date_01, therapy_end_date_15, therapy_end_date_28, 
      drug1_end_reason, 
      
      # Porgression date
      progression_date, progression_date_imputed_day, progression_date_01, progression_date_15, progression_date_28, progression_type, 
      treat_best_response, treat_best_response_date, treat_best_response_date_imputed_day, treat_best_response_date_01, treat_best_response_date_15, treat_best_response_date_28, 
      treat_best_response_petct, treat_best_response_petct_date, treat_best_response_petct_date_01, treat_best_response_petct_date_15, treat_best_response_petct_date_28,
      treat_rebiopsy,
      drug_med_all, drug_med_comb, drug_med_comb_cycle,
      drug_med_check_chemo___1, drug_med_chemo, drug_medchemo_cycles, drug_medchemo_end_y_n, drug_medchemo_end_date,
      drug_med_immune, drug_medimmune_cycles, drug_medimmune_end_y_n, drug_medimmune_end_date,
      drug_med_target, drug_medtarget_cycles, drug_medtarget_end_y_n, drug_medtarget_end_date,
      drug_med_other, drug_medother_cycles, drug_medother_end_y_n, drug_medother_end_date,
      drug_car_type, drug_car_text, drug_car_date, drug_car_date_month, drug_car_date_year, 
      drug_car_bridge, drug_car_bridge_date, drug_car_bridge_date_month, drug_car_bridge_date_year, 
      drug_car_bridge_text, drug_car_bridge_chemo, drug_car_bridge_chemo_other, comb_med_bridging,
      drug_hold_therapy, comb_med_holding, drug_car_holding_date, drug_car_holding_date_month, drug_car_holding_date_year,
      drug_lympho_therapy, comb_med_lympho,
      drug_lympho_therapy, comb_med_lympho,
      drug_intrathecal_check___1,
      drug_med_check_radio___1,
      drug_med_check_trans___1, treat_trans_type, treat_trans_date, treat_trans_date_01, treat_trans_date_15, treat_trans_date_28,
      drug_best_supportive_check___1,
      progression_last_y_n, progression_sv_last_date, progression_sv_last_date_01, progression_sv_last_date_15, progression_sv_last_date_28, 
      surv_stat, status_date, status_date_imputed_day, status_date_01, status_date_15, status_date_28
      
    ) %>%
    
    group_by(record_id, treat_line) %>%
    
    # Cleaning 
    
    # received therapy? 
    mutate(
      treat_y_n = case_when(
        treat_y_n == 1 ~ "yes",
        treat_y_n == 0 ~ "no")
      
    ) %>%
    
    # change to readable character values
    mutate(
      treat_study_line = case_when(
        any(treat_study == 1) ~ "yes",
        any(treat_study == 0) ~ "no",
        TRUE ~ NA_character_
      ),
      
      therapy_relapse_y_n_line = case_when(
        any(therapy_relapse_y_n == "yes") ~ "yes",
        any(therapy_relapse_y_n == "no") ~ "no",
        any(therapy_relapse_y_n == "Not done") ~ "Not done", 
        TRUE ~ NA_character_
      ),
      
      progression_before_y_n_line = ifelse(
        any(!is.na(progression_before_date)), "yes", "no"
      ),
      
      progression_before_type_line = case_when(
        any(progression_before_type == "CNS, Periphery") ~ "CNS, Periphery",
        any(progression_before_type == "CNS") ~ "CNS",
        any(progression_before_type == "Periphery") ~ "Periphery", 
        TRUE ~ NA_character_
      ),
      
      progression_type_line = case_when(
        any(progression_type == "CNS, Periphery") ~ "CNS, Periphery",
        any(progression_type == "CNS") ~ "CNS",
        any(progression_type == "Periphery") ~ "Periphery", 
        TRUE ~ NA_character_
      ),
      
      progression_y_n_line = ifelse(
        !is.na(progression_type), "yes", "no"
      ),
      
      treat_best_response_all = paste_without_na(treat_best_response),
      treat_best_response_line = case_when(
        grepl("CR", treat_best_response_all) ~ "CR", 
        grepl("PR", treat_best_response_all) ~ "PR",
        grepl("SD", treat_best_response_all) ~ "SD", 
        grepl("PD", treat_best_response_all) ~ "PD",
        grepl("NE", treat_best_response_all) ~ "NE", 
        grepl("Unknown|Not done", treat_best_response_all) ~ "Unknown", 
        TRUE ~ treat_best_response_all
      ),
      
      treat_best_response_petct = case_when(
        any(treat_best_response_petct == "ja") ~ "yes",
        any(treat_best_response_petct == "nein") ~ "no",
        any(treat_best_response_petct == "nd") ~ NA_character_,
        any(treat_best_response_petct == "na") ~ NA_character_, 
        TRUE ~ NA_character_
      ),
      
      treat_rebiopsy_line = case_when(
        any(treat_rebiopsy == "yes") ~ "yes",
        any(treat_rebiopsy == "no") ~ "no",
        any(treat_rebiopsy == "Not done") ~ "Not done", 
        TRUE ~ NA_character_
      ),
      
      drug_med_all_line = sort_unique_values(paste_without_na(drug_med_all)),
      .after = treat_rebiopsy
    ) %>%
    
    mutate(
      across(c(drug_med_comb:drug_best_supportive_check___1), ~ sort_unique_values(paste_without_na(.x)), .names = "{.col}_line")
    ) #%>%
    # Filter maintenance therapy to get the last therapy line
    #filter(drug1_start_reason %in% c("First diagnosis", "Progression", "No Reponse / stable disease", "Not done",
                                     #"Patient's decision", "Toxicity", NA_real_) ###### Hier überprüfen
  
  return(final2)
  
}













#' Creates a column which shows, if the current therapy line is 
#' the last one
#'
#' @param final2      Data frame on therapy lines level
#'
#' @returns
#' Same data frame but with new added column `max_treat_line`
#' 
#' @export

create_max_treat_line <- function(final2) {
  
  safe_max <- function(x) {
    x2 <- x[!is.na(x)]
    if (length(x2) == 0) NA_real_ 
    else max(x2)
  }
  
  # create max_treat_line to calc PFS more easily later
  final2 %>%
    dplyr::group_by(record_id) %>%
    dplyr::mutate(
      .max_tl = safe_max(treat_line),
      # 1 if treat_line is the last, else 0
      max_treat_line = as.integer(!is.na(treat_line) & treat_line == .max_tl)
    ) %>%
    dplyr::select(-.max_tl) %>%
    dplyr::relocate(max_treat_line, .after = treat_line) %>%
    dplyr::ungroup()
}
