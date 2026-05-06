

# scripts/cleaning/02_clean_diagnosis.R





# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# --------------- Clean Diagnosis Related Variables --------------------------
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# Sections:
#    > Calculate IPI / NCCN Scores
#    > clean patient characteristics (subtype, AA stage, ECOG, diagnosis transformation)
#    > Combine "*_other" field & extranodal sites
#    
# Goal: to make the data readable afterwards and reduce columns 
# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
## --------------- IPI/NCCN Scores --------------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# | IPI                                                  |
# | 0 points                 | 1 point                   |
# | ------------------------ | ------------------------- |
# | Age <=60 y               | Age >60 y                 |
# | A.A.-Stage I or II       | A.A.-Stage III or IV      |
# | ECOG 0 or 1              | ECOG >=2                  |
# | LDH <=normal             | LDH >normal               |
# | Extranodal sites <=1     | Extranodal sites >1       |


# | revised IPI                                          |
# | 0 points                 | 1 point                   |
# | ------------------------ | ------------------------- |
# | Age <=60 y               | Age >60 y                 |
# | A.A.-Stage I or II       | A.A.-Stage III or IV      |
# | ECOG 0 or 1              | ECOG >=2                  |
# | LDH <=normal             | LDH >normal               |
# | Extranodal sites <=1     | Extranodal sites >1       |


# | CNS-IPI                                              |
# | 0 points                 | 1 point                   |
# | ------------------------ | ------------------------- |
# | Age <=60 y               | Age >60 y                 |
# | A.A.-Stage I or II       | A.A.-Stage III or IV      |
# | ECOG <=                  | ECOG >1                   |
# | LDH <=normal             | LDH >normal               |
# | Extranodal sites <=1     | Extranodal sites >1       |
# | Kidney/adrenal gland involvement yes = 1point        |


# | NCCN                                                       |
# | ---------------------------------------------------------- |
# | Age          >=40 = 0p; 41-60 = 1p; 61-75 = 2p; >75 = 3p   |
# | LDH ratio    <=1 = 0p;  2-3 = 1p;   >3 = 2p                |
# | ECOG         >=2 = 1p                                      |
# | A.A.-Stage   III or IV = 1p                                |
# | Extranodal sites   >=1 = 1p                                |



# References:

#    International Non-Hodgkin's Lymphoma Prognostic Factors Project. A predictive model for aggressive non-Hodgkin's lymphoma. 
#    N Engl J Med. 1993 Sep 30;329(14):987-94. doi: 10.1056/NEJM199309303291402. PMID: 8141877.

#    https://www.mdcalc.com/calc/3936/international-prognostic-index-diffuse-large-b-cell-lymphoma-ipi-r-ipi#evidence

#    Sehn LH, Berry B, Chhanabhai M, Fitzgerald C, Gill K, Hoskins P, Klasa R, Savage KJ, Shenkier T, Sutherland J, 
#        Gascoyne RD, Connors JM. The revised International Prognostic Index (R-IPI) is a better predictor of outcome 
#        than the standard IPI for patients with diffuse large B-cell lymphoma treated with R-CHOP. Blood. 
#        2007 Mar 1;109(5):1857-61. doi: 10.1182/blood-2006-08-038257. Epub 2006 Nov 14. PMID: 17105812.

#    International Non-Hodgkin's Lymphoma Prognostic Factors Project. 
#        A predictive model for aggressive non-Hodgkin's lymphoma. N Engl J Med. 
#        1993 Sep 30;329(14):987-94. doi: 10.1056/NEJM199309303291402. PMID: 8141877.

#    Schmitz N, Zeynalova S, Nickelsen M, Kansara R, Villa D, Sehn LH, Glass B, Scott DW, Gascoyne RD, Connors JM, 
#        Ziepert M, Pfreundschuh M, Loeffler M, Savage KJ. CNS International Prognostic Index: A Risk Model for 
#        CNS Relapse in Patients With Diffuse Large B-Cell Lymphoma Treated With R-CHOP. J Clin Oncol. 
#        2016 Sep 10;34(26):3150-6. doi: 10.1200/JCO.2015.65.6520. Epub 2016 Jul 5. PMID: 27382100.


#' Calculate FLIPI scores 
#'
#' @param base Input dataset containing baseline variables.
#'
#' @returns    Same data set with the calculated score; sorted. 
#' @export
calc_scores <- function(base1){
  base2 <- base1 %>% 
    
    rowwise() %>%
    
    # IPI variables
    mutate(
      
      # IPI age
      diag_ipi_age = case_when(is.na(diagnosis_age) ~ NA_real_, 
                               diagnosis_age > 60 ~ 1, 
                               diagnosis_age <= 60 ~ 0),
      
      # IPI stage
      diag_ipi_stage = case_when(diag_stage %in% c("3", "4") ~ 1, 
                                 diag_stage %in% c("1", "2") ~ 0, 
                                 TRUE ~ NA_real_),
      
      # IPI ECOG
      diag_ipi_ecog = case_when(diag_ecog %in% c("2", "3", "4") ~ 1, 
                                diag_ecog %in% c("0", "1") ~ 0, 
                                TRUE ~ NA_real_),
      
      # IPI LDH
      diag_ipi_ldh = case_when(
        (!is.na(diag_ldh_diag) > !is.na(diag_ldh_baseline)) | diag_ldh_elevated_y_n == "yes" | diag_ldh_ratio > 1 ~ 1,
        (!is.na(diag_ldh_diag) <= !is.na(diag_ldh_baseline)) | diag_ldh_elevated_y_n %in% c("unk", "no") ~ 0,
        TRUE ~ NA_real_
      ),
      
      # extranodal
      # rewrite others for counting
      diag_extranodal_site_othern = ifelse(!is.na(diag_extranodal_site_other), 1, 0),
      diag_extranodal_site_other_2n = ifelse(!is.na(diag_extranodal_site_other_2), 1, 0),
      diag_extranodal_site_other_3n = ifelse(!is.na(diag_extranodal_site_other_3), 1, 0),
      
      # count all sites
      diag_ipi_site_n = sum(
        c_across(c(
          diag_extranodal_site___adrenal:diag_extranodal_site___uterus,
          diag_extranodal_site_othern, diag_extranodal_site_other_2n, diag_extranodal_site_other_3n
        ))
      ),
      diag_ipi_site = ifelse(diag_ipi_site_n > 1, 1, 0),
      diag_ipi_site_cns = ifelse(diag_extranodal_site___adrenal == 1 | diag_extranodal_site___kidney == 1, 1, 0)
    ) %>%
    
    # NCCN-IPI variables
    mutate(
      # age
      diag_nccn_age = case_when(diagnosis_age <= 40 ~ 0,
                                diagnosis_age > 40 & diagnosis_age <= 60 ~ 1,
                                diagnosis_age > 60 & diagnosis_age <= 75 ~ 2,
                                diagnosis_age > 75 ~ 3,
                                TRUE ~ NA_real_),
      
      diag_nccn_ldh = case_when(diag_ldh_ratio > 1 & diag_ldh_ratio <= 3 ~ 1,
                                diag_ldh_ratio > 3 ~ 2, 
                                !is.na(diag_ldh_ratio) ~ 0, 
                                TRUE ~ NA_real_),
      
      diag_nccn_stage = diag_ipi_stage,
      diag_nccn_ecog = diag_ipi_ecog,
      diag_nccn_site = diag_ipi_site
    ) %>%
    
    # scores
    mutate(
      diag_ipi_n = sum(diag_ipi_age, diag_ipi_stage, diag_ipi_ecog, diag_ipi_ldh, diag_ipi_site),
      diag_ipi_n_aa = sum(diag_ipi_stage, diag_ipi_ecog, diag_ipi_ldh),
      diag_ipi_n_cns = sum(diag_ipi_age, diag_ipi_stage, diag_ipi_ecog, diag_ipi_ldh, diag_ipi_site, diag_ipi_site_cns),
      diag_nccn_n = sum(diag_nccn_age, diag_ipi_stage, diag_ipi_ecog, diag_nccn_ldh, diag_ipi_site),
      
      diag_ipi_score_standard = case_when(
        diag_ipi_n == 0 ~ "Low (0-1) score: 0",
        diag_ipi_n == 1 ~ "Low (0-1) score: 1",
        diag_ipi_n == 2 ~ "Low-intermediate (2) score: 2",
        diag_ipi_n == 3 ~ "High-intermediate (3) score: 3",
        diag_ipi_n == 4 ~ "High (4-5) score: 4",
        diag_ipi_n == 5 ~ "High (4-5) score: 5",
        TRUE ~ NA_character_),
      
      diag_ipi_score_revised = case_when(
        diag_ipi_n == 0 ~ "Very good (0) score: 0",
        diag_ipi_n == 1 ~ "Good (1-2) score: 1",
        diag_ipi_n == 2 ~ "Good (1-2) score: 2",
        diag_ipi_n == 3 ~ "Poor (3-5) score: 3",
        diag_ipi_n == 4 ~ "Poor (3-5) score: 4",
        diag_ipi_n == 5 ~ "Poor (3-5) score: 5",
        TRUE ~ NA_character_),
      
      diag_ipi_score_aa = case_when(
        diag_ipi_n_aa == 0 ~ "Low (0) score: 0",
        diag_ipi_n_aa == 1 ~ "Intermediate (1) score: 1",
        diag_ipi_n_aa == 2 ~ "High (2-3) score: 2",
        diag_ipi_n_aa == 3 ~ "High (2-3) score: 3",
        TRUE ~ NA_character_),
      
      diag_ipi_score_cns = case_when(
        diag_ipi_n_cns == 0 ~ "Low (0-1) score: 0",
        diag_ipi_n_cns == 1 ~ "Low (0-1) score: 1",
        diag_ipi_n_cns == 2 ~ "Low-intermediate (2-3) score: 2",
        diag_ipi_n_cns == 3 ~ "Low-intermediate (2-3) score: 3",
        diag_ipi_n_cns == 4 ~ "High (4-6) score: 4",
        diag_ipi_n_cns == 5 ~ "High (4-6) score: 5",
        diag_ipi_n_cns == 6 ~ "High (4-6) score: 6",
        TRUE ~ NA_character_),
      
      diag_nccn_score = case_when(
        diag_nccn_n == 0 ~ "Low (0-1) score: 0",
        diag_nccn_n == 1 ~ "Low (0-1) score: 1",
        diag_nccn_n == 2 ~ "Low-intermediate (2-3) score: 2",
        diag_nccn_n == 3 ~ "Low-intermediate (2-3) score: 3",
        diag_nccn_n == 4 ~ "High-intermediate (4-5) score: 4",
        diag_nccn_n == 5 ~ "High-intermediate (4-5) score: 5",
        diag_nccn_n == 6 ~ "High (6-8) score: 6",
        diag_nccn_n == 7 ~ "High (6-8) score: 7",
        diag_nccn_n == 8 ~ "High (6-8) score: 8",
        TRUE ~ NA_character_)
    )
  
  return(base2)
}













# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Patient Characteristics ------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Transform variables to labels and cleaning some data
#'
#' @param base Input dataset containing baseline variables.
#' @return Transformed dataset with all relevant labels and cleaned data. 
#' Subtype, diag_stage, diag_ecog, diagnosis_transformation, etc.
#' @export
clean_patient_characteristics <- function(base) {
  
  
  
  base1 <- base %>%
    
    mutate(
      yob = lubridate::year(as.Date(datum_geb, "%Y-%m-%d")),
      
      # calc diagnosis age and ensure numeric data type
      diagnosis_age = ifelse(
        is.na(diagnosis_date), as.numeric(diagnosis_date_year), lubridate::year(as.Date(diagnosis_date, "%Y-%m-%d"))) - as.numeric(yob),
      
      # rewrite subtype - cell of origin
      diagnosis_subtype_coo = case_when(
        diagnosis_subtype_coo %in% c("na", "nd") ~ NA_character_,
        diagnosis_subtype_coo == "gcb" ~ "GCB",
        diagnosis_subtype_coo %in% c("abc", "nongcb") ~ "nonGCB",
        TRUE ~ NA_character_
      ),
      
      
      # clean Ann-Arbour-Stage variable
      # checked before which unique elements are inclueded
      diag_stage = case_when(diag_stage %in% c("1", "2", "3", "4") ~ diag_stage,
                             diag_stage  %in% c("999", "NA", "nd", "0") ~ NA_character_,
                             TRUE ~ NA_character_),
      
      # cleaning ECOG score 
      # checked before which unique elements are inclueded
      diag_ecog = case_when(diag_ecog %in% c("0", "1", "2", "3", "4") ~ diag_ecog,
                            diag_ecog == "na" ~ NA_character_,
                            diag_ecog == "nd" ~ NA_character_, 
                            TRUE ~ NA_character_),
      
      # cleaning initial staging with PET
      # checked before which unique elements are inclueded
      diag_stage_pet = case_when(diag_stage_pet == "n" ~ "no",
                                 diag_stage_pet == "y" ~ "yes",
                                 diag_stage_pet == "unk" ~ "unknown",
                                 diag_stage_pet == "na" ~ NA_character_,
                                 diag_stage_pet == "nd" ~ NA_character_)
    ) %>%
    
    # rewrite variables value to labels
    globaltools::values_to_labels(diagnosis) %>%
    globaltools::values_to_labels(diag_ecog) %>%
    globaltools::values_to_labels(diag_stage) %>%
    globaltools::values_to_labels(diagnosis_subtype_test) %>%
    globaltools::values_to_labels(diagnosis_subtype) %>%
    globaltools::values_to_labels(diagnosis_transformation_before) %>%
    globaltools::values_to_labels(diag_extranodal) %>%
    globaltools::values_to_labels(registry_ic) %>%
    
    
    
    # Create diagnosis cols for imputation method in (06_date_imputations.R)
    # add_ff_date_parts(diagnosis_date) %>%
    
    # mutate(
    #   # Only fill diagnosis_* if day is not imputed
    #   diagnosis_day_ff   = if_else(!is.na(diagnosis_date),
    #                                day(diagnosis_date), NA_integer_),
    #   diagnosis_month_ff = if_else(!is.na(diagnosis_date),
    #                                month(diagnosis_date), NA_integer_),
    #   diagnosis_year_ff  = if_else(!is.na(diagnosis_date),
    #                                year(diagnosis_date), NA_integer_), .after = diagnosis_date_comb
    # )
  
  return(base1) 
}
















# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## --------------- Combine "*_other" fields & extranodal sites ----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Combine the "_other" fields in one cell
#'
#' @param base1     Input dataset containing several variables.
#' 
#' @return Transformed dataset with: 
#'                combined field + "other", 
#'                combined extranodal sites-field + "other" = diag_extranodal_site
#'                
#' diagnosis, diagnosis_subtype, diagnosis_transformation_before, diag_extranodal_site___
#' 
#' @export
combine_other_fields <- function(base2){
  
  site_cols <- names(base2)[
    stringr::str_detect(names(base2), "^diag_extranodal_site___") &
      !stringr::str_detect(names(base2), "other")
  ]
  
  other_cols <- c(
    "diag_extranodal_site_other",
    "diag_extranodal_site_other_2",
    "diag_extranodal_site_other_3"
  )
  
  unite_cols <- c(site_cols, other_cols)
  
  base_ <- base2 %>%
    mutate(
      diagnosis = ifelse(
        diagnosis == "Other",
        paste0(diagnosis, " - ", diagnosis_other),
        diagnosis
      ),
      
      diagnosis_subtype = ifelse(
        diagnosis_subtype == "Other",
        paste0(diagnosis_subtype, " - ", diagnosis_subtype_other),
        diagnosis_subtype
      ),
      
      diagnosis_transformation_before = ifelse(
        diagnosis_transformation_before == "Other",
        paste0(diagnosis_transformation_before, " - ", diagnosis_subtype_other),
        diagnosis_transformation_before
      )
    ) %>%
    
    mutate(
      across(
        starts_with("diag_extranodal_site___"),
        ~ ifelse(
          .x == 1,
          stringr::str_to_title(stringr::str_split_i(cur_column(), "___", 2)),
          NA_character_
        )
      )
    ) %>%
    
    mutate(
      across(
        starts_with("diag_extranodal_site___"),
        ~ dplyr::case_match(
          as.character(.x),
          "Cns"       ~ "Central nervous system",
          "Marrow"    ~ "Bone marrow",
          "Git"       ~ "GIT",
          "Paranasal" ~ "Paranasal-sinus",
          "Colon"     ~ "Colon/large intestines",
          "Intestine" ~ "Small intestine",
          "Soft"      ~ "Soft tissue",
          .default = .x
        )
      )
    ) %>%
    
    mutate(
      diag_extranodal_site_other   = ifelse(!is.na(diag_extranodal_site_other),   paste0("Other1 - ", diag_extranodal_site_other),   diag_extranodal_site_other),
      diag_extranodal_site_other_2 = ifelse(!is.na(diag_extranodal_site_other_2), paste0("Other2 - ", diag_extranodal_site_other_2), diag_extranodal_site_other_2),
      diag_extranodal_site_other_3 = ifelse(!is.na(diag_extranodal_site_other_3), paste0("Other3 - ", diag_extranodal_site_other_3), diag_extranodal_site_other_3)
    ) %>%
    
    tidyr::unite(
      "diag_site",
      dplyr::all_of(unite_cols),
      sep = ", ",
      na.rm = TRUE,
      remove = FALSE
    ) %>%
    
    mutate(
      diag_site = ifelse(diag_site == "", NA, diag_site),
      record_id = as.numeric(record_id),
      .after = record_id
    ) %>%
    
    arrange(record_id)
  
  base_
}



