# -------------- I. Set requirements ----------------------------------------





## ------------- 1. Packages & sources -------------
source("scripts/load_lib.R")

source(".Renviron")
library(globaltools)
library(jsonlite)
library(httr)






## ------------- 2. Cleaning File -----------------

# load the functions from these files
source("scripts/cleaning/1_preparation.R")
source("scripts/cleaning/2_transformation.R")







## ------------- 3. Script path -------------------

# WD for todays data exports
results_dir <- create_results_wd("Results")






## ------------- 4. Conflict handling --------------

# conflict_prefer_all("dplyr", "stats")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")







# -------------- II. Data import ---------------------------------------------
cli_h1("2. Data import")




## ------------- SLL-Certificate for Safety -------------
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




## ------------- Transform Numeric Values -------------

blood_marker <- c(
  "diag_hemo_diag", "diag_ldh_diag", "albumin", "b2mg", "bilirubin", "crp",
  "fibrinogen", "ggt", "hemoglobine", "neutrophils", "platelet", "lymphocytes",
  "kreatinin", "leukocytes", "harnsaeure", "harnstoff", "monocytes"
)

col_spec <- do.call(readr::cols, c(
  setNames(rep(list(readr::col_character()), length(blood_marker)), blood_marker),
  list(.default = readr::col_guess())
))


# Raw data from RedCap
d <- redcap_read_oneshot(api_url, api_token, guess_max = 10000, col_types = col_spec)$data


# Data Dictionary from RedCap (local)
dd <- readr::read_delim("doc/LymphomRegister_DataDictionary_2025-07-29.csv", delim = ";", guess_max = 4000) %>%
  clean_names() %>%
  rename(field_name = 1)





## ------------- Recreate Pat ID and YOB - (optional) -------------


 create new patient ID variable
 d <- d %>% mutate(record_id_new = stringi::stri_rand_strings(1, 12), .by = record_id, .after = record_id)

 export patient identification list
 openxlsx::write.xlsx(
   d %>% select(record_id, record_id_new),
   "FL_patient_identification_list.xlsx"
 )






# Year of birth
d$yob <- lubridate::year(as.Date(d$datum_geb, "%Y-%m-%d"))

# age at diagnosis - for now
d$diagnosis_age <- ifelse(is.na(d$diagnosis_date),
  as.numeric(d$diagnosis_date_year),
  lubridate::year(as.Date(d$diagnosis_date, "%Y-%m-%d"))
) - as.numeric(d$yob)




# convert comma in point and change laboratory values to numeric
d <- d %>%
  mutate(
    across(all_of(blood_marker), ~ as.numeric(gsub(",", ".", ., fixed = TRUE)))
  ) %>%
  mutate(
    across(all_of(blood_marker), ~ ifelse(. == 999, NA, .))
  )




# Check data type
check_blood_marker <- d %>%
  select(all_of(blood_marker))












# -------------- III. Data Preparation ---------------------------------------
cli_h1("I. Data Preparation")










# split data

## Baseline date
base <- d %>% filter(is.na(redcap_repeat_instance) & site == "Salzburg" & diagnosis == "fl")

## Therapy data
thx <- d %>%
  filter(redcap_repeat_instrument == "medical_treatment" & record_id %in% base$record_id) %>%
  select(record_id, cycle = redcap_repeat_instance, treat_y_n:medical_treatment_complete)

## Survival status data
sv <- d %>%
  filter(redcap_repeat_instrument == "survival_status" & record_id %in% base$record_id) %>%
  select(record_id, cycle = redcap_repeat_instance, starts_with("dod_ic_date"), date_of_update:cause_of_death_other)






# check survival & L2FU status
check_surv_l2fu_status <- sv %>%
  filter(surv_stat == "deceased" & surv_lost2fu_y_n == 1)










## ------------- 1. FLIPI24 Values -------------

# in sperate data frame and join at the end
flipi24 <- d %>%
  select(record_id, flipi24_value) %>%
  arrange(record_id) %>%
  group_by(record_id) %>%
  fill(flipi24_value, .direction = "downup") %>% 
  slice(1) %>% 
  ungroup()










## ------------- 2. FLIPI / FLIPI2 -------------

cli_h2("FLIPI / FLIPI2")


#' | FLIPI                    | FLIPI2                    |
# | ------------------------ | --------------------------|
# | Age > 60 y               | Age > 60 y                |
# | A.A.-Stage >= III        | Largest node > 6cm        |
# | Hb < 12 g/dl             | Hb < 12 g/dl              |
# | LDH elevated             | β2 microglobulin elevated |
# | Nodal areas involved > 4 | Bone marrow involvement   |


# Reference:
#   Philippe Solal-Céligny et al; Follicular Lymphoma International Prognostic Index.
#       Blood 2004; 104 (5): 1258–1265. doi: https://doi.org/10.1182/blood-2003-12-4434

#   https://www.mdcalc.com/calc/2320/follicular-lymphoma-international-prognostic-index-flipi

#   Massimo Federico et al. Follicular Lymphoma International Prognostic Index 2:
#      A New Prognostic Index for Follicular Lymphoma Developed by the
#      International Follicular Lymphoma Prognostic Factor Project. JCO 27, 4555-4562(2009).
#      DOI:10.1200/JCO.2008.21.3991
#      https://ascopubs.org/doi/10.1200/JCO.2008.21.3991



base_ <- calc_flipi(base)
base_ <- calc_flipi2(base_)










## ------------- 3. Survival Status -------------
cli_h2("Survival Status")

sv3 <- prepare_survival_status(sv)








## ------------- 4. Prepare Therapy Data -------------
cli_h2("prepare therapy data")


# Preparation of the therapy data
thx_ <- cleaning_therapy_variables(thx)

# check all unique meds
for (i in thx_$med_vars) {
  print(unique(thx_$data[[i]]))
}


# Combine treatment columns
thx2 <- combine_treatment_columns(thx_)






### ------------- 4.1 Calculate Gelf criteria -------------
#     Reference: https://www.mdcalc.com/calc/2321/groupe-detude-des-lymphomes-folliculaires-gelf-criteria
thx2 <- calculate_gelf_criteria(thx2)









### ------------- 4.2 Date Imputations -------------


# High level function. Partitioned into several smaller functions.
# See: scripts/cleaning/1_preparation.R
thx2 <- impute_dates(thx2)

# Transform values to labels for more readable purpose.
thx2 <- transform_values_to_labels(thx2)

# overview of all therapies
thx2 %>%
  separate_rows(drug_med_all, sep = ", ") %>%
  pull(drug_med_all) %>%
  unique() %>%
  sort()















# -------------- IV. Data Transformation For Export --------------------------
cli_h1("II. Data transformation for export")





## ------------- 1. Diagnosis -------------

# Transform and combine diagnosis & FL-transformation date
base1 <- combine_diagnosis_dates(base_)

# Transform variables to labels and cleaning some data
base1 <- transform_diagnosis_labels(base1)

# Combine the "other" fields in one cell
# like diagnosis, subtype, transformation type
base1 <- combine_other_fields(base1)

# Combine extranodal sites in one cell
base1 <- combine_extranodal_sites(base1)

# Combine nodal sites in one cell
base1 <- combine_nodal_sites(base1)




# Check for combining all extranodal site variables
check_site <- base1 %>% select(
  record_id, diag_site, starts_with("diag_extranodal_site___"),
  diag_extranodal_site_other, diag_extranodal_site_other_2, diag_extranodal_site_other_3,
)




# variable selection
diag <- base1 %>%
  select(
    record_id, first_name, last_name, yob, gender, registry_ic, registry_ic_date,

    # 1. diagnosis variables
    diagnosis, diagnosis_date, diagnosis_date_imputed_day, diagnosis_date_01, diagnosis_date_15, diagnosis_date_28, diagnosis_age,
    fl_classification, fl_grading,
    diagnosis_transformation, diag_transformation_after, diag_fl_transformation_date_01, diag_fl_transformation_date_15, diag_fl_transformation_date_28,
    diag_stage, diag_stage_pet, diag_ecog,

    # 2. diagnosis variables
    diag_nodal_fl, diag_nodal_site, diag_extranodal, diag_site, diag_secondmalign_y_n,
    diag_ldh_y_n, diag_ldh_baseline, diag_ldh_diag, diag_ldh_ratio, diag_ldh_elevated_y_n,


    # Laboratory
    albumin, b2mg, bilirubin, hemoglobine, diag_hemo_diag, neutrophils, platelet, leukocytes, lymphocytes, kreatinin, ggt,
    monocytes, harnsaeure, harnstoff, crp, fibrinogen,

    # FLIPI
    diag_flipi_age, diag_flipi_stage, diag_flipi_hemo, diag_flipi_ldh, diag_nodal_site_n_tot, diag_flipi_site,
    diag_flipi_n, diag_flipi_risk_grade,
    # FLIPI2
    diag_flipi_age, diag_flipi2_node, diag_flipi_hemo, diag_flipi_b2mg, diag_flipi2_marrow,
    diag_flipi2_n, diag_flipi2_risk_grade
  )















## ------------- 2. Combine Datasets -------------

final <- diag %>%
  left_join(thx2, by = "record_id") %>%
  left_join(sv3, by = "record_id") %>%
  # from the beginning
  left_join(flipi24, by = "record_id")










## ------------- 3. Cleaning Therapy Data / PFS / OS -------------


# Impute specific progression dates
##### final_ ----
final_ <- transform_progression_dates(final)

# Transform variables to labels of progression columns.
final2 <- transform_progression_labels(final_)

# Creates a column which shows, if the current therapy line is the last one
final2 <- create_max_treat_line(final2)



















## 4. Data Checks ----

# SD or PD, or progression date but the same line in the next therapy
final %>%
  group_by(record_id) %>%
  filter((treat_best_response %in% c("SD", "PD") & (!is.na(progression_before_date_01) | !is.na(therapy_relapse_date_15)) & (treat_line == lead(treat_line) & lead(drug1_start_reason) != "Maintenance therapy")))


final %>%
  filter(treat_best_response %in% c("SD") & (drug_med_all == lead(drug_med_all)) & lead(drug1_start_reason != "Maintenance therapy"))


final %>%
  group_by(record_id) %>%
  filter(is.na(progression_date_15) & (!is.na(progression_before_date_15) & treat_line != lead(treat_line)))


# progression before start of therapy but the therapy line is the same as the previous
final %>%
  group_by(record_id) %>%
  filter((!is.na(progression_before_date_15) | !is.na(therapy_relapse_date_15)) & treat_line == lag(treat_line))


# new therapy starts on new line but there is no PD or SD
final %>%
  group_by(record_id) %>%
  filter(treat_line != lag(treat_line) & (!is.na(progression_date_15) & !is.na(therapy_relapse_date_15)) & !treat_best_response %in% c("SD", "PD"))


final %>%
  select(record_id, cycle, treat_line, therapy_relapse_date_15, progression_date_01, treat_best_response)






final_check <- final %>%
  select(
    record_id, last_name, diagnosis,
    diagnosis_date, diagnosis_date_imputed_day, diagnosis_date_01, diagnosis_date_15, diagnosis_date_28,
    diagnosis_age,
    cycle, treat_y_n, treat_line,
    w_and_w_y_n, w_w_start_date_01, w_w_start_date_15, w_w_start_date_28,
    w_and_w_progr_no_therapy, w_w_progr_date_no_therapy_01, w_w_progr_date_no_therapy_15, w_w_progr_date_no_therapy_28,
    w_and_w_progr_therapy, w_w_progr_date_therapy_01, w_w_progr_date_therapy_15, w_w_progr_date_therapy_28,
    therapy_relapse_y_n, therapy_relapse_date, therapy_relapse_date_01, therapy_relapse_date_15, therapy_relapse_date_28,
    drug1_start_reason, progression_before_date, progression_before_date_, progression_before_date_01, progression_before_date_15, progression_before_date_28,
    therapy_start_date_, therapy_start_date_01, therapy_start_date_15, therapy_start_date_28,
    therapy_end_date_, therapy_end_date_01, therapy_end_date_15, therapy_end_date_28,
    progression_date, progression_date_, progression_date_01, progression_date_15, progression_date_28,
    therapy_status, drug1_end_reason,
    treat_best_response, treat_best_response_date, treat_best_response_date_,
    treat_best_response_date_01, treat_best_response_date_15, treat_best_response_date_28,
    treat_best_response_petct, treat_best_response_petct_date,
    treat_best_response_petct_date_01, treat_best_response_petct_date_15, treat_best_response_petct_date_28,
    surv_stat, status_date, status_date_01, status_date_15, status_date_28,
    progression_last_y_n, progression_sv_last_date
  )










final %>%
  arrange(therapy_start_date_15) %>%
  select(record_id, therapy_start_date_15)











# Filter, if patients have different progression_date__ and progression_before_date__
a <- final2 %>%
  group_by(record_id) %>%
  arrange(cycle, treat_line) %>%
  filter(lead(treat_line) > treat_line) %>%
  filter(progression_date_15 != lead(progression_before_date_15)) %>%
  ungroup()

b <- final2 %>%
  filter(record_id %in% a$record_id) %>%
  select(record_id, last_name, yob, treat_y_n, cycle, treat_line, w_and_w_y_n, treat_study:treat_best_response_petct_date_15)


# openxlsx::write.xlsx(b, "FL_Pat_diff_Progr_date.xlsx")
# _______________________________________________________










# Arrange cycle and treat_line correctly
final2a <- final2 %>%
  group_by(record_id) %>%
  filter(is.unsorted(treat_line)) %>%
  arrange(record_id, treat_line, cycle) %>%
  ungroup()


final2b <- final2 %>%
  anti_join(final2a, by = "record_id")


# Beide DF zusammenfügen
final2 <- bind_rows(final2b, final2a)

















## ------------- 5. Calculation of PFS / OS -------------


#' Calculates PFS & OS Values for the therapy line based data frame
final3 <- calc_pfs_os(final2)







# variable selection for merging later
line <- final3 %>%
  select(
    # characteristics
    record_id, gelf_criteria_all, treat_line_ecog,

    # Treat line characteristics
    treat_y_n, cycle, treat_line, max_treat_line,

    # Therapy data (w&w, relapse, start, end, progression, PFS, treatment)
    ### W&W
    w_and_w_y_n, w_w_start_date_imputed_day,
    w_w_start_date_01, w_w_start_date_15, w_w_start_date_28,
    w_and_w_progr_no_therapy, w_w_progr_date_no_therapy_01, w_w_progr_date_no_therapy_15, w_w_progr_date_no_therapy_28,
    w_and_w_progr_therapy, w_w_progr_date_therapy_imputed_day, w_w_progr_date_therapy_01, w_w_progr_date_therapy_15, w_w_progr_date_therapy_28,
    treat_no_reason,

    ### study info
    treat_study, treat_study_text,

    ### relapse
    therapy_relapse_y_n, therapy_relapse_date_imputed_day, therapy_relapse_date_01, therapy_relapse_date_15, therapy_relapse_date_28,

    ### progression dates
    drug1_start_reason,
    progression_before_date_imputed_day, progression_before_date_01, progression_before_date_15, progression_before_date_28,
    progression_before_type,

    ### therapy start & end date
    therapy_status, therapy_start_date_imputed_day, therapy_start_date_01, therapy_start_date_15, therapy_start_date_28,
    therapy_end_date_01, therapy_end_date_15, therapy_end_date_28,
    drug1_end_reason,

    ### Progression date with respect to all cases (progression, W&W, relapse)
    progression_date_imputed_day, progression_date__01, progression_date__15, progression_date__28, progression_type_line,
    treat_rebiopsy,

    ### best response & dates
    treat_best_response_line, treat_best_response_date_imputed_day, treat_best_response_date_01, treat_best_response_date_15, treat_best_response_date_28,
    treat_best_response_all,

    ### PET-CT dates
    treat_best_response_petct, treat_best_response_petct_date_01, treat_best_response_petct_date_15, treat_best_response_petct_date_28,

    ### PFS values for bootstapping & event
    PFS_01_01:PFS_event,

    ### treatment details (like chemo, radio, target, immune, CAR-T, stem cell transplant)
    drug_med_all_line, drug_med_all, drug_med_comb, drug_med_comb_cycle,
    drug_med_check_chemo___1, drug_med_chemo, drug_medchemo_cycles, drug_medchemo_end_y_n, drug_medchemo_end_date,
    drug_med_immune, drug_medimmune_cycles, drug_medimmune_end_y_n, drug_medimmune_end_date,
    drug_med_target, drug_medtarget_cycles, drug_medtarget_end_y_n, drug_medtarget_end_date,
    drug_med_other, drug_medother_cycles, drug_medother_end_y_n, drug_medother_end_date,
    drug_car_type, drug_car_text, drug_car_bridge, drug_car_bridge_text, drug_car_bridge_chemo, drug_car_bridge_chemo_other, comb_med_bridging,
    drug_hold_therapy, comb_med_holding,
    drug_lympho_therapy, comb_med_lympho,
    drug_intrathecal_check___1,
    drug_med_check_radio___1,
    drug_med_check_trans___1, treat_trans_type, treat_trans_date_01, treat_trans_date_15, treat_trans_date_28,
    drug_best_supportive_check___1,

    # _______________ Survival info
    ### (progression before death?)
    progression_last_y_n, progression_sv_last_date_01, progression_sv_last_date_15, progression_sv_last_date_28,
    ### Overall survival
    OS_01_01:OS_event,
    ### survival status & date
    surv_stat, status_date, status_date_imputed_day, status_date_01, status_date_15, status_date_28
  )


















# ##### V. Add labels (optional) ----
#
# # add labels based on the data dictionary
# final_ <- add_labels(final, dd)
#
# # capture labels to add as row
# labels <- sapply(final_, var_label)
# # convert NULL values in labels to NA
# labels <- lapply(labels, function(x) if (is.null(x)) NA else x)
# # combine labels with df
# final__ <- rbind(as.list(labels), final_)
#
#
# # export dataframe to excel file
# openxlsx::write.xlsx(final_, paste0("Lymphom_register_data_prepared2_",format(Sys.Date(),"%Y%m%d"),".xlsx"))












# Checks for pfs/os & treat_line & progression_sv_last_date_

# ggplot(survfit_model, aes(x = os_years, fill = factor(os_event))) +
#   geom_histogram(bins = 25, alpha = 0.5, position = "identity")




check_calc <- final3 %>%
  select(
    record_id, cycle, treat_line,
    w_and_w_y_n, w_w_progr_date_therapy_01, w_w_progr_date_therapy_15, w_w_progr_date_therapy_28,
    therapy_relapse_date_, therapy_relapse_date_01, therapy_relapse_date_15, therapy_relapse_date_28,
    progression_before_date_, progression_before_date_01, progression_before_date_15, progression_before_date_28,
    therapy_start_date_, therapy_start_date_01, therapy_start_date_15, therapy_start_date_28,
    progression_date_, progression_date_01, progression_date_15, progression_date_28,
    progression_date__01, progression_date__15, progression_date__28,
    PFS_01_01:PFS_event,
    diagnosis_date, diagnosis_date_01, diagnosis_date_15, diagnosis_date_28,
    surv_stat, status_date, status_date_01, status_date_15, status_date_28,
    OS_01_01:OS_event
  )

# openxlsx::write.xlsx(check_calc, file = file.path(results_dir, "FL_patient_PFS_OS_checks.xlsx"))






final_check2 <- final3 %>%
  filter(is.na(treat_line) & is.na(w_and_w_y_n)) %>%
  select(record_id, yob, diagnosis_date, cycle, treat_y_n, treat_line, w_and_w_y_n)

# openxlsx::write.xlsx(final_check2, "FL_patient_treat_line_checks.xlsx")




final_check3 <- final3 %>%
  filter(!is.na(progression_sv_last_date)) %>%
  select(record_id, yob, cycle, treat_y_n, treat_line, progression_sv_last_date, progression_sv_last_date)

# openxlsx::write.xlsx(final_check3, "FL_patient_treat_line_checks.xlsx")








# just specific ID numbers

# final for patient level
final <- final %>%
  filter(grepl("^1", record_id))



# for therpy lines level
final3 <- final3 %>%
  filter(grepl("^1", record_id))



# take over relevent changed variables
final <- final %>%
  filter(drug1_start_reason %in% c(
    "First diagnosis", "Progression", "No Reponse / stable disease", "Not done",
    "Patient's decision", "Toxicity", NA_real_
  ))




# Select specific variables
final <- final %>%
  select(record_id:diag_flipi2_risk_grade, gelf_criteria_all, flipi24_value, cycle, surv_stat, status_date:status_date_28)



















# variable selection for merging later
line <- final3 %>%
  select(
    # _______________ characteristics
    record_id, gelf_criteria_all, treat_line_ecog,

    # _______________ Treat line characteristics
    treat_y_n, cycle, treat_line, max_treat_line,

    # _______________ Therapy data (w&w, relapse, start, end, progression, PFS, treatment)
    ### W&W
    w_and_w_y_n, w_w_start_date_imputed_day,
    w_w_start_date_01, w_w_start_date_15, w_w_start_date_28,
    w_and_w_progr_no_therapy, w_w_progr_date_no_therapy_01, w_w_progr_date_no_therapy_15, w_w_progr_date_no_therapy_28,
    w_and_w_progr_therapy, w_w_progr_date_therapy_imputed_day, w_w_progr_date_therapy_01, w_w_progr_date_therapy_15, w_w_progr_date_therapy_28,
    treat_no_reason,

    ### study info
    treat_study, treat_study_text,

    ### relapse
    therapy_relapse_y_n, therapy_relapse_date_imputed_day, therapy_relapse_date_01, therapy_relapse_date_15, therapy_relapse_date_28,

    ### progression dates
    drug1_start_reason,
    progression_before_date_imputed_day, progression_before_date_01, progression_before_date_15, progression_before_date_28,
    progression_before_type,

    ### therapy start & end date
    therapy_status, therapy_start_date_imputed_day, therapy_start_date_01, therapy_start_date_15, therapy_start_date_28,
    therapy_end_date_01, therapy_end_date_15, therapy_end_date_28,
    drug1_end_reason,

    ### Progression date with respect to all cases (progression, W&W, relapse)
    progression_date_imputed_day, progression_date__01, progression_date__15, progression_date__28, progression_type_line,
    treat_rebiopsy,

    ### best response & dates
    treat_best_response_line, treat_best_response_date_imputed_day, treat_best_response_date_01, treat_best_response_date_15, treat_best_response_date_28,
    treat_best_response_all,

    ### PET-CT dates
    treat_best_response_petct, treat_best_response_petct_date_01, treat_best_response_petct_date_15, treat_best_response_petct_date_28,

    ### PFS values for bootstapping & event
    PFS_01_01:PFS_event,

    ### treatment details (like chemo, radio, target, immune, CAR-T, stem cell transplant)
    drug_med_all_line, drug_med_all, drug_med_comb, drug_med_comb_cycle,
    drug_med_check_chemo___1, drug_med_chemo, drug_medchemo_cycles, drug_medchemo_end_y_n, drug_medchemo_end_date,
    drug_med_immune, drug_medimmune_cycles, drug_medimmune_end_y_n, drug_medimmune_end_date,
    drug_med_target, drug_medtarget_cycles, drug_medtarget_end_y_n, drug_medtarget_end_date,
    drug_med_other, drug_medother_cycles, drug_medother_end_y_n, drug_medother_end_date,
    drug_car_type, drug_car_text, drug_car_bridge, drug_car_bridge_text, drug_car_bridge_chemo, drug_car_bridge_chemo_other, comb_med_bridging,
    drug_hold_therapy, comb_med_holding,
    drug_lympho_therapy, comb_med_lympho,
    drug_intrathecal_check___1,
    drug_med_check_radio___1,
    drug_med_check_trans___1, treat_trans_type, treat_trans_date_01, treat_trans_date_15, treat_trans_date_28,
    drug_best_supportive_check___1,

    # _______________ Survival info
    ### (progression before death?)
    progression_last_y_n, progression_sv_last_date_01, progression_sv_last_date_15, progression_sv_last_date_28,
    ### Overall survival
    OS_01_01:OS_event,
    ### survival status & date
    surv_stat, status_date, status_date_imputed_day, status_date_01, status_date_15, status_date_28
  )







































# -------------- VI. Completed data export -----------------------------------
cli_h1("5. Completed data export")




# combine therapy variables with the ones of diagnosis
tot <- final %>%
  filter(!is.na(record_id)) %>%
  select(record_id:diag_flipi2_risk_grade) %>%
  distinct(record_id, .keep_all = T) %>%
  left_join(line, by = "record_id", "cycle")





# export dataset on therapy line level
# export it to results/%Y%m%d-cleaning.csv
openxlsx::write.xlsx(tot, file = file.path(results_dir, "cleaned_therapy_lines_data.xlsx"))
readr::write_csv(tot, file = file.path(results_dir, "cleaned_therapy_lines_data.csv"))

# export cleaned data to the data/cleaned folder
save_cleaned_result(tot, filename_prefix = "therapy_lines")






# create dataframe of patient level without therapy variables
pat <- final %>%
  filter(!is.na(record_id)) %>%
  select(record_id:flipi24_value, surv_stat, status_date:status_date_28) %>%
  # Create different OS version
  mutate(
    !!sym("OS_01_01") := !!os_time_logic("01", "01"),
    !!sym("OS_01_15") := !!os_time_logic("15", "01"),
    !!sym("OS_01_28") := !!os_time_logic("28", "01"),
    !!sym("OS_15_01") := !!os_time_logic("01", "15"),
    !!sym("OS_15_15") := !!os_time_logic("15", "15"),
    !!sym("OS_15_28") := !!os_time_logic("28", "15"),
    !!sym("OS_28_01") := !!os_time_logic("01", "28"),
    !!sym("OS_28_15") := !!os_time_logic("15", "28"),
    !!sym("OS_28_28") := !!os_time_logic("28", "28"),
    OS_event = case_when(
      surv_stat == "deceased" ~ 1,
      surv_stat %in% c("alive", "Lost2FU") ~ 0,
      TRUE ~ NA_real_
    ), .before = surv_stat
  ) %>%
  distinct(record_id, .keep_all = T)



# export dataset on patient level
# export it to results/%Y%m%d-cleaning.csv
openxlsx::write.xlsx(pat, file = file.path(results_dir, "cleaned_diagnosis_survival_data.xlsx"))
readr::write_csv(pat, file = file.path(results_dir, "cleaned_diagnosis_survival_data.csv"))

# export cleaned data to the data/cleaned folder
save_cleaned_result(pat, filename_prefix = "diagosis_survival")












# -------------- VII. Biobank -----------------------------------

bio <- create_biobank_file(d)

### File & stats of biobank

bio_all <- bio %>%
  left_join(pat, by = "record_id") %>%
  select(
    record_id, first_name, last_name, yob, gender,
    diagnosis, diagnosis_date, diagnosis_date_imputed_day, diagnosis_date_15, diagnosis_age,
    fl_classification, fl_grading,
    diagnosis_transformation, diag_transformation_after,
    diag_stage, diag_stage_pet, diag_ecog, diag_flipi_risk_grade, diag_flipi2_risk_grade,
    cycle:sample_region_details
  )




# write.xlsx(bio_all, "FL_Biobank.xlsx")

c <- bio_all %>%
  distinct(record_id, sample_origin) %>%
  count(sample_origin)
c <- as.data.frame(c)

# write.xlsx(c, "c.xlsx")
