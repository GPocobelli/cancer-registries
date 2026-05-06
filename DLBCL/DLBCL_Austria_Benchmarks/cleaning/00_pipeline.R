

# scripts/cleaning/00_pipeline.R







# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# --------------- Tumor Registry - Data Handling -----------------------------------------------------------
# 
#' Structure:
#' 
#'    > {cli} Pipeline for clean output. Jump to: **>> START PIPELINE HERE <<**
#' 
#' 
#'    > Loading Required Packages:    "scripts/load_lib.R" includes all relevant packages (adjustable)
#'    > Loading Data Handling Files:  all files where relevant functions are found
#'    > General Path for Output:      creates a path and related folder, where all created results are saved
#'    > Conflict Handling:            prevents errors regarding package crossovers
#'    > Data import:                  imports data from REDCap via API key and latest Data Dictionary (must be downloaded before and saved in the path: "doc/")
#'    > Data Cleaning:                cleaning and transforming data set to prepare it for the analysis
#'    > Data Checks:                  checks created outputs -> proper and detailed testing is found in the path: "tests/"
#'    > Data Export:                  making the final data set useful, readable and export ready 
#'                                    --> Use function `save_cleaned_result()` to save the final result in 3 different data formats (csv, xlsx, rds) in the directory ⁠data/cleaned/⁠.
#
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



run_cleaning_pipeline <- function(api_url,
                                  api_token,
                                  results_prefix = "Result",
                                  dd_folder_path = "doc/",                                  
                                  show_session_info = FALSE,
                                  verbose = FALSE,
                                  export_results = TRUE,
                                  assign_dd_global = TRUE) {
  
  cli::cli_h1("Pipeline: Datenimport & Cleaning")
  cli::cli_rule()
  
  steps <- c(
    "1. Setup: Results-Ordner",
    "2. Setup: Conflict handling",
    "3. Import: REDCap Daten + Data Dictionary",
    "4. Check: Import-Struktur",
    "5. Clean: Scores berechnen",
    "6. Clean: Patient Characteristics",
    "7. Clean: combine `*_other` fields",
    "8. Clean: Diagnose-Daten zusammenführen",
    "9. Clean: Therapy variables (raw cleaning)",
    "10. Clean: Therapy columns zusammenführen",
    "11. Clean: Therapy values -> labels",
    "12. Impute: Therapy dates (01/15/28 + flags)",
    "13. Clean: Survival Status",
    "14. Select: Diagnose-Variablen",
    "15. Merge: Diagnose + Therapie + Survival",
    "16. Transform: Progression labels",
    "17. Transform: Max therapy line",
    "18. Impute: empirische Datumswerte",
    "19. Calculate: PFS / OS",
    "20. Create Treatment-Flags",
    "21. Create: Therapie-Linien-Datensatz",
    "22. Ceate: Patienten-Datensatz",
    "23. Arrange: Patienten-Datensatz Spaltenreihenfolge",
    "24. Export: Ergebnisse speichern"
  )
  
  pb_env <- environment()
  pb_id <- cli::cli_progress_bar(
    name = "Fortschritt",
    type = "tasks",
    total = length(steps),
    format = "{cli::pb_current}/{cli::pb_total}  {cli::pb_status}",
    auto_terminate = FALSE,
    .envir = pb_env
  )
  on.exit(cli::cli_progress_done(id = pb_id, .envir = pb_env), add = TRUE)
  
  i <- 0
  logs <- list()
  warns <- list()
  
  run_step <- function(step_label, expr) {
    i <<- i + 1
    cli::cli_progress_update(
      id = pb_id,
      set = i,
      status = step_label,
      force = TRUE,
      .envir = pb_env
    )
    
    t0 <- Sys.time()
    out_lines <- character(0)
    msg_lines <- character(0)
    step_warnings <- character(0)
    
    out <- tryCatch(
      withCallingHandlers(
        {
          msg_handler <- function(m) {
            msg_lines <<- c(msg_lines, conditionMessage(m))
            invokeRestart("muffleMessage")
          }
          
          if (!isTRUE(verbose)) {
            out_obj <- NULL
            out_lines <<- utils::capture.output(
              out_obj <- withCallingHandlers(force(expr), message = msg_handler),
              type = "output"
            )
            out_obj
          } else {
            withCallingHandlers(force(expr), message = msg_handler)
          }
        },
        warning = function(w) {
          step_warnings <<- c(step_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        cli::cli_alert_danger("Fehler in Schritt: {step_label}")
        cli::cli_alert_danger("{conditionMessage(e)}")
        
        if (length(msg_lines)) {
          cli::cli_text("{.strong Captured messages:}")
          cli::cli_text("• {msg_lines}")
        }
        if (length(out_lines)) {
          cli::cli_text("{.strong Captured output:}")
          cli::cli_text("• {out_lines}")
        }
        
        cli::cli_progress_done(id = pb_id, result = "failed", .envir = pb_env)
        stop(e)
      }
    )
    
    dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    
    logs[[step_label]] <<- list(
      output = out_lines,
      messages = msg_lines
    )
    
    if (length(step_warnings)) {
      warns[[step_label]] <<- unique(step_warnings)
    }
    
    cli::cli_alert_success("{step_label} ({dt} s)")
    out
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 1) Setup ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  results_dir <- run_step("1. Setup: Results-Ordner", {
    create_results_wd(results_prefix)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 2) Conflict handling ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  run_step("2. Setup: Conflict handling", {
    if (requireNamespace("conflicted", quietly = TRUE)) {
      conflicted::conflict_prefer("filter", "dplyr")
      conflicted::conflict_prefer("lag", "dplyr")
      conflicted::conflict_prefer("select", "dplyr")
    } else {
      cli::cli_alert_warning(
        "Package 'conflicted' nicht verfügbar – conflict_prefer() wird übersprungen."
      )
    }
    invisible(TRUE)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 3) Import ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data <- run_step("3. Import: REDCap Daten + Data Dictionary", {
    import_data(
      api_url = api_url,
      api_token = api_token,
      folder_path = dd_folder_path
    )
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 4) Check: Import-Struktur ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  run_step("4. Check: Import-Struktur", {
    needed <- c("base", "thx", "sv", "dd")
    missing <- needed[!needed %in% names(data)]
    
    if (length(missing)) {
      stop(
        "import_data() liefert diese Komponenten nicht: ",
        paste(missing, collapse = ", ")
      )
    }
    
    if (is.null(data$base)) stop("data$base ist NULL (Patienten-Basisdaten fehlen).")
    if (is.null(data$thx))  stop("data$thx ist NULL (Therapie-Daten fehlen).")
    if (is.null(data$sv))   stop("data$sv ist NULL (Survival-Status fehlt).")
    if (is.null(data$dd))   stop("data$dd ist NULL (Data Dictionary fehlt).")
    
    invisible(TRUE)
  })
  
  dd <- data$dd
  
  if (isTRUE(assign_dd_global)) {
    assign("dd", dd, envir = .GlobalEnv)
  }
  
  if (isTRUE(show_session_info)) {
    cli::cli_h2("Session Info")
    print(sessionInfo())
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 5-8) Basisdaten / Diagnose ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  base1 <- run_step("5. Clean: Scores berechnen", {
    calc_scores(data$base)
  })
  
  base2 <- run_step("6. Clean: Patient Characteristics", {
    clean_patient_characteristics(base1)
  })
  
  base3 <- run_step("7. Clean: combine `*_other` fields", {
    combine_other_fields(base2)
  })
  
  base_ <- run_step("8. Clean: Diagnose-Daten zusammenführen", {
    combine_diagnosis_dates(base3)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 9-12) Therapie ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  thx1 <- run_step("9. Clean: Therapy variables (raw cleaning)", {
    cleaning_therapy_variables(data$thx)
  })
  
  date_vars <- thx1$date_vars
  med_vars  <- thx1$med_vars
  
  thx2 <- run_step("10. Clean: Therapy columns zusammenführen", {
    combine_treatment_columns(thx1)
  })
  
  thx3 <- run_step("11. Clean: Therapy values -> labels", {
    transform_treatment_to_labels(thx2)
  })
  
  thx_dates <- run_step("12. Impute: Therapy dates (01/15/28 + flags)", {
    impute_therapy_dates_fixed_day(thx3)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 13) Survival ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sv_ <- run_step("13. Clean: Survival Status", {
    prepare_survival_status(data$sv)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 14-15) Merge ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  diag <- run_step("14. Select: Diagnose-Variablen", {
    base_ %>%
      dplyr::select(
        record_id, first_name, last_name, datum_geb, gender, yob,
        registry_ic, registry_ic_date,
        
        diagnosis,
        diagnosis_date, diagnosis_date_imputed_day, diagnosis_date_month, diagnosis_date_year,
        diagnosis_date_01, diagnosis_date_15, diagnosis_date_28,
        diagnosis_age,
        
        diagnosis_subtype_coo, diagnosis_subtype, diag_stage, diag_ecog,
        
        diagnosis_transformation_date, diagnosis_transformation_date_imputed_day,
        diagnosis_transformation_date_01, diagnosis_transformation_date_15, diagnosis_transformation_date_28,
        diagnosis_transformation_before,
        
        diag_extranodal_site_n_tot, diag_ipi_n, diag_ipi_score_standard, diag_ipi_score_revised, diag_ipi_score_aa,
        diag_ipi_score_cns,
        
        diag_nccn_n, diag_nccn_score,
        diag_secondmalign_y_n,
        
        diag_ldh_baseline, diag_ldh_diag, diag_ldh_ratio, diag_ldh_elevated_y_n,
        
        albumin, b2mg, bilirubin, crp, fibrinogen, ggt, harnsaeure, harnstoff, hemoglobine,
        kreatinin, ldh, leukocytes, lymphocytes, monocytes, neutrophils, platelet
      )
  })
  
  final <- run_step("15. Merge: Diagnose + Therapie + Survival", {
    diag %>%
      dplyr::left_join(thx_dates, by = "record_id") %>%
      dplyr::left_join(sv_, by = "record_id")
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 16-18) weitere Transformationen ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final_ <- run_step("16. Transform: Progression labels", {
    transform_progression_to_labels(final)
  })
  
  line <- run_step("17. Transform: Max therapy line", {
    create_max_treat_line(final_)
  })
  
  final2 <- run_step("18. Impute: empirische Datumswerte", {
    impute_dates_empirical(final_)
  })
  
  final3 <- run_step("19. Calculate: PFS / OS", {
    calc_pfs_os(final2)
  })
  
  final3 <- run_step("20. Create Treatment-Flags", {
    create_treatment_flags(final3)
  })
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 19) Spaltenlisten für Output-Datensätze ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  patienteninfo <- c(
    "record_id",
    "first_name",
    "last_name",
    "datum_geb",
    "yob",
    "gender",
    "registry_ic",
    "registry_ic_date"
  )
  
  characteristics <- c(
    "diagnosis_age",
    "diagnosis",
    "diagnosis_subtype_coo",
    "diagnosis_subtype",
    "diagnosis_date",
    "diagnosis_date_month",
    "diagnosis_date_year",
    "diagnosis_date_imputed_day",
    "diagnosis_date_01",
    "diagnosis_date_15",
    "diagnosis_date_28",
    "diagnosis_date_",
    "diagnosis_date_density_imp",
    "diagnosis_transformation_before",
    "diagnosis_transformation_date",
    "diagnosis_transformation_date_imputed_day",
    "diagnosis_transformation_date_01",
    "diagnosis_transformation_date_15",
    "diagnosis_transformation_date_28",
    "diag_stage",
    "diag_ecog",
    "diag_extranodal_site_n_tot",
    "diag_ipi_n",
    "diag_ipi_score_standard",
    "diag_ipi_score_revised",
    "diag_ipi_score_aa",
    "diag_ipi_score_cns",
    "diag_nccn_n",
    "diag_nccn_score",
    "diag_secondmalign_y_n"
  )
  
  lab <- c(
    "diag_ldh_baseline",
    "diag_ldh_diag",
    "diag_ldh_ratio",
    "diag_ldh_elevated_y_n",
    "ldh_group",
    "albumin",
    "b2mg",
    "bilirubin",
    "crp",
    "fibrinogen",
    "ggt",
    "harnsaeure",
    "harnstoff",
    "hemoglobine",
    "kreatinin",
    "ldh",
    "leukocytes",
    "lymphocytes",
    "monocytes",
    "neutrophils",
    "platelet"
  )
  
  therapy <- c(
    "treat_y_n",
    "treat_line",
    "cycle",
    "max_treat_line",
    "max_treat_line_num",
    "is_last_line",
    
    "treat_study",
    "treat_study_text",
    "treat_study_line",
    
    "therapy_relapse_y_n",
    "therapy_relapse_date",
    "therapy_relapse_date_imputed_day",
    "therapy_relapse_date_01",
    "therapy_relapse_date_15",
    "therapy_relapse_date_28",
    "therapy_relapse_y_n_line",
    
    "drug1_start_reason",
    
    "therapy_status",
    "therapy_start_date",
    "therapy_start_date_month",
    "therapy_start_date_year",
    "therapy_start_date_imputed_day",
    "therapy_start_date_01",
    "therapy_start_date_15",
    "therapy_start_date_28",
    "therapy_start_date_",
    "therapy_start_date_density_imp",
    
    "Time_diag_to_txstart",
    "delta_imp",
    "empirical_fallback",
    
    "progression_before_y_n_line",
    "progression_before_date",
    "progression_before_date_imputed_day",
    "progression_before_date_01",
    "progression_before_date_15",
    "progression_before_date_28",
    "progression_before_type",
    "progression_before_type_line",
    
    "therapy_end_date",
    "therapy_end_date_month",
    "therapy_end_date_year",
    "therapy_end_date_imputed_day",
    "therapy_end_date_01",
    "therapy_end_date_15",
    "therapy_end_date_28",
    "therapy_end_date_",
    
    "drug1_end_reason",
    
    "progression_date",
    "progression_date_imputed_day",
    "progression_date_01",
    "progression_date_15",
    "progression_date_28",
    "progression_event_date_imputed_day",
    "progression_event_date_01",
    "progression_event_date_15",
    "progression_event_date_28",
    "progression_type",
    "progression_type_line",
    "progression_y_n_line",
    
    "treat_best_response_all",
    "treat_best_response",
    "treat_best_response_line",
    "treat_best_response_date",
    "treat_best_response_date_imputed_day",
    "treat_best_response_date_01",
    "treat_best_response_date_15",
    "treat_best_response_date_28",
    "treat_best_response_petct",
    "treat_best_response_petct_date",
    "treat_best_response_petct_date_01",
    "treat_best_response_petct_date_15",
    "treat_best_response_petct_date_28",
    
    "treat_rebiopsy",
    "treat_rebiopsy_line",
    
    "drug_med_all",
    "drug_med_all_line",
    "drug_clean",
    "drug_upper",
    
    "empty_flag",              # keine Angabe / leer
    
    
    # Nicht-systemische Therapie
    "bsc_any_flag",            # irgendeine BSC
    "no_treatment_bsc_flag",   # explizit keine Therapie (BSC only)
    
    "rt_flag",                 # Radiotherapie
    "intrathecal_flag",        # intrathekale Therapie
    
    
    # Systemische Therapie – klassische Chemo
    "anthracyclin_flag",       # zentrale mechanistische Klasse
    
    "rchop_flag",
    "chop_flag",
    "rcomp_flag",
    "repoch_flag",
    "pola_rchp_flag",
    
    "benda_flag",
    "gemox_flag",
    "matrix_flag",
    
    "intensive_chemo_flag",
    "nonintensive_chemo_flag",
    
    
    # Salvage-Regime (eigene Kategorie!)
    "salvage_gdp_flag",
    "salvage_dhap_flag",
    "salvage_ice_flag",
    
    

    # Einzelsubstanzen (Chemo)
    "gemcitabine_flag",
    "pixantrone_flag",
    "paclitaxel_flag",
    
    
    # Immuntherapie / Antikörper / Bispecifics
    "ritux_only_flag",
    "antibody_only_flag",
    
    "tafa_flag",
    "glofi_flag",
    "pola_flag",
    "epco_flag",
    "mosun_flag",
    "bispecific_flag",
    
    
    # Targeted Therapien
    "ibrutinib_flag",
    "acala_flag",
    "btk_flag",
    
    "len_flag",
    "veneto_flag",
    
    "checkpoint_flag",
    
    "other_targeted_flag",
    "targeted_flag",
    
    

    # Eskalation / zelluläre Therapie
    "asct_flag",
    "allo_flag",
    "beam_flag",
    
    "cart_flag",
    
    
    # Globale Therapie-Indikatoren
    "any_systemic_flag",
    
    

    # Gruppierung / Analyseebene
    "therapy_group",
    "therapy_subgroup",
    "therapy_category",
    "treatment_intention",
    
    
    "PFS_event",
    "PFS_01_01",
    "PFS_15_01",
    "PFS_28_01",
    "PFS_01_15",
    "PFS_15_15",
    "PFS_28_15",
    "PFS_01_28",
    "PFS_15_28",
    "PFS_28_28",
    "PFS_density",
    
    "drug_med_comb",
    "drug_med_comb_cycle",
    "drug_med_check_chemo___1",
    "drug_med_chemo",
    "drug_medchemo_cycles",
    "drug_medchemo_end_y_n",
    "drug_medchemo_end_date",
    "drug_med_immune",
    "drug_medimmune_cycles",
    "drug_medimmune_end_y_n",
    "drug_medimmune_end_date",
    "drug_med_target",
    "drug_medtarget_cycles",
    "drug_medtarget_end_y_n",
    "drug_medtarget_end_date",
    "drug_med_other",
    "drug_medother_cycles",
    "drug_medother_end_y_n",
    "drug_medother_end_date",
    "drug_med_comb_line",
    "drug_med_comb_cycle_line",
    "drug_med_check_chemo___1_line",
    "drug_med_chemo_line",
    "drug_medchemo_cycles_line",
    "drug_medchemo_end_y_n_line",
    "drug_medchemo_end_date_line",
    "drug_med_immune_line",
    "drug_medimmune_cycles_line",
    "drug_medimmune_end_y_n_line",
    "drug_medimmune_end_date_line",
    "drug_med_target_line",
    "drug_medtarget_cycles_line",
    "drug_medtarget_end_y_n_line",
    "drug_medtarget_end_date_line",
    "drug_med_other_line",
    "drug_medother_cycles_line",
    "drug_medother_end_y_n_line",
    "drug_medother_end_date_line",
    "drug_car_type",
    "drug_car_text",
    "drug_car_date",
    "drug_car_date_month",
    "drug_car_date_year",
    "drug_car_bridge",
    "drug_car_bridge_date",
    "drug_car_bridge_date_month",
    "drug_car_bridge_date_year",
    "drug_car_bridge_text",
    "drug_car_bridge_chemo",
    "drug_car_bridge_chemo_other",
    "comb_med_bridging",
    "drug_hold_therapy",
    "comb_med_holding",
    "drug_car_holding_date",
    "drug_car_holding_date_month",
    "drug_car_holding_date_year",
    "drug_lympho_therapy",
    "comb_med_lympho",
    "drug_car_type_line",
    "drug_car_text_line",
    "drug_car_date_line",
    "drug_car_date_month_line",
    "drug_car_date_year_line",
    "drug_car_bridge_line",
    "drug_car_bridge_date_line",
    "drug_car_bridge_date_month_line",
    "drug_car_bridge_date_year_line",
    "drug_car_bridge_text_line",
    "drug_car_bridge_chemo_line",
    "drug_car_bridge_chemo_other_line",
    "comb_med_bridging_line",
    "drug_hold_therapy_line",
    "comb_med_holding_line",
    "drug_car_holding_date_line",
    "drug_car_holding_date_month_line",
    "drug_car_holding_date_year_line",
    "drug_lympho_therapy_line",
    "comb_med_lympho_line",
    "drug_intrathecal_check___1",
    "drug_med_check_radio___1",
    "drug_med_check_trans___1",
    "drug_best_supportive_check___1",
    "drug_intrathecal_check___1_line",
    "drug_med_check_radio___1_line",
    "drug_med_check_trans___1_line",
    "drug_best_supportive_check___1_line",
    "treat_trans_type",
    "treat_trans_date",
    "treat_trans_date_01",
    "treat_trans_date_15",
    "treat_trans_date_28",
    "treat_trans_type_line",
    "treat_trans_date_line",
    "treat_trans_date_01_line",
    "treat_trans_date_15_line",
    "treat_trans_date_28_line",
    
    "progression_last_y_n",
    "progression_sv_last_date",
    "progression_sv_last_date_01",
    "progression_sv_last_date_15",
    "progression_sv_last_date_28"
  )
  
  status_tot <- c(
    "surv_stat",
    "status_date",
    "status_date_imputed_day",
    "status_date_01",
    "status_date_15",
    "status_date_28",
    "OS_event",
    "OS_01_01",
    "OS_15_01",
    "OS_28_01",
    "OS_01_15",
    "OS_15_15",
    "OS_28_15",
    "OS_01_28",
    "OS_15_28",
    "OS_28_28",
    "OS_density"
  )
  
  status_pat <- c(
    "surv_stat",
    "status_date",
    "status_date_imputed_day",
    "status_date_01",
    "status_date_15",
    "status_date_28",
    "OS_event",
    "OS_01_01",
    "OS_15_01",
    "OS_28_01",
    "OS_01_15",
    "OS_15_15",
    "OS_28_15",
    "OS_01_28",
    "OS_15_28",
    "OS_28_28",
    "OS_density"
  )
  
  tot_ordered_names <- c(
    patienteninfo,
    characteristics,
    lab,
    therapy,
    status_tot
  )
  
  pat_ordered_names <- c(
    patienteninfo,
    characteristics,
    lab,
    status_pat
  )
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 20) Therapie-Linien-Datensatz ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tot <- run_step("21. Create: Therapie-Linien-Datensatz", {
    out <- final3 %>%
      dplyr::filter(!is.na(record_id)) %>%
      dplyr::distinct()
    
    if (is.data.frame(line)) {
      line_small <- line %>%
        dplyr::select(record_id, cycle, max_treat_line) %>%
        dplyr::distinct()
      
      out <- out %>%
        dplyr::left_join(line_small, by = c("record_id", "cycle"))
    }
    
    out
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 21) Therapie-Linien-Datensatz Spaltenreihenfolge ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tot <- run_step("22. Arrange: Therapie-Linien-Datensatz Spaltenreihenfolge", {
    
    missing_in_order <- setdiff(colnames(tot), tot_ordered_names)
    missing_in_tot   <- setdiff(tot_ordered_names, colnames(tot))
    duplicated_order <- tot_ordered_names[duplicated(tot_ordered_names)]
    
    if (length(missing_in_order) > 0) {
      stop(
        "Diese Variablen sind in tot vorhanden, aber nicht in tot_ordered_names: ",
        paste(missing_in_order, collapse = ", ")
      )
    }
    
    if (length(missing_in_tot) > 0) {
      stop(
        "Diese Variablen sind in tot_ordered_names definiert, fehlen aber in tot: ",
        paste(missing_in_tot, collapse = ", ")
      )
    }
    
    if (length(duplicated_order) > 0) {
      stop(
        "Doppelte Variablennamen in tot_ordered_names: ",
        paste(unique(duplicated_order), collapse = ", ")
      )
    }
    
    tot[, tot_ordered_names]
  })
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 22) Patienten-Datensatz ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pat <- run_step("23. Create: Patienten-Datensatz", {
    out <- final3 %>%
      dplyr::filter(!is.na(record_id)) %>%
      dplyr::select(dplyr::all_of(pat_ordered_names)) %>%
      dplyr::distinct(record_id, .keep_all = TRUE)
    
    missing_in_order <- setdiff(colnames(out), pat_ordered_names)
    missing_in_pat   <- setdiff(pat_ordered_names, colnames(out))
    duplicated_order <- pat_ordered_names[duplicated(pat_ordered_names)]
    
    if (length(missing_in_order) > 0) {
      stop(
        "Diese Variablen sind in pat vorhanden, aber nicht in pat_ordered_names: ",
        paste(missing_in_order, collapse = ", ")
      )
    }
    
    if (length(missing_in_pat) > 0) {
      stop(
        "Diese Variablen sind in pat_ordered_names definiert, fehlen aber in pat: ",
        paste(missing_in_pat, collapse = ", ")
      )
    }
    
    if (length(duplicated_order) > 0) {
      stop(
        "Doppelte Variablennamen in pat_ordered_names: ",
        paste(unique(duplicated_order), collapse = ", ")
      )
    }
    
    out[, pat_ordered_names]
  })
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---- 23) Export ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  run_step("24. Export: Ergebnisse speichern", {
    if (isTRUE(export_results)) {
      # Therapie-Linien
      openxlsx::write.xlsx(
        tot,
        file = file.path(results_dir, "cleaned_therapy_lines_data.xlsx")
      )
      readr::write_csv(
        tot,
        file = file.path(results_dir, "cleaned_therapy_lines_data.csv")
      )
      save_cleaned_result(tot, filename_prefix = "therapy_lines")
      
      # Patientenebene
      openxlsx::write.xlsx(
        pat,
        file = file.path(results_dir, "cleaned_diagnosis_survival_data.xlsx")
      )
      readr::write_csv(
        pat,
        file = file.path(results_dir, "cleaned_diagnosis_survival_data.csv")
      )
      save_cleaned_result(pat, filename_prefix = "diagnosis_survival")
    }
    
    invisible(TRUE)
  })
  
  
  
  cli::cli_h1("Pipeline abgeschlossen")
  cli::cli_rule()
  cli::cli_alert_info("Output-Ordner: {results_dir}")
  
  if (length(warns)) {
    cli::cli_h2("Warnungen (kompakt)")
    for (nm in names(warns)) {
      cli::cli_alert_warning("{nm}: {length(warns[[nm]])} Warnung(en)")
      if (isTRUE(verbose)) {
        cli::cli_text("• {warns[[nm]]}")
      }
    }
    cli::cli_text("Hinweis: Für Details setze verbose = TRUE.")
  }
  
  if (isTRUE(verbose)) {
    cli::cli_h2("Step-Logs (verbose)")
    for (nm in names(logs)) {
      if (length(logs[[nm]]$messages)) {
        cli::cli_text("{.strong {nm}} – Messages:")
        cli::cli_text("• {logs[[nm]]$messages}")
      }
      if (length(logs[[nm]]$output)) {
        cli::cli_text("{.strong {nm}} – Output:")
        cli::cli_text("• {logs[[nm]]$output}")
      }
    }
  }
  
  list(
    results_dir   = results_dir,
    dd            = dd,
    raw           = data,
    base1         = base1,
    base2         = base2,
    base3         = base3,
    base          = base_,
    diag          = diag,
    thx1          = thx1,
    thx2          = thx2,
    thx3          = thx3,
    thx           = thx_dates,
    sv            = sv_,
    final_raw     = final,
    final_lbl     = final_,
    final_imp     = final2,
    final         = final3,
    therapy_lines = tot,
    patient_level = pat,
    date_vars     = date_vars,
    med_vars      = med_vars,
    logs          = logs,
    warnings      = warns
  )
}
