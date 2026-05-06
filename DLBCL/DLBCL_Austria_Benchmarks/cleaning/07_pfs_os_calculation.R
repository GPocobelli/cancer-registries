

# scripts/07_pfs_os_calculation.R



## 3 Calculation of PFS / OS ----





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helpers for collapsing
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
safe_min_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(as.Date(NA))
  min(x)
}

safe_max_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(as.Date(NA))
  max(x)
}

last_non_na <- function(x) {
  x2 <- x[!is.na(x)]
  if (length(x2) == 0) NA else dplyr::last(x2)
}

flag_for_min_date <- function(date_vec, flag_vec) {
  if (all(is.na(date_vec))) return(NA)
  idx <- which(!is.na(date_vec))
  i2 <- idx[which.min(date_vec[idx])]
  flag_vec[i2]
}

base_date_name <- function(x) str_replace(x, "_(01|15|28)$", "")



flag_for_extreme_date <- function(date_vec, flag_vec, which = c("min","max")) {
  which <- match.arg(which)
  if (all(is.na(date_vec))) return(NA)
  idx <- which(!is.na(date_vec))
  if (length(idx) == 0) return(NA)
  i2 <- if (which == "min") idx[which.min(date_vec[idx])] else idx[which.max(date_vec[idx])]
  flag_vec[i2]
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Collapse rulebased:
# - Date-Spalten: min oder max je nach Name (Start/Diagnosis = min; Status/End/Last = max)
# - Character/Factor: last_non_na
# - Flags *_imputed_day: passend zum gewählten min-Datum
# - Ergebnis: 1 Zeile pro record_id × treat_line
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collapse_lines_rulebased <- function(df,
                                     date_order_col = "therapy_start_date_15") {
  
  
  stopifnot(date_order_col %in% names(df))
  
  # --- Whitelist: Date-Spalten, die wirklich collapsed werden sollen
  date_cols <- intersect(names(df), c(
    # anchors
    "diagnosis_date_01","diagnosis_date_15","diagnosis_date_28",
    "therapy_start_date_01","therapy_start_date_15","therapy_start_date_28",
    "therapy_end_date_01","therapy_end_date_15","therapy_end_date_28",
    
    # censoring / status
    "status_date_01","status_date_15","status_date_28",
    
    # progression sources
    "progression_date_01","progression_date_15","progression_date_28",
    "progression_before_date_01","progression_before_date_15","progression_before_date_28",
    "therapy_relapse_date_01","therapy_relapse_date_15","therapy_relapse_date_28",
    
    # sv last
    "progression_sv_last_date_01","progression_sv_last_date_15","progression_sv_last_date_28"
  ))
  
  
  use_max <- function(col) str_detect(col, "(status_date|_end_date|progression_sv_last_date)")
  
  char_keep_last_cols <- intersect(names(df), c("drug_med_all", "drug_med_all_line",
                                                "treat_best_response", "surv_stat"
                                                # ggf. weitere "echte" Textfelder hier ergänzen
                                                ))
  
  
  out <- df %>%
    arrange(.data$record_id, .data$treat_line, .data[[date_order_col]]) %>%
    group_by(record_id, treat_line) %>%
    mutate(n_in_line = n()) %>%
    
    # 1) Date cols: min/max (NA-sicher) — aber NUR für die Whitelist
    mutate(
      across(all_of(date_cols), ~{
        if (n_in_line[1] <= 1) return(.x)
        colname <- cur_column()
        if (use_max(colname)) safe_max_date(.x) else safe_min_date(.x)
      })
    ) %>%
    
    # 2) character/factor: last_non_na — aber NUR für ausgewählte Spalten

      mutate(
        across(all_of(char_keep_last_cols), ~ if_else(n_in_line > 1,
                                                      as.character(last_non_na(.x)),
                                                      as.character(.x)))
        ) %>%
    
    # 3) Flags passend zu (min/max)-Datum setzen – aber NUR EINMAL pro Basis-Flag,
    #    und anhand einer Referenz-Date-Spalte (bevorzugt _15).
    {
      tmp <- .
      
      # Welche Basis-Flags existieren überhaupt?
      flag_cols <- names(tmp)[str_detect(names(tmp), "_imputed_day$")]
      
      # Heuristik: zu jedem Flag die zugehörige Date-Spalte wählen.
      # Wir nehmen bevorzugt "<base>_15", sonst "<base>" (falls vorhanden), sonst erste passende.
      for (fc in flag_cols) {
        base <- str_replace(fc, "_imputed_day$", "")          # z.B. "therapy_start_date"
        candidates <- intersect(names(tmp), c(paste0(base, "_15"), base, paste0(base, "_01"), paste0(base, "_28")))
        
        if (length(candidates) == 0) next
        dc_ref <- candidates[1]
        
        # min vs max analog zu deiner use_max-Logik
        which_ext <- if (str_detect(dc_ref, "(status_date|_end_date|progression_sv_last_date)")) "max" else "min"
        
        if (tmp$n_in_line[1] > 1) {
          tmp[[fc]] <- flag_for_extreme_date(tmp[[dc_ref]], tmp[[fc]], which = which_ext)
        }
      }
      
      tmp
    } %>%
    
    
    slice(1) %>%
    ungroup() %>%
    
    group_by(record_id) %>%
    mutate(
      max_treat_line_num = suppressWarnings(max(treat_line, na.rm = TRUE)),
      max_treat_line_num = ifelse(is.finite(max_treat_line_num), max_treat_line_num, NA_real_),
      is_last_line = !is.na(max_treat_line_num) & treat_line == max_treat_line_num
    ) %>%
    ungroup() %>%
    select(-n_in_line)
  
  out
}







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unified progression event date on line-level, for each imputation variant
# Rules (per record_id, per treat_line row):
# 1) if progression_date_<day> present -> use it
# 2) else if next line exists -> use progression_before_date_<day> from next line
# 3) else if next line exists -> use therapy_relapse_date_<day> from next line
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_progression_event_dates <- function(line_df, days = c("01","15","28")) {
  
  stopifnot(all(days %in% c("01","15","28")))
  
  exprs <- setNames(vector("list", length(days)),
                    paste0("progression_event_date_", days))
  
  for (day in days) {
    prog     <- sym(paste0("progression_date_", day))
    prog_bef <- sym(paste0("progression_before_date_", day))
    relapse  <- sym(paste0("therapy_relapse_date_", day))
    
    exprs[[paste0("progression_event_date_", day)]] <-
      expr(case_when(
        # 1) progression in this line
        !is.na(!!prog) ~ !!prog,
        
        # 2) else take from next line (progression_before preferred)
        is.na(!!prog) & !is.na(lead(treat_line)) & !is.na(lead(!!prog_bef)) ~ lead(!!prog_bef),
        
        # 3) else take relapse from next line
        is.na(!!prog) & !is.na(lead(treat_line)) & !is.na(lead(!!relapse)) ~ lead(!!relapse),
        
        TRUE ~ as.Date(NA)
      ))
  }
  
  line_df %>%
    arrange(record_id, treat_line) %>%
    group_by(record_id) %>%
    mutate(!!!exprs) %>%
    ungroup()
}







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PFS combinations on line-level using progression_event_date_<prog_day>
# and therapy_start_date_<start_day>
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_pfs_linelevel <- function(line_df, days = c("01","15","28")) {
  
  # helper to compute one combination column name + expression
  compute_one <- function(prog_day, start_day) {
    prog_evt <- sym(paste0("progression_event_date_", prog_day))
    start    <- sym(paste0("therapy_start_date_", start_day))
    status   <- sym(paste0("status_date_", start_day))
    sv_last  <- sym(paste0("progression_sv_last_date_", start_day))
    
    # name: PFS_<progDay>_<startDay>
    col_nm <- paste0("PFS_", prog_day, "_", start_day)
    
    # expression
    col_expr <- expr(case_when(
      !is.na(!!prog_evt) ~ calc_time(!!prog_evt, !!start),
      
      # optional: if last line and sv_last exists after start use it (wie bei dir)
      is_last_line & !is.na(!!sv_last) & (!!sv_last >= !!start) ~ calc_time(!!sv_last, !!start),
      
      # last line censor at status
      is_last_line ~ calc_time(!!status, !!start),
      
      TRUE ~ NA_real_
    ))
    
    list(name = col_nm, expr = col_expr)
  }
  
  # build all 9 combos
  combos <- expand.grid(prog_day = days, start_day = days, stringsAsFactors = FALSE)
  cols <- lapply(seq_len(nrow(combos)), function(i) compute_one(combos$prog_day[i], combos$start_day[i]))
  
  # inject into mutate
  mutate_args <- setNames(lapply(cols, `[[`, "expr"), vapply(cols, `[[`, "", "name"))
  
  line_df %>%
    group_by(record_id) %>%
    mutate(!!!mutate_args,
           
           # TRUE wenn irgendein progression_event_date_* existiert
           progression_event_date_imputed_day =
             if_any(
               all_of(paste0("progression_event_date_", days)),
               ~ !is.na(.x)
             )
    ) %>%
    
    mutate(
      # Event-Definition (wie bisher): event wenn progression_event_date_15 existiert,
      # plus last-line death etc. (anpassen wenn du willst)
      PFS_event = case_when(
        !is.na(progression_event_date_15) ~ 1,
        is_last_line & surv_stat == "deceased" ~ 1,
        is_last_line & surv_stat %in% c("alive", "Lost2FU") ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup()
}









calc_os_linelevel <- function(line_df, days = c("01","15","28")) {
  
  compute_one <- function(status_day, diag_day) {
    
    status <- rlang::sym(paste0("status_date_", status_day))
    diag   <- rlang::sym(paste0("diagnosis_date_", diag_day))
    
    col_nm <- paste0("OS_", status_day, "_", diag_day)
    
    col_expr <- rlang::expr(dplyr::case_when(
      is_last_line & !is.na(!!status) & !is.na(!!diag) ~ calc_time(!!status, !!diag),
      TRUE ~ NA_real_
    ))
    
    list(name = col_nm, expr = col_expr)
  }
  
  combos <- expand.grid(status_day = days, diag_day = days, stringsAsFactors = FALSE)
  cols <- lapply(seq_len(nrow(combos)), function(i) compute_one(combos$status_day[i], combos$diag_day[i]))
  
  mutate_args <- setNames(lapply(cols, `[[`, "expr"), vapply(cols, `[[`, "", "name"))
  
  line_df %>%
    group_by(record_id) %>%
    mutate(!!!mutate_args) %>%
    mutate(
      OS_event = dplyr::case_when(
        surv_stat == "deceased" ~ 1,
        surv_stat %in% c("alive", "Lost2FU") ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup()
}










calc_pfs_density <- function(df) {
  df %>%
    mutate(
      PFS_density = case_when(
        !is.na(progression_event_date_15) ~ calc_time(progression_event_date_15, therapy_start_date_density_imp),
        is_last_line & !is.na(progression_sv_last_date_15) &
          progression_sv_last_date_15 >= therapy_start_date_density_imp ~
          calc_time(progression_sv_last_date_15, therapy_start_date_density_imp),
        is_last_line & !is.na(status_date_15) ~ calc_time(status_date_15, therapy_start_date_density_imp),
        TRUE ~ NA_real_
      )
    )
}





calc_os_density <- function(df) {
  
  df %>%
    mutate(
      OS_density = case_when(
        !is.na(diagnosis_date_density_imp) & !is.na(status_date_15) ~
          calc_time(status_date_15, diagnosis_date_density_imp),
        TRUE ~ NA_real_
      )
    )
}






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Robust: parse "nk/mm/YYYY" or "dd/mm/YYYY" or ISO or Date
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_imputed_date <- function(x, day = c("01", "15", "28")) {
  day <- as.character(day)[1]
  stopifnot(day %in% c("01", "15", "28"))
  
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  
  x <- as.character(x)
  x[x %in% c("", "NA")] <- NA_character_
  
  x <- stringr::str_replace(x, "^nk/", paste0(day, "/"))
  
  iso <- stringr::str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")
  iso[is.na(iso)] <- FALSE
  
  out <- rep(as.Date(NA), length(x))
  if (any(iso)) {
    idx <- which(iso)
    out[idx] <- as.Date(x[idx])
  }
  if (any(!iso)) {
    idx <- which(!iso)
    out[idx] <- as.Date(x[idx], format = "%d/%m/%Y")
  }
  out
}









# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ONE CALL: calc_pfs_os(final3)
# - assumes *_01/_15/_28 date cols exist (as Date or parseable)
# - collapses to line-level
# - computes unified progression_event_date_01/15/28
# - computes all PFS combos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_pfs_os <- function(df) {
  
  # 0) ensure the core date columns are Date (if they are already Date, parse_imputed_date returns them unchanged)
  #    We only touch columns that exist.
  date_targets <- c(
    "diagnosis_date_density_imp", "therapy_start_date_density_imp",
    "diagnosis_date_01","diagnosis_date_15","diagnosis_date_28",
    "therapy_start_date_01","therapy_start_date_15","therapy_start_date_28",
    "therapy_end_date_01","therapy_end_date_15","therapy_end_date_28",
    "status_date_01","status_date_15","status_date_28",
    "progression_date_01","progression_date_15","progression_date_28",
    "progression_before_date_01","progression_before_date_15","progression_before_date_28",
    "therapy_relapse_date_01","therapy_relapse_date_15","therapy_relapse_date_28",
    "progression_sv_last_date_01","progression_sv_last_date_15","progression_sv_last_date_28"
  )
  
  present <- intersect(date_targets, names(df))
  
  df2 <- df %>%
    mutate(
      across(all_of(present), ~{
        nm <- cur_column()
        # infer day from suffix if present, else default to 15
        day <- str_extract(nm, "(01|15|28)$")
        if (is.na(day)) day <- "15"
        parse_imputed_date(.x, day = day)
      })
    )
  
  # ---- 1) collapse to line-level (erzeugt u.a. is_last_line) ----
  line_df <- collapse_lines_rulebased(df2, date_order_col = "therapy_start_date_15")
  
  # ---- 2) progression_event_date_01/15/28 ----
  line_df <- add_progression_event_dates(line_df, days = c("01","15","28"))
  
  # ---- 3) PFS/OS Kombis ----
  line_df <- calc_pfs_linelevel(line_df, days = c("01","15","28"))
  line_df <- calc_os_linelevel(line_df,  days = c("01","15","28"))
  
  # ---- 4) density PFS + density OS ----
  if (all(c("therapy_start_date_density_imp", "diagnosis_date_density_imp", "status_date_15") %in% names(line_df))) {
    line_df <- calc_pfs_density(line_df)
    line_df <- calc_os_density(line_df)
  }
  
  line_df
}



