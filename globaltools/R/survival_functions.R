





# ______________________________________________________
#
## Survival Analysis ----
# ______________________________________________________





#' Calculate time difference between two dates
#'
#' @description
#' Calculate difference in months between two dates
#'
#' @param date1   Date. First date (usually later date).
#' @param date2   Date. Second date (earlier date).
#' @param unit    String. One of "days", "months", or "years". (Default "months").
#'
#' @description   In case for unit = "years", it uses the average year value of 365.25,
#'                for unit = "months" it uses the average year value of 365.25 divided by 12
#'
#' @return        Numeric vector with the same length as date1/date2. Difference in months.
#' @export
calc_time <- function(date1, date2, unit = "months") {

  stopifnot(inherits(date1, c("Date", "POSIXct", "POSIXt")),
            inherits(date2, c("Date", "POSIXct", "POSIXt")))

  as.numeric(difftime(date1, date2, units = "days"))

  diff_days <- as.numeric(difftime(date1, date2, units = "days"))
  switch(tolower(unit),
         days   = diff_days,
         months = diff_days / (365.25 / 12),
         years  = diff_days / 365.25,
         stop("`unit` must be 'days', 'months', or 'years'."))
}













#' (Helper) Transform the x-achsis of KM curves
#'
#' @description
#' Multiplication factor to transform the x-axis of KM curves
#' define x-Scale
#'
#' @param xscale   Character of the form "d_m" (= days to months) or "m_y"
#'               (= months to years). See details.
#'
#' @details
#' Allowed pairs (`from`_`to`): d_m, d_y, m_d, m_y, y_d, y_m.
#'
#' @return         Numeric factor
#' @export
get_xscale <- function(xscale = NULL){


  if (is.null(xscale) || xscale %in% c("", "none")) {
    return (1)
  }


  pair <- tolower(xscale)
  if (!grepl("^[dmy]_[dmy]$", pair))
    stop("❗'xscale' must have the pattern: 'd_m', 'm_y', 'm_m', ...")



  # calculations for the right scale
  switch(pair,
         d_m = 12/365.25,
         d_y = 1/365.25,
         m_d = 365.25/12,
         m_y = 1/12,
         y_d = 365.25,
         y_m = 12,
         d_d = 1,
         m_m = 1,
         y_y = 1,
         stop("❗Unknown combination in 'get_xscale()'.")
  )

}
# get_xscale <- function(xscale = "m_y"){
#   # calculations for the right scale
#   xtrans <- switch(xscale,
#                    d_m = 12/365.25,
#                    d_y = 1/365.25,
#                   m_d = 365.25/12,
#                    m_y = 1/12,
#                    y_d = 365.25,
#                    y_m = 12,
#                    1
#   )
#   return(xtrans)
# }



# (Helper) Transform the x-achsis of KM curves
#
# @description
# Multiplication factor to transform the x-axis of KM curves
# define x-Scale
#
# @param xscale   Character of the form "d_m" (= days to months) or "m_y"
#                 (= months to years) or "m_m" (= remains months). See details.
#
# @details
# Allowed pairs (`from_to`): "d_m", "d_y", "m_d", "m_y", "y_d", "y_m", "d_d", "m_m", "y_y",
# or "" / NULL (= no conversion).
#
# @return         Numeric factor (1 = no conversion)
# @export
# get_xscale <- function(xscale = "m_y"){
#   # calculations for the right scale
#   xtrans <- switch(xscale,
#                    d_m = 12/365.25,
#                    d_y = 1/365.25,
#                    m_d = 365.25/12,
#                    m_y = 1/12,
#                    y_d = 365.25,
#                    y_m = 12,
#                    1
#   )
#   return(xtrans)
# }














#' (Helper) Median table
#'
#' @description
#' Median Survival (with 95% CI) as a one‑row `tibble`
#'
#' @param fit               `survfit` object.
#' @param time_unit         String; For the description in the table; e.g. "Years", "Months" or "Days".
#' @param xscale            String; Transformation specification passed to `get_xscale()`.
#'                          Character of the form "d_m" (= days to months) or "m_y" (= months to years)
#'                          or "m_m" (= remains months).
#'                          Allowed pairs (`from_to`): d_m, d_y, m_d, m_y, y_d, y_m, d_d, m_m, y_y.
#'
#' @returns                 A tibble with a single column that is ready to be printed or annotated.
#' @import tidyverse
#' @export
get_median_table <- function(fit, time_unit = "Years", xscale = "m_y") {

  xtrans <- get_xscale(xscale)

  # summary object
  tab <- summary(fit)$table

  # Get values
  records <- tab["records"]
  events  <- tab["events"]
  median  <- round(tab["median"] * xtrans, 2)
  lcl     <- round(tab["0.95LCL"] * xtrans, 2)
  ucl     <- round(tab["0.95UCL"] * xtrans, 2)

  # Output
  data.frame(
    Median_survival = paste0(median, "  [", lcl, "–", ucl, "]"),
    check.names = FALSE  # prevents conversion of column name
  ) %>%
    setNames(paste0("Median\n Survival, ", time_unit,"\n[95%-CI]"))
}













#' (Helper) Follow-up times table
#'
#' @description
#' Survival probabilities at pre‑defined follow‑up times
#' Follow up Survival probability (in %)
#'
#' @param fit              `survfit` object.
#' @param times            Numeric vector of *original* time scale (e.g. months).
#' @param years            Default; Numeric vector of time scale in years.
#' @param time_unit        Label for column header.
#' @param xscale           Transformation specification passed to `get_xscale()`
#'
#' @returns                A table with two columns (follow up survival probabilities (+ confidence intervals)
#'                         at 1, 2, and 5 years.)
#' @import tibble
#' @export
get_surv_times <- function(fit,
                           times = NULL,
                           years = c(1, 2, 5),
                           time_unit = "Years",
                           xscale = "m_y") {


  xtrans <- get_xscale(xscale)
  origin <- substr(ifelse(is.null(xscale), "y_y", xscale), 1, 1)   # first letter encodes the original unit

  if (is.null(times)) {

    per_year <- switch(origin,
                       d = 365.25,  # days per year
                       m = 12,      # months per year
                       y = 1,       # already in years
                       stop("Cannot determine origin unit from `xscale`."))
    times <- years * per_year
  }


  # summary object
  surv_summary <- summary(fit, times = times)

  time_months <- round(surv_summary$time, 2)
  surv_percent <- round(surv_summary$surv * 100, 2)
  ci_lower <- round(surv_summary$lower * 100, 2)
  ci_upper <- round(surv_summary$upper * 100, 2)


  # table
  tibble::tibble(
    FU_in_Years = round(time_months * xtrans, 2),
    Survival =  paste0(surv_percent, "% [", ci_lower, "–", ci_upper, "]")) %>%

    rename_with(~ c(paste0("Follow up\nin ", time_unit),
                    paste0("Survival probability (%),\n[95%-CI]")))
}














#' High‑level Kaplan–Meier plot wrapper
#'
#' @description
#' High‑level Kaplan–Meier plot wrapper with add-ons like median & follow-up times table,
#' table positions, customazation of the scale, title, title-size and more.
#'
#' @param data                     Optional `data.frame` containing all variables. used for the risk table. If omitted the
#'                                 risk table is suppressed (because `ggsurvplot()` requires `data` for that panel).
#' @param fit                      `survfit` object. Stratification present in `fit` will be respected automatically.
#' @param time_unit                String. Original time unit of the model (informational, used for axis label).
#' @param xscale                   See `get_xscale()`. Default = "m_y" (= from months to years).
#'                                 Allowed pairs (`from_to`): "d_m", "d_y", "m_d", "m_y", "y_d", "y_m", "d_d", "m_m", "y_y",
#'                                 or "" / NULL (= no conversion).
#' @param x_end                    Numeric. Optional. Gets calculated by default from `survfit` object.
#'                                 Defines the x-scale end point. In *original* time unit.
#' @param scale_break              Numeric. Break points of the x-scale. Default = 24 months. In *original* time unit.
#' @param title                    String. Plot title
#' @param title_size               Numeric. Font size of the title. Defaule = 2.7.
#' @param show_tbls                Logical. Adds summary tables inside the Kaplan-Meier-plot.
#' @param tbl1_pos                 Numeric Vector length 2 (x, y) for `get_surv_times()`.
#' @param tbl2_pos                 Numeric Vector length 2 (x, y) for `get_median_table()`.
#' @param followup_times           Numeric vector of individual length for `get_surv_times()`. Defaut = NULL.
#' @param risk_table_size          Numeric. Font size of the risk table.
#' @param ...                      Further arguments forwarded to `ggsurvplot()`
#'                                 (e.g. `conf.int = TRUE`, `pval = TRUE`, `linetype = "strata"`).
#'
#' @returns                        A `ggsurvplot` object list.
#' @export
#' @import survminer
#' @import tidyverse
#' @import survival
#' @import ggpp
#' @import ggpubr
#' @examples
#' library(survival)
#' surv <- Surv(time = lung$time, event = lung$status)
#' fit <- survfit(surv ~ 1, data = lung)
#' create_surv_plot(lung, fit, xscale = "d_m", x_end = 1300, scale_break = 60.88, time_unit = "Months")
create_surv_plot <- function(data = NULL,
                             fit,
                             time_unit = "Years",
                             xscale = "m_y",
                             scale_break = 24,
                             x_end = NULL,
                             title = "Kaplan-Meier curve",
                             title_size = 11,
                             show_tbls = TRUE,
                             tbl1_pos = c(0, 0),
                             tbl2_pos = c(0.8, 0),
                             followup_times = NULL,
                             risk_table_size = 2.7,
                             palette = "black",
                             ...) {

  stopifnot(inherits(fit, "survfit"))

  if (is.null(x_end)) {
    x_end <- dplyr::last(fit$time)
  }


  reldata_x <- function(rel) rel * x_end
  reldata_y <- function(rel) rel




  # Axis transformation
  xtrans <- get_xscale(xscale)
  breaks_x <- seq(0, x_end, by = scale_break)
  breaks_y <- c(0, 25, 50, 75, 100)


  risk_tbl <- if (!is.null(data)) "nrisk_cumevents" else FALSE
  plot_obj <- survminer::ggsurvplot(fit,
                                    data = data,
                                    risk.table = risk_tbl,
                                    xlab = paste0("Time (", time_unit, ")"),
                                    ylab = "Survival probability (in %)",
                                    title = title,
                                    risk.table.y.text = TRUE,
                                    break.time.by = scale_break,
                                    risk.table.fontsize = risk_table_size,
                                    conf.int = TRUE,
                                    censor.shape = "|",
                                    surv.median.line = "hv",
                                    ggtheme = ggpubr::theme_pubr(),
                                    palette = palette,
                                    ...
  )



  # Scale Adjustments
  plot_obj$plot <- plot_obj$plot +
    ggplot2::scale_x_continuous(breaks = breaks_x,
                                labels = round(breaks_x * xtrans, 2), expand = c(0.05, 0.1)) +
    ggplot2::scale_y_continuous(labels = breaks_y) +
    ggplot2::theme(axis.text.x = element_text(size = title_size),
                   axis.title.x = element_text(size = title_size, face = "bold"),
                   axis.text.y = element_text(size = title_size, face = "bold"),
                   axis.title.y = element_text(size = title_size, face = "bold"),
                   title = element_text(size = title_size, face = "bold")
    ) + ggplot2::theme(legend.position = "none")


  if (show_tbls){
    # get the Median time
    tbl1 <- get_median_table(fit, time_unit = time_unit, xscale = xscale)
    tbl1 <- as.data.frame(tbl1)
    # get the Followup Survival Probability in %
    tbl2 <- get_surv_times(fit, time_unit = time_unit, times = followup_times, xscale = xscale)
    tbl2 <- as.data.frame(tbl2)



    plot_obj$plot <- plot_obj$plot +
      ggpp::annotate(geom = "table", x = reldata_x(tbl1_pos[1]),  y = reldata_y(tbl1_pos[2]), label = tbl1) +
      ggpp::annotate(geom = "table", x = reldata_x(tbl2_pos[1]),  y = reldata_y(tbl2_pos[2]), label = tbl2)
  }

  if (!isFALSE(risk_tbl)) {

    plot_obj$table <- plot_obj$table +
      ggplot2::scale_x_continuous(breaks = breaks_x, labels = round(breaks_x * xtrans, 2), expand = c(0.05, 0.1)) +
      ggplot2::xlab("") +
      ggplot2::theme(axis.text.x = element_text(size = title_size),
                     axis.title.y = element_text(size = title_size),
                     title = element_text(size = title_size, face = "bold"))
  }

  return(plot_obj)
}














