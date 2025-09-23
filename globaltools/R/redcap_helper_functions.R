




# ______________________________________________________
#
## ----------- Registry RedCap Functions --------------
# ______________________________________________________





#' Parse a RedCap mapping string to a named vector
#'
#' @param mapping_str   Character string. RedCap mapping string (e.g. "1, Yes | 2, No").
#'
#' @return              Named character vector: values as names, labels as values.
#'
#' @export
parse_mapping <- function(mapping_str) {
  # setNames(
  #   sapply(strsplit(strsplit(mapping_str, "\\|")[[1]], ", "), `[`, 2),
  #   sapply(strsplit(strsplit(mapping_str, "\\|")[[1]], ", "), `[`, 1)
  # )
  str_split(mapping_str, "\\s*\\|\\s*") %>%
    unlist() %>%
    str_match("^\\s*(.+?),\\s*(.+?)\\s*$") %>%
    { setNames(.[, 3], .[, 2]) }
}






#' Transform raw values to the labels
#'
#' @description
#' Map coded variable values to their labels using the RedCap data dictionary
#' Generalized function to map variable values to their labels
#'
#' @param df        Data frame. The data you want to modify.
#' @param var       Unquoted variable name in df to convert.
#'
#' @return          Data frame with the mapped variable replaced by its label.
#' @export
values_to_labels <- function(df, var) {
  var <- enquo(var) # Capture the variable using tidy evaluation
  var_name <- quo_name(var)
  # Requires data dictionary to be loaded as dd
  mapping_str <- dd %>%
    filter(field_name == var_name) %>% # Use quo_name() to get the name of the variable
    pull(choices_calculations_or_slider_labels) %>%
    .[1]
  
  if (length(mapping_str) == 0 || is.na(mapping_str)) {
    stop(paste("Keine Mapping-Information für", var_name, "gefunden."))
  }
  
  mapping <- parse_mapping(mapping_str)
  mapping <- c(mapping, "nd" = "Not done")
  
  df[[var_name]] <- mapping[as.character(df[[var_name]])] # Use tidy evaluation to modify the variable in the df
  
  return(df)
}












#' Create Raw Date Columns with Missing Day/Month Filled as "nk"
#'
#'
#' @description
#' This function generates a raw date column based on the provided conditions,
#' by combining month and year columns into a single date. If the date column
#' is missing, it inserts a placeholder "nk" with the respective month and year.
#'
#'
#' @param df                 Data frame. The DataFrame in which the date column will be created.
#' @param date_column        Character. The name of the date column to be handled (e.g., `“start_date”`).
#' @param month_column       Character. The name of the month column (e.g., `“start_date_month”`).
#' @param year_column        Character. The name of the year column (e.g., `“start_date_year”`).
#' @param flag_column        Character. The name of the flag column that indicates whether a date should be set (e.g., `“treatment_y_n”`).
#' @param condition          A logical condition (or vector) to determine when to create or update the raw date column.
#'                           This condition can be any logical expression, such as `df$surv_stat == "alive"/1`.
#'
#' @returns                  The input data frame with the added raw date column containing missing days/months as `“nk”`.
#'                           The new column will be named: `<date_column>_raw`.
#' @export
#'
#' @examples
#' # Example: Create raw status date column when surv_stat is "alive"
#' df2 <- df %>%
#' create_raw_date_column(
#'   date_column  = "status_date",
#'   month_column = "last_contact_date_month",
#'   year_column  = "last_contact_date_year",
#'   condition    = df$surv_stat == "alive")
#'
#' # Example: Create raw therapy start date column when treat_y_n is 1
#' df <- create_raw_date_column(
#'   df2 = df,
#'   date_column = "therapy_start_date",
#'   month_column = "therapy_start_date_month",
#'   year_column = "therapy_start_date_year",
#'   condition = df$treat_y_n == 1

create_raw_date_column <- function(df, date_column, month_column, year_column, condition) {
  
  df <- df %>%
    
    mutate(
      !!paste0(date_column, "_raw") := case_when(condition & !is.na(!!sym(month_column)) & !is.na(!!sym(year_column))
                                                 & is.na(!!sym(date_column))  ~ paste("nk", !!sym(month_column), !!sym(year_column), sep = "/"),
                                                 
                                                 condition & !is.na(!!sym(date_column))  ~ !!sym(date_column),
                                                 
                                                 TRUE  ~ NA_character_
      )
    )
  
  
  return(df)
}













#' Impute Missing Dates by Replacing "nk" with Standard Day Values

#' @description
#' This function imputes date values by replacing `“nk”` with a specified default day (e.g., `01`, `15`, `28`).
#' The imputed date column is then stored in the DataFrame.
#'
#' @param df                Data frame. Data Frame where imputations should take place.
#' @param raw_date_column   Character. Name of the "raw" date-column, which has the "nk" values. (e.g. `"start_date_raw"`).
#' @param day               Character. Day value, which should be imputed (default is `"15"`).
#'
#' @return                 Data frame with imputed day-value column.
#' @export

impute_missing_dates <- function(df, raw_date_column, day = "15") {
  df <- df %>%
    mutate(!!paste0(raw_date_column, "_imputed_", day) := as.Date(
      stringr::str_replace(!!sym(raw_date_column), "nk", day), "%d/%m/%Y"))
  
  return(df)
}
















#' (Helper) Get or set variable label as an attribute.
#'
#' @description
#' Get or set a variable label as an attribute
#' Define the var_label function for setting and getting labels
#'
#' @param x       Vector. The variable to label.
#' @param value   Character (optional). If supplied, sets the label; if omitted, returns label.
#'
#' @return        Label attribute (if value is NULL), or labelled vector (if value is set).
#' @export
var_label <- function(x, value = NULL) {
  if (is.null(value)) {
    attr(x, "label")
  } else {
    attr(x, "label") <- value
    return(x)
  }
}




#' Add readable labels to columns
#'
#' @description
#' Set cleaned labels on all variables in a data frame, using a data dictionary
#' Define function for piping and combine label setting and cleaning
#'
#' @param df     Data frame. The data to label.
#' @param dict   Data frame. Data dictionary with `field_name` and `field_label`.
#'
#' @return       Data frame with labelled variables.
#' @export
add_labels <- function(df, dict) {
  for (var_name in names(df)) {
    if (var_name %in% dict$field_name) {
      # Extract and clean the label
      var_label_new <- clean_html_css(dict$field_label[dict$field_name == var_name])
      # Set the cleaned label to the variable and update the dataframe
      df[[var_name]] <- var_label(df[[var_name]], var_label_new)
    }
  }
  return(df)
}







#' (Helper) Remove html and css tags
#'
#' @description
#' Remove HTML and CSS tags from a string from the label
#'
#' @param label   Character string. Input label with possible HTML tags.
#'
#' @return        Cleaned character string.
#' @export
clean_html_css <- function(label) {
  # Remove HTML tags
  label <- gsub("<[^>]+>", "", label)
  # Optionally, you can add more cleaning rules here if needed
  return(label)
}





#' Clean Data dictionary variable names
#'
#' @description
#' Clean data frame column names (to lowercase, underscores, remove special chars)
#'
#' @param df      Data frame whose column names should be cleaned.
#'
#' @return        Data frame with cleaned names.
#' @export
clean_names <- function(df) {
  names(df) <- names(df) %>%
    tolower() %>% # Convert to lowercase
    gsub(" ", "_", .) %>% # Replace spaces with underscores
    gsub("[^[:alnum:]_]", "", .) # Remove special characters, keeping only alphanumeric and underscores
  return(df)
}






#' Convert to numeric with dot
#'
#' @description
#' Convert numbers with comma as decimal to numeric
#'
#' @param x    Character vector or factor to convert (e.g. "1,23").
#'
#' @return     Numeric vector.
#' @export
convert_to_numeric <- function(x){
  x <- gsub(",", ".", x)
  as.numeric(x)
}






#' Capitalize first letter; string separated with comma
#'
#' @description
#' Capitalize first letter of each word in a comma-separated string
#' easier reading
#'
#' @param x    Character vector with words separated by commas.
#'
#' @return     Character vector with capitalized words.
#' @export
capitalize_words <- function(x) {
  sapply(x, function(row) {
    paste(sapply(strsplit(row, ",\\s*")[[1]], function(word) {
      ifelse(grepl("^[A-Z]", word), word, paste0(toupper(substring(word, 1, 1)), substring(word, 2)))
    }), collapse = ", ")
  })
}





#' Reform month & year columns, considering partial dates
#'
#' @description
#' Reformat dates using month and year columns, or format full dates
#' for reformatting, considering partial dates
#'
#' @param df           Data frame containing the date columns.
#' @param base_names   Character vector. Base names (without _month/_year).
#'
#' @return             Data frame with reformatted date columns.
#' @export
reformat_dates <- function(df, base_names) {
  for (base_name in base_names) {
    month_var <- paste0(base_name, "_month")
    year_var <- paste0(base_name, "_year")
    date_var <- sym(base_name) # Convert to symbol
    
    df <- df %>%
      mutate(
        !!date_var := ifelse(
          !is.na(.data[[month_var]]) & !is.na(.data[[year_var]]) & is.na(.data[[base_name]]),
          paste("nk", .data[[month_var]], .data[[year_var]], sep = "/"),
          format(as.Date(.data[[base_name]], "%Y-%m-%d"), "%d/%m/%Y")
        )
      )
  }
  return(df)
}





#' Sort unique columns
#'
#' @description
#' Sort and return unique values in a comma-separated character column
#' filter unique therapies in column
#'
#' @param x   Character vector with comma-separated values.
#'
#' @return    Character vector with sorted, unique, comma-separated values.
#' @export
sort_unique_values <- function(x) {
  sapply(strsplit(x, ",\\s*"), function(y) paste(sort(unique(trimws(y))), collapse = ", "))
}






#' Paste elements together, omitting NAs
#'
#' @description
#' Paste elements together, omitting NAs
#' combine without NA
#'
#' @param x   Vector. Elements to combine.
#'
#' @return    Single character string, elements separated by ", ".
#' @export
paste_without_na <- function(x) {
  return(paste(x[!is.na(x)], collapse = ", "))
}

