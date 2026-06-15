# Helper functions for the housing-inequality-nl report.
# Sourced by report.Rmd and run_all.R.

suppressMessages({
  library(cbsodataR)
  library(tidyverse)
})

#' Download a CBS table, caching the result to disk (data/<id>.rds).
cbs_cached <- function(table_id, cache_dir = "data") {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, paste0(table_id, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))
  dat <- cbs_get_data(table_id)
  saveRDS(dat, cache_file)
  dat
}

#' Safe wrapper: returns NULL instead of erroring if the API is unreachable.
try_cbs <- function(table_id) {
  tryCatch(cbs_cached(table_id), error = function(e) NULL)
}

#' Price-to-income ratio.
price_to_income <- function(avg_price, median_income) avg_price / median_income

#' Normalise a raw CBS table into a tidy province-year tibble.
#'
#' CBS tables use long Dutch column names, store the region in a coded
#' "RegioS"/"Regio" column and the period in "Perioden". This squishes names,
#' parses the year, and renames the chosen value column. ADAPT the value-column
#' selector to the actual table you pulled before final submission.
#'
#' @param raw    data frame returned by cbs_get_data().
#' @param value  desired name for the numeric value column, e.g. "avg_price".
normalise_cbs <- function(raw, value) {
  df <- as_tibble(raw)
  names(df) <- stringr::str_squish(names(df))

  period_col <- intersect(c("Perioden", "Periods"), names(df))[1]
  region_col <- intersect(c("RegioS", "Regio", "Regions"), names(df))[1]
  # First numeric column is taken as the measure; adjust if your table differs.
  num_cols   <- names(df)[sapply(df, is.numeric)]
  value_col  <- num_cols[1]

  out <- df |>
    mutate(year = readr::parse_number(as.character(.data[[period_col]]))) |>
    filter(!is.na(year))
  if (!is.na(region_col)) {
    out <- out |> mutate(province = stringr::str_squish(as.character(.data[[region_col]])))
  } else {
    out <- out |> mutate(province = "Netherlands")
  }
  out |>
    transmute(province, year, !!value := .data[[value_col]]) |>
    filter(!is.na(.data[[value]]))
}
