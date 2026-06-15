# =============================================================================
# R/helpers.R  —  Data helpers for the woningmarkt-project report
# Programming for Economists (E_EBE1_PFE), VU Amsterdam 2025-2026
#
# Functions used by report.Rmd:
#   cbs_cached(id, fetch)        cache a CBS download to data/<id>.rds
#   try_cbs(id)                  download a verified CBS table (NULL on failure)
#   normalise_cbs(df, value)     tidy raw CBS df -> province x year x <value>
#   price_to_income(price, inc)  price-to-income ratio
#
# Verified province-level tables (RegioS = PV20..PV31, yearly Perioden):
#   83625NED  Bestaande koopwoningen; prijzen, regio      (avg price, 1995-)
#   86161NED  Inkomen van huishoudens; regio              (income, 2011-2024)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cbsodataR)
})

# ---- CBS province code <-> name --------------------------------------------
# Names match the `statnaam` of the cartomap NL GeoJSON; we nonetheless join the
# map on the province CODE (statcode) to avoid any name-encoding issues.
pv_to_name <- c(
  PV20 = "Groningen",    PV21 = "Fryslân",   PV22 = "Drenthe",
  PV23 = "Overijssel",   PV24 = "Flevoland",      PV25 = "Gelderland",
  PV26 = "Utrecht",      PV27 = "Noord-Holland",  PV28 = "Zuid-Holland",
  PV29 = "Zeeland",      PV30 = "Noord-Brabant",  PV31 = "Limburg"
)
name_to_pv <- setNames(names(pv_to_name), pv_to_name)

# ---- cbs_cached() -----------------------------------------------------------
# Run `fetch()` once and cache the result to data/<id>.rds; reuse on later knits
# so the report does not re-hit the CBS API every time.
cbs_cached <- function(id, fetch) {
  dir.create("data", showWarnings = FALSE)
  f <- file.path("data", paste0(id, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  x <- fetch()
  saveRDS(x, f)
  x
}

# ---- try_cbs() --------------------------------------------------------------
# Download a verified province-level table with the right server-side filters.
# Returns NULL on any error so the report can fall back gracefully.
try_cbs <- function(id) {
  tryCatch(
    cbs_cached(id, function() {
      options(timeout = 300)
      if (id == "86161NED") {
        cbs_get_data(
          id,
          Populatie               = "1050010",  # particuliere huishoudens incl. studenten
          KenmerkenVanHuishoudens = "1050010",  # all households (total)
          RegioS                  = has_substring("PV"),
          Perioden                = has_substring("JJ00")
        )
      } else {
        cbs_get_data(
          id,
          RegioS   = has_substring("PV"),
          Perioden = has_substring("JJ00")
        )
      }
    }),
    error = function(e) {
      message("try_cbs(", id, ") failed: ", conditionMessage(e))
      NULL
    }
  )
}

# ---- normalise_cbs() --------------------------------------------------------
# Turn a raw CBS data frame into a tidy province x year table with one value
# column named `value_name`. Detects the source column and unit automatically:
#   - house price  (GemiddeldeVerkoopprijs_1)   unit "euro"      -> scale 1
#   - median income (MediaanBesteedbaarInkomen_6) unit "1000 eur" -> scale 1000
normalise_cbs <- function(df, value_name) {
  df <- df %>% mutate(across(where(is.character), str_trim)) %>%
    filter(str_starts(RegioS, "PV"))

  if ("GemiddeldeVerkoopprijs_1" %in% names(df)) {
    raw_val <- df[["GemiddeldeVerkoopprijs_1"]]; scale <- 1
  } else if ("MediaanBesteedbaarInkomen_6" %in% names(df)) {
    raw_val <- df[["MediaanBesteedbaarInkomen_6"]]; scale <- 1000
  } else {
    stop("normalise_cbs(): no known value column found in this table.")
  }

  tibble(
    province = unname(pv_to_name[df$RegioS]),
    year     = as.integer(str_sub(df$Perioden, 1, 4)),
    !!value_name := as.numeric(raw_val) * scale
  ) %>%
    filter(!is.na(province), !is.na(year)) %>%
    arrange(province, year)
}

# ---- price_to_income() ------------------------------------------------------
price_to_income <- function(price, income) price / income
