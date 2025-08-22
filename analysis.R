# =========================
# analysis.R  (service-account, headless)
# =========================

# ---- Packages ----
suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(scales); library(gt); library(broom)
  library(readr); library(stringr); library(jsonlite)
})

# ---- Auth (Service Account) ----
SA_PATH <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = "service-account.json")
if (!file.exists(SA_PATH)) {
  stop(paste0(
    "Service account key not found at: ", SA_PATH,
    "\nSet GOOGLE_APPLICATION_CREDENTIALS in .Renviron or place service-account.json in project root."
  ))
}

.sa_json <- tryCatch(jsonlite::fromJSON(SA_PATH), error = function(e) NULL)
if (is.null(.sa_json) || is.null(.sa_json$type) || .sa_json$type != "service_account") {
  stop("The JSON at GOOGLE_APPLICATION_CREDENTIALS is not a service-account key. Create a SA key (Keys → Add key → JSON) and try again.")
}

gs4_auth(
  path   = SA_PATH,
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
)

# ---- Config ----
SHEET_ID <- Sys.getenv("GOOGLE_SHEET_ID")
if (SHEET_ID == "") stop("GOOGLE_SHEET_ID not set in .Renviron (the long ID from your Sheet URL).")

default_tabs <- c(
  "Annual_DGS10",
  "Annual_DPRIME",
  "Annual_PCETRIM12M159SFRBDAL",
  "Annual_UMCSENT",
  "Annual_PAYEMS",
  "Annual_UNRATE"
)
tabs_env <- Sys.getenv("FRED_TABS")
TABS <- if (tabs_env != "") strsplit(tabs_env, ",")[[1]] else default_tabs
TABS <- trimws(TABS)

# Optional: exclude the current (incomplete) year by setting EXCLUDE_CURRENT_YEAR=true in .Renviron
EXCLUDE_CURRENT_YEAR <- tolower(Sys.getenv("EXCLUDE_CURRENT_YEAR", unset = "false")) %in% c("1","true","yes","y")

# ---- Helpers ----
pretty_series <- function(x) {
  x |>
    str_remove("^Annual_") |>
    str_replace_all("_", " ")
}

read_tab <- function(tab) {
  out <- googlesheets4::read_sheet(
    SHEET_ID, sheet = tab, range = "A1:B", col_types = "cn"
  )
  names(out)[1:2] <- c("Year", "Value")
  out |>
    mutate(
      Year  = suppressWarnings(as.integer(Year)),
      # Strip anything that's not digit, dot, or minus before numeric cast
      Value = suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", as.character(Value)))),
      Series = tab
    ) |>
    filter(!is.na(Year))  # keep rows even if Value is NA; models handle NAs via complete.cases
}


# ---- Read all tabs ----
df <- dplyr::bind_rows(lapply(TABS, read_tab))

# Optionally drop current year if incomplete
if (EXCLUDE_CURRENT_YEAR) {
  current_year <- lubridate::year(Sys.Date())
  df <- df |> filter(Year < current_year)
}

# Add pretty name for legends/headers
df$SeriesPretty <- pretty_series(df$Series)

# ---- Wide form for correlations / regressions (safe) ----
# Keep only Year, Series (for names), and Value before widening.
# This avoids carrying text columns (e.g., SeriesPretty) into the wide frame.
wide <- df |>
  transmute(
    Year,
    Series = str_remove(Series, "^Annual_"),
    Value
  ) |>
  tidyr::pivot_wider(names_from = Series, values_from = Value) |>
  arrange(Year)

# Numeric series columns (exclude Year and any non-numeric)
num_cols <- setdiff(names(wide), "Year")
num_cols <- num_cols[vapply(wide[num_cols], is.numeric, logical(1))]

# ---- Example analyses ----

# 1) Coverage table
coverage_tbl <- df |>
  group_by(SeriesPretty) |>
  summarise(
    min_year = min(Year, na.rm = TRUE),
    max_year = max(Year, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  arrange(SeriesPretty)

# 2) Correlation matrix (pairwise complete observations)
if (length(num_cols) >= 2) {
  cors <- round(cor(wide[num_cols], use = "pairwise.complete.obs"), 2)
} else {
  cors <- matrix(NA_real_, nrow = length(num_cols), ncol = length(num_cols),
                 dimnames = list(num_cols, num_cols))
}

# 3) OLS: UNRATE ~ DGS10 + DPRIME (guarded)
reg_formula <- as.formula("UNRATE ~ DGS10 + DPRIME")
needed_cols <- c("UNRATE", "DGS10", "DPRIME")

if (all(needed_cols %in% names(wide))) {
  cc <- stats::complete.cases(wide[needed_cols])
  if (sum(cc) >= 3) {
    model <- lm(reg_formula, data = wide[cc, , drop = FALSE])
    model_tidy  <- broom::tidy(model)
    model_glance <- broom::glance(model)
  } else {
    model_tidy  <- tibble::tibble(term = character(), estimate = numeric(),
                                  std.error = numeric(), statistic = numeric(), p.value = numeric())
    model_glance <- tibble::tibble()
  }
} else {
  model_tidy  <- tibble::tibble(term = character(), estimate = numeric(),
                                std.error = numeric(), statistic = numeric(), p.value = numeric())
  model_glance <- tibble::tibble()
}

# 4) Standardized (z-score) series for shape comparison (safe)
wide_std <- wide
if (length(num_cols) > 0) {
  wide_std[num_cols] <- scale(wide[num_cols])
}

df_std <- wide_std |>
  tidyr::pivot_longer(
    cols = all_of(num_cols),   # only numeric series columns
    names_to = "Series",
    values_to = "Z"
  )
df_std$SeriesPretty <- df_std$Series |> stringr::str_replace_all("_", " ")

# 5) Single combined long-run plot (levels)
p_all <- ggplot(df, aes(Year, Value, color = SeriesPretty)) +
  geom_line(linewidth = 0.9) +
  labs(title = "Annualized Macro Series (FRED)",
       x = NULL, y = NULL, color = "Series") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 6) Standardized plot
p_std <- ggplot(df_std, aes(Year, Z, color = SeriesPretty)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Standardized (z-score) Series",
       x = NULL, y = "Standard Deviations", color = "Series") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 7) Simple lead/lag exploratory (UNRATE vs DGS10 lagged 1y), guarded
if (all(c("UNRATE","DGS10") %in% names(wide))) {
  wide2 <- wide |>
    mutate(DGS10_lag1 = dplyr::lag(DGS10, 1))
  cc2 <- stats::complete.cases(wide2[, c("UNRATE","DGS10_lag1")])
  if (sum(cc2) >= 3) {
    lag_model <- lm(UNRATE ~ DGS10_lag1, data = wide2[cc2, , drop = FALSE])
    lag_tidy <- broom::tidy(lag_model)
  } else {
    lag_tidy <- tibble::tibble()
  }
} else {
  lag_tidy <- tibble::tibble()
}

# ---- Export objects for report.qmd ----
analysis_objects <- list(
  df = df,
  coverage_tbl = coverage_tbl,
  cors = cors,
  model_tidy = model_tidy,
  model_glance = model_glance,
  p_all = p_all,
  p_std = p_std,
  lag_tidy = lag_tidy
)

analysis_objects
