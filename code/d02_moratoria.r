# ==========================================================================
# Purpose: Process eviction moratoria data — notice and filing moratoria only
#
# Script:
# ----------
# 1. PACKAGES & OPTIONS
# 2. LOAD DATA
# 3. BUILD DAILY TREATMENT INDICATORS
# 4. COMBINE NOTICE AND FILING INTO ONE TREATMENT VARIABLE
# 5. COLLAPSE TO MONTHLY
# 6. SAVE
#
# Data source: Eviction Moratoria & Housing Policy: Federal, State,
#              Commonwealth, and Territory dataset (ICPSR 157201 V2).
#              Available at https://doi.org/10.3886/E157201V2
#
# Key design decision — which moratoria types to use:
# ----------
# The moratoria database tracks five stages of the eviction process:
#   S1 = notice of eviction (prevents serving notice)
#   S2 = filing of eviction claim (prevents court filing)
#   S3 = hearing
#   S4 = judgment / writ of issuance
#   S5 = enforcement / writ execution
#
# Since the outcome of interest is eviction *filings*, only S1 and S2
# are valid treatments. Moratoria at S3-S5 occur *after* a case is filed
# and therefore cannot reduce filings — including them would introduce
# post-treatment contamination and violate the research design.
# We combine S1 and S2: a state-month is treated if EITHER type was active.
# ==========================================================================


# ==========================================================================
# 1. PACKAGES & OPTIONS
# ==========================================================================

rm(list = ls())

#
# Load libraries
# --------------------------------------------------------------------------
librarian::shelf(tidyverse, readxl, janitor, tidycensus, qs)

#
# Options
# --------------------------------------------------------------------------
options(width = max(80L, as.integer(Sys.getenv("COLUMNS")), na.rm = TRUE), setWidthOnResize = TRUE, scipen = 10)


# ==========================================================================
# 2. LOAD DATA
# ==========================================================================

#
# Raw moratoria dataset
# --------------------------------------------------------------------------
moratoria_raw <-
  read_excel(
    "data/moratoria/2023.02.01 Moratoria + Supportive Measures Datasets.xlsx",
    sheet     = "Moratoria Dataset",
    col_types = c("text", "text", rep("date", 36))
  ) %>%
  clean_names() %>%
  glimpse()

#
# FIPS reference table (for filtering to 50 states + DC)
# --------------------------------------------------------------------------
fips_data <-
  fips_codes %>%
  select(state_abbr = state, state_code, state_name) %>%
  distinct(state_code, .keep_all = TRUE) %>%
  mutate(fips_num = as.numeric(state_code)) %>%
  filter(fips_num <= 56) %>%  # 50 states + DC only
  glimpse()


# ==========================================================================
# 3. BUILD DAILY TREATMENT INDICATORS
# ==========================================================================
# For each moratorium type, we expand each row to a daily time series
# spanning the full analysis period, then flag each day as treated or not.
# The daily resolution handles mid-month moratorium starts correctly —
# when later collapsed to months, any month with at least one treated day
# is coded as a treatment month.
#
# Analysis period: March 2020 (first state moratoria) through June 2022
# (all remaining state moratoria had ended by this point).
# --------------------------------------------------------------------------

PERIOD_START <- as.Date("2020-01-01")
PERIOD_END   <- as.Date("2022-12-31")

#
# Helper function: expand a moratorium dataset to daily flags
# ----------
# Inputs:
#   df           - data frame with 'state', 'start_date', 'end_date' columns
#   flag_col     - name for the binary treatment column to create
# ----------
make_daily_flags <- function(df, flag_col) {
  df %>%
    drop_na(start_date) %>%
    # generate every day in the analysis period as a list column, then unnest
    mutate(date = list(seq(PERIOD_START, PERIOD_END, by = "day"))) %>%
    unnest(cols = c(date)) %>%
    group_by(state) %>%
    mutate(
      !!flag_col := as.integer(between(date, start_date, end_date))
    ) %>%
    ungroup() %>%
    # where multiple rows per state overlap, keep the max (any coverage = 1)
    group_by(state, date) %>%
    arrange(desc(.data[[flag_col]])) %>%
    distinct(date, .keep_all = TRUE) %>%
    ungroup()
}


#
# S1: Notice moratoria
# --------------------------------------------------------------------------
# Prevents a landlord from serving a formal notice to quit — the first
# step in the eviction process before any court filing occurs.
notice_moratoria <-
  moratoria_raw %>%
  select(state, starts_with("s1_"), -contains("covid"), -contains("non_payment")) %>%
  mutate(state = str_squish(str_replace_all(state, "\\d", ""))) %>%
  rename(start_date = s1_first_date_of_effect,
         end_date   = s1_date_of_expiration) %>%
  make_daily_flags("notice_moratoria")


#
# S2: Filing moratoria
# --------------------------------------------------------------------------
# Prevents a landlord from filing an eviction claim with the court —
# the measure that most directly suppresses the eviction filing count.
filing_moratoria <-
  moratoria_raw %>%
  select(state, starts_with("s2_"), -contains("covid"), -contains("non_payment")) %>%
  mutate(state = str_squish(str_replace_all(state, "\\d", ""))) %>%
  rename(start_date = s2_first_date_of_effect,
         end_date   = s2_date_of_expiration) %>%
  make_daily_flags("filing_moratoria")


# ==========================================================================
# 4. COMBINE NOTICE AND FILING INTO ONE TREATMENT VARIABLE
# ==========================================================================
# A state-day is treated if EITHER a notice OR filing moratorium was active.
# We also keep the separate flags for descriptive purposes.
# --------------------------------------------------------------------------

# build a complete daily panel: every state × every day in the period
all_states_daily <-
  crossing(
    state = fips_data$state_name,
    date  = seq(PERIOD_START, PERIOD_END, by = "day")
  )

moratoria_daily <-
  all_states_daily %>%
  # join notice flags
  left_join(
    notice_moratoria %>%
      select(state, date, notice_moratoria) %>%
      mutate(state = case_when(
        state == "Washington, DC" ~ "District of Columbia",
        TRUE ~ state
      )),
    by = c("state", "date")
  ) %>%
  # join filing flags
  left_join(
    filing_moratoria %>%
      select(state, date, filing_moratoria) %>%
      mutate(state = case_when(
        state == "Washington, DC" ~ "District of Columbia",
        TRUE ~ state
      )),
    by = c("state", "date")
  ) %>%
  # missing = no moratorium (0)
  mutate(
    notice_moratoria = replace_na(notice_moratoria, 0L),
    filing_moratoria = replace_na(filing_moratoria, 0L),
    moratoria        = as.integer(notice_moratoria == 1 | filing_moratoria == 1)
  )


# ==========================================================================
# 5. COLLAPSE TO MONTHLY
# ==========================================================================
# A month is treated if ANY day in that month had a notice or filing
# moratorium in effect. This correctly handles moratoria that start or
# end mid-month — the month of onset is counted as treatment.
# --------------------------------------------------------------------------

moratoria_monthly <-
  moratoria_daily %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(state, month) %>%
  summarize(
    notice_moratoria = as.integer(any(notice_moratoria == 1)),
    filing_moratoria = as.integer(any(filing_moratoria == 1)),
    moratoria        = as.integer(any(moratoria == 1)),
    .groups          = "drop"
  ) %>%
  rename(date = month) %>%
  arrange(state, date)

# quick sanity check — states with moratoria and their coverage dates
moratoria_monthly %>%
  filter(moratoria == 1) %>%
  group_by(state) %>%
  summarize(
    start  = min(date),
    end    = max(date),
    months = n(),
    .groups = "drop"
  ) %>%
  arrange(start) %>%
  print(n = 40)

# states with no moratoria (will serve as pure controls)
moratoria_monthly %>%
  group_by(state) %>%
  summarize(ever_treated = any(moratoria == 1), .groups = "drop") %>%
  filter(!ever_treated) %>%
  arrange(state) %>%
  print(n = 30)


# ==========================================================================
# 6. SAVE
# ==========================================================================

qsave(moratoria_monthly, "data/moratoria/moratoria_processed.qs")
