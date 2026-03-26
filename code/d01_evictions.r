# ==========================================================================
# Purpose: Process state-level eviction filing data
#
# Script:
# ----------
# 1. PACKAGES & OPTIONS
# 2. LOAD DATA
# 3. CHECK DATA COMPLETENESS
# 4. PROCESS & CLEAN
# 5. SAVE
#
# Data source: Eviction Lab monthly and county-level filing counts
# Note: The new data source includes renter_occupied_housing_units directly,
#       so we do not need a separate ACS pull for the rate denominator.
#       The renter pop is a single point estimate (constant) per state —
#       adequate for computing a relative filing rate across time.
# ==========================================================================


# ==========================================================================
# 1. PACKAGES & OPTIONS
# ==========================================================================

rm(list = ls())

#
# Load libraries
# --------------------------------------------------------------------------
librarian::shelf(tidyverse, qs, janitor)

#
# Options
# --------------------------------------------------------------------------
options(width = max(80L, as.integer(Sys.getenv("COLUMNS")), na.rm = TRUE), setWidthOnResize = TRUE, scipen = 10)


# ==========================================================================
# 2. LOAD DATA
# ==========================================================================

#
# Monthly state-level filing counts
# --------------------------------------------------------------------------
# State totals are provided directly; county data is used only to
# verify completeness (see section 3 below).
monthly_state <-
  read_csv("data/evictions/monthly_state_data_download.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  glimpse()

#
# Monthly county-level filing counts (for completeness check)
# --------------------------------------------------------------------------
monthly_county <-
  read_csv("data/evictions/monthly_county_data_download.csv", show_col_types = FALSE) %>%
  mutate(
    date       = as.Date(date),
    county_fips = str_pad(as.character(fips), 5, pad = "0"),
    state_fips  = str_sub(county_fips, 1, 2)
  ) %>%
  glimpse()


# ==========================================================================
# 3. CHECK DATA COMPLETENESS
# ==========================================================================
# Verify that county-level filings sum to state totals.
# If a state's county data is incomplete (e.g., missing counties),
# the state total will exceed the county sum and we'd want to use
# the state total directly rather than rebuild from counties.
# In practice, the state totals are authoritative — this check just
# confirms we can trust them.
#
# Results from running this check: all 34 states match at or near 100%
# (Colorado averages 99.5% due to minor rounding). We proceed with
# state-level totals directly.
# --------------------------------------------------------------------------

county_to_state <-
  monthly_county %>%
  group_by(state_fips, date) %>%
  summarize(county_sum = sum(filings_count, na.rm = TRUE), .groups = "drop")

completeness_check <-
  monthly_state %>%
  mutate(state_fips = str_pad(as.character(fips), 2, pad = "0")) %>%
  left_join(county_to_state, by = c("state_fips", "date")) %>%
  mutate(match_pct = county_sum / filings_count * 100) %>%
  group_by(name) %>%
  summarize(
    avg_match_pct = round(mean(match_pct, na.rm = TRUE), 1),
    min_match_pct = round(min(match_pct, na.rm = TRUE), 1),
    n_months      = n(),
    .groups       = "drop"
  ) %>%
  arrange(avg_match_pct)

print(completeness_check, n = 40)
# All states ≥ 99.5% — proceed with state totals directly.


# ==========================================================================
# 4. PROCESS & CLEAN
# ==========================================================================

#
# States to exclude from the analysis
# --------------------------------------------------------------------------
# Hawaii: data only starts January 2020 — no pre-treatment period.
# New York: data starts January 2019 (14 months pre-treatment) and the
#           state's policy environment is highly unusual (strong tenant
#           protections, large rental market, complex multi-layered moratoria).
#           The limited pre-treatment window and policy idiosyncrasies make it
#           a poor candidate for synthetic control comparisons.
# Puerto Rico, Virgin Islands: territories, not part of 50-state analysis.

exclude_states <- c("Hawaii", "New York", "Puerto Rico", "Virgin Islands")

#
# Clean and filter
# --------------------------------------------------------------------------
evictions <-
  monthly_state %>%
  # drop excluded states
  filter(!name %in% exclude_states) %>%
  # clean and standardize fields
  rename(
    state          = name,
    filings        = filings_count,
    renter_pop     = renter_occupied_housing_units
  ) %>%
  select(state, date, filings, renter_pop) %>%
  # compute monthly eviction filing rate per 1,000 renters
  mutate(
    filing_rate     = (filings / renter_pop) * 1000,
    filing_rate_log = log(filing_rate + 0.01)    # log-transform to reduce right skew
  ) %>%
  arrange(state, date) %>%
  glimpse()

# quick coverage check
evictions %>%
  group_by(state) %>%
  summarize(
    min_date  = min(date),
    max_date  = max(date),
    n_months  = n(),
    .groups   = "drop"
  ) %>%
  arrange(min_date) %>%
  print(n = 40)

# states in final dataset
evictions %>% distinct(state) %>% print(n = 40)


# ==========================================================================
# 5. SAVE
# ==========================================================================

qsave(evictions, "data/evictions/evictions_processed.qs")
