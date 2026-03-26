# ==========================================================================
# Purpose: Combine eviction and moratoria data into an analysis-ready panel
#
# Script:
# ----------
# 1. PACKAGES & OPTIONS
# 2. LOAD DATA
# 3. COMBINE
# 4. APPLY STUDY WINDOW AND STATE FILTERS
# 5. SAVE
#
# Study window: January 2017 – June 2022
# ----------
# Pre-treatment period (Jan 2017 – Feb 2020):  38 months
# Post-treatment period (Mar 2020 – Jun 2022):  28 months
# Pre ≥ Post ✓
#
# The window starts in 2017 rather than 2016 because several treated
# states with shorter data histories (Colorado, Wisconsin, etc.) would
# have fewer pre-treatment observations. Starting in 2017 keeps the
# panel more balanced while still preserving a meaningful pre-treatment
# window for all included states.
# ==========================================================================


# ==========================================================================
# 1. PACKAGES & OPTIONS
# ==========================================================================

rm(list = ls())

#
# Load libraries
# --------------------------------------------------------------------------
librarian::shelf(tidyverse, qs, DescTools)

#
# Options
# --------------------------------------------------------------------------
options(width = max(80L, as.integer(Sys.getenv("COLUMNS")), na.rm = TRUE), setWidthOnResize = TRUE, scipen = 10)


# ==========================================================================
# 2. LOAD DATA
# ==========================================================================

evictions <- qread("data/evictions/evictions_processed.qs")
moratoria <- qread("data/moratoria/moratoria_processed.qs")


# ==========================================================================
# 3. COMBINE
# ==========================================================================

data <-
  evictions %>%
  left_join(moratoria, by = c("state", "date")) %>%
  # states with no moratoria data are pure controls — code as untreated
  mutate(
    moratoria        = replace_na(moratoria, 0L),
    notice_moratoria = replace_na(notice_moratoria, 0L),
    filing_moratoria = replace_na(filing_moratoria, 0L)
  )


# ==========================================================================
# 4. APPLY STUDY WINDOW AND STATE FILTERS
# ==========================================================================

#
# Study window
# --------------------------------------------------------------------------
STUDY_START <- as.Date("2017-01-01")
STUDY_END   <- as.Date("2022-06-01")

#
# States to drop from analysis
# --------------------------------------------------------------------------
# These states are dropped from the eviction script (d01) and are not
# in the eviction data, but documenting here for clarity:
#   Hawaii:          data starts Jan 2020 — no pre-treatment period
#   New York:        limited pre-treatment (starts Jan 2019); idiosyncratic
#                    policy environment makes it a poor synthetic control unit
#
# Additional drop for analysis:
#   North Carolina:  notice moratorium was only active for ~3 weeks
#                    (May 30 – June 20, 2020); too short to measure a
#                    meaningful filing effect and insufficient contrast
#                    for synthetic control matching
drop_from_analysis <- c("North Carolina")

#
# Final analysis dataset
# --------------------------------------------------------------------------
model_data <-
  data %>%
  filter(between(date, STUDY_START, STUDY_END)) %>%
  filter(!state %in% drop_from_analysis) %>%
  arrange(state, date)


#
# Coverage summary
# --------------------------------------------------------------------------
cat("\n--- Study period coverage by state ---\n")
model_data %>%
  group_by(state) %>%
  summarize(
    min_date     = min(date),
    max_date     = max(date),
    n_months     = n(),
    ever_treated = any(moratoria == 1),
    treat_months = sum(moratoria),
    .groups      = "drop"
  ) %>%
  arrange(ever_treated, state) %>%
  print(n = 40)

cat("\n--- Distribution of filing rate ---\n")
cat("Skew (raw):   ", round(Skew(model_data$filing_rate,     na.rm = TRUE, method = 1), 3), "\n")
cat("Skew (log):   ", round(Skew(model_data$filing_rate_log, na.rm = TRUE, method = 1), 3), "\n")
# Log transform brings skew substantially closer to 0; we use log in models.

cat("\n--- Treated state moratoria periods ---\n")
model_data %>%
  filter(moratoria == 1) %>%
  group_by(state) %>%
  summarize(
    start_date   = min(date),
    end_date     = max(date),
    treat_months = n(),
    .groups      = "drop"
  ) %>%
  arrange(start_date) %>%
  print(n = 30)

cat("\n--- Control states (never treated) ---\n")
model_data %>%
  group_by(state) %>%
  summarize(ever_treated = any(moratoria == 1), .groups = "drop") %>%
  filter(!ever_treated) %>%
  arrange(state) %>%
  print()


# ==========================================================================
# 5. SAVE
# ==========================================================================

qsave(model_data, "data/model_ready.qs")
