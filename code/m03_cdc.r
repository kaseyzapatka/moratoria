# ==========================================================================
# Purpose: Analysis focused on the CDC eviction moratorium
#
# Script:
# ----------
# 1. PACKAGES & OPTIONS
# 2. LOAD DATA
# 3. CODE CDC TREATMENT VARIABLE AND POLICY PERIODS
# 4. DEFINE STATE GROUPS
# 5. DESCRIPTIVE: FILING RATES ACROSS POLICY PERIODS
# 6. MULTI-PERIOD DiD: CDC-ONLY STATES
# 7. FULL MODEL: STATE MORATORIA + CDC (ALL STATES)
# 8. REBOUND ANALYSIS: AROUND CDC EXPIRATION
#
# Identification note — simultaneous treatment:
# ----------
# The CDC moratorium (September 4, 2020 – August 26, 2021) applied to ALL
# states simultaneously. This eliminates cross-sectional variation in
# CDC treatment status — unlike the state moratoria analysis (m01, m02),
# we cannot form a valid synthetic control or TWFE DiD with date fixed
# effects (the CDC indicator would be collinear with date FE).
#
# Instead, identification comes from temporal variation within states:
#   (a) Pre-CDC baseline (Jan 2017–Feb 2020) vs. CDC vs. post-CDC, using
#       month-of-year FE to absorb seasonality and year FE to absorb
#       year-level shocks. Within-year variation in CDC status
#       (Sept–Dec 2020 and Jan–Aug 2021 vs. Sept–Dec 2021) identifies
#       the CDC coefficient.
#   (b) Rebound analysis: do filing rates increase when CDC expires in
#       August 2021? A visible rebound in CDC-only states — but not in
#       state-moratorium states (which had already seen their state
#       moratoria expire in 2020) — would implicate CDC as the suppressing
#       force.
#
# Key implication for prior analyses:
# ----------
# The state moratorium estimates in m01 and m02 already NET OUT the CDC
# effect. Both treated and control states were under CDC during Sept 2020–
# Aug 2021, so DiD comparisons between state-moratorium and no-state-
# moratorium states difference out the federal component. Those estimates
# represent the MARGINAL effect of state moratoria above and beyond CDC,
# not the total effect of all moratoria combined. The current script
# estimates the total effect of the CDC moratorium across all states.
# ==========================================================================


# ==========================================================================
# 1. PACKAGES & OPTIONS
# ==========================================================================

rm(list = ls())

#
# Load libraries
# --------------------------------------------------------------------------
librarian::shelf(tidyverse, qs, fixest, broom)

#
# Options
# --------------------------------------------------------------------------
options(width = max(80L, as.integer(Sys.getenv("COLUMNS")), na.rm = TRUE), setWidthOnResize = TRUE, scipen = 10)

#
# Output path
# --------------------------------------------------------------------------
output <- "output/"

#
# Plot theme
# --------------------------------------------------------------------------
plot_theme <-
  theme_bw() +
  theme(
    legend.position        = "bottom",
    legend.title           = element_text(size = 13, face = "bold"),
    legend.text            = element_text(size = 13),
    axis.title.x           = element_text(size = 13),
    axis.title.y           = element_text(size = 13),
    axis.text.x            = element_text(size = 12),
    axis.text.y            = element_text(size = 12),
    plot.title.position    = "plot",
    plot.title             = element_text(size = 18, face = "bold", hjust = 0.05),
    plot.subtitle          = element_text(size = 14, face = "italic", hjust = 0.05),
    plot.caption           = element_text(size = 11, hjust = 0),
    plot.caption.position  = "plot",
    panel.border           = element_blank(),
    axis.line.x            = element_line(color = "black"),
    axis.line.y            = element_line(color = "black"),
    strip.background       = element_blank(),
    strip.text             = element_text(face = "bold", size = 12)
  )


# ==========================================================================
# 2. LOAD DATA
# ==========================================================================

data <- qread("data/model_ready.qs")


# ==========================================================================
# 3. CODE CDC TREATMENT VARIABLE AND POLICY PERIODS
# ==========================================================================
#
# CDC moratorium timeline:
#   Issued:   September 4, 2020
#   Extended: multiple times through the Biden administration
#   Expired:  August 26, 2021 (Supreme Court blocked further extension)
#
# Coded monthly: September 2020 (first full month under CDC) through
# August 2021 (last full month; expired Aug 26 so August is fully covered).
# --------------------------------------------------------------------------

CDC_START <- as.Date("2020-09-01")
CDC_END   <- as.Date("2021-08-01")

model_data <-
  data %>%
  mutate(
    cdc_moratoria   = as.integer(between(date, CDC_START, CDC_END)),
    month_of_year   = month(date),
    year            = factor(year(date)),
    # Policy period categories (for descriptive analysis and multi-period model)
    period = case_when(
      date < as.Date("2020-03-01") ~ "Pre-pandemic",
      date < CDC_START             ~ "Pandemic (pre-CDC)",
      date <= CDC_END              ~ "CDC moratorium",
      TRUE                         ~ "Post-moratorium"
    ),
    period = factor(period,
                    levels = c("Pre-pandemic", "Pandemic (pre-CDC)",
                               "CDC moratorium", "Post-moratorium"))
  )


# ==========================================================================
# 4. DEFINE STATE GROUPS
# ==========================================================================

#
# Pure control states: never had a state-level notice or filing moratorium.
# The CDC moratorium was their ONLY treatment — these are the most useful
# states for isolating the CDC effect.
# --------------------------------------------------------------------------
cdc_only_states <-
  model_data %>%
  group_by(state) %>%
  summarize(ever_state_treated = any(moratoria == 1), .groups = "drop") %>%
  filter(!ever_state_treated) %>%
  pull(state)

#
# State moratorium states: had state-level moratoria (most expired by Aug 2020,
# before CDC began in Sept 2020). Also received CDC moratorium Sept 2020–Aug 2021.
# --------------------------------------------------------------------------
state_moratoria_states <- setdiff(unique(model_data$state), cdc_only_states)

cat("CDC-only states (n =", length(cdc_only_states), "):\n")
print(sort(cdc_only_states))

cat("\nState moratorium states (n =", length(state_moratoria_states), "):\n")
print(sort(state_moratoria_states))


# ==========================================================================
# 5. DESCRIPTIVE: FILING RATES ACROSS POLICY PERIODS
# ==========================================================================
# Plot average log filing rates over time for both groups, with vertical
# lines marking CDC start and end and the inter-period boundaries shaded.
# --------------------------------------------------------------------------

period_avg <-
  model_data %>%
  mutate(group = if_else(state %in% cdc_only_states,
                         "CDC only (no state moratorium)",
                         "Had state moratorium + CDC")) %>%
  group_by(group, date) %>%
  summarize(mean_log_rate = mean(filing_rate_log, na.rm = TRUE), .groups = "drop")

ggplot(period_avg, aes(x = date, y = mean_log_rate, color = group)) +

  # shade CDC period
  annotate("rect",
           xmin = CDC_START, xmax = CDC_END + months(1),
           ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.08) +

  # shade pandemic-pre-CDC period
  annotate("rect",
           xmin = as.Date("2020-03-01"), xmax = CDC_START,
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.08) +

  geom_line(linewidth = 1) +

  # period boundary lines
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = CDC_START,
             linetype = "dashed", color = "steelblue", linewidth = 0.7) +
  geom_vline(xintercept = CDC_END + months(1),
             linetype = "dashed", color = "steelblue", linewidth = 0.7) +

  # period labels
  annotate("text", x = as.Date("2018-06-01"), y = -0.2,
           label = "Pre-pandemic", size = 3.5, color = "grey40") +
  annotate("text", x = as.Date("2020-05-15"), y = -0.2,
           label = "Pandemic\n(pre-CDC)", size = 3.5, color = "grey40") +
  annotate("text", x = as.Date("2021-02-15"), y = -0.2,
           label = "CDC\nmoratorium", size = 3.5, color = "steelblue") +
  annotate("text", x = as.Date("2022-01-15"), y = -0.2,
           label = "Post-\nmoratorium", size = 3.5, color = "grey40") +

  scale_color_manual(values = c(
    "CDC only (no state moratorium)" = "black",
    "Had state moratorium + CDC"     = "firebrick"
  )) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Mean log eviction filing rate",
    x        = "",
    color    = "",
    title    = "Eviction filing rates by policy period and group",
    subtitle = "CDC moratorium (Sept 2020–Aug 2021) highlighted in blue",
    caption  = paste0(
      "Blue shading = CDC moratorium period; grey shading = pandemic onset (pre-CDC).\n",
      "Lines show group means; CDC-only states had no state-level moratorium.\n",
      "State moratorium states' state-level moratoria mostly expired by Aug 2020."
    )
  )

ggsave(paste0(output, "cdc_filing_rates.png"), width = 14, height = 8, dpi = 300)


# ==========================================================================
# 6. MULTI-PERIOD DiD: CDC-ONLY STATES
# ==========================================================================
# For states that never had a state-level moratorium, CDC was the sole
# treatment. We estimate period-specific effects relative to the pre-
# pandemic baseline using state FE, year FE, and month-of-year FE.
#
# Fixed effects:
#   state        — absorbs time-invariant state-level differences
#   year         — absorbs year-level shocks (e.g., the general 2020
#                  pandemic effect separate from moratoria)
#   month_of_year — absorbs within-year seasonality (eviction filings
#                   are highly seasonal)
#
# The CDC period dummy is identified from within-year, within-state
# variation: Sept–Dec 2020 and Jan–Aug 2021 are compared to the same
# calendar months in other years, controlling for the 2020/2021 year
# effects. This is a temporal ITS within a panel framework.
#
# Note: the pandemic-pre-CDC period (Mar–Aug 2020) confounds pandemic
# shock with the absence of a federal moratorium. We include it as a
# separate indicator so it does not bias the CDC or post-CDC estimates.
# --------------------------------------------------------------------------

cdc_only_data <-
  model_data %>%
  filter(state %in% cdc_only_states) %>%
  mutate(
    pandemic_no_cdc = as.integer(period == "Pandemic (pre-CDC)"),
    cdc_period      = as.integer(period == "CDC moratorium"),
    post_cdc        = as.integer(period == "Post-moratorium")
    # reference category: pre-pandemic (period == "Pre-pandemic")
  )

#
# Multi-period model
# --------------------------------------------------------------------------
cdc_multi <- feols(
  filing_rate_log ~ pandemic_no_cdc + cdc_period + post_cdc |
    state + year + month_of_year,
  data = cdc_only_data,
  vcov = ~state
)

cat("\n--- Multi-period model: CDC-only states ---\n")
etable(cdc_multi,
       coefstat = "se",
       dict     = c(
         pandemic_no_cdc = "Pandemic (pre-CDC, Mar–Aug 2020)",
         cdc_period      = "CDC moratorium (Sept 2020–Aug 2021)",
         post_cdc        = "Post-moratorium (Sept 2021–Jun 2022)"
       ))

# Marginal effect: CDC period vs. post-CDC period
# (tests whether filing rates were additionally suppressed during CDC vs. after)
# Computed manually: diff = cdc_period coef - post_cdc coef; SE from delta method
vcov_multi     <- vcov(cdc_multi)
cdc_vs_post    <- coef(cdc_multi)["cdc_period"] - coef(cdc_multi)["post_cdc"]
cdc_vs_post_se <- sqrt(
  vcov_multi["cdc_period", "cdc_period"] +
  vcov_multi["post_cdc",   "post_cdc"]   -
  2 * vcov_multi["cdc_period", "post_cdc"]
)
cat(sprintf(
  "\nCDC vs. post-CDC contrast: est = %.3f, SE = %.3f, t = %.3f\n",
  cdc_vs_post, cdc_vs_post_se, cdc_vs_post / cdc_vs_post_se
))


#
# Coefficient plot — period effects
# --------------------------------------------------------------------------
period_coefs <-
  tibble(
    period   = c("Pandemic\n(pre-CDC)", "CDC\nmoratorium", "Post-\nmoratorium"),
    estimate = coef(cdc_multi)[c("pandemic_no_cdc", "cdc_period", "post_cdc")],
    lo       = confint(cdc_multi)[c("pandemic_no_cdc", "cdc_period", "post_cdc"), 1],
    hi       = confint(cdc_multi)[c("pandemic_no_cdc", "cdc_period", "post_cdc"), 2]
  ) %>%
  mutate(period = factor(period, levels = period))

period_coefs %>%
  ggplot(aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.15, linewidth = 0.9, color = "steelblue") +
  geom_point(size = 3.5, color = "steelblue") +
  plot_theme +
  labs(
    x        = "",
    y        = "Effect on log filing rate (vs. pre-pandemic baseline)",
    title    = "CDC moratorium and filing rates — CDC-only states",
    subtitle = "Period-specific effects relative to pre-pandemic (Jan 2017–Feb 2020)",
    caption  = paste0(
      "Sample: 15 states with no state-level eviction moratorium (CDC was their only treatment).\n",
      "Fixed effects: state + year + month-of-year. Standard errors clustered by state.\n",
      "The CDC coefficient captures the within-year deviation from seasonal + year-level baselines."
    )
  )

ggsave(paste0(output, "cdc_period_effects.png"), width = 10, height = 7, dpi = 300)


# ==========================================================================
# 7. FULL MODEL: STATE MORATORIA + CDC (ALL STATES)
# ==========================================================================
# Including both the state-level moratorium indicator and the CDC indicator
# in a single model allows us to estimate each effect holding the other
# constant. This directly answers: what is the marginal effect of each
# policy type?
#
# The `moratoria` coefficient: marginal effect of a state-level moratorium,
#   above and beyond the CDC moratorium that may be simultaneously active.
#   Identified from cross-state variation in treatment timing (some states
#   had moratoria, others didn't).
#
# The `cdc_moratoria` coefficient: effect of being in the CDC period,
#   across all states simultaneously. Identified from temporal variation
#   (within-year, within-state comparison of CDC vs. non-CDC months),
#   controlling for seasonality and year-level shocks via FE.
#
# NOTE: The pandemic-pre-CDC period (Mar–Aug 2020) includes large non-
# moratorium shocks (court closures, behavioral change). We include it
# as a separate indicator to isolate the CDC and state-moratorium effects.
# --------------------------------------------------------------------------

full_data <-
  model_data %>%
  mutate(
    pandemic_no_cdc = as.integer(period == "Pandemic (pre-CDC)")
  )

#
# Full model — state + CDC moratoria
# --------------------------------------------------------------------------
full_model <- feols(
  filing_rate_log ~ moratoria + cdc_moratoria + pandemic_no_cdc |
    state + year + month_of_year,
  data = full_data,
  vcov = ~state
)

cat("\n--- Full model: state moratoria + CDC, all states ---\n")
etable(full_model,
       coefstat = "se",
       dict     = c(
         moratoria       = "State moratorium (S1/S2)",
         cdc_moratoria   = "CDC moratorium (Sept 2020–Aug 2021)",
         pandemic_no_cdc = "Pandemic onset (Mar–Aug 2020, no CDC)"
       ))


#
# Coefficient plot — state vs CDC effects
# --------------------------------------------------------------------------
both_coefs <-
  tibble(
    policy   = c("State moratorium\n(S1/S2)", "CDC moratorium\n(federal)"),
    estimate = coef(full_model)[c("moratoria", "cdc_moratoria")],
    lo       = confint(full_model)[c("moratoria", "cdc_moratoria"), 1],
    hi       = confint(full_model)[c("moratoria", "cdc_moratoria"), 2]
  )

both_coefs %>%
  ggplot(aes(x = policy, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.15, linewidth = 0.9, color = "steelblue") +
  geom_point(size = 3.5, color = "steelblue") +
  plot_theme +
  labs(
    x        = "",
    y        = "Effect on log filing rate",
    title    = "State vs. federal moratorium effects on eviction filings",
    subtitle = "Both treatment types in a single model — all states",
    caption  = paste0(
      "All states, Jan 2017–Jun 2022. Fixed effects: state + year + month-of-year.\n",
      "Standard errors clustered by state.\n",
      "State moratorium = S1 (notice) or S2 (filing) moratorium active that month.\n",
      "CDC moratorium = September 2020–August 2021 (federal, applies to all states)."
    )
  )

ggsave(paste0(output, "cdc_state_vs_federal.png"), width = 10, height = 7, dpi = 300)


# ==========================================================================
# 8. REBOUND ANALYSIS: AROUND CDC EXPIRATION
# ==========================================================================
# If the CDC moratorium was suppressing eviction filings, we expect filing
# rates to increase when CDC expires in August 2021. The cleanest test
# compares the trajectory of CDC-only states around CDC expiration to the
# trajectory of state-moratorium states over the same window.
#
# By August 2021:
#   - CDC-only states: have been under CDC since Sept 2020 (11 months)
#     and have no prior state moratoria — any rebound in Sept 2021 is
#     attributable to CDC expiration.
#   - State moratorium states: their state moratoria mostly expired by
#     Aug 2020. They also exit CDC in Aug 2021. If CDC was meaningfully
#     suppressing filings in these states too, they should also rebound;
#     if their filings had already normalized under CDC, the rebound is
#     smaller. The difference in rebound magnitude between the two groups
#     is evidence that CDC had a differential effect.
# --------------------------------------------------------------------------

rebound_data <-
  model_data %>%
  mutate(
    group = factor(
      if_else(state %in% cdc_only_states,
              "CDC only (no state moratorium)",
              "Had state moratorium + CDC"),
      levels = c("CDC only (no state moratorium)", "Had state moratorium + CDC")
    ),
    # months relative to CDC expiration (August 2021 = month 0)
    months_to_expiry = interval(CDC_END, date) %/% months(1)
  ) %>%
  filter(between(months_to_expiry, -12, 10))

#
# Average filing rates around CDC expiration, by group
# --------------------------------------------------------------------------
rebound_avg <-
  rebound_data %>%
  group_by(group, months_to_expiry) %>%
  summarize(
    mean_log_rate = mean(filing_rate_log, na.rm = TRUE),
    .groups       = "drop"
  )

rebound_avg %>%
  ggplot(aes(x = months_to_expiry, y = mean_log_rate, color = group)) +

  # shade post-expiry period
  annotate("rect",
           xmin = 0, xmax = max(rebound_avg$months_to_expiry),
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +

  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +

  annotate("text", x = -6, y = max(rebound_avg$mean_log_rate, na.rm = TRUE),
           label = "CDC active", hjust = 0.5, size = 4, color = "grey40") +
  annotate("text", x = 5, y = max(rebound_avg$mean_log_rate, na.rm = TRUE),
           label = "Post-CDC", hjust = 0.5, size = 4, color = "grey40") +

  scale_color_manual(values = c(
    "CDC only (no state moratorium)" = "black",
    "Had state moratorium + CDC"     = "firebrick"
  )) +
  scale_x_continuous(breaks = seq(-12, 10, by = 2)) +
  plot_theme +
  labs(
    x        = "Months relative to CDC expiration (August 2021 = 0)",
    y        = "Mean log eviction filing rate",
    color    = "",
    title    = "Filing rate rebound around CDC moratorium expiration",
    subtitle = "Both groups exit CDC simultaneously in August 2021",
    caption  = paste0(
      "Grey shading = post-CDC period (September 2021 onward).\n",
      "A steeper rebound in CDC-only states (black line) vs. state-moratorium states\n",
      "(red line) would indicate CDC was the primary suppressing force."
    )
  )

ggsave(paste0(output, "cdc_expiry_rebound.png"), width = 14, height = 8, dpi = 300)


#
# Rebound regression: within CDC-only states, test for significant increase
# at CDC expiration (month 0 vs months -1 to -6)
# --------------------------------------------------------------------------
# An event-study style regression: relative-month dummies, reference = month -1
# (the last month of CDC). If the CDC moratorium was binding, months ≥ 0
# should have positive, significant coefficients.

rebound_reg_data <-
  rebound_data %>%
  filter(state %in% cdc_only_states) %>%
  mutate(
    rel_month       = months_to_expiry,
    rel_month_fct   = relevel(factor(rel_month), ref = "-1")
  )

rebound_event <- feols(
  filing_rate_log ~ i(rel_month, ref = -1) | state + month_of_year,
  data = rebound_reg_data,
  vcov = ~state
)

cat("\n--- Rebound event study: CDC-only states around CDC expiration ---\n")
etable(rebound_event, coefstat = "se", keep = "%rel_month")

#
# Plot the rebound event study coefficients
# --------------------------------------------------------------------------
rebound_coef_df <-
  broom::tidy(rebound_event, conf.int = TRUE) %>%
  filter(str_detect(term, "rel_month")) %>%
  mutate(
    rel_month = as.integer(str_extract(term, "-?[0-9]+"))
  ) %>%
  filter(rel_month != -1)   # reference month excluded

rebound_coef_df %>%
  ggplot(aes(x = rel_month, y = estimate)) +

  annotate("rect",
           xmin = 0, xmax = max(rebound_coef_df$rel_month),
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +

  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +

  geom_ribbon(
    data = rebound_coef_df %>% filter(rel_month >= 0),
    aes(ymin = conf.low, ymax = conf.high),
    fill = "steelblue", alpha = 0.2, color = NA
  ) +
  geom_ribbon(
    data = rebound_coef_df %>% filter(rel_month < 0),
    aes(ymin = conf.low, ymax = conf.high),
    fill = "grey60", alpha = 0.2, color = NA
  ) +

  geom_point(size = 2.5, color = "black") +
  geom_line(color = "black") +

  scale_x_continuous(breaks = seq(-12, 10, by = 2)) +
  plot_theme +
  labs(
    x        = "Months relative to CDC expiration (month −1 = last CDC month)",
    y        = "Effect on log filing rate (vs. month −1)",
    title    = "Rebound in filings at CDC expiration — CDC-only states",
    subtitle = "Event study around August 2021 (month 0 = September 2021)",
    caption  = paste0(
      "Sample: 15 states with no state-level moratorium.\n",
      "Fixed effects: state + month-of-year. Standard errors clustered by state.\n",
      "Reference = month −1 (last month CDC was active). Shaded bands = 95% CI.\n",
      "Positive post-expiry coefficients indicate a rebound in filing activity."
    )
  )

ggsave(paste0(output, "cdc_expiry_event_study.png"), width = 14, height = 8, dpi = 300)
