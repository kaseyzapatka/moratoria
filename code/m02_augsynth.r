# ==========================================================================
# Purpose: Augmented synthetic control analysis of eviction moratoria effects
#
# Script:
# ----------
# 1. PACKAGES & OPTIONS
# 2. LOAD DATA
# 3. DEFINE STATE GROUPS AND STUDY SUBSETS
# 4. SINGLE ADOPTION: Indiana (augmented synthetic control)
# 5. STAGGERED ADOPTION: all treated states
# 6. TIME COHORT EFFECTS
#
# Modeling approach:
# ----------
# The augmented synthetic control (Ben-Michael, Feller & Rothstein 2021)
# addresses two key shortcomings of the standard DiD and synthetic control:
#   (1) the pandemic hit all states simultaneously, making any single
#       untreated state an imperfect counterfactual;
#   (2) treatment was adopted at different times across states (staggered),
#       which biases the standard TWFE estimator.
#
# The augsynth package handles both issues:
#   - augsynth()     estimates the average treatment effect for a single
#                    adoption date, using ridge regression to augment the
#                    synthetic control weights
#   - multisynth()   extends this to staggered adoption, estimating
#                    state-level and average ATTs pooled across cohorts
#
# We present three analyses:
#   (A) Single adoption — Indiana as the primary case study, comparing to
#       a synthetic counterfactual built from all untreated control states
#   (B) Staggered adoption — pooled estimates across all treated states
#       using partially-pooled synthetic controls
#   (C) Time cohort analysis — effects broken out by when states adopted
#
# Note on weights: the augsynth estimator automatically assigns weights to
# control units to best approximate the pre-treatment outcome trajectory
# of the treated unit. Weights are adjusted by ridge regression when
# progfunc = "ridge". We report the resulting weights to assess which
# states drive the counterfactual.
# ==========================================================================


# ==========================================================================
# 1. PACKAGES & OPTIONS
# ==========================================================================

rm(list = ls())

#
# Load libraries
# --------------------------------------------------------------------------
# augsynth must be installed from GitHub: devtools::install_github("ebenmichael/augsynth")
librarian::shelf(tidyverse, qs, augsynth, gt)

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
# 3. DEFINE STATE GROUPS AND STUDY SUBSETS
# ==========================================================================

#
# Control states (never had notice or filing moratoria)
# --------------------------------------------------------------------------
control_states <-
  data %>%
  group_by(state) %>%
  summarize(ever_treated = any(moratoria == 1), .groups = "drop") %>%
  filter(!ever_treated) %>%
  pull(state)

cat("Control states:\n")
print(control_states)

#
# Treated states — with at least 3 months of moratorium coverage
# --------------------------------------------------------------------------
# States with very short moratoria (< 3 months) offer insufficient
# treatment contrast for the synthetic control to detect an effect.
# Montana (3 months, borderline), South Carolina (3 months), and
# Wisconsin (3 months) are included but should be interpreted cautiously.
treated_states <-
  data %>%
  filter(moratoria == 1) %>%
  group_by(state) %>%
  summarize(treat_months = n(), .groups = "drop") %>%
  filter(treat_months >= 3) %>%
  pull(state)

cat("\nTreated states (≥3 months of moratorium):\n")
print(treated_states)

#
# States to exclude from staggered adoption
# --------------------------------------------------------------------------
# Colorado had two separate treatment windows (Apr–Jun 2020 and Oct 2020–
# Jan 2021). The partially pooled synthetic control in multisynth expects
# a single adoption date per unit; Colorado's re-treatment complicates
# this. We include it but note the limitation — the onset date used will
# be April 2020 (its first treatment month), and the off-treatment
# interval will appear as a post-treatment dip in estimates.
staggered_exclude <- character(0)   # no exclusions by default; revise if needed


# ==========================================================================
# 4. SINGLE ADOPTION: Indiana (augmented synthetic control)
# ==========================================================================
# Indiana is the primary single-adoption case:
#   - Clean, unambiguous treatment (Mar 19 – Aug 15, 2020)
#   - Full data running back to 2016 (longest pre-treatment window)
#   - Geographically in the Midwest; compared to a pool of 15 control states
#
# We fit two versions:
#   (a) Standard synthetic control (scm = TRUE, progfunc = "None"):
#       weights on control states, no outcome model adjustment
#   (b) Augmented synthetic control (scm = TRUE, progfunc = "ridge"):
#       weights + ridge-regression adjustment for systematic bias
# --------------------------------------------------------------------------

#
# Indiana dataset: one treated state + all control states, study window
# --------------------------------------------------------------------------
# Study window for single adoption: narrow to a window where the pre-
# and post-treatment periods are approximately equal and the treated
# state shows a clear counterfactual contrast.
indiana_study_start <- as.Date("2018-01-01")
indiana_study_end   <- as.Date("2020-08-01")   # 2 months post-treatment end

indiana_data <-
  data %>%
  filter(state %in% c("Indiana", control_states)) %>%
  filter(between(date, indiana_study_start, indiana_study_end)) %>%
  glimpse()

# Pre-treatment: Jan 2018 – Feb 2020 = 26 months
# Post-treatment: Mar 2020 – Aug 2020 = 6 months
# Pre (26) >> Post (6) ✓


#
# (a) Standard synthetic control — Indiana
# --------------------------------------------------------------------------
indiana_syn <- augsynth(
  filing_rate_log ~ moratoria,   # outcome ~ treatment
  state,                          # unit identifier
  date,                           # time identifier
  data     = indiana_data,
  progfunc = "None",              # pure synthetic control (no ridge adjustment)
  scm      = TRUE
)

indiana_syn
indiana_syn_sum <- summary(indiana_syn)
plot(indiana_syn, inf_type = "jackknife+")

#
# (b) Augmented synthetic control — Indiana (ridge)
# --------------------------------------------------------------------------
indiana_ridge <- augsynth(
  filing_rate_log ~ moratoria,
  state,
  date,
  data     = indiana_data,
  progfunc = "ridge",             # ridge regression augmentation
  scm      = TRUE
)

indiana_ridge
indiana_ridge_sum <- summary(indiana_ridge)
plot(indiana_ridge, inf_type = "jackknife+")


#
# Synthetic control weights — what states drive Indiana's counterfactual?
# --------------------------------------------------------------------------

# standard synthetic control weights
data.frame(indiana_syn$weights) %>%
  rownames_to_column("State") %>%
  filter(indiana_syn.weights > 0.001) %>%
  ggplot() +
  geom_col(aes(x = reorder(State, indiana_syn.weights),
               y = indiana_syn.weights)) +
  coord_flip() +
  plot_theme +
  labs(
    x     = "",
    y     = "Weight",
    title = "Synthetic control weights — Indiana",
    subtitle = "States contributing to Indiana's synthetic counterfactual"
  )

ggsave(paste0(output, "indiana_synthcontrol_weights.png"), width = 10, height = 7, dpi = 300)

# ridge augmented weights
data.frame(indiana_ridge$weights) %>%
  rownames_to_column("State") %>%
  filter(indiana_ridge.weights > 0.001) %>%
  ggplot() +
  geom_col(aes(x = reorder(State, indiana_ridge.weights),
               y = indiana_ridge.weights)) +
  coord_flip() +
  plot_theme +
  labs(
    x        = "",
    y        = "Weight",
    title    = "Augmented synthetic control weights — Indiana (ridge)",
    subtitle = "States contributing to Indiana's ridge-adjusted counterfactual"
  )

ggsave(paste0(output, "indiana_augsynth_weights.png"), width = 10, height = 7, dpi = 300)


#
# Indiana observed vs. synthetic (with and without ridge augmentation)
# --------------------------------------------------------------------------
indiana_plot_data <-
  indiana_data %>%
  filter(state == "Indiana") %>%
  bind_cols(
    syn_diff   = indiana_syn_sum$att$Estimate,
    ridge_diff = indiana_ridge_sum$att$Estimate,
    ridge_lo   = indiana_ridge_sum$att$lower_bound,
    ridge_hi   = indiana_ridge_sum$att$upper_bound
  ) %>%
  mutate(
    synthetic_indiana = filing_rate + syn_diff,
    ridge_indiana     = filing_rate + ridge_diff
  )

# plot observed, synthetic, and ridge
indiana_plot_data %>%
  ggplot() +
  geom_line(aes(x = date, y = filing_rate,
                color = "Indiana (observed)"),
            linewidth = 1) +
  geom_line(aes(x = date, y = ridge_indiana,
                color = "Synthetic Indiana (ridge)"),
            linewidth = 1, linetype = "dashed") +
  geom_ribbon(
    data = indiana_plot_data %>% filter(date >= as.Date("2020-03-01")),
    aes(x = date, ymin = ridge_indiana + ridge_lo, ymax = ridge_indiana + ridge_hi),
    fill = "steelblue", alpha = 0.2
  ) +
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dashed", color = "black") +
  annotate("text",
           x = as.Date("2020-03-15"), y = max(indiana_plot_data$filing_rate) * 0.9,
           label = "Moratorium begins",
           hjust = 0, size = 4) +
  scale_color_manual(values = c(
    "Indiana (observed)"       = "black",
    "Synthetic Indiana (ridge)" = "steelblue"
  )) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Eviction filing rate (log scale)",
    x        = "",
    color    = "",
    title    = "Indiana vs. Synthetic Indiana",
    subtitle = "Augmented synthetic control with ridge regression",
    caption  = "Shaded band shows 95% jackknife+ confidence interval for the post-treatment period."
  )

ggsave(paste0(output, "indiana_synthcontrol.png"), width = 14, height = 8, dpi = 300)


# confidence interval version (cleaner — ridge only)
indiana_plot_data %>%
  ggplot() +
  geom_ribbon(
    data = indiana_plot_data %>% filter(date >= as.Date("2020-03-01")),
    aes(x = date, ymin = ridge_indiana + ridge_lo, ymax = ridge_indiana + ridge_hi),
    fill = "steelblue", alpha = 0.2, color = NA
  ) +
  geom_line(aes(x = date, y = filing_rate, color = "Indiana (observed)"),
            linewidth = 1) +
  geom_line(aes(x = date, y = ridge_indiana, color = "Synthetic Indiana"),
            linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dashed", color = "black") +
  scale_color_manual(values = c(
    "Indiana (observed)" = "black",
    "Synthetic Indiana"  = "steelblue"
  )) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Eviction filing rate (log scale)",
    x        = "",
    color    = "",
    title    = "Indiana vs. Synthetic Indiana",
    subtitle = "Augmented synthetic control — with 95% confidence interval",
    caption  = "Shaded band shows 95% jackknife+ confidence interval (post-treatment only)."
  )

ggsave(paste0(output, "indiana_synthcontrol_CI.png"), width = 14, height = 8, dpi = 300)


# ==========================================================================
# 5. STAGGERED ADOPTION: all treated states
# ==========================================================================
# The multisynth estimator (partially pooled synthetic controls) pools
# information across treated states that adopted at different times.
# It estimates a per-state ATT and an overall average ATT, using a
# shared synthetic control that is partially pooled across cohorts.
#
# The degree of pooling is controlled by nu: nu = 0 is fully separate
# synthetic controls per state; nu = 1 is a single pooled control.
# The default (nu estimated from data) typically lands somewhere between.
# --------------------------------------------------------------------------

#
# Staggered adoption dataset: all treated + all control states, full window
# --------------------------------------------------------------------------
staggered_data <-
  data %>%
  filter(
    state %in% c(treated_states, control_states),
    !state %in% staggered_exclude
  ) %>%
  # full study window
  filter(between(date, as.Date("2017-01-01"), as.Date("2022-06-01")))

# multisynth requires a balanced panel — the internal QP solver fails if any
# state has missing observations (NA rows) for months within the study window.
# States with data starting after Jan 2017 (Colorado, Wisconsin, New Hampshire,
# South Carolina) are dropped here; they either have short moratoria or limited
# pre-treatment data that would produce unreliable synthetic controls anyway.
n_expected <- n_distinct(staggered_data$date)

staggered_balanced <- staggered_data %>%
  group_by(state) %>%
  filter(n() == n_expected) %>%
  ungroup()

cat("\nDropped from staggered analysis (incomplete data for full window):\n")
setdiff(unique(staggered_data$state), unique(staggered_balanced$state)) %>%
  sort() %>% print()

cat("\nStates in staggered adoption analysis:\n")
staggered_balanced %>% distinct(state) %>% arrange(state) %>% print(n = 40)

# Pre-treatment: Jan 2017 – Feb 2020 = 38 months
# Post-treatment: Mar 2020 – Jun 2022 = 28 months
# Pre (38) > Post (28) ✓


#
# Staggered adoption model
# --------------------------------------------------------------------------
# n_lags:  number of pre-treatment lags to use for matching.
#          12 months captures a full year of seasonality.
# n_leads: number of post-treatment periods to estimate ATT.
#          24 months post-onset; the latest cohort (May 2020) has 25 months
#          of data through June 2022, so this is within the window.
ppool_syn <- multisynth(
  filing_rate_log ~ moratoria,
  state,
  date,
  data    = staggered_balanced,
  n_lags  = 12,
  n_leads = 24
)

ppool_syn
cat("Pooling parameter (nu):", ppool_syn$nu, "\n")
ppool_syn_sum <- summary(ppool_syn)


#
# Average treatment effect across all treated states
# --------------------------------------------------------------------------
ppool_syn_sum$att %>%
  as_tibble() %>%
  filter(Level == "Average") %>%
  select(Time, Estimate, lower_bound, upper_bound) %>%
  print(n = 30)

ppool_syn_sum$att %>%
  filter(Level == "Average") %>%
  ggplot(aes(x = Time, y = Estimate)) +

  # post-treatment shading
  annotate("rect",
           xmin = 0, xmax = max(ppool_syn_sum$att$Time, na.rm = TRUE),
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.4) +

  # confidence band (post-treatment only)
  geom_ribbon(
    data = ppool_syn_sum$att %>%
      filter(Level == "Average", Time >= 0),
    aes(ymin = lower_bound, ymax = upper_bound),
    fill = "black", alpha = 0.15, color = NA
  ) +

  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(color = "black") +
  geom_line(color = "black") +

  # annotate pre / post
  annotate("text",
           x = min(ppool_syn_sum$att$Time, na.rm = TRUE) * 0.5,
           y = max(abs(ppool_syn_sum$att$Estimate), na.rm = TRUE) * 1.1,
           label = "Pre-treatment", fontface = "bold", size = 4.5) +
  annotate("text",
           x = max(ppool_syn_sum$att$Time, na.rm = TRUE) * 0.5,
           y = max(abs(ppool_syn_sum$att$Estimate), na.rm = TRUE) * 1.1,
           label = "Post-treatment", fontface = "bold", size = 4.5) +

  scale_x_continuous(
    breaks = seq(
      floor(min(ppool_syn_sum$att$Time,  na.rm = TRUE)),
      ceiling(max(ppool_syn_sum$att$Time, na.rm = TRUE)),
      by = 3
    )
  ) +
  plot_theme +
  labs(
    y        = "Average treatment effect (log filing rate)",
    x        = "Months since moratorium enactment",
    title    = "Average effect of eviction moratoria on filing rates",
    subtitle = "Staggered adoption — partially pooled synthetic controls",
    caption  = paste0(
      "Note: Time = 0 is the first month of treatment for each state. Pre-treatment estimates near\n",
      "zero support the parallel trends assumption. Shaded band is 95% jackknife+ CI."
    )
  )

ggsave(paste0(output, "synthcontrol_avg.png"), width = 14, height = 8, dpi = 300)


#
# State-level treatment effects (with 95% jackknife+ confidence intervals)
# --------------------------------------------------------------------------
ppool_syn_sum$att %>%
  filter(Level != "Average") %>%
  ggplot(aes(x = Time, y = Estimate)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # CI ribbon — post-treatment only (jackknife+ inference is post-treatment)
  geom_ribbon(
    data = ~ filter(., Level != "Average", Time >= 0),
    aes(ymin = lower_bound, ymax = upper_bound),
    fill = "steelblue", alpha = 0.2, color = NA
  ) +
  geom_line(color = "black", linewidth = 0.7) +
  facet_wrap(~Level) +
  plot_theme +
  theme(legend.position = "none") +
  labs(
    x        = "Months since moratorium enactment",
    y        = "Treatment effect (log filing rate)",
    title    = "State-level effects of eviction moratoria",
    subtitle = "Staggered adoption — partially pooled synthetic controls",
    caption  = "Shaded band shows 95% jackknife+ confidence interval (post-treatment only)."
  )

ggsave(paste0(output, "synthcontrol_by_state.png"), width = 16, height = 12, dpi = 300)


# ==========================================================================
# 6. TIME COHORT EFFECTS
# ==========================================================================
# Time cohort analysis groups states by when they adopted the moratorium
# and estimates separate ATTs for each cohort. This tests whether the
# timing of adoption matters for the size of the effect — for example,
# early adopters (March 2020) may have prevented a larger initial surge
# than late adopters (April or May 2020).
# --------------------------------------------------------------------------

#
# Time cohort model
# --------------------------------------------------------------------------
ppool_syn_time <- multisynth(
  filing_rate_log ~ moratoria,
  state,
  date,
  data        = staggered_balanced,   # same balanced panel as above
  n_lags      = 12,
  n_leads     = 24,
  time_cohort = TRUE   # estimate ATT separately by treatment cohort (month)
)

ppool_syn_time
ppool_syn_time_sum <- summary(ppool_syn_time)

# which cohorts exist in the data?
ppool_syn_time_sum$att %>% as_tibble() %>% distinct(Level) %>% print()


#
# Visualize time cohort effects
# --------------------------------------------------------------------------
# Recode cohort labels to readable month-year format
cohort_labels <- c(
  "2020-03-01" = "March 2020 cohort",
  "2020-04-01" = "April 2020 cohort",
  "2020-05-01" = "May 2020 cohort",
  "2020-10-01" = "October 2020 cohort",
  "Average"    = "Average (all cohorts)"
)

effects_by_cohort <-
  ppool_syn_time_sum$att %>%
  as_tibble() %>%
  mutate(
    cohort = recode(as.character(Level), !!!cohort_labels),
    # order: Average first, then chronologically
    cohort = fct_relevel(cohort, "Average (all cohorts)")
  )

effects_by_cohort %>%
  ggplot(aes(x = Time, y = Estimate, color = cohort)) +

  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +

  # confidence ribbon (post-treatment)
  geom_ribbon(
    data = effects_by_cohort %>% filter(Time >= 0),
    aes(ymin = lower_bound, ymax = upper_bound, fill = cohort),
    alpha = 0.15, color = NA
  ) +

  geom_point(size = 1.5) +
  geom_line(linewidth = 0.8) +

  facet_wrap(~cohort, nrow = 2) +

  # highlight the average cohort
  scale_color_manual(
    values = c(
      "Average (all cohorts)"  = "#800000",
      "March 2020 cohort"      = "black",
      "April 2020 cohort"      = "steelblue",
      "May 2020 cohort"        = "grey40",
      "October 2020 cohort"    = "darkorange"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Average (all cohorts)"  = "#800000",
      "March 2020 cohort"      = "black",
      "April 2020 cohort"      = "steelblue",
      "May 2020 cohort"        = "grey40",
      "October 2020 cohort"    = "darkorange"
    )
  ) +

  plot_theme +
  theme(legend.position = "none") +
  labs(
    x        = "Months since moratorium enactment",
    y        = "Treatment effect (log filing rate)",
    title    = "Eviction moratorium effects by adoption cohort",
    subtitle = "Staggered adoption — partially pooled synthetic controls by treatment month",
    caption  = paste0(
      "March 2020 cohort: Delaware, Indiana, Minnesota, Montana, New Hampshire, South Carolina, Wisconsin\n",
      "April 2020 cohort: Alaska, Colorado, Connecticut, Florida, Kentucky\n",
      "May 2020 cohort: Pennsylvania, Vermont\n",
      "October 2020 cohort: Colorado (second window)\n",
      "Shaded band shows 95% jackknife+ CI (post-treatment only)."
    )
  )

ggsave(paste0(output, "synthcontrol_by_time.png"), width = 16, height = 10, dpi = 300)


#
# Summary table: average treatment effect by state
# --------------------------------------------------------------------------
att_table <-
  ppool_syn_sum$att %>%
  as_tibble() %>%
  filter(Level != "Average", Time == 1) %>%   # first post-treatment month
  select(State = Level, ATT = Estimate, Lower = lower_bound, Upper = upper_bound) %>%
  arrange(ATT) %>%
  mutate(across(c(ATT, Lower, Upper), ~round(., 3))) %>%
  gt() %>%
  tab_header(title = "Average treatment effect — first post-treatment month") %>%
  cols_align(align = "left", columns = State) %>%
  tab_options(
    table.font.size           = "medium",
    heading.title.font.size   = "large",
    column_labels.font.weight = "bold",
    row.striping.include_table_body = TRUE
  )

gt::gtsave(att_table, paste0(output, "att_by_state_table.html"))
