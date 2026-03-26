# ==========================================================================
# Purpose: Difference-in-differences analysis of eviction moratoria effects
#
# Script:
# ----------
# 1. PACKAGES & OPTIONS
# 2. LOAD DATA
# 3. IDENTIFY CANDIDATE DiD PAIRS (parallel trends check)
# 4. DIFF-IN-DIFF MODELS
# 5. VISUALIZATIONS
#
# DiD design:
# ----------
# A difference-in-differences requires one treated state and one comparable
# control state with parallel pre-treatment trends. We evaluate parallel
# trends visually and select two pairs: one for each primary comparison.
#
# Candidate treated states (clean treatment, full data, clear onset):
#   - Indiana:   notice + filing, Mar–Aug 2020 (6 months)
#   - Minnesota: filing Mar 2020, notice Aug 2020, through Jun 2022 (longest)
#   - Vermont:   notice + filing, May 2020–Jul 2021 (15 months)
#
# Limitation of DiD:
# The DiD assumes a single common treatment date and strictly parallel
# counterfactual trends — assumptions that become increasingly strained
# when (a) treatment timing varies across states and (b) pandemic-era
# shocks hit all states simultaneously. This analysis is presented
# primarily to motivate the move to augmented synthetic control.
# ==========================================================================


# ==========================================================================
# 1. PACKAGES & OPTIONS
# ==========================================================================

rm(list = ls())

#
# Load libraries
# --------------------------------------------------------------------------
librarian::shelf(tidyverse, qs, fixest)

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
    axis.line.y            = element_line(color = "black")
  )


# ==========================================================================
# 2. LOAD DATA
# ==========================================================================

data <- qread("data/model_ready.qs")

#
# State group definitions
# --------------------------------------------------------------------------
# These are updated from the previous analysis to reflect states available
# in the new data source.

# States that never had a notice or filing moratorium — pure controls
control_states <- data %>%
  group_by(state) %>%
  summarize(ever_treated = any(moratoria == 1), .groups = "drop") %>%
  filter(!ever_treated) %>%
  pull(state)

cat("Control states available:\n")
print(control_states)


# ==========================================================================
# 3. IDENTIFY CANDIDATE DiD PAIRS (parallel trends check)
# ==========================================================================
# For a credible DiD, the treated and control state must exhibit parallel
# pre-treatment trends in eviction filing rates. We plot all plausible
# pairs to identify the best comparisons.
#
# The pre-treatment window for these plots is Oct 2019 – Feb 2020
# (6 months before treatment onset) to focus on recent trend alignment.
# --------------------------------------------------------------------------

PRE_START   <- as.Date("2019-10-01")
TREAT_START <- as.Date("2020-03-01")   # first treatment month (Indiana, Minnesota)


#
# Indiana parallel trends — candidate control states
# --------------------------------------------------------------------------
# Indiana: notice + filing moratorium Mar 19 – Aug 15, 2020.
# Strong candidate because treatment is clean and data runs from 2016.
# Geographically and demographically, Midwestern/Southern states
# with similar housing markets make the best controls.
# --------------------------------------------------------------------------

indiana_pre <-
  data %>%
  filter(state %in% control_states | state == "Indiana") %>%
  filter(between(date, PRE_START, as.Date("2020-08-01")))

# Plot Indiana vs. all control states
ggplot(indiana_pre) +
  geom_line(aes(x = date, y = filing_rate, group = state), color = "grey75") +
  geom_line(
    data = indiana_pre %>% filter(state == "Indiana"),
    aes(x = date, y = filing_rate, color = "Indiana"),
    linewidth = 1.2
  ) +
  geom_vline(xintercept = TREAT_START, linetype = "dashed", color = "black") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("Indiana" = "#800000")) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Eviction filing rate (per 1,000 renters)",
    x        = "",
    color    = "",
    title    = "Indiana vs. potential control states",
    subtitle = "Pre-treatment period only — checking for parallel trends",
    caption  = "Dashed line marks March 2020 (treatment onset)."
  )

ggsave(paste0(output, "did_indiana_parallel_trends.png"), width = 14, height = 8, dpi = 300)


#
# Indiana vs. best individual control states
# --------------------------------------------------------------------------
# From visual inspection, highlight the most parallel candidates.
# Update the vector below after reviewing the plot above.
indiana_controls <- c("Virginia", "Ohio", "Tennessee", "Oklahoma")

data %>%
  filter(state %in% c("Indiana", indiana_controls)) %>%
  filter(between(date, PRE_START, as.Date("2020-08-01"))) %>%
  ggplot() +
  geom_line(aes(x = date, y = filing_rate, color = state), linewidth = 1) +
  geom_vline(xintercept = TREAT_START, linetype = "dashed", color = "black") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(
    values = c(
      "Indiana"   = "#800000",
      "Virginia"  = "grey60",
      "Ohio"      = "steelblue",
      "Tennessee" = "grey40",
      "Oklahoma"  = "grey20"
    )
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Eviction filing rate (per 1,000 renters)",
    x        = "",
    color    = "State",
    title    = "Indiana vs. candidate control states",
    subtitle = "Assessing parallel pre-treatment trends",
    caption  = "Dashed line marks March 2020 (treatment onset)."
  )

ggsave(paste0(output, "did_indiana_controls.png"), width = 14, height = 8, dpi = 300)


#
# Minnesota parallel trends — candidate control states
# --------------------------------------------------------------------------
# Minnesota: notice + filing moratorium from Mar/Aug 2020 through Jun 2022.
# The unusually long treatment makes Minnesota interesting for the
# long-run effect, but a control with parallel pre-treatment trends
# is harder to find given Minnesota's distinctive rental housing market.
# --------------------------------------------------------------------------

minnesota_pre <-
  data %>%
  filter(state %in% control_states | state == "Minnesota") %>%
  filter(between(date, PRE_START, as.Date("2020-08-01")))

ggplot(minnesota_pre) +
  geom_line(aes(x = date, y = filing_rate, group = state), color = "grey75") +
  geom_line(
    data = minnesota_pre %>% filter(state == "Minnesota"),
    aes(x = date, y = filing_rate, color = "Minnesota"),
    linewidth = 1.2
  ) +
  geom_vline(xintercept = TREAT_START, linetype = "dashed", color = "black") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("Minnesota" = "#800000")) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Eviction filing rate (per 1,000 renters)",
    x        = "",
    color    = "",
    title    = "Minnesota vs. potential control states",
    subtitle = "Pre-treatment period only — checking for parallel trends",
    caption  = "Dashed line marks March 2020 (treatment onset)."
  )

ggsave(paste0(output, "did_minnesota_parallel_trends.png"), width = 14, height = 8, dpi = 300)


# ==========================================================================
# 4. DIFF-IN-DIFF MODELS
# ==========================================================================
# We use a two-way fixed effects (TWFE) estimator with state and time fixed
# effects. This is the standard DiD implementation for panel data.
# Standard errors are clustered at the state level.
#
# NOTE: The TWFE DiD is presented primarily as a baseline comparison.
# Its key assumptions — parallel counterfactual trends and no spillovers
# across states — are likely violated during the pandemic, since the
# national shock affected all states simultaneously and some control
# states experienced informal or indirect effects on eviction activity.
# The augmented synthetic control in m02 addresses these concerns.
# --------------------------------------------------------------------------

#
# DiD dataset — Indiana vs. best control state
# --------------------------------------------------------------------------
# Update control_for_indiana below after reviewing the parallel trends plots.
# The default is Virginia, which showed the closest pre-treatment alignment
# in the original analysis; verify with the new data.
control_for_indiana <- "Virginia"

indiana_did_data <-
  data %>%
  filter(state %in% c("Indiana", control_for_indiana)) %>%
  filter(between(date, as.Date("2017-01-01"), as.Date("2020-08-01"))) %>%
  mutate(
    treated  = as.integer(state == "Indiana"),
    post     = as.integer(date >= TREAT_START),
    treat_x_post = treated * post
  )

#
# TWFE DiD — Indiana
# --------------------------------------------------------------------------
# With only 2 states, cluster-robust SEs at the state level are degenerate
# (2 clusters → singular covariance matrix). We use HC1 heteroskedasticity-
# robust SEs instead; the point estimate is identical either way.
did_indiana <- feols(
  filing_rate_log ~ treat_x_post | state + date,
  data  = indiana_did_data,
  vcov  = "HC1"
)

summary(did_indiana)

#
# Event study — Indiana (manual gap plot)
# --------------------------------------------------------------------------
# With only 2 units a formal iplot()-based event study cannot produce valid
# standard errors. Instead we plot the raw period-by-period log filing rate
# gap between Indiana and its control — the non-parametric equivalent.
# Pre-treatment gaps near zero support the parallel trends assumption;
# post-treatment gaps show the dynamic effect of the moratorium.
indiana_gap <-
  indiana_did_data %>%
  select(state, date, filing_rate_log) %>%
  pivot_wider(names_from = state, values_from = filing_rate_log) %>%
  rename(indiana = Indiana,
         control = !!sym(control_for_indiana)) %>%
  mutate(gap = indiana - control)

ggplot(indiana_gap, aes(x = date, y = gap)) +
  annotate("rect",
           xmin = TREAT_START, xmax = max(indiana_gap$date),
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_vline(xintercept = TREAT_START, linetype = "dashed", color = "black") +
  geom_line(color = "black") +
  geom_point(color = "black") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = paste0("Log filing rate gap (Indiana minus ", control_for_indiana, ")"),
    x        = "",
    title    = paste0("Event study: Indiana vs. ", control_for_indiana),
    subtitle = "Pre-treatment gap near zero supports parallel trends assumption",
    caption  = "Shaded area = post-treatment period. Dashed line = moratorium onset."
  )

ggsave(paste0(output, "indiana_event_study.png"), width = 14, height = 8, dpi = 300)


#
# DiD dataset — Minnesota vs. best control state
# --------------------------------------------------------------------------
control_for_minnesota <- "Ohio"    # update after reviewing parallel trends plots

# for Minnesota, extend the post-treatment window to capture the full treatment
minnesota_did_data <-
  data %>%
  filter(state %in% c("Minnesota", control_for_minnesota)) %>%
  filter(between(date, as.Date("2017-01-01"), as.Date("2022-06-01"))) %>%
  mutate(
    treated      = as.integer(state == "Minnesota"),
    post         = as.integer(date >= TREAT_START),
    treat_x_post = treated * post
  )

#
# TWFE DiD — Minnesota
# --------------------------------------------------------------------------
did_minnesota <- feols(
  filing_rate_log ~ treat_x_post | state + date,
  data  = minnesota_did_data,
  vcov  = "HC1"
)

summary(did_minnesota)

#
# Event study — Minnesota (manual gap plot)
# --------------------------------------------------------------------------
minnesota_gap <-
  minnesota_did_data %>%
  select(state, date, filing_rate_log) %>%
  pivot_wider(names_from = state, values_from = filing_rate_log) %>%
  rename(minnesota = Minnesota,
         control   = !!sym(control_for_minnesota)) %>%
  mutate(gap = minnesota - control)

ggplot(minnesota_gap, aes(x = date, y = gap)) +
  annotate("rect",
           xmin = TREAT_START, xmax = max(minnesota_gap$date),
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_vline(xintercept = TREAT_START, linetype = "dashed", color = "black") +
  geom_line(color = "black") +
  geom_point(color = "black") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = paste0("Log filing rate gap (Minnesota minus ", control_for_minnesota, ")"),
    x        = "",
    title    = paste0("Event study: Minnesota vs. ", control_for_minnesota),
    subtitle = "Pre-treatment gap near zero supports parallel trends assumption",
    caption  = "Shaded area = post-treatment period. Dashed line = moratorium onset."
  )

ggsave(paste0(output, "minnesota_event_study.png"), width = 14, height = 8, dpi = 300)


# ==========================================================================
# 5. VISUALIZATIONS
# ==========================================================================

#
# Descriptive overview: filing rates across all states
# --------------------------------------------------------------------------
# Computes the mean filing rate across all states in each month and
# overlays individual state trends as context. The pandemic shock is
# immediately visible in the collapse of filings around March 2020.

data %>%
  group_by(date) %>%
  mutate(mean_rate = mean(filing_rate, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = date, y = filing_rate, group = state),
            color = "grey70", linewidth = 0.4, alpha = 0.7) +
  geom_line(aes(x = date, y = mean_rate),
            color = "black", linewidth = 1.3) +
  geom_vline(xintercept = TREAT_START, linetype = "dashed", color = "#198942") +
  annotate("text",
           x = TREAT_START + 10, y = max(data$filing_rate, na.rm = TRUE) * 0.92,
           label = "First state moratoria enacted",
           color = "#198942", hjust = 0, size = 4.5, fontface = "bold") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Eviction filing rate (per 1,000 renters)",
    x        = "",
    title    = "State eviction filing rates over time",
    subtitle = "Individual states (grey) and cross-state mean (black)",
    caption  = paste0(
      "Note: Includes ", n_distinct(data$state), " states with complete data from 2017 through June 2022. ",
      "Hawaii and New York excluded due to data limitations.\n",
      "Source: Eviction Lab."
    )
  )

ggsave(paste0(output, "average_filing_rate.png"), width = 14, height = 8, dpi = 300)


#
# Moratoria timeline: which states had notice/filing moratoria and when
# --------------------------------------------------------------------------
moratoria_timeline <-
  data %>%
  select(state, date, moratoria) %>%
  filter(moratoria == 1) %>%
  group_by(state) %>%
  mutate(total_months = n()) %>%
  ungroup() %>%
  mutate(coverage = "Moratorium active")

ggplot(moratoria_timeline) +
  geom_tile(
    aes(y = reorder(state, total_months), x = date, fill = coverage),
    alpha = 0.85
  ) +
  scale_fill_manual(values = c("Moratorium active" = "grey30")) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "3 months",
    limits      = c(as.Date("2020-01-01"), as.Date("2022-07-01"))
  ) +
  plot_theme +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  labs(
    y       = "",
    x       = "",
    title   = "State eviction moratoria (notice and filing) by month",
    subtitle = "States ordered by total months of moratorium coverage",
    caption = paste0(
      "Note: Includes only notice (S1) and filing (S2) moratoria from the ICPSR dataset (157201 V2).\n",
      "Enforcement, hearing, and judgment moratoria are excluded as they occur after filing."
    )
  )

ggsave(paste0(output, "moratoria_start_and_end.png"), width = 14, height = 9, dpi = 300)


#
# Indiana DiD: observed vs. counterfactual
# --------------------------------------------------------------------------
indiana_did_plot_data <-
  data %>%
  filter(state %in% c("Indiana", control_for_indiana)) %>%
  filter(between(date, as.Date("2019-10-01"), as.Date("2020-08-01")))

ggplot(indiana_did_plot_data) +
  geom_line(aes(x = date, y = filing_rate, color = state), linewidth = 1.2) +
  geom_vline(xintercept = TREAT_START, linetype = "dashed", color = "black") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(
    values = c("Indiana" = "#800000",
               setNames("grey40", control_for_indiana))
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Eviction filing rate (per 1,000 renters)",
    x        = "",
    color    = "State",
    title    = paste0("Indiana vs. ", control_for_indiana),
    subtitle = "Difference-in-differences: notice and filing moratorium",
    caption  = "Dashed line marks March 2020 (treatment onset for Indiana)."
  )

ggsave(paste0(output, "indiana_DiD.png"), width = 14, height = 8, dpi = 300)


#
# Minnesota DiD: observed vs. counterfactual
# --------------------------------------------------------------------------
minnesota_did_plot_data <-
  data %>%
  filter(state %in% c("Minnesota", control_for_minnesota)) %>%
  filter(between(date, as.Date("2019-10-01"), as.Date("2021-06-01")))

ggplot(minnesota_did_plot_data) +
  geom_line(aes(x = date, y = filing_rate, color = state), linewidth = 1.2) +
  geom_vline(xintercept = TREAT_START, linetype = "dashed", color = "black") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_color_manual(
    values = c("Minnesota" = "#800000",
               setNames("grey40", control_for_minnesota))
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y        = "Eviction filing rate (per 1,000 renters)",
    x        = "",
    color    = "State",
    title    = paste0("Minnesota vs. ", control_for_minnesota),
    subtitle = "Difference-in-differences: notice and filing moratorium",
    caption  = "Dashed line marks March 2020 (treatment onset for Minnesota's filing moratorium)."
  )

ggsave(paste0(output, "minnesota_DiD.png"), width = 14, height = 8, dpi = 300)
