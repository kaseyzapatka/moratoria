# summary of findings

1. Descriptive trends (average_filing_rate.png)
The filing rate data shows a sharp and near-universal collapse in March 2020 — filings dropped from a pre-pandemic mean of roughly 4–5 per 1,000 renters to near zero across virtually all states, treated and control alike. This is the central identification challenge: the pandemic itself caused an enormous decline in eviction filings independent of any moratorium, because courts closed, landlords didn't file, and renters received stimulus. By 2021–2022 rates had partially recovered to ~2–4. The treated states look similar in trajectory to controls through this period, which is the first hint that estimating a moratorium-specific effect will be difficult.

2. DiD validity (indiana_DiD.png, indiana_event_study.png)
The Indiana/Virginia comparison illustrates the fundamental DiD problem here. Both states show nearly identical dramatic collapses in March 2020, but Virginia never had a moratorium. The DiD estimate picks up the gap between Indiana and Virginia, but:

Virginia's raw filings drop almost as sharply as Indiana's — probably driven by the same pandemic-era court closures and behavioral changes.
The pre-treatment gap in the event study (indiana_event_study.png) is not flat at zero — Indiana has systematically lower log filing rates than Virginia throughout the pre-period. This non-zero intercept isn't automatically a problem, but the gap is somewhat noisy and the post-moratorium trajectory is hard to interpret cleanly.
The chosen control (Virginia) was itself affected by the general pandemic shock, making it a poor counterfactual for Indiana specifically during the moratorium period.
This is exactly the motivation for moving to synthetic controls — the parallel trends assumption is questionable when all states were hit by the same macro shock simultaneously.

3. Indiana single-adoption synthetic control (indiana_synthcontrol_CI.png)
The ridge-augmented synthetic control constructs a weighted donor pool to match Indiana's pre-treatment trajectory. The pre-treatment fit is excellent — observed and synthetic nearly overlap from 2017–Feb 2020. Post-treatment, however, both the observed Indiana and the synthetic Indiana collapse together and then recover together. The observed line tracks inside the synthetic's confidence band for essentially the entire post-treatment period.

Interpretation: There is no detectable moratorium effect for Indiana distinguishable from the synthetic counterfactual. The moratorium period coincides with the same general pandemic decline that affected all states, and the synthetic control (built from control states) captures that decline. Whatever additional suppression Indiana's moratorium caused is either small relative to the pandemic shock, or is confounded by the CDC federal moratorium that was simultaneously active across all states.

4. Staggered adoption average ATT (synthcontrol_avg.png)
The pooled average effect across all treated states (averaged over cohorts and post-treatment periods) shows:

A sharp initial drop of roughly −1.5 log points at T=1 (first month of treatment)
This effect decays toward zero by months T=8–10
Confidence intervals are wide — the 95% band spans roughly −3.5 to +0.5 at peak
The initial drop pattern is consistent with a real immediate suppression effect: when moratoria first go into effect, filings fall. But the confidence intervals are too wide to rule out zero, and the effect disappears quickly. This is unsurprising given that all treated states adopted moratoria in March–May 2020, which is also when the pandemic shock hit hardest — the timing makes disentangling the moratorium from the pandemic nearly impossible.

5. Time cohort heterogeneity (synthcontrol_by_time.png)
The three adoption cohorts (March, April, May 2020 adopters) all show the same basic pattern: an immediate T=0 drop that partially recovers by T=5–8 and then levels off near zero. All cohorts have overlapping confidence intervals that cross zero at most post-treatment time points. The similarity across cohorts makes substantive sense — all three cohort groups adopted within a two-month window, so they faced the same pandemic macro-environment.

6. State-level heterogeneity (synthcontrol_by_state.png)
State-level effects vary, which is expected. Connecticut, Delaware, and Pennsylvania show larger estimated initial drops (−2 to −3 log points); Indiana and Montana show smaller drops. Kentucky shows a counterintuitive positive spike. All states converge toward zero effect within 10–15 months. The variation is likely partly real (different moratorium strength, duration, and enforcement) and partly noise from imprecise synthetic control donors. With wide CIs for individual states, none of these state-level estimates are statistically distinguishable from zero.

Bottom Line
Your read is correct: the augmented synthetic controls do not find a statistically significant effect of state eviction moratoria on filing rates. The core reason is fundamental to the research design, not a modeling artifact:

Simultaneous treatment and shock: All state moratoria went into effect in March–May 2020, exactly when COVID-19 caused a massive, universal reduction in eviction filings across all states (treated and control). Separating "moratorium effect" from "pandemic shock" is extremely difficult even with a good synthetic counterfactual.

Federal moratorium contamination: The CDC moratorium was active across all states (including controls) for much of the post-treatment period. Control states were not actually untreated — they were under federal protection too. This compresses the estimated state moratorium effect toward zero.

Effect duration: The visible initial drop at T=1 (consistent across methods) suggests moratoria may have had a real, short-lived suppression effect in the immediate months, but the effect dissipates quickly — either because the moratorium binding constraint was soon overlaid by other factors, or because landlords adjusted behavior.

Wide confidence intervals: Even if the point estimates are negative, the precision is low because the panel is short (28 post-treatment months), treated states all adopted simultaneously (limiting variation), and the pandemic-era filing data is unusually noisy.

A notable finding to highlight: the pattern is consistent across all methods — an immediate negative spike that decays. This is suggestive of a real short-run effect even if individual estimates don't clear the significance threshold. The substantive story is plausibly that state moratoria added marginal additional suppression on top of the federal moratorium and general pandemic effects, but this marginal increment is too small to estimate precisely given the macro confounding.