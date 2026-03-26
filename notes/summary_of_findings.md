# Ideas for pre-treatment covariates

Adding pre-treatment covariates to the augmented synthetic control improves balance
between treated and synthetic control units on dimensions beyond the outcome trajectory.
These variables should be measured in the pre-treatment period (ideally 2017–2019) and
be plausibly unaffected by the moratoria themselves.

## Strong candidates

**1. Unemployment rate**
State-level monthly data from the BLS Local Area Unemployment Statistics (LAUS) program.
Unemployment is the primary economic driver of non-payment evictions, and states with
higher pre-pandemic unemployment likely had higher baseline eviction risk. Including it
helps the synthetic control match on economic conditions, not just filing trends.

**2. Rent burden (median gross rent as % of renter household income)**
Available from ACS 5-year estimates. High rent burden states have more financially
marginal tenants and higher baseline eviction risk. This is one of the strongest
structural predictors of eviction rates and would substantially improve matching.

**3. Poverty rate among renters**
Already present in the data as `poverty_rate`. Use the pre-treatment average (2017–2019).

**4. Pre-pandemic eviction filing rate (baseline)**
Average monthly filing rate from 2017–2019. Captures the structural propensity for
landlords to file in each state — courts vary widely in how routinely filings are used
as a rent collection tool vs. a last resort.

**5. Just-cause eviction protections**
Binary indicator for whether the state required just cause for eviction (from the LSC
eviction laws database already in `data/tenant_protections/`). States with just-cause
protections have lower baseline filing rates and may have absorbed the shock differently.

**6. Tenant right-to-counsel**
Binary indicator for statewide or major-city right-to-counsel laws (also in the LSC
data). Reduces eviction filings by increasing tenant bargaining power and court
representation.

## Competing treatments to control for

**7. CDC moratorium recognition**
Several states formally required landlords to certify CDC non-coverage before filing
(ICPSR dataset tracks this). These states had a federal-level treatment operating in
parallel with or instead of a state moratorium. Including a CDC moratorium indicator
as a covariate — or using it to refine state inclusion/exclusion — would sharpen the
state moratorium estimate.

**8. State unemployment insurance replacement rate**
Higher UI generosity reduced the income shock to renters during the pandemic. States
that replaced a larger share of lost wages likely saw less non-payment pressure
independent of moratoria. Available from DOL.

## Structural / political controls

**9. Share of renter-occupied housing units**
Available from ACS; already used as the filing rate denominator but also useful as
a covariate capturing how rental-heavy the housing market is.

**10. Governor party affiliation**
Proxy for the broader policy environment and enforcement posture. Republican-led states
were less likely to enact state moratoria and may have had systematically different
eviction trajectories during the pandemic for reasons beyond the moratorium itself.

**11. Court processing delays (procedural)**
Many state courts reduced capacity or closed in spring 2020 independent of any
moratorium, creating de facto filing slowdowns even in non-moratorium states. If
court closure data is available (e.g., from the National Center for State Courts),
including it would separate moratorium effects from procedural delays.
This is probably the single hardest-to-measure confounder in this analysis.
