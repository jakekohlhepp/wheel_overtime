# Pipeline Officer Counts

This note documents how the number of officers changes across the main pipeline stages used for the stylized facts and the main model.

## Summary

| Stage | File / Script | Officer count | Observation count | Notes |
| --- | --- | ---: | ---: | --- |
| Network panel | `data/01_06_panel_working.rds` | 535 | 365,979 | Full 90-day network panel used as input to `02_01_mk_estimation_sample.R` |
| Estimation sample after date and injury filters | `data/02_01_estimation_sample.rds` via `02_01_mk_estimation_sample.R` | 535 | 274,908 | No officers are dropped; only officer-days are removed |
| Stylized facts sample | `03_01_facts.R` | 535 | 274,908 | `03_01` reads the estimation sample directly and keeps the same officer set |
| Main estimation input | `04_01_estimate.R` | 535 | 274,908 | `04_01` also reads the estimation sample directly |
| Officers with identified officer fixed effects | `data/04_01_estimate.Rdata` | 481 | N/A | Effective officer count in the two-way FE logit/probit |

## Interpretation

The officer set is stable through the descriptive pipeline:

- `02_01_mk_estimation_sample.R` logs `535` officers after the date and injury filters.
- `03_01_facts.R` asserts `CONFIG$expected_estimation_officers == 535` for both the officer-day and officer-level summary tables.
- `04_01_estimate.R` uses the same `02_01_estimation_sample.rds` file as its estimation input.

So there is no officer attrition between the network panel, the stylized facts, and the raw estimation sample. The only change before estimation is a reduction in officer-days from `365,979` to `274,908`.

## Why The Officer Count Falls In Estimation

The drop from `535` officers in the estimation sample to `481` officers with identified fixed effects is caused by officers with no within-officer variation in the binary outcome:

- `54` officers have `ot_work = 0` on every observation in `data/02_01_estimation_sample.rds`.
- Those `54` officers account for `25,476` officer-days.
- They contribute `0` overtime days and `17,350` normal-work days.

In the two-way fixed-effects logit/probit estimated in `04_01_estimate.R`, those officers do not identify an officer fixed effect because they never switch into overtime. As a result, later scripts that merge in estimated officer fixed effects effectively work with `481` officers.

This is why downstream scripts such as the simulation programs report that about `10.09%` of officers are missing fixed effects:

`54 / 535 = 0.1009346`

## Related Files

- `02_01_mk_estimation_sample.R`
- `03_01_facts.R`
- `04_01_estimate.R`
- `logs/02_01_mk_estimation_sample.log`
- `data/02_01_estimation_sample.rds`
- `data/04_01_estimate.Rdata`

## Provenance

These counts were computed from the current local pipeline artifacts on March 30, 2026. If the raw data or sample-construction logic changes, this note should be recomputed.