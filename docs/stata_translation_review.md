# Stata-to-R Translation Review

**Date:** 2026-03-18
**Reviewer:** Claude Code (Opus 4.6)

## Executive Summary

The translation of 5 Stata scripts into R is high quality overall. The project includes a
rigorous verification harness (`verify_stata_rewrite.R`) that compares R outputs against Stata
baselines at both the `.dta` and CSV level. If verification passes, the core data pipeline is
correct. No show-stopping bugs were found, but there are several code-quality issues, missing
assertions, and undocumented deviations worth addressing.

## Scripts Reviewed

| Stata Original | R Translation | Purpose |
|---|---|---|
| `process_weather.do` | `process_weather.R` | Convert weather CSV to usable format |
| `process_holidays.do` | `process_holidays.R` | Convert holidays CSV to usable format |
| `01_01_mk_working.do` | `01_01_mk_working.R` | Separate injury/pay data from raw |
| `01_02_mk_expanded_pay.do` | `01_02_mk_expanded_pay.R` | Expand pay data to daily level |
| `01_03_mk_pre_network.do` | `01_03_mk_pre_network.R` | Create exposure matrices for network panels |

## Issues by Severity

### CRITICAL (0 confirmed -- verification protects against these)

The following were flagged but are safe if `verify_stata_rewrite.R` passes:

| Issue | File:Line | Notes |
|---|---|---|
| `geo_div < 800 -> NA` filter not in Stata | `01_02:157` | Added intentionally during translation to match Stata output. Passes verification. Investigated separately -- see geo_div analysis below. |
| Extra columns kept (`yearsoldonworkdate`, `variation_description`) | `01_01:127-131` | Deduplication still matches because `unique()` operates on the same key columns. Verified. |
| IOD flag recomputed from pay_data instead of using working_expanded's | `01_03:44-49,75-76` | Extra logic beyond Stata. If verified, this was an intentional improvement. Needs a comment. |

### WARNING (8 -- code quality / robustness issues)

| # | Issue | File:Line | Fix |
|---|---|---|---|
| W1 | `nzchar(NA)` returns `TRUE` -- assertion on line 90 is weaker than Stata's | `01_01:90` | Change to `!is.na(x) & nzchar(x)` |
| W2 | Missing `stopifnot(!any(is.na(pay$maximum_gap_2015)))` after fill | `01_01:156` | Add assertion |
| W3 | Merge assertion is one-directional (doesn't check all `info` rows used) | `01_01:133-134` | Add `stopifnot(all(info$cleaned_variation_desc %chin% pay$cleaned_variation_desc))` |
| W4 | Missing assertion that `gap_end` has at most one distinct non-NA value per group | `01_01:159-165` | Add uniqueness check before fill |
| W5 | Missing merge assertions -- exposure_panel rows silently dropped if unmatched | `01_03:73-74` | Add `stopifnot` that no exposure rows are lost |
| W6 | `first_inj_date` exported as integer Stata daily number while other dates are formatted strings | `01_03:141` | Document or unify format |
| W7 | Hard-coded `setcolorder` is fragile if upstream columns change | `01_03:122-135` | Add a try/catch or column-existence check |
| W8 | `run_stata_rewrites.R` doesn't use `needs_rerun`/`run_with_logging` pattern | `run_stata_rewrites.R` | Acceptable for transitional script; retired during integration. |

### MINOR (5 -- cosmetic / low-risk)

| # | Issue | File:Line | Fix |
|---|---|---|---|
| M1 | `gsub('Incl', 'Include')` is unconditional -- could corrupt "Including" -> "Includeing" | `01_02:82` | Use word-boundary pattern or add conditional |
| M2 | Missing year<=2013 outlier filter before pay counts summary | `01_02:55-74` | Only affects `01_02_work_data_counts.csv` (descriptive table) |
| M3 | `process_weather.R` column order `prcp,tmax,tmin,date` differs from Stata's `tmin,tmax,prcp,date` | `process_weather:27` | Cosmetic; doesn't affect merges |
| M4 | `as.Date(DATE)` lacks explicit format in process_weather | `process_weather:19` | Add `format = '%Y-%m-%d'` for robustness |
| M5 | Missing `confirm variable` equivalents for expected exposure column IDs | `01_03` | Add assertions for expected column names |

### BUG FIX: `rain` variable

**Location:** `01_02_mk_expanded_pay.R:307`

- **Stata:** `gen rain = prcp>0` -- In Stata, if `prcp` is missing, `prcp>0` evaluates to 1
  (missing treated as +Inf). This is a Stata quirk that produces semantically wrong results:
  a missing precipitation reading gets coded as "it rained."
- **R original:** `fifelse(is.na(prcp), 1, as.numeric(prcp > 0))` -- replicated Stata's
  buggy behavior.
- **Fix:** Changed to `fifelse(is.na(prcp), 0, as.numeric(prcp > 0))` -- missing precip
  defaults to no rain, which is the correct semantic.

## tmp_*.R File Cleanup

36 temporary debugging files were created during translation verification:

| Category | Count |
|---|---|
| 01_01 (pay_data debugging) | 17 |
| 01_02 (working_expanded / fornetwork) | 6 |
| 01_03 (pre_network CSVs) | 5 |
| process_holidays | 3 |
| Full pipeline end-to-end | 2 |
| Calendar week logic | 2 |
| process_weather | 1 |

All 36 files were deleted. The authoritative verification is `verify_stata_rewrite.R`.

## Integration Plan (Executed)

1. **Phase 1:** Fixed assertions, added comments, deleted tmp files
2. **Phase 2:** Switched intermediate outputs from .dta to .rds
3. **Phase 3:** Integrated into `run_prep_data.R` with `run_with_logging()`
4. **Phase 4:** Archived .do files to `legacy/stata/`, retired transitional scripts
5. **Phase 5:** Updated README, config helpers, validated pipeline structure
