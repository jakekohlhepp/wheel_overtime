# The Wheel of (Over)Time

Replication code for **"The Wheel of (Over)Time"** by [Jacob Kohlhepp](https://www.jkohlhepp.com) and [Robert McDonough](https://robmcdonough.com).

> In the United States public sector, there are many examples where overtime is allocated informally, and overtime earnings are concentrated among a small number of government workers. Is this government inefficiency driven by insider influence, or an efficient reflection of worker preferences? We study the Los Angeles Department of Transportation, where a few traffic officers earned more than $100,000 in overtime over 1.5 years. A constantly rotating list called "the wheel" assigns overtime equally initially, but officers are allowed to informally trade. Using novel daily personnel records, we recover the position of the wheel as well as the time-varying network of potential relationships between officers. Officers are several times more likely to work overtime when they are well-connected to coworkers likely endowed with overtime. Nevertheless, overtime inequality primarily reflects underlying differences in preferences. Informal trading achieves 93.8% of the maximum possible allocative efficiency, or $4.15 million more than random assignment.

Paper: [wheel_overtime_draft.pdf](https://www.jkohlhepp.com/pdf/wheel_overtime_draft.pdf)

## Quick Start

```r
# 1. Open the RStudio project
#    File > Open Project > productivity.Rproj

# 2. Run data preparation (network panels + map)
source("run_prep_data.R")

# 3. Run analysis pipeline in order (see Pipeline below)
source("00_02_mk_estimation_sample.R")
source("02_00_estimate.R")
# ... etc.
```

## Project Structure

```
wheel_code/
├── config.R                          # Centralized configuration (paths, parameters)
├── utils/logging.R                   # Logging and conditional re-run logic
│
├── process_holidays.R                # Pre-Stata: holidays CSV -> .dta
├── process_weather.R                 # Pre-Stata: weather CSV -> .dta
├── 01_01_mk_working.R               # R port of Stata 01_01 (working panel)
├── 01_02_mk_expanded_pay.R          # R port of Stata 01_02 (expanded pay)
├── 01_03_mk_pre_network.R           # R port of Stata 01_03 (pre-network panel)
│
├── run_prep_data.R                   # Orchestrator: prep_01 + prep_02
├── prep_01_mk_network.R             # Network panels (30/90/180-day windows)
├── prep_02_mk_map.R                 # Enforcement districts map
│
├── 00_02_mk_estimation_sample.R     # Build main estimation sample
├── 01_00_facts.R                    # Descriptive facts and figures
├── 01_02_lag_check.R                # Lag structure validation
├── 01_04_termination_did.R          # Event study: terminations
├── 01_05_new_hire.R                 # Event study: new hires
├── 01_06_fmla.R                     # Event study: peer FMLA leave
├── 01_06b_own_fmla.R               # Event study: own FMLA leave
├── 01_07_bereave.R                  # Event study: peer bereavement
├── 01_07b_own_bereave.R            # Event study: own bereavement
│
├── 02_00_estimate.R                 # Main logit/probit estimation
├── 02_00b_estimate_many.R           # Time-varying estimates
├── 02_01_display.R                  # Estimation result figures
├── 02_02_validate_valuations.R      # Validate against special events
├── 02_03_cartel_age.R               # Heterogeneity by age/tenure
├── 02_04_decomp_pref_network.R      # Preference vs. network decomposition
├── 02_07_labor_supply.R             # Labor supply analysis
│
├── 03_00_sim_random.R               # Simulation: random allocation
├── 03_01_auction_sim.R              # Simulation: auction mechanisms
├── 03_02_sim_informal.R             # Simulation: informal trading
├── 03_03_sim_informal_reverse.R     # Simulation: reverse-seniority trading
├── 03_04_sim_informal_perfect.R     # Simulation: perfect-information trading
├── 03_98_heatmap.R                  # Simulation heatmap visualization
├── 03_99_compare_sims.R             # Cross-simulation comparison
│
├── 04_00_elasticity_did.R           # Elasticity difference-in-differences
│
├── data/                            # Intermediate data files (gitignored)
├── out/figures/                     # Output figures (gitignored)
├── logs/                            # Execution logs (gitignored)
├── 20170803_payworkers_comp/        # Raw pay/workers comp data (gitignored)
├── 20190811_weather/                # Raw weather data (gitignored)
├── 20190814_fed_holidays/           # Raw holiday data (gitignored)
└── 20250311_ladot_enforcement_districts/  # District shapefiles (gitignored)
```

## Pipeline

Scripts are numbered to reflect execution order. Each tier depends on the outputs of the previous tier.

### Data Preparation

Run via `source("run_prep_data.R")`. Steps are skipped automatically when inputs and code have not changed.

| Step | Script | Output |
|------|--------|--------|
| prep_01 | `prep_01_mk_network.R` | `data/prep_01_panel_working{_30,_180}.rds` |
| prep_02 | `prep_02_mk_map.R` | `out/figures/prep_02_la_street_map.png` |

### Analysis

| Tier | Scripts | Depends On |
|------|---------|------------|
| 0 | `00_02_mk_estimation_sample.R` | prep_01 output |
| 1 | `01_00_facts.R`, `01_02` -- `01_07b` (event studies) | Tier 0 |
| 2 | `02_00_estimate.R` (main estimation) | Tier 0 |
| 3 | `02_01` -- `02_07` (estimation analysis), `03_00` -- `03_04` (simulations) | Tier 2 |
| 4 | `03_98_heatmap.R`, `03_99_compare_sims.R` | Tier 3 |
| 5 | `04_00_elasticity_did.R` | Tier 0 |

## Configuration

All parameters, paths, and thresholds are centralized in `config.R`. Key settings:

- **`network_windows`**: Rolling window sizes for exposure matrices (30, 90, 180 days)
- **`estimation_start` / `estimation_end`**: Sample period (2015-01-01 to 2016-06-30)
- **`data_dir`**, **`raw_pay_dir`**: Data locations (overridable via environment variables)
- **`force_rerun`**: Set `TRUE` to bypass conditional execution checks

Machine-specific paths can be set in `.Renviron` (gitignored):

```
WHEEL_DATA_DIR=data
WHEEL_OUT_DIR=out
WHEEL_LOG_DIR=logs
```

## Conditional Execution

The pipeline uses `needs_rerun()` from `utils/logging.R` to skip steps whose inputs, code, and outputs have not changed. A step re-runs when:

- No previous log exists, or previous run failed
- The script file was modified since the last successful run
- Any dependency file was modified since the last successful run
- Any expected output file is missing

Set `CONFIG$force_rerun <- TRUE` to override and re-run everything.

## Data

Raw data directories are date-stamped (`YYYYMMDD_description/`) and gitignored. Stata scripts (01_01 through 01_03) produce intermediate CSVs in `data/` that the R pipeline takes as given. Key intermediate files:

| File | Source |
|------|--------|
| `data/01_03_pre_network_{30,90,180}.csv` | Stata 01_03 (or R port) |
| `data/prep_01_panel_working.rds` | `prep_01_mk_network.R` |
| `data/00_02_estimation_sample.rds` | `00_02_mk_estimation_sample.R` |

## Requirements

- R >= 4.3.0
- Key packages: `data.table`, `fixest`, `circular`, `ClusTorus`, `sf`, `osmdata`, `ggplot2`, `lubridate`, `stringr`, `almanac`, `tidygeocoder`, `haven`
