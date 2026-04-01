# The Wheel of (Over)Time

Replication code for **"The Wheel of (Over)Time"** by [Jacob Kohlhepp](https://www.jkohlhepp.com) and [Robert McDonough](https://robmcdonough.com).

> In the United States public sector, there are many examples where overtime is allocated informally, and overtime earnings are concentrated among a small number of government workers. Is this government inefficiency driven by insider influence, or an efficient reflection of worker preferences? We study the Los Angeles Department of Transportation, where a few traffic officers earned more than $100,000 in overtime over 1.5 years. A constantly rotating list called "the wheel" assigns overtime equally initially, but officers are allowed to informally trade. Using novel daily personnel records, we recover the position of the wheel as well as the time-varying network of potential relationships between officers. Officers are several times more likely to work overtime when they are well-connected to coworkers likely endowed with overtime. Nevertheless, overtime inequality primarily reflects underlying differences in preferences. Informal trading achieves 93.8% of the maximum possible allocative efficiency, or $4.15 million more than random assignment.

Paper: [wheel_overtime_draft.pdf](https://www.jkohlhepp.com/pdf/wheel_overtime_draft.pdf)

## Quick Start

```r
# 1. Open the RStudio project
#    File > Open Project > productivity.Rproj

# 2. Run data preparation (raw processing + network panels + map)
source("run_prep_data.R")

# 3. Run full analysis pipeline
source("run_analysis.R")
```

## Project Structure

```
wheel_code/
â”œâ”€â”€ config.R                          # Centralized configuration (paths, parameters)
â”œâ”€â”€ utils/logging.R                   # Logging and conditional re-run logic
â”‚
â”œâ”€â”€ run_prep_data.R                   # Orchestrator: data preparation (01_01 -- 01_07)
â”œâ”€â”€ 01_01_process_weather.R           # Weather CSV â†’ .dta
â”œâ”€â”€ 01_02_process_holidays.R          # Holidays CSV â†’ .dta
â”œâ”€â”€ 01_03_mk_working.R               # Split raw data into employee/workers_comp/pay
â”œâ”€â”€ 01_04_mk_expanded_pay.R           # Expand pay to daily panel + merge weather/holidays
â”œâ”€â”€ 01_05_mk_pre_network.R            # Build exposure matrices (30/90/180/1000-day windows)
â”œâ”€â”€ 01_06_mk_network.R                # Network panels (30/90/180-day windows)
â”œâ”€â”€ 01_07_mk_map.R                    # Enforcement districts map
â”‚
â”œâ”€â”€ run_analysis.R                    # Orchestrator: analysis pipeline (02_01 -- 07_02)
â”œâ”€â”€ 02_01_mk_estimation_sample.R      # Build main estimation sample
â”œâ”€â”€ 03_01_facts.R                     # Descriptive facts and figures
â”œâ”€â”€ 03_02_lag_check.R                 # Lag structure validation
â”œâ”€â”€ 03_03_termination_did.R           # Event study: terminations
â”œâ”€â”€ 03_04_new_hire.R                  # Event study: new hires
â”œâ”€â”€ 03_05_fmla.R                      # Event study: peer FMLA leave
â”œâ”€â”€ 03_06_own_fmla.R                  # Event study: own FMLA leave
â”œâ”€â”€ 03_07_bereave.R                   # Event study: peer bereavement
â”œâ”€â”€ 03_08_own_bereave.R               # Event study: own bereavement
â”‚
â”œâ”€â”€ 04_01_estimate.R                  # Main logit/probit estimation
â”œâ”€â”€ 04_02_estimate_many.R             # Time-varying estimates
â”œâ”€â”€ 05_01_display.R                   # Estimation result figures
â”œâ”€â”€ 05_02_validate_valuations.R       # Validate against special events
â”œâ”€â”€ 05_03_cartel_age.R                # Heterogeneity by age/tenure
â”œâ”€â”€ 05_04_decomp_pref_network.R       # Preference vs. network decomposition
â”œâ”€â”€ 05_05_labor_supply.R              # Labor supply analysis
â”‚
â”œâ”€â”€ 06_01_sim_frontier.R              # Simulation: efficiency-equity frontier
â”œâ”€â”€ 06_02_sim_random.R                # Simulation: random allocation
â”œâ”€â”€ 06_03_auction_sim.R               # Simulation: auction mechanisms
â”œâ”€â”€ 06_04_sim_informal.R              # Simulation: informal trading
â”œâ”€â”€ 06_05_sim_informal_reverse.R      # Simulation: reverse-seniority trading
â”œâ”€â”€ 06_06_sim_informal_perfect.R      # Simulation: perfect-information trading
â”œâ”€â”€ 07_01_heatmap.R                   # Simulation heatmap visualization
â”œâ”€â”€ 07_02_compare_sims.R              # Cross-simulation comparison
â”‚
â”œâ”€â”€ legacy/stata/                     # Original Stata scripts and verification tools
├── legacy/did2s/                     # Gardner (2022) did2s scripts (vcov bug with fixest 0.11.x)
├── legacy/cs/                        # Callaway & Sant'Anna (2021) scripts (too slow for daily panels)
â”œâ”€â”€ data/                             # Intermediate data files (gitignored)
â”œâ”€â”€ out/figures/                      # Output figures (gitignored)
â”œâ”€â”€ out/tables/                       # Output tables (gitignored)
â”œâ”€â”€ logs/                             # Execution logs (gitignored)
â”œâ”€â”€ 20170803_payworkers_comp/         # Raw pay/workers comp data (gitignored)
â”œâ”€â”€ 20190811_weather/                 # Raw weather data (gitignored)
â”œâ”€â”€ 20190814_fed_holidays/            # Raw holiday data (gitignored)
â””â”€â”€ 20250311_ladot_enforcement_districts/  # District shapefiles (gitignored)
```

## Pipeline

Scripts are numbered to reflect execution order. Each tier depends on the outputs of the previous tier.

### Data Preparation

Run via `source("run_prep_data.R")`. Steps are skipped automatically when inputs and code have not changed.

| Step | Script | Output |
|------|--------|--------|
| 01_01 | `01_01_process_weather.R` | `data/weather_daily.dta` |
| 01_02 | `01_02_process_holidays.R` | `data/holidays.dta` |
| 01_03 | `01_03_mk_working.R` | `data/employee_data.dta`, `data/pay_data.dta`, `data/workers_comp.dta` |
| 01_04 | `01_04_mk_expanded_pay.R` | `data/working_expanded.dta` |
| 01_05 | `01_05_mk_pre_network.R` | `data/01_05_pre_network_{30,90,180,1000}.csv` |
| 01_06 | `01_06_mk_network.R` | `data/01_06_panel_working{_30,_180}.rds` |
| 01_07 | `01_07_mk_map.R` | `out/figures/01_07_la_street_map.png` |

### Analysis

Run via `source("run_analysis.R")`.

| Tier | Scripts | Depends On |
|------|---------|------------|
| 2 | `02_01_mk_estimation_sample.R` | 01_06 output |
| 3 | `03_01_facts.R`, `03_02` -- `03_08` (TWFE event studies) | Tier 2 |
| 3b | `03_03` -- `03_08` sunab (Sun & Abraham 2021), `03_09` diagnostic | Tier 2 |
| 4 | `04_01_estimate.R` (main estimation) | Tier 2 |
| 5 | `05_01` -- `05_05` (estimation analysis) | Tier 4 |
| 6 | `06_01` -- `06_06` (simulations) | Tier 4 |
| 7 | `07_01_heatmap.R`, `07_02_compare_sims.R` | Tier 6 |

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

Raw data directories are date-stamped (`YYYYMMDD_description/`) and gitignored. Key intermediate files:

| File | Source |
|------|--------|
| `data/01_05_pre_network_{30,90,180}.csv` | `01_05_mk_pre_network.R` |
| `data/01_06_panel_working.rds` | `01_06_mk_network.R` |
| `data/02_01_estimation_sample.rds` | `02_01_mk_estimation_sample.R` |

## Requirements

- R >= 4.3.0
- Key packages: `data.table`, `fixest`, `circular`, `ClusTorus`, `sf`, `osmdata`, `ggplot2`, `lubridate`, `stringr`, `almanac`, `tidygeocoder`, `haven`

