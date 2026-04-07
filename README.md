# The Wheel of (Over)Time

Replication code for **"The Wheel of (Over)Time"** by [Jacob Kohlhepp](https://www.jkohlhepp.com) and [Robert McDonough](https://robmcdonough.com).

> In the United States public sector, there are many examples where overtime is allocated informally, and overtime earnings are concentrated among a small number of government workers. Is this government inefficiency driven by insider influence, or an efficient reflection of worker preferences? We study the Los Angeles Department of Transportation, where a few traffic officers earned more than $100,000 in overtime over 1.5 years. A constantly rotating list called "the wheel" assigns overtime equally initially, but officers are allowed to informally trade. Using novel daily personnel records, we recover the position of the wheel as well as the time-varying network of potential relationships between officers. Officers are several times more likely to work overtime when they are well-connected to coworkers likely endowed with overtime. Nevertheless, overtime inequality primarily reflects underlying differences in preferences. Informal trading achieves 93.8% of the maximum possible allocative efficiency, or $4.15 million more than random assignment.

Paper: [wheel_overtime_draft.pdf](https://www.jkohlhepp.com/pdf/wheel_overtime_draft.pdf)

## Quick Start

```r
# 1. Open the RStudio project
#    File > Open Project > wheel.Rproj

# 2. Run everything end to end
source("run_all.R")

# 3. Or run the two master pipelines separately
source("run_prep_data.R")
source("run_analysis.R")
```

## Main Entry Points

- `run_prep_data.R`: builds the intermediate data products and network panels.
- `run_analysis.R`: runs the estimation, descriptive analysis, event studies, the active `06_01`-`06_05` simulation block, and the simulation-comparison scripts.
- `run_all.R`: runs `run_prep_data.R` and then `run_analysis.R`.

## Logging and Conditional Execution

Logging lives in `utils/logging.R`.

- Every numbered script writes its own log in `logs/`.
- The three master runners also write logs: `run_prep_data.log`, `run_analysis.log`, and `run_all.log`.
- Scripts are skipped automatically when their previous log is successful, their expected outputs exist, and none of their dependencies are newer than the logged completion time.
- Set `CONFIG$force_rerun <- TRUE` to bypass the skip logic.

## Pipeline Overview

### Data Preparation

Run via `source("run_prep_data.R")`.

| Step | Script | Main outputs |
|------|--------|--------------|
| 01_01 | `01_01_process_weather.R` | `data/weather_daily.rds` |
| 01_02 | `01_02_process_holidays.R` | `data/holidays.rds` |
| 01_03 | `01_03_mk_working.R` | `data/employee_data.rds`, `data/pay_data.rds`, `data/workers_comp.rds` |
| 01_04 | `01_04_mk_expanded_pay.R` | `data/working_expanded.rds`, `data/01_04_fornetwork.rds` |
| 01_05 | `01_05_mk_pre_network.R` | `data/01_05_pre_network_{30,90,180,1000}.csv` |
| 01_06 | `01_06_mk_network.R` | `data/01_06_panel_working.rds`, `data/01_06_panel_working_30.rds`, `data/01_06_panel_working_180.rds` |
| 01_07 | `01_07_mk_map.R` | `out/figures/01_07_la_street_map.png` |

### Analysis

Run via `source("run_analysis.R")`.

| Tier | Scripts | Depends on |
|------|---------|------------|
| 2 | `02_01_mk_estimation_sample.R` | `01_06` outputs |
| 3 | `03_01_facts.R`, `03_02_lag_check.R`, `03_03`-`03_08` event studies | Tier 2 |
| 3b | Existing modern DiD scripts (`*_did2s.R`, `*_sunab.R`, `*_cs.R`) | Tier 2 |
| 4 | `04_01_estimate.R`, `04_02_estimate_many.R` | Tier 2 |
| 5 | `05_01_display.R`, `05_02_validate_valuations.R`, `05_03_cartel_demographics.R`, `05_04_decomp_pref_network.R`, `05_05_labor_supply.R` | Tier 4 |
| 6 | `06_01`-`06_05` simulation scripts | Tier 4 |
| 7 | `07_01_heatmap.R`, `07_02_compare_sims.R` | Tier 6 |

Legacy note: `06_99_sim_frontier.R` is kept for optional manual runs, but it is no longer part of `run_analysis.R`. `07_02_compare_sims.R` will use `data/06_99_sim_frontier.rds` only when that file already exists.

## Configuration

Project-wide settings live in `config.R`.

Important fields include:

- `log_dir`, `data_dir`, `output_dir`
- `network_windows` and `network_window_default`
- `estimation_start` and `estimation_end`
- `force_rerun` and `verbose_logging`

Machine-specific locations can be overridden with environment variables such as:

```text
WHEEL_DATA_DIR=data
WHEEL_OUT_DIR=out
WHEEL_LOG_DIR=logs
```

## Project Layout

- `config.R`: central configuration and helper utilities.
- `utils/logging.R`: logging and skip logic.
- `run_prep_data.R`, `run_analysis.R`, `run_all.R`: pipeline orchestrators.
- `data/`: intermediate data files.
- `out/figures/`, `out/tables/`: generated outputs.
- `logs/`: step-level and master-runner logs.
- date-stamped raw data directories such as `20170803_payworkers_comp/` and `20190811_weather/`.

## Requirements

- R >= 4.3.0
- Core packages include `data.table`, `fixest`, `circular`, `ClusTorus`, `sf`, `osmdata`, `ggplot2`, `lubridate`, `stringr`, `almanac`, `tidygeocoder`, and `haven`
