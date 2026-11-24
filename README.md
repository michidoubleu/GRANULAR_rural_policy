# Rural Population Change in Europe: The Role of Policy, Economic Structure, and Space

Replication code for the paper by Michael Wögerer, Dejana Golic and Tamas Krisztin
(International Institute for Applied Systems Analysis, IIASA), submitted to the
*Journal of Agricultural Economics*.

The paper relates migration-induced population change in 1,137 European NUTS 3 regions
(2011–2021) to economic structure, geography and exposure to CAP Pillar I, CAP Pillar II
and Cohesion Policy (ESIF) funding, using Bayesian spatial autoregressive (SAR) and spatial
Durbin (SDM) models estimated with the R package
[`estimateW`](https://cran.r-project.org/package=estimateW).

This branch (`JAE_submission`) contains the code exactly as used for the submitted version,
cleaned up so that it runs on a single computer (the original runs used the IIASA HTCondor
cluster). The model specification, variable construction and priors are unchanged.

## Quick start

```r
# R >= 4.1 (tested with R 4.5.3)
install.packages("renv")
renv::restore()          # installs the package versions in renv.lock
source("main.R")         # or from a shell:  Rscript main.R
```

`main.R` runs the whole pipeline; each step can be switched off at the top of the file:

| Step | Script | Output |
|---|---|---|
| Boundary files (downloaded) | `Code/get_geo_data.R` | `data/geo/` |
| Estimation data from raw data | `Code/load_data.R` | `inputs/*.rds` |
| Model grid (14 models) | `main.R` | `inputs/model_grid.RData` |
| MCMC estimation, one R process per model, in parallel | `Code/estimateW_estim.R` (uses `Code/prepare_data.R`, `Code/knn.R`) | `output/GRANULAR_estim_local.*.RData` |
| Posterior summaries | `Code/summarize_results.R` | `economic_res/model_res_local_includingSD.rds` |
| Convergence diagnostics | `Code/convergence_diagnostics.R` | Table A3, Figure 6 |
| Results tables | `Code/write_res_latex.R` (uses `Code/results_processing.R`, `Code/write_latextable.R`) | Tables 2, A1, A2 |
| Figures | `Code/Figure1_typology_map.R`, `Code/Figure2_density_growth_corr.R`, `Code/Figure3_growth_maps.R`, `Code/policy_plots.R` | Figures 1–5 |

Tables are written to `tables/` and figures to `plots/`. All figures share the colour scheme in
`Code/figure_palette.R`.

**Runtime.** With the settings of the paper (2,000 burn-in and 8,000 retained draws), one model
takes roughly 4–5 hours, so all 14 models take one to two days on a computer with several cores
(`N_CORES` in `main.R`). Fewer draws (`save.draws`, `burn.draws`) give a quick test run.

## Reproducing the tables without the restricted data

`economic_res/model_res_300_includingSD.rds` holds the posterior summaries (quantiles and
standard deviations) of the estimation run reported in the paper (run 300), and
`economic_res/geweke_diagnostics.rds` its convergence diagnostics. Tables 2, A1 and A2 can be
regenerated from these files alone:

```r
source("Code/write_res_latex.R")   # -> tables/bigsar.tex, bigsar_sdm.tex, bigsar_wspec.tex
```

`tables/` contains the tables as they appear in the submitted manuscript.

## Data

The data are **not** included in this repository. The CAP and ESIF payment data come from the
CATS database; they cannot be redistributed, and access has to be requested from the European
Commission. All other inputs are publicly available. Because the prepared estimation data and
the posterior draws contain the payment data, they are not shared either.

Put the raw data files in a folder `raw_data/` (or set the environment variable
`GRANULAR_RAW_DATA` to another folder). `Code/load_data.R` expects:

| File | Content | Source |
|---|---|---|
| `pop_NUTS3.csv` | Population 2011 and 2021 aggregated from the 1 km population grid to NUTS 2016 regions, with the urban–rural typology. Columns: NUTS 3 code, (unused), (unused), country code, typology (`predominantly urban` / `intermediate` / `predominantly rural`), (unused), (unused), population 2011, population 2021 | Eurostat (GISCO) |
| `Total_live_births_NUTS3.csv` | Live births by NUTS 3 region and year (`geo`, `TIME_PERIOD`, `OBS_VALUE`) | Eurostat |
| `Total_deaths_NUTS3.csv` | Deaths by NUTS 3 region and year (`geo`, `TIME_PERIOD`, `OBS_VALUE`) | Eurostat |
| `nuts3_migration_2016.csv` | Reported net migration by NUTS 3 region, 2011–2021 (`NUTS_ID`, one column per year); descriptive only | Eurostat |
| `economic_data.csv` | GDP and gross value added by NACE sector (`NUTS`, `year`, `gdp`, `gva_A`, `gva_B.E`, …), first column is a row index | ARDECO |
| `Employment_per_capita_NUTS3.csv` | Employment per capita (`TERRITORY_ID`, `LEVEL_ID`, `X2011`, `X2021`) | ARDECO |
| `Health_and_educ_accessibility_NUTS3.csv` | Drive times to health and education services (`NUTS_ID`, `health_2020_n1`, `educ_2020_n1`, …), first column is a row index | Eurostat (GISCO) |
| `NUTS3_harm_ESIF_2024-06-10.csv` | Cohesion Policy (ESIF) payments by NUTS 3 region, year and category (`year`, `geo`, `Category`, `values`) | CATS database (see paper, Table 1) |
| `CAPpayments_processed.csv` | CAP payments by NUTS 3 region and measure (`geo`, `type`, `values`, …) | CATS database, **access on request** |
| `Metro-regions-NUTS-2016.xlsx` | Metropolitan regions (sheet 2: `NUTS_ID`, `MREG_CODE`) | Eurostat |

NUTS 2016 boundaries (incl. the coastal and mountain typologies) and the country background map
are downloaded automatically from Eurostat GISCO by `Code/get_geo_data.R`.

## Reproducibility notes

- With the raw data used for the paper, `Code/load_data.R` and `Code/prepare_data.R` rebuild
  the dependent variable, regressors and spatial weight matrices of all 14 models
  **identically** to those stored with the draws of run 300 (checked on 2026-11-24).
- The draws of run 300 were produced without a fixed random seed, so a new estimation
  reproduces the published results up to Monte Carlo error. `Code/estimateW_estim.R` now sets
  a seed per model, so re-runs of this code are reproducible.
- `renv.lock` lists the package versions with which the data, tables and figures were
  rebuilt and checked (R 4.5.3, `estimateW` 0.1.0). The models in the paper were estimated
  with R 4.1.1 and `estimateW` 0.0.1; that environment is recorded in
  `renv_estimation_R4.1.1.lock`.

## Funding

This research was conducted as part of the GRANULAR project, funded by the European Union's
Horizon Europe research and innovation programme under grant agreement No. 101061068. Data and
methodological infrastructure were also supported by the LAMASUS project (Horizon Europe, grant
agreement No. 101060423).

## Contact

Michael Wögerer, IIASA, wogerer@iiasa.ac.at
