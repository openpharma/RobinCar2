# Copilot / Claude Instructions for RobinCar2

This file is the shared agent brief. It is used by GitHub Copilot and is also
referenced from `CLAUDE.md` at the repo root so Claude Code picks it up.

## Project Overview

`RobinCar2` is an R package for **robust covariate adjustment** in randomized
clinical trials. It provides methods for estimating and inferring treatment
effects under stratified / covariate-adaptive randomization, aligned with the
[FDA's final guidance on covariate adjustment](https://www.regulations.gov/docket/FDA-2019-D-0934).

Maintainer: Liming Li (AstraZeneca). Repo lives under
[openpharma/RobinCar2](https://github.com/openpharma/RobinCar2). Released to
CRAN; current dev version is `0.2.2.9000`.

### Core User-Facing Functions

| Function | Purpose |
|----------|---------|
| `robin_lm()` | Linear model covariate adjustment (ANCOVA) |
| `robin_glm()` | GLM-based covariate adjustment (Gaussian / binomial / Poisson / negative binomial via `MASS::glm.nb`) |
| `robin_surv()` | Stratified, covariate-adjusted log-rank test and log hazard ratio |
| `predict_counterfactual()` | Counterfactual marginal mean prediction (driver behind `robin_lm`/`robin_glm`) |
| `treatment_effect()` | Compute contrast + variance from a `prediction_cf` object |
| `vcovG()`, `vcovHC()` | Robust variance estimators |

### Key Concepts

- **Treatment formula**: `treatment ~ schema(strata)` syntax,
  e.g. `treatment ~ s1` (defaults to `sr`), `treatment ~ pb(s1, s2)`,
  `treatment ~ ps(s1)`.
- **Randomization schemas** (parsed in `h_get_vars()` via `terms(..., specials = ...)`):
  - `sr` — simple randomization (was `sp` pre-0.2.2; do not reintroduce `sp`)
  - `pb` — permuted-block
  - `ps` — Pocock–Simon
- **Variance estimators**:
  - `vcovG` (default, robust ANHECOVA-style)
  - `vcovHC` (Huber–White; **only valid** for linear model, no
    treatment×covariate interactions, `difference` contrast,
    Gaussian family — enforced in `robin_lm` and `robin_glm`)
- **Contrasts** (string shortcuts on `robin_glm`):
  `"difference"`, `"risk_ratio"`, `"odds_ratio"`, `"log_risk_ratio"`,
  `"log_odds_ratio"`. Bare `risk_ratio` / `odds_ratio` emit a warning
  recommending the log version. A user-supplied `contrast` function with
  signature `function(x, y)` is also accepted; if no `contrast_jac` is given,
  `eff_jacob()` will derive it numerically with `numDeriv::grad`.
- **Return shape** of `robin_lm` / `robin_glm`: a `robin_output` list with
  `$marginal_mean` (class `prediction_cf`) and `$contrast` (class
  `treatment_effect`). `robin_surv` returns class `surv_effect`.

## Project Structure

```text
R/
├── RobinCar2-package.R       # package-level roxygen, imports
├── robin_lm.R                # robin_lm() + h_interaction()
├── robin_glm.R               # robin_glm() + print.robin_output
├── predict_couterfactual.R   # predict_counterfactual() (note: filename typo, kept)
├── prediction_cf.R           # print/confint methods for prediction_cf
├── treatment_effect.R        # treatment_effect() S3, contrast helpers (h_diff, h_risk_ratio, ...)
├── variance_anhecova.R       # vcovG() + h_get_erb (Erb correction for pb)
├── variance_hc.R             # vcovHC() wrapping sandwich::vcovHC
├── bias.R                    # bias() prediction-bias correction; h_unbiased_means_across_strata
├── find_data.R               # find_data() S3 to recover data from a fit
├── utils.R                   # h_get_vars, randomization_schema, contrast/pairwise/against_ref, h_confint, h_first_fct_nested_in_second
├── survival.R                # robin_surv() + dispatch to survival comparison helpers
├── surv_effect.R             # print/confint/table methods for surv_effect
├── survival_cov_adj.R        # covariate-adjusted survival helpers
├── survival_score.R          # log-rank score functions (no-strata/no-cov, strat, cov, strat_cov)
└── data.R                    # roxygen for glm_data / surv_data

data/                        # glm_data.rda, surv_data.rda
data-raw/                    # create_glm_data.R, create_surv_data.R (NOT shipped; in .Rbuildignore)
tests/testthat/              # test files + _snaps/ snapshot dir
vignettes/intro.Rmd          # CRAN vignette
vignettes/articles/          # pkgdown-only longer articles (biometric_bulletin, robincar-comparison, robincar-survival, robincar-validate)
design/                      # design notes (code_replicate, package_structure, survival)
```

## Code Style & Conventions

### Formatting

Two formatters are configured — both run on commit:

- `air` (see `air.toml`): line width 120, 2-space indent, persistent line breaks.
- `styler` with `tidyverse_style` via `pre-commit` (`style-files` hook).

Formatting is idempotent across both, so just run whichever is convenient and
let `pre-commit` settle the rest. **`lintr` is also enforced** via
`pre-commit` and CI; defaults plus `line_length_linter(120)` and
`indentation_linter(2)`, with `object_usage_linter` disabled (see `.lintr`).

### Function Naming

- **Public API** — `robin_*()` for trial-analysis entry points
- **Exported helpers** — `h_*()` prefix for contrast / Jacobian functions
  (`h_diff`, `h_risk_ratio`, `h_jac_log_odds_ratio`, ...). Several are
  exported despite the `h_` prefix because users compose them.
- **Internal helpers** — also `h_*()` but with `@keywords internal`
  (e.g. `h_get_vars`, `h_get_erb`, `h_prep_survival_input`,
  `h_first_fct_nested_in_second`)
- **S3 methods** — standard naming (`print.robin_output`,
  `confint.prediction_cf`, `predict_counterfactual.glm`, `find_data.lm`, ...)

### Documentation (roxygen2, markdown enabled)

```r
#' Title (one line)
#'
#' @param formula (`formula`) A formula of analysis.
#' @param data (`data.frame`) Input data frame.
#' @return A `robin_output` list with `marginal_mean` and `contrast`.
#' @export
#' @examples
#' robin_lm(y ~ treatment * s1, data = glm_data, treatment = treatment ~ s1)
```

- Type annotations on `@param` use backticks: `(`formula`)`, `(`data.frame`)`,
  `(`character(1)`)`, `(`flag`)`, `(`count`)`.
- Internal helpers get `@keywords internal`; do not `@export` them.
- Re-run `devtools::document()` after editing roxygen — CI's `roxygen` job will
  auto-update otherwise, but local diffs should match.
- `RoxygenNote: 7.3.3`.

### Assertions

Use `checkmate` (the package `@import`s it):

```r
assert_formula(formula)
assert_data_frame(data)
assert_subset(all.vars(formula), names(data))
assert_function(contrast, args = c("x", "y"))
assert_class(x, "prediction_cf")
```

For multi-shape inputs use `assert(test_string(x), test_function(x), test_null(x))`.

### Testing (testthat 3rd edition)

- File naming: `test-<topic>.R` mirroring the source file.
- Snapshot tests live in `tests/testthat/_snaps/` — update with
  `testthat::snapshot_accept()` only after eyeballing the diff.
- Helper data fixtures are in `tests/testthat/helper-fit.R`
  (`fit_glm`, `fit_lm`, `fit_binom`, `robin_res1`, `robin_res2`).
- Prefer `expect_silent()` for happy paths and `expect_error()` /
  `expect_warning()` with regex for the known guard messages.

## Developer Workflows

### Common Commands

```bash
# Tests
Rscript -e "devtools::test()"

# Documentation (regenerates man/ and NAMESPACE)
Rscript -e "devtools::document()"

# Full check (CI runs --as-cran)
R CMD build .
R CMD check --as-cran RobinCar2_*.tar.gz

# pkgdown site (only used for docs deploy)
Rscript -e "pkgdown::build_site()"

# Format
air format R/
# or via pre-commit
pre-commit run --all-files
```

### Adding a New Feature

1. Skim `design/` for any existing design note on the area.
2. Implement in `R/`. Reuse `h_get_vars()` for treatment-formula parsing and
   `pairwise()` / `custom_contrast()` for treatment-arm comparison structure.
3. Add roxygen, run `devtools::document()`.
4. Add tests under `tests/testthat/test-<name>.R`. If the output is a printed
   matrix or formatted block, prefer a snapshot test.
5. Run `devtools::test()` and `R CMD check --as-cran`. CI also runs SuperLinter,
   spelling, license, version-bump, and roxygen-drift checks.
6. Update `NEWS.md` (under the current `# RobinCar2 0.x.y.9000` heading).

### Key Dependencies (DESCRIPTION)

- `checkmate` — input validation (imported wholesale)
- `numDeriv` — `grad()` for numerical Jacobians
- `MASS` — `negative.binomial()` family + `glm.nb`
- `sandwich` — `vcovHC()` (re-exported with our wrapper of the same name)
- `survival` — `Surv`, `coxph` infrastructure, `untangle.specials`
- `stats`, `utils` — base
- `Suggests`: `testthat (>= 3.0)`, `knitr`, `rmarkdown`

If you add an import, also update `RobinCar2-package.R` (`@importFrom`) and
`DESCRIPTION` Imports/Suggests; the `deps-in-desc` pre-commit hook will
otherwise fail.

## Important Patterns

### Treatment Formula Parsing

`h_get_vars()` (in `R/utils.R`) parses the treatment formula. The randomization
schema is detected via `terms(treatment, specials = randomization_schema$id)`.

```r
h_get_vars(treatment ~ pb(s1, s2))
# list(treatment = "treatment", schema = "pb", strata = c("s1", "s2"))
```

For `robin_surv`, `h_prep_survival_input()` extends this — it also pulls
`time` / `status` from a `Surv()` LHS and analysis-stratification variables
out of `strata()` terms via `survival::untangle.specials()`.

### Counterfactual Prediction Pipeline

`predict_counterfactual.lm` is the workhorse — `predict_counterfactual.glm`
delegates to it. It:

1. Replicates the data once per treatment level, sets the treatment column to
   each level via `gl()`.
2. Builds `model.matrix` and applies `family(fit)$linkinv()` to linear preds.
3. Adds prediction bias via `bias()` — stratum-wise for `ps`, overall otherwise.
4. Calls the chosen `vcov` (string name → `match.fun`, function → used directly).

The bias step is what makes the predictions "unbiased" within randomization
strata for Pocock–Simon.

### Variance Estimation Constraints

- `vcovG` is the default, supports all combinations.
- `vcovHC` is rejected (with a clear `stop()`) when:
  - any treatment×covariate interaction is in `formula` (`h_interaction()` checks)
  - contrast is not `"difference"`
  - family is not Gaussian (so `robin_glm` non-Gaussian → no `vcovHC`)

### Survival Score-Function Dispatch

`robin_surv()` picks one of four helpers based on `(has_strata, has_covariates)`:
`robin_surv_no_strata_no_cov`, `robin_surv_strata`, `robin_surv_cov`,
`robin_surv_strata_cov`. Each wraps a log-rank score function from
`R/survival_score.R` and is driven through `robin_surv_comparison()`, which
runs `h_lr_test_via_score()` (test) and optionally `h_log_hr_est_via_score()`
(estimate via `uniroot`). `contrast = "none"` skips the HR estimation —
useful for cheap simulation studies.

If randomization strata are not adequately covered by the analysis model,
`robin_surv` emits a single warning recommending either covariate adjustment
on `interaction(...)` or a stratified test via `+ strata(...)`. See
`vignettes/articles/robincar-survival.Rmd`.

## Common Pitfalls

- **Do NOT** use `vcovHC` with GLM, treatment-covariate interactions, or
  non-`difference` contrasts.
- **Do NOT** rename / delete `predict_couterfactual.R` to fix the typo —
  it's been like that since release; touching the filename will churn diffs
  and pkgdown URLs.
- **Edit `README.Rmd`**, not `README.md` — the `readme-rmd-rendered` pre-commit
  hook will fail otherwise. Re-render with `devtools::build_readme()`.
- **Run `devtools::document()`** after roxygen edits; the CI roxygen job
  auto-updates but local `NAMESPACE` drift is annoying.
- New schemas (beyond `sr`/`pb`/`ps`) require updating `randomization_schema`
  in `R/utils.R` AND any branching in `vcovG` / `bias` / `predict_counterfactual`.
- `MASS::negative.binomial(NA)` is the sentinel that triggers `MASS::glm.nb`
  inside `robin_glm`; passing a fixed `theta` keeps `glm()`.

## References (cited in FDA guidance)

- Tsiatis et al. (2008) — ANCOVA theory
- Bugni, Canay, Shaikh (2018) — Inference under covariate-adaptive randomization
- Wang et al. (2021) — Model-robust inference
- Ye, Shao, Yi, Zhao (2023); Ye, Shao, Yi (2022) — Stratified randomization
- Rosenblum & van der Laan (2010) — GLM-based adjustment
- Bannick, Shao, Liu, Du, Yi, Ye (2024) — Survival covariate adjustment

See `vignettes/articles/biometric_bulletin.Rmd` for the latest summary article.
