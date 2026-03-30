# Copilot Instructions for RobinCar2

## Project Overview

`RobinCar2` is an R package for **robust covariate adjustment** in randomized clinical trials. It provides methods for estimating and inferring treatment effects under stratified randomization, aligned with the [FDA's final guidance on covariate adjustment](https://www.regulations.gov/docket/FDA-2019-D-0934).

### Core Functionality

| Function | Purpose |
|----------|---------|
| `robin_lm()` | Linear model covariate adjustment (ANCOVA) |
| `robin_glm()` | GLM-based covariate adjustment (logistic, Poisson, negative binomial) |
| `robin_surv()` | Survival analysis with covariate adjustment |

### Key Concepts

- **Treatment formulas**: Use `treatment ~ strata` syntax (e.g., `treatment ~ s1` or `treatment ~ pb(s1)` for permuted-block)
- **Randomization schemas**: `sr` (simple), `pb` (permuted-block), `ps` (Pocock-Simon)
- **Variance estimators**: `vcovG` (default, robust) and `vcovHC` (Huber-White, limited use cases)
- **Contrasts**: `"difference"`, `"risk_ratio"`, `"odds_ratio"`, `"log_risk_ratio"`, `"log_odds_ratio"`

## Project Structure

```plaintext
R/                    # Source code
├── robin_lm.R        # Linear model adjustment
├── robin_glm.R       # GLM adjustment
├── survival.R        # Survival analysis methods
├── utils.R           # Helper functions (h_* prefix)
├── variance_*.R      # Variance estimators
└── treatment_effect.R # Treatment effect calculations

tests/testthat/       # Unit tests (testthat v3)
data-raw/             # Scripts to generate package datasets
design/               # Design documents and specifications
vignettes/            # User-facing tutorials
```

## Code Style & Conventions

### Formatting (air.toml)

- **Line width**: 120 characters
- **Indentation**: 2 spaces
- **Use `air` formatter** for R code formatting

### Function Naming

- **Public API**: `robin_*()` for main user-facing functions
- **Internal helpers**: `h_*()` prefix (e.g., `h_get_vars()`, `h_interaction()`)
- **S3 methods**: Standard naming (e.g., `confint.robin_output`)

### Documentation (roxygen2)

```r
#' Title (one line)
#'
#' @param name (`type`) Description.
#' @param formula (`formula`) A formula of analysis.
#' @return Description of return value.
#' @export
#' @examples
#' robin_lm(y ~ treatment * s1, data = glm_data, treatment = treatment ~ s1)
```

- Use `@keywords internal` for non-exported helper functions
- Include type annotations in param descriptions: `(`type`)`
- Add `@export` tag for public functions

### Assertions

Use `checkmate` for input validation:

```r
assert_formula(formula)
assert_subset(all.vars(formula), names(data))
assert_function(contrast, args = c("x", "y"))
```

### Testing (testthat v3)

- Test file naming: `test-<function_name>.R`
- Use `expect_silent()`, `expect_error()`, `expect_warning()` for behavior tests
- Use snapshot tests in `tests/testthat/_snaps/` for complex outputs

```r
test_that("robin_glm works correctly", {

  expect_silent(
    robin_glm(y ~ treatment * s1, data = glm_data, treatment = treatment ~ s1, contrast = "difference")
  )
})
```

## Developer Workflows

### Common Commands

```bash
# Run tests
Rscript -e "devtools::test()"

# Update documentation (regenerates man/ and NAMESPACE)
Rscript -e "devtools::document()"

# Build and check package
R CMD build .
R CMD check RobinCar2_*.tar.gz

# Build pkgdown site
Rscript -e "pkgdown::build_site()"

# Format code with air
air format R/
```

### Adding a New Feature

1. **Design first**: Review relevant documents in `design/` for architectural context
2. **Implement**: Create/modify files in `R/`, following existing patterns
3. **Document**: Add roxygen2 comments; run `devtools::document()`
4. **Test**: Add tests in `tests/testthat/test-<name>.R`
5. **Validate**: Run `devtools::test()` and `R CMD check`

### Key Dependencies

From `DESCRIPTION`:

- `checkmate` - Input validation
- `numDeriv` - Numerical derivatives (Jacobian)
- `MASS` - Negative binomial GLM
- `sandwich` - Robust variance estimation
- `survival` - Survival analysis

## Important Patterns

### Treatment Formula Parsing

The `h_get_vars()` function extracts treatment, strata, and randomization schema:

```r
# Input: treatment ~ pb(s1, s2)
# Output: list(treatment = "treatment", schema = "pb", strata = c("s1", "s2"))
```

### Return Objects

Main functions return `robin_output` objects with:

- `marginal_mean`: Counterfactual predictions
- `contrast`: Treatment effect estimates

### Variance Estimation

- `vcovG`: General robust variance (recommended)
- `vcovHC`: Huber-White (only for linear models without interactions, difference contrast)

## Common Pitfalls

- **Do NOT** use `vcovHC` with GLM or treatment-covariate interactions
- **Edit `README.Rmd`**, not `README.md` (regenerate with knitr)
- **Run `devtools::document()`** after modifying roxygen2 comments
- **Add new dependencies** to `DESCRIPTION` Imports/Suggests sections

## References

Key papers (cited in FDA guidance):

- Tsiatis et al. (2008) - ANCOVA theory
- Wang et al. (2021) - Model-robust inference
- Ye et al. (2022, 2023) - Stratified randomization
- Bannick et al. (2024) - Survival covariate adjustment
