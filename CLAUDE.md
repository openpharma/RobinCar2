# CLAUDE.md

Project-level instructions for Claude Code working on the `RobinCar2` package.

The full agent brief — package overview, conventions, dependencies, schemas,
pitfalls — lives in [.github/copilot-instructions.md](.github/copilot-instructions.md).
Read it first; the points below are Claude-specific and complement it rather
than replace it.

## Environment (this Linux server)

R and Quarto are only available via the Lmod `ml` module system. Always
invoke through `bash -lc` so the `ml` shell function is loaded.

```bash
# R
bash -lc 'ml R/4.4.1-gfbf-2023a && Rscript -e "devtools::test()"'

# R + Quarto (for vignette / design-doc rendering)
bash -lc 'ml R/4.4.1-gfbf-2023a quarto-cli/1.8.25 && quarto render design/survival/design_survival.qmd'
```

Running `Rscript` or `quarto` directly (without `ml`) fails with
"command not found".

## Working agreements

- **Default to `devtools` for local checks**: `devtools::test()`,
  `devtools::document()`, `devtools::check()`. CI runs `R CMD check --as-cran`
  via the `insightsengineering/r.pkg.template` workflows.
- **Don't modify `NAMESPACE` by hand** — it's roxygen-generated. Edit roxygen
  tags in `R/*.R`, then `devtools::document()`.
- **Don't touch `README.md` directly** — edit `README.Rmd` and re-render with
  `devtools::build_readme()`. Pre-commit's `readme-rmd-rendered` hook enforces this.
- **Snapshot tests**: when an `_snaps/*.md` file changes, eyeball the diff
  before `testthat::snapshot_accept()`. Many snapshots capture printed
  coefficient matrices that are sensitive to ordering and rounding.
- **Pre-commit is the single source of truth for style** — `air`, `styler`,
  `lintr`, spelling, `deps-in-desc`, and `roxygenize` all run there.
- **Don't reintroduce `sp`** — it was renamed to `sr` (simple randomization)
  in 0.2.2.
