# fixated Agent Instructions

This repository contains the `fixated` R package. Development occurs within a pre-configured Docker environment (based on `rocker/verse`) to ensure reproducibility.

## Environment & Infrastructure

- **R Version**: Latest compatible with `rocker/verse` (as of 2026-03-13 snapshot).
- **CRAN Snapshot**: [2026-03-13](https://packagemanager.posit.co/cran/2026-03-13).
- **CmdStan**: Installed at `/opt/cmdstan/cmdstan-2.38.0`.
  - Use `cmdstanr` package to interface with it.
  - The path is automatically configured in the environment (`CMDSTAN` env var).
- **System Tools**: `latexdiff` is available for comparing LaTeX versions of documents (e.g., `papaja` manuscripts).
- **TeX Live**: Minimal installation with additional packages (`apa7`, `fontawesome5`, etc.) via `tinytex`.

## Repository Structure

Standard R package layout:
- `R/`: R source files
- `tests/testthat/`: Unit tests
- `man/`: Auto-generated documentation (via `roxygen2`)
- `DESCRIPTION`: Package metadata
- `NAMESPACE`: Exported functions (auto-generated)

## Key R Packages

The environment includes a wide range of packages for statistical modeling and reporting:
- **Bayesian Modeling**: `brms`, `cmdstanr`, `rstan`, `tidybayes`, `BayesFactor`.
- **Mixed-Effect Models**: `lme4`, `lmerTest`, `afex`, `marginaleffects`.
- **Formatting & Reporting**: `gt`, `gtsummary`, `kableExtra`, `papaja`, `modelsummary`.
- **Data Manipulation & Visualization**: `tidyverse` (via base image), `ggpubr`, `patchwork`, `cowplot`.
- **Performance & Diagnostics**: `performance`, `see`, `bayestestR`.

## Coding Conventions

- Follow the [tidyverse style guide](https://style.tidyverse.org/).
- Document all exported functions using `roxygen2` comments (`#'`).
- Use `snake_case` for names.
- Keep functions small and focused.
- Use `testthat` for all new functionality.
- Use the `usethis` package for boilerplate (e.g., `usethis::use_test()`).

## Workflow Summary

1.  **Develop**: Edit/Add files in `R/`.
2.  **Document**: Run `devtools::document()`.
3.  **Test**: Run `devtools::test()`.
4.  **Check**: Run `devtools::check()` before any PR.
5.  **Manuscripts**: If working on `papaja` documents, ensure `tinytex` is used for rendering.
