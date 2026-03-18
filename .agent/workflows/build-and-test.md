---
description: Build, document, and test the R package
---

This workflow ensures the package is in a healthy state.

1. Install dependencies (if needed)
// turbo
run_command("Rscript -e 'devtools::install_deps()'", cwd="/Users/bernhardangele/Documents/Experiments/fixated")

2. Update documentation and NAMESPACE
// turbo
run_command("Rscript -e 'devtools::document()'", cwd="/Users/bernhardangele/Documents/Experiments/fixated")

3. Run unit tests
// turbo
run_command("Rscript -e 'devtools::test()'", cwd="/Users/bernhardangele/Documents/Experiments/fixated")

4. Run full CRAN checks
// turbo
run_command("Rscript -e 'devtools::check()'", cwd="/Users/bernhardangele/Documents/Experiments/fixated")
