---
description: Add a new feature or function to the package
---

This workflow follows the standard R package development lifecycle.

1. Create a new R script or add to existing one in `R/`
   - Use `usethis::use_r("feature_name")` if it's a new component.

2. Add roxygen2 documentation to the new function(s).

3. Create a test file
   - Use `usethis::use_test("feature_name")`.

4. Build and Verify
   - Run the [build-and-test.md](~/projects/fixated/.agent/workflows/build-and-test.md) workflow.