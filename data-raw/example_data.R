## Script to document the example datasets bundled in inst/extdata/
## Run this script (source() from the data-raw/ directory) to regenerate
## documentation, not the files themselves.
##
## The example files in inst/extdata/ are hand-crafted minimal datasets used
## in examples and tests.  They are not generated from real experimental data.
##
## Files:
##   inst/extdata/example.asc        – Minimal EyeLink ASC file with
##                                     samples and events for one trial.
##   inst/extdata/example_samples.csv – Raw gaze samples in CSV format.
##   inst/extdata/example_roi.csv     – Word ROI definitions for two trials.

## No usethis::use_data() call is needed here because these are raw text files
## placed directly in inst/extdata/, not .rda files.

message("Example extdata files are located in inst/extdata/ and do not need")
message("to be regenerated.  See file headers for format descriptions.")
