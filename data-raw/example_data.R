## Script to document the example datasets bundled in inst/extdata/
## Run this script (source() from the data-raw/ directory) to regenerate
## documentation, not the files themselves.
##
## The example files in inst/extdata/ are hand-crafted minimal datasets used
## in examples and tests.  They are not generated from real experimental data,
## except where noted.
##
## Files:
##   inst/extdata/example.asc        – Minimal EyeLink ASC file with
##                                     samples and events for one trial.
##                                     Monocular (left eye), synthetic data.
##   inst/extdata/example_samples.csv – Raw gaze samples in CSV format.
##   inst/extdata/example_roi.csv     – Word ROI definitions for two trials.
##   inst/extdata/sub_1_example.asc  – Small excerpt from a real binocular
##                                     EyeLink recording made with OpenSesame.
##                                     Contains:
##                                     - File header and calibration/validation
##                                       summary lines
##                                     - TRIAL/ITEM/WORD word-boundary messages
##                                       (OpenSesame format)
##                                     - Binocular gaze samples (7 columns:
##                                       time, xl, yl, pl, xr, yr, pr)
##                                     - EyeLink fixation/saccade events
##                                     Suitable for testing read_asc() with
##                                     binocular data and the new word-boundary
##                                     and calibration parsing features.

## No usethis::use_data() call is needed here because these are raw text files
## placed directly in inst/extdata/, not .rda files.

message("Example extdata files are located in inst/extdata/ and do not need")
message("to be regenerated.  See file headers for format descriptions.")
