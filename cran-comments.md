
## R CMD check results

### Local Windows check
0 errors | 0 warnings | 0 notes

Checked using devtools::check(cran = TRUE, manual = TRUE).

### Windows R-devel (win-builder)
0 errors | 0 warnings | 1 note

The NOTE concerns:
- New submission (first CRAN release).
- "RStudio" flagged as a possible spelling error. RStudio is the correct software name.

### R-hub cross-platform testing
R-devel GitHub Actions jobs completed successfully on:
- Linux
- Windows
- macOS

The Linux check initially produced one NOTE because the
.github directory was included in the package bundle.

This has been corrected by adding .github to .Rbuildignore.
The subsequent local CRAN check completed with 0 errors,
0 warnings, and 0 notes.

## Submission

This is the first CRAN submission of aindpackin.


