# CLAUDE.md

## Project Overview

**urbnindicators** is an R package that provides analysis-ready data from the American Community Survey (ACS). The package provides standardized variables, meaningful names, accurate calculations of derived measures, and proper handling of margins of error.

## Key Development Commands

### Package Development
- **Install package**: `renv::install(".")`
- **Load package for development**: `devtools::load_all()`
- **Build package**: `devtools::build()`
- **Check package**: `devtools::check()`
- **Generate documentation**: `devtools::document()`

## Architecture Overview

### Core Functions
- **`compile_acs_data()`**: Main function that retrieves ACS data and calculates derived indicators
- **`internal_compute_acs_variables()`**: Internal function that calculates hundreds of derived variables (percentages, ratios, etc.)
- **`list_acs_variables()`**: Returns the standardized variable list for a given year
- **`generate_codebook()`**: Creates documentation for all calculated variables

### Statistical Functions
- **`calculate_cvs()`**: Calculates coefficients of variation and standard errors
- **`se_simple()`**, **`se_sum()`**, **`se_proportion_ratio()`**: Handle margin of error calculations following Census Bureau guidelines
- **`calculate_segregation_metrics()`**: Computes multi-group segregation using Mutual Information Index
- **`safe_divide()`**: Division helper that returns 0 when divisor is 0

### Data Processing
- **`make_pretty_names()`**: Converts ACS variable codes to human-readable names
- **`filter_variables()`**, **`select_variables_by_name()`**: Variable selection utilities

### Dependencies
- Built on top of `tidycensus` for Census Bureau API access
- Standard tidyverse packages: `dplyr`, `tidyr`, `stringr`, `purrr`
- Spatial data support via `sf` and `tigris`

## Coding Conventions

### Style
- Follow tidyverse style and use tidyverse packages over base R when possible
- Always include the package name when calling a function, e.g., `dplyr::filter()` (not just `filter()`)

## Important Implementation Details

### Variable Naming Convention
Variables follow a consistent pattern: `[topic]_[subtopic]_[measure]_[unit]`
- Example: `race_nonhispanic_white_alone_percent`
- Raw estimates, mostly counts, don't have a unit suffix
- Percentages end with `_percent`
- Margins of error end with `_M`
- Standard errors (SEs) end with `_SE`
- Coefficients of variation (CVs) end with `_CV`

## File Structure
- `R/compile_acs_data.R`: Main data compilation function
- `R/calculate_cvs.R`: Statistical calculations for margins of error
- `R/calculate_segregation_metrics.R`: Segregation analysis functions
- `R/generate_codebook.R`: Documentation generation
- `R/list_acs_variables.R`: Variable definitions and selection
- `tests/testthat/`: Test suite with coverage for core functions
- `vignettes/`: Package documentation including design philosophy and usage examples