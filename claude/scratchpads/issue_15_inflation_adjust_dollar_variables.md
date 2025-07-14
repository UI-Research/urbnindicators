# Issue #15: Inflation-adjust dollar-denominated variables

**GitHub Issue**: [#15](https://github.com/UI-Research/urbnindicators/issues/15)

## Problem Description

For multi-year calls, dollar-denominated variables are returned in dollars corresponding to the end-year of the five-year period. For example, ACS data from 2018-2022 are in 2022 USD, while 2012-2016 data are in 2016 USD. This makes comparisons across time periods difficult.

## Variables Requiring Inflation Adjustment

### Income Variables (21 main variables):
- Household income quintile upper limits (5 variables)
- Mean household income by quintiles (6 variables)
- Median household income by race/ethnicity (9 variables)
- Housing cost monthly median (1 variable)

### Additional variables from select_variables():
- Household Income by Gross Rent (Table B25074)
- Tenure by Housing Costs (Table B25106)

## Implementation Plan

### 1. Add `dollar_year` parameter to `compile_acs_data()` function
- [x] Add parameter to function signature with default value NULL
- [x] Add parameter documentation

### 2. Implement inflation adjustment logic
- [x] Create helper function to identify dollar-denominated variables
- [x] Create function to get inflation adjustment factors (Consumer Price Index)
- [x] Create function to apply inflation adjustment to dollar variables
- [x] Handle both raw estimates and margins of error

### 3. Integrate with main function
- [x] Add logic to determine target year when `dollar_year` is NULL
- [x] Apply inflation adjustment to appropriate variables
- [x] Update margins of error for adjusted variables

### 4. Update codebook generation
- [x] Modify `generate_codebook()` to include inflation adjustment information
- [x] Add notes about which year dollars are adjusted to

### 5. Add user messaging
- [x] Add message to inform users about inflation adjustment
- [x] Include year to which dollars were adjusted

### 6. Tests
- [x] Write tests for inflation adjustment helper functions
- [x] Write tests for main function with `dollar_year` parameter
- [x] Test edge cases (single year, multiple years, NULL parameter)

### 7. Documentation
- [x] Update function documentation
- [x] Add examples showing inflation adjustment

## Technical Details

### Inflation Data Source
Will need to source Consumer Price Index data, likely from:
- Bureau of Labor Statistics (BLS) API
- Or embed CPI data in the package

### Variables to Adjust
All variables containing dollar amounts need both:
- Raw estimates adjusted
- Margins of error adjusted (scaled by same factor)

### Formula
```
adjusted_value = original_value * (CPI_target_year / CPI_original_year)
```