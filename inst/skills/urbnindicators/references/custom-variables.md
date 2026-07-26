# Custom derived variables: the `define_*()` DSL

Pass `define_*()` objects in `compile_acs_data(tables = ...)` to compute derived
variables on top of the tables you request. They land in the returned data frame
and in the codebook, with margins of error computed automatically.

**Wrap mixed input in `list()`, never `c()`.** `c()` flattens the definition
lists and destroys them.

```r
compile_acs_data(
  tables = list("snap", define_percent("snap_received", "snap_universe")),
  years = 2024, geography = "county", states = "DC")
```

Every column a definition references must already exist in the data produced by
the tables you requested. Check with `list_variables(year)` first; a missing
name aborts the call.

## Regex vs. literal — the rule that trips agents up

For `numerator`, `denominator`, and the `subtract_from_*` arguments:

- A **character vector of length > 1** is a list of literal column names, summed.
- A **single string** is treated as a literal column name if it contains no
  regex metacharacters, and otherwise as a **regex whose every match is summed**.

So `"snap_received"` is one column, but `"^race_nonhispanic|^race_hispanic"` is
a pattern matching many. A single string that accidentally contains `.`, `(`,
`|`, `[`, `$`, `^`, `+`, `?`, `*`, or braces becomes a regex. Anchor patterns
(`^…$`) when you mean one specific column — `"^sex_by_age_female$"`, not
`"sex_by_age_female"`.

This distinction also determines whether typos are caught. Explicit column names
(the vector form) are validated against the data and a bad one aborts with
"variables not found in the data". A **regex is not validated** — a pattern that
matches nothing fails silently or produces a zero numerator.

So prefer the vector form when you know the exact columns, and always verify a
pattern's matches before relying on it:

```r
grep("^race_nonhispanic|^race_hispanic", names(df), value = TRUE)
```

## `define_percent()`

```r
define_percent(numerator, denominator = NULL, output = NULL, each = FALSE,
               denominator_replace = NULL, subtract_from_numerator = NULL,
               subtract_from_denominator = NULL, exclude = NULL)
```

`output` is **inferred** as `paste0(numerator, "_percent")` only when
`numerator` is a single non-regex string and `each = FALSE`. It is **required**
when `numerator` is a vector or a regex, and **ignored** when `each = TRUE` (a
warning is emitted if you set it).

Single output:

```r
## output inferred: "snap_received_percent"
define_percent("snap_received", "snap_universe")

## vector numerator is summed; output required
define_percent(c("age_under_5_years", "age_5_9_years",
                 "age_10_14_years", "age_15_17_years"),
               denominator = "sex_by_age_universe",
               output = "age_under_18_percent")

## regex numerator, matches summed; output required
define_percent("age_(6(5|7)|7|8).*_years($|_over$)",
               denominator = "sex_by_age_universe",
               output = "age_over_64_percent")
```

Subtraction — the complement of a percentage, computed within the DSL so the
MOE propagates correctly:

```r
define_percent("snap_universe", "snap_universe",
               subtract_from_numerator = "snap_received",
               output = "snap_not_received_percent")
```

**Batch mode (`each = TRUE`)** treats `numerator` as a regex and emits one
percentage per matched column, named `<matched_column>_percent`:

```r
define_percent("^race_nonhispanic|^race_hispanic",
               denominator = "race_universe",
               each = TRUE)
```

When each match needs a *different* denominator, derive it from the matched
column name with `denominator_replace`, a named vector of
`pattern = replacement` string substitutions. This is how the `poverty` table
pairs each `..._below_<group>` column with its own `..._universe_<group>`:

```r
define_percent("federal_poverty_limit.*below",
               denominator_replace = c("below" = "universe"),
               each = TRUE,
               exclude = "percent")
```

`exclude` is a regex removing columns from pattern matching in either mode. It
is usually needed in batch mode to keep already-computed `_percent` columns from
being matched a second time.

## `define_sum()`

```r
define_sum(columns, output = NULL, each = FALSE,
           add_replace = NULL, output_replace = NULL, exclude = NULL)
```

Single output — `columns` is a character vector, `output` required:

```r
define_sum(c("col_a", "col_b", "col_c"), output = "total_abc")
```

Batch mode adds a *pair* of columns per match: for each column matching
`columns`, `add_replace` derives the addend's name and `output_replace` derives
the output's name, both by string substitution:

```r
## for each sex_by_age_female_*_years column, add the male column,
## and name the result age_*_years
define_sum("sex_by_age_female_.*years($|_over$)",
           each = TRUE,
           add_replace = c("female" = "male"),
           output_replace = c("sex_by_age_female_" = "age_"))
```

## `define_complement()`

```r
define_complement(source, output)   # computes 1 - source
```

```r
define_complement("race_nonhispanic_white_alone_percent",
                  output = "race_personofcolor_percent")
```

Only meaningful for a `_percent` (0–1) source.

## `define_metadata()`

```r
define_metadata(output, definition)
```

Computes nothing; creates a codebook entry for a column added by other means.
Use it so a hand-added column is documented and gets an aggregation strategy for
`interpolate_acs()`.

## MOEs and multi-table definitions

Each definition's `type` sets the `se_calculation_type` in the codebook, which
selects the MOE-propagation formula. Derived MOEs are Census Bureau
approximations and experimental.

MOE math assumes a definition's variables come from a single ACS table. Spanning
tables emits a warning — the estimate is still correct, but its MOE is
approximate in a way the formulas do not model.

## Verify a new variable

1. **Compare to a published benchmark.** Census Subject Tables (`S` prefix)
   report percentages that serve as reference values for variables derived from
   detailed (`B`/`C`) tables.
2. **Recompute by hand.** Pull the numerator and denominator columns and divide;
   compare. Most valuable for summed or subtracted numerators.
3. **Check the range.** Percentages must fall in 0–1. Complements should sum to
   1 with their source.
4. **Check missingness.** Derived variables should have little or none;
   substantial `NA` suggests a pattern matched the wrong columns.
5. **Read the codebook entry.** `attr(df, "codebook")` must document the
   variable as intended — the definition drives the MOE calculation, so an error
   there propagates.
