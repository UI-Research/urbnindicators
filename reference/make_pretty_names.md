# Pretty-ify variable names

Variable names are lower-case and underscore-separated. This converts
them to title case and replaces underscores with spaces. It also makes
other adjustments to facilitate reading and interpretation of variables
in the context of, for example, plots and tables.

## Usage

``` r
make_pretty_names(.data, .case = "title")
```

## Arguments

- .data:

  A data.frame–or something coercible thereto–or a character vector
  containing variables/variable names that will be converted more
  publication-appropriate formats.

- .case:

  Capitalization scheme of resulting variable names. One of "title",
  "sentence", or "upper".

## Examples

``` r
if (FALSE) { # \dontrun{
"race_personofcolor_percent" %>% make_pretty_names()
} # }
```
