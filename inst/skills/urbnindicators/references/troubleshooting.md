# Troubleshooting

## API key — four distinct failure states

The Census API returns an HTML error page where JSON is expected when a key is
rejected, which surfaces as a confusing JSON parse error. The package
disambiguates; match the message you actually got.

| Message | Cause | Fix |
|---|---|---|
| "A Census Bureau API key is required… No key was found in the `CENSUS_API_KEY` environment variable." | Unset | Request at <https://api.census.gov/data/key_signup.html>, then `tidycensus::census_api_key("KEY", install = TRUE)` and restart R |
| "The value of `CENSUS_API_KEY` does not look like a valid Census Bureau API key." | Set but too short or malformed (a valid key is ≥10 characters with both letters and numbers) | Usually a truncated paste or a placeholder value — re-copy the key |
| "The Census Bureau API rejected the API key… The API is reachable right now, so this is a problem with the key, not an outage." | Well-formed but inactive, revoked, or mistyped | Check for stray characters; confirm activation via the link in the sign-up email; request a new key if never activated |
| "The Census Bureau API returned an error page instead of data, and a status check of the API also failed." | API outage | Check <https://api.census.gov/status/> and retry later |

If you see a raw **`lexical error: invalid char in json text`**, that is a
rejected key or an outage, *not* corrupt data. Do not retry blindly or work
around it — resolve the key.

## Discovery and naming

**"Unknown table(s): …" / "Use `list_tables()` to see available tables."**
The name is not a registered table, a construct name, or a valid raw ACS code.
Call `list_tables()` and pick a real one. Registered tables use semantic names
(`"snap"`), not ACS codes.

**"User-supplied definitions reference variables not found in the data: …"**
A `define_*()` object names a column that the requested tables do not produce.
Two causes: the variable belongs to a table you did not request, or the name is
invented. Run `list_variables(year)` and confirm the exact string; add the
owning table to `tables`.

**"`<x>` is ambiguous: matches variables across multiple ACS tables."**
A partial name or prefix matched more than one table. Pass the full ACS table
code (`"B25070"`) or the exact variable name.

**A regex matched nothing or too much.** Definition patterns are silent about
over-matching. Check with `grep(pattern, names(df), value = TRUE)` before
trusting the output.

## Data selection

**"ACS table `<code>` overlaps the registered table(s) … the registered version
is returned."**
You requested both a raw code and a registered table covering it. The registered
version wins. To get the auto-processed version, request the code *without* the
overlapping registered table.

**"`counties` is ignored when `geography` is …"**
`counties` only applies to `"county"`, `"county subdivision"`, `"tract"`, and
`"block group"`. National geographies (`"us"`, `"cbsa"`, `"zcta"`, `"urban
area"`, …) return nationally; filter afterward.

**"N invalid county code(s) found."** `counties` takes **five-digit FIPS
codes** (state + county), not county names or three-digit codes.

**"County-level queries can be slow for more than a few counties."**
More than 5 counties. Omit `counties`, pull the state, and filter after.

## Block groups

**"These tables are not published at the block-group level and have been
dropped."** / **"Some variables are not published at the block-group level…"**
Expected — the ACS publishes only a subset at this geography. Check what is
available with `list_tables(geography = "block group", year = ...)` and adjust
the request, or use tracts. The call errors only if *none* of the requested
tables are available.

**"ACS table `<code>` is not published at the block-group level for `<year>`."**
Same cause for a raw code. The message reports the lowest geography at which
that table *is* published.

Block group also requires `states` and years ≥ 2013.

## Attributes

**"Input data must have a `codebook` attribute. Use output from
`compile_acs_data()`."** (from `interpolate_acs()`)
**"`.data` is missing its `codebook` attribute."** (from `view_acs_data()`)

Something between the pull and the call dropped the attribute. The culprits are
**aggregating and reshaping** verbs — `summarise()` (with or without
`group_by()`), `pivot_longer()`, `pivot_wider()` — which rebuild the data frame.

Subsetting and transformation preserve it, including the ones people commonly
suspect: `filter()`, `select()`, `mutate()`, `slice()`, `distinct()`, joins,
base `[`, `subset()`, `as.data.frame()`, and `sf::st_transform()` /
`st_drop_geometry()` all keep the attribute.

Do the reshaping step last, or capture and re-attach:

```r
cb = attr(df, "codebook")
x  = df %>% group_by(region) %>% summarise(across(everything(), sum))
attr(x, "codebook") = cb
```

Bisect with `!is.null(attr(x, "codebook"))` after each step to find where it
was lost.

`attr(df, "resolved_tables")` is dropped the same way and is also used by
`interpolate_acs()`.

**"`.data` must be an `sf` object."** (from `view_acs_data()`) — re-pull with
`spatial = TRUE`.

## Arguments and years

**"`years` must be a vector of four-digit integers."** / **"must be between
2009 and the current year."** Five-year ACS starts in 2009.

**An API error on a year you did not specify.** `years` defaults to *current
year − 2*, a guess at the latest release rather than a validated one. Pass an
explicit year.

**"Unknown argument(s) passed to `compile_acs_data()`… will be ignored."**
A typo in an argument name, or a `tidycensus` argument that is not forwarded.
Note `variables` is deprecated and ignored — use `tables`.

**"Requested years span 2020…"** Tract and block-group boundaries changed. Do
not compare across that line; use a crosswalk.

## Performance

Slow or seemingly hung queries are almost always scope. Confirm `tables` is set
(the default pulls every registered table), confirm `states` is set, and pass
`cache = TRUE` so a retry does not re-download. `clear_acs_cache()` empties the
cache.

`population_density` downloads tigris geometry even when `spatial = FALSE`.
