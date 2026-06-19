# Build a static test dataset for the shinylive/pkgdown vignette spike:
# tract-level housing cost burden, Essex County, NJ, 2024, with geometry.
#
# Saved as .rds (not .parquet/.geojson) on purpose: view_acs_data() and
# interpolate_acs() rely on the codebook + registry attributes that
# compile_acs_data() attaches to the object. Only .rds round-trips those
# attributes intact; parquet/geojson would strip them.

library(sf)
devtools::load_all(quiet = TRUE)

# Census API key lives in the Windows-side .Renviron, not this Linux session.
key_line = readLines("/mnt/c/Users/wcurrangroome/Documents/.Renviron")
key1 = key_line[grepl("CENSUS_API_KEY", key_line, ignore.case = TRUE)][1]
key2 = sub("^[^=]+=", "", key1)              # drop "CENSUS_API_KEY="
key3 = gsub("[\"' ]", "", key2)              # strip single/double quotes + spaces
Sys.setenv(CENSUS_API_KEY = key3)
stopifnot(nchar(key3) == 40)                 # Census keys are 40 hex chars

df = compile_acs_data(
  tables    = "cost_burden",
  years     = 2024,
  geography = "tract",
  states    = "NJ",
  counties  = "34013",   # Essex County, NJ (state FIPS 34 + county FIPS 013)
  spatial   = TRUE)

out = "dev/essex_cost_burden_tract_2024.rds"
saveRDS(df, out)

cat("\n=== SUMMARY ===\n")
cat("rows:", nrow(df), " cols:", ncol(df), "\n")
cat("class:", paste(class(df), collapse = ", "), "\n")
cat("attrs:", paste(setdiff(names(attributes(df)),
                            c("names","row.names","class","sf_column")), collapse = ", "), "\n")
cat("crs:", sf::st_crs(df)$input, "\n")
cat("file size (KB):", round(file.info(out)$size / 1024, 1), "\n")
cat("\ncost-burden columns:\n")
print(grep("burden|cost", names(df), value = TRUE, ignore.case = TRUE))
