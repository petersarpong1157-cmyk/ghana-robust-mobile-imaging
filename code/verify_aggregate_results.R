# Standalone aggregate-table verification; does not refit any models.
# Run from the repository root: Rscript code/verify_aggregate_results.R
a <- read.csv("supplementary/Extended_all_allocations.csv")
d <- read.csv("tables/Table_06b_Extended_D_minus_A.csv")
r <- read.csv("tables/Table_08_Reproduction.csv")
stopifnot(nrow(a) == 60L, nrow(d) == 15L, nrow(r) == 20L)
stopifnot(!anyDuplicated(a[c("modality","fleet","model")]))
stopifnot(all(r$passed), all(r$max_abs_error <= r$tolerance))
keys <- c("modality","fleet")
metrics <- c("scenario_mean_pct","worst_scenario_mean_pct","worst_scenario_region_mean_pct")
x <- merge(a[a$model=="D",c(keys,metrics)], a[a$model=="A",c(keys,metrics)], by=keys, suffixes=c(".D",".A"))
x <- merge(x,d,by=keys)
stopifnot(nrow(x)==15L)
for(i in seq_along(metrics)) {
  v <- x[[paste0(metrics[i],".D")]] - x[[paste0(metrics[i],".A")]]
  stopifnot(max(abs(v-x[[c("delta_mean_pp","delta_worst_pp","delta_region_pp")[i]]])) < 1e-8)
}
stopifnot(all(x$delta_region_pp > 0))
cat("Aggregate table consistency checks PASS\n")
