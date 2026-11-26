# Run summaries and their comparison (R/run_summary.R), on the expected outputs
# of the control datasets

level_files <- function() list.files(expected_dir("levels"), pattern = "\\.csv$", full.names = TRUE)

test_that("dataset names drop their years and version suffix", {
  expect_equal(gta_dataset_key(c("global_catch_ird_level0_1950_2024.csv",
                                 "global_effort_2026.csv",
                                 "global_catch_firms_level0_public.parquet")),
               c("global_catch_ird_level0", "global_effort", "global_catch_firms_level0"))
})

test_that("1 degree squares are grouped into their 5 degree square", {
  expect_equal(gta_cwp_to_5deg(c("5212124", "5100003", "6230150", "IOTC_WEST", NA)),
               c("6210120", "6100000", "6230150", "IOTC_WEST", NA))
})

test_that("year ranges are compact", {
  expect_equal(gta_year_ranges(c(1990, 1950:1952, 1960)), "1950-1952, 1960, 1990")
  expect_equal(gta_year_ranges(integer(0)), "")
})

test_that("the summary keeps the totals of each dataset", {
  s <- gta_run_summary(level_files())
  expect_setequal(unique(s$dataset), gta_dataset_key(basename(level_files())))
  for (f in level_files()) {
    d <- utils::read.csv(f, colClasses = "character")
    key <- gta_dataset_key(basename(f))
    expect_equal(s[dataset == key, sum(measurement_value)], sum(as.numeric(d$measurement_value)))
    expect_equal(s[dataset == key, sum(n_rows)], nrow(d))
  }
  expect_true(all(grepl("^[0-9]{4}-01-01$", s$time_start)))
})

test_that("the comparison separates the common years from the years of one run only", {
  new <- gta_run_summary(level_files())
  # Previous run: no WCPFC driftnet catch (gear 07.2), and no year 2005 for
  # IATTC (as if 2005 were the new year of a release)
  previous <- new[!(gear_type %in% "07.2") & !(source_authority == "IATTC" & time_start == "2005-01-01")]
  r <- gta_compare_summaries(previous, new, labels = c("before", "after"))

  expect_true(all(r$datasets$before & r$datasets$after))
  x <- r$comparison$global_catch_ird_level0

  iattc <- x$years[source_authority == "IATTC"]
  expect_equal(iattc[["only in after"]], "2005")
  expect_false(grepl("2005", iattc$common))
  expect_true(all(c("IATTC", "WCPFC") %in% x$uncommon$source_authority))
  expect_true(all(x$uncommon$run == "after"))

  # Common years: only WCPFC changes, and only by its driftnet catch of the
  # common years
  changed <- x$by_source[difference != 0]
  expect_equal(unique(changed$source_authority), "WCPFC")
  common_driftnet <- new[dataset == "global_catch_ird_level0" & gear_type == "07.2" &
                           as.integer(substr(time_start, 1, 4)) %in%
                           previous[dataset == "global_catch_ird_level0" & source_authority == "WCPFC",
                                    as.integer(substr(time_start, 1, 4))],
                         sum(measurement_value)]
  expect_equal(sum(changed$difference), common_driftnet)

  # CWP.dataset finds the gear present in the new run only
  expect_type(x$analysis, "list")
  strata <- x$analysis$compare_strata_differences_list$disapandap
  expect_true("07.2" %in% strata$Precision[strata$Dimension == "gear_type"])
})

test_that("summaries written to a file are read back with their codes", {
  s <- gta_run_summary(level_files())
  f <- tempfile(fileext = ".csv.gz")
  data.table::fwrite(s, f)
  r <- gta_compare_summaries(f, s, analyse = FALSE)
  expect_true(all(vapply(r$comparison, function(x) all(x$by_source$difference == 0), logical(1))))
})
