test_that("gta_data() returns a data frame as is and reads a path", {
  df <- data.frame(a = 1:2)
  expect_identical(gta_data(df), df)
  f <- tempfile(fileext = ".csv"); utils::write.csv(df, f, row.names = FALSE)
  expect_equal(gta_data(f), df)
  expect_error(gta_data("no_such_file.csv"), "data frame or the path")
})

test_that("gta_dataset_outputs() formats the harmonised and public versions", {
  d <- data.frame(source_authority = "IOTC", species = "YFT", time_start = "2020-04-01",
                  time_end = "2020-04-30", geographic_identifier = 6100000, measurement_unit = "TRUE",
                  measurement_value = 1, other = "x")
  out <- gta_dataset_outputs(d)
  expect_identical(out$harmonized$measurement_unit, "t")
  expect_s3_class(out$harmonized$time_start, "Date")
  expect_identical(out$harmonized$geographic_identifier, "6100000")
  expect_identical(c(out$public$year, out$public$month, out$public$quarter), c(2020L, 4L, 2L))
  expect_false("other" %in% names(out$public))
  expect_identical(out$temporal_extent, "2020-04-01/2020-04-30")
})

test_that("gta_require_con() stops with a clear message without connection", {
  expect_error(gta_require_con(NULL, "This branch"), "needs a connection to the Tuna Atlas database")
})

test_that("gta_rawdata_function() finds the core function of a dataset", {
  expect_identical(gta_rawdata_function("wcpfc/catch/ps"), gta_rawdata_wcpfc_catch_ps)
  expect_identical(gta_rawdata_function("iotc/nominal/cwp_format"), gta_rawdata_cwp_format)
})

test_that("gta_nominal_catch() keeps the SBF data of a single source", {
  d <- data.frame(source_authority = c("CCSBT", "IOTC", "IOTC"), species = c("SBF", "SBF", "YFT"),
                  gear_type = "09.1", fishing_mode = "UNK", fishing_fleet = "NEI",
                  time_start = "2020-01-01", time_end = "2020-12-31", geographic_identifier = "IOTC",
                  measurement = "catch", measurement_type = "NC", measurement_value = c(10, 5, 3),
                  measurement_unit = "t", measurement_processing_level = "unknown")
  ccsbt <- gta_nominal_catch(list(d))
  expect_identical(paste(ccsbt$source_authority, ccsbt$species), c("CCSBT SBF", "IOTC YFT"))
  others <- gta_nominal_catch(list(d), SBF_data_rfmo_to_keep = "other_trfmos")
  expect_identical(paste(others$source_authority, others$species), c("IOTC SBF", "IOTC YFT"))
  expect_identical(nrow(gta_nominal_catch(list(d), SBF_data_rfmo_to_keep = NULL)), 3L)
})

test_that("gta_default_options() follows the entities of the workflow", {
  o <- gta_default_options()
  expect_named(o, c("nominal", "level0", "effort", "level1", "level2"))
  expect_identical(o$level2$options$dataset_level, "2")
  expect_true(o$effort$options$iattc_ps_raise_flags_to_schooltype)
})
