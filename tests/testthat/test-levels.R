# The levels of the control data, from the control outputs of the raw datasets:
# gta_create_levels() gives the outputs of the workflow
skip_if_no_control_data()
options <- gta_default_options()
atlas <- suppressMessages(gta_create_levels(
  harmonised = list.files(expected_dir("raw"), full.names = TRUE), files = control_files(),
  options = options, steps = c("nominal", "level0", "effort", "level1"), work_dir = tempfile("gta_test_")))

for (level in c("nominal", "level0", "effort", "level1")) {
  test_that(paste("dataset", level), {
    expect_same_data(atlas[[level]], expected_dir("levels", paste0(options[[level]]$id, ".csv")))
  })
}

test_that("dataset level2", {
  skip(paste("the sample data do not allow the raising to the nominal catch to complete",
             "(the workflow stops at the same step on them): level 2 to test on fuller control data"))
})
