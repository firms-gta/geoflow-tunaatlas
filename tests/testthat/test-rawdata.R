# Each raw dataset of the control data: its gta_rawdata_*() function, then
# gta_tidy_raw_dataset(), give the output of the workflow
skip_if_no_control_data()
raw <- gta_raw_datasets(entities_dir = repo_path("config", "entities"), data_dir = sample_dir())
atlas <- suppressMessages(create_global_tuna_atlas(data = list(raw = raw), steps = "rawdata",
                                                   work_dir = tempfile("gta_test_")))

test_that("every raw dataset of the entities has a control output", {
  expect_setequal(paste0(names(raw), ".csv"), list.files(expected_dir("raw")))
})

for (id in names(raw)) {
  test_that(paste("raw dataset", id), {
    expect_same_data(atlas$raw[[id]], expected_dir("raw", paste0(id, ".csv")))
  })
}
