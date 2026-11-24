# Control datasets of the tests -------------------------------------------------
# Inputs: the samples of the raw datasets (tests/sample_data) and the entity
# tables of the workflow (config/entities). Expected outputs: control/expected,
# the outputs of the geoflow workflow on these samples (raw/: dataoutputpreharmo,
# levels/: dataoutputGTA). To refresh them, run the workflow offline on the
# samples (docker/run_workflow.sh) and copy these two folders.

repo_path <- function(...) normalizePath(testthat::test_path("..", "..", ...), mustWork = FALSE)
sample_dir <- function() repo_path("tests", "sample_data")
expected_dir <- function(...) testthat::test_path("control", "expected", ...)

skip_if_no_control_data <- function() {
  testthat::skip_if_not(dir.exists(sample_dir()) && dir.exists(repo_path("config", "entities")),
                        "control inputs (tests/sample_data, config/entities) not available")
}

# Auxiliary files read by the levels (conversion factors, code lists...)
control_files <- function() {
  c(list.files(sample_dir(), full.names = TRUE, pattern = "\\.(csv|xlsx)$"),
    repo_path("data", "global_effort_codelists.csv"))
}

# A dataset as a sorted vector of rows, numbers rounded, columns sorted: two
# datasets are the same data when these vectors are identical
as_rows <- function(x) {
  d <- if (is.character(x)) utils::read.csv(x, colClasses = "character", check.names = FALSE) else
    as.data.frame(lapply(as.data.frame(x), as.character), check.names = FALSE, stringsAsFactors = FALSE)
  for (col in names(d)) {
    v <- suppressWarnings(as.numeric(d[[col]]))
    if (all(!is.na(v) | is.na(d[[col]]) | d[[col]] == "")) {
      d[[col]] <- ifelse(is.na(v), d[[col]], format(round(v, 6), scientific = FALSE, trim = TRUE))
    }
  }
  d <- d[, sort(names(d)), drop = FALSE]
  sort(do.call(paste, c(d, sep = "|")))
}

expect_same_data <- function(actual, expected_file) {
  a <- as_rows(actual)
  e <- as_rows(expected_file)
  testthat::expect_identical(length(a), length(e), label = paste("rows of", basename(expected_file)))
  testthat::expect_identical(a, e, label = basename(expected_file))
}
