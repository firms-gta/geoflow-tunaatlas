# gta_default_options() is the copy, in the package, of the level entities of
# the workflow (config/entities): both must give the same files and options
skip_if_not(dir.exists(repo_path("config", "entities")), "config/entities not available")

entity_tables <- c(nominal = "entities_nominal_catch.csv", level0 = "entities_catch_level0.csv",
                   effort = "entities_effort.csv", level1 = "entities_catch_level1.csv",
                   level2 = "entities_catch_level2.csv")

# Identifier, source file names and options of the entity of a table, with
# the types geoflow gives the options
read_entity <- function(table) {
  e <- utils::read.csv(repo_path("config", "entities", table), stringsAsFactors = FALSE,
                       check.names = FALSE, encoding = "UTF-8")[1, ]
  data <- strsplit(e$Data, "\n", fixed = TRUE)[[1]]
  data <- sub("_$", "", data)
  sources <- strsplit(sub("^source:", "", grep("^source:", data, value = TRUE)), ",", fixed = TRUE)[[1]]
  opt_lines <- sub("^action_option_", "", grep("^action_option_", data, value = TRUE))
  options <- lapply(sub("^[^:]*:", "", opt_lines), function(v) if (v %in% c("true", "false")) v == "true" else v)
  names(options) <- sub(":.*$", "", opt_lines)
  list(id = sub("^id:", "", strsplit(e$Identifier, "\n", fixed = TRUE)[[1]][1]),
       files = basename(sources), options = options)
}

defaults <- gta_default_options()
for (level in names(entity_tables)) {
  test_that(paste("default options of", level, "are those of its entity"), {
    entity <- read_entity(entity_tables[[level]])
    o <- defaults[[level]]
    expect_identical(o$id, entity$id)
    expect_setequal(c(o$datasets, o$nominal, o$files), entity$files)
    # keynominal (level 2) and keylevel0 (level 1) are given by `nominal` and `datasets`
    entity_options <- entity$options
    if (!is.null(o$nominal)) {
      expect_identical(entity_options$keynominal, o$nominal)
      entity_options$keynominal <- NULL
    }
    if (level == "level1") {
      expect_identical(entity_options$keylevel0, o$datasets[1])
      entity_options$keylevel0 <- NULL
    }
    expect_identical(o$options[order(names(o$options))], entity_options[order(names(entity_options))])
  })
}

test_that("a level stops when a file it names is missing, or named twice", {
  o <- gta_default_options()
  expect_error(suppressMessages(gta_create_levels(character(0), options = o, steps = "nominal")),
               "nominal_catch_ccsbt_level0__bygear.csv not found")
  o$nominal$datasets <- c(o$nominal$datasets, o$nominal$datasets[1])
  expect_error(suppressMessages(gta_create_levels(character(0), options = o, steps = "nominal")),
               "names given more than once")
})

test_that("two different files with the same name are an error", {
  d1 <- file.path(tempfile(), "a"); d2 <- file.path(tempfile(), "b")
  dir.create(d1, recursive = TRUE); dir.create(d2, recursive = TRUE)
  writeLines("x", file.path(d1, "f.csv")); writeLines("y", file.path(d2, "f.csv"))
  expect_error(gta_find_file("f.csv", c(file.path(d1, "f.csv"), file.path(d2, "f.csv")), "test"),
               "several different files named f.csv")
  writeLines("x", file.path(d2, "f.csv"))
  expect_identical(gta_find_file("f.csv", c(file.path(d1, "f.csv"), file.path(d2, "f.csv")), "test"),
                   file.path(d1, "f.csv"))
})
