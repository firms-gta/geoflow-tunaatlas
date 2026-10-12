# Read a Global Tuna Atlas dataset for the summary reports.
#
# `path` can be a local file or an http(s) URL (downloaded once to `cache_dir`).
# Supported formats: .csv, .parquet, .gpkg, .rds, and .qs if {qs} is installed
# (no longer in the image: older Zenodo releases only). The geometry is dropped: the
# reports add it back from the CWP grid only where a map needs it.
read_gta_dataset <- function(path, cache_dir = here::here("data")) {
  if (grepl("^https?://", path)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    local <- file.path(cache_dir, basename(sub("\\?.*$", "", path)))
    if (!file.exists(local)) {
      options(timeout = max(3600, getOption("timeout")))
      utils::download.file(path, local, mode = "wb")
    }
    path <- local
  }
  if (!file.exists(path)) stop("Dataset not found: ", path)

  ext <- tolower(tools::file_ext(path))
  data <- switch(
    ext,
    csv     = readr::read_csv(path, show_col_types = FALSE),
    rds     = readRDS(path),
    qs      = {
      if (!requireNamespace("qs", quietly = TRUE)) {
        stop("'", path, "' is a .qs file: install {qs} to read it, or use the .csv of the release.")
      }
      getExportedValue("qs", "qread")(path)
    },
    parquet = arrow::read_parquet(path),
    gpkg    = sf::st_read(path, quiet = TRUE),
    stop("Unsupported format: .", ext)
  )

  if (inherits(data, "sf")) data <- sf::st_drop_geometry(data)
  data <- as.data.frame(data)
  data$geom <- NULL
  data$geom_wkt <- NULL
  if ("geographic_identifier" %in% names(data)) {
    data$geographic_identifier <- as.character(data$geographic_identifier)
  }
  data
}
