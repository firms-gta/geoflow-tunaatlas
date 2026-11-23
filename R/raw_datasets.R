#' Raw datasets of the Global Tuna Atlas, from the entity tables
#'
#' Reads the raw dataset entities (config/entities/entities_raw_*.csv of the
#' workflow) and returns, for each one, its dataset folder (which gives its
#' core function gta_rawdata_*()) and its raw files.
#'
#' @param entities_dir Folder of the entity tables.
#' @param data_dir Folder of the raw files; NULL to keep the paths of the
#'   entities (relative to the repository).
#' @return A named list (names: entity identifiers, i.e. the names of the
#'   output files <id>.csv); each element is a list with `dataset` (e.g.
#'   "wcpfc/catch/ps", or "iccat/catch/cwp_format") and `files` (raw files, in
#'   the order of the arguments of the core function).
#' @export
gta_raw_datasets <- function(entities_dir = here::here("config/entities"), data_dir = NULL) {
  tables <- Sys.glob(file.path(entities_dir, "entities_raw_*.csv"))
  rows <- do.call(rbind, lapply(tables, function(f) {
    utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")[, c("Identifier", "Data")]
  }))
  field <- function(data, key) {
    m <- regmatches(data, regexpr(sprintf("%s:[^\n]*", key), data))
    sub("_$", "", sub(sprintf("^%s:", key), "", m))
  }
  out <- list()
  for (i in seq_len(nrow(rows))) {
    id <- sub("^id:", "", strsplit(rows$Identifier[i], "\n", fixed = TRUE)[[1]][1])
    script <- sub("^[^@]*@\\./", "", field(rows$Data[i], "action"))
    sources <- strsplit(field(rows$Data[i], "source"), ",", fixed = TRUE)[[1]]
    files <- sources[!grepl("code_lists", basename(sources))]
    files <- if (is.null(data_dir)) sub("^\\./", "", files) else file.path(data_dir, basename(files))
    out[[id]] <- list(dataset = gta_rawdata_report_dataset(script, basename(files)), files = files)
  }
  out
}

#' Core function of a raw dataset
#'
#' @param dataset Dataset folder, e.g. "wcpfc/catch/ps" or
#'   "iotc/nominal/cwp_format" (see gta_raw_datasets()).
#' @return The function gta_rawdata_<tRFMO>_<type>_<dataset>(), or
#'   gta_rawdata_cwp_format() for the datasets already in CWP format.
#' @export
gta_rawdata_function <- function(dataset) {
  parts <- strsplit(dataset, "/", fixed = TRUE)[[1]]
  slug <- if (parts[length(parts)] == "cwp_format") "cwp_format" else paste(parts, collapse = "_")
  get(paste0("gta_rawdata_", slug), mode = "function")
}
