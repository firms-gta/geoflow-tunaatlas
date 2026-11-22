# Checks of the data loaded in the Tuna Atlas database
# -----------------------------------------------------------------------------
# gta_check_fact_nulls()      no NULL value in the fact tables
#                             (fact_tables.catch, fact_tables.effort)
# gta_check_loaded_dataset()  after load_dataset: the dataset read back from
#                             the database (its materialized view) must be
#                             exactly the file that was sent, row for row
#
# mode: "error" stops the workflow, "warn" only warns, "none" checks nothing.
#
# Why the exact comparison matters: load_dataset replaces a code that is not
# found in the code lists of the database by 0, i.e. "UNK" (see "Replace NA
# by 0" in load_dataset.R). Such a row is loaded without NULL and keeps its
# value: only a row-by-row comparison shows it.
# -----------------------------------------------------------------------------

gta_check_report <- function(problems, mode, what) {
  mode <- match.arg(mode, c("error", "warn", "none"))
  if (length(problems) == 0) {
    message("[check] ", what, ": OK")
    return(invisible(TRUE))
  }
  msg <- paste0(what, ":\n  - ", paste(problems, collapse = "\n  - "))
  if (mode == "error") stop(msg, call. = FALSE)
  if (mode == "warn") warning(msg, call. = FALSE, immediate. = TRUE)
  invisible(FALSE)
}

gta_check_name <- function(x) {
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)?$", x)) {
    stop("Invalid table name: ", x, call. = FALSE)
  }
  x
}

# NULL values per column of the fact tables (whole table, or one dataset)
gta_fact_null_problems <- function(con, tables = c("catch", "effort"),
                                   id_metadata = NULL) {
  problems <- character(0)
  for (t in tables) {
    t <- gta_check_name(sub("^fact_tables\\.", "", t))
    cols <- DBI::dbGetQuery(con, sprintf(
      "SELECT column_name FROM information_schema.columns
       WHERE table_schema = 'fact_tables' AND table_name = '%s'
       ORDER BY ordinal_position", t))$column_name
    if (length(cols) == 0) next   # this fact table does not exist here
    where <- if (is.null(id_metadata)) "" else
      sprintf(" WHERE id_metadata = %d", as.integer(id_metadata))
    counts <- DBI::dbGetQuery(con, sprintf(
      "SELECT %s FROM fact_tables.%s%s",
      paste(sprintf('count(*) FILTER (WHERE "%s" IS NULL) AS "%s"', cols, cols),
            collapse = ", "),
      t, where))
    n <- vapply(counts, function(x) as.numeric(x[1]), numeric(1))
    n <- n[!is.na(n) & n > 0]
    if (length(n) > 0) {
      problems <- c(problems, sprintf("fact_tables.%s.%s: %s NULL value(s)",
                                      t, names(n), format(n, big.mark = " ")))
    }
  }
  problems
}

gta_check_fact_nulls <- function(con, tables = c("catch", "effort"),
                                 id_metadata = NULL, mode = "error") {
  if (identical(mode, "none")) return(invisible(TRUE))
  what <- if (is.null(id_metadata)) "NULL values in the fact tables" else
    sprintf("NULL values in the fact tables (id_metadata = %s)", id_metadata)
  gta_check_report(gta_fact_null_problems(con, tables, id_metadata), mode, what)
}

# Same textual form on both sides before comparing the dimensions:
#   time_start/end     date part only
#   codes              trimmed; empty or NA is loaded as UNK
# measurement_value is compared separately, as a number (see below).
gta_normalise_rows <- function(df, cols) {
  data.table::as.data.table(lapply(stats::setNames(nm = cols), function(col) {
    x <- trimws(as.character(df[[col]]))
    if (col %in% c("time_start", "time_end")) return(substr(x, 1, 10))
    x[is.na(x) | x == "" | x == "NA"] <- "UNK"
    x
  }))
}

# Compares the file sent with the dataset read back from the database.
#   file        the CSV loaded by load_dataset
#   view        materialized view of the dataset, e.g. "fact_tables.<id>"
#   query       SQL of the dataset (the one the view is made of), used when
#               there is no view
#   table       fact table, e.g. "fact_tables.catch" (NULL check)
#   id_metadata id of the dataset in metadata.metadata
gta_check_loaded_dataset <- function(con, file, table, id_metadata,
                                     view = NULL, query = NULL, mode = "error") {
  if (identical(mode, "none")) return(invisible(TRUE))
  id_metadata <- as.integer(id_metadata)
  what <- sprintf("Check of the loaded dataset %s (id_metadata = %d)",
                  basename(file), id_metadata)

  # 1. No NULL value for this dataset in the fact table
  problems <- gta_fact_null_problems(con, gta_check_name(table), id_metadata)

  # 2. The dataset in the database, read back
  source_sql <- NULL
  if (!is.null(view)) {
    view <- gta_check_name(view)
    exists <- !is.na(DBI::dbGetQuery(con, sprintf(
      "SELECT to_regclass('%s') AS r", view))$r)
    if (exists) source_sql <- view
  }
  if (is.null(source_sql) && !is.null(query)) source_sql <- sprintf("(%s) AS q", query)
  if (is.null(source_sql)) {
    return(gta_check_report(c(problems, "no materialized view and no query to read the dataset back"),
                            mode, what))
  }

  file_df <- data.table::fread(file, colClasses = "character",
                               na.strings = c("", "NA"), showProgress = FALSE)
  db_cols <- names(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 0", source_sql)))

  # Columns compared: those of the file that the database gives back.
  # Derived columns (labels, year/month/quarter, geometry) are left out.
  derived <- c("year", "month", "quarter", "time_period", "geom_wkt",
               "longitude", "latitude")
  cols <- setdiff(intersect(names(file_df), db_cols), derived)
  cols <- cols[!grepl("_label$", cols)]
  if (!"measurement_value" %in% cols) {
    return(gta_check_report(c(problems, "no measurement_value column to compare"),
                            mode, what))
  }
  not_back <- setdiff(names(file_df), c(db_cols, derived))
  not_back <- not_back[!grepl("_label$", not_back)]
  if (length(not_back) > 0) {
    message("[check] columns of the file not in the database (not compared): ",
            paste(not_back, collapse = ", "))
  }

  db_df <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM %s",
    paste(sprintf('"%s"::text AS "%s"', cols, cols), collapse = ", "), source_sql))

  # 3. Same rows, the same number of times (a row may legitimately appear
  #    twice in the file). The dimensions must be identical; measurement_value
  #    must match within the rounding of numeric(12,2): the database keeps 2
  #    decimals, and how a value lying on a half (0.145, 11639.525) ends up
  #    depends on the text R sent, so an exact rounding cannot be reproduced.
  #    Within the same dimensions, values are paired in increasing order.
  n_file <- nrow(file_df); n_db <- nrow(db_df)
  keys <- setdiff(cols, "measurement_value")
  tolerance <- 0.005 + 1e-9
  
  a <- gta_normalise_rows(file_df, keys)
  a$v_file <- suppressWarnings(as.numeric(file_df$measurement_value))
  b <- gta_normalise_rows(db_df, keys)
  b$v_db <- suppressWarnings(as.numeric(db_df$measurement_value))
  rm(file_df, db_df)
  data.table::setorderv(a, c(keys, "v_file"))
  data.table::setorderv(b, c(keys, "v_db"))
  a$k <- data.table::rowidv(a, cols = keys)
  b$k <- data.table::rowidv(b, cols = keys)
  m <- merge(a, b, by = c(keys, "k"), all = TRUE)
  rm(a, b)
  diff <- m[is.na(m$v_file) | is.na(m$v_db) | abs(m$v_file - m$v_db) > tolerance, ]
  rm(m)
  
  if (n_file != n_db) {
    problems <- c(problems, sprintf("%s rows in the file, %s in the database",
                                    format(n_file, big.mark = " "),
                                    format(n_db, big.mark = " ")))
  }
  if (nrow(diff) > 0) {
    only_file <- sum(!is.na(diff$v_file))
    only_db <- sum(!is.na(diff$v_db))
    fmt_v <- function(v) ifelse(is.na(v), "-", format(v, digits = 15, scientific = FALSE))
    examples <- utils::head(diff, 10)
    examples <- vapply(seq_len(nrow(examples)), function(i) {
      r <- examples[i, ]
      paste0(paste(keys, unlist(r[, keys, with = FALSE]), sep = "=", collapse = ", "),
             "  [measurement_value file: ", fmt_v(r$v_file),
             ", database: ", fmt_v(r$v_db), "]")
    }, character(1))
    problems <- c(problems,
                  sprintf("%s row(s) of the file without an identical row in the database, %s row(s) of the database without one in the file (dimensions compared: %s; measurement_value within %s)",
                          format(only_file, big.mark = " "), format(only_db, big.mark = " "),
                          paste(keys, collapse = ", "), "0.005"),
                  paste0("first differences:\n      ", paste(examples, collapse = "\n      ")))
  }
  
  gta_check_report(problems, mode, what)
}
