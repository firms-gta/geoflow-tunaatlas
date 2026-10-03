#' Decrease nominal categorical precision to match georeferenced availability
#'
#' For each combination of `keys` (typically `year` and `source_authority`), this
#' function checks whether each value of selected categorical dimensions
#' (`dims_to_check`) present in the nominal dataset also exists in the
#' georeferenced dataset. Values that appear only in `nominal` are replaced by
#' a coarser category (e.g., `NEI`, `UNK`, `99.9`) defined in `replacement`.
#'
#' The function returns the updated nominal dataset and, optionally, a summary
#' table reporting how many rows were affected and whether the fallback category
#' is itself present in the georeferenced data (and whether georeferenced data
#' exists at all for the given `keys`).
#'
#' @param nominal A data.frame/tibble with nominal data.
#' @param georef A data.frame/tibble with georeferenced data. Must include
#'   `time_start` so that `year = lubridate::year(time_start)` can be computed.
#' @param keys Character vector of grouping keys used for the comparison
#'   (default: `c("year","source_authority")`). These columns must exist in
#'   `nominal`, and will be created/ensured in `georef` via `time_start`.
#' @param dims_to_check Character vector of categorical columns to check and
#'   potentially replace (default: gear_type, fishing_fleet, fishing_mode).
#' @param replacement Named character vector mapping each dimension in
#'   `dims_to_check` to its fallback category (default: `99.9`, `NEI`, `UNK`).
#' @param keep_na Logical. If TRUE, missing values (`NA`) are left unchanged.
#'   If FALSE, `NA` values are also replaced using `replacement`.
#' @param return_summary Logical. If TRUE, returns a list with `data` and
#'   `summary`. If FALSE, returns only the updated nominal dataset.
#'
#' @return If `return_summary = TRUE`, a list with:
#' \describe{
#'   \item{data}{The updated nominal dataset with replaced values.}
#'   \item{summary}{A tibble with one row per replaced (key, dimension, value)
#'   combination including `n_changed`, the fallback value `.new_value__`,
#'   `georef_has_fallback` (fallback exists in georef for same keys/dimension),
#'   `georef_has_any_data` (any georef exists for the keys) and `georef_zero_data`.}
#' }
#' If `return_summary = FALSE`, returns only the updated nominal dataset.
#'
#' @examples
#' \dontrun{
#' res <- decrease_precision_of_nominal_with_georef(
#'   nominal = nominal_catch,
#'   georef  = georef_dataset
#' )
#' res$data
#' res$summary
#' }
#'
#' @export
decrease_precision_of_nominal_with_georef <- function(
    nominal,
    georef,
    keys = c("year", "source_authority"),
    dims_to_check = c("gear_type", "fishing_fleet", "fishing_mode"),
    replacement = c(gear_type = "99.9", fishing_fleet = "NEI", fishing_mode = "UNK"),
    keep_na = TRUE,
    return_summary = TRUE
) {
  stopifnot(is.data.frame(nominal), is.data.frame(georef))
  georef <- georef %>%
    dplyr::mutate(year = lubridate::year(time_start))
  stopifnot(all(dims_to_check %in% names(nominal)), all(dims_to_check %in% names(georef)))
  stopifnot(all(dims_to_check %in% names(replacement)))
  
  # Coerce to character to avoid factor surprises
  nominal <- dplyr::as_tibble(nominal)
  georef  <- dplyr::as_tibble(georef)
  for (d in dims_to_check) {
    nominal[[d]] <- as.character(nominal[[d]])
    georef[[d]]  <- as.character(georef[[d]])
  }
  
  # Values present in georef by keys + dimension
  allowed_long <- georef %>%
    dplyr::select(dplyr::all_of(keys), dplyr::all_of(dims_to_check)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(dims_to_check),
      names_to = "dimension",
      values_to = "value"
    ) %>%
    dplyr::distinct()
  
  # Nominal in long form for checking / replacement
  nominal_long <- nominal %>%
    dplyr::mutate(.row_id__ = dplyr::row_number()) %>%
    dplyr::select(.row_id__, dplyr::all_of(keys), dplyr::all_of(dims_to_check)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(dims_to_check),
      names_to = "dimension",
      values_to = "value"
    )
  
  flagged <- nominal_long %>%
    dplyr::left_join(
      allowed_long %>% dplyr::mutate(.allowed__ = TRUE),
      by = c(stats::setNames(keys, keys), "dimension", "value")
    ) %>%
    dplyr::mutate(
      .allowed__ = dplyr::coalesce(.allowed__, FALSE),
      .needs_replace__ = dplyr::case_when(
        keep_na & is.na(value) ~ FALSE,
        is.na(value) ~ TRUE,
        TRUE ~ !.allowed__
      ),
      .new_value__ = dplyr::if_else(
        .needs_replace__,
        unname(replacement[dimension]),
        value
      )
    )
  
  summary_tbl <- flagged %>%
    dplyr::filter(.needs_replace__) %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys)), dimension, value, .new_value__, name = "n_changed") %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)), dplyr::desc(n_changed))
  
  georef_long <- georef %>%
    dplyr::select(dplyr::all_of(keys), dplyr::all_of(dims_to_check)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(dims_to_check),
      names_to = "dimension",
      values_to = "value"
    ) %>%
    dplyr::mutate(value = as.character(value)) %>%
    dplyr::distinct()
  
  georef_fallback <- georef_long %>%
    dplyr::select(dplyr::all_of(keys), dimension, value) %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      dimension,
      .new_value__ = value,
      georef_has_fallback = TRUE
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(year = as.character(year))
  
  georef_presence <- georef %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::mutate(georef_has_any_data = TRUE) %>%
    dplyr::mutate(year = as.character(year))
  
  summary_flagged <- summary_tbl %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::mutate(.new_value__ = as.character(.new_value__)) %>%
    dplyr::left_join(
      georef_fallback %>% dplyr::distinct(),
      by = c(keys, "dimension", ".new_value__")
    ) %>%
    dplyr::left_join(
      georef_presence,
      by = keys
    ) %>%
    dplyr::mutate(
      georef_has_fallback = dplyr::coalesce(georef_has_fallback, FALSE),
      georef_has_any_data = dplyr::coalesce(georef_has_any_data, FALSE),
      georef_zero_data = !georef_has_any_data
    )
  
  replacements_wide <- flagged %>%
    dplyr::select(.row_id__, dimension, .new_value__) %>%
    tidyr::pivot_wider(names_from = dimension, values_from = .new_value__)
  
  out <- nominal %>%
    dplyr::mutate(.row_id__ = dplyr::row_number()) %>%
    dplyr::left_join(replacements_wide, by = ".row_id__", suffix = c("", ".repl")) %>%
    {
      x <- .
      for (d in dims_to_check) {
        repl_col <- paste0(d, ".repl")
        x[[d]] <- dplyr::coalesce(x[[repl_col]], x[[d]])
      }
      dplyr::select(x, -dplyr::matches("\\.repl$"), -.row_id__)
    }
  
  if (return_summary) {
    return(list(data = out, summary = summary_flagged))
  } else {
    return(out)
  }
}