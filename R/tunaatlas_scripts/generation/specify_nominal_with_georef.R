#' Specify (disaggregate) unknown nominal strata using georeferenced proportions
#'
#' @description
#' This function "specifies" (i.e. disaggregates) nominal records where one of the
#' target dimensions is unknown (e.g. `fishing_fleet == "NEI"`, `gear_type == "99.9"`,
#' `fishing_mode == "UNK"`), by redistributing the unknown nominal value across the
#' detailed categories observed in the georeferenced dataset **for the same strata**.
#'
#' It iterates over a set of dimensions (by default: fishing_mode -> gear_type -> fishing_fleet)
#' and, at each step, replaces nominal unknown rows by one or more rows whose breakdown
#' follows the georeferenced proportions.
#'
#' Two behaviors are available:
#' - `mode = "strong"`: always redistribute the nominal unknown value by georef proportions.
#' - `mode = "weak"`: if the nominal unknown value exceeds the georef total for that strata,
#'   copy georef detailed totals as-is, and keep the leftover as an unknown row (so totals match).
#'
#' @param nominal_df A data.frame/tibble of nominal data (must include `measurement_value`
#'   and the dimensions used in `strata_cols` and `step_order`).
#' @param georef_df A data.frame/tibble of georeferenced data (same requirements).
#' @param strata_cols Character vector of columns defining the "stratum" shared between
#'   nominal and georef (the join key when looking up georef proportions).
#'   These columns must exist in both datasets.
#' @param step_order Character vector of dimension columns to "specify" iteratively.
#'   Each of these is expected to use an unknown code defined in `unk_map`.
#' @param unk_map Named list mapping each dimension in `step_order` to its unknown code.
#'   Example: list(fishing_fleet="NEI", gear_type="99.9", fishing_mode="UNK").
#' @param mode Character, either `"strong"` or `"weak"`.
#' @param logger A logging function taking a single string (default: `message`).
#'
#' @return A tibble with the same structure as `nominal_df`, where unknown rows for the
#'   dimensions in `step_order` may have been replaced by specified rows. At the end,
#'   the result is re-aggregated by all columns except `measurement_value`.
#'
#' @details
#' ## What happens internally (high level)
#' For each `dim_col` in `step_order`:
#' 1. Compute per-stratum proportions of each `dim_col` category in `georef_df`.
#' 2. Extract nominal rows where `dim_col` is unknown.
#' 3. Keep only strata where georef has information (inner join on `strata_cols`).
#' 4. Create specified rows:
#'    - strong: nominal_value * georef_prop for each detailed georef category
#'    - weak: if nominal_value > georef_total, copy georef detailed values and keep leftover as unknown
#' 5. Remove original unknown nominal rows for those strata and bind the new specified rows.
#'
#' ## Important assumption
#' `strata_cols` must NOT include the dimension currently being specified (`dim_col`),
#' otherwise the join cannot "find" the detailed breakdown. In your usage, you do:
#' `strata_cols_updated <- setdiff(strata_cols_updated, step_order_updated)` which ensures that.
#'
#' @examples
#' \dontrun{
#' strata_cols_updated <- c("species","source_authority","year","geographic_identifier_nom","measurement_unit")
#' step_order_updated  <- c("fishing_mode","gear_type","fishing_fleet")
#'
#' nominal_out <- specify_nominal_with_georef(
#'   nominal_df = nominal_catch,
#'   georef_df  = georef_dataset,
#'   strata_cols = strata_cols_updated,
#'   step_order  = step_order_updated,
#'   mode = "weak",
#'   logger = function(msg) cat("[Specify]", msg, "\n")
#' )
#' }
#'
#' @export
specify_nominal_with_georef <- function(
    nominal_df,
    georef_df,
    strata_cols = c("species", "source_authority", "year", "geographic_identifier_nom", "measurement_unit"),
    step_order  = c("fishing_mode", "gear_type", "fishing_fleet"),
    unk_map     = list(fishing_fleet = "NEI", gear_type = "99.9", fishing_mode = "UNK"),
    mode        = opts$strong_weak_upgrade,
    logger      = message
) {
  
  # ---- Ensure a `year` column exists in both datasets (derived from time_start) ----
  # This allows stratification/joining by year even if the input data only has dates.
  if (!"year" %in% names(nominal_df)) {
    nominal_df <- nominal_df %>%
      dplyr::mutate(year = lubridate::year(time_start))
  }
  if (!"year" %in% names(georef_df)) {
    georef_df <- georef_df %>%
      dplyr::mutate(year = lubridate::year(time_start))
  }
  
  # ---- Iterate over each dimension to specify (e.g. fishing_mode, gear_type, fleet) ----
  for (dim_col in step_order) {
    
    # Unknown code for the current dimension, e.g. "UNK" for fishing_mode
    unk_code <- unk_map[[dim_col]]
    
    # Symbol versions for tidy eval in mutate (so we can write !!dim_sym := ...)
    dim_sym  <- rlang::ensym(dim_col)
    
    # -------------------------------------------------------------------------
    # 1) Compute georeferenced proportions by stratum and by detailed dim_col
    # -------------------------------------------------------------------------
    # We first aggregate georef values for each (strata_cols + dim_col) combination.
    # Then within each stratum (strata_cols), we compute:
    # - `prop`: the share of each dim_col category
    # - `total`: the total georef value for that stratum
    #
    # NOTE: we keep ALL georef codes (including unknown ones). That is intentional:
    # it lets you specify nominal unknowns even if georef contains an unknown category;
    # the unknown category will simply receive its proportional share like any other.
    geo_props <- georef_df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(strata_cols, dim_col)))) %>%
      dplyr::summarise(val = sum(measurement_value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(strata_cols))) %>%
      dplyr::mutate(
        prop  = val / sum(val, na.rm = TRUE),
        total = sum(val, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    
    # -------------------------------------------------------------------------
    # 2) Extract nominal rows where the current dimension is unknown
    # -------------------------------------------------------------------------
    unknown_rows <- nominal_df %>%
      dplyr::filter(.data[[dim_col]] == unk_code)
    
    # If there is nothing unknown for this dimension, skip this step.
    if (nrow(unknown_rows) == 0) next
    
    # -------------------------------------------------------------------------
    # 3) Keep only strata where georef provides a breakdown (inner join on strata_cols)
    # -------------------------------------------------------------------------
    # After the join, each unknown nominal row is replicated across all georef dim_col
    # categories for that stratum, with extra columns:
    # - val, prop, total
    # - and the georef dim_col value in a column named paste0(dim_col, "_geo")
    to_specify <- unknown_rows %>%
      dplyr::inner_join(geo_props, by = strata_cols, suffix = c("", "_geo"))
    
    # If georef has no detail for those strata, we cannot specify them.
    if (nrow(to_specify) == 0) next
    
    # -------------------------------------------------------------------------
    # 4) Create specified rows (strong vs weak behavior)
    # -------------------------------------------------------------------------
    if (mode == "weak") {
      
      # Split cases:
      # A) nominal unknown <= georef total: safe to redistribute by proportions
      # B) nominal unknown >  georef total: georef doesn't "cover" the nominal total
      #    -> copy georef detailed values (val) and keep leftover as unknown.
      prop_diff_de_un_diff <- to_specify %>%
        dplyr::filter(measurement_value > total)
      
      prop_diff_de_un_prop <- to_specify %>%
        dplyr::filter(measurement_value <= total)
      
      # ---- Case A: proportional redistribution ----
      prop_diff_de_un_prop_specified <- prop_diff_de_un_prop %>%
        dplyr::mutate(
          measurement_value = measurement_value * prop,
          !!dim_sym := !!rlang::sym(paste0(dim_col, "_geo"))
        ) %>%
        dplyr::select(-val, -prop, -ends_with("_geo"))
      
      # ---- Case B: copy georef detail + leftover unknown ----
      # 1) Replace measurement_value by georef `val` (detailed totals)
      # 2) Compute leftover: unknown_to_add = nominal_total - georef_total
      prop_diff_part <- prop_diff_de_un_diff %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          total_nom         = measurement_value,  # original nominal unknown value
          measurement_value = val,                # copy georef detailed value
          !!dim_sym         := .data[[paste0(dim_col, "_geo")]],
          unknown_to_add    = total_nom - total   # leftover after copying georef total
        ) %>%
        dplyr::select(-val, -prop, -ends_with("_geo"))
      
      # To add the leftover unknown row, we need to group by all columns except:
      # - measurement_value (we will set it)
      # - dim_col (we will set it back to unk_code)
      group_cols <- setdiff(names(prop_diff_part), c("measurement_value", dim_col))
      
      data_init <- prop_diff_part
      
      # Build one "leftover" unknown row per stratum
      prop_diff_specified <- prop_diff_part %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::slice(1) %>%  # keep one row per group to carry identifiers
        dplyr::mutate(
          measurement_value = unknown_to_add,
          !!rlang::sym(dim_col) := unk_code
        ) %>%
        dplyr::ungroup()
      
      # Combine detailed copied rows + leftover unknown rows
      prop_diff_specified_final <- rbind(prop_diff_specified, data_init)
      
      # Combine Case A + Case B and keep nominal_df structure
      specified <- rbind(
        prop_diff_de_un_prop_specified %>%
          dplyr::select(dplyr::all_of(names(nominal_df))),
        prop_diff_specified_final %>%
          dplyr::select(dplyr::all_of(names(nominal_df)))
      )
      
    } else {
      
      # ---- Strong mode: always redistribute nominal unknown by georef proportions ----
      specified <- to_specify %>%
        dplyr::mutate(
          measurement_value = measurement_value * prop,
          !!dim_sym := !!rlang::sym(paste0(dim_col, "_geo"))
        ) %>%
        dplyr::select(-val, -prop, -ends_with("_geo"))
    }
    
    # -------------------------------------------------------------------------
    # 5) Replace original unknown nominal rows (for those strata) by `specified`
    # -------------------------------------------------------------------------
    # `replaced_keys` identifies the exact rows to remove from nominal_df.
    # We remove by all columns except measurement_value (so we don't require exact equality of the value).
    replaced_keys <- to_specify %>%
      dplyr::select(dplyr::all_of(names(nominal_df))) %>%
      dplyr::distinct()
    
    nominal_df <- nominal_df %>%
      dplyr::anti_join(
        replaced_keys,
        by = setdiff(names(nominal_df), "measurement_value")
      ) %>%
      dplyr::bind_rows(specified)
    
    # -------------------------------------------------------------------------
    # 6) Log how much was specified (tons vs numbers)
    # -------------------------------------------------------------------------
    raised_t <- specified %>%
      dplyr::ungroup() %>%
      dplyr::filter(measurement_unit %in% c("t", "MT", "MTNO")) %>%
      dplyr::summarise(sum_val = sum(measurement_value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(sum_val)
    
    raised_no <- specified %>%
      dplyr::ungroup() %>%
      dplyr::filter(measurement_unit %in% c("no", "NO", "NOMT")) %>%
      dplyr::summarise(sum_val = sum(measurement_value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(sum_val)
    
    logger(sprintf("Specified %s — %.3f t, %.3f no", dim_col, raised_t, raised_no))
  }
  
  # ---- Final cleanup: re-aggregate duplicates created by bind_rows ----
  # After specifying, multiple rows can collapse to identical strata; we sum them.
  nominal_df <- nominal_df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(-measurement_value)) %>%
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), .groups = "drop")
  
  nominal_df
}