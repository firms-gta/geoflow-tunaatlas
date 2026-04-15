check_unique_strata_georef_nominal <- function(rawdata, nominal_catch) {
  
  dims <- c(
    "source_authority",
    # "fishing_mode_wcpfc_issue_unk_solved",
    # 
    # "group_species_iattc_sharks",
    "gear_type",
    # "geographic_identifier_nom",
    # "year",
    # "fishing_fleet"
    
  )
  
  # ajouter year dans rawdata si absent
  if (!"year" %in% names(rawdata)) {
    rawdata$year <- as.integer(substr(rawdata$time_start, 1, 4))
  }
  
  # garder seulement les colonnes utiles + enlever doublons
  georef_strata <- rawdata[, dims] |> unique()
  nominal_strata <- nominal_catch[, dims] |> unique()
  
  # combinaisons seulement dans georef
  only_georef <- dplyr::anti_join(
    georef_strata,
    nominal_strata,
    by = dims
  )
  
  # combinaisons seulement dans nominal
  only_nominal <- dplyr::anti_join(
    nominal_strata,
    georef_strata,
    by = dims
  )
  
  list(
    only_georef = only_georef,
    only_nominal = only_nominal,
    summary_only_georef = only_georef |>
      dplyr::count(source_authority, name = "n_only_georef"),
    summary_only_nominal = only_nominal |>
      dplyr::count(source_authority, name = "n_only_nominal")
  )
}


check_multiple_strata_georef_nominal <- function(rawdata, nominal_catch,
                                                 dims_list = list(
                                                   c("source_authority", "fishing_fleet"),
                                                   c("source_authority", "gear_type"),
                                                   c("source_authority", "fishing_mode_wcpfc_issue_unk_solved"),
                                                   c("source_authority", "group_species_iattc_sharks"),
                                                   c("source_authority", "fishing_fleet", "gear_type"),
                                                   c("source_authority", "fishing_fleet", "year"),
                                                   c("source_authority", "fishing_fleet", "group_species_iattc_sharks"),
                                                   c("source_authority", "fishing_fleet", "year")
                                                   
                                                 )) {
  
  if (!"year" %in% names(rawdata)) {
    rawdata$year <- as.integer(substr(rawdata$time_start, 1, 4))
  }
  
  res <- lapply(dims_list, function(dims) {
    
    missing_raw <- setdiff(dims, names(rawdata))
    missing_nom <- setdiff(dims, names(nominal_catch))
    
    if (length(missing_raw) > 0 || length(missing_nom) > 0) {
      return(list(
        dims = dims,
        error = paste(
          "Missing columns:",
          paste(unique(c(missing_raw, missing_nom)), collapse = ", ")
        )
      ))
    }
    
    georef_strata <- unique(rawdata[, dims, drop = FALSE])
    nominal_strata <- unique(nominal_catch[, dims, drop = FALSE])
    
    only_georef <- dplyr::anti_join(georef_strata, nominal_strata, by = dims)
    only_nominal <- dplyr::anti_join(nominal_strata, georef_strata, by = dims)
    
    list(
      dims = dims,
      only_georef = only_georef,
      only_nominal = only_nominal,
      summary_only_georef = dplyr::count(only_georef, source_authority, name = "n_only_georef"),
      summary_only_nominal = dplyr::count(only_nominal, source_authority, name = "n_only_nominal")
    )
  })
  
  names(res) <- vapply(dims_list, paste, collapse = " + ", FUN.VALUE = character(1))
  res
}

# res_tests <- check_multiple_strata_georef_nominal(georef, nominal_catch)

extract_incremental_only_georef <- function(res) {
  
  out <- vector("list", length(res))
  
  previous_only_georef <- NULL
  previous_dims <- NULL
  
  for (i in seq_along(res)) {
    
    x <- res[[i]]
    
    if (!is.null(x$error)) {
      out[[i]] <- x
      next
    }
    
    dims <- x$dims
    only_georef <- x$only_georef
    
    # Par défaut, on garde tout
    only_georef_incremental <- only_georef
    
    # Si le niveau précédent est un sous-ensemble du niveau actuel,
    # on enlève ce qui existait déjà au niveau précédent
    if (!is.null(previous_only_georef) &&
        !is.null(previous_dims) &&
        all(previous_dims %in% dims)) {
      
      previous_projected <- unique(previous_only_georef[, previous_dims, drop = FALSE])
      
      only_georef_incremental <- only_georef %>%
        dplyr::anti_join(previous_projected, by = previous_dims)
    }
    
    out[[i]] <- c(
      x,
      list(
        only_georef_incremental = only_georef_incremental,
        summary_only_georef_incremental = dplyr::count(
          only_georef_incremental,
          source_authority,
          name = "n_only_georef_incremental"
        )
      )
    )
    
    previous_only_georef <- only_georef
    previous_dims <- dims
  }
  
  names(out) <- names(res)
  out
}

create_mismatch_report_rmd <- function(res_tests, output_file = "mismatch_report.Rmd") {
  
  lines <- c(
    "---",
    "title: \"Georef vs Nominal - Strata Comparison Report\"",
    "output: html_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(dplyr)",
    "```",
    "",
    "# Summary",
    "",
    "```{r}",
    "make_summary_report <- function(res_tests) {",
    "  do.call(rbind, lapply(res_tests, function(x) {",
    "    test_name <- paste(x$dims, collapse = ' + ')",
    "    if (!is.null(x$error)) {",
    "      return(data.frame(test = test_name, n_only_georef = NA, n_only_nominal = NA, diff = NA, status = 'ERROR'))",
    "    }",
    "    n_geo <- nrow(x$only_georef)",
    "    n_nom <- nrow(x$only_nominal)",
    "    data.frame(",
    "      test = test_name,",
    "      n_only_georef = n_geo,",
    "      n_only_nominal = n_nom,",
    "      diff = n_geo - n_nom,",
    "      status = ifelse(n_geo == 0 & n_nom == 0, 'OK', 'MISMATCH')",
    "    )",
    "  }))",
    "}",
    "",
    "report <- make_summary_report(res_tests)",
    "report <- report[order(-abs(report$diff)), ]",
    "report",
    "```",
    "",
    "# Detailed results"
  )
  
  # Ajouter une section par test
  for (name in names(res_tests)) {
    
    safe_name <- gsub("\\+", "_", name)
    
    section <- c(
      "",
      paste0("## ", name),
      "",
      "```{r}",
      paste0("x <- res_tests[['", name, "']]"),
      "if (!is.null(x$error)) {",
      "  print(x$error)",
      "} else {",
      "  cat('Only in georef:', nrow(x$only_georef), '\\n')",
      "  cat('Only in nominal:', nrow(x$only_nominal), '\\n')",
      "",
      "  cat('\\n### Summary by source_authority (georef)\\n')",
      "  print(x$summary_only_georef)",
      "",
      "  cat('\\n### Summary by source_authority (nominal)\\n')",
      "  print(x$summary_only_nominal)",
      "",
      "  cat('\\n### Sample only_georef\\n')",
      "  print(head(x$only_georef, 10))",
      "",
      "  cat('\\n### Sample only_nominal\\n')",
      "  print(head(x$only_nominal, 10))",
      "}",
      "```"
    )
    
    lines <- c(lines, section)
  }
  
  writeLines(lines, output_file)
  
  message("RMarkdown report created: ", output_file)
}

dims_progressive <- list(
  c("source_authority", "group_species_iattc_sharks"),
  c("source_authority", "gear_type"),
  c("source_authority", "fishing_fleet"),
  c("source_authority", "group_species_iattc_sharks", "year"),
  c("source_authority", "gear_type", "year"),
  c("source_authority", "fishing_fleet", "year"),
  c("source_authority", "fishing_fleet", "gear_type")
)

extract_incremental_only_georef_by_year <- function(res) {
  
  out <- vector("list", length(res))
  names(out) <- names(res)
  
  dims_strings <- vapply(res, function(x) {
    if (!is.null(x$dims)) paste(sort(x$dims), collapse = "|") else NA_character_
  }, FUN.VALUE = character(1))
  
  for (i in seq_along(res)) {
    x <- res[[i]] 
    
    if (!is.null(x$error)) {
      out[[i]] <- x
      next
    }
    
    dims <- x$dims
    only_georef <- x$only_georef
    
    only_georef_incremental <- only_georef
    
    # Seulement pour les cas où year est présent
    if ("year" %in% dims) {
      
      base_dims <- setdiff(dims, "year")
      base_key <- paste(sort(base_dims), collapse = "|")
      base_index <- match(base_key, dims_strings)
      
      # Si on retrouve la version correspondante sans year
      if (!is.na(base_index)) {
        base_only_georef <- res[[base_index]]$only_georef
        
        if (!is.null(base_only_georef) && nrow(base_only_georef) > 0) {
          base_projected <- unique(base_only_georef[, base_dims, drop = FALSE])
          
          only_georef_incremental <- only_georef %>%
            dplyr::anti_join(base_projected, by = base_dims)
        }
      }
    }
    
    out[[i]] <- c(
      x,
      list(
        only_georef_incremental = only_georef_incremental,
        summary_only_georef_incremental = dplyr::count(
          only_georef_incremental,
          source_authority,
          name = "n_only_georef_incremental"
        )
      )
    )
  }
  
  out
}

# res_base <- check_multiple_strata_georef_nominal(
#   rawdata = georef,
#   nominal_catch = nominal_catch,
#   dims_list = dims_progressive
# )
# 
# res_incremental <- extract_incremental_only_georef_by_year(res_base)

# lapply(res_incremental, function(x) {
#   
#   a <- x$only_georef_incremental
#   if("fishing_fleet"%in%colnames(a)){
#     a <- a %>% dplyr::filter(fishing_fleet != "NEI")
#   }
#   if("gear_type"%in%colnames(a)){
#     a <- a %>% dplyr::filter(gear_type != "99.9")
#   }
#   a
# })

# create_mismatch_report_rmd(res_tests)

# rmarkdown::render("mismatch_report.Rmd")


library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
is_candidate_acceptable <- function(df, allow_generic_fallback = FALSE) {
  
  ok <- rep(TRUE, nrow(df))
  
  generic_fleet <- "NEI"
  generic_gear  <- "99.9"
  
  # ============================================================
  # fishing_fleet
  # - exact match: OK
  # - nominal = NEI: only allowed in fallback pass
  # - different specific fleet: rejected
  # ============================================================
  if ("fishing_fleet_georef" %in% names(df) && "fishing_fleet_nominal" %in% names(df)) {
    
    same <- !is.na(df$fishing_fleet_georef) &
      !is.na(df$fishing_fleet_nominal) &
      df$fishing_fleet_georef == df$fishing_fleet_nominal
    
    nominal_generic <- !is.na(df$fishing_fleet_nominal) &
      df$fishing_fleet_nominal %in% generic_fleet
    
    different_specific <- !is.na(df$fishing_fleet_georef) &
      !is.na(df$fishing_fleet_nominal) &
      df$fishing_fleet_georef != df$fishing_fleet_nominal &
      !nominal_generic
    
    ok[different_specific] <- FALSE
    
    if (!allow_generic_fallback) {
      ok[!same & nominal_generic] <- FALSE
    }
  }
  
  # ============================================================
  # gear_type
  # - exact match: OK
  # - same family but different: OK
  # - nominal = 99.9: only allowed in fallback pass
  # - different family: rejected
  # ============================================================
  if ("gear_type_georef" %in% names(df) && "gear_type_nominal" %in% names(df)) {
    
    same <- !is.na(df$gear_type_georef) &
      !is.na(df$gear_type_nominal) &
      df$gear_type_georef == df$gear_type_nominal
    
    nominal_generic <- !is.na(df$gear_type_nominal) &
      df$gear_type_nominal %in% generic_gear
    
    prefix_georef  <- substr(df$gear_type_georef, 1, 2)
    prefix_nominal <- substr(df$gear_type_nominal, 1, 2)
    
    different_other_family <- !is.na(df$gear_type_georef) &
      !is.na(df$gear_type_nominal) &
      df$gear_type_georef != df$gear_type_nominal &
      !nominal_generic &
      prefix_georef != prefix_nominal
    
    ok[different_other_family] <- FALSE
    
    if (!allow_generic_fallback) {
      ok[!same & nominal_generic] <- FALSE
    }
  }
  
  ok
}

propose_georef_to_nominal_mappings_clean <- function(
    georef,
    nominal,
    id_cols = c("source_authority", "group_species_iattc_sharks", "year"),
    candidate_cols_order = c("gear_type", "fishing_mode_wcpfc_issue_unk_solved", "geographic_identifier_nom", "fishing_fleet"),
    max_relaxed = 2,
    use_only_unmatched_nominal_first = FALSE,
    verbose = TRUE
) {
  
  clean_char_cols <- function(df) {
    df[] <- lapply(df, function(x) {
      if (is.character(x)) trimws(x) else x
    })
    df
  }

  georef <- georef %>% dplyr::mutate(year = as.character(lubridate::year(time_start))) %>% 
    #on converti toutes les données en nombre en tonnes en mettant un facteur de converiosn de 100 kilos, c'est juste pour estimer c'est pas trop utilisé dans le score
  dplyr::mutate(measurement_value = ifelse(measurement_unit == "t", measurement_value, measurement_value * 0.01))
    georef <- clean_char_cols(georef)
  nominal <- clean_char_cols(nominal)
  
  nominal <- nominal %>% dplyr::mutate(year = as.character(lubridate::year(time_start))) 
  
  all_cols_needed <- unique(c(id_cols, candidate_cols_order, "measurement_value"))
  
  missing_georef <- setdiff(all_cols_needed, names(georef))
  missing_nominal <- setdiff(all_cols_needed, names(nominal))
  
  if (length(missing_georef) > 0) {
    stop("Missing in georef: ", paste(missing_georef, collapse = ", "))
  }
  if (length(missing_nominal) > 0) {
    stop("Missing in nominal: ", paste(missing_nominal, collapse = ", "))
  }
  
  group_cols <- unique(c(id_cols, candidate_cols_order))
  
  georef <- georef %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      measurement_value = sum(measurement_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  nominal <- nominal %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      measurement_value = sum(measurement_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  exact_by <- c(id_cols, candidate_cols_order)
  
  georef_exact_keys <- georef %>% dplyr::distinct(dplyr::across(dplyr::all_of(exact_by)))
  nominal_exact_keys <- nominal %>% dplyr::distinct(dplyr::across(dplyr::all_of(exact_by)))
  
  georef_unmatched <- georef %>%
    dplyr::anti_join(nominal_exact_keys, by = exact_by) %>%
    dplyr::mutate(.georef_row_id = dplyr::row_number())
  
  nominal_unmatched <- nominal %>%
    dplyr::anti_join(georef_exact_keys, by = exact_by) %>%
    dplyr::mutate(.nominal_row_id = dplyr::row_number())
  
  nominal_all <- nominal %>%
    dplyr::mutate(.nominal_row_id = dplyr::row_number())
  
  nominal_pool <- if (use_only_unmatched_nominal_first) nominal_unmatched else nominal_all
  
  if (verbose) {
    message("nrow(georef distinct): ", nrow(georef))
    message("nrow(nominal distinct): ", nrow(nominal))
    message("nrow(georef_unmatched): ", nrow(georef_unmatched))
    message("nrow(nominal_unmatched): ", nrow(nominal_unmatched))
  }
  
  all_relaxations <- unlist(
    lapply(seq_len(max_relaxed), function(k) {
      combn(candidate_cols_order, k, simplify = FALSE)
    }),
    recursive = FALSE
  )
  
  extra_relaxations_for_remaining <- unlist(
    lapply(3:length(candidate_cols_order), function(k) {
      combn(candidate_cols_order, k, simplify = FALSE)
    }),
    recursive = FALSE
  )
  
  extra_relaxations_for_remaining <- extra_relaxations_for_remaining[
    vapply(
      extra_relaxations_for_remaining,
      function(x) all(x %in% candidate_cols_order),
      logical(1)
    )
  ]
  
  # enlever les doublons déjà présents dans all_relaxations
  relaxation_key <- function(x) paste(sort(x), collapse = "|")
  
  existing_keys <- vapply(all_relaxations, relaxation_key, character(1))
  extra_keys <- vapply(extra_relaxations_for_remaining, relaxation_key, character(1))
  
  extra_relaxations_for_remaining <- extra_relaxations_for_remaining[
    !(extra_keys %in% existing_keys)
  ]
  
  remaining_georef <- georef_unmatched
  
  build_candidates_for_relaxation <- function(
    georef_input,
    nominal_pool,
    relaxed_cols,
    allow_generic_fallback = FALSE
  ) {
    strict_cols <- setdiff(candidate_cols_order, relaxed_cols)
    join_cols <- c(id_cols, strict_cols)
    
    cand <- georef_input %>%
      dplyr::inner_join(
        nominal_pool,
        by = join_cols,
        suffix = c("_georef", "_nominal")
      )
    
    if (nrow(cand) == 0) return(NULL)
    
    for (col in candidate_cols_order) {
      georef_col <- paste0(col, "_georef")
      nominal_col <- paste0(col, "_nominal")
      
      if (!georef_col %in% names(cand) && col %in% names(cand)) {
        cand[[georef_col]] <- cand[[col]]
      }
      if (!nominal_col %in% names(cand) && col %in% names(cand)) {
        cand[[nominal_col]] <- cand[[col]]
      }
      
      cand[[paste0("diff_", col)]] <-
        dplyr::coalesce(as.character(cand[[georef_col]]), "__NA__") !=
        dplyr::coalesce(as.character(cand[[nominal_col]]), "__NA__")
    }
    
    for (col in id_cols) {
      georef_col <- paste0(col, "_georef")
      nominal_col <- paste0(col, "_nominal")
      
      if (!georef_col %in% names(cand) && col %in% names(cand)) {
        cand[[georef_col]] <- cand[[col]]
      }
      if (!nominal_col %in% names(cand) && col %in% names(cand)) {
        cand[[nominal_col]] <- cand[[col]]
      }
    }
    
    diff_flag_cols <- paste0("diff_", candidate_cols_order)
    
    cand <- cand %>%
      dplyr::mutate(
        n_different_dims = rowSums(as.data.frame(dplyr::across(dplyr::all_of(diff_flag_cols)))),
        different_dims = apply(
          as.data.frame(dplyr::across(dplyr::all_of(diff_flag_cols))),
          1,
          function(z) {
            cols <- candidate_cols_order[as.logical(z)]
            if (length(cols) == 0) "" else paste(cols, collapse = " + ")
          }
        ),
        relaxed_cols = paste(relaxed_cols, collapse = " + "),
        proposal_rank = length(relaxed_cols)
      ) %>%
      dplyr::filter(n_different_dims > 0) %>%
      dplyr::filter(n_different_dims <= proposal_rank)
    
    if (nrow(cand) == 0) return(NULL)
    
    cand <- cand %>%
      dplyr::mutate(
        acceptable_candidate = is_candidate_acceptable(
          .,
          allow_generic_fallback = allow_generic_fallback
        )
      ) %>%
      dplyr::filter(acceptable_candidate)
    
    if (nrow(cand) == 0) return(NULL)
    
    cand %>%
      dplyr::mutate(
        preference_score = preference_score(.)
      )
  }
  
  preference_score <- function(df) {
    score <- rep(0, nrow(df))
    
    # ============================================================
    # DEFINE GENERIC CODES
    # These codes represent "fallback" or non-informative categories.
    # They should NOT be penalized when used, but also not rewarded,
    # except when they exactly match the georef value.
    # ============================================================
    
    generic_fleet <- c("NEI")
    generic_mode  <- c("UNK")
    generic_gear  <- c("99.9")
    
    # ============================================================
    # FISHING FLEET COMPARISON
    #
    # Logic:
    # - Exact match → strong bonus (this is ideal)
    # - Different value → strong penalty
    # - Exception: if nominal = NEI → no penalty (neutral fallback)
    #
    # Examples:
    # AT vs AT   → +200
    # AT vs NEI  → 0
    # AT vs IOTC → -1000
    # NEI vs NEI → +200
    # ============================================================
    
    if ("fishing_fleet_georef" %in% names(df) && "fishing_fleet_nominal" %in% names(df)) {
      
      same <- !is.na(df$fishing_fleet_georef) &
        !is.na(df$fishing_fleet_nominal) &
        df$fishing_fleet_georef == df$fishing_fleet_nominal
      
      diff <- !is.na(df$fishing_fleet_georef) &
        !is.na(df$fishing_fleet_nominal) &
        df$fishing_fleet_georef != df$fishing_fleet_nominal &
        !df$fishing_fleet_nominal %in% generic_fleet
      
      score[same] <- score[same] + 200
      score[diff] <- score[diff] - 1000
    }
    
    # ============================================================
    # GEAR TYPE COMPARISON
    #
    # Logic:
    # - Exact match → bonus
    # - Same family (e.g. 09.1 vs 09.39) → moderate penalty
    # - Different family (e.g. 09 vs 01) → strong penalty
    # - Exception: if nominal = 99.9 → no penalty (neutral fallback)
    #
    # This ensures we prefer:
    #   exact match > similar gear > fallback > wrong gear
    # ============================================================
    
    if ("gear_type_georef" %in% names(df) && "gear_type_nominal" %in% names(df)) {
      
      same <- !is.na(df$gear_type_georef) &
        !is.na(df$gear_type_nominal) &
        df$gear_type_georef == df$gear_type_nominal
      
      prefix_georef <- substr(df$gear_type_georef, 1, 2)
      prefix_nominal <- substr(df$gear_type_nominal, 1, 2)
      
      diff_same_family <- !is.na(df$gear_type_georef) &
        !is.na(df$gear_type_nominal) &
        df$gear_type_georef != df$gear_type_nominal &
        !df$gear_type_nominal %in% generic_gear &
        prefix_georef == prefix_nominal
      
      diff_other_family <- !is.na(df$gear_type_georef) &
        !is.na(df$gear_type_nominal) &
        df$gear_type_georef != df$gear_type_nominal &
        !df$gear_type_nominal %in% generic_gear &
        prefix_georef != prefix_nominal
      
      score[same] <- score[same] + 100
      score[diff_same_family] <- score[diff_same_family] - 150
      score[diff_other_family] <- score[diff_other_family] - 500
    }
    
    # ============================================================
    # FISHING MODE COMPARISON
    #
    # Logic:
    # - Exact match → bonus
    # - Different value → penalty
    # - Exception: if nominal = UNK → no penalty (neutral fallback)
    #
    # This avoids rewarding UNK but also avoids penalizing it,
    # as it represents missing or unresolved information.
    # ============================================================
    
    if ("fishing_mode_wcpfc_issue_unk_solved_georef" %in% names(df) &&
        "fishing_mode_wcpfc_issue_unk_solved_nominal" %in% names(df)) {
      
      same <- !is.na(df$fishing_mode_wcpfc_issue_unk_solved_georef) &
        !is.na(df$fishing_mode_wcpfc_issue_unk_solved_nominal) &
        df$fishing_mode_wcpfc_issue_unk_solved_georef ==
        df$fishing_mode_wcpfc_issue_unk_solved_nominal
      
      diff <- !is.na(df$fishing_mode_wcpfc_issue_unk_solved_georef) &
        !is.na(df$fishing_mode_wcpfc_issue_unk_solved_nominal) &
        df$fishing_mode_wcpfc_issue_unk_solved_georef !=
        df$fishing_mode_wcpfc_issue_unk_solved_nominal &
        !df$fishing_mode_wcpfc_issue_unk_solved_nominal %in% generic_mode
      
      score[same] <- score[same] + 100
      score[diff] <- score[diff] - 200
    }
    
    # ============================================================
    # VOLUME CONSISTENCY CHECK
    #
    # Logic:
    # - If georef <= nominal → small bonus (consistent allocation)
    # - If georef > nominal → penalty (over-allocation)
    # - Larger exceedance → stronger penalty
    #
    # Also includes a very small continuous penalty to break ties.
    # ============================================================
    
    if ("measurement_value_georef" %in% names(df) &&
        "measurement_value_nominal" %in% names(df)) {
      
      ratio <- ifelse(
        !is.na(df$measurement_value_nominal) & df$measurement_value_nominal != 0,
        df$measurement_value_georef / df$measurement_value_nominal,
        NA_real_
      )
      
      score <- score + ifelse(!is.na(ratio) & ratio <= 1, 10, 0)
      score <- score - ifelse(!is.na(ratio) & ratio > 1, 50, 0)
      score <- score - ifelse(!is.na(ratio) & ratio > 1.2, 100, 0)
      
      # tiny penalty to avoid ties between equal candidates
      score <- score - ratio / 10000
    }
    
    score
  }
  
  for (i in seq_along(all_relaxations)) {
    
    relaxed_cols <- all_relaxations[[i]]
    
    if (verbose) {
      message("----")
      message("iteration ", i, " / ", length(all_relaxations))
      message("relaxed_cols: ", paste(relaxed_cols, collapse = " + "))
      message("remaining georef: ", nrow(remaining_georef))
    }
    
    if (nrow(remaining_georef) == 0) break
    
    cand <- build_candidates_for_relaxation(
      georef_input = remaining_georef,
      nominal_pool = nominal_pool,
      relaxed_cols = relaxed_cols
    )
    
    if (verbose) {
      message("nrow(cand after join/filter): ", ifelse(is.null(cand), 0, nrow(cand)))
    }
    
    if (is.null(cand) || nrow(cand) == 0) next
    
    cand <- cand %>%
      dplyr::group_by(.georef_row_id) %>%
      dplyr::mutate(
        n_candidates_same_rank = dplyr::n(),
        has_multiple_candidates = dplyr::n() > 1
      ) %>%
      dplyr::ungroup()
    
    if (verbose) {
      message("nrow(cand kept for this relaxation): ", nrow(cand))
    }
  }
  
  # ============================================================
  # PASS 1: strict/coherent candidates only
  # - no fallback generic codes
  # - reject incompatible specific substitutions
  # ============================================================
  
  all_candidates_pass1 <- lapply(all_relaxations, function(relaxed_cols) {
    build_candidates_for_relaxation(
      georef_input = georef_unmatched,
      nominal_pool = nominal_pool,
      relaxed_cols = relaxed_cols,
      allow_generic_fallback = FALSE
    )
  })
  
  all_candidates_pass1_df <- dplyr::bind_rows(all_candidates_pass1)
  
  if (nrow(all_candidates_pass1_df) > 0) {
    all_candidates_pass1_df <- all_candidates_pass1_df %>%
      dplyr::group_by(.georef_row_id, proposal_rank) %>%
      dplyr::mutate(
        n_candidates_same_rank = dplyr::n(),
        has_multiple_candidates = dplyr::n() > 1,
        matching_pass = "strict"
      ) %>%
      dplyr::ungroup()
    
    candidate_mappings_pass1 <- all_candidates_pass1_df %>%
      dplyr::arrange(
        .georef_row_id,
        dplyr::desc(preference_score),
        n_different_dims,
        proposal_rank,
        .nominal_row_id
      ) %>%
      dplyr::group_by(.georef_row_id) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  } else {
    candidate_mappings_pass1 <- tibble::tibble()
  }
  
  matched_georef_ids_pass1 <- candidate_mappings_pass1$.georef_row_id
  
  remaining_after_pass1 <- georef_unmatched %>%
    dplyr::filter(!.georef_row_id %in% matched_georef_ids_pass1)
  
  # ============================================================
  # PASS 2: fallback allowed
  # - NEI / 99.9 / UNK allowed
  # - but still reject incoherent specific substitutions
  # ============================================================
  
  all_candidates_pass2 <- lapply(all_relaxations, function(relaxed_cols) {
    build_candidates_for_relaxation(
      georef_input = remaining_after_pass1,
      nominal_pool = nominal_pool,
      relaxed_cols = relaxed_cols,
      allow_generic_fallback = TRUE
    )
  })
  
  all_candidates_pass2_df <- dplyr::bind_rows(all_candidates_pass2)
  
  if (nrow(all_candidates_pass2_df) > 0) {
    all_candidates_pass2_df <- all_candidates_pass2_df %>%
      dplyr::group_by(.georef_row_id, proposal_rank) %>%
      dplyr::mutate(
        n_candidates_same_rank = dplyr::n(),
        has_multiple_candidates = dplyr::n() > 1,
        matching_pass = "fallback"
      ) %>%
      dplyr::ungroup()
    
    candidate_mappings_pass2 <- all_candidates_pass2_df %>%
      dplyr::arrange(
        .georef_row_id,
        dplyr::desc(preference_score),
        n_different_dims,
        proposal_rank,
        .nominal_row_id
      ) %>%
      dplyr::group_by(.georef_row_id) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  } else {
    candidate_mappings_pass2 <- tibble::tibble()
  }
  
  matched_georef_ids_pass2 <- candidate_mappings_pass2$.georef_row_id
  
  remaining_after_pass2 <- remaining_after_pass1 %>%
    dplyr::filter(!.georef_row_id %in% matched_georef_ids_pass2)
  
  # ============================================================
  # PASS 3: extra targeted relaxations on still unmatched georef
  # - generic fallback allowed
  # - still reject incoherent specific substitutions
  # ============================================================
  
  all_candidates_pass3 <- lapply(extra_relaxations_for_remaining, function(relaxed_cols) {
    build_candidates_for_relaxation(
      georef_input = remaining_after_pass2,
      nominal_pool = nominal_pool,
      relaxed_cols = relaxed_cols,
      allow_generic_fallback = TRUE
    )
  })
  
  all_candidates_pass3_df <- dplyr::bind_rows(all_candidates_pass3)
  
  if (nrow(all_candidates_pass3_df) > 0) {
    all_candidates_pass3_df <- all_candidates_pass3_df %>%
      dplyr::group_by(.georef_row_id, proposal_rank) %>%
      dplyr::mutate(
        n_candidates_same_rank = dplyr::n(),
        has_multiple_candidates = dplyr::n() > 1,
        matching_pass = "extra_relaxed"
      ) %>%
      dplyr::ungroup()
    
    candidate_mappings_pass3 <- all_candidates_pass3_df %>%
      dplyr::arrange(
        .georef_row_id,
        dplyr::desc(preference_score),
        n_different_dims,
        proposal_rank,
        .nominal_row_id
      ) %>%
      dplyr::group_by(.georef_row_id) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  } else {
    candidate_mappings_pass3 <- tibble::tibble()
  }
  
  all_candidates_df <- dplyr::bind_rows(
    all_candidates_pass1_df,
    all_candidates_pass2_df,
    all_candidates_pass3_df
  )
  candidate_mappings <- dplyr::bind_rows(
    candidate_mappings_pass1,
    candidate_mappings_pass2,
    candidate_mappings_pass3
  )
  remaining_georef <- remaining_after_pass2 %>%
    dplyr::filter(!.georef_row_id %in% candidate_mappings_pass3$.georef_row_id)
  
  if (nrow(candidate_mappings) == 0) {
    return(list(
      candidate_mappings = tibble::tibble(),
      summary_mapping = tibble::tibble(),
      remaining_georef = remaining_georef,
      georef_unmatched = georef_unmatched,
      nominal_unmatched = nominal_unmatched
    ))
  }
  
  chosen_pairs <- candidate_mappings %>%
    dplyr::distinct(.georef_row_id, .nominal_row_id) %>%
    dplyr::mutate(chosen = TRUE)
  
  ambiguous_cases <- all_candidates_df %>%
    dplyr::filter(has_multiple_candidates) %>%
    dplyr::left_join(chosen_pairs, by = c(".georef_row_id", ".nominal_row_id")) %>%
    dplyr::mutate(chosen = dplyr::coalesce(chosen, FALSE))
  
  ambiguous_cases_clean <- ambiguous_cases %>%
    dplyr::transmute(
      .georef_row_id,
      source_authority = source_authority_georef,
      species = group_species_iattc_sharks_georef,
      year = year_georef,
      
      georef = paste(
        gear_type_georef,
        fishing_mode_wcpfc_issue_unk_solved_georef,
        geographic_identifier_nom_georef,
        fishing_fleet_georef,
        sep = " | "
      ),
      
      nominal = paste(
        gear_type_nominal,
        fishing_mode_wcpfc_issue_unk_solved_nominal,
        geographic_identifier_nom_nominal,
        fishing_fleet_nominal,
        sep = " | "
      ),
      preference_score,
      different_dims,
      n_different_dims,
      chosen
    ) %>%
    dplyr::arrange(.georef_row_id, desc(chosen), desc(preference_score)) %>% 
    dplyr::distinct()
  
  summary_ambiguity <- ambiguous_cases %>%
    dplyr::group_by(different_dims) %>%
    dplyr::summarise(
      n_cases = dplyr::n_distinct(.georef_row_id),
      avg_candidates = mean(n_candidates_same_rank),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(n_cases))
  
  candidate_mappings <- candidate_mappings %>%
    dplyr::transmute(
      .georef_row_id,
      .nominal_row_id,
      matching_pass,
      source_authority = source_authority_georef,
      species = group_species_iattc_sharks_georef,
      year = year_georef,
      georef_value = measurement_value_georef,
      nominal_value = measurement_value_nominal,
      ratio_georef_nominal = dplyr::if_else(
        is.na(measurement_value_nominal) | measurement_value_nominal == 0,
        NA_real_,
        measurement_value_georef / measurement_value_nominal
      ),
      gear_type_georef,
      gear_type_nominal,
      fishing_mode_georef = fishing_mode_wcpfc_issue_unk_solved_georef,
      fishing_mode_nominal = fishing_mode_wcpfc_issue_unk_solved_nominal,
      geographic_identifier_nom_georef,
      geographic_identifier_nom_nominal,
      fishing_fleet_georef,
      fishing_fleet_nominal,
      diff_gear_type,
      diff_fishing_mode_wcpfc_issue_unk_solved,
      diff_geographic_identifier_nom,
      diff_fishing_fleet,
      different_dims,
      n_different_dims,
      relaxed_cols,
      proposal_rank,
      preference_score,
      n_candidates_same_rank,
      has_multiple_candidates
    )
  
  summary_mapping <- candidate_mappings %>%
    dplyr::group_by(
      source_authority,
      species,
      fishing_fleet_georef,
      fishing_fleet_nominal,
      gear_type_georef,
      gear_type_nominal,
      fishing_mode_georef,
      fishing_mode_nominal,
      geographic_identifier_nom_georef,
      geographic_identifier_nom_nominal,
      different_dims,
      n_different_dims,
      relaxed_cols,
      proposal_rank
    ) %>%
    dplyr::summarise(
      n_cases = dplyr::n(),
      years = paste(sort(unique(year)), collapse = ", "),
      mean_georef = mean(georef_value, na.rm = TRUE),
      mean_nominal = mean(nominal_value, na.rm = TRUE),
      n_with_multiple_candidates = sum(has_multiple_candidates, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(n_different_dims, dplyr::desc(n_cases))
  
  list(
    candidate_mappings = candidate_mappings,
    summary_mapping = summary_mapping,
    remaining_georef = remaining_georef,
    georef_unmatched = georef_unmatched,
    nominal_unmatched = nominal_unmatched, 
    summary_ambiguity = summary_ambiguity, 
    ambiguous_cases_clean = ambiguous_cases_clean, 
    ambiguous_cases = ambiguous_cases
    
  )
}
# rawdata <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260407191950level_2_catch_2026//entities/global_catch_ird_level2_1950_2024/Markdown/Conv_NO_nominal1/ancient.qs")
# nominal_catch <- readRDS("~/firms-gta/geoflow-tunaatlas/jobs/20260407191950level_2_catch_2026//entities/global_catch_ird_level2_1950_2024/data/nominal_catch_for_raising.rds")
# 
# rawdata <- rawdata %>% dplyr::mutate(year = as.character(lubridate::year(time_start))) %>% dplyr::filter(fishing_fleet!="NEI") %>% 
#   dplyr::filter(gear_type!="99.9")
# 
# res_map <- propose_georef_to_nominal_mappings_clean(
#   georef = rawdata,
#   nominal = nominal_catch,
#   id_cols = c("source_authority", "group_species_iattc_sharks", "year"),
#   candidate_cols_order = c("fishing_mode_wcpfc_issue_unk_solved", "gear_type", "geographic_identifier_nom", "fishing_fleet")
# )
# 
# true <- res_map$ambiguous_cases_clean %>% dplyr::filter(chosen) %>% dplyr::filter(preference_score <0)
# 
# ambiguous_problem <- res_map$ambiguous_cases_clean %>% dplyr::inner_join(true %>% dplyr::select(.georef_row_id), by = c(".georef_row_id"))

apply_proposed_mappings_to_raw_georef <- function(
    georef,
    candidate_mappings,
    id_cols = c("source_authority", "group_species_iattc_sharks", "year"),
    candidate_cols_order = c(
      "gear_type",
      "fishing_mode_wcpfc_issue_unk_solved",
      "geographic_identifier_nom",
      "fishing_fleet"
    )
) {
  
  clean_char_cols <- function(df) {
    df[] <- lapply(df, function(x) {
      if (is.character(x)) trimws(x) else x
    })
    df
  }
  
  georef_raw <- clean_char_cols(georef)
  
  year_was_missing <- !"year" %in% names(georef_raw)
  if (year_was_missing) {
    georef_raw <- georef_raw %>%
      dplyr::mutate(year = as.character(lubridate::year(time_start)))
  } else {
    georef_raw <- georef_raw %>%
      dplyr::mutate(year = as.character(year))
  }
  
  group_cols <- unique(c(id_cols, candidate_cols_order))
  
  # Clés de strate au même niveau que le mapping
  georef_keys <- georef_raw %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(.georef_row_id = dplyr::row_number())
  
  # Colonnes utiles du mapping
  mapping_to_join <- candidate_mappings %>%
    dplyr::select(
      .georef_row_id,
      gear_type_nominal,
      fishing_mode_nominal,
      geographic_identifier_nom_nominal,
      fishing_fleet_nominal,
      different_dims,
      n_different_dims,
      relaxed_cols,
      proposal_rank,
      preference_score
    )
  
  # Table de correspondance au niveau des strates
  georef_keys_mapped <- georef_keys %>%
    dplyr::left_join(mapping_to_join, by = ".georef_row_id") %>%
    dplyr::mutate(
      fishing_mode_wcpfc_issue_unk_solved_original = fishing_mode_wcpfc_issue_unk_solved,
      
      # seul fishing_mode est écrasé
      fishing_mode_wcpfc_issue_unk_solved = dplyr::coalesce(
        fishing_mode_nominal,
        fishing_mode_wcpfc_issue_unk_solved
      ),
      
      # les autres sont ajoutées
      gear_type_solved = dplyr::coalesce(
        gear_type_nominal,
        gear_type
      ),
      geographic_identifier_nom_solved = dplyr::coalesce(
        geographic_identifier_nom_nominal,
        geographic_identifier_nom
      ),
      fishing_fleet_solved = dplyr::coalesce(
        fishing_fleet_nominal,
        fishing_fleet
      ),
      
      mapping_applied = !is.na(gear_type_nominal) |
        !is.na(fishing_mode_nominal) |
        !is.na(geographic_identifier_nom_nominal) |
        !is.na(fishing_fleet_nominal)
    ) %>%
    dplyr::select(
      dplyr::all_of(group_cols),
      fishing_mode_wcpfc_issue_unk_solved,
      fishing_mode_wcpfc_issue_unk_solved_original,
      gear_type_solved,
      geographic_identifier_nom_solved,
      fishing_fleet_solved,
      mapping_applied,
      different_dims,
      n_different_dims,
      relaxed_cols,
      proposal_rank,
      preference_score
    )
  
  # On garde rawdata tel quel, avec ajout des colonnes mappées
  out <- georef_raw %>%
    dplyr::select(-dplyr::any_of(c(
      "gear_type_solved",
      "geographic_identifier_nom_solved",
      "fishing_fleet_solved",
      "fishing_mode_wcpfc_issue_unk_solved_original",
      "mapping_applied",
      "different_dims",
      "n_different_dims",
      "relaxed_cols",
      "proposal_rank",
      "preference_score"
    ))) %>%
    dplyr::left_join(
      georef_keys_mapped,
      by = group_cols,
      suffix = c("", "_mapped")
    )
  
  # garder seulement la version mappée de fishing_mode
  if ("fishing_mode_wcpfc_issue_unk_solved_mapped" %in% names(out)) {
    out$fishing_mode_wcpfc_issue_unk_solved <- out$fishing_mode_wcpfc_issue_unk_solved_mapped
    out$fishing_mode_wcpfc_issue_unk_solved_mapped <- NULL
  }
  
  if (year_was_missing) {
    out <- out %>% dplyr::select(-year)
  }
  
  out
}


# georef_solved <- apply_proposed_mappings_to_georef(
#   georef = rawdata,
#   candidate_mappings = res_map$candidate_mappings
# )
