library(dplyr)
library(readr)

make_readable_mapping_csv <- function(df) {
  df %>%
    arrange(.georef_row_id, desc(chosen), desc(preference_score)) %>%
    group_by(.georef_row_id) %>%
    mutate(
      georef_case = paste("Case", cur_group_id()),
      mapping_status = if_else(chosen, "chosen", "alternative"),
      row_in_case = row_number()
    ) %>%
    ungroup() %>%
    mutate(
      georef_case = if_else(row_in_case == 1, georef_case, ""),
      source_authority = if_else(row_in_case == 1, source_authority, ""),
      species = if_else(row_in_case == 1, species, ""),
      year = if_else(row_in_case == 1, year, ""),
      georef = if_else(row_in_case == 1, georef, "")
    ) %>%
    select(
      georef_case,
      source_authority,
      species,
      year,
      georef,
      mapping_status,
      nominal,
      preference_score,
      different_dims,
      n_different_dims
    )
}


readr::write_csv(make_readable_mapping_csv(df=iotc), "iotc_readable.csv")

