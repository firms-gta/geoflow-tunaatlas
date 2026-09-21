# patch-geoflow-entities.R
#
# Patch for geoflow 0.20240226:
# entity_handler_dbi_df.R overwrites an explicitly configured
# entity$data when a DB table with the same identifier exists.
#
# The handler is sourced dynamically by handle_entities_dbi(), so
# handle_entities_dbi_df() cannot be patched through the namespace.

if (requireNamespace("geoflow", quietly = TRUE)) {
  
  geoflow_path <- system.file(
    "metadata/entity/entity_handler_dbi_df.R",
    package = "geoflow"
  )
  
  if (!nzchar(geoflow_path) || !file.exists(geoflow_path)) {
    
    message(
      "[patch] WARNING: geoflow entity_handler_dbi_df.R not found."
    )
    
  } else {
    
    txt <- readLines(geoflow_path, warn = FALSE)
    
    # Only apply to the expected geoflow implementation.
    if (!any(grepl(
      "create_geoflow_data_from_dbi",
      txt,
      fixed = TRUE
    ))) {
      
      message(
        "[patch] WARNING: unexpected geoflow entity_handler_dbi.R structure - patch not applied."
      )
      
    } else if (any(grepl(
      "PATCH: preserve explicitly configured Data",
      txt,
      fixed = TRUE
    ))) {
      
      message(
        "[patch] geoflow::entity_handler_dbi_df() already patched."
      )
      
    } else {
      
      idx <- which(grepl(
        "^\\s*expected_table_id\\s*=\\s*entity\\$identifiers\\$id",
        txt
      ))
      
      if (length(idx) != 1) {
        
        message(
          "[patch] WARNING: could not locate expected_table_id - patch not applied."
        )
        
      } else {
        
        guard <- c(
          "    # PATCH: preserve explicitly configured Data",
          "    already_configured <- !is.null(entity$data) &&",
          "      !is.null(entity$data$uploadType) &&",
          "      entity$data$uploadType != \"other\" &&",
          "      !is.null(entity$data$sql)",
          "",
          "    if (already_configured) {",
          "      message(sprintf(",
          "        \"[patch] Entity '%s' has explicit Data configuration ('%s' + SQL); skipping DB table auto-enrichment.\",",
          "        entity$identifiers$id,",
          "        entity$data$uploadType",
          "      ))",
          "      return(entity)",
          "    }",
          ""
        )
        
        txt <- append(
          txt,
          guard,
          after = idx - 1
        )
        
        writeLines(txt, geoflow_path)
        
        message(
          "[patch] geoflow::entity_handler_dbi_df() patched successfully."
        )
      }
    }
  }
}