# geoflow 1.3.0 : lapply() perd l'attribut "source" des entités, donc config$src_entities reste vide
# et exportPIDs() plante. On remet l'attribut avant le return.
f <- file.path(find.package("geoflow"), "metadata", "entity", "entity_handler_dbi_df.R")
x <- readLines(f, warn = FALSE)
if (any(grepl("PATCH: keep source attribute", x, fixed = TRUE))) {
  message("[patch] entity_handler_dbi_df.R : attribut source deja preserve")
} else {
  i <- grep("^\\s*return\\(enriched_entities\\)", x)
  stopifnot(length(i) == 1)
  x <- append(x, c("  # PATCH: keep source attribute (lapply drops it)",
                   "  attr(enriched_entities, \"source\") <- attr(entities, \"source\")"), after = i - 1)
  writeLines(x, f)
  message("[patch] entity_handler_dbi_df.R : attribut source preserve")
}