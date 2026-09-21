# Corrige geometa::GMLUnitDefinition$buildFrom() (unité NA ou non scalaire).
# Idempotent : peut être sourcé plusieurs fois sans empiler les remplacements.
if (requireNamespace("geometa", quietly = TRUE)) {
  local({
    GMLUD <- geometa::GMLUnitDefinition
    if (!isTRUE(attr(GMLUD$buildFrom, "gta_patched"))) {
      orig <- GMLUD$buildFrom
      patched <- function(x, by = "symbol", unitsystem = "udunits2") {
        if (is.null(x) || length(x) != 1 || is.na(x)) return(NULL)
        orig(x, by, unitsystem)
      }
      attr(patched, "gta_patched") <- TRUE
      GMLUD$buildFrom <- patched
      message("[patch] geometa::GMLUnitDefinition$buildFrom() patché (garde-fou NA/non-scalaire).")
    }
  })
}