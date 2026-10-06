# geoflow 1.3.0 demande size = 1000 à Zenodo, qui refuse au-delà de 100 (HTTP 400).
f <- file.path(find.package("geoflow"), "actions", "zen4R_deposit_record.R")
x <- readLines(f, warn = FALSE)
if (any(grepl("size = 1000L", x, fixed = TRUE))) {
  writeLines(gsub("size = 1000L", "size = 100L", x, fixed = TRUE), f)
  message("[patch] zen4R_deposit_record.R : size 1000 -> 100")
} else {
  message("[patch] zen4R_deposit_record.R : size deja patché ou ligne introuvable")
}