# tests/make_sample_data.R
# Usage : Rscript tests/make_sample_data.R [n_lignes]
args <- commandArgs(trailingOnly = TRUE)
n    <- if (length(args)) as.integer(args[1]) else 5

src <-  here::here("runtime/extracted/all_raw_data_GTA")
dst <- here::here("tests/sample_data")

unlink(dst, recursive = TRUE)

head_file <- function(f, out) {
  ext <- tolower(tools::file_ext(f))
  if (ext %in% c("csv", "txt", "tsv")) {
    # lecture brute : garde séparateur, encodage et guillemets tels quels
    writeLines(readLines(f, n = n + 1, warn = FALSE, encoding = "bytes"), out, useBytes = TRUE)
  } else if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(f)
    l <- lapply(sheets, function(s) readxl::read_excel(f, sheet = s, n_max = n))
    names(l) <- sheets
    writexl::write_xlsx(l, sub("\\.xls$", ".xlsx", out))
  } else {
    message("Format non géré, copié tel quel : ", f)
    file.copy(f, out)
  }
}

files <- list.files(src, recursive = FALSE, full.names = TRUE)
for (f in files) {
  out <- file.path(dst, basename(f))
  if (dir.exists(f)) {
    dir.create(out, recursive = TRUE, showWarnings = FALSE)   # dossier vide, sans contenu, necessite de relancer workflow depuis rawdata
    file.create(file.path(out, ".gitkeep"))
  } else {
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    head_file(f, out)
  }
}

message("Taille totale : ",
        round(sum(file.size(list.files(dst, recursive = TRUE, full.names = TRUE))) / 1e6, 2), " Mo")