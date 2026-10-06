# tests/make_sample_data.R
# Usage : Rscript tests/make_sample_data.R [n_lignes]
args <- commandArgs(trailingOnly = TRUE)
n    <- if (length(args)) as.integer(args[1]) else 20 

src <-  here::here("runtime/extracted/all_raw_data_GTA")
dst <- here::here("tests/sample_data")

unlink(dst, recursive = TRUE)

set.seed(42)  #: même échantillon à chaque exécution

pick <- function(x_len, n) sort(sample(seq_len(x_len), min(n, x_len)))

head_file <- function(f, out) {
  ext <- tolower(tools::file_ext(f))
  
  if (ext %in% c("csv", "txt", "tsv")) {
    lines  <- readLines(f, warn = FALSE, encoding = "bytes")
    header <- lines[1]
    body   <- lines[-1]
    body   <- body[trimws(body) != ""]
    writeLines(c(header, body[pick(length(body), n)]), out, useBytes = TRUE)
    
  } else if (ext == "xlsx") {
    sheets <- readxl::excel_sheets(f)
    l <- lapply(sheets, function(s) {
      d <- readxl::read_excel(f, sheet = s)
      d <- d[rowSums(!is.na(d)) > 0, ]
      d[pick(nrow(d), n), ]
    })
    names(l) <- sheets
    writexl::write_xlsx(l, out)
    
  } else {
    message("Copié tel quel : ", f)
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
