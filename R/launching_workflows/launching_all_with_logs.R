dir.create("logs", showWarnings = FALSE)

log_file <- file.path(
  "logs",
  paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

script <- here::here("R/launching_workflows/GTA_2026_creation.R")

cmd <- sprintf(
  "Rscript %s --force 2>&1 | tee %s",
  shQuote(script),
  shQuote(log_file)
)

cat("Command:\n", cmd, "\n\n")
system(cmd)

cat("\nLog saved to:", log_file, "\n")