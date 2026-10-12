# Temporary runtime patch: create missing ggsave directories automatically.

library(ggplot2)

trace(
  "ggsave",
  where = asNamespace("ggplot2"),
  tracer = quote({
    output_file <- if (is.null(path) || !nzchar(path)) {
      filename
    } else {
      file.path(path, filename)
    }
    
    output_dir <- dirname(output_file)
    
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }
  }),
  print = FALSE
)

source("R/launching_workflows/run_gta_2026_workflow_cli.R")