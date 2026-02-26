replace_preharmo_files_from_gta_2026_folder <- function(
    gta_dir,
    preharmo_dir = here::here("R/tunaatlas_scripts/pre-harmonization"),
    dry_run = FALSE
) {
  stopifnot(dir.exists(gta_dir))
  stopifnot(dir.exists(preharmo_dir))
  
  message("=== replace_preharmo_files_from_gta() ===")
  message("GTA_2026 dir: ", gta_dir)
  message("Pre-harmo dir: ", preharmo_dir)
  message("dry_run = ", dry_run)
  
  gta_files <- fs::dir_ls(gta_dir, recurse = TRUE, type = "file")
  pre_files <- fs::dir_ls(preharmo_dir, recurse = TRUE, type = "file")
  
  gta_names <- fs::path_file(gta_files)
  pre_names <- fs::path_file(pre_files)
  
  common_names <- intersect(gta_names, pre_names)
  message("GTA files: ", length(gta_files))
  message("Pre-harmo files: ", length(pre_files))
  message("Common filenames to replace: ", length(common_names))
  
  if (length(common_names) == 0) {
    message("Nothing to replace.")
    return(invisible(list(
      replaced = character(),
      skipped = character(),
      dry_run = dry_run
    )))
  }
  
  # Map: for each common filename, pick the first match (and warn if duplicates)
  replaced <- character()
  skipped <- character()
  
  dup_gta <- common_names[duplicated(common_names)]
  if (length(dup_gta) > 0) {
    message("⚠️ Warning: duplicate filenames in GTA_2026 (same basename in multiple subfolders).")
    message("   First occurrence will be used for those duplicates.")
  }
  
  dup_pre <- common_names[duplicated(common_names)]
  if (length(dup_pre) > 0) {
    message("⚠️ Warning: duplicate filenames in pre-harmo (same basename in multiple subfolders).")
    message("   First occurrence will be overwritten for those duplicates.")
  }
  
  start_time <- Sys.time()
  
  for (i in seq_along(common_names)) {
    nm <- common_names[[i]]
    
    src <- gta_files[match(nm, gta_names)]
    dst <- pre_files[match(nm, pre_names)]
    
    if (is.na(src) || is.na(dst)) next
    
    message(sprintf("[%d/%d] Replacing %s", i, length(common_names), nm))
    message("  src: ", src)
    message("  dst: ", dst)
    message("  size src: ", round(fs::file_info(src)$size / 1024^2, 2), " MB")
    message("  size dst: ", round(fs::file_info(dst)$size / 1024^2, 2), " MB (after copy)")
    if (!dry_run) {
      fs::file_copy(src, dst, overwrite = TRUE)
    }
    
    replaced <- c(replaced, nm)
  }
  
  total_s <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
  message("=== Done. Replaced ", length(replaced), " files in ", total_s, "s ===")

  invisible(list(
    replaced = replaced,
    dry_run = dry_run
  ))
}