get_cached_file <- function(url, local_path, allow_download = TRUE) {
  if (file.exists(local_path)) {
    return(local_path)
  }
  
  if (!allow_download) {
    stop(
      "Missing local file and download is disabled:\n",
      local_path,
      "\nOriginal URL was:\n",
      url,
      call. = FALSE
    )
  }
  
  local_dir <- dirname(local_path)
  
  if (!dir.exists(local_dir)) {
    ok <- dir.create(local_dir, recursive = TRUE, showWarnings = TRUE)
    
    if (!ok && !dir.exists(local_dir)) {
      stop(
        "Could not create local directory:\n",
        local_dir,
        "\nFor target file:\n",
        local_path,
        call. = FALSE
      )
    }
  }
  
  message("Local file not found, trying to download: ", url)
  message("Local path: ", local_path)
  message("Local dir exists: ", dir.exists(local_dir))
  
  status <- utils::download.file(
    url,
    local_path,
    mode = "wb",
    quiet = FALSE
  )
  
  if (!identical(status, 0L) || !file.exists(local_path)) {
    stop(
      "Download failed or did not create the expected local file.\n",
      "Local path: ", local_path, "\n",
      "URL: ", url, "\n",
      "Download status: ", status, "\n",
      "Local dir exists: ", dir.exists(local_dir),
      call. = FALSE
    )
  }
  
  return(local_path)
}