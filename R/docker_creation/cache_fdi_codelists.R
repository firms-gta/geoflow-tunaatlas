# =============================================================================
# Cache FDI codelists
# =============================================================================

cache_fdi_codelists <- function(
  codelists_ref = Sys.getenv(
    "FDI_CODELISTS_REF",
    "b02ecc63edcf820f1719c0fc3200b5b177518bc4"
  ),
  config_file = "config/geoflow_entities_tuna_codelists.csv",
  data_dir = "data"
) {

  if (!file.exists(config_file)) {
    stop(
      "Configuration file not found: ",
      config_file
    )
  }

  message("Using FDI codelists ref: ", codelists_ref)

  config <- read.csv(
    config_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # ---------------------------------------------------------------------------
  # Extract FDI codelist sources from the source column
  # ---------------------------------------------------------------------------

  if (!"source" %in% names(config)) {
    stop(
      "Column 'source' not found in ",
      config_file
    )
  }

  source_values <- config$source

  source_values <- source_values[
    !is.na(source_values) &
      grepl(
        "^https://(raw\\.githubusercontent\\.com|github\\.com)/fdiwg/fdi-codelists/",
        source_values
      )
  ]

  if (length(source_values) == 0) {
    message("No FDI codelist URLs found.")
    return(invisible(NULL))
  }

  # ---------------------------------------------------------------------------
  # Extract filename and URL
  #
  # Expected format:
  #
  #   filename.csv@https://...
  #
  # ---------------------------------------------------------------------------

  sources <- lapply(source_values, function(source) {

    parts <- strsplit(source, "@", fixed = TRUE)[[1]]

    if (length(parts) < 2) {
      return(NULL)
    }

    data.frame(
      filename = parts[1],
      url = paste(parts[-1], collapse = "@"),
      stringsAsFactors = FALSE
    )
  })

  sources <- sources[!vapply(sources, is.null, logical(1))]

  if (length(sources) == 0) {
    message("No valid FDI codelist sources found.")
    return(invisible(NULL))
  }

  sources <- unique(do.call(rbind, sources))

  # ---------------------------------------------------------------------------
  # Cache directory
  # ---------------------------------------------------------------------------

  dir.create(
    data_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # ---------------------------------------------------------------------------
  # Download each codelist
  # ---------------------------------------------------------------------------

  for (i in seq_len(nrow(sources))) {

    filename <- basename(sources$filename[i])

    local_path <- file.path(
      data_dir,
      filename
    )

    # Convert the original URL to the exact requested FDI codelists ref.
    #
    # Examples:
    #
    # https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/...
    #
    # https://github.com/fdiwg/fdi-codelists/raw/main/global/...
    #
    # become:
    #
    # https://raw.githubusercontent.com/fdiwg/fdi-codelists/<ref>/global/...
    # -------------------------------------------------------------------------

    url <- sources$url[i]

    path <- sub(
      "^https://raw\\.githubusercontent\\.com/fdiwg/fdi-codelists/",
      "",
      url
    )

    if (identical(path, url)) {
      path <- sub(
        "^https://github\\.com/fdiwg/fdi-codelists/raw/[^/]+/",
        "",
        url
      )
    }

    if (identical(path, url)) {
      stop(
        "Unsupported FDI codelist URL:\n",
        url
      )
    }

    url <- paste0(
      "https://raw.githubusercontent.com/fdiwg/fdi-codelists/",
      codelists_ref,
      "/",
      path
    )

    message("")
    message("Codelist: ", filename)
    message("URL:      ", url)
    message("Local:    ", local_path)

    # Do not download again if the file already exists.
    if (file.exists(local_path)) {
      message("Already cached.")
      next
    }

    utils::download.file(
      url = url,
      destfile = local_path,
      mode = "wb"
    )

    if (!file.exists(local_path)) {
      stop(
        "Download failed or did not create expected file:\n",
        local_path
      )
    }

    message("Downloaded.")
  }

  message("")
  message(
    "FDI codelists successfully cached using ref ",
    codelists_ref
  )

  invisible(sources)
}
