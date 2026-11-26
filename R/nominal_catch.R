#' Select the nominal catch files of the included tRFMOs
#'
#' Keeps, among the files of the nominal catch datasets, those whose name
#' contains "nominal", leaves out the code list files and the files of the
#' tRFMOs that are not included (the tRFMO is found in the file name).
#'
#' @param files Paths of the files.
#' @param include Named logical vector or list: tRFMOs to include, e.g.
#'   `c(IOTC = TRUE, ICCAT = TRUE, WCPFC = TRUE, CCSBT = TRUE, IATTC = TRUE)`.
#'   A tRFMO not named is included.
#' @return The paths kept, in their order.
#' @export
gta_nominal_catch_files <- function(files, include = list()) {
  files <- files[regexpr("nominal", basename(files)) > 0]
  files <- files[!grepl("codelist", basename(files), ignore.case = TRUE)]
  for (rfmo in c("IOTC", "ICCAT", "WCPFC", "CCSBT", "IATTC")) {
    if (isFALSE(as.logical(include[[rfmo]]))) {
      files <- files[!grepl(tolower(rfmo), files, ignore.case = TRUE)]
    }
  }
  files
}

#' Read and bind the nominal catch datasets of the tRFMOs
#'
#' Reads each dataset (output of the raw dataset workflow, in CWP format),
#' keeps the columns of the nominal catch and binds them.
#'
#' @param datasets List (or vector) of datasets: data frames, or paths to the
#'   CSV files.
#' @return The nominal catch of all the datasets (data frame), with
#'   `measurement_value` numeric and the dates as "YYYY-MM-DD" strings.
#' @export
gta_read_nominal_catch <- function(datasets) {
  columns_to_keep <- c("source_authority", "species", "gear_type", "fishing_mode", "fishing_fleet",
                       "time_start", "time_end", "geographic_identifier", "measurement",
                       "measurement_type", "measurement_value", "measurement_unit",
                       "measurement_processing_level")
  reader <- function(path) readr::read_csv(path, guess_max = 0)
  nominal_catch <- as.data.frame(do.call("rbind", lapply(datasets, function(x) {
    dplyr::select(gta_data(x, reader = reader), dplyr::all_of(columns_to_keep))
  })))
  nominal_catch$measurement_value <- methods::as(nominal_catch$measurement_value, "numeric")
  nominal_catch$time_start <- substr(as.character(nominal_catch$time_start), 1, 10)
  nominal_catch$time_end <- substr(as.character(nominal_catch$time_end), 1, 10)
  nominal_catch
}

#' Global nominal catch (FIRMS level 0)
#'
#' Builds the global nominal catch dataset from the nominal catch datasets of
#' the tRFMOs: binds them, then keeps, for the Southern Bluefin Tuna (SBF),
#' the data of a single source (SBF is reported both by CCSBT and by the
#' other tRFMOs).
#'
#' @param datasets List (or vector) of the nominal catch datasets of the
#'   tRFMOs: data frames, or paths to the CSV files (see
#'   gta_nominal_catch_files() to select them).
#' @param SBF_data_rfmo_to_keep For SBF, keep the data of `"CCSBT"`, of the
#'   other tRFMOs (any other value), or of all (`NULL`).
#' @param recap_each_step Write the summary of each step
#'   (CWP.dataset::function_recap_each_step(), in the working directory).
#' @return The global nominal catch (data frame).
#' @export
gta_nominal_catch <- function(datasets, SBF_data_rfmo_to_keep = "CCSBT", recap_each_step = FALSE) {
  message("Retrieving RFMOs nominal catch...")
  nominal_catch <- gta_read_nominal_catch(datasets)
  message("Retrieving RFMOs nominal catch OK")

  if (recap_each_step) {
    CWP.dataset::function_recap_each_step(
      "rawdata",
      nominal_catch, "rawdata", "retrieve_nominal_catch", NULL)
  }

  # Southern Bluefin Tuna (SBF): keep data from CCSBT or data from the other tuna RFMOs?
  if (!is.null(SBF_data_rfmo_to_keep)) {
    message(paste0("Keeping only data from ", SBF_data_rfmo_to_keep, " for the Southern Bluefin Tuna..."))
    if (SBF_data_rfmo_to_keep == "CCSBT") {
      nominal_catch <- nominal_catch[which(!(nominal_catch$species %in% "SBF" & nominal_catch$source_authority %in% c("ICCAT", "IOTC", "IATTC", "WCPFC"))), ]
    } else {
      nominal_catch <- nominal_catch[which(!(nominal_catch$species %in% "SBF" & nominal_catch$source_authority == "CCSBT")), ]
    }
    message(paste0("Keeping only data from ", SBF_data_rfmo_to_keep, " for the Southern Bluefin Tuna OK"))

    if (recap_each_step) {
      CWP.dataset::function_recap_each_step(
        "Filtering_SBF_data",
        nominal_catch,
        paste0("Filtering SBF data to keep only data from", SBF_data_rfmo_to_keep),
        "inner_join",
        NULL
      )
    }
  }
  nominal_catch
}
