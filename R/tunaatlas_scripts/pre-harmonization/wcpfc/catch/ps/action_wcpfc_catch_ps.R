#' Harmonize WCPFC Purse Seine Catch Datasets (geoflow action)
#'
#' geoflow action of the entity: finds the files of the entity, runs the core
#' function gta_rawdata_wcpfc_catch_ps() (R/rawdata_wcpfc_catch_ps.R),
#' writes the harmonized dataset and declares the resources of the entity.
#' The processing itself is in the core function, usable without geoflow.
#'
#' @param action The action context from geoflow.
#' @param entity The entity context from geoflow.
#' @param config The configuration context from geoflow.
#' @return None; writes `<source>_harmonized.csv` and `<source>_codelists.csv`
#'   next to the source file and adds them to the entity resources.
function(action, entity, config){

# Packages attached as by the former version of this script (later scripts of
# the same run may rely on them)
if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
}
if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}


# Files of the entity, in the geoflow job directory
filename1 <- entity$data$source[[1]] # data (at source: WCPFC_S_PUBLIC_BY_YR_MON.csv)
filename2 <- entity$data$source[[2]] # structure (wcpfc_catch_code_lists.csv)
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)

opts <- options()
options(encoding = "UTF-8")

catches <- gta_rawdata_wcpfc_catch_ps(path_to_raw_dataset)

# Temporal coverage of the entity
dataset_temporal_extent <- paste(
  paste0(format(min(catches$time_start), "%Y"), "-01-01"),
  paste0(format(max(catches$time_end), "%Y"), "-12-31"),
  sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

# Outputs next to the source file
base1 <- tools::file_path_sans_ext(basename(filename1))
output_name_dataset   <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_harmonized.csv"))
output_name_codelists <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_codelists.csv"))
write.csv(catches, output_name_dataset, row.names = FALSE)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)

entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
