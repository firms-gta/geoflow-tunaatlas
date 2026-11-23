#' gta: Global Tuna Atlas datasets
#'
#' Business functions of the Global Tuna Atlas: harmonisation of the raw
#' datasets of the tuna Regional Fisheries Management Organisations
#' (gta_rawdata_*(), gta_tidy_raw_dataset()), nominal catch
#' (gta_nominal_catch()), georeferenced catch of levels 0, 1 and 2 and effort
#' (gta_georef_dataset(), gta_level1()), and the whole chain with
#' create_global_tuna_atlas().
#'
#' @keywords internal
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom data.table := data.table setDT
#' @importFrom DBI dbGetQuery
#' @importFrom futile.logger flog.info
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate %m+% days year
#' @importFrom purrr reduce
#' @importFrom readr col_character cols read_csv
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom sf st_geometry<- st_is_empty st_make_valid st_read st_within
#' @importFrom stringr str_c
#' @importFrom stats aggregate
#' @importFrom utils combn download.file head read.csv read.table unzip
"_PACKAGE"
