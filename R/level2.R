#' Georeferenced catch, level 2 (raised to the nominal catch)
#'
#' Level 2 part of create_georef_dataset(), moved as is: from
#' the level 1 georeferenced catch, loads the nominal catch (file in data/, or
#' Zenodo), converts and maps it, raises the georeferenced catch to the
#' nominal catch (successive passes on fewer and fewer dimensions), then
#' disaggregates or removes the data at a resolution coarser than 5 or 1
#' degree, according to the options.
#'
#' @param georef_dataset The georeferenced catch (data frame).
#' @param opts List of the options, with their defaults already set (see the
#'   geoflow action workflow/actions/create_georef_dataset.R).
#' @param con Connection to the Tuna Atlas database, or NULL (default): not needed
#'   with the default options (see gta_require_con()).
#' @param files Names of the tRFMO dataset files (to read again some tRFMO
#'   datasets, see gta_rfmo_dataset_level0()).
#' @param rawdata The options used to read the tRFMO datasets (returned by
#'   gta_georef_level0()).
#' @param geographic_identifier_to_nominal Correspondence between the grid
#'   cells and the areas of the nominal catch, or `NULL`.
#' @param DATASET_LEVEL Level of the dataset (2).
#' @param entity A geoflow entity whose metadata the steps complete, or `NULL`.
#' @param recap_each_step Write the summary of each step.
#' @param stepnumber Number of the first step, for the log.
#' @param data_dir Folder of the files.
#' @return A list: `georef_dataset` (data frame) and `stepnumber`.
gta_georef_level2 <- function(georef_dataset, opts, con = NULL, files = character(0), rawdata = opts,
                              geographic_identifier_to_nominal = NULL, DATASET_LEVEL = 2,
                              entity = NULL, recap_each_step = TRUE, stepnumber = 1,
                              data_dir = "data") {
  url_scripts_create_own_tuna_atlas <- here::here("R/tunaatlas_scripts/generation")
  stepLogger <- function(level, step, msg) {
    message(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  # Some helpers still read the database connection from a geoflow
  # configuration: they get this one, which only holds the connection
  config <- list(software = list(output = list(dbi = con)))

  
  message(
    "Extract and load FIRMS Level 0 nominal catch data input (required if raising process is asked) "
  )
  if(file.exists(file.path("data", opts$keynominal)) && !opts$forceuseofdoi){
    nominal_catch <-
      readr::read_csv((file.path("data", opts$keynominal)),
                      guess_max = 0)
    class(nominal_catch$measurement_value) <- "numeric"
    #@juldebar if not provided by Google drive line below should be used if nominal catch has to be extracted from the database
    # } else if(!is.null(opts$doinominal)){
    #   zen4R::download_zenodo(doi = opts$doinominal, files = opts$keynominal, path = "data")
    #   nominal_catch <-
    #     readr::read_csv(here::here(file.path("data", opts$keynominal)),
    #                     guess_max = 0)
    #   class(nominal_catch$measurement_value) <- "numeric"
    
  } else {
    stop("Please provide a nominal catch dataset")
    # nominal_catch <- retrieve_nominal_catch(entity, config, opts)
  }
  
  class(nominal_catch$measurement_value) <- "numeric"
  mapping_keep_src_code <- FALSE
  
  class(nominal_catch$measurement_unit) <- "character"
  
  if (any(nominal_catch$measurement_unit == "t"))
    nominal_catch[nominal_catch$measurement_unit == "t",]$measurement_unit <- "t"
  if (any(nominal_catch$measurement_unit == "TRUE"))
    nominal_catch[nominal_catch$measurement_unit == "TRUE",]$measurement_unit <- "t"
  if (any(nominal_catch$measurement_unit == "no"))
    nominal_catch[nominal_catch$measurement_unit == "no",]$measurement_unit <- "no"
  class(nominal_catch$measurement_value) <- "numeric"
  
  # Based on this analysis, the following cutoff years were determined for data completeness:
  #   
  # IOTC: Keep data starting from 1953.
  # ICCAT: Keep data starting from 1957.
  # WCPFC: Keep data starting from 1952.
  # IATTC: Keep data starting from 1957.
  # CCSBT: Keep all data, although years 2017 and 2020 have missing months, likely due to no fishing activity rather than missing data.
  # Étape 1 : extraire les années complètes dans chaque dataset
  get_complete_years <- function(data, date_col_start = "time_start", date_col_end = "time_end") {
    data %>%
      dplyr::select(.data[[date_col_start]],.data[[date_col_end]], source_authority) %>%
      dplyr::distinct() %>%
      dplyr::mutate(year = year(.data[[date_col_start]])) %>%
      dplyr::group_by(source_authority, year) %>%
      dplyr::summarise(
        min_date = min(as.Date(.data[[date_col_start]]), na.rm = TRUE),
        max_date = max(as.Date(.data[[date_col_end]]), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(
        format(min_date, "%m-%d") == "01-01",
        format(max_date, "%m-%d") == "12-31"
      )
  }

  message(sprintf("Begin common years nom georef %s", Sys.time()))  # Log after processing
  # qs::qsave(georef_dataset,"data/georef_dataset_test_debug.qs")
  # qs::qsave(nominal_catch,"data/nominal_catch_test_debug.qs")
  # Appliquer aux deux datasets
  complete_years_georef <- get_complete_years(georef_dataset %>% dplyr::ungroup())
  complete_years_nominal <- get_complete_years(nominal_catch%>% dplyr::ungroup())

  # Intersection des années complètes
  common_complete_years <- inner_join(
    complete_years_georef,
    complete_years_nominal,
    by = c("source_authority", "year")
  )

  # Pour chaque source_authority, garder la plus ancienne année commune complète
  threshold_years <- common_complete_years %>%
    dplyr::group_by(source_authority) %>%
    dplyr::summarise(min_complete_year = min(year), .groups = "drop")

  # Filtrer les deux jeux de données à partir de cette année
  georef_dataset <- georef_dataset %>% dplyr::ungroup() %>%
    dplyr::mutate(year = year(time_start)) %>%
    dplyr::left_join(threshold_years, by = "source_authority") %>%
    dplyr::filter(year >= min_complete_year) %>%
    dplyr::select(-year, -min_complete_year) %>% # et ajouter la colonne geographic_identifier_to_nominal
    dplyr::left_join(geographic_identifier_to_nominal , by = c("geographic_identifier" = "code", "source_authority")) %>% 
    dplyr::mutate(geographic_identifier_nom = source_authority) # on met un hotfix car pa le temsp de m'en occuper

  nominal_catch <- nominal_catch %>% dplyr::ungroup() %>%
    dplyr::mutate(year = year(time_start)) %>%
    dplyr::left_join(threshold_years, by = "source_authority") %>%
    dplyr::filter(year >= min_complete_year) %>%
    dplyr::select(-min_complete_year)

  message(sprintf("End common years nom georef %s", Sys.time()))  # Log after processing

  CWP.dataset::function_recap_each_step(
    paste0("Removing_data_with_no_corresponding_years_in_nominal"),
    georef_dataset,
    "Since the nominal catch dataset does not cover every year, and the georeferenced data for the first years are not complete, raising would only apply to certain years and/or raising would be not accurate for first years. To avoid mixing raised and unraised data, we prefer to remove records for years that have no equivalent in the nominal dataset.",
    ""
  )
  message(sprintf("End common years recap step %s", Sys.time()))  # Log after processing
  message("Level 1 start")
  # DATASET LEVEL 2 ---------------------------------------------------
  if(DATASET_LEVEL >= 2){ #with this condition code will be run to deal with dataset level 2
    
    nominal_catch <- nominal_catch %>% dplyr::rename(geographic_identifier_nom = geographic_identifier) %>% 
      dplyr::mutate(geographic_identifier_nom = source_authority) #seee ligne 630, on met tout en WCPFC
    
    stepLogger(level = 2, step = stepnumber, "Extract and load IRD Level 1 gridded catch data input")
    stepnumber <<- stepnumber + 1
    
    
    
    if(is.null(opts$upgrading_nominal_on_georef)){
      opts$upgrading_nominal_on_georef <- FALSE
    }
    
    if (!is.null(opts$raising_georef_to_nominal))
      if (opts$raising_georef_to_nominal) {
        
        stepLogger(level = 2, step = stepnumber, "Raise IRD gridded Level 1 (1 or 5 deg) input with FIRMS Level O total (nominal) catch dataset")
        stepnumber <<- stepnumber + 1
        
        message("Start raising process")
        
        if (opts$fact == "catch") {
          message("Fact=catch !")
          dataset_to_compute_rf = georef_dataset
          # year is used as a dimension to match the conversion factors dimension 
          if (is.null(opts$x_raising_dimensions)) {
            x_raising_dimensions = c("gear_type", "species", "year", "source_authority", "fishing_fleet", "geographic_identifier_nom", "fishing_mode")
          }
          
          
        } else if (opts$fact == "effort") {
          ## If we raise the efforts, the RF is calculated using the georeferenced catch data. Hence, we need to retrieve the georeferenced catch data.
          cat(
            "Catch datasets must be retrieved and processed in order to raise efforts. \nRetrieving georeferenced catch datasets from the Tuna atlas database...\n"
          )
          dataset_catch <- NULL
          rfmo_dataset <-
            gta_rfmo_dataset_level0("IOTC", files, rawdata, data_dir)
          dataset_catch <- rbind(dataset_catch, rfmo_dataset)
          rm(rfmo_dataset)
          
          rfmo_dataset <-
            gta_rfmo_dataset_level0("WCPFC", files, rawdata, data_dir)
          dataset_catch <- rbind(dataset_catch, rfmo_dataset)
          rm(rfmo_dataset)
          
          rfmo_dataset <-
            gta_rfmo_dataset_level0("CCSBT", files, rawdata, data_dir)
          dataset_catch <- rbind(dataset_catch, rfmo_dataset)
          rm(rfmo_dataset)
          
          rfmo_dataset <- gta_rfmo_dataset_level0("IATTC", files, rawdata, data_dir)
          dataset_catch <- rbind(dataset_catch, rfmo_dataset)
          rm(rfmo_dataset)
          
          rfmo_dataset <- gta_rfmo_dataset_level0("ICCAT", files, rawdata, data_dir)
          dataset_catch <- rbind(dataset_catch, rfmo_dataset)
          rm(rfmo_dataset)
          
          
          if (mapping_map_code_lists == "TRUE") {
            dataset_catch <-
              map_codelists(
                "catch",
                "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv",
                dataset_catch,
                mapping_keep_src_code
              )$dataset_mapped
          }
          
          dataset_catch$time_start <-
            substr(as.character(dataset_catch$time_start), 1, 10)
          dataset_catch$time_end <-
            substr(as.character(dataset_catch$time_end), 1, 10)
          if (opts$unit_conversion_convert == "TRUE") {
            # We use our conversion factors (IRD). This is now an input parameter of the script
            #@juldebar URL for unit_conversion_csv_conversion_factor_url of should not be hard coded, temporary patch
            #
            dataset_catch <- do_unit_conversion(
              entity = entity,
              config = config,
              fact = "catch",
              unit_conversion_csv_conversion_factor_url =
                opts$unit_conversion_csv_conversion_factor_url,
              unit_conversion_codelist_geoidentifiers_conversion_factors =
                opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
              mapping_map_code_lists = opts$mapping_map_code_lists,
              dataset_catch, removing_numberfish_final= FALSE
            )
          }
          
          dataset_to_compute_rf = dataset_catch
          #@juldebar insert patch below to fix error in raise_get_rf function
          
          rm(dataset_catch)
        }
        
        class(dataset_to_compute_rf$measurement_value) <- "numeric"
        
        
        message("Executing function function_raising_georef_to_nominal")
        
        if (opts$fact=="catch"){
          raising_dimensions=c(x_raising_dimensions,"measurement_unit")
        } else if (opts$fact=="effort"){
          raising_dimensions=x_raising_dimensions
          df_rf$measurement_unit=NULL
        }
        # idee raise only les donnees qui sont que en tonnes
        
        if(opts$upgrading_nominal_on_georef){
          
          if(is.null(opts$strong_weak_upgrade)){
            opts$strong_weak_upgrade <- "weak"
          } 
          
          
          
          strata_cols_updated <- c("gear_type", "species", "year", "source_authority",
                                   "fishing_fleet", "geographic_identifier_nom", "fishing_mode")
          message(paste("Time before specifying nom:", Sys.time()))
          # on spécifie deux fois comme ça ça converge direct
          step_order_updated <-  c("fishing_mode", "gear_type", "fishing_fleet")
          
          strata_cols_updated <- setdiff(strata_cols_updated, step_order_updated)
            # nominal_catch <- specify_nominal_with_georef(nominal_catch,
            #                                              georef_dataset,
            #                                              strata_cols   = strata_cols_updated ,
            #                                              step_order    =  step_order_updated,
            #                                              mode = "weak", # could be strong
            #                                              logger = function(msg) {
            #                                                cat("[Specify] ", msg, "\n")
            #                                              })
            # # on spécifie deux fois comme ça ça converge direct
            # nominal_catch <- specify_nominal_with_georef(nominal_catch,
            #                                              georef_dataset,
            #                                              strata_cols   = strata_cols_updated ,
            #                                              step_order    =  step_order_updated,
            #                                              mode = "weak", # could be strong
            #                                              logger = function(msg) {
            #                                                cat("[Specify] ", msg, "\n")
            #                                              })
            
            message(paste("Time after specifiying nom:", Sys.time()))
# Decrease precision of nominal as sometimes (mainly for WCPFC) th --------

            # for each year, checking all the values of all dimensions of georef and nominal, if some appears only in nominal, we convert it to NEI, UNK or 99.9 
            # le probleme c'est par exemple si du georef a été mis en UNK mais correspond à un FF qui est dans nominal (ex JPN pour WCPFC, certaines captures en UNK et d'autres en JPN, 
            # quand on fera le raising, ces UNK seront augmentées sur le UNK nominal et pas sur le JPN nominal mais je suis pas sûr qu'on puisse y faire grand chose'
            # decrease_precision_of_nominal_with_georef <- nominal_catch
            
            
            decrease_precision_of_nominal_with_georef_tables <- decrease_precision_of_nominal_with_georef(nominal = nominal_catch, georef = georef_dataset)
            
            nominal_catch <- decrease_precision_of_nominal_with_georef_tables$data
            summary_decrease_precision_and_not_allowed_nominal_or_georef_values <- decrease_precision_of_nominal_with_georef_tables$summary
            saveRDS(summary_decrease_precision_and_not_allowed_nominal_or_georef_values, "data/recap_deteriorate_nominal.rds")
            saveRDS(nominal_catch, "data/nominal_catch_deteriorated.rds")
            
        }
        
# We create the group of sharks for IATTC has there is no detail o -------- see analyses/analyse_BIL_Sharks.R for more details
        # Hardcoded ffrom data review detected by humans
        # En cas de non mapping, on regarde les strates ou il n'y pas dambiguite car par exemple un gear existe uniquement en georef et on fait un choix facile pour cette strate
        # (exemple mettre en 99.9), et on le code a la main. Pour le reste, on a le score qui calcule une probabilite de mapping avec une autre strate
        
        recode_gears <- function(gear_type, source_authority) {
          out <- gear_type
          
          idx <- source_authority == "IOTC" & out %in% c("10.9", "99.9", "99", "08.9", "08.4", "06.9")
          out[idx] <- "99.9"
          
          idx <- out %in% c("10.9") 
          out[idx] <- "99.9"
          
          out
        } # a voir si on garde ca ou plutot le mapping
        
        recode_group_species <- function(species, source_authority) {
          out <- species
          
          idx <- source_authority == "IATTC" & out %in% c("BLR","BSH","CCL","FAL","MAK","OCS","SMA","SPL","SPN","SPZ","THR")
          out[idx] <- "SKH"
          
          idx <- source_authority %in% c("IOTC", "ICCAT") & out %in% c("ALV","PTH","BTH","THR","SMA","LMA","MAK","POR","FAL","OCS","BSH","RSK","SKH")
          out[idx] <- "SKH"
          
          idx <- source_authority %in% c("IOTC", "ICCAT") & out %in% c("SPL","SPK","SPZ","SPN","SPY")
          out[idx] <- "SPY"
          
          idx <- source_authority %in% c("IOTC", "ICCAT") & out %in% c("BLM","BUM","BXQ","MLS","SFA","SAI","SSP","SPF","MSP","WHM","BIL")
          out[idx] <- "BIL"
          
          idx <- source_authority %in% c("IOTC", "ICCAT") & out %in% c("FRI","BLT","FRZ")
          out[idx] <- "FRZ"
          
          out
        }
        
        recode_fishing_mode <- function(fishing_mode, source_authority) {
          out <- fishing_mode
          
          idx <- source_authority == "WCPFC" 
          out[idx] <- "UNK"
          
          out
        }
        
        georef_dataset$group_species_iattc_sharks <- recode_group_species(
          georef_dataset$species,
          georef_dataset$source_authority
        )
        nominal_catch$group_species_iattc_sharks <- recode_group_species(
          nominal_catch$species,
          nominal_catch$source_authority
        )
        
        georef_dataset$gear_type <- recode_gears(
          georef_dataset$gear_type,
          georef_dataset$source_authority
        )
        nominal_catch$gear_type <- recode_gears(
          nominal_catch$gear_type,
          nominal_catch$source_authority
        )
        
        georef_dataset$fishing_mode_wcpfc_issue_unk_solved <- recode_fishing_mode(
          georef_dataset$fishing_mode,
          georef_dataset$source_authority
        )
        nominal_catch$fishing_mode_wcpfc_issue_unk_solved <- recode_fishing_mode(
          nominal_catch$fishing_mode,
          nominal_catch$source_authority
        )
        
        nominal_catch <- nominal_catch %>% dplyr::mutate(gear_type_solved = gear_type, fishing_fleet_solved = fishing_fleet, geographic_identifier_nom_solved = geographic_identifier_nom)

        saveRDS(nominal_catch, "data/nominal_catch_for_raising.rds")
        
        CWP.dataset::function_recap_each_step(
          "Remapping_geoerf_on_sharks_and_minor_changes",
          georef_dataset,
          paste0("The data is remapped for sharks as there is some aggregation in the nominal but not in georef"),
          "mutate",
          list(""), entity
        )
        
        #hotfix
        if(is.null(opts$decrease_nei)){
          opts$decrease_nei <- FALSE
        }
        
        use_groups <- function(
  dims,
  shark_col = "group_species_iattc_sharks",
  species_col = "species",
  gear_solved = "gear_type_solved",
  gear_col = "gear_type",
  fishing_fleet_solved = "fishing_fleet_solved",
  fishing_fleet_col = "fishing_fleet",
  geographic_identifier_nom_solved = "geographic_identifier_nom_solved",
  geographic_identifier_nom_col = "geographic_identifier_nom",
  fishing_mode_col_solved = "fishing_mode_wcpfc_issue_unk_solved",
  fishing_mode_col = "fishing_mode"
        ) {
          
          dims2 <- unique(dims)
          
          # --- Species → shark group ---
          if (species_col %in% dims2) {
            dims2 <- c(shark_col, setdiff(dims2, species_col))
          }
          
          # Always ensure shark group is present
          dims2 <- unique(c(shark_col, dims2))
          
          
          # --- Gear → gear group ---
          if (fishing_mode_col %in% dims2) {
            dims2 <- c(fishing_mode_col_solved, setdiff(dims2, fishing_mode_col))
          }
          
          # Always ensure gear group is present
          dims2 <- unique(c(fishing_mode_col_solved, dims2))
          
          if (gear_col %in% dims2) {
            dims2 <- c(gear_solved, setdiff(dims2, gear_col))
          }
          
          # Always ensure gear group is present
          dims2 <- unique(c(gear_solved, dims2))
          
          
          if (fishing_fleet_col %in% dims2) {
            dims2 <- c(fishing_fleet_solved, setdiff(dims2, fishing_fleet_col))
          }
          
          # Always ensure gear group is present
          dims2 <- unique(c(fishing_fleet_solved, dims2))
          
          if (geographic_identifier_nom_col %in% dims2) {
            dims2 <- c(geographic_identifier_nom_solved, setdiff(dims2, geographic_identifier_nom_col))
          }
          
          # Always ensure gear group is present
          dims2 <- unique(c(geographic_identifier_nom_solved, dims2))
          
          return(dims2)
        }
        
        full_dims <- c("gear_type", "species", "year", "source_authority",
                       "fishing_fleet", "geographic_identifier_nom", "fishing_mode")
        
        # a verif si group gears est vraiment utile il a pas tant lair
        # l'iterative est plus utile parce que on remape la donnee
        dim_sets_raw <- list( # avec le cahngement majeur de gestion des inconnus georef/nom seuls les deux premières étapes sont utiles/utilisees
          full_dims,
          setdiff(full_dims, "fishing_mode"),
          # c("group_gears",setdiff(full_dims, "gear_type")), # pas util
          setdiff(full_dims, "geographic_identifier_nom"), # could be removed ? or not
          setdiff(full_dims, c("gear_type")), # usefull for gear_type on 99.9, need to be kept
          setdiff(full_dims, c("gear_type", "geographic_identifier_nom")), # usefull for sharks IATTC
          setdiff(full_dims, c("fishing_fleet")),
          # c("group_gears", setdiff(full_dims, c("fishing_mode", "gear_type", "geographic_identifier_nom"))), # on rajoute geographic_identifier_nom car prblm conversion données WCPFC marlins lately # pas utile
          c(setdiff(full_dims, c("fishing_mode", "gear_type", "geographic_identifier_nom"))), # on test
          setdiff(full_dims, c("fishing_mode", "fishing_fleet", "geographic_identifier_nom")),
          # c("group_gears",setdiff(full_dims, c("gear_type", "fishing_fleet", "geographic_identifier_nom"))), # pas utile
          setdiff(full_dims, c("gear_type", "fishing_fleet", "geographic_identifier_nom")), # fais des truc un peu de zinzin, augmente beaucoup les georef sup nom, en gros c'est vraiment on augmente sur toute l'espece sans engin pays ou quoi
          # masi ca devrait pas parce quon augmente plus les choses qui ont deja ete augmentee avant
          # c("group_gears",setdiff(full_dims, c("fishing_mode", "gear_type", "fishing_fleet", "geographic_identifier_nom"))), # mais celui là utile donc group_gears c'est intéressant au moins une fois # update apparemment ca l'est plus
          setdiff(full_dims, c("fishing_mode", "gear_type", "fishing_fleet", "geographic_identifier_nom"))
          # setdiff(full_dims, c("fishing_mode", "gear_type", "fishing_fleet", "geographic_identifier_nom")) # la dernière is the same but do_not_raise_perfectly_compatible_but_unknown_data is to true
          # on lenleve parce que ça cree vraiment beaucou de georefsup nom surtout bcp sur des especes majeures, 
          # on rajotue donc la dernière mais en faisant augmenter juste les UNK 
          # full_dims,
          # full_dims # et on rajoute encore full dims mais avec la décraoissance si sup ? sauf si déjà sup ? non on enlève ça cré trop baisse on revient aux niveaux d'avant level 1
        ) # attena
        
        dim_sets_raw <- lapply(dim_sets_raw, use_groups)
        full_dims <- use_groups(full_dims)
        
        make_raise_flag <- function(x_dims, full_dims) {
          # Build an automatic processing flag based on the
          # dimensions that were removed for the current pass.
          #
          # Example:
          # - if no dimension is removed: "raised_all"
          # - if gear_type and fishing_mode are removed:
          #   "raised_minus_gear_mode"
          
          removed_dims <- setdiff(full_dims, x_dims)
          
          if (length(removed_dims) == 0) {
            "raised_all"
          } else {
            paste0("raised_minus_", paste(short_dim_name(removed_dims), collapse = "_"))
          }
        }
        
        flag_full_dims <- make_raise_flag(full_dims, full_dims)
        # Generate the processing flag corresponding to the most detailed pass,
        # i.e. the pass where no dimension was removed.
        
        if(is.null(opts$passes)){
          opts$passes <- 4
        } else if(opts$passes == "max"){
          opts$passes <- length(dim_sets_raw)
        }
        message(
          sprintf("passes to : %s", opts$passes)
        )
        
        short_dim_name <- function(x) {
          # Convert long dimension names into short labels
          # so the generated flags stay readable and compact.
          dplyr::recode(
            x,
            gear_type = "gear",
            species = "sp",
            year = "yr",
            source_authority = "auth",
            fishing_fleet = "fleet",
            geographic_identifier_nom = "geo",
            fishing_mode = "mode",
            group_gears = "ggears"
          )
        }
        
        subtract_consumed_nominal <- function(remaining_nominal, newly_raised) {
          # Subtract from the remaining nominal dataset the amount
          # that has just been consumed by the newly raised georeferenced data.
          #
          # This prevents the next passes from reusing nominal quantities
          # that were already reached in previous passes.
          
          if (nrow(newly_raised) == 0) return(remaining_nominal)
          
          newly_raised_t <- newly_raised %>%
            dplyr::filter(measurement_unit %in% c("t")) %>%
            dplyr::mutate(year = as.character(lubridate::year(time_start)))
          
          if (nrow(newly_raised_t) == 0) return(remaining_nominal)
          
          # Define the columns used to match nominal and georeferenced data
          # while excluding value and time detail columns not needed for aggregation.
          join_cols <- intersect(
            setdiff(colnames(remaining_nominal), c("measurement_value", "measurement_unit", "time_start")),
            setdiff(colnames(newly_raised_t), c("measurement_value", "measurement_unit", "time_start", "time_end"))
          )
          
          # Compute how much nominal was consumed by the current pass
          consumed_nominal <- newly_raised_t %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
            dplyr::summarise(consumed_value = sum(measurement_value, na.rm = TRUE), .groups = "drop")
          
          # Subtract consumed values from the remaining nominal dataset
          remaining_nominal %>%
            dplyr::mutate(year = as.character(year)) %>%
            dplyr::left_join(consumed_nominal, by = join_cols) %>%
            dplyr::mutate(
              consumed_value = dplyr::coalesce(consumed_value, 0),
              measurement_value = measurement_value - consumed_value
            ) %>%
            dplyr::filter(measurement_value > 0) %>%
            dplyr::select(-consumed_value)
        }
        
        make_raise_flag <- function(x_dims, full_dims) {
          removed_dims <- setdiff(full_dims, x_dims)
          
          if (length(removed_dims) == 0) {
            "raised_all"
          } else {
            paste0("raised_minus_", paste(short_dim_name(removed_dims), collapse = "_"))
          }
        }
        
        iterative_raising <- function(fact               = "catch",
                                      georef_dataset,
                                      nominal_catch,
                                      entity,
                                      passes            = 20L,
                                      decrease_on_last  = FALSE,
                                      recap_each_step   = TRUE,
                                      stepnumber        = 1,
                                      decrease_nei      = FALSE,
                                      dim_sets = list(c("gear_type", "species", "year", "source_authority",
                                                        "fishing_fleet", "geographic_identifier_nom", "fishing_mode")),
                                      dataset_to_add_in_recap = NULL) {
          
          # Define the full set of dimensions used in the most detailed pass
          full_dims <- c("gear_type", "species", "year", "source_authority",
                         "fishing_fleet", "geographic_identifier_nom", "fishing_mode")
          full_dims <- use_groups(full_dims)
          
          # Convert a single number of passes into an explicit sequence
          # or keep the provided vector of pass indices as is
          if (length(passes) == 1) {
            passes_seq <- seq_len(max(0L, min(as.integer(passes), length(dim_sets))))
          } else {
            passes_seq <- passes
          }
          
          if (length(passes_seq) == 0) return(georef_dataset)
          passes_seq <- passes_seq[passes_seq <= length(dim_sets)]
          
          # Keep track of what remains to be raised and what remains available
          # in the nominal dataset after each pass
          remaining_georef <- georef_dataset
          remaining_nominal <- nominal_catch
          finalized_georef <- georef_dataset[0, ]
          
          build_full_state <- function() {
            parts <- list(
              dataset_to_add_in_recap,
              finalized_georef,
              remaining_georef
            )
            parts <- parts[!vapply(parts, is.null, logical(1))]
            dplyr::bind_rows(parts)
          }
          
          for (i in passes_seq) {
            
            # Dimensions used for the current pass
            x_dims <- dim_sets[[i]]
            
            # Enable decreasing only on the last pass if requested,
            # or always if decrease_nei is set to TRUE
            decrease_flag <- isTRUE(decrease_on_last) && i == max(passes_seq) | isTRUE(decrease_nei)
            
            # Build an automatic processing label for the current pass
            flag_this_pass <- make_raise_flag(x_dims, full_dims)
            strata_cols_updated <- x_dims
            
            # First, convert number-based georeferenced data to nominal-compatible
            # tonnage when possible, using only the remaining data
            convert_number_to_nominal_output <- convert_number_to_nominal(
              remaining_georef,
              remaining_nominal,
              strata = strata_cols_updated,
              raise_only_unmatched = FALSE,
              flag_for_measurement_processing_level = paste0("conversioned_and_",flag_this_pass),
              dim_perfectly_compatible_data = full_dims
            )
            
            remaining_georef <- convert_number_to_nominal_output$georef_dataset
            nouvelles_strates <- convert_number_to_nominal_output$nouvelles_strates
            
            # Build the dataset used for recap output
            rds_data <- build_full_state()
            
            CWP.dataset::function_recap_each_step(
              paste0("Conv_NO_nominal", i),
              rds_data = rds_data,
              paste0(
                "The data that remains in Number of Fish, for which the entirety of the strata with the following dimensions: ",
                toString(strata_cols_updated),
                " containing catch information in tons, is converted and raised using the nominal dataset ",
                opts$doinominal,
                ". The key identifier for this operation is: ",
                opts$keynominal,
                ". This process relies on the fact that for a strata reported in both number and tons, the spatial footprint of the data in number more often contains the spatial footprint of the data in tons. ",
                "Then, as there is a need to choose one measurement unit for the raising, and given the limited conversion factors available, ",
                "we choose to raise the data in 'Number of fish' on the basis of the equivalent nominal strata downgraded by the corresponding georeferenced catch in tons."
              ),
              ""
            )
            
            # Log the start of the pass
            stepLogger(
              level = 2,
              step  = stepnumber,
              msg   = paste0(
                "Raising pass ", i, " on dimensions: ",
                paste(x_dims, collapse = ", "),
                if (decrease_flag) " [decrease enabled]" else ""
              )
            )
            stepnumber <- stepnumber + 1
            
            tons_units   <- c("t", "MT", "MTNO")
            number_units <- c("no", "NO", "NOMT")
            
            # Compute totals before the current pass
            tot_before <- remaining_georef %>%
              dplyr::mutate(
                unit_class = dplyr::case_when(
                  measurement_unit %in% tons_units   ~ "tons",
                  measurement_unit %in% number_units ~ "numbers",
                  TRUE                               ~ "other"
                )
              ) %>%
              dplyr::group_by(unit_class) %>%
              dplyr::summarise(total = sum(measurement_value, na.rm = TRUE), .groups = "drop")
            # Perform the raising on the remaining georeferenced data
            raise_out <- function_raise_data(
              fact                              = fact,
              source_authority_filter           = c("IOTC", "ICCAT", "IATTC", "CCSBT", "WCPFC"),
              dataset_to_raise                  = remaining_georef,
              dataset_to_compute_rf             = remaining_georef,
              nominal_dataset_df                = remaining_nominal,
              x_raising_dimensions              = x_dims,
              decrease_when_rf_inferior_to_one  = decrease_flag,
              raise_only_on_unk                 = TRUE,
              do_not_raise_perfectly_compatible_but_unknown_data = FALSE,
              do_not_raise_any_unk              = FALSE,
              flag_for_measurement_processing_level = flag_this_pass,
              dim_perfectly_compatible_data     = full_dims
            )
            
            raised_pass <- raise_out$data_raised
            df_rf <- raise_out$df_rf
            saveRDS(df_rf, file.path("data", sprintf("t_%s_raising_factors.rds", i)))
            
            newly_raised <- raised_pass %>%
              dplyr::filter(measurement_processing_level == flag_this_pass)
            
            remaining_georef <- raised_pass %>%
              dplyr::filter(measurement_processing_level != flag_this_pass)
            
            saveRDS(df_rf, file.path("data", sprintf("t_%s_remaining_georef_to_be_raised.rds", i)))
            
            
            finalized_georef <- dplyr::bind_rows(finalized_georef, newly_raised)
            
            # Remove from the remaining nominal dataset what has already been consumed
            remaining_nominal <- subtract_consumed_nominal(
              remaining_nominal = remaining_nominal,
              newly_raised = newly_raised
            )
            
            saveRDS(remaining_nominal, file.path("data", sprintf("t_%s_remaining_nominal.rds", i)))
            
            remainging_nominal_reduced <- remaining_nominal %>% dplyr::group_by(source_authority) %>% dplyr::summarise(sum=sum(measurement_value))
            saveRDS(remainging_nominal_reduced, file.path("data", sprintf("t_%s_remaining_nominal_reduced.rds", i)))
            rm(remainging_nominal_reduced)
            
            # Rebuild a temporary dataset for recap and total comparisons
            current_georef_for_output <- dplyr::bind_rows(finalized_georef, remaining_georef)
            
            # Compute totals after the current pass
            tot_after <- current_georef_for_output %>%
              dplyr::mutate(
                unit_class = dplyr::case_when(
                  measurement_unit %in% tons_units   ~ "tons",
                  measurement_unit %in% number_units ~ "numbers",
                  TRUE                               ~ "other"
                )
              ) %>%
              dplyr::group_by(unit_class) %>%
              dplyr::summarise(total = sum(measurement_value, na.rm = TRUE), .groups = "drop")
            
            # Helper to compute deltas between before and after totals
            get_delta <- function(cls, type = c("raised", "decreased")) {
              before <- tot_before$total[match(cls, tot_before$unit_class)]
              after  <- tot_after$total[match(cls,  tot_after$unit_class)]
              before[is.na(before)] <- 0
              after[is.na(after)] <- 0
              
              if (type[1] == "raised") max(after - before, 0)
              else max(before - after, 0)
            }
            
            raised_tons <- get_delta("tons", "raised")
            decr_tons   <- get_delta("tons", "decreased")
            raised_num  <- get_delta("numbers", "raised")
            decr_num    <- get_delta("numbers", "decreased")
            
            # Log summary statistics for the pass
            stepLogger(
              level = 2,
              step  = stepnumber - 1,
              msg   = sprintf(
                paste0("Pass %d summary – tons: +%.3f / -%.3f, ",
                       "numbers: +%.3f / -%.3f"),
                i, raised_tons, decr_tons, raised_num, decr_num
              )
            )
            
            # Save a recap of the current pass if requested
            if (isTRUE(recap_each_step)) {
              recap_msg <- paste0(
                "Pass ", i, "/", max(passes_seq), ". ",
                "Flag: ", flag_this_pass, ". ",
                "Dimensions used: ", paste(x_dims, collapse = ", "), ". ",
                "Dimensions removed: ",
                if (length(setdiff(full_dims, x_dims)) == 0) {
                  "none"
                } else {
                  paste(setdiff(full_dims, x_dims), collapse = ", ")
                },
                ". ",
                sprintf("Tons raised %.3f, decreased %.3f. ", raised_tons, decr_tons),
                sprintf("Numbers raised %.3f, decreased %.3f. ", raised_num, decr_num),
                if (decrease_flag) {
                  "Values above the nominal total were scaled down on this pass."
                } else {
                  "No down-scaling was applied on this pass."
                }
              )
              
              rds_data <- build_full_state()
              
              CWP.dataset::function_recap_each_step(
                step_name   = paste0("RF_pass_", i),
                rds_data    = rds_data,
                explanation = recap_msg,
                functions   = "iterative_raising",
                option_list = list(
                  dimensions        = x_dims,
                  decreased_on_pass = decrease_flag,
                  raised_tons       = raised_tons,
                  decreased_tons    = decr_tons,
                  raised_numbers    = raised_num,
                  decreased_numbers = decr_num
                ),
                entity      = entity
              )
            }
          }
          
          # Return only the data that still belongs to the current raising workflow
          dplyr::bind_rows(finalized_georef, remaining_georef)
        }
        
        
        res_map <- propose_georef_to_nominal_mappings_clean(
          georef = georef_dataset,
          nominal = nominal_catch,
          id_cols = c("source_authority", "group_species_iattc_sharks", "year"),
          candidate_cols_order = c("fishing_mode_wcpfc_issue_unk_solved", "gear_type", "geographic_identifier_nom", "fishing_fleet")
        )
        
        saveRDS(res_map, file.path("data", "mapping_georef_to_nominal.rds"))
        
        
        georef_dataset <- georef_dataset %>% dplyr::mutate(year = as.character(lubridate::year(time_start))) %>% dplyr::left_join(res_map$candidate_mappings, 
        by = c("source_authority", "group_species_iattc_sharks" = "species", "year","fishing_mode_wcpfc_issue_unk_solved" = "fishing_mode_georef",
               "gear_type" = "gear_type_georef", "geographic_identifier_nom" = "geographic_identifier_nom_georef", "fishing_fleet" = "fishing_fleet_georef")) %>% 
          dplyr::mutate(fishing_mode_wcpfc_issue_unk_solved = ifelse(is.na(fishing_mode_nominal), fishing_mode_wcpfc_issue_unk_solved, fishing_mode_nominal)) %>% 
          dplyr::mutate(geographic_identifier_nom_solved = ifelse(is.na(geographic_identifier_nom_nominal), geographic_identifier_nom, geographic_identifier_nom_nominal)) %>% 
          dplyr::mutate(fishing_fleet_solved = ifelse(is.na(fishing_fleet_nominal), fishing_fleet, fishing_fleet_nominal)) %>% 
          dplyr::mutate(gear_type_solved = ifelse(is.na(gear_type_nominal), gear_type, gear_type_nominal))
        
        data.table::fwrite(georef_dataset, file.path("data", "georef_mapped_with_info_on_recoding.csv"))
        
        
        cols_after_georef_no_solved <- c(
          ".georef_row_id",
          ".nominal_row_id",
          "georef_value",
          "nominal_value",
          "ratio_georef_nominal",
          "gear_type_nominal",
          "fishing_mode_nominal",
          "geographic_identifier_nom_nominal",
          "fishing_fleet_nominal",
          "diff_gear_type",
          "diff_fishing_mode_wcpfc_issue_unk_solved",
          "diff_geographic_identifier_nom",
          "diff_fishing_fleet",
          "different_dims",
          "n_different_dims",
          "relaxed_cols",
          "proposal_rank",
          "preference_score",
          "n_candidates_same_rank",
          "has_multiple_candidates", 
          "year"
        )
        
        georef_dataset <- georef_dataset %>% dplyr::select(-all_of(cols_after_georef_no_solved))
        
        CWP.dataset::function_recap_each_step(
          "Remapping_georef_dataset_for_later_raising",
          georef_dataset,
          paste0("The data is remapped, on other column so it shouldn't change the values of the current"),
          "propose_georef_to_nominal_mappings_clean",
          list(""), entity
        )
        
        georef_dataset <- iterative_raising(
          fact              = "catch",
          georef_dataset    = georef_dataset,
          nominal_catch     = nominal_catch,
          entity            = entity,
          passes            = 1,
          dim_sets          = dim_sets_raw,
          decrease_on_last  = FALSE,
          decrease_nei      = opts$decrease_nei
        )
        # First raising pass: use the most detailed dimension set only.
        

# Deprecated noly one raising now ---------------------------------------------

        
        # georef_dataset_to_raise <- georef_dataset %>% 
        #   dplyr::filter(measurement_processing_level != flag_full_dims)
        # 
        # georef_dataset_already_perfectly_raised <- georef_dataset %>% 
        #   dplyr::filter(measurement_processing_level == flag_full_dims)
        # 
        # georef_dataset_not_raised_groupped <- georef_dataset_already_perfectly_raised %>% 
        #   dplyr::ungroup() %>% 
        #   dplyr::mutate(year = as.character(lubridate::year(time_start))) %>% 
        #   dplyr::group_by(across(setdiff(colnames(.), c("measurement_value", "measurement_unit",
        #                                                 "time_start", "time_end", "measurement", "measurement_processing_level", 
        #                                                 "geographic_identifier", "measurement_type")))) %>% 
        #   dplyr::summarise(measurement_value_georef_perfect = sum(measurement_value, na.rm = TRUE), .groups = "drop")
        # 
        # nominal_catch_to_use_for_raising <- nominal_catch %>% 
        #   dplyr::group_by(across(setdiff(colnames(.), c("measurement_value", "measurement_unit")))) %>% 
        #   dplyr::summarise(sum_nom = sum(measurement_value, na.rm = TRUE), .groups = "drop") %>% 
        #   dplyr::mutate(year = as.character(year)) %>% 
        #   dplyr::left_join(
        #     georef_dataset_not_raised_groupped,
        #     by = dplyr::setdiff(
        #       intersect(colnames(georef_dataset_not_raised_groupped), colnames(nominal_catch)),
        #       c("measurement_value", "measurement_unit", "time_start", "time_end", "measurement", "measurement_processing_level", "measurement_type")
        #     )
        #   ) %>% 
        #   dplyr::mutate(measurement_value_georef_perfect = dplyr::coalesce(measurement_value_georef_perfect, 0)) %>% 
        #   dplyr::mutate(measurement_value = sum_nom - measurement_value_georef_perfect)
        # 
        # nominal_catch_to_use_for_raising_filtered <- nominal_catch_to_use_for_raising%>% 
        #   dplyr::filter(measurement_value >= 1) %>% 
        #   dplyr::select(-c(sum_nom, measurement_value_georef_perfect)) %>% 
        #   dplyr::mutate(time_start = as.Date(sprintf("%s-01-01", year))) %>% 
        #   dplyr::mutate(measurement_unit = "t")
        # 
        # saveRDS(object = nominal_catch_to_use_for_raising_filtered, "data/nominal_catch_to_use_for_raising.rds")
        # 
        # georef_dataset <- iterative_raising(
        #   fact                   = "catch",
        #   georef_dataset         = georef_dataset_to_raise,
        #   nominal_catch          = nominal_catch_to_use_for_raising_filtered,
        #   entity                 = entity,
        #   dim_sets               = dim_sets_raw,
        #   decrease_nei           = FALSE,
        #   passes                 = 2:opts$passes,
        #   dataset_to_add_in_recap = georef_dataset_already_perfectly_raised
        # )
        # # Subsequent raising passes: use progressively less detailed dimension sets
        # # on the data that was not already raised in the first full-dimension pass.
        # 
        # georef_dataset <- rbind(georef_dataset_already_perfectly_raised, georef_dataset)
        
      }
  }
  
  if (opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg %in% c("disaggregate", "remove")) {
    message(
      "BEGIN function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
    )
    
    georef_dataset <-
      function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg(
        entity,
        config,
        opts,
        georef_dataset =
          georef_dataset,
        resolution =
          5,
        action_to_do =
          opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg
      )
    message(
      "END function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
    )
    
    georef_dataset <- georef_dataset$dataset
    CWP.dataset::function_recap_each_step(
      "Disaggregate5deg",
      georef_dataset,
      paste0("The data is disaggregated data on resolution higher than 5o in 5o resolution."),
      "function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg",
      list(
        options_disaggregate_on_5deg_data_with_resolution_superior_to_5deg
      ), entity
    )
  }
  
  if (opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg %in% c("disaggregate", "remove")) {
    message(
      "STEP 5/5: BEGIN function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
    )
    georef_dataset <-
      function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg(
        entity,
        config,
        opts,
        georef_dataset =
          georef_dataset,
        resolution =
          1,
        action_to_do =
          opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg
      )
    message(
      "STEP 5/5: END function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
    )
    
    georef_dataset <- georef_dataset$dataset
    
    CWP.dataset::function_recap_each_step(
      "Disaggregate1deg",
      georef_dataset,
      "This step disaggregate data on resolution higher than 1o in 1o resolution",
      "function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg",
      list(
        options_disaggregate_on_1deg_data_with_resolution_superior_to_1deg
      ), entity
    )
    
  }
  gc()

  list(georef_dataset = georef_dataset, stepnumber = stepnumber)
}
