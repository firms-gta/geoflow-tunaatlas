res_bil <- inspect_species_group_mismatches(
  nominal_data = iotc_nom,
  georef_data = iotc,
  group_species = c(
    "BLM",  # Black marlin
    "BUM",  # Blue marlin
    "BXQ",  # Marlins nei
    "MLS",  # Striped marlin
    "SFA",  # Indo-Pacific sailfish
    "SAI",  # Atlantic sailfish
    "SSP",  # Shortbill spearfish
    "SPF",  # Longbill spearfish
    "MSP",  # Mediterranean spearfish
    "WHM",  # White marlin
    "BIL"   # Billfishes nei
  ),
  group_label = "BIL_group",
  grouping_keys = c("year", "fishing_fleet", "gear_type"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)

View(res_bil$grouped_diagnosis %>%
  dplyr::filter(regrouping_possible))

# Pas clair la diff entre BXQ,NA,Marlins nei et BIL,NA,"Marlins,sailfishes,etc. nei", surement à regroupper. 
# SSP et SFA,NA,Indo-Pacific sailfish non présent géoref certaines années mais peas très grave 
# apas facile de choisir, doit on augmenter toutes les espèces sur la base de la somme ou seuelment le BIL/BXQ sur la base de la somme.  Peut être tout et on verrra après

res_hammerhead <- inspect_species_group_mismatches(
  nominal_data = iotc_nom,
  georef_data = iotc,
  group_species = c(
    "SPL",  # Scalloped hammerhead
    "SPK",  # Great hammerhead
    "SPZ",  # Smooth hammerhead
    "SPN",  # Hammerhead sharks nei
    "SPY"   # Hammerhead sharks group
  ),
  group_label = "HAMMERHEAD_group",
  grouping_keys = c("year", "fishing_fleet", "gear_type"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)

View(res_hammerhead$grouped_diagnosis %>%
       dplyr::filter(regrouping_possible))

# normalemetn 0 soucis pour IOTC car jamais only_georef_species
# Noter quand mêem que pas clair la diff SPN et SPY.
# SPN,NA,Hammerhead sharks nei,Sphyrna spp
# SPY,NA,"Hammerhead sharks, etc. nei"
# Le miexu surement augmenter sur la base de tout aussi




res_sharks <- inspect_species_group_mismatches(
  nominal_data = iotc_nom,
  georef_data = iotc,
  group_species = c(
    "ALV",  # Thresher
    "PTH",  # Pelagic thresher
    "BTH",  # Bigeye thresher
    "THR",  # Thresher sharks nei
    "SMA",  # Shortfin mako
    "LMA",  # Longfin mako
    "MAK",  # Mako sharks nei
    "POR",  # Porbeagle
    "FAL",  # Silky shark
    "OCS",  # Oceanic whitetip
    "BSH",  # Blue shark
    "RSK",  # Requiem sharks nei
    "SKH"   # Various sharks nei
  ),
  group_label = "PELAGIC_SHARK_group",
  grouping_keys = c("year", "fishing_fleet", "gear_type"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)
# Pb BTH en 2021 only georef, doit le mettre dans SKH car SKH existe en nom
# pour de 77 à 93, que SKH en georef alors que bcp dans le reste, opn peut epetreu augmenter le SKH sur la base de tout. c("BSH", "BTH", "FAL", "LMA", "MAK", "OCS", "POR", etc
# pour les années d'apèrs est-ce uq o'n augmenter sur la base de tout ou espèces par espèces? --> pas facile bcp de recoupemetn
View(res_sharks$grouped_diagnosis %>%
  dplyr::filter(regrouping_possible))



# ICCAT -------------------------------------------------------------------

res_bil <- inspect_species_group_mismatches(
  nominal_data = iccat_nom,
  georef_data = iccat,
  group_species = c(
    "BLM",  # Black marlin
    "BUM",  # Blue marlin
    "BXQ",  # Marlins nei
    "MLS",  # Striped marlin
    "SFA",  # Indo-Pacific sailfish
    "SAI",  # Atlantic sailfish
    "SSP",  # Shortbill spearfish
    "SPF",  # Longbill spearfish
    "MSP",  # Mediterranean spearfish
    "WHM",  # White marlin
    "BIL"   # Billfishes nei
  ),
  group_label = "BIL_group",
  grouping_keys = c("year", "fishing_fleet", "gear_type"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)

# très compliqué en gardant ff et gg, 2022, 2023 toujours manquant les trucs

View(res_bil$grouped_diagnosis %>%
       dplyr::filter(regrouping_possible))

res_bil <- inspect_species_group_mismatches(
  nominal_data = iccat_nom,
  georef_data = iccat,
  group_species = c(
    "BLM",  # Black marlin
    "BUM",  # Blue marlin
    "BXQ",  # Marlins nei
    "MLS",  # Striped marlin
    "SFA",  # Indo-Pacific sailfish
    "SAI",  # Atlantic sailfish
    "SSP",  # Shortbill spearfish
    "SPF",  # Longbill spearfish
    "MSP",  # Mediterranean spearfish
    "WHM",  # White marlin
    "BIL"   # Billfishes nei
  ),
  group_label = "BIL_group",
  grouping_keys = c("year"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)


View(res_bil$grouped_diagnosis )

res_hammerhead <- inspect_species_group_mismatches(
  nominal_data = iccat_nom,
  georef_data = iccat,
  group_species = c(
    "SPL",  # Scalloped hammerhead
    "SPK",  # Great hammerhead
    "SPZ",  # Smooth hammerhead
    "SPN",  # Hammerhead sharks nei
    "SPY"   # Hammerhead sharks group
  ),
  group_label = "HAMMERHEAD_group",
  grouping_keys = c("year", "fishing_fleet", "gear_type"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)

View(res_hammerhead$grouped_diagnosis %>%
       dplyr::filter(regrouping_possible))


res_sharks <- inspect_species_group_mismatches(
  nominal_data = iccat_nom,
  georef_data = iccat,
  group_species = c(
    "ALV",  # Thresher
    "PTH",  # Pelagic thresher
    "BTH",  # Bigeye thresher
    "THR",  # Thresher sharks nei
    "SMA",  # Shortfin mako
    "LMA",  # Longfin mako
    "MAK",  # Mako sharks nei
    "POR",  # Porbeagle
    "FAL",  # Silky shark
    "OCS",  # Oceanic whitetip
    "BSH",  # Blue shark
    "RSK",  # Requiem sharks nei
    "SKH"   # Various sharks nei
  ),
  group_label = "PELAGIC_SHARK_group",
  grouping_keys = c("year", "fishing_fleet", "gear_type"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)
View(res_sharks$grouped_diagnosis %>%
       dplyr::filter(regrouping_possible))




