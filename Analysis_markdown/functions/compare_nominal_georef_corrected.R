compare_nominal_georef_corrected <- function(nominal, georef_mapped, list_strata = list(c("species", "year", "source_authority", "gear_type", "fishing_fleet", "geographic_identifier_nom"))) {
  # Convertir les data.frames en data.tables
  setDT(nominal)
  setDT(georef_mapped)
  
  # Créer la colonne "year" à partir de time_start
  georef_mapped[, year := as.character(year(ymd(time_start)))]
  nominal[, year := as.character(year(ymd(time_start)))]
  
  # Conserver uniquement les données en tonnes
  georef_mapped_tons <- georef_mapped[measurement_unit == "t"]
  
  # Initialise une liste pour stocker les résultats (un résultat pour chaque liste de dimensions à conserver pour faire la comparaison)
  results <- list()
  
  for (strata in list_strata) {
    # Nom pour la catégorie actuelle de strata
    name <- paste0(toString(strata))
    
    # Agréger les données pour le nominal et georef sur les colonnes spécifiées dans 'strata' (ex groupper les données par années, espèces, engins, pavillon)
    nominal_grouped <- nominal[, .(measurement_value_nominal = sum(measurement_value, na.rm = TRUE)), by = strata]
    georef_mapped_grouped <- georef_mapped[, .(measurement_value_georef = sum(measurement_value, na.rm = TRUE)), by = strata]
    georef_mapped_tons_grouped <- georef_mapped_tons[, .(measurement_value_georef_tons = sum(measurement_value, na.rm = TRUE)), by = strata]
    
    # # Retirer les valeurs des colonnes pour comparer uniquement les strates (si on veut garder que elles)
    nominal_grouped_without_value <- nominal_grouped[, .SD, .SDcols = strata]
    georef_grouped_without_value <- georef_mapped_grouped[, .SD, .SDcols = strata]
    georef_tons_grouped_without_value <- georef_mapped_tons_grouped[, .SD, .SDcols = strata]
    
    
    # # Assurer que les colonnes sont dans le même ordre pour la comparaison
    setcolorder(georef_grouped_without_value, names(nominal_grouped_without_value))
    setcolorder(georef_tons_grouped_without_value, names(nominal_grouped_without_value))
    
    # Trouver les strates présentes dans georef_mapped mais absentes de nominal
    georef_no_nominal <- fsetdiff(georef_grouped_without_value, nominal_grouped_without_value, all = FALSE)
    georef_no_nominal_with_value <- merge(georef_mapped_tons_grouped, georef_no_nominal, by = strata, all = FALSE)
    sum_georef_no_nominal_tons <- sum(georef_no_nominal_with_value$measurement_value_georef_tons ,na.rm = TRUE)
    
    
    # Comparer uniquement les données en tonnes
    georef_tons_no_nominal <- fsetdiff(georef_tons_grouped_without_value, nominal_grouped_without_value, all = FALSE)
    
    # Comparer les valeurs des strates communes entre nominal et georef_mapped pour les données en tonnes
    georef_sup_nominal <- merge(nominal_grouped, georef_mapped_tons_grouped, by = strata, all = FALSE)
    
    # Vérifier si les colonnes existent après le merge
    if ("measurement_value_georef_tons" %in% names(georef_sup_nominal) && 
        "measurement_value_nominal" %in% names(georef_sup_nominal)) {
      georef_sup_nominal[, Difference := measurement_value_georef_tons - measurement_value_nominal]
      georef_sup_nominal <- georef_sup_nominal[round(Difference, 3) > 1] # Supérieur strictement à 1, on s'affranchit des petits kouaks
    } else {
      georef_sup_nominal <- data.table()  # Retourne une table vide s'il n'y a pas de données
    }
    
    if ("fishing_fleet" %in% colnames(georef_sup_nominal)){
      tons_nei_georef <- georef_no_nominal_with_value[
        fishing_fleet == "NEI" ,
        sum(measurement_value_georef_tons)] + georef_sup_nominal[
          fishing_fleet == "NEI" ,
          sum(measurement_value_georef_tons) 
        ]} else {
          tons_nei_georef <- 0
        }
    
    tons_aggregated_georef <- georef_no_nominal_with_value[
      species %in% c("TUN", "TUS" ,"BIL"),
      sum(measurement_value_georef_tons)
    ] + georef_sup_nominal[
      species %in% c("TUN", "TUS" ,"BIL"),
      sum(measurement_value_georef_tons)
    ]
    
    if ("fishing_fleet" %in% colnames(nominal_grouped)){
      tons_nei_nominal <- nominal_grouped[
        fishing_fleet == "NEI",
        sum(measurement_value_nominal)
      ]} else {tons_nei_nominal <- 0}
    
    
    sum_georef_sup_nom <- sum(georef_sup_nominal$Difference, na.rm = TRUE)
    
    suffisant <- ifelse(sum_georef_no_nominal_tons + sum_georef_sup_nom -(tons_aggregated_georef + tons_nei_georef) > 0, FALSE, TRUE)
    # Stocker les résultats
    results[[name]] <- list(
      georef_no_nominal = georef_no_nominal,           # Strates dans georef mais absentes dans nominal
      georef_no_nominal_with_value = georef_no_nominal_with_value %>% dplyr::rename(measurement_value = measurement_value_georef_tons),           # Strates dans georef mais absentes dans nominal avec la valeur totale
      georef_tons_no_nominal = georef_tons_no_nominal, # Strates en tonnes absentes dans nominal
      georef_sup_nominal = georef_sup_nominal,          # Strates où georef est supérieur à nominal
      tons_nei_nominal = tons_nei_nominal,          # Strates nei qui pourraient expliquer les différences
      tons_nei_georef = tons_nei_georef,          # Strates nei qui pourraient expliquer les différences
      sum_georef_no_nominal = sum_georef_no_nominal_tons, 
      suffisant = suffisant, 
      tons_aggregated_georef = tons_aggregated_georef,
      sum_georef_sup_nom = sum_georef_sup_nom
    )
  }
  
  return(results)
}
