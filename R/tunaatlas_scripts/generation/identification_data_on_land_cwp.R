#' Identification des données sur terre dans le jeu de données cwp
#'
#' Cette fonction identifie les données sur terre dans le jeu de données cwp en utilisant la connexion à la base de données fournie et le jeu de données d'entrée.
#'
#' @param con Un objet de connexion à la base de données.
#' @param df_input Le jeu de données d'entrée à utiliser pour l'identification des données sur terre.
#' @return Un vecteur contenant les identifiants uniques des zones sur terre dans le jeu de données cwp.
#' @export
#' @import DBI
#' @import dplyr
#' @importFrom here here

identification_data_on_land_cwp = function(con, df_input){
  
  # Essayer d'exécuter la requête DBI::dbGetQuery
  tryCatch({
    # Votre requête DBI::dbGetQuery
    cwp_grid <- DBI::dbGetQuery(con, "SELECT ON_LAND_P, cwp_code from area.cwp_grid") %>%
      dplyr::rename(geographic_identifier = cwp_code)
  }, error = function(e) {
    message("no connexion to DB trying to unzip file")
    csv_file <- here::here("data/cl_areal_grid.csv")
    if(!file.exists(csv_file)){
      message("no connexion to DB nor zip, downloading cwp_grid")
      
    # Téléchargement du fichier ZIP depuis l'URL
    zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
    local_file <- here::here("data", "cwp_grid.zip")
    download.file(zip_url, local_file, mode = "wb")
    
    # Extraction du contenu du fichier ZIP
    unzip(local_file, exdir = here::here("data"))
    
    # Lecture du fichier CSV extrait
    
    } 
    cwp_grid <- read.csv(csv_file)
    
    # Renommage de colonnes
    cwp_grid <- cwp_grid %>% dplyr::select(ON_LAND_P, CWP_CODE) %>% 
      dplyr::rename(geographic_identifier = CWP_CODE, on_land_p = ON_LAND_P)
  })

  
  # Merge 5x5 catch data with grid
  CA_WITH_GRIDS = merge(df_input, cwp_grid, by = "geographic_identifier")
  CA_WITH_GRIDS$on_land_p <- as.numeric(CA_WITH_GRIDS$on_land_p)
  # Identify catch data in grids on land
  CA_ON_LAND = CA_WITH_GRIDS %>% dplyr::filter(on_land_p ==100)

  return(areas_in_land =unique(CA_ON_LAND$geographic_identifier))
}
