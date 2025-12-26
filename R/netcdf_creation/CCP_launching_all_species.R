library(httr)
library(jsonlite)
library(rstudioapi)
library(readr)

user <- "bastien.grasset65011"
context <- "%2Fd4science.research-infrastructures.eu%2FD4OS%2FBlue-CloudTrainingLab"
auth_ep <- "https://accounts.d4science.org/auth/realms/d4science/protocol/openid-connect/token"
client_id <- "blue-cloud.d4science.org"
ccp_ep <- "https://ccp.cloud.d4science.org"

getToken <- function(user, pwd, context) {
  scope <- paste0("offline_access d4s-context:", context)
  res <- POST(
    auth_ep,
    body = list(
      grant_type = "password",
      client_id = client_id,
      username = user,
      password = pwd,
      scope = scope
    ),
    encode = "form",
    add_headers("Content-Type" = "application/x-www-form-urlencoded")
  )
  txt <- content(res, "text", encoding = "UTF-8")
  if (status_code(res) != 200) stop(txt)
  tok <- fromJSON(txt)
  tok$access_token
}

# ---- AUTH AVANT LA BOUCLE ----
pwd <- askForPassword("Password to login")
at <- getToken(user, pwd, context)

# Template request (sans guillemets dans name)
request <- fromJSON('{
  "inputs":{
    "ccpimage":"ghcr.io/firms-gta/geoflow-tunaatlas-netcdf-creation-method:sha-0c0fa52fff3b",
    "Pathtodataset":"/workspace/data/global_catch_tunaatlasird_level2_1950_2023.qs",
    "dimensions":"gear_type_label,fishing_fleet_label",
    "parameterfiltering":"species=NULL",
    "name":"GlobalTunaAtlaslevel2",
    "Onebyspecies":"false",
    "ccpmaxtime":"86400",
    "ccpsize":"large"
  },
  "outputs":{
    "stdout":{"transmissionMode":"value"},
    "stderr":{"transmissionMode":"value"}
  },
  "response":"raw",
  "subscribers":[{"successUri":"http://registry:8080/executions/outputs/archive-to-folder"}]
}', simplifyVector = FALSE)

# Species list
df <- read_csv("data/global_catch_ird_level2_1950_2023_harmonized.csv", show_col_types = FALSE)
species_vec <- sort(unique(df$species))

process_id <- "aaf26bef-d72f-4af3-a9ac-1a16045254cd"
url <- paste0(ccp_ep, "/processes/", process_id, "/execution")

jobs <- list()

post_one <- function(url, request, at) {
  POST(
    url,
    body = request,
    encode = "json",
    add_headers(
      Authorization = paste("Bearer", at),
      Accept = "application/json"
    )
  )
}


for (sp in species_vec) {
  
  tries <- 0
  max_tries <- 10
  
  repeat {
    tries <- tries + 1
    if (tries > max_tries) stop("Too many retries for species ", sp)
    
  request$ccpnote <- paste(
    "Export NetCDF GTA level-2",
    "- species =", sp,
    "- dimensions = gear_type_label,fishing_fleet_label",
    "- one job per species"
  )
  
  request$inputs$parameterfiltering <- paste0("species=", sp)
  request$inputs$name <- paste0("GlobalTunaAtlaslevel2_", sp)
  request$inputs$Onebyspecies <- "true"
  
  res <- POST(
    url,
    body = request,
    encode = "json",
    add_headers(
      Authorization = paste("Bearer", at),
      Accept = "application/json"
    )
  )
  
  sc <- status_code(res)
  
  if (sc == 201) {
    js <- fromJSON(content(res, "text", encoding="UTF-8"))
    jobs[[sp]] <- js$jobID
    cat("Species", sp, "-> 201 | jobID:", js$jobID, "\n")
    break
  }
  
  if (sc == 401) {
    cat("Species", sp, "-> 401, waiting & retrying...\n")
    Sys.sleep(3)
    at <- getToken(user, pwd, context)
    next
  }
  
  # Autres erreurs → attendre aussi
  cat("Species", sp, "->", sc, "waiting & retrying...\n")
  Sys.sleep(5)
}

Sys.sleep(1)
}

print(jobs)


# Merging all contents ----------------------------------------------------

library(fs)
library(stringr)

## --- Paramètres ---
src_root  <- "~/workspace/CCP/outputs/Clone_of_TunaAtlas_NetCDF_export_(unit_×_resolution)_New_image_EDITO__v._1.0.2"
dest_root <- "~/workspace/CCP/outputs/ALL_MERGED"

# Crée le dossier de destination (sans toucher aux permissions)
dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)

# Dossiers outputs, outputs(1), outputs(2)...
out_dirs <- list.dirs(src_root, recursive = FALSE, full.names = TRUE)
out_dirs <- out_dirs[grepl("^outputs(\\(\\d+\\))?$", basename(out_dirs))]

# Tous les .nc (récursif dans chaque dossier)
nc_files <- unlist(lapply(out_dirs, function(d) {
  list.files(d, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
}), use.names = FALSE)

# Parser unit + res depuis le nom : *_<unit>_<res>.nc (res = 1deg/5deg)
parse_unit_res <- function(filename) {
  m <- regexec("_([A-Za-z0-9]+)_(\\ddeg)\\.nc$", filename)
  mm <- regmatches(filename, m)[[1]]
  if (length(mm) == 0) return(list(unit = "unknown", res = "unknown"))
  list(unit = mm[2], res = mm[3])
}

# Renommer si doublon
unique_dest_path <- function(dest_path) {
  if (!file.exists(dest_path)) return(dest_path)
  
  ext  <- sub("^.*\\.", "", dest_path)
  stem <- sub(paste0("\\.", ext, "$"), "", basename(dest_path))
  dir  <- dirname(dest_path)
  
  i <- 1L
  repeat {
    candidate <- file.path(dir, sprintf("%s_duplicated_%03d.%s", stem, i, ext))
    if (!file.exists(candidate)) return(candidate)
    i <- i + 1L
  }
}

copied <- 0L

for (f in nc_files) {
  fn <- basename(f)
  info <- parse_unit_res(fn)
  
  dest_dir <- file.path(dest_root, paste0("res=", info$res), paste0("unit=", info$unit))
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  dest_path <- file.path(dest_dir, fn)
  dest_path <- unique_dest_path(dest_path)
  
  ok <- file.copy(f, dest_path, overwrite = FALSE)
  if (!ok) warning("Copy failed for: ", f, " -> ", dest_path)
  
  copied <- copied + 1L
}

cat("Done. Copied:", copied, "NetCDF files into:", dest_root, "\n")
