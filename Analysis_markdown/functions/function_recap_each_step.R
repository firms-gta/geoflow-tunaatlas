function_recap_each_step=function(nom_dossier, nomrds, explication="",fonctions="", option_list=NULL) {
  if(!exists("options_written_total")){assign("options_written_total", "", envir = .GlobalEnv)}
  if(!exists("explenation_total")){assign("explenation_total", "", envir = .GlobalEnv)}
  dir.create("Markdown")
  dir.create(paste0("Markdown/",nom_dossier))
  nom_dossier <- paste0("Markdown/",nom_dossier)
  class(nomrds$measurement_value) <- "integer"
  rds_t <- (nomrds %>% filter(measurement_unit %in% c("t", "MTNO","MT"))) 
  rds_no <- (nomrds %>% filter(measurement_unit %in% c("no", "NOMT","NO"))) 
  somme_t <- sum(rds_t$measurement_value, na.rm = TRUE)
  somme_no <- sum(rds_no$measurement_value, na.rm = TRUE)
  lines <- nrow(nomrds)
  fwrite(data.frame(somme_t, somme_no, lines), paste0(nom_dossier,"/sums.csv"))
  if (!is.null(option_list)){
    options_substi <- as.list(substitute(option_list))[-1]
    print(options_substi)
    options_written <- ""
    for (i in 1:length(options_substi)){
      options_written <- paste0(options_written, (paste0(options_substi[i], " = ", option_list[i])), 
                                sep = " , \n ")
    }
    print(options_written)
  } else {options_written = "NONE"}
  options_written_total <- assign("options_written_total", paste0(options_written_total, options_written), envir = .GlobalEnv)
  explenation_total <- assign("explenation_total", paste0(explenation_total, explication), envir = .GlobalEnv)
  saveRDS(nomrds,paste0(nom_dossier,"/rds.rds"))
  write(explication, paste0(nom_dossier,"/explication.txt")) 
  write(explenation_total, paste0(nom_dossier,"/explenation_total.txt")) 
  write(fonctions, paste0(nom_dossier,"/fonctions.txt"))
  write(options_written_total, "options_total.txt")
  write(options_written_total, paste0(nom_dossier,"/options_total.txt"))
  write(options_written, paste0(nom_dossier,"/options_written.txt"))
  
}
