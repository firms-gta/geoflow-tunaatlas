#' ---
#' title: "Grouppingdifferences"
#' author: "BastienG"
#' date: "2023-09-12"
#' output: html_document
#' ---
#' 
## ----groupping-differences, message=FALSE, warning=FALSE, include=FALSE----

units <- unique(c(unique(init$measurement_unit), unique(final$measurement_unit)))

Other_dimensions <- colnames(init)[colnames(init) != "measurement_unit" & 
  colnames(init)!= "measurement_value" & 
    colnames(init) %notin% parameter_time_dimension] &
  colnames(init) %notin% parameter_geographical_dimension & colnames(init) %notin% parameter_geographical_dimension_groupping

Dimensions <- Other_dimensions

carte_init <- fonction_groupement(Dimensions[[1]],init %>% head(1), final%>% head(1))

Groupped_all <- carte_init[0,]

for (i in Dimensions){
  temporaire <- fonction_groupement(i,init, final)
  assign(paste0("Groupped", i), temporaire)
  
  Groupped_all <- rbind(Groupped_all, temporaire)
}

Groupped_all$Dimension <-as.character(Groupped_all$Dimension)
Groupped_all$Precision <-as.character(Groupped_all$Precision)


## ----fonction-grouppement-for-geographic-dimension, include=FALSE----

geographic_dimension <-  lapply(parameter_geographical_dimension, fonction_groupement, init = init, final = final)


#' 
#' 
## ----fonction-grouppement-for-time-dimension, include=FALSE-----

time_dimension_list_groupped <- lapply(parameter_time_dimension, fonction_groupement, init = init, final = final)

Groupped_time_dimension <- do.call(rbind, time_dimension_list_groupped)


#' 
