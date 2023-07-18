#strata_in_georef_but_not_in_nominal_report
copyrmd("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_Markdown/strata_in_georef_but_no_nominal.Rmd")


tryCatch({
  for (i in c( 'IATTC','IOTC', 'ICCAT', 'WCPFC', 'CCSBT')){
    entity$data$actions[[1]]$options$filtering <- list(source_authority = c(paste(i)))
    dir.create(paste0("georef_not_nominal_markdown/",i, "/figures"), recursive = TRUE, showWarnings = FALSE)
    
    parameters_child_global <- list(action = action,
                                    entity = entity, config = config, debugging = TRUE,
                                    strata = c("species", "year", "source_authority"), 
                                    fig.path = paste0("georef_not_nominal_markdown/",i, "/figures/"))
    child_env_global = new.env()
    list2env(parameters_child_global, env = child_env_global)
    rmarkdown::render("strata_in_georef_but_no_nominal.Rmd",
                      envir = child_env_global,
                      output_file = paste0("georef_not_nominal_markdown/",i, "/",i, "_strata_in_georef_but_not_in_nominal_stratadifferent.html"),
                      )
  }
}, error = function(e) {
  # Handle the error, e.g., print an error message
  message("An error occurred: ", conditionMessage(e))
})


