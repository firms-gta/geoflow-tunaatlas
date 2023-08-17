# This function write a csv containing all the options of the data column of the entity
# Moreover it creates a variable named "options_"name of the corresponding options for each opts$
write_options_to_csv <- function(opts) {
  list_options <- data.frame(Options = character(),
                             Position = character(),
                             stringsAsFactors = FALSE)
  
  for (i in names(opts)) {
    if (i != "") {
      option_value <- paste(opts[[i]], collapse = ' ; ')
      assign(paste0("options_", i), option_value, envir = globalenv())
      
      if (!exists(i)) {
        assign(i, option_value, envir = globalenv())
      }
    }
    
    option_data <- data.frame(Options = i, Position = paste(opts[[i]], collapse = ' ; '))
    list_options <- rbind(list_options, option_data)
  }
  
  list_options <- list_options[-1, ]
  
  write.csv(list_options, "list_options.csv", row.names = FALSE)
}


