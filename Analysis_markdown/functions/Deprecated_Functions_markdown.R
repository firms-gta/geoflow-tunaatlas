fSpatPlan_Convert2PacificRobinson = function(df, buff = 0){
  
  if(!exists("df_robinson")){
    df_robinson <- df}
  return(df_robinson)
  
}
knit_child_map =function(list_map_unit, fig.pathinside = fig.path, folder = "Geographicdiff"){
  assign("unit_name_map", list_map_unit[2][[1]], envir = environment())
  assign("map_for_knit", list_map_unit[1][[1]], envir = environment())
  
  unit_title <- gsub("_","..",unit_name_map)
  save_image(title = paste0("Map of the differences between two datasets for the unit ", unit_title),folder = folder,  plott = map_for_knit,fig.pathinside = fig.pathinside)
  
  knit_child(text = c(
    '```{r results = "asis"}',
    '',
    'unit_title <- gsub("_","..",unit_name_map)',
    '```',
    '',
    #     '    ```{r setup, include=FALSE}',
    # 'knitr::opts_chunk$set(label = paste0("mapdiff", unit_title))', 
    # '```',
    '', 
    '', 
    
    '',
    '```{r results="asis", fig.cap = paste0("Map of the differences between two datasets for the unit ", unit_title)}',
    '',
    'map_for_knit',
    '',
    '```',
    '',
    '',''),
    envir = environment(), quiet= TRUE)
}

function_pie_chart_df <- function(dimension, ...) {
  
  # Make a copy of the function to avoid modifying the original
  local_pie_chart_2 <- pie_chart_2_default
  
  # Modify the dimension formal argument
  formals(local_pie_chart_2)$dimension <- dimension
  
  # Call the function with the ... arguments
  pie_chart_result <- local_pie_chart_2(...)
  
  # Check if pie_chart_result has a df element
  if(is.list(pie_chart_result) && "df" %in% names(pie_chart_result)) {
    df <- pie_chart_result$df
    pie_chart <- pie_chart_result$plot
  } else {
    df <- NULL
    pie_chart <- pie_chart_result
  }
  
  # Process df if not null
  if(!is.null(df)) {
    df <- qflextable2(df, captionn = paste0("Strata movement for the dimension: ", dimension))
  }
  
  return(list(pie_chart = pie_chart, df = df, dimension = dimension))
}


resume_knit_child <- function(x, find_and_print = FALSE, folder = "Piechartsdimensions", fig.pathinside = fig.path) {
  if (find_and_print) {
    for (i in files(folder)) {
      titles <- gsub(".", "", i)
      knitr::knit_child(text = c(
        '```{r otherdim, fig.cap=`titles`, fig.align = "center", out.width = "100%"}',
        '',
        '',
        'knitr::include_graphics(file.path(fig.path, file.path(folder, i)))',
        '',
        '```'
      ), envir = environment(), quiet = TRUE)
    }
  } else {
    dimension_title <- gsub("_", "-", unique(x$dimension))
    title_knit <- paste0("Distribution in value for the dimension : ", dimension_title)
    assign("dimension_title", gsub("_", "-", unique(x$dimension)), envir = environment())
    save_image(title = title_knit, plott = x$pie_chart, folder = folder, fig.pathinside = fig.pathinside)
    assign("x", x, envir = environment())
    
    knitr::knit_child(text = c(
      '```{r distribinvaluedim, results = "asis" ,fig.cap =`title_knit`}',
      '',
      'x$pie_chart',
      '```',
      '',
      '',
      '```{r results = "asis", eval= !is.null(x$df)}',
      'x$df',
      '```',
      ''
    ), envir = environment(), quiet = TRUE)
  }
}

function_knitting <- function(x, titre_init = titre_1, titre_final = titre_2, find_and_print = FALSE, folder, difference = FALSE, fig.pathinside = fig.path, unique_analyse = FALSE) {
  if (find_and_print) {
    for (i in files(folder)) {
      titles <- gsub(".", "", i)
      knitr::knit_child(text = c(
        '```{r evolvaluedim, fig.cap=`titles`, fig.align = "center", out.width = "100%"}',
        '',
        '',
        'knitr::include_graphics(file.path(fig.pathinside, file.path(folder, i)))',
        '',
        '```'
      ), envir = environment(), quiet = TRUE)
    }
  } else {
    assign("Dimension2", gsub("_", "-", unique(x$data$Dimension)))
    assign("y", x, envir = environment())
    
    if (difference) {
      title_knit <- paste0("Differences in percent of value for temporal dimension :  ", Dimension2, " between ", titre_init, " and ", titre_final, " dataset ")
      folder = "Temporaldiff"
    } else {
      title_knit <- paste0("Evolutions of values for the dimension ", Dimension2, " for ", titre_init, " and ", titre_final, " dataset ")
      if (unique_analyse) {
        title_knit <- paste0("Evolutions of values for the dimension ", Dimension2, " for ", titre_init, " dataset ")
      }
    }
    
    save_image(title = title_knit, plott = y, folder = folder, fig.pathinside = fig.pathinside)
    
    knitr::knit_child(text = c(
      '```{r evolvaluedimdiff, fig.cap=`title_knit`, fig.align = "center", out.width = "100%"}',
      '',
      '',
      'y',
      '',
      '```'
    ), envir = environment(), quiet = TRUE)
  }
}

map_unit_knit = function(map, title, find_and_print = FALSE,folder = "Geodistrib", fig.pathinside = fig.path){
  save_image(title = title,plott = map,  folder = folder, fig.pathinside = fig.pathinside)
  
  knitr::knit_child(text = c(
    '',
    '```{r distributioninvalueforunit, fig.cap = title, out.width = "100%"}',
    '',
    'map',
    '```',
    '',
    ''
  ), envir = environment(), quiet= TRUE)
  
}
