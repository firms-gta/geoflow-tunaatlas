#' Negate `%in%` Operator
#'
#' @description This operator checks if elements are not present in a given vector.
#' @param x A vector of elements to check.
#' @param table A vector to check against.
#' @return Logical vector indicating if each element of `x` is not in `table`.
`%notin%` <- Negate(`%in%`)

#' Read Data from Various File Types
#'
#' @description This function reads data based on the provided file path and file type.
#' @param file_path A character string indicating the path of the file to read.
#' @return Data read from the specified file, either as a data frame or data table.
#' @export
read_data <- function(file_path){
  if (grepl("\\.rds$", file_path)) {
    readRDS(file_path)
  } else if (grepl("\\.csv$", file_path)) {
    fread(file_path)
  } else if (grepl("\\.qs$", file_path)) {
    qs::qread(file_path)
  } else {    stop("File type not supported")
  }
}

#' Extract Last Part of a File Path
#'
#' @description This function returns the last part of a file path after removing a specific suffix.
#' @param x A character string representing the file path.
#' @return A character string representing the last part of the file path.
#' @export
last_path <- function(x){
  x <- gsub("/rds.rds", "", x)
  substr(x, max(gregexpr("/", x)[[1]]) + 1, nchar(x))
}

#' Reduce Last Path for Georeferenced Datasets
#'
#' @description This function modifies the last path by removing specific substrings for clarity.
#' @param x A character string representing the file path.
#' @return A character string representing the modified last path.
#' @export
last_path_reduced <- function(x) {
  gsub("georef_dataset", "", last_path(x))
}

#' Check if Variable is NULL or Does Not Exist
#'
#' @description This function checks if a variable is either NULL or does not exist in the environment.
#' @param x The variable to check.
#' @return Logical value indicating whether the variable is NULL or does not exist.
#' @export
is_null_or_not_exist <- function(x) {
  var_name <- deparse(substitute(x))
  if (!exists(var_name, envir = parent.frame()) || 
      is.null(get(var_name, envir = parent.frame()))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Create a Title with Optional Child Header
#'
#' @description This function formats a title with optional child headers for output.
#' @param x A character string representing the title.
#' @param child_headerinside An optional character string for the child header.
#' @return A formatted character string.
#' @export
cat_title <- function(x, child_headerinside = "") {
  if(is.null(child_headerinside)){
    child_headerinside <- ""
  }
  if(child_headerinside == "-#"){
    x <- sub("#", "", x)
    child_headerinside = ""
  }
    
  output = paste0(child_headerinside, x, " \n")
  return(output)
}

#' Check if All Elements in a List are NULL or Empty
#'
#' @description This function checks if all elements in a list are of zero length.
#' @param x A list to check.
#' @return Logical value indicating whether all elements are NULL or empty.
#' @export
isNullList <- function(x) { all(!lengths(x))}

#' Filter Data Frame Based on Provided Parameters
#'
#' @description This function filters a data frame based on specified filtering parameters.
#' @param dataframe_to_filter A data frame to be filtered.
#' @param parameter_filtering A list of parameters to use for filtering.
#' @return A filtered data frame.
#' @export
filtering_function <- function(dataframe_to_filter, parameter_filtering) {
  
  matchingList <- parameter_filtering %>% purrr::keep( ~ !is.null(.) )
  if(length(matchingList)!= 0){  
    
    colnames_to_filter <- colnames(dataframe_to_filter %>% dplyr::select(names(matchingList)))
    
    names(matchingList) <- colnames_to_filter
    
    matchingList <- lapply(matchingList, function(x){ #handling single filter
      if(length(x) == 1){
        x <- c(x, x) }else {
          x
        }
      
    }
    )
    dataframe_to_filter <- dataframe_to_filter%>% dplyr::filter(!! rlang::parse_expr(str_c(colnames_to_filter, matchingList, sep = '%in%', collapse="&")))}
  return(dataframe_to_filter)
}

#' Tidying Data by Keeping Specific Columns
#'
#' @description This function tidies a data frame by selecting specified columns and converting time columns to character.
#' @param dataframe A data frame to tidy.
#' @param parameter_colnames_to_keep_dataframe A character vector of column names to keep.
#' @param time_dimension A character vector of time dimension column names.
#' @return A tidied data frame.
#' @export
tidying_data <- function(dataframe, parameter_colnames_to_keep_dataframe, time_dimension){
  dataframe <- dataframe %>% ungroup()
  dataframe <- dataframe %>% dplyr::select(any_of(parameter_colnames_to_keep_dataframe))
  dataframe <- dataframe %>% mutate_at(all_of(time_dimension), as.character)
  return(dataframe)
  
}

#' Rename Geographic Identifiers and Handle Standard Units
#'
#' @description This function renames geographic identifiers in a data frame and manages non-standard units.
#' @param dataframe_to_filter A data frame to filter and rename columns.
#' @param geo_dim The geographic dimension to rename.
#' @param parameter_fact A parameter indicating the measurement context.
#' @param parameter_UNK_for_not_standards_unit Logical indicating if non-standard units should be labeled as 'UNK'.
#' @param geo_dim_group The grouping geographic dimension to rename.
#' @return A modified data frame with renamed columns.
#' @export
function_geographic_identifier_renaming_and_not_standards_unit <- function(dataframe_to_filter, geo_dim , parameter_fact, parameter_UNK_for_not_standards_unit = TRUE, geo_dim_group){
  if(  parameter_UNK_for_not_standards_unit & parameter_fact == "effort"){
    dataframe_to_filter <- dataframe_to_filter %>% dplyr::mutate(measurement_unit = ifelse(measurement_unit%in%c("HOOKS","FDAYS"), measurement_unit, "UNK" ))}
  
  if(geo_dim != "geographic_identifier" && "geographic_identifier"%notin%colnames(dataframe_to_filter)){
    dataframe_to_filter <- dataframe_to_filter %>% dplyr::rename("geographic_identifier" := {{geo_dim}})
  }
  if(geo_dim_group != "GRIDTYPE" && "GRIDTYPE"%notin%colnames(dataframe_to_filter)){
    dataframe_to_filter <- dataframe_to_filter %>% dplyr::rename("GRIDTYPE" := {{geo_dim_group}})
  }
  dataframe_to_filter
}

#' Check if an Object is a ggplot
#'
#' @description This function checks if an object is a ggplot object.
#' @param obj The object to check.
#' @return Logical value indicating whether the object is a ggplot.
#' @export
is_ggplot <- function(obj) {
  inherits(obj, "gg") || inherits(obj, "ggplot")
}

#' Create and Save a Flextable
#'
#' @description This function creates a flextable from a data frame, optionally saving it as an image.
#' @param x A data frame or flextable to create the table from.
#' @param captionn An optional character string for the table caption.
#' @param autonumm An optional automatic numbering parameter.
#' @param pgwidth A numeric value for the width of the table.
#' @param columns_to_color Optional columns to apply color coding.
#' @param save_folder Optional folder to save the flextable.
#' @param fig.pathinside A character string for the path to save figures.
#' @param grouped_data Optional grouped data for formatting.
#' @param interactive_plot Logical indicating if the output should be interactive.
#' @return A flextable object or a DT datatable if interactive_plot is TRUE.
#' @export
qflextable2 <- function(x, captionn = NULL, autonumm = autonum, pgwidth = 6, columns_to_color = NULL, save_folder = NULL, fig.pathinside = "Figures", grouped_data = NULL, interactive_plot = FALSE, find_and_print = FALSE) {
  captionn <- eval(captionn)
  
  if (all(class(x) == "flextable")) {
    flextabley <- x
  } else {
    if (!(all(class(x) == "data.frame"))) {
      x <- as.data.frame(x %>% dplyr::ungroup())
    }
    if (!is.null(save_folder)) {
      if (!dir.exists(file.path(fig.pathinside, save_folder))) {
        dir.create(file.path(fig.pathinside, save_folder), recursive = TRUE)
      }
      save_path_data <- file.path(fig.pathinside, save_folder, paste0(make.names(captionn), ".csv" ))  # Adjust the file name as needed
      fwrite(x, file = save_path_data)
    }

    y <- x %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate_if(is.factor, as.character) %>% 
      dplyr::mutate_if(is.character, ~ str_replace_all(., ",", ", \n" )) %>% 
      dplyr::mutate_if(is.character, ~ str_replace_all(., "_", "-" )) %>% 
      dplyr::mutate_if(is.numeric, function(.) { round(., 2) }) 

    if(!is.null(grouped_data)){
    y <- as_grouped_data(y, groups = grouped_data)
  }

if (!is.null(columns_to_color)) {
    colormatrix <- ifelse(y %>% dplyr::select(all_of(columns_to_color)) < 0, "red", 
                           ifelse(y %>% dplyr::select(all_of(columns_to_color)) > 0, "green", "white"))
    flextabley <- flextable::flextable(y)
    
     flextabley <- flextabley %>% highlight(j = all_of(columns_to_color), color = colormatrix)
    

    } else {    flextabley <- flextable::flextable(y)}
    
}

if (!is.null(captionn)) {
    flextable_captionned <- flextable::set_caption(flextabley, caption = captionn, style = "Table Caption")
  } else {
    flextable_captionned <- flextabley
  }
  

  
  ft_out <- flextable_captionned %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths * pgwidth / flextable_dim(ft_out)$widths)
  
  if (!is.null(save_folder)) {
    save_path_flextable <- file.path(fig.pathinside, save_folder, paste0(make.names(captionn), ".png"))  # Adjust the file name as needed
    save_as_image(ft_out, path = save_path_flextable)
  }
  if (interactive_plot) {
    # Create an interactive plot using DT
    return(DT::datatable(y))
  }
  ft_out
  
}

#' Group and Summarize Data
#'
#' This function takes two data.tables and groups them by specified columns, 
#' summing the measurement values for each group, and then compares the results 
#' from both data.tables. It computes the loss or gain in measurement values 
#' and provides additional metrics related to the comparison.
#'
#' @param these_col A character vector of column names to group by.
#' @param init A data.table containing the initial measurement data.
#' @param final A data.table containing the final measurement data.
#'
#' @return A data.table containing the results of the comparison between the 
#'         two input data.tables, including summed values, losses or gains, 
#'         and percentage differences.
#' @export
fonction_groupement <- function(these_col, init, final) {
  
  # Ensure input data are data.tables
  init <- as.data.table(init)
  final <- as.data.table(final)
  
  # Compute sum of values for each combination of the columns in "these_col" 
  # and "measurement_unit" in the "init" data.table
  groupement_1 <- init[, .(value_sum_1 = round(sum(measurement_value, na.rm = TRUE), digits = 3),
                           number_lines1 = .N), 
                       by = c(these_col, "measurement_unit")]
  
  groupement_1[, measurement_unit := as.character(measurement_unit)]
  groupement_1[is.na(value_sum_1), value_sum_1 := 0]
  
  # Compute sum of values for each combination of the columns in "these_col" 
  # and "measurement_unit" in the "final" data.table
  groupement_2 <- final[, .(value_sum_2 = round(sum(measurement_value, na.rm = TRUE), digits = 3),
                            number_lines2 = .N), 
                        by = c(these_col, "measurement_unit")]
  
  groupement_2[, measurement_unit := as.character(measurement_unit)]
  groupement_2[is.na(value_sum_2), value_sum_2 := 0]
  
  # Join the two data.tables based on the columns in "these_col" and "measurement_unit"
  fulljoin <- merge(groupement_1, groupement_2, 
                    by = c(these_col, "measurement_unit"), 
                    all = TRUE, 
                    suffixes = c("_1", "_2"))
  
  fulljoin[is.na(value_sum_1), value_sum_1 := 0]
  fulljoin[is.na(value_sum_2), value_sum_2 := 0]
  
  # Calculate losses and gains
  fulljoin[, loss := value_sum_1 - value_sum_2]
  fulljoin[, `Loss / Gain` := fifelse(abs(loss) <= 1, "Egal", 
                                      fifelse(loss > 1, "Loss", "Gain"))]
  fulljoin[, Loss_pourcent := -100 * (loss / value_sum_1)]
  
  # Handle NA values in Loss_pourcent
  fulljoin[is.na(Loss_pourcent) | Loss_pourcent == -Inf, Loss_pourcent := 100]
  
  # Add additional columns
  fulljoin[, Dimension := names(groupement_1)[1]]
  setnames(fulljoin, names(groupement_1)[1], "Precision")
  fulljoin[, Precision := as.character(Precision)]
  
  # Calculate differences
  fulljoin[, loss_nb_ligne := - (number_lines1 - number_lines2)]
  fulljoin[, `Difference in value` := - (value_sum_1 - value_sum_2)]
  setnames(fulljoin, "Loss_pourcent", "Difference (in %)")
  setnames(fulljoin, "loss_nb_ligne", "Difference in number of lines")
  
  # Replace NA with 0 for numeric columns
  fulljoin[, lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = where(is.numeric)]
  
  return(fulljoin)
}


#' Save Plot as Image
#'
#' @description This function saves the current plot as an image in a specified folder.
#' @param title A character string representing the title of the plot.
#' @param plott The plot object to save.
#' @param folder The folder where the image will be saved.
#' @param fig.pathinside The path for saving the figure.
#' @param find_and_print Logical indicating if results should be printed.
#' @return None
#' @export
save_image <- function(title, plott = last_plot(), folder = NULL, fig.pathinside = fig.path, find_and_print = FALSE){
  current <- tmap_mode()
  title <- eval(title)
  if(!is.null(folder)){
    dir.create(file.path(fig.pathinside, folder), recursive = TRUE)
  
    if(all(class(plott) == "flextable")){
    save_as_image(plott,path = file.path(fig.pathinside, file.path(folder, paste0( make.names(title), ".png"))))
    } else if(all(class(plott) == "tmap")){
    tmap_mode("plot")
    filenametmap <- file.path(fig.pathinside, file.path(folder, paste0( make.names(title), ".png")))
    tmap_save(tm = plott, filename = filenametmap)
    tmap_mode(current)
  } else {
ggsave(paste0( make.names(title), ".png"),plot = plott,   device = "png", path = file.path(fig.pathinside, folder), create.dir = TRUE)
  }
  } else { print("Cannot save the image the folder does not exist")}
  # return(plott)

}

#' Spatial Footprint Function
#'
#' This function generates a spatial representation of measurement values from two datasets, 
#' allowing for comparison between initial and final datasets using a provided shapefile.
#'
#' @param variable_affichee A character string indicating the measurement unit to be displayed.
#' @param initial_dataset A data.table containing the initial measurement data (default: init).
#' @param final_dataset A data.table containing the final measurement data (default: final).
#' @param titre_1 A character string for the title of the first dataset (default: "Dataset 1").
#' @param titre_2 A character string for the title of the second dataset (default: "Dataset 2").
#' @param shapefile.fix A spatial object (sf) for the polygons that defines the geographical areas.
#' @param plotting_type A character string indicating the type of plot ("plot" or "view").
#' @param continent An optional spatial object for adding continent borders to the plot.
#'
#' @return A plot object representing the spatial footprint of the measurement values.
#' @export
fonction_empreinte_spatiale <- function(variable_affichee, initial_dataset = init, final_dataset = final, 
                                        titre_1 = "Dataset 1", titre_2 = "Dataset 2", 
                                        shapefile.fix = NULL, plotting_type = "plot", continent = NULL) {
  
  if(is.null(shapefile.fix)){
    stop("Please provide a shape for the polygons")
  }
  
  selection <- function(x) {
    x[, .(geographic_identifier = as.character(geographic_identifier), 
          measurement_value, 
          GRIDTYPE, 
          measurement_unit)]
  }
  
  Initial_dataframe <- selection(as.data.table(initial_dataset))
  Final_dataframe <- selection(as.data.table(final_dataset))
  
  Initial_dataframe[, source := "Initial_dataframe"]
  Final_dataframe[, source := "Final_dataframe"]
  
  geo_data <- rbind(Initial_dataframe, Final_dataframe, use.names = TRUE, fill = TRUE)
  geo_data[, source := fifelse(source == "Initial_dataframe", titre_1,
                               fifelse(source == "Final_dataframe", titre_2, "Error"))]
  
  inner_join_data <- geo_data[, .(measurement_value = sum(measurement_value, na.rm = TRUE)), 
                         by = .(geographic_identifier, measurement_unit, source, GRIDTYPE)][
                           measurement_value != 0]
  
  inner_join_data <- st_as_sf(inner_join(inner_join_data, 
                                shapefile.fix %>% select(code, geom), 
                                by = c("geographic_identifier" = "code")))
  
  if (nrow(inner_join_data%>% dplyr::filter(measurement_unit == variable_affichee)) != 0) {
      inner_join_data <- inner_join_data %>%
        dplyr::mutate(Group = paste0(GRIDTYPE, "_", source))
      tmap_options(component.autoscale = FALSE) # Globally disable autoscale warnings
      
      if (plotting_type == "view") {
        image <- tm_shape(inner_join_data %>% dplyr::filter(measurement_unit == variable_affichee)) +
          tm_fill(
            "measurement_value",
            fill.scale = tm_scale_continuous(values = "brewer.rd_yl_gn", midpoint = 0, n = 8),
            id = "geographic_identifier" 
          ) +
          tm_layout(legend.outside = TRUE) +
          tm_facets(by = "Group", fill.free = TRUE)
      } else {
        image <- tm_shape(inner_join_data %>% dplyr::filter(measurement_unit == variable_affichee)) +
          tm_fill(
            "measurement_value",
            fill.scale = tm_scale_continuous(values = "brewer.rd_yl_gn", midpoint = 0, n = 8),
            id = "geographic_identifier" 
          ) +
          tm_layout(legend.outside = TRUE) +
          tm_facets(by = "Group", fill.free = TRUE) +
          tm_shape(continent) +
          tm_borders()
      }
    
    return(image)
  }
}

#' Save Plots with Subfigures in a Knitr Environment
#'
#' @description This function saves plots and creates subfigures for rendering in RMarkdown.
#' @param plot The plot object to save.
#' @param title A character string for the plot title.
#' @param folder The folder where the plot will be saved.
#' @param fig.pathinside The path for saving the figure.
#' @return None
#' @export
knitting_plots_subfigures <- function(plot, title, folder = "Unknown_folder", fig.pathinside = fig.path) {
  # Check if the function is being run in a knitr environment
  in_knitr <- !is.null(knitr::opts_knit$get("out.format"))
  
  # Save the ggplot object in the current environment with a unique name
  if(is_ggplot(plot)) {
    save_image(title = title, plott = plot, folder = folder, fig.pathinside = fig.pathinside) 
    if(in_knitr) {
      # This will run if inside a knitr/RMarkdown environment
      
      # Adjust title for use in fig.cap
      assign("title_adj", gsub("_", "-", title), envir = environment())
      assign("plot_obj", plot, envir = environment())
      
      # Create the R chunk as a string referencing the ggplot object by its name
      knitr::knit_child(text = c(
        '```{r evolvaluedimdiff, fig.cap=`title_adj`, fig.align = "center", out.width = "100%", results= "asis"}',
        '',
        '',
        'plot(plot_obj)',
        '',
        '```'
      ), envir = environment(), quiet = TRUE)
    } else {
      # This will run if outside a knitr/RMarkdown environment (e.g., in a plain R script)
      print(plot)
    }
  } else if (class(plot) == "tmap"){
    if(in_knitr) {
      # This will run if inside a knitr/RMarkdown environment
      
      # Adjust title for use in fig.cap
      assign("title_adj", gsub("_", "-", title), envir = environment())
      assign("plot_obj", plot, envir = environment())
      # the following code allows to print title even if the tmap object is in view mode, issue is know https://github.com/r-spatial/mapview/issues/146 but not fixed so here is a patch
      knitr::knit_child(text = c(
        '```{r nametitleplotting, echo=FALSE, fig.cap=`title_adj`, fig.height=0.1, fig.width=0.1}',
        '',
        '',
        'par(mar = c(0, 0, 0, 0))',
        'plot.new()',
        '',
        '',
        '```',
        '',
        '',
        '```{r knittingplotsubfigures, fig.align = "center", out.width = "100%", results= "asis"}',
        '',
        '',
        'plot_obj',
        '',
        '',
        '```'
      ), envir = environment(), quiet = TRUE)
      
    } else {
      # This will run if outside a knitr/RMarkdown environment (e.g., in a plain R script)
      plot
    }
  } else {
    stop("Not a ggplot or tmap object")
      }
}

#' Render Subfigures from a List of Plots
#'
#' @description This function creates a grid of subfigures from a list of plots and titles.
#' @param plots_list A list of plot objects.
#' @param titles_list A list of character strings for plot titles.
#' @param general_title A character string for the general title of the plots.
#' @return None
#' @export
render_subfigures <- function(plots_list, titles_list, general_title) {
  # Check if the function is being run in a knitr environment
  in_knitr <- !is.null(knitr::opts_knit$get("out.format"))
  # Check if the lists are of the same length
  if (length(plots_list) != length(titles_list)) {
    stop("The lengths of plots_list and titles_list are not the same.")
  }
  # Check if all plots are ggplots
  if (!all(sapply(plots_list, inherits, "gg"))) {
    stop("Not all items in plots_list are ggplot objects.")
  }
  if (in_knitr) {
    # Create subcaptions
    subcaps <- lapply(titles_list, function(title) paste0("(", title, ")"))
    # Start creating the dynamic R chunk as a string
    chunk_header <- sprintf(
      '```{r subfigures, fig.cap="%s", fig.subcap=c(%s), fig.ncol=2, out.width="50%%", fig.align="center"}',
      general_title,
      paste0('"', subcaps, '"', collapse=", ")
    )
    # Assign each plot in plots_list to a new variable in the environment
    for (i in seq_along(plots_list)) {
      assign(paste0("plot_", i), plots_list[[i]], envir = environment())
    }
    # Create a string for each plot command, referring to the correct plot objects
    plot_commands <- lapply(seq_along(plots_list), function(i) {
      paste0("print(plot_", i, ")")
    })
    # Combine all elements into a single character string
    chunk_text <- paste(chunk_header, paste(plot_commands, collapse = '\n'), '```', sep = '\n')
    # Render the chunk
    result <- knitr::knit_child(text = chunk_text, envir = environment(), quiet = TRUE)
    cat(result)
  } else {
    # If not in a knitr environment, just print the plots
    for (plot in plots_list) {
      print(plot)
    }
  }
}


## ----function-bar-plot-pie-plot---------------
#' Create Pie Charts from Data
#'
#' @description This function creates pie charts from measurement data for one or two datasets.
#' @param dimension A character string indicating the dimension for grouping.
#' @param first A data frame representing the first dataset.
#' @param second An optional second data frame.
#' @param topn An integer for the number of top categories to display.
#' @param titre_1 A character string for the title of the first dataset.
#' @param titre_2 A character string for the title of the second dataset.
#' @param title_yes_no Logical indicating if a title should be displayed.
#' @param dataframe Logical indicating if a data frame should be returned.
#' @return A pie chart or a list containing the pie chart and data frame, if specified.
#' @export
pie_chart_2_default <- function (dimension, first, second = NULL, topn = 5, titre_1 = "first", 
  titre_2 = "second", title_yes_no = TRUE, dataframe = FALSE) 
{
  topn = 5
  first[is.na(first)] <- "NA"
  if (deparse(substitute(dimension)) == "X[[i]]") {
    r <- dimension
  }
  else {
    r <- deparse(substitute(dimension))
  }
  dimension <- gsub("\"", "", r)
  if (dimension == "source_authority") {
    topn = 6
  }
  name1 <- titre_1
  name2 <- titre_2
  if(is.null(second)){
    name1 <- ""
  }
  all_class_i <- first %>% dplyr::group_by(across(c(dimension, 
    "measurement_unit"))) %>% dplyr::summarise(measurement_value = sum(measurement_value, 
    na.rm = TRUE)) %>% filter(measurement_value != 0) %>% 
    dplyr::select(-measurement_value)
  colnames(all_class_i) <- c("class", "measurement_unit")
  all_class_i <- all_class_i %>% mutate(class = paste(class, 
    measurement_unit, sep = " / "))
  provisoire_i <- first %>% dplyr::group_by(dplyr::across(c(dimension, 
    "measurement_unit"))) %>% dplyr::summarise(measurement_value = sum(measurement_value, 
    na.rm = TRUE)) %>% dplyr::group_by(measurement_unit) %>% 
    dplyr::arrange(desc(measurement_value)) %>% dplyr::mutate(id = row_number()) %>% 
    dplyr::mutate(class = as.factor(ifelse(id < topn, !!rlang::sym(dimension), 
      "Others"))) %>% dplyr::group_by(class, measurement_unit) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, 
      na.rm = TRUE)) %>% dplyr::ungroup() %>% dplyr::select(measurement_value, 
    class, measurement_unit) %>% dplyr::group_by(measurement_unit) %>% 
    dplyr::mutate(pourcentage = prop.table(measurement_value) * 
      100) %>% dplyr::mutate(labels = paste0(pourcentage, 
    " ", " % ")) %>% dplyr::arrange(desc(class)) %>% dplyr::mutate(ypos_ligne = cumsum(pourcentage) - 
    0.5 * pourcentage) %>% dplyr::distinct() %>% dplyr::filter(!is.na(class))
  if (!is.null(second)) {
    all_class_t <- first %>% dplyr::group_by(across(c(dimension, 
      "measurement_unit"))) %>% dplyr::summarise(measurement_value = sum(measurement_value, 
      na.rm = TRUE)) %>% filter(measurement_value != 0) %>% 
      dplyr::select(-measurement_value)
    colnames(all_class_t) <- c("class", "measurement_unit")
    all_class_t <- all_class_t %>% mutate(class = paste(class, 
      measurement_unit, sep = " / "))
    provisoire_t <- second %>% dplyr::group_by(across(c(dimension, 
      "measurement_unit"))) %>% dplyr::summarise(measurement_value = sum(measurement_value, 
      na.rm = TRUE)) %>% dplyr::group_by(measurement_unit) %>% 
      dplyr::arrange(desc(measurement_value)) %>% dplyr::mutate(id = row_number()) %>% 
      dplyr::mutate(class = as.factor(ifelse(id < topn, 
        !!rlang::sym(dimension), "Others"))) %>% dplyr::group_by(class, 
      measurement_unit) %>% dplyr::summarise(measurement_value = sum(measurement_value, 
      na.rm = TRUE)) %>% dplyr::ungroup() %>% dplyr::select(measurement_value, 
      class, measurement_unit) %>% dplyr::group_by(measurement_unit) %>% 
      dplyr::mutate(pourcentage = prop.table(measurement_value) * 
        100) %>% dplyr::mutate(labels = paste0(pourcentage, 
      " ", " % ")) %>% dplyr::arrange(desc(class)) %>% 
      dplyr::mutate(ypos_ligne = cumsum(pourcentage) - 
        0.5 * pourcentage) %>% dplyr::distinct() %>% 
      dplyr::filter(!is.na(class))
  }
  if (!is.null(second)) {
    disappearing_stratas <- anti_join(all_class_i %>% dplyr::select(class), 
      all_class_t %>% dplyr::select(class)) %>% distinct()
    appearing_stratas <- anti_join(all_class_t %>% dplyr::select(class), 
      all_class_i %>% dplyr::select(class)) %>% distinct()
    number_disappearing_stratas <- nrow(disappearing_stratas)
    number_appearing_stratas <- nrow(appearing_stratas)
    summary_apparition <- ggdraw() + draw_label(paste0("Number of appearing stratas : ", 
      number_appearing_stratas), size = 10)
    if (number_appearing_stratas != 0) 
      summary_apparition <- summary_apparition + draw_label(paste0(" \nThey are ", 
        paste((appearing_stratas %>% dplyr::select(class) %>% 
          distinct())$class, sep = ";")), size = 10)
    summary_apparition <- summary_apparition + draw_label(paste0(" \nNumber of disappearing stratas : ", 
      number_disappearing_stratas), size = 10)
    if (number_disappearing_stratas != 0) 
      summary_apparition <- summary_apparition + draw_label(paste0(" \nThey are ", 
        paste((disappearing_stratas %>% dplyr::select(class) %>% 
          distinct())$class, sep = ";")), size = 10)
  }
  set.seed(2)
  if (!(is.null(second))) {
    number <- length(unique(unlist(as.character(c(provisoire_i$class, 
      provisoire_t$class)))))
  }
  else {
    number <- length(unique(unlist(as.character(c(provisoire_i$class)))))
  }
  pal <- brewer.pal(number, "Paired")
  if (!(is.null(second))) {
    pal = setNames(pal, unique(unlist(as.character(c(provisoire_i$class, 
      provisoire_t$class)))))
  }
  else {
    pal = setNames(pal, unique(unlist(as.character(c(provisoire_i$class)))))
  }
  ggplot_i <- ggplot(provisoire_i %>% dplyr::filter(!is.na(class))) + 
    aes(x = "", fill = class, group = class, weight = pourcentage) + 
    geom_bar(position = "fill") + scale_fill_hue(direction = 1) + 
    scale_color_hue(direction = 1) + theme_minimal() + coord_polar("y", 
    start = 0) + geom_text(first = (provisoire_i %>% dplyr::filter(!is.na(class)) %>% 
    dplyr::mutate_if(is.numeric, round)), size = 3, aes(x = 1, 
    y = ypos_ligne/100, label = paste0(round(pourcentage), 
      "%")), color = "black") + theme(axis.ticks.x = element_blank(), 
    axis.text.x = element_blank(),
    plot.margin = margin(0, 0, 0, 0)) + labs(x = "", y = "") + 
    scale_fill_manual(values = pal) + guides(fill = guide_legend(title = toupper(r))) + 
    facet_wrap("measurement_unit")
  if (!is.null(second)) {
    to_get_legend <- ggplot(rbind(provisoire_i %>% dplyr::filter(!is.na(class)), 
      provisoire_t %>% dplyr::filter(!is.na(class)))) + 
      aes(x = "", fill = class, group = class, weight = pourcentage) + 
      geom_bar(position = "fill") + guides(fill = guide_legend(title = toupper(r)))+ 
      scale_fill_manual(values = pal)
    legend <- cowplot::get_legend(to_get_legend)
    ggplot_t <- ggplot(provisoire_t %>% dplyr::filter(!is.na(class))) + 
      aes(x = "", fill = class, group = class, weight = pourcentage) + 
      geom_bar(position = "fill") + scale_fill_hue(direction = 1) + 
      scale_color_hue(direction = 1) + theme_minimal() + 
      coord_polar("y", start = 0) + geom_text(first = (provisoire_t %>% 
      dplyr::filter(!is.na(class)) %>% dplyr::mutate_if(is.numeric, 
      round)), size = 3, aes(x = 1, y = ypos_ligne/100, 
      label = paste0(round(pourcentage), "%")), color = "black") + 
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
            plot.margin = margin(0, 0, 0, 0))  +
      labs(x = "", y = "") +  scale_fill_manual(values = pal) +
      guides(fill = guide_legend(title = toupper(r))) + 
    facet_wrap("measurement_unit") + 
      theme(legend.position = "none") 
  }
  else {
    legend <- cowplot::get_legend(ggplot_i + 
      scale_fill_manual(values = pal))
  }
  if (title_yes_no) {
    title <- ggdraw() + draw_label(paste0("Distribution in measurement_value for the dimension : ", 
      r), fontface = "bold", x = 0, hjust = 0) + theme(plot.margin = margin(0, 
      0, 0, 7))
  }
  else {
    title <- ggdraw() + draw_label(" \n ")
  }
  if (!is.null(second)) {
    graph <- plot_grid(ggplot_i + theme(legend.position = "none"), 
      ggplot_t, nrow = 2, labels = c(gsub("\"", "", gsub("~\"", 
        "", deparse(substitute(name1)))), gsub("\"", 
        "", gsub("~\"", "", deparse(substitute(name2))))), 
      label_size = 10, vjust = 1.3, label_x = c(0, 0), 
      label_y = 1.025, axis = "l", align = "v")
    ploting_map <- plot_grid(title, nrow = 2, plot_grid(graph, 
      legend, ncol = 2), rel_heights = c(0.1, 1)) + theme(plot.background = element_rect(color = "black"))
    if (sum(!(round(provisoire_i$pourcentage) == round(provisoire_t$pourcentage))) == 
      0) {
      title <- ggdraw() + draw_label(paste0("Distribution in measurement_value for the dimension : ", 
        r, "\n(same distribution to the nearest rounding for both datasets : \n", 
        gsub("\"", "", gsub("~\"", "", deparse(substitute(name1)))), 
        " and \n", gsub("\"", "", gsub("~\"", "", deparse(substitute(name2)))), 
        ")"), fontface = "bold", x = 0, hjust = 0, vjust = 0.5, 
        size = 13) + theme(plot.margin = margin(0, 0, 
        0, 7))
      graph <- ggplot_i + theme(legend.position = "none")
    }
  }
  else {
    graph <- plot_grid(ggplot_i + theme(legend.position = "none"), 
      nrow = 1, labels = c(gsub("\"", "", gsub("~\"", 
        "", deparse(substitute(name1))))), label_size = 10, 
      vjust = 1.3, label_x = c(0, 0), label_y = 0.8, axis = "l", 
      align = "v")
  }
  if (title_yes_no) {
    if (exists("provisoire_t")) 
      if (sum(!(round(provisoire_i$pourcentage) == round(provisoire_t$pourcentage))) == 
        0) {
        title <- ggdraw() + draw_label(paste0("(same distribution to the nearest rounding for both datasets :\n", 
          gsub("\"", "", gsub("~\"", "", deparse(substitute(name1)))), 
          " and ", gsub("\"", "", gsub("~\"", "", deparse(substitute(name2)))), 
          ")"), fontface = "bold", x = 0, hjust = 0, 
          vjust = 0.5, size = 13) + theme(plot.margin = margin(0, 
          0, 0, 7))
      }
      else {
        title <- ggdraw() + draw_label(" \n ")
      }
  }
  ploting_map <- plot_grid(title, nrow = 2, plot_grid(graph, 
    legend, ncol = 2), rel_heights = c(0.1, 1)) + theme(plot.background = element_rect(color = "black"))
  if (exists("summary_apparition") & dataframe) {
    df <- data.frame(` ` = c("Stratas appearing", "Stratas disappearing"), 
      Number = c(number_appearing_stratas, number_disappearing_stratas), 
      Detail = c(toString(paste((appearing_stratas %>% 
        dplyr::select(class) %>% mutate(class = gsub(" ", 
        "", class)) %>% distinct())$class, sep = ";")), 
        toString(paste((disappearing_stratas %>% dplyr::select(class) %>% 
          mutate(class = gsub(" ", "", class)) %>% distinct())$class, 
          sep = ";"))), check.names = FALSE, fix.empty.names = FALSE)
    if (number_disappearing_stratas == 0 & number_appearing_stratas == 
      0) {
      df <- df %>% dplyr::select(-Detail)
    }
    list_df_plot <- list(plot = ploting_map, df = df)
    return(list_df_plot)
  }
  else {
    return(ploting_map)
  }
}

#' Create 3D Pie Charts from Data Using plotrix
#'
#' @description This function creates 3D pie charts from measurement data for one or two datasets using plotrix.
#' @param dimension A character string indicating the dimension for grouping.
#' @param first A data frame representing the first dataset.
#' @param second An optional second data frame.
#' @param topn An integer for the number of top categories to display.
#' @param titre_1 A character string for the title of the first dataset.
#' @param titre_2 A character string for the title of the second dataset.
#' @param title_yes_no Logical indicating if a title should be displayed.
#' @param dataframe Logical indicating if a data frame should be returned.
#' @return None
#' @export
pie_chart_2_default_plotrix <- function (dimension, first, second = NULL, topn = 5, titre_1 = "first", 
                                         titre_2 = "second", title_yes_no = TRUE, dataframe = FALSE) 
{
  topn = 5
  first[is.na(first)] <- "NA"
  if (deparse(substitute(dimension)) == "X[[i]]") {
    r <- dimension
  } else {
    r <- deparse(substitute(dimension))
  }
  dimension <- gsub("\"", "", r)
  
  if (dimension == "source_authority") {
    topn = 6
  }
  
  name1 <- titre_1
  name2 <- titre_2
  if(is.null(second)) {
    name1 <- ""
  }
  
  # Summarize the data for the first dataset
  provisoire_i <- first %>%
    dplyr::group_by(dplyr::across(c(dimension, "measurement_unit"))) %>%
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
    dplyr::group_by(measurement_unit) %>%
    dplyr::arrange(desc(measurement_value)) %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::mutate(class = as.factor(ifelse(id < topn, !!rlang::sym(dimension), "Others"))) %>%
    dplyr::group_by(class, measurement_unit) %>%
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pourcentage = prop.table(measurement_value) * 100) %>%
    dplyr::mutate(labels = paste0(round(pourcentage), " %")) %>%
    dplyr::arrange(desc(class)) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(class))
  
  # Prepare for second dataset if available
  if (!is.null(second)) {
    provisoire_t <- second %>%
      dplyr::group_by(across(c(dimension, "measurement_unit"))) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
      dplyr::group_by(measurement_unit) %>%
      dplyr::arrange(desc(measurement_value)) %>%
      dplyr::mutate(id = row_number()) %>%
      dplyr::mutate(class = as.factor(ifelse(id < topn, !!rlang::sym(dimension), "Others"))) %>%
      dplyr::group_by(class, measurement_unit) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pourcentage = prop.table(measurement_value) * 100) %>%
      dplyr::mutate(labels = paste0(round(pourcentage), " %")) %>%
      dplyr::arrange(desc(class)) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(class))
  }
  
  # Set up colors
  number <- length(unique(provisoire_i$class))
  pal <- brewer.pal(number, "Paired")
  pal <- setNames(pal, unique(provisoire_i$class))
  
  # Create a pie chart for the first dataset using plotrix
  pie_labels <- provisoire_i$labels
  pie_values <- provisoire_i$pourcentage
  pie_classes <- provisoire_i$class
  
  plotrix::pie3D(pie_values, labels = pie_labels, explode = 0.1,
                 main = paste("Distribution in measurement_value for the dimension:", dimension),
                 col = pal, labelcex = 0.7, radius = 0.8)
  
  # Plot for the second dataset if provided
  if (!is.null(second)) {
    pie_labels_t <- provisoire_t$labels
    pie_values_t <- provisoire_t$pourcentage
    pie_classes_t <- provisoire_t$class
    
    plotrix::pie3D(pie_values_t, labels = pie_labels_t, explode = 0.1,
                   main = paste("Distribution in measurement_value for the dimension:", dimension, " (", titre_2, ")"),
                   col = pal, labelcex = 0.7, radius = 0.8)
  }
  
  # If title_yes_no is TRUE, add a title using cowplot
  if (title_yes_no) {
    title <- paste0("Distribution in measurement_value for the dimension: ", r)
    if (!is.null(second)) {
      title <- paste0(title, "\nComparing ", titre_1, " and ", titre_2)
    }
    title
  }
}

#' Create Bar Plots from Measurement Data
#'
#' @description This function creates bar plots comparing measurement data for one or two datasets.
#' @param first A data frame representing the first dataset.
#' @param second An optional second data frame.
#' @param dimension A character string indicating the dimension for grouping.
#' @param topn An integer for the number of top categories to display.
#' @param titre_1 A character string for the title of the first dataset.
#' @param titre_2 A character string for the title of the second dataset.
#' @param fill_colors Optional vector of fill colors for the bars.
#' @param outline_colors Optional vector of outline colors for the bars.
#' @return A bar plot object.
#' @export
bar_plot_default <- function(first, 
                             second = NULL, 
                             dimension, 
                             topn = 10, 
                             titre_1 = "first", 
                             titre_2= "second", 
                             fill_colors = NULL, 
                             outline_colors = NULL) {
  
  name1 = titre_1
  name2 = titre_2


  # Handle NA values and get dimension name
  first[is.na(first)] <- "NA"

  
  if (deparse(substitute(dimension)) == "X[[i]]") {
    r <- dimension
  } else {
    r <- deparse(substitute(dimension))
  }
  dimension <- gsub("\"", "", r)
  
  if (dimension == "source_authority") {
    topn = 6
  }
  first$dataset <- name1
  second$dataset <- name2
  dataset <- rbind(first, second)
  # Function to generate summary and plot
  generate_plot <- function(data, title, dimensioninside, topninside, dataset) {

        data_summary <- data %>%
        dplyr::group_by(dplyr::across(c(dimensioninside,   "measurement_unit", dataset))) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% dplyr::group_by(measurement_unit, dataset) %>% 
    dplyr::arrange(desc(measurement_value)) %>% dplyr::mutate(id = row_number()) %>% 
    dplyr::mutate(class = as.factor(ifelse(id < topn, !!rlang::sym(dimensioninside), "Others"))) %>%       dplyr::group_by(class, measurement_unit, dataset) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
          group_by(measurement_unit, dataset) %>% 
      dplyr::arrange(desc(ifelse(class == "Others", -Inf, measurement_value))) # Order data
    
    # Reorder class as a factor
    # data_summary$class <- factor(data_summary$class, levels = data_summary$class)
    
    # Create the bar plot
    plot <- ggplot(data = data_summary, aes(x = class, y = measurement_value, fill = class, color = class)) +
  geom_col() +
  labs(x = dimension, y = "Total Measurement") +
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size = 7), 
        legend.key.size = unit(.4, "cm"),
        axis.text.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0)) + 
  ggtitle(title)+ 
    facet_grid(c("measurement_unit", "dataset"))

    
    if (!is.null(fill_colors) && !is.null(outline_colors)) {
      plot <- plot +
        scale_fill_manual(values = fill_colors) + 
        scale_color_manual(values = outline_colors)
    }

    return(plot)
  }

  # Generate plot for initial data
  if(is.null(second)){
    bar_plot <- generate_plot(first, title = NULL, dimensioninside = dimension, topninside = topn)
  } else {
  
  # If final data is provided, generate plot for final data and return combined plot
    second[is.na(second)] <- "NA"
    bar_plot <- generate_plot(dataset,title =NULL, dimensioninside = dimension, topninside = topn)
    # bar_plot <- plot_grid(bar_plot, bar_plot_final, ncol = 2)
  }
  return(bar_plot)
}


#' Compute Summary of Differences
#'
#' This function computes the summary of differences in measurement values 
#' between two datasets by grouping them by measurement unit and calculating 
#' the sum of values. It also computes the percentage difference.
#'
#' @param init A data.table containing the initial measurement data.
#' @param final A data.table containing the final measurement data.
#' @param titre_1 A character string for the title of the first dataset (default: "Dataset 1").
#' @param titre_2 A character string for the title of the second dataset (default: "Dataset 2").
#'
#' @return A data.table summarizing the differences between the two datasets, 
#'         including total measurements and percentage differences.
#' @export
compute_summary_of_differences <- function(init, final, titre_1 = "Dataset 1", titre_2 = "Dataset 2") {
  
  # Ensure input data are data.tables
  init <- as.data.table(init)
  final <- as.data.table(final)
  
  # Group and summarize initial dataset
  init_group <- init[, .(titre_test1 = sum(measurement_value, na.rm = TRUE)), by = measurement_unit]
  
  # Group and summarize final dataset
  final_group <- final[, .(titre_test2 = sum(measurement_value, na.rm = TRUE)), by = measurement_unit]
  
  # Compute summary of differences
  summary <- merge(init_group, final_group, by = "measurement_unit", all = TRUE)
  
  # Replace NA with 0 in numeric columns and calculate differences
  summary[is.na(titre_test1), titre_test1 := 0]
  summary[is.na(titre_test2), titre_test2 := 0]
  
  summary[, Difference := -titre_test1 + titre_test2]
  summary[, `Difference (in %)` := 100 * (Difference / titre_test1)]
  
  # Rename columns for clarity
  setnames(summary, "titre_test1", titre_1)
  setnames(summary, "titre_test2", titre_2)
  
  # Arrange the summary by measurement_unit
  setorder(summary, measurement_unit)
  
  return(summary)
}


