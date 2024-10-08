`%notin%` <- Negate(`%in%`)

# Function to read data based on file type
read_data <- function(file_path) {
  if (grepl("\\.rds$", file_path)) {
    readRDS(file_path)
  } else if (grepl("\\.csv$", file_path)) {
    fread(file_path)
  } else if (grepl("\\.qs$", file_path)) {
    qs::qread(file_path)
  } else {    stop("File type not supported")
  }
}

last_path = function(x) {
  x <- gsub("/rds.rds", "", x)
  substr(x, max(gregexpr("/", x)[[1]]) + 1, nchar(x))
}
last_path_reduced = function(x) {
  gsub("georef_dataset", "", last_path(x))
}
is_null_or_not_exist <- function(x) {
  var_name <- deparse(substitute(x))
  if (!exists(var_name, envir = parent.frame()) || 
      is.null(get(var_name, envir = parent.frame()))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

cat_title = function(x, child_headerinside ="") {
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
isNullList = function(x) all(!lengths(x))
filtering_function= function(dataframe_to_filter, parameter_filtering){
  
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
tidying_data <- function(dataframe, parameter_colnames_to_keep_dataframe, time_dimension){
  dataframe <- dataframe %>% ungroup()
  dataframe <- dataframe %>% dplyr::select(any_of(parameter_colnames_to_keep_dataframe))
  dataframe <- dataframe %>% mutate_at(all_of(time_dimension), as.character)
  return(dataframe)
  
}
function_geographic_identifier_renaming_and_not_standards_unit= function(dataframe_to_filter, geo_dim , parameter_fact, parameter_UNK_for_not_standards_unit = TRUE, geo_dim_group){
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
is_ggplot <- function(obj) {
  inherits(obj, "gg") || inherits(obj, "ggplot")
}


qflextable2 <- function(x, captionn = NULL, autonumm = autonum, pgwidth = 6, columns_to_color = NULL, save_folder = NULL, fig.pathinside = "Figures", grouped_data = NULL, interactive_plot = FALSE, find_and_print = FALSE) {
  captionn <- eval(captionn)
  if(find_and_print){
    
    filepath <- file.path(fig.pathinside, save_folder, paste0(make.names(captionn), ".png"))
    knitr::knit_child(text = paste0(
      '\n```{r evolvaluedim, fig.cap="', captionn, '", fig.align="center", out.width="100%"}',
      '\nknitr::include_graphics("', filepath, '")',
      '\n```\n'
    ), envir = environment(), quiet = TRUE)
  } else {
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
}

fonction_groupement = function(these_col, init, final){
  
  # Compute sum of values for each combination of the columns in "these_col" and "measurement_unit" in the "init" dataframe
  groupement_1 <- init %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!rlang::syms(these_col), measurement_unit) %>%
    dplyr::summarise(value_sum_1 = round(sum(measurement_value, na.rm=TRUE),digits = 3)) %>%
    # dplyr::summarise(value_sum_1 = round(((sum(measurement_value, na.rm=TRUE))))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value_sum_1 = dplyr::coalesce(value_sum_1, 0)) %>%
    dplyr::mutate(measurement_unit = as.character(measurement_unit)) %>%
    dplyr::mutate(number_lines1 = n())
  
  # Compute sum of values for each combination of the columns in "these_col" and "measurement_unit" in the "final" dataframe
  groupement_2 <- final %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!rlang::syms(these_col), measurement_unit) %>%
    # dplyr::summarise(value_sum_2 = round(((sum(measurement_value, na.rm=TRUE))))) %>%
    dplyr::summarise(value_sum_2 = round(sum(measurement_value, na.rm=TRUE),digits = 3)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value_sum_2 = dplyr::coalesce(value_sum_2, 0)) %>%
    dplyr::mutate(measurement_unit = as.character(measurement_unit)) %>%
    dplyr::mutate(number_lines2 = n())
  
  # Join the two dataframes based on the columns in "these_col" and "measurement_unit"
  fulljoin <- dplyr::full_join(groupement_1, groupement_2) %>%
    dplyr::mutate(value_sum_2 = dplyr::coalesce(value_sum_2, 0)) %>%
    dplyr::mutate(value_sum_1 = dplyr::coalesce(value_sum_1, 0)) %>%
    dplyr::mutate(loss = value_sum_1 - value_sum_2) %>%
    # Compute whether the difference in value is a gain or loss
    dplyr::mutate(`Loss / Gain` = dplyr::case_when(abs(loss) <= 1 ~ "Egal", loss > 1 ~"Loss",  loss < 1 ~"Gain")) %>%
    dplyr::mutate(Loss_pourcent = - (100*((value_sum_1 - value_sum_2)/value_sum_1))) %>%
    dplyr::mutate(Dimension = colnames(groupement_1[1])) %>%
    dplyr::rename("Precision" = 1) %>%
    dplyr::mutate(Precision = as.character(Precision)) %>%
    dplyr::mutate(value_sum_2 = dplyr::coalesce(value_sum_2, 0)) %>%
    # Set Loss_pourcent to 100 if it is NA or -Inf or 0 
    dplyr::mutate(Loss_pourcent = base::ifelse(is.na(Loss_pourcent)|Loss_pourcent==-Inf, 100, Loss_pourcent)) %>%
    dplyr::ungroup() %>%
    # Compute the difference in the number of lines between the two dataframes
    dplyr::mutate(loss_nb_ligne = - (number_lines1 - number_lines2)) %>%
    dplyr::mutate(`Difference in value`= - (value_sum_1 - value_sum_2)) %>% #final - init
    dplyr::rename(`Difference (in %)` = Loss_pourcent,`Difference in number of lines` = loss_nb_ligne)%>%
    dplyr::mutate_if(is.numeric, list(~replace_na(., 0)))
  
  return(fulljoin)
}
save_image = function(title, plott = last_plot(), folder = NULL, fig.pathinside = fig.path, find_and_print = FALSE){
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

# Function to create spatial plots from init final variable
fonction_empreinte_spatiale <- function(variable_affichee, initial_dataset = init, final_dataset = final, titre_1 = "Dataset 1", titre_2 = "Dataset 2", 
shapefile.fix, plotting_type = "plot", continent) {

  selection <- function(x) {
    
    # x <- dtplyr::lazy_dt(x)
    x %>% 
      dplyr::ungroup() %>% 
      dplyr::select(geographic_identifier, measurement_value, GRIDTYPE, measurement_unit) %>%
      dplyr::mutate(geographic_identifier = as.character(geographic_identifier)) %>% 
      as.data.frame()
  }
  
  Initial_dataframe <- selection(initial_dataset)
  Final_dataframe <- selection(final_dataset)
  
  geo_data <- gdata::combine(Initial_dataframe, Final_dataframe)
  rm(Initial_dataframe, Final_dataframe)
  gc()
  
  geo_data <- geo_data %>% 
    dplyr::mutate(source = dplyr::case_when(
      source == "Initial_dataframe" ~ eval(parse(text = "titre_1")),
      source == "Final_dataframe" ~ eval(parse(text = "titre_2")),
      TRUE ~ "Error"
    ))
  
  inner_join <- st_as_sf(geo_data %>% 
                           dplyr::group_by(geographic_identifier, measurement_unit, source, GRIDTYPE) %>%
                           dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
                           dplyr::filter(measurement_value != 0) %>%
                           dplyr::inner_join(shapefile.fix %>% dplyr::select(-GRIDTYPE), by = c("geographic_identifier" = "cwp_code"))
  )
  if (nrow(inner_join %>% dplyr::filter(measurement_unit == variable_affichee)) != 0) {
    if (plotting_type == "view") {
      image <- tm_shape(inner_join %>% dplyr::filter(measurement_unit == variable_affichee)) +
        tm_fill("measurement_value", palette = "RdYlGn", style = "cont", n = 8, id = "name", midpoint = 0) +
        tm_layout(legend.outside = FALSE) + tm_facets(by = c("GRIDTYPE", "source"), free.scales = TRUE)
    } else {
      image <- tm_shape(inner_join %>% dplyr::filter(measurement_unit == variable_affichee)) +
        tm_fill("measurement_value", palette = "RdYlGn", style = "cont", n = 8, id = "name", midpoint = 0) +
        tm_layout(legend.outside = FALSE) + tm_facets(by = c("GRIDTYPE", "source"), free.scales = TRUE) + tm_shape(continent) + tm_borders()
    }
    
    return(image)
  }
}

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
      
      # Create the R chunk as a string referencing the ggplot object by its name
      knitr::knit_child(text = c(
        '```{r evolvaluedimdiff, fig.cap=`title_adj`, fig.align = "center", out.width = "100%", results= "asis"}',
        '',
        '',
        'plot_obj',
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

pie_chart_2_default = function (dimension, first, second = NULL, topn = 5, titre_1 = "first", 
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
    axis.text.x = element_blank()) + labs(x = "", y = "") + 
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
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())  +
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
        axis.text.x = element_blank()) + 
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


#' Compute Summary of Differences Between Two Datasets
#'
#' This function computes the differences between two datasets based on their measurement units.
#' It returns a summary dataframe that shows the differences in values and percentages for each unit.
#'
#' @param init A dataframe representing the initial dataset.
#' @param final A dataframe representing the final dataset.
#' @param titre_1 A string representing the name for the title of the initial dataset in the summary.
#' @param titre_2 A string representing the name for the title of the final dataset in the summary.
#' 
#' @return A dataframe summarizing the differences between the two datasets.
#' @examples
#' \dontrun{
#' init_dataset <- data.frame(measurement_unit = c("unit1", "unit2"), measurement_value = c(10, 20))
#' final_dataset <- data.frame(measurement_unit = c("unit1", "unit2"), measurement_value = c(15, 25))
#' summary <- compute_summary_of_differences(init_dataset, final_dataset, "Initial Data", "Final Data")
#' print(summary)
#' }
#' @export
compute_summary_of_differences <- function(init, final, titre_1 = "Dataset 1", titre_2 = "Dataset 2", meanorsum = "sum") {
  
  if(meanorsum == "sum"){
    # Group and summarize initial dataset
    init_group <- init %>%
      dplyr::group_by(measurement_unit) %>%
      dplyr::summarise(titre_test1 = sum(measurement_value)) 
    
    # Group and summarize final dataset
    final_group <- final %>%
      dplyr::group_by(measurement_unit) %>%
      dplyr::summarise(titre_test2 = sum(measurement_value))
  } else{
    
    # Group and summarize initial dataset
    init_group <- init %>%
      dplyr::group_by(measurement_unit) %>%
      dplyr::summarise(titre_test1 = mean(measurement_value)) 
    
    # Group and summarize final dataset
    final_group <- final %>%
      dplyr::group_by(measurement_unit) %>%
      dplyr::summarise(titre_test2 = mean(measurement_value))
  }
  # Compute summary of differences
  summary <- dplyr::full_join(init_group, final_group) %>%
    dplyr::mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%
    dplyr::arrange(measurement_unit) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      measurement_unit = paste0(measurement_unit),
      Difference = -titre_test1 + titre_test2,
      `Difference (in %)` = 100 * (Difference / titre_test1)
    ) %>%
    dplyr::rename_with(~ case_when(
      .x == "titre_test1" ~ titre_1,
      .x == "titre_test2" ~ titre_2,
      TRUE ~ .x
    ))
  
  return(summary)
}

