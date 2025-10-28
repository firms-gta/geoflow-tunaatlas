convert_to_netcdf = function(action, config, entity, uploadgoogledrive = TRUE){
  require(dplyr)
  require(RPostgreSQL)
  
  opts <- action$options
  library(sf)
  
  wkt2sfd <- function(df, wkt_col_name, id_col_name, crs="+proj=longlat +datum=WGS84") {
    if (!wkt_col_name %in% names(df)) {
      stop(paste("Cannot find the specified WKT column name (\"", wkt_col_name, "\") in the specified data frame.", sep=""))
    }
    
    if (!id_col_name %in% names(df)) {
      stop(paste("Cannot find the specified ID column name (\"", id_col_name, "\") in the specified data frame.", sep=""))
    }
    
    if(any(duplicated(df[[id_col_name]]))) {
      stop("The ID is not unique in the specified data frame.")
    }
    
    # Convert WKT to sf object
    sf_object <- st_as_sf(df, wkt = wkt_col_name, crs = crs)
    
    # Ensure the ID column is kept
    sf_object[[id_col_name]] <- df[[id_col_name]]
    
    return(sf_object)
  }
  
  
  
  
  # wkt2spdf <- function(df, wkt_col_name, id_col_name, crs="+proj=longlat")
  # {
  #   # if(require("rgeos")==FALSE)
  #   # {
  #   #   stop("You have to install the \"rgeos\" library.")
  #   # }
  #   
  #   if(length(which(names(df)==wkt_col_name))==0)
  #   {
  #     stop(paste("Cannot found the specified WKT column name (\"", wkt_col_name, "\") in the specified data frame.", sep=""))
  #   }
  #   
  #   if(length(which(names(df)==id_col_name))==0)
  #   {
  #     stop(paste("Cannot found the specified ID column name (\"", id_col_name, "\") in the specified data frame.", sep=""))
  #   }
  #   
  #   if(length(which(duplicated(df[,which(names(df)==id_col_name)])==TRUE)) > 0)
  #   {
  #     stop(paste("The ID is not unique in the specified data frame.", sep=""))
  #   }
  #   
  #   wkt2sp <- function(wkt, id)
  #   {
  #     readWKT(wkt, p4s=CRS(crs), id=as.character(id))
  #   }
  #   sp_object_list <- mapply(wkt2sp, df[,which(names(df)==wkt_col_name)], df[,which(names(df)==id_col_name)])
  #   
  #   if(length(sp_object_list) == 0)
  #   {
  #     stop("Empty list.")
  #   }
  #   
  #   names(sp_object_list) <- df[,which(names(df)==id_col_name)]
  #   sp_object_collection <- do.call(rbind, sp_object_list)
  #   row.names(df) <- df[,which(names(df)==id_col_name)]
  #   
  #   if(class(sp_object_list[[1]])=="SpatialPolygons")
  #   {
  #     return(SpatialPolygonsDataFrame(sp_object_collection, data=df))
  #   } else {
  #     if(class(sp_object_list[[1]])=="SpatialPoints")
  #     {
  #       return(SpatialPointsDataFrame(sp_object_collection, data=df))
  #     } else {
  #       stop(paste("Type ", class(sp_object_list[[1]]), " not yet implmented."), sep="")
  #     }
  #   }
  # }
  
  write_NetCDF <- function(config, con, dataset_metadata,Variable=NULL, dimensions='all', path = "data",res_dimensions_and_variables, specie){
    ########################
    #Used Packages:
    #######################
    require(RPostgreSQL)
    require(raster)
    require(ncdf4)
    library(plyr)
    config$logger.info("Beginning function write Netcdf")
    
    # res_dimensions_and_variables2 <-res_dimensions_and_variables
    
    ##netcdf name
    NetCDF_file_name <- file.path(path,paste0(specie,dataset_metadata$identifier,".nc"))
    netCDF_CF_filename <-  NetCDF_file_name
    sp_resolution <- dataset_metadata$sp_resolution
    
    if(!is.null(sp_resolution) && is.na(sp_resolution)){
      sp_resolution <- 5
    }
    sp_resolution_unit <- dataset_metadata$sp_resolution_unit
    
    
    if(grepl("nominal_catch",NetCDF_file_name)){sp_resolution=NULL}
    
    variable <- dataset_metadata$variable
    if(is.null(variable)){variable="catch"}
    
    t_resolution <- as.numeric(diff(c(as.Date(res_dimensions_and_variables$time_start[1]),as.Date(res_dimensions_and_variables$time_end[1])),unit='day'))
    ##################
    ##aggregate data:
    ##################
    #cas nominal catch or else
    if(is.null(sp_resolution)){aggBy="time_start"
    } else {aggBy <- c("geom_wkt","time_start")}
    
    
    if(nchar(dimensions[1]) != 0 & !dimensions[1] %in% c('all','no') ){aggBy <- c(aggBy,dimensions)
    } else if (tolower(dimensions)=='all' ){dimensions <- names(res_dimensions_and_variables)[-which( names(res_dimensions_and_variables) %in% c('measurement_value','geom_wkt','time_start','time_end', "source_authority", "measurement_type"))];aggBy <- c(aggBy,dimensions)
    } else if(tolower(dimensions)=='no' | nchar(dimensions[1]) == 0 ){dimensions = ''}
    # res_dimensions_and_variables <- aggregate(res_dimensions_and_variables['measurement_value'],setdiff(res_dimensions_and_variables[aggBy],"unit"),FUN=aggFun)
    res_dimensions_and_variables <- aggregate(res_dimensions_and_variables['measurement_value'],res_dimensions_and_variables[aggBy],FUN='sum')
    
    # head(res_dimensions_and_variables)
    vars <-res_dimensions_and_variables[['measurement_value']]
    
    if('spatial_coverage' %in% names(dataset_metadata)){
      geom_wkt <- dataset_metadata$spatial_coverage
      test <- data.frame(wkt=unique(geom_wkt),id=1)
      sptest <- wkt2sfd(df = test,wkt_col_name = 'wkt',id_col_name = 'id')
      box <- st_bbox(sptest)
      
    } else {
      geom_wkt <- res_dimensions_and_variables$geom_wkt
      #####test exact box:
      test <- data.frame(wkt=unique(geom_wkt),id=1:length(unique(geom_wkt)))
      sptest <- wkt2sfd(df = test,wkt_col_name = 'wkt',id_col_name = 'id')
      box <- st_bbox(sptest)
      
    }
    ##################
    ##DEFINE DIMENSIONS FOR NETCDF:
    ##################
    ##### TEMPORAL DIMENSION  ##########
    
    dateVector1 <- sort(unique((res_dimensions_and_variables$time_start)))
    library(chron)
    dateVector <- julian(x=as.numeric(format(as.Date(dateVector1),'%m')),d=as.numeric(format(as.Date(dateVector1),'%d')),y=as.numeric(format(as.Date(dateVector1),'%Y')),origin.=c(month = 1, day = 1, year = 1950))
    
    ##### SPATIAL DIMENSION  ##########
    if(!is.null(sp_resolution)) {
      longitudeVector <- seq(from = box["xmin"] + (sp_resolution / 2), 
                             to = box["xmax"] - (sp_resolution / 2), 
                             by = sp_resolution)
      
      latitudeVector <- seq(from = box["ymin"] + (sp_resolution / 2), 
                            to = box["ymax"] - (sp_resolution / 2), 
                            by = sp_resolution)
    }
    
    ######################################################################
    ##### DEFINE SPATIAL and TEMPORAL DIMENSIONS  ##########
    ######################################################################
    # XYT dimensions
    if(!is.null(sp_resolution)){
      dimX <- ncdim_def("longitude", "degrees_east", longitudeVector)
      dimY <- ncdim_def("latitude", "degrees_north", latitudeVector)}
    
    dimT <- ncdim_def("time", "days since 1950-01-01 00:00:00", dateVector, unlim = FALSE)
    
    if(!is.null(sp_resolution)){
      listDims <- list(dimX,dimY)
    } else {listDims <- list()}
    
    names <- list()
    ref=list()
    values <- list()
    meaningValue= list(names=list(names),ref=list(ref),values=list(values))
    ######################################################################
    ##### DEFINE OTHERS DIMENSIONS  ##########
    ######################################################################
    if(nchar(dimensions[1])!=0){
      for(dim in dimensions){
        config$logger.info(dim)
        dimvals <- unique(res_dimensions_and_variables[[dim]])
        if(is.numeric(dimvals)){
          dimVector = sort(dimvals)
          if(grepl('_',dim)){unit=unlist(strsplit(dim,'_'))[2]}
          dim=unlist(strsplit(dim,'_'))[1]
        } else {
          dimVector = 1:length(dimvals)
          unit =""
          meaningValue$names[[match(dim,dimensions)]] <- dim
          meaningValue$ref[[match(dim,dimensions)]] <- dimVector
          meaningValue$values[[match(dim,dimensions)]] <- dimvals
          ##meaningvalue to add automatically as attribut for the character dimensions 
        }
        dimns <- ncdim_def(dim, unit, dimVector)
        listDims <- append(listDims,list(dimns))
      }
    }
    listDims <- append(listDims,list(dimT))
    
    ######################################################################
    ##### DEFINE VARIABLES  ##########
    ######################################################################
    
    if('FillValue' %in% names(dataset_metadata) ){
      nonAvailable <- dataset_metadata$Fillvalue
    } else { nonAvailable <- -9999 }
    if(is.na(nonAvailable)| is.null(nonAvailable)){nonAvailable <- -9999}
    
    varproj <-  ncvar_def(name="crs", units="", dim=NULL, missval=nonAvailable, prec="integer")
    
    if('measurement_unit' %in% names(dataset_metadata)){
      unite <- dataset_metadata$measurement_unit
    } else { unite <- '' }
    # switch (variable_longname,
    #         'catch' = unite <- '???',
    #         'effort' = unite <- '???',
    #         'catch_at_size' = unite <- '???')
    varXd <- ncvar_def(name=as.character(variable),  units=as.character(unite), dim=listDims, missval=nonAvailable, prec="float", compression = compression)
    
    ######################################################################
    ##### CREATE EMPTY NETCDF FILE  ##########  # create netCDF file and put arrays
    ######################################################################
    # netCDF_CF_filename <- paste("SARDARA_",variables,"_",paste(dimensions,collapse = "-"),"_",sp_resolution,"deg_",t_resolution,'D',dateVector1[1],"_",dateVector1[length(dateVector1)],".nc",sep="")
    if(file.exists(netCDF_CF_filename)){
      file.remove(netCDF_CF_filename)
    }
    
    nc <- nc_create(netCDF_CF_filename,list(varXd,varproj),force_v4 = TRUE, verbose = TRUE)
    cat("netCDF File created")
    
    ######################################################################
    ##### ADD VALUES TO VARIABLES  ##########
    ######################################################################
    ###Generic loops for The dimensions
    ####################################
    if(nchar(dimensions[1])!=0){
      gridDimInd2 <- res_dimensions_and_variables[dimensions]
      for(dim1 in dimensions ){
        if(!(dim1 %in% meaningValue$names) & is.numeric(res_dimensions_and_variables[[dim1]])){plyr::mapvalues(gridDimInd2[[dim1]], from =sort(unique(gridDimInd2[[dim1]])), to= 1:length(unique(gridDimInd2[[dim1]]))) }
        if(dim1 %in% meaningValue$names){gridDimInd2[[dim1]]=plyr::mapvalues(gridDimInd2[[dim1]], from = meaningValue$values[[ which( meaningValue$names %in% dim1)]], to = meaningValue$ref[[ which( meaningValue$names %in% dim1)]])}
      }
    }else {gridDimInd2= data.frame(time_start=rep(NA,nrow(res_dimensions_and_variables)))}
    
    gridDimInd2$time_start <- julian(x=as.numeric(format(as.Date(res_dimensions_and_variables$time_start),'%m')),d=as.numeric(format(as.Date(res_dimensions_and_variables$time_start),'%d')),y=as.numeric(format(as.Date(res_dimensions_and_variables$time_start),'%Y')),origin.=c(month = 1, day = 1, year = 1950))
    gridDimInd2$time_start <- plyr::mapvalues(gridDimInd2$time_start, from = dimT$vals, to = 1:length(dimT$vals))
    if(nchar(dimensions[1])!=0){
      res_dimensions_and_variables[dimensions] <- gridDimInd2[dimensions]}
    res_dimensions_and_variables$time_start <- gridDimInd2$time_start
    gridDimInd2 <- unique(gridDimInd2)
    # gridDimInd2 <- as.data.frame(lapply(gridDimInd2, as.character))
    if(nchar(dimensions[1])!=0){
      cols <- c(dimensions,"time_start")
      gridDimInd_InTab <- apply( res_dimensions_and_variables[ , cols ] , 1 , paste , collapse = "_" )
    } else {cols = "time_start" 
    gridDimInd_InTab <- res_dimensions_and_variables[ , cols ]}
    gridDimInd_InTab <- gsub(' ','',gridDimInd_InTab)
    # for(i in 1:nrow(gridDimInd2)){
    #   config$logger.info(paste('couche ',i,' sur ',nrow(gridDimInd2),sep=''))
    #   tot <- unlist(as.vector(lapply(gridDimInd2[i,], as.character)))
    #   resTD <- res_dimensions_and_variables[which(gridDimInd_InTab %in% paste0(tot,collapse='_')),]
    #   if(!is.null(sp_resolution)){
    #     resTD$fake_id <- seq(1:nrow(resTD))
    #     spDF <- wkt2spdf(resTD, "geom_wkt", "fake_id")
    #     # head(spDF)
    #     # rast <- rasterize(spDF, raster(ncol=72, nrow=36, xmn=-180, xmx=180, ymn=-90, ymx=90, crs=CRS("+init=epsg:4326")), "v_catch")
    #     #resolution a automatiser
    #     rast <- rasterize(spDF, raster(ncol=as.numeric(diff(box['x',]))/sp_resolution, nrow=as.numeric(diff(box['y',]))/sp_resolution, xmn=box['x','min'], xmx=box['x','max'], ymn=box['y','min'], ymx=box['y','max'], crs=CRS("+init=epsg:4326")), 'measurement_value')
    #     data <- as.matrix(rast)
    #     data <- t(data[nrow(data):1,])
    #     data <- replace(data, is.na(data), -9999)
    #     strt <- as.numeric(gridDimInd2[i,])
    #     ncvar_put(nc=nc, varid=varXd, vals=data, start = c(1, 1, strt),  count = c(-1, -1, rep(1,ncol(gridDimInd2))))
    #   }
    #   if(is.null(sp_resolution)){ strt <- as.numeric(gridDimInd2[i,])
    #   ncvar_put(nc=nc, varid=varXd, vals=resTD[['measurement_value']], start = c( strt),  count = c( rep(1,ncol(gridDimInd2))))}
    #   
    #   
    # }
    
    #test no to use rgoes anymore library(sf)
    library(terra) # terra replaces raster and is more recent and efficient
    
    res_dimensions_and_variables_sf <- st_as_sf(res_dimensions_and_variables, wkt = "geom_wkt", crs = 4326)
    
    for(i in 1:nrow(gridDimInd2)) {
      config$logger.info(paste('Layer ', i, ' of ', nrow(gridDimInd2), sep = ''))
      tot <- unlist(as.vector(lapply(gridDimInd2[i, ], as.character)))
      # Subset the sf object 
      resTD_sf <- res_dimensions_and_variables_sf[which(gridDimInd_InTab %in% paste0(tot, collapse = '_')), ]
      
      if(!is.null(sp_resolution)) {
        # Create a raster template with resolution and extent matching 'box'
        rast_template <- rast(nrows = as.integer(diff(c(box["ymin"], box["ymax"])) / sp_resolution), 
                              ncols = as.integer(diff(c(box["xmin"], box["xmax"])) / sp_resolution),
                              extent = ext(box["xmin"], box["xmax"], box["ymin"], box["ymax"]), 
                              crs = crs(resTD_sf))
        
        # Rasterize the 'measurement_value' from sf object
        rast_resTD <- rasterize(resTD_sf, rast_template, field = 'measurement_value', fun = sum, background = -9999)
        
        data <- values(rast_resTD)
        strt <- as.numeric(gridDimInd2[i, ])
        ncvar_put(nc = nc, varid = varXd, vals = data, start = c(1, 1, strt), count = c(-1, -1, rep(1, ncol(gridDimInd2))))
      }
      
      if(is.null(sp_resolution)) {
        strt <- as.numeric(gridDimInd2[i, ])
        ncvar_put(nc = nc, varid = varXd, vals = resTD_sf[['measurement_value']], start = c(strt), count = c(rep(1, ncol(gridDimInd2))))
      }
    }
    
    
    ###add a meaning Automatically to Non-numerical dimensions
    ################################################################
    if(nchar(dimensions[1])!=0){
      for(indD in 1: length(dimensions)){
        loc <-  match(dimensions[indD], unlist(meaningValue$names))
        if(!is.na(loc)){
          ncatt_put(nc,dimensions[indD],paste0(meaningValue$names[[loc]],"_values"),paste((meaningValue$ref[[loc]]),collapse = ","))
          config$logger.info(paste(paste0(dimensions[indD],paste0(meaningValue$names[[loc]],"_values"),paste((meaningValue$ref[[loc]]),collapse = ","))
          ))
          ncatt_put(nc,dimensions[indD],paste0(meaningValue$names[[loc]],"_meanings"),paste(gsub(" ","_",as.character(meaningValue$values[[loc]])),collapse=" "))
          ncatt_put(nc,dimensions[indD],paste0(meaningValue$names[[loc]],"_valid_range"),paste(c(min(meaningValue$ref[[loc]]),max(meaningValue$ref[[loc]])),collapse = ","))}
      }}
    
    ######################################################################
    #####  DIM VAR ATTRIBUTES  ##########
    ######################################################################
    if(is.null(sp_resolution)){
      wkt_area <- res_dimensions_and_variables2[,c('geographic_identifier_label','geom_wkt')]
      wkt_area <- wkt_area[-which(duplicated(wkt_area)),]
      ncatt_put(nc,0,"WKT_area_polygon",paste0(wkt_area$geom_wkt,collapse = ','))
      
      ncatt_put(nc,0,"reference_label_polygon",paste0(wkt_area$geographic_identifier_label,collapse = ','))
    }
    
    if(!is.null(sp_resolution)){
      # Longitude attributes
      ncatt_put(nc, varid = "longitude", attname = "standard_name", attval = "longitude")
      ncatt_put(nc, varid = "longitude", attname = "axis", attval = "X") 
      ncatt_put(nc, varid = "longitude", attname = "_CoordinateAxisType", attval = "Lon")
      ncatt_put(nc, varid = "longitude", attname = "valid_min", attval = box["xmin"])
      ncatt_put(nc, varid = "longitude", attname = "valid_max", attval = box["xmax"])
      
      # Latitude attributes
      ncatt_put(nc, varid = "latitude", attname = "standard_name", attval = "latitude")
      ncatt_put(nc, varid = "latitude", attname = "axis", attval = "Y")
      ncatt_put(nc, varid = "latitude", attname = "_CoordinateAxisType", attval = "Lat")
      ncatt_put(nc, varid = "latitude", attname = "valid_min", attval = box["ymin"])
      ncatt_put(nc, varid = "latitude", attname = "valid_max", attval = box["ymax"])
    }
    
    ncatt_put(nc,"time","standard_name","time")
    ncatt_put(nc,"time","axis","T")
    ncatt_put(nc,"time","_CoordinateAxisType","time")
    ncatt_put(nc,"time","valid_min",min(dateVector))
    ncatt_put(nc,"time","valid_max",max(dateVector))
    ncatt_put(nc,"time","calendar",'standard')
    
    ncatt_put(nc,"crs","grid_mapping_name","latitude_longitude")
    ncatt_put(nc,"crs","longitude_of_prime_meridian",0.0)
    ncatt_put(nc,"crs","semi_major_axis",6378137.0)
    ncatt_put(nc,"crs","inverse_flattening",298.257223563)
    
    ncatt_put(nc,as.character(variable),"grid_mapping","crs")
    # Coordinate reference system variable
    crs_var <- ncvar_def('crs', 'Coordinate Reference System', NULL)
    crs_val <- '+proj=longlat +datum=WGS84'
    ncvar_put(nc, crs_var, crs_val)
    
    #################################
    ##### GLOBAL ATTRIBUTES  ##########
    #################################
    
    ##Extent Search
    if(!is.null(sp_resolution)){
      # Global geospatial attributes
      ncatt_put(nc, varid = 0, attname = "geospatial_lat_min", attval = box["ymin"])
      ncatt_put(nc, varid = 0, attname = "geospatial_lat_max", attval = box["ymax"])
      ncatt_put(nc, varid = 0, attname = "geospatial_lon_min", attval = box["xmin"])
      ncatt_put(nc, varid = 0, attname = "geospatial_lon_max", attval = box["xmax"])
    }
    
    
    ncatt_put(nc,0,"time_coverage_start",as.character(min(as.Date(dateVector1))))
    ncatt_put(nc,0,"time_coverage_end",as.character(max(as.Date(dateVector1))))
    ncatt_put(nc,0,"julian_day_unit","days since 1950-01-01 00:00:00")
    ncatt_put(nc,0,"time_min",min(dateVector))
    ncatt_put(nc,0,"time_max",max(dateVector))
    
    ##Extent Information
    ncatt_put(nc,0,"geospatial_lat_units","degrees_north")
    ncatt_put(nc,0,"geospatial_lat_resolution",sp_resolution)
    ncatt_put(nc,0,"geospatial_lon_units","degrees_east")
    ncatt_put(nc,0,"geospatial_lon_resolution",sp_resolution)
    YY <- format(as.Date(max(dateVector) - min(dateVector),origin = "0000-01-01 00:00:00"),'%Y')
    MM <- format(as.Date(max(dateVector) - min(dateVector),origin = "0000-01-01 00:00:00"),'%m')
    DD <- format(as.Date(max(dateVector) - min(dateVector),origin = "0000-01-01 00:00:00"),'%d')
    ncatt_put(nc,0,"time_coverage_duration",paste("P",YY,"Y",as.numeric(MM)-1,"M",as.numeric(DD)-1,"D",sep=""))
    YY <- 0
    MM <- 0
    ncatt_put(nc,0,"time_coverage_resolution",paste("P",YY,"Y",MM,"M",t_resolution,"D",sep=""))
    
    ## Text search
    ##for SARDARA
    ncatt_put(nc,0,"title",entity$titles$title)
    ncatt_put(nc,0,"summary",entity$descriptions$abstract)
    ncatt_put(nc,0,"keywords",paste("tuna, fisheries, ",variable,",tuna RFMOs",sep=""))
    ncatt_put(nc,0,"comment",entity$descriptions$info)
    ncatt_put(nc,0,"source","SARDARA database")
    ncatt_put(nc,0,"Conventions","CF 1.6")
    
    ##creator search
    ncatt_put(nc,0,"creator_email","julien.barde@ird.fr emmanuel.blondel@fao.org bastien.grasset@ird.fr")
    ncatt_put(nc,0,"creator_name","BARDE.J BLONDEL.E GRASSET.B ")
    ncatt_put(nc,0,"date_created", as.character(Sys.time()))
    ncatt_put(nc,0,"date_modified", as.character(Sys.time()))
    ncatt_put(nc,0,"institution","Institut de Recherche pour le Developpement, Food and Agriculture Organisation")
    ncatt_put(nc,0,"contributor_name","ICCAT, IOTC, IATTC, WCPFC, CCSBT")
    ncatt_put(nc,0,"contributor_role","data provider")
    
    
    nc_close(nc)
    
  }
  
  dataset_pid <- entity$identifiers[["id"]]
  
  if(!(!grepl("nominal_catch",dataset_pid) | !grepl("deg",dataset_pid) | grepl("0",dataset_pid))){
    config$logger.info("This dataset is not converted to netcdf due to the fact that it contains multiples resolution and/or units")
    return(NULL)
  } #if it is not only one resolution (1 deg or 5 deg), we return nothing as well as if there are several units
  
  ## 1) Extract dataset in the appropriate structure
  
  con <- config$software$output$dbi
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/getSQLSardaraQueries.R")
  
  dataset_metadata<-dbGetQuery(con,paste0("SELECT * FROM metadata.metadata where identifier='",dataset_pid, "'"))
  query_netCDF<-getSQLSardaraQueries(con,dataset_metadata)$query_NetCDF
  dataset<-dbGetQuery(con,query_netCDF)
  
  # head(dataset)
  # source_authority fishing_fleet gear_type species fishing_mode time_start
  # 1            ICCAT           SEN     09.32     SPZ          UNK 2015-09-01
  # 2            ICCAT         EUFRA      01.1     SPZ           LS 2020-07-01
  # 3            ICCAT         EUFRA      01.1     SPZ           LS 2020-08-01
  # 4            ICCAT         EUFRA      01.1     SPZ           LS 2020-09-01
  # 5            ICCAT         EUFRA      01.1     SPZ           LS 2021-06-01
  # 6            ICCAT         EUFRA      01.1     SPZ           LS 2021-07-01
  # time_end measurement_type measurement_unit                       geom_wkt
  # 1 2015-09-30              UNK                t POLYGON((0 0,0 5,5 5,5 0,0 0))
  # 2 2020-07-31              UNK                t POLYGON((0 0,0 5,5 5,5 0,0 0))
  # 3 2020-08-31              UNK                t POLYGON((0 0,0 5,5 5,5 0,0 0))
  # 4 2020-09-30              UNK                t POLYGON((0 0,0 5,5 5,5 0,0 0))
  # 5 2021-06-30              UNK                t POLYGON((0 0,0 5,5 5,5 0,0 0))
  # 6 2021-07-31              UNK                t POLYGON((0 0,0 5,5 5,5 0,0 0))
  # measurement_value
  # 1              0.64
  # 2              0.10
  # 3              0.48
  # 4              0.17
  # 5              0.01
  # 6              0.03
  # > summary(dataset)
  # source_authority   fishing_fleet       gear_type           species         
  # Length:3397852     Length:3397852     Length:3397852     Length:3397852    
  # Class :character   Class :character   Class :character   Class :character  
  # Mode  :character   Mode  :character   Mode  :character   Mode  :character  
  # 
  # 
  # 
  # fishing_mode         time_start                     
  # Length:3397852     Min.   :1950-04-01 00:00:00.000  
  # Class :character   1st Qu.:1985-05-01 00:00:00.000  
  # Mode  :character   Median :2001-11-01 00:00:00.000  
  # Mean   :1998-02-10 13:48:00.664  
  # 3rd Qu.:2012-08-01 00:00:00.000  
  # Max.   :2021-12-01 00:00:00.000  
  # time_end                       measurement_type   measurement_unit  
  # Min.   :1950-04-30 00:00:00.000   Length:3397852     Length:3397852    
  # 1st Qu.:1985-05-31 00:00:00.000   Class :character   Class :character  
  # Median :2001-11-30 00:00:00.000   Mode  :character   Mode  :character  
  # Mean   :1998-03-12 00:25:29.203                                        
  # 3rd Qu.:2012-08-31 00:00:00.000                                        
  # Max.   :2021-12-31 00:00:00.000                                        
  # geom_wkt         measurement_value  
  # Length:3397852     Min.   :      0.0  
  # Class :character   1st Qu.:      0.7  
  # Mode  :character   Median :      4.0  
  # Mean   :    118.7  
  # 3rd Qu.:     27.1  
  # Max.   :1076771.0  
  
  ## 2) Extract metadata in the appropriate structure
  # unit
  paste("Data are expressed in one of these units: ",paste(unique(dataset$measurement_unit),collapse = ",",sep=""))
  # variable
  if(length(unique(dataset$measurement_unit)) == 1){
    dataset_metadata$measurement_unit <- unique(dataset$measurement_unit)
  } else {
    dataset <- dataset %>% dplyr::filter(measurement_unit == "t")
    print(config$logger.info("Multiple units shouldn't be handled by netcdf only tons are kept"))
    dataset_metadata$measurement_unit <- unique(dataset$measurement_unit)
  }
  
  dataset_metadata$variable<-sub('.*\\.', '',dataset_metadata$database_table_name )
  # sp_resolution and sp_resolution_unit
  if (grepl("nominal_catch",dataset_metadata$identifier)){
    dataset_metadata$sp_resolution_unit<-NA
    dataset_metadata$sp_resolution<-NA
    compression = 9
  } else {
    compression = 9
    query_resolution <- paste0("SELECT DISTINCT cwp_grid.gridtype
                 FROM (
                     SELECT DISTINCT id_area
                     FROM fact_tables.catch
                     WHERE id_metadata = ", dataset_metadata$id_metadata, "
                 ) AS tab
                 LEFT OUTER JOIN area.area USING (id_area)
                 LEFT JOIN area.cwp_grid ON cwp_grid.cwp_code = area.codesource_area;")
    resolution<-dbGetQuery(con,query_resolution)
    
    if(length(resolution$gridtype) == 1){
      position<-regexpr('deg', resolution$gridtype)[1]
      dataset_metadata$sp_resolution <-as.numeric(substr(resolution$gridtype, position-1, position-1))
      dataset_metadata$sp_resolution_unit<-"degree"
    } else if(length(resolution$gridtype) > 1){
      #to be handled
      resolution_type <- "multiple_resolution"
      for (i in length(resolution$gridtype)){
        sp_resolution <- c()
        position<-regexpr('deg', resolution$gridtype[i])[1]
        resolution <-as.numeric(substr(resolution$gridtype, position-1, position-1))
        sp_resolution <- c(sp_resolution,resolution)
      }
      dataset_metadata$sp_resolution = max(sp_resolution)
      
      
      
      config$logger.info("Multiple resolution cannot be handled by netcdf please aggregate or disagreggate some before writing to netcdf,
          for now we use the largest resolution")
    }
    
  }
  
  # dataset_groupped <- dataset %>% dplyr::group_by(species)# %>% dplyr::slice_head(n = 1000) %>% dplyr::filter(species%in% c("YFT", "ALB", "SKJ", "SBF", "BET"))
  #########!!!! 1.2 execution de la fonction pour transformer les données en Netcdf
  
  # start_time <- Sys.time()
  # 
  # for (specie in unique(dataset_groupped$species)){
  #   dataset_species_filtered <- dataset_groupped %>% dplyr::filter(species == specie)
  #   write_NetCDF(config = config, con = con, dataset_metadata,Variable='auto',dimensions='all', res_dimensions_and_variables = dataset_species_filtered, path = "data", 
  #                specie = specie)
  # }
  # 
  # end_time <- Sys.time()
  # time_each_species <- difftime(start_time, end_time)
  
  ### all in one 
  # start_time <- Sys.time()
  
  write_NetCDF(config = config, con = con, dataset_metadata,Variable='auto',dimensions='all', res_dimensions_and_variables = dataset, path = "data", 
               specie = "all")
  
  # end_time <- Sys.time()
  # time_all_in_one <- difftime(start_time, end_time)
  
  # write_NetCDF(config = config, con = con, dataset_metadata,Variable='auto',dimensions='all', path = "data")
  
  entity$addResource("netcdf", file.path("data",paste0(dataset_pid, ".nc")))
  if(uploadgoogledrive){
    config$logger.info("Upload netcdf to Google Drive")
    folder_datasets_id <- "16fVLytARK13uHCKffho3kYJgm0KopbKL"
    path_to_dataset_new <- file.path(getwd(), "data", paste0(dataset_pid, ".nc"))
    id_csv_dataset <- drive_upload(path_to_dataset_new, as_id(folder_datasets_id), overwrite = TRUE)$id
  }
  
}

