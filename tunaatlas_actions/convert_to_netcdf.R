convert_to_netcdf = function(action, config, entity, uploadgoogledrive = TRUE){
  require(dplyr)
  
  opts <- action$options
  wkt2spdf <- function(df, wkt_col_name, id_col_name, crs="+proj=longlat")
  {
    if(require("rgeos")==FALSE)
    {
      stop("You have to install the \"rgeos\" library.")
    }
    
    if(length(which(names(df)==wkt_col_name))==0)
    {
      stop(paste("Cannot found the specified WKT column name (\"", wkt_col_name, "\") in the specified data frame.", sep=""))
    }
    
    if(length(which(names(df)==id_col_name))==0)
    {
      stop(paste("Cannot found the specified ID column name (\"", id_col_name, "\") in the specified data frame.", sep=""))
    }
    
    if(length(which(duplicated(df[,which(names(df)==id_col_name)])==TRUE)) > 0)
    {
      stop(paste("The ID is not unique in the specified data frame.", sep=""))
    }
    
    wkt2sp <- function(wkt, id)
    {
      readWKT(wkt, p4s=CRS(crs), id=as.character(id))
    }
    sp_object_list <- mapply(wkt2sp, df[,which(names(df)==wkt_col_name)], df[,which(names(df)==id_col_name)])
    
    if(length(sp_object_list) == 0)
    {
      stop("Empty list.")
    }
    
    names(sp_object_list) <- df[,which(names(df)==id_col_name)]
    sp_object_collection <- do.call(rbind, sp_object_list)
    row.names(df) <- df[,which(names(df)==id_col_name)]
    
    if(class(sp_object_list[[1]])=="SpatialPolygons")
    {
      return(SpatialPolygonsDataFrame(sp_object_collection, data=df))
    } else {
      if(class(sp_object_list[[1]])=="SpatialPoints")
      {
        return(SpatialPointsDataFrame(sp_object_collection, data=df))
      } else {
        stop(paste("Type ", class(sp_object_list[[1]]), " not yet implmented."), sep="")
      }
    }
  }
  
  write_NetCDF <- function(config, con, dataset_metadata,Variable=NULL, dimensions='all', path = "data"){
    ########################
    #Used Packages:
    #######################
    require(RPostgreSQL)
    require(raster)
    require(ncdf4)
    library(plyr)
    config$logger.info("Beginning function write Netcdf")
    
    
    ##Lecture du tableau de données (2 CAS: SARDARA/ TRAJECTOIRE)
    
    #cas SARDARA
    query_netcdf<-getSQLSardaraQueries(con,dataset_metadata)$query_NetCDF
    res_dimensions_and_variables<-dbGetQuery(con,query_netcdf)
    res_dimensions_and_variables2 <-res_dimensions_and_variables
    
    ##netcdf name
    NetCDF_file_name <- file.path(path,paste0(dataset_metadata$identifier,".nc"))
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
    } else if (tolower(dimensions)=='all' ){dimensions <- names(res_dimensions_and_variables)[-which( names(res_dimensions_and_variables) %in% c('measurement_value','geom_wkt','time_start','time_end'))];aggBy <- c(aggBy,dimensions)
    } else if(tolower(dimensions)=='no' | nchar(dimensions[1]) == 0 ){dimensions = ''}
    
    # res_dimensions_and_variables <- aggregate(res_dimensions_and_variables['measurement_value'],setdiff(res_dimensions_and_variables[aggBy],"unit"),FUN=aggFun)
    res_dimensions_and_variables <- aggregate(res_dimensions_and_variables['measurement_value'],res_dimensions_and_variables[aggBy],FUN='sum')
    
    # head(res_dimensions_and_variables)
    vars <-res_dimensions_and_variables[['measurement_value']]
    
    if('spatial_coverage' %in% names(dataset_metadata)){
      geom_wkt <- dataset_metadata$spatial_coverage
      test <- data.frame(wkt=unique(geom_wkt),id=1)
      sptest <- wkt2spdf(df = test,wkt_col_name = 'wkt',id_col_name = 'id')
      box <- bbox(sptest)
    } else {
      geom_wkt <- res_dimensions_and_variables$geom_wkt
      #####test exact box:
      test <- data.frame(wkt=unique(geom_wkt),id=1:length(unique(geom_wkt)))
      sptest <- wkt2spdf(df = test,wkt_col_name = 'wkt',id_col_name = 'id')
      box <- bbox(sptest)
    }
    ##################
    ##DEFINE DIMENSIONS FOR NETCDF:
    ##################
    ##### TEMPORAL DIMENSION  ##########
    
    dateVector1 <- sort(unique((res_dimensions_and_variables$time_start)))
    library(chron)
    dateVector <- julian(x=as.numeric(format(as.Date(dateVector1),'%m')),d=as.numeric(format(as.Date(dateVector1),'%d')),y=as.numeric(format(as.Date(dateVector1),'%Y')),origin.=c(month = 1, day = 1, year = 1950))
    
    ##### SPATIAL DIMENSION  ##########
    if(!is.null(sp_resolution)){
      longitudeVector = seq(from=box['x','min']+(sp_resolution/2), to=box['x','max']-(sp_resolution/2), by=sp_resolution)
      latitudeVector = seq(from=box['y','min']+(sp_resolution/2), to=box['y','max']-(sp_resolution/2), by=sp_resolution)}
    
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
          ##Taha: modif 07/09/2017 :: sort num dim
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
    # varproj <- ncvar_def(name="crs", units="", dim=NULL, missval=nonAvailable, prec="integer") #compression does not work with current version
    
    if('measurement_unit' %in% names(dataset_metadata)){
      unite <- dataset_metadata$measurement_unit
    } else { unite <- '' }
    # switch (variable_longname,
    #         'catch' = unite <- '???',
    #         'effort' = unite <- '???',
    #         'catch_at_size' = unite <- '???')
    varXd <- ncvar_def(name=as.character(variable),  units=as.character(unite), dim=listDims, missval=nonAvailable, prec="float", compression = compression)
    # varXd <- ncvar_def(name=as.character(variable),  units=as.character(unite), dim=listDims, missval=nonAvailable, prec="float") # compression does not work with current version
    ######################################################################
    ##### CREATE EMPTY NETCDF FILE  ##########  # create netCDF file and put arrays
    ######################################################################
    # netCDF_CF_filename <- paste("SARDARA_",variables,"_",paste(dimensions,collapse = "-"),"_",sp_resolution,"deg_",t_resolution,'D',dateVector1[1],"_",dateVector1[length(dateVector1)],".nc",sep="")
    if(file.exists(netCDF_CF_filename)){
      file.remove(netCDF_CF_filename)
    }
    nc <- nc_create(netCDF_CF_filename,list(varproj, varXd),force_v4 = TRUE, verbose = TRUE)
    cat("netCDF File created")
    ######################################################################
    ##### ADD VALUES TO VARIABLES  ##########
    ######################################################################
    ###Generic loops for The dimensions
    ####################################
    ##Taha modif 07/09/2017 new method to create loops
    if(nchar(dimensions[1])!=0){
      gridDimInd2 <- res_dimensions_and_variables[dimensions]
      for(dim1 in dimensions ){
        if(!(dim1 %in% meaningValue$names) & is.numeric(res_dimensions_and_variables[[dim1]])){mapvalues(gridDimInd2[[dim1]], from =sort(unique(gridDimInd2[[dim1]])), to= 1:length(unique(gridDimInd2[[dim1]]))) }
        if(dim1 %in% meaningValue$names){gridDimInd2[[dim1]]=mapvalues(gridDimInd2[[dim1]], from = meaningValue$values[[ which( meaningValue$names %in% dim1)]], to = meaningValue$ref[[ which( meaningValue$names %in% dim1)]])}
      }
    }else {gridDimInd2= data.frame(time_start=rep(NA,nrow(res_dimensions_and_variables)))}
    
    gridDimInd2$time_start <- julian(x=as.numeric(format(as.Date(res_dimensions_and_variables$time_start),'%m')),d=as.numeric(format(as.Date(res_dimensions_and_variables$time_start),'%d')),y=as.numeric(format(as.Date(res_dimensions_and_variables$time_start),'%Y')),origin.=c(month = 1, day = 1, year = 1950))
    gridDimInd2$time_start <- mapvalues(gridDimInd2$time_start, from = dimT$vals, to = 1:length(dimT$vals))
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
    for(i in 1:nrow(gridDimInd2)){
      config$logger.info(paste('couche ',i,' sur ',nrow(gridDimInd2),sep=''))
      tot <- unlist(as.vector(lapply(gridDimInd2[i,], as.character)))
      resTD <- res_dimensions_and_variables[which(gridDimInd_InTab %in% paste0(tot,collapse='_')),]
      if(!is.null(sp_resolution)){
        resTD$fake_id <- seq(1:nrow(resTD))
        spDF <- wkt2spdf(resTD, "geom_wkt", "fake_id")
        # head(spDF)
        # rast <- rasterize(spDF, raster(ncol=72, nrow=36, xmn=-180, xmx=180, ymn=-90, ymx=90, crs=CRS("+init=epsg:4326")), "v_catch")
        #resolution a automatiser
        rast <- rasterize(spDF, raster(ncol=as.numeric(diff(box['x',]))/sp_resolution, nrow=as.numeric(diff(box['y',]))/sp_resolution, xmn=box['x','min'], xmx=box['x','max'], ymn=box['y','min'], ymx=box['y','max'], crs=CRS("+init=epsg:4326")), 'measurement_value')
        data <- as.matrix(rast)
        data <- t(data[nrow(data):1,])
        data <- replace(data, is.na(data), -9999)
        strt <- as.numeric(gridDimInd2[i,])
        ncvar_put(nc=nc, varid=varXd, vals=data, start = c(1, 1, strt),  count = c(-1, -1, rep(1,ncol(gridDimInd2))))
      }
      if(is.null(sp_resolution)){ strt <- as.numeric(gridDimInd2[i,])
      ncvar_put(nc=nc, varid=varXd, vals=resTD[['measurement_value']], start = c( strt),  count = c( rep(1,ncol(gridDimInd2))))}
      
      
    }
    
    ###add a fishing_fleets meaning Automatically to Non-numerical dimensions
    ################################################################
    # if(nchar(dimensions[1])!=0){
    #   for(indD in 1: length(dimensions)){
    #     loc <-  match(dimensions[indD], unlist(meaningValue$names))
    #     if(!is.na(loc)){
    #       ncatt_put(nc,dimensions[indD],"fishing_fleet_values",paste((meaningValue$ref[[loc]]),collapse = ","))
    #       ncatt_put(nc,dimensions[indD],"fishing_fleet_meanings",paste(gsub(" ","_",as.character(meaningValue$values[[loc]])),collapse=" "))
    #       ncatt_put(nc,dimensions[indD],"valid_range",paste(c(min(meaningValue$ref[[loc]]),max(meaningValue$ref[[loc]])),collapse = ","))}
    #   }}
    
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
      ncatt_put(nc,"longitude","standard_name","longitude")
      ncatt_put(nc,"longitude","axis","X") 
      ncatt_put(nc,"longitude","_CoordinateAxisType","longitude")
      ncatt_put(nc,"longitude","valid_min",box['x','min'])
      ncatt_put(nc,"longitude","valid_max",box['x','max'])
      
      ncatt_put(nc,"latitude","standard_name","latitude")
      ncatt_put(nc,"latitude","axis","Y")
      ncatt_put(nc,"latitude","_CoordinateAxisType","latitude")
      ncatt_put(nc,"latitude","valid_min",box['y','min'])
      ncatt_put(nc,"latitude","valid_max",box['y','max'])
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
    
    #################################
    ##### GLOBAL ATTRIBUTES  ##########
    #################################
    
    ##Extent Search
    if(!is.null(sp_resolution)){
      ncatt_put(nc,0,"geospatial_lat_min",box['y','min'])
      ncatt_put(nc,0,"geospatial_lat_max",box['y','max'])
      ncatt_put(nc,0,"geospatial_lon_min",box['x','min'])
      ncatt_put(nc,0,"geospatial_lon_max",box['x','max'])
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
  
  require(RPostgreSQL)
  dataset_pid <- entity$identifiers[["id"]]
  
  if(!(!grepl("nominal_catch",dataset_pid) | !grepl("deg",dataset_pid) | grepl("0",dataset_pid))){
    config$logger.info("This dataset is not converted to netcdf due to the fact that it contains multiples resolution and/or units")
    return(NULL)
  } #if it is not only one resolution (1 deg or 5 deg), we return nothing as well as if there are several units
  
  ## 1) Extract dataset in the appropriate structure
  
  con <- config$software$output$dbi
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/getSQLSardaraQueries.R")
  
  dataset_metadata<-dbGetQuery(con,paste0("SELECT * FROM metadata.metadata where identifier='",dataset_pid, "'"))
  query_netCDF<-getSQLSardaraQueries(con,dataset_metadata)$query_NetCDF
  dataset<-dbGetQuery(con,query_netCDF)
  ## 2) Extract metadata in the appropriate structure
  # unit
  paste("Data are expressed in one of these units: ",paste(unique(dataset$measurement_unit),collapse = ",",sep=""))
  # variable
  dataset_metadata$variable<-sub('.*\\.', '',dataset_metadata$database_table_name )
  # sp_resolution and sp_resolution_unit
  if (grepl("nominal_catch",dataset_metadata$identifier)){
    dataset_metadata$sp_resolution_unit<-NA
    dataset_metadata$sp_resolution<-NA
    compression = 9
  } else {
    compression = 9
    query_resolution <- paste0("select distinct(cwp_grid.gridtype) from fact_tables.catch as tab
LEFT OUTER JOIN
    area.area USING (id_area)
LEFT JOIN
    area.cwp_grid ON cwp_grid.cwp_code = area.codesource_area where tab.id_metadata=",dataset_metadata$id_metadata)
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
  
  if(length(unique(dataset$measurement_unit)) == 1){
    dataset_metadata$measurement_unit <- unique(dataset$measurement_unit)
  } else {print(config$logger.info("Multiple units shouldn't be handled by netcdf please convert data"))
  }
  #########!!!! 1.2 execution de la fonction pour transformer les données en Netcdf
  write_NetCDF(config = config, con = con, dataset_metadata,Variable='auto',dimensions='all', path = "data")
  
  entity$addResource("netcdf", file.path("data",paste0(dataset_pid, ".nc")))
  if(uploadgoogledrive){
  config$logger.info("Upload netcdf to Google Drive")
  folder_datasets_id <- "16fVLytARK13uHCKffho3kYJgm0KopbKL"
  path_to_dataset_new <- file.path(getwd(), "data", paste0(dataset_pid, ".nc"))
  id_csv_dataset <- drive_upload(path_to_dataset_new, as_id(folder_datasets_id), overwrite = TRUE)$id
  }
  
}

