function_raise_data<-function(fact,source_authority_filter,dataset_to_raise,dataset_to_compute_rf,nominal_dataset_df,x_raising_dimensions,
                              decrease_when_rf_inferior_to_one = FALSE){
  
  dataset_to_raise<-dataset_to_raise[which(dataset_to_raise$source_authority %in% source_authority_filter),]
  
  dataset_to_compute_rf<-dataset_to_compute_rf[which(dataset_to_compute_rf$source_authority %in% source_authority_filter),]
  
  nominal_dataset_df<-nominal_dataset_df[which(nominal_dataset_df$source_authority %in% source_authority_filter),]
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/raise_get_rf.R")
  
  df_rf <- raise_get_rf(df_input_incomplete = dataset_to_compute_rf,
                        df_input_total = nominal_dataset_df,
                        x_raising_dimensions = c(x_raising_dimensions,"measurement_unit")
  )
  saveRDS(df_rf, paste0("data/",gsub(Sys.time(),pattern = " ", replacement = "_"),"raisingfactordataset.rds"))
  
  if (fact=="catch"){
    raising_dimensions=c(x_raising_dimensions,"measurement_unit")
  } else if (fact=="effort"){
    raising_dimensions=x_raising_dimensions
    df_rf$measurement_unit=NULL
  }
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/raise_incomplete_dataset_to_total_dataset.R")
  
  data_raised<-raise_incomplete_dataset_to_total_dataset(df_input_incomplete = dataset_to_raise,
                                                         df_input_total = nominal_dataset_df,
                                                         df_rf = df_rf,
                                                         x_raising_dimensions = raising_dimensions,
                                                         decrease_when_rf_inferior_to_one = decrease_when_rf_inferior_to_one,
                                                         threshold_rf = NULL)
  
  return(list(data_raised = data_raised$df, df_rf = df_rf))
  
}