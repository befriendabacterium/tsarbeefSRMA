convert_varbarlength2sd<-function(pico_df, varbartype_col, varbarlength_cols, replicate_cols){
  
  #identify column indices
  varbarlength_cols_index<-colnames(pico_df)%in%varbarlength_cols
  replicate_cols_index<-colnames(pico_df)%in%replicate_cols
  
  #for the 95% conf intervals, divide by 1.92 to get standard error
  #pico_df[which(pico_df$outcome_plot_varbartype=='95conf'),varbarlength_cols_index]<-pico_df[which(pico_df$outcome_plot_varbartype=='95conf'),cols_to_convert]/1.92
  
  #now convert these all the variance columns (which are now all in standard error format) to standard deviations
  pico_df[,varbarlength_cols_index]<-pico_df[,varbarlength_cols_index]*sqrt(pico_df[,replicate_cols_index])
  
  #remove unwanted objects
  rm(varbarlength_cols_index)
  rm(replicate_cols_index)
  
  #return pico_df df
  return(pico_df)
  
}
  
