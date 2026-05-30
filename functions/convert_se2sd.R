convert_se2sd<-function(pico_df, varbartype_col, varbarlength_cols, replicate_cols){
  
  #identify column indices
  varbarlength_cols_index<-colnames(pico_df)%in%varbarlength_cols
  replicate_cols_index<-colnames(pico_df)%in%replicate_cols
  
  #now convert these all the variance columns (which are now all in standard error format) to standard deviations
  pico_df[,varbarlength_cols_index]<-pico_df[,varbarlength_cols_index]*sqrt(pico_df[,replicate_cols_index])
  
  #remove unwanted objects
  rm(varbarlength_cols_index)
  rm(replicate_cols_index)
  
  #return pico_df df
  return(pico_df)
  
}
  
