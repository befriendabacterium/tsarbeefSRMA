# 1. SOURCE ALL FUNCTIONS ----------------------------------------------------

sapply(list.files('functions/', full.names = T), source)

# # 2. CONVERT EXTRACTED PERCENTAGES TO PROPORTIONS -------------------------------------------------------------------------
# 
# #vector of extracted proportion columns
# extracted_proportion_cols<-c(
#                               "outcome_plot_proportionresistancedeterminants_intervention_mean",
#                               "outcome_plot_proportionresistancedeterminants_intervention_errorbarlimit",
#                               "outcome_plot_proportionresistancedeterminants_control_mean",
#                               "outcome_plot_proportionresistancedeterminants_control_errorbarlimit"
#                               )
# 
# #pico_processed$outcome_plot_proportionorpercentage
# 
# ##for the outcomes extracted as percentages, divide by 100 to get proportions in 0-1 range
# pico_processed<-convert_percent2proportion(pico_df = pico_processed, cols_to_convert=extracted_proportion_cols)
# 
# rcompanion::plotNormalHistogram(pico_processed$outcome_plot_proportionresistancedeterminants_intervention_mean[pico_processed$study_publicationID=='lowrance2007'])
# rcompanion::plotNormalHistogram(car::logit(pico_processed$outcome_plot_proportionresistancedeterminants_intervention_mean[pico_processed$study_publicationID=='lowrance2007'], adjust=0.025))
# rcompanion::plotNormalHistogram(metafor::transf.arcsin(pico_processed$outcome_plot_proportionresistancedeterminants_intervention_mean[pico_processed$study_publicationID=='lowrance2007']))
# 
# #check
# pico_processed[,which(colnames(pico_processed)%in%extracted_proportion_cols)]

# 3. CONVERT EXTRACTED BELOW ZERO MEANS TO ZERO -------------------------------------------------------------------------
# These are just minor extraction errors, because the minimum value for count/proportion data is 0

extracted_mean_cols<-c(
  "outcome_plot_totaldeterminantslog10_intervention_mean",
  "outcome_plot_totaldeterminantslog10_control_mean",
  "outcome_plot_totalresistancedeterminantslog10_intervention_mean",
  "outcome_plot_totalresistancedeterminantslog10_control_mean",
  "outcome_plot_proportionresistancedeterminants_intervention_mean",
  "outcome_plot_proportionresistancedeterminants_control_mean"
)

col_ids<-which(colnames(pico_processed)%in%extracted_mean_cols)

#loop to convert all below zeros (excluding NAs) to zero
for (i in col_ids){
  belowzero<-which((pico_processed[,i]<0)==TRUE)
  pico_processed[belowzero,i]<-0
}

#check how many are below zero in each column
colSums(apply(pico_processed[,extracted_mean_cols],2,function(x){x<0}),na.rm=T)

# 4. CALCULATE LENGTH (1/2) OF ERROR BARS (produces _var columns) -------------------------------------------------------------------------

#get varbar length
pico_processed<-calculate_varbarlength(pico_df = pico_processed)

# 5. CALCULATE VARIANCE FROM THE EXTRACTED VARIANCE BAR LIMITS -------------------------------------------------------------------------

varbarlength_cols<-c(
                      "outcome_plot_totaldeterminantslog10_intervention_var",
                      "outcome_plot_totaldeterminantslog10_control_var",
                      "outcome_plot_totalresistancedeterminantslog10_intervention_var",
                      "outcome_plot_totalresistancedeterminantslog10_control_var",
                      "outcome_plot_proportionresistancedeterminants_intervention_var",
                      "outcome_plot_proportionresistancedeterminants_control_var"
                    )

replicate_cols<-c(
                  "outcome_plot_totaldeterminantslog10_intervention_reps",
                  "outcome_plot_totaldeterminantslog10_control_reps",
                  "outcome_plot_totalresistancedeterminantslog10_intervention_reps",
                  "outcome_plot_totalresistancedeterminantslog10_control_reps",
                  "outcome_plot_proportionresistancedeterminants_intervention_reps",
                  "outcome_plot_proportionresistancedeterminants_control_reps"
                  )


#N.B. NEED TO ADAPT THIS FUNCTION FOR CONFIDENCE INTERVALS AND STANDARD DEVIATIONS AS IT CURRENTLY ASSUMES EVERYTHING IS A STANDARD ERROR
#convert var bar lengths to standard deviation according to varbartype
pico_processed<-convert_varbarlength2sd(
                              pico_df = pico_processed, #pico_processed dataframe
                              varbartype_col = 'outcome_plot_varbartype',
                              varbarlength_cols = varbarlength_cols, #vector of var bar length column names
                              replicate_cols = replicate_cols #vector of replicate (n=?) column names
                              )
#check output
pico_processed[,varbarlength_cols]

# 6. TRANSFER AVAILABLE PROPORTION DATA TO NEW TEMPORARY DATAFRAME -----------------------

#create a dataframe, filling it with core data the raw resistance proportion means and sds to start
meta_df<-as.data.frame(
  cbind(
    intervention_mean_logitpropres=pico_processed$outcome_actual_logitproportionresistancedeterminants_intervention_mean,
    intervention_sd_logitpropres=pico_processed$outcome_actual_logitproportionresistancedeterminants_intervention_sd,
    intervention_reps_logitpropres=pico_processed$outcome_actual_logitproportionresistancedeterminants_intervention_reps,
    control_mean_logitpropres=pico_processed$outcome_actual_logitproportionresistancedeterminants_control_mean,
    control_sd_logitpropres=pico_processed$outcome_actual_logitproportionresistancedeterminants_control_sd,
    control_reps_logitpropres=pico_processed$outcome_actual_logitproportionresistancedeterminants_control_reps,
    intervention_mean_arcsinpropres=pico_processed$outcome_actual_arcsinproportionresistancedeterminants_intervention_mean,
    intervention_sd_arcsinpropres=pico_processed$outcome_actual_arcsinproportionresistancedeterminants_intervention_sd,
    intervention_reps_arcsinpropres=pico_processed$outcome_actual_arcsinproportionresistancedeterminants_intervention_reps,
    control_mean_arcsinpropres=pico_processed$outcome_actual_arcsinproportionresistancedeterminants_control_mean,
    control_sd_arcsinpropres=pico_processed$outcome_actual_arcsinproportionresistancedeterminants_control_sd,
    control_reps_arcsinpropres=pico_processed$outcome_actual_arcsinproportionresistancedeterminants_control_reps
  )
)

origin<-NA

#specify where the existing proportion data came from
origin[complete.cases(meta_df)]<-'raw'

#where proportions are directly available form plots, add these

# 7. FILL IN MISSING DATA IN THE META-ANALYSIS DATAFRAME WITH DATA FROM PLOTS OF PROPORTIONS ----------------------------------

#IT IS BECAUSE VARIANCE IS BEING TRANSFORED TO LOGIT AFTER CALC!!!!!
# 
# cols_to_add<-c(
#   "outcome_plot_proportionresistancedeterminants_intervention_mean",
#   "outcome_plot_proportionresistancedeterminants_intervention_var",
#   "outcome_plot_proportionresistancedeterminants_intervention_reps",
#   "outcome_plot_proportionresistancedeterminants_control_mean",
#   "outcome_plot_proportionresistancedeterminants_control_var",
#   "outcome_plot_proportionresistancedeterminants_control_reps"
# )
# 
# #make temp df of plotted props
# plotprops<-dplyr::select(pico_processed, cols_to_add)
# #get an index of the rows with data in all 4 cols (i.e. rowSums isn't NA)
# dataavailable<-which(!is.na(rowSums(plotprops)))
# #use the index to fill the holes in the meta_df
# meta_df[dataavailable,1:6]<-plotprops[dataavailable,] #to be logit-transformed
# meta_df[dataavailable,7:12]<-plotprops[dataavailable,] #to be arcsin-transformed
# #meta_df[dataavailable,c(1,2,4,5)]<-car::logit(meta_df[dataavailable,c(1,2,4,5)],adjust=0.025) #logit-transform all columns except reps
# #meta_df[dataavailable,c(7,8,10,11)]<-metafor::transf.arcsin(meta_df[dataavailable,c(7,8,10,11)]) #arcsin-transform all columns except reps
# #convert cols to numeric
# meta_df<-as.data.frame(apply(meta_df[,], 2, as.numeric))
# 
# #specify where the proportion data came from
# origin[dataavailable]<-'figures'

#TO RESUME MONDAY 15/09/25 ----------------------------------

# 8. FILL IN MISSING DATA in the META-ANALYSIS DATAFRAME WITH PROPORTIONS FROM DATA SIMULATED FROM MEANS AND SDS OF THE RES AND TOTAL  ----------------------------------

#index of rows with missing data
missingprevs<-rowSums(is.na(meta_df[,]))==ncol(meta_df)

#check names of studies with missing prevelance/proportion data
pico_processed$study_publicationID[missingprevs]

#turn sharma missing prevs to FALSE as they are missing but not calculable as no 'total' data
#missingprevs[pico_processed$study_publicationID=='sharma2008']<-FALSE

#ESTIMATE INTERVENTION PROPORTIONS

#create empty vectors of simulated means and standard deviations of proportion of resistance determinants
logitproportionresistancedeterminants_intervention_mean_simulated<-c()
logitproportionresistancedeterminants_intervention_sd_simulated<-c()
arcsinproportionresistancedeterminants_intervention_mean_simulated<-c()
arcsinproportionresistancedeterminants_intervention_sd_simulated<-c()

for (i in which(missingprevs)){
  
  set.seed(123)
  #simulate total resistance determinants
  totalresistancedeterminants_simulated<-rnorm(
    mean=pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_mean[i],
    sd=pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_var[i],
    n=1000
  )
  set.seed(123)
  #simulate total determinants                                
  totaldeterminants_simulated<-rnorm(
    mean=pico_processed$outcome_plot_totaldeterminantslog10_intervention_mean[i],
    sd=pico_processed$outcome_plot_totaldeterminantslog10_intervention_var[i],
    n=1000
  )
  
  #sort them because it's likely they're correlated
  # totalresistancedeterminants_simulated<-sort(totalresistancedeterminants_simulated)
  # totaldeterminants_simulated<-sort(totaldeterminants_simulated)
  
  #calculated simulated proportion
  proportionresistancedeterminants_simulated<-(10^totalresistancedeterminants_simulated)/(10^totaldeterminants_simulated)
  #turn all proportions over 1 to 1
  proportionresistancedeterminants_simulated[proportionresistancedeterminants_simulated>1]<-1
  
  logitproportionresistancedeterminants_simulated<-car::logit(proportionresistancedeterminants_simulated, adjust=0.025)
  arcsinproportionresistancedeterminants_simulated<-metafor::transf.arcsin(proportionresistancedeterminants_simulated)
  
  #bind to vector of simulated intervention means
  logitproportionresistancedeterminants_intervention_mean_simulated<-c(logitproportionresistancedeterminants_intervention_mean_simulated,
                                                                  mean(logitproportionresistancedeterminants_simulated))
  #bind to vector of simulated intervention sds
  logitproportionresistancedeterminants_intervention_sd_simulated<-c(logitproportionresistancedeterminants_intervention_sd_simulated,
                                                                sd(logitproportionresistancedeterminants_simulated))
  
  #bind to vector of simulated intervention means
  arcsinproportionresistancedeterminants_intervention_mean_simulated<-c(arcsinproportionresistancedeterminants_intervention_mean_simulated,
                                                                  mean(arcsinproportionresistancedeterminants_simulated))
  #bind to vector of simulated intervention sds
  arcsinproportionresistancedeterminants_intervention_sd_simulated<-c(arcsinproportionresistancedeterminants_intervention_sd_simulated,
                                                                sd(arcsinproportionresistancedeterminants_simulated))
  
}

any(meta_df$intervention_sd_logitpropres<0, na.rm=T)
any(meta_df$control_sd_logitpropres<0, na.rm=T)

#intervention proportion MEANS
meta_df$intervention_mean_logitpropres[missingprevs]<-logitproportionresistancedeterminants_intervention_mean_simulated
#intervention proportion MEANS
meta_df$intervention_mean_arcsinpropres[missingprevs]<-arcsinproportionresistancedeterminants_intervention_mean_simulated

#intervention proportion SDS
meta_df$intervention_sd_logitpropres[missingprevs]<-logitproportionresistancedeterminants_intervention_sd_simulated
#intervention proportion SDS
meta_df$intervention_sd_arcsinpropres[missingprevs]<-arcsinproportionresistancedeterminants_intervention_sd_simulated

#intervention proportion REPS
meta_df$intervention_reps_logitpropres[missingprevs]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_reps[missingprevs]
meta_df$intervention_reps_arcsinpropres[missingprevs]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_reps[missingprevs]

#meta_df$intervention_mean_propres[pico_processed$study_publicationID=='alali2009']

#ESTIMATE CONTROL PROPORTIONS

#create empty vectors of simulated means and standard deviations of proportion of resistance determinants
logitproportionresistancedeterminants_control_mean_simulated<-c()
logitproportionresistancedeterminants_control_sd_simulated<-c()
arcsinproportionresistancedeterminants_control_mean_simulated<-c()
arcsinproportionresistancedeterminants_control_sd_simulated<-c()

for (i in which(missingprevs)){
  #simulate total resistance determinants
  totalresistancedeterminants_simulated<-rnorm(
    mean=pico_processed$outcome_plot_totalresistancedeterminantslog10_control_mean[i],
    sd=pico_processed$outcome_plot_totalresistancedeterminantslog10_control_var[i],
    n=1000
  )
  
  #simulate total determinants                                
  totaldeterminants_simulated<-rnorm(
    mean=pico_processed$outcome_plot_totaldeterminantslog10_control_mean[i],
    sd=pico_processed$outcome_plot_totaldeterminantslog10_control_var[i],
    n=1000
  )
  
  #sort them because it's likely they're correlated
  # totalresistancedeterminants_simulated<-sort(totalresistancedeterminants_simulated)
  # totaldeterminants_simulated<-sort(totaldeterminants_simulated)
  # 
  #calculated simulated proportion
  proportionresistancedeterminants_simulated<-(10^totalresistancedeterminants_simulated)/(10^totaldeterminants_simulated)
  
  #turn all proportions over 1 to 1
  proportionresistancedeterminants_simulated[proportionresistancedeterminants_simulated>1]<-1
  
  logitproportionresistancedeterminants_simulated<-car::logit(proportionresistancedeterminants_simulated, adjust=0.025)
  arcsinproportionresistancedeterminants_simulated<-metafor::transf.arcsin(proportionresistancedeterminants_simulated)
  
  #bind to vector of simulated control means
  logitproportionresistancedeterminants_control_mean_simulated<-c(logitproportionresistancedeterminants_control_mean_simulated,
                                                             mean(logitproportionresistancedeterminants_simulated))
  
  logitproportionresistancedeterminants_control_sd_simulated<-c(logitproportionresistancedeterminants_control_sd_simulated,
                                                           sd(logitproportionresistancedeterminants_simulated))
  
  #bind to vector of simulated control means
  arcsinproportionresistancedeterminants_control_mean_simulated<-c(arcsinproportionresistancedeterminants_control_mean_simulated,
                                                             mean(arcsinproportionresistancedeterminants_simulated))
  
  arcsinproportionresistancedeterminants_control_sd_simulated<-c(arcsinproportionresistancedeterminants_control_sd_simulated,
                                                           sd(arcsinproportionresistancedeterminants_simulated))
  
}

#CONTROL proportion MEANS
meta_df$control_mean_logitpropres[missingprevs]<-logitproportionresistancedeterminants_control_mean_simulated
meta_df$control_mean_arcsinpropres[missingprevs]<-arcsinproportionresistancedeterminants_control_mean_simulated

#CONTROL proportion SDS
meta_df$control_sd_logitpropres[missingprevs]<-logitproportionresistancedeterminants_control_sd_simulated
meta_df$control_sd_arcsinpropres[missingprevs]<-arcsinproportionresistancedeterminants_control_sd_simulated

#CONTROL proportion REPS
meta_df$control_reps_logitpropres[missingprevs]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_control_reps[missingprevs]
meta_df$control_reps_arcsinpropres[missingprevs]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_control_reps[missingprevs]

#set options to make decimals easier to see
options(scipen=1000)

# #coerce all NaNs - 0/0 - sds and means to 0 - these are just a result of dividing 0 by 0 to get proportions so mean and sd is 0
# meta_df$intervention_sd_logitpropres[is.nan(meta_df$intervention_mean_logitpropres)]<-0
# meta_df$intervention_mean_logitpropres[is.nan(meta_df$intervention_mean_logitpropres)]<-0
# 
# meta_df$control_sd_logitpropres[is.nan(meta_df$control_mean_logitpropres)]<-0
# meta_df$control_mean_logitpropres[is.nan(meta_df$control_mean_logitpropres)]<-0

meta_df$intervention_mean_logitpropres[missingprevs]
meta_df$control_mean_arcsinpropres[missingprevs]

#specify where the proportion data came from
origin[missingprevs]<-'simulated'

# todrop<-which(meta_df$intervention_mean<0|meta_df$intervention_mean>1|meta_df$control_mean<0|meta_df$control_mean>1)
# 
# #inspect the ones not in proportion
# meta_df$intervention_mean[todrop]
# meta_df$control_mean[todrop]
# 
# #if there are rows to drop, drop them
# if(length(todrop)!=0){
#   
#   pico_processed<-pico_processed[-todrop,]
#   pico_processed<-pico_processed[-todrop,]
#   meta_df<-meta_df[-todrop,]
#   meta_df<-meta_df[-todrop,]
#   
# }

#assign untransformed propres to new, properly labelled column
#meta_df$intervention_mean_propres<-meta_df$intervention_mean
#meta_df$intervention_sd_propres<-meta_df$intervention_sd
#meta_df$control_mean_propres<-meta_df$control_mean
#meta_df$control_sd_propres<-meta_df$control_sd


# ADD TO DATAFRAME --------------------------------------------------------

## ADD COLUMNS FOR RESISTANCE DETERMINANTS -------------------------------------

#intervention
intervention_mean_totalresistancedeterminants<-pico_processed$outcome_actual_totalresistancedeterminantslog10_intervention_mean  
intervention_mean_totalresistancedeterminants
intervention_mean_totalresistancedeterminants[is.na(intervention_mean_totalresistancedeterminants)]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_mean[is.na(intervention_mean_totalresistancedeterminants)]  
intervention_mean_totalresistancedeterminants

intervention_sd_totalresistancedeterminants<-pico_processed$outcome_actual_totalresistancedeterminantslog10_intervention_sd  
intervention_sd_totalresistancedeterminants
intervention_sd_totalresistancedeterminants[is.na(intervention_sd_totalresistancedeterminants)]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_var[is.na(intervention_sd_totalresistancedeterminants)]  
intervention_sd_totalresistancedeterminants

#check NAs - agga 2016
pico_processed$study_publicationID[which(is.na(intervention_sd_totalresistancedeterminants))]

intervention_reps_totalresistancedeterminants<-pico_processed$outcome_actual_totalresistancedeterminantslog10_intervention_reps  
intervention_reps_totalresistancedeterminants
intervention_reps_totalresistancedeterminants[is.na(intervention_reps_totalresistancedeterminants)]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_reps[is.na(intervention_reps_totalresistancedeterminants)]  
intervention_reps_totalresistancedeterminants

#check why - cos only 1 rep
intervention_reps_totalresistancedeterminants[which(is.na(intervention_sd_totalresistancedeterminants))]

#control
control_mean_totalresistancedeterminants<-pico_processed$outcome_actual_totalresistancedeterminantslog10_control_mean  
control_mean_totalresistancedeterminants
control_mean_totalresistancedeterminants[is.na(control_mean_totalresistancedeterminants)]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_control_mean[is.na(control_mean_totalresistancedeterminants)]  
control_mean_totalresistancedeterminants

#check NAs - agga 2016
pico_processed$study_publicationID[which(is.na(control_mean_totalresistancedeterminants))]

control_sd_totalresistancedeterminants<-pico_processed$outcome_actual_totalresistancedeterminantslog10_control_sd  
control_sd_totalresistancedeterminants
control_sd_totalresistancedeterminants[is.na(control_sd_totalresistancedeterminants)]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_control_var[is.na(control_sd_totalresistancedeterminants)]  
control_sd_totalresistancedeterminants

#check NAs - agga 2016
pico_processed$study_publicationID[which(is.na(control_sd_totalresistancedeterminants))]

control_reps_totalresistancedeterminants<-pico_processed$outcome_actual_totalresistancedeterminantslog10_control_reps  
control_reps_totalresistancedeterminants
control_reps_totalresistancedeterminants[is.na(control_reps_totalresistancedeterminants)]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_control_reps[is.na(control_reps_totalresistancedeterminants)]  
control_reps_totalresistancedeterminants

#check NAs - one n = 1 and other is NA reps so needs investigating
control_reps_totalresistancedeterminants[which(is.na(control_sd_totalresistancedeterminants))]

pico_processed<-
  cbind(pico_processed,
        intervention_mean_totalresistancedeterminants,
        intervention_sd_totalresistancedeterminants,
        intervention_reps_totalresistancedeterminants,
        control_mean_totalresistancedeterminants,
        control_sd_totalresistancedeterminants,
        control_reps_totalresistancedeterminants
  )

## ADD COLUMNS FOR TOTAL DETERMINANTS -------------------------------------

#intervention
intervention_mean_totaldeterminants<-pico_processed$outcome_actual_totaldeterminantslog10_intervention_mean  
intervention_mean_totaldeterminants
intervention_mean_totaldeterminants[is.na(intervention_mean_totaldeterminants)]<-pico_processed$outcome_plot_totaldeterminantslog10_intervention_mean[is.na(intervention_mean_totaldeterminants)]  
intervention_mean_totaldeterminants

intervention_sd_totaldeterminants<-pico_processed$outcome_actual_totaldeterminantslog10_intervention_sd 
intervention_sd_totaldeterminants
intervention_sd_totaldeterminants[is.na(intervention_sd_totaldeterminants)]<-pico_processed$outcome_plot_totaldeterminantslog10_intervention_var[is.na(intervention_sd_totaldeterminants)]  
intervention_sd_totaldeterminants

#query NAs in Ohta and Agga2016 - appears to be lack of reps
pico_processed$study_publicationID[which(is.na(intervention_sd_totaldeterminants))]

intervention_reps_totaldeterminants<-pico_processed$outcome_actual_totaldeterminantslog10_intervention_reps  
intervention_reps_totaldeterminants
intervention_reps_totaldeterminants[is.na(intervention_reps_totaldeterminants)]<-pico_processed$outcome_plot_totaldeterminantslog10_intervention_reps[is.na(intervention_reps_totaldeterminants)]  
intervention_reps_totaldeterminants

#Yes it is lack of reps - investigate why in underlying code
intervention_reps_totaldeterminants[which(is.na(intervention_sd_totaldeterminants))]

#control
control_mean_totaldeterminants<-pico_processed$outcome_actual_totaldeterminantslog10_control_mean  
control_mean_totaldeterminants
control_mean_totaldeterminants[is.na(control_mean_totaldeterminants)]<-pico_processed$outcome_plot_totaldeterminantslog10_control_mean[is.na(control_mean_totaldeterminants)]  
control_mean_totaldeterminants

#query NAs in Ohta and Agga2016
pico_processed$study_publicationID[which(is.na(control_mean_totaldeterminants))]

control_sd_totaldeterminants<-pico_processed$outcome_actual_totaldeterminantslog10_control_sd  
control_sd_totaldeterminants
control_sd_totaldeterminants[is.na(control_sd_totaldeterminants)]<-pico_processed$outcome_plot_totaldeterminantslog10_control_var[is.na(control_sd_totaldeterminants)]  
control_sd_totaldeterminants

#query NAs in Ohta and Agga2016 - appears to be lack of reps
pico_processed$study_publicationID[which(is.na(control_sd_totaldeterminants))]

control_reps_totaldeterminants<-pico_processed$outcome_actual_totaldeterminantslog10_control_reps  
control_reps_totaldeterminants
control_reps_totaldeterminants[is.na(control_reps_totaldeterminants)]<-pico_processed$outcome_plot_totaldeterminantslog10_control_reps[is.na(control_reps_totaldeterminants)]  
control_reps_totaldeterminants

#query NAs in Ohta and Agga2016 - appears to be lack of reps
control_reps_totaldeterminants[which(is.na(control_sd_totaldeterminants))]

pico_processed<-
      cbind(pico_processed,
      intervention_mean_totaldeterminants,
      intervention_sd_totaldeterminants,
      intervention_reps_totaldeterminants,
      control_mean_totaldeterminants,
      control_sd_totaldeterminants,
      control_reps_totaldeterminants
      )

## ADD COLUMNS FOR PROPORTION RESISTANCE DETERMINANTS -------------------------------------

pico_processed<-
  cbind(pico_processed,
        proportiondata_origin = origin,
        intervention_mean_logitpropres = meta_df$intervention_mean_logitpropres,
        intervention_sd_logitpropres = meta_df$intervention_sd_logitpropres,
        intervention_reps_logitpropres = meta_df$intervention_reps_logitpropres,
        intervention_mean_arcsinpropres = meta_df$intervention_mean_arcsinpropres,
        intervention_sd_arcsinpropres = meta_df$intervention_sd_arcsinpropres,
        intervention_reps_arcsinpropres = meta_df$intervention_reps_arcsinpropres,
        control_mean_logitpropres = meta_df$control_mean_logitpropres,
        control_sd_logitpropres = meta_df$control_sd_logitpropres,
        control_reps_logitpropres = meta_df$control_reps_logitpropres,
        control_mean_arcsinpropres = meta_df$control_mean_arcsinpropres,
        control_sd_arcsinpropres = meta_df$control_sd_arcsinpropres,
        control_reps_arcsinpropres = meta_df$control_reps_arcsinpropres
  )


# 9. REMOVE ANY WHERE THE STANDARD DEVIATION IS NA (BECAUSE THESE MUST BE BASED ON ONE SAMPLE)

#pico_processed<-pico_processed[-which(is.na(pico_processed$intervention_sd_propres)|is.na(pico_processed$control_sd_propres)),]

# REMOVE UNWANTED VARIABLES -----------------------------------------------

#droplevels 
pico_processed<-droplevels(pico_processed)

#rm(plotprops)

pico_processed$control_sd_logitpropres[pico_processed$proportiondata_origin=='simulated']

