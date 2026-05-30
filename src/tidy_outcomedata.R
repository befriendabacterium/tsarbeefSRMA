# 1. SOURCE ALL FUNCTIONS ----------------------------------------------------

sapply(list.files('functions', full.names = T, pattern = '.R'), source)

# 2. CONVERT EXTRACTED BELOW ZERO MEANS TO ZERO -------------------------------------------------------------------------
# These are just minor extraction errors, because the minimum value for count/proportion data is 0

extracted_mean_cols<-c(
  "outcome_plot_totaldeterminantslog10_intervention_mean",
  "outcome_plot_totaldeterminantslog10_control_mean",
  "outcome_plot_totalresistancedeterminantslog10_intervention_mean",
  "outcome_plot_totalresistancedeterminantslog10_control_mean",
  "outcome_plot_proportionresistancedeterminants_intervention_mean",
  "outcome_plot_proportionresistancedeterminants_control_mean"
)

#check how many are below zero in each column
colSums(apply(pico_processed[,extracted_mean_cols],2,function(x){x<0}),na.rm=T)

#check how many are extracted
sum(pico_processed$outcome_from_raworextracted=='extracted')

#get column ids of the extracted mean cols
col_ids<-which(colnames(pico_processed)%in%extracted_mean_cols)

#loop to convert all below zeros (excluding NAs) to zero
for (i in col_ids){
  belowzero<-which((pico_processed[,i]<0)==TRUE)
  pico_processed[belowzero,i]<-0
}

#check how many are below zero in each column
colSums(apply(pico_processed[,extracted_mean_cols],2,function(x){x<0}),na.rm=T)

# 3. CALCULATE LENGTH (1/2) OF ERROR BARS (produces _var columns) -------------------------------------------------------------------------

#get varbar length
pico_processed<-calculate_varbarlength(pico_df = pico_processed)

# 4. CALCULATE VARIANCE FROM THE EXTRACTED VARIANCE BAR LIMITS -------------------------------------------------------------------------

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

#convert SE var bar lengths to SDto standard deviation according to varbartype (N.B. all var bar lengths are SE so there is no need for another conversion)
pico_processed[which(pico_processed$outcome_plot_varbartype=='std_error'),]<-convert_se2sd(
                                                                      pico_df = pico_processed[which(pico_processed$outcome_plot_varbartype=='std_error'),], #pico_processed dataframe
                                                                      varbarlength_cols = varbarlength_cols, #vector of var bar length column names
                                                                      replicate_cols = replicate_cols #vector of replicate (n=?) column names
                                                                      )
#check output
pico_processed[,varbarlength_cols]

# 5. TRANSFER IPD-DERIVED PROPORTION DATA TO NEW TEMPORARY DATAFRAME -----------------------

#create a dataframe, filling it with core data the raw resistance proportion means and sds (derived from IPD) to start
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

# 6. CHECK IF ANY PROPORTION MEANS/SDS EXTRACTED FROM PLOTS TO ADD TO THE TEMPORARY DATAFRAME

#vector of extracted proportion columns
extracted_proportion_cols<-c(
                              "outcome_plot_proportionresistancedeterminants_intervention_mean",
                              "outcome_plot_proportionresistancedeterminants_intervention_errorbarlimit",
                              "outcome_plot_proportionresistancedeterminants_control_mean",
                              "outcome_plot_proportionresistancedeterminants_control_errorbarlimit"
                              )

#check if all are NA (i.e. no directly extracted proportions)
all(is.na(pico_processed[,extracted_proportion_cols]))

#they are, so we can move on (there is no data with which to populate the dataframe)

# 6. FILL IN MISSING DATA IN THE META-ANALYSIS DATAFRAME WITH PROPORTIONS FROM DATA SIMULATED FROM MEANS AND SDS OF THE RES AND TOTAL  ----------------------------------

#index of rows with missing data
missingprops<-rowSums(is.na(meta_df[,]))==ncol(meta_df)

#check names of studies with missing prevelance/proportion data
pico_processed$study_publicationID[missingprops]

## SIMULATE INTERVENTION PROPORTIONS AND MEAN AND SDs -----------

#create empty vectors of simulated means and standard deviations of proportion of resistance determinants
logitproportionresistancedeterminants_intervention_mean_simulated<-c()
logitproportionresistancedeterminants_intervention_sd_simulated<-c()
arcsinproportionresistancedeterminants_intervention_mean_simulated<-c()
arcsinproportionresistancedeterminants_intervention_sd_simulated<-c()

for (i in which(missingprops)){
  
  #SIMULATE THE NUMERATOR (ABSOLUTE ABUNDANCE OF RESISTANCE) AND DENOMINATOR (ABSOLUTE ABUNDANCE OF ALL DETERMINANTS) DATA
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
  
  #CALCULATE THE SIMULATED PROPORTION (untransforming in order to do so)
  proportionresistancedeterminants_simulated<-(10^totalresistancedeterminants_simulated)/(10^totaldeterminants_simulated)
  #turn all proportions over 1 to 1
  proportionresistancedeterminants_simulated[proportionresistancedeterminants_simulated>1]<-1
  #logit and arcsine transform them
  logitproportionresistancedeterminants_simulated<-car::logit(proportionresistancedeterminants_simulated, adjust=0.025)
  arcsinproportionresistancedeterminants_simulated<-metafor::transf.arcsin(proportionresistancedeterminants_simulated)
  
  #make an object representing the simulated intervention mean (of logit-transformed data)
  logitproportionresistancedeterminants_intervention_mean_simulated<-mean(logitproportionresistancedeterminants_simulated)
  #make an object representing the simulated intervention sd (of logit-transformed data)
  logitproportionresistancedeterminants_intervention_sd_simulated<-sd(logitproportionresistancedeterminants_simulated)
  
  #make an object representing the simulated intervention mean (of arcsine-transformed data)
  arcsinproportionresistancedeterminants_intervention_mean_simulated<-mean(arcsinproportionresistancedeterminants_simulated)
  #make an object representing the simulated intervention mean (of arcsine-transformed data)
  arcsinproportionresistancedeterminants_intervention_sd_simulated<-sd(arcsinproportionresistancedeterminants_simulated)
  
}

#ADD TO THE TEMPORARY DATAFRAME
#intervention proportion MEANS
meta_df$intervention_mean_logitpropres[missingprops]<-logitproportionresistancedeterminants_intervention_mean_simulated
#intervention proportion MEANS
meta_df$intervention_mean_arcsinpropres[missingprops]<-arcsinproportionresistancedeterminants_intervention_mean_simulated

#intervention proportion SDS
meta_df$intervention_sd_logitpropres[missingprops]<-logitproportionresistancedeterminants_intervention_sd_simulated
#intervention proportion SDS
meta_df$intervention_sd_arcsinpropres[missingprops]<-arcsinproportionresistancedeterminants_intervention_sd_simulated

#intervention proportion REPS
meta_df$intervention_reps_logitpropres[missingprops]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_reps[missingprops]
meta_df$intervention_reps_arcsinpropres[missingprops]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_intervention_reps[missingprops]

## SIMULATE CONTROL PROPORTIONS AND MEAN AND SDs -----------

#create empty vectors of simulated means and standard deviations of proportion of resistance determinants
logitproportionresistancedeterminants_control_mean_simulated<-c()
logitproportionresistancedeterminants_control_sd_simulated<-c()
arcsinproportionresistancedeterminants_control_mean_simulated<-c()
arcsinproportionresistancedeterminants_control_sd_simulated<-c()

for (i in which(missingprops)){
  
  #SIMULATE THE NUMERATOR (ABSOLUTE ABUNDANCE OF RESISTANCE) AND DENOMINATOR (ABSOLUTE ABUNDANCE OF ALL DETERMINANTS) DATA
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
  
  #CALCULATE THE SIMULATED PROPORTION (untransforming in order to do so)
  proportionresistancedeterminants_simulated<-(10^totalresistancedeterminants_simulated)/(10^totaldeterminants_simulated)
  #turn all proportions over 1 to 1
  proportionresistancedeterminants_simulated[proportionresistancedeterminants_simulated>1]<-1
  #logit and arcsine transform them
  logitproportionresistancedeterminants_simulated<-car::logit(proportionresistancedeterminants_simulated, adjust=0.025)
  arcsinproportionresistancedeterminants_simulated<-metafor::transf.arcsin(proportionresistancedeterminants_simulated)
  
  #make an object representing the simulated intervention mean (of logit-transformed data)
  logitproportionresistancedeterminants_control_mean_simulated<-mean(logitproportionresistancedeterminants_simulated)
  #make an object representing the simulated intervention SD (of logit-transformed data)
  logitproportionresistancedeterminants_control_sd_simulated<-sd(logitproportionresistancedeterminants_simulated)
  
  #make an object representing the simulated intervention mean (of arcsine-transformed data)
  arcsinproportionresistancedeterminants_control_mean_simulated<-mean(arcsinproportionresistancedeterminants_simulated)
  #make an object representing the simulated intervention SD (of logit-transformed data)
  arcsinproportionresistancedeterminants_control_sd_simulated<-c(arcsinproportionresistancedeterminants_control_sd_simulated,
                                                           sd(arcsinproportionresistancedeterminants_simulated))
  
}

#ADD TO THE TEMPORARY DATAFRAME
#CONTROL proportion MEANS
meta_df$control_mean_logitpropres[missingprops]<-logitproportionresistancedeterminants_control_mean_simulated
meta_df$control_mean_arcsinpropres[missingprops]<-arcsinproportionresistancedeterminants_control_mean_simulated

#CONTROL proportion SDS
meta_df$control_sd_logitpropres[missingprops]<-logitproportionresistancedeterminants_control_sd_simulated
meta_df$control_sd_arcsinpropres[missingprops]<-arcsinproportionresistancedeterminants_control_sd_simulated

#CONTROL proportion REPS
meta_df$control_reps_logitpropres[missingprops]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_control_reps[missingprops]
meta_df$control_reps_arcsinpropres[missingprops]<-pico_processed$outcome_plot_totalresistancedeterminantslog10_control_reps[missingprops]

#set options to make decimals easier to see
options(scipen=1000)

#specify where the proportion data came from
origin[missingprops]<-'simulated'

## ADD TO MAIN DATAFRAME --------------------------------------------------------

### ADD COLUMNS FOR RESISTANCE DETERMINANTS TO MAIN DATAFRAME -------------------------------------

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

#check why agga was NA - cos only 1 rep (will be removed later)
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

#add to main dataframe
pico_processed<-
  cbind(pico_processed,
        intervention_mean_totalresistancedeterminants,
        intervention_sd_totalresistancedeterminants,
        intervention_reps_totalresistancedeterminants,
        control_mean_totalresistancedeterminants,
        control_sd_totalresistancedeterminants,
        control_reps_totalresistancedeterminants
  )

### ADD COLUMNS FOR TOTAL DETERMINANTS TO MAIN DATAFRAME ------------------------------------

#intervention
intervention_mean_totaldeterminants<-pico_processed$outcome_actual_totaldeterminantslog10_intervention_mean  
intervention_mean_totaldeterminants
intervention_mean_totaldeterminants[is.na(intervention_mean_totaldeterminants)]<-pico_processed$outcome_plot_totaldeterminantslog10_intervention_mean[is.na(intervention_mean_totaldeterminants)]  
intervention_mean_totaldeterminants

intervention_sd_totaldeterminants<-pico_processed$outcome_actual_totaldeterminantslog10_intervention_sd 
intervention_sd_totaldeterminants
intervention_sd_totaldeterminants[is.na(intervention_sd_totaldeterminants)]<-pico_processed$outcome_plot_totaldeterminantslog10_intervention_var[is.na(intervention_sd_totaldeterminants)]  
intervention_sd_totaldeterminants

#query NAs in Agga2016 - appears to be lack of reps
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

#query NAs in Agga2016 - appears to be lack of reps
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

### ADD COLUMNS FOR PROPORTION RESISTANCE DETERMINANTS TO MAIN DATAFRAME -------------------------------------

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


# 9. REMOVE UNWANTED VARIABLES (DROP LEVELS) -----------------------------------------------

#droplevels 
pico_processed<-droplevels(pico_processed)