# LOAD FUNCTIONS ----------------------------------------------------------

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# GET LEGENDS -----------------------------------------------------

#extract legends for combined plots
source('3_metaanalysis/src/plot_legends.R')

# HYPOTHESIS 0 (study level) ------------------------------------------------------------

#run models
source('3_metaanalysis/src/runmodels_plotforests_H0.R')

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))

# HYPOTHESIS 1 ANALYSIS: CHECK FOR HETEROGENEITY --------------------------------------------------------

#run hypothesis 1 models to check for heterogeneity in the injection, feed_during and feed_after subsets
source('3_metaanalysis/src/runmodels_plotorchards_H1.R')

#run models
source('3_metaanalysis/src/plot_profilelikelihoodplots_H1.R')

# HYPOTHESIS 2 ANALYSIS: TRY TO EXPLAIN HETEROGENEITY WITH MODELS--------------------------------------------------------

#make models
source('3_metaanalysis/src/runmodels_plotbubbles_H2.R')

#run models
source('3_metaanalysis/src/plot_profilelikelihoodplots_H2.R')

 # SENSITIVITY ANALYSIS ----------------------------------------------------

source('3_metaanalysis/src/run_sensitivity_leave1out.R')
source('3_metaanalysis/src/run_sensitivity_removestudies.R')
source('3_metaanalysis/src/run_sensitivity_SCNP.R')
source('3_metaanalysis/src/run_sensitivity_varcovar.R')

source('3_metaanalysis/src/plot_orchardplots_H1_sensitivity.R')
source('3_metaanalysis/src/plot_bubbleplots_H2_sensitivity.R')

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))

# ALTERNATIVE AND SUBGROUP ANALYSIS -------------------------------------------------------------

#temporary workaround change units of analysis for three studies
unique(pico_witheffectsizes_post_faeces$study_studyID[pico_witheffectsizes_post_faeces$outcome_unitofanalysis=='animal'])
pico_witheffectsizes_post_faeces$outcome_unitofanalysis[pico_witheffectsizes_post_faeces$study_studyID%in%c('Lethbridge_Tyl','TexasA&M_Cef')]<-'pen'

#relevel antibiotic order
pico_witheffectsizes_post_faeces$intervention_antibiotic_class<-factor(pico_witheffectsizes_post_faeces$intervention_antibiotic_class, levels=c('sulfonamide',
                                                                                'tetracycline',
                                                                                'Macrolides, lincosamides, \n & streptogramin B',
                                                                                'extended-spectrum \n cephalosporin'))
source('3_metaanalysis/src/run_models_subgroup.R')
source('3_metaanalysis/src/plot_orchardplots_subgroup.R')

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))

source('3_metaanalysis/src/run_dose_models.R')
source('3_metaanalysis/src/plot_bubbleplots_dose.R')

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))
 
# SUB-SUBGROUP ANALYSES ---------------------------------------------------

source('3_metaanalysis/src/run_CFUantibiotics_model.R')
source('3_metaanalysis/src/plot_orchardplots_CFUantibiotics.R')

source('3_metaanalysis/src/run_CFUbreakpoints_models.R')
source('3_metaanalysis/src/plot_orchardplots_CFUbreakpoints.R')

source('3_metaanalysis/src/run_qPCRgenes_model.R')
source('3_metaanalysis/src/plot_orchardplots_qPCRgenes.R')

# COPY OVER FINAL OUTCOMES TO FINAL FOLDER -------------------------------------------------------------

#during
file.copy('3_metaanalysis/3_models/hypothesis_1/H1_model_during_totalresistancedeterminants.RDS',to = '3_metaanalysis/3_models/final/Outcome1_post_during_resdensity_model.RDS',overwrite = T)

#during (time effect)
file.copy('3_metaanalysis/3_models/hypothesis_2/H2_model_during_totalresistancedeterminants_timelinear.RDS',to = '3_metaanalysis/3_models/final/Outcome2_post_duringtime_resdensity_model.RDS',overwrite = T)

#after
file.copy('3_metaanalysis/3_models/hypothesis_1/H1_model_after_totalresistancedeterminants.RDS',to = '3_metaanalysis/3_models/final/Outcome3_post_after_resdensity_model.RDS',overwrite = T)

#after (time effect)
file.copy('3_metaanalysis/3_models/hypothesis_2/H2_model_after_totalresistancedeterminants_timelinear.RDS',to = '3_metaanalysis/3_models/final/Outcome4_post_aftertime_resdensity_model.RDS',overwrite = T)

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))

# PUBLICATION BIAS -------------------------------------------------------------

source('3_metaanalysis/src/testfor_publicationbias_outcomes.R')

# SUMMARY OF FINDINGS -----------------------------------------------------

#input file names
final_resdensity_models_files<-normalizePath(list.files('3_metaanalysis/3_models/final/', pattern ='resdensity_model.RDS', full.names = T))

final_resdensity_models_files

## Outcome 1 (During H1) ---------------------------------------------------

Outcome1_model<-readRDS(final_resdensity_models_files[1])
Outcome1_model$b
predict(Outcome1_model)
Outcome1_data<-Outcome1_model$data
Outcome1_data$diffs_pooled_individual<-abs(Outcome1_model$b[1]-Outcome1_data$yi_totalresistancedeterminants)

10^min(Outcome1_data$outcome_time_days)-1
10^median(Outcome1_data$outcome_time_days)-1
10^max(Outcome1_data$outcome_time_days)-1

Outcome1_data$study_publicationID[Outcome1_data$outcome_timesinceintervention_start_days>300]


#10^max(Outcome1_data$outcome_timesinceintervention_start_days) - CHECK

minslice<-dplyr::slice_min(Outcome1_data, order_by = diffs_pooled_individual, n = 20)
minslice$diffs_pooled_individual
minslice$id
es_id<-75

minslice$outcome_resistance_target[minslice$id==es_id]
minslice$outcome_resistance_studybreakpointmgL[minslice$id==es_id]
minslice$outcome_organism[minslice$id==es_id]

minslice$intervention_antibiotic_name[minslice$id==es_id]
minslice$intervention_dosage_value[minslice$id==es_id]
minslice$intervention_dosage_unit[minslice$id==es_id]
minslice$intervention_perdayUDD_mgkg[minslice$id==es_id]

10^minslice$outcome_time_days[minslice$id==es_id]-1
minslice$yi_totalresistancedeterminants[minslice$id==es_id]
minslice$control_mean_totalresistancedeterminants[minslice$id==es_id]
minslice$intervention_mean_totalresistancedeterminants[minslice$id==es_id]
minslice$study_publicationID[minslice$id==es_id]

Outcome1_studydesigns_tally<-plyr::count(tapply(Outcome1_data$study_studydesign, 
                                                  Outcome1_data$study_studyID, 
                                                  function(x){unique(as.character(x))}))
Outcome1_studydesigns_tally

## Outcome 2 (During Time) -------------------------------------------------

Outcome2_model<-readRDS(final_resdensity_models_files[2])
Outcome2_data<-Outcome2_model$data

slopes<-c()

for (i in unique(Outcome2_data$cluster_intervoutcome)){
  print(i)
  dat<-Outcome2_data[Outcome2_data$cluster_intervoutcome==i,]
  mod<-lm(yi_totalresistancedeterminants~outcome_time_days, data=dat)
  slopes<-c(slopes,mod$coefficients[2])
}

slopes

names(slopes)<-unique(Outcome2_data$cluster_intervoutcome)

slopediffs<-abs(Outcome2_model$b[2]-slopes)
slopediffs<-sort(slopediffs)
es_id<-names(slopediffs)[1]

es_id
minslice<-Outcome2_data

data<-Outcome2_data[Outcome2_data$cluster_intervoutcome==es_id,]
plot(data$yi_totalresistancedeterminants~data$outcome_time_days)


minslice$slope[minslice$cluster_intervoutcome==es_id]
minslice$study_publicationID[minslice$cluster_intervoutcome==es_id]

minslice$outcome_resistance_target[minslice$cluster_intervoutcome==es_id]
minslice$outcome_resistance_studybreakpointmgL[minslice$cluster_intervoutcome==es_id]
minslice$outcome_organism[minslice$cluster_intervoutcome==es_id]

minslice$intervention_antibiotic_name[minslice$cluster_intervoutcome==es_id]
minslice$intervention_dosage_value[minslice$cluster_intervoutcome==es_id]
minslice$intervention_dosage_unit[minslice$cluster_intervoutcome==es_id]
minslice$intervention_perdayUDD_mgkg[minslice$cluster_intervoutcome==es_id]

10^minslice$outcome_time_days[minslice$cluster_intervoutcome==es_id]-1

slopes[which(names(slopes)==es_id)]minslice$yi_totalresistancedeterminants[minslice$cluster_intervoutcome==es_id]
minslice$control_mean_totalresistancedeterminants[minslice$cluster_intervoutcome==es_id]
minslice$intervention_mean_totalresistancedeterminants[minslice$cluster_intervoutcome==es_id]

Outcome2_studydesigns_tally<-plyr::count(tapply(Outcome2_data$study_studydesign, 
                                                Outcome2_data$study_studyID, 
                                                function(x){unique(as.character(x))}))
Outcome2_studydesigns_tally

## Outcome 3 (After H1) ---------------------------------------------------

Outcome3_model<-readRDS(final_resdensity_models_files[3])
Outcome3_model$b
predict(Outcome3_model)
Outcome3_data<-Outcome3_model$data
Outcome3_data$diffs_pooled_individual<-abs(Outcome3_model$b[1]-Outcome3_data$yi_totalresistancedeterminants)

10^min(Outcome3_data$outcome_time_days)-1
10^median(Outcome3_data$outcome_time_days)-1
10^max(Outcome3_data$outcome_time_days)-1

Outcome3_data$study_publicationID[Outcome3_data$outcome_timesinceintervention_start_days>300]

#10^max(Outcome3_data$outcome_timesinceintervention_start_days) - CHECK

minslice<-dplyr::slice_min(Outcome3_data, order_by = diffs_pooled_individual, n = 20)
minslice$id
minslice$yi_totalresistancedeterminants
es_id<-192
minslice$yi_totalresistancedeterminants[minslice$id==es_id]
10^minslice$outcome_time_days[minslice$id==es_id]-1

minslice$outcome_resistance_target[minslice$id==es_id]
minslice$outcome_resistance_studybreakpointmgL[minslice$id==es_id]
minslice$outcome_organism[minslice$id==es_id]

minslice$intervention_antibiotic_name[minslice$id==es_id]
minslice$intervention_dosage_value[minslice$id==es_id]
minslice$intervention_dosage_unit[minslice$id==es_id]

minslice$yi_totalresistancedeterminants[minslice$id==es_id]
minslice$control_mean_totalresistancedeterminants[minslice$id==es_id]
minslice$intervention_mean_totalresistancedeterminants[minslice$id==es_id]
minslice$study_publicationID[minslice$id==es_id]


Outcome3_studydesigns_tally<-plyr::count(tapply(Outcome3_data$study_studydesign, 
                                                Outcome3_data$study_studyID, 
                                                function(x){unique(as.character(x))}))
Outcome3_studydesigns_tally

## Outcome 4 (After Time) -------------------------------------------------

Outcome4_model<-readRDS(final_resdensity_models_files[4])
Outcome4_data<-Outcome4_model$data

slopes<-c()

for (i in unique(Outcome4_data$cluster_intervoutcome)){
  print(i)
  dat<-Outcome4_data[Outcome4_data$cluster_intervoutcome==i,]
  mod<-lm(yi_totalresistancedeterminants~outcome_time_days, data=dat)
  slopes<-c(slopes,mod$coefficients[2])
}

slopes

names(slopes)<-unique(Outcome4_data$cluster_intervoutcome)

slopediffs<-abs(Outcome4_model$b[2]-slopes)
slopediffs<-sort(slopediffs)
slopediffs
es_id<-names(slopediffs)[1]

es_id
minslice<-Outcome4_data

data<-Outcome4_data[Outcome4_data$cluster_intervoutcome==es_id,]
plot(data$yi_totalresistancedeterminants~data$outcome_time_days)
slopes[which(names(slopes)==es_id)]


minslice$study_publicationID[minslice$cluster_intervoutcome==es_id]

minslice$outcome_resistance_target[minslice$cluster_intervoutcome==es_id]
minslice$outcome_resistance_studybreakpointmgL[minslice$cluster_intervoutcome==es_id]
minslice$outcome_organism[minslice$cluster_intervoutcome==es_id]

minslice$intervention_antibiotic_name[minslice$cluster_intervoutcome==es_id]
minslice$intervention_dosage_value[minslice$cluster_intervoutcome==es_id]
minslice$intervention_dosage_unit[minslice$cluster_intervoutcome==es_id]
minslice$intervention_perdayUDD_mgkg[minslice$cluster_intervoutcome==es_id]

10^minslice$outcome_time_days[minslice$cluster_intervoutcome==es_id]-1

slopes[which(names(slopes)==es_id)]
minslice$yi_totalresistancedeterminants[minslice$cluster_intervoutcome==es_id]
minslice$control_mean_totalresistancedeterminants[minslice$cluster_intervoutcome==es_id]
minslice$intervention_mean_totalresistancedeterminants[minslice$cluster_intervoutcome==es_id]

Outcome4_studydesigns_tally<-plyr::count(tapply(Outcome4_data$study_studydesign, 
                                                Outcome4_data$study_studyID, 
                                                function(x){unique(as.character(x))}))
Outcome4_studydesigns_tally









### 3 days -------------------------------------------------

Outcome4_model<-readRDS(final_resdensity_models_files[4])
Outcome4_data<-Outcome4_model$data

data<-Outcome4_data
mod<-'outcome_time_days'
by=NULL

#generate x values (continuous x values) from which to predict y values
#xs <- seq(0.1, 2, 0.1)
xs <- seq(min(data[,mod], na.rm = TRUE), max(data[,mod], na.rm = TRUE), length.out = 100)

#if there is no conditioning variable...
if(is.null(by)){
  
  #initiate dataframe with x values and a blank 'by' column
  newgrid<-data.frame(xs=xs,by=NA)
  
  #if there is a conditioning variable...
} else{
  
  #initiate dataframe by generating x values for all levels of factors using same term names in same order as in model formula
  newgrid <- data.frame(expand.grid(xs=xs,by=levels(as.factor(data[,by]))))
  
}

#rename columns with the moderator and by variables
colnames(newgrid)<-c(mod,by)

#list other variables in the model
othervars<-names(coef(model))[-1][grep(mod,names(coef(model))[-1], invert = T)]

#if there are other variables, set to mean and add to 'newgrid' prediction matrix
if(length(othervars)!=0){
  newgrid[,othervars]<-colMeans(as.data.frame(data[,othervars], na.rm = T))
}

#create the new model matrix and remove the intercept
predgrid<-model.matrix(model$formula.mods,data=newgrid)[,-1]

#predict onto the new model matrix
mypreds <- as.data.frame(predict.rma(model, newmods=predgrid))


#make mod_table
mod_table <- data.frame(moderator = newgrid[,mod],
                        condition = newgrid[,by],
                        estimate = mypreds$pred,
                        lowerCL = mypreds$ci.lb,
                        upperCL = mypreds$ci.ub,
                        lowerPR = mypreds$pi.lb,
                        upperPR = mypreds$pi.ub)

mod_table


preds<-mod_table[1,]
preds

Outcome4_data$diffs_pooled_individual<-abs(preds$estimate[1]-Outcome4_data$yi_totalresistancedeterminants)

minslice<-dplyr::slice_min(Outcome4_data, order_by = diffs_pooled_individual, n = 20)
minslice$id
10^minslice$outcome_time_days

es_id<-61
minslice$study_publicationID[minslice$id==es_id]
minslice$yi_totalresistancedeterminants[minslice$id==es_id]
10^minslice$outcome_time_days[minslice$id==es_id]
minslice$outcome_resistance_target[minslice$id==es_id]
minslice$outcome_resistance_studybreakpointmgL[minslice$id==es_id]
minslice$outcome_organism[minslice$id==es_id]
minslice$intervention_antibiotic_class[minslice$id==es_id]
minslice$intervention_dosage_value[minslice$id==es_id]
minslice$intervention_dosage_unit[minslice$id==es_id]
minslice$cointervention1_antibiotic_class[minslice$id==es_id]
minslice$yi_totalresistancedeterminants[minslice$id==es_id]
minslice$control_mean_totalresistancedeterminants[minslice$id==es_id]
minslice$intervention_mean_totalresistancedeterminants[minslice$id==es_id]


Outcome4_studydesigns_tally<-plyr::count(tapply(Outcome4_data$study_studydesign, 
                                                Outcome4_data$study_studyID, 
                                                function(x){unique(as.character(x))}))
Outcome4_studydesigns_tally

### 100 days -------------------------------------------------

Outcome4_model<-readRDS(final_resdensity_models_files[4])
Outcome4_data<-Outcome4_model$data


data<-Outcome4_data
mod<-'outcome_time_days'
by=NULL

#generate x values (continuous x values) from which to predict y values
#xs <- seq(0.1, 2, 0.1)
xs <- seq(min(data[,mod], na.rm = TRUE), max(data[,mod], na.rm = TRUE), length.out = 100)

#if there is no conditioning variable...
if(is.null(by)){
  
  #initiate dataframe with x values and a blank 'by' column
  newgrid<-data.frame(xs=xs,by=NA)
  
  #if there is a conditioning variable...
} else{
  
  #initiate dataframe by generating x values for all levels of factors using same term names in same order as in model formula
  newgrid <- data.frame(expand.grid(xs=xs,by=levels(as.factor(data[,by]))))
  
}

#rename columns with the moderator and by variables
colnames(newgrid)<-c(mod,by)

#list other variables in the model
othervars<-names(coef(model))[-1][grep(mod,names(coef(model))[-1], invert = T)]

#if there are other variables, set to mean and add to 'newgrid' prediction matrix
if(length(othervars)!=0){
  newgrid[,othervars]<-colMeans(as.data.frame(data[,othervars], na.rm = T))
}

#create the new model matrix and remove the intercept
predgrid<-model.matrix(model$formula.mods,data=newgrid)[,-1]

#predict onto the new model matrix
mypreds <- as.data.frame(predict.rma(model, newmods=predgrid))


#make mod_table
mod_table <- data.frame(moderator = newgrid[,mod],
                        condition = newgrid[,by],
                        estimate = mypreds$pred,
                        lowerCL = mypreds$ci.lb,
                        upperCL = mypreds$ci.ub,
                        lowerPR = mypreds$pi.lb,
                        upperPR = mypreds$pi.ub)

mod_table

log10(100)
preds<-mod_table[95,]
preds

Outcome4_data$diffs_pooled_individual<-abs(preds$estimate[1]-Outcome4_data$yi_totalresistancedeterminants)

minslice<-dplyr::slice_min(Outcome4_data, order_by = diffs_pooled_individual, n = 20)
minslice$id
10^minslice$outcome_time_days

es_id<-99
minslice$study_publicationID[minslice$id==es_id]
minslice$yi_totalresistancedeterminants[minslice$id==es_id]
10^minslice$outcome_time_days[minslice$id==es_id]
minslice$outcome_resistance_target[minslice$id==es_id]
minslice$outcome_resistance_studybreakpointmgL[minslice$id==es_id]
minslice$outcome_organism[minslice$id==es_id]
minslice$intervention_antibiotic_name[minslice$id==es_id]
minslice$intervention_dosage_value[minslice$id==es_id]
minslice$intervention_dosage_unit[minslice$id==es_id]
minslice$yi_totalresistancedeterminants[minslice$id==es_id]
minslice$control_mean_totalresistancedeterminants[minslice$id==es_id]
minslice$intervention_mean_totalresistancedeterminants[minslice$id==es_id]


Outcome4_studydesigns_tally<-plyr::count(tapply(Outcome4_data$study_studydesign, 
                                                Outcome4_data$study_studyID, 
                                                function(x){unique(as.character(x))}))
Outcome4_studydesigns_tally


# END ---------------------------------------------------------------------
