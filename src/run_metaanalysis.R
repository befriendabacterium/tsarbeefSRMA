# GET LEGENDS -----------------------------------------------------

#extract legends for combined plots
source('src/plot_legends.R')

# HYPOTHESIS 1 ANALYSIS: CHECK FOR HETEROGENEITY --------------------------------------------------------

#run hypothesis 1 models to check for heterogeneity in the injection, feed_during and feed_after subsets
source('src/runmodels_plotorchards_H1.R')

#run models
source('src/plot_profilelikelihoodplots_H1.R')

# HYPOTHESIS 2 ANALYSIS: TRY TO EXPLAIN HETEROGENEITY WITH MODELS--------------------------------------------------------

#make models
source('src/runmodels_plotbubbles_H2.R')

#run models
source('src/plot_profilelikelihoodplots_H2.R')

 # SENSITIVITY ANALYSIS ----------------------------------------------------

source('src/run_sensitivity_leave1out.R')
source('src/run_sensitivity_removestudies.R')
source('src/run_sensitivity_SCNP.R')
source('src/run_sensitivity_varcovar.R')

source('src/plot_orchardplots_H1_sensitivity.R')
source('src/plot_bubbleplots_H2_sensitivity.R')

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))

# ALTERNATIVE AND SUBGROUP ANALYSIS -------------------------------------------------------------

#show the studies where unit of analysis is pen
unique(pico_witheffectsizes_post_faeces$study_studyID[pico_witheffectsizes_post_faeces$outcome_unitofanalysis=='animal'])

#relevel antibiotic order
pico_witheffectsizes_post_faeces$intervention_antibiotic_class<-factor(pico_witheffectsizes_post_faeces$intervention_antibiotic_class, levels=c('sulfonamide',
                                                                                'tetracycline',
                                                                                'Macrolides, lincosamides, \n & streptogramin B',
                                                                                'extended-spectrum \n cephalosporin'))
source('src/run_models_subgroup.R')
source('src/plot_orchardplots_subgroup.R')

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))

source('src/run_dose_models.R')
source('src/plot_bubbleplots_dose.R')

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))
 
# SUB-SUBGROUP ANALYSES ---------------------------------------------------

source('src/run_CFUantibiotics_model.R')
source('src/plot_orchardplots_CFUantibiotics.R')

source('src/run_CFUbreakpoints_models.R')
source('src/plot_orchardplots_CFUbreakpoints.R')

source('src/run_qPCRgenes_model.R')
source('src/plot_orchardplots_qPCRgenes.R')

# COPY OVER FINAL OUTCOMES TO FINAL FOLDER -------------------------------------------------------------

#during
file.copy('3_models/hypothesis_1/H1_model_during_totalresistancedeterminants.RDS',to = '3_models/final/Outcome1_post_during_resdensity_model.RDS',overwrite = T)

#during (time effect)
file.copy('3_models/hypothesis_2/H2_model_during_totalresistancedeterminants_timelinear.RDS',to = '3_models/final/Outcome2_post_duringtime_resdensity_model.RDS',overwrite = T)

#after
file.copy('3_models/hypothesis_1/H1_model_after_totalresistancedeterminants.RDS',to = '3_models/final/Outcome3_post_after_resdensity_model.RDS',overwrite = T)

#after (time effect)
file.copy('3_models/hypothesis_2/H2_model_after_totalresistancedeterminants_timelinear.RDS',to = '3_models/final/Outcome4_post_aftertime_resdensity_model.RDS',overwrite = T)

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))

# PUBLICATION BIAS -------------------------------------------------------------

source('src/testfor_publicationbias_outcomes.R')

# END ---------------------------------------------------------------------
