# LOAD CUSTOM FUNCTIONS ---------------------------------------------------

# 1. READ IN PICO DATAFRAME ---------------------------------------------------

#read in pico_processed data
pico_processed<-readRDS('2_processeddata/pico_processed.RDS')

#add id column
pico_processed<-tibble::add_column(pico_processed,id=1:nrow(pico_processed), .before=1)

# 4. CALCULATE EFFECT SIZE (TOTAL RESISTANCE DETERMINANTS) --------------------------------------------------

#SMDH effect size calc: Standardised mean difference between intervention and control
effectsizes_totalresistancedeterminants<-metafor::escalc(measure="SMDH",
                                                   data = pico_processed,
                                                   m1i = intervention_mean_totalresistancedeterminants,
                                                   m2i = control_mean_totalresistancedeterminants,
                                                   sd1i = intervention_sd_totalresistancedeterminants,
                                                   sd2i = control_sd_totalresistancedeterminants,
                                                   n1i = intervention_reps_totalresistancedeterminants,
                                                   n2i = control_reps_totalresistancedeterminants,
                                                   append=T)

#make the ni column out of the attributes variable
effectsizes_totalresistancedeterminants$ni_totalresistancedeterminants<-attr(effectsizes_totalresistancedeterminants$yi, which="ni")

#customise colnames to include the name of the outcome
effectsizes_totalresistancedeterminants<-dplyr::rename(effectsizes_totalresistancedeterminants,
                                yi_totalresistancedeterminants=yi,
                                vi_totalresistancedeterminants=vi)

# 5. CALCULATE EFFECT SIZE (TOTAL DETERMINANTS) --------------------------------------------------

#SMDH effect size calc: Standardised mean difference between intervention and control
effectsizes_totaldeterminants<-metafor::escalc(measure="SMDH",
                                 data = pico_processed,
                                 m1i = intervention_mean_totaldeterminants,
                                 m2i = control_mean_totaldeterminants,
                                 sd1i = intervention_sd_totaldeterminants,
                                 sd2i = control_sd_totaldeterminants,
                                 n1i = intervention_reps_totaldeterminants,
                                 n2i = control_reps_totaldeterminants,
                                 append=T)

#make the ni column out of the attributes variable
effectsizes_totaldeterminants$ni_totaldeterminants<-attr(effectsizes_totaldeterminants$yi, which="ni")

#customise colnames to include the name of the outcome
effectsizes_totaldeterminants<-dplyr::rename(effectsizes_totaldeterminants,
                                yi_totaldeterminants=yi,
                                vi_totaldeterminants=vi)

# 3A. CALCULATE EFFECT SIZE (LOGIT PROPORTION RESISTANT) --------------------------------------------------

#SMDH effect size calc: Standardised mean difference between intervention and control
effectsizes_logitpropres<-metafor::escalc(measure="SMDH",
                                     data = pico_processed,
                                     m1i = intervention_mean_logitpropres,
                                     m2i = control_mean_logitpropres,
                                     sd1i = intervention_sd_logitpropres,
                                     sd2i = control_sd_logitpropres,
                                     n1i = intervention_reps_logitpropres,
                                     n2i = control_reps_logitpropres,
                                     append=T)

#make the ni column out of the attributes variable
effectsizes_logitpropres$ni_logitpropres<-attr(effectsizes_logitpropres$yi, which="ni")

#customise colnames to include the name of the outcome
effectsizes_logitpropres<-dplyr::rename(effectsizes_logitpropres,
                                   yi_logitpropres=yi,
                                   vi_logitpropres=vi)

# 3B. CALCULATE EFFECT SIZE (arcsin PROPORTION RESISTANT) --------------------------------------------------

#SMDH effect size calc: Standardised mean difference between intervention and control
effectsizes_arcsinpropres<-metafor::escalc(measure="SMDH",
                                          data = pico_processed,
                                          m1i = intervention_mean_arcsinpropres,
                                          m2i = control_mean_arcsinpropres,
                                          sd1i = intervention_sd_arcsinpropres,
                                          sd2i = control_sd_arcsinpropres,
                                          n1i = intervention_reps_arcsinpropres,
                                          n2i = control_reps_arcsinpropres,
                                          append=T)

#make the ni column out of the attributes variable
effectsizes_arcsinpropres$ni_arcsinpropres<-attr(effectsizes_arcsinpropres$yi, which="ni")

#customise colnames to include the name of the outcome
effectsizes_arcsinpropres<-dplyr::rename(effectsizes_arcsinpropres,
                                        yi_arcsinpropres=yi,
                                        vi_arcsinpropres=vi)

# COMBINE -----------------------------------------------------------------

effectsizes_dfs<-list(effectsizes_totalresistancedeterminants,
                      effectsizes_totaldeterminants[,c("id","yi_totaldeterminants","vi_totaldeterminants", "ni_totaldeterminants")],
                      effectsizes_logitpropres[,c("id","yi_logitpropres","vi_logitpropres", "ni_logitpropres")],
                      effectsizes_arcsinpropres[,c("id","yi_arcsinpropres","vi_arcsinpropres", "ni_arcsinpropres")]
)

pico_witheffectsizes<-effectsizes_dfs %>% purrr::reduce(full_join, 
                                         by='id')

colnames(pico_witheffectsizes)

# CLEAN UP -----------------------------------------------------------------

#remove all objects bar the dataframe we need for rest of analysis
rm(list=setdiff(ls(), "pico_witheffectsizes"))
