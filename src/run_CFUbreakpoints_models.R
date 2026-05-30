#sub-subgroup analysis: CFU
#make CFU-only dataset
pico_witheffectsizes_post_faeces_CFUonly<-pico_witheffectsizes_post_faeces[pico_witheffectsizes_post_faeces$outcome_measurement_type=='Direct plating \n on agar',]

#rename
#pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_target<-plyr::revalue(pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_target,c("ery8"="ery",'ery128'='ery'))

#drop levels
pico_witheffectsizes_post_faeces_CFUonly<-droplevels(pico_witheffectsizes_post_faeces_CFUonly)

# STUDY BREAKPOINT (ABSOLUTE) ---------------------------------------------

#log study breakpoints
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointmgL<-log10(pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointmgL)

## RUN AN INTERCEPT-ONLY MODEL FOR ALL DATA (H0) -----------------

model_CFUstudybreakpoint_H0 <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                   random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                   struct = c('CAR'),
                                   data = pico_witheffectsizes_post_faeces_CFUonly,
                                   control=list(rel.tol=1e-8))


## RUN A MODEL WITH RESISTANCE TARGET AS MODERATOR -----------------

model_CFUstudybreakpoint <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                              mod = ~ outcome_resistance_studybreakpointmgL,
                              random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                              struct = c('CAR'),
                              data = pico_witheffectsizes_post_faeces_CFUonly,
                              control=list(rel.tol=1e-8))
#refit model
tryCatch(
model_CFUstudybreakpoint_noranf <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                   mod = ~ outcome_resistance_studybreakpointmgL,
                                   #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                   #struct = c('CAR'),
                                   data = pico_witheffectsizes_post_faeces_CFUonly,
                                   control=list(rel.tol=1e-8))
,

#if error, 
error = function(e){
  print('Could not compute model - skipped')
}
)

tryCatch(
  #use Jackson approach to calculate I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
  model_CFUstudybreakpoint_I2<-c(100 * (vcov(model_CFUstudybreakpoint)[1,1] - vcov(model_CFUstudybreakpoint_noranf)[1,1]) / vcov(model_CFUstudybreakpoint)[1,1])
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })


tryCatch(
  #use Wolfgang's approach to calculate pseudo-R2 of 3 variance components (2 sigmas and tau) - https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-October/000247.html; https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor; https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
  model_CFUstudybreakpoint_R2<-100*(max(0,(sum(model_CFUstudybreakpoint_H0$sigma2,model_CFUstudybreakpoint_H0$tau2) -
                                   sum(model_CFUstudybreakpoint$sigma2,model_CFUstudybreakpoint$tau2)) / 
                                sum(model_CFUstudybreakpoint_H0$sigma2,model_CFUstudybreakpoint_H0$tau2)))
  
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })

saveRDS(model_CFUstudybreakpoint,'3_models/alternative_analyses/CFUbreakpoints/subalternative_model_CFUstudybreakpoint.RDS')
saveRDS(model_CFUstudybreakpoint_I2,'3_models/alternative_analyses/CFUbreakpoints/subalternative_I2_CFUstudybreakpoint.RDS')
saveRDS(model_CFUstudybreakpoint_R2,'3_models/alternative_analyses/CFUbreakpoints/subalternative_R2_CFUstudybreakpoint.RDS')

# STUDY BREAKPOINT (RELATIVE TO CLSI) ---------------------------------------------

#log CLSI breakpoints
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_CLSIorNARMSbreakpointmgL<-log10(pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_CLSIorNARMSbreakpointmgL)

#calculate variable of how much (or less) more study breakpoint is than CLSI 'Resistant' breakpoint (relative) 
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studyCLSIbreakpointdiff<-
  (10^(pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointmgL))/
  (10^(pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_CLSIorNARMSbreakpointmgL))

pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studyCLSIbreakpointdiff

## RUN AN INTERCEPT-ONLY MODEL FOR ALL DATA (H0) -----------------

model_CFUbreakpointsdiff_H0 <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                           random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                           struct = c('CAR'),
                                           data = pico_witheffectsizes_post_faeces_CFUonly,
                                           control=list(rel.tol=1e-8))

## RUN A MODEL WITH RESISTANCE TARGET AS MODERATOR -----------------

model_CFUbreakpointsdiff <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                        mod = ~ outcome_resistance_studyCLSIbreakpointdiff,
                                        random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                        struct = c('CAR'),
                                        data = pico_witheffectsizes_post_faeces_CFUonly,
                                        control=list(rel.tol=1e-8))
#refit model
tryCatch(
  model_CFUbreakpointsdiff_noranf <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                                 mod = ~ outcome_resistance_studyCLSIbreakpointdiff,
                                                 #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                                 #struct = c('CAR'),
                                                 data = pico_witheffectsizes_post_faeces_CFUonly,
                                                 control=list(rel.tol=1e-8))
  ,
  
  #if error, 
  error = function(e){
    print('Could not compute model - skipped')
  }
)

tryCatch(
  #use Jackson approach to calculate I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
  model_CFUbreakpointsdiff_I2<-c(100 * (vcov(model_CFUbreakpointsdiff)[1,1] - vcov(model_CFUbreakpointsdiff_noranf)[1,1]) / vcov(model_CFUbreakpointsdiff)[1,1])
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })


tryCatch(
  #use Wolfgang's approach to calculate pseudo-R2 of 3 variance components (2 sigmas and tau) - https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-October/000247.html; https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor; https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
  model_CFUbreakpointsdiff_R2<-100*(max(0,(sum(model_CFUbreakpointsdiff_H0$sigma2,model_CFUbreakpointsdiff_H0$tau2) -
                                         sum(model_CFUbreakpointsdiff$sigma2,model_CFUbreakpointsdiff$tau2)) / 
                                      sum(model_CFUbreakpointsdiff_H0$sigma2,model_CFUbreakpointsdiff_H0$tau2)))
  
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })

saveRDS(model_CFUbreakpointsdiff,'3_models/alternative_analyses/CFUbreakpoints/subalternative_model_CFUbreakpointsdiff.RDS')
saveRDS(model_CFUbreakpointsdiff_I2,'3_models/alternative_analyses/CFUbreakpoints/subalternative_I2_CFUbreakpointsdiff.RDS')
saveRDS(model_CFUbreakpointsdiff_R2,'3_models/alternative_analyses/CFUbreakpoints/subalternative_R2_CFUbreakpointsdiff.RDS')

# STUDY BREAKPOINTS (RELATIVE TO CLSI - CATEGORICAL) -------------------------------------------------------------

#create a new blank variable
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointtype<-NA

pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointtype[is.na(pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studyCLSIbreakpointdiff)]<-'No CLSI/NARMS breakpoint'
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointtype[pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studyCLSIbreakpointdiff<1]<-'Below CLSI/NARMS breakpoint \n (i.e. intermediate/ \n susceptible dose-dependent)'
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointtype[pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studyCLSIbreakpointdiff==1]<-'CLSI/NARMS breakpoint'
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointtype[pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studyCLSIbreakpointdiff>1]<-'Above CLSI/NARMS breakpoint'

#make a factor
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointtype<-as.factor(pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointtype)

#check it
pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_studybreakpointtype

## RUN AN INTERCEPT-ONLY MODEL FOR ALL DATA (H0) -----------------

model_CFUstudybreakpointtype_H0 <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                               random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                               struct = c('CAR'),
                                               data = pico_witheffectsizes_post_faeces_CFUonly,
                                               control=list(rel.tol=1e-8))

## RUN A MODEL WITH RESISTANCE TARGET AS MODERATOR -----------------

model_CFUstudybreakpointtype <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                            mod = ~ 0 + outcome_resistance_studybreakpointtype,
                                            random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                            struct = c('CAR'),
                                            data = pico_witheffectsizes_post_faeces_CFUonly,
                                            control=list(rel.tol=1e-8))
#refit model
tryCatch(
  model_CFUstudybreakpointtype_noranf <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                                     mod = ~ 0 + outcome_resistance_studybreakpointtype,
                                                     #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                                     #struct = c('CAR'),
                                                     data = pico_witheffectsizes_post_faeces_CFUonly,
                                                     control=list(rel.tol=1e-8))
  ,
  
  #if error, 
  error = function(e){
    print('Could not compute model - skipped')
  }
)

tryCatch(
  #use Jackson approach to calculate I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
  model_CFUstudybreakpointtype_I2<-c(100 * (vcov(model_CFUstudybreakpointtype)[1,1] - vcov(model_CFUstudybreakpointtype_noranf)[1,1]) / vcov(model_CFUstudybreakpointtype)[1,1])
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })


tryCatch(
  #use Wolfgang's approach to calculate pseudo-R2 of 3 variance components (2 sigmas and tau) - https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-October/000247.html; https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor; https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
  model_CFUstudybreakpointtype_R2<-100*(max(0,(sum(model_CFUstudybreakpointtype_H0$sigma2,model_CFUstudybreakpointtype_H0$tau2) -
                                             sum(model_CFUstudybreakpointtype$sigma2,model_CFUstudybreakpointtype$tau2)) / 
                                          sum(model_CFUstudybreakpointtype_H0$sigma2,model_CFUstudybreakpointtype_H0$tau2)))
  
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })

saveRDS(model_CFUstudybreakpointtype,'3_models/alternative_analyses/CFUbreakpoints/subsubgroup_model_CFUstudybreakpointtype.RDS')
saveRDS(model_CFUstudybreakpointtype_I2,'3_models/alternative_analyses/CFUbreakpoints/subsubgroup_I2_CFUstudybreakpointtype.RDS')
saveRDS(model_CFUstudybreakpointtype_R2,'3_models/alternative_analyses/CFUbreakpoints/subsubgroup_R2_CFUstudybreakpointtype.RDS')
