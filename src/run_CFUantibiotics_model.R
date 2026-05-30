#sub-subgroup analysis: CFU
#make CFU-only dataset
pico_witheffectsizes_post_faeces_CFUonly<-pico_witheffectsizes_post_faeces[pico_witheffectsizes_post_faeces$outcome_measurement_type=='Direct plating \n on agar',]

#rename
#pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_antibiotic<-plyr::revalue(pico_witheffectsizes_post_faeces_CFUonly$outcome_resistance_antibiotic,c("ery8"="ery",'ery128'='ery'))

#drop levels
pico_witheffectsizes_post_faeces_CFUonly<-droplevels(pico_witheffectsizes_post_faeces_CFUonly)

# ANTIBIOTIC RESISTANCE TESTED FOR ----------------------------------------

## RUN AN INTERCEPT-ONLY MODEL FOR ALL DATA (H0) -----------------

model_CFUantibiotics_H0 <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                   random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                   struct = c('CAR'),
                                   data = pico_witheffectsizes_post_faeces_CFUonly,
                                   control=list(rel.tol=1e-8))

## RUN A MODEL WITH RESISTANCE TARGET AS MODERATOR -----------------

model_CFUantibiotics <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                              mod = ~ 0 + outcome_resistance_antibiotic,
                              random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                              struct = c('CAR'),
                              data = pico_witheffectsizes_post_faeces_CFUonly,
                              control=list(rel.tol=1e-8))
#refit model
tryCatch(
model_CFUantibiotics_noranf <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                   mod = ~ 0 + outcome_resistance_antibiotic,
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
  model_CFUantibiotics_I2<-c(100 * (vcov(model_CFUantibiotics)[1,1] - vcov(model_CFUantibiotics_noranf)[1,1]) / vcov(model_CFUantibiotics)[1,1])
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })


tryCatch(
  #use Wolfgang's approach to calculate pseudo-R2 of 3 variance components (2 sigmas and tau) - https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-October/000247.html; https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor; https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
  model_CFUantibiotics_R2<-100*(max(0,(sum(model_CFUantibiotics_H0$sigma2,model_CFUantibiotics_H0$tau2) -
                                   sum(model_CFUantibiotics$sigma2,model_CFUantibiotics$tau2)) / 
                                sum(model_CFUantibiotics_H0$sigma2,model_CFUantibiotics_H0$tau2)))
  
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })

saveRDS(model_CFUantibiotics,'3_models/alternative_analyses/CFUantibiotics/subsubgroup_model_CFUantibiotics.RDS')
saveRDS(model_CFUantibiotics_I2,'3_models/alternative_analyses/CFUantibiotics/subsubgroup_I2_CFUantibiotics.RDS')
saveRDS(model_CFUantibiotics_R2,'3_models/alternative_analyses/CFUantibiotics/subsubgroup_R2_CFUantibiotics.RDS')
