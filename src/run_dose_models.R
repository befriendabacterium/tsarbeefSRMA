post_H0_model<-readRDS('3_models/hypothesis_0/H0_model_post_faeces_totalresistancedeterminants.RDS')

doseforms<-c('intervention_perdayUDD_mgkg','intervention_perdayUDDlog10_mgkg','intervention_cumulativeUDD_mgkg','intervention_cumulativeUDDlog10_mgkg')

for (f in 1:length(doseforms)){
  
#drop levels before modelling
pico_witheffectsizes_post_faeces<-droplevels(pico_witheffectsizes_post_faeces)
  
#refit model
tryCatch(
  #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
  model_dose <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                    mod = formula(paste('~',doseforms[f], sep='')),
                                    random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                    struct = c('CAR'),
                                    data = pico_witheffectsizes_post_faeces,
                                    control=list(rel.tol=1e-8)
  ) 
  
  , 
  
  #if error, 
  error = function(e){
    print('Could not compute model - skipped')
  }
)

#refit model
tryCatch(
  #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
  model_dose_noranf <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                           mod = formula(paste('~',doseforms[f], sep='')),
                                           #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                           #struct = c('CAR'),
                                           data = pico_witheffectsizes_post_faeces,
                                           control=list(rel.tol=1e-8)
  ) 
  
  , 
  
  #if error, 
  error = function(e){
    print('Could not compute model - skipped')
  }
)

tryCatch(
  #use Jackson approach to calculate I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
  model_dose_I2<-c(100 * (vcov(model_dose)[1,1] - vcov(model_dose_noranf)[1,1]) / vcov(model_dose)[1,1])
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })


tryCatch(
  #use Wolfgang's approach to calculate pseudo-R2 of 3 variance components (2 sigmas and tau) - https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-October/000247.html; https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor; https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
  model_dose_R2<-100*(max(0,(sum(post_H0_model$sigma2,post_H0_model$tau2) -
                                   sum(model_dose$sigma2,model_dose$tau2)) / 
                                sum(post_H0_model$sigma2,post_H0_model$tau2)))
  
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })
  
  #directory creation chunk
  modelname<-paste("alternative_model_", doseforms[f], sep='')
  I2name<-sub("model","I2",modelname)
  
  saveRDS(model_dose, paste('3_models/alternative_analyses/dose/',modelname,'.RDS', sep=''))
  saveRDS(model_dose_I2, paste('3_models/alternative_analyses/dose/',I2name,'.RDS', sep=''))
  #saveRDS(model_dose_R2, paste(writepath,'_R2.RDS', sep=''))  
  
}
