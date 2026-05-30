
pico_witheffectsizes_post_faeces_qPCRonly<-pico_witheffectsizes_post_faeces[pico_witheffectsizes_post_faeces$outcome_measurement_type=='Quantitative PCR \n (qPCR)',]

#drop levels
pico_witheffectsizes_post_faeces_qPCRonly<-droplevels(pico_witheffectsizes_post_faeces_qPCRonly)

levels(pico_witheffectsizes_post_faeces_qPCRonly$outcome_resistance_target)

#drop levels
pico_witheffectsizes_post_faeces_qPCRonly<-droplevels(pico_witheffectsizes_post_faeces_qPCRonly)

# RUN AN INTERCEPT-ONLY MODEL FOR ALL DATA (H0) -----------------

model_qPCRgenes_H0 <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                   random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                   struct = c('CAR'),
                                   data = pico_witheffectsizes_post_faeces_qPCRonly,
                                   control=list(rel.tol=1e-8))

# RUN A MODEL WITH RESISTANCE TARGET AS MODERATOR -----------------

model_qPCRgenes <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                              mod = ~ 0 + outcome_resistance_target,
                              random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                              struct = c('CAR'),
                              data = pico_witheffectsizes_post_faeces_qPCRonly,
                              control=list(rel.tol=1e-8))
#refit model
tryCatch(
model_qPCRgenes_noranf <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                   mod = ~ 0 + outcome_resistance_target,
                                   #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                   #struct = c('CAR'),
                                   data = pico_witheffectsizes_post_faeces_qPCRonly,
                                   control=list(rel.tol=1e-8))
,

#if error, 
error = function(e){
  print('Could not compute model - skipped')
}
)

tryCatch(
  #use Jackson approach to calculate I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
  model_qPCRgenes_I2<-c(100 * (vcov(model_qPCRgenes)[1,1] - vcov(model_qPCRgenes_noranf)[1,1]) / vcov(model_qPCRgenes)[1,1])
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })


tryCatch(
  #use Wolfgang's approach to calculate pseudo-R2 of 3 variance components (2 sigmas and tau) - https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-October/000247.html; https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor; https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
  model_qPCRgenes_R2<-100*(max(0,(sum(model_qPCRgenes_H0$sigma2,model_qPCRgenes_H0$tau2) -
                                   sum(model_qPCRgenes$sigma2,model_qPCRgenes$tau2)) / 
                                sum(model_qPCRgenes_H0$sigma2,model_qPCRgenes_H0$tau2)))
  
  ,
  #if error, 
  error = function(e){
    #print('Could not compute model - skipped')
  })


saveRDS(model_qPCRgenes,'3_models/alternative_analyses/qPCRgenes/subsubgroup_model_qPCRgenes.RDS')
saveRDS(model_qPCRgenes_I2,'3_models/alternative_analyses/qPCRgenes/subsubgroup_I2_qPCRgenes.RDS')
saveRDS(model_qPCRgenes_R2,'3_models/alternative_analyses/qPCRgenes/subsubgroup_R2_qPCRgenes.RDS')
