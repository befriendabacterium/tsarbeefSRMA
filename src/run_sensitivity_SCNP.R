# LOAD MODELS -------------------------------------------------------------

during_H1_model<-readRDS('3_models/hypothesis_1/H1_model_during_totalresistancedeterminants.RDS')
during_H1_model_I2<-readRDS('3_models/hypothesis_1/H1_I2_during_totalresistancedeterminants.RDS')
after_H1_model<-readRDS('3_models/hypothesis_1/H1_model_after_totalresistancedeterminants.RDS')
after_H1_model_I2<-readRDS('3_models/hypothesis_1/H1_I2_after_totalresistancedeterminants.RDS')

during_H2_model<-readRDS('3_models/hypothesis_2/H2_model_during_totalresistancedeterminants_timelinear.RDS')
during_H2_model_I2<-readRDS('3_models/hypothesis_2/H2_I2_during_totalresistancedeterminants_timelinear.RDS')
after_H2_model<-readRDS('3_models/hypothesis_2/H2_model_after_totalresistancedeterminants_timelinear.RDS')
after_H2_model_I2<-readRDS('3_models/hypothesis_2/H2_I2_after_totalresistancedeterminants_timelinear.RDS')

candidate_models_H1_list<-list(during_H1_model=during_H1_model, after_H1_model=after_H1_model)
candidate_models_H2_list<-list(during_H2_model=during_H2_model, after_H2_model=after_H2_model)

# SHARED CONTROL NON-INDEPENDENCE-----------------------------------------

  for (m in 1:length(candidate_models_H1_list)){
    model_H1<-'Cannot run model'
    model_H1_noranf<-'Cannot run model'
    model_H1_I2<-'Cannot run at least one of the models'
    model_H2<-'Cannot run model'
    model_H2_noranf<-'Cannot run model'
    model_H2_I2<-'Cannot run at least one of the models'
    
    df_current<-candidate_models_H1_list[[m]]$data
    
    #apply shared control penalty by dividing the intervention sample size by the number of intervention groups sharing 1 control and flooring it (round down)
    df_current$intervention_reps_totalresistancedeterminants<-floor(df_current$intervention_reps_totalresistancedeterminants/df_current$outcome_interventionssharingcontrol)
    
    #remove any with no replication after penalty (i.e. n=1)
    df_current<-df_current[which(df_current$intervention_reps_totalresistancedeterminants!=1),]
    
    df_current
    
    #recalculate effect sizes after penalisation
    effectsizes_totalresistancedeterminants<-metafor::escalc(measure="SMDH",
                                         data = df_current,
                                         m1i = intervention_mean_totalresistancedeterminants,
                                         m2i = control_mean_totalresistancedeterminants,
                                         sd1i = intervention_sd_totalresistancedeterminants,
                                         sd2i = control_sd_totalresistancedeterminants,
                                         n1i = intervention_reps_totalresistancedeterminants,
                                         n2i = control_reps_totalresistancedeterminants,
                                         drop00=T,
                                         append=F)
    
    #make the ni column out of the attributes variable
    effectsizes_totalresistancedeterminants$ni<-attr(effectsizes_totalresistancedeterminants$yi, which="ni")
    
    #replace old ones with recalced ones
    df_current$yi_totalresistancedeterminants<-effectsizes_totalresistancedeterminants$yi
    df_current$vi_totalresistancedeterminants<-effectsizes_totalresistancedeterminants$vi
    df_current$ni_totalresistancedeterminants<-effectsizes_totalresistancedeterminants$ni
    
    #drop levels before modelling
    df_current<-droplevels(df_current)
    
    #refit model
    tryCatch(
      #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
      model_H1<- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                 #mod = candidate_models_list[[m]]$formula.mods,
                                 random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                 struct = c('CAR','GEN','GEN'),
                                 data = df_current,
                                 control=list(rel.tol=1e-8)
      ) 
      , 
      
      #if error, 
      error = function(e){
        #print('Could not compute model - skipped')
      }
    )
    
    #refit model
    tryCatch(
      #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
      model_H1_noranf<- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                        #mod = candidate_models_list[[m]]$formula.mods,
                                        #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                        #struct = c('CAR'),
                                        data = df_current,
                                        control=list(rel.tol=1e-8)
      ) 
      , 
      
      #if error, 
      error = function(e){
        #print('Could not compute model - skipped')
      }
    )
    
    tryCatch(
      #use Jackson approach to calculate I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
      model_H1_I2<-c(100 * (vcov(model_H1)[1,1] - vcov(model_H1_noranf)[1,1]) / vcov(model_H1)[1,1])
      ,
      #if error, 
      error = function(e){
        #print('Could not compute model - skipped')
      })
    
    modelname<-names(candidate_models_H1_list)[m]
    I2name<-sub("model","I2",names(candidate_models_H1_list)[m])
    
    saveRDS(model_H1, paste('3_models/hypothesis_1/sensitivity/',modelname,'_correct_SCNI.RDS', sep=''))
    saveRDS(model_H1_I2, paste('3_models/hypothesis_1/sensitivity/',I2name,'_correct_SCNI.RDS', sep=''))
    
    #refit model
    tryCatch(
      #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
      model_H2<- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                 mod = candidate_models_H2_list[[m]]$formula.mods,
                                 random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                 struct = c('CAR'),
                                 data = df_current,
                                 control=list(rel.tol=1e-8)
      ) 
      , 
      
      #if error, 
      error = function(e){
        #print('Could not compute model - skipped')
      }
    )
    
    tryCatch(
      #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
      model_H2_noranf<- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                        mod = candidate_models_H2_list[[m]]$formula.mods,
                                        #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                        #struct = c('CAR'),
                                        data = df_current,
                                        control=list(rel.tol=1e-8)
      ) 
      , 
      
      #if error, 
      error = function(e){
        #print('Could not compute model - skipped')
      }
    )
    
    tryCatch(
      #use Jackson approach to calculate I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
      model_H2_I2<-c(100 * (vcov(model_H2)[1,1] - vcov(model_H2_noranf)[1,1]) / vcov(model_H2)[1,1])
      ,
      #if error, 
      error = function(e){
        #print('Could not compute model - skipped')
      })
    
    modelname<-names(candidate_models_H2_list)[m]
    I2name<-sub("model","I2",names(candidate_models_H2_list)[m])
    
    saveRDS(model_H2, paste('3_models/hypothesis_2/sensitivity/',modelname,'_correct_SCNI.RDS', sep=''))
    saveRDS(model_H2_I2, paste('3_models/hypothesis_2/sensitivity/',I2name,'_correct_SCNI.RDS', sep=''))
    
  }
