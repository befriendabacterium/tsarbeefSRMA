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

# VARIANCE COVARIANCE -----------------------------------------

correlations<-seq(0,0.8,0.2)

for (m in 1:length(candidate_models_H1_list)){
    
    df_current<-candidate_models_H1_list[[m]]$data
    
    ###############################################
    # Sampling variance-covariance matrix
    ###############################################
    
    # We specified sampling variance as variance-covariance matrix,
    # with the sampling variance for each effect size on the diagonal,
    # and the covariance between these measures as off-diagonal elements.
    # The conservative model assumed a 0.5 correlation between the effect
    # size sample variances with the same experiment ID. For a similar
    # approach, see O'Dea et al., 2019: https://doi.org/10.1111/faf.12394
    
    # Coded by Nicholas P. Moran
    
    # creates a matrix (called 'VCV_totalresistancedeterminants2') with the dimesions n(effect_sizes) x n(effect_sizes)
    VCV_totalresistancedeterminants <- matrix(0, nrow = dim(df_current)[1], ncol = dim(df_current)[1])
    
    # names rows and columns for each EffectID
    rownames(VCV_totalresistancedeterminants) <- rownames(df_current)
    colnames(VCV_totalresistancedeterminants) <-  rownames(df_current)
    
    # finds effect sizes that come from the same study
    shared_coord <- which(df_current[, "study_studyID"] %in% df_current[duplicated(df_current[, "study_studyID"]), "study_studyID"] == TRUE) 
    combinations <- do.call("rbind", tapply(shared_coord, df_current[shared_coord, "study_studyID"], function(x) t(utils::combn(x, 2))))
    
    for (c in 1:length(correlations)){
      
      model_H1<-'Cannot run model'
      model_H1_noranf<-'Cannot run model'
      model_H1_I2<-'Cannot run at least one of the models'
      model_H2<-'Cannot run model'
      model_H2_noranf<-'Cannot run model'
      model_H2_I2<-'Cannot run at least one of the models'
      
      # calculates the covariance between effect sizes and enters them in each combination of coordinates
      for (i in 1:dim(combinations)[1]) {
        p1 <- combinations[i, 1]
        p2 <- combinations[i, 2]
        p1_p2_cov <- correlations[c] * sqrt(df_current[p1, "vi_totalresistancedeterminants"]) * sqrt(df_current[p2, "vi_totalresistancedeterminants"])
        VCV_totalresistancedeterminants[p1, p2] <- p1_p2_cov
        VCV_totalresistancedeterminants[p2, p1] <- p1_p2_cov
      } 
      
      # enters previously calculated effect size sampling variances into diagonals 
      diag(VCV_totalresistancedeterminants) <- df_current[, "vi_totalresistancedeterminants"]
      
      # #In case you want to visually double check the matrix outside of R
      write.csv(VCV_totalresistancedeterminants, paste('2_processeddata/',
                                      stringr::str_remove_all(names(candidate_models_H1_list)[m],"_H1_model"),
                                      '_varcovarmatrix_',correlations[c],'.csv', sep=''))
      #drop levels before modelling
      df_current<-droplevels(df_current)
      
      #refit model
      tryCatch(
      #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
      model_H1<- metafor::rma.mv(yi_totalresistancedeterminants, VCV_totalresistancedeterminants, 
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
      model_H1_noranf<- metafor::rma.mv(yi_totalresistancedeterminants, VCV_totalresistancedeterminants, 
                                        #mod = candidate_models_list[[m]]$formula.mods,
                                        #random = list(~outcome_time_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
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
      
      saveRDS(model_H1, paste('3_models/hypothesis_1/sensitivity/',modelname,'_varcov_',correlations[c],'.RDS', sep=''))
      saveRDS(model_H1_I2, paste('3_models/hypothesis_1/sensitivity/',I2name,'_varcov_',correlations[c],'.RDS', sep=''))
      
    #refit model
    tryCatch(
      #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
      model_H2<- metafor::rma.mv(yi_totalresistancedeterminants, VCV_totalresistancedeterminants, 
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
      model_H2_noranf<- metafor::rma.mv(yi_totalresistancedeterminants, VCV_totalresistancedeterminants, 
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
      
      saveRDS(model_H2, paste('3_models/hypothesis_2/sensitivity/',modelname,'_varcov_',correlations[c],'.RDS', sep=''))
      saveRDS(model_H2_I2, paste('3_models/hypothesis_2/sensitivity/',I2name,'_varcov_',correlations[c],'.RDS', sep=''))
    
    }
}
    
