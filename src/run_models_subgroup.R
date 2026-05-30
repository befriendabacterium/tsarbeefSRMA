# READ IN ORIGINAL INTERCEPT-ONLY MODEL FOR ALL DATA (H0) -----------------

post_H0_model<-readRDS('3_models/hypothesis_0/H0_model_post_faeces_totalresistancedeterminants.RDS')
#post_H0_model_I2<-readRDS('3_models/hypothesis_0/H0_I2_post_faeces_totalresistancedeterminants.RDS')

# SOURCE MY UPDATED ORCHARD FUNCTIONS -------------------------------------

source('functions/plot_bubbleplots_H2_supplementaryfunctions.R')

subgroup_analysis_names<-c('study_studydesign',
                           'population_antibioticfreebefore',
                           'intervention_mode',
                           'intervention_cointervention_TF',
                           'intervention_antibiotic_class',
                           'outcome_frommixedpen_TF',
                           'outcome_measurement_type', 
                           'outcome_organism', 
                           'outcome_unitofanalysis')

for (r in 1:length(subgroup_analysis_names)){
  
    print(paste('Subgroup analysis:',subgroup_analysis_names[r]))

    model_subgroup<-'Cannot run model'
    model_subgroup_noranf<-'Cannot run model'
    model_subgroup_I2<-'Cannot run at least one of the models'
    model_H2<-'Cannot run model'
    model_H2_noranf<-'Cannot run model'
    model_H2_I2<-'Cannot run at least one of the models'
    
    #coerce to factor
    pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]]<-as.factor(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])
    #drop relict levels
    pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]]<-droplevels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])
    #count panels for plotting
    npanels<-nlevels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])

    if (subgroup_analysis_names[r]=='outcome_organism'){
      levels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])[levels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])%in%c('e.coli','ecoli','enterobacteriaceae','salm')]<-'Enterobacteriaceae'
      levels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])[levels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])%in%c('enteroc')]<-'Enterococcaceae'
      levels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])[levels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])%in%c('cjej')]<-'Campylobacteraceae'
    }
    #drop levels before modelling
    pico_witheffectsizes_post_faeces<-droplevels(pico_witheffectsizes_post_faeces)
    
    if(nlevels(pico_witheffectsizes_post_faeces[,subgroup_analysis_names[r]])>1){
    #refit model
    tryCatch(
      #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
      model_subgroup <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                 mod = formula(paste('~ 0 + ',subgroup_analysis_names[r], sep='')),
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
        model_subgroup_noranf <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants, 
                                   mod = formula(paste('~ 0 + ',subgroup_analysis_names[r], sep='')),
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
        model_subgroup_I2<-c(100 * (vcov(model_subgroup)[1,1] - vcov(model_subgroup_noranf)[1,1]) / vcov(model_subgroup)[1,1])
        ,
        #if error, 
        error = function(e){
          #print('Could not compute model - skipped')
        })
      
      
      tryCatch(
        #use Wolfgang's approach to calculate pseudo-R2 of 3 variance components (2 sigmas and tau) - https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-October/000247.html; https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor; https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
        model_subgroup_R2<-100*(max(0,(sum(post_H0_model$sigma2,post_H0_model$tau2) -
                          sum(model_subgroup$sigma2,model_subgroup$tau2)) / 
                          sum(post_H0_model$sigma2,post_H0_model$tau2)))
        
        ,
        #if error, 
        error = function(e){
          #print('Could not compute model - skipped')
        })
      
      
      #plot ('Could not run model' so this saves as last plot unless a plot can be plotted)
      text = paste("Could not run model")
      ggplot(mtcars, aes(wt, mpg)) + 
        annotate("text", x = 4, y = 25, size=8, label = text)
      
      modelname<-paste("subgroup_model_", subgroup_analysis_names[r], sep='')
      I2name<-sub("model","I2",modelname)
      
      saveRDS(model_subgroup, paste('3_models/alternative_analyses/subgroup/', modelname,'.RDS', sep=''))
      saveRDS(model_subgroup_I2, paste('3_models/alternative_analyses/subgroup/',I2name,'.RDS', sep=''))
      #saveRDS(model_subgroup_R2, paste('3_models/alternative_analyses/subgroup/','_R2.RDS', sep=''))  
     
    } #if statement for levels>1 
  }

