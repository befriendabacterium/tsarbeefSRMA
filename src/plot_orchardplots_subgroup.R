library(patchwork)

# LOAD MODELS AND I2S AND R2S ---------------------------------------------

subgroup_models_files<-normalizePath(list.files('3_models/alternative_analyses/subgroup/', pattern='model', full.names = T))
subgroup_models_I2s_files<-normalizePath(list.files('3_models/alternative_analyses/subgroup/', pattern='I2', full.names = T))
# subgroup_models_R2s_files<-normalizePath(list.files('3_models/alternative_analyses/subgroup/', pattern='model_R2.RDS', full.names = T))

legend_allstudies<-readRDS('4_figures/legend_allstudies.RDS')

source('functions/plot_bubbleplots_H2_supplementaryfunctions.R')
source('functions/load_plotdefaults.R')

# PRESETS -----------------------------------------------------------------

subgroup_analysis_names<-gsub(".*3_models/alternative_analyses/subgroup/subgroup_model_(.+).RDS*", "\\1", subgroup_models_files)

xlabs_new<-c('Intervention antibiotic class',
             'Were other antibiotics given alongside the intervention antibiotic?',
             'Mode of intervention antibiotic administration',
             'Were the intervention cattle housed with control cattle?', 
             'Method of measuring antibiotic resistance outcome', 
             'In what microorganism(s) was resistance tested?',
             'Unit of analysis',
             'Had cattle received antibiotics prior to the study?',
             'Study design')

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)
scales::show_col(microshades_cvd, labels=T)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_post_faeces$study_studyID), names(microshades_cvd))]

# LOOP OVER MODELS TO MAKE ORCHARD PLOTS ----------------------------------

for (m in 1:length(subgroup_models_files)){
  
print(paste("Subgroup analysis being plotted:", subgroup_analysis_names[m]))
  
model_subgroup<-readRDS(subgroup_models_files[m])
model_subgroup_I2<-readRDS(subgroup_models_I2s_files[m])
# model_subgroup_R2<-readRDS(subgroup_models_R2s_files[m])

#get CI and PI estimates from model
estimates<-orchaRd::mod_results(model_subgroup,
                                mod = as.character(model_subgroup$formula.mods[[2]][[3]]), 
                                group='study_studyID')$mod_table

#round for presentation
estimates[,-1]<-round(estimates[,-1], 2)

#add p value to estimates dataframe
estimates$pval<-model_subgroup$pval

tryCatch(
  orchardplot_temp<-orchard_plot_MLJ(object = model_subgroup,
                   mod = as.character(model_subgroup$formula.mods[[2]][[3]]),
                   group = "study_studyID",
                   flip = F,
                   g = T,
                   colour = T,
                   k.pos = -1.5,
                   legend.pos = "none",
                   xlab = "Standardised mean difference \n(with heteroscedastic population variances)")+
    labs(x=xlabs_new[m])+
    scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
    ggplot2::scale_colour_manual(values = cbpl_temp)+
    ggplot2::scale_fill_manual(values = cbpl_temp)+
    annotate("text", size=3.25, x=1:nrow(estimates)-0.525, y=2.5, hjust=0, 
             label=paste("Estimate = ", estimates[,2],'\n',
                         "95% CI = ", estimates[,3],' to ',estimates[,4],'\n',
                         "95% PI = ", estimates[,5],' to ',estimates[,6],'\n',
                         "p = ", format.pval(estimates$pval, eps = 0.01),
                         sep=''))+
    theme_plots()
  , 
  
  #if error, 
  error = function(e){
    print('Could not compute model - skipped');
    
    orchardplot_temp<-
      orchard_plot_MLJ(object = pico_witheffectsizes_post_faeces,
                     yi = "yi_totalresistancedeterminants",
                     vi = "vi_totalresistancedeterminants",
                     stdy = "study_studyID",
                     mod = subgroup_analysis_names[m],
                     by = NULL,
                     group = "study_studyID",
                     flip = F,
                     g = T,
                     colour = T,
                     k.pos = -1.5,
                     legend.pos = "none",
                     xlab = "Standardised mean difference \n(with heteroscedastic population variances)")+
      scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
      labs(x=xlabs_new[m])+
      ggplot2::scale_colour_manual(values = cbpl_temp)+
      ggplot2::scale_fill_manual(values = cbpl_temp)+
      theme_plots()
  }
)

plotwidth<-length(model_subgroup$b)

  #use patchwork to plot the plot with legend for all studies
    tryCatch(
    orchardplot_temp+
      legend_allstudies+
      patchwork::plot_layout(widths = c(plotwidth, (plotwidth/plotwidth)*1.25))
    , 
    
    #if error, 
    error = function(e){
      print('Could not compute model - skipped')
    }
    )

ggsave(paste('4_figures/alternative_analyses/subgroup/orchardplot_',subgroup_analysis_names[m],'.tiff', sep=''), plot=last_plot(), width=8.5*sqrt(plotwidth), height=6)

}
       
