source('functions/plot_bubbleplots_H2_supplementaryfunctions.R')
source('functions/load_plotdefaults.R')

# LOAD MODELS AND I2S AND R2S ---------------------------------------------

dose_models_files<-normalizePath(list.files('3_models/alternative_analyses/dose/', pattern='model', full.names = T))
dose_models_I2s_files<-normalizePath(list.files('3_models/alternative_analyses/dose/', pattern='I2', full.names = T))
#dose_models_R2s_files<-normalizePath(list.files('3_models/alternative_analyses/dose/', pattern='model_R2.RDS', full.names = T))

legend_allstudies<-readRDS('4_figures/legend_allstudies.RDS')

# PRESETS -----------------------------------------------------------------

dose_analysis_names<-gsub(".*3_models/alternative_analyses/dose/alternative_model_(.+).RDS*", "\\1", dose_models_files)

xlabs_new<-c('Cumulative doses of antibiotic administered \n before resistance measurement (mg/kg)',
             'Cumulative dose of antibiotic administered \n before resistance measurement \n (log10-transformed mg/kg)',
             'Daily dose of antibiotic administered \n (mg/kg)',
             'Daily dose of antibiotic administered \n (log10-transformed mg/kg)')

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)
#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_post_faeces$study_studyID), names(microshades_cvd))]

# LOOP OVER MODELS TO MAKE bubble PLOTS ----------------------------------

for (m in 1:length(dose_models_files)){
  
print(paste("dose analysis being plotted:", dose_analysis_names[m]))
  
model_dose<-readRDS(dose_models_files[m])
model_dose_I2<-readRDS(dose_models_I2s_files[m])
#model_dose_R2<-readRDS(dose_models_R2s_files[m])

tryCatch(
    bubbleplot<-
      bubble_plot_MLJ(model_dose, 
                      group = "study_studyID", 
                      mod = dose_analysis_names[m], 
                      g = T,
                      k.pos = 'bottom.right',
                      xlab =  xlabs_new[m],
                      ylab = "Standardised mean difference \n(with heteroscedastic population variances)")+
      ggplot2::geom_hline(yintercept = 0, 
                          linetype = 2, colour = "black", alpha=0.5)+
      ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
      ggplot2::scale_colour_manual(values = cbpl_temp)+
      ggplot2::scale_fill_manual(values = cbpl_temp)+
      theme_plots()+
      # theme(legend.position = "inside",
      #       legend.justification = c(0.03, 1),
      #       legend.text=element_text(size=0.25),
      #       legend.direction = "horizontal",
      #       legend.background = ggplot2::element_blank())+
      NULL
    , 
    
  #if error, 
  error = function(e){
    print('Could not compute model - skipped');
    
    bubbleplot<-
    bubble_plot_MLJ(object = pico_witheffectsizes_post_faeces,
                     yi = "yi_totalresistancedeterminants",
                     vi = "vi_totalresistancedeterminants",
                     stdy = "study_studyID",
                     by = NULL,
                     group = "study_studyID",
                     mod = dose_analysis_names[m],
                     g = T,
                     k.pos = 'bottom.right',
                     xlab =  xlabs_new[m],
                     ylab = "Standardised mean difference \n(with heteroscedastic population variances)")+
      ggplot2::geom_hline(yintercept = 0, 
                          linetype = 2, colour = "black", alpha=0.5)+
      ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
      ggplot2::scale_colour_manual(values = cbpl_temp)+
      ggplot2::scale_fill_manual(values = cbpl_temp)+
      theme_plots()
  }
)

if (m%in%grep('log10',dose_analysis_names)){
  bubbleplot<-bubbleplot+
    scale_x_continuous(breaks = log10(c(0.1,0.3,1,3,10,30,100,300,1000)), labels=c(0.1,0.3,1,3,10,30,100,300,1000))

}

if(length(model_dose$b)==2){
  bubbleplot<-
    bubbleplot+
    annotate("text", size=3.5, x=floor(min(pico_witheffectsizes_post_faeces[dose_analysis_names[m]])), y=2.5, hjust=0,
             label=paste("Slope = ", round(model_dose$b[2],2),'\n',
                         "95% CI = ", round(model_dose$ci.ub[2],2),' to ', round(model_dose$ci.ub[2],2),'\n',
                         #"95% PI = ", estimates[,5],' to ',estimates[,6],'\n',
                         "p = ", format.pval(model_dose$pval[2], eps = 0.01, digits = 2),
                         sep=''))
}

ggsave(paste('4_figures/alternative_analyses/dose/bubbleplot_',dose_analysis_names[m],'.tiff', sep=''), plot=last_plot(), width=6, height=6)
saveRDS(bubbleplot, paste('4_figures/alternative_analyses/dose/bubbleplot_',dose_analysis_names[m],'.RDS', sep=''))

}

# PANEL PLOT OF ALL FOUR --------------------------------------------------

bubbleplot_intervention_perdayUDD_mgkg<-readRDS('4_figures/alternative_analyses/dose/bubbleplot_intervention_perdayUDD_mgkg.RDS')
bubbleplot_intervention_perdayUDDlog10_mgkg<-readRDS('4_figures/alternative_analyses/dose/bubbleplot_intervention_perdayUDDlog10_mgkg.RDS')
bubbleplot_intervention_cumulativeUDD_mgkg<-readRDS('4_figures/alternative_analyses/dose/bubbleplot_intervention_cumulativeUDD_mgkg.RDS')
bubbleplot_intervention_cumulativeUDDlog10_mgkg<-readRDS('4_figures/alternative_analyses/dose/bubbleplot_intervention_cumulativeUDDlog10_mgkg.RDS')

library(patchwork)

layout <-
  "AABBEEE
   CCDDEEE"

#varcovar
plot(bubbleplot_intervention_perdayUDD_mgkg+#ggpubr::rremove('ylab')+
       bubbleplot_intervention_perdayUDDlog10_mgkg+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('x.ticks')+
       bubbleplot_intervention_cumulativeUDD_mgkg+#ggpubr::rremove('ylab')+
       bubbleplot_intervention_cumulativeUDDlog10_mgkg+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('x.ticks')+
       legend_allstudies+
       patchwork::plot_layout(design=layout))
ggsave('4_figures/alternative_analyses/dose/bubbleplot_intervention_dose_allforms.tiff', plot=last_plot(), width=12, height=8)

