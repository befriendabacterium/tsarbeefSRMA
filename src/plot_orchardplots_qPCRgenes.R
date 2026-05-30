source('functions/plot_bubbleplots_H2_supplementaryfunctions.R')
source('functions/load_plotdefaults.R')

library(ggplot2)
library(metafor)
library(dplyr)
library(orchaRd)

model_qPCRgenes<-readRDS('3_models/alternative_analyses/qPCRgenes/subsubgroup_model_qPCRgenes.RDS')
model_qPCRgenes_I2<-readRDS('3_models/alternative_analyses/qPCRgenes/subsubgroup_I2_qPCRgenes.RDS')
#model_qPCRgenes_R2<-readRDS('3_models/alternative_analyses/qPCRgenes/model_qPCRgenes_R2.RDS')

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(model_qPCRgenes$data$study_studyID), names(microshades_cvd))]

#add new xlabs (override capitalisation)
xticklabs<-levels(as.factor(model_qPCRgenes$data$outcome_resistance_target))
plottingorder<-orchaRd::firstup(xticklabs)
plottingorder<-plottingorder[order(xticklabs)]

xticklabs_new<-c('blaCMY-2 \n (Extended \n -spectrum \n cephalosporin)',
                 'blaCTX-M \n (Extended \n -spectrum \n cephalosporin)',
                 'sul2 \n (Sulfonamide)',
                 'tetB \n (Tetracycline)',
                 'tetC \n (Tetracycline)',
                 'tetL \n (Tetracycline)',
                 'tetM \n (Tetracycline)',
                 'tetO \n (Tetracycline)',
                 'tetW \n (Tetracycline)')

#option 1  - order by pooled effect size
#desired_order<-xlabs_new[order(model_qPCRgenes$b)]

#get CI and PI estimates from model
estimates<-orchaRd::mod_results(model = model_qPCRgenes,
                                mod = as.character(model_qPCRgenes$formula.mods[[2]][[3]]),
                                group = 'study_studyID')$mod_table
  
#round for presentation
estimates[,-1]<-round(estimates[,-1], 2)

#reorder
estimates<-estimates[order(as.character(estimates$name)),]

#add p value to estimates dataframe
estimates$pval<-model_qPCRgenes$pval

tryCatch(
orchardplot<-orchard_plot_MLJ(object = model_qPCRgenes,
                   mod = as.character(model_qPCRgenes$formula.mods[[2]][[3]]),
                   group = "study_studyID",
                   flip = F,
                   g = T,
                   colour = T,
                   k.pos = -1.5,
                   legend.pos = "none",
                   xlab = "Standardised mean difference \n(with heteroscedastic population variances)",
                   tree.order = plottingorder)+
  labs(x='Antibiotic resistance gene targetting in qPCR assay')+
  scale_x_discrete(labels=xticklabs_new[order(xticklabs)])+
  scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
  ggplot2::scale_colour_manual(values = cbpl_temp)+
  ggplot2::scale_fill_manual(values = cbpl_temp)+
  annotate("text", size=2.5, x=1:nrow(estimates)-0.55, y=2.6, hjust=0,
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
  
  orchardplot<-
    orchard_plot_MLJ(object = model_qPCRgenes$data,
                     yi = "yi_propres",
                     vi = "vi_propres",
                     stdy = "study_studyID",
                     mod = "outcome_resistance_target",
                     group = "study_studyID",
                     flip = F,
                     g = T,
                     colour = T,
                     k.pos = -1.5,
                     legend.pos = "none",
                     xlab = "Standardised mean difference \n(with heteroscedastic population variances)")+
    labs(x='Antibiotic resistance gene targetting in qPCR assay')+
    scale_x_discrete(labels = xticklabs_new)+
    scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
    ggplot2::scale_colour_manual(values = cbpl_temp)+
    ggplot2::scale_fill_manual(values = cbpl_temp)+
    theme_plots()
  
}
)

#read in legend
legend_allstudies_qPCRonly<-readRDS('4_figures/legend_allstudies_qPCRonly.RDS')

#combine into one plot with legend with patchwork
library(patchwork)
orchardplot<-orchardplot+legend_allstudies_qPCRonly+patchwork::plot_layout(widths = c(12,4))
orchardplot

ggsave('4_figures/alternative_analyses/qPCRgenes/orchardplot_qPCRgenes.tiff', plot=last_plot(), width=16, height=6)
saveRDS(orchardplot,'4_figures/alternative_analyses/qPCRgenes/orchardplot_qPCRgenes.RDS')

