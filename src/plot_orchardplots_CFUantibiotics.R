source('functions/plot_bubbleplots_H2_supplementaryfunctions.R')
source('functions/load_plotdefaults.R')

library(ggplot2)
library(metafor)
library(dplyr)
library(orchaRd)

# ANTIBIOTIC RESISTANCE TESTED FOR ----------------------------------------

model_CFUantibiotics<-readRDS('3_models/alternative_analyses/CFUantibiotics/subsubgroup_model_CFUantibiotics.RDS')
I2_CFUantibiotics<-readRDS('3_models/alternative_analyses/CFUantibiotics/subsubgroup_I2_CFUantibiotics.RDS')
#model_CFUantibiotics_R2<-readRDS('3_models/alternative_analyses/CFUantibiotics/R2_CFUantibiotics_R2.RDS')

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(model_CFUantibiotics$data$study_studyID), names(microshades_cvd))]

#add new xlabs (override capitalisation)
xticklabs_new<-levels(as.factor(model_CFUantibiotics$data$outcome_resistance_antibiotic))
plottingorder<-orchaRd::firstup(xticklabs_new)
xticklabs_new<-c('Ceftiofur \n (Extended-spectrum \n cephalosporin)',
                 'Ceftriaxone \n (Extended-spectrum \n cephalosporin)',
                 'Erthromycin \n (Macrolides, lincosamides \n & strepogramin B)',
                 'Tetracycline \n (Tetracycline)',
                 'Tylosin \n (Macrolides, lincosamides \n & strepogramin B)')

#option 1 - order by pooled effect size
#desired_order<-order(model_CFUantibiotics$b)

#option 2 - order by antibiotic class
desired_order<-match(c('tetracycline','erythromycin','tylosin','ceftiofur','ceftriaxone'),
                     levels(as.factor(model_CFUantibiotics$data$outcome_resistance_antibiotic)))

desired_order<-desired_order[!is.na(desired_order)]

#apply order
xticklabs_new<-xticklabs_new[desired_order]
plottingorder<-plottingorder[desired_order]

#get CI and PI estimates from model
estimates<-orchaRd::mod_results(model = model_CFUantibiotics,
                                mod = as.character(model_CFUantibiotics$formula.mods[[2]][[3]]),
                                group = 'study_studyID')$mod_table

#add p value to estimates dataframe
estimates$pval<-model_CFUantibiotics$pval

#round for presentation
estimates[,-1]<-round(estimates[,-1], 2)

#put in right order
estimates<-estimates[desired_order,]

tryCatch(
orchardplot<-orchard_plot_MLJ(object = model_CFUantibiotics,
                   mod = as.character(model_CFUantibiotics$formula.mods[[2]][[3]]),
                   by = NULL,
                   group = "study_studyID",,
                   flip = F,
                   g = T,
                   colour = T,
                   k.pos = -1.5,
                   legend.pos = "none",
                   tree.order = plottingorder, 
                   xlab = "Standardised mean difference \n(with heteroscedastic population variances)")+
  scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
  labs(x="Antibiotic class of intervention/outcome")+
  scale_x_discrete(labels=xticklabs_new)+
  ggplot2::scale_colour_manual(values = cbpl_temp)+
  ggplot2::scale_fill_manual(values = cbpl_temp)+
  annotate("text", size=3.5, x=1:nrow(estimates)-0.525, y=2.7, hjust=0, 
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
    orchard_plot_MLJ(object = model_CFUantibiotics$data,
                     yi = "yi_totalresistancedeterminants",
                     vi = "vi_totalresistancedeterminants",
                     stdy = "study_studyID",
                     mod = "outcome_resistance_antibiotic",
                     group = "study_studyID",,
                     flip = F,
                     g = T,
                     colour = T,
                     k.pos = -1.5,
                     legend.pos = "none",
                     tree.order = plottingorder, 
                     xlab = "Standardised mean difference \n(with heteroscedastic population variances)")+
    scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
    labs(x="Antibiotic class of intervention/outcome")+
    scale_x_discrete(labels=xticklabs_new)+
    ggplot2::scale_colour_manual(values = cbpl_temp)+
    ggplot2::scale_fill_manual(values = cbpl_temp)+
    theme_plots()
}
)

#read in legend
legend_allstudies_CFUonly<-readRDS('4_figures/legend_allstudies_CFUonly.RDS')

#combine into one plot with legend with patchwork
library(patchwork)
orchardplot<-orchardplot+legend_allstudies_CFUonly+patchwork::plot_layout(widths = c(14,4))
orchardplot

ggsave('4_figures/alternative_analyses/CFUantibiotics/orchardplot_CFUantibiotics.tiff', plot=last_plot(), width=16, height=6)
saveRDS(orchardplot,'4_figures/alternative_analyses/CFUantibiotics/orchardplot_CFUantibiotics.RDS')
