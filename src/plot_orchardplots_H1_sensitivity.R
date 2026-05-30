library(orchaRd)
library(ggplot2)
library(metafor)

#grDevices::tiff('4_figures/forestplots.tiff', res=300, units='in', width=6, height=8)
source('functions/load_plotdefaults.R')

# READ IN SENSITIVITY ANALYSES MODELS -------------------------------------

mode_period_hypothesis<-c('during_H1_model','after_H1_model')

for (mph in 1:length(mode_period_hypothesis)){
  
  print(mph)
  
  #read in original model
  #injection post
  #original_model<-readRDS(paste('3_models/hypothesis_1/',mode_period_hypothesis[mph],'.RDS',sep=''))
  
  # #initiate a list with the original model
  #H1_models_list<-list(original_model)
  #name first element as original model
  #names(H1_models_list)[1]<-mode_period_hypothesis[mph]
  
  H1_models_list<-list()
  
  #create vector of the associated sensitivity analyses models
  sensitivity_H1_files<-list.files('3_models/hypothesis_1/sensitivity', pattern=mode_period_hypothesis[mph], full.names = T)
  #remove all the I2 files
  sensitivity_H1_files<-sensitivity_H1_files[grep('_I2', sensitivity_H1_files, invert=T)]
  
  #remove the varcovar ones from this as they need to be plotted differently/separately
  #sensitivity_H1_files<-sensitivity_H1_files[grep('varcov',sensitivity_H1_files, invert=T)]
  
  #loop through H1 models and bind to list 
  for (f in 1:length(sensitivity_H1_files)) {
    
    #read in current model
    mod_temp<-readRDS(sensitivity_H1_files[f])
    
    #bind to list
    H1_models_list[[f]]<-mod_temp
    
    sensitivity_analysis_name<-
      gsub(pattern = "(.*/)(.*)(.RDS.*)",
           replacement = "\\2",
           x = sensitivity_H1_files[f])
    
    #rename
    names(H1_models_list)[f]<-sensitivity_analysis_name
    
    #create new model
    assign(paste(mode_period_hypothesis[mph],'s_list',sep=''),
           H1_models_list)
    }
}

during_ugly_to_pretty<-c(
                        during_H1_model_correct_SCNI = 'Corrected for \n shared control \n non-independence',
                        during_H1_model_leaveout_Lethbridge_Chlortetsul = 'Leave out Lethbridge_Chlortetsul',
                        during_H1_model_leaveout_Lethbridge_Tyl = 'Leave out _Lethbridge_Tyl',
                        `during_H1_model_leaveout_TexasA&M_Tyl` = 'Leave out TexasA&M_Tyl',
                        `during_H1_model_leaveout_TexasA&M_TylTylprobiotic` = 'Leave out TexasA&M_TylTylprobiotic',
                        during_H1_model_leaveout_USMARC_Tyl = 'Leave out USMARC_Tyl',
                        during_H1_model_leaveout_USMARC_Tylmon = 'Leave out USMARC_Tylmon',
                        during_H1_model_leaveout_WKU_Tyl = 'Leave out WKU_Tyl',
                        during_H1_model_remove_extracted = 'Removed studies for which we \n had to extract data',
                        during_H1_model_remove_ignoredblock = 'Removed studies for which we were \n not able to account for blocking',
                        during_H1_model_remove_ignoredpen = 'Removed studies for which we were \n not able to account for pen',
                        during_H1_model_remove_nonrandomised   = 'Removed studies that \n  were not randomised \n (or not correctly randomised)',
                        during_H1_model_remove_simulatedproportions = 'Removed studies for which we \n had to simulate proportion data',
                        during_H1_model_varcov_0.0 = 'cov = 0',
                        during_H1_model_varcov_0.2 = 'cov = 0.2',
                        during_H1_model_varcov_0.4 = 'cov = 0.4',
                        during_H1_model_varcov_0.6 = 'cov = 0.6',
                        during_H1_model_varcov_0.8 = 'cov = 0.8'
                        )

during_xlabs_new<-as.character(
  during_ugly_to_pretty[match(names(during_H1_models_list),
                       names(during_ugly_to_pretty))])

after_ugly_to_pretty<-c(
                       after_H1_model_correct_SCNI = 'Corrected for \n shared control \n non-independence',
                       after_H1_model_leaveout_IowaState_Dano = 'Leave out IowaState_Dano',
                       after_H1_model_leaveout_Lethbridge_Chlortetsul = 'Leave out Lethbridge_Chlortetsul',
                       after_H1_model_leaveout_Lethbridge_Tyl = 'Leave out _Lethbridge_Tyl',
                       `after_H1_model_leaveout_TexasA&M_Cef` = 'Leave out TexasA&M_Cef',
                       `after_H1_model_leaveout_TexasA&M_CefCefchlortet` = 'Leave out TexasA&M_CefCefchlortet',
                       `after_H1_model_leaveout_TexasA&M_CefTul` = 'Leave out TexasA&M_CefTul',
                       `after_H1_model_leaveout_TexasA&M_TylTylprobiotic` = 'Leave out TexasA&M_TylTylprobiotic',
                       after_H1_model_leaveout_USMARC_Chlortet = 'Leave out USMARC_Chlortet',
                       after_H1_model_remove_extracted = 'Removed studies for which we \n had to extract data',
                       after_H1_model_remove_ignoredblock = 'Removed studies for which we were \n not able to account for blocking',
                       after_H1_model_remove_ignoredpen = 'Removed studies for which we were \n not able to account for pen',
                       after_H1_model_remove_nonrandomised   = 'Removed studies that were not randomised \n (or not correctly randomised)',
                       after_H1_model_remove_simulatedproportions = 'Removed studies for which we \n had to simulate proportion data',
                       after_H1_model_varcov_0.0 = 'cov = 0',
                       after_H1_model_varcov_0.2 = 'cov = 0.2',
                       after_H1_model_varcov_0.4 = 'cov = 0.4',
                       after_H1_model_varcov_0.6 = 'cov = 0.6',
                       after_H1_model_varcov_0.8 = 'cov = 0.8'
                    )

after_xlabs_new<-as.character(
  after_ugly_to_pretty[match(names(after_H1_models_list),
                       names(after_ugly_to_pretty))])


xlabs_new<-list(during_xlabs_new,after_xlabs_new)

# MAKE LISTS OF MODEL RESULTS  --------------------------------------------

#make list of model results
H1_modelresults_list<-lapply(H1_models_list,
                             function(x){orchaRd::mod_results(model=x,group='study_studyID')}
)

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)

# CREATE OVERALL ORCHARD PLOTS FOR EACH OF THE MODELS ---------------------

sets<-list(during_H1_models_list,after_H1_models_list)
names(sets)<-c('during','after')

#loop through the two sets based on mode of administration
for (s in 1:length(sets)){
print(s)
#loop through H1 models and bind to list 
for (m in 1:length(sets[[s]])){
print(m)

  #subset the microshades per study palette to just the studies included in the current model/sub-dataset
  cbpl_temp<-microshades_cvd[match(unique(sets[[s]][[m]]$data$study_studyID), names(microshades_cvd))]
  
  #read in current model
  mod_temp<-sets[[s]][[m]]
  
  #get CI and PI estimates from model
  estimates<-orchaRd::mod_results(mod_temp,group='study_studyID')$mod_table
  
  #round for presentation
  estimates[,-1]<-round(estimates[,-1], 2)
  
  #add p value to estimates dataframe
  estimates$pval<-mod_temp$pval

#intercept-only plot (caterpillar-like orchard plot)

  #refit model
  tryCatch(
overallplot<-
  orchaRd::orchard_plot(mod_temp,
                        group = "study_studyID",
                        flip = F,
                        g = T,
                        colour = T,
                        k.pos = 1.6,
                        legend.pos = "none",
                        xlab = "Standardised mean difference \n(with heteroscedastic population variances)")+
  scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
  scale_x_discrete(labels=c("Overall estimate \n(intercept-only model)"))+
  ggplot2::scale_colour_manual(values = cbpl_temp)+
  ggplot2::scale_fill_manual(values = cbpl_temp)+
  annotate("text", size=2.5, x=0.45, y=2.5, hjust = 0,
           label=paste("Estimate = ", estimates[,2],'\n',
                       "95% CI = ", estimates[,3],' to ',estimates[,4],'\n',
                       "95% PI = ", estimates[,5],' to ',estimates[,6],'\n',
                       "p = ", format.pval(estimates$pval, eps = 0.01),
                       sep=''))+
  theme_plots()
)

#relabel with name of sensitivity analysis
overallplot<-overallplot+
    scale_x_discrete(labels=c(xlabs_new[[s]][m]))

#if coefficint is same it's because no sens analysis was poss
if (any(mod_temp$b%in%c(during_H1_model$b,after_H1_model$b))){
  
  #remove everything but bottom layer
  overallplot<-gginnards::delete_layers(overallplot,idx = c(1,2,3,4,5,6))
  
  text<-"No removals \n or \n corrections \n to make"
  overallplot<-
    overallplot+
    annotate("text", x = 1, y = 0.5, size=4, label = text)

  #add label back in (have to add limits=factor(1) to get it to work with blank plot)
  overallplot<-overallplot+
    scale_x_discrete(limits=factor(1), labels=c(xlabs_new[[s]][m]))
  
  overallplot
}

assign(paste0(names(sets[[s]])[m], '_orchardplot'), overallplot)
  }
}

# PLOT PANELS -------------------------------------------------------------

legend_duringstudies<-readRDS('4_figures/legend_duringstudies.RDS')
legend_afterstudies<-readRDS('4_figures/legend_afterstudies.RDS')

library(patchwork)

# #publication bias
# plot(during_H1_model_orchardplot+
# during_H1_model_publicationbias_timelag_orchardplot+
# plot_layout(ncol = 2))

#ggsave('4_figures/hypothesis_1/sensitivity/during_H1_model_orchardplot_sensitivityanalyses_publicationbias.tiff', plot=last_plot(), width=8, height=4)

# #publication bias
# plot(after_H1_model_orchardplot+
#        after_H1_model_publicationbias_srin_orchardplot+
#        after_H1_model_publicationbias_timelag_orchardplot+
#        plot_layout(ncol = 3))
# 
# ggsave('4_figures/hypothesis_1/sensitivity/after_H1_model_orchardplot_sensitivityanalyses_publicationbias.tiff', plot=last_plot(), width=12, height=4)

## ALL OTHER -----------------

layout <- "ABC
           DEF"

plot(during_H1_model_remove_nonrandomised_orchardplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     during_H1_model_remove_extracted_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     during_H1_model_remove_ignoredpen_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     during_H1_model_remove_ignoredblock_orchardplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     during_H1_model_correct_SCNI_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     legend_duringstudies+
     plot_layout(design=layout))

ggsave('4_figures/hypothesis_1/sensitivity/during_H1_model_orchardplot_sensitivityanalyses_allother.tiff', plot=last_plot(), width=10, height=8)

plot(after_H1_model_remove_nonrandomised_orchardplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     after_H1_model_remove_extracted_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     after_H1_model_remove_ignoredpen_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     after_H1_model_remove_ignoredblock_orchardplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     after_H1_model_correct_SCNI_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     legend_afterstudies+
       plot_layout(design=layout))

ggsave('4_figures/hypothesis_1/sensitivity/after_H1_model_orchardplot_sensitivityanalyses_allother.tiff', plot=last_plot(), width=10, height=8)

## LEAVE1OUT -----------------

layout <- "ABCG
           DEFH"

during_H1_model_leaveout_Lethbridge_Chlortetsul_orchardplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
during_H1_model_leaveout_Lethbridge_Tyl_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
during_H1_model_leaveout_USMARC_Tyl_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
during_H1_model_leaveout_USMARC_Tylmon_orchardplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
`during_H1_model_leaveout_TexasA&M_TylTylprobiotic_orchardplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
`during_H1_model_leaveout_TexasA&M_Tyl_orchardplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
during_H1_model_leaveout_WKU_Tyl_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
legend_duringstudies+
plot_layout(design=layout)

ggsave('4_figures/hypothesis_1/sensitivity/during_H1_model_orchardplot_sensitivityanalyses_leave1out.tiff', plot=last_plot(), width=14, height=8)


#after_H1_model_leaveout_IowaState_Dano_orchardplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
after_H1_model_leaveout_USMARC_Chlortet_orchardplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
after_H1_model_leaveout_Lethbridge_Chlortetsul_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
after_H1_model_leaveout_Lethbridge_Tyl_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
`after_H1_model_leaveout_TexasA&M_TylTylprobiotic_orchardplot`+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
`after_H1_model_leaveout_TexasA&M_Cef_orchardplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
`after_H1_model_leaveout_TexasA&M_CefTul_orchardplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
`after_H1_model_leaveout_TexasA&M_CefCefchlortet_orchardplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
legend_afterstudies+
plot_layout(design=layout)

ggsave('4_figures/hypothesis_1/sensitivity/after_H1_model_orchardplot_sensitivityanalyses_leave1out.tiff', plot=last_plot(), width=14, height=8)

## VARCOVAR -----------------

layout <- "ABCDEE"

#varcovar
plot(during_H1_model_varcov_0.2_orchardplot+
    during_H1_model_varcov_0.4_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
    during_H1_model_varcov_0.6_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
    during_H1_model_varcov_0.8_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
    legend_duringstudies+
    plot_layout(design=layout))

ggsave('4_figures/hypothesis_1/sensitivity/during_H1_model_orchardplot_sensitivityanalyses_varcovar.tiff', plot=last_plot(), width=14, height=4.5)

#varcovar
plot(after_H1_model_varcov_0.2_orchardplot+
     after_H1_model_varcov_0.4_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     after_H1_model_varcov_0.6_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     after_H1_model_varcov_0.8_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
     legend_afterstudies+
     plot_layout(design=layout))

ggsave('4_figures/hypothesis_1/sensitivity/after_H1_model_orchardplot_sensitivityanalyses_varcovar.tiff', plot=last_plot(), width=14, height=4.5)
