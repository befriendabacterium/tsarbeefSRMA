library(orchaRd)
library(ggplot2)
library(metafor)

source('functions/load_plotdefaults.R')

during_H2_model<-readRDS('3_models/hypothesis_2/H2_model_during_totalresistancedeterminants_timelinear.RDS')
after_H2_model<-readRDS('3_models/hypothesis_2/H2_model_after_totalresistancedeterminants_timercs.RDS')

#grDevices::tiff('4_figures/forestplots.tiff', res=300, units='in', width=6, height=8)

# READ IN SENSITIVITY ANALYSES MODELS -------------------------------------

mode_period_hypothesis<-c('during_H2_model','after_H2_model')
#mode_period_hypothesis2<-c('during_H2_model','after_H2_model')

for (mph in 1:length(mode_period_hypothesis)){
  
  print(mph)
  
  #read in original model
  #injection post
  #original_model<-readRDS(paste('3_models/hypothesis_2/',mode_period_hypothesis[mph],'.RDS',sep=''))
  
  # #initiate a list with the original model
  #H2_models_list<-list(original_model)
  #name first element as original model
  #names(H2_models_list)[1]<-mode_period_hypothesis2[mph]
  
  H2_models_list<-list()
  
  #create vector of the associated sensitivity analyses models
  sensitivity_H2_files<-list.files('3_models/hypothesis_2/sensitivity/', pattern=mode_period_hypothesis[mph], full.names = T)
  #remove all the I2 files
  sensitivity_H2_files<-sensitivity_H2_files[grep('_I2', sensitivity_H2_files, invert=T)]
  
  #remove the varcovar ones from this as they need to be plotted differently/separately
  #sensitivity_H2_files<-sensitivity_H2_files[grep('varcov',sensitivity_H2_files, invert=T)]
  
  #loop through H2 models and bind to list 
  for (f in 1:length(sensitivity_H2_files)) {
    
    #read in current model
    mod_temp<-readRDS(sensitivity_H2_files[f])
    
    #bind to list
    H2_models_list[[f]]<-mod_temp
    
    sensitivity_analysis_name<-
      gsub(pattern = "(.*/)(.*)(.RDS.*)",
           replacement = "\\2",
           x = sensitivity_H2_files[f])
    
    #rename
    names(H2_models_list)[f]<-sensitivity_analysis_name
    
    #create new model
    assign(paste(mode_period_hypothesis[mph],'s_list',sep=''),
           H2_models_list)
  }
}


during_ugly_to_pretty<-c(
  during_H2_model_correct_SCNI = 'Corrected for \n shared control \n non-independence',
  during_H2_model_leaveout_Lethbridge_Chlortetsul = 'Leave out Lethbridge_Chlortetsul',
  during_H2_model_leaveout_Lethbridge_Tyl = 'Leave out _Lethbridge_Tyl',
  `during_H2_model_leaveout_TexasA&M_Tyl` = 'Leave out TexasA&M_Tyl',
  `during_H2_model_leaveout_TexasA&M_TylTylprobiotic` = 'Leave out TexasA&M_TylTylprobiotic',
  during_H2_model_leaveout_USMARC_Tyl = 'Leave out USMARC_Tyl',
  during_H2_model_leaveout_USMARC_Tylmon = 'Leave out USMARC_Tylmon',
  during_H2_model_leaveout_WKU_Tyl = 'Leave out WKU_Tyl',
  during_H2_model_remove_extracted = 'Removed studies for which we \n had to extract data \n from publications',
  during_H2_model_remove_ignoredblock = 'Removed studies for which we were \n not able to account for blocking',
  during_H2_model_remove_ignoredpen = 'Removed studies for which we were \n not able to account for pen',
  during_H2_model_remove_nonrandomised   = 'Removed studies that \n  were not randomised \n (or not correctly randomised)',
  during_H2_model_remove_simulatedproportions = 'Removed studies for which we \n had to simulate proportion data',
  during_H2_model_varcov_0.0 = 'cov = 0',
  during_H2_model_varcov_0.2 = 'cov = 0.2',
  during_H2_model_varcov_0.4 = 'cov = 0.4',
  during_H2_model_varcov_0.6 = 'cov = 0.6',
  during_H2_model_varcov_0.8 = 'cov = 0.8'
)

during_xlabs_new<-as.character(
  during_ugly_to_pretty[match(names(during_H2_models_list),
                              names(during_ugly_to_pretty))])

after_ugly_to_pretty<-c(
  after_H2_model_correct_SCNI = 'Corrected for \n shared control \n non-independence',
  after_H2_model_leaveout_IowaState_Dano = 'Leave out IowaState_Dano',
  after_H2_model_leaveout_Lethbridge_Chlortetsul = 'Leave out Lethbridge_Chlortetsul',
  after_H2_model_leaveout_Lethbridge_Tyl = 'Leave out _Lethbridge_Tyl',
  `after_H2_model_leaveout_TexasA&M_Cef` = 'Leave out TexasA&M_Cef',
  `after_H2_model_leaveout_TexasA&M_CefCefchlortet` = 'Leave out TexasA&M_CefCefchlortet',
  `after_H2_model_leaveout_TexasA&M_CefTul` = 'Leave out TexasA&M_CefTul',
  `after_H2_model_leaveout_TexasA&M_TylTylprobiotic` = 'Leave out TexasA&M_TylTylprobiotic',
  after_H2_model_leaveout_USMARC_Chlortet = 'Leave out USMARC_Chlortet',
  after_H2_model_remove_extracted = 'Removed studies for which we \n had to extract data',
  after_H2_model_remove_ignoredblock = 'Removed studies for which we were \n not able to account for blocking',
  after_H2_model_remove_ignoredpen = 'Removed studies for which we were \n not able to account for pen',
  after_H2_model_remove_nonrandomised   = 'Removed studies that were not randomised \n (or not correctly randomised)',
  after_H2_model_remove_simulatedproportions = 'Removed studies for which we \n had to simulate proportion data \n from publications',
  after_H2_model_varcov_0.0 = 'cov = 0',
  after_H2_model_varcov_0.2 = 'cov = 0.2',
  after_H2_model_varcov_0.4 = 'cov = 0.4',
  after_H2_model_varcov_0.6 = 'cov = 0.6',
  after_H2_model_varcov_0.8 = 'cov = 0.8'
)

after_xlabs_new<-as.character(
  after_ugly_to_pretty[match(names(after_H2_models_list),
                             names(after_ugly_to_pretty))])


xlabs_new<-list(during_xlabs_new,after_xlabs_new)

# CREATE BUBBLE PLOTS FOR EACH OF THE MODELS ---------------------

source('functions/plot_bubbleplots_H2_supplementaryfunctions.R')

sets<-list(during_H2_models_list,after_H2_models_list)
names(sets)<-c('during','after')
xlabs<-c('Days of continuous antibiotic administration','Days since cessation of antibiotic administration')

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)

#loop through the two sets based period
for (s in 1:length(sets)){

#loop through H2 models and bind to list 
for (m in 1:length(sets[[s]])) {
  
print(paste('Period', names(sets)[s],', sensitivity analysis', names(sets[[s]])[m]))
  
#read in current model
mod_temp<-sets[[s]][[m]]

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(sets[[s]][[m]]$data$study_studyID), names(microshades_cvd))]

#extract its colours column
#cbpl_temp<-unique(during_model$data$plotting_colour)

bubbleplot<-
  bubble_plot_MLJ(mod_temp,
                  group = "study_studyID", 
                  mod = "outcome_time_days", 
                  g = T,
                  k.pos = 'bottom.right',
                  xlab =  xlabs_new[s],
                  ylab = "Standardised mean difference \n(with heteroscedastic population variances)")+
  ggplot2::geom_hline(yintercept = 0, 
                      linetype = 2, colour = "black", alpha=0.5)+
  coord_cartesian(xlim = log10(c(3,300))+c(-0.25,0.25))+ #expands limits whilst keeping consistent between plots https://stackoverflow.com/questions/42147636/how-to-keep-consistent-axes-scaling-in-a-grid-of-ggplot2-plots-with-different-c
  ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
  ggplot2::scale_colour_manual(values = cbpl_temp)+
  ggplot2::scale_fill_manual(values = cbpl_temp)+
  theme_plots()+
  theme(legend.position = "inside", 
        legend.justification = c(0.03, 1),
        legend.text=element_text(size=0.25),
        legend.direction = "horizontal",
        legend.background = ggplot2::element_blank())

#remove legend
bubbleplot<-bubbleplot+ggpubr::rremove('legend')+ggpubr::rremove('ylab')

if(length(mod_temp$b)==2){
  bubbleplot<-
    bubbleplot+
    annotate("text", size=3.5, x=0.25, y=2.5, hjust = 0,
             label=paste("Slope = ", round(mod_temp$b[2],2),'\n',
                         "95% CI = ", round(mod_temp$ci.ub[2],2),' to ', round(mod_temp$ci.ub[2],2),'\n',
                         #"95% PI = ", estimates[,5],' to ',estimates[,6],'\n',
                         "p = ", format.pval(mod_temp$pval[2], eps = 0.01, digits = 2),
                         sep=''))
}

#if coefficient is same it's because no sens analysis was poss
if (any(mod_temp$b%in%c(during_H2_model$b,after_H2_model$b))){
  text<-"No removals \n or \n corrections \n to make"
  bubbleplot<-
    bubbleplot+
    annotate("text", x = 1.5, y = 0, size=4, label = text)
  
  #remove everything but bottom layer
  bubbleplot<-gginnards::delete_layers(bubbleplot,idx = 1:gginnards::top_layer(bubbleplot)-1)
  
}

#add annotation for what sensitivity analysis it was
bubbleplot<-bubbleplot+
  coord_cartesian(xlim = log10(c(3,300))+c(-0.25,0.25))+ #expands limits whilst keeping consistent between plots https://stackoverflow.com/questions/42147636/how-to-keep-consistent-axes-scaling-in-a-grid-of-ggplot2-plots-with-different-c
  theme(axis.title.x=element_blank())+
  scale_x_continuous(breaks = log10(c(3,10,30,100,300)), labels=c(3,10,30,100,300))+
  ggplot2::annotate("text", y = -2.5, x = 0.25, label = xlabs_new[[s]][m], 
                    parse = FALSE, hjust = "left", size = 3.5)  



# if (m!=5){
# bubbleplot<-bubbleplot+
#         ggpubr::rremove('xlab')+
#         ggpubr::rremove('x.text')+
#         ggpubr::rremove('x.ticks')
# }
assign(paste0(names(sets[[s]])[m], '_bubbleplot'), bubbleplot)

  }
}

#weird bug where sens model not producing exaaaactly same result even when same data (hence round to 5 dp)

# PLOT PANELS -------------------------------------------------------------

library(patchwork)

## ALL OTHER -----------------

layout <- "ABC
           DEF"

plot(during_H2_model_remove_nonrandomised_bubbleplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       during_H2_model_remove_extracted_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       during_H2_model_remove_ignoredpen_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       during_H2_model_remove_ignoredblock_bubbleplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       during_H2_model_correct_SCNI_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       legend_duringstudies+
       plot_layout(design=layout))

ggsave('4_figures/hypothesis_2/sensitivity/during_H2_model_bubbleplot_sensitivityanalyses_allother.tiff', plot=last_plot(), width=12, height=8)

plot(after_H2_model_remove_nonrandomised_bubbleplot+#+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       after_H2_model_remove_extracted_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       after_H2_model_remove_ignoredpen_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       after_H2_model_remove_ignoredblock_bubbleplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       after_H2_model_correct_SCNI_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       legend_afterstudies+
       plot_layout(design=layout))

ggsave('4_figures/hypothesis_2/sensitivity/after_H2_model_bubbleplot_sensitivityanalyses_allother.tiff', plot=last_plot(), width=12, height=8)

## LEAVE1OUT -----------------

layout <- "ABCG
           DEFH"

during_H2_model_leaveout_Lethbridge_Chlortetsul_bubbleplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  during_H2_model_leaveout_Lethbridge_Tyl_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  during_H2_model_leaveout_USMARC_Tyl_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  during_H2_model_leaveout_USMARC_Tylmon_bubbleplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  `during_H2_model_leaveout_TexasA&M_TylTylprobiotic_bubbleplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  `during_H2_model_leaveout_TexasA&M_Tyl_bubbleplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  during_H2_model_leaveout_WKU_Tyl_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  legend_duringstudies+
  plot_layout(design=layout)

ggsave('4_figures/hypothesis_2/sensitivity/during_H2_model_bubbleplot_sensitivityanalyses_leave1out.tiff', plot=last_plot(), width=16, height=8)

#after_H2_model_leaveout_IowaState_Dano_bubbleplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
after_H2_model_leaveout_USMARC_Chlortet_bubbleplot+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  after_H2_model_leaveout_Lethbridge_Chlortetsul_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  after_H2_model_leaveout_Lethbridge_Tyl_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  `after_H2_model_leaveout_TexasA&M_TylTylprobiotic_bubbleplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  `after_H2_model_leaveout_TexasA&M_Cef_bubbleplot`+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  `after_H2_model_leaveout_TexasA&M_CefTul_bubbleplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  `after_H2_model_leaveout_TexasA&M_CefCefchlortet_bubbleplot`+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
  legend_afterstudies+
  plot_layout(design=layout)

ggsave('4_figures/hypothesis_2/sensitivity/after_H2_model_bubbleplot_sensitivityanalyses_leave1out.tiff', plot=last_plot(), width=16, height=8)

 ## VARCOVAR -----------------

layout <- "ABCDE"

#varcovar
plot(during_H2_model_varcov_0.2_bubbleplot+
       during_H2_model_varcov_0.4_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       during_H2_model_varcov_0.6_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       during_H2_model_varcov_0.8_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       legend_duringstudies+
       plot_layout(design=layout))

ggsave('4_figures/hypothesis_2/sensitivity/during_H2_model_bubbleplot_sensitivityanalyses_varcovar.tiff', plot=last_plot(), width=20, height=4.5)

#varcovar
plot(after_H2_model_varcov_0.2_bubbleplot+
       after_H2_model_varcov_0.4_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       after_H2_model_varcov_0.6_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       after_H2_model_varcov_0.8_bubbleplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
       legend_afterstudies+
       plot_layout(design=layout))

ggsave('4_figures/hypothesis_2/sensitivity/after_H2_model_bubbleplot_sensitivityanalyses_varcovar.tiff', plot=last_plot(), width=20, height=4.5)

# library(patchwork)
# 
# ## ALL OTHER -----------------
# 
# layout <-
#   "AABBGGG
#    CCDDGGG
#    EEFFGGG"
# 
# #allother
# plot(during_H2_model_correct_SCNI_orchardplot+
#        during_H2_model_remove_nonrandomised_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
#        during_H2_model_remove_extracted_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
#        during_H2_model_remove_simulatedproportions_orchardplot+
#        during_H2_model_remove_ignoredpen_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
#        during_H2_model_remove_ignoredblock_orchardplot+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+ggpubr::rremove('y.ticks')+
#        legend_duringstudies+
#        plot_layout(design=layout))
# 
# ggsave('4_figures/hypothesis_2/sensitivity/during_H2_model_bubbleplot_sensitivityanalyses_allother.tiff', plot=last_plot(), width=10, height=8)
# 
# #allother
# plot(after_H2_model_remove_observational_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        after_H2_model_remove_baselinediffs_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        after_H2_model_remove_extracted_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        after_H2_model_remove_simulatedproportions_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        after_H2_model_remove_peneffectunaccountedfor_bubbleplot+ggpubr::rremove('xlab')+#ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        after_H2_model_correct_SCNI_bubbleplot+ggpubr::rremove('xlab')+#ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        bubble_h2_after_legend+
#        plot_layout(design=layout))
# ggsave('4_figures/hypothesis_2/sensitivity/after_H2_model_bubbleplot_sensitivityanalyses_allother.tiff', plot=last_plot(), width=10, height=8)
# 
# ## LEAVE1OUT -----------------
# 
# layout <-
#   "AABBIII
#    CCDDIII
#    EEFFIII
#    GGHHIII"
# 
# #leave1out
# during_H2_model_leaveout_KansasState_Tyl_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   during_H2_model_leaveout_Lethbridge_Chlortetsul_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   during_H2_model_leaveout_Lethbridge_Tyl_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   during_H2_model_leaveout_Lethbridge_TyTulTil_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   `during_H2_model_leaveout_TexasA&M_CefChlortet_bubbleplot`+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   `during_H2_model_leaveout_TexasA&M_TylProbiotic_bubbleplot`+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   during_H2_model_leaveout_USMARC_Tyl_bubbleplot+ggpubr::rremove('xlab')+#ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#   during_H2_model_leaveout_WKU_Tyl_bubbleplot+ggpubr::rremove('xlab')+#ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#   bubble_h2_during_legend+
#   plot_layout(design=layout)
# 
# ggsave('4_figures/hypothesis_2/sensitivity/during_H2_model_bubbleplot_sensitivityanalyses_leave1out.tiff', plot=last_plot(), width=10, height=10)
# 
# #leave1out
#   after_H2_model_leaveout_IowaState_Dano_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#    after_H2_model_leaveout_Lethbridge_Tyl_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#    after_H2_model_leaveout_Lethbridge_Chlortetsul_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   `after_H2_model_leaveout_TexasA&M_Cef_bubbleplot`+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   `after_H2_model_leaveout_TexasA&M_CefChlortet_bubbleplot`+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   `after_H2_model_leaveout_TexasA&M_CefTul_bubbleplot`+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   `after_H2_model_leaveout_TexasA&M_TylProbiotic_bubbleplot`+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   after_H2_model_leaveout_USMARC_Chlortet_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+#ggpubr::rremove('x.ticks')+
#   bubble_h2_after_legend+
#   plot_layout(design=layout)
# ggsave('4_figures/hypothesis_2/sensitivity/after_H2_model_bubbleplot_sensitivityanalyses_leave1out.tiff', plot=last_plot(), width=10, height=12)
# 
# # VARCOVAR -----------------
# 
# layout <-
#   "AABBGG
#    CCDDGG"
# 
# #varcovar
# plot(during_H2_model_varcov_0.2_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        during_H2_model_varcov_0.4_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        during_H2_model_varcov_0.6_bubbleplot+ggpubr::rremove('xlab')+#ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        during_H2_model_varcov_0.8_bubbleplot+ggpubr::rremove('xlab')+#ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        bubble_h2_during_legend+
#        plot_layout(design = layout))
# ggsave('4_figures/hypothesis_2/sensitivity/during_H2_model_bubbleplot_sensitivityanalyses_varcovar.tiff', plot=last_plot(), width=12, height=8)
# 
# #varcovar
# plot(after_H2_model_varcov_0.2_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        after_H2_model_varcov_0.4_bubbleplot+ggpubr::rremove('xlab')+ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        after_H2_model_varcov_0.6_bubbleplot+ggpubr::rremove('xlab')+#ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        after_H2_model_varcov_0.8_bubbleplot+ggpubr::rremove('xlab')+#ggpubr::rremove('x.text')+ggpubr::rremove('x.ticks')+
#        bubble_h2_after_legend+
#        plot_layout(design = layout))
# ggsave('4_figures/hypothesis_2/sensitivity/after_H2_model_bubbleplot_sensitivityanalyses_varcovar.tiff', plot=last_plot(), width=12, height=8)
