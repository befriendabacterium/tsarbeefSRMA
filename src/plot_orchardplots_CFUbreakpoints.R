source('functions/plot_bubbleplots_H2_supplementaryfunctions.R')
source('functions/load_plotdefaults.R')

library(ggplot2)
library(metafor)
library(dplyr)
library(orchaRd)

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(model_CFUantibiotics$data$study_studyID), names(microshades_cvd))]

# STUDY BREAKPOINT (ABSOLUTE) ---------------------------------------------

model_CFUbreakpoints<-readRDS('3_models/alternative_analyses/CFUbreakpoints/subalternative_model_CFUstudybreakpoint.RDS')
model_CFUbreakpoints_I2<-readRDS('3_models/alternative_analyses/CFUbreakpoints/subalternative_I2_CFUstudybreakpoint.RDS')
#model_CFUbreakpoints_R2<-readRDS('3_models/alternative_analyses/CFUbreakpoints/model_CFUstudybreakpoint_R2.RDS')

tryCatch(
  bubbleplot<-bubble_plot_MLJ(object = model_CFUbreakpoints,
                              mod = "outcome_resistance_studybreakpointmgL",
                              by = NULL,
                              group = "study_studyID",
                              g=T,
                              k.pos = 'bottom.right',
                              xlab = "Resistance breakpoint used in study (mg/L)",
                              ylab = "Standardised mean difference \n(with heteroscedastic population variances)")+
                              ggplot2::geom_hline(yintercept = 0, 
                                                    linetype = 2, colour = "black", alpha=0.5)+
                              coord_cartesian(xlim = c(-0.25,2.25))+
                              ggplot2::scale_x_continuous(breaks = c(0,1,2), labels = 10^(c(0,1,2)), minor_breaks = NULL)+
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
      bubble_plot_MLJ(object = model_CFUbreakpoints$data,
                      yi = "yi_totalresistancedeterminants",
                      vi = "vi_totalresistancedeterminants",
                      stdy = "study_studyID",
                      mod = "outcome_resistance_studybreakpointmgL",
                      by = NULL,
                      group = "study_studyID",
                      g=T,
                      k.pos = 'bottom.right',
                      xlab = "Resistance breakpoint used in study (mg/L)",
                      ylab = "Standardised mean difference \n(with heteroscedastic population variances)")+
      ggplot2::geom_hline(yintercept = 0, 
                          linetype = 2, colour = "black", alpha=0.5)+
      coord_cartesian(xlim = c(-0.25,2.25))+
      ggplot2::scale_x_continuous(breaks = c(0,1,2), labels = 10^(c(0,1,2)), minor_breaks = NULL)+
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
  }
)


if(length(model_CFUbreakpoints$b)==2){
  bubbleplot<-
    bubbleplot+
    annotate("text", size=3.5, x=-0.25, y=2.5, hjust=0,
             label=paste("Slope = ", round(model_CFUbreakpoints$b[2],2),'\n',
                         "95% CI = ", round(model_CFUbreakpoints$ci.ub[2],2),' to ', round(model_CFUbreakpoints$ci.ub[2],2),'\n',
                         #"95% PI = ", estimates[,5],' to ',estimates[,6],'\n',
                         "p = ", format.pval(model_CFUbreakpoints$pval[2], eps = 0.01, digits = 2),
                         sep=''))
}

#read in legend
legend_allstudies_CFUonly<-readRDS('4_figures/legend_allstudies_CFUonly.RDS')

#combine into one plot with legend with patchwork
library(patchwork)
bubbleplot<-bubbleplot+legend_allstudies_CFUonly+patchwork::plot_layout(widths = c(8,5))
bubbleplot

ggsave('4_figures/alternative_analyses/CFUbreakpoints/bubbleplot_CFUstudybreakpoints.tiff', plot=last_plot(), width=12, height=6)
saveRDS(bubbleplot,'4_figures/alternative_analyses/CFUbreakpoints/bubbleplot_CFUstudybreakpoints.RDS')

# STUDY BREAKPOINT (RELATIVE TO CLSI) ---------------------------------------------

model_CFUbreakpointsdiff<-readRDS('3_models/alternative_analyses/CFUbreakpoints/subalternative_model_CFUbreakpointsdiff.RDS')
I2_CFUbreakpointsdiff<-readRDS('3_models/alternative_analyses/CFUbreakpoints/subalternative_I2_CFUbreakpointsdiff.RDS')
#model_CFUbreakpointsdiff_R2<-readRDS('3_models/alternative_analyses/CFUbreakpoints/model_CFUbreakpointsdiff_R2.RDS')

tryCatch(
  bubbleplot<-bubble_plot_MLJ(object = model_CFUbreakpointsdiff,
                              mod = "outcome_resistance_studyCLSIbreakpointdiff",
                              by = NULL,
                              group = "study_studyID",
                              g=T,
                              k.pos = 'bottom.right',
                              xlab = "Relative difference between resistance breakpoint used in study and CLSI 'Resistance' breakpoint \n (how many times more than the CLSI breakpoint the study breakpoint is)",
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
      bubble_plot_MLJ(object = model_CFUbreakpointsdiff$data,
                      yi = "yi_totalresistancedeterminants",
                      vi = "vi_totalresistancedeterminants",
                      mod = "outcome_resistance_studyCLSIbreakpointdiff",
                      stdy = "study_studyID",
                      group = "study_studyID",
                      g=T,
                      k.pos = 'bottom.right',
                      xlab = "Relative difference between resistance breakpoint used in study and CLSI 'Resistance' breakpoint \n (how many times more than the CLSI breakpoint the study breakpoint is)",
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
    
  }
)

if(length(model_CFUbreakpointsdiff$b)==2){
  bubbleplot<-
    bubbleplot+
    annotate("text", size=3.5, x=-0.25, y=2.5, hjust=0,
             label=paste("Slope = ", round(model_CFUbreakpointsdiff$b[2],2),'\n',
                         "95% CI = ", round(model_CFUbreakpointsdiff$ci.ub[2],2),' to ', round(model_CFUbreakpointsdiff$ci.ub[2],2),'\n',
                         #"95% PI = ", estimates[,5],' to ',estimates[,6],'\n',
                         "p = ", format.pval(model_CFUbreakpointsdiff$pval[2], eps = 0.01, digits = 2),
                         sep=''))
}

#read in legend
legend_allstudies_CFUonly<-readRDS('4_figures/legend_allstudies_CFUonly.RDS')

#combine into one plot with legend with patchwork
library(patchwork)
bubbleplot<-bubbleplot+legend_allstudies_CFUonly+patchwork::plot_layout(widths = c(8,5))
bubbleplot

ggsave('4_figures/alternative_analyses/CFUbreakpoints/bubbleplot_CFUstudyCLSIbreakpointdiff.tiff', plot=last_plot(), width=12, height=6)
saveRDS(bubbleplot,'4_figures/alternative_analyses/CFUbreakpoints/CFUstudyCLSIbreakpointdiff.RDS')

# STUDY BREAKPOINT (RELATIVE TO CLSI - CATEGORICAL) ---------------------------------------------

model_CFUstudybreakpointtype<-readRDS('3_models/alternative_analyses/CFUbreakpoints/subsubgroup_model_CFUstudybreakpointtype.RDS')
I2_CFUstudybreakpointtype<-readRDS('3_models/alternative_analyses/CFUbreakpoints/subsubgroup_I2_CFUstudybreakpointtype.RDS')
#model_CFUstudybreakpointtype_R2<-readRDS('3_models/alternative_analyses/CFUbreakpoints/model_CFUstudybreakpointtype_R2.RDS')

xlabticks_new<-levels(model_CFUstudybreakpointtype$data$outcome_resistance_studybreakpointtype)
plottingorder<-orchaRd::firstup(xlabticks_new)

desired_order<-match(c('No CLSI/NARMS breakpoint',
                       'Below CLSI/NARMS breakpoint \n (i.e. intermediate/ \n susceptible dose-dependent)',
                       'CLSI/NARMS breakpoint',
                       'Above CLSI/NARMS breakpoint'),
                     levels(model_CFUstudybreakpointtype$data$outcome_resistance_studybreakpointtype))

desired_order<-desired_order[!is.na(desired_order)]

#apply order
xlabticks_new<-xlabticks_new[desired_order]
plottingorder<-plottingorder[desired_order]

#get CI and PI estimates from model
estimates<-orchaRd::mod_results(model_CFUstudybreakpointtype,
                                mod = as.character(model_CFUstudybreakpointtype$formula.mods[[2]][[3]]), 
                                group='study_studyID')$mod_table

#round for presentation
estimates[,-1]<-round(estimates[,-1], 2)
#reorder into desired order
estimates<-estimates[desired_order,]
#add p value to estimates dataframe
estimates$pval<-model_CFUstudybreakpointtype$pval

tryCatch(
  orchardplot<-orchard_plot_MLJ(object = model_CFUstudybreakpointtype,
                              mod = "outcome_resistance_studybreakpointtype",
                              group = "study_studyID",
                              flip = F,
                              g = T,
                              colour = T,
                              k.pos = -1.5,
                              legend.pos = "none",
                              xlab = "Standardised mean difference \n(with heteroscedastic population variances)",
                              tree.order = plottingorder)+
    labs(x="Type of resistance breakpoint used")+
    scale_x_discrete(labels=c(xlabticks_new))+
    scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
    ggplot2::scale_colour_manual(values = cbpl_temp)+
    ggplot2::scale_fill_manual(values = cbpl_temp)+
    annotate("text", size=3.25, x=1:nrow(estimates), y=2.75, 
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
      orchard_plot_MLJ(object = model_CFUstudybreakpointtype$data,
                      yi = "yi_totalresistancedeterminants",
                      vi = "vi_totalresistancedeterminants",
                      stdy = "study_studyID",
                      mod = "outcome_resistance_studybreakpointtype",
                      group = "study_studyID",
                      flip = F,
                      g = T,
                      colour = T,
                      k.pos = -1.5,
                      legend.pos = "none",
                      xlab = "Standardised mean difference \n(with heteroscedastic population variances)",
                      tree.order = plottingorder)+
      labs(x="Type of resistance breakpoint used")+
      scale_x_discrete(labels=c(xlabticks_new))+
      scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
      ggplot2::scale_colour_manual(values = cbpl_temp)+
      ggplot2::scale_fill_manual(values = cbpl_temp)+
      theme_plots()
    
  }
)

#read in legend
legend_allstudies_CFUonly<-readRDS('4_figures/legend_allstudies_CFUonly.RDS')

#combine into one plot with legend with patchwork
library(patchwork)
orchardplot<-orchardplot+legend_allstudies_CFUonly+patchwork::plot_layout(widths = c(8,5))
orchardplot

ggsave('4_figures/alternative_analyses/CFUbreakpoints/orchardplot_CFUstudybreakpointtype.tiff', plot=last_plot(), width=12, height=6)
saveRDS(orchardplot,'4_figures/alternative_analyses/CFUbreakpoints/CFUstudybreakpointtype.RDS')
