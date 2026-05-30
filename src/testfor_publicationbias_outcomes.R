#adapted from
#https://itchyshin.github.io/publication_bias/#S424:_Adjusting_overall_effect_size_estimates_for_lnRR_and_SMD

#https://stackoverflow.com/questions/38011885/can-i-convert-a-base-plot-in-r-to-a-ggplot-object
#install.packages('ggplotify')
library(ggplotify)
library(patchwork)

# LOAD FUNCTIONS ----------------------------------------------------------

# custom function for extracting mean and CI from each metafor model
estimates.CI <- function(model){
  db.mf <- data.frame(model$b,row.names = 1:nrow(model$b))
  db.mf <- cbind(db.mf,model$ci.lb,model$ci.ub,row.names(model$b))
  names(db.mf) <- c("mean","lower","upper","estimate")
  return(db.mf[,c("estimate","mean","lower","upper")])
}

# custom function for extracting mean and CI for emmeans (marginalized means) 
estimates.CI2 <- function(res){
  db.mf <- data.frame(summary(res)[,2],row.names = 1:length( summary(res)[,2]))
  db.mf <- cbind(db.mf,summary(res)[,5],summary(res)[,6],names(res@levels),summary(res)[,1])
  names(db.mf) <- c("mean","lower","upper","estimate_name","estimate")
  return(db.mf[,c("estimate_name","estimate","mean","lower","upper")])
}

estimates.CI3 <- function(res, newxs, x_name, knots){
  db.mf <-as.data.frame(predict(res, newmods=Hmisc::rcspline.eval(newxs, knots, inclx=TRUE)))[,c(1,3,4)]
  db.mf <-cbind(x_name,newxs,db.mf)
  names(db.mf) <- c("estimate_name","estimate","mean","lower","upper")
  return(db.mf)
}

# READ IN ORIGINAL INTERCEPT-ONLY MODEL FOR ALL DATA (H0) -----------------

#input file names
final_resdensity_models_files<-normalizePath(list.files('3_models/final/', pattern ='resdensity_model.RDS', full.names = T))

#output file names
final_resdensity_models_files_shortnames<-list.files('3_models/final/', pattern ='resdensity_model.RDS', full.names = F)
final_figures_files_shortnames<-sub(pattern='model.RDS',x=final_resdensity_models_files_shortnames, replacement='publicationbias')
final_figures_files_shortnames<-sub(pattern='Outcome.*post_',x=final_figures_files_shortnames, replacement='')

#legends list
legend_CFUonly_during<-readRDS('4_figures/legend_duringstudies.RDS')
legend_CFUonly_after<-readRDS('4_figures/legend_afterstudies.RDS')

legends_list<-list(legend_CFUonly_during,
                   legend_CFUonly_during,
                   legend_CFUonly_after,
                   legend_CFUonly_after)

# FUNNEL PLOTS WITH EFFECTIVE SAMPLE SIZE (AGGREGATED) -------------------------------------------------------------

for (m in 1:length(final_resdensity_models_files)){
  
  #code to return the study study_palette in later code
  microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)

  model<-readRDS(final_resdensity_models_files[m])
  
  #subset the microshades per study palette to just the studies included in the current model/sub-dataset
  cbpl_temp<-microshades_cvd[match(unique(model$data$study_studyID), names(microshades_cvd))]
  
# FUNNEL PLOTS WITH EFFECTIVE SAMPLE SIZE (NON-AGGREGATED) -------------------------------------------------------------

if(m%in%c(1,3)){  
  
#adapted from https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/
funnel = ggplot(aes(y = log10(n_tilda), x = residuals.rma(model)), data = model$data)+
  geom_point(aes(colour=factor(study_studyID)))+
  ggplot2::scale_colour_manual(values=alpha(cbpl_temp,0.5))+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.5))+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous(breaks=seq(-2,2,1), labels = seq(-2,2,1), limits=c(-2,2))+
  scale_y_continuous(breaks=log10(c(1,10,100)), labels = c(1,10,100), limits=c(0,2))+
  #Give the x- and y- axes informative labels
  xlab('Effective sample size') + ylab('Residuals of samplesize model')

funnel<-funnel+ggpubr::rremove('legend')

model2<-readRDS(final_resdensity_models_files[m+1])

#adapted from https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/
funnel2 = ggplot(aes(y = log10(n_tilda), x = residuals.rma(model2)), data = model2$data)+
  geom_point(aes(colour=factor(study_studyID)))+
  ggplot2::scale_colour_manual(values=alpha(cbpl_temp,0.5))+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.5))+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous(breaks=seq(-2,2,1), labels = seq(-2,2,1), limits=c(-2,2))+
  scale_y_continuous(breaks=log10(c(1,10,100)), labels = c(1,10,100), limits=c(0,2))+
  #Give the x- and y- axes informative labels
  xlab('Effective sample size') + ylab('Residuals of time-lag model')

funnel2<-funnel2+ggpubr::rremove('legend')
}
# saveRDS(object = funnel,
#         file=paste('4_figures/publicationbias/',final_figures_files_shortnames[m],'_funnelplot_residuals.RDS', sep=''))

# ggsave(plot=funnel, 
#        file=paste('4_figures/publicationbias/',final_figures_files_shortnames[m],'_funnelplot_residuals.tiff', sep=''),
#        width = 6, height=6)

# EGGER'S REGRESSIONS -------------------------------------------------------------

# SQUARE ROOT OF INVERSE OF EFFECTIVE SAMPLING SIZE
publication.bias.model.smd.srin <- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants,
                                                   mods=~1+sqrt_inv_n_tilda, #square root of inverse sample size (srin)
                                                   random = list(~outcome_study_days|intervention_name, ~1|study_studyID, ~1|unitlevel_id),
                                                   struct = c('CAR'),
                                                   data = model$data,
                                                   control=list(rel.tol=1e-8),
                                                   test="t") # using t dist rather than z)

saveRDS(publication.bias.model.smd.srin, paste('3_models/publicationbias/',final_figures_files_shortnames[m],'_model_srin.RDS', sep=''))
publication.bias.model.smd.srin
publication.bias.model.smd.srin
estimates.publication.bias.model.smd.srin<- estimates.CI(publication.bias.model.smd.srin)

#demonstrative plot
plot_srin = ggplot(aes(x = sqrt_inv_n_tilda, y = yi_totalresistancedeterminants), data = model$data)+
  geom_point(aes(colour=factor(study_studyID)))+
  ggplot2::scale_colour_manual(values=alpha(cbpl_temp,0.5))+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.5))+
  #geom_vline(xintercept=0, linetype="dashed")+
  scale_y_continuous(labels = seq(-1,6,1),breaks=seq(-1,6,1),limits = c(-1.4, 6.2)) +
  #scale_y_continuous(breaks=log(c(10,25,100)), labels = c(10,25,100), limits=c(2,4.6))+
  #Give the x- and y- axes informative labels
  xlab('Square root of inverse of effective sample size') + ylab('Standardised mean difference \n(with heteroscedastic population variances)')

plot_srin<-plot_srin+ggpubr::rremove('legend')

# saveRDS(object = plot_timelag,
#         file=paste('4_figures/publicationbias/',final_figures_files_shortnames[m],'_plot_srin.RDS', sep=''))

# ggsave(plot=funnel, 
#        file=paste('4_figures/publicationbias/',final_figures_files_shortnames[m],'_plot_srin.tiff', sep=''),
#        width = 6, height=6)

# TIME LAG ----------------------------------------------------------------

# model
publication.bias.model.smd.timelag<- metafor::rma.mv(yi_totalresistancedeterminants, vi_totalresistancedeterminants,
                                                     mods=~1+publication_year_mc, #square root of inverse sample size (srin)
                                                     random = list(~outcome_study_days|intervention_name, ~1|study_studyID, ~1|unitlevel_id),
                                                     struct = c('CAR'),
                                                     data = model$data,
                                                     control=list(rel.tol=1e-8),
                                                     test="t") # using t dist rather than z)

saveRDS(publication.bias.model.smd.timelag, paste('3_models/publicationbias/',final_figures_files_shortnames[m],'_model_timelag.RDS', sep=''))
publication.bias.model.smd.timelag
estimates.publication.bias.model.smd.timelag<- estimates.CI(publication.bias.model.smd.timelag)
estimates.publication.bias.model.smd.timelag

#demonstrative plot
plot_timelag = ggplot(aes(x = publication_year_mc, y = yi_totalresistancedeterminants), data = model$data)+
  geom_point(aes(colour=factor(study_studyID)))+
  ggplot2::scale_colour_manual(values=alpha(cbpl_temp,0.5))+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.5))+
  #geom_vline(xintercept=0, linetype="dashed")+
  scale_y_continuous(labels = seq(-1,6,1),breaks=seq(-1,6,1),limits = c(-1.4, 6.2)) +
  #scale_y_continuous(breaks=log(c(10,25,100)), labels = c(10,25,100), limits=c(2,4.6))+
  #Give the x- and y- axes informative labels
  xlab('Mean-centered publication year') + ylab('Standardised mean difference \n(with heteroscedastic population variances)')

plot_timelag<-plot_timelag+ggpubr::rremove('legend')

plot_timelag

#saveRDS(object = plot_timelag,
#        file=paste('4_figures/publicationbias/',final_figures_files_shortnames[m],'_timelag.RDS', sep=''))

# ggsave(plot=plot_timelag, 
#        file=paste('4_figures/publicationbias/',final_figures_files_shortnames[m],'_timelag.tiff', sep=''),
#        width = 6, height=6)


# COMBINED PLOT -----------------------------------------------------------

if(m%in%c(1,3)){  
  
#EDIT LEGENDS TO SEP BEFORE DURING AFTER ONES

layout <- "AABBEE
           CCDDEE"

combinedplot<-
  funnel+
  funnel2+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+
  plot_srin+
  plot_timelag+#ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+
  legends_list[[m]]+
  
plot_layout(design = layout)

combinedplot

saveRDS(object = combinedplot,
        file=paste('4_figures/publicationbias/',final_figures_files_shortnames[m],'_combinedplot.RDS', sep=''))

ggsave(plot = combinedplot, 
       file=paste('4_figures/publicationbias/',final_figures_files_shortnames[m],'_combinedplot.tiff', sep=''),
       width = 12, height=8)
}

}
