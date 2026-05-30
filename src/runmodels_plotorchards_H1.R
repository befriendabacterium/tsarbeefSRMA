periods_to_model<-c('during','after')
outcome_types<-c("totalresistancedeterminants",'totaldeterminants')
yis<-paste("yi",outcome_types,sep='_')
vis<-paste("vi",outcome_types,sep='_')

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)

source("functions/load_plotdefaults.R")

# LOOP OVER THE DURING AND AFTER SUBSETS DATAFRAMES AND RUN INTERC --------

for (p in 1:length(periods_to_model)){
  
  for (o in 1:length(outcome_types)){
    
    #read in dataframe
    df_temp<-subset(pico_witheffectsizes_post_faeces,
                    pico_witheffectsizes_post_faeces$period==periods_to_model[p])
    
    #remove NA yis
    df_temp<-df_temp[!is.na(df_temp[,yis[o]]),]
    #remove NA vis
    df_temp<df_temp[!is.na(df_temp[,vis[o]]),]
    
    #droplevels
    df_temp<-droplevels(df_temp)
    
    # RUN MODELS -----------------------------------------------------------
    
    #main model
    model<- metafor::rma.mv(df_temp[,yis[o]], df_temp[,vis[o]],
                            random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                            struct = c('CAR'),
                            data = df_temp,
                            control=list(rel.tol=1e-8))
    
    #model with random effect removed for calculating I2 via Jackson apporach (see below)
    model_noranf<- metafor::rma.mv(df_temp[,yis[o]], df_temp[,vis[o]],
                                   #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                   #struct = c('CAR'),
                                   data = df_temp,
                                   control=list(rel.tol=1e-8))
    #profile(model, tau2=1)
    #metafor::profile.rma.mv(test, tau2 = 1)

    #use Jackson approach to calculate I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
    I2<-c(100 * (vcov(model)[1,1] - vcov(model_noranf)[1,1]) / vcov(model)[1,1])

    #interpret according to Cochrane guidelines
    interpretation<-ifelse(I2>=0.00&I2<=40, paste('Might not be important'),
                       ifelse(I2>=30&I2<=60, paste('Moderate heterogeneity'),
                        ifelse(I2>=50&I2<=90, paste('Substantial heterogeneity'),
                          ifelse(I2>=90&I2<=100, paste('Considerable heterogeneity'), paste('Not a percentage')))))
    
    #print the I2 and interpretation
    print(paste(periods_to_model[p],
                paste('Estimate = ',round(mean(model$b),2),' (',round(model$ci.lb,2),'-',round(model$ci.ub,2),')', sep=''),
                paste(round(I2),'%',sep=''),interpretation,sep=' - '))
    
    #save RDS of the model
    writepath<-paste('3_models/hypothesis_1/')
    filename<-paste(periods_to_model[p],outcome_types[o], sep='_')
    saveRDS(model, paste(writepath,'H1_model_',filename,'.RDS', sep=''))
    saveRDS(I2, paste(writepath,'H1_I2_',filename,'.RDS', sep=''))

# PLOT ORCHARDS -----------------------------------------------------------
    
    #subset the microshades per study palette to just the studies included in the current model/sub-dataset
    cbpl_temp<-microshades_cvd[match(df_temp$study_studyID, names(microshades_cvd))]
    
    #get CI and PI estimates from model
    estimates<-orchaRd::mod_results(model=model,group='study_studyID')$mod_table
    
    #round for presentation
    estimates[,-1]<-round(estimates[,-1], 2)
    
    #add p value to estimates dataframe
    estimates$pval<-model$pval
    
    #intercept-only plot (caterpillar-like orchard plot)
    orchardplot<-
      orchaRd::orchard_plot(model,
                            group = "study_studyID",
                            flip = F,
                            g = T,
                            colour = T,
                            k.pos = 1.5,
                            legend.pos = "top.right",
                            xlab = "Standardised mean difference \n(with heteroscedastic population variances)")+
      scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
      scale_x_discrete(labels=c("Overall estimate \n(intercept-only model)"))+
      ggplot2::scale_colour_manual(values = cbpl_temp)+
      ggplot2::scale_fill_manual(values = cbpl_temp)+
      annotate("text", size=3.5, x=0.45, y=2.5, hjust = 0,
               label=paste("Estimate = ", estimates[,2],'\n',
                           "95% CI = ", estimates[,3],' to ',estimates[,4],'\n',
                           "95% PI = ", estimates[,5],' to ',estimates[,6],'\n',
                           "p = ", format.pval(estimates$pval, eps = 0.01),
                           sep=''))+
      theme_plots()+
      theme(legend.position = "inside", 
            legend.justification = c(0.05, 1),
            legend.title = element_text(size=7.5),
            legend.text = element_text(size=5),
            legend.direction = "horizontal",  
            legend.background = element_rect(fill='transparent'))+
    theme(plot.margin = unit(c(0,1,-0.5,1), "cm"))
    
    #set scaledown factor
    scaledown<-0.35
    
    #make mini orchard plot
    miniorchardplot <-  orchaRd::orchard_plot(model,
                                              group = "study_studyID",
                                              flip = F,
                                              g = T,
                                              colour = T,
                                              k = F,
                                              legend.pos = "none",
                                              xlab = "",
                                              trunk.size = 0.5*scaledown^10, branch.size = 1.2*scaledown, twig.size = 0.5*scaledown,
    )+
      scale_y_continuous(limits = c(ylowermax-0.25,yuppermax+0.25), breaks = seq(ylowermax,yuppermax,1), minor_breaks = NULL)+
      ggplot2::scale_colour_manual(values = cbpl_temp)+
      ggplot2::scale_fill_manual(values = cbpl_temp)+
      #downscale elements
      scale_size_continuous(range = c(0.5,1))+
      theme(axis.text.y = ggplot2::element_text(size = 10*scaledown, colour ="black",angle = 0))+
      ggpubr::rremove('x.text')+
      ggpubr::rremove('x.ticks')+
      theme(
        panel.background = element_rect(fill='white'),
        plot.background = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
      )+
      annotate("rect", xmin = 0.5, xmax = 1.5, ymin = ylowermaj-0.25, ymax = yuppermaj+0.25,
               alpha = 0, color= "darkgrey")
    
    #plot mini orchard plot as inset on main orchard plot
    orchardplot_withinset<-orchardplot+
      patchwork::inset_element(miniorchardplot,0.7,0,0.95,0.4)+
      theme(plot.margin = unit(c(0,0,0,0),"mm"))
    
    #save RDS of the model
    filename<-paste('H1_orchardplot', periods_to_model[p],outcome_types[o], sep='_')
    writepath<-paste('4_figures/hypothesis_1',filename,sep='/')
    saveRDS(object = orchardplot_withinset, paste(writepath,'.RDS', sep=''))
    ggsave(plot = orchardplot_withinset, paste(writepath,'.tiff', sep=''), width=6, height=7.5)

  }
}
