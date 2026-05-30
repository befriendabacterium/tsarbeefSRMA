# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes_post_faeces$study_colour,pico_witheffectsizes_post_faeces$study_studyID, unique)

# SOURCE CUSTOM FUNCTIONS  ------------------------------------------

source('functions/plot_bubbleplots_H2_supplementaryfunctions.R')

# MAKE PARAMS FOR EACH OF OUTCOME TYPES ------------------------------------------

H1_models<-rev(list.files('3_models/hypothesis_1', pattern='model.*RDS',full.names = T))
H1_orchards<-rev(list.files('4_figures/hypothesis_1', pattern=c('orchard.*RDS'),full.names = T))

H2_models<-H1_models
H2_models<-gsub("hypothesis_1","hypothesis_2", H2_models)
H2_models<-gsub("H1","H2", H2_models)

H2_bubbles<-H1_orchards
H2_bubbles<-gsub("orchard","bubble",H2_bubbles)
H2_bubbles<-gsub("hypothesis_1","hypothesis_2", H2_bubbles)
H2_bubbles<-gsub("H1","H2", H2_bubbles)

H1H2_combined<<-H2_bubbles
H1H2_combined<-gsub("bubble","combined",H1H2_combined)
#H1H2_bubbles<-gsub("hypothesis_1","hypothesis_2", H2_bubbles)
H1H2_combined<-gsub("H2","H1H2", H1H2_combined)
  
outcome_types<-c("totalresistancedeterminants",'totaldeterminants',"totalresistancedeterminants",'totaldeterminants')
yis<-paste("yi",outcome_types,sep='_')
vis<-paste("vi",outcome_types,sep='_')

xlabs<-c("Days of continuous antibiotic administration","Days of continuous antibiotic administration",
         "Days since cessation of antibiotic administration","Days since cessation of antibiotic administration")

during_legend<-readRDS('4_figures/legend_duringstudies.RDS')
after_legend<-readRDS('4_figures/legend_afterstudies.RDS')

legends_list<-list(during_legend,during_legend,after_legend,after_legend)

# MAKE MODEL SPECIFICATIONS -----------------------------------------------

#specify number of knots -  choose 3 because only 13-11 studies per analysis
knots=3

#named vector with forms in which time is modeled
time_form<-c('~ outcome_time_days',
             paste('~ rms::rcs(outcome_time_days, ', knots,')', sep='')) #restricted cubic spline with 3 knots
names(time_form)<-c('timelinear','timercs')

#initiate empty vector to store model specifications
all_model_specifications<-c()

for (f in 1:length(time_form)){
  
  model_specification<-as.formula(paste(time_form[f]))
  all_model_specifications<-c(all_model_specifications,model_specification)
  names(all_model_specifications)[length(all_model_specifications)]<-paste(names(time_form[f]),sep='_')
  #}
}

all_model_specifications

residualvar_spec<-as.formula('~1|unitlevel_id')

# LOOP OVER THE DURING AND AFTER SUBSETS DATAFRAMES --------

for (m in 1:length(H1_models)){
  
  H1_model<-readRDS(H1_models[m])
  df_temp<-H1_model$data
  orchardplot_withinset<-readRDS(H1_orchards[m])
    
    for (s in 1:length(all_model_specifications)){
      
    # RUN MODELS (looping over two specifications) -----------------------------------------------------------
      
    model<-'Cannot run model'
    model_noranf<-'Cannot run model'
    I2<-'Cannot run at least one of the models'
    R2<-'Cannot run at least one of the models'
    print(paste(m,s),sep='-')
    
    set.seed(123)
    tryCatch(
    #https://stats.stackexchange.com/questions/404033/coding-nested-factors-for-3-level-in-rma-mv-function-of-metafor-package
    model <- metafor::rma.mv(df_temp[,yis[m]], df_temp[,vis[m]],
                              mod = all_model_specifications[[s]],
                              random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                              struct = c('CAR'),
                              data = df_temp,
                              control=list(rel.tol=1e-8)
                            )
    ,
    #if error, 
    error = function(e){
      #print('Could not compute model - skipped')
    })
    
    set.seed(123)
    tryCatch(
    #model with random effect removed for calculating I2 via Jackson apporach (see below)
    model_noranf <- metafor::rma.mv(df_temp[,yis[m]], df_temp[,vis[m]],
                                    mod = all_model_specifications[[s]],
                                    #random = list(~outcome_study_days|intervention_name, ~1|study_studyID,  ~1|unitlevel_id),
                                    #struct = c('CAR'),
                                    data = df_temp,
                                    control=list(rel.tol=1e-8)
                              )
    ,
    #if error, 
    error = function(e){
      #print('Could not compute model - skipped')
    })
    
    tryCatch(
    #use Jackson approach to calculate pseudo-I2 - https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
    I2<-c(100 * (vcov(model)[1,1] - vcov(model_noranf)[1,1]) / vcov(model)[1,1])
    ,
    #if error, 
    error = function(e){
      #print('Could not compute model - skipped')
    })
    
    tryCatch(
      #use Wolfgang's approach to calculate pseudo-R2 of 3 variance components (2 sigmas and tau) - https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-October/000247.html; https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor; https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
      R2<-100*(max(0,(sum(model$sigma2,model$tau2) -
          sum(model$sigma2,model$tau2)) / 
          sum(model$sigma2,model$tau2)))
        
      ,
      #if error, 
      error = function(e){
        #print('Could not compute model - skipped')
      })
    
    #random = list(~1 | interaction(outcome_time_days,intervention_name)),
    
    #directory creation chunk
    filename<-H2_models[m]
    filename<-gsub(".RDS",paste("_",names(all_model_specifications)[s],".RDS",sep=''), filename)
    saveRDS(model, file = filename)
    saveRDS(I2, file = gsub("H2_model","H2_I2",filename))
    saveRDS(R2, file = gsub("H2_model","H2_R2",filename))
    
    # PLOT BUBBLES -----------------------------------------------------------
    
    #subset the microshades per study palette to just the studies included in the current model/sub-dataset
    cbpl_temp<-microshades_cvd[match(df_temp$study_studyID, names(microshades_cvd))]
    
    #make bubbleplot
    bubbleplot<-bubble_plot_MLJ(model, 
                    group = "study_studyID", 
                    mod = "outcome_time_days", 
                    g=T,
                    k = F,
                    xlab = xlabs[m],
                    ylab = "Standardised mean difference \n(with heteroscedastic population variances)")+
      ggplot2::geom_hline(yintercept = 0, 
                          linetype = 2, colour = "black", alpha=0.5)+
      coord_cartesian(xlim = log10(c(3,300)+1)+c(-0.25,0.25))+ #expands limits whilst keeping consistent between plots https://stackoverflow.com/questions/42147636/how-to-keep-consistent-axes-scaling-in-a-grid-of-ggplot2-plots-with-different-ca
      ggplot2::scale_x_continuous(breaks = log10(c(3,10,30,100,300)+1), labels = c(3,10,30,100,300), minor_breaks = NULL)+
      ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks, minor_breaks = NULL)+
      ggplot2::scale_colour_manual(values = cbpl_temp)+
      ggplot2::scale_fill_manual(values = cbpl_temp)+
      theme_plots()
    
    if(length(model$b)==2){
    bubbleplot<-
      bubbleplot+
      annotate("text", size=3.5, x=0.45, y=2.5, hjust = 0,
               label=paste("Slope = ", round(model$b[2],2),'\n',
                           "95% CI = ", round(model$ci.lb[2],2),' to ', round(model$ci.ub[2],2),'\n',
                           #"95% PI = ", estimates[,5],' to ',estimates[,6],'\n',
                           "p = ", format.pval(model$pval[2], eps = 0.01, digits = 2),
                           sep=''))
    }
    
    #set scaledown factor
    scaledown<-0.35
    
    #make mini bubbleplot for inset
    minibubbleplot<-
      bubble_plot_MLJ(model, 
                      group = "study_studyID", 
                      mod = "outcome_time_days", 
                      g = T,
                      k = F,
                      xlab = "",
                      ylab = "")+
      coord_cartesian(xlim = log10(c(3,300)+1)+c(-0.25,0.25))+ #expands limits whilst keeping consistent between plots https://stackoverflow.com/questions/42147636/how-to-keep-consistent-axes-scaling-in-a-grid-of-ggplot2-plots-with-different-ca
      ggplot2::scale_x_continuous(breaks = log10(c(1,3,10,30,100,300)+1), labels = c(1,3,10,30,100,300), minor_breaks = NULL)+
      scale_y_continuous(limits = c(ylowermax-0.25,yuppermax+0.25), breaks = seq(ylowermax,yuppermax,1), minor_breaks = NULL)+
      ggplot2::scale_colour_manual(values = cbpl_temp)+
      ggplot2::scale_fill_manual(values = cbpl_temp)+
      #downscale elements
      scale_size_continuous(range = c(0.1,1))+
      theme(axis.text.y = ggplot2::element_text(size = 10*scaledown, colour ="black",angle = 0))+
      ggpubr::rremove('x.text')+
      ggpubr::rremove('x.ticks')+
      ggpubr::rremove('legend')+
      theme(
        panel.background = element_rect(fill='white'),
        plot.background = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
      )+
      annotate("rect", xmin = log10(3+1)-0.25, xmax = log10(300+1)+0.25, ymin = ylowermaj-0.25, ymax = yuppermaj+0.25,
               alpha = 0, color= "darkgrey")
    
    #plot mini orchard plot as inset on main orchard plot
    bubbleplot_withinset<-
      bubbleplot+
      patchwork::inset_element(minibubbleplot,0.75,0,0.975,0.4)+
      theme(plot.margin = unit(c(0,0,0,0),"mm"))
      
    bubbleplot_withinset
    
    #directory creation chunk
    filename<-H2_bubbles[m]
    filename<-gsub(".RDS",paste("_",names(all_model_specifications)[s],".RDS",sep=''), filename)
    saveRDS(object=bubbleplot_withinset, file = filename)
    ggsave(plot=bubbleplot_withinset, file = gsub(".RDS",".tiff",filename), width=10, height=7.5)
    
    #combine plots
    combinedplot<-
      orchardplot_withinset[[1]]+
      orchardplot_withinset[[2]]+ #inset
      bubbleplot_withinset[[1]]+coord_cartesian(xlim = log10(c(3,300)+1)+c(-0.25,0.25))+ggpubr::rremove('ylab')+ggpubr::rremove('y.text')+
      bubbleplot_withinset[[2]]+ #inset
      patchwork::plot_annotation(tag_levels = list(c('A', '','B','')))+
      legends_list[[m]]+
      patchwork::plot_layout(widths = c(4, 8, 4))
    
    #directory creation chunk
    filename<-H1H2_combined[m]
    filename<-gsub(".RDS",paste("_",names(all_model_specifications)[s],".RDS",sep=''), filename)
    saveRDS(object=combinedplot, file = filename)
    ggsave(plot=combinedplot, file = gsub(".RDS",".tiff",filename), width=16, height=7.5)
    
    }
  }

# CLEAN UP ----------------------------------------------------------------

rm(all_model_specifications)
