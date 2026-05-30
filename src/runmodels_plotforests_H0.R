source("functions/load_plotdefaults.R")

# PRE DATA: FAECAL ----------------------------------------------------------------

pico_witheffectsizes_pre_faeces<-pico_witheffectsizes[pico_witheffectsizes$pre_or_post_intervention=='pre',]
pico_witheffectsizes_pre_faeces<-pico_witheffectsizes_pre_faeces[pico_witheffectsizes_pre_faeces$outcome_sample_type%in%c('faeces'),]
saveRDS(pico_witheffectsizes_pre_faeces,'2_processeddata/pico_witheffectsies_pre_faeces.RDS')
write.csv(pico_witheffectsizes_pre_faeces,'2_processeddata/pico_witheffectsies_pre_faeces.csv')

# POST DATA: FAECAL ----------------------------------------------------------------

pico_witheffectsizes_post_faeces<-pico_witheffectsizes[pico_witheffectsizes$pre_or_post_intervention=='post',]
pico_witheffectsizes_post_faeces<-pico_witheffectsizes_post_faeces[pico_witheffectsizes_post_faeces$outcome_sample_type%in%c('faeces'),]
pico_witheffectsizes_post_faeces<-droplevels(pico_witheffectsizes_post_faeces)
saveRDS(pico_witheffectsizes_post_faeces,'2_processeddata/pico_witheffectsies_post_faeces.RDS')
write.csv(pico_witheffectsizes_post_faeces,'2_processeddata/pico_witheffectsies_post_faeces.csv')

#drop levels before modelling
pico_witheffectsizes_post_faeces<-droplevels(pico_witheffectsizes_post_faeces)

# PRE DATA: ENVIRONMENTAL ----------------------------------------------------------------

pico_witheffectsizes_pre_env<-pico_witheffectsizes[pico_witheffectsizes$pre_or_post_intervention=='pre',]
pico_witheffectsizes_pre_env<-pico_witheffectsizes_pre_env[pico_witheffectsizes_pre_env$outcome_sample_type%in%c('soil'),]
pico_witheffectsizes_pre_env<-droplevels(pico_witheffectsizes_pre_env)
saveRDS(pico_witheffectsizes_pre_env,'2_processeddata/pico_witheffectsies_post_env.RDS')
write.csv(pico_witheffectsizes_pre_env,'2_processeddata/pico_witheffectsies_post_env.csv')

#drop levels before modelling
pico_witheffectsizes_pre_env<-droplevels(pico_witheffectsizes_pre_env)

# POST DATA: ENV ----------------------------------------------------------------

pico_witheffectsizes_post_env<-pico_witheffectsizes[pico_witheffectsizes$pre_or_post_intervention=='post',]
pico_witheffectsizes_post_env<-pico_witheffectsizes_post_env[pico_witheffectsizes_post_env$outcome_sample_type%in%c('soil'),]
pico_witheffectsizes_post_env<-droplevels(pico_witheffectsizes_post_env)
saveRDS(pico_witheffectsizes_post_env,'2_processeddata/pico_witheffectsies_post_env.RDS')
write.csv(pico_witheffectsizes_post_env,'2_processeddata/pico_witheffectsies_post_env.csv')

#drop levels before modelling
pico_witheffectsizes_post_env<-droplevels(pico_witheffectsizes_post_env)

# LOOP ----------------------------------------------------------------

pico_witheffectsizes_dflist<-
  list(pre_faeces = pico_witheffectsizes_pre_faeces,
       post_faeces = pico_witheffectsizes_post_faeces,
       pre_env = pico_witheffectsizes_pre_env,
       post_env = pico_witheffectsizes_post_env)

outcome_types<-c("totalresistancedeterminants",'totaldeterminants','logitpropres','arcsinpropres')

yis<-paste("yi",outcome_types,sep='_')
vis<-paste("vi",outcome_types,sep='_')

for (d in 1:length(pico_witheffectsizes_dflist)){
  
  for (o in 1:length(outcome_types)){
  
  print(paste(names(pico_witheffectsizes_dflist)[d],'_',outcome_types[o], sep=''))
  
  #read in dataframe
  df_temp<-pico_witheffectsizes_dflist[[d]]
  
  #remove NA yis
  df_temp<-df_temp[!is.na(df_temp[,yis[o]]),]
  #remove NA vis
  df_temp<df_temp[!is.na(df_temp[,vis[o]]),]
  
# RUN MODELS --------------------------------------------------------------
  
  ## BASE MODEL --------------------------------------------------------------
  
  #main model
  model <- metafor::rma.mv(df_temp[,yis[o]], df_temp[,vis[o]],
                           random = list(~outcome_study_days|intervention_name, ~1|study_studyID, ~1|unitlevel_id),
                           struct = c('CAR'),
                           data = df_temp,
                           control=list(rel.tol=1e-8))

  W <- diag(1/model$vi)
  X <- model.matrix(model)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  100 * model$sigma2 / (sum(model$sigma2) + (model$k-model$p)/sum(diag(P)))
  
  ## AGGREGATED MODEL --------------------------------------------------------------
  
  agg <- aggregate(df_temp, cluster=study_studyID, V=vcov(model, type="obs"), addk=TRUE, var.names=c(yis[o],vis[o]))
  
  #agg <- agg[match(levels(df_temp$study_studyID),agg$study_studyID),]
  
  agg <- select(agg,"study_studyID","study_and_pubs_plotready", "publication_year_mc", "study_colour",
                paste("control_reps_",outcome_types[o], sep=''), paste("intervention_reps_",outcome_types[o], sep=''),
                paste("yi_",outcome_types[o], sep=''), paste("vi_",outcome_types[o], sep=''),
                "n_tilda", "sqrt_inv_n_tilda")
  
  #agg <- agg[order(agg$study_studyID),]
  
  agg <- agg[match(levels(model$data$study_studyID),agg$study_studyID),]
  
  #colnames(agg)<-sub("_totalresistancedeterminants","",colnames(agg))
  
  k <- nrow(agg)
  
  model_aggregated <- metafor::rma(agg[,yis[o]], agg[,vis[o]], method="EE", data=agg, digits=3)
  
  saveRDS(model_aggregated, paste('3_models/hypothesis_0/','H0_model_',names(pico_witheffectsizes_dflist)[d],"_",outcome_types[o],'.RDS',sep=''))

  # PLOT FORESTS --------------------------------------------------------------
  
  grDevices::tiff(paste('4_figures/hypothesis_0/H0_forestplot_',names(pico_witheffectsizes_dflist)[d],"_",outcome_types[o],'.tiff',sep=''),
                  res=300, units='in', width=10, height=8)
  
  par(mar = c(4, 4, 0, 4)) # Reduce top margin
  
  sav<-metafor::forest(model_aggregated,
                      slab=model_aggregated$data$study_and_pubs_plotready,
                       efac=c(0,1),
                       at=c(seq(ylowermaj,yuppermaj,1)),
                       alim=c(ylowermaj-0.25,yuppermaj+0.25),
                       xlim=c(-10,6),
                       ilab=cbind(paste(round(agg$intervention_reps)),paste(round(agg$control_reps))),
                       ilab.xpos=c(-4.5,ylowermaj),
                       header=TRUE, 
                       xlab='Standardised mean difference',
                       mlab="Overall", 
                       cex=0.7,
                       shade='zebra',
                       colout=model_aggregated$data$study_colour)
  
  text(sav$ilab.xpos[1:2], rep(k,2)+c(1.425,1.445), c("Experimental","Control"), cex=0.8)
  #segments(sav$ilab.xpos[1]-0.22, k+1.75, sav$ilab.xpos[2]+0.13, k+1.75)
  text(mean(sav$ilab.xpos[1:2]), k+2.25, "Average number of reps \n in each study arm*",cex=1)
  
  # text(sav$xlim[1], -1.75, pos=4, 
  #      bquote(paste(I^2, " = ", .(round(I2)), "%", "; ",
  #                   tau^2, " = ", .(metafor::fmtx(model$tau2, digits=2)), "; ",
  #                   chi^2, " = ", .(metafor::fmtx(model$QE, digits=2)),
  #                   ", df = ", .(model$k - model$p), ", ",
  #                   .(metafor::fmtp(df_temp$QEp, digits=2, pname="P", add0=TRUE, equal=TRUE)))))
  
  mtext('*Effects from multiple study arms within studies are aggregated', side=1, line=1, at=-9.75, adj=0, cex=0.75)
  
  dev.off()
  
  saveRDS(model_aggregated,paste('3_models/hypothesis_0/H0_model_',names(pico_witheffectsizes_dflist)[d],outcome_types[o],'.RDS',sep=''))
  
  rm(model, model_aggregated)
  
  }
}
