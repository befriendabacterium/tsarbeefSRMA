# CHECK COINTERVENTIONS ----------------------------------------

all(pico_witheffectsizes$intervention_cointervention_TF==pico_witheffectsizes$control_cointervention_TF)

# CHECK NO. OF INTERVENTION-OUTCOME COMBINATIONS ---------------------------------

#check how many intervention-outcomes
unique(pico_witheffectsizes$cluster_intervoutcome) 

#should equal number of series
sum(lengths(tapply(pico_witheffectsizes$outcome_series_id,pico_witheffectsizes$study_studyID,unique))) #it does!

# PREP DATAFRAME TO GUIDE PLOTTING ----------------------------------------

basesize<-5

figurematch_df<-as.data.frame(rbind(
  #1
  cbind("agga2016_USMARC_Chlortet_NA_chlortetracycline_10_monensin_tylosin_NA_faeces_ecoli_tet","Agga2016_Figure1B.png",1,7,2,"Agga2016_Figure1A.png",1,7,2),
  #2
  cbind("agga2016_USMARC_Chlortet_NA_chlortetracycline_10_monensin_tylosin_NA_soil_ecoli_tet","Agga2016_Figure2B.png",0,8,2,"Agga2016_Figure2A.png",0,8,2),
  #3
  cbind("agga2023_WKU_Tyl_NA_tylosin_75_NA_NA_NA_faeces_enteroc_ery","Agga2023_Figure1D.png",-2,6,2,"Agga2023_Figure1A.png",-0.5,8,2),
  #4
  cbind("beukers2015_Lethbridge_Tyl_NA_tylosin_11_NA_NA_NA_faeces_enteroc_ery","Beukers2015_Figure2.png",0,6,1,"Beukers2015_Figure2.png",0,6,1),
  #5
  cbind("beukers2015_Lethbridge_Tyl_NA_tylosin_11_NA_NA_NA_faeces_enteroc_tyl","Beukers2015_Figure2.png",0,6,1,"Beukers2015_Figure2.png",0,6,1),
  #6
  cbind("dornbach2025_USMARC_Tylmon_trial1_tylosin_85_monensin_NA_NA_faeces_ecoli_ery128","Dornbach2025_Figure1C_nearestmatch.png",1,4,0.5,"Dornbach2025_Figure1A_nearestmatch.png",5.5,8,0.5),
  #7
  cbind("dornbach2025_USMARC_Tylmon_trial1_tylosin_85_monensin_NA_NA_faeces_ecoli_ery8","Dornbach2025_Figure1B_nearestmatch.png",3.5,7.5,0.5,"Dornbach2025_Figure1A_nearestmatch.png",5.5,8,0.5),
  #8
  cbind("dornbach2025_USMARC_Tylmon_trial1_tylosin_85_monensin_NA_NA_faeces_enteroc_ery128","Dornbach2025_Figure4C_nearestmatch.png",2,5,0.5,"Dornbach2025_Figure4A_nearestmatch.png",4.5,8,0.5),
  #9
  cbind("dornbach2025_USMARC_Tylmon_trial1_tylosin_85_monensin_NA_NA_faeces_enteroc_ery8","Dornbach2025_Figure4B_nearestmatch.png",3,6,0.5,"Dornbach2025_Figure4A_nearestmatch.png",4.5,8,0.5),
  #10
  cbind("dornbach2025_USMARC_Tylmon_trial2_tylosin_85_monensin_NA_NA_faeces_ecoli_ery128","Dornbach2025_Figure1C_nearestmatch.png",1,4,0.5,"Dornbach2025_Figure1A_nearestmatch.png",5.5,8,0.5),
  #11
  cbind("dornbach2025_USMARC_Tylmon_trial2_tylosin_85_monensin_NA_NA_faeces_ecoli_ery8","Dornbach2025_Figure1B_nearestmatch.png",3.5,7.5,0.5,"Dornbach2025_Figure1A_nearestmatch.png",5.5,8,0.5),
  #12
  cbind("dornbach2025_USMARC_Tylmon_trial2_tylosin_85_monensin_NA_NA_faeces_enteroc_ery128","Dornbach2025_Figure4C_nearestmatch.png",2,5,0.5,"Dornbach2025_Figure4A_nearestmatch.png",5,8,0.5),
  #13
  cbind("dornbach2025_USMARC_Tylmon_trial2_tylosin_85_monensin_NA_NA_faeces_enteroc_ery8","Dornbach2025_Figure4B_nearestmatch.png",3,6,0.5,"Dornbach2025_Figure4A_nearestmatch.png",5,8,0.5),
  #14
  cbind("hoffman2025_TexasA&M_Tyl_NA_tylosin_75_monensin_tylosin_NA_faeces_ecoli_ery128","Hoffman2025_Figure5C_nearestmatch.png",0,5,0.5,"Hoffman2025_Figure3A_nearestmatch.png",0,7,0.5),
  #15
  cbind("hoffman2025_TexasA&M_Tyl_NA_tylosin_75_monensin_tylosin_NA_faeces_enteroc_ery128","Hoffman2025_Figure2C_nearestmatch.png",0,6,0.5,"Hoffman2025_Figure2A_nearestmatch.png",0,7,0.5),
  #16
  cbind("hoffman2025_TexasA&M_Tyl_NA_tylosin_75_monensin_tylosin_NA_faeces_enteroc_ery8","Hoffman2025_Figure2B_nearestmatch.png",0,6,0.5,"Hoffman2025_Figure2A_nearestmatch.png",0,7,0.5),
  #17
  cbind("inglis2019_Lethbridge_Chlortetsul_NA_chlortetracycline sulfamethazine_350_chlortetracycline_NA_NA_faeces_bacteria_sul2","Inglis2019_Figure1H.png",0,7.5,1,"Inglis2019_Figure1A.png",0,10.5,0.5),
  #18
  cbind("inglis2019_Lethbridge_Chlortetsul_NA_chlortetracycline sulfamethazine_350_sulfamethazine_NA_NA_faeces_bacteria_tetB","Inglis2019_Figure1B.png",0,8,1,"Inglis2019_Figure1A.png",0,10.5,0.5),
  #19
  cbind("inglis2019_Lethbridge_Chlortetsul_NA_chlortetracycline sulfamethazine_350_sulfamethazine_NA_NA_faeces_bacteria_tetC","Inglis2019_Figure1C.png",0,7.5,1,"Inglis2019_Figure1A.png",0,10.5,0.5),
  #20
  cbind("inglis2019_Lethbridge_Chlortetsul_NA_chlortetracycline sulfamethazine_350_sulfamethazine_NA_NA_faeces_bacteria_tetL","Inglis2019_Figure1D.png",0,7.5,1,"Inglis2019_Figure1A.png",0,10.5,0.5),
  #21
  cbind("inglis2019_Lethbridge_Chlortetsul_NA_chlortetracycline sulfamethazine_350_sulfamethazine_NA_NA_faeces_bacteria_tetM","Inglis2019_Figure1E.png",0,9,1,"Inglis2019_Figure1A.png",0,10.5,0.5),
  #22
  cbind("inglis2019_Lethbridge_Chlortetsul_NA_chlortetracycline sulfamethazine_350_sulfamethazine_NA_NA_faeces_bacteria_tetO","Inglis2019_Figure1F.png",0,10,1,"Inglis2019_Figure1A.png",0,10.5,0.5),
  #23
  cbind("inglis2019_Lethbridge_Chlortetsul_NA_chlortetracycline sulfamethazine_350_sulfamethazine_NA_NA_faeces_bacteria_tetW","Inglis2019_Figure1G.png",0,10,1,"Inglis2019_Figure1A.png",0,10.5,0.5),
  #24
  cbind("kanwar2014_TexasA&M_CefCefchlortet_NA_ceftiofur_6.6_chlortetracycline_NA_NA_faeces_bacteria_blaCMY-2","Kanwar2014_Figure3A.png",3.5,6,1,"Kanwar2014_Figure3C.png",0,11,0.5),
  #25
  cbind("kanwar2014_TexasA&M_CefCefchlortet_NA_ceftiofur_6.6_chlortetracycline_NA_NA_faeces_bacteria_blaCTX-M","Kanwar2014_Figure4A.png",2,10,1,"Kanwar2014_Figure4A.png",0,10.5,2),
  #26
  cbind("kanwar2014_TexasA&M_CefCefchlortet_NA_ceftiofur_6.6_NA_NA_NA_faeces_bacteria_blaCMY-2","Kanwar2014_Figure3A.png",3.5,6,1,"Kanwar2014_Figure3C.png",0,11,0.5),
  #27
  cbind("kanwar2014_TexasA&M_CefCefchlortet_NA_ceftiofur_6.6_NA_NA_NA_faeces_bacteria_blaCTX-M","Kanwar2014_Figure4A.png",2,10,1,"Kanwar2014_Figure4A.png",0,10.5,2),
  #28
  cbind("levent2022_TexasA&M_CefTul_NA_ceftiofur_6.6_NA_NA_NA_faeces_ecoli_axo","Levent2022_Figure2B_nearestmatch.png",0,3.5,1,"Levent2022_Figure2A.png",1,7,1),
  #29
  cbind("lowrance2007_TexasA&M_Cef_NA_ceftiofur_4.4_NA_NA_NA_faeces_ecoli_tio","Lowrance2007_Figure3B.png",0,6,1,"Lowrance2007_Figure3A.png",4,7,1),
  #30
  cbind("lowrance2007_TexasA&M_Cef_NA_ceftiofur_6.6_NA_NA_NA_faeces_ecoli_tio","Lowrance2007_Figure3B.png",0,6,1,"Lowrance2007_Figure3A.png",4,7,1),
  #31
  cbind("murray2022_TexasA&M_TylTylprobiotic_bodyweight1_tylosin_7.3_NA_NA_E.faecium probiotic_faeces_enteroc_ery","Murray2022_Figure3C_nearestmatch.png",-0.5,7,1,"Murray2022_Figure3A_nearestmatch.png",4,7,1),
  #32
  cbind("murray2022_TexasA&M_TylTylprobiotic_bodyweight1_tylosin_7.3_NA_NA_NA_faeces_enteroc_ery","Murray2022_Figure3C_nearestmatch.png",-0.5,7,1,"Murray2022_Figure3A_nearestmatch.png",4,7,1),
  #33
  cbind("murray2022_TexasA&M_TylTylprobiotic_bodyweight2_tylosin_7.3_NA_NA_E.faecium probiotic_faeces_enteroc_ery","Murray2022_Figure3C_nearestmatch.png",-0.5,7,1,"Murray2022_Figure3A_nearestmatch.png",4,7,1),
  #34
  cbind("murray2022_TexasA&M_TylTylprobiotic_bodyweight2_tylosin_7.3_NA_NA_NA_faeces_enteroc_ery","Murray2022_Figure3C_nearestmatch.png",-0.5,7,1,"Murray2022_Figure3A_nearestmatch.png",4,7,1),
  #35
  cbind("ohta2019_TexasA&M_CefCefchlortet_NA_ceftiofur_6.6_chlortetracycline_NA_NA_faeces_ecoli_tio","Ohta2019_Figure3C&D.png",-0.5,6,1,"Ohta2019_Figure3C&D.png",0,6,1),
  #36
  cbind("ohta2019_TexasA&M_CefCefchlortet_NA_ceftiofur_6.6_chlortetracycline_NA_NA_faeces_salm_tio","Ohta2019_Figure2C&D.png",-0.5,6,1,"Ohta2019_Figure2C&D.png",0,6,1),
  #37
  cbind("ohta2019_TexasA&M_CefCefchlortet_NA_ceftiofur_6.6_NA_NA_NA_faeces_ecoli_tio","Ohta2019_Figure3A&B.png",-0.5,6,1,"Ohta2019_Figure3A&B.png",0,6,1),
  #38
  cbind("ohta2019_TexasA&M_CefCefchlortet_NA_ceftiofur_6.6_NA_NA_NA_faeces_salm_tio","Ohta2019_Figure2A&B.png",-0.5,6,1,"Ohta2019_Figure2A&B.png",0,6,1),
  #39
  cbind("schmidt2020_USMARC_Tyl_NA_tylosin_75_monensin_NA_NA_faeces_enteroc_ery","Schmidt2020_Figure5C_nearestmatch.png",-1,5,1,"Schmidt2020_Figure5A_nearestmatch.png",-1,5,1),
  #40
  cbind("schmidt2020_USMARC_Tyl_NA_tylosin_75_monensin_NA_NA_soil_enteroc_ery","Schmidt2020_Figure7C.png",0,8,1,"Schmidt2020_Figure7A.png",0,8,1)
))

nrow(figurematch_df)

colnames(figurematch_df)<-c('cluster_intervoutcome','resfig_file','resfig_ymin','resfig_ymax','resfig_breaks','totfig_file','totfig_ymin','totfig_ymax','totfig_breaks')

figurematch_df <- figurematch_df %>% mutate_at(c('resfig_ymin','resfig_ymax','resfig_breaks','totfig_ymin','totfig_ymax','totfig_breaks'), as.numeric)

# LOOP THROUGH AND PLOT ----------------------------------------

unique(pico_witheffectsizes$cluster_intervoutcome)

outcome<-c('totalresistancedeterminants','totaldeterminants')
figfile_cols<-c('resfig_file','totfig_file')
filymin_cols<-c('resfig_ymin','totfig_ymin')
filymax_cols<-c('resfig_ymax','totfig_ymax')
filybreaks_cols<-c('resfig_breaks','totfig_breaks')

for (o in 1:2){
  
  print(outcome[o])
  plotlist <- list()
  
  for (row_selected in 1:nrow(figurematch_df)){
    
    print(row_selected)
    
  ## PLOT ORIGINAL FIGURE ----------------------------------------------------
    
  orig_fig_directory<-'1_data/originalfigures/'
  img<-png::readPNG(paste(orig_fig_directory,figurematch_df[row_selected,figfile_cols[o]],sep=''))
  #orig_fig<-kfbmisc::png_to_grob('1_data/originalfigures/agga2023_tylosin75_enteroc_ery.png')
  g <- grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))
  # Plot as ggplot object
  orig_fig <- ggplot() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme_void()
  
  ## PLOT REPROCESSED FIGURE ----------------------------------------------------
  
  temp<-pico_witheffectsizes[pico_witheffectsizes$cluster_intervoutcome==figurematch_df$cluster_intervoutcome[row_selected],]
  
  #plot both per ml and per swabs for agga plot since latter is more similar to plot
  if(all(temp$study_publicationID=='agga2016')){
    temp<-temp[which(temp$outcome_units%in%c('cfuml','cfug')),]
  }
  
  # if(all(temp$study_publicationID=='dornbach2025')){
  #   temp<-temp[which(temp$study_block=='trial1'),]
  # }
  
  if (o==1){
  
  reprocessed_fig<-ggplot(temp) +
    geom_point(mapping=aes(x=outcome_study_days,
                           y=intervention_mean_totalresistancedeterminants), col='red')+
    geom_line(mapping=aes(x=outcome_study_days,
                          y=intervention_mean_totalresistancedeterminants), col='red')+
    geom_errorbar(mapping=aes(x=outcome_study_days, 
                              ymin = intervention_mean_totalresistancedeterminants-intervention_sd_totalresistancedeterminants,
                              ymax = intervention_mean_totalresistancedeterminants+intervention_sd_totalresistancedeterminants),
                  width=0, col='red')+
    geom_point(mapping=aes(x=outcome_study_days, 
                           y=control_mean_totalresistancedeterminants), col='blue')+
    geom_line(mapping=aes(x=outcome_study_days, 
                          y=control_mean_totalresistancedeterminants), col='blue')+
    geom_errorbar(mapping=aes(x=outcome_study_days, 
                              ymin = control_mean_totalresistancedeterminants-control_sd_totalresistancedeterminants,
                              ymax = control_mean_totalresistancedeterminants+control_sd_totalresistancedeterminants),
                  width=0, col='blue')+
    scale_y_continuous(limits = c(figurematch_df[row_selected,filymin_cols[o]],figurematch_df[row_selected,filymax_cols[o]]), 
                       breaks = seq(figurematch_df[row_selected,filymin_cols[o]],figurematch_df[row_selected,filymax_cols[o]], by = figurematch_df[row_selected,filybreaks_cols[o]]))+
    theme_minimal(base_size = basesize)
  }
  
  if (o==2){
  
  reprocessed_fig<-ggplot(temp) +
    geom_point(mapping=aes(x=outcome_study_days,
                           y=intervention_mean_totaldeterminants), col='red')+
    geom_line(mapping=aes(x=outcome_study_days,
                          y=intervention_mean_totaldeterminants), col='red')+
    geom_errorbar(mapping=aes(x=outcome_study_days, 
                              ymin = intervention_mean_totaldeterminants-intervention_sd_totaldeterminants,
                              ymax = intervention_mean_totaldeterminants+intervention_sd_totaldeterminants),
                  width=0, col='red')+
    geom_point(mapping=aes(x=outcome_study_days, 
                           y=control_mean_totaldeterminants), col='blue')+
    geom_line(mapping=aes(x=outcome_study_days, 
                          y=control_mean_totaldeterminants), col='blue')+
    geom_errorbar(mapping=aes(x=outcome_study_days, 
                              ymin = control_mean_totaldeterminants-control_sd_totaldeterminants,
                              ymax = control_mean_totaldeterminants+control_sd_totaldeterminants),
                  width=0, col='blue')+
    scale_y_continuous(limits = c(figurematch_df[row_selected,filymin_cols[o]],figurematch_df[row_selected,filymax_cols[o]]), 
                       breaks = seq(figurematch_df[row_selected,filymin_cols[o]],figurematch_df[row_selected,filymax_cols[o]], by = figurematch_df[row_selected,filybreaks_cols[o]]))+
    theme_minimal(base_size = basesize)
  
  }
  
  ## PLOT EFFECT SIZE FIGURE ----------------------------------------------------
  
  if (o==1){
  
  effectsize_fig<-ggplot(temp) +
    geom_point(mapping=aes(x=outcome_study_days,
                           y=yi_totalresistancedeterminants), col='purple4')+
    geom_line(mapping=aes(x=outcome_study_days,
                          y=yi_totalresistancedeterminants), col='purple4')+
    geom_errorbar(mapping=aes(x=outcome_study_days, 
                              ymin = yi_totalresistancedeterminants-vi_totalresistancedeterminants,
                              ymax = yi_totalresistancedeterminants+vi_totalresistancedeterminants),
                  width=0, col='purple4')+
    geom_hline(yintercept = 0, linetype = 'dashed')+
    theme_minimal(base_size = basesize)
  
  }
  
  if (o==2){
    
    effectsize_fig<-ggplot(temp) +
      geom_point(mapping=aes(x=outcome_study_days,
                             y=yi_totaldeterminants), col='purple4')+
      geom_line(mapping=aes(x=outcome_study_days,
                            y=yi_totaldeterminants), col='purple4')+
      geom_errorbar(mapping=aes(x=outcome_study_days, 
                                ymin = yi_totaldeterminants-vi_totaldeterminants,
                                ymax = yi_totaldeterminants+vi_totaldeterminants),
                    width=0, col='purple4')+
      geom_hline(yintercept = 0, linetype = 'dashed')+
      theme_minimal(base_size = basesize)
  }
  
  ## PLOT ALL THREE FIGURES TOGETHERE ----------------------------------------------------
  
  plottype<-'Actual/marginal means'
  
  if(grepl("nearestmatch", figurematch_df$resfig_file[row_selected])){
    plottype<-'Nearest matching plot'
  }
  
  combined_fig<-orig_fig+
                  reprocessed_fig+
                    effectsize_fig+
                        plot_annotation(
                          title = paste(row_selected,". ",figurematch_df$cluster_intervoutcome[row_selected],sep=''),
                          subtitle = plottype,
                          theme = theme(plot.title=element_text(size=8)))
  
  plotlist[[row_selected]] <- combined_fig
  
  }
  
  
  multi.page<-ggpubr::ggarrange(plotlist = plotlist,
                    nrow=3, ncol=1,
                    widths = 21.0,
                    heights = 29.7)
  
  ggexport(multi.page, 
           filename = paste(orig_fig_directory,"dataexvalidation_",outcome[o],".pdf", sep='')
           )
}
