# LOAD FUNCTIONS ----------------------------------------------------------

alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# code to return the study study_palette in later code
microshades_cvd<-tapply(pico_witheffectsizes$study_colour,pico_witheffectsizes$study_studyID, unique)

# ALL METHODS LEGENDS ------------------------------------------------------

## BEFORE/DURING/AFTER ------------------------------------------------------

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes$study_and_pubs_plotready,
                           pico_witheffectsizes$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes$study_studyID), names(microshades_cvd))]

#crap workaround to overide bubble_plot()'s defaults to get a legend for the study colours
#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes,
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_allstudies.RDS')

## OUTCOME 1: BEFORE LEGEND -----------------------------------------------------------

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes[pico_witheffectsizes$period=='before',]$study_and_pubs_plotready,
                           pico_witheffectsizes[pico_witheffectsizes$period=='before',]$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes[pico_witheffectsizes$period=='before',]$study_studyID), names(microshades_cvd))]

#crap workaround to overide bubble_plot()'s defaults to get a legend for the study colours
#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes[pico_witheffectsizes$period=='before',],
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_beforestudies.RDS')

## OUTCOME 2/3: DURING LEGEND -----------------------------------------------------------

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes[pico_witheffectsizes$period=='during',]$study_and_pubs_plotready,
                           pico_witheffectsizes[pico_witheffectsizes$period=='during',]$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes[pico_witheffectsizes$period=='during',]$study_studyID), names(microshades_cvd))]

#crap workaround to overide bubble_plot()'s defaults to get a legend for the study colours
#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes[pico_witheffectsizes$period=='during',],
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_duringstudies.RDS')

## OUTCOME 4/5: AFTER LEGEND ------------------------------------------------------------

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes[pico_witheffectsizes$period=='after',]$study_and_pubs_plotready,
                           pico_witheffectsizes[pico_witheffectsizes$period=='after',]$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes[pico_witheffectsizes$period=='after',]$study_studyID), names(microshades_cvd))]

#crap workaround to overide bubble_plot()'s defaults to get a legend for the study colours
#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes[pico_witheffectsizes$period=='after',],
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_afterstudies.RDS')

## OUTCOME 6: MLSBs ------------------------------------------------------------

pico_witheffectsizes_MLSBs<-subset(pico_witheffectsizes, pico_witheffectsizes$intervention_antibiotic_class==c('Macrolides, lincosamides, \n & streptogramin B'))

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes_MLSBs[pico_witheffectsizes_MLSBs$period=='after',]$study_and_pubs_plotready,
                           pico_witheffectsizes_MLSBs[pico_witheffectsizes_MLSBs$period=='after',]$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_MLSBs[pico_witheffectsizes_MLSBs$period=='after',]$study_studyID), names(microshades_cvd))]

#crap workaround to overide bubble_plot()'s defaults to get a legend for the study colours
#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_MLSBs[pico_witheffectsizes_MLSBs$period=='after',],
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_MLSBs.RDS')

## OUTCOME 7: ESCs ------------------------------------------------------------

pico_witheffectsizes_ESCs<-subset(pico_witheffectsizes, pico_witheffectsizes$intervention_antibiotic_class==c('extended-spectrum \n cephalosporin'))

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes_ESCs[pico_witheffectsizes_ESCs$period=='after',]$study_and_pubs_plotready,
                           pico_witheffectsizes_ESCs[pico_witheffectsizes_ESCs$period=='after',]$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_ESCs[pico_witheffectsizes_ESCs$period=='after',]$study_studyID), names(microshades_cvd))]

#crap workaround to overide bubble_plot()'s defaults to get a legend for the study colours
#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_ESCs[pico_witheffectsizes_ESCs$period=='after',],
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_ESCs.RDS')

# CFU-ONLY LEGENDS ------------------------------------------------------

#the before and after legends are different here

pico_witheffectsizes_CFUonly<-pico_witheffectsizes[pico_witheffectsizes$outcome_measurement_type=='Direct plating \n on agar',]
pico_witheffectsizes_CFUonly<-droplevels(pico_witheffectsizes_CFUonly)

pico_witheffectsizes_CFUonly<-pico_witheffectsizes[pico_witheffectsizes$outcome_measurement_type=='Direct plating \n on agar',]
pico_witheffectsizes_CFUonly<-droplevels(pico_witheffectsizes_CFUonly)

## BEFORE/DURING/AFTER ------------------------------------------------------

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes_CFUonly$study_and_pubs_plotready,
                           pico_witheffectsizes_CFUonly$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_CFUonly$study_studyID), names(microshades_cvd))]

#crap workaround to overide bubble_plot()'s defaults to get a legend for the study colours
#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_CFUonly,
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_allstudies_CFUonly.RDS')

## BEFORE ------------------------------------------------------------------

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes_CFUonly$study_and_pubs_plotready,
                           pico_witheffectsizes_CFUonly$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_CFUonly$study_studyID), names(microshades_cvd))]

#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_CFUonly,
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_CFUonly_before.RDS')

## DURING ------------------------------------------------------------------

pico_witheffectsizes_CFUonly_during<-pico_witheffectsizes_CFUonly[pico_witheffectsizes_CFUonly$period=='during',]

pico_witheffectsizes_CFUonly_during<-droplevels(pico_witheffectsizes_CFUonly_during)

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes_CFUonly_during$study_and_pubs_plotready,
                           pico_witheffectsizes_CFUonly_during$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_CFUonly_during$study_studyID), names(microshades_cvd))]

#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_CFUonly_during,
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_CFUonly_during.RDS')

## AFTER ------------------------------------------------------------------

pico_witheffectsizes_CFUonly_after<-pico_witheffectsizes_CFUonly[pico_witheffectsizes_CFUonly$period=='after',]

pico_witheffectsizes_CFUonly_after<-droplevels(pico_witheffectsizes_CFUonly_after)

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes_CFUonly_after$study_and_pubs_plotready,
                           pico_witheffectsizes_CFUonly_after$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_CFUonly_after$study_studyID), names(microshades_cvd))]


#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_CFUonly[pico_witheffectsizes_CFUonly$period=='after',],
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_CFUonly_after.RDS')

# ALL STUDIES LEGEND (QPCR ONLY) ------------------------------------------------------

pico_witheffectsizes_qPCRonly<-pico_witheffectsizes[pico_witheffectsizes$outcome_measurement_type=='Quantitative PCR \n (qPCR)',]
pico_witheffectsizes_qPCRonly<-droplevels(pico_witheffectsizes_qPCRonly)

pico_witheffectsizes_qPCRonly<-pico_witheffectsizes[pico_witheffectsizes$outcome_measurement_type=='Quantitative PCR \n (qPCR)',]
pico_witheffectsizes_qPCRonly<-droplevels(pico_witheffectsizes_qPCRonly)

## BEFORE/DURING/AFTER ------------------------------------------------------

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes_qPCRonly$study_and_pubs_plotready,
                           pico_witheffectsizes_qPCRonly$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_qPCRonly$study_studyID), names(microshades_cvd))]

#crap workaround to overide bubble_plot()'s defaults to get a legend for the study colours
#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_qPCRonly,
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_allstudies_qPCRonly.RDS')

## BEFORE ------------------------------------------------------------------

#list tidy labels for each of the study IDs
untidy_to_tidylabs<-tapply(pico_witheffectsizes_qPCRonly$study_and_pubs_plotready,
                           pico_witheffectsizes_qPCRonly$study_studyID,
                           paste)

#reduce to one per study with unique()
untidy_to_tidylabs<-lapply(untidy_to_tidylabs, unique)

#unlist so can use for re-labelling
untidy_to_tidylabs<-unlist(untidy_to_tidylabs)

#subset the microshades per study palette to just the studies included in the current model/sub-dataset
cbpl_temp<-microshades_cvd[match(unique(pico_witheffectsizes_qPCRonly$study_studyID), names(microshades_cvd))]

#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_qPCRonly,
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_qPCRonly_before.RDS')

## DURING ------------------------------------------------------------------

#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_qPCRonly[pico_witheffectsizes_qPCRonly$period=='during',],
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_qPCRonly_during.RDS')

## AFTER ------------------------------------------------------------------

#briefly - make a new plot, extract legend, make it a plot, then plot alongside old plot. lol. 
plotforleg<-ggplot(pico_witheffectsizes_qPCRonly[pico_witheffectsizes_qPCRonly$period=='after',],
                   aes(x = NA, y = yi_totalresistancedeterminants, color=study_studyID), alpha=0.5)+
  geom_point()+
  ggplot2::scale_colour_manual(name="Study", values=alpha(cbpl_temp,0.6), labels=untidy_to_tidylabs)+
  ggplot2::scale_fill_manual(values=alpha(cbpl_temp,0.6))+
  ggplot2::theme(legend.background = ggplot2::element_blank())+
  theme_set(theme_bw()) + 
  theme(legend.key=element_blank(), legend.text=element_text(size=8), legend.key.size = unit(1.5, 'lines'))

plotforleg

#extract legend
legend<-ggpubr::get_legend(plotforleg)
#make the legend a plot of its own
legend_plot<-ggpubr::as_ggplot(legend)

saveRDS(legend_plot,'4_figures/legend_qPCRonly_after.RDS')

