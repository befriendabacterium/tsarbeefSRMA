# START -------------------------------------------------------------------

#make list of packages installed
packages_installed<-installed.packages()[,'Package']

#if 'renv' package is not installed, install it
if('renv'%in%packages_installed==FALSE){
  install.packages('renv')
}

#if 'librarian' package is not installed, install it
if('librarian'%in%packages_installed==FALSE){
  install.packages('librarian')
}

#make list of packages needed
packages_needed<-renv::dependencies(path = 'src/')$Package

github_packages<-c('daniel1noble/orchaRd','KarstensLab/microshades')

packages_needed<-c(packages_needed,github_packages)

#use librarian to install any r packages you do not already have
librarian::shelf(packages_needed)

# FUNCTIONS--------------------------------------------------------

inv.logit <- function(f,adjust=0.025) {
  adjust <- (1-2*adjust)
  (adjust*(1+exp(f))+(exp(f)-1))/(2*adjust*(1+exp(f)))
}

# 1. READ IN RAW DATA FROM CSV --------------------------------------------------------

faecaloutcomes<-read.csv('1_data/faecaloutcomes_PICOdf.csv')
environmentaloutcomes<-read.csv('1_data/environmentaloutcomes_PICOdf.csv')

#bind faecal and environmental together
pico_processed<-dplyr::full_join(faecaloutcomes,environmentaloutcomes)

rm(faecaloutcomes,environmentaloutcomes)

#check studies
unique(pico_processed$study_studyID)

#check publications
unique(pico_processed$study_publicationID)

# TIDY DATA AND EXPORT -------------------------------------------------------------

source('src/tidy_outcomedata.R')
source('src/tidy_metadata.R')

#REMOVE SAMPLE TYPES ONLY REPRESENTED BY ONE STUDY -----------------

studies_persampletype<-tapply(pico_processed$study_studyID,pico_processed$outcome_sample_type,function(x){length(unique(x))})

singletons<-which(studies_persampletype==1)

#check singletons
singletons

#subset to ones where over one study per sample type
pico_processed<-pico_processed[!pico_processed$outcome_sample_type%in%c(names(singletons)),]

pico_processed<-pico_processed[order(pico_processed$cluster_intervoutcome),]

write.csv(pico_processed,'2_processeddata/pico_processed.csv', row.names = F)
saveRDS(pico_processed,'2_processeddata/pico_processed.RDS')
#remove all objects bar the dataframe we need for rest of analysis
rm(list=setdiff(ls(), "pico_processed"))

# CALCULATE EFFECT SIZES --------------------------------------------------------------------

#calculate effect sizes for proportions
source('src/calculate_effectsizes.R')

#should equal number in study char table
unique(pico_witheffectsizes$cluster_intervoutcome_ignoreblock)

# CHECK INPUT DATA AFTER CALCULATING EFFECT SIZES --------------------------------------------------------------------

source('src/check_dataextraction.R')

# TIDY UP BEFORE META-ANALYSIS -------------------------------------------------------------------

#pre-meta-analysis tidy of dataframes
source('src/tidy_effectsizes.R')

# WRITE CSV ----------------------------------------------------------------

#save escalc
saveRDS(pico_witheffectsizes,'2_processeddata/pico_witheffectsizes.RDS')
#write csv
write.csv(pico_witheffectsizes,'2_processeddata/pico_witheffectsizes.csv')

#remove all objects bar the dataframe we need for rest of analysis
rm(list=setdiff(ls(), "pico_witheffectsizes"))

unique(pico_witheffectsizes$cluster_resistancetarget)
unique(pico_witheffectsizes$cluster_intervoutcome_ignoreblock) #should match number in the study characteristics table

rcompanion::plotNormalHistogram(pico_witheffectsizes$yi_totalresistancedeterminants)
rcompanion::plotNormalHistogram(pico_witheffectsizes$vi_totalresistancedeterminants)
rcompanion::plotNormalHistogram(pico_witheffectsizes$yi_totaldeterminants)
rcompanion::plotNormalHistogram(pico_witheffectsizes$vi_totaldeterminants)
rcompanion::plotNormalHistogram(pico_witheffectsizes$yi_logitpropres)
rcompanion::plotNormalHistogram(pico_witheffectsizes$vi_logitpropres)
rcompanion::plotNormalHistogram(pico_witheffectsizes$yi_arcsinpropres)
rcompanion::plotNormalHistogram(pico_witheffectsizes$vi_arcsinpropres)