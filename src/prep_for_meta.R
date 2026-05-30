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

#make list of github packages needed
github_packages<-c('daniel1noble/orchaRd','KarstensLab/microshades')

#add the github packages to the main list
packages_needed<-c(packages_needed,github_packages)

#use librarian to install any r packages you do not already have
librarian::shelf(packages_needed)

# 1. DOWNLOAD DATA FROM OSF (IF NECESSARY - SKIP IF ALREADY HAVE DATA IN '1_data') --------------------------------------------------

if(!all(c('faecaloutcomes_PICOdf.csv','environmentaloutcomes_PICOdf.csv')%in%list.files('1_data'))){

APIkey<-as.character(readLines('APIkey.txt'))

#authenticate OSF
osf_auth(token = APIkey)

# FROM OSF-ARCHIVED DATA (TO DOWNLOAD FROM OSF)
my_project <- osfr::osf_ls_files(osfr::osf_retrieve_node("2fyc8"), path = '2_metaanalysis_data')
#download all the folders (inputs and outputs)
osfr::osf_download(my_project, path = '1_data', recurse = T)

#remove the osf project as no longer needed as we have a local copy
rm(my_project)
}

# 2. LOAD DATA INTO R --------------------------------------------------

#load dataframes of faecal and environment outcomes
faecaloutcomes<-read.csv('1_data/faecaloutcomes_PICOdf.csv')
environmentaloutcomes<-read.csv('1_data/environmentaloutcomes_PICOdf.csv')

#bind faecal and environmental dataframes together
pico_processed<-dplyr::full_join(faecaloutcomes,environmentaloutcomes)

#remove the unbound dataframes
rm(faecaloutcomes,environmentaloutcomes)

#check studies included
unique(pico_processed$study_studyID)

#check publications included
unique(pico_processed$study_publicationID)

# 3. TIDY DATA AND EXPORT -------------------------------------------------------------

source('src/tidy_outcomedata.R')
source('src/tidy_metadata.R')

## 4. REMOVE SAMPLE TYPES ONLY REPRESENTED BY ONE STUDY -----------------

#calculate studies per sample type
studies_persampletype<-tapply(pico_processed$study_studyID,pico_processed$outcome_sample_type,function(x){length(unique(x))})
#show studies per sample type
studies_persampletype

#check which sample types only have one study
singletons<-which(studies_persampletype==1)

#check singletons
singletons

#subset to ones where over one study per sample type
pico_processed<-pico_processed[!pico_processed$outcome_sample_type%in%c(names(singletons)),]

#for neatness, order by interventionoutcome cluster (i.e. a distinct 'effect' that is repeatedly measured over time)
pico_processed<-pico_processed[order(pico_processed$cluster_intervoutcome),]

#save the processed csv to the processed data folder
write.csv(pico_processed,'2_processeddata/pico_processed.csv', row.names = F)
saveRDS(pico_processed,'2_processeddata/pico_processed.RDS')

#remove all objects bar the dataframe we need for rest of analysis
rm(list=setdiff(ls(), "pico_processed"))

# 5. CALCULATE EFFECT SIZES --------------------------------------------------------------------

#calculate effect sizes for proportions
source('src/calculate_effectsizes.R')

#should equal number in meta-analysis section of the study char table
unique(pico_witheffectsizes$cluster_intervoutcome_ignoreblock)

# 6. CHECK INPUT DATA AFTER CALCULATING EFFECT SIZES (UNHASH TO RUN IF YOU HAVE OBTAINED THE NECESSARY ORIGINAL FIGURES FILES FROM AUTHORS) --------------------------------------------------------------------

# source('src/check_dataextraction.R')

# 7. TIDY UP BEFORE META-ANALYSIS -------------------------------------------------------------------

#pre-meta-analysis tidy of dataframes
source('src/tidy_effectsizes.R')

# 8. WRITE CSV ----------------------------------------------------------------

#save escalc
saveRDS(pico_witheffectsizes,'2_processeddata/pico_witheffectsizes.RDS')
#write csv
write.csv(pico_witheffectsizes,'2_processeddata/pico_witheffectsizes.csv')

#remove all objects bar the dataframe we need for rest of analysis
rm(list=setdiff(ls(), "pico_witheffectsizes"))

unique(pico_witheffectsizes$cluster_resistancetarget)
unique(pico_witheffectsizes$cluster_intervoutcome_ignoreblock) #should match number in the study characteristics table

# 1. SOURCE ALL FUNCTIONS ----------------------------------------------------

sapply(list.files('functions', full.names = T, pattern = '.R'), source)

# QUANTITATIVE SUMMARY OF RESULTS OF INDIVIDUAL STUDIES ------------------------------------------------------------

#run models
source('src/runmodels_plotforests_H0.R')

#remove all objects bar the dataframes we need for rest of analysis
rm(list=setdiff(ls(), c("pico_witheffectsizes","pico_witheffectsizes_pre_faeces","pico_witheffectsizes_post_faeces")))