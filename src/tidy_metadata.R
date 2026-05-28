# 1. CHANGE TIME COLUMNS ---------------------------------------------------------

#BASELINES: move zeros to baseline column
pico_processed$outcome_timeofbaseline_days<-0

pico_processed$outcome_timeofbaseline_days[pico_processed$study_publicationID=='agga2016']<-5 #9th feb, then treatment from 14th-18th feb (5 days), then first measure on 23rd feb 5 days post treatment
pico_processed$outcome_timeofbaseline_days[pico_processed$study_publicationID=='inglis2020']<-5

#pico_processed$outcome_timeofbaseline_days[baseline_rows]<-log10(pico_processed$outcome_timeofbaseline_days[baseline_rows]+1)

#DURING: no need to log because are normally distributed
#rcompanion::plotNormalHistogram(pico_processed$outcome_timesinceintervention_start_days)
#rcompanion::plotNormalHistogram(log10(pico_processed$outcome_timesinceintervention_start_days+1))
#remove baselines as they zero-skew the data
#pico_processed$outcome_timesinceintervention_start_days[baseline_rows]<-NA
#pico_processed$outcome_timesinceintervention_start_days<-log10(pico_processed$outcome_timesinceintervention_start_days+1)

#END: remove 1 from time since intervention end so that it represents number of days clear of antibiotic not days since treatment
#rcompanion::plotNormalHistogram(pico_processed$outcome_timesinceintervention_end_days)
#rcompanion::plotNormalHistogram(log10(pico_processed$outcome_timesinceintervention_end_days+1))
#pico_processed$outcome_timesinceintervention_end_days<-pico_processed$outcome_timesinceintervention_end_days-1
#log time since intervention end
rcompanion::plotNormalHistogram(pico_processed$outcome_timesinceintervention_end_days)

#INTERVENTION LENGTH: coerce zeros in intervention length to NA --------------
#these are baseline measures so should've been coded as NAs anyway
pico_processed$intervention_length_days[pico_processed$pre_or_post_intervention=='pre']<-NA

# ADD COLUMN FOR PRE OR POST INTERVENTION (i.e. is it baseline data ---------------------------

pico_processed$pre_or_post_intervention<-NA

pico_processed$pre_or_post_intervention[which(pico_processed$outcome_study_days<=0)]<-'pre'

pico_processed$pre_or_post_intervention[is.na(pico_processed$pre_or_post_intervention)]<-'post'

# ADD PERIODS COLUMN FOR TIME -----------------------------------------------------------------

#add a column denoting the period of each measurement (before, during or after)
pico_processed$period<-NA

pico_processed$period[pico_processed$outcome_study_days<=0]<-'before'
pico_processed$period[pico_processed$outcome_timesinceintervention_start_days>0]<-'during'
pico_processed$period[pico_processed$outcome_timesinceintervention_end_days>0]<-'after'

#coerce to factor with specified before, during after order or levels
pico_processed$period<-factor(pico_processed$period, levels=c('before','during','after'))

plyr::count(pico_processed$period)

# 2. CHANGE ANTIBIOTIC CLASS COLUMNS ---------------------------------------------------------

#rename ceph as third gen ceph as all ceftiofur (more specific)
pico_processed$intervention_antibiotic_class<-plyr::revalue(pico_processed$intervention_antibiotic_class, c("cephalosporin"="extended-spectrum \n cephalosporin",
                                                                                                            "MLSB"="Macrolides, lincosamides, \n & streptogramin B"))
#coerce antibiotic class column to factor
pico_processed$intervention_antibiotic_class<-as.factor(pico_processed$intervention_antibiotic_class)

# 3. FILL IN MISSING ANTIBIOTIC DOSAGES ---------------------------------------------------------

# #FILL IN MISSING DOSAGES
# #check which one(s) is NA
# pico_processed$study_publicationID[which(is.na(pico_processed$intervention_dosage_value))]
# #replace schmidt missing dosage with 6.6mgkg
# pico_processed$intervention_dosage_value[is.na(pico_processed$intervention_dosage_value)]<-6.6
# #replace schmidt missing dosage with 6.6mgkg
# pico_processed$intervention_dosage_unit[is.na(pico_processed$intervention_dosage_unit)]<-'mg_kg'

# 4. STANDARDISE ANTIBIOTIC DOSAGES TO MGKG ---------------------------------------------------------

#make new dosage vector for standardising doses  (easier to work with than directly on column)
dosage_mgkg<-pico_processed$intervention_dosage_value

#check antibiotic dosage units
plyr::count(pico_processed$intervention_dosage_unit)

#1. MG PER LB BODY WEIGHT: convert all mglb weights to kg
dosage_mgkg[which(pico_processed$intervention_dosage_unit=='mg_lb_bodyweight')]<-dosage_mgkg[which(pico_processed$intervention_dosage_unit=='mg_lb_bodyweight')]*2.20462

#2. G PER TONNE FEED/MG PER KG OR FEED/PPM (ALL SAME)

#check which studies are in g per tonne feed/mg per kg feed/ppm
pico_processed$study_publicationID[which(pico_processed$intervention_dosage_unit%in%c('g_tonne_feed','mg_kg_feed','ppm'))]
#convert g per tonne it into g per kg feed (/1000)
dosage_mgkg[which(pico_processed$intervention_dosage_unit%in%c('g_tonne_feed','mg_kg_feed','ppm'))]<-dosage_mgkg[which(pico_processed$intervention_dosage_unit%in%c('g_tonne_feed','mg_kg_feed','ppm'))]*0.02

#3. MG PER HEAD: convert mg_head weights to mg_kg, assuming a cattle weight from the known weights
#check which ones are mg_head
pico_processed$study_publicationID[which(pico_processed$intervention_dosage_unit=='mg_head')]
#look at distribution of animal weights (on entry)
##hist(pico_processed$population_weight_kg, breaks=50)
#calculate mean weight
meanweight<-mean(pico_processed$population_weight_kg, na.rm=T)
#make a weights vector
weights<-pico_processed$population_weight_kg
#replace weights that are not available with average weight across studies
weights[is.na(weights)]<-meanweight
#divide per-head values by weight
dosage_mgkg[which(pico_processed$intervention_dosage_unit=='mg_head')]<-dosage_mgkg[which(pico_processed$intervention_dosage_unit=='mg_head')]/
  weights[which(pico_processed$intervention_dosage_unit=='mg_head')]


#turn baseline doses to 0
dosage_mgkg[pico_processed$pre_or_post_intervention=='pre']<-0

#check distribution of doses
hist(dosage_mgkg)
#check distribution of log10 doses
hist(log10(dosage_mgkg+1))

#add doses column to dataframe
pico_processed$intervention_perdayUDD_mgkg<-dosage_mgkg
pico_processed$intervention_perdayUDDlog10_mgkg<-log10(dosage_mgkg)

#add total doses column to dataframe, calculated as length x dose
pico_processed$intervention_cumulativeUDD_mgkg<-as.numeric(dosage_mgkg*pico_processed$intervention_length_days)
pico_processed$intervention_cumulativeUDDlog10_mgkg<-log10(as.numeric(dosage_mgkg*pico_processed$intervention_length_days)+1)

#check distribution of doses
#hist(pico_processed$intervention_cumulativeUDDlog10_mgkg)
#check distribution of log10 doses
#hist(log10(pico_processed$intervention_cumulativeUDDlog10_mgkg+1))

# LOG INTERVENTION LENGTH -------------------------------------------------

#log intervention length
pico_processed$intervention_length_days<-log10(pico_processed$intervention_length_days)

# 6. OTHER COLUMNS -----------------------------------------------------------

pico_processed$outcome_measurement_type<-
  plyr::revalue(pico_processed$outcome_measurement_type,
                c("CFU" = "Direct plating \n on agar",
                  "qPCR" = "Quantitative PCR \n (qPCR)"))

#coerce unknown antibiotic free before to unknown
pico_processed$population_antibioticfreebefore[is.na(pico_processed$population_antibioticfreebefore)]<-'Unknown'

#add column specifying whether the outcome was sampled from a pen where intervention and control animals were cohoused (i.e. potential contamination)
pico_processed$outcome_frommixedpen_TF<-pico_processed$intervention_proportion_perpen!=1
# 
# #add a dummy risk of bias column (for now)
# levels(pico_processed$study_publicationID)
# lowrisk_studies<-c('agga2016','alali2009','beukers2015','goulart2022a','holman2019','inglis2005','inglis2020','kanwar2014','levent2022','long2022','lowrance2007','muller2018','murray2022','ohta2019','schmidt2020','zaheer2013')
# someconcerns_studies<-c('')
# highrisk_studies<-c('berge2005','edrington2014','goulart2022b','kanwar2013','lhermie2017','lefebvre2005', 'schmidt2013')
# 
# pico_processed$rob_overall<-NA
# 
# pico_processed$rob_overall[pico_processed$study_publicationID%in%lowrisk_studies]<-'Low risk'
# pico_processed$rob_overall[pico_processed$study_publicationID%in%someconcerns_studies]<-'Some concerns'
# pico_processed$rob_overall[pico_processed$study_publicationID%in%highrisk_studies]<-'High risk'
# 
# pico_processed$rob_overall<-as.factor(pico_processed$rob_overall)
# 
# pico_processed$rob_overall

# 7. ADD A COLUMN OF COLOURS FOR EACH STUDY, FOR PLOTTING ----------------------------------------------
#install.packages("devtools")
#require("devtools")
#remotes::install_github("KarstensLab/microshades", dependencies=TRUE)
# add colours column (for plotting)

#set seed so colour palette generation reproduces previous version in tidy_metadata()
set.seed(123)
#(re)generate global palette of colours for each study using microshades package
study_palette<-sample(unlist(microshades::microshades_cvd_palettes),
                        length(unique(pico_processed$study_studyID)), replace=F)

#assign the study names to the palette
names(study_palette)<-unique(pico_processed$study_studyID)

#sample study palette colours based on studyID
pico_processed$study_colour<-study_palette[match(pico_processed$study_studyID,names(study_palette))]

#make each publication within a study a unique point type (pch)
pico_processed$publication_pch<-NA

#vector of studies  
studies<-levels(pico_processed$study_studyID)
#point types in order of preferance
pchs<-c(16,21,1)

#cycle of studies
for (s in 1:length(studies)){

  #identify rows for this study  
  whichrows<-which(pico_processed$study_studyID==studies[s])

  #identify actual pub ids in this study
  pubs_in_study<-unique(pico_processed$study_publicationID[whichrows])

  #cycle over pubs within this study
  for (p in 1:length(pubs_in_study)){
  
    #for the publication pch vector, select the rows for each publication within study and assign either 16, 21 or 1
    pico_processed$publication_pch[which(pico_processed$study_publicationID==pubs_in_study[p])]<-pchs[p]
    
  }
}


#reorder levels
pico_processed$study_studyID<-
  factor(pico_processed$study_studyID,
         levels=c("USMARC_Tylmon",
                  "IowaState_Dano",
                  "Lethbridge_ChlortetChlortetsul",
                  "Lethbridge_Chlortetsul",
                  "Lethbridge_Tyl",
                  "TexasA&M_Cef",
                  "TexasA&M_CefCefchlortet",
                  "WKU_Tyl",
                  "TexasA&M_CefTul",
                  "TexasA&M_Tyl",
                  "TexasA&M_TylTylprobiotic",
                  "USMARC_Chlortet",
                  "USMARC_Tyl"))

# #save the palette as an attribute of the plotting_colour column so it can be used by ggplot
# plotting_colour<-data.table::setattr(plotting_colour,'palette', study_palette)
# 
# pico_processed<-cbind(pico_processed,plotting_colour=plotting_colour)
# 
# attr(pico_processed$plotting_colour)

# library(data.table)

# plotting_colour

# #studycolours<-microshades_cvd[as.factor(pico_witheffectsizes$study_studyID)]
# 
# set.seed(123)
# microshades_cvd<-sample(unlist(microshades::microshades_cvd_palettes),nlevels(pico_processed$study_studyID), replace=F)
# studycolours<-microshades_cvd[as.factor(pico_processed$study_studyID)]
# names(studycolours)<-pico_processed$study_studyID
# 
# #subset the microshades per study palette to just the studies included in the current model/sub-dataset
# cbpl_temp<-microshades_cvd[match(unique(H1_modelresults_list[[m]]$data$stdy), names(microshades_cvd))]
#
# pico_processed<-cbind(pico_processed,plotting_colour=studycolours)

# 8. ADD THE METADF TO PICO DF -----------------------------------------------

#pico_processed<-cbind(pico_processed, meta_df)
rm(meta_df)

# ADD EFFECTIVE SAMPLE SIZE AND PUBLICATION YEAR VARIABLES NEEDED FOR PUBLICATION BIAS  --------

pico_processed$n_tilda<-with(pico_processed, (4*(control_reps_totalresistancedeterminants*intervention_reps_totalresistancedeterminants)) / (control_reps_totalresistancedeterminants + intervention_reps_totalresistancedeterminants))
pico_processed$inv_n_tilda<-with(pico_processed, (control_reps_totalresistancedeterminants+intervention_reps_totalresistancedeterminants) / (control_reps_totalresistancedeterminants * intervention_reps_totalresistancedeterminants))
pico_processed$sqrt_inv_n_tilda<-with(pico_processed, sqrt(inv_n_tilda))

# #get publication year
pico_processed$publication_year<-readr::parse_number(as.character(pico_processed$study_publicationID))
#mean center it
pico_processed$publication_year_mc<-as.numeric(scale(pico_processed$publication_year, scale=FALSE))

# ADD STUDY DESIGNS -------------------------------------------------------

unique(pico_processed$study_studyID)

study_and_designs<-c(
                Lethbridge_Chlortetsul = "completely randomised design",
                Lethbridge_Tyl = "completely randomised design",
                USMARC_Tylmon = 'non-randomised design',
                USMARC_Tyl = "randomised block design",
                `TexasA&M_TylTylprobiotic` = "randomised block design",
                `TexasA&M_Tyl` = "randomised block design",
                WKU_Tyl = "randomised block design",
                #IowaState_Dano = "completely randomised design",
                #Lethbridge_ChlortetChlortetsul = "completely randomised design",
                USMARC_Chlortet = "randomised block design",
                `TexasA&M_Cef` = "completely randomised design",
                `TexasA&M_CefTul` = "randomised block design",
                `TexasA&M_CefCefchlortet` = "completely randomised design"
                #Lethbridge_ChlortetsulChlortetTetMonTyVir = "randomised block design",
                #Lethbridge_OxytetTul = "randomised block design",
                #Lethbridge_Tyl = "completely randomised design",
                #IowaState_Enro = "completely randomised design",
                #INRA_Marbo = 'OBS', #prospective cohort
                #Lethbridge_TyTulTil = "completely randomised design",
                #SouthDakotaState_Florfen = "completely randomised design",
                #USDAARS_Virg = "completely randomised design",
                #CRSAD_Oxytet = "completely randomised design",
                #USMARC_CefTulFlor = "randomised block design",
                #KansasState_Tyl = "randomised block design",
                )

pico_processed$study_studyID

pico_processed$study_studyID[which(is.na(study_and_designs[match(pico_processed$study_studyID,names(study_and_designs))]))]

study_studydesign<-as.factor(study_and_designs[match(pico_processed$study_studyID,names(study_and_designs))])

pico_processed<-tibble::add_column(pico_processed, study_studydesign, .after='study_studyID')

pico_processed$study_studydesign <- factor(pico_processed$study_studydesign, levels = c("completely randomised design", "randomised block design", "non-randomised design"))

# ADD PLOT-WORTHY STUDY AND PUB NAMES --------------------------------------------

#rename with more human-readable study names

#populate new vector with study IDs
pico_processed$study_studyID_pubready<-pico_processed$study_studyID

#relabel levels in it
pico_processed$study_studyID_pubready<-
  plyr::revalue(pico_processed$study_studyID_pubready, 
                c("USMARC_Chlortet" = "USMARC - Chlortetracycline",
                  "Lethbridge_Chlortetsul" = "Lethbridge - Chlortetracycline with sulfamethazine",
                  "Lethbridge_Tyl" = "Lethbridge - Tylosin",
                  "USMARC_Tylmon" = "USMARC - Tylosin with monensin",
                  "USMARC_Tyl" =   "USMARC - Tylosin",
                  "TexasA&M_TylTylprobiotic" = "Texas A&M - Tylosin; Tylosin with probiotic",
                  "TexasA&M_Tyl" = "Texas A&M - Tylosin",
                  "WKU_Tyl" = "West Kentucky University - Tylosin",
                  #"IowaState_Dano" = "Iowa State - Danofloxacin",
                  #"Lethbridge_ChlortetChlortetsul" = "Lethbridge - Chlortetracycline & Chlortetracycline with sulfamethazine", 
                  "TexasA&M_Cef" =  "Texas A&M - Ceftiofur",
                  "TexasA&M_CefTul" = "Texas A&M - Ceftiofur; Tulathromycin",
                  "TexasA&M_CefCefchlortet" =  "Texas A&M - Ceftiofur; Ceftiofur with Chlortetracycline")
                )

#"INRA - Marbofloxacin" = "INRA_Marbo",
  # "Iowa State - Enrofloxacin" = "IowaState_Enro",                        
  # "Lethbridge - Oxytetracycline; Tulathromycin" = "Lethbridge_OxytetTul",                    
  # "Lethbridge - Tylosin; Tulathromycin; Tilmicosin" = "Lethbridge_TyTulTil",                      
  # "South Dakota State - Florfenicol" = "SouthDakotaState_Florfen",                  
  # "USDAARS - Virginiamycin" = "USDAARS_Virg",                          
  # "USMARC - Ceftiofur; Tulathromycin; Florfenicol" ="USMARC_CefTulFlor",                      
  # "CRSAD - Oxytetracycline" = "CRSAD_Oxytet",                         
  # "Kansas State - Tylosin" = "KansasState_Tyl",
  # "Lethbridge - Chlortetracycline with sulfamethazine; Chlortetracycline; Monensin; Tylosin; Virginiamycin" = "Lethbridge_ChlortetsulChlortetTetMonTyVir" 



pico_processed$study_publicationID_pubready<-as.factor(pico_processed$study_publicationID)

pico_processed$study_publicationID_pubready<-
  plyr::revalue(pico_processed$study_publicationID_pubready, 
                c(
                  #"goulart2022a" = "Goulart et al. 2022a",
                  #"alexander2009" = "Alexander et al. 2009†",  #sharma also but not included in meta-analysis cos not full data
                  "inglis2019" =  "Inglis et al. 2019",
                   "beukers2015" = "Beukers et al. 2005",
                   "dornbach2025" = "Dornbach et al. 2025",
                   "schmidt2020" = "Schmidt et al. 2020", 
                   "murray2022" = "Murray et al. 2022",
                  "hoffman2025" = "Hoffman et al. 2025",
                   "agga2023" = "Agga et al. 2023",
                   "agga2016" = "Agga et al. 2016" ,
                   "lowrance2007" = "Lowrance et al. 2007",
                   "levent2022" = "Levent et al. 2022",
                   "kanwar2014" = "Kanwar et al. 2014", #kanwar2013 also but not included in meta-analysis cos data duplicated in ohta2019
                   "ohta2019" = "Ohta et al. 2019"
                   ))
# `Alali et al. 2009` = "alali2009",
# `Berge et al. 2005` ="berge2005",
# `Edrington et al. 2014` = "edrington2014",
# `Goulart et al. 2022b` = "goulart2022b" ,
# `Kanwar et al. 2013` = "kanwar2013",   
# `Lhermie et al. 2017` = "lhermie2017",   
# `Long et al. 2022` = "long2022",      
# `Zaheer et al. 2013` = "zaheer2013",
# `Inglis et al. 2005` = "inglis2005",
# `Lefebvre et al. 2005` = "lefebvre2005", 
# `Muller et al. 2018` = "muller2018",    
# `Sharma et al. 2008` = "sharma2008"
#,

#list the publications for each study
pubs_per_study<-as.list(tapply(pico_processed$study_publicationID_pubready,
                               pico_processed$study_studyID_pubready,
                               unique))

#collapse into one string separated by ;
pubs_per_study<-unlist(lapply(pubs_per_study,function(x){paste(x, collapse='; ')}))

#make a vector same length as number of points in dataframe, specifying the fellow publications of each point
fellowpubs<-pubs_per_study[match(pico_processed$study_studyID_pubready, names(pubs_per_study))]

#add brackets around so like a citation
fellowpubs<-paste("(",fellowpubs,")",sep='')

#paste together tidy publication name and 
pico_processed$study_and_pubs_plotready<-paste(pico_processed$study_studyID_pubready,
                                               '\n',
                                               fellowpubs)

# EDIT OUTCOME FORMAT -----------------------------------------------------

pico_processed$outcome_metaanalysisaccountedforpen<-plyr::revalue(pico_processed$outcome_metaanalysisaccountedforpen,
                                                                  c("aggregated_within_pen"="Averaged within pen \n (raw data or extracted at right level)",
                                                                    "not_possible" = "Not possible \n (extracted data)"
                                                                    )
                                                                  )

pico_processed$outcome_metaanalysisaccountedforblocking<-plyr::revalue(pico_processed$outcome_metaanalysisaccountedforblocking,
                                                                  c("aggregated_within_block"="Averaged within block \n (blocks as replicates)",
                                                                    "not_possible" = "Not possible \n (no blocking data)"
                                                                  )
)

# #rename ceph as third gen ceph as all ceftiofur (more specific)
# pico_processed$outcome_format_meta<-plyr::revalue(pico_processed$outcome_format_meta, 
#                                                   c("individual (intervention and pen effect conflated)"="individual \n(intervention and pen effect conflated)",
#                                                     "individual (pen effect not accounted for)"="individual \n (pen effect not accounted for)",
#                                                     "individual (pen as random effect)"="individual \n (pen as random effect)",
#                                                     "pen (individuals as pseudoreplicates)"="pen \n (individuals as pseudoreplicates)"))



# MAKE CLUSTERS FOR DIFFERENT LEVELS OF CLUSTERING OF OBSERVATIONS --------

#make a vector to indicate the clusters within which things should be aggregated
#the hashed out bits are the clusters that will be aggregated, the unhashed bits are the bits that will be kept
pico_processed$cluster_none<-paste(pico_processed$study_publicationID,
                                   pico_processed$study_studyID,
                                   pico_processed$study_block,
                                   pico_processed$intervention_antibiotic_name,
                                   pico_processed$intervention_dosage_value,
                                   pico_processed$cointervention1_antibiotic_name,
                                   pico_processed$cointervention2_antibiotic_name,
                                   pico_processed$cointervention3_nonantibiotic_name,
                                   pico_processed$outcome_sample_type,
                                   pico_processed$outcome_organism,
                                   pico_processed$outcome_resistance_target,
                                   pico_processed$outcome_study_days,
                                   sep='_')

#should equal length of pico_processed when all unhashed
length(unique(pico_processed$cluster_none))
#shows dupliacates (should be none when all unhashed)
pico_processed$cluster_none[duplicated(pico_processed$cluster_none)]

pico_processed$cluster_intervoutcome<-paste(pico_processed$study_publicationID,
                                            pico_processed$study_studyID,
                                            pico_processed$study_block,
                                            pico_processed$intervention_antibiotic_name,
                                            pico_processed$intervention_dosage_value,
                                            pico_processed$cointervention1_antibiotic_name,
                                            pico_processed$cointervention2_antibiotic_name,
                                            pico_processed$cointervention3_nonantibiotic_name,
                                            pico_processed$outcome_sample_type,
                                            pico_processed$outcome_organism,
                                            pico_processed$outcome_resistance_target,
                                            #pico_processed$outcome_study_days,
                                            sep='_')


pico_processed$cluster_intervoutcome_ignoreblock<-paste(pico_processed$study_publicationID,
                                            pico_processed$study_studyID,
                                            #pico_processed$study_block,
                                            pico_processed$intervention_antibiotic_name,
                                            pico_processed$intervention_dosage_value,
                                            pico_processed$cointervention1_antibiotic_name,
                                            pico_processed$cointervention2_antibiotic_name,
                                            pico_processed$cointervention3_nonantibiotic_name,
                                            pico_processed$outcome_sample_type,
                                            pico_processed$outcome_organism,
                                            pico_processed$outcome_resistance_target,
                                            #pico_processed$outcome_study_days,
                                            sep='_')

#should equal number of intervention outcome combos
length(unique(pico_processed$cluster_intervoutcome))

sort(unique(pico_processed$cluster_intervoutcome))

# units reported cluster - same outcome but reported in diff units
pico_processed$cluster_studyarm<-paste(pico_processed$study_publicationID,
                                       pico_processed$study_studyID,
                                       pico_processed$study_block,
                                       pico_processed$intervention_antibiotic_name,
                                       pico_processed$intervention_dosage_value,
                                       pico_processed$cointervention1_antibiotic_name,
                                       pico_processed$cointervention2_antibiotic_name,
                                       pico_processed$cointervention3_nonantibiotic_name,
                                       #pico_processed$outcome_sample_type,
                                       #pico_processed$outcome_organism,
                                       #pico_processed$outcome_resistance_target,
                                       #pico_processed$outcome_study_days,
                                       sep='_')

#organism cluster - same outcome but measured in different (micro)organisms
pico_processed$cluster_organism<-paste(pico_processed$study_publicationID,
                                       pico_processed$study_studyID,
                                       pico_processed$intervention_antibiotic_name,
                                       pico_processed$intervention_dosage_value,
                                       pico_processed$cointervention1_antibiotic_name,
                                       pico_processed$cointervention2_antibiotic_name,
                                       pico_processed$cointervention3_nonantibiotic_name,
                                       pico_processed$outcome_sample_type,
                                       pico_processed$outcome_organism,
                                       #pico_processed$outcome_resistance_target,
                                       #pico_processed$outcome_study_days,
                                       sep='_')

#resistance target cluster - same outcome but measured in different ways
pico_processed$cluster_resistancetarget<-paste(pico_processed$study_publicationID,
                                               pico_processed$study_studyID,
                                               pico_processed$study_block,
                                               pico_processed$intervention_antibiotic_name,
                                               pico_processed$intervention_dosage_value,
                                               pico_processed$cointervention1_antibiotic_name,
                                               pico_processed$cointervention2_antibiotic_name,
                                               pico_processed$cointervention3_nonantibiotic_name,
                                               pico_processed$outcome_sample_type,
                                               pico_processed$outcome_organism,
                                               pico_processed$outcome_resistance_target,
                                               # pico_processed$outcome_study_days,
                                               sep='_')

#resistance target cluster - same outcome but measured in different ways
pico_processed$cluster_sampletype<-paste(pico_processed$study_publicationID,
                                         pico_processed$study_studyID,
                                         pico_processed$study_block,
                                         pico_processed$intervention_antibiotic_name,
                                         pico_processed$intervention_dosage_value,
                                         pico_processed$cointervention1_antibiotic_name,
                                         pico_processed$cointervention2_antibiotic_name,
                                         pico_processed$cointervention3_nonantibiotic_name,
                                         pico_processed$outcome_sample_type,
                                         pico_processed$outcome_organism,
                                         pico_processed$outcome_resistance_target,
                                         #pico_processed$outcome_study_days,
                                         sep='_')