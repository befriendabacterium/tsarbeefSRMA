# 1. CHANGE TIME COLUMNS ---------------------------------------------------------

#BASELINES: Create a baseline column where the default time of baseline is zero days (most studies)
pico_processed$outcome_timeofbaseline_days<-0

#for particular studies where time of baseline is different to study days, change time of baseline
pico_processed$outcome_timeofbaseline_days[pico_processed$study_publicationID=='agga2016']<-5 #9th feb, then treatment from 14th-18th feb (5 days), then first measure on 23rd feb 5 days post treatment
pico_processed$outcome_timeofbaseline_days[pico_processed$study_publicationID=='inglis2020']<-5

## INTERVENTION LENGTH: coerce zeros in intervention length to NA --------------
#these are baseline measures so recode them as NAs
pico_processed$intervention_length_days[pico_processed$pre_or_post_intervention=='pre']<-NA

## ADD COLUMN FOR PRE OR POST INTERVENTION (i.e. is it baseline data ---------------------------

pico_processed$pre_or_post_intervention<-NA

#NB OUTCOME_STUDY_DAYS IS THE RANDOM EFFECT OF TIME
#if outcome_study_days is below 0, then count as pre-intervention
pico_processed$pre_or_post_intervention[which(pico_processed$outcome_study_days<=0)]<-'pre'

#if outcome_study_days is above 0, then count as post-intervention
pico_processed$pre_or_post_intervention[is.na(pico_processed$pre_or_post_intervention)]<-'post'

## ADD PERIODS COLUMN FOR TIME -----------------------------------------------------------------

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

# 3. STANDARDISE ANTIBIOTIC DOSAGES TO MGKG ---------------------------------------------------------

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

# 7. ADD A COLUMN OF COLOURS FOR EACH STUDY, FOR PLOTTING ----------------------------------------------

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
         levels=c("USMARC_Chlortet",
                  "Lethbridge_Chlortetsul",
                  "Lethbridge_Tyl",
                  "TexasA&M_TylTylprobiotic",
                  "USMARC_Tylmon",
                  "USMARC_Tyl",
                  "TexasA&M_Tyl",
                  "WKU_Tyl",
                  "TexasA&M_Cef",
                  "TexasA&M_CefTul",
                  "TexasA&M_CefCefchlortet"
                  ))

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
                USMARC_Chlortet = "randomised block design",
                Lethbridge_Chlortetsul = "completely randomised design",
                Lethbridge_Tyl = "completely randomised design",
                `TexasA&M_TylTylprobiotic` = "randomised block design",
                USMARC_Tylmon = 'non-randomised design',
                USMARC_Tyl = "randomised block design",
                `TexasA&M_Tyl` = "randomised block design",
                WKU_Tyl = "randomised block design",
                `TexasA&M_Cef` = "completely randomised design",
                `TexasA&M_CefTul` = "randomised block design",
                `TexasA&M_CefCefchlortet` = "completely randomised design"
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


pico_processed$study_publicationID_pubready<-as.factor(pico_processed$study_publicationID)

pico_processed$study_publicationID_pubready<-
  plyr::revalue(pico_processed$study_publicationID_pubready, 
                c(
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