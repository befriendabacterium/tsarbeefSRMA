# REMOVE NAs -----------------------------------------------------

#show studies with NA for total resistance determinants effect size
pico_witheffectsizes$study_publicationID[is.na(pico_witheffectsizes$yi_totalresistancedeterminants)]

#check NAs
plyr::count(is.na(pico_witheffectsizes$yi_totalresistancedeterminants))
plyr::count(is.na(pico_witheffectsizes$yi_totaldeterminants))
plyr::count(is.na(pico_witheffectsizes$yi_logitpropres))
plyr::count(is.na(pico_witheffectsizes$yi_arcsinpropres))

#only in totalresistance outcome - check why
NAs<-pico_witheffectsizes[which(is.na(pico_witheffectsizes$yi_totalresistancedeterminants)),]
NAs$cluster_intervoutcome
NAs$outcome_study_days
NAs$intervention_sd_totalresistancedeterminants
NAs$control_sd_totalresistancedeterminants

#seems to be because there is no standard deviation for these (the number of resistance determinants is set to the detection limit for all reps)

#remove non-calculable effect sizes
pico_witheffectsizes<-pico_witheffectsizes[!is.na(pico_witheffectsizes$yi_totalresistancedeterminants),]

#remove Agga swab samples as duplicated (use the CFU/ml version as more standard)
pico_witheffectsizes<-pico_witheffectsizes[-which(pico_witheffectsizes$outcome_measurement_type=='CFU_swab'),]

# SUBSET TO EFFECTS WITH BOTH PRE AND POST DATA ------------------------------------

#identify study arms with pre-intervention data
studyarms_with_preinterventiondata<-unique(pico_witheffectsizes$cluster_sampletype[pico_witheffectsizes$pre_or_post_intervention=='pre'])

#filter out study arms with pre-intervention data
pico_witheffectsizes<-pico_witheffectsizes[pico_witheffectsizes$cluster_sampletype%in%studyarms_with_preinterventiondata,]

#identify studies with post-intervention data
studyarms_with_postinterventiondata<-unique(pico_witheffectsizes$cluster_sampletype[pico_witheffectsizes$pre_or_post_intervention=='post'])

#filter out studies with post-intervention data
pico_witheffectsizes<-pico_witheffectsizes[pico_witheffectsizes$cluster_sampletype%in%studyarms_with_postinterventiondata,]

# ADD UNIT LEVEL IDs NOW WE HAVE ALL EFFECT SIZES FOR TO BE INCLUDED  ----------------------------------------------------------------

#add a unit level ID (just row number to each dataframe in the list, for modelling purposes)
pico_witheffectsizes$unitlevel_id<-1:nrow(pico_witheffectsizes)

# TIDY TIME COLUMNS --------------------------------------------------------

#outcome_study_days (used in random effect)
#set outcome_study_days below zero to zero so can log them
pico_witheffectsizes$outcome_study_days[pico_witheffectsizes$outcome_study_days<0]<-0
pico_witheffectsizes$outcome_study_days<-log10(pico_witheffectsizes$outcome_study_days+1)

#outcome_study_days (used in fixed effect)
#initiate the column
pico_witheffectsizes$outcome_time_days<-NA
#combine the during and after times into one column, logging in process
pico_witheffectsizes$outcome_time_days[pico_witheffectsizes$period=='during']<-log10(pico_witheffectsizes$outcome_timesinceintervention_start_days[pico_witheffectsizes$period=='during']+1)
pico_witheffectsizes$outcome_time_days[pico_witheffectsizes$period=='after']<-log10(pico_witheffectsizes$outcome_timesinceintervention_end_days[pico_witheffectsizes$period=='after']+1)

# REORDER LEVELS ------------------------------------------------------------

pico_witheffectsizes$study_studyID<-as.factor(pico_witheffectsizes$study_studyID)
pico_witheffectsizes$study_and_pubs_plotready<-as.factor(pico_witheffectsizes$study_and_pubs_plotready)

pico_witheffectsizes$study_studyID<-droplevels(pico_witheffectsizes$study_studyID)
levels(pico_witheffectsizes$study_studyID)

studyorder<-c("USMARC_Chlortet",
              "Lethbridge_Chlortetsul",
              "Lethbridge_Tyl",
              "TexasA&M_TylTylprobiotic",
              "USMARC_Tylmon",
              "USMARC_Tyl",
              "TexasA&M_Tyl",
              "WKU_Tyl",
              "TexasA&M_Cef",
              "TexasA&M_CefTul",
              "TexasA&M_CefCefchlortet")

desiredorder<-match(studyorder,levels(pico_witheffectsizes$study_studyID))
pico_witheffectsizes$study_studyID<-factor(pico_witheffectsizes$study_studyID,
                                           levels = levels(pico_witheffectsizes$study_studyID)[desiredorder])

levels(pico_witheffectsizes$study_studyID)

# LAST CHECKS --------------------------------------------------------

#drop levels 
pico_witheffectsizes<-droplevels(pico_witheffectsizes)

#check studies
unique(pico_witheffectsizes$study_studyID)

#check publications
unique(pico_witheffectsizes$study_publicationID)

#check class is still escalc
class(pico_witheffectsizes)

