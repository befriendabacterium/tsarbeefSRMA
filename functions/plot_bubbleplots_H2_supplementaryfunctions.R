# MODEL RESULTS -------------------------------------------------------------

############# Key functions #############

#' @title mod_results_MLJ
#' @description Using a \pkg{metafor} model object of class \code{rma} or \code{rma.mv}, this function creates a table of model results containing the mean effect size estimates for all levels of a given categorical moderator, and their corresponding confidence and prediction intervals. The function is capable of calculating marginal means from meta-regression models, including those with multiple moderator variables of mixed types (i.e. continuous and categorical variables).
#' @param model \code{rma.mv} model object
#' @param mod Moderator variable of interest that one wants marginal means for. Defaults to the intercept, i.e. \code{"1"}.
#' @param group The grouping variable that one wishes to plot beside total effect sizes, k. This could be study, species, or any grouping variable one wishes to present sample sizes for.
#' @param by Character vector indicating the name that predictions should be conditioned on for the levels of the moderator.
#' @param at List of levels one wishes to predict at for the corresponding variables in \code{by}. Used when one wants marginalised means. This argument can also be used to suppress levels of the moderator when argument \code{subset = TRUE}. Provide a list as follows: \code{list(mod = c("level1", "level2"))}.
#' @param weights How to marginalize categorical variables. The default is \code{weights = "prop"}, which weights moderator level means based on their proportional representation in the data. For example, if "sex" is a moderator, and males have a larger sample size than females, then this will produce a weighted average, where males are weighted more towards the mean than females. This may not always be ideal. In the case of sex, for example, males and females are roughly equally prevalent in a population. As such, you can give the moderator levels equal weight using \code{weights = "equal"}.
#' @param subset Used when one wishes to only plot a subset of levels within the main moderator of interest defined by \code{mod}. Default is \code{FALSE}, but use \code{TRUE} if you wish to subset levels of a moderator plotted (defined by \code{mod}) for plotting. Levels one wishes to plot are specified as a list, with the level names as a character string in the \code{at} argument. For subsetting to work, the \code{at} argument also needs to be specified so that the \code{mod_results_MLJ} function knows what levels one wishes to plot.
#' @param N The name of the column in the data specifying the sample size so that each effect size estimate is scaled to the sample size, N. Defaults to \code{NULL}, so that precision is used for scaling each raw effect size estimate instead of sample size.
#' @param upper Logical, defaults to \code{TRUE}, indicating that the first letter of the character string for the moderator variable should be capitalized.
#' @param ... Additional arguments passed to \code{emmeans::emmeans()}.
#' @return A data frame containing all the model results including mean effect size estimate, confidence and prediction intervals
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @examples \dontrun{
#' # Simple eklof data
#' data(eklof)
#' eklof<-metafor::escalc(measure="ROM", n1i=N_control, sd1i=SD_control,
#' m1i=mean_control, n2i=N_treatment, sd2i=SD_treatment, m2i=mean_treatment, data = eklof)
#' # Add the unit level predictor
#' eklof$Datapoint<-as.factor(seq(1, dim(eklof)[1], 1))
#' # fit a MLMR - accouting for some non-independence
#' eklof_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~ Grazer.type, random=list(~1|ExptID,
#' ~1|Datapoint), data = eklof)
#' results <- mod_results_MLJ(eklof_MR, mod = "Grazer.type", group = "ExptID")
#'
#' # Fish example demonstrating marginalised means
#' data(fish)
#' warm_dat <- fish
#' model <- metafor::rma.mv(yi = lnrr, V = lnrr_vi,
#' random = list(~1 | group_ID, ~1 | es_ID),
#' mods = ~ experimental_design + trait.type + deg_dif + treat_end_days,
#' method = "REML", test = "t",
#' control=list(optimizer="optim", optmethod="Nelder-Mead"), data = warm_dat)
#'   overall <- mod_results_MLJ(model, group = "group_ID")
#' across_trait <- mod_results_MLJ(model, group = "group_ID", mod = "trait.type")
#' across_trait_by_degree_diff <- mod_results_MLJ(model, group = "group_ID",
#' mod = "trait.type", at = list(deg_dif = c(5, 10, 15)), by = "deg_dif")
#' across_trait_by_degree_diff_at_treat_end_days10 <- mod_results_MLJ(model, group = "group_ID",
#' mod = "trait.type", at = list(deg_dif = c(5, 10, 15), treat_end_days = 10),
#' by = "deg_dif",data = warm_dat)
#' across_trait_by_degree_diff_at_treat_end_days10And50 <- mod_results_MLJ(model, group = "group_ID",
#' mod = "trait.type", at = list(deg_dif = c(5, 10, 15),
#'  treat_end_days = c(10, 50)), by = "deg_dif")
#' across_trait_by_treat_end_days10And50 <- mod_results_MLJ(model, group = "group_ID",
#' mod = "trait.type", at = list(deg_dif = c(5, 10, 15), treat_end_days = c(10, 50)),
#' by = "treat_end_days")
#' across_trait_by_treat_end_days10And50_ordinaryMM <- mod_results_MLJ(model, group = "group_ID",
#' mod = "trait.type", at = list(deg_dif = c(5, 10, 15), treat_end_days = c(10, 50)),
#' by = "treat_end_days", weights = "prop")
#'
#' # Fish data example with a heteroscedastic error
#' model_het <- metafor::rma.mv(yi = lnrr, V = lnrr_vi, random = list(~1 | group_ID, ~1 + trait.type| es_ID), mods = ~ trait.type + deg_dif, method = "REML", test = "t", rho = 0, struc = "HCS", control=list(optimizer="optim", optmethod="Nelder-Mead"), data = warm_dat)
#' HetModel <- mod_results_MLJ(model_het, group = "group_ID", mod = "trait.type", at = list(deg_dif = c(5, 10, 15)), by = "deg_dif", weights = "prop")
#' orchard_plot(HetModel, xlab = "lnRR")
#' }
#' @export
#'
#'
# We will need to make sure people use "1" or"moderator_names"

mod_results_MLJ <- function(model, mod = "1", group,  N = NULL,  weights = "prop", by = NULL, at = NULL, subset = FALSE, upper = TRUE, ...){
  
  if(any(grepl("-1|0", as.character(model$formula.mods)))){
    warning("It is recommended that you fit the model with an intercept. Unanticipated errors can occur otherwise.")
  }
  
  if(any(model$struct %in% c("GEN", "HCS"))){ #**MLJ NOTE**: DOESN'T PICK UP MY INNER OUTER RMA MODEL WITH 'CAR' STRUCT
    warning("We noticed you're fitting an ~inner|outer rma model ('random slope'). There are circumstances where the prediction intervals for such models are calculated incorrectly. Please check your results carefully.")
  }
  
  if(missing(model)){
    stop("Please specify the 'model' argument by providing rma.mv or rma model object. See ?mod_results")
  }
  
  if(all(class(model) %in% c("robust.rma", "rma.mv", "rma", "rma.uni")) == FALSE) {stop("Sorry, you need to fit a metafor model of class rma.mv, rma, or robust.rma")}
  
  if(missing(group)){
    stop("Please specify the 'group' argument by providing the name of the grouping variable. See ?mod_results")
  }
  
  
  if(is.null(stats::formula(model))){ ##**NOTE** Not sure we need this bit of code anymore. Left here for now
    #model <- stats::update(model, "~1")
    model$formula.mods <- ~ 1
    #dat_tmp <- model$data$`1` <- "Intrcpt"
    #model$data <- dat_tmp
  }
  
  if(model$test == "t"){
    df_mod = as.numeric(model$ddf[[1]])
  } else{
    df_mod = 1.0e6 # almost identical to z value
  }
  
  # Extract the data from the model object
  data <- model$data
  
  # Check if missing values exist and use complete case data
  if(any(model$not.na == FALSE)){
    data <- data[model$not.na,]
  }
  
  #CATEGORICAL MODERATOR ----------------------------------------------------
  # prediction method/code adapted from cornbunting's solution at https://stackoverflow.com/a/64493228/8349925
  if(is.character(data[[mod]]) | is.factor(data[[mod]]) | is.null(data[[mod]])) {
    
    #coerce moderator to factor if it's a character - or factor (already) or NULL
    data[[mod]]<-as.factor(data[[mod]])
    
    #generate x values (levels of categories) from which to predict y values
    xs = levels(data[[mod]])
    
    #if there is no conditioning variable...
    if(is.null(by)){
      
      #initiate dataframe with x values and a blank 'by' column
      newgrid<-data.frame(xs=xs,by=NA)
      
      #if there is a conditioning variable...
    } else{
      
      #initiate dataframe by generating x values for all levels of factors using same term names in same order as in model formula
      newgrid <- data.frame(expand.grid(xs=xs,by=levels(as.factor(data[,by]))))
      
    }
    
    #rename columns with the moderator and by variables
    colnames(newgrid)<-c(mod,by)
    
    #create the new model matrix
    predgrid<-model.matrix(model$formula.mods,data=newgrid)
    
    #if any of columns is the intercept, remove it from the prediction grid so predict() works properly
    if(any(colnames(predgrid)!="(Intercept)")){
      predgrid<-predgrid[,colnames(predgrid)!="(Intercept)"]
      #colnames(predgrid)[1]<-'intrcpt'
    }
    
    if(is.null(by)){
      
      #predict onto the new model matrix
      mypreds <- as.data.frame(predict.rma(model, tau2.levels=1:nlevels(data[[mod]]), newmods=predgrid))
   
    } else{

    #predict onto the new model matrix
    mypreds <- as.data.frame(predict.rma(model, tau2.levels=rep(1:nlevels(data[[mod]]),nlevels(as.factor(data[[by]]))), newmods=predgrid))
    
    }
    
    #make mod_table
    mod_table <- data.frame(name = firstup(as.character(newgrid[,mod]), upper = T),
                            condition = newgrid[,by],
                            estimate = mypreds$pred,
                            lowerCL = mypreds$ci.lb,
                            upperCL = mypreds$ci.ub,
                            lowerPR = mypreds$pi.lb,
                            upperPR = mypreds$pi.ub)
    
    # Extract data
    data2 <- get_data_raw(model, mod, group, N, at = at, subset)
    
    mod_table$name <- factor(mod_table$name,
                             levels = mod_table$name,
                             labels = mod_table$name)
    
    #MUST CHANGE FIRST COL NAME TO INTERCEPT WHEN PRESENT
    
  }
  
  # CONTINUOUS MODERATOR ----------------------------------------------------
  # prediction method/code adapted from cornbunting's solution at https://stackoverflow.com/a/64493228/8349925

  else{
    
    # model2<-sets[[s]][[2]]
    # model3<-sets[[s]][[3]]
    # data<-model$data
    
    #generate x values (continuous x values) from which to predict y values
    xs <- seq(min(data[,mod], na.rm = TRUE), max(data[,mod], na.rm = TRUE), length.out = 100)
    
    #if there is no conditioning variable...
    if(is.null(by)){
      
      #initiate dataframe with x values and a blank 'by' column
      newgrid<-data.frame(xs=xs,by=NA)
      
      #if there is a conditioning variable...
    } else{
      
      #initiate dataframe by generating x values for all levels of factors using same term names in same order as in model formula
      newgrid <- data.frame(expand.grid(xs=xs,by=levels(as.factor(data[,by]))))
      
    }
    
    #rename columns with the moderator and by variables
    colnames(newgrid)<-c(mod,by)
    
    #list other variables in the model
    othervars<-names(coef(model))[-1][grep(mod,names(coef(model))[-1], invert = T)]
                                      
    #if there are other variables, set to mean and add to 'newgrid' prediction matrix
    if(length(othervars)!=0){
    newgrid[,othervars]<-colMeans(as.data.frame(data[,othervars], na.rm = T))
    }
    
    #create the new model matrix and remove the intercept
    predgrid<-model.matrix(model$formula.mods,data=newgrid)[,-1]
    
    #predict onto the new model matrix
    mypreds <- as.data.frame(predict.rma(model, newmods=predgrid))
    
    #make mod_table
    mod_table <- data.frame(moderator = newgrid[,mod],
                            condition = newgrid[,by],
                            estimate = mypreds$pred,
                            lowerCL = mypreds$ci.lb,
                            upperCL = mypreds$ci.ub,
                            lowerPR = mypreds$pi.lb,
                            upperPR = mypreds$pi.ub)
    
  }
  
  # extract data
  data2 <- get_data_raw_cont(model, mod, group, N, by = by)
  
  output <- list(mod_table = mod_table,
                 data = data2)
  
  class(output) <- c("orchard", "data.frame")
  
  return(output)
}


############# Key Sub-functions #############

#' @title pred_interval_esmeans
#' @description Function to get prediction intervals (credibility intervals) from \code{esmeans} objects (\pkg{metafor}).
#' @param model \code{rma.mv} object.
#' @param mm result from \code{emmeans::emmeans} object.
#' @param mod Moderator of interest.
#' @param ... other arguments passed to function.
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @export


pred_interval_esmeans <- function(model, mm, mod, ...){
  
  tmp <- summary(mm)
  tmp <- tmp[ , ]
  test.stat <- stats::qt(0.975, tmp$df[[1]])
  
  if(length(model$tau2) <= 1 | length(model$gamma2) <= 1){ # Note this should fix #46 but code is repetitive and needs to be cleaned up. Other issue is how this plays with different rma. objects. uni models will treat slots for gamma NULL and we need to deal with this.
    sigmas <- sum(model$sigma2)
    taus   <- model$tau2
    gamma2 <- ifelse(is.null(model$gamma2), 0, model$gamma2)
    PI <- test.stat * base::sqrt(tmp$SE^2 + sigmas + taus + gamma2)
  } else {
    sigmas <- sum(model$sigma2)
    taus   <- model$tau2
    gammas <- model$gamma2
    w_tau <- model$g.levels.k
    w_gamma <- model$g.levels.k
    
    if(mod == "1"){
      tau <- weighted_var(taus, weights = w_tau)
      gamma <- weighted_var(gamma, weights = w_gamma)
      PI <- test.stat * sqrt(tmp$SE^2 + sigmas + tau + gamma)
      
    } else {
      PI <- test.stat * sqrt(tmp$SE^2 + sigmas + taus + gammas)
    }
  }
  
  tmp$lower.PI <- tmp$emmean - PI
  tmp$upper.PI <- tmp$emmean + PI
  
  # renaming "overall" to ""
  if(tmp[1,1] == "overall"){tmp[,1] <- "intrcpt"}
  
  return(tmp)
}

#' @title get_data_raw
#' @description Collects and builds the data used to fit the \code{rma.mv} or \code{rma} model in \pkg{metafor}.
#' @param model \code{rma.mv} object.
#' @param mod the moderator variable.
#' @param group The grouping variable that one wishes to plot beside total effect sizes, k. This could be study, species, or whatever other grouping variable one wishes to present sample sizes.
#' @param N The name of the column in the data specifying the sample size, N. Defaults to \code{NULL}, so precision is plotted instead of sample size.
#' @param at List of moderators. If \code{at} is equal to \code{mod} then levels specified within \code{at} will be used to subset levels when \code{subset = TRUE}. Otherwise, it will marginalise over the moderators at the specified levels.
#' @param subset Whether or not to subset levels within the \code{mod} argument. Defaults to \code{FALSE}.
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @return Returns a data frame
#' @export
#' @examples \dontrun{
#' data(fish)
#' warm_dat <- fish
#' model <- metafor::rma.mv(yi = lnrr, V = lnrr_vi, random = list(~1 | group_ID, ~1 | es_ID), mods = ~ experimental_design + trait.type + deg_dif + treat_end_days, method = "REML", test = "t", data = warm_dat, control=list(optimizer="optim", optmethod="Nelder-Mead"))
#'  test <- get_data_raw(model, mod = "trait.type", group = "group_ID", at = list(trait.type = c("physiology", "morphology")))
#'  test2 <- get_data_raw(model, mod = "1", group = "group_ID")
#'
#'  data(english)
#'  # We need to calculate the effect sizes, in this case d
#'  english <- escalc(measure = "SMD", n1i = NStartControl, sd1i = SD_C, m1i = MeanC, n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, var.names=c("SMD","vSMD"))
#'  model <- rma.mv(yi = SMD, V = vSMD, random = list( ~ 1 | StudyNo, ~ 1 | EffectID), data = english)
#'  test3 <-  get_data_raw(model, mod = "1", group = "StudyNo")}

get_data_raw <- function(model, mod, group, N = NULL, at = NULL, subset = TRUE){
  if(missing(group)){
    stop("Please specify the 'group' argument by providing the name of the grouping variable. See ?mod_results_MLJ")
  }
  
  # Extract the data from the model object
  data <- model$data
  
  # Check if missing values exist and use complete case data
  if(any(model$not.na == FALSE)){
    data <- data[model$not.na,]
  }
  
  if(!is.null(at) & subset){
    # Find the at slot in list that pertains to the moderator and extract levels
    at_mod <- at[[mod]]
    position2 <- which(data[,mod] %in% at_mod)
    # Subset the data to only the levels in the moderator
    data <- data[position2,]
    yi <- model$yi[position2]
    vi <- model$vi[position2]
    type <- attr(model$yi, "measure")
  } else {
    # Extract effect sizes
    yi <- model$yi
    vi <- model$vi
    type <- attr(model$yi, "measure")
  }
  if(mod == "1"){
    moderator <- "Intrcpt"
  }else{
    # Get moderator
    moderator <- as.character(data[[mod]]) # Could default to base instead of tidy
    moderator <- firstup(moderator)
  }
  # Extract study grouping variable to calculate the
  stdy <- data[[group]] # Could default to base instead of tidy
  data_reorg <- data.frame(yi, vi, moderator, stdy, type)
  #names(data_reorg)[4] <- "stdy" # sometimes stdy gets replaced by group's names
  row.names(data_reorg) <- 1:nrow(data_reorg)
  
  if(is.null(N) == FALSE){
    data_reorg$N <- data[ ,N]
  }
  
  return(data_reorg)
}

#' @title get_data_raw_cont
#' @description Collects and builds the data used to fit the \code{rma.mv} or \code{rma} model in \pkg{metafor} when a continuous variable is fit within a model object.
#' @param model \code{rma.mv} object.
#' @param mod the moderator variable.
#' @param group The grouping variable that one wishes to plot beside total effect sizes, k. This could be study, species or whatever other grouping variable one wishes to present sample sizes.
#' @param N  The name of the column in the data specifying the sample size, N. Defaults to \code{NULL} so that precision is plotted instead of sample size.
#' @param by Character name(s) of the 'condition' variables to use for grouping into separate tables.
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @return Returns a data frame
#' @export

#TODO what if there is no "by"

get_data_raw_cont <- function(model, mod, group, N = NULL, by){
  if(missing(group)){
    stop("Please specify the 'group' argument by providing the name of the grouping variable. See ?mod_results_MLJ")
  }
  
  # Extract the data from the model object
  data <- model$data
  
  # Check if missing values exist and use complete case data
  if(any(model$not.na == FALSE)){
    data <- data[model$not.na,]
  }
  
  # Extract effect sizes
  yi <- model$yi
  vi <- model$vi
  type <- attr(model$yi, "measure")
  # Get moderator
  moderator <- data[[mod]] # Could default to base instead of tidy
  #names(moderator) <  "moderator"
  if(is.null(by)){
    condition <- data[ , by]
  }else{
    condition <- data[[by]]
  }
  #names(condition) <  "condition"
  # Extract study grouping variable to calculate the
  stdy <- data[[group]] # Could default to base instead of tidy
  data_reorg <- data.frame(yi, vi, moderator, condition, stdy, type)
  # if(!is.na(names(data_reorg)[names(data_reorg) == by]) == TRUE) {  ## FAILING HERE
  #   names(data_reorg)[names(data_reorg) == by] <- "condition"
  # }
  #names(data_reorg)[5] <- "stdy" # sometimes stdy gets replaced by group's names
  row.names(data_reorg) <- 1:nrow(data_reorg)
  
  if(is.null(N) == FALSE){
    data_reorg$N <- data[ ,N]
  }
  
  return(data_reorg)
}

############# Helper-functions #############

#' @title firstup
#' @description Uppercase moderator names
#' @param x a character string
#' @param upper logical indicating if the first letter of the character string should be capitalized. Defaults to TRUE.
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @return Returns a character string with all combinations of the moderator level names with upper case first letters
#' @export

firstup <- function(x, upper = TRUE) {
  if(upper){
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  } else{ x }
}


#' @title print.orchard
#' @description Print method for class 'orchard'
#' @param x an R object of class orchard
#' @param ... Other arguments passed to print
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @return Returns a data frame
#' @export
#'

print.orchard <- function(x, ...){
  return(print(x$mod_table))
}

#' @title weighted_var
#' @description Calculate weighted variance
#' @param x A vector of tau2s to be averaged
#' @param weights Weights, or sample sizes, used to average the variance
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @return Returns a vector with a single weighted variance
#' @export
#'

weighted_var <- function(x, weights){
  weight_var <- sum(x * weights) / sum(weights)
  return(weight_var)
}


#' @title num_studies
#' @description Computes how many studies are in each level of categorical moderators of a \code{rma.mv} model object.
#' @param mod Character string describing the moderator of interest.
#' @param data Raw data from object of class "orchard"
#' @param group A character string specifying the column name of the study ID grouping variable.
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @return Returns a table with the number of studies in each level of all parameters within a \code{rma.mv} or \code{rma} object.
#' @export
#' @examples
#' \dontrun{data(fish)
#'warm_dat <- fish
#' model <- metafor::rma.mv(yi = lnrr, V = lnrr_vi, random = list( ~1 | es_ID,~1 | group_ID), mods = ~experimental_design-1, method = "REML", test = "t", data = warm_dat, control=list(optimizer="optim", optmethod="Nelder-Mead"))
#' num_studies(model$data, experimental_design, group_ID)
#' }

num_studies <- function(data, mod, group){
  
  # Summarize the number of studies within each level of moderator
  table <- data        %>%
    dplyr::group_by({{mod}}) %>%
    dplyr::summarise(stdy = length(unique({{group}})))
  
  table <- table[!is.na(table$moderator),]
  # Rename, and return
  colnames(table) <- c("Parameter", "Num_Studies")
  return(data.frame(table))
  
}

# ORCHARD PLOT -------------------------------------------------------------
#' @title orchard_plot
#' @description Using a \pkg{metafor} model object of class \code{rma} or \code{rma.mv}, or a results table of class \code{orchard}, it creates an orchard plot from mean effect size estimates for all levels of a given categorical moderator, and their corresponding confidence and prediction intervals.
#' @param object model object of class \code{rma.mv}, \code{rma}, or \code{orchard} table of model results.
#' @param mod the name of a moderator. Defaults to \code{"1"} for an intercept-only model. Not needed if an \code{orchard_plot} is provided with a \code{mod_results} object of class \code{orchard}.
#' @param group The grouping variable that one wishes to plot beside total effect sizes, k. This could be study, species, or any grouping variable one wishes to present sample sizes for. Not needed if an \code{orchard_plot} is provided with a \code{mod_results} object of class \code{orchard}.
#' @param by Character vector indicating the name that predictions should be conditioned on for the levels of the moderator.
#' @param at List of levels one wishes to predict at for the corresponding varaibles in 'by'. Used when one wants marginalised means. This argument can also be used to suppress levels of the moderator when argument \code{subset = TRUE}. Provide a list as follows: \code{list(mod = c("level1", "level2"))}.
#' @param weights Used when one wants marginalised means. How to marginalize categorical variables. The default is \code{weights = "prop"}, which weights moderator level means based on their proportional representation in the data. For example, if "sex" is a moderator, and males have a larger sample size than females, then this will produce a weighted average, where males are weighted more towards the mean than females. This may not always be ideal. In the case of sex, for example, males and females are roughly equally prevalent in a population. As such, you can give the moderator levels equal weight using \code{weights = "equal"}.
#' @param xlab The effect size measure label.
#' @param N The name of the column in the data specifying the sample size so that each effect size estimate is scaled to the sample size, N. Defaults to \code{NULL}, so that precision is used for scaling each raw effect size estimate instead of sample size.
#' @param alpha The level of transparency for effect sizes represented in the orchard plot.
#' @param angle The angle of y labels. The default is 90 degrees.
#' @param cb If \code{TRUE}, it uses 20 colour blind friendly colors.
#' @param k If \code{TRUE}, it displays k (number of effect sizes) on the plot.
#' @param g If \code{TRUE}, it displays g (number of grouping levels for each level of the moderator) on the plot.
#' @param transfm If set to \code{"tanh"}, a tanh transformation will be applied to effect sizes, converting Zr to a correlation or pulling in extreme values for other effect sizes (lnRR, lnCVR, SMD). Defaults to \code{"none"}.
#' @param condition.lab Label for the condition being marginalized over.
#' @param tree.order Order in which to plot the groups of the moderator when it is a categorical one. Should be a vector of equal length to number of groups in the categorical moderator, in the desired order (bottom to top, or left to right for flipped orchard plot)
#' @param trunk.size Size of the mean, or central point.
#' @param branch.size Size of the confidence intervals.
#' @param twig.size Size of the prediction intervals.
#' @param legend.pos Where to place the legend. To remove the legend, use \code{legend.pos = "none"}.
#' @param k.pos Where to put k (number of effect sizes) on the plot. Users can specify the exact position or they can use specify \code{"right"}, \code{"left"},  or \code{"none"}. Note that numeric values (0, 0.5, 1) can also be specified and this would give greater precision.
#' @param colour Colour of effect size shapes. By default, effect sizes are colored according to the \code{mod} argument. If \code{TRUE}, they are colored according to the grouping variable
#' @param fill If \code{TRUE}, effect sizes will be filled with colours. If \code{FALSE}, they will not be filled with colours.
#' @param weights Used when one wants marginalised means. How to marginalize categorical variables. The default is \code{weights = "prop"}, which weights moderator level means based on their proportional representation in the data. For example, if "sex" is a moderator, and males have a larger sample size than females, then this will produce a weighted average, where males are weighted more towards the mean than females. This may not always be ideal. In the case of sex, for example, males and females are roughly equally prevalent in a population. As such, you can give the moderator levels equal weight using \code{weights = "equal"}.
#' @param upper Logical, defaults to \code{TRUE}, indicating that the first letter of the character string for the moderator variable should be capitalized.
#' @param flip Logical, defaults to \code{TRUE}, indicating whether the plot should be flipped.
#' @return Orchard plot
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @examples
#' \dontrun{
#' data(eklof)
#' eklof<-metafor::escalc(measure="ROM", n1i=N_control, sd1i=SD_control,
#' m1i=mean_control, n2i=N_treatment, sd2i=SD_treatment, m2i=mean_treatment,
#' data=eklof)
#' # Add the unit level predictor
#' eklof$Datapoint<-as.factor(seq(1, dim(eklof)[1], 1))
#' # fit a MLMR - accounting for some non-independence
#' eklof_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~ Grazer.type-1,
#' random=list(~1|ExptID, ~1|Datapoint), data=eklof)
#' results <- mod_results(eklof_MR, mod = "Grazer.type", group = "ExptID")
#' orchard_plot(results, mod = "Grazer.type",
#' group = "ExptID", xlab = "log(Response ratio) (lnRR)")
#' # or
#' orchard_plot(eklof_MR, mod = "Grazer.type", group = "ExptID",
#' xlab = "log(Response ratio) (lnRR)")
#'
#' # Example 2
#' data(lim)
#' lim$vi<- 1/(lim$N - 3)
#' lim_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~Phylum-1, random=list(~1|Article,
#' ~1|Datapoint), data=lim)
#' orchard_plot(lim_MR, mod = "Phylum", group = "Article",
#' xlab = "Correlation coefficient", transfm = "tanh", N = lim$N)
#' }
#' @export

orchard_plot_MLJ <- function (object,
                              yi = NULL, vi = NULL, stdy = NULL, pch=1,
                              mod = "1", group, xlab, N = NULL, alpha = 0.5,
                              angle = 90, cb = TRUE, k = TRUE, g = TRUE, tree.order = NULL,
                              trunk.size = 3, branch.size = 1.2, twig.size = 0.5,
                              transfm = c("none","tanh"), condition.lab = "Condition", legend.pos = c("bottom.right",
                                                                                                      "bottom.left", "top.right", "top.left", "top.out", "bottom.out",
                                                                                                      "none"), k.pos = c("right", "left", "none"), colour = FALSE,
                              fill = TRUE, weights = "prop", by = NULL, at = NULL, upper = FALSE,
                              flip = TRUE)
{
  transfm <- match.arg(NULL, choices = transfm)
  legend.pos <- match.arg(NULL, choices = legend.pos)
  k.pos <- match.arg(NULL, choices = k.pos)
  if (any(class(object) %in% c("robust.rma", "rma.mv", "rma",
                               "rma.uni"))) {
    if (mod != "1") {
      results <- mod_results_MLJ(object, mod, group,
                                 N, by = by, at = at, weights = weights, upper = upper)
    }
    else {
      results <- mod_results_MLJ(object, mod = "1",
                                 group, N, by = by, at = at, weights = weights,
                                 upper = upper)
    }
    
    #create object for model
    mod_table <- results$mod_table
    #create object for data underlying model
    data_trim <- results$data
    
    #MLJ added uppercase
    data_trim$moderator <- factor(firstup(as.character(data_trim$moderator), upper=T), levels = mod_table$name,
                                  labels = mod_table$name)
    
  }
  
  if (any(class(object) %in% c("orchard"))) {
    results <- object
    
    #create object for model
    mod_table <- results$mod_table
    #create object for data underlying model
    data_trim <- results$data
    
    #MLJ added uppercase
    data_trim$moderator <- factor(firstup(as.character(data_trim$moderator), upper=T), levels = mod_table$name,
                                  labels = mod_table$name)
    
  }
  
  #if the class of the object is 'escalc' and 'data.frame' or 'only 'data.frame'
  if(all(class(object) %in% c("escalc","data.frame"))|all(class(object) %in% c("data.frame"))) {
    
    if(is.null(yi)){
      stop("Must specify 'yi' when providing a dataframe as the object to be plotted. See ?bubble_plot")
    }
    if(is.null(vi)){
      stop("Must specify 'vi' when providing a dataframe as the object to be plotted. See ?bubble_plot")
    }
    if(is.null(stdy)){
      stop("Must specify 'stdy' when providing a dataframe as the object to be plotted. See ?bubble_plot")
    }
    if(is.null(mod)){
      stop("Must specify 'mod' when providing a dataframe as the object to be plotted. See ?bubble_plot")
    }
    
    #create mod_table object with just names (because there is no model)
    mod_table<-data.frame(name=levels(as.factor(object[[mod]])))
    
    #create object for data underlying model from the data directly provided
    data_trim <- data.frame(yi = object[,yi], vi = object[,vi], stdy=object[,stdy])
    data_trim <- data_trim[complete.cases(data_trim),]
    data_trim$moderator<-object[[mod]]
    
    #if a moderating variable is supplied, add to df
    if(!is.null(by)){
      #add the conditioning variable to 'model' table if present (workaround)
      mod_table <- expand.grid(name = mod_table$name, condition = levels(as.factor(object[,by])))
      #add conditioning variable to data_trim
      data_trim$condition<-object[,by]
    }
    
  }
  
  data_trim$scale <- (pch/sqrt(data_trim[, "vi"]))
  legend <- "Precision (1/SE)"
  if (!is.null(tree.order) & length(tree.order) != nlevels(data_trim[,
                                                                     "moderator"])) {
    stop("Length of 'tree.order' does not equal number of categories in moderator")
  }
  if (!is.null(tree.order)) {
    data_trim$moderator <- factor(data_trim$moderator, levels = tree.order,
                                  labels = tree.order)
    mod_table <- mod_table %>% dplyr::arrange(factor(name,
                                                     levels = tree.order))
  }
  if (is.null(N) == FALSE) {
    data_trim$scale <- data_trim$N
    legend <- paste0("Sample Size ($\\textit{N}$)")
  }
  if (transfm == "tanh") {
    cols <- sapply(mod_table, is.numeric)
    mod_table[, cols] <- Zr_to_r(mod_table[, cols])
    data_trim$yi <- Zr_to_r(data_trim$yi)
    label <- xlab
  }
  else {
    label <- xlab
  }
  mod_table$K <- as.vector(by(data_trim, data_trim[, "moderator"],
                              function(x) length(x[, "yi"])))
  mod_table$g <- as.vector(num_studies(data_trim, moderator,
                                       stdy)[, 2])
  
  group_no <- length(unique(mod_table[, "name"]))
  cbpl <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288",
            "#AA4499", "#44AA99", "#999933", "#882255", "#661100",
            "#6699CC", "#888888", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  if (colour == TRUE) {
    color <- as.factor(data_trim$stdy)
    color2 <- NULL
  }
  
  else {
    color <- data_trim$mod
    color2 <- mod_table$name
  }
  if (fill == TRUE) {
    fill <- color
  }
  else {
    fill <- NULL
  }
  
  # PLOT WITHOUT ANOTHER CONDITIONING VARIABLE IN ADDITION TO THE MODERATOR ---------------------------------
  
  #if there is not another conditioning variable...
  if (names(mod_table)[2] != "condition") {
    
    # basic plot with just points
    plot <- ggplot2::ggplot() + ggbeeswarm::geom_quasirandom(data = data_trim,
                                                             ggplot2::aes(y = yi, x = moderator, size = scale,
                                                                          colour = color, fill = fill), alpha = alpha,
                                                             shape = 21) + ggplot2::geom_hline(yintercept = 0,
                                                                                               linetype = 2, colour = "black", alpha = alpha) +
      ggplot2::theme_bw() + ggplot2::guides(fill = "none",
                                            colour = "none") + ggplot2::theme(legend.title = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.direction = "horizontal") +
      ggplot2::theme(legend.background = ggplot2::element_blank()) +
      ggplot2::labs(y = label, x = "", size = latex2exp::TeX(legend)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10,
                                                         colour = "black", hjust = 0.5, angle = angle))
    
    #if a model is provided, add model fit to plot
    if(any(class(object) %in% c("robust.rma", "rma.mv", "rma", "rma.uni", "orchard"))){
      plot <- plot +
        # confidence interval
        ggplot2::geom_linerange(data = mod_table, ggplot2::aes(x = name,
                                                               ymin = lowerCL, ymax = upperCL), size = branch.size) +
        # mean and prediction interval
        ggplot2::geom_pointrange(data = mod_table, ggplot2::aes(y = estimate,
                                                                x = name, ymin = lowerPR, ymax = upperPR, fill = color2),
                                 size = twig.size, fatten = trunk.size, shape = 21)
      
    }
  }
  
  # PLOT WITH ANOTHER CONDITIONING VARIABLE IN ADDITION TO THE MODERATOR ---------------------------------
  
  #if there is a conditioning variable...
  else {
    condition_no <- length(unique(mod_table[, "condition"]))
    plot <- ggplot2::ggplot() + ggbeeswarm::geom_quasirandom(data = data_trim,
                                                             ggplot2::aes(y = yi, x = moderator, size = scale,
                                                                          colour = color, fill = fill), alpha = alpha,
                                                             shape = 21) + ggplot2::geom_hline(yintercept = 0,
                                                                                               linetype = 2, colour = "black", alpha = alpha) +
      ggplot2::scale_shape_manual(values = 20 + (1:condition_no)) + ggplot2::theme_bw() + ggplot2::guides(fill = "none",
                                                                                                          colour = "none") + ggplot2::theme(legend.position = c(0,
                                                                                                                                                                1), legend.justification = c(0, 1)) + ggplot2::theme(legend.title = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.direction = "horizontal") +
      ggplot2::theme(legend.background = ggplot2::element_blank()) +
      ggplot2::labs(y = label, x = "", size = latex2exp::TeX(legend)) +
      ggplot2::labs(shape = condition.lab) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10,
                                                                                                colour = "black", hjust = 0.5, angle = angle))
    #if a model is provided, add model fit to plot
    if(any(class(object) %in% c("robust.rma", "rma.mv", "rma", "rma.uni", "orchard"))){
      
      plot <- plot +
        # confidence interval
        ggplot2::geom_linerange(data = mod_table, ggplot2::aes(x = name,
                                                               ymin = lowerCL, ymax = upperCL), size = branch.size,
                                position = ggplot2::position_dodge2(width = 0.3)) +
        # prediction interval
        ggplot2::geom_pointrange(data = mod_table, ggplot2::aes(y = estimate,
                                                                x = name, ymin = lowerPR, ymax = upperPR, shape = as.factor(condition),
                                                                fill = color2), size = twig.size, position = ggplot2::position_dodge2(width = 0.3),
                                 fatten = trunk.size)
    }
  }
  
  # FLIP PLOT ---------------------------------------------------------------
  
  if (flip) {
    plot <- plot + ggplot2::coord_flip()
  }
  
  if (legend.pos == "bottom.right") {
    plot <- plot + ggplot2::theme(legend.position = c(1,
                                                      0), legend.justification = c(1, 0))
  }
  else if (legend.pos == "bottom.left") {
    plot <- plot + ggplot2::theme(legend.position = c(0,
                                                      0), legend.justification = c(0, 0))
  }
  else if (legend.pos == "top.right") {
    plot <- plot + ggplot2::theme(legend.position = c(1,
                                                      1), legend.justification = c(1, 1))
  }
  else if (legend.pos == "top.left") {
    plot <- plot + ggplot2::theme(legend.position = c(0,
                                                      1), legend.justification = c(0, 1))
  }
  else if (legend.pos == "top.out") {
    plot <- plot + ggplot2::theme(legend.position = "top")
  }
  else if (legend.pos == "bottom.out") {
    plot <- plot + ggplot2::theme(legend.position = "bottom")
  }
  else if (legend.pos == "none") {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }
  if (cb == TRUE) {
    plot <- plot + ggplot2::scale_fill_manual(values = cbpl) +
      ggplot2::scale_colour_manual(values = cbpl)
  }
  if (k == TRUE && g == FALSE && k.pos == "right") {
    plot <- plot + ggplot2::annotate("text", y = (max(data_trim$yi) +
                                                    (max(data_trim$yi) * 0.1)), x = (seq(1, group_no,
                                                                                         1) + 0.3), label = paste("italic(k)==", mod_table$K[1:group_no]),
                                     parse = TRUE, hjust = "right", size = 3.5)
  }
  else if (k == TRUE && g == FALSE && k.pos == "left") {
    plot <- plot + ggplot2::annotate("text", y = (min(data_trim$yi) +
                                                    (min(data_trim$yi) * 0.1)), x = (seq(1, group_no,
                                                                                         1) + 0.3), label = paste("italic(k)==", mod_table$K[1:group_no]),
                                     parse = TRUE, hjust = "left", size = 3.5)
  }
  else if (k == TRUE && g == TRUE && k.pos == "right") {
    plot <- plot + ggplot2::annotate("text", y = (max(data_trim$yi) +
                                                    (max(data_trim$yi) * 0.1)), x = (seq(1, group_no,
                                                                                         1) + 0.3), label = paste("italic(k)==", mod_table$K[1:group_no],
                                                                                                                  "~", "(", mod_table$g[1:group_no], ")"), parse = TRUE,
                                     hjust = "right", size = 3.5)
  }
  else if (k == TRUE && g == TRUE && k.pos == "left") {
    plot <- plot + ggplot2::annotate("text", y = (min(data_trim$yi) +
                                                    (min(data_trim$yi) * 0.1)), x = (seq(1, group_no,
                                                                                         1) + 0.3), label = paste("italic(k)==", mod_table$K[1:group_no],
                                                                                                                  "~", "(", mod_table$g[1:group_no], ")"), parse = TRUE,
                                     hjust = "left", size = 3.5)
  }
  else if (k == TRUE && g == FALSE && k.pos %in% c("right",
                                                   "left", "none") == FALSE) {
    plot <- plot + ggplot2::annotate("text", y = k.pos,
                                     x = (seq(1, group_no, 1) + 0.3), label = paste("italic(k)==",
                                                                                    mod_table$K[1:group_no]), parse = TRUE, size = 3.5)
  }
  else if (k == TRUE && g == TRUE && k.pos %in% c("right",
                                                  "left", "none") == FALSE) {
    plot <- plot + ggplot2::annotate("text", y = k.pos,
                                     x = (seq(1, group_no, 1) + 0.3), label = paste("italic(k)==",
                                                                                    mod_table$K[1:group_no], "~", "(", mod_table$g[1:group_no],
                                                                                    ")"), parse = TRUE, size = 3.5)
  }
  return(plot)
}

# BUBBLE PLOT -------------------------------------------------------------

#' @title bubble_plot
#' @description Using a \pkg{metafor} model object of class \code{rma} or \code{rma.mv}, or a results table of class \code{orchard}, the \code{bubble_plot} function creates a bubble plot from slope estimates. In cases when a model includes interaction terms, this function creates panels of bubble plots.
#' @param object Model object of class \code{rma}, \code{rma.mv}, or \code{orchard} table of model results
#' @param yi The name of the yi variable, to be plotted on the y-axis of the bubble plot. Only needed when providing a dataframe rather than a metafor model object.
#' @param vi The name of the vi variable, to be plotted on the y-axis of the bubble plot. Only needed when providing a dataframe rather than a metafor model object.
#' @param stdy The name of the study variable, for colouring points. Only needed when providing a dataframe rather than a metafor model object.
#' @param mod The name of a continuous moderator, to be plotted on the x-axis of the bubble plot.
#' @param group The grouping variable that one wishes to plot beside total effect sizes, k. This could be study, species, or any grouping variable one wishes to present sample sizes for. Not needed if an \code{orchard_plot} is provided with a \code{mod_results_MLJ} object of class \code{orchard}.
#' @param by Character vector indicating the name that predictions should be conditioned on for the levels of the moderator.
#' @param at List of levels one wishes to predict at for the corresponding variables in \code{by}. Used when one wants marginalised means. This argument can also be used to suppress levels of the moderator when argument \code{subset = TRUE}. Provide a list as follows: \code{list(mod = c("level1", "level2"))}.
#' @param weights How to marginalize categorical variables; used when one wants marginalised means. The default is \code{weights = "prop"}, which weights means for moderator levels based on their proportional representation in the data. For example, if \code{"sex"} is a moderator, and males have a larger sample size than females, then this will produce a weighted average, where males are weighted more towards the mean than females. This may not always be ideal when, for example, males and females are typically roughly equally prevalent in a population. In cases such as these, you can give the moderator levels equal weight using \code{weights = "equal"}.
#' @param xlab Moderator label.
#' @param ylab Effect size measure label.
#' @param k.pos The position of effect size number, k.
#' @param N The vector of sample size which an effect size is based on. Defaults to precision (the inverse of sampling standard error).
#' @param alpha The level of transparency for pieces of fruit (effect size).
#' @param cb If \code{TRUE}, it uses a colourblind-friendly palette of 20 colours (do not make this \code{TRUE}, when colour = \code{TRUE}).
#' @param k If \code{TRUE}, it displays k (number of effect sizes) on the plot.
#' @param g If \code{TRUE}, it displays g (number of grouping levels for each level of the moderator) on the plot.
#' @param est.lwd Size of the point estimate.
#' @param ci.lwd Size of the confidence interval.
#' @param pi.lwd Size of the prediction interval.
#' @param est.col Colour of the point estimate.
#' @param ci.col Colour of the confidence interval.
#' @param pi.col Colour of the prediction interval.
#' @param condition.nrow Number of rows to plot condition variable.
#' @param legend.pos Where to place the legend, or not to include a legend ("none").
#'
#' @return Orchard plot
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @examples
#' \dontrun{
#' data(lim)
#' lim[, "year"] <- as.numeric(lim$year)
#' lim$vi<- 1/(lim$N - 3)
#' model<-metafor::rma.mv(yi=yi, V=vi, mods= ~Environment*year,
#' random=list(~1|Article,~1|Datapoint), data=na.omit(lim))
#' test <- mod_results_MLJ(model, mod = "year", group = "Article", data = lim, weights = "prop", by = "Environment")
#' orchaRd::bubble_plot(test, mod = "year", data = lim, group = "Article",legend.pos = "top.left")
#' # Or just using model directly
#' orchaRd::bubble_plot(model, mod = "year", legend.pos = "top.left", data = lim, group = "Article", weights = "prop", by = "Environment")
#'
#' }
#' @export

# TODO - make poly works for bubble???
# TODO - write to https://github.com/rvlenth/emmeans/issues (missing combinations or interaction not allowed)

bubble_plot_MLJ <- function(object,
                            yi = NULL, vi = NULL, stdy = NULL, mod = NULL,
                            group = NULL, xlab = "Moderator", ylab = "Effect size", N = "none",
                            alpha = 0.5, cb = TRUE, k = TRUE, g = FALSE,
                            est.lwd = 1, ci.lwd = 0.5, pi.lwd = 0.5,
                            est.col = "black", ci.col = "black", pi.col = "black",
                            legend.pos = c("top.left", "top.right",
                                           "bottom.right", "bottom.left",
                                           "top.out", "bottom.out",
                                           "none"),
                            k.pos = c("top.right", "top.left",
                                      "bottom.right", "bottom.left",
                                      "none"),
                            colour = TRUE, fill = TRUE, #ADDED FOR COLOURING BY STUDY
                            condition.nrow = 2,
                            #condition.lab = "Condition",
                            weights = "prop", by = NULL, at = NULL)
{
  legend.pos <- match.arg(NULL, choices = legend.pos)
  k.pos <- match.arg(NULL, choices = k.pos)
  #facet <- match.arg(NULL, choices = facet)
  
  #Add warning message to make it clear that model parameters override those
  if(any(class(object) %in% c("robust.rma", "rma.mv", "rma", "rma.uni","orchard"))
     &!missing(yi)|missing(vi)|missing(stdy)|missing(by)){
    warning("N.B:'yi', 'vi', and 'by' arguments are disregarded and overriden by those in the model/model results object provided. Plotted values and their splitting into sub-plots is based on the values of these parameters in the model, not those given in the bubble_plot() function!")
  }
  
  if(missing(group)){
    stop("Please specify the 'group' argument by providing the name of the grouping variable. See ?bubble_plot")
  }
  
  if(is.numeric(by)){
    k = FALSE
    g = FALSE
  }
  
  #if object is a metafor model object, extract results and split them into two separate objects
  if(any(class(object) %in% c("robust.rma", "rma.mv", "rma", "rma.uni"))){
    
    if(mod != "1"){
      results <-  mod_results_MLJ(object, mod, group,
                                  by = by, at = at, weights = weights)
    } else {
      results <-  mod_results_MLJ(object, mod = "1", group,
                                  by = by, at = at, weights = weights)
    }
    
    #create object for model
    mod_table <- results$mod_table
    
    #create object for data underlying model
    data_trim <- results$data
    
  }
  
  if(any(class(object) %in% c("orchard"))) {
    results <- object
    
    #create object for model
    mod_table <- results$mod_table
    
    #create object for data underlying model
    data_trim <- results$data
  }
  
  #if the class of the object is 'escalc' and 'data.frame' or 'only 'data.frame'
  if(all(class(object) %in% c("escalc","data.frame"))|all(class(object) %in% c("data.frame"))) {
    
    if(is.null(yi)){
      stop("Must specify 'yi' when providing a dataframe as the object to be plotted. See ?bubble_plot")
    }
    if(is.null(vi)){
      stop("Must specify 'vi' when providing a dataframe as the object to be plotted. See ?bubble_plot")
    }
    if(is.null(stdy)){
      stop("Must specify 'stdy' when providing a dataframe as the object to be plotted. See ?bubble_plot")
    }
    if(is.null(mod)){
      stop("Must specify 'mod' when providing a dataframe as the object to be plotted. See ?bubble_plot")
    }
    
    #create mod_table object with just names (because there is no model)
    mod_table<-data.frame(name=levels(as.factor(object[[mod]])))
    
    #create object for data underlying model from the data directly provided
    data_trim <- data.frame(yi = object[,yi], vi = object[,vi], moderator=object[,mod], stdy=object[,stdy])
    data_trim <- data_trim[complete.cases(data_trim),]
    
    #if a conditioning variable is supplied, add to df
    if(!is.null(by)){
      #add the conditioning variable to 'model' table if present (workaround)
      mod_table <- expand.grid(name = mod_table$name, condition = levels(as.factor(object[,by])))
      #add conditioning variable to data_trim
      data_trim$condition<-object[,by]
    }
    
  }
  
  #scale the point size by 1 divided by square room of variance
  data_trim$scale <- (1/sqrt(data_trim[,"vi"]))
  legend <- "Precision (1/SE)"
  
  if(any(N != "none")){
    data_trim$scale <- data_trim$N
    legend <- paste0("Sample Size ($\\textit{N}$)") # we want to use italic
  }
  
  label <- xlab
  # if(transfm == "tanh"){
  #   cols <- sapply(mod_table, is.numeric)
  #   mod_table[,cols] <- Zr_to_r(mod_table[,cols])
  #   data_trim$yi <- Zr_to_r(data_trim$yi)
  #   label <- xlab
  # }else{
  #   label <- xlab
  # }
  
  if(is.null(data_trim$condition) == TRUE){
    
    # the number of effect sizes
    effect_num <- nrow(data_trim)
    
    # Add in total levels of a grouping variable (e.g., study ID) within each moderator level.
    group_num <- length(unique(data_trim$stdy))
    
    dat_text <- data.frame(K = effect_num, G = group_num)
    
  }else{
    
    # making sure factor names match
    data_trim$condition <- factor(data_trim$condition, levels = mod_table$condition, labels = mod_table$condition)
    
    effect_num <- as.vector(by(data_trim, data_trim[,"condition"], function(x) base::length(x[,"yi"])))
    
    # Add in total levels of a grouping variable (e.g., study ID) within each moderator level.
    #group_num <- c(2,4)
    group_num <- as.vector(by(data_trim, data_trim[,"condition"], function(x) base::length(base::unique(x[,"stdy"]))))
    
    dat_text <- data.frame(K = effect_num, G = group_num, condition = as.vector(base::levels(data_trim$condition)))
  }
  # the number of groups in a moderator & data points
  #group_no <- length(unique(mod_table[, "name"]))
  
  #data_no <- nrow(data)
  
  # # colour blind friendly colours with grey
  # cbpl <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  
  #ADDED FOR COLOURING BY STUDY
  if (colour == TRUE) {
    color <- as.factor(data_trim$stdy)
    color2 <- NULL
  }
  else {
    color <- NULL
  }
  if (fill == TRUE) {
    fill <- as.factor(data_trim$stdy)
  }
  else {
    fill <- NULL
  }
  
  # PLOT WITHOUT ANOTHER CONDITIONING VARIABLE IN ADDITION TO THE MODERATOR ---------------------------------
  
  #if there is not another conditioning variable...
  if(is.null(data_trim$condition) == TRUE){
    
    # basic plot with just points
    plot <-ggplot2::ggplot() +
      # putting bubbles
      ggplot2::geom_point(data = data_trim, ggplot2::aes(x = moderator, y = yi, size = scale, colour = color, fill = fill), shape = 21, alpha = alpha) +  #ADDED FOR COLOURING BY STUDY (COLOUR AND FILL OPTIONS INSIDE AES)
      #facet_grid(rows = vars(condition)) +
      ggplot2::labs(x = xlab, y = ylab, size = legend, parse = TRUE) +
      ggplot2::guides(fill = "none", colour = "none") +
      # themes
      ggplot2::theme_bw() +
      #theme(legend.position= c(1, 1), legend.justification = c(1, 1)) +
      ggplot2::theme(legend.direction="horizontal") +
      #theme(legend.background = element_rect(fill = "white", colour = "black")) +
      ggplot2::theme(legend.background = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, colour ="black", hjust = 0.5, angle = 90))
    
    #if a model is provided, add model fit to plot
    if(any(class(object) %in% c("robust.rma", "rma.mv", "rma", "rma.uni"))){
      plot <-plot+
        # prediction interval
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerPR), method =  "loess", formula = y~x, se = FALSE, lty =  "dotted", lwd = pi.lwd, colour = pi.col) +
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperPR), method =  "loess", formula = y~x, se = FALSE, lty = "dotted", lwd = pi.lwd, colour = pi.col) +
        # confidence interval
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerCL), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", lwd = ci.lwd, colour = ci.col) +
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperCL), method =  "loess", formula = y~x, se = FALSE, lty ="dashed", lwd = ci.lwd, colour = ci.col) +
        # main line
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = estimate), method =  "loess", formula = y~x, se = FALSE, lwd = est.lwd, colour = est.col)
      
    }
  }
  
  # PLOT WITH ANOTHER CONDITIONING VARIABLE IN ADDITION TO THE MODERATOR ---------------------------------
  
  #if there is a conditioning variable...
  if(is.character(data_trim$condition) == TRUE || is.factor(data_trim$condition) == TRUE){
    
    # basic plot with just points
    plot <-ggplot2::ggplot() +
      # putting bubbles
      ggplot2::geom_point(data = data_trim, ggplot2::aes(x = moderator, y = yi, size = scale, colour = color, fill = fill), shape = 21, alpha = alpha) + #ADDED FOR COLOURING BY STUDY (COLOUR AND FILL OPTIONS INSIDE AES)
      ggplot2::facet_wrap(ggplot2::vars(condition), nrow = condition.nrow) +
      ggplot2::labs(x = xlab, y = ylab, size = legend, parse = TRUE) +
      ggplot2::guides(fill = "none", colour = "none") +
      # themes
      ggplot2::theme_bw() +
      #theme(legend.position= c(1, 1), legend.justification = c(1, 1)) +
      ggplot2::theme(legend.direction="horizontal") +
      #theme(legend.background = element_rect(fill = "white", colour = "black")) +
      ggplot2::theme(legend.background = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, colour ="black", hjust = 0.5, angle = 90))
    
    #if a model is provided, add model fit to plot
    if(any(class(object) %in% c("orchard"))){
      plot <-plot+
        # prediction interval
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerPR), method =  "loess", formula = y~x, se = FALSE, lty =  "dotted", lwd = pi.lwd, colour = pi.col) +
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperPR), method =  "loess", formula = y~x,se = FALSE, lty = "dotted", lwd = pi.lwd, colour = pi.col) +
        # confidence interval
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerCL), method =  "loess", formula = y~x,se = FALSE,lty = "dashed", lwd = ci.lwd, colour = ci.col) +
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperCL), method =  "loess", formula = y~x,se = FALSE, lty ="dashed", lwd = ci.lwd, colour = ci.col) +
        # main line
        ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = estimate), method =  "loess", formula = y~x, se = FALSE, lwd = est.lwd, colour = est.col)
      # if(facet == "rows"){
      #   plot <- plot + facet_grid(rows = vars(condition))
      # } else{
      #   plot <- plot + facet_grid(cols = vars(condition))
      # }
    }
  }
  
  # else{
  #   plot <-ggplot2::ggplot() +
  #     # putting bubbles
  #     #geom_point(data = data, aes(x = moderator, y = yi, size = scale), shape = 21, alpha = alpha, fill = "grey90" ) +
  #     # prediction interval
  #     ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerPR), method =  "loess", formula = y~x, se = FALSE, lty =  "dotted", lwd = pi.lwd, colour = pi.col) +
  #     ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperPR), method =  "loess", formula = y~x,se = FALSE, lty = "dotted", lwd = pi.lwd, colour = pi.col) +
  #     # confidence interval
  #     ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerCL), method =  "loess", formula = y~x,se = FALSE,lty = "dashed", lwd = ci.lwd, colour = ci.col) +
  #     ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperCL), method =  "loess", formula = y~x,se = FALSE, lty ="dashed", lwd = ci.lwd, colour = ci.col) +
  #     # main line
  #     ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = estimate), method =  "loess", formula = y~x, se = FALSE, lwd = est.lwd, colour = est.col) +
  #     ggplot2::facet_wrap(ggplot2::vars(condition), nrow = condition.nrow) +
  #     ggplot2::labs(x = xlab, y = ylab, size = legend, parse = TRUE) +
  #     ggplot2::guides(fill = "none", colour = "none") +
  #     # themes
  #     ggplot2::theme_bw() # +
  #     #theme(legend.position= c(1, 1), legend.justification = c(1, 1)) +
  #     # theme(legend.direction="horizontal") +
  #     # #theme(legend.background = element_rect(fill = "white", colour = "black")) +
  #     # theme(legend.background = element_blank()) +
  #     # theme(axis.text.y = element_text(size = 10, colour ="black", hjust = 0.5, angle = 90))
  # }
  
  # adding legend
  if(legend.pos == "bottom.right"){
    plot <- plot + ggplot2::theme(legend.position= c(1, 0), legend.justification = c(1, 0))
  } else if ( legend.pos == "bottom.left") {
    plot <- plot + ggplot2::theme(legend.position= c(0, 0), legend.justification = c(0, 0))
  } else if ( legend.pos == "top.right") {
    plot <- plot + ggplot2::theme(legend.position= c(1, 1), legend.justification = c(1, 1))
  } else if (legend.pos == "top.left") {
    plot <- plot + ggplot2::theme(legend.position= c(0, 1), legend.justification = c(0, 1))
  } else if (legend.pos == "top.out") {
    plot <- plot + ggplot2::theme(legend.position="top")
  } else if (legend.pos == "bottom.out") {
    plot <- plot + ggplot2::theme(legend.position="bottom")
  } else if (legend.pos == "none") {
    plot <- plot + ggplot2::theme(legend.position="none")
  }
  
  # putting k and g in
  # c("top.right", "top.left", "bottom.right", "bottom.left","none")
  if(k == TRUE && g == FALSE && k.pos == "top.right"){
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = Inf, y = Inf),
                         label =  paste("italic(k)==", dat_text$K),
                         parse = TRUE,
                         hjust   = 2,
                         vjust   = 2.5
      )
    
  } else if(k == TRUE && g == FALSE && k.pos == "top.left") {
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = -Inf, y = Inf),
                         label =  paste("italic(k)==", dat_text$K),
                         parse = TRUE,
                         hjust   = -0.5,
                         vjust   = 2.5
      )
  } else if(k == TRUE && g == FALSE && k.pos == "bottom.right") {
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = Inf, y = -Inf),
                         label =  paste("italic(k)==", rev(dat_text$K)),
                         parse = TRUE,
                         hjust   = 2,
                         vjust   = -1.5
      )
  } else if (k == TRUE && g == FALSE && k.pos == "bottom.left"){
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = -Inf, y = -Inf),
                         label =  paste("italic(k)==", dat_text$K),
                         parse = TRUE,
                         hjust   = -0.5,
                         vjust   = -1.5
      )
    # below get g ----
    
  } else if (k == TRUE && g == TRUE && k.pos == "top.right"){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = Inf, y = Inf),
                         label =  paste("italic(k)==", dat_text$K,
                                        "~","(", dat_text$G, ")"),
                         parse = TRUE,
                         hjust   = 1.5,
                         vjust   = 2)
    
  } else if (k == TRUE && g == TRUE && k.pos == "top.left"){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = -Inf, y = Inf),
                         label =  paste("italic(k)==", dat_text$K,
                                        "~","(", dat_text$G, ")"),
                         parse = TRUE,
                         hjust   = -0.5,
                         vjust   = 2)
  } else if (k == TRUE && g == TRUE && k.pos == "bottom.right"){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = Inf, y = -Inf),
                         label =  paste("italic(k)==", dat_text$K,
                                        "~","(", dat_text$G, ")"),
                         parse = TRUE,
                         hjust   = 1.5,
                         vjust   = -0.5)
  } else if (k == TRUE && g == TRUE && k.pos == "bottom.left"){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = -Inf, y = -Inf),
                         label =  paste("italic(k)==", dat_text$K,
                                        "~","(", dat_text$G, ")"),
                         parse = TRUE,
                         hjust   = -0.5,
                         vjust   = -0.5)
  }
  
  # # putting colors in
  # if(cb == TRUE){
  #   plot <- plot +
  #     ggplot2::scale_fill_manual(values=cbpl) +
  #     ggplot2::scale_colour_manual(values=cbpl)
  # }
  
  return(plot)
}