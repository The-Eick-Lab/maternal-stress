
#######################################################
#######################################################
## Stress and oxidative stress in CIOB 
#######################################################
#######################################################

#######################################################
## Table 1 - demographics distributions
#######################################################

df.wide$mat_educ1<-
  factor(df.wide$mat_educ,
         levels=c(1,2,3,4),
         labels=c("less than high school education", "high education or scome college", "college degree", "graduate degree"))

df.wide$mat_race_eth.1<-
  factor(df.wide$mat_race_eth,
         levels=c(1,2,3,4,5),
         labels=c("White", "Black", "AAPI", "Latina", "Multiracial"))

df.wide$parity.1<-
  factor(df.wide$parity,
         levels=c(0,1),
         labels=c("0", "1+"))


df.wide$marital.1<-
  factor(df.wide$marital,
         levels=c(1,2,3),
         labels=c("Married", "Living Together", "Single"))


df.wide$econ_cat.1<-
  factor(df.wide$econ_cat,
         levels=c(0,1),
         labels=c("No", "Yes"))


df.wide$food_cat.1<-
  factor(df.wide$food_cat,
         levels=c(0,1),
         labels=c("No", "Yes"))

df.wide$job_highstrain_missing.1<-
  factor(df.wide$job_highstrain_missing,
         levels=c(0,1),
         labels=c("No", "Yes"))

df.wide$hood_cat_r.1<-
  factor(df.wide$hood_cat_r,
         levels=c(0,1),
         labels=c("No", "Yes"))

df.wide$caregiving_missing.1<-
  factor(df.wide$caregiving_missing,
         levels=c(0,1),
         labels=c("No", "Yes"))

df.wide$sle_cat.1<-
  factor(df.wide$sle_cat,
         levels=c(0,1),
         labels=c("No", "Yes"))

df.wide$unplanned_pregnancy.1<-
  factor(df.wide$unplanned_pregnancy,
         levels=c(0,1),
         labels=c("No", "Yes"))

df.wide$ladder_cat.1<-
  factor(df.wide$ladder_cat,
         levels=c(0,1),
         labels=c("No", "Yes"))


table1.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}


table1.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}



label(df.wide$mat_age)<-"Maternal age at delivery (years)"
label(df.wide$mat_educ1)<-"Maternal education"
label(df.wide$mat_race_eth.1)<-"Maternal race/ethnicity"
label(df.wide$ppbmi)<-"Pre-pregnancy Body Mass Index (kg/m2)"
label(df.wide$marital.1)<-"Marital Status"
label(df.wide$parity.1)<-"Parity"
label(df.wide$econ_cat.1)<-"Financial strain"
label(df.wide$food_cat.1)<-"Food insecurity"
label(df.wide$job_highstrain_missing.1)<-"Job strain"
label(df.wide$hood_cat_r.1)<-"Poor percieved neighborhood quality"
label(df.wide$caregiving_missing.1)<-"Caregiving"
label(df.wide$sle_cat.1)<-"Stressful life events"
label(df.wide$unplanned_pregnancy.1)<-"Unplanned pregnancy"
label(df.wide$ladder_cat.1)<-"Low percieved community status"
label(df.wide$final_ga_weeks)<-"Gestational age at delivery (weeks)"

table1(~ mat_age + as.numeric(ppbmi)+ mat_educ1 + mat_race_eth.1 + parity.1 +
         marital.1 + final_ga_weeks + econ_cat.1 + food_cat.1 + job_highstrain_missing.1 + hood_cat_r.1 +
         caregiving_missing.1 + sle_cat.1 + unplanned_pregnancy.1 + ladder_cat.1
       , overall="N (%) or Mean (SD)", 
       render.continuous=table1.cont, render.categorical=table1.cat, 
       data=df.wide)
# Note: not including smoking because there are only two people that report smoking 



#######################################################
## Table S1 - stressor and stress combination distributions  
# do this also for the N=173 complete cases for stressors
#######################################################
# Removing combinations of stressors that have less than 10 particpants 

label(df.wide$econ_food)<-"Financial strain & food insecurity"
label(df.wide$econ_job)<-"Financial strain & job strain"
label(df.wide$econ_hood)<-"Financial strain & poor percieved neighborhood quality"
label(df.wide$econ_care)<-"Financial strain & caregiving"
label(df.wide$econ_sle)<-"Financial strain & stressful life events"
label(df.wide$econ_unplan)<-"Financial strain & unplanned pregnancy"
label(df.wide$econ_lad)<-"Financial strain & low percieved community status"
label(df.wide$food_hood)<-"Food insecurity & poor percieved neighborhood quality"
label(df.wide$food_care)<-"Food insecurity & caregiving"
label(df.wide$food_sle)<-"Food insecurity & stressful life events"
label(df.wide$food_job)<-"Food insecurity & job strain"
label(df.wide$food_lad)<-"Food insecurity & low percieved community status"
label(df.wide$food_unplan)<-"Food insecurity & unplanned pregnancy"
label(df.wide$job_hood)<-"Job strain & poor percieved neighborhood quality"
label(df.wide$job_care)<-"Job strain and caregiving"
label(df.wide$job_sle)<-"Job strain and stressful life events"
label(df.wide$job_unplan)<-"Job strain & unplanned pregnancy"
label(df.wide$job_lad)<-"Job strain & low percieved community status"
label(df.wide$hood_care)<-"Poor percieved neighborhood quality & caregiving"
label(df.wide$hood_sle)<-"Poor percieved neighborhood quality & stressful life events"
label(df.wide$hood_unplan)<-"Poor percieved neighborhood quality & unplanned pregnancy"
label(df.wide$hood_lad)<-"Poor percieved neighborhood quality & low percieved community status"
label(df.wide$care_sle)<-"Caregiving & stressful life events"
label(df.wide$care_unplan)<-"Caregiving & unplanned pregnancy"
label(df.wide$care_lad)<-"Caregiving & low percieved community status"
label(df.wide$sle_unplan)<-"Stressful life events & unplanned pregnancy"
label(df.wide$sle_lad)<-"Stressful life events & low percieved community status"
label(df.wide$unplan_lad)<-"Unplanned pregnancy & low percieved community status" 




# larger population
table1(~ econ_cat.1 + food_cat.1 + job_highstrain_missing.1 + hood_cat_r.1 +
         caregiving_missing.1 + sle_cat.1 + unplanned_pregnancy.1 + ladder_cat.1 +
          econ_food  + 
          econ_job  + 
          econ_hood  + 
          econ_care  + 
          econ_sle  + 
          econ_unplan  +
          econ_lad  + 
          food_job  + 
          food_hood  + 
          food_care  + 
          food_sle  +
          food_unplan  + 
          food_lad  + 
          job_hood  + 
          job_care  +  
          job_sle  + 
          job_unplan  + 
          job_lad  +
          hood_care  +
          hood_sle  + 
          hood_unplan  + 
          hood_lad  + 
          care_sle  + 
          care_unplan  + 
          care_lad  +
          sle_unplan  + 
          sle_lad  + 
          unplan_lad 
       , overall="N (%) or Mean (SD)", 
       render.continuous=table1.cont, render.categorical=table1.cat, 
       data=df.wide)

## only among those with complete stress data
table1(~econ_cat.1 + food_cat.1 + job_highstrain_missing.1 + hood_cat_r.1 +
         caregiving_missing.1 + sle_cat.1 + unplanned_pregnancy.1 + ladder_cat.1 +
         econ_food  + 
         econ_job  + 
         econ_hood  + 
         econ_care  + 
         econ_sle  + 
         econ_unplan  +
         econ_lad  + 
         food_job  + 
         food_hood  + 
         food_care  + 
         food_sle  +
         food_unplan  + 
         food_lad  + 
         job_hood  + 
         job_care  +  
         job_sle  + 
         job_unplan  + 
         job_lad  +
         hood_care  +
         hood_sle  + 
         hood_unplan  + 
         hood_lad  + 
         care_sle  + 
         care_unplan  + 
         care_lad  +
         sle_unplan  + 
         sle_lad  + 
         unplan_lad  
       , overall="N (%) or Mean (SD)", 
       render.continuous=table1.cont, render.categorical=table1.cat, 
       data=df.wide [which(!is.na(df.wide$econ_cat) &
                             !is.na(df.wide$food_cat) & 
                             !is.na(df.wide$job_highstrain_missing) &
                             !is.na(df.wide$hood_cat_r) &
                             !is.na(df.wide$caregiving_missing) &
                             !is.na(df.wide$sle_cat) &
                             !is.na(df.wide$unplanned_pregnancy) &
                             !is.na(df.wide$ladder_cat) &
                             !is.na(df.wide$mat_age)),])



#######################################################
## Table S2 - ox stress distributions 
#######################################################

##### create list of ox stress names
chem.names <- c(
  "iso.sg",
  "isom.sg",
  "isom.new.sg",
  "pgf.sg",
  "iso.chem.sg",
  "iso.enz.sg")


# Initialize an empty result table
result_table <- tibble()

# Loop through chemical names
for (name in chem.names) {
  x <- df.wide[[name]]
  n <-NROW(na.omit(x))
  mean <- round(geometric.mean(x, na.rm = TRUE), 2)
  sd <- round(exp(sd(log(x), na.rm = TRUE)), 2)
  quantile_05 <- round(quantile(na.omit(x), 0.05), 2)
  quantile_25 <- round(quantile(na.omit(x), 0.25), 2)
  quantile_50 <- round(quantile(na.omit(x), 0.50), 2)
  quantile_75 <- round(quantile(na.omit(x), 0.75), 2)
  quantile_95 <- round(quantile(na.omit(x), 0.95), 2)
  
  result_data <- tibble(
    " " = name,
    "N" = n,
    "Geometric Mean" = mean,
    "Geometric Standard Deviation" = sd,
    "5%" = quantile_05,
    "25%" = quantile_25,
    "50%" = quantile_50,
    "75%" = quantile_75,
    "95%" = quantile_95
  )
  
  # Append the results to the table
  result_table <- bind_rows(result_table, result_data) 
}

### Print out distributions of chemical table 
result_table

###########################################################
## Regression models pre- processing
###########################################################

#### Create list of outcomes

ox.stress.names <- c(
  "iso.sg",
  "isom.sg",
  "isom.new.sg",
  "pgf.sg",
  "iso.chem.sg",
  "iso.enz.sg")

ox.stress.label <- c("8-iso-PGF2α", 
                     "2,3-dinor-5,6-dihydro-8-iso-PGF2α", 
                     "2,3-dinor-8-iso-PGF2α", 
                     "PGF2α", 
                     "8-iso-PGF2α - Chemical", 
                     "8-iso-PGF2α - Enzymatic")


#### Create list of indiviudal stress exposures
exposure_names <- c("econ_cat",
                    "food_cat",
                    "job_highstrain_missing",
                    "hood_cat_r",
                    "caregiving_missing",
                    "sle_cat",
                    "unplanned_pregnancy",
                    "ladder_cat")  

#### Create list of stress combinations for exposures - only keeping those with >=10 in a comb
exposure_names_comb<-c("econ_food", 
  "econ_job", 
  "econ_hood", 
  "econ_care", 
  "econ_sle", 
  "econ_unplan",
  "econ_lad", 
  "food_hood", 
  "food_care", 
  "food_sle",
  "food_unplan", 
  "job_sle", 
  "job_unplan", 
  "hood_care",
  "hood_sle", 
  "hood_unplan", 
  "hood_lad", 
  "care_sle", 
  "care_unplan", 
  "sle_unplan", 
  "sle_lad")


#### Create list of different covariates to include in different adjusted models 
covar.adj.wide<-c("as.numeric(mat_age) + as.factor(mat_educ) + as.numeric(ppbmi)")
covar.adj.no.educ<-c("as.numeric(mat_age) + as.numeric(ppbmi)")


####### Create function for linear regression - UNADJSUTED, SINGLE STRESSOR ####### 
lm.regression.1 <- function(data) {
  
  # Create empty dataframe for results
  results <- data.frame(Stress = character(0), 
                        Ox_Stress = character(0), 
                        N = numeric(0),
                        Beta = numeric(0), 
                        conf_low = numeric(0), 
                        conf_high = numeric(0))
  
  # Loop over ox stress variables
  for (ox.stress.name in ox.stress.names) {
    # Loop over stress exposure variables
    for (exposure_name in exposure_names) {
      # Create a formula for the linear mixed-effects model
      formula <- as.formula(paste("log(", ox.stress.name, ") ~ (", exposure_name, ") "))
      # Fit the linear mixed-effects model
      model <- lm(formula, data = data)
      # Get n, beta and its confidence interval
      nobs<-nobs(model)
      coef.beta <- round(coef(model)[2], 2)
      conf_intervals <- confint(model)
      conf_low <- round(conf_intervals[2, 1], 2)
      conf_high <- round(conf_intervals[2, 2], 2)
      
      result <- data.frame(
        Stress = exposure_name,
        Ox_Stress = ox.stress.name,
        N = nobs,
        Beta = coef.beta,
        conf_low = conf_low,
        conf_high = conf_high
      )
      results <- bind_rows(results, result)
    }
  }
  
  return(results)
}



####### Create function for linear regression - ADJUSTED, SINGLE STRESSOR ####### 
lm.regression.2 <- function(data, covar) {
  
  # Create empty dataframe for results
  results <- data.frame(Stress = character(0), 
                        Ox_Stress = character(0), 
                        N = numeric(0),
                        Beta = numeric(0), 
                        conf_low = numeric(0), 
                        conf_high = numeric(0))
  
  # Loop over ox stress variables
  for (ox.stress.name in ox.stress.names)  {
    # Loop over stress exposure variables
    for (exposure_name in exposure_names) {
      # Create a formula for the linear mixed-effects model
      formula <- as.formula(paste("log(", ox.stress.name, ") ~ (", exposure_name, ") +  ",covar," "))
      # Fit the linear mixed-effects model
      model <- lm(formula, data = data)
      # Get n, beta and its confidence interval
      nobs<-nobs(model)
      coef.beta <- round(coef(model)[2], 2)
      conf_intervals <- confint(model)
      conf_low <- round(conf_intervals[2, 1], 2)
      conf_high <- round(conf_intervals[2, 2], 2)
      
      result <- data.frame(
        Stress = exposure_name,
        Ox_Stress = ox.stress.name,
        N = nobs,
        Beta = coef.beta,
        conf_low = conf_low,
        conf_high = conf_high
      )
      results <- bind_rows(results, result)
    }
  }
  
  return(results)
}


####### Create function for linear regression - COMBINATIONS OF STRESSOR, UNADJUSTED ####### 

lm.regression.3 <- function(data, covar) {
  
  # Create empty dataframe for results
  results <- data.frame(Stress = character(0), 
                        Ox_Stress = character(0), 
                        N = numeric(0),
                        Beta = numeric(0), 
                        conf_low = numeric(0), 
                        conf_high = numeric(0))
  
  # Loop over ox stress variables
  for (ox.stress.name in ox.stress.names)  {
    # Loop over stress exposure variables
    for (exposure_name in exposure_names_comb) {
      # Create a formula for the linear mixed-effects model
      formula <- as.formula(paste("log(", ox.stress.name, ") ~ (", exposure_name, ") "))
      # Fit the linear mixed-effects model
      model <- lm(formula, data = data)
      # Get n, beta and its confidence interval
      nobs<-nobs(model)
      coef.beta <- round(coef(model)[2], 2)
      conf_intervals <- confint(model)
      conf_low <- round(conf_intervals[2, 1], 2)
      conf_high <- round(conf_intervals[2, 2], 2)
      
      result <- data.frame(
        Stress = exposure_name,
        Ox_Stress = ox.stress.name,
        N = nobs,
        Beta = coef.beta,
        conf_low = conf_low,
        conf_high = conf_high
      )
      results <- bind_rows(results, result)
    }
  }
  
  return(results)
}

####### Create function for linear regression - COMBINATIONS OF STRESSOR, ADJSUTED ####### 

lm.regression.4 <- function(data, covar) {
  
  # Create empty dataframe for results
  results <- data.frame(Stress = character(0), 
                        Ox_Stress = character(0), 
                        N = numeric(0),
                        Beta = numeric(0), 
                        conf_low = numeric(0), 
                        conf_high = numeric(0))
  
  # Loop over ox stress variables
  for (ox.stress.name in ox.stress.names)  {
    # Loop over stress exposure variables
    for (exposure_name in exposure_names_comb) {
      # Create a formula for the linear mixed-effects model
      formula <- as.formula(paste("log(", ox.stress.name, ") ~ (", exposure_name, ") + ",covar," "))
      # Fit the linear mixed-effects model
      model <- lm(formula, data = data)
      # Get n, beta and its confidence interval
      nobs<-nobs(model)
      coef.beta <- round(coef(model)[2], 2)
      conf_intervals <- confint(model)
      conf_low <- round(conf_intervals[2, 1], 2)
      conf_high <- round(conf_intervals[2, 2], 2)
      
      result <- data.frame(
        Stress = exposure_name,
        Ox_Stress = ox.stress.name,
        N = nobs,
        Beta = coef.beta,
        conf_low = conf_low,
        conf_high = conf_high
      )
      results <- bind_rows(results, result)
    }
  }
  
  return(results)
}




#######################################################
## Table S3 - linear regression models - individual stressors  - unadjusted
#######################################################
results_1 <- lm.regression.1(data = df.wide)
results_1 <- results_1 %>%
  mutate(model = "Unadjusted")


#######################################################
## Table S3 - linear regression models - individual stressors  -  covariate adjusted
#######################################################

results_2 <- lm.regression.2(data = df.wide, covar = covar.adj.no.educ)
results_2 <- results_2 %>%
  mutate(model = "Covariate Adjusted")

  
  #######################################################
## Table S3 - linear regression models - individual stressors  -  covariate  & stressors adjusted
#######################################################

########## 8 - iso

model.iso<-lm(log(iso.sg) ~ econ_cat +food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat +
                  as.numeric(mat_age) + as.numeric(ppbmi)  ,
                data = df.wide)

n<-nobs(model.iso)
econ_beta <-round(coef(model.iso)[2], 2)
econ_ci_ll<-round(confint(model.iso),2) [2,1]
econ_ci_ul<-round(confint(model.iso),2) [2,2]

econ<-cbind(n, econ_beta, econ_ci_ll, econ_ci_ul)

food_beta <- round(coef(model.iso)[3], 2)
food_ci_ll<-round(confint(model.iso),2) [3,1]
food_ci_ul<-round(confint(model.iso),2) [3,2]

food<-cbind(n, food_beta, food_ci_ll, food_ci_ul)


job_beta <- round(coef(model.iso)[4], 2)
job_ci_ll<-round(confint(model.iso),2) [4,1]
job_ci_ul<-round(confint(model.iso),2) [4,2]

job<-cbind(n, job_beta, job_ci_ll, job_ci_ul)


hood_beta <- round(coef(model.iso)[5], 2)
hood_ci_ll<-round(confint(model.iso),2) [5,1]
hood_ci_ul<-round(confint(model.iso),2) [5,2]

hood<-cbind(n, hood_beta, hood_ci_ll, hood_ci_ul)


care_beta <- round(coef(model.iso)[6], 2)
care_ci_ll<-round(confint(model.iso),2) [6,1]
care_ci_ul<-round(confint(model.iso),2) [6,2]

care<-cbind(n, care_beta, care_ci_ll, care_ci_ul)


sle_beta <- round(coef(model.iso)[7], 2)
sle_ci_ll<-round(confint(model.iso),2) [7,1]
sle_ci_ul<-round(confint(model.iso),2) [7,2]

sle<-cbind(n, sle_beta, sle_ci_ll, sle_ci_ul)


unplan_beta <- round(coef(model.iso)[8], 2)
unplan_ci_ll<-round(confint(model.iso),2) [8,1]
unplan_ci_ul<-round(confint(model.iso),2) [8,2]

unplan<-cbind(n, unplan_beta, unplan_ci_ll, unplan_ci_ul)


ladder_beta <- round(coef(model.iso)[9], 2)
ladder_ci_ll<-round(confint(model.iso),2) [9,1]
ladder_ci_ul<-round(confint(model.iso),2) [9,2]

ladder<-cbind(n, ladder_beta, ladder_ci_ll, ladder_ci_ul)


results_comb_iso<-rbind(econ, food, job, hood, care, sle, unplan, ladder)
results_comb_iso <- as.data.frame(results_comb_iso) %>%
  mutate(Ox_Stress = "iso.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_iso)) %>%
  rename("Beta" = econ_beta, 
         "conf_low" = econ_ci_ll, 
         "conf_high" = econ_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)


########## 8 - iso, metabolite


model.isom<-lm(log(isom.sg) ~ econ_cat +food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat +
                as.numeric(mat_age) + as.numeric(ppbmi),
              data = df.wide)

n<-nobs(model.isom)
econ_beta <-round(coef(model.isom)[2], 2)
econ_ci_ll<-round(confint(model.isom),2) [2,1]
econ_ci_ul<-round(confint(model.isom),2) [2,2]

econ<-cbind(n, econ_beta, econ_ci_ll, econ_ci_ul)

food_beta <- round(coef(model.isom)[3], 2)
food_ci_ll<-round(confint(model.isom),2) [3,1]
food_ci_ul<-round(confint(model.isom),2) [3,2]

food<-cbind(n, food_beta, food_ci_ll, food_ci_ul)


job_beta <- round(coef(model.isom)[4], 2)
job_ci_ll<-round(confint(model.isom),2) [4,1]
job_ci_ul<-round(confint(model.isom),2) [4,2]

job<-cbind(n, job_beta, job_ci_ll, job_ci_ul)


hood_beta <- round(coef(model.isom)[5], 2)
hood_ci_ll<-round(confint(model.isom),2) [5,1]
hood_ci_ul<-round(confint(model.isom),2) [5,2]

hood<-cbind(n, hood_beta, hood_ci_ll, hood_ci_ul)


care_beta <- round(coef(model.isom)[6], 2)
care_ci_ll<-round(confint(model.isom),2) [6,1]
care_ci_ul<-round(confint(model.isom),2) [6,2]

care<-cbind(n, care_beta, care_ci_ll, care_ci_ul)


sle_beta <- round(coef(model.isom)[7], 2)
sle_ci_ll<-round(confint(model.isom),2) [7,1]
sle_ci_ul<-round(confint(model.isom),2) [7,2]

sle<-cbind(n, sle_beta, sle_ci_ll, sle_ci_ul)


unplan_beta <- round(coef(model.isom)[8], 2)
unplan_ci_ll<-round(confint(model.isom),2) [8,1]
unplan_ci_ul<-round(confint(model.isom),2) [8,2]

unplan<-cbind(n, unplan_beta, unplan_ci_ll, unplan_ci_ul)


ladder_beta <- round(coef(model.isom)[9], 2)
ladder_ci_ll<-round(confint(model.isom),2) [9,1]
ladder_ci_ul<-round(confint(model.isom),2) [9,2]

ladder<-cbind(n, ladder_beta, ladder_ci_ll, ladder_ci_ul)


results_comb_isom<-rbind(econ, food, job, hood, care, sle, unplan, ladder)
results_comb_isom <- as.data.frame(results_comb_isom) %>%
  mutate(Ox_Stress = "isom.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_isom)) %>%
  rename("Beta" = econ_beta, 
         "conf_low" = econ_ci_ll, 
         "conf_high" = econ_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)

########## 8 - iso, metabolite new


model.isom.new<-lm(log(isom.new.sg) ~ econ_cat +food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat +
                as.numeric(mat_age) + as.numeric(ppbmi), 
              data = df.wide)

n<-nobs(model.isom.new)
econ_beta <-round(coef(model.isom.new)[2], 2)
econ_ci_ll<-round(confint(model.isom.new),2) [2,1]
econ_ci_ul<-round(confint(model.isom.new),2) [2,2]

econ<-cbind(n, econ_beta, econ_ci_ll, econ_ci_ul)

food_beta <- round(coef(model.isom.new)[3], 2)
food_ci_ll<-round(confint(model.isom.new),2) [3,1]
food_ci_ul<-round(confint(model.isom.new),2) [3,2]

food<-cbind(n, food_beta, food_ci_ll, food_ci_ul)


job_beta <- round(coef(model.isom.new)[4], 2)
job_ci_ll<-round(confint(model.isom.new),2) [4,1]
job_ci_ul<-round(confint(model.isom.new),2) [4,2]

job<-cbind(n, job_beta, job_ci_ll, job_ci_ul)


hood_beta <- round(coef(model.isom.new)[5], 2)
hood_ci_ll<-round(confint(model.isom.new),2) [5,1]
hood_ci_ul<-round(confint(model.isom.new),2) [5,2]

hood<-cbind(n, hood_beta, hood_ci_ll, hood_ci_ul)


care_beta <- round(coef(model.isom.new)[6], 2)
care_ci_ll<-round(confint(model.isom.new),2) [6,1]
care_ci_ul<-round(confint(model.isom.new),2) [6,2]

care<-cbind(n, care_beta, care_ci_ll, care_ci_ul)


sle_beta <- round(coef(model.isom.new)[7], 2)
sle_ci_ll<-round(confint(model.isom.new),2) [7,1]
sle_ci_ul<-round(confint(model.isom.new),2) [7,2]

sle<-cbind(n, sle_beta, sle_ci_ll, sle_ci_ul)


unplan_beta <- round(coef(model.isom.new)[8], 2)
unplan_ci_ll<-round(confint(model.isom.new),2) [8,1]
unplan_ci_ul<-round(confint(model.isom.new),2) [8,2]

unplan<-cbind(n, unplan_beta, unplan_ci_ll, unplan_ci_ul)


ladder_beta <- round(coef(model.isom.new)[9], 2)
ladder_ci_ll<-round(confint(model.isom.new),2) [9,1]
ladder_ci_ul<-round(confint(model.isom.new),2) [9,2]

ladder<-cbind(n, ladder_beta, ladder_ci_ll, ladder_ci_ul)


results_comb_isom.new<-rbind(econ, food, job, hood, care, sle, unplan, ladder)
results_comb_isom.new <- as.data.frame(results_comb_isom.new) %>%
  mutate(Ox_Stress = "isom.new.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_isom.new)) %>%
  rename("Beta" = econ_beta, 
         "conf_low" = econ_ci_ll, 
         "conf_high" = econ_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)


########## PGF


model.pgf<-lm(log(pgf.sg) ~ econ_cat +food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat +
                as.numeric(mat_age) + as.numeric(ppbmi), 
              data = df.wide)

n<-nobs(model.pgf)
econ_beta <-round(coef(model.pgf)[2], 2)
econ_ci_ll<-round(confint(model.pgf),2) [2,1]
econ_ci_ul<-round(confint(model.pgf),2) [2,2]

econ<-cbind(n, econ_beta, econ_ci_ll, econ_ci_ul)

food_beta <- round(coef(model.pgf)[3], 2)
food_ci_ll<-round(confint(model.pgf),2) [3,1]
food_ci_ul<-round(confint(model.pgf),2) [3,2]

food<-cbind(n, food_beta, food_ci_ll, food_ci_ul)


job_beta <- round(coef(model.pgf)[4], 2)
job_ci_ll<-round(confint(model.pgf),2) [4,1]
job_ci_ul<-round(confint(model.pgf),2) [4,2]

job<-cbind(n, job_beta, job_ci_ll, job_ci_ul)


hood_beta <- round(coef(model.pgf)[5], 2)
hood_ci_ll<-round(confint(model.pgf),2) [5,1]
hood_ci_ul<-round(confint(model.pgf),2) [5,2]

hood<-cbind(n, hood_beta, hood_ci_ll, hood_ci_ul)


care_beta <- round(coef(model.pgf)[6], 2)
care_ci_ll<-round(confint(model.pgf),2) [6,1]
care_ci_ul<-round(confint(model.pgf),2) [6,2]

care<-cbind(n, care_beta, care_ci_ll, care_ci_ul)


sle_beta <- round(coef(model.pgf)[7], 2)
sle_ci_ll<-round(confint(model.pgf),2) [7,1]
sle_ci_ul<-round(confint(model.pgf),2) [7,2]

sle<-cbind(n, sle_beta, sle_ci_ll, sle_ci_ul)


unplan_beta <- round(coef(model.pgf)[8], 2)
unplan_ci_ll<-round(confint(model.pgf),2) [8,1]
unplan_ci_ul<-round(confint(model.pgf),2) [8,2]

unplan<-cbind(n, unplan_beta, unplan_ci_ll, unplan_ci_ul)


ladder_beta <- round(coef(model.pgf)[9], 2)
ladder_ci_ll<-round(confint(model.pgf),2) [9,1]
ladder_ci_ul<-round(confint(model.pgf),2) [9,2]

ladder<-cbind(n, ladder_beta, ladder_ci_ll, ladder_ci_ul)


results_comb_pgf<-rbind(econ, food, job, hood, care, sle, unplan, ladder)
results_comb_pgf <- as.data.frame(results_comb_pgf) %>%
  mutate(Ox_Stress = "pgf.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_pgf)) %>%
  rename("Beta" = econ_beta, 
         "conf_low" = econ_ci_ll, 
         "conf_high" = econ_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)


########## 8 - iso, chemical fraction


model.iso.chem<-lm(log(iso.chem.sg) ~ econ_cat +food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat +
                as.numeric(mat_age) + as.numeric(ppbmi),
              data = df.wide)

n<-nobs(model.iso.chem)
econ_beta <-round(coef(model.iso.chem)[2], 2)
econ_ci_ll<-round(confint(model.iso.chem),2) [2,1]
econ_ci_ul<-round(confint(model.iso.chem),2) [2,2]

econ<-cbind(n, econ_beta, econ_ci_ll, econ_ci_ul)

food_beta <- round(coef(model.iso.chem)[3], 2)
food_ci_ll<-round(confint(model.iso.chem),2) [3,1]
food_ci_ul<-round(confint(model.iso.chem),2) [3,2]

food<-cbind(n, food_beta, food_ci_ll, food_ci_ul)


job_beta <- round(coef(model.iso.chem)[4], 2)
job_ci_ll<-round(confint(model.iso.chem),2) [4,1]
job_ci_ul<-round(confint(model.iso.chem),2) [4,2]

job<-cbind(n, job_beta, job_ci_ll, job_ci_ul)


hood_beta <- round(coef(model.iso.chem)[5], 2)
hood_ci_ll<-round(confint(model.iso.chem),2) [5,1]
hood_ci_ul<-round(confint(model.iso.chem),2) [5,2]

hood<-cbind(n, hood_beta, hood_ci_ll, hood_ci_ul)


care_beta <- round(coef(model.iso.chem)[6], 2)
care_ci_ll<-round(confint(model.iso.chem),2) [6,1]
care_ci_ul<-round(confint(model.iso.chem),2) [6,2]

care<-cbind(n, care_beta, care_ci_ll, care_ci_ul)


sle_beta <- round(coef(model.iso.chem)[7], 2)
sle_ci_ll<-round(confint(model.iso.chem),2) [7,1]
sle_ci_ul<-round(confint(model.iso.chem),2) [7,2]

sle<-cbind(n, sle_beta, sle_ci_ll, sle_ci_ul)


unplan_beta <- round(coef(model.iso.chem)[8], 2)
unplan_ci_ll<-round(confint(model.iso.chem),2) [8,1]
unplan_ci_ul<-round(confint(model.iso.chem),2) [8,2]

unplan<-cbind(n, unplan_beta, unplan_ci_ll, unplan_ci_ul)


ladder_beta <- round(coef(model.iso.chem)[9], 2)
ladder_ci_ll<-round(confint(model.iso.chem),2) [9,1]
ladder_ci_ul<-round(confint(model.iso.chem),2) [9,2]

ladder<-cbind(n, ladder_beta, ladder_ci_ll, ladder_ci_ul)


results_comb_iso.chem<-rbind(econ, food, job, hood, care, sle, unplan, ladder)
results_comb_iso.chem <- as.data.frame(results_comb_iso.chem) %>%
  mutate(Ox_Stress = "iso.chem.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_iso.chem)) %>%
  rename("Beta" = econ_beta, 
         "conf_low" = econ_ci_ll, 
         "conf_high" = econ_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)



########## 8 - iso, enzymatic fraction


model.iso.enz<-lm(log(iso.enz.sg) ~ econ_cat +food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat +
                as.numeric(mat_age) + as.numeric(ppbmi),
              data = df.wide)

n<-nobs(model.iso.enz)
econ_beta <-round(coef(model.iso.enz)[2], 2)
econ_ci_ll<-round(confint(model.iso.enz),2) [2,1]
econ_ci_ul<-round(confint(model.iso.enz),2) [2,2]

econ<-cbind(n, econ_beta, econ_ci_ll, econ_ci_ul)

food_beta <- round(coef(model.iso.enz)[3], 2)
food_ci_ll<-round(confint(model.iso.enz),2) [3,1]
food_ci_ul<-round(confint(model.iso.enz),2) [3,2]

food<-cbind(n, food_beta, food_ci_ll, food_ci_ul)


job_beta <- round(coef(model.iso.enz)[4], 2)
job_ci_ll<-round(confint(model.iso.enz),2) [4,1]
job_ci_ul<-round(confint(model.iso.enz),2) [4,2]

job<-cbind(n, job_beta, job_ci_ll, job_ci_ul)


hood_beta <- round(coef(model.iso.enz)[5], 2)
hood_ci_ll<-round(confint(model.iso.enz),2) [5,1]
hood_ci_ul<-round(confint(model.iso.enz),2) [5,2]

hood<-cbind(n, hood_beta, hood_ci_ll, hood_ci_ul)


care_beta <- round(coef(model.iso.enz)[6], 2)
care_ci_ll<-round(confint(model.iso.enz),2) [6,1]
care_ci_ul<-round(confint(model.iso.enz),2) [6,2]

care<-cbind(n, care_beta, care_ci_ll, care_ci_ul)


sle_beta <- round(coef(model.iso.enz)[7], 2)
sle_ci_ll<-round(confint(model.iso.enz),2) [7,1]
sle_ci_ul<-round(confint(model.iso.enz),2) [7,2]

sle<-cbind(n, sle_beta, sle_ci_ll, sle_ci_ul)


unplan_beta <- round(coef(model.iso.enz)[8], 2)
unplan_ci_ll<-round(confint(model.iso.enz),2) [8,1]
unplan_ci_ul<-round(confint(model.iso.enz),2) [8,2]

unplan<-cbind(n, unplan_beta, unplan_ci_ll, unplan_ci_ul)


ladder_beta <- round(coef(model.iso.enz)[9], 2)
ladder_ci_ll<-round(confint(model.iso.enz),2) [9,1]
ladder_ci_ul<-round(confint(model.iso.enz),2) [9,2]

ladder<-cbind(n, ladder_beta, ladder_ci_ll, ladder_ci_ul)


results_comb_iso.enz<-rbind(econ, food, job, hood, care, sle, unplan, ladder)
results_comb_iso.enz  <-as.data.frame(results_comb_iso.enz) %>%
  mutate(Ox_Stress = "iso.enz.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_iso.enz)) %>%
  rename("Beta" = econ_beta, 
         "conf_low" = econ_ci_ll, 
         "conf_high" = econ_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)


table_s3 <- rbind(results_1, results_2, results_comb_iso, results_comb_iso.chem, results_comb_iso.enz, results_comb_isom, results_comb_isom.new, results_comb_pgf)
table_s3 %>%
  mutate("Beta (CI)" = paste0(Beta, " (", conf_low, ", ", conf_high, ")"))  %>%
  dplyr::select(Stress, Ox_Stress, model, N, `Beta (CI)`) %>%
  pivot_wider(names_from = Ox_Stress, values_from = `Beta (CI)`) %>%
  as_grouped_data(groups = "model") %>%
  flextable() %>%
  autofit()

#######################################################
## Table S4 - linear models, sensitivity analysis adding education as a covariate
#######################################################

## Single stressors
results_5 <- lm.regression.2(data = df.wide, covar = covar.adj.wide)
results_5 <- results_5 %>%
  mutate(model = "Single Stressors")

## Stressor combinations

results_6 <- lm.regression.4(data = df.wide, covar = covar.adj.wide)
results_6 <- results_6 %>%
  mutate(model = "Stressor combos")

table_s4 <- rbind(results_5, results_6)
table_s4 %>%
  mutate("Beta (CI)" = paste0(Beta, " (", conf_low, ", ", conf_high, ")"))  %>%
  dplyr::select(Stress, Ox_Stress, model, N, `Beta (CI)`) %>%
  as_grouped_data(groups = "model") %>%
  flextable() %>%
  autofit()
 
#######################################################
## Table S5 - linear regression models - stressors combinations - unadjusted
#######################################################

results_6 <- lm.regression.3(data = df.wide)
results_6 <- results_6
  mutate(model = "Unadjusted")


#######################################################
## Table S5- linear regression models - stressors combinations -  covariate adjusted
#######################################################

results_4 <- lm.regression.4(data = df.wide, covar = covar.adj.no.educ)
results_4 <- results_4 %>%
  mutate(model = "Covariate Adjusted")


#######################################################
## Table S5 - linear regression models - stressors combinations -  covariate + stressor adjusted  - see above
#######################################################

################ 8 - iso
econ_food<-lm(log(iso.sg)~as.factor(econ_food)   + as.numeric(mat_age)  + as.numeric(ppbmi) +
                  job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
                data = df.wide)
n<-nobs(econ_food)
econ_food_beta <-round(coef(econ_food)[2], 2)
econ_food_ci_ll<-round(confint(econ_food),2) [2,1]
econ_food_ci_ul<-round(confint(econ_food),2) [2,2]
econ_food<-cbind(n, econ_food_beta, econ_food_ci_ll, econ_food_ci_ul)


econ_job<-lm(log(iso.sg)~as.factor(econ_job)   + as.numeric(mat_age)  + as.numeric(ppbmi) +
                 food_cat + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
               data = df.wide)

n<-nobs(econ_job)
econ_job_beta <-round(coef(econ_job)[2], 2)
econ_job_ci_ll<-round(confint(econ_job),2) [2,1]
econ_job_ci_ul<-round(confint(econ_job),2) [2,2]
econ_job<-cbind(n, econ_job_beta, econ_job_ci_ll, econ_job_ci_ul)


econ_hood<-lm(log(iso.sg)~as.factor(econ_hood)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + job_highstrain_missing + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
                data = df.wide)

n<-nobs(econ_hood)
econ_hood_beta <-round(coef(econ_hood)[2], 2)
econ_hood_ci_ll<-round(confint(econ_hood),2) [2,1]
econ_hood_ci_ul<-round(confint(econ_hood),2) [2,2]
econ_hood<-cbind(n, econ_hood_beta, econ_hood_ci_ll, econ_hood_ci_ul)

econ_care<-lm(log(iso.sg)~as.factor(econ_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  food_cat + job_highstrain_missing + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
                data = df.wide)

n<-nobs(econ_care)
econ_care_beta <-round(coef(econ_care)[2], 2)
econ_care_ci_ll<-round(confint(econ_care),2) [2,1]
econ_care_ci_ul<-round(confint(econ_care),2) [2,2]
econ_care<-cbind(n, econ_care_beta, econ_care_ci_ll, econ_care_ci_ul)

econ_sle<-lm(log(iso.sg)~as.factor(econ_sle)   + as.numeric(mat_age)  + as.numeric(ppbmi) +
                 food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
               data = df.wide)

n<-nobs(econ_sle)
econ_sle_beta <-round(coef(econ_sle)[2], 2)
econ_sle_ci_ll<-round(confint(econ_sle),2) [2,1]
econ_sle_ci_ul<-round(confint(econ_sle),2) [2,2]
econ_sle<-cbind(n, econ_sle_beta, econ_sle_ci_ll, econ_sle_ci_ul)

econ_unplan<-lm(log(iso.sg)~as.factor(econ_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                    food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                  data = df.wide)

n<-nobs(econ_unplan)
econ_unplan_beta <-round(coef(econ_unplan)[2], 2)
econ_unplan_ci_ll<-round(confint(econ_unplan),2) [2,1]
econ_unplan_ci_ul<-round(confint(econ_unplan),2) [2,2]
econ_unplan<-cbind(n, econ_unplan_beta, econ_unplan_ci_ll, econ_unplan_ci_ul)

econ_lad<-lm(log(iso.sg)~as.factor(econ_lad)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                 food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy,
               data = df.wide)

n<-nobs(econ_lad)
econ_lad_beta <-round(coef(econ_lad)[2], 2)
econ_lad_ci_ll<-round(confint(econ_lad),2) [2,1]
econ_lad_ci_ul<-round(confint(econ_lad),2) [2,2]
econ_lad<-cbind(n, econ_lad_beta, econ_lad_ci_ll, econ_lad_ci_ul)


food_hood<-lm(log(iso.sg)~as.factor(food_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  job_highstrain_missing + econ_cat + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
                data = df.wide)

n<-nobs(food_hood)
food_hood_beta <-round(coef(food_hood)[2], 2)
food_hood_ci_ll<-round(confint(food_hood),2) [2,1]
food_hood_ci_ul<-round(confint(food_hood),2) [2,2]
food_hood<-cbind(n, food_hood_beta, food_hood_ci_ll, food_hood_ci_ul)


food_care<-lm(log(iso.sg)~as.factor(food_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  job_highstrain_missing + econ_cat + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
                data = df.wide)

n<-nobs(food_care)
food_care_beta <-round(coef(food_care)[2], 2)
food_care_ci_ll<-round(confint(food_care),2) [2,1]
food_care_ci_ul<-round(confint(food_care),2) [2,2]
food_care<-cbind(n, food_care_beta, food_care_ci_ll, food_care_ci_ul)

food_sle<-lm(log(iso.sg)~as.factor(food_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                 job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
               data = df.wide)

n<-nobs(food_sle)
food_sle_beta <-round(coef(food_sle)[2], 2)
food_sle_ci_ll<-round(confint(food_sle),2) [2,1]
food_sle_ci_ul<-round(confint(food_sle),2) [2,2]
food_sle<-cbind(n, food_sle_beta, food_sle_ci_ll, food_sle_ci_ul)

food_unplan<-lm(log(iso.sg)~as.factor(food_unplan)   + as.numeric(mat_age)  + as.numeric(ppbmi) +
                    job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                  data = df.wide)

n<-nobs(food_unplan)
food_unplan_beta <-round(coef(food_unplan)[2], 2)
food_unplan_ci_ll<-round(confint(food_unplan),2) [2,1]
food_unplan_ci_ul<-round(confint(food_unplan),2) [2,2]
food_unplan<-cbind(n, food_unplan_beta, food_unplan_ci_ll, food_unplan_ci_ul)


job_hood<-lm(log(iso.sg)~as.factor(job_hood)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                 food_cat + econ_cat  + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
               data = df.wide)

n<-nobs(job_hood)
job_hood_beta <-round(coef(job_hood)[2], 2)
job_hood_ci_ll<-round(confint(job_hood),2) [2,1]
job_hood_ci_ul<-round(confint(job_hood),2) [2,2]
job_hood<-cbind(n, job_hood_beta, job_hood_ci_ll, job_hood_ci_ul)


job_sle<-lm(log(iso.sg)~as.factor(job_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + econ_cat  + caregiving_missing + hood_cat_r + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(job_sle)
job_sle_beta <-round(coef(job_sle)[2], 2)
job_sle_ci_ll<-round(confint(job_sle),2) [2,1]
job_sle_ci_ul<-round(confint(job_sle),2) [2,2]
job_sle<-cbind(n, job_sle_beta, job_sle_ci_ll, job_sle_ci_ul)

job_unplan<-lm(log(iso.sg)~as.factor(job_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                   food_cat + econ_cat  + caregiving_missing + hood_cat_r + sle_cat + ladder_cat,
                 data = df.wide)

n<-nobs(job_unplan)
job_unplan_beta <-round(coef(job_unplan)[2], 2)
job_unplan_ci_ll<-round(confint(job_unplan),2) [2,1]
job_unplan_ci_ul<-round(confint(job_unplan),2) [2,2]
job_unplan<-cbind(n, job_unplan_beta, job_unplan_ci_ll, job_unplan_ci_ul)


hood_care<-lm(log(iso.sg)~as.factor(hood_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy  + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(hood_care)
hood_care_beta <-round(coef(hood_care)[2], 2)
hood_care_ci_ll<-round(confint(hood_care),2) [2,1]
hood_care_ci_ul<-round(confint(hood_care),2) [2,2]
hood_care<-cbind(n, hood_care_beta, hood_care_ci_ll, hood_care_ci_ul)

hood_sle<-lm(log(iso.sg)~as.factor(hood_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                 food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy + caregiving_missing + ladder_cat,
               data = df.wide)

n<-nobs(hood_sle)
hood_sle_beta <-round(coef(hood_sle)[2], 2)
hood_sle_ci_ll<-round(confint(hood_sle),2) [2,1]
hood_sle_ci_ul<-round(confint(hood_sle),2) [2,2]
hood_sle<-cbind(n, hood_sle_beta, hood_sle_ci_ll, hood_sle_ci_ul)


hood_unplan<-lm(log(iso.sg)~as.factor(hood_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                    food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + ladder_cat,
                  data = df.wide)

n<-nobs(hood_unplan)
hood_unplan_beta <-round(coef(hood_unplan)[2], 2)
hood_unplan_ci_ll<-round(confint(hood_unplan),2) [2,1]
hood_unplan_ci_ul<-round(confint(hood_unplan),2) [2,2]
hood_unplan<-cbind(n, hood_unplan_beta, hood_unplan_ci_ll, hood_unplan_ci_ul)

hood_lad<-lm(log(iso.sg)~as.factor(hood_lad)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                 food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + unplanned_pregnancy,
               data = df.wide)

n<-nobs(hood_lad)
hood_lad_beta <-round(coef(hood_lad)[2], 2)
hood_lad_ci_ll<-round(confint(hood_lad),2) [2,1]
hood_lad_ci_ul<-round(confint(hood_lad),2) [2,2]
hood_lad<-cbind(n, hood_lad_beta, hood_lad_ci_ll, hood_lad_ci_ul)

care_sle <-lm(log(iso.sg)~as.factor(care_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + unplanned_pregnancy + ladder_cat,
                data = df.wide)

n<-nobs(care_sle)
care_sle_beta <-round(coef(care_sle)[2], 2)
care_sle_ci_ll<-round(confint(care_sle),2) [2,1]
care_sle_ci_ul<-round(confint(care_sle),2) [2,2]
care_sle<-cbind(n, care_sle_beta, care_sle_ci_ll, care_sle_ci_ul)

care_unplan <-lm(log(iso.sg)~as.factor(care_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                     food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + sle_cat + ladder_cat,
                   data = df.wide)

n<-nobs(care_unplan)
care_unplan_beta <-round(coef(care_unplan)[2], 2)
care_unplan_ci_ll<-round(confint(care_unplan),2) [2,1]
care_unplan_ci_ul<-round(confint(care_unplan),2) [2,2]
care_unplan<-cbind(n, care_unplan_beta, care_unplan_ci_ll, care_unplan_ci_ul)

sle_unplan <-lm(log(iso.sg)~as.factor(sle_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                    food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + ladder_cat,
                  data = df.wide)

n<-nobs(sle_unplan)
sle_unplan_beta <-round(coef(sle_unplan)[2], 2)
sle_unplan_ci_ll<-round(confint(sle_unplan),2) [2,1]
sle_unplan_ci_ul<-round(confint(sle_unplan),2) [2,2]
sle_unplan<-cbind(n, sle_unplan_beta, sle_unplan_ci_ll, sle_unplan_ci_ul)

sle_lad <-lm(log(iso.sg)~as.factor(sle_lad)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                 food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + unplanned_pregnancy,
               data = df.wide)

n<-nobs(sle_lad)
sle_lad_beta <-round(coef(sle_lad)[2], 2)
sle_lad_ci_ll<-round(confint(sle_lad),2) [2,1]
sle_lad_ci_ul<-round(confint(sle_lad),2) [2,2]
sle_lad<-cbind(n, sle_lad_beta, sle_lad_ci_ll, sle_lad_ci_ul)

results_comb_iso<-rbind(econ_food,
                        econ_job,
                        econ_hood,
                        econ_care,
                        econ_sle,
                        econ_unplan,
                        econ_lad,
                        food_hood,
                        food_care,
                        food_sle,
                        food_unplan,
                        job_hood,
                        job_sle,
                        job_unplan,
                        hood_care,
                        hood_sle,
                        hood_unplan,
                        hood_lad,
                        care_sle,
                        care_unplan,
                        sle_unplan,
                        sle_lad)
results_comb_iso <- as.data.frame(results_comb_iso) %>%
  mutate(Ox_Stress = "iso.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_iso)) %>%
  rename("Beta" = econ_food_beta, 
         "conf_low" = econ_food_ci_ll, 
         "conf_high" = econ_food_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)

################ 8 - iso, metabolite
econ_food<-lm(log(isom.sg)~as.factor(econ_food)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)
n<-nobs(econ_food)
econ_food_beta <-round(coef(econ_food)[2], 2)
econ_food_ci_ll<-round(confint(econ_food),2) [2,1]
econ_food_ci_ul<-round(confint(econ_food),2) [2,2]
econ_food<-cbind(n, econ_food_beta, econ_food_ci_ll, econ_food_ci_ul)


econ_job<-lm(log(isom.sg)~as.factor(econ_job)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_job)
econ_job_beta <-round(coef(econ_job)[2], 2)
econ_job_ci_ll<-round(confint(econ_job),2) [2,1]
econ_job_ci_ul<-round(confint(econ_job),2) [2,2]
econ_job<-cbind(n, econ_job_beta, econ_job_ci_ll, econ_job_ci_ul)


econ_hood<-lm(log(isom.sg)~as.factor(econ_hood)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_hood)
econ_hood_beta <-round(coef(econ_hood)[2], 2)
econ_hood_ci_ll<-round(confint(econ_hood),2) [2,1]
econ_hood_ci_ul<-round(confint(econ_hood),2) [2,2]
econ_hood<-cbind(n, econ_hood_beta, econ_hood_ci_ll, econ_hood_ci_ul)

econ_care<-lm(log(isom.sg)~as.factor(econ_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_care)
econ_care_beta <-round(coef(econ_care)[2], 2)
econ_care_ci_ll<-round(confint(econ_care),2) [2,1]
econ_care_ci_ul<-round(confint(econ_care),2) [2,2]
econ_care<-cbind(n, econ_care_beta, econ_care_ci_ll, econ_care_ci_ul)

econ_sle<-lm(log(isom.sg)~as.factor(econ_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_sle)
econ_sle_beta <-round(coef(econ_sle)[2], 2)
econ_sle_ci_ll<-round(confint(econ_sle),2) [2,1]
econ_sle_ci_ul<-round(confint(econ_sle),2) [2,2]
econ_sle<-cbind(n, econ_sle_beta, econ_sle_ci_ll, econ_sle_ci_ul)

econ_unplan<-lm(log(isom.sg)~as.factor(econ_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(econ_unplan)
econ_unplan_beta <-round(coef(econ_unplan)[2], 2)
econ_unplan_ci_ll<-round(confint(econ_unplan),2) [2,1]
econ_unplan_ci_ul<-round(confint(econ_unplan),2) [2,2]
econ_unplan<-cbind(n, econ_unplan_beta, econ_unplan_ci_ll, econ_unplan_ci_ul)

econ_lad<-lm(log(isom.sg)~as.factor(econ_lad)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy,
             data = df.wide)

n<-nobs(econ_lad)
econ_lad_beta <-round(coef(econ_lad)[2], 2)
econ_lad_ci_ll<-round(confint(econ_lad),2) [2,1]
econ_lad_ci_ul<-round(confint(econ_lad),2) [2,2]
econ_lad<-cbind(n, econ_lad_beta, econ_lad_ci_ll, econ_lad_ci_ul)


food_hood<-lm(log(isom.sg)~as.factor(food_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_hood)
food_hood_beta <-round(coef(food_hood)[2], 2)
food_hood_ci_ll<-round(confint(food_hood),2) [2,1]
food_hood_ci_ul<-round(confint(food_hood),2) [2,2]
food_hood<-cbind(n, food_hood_beta, food_hood_ci_ll, food_hood_ci_ul)


food_care<-lm(log(isom.sg)~as.factor(food_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_care)
food_care_beta <-round(coef(food_care)[2], 2)
food_care_ci_ll<-round(confint(food_care),2) [2,1]
food_care_ci_ul<-round(confint(food_care),2) [2,2]
food_care<-cbind(n, food_care_beta, food_care_ci_ll, food_care_ci_ul)

food_sle<-lm(log(isom.sg)~as.factor(food_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(food_sle)
food_sle_beta <-round(coef(food_sle)[2], 2)
food_sle_ci_ll<-round(confint(food_sle),2) [2,1]
food_sle_ci_ul<-round(confint(food_sle),2) [2,2]
food_sle<-cbind(n, food_sle_beta, food_sle_ci_ll, food_sle_ci_ul)

food_unplan<-lm(log(isom.sg)~as.factor(food_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(food_unplan)
food_unplan_beta <-round(coef(food_unplan)[2], 2)
food_unplan_ci_ll<-round(confint(food_unplan),2) [2,1]
food_unplan_ci_ul<-round(confint(food_unplan),2) [2,2]
food_unplan<-cbind(n, food_unplan_beta, food_unplan_ci_ll, food_unplan_ci_ul)


job_hood<-lm(log(isom.sg)~as.factor(job_hood)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + econ_cat  + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(job_hood)
job_hood_beta <-round(coef(job_hood)[2], 2)
job_hood_ci_ll<-round(confint(job_hood),2) [2,1]
job_hood_ci_ul<-round(confint(job_hood),2) [2,2]
job_hood<-cbind(n, job_hood_beta, job_hood_ci_ll, job_hood_ci_ul)


job_sle<-lm(log(isom.sg)~as.factor(job_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
              food_cat + econ_cat  + caregiving_missing + hood_cat_r + unplanned_pregnancy + ladder_cat,
            data = df.wide)

n<-nobs(job_sle)
job_sle_beta <-round(coef(job_sle)[2], 2)
job_sle_ci_ll<-round(confint(job_sle),2) [2,1]
job_sle_ci_ul<-round(confint(job_sle),2) [2,2]
job_sle<-cbind(n, job_sle_beta, job_sle_ci_ll, job_sle_ci_ul)

job_unplan<-lm(log(isom.sg)~as.factor(job_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                 food_cat + econ_cat  + caregiving_missing + hood_cat_r + sle_cat + ladder_cat,
               data = df.wide)

n<-nobs(job_unplan)
job_unplan_beta <-round(coef(job_unplan)[2], 2)
job_unplan_ci_ll<-round(confint(job_unplan),2) [2,1]
job_unplan_ci_ul<-round(confint(job_unplan),2) [2,2]
job_unplan<-cbind(n, job_unplan_beta, job_unplan_ci_ll, job_unplan_ci_ul)


hood_care<-lm(log(isom.sg)~as.factor(hood_care)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy  + sle_cat + ladder_cat,
              data = df.wide)

n<-nobs(hood_care)
hood_care_beta <-round(coef(hood_care)[2], 2)
hood_care_ci_ll<-round(confint(hood_care),2) [2,1]
hood_care_ci_ul<-round(confint(hood_care),2) [2,2]
hood_care<-cbind(n, hood_care_beta, hood_care_ci_ll, hood_care_ci_ul)

hood_sle<-lm(log(isom.sg)~as.factor(hood_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy + caregiving_missing + ladder_cat,
             data = df.wide)

n<-nobs(hood_sle)
hood_sle_beta <-round(coef(hood_sle)[2], 2)
hood_sle_ci_ll<-round(confint(hood_sle),2) [2,1]
hood_sle_ci_ul<-round(confint(hood_sle),2) [2,2]
hood_sle<-cbind(n, hood_sle_beta, hood_sle_ci_ll, hood_sle_ci_ul)


hood_unplan<-lm(log(isom.sg)~as.factor(hood_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(hood_unplan)
hood_unplan_beta <-round(coef(hood_unplan)[2], 2)
hood_unplan_ci_ll<-round(confint(hood_unplan),2) [2,1]
hood_unplan_ci_ul<-round(confint(hood_unplan),2) [2,2]
hood_unplan<-cbind(n, hood_unplan_beta, hood_unplan_ci_ll, hood_unplan_ci_ul)

hood_lad<-lm(log(isom.sg)~as.factor(hood_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(hood_lad)
hood_lad_beta <-round(coef(hood_lad)[2], 2)
hood_lad_ci_ll<-round(confint(hood_lad),2) [2,1]
hood_lad_ci_ul<-round(confint(hood_lad),2) [2,2]
hood_lad<-cbind(n, hood_lad_beta, hood_lad_ci_ll, hood_lad_ci_ul)

care_sle <-lm(log(isom.sg)~as.factor(care_sle)   + as.numeric(mat_age) +as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(care_sle)
care_sle_beta <-round(coef(care_sle)[2], 2)
care_sle_ci_ll<-round(confint(care_sle),2) [2,1]
care_sle_ci_ul<-round(confint(care_sle),2) [2,2]
care_sle<-cbind(n, care_sle_beta, care_sle_ci_ll, care_sle_ci_ul)

care_unplan <-lm(log(isom.sg)~as.factor(care_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                   food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + sle_cat + ladder_cat,
                 data = df.wide)

n<-nobs(care_unplan)
care_unplan_beta <-round(coef(care_unplan)[2], 2)
care_unplan_ci_ll<-round(confint(care_unplan),2) [2,1]
care_unplan_ci_ul<-round(confint(care_unplan),2) [2,2]
care_unplan<-cbind(n, care_unplan_beta, care_unplan_ci_ll, care_unplan_ci_ul)

sle_unplan <-lm(log(isom.sg)~as.factor(sle_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(sle_unplan)
sle_unplan_beta <-round(coef(sle_unplan)[2], 2)
sle_unplan_ci_ll<-round(confint(sle_unplan),2) [2,1]
sle_unplan_ci_ul<-round(confint(sle_unplan),2) [2,2]
sle_unplan<-cbind(n, sle_unplan_beta, sle_unplan_ci_ll, sle_unplan_ci_ul)

sle_lad <-lm(log(isom.sg)~as.factor(sle_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(sle_lad)
sle_lad_beta <-round(coef(sle_lad)[2], 2)
sle_lad_ci_ll<-round(confint(sle_lad),2) [2,1]
sle_lad_ci_ul<-round(confint(sle_lad),2) [2,2]
sle_lad<-cbind(n, sle_lad_beta, sle_lad_ci_ll, sle_lad_ci_ul)

results_comb_isom<-rbind(econ_food,
                        econ_job,
                        econ_hood,
                        econ_care,
                        econ_sle,
                        econ_unplan,
                        econ_lad,
                        food_hood,
                        food_care,
                        food_sle,
                        food_unplan,
                        job_hood,
                        job_sle,
                        job_unplan,
                        hood_care,
                        hood_sle,
                        hood_unplan,
                        hood_lad,
                        care_sle,
                        care_unplan,
                        sle_unplan,
                        sle_lad)
results_comb_isom <- as.data.frame(results_comb_isom) %>%
  mutate(Ox_Stress = "isom.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_isom)) %>%
  rename("Beta" = econ_food_beta, 
         "conf_low" = econ_food_ci_ll, 
         "conf_high" = econ_food_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)

################ 8 - iso, metabolite, new
econ_food<-lm(log(isom.new.sg)~as.factor(econ_food)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)
n<-nobs(econ_food)
econ_food_beta <-round(coef(econ_food)[2], 2)
econ_food_ci_ll<-round(confint(econ_food),2) [2,1]
econ_food_ci_ul<-round(confint(econ_food),2) [2,2]
econ_food<-cbind(n, econ_food_beta, econ_food_ci_ll, econ_food_ci_ul)


econ_job<-lm(log(isom.new.sg)~as.factor(econ_job)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_job)
econ_job_beta <-round(coef(econ_job)[2], 2)
econ_job_ci_ll<-round(confint(econ_job),2) [2,1]
econ_job_ci_ul<-round(confint(econ_job),2) [2,2]
econ_job<-cbind(n, econ_job_beta, econ_job_ci_ll, econ_job_ci_ul)


econ_hood<-lm(log(isom.new.sg)~as.factor(econ_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_hood)
econ_hood_beta <-round(coef(econ_hood)[2], 2)
econ_hood_ci_ll<-round(confint(econ_hood),2) [2,1]
econ_hood_ci_ul<-round(confint(econ_hood),2) [2,2]
econ_hood<-cbind(n, econ_hood_beta, econ_hood_ci_ll, econ_hood_ci_ul)

econ_care<-lm(log(isom.new.sg)~as.factor(econ_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_care)
econ_care_beta <-round(coef(econ_care)[2], 2)
econ_care_ci_ll<-round(confint(econ_care),2) [2,1]
econ_care_ci_ul<-round(confint(econ_care),2) [2,2]
econ_care<-cbind(n, econ_care_beta, econ_care_ci_ll, econ_care_ci_ul)

econ_sle<-lm(log(isom.new.sg)~as.factor(econ_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_sle)
econ_sle_beta <-round(coef(econ_sle)[2], 2)
econ_sle_ci_ll<-round(confint(econ_sle),2) [2,1]
econ_sle_ci_ul<-round(confint(econ_sle),2) [2,2]
econ_sle<-cbind(n, econ_sle_beta, econ_sle_ci_ll, econ_sle_ci_ul)

econ_unplan<-lm(log(isom.new.sg)~as.factor(econ_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(econ_unplan)
econ_unplan_beta <-round(coef(econ_unplan)[2], 2)
econ_unplan_ci_ll<-round(confint(econ_unplan),2) [2,1]
econ_unplan_ci_ul<-round(confint(econ_unplan),2) [2,2]
econ_unplan<-cbind(n, econ_unplan_beta, econ_unplan_ci_ll, econ_unplan_ci_ul)

econ_lad<-lm(log(isom.new.sg)~as.factor(econ_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy,
             data = df.wide)

n<-nobs(econ_lad)
econ_lad_beta <-round(coef(econ_lad)[2], 2)
econ_lad_ci_ll<-round(confint(econ_lad),2) [2,1]
econ_lad_ci_ul<-round(confint(econ_lad),2) [2,2]
econ_lad<-cbind(n, econ_lad_beta, econ_lad_ci_ll, econ_lad_ci_ul)


food_hood<-lm(log(isom.new.sg)~as.factor(food_hood)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_hood)
food_hood_beta <-round(coef(food_hood)[2], 2)
food_hood_ci_ll<-round(confint(food_hood),2) [2,1]
food_hood_ci_ul<-round(confint(food_hood),2) [2,2]
food_hood<-cbind(n, food_hood_beta, food_hood_ci_ll, food_hood_ci_ul)


food_care<-lm(log(isom.new.sg)~as.factor(food_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_care)
food_care_beta <-round(coef(food_care)[2], 2)
food_care_ci_ll<-round(confint(food_care),2) [2,1]
food_care_ci_ul<-round(confint(food_care),2) [2,2]
food_care<-cbind(n, food_care_beta, food_care_ci_ll, food_care_ci_ul)

food_sle<-lm(log(isom.new.sg)~as.factor(food_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(food_sle)
food_sle_beta <-round(coef(food_sle)[2], 2)
food_sle_ci_ll<-round(confint(food_sle),2) [2,1]
food_sle_ci_ul<-round(confint(food_sle),2) [2,2]
food_sle<-cbind(n, food_sle_beta, food_sle_ci_ll, food_sle_ci_ul)

food_unplan<-lm(log(isom.new.sg)~as.factor(food_unplan)   + as.numeric(mat_age)   + as.numeric(ppbmi) +
                  job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(food_unplan)
food_unplan_beta <-round(coef(food_unplan)[2], 2)
food_unplan_ci_ll<-round(confint(food_unplan),2) [2,1]
food_unplan_ci_ul<-round(confint(food_unplan),2) [2,2]
food_unplan<-cbind(n, food_unplan_beta, food_unplan_ci_ll, food_unplan_ci_ul)


job_hood<-lm(log(isom.new.sg)~as.factor(job_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat  + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(job_hood)
job_hood_beta <-round(coef(job_hood)[2], 2)
job_hood_ci_ll<-round(confint(job_hood),2) [2,1]
job_hood_ci_ul<-round(confint(job_hood),2) [2,2]
job_hood<-cbind(n, job_hood_beta, job_hood_ci_ll, job_hood_ci_ul)


job_sle<-lm(log(isom.new.sg)~as.factor(job_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
              food_cat + econ_cat  + caregiving_missing + hood_cat_r + unplanned_pregnancy + ladder_cat,
            data = df.wide)

n<-nobs(job_sle)
job_sle_beta <-round(coef(job_sle)[2], 2)
job_sle_ci_ll<-round(confint(job_sle),2) [2,1]
job_sle_ci_ul<-round(confint(job_sle),2) [2,2]
job_sle<-cbind(n, job_sle_beta, job_sle_ci_ll, job_sle_ci_ul)

job_unplan<-lm(log(isom.new.sg)~as.factor(job_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                 food_cat + econ_cat  + caregiving_missing + hood_cat_r + sle_cat + ladder_cat,
               data = df.wide)

n<-nobs(job_unplan)
job_unplan_beta <-round(coef(job_unplan)[2], 2)
job_unplan_ci_ll<-round(confint(job_unplan),2) [2,1]
job_unplan_ci_ul<-round(confint(job_unplan),2) [2,2]
job_unplan<-cbind(n, job_unplan_beta, job_unplan_ci_ll, job_unplan_ci_ul)


hood_care<-lm(log(isom.new.sg)~as.factor(hood_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy  + sle_cat + ladder_cat,
              data = df.wide)

n<-nobs(hood_care)
hood_care_beta <-round(coef(hood_care)[2], 2)
hood_care_ci_ll<-round(confint(hood_care),2) [2,1]
hood_care_ci_ul<-round(confint(hood_care),2) [2,2]
hood_care<-cbind(n, hood_care_beta, hood_care_ci_ll, hood_care_ci_ul)

hood_sle<-lm(log(isom.new.sg)~as.factor(hood_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy + caregiving_missing + ladder_cat,
             data = df.wide)

n<-nobs(hood_sle)
hood_sle_beta <-round(coef(hood_sle)[2], 2)
hood_sle_ci_ll<-round(confint(hood_sle),2) [2,1]
hood_sle_ci_ul<-round(confint(hood_sle),2) [2,2]
hood_sle<-cbind(n, hood_sle_beta, hood_sle_ci_ll, hood_sle_ci_ul)


hood_unplan<-lm(log(isom.new.sg)~as.factor(hood_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(hood_unplan)
hood_unplan_beta <-round(coef(hood_unplan)[2], 2)
hood_unplan_ci_ll<-round(confint(hood_unplan),2) [2,1]
hood_unplan_ci_ul<-round(confint(hood_unplan),2) [2,2]
hood_unplan<-cbind(n, hood_unplan_beta, hood_unplan_ci_ll, hood_unplan_ci_ul)

hood_lad<-lm(log(isom.new.sg)~as.factor(hood_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(hood_lad)
hood_lad_beta <-round(coef(hood_lad)[2], 2)
hood_lad_ci_ll<-round(confint(hood_lad),2) [2,1]
hood_lad_ci_ul<-round(confint(hood_lad),2) [2,2]
hood_lad<-cbind(n, hood_lad_beta, hood_lad_ci_ll, hood_lad_ci_ul)

care_sle <-lm(log(isom.new.sg)~as.factor(care_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(care_sle)
care_sle_beta <-round(coef(care_sle)[2], 2)
care_sle_ci_ll<-round(confint(care_sle),2) [2,1]
care_sle_ci_ul<-round(confint(care_sle),2) [2,2]
care_sle<-cbind(n, care_sle_beta, care_sle_ci_ll, care_sle_ci_ul)

care_unplan <-lm(log(isom.new.sg)~as.factor(care_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                   food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + sle_cat + ladder_cat,
                 data = df.wide)

n<-nobs(care_unplan)
care_unplan_beta <-round(coef(care_unplan)[2], 2)
care_unplan_ci_ll<-round(confint(care_unplan),2) [2,1]
care_unplan_ci_ul<-round(confint(care_unplan),2) [2,2]
care_unplan<-cbind(n, care_unplan_beta, care_unplan_ci_ll, care_unplan_ci_ul)

sle_unplan <-lm(log(isom.new.sg)~as.factor(sle_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(sle_unplan)
sle_unplan_beta <-round(coef(sle_unplan)[2], 2)
sle_unplan_ci_ll<-round(confint(sle_unplan),2) [2,1]
sle_unplan_ci_ul<-round(confint(sle_unplan),2) [2,2]
sle_unplan<-cbind(n, sle_unplan_beta, sle_unplan_ci_ll, sle_unplan_ci_ul)

sle_lad <-lm(log(isom.new.sg)~as.factor(sle_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(sle_lad)
sle_lad_beta <-round(coef(sle_lad)[2], 2)
sle_lad_ci_ll<-round(confint(sle_lad),2) [2,1]
sle_lad_ci_ul<-round(confint(sle_lad),2) [2,2]
sle_lad<-cbind(n, sle_lad_beta, sle_lad_ci_ll, sle_lad_ci_ul)

results_comb_isom.new<-rbind(econ_food,
                        econ_job,
                        econ_hood,
                        econ_care,
                        econ_sle,
                        econ_unplan,
                        econ_lad,
                        food_hood,
                        food_care,
                        food_sle,
                        food_unplan,
                        job_hood,
                        job_sle,
                        job_unplan,
                        hood_care,
                        hood_sle,
                        hood_unplan,
                        hood_lad,
                        care_sle,
                        care_unplan,
                        sle_unplan,
                        sle_lad)
results_comb_isom.new <- as.data.frame(results_comb_isom.new) %>%
  mutate(Ox_Stress = "isom.new.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_isom.new)) %>%
  rename("Beta" = econ_food_beta, 
         "conf_low" = econ_food_ci_ll, 
         "conf_high" = econ_food_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)

################ PGF
econ_food<-lm(log(pgf.sg)~as.factor(econ_food)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)
n<-nobs(econ_food)
econ_food_beta <-round(coef(econ_food)[2], 2)
econ_food_ci_ll<-round(confint(econ_food),2) [2,1]
econ_food_ci_ul<-round(confint(econ_food),2) [2,2]
econ_food<-cbind(n, econ_food_beta, econ_food_ci_ll, econ_food_ci_ul)


econ_job<-lm(log(pgf.sg)~as.factor(econ_job)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_job)
econ_job_beta <-round(coef(econ_job)[2], 2)
econ_job_ci_ll<-round(confint(econ_job),2) [2,1]
econ_job_ci_ul<-round(confint(econ_job),2) [2,2]
econ_job<-cbind(n, econ_job_beta, econ_job_ci_ll, econ_job_ci_ul)


econ_hood<-lm(log(pgf.sg)~as.factor(econ_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_hood)
econ_hood_beta <-round(coef(econ_hood)[2], 2)
econ_hood_ci_ll<-round(confint(econ_hood),2) [2,1]
econ_hood_ci_ul<-round(confint(econ_hood),2) [2,2]
econ_hood<-cbind(n, econ_hood_beta, econ_hood_ci_ll, econ_hood_ci_ul)

econ_care<-lm(log(pgf.sg)~as.factor(econ_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_care)
econ_care_beta <-round(coef(econ_care)[2], 2)
econ_care_ci_ll<-round(confint(econ_care),2) [2,1]
econ_care_ci_ul<-round(confint(econ_care),2) [2,2]
econ_care<-cbind(n, econ_care_beta, econ_care_ci_ll, econ_care_ci_ul)

econ_sle<-lm(log(pgf.sg)~as.factor(econ_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_sle)
econ_sle_beta <-round(coef(econ_sle)[2], 2)
econ_sle_ci_ll<-round(confint(econ_sle),2) [2,1]
econ_sle_ci_ul<-round(confint(econ_sle),2) [2,2]
econ_sle<-cbind(n, econ_sle_beta, econ_sle_ci_ll, econ_sle_ci_ul)

econ_unplan<-lm(log(pgf.sg)~as.factor(econ_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(econ_unplan)
econ_unplan_beta <-round(coef(econ_unplan)[2], 2)
econ_unplan_ci_ll<-round(confint(econ_unplan),2) [2,1]
econ_unplan_ci_ul<-round(confint(econ_unplan),2) [2,2]
econ_unplan<-cbind(n, econ_unplan_beta, econ_unplan_ci_ll, econ_unplan_ci_ul)

econ_lad<-lm(log(pgf.sg)~as.factor(econ_lad)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy,
             data = df.wide)

n<-nobs(econ_lad)
econ_lad_beta <-round(coef(econ_lad)[2], 2)
econ_lad_ci_ll<-round(confint(econ_lad),2) [2,1]
econ_lad_ci_ul<-round(confint(econ_lad),2) [2,2]
econ_lad<-cbind(n, econ_lad_beta, econ_lad_ci_ll, econ_lad_ci_ul)


food_hood<-lm(log(pgf.sg)~as.factor(food_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_hood)
food_hood_beta <-round(coef(food_hood)[2], 2)
food_hood_ci_ll<-round(confint(food_hood),2) [2,1]
food_hood_ci_ul<-round(confint(food_hood),2) [2,2]
food_hood<-cbind(n, food_hood_beta, food_hood_ci_ll, food_hood_ci_ul)


food_care<-lm(log(pgf.sg)~as.factor(food_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_care)
food_care_beta <-round(coef(food_care)[2], 2)
food_care_ci_ll<-round(confint(food_care),2) [2,1]
food_care_ci_ul<-round(confint(food_care),2) [2,2]
food_care<-cbind(n, food_care_beta, food_care_ci_ll, food_care_ci_ul)

food_sle<-lm(log(pgf.sg)~as.factor(food_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(food_sle)
food_sle_beta <-round(coef(food_sle)[2], 2)
food_sle_ci_ll<-round(confint(food_sle),2) [2,1]
food_sle_ci_ul<-round(confint(food_sle),2) [2,2]
food_sle<-cbind(n, food_sle_beta, food_sle_ci_ll, food_sle_ci_ul)

food_unplan<-lm(log(pgf.sg)~as.factor(food_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(food_unplan)
food_unplan_beta <-round(coef(food_unplan)[2], 2)
food_unplan_ci_ll<-round(confint(food_unplan),2) [2,1]
food_unplan_ci_ul<-round(confint(food_unplan),2) [2,2]
food_unplan<-cbind(n, food_unplan_beta, food_unplan_ci_ll, food_unplan_ci_ul)


job_hood<-lm(log(pgf.sg)~as.factor(job_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat  + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(job_hood)
job_hood_beta <-round(coef(job_hood)[2], 2)
job_hood_ci_ll<-round(confint(job_hood),2) [2,1]
job_hood_ci_ul<-round(confint(job_hood),2) [2,2]
job_hood<-cbind(n, job_hood_beta, job_hood_ci_ll, job_hood_ci_ul)


job_sle<-lm(log(pgf.sg)~as.factor(job_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
              food_cat + econ_cat  + caregiving_missing + hood_cat_r + unplanned_pregnancy + ladder_cat,
            data = df.wide)

n<-nobs(job_sle)
job_sle_beta <-round(coef(job_sle)[2], 2)
job_sle_ci_ll<-round(confint(job_sle),2) [2,1]
job_sle_ci_ul<-round(confint(job_sle),2) [2,2]
job_sle<-cbind(n, job_sle_beta, job_sle_ci_ll, job_sle_ci_ul)

job_unplan<-lm(log(pgf.sg)~as.factor(job_unplan)   + as.numeric(mat_age) +as.numeric(ppbmi) +
                 food_cat + econ_cat  + caregiving_missing + hood_cat_r + sle_cat + ladder_cat,
               data = df.wide)

n<-nobs(job_unplan)
job_unplan_beta <-round(coef(job_unplan)[2], 2)
job_unplan_ci_ll<-round(confint(job_unplan),2) [2,1]
job_unplan_ci_ul<-round(confint(job_unplan),2) [2,2]
job_unplan<-cbind(n, job_unplan_beta, job_unplan_ci_ll, job_unplan_ci_ul)


hood_care<-lm(log(pgf.sg)~as.factor(hood_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy  + sle_cat + ladder_cat,
              data = df.wide)

n<-nobs(hood_care)
hood_care_beta <-round(coef(hood_care)[2], 2)
hood_care_ci_ll<-round(confint(hood_care),2) [2,1]
hood_care_ci_ul<-round(confint(hood_care),2) [2,2]
hood_care<-cbind(n, hood_care_beta, hood_care_ci_ll, hood_care_ci_ul)

hood_sle<-lm(log(pgf.sg)~as.factor(hood_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy + caregiving_missing + ladder_cat,
             data = df.wide)

n<-nobs(hood_sle)
hood_sle_beta <-round(coef(hood_sle)[2], 2)
hood_sle_ci_ll<-round(confint(hood_sle),2) [2,1]
hood_sle_ci_ul<-round(confint(hood_sle),2) [2,2]
hood_sle<-cbind(n, hood_sle_beta, hood_sle_ci_ll, hood_sle_ci_ul)


hood_unplan<-lm(log(pgf.sg)~as.factor(hood_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(hood_unplan)
hood_unplan_beta <-round(coef(hood_unplan)[2], 2)
hood_unplan_ci_ll<-round(confint(hood_unplan),2) [2,1]
hood_unplan_ci_ul<-round(confint(hood_unplan),2) [2,2]
hood_unplan<-cbind(n, hood_unplan_beta, hood_unplan_ci_ll, hood_unplan_ci_ul)

hood_lad<-lm(log(pgf.sg)~as.factor(hood_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(hood_lad)
hood_lad_beta <-round(coef(hood_lad)[2], 2)
hood_lad_ci_ll<-round(confint(hood_lad),2) [2,1]
hood_lad_ci_ul<-round(confint(hood_lad),2) [2,2]
hood_lad<-cbind(n, hood_lad_beta, hood_lad_ci_ll, hood_lad_ci_ul)

care_sle <-lm(log(pgf.sg)~as.factor(care_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(care_sle)
care_sle_beta <-round(coef(care_sle)[2], 2)
care_sle_ci_ll<-round(confint(care_sle),2) [2,1]
care_sle_ci_ul<-round(confint(care_sle),2) [2,2]
care_sle<-cbind(n, care_sle_beta, care_sle_ci_ll, care_sle_ci_ul)

care_unplan <-lm(log(pgf.sg)~as.factor(care_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                   food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + sle_cat + ladder_cat,
                 data = df.wide)

n<-nobs(care_unplan)
care_unplan_beta <-round(coef(care_unplan)[2], 2)
care_unplan_ci_ll<-round(confint(care_unplan),2) [2,1]
care_unplan_ci_ul<-round(confint(care_unplan),2) [2,2]
care_unplan<-cbind(n, care_unplan_beta, care_unplan_ci_ll, care_unplan_ci_ul)

sle_unplan <-lm(log(pgf.sg)~as.factor(sle_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(sle_unplan)
sle_unplan_beta <-round(coef(sle_unplan)[2], 2)
sle_unplan_ci_ll<-round(confint(sle_unplan),2) [2,1]
sle_unplan_ci_ul<-round(confint(sle_unplan),2) [2,2]
sle_unplan<-cbind(n, sle_unplan_beta, sle_unplan_ci_ll, sle_unplan_ci_ul)

sle_lad <-lm(log(pgf.sg)~as.factor(sle_lad)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(sle_lad)
sle_lad_beta <-round(coef(sle_lad)[2], 2)
sle_lad_ci_ll<-round(confint(sle_lad),2) [2,1]
sle_lad_ci_ul<-round(confint(sle_lad),2) [2,2]
sle_lad<-cbind(n, sle_lad_beta, sle_lad_ci_ll, sle_lad_ci_ul)

results_comb_pgf<-rbind(econ_food,
                        econ_job,
                        econ_hood,
                        econ_care,
                        econ_sle,
                        econ_unplan,
                        econ_lad,
                        food_hood,
                        food_care,
                        food_sle,
                        food_unplan,
                        job_hood,
                        job_sle,
                        job_unplan,
                        hood_care,
                        hood_sle,
                        hood_unplan,
                        hood_lad,
                        care_sle,
                        care_unplan,
                        sle_unplan,
                        sle_lad)
results_comb_pgf <- as.data.frame(results_comb_pgf) %>%
  mutate(Ox_Stress = "pgf.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_pgf)) %>%
  rename("Beta" = econ_food_beta, 
         "conf_low" = econ_food_ci_ll, 
         "conf_high" = econ_food_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)

################ 8 - iso, chemical
econ_food<-lm(log(iso.chem.sg)~as.factor(econ_food)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)
n<-nobs(econ_food)
econ_food_beta <-round(coef(econ_food)[2], 2)
econ_food_ci_ll<-round(confint(econ_food),2) [2,1]
econ_food_ci_ul<-round(confint(econ_food),2) [2,2]
econ_food<-cbind(n, econ_food_beta, econ_food_ci_ll, econ_food_ci_ul)


econ_job<-lm(log(iso.chem.sg)~as.factor(econ_job)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_job)
econ_job_beta <-round(coef(econ_job)[2], 2)
econ_job_ci_ll<-round(confint(econ_job),2) [2,1]
econ_job_ci_ul<-round(confint(econ_job),2) [2,2]
econ_job<-cbind(n, econ_job_beta, econ_job_ci_ll, econ_job_ci_ul)


econ_hood<-lm(log(iso.chem.sg)~as.factor(econ_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_hood)
econ_hood_beta <-round(coef(econ_hood)[2], 2)
econ_hood_ci_ll<-round(confint(econ_hood),2) [2,1]
econ_hood_ci_ul<-round(confint(econ_hood),2) [2,2]
econ_hood<-cbind(n, econ_hood_beta, econ_hood_ci_ll, econ_hood_ci_ul)

econ_care<-lm(log(iso.chem.sg)~as.factor(econ_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_care)
econ_care_beta <-round(coef(econ_care)[2], 2)
econ_care_ci_ll<-round(confint(econ_care),2) [2,1]
econ_care_ci_ul<-round(confint(econ_care),2) [2,2]
econ_care<-cbind(n, econ_care_beta, econ_care_ci_ll, econ_care_ci_ul)

econ_sle<-lm(log(iso.chem.sg)~as.factor(econ_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_sle)
econ_sle_beta <-round(coef(econ_sle)[2], 2)
econ_sle_ci_ll<-round(confint(econ_sle),2) [2,1]
econ_sle_ci_ul<-round(confint(econ_sle),2) [2,2]
econ_sle<-cbind(n, econ_sle_beta, econ_sle_ci_ll, econ_sle_ci_ul)

econ_unplan<-lm(log(iso.chem.sg)~as.factor(econ_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(econ_unplan)
econ_unplan_beta <-round(coef(econ_unplan)[2], 2)
econ_unplan_ci_ll<-round(confint(econ_unplan),2) [2,1]
econ_unplan_ci_ul<-round(confint(econ_unplan),2) [2,2]
econ_unplan<-cbind(n, econ_unplan_beta, econ_unplan_ci_ll, econ_unplan_ci_ul)

econ_lad<-lm(log(iso.chem.sg)~as.factor(econ_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy,
             data = df.wide)

n<-nobs(econ_lad)
econ_lad_beta <-round(coef(econ_lad)[2], 2)
econ_lad_ci_ll<-round(confint(econ_lad),2) [2,1]
econ_lad_ci_ul<-round(confint(econ_lad),2) [2,2]
econ_lad<-cbind(n, econ_lad_beta, econ_lad_ci_ll, econ_lad_ci_ul)


food_hood<-lm(log(iso.chem.sg)~as.factor(food_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_hood)
food_hood_beta <-round(coef(food_hood)[2], 2)
food_hood_ci_ll<-round(confint(food_hood),2) [2,1]
food_hood_ci_ul<-round(confint(food_hood),2) [2,2]
food_hood<-cbind(n, food_hood_beta, food_hood_ci_ll, food_hood_ci_ul)


food_care<-lm(log(iso.chem.sg)~as.factor(food_care)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_care)
food_care_beta <-round(coef(food_care)[2], 2)
food_care_ci_ll<-round(confint(food_care),2) [2,1]
food_care_ci_ul<-round(confint(food_care),2) [2,2]
food_care<-cbind(n, food_care_beta, food_care_ci_ll, food_care_ci_ul)

food_sle<-lm(log(iso.chem.sg)~as.factor(food_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(food_sle)
food_sle_beta <-round(coef(food_sle)[2], 2)
food_sle_ci_ll<-round(confint(food_sle),2) [2,1]
food_sle_ci_ul<-round(confint(food_sle),2) [2,2]
food_sle<-cbind(n, food_sle_beta, food_sle_ci_ll, food_sle_ci_ul)

food_unplan<-lm(log(iso.chem.sg)~as.factor(food_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(food_unplan)
food_unplan_beta <-round(coef(food_unplan)[2], 2)
food_unplan_ci_ll<-round(confint(food_unplan),2) [2,1]
food_unplan_ci_ul<-round(confint(food_unplan),2) [2,2]
food_unplan<-cbind(n, food_unplan_beta, food_unplan_ci_ll, food_unplan_ci_ul)


job_hood<-lm(log(iso.chem.sg)~as.factor(job_hood)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + econ_cat  + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(job_hood)
job_hood_beta <-round(coef(job_hood)[2], 2)
job_hood_ci_ll<-round(confint(job_hood),2) [2,1]
job_hood_ci_ul<-round(confint(job_hood),2) [2,2]
job_hood<-cbind(n, job_hood_beta, job_hood_ci_ll, job_hood_ci_ul)


job_sle<-lm(log(iso.chem.sg)~as.factor(job_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
              food_cat + econ_cat  + caregiving_missing + hood_cat_r + unplanned_pregnancy + ladder_cat,
            data = df.wide)

n<-nobs(job_sle)
job_sle_beta <-round(coef(job_sle)[2], 2)
job_sle_ci_ll<-round(confint(job_sle),2) [2,1]
job_sle_ci_ul<-round(confint(job_sle),2) [2,2]
job_sle<-cbind(n, job_sle_beta, job_sle_ci_ll, job_sle_ci_ul)

job_unplan<-lm(log(iso.chem.sg)~as.factor(job_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                 food_cat + econ_cat  + caregiving_missing + hood_cat_r + sle_cat + ladder_cat,
               data = df.wide)

n<-nobs(job_unplan)
job_unplan_beta <-round(coef(job_unplan)[2], 2)
job_unplan_ci_ll<-round(confint(job_unplan),2) [2,1]
job_unplan_ci_ul<-round(confint(job_unplan),2) [2,2]
job_unplan<-cbind(n, job_unplan_beta, job_unplan_ci_ll, job_unplan_ci_ul)


hood_care<-lm(log(iso.chem.sg)~as.factor(hood_care)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy  + sle_cat + ladder_cat,
              data = df.wide)

n<-nobs(hood_care)
hood_care_beta <-round(coef(hood_care)[2], 2)
hood_care_ci_ll<-round(confint(hood_care),2) [2,1]
hood_care_ci_ul<-round(confint(hood_care),2) [2,2]
hood_care<-cbind(n, hood_care_beta, hood_care_ci_ll, hood_care_ci_ul)

hood_sle<-lm(log(iso.chem.sg)~as.factor(hood_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy + caregiving_missing + ladder_cat,
             data = df.wide)

n<-nobs(hood_sle)
hood_sle_beta <-round(coef(hood_sle)[2], 2)
hood_sle_ci_ll<-round(confint(hood_sle),2) [2,1]
hood_sle_ci_ul<-round(confint(hood_sle),2) [2,2]
hood_sle<-cbind(n, hood_sle_beta, hood_sle_ci_ll, hood_sle_ci_ul)


hood_unplan<-lm(log(iso.chem.sg)~as.factor(hood_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(hood_unplan)
hood_unplan_beta <-round(coef(hood_unplan)[2], 2)
hood_unplan_ci_ll<-round(confint(hood_unplan),2) [2,1]
hood_unplan_ci_ul<-round(confint(hood_unplan),2) [2,2]
hood_unplan<-cbind(n, hood_unplan_beta, hood_unplan_ci_ll, hood_unplan_ci_ul)

hood_lad<-lm(log(iso.chem.sg)~as.factor(hood_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(hood_lad)
hood_lad_beta <-round(coef(hood_lad)[2], 2)
hood_lad_ci_ll<-round(confint(hood_lad),2) [2,1]
hood_lad_ci_ul<-round(confint(hood_lad),2) [2,2]
hood_lad<-cbind(n, hood_lad_beta, hood_lad_ci_ll, hood_lad_ci_ul)

care_sle <-lm(log(iso.chem.sg)~as.factor(care_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(care_sle)
care_sle_beta <-round(coef(care_sle)[2], 2)
care_sle_ci_ll<-round(confint(care_sle),2) [2,1]
care_sle_ci_ul<-round(confint(care_sle),2) [2,2]
care_sle<-cbind(n, care_sle_beta, care_sle_ci_ll, care_sle_ci_ul)

care_unplan <-lm(log(iso.chem.sg)~as.factor(care_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                   food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + sle_cat + ladder_cat,
                 data = df.wide)

n<-nobs(care_unplan)
care_unplan_beta <-round(coef(care_unplan)[2], 2)
care_unplan_ci_ll<-round(confint(care_unplan),2) [2,1]
care_unplan_ci_ul<-round(confint(care_unplan),2) [2,2]
care_unplan<-cbind(n, care_unplan_beta, care_unplan_ci_ll, care_unplan_ci_ul)

sle_unplan <-lm(log(iso.chem.sg)~as.factor(sle_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(sle_unplan)
sle_unplan_beta <-round(coef(sle_unplan)[2], 2)
sle_unplan_ci_ll<-round(confint(sle_unplan),2) [2,1]
sle_unplan_ci_ul<-round(confint(sle_unplan),2) [2,2]
sle_unplan<-cbind(n, sle_unplan_beta, sle_unplan_ci_ll, sle_unplan_ci_ul)

sle_lad <-lm(log(iso.chem.sg)~as.factor(sle_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(sle_lad)
sle_lad_beta <-round(coef(sle_lad)[2], 2)
sle_lad_ci_ll<-round(confint(sle_lad),2) [2,1]
sle_lad_ci_ul<-round(confint(sle_lad),2) [2,2]
sle_lad<-cbind(n, sle_lad_beta, sle_lad_ci_ll, sle_lad_ci_ul)

results_comb_iso.chem<-rbind(econ_food,
                        econ_job,
                        econ_hood,
                        econ_care,
                        econ_sle,
                        econ_unplan,
                        econ_lad,
                        food_hood,
                        food_care,
                        food_sle,
                        food_unplan,
                        job_hood,
                        job_sle,
                        job_unplan,
                        hood_care,
                        hood_sle,
                        hood_unplan,
                        hood_lad,
                        care_sle,
                        care_unplan,
                        sle_unplan,
                        sle_lad)
results_comb_iso.chem <- as.data.frame(results_comb_iso.chem) %>%
  mutate(Ox_Stress = "iso.chem.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_iso.chem)) %>%
  rename("Beta" = econ_food_beta, 
         "conf_low" = econ_food_ci_ll, 
         "conf_high" = econ_food_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)

################ 8 - iso, enzymatic
econ_food<-lm(log(iso.enz.sg)~as.factor(econ_food)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)
n<-nobs(econ_food)
econ_food_beta <-round(coef(econ_food)[2], 2)
econ_food_ci_ll<-round(confint(econ_food),2) [2,1]
econ_food_ci_ul<-round(confint(econ_food),2) [2,2]
econ_food<-cbind(n, econ_food_beta, econ_food_ci_ll, econ_food_ci_ul)


econ_job<-lm(log(iso.enz.sg)~as.factor(econ_job)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_job)
econ_job_beta <-round(coef(econ_job)[2], 2)
econ_job_ci_ll<-round(confint(econ_job),2) [2,1]
econ_job_ci_ul<-round(confint(econ_job),2) [2,2]
econ_job<-cbind(n, econ_job_beta, econ_job_ci_ll, econ_job_ci_ul)


econ_hood<-lm(log(iso.enz.sg)~as.factor(econ_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_hood)
econ_hood_beta <-round(coef(econ_hood)[2], 2)
econ_hood_ci_ll<-round(confint(econ_hood),2) [2,1]
econ_hood_ci_ul<-round(confint(econ_hood),2) [2,2]
econ_hood<-cbind(n, econ_hood_beta, econ_hood_ci_ll, econ_hood_ci_ul)

econ_care<-lm(log(iso.enz.sg)~as.factor(econ_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + job_highstrain_missing + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(econ_care)
econ_care_beta <-round(coef(econ_care)[2], 2)
econ_care_ci_ll<-round(confint(econ_care),2) [2,1]
econ_care_ci_ul<-round(confint(econ_care),2) [2,2]
econ_care<-cbind(n, econ_care_beta, econ_care_ci_ll, econ_care_ci_ul)

econ_sle<-lm(log(iso.enz.sg)~as.factor(econ_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(econ_sle)
econ_sle_beta <-round(coef(econ_sle)[2], 2)
econ_sle_ci_ll<-round(confint(econ_sle),2) [2,1]
econ_sle_ci_ul<-round(confint(econ_sle),2) [2,2]
econ_sle<-cbind(n, econ_sle_beta, econ_sle_ci_ll, econ_sle_ci_ul)

econ_unplan<-lm(log(iso.enz.sg)~as.factor(econ_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(econ_unplan)
econ_unplan_beta <-round(coef(econ_unplan)[2], 2)
econ_unplan_ci_ll<-round(confint(econ_unplan),2) [2,1]
econ_unplan_ci_ul<-round(confint(econ_unplan),2) [2,2]
econ_unplan<-cbind(n, econ_unplan_beta, econ_unplan_ci_ll, econ_unplan_ci_ul)

econ_lad<-lm(log(iso.enz.sg)~as.factor(econ_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + job_highstrain_missing + hood_cat_r + caregiving_missing + sle_cat + unplanned_pregnancy,
             data = df.wide)

n<-nobs(econ_lad)
econ_lad_beta <-round(coef(econ_lad)[2], 2)
econ_lad_ci_ll<-round(confint(econ_lad),2) [2,1]
econ_lad_ci_ul<-round(confint(econ_lad),2) [2,2]
econ_lad<-cbind(n, econ_lad_beta, econ_lad_ci_ll, econ_lad_ci_ul)


food_hood<-lm(log(iso.enz.sg)~as.factor(food_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_hood)
food_hood_beta <-round(coef(food_hood)[2], 2)
food_hood_ci_ll<-round(confint(food_hood),2) [2,1]
food_hood_ci_ul<-round(confint(food_hood),2) [2,2]
food_hood<-cbind(n, food_hood_beta, food_hood_ci_ll, food_hood_ci_ul)


food_care<-lm(log(iso.enz.sg)~as.factor(food_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                job_highstrain_missing + econ_cat + hood_cat_r + sle_cat + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(food_care)
food_care_beta <-round(coef(food_care)[2], 2)
food_care_ci_ll<-round(confint(food_care),2) [2,1]
food_care_ci_ul<-round(confint(food_care),2) [2,2]
food_care<-cbind(n, food_care_beta, food_care_ci_ll, food_care_ci_ul)

food_sle<-lm(log(iso.enz.sg)~as.factor(food_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(food_sle)
food_sle_beta <-round(coef(food_sle)[2], 2)
food_sle_ci_ll<-round(confint(food_sle),2) [2,1]
food_sle_ci_ul<-round(confint(food_sle),2) [2,2]
food_sle<-cbind(n, food_sle_beta, food_sle_ci_ll, food_sle_ci_ul)

food_unplan<-lm(log(iso.enz.sg)~as.factor(food_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                  job_highstrain_missing + econ_cat + hood_cat_r + caregiving_missing + sle_cat + ladder_cat,
                data = df.wide)

n<-nobs(food_unplan)
food_unplan_beta <-round(coef(food_unplan)[2], 2)
food_unplan_ci_ll<-round(confint(food_unplan),2) [2,1]
food_unplan_ci_ul<-round(confint(food_unplan),2) [2,2]
food_unplan<-cbind(n, food_unplan_beta, food_unplan_ci_ll, food_unplan_ci_ul)


job_hood<-lm(log(iso.enz.sg)~as.factor(job_hood)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat  + caregiving_missing + sle_cat + unplanned_pregnancy + ladder_cat,
             data = df.wide)

n<-nobs(job_hood)
job_hood_beta <-round(coef(job_hood)[2], 2)
job_hood_ci_ll<-round(confint(job_hood),2) [2,1]
job_hood_ci_ul<-round(confint(job_hood),2) [2,2]
job_hood<-cbind(n, job_hood_beta, job_hood_ci_ll, job_hood_ci_ul)


job_sle<-lm(log(iso.enz.sg)~as.factor(job_sle)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
              food_cat + econ_cat  + caregiving_missing + hood_cat_r + unplanned_pregnancy + ladder_cat,
            data = df.wide)

n<-nobs(job_sle)
job_sle_beta <-round(coef(job_sle)[2], 2)
job_sle_ci_ll<-round(confint(job_sle),2) [2,1]
job_sle_ci_ul<-round(confint(job_sle),2) [2,2]
job_sle<-cbind(n, job_sle_beta, job_sle_ci_ll, job_sle_ci_ul)

job_unplan<-lm(log(iso.enz.sg)~as.factor(job_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                 food_cat + econ_cat  + caregiving_missing + hood_cat_r + sle_cat + ladder_cat,
               data = df.wide)

n<-nobs(job_unplan)
job_unplan_beta <-round(coef(job_unplan)[2], 2)
job_unplan_ci_ll<-round(confint(job_unplan),2) [2,1]
job_unplan_ci_ul<-round(confint(job_unplan),2) [2,2]
job_unplan<-cbind(n, job_unplan_beta, job_unplan_ci_ll, job_unplan_ci_ul)


hood_care<-lm(log(iso.enz.sg)~as.factor(hood_care)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy  + sle_cat + ladder_cat,
              data = df.wide)

n<-nobs(hood_care)
hood_care_beta <-round(coef(hood_care)[2], 2)
hood_care_ci_ll<-round(confint(hood_care),2) [2,1]
hood_care_ci_ul<-round(confint(hood_care),2) [2,2]
hood_care<-cbind(n, hood_care_beta, hood_care_ci_ll, hood_care_ci_ul)

hood_sle<-lm(log(iso.enz.sg)~as.factor(hood_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + unplanned_pregnancy + caregiving_missing + ladder_cat,
             data = df.wide)

n<-nobs(hood_sle)
hood_sle_beta <-round(coef(hood_sle)[2], 2)
hood_sle_ci_ll<-round(confint(hood_sle),2) [2,1]
hood_sle_ci_ul<-round(confint(hood_sle),2) [2,2]
hood_sle<-cbind(n, hood_sle_beta, hood_sle_ci_ll, hood_sle_ci_ul)


hood_unplan<-lm(log(iso.enz.sg)~as.factor(hood_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(hood_unplan)
hood_unplan_beta <-round(coef(hood_unplan)[2], 2)
hood_unplan_ci_ll<-round(confint(hood_unplan),2) [2,1]
hood_unplan_ci_ul<-round(confint(hood_unplan),2) [2,2]
hood_unplan<-cbind(n, hood_unplan_beta, hood_unplan_ci_ll, hood_unplan_ci_ul)

hood_lad<-lm(log(iso.enz.sg)~as.factor(hood_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing + sle_cat + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(hood_lad)
hood_lad_beta <-round(coef(hood_lad)[2], 2)
hood_lad_ci_ll<-round(confint(hood_lad),2) [2,1]
hood_lad_ci_ul<-round(confint(hood_lad),2) [2,2]
hood_lad<-cbind(n, hood_lad_beta, hood_lad_ci_ll, hood_lad_ci_ul)

care_sle <-lm(log(iso.enz.sg)~as.factor(care_sle)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + unplanned_pregnancy + ladder_cat,
              data = df.wide)

n<-nobs(care_sle)
care_sle_beta <-round(coef(care_sle)[2], 2)
care_sle_ci_ll<-round(confint(care_sle),2) [2,1]
care_sle_ci_ul<-round(confint(care_sle),2) [2,2]
care_sle<-cbind(n, care_sle_beta, care_sle_ci_ll, care_sle_ci_ul)

care_unplan <-lm(log(iso.enz.sg)~as.factor(care_unplan)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
                   food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + sle_cat + ladder_cat,
                 data = df.wide)

n<-nobs(care_unplan)
care_unplan_beta <-round(coef(care_unplan)[2], 2)
care_unplan_ci_ll<-round(confint(care_unplan),2) [2,1]
care_unplan_ci_ul<-round(confint(care_unplan),2) [2,2]
care_unplan<-cbind(n, care_unplan_beta, care_unplan_ci_ll, care_unplan_ci_ul)

sle_unplan <-lm(log(iso.enz.sg)~as.factor(sle_unplan)   + as.numeric(mat_age) + as.numeric(ppbmi) +
                  food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + ladder_cat,
                data = df.wide)

n<-nobs(sle_unplan)
sle_unplan_beta <-round(coef(sle_unplan)[2], 2)
sle_unplan_ci_ll<-round(confint(sle_unplan),2) [2,1]
sle_unplan_ci_ul<-round(confint(sle_unplan),2) [2,2]
sle_unplan<-cbind(n, sle_unplan_beta, sle_unplan_ci_ll, sle_unplan_ci_ul)

sle_lad <-lm(log(iso.enz.sg)~as.factor(sle_lad)   + as.numeric(mat_age) +  as.numeric(ppbmi) +
               food_cat + econ_cat + job_highstrain_missing  + hood_cat_r + caregiving_missing + unplanned_pregnancy,
             data = df.wide)

n<-nobs(sle_lad)
sle_lad_beta <-round(coef(sle_lad)[2], 2)
sle_lad_ci_ll<-round(confint(sle_lad),2) [2,1]
sle_lad_ci_ul<-round(confint(sle_lad),2) [2,2]
sle_lad<-cbind(n, sle_lad_beta, sle_lad_ci_ll, sle_lad_ci_ul)

results_comb_iso.enz<-rbind(econ_food,
                        econ_job,
                        econ_hood,
                        econ_care,
                        econ_sle,
                        econ_unplan,
                        econ_lad,
                        food_hood,
                        food_care,
                        food_sle,
                        food_unplan,
                        job_hood,
                        job_sle,
                        job_unplan,
                        hood_care,
                        hood_sle,
                        hood_unplan,
                        hood_lad,
                        care_sle,
                        care_unplan,
                        sle_unplan,
                        sle_lad)
results_comb_iso.enz <- as.data.frame(results_comb_iso.enz) %>%
  mutate(Ox_Stress = "iso.enz.sg", 
         model = "Fully Adjusted",
         Stress = rownames(results_comb_iso.enz)) %>%
  rename("Beta" = econ_food_beta, 
         "conf_low" = econ_food_ci_ll, 
         "conf_high" = econ_food_ci_ul,
         "N" = n) %>%
  dplyr::select(Stress, Ox_Stress, N, Beta, conf_low, conf_high, model)


table_s5 <- tables_5

#######################################################
## Fig 1 - linear mixed effect models - single stressors -  covariate adjusted (done)
#######################################################

single_stressor_plot <- bind_rows(
  results_2)




single_stressor_plot<-single_stressor_plot[which(single_stressor_plot$Ox_Stress == "iso.sg" |
                                                   single_stressor_plot$Ox_Stress == "isom.sg"|
                                                   single_stressor_plot$Ox_Stress == "isom.new.sg" |
                                                   single_stressor_plot$Ox_Stress == "pgf.sg"),]
exposure_names<-c("caregiving", 
                  "financial strain", 
                  "food insecurity",
                  "poor neighborhood quality",
                  "job strain",
                  "low perceived community status",
                  "stressful life events",
                  "unplanned pregnancy")    


stress_label <- c("15-F2t-IsoP", "2,3-dinor-5,6-dihydro-15-F2t-IsoP", "2,3-dinor-15-F2t-IsoP", "PGF2α ")
names(stress_label) <- c("iso.sg", "isom.sg", "isom.new.sg", "pgf.sg")


fig1<-ggplot(single_stressor_plot, aes(x = Beta, y = Stress)) +
  geom_point(position = position_dodge(width = 0.25), size = 2) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.25, size = 0.60, position = "dodge") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
  facet_grid(~Ox_Stress, labeller = labeller(Ox_Stress = stress_label)) +
  xlab("Beta (95% Confidence Interval)") +
  scale_y_discrete(labels = exposure_names) +
  ylab(" ") +
  scale_color_locuszoom() +
  guides(color = "none", linetype = guide_legend(reverse = TRUE)) + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 15, angle = 45, color = "black"),
        axis.text.y = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        strip.background =element_rect(colour="black",
                                       fill="white"),
        strip.text = element_text(face = "bold"))


pdf("fig1.pdf", fig.width = 15, fig.height = 15)
fig1
dev.off()
table_s5 <- rbind(results_1, results_2, results_comb_iso, results_comb_iso.chem, results_comb_iso.enz, results_comb_isom, results_comb_isom.new, results_comb_pgf)
table_s5 %>%
  mutate("Beta (CI)" = paste0(Beta, " (", conf_low, ", ", conf_high, ")"))  %>%
  dplyr::select(Stress, Ox_Stress, model, N, `Beta (CI)`) %>%
  as_grouped_data(groups = "model") %>%
  flextable() %>%
  autofit()

#######################################################
## Fig 2 - linear mixed effect models - combination stressors -  covariate adjusted
#######################################################


comb_stressor_plot <- bind_rows(
  results_4 %>% mutate(Model = "Covar Adj")
)

comb_stressor_plot <- comb_stressor_plot[order(comb_stressor_plot$Beta),]

model_label <- c("Covariate Adjusted")
names(model_label) <- c("Covariate Adjusted")



comb_stressor_plot<-comb_stressor_plot[which(comb_stressor_plot$Ox_Stress == "iso.sg" |
                                                   comb_stressor_plot$Ox_Stress == "isom.sg"|
                                                   comb_stressor_plot$Ox_Stress == "isom.new.sg" |
                                                   comb_stressor_plot$Ox_Stress == "pgf.sg"),]


exposure_names<-c("caregiving & stressful life events",
                  "caregiving & unplanned pregnancy",
                  "financial strain & caregiving",
                  "financial strain & food insecurity",
                  "financial strain & poor neighborhood quality",
                  "financial strain & job strain",
                  "financial strain & low perceived community status",
                  "financial strain & stressful life events",
                  "financial strain & unplanned pregnancy",
                  "food insecurity & caregiving",
                  "food insecurity & poor neighborhood quality",
                  "food insecurity & stressful life events",
                  "food insecruity & unplanned pregnancy",
                  "poor neighborhood quality & caregiving",
                  "poor neighborhood quality & low perceived community status",
                  "poor neighborhood quality & stressful life events",
                  "poor neighborhood quality & unplanned pregnancy",
                  "job strain & stressful life events",
                  "job strain & unplanned pregnancy",
                  "stressful life events & low perceived community status",
                  "stressful life events & unplanned pregnancy")

stress_label <- c("15-F2t-IsoP", "2,3-dinor-5,6-dihydro-15-F2t-IsoP", "2,3-dinor-15-F2t-IsoP", "PGF2α ")
names(stress_label) <- c("iso.sg", "isom.sg", "isom.new.sg", "pgf.sg")


comb_stressor_plot$sig<-ifelse(comb_stressor_plot$conf_low > 0, 
                               "Significant", "Non-significant")
  
fig2<-ggplot(comb_stressor_plot, aes(x = Beta, y = Stress, color=sig)) +
  geom_point(position = position_dodge(width = 0.25), size = 2) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.25, size = 0.60, position = "dodge") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
  facet_grid(~Ox_Stress, labeller = labeller(Ox_Stress = stress_label)) +
  xlab("Beta (95% Confidence Interval)") +
   scale_y_discrete(labels = exposure_names) +
  ylab(" ") +
  scale_color_manual(values=c("gray50","black")) +
  guides(color = "none", linetype = guide_legend(reverse = FALSE)) + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 15, angle = 45, color = "black"),
        axis.text.y = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        strip.background =element_rect(colour="black",
                                       fill="white"),
        strip.text = element_text(face = "bold"))

fig2



#######################################################
## Graphical abstract - linear regression model with just 8-iso 
## - individual stressors  -  covariate adjusted
#######################################################

single_stressor_plot <- bind_rows(
  results_2)


single_stressor_plot<-single_stressor_plot[which(single_stressor_plot$Ox_Stress == "iso.sg" ),]
exposure_names<-c("caregiving", 
                  "financial strain", 
                  "food insecurity",
                  "poor neighborhood quality",
                  "job strain",
                  "low perceived community status",
                  "stressful life events",
                  "unplanned pregnancy")    


stress_label <- c("15-F2t-IsoP")
names(stress_label) <- c("iso.sg")


abstrct<-ggplot(single_stressor_plot, aes(x = Beta, y = Stress)) +
  geom_point(position = position_dodge(width = 0.25), size = 2) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.25, size = 0.60, position = "dodge") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
  facet_grid(~Ox_Stress, labeller = labeller(Ox_Stress = stress_label)) +
  xlab("Beta (95% Confidence Interval)") +
  scale_y_discrete(labels = exposure_names) +
  ylab(" ") +
  scale_color_locuszoom() +
  guides(color = "none", linetype = guide_legend(reverse = TRUE)) + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 15, angle = 45, color = "black"),
        axis.text.y = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        strip.background =element_rect(colour="black",
                                       fill="white"),
        strip.text = element_text(face = "bold"))



abstrct


