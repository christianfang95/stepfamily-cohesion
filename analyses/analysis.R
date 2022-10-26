
#library(reticulate)#

#use_python("~/Library/r-miniconda-arm64/bin/python3.9")
#py_install("pandas")

#Run file
#py_run_file("/Users/christianfang/GitHub/stepfamily-cohesion/data cleaning/data_cleaning.py")
#data <- py$frame

#frame <- data.frame(data)




#arr = py_to_r(py$arr)
#frame <- data$T

#arr = py_to_r(frame)

#df <- data.frame(arr)

#df <- py_to_r.pandas.core.frame.DataFrame(data)

library(mice)
library(ggeffects)
library(ggpubr)
library(cowplot)
library(ggplot2)
library(sjmisc)
library(ggeffects)
library(estimatr)
library(esc)
library(ltm)
#Import data as csv

raw <- data.frame(read.csv('/Volumes/webdav.uu.nl/Data/Research/FSW/Research_data/SOC/Anne-Rigt Poortman/Nieuwe Families NL/Zielinski/Cohesion paper/data_cleaned.csv'))
raw$cohesion <- (raw$cohes_a + raw$cohes_b + raw$cohes_c + raw$cohes_d) / 4

mean_before <- mean(raw$cohesion, na.rm = TRUE)

#Select only relevant variables

data <- dplyr::select(raw, CBSvolgnr_hh, cohes_a, cohes_b, cohes_c, cohes_d, cohesion, combinations, 
                      sharedchild, parttime, age_child, female_child, age_parent, 
                      female_respondent, duration, educ_par, educ_partner, age_partner, A3J02, A3J29, A3P12)

#Check which variables have missing values
sapply(data, function(x) sum(is.na(x)))


#Multiple imputations
impute_data <- function(original_data){
  imp <- mice(original_data, maxit=0)
  predictor_matrix<-imp$predictorMatrix
  predictor_matrix[,c("CBSvolgnr_hh")]=0
  method<-imp$method
  method[c("CBSvolgnr_hh")]=""
  method[c("cohes_a")]="cart"
  method[c("cohes_b")]="cart"
  method[c("cohes_c")]="cart"
  method[c("cohes_d")]="cart"
  method[c("sharedchild")]="cart"
  method[c("female_child")]="cart"
  method[c("duration")]="cart"
  method[c("educ_par")]="cart"
  method[c("educ_partner")]="cart"
  method[c("age_partner")]="cart"
  method[c("cohesion")]="cart"
  method[c("A3J02")]="cart"
  method[c("A3J29")]="cart"
  method[c("A3P12")]="cart"
  
  imputed <- mice(original_data, m=5, maxit=50, meth= method, seed=500,  predictorMatrix = predictor_matrix)
  return(imputed)}

imputed <- impute_data(data)


#Check imputations, seems OK
densityplot(imputed)

#Summary table for descriptive statistics
summary_table <- function(){
  impL <- complete(imputed,"long",include = F) # long format without the original dataset
  #Before imputations
  cohesion_b <- list(mean(raw$cohesion, na.rm = TRUE), min(raw$cohesion, na.rm = TRUE), 
                     max(raw$cohesion, na.rm = TRUE), sd(raw$cohesion, na.rm = TRUE))
  resnostep_b <- list(sum(raw$combinations == 0)/length(raw$combinations), min(raw$combinations, na.rm = TRUE),
                      max(raw$combinations, na.rm = TRUE), '')
  resres_b <- list(sum(raw$combinations == 1)/length(raw$combinations), min(raw$combinations == 1, na.rm = TRUE),
                   max(raw$combinations == 1, na.rm = TRUE), '')
  assym_b <- list(sum(raw$combinations == 2)/length(raw$combinations), min(raw$combinations == 2, na.rm = TRUE),
                  max(raw$combinations == 2, na.rm = TRUE), '')
  parttime_b <- list(mean(raw$parttime, na.rm = TRUE), min(raw$parttime, na.rm = TRUE), 
                     max(raw$parttime, na.rm = TRUE), '')
  
  sharedchild_b <- list(mean(raw$sharedchild, na.rm = TRUE), min(raw$sharedchild, na.rm = TRUE), 
                        max(raw$sharedchild, na.rm = TRUE), '')
  
  age_child_b <- list(mean(raw$age_child, na.rm = TRUE), min(raw$age_child, na.rm = TRUE), 
                      max(raw$age_child, na.rm = TRUE), sd(raw$age_child, na.rm = TRUE))
  
  female_child_b <- list(mean(raw$female_child, na.rm = TRUE), min(raw$female_child, na.rm = TRUE), 
                         max(raw$female_child, na.rm = TRUE), '')
  
  age_parent_b <-  list(mean(raw$age_parent, na.rm = TRUE), min(raw$age_parent, na.rm = TRUE), 
                        max(raw$age_parent, na.rm = TRUE), sd(raw$age_parent, na.rm = TRUE))
  
  female_respondent_b <- list(mean(raw$female_respondent, na.rm = TRUE), min(raw$female_respondent, na.rm = TRUE), 
                              max(raw$female_respondent, na.rm = TRUE), '')
  
  duration_b <- list(mean(raw$duration, na.rm = TRUE), min(raw$duration, na.rm = TRUE), 
                     max(raw$duration, na.rm = TRUE), sd(raw$duration, na.rm = TRUE))
  
  educ_par_b <- list(mean(raw$educ_par, na.rm = TRUE), min(raw$educ_par, na.rm = TRUE), 
                     max(raw$educ_par, na.rm = TRUE), sd(raw$educ_par, na.rm = TRUE))
  
  age_partner_b <-  list(mean(raw$age_partner, na.rm = TRUE), min(raw$age_partner, na.rm = TRUE), 
                         max(raw$age_partner, na.rm = TRUE), sd(raw$age_partner, na.rm = TRUE))
  
  educ_partner_b <- list(mean(raw$educ_partner, na.rm = TRUE), min(raw$educ_partner, na.rm = TRUE), 
                         max(raw$educ_partner, na.rm = TRUE), sd(raw$educ_partner, na.rm = TRUE))
  
  relqual_child_b <- list(mean(raw$A3J02, na.rm = TRUE), min(raw$A3J02, na.rm = TRUE), 
                        max(raw$A3J02, na.rm = TRUE), sd(raw$A3J02, na.rm = TRUE))
    
  rel_child_step_b <- list(mean(raw$A3J29, na.rm = TRUE), min(raw$A3J29, na.rm = TRUE), 
                         max(raw$A3J29, na.rm = TRUE), sd(raw$A3J29, na.rm = TRUE))
  rel_partner_b <- list(mean(raw$A3P12, na.rm = TRUE), min(raw$A3P12, na.rm = TRUE), 
                         max(raw$A3P12, na.rm = TRUE), sd(raw$A3P12, na.rm = TRUE))
  

  list_b <- list(cohesion_b, resnostep_b, resres_b, assym_b, sharedchild_b, parttime_b, age_child_b, 
                 female_child_b, age_parent_b, female_respondent_b, educ_par_b, age_partner_b, educ_partner_b,
                 duration_b, relqual_child_b, rel_child_step_b,  rel_partner_b)
  #After imputation
  resnostep_a <- list(sum(impL$combinations == 0)/length(impL$combinations), min(impL$combinations, na.rm = TRUE),
                      max(impL$combinations, na.rm = TRUE), '')
  resres_a <- list(sum(impL$combinations == 1)/length(impL$combinations), min(impL$combinations == 1, na.rm = TRUE),
                   max(impL$combinations == 1, na.rm = TRUE), '')
  assym_a <- list(sum(impL$combinations == 2)/length(impL$combinations), min(impL$combinations == 2, na.rm = TRUE),
                  max(impL$combinations == 2, na.rm = TRUE), '')
  parttime_a <- list(mean(impL$parttime, na.rm = TRUE), min(impL$parttime, na.rm = TRUE), 
                     max(impL$parttime, na.rm = TRUE), '')
  
  sharedchild_a <- list(mean(impL$sharedchild, na.rm = TRUE), min(impL$sharedchild, na.rm = TRUE), 
                        max(impL$sharedchild, na.rm = TRUE), '')
  
  age_child_a <- list(mean(impL$age_child, na.rm = TRUE), min(impL$age_child, na.rm = TRUE), 
                      max(impL$age_child, na.rm = TRUE), sd(impL$age_child, na.rm = TRUE))
  
  female_child_a <- list(mean(impL$female_child, na.rm = TRUE), min(impL$female_child, na.rm = TRUE), 
                         max(impL$female_child, na.rm = TRUE), '')
  
  age_parent_a <-  list(mean(impL$age_parent, na.rm = TRUE), min(impL$age_parent, na.rm = TRUE), 
                        max(impL$age_parent, na.rm = TRUE), sd(impL$age_parent, na.rm = TRUE))
  
  female_respondent_a <- list(mean(impL$female_respondent, na.rm = TRUE), min(impL$female_respondent, na.rm = TRUE), 
                              max(impL$female_respondent, na.rm = TRUE), '')
  
  duration_a <- list(mean(impL$duration, na.rm = TRUE), min(impL$duration, na.rm = TRUE), 
                     max(impL$duration, na.rm = TRUE), sd(impL$duration, na.rm = TRUE))
  
  educ_par_a <- list(mean(impL$educ_par, na.rm = TRUE), min(impL$educ_par, na.rm = TRUE), 
                     max(impL$educ_par, na.rm = TRUE), sd(impL$educ_par, na.rm = TRUE))
  
  age_partner_a <-  list(mean(impL$age_partner, na.rm = TRUE), min(impL$age_partner, na.rm = TRUE), 
                         max(impL$age_partner, na.rm = TRUE), sd(impL$age_partner, na.rm = TRUE))
  
  educ_partner_a <- list(mean(impL$educ_partner, na.rm = TRUE), min(impL$educ_partner, na.rm = TRUE), 
                         max(impL$educ_partner, na.rm = TRUE), sd(impL$educ_partner, na.rm = TRUE))
  
  relqual_child_a <- list(mean(impL$A3J02, na.rm = TRUE), min(impL$A3J02, na.rm = TRUE), 
                          max(impL$A3J02, na.rm = TRUE), sd(impL$A3J02, na.rm = TRUE))
  rel_child_step_a <- list(mean(impL$A3J29, na.rm = TRUE), min(impL$A3J29, na.rm = TRUE), 
                           max(impL$A3J29, na.rm = TRUE), sd(impL$A3J29, na.rm = TRUE))
  rel_partner_a <- list(mean(impL$A3P12, na.rm = TRUE), min(impL$A3P12, na.rm = TRUE), 
                        max(impL$A3P12, na.rm = TRUE), sd(impL$A3P12, na.rm = TRUE))
  
  
  list_a <- list(cohesion_a, resnostep_a, resres_a, assym_a, sharedchild_a, parttime_a, age_child_a, 
                 female_child_a, age_parent_a, female_respondent_a, educ_par_a, age_partner_a, educ_partner_a,
                 duration_a, relqual_child_a, rel_child_step_a, rel_partner_a)
  
  frame_b <- as.data.frame(do.call(rbind, list_b))
  colnames(frame_b) <- c('M', 'Min', 'Max', 'sd')
  rownames(frame_b) <- c('Cohesion', 'Res-no step', 'Res - res', 'Asymm', 'Shared Child', 'parttime',
                         'Age child', 'Child female', 'Age parent', 'Parent female', 'Education parent', 
                         'Age partner', 'Education partner', 'Duration stepfamily', 'Rel child', 'Rel c-step', 'Rel partner')
  frame_b$ID <- rownames(frame_b)
  
  frame_a <- as.data.frame(do.call(rbind, list_a))
  colnames(frame_a) <- c('M', 'Min', 'Max', 'sd')
  rownames(frame_a) <- c('Cohesion', 'Res-no step', 'Res - res', 'Asymm', 'Shared Child', 'parttime',
                         'Age child', 'Child female', 'Age parent', 'Parent female', 'Education parent', 
                         'Age partner', 'Education partner', 'Duration stepfamily', 'Rel child', 'Rel c-step', 'Rel partner')
  
  frame_a$ID <- rownames(frame_a)
  total <- merge(frame_b, frame_a, by = 'ID')
  return(total)}

total = summary_table()

#Calculate alpha for cohesion

alpha <- lapply(1:5, function(i){
  complete <- complete(imputed, action = i) 
  alpha <- cronbach.alpha(complete[c("cohes_a", "cohes_b", "cohes_c", "cohes_d")])
})
alpha <- mean(alpha[[1]]$alpha, alpha[[2]]$alpha, alpha[[3]]$alpha, alpha[[4]]$alpha, alpha[[5]]$alpha)

#store number of former households  
nformerhh <- length(unique(complete$CBSvolgnr_hh))

#Histogram of cohesion

histogram <- function(){
  merged <- merge_imputations(data, imputed)
  # 1. Create the histogram plot
  phist <- gghistogram(
    merged, x = "cohesion", bins=10, ylab = "Count", xlab = "Stepfamily cohesion")
  
  # 2. Create the density plot with y-axis on the right
  # Remove x axis elements
  pdensity <- ggdensity(
    merged, x = "cohesion", 
    alpha = 0, ylab = "Density") +
    scale_y_continuous( position = "right")  +
    theme_half_open(11, rel_small = 1) +
    rremove("x.axis")+
    rremove("xlab") +
    rremove("x.text") +
    rremove("x.ticks") +
    rremove("legend") +
    labs(title = "Histogram and density of Stepfamily Cohesion")
  
  # 3. Align the two plots and then overlay them.
  aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")
  ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

}


h <- histogram()
h


#Run regressions
options(scipen=999)
model1 <- with(imputed, lm_robust(cohesion ~ factor(combinations)
                                   + age_child + 
                                    female_child  + age_parent + female_respondent + educ_par + 
                                    age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12 , clusters = CBSvolgnr_hh))
model1 <- pool(model1)
summary(model1)
pool.r.squared(model1)
pool.r.squared(model1, adjusted = TRUE)

#Calculate effect sizes
cohensd <- lapply(1:5, function(i) {
  complete <- complete(imputed, action = i)
  SD_Y <- sd(complete$cohesion)
  model1ES <- lm_robust(cohesion ~ factor(combinations)
                        + age_child + 
                          female_child  + age_parent + female_respondent + educ_par + 
                          age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh, data = complete)
  table <- table(complete$combinations)
  control <- table[names(table) == 0] #Control group size
  t1 <- table[names(table) == 1] #treatment group 1
  t2 <- table[names(table) == 2] #treatment group 2
  esize_1 <- esc_B(model1ES$coefficients[2], SD_Y, t1, control)
  es_1 <- esize_1$es
  esize_2 <- esc_B(model1ES$coefficients[3], SD_Y, t2, control)
  es_2 <- esize_2$es
  return(c(es_1, es_2))
})

pooled_effects <- function(){
  fr <- t(data.frame(cohensd))
  m1 <- mean(fr[1:5])
  m2 <- mean(fr[6:10])
  return(c(m1, m2))
}

effectsizes <- pooled_effects()



#Model 2

model2 <- with(imputed, lm_robust(cohesion ~ factor(combinations) + sharedchild
                                  + age_child + factor(parttime) + 
                                    female_child  + age_parent + female_respondent + educ_par + 
                                    age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12 , clusters = CBSvolgnr_hh))
model2 <- pool(model2)
summary(model2)
pool.r.squared(model2)
pool.r.squared(model2, adjusted = TRUE)

#Effect sizes for stepfamily constellations
#Calculate effect sizes
cohensd <- lapply(1:5, function(i) {
  complete <- complete(imputed, action = i)
  SD_Y <- sd(complete$cohesion)
  model1ES <- lm_robust(cohesion ~ factor(combinations)
                        + age_child + factor(sharedchild) + factor(parttime) + 
                          female_child  + age_parent + female_respondent + educ_par + 
                          age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh, data = complete)
  table <- table(complete$combinations)
  control <- table[names(table) == 0] #Control group size
  t1 <- table[names(table) == 1] #treatment group 1
  t2 <- table[names(table) == 2] #treatment group 2
  esize_1 <- esc_B(model1ES$coefficients[2], SD_Y, t1, control)
  es_1 <- esize_1$es
  esize_2 <- esc_B(model1ES$coefficients[3], SD_Y, t2, control)
  es_2 <- esize_2$es
  return(c(es_1, es_2))
})

pooled_effects <- function(){
  fr <- t(data.frame(cohensd))
  m1 <- mean(fr[1:5])
  m2 <- mean(fr[6:10])
  return(c(m1, m2))
}

effectsizes <- pooled_effects()



#Effect sizes for shared child
cohensd_child <- lapply(1:5, function(i) {
  complete <- complete(imputed, action = i)
  SD_Y <- sd(complete$cohesion)
  model2ES <- lm_robust(cohesion ~ factor(combinations)
                        + age_child + factor(sharedchild) + factor(parttime) + 
                          female_child  + age_parent + female_respondent + educ_par + 
                          age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh, data = complete)
  table <- table(complete$sharedchild)
  control <- table[names(table) == 0] #Control group size
  t1 <- table[names(table) == 1] #treatment group
  esize_1 <- esc_B(model2ES$coefficients[5], SD_Y, t1, control)
  es_1 <- esize_1$es
  return(c(es_1))
})

pooled_effects_child <- function(){
  fr <- t(data.frame(cohensd_child))
  m1 <- mean(fr[1:5])
  return(c(m1))
}
effectsizes_child <- pooled_effects_child()

#Effect size parttime
cohensd_parttime <- lapply(1:5, function(i) {
  complete <- complete(imputed, action = i)
  SD_Y <- sd(complete$cohesion)
  model2ES <- lm_robust(cohesion ~ factor(combinations)
                        + age_child + factor(sharedchild) + factor(parttime) + 
                          female_child  + age_parent + female_respondent + educ_par + 
                          age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh, data = complete)
  table <- table(complete$parttime)
  control <- table[names(table) == 0] #Control group size
  t1 <- table[names(table) == 1] #treatment group
  esize_1 <- esc_B(model2ES$coefficients[6], SD_Y, t1, control)
  es_1 <- esize_1$es
  return(c(es_1))
})

pooled_effects_parttime <- function(){
  fr <- t(data.frame(cohensd_parttime))
  m1 <- mean(fr[1:5])
  return(c(m1))
}
effectsizes_parttime <- pooled_effects_parttime()

cohensd_age <- lapply(1:5, function(i) {
  complete <- complete(imputed, action = i)
  SD_Y <- sd(complete$cohesion)
  model2ES <- lm_robust(cohesion ~ factor(combinations)
                        + age_child + factor(sharedchild) + factor(parttime) + 
                          female_child  + age_parent + female_respondent + educ_par + 
                          age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh, data = complete)
  table <- table(complete$age_child)
  control <- table[names(table) == 0] #Control group size
  t1 <- table[names(table) == 1] #treatment group
  esize_1 <- esc_B(model2ES$coefficients[4], SD_Y, t1, control)
  es_1 <- esize_1$es
  return(c(es_1))
})

pooled_effects_age <- function(){
  fr <- t(data.frame(cohensd_age))
  m1 <- mean(fr[1:5])
  return(c(m1))
}
age <- pooled_effects_age()


#PLOTS
#Pool marginal effects
predictions <- lapply(1:5, function(i) {
  m <- lm_robust(cohesion ~ factor(combinations)
                 + age_child + factor(sharedchild) + factor(parttime) +
                   female_child  + age_parent + female_respondent + educ_par + 
                   age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh, data = complete(imputed, action = i))
  ggpredict(m, "combinations")
})
predictions <- pool_predictions(predictions)

#Plot
p1 <- ggplot(data=predictions, aes(x=factor(x), y=predicted)) +
  geom_bar(stat="identity", fill="white", color='black')+ 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) + 
  labs(title = "Stepfamily Cohesion, by Stepfamily Constellation", 
       x = "Stepfamily constellation", y = "Predicted stepfamily cohesion") +
  scale_x_discrete(labels = c('Resident biological child, \n no stepchild', 
                              'Resident biological child & \n resident stepchild',
                              '(Non)resident \n biological child & \n (non)resident \n stepchild')) +
  ylim(0,5) +
  theme(plot.title=element_text(hjust=0.5))
p1

#Predictions for shared child
predictions <- lapply(1:5, function(i) {
  m <- lm_robust(cohesion ~ factor(combinations)
                 + age_child + factor(sharedchild) + factor(parttime) +
                   female_child  + age_parent + female_respondent + educ_par + 
                   age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh, data = complete(imputed, action = i))
  ggpredict(m, "sharedchild")
})
predictions <- pool_predictions(predictions)



p2 <- ggplot(data=predictions, aes(x=factor(x), y=predicted)) +
  geom_bar(stat="identity", fill="white", color='black')+ 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) + 
  labs(title = 'Stepfamily Cohesion, by Shared Biological Child',
       x = "Having a shared child", y = "Predicted stepfamily cohesion") +
  scale_x_discrete(labels = c('No shared child', 
                              'Shared child')) +
  ylim(0,5) +
  theme(plot.title=element_text(hjust=0.5))
p2




















#Model 3

model3 <- with(imputed, lm_robust(cohesion ~ factor(combinations) + sharedchild + factor(parttime)
                                  + age_child + 
                                    female_child  + age_parent + female_respondent + educ_par + 
                                    age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh))
model3 <- pool(model3)
summary(model3)
pool.r.squared(model3)
pool.r.squared(model3, adjusted = TRUE)


#Model 4
model4 <- with(imputed, lm_robust(cohesion ~ factor(combinations) + sharedchild + factor(parttime)
                                  + age_child + factor(combinations):factor(parttime) +
                                    female_child  + age_parent + female_respondent + educ_par + 
                                    age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12 , clusters = CBSvolgnr_hh))
model4 <- pool(model4)
summary(model4)
pool.r.squared(model4)
pool.r.squared(model4, adjusted = TRUE)




#Tabulate models 
m1 <- summary(model2)
m2 <- summary(model3)
m3 <- summary(model4)
  #Merge
total_models <- merge(m1, m2, by='term', all = TRUE)
total_models <- merge(total_models, m3, by='term', all = TRUE)
#Remove unnecessary columns
total_models <- subset(total_models, select = -c(statistic.x, df.x, statistic.y, df.y, statistic, df) )



#Predictions from Model 3
  #Difference between constellations






#Interaction
predictions_interact <- lapply(1:5, function(i) {
  m <- lm_robust(cohesion ~ factor(combinations) + sharedchild + factor(parttime)
                 + age_child + factor(combinations):factor(parttime) +
                   female_child  + age_parent + female_respondent + educ_par + 
                   age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12 , clusters = CBSvolgnr_hh, data = complete(imputed, action = i))
  ggpredict(m, terms = c("combinations", "parttime"))
})
predictions_interact <- pool_predictions(predictions_interact)

p3 <- ggplot(data=predictions_interact, aes(x=factor(x), y=predicted, fill = factor(group))) +
  geom_bar(stat="identity", position = "dodge", color='black') + 
  #scale_fill_viridis(discrete = T) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1, position=position_dodge(.9)) + 
  labs(title = 'Stepfamily Cohesion by Part-time residence',
       x = "Stepfamily configuration", y = "Predicted stepfamily cohesion", ) +
  scale_x_discrete(labels = c('Resident biological child, \n no stepchild', 
                              'Resident biological child & \n resident stepchild',
                              '(Non)resident \n biological child & \n (non)resident \n stepchild')) +
  ylim(0,5) +
  theme(plot.title=element_text(hjust=0.5), legend.direction = "horizontal") +
  #scale_fill_discrete(name = "Having shared \n biological child", labels = c("Yes", "No")) +
  scale_fill_manual(name = "Part-time residence of either child", values = c('white', 'grey'), , labels = c("No", "Yes")) +
  theme(legend.position="top")
p3


