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
library(dplyr)
options(scipen=999)
library("margins")
library(plotrix)
library(reshape2)
library(patchwork)

#Import data as csv

raw <- data.frame(read.csv('/Users/christianfang/Downloads/data_cleaned.csv'))
raw$cohesion <- (raw$cohes_a + raw$cohes_b + raw$cohes_c + raw$cohes_d) / 4

mean_before <- mean(raw$cohesion, na.rm = TRUE)

#Recode nonresident 
raw$resbiochild <- recode(raw$resbiochild, '1' = 1L, '2' = 2L, '3' = 3L, '4' = 2L)


#Make residence variables

#Stepchild residence
#raw$stepres_alone <- raw$A3P10
#raw$stepres_alone <- na_if(raw$A3P10, 98)
#raw$stepres_alone <- case_when(raw$step == 0 ~ 0, 
#                         raw$A3P10 == 1 ~ 1,
#                         raw$A3P10 == 2 ~ 2, 
#                         raw$A3P10 == 3 ~ 3,
#                         raw$A3P10 == 4 ~ 4)

#Drop respondents if: stepres == 0 & biores == 2, stepres == 2 & biores == 2
#raw$filter <- case_when(raw$stepres == 0 & raw$biores == 2 ~ 1,
#                        raw$stepres == 2 & raw$biores == 2 ~ 1,
#                        TRUE ~ 0)
#length(raw[raw$filter == 0])
#test_df <- filter(raw, filter == 0)


#Select only relevant variables

data <- dplyr::select(raw, single, LAT, 
                      CBSvolgnr_hh, cohes_a, cohes_b, cohes_c, cohes_d, cohesion, 
                      sharedchild, parttime, age_child, female_child, age_parent, 
                      female_respondent, duration, educ_par, educ_partner, 
                      age_partner, resstepchild, resbiochild, step, relqual_child, 
                      relqual_partner, relqual_child_partner)

#Exclude LAT and single people
data <- data[data$single == 0,]
data <- data[! is.na(data$single), ]
n_singles_rem <- nrow(raw) - nrow(data)
len <- nrow(data)
data <- data[data$LAT == 0, ]
data <- data[! is.na(data$LAT),]
n_lat_rem <- len - nrow(data) 

#Check which variables have missing values
sapply(data, function(x) sum(is.na(x)))

#Multiple imputations
impute_data <- function(original_data){
  imp <- mice(original_data, maxit=0)
  predictor_matrix<-imp$predictorMatrix
  predictor_matrix[,c("CBSvolgnr_hh")]=0
  method<-imp$method
  method[c("CBSvolgnr_hh")]=""
  method[c("resbiochild")]="cart"
  method[c("step")]="cart"
  method[c("cohes_a")]="cart"
  method[c("cohes_b")]="cart"
  method[c("cohes_c")]="cart"
  method[c("cohes_d")]="cart"
  method[c("sharedchild")]="cart"
  method[c("female_child")]="cart"
  method[c("duration")]="cart"
  method[c("educ_partner")]="cart"
  method[c("cohesion")]="cart"
  method[c("relqual_child")]="cart"
  method[c("relqual_partner")]="cart"
  method[c("relqual_child_partner")]="cart"
  
  imputed <- mice(original_data, m=10, maxit=50, meth= method, seed=500,  predictorMatrix = predictor_matrix)
  return(imputed)}

imputed <- impute_data(data)
#Store copy
imputed_ <- imputed

#Check imputations, seems OK
densityplot(imputed)

# Make selections
with(imputed, table(resbiochild, resstepchild))

#Remove nonres bio child and no stepchild
imputed <- filter(imputed, ! ((resbiochild == 2) & (resstepchild == 2))) # Nonres-nonres out
imputed <- filter(imputed, ! ((resbiochild == 2) & (resstepchild == 0))) # Nonres-no step out

final_n <- nrow(imputed$data)

#Recode 

with(imputed, table(resbiochild, stepres_alone))


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
  cohesion_a <- list(mean(impL$cohesion, na.rm = TRUE), min(impL$cohesion, na.rm = TRUE), 
                     max(impL$cohesion, na.rm = TRUE), sd(impL$cohesion, na.rm = TRUE))
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
alpha

#store number of former households  
nformerhh <- length(unique(complete(imputed)$CBSvolgnr_hh))
nresp <- nrow(complete(imputed))


#Bar plots of subdimensions of cohesion
cohes_values <- lapply(1:5, function(i){
  complete <- complete(imputed, action = i) 
  means <- c(mean(complete$cohes_a), mean(complete$cohes_b), mean(complete$cohes_c), mean(complete$cohes_d))
  ses <- c(std.error(complete$cohes_a), std.error(complete$cohes_b), std.error(complete$cohes_c), std.error(complete$cohes_d))
  return(c(means, ses))
})
cohes_frame <- data.frame(t(data.frame(cohes_values)))
colnames(cohes_frame) <- c("cohes_a", "cohes_b", "cohes_c", "cohes_d", "se_cohes_a", "se_cohes_b", "se_cohes_c", "se_cohes_d")
cohes_frame <- t(data.frame(colMeans(cohes_frame)))
melted1 <- melt(cohes_frame[, c("cohes_a", "cohes_b", "cohes_c", "cohes_d")], id.vars = c("cohes_a", "cohes_b", "cohes_c", "cohes_d"))
melted2 <- melt(cohes_frame[, c( "se_cohes_a", "se_cohes_b", "se_cohes_c", "se_cohes_d")], id.vars = c( "se_cohes_a", "se_cohes_b", "se_cohes_c", "se_cohes_d"))
rownames(melted2) <- c("cohes_a", "cohes_b", "cohes_c", "cohes_d")
melt <- cbind(melted1, melted2)
colnames(melt) <- c("mean", "se")

barplot <- ggplot(melt, aes(x = rownames(melt), y = mean)) +
           geom_bar(position = 'dodge', stat='identity',  fill="white", color='black') +
           geom_errorbar(aes(x=rownames(melt), ymin=mean-1.96*se, ymax=mean+1.96*se, width=.1)) +
           xlab(' ') +
           ylab('Mean') +
           ylim(0,5)+
           ggtitle('Means of Subdimensions of Stepfamily Cohesion') +
           theme(plot.title = element_text(hjust = 0.5)) +
           scale_x_discrete(labels = c('Having close \n relationships ', 
                                       'Keeping each other \n informed',
                                       'More disjoint than unit \n (reversed)', 
                                      'Very involved'))

barplot



#Histogram of cohesion overall

histogram <- function(){
  merged <- merge_imputations(data, imputed)
  # 1. Create the histogram plot
  phist <- gghistogram(
    merged, x = "cohesion", bins=10, ylab = "Count", xlab = "Stepfamily cohesion") +
    labs(title = "Histogram and density of Stepfamily Cohesion")

}

h <- histogram()
h

#histogram relationship qualities
merged <- merge_imputations(data, imputed)

child <- gghistogram(data = merged, x = 'relqual_child', bins = 10, ylab = 'Count', 
                     xlab = 'Relationship quality child', 
                     title = 'Relationship quality child') 
partner_child <- gghistogram(data = merged, x = 'relqual_child_partner', bins = 10, ylab = 'Count', 
                       xlab = 'Relationship quality partner-child',
                       title = 'Relationship quality partner-child') 
partner <- gghistogram(data = merged, x = 'relqual_partner', bins = 10, ylab = 'Count', 
                             xlab = 'Relationship quality partner',
                       title = 'Relationship quality partner') 

child + partner + partner_child

gghistogram(data = merged, x = 'cohes_a', bins = 5, ylab = 'Count', 
            xlab = 'cohes_a', 
            title = 'cohes_a') 
gghistogram(data = merged, x = 'cohes_b', bins = 5, ylab = 'Count', 
            xlab = 'cohes_b', 
            title = 'cohes_b') 
gghistogram(data = merged, x = 'cohes_c', bins = 5, ylab = 'Count', 
            xlab = 'cohes_c', 
            title = 'cohes_c') 
gghistogram(data = merged, x = 'cohes_d', bins = 5, ylab = 'Count', 
            xlab = 'cohes_d', 
            title = 'cohes_d') 




#Model 1a: Stepchild + Biochild, no qualities

m1a <- with(imputed, lm_robust(cohesion ~ factor(step) + factor(sharedchild) +
                              age_child + female_child  + age_parent + female_respondent + 
                              educ_par + age_partner + educ_partner + duration ,
                               clusters = CBSvolgnr_hh))
m1a <- pool(m1a)
m1a <- summary(m1a)
m1a
pool.r.squared(with(imputed, lm(cohesion ~ factor(step) + factor(sharedchild) +
                                  age_child + female_child  + age_parent + female_respondent + 
                                  educ_par + age_partner + educ_partner + duration)))

#Model 1b: Stepchild + biochild, controls
m1b <- with(imputed, lm_robust(cohesion ~ factor(step) + factor(sharedchild) +
                              age_child + female_child  + age_parent + female_respondent + 
                              educ_par + age_partner + educ_partner + duration + 
                              relqual_partner + relqual_child + relqual_child_partner, clusters = CBSvolgnr_hh))
m1b <- pool(m1b)
m1b <- summary(m1b)
m1b
pool.r.squared(with(imputed, lm(cohesion ~ factor(step) + factor(sharedchild) +
                                         age_child + female_child  + age_parent + female_respondent + 
                                         educ_par + age_partner + educ_partner + duration + 
                                        relqual_partner + relqual_child + relqual_child_partner)))

#Test if M1a and M1b differ statistically significantly from one another
m1a_test <- with(imputed, lm(cohesion ~ factor(step) + factor(sharedchild) +
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration))

m1b_test <- with(imputed, lm(cohesion ~ factor(step) + factor(sharedchild) +
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration + 
                               relqual_partner + relqual_child + relqual_child_partner))

cat('p-value for H0:', D1(m1b_test, m1a_test)[["result"]][4])

# Create and plot predicted values
#Pool marginal effects
predictions <- lapply(1:5, function(i) {
  m <- lm_robust(cohesion ~ factor(step) + factor(sharedchild) +
            age_child + female_child  + age_parent + female_respondent + 
            educ_par + age_partner + educ_partner + duration + 
            relqual_partner + relqual_child + relqual_child_partner, clusters = CBSvolgnr_hh, data = complete(imputed, action = i))
  ggpredict(m, "step")
})
predictions_step <- pool_predictions(predictions)

#Plot
step_yhat <- ggplot(data=predictions_step, aes(x=factor(x), y=predicted)) +
             geom_bar(stat="identity", fill="white", color='black')+ 
             geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) + 
             ylab("Predicted stepfamily cohesion") +
             xlab('')+
             ggtitle("Having a stepchild") +
             scale_x_discrete(labels = c('No', 'Yes')) +
             ylim(0,5) +
             theme(plot.title=element_text(hjust=0.5))

predictions <- lapply(1:5, function(i) {
  m <- lm_robust(cohesion ~ factor(step) + factor(sharedchild) +
                   age_child + female_child  + age_parent + female_respondent + 
                   educ_par + age_partner + educ_partner + duration + 
                   relqual_partner + relqual_child + relqual_child_partner, clusters = CBSvolgnr_hh, data = complete(imputed, action = i))
  ggpredict(m, "sharedchild")
})
predictions_concrete <- pool_predictions(predictions)

concrete_yhat <- ggplot(data=predictions_concrete, aes(x=factor(x), y=predicted)) +
                 geom_bar(stat="identity", fill="white", color='black')+ 
                 geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) + 
                 ylab("Predicted stepfamily cohesion") +
                 xlab('')+
                 ggtitle("Having a shared biological child") +
                 scale_x_discrete(labels = c('No', 'Yes')) +
                 ylim(0,5) +
                 theme(plot.title=element_text(hjust=0.5))

#Panel A and B for Figure 3

ggarrange(step_yhat, concrete_yhat, 
          labels = c('A', 'B'), 
          ncol = 2, 
          nrow = 1)




#Test for significant differences
m1b_interact_test <- with(imputed, lm(cohesion ~ factor(step)*duration + factor(sharedchild)*duration +
                                          age_child + female_child  + age_parent + female_respondent + 
                                          educ_par + age_partner + educ_partner + duration + 
                                        relqual_partner + relqual_child + relqual_child_partner))

cat('p-value for H0:', D1(m1b_interact_test, m1b_test)[["result"]][4])


#Model 2a: residence, no qualities
m2a <- with(imputed, lm_robust(cohesion ~ factor(resbiochild_nonres_recode) + factor(resstepchild) + factor(sharedchild) + 
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration,
                               clusters = CBSvolgnr_hh))
m2a <- pool(m2a)
m2a <- summary(m2a)
m2a

pool.r.squared(with(imputed, lm(cohesion ~ factor(resbiochild_nonres_recode) + factor(resstepchild) + factor(sharedchild) + 
                                  age_child + female_child  + age_parent + female_respondent + 
                                  educ_par + age_partner + educ_partner + duration)))




#Model 2b: residence, controls
m2b <- with(imputed, lm_robust(cohesion ~ factor(resbiochild) + factor(resstepchild) + factor(sharedchild) +
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration + 
                                 relqual_child + relqual_child_partner + relqual_partner, clusters = CBSvolgnr_hh))
m2b_sum <- pool(m2b)
m2b_sum <- summary(m2b_sum)
m2b_sum

pool.r.squared(with(imputed, lm(cohesion ~ factor(resbiochild) + factor(resstepchild) + factor(sharedchild) +
                                  age_child + female_child  + age_parent + female_respondent + 
                                  educ_par + age_partner + educ_partner + duration + 
                                  relqual_child + relqual_child_partner + relqual_partner)))

predictions <- lapply(1:5, function(i) {
  m <- lm_robust(cohesion ~ factor(resbiochild) + factor(resstepchild) + factor(sharedchild) +
                   age_child + female_child  + age_parent + female_respondent + 
                   educ_par + age_partner + educ_partner + duration + 
                   relqual_child + relqual_child_partner + relqual_partner, clusters = CBSvolgnr_hh, data = complete(imputed, action = i))
  ggpredict(m, "resstepchild")
})
predictions_stepresidence <- pool_predictions(predictions)

stepresidence_yhat <- ggplot(data=predictions_stepresidence, aes(x=factor(x), y=predicted)) +
  geom_bar(stat="identity", fill="white", color='black')+ 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) + 
  ylab("Predicted stepfamily cohesion") +
  xlab('')+
  ggtitle("Presence and residence of stepchild") +
  scale_x_discrete(labels = c('No stepchild', 'Residential \nstepchild',
                              'Nonresidential \nstepchild', 'Part-time residential \nstepchild')) +
  ylim(0,5) +
  theme(plot.title=element_text(hjust=0.5))

predictions <- lapply(1:5, function(i) {
  m <- lm_robust(cohesion ~ factor(resbiochild) + factor(resstepchild) + factor(sharedchild) +
                   age_child + female_child  + age_parent + female_respondent + 
                   educ_par + age_partner + educ_partner + duration + 
                   relqual_child + relqual_child_partner + relqual_partner, clusters = CBSvolgnr_hh, data = complete(imputed, action = i))
  ggpredict(m, "resbiochild")
})
predictions_resbiochild <- pool_predictions(predictions)


resbiochild_yhat <- ggplot(data=predictions_resbiochild, aes(x=factor(x), y=predicted)) +
  geom_bar(stat="identity", fill="white", color='black')+ 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) + 
  ylab("Predicted stepfamily cohesion") +
  xlab('')+
  ggtitle("Residence of focal child") +
  scale_x_discrete(labels = c('Residential \nchild',
                              'Nonresidential \nchild', 
                              'Part-time residential \nchild')) +
  ylim(0,5) +
  theme(plot.title=element_text(hjust=0.5))

#Panel C and D for Figure 3

ggarrange(resbiochild_yhat, stepresidence_yhat, 
          labels = c('C', 'D'), 
          ncol = 2, 
          nrow = 1)





#Test if there is an interaction with duration
m2b_interact <- with(imputed, lm_robust(cohesion ~ factor(resbiochild)*duration + factor(stepres_alone)*duration + factor(sharedchild) +
                                          age_child + female_child  + age_parent + female_respondent + 
                                          educ_par + age_partner + educ_partner + duration + 
                                          relqual_child + relqual_child_partner + relqual_partner, clusters = CBSvolgnr_hh))
cat('p-value for H0:', D1(m2b_interact, m2b)[["result"]][4])



#TEST IF THERE IS AN INTERACTION EFFECT ABOVE AND BEYOND RESIDENCE PER SE
residence_nullmodel <- with(imputed, lm(cohesion ~ relevel(factor(resbiochild), ref = '2') + relevel(factor(stepres_alone), ref = '2')+ factor(sharedchild) +
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration 
                                 + 
                                 relqual_child + relqual_child_partner + relqual_partner
                                ))
summary(pool(residence_nullmodel))
#Calculate interaction term in such a way that there are no NA terms in the model
full.impdata <- complete(imputed, 'long', include = TRUE)
full.impdata$combinations <- case_when(
                                      (full.impdata$resstepchild == 0 & full.impdata$resbiochild == 1) ~ 1, #no-res
                                      (full.impdata$resstepchild == 1 & full.impdata$resbiochild == 1) ~ 2, #resres
                                      (full.impdata$resstepchild == 1 & full.impdata$resbiochild == 2) ~ 3, #res-nonres
                                      (full.impdata$resstepchild == 2 & full.impdata$resbiochild == 1) ~ 3, #nonres-res
                                      (full.impdata$resstepchild == 3 & full.impdata$resbiochild == 3) ~ 4, #pt-pt
                                      (full.impdata$resstepchild == 3 & full.impdata$resbiochild == 1) ~ 2, #pt-res
                                      (full.impdata$resstepchild == 2 & full.impdata$resbiochild == 3) ~ 2, #res-pt
                                      (full.impdata$resstepchild == 3 & full.impdata$resbiochild == 2) ~ 2, #pt-res
    
                                      )
new_imp <- as.mids(full.impdata)


residence_interactionmodel <- with(imputed, lm(cohesion ~ factor(resbiochild)*factor(stepres_alone)  + factor(sharedchild) +
                                                 age_child + female_child  + age_parent + female_respondent + 
                                                 educ_par + age_partner + educ_partner + duration 
                                               + relqual_child + relqual_child_partner + relqual_partner
                                                 ))

likelihood_test <- D1(residence_interactionmodel, residence_nullmodel)
likelihood_test

#test for each model separately
f_test <- lapply(1:5, function(i){
  complete <- complete(new_imp, action = i)
  lm1 <- lm(cohesion ~ factor(combinations) + factor(resstepchild) + factor(resbiochild_nonres_recode) + factor(sharedchild) +
              age_child + female_child  + age_parent + female_respondent + 
              educ_par + age_partner + educ_partner + duration + 
              relqual_child + relqual_child_partner + relqual_partner, data = complete)
  lm0 <- lm(cohesion ~ factor(resbiochild_nonres_recode)+ factor(resstepchild) + factor(sharedchild) +
              age_child + female_child  + age_parent + female_respondent + 
              educ_par + age_partner + educ_partner + duration + 
              relqual_child + relqual_child_partner + relqual_partner, data = complete)
  anova <- anova(lm1, lm0)
  return(anova)
})

p_val_anova <-
  (f_test[[1]][["Pr(>F)"]][2] + f_test[[2]][["Pr(>F)"]][2] + f_test[[3]][["Pr(>F)"]][2] +
                   f_test[[4]][["Pr(>F)"]][2] + f_test[[5]][["Pr(>F)"]][2])/5

f_anova <- (f_test[[1]][["F"]][2] + f_test[[2]][["F"]][2] + f_test[[3]][["F"]][2] +
                f_test[[4]][["F"]][2] + f_test[[5]][["F"]][2])/5
  
df_anova <- (f_test[[1]][["F"]][2] + f_test[[2]][["F"]][2] + f_test[[3]][["F"]][2] +
               f_test[[4]][["F"]][2] + f_test[[5]][["F"]][2])/5

df <- f_test[[1]][["Df"]][2]

cat('df: ', df, 'F: ', f_anova, 'Pr(>F): ', p_val_anova)

#Model 3a
imputed_combinations <- 


#Model 3b
m3b <- with(new_imp, lm_robust(cohesion ~ factor(combinations) +
                               factor(sharedchild) +
                               age_child + female_child  + age_parent + 
                               female_respondent + educ_par + age_partner + 
                               educ_partner + duration + relqual_child + 
                               relqual_child_partner + relqual_partner, 
                               clusters = CBSvolgnr_hh))
m3b <- pool(m3b)
m3b <- summary(m3b)
m3b

pool.r.squared(with(new_imp, lm(cohesion ~ factor(combinations) +
                                  factor(sharedchild) +
                                  age_child + female_child  + age_parent + 
                                  female_respondent + educ_par + age_partner + 
                                  educ_partner + duration + relqual_child + 
                                  relqual_child_partner + relqual_partner, )))

D1(m2b, m3b)



#plot the differences

predictions <- lapply(1:5, function(i) {
  m <- lm_robust(cohesion ~ factor(combinations) + factor(sharedchild) +
                   age_child + female_child  + age_parent + female_respondent + 
                   educ_par + age_partner + educ_partner + duration + 
                   relqual_child + relqual_child_partner + relqual_partner, 
                 clusters = CBSvolgnr_hh, data = complete(new_imp, action = i))
  ggpredict(m, "combinations")
})
predictions_combinations <- pool_predictions(predictions)
combinations_yhat <- ggplot(data=predictions_combinations, aes(x=factor(x), y=predicted)) +
  geom_bar(stat="identity", fill="white", color='black')+ 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) + 
  ylab("Predicted stepfamily cohesion") +
  xlab('')+
  ggtitle("Presence and residence of stepchild") +
  scale_x_discrete(labels = c('No stepchild, \nresidential \nfocal child', 
                              'Both (part-time) \nresident',
                              'One (part-time) \nand the other \nnonresident',
                              'Both part-time')) +
  ylim(0,5) +
  theme(plot.title=element_text(hjust=0.5))

combinations_yhat

ggarrange(combinations_yhat, 
          labels = c('E'))

comp <- complete(new_imp, action = 1)

lm1 <- lm(cohesion ~ factor(resstepchild)*factor(resbiochild_nonres_recode) + factor(resstepchild) + factor(resbiochild_nonres_recode) + factor(sharedchild) +
            age_child + female_child  + age_parent + female_respondent + 
            educ_par + age_partner + educ_partner + duration + 
            relqual_child + relqual_child_partner + relqual_partner, data = comp)

plot.interact <- qplot(x = resbiochild_nonres_recode, y = cohesion, color = factor(resstepchild), data = comp, geom=c("point", "jitter")) + stat_smooth(method = "lm", se = TRUE, fullrange = TRUE) + theme(legend.position="top")
plot.main <- qplot(x = resstepchild, y = cohesion, data = comp, geom=c("point", "jitter")) + stat_smooth(method = "lm", se = TRUE, fullrange = TRUE)
grid.arrange(plot.interact, plot.main, ncol = 2)




plot.interact

library(emmeans)
emcatcat <- emmeans(lm1, ~ resbiochild_nonres_recode*resstepchild)
emcatcat <- data.frame(emcatcat)
contrast(emcatcat, "revpairwise", by="resbiochild_nonres_recode", adjust="sidak")

colors <- c("No" = "red", "Resident" = "blue", "Nonresident" = "darkgreen", "P.t. resident" = "orange")

pd <- position_dodge(0.1) # move them .05 to the left and right
ggplot(data = emcatcat, aes(x = factor(resbiochild_nonres_recode), y = emmean, color=factor(resstepchild)
                            )) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.1, position = pd) +
  ylim(1,5) +
  scale_x_discrete(labels = c('Resident', 'Non resident', 'Part-time resident'))+
  xlab('Residence focal child') +
  ylab('Predicted value cohesion') +
  scale_color_manual(name = "Presence/Residence Stepchild", labels = c("No", "Resident", 'Nonresident', 'P.t. resident'), 
                    values = c('red', 'black', 'blue', 'green'))
  
  

emmip(lm1, resbiochild_nonres_recode ~ resstepchild,CIs=TRUE, style = 'factor', type = "response")



#Model 3a: combinations, no controls
m3a <- with(imputed, lm_robust(cohesion ~ factor(combinations) +
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration, clusters = CBSvolgnr_hh))
m3a <- pool(m3a)
m3a <- summary(m3a)
pool.r.squared(with(imputed, lm(cohesion ~ factor(combinations)+
                                  age_child + female_child  + age_parent + female_respondent + 
                                  educ_par + age_partner + educ_partner + duration)))


#Model 3b: combinations,  controls
m3b <- with(imputed, lm_robust(cohesion ~ factor(combinations)+
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration + 
                                 A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh))
m3b <- pool(m3b)
m3b <- summary(m3b)
pool.r.squared(with(imputed, lm(cohesion ~ factor(combinations)+
                                         age_child + female_child  + age_parent + female_respondent + 
                                         educ_par + age_partner + educ_partner + duration + 
                                         A3J02 + A3J29 + A3P12)))
 

m2b <- with(imputed, lm(cohesion ~ factor(biores) + factor(stepres)+
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration + 
                                 A3J02 + A3J29 + A3P12))
m3b <- with(imputed, lm(cohesion ~ factor(combinations)+
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration + 
                                 A3J02 + A3J29 + A3P12))
d3 <- D3(m3b, m2b)
summary(d3)

#interaction without rank deficiency
table <- with(imputed, table(factor(resstepchild), factor(resbiochild_nonres_recode)))
table <- round((table[["analyses"]][[1]] + table[["analyses"]][[2]] + table[["analyses"]][[3]] +
              table[["analyses"]][[4]] + table[["analyses"]][[5]]) / 5, 0)
table

interact <- with(imputed, lm(cohesion ~ factor(resstepchild) + factor(resbiochild) + factor(resstepchild)*factor(resbiochild)))
summary(pool(interact))

#TEST OF INTERACTIONS RESIDENCE
m_res_interactions <- with(imputed, lm_robust(cohesion ~ relevel(factor(biores), ref = '2'):relevel(factor(stepres), ref = '2')+
                                 age_child + female_child  + age_parent + female_respondent + 
                                 educ_par + age_partner + educ_partner + duration + 
                                 A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh))
m_res_interactions <- pool(m_res_interactions)
m_res_interactions <- summary (m_res_interactions)









#Run regressions
model1 <- with(imputed, lm_robust(cohesion ~ factor(stepres) + factor(biores)
                                   + age_child + 
                                    female_child  + age_parent + female_respondent + educ_par + 
                                    age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12 , clusters = CBSvolgnr_hh))

model1 <- pool(model1)
summary(model1)
plot(model1)
pool.r.squared(model1)
pool.r.squared(model1, adjusted = TRUE)

#

#Check assumptions
model1_assumptions <- lapply(1:5, function(i){
  dat <- complete(imputed, action = i)
  model1 <- lm(cohesion ~ factor(combinations)
                     + age_child + 
                       female_child  + age_parent + female_respondent + educ_par + 
                       age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12,
                     data = dat)
  residuals <- model1$residuals
  fitted <- model1$fitted.values
  df <- data.frame(residuals, fitted)
  return(df)
})

assumptions_model1 <- (model1_assumptions[[1]]+model1_assumptions[[2]]+model1_assumptions[[3]]+model1_assumptions[[4]]+model1_assumptions[[5]])/5

ggplot(assumptions_model1, aes(x=fitted, y=residuals))+
  geom_point()+
  geom_smooth(method=loess, se=FALSE)

#Examine predicted values




normality <- data.frame(model1_assumptions)

dat <- complete(imputed, action = 1)
model1 <- lm(cohesion ~ factor(combinations)
             + age_child + 
               female_child  + age_parent + female_respondent + educ_par + 
               age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12,
             data = dat)

plot(residuals, fitted)

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
  m <- lm_robust(cohesion ~ factor(stepres) + factor(biores)
                 + age_child + factor(sharedchild) +
                   female_child  + age_parent + female_respondent + educ_par + 
                   age_partner + educ_partner + duration + A3J02 + A3J29 + A3P12, clusters = CBSvolgnr_hh, data = complete(imputed, action = i))
  ggpredict(m, "biores")
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

model3 <- with(imputed, lm_robust(cohesion ~ factor(stepres) + factor(biores)
                                  + age_child + factor(sharedchild) +
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


