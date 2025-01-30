################################################################
#                                                              #
#   POINT OF SUBJECTIVE EQUALITY (PSE) AND SLOPE COMPUTATION   #
#               L. Geers, April 2022                           #
#                                                              #
################################################################

# This script computes for each participant and level of a condition variable the logistic regression of Y on X
# and then computes the X value for which Y = 0.5 (that is to say the point of subjective equality [PSE])

# It should provide you with
# - a csv file with the intercept (a), slope (b) and PSE for each participant and condition
# - an image containing all individual graph of Y as a function of X
# - an image containing all individual logistic regressions
# - an image with the average PSE as a function of the condition

rm(list=ls()) #clears working space


# 1. LIBRARIES
# -------------

# 1.1. List of the packages that will be needed
packages= c("readxl","tidyverse", "broom", "ggthemes", "writexl","glmnet")

# 1.2. Function to install packages
install_if_not_installed <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)}}

# 1.3. Install  and load packages if required
lapply(packages, install_if_not_installed)
lapply(packages, require, character.only= T)



# 2. DATA IMPORTATION
# -------------------

# Please, store the data in a .xlsx/.csv file in the same folder than this script, with one line per trial and a: 
# - "ID" categorical variable that indicates for each data the participant it comes from.
# - "Condition" categorical variable that indicates for each data the condition it comes from.
#    If you have several independent categorical variables (e.g., gender and color hair), please specify them all in this single variable (e.g., Female_Brown)
# - "X" continuous variable that will be introduced in the logistic model to explain the response
# - "Y" binary variable that indicates the response as 0 (e.g.,"no") or 1 ([)e.g.,"yes")

# 2.1. Get the directory name of the current script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)


# 2.2. Set the working directory to the script directory
setwd(script_dir)

# 2.3A. Import xlsx file
data <- read_xlsx(path = "AOI_by_trials.xlsx")

# 2.3B Import csv file:
#data <- read.csv("data.csv", sep = ",") 

# !! change the name so it corresponds to the name of your path and or data file
# !! change the type of separator if required 



# 3. DATA FORMATTING
# -------------------


# 3.1 Specify x upper and lower limit 
x_min <- 1
x_max <- 6

breaks <- seq(x_min, x_max, by = 1)

# !! Change xmin and xmax if needed according to your data
  

# 3.2 Transform Condition in a categorical variable 
data$Condition <- as.factor(data$Condition) 


# 3.3 Dislay head of the data
head(data)


# Should look like this:

# ID             Condition    X Y
# 1  1 Equiluminance_Neutral 2.00 1
# 2  1 Equiluminance_Neutral 3.75 0
# 3  1 Equiluminance_Neutral 3.25 1
# 4  1   Equiluminance_Small 2.00 1
# 5  1 Equiluminance_Neutral 4.50 0
# 6  1   Equiluminance_Small 3.50 1



# 4. DATA AVERAGING 
# -----------------


# 4.1 At the individual level:

data_avg_ind <- data %>% 
                   group_by(ID, Condition, X) %>%  
                   summarise(prop = mean(Y), ones = sum(Y), 
                   n = n(), zeros = n-ones) 

# calculates the probability of 1 response for each participant and condition.
# It also count the number of one (e.g., "yes") and zero (e.g., "no") responses, those are needed for later GLM logistic regression.


# 4.2 At the group level:

data_avg_all <- data %>% 
                 group_by(Condition, X) %>%  
                 summarise(prop = mean(Y), ones = sum(Y), 
                 n = n(), zeros = n-ones)

# calculates tthe probability of 1 response for each condition.
# It also count the number of one (e.g., "yes") and zero (e.g., "no") responses, those are needed for later GLM logistic regression.



# 5. DATA VISUALISATION 
# ---------------------

jpeg("ind_data.jpg", width = 1920, height = 1080) #creates images where to write

data_avg_ind %>% ggplot(aes(x = X, y = prop, color = Condition)) + 
                      geom_point() + 
                      geom_line() +
                      facet_wrap(~ID) + 
                      ylab('Prop. of "1" Responses') +
                      scale_x_continuous(limits = c(x_min, x_max), breaks = breaks)+
                      theme_bw(base_size = 20)

# represents the psychometric curves of Y according to X for each participant
# by visual inspection, you can see if each participant shows the required psychometric function in each condition

dev.off() #saves image



# 6. LOGISTIC REGRESSION ESTIMATION 
# ---------------------------------

## 6.1 At the individual level 
glm_results_ind = data_avg_ind %>% 
                          group_by(ID, Condition) %>%
                          do(tidy (glm(cbind(ones,zeros) ~ X, 
                          family = binomial(logit), data=.))) 
#computes the logistic regression of Y on X for each participant and condition


## 6.2 At the group level
glm_results_all = data_avg_all %>% 
                       group_by(Condition) %>%
                       do(tidy (glm(cbind(ones,zeros) ~ X, 
                       family = binomial(logit), data=.))) 
#computes the logistic regression of Y on X for condition



# 7. PSE COMPUTATION 
# ------------------

## 7.1 At the individual level 
pse_results = glm_results_ind %>%
                      select(., one_of(c('ID','Condition', 'term','estimate'))) %>%
                      spread(.,term, estimate) %>% rename(., b = X, a = `(Intercept)`) %>%
                      mutate(., pse = -a/b)

p_values = glm_results_ind %>%
                    select(ID, Condition, p.value) %>%
                    distinct()  

pse_results = pse_results %>%
                    left_join(p_values, by = c("ID", "Condition"))

#computes the PSE for each participant and condition from the logistic regression parameters

write.csv(pse_results, "PSE_data.csv", row.names = FALSE) #saves PSE in a csv file
write_xlsx(pse_results, "PSE_data.xlsx")

## 7.2 At the group level
pse_group_results = glm_results_all %>%
                            select(., one_of(c('Condition', 'term','estimate'))) %>%
                            spread(.,term, estimate) %>% rename(., b = X, a = `(Intercept)`) %>%
                            mutate(., pse = -a/b)



# 8. LOGISTIC REGRESSION AND PSE VISUALISATION 
# --------------------------------------------

## 8.1 At the individual level 

jpeg("ind_glmm.jpg", width = 1920, height = 1080)#creates images where to write

data_avg_ind %>% ggplot(aes(x = X, y = prop, color = Condition)) +  
                           geom_point(size=3) + 
                           geom_smooth(method = glm, method.args= list(family = binomial(logit)),se = FALSE) +
                           scale_x_continuous(limits = c(x_min, x_max), breaks = breaks)+
                           geom_hline(yintercept = 0.5, linetype='dashed', size = 1, color = "lightgrey")+
                           xlab('X') +
                           ylab('Prop. of "1" Responses') +
                           facet_wrap(~ID)+
                           theme_bw(base_size = 20)
dev.off() #saves the image


## 8.2 At the group level

jpeg("group_glmm.jpg", width = 1920, height = 1080)#creates images where to write
data_avg_all %>% ggplot(aes(x = X, y = prop, color = Condition)) +  
                           geom_point(size=3) + 
                           geom_smooth(method = glm, method.args= list(family = binomial(logit)),se = FALSE) +
                           scale_y_continuous(limits = c(0, 1), breaks = seq(0,1, by = 0.25), expand=c(0,0))+
                           ylab('Prop. of "1" Responses') +
                           scale_x_continuous(limits = c(x_min, x_max), breaks = breaks)+
                           xlab('X') +
                           geom_hline(yintercept = 0.5, linetype='dashed', size = 1, color = "lightgrey")+
                           geom_segment(aes(x = pse_group_results$pse[1] , y = 0, xend = pse_group_results$pse[1], yend = 0.5), size = 1, linetype= 2, color = '#F8766D') +
                           geom_segment(aes(x = pse_group_results$pse[2] , y = 0, xend = pse_group_results$pse[2], yend = 0.5), size = 1, linetype= 2, color = '#00BFC4') +
                           theme_bw(base_size = 22)

dev.off() #saves the image




#9. PSE VISUALISATION
# -------------------

jpeg("mean_PSE.jpg", width = 1920, height = 1080)#creates images where to write

pse_results %>% group_by(Condition) %>%
                summarise(mPSE = mean(pse), sdPSE = sd(pse)) %>% 
                ggplot(aes(x = Condition, y = mPSE, fill = Condition)) + 
                  geom_bar(stat = 'identity', width = 0.5) +
                  geom_errorbar( aes(x=Condition, ymin= mPSE, ymax = mPSE + sdPSE), width=0.2, colour="grey", alpha=0.9, size=1.3)+
                  theme_bw(base_size = 22)
  
dev.off() #saves image