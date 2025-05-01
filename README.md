# SOC-302-Final-Project
This study explores the effects of sociodemographics, work/life balance, and experiences with working mothers on gendered expectations of work, specifically measuring how people in society use their own personal experiences which create these different beliefs surrounding this issue. 


## Project:  SOC 302 Final Multivariate Project
# Located:   Class Folder on ELSA
# File Name: volk-gendered_expectations.R
# Date:      5/1/2025
# Who:       Amanda N. Volk


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages

library(dplyr)

library(psych)

### Load data 
GSS <- read.csv("GSS2022.csv")

####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary()
table(GSS$fefam)
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
# Step 3: Confirm: table() / summary()
summary(GSS$fefam)

 
############                        DEPENDENT VARIABLE                          ############
############             BETTER FOR MAN TO WORK, WOMAN TEND HOME                ############

# STEP 1: Examine variable and coding schema 
table(GSS$fefam)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, agree = ifelse(fefam <= 2, 1, 0))
GSS <- mutate(GSS, disagree = ifelse(fefam >= 3, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$fefam, GSS$agree)
table(GSS$fefam, GSS$disagree)

############                  INDEPENDENT VARIABLE                    ############
############               MOTHERS EMPLOYMENT WHEN 16                ############

# STEP 1: Examine variable and coding schema 
table(GSS$mawrkgrw)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, yes = ifelse(mawrkgrw == 1, 1, 0))
GSS <- mutate(GSS, no = ifelse(fefam == 2, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$mawrkgrw, GSS$yes)
table(GSS$mawrkgrw, GSS$no)

table(GSS$mawrkgrw, GSS$fefam)

##########################VARIABLE################################## 
########################## Gender############################################ 

# STEP 1 : examine initial variable 
table(GSS$sex)

# STEP 2 : Create dummy variables for men and women 
GSS <- mutate(GSS, man = ifelse(sex == 1, 1, 0)) 
GSS <- mutate(GSS, woman = ifelse(sex == 2, 1, 0)) 


# STEP 3 : confirm 
table(GSS$sex, GSS$man) 
table(GSS$sex, GSS$woman)


####################VARIABLE###################
################## income####################

#STEP 1 : examine initial variable 
table(GSS$finrela) 

# STEP 2 : Create dummy variables for men and women 
GSS <- mutate(GSS, below_average = ifelse(finrela == 1 | finrela == 2, 1, 0))
GSS <- mutate(GSS, average = ifelse(finrela == 3, 1, 0))
GSS <- mutate(GSS, above_average = ifelse(finrela == 4 | finrela == 5, 1, 0))


# STEP 3 : confirm 
table(GSS$finrela, GSS$below_average)
table(GSS$finrela, GSS$average)
table(GSS$finrela, GSS$above_average)


####################VARIABLE###################
##################mother's education####################

#STEP 1 : examine initial variable 
table(GSS$madeg)

#STEP 2 : Create dummy variables for 
GSS <- mutate(GSS, hs_or_less = ifelse(madeg == 0 | madeg == 1, 1, 0))
GSS <- mutate(GSS, associate = ifelse(madeg == 2, 1, 0))
GSS <- mutate(GSS, bach_or_more = ifelse(madeg == 3 | madeg == 4, 1, 0))

###tables to make sure this works##
table(GSS$madeg, GSS$hs_or_less)
table(GSS$madeg, GSS$associate)
table(GSS$madeg, GSS$bach_or_more)
##############################VARIABLE###################################
#############################interference of work outside hours####################

#STEP 1: examine initial variable
table(GSS$wkvsfam)

#STEP 2: 
GSS <- mutate(GSS, often = ifelse(wkvsfam == 1, 1, 0))
GSS <- mutate(GSS, sometimes = ifelse(wkvsfam == 2, 1, 0))
GSS <- mutate(GSS, rarely = ifelse(wkvsfam == 3, 1, 0))
GSS <- mutate(GSS, never = ifelse(wkvsfam == 4, 1, 0))

###tables to make sure this works##
table(GSS$wkvsfam, GSS$often)
table(GSS$wkvsfam, GSS$sometimes)
table(GSS$wkvsfam, GSS$rarely)
table(GSS$wkvsfam, GSS$never)
###########VARIABLE###############
###########Gender norms###########

#STEP 1: examine initial variable
table(GSS$fepresch)

#STEP 2: 
GSS <- mutate(GSS, agree_childsuffer = ifelse(fepresch <= 2, 1, 0))
GSS <- mutate(GSS, disagree_childsuffer = ifelse(fepresch >= 3, 1, 0))

###tables to make sure this works##
table(GSS$fepresch, GSS$agree_childsuffer)
table(GSS$fepresch, GSS$disagree_childsuffer)
####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("agree", "disagree", 
                "yes", "no", 
                "man", "woman", 
                "below_average", "average", "above_average", 
                "hs_or_less", "associate", "bach_or_more", 
                "often", "sometimes", "rarely", "never", 
                "agree_childsuffer", "disagree_childsuffer")

### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE
describe(my_dataset)

####################################################################################
############              PHASE 4: correlation matrix                  ############
####################################################################################
cor(my_dataset)


# TABLE 2: CONTINGENCY TABLE HERE
table(my_dataset$mawrkgrw)
table(my_dataset$fefam)
table(my_dataset$mawrkgrw, my_dataset$fefam)

chisq.test(my_dataset$mawrkgrw, my_dataset$fefam)

########################################################################################
################## PHASE 5: Logistic Regression Analysis#################################
#########################################################################################

#model 1:
model1a <- glm(agree ~ yes , data = my_dataset, family = binomial)
summary(model1a)

#model 2:
model2a <- glm(agree ~ yes +
                 often + sometimes + rarely + never ,  data = my_dataset, family = binomial)
summary(model2a)


#model 3:
model3a <- glm(agree ~ yes + 
                often + sometimes + rarely + never +
               below_average + average + above_average, data = my_dataset, family = binomial)
summary(model3a)


#model 4:
model4a <- glm(agree ~ yes +
                 often + sometimes + rarely + never +
               below_average + average + above_average + 
              man + woman, data = my_dataset, family = binomial)
summary(model4a)

#FINAL MODEL
model5a <- glm(agree ~ yes +
              often + sometimes + rarely + never +
              below_average + average + above_average + 
              man + woman + agree_childsuffer + disagree_childsuffer + 
                hs_or_less + associate + bach_or_more , data = my_dataset, family = binomial)
summary(model5a)


#odds ratio calculations
exp(1.7423)
exp(0.2888)
exp(0.2888)
exp(0.4594)
