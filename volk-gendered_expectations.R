
## Project:  SOC 302 Final Multivariate Project
# Located:   Class Folder on ELSA
# File Name: volk-gendered_expectations.R
# Date:      3/31/2025
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
GSS <- mutate(GSS, stronglyagree_wssh = ifelse(fefam == 1, 1, 0))
GSS <- mutate(GSS, agree_wssh = ifelse(fefam == 2, 1, 0))
GSS <- mutate(GSS, disagree_wssh = ifelse(fefam == 3, 1, 0))
GSS <- mutate(GSS, stronglydisagree_wssh = ifelse(fefam == 4, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$fefam, GSS$stronglyagree_wssh)
table(GSS$fefam, GSS$agree_wssh)
table(GSS$fefam, GSS$disagree_wssh)
table(GSS$fefam, GSS$stronglydisagree_wssh)

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
################## Gender capital####################

#STEP 1 : examine initial variable 
table(GSS$hlthacc3) 

#STEP 2 : Create dummy variables for men and women 
GSS <- mutate(GSS, much_easier = ifelse(hlthacc3 == 1, 1, 0))  
GSS <- mutate(GSS, somewhat_easier = ifelse(hlthacc3 == 2, 1, 0)) 
GSS <- mutate(GSS, about_the_same = ifelse(hlthacc3 == 3, 1, 0))  
GSS <- mutate(GSS, somewhat_harder = ifelse(hlthacc3 == 4, 1, 0))  
GSS <- mutate(GSS, much_harder = ifelse(hlthacc3 == 5, 1, 0)) 

#STEP 3 : confirm 
table(GSS$hlthacc3, GSS$much_easier)  
table(GSS$hlthacc3, GSS$somewhat_easier)  
table(GSS$hlthacc3, GSS$about_the_same)  
table(GSS$hlthacc3, GSS$somewhat_harder)  
table(GSS$hlthacc3, GSS$much_harder)


####################VARIABLE###################
################## income####################

#STEP 1 : examine initial variable 
table(GSS$finrela) 

#STEP 2 : no recoding bc ordinal


####################VARIABLE###################
##################masculinized jobs####################

#STEP 1 : examine initial variable 
table(GSS$mascself)

#STEP 2 : no recoding bc ordinal

##############################VARIABLE###################################
#############################Traditional family style####################

#STEP 1: examine initial variable
table(GSS$wkvsfam)

#STEP 2: no recoding bc ordinal

####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("fefam", "stronglyagree_wssh", "agree_wssh", "disagree_wssh", "stronglydisagree_wssh",
"mawrkgrw", "yes", "no", "sex", "man", "woman", "hlthacc3", "much_easier", "about_the_same", "somewhat_harder", "much_harder", "finrela", "mascself",   )
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
############              PHASE 4: Contingency Table + Chi2                  ############
####################################################################################
# TABLE 2: CONTINGENCY TABLE HERE
table(my_dataset$mawrkgrw)
table(my_dataset$fefam)
table(my_dataset$mawrkgrw, my_dataset$fefam)

chisq.test(my_dataset$mawrkgrw, my_dataset$fefam)