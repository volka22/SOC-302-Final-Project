# SOC-302-Final-Project
This study explores the effects of sociodemographics, work/life balance, and experiences with working mothers on gendered expectations of work, specifically measuring how people in society use their own personal experiences which create these different beliefs surrounding this issue. 


Data
The data that is used in this section was gathered from the General Social Survey (GSS). Respondents are aged 18+ and selected through probability-based sampling, which ensures diversity within the sample and only focusing on the US Population alone. In 2022, the GSS total population that was studied was 4,159. In my dataset, the sample size was 2,588. The major discrepancies between the GSS total population and my total number of observations can primarily be explained by answer choices like “Inapplicable”, “No Answer”, “Skipped on Web” or “Cannot Choose” as a response to the survey. For instance, if a mother does not have children, they would not be able to answer if their mother worked during their childhood. Also, if people completely skipped this survey, their thoughts would obviously not be taken into the final analysis.

Variables Used

The data that is used in this section was gathered from the General Social Survey (GSS). My independent variable measures how many mothers worked when their child was growing up. My dependent variable measures how many people agree/disagree if men should work, and womentend home. Both samples were collected in 2022, through probability-based sampling ensuring diversity within the sample and only focusing on the US Population alone. The GSS total population that was studied was 4,159. In “my_dataset”, the sample size was 2,588. The major discrepancies between these two numbers are people not feeling strongly about either side or skipping this survey completely. 

To get a sense of how people in society feel about gendered expectations in the workplace, I conceptualized this variable by measuring beliefs on if women should stay home and operationalized it by looking at respondents’ opinions on women working or tending to the home. Within this specific research study, my dependent variable and the question respondents were asked were: “It is much better for everyone involved if the man is the achiever outside the home and the woman takes care of the home and family?” They could then choose the following answers: Strongly Agree, Agree, Disagree or Strongly Disagree. I recoded these answers into agree and disagree to get more concise data. By researching the dependent variable, it gives an overall consensus in today’s world, what people’s gendered expectations of work look like. 

By looking at the independent variable, this specifically researched how many mothers worked during their child’s younger years. I conceptualized this variable by examining the answers of the respondents and operationalized this variable by looking at the data on whether or not they had a working mother or stay-at-home mom. The specific question asked by GSS within the survey was: “Did your mother ever work for pay for as long as a year, while you were growing up?” Respondents could either respond with Yes or No to this question. 

Another independent variable that was researched looked at respondents’ sex. I conceptualized this variable by examining the answers of the respondents and operationalized this variable by looking at the data on whether they were male or female. The GSS survey asked: “Respondents sex?”. Respondents could either respond with Male or Female to this question. 

Another independent variable that was researched was educational attainment of mothers. I conceptualized this variable by examining the answers of the respondents and operationalized this variable by looking at the data on how high of an education mothers had. The GSS survey asked: “Respondents mothers' degree?”. Respondents could either respond with Less than high school, High school, Associates/junior college, Bachelor’s or Graduate to this question. I recoded and made new codes which were H.S degree or less, Associates and bachelor's or more. This would make the answers more concise. 

Another independent variable that was researched was family income. I conceptualized this variable by examining the answers of the respondents and operationalized this variable by looking at the data on where families believe they fall compared to other American families for their income. The GSS survey asked: “Compared with American families in general, would you say your family income is far below average, below average, average, above average, or far above average?”. Respondents could either respond with far below average, below average, average, above average and far above average. I recoded and made new codes which were above average, average and below average. This would make the answers more concise. 

Another independent variable that was researched was work/life balance. I conceptualized this variable by examining the answers of the respondents and operationalized this variable by looking at the data on how much respondent’s work interfered with their personal life. The GSS survey asked: “How often do the demands of your job interfere with your family life?”. Respondents could either respond often, sometimes, rarely or never. 

Another independent variable that was researched was opinions on working mothers. I conceptualized this variable by examining the answers of the respondents and operationalized this variable by looking at the data where respondents thought that having a working mother would harm the child. The GSS survey asked: “A preschool child is likely to suffer if his or her mother works...”. Respondents could either respond with strongly agree, agree, disagree or strongly disagree. I recoded and made new codes which were agree-suffer or disagree-suffer. This would make the answers more concise. 




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
