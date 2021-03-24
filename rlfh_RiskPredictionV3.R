################################################################
##### RISK PREDICTION - A PRACTICAL EXAMPLE USING R ############

# INTRODUCTION
#-------------
# This script was written to accompany the Risk Prediction 
# e-Learning for Health module. It may be helpful to have a copy
# of the flow diagram which is available to download from within
# the module. You will also need to download the accompanying
# BirthsData.csv. This was adapted from the MASH data availabl
# for download at https://www.sheffield.ac.uk/mash/statistics/datasets.

##########################################################################################
############################# THE SCENARIO ###############################################

# Health professionals are concerned about the high-rate of babies born with low birthweight
# and have contacted you to do some research to help understand the issue.
# The health professionals have provided you with a dataset containing data with different 
# baby measurements taken at birth, along with characteristics collected about their parents.
# (This is a hypothetical dataset constructed purely for the purpose of this module).

# GETTING STARTED
# 1) INSTALL R and RStudio
# If you haven't already done so:
# Download the latest version of RStudio - https://rstudio.com/products/rstudio/download/
# and the latest version of R - https://www.r-project.org/

# (RStudio is a front end application which makes it easier to work with R).

# Option A: CREATING THE R PROJECT (MANUALLY, WITH THE .csv AND .R)
# A1) Create a working folder on your computer or network drive
# A2) Save this script and the .csv file to the folder
# A3) Create a new R project in RStudio:
# -- File --> New Project -- > Existing directory --> navigate
# to the folder you created in step 2) above
# Now you should see your .csv file and R script in the "Files" pane in the bottom
# right corner of your RStudio.
# Now you are ready to get started. 

# Option B: OPENING THE EXISTING R PROJECT (GIT) [https://github.com/nhsx/eLfH-PHM-RiskStrat.git]
# If using Git, you can either choose to download the project manually or - if familiar with Git and R integration - clone it into R Studio:
# B) Download manually
# B1) Access the git via the url provided
# B2) Click on the green button saying 'Code'
# B3) Click 'Download ZIP'
# B4) Unzip as folder in appropriate location
# B5) In R Studio, choose File --> Open Project ... --> navigate to .Rproj file in unzipped folder
# or
# C) Clone from Git
# C1) Access the git via the url provided
# C2) Click on the green button saying 'Code' and copy the 'HTTPS' link provided
# C3) In R Studio, choose File --> New Project --> Version Control --> Git and paste the url provided. Click 'Create new project'
# More information on using Github and connecting RStudio and Git can be found in this resource: https://happygitwithr.com/rstudio-git-github.html

#############################################################################################
############# PACKAGES ######################################################################

#########################################################################################
# BUILDING THE RISK PREDICTION MODEL ####################################################
#########################################################################################

# For this module you will need to install the following packages:
# (NB: you only need to run this once. When the packages are installed they live permanently
# on your c: drive)
install.packages("PerformanceAnalytics","dplyr","InformationValue","pROC","skimr","epiDisplay","gtsummary","questionr")

# Set the required libraries (these need to be set every time you open RStudio)
library(PerformanceAnalytics)
library(dplyr)
library(InformationValue)
library(pROC)
library(skimr)
library(gtsummary)
library(questionr)
# If you are working through this script in conjunction with the flow diagram, you can 
# assume that you have already received the dataset. Therefore, we will start at the point
# "Statistical methods" --> "Receive your data" --> "Understand your data"

# (STEP 0: READ THE DATA INTO R)
# (To run the code, highlight the row of code and press "Run" or ctrl + enter)
data<-read.csv("BirthsDataset.csv")

# STEP 1: Understand your data
#-----------------------------

# First open and inspect the dataset to get a feel for it
View(data)

# take a look at the variable names - can you understand them?
names(data)

# The fields you have been provided are:
# ID --> Unique record ID
# Length of baby at birth (cm)
# Birthweight of baby at birth (kg)
# Headcirc --> head circumference at birth (cm)
# Gestation (weeks)
# Smoker
# mage --> mother's age at birth
# mnocig --> number of cigarettes smoked per day (mother)
# mheight --> mother's height (cm)
# mppwt --> mother's pre-pregnancy weight (kg)
# fage --> father's age
# fedyrs --> father's years in education
# fnocig --> number of cigarettes smoked per day (father)
# fheight --> father's height (kg)
# lowbwt

# How many records does your dataset contain?
nrow(data)
# How many variables does your dataset contain?
ncol(data)

### What types of variables are these?
### A quick way to check this is to run summary(data set nam)
summary(data)


# Another way to do this is to use the skimr package. This is really useful as it presents the summary
# statistics as well as a distribution of each of the variables:
skim(data[,-1]) # we don't need to see the information for the ID variable

# Using the results which appear in the console window we can identify the 
# descriptive statistics for each of the variables in our dataset
# Scrolling through the result set we can see that all of the variables are continuous, with
# the exception of 
# smoker --> min =0 and max = 1 --> this is a binary variable
# lowbwt --> min =0 and max = 1 --> this is a binary variable (and our outcome variable)

# Therefore, we need to tell R that these variables are categorical, or factors, otherwise
# R will treat them as continuous when we input into the model.

data$smoker=as.factor(data$smoker)
data$lowbwt=as.factor(data$lowbwt)

# After running the skim function, we can also see that the variable 
# fedyrs (fathers number of years in education) consists of 157 missing variables.
# Perhaps this variable was difficult to capture. 
# There are 157 missing values (15% of records missing). It is not practical or sensible to try and
# impute this volume of missing data.
# (Run the function again to check for yourself!)
skim(data[,-1])
# Therefore, this variable can be omitted from the dataset

data<-data%>%select(-fedyrs) 
# We now also want to check to see which variables are correlated with one another and to check for
# relationships
# (We could investigate these individually but this isn't efficient for large datasets)
# Remember, we do not want to investigate or check the ID variable. This will not be used in
# any analysis! This is in the first column of the dataset. We also know that "smoker" and "lowbwt"
# are categorical and so we can remove these from the correlation analyses.


windows();
chart.Correlation(data %>% dplyr::select(-c(ID,smoker,lowbwt)), histogram=TRUE, pch=19)

# This chart tells us so much information about the data without having to do to much analysis.
# We can see:
# - the distributions of the data
# - we can see the relationships between each of the variables and establish auto-correlation
# visually

# For example, we can see a strong correlation between "Length" and "Head circ" of 0.74.
# Therefore, we can remove one of these variables from our dataset. 
data <- data %>% select(-Headcirc)
# So we now have one less variable in our dataset.
# Let's take another look at the correlation matrix
windows();
chart.Correlation(data %>% dplyr::select(-c(ID,smoker,lowbwt)), histogram=TRUE, pch=19)

# There are no other highly correlated variables so we are ready to move on.

# STEP 2: Understand data completeness
#------------------------------------- 

# We addressed this element in the previous step. However, as a reminder we can 
# check the completeness of the data quite easily using the skim function.

skim(data)

# STEP 3: Consider your predictors
#------------------------------------- 
# Consider your predictors. Are they sensible?

# We are looking at this with independent eyes, on behalf of the health clinicians. Therefore,
# we may not have the expertise required.
# As these data have been collected with the intention of assessing predictors of low birth weight, we can include
# all variables in the model.
# Let's take another look at our variables
names(data)

# We can see that we collect 2 variables which include the same information
# -- smoker
# -- mnocig
# We can use this to check that the variables are in concordance

with(data, table(mnocig,smoker))

# The data are in perfect alignment and we can assume that these have been
# collected and coded correctly.
# However, is it really necessary to include both of these in the model when they contain the same information?
# The categorical variable assesses whether the smoking statement is true
# With the continuous variable we can assess the magnitude of effect.

# We also have a Birthweight field. Surely it wouldn't be sensible to use this as a predictor
# for if a baby has low birthweight or not (afterall this is what we are trying to establish)!
# Therefore, we can remove Birthweight as a predictor
# (We can check this as follows)

windows();
cdplot(lowbwt~Birthweight, data)

# We can see a clear relationship between birthweights categorised as low and high!

data<-data%>%
  select(-Birthweight)


# STEP 4: Transform your predictors
#------------------------------------- 

# Let's take another look at the variable relationships:
windows();
chart.Correlation(data %>% dplyr::select(-c(ID,smoker,lowbwt)), histogram=TRUE, pch=19)

# We can see that the variables mnocig and fnocig (no.of cigarettes smoked by mother and father respectively) can
# be transformed into categorical variables. Most values are 0, and we can group the smaller frequencies into categories.

#let's start with mnocig

data$mcig_cat<-cut(data$mnocig, c(0,1,11,21,100),right=FALSE)

#check these have been assigned correctly
with(data, table(mnocig,mcig_cat))

# So we can see we have 4 categories:
# - 0 --> 0 cigarettes
# - 1 --> 10 cigarettes
# - 11 --> 20 cigarettes
# - >20 cigarettes

# Now let's do the same for the number of cigarettes smoked by the father

data$fcig_cat<-cut(data$fnocig, c(0,1,11,21,100),right=FALSE)

#check these have been assigned correctly
with(data, table(fnocig,fcig_cat))

# Remember, we need to change these variables to factors otherwise
# R will treat them as continuous.

data$mcig_cat<-as.factor(data$mcig_cat)
data$fcig_cat<-as.factor(data$fcig_cat)

# Now we have transformed all of the variables as required.

# STEP 5: Is your outcome variable binary?
# ----------------------------------------

# In this example we are interested in the lowbwt variable, and identifying the risk factors
# of a baby being born with low birth weight.
# Our outcome variable is binary
# -0 --> normal birthweight
# -1--> low birthweight

with(data,table(lowbwt))

# We can see that we have 897 values of 0
# and 103 values of 1 --> i.e. 103 babies were classified as low birth weight

# STEP 6: at this point consider splitting your data.
# ---------------------------------------------------

# Now our data is ready for modelling.
# Before we go any further, we can split our data for modelling into separate train and test datasets.
# Ideally, we want to use 70%/80% of our data to build the model, and 30%/20% to test the accuracy of the model.

# These datasets should be assigned randomly in case there is any kind of order to the data.

# Now split the data randomly 80/20 for building and testing

# (As this is a relatively small dataset you may want to consider cross-validation methods see: 
# https://www.geeksforgeeks.org/cross-validation-in-r-programming/ and the 'caret' package. )
# (As this is an introductory module, we will keep this as a simple build and test example)

n<-nrow(data) # count the number of rows and assign this to the variable 'n'

# create the training dataset
# For this tutorial, we will fix a 'seed' for the randomisation so that the same sample is drawn. 
# we are then telling R to take a sample, which should be of size n*0.8.

set.seed(1999); train<-sample(1:nrow(data),n*0.8)

# set the train_data
train_dataset<-data[train,] # assign the train indices to the dataset
test_dataset<-data[setdiff(1:nrow(data),train),] # set the test data to all of the records not in the train dataset

# now we have a training dataset containing 800 records,
# and a dataset containing 200 records.

# STEP 7: WE ARE LOOKING AT A TYPICAL RISK PREDICTION MODEL
# ---------------------------------------------------------

# STEP 8: WITH THIS SCENARIO, WE COULD BE INTERESTED IN WHETHER THE 
# CONDITION IS PRESENT NOW (DIAGNOSTIC), OR THE PROBABILITY THAT A
# SPECIFIC EVENT WILL OCCUR IN THE FUTURE (PROGNOSTIC) WITH A SHORT-
# TERM OUTCOME
# -----------------------------------------------------------------

# STEP 9: THE RECOMMENDATION IS A RISK PREDICTION MODEL WITH BINARY OUTCOME 
# -------------------------------------------------------------------------

# In this example we will focus on the traditional logistic regression model 
# as it is most widely used and documented for research questions such as this 
# one, although you could equally apply methods such as Decision Trees, Random
# Forests or Support Vector Machines

# STEP 10: BUILD THE MODEL 
# -------------------------------------------------------------------------
# build with model with the train_dataset

logmod<-glm(lowbwt~Length+Gestation+ smoker+mage+mheight+mppwt+fage+fcig_cat+fheight,
            data=train_dataset, family=binomial)
# You should now see the "logmod" appear in the Global Environment in
# the "Data" window.
# We can extract the model information using the summary() function
summary(logmod)

# You should be able to see that our only significant variables (at p<0.05 level) are 
# - the Length of the baby at birth (p=0.0486)
# - the Gestation period (p=0.0445)
# And we can look at which variables on the whole (rather than underlying levels or categories) are significant with:
anova(logmod, test="Chisq")

# You should be able to see that, on the whole, all variables with the exception of:
# - mage
# - fage
# - fheight

# are statistically significant
# Your results may show some values being predicted perfectly, and this should not occur!
#########################################
# Warning message: 
# glm.fit: fitted probabilities numerically 0 or 1 occurred
#########################################

# Do not worry about warning such as this, especially when dealing with smaller datasets.

# In the module, you learnt about the different forms of variable selection:
# 1) Forward stepwise selection
# 2) Backwards stepwise selection
# 3) Bidirectional stepwise selection

# So far, we have included all of the variables in the model (Full Model).
# Now we can undertake some Backwards Stepwise variable selection to try and simplify our model.

# Let's take our Full model and remove the least significant variables

backwards<-step(logmod, direction="backward")

# run some summary statistics on the "backwards" model
summary(backwards)

# The backward stepwise regression results in an optimal model:

# Length 
# Gestation
# smoker1              
# mheight            
# fage                  
# fcig_cat[1,11)       
# fcig_cat[11,21)      
# fcig_cat[21,100)  

## We can test model differences between our original logmod model (which include all variables)
# and the backwards regression, with chi square test
anova(backwards, logmod, test="Chisq")

# With a P value = 0.6679, there is no statistical difference between the two models.

# However, lets use our "backwards" model (we have gone to the effort to build it after all
# and it has fewer variables and so is a simpler model!)

# Let's take a look at the summary information again!

summary(backwards)

# Currently, the model returns our coefficient estimates.
# However, this information is not so easy to interpret as we express
# the coefficient as "log odds"
# For example, the "mother's pre-pregnancy weight" has a coefficient of -0.174
# This would be interpreted as:

#  "The log odds of low birthweight decrease by 0.174 for each annual/unit increase in the mother's pre-pregnancy weight"

# This isn't really easy to interpret

# So we can transform the coefficients by exponentiating: 
t_backwards <- gtsummary::tbl_regression(backwards,exponentiate=TRUE)
t_backwards
# (press the zoom button in the bottom right window)
# Now you can see the fage variable has an odds ratio of 0.84

# This is much easier to interpret:

# "All things being equal, the odds of a baby being born with a low birth weight 
# decrease by 16% (1-0.84) for each kg increase in the mother's weight pre-pregnancy"

# So what does this information tell us from a risk prediction perspective?
# Well, any variable with an OR greater than 1 has an adverse effect on low birth rates.
# In our example, these are all of the indicators which relate to smoking:
# --Mothers smoking status at birth
# --Fcig_cat (however these have a p-value > 0.05 and so are not statistically significant)

#
# These are the predictions on the train dataset (which we used to build the model)
modtrain_resp<-predict(backwards, newdata=train_dataset, type="response")

# Some performance measures on the training data are given below
# Pseudo-R2:
# These Pseudo-R2 measures are however most useful when comparing /among/ models trained on the same data, for a same outcome.
DescTools::PseudoR2(backwards,which="Nagelkerke")  # Can range from 0 to 1.
# This gives a value of 0.974. These pseudo r2 values are NOT interpreted in the same manner as when performing linear regression.
# The pseudo r2 are useful when comparing different models, on the same dataset, predicting the same outcome. The higher the pseudo r2 value
# the better the model. Hence, though the value 0.974 looks promising, it has more relevant meaning when building in parallel with different models. Then, we can compare
# the pseudo r2 values of the two models.

# Now it looks like we have a model which we can apply to the test dataset.

#######################################################################################################
#######################################################################################################
# We can now take the model which we built using the train data, and test the model performance
# using the test_dataset which contains 200 records

modtest_resp<-predict(backwards, newdata=test_dataset, type="response")

# bind this onto the test dataset so we can compare the predicted values with the actual values
test_dataset<-cbind(test_dataset, modtest_resp)

# open up the test_dataset and scan the lowbt and modtest_resp fields
# have a quick scour of variables 
# - lowbwt
# - modtest_resp
# these values should be as similar to each other as possible
View(test_dataset)

# scanning the first few records it looks like some of our lowbwt and modtest_resp values are quite similar.

# Now we can prepare the data for plotting on an ROC

# plot the ROC
windows();
plot(roc(test_dataset$lowbwt,modtest_resp),print.auc=TRUE) 

# The area under the curve is 0.99, so our current model fits the data quite well
# and performs quite well when we apply the model to the training dataset.

# We can run some more tests on the model to test its accuracy.

#### Confusion Matrix at 0.5 threshold ####
# Will capture useful statistics such as sensitivity and specificity
# Train data:
DescTools::Conf(x=as.numeric(ifelse(modtrain_resp>0.5,1,0)),
                ref=train_dataset$lowbwt,
                pos=1)
# Test data:
DescTools::Conf(x=as.numeric(ifelse(modtest_resp>0.5,1,0)),
                ref=as.numeric(as.character(test_dataset$lowbwt)),
                pos=1)

#### Brier score #####
# Remember, the closer the Brier score is to 0, the better the model.
#Brier score on train data:
DescTools::BrierScore(resp=as.numeric(as.character(train_dataset$lowbwt)),pred=modtrain_resp) # will yield the same result!
#Brier score on test data:
DescTools::BrierScore(resp=as.numeric(as.character(test_dataset$lowbwt)),pred=modtest_resp)

# What score do you get?
# In the current iteration, the Brier Score is very low on the test data which implies that our model is broadly accurate 
# at predicting future events, such as predicting low birth weight.

# Therefore, our model is quite accurate at predicting outcomes based on the data we have available.
# Performing the backwards stepwise regression we found that the "smoking status of the mother" was the most significant factor
# for a baby to be born with low birth weight.

# Recall: 
t_backwards <- gtsummary::tbl_regression(backwards,exponentiate=TRUE)
t_backwards

# In fact, the OR for smoking status is 6.5, indicating that babies who are born to mothers who smoke have 6.5 times greater odds of being
# born with low birth weight (with very wide confidence intervals)!

# Going back to the original scenario, how do you think this information could be conveyed to the healthcare professionals and
# how do you think these results can be used from a risk prediction perspective?

############################################################################################################################################
# Within this script, we have worked to a brief and used the accompanying flowchart to go through a logical process to build a risk
# prediction model to establish the significant factors which result in an adverse outcome.
# This is quite similar to other scenarios which you may face as an analyst working within the NHS, or a public health setting.
##############################END##########################################################################################################