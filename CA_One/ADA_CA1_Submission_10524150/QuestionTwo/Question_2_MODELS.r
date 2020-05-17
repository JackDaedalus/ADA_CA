## CA One Advanced Data Analytics : Modeule Code B8IT109
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## May 2020

## Question Two - R program performing discrete and continous probability modelling on attributes 
## in the Pima Indian Diabetes dataset

## Set Working Directory Accordingly


########################################################################################################################
# Read the Pima Indian Diabetes File and perform some initial data clean up and manipulation
diabetes <- read.csv("pima-indians-diabetes.csv")
diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
diabetes <- na.omit(diabetes) # now we omit all NA values
########################################################################################################################

# Steps - 
# 1 - Choose Data Model
# 2 - Parameter Estimation
# 3 - Prediction

########################################################################################################################
# Question 2 - (a) - Select four variables of the dataset, and propose an appropriate probability model to quantify 
# uncertainty of each variable.                    . 
########################################################################################################################

#####################################################################################################
# Discrete Variable in Pima Indian dataset - 'Outcome'
#####################################################################################################

# Take 'Outcome' - use a Bernoulli Model
head(diabetes)
table(diabetes$Outcome)
tabOut = table(diabetes$Outcome)
tabOut

# 'Outcome' could be modelled using Bernoulli (p) Probability Model. The values are binary : '0' or '1' 
# (denoting outcome of diabetic or not)


########  ----- 'Outcome' ------  ###########



#####################################################################################################
# Discrete Variable in Pima Indian dataset - SkinThickness
#####################################################################################################

# # Take 'SkinThickness' - use a Poisson Model
head(diabetes)
table(diabetes$SkinThickness)
tabSkin = table(diabetes$SkinThickness)
sort(tabSkin)

# The range of values in Skin Thickness is between 7 and 63 (this is after data clean-up).
# A Multinominal or Binomial model will not be appropriate there we will use a Poisson Model to quantify 
# SkinThickness. The number of entries for skin thickness are large and the probability of a given 
# attribute applying to a given individual is small


########  ----- SkinThickness ------  ###########



#####################################################################################################
# Continous Variable in Pima Indian dataset - BMI
#####################################################################################################

# Take BMI - Use the Normal Model.
head(diabetes)


# 1 - Use Normal Model to quantify uncertainty of the data. BMI is a continous variable and it would be  
# common practice to model the data using the Normal Model.
xBMI = diabetes$BMI 

########  ----- BMI ------  ###########



#####################################################################################################
# Continous Variable in Pima Indian dataset - DiabetesPedigreeFunction
#####################################################################################################

# Take DiabetesPedigreeFunction - Use Exponential Model 
head(diabetes)

xDBF = diabetes$DiabetesPedigreeFunction 
range(xDBF)
# 1 - Use Exponential Model to quantify uncertainty of the data because the data is continous and positive.







########################################################################################################################
# Question 2 - (b) - For each model in part (a), estimate the parameters of model.                         . 
########################################################################################################################

##   ----- 'Outcome' - Parameter - Bernoulli Model  ----- 

p_hat=tabOut[2]/sum(tabOut) # Estimation of p - relative frequency of '1'.
p_hat

########  ----- 'Outcome' ------  ###########




##   ----- Skin Thickness - Parameter - Poisson Model  ----- 

lamdaST=mean(diabetes$SkinThickness)  # Parameter
lamdaST

########  ----- SkinThickness ------  ###########




##   ----- BMI - Parameter - Normal Model  ----- 

# 2- Need to estimate two parameters :  'µ' (mu) and 'sigma'

muBMI = mean(xBMI) # Mean of BMI values gives us 'µ'

sigmaBMI = sd(xBMI) # Standard deviation of BMI values gives 'sigma'

muBMI
sigmaBMI

########  ----- BMI ------  ###########




##   ----- DiabetesPedigreeFunction - Parameter - Exponential Model  ----- 

# 2 - Paramter is 'lambda'  - 1 divided by mean of DBF values
lambdaDBF = 1/mean(xDBF)  
lambdaDBF

########  ----- DiabetesPedigreeFunction ------  ###########






########################################################################################################################
# Question 3 - (c) - Express the way in which each model can be used for the predictive analytics, then find the 
# prediction for each attribute.                                                       . 
########################################################################################################################

## ---------'Outcome' Prediction - Bernoulli Model  --------------

# Prediction using standard benchmark of 1000 generated samples
# We can use a binomial function with n = 1 for bernoulli
set.seed(3243)  # Use 'set Seed to obtain a reproducible result for this answer.
dataOut=rbinom(1000,1,p_hat)

# Count the number of '1's
# 'Outcome' Prediction - Bernoulli Model
#
sum(dataOut) 

# Sum of 1 values is <329>, out of 1000. Therefore prediction is zero since the frequency of zero is higher (<671>).
# For consistency of result for this question I used the 'set.seed(3243)' command. This ensures 
# A unique seed returns a unique random number sequence, and in this case allows the result from the prediction 
# function to be reproducible.
# 
# Without this the value for the sum of '1's would differ each time. However, it would only be a slight variation 
# and the ultimate prediction would remain zero.

########  ----- 'Outcome' ------  ###########







## --------- 'SkinThickness' Prediction - Poisson Model  ---------

# Make Prediction. Generate 1000 samples with the lambda parameter 

# The following line of code will set the seed to a random value for the seed argument each time. In practice this has little impact on the
# result of this prediction but it does allow for some randomness in the outcome - effectively 'unsetting' the 
rm(.Random.seed, envir=globalenv()) # reset seed
# sample(10)
dataST=rpois(1000, lamdaST)

# Take mean of the samples and round to get an integer value, which is in line with the nature of the DBF data in this dataset
predST=round(mean(dataST)) 
#
# SkinThickness Prediction - Poisson Model
predST

########  ----- SkinThickness ------  ###########







## --------- BMI - Prediction - Normal Model ---------

rm(.Random.seed, envir=globalenv()) # reset seed

# Simulate data from model. Generate data from model with 1000 samples. Take mean of samples
dataBMI = rnorm(1000, muBMI, sigmaBMI)
predBMI = mean(dataBMI)

# BMI Prediction - Normal Model
predBMI  # Value is close to 'µ' (mu), but there is uncertainty in any prediction. 
# Changing the amount of simulated data will alter the prediction.

########  ----- BMI ------  ###########








## --------- DiabetesPedigreeFunction - Prediction - Exponential Model ---------

rm(.Random.seed, envir=globalenv()) # reset seed

dataDBF =  rexp(1000, lambdaDBF)  # Generate 1000 samples with corresponding lambda
predDBF = mean(dataDBF) # Prediction is mean of data
# DBF Prediction - Exponential Model
predDBF  

########  ----- DiabetesPedigreeFunction ------  ###########


