## CA One Advanced Data Analytics : Modeule Code B8IT109
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## May 2020


## Question Three - GLM modelling of the Pima Indian Diabetes dataset

library(caTools) # useful to split data to training and test datasets

## Set Working Directory Accordingly


####################################################################################################
# Read the Pima Indian Diabetes File and perform some initial data clean up and manipulation
diabetes <- read.csv("pima-indians-diabetes.csv")
diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
diabetes <- na.omit(diabetes) # now we omit all NA values
####################################################################################################


########################################################################################################################
# Question 3 - (a) - Suggest an appropriate GLM to model ouput to input variables. 
########################################################################################################################
dataset=diabetes 
head(dataset) 

# The output variable is OUTCOME. As this is a binary outcome (0,1) - representing those subjects diagnosed with diabetes
# and those not diabetic - the best GLM is therefore Logistic Regression.

# We can see the range of binary values in the Outcome attribute in the Pima Indian dataset (after data clean-up)
table(dataset$Outcome) 

# The other attributes in the Pima Indian dataset are the input variables.



########################################################################################################################
# Question 3 - (b) - Split the dataset into 80% as a trainset and 20% testset, then model the trainset by fitting your 
# proposed GLM. 
########################################################################################################################

# For this section of the question use 'set.seed()' to ensure consistency of results
set.seed(3423)  # The purpose of this is to ensure consistency in the initial prediction
# The seed will be 're-set' to ensure randomness is re-introduced into the Monte Carlo runs below


# Split diabetes dataset in 80/20 ratio
sample = sample.split(dataset$Outcome, SplitRatio=0.80)
trainset = subset(dataset, sample==TRUE)
testset = subset(dataset, sample==FALSE)


# Display number of rows are splitting the data
nrow(dataset)
nrow(trainset)
nrow(testset)


# Run this command again to look at baseline model
table(dataset$Outcome) 
# Basline accuracy
BaseAccuracy <- round(262/nrow(dataset),2)
BaseAccuracy  # This comes out as 0.67, therefore we would not select a model whose accuracy is lower than this figure.


# Model the trainset by fitting the Logistic Regression GLM - all input (independent) variables are used
# The 'family' variable is set to 'binomial' because as we are using Logistic Regression
fit=glm(Outcome~., data=trainset, family='binomial') 
summary(fit) 


########################################################################################################################
# Question 3 - (c) - Specify the significant variables on the output variable at the level of 𝛼=0.0# and explore the
# related hypotheses test.
# Estimate the parameters of your model
########################################################################################################################

# Running the above lines of R code for Question 1(b) to fit the model displays the 
# estimation of parameters, Std Error, P Value etc. 

# 'Screen shot'..of 'Coefficients:'..

# Coefficients:               Estimate  Std. Error  z value   Pr(>|z|)    
#  (Intercept)              -9.4093094  1.3223976  -7.115     1.12e-12 ***
#  Pregnancies               0.1261245  0.0619177   2.037     0.0417 *  
#  Glucose                   0.0395147  0.0067703   5.836     5.33e-09 ***
#  BloodPressure            -0.0041467  0.0129406  -0.320     0.7486    
#  SkinThickness             0.0120509  0.0194255   0.620     0.5350    
#  Insulin                  -0.0008108  0.0015216  -0.533     0.5941    
#  BMI                       0.0723295  0.0301985   2.395     0.0166 *  
#  DiabetesPedigreeFunction  1.0021043  0.4654769   2.153     0.0313 *  
#  Age                       0.0098559  0.0191826   0.514     0.6074    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# The significant variables at the level of 𝛼=0.05 are; (we used a 'seed' setting to ensure consistent results)
#
# Pregnancies
# Glucose
# BMI
# DiabetesPedigreeFunction
# 
# These variables have a 'P' value less than 0.05.
#

# The estimated parameters for the significant variables are; (rounded slightly)
#
# Intercept   : -9.4
# Pregnancies :  0.126
# Glucose     :  0.04
# BMI         :  0.072
# DiabetesPedigreeFunction  : 1.002




########################################################################################################################
# Question 3 - (d) - Predict the output of the test dataset using the trained model.
# Provide the functional form of the optimal predictive model
########################################################################################################################

pred=predict(fit, testset, type='response') 
pred

# Provide the functional form of the optimal predictive model
# ...
# Also - See WORD document included in CA Submission

# p̂i = 1/1+e-t̂i

# ŷ = {10    p̂I ≥ 0.5, p̂I < 0.5

# t̂i = -9.4 + 0.126(Pregnancies) + 0.04(Glucose) + 0.072(BMI) + 1.002(DBF) 
  



########################################################################################################################
# Question 3 - (e) - Propose the appropriate measure of performance to evaluate the model and compute it for your derived model. 
########################################################################################################################

# As this is a Classification task the measure of performance is - Accuracy

# Monte Carlo Runs will be used to compare a large volume of model evaluations and get the average results.

mc=1000 # Set number of Monte Carlo Runs. A larger value of runs should generate a better rating of the accuracy of the model

acc=0 # Initialise the accuracy variable

rm(.Random.seed, envir=globalenv()) # 'Reset the 'seed' value and re-introduce randomness

# Create a new dataset based on the original Pima Indian dataset but only including the 'siginificant' variables, 
# along with the 'Outcome' target variable. This is based on the output from answer 3(c).
dataset1 =data.frame(dataset$Pregnancies, dataset$Glucose, dataset$BMI, dataset$DiabetesPedigreeFunction, dataset$Outcome)

# Set up FOR LOOP to execute the required number of model evaluations
for (i in 1:mc) { # Execute multiple runs, present average accuracy result at the end 
  
  # The 'dataset' references below are column identifiers within the 'dataset1' data frame
  # Split reduced diabetes dataset - with only significant variables - again in 80/20 ratio
  sample1   = sample.split(dataset1$dataset.Outcome, SplitRatio=0.80)
  trainset1 = subset(dataset1, sample1==TRUE)
  testset1  = subset(dataset1, sample1==FALSE)
  

  # Fit model using only the significant variables
  fit1=glm(trainset1$dataset.Outcome~., data=trainset1, family='binomial')
  
  # Prediction based on model usingg testset of data
  predres=predict(fit1, testset1, type='response') 
  
  # convert phat to yhat  
  # Set up so that predicted results from the model are compared against the actual values
  # in the testset data
  predictedvalues=rep(0,nrow(testset1)) 
  # Assess the number of matching 'Outcomes' in the testset against the output of the model
  predictedvalues[predres>0.5]=1   # probability of Outcome being 1, if p<0.5 then Outcome=0 
  
  # Get average of correct Outcome results
  # 'Outcome' is colum five in the dataset
  accuracy=mean(predictedvalues==testset1[,5]) 
  
  acc=acc+(1/mc)*accuracy 
  
} 

acc # Print final accuracy after all Monte Carlo runs completed
