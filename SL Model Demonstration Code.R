
## ----------
## Robert Schell
## Demonstration of SL Model
## R Version 3.6.2
## 8/31/23
## ----------


# Loading in libraries
library(glmnet)
library(SuperLearner)


# Split training dataset into outcomes vs predictors
xtrain <- TrainingSet[,!colnames(TrainingSet) %in% "all_drug_overdose_count"]
ytrain<- TrainingSet[,"all_drug_overdose_count"]


# ------------
# Creating Super Learner candidate library
# ------------

# Creating range of alpha values for EN regression
alpha1 <- c(0.0001, 0.00025, 0.0005, 0.00075)
alpha2 <- 10*alpha1
alpha3 <- 10*alpha2
alpha4 <- 10*alpha3

a <- c(alpha1, alpha2, alpha3, alpha4)


# Number of lambda values to test (controls strength of penalty in EN)
nlamb <- c(100,200,300,400)


# Create empty list object to store algorithms
LibList <- list()

# ---
# Screening algorithm
# ---

ScreenLearners <- create.Learner("screen.glmnet", tune = list(alpha = 0.04, nlambda = 400))

# ---
# Prediction algorithms
# ---

# Specifying Elastic Net Learners
ElastNetLearners <- create.Learner("SL.glmnet", tune = list(alpha = a, nlambda = nlamb))

# Specifying Random Forest Learners and hyperparameters
RFLearners <- create.Learner("SL.randomForest", tune = list(ntree = c(1000,5000), nodesize = c(5,10,15), mtry = c(99,17)))

# Creating Gradient Boosting Machine (GBM) w/ range of hyperparameters: ntrees, max depth, shrinkage, minobspernode - REEVAL THESE PARAMS
GBMLearners <- create.Learner("SL.xgboost", tune = list(ntree = c(1000,5000), max_depth = c(1,5,10),
                                                        shrinkage = c(0.1,0.5,0.01), minobspernode = c(5,10,15)))


# Add screened elastic net algorithms
for(i in 1:64){
  
LibList[i] <- list(c(ElastNetLearners$names[i], ScreenLearners$names[4]))

}

# Add screened random forest algorithms
for(i in 1:12){
  
  LibList[i + 64] <- list(c(RFLearners$names[i], ScreenLearners$names[4]))
  
}

# Add screened gradient boosting machine algorithms
for(i in 1:54){
  
  LibList[i + 76] <- list(c(GBMLearners$names[i], ScreenLearners$names[4]))
  
}

# Setting seed
set.seed(1234)

# Now fitting entire set of learners in SL w/ LibList object
fitScreenedModel <- SuperLearner(Y=ytrain, X=xtrain, family = gaussian(), 
                                    method = "method.NNLS", SL.library = LibList,
                                    cvControl = list(V=5))


# Then save these preds
ySScreen.2022.0 <- predict(fitYScreenOut.20220, newdata = xtrain.2022.0)$pred



