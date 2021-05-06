#=============================================== ITEM 0 ========================================================

## PERFORMING YEO-JOHNSON AND STANDARDIZING DATA ##

## solTrainX ## 

solTrainX_YJ <- yeojohnson(data.matrix(solTrainX), eps = 0.001, standardize = TRUE)
solTrainX_YJ <- as.data.frame(solTrainX_YJ$x.t)


## solTestX ## 

solTestX_YJ <- yeojohnson(data.matrix(solTestX), eps = 0.001, standardize = TRUE)
solTestX_YJ <- as.data.frame(solTestX_YJ$x.t)


## solTestY ## 

solTestY_YJ <- yeojohnson(solTestY, eps = 0.001, standardize = TRUE)
solTestY_YJ <- solTestY_YJ$x.t


## solTrainY ## 

solTrainY_YJ <- yeojohnson(solTrainY, eps = 0.001, standardize = TRUE)
solTrainY_YJ <- solTrainY_YJ$x.t

### REMOVING HIGH-CORRELATED PREDICTORS ### 

##TRAIN##

copy1 <- solTrainX_YJ

copy1 <- copy1[,-228] ## REMOVING SURFACE AREA 2
copy1 <- copy1[,-211] ## REMOVING NUMNONHATOMS
copy1 <- copy1[,-211] ## REMOVING NUM BONDS 

###TEST###

copy2 <- solTestX_YJ

copy2 <- copy2[,-228] ## REMOVING SURFACE AREA 2
copy2 <- copy2[,-211] ## REMOVING NUMNONHATOMS
copy2 <- copy2[,-211] ## REMOVING NUM BONDS 

View(copy2)

#=============================================== ITEM 1 ========================================================
## MUTIPLE LINEAR REGRESSION##

## Binding the outcome with the predictors ##

regtrainset <- cbind(copy1,solTrainY_YJ)

## Building the linear model ##

lmodel <- lm(regtrainset$solTrainY_YJ ~., data = regtrainset)
summary(lmodel)

## Testing the model performance with the test set ## 

lmodelpred <- predict(lmodel,solTestX_YJ)
lmodelval <- data.frame(obs = solTestY, pred = lmodelpred)
defaultSummary(lmodelval)

## Training the model with a 10-fold cross validation ##

resampctrl <- trainControl(method = "cv", number = 10)
set.seed(100)
lmodelfold <- train(x = copy1, y= solTrainY_YJ, method = "lm", trControl = resampctrl)
#=============================================== ITEM 2 ========================================================
set.seed(100)
trControl10 <- trainControl(method = "cv", number = 10)
modelo <- train(x = trainxbc, y = trainybc, method = "ridge", trControl = trControl10, tuneLength = 10)
print(modelo)

predicao_treino <- predict(modelo, TrainX)
predicao_teste <- predict(modelo, TestX)

ggplot(data.frame(solTestY_YJ, predicao_teste), aes(solTestY_YJ, predicao_teste)) + geom_point(color = "red") + xlab("Real") + ylab("Predito") + ggtitle("Ridge 10-fold Testset")
ggplot(data.frame(solTrainY_YJ, predicao_treino), aes(solTrainY_YJ, predicao_treino)) + geom_point(color = "red") + xlab("Real") + ylab("Predito") + ggtitle("Ridge 10-fold Trainset")
#=============================================== ITEM 3 ========================================================
# Principal Components Regression

library(pls)
library(AppliedPredictiveModeling)
data(solubility)

#Training Set
trainxbc <- copy1
trainybc <- solTrainY_YJ
#Test Set
testxbc <- copy2
testybc <- solTestY
#dataset Train
regtrainset <- cbind(trainxbc, trainybc)

#Obtaining pcr model
set.seed(777)
pcr.fit10 <- pcr(trainybc~., data=regtrainset, scale=T, validation = "CV")
summary(pcr.fit10)

#set.seed(777)
#pcr.fit10 <- pcr(trainybc~., data=regtrainset, scale=T, validation = "CV", segments = 5)
#summary(pcr.fit5)

#ValidationPlot
validationplot(pcr.fit10, val.type = "MSEP")
validationplot(pcr.fit10, val.type = "R2")
#validationplot(pcr.fit5, val.type = "MSEP")
#validationplot(pcr.fit5, val.type = "R2")

#Predict with cross-validation
set.seed(777)
pcr.pred10 = predict(pcr.fit10, testxbc, ncomp = 23)
mean((pcr.pred10-testybc)^2)


#TestSet
predict <- cbind(pcr.pred10, testybc)
ggplot(data.frame(predict[1:316,2], predict[1:316,1]), aes(predict[1:316,2], predict[1:316,1])) + geom_point(color = "red") + xlab("Real") + ylab("Predito") + ggtitle("PCR 10-Fold Testset")
+ geom_point(aes(x = predict[1:316,2], y = predict[1:316,1]))

coefplot(pcr.fit10)
