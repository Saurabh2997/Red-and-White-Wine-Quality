setwd('~/Desktop/UTD/BUAN6356/Project/Project_data/Wine_YG')
require(Sleuth3)
require(mosaic)
require(knitr)
library(rpart)
library(rpart.plot)
library(caret)

##########################################################################################
# Classification combine quality value to grade (<=5: below average, >=6: above average) #
##########################################################################################

#convert quality to grade
white = read.csv("winequality-white.csv", sep = ";")   #reload dataset to remove previous changes
white$grade = ""
dim(white)[1]
i = 1
while (i <= dim(white)[1]) {
  if (white[i,12] <= 5) {
    #print(red[i,12])
    #print(i)
    white[i,13] = "Below Average"
  } else if (white[i,12] >= 6) {
    white[i,13] = "Above Average"
  }
  i = i+1
}

summary(as.factor(white$grade)) #check by quality

#training dataset generation
set.seed(1)
white.train.index = sample(c(1:dim(white)[1]), dim(white)[1]*0.5)
white.train = white[white.train.index, ]
white.train$quality <- as.factor(white.train$quality)
summary(as.factor(white.train$quality))
white.train.mod = white.train[, -12] #remove the quality column (now we have the grade column)
summary(as.factor(white.train.mod$grade))

#Validation dataset generation
white.valid = white[-white.train.index, ]
white.valid$quality <- as.factor(white.valid$quality)
summary(white.valid$quality)
white.valid.mod = white.valid[, -12]
summary(as.factor(white.valid.mod$grade))


#decision tree - default
white.grade.class = rpart(grade ~., data = white.train.mod, method = "class")
prp(white.grade.class, type =1, extra =1 ,under = TRUE, split.font = 1,box.palette = "Reds", varlen = -20, tweak=1.2)
title(main="White wine no treatment", line = 3) 

##training statistics - default tree
white.grade.predict = predict(white.grade.class, white.train.mod, type = "class")
confusionMatrix(white.grade.predict, as.factor(white.train.mod$grade))  #low accuracy predicting "Low" and "High" classes

##validation statistics - default tree
white.grade.predict.valid = predict(white.grade.class, white.valid.mod, type = "class")
confusionMatrix(white.grade.predict.valid, as.factor(white.valid.mod$grade))

#decision tree - full tree + prune
white.grade.class = rpart(grade ~., data = white.train.mod, cp = -1, method = "class")

##training statistics - full tree
white.grade.predict = predict(white.grade.class, white.train.mod, type = "class")
confusionMatrix(white.grade.predict, as.factor(white.train.mod$grade))  #low accuracy predicting "Low" and "High" classes

##validation statistics - full tree
white.grade.predict.valid = predict(white.grade.class, white.valid.mod, type = "class")
confusionMatrix(white.grade.predict.valid, as.factor(white.valid.mod$grade)) #low accuracy predicting "Low" and "High" classes

##pruned tree
printcp(white.grade.class)
pruned = prune(white.grade.class, cp =  0.00180941)
prp(pruned, type =1, extra =1 ,under = TRUE, split.font = 1,box.palette = "Reds", varlen = -20, tweak=1.4)
title(main="White wine pruned", line = 3)

##training statistics - pruned tree
white.grade.predict = predict(pruned, white.train.mod, type = "class")
confusionMatrix(white.grade.predict, as.factor(white.train.mod$grade))  #low accuracy predicting "Low" and "High" classes

##validation statistics - pruned tree
white.grade.predict.valid = predict(pruned, white.valid.mod, type = "class")
confusionMatrix(white.grade.predict.valid, as.factor(white.valid.mod$grade)) #low accuracy predicting "Low" and "High" classes

#random forest
library(randomForest)
rf.white = randomForest(as.factor(white.train.mod$grade) ~., data = white.train.mod, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)
varImpPlot(rf.white, type = 1)

##training statistics - random forest
rf.white.pred = predict(rf.white, white.train.mod)
confusionMatrix(rf.white.pred, as.factor(white.train.mod$grade))

#validation statistics - random forest
rf.white.pred = predict(rf.white, white.valid.mod)
confusionMatrix(rf.white.pred, as.factor(white.valid.mod$grade))  ###Best results so far

#boosted tree
library(adabag)
white.train.mod$grade = as.factor(white.train.mod$grade)
set.seed(1)
white.boost = boosting(grade ~., data = white.train.mod)

##training statistics - boosted tree
white.boost.pred = predict(white.boost, white.train.mod)
confusionMatrix(as.factor(white.boost.pred$class), as.factor(white.train.mod$grade))

#validation statistics - boosted tree
red.boost.pred = predict(white.boost, white.valid.mod)
confusionMatrix(as.factor(white.boost.pred$class), as.factor(white.valid.mod$grade))




#logistic regression - full model + backward trimming
full.logit.reg = glm(grade ~., data = white.train.mod, family = "binomial")
summary(full.logit.reg)
backwards = step(full.logit.reg)
summary(backwards)

logit.reg = glm(formula = grade ~ volatile.acidity + residual.sugar + free.sulfur.dioxide + 
                  density + pH + sulphates + alcohol, family = "binomial", 
                data = white.train.mod)

##training statistics - full model + backward
logit.reg.pred = predict(logit.reg, white.train.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = white.train.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(white.train.mod$grade))

##validation statistics - full model + backward
logit.reg.pred = predict(logit.reg, white.valid.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = white.valid.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10] #"Above Average" is 0 because R choose alphabetically
confusionMatrix(as.factor(logit.reg.pred), as.factor(white.valid.mod$grade))

#logistic regression - empty model + forward
reduced.logit.reg = glm(grade ~ 1, data = white.train.mod, family = "binomial")
summary(reduced.logit.reg)
forward = step(reduced.logit.reg, scope = list(lower = formula(reduced.logit.reg), upper = formula(full.logit.reg), direction = "forward"))
summary(forward)
logit.reg = glm(formula = grade ~ alcohol + volatile.acidity + residual.sugar + 
                  sulphates + density + pH + free.sulfur.dioxide, family = "binomial", data = white.train.mod)

##training statistics - empty model + forward
logit.reg.pred = predict(logit.reg, white.train.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = white.train.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(white.train.mod$grade))

##validation statistics - empty model + forward
logit.reg.pred = predict(logit.reg, white.valid.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = white.valid.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(white.valid.mod$grade))


#logistic regression - both direction
mid.logit.reg = glm(formula = grade ~ alcohol + volatile.acidity + residual.sugar + 
                      sulphates + density + pH + free.sulfur.dioxide, family = "binomial", data = white.train.mod)
stepwise = step(mid.logit.reg, scope = list(lower = formula(reduced.logit.reg), upper = formula(full.logit.reg), direction = "both", trace = 0))
summary(stepwise)

logit.reg = glm(formula = grade ~ alcohol + volatile.acidity + residual.sugar + 
                  sulphates + density + pH + free.sulfur.dioxide, family = "binomial", data = red.train.mod)

##training statistics - both direction
logit.reg.pred = predict(logit.reg, white.train.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = white.train.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(white.train.mod$grade))

##training statistics - both direction
logit.reg.pred = predict(logit.reg, white.valid.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = white.valid.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(white.valid.mod$grade))


#Neural network
library(neuralnet)
##create dummy variables for training set
white.train$above = (white.train$grade == "Above Average")
white.train$below = (white.train$grade == "Below Average")

##normalize training set attributes between 0 and 1 (based on boosted tree results, pick first 3 main variables)
white.train$alcohol.norm = (white.train$alcohol - min(white.train$alcohol))/(max(white.train$alcohol)-min(white.train$alcohol))
white.train$volatile.acidity.norm = (white.train$volatile.acidity - min(white.train$volatile.acidity))/(max(white.train$volatile.acidity)-(min(white.train$volatile.acidity)))
white.train$density.norm = (white.train$density - min(white.train$density))/(max(white.train$density)-(min(white.train$density)))

##create dummy variables for validation set
white.valid$above = (white.valid$grade == "Above Average")
white.valid$below = (white.valid$grade == "Below Average")

##normalize validation set attributes between 0 and 1 ((based on boosted tree results, pick first 3 main variables)
white.valid$alcohol.norm = (white.valid$alcohol - min(white.valid$alcohol))/(max(white.valid$alcohol)-min(white.valid$alcohol))
white.valid$volatile.acidity.norm = (white.valid$volatile.acidity - min(white.valid$volatile.acidity))/(max(white.valid$volatile.acidity)-(min(white.valid$volatile.acidity)))
white.valid$density.norm = (white.valid$density - min(white.valid$density))/(max(white.valid$density)-(min(white.valid$density)))


##build neural network
set.seed(2)
nn = neuralnet(above + below ~ alcohol.norm + density.norm + volatile.acidity.norm, data = white.train, linear.output = F, hidden = 4, stepmax = 1e7) #had to increase stepmax to converge
plot(nn, rep = "best")
prediction(nn)

library(caret)
##training set statistics
predict = compute(nn, data.frame(white.train$alcohol.norm, white.train$density.norm, white.train$volatile.acidity.norm))
predicted.class = apply(predict$net.result, 1, which.max)-1
predicted.class
result = ifelse(predicted.class == "1","Below Average","Above Average")
confusionMatrix(as.factor(result), as.factor(white.train$grade))

#validation set statistics
validation.predict = compute(nn,data.frame(white.valid$alcohol.norm, white.valid$density.norm, white.valid$volatile.acidity))
valid.predicted.class = apply(validation.predict$net.result, 1, which.max)-1
result = ifelse(valid.predicted.class == "1","Below Average","Above Average")
confusionMatrix(as.factor(result), as.factor(white.valid$grade))
