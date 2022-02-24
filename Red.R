setwd('~/Desktop/UTD/BUAN6356/Project/Project_data/Wine_YG')
require(Sleuth3)
require(mosaic)
require(knitr)
white = read.csv("winequality-white.csv", sep = ";")
red = read.csv("winequality-red.csv", sep = ";")

#summary statistics and plot, analyze each column to check outliers and skewness, red and white wine
sum_white = summary(white); sum_white
which(is.na(white))
sum_red = summary(red); sum_red
which(is.na(red))
dev.off(dev.list()["RStudioGD"])
par(mfcol = c(1, 4))
i = 1
while (i <= 12) {
  boxplot(red[,i],white[,i], xlab = colnames(red)[i], ylab = "Unit", names = c("red","white"),cex.axis=1.5, cex.names = 1.5, cex.lab = 1.5)
  points(c(mean(red[,i]),mean(white[,i])),
         col="red",
         pch="+",   ### symbol to use, see ?points
         cex=2)
  i = i+1
}


dev.off(dev.list()["RStudioGD"])
par(mfcol = c(1, 2))
boxplot(red$quality, xlab = "Red wine quality", ylab = "Quality score", ylim = c(1,10), cex.axis=1, cex.names = 1, cex.lab = 1)  #normally distributed, no need to transform
boxplot(white$quality, xlab = "White wine quality", ylab = "", ylim = c(1,10), cex.axis=1, cex.names = 1, cex.lab = 1)

hist(red$quality, main = "", xlab = "Red wine quality", breaks = seq(min(red$quality)-1, max(red$quality)),cex.axis=1, cex.names = 1, cex.lab = 1)
hist(white$quality, main = "", xlab = "White wine quality", breaks = seq(min(white$quality)-1, max(white$quality)),cex.axis=1, cex.names = 1, cex.lab = 1)

##############################################################
#Check correlation between columns, red and white wine
cor.matrix = cor(red[,1:11]); cor.matrix

#Print out attributes with > 0.5 correlation
i = 1
while (i <= nrow(cor.matrix)) {
  j = i + 1
  while (j <= ncol(cor.matrix)) {
    if (abs(cor.matrix[i,j]) > 0.5) {
      print(paste(colnames(cor.matrix)[i],"AND",rownames(cor.matrix)[j],":",round(cor.matrix[i,j],4)))
      j = j+1
    } else {
      j = j+1
    }
  }
  i = i+1
}


#Heatmap to show correlation of all attributes, red and white wine
library(gplots)
my_palette <- colorRampPalette(c("red", "white", "blue"));my_palette
heatmap.2(cor(red), Rowv = FALSE, Colv = FALSE, col=my_palette(200),dendrogram = "none", symm=F,symkey=F,symbreaks=T, scale="none",
          cellnote = round(cor(red),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

heatmap.2(cor(white), Rowv = FALSE, Colv = FALSE, col=my_palette(200),dendrogram = "none", symm=F,symkey=F,symbreaks=T, scale="none",
          cellnote = round(cor(white),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))




##################
#  Red wine only #
##################

###############################################
# Hierarchical clustering with normalize data #
###############################################

#normalization
red.norm <- sapply(red, scale)
row.names(red.norm) = row.names(red)
options(max.print=10000)
red.norm.dist = dist(red.norm, method = "euclidean")

#Use single distance
hc = hclust(red.norm.dist, method = "single");hc
plot(hc, hang = -1, ann = FALSE)

k = 3
memb = cutree(hc, k)
memb_sum = table(memb)
names(memb_sum) = paste("cluster", c(1:k)) # same as names(memb_sum) = c("cluster 1","cluster 2","cluster 3", "cluster 4", "cluster 5", "cluster 6")
memb_sum

#Use complete distance
hc = hclust(red.norm.dist, method = "complete");hc
plot(hc, hang = -1, ann = FALSE)

k = 3
memb = cutree(hc, k)
memb_sum = table(memb)
names(memb_sum) = paste("cluster", c(1:k)) # same as names(memb_sum) = c("cluster 1","cluster 2","cluster 3", "cluster 4", "cluster 5", "cluster 6")
memb_sum

######################
# K-means clustering #
######################

#k = 2 to 4
set.seed(42)
k = 2
km <- kmeans(red.norm, k)  #similar results using quality attribute or not use quality attribute

#plot k-means results
#dev.off(dev.list()["RStudioGD"])
par(mar = c(5, 5, 2, 2))
plot(c(0), xaxt = 'n', xlab = "Attribute",ylab = "Center", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0.5, ncol(red.norm)), cex.lab = 1.5)

#axis(1, at = c(1:ncol(red.norm)), labels = colnames(red.norm), cex.axis = 0.8)
axis(1, at = c(1:ncol(red.norm)), labels = c("Fixed acidity","Volatile acidity", "Citric acid","Residual sugar","Chlorides","SO2(Free)","SO2(Total)","Density","pH","Sulphates","Alcohol","Quality"))

for (i in c(1:k))
  lines(km$centers[i,], lty = i, lwd = 3, col = i)
#lines(km$centers[i,], lty = i, lwd = 3, col = color_list[i])

text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:k)))

km$centers
km$withinss
km$size
dist(km$centers)
dev.off(dev.list()["RStudioGD"])
hist(red.norm[,12], main = "", xlab = "Red wine quality", xlim = c(min(red.norm[,12]),max(red.norm[,12])),cex.axis=1, cex.lab = 1, breaks = 20)

##############
# Regression #
##############

model0 = lm(quality ~., data = red); model0
summary(model0)
library("HH")
vif = vif(model0);vif

backward<- step(model0, direction="backward")
summary(backward)
model1 = lm(formula = quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
              total.sulfur.dioxide + pH + sulphates + alcohol, data = red); summary(model1)
library("HH")
vif = vif(model1);vif

model3 = lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
              chlorides + free.sulfur.dioxide + sulphates + alcohol, data = red)
summary(model3)
backward<- step(model3, direction="backward")
summary(backward)







#########################################
# Classification based on quality score #
#########################################

#training and validation data generation
library(rpart)
library(rpart.plot)
library(caret)
dim(red)
summary(as.factor(red$quality))  #check distribution
set.seed(1)
red.train.index = sample(c(1:dim(red)[1]), dim(red)[1]*0.5) #use 50% as training set

red.train = red[red.train.index, ]
red.train$quality <- as.factor(red.train$quality)
summary(as.factor(red.train$quality)) #already captured all low quality wines
red.valid = red[-red.train.index, ]

#decision tree
red.quality.class = rpart(quality ~., data = red.train, method = "class")
prp(red.quality.class, type =1, extra =2 ,under = TRUE, split.font = 1, varlen = -10) #result does not capture all quality number as classes

red.quality.predict = predict(red.quality.class, red.train, type = "class")
confusionMatrix(red.quality.predict, as.factor(red.train$quality))

red.quality.class = rpart(quality ~., data = red.train, cp = -1, method = "class")  #full tree
red.quality.predict = predict(red.quality.class, red.train, type = "class")
confusionMatrix(red.quality.predict, as.factor(red.train$quality))

#random forest
rf.red = randomForest(as.factor(red.train$quality) ~., data = red.train, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)
varImpPlot(rf.red, type = 1)
rf.red.pred = predict(rf.red, red.valid)
confusionMatrix(rf.red.pred, as.factor(red.valid$quality))

###boosted tree
library(adabag)
red.train.mod$grade = as.factor(red.train.mod$grade)
set.seed(1)

red.boost = boosting(grade ~., data = red.train.mod)
red.boost.pred = predict(red.boost, red.valid.mod); red.boost.pred
confusionMatrix(as.factor(red.boost.pred$class), as.factor(red.valid.mod$grade))

###################################################################################
# Classification combine quality value to grade (<5: Low, >6: High, rest: Medium) #
###################################################################################

#Create new column called "grade" and set to Low, Medium, High based on the quality score
red$grade = ""
dim(red)[1]
i = 1
while (i <= dim(red)[1]) {
  if (red[i,12] < 5) {
    #print(red[i,12])
    #print(i)
    red[i,13] = "Low"
  } else if (red[i,12] > 6) {
    red[i,13] = "High"
  } else {
    red[i,13] = "Medium"
  }
  i = i+1
}

summary(as.factor(red$quality)) #check by quality

#sampling red with slight increase probability for quality < 5 and quality >6
#red training data set generation
#------- No upsampling ------ #
set.seed(1)
red.train.index = sample(c(1:dim(red)[1]), dim(red)[1]*0.5, prob = ifelse((red$quality < 5 | red$quality > 6), 0.5, 0.5 ))
red.train = red[red.train.index, ]
red.train$quality <- as.factor(red.train$quality)
summary(as.factor(red.train$quality))
red.train.mod = red.train[, -12] #remove the quality column (now we have the grade column)
summary(as.factor(red.train.mod$grade))

#red validation dataset generation
red.valid = red[-red.train.index, ]
red.valid$quality <- as.factor(red.valid$quality)
summary(red.valid$quality)
red.valid.mod = red.valid[, -12]
summary(as.factor(red.valid.mod$grade))

#decision tree - default
red.grade.class = rpart(grade ~., data = red.train.mod, method = "class")
prp(red.grade.class, type =1, extra =1 ,under = TRUE, split.font = 1,box.palette = "Reds", varlen = -20, tweak=1.2)
title(main="Red wine no treatment", line = 3) 

##training statistics default tree
red.grade.predict = predict(red.grade.class, red.train.mod, type = "class")
confusionMatrix(red.grade.predict, as.factor(red.train.mod$grade))  #low accuracy predicting "Low" and "High" classes

##validation statistics default tree
red.grade.predict.valid = predict(red.grade.class, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade)) #low accuracy predictiong "Low" and "High" classes

#decision tree full tree + prune
red.grade.class = rpart(grade ~., data = red.train.mod, cp = -1, method = "class")
printcp(red.grade.class)

##training statistics full tree + prune
red.grade.predict = predict(red.grade.class, red.train.mod, type = "class")
confusionMatrix(red.grade.predict, as.factor(red.train.mod$grade))  #low accuracy predicting "Low" and "High" classes

##validation statistics full tree + prune
red.grade.predict.valid = predict(red.grade.class, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade)) #low accuracy predicting "Low" and "High" classes

#random forest
library(randomForest)
rf.red = randomForest(as.factor(red.train.mod$grade) ~., data = red.train.mod, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)
varImpPlot(rf.red, type = 1)
rf.red.pred = predict(rf.red, red.valid.mod)
confusionMatrix(rf.red.pred, as.factor(red.valid.mod$grade))

#boosted tree
library(adabag)
red.train.mod$grade = as.factor(red.train.mod$grade)
set.seed(1)

red.boost = boosting(grade ~., data = red.train.mod)
red.boost.pred = predict(red.boost, red.valid.mod); red.boost.pred
confusionMatrix(as.factor(red.boost.pred$class), as.factor(red.valid.mod$grade))

#------- use up-sampling -------#
table(red.train.mod$grade)
set.seed(234)
dtrain0 <- upSample(x = red.train.mod[, -12], y = as.factor(red.train.mod$grade))
table(dtrain0$Class)

#------- Not include cost matrix -------#
#decision tree - default
dtrain0.class = rpart(Class ~., data = dtrain0, method = "class")
prp(dtrain0.class, type =1, extra =1 ,under = TRUE, split.font = 1, box.palette = "Reds", varlen = -20, tweak=1.2)
title(main="Red wine with up sampling", line = 3) 

##training statistics - default tree
red.grade.predict = predict(dtrain0.class, dtrain0, type = "class")
confusionMatrix(red.grade.predict, as.factor(dtrain0$Class))  #Good at predicting "Low" and "High", mediocre at "Medium"

##validation statistics - default tree
red.grade.predict.valid = predict(dtrain0.class, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade))

#decision tree full + prune
red.grade.class = rpart(Class ~., data = dtrain0, cp = -1, method = "class")
printcp(red.grade.class)

##training statistics - full tree
red.grade.predict = predict(red.grade.class, dtrain0, type = "class")
confusionMatrix(red.grade.predict, as.factor(dtrain0$Class))  #low accuracy predicting "Low" and "High" classes

##validation statistics - full tree
red.grade.predict.valid = predict(red.grade.class, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade)) #low accuracy predicting "Low" and "High" classes

##prune full tree
red.prune = rpart(Class ~., data = dtrain0, cp = 0.0000001, minsplit = 1, xval = 5, method = "class")
printcp(red.grade.class)

#random forest
library(randomForest)
rf.red = randomForest(as.factor(dtrain0$Class) ~., data = dtrain0, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)
varImpPlot(rf.red, type = 1)
rf.red.pred = predict(rf.red, red.valid.mod)
confusionMatrix(rf.red.pred, as.factor(red.valid.mod$grade))

#boosted tree
library(adabag)
dtrain0$Class = as.factor(dtrain0$Class)
set.seed(1)

red.boost = boosting(Class ~., data = dtrain0)
red.boost.pred = predict(red.boost, red.valid.mod); red.boost.pred
confusionMatrix(as.factor(red.boost.pred$class), as.factor(red.valid.mod$grade))

#------- Include cost matrix -------#
#cost matrix generation
loss_matr <- matrix(c(0,2,2,1,0,1,1,1,0), nrow = 3); loss_matr

#decision tree - default tree
dtrain0.class = rpart(Class ~., data = dtrain0, method = "class", parms = list(loss = loss_matr))
prp(dtrain0.class, type =1, extra =1 ,under = TRUE, split.font = 1, box.palette = "Reds", varlen = -20, tweak=1.2)
title(main="Red wine with upsampling and cost matrix", line = 3) 

##training statistics - default tree
red.grade.predict = predict(dtrain0.class, dtrain0, type = "class")
confusionMatrix(red.grade.predict, as.factor(dtrain0$Class))  #Good at predicting "Low" and "High", mediocre at "Medium"

##validation statistics - default tree
red.grade.predict.valid = predict(dtrain0.class, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade))

#decision tree - full tree + prune
red.grade.class = rpart(Class ~., data = dtrain0, cp = -1, method = "class")
printcp(red.grade.class)

##training statistics - full tree
red.grade.predict = predict(red.grade.class, dtrain0, type = "class")
confusionMatrix(red.grade.predict, as.factor(dtrain0$Class))  #low accuracy predicting "Low" and "High" classes

##validation statistics - full tree
red.grade.predict.valid = predict(red.grade.class, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade)) #low accuracy predicting "Low" and "High" classes

##prune full tree
red.prune = rpart(Class ~., data = dtrain0, cp = 0.0000001, minsplit = 1, xval = 5, method = "class")
printcp(red.grade.class)

##random forest
library(randomForest)
rf.red = randomForest(as.factor(dtrain0$Class) ~., data = dtrain0, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)
varImpPlot(rf.red, type = 1)
rf.red.pred = predict(rf.red, red.valid.mod)
confusionMatrix(rf.red.pred, as.factor(red.valid.mod$grade))

##boosted tree
library(adabag)
dtrain0$Class = as.factor(dtrain0$Class)
set.seed(1)

red.boost = boosting(Class ~., data = dtrain0)
red.boost.pred = predict(red.boost, red.valid.mod); red.boost.pred
confusionMatrix(as.factor(red.boost.pred$class), as.factor(red.valid.mod$grade))

##########################################################################################
# Classification combine quality value to grade (<=5: below average, >=6: above average) #
##########################################################################################

#convert quality to grade
red = read.csv("winequality-red.csv", sep = ";")   #reload dataset to remove previous changes
red$grade = ""
dim(red)[1]
i = 1
while (i <= dim(red)[1]) {
  if (red[i,12] <= 5) {
    #print(red[i,12])
    #print(i)
    red[i,13] = "Below Average"
  } else if (red[i,12] >= 6) {
    red[i,13] = "Above Average"
  }
  i = i+1
}

summary(as.factor(red$grade)) #check by quality

#training dataset generation
set.seed(1)
red.train.index = sample(c(1:dim(red)[1]), dim(red)[1]*0.5)
red.train = red[red.train.index, ]
red.train$quality <- as.factor(red.train$quality)
summary(as.factor(red.train$quality))
red.train.mod = red.train[, -12] #remove the quality column (now we have the grade column)
summary(as.factor(red.train.mod$grade))

#Validation dataset generation
red.valid = red[-red.train.index, ]
red.valid$quality <- as.factor(red.valid$quality)
summary(red.valid$quality)
red.valid.mod = red.valid[, -12]
summary(as.factor(red.valid.mod$grade))

#decision tree - default
red.grade.class = rpart(grade ~., data = red.train.mod, method = "class")
prp(red.grade.class, type =1, extra =1 ,under = TRUE, split.font = 1,box.palette = "Reds", varlen = -20, tweak=1.2)
title(main="Red wine no treatment", line = 3) 

##training statistics - default tree
red.grade.predict = predict(red.grade.class, red.train.mod, type = "class")
confusionMatrix(red.grade.predict, as.factor(red.train.mod$grade))  #low accuracy predicting "Low" and "High" classes

##validation statistics - default tree
red.grade.predict.valid = predict(red.grade.class, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade))

#decision tree - full tree + prune
red.grade.class = rpart(grade ~., data = red.train.mod, cp = -1, method = "class")

##training statistics - full tree
red.grade.predict = predict(red.grade.class, red.train.mod, type = "class")
confusionMatrix(red.grade.predict, as.factor(red.train.mod$grade))  #low accuracy predicting "Low" and "High" classes

##validation statistics - full tree
red.grade.predict.valid = predict(red.grade.class, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade)) #low accuracy predicting "Low" and "High" classes

##pruned tree
printcp(red.grade.class)
pruned = prune(red.grade.class, cp =  0.0079365)
prp(pruned, type =1, extra =1 ,under = TRUE, split.font = 1,box.palette = "Reds", varlen = -20, tweak=1.4)
title(main="Red wine pruned", line = 3)

##training statistics - pruned tree
red.grade.predict = predict(pruned, red.train.mod, type = "class")
confusionMatrix(red.grade.predict, as.factor(red.train.mod$grade))  #low accuracy predicting "Low" and "High" classes

##validation statistics - pruned tree
red.grade.predict.valid = predict(pruned, red.valid.mod, type = "class")
confusionMatrix(red.grade.predict.valid, as.factor(red.valid.mod$grade)) #low accuracy predicting "Low" and "High" classes

#random forest
library(randomForest)
rf.red = randomForest(as.factor(red.train.mod$grade) ~., data = red.train.mod, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)
varImpPlot(rf.red, type = 1)

##training statistics - random forest
rf.red.pred = predict(rf.red, red.train.mod)
confusionMatrix(rf.red.pred, as.factor(red.train.mod$grade))

#validation statistics - random forest
rf.red.pred = predict(rf.red, red.valid.mod)
confusionMatrix(rf.red.pred, as.factor(red.valid.mod$grade))

#boosted tree
library(adabag)
red.train.mod$grade = as.factor(red.train.mod$grade)
set.seed(1)
red.boost = boosting(grade ~., data = red.train.mod)

##training statistics - boosted tree
red.boost.pred = predict(red.boost, red.train.mod)
confusionMatrix(as.factor(red.boost.pred$class), as.factor(red.train.mod$grade))

#validation statistics - boosted tree
red.boost.pred = predict(red.boost, red.valid.mod)
confusionMatrix(as.factor(red.boost.pred$class), as.factor(red.valid.mod$grade))


#logistic regression - full model + backward trimming
full.logit.reg = glm(grade ~., data = red.train.mod, family = "binomial")
summary(full.logit.reg)
backwards = step(full.logit.reg)
summary(backwards)

logit.reg = glm(formula = grade ~ alcohol + volatile.acidity + sulphates + 
                  total.sulfur.dioxide + free.sulfur.dioxide, family = "binomial", 
                data = red.train.mod)

##training statistics - full model + backward
logit.reg.pred = predict(logit.reg, red.train.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = red.train.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(red.train.mod$grade))

##validation statistics - full model + backward
logit.reg.pred = predict(logit.reg, red.valid.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = red.valid.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(red.valid.mod$grade))

#logistic regression - empty model + forward
reduced.logit.reg = glm(grade ~ 1, data = red.train.mod, family = "binomial")
summary(reduced.logit.reg)
forward = step(reduced.logit.reg, scope = list(lower = formula(reduced.logit.reg), upper = formula(full.logit.reg), direction = "forward"))
summary(forward)
logit.reg = glm(formula = grade ~ fixed.acidity + volatile.acidity + citric.acid + 
                  free.sulfur.dioxide + total.sulfur.dioxide + sulphates + 
                  alcohol, family = "binomial", data = red.train.mod)

##training statistics - empty model + forward
logit.reg.pred = predict(logit.reg, red.train.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = red.train.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(red.train.mod$grade))

##validation statistics - empty model + forward
logit.reg.pred = predict(logit.reg, red.valid.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = red.valid.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(red.valid.mod$grade))


#logistic regression - both direction
mid.logit.reg = glm(formula = grade ~ fixed.acidity + volatile.acidity + citric.acid + 
                                  free.sulfur.dioxide + total.sulfur.dioxide + sulphates + 
                                  alcohol, family = "binomial", data = red.train.mod)
stepwise = step(mid.logit.reg, scope = list(lower = formula(reduced.logit.reg), upper = formula(full.logit.reg), direction = "both", trace = 0))
summary(stepwise)

logit.reg = glm(formula = grade ~ fixed.acidity + volatile.acidity + citric.acid + 
                  free.sulfur.dioxide + total.sulfur.dioxide + sulphates + 
                  alcohol, family = "binomial", data = red.train.mod)

##training statistics - both direction
logit.reg.pred = predict(logit.reg, red.train.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = red.train.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(red.train.mod$grade))

##training statistics - both direction
logit.reg.pred = predict(logit.reg, red.valid.mod, type = "response"); logit.reg.pred[1:10]
data.frame(actual = red.valid.mod$grade[1:10], predicted = logit.reg.pred[1:10])

logit.reg.pred = ifelse(logit.reg.pred>0.5, "Below Average", "Above Average"); logit.reg.pred[1:10]
confusionMatrix(as.factor(logit.reg.pred), as.factor(red.valid.mod$grade))

#Neural network
library(neuralnet)
##create dummy variables for training set
red.train$above = (red.train$grade == "Above Average")
red.train$below = (red.train$grade == "Below Average")

##normalize training set attributes between 0 and 1 (based on boosted tree results, pick first 3 main variables)
red.train$alcohol.norm = (red.train$alcohol - min(red.train$alcohol))/(max(red.train$alcohol)-min(red.train$alcohol))
red.train$sulphates.norm = (red.train$sulphates - min(red.train$sulphates))/(max(red.train$sulphates)-(min(red.train$sulphates)))
red.train$volatile.acidity.norm = (red.train$volatile.acidity - min(red.train$volatile.acidity))/(max(red.train$volatile.acidity)-(min(red.train$volatile.acidity)))

##create dummy variables for validation set
red.valid$above = (red.valid$grade == "Above Average")
red.valid$below = (red.valid$grade == "Below Average")

##normalize validation set attributes between 0 and 1 ((based on boosted tree results, pick first 3 main variables)
red.valid$alcohol.norm = (red.valid$alcohol - min(red.valid$alcohol))/(max(red.valid$alcohol)-(min(red.valid$alcohol)))
red.valid$sulphates.norm = (red.valid$sulphates - min(red.valid$sulphates))/(max(red.valid$sulphates)-(min(red.valid$sulphates)))
red.valid$volatile.acidity.norm = (red.valid$volatile.acidity - min(red.valid$volatile.acidity))/(max(red.valid$volatile.acidity)-(min(red.valid$volatile.acidity)))

##build neural network
set.seed(2)
nn = neuralnet(above + below ~ alcohol.norm + sulphates.norm + volatile.acidity.norm, data = red.train, linear.output = F, hidden = 4)
plot(nn, rep = "best")
prediction(nn)

library(caret)
##training set statistics
predict = compute(nn, data.frame(red.train$alcohol.norm, red.train$sulphates.norm, red.train$volatile.acidity.norm))
predicted.class = apply(predict$net.result, 1, which.max)-1
predicted.class
result = ifelse(predicted.class == "1","Below Average","Above Average")
confusionMatrix(as.factor(result), as.factor(red.train$grade))

#validation set statistics
validation.predict = compute(nn,data.frame(red.valid$alcohol.norm, red.valid$sulphates.norm, red.valid$volatile.acidity))
valid.predicted.class = apply(validation.predict$net.result, 1, which.max)-1
result = ifelse(valid.predicted.class == "1","Below Average","Above Average")
confusionMatrix(as.factor(result), as.factor(red.valid$grade))
