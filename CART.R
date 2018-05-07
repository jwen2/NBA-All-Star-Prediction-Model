data = read.csv("Team.csv - UpdatedNBA.csv.csv")


library(rpart) 
library(rpart.plot) 
library(rattle)

data = data [, -(1:3)]
data = data[,-2]
data = data[,-3]

#Check the variable types and makes sure all the categorical variables are read as factor
str(data)

data$All.star = as.factor(data$All.star)





Index_2017 = (9610:10204)
Index_2016 = (9045:9609)
Index_2015 = (8380:9044)
Index_2014 = (7770:8379)


training = data[-Index_2017,]
validation = data[Index_2017,]





model = rpart(All.star ~ ., data=training)
prp(model,type=1,extra=1)
fancyRpartPlot(model) 


asRules(model) # display the set of rules

printcp(model) #summary of the results





stoppingRules = rpart.control(cp = 0.006, minsplit=3, minbucket=2)
unpruned = rpart(All.star ~ ., data=training, control=stoppingRules)
prp(unpruned,type=1,extra=1)
printcp(unpruned)  
asRules(unpruned)


pred_model = predict(model, validation, type="prob")
pred_unpruned = predict(unpruned, validation, type="prob")


library(SDMTools)
measures_model = accuracy(validation$All.star,pred_model[,2],threshold=0.1) 
measures_unpruned = accuracy(validation$All.star,pred_unpruned[,2],threshold=0.1)

# Compare overall accuracy
measures_model$prop.correct
measures_unpruned$prop.correct

# Compare sensitivity
measures_model$sensitivity
measures_unpruned$sensitivity

library('ROCR')
pred_full = prediction(pred_model[,2],validation$All.star)
pred_full_unpruned = prediction(pred_unpruned[,2], validation$All.star)

roc_full = performance(pred_full, "tpr", "fpr")
roc_unpruned = performance(pred_full_unpruned, "tpr", "fpr")

plot(roc_full, main = "ROC Chart",colorize=TRUE)
plot(roc_unpruned, add = TRUE, colorize=FALSE)

lines(x=c(0,1),y=c(0,1),lty=3)



