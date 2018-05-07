#### Logistic Regression ####

# Load the Dataset 

NBA <- read.csv("RemovedVar.csv")

# Remove: ID, Names, Team, Position, Years

NBA = NBA[,-(1:3)]
NBA = NBA[,-2]
NBA = NBA[,-3]

# Make Data Categorical

NBA$All.star = as.factor(NBA$All.star)

# Split the Data 

Score = NBA[(9610:10204),]
NBA = NBA[-(9610:10204),]

Index_2016 = (9045:9609)
Index_2015 = (8380:9044)
Index_2014 = (7770:8379)

training = NBA[-Index_2016,]
validation = NBA[Index_2016,]
validation = NBA[Index_2015,]
validation = NBA[Index_2014,]

training2 = NBA[-(Index_2015),]
validation2 = NBA[Index_2015,]
training3 = NBA[-(Index_2014),]
validation3 = NBA[Index_2014,]

# Run Logisitc Regression - 2016

all_star_logistic = glm(All.star ~ ., training, family="binomial")
pred_probs_logistic = predict(all_star_logistic, validation,type="response")
summary(all_star_logistic)

# 2015

all_star_logistic2 = glm(All.star ~ ., training2, family="binomial")
pred_probs_logistic2 = predict(all_star_logistic2, validation2,type="response")

# 2014

all_star_logistic3 = glm(All.star ~ ., training3, family="binomial")
pred_probs_logistic3 = predict(all_star_logistic3, validation3,type="response")

# Accuracy Measures

AccuMeasures_logistic = accuracy(validation$All.star,pred_probs_logistic,threshold=0.1)
AccuMeasures_logistic2 = accuracy(validation2$All.star,pred_probs_logistic2,threshold=0.1)
AccuMeasures_logistic3 = accuracy(validation3$All.star,pred_probs_logistic3,threshold=0.1)

AccuMeasures_logistic$prop.correct
AccuMeasures_logistic$sensitivity
AccuMeasures_logistic$AUC

AccuMeasures_logistic2$prop.correct
AccuMeasures_logistic2$sensitivity
AccuMeasures_logistic2$AUC

AccuMeasures_logistic3$prop.correct
AccuMeasures_logistic3$sensitivity
AccuMeasures_logistic3$AUC

# Area Under the Curve 

library(SDMTools)
confusion.matrix(validation$All.star,pred_probs_logistic,threshold=0.5)
AccuMeasures_logistic = accuracy(validation$All.star,pred_probs_logistic,threshold=0.5)

AccuMeasures_logistic$prop.correct
AccuMeasures_logistic$sensitivity
AccuMeasures_logistic$AUC

# Use stepwise selection to identify significant variables

NBA_null = glm(All.star ~ 1, training, family="binomial") 
backward = step(all_star_logistic, direction="backward")
forward = step(NBA_null,scope=list(upper=all_star_logistic),direction="forward")
both3 = step(all_star_logistic,direction="both")

pred_probs_both = predict(both, validation,type ="response")
pred_probs_both2 = predict(both2, validation,type ="response")
pred_probs_both3 = predict(both3, validation,type ="response")
summary(both)

confusion.matrix(validation$All.star,pred_probs_both,threshold=0.1)
AccuMeasures_logistic_both = accuracy(validation$All.star,pred_probs_both,threshold=0.1)
AccuMeasures_logistic_both2 = accuracy(validation$All.star,pred_probs_both2,threshold=0.1)
AccuMeasures_logistic_both3 = accuracy(validation$All.star,pred_probs_both3,threshold=0.1)

AccuMeasures_logistic_both$prop.correct
AccuMeasures_logistic_both$sensitivity
AccuMeasures_logistic_both$AUC

AccuMeasures_logistic_both$prop.correct
AccuMeasures_logistic_both2$sensitivity
AccuMeasures_logistic_both3$AUC

AccuMeasures_logistic_both$prop.correct
AccuMeasures_logistic_both2$sensitivity
AccuMeasures_logistic_both3$AUC

# ROC Plots 

library('ROCR')
pred_logistic = prediction(pred_probs_logistic,validation$All.star)
pred_both = prediction(pred_probs_both,validation$All.star)

acc_logistic = performance(pred_logistic,"acc")
plot(acc_logistic,main = "Accuracy for different cutoffs")

gain_logistic = performance(pred_logistic,"tpr","fpr")
plot(gain_logistic, main = "Gain Chart")
lines(x=c(0,1),y=c(0,1),lty=3)

# Adding the result of the model called "both" to ROC curve for comparison to the full model

roc_logistic = performance(pred_logistic,"tpr","fpr")
roc_both = performance(pred_both,"tpr","fpr")
plot(roc_logistic, main = "ROC Chart")
lines(x=c(0,1),y=c(0,1),lty=3)
plot(roc_logistic, add=TRUE,col="red")
