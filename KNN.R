NBA = read.csv("Team.csv")

#Made sure our allstars were a categorical variable
#data preprocessing/ lessons learned
NBA$All.star = as.factor(NBA$All.star)


#Removed Categorical Variables
NBA = NBA[,-(1:3)]
NBA = NBA[, -2]
NBA = NBA[,-3]

#Trained data by removing 2017 data from training set
Score = NBA[(9610:10204),]
NBA = NBA[-(9610:10204),]
Index = 1:9610

Index_2016 = (9045:9609)
Index_2015 = (8380:9044)
Index_2014 = (7770:8379)

training = NBA[-Index_2016,]
validation = NBA[Index_2016,]  

str(NBA)


#KNN Model with n = 5, tested for other KNN's and 5 gave the highest accuracy
library("DMwR")

nearest3 = kNN(All.star~ training$ws + training$PTS + training$USG + training$PER + training$OWS,training,validation,norm=TRUE,k=3)
accr_3 = sum(validation[,1]==nearest3)/nrow(validation)
accr_3

table(validation[,1],nearest3)

############################### KNN with all models


nearest3_all = kNN(All.star~.,training,validation,norm=TRUE,k=3)
accr_3_all = sum(validation[,1]==nearest5_all)/nrow(validation)


table(validation[,1],nearest3_all)


#for (i in 1:nrow(NBA))
#{if (NBA$GS[i] > 70)
#  {NBA$GS[i] = 'Starter'
#}}
