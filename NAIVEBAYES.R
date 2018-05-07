###VERSION 1 HANDPICKED SIGNIFICANT FACTORS###

### Load the Dataset ###

NBA=read.csv("Team.csv")

### Transform the Data ###

#G

for (i in 1:nrow(NBA))
{if (NBA$G[i] > 75){
  NBA$G[i] = 'Iron Man'
}
}
for (i in 1:nrow(NBA))
{if (NBA$G[i] <= 75 & NBA$G[i]>50 ){
  NBA$G[i] = 'Healthy'}
}
for (i in 1:nrow(NBA))
{if (NBA$G[i] <50 ){
  NBA$G[i] = 'Injured'}
}

#GS

for (i in 1:nrow(NBA))
{if (NBA$GS[i] > 70 ){
  NBA$G[i] = 'Starter'}
  
}
for (i in 1:nrow(NBA))
{if (NBA$GS[i] <= 70 & NBA$G[i]>30 ){
  NBA$GS[i] = 'Reserve'}
}
for (i in 1:nrow(NBA))
{if (NBA$GS[i] <30 ){
  NBA$GS[i] = 'Bench'}
}

### PER ###

{if (NBA$PER[i] > 20){
  NBA$PER[i] = 'Amazing PER'
}
}
{if (NBA$PER[i] <= 20){
  NBA$PER[i] = 'Normal PER'
}
}


### TS. ###

for (i in 1:nrow(NBA))
{if (NBA$TS.[i] > 70)
{NBA$TS.[i] = 'Starter'
}}

### TRB. ###

for (i in 1:nrow(NBA))
{if (NBA$TRB.[i] > 15)
{NBA$TRB.[i] = 'Insane_Rebounder'
}}

for (i in 1:nrow(NBA))
{if (NBA$TRB.[i] <= 15 & NBA$TRB.[i] > 8)
{NBA$TRB.[i] = 'Great_Rebounder'
}}

for (i in 1:nrow(NBA))
{if (NBA$TRB.[i] >= 8)
{NBA$TRB.[i] = 'Average_Rebounder'
}}

### AST. ###

for (i in 1:nrow(NBA))
{if (NBA$AST.[i] > 10)
{NBA$AST.[i] = 'Great_Facilitator'
}}

for (i in 1:nrow(NBA))
{if (NBA$AST.<= 9 & NBA$AST.[i] > 5)
{NBA$AST.[i] = 'Good_Facilitator'
}}

for (i in 1:nrow(NBA))
{if (NBA$AST.[i] <=5)
{NBA$AST.[i] = 'Average_Facilitator'
}}

### STL. ###

for (i in 1:nrow(NBA))
{if (NBA$STL.[i] >= 1)
{NBA$STL.[i] = 'Good_Stealer'
}}

for (i in 1:nrow(NBA))
{if (NBA$STL.[i] < 1)
{NBA$STL.[i] = 'Average_Stealer'
}}

### BLK. ###

for (i in 1:nrow(NBA))
{if (NBA$BLK.[i] >= 2)
{NBA$BLK.[i] = 'Good_Defender'
}}

for (i in 1:nrow(NBA))
{if (NBA$BLK.[i] < 2)
{NBA$BLK.[i] = 'Average_Defender'
}}

### PTS ###

for (i in 1:nrow(NBA))
{if (NBA$PTS[i] > 20)
{NBA$PTS[i] = 'Amazing_Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$PTS[i] >= 10 & NBA$PTS[i] < 20)
{NBA$PTS[i] = 'Good_Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$PTS[i] < 10)
{NBA$PTS[i] = 'Low_Scorer'
}}

### FGA ###

for (i in 1:nrow(NBA))
{if (NBA$FGA[i] > 800)
{NBA$FGA[i] = 'Primary_Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$FGA[i] <= 800 & NBA$FGA[i] > 500)
{NBA$FGA[i] = 'Secondary_Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$FGA[i] < 500)
{NBA$FGA[i] = 'Average_Scorer'
}}

str(NBA)
NBA$G = as.factor(NBA$G)
NBA$GS = as.factor(NBA$GS)
NBA$PER = as.factor(NBA$PER)
NBA$TS. = as.factor(NBA$TS.)
NBA$TRB. = as.factor(NBA$TRB.)
NBA$AST. = as.factor(NBA$AST.)
NBA$STL. = as.factor(NBA$STL.)
NBA$BLK. = as.factor(NBA$BLK.)
NBA$PTS = as.factor(NBA$PTS)
NBA$FGA = as.factor(NBA$FGA)

NBA = read.csv("RemovedVar.csv")

#Made sure our allstars were a categorical variable
#data preprocessing/ lessons learned
NBA$All.star = as.factor(NBA$All.star)

#Trained data by removing 2017 data from training set
Score = NBA[(9610:10204),]
NBA = NBA[-(9610:10204),]

Index_2016 = (9045:9609)
Index_2015 = (8380:9044)
Index_2014 = (7770:8379)

training1 = NBA[-(Index_2016),]
validation1 = NBA[Index_2016,]
training2 = NBA[-(Index_2015),]
validation2 = NBA[Index_2015,]
training3 = NBA[-(Index_2014),]
validation3 = NBA[Index_2014,]

library(e1071)
Naivebayes_NBA1 = naiveBayes(All.star ~ G+GS+PER+TS.+TRB.+AST.+STL.+BLK.+PTS+FGA,training1)
Naivebayes_NBA1
Naivebayes_NBA2 = naiveBayes(All.star ~ G+GS+PER+TS.+TRB.+AST.+STL.+BLK.+PTS+FGA,training2)
Naivebayes_NBA2
Naivebayes_NBA3 = naiveBayes(All.star ~ G+GS+PER+TS.+TRB.+AST.+STL.+BLK.+PTS+FGA,training3)
Naivebayes_NBA3

Pred_NBA1 = predict(Naivebayes_NBA1, validation1,type="raw")
Pred_NBA1
Pred_NBA2= predict(Naivebayes_NBA2, validation2,type="raw")
Pred_NBA2
Pred_NBA3 = predict(Naivebayes_NBA3, validation3,type="raw")
Pred_NBA3

library(SDMTools)
confusion.matrix(validation1$All.star,Pred_NBA1[,2],threshold=0.5)
AccuMeasures_full1 = accuracy(validation1$All.star,Pred_NBA1[,2],threshold=0.5)
confusion.matrix(validation2$All.star,Pred_NBA2[,2],threshold=0.5)
AccuMeasures_full2 = accuracy(validation2$All.star,Pred_NBA2[,2],threshold=0.5)
confusion.matrix(validation3$All.star,Pred_NBA3[,2],threshold=0.5)
AccuMeasures_full3 = accuracy(validation3$All.star,Pred_NBA3[,2],threshold=0.5)

library('ROCR')
pred_full = prediction(Pred_NBA1[,2],validation1$All.star)
roc_full = performance(pred_full, "tpr", "fpr")
plot(roc_full, main = "ROC Chart",colorize=TRUE)
lines(x=c(0,1),y=c(0,1),lty=3)








### VERSION 2 (TREE AND LOGISTIC REGRESSION) ###

NBA=read.csv("Team.csv")

### Transform the Data ###

#WS
for (i in 1:nrow(NBA))
{if (NBA$WS[i] > 8){
  NBA$WS[i] = 'Great Win Contributor'
}
}
for (i in 1:nrow(NBA))
{if (NBA$WS[i] <= 8 & NBA$WS[i]>2 ){
  NBA$WS[i] = 'Good Win Contributor'}
}
for (i in 1:nrow(NBA))
{if (NBA$WS[i] <= 2 ){
  NBA$WS[i] = 'Average Win Contributor'}
}

### PTS ###

for (i in 1:nrow(NBA))
{if (NBA$PTS[i] > 20)
{NBA$PTS[i] = 'Amazing_Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$PTS[i] >= 10 & NBA$PTS[i] < 20)
{NBA$PTS[i] = 'Good_Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$PTS[i] < 10)
{NBA$PTS[i] = 'Low_Scorer'
}}

###USG###

for (i in 1:nrow(NBA))
{if (NBA$USG.[i] > 25)
{NBA$USG.[i] = 'Great Usage'
}}

for (i in 1:nrow(NBA))
{if (NBA$USG.[i] <= 25)
{NBA$USG.[i] = 'Average Usage'
}}

###FT###
for (i in 1:nrow(NBA))
{if (NBA$FT[i] > 250)
{NBA$FT[i] = 'Great Free Throw'
}}

for (i in 1:nrow(NBA))
{if (NBA$FT[i] <= 250)
{NBA$FT[i] = 'Average Free Throw'
}}

###Box+-###

for (i in 1:nrow(NBA))
{if (NBA$BPM[i] > 20)
{NBA$BPM[i] = 'Good Performance'
}}

for (i in 1:nrow(NBA))
{if (NBA$BPM[i] >= 10 & NBA$BPM[i] < 20)
{NBA$BPM[i] = 'Average Performance'
}}

for (i in 1:nrow(NBA))
{if (NBA$BPM[i] < 10)
{NBA$BPM[i] = 'Bad Performance'
}}

###PER###
{if (NBA$PER[i] > 20){
  NBA$PER[i] = 'Amazing PER'
}
}
{if (NBA$PER[i] <= 20){
  NBA$PER[i] = 'Normal PER'
}
}

### TS. ###

for (i in 1:nrow(NBA))
{if (NBA$TS.[i] > 0.6)
{NBA$TS.[i] = 'Efficient Scorer'
}}
for (i in 1:nrow(NBA))
{if (NBA$TS.[i] > 0.5 & NBA$TS.[i] <= 0.6)
{NBA$TS.[i] = 'Good Scorer'
}}
for (i in 1:nrow(NBA))
{if (NBA$TS.[i] <= 0.5)
{NBA$TS.[i] = 'Average Scorer'
}}

###TRB.###
for (i in 1:nrow(NBA))
{if (NBA$TRB.[i] > 15)
{NBA$TRB.[i] = 'Insane_Rebounder'
}}

for (i in 1:nrow(NBA))
{if (NBA$TRB.[i] <= 15 & NBA$TRB.[i] > 8)
{NBA$TRB.[i] = 'Great_Rebounder'
}}

for (i in 1:nrow(NBA))
{if (NBA$TRB.[i] >= 8)
{NBA$TRB.[i] = 'Average_Rebounder'
}}

###FG.###
for (i in 1:nrow(NBA))
{if (NBA$FG.[i] > 15)
{NBA$FG.[i] = 'Great Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$FG.[i] <= 15 & NBA$FG.[i] > 8)
{NBA$FG.[i] = 'Good Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$FG.[i] >= 8)
{NBA$FG.[i] = 'Average Scorer'
}}

###FG###
for (i in 1:nrow(NBA))
{if (NBA$FG[i] > 500)
{NBA$FG[i] = 'Great Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$FG[i] < 300 & NBA$FG[i] >= 500)
{NBA$FG[i] = 'Good Scorer'
}}

for (i in 1:nrow(NBA))
{if (NBA$FG[i] >= 300)
{NBA$FG[i] = 'Average Scorer'
}}

###MP###
for (i in 1:nrow(NBA))
{if (NBA$MP[i] > 2500)
{NBA$MP[i] = 'Good player'
}}

for (i in 1:nrow(NBA))
{if (NBA$MP[i] < 1000 & NBA$MP[i] >= 2500)
{NBA$MP[i] = 'Average player'
}}

for (i in 1:nrow(NBA))
{if (NBA$MP[i] >= 1000)
{NBA$MP[i] = 'Bad player'
}}



str(NBA)
NBA$WS = as.factor(NBA$WS)
NBA$PTS = as.factor(NBA$PTS)
NBA$USG. = as.factor(NBA$USG.)
NBA$FT = as.factor(NBA$FT)
NBA$BPM = as.factor(NBA$BPM)
NBA$PER = as.factor(NBA$PER)
NBA$TS. = as.factor(NBA$TS.)
NBA$TRB. = as.factor(NBA$TRB.)
NBA$FG. = as.factor(NBA$FG.)
NBA$FG = as.factor(NBA$FG)
NBA$MP = as.factor(NBA$MP)

#Made sure our allstars were a categorical variable
#data preprocessing/ lessons learned
NBA$All.star = as.factor(NBA$All.star)

#Trained data by removing 2017 data from training set
Score = NBA[(9610:10204),]
NBA = NBA[-(9610:10204),]

Index_2016 = (9045:9609)
Index_2015 = (8380:9044)
Index_2014 = (7770:8379)

training1 = NBA[-(Index_2016),]
validation1 = NBA[Index_2016,]
training2 = NBA[-(Index_2015),]
validation2 = NBA[Index_2015,]
training3 = NBA[-(Index_2014),]
validation3 = NBA[Index_2014,]

library(e1071)
Naivebayes_NBA1 = naiveBayes(All.star ~ WS+PTS+USG.+FT+BPM+PER+TS.+TRB.+FG.+FG+MP,training1)
Naivebayes_NBA1
Naivebayes_NBA2 = naiveBayes(All.star ~ WS+PTS+USG.+FT+BPM+PER+TS.+TRB.+FG.+FG+MP,training2)
Naivebayes_NBA2
Naivebayes_NBA3 = naiveBayes(All.star ~ WS+PTS+USG.+FT+BPM+PER+TS.+TRB.+FG.+FG+MP,training3)
Naivebayes_NBA3

Pred_NBA1 = predict(Naivebayes_NBA1, validation1,type="raw")
Pred_NBA1
Pred_NBA2= predict(Naivebayes_NBA2, validation2,type="raw")
Pred_NBA2
Pred_NBA3 = predict(Naivebayes_NBA3, validation3,type="raw")
Pred_NBA3

library(SDMTools)
confusion.matrix(validation1$All.star,Pred_NBA1[,2],threshold=0.5)
AccuMeasures_full1 = accuracy(validation1$All.star,Pred_NBA1[,2],threshold=0.5)
confusion.matrix(validation2$All.star,Pred_NBA2[,2],threshold=0.5)
AccuMeasures_full2 = accuracy(validation2$All.star,Pred_NBA2[,2],threshold=0.5)
confusion.matrix(validation3$All.star,Pred_NBA3[,2],threshold=0.5)
AccuMeasures_full3 = accuracy(validation3$All.star,Pred_NBA3[,2],threshold=0.5)

library('ROCR')
pred_full = prediction(Pred_NBA1[,2],validation1$All.star)
roc_full = performance(pred_full, "tpr", "fpr")
plot(roc_full, main = "ROC Chart",colorize=TRUE)
lines(x=c(0,1),y=c(0,1),lty=3)