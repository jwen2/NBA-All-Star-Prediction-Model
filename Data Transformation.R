### Load the Dataset ###

NBA = read.csv("Team.csv")

### Transform the Data ###

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


### TS ###

for (i in 1:nrow(NBA))
{if (NBA$TS[i] > 70)
{NBA$TS[i] = 'Starter'
}}

### TRB ###

for (i in 1:nrow(NBA))
{if (NBA$TRB[i] > 15)
{NBA$TRB[i] = 'Insane_Rebounder'
}}

for (i in 1:nrow(NBA))
{if (NBA$TRB[i] <= 15 & NBA$TRB[i] > 8)
{NBA$TRB[i] = 'Great_Rebounder'
}}

for (i in 1:nrow(NBA))
{if (NBA$TRB[i] >= 8)
{NBA$TRB[i] = 'Average_Rebounder'
}}

### AST ###

for (i in 1:nrow(NBA))
{if (NBA$AST[i] > 10)
{NBA$AST[i] = 'Great_Facilitator'
}}

for (i in 1:nrow(NBA))
{if (NBA$AST[i] <= 9 & NBA$AST[i] > 5)
{NBA$AST[i] = 'Good_Facilitator'
}}

for (i in 1:nrow(NBA))
{if (NBA$AST[i] < 5)
{NBA$AST[i] = 'Average_Facilitator'
}}

### STL ###

for (i in 1:nrow(NBA))
{if (NBA$STL[i] >= 1)
{NBA$GS[i] = 'Good_Stealer'
}}

for (i in 1:nrow(NBA))
{if (NBA$GS[i] < 1)
{NBA$GS[i] = 'Average_Stealer'
}}

### BLK ###

for (i in 1:nrow(NBA))
{if (NBA$BLK[i] >= 2)
{NBA$BLK[i] = 'Good_Defender'
}}

for (i in 1:nrow(NBA))
{if (NBA$BLK[i] < 2)
{NBA$BLK[i] = 'Average_Defender'
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

