#Welcome to the ND2 project. This project has a trickster data crocodile that I am attempting to wrangle
#Join me on the adventures of the elusive mitochondrial mutation and the inflation of zeroes as we go through
#The jungle of transformations and the valley of central means. 

#General opening of files and creating a histogram to see normalization. Gather all
#recovery times into one column.

getwd()
setwd("/Users/valeriaaizen/Documents/College- Research")
dir()
read.csv("ND2raw.csv", header = T) ->ND2
head(ND2)

#Selecting only those columns in dataset that are important for analysis and defining as x
(ND2[,7:18]) -> x

#Installing tidyr library
install.packages("tidyr")
library("tidyr")

#Taking selected columns 7:18 and collapsing them into key value pairs using the gather function in tidyr
gather(x) -> y
head(y)
sum(unique(ND2$line)) #Number of unique Drosophila lines present in the dataset

#Tried to see what normalizing the data using just a log would be like using x and y.H3 is created below 

hist(y$value, col = "red", breaks = 20, xlab = "Recovery Time (s)", main = "Histogram")
hist(log(y$value), col = "red", breaks = 20, xlab = "Recovery Time (log s)", main = "Histogram")

#Gathered all flies in the same line into one and replicated original.order, block, tag, N, line, and vial using the reshape function. 
#Defined as h

reshape(ND2, direction = "long", varying = list(names(ND2)[7:18]), v.names = "Recovery Time", 
        idvar = c("original.order","block","tag","line","vial","N"), timevar = "Fly Number", times = 1:12) -> h
class(h)
h$'Recovery Time' 
#Transforming reshaped recovery times in h using a logarithmic transformation and visualizing in a histogram
h$log.rec.time <- log(h$'Recovery Time')
head(h)

hist(h$`Recovery Time`)  #extracted all values from the 'reshaped' h array with recovery time since we don't need the key

table(is.na(h$`Recovery Time`))
h[!is.na(h$`Recovery Time`), ] -> h2 #removed all na values from the dataset in the hopes that boxcox would work better.
#in reality the na values aren't really that important so this dataset is useful for further analysis.
#From here, we will be working with the dataset h2.


#Use the aggregate function for each group to find N
#Full equation: k = log(N)/log(M) (which is what we are searching/adjusting for) 
#and x'=x^((1/log(N)-log(M))) which is the transformed data function

head(h2)
#Decided to group h2 by block by taking the average and creating a new dataset called allAverages
aggregate(h2, by=list(h2$block),FUN = mean) -> allAverages
head(allAverages)

#Exporting new dataset to a csv file with all average N values.
getwd()
write.csv(allAverages, file='Average N values.csv')

#Start calculating log(N) for each group
#Can do this by adding a column to the matrix that is trial

allAverages$logN <- log(allAverages$`Recovery Time`)
head(allAverages)

#k = log(N)/log(M)
#Again can add another separate column for all values of k. Choose M to be equal to 1.5 

log(1.5) 

allAverages$K <- allAverages$logN/log(1.5)

head(allAverages)

#Final calculation. Keep in mind that k for each line will differ, but not M. We are just trying to 
#find the optimal M for all lines and batches. 
#Since we already have varying values of k within a column, we don't have to worry about that now. 

allAverages$inverseK <- 1/allAverages$K #Makes it easier for actually doing the calculation
allAverages[ ,c(3, 13)] -> kays #isolated all k values
head(kays) #visualizing k values
head(h2)
merge(h2, kays, by='block') -> h3 #merging and isolating original columns from h2 dataframe with k values to create new dataframe h3
head(h3)

#Gathered all flies into one line and replicated original.order, block, tag, N, line, and vial

reshape(ND2, direction = "long", varying = list(names(ND2)[7:18]), v.names = "Recovery Time", 
       idvar = c("original.order","block","tag","line","vial","N"), timevar = "Fly Number", times = 1:12) -> h
class(h)
h$'Recovery Time' 
h$log.rec.time <- log(h$'Recovery Time')
head(h)

hist(h$`Recovery Time`)  #extracted all values from the 'reshaped' h array with recovery time since we don't need the key

table(is.na(h$`Recovery Time`))
h[!is.na(h$`Recovery Time`), ] -> h2 #removed all na values from the dataset in the hopes that boxcox would work better.
                                    #in reality the na values aren't really that important so this dataset is useful for further analysis.


#Recovery time vs. transformed recovery time for h3.

h3$recovery.time = h3$`Recovery Time`; #Changing the name of the column so that it's easier
h3$recovery.time=factor(h3$recovery.time) #making recovery.time a factor not numerical
head(h3)

#New Attempt at normalization and inclusion of all values before we go on.
#Aggregating h3 and then scaling the dataframe before anything else happens
#Aggregate so that within each block, there is an average for each line separately, not for every fly individually

aggregate(h3, by = list(h3$line, h3$block), FUN = mean) -> newAttempt
head(newAttempt)
newAttempt <- newAttempt[,-2]
newAttempt <- newAttempt[,-5]
newAttempt <- newAttempt [, -9]
newAttempt <- newAttempt [, -9]
newAttempt <- newAttempt [, -9]
hist(newAttempt$`Recovery Time`)
higherZero <- subset(newAttempt, newAttempt$`Recovery Time` > 0) #These are all the values greater than zero, which allows us to 
                                                                #look at the variability of the values greater than zero rather than just generalizing them
summary(higherZero)
hist(higherZero$`Recovery Time`, breaks = 15)
higherZero$sqrtRecs <- sqrt(higherZero$`Recovery Time`) #A square root transformation to make the 
                                                        #data more normally distributed. The original data was right-skewed and 
                                                        #this helped considerably
hist(higherZero$sqrtRecs)

#Next: Find the proportion of zeroes and non-zeroes within each line WITHIN EACH BLOCK within each line. 
#This would involve looking at each fly looked at for each line within each block
#Based on information in Dalgaard instead (I wrote some notes on it on page 65 of my notebook)
subset(h3, h3$block == 6) -> block6
head(block6)
times.line6 <- table(block6$`Recovery Time`, block6$line)
head(times.line6)
as.data.frame.matrix(times.line6) -> Times.Line6

margin.table(times.line6, 2) ->margin.Time6
margin.Time6

prop.table(times.line6, 2) ->prop.Time6
head(prop.Time6)
as.data.frame.matrix(prop.Time6) -> Prop.Time6 

write.csv(Times.Line6, file = "Times.Line6.csv")
write.csv(Prop.Time6, file = "Prop.Time6.csv")

#block 7
subset(h3, h3$block == 7) -> block7
head(block7)
times.line7 <- table(block7$`Recovery Time`, block7$line)
head(times.line7)
as.data.frame.matrix(times.line7) -> Times.Line7

margin.table(times.line7, 2) ->margin.Time7
margin.Time7

prop.table(times.line7, 2) ->prop.Time7
head(prop.Time7)
as.data.frame.matrix(prop.Time7) -> Prop.Time7 

write.csv(times.line7, file = "Times.Line7.csv")
write.csv(Prop.Time7, file = "Prop.Time7.csv")

#block 8
subset(h3, h3$block == 8) -> block8
head(block8)
times.line8 <- table(block8$`Recovery Time`, block8$line)
head(times.line8)
as.data.frame.matrix(times.line8) -> Times.Line8

margin.table(times.line8, 2) ->margin.Time8
margin.Time8

prop.table(times.line8, 2) ->prop.Time8
head(prop.Time8)
as.data.frame.matrix(prop.Time8) -> Prop.Time8 

write.csv(Times.Line8, file = "Times.Line8.csv")
write.csv(Prop.Time8, file = "Prop.Time8.csv")

#Block 9
subset(h3, h3$block == 9) -> block9
head(block9)
times.line9 <- table(block9$`Recovery Time`, block9$line)
head(times.line9)
as.data.frame.matrix(times.line9) -> Times.Line9

margin.table(times.line9, 2) ->margin.Time9
margin.Time9

prop.table(times.line9, 2) ->prop.Time9
head(prop.Time9)
as.data.frame.matrix(prop.Time9) -> Prop.Time9 

write.csv(Times.Line9, file = "Times.Line9.csv")
write.csv(Prop.Time9, file = "Prop.Time9.csv")

#block 10
subset(h3, h3$block == 10) -> block10
head(block10)
times.line10 <- table(block10$`Recovery Time`, block10$line)
head(times.line10)
as.data.frame.matrix(times.line10) -> Times.Line10

margin.table(times.line10, 2) ->margin.Time10
margin.Time10

prop.table(times.line10, 2) ->prop.Time10
head(prop.Time10)
as.data.frame.matrix(prop.Time10) -> Prop.Time10 

write.csv(Times.Line10, file = "Times.Line10.csv")
write.csv(Prop.Time10, file = "Prop.Time10.csv")

#block 11
subset(h3, h3$block == 11) -> block11
head(block11)
times.line11 <- table(block11$`Recovery Time`, block11$line)
head(times.line11)
as.data.frame.matrix(times.line11) -> Times.Line11

margin.table(times.line11, 2) ->margin.Time11
margin.Time11

prop.table(times.line11, 2) ->prop.Time11
head(prop.Time11)
as.data.frame.matrix(prop.Time11) -> Prop.Time11 

write.csv(Times.Line11, file = "Times.Line11.csv")
write.csv(Prop.Time11, file = "Prop.Time11.csv")

#block 12
subset(h3, h3$block == 12) -> block12
head(block12)
times.line12 <- table(block12$`Recovery Time`, block12$line)
head(times.line12)
as.data.frame.matrix(times.line12) -> Times.Line12

margin.table(times.line12, 2) ->margin.Time12
margin.Time12

prop.table(times.line12, 2) ->prop.Time12
head(prop.Time12)
as.data.frame.matrix(prop.Time12) -> Prop.Time12 

write.csv(Times.Line12, file = "Times.Line12.csv")
write.csv(Prop.Time12, file = "Prop.Time12.csv")

#block 13
subset(h3, h3$block == 13) -> block13
head(block13)
times.line13 <- table(block13$`Recovery Time`, block13$line)
head(times.line13)
as.data.frame.matrix(times.line13) -> Times.Line13

margin.table(times.line13, 2) ->margin.Time13
margin.Time13

prop.table(times.line13, 2) ->prop.Time13
head(prop.Time13)
as.data.frame.matrix(prop.Time13) -> Prop.Time13 

write.csv(Times.Line13, file = "Times.Line13.csv")
write.csv(Prop.Time13, file = "Prop.Time13.csv")

#block 14
subset(h3, h3$block == 14) -> block14
head(block14)
times.line14 <- table(block14$`Recovery Time`, block14$line)
head(times.line14)
as.data.frame.matrix(times.line14) -> Times.Line14

margin.table(times.line14, 2) ->margin.Time14
margin.Time14

prop.table(times.line14, 2) ->prop.Time14
head(prop.Time14)
as.data.frame.matrix(prop.Time14) -> Prop.Time14 

write.csv(Times.Line14, file = "Times.Line14.csv")
write.csv(Prop.Time14, file = "Prop.Time14.csv")

#block 15
subset(h3, h3$block == 15) -> block15
head(block15)
times.line15 <- table(block15$`Recovery Time`, block15$line)
head(times.line15)
as.data.frame.matrix(times.line15) -> Times.Line15

margin.table(times.line15, 2) ->margin.Time15
margin.Time15

prop.table(times.line15, 2) ->prop.Time15
head(prop.Time15)
as.data.frame.matrix(prop.Time15) -> Prop.Time15 

write.csv(Times.Line15, file = "Times.Line15.csv")
write.csv(Prop.Time15, file = "Prop.Time15.csv")

#block 16
subset(h3, h3$block == 16) -> block16
head(block16)
times.line16 <- table(block16$`Recovery Time`, block16$line)
head(times.line16)
as.data.frame.matrix(times.line16) -> Times.Line16

margin.table(times.line16, 2) ->margin.Time16
margin.Time16 
prop.table(times.line16, 2) ->prop.Time16
head(prop.Time16)
as.data.frame.matrix(prop.Time16) -> Prop.Time16 

write.csv(Times.Line16, file = "Times.Line16.csv")
write.csv(Prop.Time16, file = "Prop.Time16.csv")

#block 18
subset(h3, h3$block == 18) -> block18
head(block18)
times.line18 <- table(block18$`Recovery Time`, block18$line)
head(times.line18)
as.data.frame.matrix(times.line18) -> Times.Line18

margin.table(times.line18, 2) ->margin.Time18
margin.Time18

prop.table(times.line18, 2) ->prop.Time18
head(prop.Time18)
as.data.frame.matrix(prop.Time18) -> Prop.Time18 

write.csv(Times.Line18, file = "Times.Line18.csv")
write.csv(Prop.Time18, file = "Prop.Time18.csv")

#Block 19
subset(h3, h3$block == 19) -> block19
head(block19)
times.line19 <- table(block19$`Recovery Time`, block19$line)
head(times.line19)
as.data.frame.matrix(times.line19) -> Times.Line19

margin.table(times.line19, 2) ->margin.Time19
margin.Time19

prop.table(times.line19, 2) ->prop.Time19
head(prop.Time19)
as.data.frame.matrix(prop.Time19) -> Prop.Time19 

write.csv(Times.Line19, file = "Times.Line19.csv")
write.csv(Prop.Time19, file = "Prop.Time19.csv")

#block 20
subset(h3, h3$block == 20) -> block20
head(block20)
times.line20 <- table(block20$`Recovery Time`, block20$line)
head(times.line20)
as.data.frame.matrix(times.line20) -> Times.Line20

margin.table(times.line20, 2) ->margin.Time20
margin.Time20

prop.table(times.line20, 2) ->prop.Time20
head(prop.Time20)
as.data.frame.matrix(prop.Time20) -> Prop.Time20 


write.csv(Times.Line20, file = "Times.Line20.csv")
write.csv(Prop.Time20, file = "Prop.Time20.csv")

#block 21
subset(h3, h3$block == 21) -> block21
head(block21)
times.line21 <- table(block21$`Recovery Time`, block21$line)
head(times.line21)
as.data.frame.matrix(times.line21) -> Times.Line21

margin.table(times.line21, 2) ->margin.Time6
margin.Time21

prop.table(times.line21, 2) ->prop.Time21
head(prop.Time21)
as.data.frame.matrix(prop.Time21) -> Prop.Time21 


write.csv(Times.Line21, file = "Times.Line21.csv")
write.csv(Prop.Time21, file = "Prop.Time21.csv")

#block 22
subset(h3, h3$block == 22) -> block22
head(block22)
times.line22 <- table(block22$`Recovery Time`, block22$line)
head(times.line22)
as.data.frame.matrix(times.line22) -> Times.Line22

margin.table(times.line22, 2) ->margin.Time22
margin.Time22

prop.table(times.line22, 2) ->prop.Time22
head(prop.Time22)
as.data.frame.matrix(prop.Time22) -> Prop.Time22 

#Writing an excel file to check. If the numbers are wrong, just to be safe I will 
#do the proportion calculations to make sure they are right.
write.csv(Times.Line22, file = "Times.Line22.csv")
write.csv(Prop.Time22, file = "Prop.Time22.csv")

#Block 23
subset(h3, h3$block == 23) -> block23
head(block23)
times.line23 <- table(block23$`Recovery Time`, block23$line)
head(times.line23)
as.data.frame.matrix(times.line23) -> Times.Line23

margin.table(times.line23, 2) ->margin.Time23
margin.Time23

prop.table(times.line23, 2) ->prop.Time23
head(prop.Time23)
as.data.frame.matrix(prop.Time23) -> Prop.Time23 #The numbers look iffy. I want to check by doing my own calculations first

#Writing an excel file to check. If the numbers are wrong, just to be safe I will do the proportion calculations in excel because I have no idea how to do it otherwise.
write.csv(Times.Line23, file = "Times.Line23.csv")
write.csv(Prop.Time23, file = "Prop.Time23.csv")

#According to my manaual calculations, these operations seem to be right. 
read.csv("Proportions.Important.csv") -> blockProportions
head(blockProportions) #Where Group.1 is zeroes and group 2 is nonzeroes

#Making a histogram of just the ratio of greater than zero without separating into groups 
hist(blockProportions$Group.2) #looks fairly normal
hist(blockProportions$Group.2, breaks=15)
#Normalizing the frequency distributions of the proportions by using the arcsine of the 
#sqrt of the proportions, which elongates both tails making the distribution of values more akin to a normal
#distribution
blockProportions$angleGroup2 <- asin(sqrt(blockProportions$Group.2))

#graphing the angles or arcsine transformations to see if the distributions are more normalized now. 
hist(blockProportions$angleGroup2) 
hist(blockProportions$angleGroup2, breaks = 15) #seems more elongated rather than most densely distributed towards the center

#Some calculations to see some characteristics of the distribution of the proportions
sd(blockProportions$angleGroup2) #0.2423623
mean((blockProportions$angleGroup2)) #0.8168551

#07/25: Doing  tests to see the variance within and between blocks and decide how to take into account block effect
(summary(aov(blockProportions$Group.2 ~ blockProportions$Line))) #very close to 1
(summary(aov(blockProportions$Group.2~blockProportions$Block))) #for block it is greater than one meaning the variation between groups is greater than within lines
(summary(aov(blockProportions$Group.2~blockProportions$Line+blockProportions$Block))) # same as above

#Next to figure out: Since it is confirmed that we do have block effect, first of all how do we account for that and also
#how do we include more variance in the lines that are greater than zero? 

boxplot(blockProportions$Group.2 ~ blockProportions$Block)
colSums(table(blockProportions$Block, blockProportions$Line)) -> linesBlock
as.data.frame(linesBlock) -> freqBlock
head(linesBlock)
head(linesBlock)
boxplot(blockProportions$Group.2 ~ blockProportions$Line)

#Subsetting the data to only include those lines that were measured more than one time and then looking at 
#the proportions for that
subset(freqBlock, freqBlock$linesBlock >1) -> greaterLines
as.vector(greaterLines) -> greater
head(greater)
write.csv(greaterLines, "greaterLines.csv")
read.csv("greaterLines.csv") -> greaterLines
head(greaterLines)
df[df$Col1 %in% keep, ]
blockProportions[blockProportions$Line %in% greaterLines$X, ] -> replicates
head(replicates)
length(replicates$Group.2)

plot(blockProportions$Block, blockProportions$Group.2, pch = 19, col= 8) #original data
points(replicates$Block, replicates$Group.2, col = 'red', pch=19) # lines with replicates in more than one block only


#Line 338
subset(replicates, replicates$Line == "Ral-338") -> line338
head(line338)

plot(blockProportions$Block, blockProportions$Group.2, pch= 19, col= 8)
points(line338$Block, line338$Group.2, col= "red", pch=19) #Original without the "scaling

library(dplyr)
df_scaled <- blockProportions %>% group_by(blockProportions$Block) %>% mutate(Group.2 = scale(Group.2))
df_scaled2<- blockProportions %>% group_by(blockProportions$Block) %>% mutate(angleGroup2 = scale(angleGroup2))
df_scaled [,-7] -> df_scaled
df_scaled2 [,-7] -> df_scaled2
df_scaled [,-6] -> df_scaled
df_scaled2 [,-6] -> df_scaled2

head(df_scaled2)
head(df_scaled)
hist(df_scaled$Group.2, breaks = 20)
hist(df_scaled2$angleGroup2, breaks = 20)

#Scaling just for one line now
subset(df_scaled, df_scaled$Line == "Ral-338") -> scaled338
head(scaled338)
plot(df_scaled$Block, df_scaled$Group.2, pch=19, col = 8)
points(scaled338$Block, scaled338$Group.2, pch=19, col='red') #they're closer to eachother now...

#Testing for another line just in case : Line 822
subset(df_scaled, df_scaled$Line == "Ral-822") -> scaled822
head(scaled822)
plot(df_scaled$Block, df_scaled$Group.2, pch=19, col = 8)
points(scaled822$Block, scaled822$Group.2, pch=19, col='blue') 

subset(blockProportions, blockProportions$Line == 'Ral-822') -> line822
plot(blockProportions$Block, blockProportions$Group.2, pch= 19, col= 8)
points(line822$Block, line822$Group.2, col= "blue", pch=19) #Original without the "scaling

#The scaling has an effect but we do not know, just by looking at the graphs, if it really is due to
#different blocks. Maybe there is a way to quantitatively tell, with some kind of a test, that the values have
#significantly changed. My first thought is an anova test, but I will look over to see if that is the correct
#choice. 

length(replicates$Group.2)
length(df_scaled$Group.2)

df_scaled[df_scaled$Line %in% greaterLines$X, ] -> scaledRep #Scaled values of only the lines that were replicated
                                                              #In multiple blocks

#Doing an anova test on the scaled, replicated lines to see if the effect of block is reduced upon scaling lines
#And now that we are only looking at those lines that are replicated, we can, with more confidence, say whether
#The difference we see is due to differences in lines between blocks or differences between blocks. Since
#We are only looking at those lines that were replicated in at least 2 blocks (still not fantastic), we can see from
#The results of this anova that the effect of block is removed after scaling and the differences between lines is maintained
#which is what we want

(summary(aov(scaledRep$Group.2~scaledRep$Line+scaledRep$Block))) 

#Results: Lines: F value of 1.750 and p value of 0.0241. Block: F value of 0.045 but p value of 0.8320




#08/08: Are we still going to use the arcsine values for DGRP or the non-arcsine values? Because those are more
#normally distributed rather than the raw values. After that can I go on with GWAS?

hist(df_scaled$Group.2) #looks a little concentrated in the center... are we sure we don't want to use the 
                        #arcsine instead? Or maybe I could try both? Idk..the more normal the distribution is
                        #the more likely we get accurate results back


#I'm going to repeat all of the above of the work but with sqrt arcsine values of the proportions
#First do anova tests and see if I get the same results
(summary(aov(blockProportions$angleGroup2 ~ blockProportions$Line))) #very close to 1
(summary(aov(blockProportions$angleGroup2~blockProportions$Block))) #for block it is greater than one meaning the variation between groups is greater than within lines
(summary(aov(blockProportions$angleGroup2~blockProportions$Line+blockProportions$Block))) # same as above

#The values for the sqrt arcsine values look even more accurate

boxplot(blockProportions$angleGroup2 ~ blockProportions$angleGroup2)
colSums(table(blockProportions$Block, blockProportions$Line)) -> linesBlock
as.data.frame(linesBlock) -> freqBlock
head(linesBlock)
head(linesBlock)
boxplot(blockProportions$angleGroup2 ~ blockProportions$Line)

#Subsetting the data to only include those lines that were measured more than one time and then looking at 
#the proportions for that
subset(freqBlock, freqBlock$linesBlock >1) -> greaterLines
as.vector(greaterLines) -> greater
head(greater)
write.csv(greaterLines, "greaterLines.csv")
read.csv("greaterLines.csv") -> greaterLines
head(greaterLines)
df[df$Col1 %in% keep, ]
blockProportions[blockProportions$Line %in% greaterLines$X, ] -> replicates
head(replicates)


plot(blockProportions$Block, blockProportions$angleGroup2, pch = 19, col= 8) #original data
points(replicates$Block, replicates$angleGroup2, col = 'red', pch=19) # lines with replicates in more than one block only

#Line 338
subset(replicates, replicates$Line == "Ral-338") -> line338
head(line338)

plot(blockProportions$Block, blockProportions$angleGroup2, pch= 19, col= 8)
points(line338$Block, line338$angleGroup2, col= "red", pch=19) #Original without the "scaling

#Scaling just for one line now
subset(df_scaled2, df_scaled2$Line == "Ral-338") -> scaled338
head(scaled338)
plot(df_scaled2$Block, df_scaled2$angleGroup2, pch=19, col = 8)
points(scaled338$Block, scaled338$angleGroup2, pch=19, col='red') #they're closer to eachother now...

#Testing for another line just in case : Line 822
subset(df_scaled2, df_scaled2$Line == "Ral-822") -> scaled822
head(scaled822)
plot(df_scaled2$Block, df_scaled2$angleGroup2, pch=19, col = 8)
points(scaled822$Block, scaled822$angleGroup2, pch=19, col='blue') 

subset(blockProportions, blockProportions$Line == 'Ral-822') -> line822
plot(blockProportions$Block, blockProportions$angleGroup2, pch= 19, col= 8)
points(line822$Block, line822$angleGroup2, col= "blue", pch=19) #Original without the "scaling

#The scaling has an effect but we do not know, just by looking at the graphs, if it really is due to
#different blocks. Maybe there is a way to quantitatively tell, with some kind of a test, that the values have
#significantly changed. My first thought is an anova test, but I will look over to see if that is the correct
#choice. 

length(replicates$angleGroup2)
length(df_scaled2$angleGroup2)

df_scaled2[df_scaled2$Line %in% greaterLines$X, ] -> scaledRep2 #Scaled values of only the lines that were replicated
#In multiple blocks

#Doing an anova test on the scaled, replicated lines to see if the effect of block is reduced upon scaling lines
#And now that we are only looking at those lines that are replicated, we can, with more confidence, say whether
#The difference we see is due to differences in lines between blocks or differences between blocks. Since
#We are only looking at those lines that were replicated in at least 2 blocks (still not fantastic), we can see from
#The results of this anova that the effect of block is removed after scaling and the differences between lines is maintained
#which is what we want

(summary(aov(scaledRep2$angleGroup2~scaledRep2$Line+scaledRep2$Block))) 


#Results: F value for lines is 1.804 with p value of 0.0187. For blocks is 0.086 and p value of 0.7709 (not significant)

hist(df_scaled2$angleGroup2, breaks = 20) # "normalized" with sqrt arcsine transformation
hist(df_scaled2$Group.2, breaks = 20) #not "normalized" with sqrt arcsine transformation
hist(blockProportions2$Group.2, breaks= 20)
hist(blockProportions$angleGroup2, breaks= 20)

#I will submit the data for GWAS that is angleGroup2 first and see what I get back.
#To do so, I will first have to aggregate df_scaled to have a single value for each line, now that we have
#taken into account for block effect - take the means.

aggregate(df_scaled2, by=list(df_scaled2$Line),FUN = mean) -> anglePropMeans
hist(anglePropMeans$angleGroup2, breaks = 20)

write.csv(anglePropMeans, "anglePropMeans.csv")

#What if I tried this just for group.2 without the arsine?
aggregate(df_scaled, by=list(df_scaled$Line),FUN = mean) -> propMeans
hist(propMeans$Group.2, breaks = 20)

write.csv(propMeans, "propMeans.csv")
length(propMeans$Group.2)
length(df_scaled$Group.2)
#####################################################################################################

#08/09: GWAS data analysis for group.2 data
#Previous template code for GWAS analysis that I will use:
#Making a manhattan plot with the data I just got back:
#Use qqman() package to do so:
library(qqman)
getwd()
setwd("/Users/valeriaaizen/Documents/College- Research")
dir()
read.table("gwas.all(3).assoc", header = T, stringsAsFactors = F  ) ->gwasAll #all the possible SNPs associated with the trait value. Almost 2 million!
head(gwasAll)

#Splitting the first column into chromosome, location, and type of mutation
library(stringr)
str_split_fixed(gwasAll$ID, "_", 3) -> split
gwasAll$chrom <- split[,1]
gwasAll$location <- split[,2]
gwasAll$type <- split[,3]

#Making chrom numeric attempt
#> a<-gsub(".", " ", data, fixed=TRUE)
#> a
#[1] "12 57869486" "12 57869582" "12 57870155"

gwasAll$chrom <- as.character(gwasAll$chrom)
gwasAll$chrom[gwasAll$chrom == "2L"] <- "2"
gwasAll$chrom[gwasAll$chrom == "2R"] <- "2"
gwasAll$chrom[gwasAll$chrom == "3L"] <- "3"
gwasAll$chrom[gwasAll$chrom == "3R"] <- "3"

#Replaced Chromosome X with number 1
gwasAll$chrom[gwasAll$chrom == "X"] <- "1"
gwasAll$chrom <- as.numeric(gwasAll$chrom)
gwasAll$location <- as.numeric(gwasAll$location)
gwasAll$negP <- -log10(gwasAll$SinglePval)
write.csv(gwasAll, file = 'gwasAll.csv')


manhattan(gwasAll, chr="chrom", bp="location", p="SinglePval", snp = "ID", 
          suggestiveline = FALSE, genomewideline = FALSE)



#Opening a text file in R
read.delim("gwas.top(2).annot", header = TRUE, sep = " ", dec = ".") -> my_data 

#Trying to figure out if the p-values I got back from GWAS reach genome-wide significance
library(purrr)

#Doing an FDR test to create a better cutoff for my current p-values to use
#Try to use the q value function
?q.value
library(qvalue)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("qvalue")

library(qvalue)
qvalue(gwasAll$SinglePval, fdr.level = 0.2) -> Qvalues
summary(Qvalues) #This FDR test let us know that there is one SNP that is significant based on a parametric test

#################################################################################################

#GWAS analysis for just arcsine data
library(qqman)
getwd()
setwd("/Users/valeriaaizen/Documents/College- Research")
dir()
read.table("gwas.all(4).assoc", header = T, stringsAsFactors = F  ) ->gwasAll2 #all the possible SNPs associated with the trait value. Almost 2 million!
head(gwasAll2)

#Splitting the first column into chromosome, location, and type of mutation
library(stringr)
str_split_fixed(gwasAll2$ID, "_", 3) -> split
gwasAll2$chrom <- split[,1]
gwasAll2$location <- split[,2]
gwasAll2$type <- split[,3]

#Making chrom numeric attempt
#> a<-gsub(".", " ", data, fixed=TRUE)
#> a
#[1] "12 57869486" "12 57869582" "12 57870155"

gwasAll2$chrom <- as.character(gwasAll2$chrom)
gwasAll2$chrom[gwasAll2$chrom == "2L"] <- "2"
gwasAll2$chrom[gwasAll2$chrom == "2R"] <- "2"
gwasAll2$chrom[gwasAll2$chrom == "3L"] <- "3"
gwasAll2$chrom[gwasAll2$chrom == "3R"] <- "3"

#Replaced Chromosome X with number 1
gwasAll2$chrom[gwasAll2$chrom == "X"] <- "1"
gwasAll2$chrom <- as.numeric(gwasAll2$chrom)
gwasAll2$location <- as.numeric(gwasAll2$location)
gwasAll2$negP <- -log10(gwasAll2$SinglePval)
write.csv(gwasAll2, file = 'gwasAll2.csv')


manhattan(gwasAll2, chr="chrom", bp="location", p="SinglePval", snp = "ID", 
          suggestiveline = FALSE, genomewideline = FALSE)



#Opening a text file in R
read.delim("gwas.top(3).annot", header = TRUE, sep = " ", dec = ".") -> my_data 

#Trying to figure out if the p-values I got back from GWAS reach genome-wide significance
library(purrr)

#Doing an FDR test to create a better cutoff for my current p-values to use
#Try to use the q value function
?q.value
library(qvalue)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("qvalue")

library(qvalue)
qvalue(gwasAll2$SinglePval, fdr.level = 0.2) -> Qvalues2
summary(Qvalues2) #This FDR test let us know that there is one SNP that is significant based on a parametric test
hist(df_scaled$Group.2)
hist(df_scaled$angleGroup2)
hist(propMeans$Group.2)
hist(anglePropMeans$angleGroup2,breaks = 20,  main = "Average Recovery Times for Each DGRP Line", xlab = "Recovery Times (Standard Deviations)")

