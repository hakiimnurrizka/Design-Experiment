###Blocking###
#In design experiment study, we always  focus on analyzing the "variability" (hence the name, analysis of variance)
#Many experimental studies involve more than 1 factor and often times the researcher does not desire the
#effect from their experiment variables.
#Take an example from youtube videos of Ethan Chlebowsky: "Are San Marzano Tomatoes actually worth it?"
#In this example, let's note that Ethan is trying to judge the "taste" of canned tomatoes which
#while obviously very subjective, he tried to prevent such subjectivity.
#This can be noted on the taste test #1 and #2 where he tried the tomatoes while "blinding" himself.
#This example should give you a good idea on how "blocking" works and the aim for doing such seemingly
#"common" thing in this occasion.
#The truth is, blocking in design experimental study is as simple as blinding oneself in such taste test.
#We are trying to prevent "unwanted" effects to affect our study.
#The mathematical model for blocking is just as simple as adding another factor, which is considered as
#the "blocking" factor.

#Lets try applying blocking in our one way ANOVA.
#Using the previous palmer penguin data
library(palmerpenguins)
data(package = 'palmerpenguins')
data("penguins")
#We'll use only the "penguins" data
View(penguins)
#The observation that has been conducted to get this "penguins" data is done in several islands
#Assume that geographical location may affect the physiological of these penguins
library(dplyr)
penguins %>% 
  group_by(island) %>% 
  summarize(across(where(is.numeric), mean, na.rm = TRUE))
#As it can be seen from grouping above, the mean size of penguins in Biscoe island does seem to be different
#compared to the other 2 islands. While this may also be affected by the species that mostly live in the
#corresponding island, a skeptical approach should always be applied.
#Lets use island as our block for ANOVA in penguins data, we'll use bill_depth as the response variable
blk_aov = aov(bill_depth_mm~species+island, data = penguins)
summary(blk_aov)
#Lets compare the above ANOVA table and one way ANOVA table that only consider species as the single factor
aov1 = aov(bill_depth_mm~species, data = penguins)
summary(aov1)
#From the illustration above, there is no significant difference between the two ANOVA table but
#we can learn several things from that:
#1)Blocking reduce variability that is not explained (residual SS)
#2)Blocking prevent some part of "unexplained" to become hindrance for significance test, that is
#by reducing residual SS, blocking can change the significance test of the main effect.
#3)Block effect/factor by itself is not necessary to be significant, that is again the function is to
#reduce the unexplained in the residual which usually translates into "unwanted" affecting variables.

###Latin square###
#A more specific case of blocking is known as "latin square".
#This concept of blocking can be imagined as "2 dimensional blocking", that is we do 2 layer of blocking.
#The oldest application can be found in agriculture where the researcher trying to block the effect of
#"row" and "collumn" of the field.
#The rule in which the seed be planted in this segmented field is similar to the game of sudoku (one type
#can only exist once for each collumn and row).
#The rule in which latin square is designed is also widely used as scheduling method in many sports event
#Lets try to apply latin square in the following sample data
rows = c(rep("r1",1), rep("r2",1), rep("r3",1), rep("r4",1), rep("r5",1))
cols = c(rep("cA",5), rep("cB",5), rep("cC",5), rep("cD",5), rep("cE",5))
treatments = c("A","C","D","E","B", "D","B","C","A","E", "E","A","B","C","D", "B","E","A","D","C", "C","D","E","B","A")
yield = c(7.4,11.8,10.1,8.8,11.8,8.9,6.5,17.9,10.1,8.8,5.8,8.7,9.0,15.7,14.3,12.0,7.6,8.5,11.1,18.4,14.3,7.9,7.1,7.4,10.1)
y = data.frame(cols,rows,treatments, yield)
matrix(y$treatments, 5,5)#The latin square
ltn_aov = aov(yield ~ cols+rows+treatments, data = y)
summary(ltn_aov)

##Graeco Latin Square##
#an improved version of latin square, in simplest explanation, this design is just imposing latin square 
#on top of latin square where the two latin squares are mutually orthogonal.
#To give a more approachable example, take an example of the following situation:
#a car company is trying to reduce their total cost of maintenance by making sure that they provide
#a more efficient maintenance for the tires of their cars. To study the effect of tires rotation, 
#the company observed from 4 different location based on the general weather characteristics (which translates
#into type of tires). The manufacturer also needs to ensure the maintanance can be applied for all the model
#of their released car. Lastly, the manufacturer also want to provide the best possible service without
#additional cost, that is by comparing the tire brand.
#If there is no significant difference in thread wears between the front and rear tires within the 10,000 miles, 
#then the tire manufacturer can claim that their tires do not need to be rotated before 10,000 miles. 
library(agricolae)
pst = c(rep("1",1), rep("2",1), rep("3",1), rep("4",1))
brnd = c(rep("A",1), rep("B",1), rep("C",1), rep("D",1))
tire = c(6, 16, 29, 18, 23, 15, 5, 25, 20, 4, 7, 16, 15, 19, 26, 15)
grec_design = design.graeco(pst, brnd, seed = 11)
grec_design$sketch
tire = data.frame(cbind(grec_design$book, tire))
grec_aov = aov(tire~row+col+pst+brnd, data = tire)
summary(grec_aov)
#From the result above, it can be concluded that position of the tire does have significant effects toward
#the tire endurance.