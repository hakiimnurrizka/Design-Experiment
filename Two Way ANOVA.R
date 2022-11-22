###Two way ANOVA###
#ANOVA in which we actually consider 2nd factor to be variable experiment (not something that only helps
#in the analysis like the block factor).
#Lets go back  to the previous penguins data
library(palmerpenguins)
data(package = 'palmerpenguins')
data("penguins")
#Lets consider "sex" as the 2nd factor that explain the body mass of penguins
library(dplyr)
na.omit(penguins) %>% 
  group_by(sex) %>% 
  summarize(across(where(is.numeric), mean, na.rm = TRUE))
#A clear difference between male and female body mass
#A two way ANOVA will do analysis of whether species and sex of the penguins affect their body mass.
tw_aov = aov(body_mass_g ~ species + sex, data = penguins)
summary(tw_aov)
#Interaction between factors are sometimes also within interest. This interaction term is interpreted as
#combination/composite effect between factors
tw_aov2 = aov(body_mass_g ~ species * sex, data = penguins)
summary(tw_aov2)
#the interpretation from above ANOVA on the interaction term (species:sex) is that both species and sex of 
#penguins are significantly defining the body mass of penguins simultaneously.
#It can also be said as the association between body mass and species depends on the sex of the penguin.

###3 Types of ANOVA###
#Until just before this, we always consider a "balanced design" that is for each group/factor the number
#of observation is equal (even if it was not, we have proceeded with such assumption).
#In practice, this assumption is more often not applicable due to the nature of experimental data and
#their availibility. In the case of one way ANOVA, one can easily proceed if the homogeneity assumption
#is hold or else perform a nonparametric approach.
#For two way and above, one needs more caution to use ANOVA in the case of unbalanced design.
#The most notable approach is by defining the type of sum squared, that is : type I, II, and III SS.

#1) Type I: sequential sum square, that is we first test main effect of factor A [SS(A)] then relative of
#main effect A, we test for main effect B[SS(B|A)] lastly we analyze the interaction term relative
#the previous 2 main effects [SS(AB | B, A)].
#For this type, Because of the sequential nature and the fact that the two main factors are tested in a particular order, 
#it will give different results for unbalanced data depending on which main effect is considered first. 

#2) Type II: testing main effect after the other, that is SS(A|B) for main effect A and SS(B|A) for main
#effect B. Interaction is not assumed in this type and If there is indeed no interaction, then type II is 
#statistically more powerful than type III.

#3) Type III: testing main effect relative to other main effect and their interaction, that is SS(A | AB, B)
#for main effect A and SS(B | AB, A) for main effect B.
#However, it is often not interesting to interpret a main effect if interactions are present (generally speaking, 
#if a significant interaction is present, the main effects should not be further analysed).

#Now lets compute each type of SS in the case of the previous two way ANOVA for the penguins data
#Type I SS
anova(lm(body_mass_g ~ species * sex, data = penguins))
anova(lm(body_mass_g ~ sex * species, data = penguins))
#Type II SS
#Extract from 2nd line of output from 2 codes above, that is for sex extract from 2nd line of
anova(lm(body_mass_g ~ species * sex, data = penguins))
#while for species extract from 2nd line of
anova(lm(body_mass_g ~ sex * species, data = penguins))
#Type III SS
library(car)
Anova(lm(body_mass_g ~ species * sex, data = penguins, contrasts=list(species=contr.sum, sex = contr.sum)), 
      type="III")

##Response Plot##
library(ggpubr)
penguins = na.omit(penguins)
ggline(penguins, x = "species", y = "body_mass_g", color = "sex",
       add = c("mean_se"))
#Interaction plot
interaction.plot(x.factor = penguins$species, trace.factor = penguins$sex,
                 response = penguins$body_mass_g, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Species", ylab="Body Mass (g)",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))