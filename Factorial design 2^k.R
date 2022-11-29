###Factorial Design###
#A type of experiment that use at least 2 factors to build the model
#One of the most known factorial design is the 2^k factorial design
#In order to understand this, lets take a note on the experimental study regarding
#this design.

##2^2 factorial design##
#Lets say we want to do an experiment where each of our factor has 2 possible values, we denote
#it as + and -. In the case of 2 factors design, we want to explore the combination of 
#4 possible variation between these 2 factors:
library(SixSigma)
des1 = expand.grid(A = gl(2, 1, labels = c("-", "+")),
                   B = gl(2, 1, labels = c("-", "+")))
des1
#The above design is interpreted as combination of values between the 2 factors, and for
#this example we have the 2^2 factorial design.
#In practice, after arranging such combination, the response is obviously will then be measured
#In this example, let's suppose the following simulation's yields are the response
#Randomize the experiment
des1$ord = sample(1:4, 4)
des1[order(des1$ord), ]
#Suppose we do 2 replications
data_des1 = data.frame(repl = rep(1:2, each = 4),
                           rbind(des1))
data_des1
#Add responses
data_des1$response = c(5.33, 6.99,
                       2.26, 6.24,
                       5.13, 6.76,
                       2.79, 2.48)
data_des1
#Now lets comute average score for each experiment design
aggregate(response ~ A + B,
          FUN = mean, data = data_des1)
#If we look at the mean table, we can faintly see a pattern of effect where high value of
#factor A yields high value of response while the factor B seems to have the opposite effect.
#Now lets test the factor significance with ANOVA table
#Get restuls
fac_mod1 = lm(response ~ A + B + A*B, data = data_des1)
summary(aov(fac_mod1))
#Turns out the interaction term is not significance, lets remove it from the model
fac_mod1 = lm(response ~ A + B, data = data_des1)
summary(aov(fac_mod1))
#We can conclude for this example that factor B is the only significant factor.


##2^3 factorial design##
#In general, 2^k factorial design is obviously not limited only for 2 factors.
#Lets say we want to do 2^3 factorial design with 2 replications
des2 = expand.grid(A = gl(2, 1, labels = c("-", "+")),
                   B = gl(2, 1, labels = c("-", "+")),
                   C = gl(2, 1, labels = c("-", "+")))
des2
des2$ord = sample(1:8, 8)
des2[order(des2$ord), ]
data_des2 = data.frame(repl = rep(1:2, each = 8),
                       rbind(des2))
data_des2
#Add responses
data_des2$response = c(5.33, 6.99, 4.23, 6.61,
                       2.26, 5.75, 3.26, 6.24,
                       5.7, 7.71, 5.13, 6.76,
                       2.79, 4.57, 2.48, 6.18)
data_des2
#ANOVA
fac_mod2 = lm(response ~ A * B * C, data = data_des2)
summary(aov(fac_mod2))
#Based on the above table, we got one interaction term significant and 2 individual factor
#are also significant, thus we simplify using linear model selection rule
fac_mod2 = lm(response ~ A + B + C + A:B + A:C + B:C, data = data_des2)#Remove threeway interaction
summary(aov(fac_mod2))
#Since interaction A:B is still not significant, removal is needed
fac_mod2 = lm(response ~ A + B + C + A:C + B:C, data = data_des2)
summary(aov(fac_mod2))
#Thus our factorial design concludes that factor A, B, and C are significantly affecting response
#simultaneously in the second order linear model.


##Visualization of effects##
library(ggplot2)
#For the 2nd factorial model, lets plot the effect of each term in the model for the response

##Individual factor
#Factor A
plot(c(-1,1), ylim = range(1:10),
     coef(fac_mod2)[1] + c(-1,1) * coef(fac_mod2)[2],
     type = "b", pch = 16, ylab = "Response outcome",  xlab = "Factor A value")
abline(h = coef(fac_mod2)[1])
#Factor B
plot(c(-1,1), ylim = range(1:10),
     coef(fac_mod2)[1] + c(-1,1) * coef(fac_mod2)[3],
     type = "b", pch = 16, ylab = "Response outcome",  xlab = "Factor A value")
abline(h = coef(fac_mod2)[1])
#Factor C
plot(c(-1,1), ylim = range(1:10),
     coef(fac_mod2)[1] + c(-1,1) * coef(fac_mod2)[4],
     type = "b", pch = 16, ylab = "Response outcome",  xlab = "Factor A value")
abline(h = coef(fac_mod2)[1])
#Individual factor in a comparison plots
comparative1 = data.frame(Factor = rep(c("A","B", "C"),each = 2),
                          Level = rep(c(-1,1), 3),
                          Response = c(aggregate(response ~ A, FUN = mean, data = data_des2)[,2],
                                  aggregate(response ~ B, FUN = mean, data = data_des2)[,2],
                                  aggregate(response ~ C, FUN = mean, data = data_des2)[,2]))

main_effects = ggplot(comparative1,aes(x = Level, y = Response)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(-1, 1)) +
  facet_grid(. ~ Factor)
main_effects
#Based on the plots above, we can also judge the effect's direction of each factor
#For factor A, the directionis parallel in which when the value of the factor is high,
#the response is expected to be high as well.
#On the contrary, both factors B and C have negative effect in which when the factor value
#is high, the response will be low.

##Interactions
#A and C
inteff = aggregate(response ~ A * C,FUN = mean, data = data_des2)
effects_interaction = ggplot(inteff, aes(x = A, y = response, color = C)) +
  geom_point() + 
  geom_line(aes(group = C))
effects_interaction
#B and C
inteff2 = aggregate(response ~ B * C,FUN = mean, data = data_des2)
effects_interaction2 = ggplot(inteff2, aes(x = B, y = response, color = C)) +
  geom_point() + 
  geom_line(aes(group = C))
effects_interaction2