###Analysis of Variance###
#One-way
#A method to determine whether or not three or more group of populations are different
#This method is very popular in the experimental data analysis since the model accomodate the experiment data type and
#the goals.
#to illustrate it, lets use the palmer penguin dataset from library palmer penguin
library(palmerpenguins)
data(package = 'palmerpenguins')
#The library contains two type of penguin data: penguins_raw and penguins
data("penguins")
View(penguins)
#then we can try grouping the data based on the spesies of penguin
library(tidyverse)
penguins %>% 
  count(species)
penguins %>% 
  group_by(species) %>% 
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

#Plot one of the variable into histogram
library(ggplot2)
flipper_hist = ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), 
                 alpha = 0.5, 
                 position = "identity") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Flipper length (mm)",
       y = "Frequency",
       title = "Penguin flipper lengths")

flipper_hist

#Plot 2 variables, body mass and flipper length into scatterplot
ggplot2::theme_set(ggplot2::theme_minimal())
mass_flipper = ggplot(data = penguins, 
                       aes(x = flipper_length_mm,
                           y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

mass_flipper

#Now we prepare the data for ANOVA
dat = penguins %>%
  select(species, flipper_length_mm)
dat = na.omit(dat)
summary(dat)#we'll only keep 50 observations for each spesies
dat1 = dat[dat$species == 'Adelie',]
dat2 = dat[dat$species == 'Chinstrap',]
dat3 = dat[dat$species == 'Gentoo',]
dat = rbind(dat1[1:50,], dat2[1:50,], dat3[1:50,])
summary(dat)

#Plot our data for ANOVA
library(ggplot2)
ggplot(dat) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")

res_aov = aov(flipper_length_mm ~ species,
               data = dat) #Applying ANOVA for penguin data

#Assumption test

#Normality
par(mfrow = c(1, 1))
# histogram
hist(res_aov$residuals)
# QQ-plot
library(car)
qqPlot(res_aov$residuals, id = FALSE)
#Shapiro-Wilk
shapiro.test(res_aov$residuals)

#Homogeneity of Vairance
# Boxplot
boxplot(flipper_length_mm ~ species,data = dat)
# Dotplot
library("lattice")
dotplot(flipper_length_mm ~ species,data = dat)
# Levene's test
library(car)
leveneTest(flipper_length_mm ~ species, data = dat)

#Outlier
boxplot(flipper_length_mm ~ species, data = dat)

#ANOVA
summary(res_aov)
#From the result of above ANOVA, it can be concluded that there is significant effect of species for
#the size of the penguins' flipper. In other words, we can also conclude that there is significant
#difference between the three penguin groups in term of their flippers length.