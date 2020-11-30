require(palmerpenguins)

#1-sample t-test
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)
#null hyp = the mean of the Gentoo species flipper length is 0

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218)
#null hyp = the mean of the Gentoo species flipper length is 218
#p-value = 0.167 >0.05 fail to rreject the null hyp

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less")
#fail to reject the null hyp p-value >0.05

#95% ci = -inf to 218.16

#2-sample t-test

t.test(flipper_length_mm ~ species, 
       data = subset(penguins, species != "Chinstrap"))

t.test(flipper_length_mm ~ species, 
       data = subset(penguins, species != "Chinstrap"),
       mu = mean(subset(penguins, species == "Adelie")$flipper_length_mm, 
                 na.rm =TRUE),
       alternative= "less")

#1- Analysis of Variance (ANOVA)
#Graphical Data exploration
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", 
     xlab = "body mass (g)")

plot(density(penguins$body_mass_g, na.rm = TRUE), 
     main = "density plot of body mass")


hist(subset(penguins, species == "Gentoo")$body_mass_g, breaks = 50, 
     main = "histogram of body mass", 
     xlab = "body mass (g)")
abline(v = mean(subset(penguins, species == "Gentoo")$body_mass_g,
       na.rm = TRUE))
abline(v = median(subset(penguins, species == "Gentoo")$body_mass_g,
                na.rm = TRUE), col = "red")

#The distribution seems normal

boxplot(body_mass_g ~ species, data = penguins)
#We notice that Adelie and chinstrap species have almost the same 
#body_mass. The gentoo is more spread than the other and has a 
#greater body mass

#Numerical Data exploration
dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

shapiro.test(dat_chinstrap$body_mass_g)
#P-value > 0.05 = fail to reject null hypo
#chinstrao data data are drawn from a normally distributed population
shapiro.test(subset(penguins, species == "Gentoo")$body_mass_g)

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)
#One-Way Anova
  #Fit a linear model 
fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

  #Conduct the ANOVA
anova(fit_species)

#Model Coefficients table
#the base case is Adelie

#two-way factorial ANOVA
boxplot(body_mass_g ~ sex * species, data = penguins)
fit_both = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)

anova(fit_both)
#Q1
#the boxplots show that male penguins are heavier than female penguins
#because for all the species the male median body masses are greater than the 
#female

#Q2
#adding sex to a model that already includes species will improve
#the model fit because based on the boxplots, we can notice the difference
#in body mass between the adelie and chinstrap species when we add sex which
#wasn't noticeable before that. Also the spread of the gentoo boxplot with 
#only species is greater than the two others but wth the addition of sex,
#we can notice that the adelie male body mass is even more spread than the
#gentoo male's

#Q3

fit_both = lm(body_mass_g ~ sex * species, data = penguins)

#Q4
#the base case is the Female Adelie

#Q5
# specieschinstrap & intercept
#average mass of female Chinstrap penguins
3368.836 + 158.370

-1.7+10*0.043+30*0.192-20*0.027
