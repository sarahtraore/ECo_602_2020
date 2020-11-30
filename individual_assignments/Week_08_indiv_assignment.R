##----Walkthrough----

#Catastrophe Rate Data
require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#Numerical and Graphical exploration
summary(catrate)

hist(catrate$cat.rate, main = "Histogram of catastrophe rates",
     xlab = "Catastrophe rate")

#Check for Normality
   #Shapiro test/The null hypothesis for the Shapiro-Wilk test is: “The data
#were sampled from a normally-distributed population”

shapiro.test(catrate$cat.rate)
#p-value < 0.05 so cat.rate is non normal

#One-Sample Tests: Tests for Difference From Expectation

#The t-test
t.test(catrate$cat.rate, mu = 2/7)

#p-value low = reject null hypothesis 

#One-sided Alternative Hypothesis t-test
t.test(catrate$cat.rate, alternative = "greater", mu = 2/7)

#Non-Parametric One-Sample Test: The Wilcoxon Rank Sum Test
wilcox.test(catrate$cat.rate, mu = 2/7, alternative = "greater")

#Comparing two sample means
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)

boxplot(flipper_length_mm ~ species, data = penguin_dat)

#Testing for normality
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

#Parametric and Nonparametric Tests

t.test(flipper_length_mm ~ species, data = penguin_dat)

wilcox.test(flipper_length_mm ~ species, data = penguin_dat)

levels(penguin_dat$species)

##----QUESTIONS----
#question 1 

require(here)
catrate = read.csv(here("data", "catrate.csv"))
hist(catrate$cat.rate , main = "Histogram of salamander reproduction\n catastrophic rates",
     xlab = "Reproduction Catastrophic rate")

#question 2
shapiro.test(catrate$cat.rate)

#3
#Null hypothesis="The data were sampled from a normally-distributed population"

#4
t.test(catrate$cat.rate, mu = 2/7)

#5
#p-value = 0.01193

#6
wilcox.test(catrate$cat.rate, mu = 2/7)

#

#10

par(mfrow = 1:2)
hist(dat_adelie$flipper_length_mm, main = "Histogram of Adelie flipper\n length",
     xlab = "Adelie flipper length")
hist(dat_chinstrap$flipper_length_mm, 
     main = "Histogram of Chinstrap flipper\n length",
     xlab = "Chinstrap flipper length")

#11

t.test(flipper_length_mm ~ species, data = penguin_dat)

