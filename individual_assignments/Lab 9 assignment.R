require(here)
catrate = read.csv(here("data", "catrate.csv"))
success = sum(catrate$success)
years = sum(catrate$years)
binom.test(success,years)
binom.test(success, years, p = 5/7)
binom.test(success, years, p = 5/7, alternative='less')

veg = read.csv(here("data", "vegdata.csv"), header=TRUE)
head(veg)
boxplot(pine ~ treatment, data = veg)
#Variance test
var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control', 'clipped'))
#the Fisher's test says that these two samples have different variances

#F-tests Assumes Normality
shapiro.test(veg$pine[veg$treatment == "control"])

shapiro.test(veg$pine[veg$treatment=="clipped"])

#Non-parametric Variance Test
fligner.test(pine ~ treatment,
             data = veg,
             subset = treatment %in% c('control','clipped'))
#p-value fligner is < 0.05 so the variances are different for the two samples

bartlett.test(pine ~ treatment, data=veg)

#p-value bartlett is << 0.05 so the variances are different for the two samples

#fligner test for n variances
fligner.test(pine ~ treatment, data = veg)
#the result agree with the bartlett test above

#Comparing two sample means
#T-test
t.test(pine~treatment,data=veg,
       subset=treatment %in% c('control','clipped'), conf.int=TRUE)
#p-value > 0.05 so the means are the same for the two samples
#we can say with confidence that the sample means are different

#Wilcox test
wilcox.test(pine~treatment,data=veg,
            subset=treatment %in% c('control','clipped'), conf.int=TRUE)
#the wilcox test agrees with the t test results
#I have more confidence in the wilcox test

#Tests for paired samples

control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']
t.test(control, clipped, paired=TRUE)
#results differ from the unpaired t test

wilcox.test(control, clipped, paired=TRUE)
#results agree with the unpaired wilcox test
#more confidence in the wilcox test

#correlation

disp = read.csv(here("data", "salamander_dispersal.csv"), header=TRUE)
disp
plot(disp$disp.rate.ftb, disp$disp.rate.eb)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use = 'complete.obs')
#use=’complete.obs’ argument addresses the missing values for the 
#700 m distance class (for which there are no ponds in this 
#particular distance interval).
t.test(disp$disp.rate.ftb, disp$disp.rate.eb)
t.test(disp$disp.rate.ftb, disp$disp.rate.eb, paired = T)
#spearman correlation
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#it does not agree with the t test

#Comparing two distributions
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)

plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

#Kolmogorov-Smirnov test
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)
#p-value > 0.05, reject null hypo
#dispersal distance between the ftb and eb differs

#Comparing two or more proportions
#Sex-linked killing
prop.test(c(4,16), c(40,250))

# significant p-value indicates that the proportions are different between 
#samples;i.e., that the proportions observed were unlikely to have been drawn
#from the same underlying population

prop.test(c(8,32), c(80,500))
#p-value decreases as the sample size increases

#Dependence of variables in a contingency table
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
owls

#Contingency: Chi-square test
chisq.test(owls)
#
fisher.test(owls)

#Bird habitat data
birds   = read.csv(here("data", "bird.sta.csv"), header=TRUE)
hab     = read.csv(here("data", "hab.sta.csv"), header=TRUE)
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and BRCR presence/absence
table (birdhab$s.edge, birdhab$BRCR > 0)
# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

#----QUESTIONS----

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)
head(penguin_dat)
penguins
#Question 1


bartlett.test(body_mass_g ~ species, data = penguins)
#p-value = 0.050

#Question 2: identical to Q1
bartlett.test(body_mass_g ~ sex, data = penguins)
#p-value = 0.032

#Question3
require(palmerpenguins)
par(mar = c(8, 4, 2, 2))
boxplot(
  body_mass_g ~ sex * species,
  data = penguins,
  las = 2, 
  xlab = NULL,
  ylab = "body mass (g)")

dat_groups = aggregate(
  body_mass_g ~ sex * species,
  data = penguins,
  FUN = c)
str(dat_groups)

bartlett.test(dat_groups$body_mass_g)
#p-value = 0.174

#Question 4
#1 null hypothesis
#The interior and edge of forest stands has no impact
#on the presence/absence of BRCR

#There is no variation in the presence/absence of BRCR whether it is
#the interior or the edge of forest stands

# There is no difference in the presence /absence of BRCR in terms 
#of their preference for interior or edge of forest stands
#2
chisq.test(br_creeper_table)
#p-value = 1.386e-06 <0.05 very low, so we reject the null hypothesis
#therefore, I think that Brown Creepers show a significant habitat preference