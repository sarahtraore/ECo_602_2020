#----Lab8 walkthrough----
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")
#bootstrap two-sample test
install.packages("simpleboot")
require(simpleboot)
adelie = subset(penguin_dat, species == "Adelie")
chinstrap = subset(penguin_dat, species == "Chinstrap")
mytwoboot = two.boot(
  adelie$flipper_length_mm, 
  chinstrap$flipper_length_mm,
  FUN = mean,
  R = 10000,
  na.rm = TRUE)
print(mytwoboot)

hist(mytwoboot, main = "Histogram of 10000 bootstrap\n in mean penguin flipper length",
     xlab = "Difference in mean flipper length (mm)\n Adelie and Chinstrap Penguins")

#Tree data
require(here)
veg = read.csv(here("data", "vegdata.csv"))
boxplot(pine ~ treatment, dat = veg)

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)

table(dat_tree[,4])

#non parametric two sample test
tree_boot = two.boot(
  subset(dat_tree, treatment == "clipped")$pine,
  subset(dat_tree, treatment == "control")$pine,
  FUN = mean,
  R = 10000,
  na.rm = TRUE
  )
# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)
tree_boot$t

hist(tree_boot$t)
quantile(tree_boot$t, 0.025)

#Resampling: linear regression
#bird data
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))
#model variables

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#Simple Linear Regression
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
#add regression line to the plot
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#The Slope Coefficient
#extraction of two variables needed
dat_1 = subset(
  dat_all, select = c(b.sidi, s.sidi))
#resampling the data: Monte Carlo randomization
# create two vectors of randomly generated row indices
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = data.frame(
  b.sidi = dat_1$b.sidi[index_1],
  s.sidi = dat_1$s.sidi[index_2]
)
fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]
print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#Randomization Loop
m = 10000 
result = numeric(m) 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2] 
  
  result[i] = coef(fit_resampled_i)[2]
} 
#The null distribution
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

#critical slope value
quantile(result, c(.05))

#----Lab Questions----
#Question1
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

#bootstrap two-sample test
install.packages("simpleboot")
require(simpleboot)
adelie = subset(penguin_dat, species == "Adelie")
chinstrap = subset(penguin_dat, species == "Chinstrap")
pen_boot = two.boot(
  adelie$flipper_length_mm, 
  chinstrap$flipper_length_mm,
  FUN = mean,
  R = 10000,
  na.rm = TRUE)

print(pen_boot$t)
str(pen_boot)

hist(pen_boot$t, main = "Histogram of 10000 bootstrap\n in mean penguin flipper length",
     xlab = "Difference in mean flipper length (mm)\n Adelie and Chinstrap Penguins")

#Question 2 
quantile(pen_boot$t, c(0.975,0.025))

#question 3
pen_ecdf = ecdf(pen_boot$t)

#Question 4
#There is no difference in mean flipper length between 
#the two two penguin species

#question 5
pen_ecdf(-4.5)

pen_ecdf(0)

#Question 6
require(here)
veg = read.csv(here("data", "vegdata.csv"))

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

wilcox.test(pine ~ treatment, data = dat_tree, alternative = "two.sided")

# p-value = 0.1005

#Question 7 
tree_boot = two.boot(
  subset(dat_tree, treatment == "clipped")$pine,
  subset(dat_tree, treatment == "control")$pine, 
  FUN = mean,
  R= 10000,
  na.rm=TRUE)


quantile(tree_boot$t, c(0.975, 0.025))

# 97.5%   2.5% 
#29.750  4.125 

#Other way to find bootstrap CI
install.packages(boot)
require(boot)
boot.ci(tree_boot)

#difference in means
diff_mean = mean(subset(dat_tree, treatment == "clipped")$pine)- 
  mean(subset(dat_tree, treatment == "control")$pine)

diff_mean
#Difference in mean falls into the 95% bootstrap confidence interval

#Question 8

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))


#Simple Linear Regression

dat_loop = subset(
  dat_all, select = c(b.sidi, s.sidi))

m = 10000 
result_1 = numeric(m) 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_loop), replace = TRUE)
  index_2 = sample(nrow(dat_loop), replace = TRUE)
  
  dat_resampled_i = data.frame(
    b.sidi = dat_loop$b.sidi[index_1],
    s.sidi = dat_loop$s.sidi[index_2]
  )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2] 
  
  result_1[i] = coef(fit_resampled_i)[2]
} 

#Question 9

Crit_value = quantile(result_1, 0.05)

hist(result_1, main = "Histogram of MC simulated Slope", 
     xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v = Crit_value, lty = 2, col = "red", lwd = 2)

#Question 10

# crit value = -0.013
# observed slope less than crit value

