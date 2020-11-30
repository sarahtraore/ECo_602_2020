require(palmerpenguins)
penguins<-penguins
sx = penguins$bill_depth_mm
#Formula for the standard error of the mean
#is.na function allows to see missing data (NA)
sse_mean = function(sx)
{
  n = length(sx) - length(sx[is.na(sx)])
  standard_dev = sd(sx,na.rm = TRUE)

  return(standard_dev/sqrt(n))
}
sse_mean(penguins$bill_depth_mm)

#The penguin data
boxplot(flipper_length_mm ~ species, data = penguins)
#2-species data : removal of one species
dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)
#droplevels():remove unused factor levels from a data.frame
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}
#Resampling
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

#two-sample t-test: help compare the mean values of two groups
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
#two-sample resampling
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1


#----section2 : LAB QUESTIONS----
#Question 1 : Standard error of the mean function
require(palmerpenguins)
sse_mean = function(sx)
{
  n = length(sx) - length(sx[is.na(sx)])
  standard_dev = sd(sx,na.rm = TRUE)
  
  return(standard_dev/sqrt(n))
}
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

summary(penguins$species)

#Question 2: define my resampling function
two_group_resample = function(x, n_1, n_2)
{
  x= dat_pen$flipper_length_mm
  n_1 = 45
  n_2 = 125

  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
 
  difference_in_means = mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  
  return(difference_in_means)
}

set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 45, 125)

#Question 3 : resampling histogram
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
t_test$estimate
diff_means_obs = round(diff(t_test$estimate), digits = 3)
diff_means_obs

n = 2000
mean_differences = c()
for (i in 1: n) 
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 45, 125)
  )
  
}
hist(mean_differences)

#Question4
sum(abs(mean_differences) >= diff_means_obs)
sum(abs(mean_differences) >= 5.8)

#Question 5: How many simulations

#Question 6 

#1
boxplot(bill_length_mm ~ species, data = dat_pen, 
        main = "Boxplot bill_length ~species")
#2
aggr_means = aggregate(
  bill_length_mm ~ species,
  data = dat_pen,
  FUN = mean,
  na.rm = TRUE)
diff_crit = diff(aggr_means[,2])

aggr_means
diff_crit
#3
t_test_bill = t.test(bill_length_mm ~ species, data = dat_pen)
t_test_bill
#p-value < 2.2e-16
#4

n = 1000
mean_differences_2 = c()
for (i in 1: n) 
{
  mean_differences_2 = c(
    mean_differences_2,
    two_group_resample(dat_pen$bill_length_mm, 45, 125))
}

#Question 7: interpret the p-value 
#p-value < 2.2e-16

#Question 8

sum(abs(mean_differences_2) >= diff_crit)

hist(mean_differences_2, 
     main = "Histogram of 1000 resamplings simulation results")  

