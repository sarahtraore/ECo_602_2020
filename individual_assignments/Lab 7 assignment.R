#APPLY function example
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)
#minimum and maximum values in the rows
apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)
#Mean values in each column
apply(dat, MARGIN = 2, FUN = mean)

require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)
hist(moths$anst)

#A Parametric Confidence Interval
alpha = 0.05
anst = moths$anst
n = sum(!is.na(anst))
t_crit = abs(qt(alpha / 2, df = n - 1))
sse = sd(anst)/sqrt(n)

sample_mean = mean(anst)
ci_parametric = sse * t_crit

confidence_intervals = 
  data.frame(
    technique = c("parametric : t-dist"),
    mean = sample_mean,
    ci_radius = sse *t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )
confidence_intervals

#A Simple Bootstrap Confidence Interval
#1- create the results vector (empty vector)
m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

#2- Perform the bootstrap: create the resampled data set and calcul means
for(i in 1:m)
{
  result[i] = mean(sample(anst, replace=TRUE))
}

#3- Calculate the quantiles
mean(result)
quantile(result, c(0.025, 0.975))

#Bootstrap Interval Using boot()
install.packages("boot")
require(boot)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)
#to check other attributes of the function
str(myboot)
mean(anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)
#extraction of the bootstrap CI
quantile(myboot$t, c(0.025, 0.975))

#Setting up the bootstrap
moth_dat = moths[,-1]
head(moth_dat)
n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

# The outer loop: runs once for each bootstrap iteration.
#index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums
    t2 = apply(t1, 2, sum)
  
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}
head(moth_result)


#Packaging your code into a function: way to facilitate re-use and tinkering
#draft1
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#draft2
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat) #number of rows or sample observations
  
  
  result_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      result_out[i, j] = sum(t2 > 0)
    }
  }
  
  return(result_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#Check in a fresh environment

#Debugging template
#This clears the current R session's environment

rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{

  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    
    for(j in 1:n_input_rows)
    {
    
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      
      t1 = input_dat[rows_j, ]
      
    
      t2 = apply(t1, 2, sum)
      
  
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)
#rarefaction curve
rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type="l",
  xlab="Number of sampling plots",
  ylab="Species richness", 
  main="Rarefaction Curve")

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))


##----Section 2 : Lab questions----

#Question 1: Bootstrap confidence interval
require(palmerpenguins)
alpha_peng = 0.05
gentoo = subset(penguins, species =="Gentoo")

n = sum(!is.na(gentoo$bill_length_mm))

t_crit_1 = abs(qt(alpha_peng / 2, df = n - 1))

sd_gentoo = sd(gentoo$bill_length_mm, na.rm = TRUE)

sse = sd_gentoo/sqrt(n)

sample_mean = mean(gentoo$bill_length_mm, na.rm = TRUE)

ci_parametric = sse * t_crit_1

confidence_intervals = 
  data.frame(
    technique = c("parametric : t-dist"),
    mean = sample_mean,
    ci_radius = sse *t_crit_1,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )
confidence_intervals

#Question 2

m_1 = 10000

# numeric() creates an vector of length m with all values initialized to zero
result_1 = numeric(m_1)
head(result_1)

#2- Perform the bootstrap: create the resampled data set and calcul means
for(i in 1:m_1)
{
  result_1[i] = mean(sample(gentoo$bill_length_mm, replace=TRUE))
}

#Bootstrap Interval Using boot()
install.packages("boot")
require(boot)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = gentoo$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(myboot)

#3- Calculate the quantiles

quantile(myboot$t, c(0.025, 0.975), na.rm = TRUE)

#QUESTION 3 

rm(list = ls())
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    
    for(j in 1:n_input_rows)
    {
      
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      
      t1 = input_dat[rows_j, ]
      
      
      t2 = apply(t1, 2, sum)
      
      
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)

#Question 4
#rarefaction curve
rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type="l",
  xlab="Number of sampling plots",
  ylab="Species richness", 
  main="Rarefaction Curve")

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))



