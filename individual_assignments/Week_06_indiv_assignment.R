#Q1 the probability of observing a count of exactly 3 successes in a binomial 
#distribution with parameters n = 4 and p = 0.75
dbinom(3, 4, 0.75)

#Q2
pbinom(3, 4, 0.75)

#Q3
1-pbinom(3, 5, 0.75)

#Q4:probability of observing a value of less than 1.2 from a 
#normally-distributed population with mean = 2 and standard deviation = 2
pnorm(1.2, mean = 2, sd = 2)

#Q5:probability of observing a value of greater than 1.2 from a 
#normally-distributed population with mean = 2 and standard deviation = 2
1-pnorm(1.2, mean = 2, sd = 2)

#Q6: probability of observing a value between 1.2 and 3.2 

pnorm(3.2, mean = 2, sd = 2) - pnorm(1.2, mean = 2, sd = 2)

#Q7: Sampling distribution
#As I press the sample button, the shape of the histogram changes until
# it become closer to the shape of the distribution.After that, the histogram 
#almost stabilized with slight changes as I continue to sample.widely distribution

#Q8
#As I press the sample button, the shape of the histogram changes until
# it become closer to the shape of the distribution.After that, the histogram 
#almost stabilized with slight changes. however, it represents less variables

#Q9
#The histogram is narrowed. it shows only 5 variable and their frequency 
#as the size has been increased