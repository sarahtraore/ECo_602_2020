#Question1
#The seed predation rates does not change among the species

#Question 2
rm(list = ls())

pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = 210
pol_predation_rate = 26/210
pol_predation_rate

psd_n_predation = 25
psd_n_no_predation = 706
psd_n_total = 731
psd_predation_rate = 25/731
psd_predation_rate

print(
  paste0(
    "The seed predation rate for Polyscias fulva is: ",
    round(pol_predation_rate, digits = 3))) 


print(
  paste0(
    "The seed predation rate for Pseudospondias microcarpa is: ",
    round(psd_predation_rate, digits = 3)))

#Question 3:  ratios of seed proportions.

ratio_seed = pol_predation_rate/psd_predation_rate
ratio_seed

#question 4: inference using a parametric distribution method defines
#parameters that are fixed but unknown and they constitute the basis whereas
#non parametric considers the data fixed and the parameter as random variables

#Question 5
#The Gray Jay bird species prefer habitat with higher elevation than the 
#Brown Creeper

#Question 6 
#The Gray Jay bird species and the Brown Creeper species have different
#preferences of habitat elevation

#Question 7
#As the sample size increases, the standard errors decreases so the confidence 
#interval decreases

#Question 8
#25^3
25^3
