#Way to have nicer pairplots
install.packages("psych")
require(psych)
pairs.panels(iris)

install.packages("here")
require(here)
library(here)
#Merging the data
dat_habitat<- data.frame(read.csv(here("data", "hab.sta.csv")))
dat_bird <- data.frame(read.csv(here("data", "bird.sta.csv")))
dat_all = merge(dat_bird, dat_habitat)
dat_all

#creating scatterplots
par(mfrow = c(1,2))
plot(ba.tot ~ elev, data = dat_all)

#convert bird data to presence/absence
sample(dat_all$CEWA, 100)
my_vec = dat_all$CEWA
my_vec
my_vec == 0

#select element where there are more than 1
my_vec > 1

#data type coercion
as.numeric(my_vec > 1)
cewa_present_absent <- my_vec==0
cewa_present_absent
plot(x = dat_all$elev, y = cewa_present_absent)

#fitting a logistic curve
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

#negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

#shallower slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

#-------------------------------------------------------------------------#

#Assignment
dat_all
head(dat_all)

pairs.panels(dat_all[87:98])
dat_all[,1]
dat_all[,"elev"]
dat_all[, c("elev", "slope", "aspect")]

pairs.panels(dat_all[, c("elev", "slope", "aspect")])

#creating pairplots from psych
par(mfrow = c(1,3))
plot_1 = plot(ba.tot ~ elev, data = dat_all)
plot_2 = plot(ba.tot ~ slope, data = dat_all)
plot_3 = plot(ba.tot ~ aspect, data = dat_all)

#convert bird data to presence/absence for two species

my_vec_1 = dat_all$BGWA
my_vec_1
my_vec_1 == 0

my_vec_2 = dat_all$BUSH
my_vec_2
my_vec_2 == 0

#creation plots with prsence/absence
par(mfrow = (1:2))
plot(x = dat_all$ba.tot, y = my_vec_1 == 0 )

plot(x =dat_all$ba.tot , y = my_vec_2 == 0 )


#fitting a logistic curve
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
#BGWA
plot(x = dat_all$ba.tot, y = my_vec_1 == 0, xlab = "ba.tot", ylab = "BGWA species")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = 0.5), add = TRUE, col="red", title("Logistic BGWA species"))

#BUSH
plot(x =dat_all$ba.tot , y = my_vec_2 == 0, xlab = "ba.tot", ylab = "BUSH species") 
curve(logistic_midpoint_slope(x, midpoint = 100, slope = 0.25), add = TRUE, col = "blue", title("Logistic BUSH species"))

#sum grayjays

#total number of sampling sites with GRJA
sum((dat_all$GRJA > 0))

#total number of GRJA

sum(dat_all$GRJA)
    