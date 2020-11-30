#Probability density
dnorm(-1.96, 0, 1)
dnorm(-1, 0, 1)
dnorm(0, 0, 1)
dnorm(1.96, 0, 1)

#Cumulative probability density
pnorm(-1.96, 0, 1)
pnorm(-1, 0, 1)
pnorm(0, 0, 1)
pnorm(1.96, 0, 1)

#Plotting a PDF curve
#generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0) #plot a horizontal line at y=0

#Calculate residuals 
#A residual is the difference between a predicted value and the observed value
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope *x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope)
      return(-(x1 * slope) + y1)
  linear = 
    function(x, yint, slope)
      return(yint + x * slope)
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

#Create the data
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)
#pch means point shape
# I get different numbers because of the condition runif that changes the min and the max each time 

#generate the same sequence
set.seed(1000)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)
#With set.seed, the numbers do not change anymore even when the same code is run twice
#a different seed number changes the values

#Fit a linear deterministic model
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope)
      return(-(x1 * slope) + y1)
  linear = 
    function(x, yint, slope)
      return(yint + x * slope)
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#add predicted values
dat = data.frame(x = x, y_observed = rnorm(n_pts), y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope))
dat

#calculate the residuals
resids = 
dat = data.frame(x = x, y_observed = rnorm(n_pts), y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope), resids = rnorm(n_pts) - y_predicted)
dat
sum(abs(dat$resids)) #abs helps calculte the absolute value
#A good model should have a low sum of residuals

#--------------------------------------------------------------------------------------------#
dev.off()
#LAB QUESTIONS
#Question1
mn = 10.4
sdt = 2.4
norm_17 = rnorm(17, mn, sdt)
norm_30 = rnorm(30, mn, sdt)
norm_300 = rnorm(300, mn, sdt)

#Question2: Histograms
require(here)
png(
  filename = here ("lab_04_hist_01.png"),
  width = 700, height = 1400,
  res = 180, units = "px", bg = "transparent")
par(mfrow = c(3,1))
hist(x = norm_17, main = "Histogram of 17 data points")
hist(x = norm_30, main = "Histogram of 30 data points")
hist(x = norm_300, main = "Histogram of 300 data points")
dev.off()

#Question 3
#explain histograms

#question 4
#density curve 
#generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4 )

plot(x, y, main = "Standard Normal PDF for µ = 10.4 & sd = 2.4", type = "l")
abline(h = 0) #plot a horizontal line at y=0

#save figure
require(here)
png(
  filename = here ("norm_1.png"),
  width = 1200, height = 900,
  res = 120, units = "px")

plot(x, y, main = "Standard Normal PDF for µ = 10.4 & sd = 2.4", type = "l")
dev.off()

#QUESTION5

#variation of seed number 1
#save figure
require(here)
png(
  filename = here ("sim_data_scatterplots.png"),
  width = 1200, height = 900,
  res = 120, units = "px")
par(mfrow = c(2,2))
set.seed(234)
n_pts = 15
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat_df = data.frame(x = x, y_observed = rnorm(n_pts))
plot(y_observed ~ x, data = dat_df, pch = 22, main = "Random 234 set seed")

#variation of seed number 2
set.seed(500)
n_pts = 20
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat_df = data.frame(x = x, y_observed = rnorm(n_pts))
plot(y_observed ~ x, data = dat_df, pch = 24, main = "Random 500 set seed")

#variation of seed number 3
set.seed(200)
n_pts = 25
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat_df = data.frame(x = x, y_observed = rnorm(n_pts))
plot(y_observed ~ x, data = dat_df, pch = 21, main = "Random 200 set seed")

#variation of seed number 4
set.seed(300)
n_pts = 30
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat_df = data.frame(x = x, y_observed = rnorm(n_pts))
plot(y_observed ~ x, data = dat_df, pch = 23, main = "Random 300 set seed")
dev.off()

#Question6
#Fit a linear deterministic model
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope)
      return(-(x1 * slope) + y1)
  linear = 
    function(x, yint, slope)
      return(yint + x * slope)
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
set.seed(300)
n_pts = 30
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat_df = data.frame(x = x, y_observed = rnorm(n_pts))
plot(y_observed ~ x, data = dat_df, pch = 10, main = "Random 300 set seed")

guess_x = 6
guess_y = 0
guess_slope = 0.1


curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T, col= "red")

#Question7
#Predicted values #calculate the residuals
dat_df_1 = data.frame(x = x, y_observed = rnorm(n_pts), y_predicted = line_point_slope(dat_df$x, guess_x, guess_y, guess_slope), residuals = rnorm(n_pts) - y_predicted)
dat_df_1

