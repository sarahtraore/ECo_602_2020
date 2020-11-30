ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
#The from and to arguments tells curve() what range of x-values it should 
#include in the plot
#The add = FALSE argument value creates a new plot.

exp_fun = function(x, a, b)
{
  return(a * exp(b * x))
}
curve(
  exp_fun(x, 0.1, 0.5), 
  from = 0, to = 10, add = FALSE, 
  main = "Exponential function",
  ylab = "f(x)", xlab = "x")

# to create a similar code to the exo we put -x in the exp_fun
#Simulated data on a line

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of poitns:
n_pts = 5000
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)
#Choose intercept and slope for the deterministic model
param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred)
#Add some normal errors 1

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed)

#Add some normal errors 2
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)
plot(x_sim, y_observed)

plot(x_sim, y_observed_2)
#The standard deviation error has been multiplied by x_sim
#so as x increases, standard deviation increases

#Exponentially-distributed errors #Y_observed represent the residuals

y_observed_3= 
  y_pred + 
  rexp(n = n_pts, 
    rate = 1.2)
plot(x_sim, y_observed_3)

#CHoosing a model steps
par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)
#step 1 : histograms of residuals
par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")

#------------------------------------------------------------------------------#
#Lab questions on real data

#Question1: exponential plots
#Q1
exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}
#Q2
require(here)
png(
  filename = here ("Exponential curves.png"),
  width = 1200, height = 1200,
  res = 270, units = "px")
curve(
  exp_fun(x, 1.9, 0.1), 
  from = 0, to = 100, add = FALSE, 
  main = "Exponential curves",
  ylab = "f(x)", xlab = "x", col= "black", lty = 1)

curve(
  exp_fun(x, 1.9, 0.3), 
  from = 0, to = 100, add = TRUE, 
  main = "Exponential curve 2",
  ylab = "f(x)", xlab = "x", col= "black", lty = 3)

curve(
  exp_fun(x, 1.2, 0.2), 
  from = 0, to = 100, add = TRUE, 
  main = "Exponential curve 3",
  ylab = "f(x)", xlab = "x", col= "red", lty = 1)

curve(
  exp_fun(x, 1.2, 0.4), 
  from = 0, to = 100, add = TRUE, 
  main = "Exponential curve 4",
  ylab = "f(x)", xlab = "x", col= "red", lty = 3)
dev.off()

#Question 2: Exponential parameters
#As I vary a, the height of the curve varies
#as b varies, the curves the curve moves in the x direction

#Question 3: Ricker plots
ricker_fun = function(x, a, b)
{
  return(a * x * exp(-b * x))
}

png(
  filename = here ("Ricker function curves.png"),
  width = 1200, height = 1200,
  res = 270, units = "px")
curve(
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 150, add = FALSE, 
  main = "Ricker function curves",
  ylab = "f(x)", xlab = "x", col= "black", lty = 1)

curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 150, add = TRUE, 
  main = "Ricker function curve 2",
  ylab = "f(x)", xlab = "x", col= "black", lty = 3)

curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 150, add = TRUE, 
  main = "Ricker function curve 3",
  ylab = "f(x)", xlab = "x", col= "black", lty = 3)

curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 150, add = TRUE, 
  main = "Ricker function curve 4",
  ylab = "f(x)", xlab = "x", col= "red", lty = 1)

curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 150, add = TRUE, 
  main = "Ricker function curve 5",
  ylab = "f(x)", xlab = "x", col= "red", lty = 3)
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 150, add = TRUE, 
  main = "Ricker function curve 6",
  ylab = "f(x)", xlab = "x", col= "red", lty = 3)
dev.off()

#Question 4: Ricker params
# when a varies, the amplitude (height) of the curve varies too
# b influences the width of the curve( decalage)

#Question 5 : salamander data scatterplot 
require(here)
dat_dispersal = read.csv(here("data", "salamander_dispersal.csv"))
png(
  filename = here ("Scatterplot juvenile salamander.png"),
  width = 1200, height = 900,
  res = 180, units = "px")
plot(x= dat_dispersal$dist.class , y = dat_dispersal$disp.rate.ftb , xlab = "class dispersal", ylab = "ftb dispersal rate", 
     main = "Scatterplot Juvenile salamander dispersal")
dev.off()

#interpret

#Question 6: salamander linear model
#Fit a linear deterministic model A terminer

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope)
      return(-(x1 * slope) + y1)
  linear = 
    function(x1, yint, slope)
      return(yint + x * slope)
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
png(
  filename = here ("Linear model first time breeder.png"),
  width = 1200, height = 900,
  res = 180, units = "px")
plot(disp.rate.ftb ~ dist.class , data = dat_dispersal, 
     xlab ="class dispersal" , ylab = "ftb dispersal rate",
     main = "Linear model first time breeder salamander")

curve(line_point_slope(x, x1 = 300, y1= 0.45, slope = -0.0004), add = T, col= "red")
dev.off()

#Why this result?

#Question 7: salamander exponential model

exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
  }
png(
  filename = here ("Exponential model first time breeder.png"),
  width = 1200, height = 900,
  res = 200, units = "px")  
plot(disp.rate.ftb ~ dist.class , data = dat_dispersal,
       xlab ="class dispersal" , ylab ="ftb dispersal rate" ,
       main = "Exponential model first time breeder")
  
curve(exp_fun(x, a = 1.5, b = 0.0027), add = T, col= "blue")
dev.off()

# b impact l'inclinaison, la cuvette de la courbe and a impacts the high

#Question 8 : salamander ricker model

ricker_fun = function(x, a, b)
{
  return(a * x * exp(-b * x))
}
png(
  filename = here ("Ricker model first time breeder.png"),
  width = 1200, height = 900,
  res = 200, units = "px")
plot(disp.rate.ftb ~ dist.class , data = dat_dispersal,
     xlab = "class dispersal", ylab = "ftb dispersal rate",
     main = "Ricker model first time breeder salamander")

curve(ricker_fun(x, a =0.0087, b = 0.0048), from = 0, to = 1500, add = T, col= "green")
dev.off()


#Question 9: salamander linear model residuals
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept =  
    function(x1, y1, slope)
      return(-(x1 * slope) + y1)
  linear = 
    function(x1, yint, slope)
      return(yint + x * slope)
  return(linear(x, get_y_intercept (x1, y1, slope), slope))
}
resids_linear = dat_dispersal$disp.rate.ftb - line_point_slope(x = dat_dispersal$dist.class, x1 = 300, y1 = 0.45, slope = -0.0004)
resids_linear
png(
  filename = here ("histogram of the residuals linear model.png"),
  width = 1200, height = 1200,
  res = 270, units = "px")
hist(x = resids_linear, main = "histogram of the residuals linear model")
dev.off()
#Question 10: salamander exponential model residuals
exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}

resids_exp = (dat_dispersal$disp.rate.ftb) - exp_fun(x = dat_dispersal$dist.class, a = 1.5, b = 0.0027)
resids_exp
png(
  filename = here ("histogram of the residuals exponential model.png"),
  width = 1200, height = 1200,
  res = 270, units = "px")
hist(x = resids_exp, main = " Histogram of the residuals exponential")
dev.off()

#Question 11: salamander ricker model residuals
ricker_fun = function(x, a, b)
{
  return(a * x * exp(-b * x))
}
resids_ricker = dat_dispersal$disp.rate.ftb - ricker_fun(x = dat_dispersal$dist.class, a =0.0087, b = 0.0048)
resids_ricker
png(
  filename = here ("Histogram of the residuals Ricker model.png"),
  width = 1200, height = 1200,
  res = 270, units = "px")
hist(x = resids_ricker, main = " histogram of the residuals Ricker model")
dev.off()

