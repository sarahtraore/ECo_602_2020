require(here)
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))
birdhab = merge(dat_bird, dat_habitat, by = c("basin", "sub"))         
dim(birdhab)
head(birdhab)

#Graphical Exploration
plot(BRCR ~ ls, data = birdhab, ylab = "Brow Creeper abundance", 
     xlab = "Late-successional forest extend")
#the relationship looks linear, there is an outlier

#Fit a model
fit_1 = lm(BRCR ~ ls, data = birdhab)
abline(fit_1)
summary(fit_1)
#intercept = BRCR abundance


#Simulator Function
  #Deterministic Model: Linear Function
linear = function(x,y_int,slope)
{ 
  return(y_int + slope *x)
}

linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)

  #Stochastic Model: Normal Distribution


linear_simulator = function(x, y_int, slope, st_dev)
{
  n_pts = length(x)
   
  return(linear(x, y_int, slope) + rnorm(n_pts, 0, st_dev))
}

#Test Your Simulator Function

n = 200

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, 1, 4.5, 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}


n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

#Build the simulation
#Retrieve the model coefficients

fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)
fit_1_summary= summary(fit_1)
str(fit_1_summary)
fit_1_summary$sigma

int_obs = fit_1_coefs[1]
slope_obs = fit_1_coefs[2]
sd_obs = fit_1_summary$sigma

#Choose Predictor Values
#Simulate Data
plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")


plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

#Power analysis for the linear regression model
#single simulation
alpha = 0.05
y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs)
fit_2 = lm(y_sim ~ ls, data = birdhab)
fit_2_summary = summary(fit_2)
#matrix indexing of pvalue
p_value = fit_2_summary$coefficients[2, 4]
#p-value = 4.14e-06 < alpha

#Repeated Simulations
n_sims = 1000
p_vals = numeric(n_sims)
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < 0.05) / n_sims


linear_sim_fit = function(x, y_int, slope, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  return(lm(y_sim ~ x))
}


#Simulating Effect Sizes
alpha = 0.05
n_sims = 10
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    power       = effect_size_powers,
    effect_size = effect_sizes_1)

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = coef(fit_1)[2], lty = 2, col = 'red')

#Simulating Sample Sizes
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

for(j in 1:length(sample_sizes))
{
  x_vals = seq(0, 100, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    power       = sample_size_powers,
    sample_size = sample_sizes)

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')


#reduce the sample size to 20


#Bivariate Power Analysis
#Effect Size and Sample Size
alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 50
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)
sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )
#visualize 2D

image(sim_n_effect_size$power)

#Plotting 3-Dimensional Data
contour(x = sim_n_effect_size$effect_size,
        y = sim_n_effect_size$sample_size,
        z = sim_n_effect_size$power)

#Perspective plot
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#Interactive Plot
install.packages("rgl")
library(rgl)
persp3d( x = sim_n_effect_size$effect_size,
         y = sim_n_effect_size$sample_size,
         z = sim_n_effect_size$power,
         xlab = "beta", ylab = "n", zlab = "power",
         col = 'lightblue',
         theta = 30, phi = 30, expand = .75,
         ticktype = 'detailed')

#saving an interactive plot

rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "n_effect_size_power_sim_plot.html"),
  width = 1200, height = 1200
)

#saving R Data Objects
save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))

#To load the data again
load(file = here::here("data", "lab_11_n_effect_sizes.Rdata"))
###----LAB QUESTIONS----

#Question 01: population dispersion power plot
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)
sd_obs

n_sds = 20
pop_sds = seq(0.1, 0.6, length.out = n_sds)

pop_sd_power = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sd_j
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_power[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = 
  data.frame(
    sd= pop_sds,
    power = pop_sd_power)

require(here)
save(sim_output_dispersion,
     file = here::here("data","lab_11_dat_dispersion_sim.RData"))

png(
  filename = here ("Poplulation dispersal vs plot lab 11.png"),
  width = 1200, height = 1200,
  res = 270, units = "px")
plot(
  power ~ sd, data = sim_output_dispersion,
  type = 'l', xlab = 'Standard deviation', ylab = 'Power', 
  main = "Population dispersion analysis")
abline(v = sd_obs , lty = 2, col = 'red')
dev.off()

#Question 2 : dispersion n contour plot
alpha = 0.05
n_sims = 10
p_vals = numeric(n_sims)
sd_obs

n_sds = 100
pop_sds = seq(0.05, 1.5, length.out = n_sds)

pop_sd_power = numeric(n_sds)

sample_sizes_1 = seq(5, 50)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes_1))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  for(j in 1:length(sample_sizes_1))
  {
    x_vals_1 = seq(0, 50, length.out = sample_sizes_1[j])
    
    for (i in 1:n_sims)
      {
      fit_sim = linear_sim_fit(
      x = x_vals_1,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs)
      
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
    sim_output_3[k,j] = sum(p_vals < alpha) / n_sims
}

  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}

image(sim_output_3)

sim_3_dat = 
  list(
    power       = sim_output_3,
    sample_size = sample_sizes_1,
    pop_sd      = pop_sds)


# You should save your simulation results so you don't have to run it every time
save(
  sim_3_dat, 
  file = here::here("data", "lab_ll_sim_output_dispersion_n_1000.RData"))

#CONTOUR PLOTTING Q2
contour(x = sim_3_dat$pop_sd,
        y = sim_3_dat$sample_size,
        z = sim_3_dat$power)

#Perspective plot
persp(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab = "pop sd", ylab = "n", zlab = "power",
  col = 'green',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#Interactive Plot

library(rgl)
persp3d( x = sim_3_dat$pop_sd,
         y = sim_3_dat$sample_size,
         z = sim_3_dat$power,
         xlab = "pop sd", ylab = "n", zlab = "power",
         col = 'green',
         theta = 30, phi = 30, expand = .75,
         ticktype = 'detailed')

#saving an interactive plot

rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "pop_sample_power_sim_lab11_plot.html"),
  width = 1200, height = 1200)
