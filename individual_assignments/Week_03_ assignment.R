install.packages("here")
require(here)
here()
read.csv(here("data", "bird.sta.csv"))
dat_habitat<- data.frame(read.csv(here("data", "hab.sta.csv")))
dat_habitat

#examine histograms
require(here)
png(
  filename = here ("Histograms_elev_slop_asp.png"),
  width = 1400, height = 800,
  res = 200, units = "px")
par(mfrow = c(1,3))
hist(dat_habitat$slope, main = "Histogram for slope", xlab = "Slope")
hist(dat_habitat$elev, main = "Histogram for elevation", xlab = "elevation")
hist(dat_habitat$aspect, main = "Histogram for aspect", xlab = "aspect")
dev.off()

#create scatterplots
require(here)
png(
  filename = here ("Scatterplot_elev_slop_asp.png"),
  width = 1400, height = 800,
  res = 200, units = "px")
par(mfrow = c(1,3))
plot(x= dat_habitat$slope, y= dat_habitat$ba.tot, pch = 6, main = "Plot basal_area vs slope", xlab = "Slope", ylab="basal area")
plot(x= dat_habitat$elev, y= dat_habitat$ba.tot, pch = 8, main = "Plot basal_area vs elevation", xlab = "elevation", ylab="basal area")
plot(x= dat_habitat$aspect, y= dat_habitat$ba.tot, pch = 1, main = "Plot basal_area vs aspect", xlab = "aspect", ylab="basal area")
dev.off()

# fitting linear functions
# Calculates the value of x for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
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
par(mfrow = c(1,3))
plot(x= dat_habitat$slope, y= dat_habitat$ba.tot, cex = 0.5, pch = 3, main = "Plot basal_area vs slope", xlab = "slope", ylab = "basal area")
curve(
  line_point_slope(x, x1= 40, y1= 20, slope = 0.3), add = TRUE, col = "red", lwd = 2)

plot(x= dat_habitat$elev, y= dat_habitat$ba.tot, cex = 0.5, pch = 6, main = "Plot basal_area vs elevation", xlab = "elevation", ylab = "basal area")
curve(
  line_point_slope(x, x1= 300, y1= 20, slope = 0.05), add = TRUE, col = "green", lwd = 3)

plot(x= dat_habitat$aspect, y= dat_habitat$ba.tot, cex = 0.5, pch = 10, main = "Plot basal_area vs aspect", xlab = "aspect", ylab = "basal area")
curve(
  line_point_slope(x, x1= 300, y1= 50, slope = 0.08), add = TRUE, col = "blue", lwd = 3)


