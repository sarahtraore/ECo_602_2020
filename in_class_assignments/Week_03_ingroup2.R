install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)
class(penguins)
penguins = data.frame(penguins)
penguins
mean(penguins$body_mass_g)
head(penguins)
#na.rm in the mean function allows to remove all the N/A value in the data set
mean(penguins$body_mass_g, na.rm = TRUE)
summary(penguins)
#Graphical exploration
#Boxplot
par(mfrow = c(1,2))
boxplot(penguins$bill_depth_mm, names = "penguins_bill_depth_mm")
boxplot(bill_depth_mm ~ sex, data = penguins)

#coplot
#in that case, we took sex as the conditioning variable
coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
#conditioning variable 2 : year
coplot(body_mass_g ~ bill_depth_mm | year, data = penguins)
#conditioning variable 3 : species
coplot(body_mass_g ~ bill_depth_mm | species, data = penguins)
#conditioning variable 4 : flipper length
coplot(body_mass_g ~ bill_depth_mm | flipper_length_mm, data = penguins)

#Saving the plots
require(here)
png(filename = here("coplotpenguinsspecies.png"), width = 800, height = 600)
coplot(body_mass_g ~ bill_depth_mm | species, data = penguins)
dev.off()

