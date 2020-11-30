

require(here)
dat_delomys = read.csv(here("data", "delomys.csv"))
head(dat_delomys)
View(dat_delomys)

hist(dat_delomys$body_mass)
hist(dat_delomys$body_length)
plot(body_length ~ body_mass, data = dat_delomys,
     main = "Scatterplot of body \n length & body mass")
boxplot(body_mass ~ sex, data = dat_delomys, 
        main = "Conditional boxplot of \n body mass by sex")
boxplot(body_mass ~ binomial, data = dat_delomys, 
        main = "Conditional boxplot of \n body mass grouped by species")
boxplot(body_mass ~ binomial * sex, data = dat_delomys,
        main = "Conditional boxplot of \n body mass grouped by species & sex")


fit_1 = lm(body_mass ~ sex, data = dat_delomys)
summary(fit_1)

fit_2 = lm(body_mass ~ binomial, data = dat_delomys)
summary(fit_2)
