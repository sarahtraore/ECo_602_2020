dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)
summary(dat_all$WIWA)
sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
sum(dpois(x = wiwa_counts, lambda = 4.5))
sum(dpois(x = wiwa_counts, lambda = 2.3))

#1
sum(log(dpois(x = wiwa_counts, lambda = 4)))

#2
sum(log(dpois(x = dat_all$WIWR, lambda = 1.0)))
hist(dat_all$WIWR, breaks = 5)

sum(log(dpois(x = dat_all$WIWR, lambda = 1.45)))

#3

sum(log(dbinom(x = dat_all$WIWR, size = 6, prob = 0.243)))

#4
