#Creating a dataframe from a link
dat_birds<- data.frame(read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv"))
dat_birds
dat_habitat<- data.frame(read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv"))
dat_habitat
head(dat_habitat)
#creating simple pair plots: subsetting by names, the column names were visualized by the function "head"

pairs(dat_habitat[, c("ba.con", "ba.snag", "ba.tot", "ba.ratio")])

# Creating an histogram:The argument breaks = 0:7 - 0.5 helps us to evaluate how the frequency results for each integer from 0 to 7. Since there are only integers, the range from one halfpoint to another halfpoint will include all the values of the integer.

hist(dat_birds$WIWR , xlab = "Number of birds counted", breaks = 0:7 - 0.5)
#max() function to determine the highest number of counts of your bird species column.
