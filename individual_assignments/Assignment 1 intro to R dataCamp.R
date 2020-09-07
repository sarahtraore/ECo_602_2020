#create variables
a<- "sarah"
b1 <- 45.6
class(b1)
b2<- "45.6"
c<- c(0, 1, 2, 3)
class(c)
class(a)
class(b1)
class(b2)
b1 + b2
class(c)
b1 + c
b1+c
c + b2
#create a vector v1
v1 <- c(-2, -1, 0, 1, 2)
v1
#create a vector v2
v2 <- v1 * 3
v2
#calculate the sum of elements in v2
total2 <- sum(v2)
total2

#create a list
my_list_1 <- list(5.2, "five point two", 0:5)
names(my_list_1) <- c("two", "one", "three")
my_list_1

#subsetting
my_list_1[3]
my_list_1$three
my_list_1["three"]
my_list_1["one"]
my_list_1[2]
my_list_1$one

#build a vector my_vec: my_vec = rep (,n) is a repetition of the value n times
my_vec=rep(1:3, 5)
my_vec == 3
my_bool_vec<- my_vec == 3
my_bool_vec
data.frame(my_vec, my_bool_vec)

#retrieve elements
my_vec[my_bool_vec]
        