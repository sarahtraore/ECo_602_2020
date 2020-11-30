n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
#creation of vec_2
vec_2= c(vec_1==3, TRUE)
vec_1[vec_2]

#sample allows to take sample from a population. replace is an incrementation, replacing the sample
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1==3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("sum of elements with value 3: ", sum(vec_1 == 3))

#creating a loop
for (i in 1:10) 
{
  print(paste0("This is loop iteration: 1", i))
}
print(i)
#Question4
n = 12
for (n in 1:10) 
{
  print(paste0("This is loop iteration: 1", n))
}
print(n)
#Question 5 : generate a random vector
n = 17
vec_1 = sample.int(n, 10, replace = TRUE)
for (variable in vector) {
  
}


