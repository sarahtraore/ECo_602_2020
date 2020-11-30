require(here)
rope = read.csv(here("data","rope.csv"))
rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)

# total number of observations
n_obs = nrow(rope)
n_obs
n_groups = length(levels(rope$rope.type))
n_groups

#Partitioning Variance: Total
#total sum of squares
grand_mean = mean(rope$p.cut, na.rm = TRUE)

ss_tot = sum((rope$p.cut - grand_mean)^2)  


#Partitioning Variance: Within-Group

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x- mean(x)) 
  
str(agg_resids)

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) (sum((x- mean(x))^2)))
str(agg_sq_resids)

ss_within = sum(agg_sq_resids$x) 

#Partitioning Variance: Among Groups
ss_among = ss_tot - ss_within

#Normalizing
  #degree of freedom
df_tot = n_obs-1

df_within = n_obs - n_groups
df_among = n_groups-1

  #Mean squares (constitutes the variance)
ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)

#The Test Statistic: F
f_ratio = ms_among/ms_within

#type 1 error rate(p-value): cumulative prob of F distrib
f_pval = pf(f_ratio, df_among, df_within, lower.tail = FALSE)

#ANOVA in R
fit_1 = lm(p.cut ~ rope.type, data=rope)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$"Sum Sq"

