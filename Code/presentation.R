
merge<-read.csv("merge.csv", stringsAsFactors = F)



cor(merge$mort, merge$prop_safe)
#R=-0.5299896
cor(merge$mort, merge$prop_safe)^2
#R^2=0.280889
plot(merge$mort~merge$prop_safe,
    col="blue",
    pch=16,
    xlab="Proportion of Safe Drinking Water",
    ylab="Mortality",
    main="Mortality vs. Safe Drinking Water"
    )

mergereg3<-lm(mort~prop_safe, data=merge)
abline(mergereg3, col="red", lwd=2)
coef(mergereg3)
#(Intercept)   prop_safe 
#29.786593   -0.331065 

plot(residuals(mergereg3)~predict(mergereg3))
hist(residuals(mergereg3),
     xlim=c(-40,60))
qqnorm(residuals(mergereg3))


merge<-merge[merge$mort>0,]

cor(log(merge$mort), merge$prop_safe)
#R=-0.7161818
cor(log(merge$mort), merge$prop_safe)^2
#R^2=0.5129163
plot(log(merge$mort)~merge$prop_safe,
     col="blue",
     pch=16,
     xlab="Proportion of Safe Drinking Water",
     ylab="log(Mortality)",
     main="log(Mortality) vs. Safe Drinking Water"
)

mergereg3<-lm(log(mort)~prop_safe, data=merge)
abline(mergereg3, col="red", lwd=2)
coef(mergereg3)
#(Intercept)   prop_safe 
#4.02750431 -0.05471174 

par(mfrow=c(1,3))
plot(residuals(mergereg3)~predict(mergereg3),
     col="yellow",main="Plot of Residuals vs Predicted values")
hist(residuals(mergereg3),
     xlim=c(-5,5),
     col="yellow",
     main="Histogram of Residuals")
qqnorm(residuals(mergereg3), pch=16)




merge<-merge[merge$mort>0,]
par(mfrow=c(1,1))
trials <- 47

data_example <- data.frame(slp = numeric(trials), intercept = numeric(trials))

for(i in 1:trials){
  sampled <- sample(x = (merge$uhc), size = trials, replace = F)
  regress <- coef(lm(sampled ~ merge$uhc))
  data_example[i, 1] <- regress[2]
  data_example[i, 2] <- regress[1]
}

hist(data_example$slp)

mean(data_example$slp)

par(mfrow=c(1,1))


#plot #1: uhc^2 and log(mort)
#initial slope=-511
trials <- 1000
data_example <- data.frame(slp = numeric(trials), intercept = numeric(trials))

for(i in 1:trials){
  regress<-coef(lm(sample(merge$uhc^2)~log(merge$mort)))
  data_example[i, 1] <- regress[2]
  data_example[i, 2] <- regress[1]
}
hist(data_example$slp)

mean(data_example$slp)
# -2.132044 -- near 0
min(data_example$slp)
#-323.5528, which is way greater than -511 (our original slope)
#that means our slope is statistically significant, since, after 1000 randomized trials,
#every slope was bigger than our original one


#plot #2: log(prop_safe) and uhc^2
#0.02966
trials <- 1000
data_example <- data.frame(slp = numeric(trials), intercept = numeric(trials))

for(i in 1:trials){
  regress<-coef(lm(sample(log(merge$prop_safe))~merge$uhc^2))
  data_example[i, 1] <- regress[2]
  data_example[i, 2] <- regress[1]
}
hist(data_example$slp,
     main="Histogram of Slopes after randomized trials",
     xlab="Slope Values")

mean(data_example$slp)
# -0.0002022526 --- very near 0!
max(data_example$slp)
#0.02233104 < 0.02966
#that means our slope is statistically significant, since, after 1000 randomized trials,
#every slope was smaller than our original one





#plot #3: log(prop_safe) and log(mort)
#-0.18446
trials <- 1000
data_example <- data.frame(slp = numeric(trials), intercept = numeric(trials))

for(i in 1:trials){
  regress<-coef(lm(sample(log(merge$prop_safe))~log(merge$mort)))
  data_example[i, 1] <- regress[2]
  data_example[i, 2] <- regress[1]
}
hist(data_example$slp,
     main="Histogram of Slopes after randomized trials",
     xlab="Slope Values")

mean(data_example$slp)
# -0.0004135188 --- near 0
min(data_example$slp)
# -0.1286337>-0.18446
#that means our slope is statistically significant, since, after 1000 randomized trials,
#every slope was bigger than the original one


#plot #4: log(total_dev) and log(mort)
#0.16631
trials <- 1000
data_example <- data.frame(slp = numeric(trials), intercept = numeric(trials))

for(i in 1:trials){
  regress<-coef(lm(sample(log(merge$total_dev))~log(merge$mort)))
  data_example[i, 1] <- regress[2]
  data_example[i, 2] <- regress[1]
}
hist(data_example$slp,
     main="Histogram of Slopes after randomized trials",
     xlab="Slope Values")

mean(data_example$slp)
# 0.0001166466 --- near 0
max(data_example$slp)
# 0.4999
#that means our slope is not statistically significant, since, after 1000 randomized trials,
#we had slope values that were waay bigger than our initial slope
sum(data_example$slp>0.16631)/nrow(data_example)*100
#13% of values were bigger than our initial slope




#plot #5: log(total_dev) and uhc^2
# -0.039909
trials <- 1000
data_example <- data.frame(slp = numeric(trials), intercept = numeric(trials))

for(i in 1:trials){
  regress<-coef(lm(sample(log(merge$total_dev))~merge$uhc^2))
  data_example[i, 1] <- regress[2]
  data_example[i, 2] <- regress[1]
}
hist(data_example$slp,
     main="Histogram of Slopes after randomized trials",
     xlab="Slope Values")

mean(data_example$slp)
# 0.0007682372 --- near 0
min(data_example$slp)
# -0.08334476 < -0.039909
#that means our slope is NOT statistically significant, since, after 1000 randomized trials,
#there are slope values smaller than ours
sum(-data_example$slp>0.039909)/nrow(data_example)*100
#6.3% of the trials yield smaller slopes that the initial one






#plot #6: log(prop_safe) and log(total_dev)
#-1.0593
trials <- 1000
data_example <- data.frame(slp = numeric(trials), intercept = numeric(trials))

for(i in 1:trials){
  regress<-coef(lm(sample(log(merge$prop_safe))~log(merge$total_dev)))
  data_example[i, 1] <- regress[2]
  data_example[i, 2] <- regress[1]
}
hist(data_example$slp,
     main="Histogram of Slopes after randomized trials",
     xlab="Slope Values")

mean(data_example$slp)
# -0.002215208 --- near 0
min(data_example$slp)
# -0.1142829 > -1.0593
#that means our slope is statistically significant, since, after 1000 randomized trials,
#every slope was bigger than the original one