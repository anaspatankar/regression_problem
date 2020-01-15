setwd("C:\\Users\\Anas Patankar\\Desktop\\STAT 512\\Project")


df <- read.csv("Concrete_data.csv", sep=",", header=T);
df <- subset(df, select = -c(X))



####normality tests

install.packages("dplyr")
install.packages("ggpubr")
library("dplyr")
library("ggpubr")
library("car")


Age <- factor(Age)

attach(df)

res<- lm(Strength~Fly_Ash+Water+Cement+Furn_Slag+Su_plastic+Coarse_Agg+Fine_Agg+Age, data = df)
summary(res)
plot(res, which = c(4))

df_1<- df[-c(225, 382), ]

attach(df)

res1<- lm((Strength)**(0.5)~Fly_Ash+Water+Cement+Furn_Slag+log(1+Su_plastic)+Coarse_Agg+(Fine_Agg)+Age)
summary(res1)





install.packages('leaps')
library(leaps)

leaps<- regsubsets(Strength~., data = df ,method = "exhaustive")
leaps1<- regsubsets(Strength~.^2, data = df ,method = "exhaustive")

library(car)

subsets(leaps, statistic = "bic")
subsets(leaps, statistic = "cp")

#####multi

library(fmsb)
mul<- lm(Strength~., data = df)
vif(mul)
mean(vif(mul))
####no multicolllinearity









shapiro.test(res1$residuals)
plot(res1)




#####heteroscedasticity
install.packages('lmtest')

lmtest::bptest(res1) 

ncvTest(res1)
###fail to reject null, hence the model is homoscedastic




cooksd <- cooks.distance(res1)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red") 
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")




outlierTest(res1)


shapiro.test(res1$residuals)

with(df, invTranPlot(1+Fly_Ash, (Strength)))


res11<- lm(Strength~Cement**(0.892), data = df)




hist(res$residuals)
qqnorm(res$residuals)
qqline(res$residuals)

install.packages("moments")

skewness(res$residuals)
library(MASS)
b= boxcox(Strength~log(1+Fly_Ash)+Water+Cement+log(1+Furn_Slag)+log(1+Su_plastic)+log(Coarse_Agg)+Fine_Agg+Age, data = df)

best.lam=b$x[which(b$y==max(b$y))]
best.lam

names(df)




lambda = 0.70707
adj_C <- bcPower(Coarse_Agg, -2.0895154, jacobian.adjusted=FALSE)
hist(Coarse_Agg)

adj_Strength<- bcPower(Strength, jacobian.adjusted=FALSE)


res<- lm(adj_Strength~adj_water)
plot(res)


unlist(with(df, invTranEstimate(Coarse_Agg,Strength)))

res11<- lm(Strength~Cement**(0.8929))
