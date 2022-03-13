install.packages("ISLR")
library(ISLR)
data("College")
college = read.csv("C:/Users/gayat/Downloads/College.csv") 
college = read.csv(file = 'College.csv', header = TRUE, stringsAsFactors = TRUE)
summary(College)
rownames(college)<-college[,1]
college<-college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
plot(college$Private,college$Outstate,col = c("green","yellow"))
Elite=rep("No",nrow(college)) 
Elite[college$Top10perc >50]="Yes" 
Elite=as.factor(Elite)  
college=data.frame(college ,Elite)  
summary(college$Elite)
plot(college$Elite,college$Outstate,col=c("red","green"))
par(mfrow=c(2,2))
hist(college$Accept,main="Number of applications accepted",col="red",breaks=50)
hist(college$Enroll,main="Number of new students enrolled",col="green",bin=100)
hist(college$PhD,main="Percent of faculties with PhD",col="red",breaks=20)
hist(college$perc.alumni,main="Percent of alumini donate",col="green")
summary(college$Apps)
summary(college$PhD)
data("Auto")
summary(Auto)
sapply(Auto[,c(1:6),],range)
sapply(Auto[,c(1:6),],mean)
sapply(Auto[,c(1:6),],sd)
new.auto = subset(Auto[-c(10:85),])
sapply(new.auto[,-c(9)],range)
sapply(new.auto[,-c(9)],mean)
sapply(new.auto[,-c(9)],sd)
pairs(Auto,panel=panel.smooth,main="scatter plots of all pairs of variables")
plot(as.factor(Auto$cylinders),Auto$mpg)
plot(Auto$mpg,Auto$weight)
pairs(~ mpg + horsepower + weight + displacement, data = Auto, panel = panel.smooth)
plot(factor(Auto$origin),Auto$mpg,names=(c("American","European","Japanese"))) 
library(MASS)
data(Boston)
summary(Boston)
str(Boston)
plot(as.factor(Boston$chas),Boston$age) 
plot(as.factor(Boston$chas),Boston$lstat)
plot(as.factor(Boston$chas),Boston$medv) 
plot(as.factor(Boston$chas),Boston$crim)
plot(Boston$dis,Boston$crim)
plot(as.factor(Boston$chas),Boston$tax)
range(Boston$crim)
hist(Boston$tax)
hist(Boston$crim,breaks=50)
table(Boston$chas)
median(Boston$ptratio)
plot(as.factor(Boston$chas),Boston$medv)
which.min(Boston$medv)
Boston[which.min(Boston$medv),]
summary(Boston$crim)
table(Boston$rm > 7)
table(Boston$rm >8)
rooms8 = Boston[Boston$rm > 8, ]
summary(rooms8)
table(rooms8$chas)
summary(rooms8$black)
summary(Boston$black)
