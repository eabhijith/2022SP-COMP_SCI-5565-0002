getwd()

college<-read.csv("data.csv")

rownames(college)=college [,1]  
fix(college)
college =college [,-1] > fix(college)
fix(college)
glimpse(college)
summary(college)
pairs(college[ ,1:10])
ggplot(college, aes(x = private, y = outstate, fill = private)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "none") + 
  labs(title = "Outstate vs Private - Boxplot")
Elite=rep("No",nrow(college ))
Elite[college$top10perc >50]=" Yes"
Elite=as.factor(Elite)
college=data.frame(college , Elite)

summary(college$Elite)
ggplot(college, aes(x = Elite, y = outstate, fill = Elite)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "none") + 
  labs(title = "Outstate vs Elite - Boxplot")
par(mfrow=c(2,2))
hist(college$f_undergrad,
     main="Undergrad - Histogram",
     xlab="undergrad",
     col="deeppink1",breaks=15)
hist(college$phd,
     main="PHD - Histogram",
     xlab="PHD",
     col="#BF87B3",breaks=25)

hist(college$perc_alumni,
     main="Percentage of Alumni - Histogram",
     xlab="Percentage Alumni",
     col="#000080",breaks=40)
hist(college$personal,
     main="Personal - Histogram",
     xlab="personal",
     col="#000080",breaks=20)
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)
mean(Auto$mpg)
mean(Auto$cylinders)
mean(Auto$displacement)
mean(Auto$horsepower)
mean(Auto$weight)
mean(Auto$acceleration)
mean(Auto$year)
sd(Auto$mpg)
sd(Auto$cylinders)
sd(Auto$displacement)
sd(Auto$horsepower)
sd(Auto$weight)
sd(Auto$acceleration)
sd(Auto$year)
Auto1<-Auto[-c(10:85), ]
range(Auto1$mpg)
range(Auto1$cylinders)
range(Auto1$displacement)
range(Auto1$horsepower)
range(Auto1$weight)
range(Auto1$acceleration)
range(Auto1$year)
sd(Auto1$mpg)
sd(Auto1$cylinders)
sd(Auto1$displacement)
sd(Auto1$horsepower)
sd(Auto1$weight)
sd(Auto1$acceleration)
sd(Auto1$year)
pairs(Auto[,1:7])
chrel<-ggplot(Auto, aes(x =cylinders , y = horsepower)) + 
  geom_point() + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(x = "Cylinders", 
       y = "Horsepower", 
       title = "Correlation between Cylinders and Horsepower")
chrel
wH<-ggplot(Auto, aes(x =weight , y = horsepower)) + 
  geom_point() + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(x = "Weight", 
       y = "Horsepower", 
       title = "Correlation between Weight and Horsepower")
wH
wa<-ggplot(Auto, aes(x = weight, y = acceleration)) + 
  geom_point() + 
  theme(legend.position = "none") + 
  labs(x = "Weight", 
       y = "Acceleration", 
       title = "Correlation between weight and acceleration")
wa
Auto$origin <- factor(Auto$origin, labels = c("American", "Australian", "Indian"))
ggplot(Auto, aes(x = weight, y = acceleration, col = origin)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~ origin) + 
  theme(legend.position = "none") + 
  labs(x = "Weight", 
       y = "Acceleration", 
       title = "Correlation between weight and acceleration, by origin")
ggplot(Auto, aes(x = year + 1900, y = acceleration)) + 
  geom_jitter() + 
  theme(legend.position = "none") + 
  labs(x = "Year", 
       y = "Acceleration", 
       title = "Acceleration (trends over time)")
ggplot(Auto, aes(x = year + 1900, y = acceleration, col = factor(origin))) + 
  geom_jitter() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none") + 
  labs(x = "Year", 
       y = "Acceleration", 
       title = "Acceleration (trends over time), by Origin") + 
  facet_wrap(~ origin)
boxp<-ggplot(Auto, aes(x = origin, y = mpg, fill = origin)) + 
  geom_boxplot() + 
  theme(legend.position = "none") + 
  labs(title = "Origin vs Mpg - Boxplot", 
       x = "Origin", 
       y = "MPG")
cylmpg<-ggplot(Auto, aes(x =cylinders , y = mpg)) + 
  geom_point() + geom_jitter() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(x = "Cylinders", 
       y = "MPG", 
       title = "Correlation between Cylinders and MPG")
cylmpg

dim(Boston)
g1 <- ggplot(Boston, aes(x = age, y = tax)) + geom_smooth(method = "lm", formula = "y ~ x + I(x^2)") + 
  scale_x_continuous(labels = scales::comma_format()) +
  geom_jitter(alpha = 0.1) + labs(title = "Age vs Tax - Jitter Plot", 
                                  x = "age", 
                                  y = "tax")
g1
summary(lm(age~tax,data=Boston))

best_predictor(Boston, "crim")

ggplot(Boston, aes(x = medv, y = crim)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", col = "green") +
  labs(title = "Medv vs Crim - Scatter Plot", 
       x = "Medv", 
       y = "Crim")
ggplot(Boston, aes(x = dis, y = crim)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", col = "green") +
  labs(title = "Dis vs Crim - Scatter Plot", 
       x = "Dis", 
       y = "Crim")
ggplot(Boston, aes(x = lstat, y = crim)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", col = "green") +
  labs(title = "Lstat vs Crim - Scatter Plot",
       x = "Lstat", 
       y = "Crim")

sum(Boston$rm > 7)
Boston_gt_8rooms <- Boston[Boston$rm > 8, ]

nrow(Boston_gt_8rooms)
prop.table(table(Boston_gt_8rooms$chas))
Boston_gt_8rooms_perc <- Boston_percentiles[as.numeric(rownames(Boston_gt_8rooms)), ]

glimpse(Boston_gt_8rooms_perc)
sapply(Boston_gt_8rooms_perc, mean)
