wine<- read.csv(file = "C:\\Users\\User\\Desktop\\3курс\\mmoi\\4\\winemag-data-130k-v2.csv", header = TRUE)
countr <- subset(wine, (country == "US" | country == "Italy" | country == "Spain") &  points<100 , select = c(country,points))
countr$country <- factor(countr$country)
#значення по кожній з градацій 
boxplot(points ~ country, data=countr, 
        xlab = "Country" , ylab = "Points",
        main = "Points for each country", col = c("purple", "blue", "coral"))
#значення середніх 
aggregate(x = countr$points, by = list(countr$country), FUN = mean)
#Провести дисперсійний аналіз для цих даних
desp_an<-aov(points ~ country, data = countr)
lm_an <- lm(points ~ country, data = countr)
summary(desp_an)
summary(lm_an)
hist(desp_an$residuals, main = "Гістограма залишків моделі", freq = F, col = "red")
plot(desp_an)
kruskal.test(points ~ country, data = countr)
bartlett.test(points ~ country, data = countr)
#Провести аналіз контрастів
contrasts(countr$country)
library(ggplot2)
library(ggpubr)
ggqqplot(desp_an$residuals)
shapiro.test(desp_an$residuals[0:5000])