salary<- read.csv(file = "C:\\Users\\User\\Desktop\\3курс\\mmoi\\2\\Salary_Data.csv", header = TRUE)
#побудувати діаграму розсіювання, за графікамми визначити функціональний тип залежності
Exp <- salary$YearsExperience
Sal <- salary$Salary
plot(Exp~Sal, col="green", pch=10, main = "Залежність зарплати та стажу")
#регресійнa модель
reg_model <- lm(Exp~Sal, data = salary)
summary(reg_model)
#Зобразити отриману лінію регресії на одному графіку з діаграмою розсіювання
library(ggplot2)
ggplot(salary, aes(x=Exp,y=Sal))+geom_point()+stat_smooth(method = lm)+geom_smooth(formula = y ~ x, method = lm)
#Побудувати діаграму «відгук-прогноз» 
plot(salary$Sal ~ fitted(reg_model), col ="pink", xlab = "Прогнозовані значення моделі", 
     ylab = "Значення спостережень", main = "Діаграма відгук-прогноз ")
#Побудувати діаграму «відгук-залишки»
plot(resid(reg_model) ~ fitted(reg_model), xlab = "Прогнозовані значення моделі", 
     ylab = "Залишки моделі", main = "Діаграма прогноз-залишки ", col ="red")
abline(h = 0)
#Побудувати діаграму залишків за номером спостереження
plot(c(1:length(salary$Sal)), resid(reg_model), xlab = "Порядковий номер", ylab = "Значення залишків", 
     main = "Значення залишків за номером спостереження", col ="purple")
abline(h = 0)
#Побудувати Q-Q-діаграму для залишків
qqnorm(resid(reg_model),main = "Q-Q", col="black")
qqline(resid(reg_model))