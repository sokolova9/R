#Зчитати дані у формі часового ряду (або взяти готовий датасет в цьому форматі)
meat<- read.csv(file = "C:\\Users\\User\\Desktop\\3курс\\mmoi\\5\\meat_prices_20180103_20211027.csv", header = TRUE)
beef <- subset(meat, (meat_type == "Beef, kg" ) , select = c(price))
beef.ts <-  ts(beef, frequency = 80, start = c(2018, 1))
#Зобразити отриманий часовий ряд (з підписами)
plot.ts(beef.ts,xlab = "Ціна" , ylab = "Час",main = "Зміна цін на яловичину") 
#Провести згладжування ряду методом рухомого середнього з різним кроком
library(TTR)
plot.ts(SMA(beef.ts, n=5))
SMA(beef.ts,order=3, silent=F)
plot.ts(SMA(beef.ts, n=30))
plot.ts(SMA(beef.ts, n=50))
plot.ts(SMA(beef.ts, n=100))
#Розбити вихідний часовий ряд на систематичну, періодичну та хаотичну складові
dec<-decompose(beef.ts)
plot(dec)
ggseasonplot(beef.ts)
#Побудувати корелограму та частинну корелограму ЧР
ggtsdisplay(beef.ts)
#За потреби трансформувати ЧР
trbeef1<-diff(beef.ts,lag = 7)
plot(trbeef1, main="first diff")
trbeef2<-diff(trbeef1)
plot(trbeef2, main="second diff")
ggtsdisplay(trbeef2)
#Вибрати модель, яка адекватно прогнозуватиме даний ЧР. Побудувати прогноз відповідним до моделі ЧР методом експоненційного згладжування (звичайним, подвійним або потрійним – залежно від моделі) та відповідним до ряду методом з групи ARIMA
hlt <- HoltWinters(beef.ts, gamma  = FALSE)
p=predict(hlt, 100, prediction.interval = T)
plot(hlt, p,ylab="beef price")

library(forecast)
frcst <- forecast(trbeef2)
frcst
plot(frcst)

arima.beef = arima(trbeef2, order = c(1,1,1))
checkresiduals(arima.beef$residuals)

arima.beefauto = auto.arima(trbeef2, d=0, approximation=FALSE)
checkresiduals(arima.beefauto$residuals)

arima.beef1 = auto.arima(beef.ts,  d=0, approximation=FALSE)
plot(forecast(auto.arima(beef.ts, d=1)))
checkresiduals(arima.beef1$residuals)
shapiro.test(arima.beef1$residuals)
Box.test(arima.beef1$residuals)

#Побудувати корелограми залишків та інші діаграми, що характеризують розподіл залишків моделі
frcst$residuals
pacf(frcst$residuals, main = "PACF", na.action = na.pass)
library(astsa)
sarima(beef.ts,1,1,1)
