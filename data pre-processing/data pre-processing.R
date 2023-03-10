study<-read.table(file= "xAPI-Edu-Data.csv", header=TRUE, sep=",")
library(psych)
library(ggplot2)
#Äîñë³äæåííÿ â³äâ³äàíèõ ğåñóğñ³â (ñê³ëüêè ğàç³â ñòóäåíò â³äâ³äóº êóğñ) 
z <- study$VisITedResources
#1. Ï³äğàõóâàòè ïîêàçíèêè öåíòğó: ñåğåäíº çíà÷åííÿ, ìåä³àíó. 
mean<-mean(z)
mean
median <- median(z)
median
#2. Ï³äğàõóâàòè ïîêàçíèêè âàğ³àö³¿. 
#äèñïåğñ³ÿ
var <-  var(z)
var
#ñòàíäàğòíå â³äõèëåííÿ
sd <- sd(z)
sd
#êîåô³ö³ºíò âàğ³àö³¿
covar<- sd / mean * 100
covar
#ğîçìàõ âàğ³àö³¿
range(z)
rV<-max(z)-min(z)
rV
#³íòåğêâàğòèëüíèé ğîçìàõ
rI<-IQR(z)
rI
#3. Ïîáóäóâàòè ÿùèê ç âóñàìè (ç ï³äïèñàìè).
boxplot(z, ylab="Visited resources",col ='yellow')
#4. Âèâåñòè ï’ÿòèòî÷êîâó õàğàêòåğèñòèêó (åêñòğåìàëüí³ òî÷êè òà êâàğòèë³).  
summary(z)
#5. Çíàéòè 1-é òà 9-é äåöèë³.
quantile(z, probs = c(.1, .9))
#6. Çíàéòè êîåô³ö³ºíò àñèìåòğ³¿ òà êîåô³ö³ºíò åêñöåñó. 
library(e1071)
skewness(z)
kurtosi(z)
#7. Ïîáóäóâàòè ã³ñòîãğàìó, âèêîğèñòîâóş÷è ğ³çí³ ìåòîäè ãğóïóâàííÿ.
#îö³íêó ù³ëüíîñò³
hist(z, probability = TRUE, col = "purple", main = "Density estimation")
#ãğàô³ê ù³ëüíîñò³ ã³ïîòåòè÷íîãî ğîçïîä³ëó
lines(density(z), col='blue',lwd=4)
#áàçîâå ïğàâèëî
hist(z, main = "Basic(Sturges)", col='red', xlab = "Attendance assessment")
#ïğàâèëî Ñêîòòà
hist(z, breaks = "Scott", main = "Scott rule", col = "lightgreen", 
     xlab = "Attendance assessment")
#ïğàâèëî Ôğ³äìàíà-Ä³àêîí³ñà
hist(z, breaks = "FD", main = "Freedman and Diaconis’s rule", col = "pink",
     xlab = "Attendance assessment")
#8. Çîáğàçèòè Q-Q-ä³àãğàìó.
qqnorm(z, col="red", main="Q-Q plot")
qqline(z, col="darkblue", lwd=2.5)
#9. Çîáğàçèòè P-P-ä³àãğàìó. 
#plot(pnorm(sort(z)),(1:length(z))/length(z),
#      xlab="Theoretical P", ylab="Empirical P", asp=3)
#abline(0,1,col="red")

#2
m <- mean(z)
s <- sd(z)
n <- nrow(study)
p <- (1 : n) / n - 0.5 / n
ggplot(study) + geom_point(aes(x = p, y = sort(pnorm(z, m, s))))
#3
plot((sort(pnorm(z, m, s))),(1:length(z))/length(z),
     xlab="Theoretical P", ylab="Empirical P", asp=1)
abline(0,1,col="red")
#10. Çà äîïîìîãîş îäíîãî ç ñòàòèñòè÷íèõ êğèòåğ³¿â ïåğåâ³ğèòè çãîäó ç ã³ïîòåòè÷íèì ğîçïîä³ëîì.
chisq.test(z)