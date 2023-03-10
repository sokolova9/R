salary<- read.csv(file = "C:\\Users\\User\\Desktop\\3курс\\mmoi\\2\\Salary_Data.csv", header = TRUE)
#ƒл€ к≥лькох набор≥в к≥льк≥сних даних з одного датасету зобразити матричну д≥аграму розс≥юванн€
Exp <- salary$YearsExperience
head(salary, 5)
pairs(~Exp+Age+Salary, data=salary, col="red", pch=2, lower.panel=NULL)
#картa корел€ц≥й та граф корел€ц≥й
library(corrplot)
Cor<- cor(salary)
round(Cor, 4)
corrplot(Cor, method = "circle", title = "correlation matrix")
corrplot(Cor, method = "number", title = "correlation matrix")
corrplot.mixed(Cor)
corrplot(Cor, method="square", order="AOE",type = 'lower')
library(qgraph)
qgraph(Cor)
# ƒл€ найб≥льш суттЇвих корел€ц≥й порахувати коеф≥ц≥Їнти корел€ц≥њ ѕ≥рсона, —п≥рмена або  ендала
S <- salary$Salary
cor(Exp,S,method = "pearson")
cor(Exp,S,method = "kendall")
cor(Exp,S,method = "spearman")
cor.test(Exp,S,method = "pearson")
