customers<- read.csv(file = "C:\\Users\\User\\Desktop\\3курс\\mmoi\\6\\marketing_campaign11.csv", header = TRUE, sep = ";")
products <- subset(customers , select = c(Wines, MeatProducts, FishProducts, SweetProducts, GoldProds))
str(products)
summary(products)
# Для обраного датасету провести кластерний аналіз за кількома ознаками, використовуючи ієрархічну кластеризацію та метод к-середніх (або будь-які інші методики). Якщо потрібно – трансформувати дані. Застосувати хоча б 2 метода кластеризації на вибір (наприклад, центроїдів та найближчого сусіда, або інший). Порівняти усно отримані результати. Відстань між спостереженнями для кількісних даних брати евклідову, якщо дані номінальні – один з відповідних методів
pr.scale <- scale(products)
head(pr.scale)
pr.dist <- dist(pr.scale, method = "euclidean")



#метод найближчого сусіда 
nearest.neighbour <- hclust(pr.dist, method = "single")
plot(nearest.neighbour,main = "Nearest neighbour method ")

#Метод Ворда
ward.method <- hclust(pr.dist, method = "ward.D")
plot(ward.method,main = "Ward method ", labels = FALSE)
rect.hclust(ward.method , k = 3)

#Метод центроїдів
centr.method <- hclust(pr.dist, method = "centroid")
plot(centr.method, main = "Centroid method ", cex= 0.5)


products$mf  <- factor(cutree(ward.method, k = 3))
ggplot(products, aes(x = log(MeatProducts),y = log(FishProducts),color = mf)) +
  geom_point( position = position_jitter(h = 0.3, w = 0.3)) 
#Обрати оптимальну кількість кластерів. Для цього вивести необхідні зображення, які оптимізують різні показники для різної кількості кластерів
library(fpc)
clusterboot1 <- clusterboot(pr.scale, clustermethod = hclustCBI, method = "ward.D", k = 3,count = FALSE)
hcl_cboot_groups <- clusterboot1$result$partition
clusterboot1$bootmean
clusterboot1$bootbrd

clustering_ch <- kmeansruns(pr.scale, krange = 1:10, criterion = "ch")
clustering_ch$bestk
clustering_asw <- kmeansruns(pr.scale, krange = 1:10, criterion = "asw")
clustering_asw$bestk

library(reshape2)
library(ggplot2)
criteria <- data.frame(k = 1:10, ch = scale(clustering_ch$crit), 
                       asw = scale(clustering_asw$crit))
criteria <- melt(criteria, id.vars = c("k"), 
                 variable.name = "measure", 
                 value.name = "score")
ggplot(criteria, aes(x = k, y = score, col = measure)) + 
  geom_point(aes(shape = measure)) + geom_line(aes(linetype = measure)) + 
  scale_x_continuous(breaks = 1:10, labels = 1:10) + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal()


library(factoextra)
fviz_nbclust(pr.scale , kmeans , method = "silhouette") 

kmeans_cboot <- clusterboot(pr.scale, clustermethod = kmeansCBI,
                            runs = 100, iter.max = 100,
                            krange = 3, seed = 13,
                            count = FALSE)
kmeans_cboot_groups <- kmeans_cboot$result$partition
kmeans_cboot$bootmean
kmeans_cboot$bootbrd
#Для двох-трьох найкращих, на Вашу думку, варіантів кластеризації, зобразити результати кластеризації, якщо є потреба – з використанням методу головних компонент.

principal.component<- prcomp(pr.scale)
visual <- predict(principal.component, newdata = pr.scale)[, 1:2]
visual1 <- cbind(as.data.frame(visual),cluster = as.factor(kmeans_cboot_groups))
ggplot(visual1, aes(x = PC1, y = PC2)) + 
  geom_point(aes(col = cluster), alpha = 0.7) +theme_minimal() +scale_color_brewer(palette = "Set1")


#Порівняти обрані варіанти кластеризації, використовуючи індекс Ренда. 
groups5 <- cutree(ward.method, k = 3)
m <- dist(pr.scale)
hc2 <- hclust(m, method = "average") 
groups5_2 <- cutree(hc2, k = 3) #Between-groups linkage
library(fossil)
rand.index(groups5, groups5_2)
