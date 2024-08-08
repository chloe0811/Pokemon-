#install.packages("ggforce")
#install.packages("GGally")
#install.packages("concaveman")
#install.packages("fmsb")
#install.packages("ggplot2")
#install.packages("latex2exp")

library(dplyr)
library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)
library(ggforce)
library(GGally)
library(scales)
library(cowplot)
library(FactoMineR)
library(factoextra)
library(plotly)
library(readxl)
library(corrplot)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(concaveman)
library(fmsb)
options(scipen = 123)

setwd("C:/Users/USER/Desktop/大學/三下/資料探勘/寶可夢")
pokemon<-read_excel("pokemon_Project2.xlsx",sheet="POKEMON")
str(pokemon)
ggplot(aes(type1,total_points), data=pokemon) + geom_boxplot(fill="grey") +  ggtitle("屬性間差異") + xlab("Type1") + ylab("Total_Points") +theme_bw()

poke <- pokemon %>% mutate(type1 = as.factor(type1),type2 = as.factor(type2))

colSums(is.na(poke))

poclean <- poke %>% select_if(~is.numeric(.)) %>% select(-c(pokedex_number)) %>% mutate(name = pokemon$name, type1=pokemon$type1, type2=pokemon$type2) %>% na.omit()
#資料切割
poclean2 <- poclean %>% select(-c(name, type1, type2,total_points))
poclean3 <- poclean %>% select(-c(name, type1, type2))

#檢查有沒有NA值
colSums(is.na(poclean))

#Typ1 and Type2比較
poclean  %>% group_by(type1, type2) %>% summarise(total_points = mean(total_points)) %>% 
  ggplot(aes(type1, type2, fill = total_points)) + scale_fill_viridis_c(option = "B") + 
  geom_tile() + theme_minimal()
#各項能力值熱圖
ggcorr(poclean2,hjust= 1)

#Find number of clusters
#1.Elbow Method
fviz_nbclust(poclean3, kmeans, method = "wss", k.max = 18)
#2.Silhouette Method
#fviz_nbclust(poclean3, kmeans, "silhouette", k.max = 18) + labs(subtitle = "Silhouette method")
#3.Gap Statistic
#fviz_nbclust(poclean3, kmeans, "gap_stat", k.max = 50) + labs(subtitle = "Gap Statistic method")
#k-means clustering
kmeans <- kmeans(poclean3, centers = 5)
df_clust <- poke %>% na.omit() %>% bind_cols(cluster = as.factor(kmeans$cluster)) %>% select(cluster, 1:14)

#total_points和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,total_points,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#hp和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,hp,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#height和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,height_m,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#weight和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,weight_kg,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#attack和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,attack,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#defense和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,defense,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#sp_attack和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,sp_attack,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#sp_defense和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,sp_defense,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#speed和type1的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type1,defense,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")

#total_points和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,total_points,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#height和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,height_m,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#weight和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,weight_kg,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#hp和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,hp,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#attack和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,attack,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#defense和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,defense,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#sp_attack和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,sp_attack,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#sp_defense和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,sp_defense,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#speed和type2的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(type2,speed,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")

poke_pca <- PCA(poclean3 %>% select(-cluster) , scale.unit = T, ncp = 9)
#速度、攻擊、HP、防禦、特殊攻擊和特殊防禦都與第一主成分相關。另一方面，只有速度和特殊攻擊與攻擊第二個 PC 相關。其餘變量要馬負相關，要馬根本不相關
summary(poke_pca)
fviz_eig(poke_pca)
fviz_pca_var(poke_pca, col.var = "steelblue")


poke_pca$eig
var <- get_pca_var(poke_pca)
a<-fviz_contrib(poke_pca, "var",axes = 1)
b<-fviz_contrib(poke_pca, "var",axes = 2)
a
#total_points,hp,height_m,weight_kg對第一維度有足夠的貢獻
b
#speed,defense,weight_kg,sp_attack對第二維度有足夠的貢獻
grid.arrange(a,b,top='Contribution to the Principal Components')
corrplot(var$contrib, is.corr = F)
#第一維度是由total_points跟hp所組成的，第二維度是由速度、特殊攻擊和防禦構成的

df_pca <- data.frame(poke_pca$ind$coord[,1:5])
df_pca$cluster <- kmeans$cluster
df_pca
df_pca$cluster

#看cluster內的差別
fviz_cluster(kmeans, poclean3 %>% select(-cluster))
plot_ly(df_pca, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, color = ~cluster, colors = c("black","red","green","blue")) %>% add_markers() %>% layout(scene = list(xaxis = list(title = "Dim.1"),yaxis = list(title = "Dim.2"),zaxis = list(title = "Dim.3")))
#cluster1 較為分散，custer34較集中

#5群和total_oints的關係
poclean3$cluster <- as.factor(kmeans$cluster)
cluster_all<-poclean3 %>% group_by(cluster) %>% summarise_if(is.numeric, 'mean') %>% select(c(cluster, (1:10)))
cluster_all

#5群和total_oints的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,total_points,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#5群和height的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,height_m,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#5群和weight的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,weight_kg,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#5群和hp的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,hp,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#5群和attack的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,attack,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#5群和defense的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,defense,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#5群和sp_attack的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,sp_attack,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#5群和sp_defense的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,sp_defense,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")
#5群和speed的關係
df_clust %>% mutate(cluster = cluster) %>% ggplot(aes(poclean3$cluster,speed,color = cluster)) + geom_point(alpha = 0.5) + geom_mark_hull() + scale_color_brewer(palette = "Set1") + 
  theme_minimal() + theme(legend.position = "top")

#分群雷達圖
cluster <- data.frame(
  row.names = c("cluster1", "cluster2", "cluster3", "cluster4", "cluster5"),
  hp = c(133.76923,73,49.47651,82.93684,93.40351),
  attack = c(119.46154,83.19298,54.24161,109.75789,117.21053),
  defense = c(142.92308,82.48246,54.04027,91.72632,107.91228),
  sp_attack = c(106,76.09649,47.43624,112.13684,99.87719),
  sp_defense = c(111.92308,79.44737,50.32886,92.52632,95.24561),
  speed = c(83.84615,73.32456,50.12081,95.65263,72.75439))
cluster

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  hp = c(200, 0), attack = c(200, 0), defense = c(200, 0),
  sp_attack = c(200, 0), sp_defense = c(200, 0), speed = c(200, 0))
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, cluster)
df


#第一群
cluster1_data <- df[c("Max", "Min", "cluster1"), ]
create_beautiful_radarchart <- function(data, color = color,vlabels = colnames(data), vlcex = 0.7,caxislabels = NULL, title = NULL, ...)
{radarchart(
  data, axistype = 1,
  # Customize the polygon
  pcol = "#AA0000", pfcol = scales::alpha("#AA0000", 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = vlcex, vlabels = vlabels,
  caxislabels = caxislabels, title = title)}
create_beautiful_radarchart(cluster1_data, caxislabels = c(0, 50, 100, 150, 200),title="cluster1特性")

#第二群
cluster2_data <- df[c("Max", "Min", "cluster2"), ]
create_beautiful_radarchart <- function(data, color = color,vlabels = colnames(data), vlcex = 0.7,caxislabels = NULL, title = NULL, ...)
{radarchart(
  data, axistype = 1,
  # Customize the polygon
  pcol = "#0044BB", pfcol = scales::alpha("#0044BB", 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = vlcex, vlabels = vlabels,
  caxislabels = caxislabels, title = title)}
create_beautiful_radarchart(cluster2_data, caxislabels = c(0, 50, 100, 150, 200),title="cluster2特性")

#第三群
cluster3_data <- df[c("Max", "Min", "cluster3"), ]
create_beautiful_radarchart <- function(data, color = color,vlabels = colnames(data), vlcex = 0.7,caxislabels = NULL, title = NULL, ...)
{radarchart(
  data, axistype = 1,
  # Customize the polygon
  pcol = "#008800", pfcol = scales::alpha("#008800", 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = vlcex, vlabels = vlabels,
  caxislabels = caxislabels, title = title)}
create_beautiful_radarchart(cluster3_data, caxislabels = c(0, 50, 100, 150, 200),title="cluster3特性")

#第四群
cluster4_data <- df[c("Max", "Min", "cluster4"), ]
create_beautiful_radarchart <- function(data, color = color,vlabels = colnames(data), vlcex = 0.7,caxislabels = NULL, title = NULL, ...)
{radarchart(
  data, axistype = 1,
  # Customize the polygon
  pcol = "#7700BB", pfcol = scales::alpha("#7700BB", 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = vlcex, vlabels = vlabels,
  caxislabels = caxislabels, title = title)}
create_beautiful_radarchart(cluster4_data, caxislabels = c(0, 50, 100, 150, 200),title="cluster4特性")

#第五群
cluster5_data <- df[c("Max", "Min", "cluster5"), ]
create_beautiful_radarchart <- function(data, color = color,vlabels = colnames(data), vlcex = 0.7,caxislabels = NULL, title = NULL, ...)
{radarchart(
  data, axistype = 1,
  # Customize the polygon
  pcol = "#EE7700", pfcol = scales::alpha("#EE7700", 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = vlcex, vlabels = vlabels,
  caxislabels = caxislabels, title = title)}
create_beautiful_radarchart(cluster4_data, caxislabels = c(0, 50, 100, 150, 200),title="cluster5特性")

#cluster1：壓倒性隊，又大又重，除了特功、速度以外其餘皆最高
#cluster2：普通防禦攻擊
#cluster3：弱弱小廢物，全部都低到爆
#cluster4：高速小隊，特攻強，防禦較弱
#cluster5：高防禦，血量多

