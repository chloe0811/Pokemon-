#目標一:不分群，利用寶可夢的屬性來預測捕捉率----------------------------------------------
library(readxl)
library(car)
library(reshape2)
library(ggplot2)
library(leaps)
library(ClusterR)
library(factoextra)
library(kernlab)
library(gridExtra)
library(dplyr)

#1.讀取資料
pokemon_Rdata <- read_excel("C:/Users/owo/Desktop/寶可夢數據.xlsx")
pokemon_Rdata <- pokemon_Rdata[,3:11]
pokemon_Rdata <- data.frame(pokemon_Rdata)

#2.檢查NA值?
anyNA(pokemon_Rdata) 

#3.資料標準化
pokemon_Rdata1 <- scale(pokemon_Rdata[,1:9])
pokemon_Rdata1 <- data.frame(pokemon_Rdata1)

#4.建立線性回歸模型
attach(pokemon_Rdata)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)

#5.偵測資料中的影響點(Cook's distance)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(pokemon_Rdata)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%924
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

#6.是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##從VIF值發現，雖然解釋變數height_m,weight_kg的VIF值超過平均VIF
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#7.是否有相關性?(共變異數)
cor(pokemon_Rdata[,1:8])
##畫熱圖
ggplot(melt(cor(pokemon_Rdata[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(pokemon_Rdata[, 1:8])

#8.模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(pokemon_Rdata[,c(1:8)],y=pokemon_Rdata$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
#將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
#Cp圖的X座標是各候選模型的迴歸係數數目，Y座標是相對的Cp值。
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(4,9),ylim=c(0,200))
abline(0, b=1)
#選擇lm(catch_rate~height_m+hp+attack+defense+sp_attack+sp_defense+speed) #刪除height_m

#9.模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
#選擇lm(catch_rate ~ weight_kg + hp + attack + defense + sp_attack + sp_defense + speed) #刪除height_m

#10.隨機抽樣
n <- nrow(pokemon_Rdata1)
set.seed(12345)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- pokemon_Rdata1[subset,]
testdata <- pokemon_Rdata1[ - subset,]

#11.用線性回歸模型預測捕捉率
model <- lm(catch_rate ~ speed + defense + hp + sp_attack + sp_defense + attack + weight_kg,traindata)
future <- predict(model,testdata)
future <- as.data.frame(future)
final <- cbind(testdata,future)
##還原標準化
for (i in 1:9) { 
  final[,i] <- (final[,i] * sd(pokemon_Rdata[,i])) + mean(pokemon_Rdata[,i]) 
} 
final[,10] <- (final[,10] * sd(pokemon_Rdata[,9])) + mean(pokemon_Rdata[,9]) 
##計算平均估計誤差
summary(model)
realdiff <- abs(final$catch_rate-final$future)
avg_realdiff <- sum(realdiff)/length(realdiff)
avg_realdiff

#目標二:分群後，再利用寶可夢的屬性來預測捕捉率-----------------------------------------
#1.讀取資料
pokemon_Rdata <- read_excel("C:/Users/owo/Desktop/寶可夢數據.xlsx")
pokemon_Rdata <- pokemon_Rdata[,3:11]
pokemon_Rdata <- data.frame(pokemon_Rdata)

#2.檢查NA值?
anyNA(pokemon_Rdata)

#3.標準化
pokemon_Rdata_scale <- scale(pokemon_Rdata[,1:8]) 
pokemon_Rdata_scale <- as.data.frame(pokemon_Rdata_scale)

#4.比較3種方法的組內差距值
##使用k-means法做分群
opt_km = Optimal_Clusters_KMeans(pokemon_Rdata_scale, max_clusters = 10, 
                                 initializer = 'random',criterion = "WCSSE",plot_clusters = T)

##使用K-means++做分群
opt_km = Optimal_Clusters_KMeans(pokemon_Rdata_scale, max_clusters = 10, 
                                 initializer = 'kmeans++',criterion = "WCSSE",plot_clusters = T)

##使用kernel k-means做分群
##資料需要為矩陣格式才能使用
pokemon_Rdata_scale_matrix <- as.matrix(pokemon_Rdata_scale)
##分三群
kkmeans <- kkmeans(pokemon_Rdata_scale_matrix, centers = 3, kernel = "rbfdot", 
                   kpar = "automatic",alg="kkmeans")
kkmeans_final3 <- cbind(pokemon_Rdata,kkmeans@.Data)
kkmeans3_SSE <- withinss(kkmeans)
sum(kkmeans3_SSE)
##分四群
kkmeans <- kkmeans(pokemon_Rdata_scale_matrix, centers = 4, kernel = "rbfdot", 
                   kpar = "automatic",alg="kkmeans")
kkmeans_final4 <- cbind(pokemon_Rdata,kkmeans@.Data)
kkmeans4_SSE <- withinss(kkmeans)
sum(kkmeans4_SSE)
##分五群
kkmeans <- kkmeans(pokemon_Rdata_scale_matrix, centers = 5, kernel = "rbfdot", 
                   kpar = "automatic",alg="kkmeans")
kkmeans_final5 <- cbind(pokemon_Rdata,kkmeans@.Data)
kkmeans5_SSE <- withinss(kkmeans)
sum(kkmeans5_SSE)

##由圖可知，分群最好是用K-means分3~5群

#8.kmeans分3群，去建構預測模型--------------------------------------------------
km3 = KMeans_rcpp(pokemon_Rdata_scale, clusters = 3, num_init = 5, 
                  max_iters = 100, initializer = 'random') 
##查看每一群的組內差距
k_means_SSE3 <- km3$WCSS_per_cluster
sum(k_means_SSE3)
##查看每筆資料所對應的分群
km3_out <- as.data.frame(km3$clusters) 
k_means_final3 <- cbind(pokemon_Rdata,km3_out)
kmeans1 <- subset(k_means_final3,k_means_final3[10] == 1 ) #第一群的資料
kmeans2 <- subset(k_means_final3,k_means_final3[10] == 2 ) #第二群的資料
kmeans3 <- subset(k_means_final3,k_means_final3[10] == 3 ) #第三群的資料

#9.kmeans1模型----------------------------------------------------------------------------
##標準化
kmeans1_subset <- scale(kmeans1[,1:9])
kmeans1_subset <- data.frame(kmeans1_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans1_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans1_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則
##相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans1_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點
kmeans1_subset <- kmeans1_subset[-14, ]
model=lm(data=kmeans1_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans1_subset <- kmeans1_subset[-c(42,45), ]
model=lm(data=kmeans1_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans1_subset <- kmeans1_subset[-c(14,42), ]
model=lm(data=kmeans1_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans1_subset <- kmeans1_subset[-45, ]
model=lm(data=kmeans1_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans1_subset <- kmeans1_subset[-39, ]
model=lm(data=kmeans1_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)


#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans1_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans1_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans1_subset[, 1:8])


#模型選擇(stepwise值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(formula = catch_rate ~ weight_kg + defense + speed)

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans1_subset[,c(1:8)],y=kmeans1_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
#將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
#可以搭配identify函數畫出all possible 篩選法的Cp圖，並即時點選最佳的模型。Cp圖的X座標是各候選模型的迴歸係數數目，Y座標是相對的Cp值。
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(3,9),ylim=c(0,20))
abline(0, b=1)
#選擇lm(catch_rate~height_m+hp+sp_attack+sp_defense+speed+weight_kg) #刪除attack、defense
#選擇lm(catch_rate~height_m+hp+sp_attack+sp_defense+speed+weight_kg+defense) #刪除attack

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ weight_kg + defense + speed)
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ weight_kg + defense + speed,traindata)
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ height_m+hp+sp_attack+sp_defense+speed+weight_kg) #刪除attack、defense
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ height_m+hp+sp_attack+sp_defense+speed+weight_kg,traindata) #刪除attack、defense
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ height_m+hp+sp_attack+sp_defense+speed+weight_kg+defense) #刪除attack
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ height_m+hp+sp_attack+sp_defense+speed+weight_kg+defense,traindata) #刪除attack
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1
#第一群，用lm(formula = catch_rate ~ height_m+hp+sp_attack+
#                       sp_defense+speed+weight_kg+defense)#刪除attack比較好

#10.kmeans2模型----------------------------------------------------------------------
kmeans2_subset <- scale(kmeans2[,1:9])
kmeans2_subset <- data.frame(kmeans2_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans2_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans2_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans2_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點

#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans2_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans2_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans2_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans2_subset[,c(1:8)],y=kmeans2_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
##可以搭配identify函數畫出all possible 篩選法的Cp圖，並即時點選最佳的模型。Cp圖的X座標是各候選模型的迴歸係數數目，Y座標是相對的Cp值。
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~height_m+hp+attack+sp_attack+sp_defense+speed ) #刪除weight_kg、defense
##選擇lm(catch_rate~height_m+defense+hp+attack+sp_attack+sp_defense+speed ) #刪除weight_kg

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~height_m+defense+hp+attack+sp_attack+sp_defense+speed ) #刪除weight_kg

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ height_m+defense+hp+attack+sp_attack+sp_defense+speed) #刪除weight_kg
n <- nrow(kmeans2_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans2_subset[subset,]
testdata <- kmeans2_subset[ - subset,]
model2 <- lm(catch_rate ~ height_m+defense+hp+attack+sp_attack+sp_defense+speed,traindata)
future <- predict(model2,testdata)
future <- as.data.frame(future)
final2 <- cbind(testdata,future)
for (i in 1:9) { 
  final2[,i] <- (final2[,i] * sd(kmeans2[,i])) + mean(kmeans2[,i]) 
} 
final2[,10] <- (final2[,10] * sd(kmeans2[,9])) + mean(kmeans2[,9]) 
realdiff2 <- abs(final2$catch_rate-final2$future)
avg_realdiff2 <- sum(realdiff2)/nrow(final2)
avg_realdiff2

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ height_m+hp+attack+sp_attack+sp_defense+speed) #刪除weight_kg、defense
n <- nrow(kmeans2_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans2_subset[subset,]
testdata <- kmeans2_subset[ - subset,]
model2 <- lm(catch_rate ~ height_m+hp+attack+sp_attack+sp_defense+speed,traindata) #刪除weight_kg、defense
future <- predict(model2,testdata)
future <- as.data.frame(future)
final2 <- cbind(testdata,future)
for (i in 1:9) { 
  final2[,i] <- (final2[,i] * sd(kmeans2[,i])) + mean(kmeans2[,i]) 
} 
final2[,10] <- (final2[,10] * sd(kmeans2[,9])) + mean(kmeans2[,9]) 
realdiff2 <- abs(final2$catch_rate-final2$future)
avg_realdiff2 <- sum(realdiff2)/nrow(final2)
avg_realdiff2

#第二群，選擇lm(formula = catch_rate ~ height_m+hp+attack+sp_attack+
#                         sp_defense+speed) 刪除weight_kg、defense比較好

#11.kmeans3模型----------------------------------------------------------------------
kmeans3_subset <- scale(kmeans3[,1:9])
kmeans3_subset <- data.frame(kmeans3_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans3_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans3_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans3_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點
kmeans3_subset <- kmeans3_subset[-124, ]
model=lm(data=kmeans3_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans3_subset <- kmeans3_subset[-c(316,478), ]
model=lm(data=kmeans3_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)

#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans3_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans3_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans3_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans3_subset[,c(1:8)],y=kmeans3_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~defense+hp+attack+sp_attack+sp_defense+speed+weight_kg ) #刪除height_m
##選擇lm(catch_rate~defense+hp+attack+sp_attack+sp_defense+speed+height_m ) #刪除weight_kg

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~defense+hp+attack+sp_attack+sp_defense+speed ) #刪除weight_kg、height_m

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed+weight_kg) #刪除height_m
n <- nrow(kmeans3_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans3_subset[subset,]
testdata <- kmeans3_subset[ - subset,]
model3 <- lm(catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed+weight_kg,traindata) #刪除height_m
future <- predict(model3,testdata)
future <- as.data.frame(future)
final3 <- cbind(testdata,future)
for (i in 1:9) { 
  final3[,i] <- (final3[,i] * sd(kmeans3[,i])) + mean(kmeans3[,i]) 
} 
final3[,10] <- (final3[,10] * sd(kmeans3[,9])) + mean(kmeans3[,9]) 
realdiff3 <- abs(final3$catch_rate-final3$future)
avg_realdiff3 <- sum(realdiff3)/nrow(final3)
avg_realdiff3

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed+height_m) #刪除weight_kg
n <- nrow(kmeans3_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans3_subset[subset,]
testdata <- kmeans3_subset[ - subset,]
model3 <- lm(catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed+height_m,traindata) #刪除weight_kg
future <- predict(model3,testdata)
future <- as.data.frame(future)
final3 <- cbind(testdata,future)
for (i in 1:9) { 
  final3[,i] <- (final3[,i] * sd(kmeans3[,i])) + mean(kmeans3[,i]) 
} 
final3[,10] <- (final3[,10] * sd(kmeans3[,9])) + mean(kmeans3[,9]) 
realdiff3 <- abs(final3$catch_rate-final3$future)
avg_realdiff3 <- sum(realdiff3)/nrow(final3)
avg_realdiff3

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed) #刪除weight_kg、height_m
n <- nrow(kmeans3_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans3_subset[subset,]
testdata <- kmeans3_subset[ - subset,]
model3 <- lm(catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed,traindata) #刪除weight_kg、height_m
future <- predict(model3,testdata)
future <- as.data.frame(future)
final3 <- cbind(testdata,future)
for (i in 1:9) { 
  final3[,i] <- (final3[,i] * sd(kmeans3[,i])) + mean(kmeans3[,i]) 
} 
final3[,10] <- (final3[,10] * sd(kmeans3[,9])) + mean(kmeans3[,9]) 
realdiff3 <- abs(final3$catch_rate-final3$future)
avg_realdiff3 <- sum(realdiff3)/nrow(final3)
avg_realdiff3

#第三群，選擇lm(formula = catch_rate ~ defense+hp+attack+sp_attack+
#                       sp_defense+speed) 刪除weight_kg、height_m比較好

#12.全部預測誤差平均-------------------------------------------------------------
(sum(realdiff1)+sum(realdiff2)+sum(realdiff3))/(length(realdiff1)+
                                                  length(realdiff2)+length(realdiff3))

#全部寶可夢的捕捉率預測
##丟新資料要分哪一群?
##新資料離哪一群中心近就丟哪群
##第一群模型model1
##第二群模型model2
##第三群模型model3

pokemon_testdata <- read_excel("C:/Users/owo/Desktop/寶可夢數據.xlsx")
x <- pokemon_testdata[,3:11]
distance_1 <- as.data.frame(1:nrow(x))
distance_2 <- as.data.frame(1:nrow(x))
distance_3 <- as.data.frame(1:nrow(x))
predict_1 <- as.data.frame(1:nrow(x))
predict_2 <- as.data.frame(1:nrow(x))
predict_3 <- as.data.frame(1:nrow(x))

for (j in 1:nrow(x)) { 
  
  a=0
  b=0
  c=0
  for (i in 1:8) { 
    a <- a + (x[j,i]-mean(final1[,i]))^2
    b <- b + (x[j,i]-mean(final2[,i]))^2
    c <- c + (x[j,i]-mean(final3[,i]))^2
  } 
  ##選距離最小的判斷丟哪群
  distance_1[j,1] <- a^(1/2)
  distance_2[j,1] <- b^(1/2)
  distance_3[j,1] <- c^(1/2)
  ##預測x的捕捉率
  x1=0*x
  x2=0*x
  x3=0*x
  ##將x做標準化
  for (i in 1:8) { 
    x1[j,i] <- (x[j,i]-mean(kmeans1[,i]))/sd(kmeans1[,i])
    x2[j,i] <- (x[j,i]-mean(kmeans2[,i]))/sd(kmeans2[,i])
    x3[j,i] <- (x[j,i]-mean(kmeans3[,i]))/sd(kmeans3[,i])
  } 
  ##將標準化後的x代進模型做預測
  A1 <- predict(model1,x1[j,])
  A2 <- predict(model2,x2[j,])
  A3 <- predict(model3,x3[j,])
  ##將捕捉率預測結果還原標準化，變成一般數值
  (predict_1[j,1] <- (A1 * sd(kmeans1[,9])) + mean(kmeans1[,9]) )
  (predict_2[j,1] <- (A2 * sd(kmeans2[,9])) + mean(kmeans2[,9]) )
  (predict_3[j,1] <- (A3 * sd(kmeans3[,9])) + mean(kmeans3[,9]) )
  
}

Final_prediction <- cbind(pokemon_testdata,distance_1,distance_2
                          ,distance_3,predict_1,predict_2,predict_3)
names(Final_prediction) <- c("name","rare","height_m","weight_kg","hp","attack"
                             ,"defense","sp_attack","sp_defense","speed"
                             ,"catch_rate","distance_1","distance_2"
                             ,"distance_3","predict_1","predict_2"
                             ,"predict_3")


#匯出CSV檔
write.csv(Final_prediction, file="分3群全部預測.csv", row.names = TRUE)

#13.kmeans分4群，去建構預測模型--------------------------------------------------
km4 = KMeans_rcpp(pokemon_Rdata_scale, clusters = 4, num_init = 5, 
                  max_iters = 100, initializer = 'random') 
##查看每一群的組內差距
k_means_SSE4 <- km4$WCSS_per_cluster
sum(k_means_SSE4)
##查看每筆資料所對應的分群
km4_out <- as.data.frame(km4$clusters) 
k_means_final4 <- cbind(pokemon_Rdata,km4_out)
kmeans1 <- subset(k_means_final4,k_means_final4[10] == 1 ) #第一群的資料
kmeans2 <- subset(k_means_final4,k_means_final4[10] == 2 ) #第二群的資料
kmeans3 <- subset(k_means_final4,k_means_final4[10] == 3 ) #第三群的資料
kmeans4 <- subset(k_means_final4,k_means_final4[10] == 4 ) #第四群的資料

#14.kmeans1模型----------------------------------------------------------------------------
##標準化
kmeans1_subset <- scale(kmeans1[,1:9])
kmeans1_subset <- data.frame(kmeans1_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans1_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans1_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則
##相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans1_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點
kmeans1_subset <- kmeans1_subset[-186, ]
model=lm(data=kmeans1_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)

#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans1_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans1_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans1_subset[, 1:8])


#模型選擇(stepwise值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(formula = catch_rate ~ hp + attack + defense + sp_attack + sp_defense + speed) #刪除height_m、weight_kg

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans1_subset[,c(1:8)],y=kmeans1_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
#將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
#可以搭配identify函數畫出all possible 篩選法的Cp圖，並即時點選最佳的模型。Cp圖的X座標是各候選模型的迴歸係數數目，Y座標是相對的Cp值。
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(3,9),ylim=c(0,20))
abline(0, b=1)
#選擇lm(catch_rate~hp + attack + defense + sp_attack + sp_defense + speed + height_m) #刪除weight_kg
#選擇lm(catch_rate~hp + attack + defense + sp_attack + sp_defense + speed + weight_kg) #刪除height_m

#用線性回歸模型預測捕捉率
##選擇lm(formula = catch_rate ~ hp + attack + defense + sp_attack + sp_defense + speed) #刪除height_m、weight_kg
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ hp + attack + defense + sp_attack + sp_defense + speed,traindata)
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1

#用線性回歸模型預測捕捉率
#選擇lm(catch_rate~hp + attack + defense + sp_attack + sp_defense + speed + height_m) #刪除weight_kg
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ hp + attack + defense + sp_attack + sp_defense + speed + height_m,traindata) 
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1

#用線性回歸模型預測捕捉率
#選擇lm(catch_rate~hp + attack + defense + sp_attack + sp_defense + speed + weight_kg) #刪除height_m
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ hp + attack + defense + sp_attack + sp_defense + speed + weight_kg,traindata) 
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1
#第一群，選擇lm(catch_rate~hp + attack + defense + sp_attack + sp_defense + speed + weight_kg) #刪除height_m比較好

#15.kmeans2模型----------------------------------------------------------------------
kmeans2_subset <- scale(kmeans2[,1:9])
kmeans2_subset <- data.frame(kmeans2_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans2_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans2_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans2_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點
kmeans2_subset <- kmeans2_subset[-c(68,166,254), ]
model=lm(data=kmeans2_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans2_subset <- kmeans2_subset[-92, ]
model=lm(data=kmeans2_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans2_subset <- kmeans2_subset[-91, ]
model=lm(data=kmeans2_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)


#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans2_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans2_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans2_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans2_subset[,c(1:8)],y=kmeans2_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
##可以搭配identify函數畫出all possible 篩選法的Cp圖，並即時點選最佳的模型。Cp圖的X座標是各候選模型的迴歸係數數目，Y座標是相對的Cp值。
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~height_m+hp+attack+sp_attack+sp_defense+speed+defense ) #刪除weight_kg

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~height_m+defense+hp+attack+sp_attack+sp_defense+speed ) #刪除weight_kg

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ height_m+defense+hp+attack+sp_attack+sp_defense+speed) #刪除weight_kg
n <- nrow(kmeans2_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans2_subset[subset,]
testdata <- kmeans2_subset[ - subset,]
model2 <- lm(catch_rate ~ height_m+defense+hp+attack+sp_attack+sp_defense+speed,traindata)
future <- predict(model2,testdata)
future <- as.data.frame(future)
final2 <- cbind(testdata,future)
for (i in 1:9) { 
  final2[,i] <- (final2[,i] * sd(kmeans2[,i])) + mean(kmeans2[,i]) 
} 
final2[,10] <- (final2[,10] * sd(kmeans2[,9])) + mean(kmeans2[,9]) 
realdiff2 <- abs(final2$catch_rate-final2$future)
avg_realdiff2 <- sum(realdiff2)/nrow(final2)
avg_realdiff2
#第二群，選擇lm(formula = catch_rate ~ height_m+hp+attack+sp_attack+
#                         sp_defense+speed+defense) 刪除weight_kg比較好

#16.kmeans3模型----------------------------------------------------------------------
kmeans3_subset <- scale(kmeans3[,1:9])
kmeans3_subset <- data.frame(kmeans3_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans3_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans3_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans3_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點

#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans3_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans3_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans3_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans3_subset[,c(1:8)],y=kmeans3_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~defense+hp+attack+sp_attack+sp_defense+speed+height_m ) #刪除weight_kg

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~defense+hp+attack+sp_attack+sp_defense+speed ) #刪除weight_kg

#用線性回歸模型預測捕捉率
#選擇lm(formula = catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed+height_m) #刪除weight_kg
n <- nrow(kmeans3_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans3_subset[subset,]
testdata <- kmeans3_subset[ - subset,]
model3 <- lm(catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed+height_m,traindata) 
future <- predict(model3,testdata)
future <- as.data.frame(future)
final3 <- cbind(testdata,future)
for (i in 1:9) { 
  final3[,i] <- (final3[,i] * sd(kmeans3[,i])) + mean(kmeans3[,i]) 
} 
final3[,10] <- (final3[,10] * sd(kmeans3[,9])) + mean(kmeans3[,9]) 
realdiff3 <- abs(final3$catch_rate-final3$future)
avg_realdiff3 <- sum(realdiff3)/nrow(final3)
avg_realdiff3
#第三群，選擇lm(formula = catch_rate ~ defense+hp+attack+sp_attack+sp_defense+speed+height_m) #刪除weight_kg比較好

#17.kmeans4模型----------------------------------------------------------------------
kmeans4_subset <- scale(kmeans4[,1:9])
kmeans4_subset <- data.frame(kmeans4_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans4_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans4_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans4_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點
kmeans4_subset <- kmeans4_subset[-c(5,42), ]
model=lm(data=kmeans4_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans4_subset <- kmeans4_subset[-c(12,44), ]
model=lm(data=kmeans4_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans4_subset <- kmeans4_subset[-39, ]
model=lm(data=kmeans4_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans4_subset <- kmeans4_subset[-c(5,13), ]
model=lm(data=kmeans4_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans4_subset <- kmeans4_subset[-c(15,34), ]
model=lm(data=kmeans4_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans4_subset <- kmeans4_subset[-c(5,14), ]
model=lm(data=kmeans4_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans4_subset <- kmeans4_subset[-c(9), ]
model=lm(data=kmeans4_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)


#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans4_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans4_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans4_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans4_subset[,c(1:8)],y=kmeans4_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~hp+attack+sp_attack+sp_defense+speed+height_m+weight_kg ) #刪除defense
##選擇lm(catch_rate~defense+hp+sp_attack+sp_defense+speed+height_m+weight_kg ) #刪除attack

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~weight_kg + defense + sp_attack + sp_defense + speed ) #刪除attack、height_m、hp

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~hp+attack+sp_attack+sp_defense+speed+height_m+weight_kg ) #刪除defense
n <- nrow(kmeans4_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans4_subset[subset,]
testdata <- kmeans4_subset[ - subset,]
model4 <- lm(catch_rate ~ hp+attack+sp_attack+sp_defense+speed+height_m+weight_kg,traindata) 
future <- predict(model4,testdata)
future <- as.data.frame(future)
final4 <- cbind(testdata,future)
for (i in 1:9) { 
  final4[,i] <- (final4[,i] * sd(kmeans4[,i])) + mean(kmeans4[,i]) 
} 
final4[,10] <- (final4[,10] * sd(kmeans4[,9])) + mean(kmeans4[,9]) 
realdiff4 <- abs(final4$catch_rate-final4$future)
avg_realdiff4 <- sum(realdiff4)/nrow(final4)
avg_realdiff4

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~weight_kg + defense + sp_attack + sp_defense + speed ) #刪除attack、height_m、hp
n <- nrow(kmeans4_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans4_subset[subset,]
testdata <- kmeans4_subset[ - subset,]
model4 <- lm(catch_rate ~ weight_kg + defense + sp_attack + sp_defense + speed,traindata) 
future <- predict(model4,testdata)
future <- as.data.frame(future)
final4 <- cbind(testdata,future)
for (i in 1:9) { 
  final4[,i] <- (final4[,i] * sd(kmeans4[,i])) + mean(kmeans4[,i]) 
} 
final4[,10] <- (final4[,10] * sd(kmeans4[,9])) + mean(kmeans4[,9]) 
realdiff4 <- abs(final4$catch_rate-final4$future)
avg_realdiff4 <- sum(realdiff4)/nrow(final4)
avg_realdiff4

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~defense+hp+sp_attack+sp_defense+speed+height_m+weight_kg ) #刪除attack
n <- nrow(kmeans4_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans4_subset[subset,]
testdata <- kmeans4_subset[ - subset,]
model4 <- lm(catch_rate ~ defense+hp+sp_attack+sp_defense+speed+height_m+weight_kg,traindata) 
future <- predict(model4,testdata)
future <- as.data.frame(future)
final4 <- cbind(testdata,future)
for (i in 1:9) { 
  final4[,i] <- (final4[,i] * sd(kmeans4[,i])) + mean(kmeans4[,i]) 
} 
final4[,10] <- (final4[,10] * sd(kmeans4[,9])) + mean(kmeans4[,9]) 
realdiff4 <- abs(final4$catch_rate-final4$future)
avg_realdiff4 <- sum(realdiff4)/nrow(final4)
avg_realdiff4

#第四群，選擇lm(catch_rate~weight_kg + defense + sp_attack + sp_defense + speed ) #刪除attack、height_m、hp比較好

#18.全部預測誤差平均-------------------------------------------------------------
(sum(realdiff1)+sum(realdiff2)+sum(realdiff3)+sum(realdiff4))/(length(realdiff1)+
                                                  length(realdiff2)+length(realdiff3)+length(realdiff4))

#全部寶可夢的捕捉率預測
##丟新資料要分哪一群?
##新資料離哪一群中心近就丟哪群
##第一群模型model1
##第二群模型model2
##第三群模型model3
##第四群模型model4
pokemon_testdata <- read_excel("C:/Users/owo/Desktop/寶可夢數據.xlsx")
x <- pokemon_testdata[,3:11]
distance_1 <- as.data.frame(1:nrow(x))
distance_2 <- as.data.frame(1:nrow(x))
distance_3 <- as.data.frame(1:nrow(x))
distance_4 <- as.data.frame(1:nrow(x))
predict_1 <- as.data.frame(1:nrow(x))
predict_2 <- as.data.frame(1:nrow(x))
predict_3 <- as.data.frame(1:nrow(x))
predict_4 <- as.data.frame(1:nrow(x))

for (j in 1:nrow(x)) { 
  
  a=0
  b=0
  c=0
  d=0
  for (i in 1:8) { 
    a <- a + (x[j,i]-mean(final1[,i]))^2
    b <- b + (x[j,i]-mean(final2[,i]))^2
    c <- c + (x[j,i]-mean(final3[,i]))^2
    d <- d + (x[j,i]-mean(final4[,i]))^2
  } 
  ##選距離最小的判斷丟哪群
  distance_1[j,1] <- a^(1/2)
  distance_2[j,1] <- b^(1/2)
  distance_3[j,1] <- c^(1/2)
  distance_4[j,1] <- d^(1/2)
  ##預測x的捕捉率
  x1=0*x
  x2=0*x
  x3=0*x
  x4=0*x
  ##將x做標準化
  for (i in 1:8) { 
    x1[j,i] <- (x[j,i]-mean(kmeans1[,i]))/sd(kmeans1[,i])
    x2[j,i] <- (x[j,i]-mean(kmeans2[,i]))/sd(kmeans2[,i])
    x3[j,i] <- (x[j,i]-mean(kmeans3[,i]))/sd(kmeans3[,i])
    x4[j,i] <- (x[j,i]-mean(kmeans4[,i]))/sd(kmeans4[,i])
  } 
  ##將標準化後的x代進模型做預測
  A1 <- predict(model1,x1[j,])
  A2 <- predict(model2,x2[j,])
  A3 <- predict(model3,x3[j,])
  A4 <- predict(model4,x4[j,])
  ##將捕捉率預測結果還原標準化，變成一般數值
  (predict_1[j,1] <- (A1 * sd(kmeans1[,9])) + mean(kmeans1[,9]) )
  (predict_2[j,1] <- (A2 * sd(kmeans2[,9])) + mean(kmeans2[,9]) )
  (predict_3[j,1] <- (A3 * sd(kmeans3[,9])) + mean(kmeans3[,9]) )
  (predict_4[j,1] <- (A4 * sd(kmeans4[,9])) + mean(kmeans4[,9]) )  
}

Final_prediction <- cbind(pokemon_testdata,distance_1,distance_2
                          ,distance_3,distance_4,predict_1,predict_2,predict_3,predict_4)
names(Final_prediction) <- c("name","rare","height_m","weight_kg","hp","attack"
                             ,"defense","sp_attack","sp_defense","speed"
                             ,"catch_rate","distance_1","distance_2"
                             ,"distance_3","distance_4","predict_1"
                             ,"predict_2","predict_3","predict_4")


#匯出CSV檔
write.csv(Final_prediction, file="分4群全部預測.csv", row.names = TRUE)

#19.kmeans分5群，去建構預測模型--------------------------------------------------
km5 = KMeans_rcpp(pokemon_Rdata_scale, clusters = 5, num_init = 5, 
                  max_iters = 100, initializer = 'random') 
##查看每一群的組內差距
k_means_SSE5 <- km5$WCSS_per_cluster
sum(k_means_SSE5)
##查看每筆資料所對應的分群
km5_out <- as.data.frame(km5$clusters) 
k_means_final5 <- cbind(pokemon_Rdata,km5_out)
kmeans1 <- subset(k_means_final5,k_means_final5[10] == 1 ) #第一群的資料
kmeans2 <- subset(k_means_final5,k_means_final5[10] == 2 ) #第二群的資料
kmeans3 <- subset(k_means_final5,k_means_final5[10] == 3 ) #第三群的資料
kmeans4 <- subset(k_means_final5,k_means_final5[10] == 4 ) #第四群的資料
kmeans5 <- subset(k_means_final5,k_means_final5[10] == 5 ) #第五群的資料
#20.kmeans1模型----------------------------------------------------------------------------
##標準化
kmeans1_subset <- scale(kmeans1[,1:9])
kmeans1_subset <- data.frame(kmeans1_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans1_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans1_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則
##相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans1_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點
kmeans1_subset <- kmeans1_subset[-c(77,130), ]
model=lm(data=kmeans1_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans1_subset <- kmeans1_subset[-50, ]
model=lm(data=kmeans1_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)

#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans1_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans1_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans1_subset[, 1:8])


#模型選擇(stepwise值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(formula = catch_rate ~ height_m + sp_defense + speed)

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans1_subset[,c(1:8)],y=kmeans1_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
#將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
#可以搭配identify函數畫出all possible 篩選法的Cp圖，並即時點選最佳的模型。Cp圖的X座標是各候選模型的迴歸係數數目，Y座標是相對的Cp值。
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(3,9),ylim=c(0,20))
abline(0, b=1)
#選擇lm(catch_rate~hp + defense + sp_attack + sp_defense + speed + height_m) #刪除weight_kg、attack
#選擇lm(catch_rate~hp + defense + sp_attack + sp_defense + speed + height_m + attack) #刪除weight_kg
#選擇lm(catch_rate~hp + defense + sp_attack + sp_defense + speed + height_m + weight_kg) #刪除attack

#用線性回歸模型預測捕捉率
#選擇lm(catch_rate~hp + defense + sp_attack + sp_defense + speed + height_m) #刪除weight_kg、attack
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ hp + defense + sp_attack + sp_defense + speed + height_m,traindata) 
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1

#用線性回歸模型預測捕捉率
#選擇lm(catch_rate~hp + defense + sp_attack + sp_defense + speed + height_m + attack) #刪除weight_kg
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ hp + defense + sp_attack + sp_defense + speed + height_m + attack,traindata) 
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1

#用線性回歸模型預測捕捉率
#選擇lm(catch_rate~hp + defense + sp_attack + sp_defense + speed + height_m + weight_kg) #刪除attack
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ hp + defense + sp_attack + sp_defense + speed + height_m + weight_kg,traindata) 
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1

#用線性回歸模型預測捕捉率
##選擇lm(formula = catch_rate ~ height_m + sp_defense + speed)
n <- nrow(kmeans1_subset)
set.seed(345132)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans1_subset[subset,]
testdata <- kmeans1_subset[ - subset,]
model1 <- lm(catch_rate ~ height_m + sp_defense + speed,traindata)
future <- predict(model1,testdata)
future <- as.data.frame(future)
final1 <- cbind(testdata,future)
for (i in 1:9) { 
  final1[,i] <- (final1[,i] * sd(kmeans1[,i])) + mean(kmeans1[,i]) 
} 
final1[,10] <- (final1[,10] * sd(kmeans1[,9])) + mean(kmeans1[,9]) 
realdiff1 <- abs(final1$catch_rate-final1$future)
avg_realdiff1 <- sum(realdiff1)/nrow(final1)
avg_realdiff1

#第一群，選擇lm(formula = catch_rate ~ height_m + sp_defense + speed)比較好

#21.kmeans2模型----------------------------------------------------------------------
kmeans2_subset <- scale(kmeans2[,1:9])
kmeans2_subset <- data.frame(kmeans2_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans2_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans2_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans2_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點
kmeans2_subset <- kmeans2_subset[-c(5,33,34), ]
model=lm(data=kmeans2_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)

#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans2_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans2_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans2_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans2_subset[,c(1:8)],y=kmeans2_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
##可以搭配identify函數畫出all possible 篩選法的Cp圖，並即時點選最佳的模型。Cp圖的X座標是各候選模型的迴歸係數數目，Y座標是相對的Cp值。
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~height_m+hp+attack+sp_attack+speed+defense+weight_kg ) #刪除sp_defense

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~height_m + hp + defense + sp_attack + speed )

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~height_m+hp+attack+sp_attack+speed+defense+weight_kg ) #刪除sp_defense
n <- nrow(kmeans2_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans2_subset[subset,]
testdata <- kmeans2_subset[ - subset,]
model2 <- lm(catch_rate ~ height_m+hp+attack+sp_attack+speed+defense+weight_kg,traindata)
future <- predict(model2,testdata)
future <- as.data.frame(future)
final2 <- cbind(testdata,future)
for (i in 1:9) { 
  final2[,i] <- (final2[,i] * sd(kmeans2[,i])) + mean(kmeans2[,i]) 
} 
final2[,10] <- (final2[,10] * sd(kmeans2[,9])) + mean(kmeans2[,9]) 
realdiff2 <- abs(final2$catch_rate-final2$future)
avg_realdiff2 <- sum(realdiff2)/nrow(final2)
avg_realdiff2

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~height_m + hp + defense + sp_attack + speed )
n <- nrow(kmeans2_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans2_subset[subset,]
testdata <- kmeans2_subset[ - subset,]
model2 <- lm(catch_rate ~ height_m + hp + defense + sp_attack + speed,traindata)
future <- predict(model2,testdata)
future <- as.data.frame(future)
final2 <- cbind(testdata,future)
for (i in 1:9) { 
  final2[,i] <- (final2[,i] * sd(kmeans2[,i])) + mean(kmeans2[,i]) 
} 
final2[,10] <- (final2[,10] * sd(kmeans2[,9])) + mean(kmeans2[,9]) 
realdiff2 <- abs(final2$catch_rate-final2$future)
avg_realdiff2 <- sum(realdiff2)/nrow(final2)
avg_realdiff2

#第二群，選擇lm(catch_rate~height_m + hp + defense + sp_attack + speed )比較好

#22.kmeans3模型----------------------------------------------------------------------
kmeans3_subset <- scale(kmeans3[,1:9])
kmeans3_subset <- data.frame(kmeans3_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans3_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans3_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans3_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點
kmeans3_subset <- kmeans3_subset[-c(49,68,130), ]
model=lm(data=kmeans3_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
kmeans3_subset <- kmeans3_subset[-66, ]
model=lm(data=kmeans3_subset,catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)

#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans3_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans3_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans3_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans3_subset[,c(1:8)],y=kmeans3_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~defense+hp+attack+sp_defense+speed+weight_kg ) #刪除height_m、sp_attack
##選擇lm(catch_rate~defense+hp+attack+sp_defense+speed+weight_kg+sp_attack ) #刪除height_m

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~defense+hp+attack+sp_attack+sp_defense+speed ) #刪除weight_kg

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~defense+hp+attack+sp_attack+sp_defense+speed ) #刪除weight_kg
n <- nrow(kmeans3_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans3_subset[subset,]
testdata <- kmeans3_subset[ - subset,]
model3 <- lm(catch_rate ~ defense+hp+attack+sp_defense+speed+weight_kg ,traindata) 
future <- predict(model3,testdata)
future <- as.data.frame(future)
final3 <- cbind(testdata,future)
for (i in 1:9) { 
  final3[,i] <- (final3[,i] * sd(kmeans3[,i])) + mean(kmeans3[,i]) 
} 
final3[,10] <- (final3[,10] * sd(kmeans3[,9])) + mean(kmeans3[,9]) 
realdiff3 <- abs(final3$catch_rate-final3$future)
avg_realdiff3 <- sum(realdiff3)/nrow(final3)
avg_realdiff3

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~defense+hp+attack+sp_defense+speed+weight_kg ) #刪除height_m、sp_attack
n <- nrow(kmeans3_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans3_subset[subset,]
testdata <- kmeans3_subset[ - subset,]
model3 <- lm(catch_rate ~ defense+hp+attack+sp_defense+speed+weight_kg ,traindata) 
future <- predict(model3,testdata)
future <- as.data.frame(future)
final3 <- cbind(testdata,future)
for (i in 1:9) { 
  final3[,i] <- (final3[,i] * sd(kmeans3[,i])) + mean(kmeans3[,i]) 
} 
final3[,10] <- (final3[,10] * sd(kmeans3[,9])) + mean(kmeans3[,9]) 
realdiff3 <- abs(final3$catch_rate-final3$future)
avg_realdiff3 <- sum(realdiff3)/nrow(final3)
avg_realdiff3

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~defense+hp+attack+sp_defense+speed+weight_kg+sp_attack ) #刪除height_m
n <- nrow(kmeans3_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans3_subset[subset,]
testdata <- kmeans3_subset[ - subset,]
model3 <- lm(catch_rate ~ defense+hp+attack+sp_defense+speed+weight_kg+sp_attack ,traindata) 
future <- predict(model3,testdata)
future <- as.data.frame(future)
final3 <- cbind(testdata,future)
for (i in 1:9) { 
  final3[,i] <- (final3[,i] * sd(kmeans3[,i])) + mean(kmeans3[,i]) 
} 
final3[,10] <- (final3[,10] * sd(kmeans3[,9])) + mean(kmeans3[,9]) 
realdiff3 <- abs(final3$catch_rate-final3$future)
avg_realdiff3 <- sum(realdiff3)/nrow(final3)
avg_realdiff3

#第三群，選擇lm(catch_rate~defense+hp+attack+sp_defense+speed+weight_kg+sp_attack ) #刪除height_m比較好

#23.kmeans4模型----------------------------------------------------------------------
kmeans4_subset <- scale(kmeans4[,1:9])
kmeans4_subset <- data.frame(kmeans4_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans4_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans4_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans4_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點


#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans4_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans4_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans4_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans4_subset[,c(1:8)],y=kmeans4_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~hp+attack+sp_attack+sp_defense+height_m ) #刪除defense、weight_kg、speed
##選擇lm(catch_rate~hp+attack+sp_attack+sp_defense+height_m+defense ) #刪除weight_kg、speed

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~height_m + hp + attack + sp_attack + sp_defense) #刪除defense、weight_kg、speed

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~hp+attack+sp_attack+sp_defense+height_m ) #刪除defense、weight_kg、speed
n <- nrow(kmeans4_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans4_subset[subset,]
testdata <- kmeans4_subset[ - subset,]
model4 <- lm(catch_rate ~ hp+attack+sp_attack+sp_defense+height_m,traindata) 
future <- predict(model4,testdata)
future <- as.data.frame(future)
final4 <- cbind(testdata,future)
for (i in 1:9) { 
  final4[,i] <- (final4[,i] * sd(kmeans4[,i])) + mean(kmeans4[,i]) 
} 
final4[,10] <- (final4[,10] * sd(kmeans4[,9])) + mean(kmeans4[,9]) 
realdiff4 <- abs(final4$catch_rate-final4$future)
avg_realdiff4 <- sum(realdiff4)/nrow(final4)
avg_realdiff4

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~hp+attack+sp_attack+sp_defense+height_m+defense ) #刪除weight_kg、speed
n <- nrow(kmeans4_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans4_subset[subset,]
testdata <- kmeans4_subset[ - subset,]
model4 <- lm(catch_rate ~ hp+attack+sp_attack+sp_defense+height_m+defense,traindata) 
future <- predict(model4,testdata)
future <- as.data.frame(future)
final4 <- cbind(testdata,future)
for (i in 1:9) { 
  final4[,i] <- (final4[,i] * sd(kmeans4[,i])) + mean(kmeans4[,i]) 
} 
final4[,10] <- (final4[,10] * sd(kmeans4[,9])) + mean(kmeans4[,9]) 
realdiff4 <- abs(final4$catch_rate-final4$future)
avg_realdiff4 <- sum(realdiff4)/nrow(final4)
avg_realdiff4

#第四群，選擇lm(catch_rate~hp+attack+sp_attack+sp_defense+height_m+defense ) #刪除weight_kg、speed比較好

#24.kmeans5模型----------------------------------------------------------------------
kmeans5_subset <- scale(kmeans5[,1:9])
kmeans5_subset <- data.frame(kmeans5_subset)

##偵測資料中的影響點(Cook's distance)
attach(kmeans5_subset)
model=lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model)
##Cook’s D衡量每一個觀察值被移除後，對於迴歸係數估計值的影響是否顯著。
##通常以F(0.5, p, n-p)查表值當作比較的門檻值
which(as.vector(cooks.distance(model))>qf(0.5,9,nrow(kmeans5_subset)-9))
##DFBETAS與Cook’s D一樣是以迴歸係數估計變化量的大小當作影響點的偵測指標，若DFBETAS值大於1，則相對的觀察值可能是影響點
which(as.matrix(abs(dfbetas(model)))>1)%%nrow(kmeans5_subset)
##DFFITS衡量觀察值被移除後，對於應變數估計值的影響，若DFFITS值大於1，則相對的觀察值可能是影響點
which(as.vector(abs(dffits(model)))>1)

##刪除影響點


#是否有共線性?(VIF)
##解釋變數之間如果存在嚴重的共線性問題，則某些解釋變數的VIF值應該會很大。
##一般判斷標準是若VIF值大於10，則可能有共線性問題。可用car套件的VIF函數來計算模型各解釋變數的VIF
vif(model)
mean(vif(model))
##但是沒有任何解釋變數的VIF值超過10，故這個模型的共線性不是很嚴重。

#是否有相關性?(共變異數)
cor(kmeans5_subset[,1:8])
##畫熱圖
ggplot(melt(cor(kmeans5_subset[, 1:8])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
##找熱圖中共變異數大的值
cor(kmeans5_subset[, 1:8])

#模型選擇(CP值法)
model01 <- lm(catch_rate~height_m+weight_kg+hp+attack+defense+sp_attack+sp_defense+speed)
summary(model01)
out.all=regsubsets(kmeans5_subset[,c(1:8)],y=kmeans5_subset$catch_rate, nbest=3, method="exhaustive")
s.all=summary(out.all)
##將候選模型的R2(rsq), SSE(rss), R2(adj), Cp, BIC值列出
round(cbind(s.all$which, rsq=s.all$rsq, adjr2=s.all$adjr2, rss=s.all$rss, cp=s.all$cp, bic=s.all$bic),3)
q=as.vector(rowSums(s.all$which)) #迴歸係數數目
plot(q, s.all$cp, xlim=c(1,9),ylim=c(0,200))
abline(0, b=1)
##選擇lm(catch_rate~attack+sp_attack+defense ) 

#模型選擇(stepwise值法)
step(model01)
model_stepwise<-step(model01, test="F", direction = "both")
summary(model_stepwise)
##選擇lm(catch_rate~attack + defense + sp_attack + sp_defense) 

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~attack+sp_attack+defense ) 
n <- nrow(kmeans5_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans5_subset[subset,]
testdata <- kmeans5_subset[ - subset,]
model5 <- lm(catch_rate ~ attack+sp_attack+defense,traindata) 
future <- predict(model5,testdata)
future <- as.data.frame(future)
final5 <- cbind(testdata,future)
for (i in 1:9) { 
  final5[,i] <- (final5[,i] * sd(kmeans5[,i])) + mean(kmeans5[,i]) 
} 
final5[,10] <- (final5[,10] * sd(kmeans5[,9])) + mean(kmeans5[,9]) 
realdiff5 <- abs(final5$catch_rate-final5$future)
avg_realdiff5 <- sum(realdiff5)/nrow(final5)
avg_realdiff5

#用線性回歸模型預測捕捉率
##選擇lm(catch_rate~attack + defense + sp_attack + sp_defense) 
n <- nrow(kmeans5_subset)
set.seed(74527)
subset <- sample(seq_len(n), size = round(0.7 * n))
traindata <- kmeans5_subset[subset,]
testdata <- kmeans5_subset[ - subset,]
model5 <- lm(catch_rate ~ attack + defense + sp_attack + sp_defense,traindata) 
future <- predict(model5,testdata)
future <- as.data.frame(future)
final5 <- cbind(testdata,future)
for (i in 1:9) { 
  final5[,i] <- (final5[,i] * sd(kmeans5[,i])) + mean(kmeans5[,i]) 
} 
final5[,10] <- (final5[,10] * sd(kmeans5[,9])) + mean(kmeans5[,9]) 
realdiff5 <- abs(final5$catch_rate-final5$future)
avg_realdiff5 <- sum(realdiff5)/nrow(final5)
avg_realdiff5

#第五群，選擇lm(catch_rate~attack + defense + sp_attack + sp_defense)比較好

#25.全部預測誤差平均-------------------------------------------------------------
(sum(realdiff1)+sum(realdiff2)+sum(realdiff3)+sum(realdiff4)+sum(realdiff5))/(length(realdiff1)+
                                                                 length(realdiff2)+length(realdiff3)+length(realdiff4)+length(realdiff5))

#全部寶可夢的捕捉率預測
##丟新資料要分哪一群?
##新資料離哪一群中心近就丟哪群
##第一群模型model1
##第二群模型model2
##第三群模型model3
##第四群模型model4
##第五群模型model5
pokemon_testdata <- read_excel("C:/Users/owo/Desktop/寶可夢數據.xlsx")
x <- pokemon_testdata[,3:11]
distance_1 <- as.data.frame(1:nrow(x))
distance_2 <- as.data.frame(1:nrow(x))
distance_3 <- as.data.frame(1:nrow(x))
distance_4 <- as.data.frame(1:nrow(x))
distance_5 <- as.data.frame(1:nrow(x))
predict_1 <- as.data.frame(1:nrow(x))
predict_2 <- as.data.frame(1:nrow(x))
predict_3 <- as.data.frame(1:nrow(x))
predict_4 <- as.data.frame(1:nrow(x))
predict_5 <- as.data.frame(1:nrow(x))

for (j in 1:nrow(x)) { 
  
  a=0
  b=0
  c=0
  d=0
  e=0
  for (i in 1:8) { 
    a <- a + (x[j,i]-mean(final1[,i]))^2
    b <- b + (x[j,i]-mean(final2[,i]))^2
    c <- c + (x[j,i]-mean(final3[,i]))^2
    d <- d + (x[j,i]-mean(final4[,i]))^2
    e <- e + (x[j,i]-mean(final5[,i]))^2
  } 
  ##選距離最小的判斷丟哪群
  distance_1[j,1] <- a^(1/2)
  distance_2[j,1] <- b^(1/2)
  distance_3[j,1] <- c^(1/2)
  distance_4[j,1] <- d^(1/2)
  distance_5[j,1] <- e^(1/2)
  ##預測x的捕捉率
  x1=0*x
  x2=0*x
  x3=0*x
  x4=0*x
  x5=0*x
  ##將x做標準化
  for (i in 1:8) { 
    x1[j,i] <- (x[j,i]-mean(kmeans1[,i]))/sd(kmeans1[,i])
    x2[j,i] <- (x[j,i]-mean(kmeans2[,i]))/sd(kmeans2[,i])
    x3[j,i] <- (x[j,i]-mean(kmeans3[,i]))/sd(kmeans3[,i])
    x4[j,i] <- (x[j,i]-mean(kmeans4[,i]))/sd(kmeans4[,i])
    x5[j,i] <- (x[j,i]-mean(kmeans5[,i]))/sd(kmeans5[,i])
  } 
  ##將標準化後的x代進模型做預測
  A1 <- predict(model1,x1[j,])
  A2 <- predict(model2,x2[j,])
  A3 <- predict(model3,x3[j,])
  A4 <- predict(model4,x4[j,])
  A5 <- predict(model5,x5[j,])
  ##將捕捉率預測結果還原標準化，變成一般數值
  (predict_1[j,1] <- (A1 * sd(kmeans1[,9])) + mean(kmeans1[,9]) )
  (predict_2[j,1] <- (A2 * sd(kmeans2[,9])) + mean(kmeans2[,9]) )
  (predict_3[j,1] <- (A3 * sd(kmeans3[,9])) + mean(kmeans3[,9]) )
  (predict_4[j,1] <- (A4 * sd(kmeans4[,9])) + mean(kmeans4[,9]) )
  (predict_5[j,1] <- (A5 * sd(kmeans5[,9])) + mean(kmeans5[,9]) )
}

Final_prediction <- cbind(pokemon_testdata,distance_1,distance_2
                          ,distance_3,distance_4,distance_5,predict_1,predict_2,predict_3,predict_4,predict_5)
names(Final_prediction) <- c("name","rare","height_m","weight_kg","hp","attack"
                             ,"defense","sp_attack","sp_defense","speed"
                             ,"catch_rate","distance_1","distance_2"
                             ,"distance_3","distance_4","distance_5"
                             ,"predict_1","predict_2"
                             ,"predict_3","predict_4","predict_5")


#匯出CSV檔
write.csv(Final_prediction, file="分5群全部預測.csv", row.names = TRUE)

#26.畫出分5群預測差值圖表---------------------------------------------------------------
catch_rate_3 <- read_excel("C:/Users/owo/Desktop/寶可夢捕捉率預測.xlsx", sheet = "捕捉率3")
catch_rate_45 <- read_excel("C:/Users/owo/Desktop/寶可夢捕捉率預測.xlsx", sheet = "捕捉率45")
catch_rate_60 <- read_excel("C:/Users/owo/Desktop/寶可夢捕捉率預測.xlsx", sheet = "捕捉率60")
catch_rate_75 <- read_excel("C:/Users/owo/Desktop/寶可夢捕捉率預測.xlsx", sheet = "捕捉率75")
catch_rate_90 <- read_excel("C:/Users/owo/Desktop/寶可夢捕捉率預測.xlsx", sheet = "捕捉率90")
catch_rate_120 <- read_excel("C:/Users/owo/Desktop/寶可夢捕捉率預測.xlsx", sheet = "捕捉率120")
catch_rate_190 <- read_excel("C:/Users/owo/Desktop/寶可夢捕捉率預測.xlsx", sheet = "捕捉率190")
catch_rate_255 <- read_excel("C:/Users/owo/Desktop/寶可夢捕捉率預測.xlsx", sheet = "捕捉率255")

ggplot(catch_rate_3, aes(x=估計誤差, y=total_point))+geom_point()+ggtitle("捕捉率 3")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous("實際捕捉率和分五群捕捉率的差值")+scale_y_continuous("總能力值")
ggplot(catch_rate_45, aes(x=估計誤差, y=total_point))+geom_point()+ggtitle("捕捉率 45")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous("實際捕捉率和分五群捕捉率的差值")+scale_y_continuous("總能力值")
ggplot(catch_rate_60, aes(x=估計誤差, y=total_point))+geom_point()+ggtitle("捕捉率 60")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous("實際捕捉率和分五群捕捉率的差值")+scale_y_continuous("總能力值")
ggplot(catch_rate_75, aes(x=估計誤差, y=total_point))+geom_point()+ggtitle("捕捉率 75")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous("實際捕捉率和分五群捕捉率的差值")+scale_y_continuous("總能力值")
ggplot(catch_rate_90, aes(x=估計誤差, y=total_point))+geom_point()+ggtitle("捕捉率 90")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous("實際捕捉率和分五群捕捉率的差值")+scale_y_continuous("總能力值")
ggplot(catch_rate_120, aes(x=估計誤差, y=total_point))+geom_point()+ggtitle("捕捉率 120")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous("實際捕捉率和分五群捕捉率的差值")+scale_y_continuous("總能力值")
ggplot(catch_rate_190, aes(x=估計誤差, y=total_point))+geom_point()+ggtitle("捕捉率 190")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous("實際捕捉率和分五群捕捉率的差值")+scale_y_continuous("總能力值")
ggplot(catch_rate_255, aes(x=估計誤差, y=total_point))+geom_point()+ggtitle("捕捉率 255")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous("實際捕捉率和分五群捕捉率的差值")+scale_y_continuous("總能力值")
