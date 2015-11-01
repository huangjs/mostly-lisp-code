mu.1 <- matrix(c(rnorm(10,-1,1.8),rnorm(10,1,1.8)),ncol=2)
mu.2 <- matrix(c(rnorm(10,1,1.8),rnorm(10,-1,1.8)),ncol=2)


# グラフ作成
plot(rbind(mu.1,mu.2),type="n")
points(mu.1,pch=1,col="blue")
points(mu.2,pch=2,col="red")

# 点の回りに20点ずつの乱数発生
x.1 <- matrix(c(rnorm(20,mu.1[1,1],0.5),rnorm(20,mu.1[1,2],0.5)),ncol=2)
for (i in c(2:10) ) {
  x.1 <- rbind(x.1,
                       matrix(c(rnorm(20,mu.1[i,1],0.5),rnorm(20,mu.1[i,2],0.5)),ncol=2))
}
x.2 <- matrix(c(rnorm(20,mu.2[1,1],0.5),rnorm(20,mu.2[1,2],0.5)),ncol=2)
for (i in c(2:10) ) {
  x.2 <- rbind(x.2,
                       matrix(c(rnorm(20,mu.2[i,1],0.5),rnorm(20,mu.2[i,2],0.5)),ncol=2))
}

# グラフ作成
plot(rbind(x.1,x.2),type="n")
points(x.1,pch=1,col="blue")
points(x.2,pch=2,col="red")


# グループを表す変数を作成
group <- vector("integer",length=400)
group[1:200] <- 0
group[201:400] <- 1

# これが学習用データ。
dump(c("mu.1", "mu.2","x.1", "x.2", "group"), file="~/programming/R/DMT-20071129-datafile.R")

# データフレームを作成 (第一軸がx、第二軸がy、クラスはz、離散変数がg)
x <- rbind(x.1,x.2)
training.data <- data.frame(z=group,x=x[,1],y=x[,2])
training.data$g[1:400] <- "A"
training.data$g[201:400] <- "B"
training.data$g <- as.factor(training.data$g)

# まず回帰分析
training.data.lm <- lm(z~x+y,data=training.data)
summary(training.data.lm)

# 次にロジスティック回帰分析
training.data.logistic <- glm(g~x+y, data=training.data, family="binomial")
summary(training.data.logistic)

# 線形判別分析
library(MASS)
training.data.lda <- lda(g~x+y, data=training.data)

# 二次判別分析
library(MASS)
training.data.qda <- qda(g~x+y, data=training.data)

# 一般化加法モデル (GAM)
library(mgcv)
training.data.gam <- gam(g~s(x)+s(y), data=training.data, family="binomial")
summary(training.data.gam)

# 樹形モデル
library(tree)
training.data.tree <- tree(g~x+y,data=training.data)
plot(training.data.tree)
text(training.data.tree)
## 分割の表示
plot(rbind(x.1,x.2), type="n", xlab="x", ylab="y")
points(x.1,pch=1,col="green")
points(x.2,pch=2,col="red")
partition.tree(training.data.tree,add=T,col="blue")
partition.tree(training.data.tree,add=F,col="blue")

# 樹形モデル
library(mvpart)
training.data.rpart <- rpart(g~x+y,data=training.data)
print(training.data.rpart,digit=2)
plot(training.data.rpart, uniform=T,branch=0.6,margin=0.05)
text(training.data.rpart, all=T, use.n=T)

# サポートベクターマシン
library(e1071)
training.data.svm <- svm(g~x+y,data=training.data)
plot(training.data$x, training.data$y,type="n")
text(training.data$x, 
       training.data$y,
       labels=predict(training.data.svm,training.data),
       col=(training.data$z+2),cex=0.7)

# k-nearest neighborhood
library(kknn)
# data(training.data)
training.data.kknn <- kknn(training.data$g~training.data$x+training.data$y,
                                       kernel="rectangular")
g.fit <- fitted(training.data.kknn)
table(training.data$g, g.fit)
plot(training.data$x, training.data$y,
       col=c("green", "red")[(training.data$g!=g.fit)+1])

# k-nearest neighborhood
library(MASS)
x <- cbind(training.data$x, training.data$y)

## k=1
training.data.knn <- knn(x,x,training.data$g,k=1)
table(training.data$g, training.data.knn)
plot(training.data$x, training.data$y, 
       pch=c((training.data$g!=training.data.knn)*2+1), 
       col=c("red", "blue")[c(training.data$z+1)])

## k=3
training.data.knn <- knn(x,x,training.data$g,k=3)
table(training.data$g, training.data.knn)
plot(training.data$x, training.data$y, 
       pch=c((training.data$g!=training.data.knn)*2+1), 
       col=c("red", "blue")[c(training.data$z+1)])

## k=15
training.data.knn <- knn(x,x,training.data$g,k=15)
table(training.data$g, training.data.knn)
plot(training.data$x, training.data$y, 
       pch=c((training.data$g!=training.data.knn)*2+1), 
       col=c("red", "blue")[c(training.data$z+1)])

## k=101
training.data.knn <- knn(x,x,training.data$g,k=101)
table(training.data$g, training.data.knn)
plot(training.data$x, training.data$y, 
       pch=c((training.data$g!=training.data.knn)*2+1), 
       col=c("red", "blue")[c(training.data$z+1)])

# ニューラルネットワーク
library(nnet)
training.data.nnet <- nnet(g~x+y, size=3, decay=0.1, data=training.data)
g.fit <- predict(training.data.nnet, type="class")
table(training.data$g, g.fit)
plot(training.data$x, training.data$y, 
       pch=c((training.data$g!=g.fit)*2+1), 
       col=c("red", "blue")[c(training.data$z+1)])
