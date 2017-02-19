pkgTest <- function(x){
  if (!require(x,character.only = TRUE)){
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("ggvis")
library(ggvis)
library(MASS)

iris = read.csv("Iris.csv") 


iris %>% ggvis(~SepalLength, ~SepalWidth, fill = ~Species) %>% layer_points()
iris %>% ggvis(~PetalLength, ~PetalWidth, fill = ~Species) %>% layer_points()


#Summarize the four measured variables for the three types of iris. Are 
#the data approximately normal, and do they look like they have the same 
#covariance matrix for all 3 species?

summary(iris)

setosa = iris[iris$Species == "setosa",-5]
versicolor = iris[iris$Species == "versicolor",-5]
virginica = iris[iris$Species == "virginica",-5]

##Covariance Matrices are not equal
cov(setosa)
cov(versicolor)
cov(virginica)

##====================Pooled Covariance Matrix======================
#install.packages("sparsediscrim")
#library(sparsediscrim)
#cov_pool(iris[, -5], iris$Species)
##==================================================================

##Test Normality
lm_setosa = lm(setosa)
lm_versicolor = lm(versicolor)
lm_virginica = lm(virginica)


summary(lm_setosa)
qqnorm(lm_setosa$residuals, main = "Setosa Q-Q Plot")
qqline(lm_setosa$residuals)
#A little wiggle about the line

summary(lm_versicolor)
qqnorm(lm_versicolor$residuals, main = "Versicolor Q-Q Plot")
qqline(lm_versicolor$residuals)
#Weak Tails

summary(lm_virginica)
qqnorm(lm_virginica$residuals, main = "Virginica Q-Q Plot")
qqline(lm_virginica$residuals)
#Strongest Normality

##Applicatoin of LDA
iris.lda=lda(Species ~ ., data=iris)
table(iris$Species,predict(iris.lda)$class)

iris.lda.cv=lda(Species ~ ., CV=TRUE,data=iris)
table(iris$Species,iris.lda.cv$class)

iris.lda.xval=rep(0,length=150)
x=rep(1:10,length=150)

x=sample(x)

for(i in 1:10){
  train=iris[x!=i,]
  test=iris[x==i,]
  glub=lda(Species~.,data=train)
  iris.lda.xval[x==i]=predict(glub,test)$class
}
table(iris$Species,iris.lda.xval)

#Application of QDA
