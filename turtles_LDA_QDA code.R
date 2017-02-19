library(MASS)

###############
####  LDA  ####
###############

turtles.lda=lda(Gender ~ ., data=turtles)
table(turtles$Gender,predict(turtles.lda)$class)


turtles.lda.cv=lda(Gender ~ ., CV=TRUE,data=turtles)
table(turtles$Gender,turtles.lda.cv$class)


turtles.lda.xval=rep(0,length=48)
x=rep(1:10,length=48)

x=sample(x)

for(i in 1:10){
  train=turtles[x!=i,]
  test=turtles[x==i,]
  glub=lda(Gender~Length+Width+Height,data=train)
  turtles.lda.xval[x==i]=predict(glub,test)$class
}
table(turtles$Gender,turtles.lda.xval)

###############
####  QDA  ####
###############


turtles.qda=qda(Gender ~ ., data=turtles)
table(turtles$Gender,predict(turtles.qda)$class)


turtles.qda.cv=qda(Gender ~ ., CV=TRUE,data=turtles)
table(turtles$Gender,turtles.qda.cv$class)


turtles.qda.xval=rep(0,length=48)
turtles.qda.xval

x=rep(1:10,length=48)
x=sample(x)

for(i in 1:10){
  train=turtles[x!=i,]
  test=turtles[x==i,]
  glub=qda(Gender~ . ,data=train)
  turtles.qda.xval[x==i]=predict(glub,test)$class
}
table(turtles$Gender,turtles.qda.xval)
