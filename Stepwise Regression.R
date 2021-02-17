library(MASS)
data<-road
str(data)
attach(data)
model<-step(lm(deaths~.,data=data),direction="backward")
model<-step(lm(deaths~1,data=data),direction="forward",scope=~drivers+rural+popden+temp+fuel)
model<-step(lm(deaths~.,data=data),direction="both")