library(readxl)
HOUSE <- read_excel("C:/Users/lenovo/Desktop/HOUSE.xlsx")
head(HOUSE)
attach(HOUSE)
library(MASS)
#converting the categorical variables as factor variables.
HOUSE$ST<-as.factor(HOUSE$ST)
HOUSE$CON<-as.factor(HOUSE$CON)
HOUSE$GAR<-as.factor(HOUSE$GAR)
HOUSE$CDN<-as.factor(HOUSE$CDN)
HOUSE$L1<-as.factor(HOUSE$L1)
HOUSE$L2<-as.factor(HOUSE$L2)
#function to run the all types of model
#note that here the selection criterion for adding or removing the variable is aic.
#Minimal aic will be stopping criterion in  forward,backward and step  regression.
#I have assumed the house prices as a response variable and others as a predictor variable. 
Solution.1.
#this model is having all predictors.
fullmodel=lm(PRICE~.,HOUSE)
full.values=predict(fullmodel,HOUSE[,-1])
#for predicting values from this model.

#in forward selection regression,we start up with model having no predictors
m1=lm(PRICE~1,HOUSE)#this model has no predictor.
model1=step(m1,direction = "forward",scope=formula(fullmodel))
#the output of this forward model suggest that for the suitable model for predicting House prices will be
#having predictors  variables (FLR,ST, CON ,LOT ,BDR , RMS ,GAR ,L2).i.e
re.model=lm(PRICE ~ FLR + ST + CON + LOT + BDR + RMS + GAR + L2,HOUSE)


#in backward selection we start with full model having all variables and remove variables based on aic(minimum).
model2=step(fullmodel,direction="backward")
#the output of this backward model suggest that for the suitable model for predicting House prices will be
#having predictors  variables (FLR,ST, CON ,LOT ,BDR , RMS ,GAR ,L2) which are same as forward selection output.
#so the model suggested by this algorithm is  same as the re.model from forward.


#in step selection we start with the full model but here the removal and addition of variables both occurs.
#this is the logic behind this algorithm.
model3=step(fullmodel,direction="backward")
#the output of this algorithm also suggests the same model as the forward and the backward algorithm.

#note that it is not necessary that for every data set these three algorithm give same model.
#models can be different also,depending on the data.But in this case these model are same.



Solution.2.

#model below is the optimial I got from assignment 4.
opt.model=lm(PRICE~BDR+FLR+GAR+L2+ST+LOT,HOUSE)

#all the algorithm I have used here with respect to the optimal model,as the problem suggested. 

no.pred.model=lm(PRICE~1,HOUSE)
forward.model=step(no.pred.model,direction="forward",scope =formula(opt.model))
#model suggested by this forward selection for response variable price
#should have FLR , ST ,LOT , BDR ,GAR and L2  as predictors.
#so we will fit this model sugested by forward selection
re.forward=lm(HOUSE$PRICE ~ FLR + ST + LOT + BDR + GAR + L2,HOUSE)
forward.values=predict(re.forward,HOUSE[,-1])
#predicting values from this model


backward.model=step(opt.model,direction="backward")
#model suggested by this backward selection for estimating response variable price
#should have FLR , ST ,LOT , BDR ,GAR and L2  as predictors.
#so we will fit this model sugested by backward selection
re.backward=lm(PRICE ~ BDR + FLR + GAR + L2 + ST + LOT,HOUSE)
backward.values=predict(re.backward,HOUSE[,-1])
#predicting values from this model.


step.model=step(opt.model,direction = "both")
#model suggested by this stepwise selection for estimating response variable price
#should have FLR , ST ,LOT , BDR ,GAR and L2  as predictors.
#so we will fit this model sugested by stepwise selection.
re.step=lm(HOUSE$PRICE ~ BDR + FLR +  ST + LOT + GAR + L2,HOUSE)
step.values=predict(re.step,HOUSE[,-1])
#predicting values from this model.

#note that here also all these three algorithm gives same model.


# for plottting the required plot 
actual.values<-PRICE#given price values in the data.
predicted.values<-full.values#predicted values from full model(all).
plot(actual.values,predicted.values,pch=0,col="red",xlim=c(20,100),ylim=c(20,100))
points(PRICE,forward.values,col="blue",pch=1)
points(PRICE,backward.values,col="green",pch=2)
points(PRICE,step.values,col="black",pch=3)
legend("topleft",legend=c("estv.all","estv.f","estv.b","estv.s"),col=c("red","blue","green","black"),pch=c(0,1,2,3))
abline(0,1)
#here estv.all=estimated values of price(yi) from full model
#est.f=estimated values of price from forward model
#est.b=estimated values of price from backward model
#est.step=estimated values of price from step model

#The predicting power of the full model is 
summary(fullmodel)$adj.r.squared
summary(re.forward)$adj.r.squared
summary(re.backward)$adj.r.squared
summary(re.step)$adj.r.squared

#so from the above commands we will get the adjusted r-squared for all four models.
#there are several measure of comparing the predicting power of model and I have choosen adjusted r-squared.
#so the  full model is explaining  85% of variability of data and expected to perform with 85% accuracy.
#as we know Models from forward ,backward and step regression is same,so the adjusted-r-squared
#is also same for all these 3 models.So the adjusted-r-squared for all these 3 models is around 86.5%.
#and these models have better adjusted -r-squared compare to full model.
#So these model are expected to perform with 86.5% accuracy and have greater predictive power than full model.