


#### Workspace setup ####
# Use R Projects, not setwd().


adm<-read.csv('admission.csv',header = T)

set.seed(104)
head(adm)
plot(adm[30:31],main = 'scatterplot of poverty rate vs unemployment rate')
         
plot(adm[27:28],main = 'scatterplot of pct_bachelor and pct_prof')

plot(adm[23:26],main = 'pair correlation between race percentages')
cor(adm[23:31])

adm_sel<-adm[,-c(1:4,24,28,31)]
set.seed(2)
sample<-sample.int(n=1508,size=1131,replace=F)
train<-adm_sel[sample,]
test<-adm_sel[-sample,]
ncol(train)

model_full<-lm(ADM_RATE ~ .,data=train)
summary(model_full)

library(MASS)
stepAIC(lm(ADM_RATE ~ 1, data=train),
scope=list(upper=lm(ADM_RATE ~ ., data = train)),
direction = "forward", k=2)
stepAIC(lm(ADM_RATE ~ ., data=train),
scope=list(lower=lm(ADM_RATE ~ 1, data = train)),
direction = "backward", k=2)
stepAIC(lm(ADM_RATE ~ 1, data=train),
scope=list(upper=lm(ADM_RATE ~ ., data = train)),
direction = "forward", k=log(nrow(train)))
stepAIC(lm(ADM_RATE ~ ., data=train),
scope=list(lower=lm(ADM_RATE ~ 1, data = train)),
direction = "backward", k=log(nrow(train)))

stepAIC(lm(ADM_RATE ~ ., data=train), direction = "both", k=2)
stepAIC(lm(ADM_RATE ~ ., data=train), direction = "both", k=log(nrow(train)))

model1<-lm(formula = ADM_RATE ~ AVGFACSAL + CONTROL + POVERTY_RATE + HBCU + COSTT4_A + NUMBRANCH + FEMALE + PFTFAC + MD_FAMINC + HSI + PCT_BORN_US + REGION, data = train)

model2<-lm(formula = ADM_RATE ~ NUMBRANCH + CONTROL + REGION + TRIBAL +  HSI + COSTT4_A + AVGFACSAL + PFTFAC + PAR_ED_PCT_1STGEN + FEMALE + MD_FAMINC + PCT_WHITE + PCT_HISPANIC + PCT_BA, data = train) 

model3<-lm(formula = ADM_RATE ~ AVGFACSAL + CONTROL + POVERTY_RATE + HBCU + COSTT4_A + NUMBRANCH + FEMALE, data = train) 

model4<-lm(formula = ADM_RATE ~ NUMBRANCH + HSI + COSTT4_A + AVGFACSAL + PFTFAC + FEMALE + MD_FAMINC + PCT_WHITE + PCT_HISPANIC, data = train)

select_criteria = function(model, n)
{
SSres <- sum(model$residuals^2)
5
Rsq_adj <- summary(model)$adj.r.squared
p <- length(model$coefficients) - 1
AIC <- n*log(SSres/n) + 2*p
AICc <- AIC + (2*(p+2)*(p+3)/(n-p-1))
BIC <- n*log(SSres/n) + (p+2)*log(n)
res <- c(SSres, Rsq_adj, AIC, AICc, BIC,p)
names(res) <- c("SSres", "Rsq_adj", "AIC", "AIC_c", "BIC","p")
return(res)
}
n<-nrow(train)
results<-rbind(select_criteria(model1,n),select_criteria(model2,n),select_criteria(model3,n),select_criteria(model4,n))
results

library(Metrics)
genError <-function(prediction,actual)
    return(list(MAE = signif(mae(actual,prediction),4),RMSE = signif(rmse(actual,prediction),4)))


model1.pred<-predict(model1,test)
error = genError(model1.pred,test$ADM_RATE)
model1.results<-data.frame(MAE = error$MAE,RMSE = error$RMSE,model = "model1")
model1.results

model2.pred<-predict(model2,test)
error = genError(model2.pred,test$ADM_RATE)
model2.results<-data.frame(MAE = error$MAE,RMSE = error$RMSE,model = "model2")
model2.results


model3.pred<-predict(model3,test)
error = genError(model3.pred,test$ADM_RATE)
model3.results<-data.frame(MAE = error$MAE,RMSE = error$RMSE,model = "model3")
model3.results

model4.pred<-predict(model4,test)
error = genError(model4.pred,test$ADM_RATE)
model4.results<-data.frame(MAE = error$MAE,RMSE = error$RMSE,model = "model4")
model4.results

qqnorm(rstudent(model3))
qqline(rstudent(model3))

plot(rstandard(model3) ~ train$POVERTY_RATE,xlab="POVERTY_RATE",ylab="Residuals")

h <- hatvalues(model3)
threshold <- 2 * (length(model3$coefficients)/nrow(train))
w <- which(h > threshold)
train_new<-train[-w,]


model_up<-lm(formula = ADM_RATE ~ AVGFACSAL + CONTROL + POVERTY_RATE + HBCU + COSTT4_A + NUMBRANCH + FEMALE, data = train_new) 
summary(model_up)

model3_train<-predict(model3,train)
error_train = genError(model3_train,train$ADM_RATE)
model3_train_results<-data.frame(MAE = error_train$MAE,RMSE = error_train$RMSE,model = "model3")
model3_train_results
model3.results


summary(model3)

