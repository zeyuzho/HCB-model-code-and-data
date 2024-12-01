# install.packages("GAMM")
# install.packages("mgcv")
# install.packages("locfit")
library(locfit)
library(mgcv)
library(gamm)

FittingEvaluationIndex<-function(EstiH,ObsH,p){
  Index<-array(dim=13)
  n <- length(ObsH)
  p <- p
  e <- ObsH-EstiH
  e1 <- ObsH-mean(ObsH)
  pe <- mean(e)
  po <- mean(ObsH)
  var2 <- var(e)
  MSE <- pe^2+var2
  MPSE <- sum(abs((ObsH-EstiH)/EstiH))/n*100
  RMSE<-sqrt(pe^2+var2)
  RRMSE<-(sqrt(pe^2+var2))*(1/po)
  R2 <- 1-sum(e^2)/sum((e1)^2)
  adjR2<-(1-sum(e^2)/sum((e1)^2))*((n-1)/(n-p-1))
  TRE <- 100*sum(e^2)/sum((EstiH)^2)
  MAE <- sum(abs(e))/n
  MAPE <- sum(abs(e/po))/n
  MAB <- sum(abs(e))/n
  MPB <- (sum(abs(e))/sum(ObsH))*100
  Index[1]<-pe
  Index[2]<-MSE
  Index[3]<-MPSE
  Index[4]<-RMSE
  Index[5]<-RRMSE
  Index[6]<-R2
  Index[7]<-adjR2
  Index[8]<-var2
  Index[9]<-TRE
  Index[10] <- MAE
  Index[11] <- MAPE
  Index[12] <- MAB
  Index[13] <- MPB
  dimnames(Index)<-list(c("pe","MSE","MPSE","RMSE","RRMSE","R2","adjR2","Var","TRE","MAE","MAPE","MAB","MPB"))
  return(Index)}


##-- HCB

np.b2.cr.HCB <- mgcv::gam(HCB~s(CW,bs="cr")+s(H,bs="cr")+s(Dq,bs="cr")+s(density,bs="cr")+s(DT,bs="cr")+s(HT,bs="cr")+s(Dg,bs="cr")
                          +s(Gini_H,bs="cr")+s(BAL,bs="cr")+s(Td,bs="cr")+s(Ud,bs="cr",k=5)+s(M,bs="cr",k=3)+s(Ucw,bs="cr",k=3)+s(Uh,bs="cr",k=3),family=gaussian(),data=essay_train,method="GACV.Cp")
P.np.b2.cr.HCB <- predict(np.b2.cr.HCB,newdata=essay_test,type="response")
FittingEvaluationIndex(fitted(np.b2.cr.HCB),essay_train$HCB,6)
FittingEvaluationIndex(P.np.b2.cr.HCB,essay_test$HCB,1)
summary(np.b2.cr.HCB);AIC(np.b2.cr.HCB)

stat <- NULL
mod <- list()
for (i in 1:5){
  mod[[i]] <- gam(HCB~s(CW,bs="cr")+s(H,bs="cr")+s(Dq,bs="cr")+s(density,bs="cr")+s(DT,bs="cr")+s(HT,bs="cr")+s(Dg,bs="cr")
                  +s(Gini_H,bs="cr")+s(BAL,bs="cr")+s(Td,bs="cr")+s(Ud,bs="cr",k=i)+s(M,bs="cr",k=i)+s(Ucw,bs="cr",k=i)+s(Uh,bs="cr",k=i),family=gaussian(),data=essay_train,method="GACV.Cp")
  tt <- sum(abs((pacf(mod[[i]]$residuals))$acf))
  stat <- append(stat, tt)
}
plot(c(1:5), stat, type="l")
points(stat,pch=16)

qplot(temp, np.b2.cr.HCB, type="n")+poly(c(temp, rev(temp)), 
                                c(low95,rev(up95)))

par(mfrow=c(5,3))
par(mar=c(4,4,2,2))
plot(np.b2.tp.HCB, select = 1, pch = 20, shade = TRUE, residuals = T,xlab = "CW",ylab = "residuals")
plot(np.b2.tp.HCB, select = 2, pch = 20, shade = TRUE, residuals = F,xlab = "Dq",ylab = "residuals")
plot(np.b2.tp.HCB, select = 3, pch = 20, shade = TRUE, residuals = F,xlab = "density",ylab = "residuals")
plot(np.b2.tp.HCB, select = 4, pch = 20, shade = TRUE, residuals = F,xlab = "Gini_D",ylab = "residuals")
plot(np.b2.tp.HCB, select = 5, pch = 20, shade = TRUE, residuals = F,xlab = "DT",ylab = "residuals")
plot(np.b2.tp.HCB, select = 6, pch = 20, shade = TRUE, residuals = F,xlab = "HT",ylab = "residuals")
plot(np.b2.tp.HCB, select = 7, pch = 20, shade = TRUE, residuals = F,xlab = "Dg",ylab = "residuals")
plot(np.b2.tp.HCB, select = 8, pch = 20, shade = TRUE, residuals = F,xlab = "Gini_H",ylab = "residuals")
plot(np.b2.tp.HCB, select = 9, pch = 20, shade = TRUE, residuals = F,xlab = "Gini_BA",ylab = "residuals")
plot(np.b2.tp.HCB, select = 10, pch = 20, shade = TRUE, residuals = F,xlab = "BAL",ylab = "residuals")
plot(np.b2.tp.HCB, select = 11, pch = 20, shade = TRUE, residuals = F,xlab = "uaCI",ylab = "residuals")
plot(np.b2.tp.HCB, select = 12, pch = 20, shade = TRUE, residuals = F,xlab = "Gini_H",ylab = "residuals")
plot(np.b2.tp.HCB, select = 13, pch = 20, shade = TRUE, residuals = F,xlab = "Gini_BA",ylab = "residuals")
plot(np.b2.tp.HCB, select = 14, pch = 20, shade = TRUE, residuals = F,xlab = "BAL",ylab = "residuals")
plot(np.b2.tp.HCB, select = 15, pch = 20, shade = TRUE, residuals = F,xlab = "uaCI",ylab = "residuals")
# plot(np.b2.cr.HCB)
plot.gam(np.b2.cr.HCB, residuals = F, pch =1, cex = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, pages = 1, all.terms = T)

par(mfrow=c(1,1))
vis.gam(np.b2.cr.HCB, residuals = F, pch =1, cex = 1, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE, pages = 1, all.terms = T)

plot(np.b2.tp.HCB,pages=1,shade=T)





np.b2.tp.HCB<-mgcv::gam(HCB~s(CW,bs="tp")+s(H,bs="tp")+s(Dq,bs="tp")+s(density,bs="tp")+s(DT,bs="tp")+s(HT,bs="tp")+s(Dg,bs="tp")
                        +s(Gini_H,bs="tp")+s(BAL,bs="tp")+s(Td,bs="tp")+s(Ud,bs="tp",k=3)+s(M,bs="tp",k=3)+s(Ucw,bs="tp",k=3)+s(Uh,bs="tp",k=3),family=gaussian(),data=essay_train,method="GACV.Cp")
P.np.b2.tp.HCB <- predict(np.b2.tp.HCB,newdata=essay_test,type="response")
FittingEvaluationIndex(fitted(np.b2.tp.HCB),essay_train$HCB,6)
FittingEvaluationIndex(P.np.b2.tp.HCB,essay_test$HCB,1)
summary(np.b2.tp.HCB);AIC(np.b2.tp.HCB)


np.b2.ds.HCB<-mgcv::gam(HCB~s(D,bs="ds")+s(H,bs="ds")+s(H,bs="ds")+s(CW,bs="ds")+s(Dq,bs="ds")+s(density,bs="ds")+s(HT,bs="ds")+s(Dg,bs="ds")
                        +s(Gini_H,bs="ds")+s(BAL,bs="ds")+s(M,bs="ds",m=c(1,.1),k=3)+s(Uh,bs="ds",m=c(1,.1),k=3),family=gaussian(),data=essay_train,method="GACV.Cp")
P.np.b2.ds.HCB <- predict(np.b2.ds.HCB,newdata=essay_test,type="response")
FittingEvaluationIndex(fitted(np.b2.ds.HCB),essay_train$HCB,6)
FittingEvaluationIndex(P.np.b2.ds.HCB,essay_test$HCB,1)
summary(np.b2.ds.HCB);AIC(np.b2.ds.HCB)


np.b2.ps.HCB<-mgcv::gam(HCB~s(D,bs="ps")+s(H,bs="ps")+s(CW,bs="ps")+s(Dq,bs="ps")+s(density,bs="ps")+s(Gini_D,bs="ps")+s(DT,bs="ps")+s(HT,bs="ps")+s(Dg,bs="ps")
                        +s(Gini_H,bs="ps")+s(BAL,bs="ps")+s(Td,bs="ps")+s(aCI,bs="ps")+s(Ud,bs="ds",m=c(1,.1),k=3)+s(Ucw,bs="ds",m=c(1,.1),k=3)+s(M,bs="ds",m=c(1,.1),k=3)+s(Uh,bs="ds",m=c(1,.1),k=3),family=gaussian(),data=essay_train,method="GACV.Cp")
P.np.b2.ps.HCB <- predict(np.b2.ps.HCB,newdata=essay_test,type="response")
FittingEvaluationIndex(fitted(np.b2.ps.HCB),essay_train$HCB,6)
FittingEvaluationIndex(P.np.b2.ps.HCB,essay_test$HCB,1)
summary(np.b2.ps.HCB);AIC(np.b2.ps.HCB)


np.b2.gp.HCB<-mgcv::gam(HCB~s(D,bs="gp")+s(H,bs="gp")+s(CW,bs="gp")+s(Dq,bs="gp")+s(density,bs="gp")+s(Gini_D,bs="gp")+s(DT,bs="gp")+s(HT,bs="gp")+s(Dg,bs="gp")
                        +s(Gini_H,bs="gp")+s(BAL,bs="gp")+s(Td,bs="gp")+s(aCI,bs="gp")+s(Ud,bs="gp",k=3)+s(M,bs="gp",k=3)+s(Ucw,bs="gp",k=3)+s(Uh,bs="gp",k=3),family=gaussian(),data=essay_train,method="GACV.Cp")
P.np.b2.gp.HCB <- predict(np.b2.gp.HCB,newdata=essay_test,type="response")
FittingEvaluationIndex(fitted(np.b2.gp.HCB),essay_train$HCB,1)
FittingEvaluationIndex(P.np.b2.gp.HCB,essay_test$HCB,1)
summary(np.b2.gp.HCB);AIC(np.b2.gp.HCB)


np.b2.bs.HCB <- mgcv::gam(HCB~s(D,bs="bs")+s(H,bs="bs")+s(CW,bs="bs")+s(Dq,bs="bs")+s(density,bs="bs")+s(DT,bs="bs")+s(HT,bs="bs")+s(Dg,bs="bs")
                          +s(Gini_H,bs="bs")+s(BAL,bs="bs")+s(Td,bs="bs")+s(aCI,bs="bs")+s(Ud,bs="ds",m=c(1,.1),k=3)+s(Ucw,bs="ds",m=c(1,.1),k=3)+s(M,bs="ds",m=c(1,.1),k=3)+s(Uh,bs="ds",m=c(1,.1),k=3),family=gaussian(),data=essay_train,method="GACV.Cp")
P.np.b2.bs.HCB <- predict(np.b2.bs.HCB,newdata=essay_test,type="response")
FittingEvaluationIndex(fitted(np.b2.bs.HCB),essay_train$HCB,6)
FittingEvaluationIndex(P.np.b2.bs.HCB,essay_test$HCB,1)
summary(np.b2.bs.HCB);AIC(np.b2.bs.HCB)









##==========================================================================================

##==========================================================================================

##-- DS, PS, GP, BS, CR , TP 6 kinds of spline functions

##--HCB nonparametric model

## spruce-fir tree species group

# b2.cr.HCB <- gam(HCB~s(D,bs="cr")+s(CW,bs="cr")+s(Dq,bs="cr")+s(density,bs="cr")+s(BAS,bs="cr") +s(Gini_D,bs="cr")+s(DT,bs="cr")+s(HT,bs="cr")+s(Dg,bs="cr")
#                  +s(Gini_H,bs="cr")+s(Gini_BA,bs="cr")+s(BAL,bs="cr")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="cr"),family=gaussian(),data=essay_train_YLS,method="GACV.Cp")
# P.b2.cr.HCB <- predict(b2.cr.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.cr.HCB,essay_test$HCB,1)

YLS.tp.HCB<-gam(HCB~s(D,bs="tp")+s(CW,bs="tp")+s(Dq,bs="tp")+s(BAS,bs="tp") +s(Gini_D,bs="tp")+s(HTyls,bs="tp")+s(Dg,bs="tp")
               +s(Gini_H,bs="tp")+s(Gini_BA,bs="tp")+Td+Uh,family=gaussian(),data=essay_train_YLS,method="GACV.Cp")
FittingEvaluationIndex(fitted(YLS.tp.HCB),essay_train_YLS$HCB,2)
P.YLS.tp.HCB <- predict(YLS.tp.HCB,newdata=essay_test_YLS,type="response")
summary(YLS.tp.HCB)
FittingEvaluationIndex(P.YLS.tp.HCB,essay_test_YLS$HCB,2)


par(mfrow=c(3,3))
par(mar=c(4,4,2,2))
plot(YLS.tp.HCB, select = 1, pch = 20, shade = TRUE, residuals = T,xlab = "D",ylab = "residuals")
plot(YLS.tp.HCB, select = 2, pch = 20, shade = TRUE, residuals = T,xlab = "CW",ylab = "residuals")
plot(YLS.tp.HCB, select = 3, pch = 20, shade = TRUE, residuals = T,xlab = "Dq",ylab = "residuals")
plot(YLS.tp.HCB, select = 4, pch = 20, shade = TRUE, residuals = T,xlab = "BAS",ylab = "residuals")
plot(YLS.tp.HCB, select = 5, pch = 20, shade = TRUE, residuals = T,xlab = "Gini_D",ylab = "residuals")
plot(YLS.tp.HCB, select = 6, pch = 20, shade = TRUE, residuals = T,xlab = "HTyls",ylab = "residuals")
plot(YLS.tp.HCB, select = 7, pch = 20, shade = TRUE, residuals = T,xlab = "Dg",ylab = "residuals")
plot(YLS.tp.HCB, select = 8, pch = 20, shade = TRUE, residuals = T,xlab = "Gini_H",ylab = "residuals")
plot(YLS.tp.HCB, select = 9, pch = 20, shade = TRUE, residuals = T,xlab = "Gini_BA",ylab = "residuals")

plot(YLS.tp.HCB,pages=1)
gam.check(YLS.tp.HCB,pch=19,cex=.3)

essay_train_YLS1 <- cbind(essay_train_YLS, YLS.tp.HCB$residuals)
essay_train_YLS1 <- cbind(essay_train_YLS1, YLS.tp.HCB$fitted.values)
essay_train_YLS1 <- as.data.frame(essay_train_YLS1)
essay_train_YLS1 <- rename(essay_train_YLS1, c("residuals"="YLS.tp.HCB$residuals","fittedvalue"="YLS.tp.HCB$fitted.values"))
library(ggplot2)
library(ggsci)

# 计算偏残差
residuals <- residuals(YLS.tp.HCB, type = "working")
# plot(essay_train_YLS1$D,YLS.tp.HCB$linear.predictors)
# 创建偏残差图
df <- data.frame(residuals = residuals, x = essay_train_YLS1$Gini_BA)  # 使用x1作为横坐标
ggplot(df, aes(x, residuals)) +
  geom_point() +
  ylim(-20,20)+
  geom_smooth(method = "loess") +
  labs(x = "D", y = "偏残差") +
  theme_minimal()


ggplot(essay_train_YLS1, aes(x = HCB, y =fittedvalue)) +
  geom_point() +
  # facet_wrap(~species, ncol = 3) +
  geom_smooth(se = T, method = 'gam',bs="tp") +
  labs(y = 'h/H') + theme_bw()  
  # stat_cor(method = 'pearson') +
  # scale_color_npg()


ggplot(essay_train_YLS1, aes(x = HCB, y =fittedvalue)) +
  geom_point() +
  geom_smooth(formula = HCB~s(D,bs="tp")+s(CW,bs="tp")+s(Dq,bs="tp")+s(BAS,bs="tp") +s(Gini_D,bs="tp")+s(HT,bs="tp")+s(Dg,bs="tp")
              +s(Gini_H,bs="tp")+s(Gini_BA,bs="tp")+Td+Uh,
              method = "gam", size = 0.7,
              method.args = list(family = gaussian(link = "identity")),
              aes(color = "gam", fill = "confidence interval"))



ggplot(essay_train_YLS1,aes(D,residuals))+
  geom_point()+
  # geom_smooth(method='lm',se=T,show.legend=TRUE,
  #             linetype=1,color='black',size = 1)+
  # geom_smooth(formula=y~poly(x,5),se=T,show.legend=TRUE,
  #             linetype=2,color='blue',size=1.2)+
  geom_smooth(method='gam',se=T,show.legend=TRUE,
              linetype=3,color='gold',size=1.5)
# +
  # stat_function(fun = function(x) -0.08*x +5, linetype=4,
  #               show.legend=TRUE,size=2,color='red')


# par(mfrow=c(1,1))
# plot(essay_train_YLS$Dq,YLS.tp.HCB$residuals)
# vis.gam(YLS.tp.HCB, main = "", plot.type = "contour",
#         
#         color = "terrain", contour.col = "black", lwd = 2)

# b2.ds.HCB<-gam(HCB~s(D,bs="ds")+s(CW,bs="ds")+s(Dq,bs="ds")+s(density,bs="ds")+s(BAS,bs="ds") +s(Gini_D,bs="ds")+s(DT,bs="ds")+s(HT,bs="ds")+s(Dg,bs="ds")
#                +s(Gini_H,bs="ds")+s(Gini_BA,bs="ds")+s(BAL,bs="ds")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="ds"),family=gaussian(),data=essay_train_YLS,method="GACV.Cp")
# P.b2.ds.HCB <- predict(b2.ds.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.ds.HCB,essay_test$HCB,1)
# 
# b2.ps.HCB<-gam(HCB~s(D,bs="ps")+s(CW,bs="ps")+s(Dq,bs="ps")+s(density,bs="ps")+s(BAS,bs="ps") +s(Gini_D,bs="ps")+s(DT,bs="ps")+s(HT,bs="ps")+s(Dg,bs="ps")
#                +s(Gini_H,bs="ps")+s(Gini_BA,bs="ps")+s(BAL,bs="ps")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="ps"),family=gaussian(),data=essay_train_YLS,method="GACV.Cp")
# P.b2.ps.HCB <- predict(b2.ps.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.ps.HCB,essay_test$HCB,1)
# 
# b2.gp.HCB<-gam(HCB~s(D,bs="gp")+s(CW,bs="gp")+s(Dq,bs="gp")+s(density,bs="gp")+s(BAS,bs="gp") +s(Gini_D,bs="gp")+s(DT,bs="gp")+s(HT,bs="gp")+s(Dg,bs="gp")
#                +s(Gini_H,bs="gp")+s(Gini_BA,bs="gp")+s(BAL,bs="gp")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="gp"),family=gaussian(),data=essay_train_YLS,method="GACV.Cp")
# P.b2.gp.HCB <- predict(b2.gp.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.gp.HCB,essay_test$HCB,1)
# 
# b2.bs.HCB<-gam(HCB~s(D,bs="bs")+s(CW,bs="bs")+s(Dq,bs="bs")+s(density,bs="bs")+s(BAS,bs="bs") +s(Gini_D,bs="bs")+s(DT,bs="bs")+s(HT,bs="bs")+s(Dg,bs="bs")
#                +s(Gini_H,bs="bs")+s(Gini_BA,bs="bs")+s(BAL,bs="bs")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="bs"),family=gaussian(),data=essay_train_YLS,method="GACV.Cp")
# P.b2.bs.HCB <- predict(b2.bs.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.bs.HCB,essay_test$HCB,1)





## HardB tree species group

# b2.cr.HCB <- gam(HCB~s(D,bs="cr")+s(CW,bs="cr")+s(Dq,bs="cr")+s(density,bs="cr")+s(BAS,bs="cr") +s(Gini_D,bs="cr")+s(DT,bs="cr")+s(HT,bs="cr")+s(Dg,bs="cr")
#                  +s(Gini_H,bs="cr")+s(Gini_BA,bs="cr")+s(BAL,bs="cr")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="cr"),family=gaussian(),data=essay_train_HardB,method="GACV.Cp")
# P.b2.cr.HCB <- predict(b2.cr.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.cr.HCB,essay_test$HCB,1)

HardB.tp.HCB<-gam(HCB~s(Dq,bs="tp")+s(HThardB,bs="tp")+s(Dg,bs="tp")+s(Gini_H,bs="tp")+Ud+Td+Ucw+Uh,family=gaussian(),data=essay_train_HardB,method="GACV.Cp")
FittingEvaluationIndex(fitted(HardB.tp.HCB),essay_train_HardB$HCB,4)
P.HardB.tp.HCB <- predict(HardB.tp.HCB,newdata=essay_test_HardB,type="response")
FittingEvaluationIndex(P.HardB.tp.HCB,essay_test_HardB$HCB,4)
summary(HardB.tp.HCB)

par(mfrow=c(3,3))
par(mar=c(4,4,2,2))
plot(HardB.tp.HCB, select = 1, pch = 20, shade = TRUE, residuals = T,xlab = "Dq",ylab = "residuals")
plot(HardB.tp.HCB, select = 2, pch = 20, shade = TRUE, residuals = T,xlab = "HThardB",ylab = "residuals")
plot(HardB.tp.HCB, select = 3, pch = 20, shade = TRUE, residuals = T,xlab = "Dg",ylab = "residuals")
plot(HardB.tp.HCB, select = 4, pch = 20, shade = TRUE, residuals = T,xlab = "Gini_H",ylab = "residuals")

check(HardB.tp.HCB)

# b2.ds.HCB<-gam(HCB~s(D,bs="ds")+s(CW,bs="ds")+s(Dq,bs="ds")+s(density,bs="ds")+s(BAS,bs="ds") +s(Gini_D,bs="ds")+s(DT,bs="ds")+s(HT,bs="ds")+s(Dg,bs="ds")
#                +s(Gini_H,bs="ds")+s(Gini_BA,bs="ds")+s(BAL,bs="ds")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="ds"),family=gaussian(),data=essay_train_HardB,method="GACV.Cp")
# P.b2.ds.HCB <- predict(b2.ds.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.ds.HCB,essay_test$HCB,1)
# 
# b2.ps.HCB<-gam(HCB~s(D,bs="ps")+s(CW,bs="ps")+s(Dq,bs="ps")+s(density,bs="ps")+s(BAS,bs="ps") +s(Gini_D,bs="ps")+s(DT,bs="ps")+s(HT,bs="ps")+s(Dg,bs="ps")
#                +s(Gini_H,bs="ps")+s(Gini_BA,bs="ps")+s(BAL,bs="ps")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="ps"),family=gaussian(),data=essay_train_HardB,method="GACV.Cp")
# P.b2.ps.HCB <- predict(b2.ps.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.ps.HCB,essay_test$HCB,1)
# 
# b2.gp.HCB<-gam(HCB~s(D,bs="gp")+s(CW,bs="gp")+s(Dq,bs="gp")+s(density,bs="gp")+s(BAS,bs="gp") +s(Gini_D,bs="gp")+s(DT,bs="gp")+s(HT,bs="gp")+s(Dg,bs="gp")
#                +s(Gini_H,bs="gp")+s(Gini_BA,bs="gp")+s(BAL,bs="gp")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="gp"),family=gaussian(),data=essay_train_HardB,method="GACV.Cp")
# P.b2.gp.HCB <- predict(b2.gp.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.gp.HCB,essay_test$HCB,1)
# 
# b2.bs.HCB<-gam(HCB~s(D,bs="bs")+s(CW,bs="bs")+s(Dq,bs="bs")+s(density,bs="bs")+s(BAS,bs="bs") +s(Gini_D,bs="bs")+s(DT,bs="bs")+s(HT,bs="bs")+s(Dg,bs="bs")
#                +s(Gini_H,bs="bs")+s(Gini_BA,bs="bs")+s(BAL,bs="bs")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="bs"),family=gaussian(),data=essay_train_HardB,method="GACV.Cp")
# P.b2.bs.HCB <- predict(b2.bs.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.bs.HCB,essay_test$HCB,1)





## SoftB tree species group

# b2.cr.HCB <- gam(HCB~s(D,bs="cr")+s(CW,bs="cr")+s(Dq,bs="cr")+s(density,bs="cr")+s(BAS,bs="cr") +s(Gini_D,bs="cr")+s(DT,bs="cr")+s(HT,bs="cr")+s(Dg,bs="cr")
#                  +s(Gini_H,bs="cr")+s(Gini_BA,bs="cr")+s(BAL,bs="cr")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="cr"),family=gaussian(),data=essay_train_SoftB,method="GACV.Cp")
# P.b2.cr.HCB <- predict(b2.cr.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.cr.HCB,essay_test$HCB,1)

SoftB.tp.HCB<-gam(HCB~s(CW,bs="tp")+s(Dq,bs="tp")+s(density,bs="tp")+s(HTsoftB,bs="tp")+s(Dg,bs="tp")
               +s(Gini_H,bs="tp")+Td+M+Ucw+Uh,family=gaussian(),data=essay_train_SoftB,method="GACV.Cp")
FittingEvaluationIndex(fitted(SoftB.tp.HCB),essay_train_SoftB$HCB,4)
P.SoftB.tp.HCB <- predict(SoftB.tp.HCB,newdata=essay_test_SoftB,type="response")
FittingEvaluationIndex(P.SoftB.tp.HCB,essay_test_SoftB$HCB,4)
summary(SoftB.tp.HCB)

par(mfrow=c(3,3))
par(mar=c(4,4,2,2))
plot(SoftB.tp.HCB, select = 1, pch = 20, shade = TRUE, residuals = T,xlab = "CW",ylab = "residuals")
plot(SoftB.tp.HCB, select = 2, pch = 20, shade = TRUE, residuals = T,xlab = "Dq",ylab = "residuals")
plot(SoftB.tp.HCB, select = 3, pch = 20, shade = TRUE, residuals = T,xlab = "density",ylab = "residuals")
plot(SoftB.tp.HCB, select = 4, pch = 20, shade = TRUE, residuals = T,xlab = "HTsoftB",ylab = "residuals")
plot(SoftB.tp.HCB, select = 5, pch = 20, shade = TRUE, residuals = T,xlab = "Dg",ylab = "residuals")
plot(SoftB.tp.HCB, select = 6, pch = 20, shade = TRUE, residuals = T,xlab = "Gini_H",ylab = "residuals")




# b2.ds.HCB<-gam(HCB~s(D,bs="ds")+s(CW,bs="ds")+s(Dq,bs="ds")+s(density,bs="ds")+s(BAS,bs="ds") +s(Gini_D,bs="ds")+s(DT,bs="ds")+s(HT,bs="ds")+s(Dg,bs="ds")
#                +s(Gini_H,bs="ds")+s(Gini_BA,bs="ds")+s(BAL,bs="ds")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="ds"),family=gaussian(),data=essay_train_SoftB,method="GACV.Cp")
# P.b2.ds.HCB <- predict(b2.ds.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.ds.HCB,essay_test$HCB,1)
# 
# b2.ps.HCB<-gam(HCB~s(D,bs="ps")+s(CW,bs="ps")+s(Dq,bs="ps")+s(density,bs="ps")+s(BAS,bs="ps") +s(Gini_D,bs="ps")+s(DT,bs="ps")+s(HT,bs="ps")+s(Dg,bs="ps")
#                +s(Gini_H,bs="ps")+s(Gini_BA,bs="ps")+s(BAL,bs="ps")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="ps"),family=gaussian(),data=essay_train_SoftB,method="GACV.Cp")
# P.b2.ps.HCB <- predict(b2.ps.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.ps.HCB,essay_test$HCB,1)
# 
# b2.gp.HCB<-gam(HCB~s(D,bs="gp")+s(CW,bs="gp")+s(Dq,bs="gp")+s(density,bs="gp")+s(BAS,bs="gp") +s(Gini_D,bs="gp")+s(DT,bs="gp")+s(HT,bs="gp")+s(Dg,bs="gp")
#                +s(Gini_H,bs="gp")+s(Gini_BA,bs="gp")+s(BAL,bs="gp")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="gp"),family=gaussian(),data=essay_train_SoftB,method="GACV.Cp")
# P.b2.gp.HCB <- predict(b2.gp.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.gp.HCB,essay_test$HCB,1)
# 
# b2.bs.HCB<-gam(HCB~s(D,bs="bs")+s(CW,bs="bs")+s(Dq,bs="bs")+s(density,bs="bs")+s(BAS,bs="bs") +s(Gini_D,bs="bs")+s(DT,bs="bs")+s(HT,bs="bs")+s(Dg,bs="bs")
#                +s(Gini_H,bs="bs")+s(Gini_BA,bs="bs")+s(BAL,bs="bs")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="bs"),family=gaussian(),data=essay_train_SoftB,method="GACV.Cp")
# P.b2.bs.HCB <- predict(b2.bs.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.bs.HCB,essay_test$HCB,1)




## Coniferous tree species group

# b2.cr.HCB <- gam(HCB~s(D,bs="cr")+s(CW,bs="cr")+s(Dq,bs="cr")+s(density,bs="cr")+s(BAS,bs="cr") +s(Gini_D,bs="cr")+s(DT,bs="cr")+s(HT,bs="cr")+s(Dg,bs="cr")
#                  +s(Gini_H,bs="cr")+s(Gini_BA,bs="cr")+s(BAL,bs="cr")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="cr"),family=gaussian(),data=essay_train_Coni,method="GACV.Cp")
# P.b2.cr.HCB <- predict(b2.cr.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.cr.HCB,essay_test$HCB,1)

Coni.tp.HCB<-gam(HCB~s(D,bs="tp")+s(CW,bs="tp")+s(BAS,bs="tp") +s(Gini_D,bs="tp")+s(HTconi,bs="tp")+s(Dg,bs="tp")
               +s(Gini_BA,bs="tp")+Td+M+Uh+aCI+s(uaCI,bs="tp"),family=gaussian(),data=essay_train_Coni,method="GACV.Cp")
FittingEvaluationIndex(fitted(Coni.tp.HCB),essay_train_Coni$HCB,4)
P.Coni.tp.HCB <- predict(Coni.tp.HCB,newdata=essay_test_Coni,type="response")
FittingEvaluationIndex(P.Coni.tp.HCB,essay_test_Coni$HCB,4)
summary(Coni.tp.HCB)

par(mfrow=c(3,3))
par(mar=c(4,4,2,2))
plot(Coni.tp.HCB, select = 1, pch = 20, shade = TRUE, residuals = T,xlab = "D",ylab = "residuals")
plot(Coni.tp.HCB, select = 2, pch = 20, shade = TRUE, residuals = T,xlab = "CW",ylab = "residuals")
plot(Coni.tp.HCB, select = 3, pch = 20, shade = TRUE, residuals = T,xlab = "BAS",ylab = "residuals")
plot(Coni.tp.HCB, select = 4, pch = 20, shade = TRUE, residuals = T,xlab = "Gini_D",ylab = "residuals")
plot(Coni.tp.HCB, select = 5, pch = 20, shade = TRUE, residuals = T,xlab = "HTconi",ylab = "residuals")
plot(Coni.tp.HCB, select = 6, pch = 20, shade = TRUE, residuals = T,xlab = "Dg",ylab = "residuals")
plot(Coni.tp.HCB, select = 7, pch = 20, shade = TRUE, residuals = T,xlab = "Gini_BA",ylab = "residuals")
plot(Coni.tp.HCB, select = 8, pch = 20, shade = TRUE, residuals = T,xlab = "uaCI",ylab = "residuals")



# b2.ds.HCB<-gam(HCB~s(D,bs="ds")+s(CW,bs="ds")+s(Dq,bs="ds")+s(density,bs="ds")+s(BAS,bs="ds") +s(Gini_D,bs="ds")+s(DT,bs="ds")+s(HT,bs="ds")+s(Dg,bs="ds")
#                +s(Gini_H,bs="ds")+s(Gini_BA,bs="ds")+s(BAL,bs="ds")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="ds"),family=gaussian(),data=essay_train_Coni,method="GACV.Cp")
# P.b2.ds.HCB <- predict(b2.ds.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.ds.HCB,essay_test$HCB,1)
# 
# b2.ps.HCB<-gam(HCB~s(D,bs="ps")+s(CW,bs="ps")+s(Dq,bs="ps")+s(density,bs="ps")+s(BAS,bs="ps") +s(Gini_D,bs="ps")+s(DT,bs="ps")+s(HT,bs="ps")+s(Dg,bs="ps")
#                +s(Gini_H,bs="ps")+s(Gini_BA,bs="ps")+s(BAL,bs="ps")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="ps"),family=gaussian(),data=essay_train_Coni,method="GACV.Cp")
# P.b2.ps.HCB <- predict(b2.ps.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.ps.HCB,essay_test$HCB,1)
# 
# b2.gp.HCB<-gam(HCB~s(D,bs="gp")+s(CW,bs="gp")+s(Dq,bs="gp")+s(density,bs="gp")+s(BAS,bs="gp") +s(Gini_D,bs="gp")+s(DT,bs="gp")+s(HT,bs="gp")+s(Dg,bs="gp")
#                +s(Gini_H,bs="gp")+s(Gini_BA,bs="gp")+s(BAL,bs="gp")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="gp"),family=gaussian(),data=essay_train_Coni,method="GACV.Cp")
# P.b2.gp.HCB <- predict(b2.gp.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.gp.HCB,essay_test$HCB,1)
# 
# b2.bs.HCB<-gam(HCB~s(D,bs="bs")+s(CW,bs="bs")+s(Dq,bs="bs")+s(density,bs="bs")+s(BAS,bs="bs") +s(Gini_D,bs="bs")+s(DT,bs="bs")+s(HT,bs="bs")+s(Dg,bs="bs")
#                +s(Gini_H,bs="bs")+s(Gini_BA,bs="bs")+s(BAL,bs="bs")+Ud+Td+M+Ucw+Uh+aCI+s(uaCI,bs="bs"),family=gaussian(),data=essay_train_Coni,method="GACV.Cp")
# P.b2.bs.HCB <- predict(b2.bs.HCB,newdata=essay_test,type="response")
# FittingEvaluationIndex(P.b2.bs.HCB,essay_test$HCB,1)