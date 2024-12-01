library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
setwd("D:/BaiduSyncdisk/论文/2023文章/data")
tree.comnew <- read.xlsx("tree.comnew.xlsx")


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
  MAPE <- sum(abs(e/ObsH))/n*100
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




data_essay0 <- tree.comnew
# data_essay <- PhDdata1
set.seed(12345)
essay_train<-data.frame()
essay_test<-data.frame()
n <- unique(data_essay0$plotno)
for(i in 1:length(n)){
  subdata<-split(data_essay0,data_essay0$plotno[i])
  sub<-subdata[[i]]
  x<-sub[sample(nrow(sub),size = 0.7*nrow(sub),replace=F),]
  y<-sub[-sample(nrow(sub),size = 0.7*nrow(sub),replace=F),]
  essay_train <- rbind(essay_train,x)
  essay_test <- rbind(essay_test,y)
  return(essay_train)
  return(essay_test)
}
nrow(essay_test)
nrow(essay_train)
# essay_train$species.group
essay_train_HardB <- subset(essay_train,species.group=="硬阔")
essay_train_SoftB <- subset(essay_train,species.group=="软阔")
essay_train_YLS <- subset(essay_train,species.group=="云冷杉")
essay_train_Coni <- subset(essay_train,species.group=="其他针叶")

essay_test_HardB <- subset(essay_test,species.group=="硬阔")
essay_test_SoftB <- subset(essay_test,species.group=="软阔")
essay_test_YLS <- subset(essay_test,species.group=="云冷杉")
essay_test_Coni <- subset(essay_test,species.group=="其他针叶")

library(caret) 
# install.packages("gbm")
library(gbm) 

##===========================================================================================================================================
##-- H BRT (boosted regression tree) model

ctrl <- trainControl(method = "cv", number = 10)
set.seed(123)

fit.gbm.H.HardB <- train(H ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                         data=essay_train_HardB, method = "gbm", trControl = ctrl)
plot(fit.gbm.H.HardB,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m",main="")
gbmHjg.HardB <- round(fit.gbm.H.HardB $results,digits = 4)

fit.gbm.H.SoftB <- train(H ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                         data=essay_train_SoftB, method = "gbm", trControl = ctrl)
plot(fit.gbm.H.SoftB ,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m",main="")
gbmHjg.SoftB <- round(fit.gbm.H.SoftB$results,digits = 4)

fit.gbm.H.YLS <- train(H ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                       data=essay_train_YLS, method = "gbm", trControl = ctrl)
plot(fit.gbm.H.YLS,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m",main="")
gbmHjg.YLS <- round(fit.gbm.H.YLS$results,digits = 4)

fit.gbm.H.Coni <- train(H ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                        data=essay_train_Coni, method = "gbm", trControl = ctrl)
plot(fit.gbm.H.Coni ,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m",main="")
gbmHjg.Coni  <- round(fit.gbm.H.Coni $results,digits = 4)



a <- fit.gbm.H.HardB$results
a$interaction.depth <- as.factor(a$interaction.depth)
BRT1 <- ggplot(data=a,aes(x=n.trees, y=RMSE, group=interaction.depth,col=interaction.depth))+theme_bw()+ggtitle("硬阔树种组")+theme(plot.title = element_text(size = 12.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

b <- fit.gbm.H.SoftB$results
b$interaction.depth <- as.factor(b$interaction.depth)
BRT2 <- ggplot(data=b,aes(x=n.trees, y=RMSE, group=interaction.depth,col=interaction.depth))+theme_bw()+ggtitle("软阔树种组")+theme(plot.title = element_text(size = 12.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

c <- fit.gbm.H.Coni$results
c$interaction.depth <- as.factor(c$interaction.depth)
BRT3 <- ggplot(data=c,aes(x=n.trees, y=RMSE, group=interaction.depth,col=interaction.depth))+theme_bw()+ggtitle("其他针叶树种组")+theme(plot.title = element_text(size = 12.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

d <- fit.gbm.H.YLS$results
d$interaction.depth <- as.factor(d$interaction.depth)
BRT4 <- ggplot(data=d,aes(x=n.trees, y=RMSE, group=interaction.depth,col=interaction.depth))+theme_bw()+ggtitle("云冷杉树种组")+theme(plot.title = element_text(size = 12.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

# +theme_classic()
# +theme(plot.title = element_text(size = 12.5,hjust = 0.45, vjust=1.0),legend.position=c(0.65,0.75))

BRT.H <- ggarrange(BRT1,BRT2,BRT3,BRT4,ncol = 2,nrow = 2, common.legend = T)



write.xlsx(gbmHjg,"D:/zzy essay/博士大论文/model results/H model/gbmH.xlsx")

##-- GBM H model with function of gbm
shrinkage=seq(0.001,0.1,0.001)
gbm_H.HardB <- gbm(H ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                   data=essay_train_HardB,                   
                   distribution="gaussian",    
                   n.trees=150,                
                   shrinkage=0.1,             
                   # 0.001 to 0.1 usually work
                   interaction.depth=3,         
                   # bag.fraction = 0.5,          
                   # train.fraction = 0.5,        
                   # n.minobsinnode = 10,       
                   cv.folds = 10,              
                   keep.data=TRUE,            
                   verbose=T,              
                   n.cores=1) 

gbm_H.SoftB <- gbm(H ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                   data=essay_train_SoftB,                   
                   distribution="gaussian",    
                   n.trees=150,                
                   shrinkage=0.1,             
                   # 0.001 to 0.1 usually work
                   interaction.depth=3,         
                   # bag.fraction = 0.5,          
                   # train.fraction = 0.5,        
                   # n.minobsinnode = 10,       
                   cv.folds = 10,              
                   keep.data=TRUE,            
                   verbose=T,              
                   n.cores=1) 

gbm_H.YLS <- gbm(H ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                 data=essay_train_YLS,                   
                 distribution="gaussian",    
                 n.trees=150,                
                 shrinkage=0.1,             
                 # 0.001 to 0.1 usually work
                 interaction.depth=3,         
                 # bag.fraction = 0.5,          
                 # train.fraction = 0.5,        
                 # n.minobsinnode = 10,       
                 cv.folds = 10,              
                 keep.data=TRUE,            
                 verbose=T,              
                 n.cores=1) 

gbm_H.Coni <- gbm(H ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                  data=essay_train_Coni,                   
                  distribution="gaussian",    
                  n.trees=150,                
                  shrinkage=0.1,             
                  # 0.001 to 0.1 usually work
                  interaction.depth=3,         
                  # bag.fraction = 0.5,          
                  # train.fraction = 0.5,        
                  # n.minobsinnode = 10,       
                  cv.folds = 10,              
                  keep.data=TRUE,            
                  verbose=T,              
                  n.cores=1) 

##-- importance plot
importance_otu.scale <- data.frame(relative.influence(gbm_H,n.trees = 150,scale = TRUE), check.names = T)
importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)
dataorder <- importance_otu.scale %>% arrange(importance_otu.scale$relative.influence.gbm_H..n.trees...150..scale...TRUE) #按照count一列降序排序
dataorder$OTU_name<-factor(dataorder$OTU_name,levels = unique(dataorder$OTU_name),ordered = T) #按照trait一列作为X轴排序
Himp <- ggplot(dataorder,aes(x=OTU_name, y=(relative.influence.gbm_H..n.trees...150..scale...TRUE.)*100))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("变量 variable")+ylab("相对重要性 Variable relative importance(%)")+theme_bw()+ggtitle("BRT H model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))
ggsave("D:/zzy essay/博士大论文/model results/H model/GBM importance plot.tiff",Himp)


##-- the partial dependence plots of H model of GBM
library(ggplot2)
library(lattice)

top21gbm <- topPredictors(fit.gbm.H, n = 20)
# Construct partial dependence functions for top four predictors
pdgbmh <- NULL
for (i in top21gbm) {
  tmp <- partial(gbm_H, pred.var = i,n.trees=150)
  names(tmp) <- c("x", "y")
  pdgbmh <- rbind(pdgbmh, cbind(tmp, predictor = i))
}

# Display partial dependence functions
Hgbmpartial <- ggplot(pdgbmh, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=6,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=6,face = "bold"))+
  xlab("Variables")+ylab("H/m")+ggtitle("Partial dependence plots of H BRT model")+theme(plot.title = element_text(size = 14,hjust = 0.45, vjust=1.0))
ggsave("D:/zzy essay/博士大论文/model results/H model/PDP gbm H.tiff",Hgbmpartial)

##===========================================================================================================================================



##===========================================================================================================================================
##-- HCB gbm model
fit.gbm.HCB.SoftB <- train(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTsoftB+HTsoftB+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                           data=essay_train_SoftB, method = "gbm", trControl = ctrl)
plot(fit.gbm.HCB.SoftB,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m",main="")
gbmHCBjg.SoftB <- round(fit.gbm.HCB.SoftB$results,digits = 4)
write.xlsx(gbmHCBjg.SoftB,"D:/files and materials/zzy essay/2023essay/HCB nonparametric model/SoftB BRT fitting results.xlsx")
gbmHCBjg.SoftB$interaction.depth <- as.factor(gbmHCBjg.SoftB$interaction.depth)
BRT2 <- ggplot(data=gbmHCBjg.SoftB,aes(x=n.trees, y=Rsquared, group=interaction.depth,col=interaction.depth))+theme_bw()+ggtitle("soft broadleaved group")+theme(plot.title = element_text(size = 10.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

FittingEvaluationIndex(fitted(fit.gbm.HCB.SoftB),essay_train_SoftB$HCB,0)
p.fit.gbm.HCB.SoftB <- predict(fit.gbm.HCB.SoftB,newdata=essay_test_SoftB,type="raw")
FittingEvaluationIndex(p.fit.gbm.HCB.SoftB,essay_test_SoftB$HCB,0)

gbm_HCB.SoftB <- gbm(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTsoftB+HTsoftB+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                     data=essay_train_SoftB,                   
                     distribution="gaussian",    
                     n.trees=100,                
                     shrinkage=0.1,             
                     # 0.001 to 0.1 usually work
                     interaction.depth=3,         
                     # bag.fraction = 0.5,          
                     # train.fraction = 0.5,        
                     # n.minobsinnode = 10,       
                     cv.folds = 10,              
                     keep.data=TRUE,            
                     verbose=T,              
                     n.cores=1) 



fit.gbm.HCB.HardB <- train(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                          data=essay_train_HardB, method = "gbm", trControl = ctrl)

gbm_HCB.HardB <- gbm(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                    data=essay_train_HardB,                   
                    distribution="gaussian",    
                    n.trees=50,                
                    shrinkage=0.1,             
                    # 0.001 to 0.1 usually work
                    interaction.depth=3,         
                    # bag.fraction = 0.5,          
                    # train.fraction = 0.5,        
                    # n.minobsinnode = 10,       
                    cv.folds = 10,              
                    keep.data=TRUE,            
                    verbose=T,              
                    n.cores=1) 






fit.gbm.HCB.YLS <- train(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                          data=essay_train_YLS, method = "gbm", trControl = ctrl)

gbm_HCB.YLS <- gbm(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                    data=essay_train_YLS,                   
                    distribution="gaussian",    
                    n.trees=50,                
                    shrinkage=0.1,             
                    # 0.001 to 0.1 usually work
                    interaction.depth=3,         
                    # bag.fraction = 0.5,          
                    # train.fraction = 0.5,        
                    # n.minobsinnode = 10,       
                    cv.folds = 10,              
                    keep.data=TRUE,            
                    verbose=T,              
                    n.cores=1) 






fit.gbm.HCB.Coni <- train(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                          data=essay_train_Coni, method = "gbm", trControl = ctrl)
plot(fit.gbm.HCB.Coni,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m",main="")
gbmHCBjg.Coni <- round(fit.gbm.HCB.Coni$results,digits = 4)
write.xlsx(gbmHCBjg.Coni,"D:/files and materials/zzy essay/2023essay/HCB nonparametric model/Coni BRT fitting results.xlsx")
gbmHCBjg.Coni$interaction.depth <- as.factor(gbmHCBjg.Coni$interaction.depth)
BRT4 <- ggplot(data=gbmHCBjg.Coni,aes(x=n.trees, y=Rsquared, group=interaction.depth,col=interaction.depth))+theme_bw()+ggtitle("other coniferous group")+theme(plot.title = element_text(size = 10.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

FittingEvaluationIndex(fitted(fit.gbm.HCB.Coni),essay_train_Coni$HCB,0)
p.fit.gbm.HCB.Coni <- predict(fit.gbm.HCB.Coni,newdata=essay_test_Coni,type="raw")
FittingEvaluationIndex(p.fit.gbm.HCB.Coni,essay_test_Coni$HCB,0)

gbm_HCB.Coni <- gbm(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                     data=essay_train_Coni,                   
                     distribution="gaussian",    
                     n.trees=50,                
                     shrinkage=0.1,             
                     # 0.001 to 0.1 usually work
                     interaction.depth=3,         
                     # bag.fraction = 0.5,          
                     # train.fraction = 0.5,        
                     # n.minobsinnode = 10,       
                     cv.folds = 10,              
                     keep.data=TRUE,            
                     verbose=T,              
                     n.cores=1) 


P.YLS.BRT.HCB <- predict(fit.gbm.HCB.YLS,newdata=essay_test_YLS,type="raw")
FittingEvaluationIndex(P.YLS.BRT.HCB,essay_test_YLS$HCB,0)



# BRTall <- ggarrange(BRT1,BRT2,BRT3,BRT4,ncol = 2,nrow = 2, common.legend = T)
# 
# ggsave("D:/files and materials/zzy essay/2023essay/HCB nonparametric model/HCBBRTR2.tiff",BRTall,dpi = 400)
# 





##-- importance plot
library(ggplot2)
library(gbm)
library(pdp)
library(vip)

## SoftB
BRT.SoftB.HCB.imp <- vi(fit.gbm.HCB.SoftB,type="rss")
dataorder <- BRT.SoftB.HCB.imp %>% arrange(BRT.SoftB.HCB.imp$Importance) #按照Importance一列升序排序
dataorder$Variable<-factor(dataorder$Variable,levels = unique(dataorder$Variable),ordered = T) #按照trait一列作为X轴排序
HCB.SoftB.BRT.imp <- ggplot(dataorder,aes(x=Variable, y=Importance))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("变量 variable")+ylab("相对重要性 Variable relative importance(%)")+theme_bw()+ggtitle("BRT HCB model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))

type <- c("spatial","spatial","spatial","spatial","stand","spatial","tree species","tree species","density","tree species","tree species","stand","density","tree","tree species","tree","spatial","tree","spatial","tree species","stand","stand","tree","tree", "tree")
dataorder <- cbind(dataorder,type)

library(ggrepel)

dataorder$Importance <- round(dataorder$Importance/sum(dataorder$Importance)*100,2)
impa <- ggplot(dataorder,aes(Importance,as.factor(Variable),color=type))+facet_wrap(~type)+geom_point()+xlab("relative importance(%)")+ylab("")+theme_bw()+ggtitle("Soft-broadleaved group")+theme(plot.title = element_text(size = 12,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  facet_grid(type~.,scales='free',space='free')+
  # xlim(0,100)+
  geom_pointrange(aes(xmin = 0, xmax = Importance))+
  geom_text_repel(aes(label=Importance,color=type),hjust=0.5,segment.color="black",segment.size=0.5,size=4,nudge_x = 0.25)+##arrow = arrow(ends = "first",length = unit(0.01,"npc"))+nudge_y = 0+size=2,alpha=0.5+box.padding = 0.8,point.padding = 0.8+force = 1,max.iter = 3e3
  # geom_text(aes(label=Importance),check_overlap = F)+
  theme(strip.text.y=element_text(angle=90))#设置y轴标签

ggsave("D:/zzy essay/2024论文/HCB nonparametric/HCB nonparametric/HCB nonparametric/HCB nonparametric model/redrawn plots/softB.imp.BRT.pdf",impa,dpi = 400)
ggsave("D:/zzy essay/2024论文/HCB nonparametric/HCB nonparametric/HCB nonparametric/HCB nonparametric model/redrawn plots/softB.imp.BRT.tiff",impa,dpi = 400)



## Coni
BRT.Coni.HCB.imp <- vi(fit.gbm.HCB.Coni,type="rss")
dataorder <- BRT.Coni.HCB.imp %>% arrange(BRT.Coni.HCB.imp$Importance) #按照Importance一列升序排序
dataorder$Variable<-factor(dataorder$Variable,levels = unique(dataorder$Variable),ordered = T) #按照trait一列作为X轴排序
HCB.Coni.BRT.imp <- ggplot(dataorder,aes(x=Variable, y=Importance))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("变量 variable")+ylab("相对重要性 Variable relative importance(%)")+theme_bw()+ggtitle("BRT HCB model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))

type <- c("spatial","stand","spatial","spatial","stand","spatial","tree species","spatial","tree species","stand","tree","density","tree","tree species","density","spatial","spatial","tree species","tree","tree species","tree","tree","stand","tree species","tree")
dataorder <- cbind(dataorder,type)

library(ggrepel)

dataorder$Importance <- round(dataorder$Importance/sum(dataorder$Importance)*100,2)
impb <- ggplot(dataorder,aes(Importance,as.factor(Variable),color=type))+facet_wrap(~type)+geom_point()+xlab("relative importance(%)")+ylab("")+theme_bw()+ggtitle("Coniferous group")+theme(plot.title = element_text(size = 12,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  facet_grid(type~.,scales='free',space='free')+
  # xlim(0,100)+
  geom_pointrange(aes(xmin = 0, xmax = Importance))+
  geom_text_repel(aes(label=Importance,color=type),hjust=0.5,segment.color="black",segment.size=0.5,size=4,nudge_x = 0.25)+##arrow = arrow(ends = "first",length = unit(0.01,"npc"))+nudge_y = 0+size=2,alpha=0.5+box.padding = 0.8,point.padding = 0.8+force = 1,max.iter = 3e3
  # geom_text(aes(label=Importance),check_overlap = F)+
  # coord_flip()+
  theme(strip.text.y=element_text(angle=90))#设置y轴标签

# impHCBgbm1 <- ggarrange(imp2,imp4,ncol = 2,nrow = 1, common.legend = T)

# impHCBgbm <- ggarrange(imp1,imp2,imp3,imp4,ncol = 2,nrow = 2, common.legend = T)
ggsave("D:/zzy essay/2024论文/HCB nonparametric/HCB nonparametric/HCB nonparametric/HCB nonparametric model/redrawn plots/coni.imp.BRT.pdf",impb,dpi = 400)
ggsave("D:/zzy essay/2024论文/HCB nonparametric/HCB nonparametric/HCB nonparametric/HCB nonparametric model/redrawn plots/coni.imp.BRT.tiff",impb,dpi = 400)

##############################################################################################

##-- the partial dependence plots of HCB model of BRT
library(pdp)
library(vip)
library(ggplot2)
library(lattice)
partialPlot(fit.gbm.HCB.HardB, pred.data = essay_train_HardB, x.var = "lstat")


###-- SoftB

top25gbm <- topPredictors(fit.gbm.HCB.SoftB, n = 25)
# Construct partial dependence functions for top four predictors
pdgbmhcb <- NULL
for (i in top25gbm) {
  tmp <- partial(fit.gbm.HCB.SoftB, pred.var = i,n.trees=150)
  names(tmp) <- c("x", "y")
  pdgbmhcb <- rbind(pdgbmhcb, cbind(tmp, predictor = i))
}

# Display partial dependence functions
HCBgbmpartial1 <- ggplot(pdgbmhcb, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=6,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=6,face = "bold"))+
  xlab("Variables")+ylab("HCB/m")+ggtitle("Partial dependence plots of Soft-broadleaved group HCB BRT model")+theme(plot.title = element_text(size = 14,hjust = 0.45, vjust=1.0))

ggsave("D:/zzy essay/2024论文/HCB nonparametric/HCB nonparametric/HCB nonparametric/HCB nonparametric model/redrawn plots/softb.PDP.BRT.pdf",HCBgbmpartial1,dpi = 400)




###-- Coni

top25gbm <- topPredictors(fit.gbm.HCB.Coni, n = 25)
# Construct partial dependence functions for top four predictors
pdgbmhcb <- NULL
for (i in top25gbm) {
  tmp <- partial(fit.gbm.HCB.Coni, pred.var = i,n.trees=150)
  names(tmp) <- c("x", "y")
  pdgbmhcb <- rbind(pdgbmhcb, cbind(tmp, predictor = i))
}

# Display partial dependence functions
HCBgbmpartial2 <- ggplot(pdgbmhcb, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=6,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=6,face = "bold"))+
  xlab("Variables")+ylab("HCB/m")+ggtitle("Partial dependence plots of Coniferous group HCB BRT model")+theme(plot.title = element_text(size = 14,hjust = 0.45, vjust=1.0))

ggsave("D:/zzy essay/2024论文/HCB nonparametric/HCB nonparametric/HCB nonparametric/HCB nonparametric model/redrawn plots/coni.PDP.BRT.pdf",HCBgbmpartial2,dpi = 400)

HCB.BRT.partialconisoft <- ggarrange(HCBgbmpartial2,HCBgbmpartial1,nrow = 2,ncol = 1,common.legend = T)

ggsave("D:/zzy essay/2024论文/HCB nonparametric/HCB nonparametric/HCB nonparametric/HCB nonparametric model/redrawn plots/PDPconi.tiff",HCBgbmpartial2,dpi = 300,limitsize = FALSE)
ggsave("D:/zzy essay/2024论文/HCB nonparametric/HCB nonparametric/HCB nonparametric/HCB nonparametric model/redrawn plots/PDPsoft.tiff",HCBgbmpartial1,dpi = 300,limitsize = FALSE)

# HCBgbmpartialzong1 <- ggarrange(HCBgbmpartial2,HCBgbmpartial4,ncol = 1,nrow = 2)
# HCBgbmpartialzong <- ggarrange(HCBgbmpartial1,HCBgbmpartial2,HCBgbmpartial3,HCBgbmpartial4,ncol = 2,nrow = 2)
# ggsave("D:/files and materials/zzy essay/2023essay/HCB nonparametric model/HCBgbmpartialzong1.tiff",width=25,height=50,units="cm",dpi = 500,HCBgbmpartialzong1)







##-- BRT model with function of gbm

gbm_HCB.HardB <- gbm(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                     data=essay_train_HardB,                   
                     distribution="gaussian",    
                     n.trees=150,                
                     shrinkage=0.1,             
                     # 0.001 to 0.1 usually work
                     interaction.depth=2,         
                     # bag.fraction = 0.5,          
                     # train.fraction = 0.5,        
                     # n.minobsinnode = 10,       
                     cv.folds = 10,              
                     keep.data=TRUE,            
                     verbose=T,              
                     n.cores=1) 

gbm_HCB.SoftB <- gbm(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                     data=essay_train_SoftB,                   
                     distribution="gaussian",    
                     n.trees=100,                
                     shrinkage=0.1,             
                     # 0.001 to 0.1 usually work
                     interaction.depth=3,         
                     # bag.fraction = 0.5,          
                     # train.fraction = 0.5,        
                     # n.minobsinnode = 10,       
                     cv.folds = 10,              
                     keep.data=TRUE,            
                     verbose=T,              
                     n.cores=1) 

gbm_HCB.YLS <- gbm(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                   data=essay_train_YLS,                   
                   distribution="gaussian",    
                   n.trees=150,                
                   shrinkage=0.1,             
                   # 0.001 to 0.1 usually work
                   interaction.depth=3,         
                   # bag.fraction = 0.5,          
                   # train.fraction = 0.5,        
                   # n.minobsinnode = 10,       
                   cv.folds = 10,              
                   keep.data=TRUE,            
                   verbose=T,              
                   n.cores=1) 

gbm_HCB.Coni <- gbm(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DT+HT+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                    data=essay_train_Coni,                   
                    distribution="gaussian",    
                    n.trees=50,                
                    shrinkage=0.01,             
                    # 0.001 to 0.1 usually work
                    interaction.depth=3,         
                    # bag.fraction = 0.5,          
                    # train.fraction = 0.5,        
                    # n.minobsinnode = 10,       
                    cv.folds = 10,              
                    keep.data=TRUE,            
                    verbose=T,              
                    n.cores=1) 


##-- importance plot
importance_otu.scale <- data.frame(relative.influence(gbm_HCB,n.trees = 150,scale = TRUE), check.names = T)
importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)
dataorder <- importance_otu.scale %>% arrange(importance_otu.scale$relative.influence.gbm_HCB..n.trees...150..scale...TRUE) #按照count一列降序排序
dataorder$OTU_name<-factor(dataorder$OTU_name,levels = unique(dataorder$OTU_name),ordered = T) #按照trait一列作为X轴排序
HCBimp <- ggplot(dataorder,aes(x=OTU_name, y=(relative.influence.gbm_HCB..n.trees...150..scale...TRUE.)*100))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("变量 variable")+ylab("相对重要性 Variable relative importance(%)")+theme_bw()+ggtitle("BRT HCB model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))
ggsave("D:/zzy essay/博士大论文/model results/HCB model/GBM importance plot.tiff",HCBimp)


##-- the partial dependence plots of HCB model of GBM
library(ggplot2)
library(lattice)

top21gbm <- topPredictors(fit.gbm.HCB, n = 21)
# Construct partial dependence functions for top four predictors
pdgbmhcb <- NULL
for (i in top21gbm) {
  tmp <- partial(gbm_HCB, pred.var = i,n.trees=150)
  names(tmp) <- c("x", "y")
  pdgbmhcb <- rbind(pdgbmhcb, cbind(tmp, predictor = i))
}

# Display partial dependence functions
HCBgbmpartial <- ggplot(pdgbmhcb, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=6,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=6,face = "bold"))+
  xlab("Variables")+ylab("HCB/m")+ggtitle("Partial dependence plots of HCB BRT model")+theme(plot.title = element_text(size = 14,hjust = 0.45, vjust=1.0))
ggsave("D:/zzy essay/博士大论文/model results/HCB model/PDP gbm HCB.tiff",HCBgbmpartial)

## model prediction of GBM with validation data of 4 tree species groups
predicted.GBM.HardB <- predict(gbm_HCB.HardB,newdata = essay_test_HardB,type = "response")
residuals.GBM.HardB <- predicted.GBM.HardB-essay_test_HardB$HCB
zhibiao.HardB <- FittingEvaluationIndex(predicted.GBM.HardB,essay_test_HardB$HCB,0)

predicted.GBM.SoftB <- predict(gbm_HCB.SoftB,newdata = essay_test_SoftB,type = "response")
residuals.GBM.SoftB <- predicted.GBM.SoftB-essay_test_SoftB$HCB
zhibiao.SoftB <- FittingEvaluationIndex(predicted.GBM.SoftB,essay_test_SoftB$HCB,0)

predicted.GBM.YLS <- predict(gbm_HCB.YLS,newdata = essay_test_YLS,type = "response")
residuals.GBM.YLS <- predicted.GBM.YLS-essay_test_YLS$HCB
zhibiao.YLS <- FittingEvaluationIndex(predicted.GBM.YLS,essay_test_YLS$HCB,0)

predicted.GBM.Coni <- predict(gbm_HCB.Coni,newdata = essay_test_Coni,type = "response")
residuals.GBM.Coni <- predicted.GBM.Coni-essay_test_Coni$HCB
zhibiao.Coni <- FittingEvaluationIndex(predicted.GBM.Coni,essay_test_Coni$HCB,0)

zhibiao <- cbind(zhibiao.HardB,zhibiao.SoftB,zhibiao.YLS,zhibiao.Coni)


essay_test_HardB.new <- cbind(essay_test_HardB,predicted.GBM.HardB)
essay_test_HardB.new <- cbind(essay_test_HardB.new,residuals.GBM.HardB)
essay_test_HardB.new <- rename(essay_test_HardB.new,preHCB=predicted.GBM.HardB)

essay_test_SoftB.new <- cbind(predicted.GBM.SoftB,essay_test_SoftB)
essay_test_SoftB.new <- cbind(essay_test_SoftB.new,residuals.GBM.SoftB)
essay_test_SoftB.new <- rename(essay_test_SoftB.new,preHCB=predicted.GBM.SoftB)

essay_test_YLS.new <- cbind(essay_test_YLS,predicted.GBM.YLS)
essay_test_YLS.new <- cbind(essay_test_YLS.new,residuals.GBM.YLS)
essay_test_YLS.new <- rename(essay_test_YLS.new,preHCB=predicted.GBM.YLS)

essay_test_Coni.new <- cbind(essay_test_Coni,predicted.GBM.Coni)
essay_test_Coni.new <- cbind(essay_test_Coni.new,residuals.GBM.Coni)
essay_test_Coni.new <- rename(essay_test_Coni.new,preHCB=predicted.GBM.Coni)

str(essay_test_Coni.new)
str(essay_test_Coni.new1)

str(essay_test_YLS.new)
str(essay_test_YLS.new1)

str(essay_test_HardB.new)
str(essay_test_HardB.new1)

str(essay_test_SoftB.new)
str(essay_test_SoftB.new1)

GBM.HardB <- data.frame(residuals.GBM.HardB,predicted.GBM.HardB)
Cubist.HardB <- data.frame(residuals.cubist.HardB,predicted.cubist.HardB)
HardB.residuals.data <- cbind(GBM.HardB,Cubist.HardB)
HardB.residuals.data1 <- data.frame(HardB.residuals.data,rep("Hard broadleaved tree species group",1091))
write.xlsx(HardB.residuals.data1,"D:/files and materials/论文/2023文章/data/HardBresiduals.xlsx",sheetName="sheet1")

GBM.SoftB <- data.frame(residuals.GBM.SoftB,predicted.GBM.SoftB)
Cubist.SoftB <- data.frame(residuals.cubist.SoftB,predicted.cubist.SoftB)
SoftB.residuals.data <- cbind(GBM.SoftB,Cubist.SoftB)
SoftB.residuals.data1 <- data.frame(SoftB.residuals.data,rep("Soft broadleaved tree species group",524))
write.xlsx(SoftB.residuals.data1,"D:/files and materials/论文/2023文章/data/SoftBresiduals.xlsx",sheetName="sheet1")

GBM.Coni <- data.frame(residuals.GBM.Coni,predicted.GBM.Coni)
Cubist.Coni <- data.frame(residuals.cubist.Coni,predicted.cubist.Coni)
Coni.residuals.data <- cbind(GBM.Coni,Cubist.Coni)
Coni.residuals.data1 <- data.frame(Coni.residuals.data,rep("coniferous tree species group",678))
write.xlsx(Coni.residuals.data1,"D:/files and materials/论文/2023文章/data/Coniresiduals.xlsx",sheetName="sheet1")

GBM.YLS <- data.frame(residuals.GBM.YLS,predicted.GBM.YLS)
Cubist.YLS <- data.frame(residuals.cubist.YLS,predicted.cubist.YLS)
YLS.residuals.data <- cbind(GBM.YLS,Cubist.YLS)
YLS.residuals.data1 <- data.frame(YLS.residuals.data,rep("spruce fir tree species group",728))
write.xlsx(YLS.residuals.data1,"D:/files and materials/论文/2023文章/data/YLSresiduals.xlsx",sheetName="sheet1")




### measured values VS predicted values plots for 4 tree species groups
p1 <- ggplot(essay_test_HardB.new,aes(x=HCB,y=preHCB)) + 
  geom_point(alpha = 1/5,shape = 21, colour = "black", fill = "black",size = 3,stroke = 2)+
  stat_smooth(method='lm',formula = y~x,colour='red') +
  scale_x_continuous(limits = c(0,18)) + 
  scale_y_continuous(limits = c(0,18)) + 
  geom_abline(slope = 1,intercept = 0,size=1.5,mapping = T,show.legend = T) +
  annotate("text", x = 2, y = 15, label = "atop(R^2==0.5718)", parse = TRUE, size=5) +
  annotate("text", x = 2, y = 13.5, label="P<0.001", size=5) +
  theme_bw() +
  theme_classic()+
  theme(axis.text=element_text(colour = 'black',size = 16), axis.title=element_text(size = 16)) +
  annotate("text", x=50, y=15, label="sqp", hjust=0, size=4,parse=TRUE)+
  labs(x="measured values/m",y="prediced values/m",title = "Hard-broadleaved group")+ theme(plot.title = element_text(hjust = 0.5,size = 16))

p2 <- ggplot(essay_test_SoftB.new,aes(x=HCB,y=preHCB)) + 
  geom_point(alpha = 1/5,shape = 21, colour = "black", fill = "black",size = 3,stroke = 2)+
  stat_smooth(method='lm',formula = y~x,colour='red') +
  scale_x_continuous(limits = c(0,18)) + 
  scale_y_continuous(limits = c(0,18)) + 
  geom_abline(slope = 1,intercept = 0,size=1.5,mapping = T,show.legend = T) +
  annotate("text", x = 2, y = 15, label = "atop(R^2==0.6167)", parse = TRUE, size=5) +
  annotate("text", x = 2, y = 13.5, label="P<0.001", size=5) +
  theme_bw() +
  theme_classic()+
  theme(axis.text=element_text(colour = 'black',size = 16), axis.title=element_text(size = 16)) +
  annotate("text", x=50, y=15, label="sqp", hjust=0, size=4,parse=TRUE)+
  labs(x="measured values/m",y="prediced values/m",title = "Soft-broadleaved group")+ theme(plot.title = element_text(hjust = 0.5,size = 16))

p3 <- ggplot(essay_test_YLS.new,aes(x=HCB,y=preHCB)) + 
  geom_point(alpha = 1/5,shape = 21, colour = "black", fill = "black",size = 3,stroke = 2)+
  stat_smooth(method='lm',formula = y~x,colour='red') +
  scale_x_continuous(limits = c(0,18)) + 
  scale_y_continuous(limits = c(0,18)) + 
  geom_abline(slope = 1,intercept = 0,size=1.5,mapping = T,show.legend = T) +
  annotate("text", x = 2, y = 15, label = "atop(R^2==0.5668)", parse = TRUE, size=5) +
  annotate("text", x = 2, y = 13.5, label="P<0.001", size=5) +
  theme_bw() +
  theme_classic()+
  theme(axis.text=element_text(colour = 'black',size = 16), axis.title=element_text(size = 16)) +
  annotate("text", x=50, y=15, label="sqp", hjust=0, size=4,parse=TRUE)+
  labs(x="measured values/m",y="prediced values/m",title = "Spruce-Fir group")+ theme(plot.title = element_text(hjust = 0.5,size = 16))

p4 <- ggplot(essay_test_Coni.new,aes(x=HCB,y=preHCB)) + 
  geom_point(alpha = 1/5,shape = 21, colour = "black", fill = "black",size = 3,stroke = 2)+
  stat_smooth(method='lm',formula = y~x,colour='red') +
  scale_x_continuous(limits = c(0,18)) + 
  scale_y_continuous(limits = c(0,18)) + 
  geom_abline(slope = 1,intercept = 0,size=1.5,mapping = T,show.legend = T) +
  annotate("text", x = 2, y = 15, label = "atop(R^2==0.4343)", parse = TRUE, size=5) +
  annotate("text", x = 2, y = 13.5, label="P<0.001", size=5) +
  theme_bw() +
  theme_classic()+
  theme(axis.text=element_text(colour = 'black',size = 16), axis.title=element_text(size = 16)) +
  annotate("text", x=50, y=15, label="sqp", hjust=0, size=4,parse=TRUE)+
  labs(x="measured values/m",y="prediced values/m",title = "Coniferous group")+ theme(plot.title = element_text(hjust = 0.5,size = 16))

shiceyuceBRT1 <- ggarrange(p2,p4,ncol = 2,nrow = 1,common.legend = T)
shiceyuceBRT <- ggarrange(p1,p2,p3,p4,ncol = 2,nrow = 2,common.legend = T)
ggsave("D:/files and materials/zzy essay/2023essay/HCB nonparametric model/shiceyuceBRT1SoftConi.tiff",width=40,height=20,units="cm",dpi = 500,shiceyuceBRT1)

shiceyuceBRTCubist <- ggarrange(p2,p4,pa,pc,ncol = 2,nrow = 2,common.legend = T)
ggsave("D:/files and materials/zzy essay/2023essay/HCB nonparametric model/shiceyuceBRTCubist.tiff",width=40,height=40,units="cm",dpi = 500,shiceyuceBRTCubist)


## residuals plots
pq <- ggplot(essay_test_HardB.new,aes(x=preHCB,y=residuals.GBM.HardB)) + 
  geom_point(alpha = 1/5,shape = 21, colour = "black", fill = "black",size = 3,stroke = 2)+
  # stat_smooth(method='lm',formula = y~x,colour='red') +
  scale_x_continuous(limits = c(1.5,15)) +
  scale_y_continuous(limits = c(-5,9)) +
  geom_abline(slope = 0,intercept = 0,size=1.0,mapping = T,show.legend = T) +
  # annotate("text", x = 2, y = 15, label = "atop(R^2==0.5718)", parse = TRUE, size=5) +
  # annotate("text", x = 2, y = 13.5, label="P<0.001", size=5) +
  theme_bw() +
  theme_classic()+
  theme(axis.text=element_text(colour = 'black',size = 16), axis.title=element_text(size = 16)) +
  annotate("text", x=50, y=15, label="sqp", hjust=0, size=4,parse=TRUE)+
  labs(x="measured values/m",y="residuals/m",title = "Hard-broadleaved group")+ theme(plot.title = element_text(hjust = 0.5,size = 16))

pw <- ggplot(essay_test_SoftB.new,aes(x=preHCB,y=residuals.GBM.SoftB)) + 
  geom_point(alpha = 1/5,shape = 21, colour = "black", fill = "black",size = 3,stroke = 2)+
  # stat_smooth(method='lm',formula = y~x,colour='red') +
  scale_x_continuous(limits = c(1.5,15)) +
  scale_y_continuous(limits = c(-5,9)) +
  geom_abline(slope = 0,intercept = 0,size=1.0,mapping = T,show.legend = T) +
  # annotate("text", x = 2, y = 15, label = "atop(R^2==0.6167)", parse = TRUE, size=5) +
  # annotate("text", x = 2, y = 13.5, label="P<0.001", size=5) +
  theme_bw() +
  theme_classic()+
  theme(axis.text=element_text(colour = 'black',size = 16), axis.title=element_text(size = 16)) +
  annotate("text", x=50, y=15, label="sqp", hjust=0, size=4,parse=TRUE)+
  labs(x="measured values/m",y="residuals/m",title = "Soft-broadleaved group")+ theme(plot.title = element_text(hjust = 0.5,size = 16))

pe <- ggplot(essay_test_YLS.new,aes(x=preHCB,y=residuals.GBM.YLS)) + 
  geom_point(alpha = 1/5,shape = 21, colour = "black", fill = "black",size = 3,stroke = 2)+
  # stat_smooth(method='lm',formula = y~x,colour='red') +
  scale_x_continuous(limits = c(1.5,15)) +
  scale_y_continuous(limits = c(-5,9)) +
  geom_abline(slope = 0,intercept = 0,size=1.0,mapping = T,show.legend = T) +
  # annotate("text", x = 2, y = 15, label = "atop(R^2==0.5668)", parse = TRUE, size=5) +
  # annotate("text", x = 2, y = 13.5, label="P<0.001", size=5) +
  theme_bw() +
  theme_classic()+
  theme(axis.text=element_text(colour = 'black',size = 16), axis.title=element_text(size = 16)) +
  annotate("text", x=50, y=15, label="sqp", hjust=0, size=4,parse=TRUE)+
  labs(x="measured values/m",y="residuals/m",title = "Spruce-Fir group")+ theme(plot.title = element_text(hjust = 0.5,size = 16))

pr <- ggplot(essay_test_Coni.new,aes(x=preHCB,y=residuals.GBM.Coni)) + 
  geom_point(alpha = 1/5,shape = 21, colour = "black", fill = "black",size = 3,stroke = 2)+
  # stat_smooth(method='lm',formula = y~x,colour='red') +
  scale_x_continuous(limits = c(1.5,15)) +
  scale_y_continuous(limits = c(-5,9)) +
  geom_abline(slope = 0,intercept = 0,size=1.0,mapping = T,show.legend = T) +
  # annotate("text", x = 2, y = 15, label = "atop(R^2==0.4343)", parse = TRUE, size=5) +
  # annotate("text", x = 2, y = 13.5, label="P<0.001", size=5) +
  theme_bw() +
  theme_classic()+
  theme(axis.text=element_text(colour = 'black',size = 16), axis.title=element_text(size = 16)) +
  annotate("text", x=50, y=15, label="sqp", hjust=0, size=4,parse=TRUE)+
  labs(x="measured values/m",y="residuals/m",title = "Coniferous group")+ theme(plot.title = element_text(hjust = 0.5,size = 16))


yucecanchaBRT <- ggarrange(pq,pw,pe,pr,ncol = 2,nrow = 2,common.legend = T)
