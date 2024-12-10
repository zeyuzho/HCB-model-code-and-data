# install.packages(c("ggpmisc","caret","openxlsx","dplyr","Cubist","ggplot2","ggpubr","e1071","hier.part","relaimpo","rdacca.hp","ade4","adespatial","vegan","magrittr","stringi","foreach","lme4","MuMIn","scales","ggrepel","gridExtra","effects","effectsize","GGally","glmm.hp","partR2","randomForest","pdp","vip","gbm","earth","neuralnet","kernlab","twinspanR","lattice","nnet","mgcv","gamm","fitdistrplus","agricolae","car","systemfit","geoviz","Matrix","magic","Deriv"))
# install.packages(c("sf","terra","rasterVis","ggspatial","rgdal","rnaturalearth","rnaturalearthdata","raster","cowplot","mapchina","geojsonsf"))
# install.packages("tidyverse")
library(openxlsx)
library(dplyr)
library(Cubist)
library(caret)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

library(geoviz)
library(tidyverse)
library(sf)
library(terra)
library(rasterVis)
library(ggspatial)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(cowplot)
library(tidyverse)
library(mapchina)
library(geojsonsf)
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(raster)
# install.packages("devtools")
library(devtools)
# devtools::install_github("danlwarren/ENMTools")
library(ENMTools)

setwd("D:/zzy essay/2023文章/")
tree.comnew <- read.xlsx("tree.comnew.sxlsx")



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

##================================================================================================
##-- cubist HCB model for Hard broadleaved and Spruce fir groups

grid = expand.grid(committees=0:100,neighbors= seq(1, 100, length.out = 5) %>% floor())
grid = expand.grid(committees=0:100,neighbors= 0:9)


set.seed(1)
HCB_HardB_tuned <- train(
  HCB ~ RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DThardB+HThardB+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI, data = essay_train_HardB,
  method = "cubist",
  tuneGrid = grid,
  trControl = trainControl(method = "cv",number = 10)
  #trControl = trainControl(method = "repeatedcv",number = 10,repeats = 10) 
)

HCB_SoftB_tuned <- train(
  HCB ~ RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTsoftB+HTsoftB+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI, data = essay_train_SoftB,
  method = "cubist",
  tuneGrid = grid,
  trControl = trainControl(method = "cv",number = 10)
  #trControl = trainControl(method = "repeatedcv",number = 10,repeats = 10) 
)

HCB_YLS_tuned <- train(
  HCB ~ RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTyls+HTyls+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI, data = essay_train_YLS,
  method = "cubist",
  tuneGrid = grid,
  trControl = trainControl(method = "cv",number = 10)
  #trControl = trainControl(method = "repeatedcv",number = 10,repeats = 10) 
)

HCB_Coni_tuned <- train(
  HCB ~ RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI, data = essay_train_Coni,
  method = "cubist",
  tuneGrid = grid,
  trControl = trainControl(method = "cv",number = 10)
  #trControl = trainControl(method = "repeatedcv",number = 10,repeats = 10) 
)

P.HardB.Cubist.HCB <- predict(HCB_HardB_tuned,newdata=essay_test_HardB,type="raw")
FittingEvaluationIndex(P.HardB.Cubist.HCB,essay_test_HardB$HCB,0)
#------------------trainControl()--------------------------------------
# method表示进行重采样方法 cv:cross validation,number表示 k折交叉验证，repeats表示重复交叉验证的次数
HCB.cubist <- round(HCB_tuned$results,digits = 4)
write.xlsx(HCB.cubist,"cubist fitting results.xlsx")
HCB.cubist.committee.RMSE <- ggplot(HCB_tuned) +
  theme(legend.position = "top")+xlab("committes") + ylab("RMSE/m")

##-- importance plot of cubist HCB model
##-- importance plot
cubist.HCB.imp <- vi(HCB_HardB_tuned)
dataorder <- cubist.HCB.imp %>% arrange(cubist.HCB.imp$Importance) #按照Importance一列升序排序
dataorder$Variable<-factor(dataorder$Variable,levels = unique(dataorder$Variable),ordered = T) #按照trait一列作为X轴排序
HCB.cubist.imp <- ggplot(dataorder,aes(x=Variable, y=Importance))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("变量 variable")+ylab("相对重要性 Variable relative importance(%)")+theme_bw()+ggtitle("Cubist HCB model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))

################################# PDP plot of HCB cubist #######################################

####### HardB ##########
top25hcb.cubist.hardB <- topPredictors(HCB_HardB_tuned, n = 30)
pdcubisthcb.hardB <- NULL
for (i in top25hcb.cubist.hardB) {
  tmp <- partial(HCB_HardB_tuned, pred.var = i)
  names(tmp) <- c("x", "y")
  pdcubisthcb.hardB <- rbind(pdcubisthcb.hardB, cbind(tmp, predictor = i))
}

# Display partial dependence functions
HCBcubist.partial.hardB <- ggplot(pdcubisthcb.hardB, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=6,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=6,face = "bold"))+
  xlab("Variables")+ylab("HCB/m")+ggtitle("Partial dependence plots of Hard-Broadleaved group HCB Cubist model")+theme(plot.title = element_text(size = 14,hjust = 0.45, vjust=1.0))


####### YLS ##########
top25hcb.cubist.yls <- topPredictors(HCB_YLS_tuned, n = 30)
pdcubisthcb.yls <- NULL
for (i in top25hcb.cubist.yls) {
  tmp <- partial(HCB_YLS_tuned, pred.var = i)
  names(tmp) <- c("x", "y")
  pdcubisthcb.yls <- rbind(pdcubisthcb.yls, cbind(tmp, predictor = i))
}

# Display partial dependence functions
HCBcubist.partial.yls <- ggplot(pdcubisthcb.yls, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=6,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=6,face = "bold"))+
  xlab("Variables")+ylab("HCB/m")+ggtitle("Partial dependence plots of HCB Spruce-Fir group Cubist model")+theme(plot.title = element_text(size = 14,hjust = 0.45, vjust=1.0))

# yls.hardb.PDP.Cubist <- ggarrange(HCBcubist.partial.yls,HCBcubist.partial.hardB,nrow = 2,ncol = 1,widths = 40,heights = 90)

#############################################################################################

##-- importance plot
library(ggplot2)
# library(gbm)
library(pdp)
library(vip)

## HardB
Cubist.HardB.HCB.imp <- vi(HCB_HardB_tuned,type="rss")
dataorder <- Cubist.HardB.HCB.imp %>% arrange(Cubist.HardB.HCB.imp$Importance) #按照Importance一列升序排序
dataorder$Variable<-factor(dataorder$Variable,levels = unique(dataorder$Variable),ordered = T) #按照trait一列作为X轴排序
HCB.HardB.Cubist.imp <- ggplot(dataorder,aes(x=Variable, y=Importance))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("variable")+ylab("Variable relative importance(%)")+theme_bw()+ggtitle("Cubist HCB model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))

type <- c("tree","tree","spatial","tree","density","tree species","stand","stand","stand","tree species","spatial","spatial","spatial","density","spatial","spatial","spatial","stand","tree species","tree species","tree species","tree species","tree","tree","tree")
dataorder <- cbind(dataorder,type)

library(ggrepel)

dataorder$Importance <- round(dataorder$Importance/sum(dataorder$Importance)*100,2)
imp1 <- ggplot(dataorder,aes(Importance,as.factor(Variable),color=type))+facet_wrap(~type)+geom_point()+xlab("relative importance(%)")+ylab("")+theme_bw()+ggtitle("Hard-broadleaved group")+theme(plot.title = element_text(size = 12,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  facet_grid(type~.,scales='free',space='free')+
  # xlim(0,100)+
  geom_pointrange(aes(xmin = 0, xmax = Importance))+
  geom_text_repel(aes(label=Importance,color=type),hjust=0.5,segment.color="black",segment.size=0.5,size=4,nudge_x = 0.25)+##arrow = arrow(ends = "first",length = unit(0.01,"npc"))+nudge_y = 0+size=2,alpha=0.5+box.padding = 0.8,point.padding = 0.8+force = 1,max.iter = 3e3
  # geom_text(aes(label=Importance),check_overlap = F)+
  theme(strip.text.y=element_text(angle=90))#设置y轴标签



## YLS
Cubist.YLS.HCB.imp <- vi(HCB_YLS_tuned,type="rss")
dataorder <- Cubist.YLS.HCB.imp %>% arrange(Cubist.YLS.HCB.imp$Importance) #按照Importance一列升序排序
dataorder$Variable<-factor(dataorder$Variable,levels = unique(dataorder$Variable),ordered = T) #按照trait一列作为X轴排序
HCB.YLS.Cubist.imp <- ggplot(dataorder,aes(x=Variable, y=Importance))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("variable")+ylab("Variable relative importance(%)")+theme_bw()+ggtitle("Cubist HCB model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))

type <- c("spatial","tree","tree","tree","spatial","density","density","spatial","spatial","spatial","stand","tree species","spatial","spatial","tree species","stand","stand","tree species","tree species","tree","tree","tree species","tree species","stand","tree")
dataorder <- cbind(dataorder,type)

library(ggrepel)

dataorder$Importance <- round(dataorder$Importance/sum(dataorder$Importance)*100,2)
imp2 <- ggplot(dataorder,aes(Importance,as.factor(Variable),color=type))+facet_wrap(~type)+geom_point()+xlab("relative importance(%)")+ylab("")+theme_bw()+ggtitle("Spruce-Fir group")+theme(plot.title = element_text(size = 12,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  facet_grid(type~.,scales='free',space='free')+
  # xlim(0,100)+
  geom_pointrange(aes(xmin = 0, xmax = Importance))+
  geom_text_repel(aes(label=Importance,color=type),hjust=0.5,segment.color="black",segment.size=0.5,size=4,nudge_x = 0.25)+##arrow = arrow(ends = "first",length = unit(0.01,"npc"))+nudge_y = 0+size=2,alpha=0.5+box.padding = 0.8,point.padding = 0.8+force = 1,max.iter = 3e3
  # geom_text(aes(label=Importance),check_overlap = F)+
  theme(strip.text.y=element_text(angle=90))#设置y轴标签



#############################################################################################


library(grid)
library(gridExtra)

a <- HCB_HardB_tuned$results
a$neighbors <- as.factor(a$neighbors)
Cubist1 <- ggplot(data=a,aes(x=committees, y=Rsquared, group=neighbors,col=neighbors))+theme_bw()+ggtitle("Hard broadleaved group")+theme(plot.title = element_text(size = 10.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

b <- HCB_SoftB_tuned$results
b$neighbors <- as.factor(b$neighbors)
Cubist2 <- ggplot(data=b,aes(x=committees, y=Rsquared, group=neighbors,col=neighbors))+theme_bw()+ggtitle("Soft broadleaved group")+theme(plot.title = element_text(size = 10.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

c <- HCB_Coni_tuned$results
c$neighbors <- as.factor(c$neighbors)
Cubist3 <- ggplot(data=c,aes(x=committees, y=Rsquared, group=neighbors,col=neighbors))+theme_bw()+ggtitle("Other coniferous group")+theme(plot.title = element_text(size = 10.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

d <- HCB_YLS_tuned$results
d$neighbors <- as.factor(d$neighbors)
Cubist4 <- ggplot(data=d,aes(x=committees, y=Rsquared, group=neighbors,col=neighbors))+theme_bw()+ggtitle("Spruce-fir group")+theme(plot.title = element_text(size = 10.5,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_line()+geom_point()

CubistH <- ggarrange(Cubist1,Cubist2,Cubist3,Cubist4,ncol = 2,nrow = 2,common.legend = T)
# grid.arrange(Cubist1,Cubist2,Cubist3,Cubist4,ncol = 2,nrow = 2)




##================================================================================================
##-- BRT HCB model for Soft broadleaved and coniferous groups

##===========================================================================================================================================
##-- HCB gbm model

#########   Soft broadleaved species group    #############
fit.gbm.HCB.SoftB <- train(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTsoftB+HTsoftB+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                           data=essay_train_SoftB, method = "gbm", trControl = ctrl)

plot(fit.gbm.HCB.SoftB,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m",main="")

gbmHCBjg.SoftB <- round(fit.gbm.HCB.SoftB$results,digits = 4)

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




#########  Hard broadleaved species group    #############

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




#########   Spruce fir species group    #############

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




#########   Coniferous species group    #############

fit.gbm.HCB.Coni <- train(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                          data=essay_train_Coni, method = "gbm", trControl = ctrl)

plot(fit.gbm.HCB.Coni,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m",main="")

gbmHCBjg.Coni <- round(fit.gbm.HCB.Coni$results,digits = 4)

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


##############################################################################################

##-- the partial dependence plots of HCB model of GBM
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


HCB.BRT.partialconisoft <- ggarrange(HCBgbmpartial2,HCBgbmpartial1,nrow = 2,ncol = 1,common.legend = T)






##-- GBM model with function of gbm

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

GBM.SoftB <- data.frame(residuals.GBM.SoftB,predicted.GBM.SoftB)
Cubist.SoftB <- data.frame(residuals.cubist.SoftB,predicted.cubist.SoftB)
SoftB.residuals.data <- cbind(GBM.SoftB,Cubist.SoftB)
SoftB.residuals.data1 <- data.frame(SoftB.residuals.data,rep("Soft broadleaved tree species group",524))

GBM.Coni <- data.frame(residuals.GBM.Coni,predicted.GBM.Coni)
Cubist.Coni <- data.frame(residuals.cubist.Coni,predicted.cubist.Coni)
Coni.residuals.data <- cbind(GBM.Coni,Cubist.Coni)
Coni.residuals.data1 <- data.frame(Coni.residuals.data,rep("coniferous tree species group",678))

GBM.YLS <- data.frame(residuals.GBM.YLS,predicted.GBM.YLS)
Cubist.YLS <- data.frame(residuals.cubist.YLS,predicted.cubist.YLS)
YLS.residuals.data <- cbind(GBM.YLS,Cubist.YLS)
YLS.residuals.data1 <- data.frame(YLS.residuals.data,rep("spruce fir tree species group",728))




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

shiceyuceBRTCubist <- ggarrange(p2,p4,pa,pc,ncol = 2,nrow = 2,common.legend = T)



##########################################################################################
####################        generalized additive model GAM        #########################
##########################################################################################

library(locfit)
library(mgcv)
library(gamm)

## spruce fir tree species group

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




## HardB tree species group

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



## SoftB tree species group

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


## Coniferous tree species group

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




############################################################################################
####################### MARS (multiple adaptive regression spline)  ########################
############################################################################################

### taking the Hard broadleaved species group as an example to show how to develop MARS model

### Hard broadleaved species group

##-- HCB model
set.seed(1111) # for reproducibility
MARS.HardB <- train(
  HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
  data=essay_train_HardB,  method = "earth",  metric = "RMSE",  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)
# View results
cv_HCB$bestTune
#   nprune degree
# 5     45      1
# 最佳参数组合的模型性能
cv_HCB$results %>%
  dplyr::filter(nprune == cv_HCB$bestTune$nprune, degree == cv_HCB$bestTune$degree)
MARS.HCB <- round(cv_HCB$results,digits = 4)


##-- earth to develop HCB MARS model
MARS.HardB <- earth(HCB ~RatioHardB+RatioSoftB+RatioYLS+RatioConi+D+H+CW+density+Dqmax+BAL+BAS+Dg+Dq+DTconi+HTconi+Gini_D+Gini_H+Gini_BA+Ud+Td+M+Ucw+Uh+aCI+uaCI,
                    data=essay_train_HardB,nprune=34,degree=3)
plot(MARS.HardB)

##-- importance plot
MARS.MARS.HCB.imp <- vi(MARS.HardB,type="rss")
dataorder <- MARS.MARS.HCB.imp %>% arrange(MARS.MARS.HCB.imp$Importance) #按照Importance一列升序排序
dataorder$Variable<-factor(dataorder$Variable,levels = unique(dataorder$Variable),ordered = T) #按照trait一列作为X轴排序
HardB.HCB.MARS.imp <- ggplot(dataorder,aes(x=Variable, y=Importance))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("变量 variable")+ylab("相对重要性 Variable relative importance(%)")+theme_bw()+ggtitle("MARS HCB model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))


##-- the partial dependence plots of HCB model of MARS
library(ggplot2)
library(lattice)

top21MARS.HCB <- topPredictors(MARS.HardB, n = 20)
# Construct partial dependence functions for top 11 predictors
pdmarsHardBhcb <- NULL
for (i in top21MARS.HCB) {
  tmp <- partial(MARS.HardB, pred.var = i)
  names(tmp) <- c("x", "y")
  pdmarsHardBhcb <- rbind(pdmarsHardBhcb, cbind(tmp, predictor = i))
}

# Display partial dependence functions
Hard.HCBmarspartial <- ggplot(pdmarsHardBhcb, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=8,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=8,face = "bold"))+
  xlab("Variables")+ylab("HCB/m")+ggtitle("Partial dependence plots of HCB MARS model")+theme(plot.title = element_text(size = 16,hjust = 0.45, vjust=1.0))

