
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
tree.comnew <- read.xlsx("tree.comnew.xlsx")

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
##-- cubist HCB model

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


#------------------trainControl()--------------------------------------
# method表示进行重采样方法 cv:cross validation,number表示 k折交叉验证，repeats表示重复交叉验证的次数
HCB.cubist <- round(HCB_tuned$results,digits = 4)
write.xlsx(HCB.cubist,"D:/zzy essay/博士大论文/model results/HCB model/cubist fitting results.xlsx")
HCB.cubist.committee.RMSE <- ggplot(HCB_tuned) +
  theme(legend.position = "top")+xlab("committes") + ylab("RMSE/m")
ggsave("D:/zzy essay/博士大论文/model results/HCB model/HCB.cubist.committee.RMSE.tiff",HCB.cubist.committee.RMSE)

##-- importance plot of cubist HCB model
##-- importance plot
cubist.HCB.imp <- vi(HCB_HardB_tuned)
dataorder <- cubist.HCB.imp %>% arrange(cubist.HCB.imp$Importance) #按照Importance一列升序排序
dataorder$Variable<-factor(dataorder$Variable,levels = unique(dataorder$Variable),ordered = T) #按照trait一列作为X轴排序
HCB.cubist.imp <- ggplot(dataorder,aes(x=Variable, y=Importance))+geom_bar(stat = "identity",width = 0.8)+coord_flip()+xlab("变量 variable")+ylab("相对重要性 Variable relative importance(%)")+theme_bw()+ggtitle("Cubist HCB model importance")+theme(plot.title = element_text(size = 18,hjust = 0.45, vjust=1.0))+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+ theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))
ggsave("D:/zzy essay/博士大论文/model results/HCB model/Cubist imp HCB plot.tiff",HCB.cubist.imp)

################################# PDP plot of HCB cubist #######################################

####### HardB ##########
top21hcb.cubist <- topPredictors(HCB_HardB_tuned, n = 30)
pdcubisthcb <- NULL
for (i in top21hcb.cubist) {
  tmp <- partial(HCB_HardB_tuned, pred.var = i)
  names(tmp) <- c("x", "y")
  pdcubisthcb <- rbind(pdcubisthcb, cbind(tmp, predictor = i))
}

# Display partial dependence functions
HCBcubist.partial <- ggplot(pdcubisthcb, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=6,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=6,face = "bold"))+
  xlab("Variables")+ylab("HCB/m")+ggtitle("Partial dependence plots of HCB Cubist model")+theme(plot.title = element_text(size = 16,hjust = 0.45, vjust=1.0))


####### YLS ##########
top21hcb.cubist <- topPredictors(HCB_HardB_tuned, n = 30)
pdcubisthcb <- NULL
for (i in top21hcb.cubist) {
  tmp <- partial(HCB_HardB_tuned, pred.var = i)
  names(tmp) <- c("x", "y")
  pdcubisthcb <- rbind(pdcubisthcb, cbind(tmp, predictor = i))
}

# Display partial dependence functions
HCBcubist.partial <- ggplot(pdcubisthcb, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +theme(axis.text.x=element_text(vjust=1,size=6,face = "bold"))+theme(axis.text.y=element_text(vjust=1,size=6,face = "bold"))+
  xlab("Variables")+ylab("HCB/m")+ggtitle("Partial dependence plots of HCB Cubist model")+theme(plot.title = element_text(size = 16,hjust = 0.45, vjust=1.0))

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


ggsave("D:/files and materials/zzy essay/2023essay/HCB nonparametric model/HcubistcommitteeR2.tiff",CubistH,dpi = 400)

