## LASSO
LASSO <- NULL

for (k in 1:nfold) {
  message('+++++++++++++++++++++++++++++++++++++++++++++++')
  i1<-which(foldid!=k)
  i2<-which(foldid==k)
  
  x1<-geno[i1,genes,drop=F]
  y1<-pheno[i1,,drop=F]
  
  x2<-geno[i2,genes,drop=F]
  y2<-pheno[i2,,drop=F]
  
  model.fit=cv.glmnet(x1, y1,family="binomial",type.measure="auc")
  ypred=as.numeric(predict(model.fit,x2, s="lambda.min", type='class'))
  yprob=predict(model.fit,x2, s="lambda.min", type='response')
  
  LASSO <- rbind(LASSO, data.frame(yobs=y2, ypred=ypred, yprob))
  
}

LASSO <- data.frame(LASSO, stringsAsFactors = F)
colnames(LASSO) <- c('yobs', 'ypred', 'yprob')


## SVM-RBF
library(kernlab)

SVMRBF <- NULL

for (k in 1:nfold) {
  message(paste(rep(c('+',k,'+'), each=20), collapse = '')) 
  i1<-which(foldid!=k)
  i2<-which(foldid==k)
  
  x1<-geno[i1,genes,drop=F]
  y1<-pheno[i1,,drop=F]
  
  x2<-geno[i2,genes,drop=F]
  y2<-pheno[i2,,drop=F]
  
  kern<- ksvm(x=x1,y=y1,type='C-svc',kernel = "rbfdot",kpar = "automatic",cross=0)
  ypred<-predict(kern,x2,type='response')
  yprob<-predict(kern,x2,type='decision')
  
  SVMRBF <- rbind(SVMRBF, data.frame(yobs=y2, ypred=ypred, yprob))
  
}

SVMRBF <- data.frame(SVMRBF, stringsAsFactors = F)
colnames(SVMRBF) <- c('yobs', 'ypred', 'yprob')
