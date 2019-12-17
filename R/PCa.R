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
LASSO$ypred

sum(ifelse(LASSO$ypred>0.5,1,0) == LASSO$yobs)/length(LASSO$yobs)

table(pred=ifelse(LASSO$ypred>0.5,1,0),true=LASSO$yobs)
