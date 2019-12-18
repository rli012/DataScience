## LASSO
LASSO <- NULL

for (k in 1:nfold) {
  message(paste(rep(c('+',k,'+'), each=20), collapse = '')) 
  i1<-which(foldid!=k)
  i2<-which(foldid==k)
  
  x1<-geno[i1,genes,drop=F]
  y1<-pheno[i1,,drop=F]
  
  x2<-geno[i2,genes,drop=F]
  y2<-pheno[i2,,drop=F]
  
  data1 <- data.frame(y1, x1)
  data1$y1 <- factor(data1$y1)
  
  trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
  model.fit <- train(y1 ~ ., data = data1, method = "glmnet", trControl = trControl)
  model.fit
  
  yprob <- predict(model.fit, newdata = x2, type = "prob")
  yprob=yprob[,2]
  ypred <- predict(model.fit, newdata = x2, type = "raw")
  
  LASSO <- rbind(LASSO, data.frame(yobs=y2, ypred=ypred, yprob))
  
}

LASSO <- data.frame(LASSO, stringsAsFactors = F)

colnames(LASSO) <- c('yobs', 'ypred', 'yprob')
LASSO$ypred

sum(ifelse(LASSO$ypred>0.5,1,0) == LASSO$yobs)/length(LASSO$yobs)

table(pred=LASSO$ypred,true=LASSO$yobs)

roc1 <- roc(LASSO$yobs,LASSO$yprob,plot=TRUE,ci=TRUE,auc=TRUE)
roc1
roc1$ci[1]
roc1$ci[2]
roc1$ci[3]
