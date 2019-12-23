runCaretCV <- function(geno, pheno, foldid, genes=NULL, model=NULL, seed=77) {
  nfold <- length(unique(foldid))
  
  if (is.null(genes)) {
    genes <- colnames(geno)
  } else {
    genes <- intersect(genes, colnames(geno))
  }
  
  samples <- NULL
  OUTPUT <- NULL
  
  for (k in 1:nfold) {
    message(paste(rep(c('+',k,'+'), each=20), collapse = '')) 
    i1<-which(foldid!=k)
    i2<-which(foldid==k)
    
    x1<-geno[i1,genes,drop=F]
    y1<-pheno[i1,,drop=F]
    
    x2<-geno[i2,genes,drop=F]
    y2<-pheno[i2,,drop=F]
    
    samples <- c(samples, rownames(geno)[i2])

    tr.control <- trainControl(
      method = 'cv', #"repeatedcv",
      number = 10,
      #repeats = 3, 
      classProbs = TRUE,
      #returnResamp="all",
      search = 'grid', # random
      summaryFunction=twoClassSummary)
    
    set.seed(seed)
    if (model %in% c('glmnet','svmLinear','svmRadial','rf','pls','lda')) {
      model.fit <- train(x1, y1,
                       #y1 ~ ., data = tr.data,
                       method = model,
                       metric = 'ROC',
                       preProc = c("center", "scale"),
                       #tuneGrid = tune.grid,
                       tuneLength = 10, 
                       trControl = tr.control)
      
    } else if (model %in% c('svmPoly','xgbLinear', 'xgbTree','dnn')) {
      model.fit <- train(x1, y1,
                         #y1 ~ ., data = tr.data,
                         method = model,
                         metric = 'ROC',
                         preProc = c("center", "scale"),
                         #tuneGrid = tune.grid,
                         #tuneLength = 10, 
                         trControl = tr.control)
      
    }
    
    yprob <- predict(model.fit, newdata = x2, type = "prob")
    yprob=yprob$E1
    ypred <- predict(model.fit, newdata = x2, type = "raw")
    
    OUTPUT <- rbind(OUTPUT, data.frame(yobs=y2, ypred=ypred, yprob))
  }
  
  OUTPUT <- data.frame(OUTPUT, stringsAsFactors = F)
  colnames(OUTPUT) <- c('yobs', 'ypred', 'yprob')
  rownames(OUTPUT) <- samples
  
  return (OUTPUT)
}
