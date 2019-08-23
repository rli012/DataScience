#################
library(caret)

### Accuracy
control <- trainControl(method="repeatedcv", number=5, repeats=3)

### ROC AUC
control <- trainControl(method="repeatedcv", number=5, repeats=3, 
                        classProbs=TRUE, summaryFunction=twoClassSummary)

seeds <- sample(111111, 20)
seeds

imptDa <- c()
imptDa100 <- c()

for (seed in seeds) {
  print (seed)
  set.seed(seed)
  
  rf_random <- train(trainingDa, sensitivity, 
                     method = "rf", 
                     #tuneGrid = data.frame(mtry = 2),
                     ntree = 100, 
                     tuneLength = 10, # nfeature - 1
                     trControl = control,
                     #trControl = trainControl(method = "boot"),
                     #trControl = trainControl(method = "oob", seed = seeds),
                     #seeds=77,
                     verbose = FALSE,
                     allowParallel = FALSE)
  
  impt <- varImp(rf_random, scale=FALSE)
  impt100 <- rescale(unlist(impt$importance),to=c(0,100))
  
  imptDa <- rbind(imptDa, unlist(impt$importance))
  imptDa100 <- rbind(imptDa100, impt100)
  
}

rownames(imptDa) <- seeds
colnames(imptDa) <- colnames(trainingDa[,features])

rownames(imptDa100) <- seeds
colnames(imptDa100) <- colnames(trainingDa[,features])


#imptMean <- colMeans(imptDa100)
imptMean <- colMedians(imptDa100)
names(imptMean) <- colnames(imptDa100)

feature <- colnames(trainingDa)

imptPltDa <- data.frame(importance=c(imptDa100),
                        feature=factor(rep(feature, each=length(seeds)), levels=names(imptMean[order(imptMean, decreasing = F)])))
                        
imptPltDa


bxplt <- ggplot(data=imptPltDa, aes(x=feature, y=importance))

bxplt + geom_boxplot(fill='#00AFBB',color='black', alpha=0.7, 
                     outlier.shape = NA, outlier.size = NA,#outlier.colour = 'black',
                     outlier.fill = NA) +
  coord_flip() +
  geom_jitter(size=1.5, width = 0.1) +
  labs(x='Feature', y='Scaled importance') +
  
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = 'none') +
  #theme_set(theme_minimal()) #
  theme(axis.title.x =element_text(size=18, color='grey20'), 
        axis.title.y =element_text(size=14, color='black'), 
        axis.text = element_text(size=14),
        axis.text.x = element_text(angle = 0, hjust=0.5),
        strip.text = element_text(size=14)) +
  theme(panel.border = element_blank(),
        axis.ticks=element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank())


##############
imptPltDa <- data.frame(importance=imptMean,
                        feature=factor(feature, levels=names(imptMean[order(imptMean, decreasing = F)])))

bxplt <- ggplot(data=imptPltDa, aes(x=importance, y=feature))

bxplt+ geom_segment(aes(y=feature, x=0, xend=importance, yend=feature), color='grey45', size=1) +
  geom_point(color='lightskyblue', size=3) + #facet_grid(.~type) +
  xlab('Predictive importance')+ylab('Feature') +
  xlim(0,100) +
  theme_bw()+
  #theme_set(theme_minimal()) #
  theme(legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = 'right') +
  theme(axis.title=element_text(size=18, face='bold'), 
        axis.text = element_text(size=14),
        axis.text.x = element_text(angle = 0, hjust=0.5),
        strip.text = element_text(size=14)) +
  theme(axis.line = element_line(colour = "black"),
        #panel.border = element_rect(color='grey', size=1),
        panel.background = element_blank())


