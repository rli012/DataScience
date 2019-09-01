
###########################
### ROC Curve

library(pROC)

ciauc <- ci(dataForThreshold$class,dataForThreshold$expr)
ciauc <- round(ciauc, 3)
ciauc

auc <- ciauc[2]
auc

rocobj <- roc(dataForThreshold$class, dataForThreshold$expr)
rocobj

plot.roc(rocobj, legacy.axes = TRUE)
plot.roc(rocobj, legacy.axes = FALSE)

TPR=sort(rocobj$sensitivities, decreasing = F)
FPR=1-sort(rocobj$specificities, decreasing = T)


df <- data.frame(TPR,FPR)
df
auc.test <- wilcox.test(FPR, TPR, alternative = 'two.sided')
pvalue <- format(auc.test$p.value,digits = 3)
pvalue


ggplot(df,aes(x=FPR,y=TPR))+geom_line(size = 1, alpha = 1,color='red')+
  labs(x = "False Positive Rate (1-Specificity)",y = "True Positive Rate (Sensitivity)")+ 
  #scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+
  geom_abline(intercept = 0, slope = 1) +
  #geom_segment(x=0,y=0,xend=1,yend=1, color='darkgreen') + xlim(0,1) + ylim(0,1) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour='black'),
        panel.background = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
  theme(strip.text.x = element_text(size = 12, colour = "black", angle=0)) +
  #ggplot2::annotate("text", 
  #                  x = 0.6, y = 0.125, # x and y coordinates of the text
  #                  label = paste('AUC = ',ciauc[2], ' (95% CI: ',ciauc[1], '-', ciauc[3], ')',sep=''), size = 5) #+
  ggplot2::annotate("text", 
                    x = 0.6, y = 0.16, # x and y coordinates of the text
                    label = paste('AUC = ',ciauc[2], sep=''),
                    size=5) +
  ggplot2::annotate("text", 
                    x = 0.6, y = 0.1, # x and y coordinates of the text
                    label = paste('95% CI: ',ciauc[1], '-', ciauc[3],sep=''),
                    size=5)
#ggplot2::annotate("text", 
#                  x = 0.6, y = 0.06, # x and y coordinates of the text
#                  label = paste('p = 1.47e-08'), size = 5)





smrocobj <- smooth(rocobj)
plot(smrocobj)


#TPR=smrocobj$sensitivities
#FPR=1-smrocobj$specificities

plot.roc(rocobj, legacy.axes = TRUE)
plot.roc(rocobj, legacy.axes = FALSE)

TPR=sort(rocobj$sensitivities, decreasing = F)
FPR=1-sort(rocobj$specificities, decreasing = T)

df <- data.frame(TPR,FPR)

auc.test <- wilcox.test(FPR, TPR, alternative = 'two.sided')
pvalue <- format(auc.test$p.value,digits = 3)
pvalue


ggplot(df,aes(x=FPR,y=TPR))+geom_line(size = 1, alpha = 1,color='red')+
  labs(x = "False Positive Rate (1-Specificity)",y = "True Positive Rate (Sensitivity)")+ 
  #scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+
  geom_abline(intercept = 0, slope = 1) +
  #geom_segment(x=0,y=0,xend=1,yend=1, color='darkgreen') + xlim(0,1) + ylim(0,1) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour='black'),
        panel.background = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
  theme(strip.text.x = element_text(size = 12, colour = "black", angle=0)) +
  #ggplot2::annotate("text", 
  #                  x = 0.6, y = 0.125, # x and y coordinates of the text
  #                  label = paste('AUC = ',ciauc[2], ' (95% CI: ',ciauc[1], '-', ciauc[3], ')',sep=''), size = 5) #+
  ggplot2::annotate("text", 
                    x = 0.6, y = 0.16, # x and y coordinates of the text
                    label = paste('AUC = ',ciauc[2], sep=''),
                    size=5) +
  ggplot2::annotate("text", 
                    x = 0.6, y = 0.1, # x and y coordinates of the text
                    label = paste('95% CI: ',ciauc[1], '-', ciauc[3],sep=''),
                    size=5)
#ggplot2::annotate("text", 
#                  x = 0.6, y = 0.06, # x and y coordinates of the text
#                  label = paste('p = 1.47e-08'), size = 5)



#########
library(ROCR)
pred <- prediction(dataForThreshold$expr,dataForThreshold$class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
perf

FPR <- perf@x.values[[1]]
TPR <- perf@y.values[[1]]
