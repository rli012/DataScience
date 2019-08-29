

#==========================================================#
#                             Actural                      #
#                       Positive   Negative                #
#             Positive      a         b                    #
# Predicted                                                #
#             Negative      c         d                    #
#                                                          #
#==========================================================#


pred <- ifelse(trainData$expr>threshold, 'Resistant', 'Sensitive')

a <- sum(trainData$class=='Sensitive' & pred=='Sensitive')
b <- sum(trainData$class=='Resistant' & pred=='Sensitive')
c <- sum(trainData$class=='Sensitive' & pred=='Resistant')
d <- sum(trainData$class=='Resistant' & pred=='Resistant')

sensitivity <- a/(a+c)
specificity <- d/(b+d)

trainJ <- sensitivity+specificity-1
dist <- sqrt((1-sensitivity)^2+(1-specificity)^2)

positivity <- a/(a+b)
negativity <- d/(c+d)

accuracy <- (a+d)/(a+b+c+d)
