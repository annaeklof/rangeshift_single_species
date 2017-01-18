library(dismo)

angaus.tc5.lr01 <- gbm.step(data=Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                             family = "bernoulli", tree.complexity = 5,
                             learning.rate = 0.01, bag.fraction = 0.5)

A <- MyData$PopulationNumber;
C <- A > 5000
MyData$AbsPres <- C
testTree <- gbm.step(data=MyData, gbm.x = 1:3, gbm.y = 8,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.005, bag.fraction = 0.5)

MyTree <- rpart(AbsPres~factor(LandscapeType) + aValue + 
                  Simlength, data=MyData,control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree <- prune(MyTree,cp= 0.01)
fancyRpartPlot(PrunedTree)




Method <- factor('electric', levels = levels(Anguilla_train$Method))
 add <- data.frame(Method)
 p <- predict(Anguilla_grids, angaus.tc5.lr01, const=add, n.trees=angaus.tc5.lr01$gbm.call$best.trees, type='response')
 p <- mask(p, raster(Anguilla_grids, 1))
 plot(p, main='Angaus - BRT prediction')

 