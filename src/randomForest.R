library(randomForest)
library(ggplot2)

source("utils.R")

plot_feature_importance = function(model) {
    data = data.frame(features=rownames(model$importance),
                      value=model$importance[c(1:nrow(model$importance))])
    ggplot(data, aes(x = features, y = value)) +
        geom_bar(stat = "identity", fill = "red")+
        labs(title = "Feature importance",
             x = "Feature",
             y = "MeanDecreaseGiri") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

get_best_RF = function(d, k, v) {
    n = length(unique(d$Class)) # number of classes (30)
    
    params.nodesize = c(1, 2, 4, 8)
    best.nodesize = 0
    best.accuracy = 0
    best.accuracies = numeric(k)
    best.confusion = matrix(0, nrow = n, ncol = n)
    
    for (p.nodesize in params.nodesize) {
        accuracies = numeric(k)
        accuracy.sum = 0
        confusion.sum = matrix(0, nrow = n, ncol = n)
        for (i in 1:k){
            data_split = kfold(d, k, i)
            d.learn = data_split$learn
            d.test = data_split$test
            forest.model = randomForest(Class~., d.learn, ntree=500, mtry=4, nodesize=p.nodesize, importance=T) 
            forest.predictions = predict(forest.model, d.test, type="class")
            confusion.sum = confusion.sum + table(d.test$Class, forest.predictions)
            accuracies[i] = length(which(forest.predictions == d.test$Class))/nrow(d.test)
            accuracy.sum = accuracy.sum + length(which(forest.predictions == d.test$Class))/nrow(d.test)
        }
        accuracy.avg = accuracy.sum / k
        if (accuracy.avg > best.accuracy) {
            best.accuracy = accuracy.avg
            best.accuracies = accuracies
            best.nodesize = p.nodesize
            best.confusion = confusion.sum
        }
    }
    if (v) {
        print("Random Forest")
        cat("tuned parameter nodesize =", best.nodesize,"\n")
        print_stats(best.accuracies)
        print(confusion.sum)
        print(get_class_accuracy(confusion.sum))
        model = randomForest(Class~., d, ntree=500, mtry=4, nodesize=best.nodesize) 
        print(plot_feature_importance(model))
    }
    
    return (best.accuracies)
}