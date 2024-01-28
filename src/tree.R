require(tree)
source("utils.R")

get_best_tree = function(d, k, v) {
    
    params.minsize = c(2, 5, 10)
    params.mindev = c(0, 0.001, 0.005, 0.01, 0.05)
    params.minsize = c(2)
    params.mindev = c(0)
    params = expand.grid(minsize = params.minsize, mindev = params.mindev)
    
    best.minsize = 0
    best.mindev = 0
    best.accuracy = 0
    best.accuracies = numeric(k)
    
    for (i in 1:nrow(params)) {
        p.minsize = params$minsize[i]
        p.mindev = params$mindev[i]
        accuracies = numeric(k)
        accuracy.sum = 0
        for (i in 1:k){
            data_split = kfold(d, k, i)
            d.learn = data_split$learn
            d.test = data_split$test
            tree.model = tree(Class~., d.learn, mindev=p.mindev, minsize=p.minsize)
            tree.predictions = predict(tree.model, d.test, type="class")
            accuracies[i] = get_accuracy(tree.predictions, d.test)
            accuracy.sum = accuracy.sum + length(which(tree.predictions == d.test$Class))/nrow(d.test)
        }
        accuracy.avg = accuracy.sum / k
        if (accuracy.avg > best.accuracy) {
            best.accuracies = accuracies
            best.accuracy = accuracy.avg
            best.minsize = p.minsize
            best.mindev = p.mindev
        }
    }
    if (v) {
        print("Decision Tree")
        print_stats(best.accuracies)
    }
    return(best.accuracies)
}