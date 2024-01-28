get_leaf_data = function(seed) {
    data = read.csv("../data/leaf.csv")
    data$Class = as.factor(data$Class)
    data = subset(data, select = -Specimen_Number)
    set.seed(seed)
    data = data[sample(c(1:nrow(data))),]
    return(data)
}

kfold = function(data, k, index) {
    n = nrow(data)  # total number of observations
    n_fold = floor(n/k)  # number of observations in each fold
    test_index_start = 1 + (index-1)*n_fold
    test_index_finish = index * n_fold
    data_test = data[test_index_start:test_index_finish,]
    data_learn = data[-(test_index_start:test_index_finish),]
    return (list(learn=data_learn, test=data_test))
}

get_accuracy = function(predictions, testdata) {
    length(which(predictions == testdata$Class))/nrow(testdata)
}

get_class_accuracy = function (confusion) {
    n = nrow(confusion)
    values = numeric(n)
    for (i in 1:n) {
        values[i] = confusion[i,i] / sum(confusion[i,])
    }
    acc_df = data.frame(
        Class = c(c(1:15), c(22:36)),
        Accuracy = values
    )
    return(acc_df)
}

print_stats = function(accuracies) {
    cat(accuracies, "\n")
    cat("accuracy mean =", mean(accuracies), 
        " sd =", sd(accuracies), 
        "median =",median(accuracies),
        "\n")
}