require(e1071)
source("utils.R")

params.cost = c(1, 10, 100)

evaluate_performance = function(data, k, kernel, cost, gamma=1, degree=1) {
    n = length(unique(data$Class))
    confusion.sum = matrix(0, nrow = n, ncol = n)
    accuracies = numeric(k)
    for (i in 1:k) {
        data_split = kfold(data, k, i)
        d.learn = data_split$learn
        d.test = data_split$test
        svm.model = svm(Class~., 
                        d.learn, 
                        kernel=kernel,
                        cost=cost,
                        gamma=gamma,
                        degree=degree,
                        coef0 = 1)
        svm.predictions = predict(svm.model, d.test, type="class")
        confusion.sum = confusion.sum + table(d.test$Class, svm.predictions)
        accuracies[i] = length(which(svm.predictions == d.test$Class))/nrow(d.test)
    }
    return (list(accuracies=accuracies,confusion=confusion.sum))
}

get_best_SVM_linear = function(d, k, v) {
    best.linear.accuracy = 0
    best.linear.confusion = 0
    
    for (p.cost in params.cost) {
        eval = evaluate_performance(data=d, k=k, kernel="linear", cost=p.cost)
        accuracy = eval$accuracies
        if (mean(accuracy) > mean(best.linear.accuracy)) {
            best.linear.accuracy = accuracy
            best.linear.cost = p.cost
            best.linear.confusion = eval$confusion
        }
    }
    if (v) {
        print("Linear SVM")
        cat("tuned parameter cost=", best.linear.cost, "\n")
        print_stats(best.linear.accuracy)
        print(best.linear.confusion)
        print(get_class_accuracy(best.linear.confusion))
    }
    return (best.linear.accuracy)
}


get_best_SVM_polynomial = function(d, k, v) {
    params.degree = c(1, 2, 3)
    params.polynomial = expand.grid(cost = params.cost, degree = params.degree)
    
    best.polynomial.accuracy = 0
    
    for (i in 1:nrow(params.polynomial)) {
        p.cost = params.polynomial$cost[i]
        p.degree = params.polynomial$degree[i]
        accuracy = evaluate_performance(data=d, k=k, kernel="polynomial", cost=p.cost, degree=p.degree)$accuracies
        if (mean(accuracy) > mean(best.polynomial.accuracy)) {
            best.polynomial.accuracy = accuracy
            best.polynomial.params = c(cost=p.cost, degree=p.degree)
        }
    }
    if (v) {
        print("Polynomial SVM: ")
        print(best.polynomial.params)
        print_stats(best.polynomial.accuracy)
    }
    return (best.polynomial.accuracy)
}


# ==== Radial Basis Kernel ====

get_best_SVM_radial = function(d, k, v) {
    params.gamma = c(0.01, 0.05, 0.1)
    params.radial = expand.grid(cost = params.cost, gamma = params.gamma)
    
    best.radial.accuracy = 0
    best.radial.confusion = 0
    
    for (i in 1:nrow(params.radial)) {
        p.cost = params.radial$cost[i]
        p.gamma = params.radial$gamma[i]
        eval = evaluate_performance(data=d, k=k, kernel="radial", cost=p.cost, gamma=p.gamma)
        accuracy = eval$accuracies
        if (mean(accuracy) > mean(best.radial.accuracy)) {
            best.radial.accuracy = accuracy
            best.radial.params = c(cost=p.cost, gamma=p.gamma)
            best.radial.confusion = eval$confusion
        }
    }
    if (v) {
        print("Radial SVM: ")
        print(best.radial.params)
        print_stats(best.radial.accuracy)
        print(best.radial.confusion)
        print(get_class_accuracy(best.radial.confusion))
    }
    return (best.radial.accuracy)
}