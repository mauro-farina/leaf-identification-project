source("utils.R")
source("tree.R")
source("randomForest.R")
source("SVM.R")

verbose = T

k = 5  # for k-Fold Cross-Validation

seed = sample(c(1:1000), 1)  # = 386 in project

print(paste("seed =", seed))
data = get_leaf_data(seed)
accuracies.tree = get_best_tree(data, k, verbose)
accuracies.rf = get_best_RF(data, k, verbose)
accuracies.svm.l = get_best_SVM_linear(data, k, verbose)
accuracies.svm.p = get_best_SVM_polynomial(data, k, verbose)
accuracies.svm.r = get_best_SVM_radial(data, k, verbose)


print("=================================")

cat("DT: mean =", mean(accuracies.tree), ", sd =", sd(accuracies.tree), "\n")
cat("RF: mean =", mean(accuracies.rf), ", sd =", sd(accuracies.rf), "\n")
cat("SVM_L: mean =", mean(accuracies.svm.l), ", sd =", sd(accuracies.svm.l), "\n")
cat("SVM_P: mean =", mean(accuracies.svm.p), ", sd =", sd(accuracies.svm.p), "\n")
cat("SVM_R: mean =", mean(accuracies.svm.r), ", sd =", sd(accuracies.svm.r), "\n")

df = data.frame(
    Category = rep(c("DT", "RF", "SVM-L", "SVM-R"), each = k),
    Values = c(accuracies.tree, accuracies.rf, accuracies.svm.l, accuracies.svm.r))

boxplot = ggplot(df, aes(x = Category, y = Values)) +
    geom_boxplot() +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7) +
    labs(
        x = "Model",
        y = "Accuracy")

print(boxplot)
