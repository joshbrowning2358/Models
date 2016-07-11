#' Use naive bayes approach to estimate continuous variables
#' 
#' The model assumes that each variable in X has it's own independent effect on 
#' y.  The effects are additive if log=FALSE and multiplicative if log=TRUE.
#' 
#' The baseline prediction is the mean of y.  This prediction is updated for 
#' each variable in X.  For example, if level 1 of variable 1 has a mean 20% 
#' higher on average than the overall mean, this variable will multiply the 
#' estimate by 1.2.
#' 
#' @param X Data.table, where each column is a categorical variable.  This is
#'   the design matrix (prior to dummy-fying).
#' @param y Numeric vector of the target.
#' @param log Should the y values be logged in the model?
#'   
#' @return A model for use in predictNaiveBayesRegression.
#'   

fitNaiveBayesRegression <- function(X, y, log=FALSE){
    stopifnot(is(X, "data.table"))
    stopifnot(length(y) == nrow(X))
    
    if(log){
        y = log(y + 1)
    }
    
    # Fit the model
    globalMean = mean(y)
    vars = copy(colnames(X))
    X = copy(X)
    X$y_ = y
    model = NULL
    for(var in vars){
        mod = X[, mean(y_) - globalMean, by=var]
        mod[, variable := var]
        setnames(mod, var, "level")
        setnames(mod, "V1", "adjustment")
        model = rbind(model, mod)
    }
    model = list(model=model, log=log, globalMean=globalMean)
    
    return(model)
}

predictNaiveBayesRegression <- function(model, X, allowNewLevel=TRUE){
    X = copy(X)
    X[, id_ := 1:.N]
    X[, pred_ := model$globalMean]
    vars = model$model[, unique(variable)]
    for(var in vars){
        toMerge = model$model[variable == var, ]
        setnames(toMerge, "level", var)
        toMerge[, variable := NULL]
        X = merge(X, toMerge, by=var, all.x=TRUE)
        if(allowNewLevel){
            X[is.na(adjustment), adjustment := 0]
        } else {
            stopifnot(all(!is.na(X$adjustment)))
        }
        X[, pred_ := pred_ + adjustment]
        X[, adjustment := NULL]
    }
    pred = X[order(id_), pred_]
    if(model$log){
        pred = exp(pred) - 1
    }
    X[, c("id_", "pred_") := NULL]
    return(pred)
}