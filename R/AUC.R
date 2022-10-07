.AUC_by_table <- function(roc_table) {
    Se <- rev(roc_table$Sensitivity)
    Sp <- rev(roc_table$Specificity)
    n <- length(Se)
    AUC <- 0
    for(curr in 2:n) {
        prev <- curr-1L
        AUC <- AUC + ((Sp[prev]-Sp[curr])*(Se[prev]+Se[curr]))
    }
    AUC <- AUC/2
    return(AUC)
}

#' Area Under the Curve
#'
#' Calculate the area under the ROC curve for the class \code{roc}.
#' @param measures An object returned by the \code{roc} function.
#' @return A vector containing the AUCs for each slot of the \code{roc} table.
AUC <- function(measures) {
    return(sapply(measures, .AUC_by_table))
}

