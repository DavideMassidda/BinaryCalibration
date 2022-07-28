#' @importFrom magrittr %>%
#' @importFrom rlang .data
.get_measures_by_cutoff <- function(target, scores, cut_off) {
    confusion <- table(
        observed=target,
        predicted=factor(
            scores>=cut_off,
            levels=c(FALSE,TRUE)
        )
    )
    measures <- tibble::tibble(
            Cutoff = cut_off,
            TN = confusion[1,1], TP = confusion[2,2],
            FN = confusion[2,1], FP = confusion[1,2]
        ) %>%
        dplyr::mutate(
            Accuracy = sum(diag(confusion))/sum(confusion),
            Precision = .data$TP / (.data$TP + .data$FP),
            Sensitivity = .data$TP / (.data$TP + .data$FN),
            Specificity = .data$TN / (.data$TN + .data$FP),
            `1-Specificity` = 1-.data$Specificity,
            AUC = (.data$Sensitivity + .data$Specificity)/2,
            `F1-score` = 2 * (.data$Precision * .data$Sensitivity) / (.data$Precision + .data$Sensitivity)
        )
    return(measures)
}

#' @importFrom magrittr %>%
.get_scores <- function(data, model) {
    if(any(class(model) %in% "glm")) {
        scores <- model %>%
            stats::predict(newdata=data, type="response") %>%
            as.numeric()
    } else {
        stop("The model isn\'t fitted with glm()")
    }
    return(scores)
}

#' @importFrom magrittr %>%
.get_measures <- function(data, scores, target_name, cut_offs=NULL) {
    target <- data %>%
        dplyr::select(!!target_name) %>%
        dplyr::pull()
    if(is.null(cut_offs)) {
        cut_offs <- scores %>%
            as.numeric() %>%
            unique() %>%
            sort() %>%
            c(., 1)
    }
    measures <- cut_offs %>%
        sapply(simplify=FALSE, USE.NAMES=FALSE,
        FUN=.get_measures_by_cutoff,
        target=target, scores=scores) %>%
        dplyr::bind_rows()
    return(measures)
}

#' Binary Calibration
#' @name roc
#' @importFrom magrittr %>%
#' @importFrom purrr map2
#' Given a generalized linear model fitted with the \code{glm} function,
#' it returns the performance measures for one or more data sets.
#' @param data_list List of data tables each one suitable from the argument \code{newdata} of
#' the \code{predict.glm} function.
#' @param model A model of class \code{glm}.
#' @param target_name Character specifying the name of the column of each data table
#' containing the target variable.
#' @param cut_off_from Character or integer indicating the location into the \code{data_list}
#' from which candidate cut-offs must be extracted.
#' @param x As object resulting from the function \code{roc}.
#' @param interactive Make the plot interactive using \code{ggplotly}.
#' @param ... Further arguments for the function \code{plot} (not implemented).
#' @return An object of class \code{roc} containing two data tables of class \code{tibble}.
#' @examples
#' set.seed(666)
#' n = 50
#' tab <- data.frame(x=rnorm(n))
#' tab$y <- tab$x*2+rnorm(n,sd=2)
#' tab$y <- as.integer(exp(tab$y)/(1+exp(tab$y))>0.5)
#' tab$set <- gl(2, n/2, labels=c("train","test"))
#' tab <- split(tab, f=tab$set)
#' model <- glm(y~x, family=binomial, data=tab$train)
#' measures <- roc(tab, model=model, target_name="y")
#' plot(measures)
roc <- function(data_list, model, target_name, cut_off_from=1) {
    scores <- lapply(data_list, .get_scores, model=model)
    cut_offs <- scores[[cut_off_from]] %>%
        as.numeric() %>%
        unique() %>%
        sort() %>%
        c(., 1)
    measures <- purrr::map2(data_list, scores, .f=.get_measures,
        target_name=target_name, cut_offs=cut_offs)
    class(measures) <- "roc"
    return(measures)
}
