#' @rdname roc
#' @export
#' @importFrom magrittr %>%
#' #' @importFrom rlang .data
plot.roc <- function(x, interactive=TRUE, ...) {
    measures <- unclass(x)
    if(length(measures)>1) {
        data_names <- names(measures)
        measures <- dplyr::bind_rows(measures, .id="Dataset")
        if(!is.null(data_names)) {
            measures <- measures %>%
                dplyr::mutate(Dataset=factor(.data$Dataset, levels=data_names))
        }
    } else {
        measures <- measures[[1]]
    }
    graph <- measures %>%
        dplyr::mutate(
            Label=sprintf("Dataset: %s\nCut-off: %.4f\nSensitivity: %.4f\nSpecificity: %.4f",
                .data$Dataset, .data$Cutoff, .data$Sensitivity, .data$Specificity
            )
        ) %>%
        ggplot2::ggplot(
            ggplot2::aes(
                x=.data$`1-Specificity`, y=.data$Sensitivity,
                group=.data$Dataset, colour=.data$Dataset, text=.data$Label
            )
        ) +
        ggplot2::geom_segment(x=0, y=0, xend=1, yend=1, colour="black", lty=2) +
        ggplot2::geom_path(lwd=1) +
        ggplot2::scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1)) +
        ggplot2::scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1)) +
        ggplot2::scale_color_brewer(palette="Set1", direction=-1) +
        ggplot2::xlab("False positive rate (1-specificity)") +
        ggplot2::ylab("True positive rate (sensibility)")
    if(interactive) {
        graph <- plotly::ggplotly(graph, tooltip="Label")
    }
    return(graph)
}
