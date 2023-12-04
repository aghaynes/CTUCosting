#' Summarize by particular level
#'
#' summarize_by_wp summarizes by work package
#'
#' summarize_by_div summarizes by division
#'
#' @rdname summarize_by
#'
#' @param data A data frame with columns Service, wp, wp_lab, desc, Hours, Units, Rate, Cost
#'
#' @export
summarize_by_wp <- function(data){
  Service <- wp <- wp_lab <- desc <- Hours <- Units <- Rate <- Cost <- NULL
  data |>
    group_by(Service, wp, wp_lab) |>
    collapse::fsummarize(Description = paste(desc, collapse = ", "),
              Hours = sum(Hours * Units),
              Rate = mean(Rate),
              Cost = sum(Cost)
    )
}

#' @rdname summarize_by
#' @export
summarize_by_div <- function(data){
  div <- Hours <- Units <- Cost <- NULL
  data |>
    left_join(divnames) |>
    group_by(div) |>
    collapse::fsummarize(#Description = paste(desc, collapse = ", "),
      Hours = sum(Hours * Units),
      # Rate = mean(Rate),
      Cost = sum(Cost)
    )
}
