#' @export
summarize_by_wp <- function(data){
  data %>%
    group_by(Service, wp, wp_lab) %>%
    collapse::fsummarize(Description = paste(desc, collapse = ", "),
              Hours = sum(Hours * Units),
              Rate = mean(Rate),
              Cost = sum(Cost)
    )
}


#' @export
summarize_by_div <- function(data){
  data %>%
    left_join(divnames) %>%
    group_by(div) %>%
    collapse::fsummarize(#Description = paste(desc, collapse = ", "),
      Hours = sum(Hours * Units),
      # Rate = mean(Rate),
      Cost = sum(Cost)
    )
}
