#' @export
summarize_by_wp <- function(data){
  data %>%
    group_by(Service, wp, wp_lab) %>%
    summarize(Description = paste(desc, collapse = ", "),
              Hours = sum(Hours),
              Rate = mean(Rate),
              Cost = sum(Cost),
    )
}


#' @export
summarize_by_div <- function(data){
  data %>%
    left_join(divnames) %>%
    group_by(div) %>%
    summarize(#Description = paste(desc, collapse = ", "),
      Hours = sum(Hours),
      # Rate = mean(Rate),
      Cost = sum(Cost),
    )
}
