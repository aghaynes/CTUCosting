
# create SNF table for entering proportions of hours per year

create_snf_proportions_table <- function(wp, years){

  wp <- wp |>
    left_join(snf_division_lkup |>
                mutate(wp = sprintf("%05.1f", wp)), by = "wp") |>
    summarize(.by = snf_section,
              Hours = sum(Hours),
              Cost = sum(Cost))

    nrow <- length(wp$snf_section)
    ncol <- years + 1

    df <- as.data.frame(matrix(rep(0, ncol * nrow), nrow = nrow, ncol = ncol))
    names(df) <- c(paste("Year", 1 : years), "Row sum")
    rownames(df) <- wp$snf_section

    df

}
