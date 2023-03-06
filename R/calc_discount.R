

#' @importFrom vctrs vec_cast
#' @export
calc_discount <- function(workpackages, costing, discount_db){

  summ_discount <- workpackages %>%
    ungroup() %>%
    summarize(Hours = sum(Hours),
              Cost = sum(Cost)) %>%
    mutate(Service = "CTU",
           Description = "Total",
           discount = as.numeric(as.character(cut(Hours, c(seq(0,1000,100), Inf), seq(0,1000,100)/100))),
           discount = if_else(costing == 1, discount, vec_cast(discount_db, double())),
           discount_amount = Cost * (discount / 100),
           new_amount = Cost - discount_amount) %>%
    select(-costing)

}
