#' convert the info object into the format output to excel for admins
#' @param info the info object - the output from \code{costing_info()}
#' @importFrom stringr str_detect str_remove
#' @export
info_to_dataframe <- function(info){

  tmp <- info |>
    as.data.frame() |>
    mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(everything()) |>
    mutate(name = case_when(
      name == "sponsor_fun" ~ "customer_function",
      name == "sponsor_responsible" ~ "customer_responsible",
      name == "sponsor" ~ "customer",
      TRUE ~ name))

  # make it wider for easier use by admin
  srcs <- tmp |>
    filter(str_detect(name, "_src$")) |>
    mutate(name = str_remove(name, "_src$")) |>
    rename(source = value)

  tmp <- tmp |>
    filter(!str_detect(name, "_src$")) |>
    left_join(srcs, by = c("name" = "name"))

  return(tmp)
}



