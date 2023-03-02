# select variables for specific purposes
# for admin - needs more info to set up PF
select_for_admin <- function(data){
  data %>%
    select(Service, wp, wp_lab, Hours, Rate, Cost)
}

# for pdf
select_for_pdf <- function(data){
  data %>%
    ungroup() %>%
    select(Service, Task = wp_lab, Description, Hours, Cost)
}
