#' extract external note fields
#' @param data list of dataframes from get_data
#' @importFrom dplyr select ends_with
#' @export
get_notes <- function(data){
  vals <- lapply(data, function(x){
    tmp <- x |> select(ends_with("notes"))
    unlist(tmp)
  })

  tmp <- vals[unlist(sapply(vals, function(x) length(x) > 0 && !is.na(x)))]
  tmp
}

#' concatenate external note fields with division/workpackage
#' @param notes list of notes from get_notes
#' @param header_sep separater between workpackage and note
#' @export
concat_notes <- function(notes,
                         header_sep = "\n\n",
                         collapse = "\n\n"){
  paste0("**", servicenames$Service[match(names(notes), servicenames$form)], "**",
        header_sep, notes, collapse = collapse)
}

# d <- get_data(2,1,token)
# n <- get_notes(d)
# concat_notes(n)
