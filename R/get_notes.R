#' extract external note fields
#' @param data list of dataframes from get_data
#' @importFrom dplyr select ends_with any_of where
#' @export
get_notes <- function(data){

  vals <- lapply(data, function(x){
    if(nrow(x) > 0){
      tmp <- x |>
        select(any_of("dml_notes_standard"), ends_with("notes"), any_of("dmf_notes_2"))
      if(nrow(tmp) > 0) tmp <- tmp |> select(where(~any(!is.na(.x))))
      if(ncol(tmp) > 1) tmp <- paste(tmp, collapse = "\n\n")
      unlist(tmp)
    }
  })

  tmp <- vals[unlist(sapply(vals, function(x) length(x) > 0 && !is.na(x)))]
  tmp

}

#' concatenate external note fields with division/workpackage
#' @param notes list of notes from get_notes
#' @param header_sep separater between workpackage and note
#' @param collapse line break between notes
#' @export
concat_notes <- function(notes,
                         header_sep = "\n\n",
                         collapse = "\n\n"){

  if(length(notes) > 0){
    notes <- notes[!sapply(notes, function(x) x %in% c("NA", "NA\n\nNA"))]

    paste0("**", servicenames$Service[match(names(notes), servicenames$form)], "**",
          header_sep, notes, collapse = collapse)
  }
}

# d <- get_data(2,1,token)
# n <- get_notes(d)
# concat_notes(n)
