

specific_option <- function(metadata, data, var){
  reference <- data[[var]]
  singlechoice_opts(metadata %>%
                      filter(field_name == var)) %>%
    filter(val == reference) %>%
    pull(lab)
}
