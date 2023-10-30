

token <- Sys.getenv("CTUCosting_token")
record <- "TEST_DLF"
d <- get_data(record, 1, token)

notes <- get_notes(d)

concat_notes(notes)
