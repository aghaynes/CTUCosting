# load and save unit testing dataset
# record TEST, costing 1

devtools::load_all()

token <- Sys.getenv("CTUCosting_token")
record <- "TEST"
costing <- 1

meta <- get_metadata(token = token)
d <- get_data(record = record, costing = costing, token = token)

saveRDS(d, file = "inst/data/record1.rds")
saveRDS(meta, file = "inst/data/meta.rds")

