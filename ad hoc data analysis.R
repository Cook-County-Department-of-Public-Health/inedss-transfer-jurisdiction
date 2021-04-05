#transfers done in 2020
transfers2020 = as.Date("2020-01-01"):as.Date("2020-12-31") %>% as.Date(origin = "1970-01-01") %>%
  paste0( ., "transferred_log.txt")

transferred_files = list.files("daily-transferred-cases") %>%
  intersect(transfers2020)

already_transferred_logs = lapply(paste0("daily-transferred-cases/",transferred_files), function(x){
  vroom(x, col_names = F) %>%
    pull(X1)
}) %>%
  unlist()

length(already_transferred_logs)
#7814 logged over 97 days = ~80.6 per day