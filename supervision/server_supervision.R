source("source_chunked.R", local = TRUE)
source_chunked_file(file.path("supervision", "source_parts"), "server_supervision", env = parent.frame())
