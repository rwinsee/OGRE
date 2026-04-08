source_chunked_file <- function(parts_dir, part_prefix, env = parent.frame()) {
  if (!dir.exists(parts_dir)) {
    stop(sprintf("Le dossier de morceaux est introuvable: %s", parts_dir))
  }

  pattern <- paste0("^", part_prefix, "\\.part[0-9]+\\.txt$")
  part_files <- list.files(parts_dir, pattern = pattern, full.names = TRUE)

  if (length(part_files) == 0) {
    stop(sprintf("Aucun morceau source trouve pour %s", part_prefix))
  }

  part_files <- sort(part_files)
  lines <- unlist(lapply(part_files, function(path) {
    readLines(path, warn = FALSE, encoding = "UTF-8")
  }), use.names = FALSE)

  tmp_path <- tempfile(fileext = ".R")
  on.exit(unlink(tmp_path), add = TRUE)
  writeLines(lines, tmp_path, useBytes = TRUE)
  sys.source(tmp_path, envir = env)
}
