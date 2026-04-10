library(shiny)
library(DT)
library(dplyr)

has_plotly <- requireNamespace("plotly", quietly = TRUE)

families_template <- function() {
  data.frame(
    id_famille = character(),
    idep_agent = character(),
    id_proposition_source = character(),
    idep_agent_validation = character(),
    date_validation = character(),
    code_ogr_parent = character(),
    code_rome_parent = character(),
    libelle_parent = character(),
    codes_ogr_enfants = character(),
    libelles_enfants = character(),
    nb_enfants = integer(),
    date_creation = character(),
    fichier_stockage = character(),
    stringsAsFactors = FALSE
  )
}

proposals_template <- function() {
  data.frame(
    id_proposition = character(),
    id_lignee = character(),
    id_proposition_source = character(),
    fichier_source = character(),
    phase_proposition = character(),
    statut_proposition = character(),
    type_operation = character(),
    idep_agent_edition = character(),
    horodatage_edition = character(),
    commentaire_edition = character(),
    code_ogr_parent_edition = character(),
    code_rome_parent_edition = character(),
    libelle_parent_edition = character(),
    codes_ogr_enfants_edition = character(),
    libelles_enfants_edition = character(),
    nb_enfants_edition = integer(),
    base_famille_id = character(),
    base_codes_ogr_enfants = character(),
    delta_enfants_ajoutes_edition = character(),
    delta_enfants_retires_edition = character(),
    idep_agent_supervision = character(),
    horodatage_prise_en_charge_supervision = character(),
    horodatage_decision_supervision = character(),
    decision_supervision = character(),
    commentaire_supervision = character(),
    code_ogr_parent_supervision = character(),
    code_rome_parent_supervision = character(),
    libelle_parent_supervision = character(),
    codes_ogr_enfants_supervision = character(),
    libelles_enfants_supervision = character(),
    nb_enfants_supervision = integer(),
    delta_enfants_ajoutes_supervision = character(),
    delta_enfants_retires_supervision = character(),
    idep_agent_validation = character(),
    horodatage_prise_en_charge_validation = character(),
    horodatage_decision_validation = character(),
    decision_validation = character(),
    commentaire_validation = character(),
    code_ogr_parent_validation = character(),
    code_rome_parent_validation = character(),
    libelle_parent_validation = character(),
    codes_ogr_enfants_validation = character(),
    libelles_enfants_validation = character(),
    nb_enfants_validation = integer(),
    id_famille_stock_publiee = character(),
    code_ogr_parent = character(),
    code_rome_parent = character(),
    libelle_parent = character(),
    codes_ogr_enfants = character(),
    libelles_enfants = character(),
    nb_enfants = integer(),
    fichier_stockage = character(),
    stringsAsFactors = FALSE
  )
}

reference_template <- function() {
  data.frame(
    code_ogr = character(),
    code_rome = character(),
    libelle_rome = character(),
    libelle_grand_domaine = character(),
    libelle_domaine_professionnel = character(),
    classification = character(),
    code_rome_parent = character(),
    libelle_appellation_long = character(),
    libelle_appellation_court = character(),
    libelle_appellation_metier = character(),
    stringsAsFactors = FALSE
  )
}

now_chr <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

timestamp_file_chr <- function(x = Sys.time()) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y%m%d_%H%M%S"))
  }
  
  parsed <- suppressWarnings(as.POSIXct(x, tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S"))
  if (is.na(parsed)) {
    parsed <- Sys.time()
  }
  
  format(parsed, "%Y%m%d_%H%M%S")
}

normalize_reference <- function(df) {
  df[] <- lapply(df, as.character)
  df
}

normalize_families <- function(df) {
  if (nrow(df) == 0) {
    return(families_template())
  }
  
  char_cols <- setdiff(names(df), "nb_enfants")
  df[char_cols] <- lapply(df[char_cols], as.character)
  df$nb_enfants <- suppressWarnings(as.integer(df$nb_enfants))
  df$nb_enfants[is.na(df$nb_enfants)] <- 0L
  df
}

normalize_proposals <- function(df) {
  if (nrow(df) == 0) {
    return(proposals_template())
  }
  
  int_cols <- c("nb_enfants_edition", "nb_enfants_supervision", "nb_enfants_validation", "nb_enfants")
  char_cols <- setdiff(names(df), int_cols)
  df[char_cols] <- lapply(df[char_cols], as.character)
  for (col in int_cols) {
    df[[col]] <- suppressWarnings(as.integer(df[[col]]))
    df[[col]][is.na(df[[col]])] <- 0L
  }
  df
}

normalize_edep_reference <- function(df) {
  if (nrow(df) == 0) {
    return(edep_reference_template())
  }
  
  int_cols <- intersect(c("nb_enfants_edep", "nb_parents_amont_edep", "nb_familles_parent_edep"), names(df))
  char_cols <- setdiff(names(df), int_cols)
  df[char_cols] <- lapply(df[char_cols], as.character)
  for (col in int_cols) {
    df[[col]] <- suppressWarnings(as.integer(df[[col]]))
    df[[col]][is.na(df[[col]])] <- 0L
  }
  df
}

normalize_edep_links <- function(df) {
  if (nrow(df) == 0) {
    return(edep_links_template())
  }
  
  int_cols <- c("rang_enfant", "nb_enfants_famille")
  char_cols <- setdiff(names(df), int_cols)
  df[char_cols] <- lapply(df[char_cols], as.character)
  for (col in int_cols) {
    df[[col]] <- suppressWarnings(as.integer(df[[col]]))
    df[[col]][is.na(df[[col]])] <- 0L
  }
  df
}

first_non_empty <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  
  if (length(values) == 0) {
    return("")
  }
  
  values[1]
}

first_non_na_int <- function(...) {
  values <- suppressWarnings(as.integer(unlist(list(...), use.names = FALSE)))
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(0L)
  }
  
  values[1]
}

split_pipe_values <- function(x) {
  x <- as.character(x)[1]
  
  if (length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(character(0))
  }
  
  parts <- strsplit(x, " \\| ")[[1]]
  parts[nzchar(parts)]
}

build_reference_choices <- function(df) {
  df <- df %>%
    distinct(code_ogr, .keep_all = TRUE) %>%
    filter(nzchar(code_ogr), nzchar(libelle_appellation_metier))
  
  setNames(
    df$code_ogr,
    paste0(df$libelle_appellation_metier, " [", df$code_ogr, "]")
  )
}

build_reference_label <- function(code_ogr, libelle) {
  paste0(libelle, " [", code_ogr, "]")
}

sanitize_filename_token <- function(x) {
  x <- toupper(trimws(as.character(x)[1]))
  x <- gsub("[^A-Z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  
  if (!nzchar(x)) {
    return("NA")
  }
  
  x
}

family_storage_dir <- function() {
  file.path("data", "familles", "stock")
}

legacy_families_path <- function() {
  file.path("data", "familles", "familles.csv")
}

legacy_family_storage_dir <- function() {
  file.path("data", "familles", "propositions")
}

proposal_storage_dir <- function(phase = "edition") {
  file.path("data", "propositions", phase)
}

workflow_proposal_dir <- function(phase, statut) {
  file.path("data", "workflow", phase, statut)
}

edep_reference_path <- function() {
  file.path("data", "referentiel", "referentiel_rome_edep.csv")
}

edep_links_path <- function() {
  file.path("data", "referentiel", "liens_familles_edep.csv")
}

proposal_states_for_scope <- function(scope = "edition") {
  switch(
    scope,
    edition = data.frame(
      phase = c("supervision", "supervision", "supervision", "validation", "validation", "validation", "validation"),
      statut = c("a_superviser", "en_cours", "rejetes", "a_valider", "en_cours", "en_attente", "rejetes"),
      stringsAsFactors = FALSE
    ),
    supervision = data.frame(
      phase = c("supervision", "supervision", "supervision"),
      statut = c("a_superviser", "en_cours", "rejetes"),
      stringsAsFactors = FALSE
    ),
    validation = data.frame(
      phase = c("validation", "validation", "validation", "validation", "validation"),
      statut = c("a_valider", "en_cours", "en_attente", "rejetes", "valides"),
      stringsAsFactors = FALSE
    ),
    all = data.frame(
      phase = c("supervision", "supervision", "supervision", "validation", "validation", "validation", "validation", "validation"),
      statut = c("a_superviser", "en_cours", "rejetes", "a_valider", "en_cours", "en_attente", "rejetes", "valides"),
      stringsAsFactors = FALSE
    ),
    data.frame(phase = scope, statut = "a_superviser", stringsAsFactors = FALSE)
  )
}

active_workflow_proposal_states <- function() {
  proposal_states_for_scope("edition") %>%
    filter(statut != "rejetes")
}

edep_links_template <- function() {
  data.frame(
    id_famille = character(),
    id_proposition_source = character(),
    date_validation = character(),
    idep_agent_validation = character(),
    code_ogr_parent = character(),
    code_rome_parent = character(),
    libelle_parent = character(),
    code_ogr_enfant = character(),
    libelle_enfant = character(),
    rang_enfant = integer(),
    nb_enfants_famille = integer(),
    stringsAsFactors = FALSE
  )
}

edep_reference_template <- function() {
  data.frame(
    code_ogr = character(),
    code_rome = character(),
    libelle_rome = character(),
    libelle_grand_domaine = character(),
    libelle_domaine_professionnel = character(),
    classification = character(),
    code_rome_parent = character(),
    libelle_appellation_long = character(),
    libelle_appellation_court = character(),
    libelle_appellation_metier = character(),
    a_famille_edep = character(),
    est_enfant_edep = character(),
    role_edep = character(),
    id_famille_edep = character(),
    id_proposition_source_edep = character(),
    date_validation_edep = character(),
    idep_agent_validation_edep = character(),
    nb_familles_parent_edep = integer(),
    nb_enfants_edep = integer(),
    codes_enfants_edep = character(),
    libelles_enfants_edep = character(),
    nb_parents_amont_edep = integer(),
    codes_parents_amont_edep = character(),
    libelles_parents_amont_edep = character(),
    stringsAsFactors = FALSE
  )
}

list_family_files <- function() {
  dir.create(family_storage_dir(), recursive = TRUE, showWarnings = FALSE)
  list.files(family_storage_dir(), pattern = "\\.csv$", full.names = TRUE)
}

list_proposal_files <- function(scope = "edition", states = NULL) {
  if (is.null(states)) {
    states <- proposal_states_for_scope(scope)
  }
  
  files <- unlist(lapply(seq_len(nrow(states)), function(i) {
    dir_path <- workflow_proposal_dir(states$phase[i], states$statut[i])
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  }), use.names = FALSE)
  
  unique(files)
}

build_family_file_stem <- function(idep_agent, code_ogr_parent, nb_enfants, created_at = Sys.time()) {
  paste(
    sanitize_filename_token(idep_agent),
    "famille",
    "rome",
    sanitize_filename_token(code_ogr_parent),
    as.integer(nb_enfants),
    timestamp_file_chr(created_at),
    sep = "_"
  )
}

build_proposal_file_stem <- function(idep_agent, code_ogr_parent, nb_enfants, phase = "edition", created_at = Sys.time()) {
  paste(
    sanitize_filename_token(idep_agent),
    "famille",
    "rome",
    sanitize_filename_token(code_ogr_parent),
    as.integer(nb_enfants),
    timestamp_file_chr(created_at),
    sanitize_filename_token(phase),
    sep = "_"
  )
}

next_family_file_path <- function(file_stem, exclude_path = NULL) {
  candidate <- file.path(family_storage_dir(), paste0(file_stem, ".csv"))
  normalized_exclude <- if (is.null(exclude_path)) "" else normalizePath(exclude_path, winslash = "/", mustWork = FALSE)
  
  if (!file.exists(candidate) || normalizePath(candidate, winslash = "/", mustWork = FALSE) == normalized_exclude) {
    return(candidate)
  }
  
  suffix <- 2L
  repeat {
    candidate <- file.path(family_storage_dir(), paste0(file_stem, "_", sprintf("%02d", suffix), ".csv"))
    if (!file.exists(candidate) || normalizePath(candidate, winslash = "/", mustWork = FALSE) == normalized_exclude) {
      return(candidate)
    }
    suffix <- suffix + 1L
  }
}

next_proposal_file_path <- function(file_stem, phase = "edition", exclude_path = NULL) {
  candidate <- file.path(proposal_storage_dir(phase), paste0(file_stem, ".csv"))
  normalized_exclude <- if (is.null(exclude_path)) "" else normalizePath(exclude_path, winslash = "/", mustWork = FALSE)
  
  if (!file.exists(candidate) || normalizePath(candidate, winslash = "/", mustWork = FALSE) == normalized_exclude) {
    return(candidate)
  }
  
  suffix <- 2L
  repeat {
    candidate <- file.path(proposal_storage_dir(phase), paste0(file_stem, "_", sprintf("%02d", suffix), ".csv"))
    if (!file.exists(candidate) || normalizePath(candidate, winslash = "/", mustWork = FALSE) == normalized_exclude) {
      return(candidate)
    }
    suffix <- suffix + 1L
  }
}

next_workflow_proposal_path <- function(file_stem, phase, statut, exclude_path = NULL) {
  candidate <- file.path(workflow_proposal_dir(phase, statut), paste0(file_stem, ".csv"))
  normalized_exclude <- if (is.null(exclude_path)) "" else normalizePath(exclude_path, winslash = "/", mustWork = FALSE)
  
  if (!file.exists(candidate) || normalizePath(candidate, winslash = "/", mustWork = FALSE) == normalized_exclude) {
    return(candidate)
  }
  
  suffix <- 2L
  repeat {
    candidate <- file.path(workflow_proposal_dir(phase, statut), paste0(file_stem, "_", sprintf("%02d", suffix), ".csv"))
    if (!file.exists(candidate) || normalizePath(candidate, winslash = "/", mustWork = FALSE) == normalized_exclude) {
      return(candidate)
    }
    suffix <- suffix + 1L
  }
}

normalize_family_row <- function(df) {
  missing_cols <- setdiff(names(families_template()), names(df))
  for (col in missing_cols) {
    df[[col]] <- ""
  }
  
  df <- df[, names(families_template()), drop = FALSE]
  normalize_families(df)
}

normalize_proposal_row <- function(df) {
  if ("delta_enfants_ajoutes" %in% names(df) && !("delta_enfants_ajoutes_edition" %in% names(df))) {
    df$delta_enfants_ajoutes_edition <- df$delta_enfants_ajoutes
  }
  if ("delta_enfants_retires" %in% names(df) && !("delta_enfants_retires_edition" %in% names(df))) {
    df$delta_enfants_retires_edition <- df$delta_enfants_retires
  }
  
  source_to_edition_map <- c(
    code_ogr_parent = "code_ogr_parent_edition",
    code_rome_parent = "code_rome_parent_edition",
    libelle_parent = "libelle_parent_edition",
    codes_ogr_enfants = "codes_ogr_enfants_edition",
    libelles_enfants = "libelles_enfants_edition",
    nb_enfants = "nb_enfants_edition"
  )
  
  for (source_col in names(source_to_edition_map)) {
    target_col <- source_to_edition_map[[source_col]]
    if ((!(target_col %in% names(df))) || all(is.na(df[[target_col]]) | df[[target_col]] == "")) {
      if (source_col %in% names(df)) {
        df[[target_col]] <- df[[source_col]]
      }
    }
  }
  
  missing_cols <- setdiff(names(proposals_template()), names(df))
  for (col in missing_cols) {
    df[[col]] <- ""
  }
  
  df <- df[, names(proposals_template()), drop = FALSE]
  df <- normalize_proposals(df)
  
  if (!nzchar(df$id_lignee[1])) {
    df$id_lignee[1] <- if (nzchar(df$id_proposition[1])) df$id_proposition[1] else ""
  }
  
  if (!nzchar(df$phase_proposition[1]) || identical(df$phase_proposition[1], "edition")) {
    df$phase_proposition[1] <- "supervision"
  }
  
  if (!nzchar(df$statut_proposition[1])) {
    df$statut_proposition[1] <- "a_superviser"
  }
  
  df$code_ogr_parent[1] <- first_non_empty(
    df$code_ogr_parent_validation[1],
    df$code_ogr_parent_supervision[1],
    df$code_ogr_parent[1],
    df$code_ogr_parent_edition[1]
  )
  df$code_rome_parent[1] <- first_non_empty(
    df$code_rome_parent_validation[1],
    df$code_rome_parent_supervision[1],
    df$code_rome_parent[1],
    df$code_rome_parent_edition[1]
  )
  df$libelle_parent[1] <- first_non_empty(
    df$libelle_parent_validation[1],
    df$libelle_parent_supervision[1],
    df$libelle_parent[1],
    df$libelle_parent_edition[1]
  )
  df$codes_ogr_enfants[1] <- first_non_empty(
    df$codes_ogr_enfants_validation[1],
    df$codes_ogr_enfants_supervision[1],
    df$codes_ogr_enfants[1],
    df$codes_ogr_enfants_edition[1]
  )
  df$libelles_enfants[1] <- first_non_empty(
    df$libelles_enfants_validation[1],
    df$libelles_enfants_supervision[1],
    df$libelles_enfants[1],
    df$libelles_enfants_edition[1]
  )
  df$nb_enfants[1] <- first_non_na_int(
    df$nb_enfants_validation[1],
    df$nb_enfants_supervision[1],
    df$nb_enfants[1],
    df$nb_enfants_edition[1]
  )
  
  df
}

read_family_file <- function(path_family) {
  df <- read.csv(path_family, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  df <- normalize_family_row(df)
  
  if (!nzchar(df$fichier_stockage[1])) {
    df$fichier_stockage[1] <- basename(path_family)
  }
  
  df
}

read_proposal_file <- function(path_proposal) {
  df <- read.csv(path_proposal, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  df <- normalize_proposal_row(df)
  
  if (!nzchar(df$fichier_stockage[1])) {
    df$fichier_stockage[1] <- basename(path_proposal)
  }
  
  df$path_fichier <- normalizePath(path_proposal, winslash = "/", mustWork = FALSE)
  df$dossier_proposition <- dirname(path_proposal)
  
  df
}

write_family_file <- function(df) {
  path_family <- file.path(family_storage_dir(), df$fichier_stockage[1])
  write.csv(df, path_family, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path_family)
}

build_edep_links_from_families <- function(families) {
  if (nrow(families) == 0) {
    return(edep_links_template())
  }
  
  rows <- lapply(seq_len(nrow(families)), function(i) {
    child_codes <- split_pipe_values(families$codes_ogr_enfants[i])
    child_labels <- split_pipe_values(families$libelles_enfants[i])
    max_len <- max(length(child_codes), length(child_labels), 1L)
    length(child_codes) <- max_len
    length(child_labels) <- max_len
    child_codes[is.na(child_codes)] <- ""
    child_labels[is.na(child_labels)] <- ""
    
    data.frame(
      id_famille = families$id_famille[i],
      id_proposition_source = families$id_proposition_source[i],
      date_validation = families$date_validation[i],
      idep_agent_validation = families$idep_agent_validation[i],
      code_ogr_parent = families$code_ogr_parent[i],
      code_rome_parent = families$code_rome_parent[i],
      libelle_parent = families$libelle_parent[i],
      code_ogr_enfant = child_codes,
      libelle_enfant = child_labels,
      rang_enfant = seq_len(max_len),
      nb_enfants_famille = families$nb_enfants[i],
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(rows) %>%
    normalize_edep_links()
}

build_edep_reference_from_families <- function(families, source_ref = NULL) {
  if (is.null(source_ref)) {
    source_ref <- get0("ref_ogr", ifnotfound = reference_template()[0, , drop = FALSE], inherits = TRUE)
  }
  
  source_ref <- source_ref %>%
    distinct(code_ogr, .keep_all = TRUE) %>%
    filter(nzchar(code_ogr)) %>%
    mutate(
      libelle_appellation_metier = dplyr::coalesce(
        ifelse(nzchar(libelle_appellation_metier), libelle_appellation_metier, NA_character_),
        ifelse(nzchar(libelle_appellation_long), libelle_appellation_long, NA_character_),
        ifelse(nzchar(libelle_appellation_court), libelle_appellation_court, NA_character_)
      )
    )
  
  if (nrow(source_ref) == 0) {
    return(edep_reference_template())
  }
  
  if (nrow(families) == 0) {
    empty_ref <- source_ref %>%
      mutate(
        a_famille_edep = "NON",
        est_enfant_edep = "NON",
        role_edep = "SANS_FAMILLE",
        id_famille_edep = "",
        id_proposition_source_edep = "",
        date_validation_edep = "",
        idep_agent_validation_edep = "",
        nb_familles_parent_edep = 0L,
        nb_enfants_edep = 0L,
        codes_enfants_edep = "",
        libelles_enfants_edep = "",
        nb_parents_amont_edep = 0L,
        codes_parents_amont_edep = "",
        libelles_parents_amont_edep = ""
      )
    
    return(normalize_edep_reference(empty_ref))
  }
  
  links <- build_edep_links_from_families(families)
  parent_codes <- unique(families$code_ogr_parent[nzchar(families$code_ogr_parent)])
  child_codes <- unique(links$code_ogr_enfant[nzchar(links$code_ogr_enfant)])
  child_only_codes <- setdiff(child_codes, parent_codes)
  
  parent_summary <- links %>%
    filter(nzchar(code_ogr_parent)) %>%
    group_by(code_ogr_parent) %>%
    summarise(
      id_famille_edep = paste(sort(unique(id_famille[nzchar(id_famille)])), collapse = " | "),
      id_proposition_source_edep = paste(sort(unique(id_proposition_source[nzchar(id_proposition_source)])), collapse = " | "),
      date_validation_edep = paste(sort(unique(date_validation[nzchar(date_validation)])), collapse = " | "),
      idep_agent_validation_edep = paste(sort(unique(idep_agent_validation[nzchar(idep_agent_validation)])), collapse = " | "),
      nb_familles_parent_edep = n_distinct(id_famille[nzchar(id_famille)]),
      nb_enfants_edep = length(unique(code_ogr_enfant[nzchar(code_ogr_enfant)])),
      codes_enfants_edep = paste(sort(unique(code_ogr_enfant[nzchar(code_ogr_enfant)])), collapse = " | "),
      libelles_enfants_edep = paste(sort(unique(libelle_enfant[nzchar(libelle_enfant)])), collapse = " | "),
      .groups = "drop"
    ) %>%
    rename(code_ogr = code_ogr_parent)
  
  child_summary <- links %>%
    filter(nzchar(code_ogr_enfant), nzchar(code_ogr_parent)) %>%
    group_by(code_ogr_enfant) %>%
    summarise(
      nb_parents_amont_edep = n_distinct(code_ogr_parent),
      codes_parents_amont_edep = paste(sort(unique(code_ogr_parent)), collapse = " | "),
      libelles_parents_amont_edep = paste(sort(unique(libelle_parent[nzchar(libelle_parent)])), collapse = " | "),
      .groups = "drop"
    ) %>%
    rename(code_ogr = code_ogr_enfant)
  
  source_ref %>%
    filter(!(code_ogr %in% child_only_codes)) %>%
    left_join(parent_summary, by = "code_ogr") %>%
    left_join(child_summary, by = "code_ogr") %>%
    mutate(
      a_famille_edep = ifelse(code_ogr %in% parent_codes, "OUI", "NON"),
      est_enfant_edep = ifelse(code_ogr %in% child_codes, "OUI", "NON"),
      role_edep = dplyr::case_when(
        code_ogr %in% parent_codes & code_ogr %in% child_codes ~ "PARENT_ET_ENFANT",
        code_ogr %in% parent_codes ~ "PARENT",
        code_ogr %in% child_codes ~ "ENFANT",
        TRUE ~ "SANS_FAMILLE"
      ),
      id_famille_edep = ifelse(is.na(id_famille_edep), "", id_famille_edep),
      id_proposition_source_edep = ifelse(is.na(id_proposition_source_edep), "", id_proposition_source_edep),
      date_validation_edep = ifelse(is.na(date_validation_edep), "", date_validation_edep),
      idep_agent_validation_edep = ifelse(is.na(idep_agent_validation_edep), "", idep_agent_validation_edep),
      nb_familles_parent_edep = ifelse(is.na(nb_familles_parent_edep), 0L, nb_familles_parent_edep),
      nb_enfants_edep = ifelse(is.na(nb_enfants_edep), 0L, nb_enfants_edep),
      codes_enfants_edep = ifelse(is.na(codes_enfants_edep), "", codes_enfants_edep),
      libelles_enfants_edep = ifelse(is.na(libelles_enfants_edep), "", libelles_enfants_edep),
      nb_parents_amont_edep = ifelse(is.na(nb_parents_amont_edep), 0L, nb_parents_amont_edep),
      codes_parents_amont_edep = ifelse(is.na(codes_parents_amont_edep), "", codes_parents_amont_edep),
      libelles_parents_amont_edep = ifelse(is.na(libelles_parents_amont_edep), "", libelles_parents_amont_edep)
    ) %>%
    arrange(libelle_appellation_metier, code_ogr) %>%
    normalize_edep_reference()
}

write_edep_reference <- function(df) {
  df <- normalize_edep_reference(df)
  write.csv(df, edep_reference_path(), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(edep_reference_path())
}

write_edep_links <- function(df) {
  df <- normalize_edep_links(df)
  write.csv(df, edep_links_path(), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(edep_links_path())
}

refresh_edep_reference <- function() {
  families <- load_families()
  write_edep_links(build_edep_links_from_families(families))
  write_edep_reference(build_edep_reference_from_families(families))
}

load_edep_reference <- function() {
  path_ref <- edep_reference_path()
  
  if (!file.exists(path_ref)) {
    refresh_edep_reference()
  }
  
  if (!file.exists(path_ref)) {
    return(edep_reference_template())
  }
  
  normalize_edep_reference(read.csv(path_ref, stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
}

load_edep_links <- function() {
  path_ref <- edep_links_path()
  
  if (!file.exists(path_ref)) {
    refresh_edep_reference()
  }
  
  if (!file.exists(path_ref)) {
    return(edep_links_template())
  }
  
  normalize_edep_links(read.csv(path_ref, stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
}

write_proposal_file <- function(df) {
  df <- normalize_proposal_row(df)
  path_proposal <- file.path(workflow_proposal_dir(df$phase_proposition[1], df$statut_proposition[1]), df$fichier_stockage[1])
  dir.create(dirname(path_proposal), recursive = TRUE, showWarnings = FALSE)
  write.csv(df[, names(proposals_template()), drop = FALSE], path_proposal, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path_proposal)
}

infer_parent_rome_from_ogr <- function(code_ogr_parent, ref_df = NULL) {
  if (is.null(ref_df)) {
    ref_df <- get0("ref_ogr", ifnotfound = reference_template()[0, , drop = FALSE], inherits = TRUE)
  }
  
  match_idx <- match(code_ogr_parent, ref_df$code_ogr)
  if (is.na(match_idx)) {
    return("")
  }
  
  as.character(ref_df$code_rome[match_idx])
}

migrate_legacy_family_directory <- function() {
  old_dir <- legacy_family_storage_dir()
  new_dir <- family_storage_dir()
  
  if (!dir.exists(old_dir)) {
    return(invisible(NULL))
  }
  
  dir.create(new_dir, recursive = TRUE, showWarnings = FALSE)
  legacy_files <- list.files(old_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(legacy_files) == 0) {
    return(invisible(NULL))
  }
  
  for (path_family in legacy_files) {
    target_path <- file.path(new_dir, basename(path_family))
    
    if (!file.exists(target_path)) {
      ok <- file.rename(path_family, target_path)
      if (!ok) {
        file.copy(path_family, target_path, overwrite = FALSE)
      }
    }
  }
  
  invisible(TRUE)
}

ensure_family_storage_migration <- function(ref_df = NULL) {
  legacy_path <- legacy_families_path()
  if (!file.exists(legacy_path) || length(list_family_files()) > 0) {
    return(invisible(NULL))
  }
  
  legacy <- read.csv(legacy_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  if (nrow(legacy) == 0) {
    return(invisible(NULL))
  }
  
  legacy <- normalize_family_row(legacy)
  
  for (i in seq_len(nrow(legacy))) {
    row <- legacy[i, , drop = FALSE]
    row$idep_agent[1] <- if (nzchar(row$idep_agent[1])) row$idep_agent[1] else "DFEC5Z"
    row$code_rome_parent[1] <- if (nzchar(row$code_rome_parent[1])) {
      row$code_rome_parent[1]
    } else {
      infer_parent_rome_from_ogr(row$code_ogr_parent[1], ref_df = ref_df)
    }
    
    file_stem <- build_family_file_stem(
      idep_agent = row$idep_agent[1],
      code_ogr_parent = row$code_ogr_parent[1],
      nb_enfants = row$nb_enfants[1],
      created_at = row$date_creation[1]
    )
    
    row$id_famille[1] <- file_stem
    row$fichier_stockage[1] <- basename(next_family_file_path(file_stem))
    write_family_file(row)
  }
  
  invisible(TRUE)
}

sync_family_storage_names <- function() {
  family_files <- list_family_files()
  if (length(family_files) == 0) {
    return(invisible(NULL))
  }
  
  for (path_family in family_files) {
    row <- read_family_file(path_family)
    row$idep_agent[1] <- if (nzchar(row$idep_agent[1])) row$idep_agent[1] else "DFEC5Z"
    
    file_stem <- build_family_file_stem(
      idep_agent = row$idep_agent[1],
      code_ogr_parent = row$code_ogr_parent[1],
      nb_enfants = row$nb_enfants[1],
      created_at = row$date_creation[1]
    )
    
    target_path <- next_family_file_path(file_stem, exclude_path = path_family)
    target_name <- basename(target_path)
    target_id <- tools::file_path_sans_ext(target_name)
    
    row$id_famille[1] <- target_id
    row$fichier_stockage[1] <- target_name
    
    current_path_norm <- normalizePath(path_family, winslash = "/", mustWork = FALSE)
    target_path_norm <- normalizePath(target_path, winslash = "/", mustWork = FALSE)
    
    if (current_path_norm != target_path_norm) {
      ok <- file.rename(path_family, target_path)
      if (!ok) {
        stop("Impossible de renommer un fichier de proposition vers le nouveau format.")
      }
    }
    
    write_family_file(row)
  }
  
  invisible(TRUE)
}

build_selection_recommendations <- function(df) {
  empty_reco <- list(
    selection_size = 0L,
    principal_count = 0L,
    synonym_count = 0L,
    candidate_parents = reference_template()[0, , drop = FALSE],
    suggested_parent = NULL,
    suggested_parent_code = "",
    suggested_parent_label = "",
    rome_parent_summary = data.frame(
      code_rome_parent = character(),
      nb_codes = integer(),
      stringsAsFactors = FALSE
    )
  )
  
  df <- df %>%
    distinct(code_ogr, .keep_all = TRUE) %>%
    filter(nzchar(code_ogr), nzchar(libelle_appellation_metier))
  
  if (nrow(df) == 0) {
    return(empty_reco)
  }
  
  principal_rows <- df %>%
    filter(classification == "PRINCIPALE")
  
  suggested_parent <- NULL
  if (nrow(principal_rows) == 1) {
    suggested_parent <- principal_rows[1, , drop = FALSE]
  } else if (nrow(df) == 1) {
    suggested_parent <- df[1, , drop = FALSE]
  }
  
  rome_parent_summary <- df %>%
    filter(nzchar(code_rome_parent)) %>%
    count(code_rome_parent, sort = TRUE, name = "nb_codes")
  
  list(
    selection_size = nrow(df),
    principal_count = nrow(principal_rows),
    synonym_count = sum(df$classification == "SYNONYME", na.rm = TRUE),
    candidate_parents = principal_rows,
    suggested_parent = suggested_parent,
    suggested_parent_code = if (is.null(suggested_parent)) "" else suggested_parent$code_ogr[1],
    suggested_parent_label = if (is.null(suggested_parent)) "" else build_reference_label(
      suggested_parent$code_ogr[1],
      suggested_parent$libelle_appellation_metier[1]
    ),
    rome_parent_summary = rome_parent_summary
  )
}

new_family_id <- function() {
  paste0("FAM_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sprintf("%04d", sample(1:9999, 1)))
}

ensure_storage <- function() {
  dir.create("data", showWarnings = FALSE)
  dir.create(file.path("data", "referentiel"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("data", "familles"), recursive = TRUE, showWarnings = FALSE)
  dir.create(family_storage_dir(), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("data", "propositions"), recursive = TRUE, showWarnings = FALSE)
  dir.create(proposal_storage_dir("edition"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("data", "workflow"), recursive = TRUE, showWarnings = FALSE)
  
  workflow_states <- proposal_states_for_scope("all")
  for (i in seq_len(nrow(workflow_states))) {
    dir.create(workflow_proposal_dir(workflow_states$phase[i], workflow_states$statut[i]), recursive = TRUE, showWarnings = FALSE)
  }
}

migrate_legacy_proposals_to_workflow <- function() {
  old_dir <- proposal_storage_dir("edition")
  
  if (!dir.exists(old_dir)) {
    return(invisible(NULL))
  }
  
  legacy_files <- list.files(old_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(legacy_files) == 0) {
    return(invisible(NULL))
  }
  
  for (path_proposal in legacy_files) {
    raw_df <- read.csv(path_proposal, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    row <- normalize_proposal_row(raw_df)
    
    if (!nzchar(row$fichier_stockage[1])) {
      row$fichier_stockage[1] <- basename(path_proposal)
    }
    
    row$phase_proposition[1] <- "supervision"
    row$statut_proposition[1] <- "a_superviser"
    write_proposal_file(row)
    unlink(path_proposal, force = TRUE)
  }
  
  invisible(TRUE)
}

load_rome_enrichment <- function() {
  path_enrich <- file.path("data", "referentiel", "cr_gd_dp_v4_utf8.csv")
  
  if (!file.exists(path_enrich)) {
    return(data.frame(
      code_rome = character(),
      libelle_rome_enrich = character(),
      libelle_grand_domaine_enrich = character(),
      libelle_domaine_professionnel_enrich = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  enrich <- read.csv(path_enrich, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  enrich[] <- lapply(enrich, as.character)
  
  enrich %>%
    transmute(
      code_rome = code_rome,
      libelle_rome_enrich = libelle_rome,
      libelle_grand_domaine_enrich = libelle_grand_domaine,
      libelle_domaine_professionnel_enrich = libelle_domaine_professionel
    ) %>%
    filter(nzchar(code_rome)) %>%
    distinct(code_rome, .keep_all = TRUE)
}

ensure_reference_file <- function() {
  path_ref <- file.path("data", "referentiel", "ref_ogr.csv")
  
  if (!file.exists(path_ref)) {
    ref_demo <- data.frame(
      code_ogr = c("15180", "18715", "200201", "300100", "300101"),
      libelle_appellation_metier = c(
        "Galeriste",
        "Galeriste art contemporain",
        "Galeriste conseil",
        "Commercant en art",
        "Vendeur en galerie"
      ),
      stringsAsFactors = FALSE
    )
    
    write.csv(ref_demo, path_ref, row.names = FALSE, na = "")
  }
  
  ref <- read.csv(path_ref, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  ref <- ref[, intersect(names(ref), names(reference_template())), drop = FALSE]
  
  missing_cols <- setdiff(names(reference_template()), names(ref))
  for (col in missing_cols) {
    ref[[col]] <- ""
  }
  
  ref <- ref[, names(reference_template()), drop = FALSE]
  ref <- normalize_reference(ref)
  
  enrich <- load_rome_enrichment()
  if (nrow(enrich) > 0) {
    ref <- ref %>%
      left_join(enrich, by = "code_rome") %>%
      mutate(
        libelle_rome = dplyr::coalesce(
          ifelse(nzchar(libelle_rome), libelle_rome, NA_character_),
          ifelse(nzchar(libelle_rome_enrich), libelle_rome_enrich, NA_character_)
        ),
        libelle_grand_domaine = dplyr::coalesce(
          ifelse(nzchar(libelle_grand_domaine), libelle_grand_domaine, NA_character_),
          ifelse(nzchar(libelle_grand_domaine_enrich), libelle_grand_domaine_enrich, NA_character_)
        ),
        libelle_domaine_professionnel = dplyr::coalesce(
          ifelse(nzchar(libelle_domaine_professionnel), libelle_domaine_professionnel, NA_character_),
          ifelse(nzchar(libelle_domaine_professionnel_enrich), libelle_domaine_professionnel_enrich, NA_character_)
        )
      ) %>%
      select(-libelle_rome_enrich, -libelle_grand_domaine_enrich, -libelle_domaine_professionnel_enrich)
  }
  
  ref$libelle_appellation_metier <- dplyr::coalesce(
    ifelse(nzchar(ref$libelle_appellation_metier), ref$libelle_appellation_metier, NA_character_),
    ifelse(nzchar(ref$libelle_appellation_long), ref$libelle_appellation_long, NA_character_),
    ifelse(nzchar(ref$libelle_appellation_court), ref$libelle_appellation_court, NA_character_)
  )
  
  ref %>%
    filter(nzchar(libelle_appellation_metier)) %>%
    distinct(code_ogr, .keep_all = TRUE) %>%
    arrange(libelle_appellation_metier, code_ogr)
}

load_families <- function() {
  migrate_legacy_family_directory()
  ensure_family_storage_migration(ref_df = get0("ref_ogr", ifnotfound = reference_template()[0, , drop = FALSE], inherits = TRUE))
  sync_family_storage_names()
  family_files <- list_family_files()
  
  if (length(family_files) == 0) {
    return(families_template())
  }
  
  bind_rows(lapply(family_files, read_family_file)) %>%
    normalize_families()
}

load_proposals <- function(phase = "edition", states = NULL) {
  migrate_legacy_proposals_to_workflow()
  proposal_files <- list_proposal_files(scope = phase, states = states)
  
  if (length(proposal_files) == 0) {
    return(proposals_template())
  }
  
  bind_rows(lapply(proposal_files, read_proposal_file)) %>%
    normalize_proposals() %>%
    arrange(desc(horodatage_edition), id_proposition)
}

load_active_workflow_proposals <- function() {
  load_proposals(states = active_workflow_proposal_states())
}

create_family <- function(idep_agent, code_ogr_parent, code_rome_parent, libelle_parent, codes_ogr_enfants, libelles_enfants, id_proposition_source = "", idep_agent_validation = "", date_validation = "") {
  created_at <- now_chr()
  file_stem <- build_family_file_stem(
    idep_agent = idep_agent,
    code_ogr_parent = code_ogr_parent,
    nb_enfants = length(libelles_enfants),
    created_at = created_at
  )
  file_path <- next_family_file_path(file_stem)
  new_row <- data.frame(
    id_famille = tools::file_path_sans_ext(basename(file_path)),
    idep_agent = as.character(idep_agent),
    id_proposition_source = as.character(id_proposition_source),
    idep_agent_validation = as.character(idep_agent_validation),
    date_validation = as.character(date_validation),
    code_ogr_parent = as.character(code_ogr_parent),
    code_rome_parent = as.character(code_rome_parent),
    libelle_parent = as.character(libelle_parent),
    codes_ogr_enfants = paste(as.character(codes_ogr_enfants), collapse = " | "),
    libelles_enfants = paste(as.character(libelles_enfants), collapse = " | "),
    nb_enfants = length(libelles_enfants),
    date_creation = created_at,
    fichier_stockage = basename(file_path),
    stringsAsFactors = FALSE
  )
  
  write_family_file(normalize_family_row(new_row))
  refresh_edep_reference()
  
  invisible(new_row$id_famille)
}

project_proposals_as_families <- function(proposals) {
  if (nrow(proposals) == 0) {
    return(families_template())
  }
  
  data.frame(
    id_famille = proposals$id_proposition,
    idep_agent = proposals$idep_agent_edition,
    code_ogr_parent = proposals$code_ogr_parent,
    code_rome_parent = proposals$code_rome_parent,
    libelle_parent = proposals$libelle_parent,
    codes_ogr_enfants = proposals$codes_ogr_enfants,
    libelles_enfants = proposals$libelles_enfants,
    nb_enfants = proposals$nb_enfants,
    date_creation = proposals$horodatage_edition,
    fichier_stockage = proposals$fichier_stockage,
    stringsAsFactors = FALSE
  ) %>%
    normalize_families()
}

find_base_family_for_draft <- function(draft, stock_families) {
  if (is.null(draft) || nrow(stock_families) == 0) {
    return(NULL)
  }
  
  matches <- stock_families %>%
    filter(code_ogr_parent == draft$code_ogr_parent)
  
  if (nrow(matches) == 1) {
    return(matches[1, , drop = FALSE])
  }
  
  NULL
}

build_child_delta <- function(new_child_codes, base_child_codes) {
  list(
    ajoutes = paste(setdiff(new_child_codes, base_child_codes), collapse = " | "),
    retires = paste(setdiff(base_child_codes, new_child_codes), collapse = " | ")
  )
}

create_proposal <- function(draft, stock_families, phase = "supervision", statut = "a_superviser", origin_phase = "edition") {
  created_at <- now_chr()
  base_family <- find_base_family_for_draft(draft, stock_families)
  base_child_codes <- if (is.null(base_family)) character(0) else split_pipe_values(base_family$codes_ogr_enfants[1])
  child_delta <- build_child_delta(draft$child_codes, base_child_codes)
  proposal_type <- if (is.null(base_family)) "creation" else "modification"
  
  file_stem <- build_proposal_file_stem(
    idep_agent = draft$idep_agent,
    code_ogr_parent = draft$code_ogr_parent,
    nb_enfants = length(draft$child_labels),
    phase = origin_phase,
    created_at = created_at
  )
  file_path <- next_workflow_proposal_path(file_stem, phase = phase, statut = statut)
  
  new_row <- data.frame(
    id_proposition = tools::file_path_sans_ext(basename(file_path)),
    id_lignee = tools::file_path_sans_ext(basename(file_path)),
    id_proposition_source = "",
    fichier_source = "",
    phase_proposition = phase,
    statut_proposition = statut,
    type_operation = proposal_type,
    idep_agent_edition = as.character(draft$idep_agent),
    horodatage_edition = created_at,
    commentaire_edition = "",
    code_ogr_parent_edition = as.character(draft$code_ogr_parent),
    code_rome_parent_edition = as.character(draft$code_rome_parent),
    libelle_parent_edition = as.character(draft$libelle_parent),
    codes_ogr_enfants_edition = paste(as.character(draft$child_codes), collapse = " | "),
    libelles_enfants_edition = paste(as.character(draft$child_labels), collapse = " | "),
    nb_enfants_edition = length(draft$child_labels),
    base_famille_id = if (is.null(base_family)) "" else base_family$id_famille[1],
    base_codes_ogr_enfants = paste(base_child_codes, collapse = " | "),
    delta_enfants_ajoutes_edition = child_delta$ajoutes,
    delta_enfants_retires_edition = child_delta$retires,
    idep_agent_supervision = "",
    horodatage_prise_en_charge_supervision = "",
    horodatage_decision_supervision = "",
    decision_supervision = "",
    commentaire_supervision = "",
    code_ogr_parent_supervision = "",
    code_rome_parent_supervision = "",
    libelle_parent_supervision = "",
    codes_ogr_enfants_supervision = "",
    libelles_enfants_supervision = "",
    nb_enfants_supervision = 0L,
    delta_enfants_ajoutes_supervision = "",
    delta_enfants_retires_supervision = "",
    idep_agent_validation = "",
    horodatage_prise_en_charge_validation = "",
    horodatage_decision_validation = "",
    decision_validation = "",
    commentaire_validation = "",
    code_ogr_parent_validation = "",
    code_rome_parent_validation = "",
    libelle_parent_validation = "",
    codes_ogr_enfants_validation = "",
    libelles_enfants_validation = "",
    nb_enfants_validation = 0L,
    id_famille_stock_publiee = "",
    code_ogr_parent = as.character(draft$code_ogr_parent),
    code_rome_parent = as.character(draft$code_rome_parent),
    libelle_parent = as.character(draft$libelle_parent),
    codes_ogr_enfants = paste(as.character(draft$child_codes), collapse = " | "),
    libelles_enfants = paste(as.character(draft$child_labels), collapse = " | "),
    nb_enfants = length(draft$child_labels),
    fichier_stockage = basename(file_path),
    stringsAsFactors = FALSE
  )
  
  write_proposal_file(normalize_proposal_row(new_row))
  invisible(new_row$id_proposition)
}

delete_family_by_id <- function(id_famille) {
  families <- load_families()
  family_row <- families[families$id_famille == id_famille, , drop = FALSE]
  
  if (nrow(family_row) == 0) {
    return(invisible(id_famille))
  }
  
  path_family <- file.path(family_storage_dir(), family_row$fichier_stockage[1])
  if (file.exists(path_family)) {
    unlink(path_family, force = TRUE)
  }
  
  refresh_edep_reference()
  
  invisible(id_famille)
}

delete_proposal_by_id <- function(id_proposition, phase = "edition") {
  proposals <- load_proposals(phase)
  proposal_row <- proposals[proposals$id_proposition == id_proposition, , drop = FALSE]
  
  if (nrow(proposal_row) == 0) {
    return(invisible(id_proposition))
  }
  
  path_proposal <- proposal_row$path_fichier[1]
  if (file.exists(path_proposal)) {
    unlink(path_proposal, force = TRUE)
  }
  
  invisible(id_proposition)
}

move_proposal_record <- function(proposal_row, new_phase, new_statut) {
  if (is.null(proposal_row) || nrow(proposal_row) == 0) {
    return(invisible(NULL))
  }
  
  old_path <- proposal_row$path_fichier[1]
  proposal_row$phase_proposition[1] <- new_phase
  proposal_row$statut_proposition[1] <- new_statut
  proposal_row$dossier_proposition <- NULL
  proposal_row$path_fichier <- NULL
  
  new_path <- write_proposal_file(proposal_row)
  
  if (file.exists(old_path) && normalizePath(old_path, winslash = "/", mustWork = FALSE) != normalizePath(new_path, winslash = "/", mustWork = FALSE)) {
    unlink(old_path, force = TRUE)
  }
  
  invisible(new_path)
}

take_in_charge_supervision <- function(proposal_row, idep_agent_supervision) {
  proposal_row$idep_agent_supervision[1] <- as.character(idep_agent_supervision)
  proposal_row$horodatage_prise_en_charge_supervision[1] <- now_chr()
  move_proposal_record(proposal_row, new_phase = "supervision", new_statut = "en_cours")
}

save_supervision_decision <- function(proposal_row, idep_agent_supervision, commentaire_supervision = "", decision = c("valide_en_etat", "modifie_et_valide", "rejete"), parent_code = NULL, child_codes = NULL, ref_df = ref_ogr) {
  decision <- match.arg(decision)
  
  proposal_row$idep_agent_supervision[1] <- first_non_empty(idep_agent_supervision, proposal_row$idep_agent_supervision[1])
  proposal_row$horodatage_decision_supervision[1] <- now_chr()
  proposal_row$decision_supervision[1] <- decision
  proposal_row$commentaire_supervision[1] <- as.character(commentaire_supervision)
  
  if (decision == "modifie_et_valide") {
    parent_code <- as.character(parent_code)[1]
    child_codes <- unique(as.character(child_codes))
    child_codes <- child_codes[!is.na(child_codes) & nzchar(child_codes)]
    child_codes <- setdiff(child_codes, parent_code)
    
    parent_row <- ref_df %>% filter(code_ogr == parent_code) %>% slice(1)
    child_rows <- ref_df %>% filter(code_ogr %in% child_codes) %>% distinct(code_ogr, .keep_all = TRUE)
    
    current_child_codes <- split_pipe_values(proposal_row$codes_ogr_enfants[1])
    supervision_delta <- build_child_delta(child_rows$code_ogr, current_child_codes)
    
    proposal_row$code_ogr_parent_supervision[1] <- parent_code
    proposal_row$code_rome_parent_supervision[1] <- if (nrow(parent_row) == 0) "" else parent_row$code_rome[1]
    proposal_row$libelle_parent_supervision[1] <- if (nrow(parent_row) == 0) "" else parent_row$libelle_appellation_metier[1]
    proposal_row$codes_ogr_enfants_supervision[1] <- paste(child_rows$code_ogr, collapse = " | ")
    proposal_row$libelles_enfants_supervision[1] <- paste(child_rows$libelle_appellation_metier, collapse = " | ")
    proposal_row$nb_enfants_supervision[1] <- nrow(child_rows)
    proposal_row$delta_enfants_ajoutes_supervision[1] <- supervision_delta$ajoutes
    proposal_row$delta_enfants_retires_supervision[1] <- supervision_delta$retires
    proposal_row$code_ogr_parent[1] <- proposal_row$code_ogr_parent_supervision[1]
    proposal_row$code_rome_parent[1] <- proposal_row$code_rome_parent_supervision[1]
    proposal_row$libelle_parent[1] <- proposal_row$libelle_parent_supervision[1]
    proposal_row$codes_ogr_enfants[1] <- proposal_row$codes_ogr_enfants_supervision[1]
    proposal_row$libelles_enfants[1] <- proposal_row$libelles_enfants_supervision[1]
    proposal_row$nb_enfants[1] <- proposal_row$nb_enfants_supervision[1]
  } else {
    proposal_row$code_ogr_parent_supervision[1] <- proposal_row$code_ogr_parent[1]
    proposal_row$code_rome_parent_supervision[1] <- proposal_row$code_rome_parent[1]
    proposal_row$libelle_parent_supervision[1] <- proposal_row$libelle_parent[1]
    proposal_row$codes_ogr_enfants_supervision[1] <- proposal_row$codes_ogr_enfants[1]
    proposal_row$libelles_enfants_supervision[1] <- proposal_row$libelles_enfants[1]
    proposal_row$nb_enfants_supervision[1] <- proposal_row$nb_enfants[1]
    proposal_row$delta_enfants_ajoutes_supervision[1] <- if (decision == "valide_en_etat") "" else proposal_row$delta_enfants_ajoutes_supervision[1]
    proposal_row$delta_enfants_retires_supervision[1] <- if (decision == "valide_en_etat") "" else proposal_row$delta_enfants_retires_supervision[1]
  }
  
  if (decision != "rejete") {
    workflow_checks <- build_family_conflict_checks(
      draft = proposal_row_to_draft(proposal_row),
      stock_families = load_families(),
      workflow_proposals = load_active_workflow_proposals(),
      current_proposal = proposal_row
    )
    assert_no_blocking_family_conflicts(
      workflow_checks,
      prefix = "Envoi supervision vers validation refuse"
    )
  }
  
  target_phase <- if (decision == "rejete") "supervision" else "validation"
  target_statut <- if (decision == "rejete") "rejetes" else "a_valider"
  
  move_proposal_record(proposal_row, new_phase = target_phase, new_statut = target_statut)
}

reintegrate_rejected_proposal <- function(proposal_row, idep_agent_edition) {
  if (is.null(proposal_row) || nrow(proposal_row) == 0) {
    return(invisible(NULL))
  }
  
  created_at <- now_chr()
  file_stem <- build_proposal_file_stem(
    idep_agent = idep_agent_edition,
    code_ogr_parent = proposal_row$code_ogr_parent_edition[1],
    nb_enfants = proposal_row$nb_enfants_edition[1],
    phase = "edition",
    created_at = created_at
  )
  file_path <- next_workflow_proposal_path(file_stem, phase = "supervision", statut = "a_superviser")
  
  new_row <- proposal_row
  new_row$id_proposition[1] <- tools::file_path_sans_ext(basename(file_path))
  new_row$id_lignee[1] <- first_non_empty(proposal_row$id_lignee[1], proposal_row$id_proposition[1])
  new_row$id_proposition_source[1] <- proposal_row$id_proposition[1]
  new_row$fichier_source[1] <- proposal_row$fichier_stockage[1]
  new_row$phase_proposition[1] <- "supervision"
  new_row$statut_proposition[1] <- "a_superviser"
  new_row$idep_agent_edition[1] <- as.character(idep_agent_edition)
  new_row$horodatage_edition[1] <- created_at
  new_row$commentaire_edition[1] <- paste("Reprise issue de", proposal_row$id_proposition[1])
  new_row$idep_agent_supervision[1] <- ""
  new_row$horodatage_prise_en_charge_supervision[1] <- ""
  new_row$horodatage_decision_supervision[1] <- ""
  new_row$decision_supervision[1] <- ""
  new_row$commentaire_supervision[1] <- ""
  new_row$code_ogr_parent_supervision[1] <- ""
  new_row$code_rome_parent_supervision[1] <- ""
  new_row$libelle_parent_supervision[1] <- ""
  new_row$codes_ogr_enfants_supervision[1] <- ""
  new_row$libelles_enfants_supervision[1] <- ""
  new_row$nb_enfants_supervision[1] <- 0L
  new_row$delta_enfants_ajoutes_supervision[1] <- ""
  new_row$delta_enfants_retires_supervision[1] <- ""
  new_row$idep_agent_validation[1] <- ""
  new_row$horodatage_prise_en_charge_validation[1] <- ""
  new_row$horodatage_decision_validation[1] <- ""
  new_row$decision_validation[1] <- ""
  new_row$commentaire_validation[1] <- ""
  new_row$code_ogr_parent_validation[1] <- ""
  new_row$code_rome_parent_validation[1] <- ""
  new_row$libelle_parent_validation[1] <- ""
  new_row$codes_ogr_enfants_validation[1] <- ""
  new_row$libelles_enfants_validation[1] <- ""
  new_row$nb_enfants_validation[1] <- 0L
  new_row$id_famille_stock_publiee[1] <- ""
  new_row$code_ogr_parent[1] <- proposal_row$code_ogr_parent_edition[1]
  new_row$code_rome_parent[1] <- proposal_row$code_rome_parent_edition[1]
  new_row$libelle_parent[1] <- proposal_row$libelle_parent_edition[1]
  new_row$codes_ogr_enfants[1] <- proposal_row$codes_ogr_enfants_edition[1]
  new_row$libelles_enfants[1] <- proposal_row$libelles_enfants_edition[1]
  new_row$nb_enfants[1] <- proposal_row$nb_enfants_edition[1]
  new_row$fichier_stockage[1] <- basename(file_path)
  new_row$path_fichier <- NULL
  new_row$dossier_proposition <- NULL
  
  write_proposal_file(new_row)
  invisible(new_row$id_proposition[1])
}

take_in_charge_validation <- function(proposal_row, idep_agent_validation) {
  proposal_row$idep_agent_validation[1] <- as.character(idep_agent_validation)
  proposal_row$horodatage_prise_en_charge_validation[1] <- now_chr()
  move_proposal_record(proposal_row, new_phase = "validation", new_statut = "en_cours")
}

set_validation_waiting <- function(proposal_row, idep_agent_validation, commentaire_validation = "") {
  proposal_row$idep_agent_validation[1] <- first_non_empty(idep_agent_validation, proposal_row$idep_agent_validation[1])
  proposal_row$commentaire_validation[1] <- as.character(commentaire_validation)
  move_proposal_record(proposal_row, new_phase = "validation", new_statut = "en_attente")
}

resume_validation_work <- function(proposal_row, idep_agent_validation, commentaire_validation = "") {
  proposal_row$idep_agent_validation[1] <- first_non_empty(idep_agent_validation, proposal_row$idep_agent_validation[1])
  proposal_row$commentaire_validation[1] <- as.character(commentaire_validation)
  move_proposal_record(proposal_row, new_phase = "validation", new_statut = "en_cours")
}

save_validation_decision <- function(proposal_row, idep_agent_validation, commentaire_validation = "", decision = c("validee", "rejetee")) {
  decision <- match.arg(decision)
  
  proposal_row$idep_agent_validation[1] <- first_non_empty(idep_agent_validation, proposal_row$idep_agent_validation[1])
  proposal_row$horodatage_decision_validation[1] <- now_chr()
  proposal_row$decision_validation[1] <- decision
  proposal_row$commentaire_validation[1] <- as.character(commentaire_validation)
  proposal_row$code_ogr_parent_validation[1] <- proposal_row$code_ogr_parent[1]
  proposal_row$code_rome_parent_validation[1] <- proposal_row$code_rome_parent[1]
  proposal_row$libelle_parent_validation[1] <- proposal_row$libelle_parent[1]
  proposal_row$codes_ogr_enfants_validation[1] <- proposal_row$codes_ogr_enfants[1]
  proposal_row$libelles_enfants_validation[1] <- proposal_row$libelles_enfants[1]
  proposal_row$nb_enfants_validation[1] <- proposal_row$nb_enfants[1]
  
  target_statut <- if (decision == "validee") "valides" else "rejetes"
  move_proposal_record(proposal_row, new_phase = "validation", new_statut = target_statut)
}

publish_validation_to_stock <- function(proposal_row, idep_agent_validation, commentaire_validation = "") {
  proposal_row$idep_agent_validation[1] <- first_non_empty(idep_agent_validation, proposal_row$idep_agent_validation[1])
  proposal_row$horodatage_decision_validation[1] <- now_chr()
  proposal_row$decision_validation[1] <- "validee"
  proposal_row$commentaire_validation[1] <- as.character(commentaire_validation)
  proposal_row$code_ogr_parent_validation[1] <- proposal_row$code_ogr_parent[1]
  proposal_row$code_rome_parent_validation[1] <- proposal_row$code_rome_parent[1]
  proposal_row$libelle_parent_validation[1] <- proposal_row$libelle_parent[1]
  proposal_row$codes_ogr_enfants_validation[1] <- proposal_row$codes_ogr_enfants[1]
  proposal_row$libelles_enfants_validation[1] <- proposal_row$libelles_enfants[1]
  proposal_row$nb_enfants_validation[1] <- proposal_row$nb_enfants[1]
  
  workflow_checks <- build_family_conflict_checks(
    draft = proposal_row_to_draft(proposal_row),
    stock_families = load_families(),
    workflow_proposals = load_active_workflow_proposals(),
    current_proposal = proposal_row
  )
  assert_no_blocking_family_conflicts(
    workflow_checks,
    prefix = "Publication validation refusee"
  )
  
  if (nzchar(proposal_row$base_famille_id[1])) {
    delete_family_by_id(proposal_row$base_famille_id[1])
  }
  
  family_id <- create_family(
    idep_agent = proposal_row$idep_agent_edition[1],
    code_ogr_parent = proposal_row$code_ogr_parent[1],
    code_rome_parent = proposal_row$code_rome_parent[1],
    libelle_parent = proposal_row$libelle_parent[1],
    codes_ogr_enfants = split_pipe_values(proposal_row$codes_ogr_enfants[1]),
    libelles_enfants = split_pipe_values(proposal_row$libelles_enfants[1]),
    id_proposition_source = proposal_row$id_proposition[1],
    idep_agent_validation = proposal_row$idep_agent_validation[1],
    date_validation = proposal_row$horodatage_decision_validation[1]
  )
  
  proposal_row$id_famille_stock_publiee[1] <- family_id
  move_proposal_record(proposal_row, new_phase = "validation", new_statut = "valides")
  
  invisible(family_id)
}

family_signature <- function(code_ogr_parent, libelle_parent, child_codes, child_labels) {
  parent_key <- if (length(code_ogr_parent) > 0 && !is.na(code_ogr_parent[1]) && nzchar(code_ogr_parent[1])) {
    paste0("C:", code_ogr_parent[1])
  } else {
    paste0("L:", trimws(libelle_parent[1]))
  }
  
  child_code_values <- child_codes[!is.na(child_codes) & nzchar(child_codes)]
  child_label_values <- child_labels[!is.na(child_labels) & nzchar(child_labels)]
  
  child_keys <- if (length(child_code_values) > 0) {
    paste0("C:", sort(unique(child_code_values)))
  } else {
    paste0("L:", sort(unique(child_label_values)))
  }
  
  paste(parent_key, paste(child_keys, collapse = "||"), sep = "::")
}

find_duplicate_family <- function(draft, families) {
  if (is.null(draft) || nrow(families) == 0) {
    return(NULL)
  }
  
  draft_signature <- family_signature(
    draft$code_ogr_parent,
    draft$libelle_parent,
    draft$child_codes,
    draft$child_labels
  )
  
  for (i in seq_len(nrow(families))) {
    row_signature <- family_signature(
      families$code_ogr_parent[i],
      families$libelle_parent[i],
      split_pipe_values(families$codes_ogr_enfants[i]),
      split_pipe_values(families$libelles_enfants[i])
    )
    
    if (identical(draft_signature, row_signature)) {
      return(families[i, , drop = FALSE])
    }
  }
  
  NULL
}

find_child_conflicts <- function(draft, families) {
  empty_conflicts <- data.frame(
    child_code = character(),
    child_label = character(),
    family_id = character(),
    family_parent = character(),
    stringsAsFactors = FALSE
  )
  
  if (is.null(draft) || nrow(families) == 0 || length(draft$child_labels) == 0) {
    return(empty_conflicts)
  }
  
  conflict_rows <- list()
  
  for (i in seq_len(nrow(families))) {
    existing_child_codes <- split_pipe_values(families$codes_ogr_enfants[i])
    existing_child_labels <- split_pipe_values(families$libelles_enfants[i])
    
    for (j in seq_along(draft$child_labels)) {
      draft_code <- draft$child_codes[j]
      draft_label <- draft$child_labels[j]
      
      code_match <- nzchar(draft_code) && draft_code %in% existing_child_codes
      label_match <- (!nzchar(draft_code)) && nzchar(draft_label) && draft_label %in% existing_child_labels
      
      if (code_match || label_match) {
        conflict_rows[[length(conflict_rows) + 1]] <- data.frame(
          child_code = draft_code,
          child_label = draft_label,
          family_id = families$id_famille[i],
          family_parent = families$libelle_parent[i],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(conflict_rows) == 0) {
    return(empty_conflicts)
  }
  
  bind_rows(conflict_rows) %>%
    distinct(child_code, child_label, family_id, family_parent, .keep_all = TRUE)
}

proposal_row_to_draft <- function(proposal_row) {
  if (is.null(proposal_row) || nrow(proposal_row) == 0) {
    return(NULL)
  }
  
  list(
    idep_agent = first_non_empty(proposal_row$idep_agent_edition[1], ""),
    code_ogr_parent = as.character(proposal_row$code_ogr_parent[1]),
    code_rome_parent = as.character(proposal_row$code_rome_parent[1]),
    libelle_parent = as.character(proposal_row$libelle_parent[1]),
    child_codes = split_pipe_values(proposal_row$codes_ogr_enfants[1]),
    child_labels = split_pipe_values(proposal_row$libelles_enfants[1])
  )
}

build_family_conflict_checks <- function(draft, stock_families, workflow_proposals = proposals_template(), current_proposal = NULL) {
  empty_conflicts <- data.frame(
    child_code = character(),
    child_label = character(),
    family_id = character(),
    family_parent = character(),
    stringsAsFactors = FALSE
  )
  
  empty_checks <- list(
    duplicate_stock = NULL,
    duplicate_workflow = NULL,
    child_conflicts = empty_conflicts,
    current_proposal_id = "",
    base_family_id = ""
  )
  
  if (is.null(draft) || !nzchar(first_non_empty(draft$code_ogr_parent, ""))) {
    return(empty_checks)
  }
  
  stock_families <- if (is.null(stock_families)) {
    families_template()
  } else {
    normalize_families(stock_families)
  }
  
  workflow_proposals <- if (is.null(workflow_proposals)) {
    proposals_template()
  } else {
    normalize_proposals(workflow_proposals)
  }
  
  current_proposal_id <- if (is.null(current_proposal) || nrow(current_proposal) == 0) {
    ""
  } else {
    first_non_empty(current_proposal$id_proposition[1], "")
  }
  
  base_family_id <- if (is.null(current_proposal) || nrow(current_proposal) == 0) {
    base_family <- find_base_family_for_draft(draft, stock_families)
    if (is.null(base_family)) "" else base_family$id_famille[1]
  } else {
    first_non_empty(current_proposal$base_famille_id[1], "")
  }
  
  if (nzchar(current_proposal_id)) {
    workflow_proposals <- workflow_proposals %>%
      filter(id_proposition != current_proposal_id)
  }
  
  workflow_families <- project_proposals_as_families(workflow_proposals)
  child_conflict_families <- bind_rows(stock_families, workflow_families) %>%
    normalize_families()
  
  exclude_ids <- unique(c(current_proposal_id, base_family_id))
  exclude_ids <- exclude_ids[nzchar(exclude_ids)]
  
  if (length(exclude_ids) > 0) {
    child_conflict_families <- child_conflict_families %>%
      filter(!(id_famille %in% exclude_ids))
  }
  
  list(
    duplicate_stock = find_duplicate_family(draft, stock_families),
    duplicate_workflow = find_duplicate_family(draft, workflow_families),
    child_conflicts = find_child_conflicts(draft, child_conflict_families),
    current_proposal_id = current_proposal_id,
    base_family_id = base_family_id
  )
}

has_blocking_family_conflicts <- function(checks) {
  if (is.null(checks)) {
    return(FALSE)
  }
  
  !is.null(checks$duplicate_stock) ||
    !is.null(checks$duplicate_workflow) ||
    nrow(checks$child_conflicts) > 0
}

build_family_conflict_modal_content <- function(checks, max_rows = 10L) {
  if (is.null(checks)) {
    return(tags$p("Aucun conflit detecte."))
  }
  
  tagList(
    if (!is.null(checks$duplicate_stock)) {
      tagList(
        tags$p("Cette combinaison parent / enfants existe deja dans le stock."),
        tags$p(tags$strong("Famille stock : "), checks$duplicate_stock$id_famille[1]),
        tags$p(tags$strong("Parent : "), checks$duplicate_stock$libelle_parent[1])
      )
    },
    if (!is.null(checks$duplicate_workflow)) {
      tagList(
        tags$p("Cette combinaison parent / enfants existe deja dans une autre proposition en cours de workflow."),
        tags$p(tags$strong("Proposition : "), checks$duplicate_workflow$id_famille[1]),
        tags$p(tags$strong("Parent : "), checks$duplicate_workflow$libelle_parent[1])
      )
    },
    if (nrow(checks$child_conflicts) > 0) {
      tagList(
        tags$p("Certaines appellations sont deja rattachees a une autre famille ou a une autre proposition active."),
        tags$table(
          class = "table table-striped table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Code OGR"),
              tags$th("Libelle enfant"),
              tags$th("Famille existante"),
              tags$th("Parent existant")
            )
          ),
          tags$tbody(
            lapply(seq_len(min(nrow(checks$child_conflicts), max_rows)), function(i) {
              tags$tr(
                tags$td(checks$child_conflicts$child_code[i]),
                tags$td(checks$child_conflicts$child_label[i]),
                tags$td(checks$child_conflicts$family_id[i]),
                tags$td(checks$child_conflicts$family_parent[i])
              )
            })
          )
        ),
        if (nrow(checks$child_conflicts) > max_rows) {
          tags$p(paste("...", nrow(checks$child_conflicts) - max_rows, "autres conflits non affiches."))
        }
      )
    }
  )
}

build_family_conflict_error_message <- function(checks, prefix = "Operation bloquee") {
  if (is.null(checks)) {
    return(prefix)
  }
  
  details <- character(0)
  
  if (!is.null(checks$duplicate_stock)) {
    details <- c(details, paste("doublon stock", checks$duplicate_stock$id_famille[1]))
  }
  
  if (!is.null(checks$duplicate_workflow)) {
    details <- c(details, paste("doublon workflow", checks$duplicate_workflow$id_famille[1]))
  }
  
  if (nrow(checks$child_conflicts) > 0) {
    details <- c(details, paste(nrow(checks$child_conflicts), "appellation(s) deja rattachee(s)"))
  }
  
  if (length(details) == 0) {
    return(prefix)
  }
  
  paste0(prefix, " : ", paste(details, collapse = " ; "))
}

assert_no_blocking_family_conflicts <- function(checks, prefix = "Operation bloquee") {
  if (has_blocking_family_conflicts(checks)) {
    stop(build_family_conflict_error_message(checks, prefix = prefix), call. = FALSE)
  }
  
  invisible(TRUE)
}

enrich_family_diagnostics <- function(families) {
  if (nrow(families) == 0) {
    families$doublon_parent <- character(0)
    families$doublon_enfant <- character(0)
    families$codes_enfants_en_doublon <- character(0)
    return(families)
  }
  
  parent_flags <- families %>%
    filter(nzchar(code_ogr_parent)) %>%
    count(code_ogr_parent, name = "nb_familles_parent") %>%
    mutate(doublon_parent = ifelse(nb_familles_parent > 1, "OUI", "NON"))
  
  child_rows <- bind_rows(lapply(seq_len(nrow(families)), function(i) {
    child_codes <- unique(split_pipe_values(families$codes_ogr_enfants[i]))
    if (length(child_codes) == 0) {
      return(NULL)
    }
    
    data.frame(
      id_famille = families$id_famille[i],
      code_ogr_parent = families$code_ogr_parent[i],
      child_code = child_codes,
      stringsAsFactors = FALSE
    )
  }))
  
  if (is.null(child_rows) || nrow(child_rows) == 0) {
    child_flags <- data.frame(
      id_famille = character(),
      doublon_enfant = character(),
      codes_enfants_en_doublon = character(),
      stringsAsFactors = FALSE
    )
  } else {
    duplicated_child_codes <- child_rows %>%
      filter(nzchar(child_code), nzchar(code_ogr_parent)) %>%
      distinct(id_famille, code_ogr_parent, child_code, .keep_all = TRUE) %>%
      group_by(child_code) %>%
      summarise(nb_parents = n_distinct(code_ogr_parent), .groups = "drop") %>%
      filter(nb_parents > 1)
    
    child_flags <- child_rows %>%
      semi_join(duplicated_child_codes, by = c("child_code")) %>%
      group_by(id_famille) %>%
      summarise(
        doublon_enfant = "OUI",
        codes_enfants_en_doublon = paste(sort(unique(child_code)), collapse = " | "),
        .groups = "drop"
      )
  }
  
  families %>%
    left_join(parent_flags %>% select(code_ogr_parent, doublon_parent), by = "code_ogr_parent") %>%
    left_join(child_flags, by = "id_famille") %>%
    mutate(
      doublon_parent = ifelse(is.na(doublon_parent), "NON", doublon_parent),
      doublon_enfant = ifelse(is.na(doublon_enfant), "NON", doublon_enfant),
      codes_enfants_en_doublon = ifelse(is.na(codes_enfants_en_doublon), "", codes_enfants_en_doublon)
    )
}

child_df_from_vectors <- function(child_codes, child_labels) {
  max_len <- max(length(child_codes), length(child_labels))
  
  if (max_len == 0) {
    return(data.frame(code_ogr = character(), libelle = character(), stringsAsFactors = FALSE))
  }
  
  length(child_codes) <- max_len
  length(child_labels) <- max_len
  
  data.frame(
    code_ogr = ifelse(is.na(child_codes), "", child_codes),
    libelle = ifelse(is.na(child_labels), "", child_labels),
    stringsAsFactors = FALSE
  )
}

find_stock_family_by_id <- function(stock_families, id_famille) {
  if (is.null(stock_families) || nrow(stock_families) == 0) {
    return(NULL)
  }
  
  id_famille <- as.character(id_famille)[1]
  
  if (length(id_famille) == 0 || is.na(id_famille) || !nzchar(id_famille)) {
    return(NULL)
  }
  
  match_idx <- which(as.character(stock_families$id_famille) == id_famille)
  
  if (length(match_idx) == 0) {
    return(NULL)
  }
  
  stock_families[match_idx[1], , drop = FALSE]
}

format_family_reference <- function(code_ogr, libelle, empty_label = "Aucun") {
  code_ogr <- as.character(code_ogr)[1]
  libelle <- as.character(libelle)[1]
  
  has_code <- length(code_ogr) > 0 && !is.na(code_ogr) && nzchar(code_ogr)
  has_label <- length(libelle) > 0 && !is.na(libelle) && nzchar(libelle)
  
  if (has_code && has_label) {
    return(paste0(libelle, " [", code_ogr, "]"))
  }
  
  if (has_label) {
    return(libelle)
  }
  
  if (has_code) {
    return(code_ogr)
  }
  
  empty_label
}

build_before_after_child_df <- function(before_codes, before_labels, after_codes, after_labels) {
  before_df <- child_df_from_vectors(before_codes, before_labels)
  after_df <- child_df_from_vectors(after_codes, after_labels)
  compare_rows <- list()
  
  register_rows <- function(df, state = c("before", "after")) {
    state <- match.arg(state)
    
    if (nrow(df) == 0) {
      return(invisible(NULL))
    }
    
    for (i in seq_len(nrow(df))) {
      code_ogr <- as.character(df$code_ogr[i])
      libelle <- as.character(df$libelle[i])
      
      row_key <- if (!is.na(code_ogr) && nzchar(code_ogr)) {
        paste0("code:", code_ogr)
      } else if (!is.na(libelle) && nzchar(libelle)) {
        paste0("label:", libelle)
      } else {
        paste0("row:", state, ":", i)
      }
      
      row_value <- compare_rows[[row_key]]
      
      if (is.null(row_value)) {
        row_value <- list(
          code_ogr = "",
          libelle = "",
          before = FALSE,
          after = FALSE
        )
      }
      
      row_value$code_ogr <- first_non_empty(row_value$code_ogr, code_ogr)
      row_value$libelle <- first_non_empty(row_value$libelle, libelle)
      row_value[[state]] <- TRUE
      compare_rows[[row_key]] <<- row_value
    }
    
    invisible(NULL)
  }
  
  register_rows(before_df, "before")
  register_rows(after_df, "after")
  
  if (length(compare_rows) == 0) {
    return(data.frame(
      code_ogr = character(),
      libelle = character(),
      avant = character(),
      apres = character(),
      evolution = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  compare_df <- do.call(rbind, lapply(compare_rows, function(row_value) {
    data.frame(
      code_ogr = first_non_empty(row_value$code_ogr, "-"),
      libelle = first_non_empty(row_value$libelle, "-"),
      avant = if (isTRUE(row_value$before)) "Oui" else "-",
      apres = if (isTRUE(row_value$after)) "Oui" else "-",
      evolution = if (isTRUE(row_value$before) && isTRUE(row_value$after)) {
        "Conserve"
      } else if (isTRUE(row_value$after)) {
        "Ajoute"
      } else {
        "Retire"
      },
      stringsAsFactors = FALSE
    )
  }))
  
  compare_df$sort_order <- match(compare_df$evolution, c("Ajoute", "Retire", "Conserve"))
  compare_df <- compare_df[
    order(compare_df$sort_order, compare_df$libelle, compare_df$code_ogr),
    c("code_ogr", "libelle", "avant", "apres", "evolution"),
    drop = FALSE
  ]
  rownames(compare_df) <- NULL
  compare_df
}

build_modification_compare <- function(proposal_row, stock_families) {
  empty_child_compare <- build_before_after_child_df(
    character(0),
    character(0),
    character(0),
    character(0)
  )
  
  if (is.null(proposal_row) || nrow(proposal_row) == 0) {
    return(list(
      is_modification = FALSE,
      available = FALSE,
      reason = "",
      base_family_id = "",
      base_family = NULL,
      parent_before = "Aucun",
      parent_after = "Aucun",
      before_child_count = 0L,
      after_child_count = 0L,
      child_compare = empty_child_compare
    ))
  }
  
  after_child_codes <- split_pipe_values(proposal_row$codes_ogr_enfants[1])
  after_child_labels <- split_pipe_values(proposal_row$libelles_enfants[1])
  after_child_count <- nrow(child_df_from_vectors(after_child_codes, after_child_labels))
  parent_after <- format_family_reference(
    proposal_row$code_ogr_parent[1],
    proposal_row$libelle_parent[1]
  )
  
  if (!identical(as.character(proposal_row$type_operation[1]), "modification")) {
    return(list(
      is_modification = FALSE,
      available = FALSE,
      reason = "",
      base_family_id = "",
      base_family = NULL,
      parent_before = "Aucun",
      parent_after = parent_after,
      before_child_count = 0L,
      after_child_count = after_child_count,
      child_compare = empty_child_compare
    ))
  }
  
  base_family_id <- first_non_empty(proposal_row$base_famille_id[1], "")
  
  if (!nzchar(base_family_id)) {
    return(list(
      is_modification = TRUE,
      available = FALSE,
      reason = "Famille stock de base non renseignee pour comparer l'avant/apres.",
      base_family_id = "",
      base_family = NULL,
      parent_before = "Aucune",
      parent_after = parent_after,
      before_child_count = 0L,
      after_child_count = after_child_count,
      child_compare = empty_child_compare
    ))
  }
  
  base_family <- find_stock_family_by_id(stock_families, base_family_id)
  
  if (is.null(base_family)) {
    return(list(
      is_modification = TRUE,
      available = FALSE,
      reason = paste("Famille stock de base", base_family_id, "introuvable pour comparer l'avant/apres."),
      base_family_id = base_family_id,
      base_family = NULL,
      parent_before = "Introuvable",
      parent_after = parent_after,
      before_child_count = 0L,
      after_child_count = after_child_count,
      child_compare = empty_child_compare
    ))
  }
  
  before_child_codes <- split_pipe_values(base_family$codes_ogr_enfants[1])
  before_child_labels <- split_pipe_values(base_family$libelles_enfants[1])
  before_child_count <- nrow(child_df_from_vectors(before_child_codes, before_child_labels))
  
  list(
    is_modification = TRUE,
    available = TRUE,
    reason = "",
    base_family_id = base_family_id,
    base_family = base_family,
    parent_before = format_family_reference(
      base_family$code_ogr_parent[1],
      base_family$libelle_parent[1]
    ),
    parent_after = parent_after,
    before_child_count = before_child_count,
    after_child_count = after_child_count,
    child_compare = build_before_after_child_df(
      before_child_codes,
      before_child_labels,
      after_child_codes,
      after_child_labels
    )
  )
}

make_modification_compare_datatable <- function(compare_df) {
  DT::datatable(
    compare_df,
    rownames = FALSE,
    selection = "none",
    class = "compact stripe hover",
    colnames = c("Code OGR", "Libelle", "Avant", "Apres", "Evolution"),
    options = list(
      dom = "t",
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      ordering = FALSE,
      autoWidth = TRUE,
      scrollX = TRUE
    )
  ) %>%
    DT::formatStyle(
      "evolution",
      fontWeight = "600",
      color = DT::styleEqual(
        c("Ajoute", "Retire", "Conserve"),
        c("#177245", "#b85a00", "#5f718d")
      )
    ) %>%
    DT::formatStyle(
      c("avant", "apres"),
      color = DT::styleEqual(c("Oui", "-"), c("#177245", "#a0acba"))
    )
}

collapse_reference_labels <- function(code_ogr, libelle) {
  code_ogr <- as.character(code_ogr)
  libelle <- as.character(libelle)
  keep <- !is.na(code_ogr) & nzchar(code_ogr) & !is.na(libelle) & nzchar(libelle)
  
  if (!any(keep)) {
    return("")
  }
  
  paste(build_reference_label(code_ogr[keep], libelle[keep]), collapse = " | ")
}

pipe_values_to_tags <- function(x, empty_label = "Aucun", css_class = "reco-ref-chip") {
  values <- split_pipe_values(x)
  
  if (length(values) == 0) {
    return(tags$span(class = paste(css_class, "is-empty"), empty_label))
  }
  
  tagList(lapply(values, function(value) {
    tags$span(class = css_class, value)
  }))
}

build_recommendation_context <- function(selected_df, ref_df) {
  empty_code_detail <- data.frame(
    code_ogr = character(),
    libelle = character(),
    classification = character(),
    code_rome = character(),
    code_rome_parent = character(),
    lecture_preco = character(),
    principales_du_rome = character(),
    synonymes_du_rome = character(),
    stringsAsFactors = FALSE
  )
  
  empty_rome_detail <- data.frame(
    code_rome = character(),
    code_rome_parent = character(),
    nb_codes_selectionnes = integer(),
    codes_selectionnes = character(),
    nb_principales_rome = integer(),
    principales_du_rome = character(),
    nb_synonymes_rome = integer(),
    synonymes_du_rome = character(),
    stringsAsFactors = FALSE
  )
  
  selected_df <- selected_df %>%
    distinct(code_ogr, .keep_all = TRUE) %>%
    filter(nzchar(code_ogr), nzchar(libelle_appellation_metier))
  
  if (nrow(selected_df) == 0) {
    return(list(
      code_detail = empty_code_detail,
      rome_detail = empty_rome_detail
    ))
  }
  
  ref_scope <- ref_df %>%
    distinct(code_ogr, .keep_all = TRUE) %>%
    filter(code_rome %in% unique(selected_df$code_rome))
  
  rome_context <- ref_scope %>%
    group_by(code_rome) %>%
    summarise(
      code_rome_parent_scope = dplyr::first(c(code_rome_parent[nzchar(code_rome_parent)], "")),
      nb_principales_rome = sum(classification == "PRINCIPALE", na.rm = TRUE),
      principales_du_rome = collapse_reference_labels(
        code_ogr[classification == "PRINCIPALE"],
        libelle_appellation_metier[classification == "PRINCIPALE"]
      ),
      nb_synonymes_rome = sum(classification == "SYNONYME", na.rm = TRUE),
      synonymes_du_rome = collapse_reference_labels(
        code_ogr[classification == "SYNONYME"],
        libelle_appellation_metier[classification == "SYNONYME"]
      ),
      .groups = "drop"
    )
  
  code_detail <- selected_df %>%
    left_join(rome_context, by = "code_rome") %>%
    transmute(
      code_ogr = code_ogr,
      libelle = libelle_appellation_metier,
      classification = ifelse(nzchar(classification), classification, "ND"),
      code_rome = ifelse(nzchar(code_rome), code_rome, "ND"),
      code_rome_parent = ifelse(nzchar(code_rome_parent), code_rome_parent, "ND"),
      lecture_preco = dplyr::case_when(
        classification == "PRINCIPALE" ~ "Plutot parent",
        classification == "SYNONYME" & nb_principales_rome > 0 ~ "Plutot enfant",
        classification == "SYNONYME" ~ "Synonyme sans principale visible",
        TRUE ~ "A arbitrer"
      ),
      principales_du_rome = ifelse(nzchar(principales_du_rome), principales_du_rome, "Aucune"),
      synonymes_du_rome = ifelse(nzchar(synonymes_du_rome), synonymes_du_rome, "Aucun")
    ) %>%
    arrange(code_rome, desc(classification == "PRINCIPALE"), libelle)
  
  rome_detail <- selected_df %>%
    group_by(code_rome) %>%
    summarise(
      code_rome_parent = dplyr::first(c(code_rome_parent[nzchar(code_rome_parent)], "")),
      nb_codes_selectionnes = n_distinct(code_ogr),
      codes_selectionnes = collapse_reference_labels(code_ogr, libelle_appellation_metier),
      .groups = "drop"
    ) %>%
    left_join(rome_context, by = "code_rome") %>%
    transmute(
      code_rome = ifelse(nzchar(code_rome), code_rome, "ND"),
      code_rome_parent = ifelse(nzchar(code_rome_parent), code_rome_parent, ifelse(nzchar(code_rome_parent_scope), code_rome_parent_scope, "ND")),
      nb_codes_selectionnes = nb_codes_selectionnes,
      codes_selectionnes = codes_selectionnes,
      nb_principales_rome = ifelse(is.na(nb_principales_rome), 0L, nb_principales_rome),
      principales_du_rome = ifelse(nzchar(principales_du_rome), principales_du_rome, "Aucune"),
      nb_synonymes_rome = ifelse(is.na(nb_synonymes_rome), 0L, nb_synonymes_rome),
      synonymes_du_rome = ifelse(nzchar(synonymes_du_rome), synonymes_du_rome, "Aucun")
    ) %>%
    arrange(code_rome)
  
  list(
    code_detail = code_detail,
    rome_detail = rome_detail
  )
}

hex_to_rgba <- function(color, alpha = 0.35) {
  rgb_values <- grDevices::col2rgb(color)
  apply(rgb_values, 2, function(x) {
    sprintf("rgba(%d, %d, %d, %.2f)", x[1], x[2], x[3], alpha)
  })
}

ensure_storage()
ref_ogr <- ensure_reference_file()

