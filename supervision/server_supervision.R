supervision_server <- function(input, output, session) {
  if (is.null(session$userData$workflow_refresh)) {
    session$userData$workflow_refresh <- reactiveVal(0)
  }
  
  refresh_token <- session$userData$workflow_refresh
  
  supervision_states <- reactive({
    refresh_token()
    load_proposals("supervision") %>%
      arrange(desc(horodatage_edition), id_proposition)
  })
  
  supervision_queue <- reactive({
    supervision_states() %>%
      filter(statut_proposition == "a_superviser")
  })
  
  supervision_in_progress <- reactive({
    supervision_states() %>%
      filter(statut_proposition == "en_cours")
  })
  
  supervision_rejected <- reactive({
    supervision_states() %>%
      filter(statut_proposition == "rejetes")
  })
  
  validation_pending <- reactive({
    refresh_token()
    load_proposals(
      states = data.frame(
        phase = "validation",
        statut = "a_valider",
        stringsAsFactors = FALSE
      )
    )
  })
  
  stock_families <- reactive({
    refresh_token()
    load_families()
  })
  
  workflow_active_proposals <- reactive({
    refresh_token()
    load_active_workflow_proposals() %>%
      arrange(desc(horodatage_edition), id_proposition)
  })
  
  selected_supervision <- reactive({
    active_tab <- input$supervision_status_tab
    
    if (is.null(active_tab) || identical(active_tab, "A superviser")) {
      idx <- input$table_supervision_queue_rows_selected
      df <- supervision_queue()
    } else if (identical(active_tab, "En cours")) {
      idx <- input$table_supervision_in_progress_rows_selected
      df <- supervision_in_progress()
    } else {
      idx <- input$table_supervision_rejected_rows_selected
      df <- supervision_rejected()
    }
    
    if (length(idx) != 1 || nrow(df) < idx) {
      return(NULL)
    }
    
    df[idx, , drop = FALSE]
  })
  
  show_family_conflict_modal <- function(title, checks, closing_copy) {
    showModal(modalDialog(
      title = title,
      build_family_conflict_modal_content(checks),
      tags$p(closing_copy),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  }
  
  supervision_modification_compare <- reactive({
    proposal <- selected_supervision()
    
    if (is.null(proposal)) {
      return(NULL)
    }
    
    build_modification_compare(proposal, stock_families())
  })
  
  observe({
    ref_choices <- build_reference_choices(
      ref_ogr %>%
        distinct(code_ogr, .keep_all = TRUE)
    )
    
    proposal <- selected_supervision()
    parent_code <- if (is.null(proposal)) character(0) else proposal$code_ogr_parent[1]
    child_codes <- if (is.null(proposal)) character(0) else split_pipe_values(proposal$codes_ogr_enfants[1])
    
    updateSelectizeInput(
      session = session,
      inputId = "supervision_parent_code",
      choices = ref_choices,
      selected = parent_code,
      server = TRUE
    )
    
    child_choices <- ref_choices[unname(ref_choices) != parent_code]
    child_codes <- child_codes[child_codes %in% unname(child_choices)]
    
    updateSelectizeInput(
      session = session,
      inputId = "supervision_child_codes",
      choices = child_choices,
      selected = child_codes,
      server = TRUE
    )
  })
  
  observe({
    ref_choices <- build_reference_choices(
      ref_ogr %>%
        distinct(code_ogr, .keep_all = TRUE)
    )
    
    parent_code <- input$supervision_parent_code
    if (!nzchar(parent_code) || !(parent_code %in% unname(ref_choices))) {
      parent_code <- ""
    }
    
    child_choices <- ref_choices[unname(ref_choices) != parent_code]
    current_children <- isolate(input$supervision_child_codes)
    current_children <- current_children[current_children %in% unname(child_choices)]
    
    updateSelectizeInput(
      session = session,
      inputId = "supervision_child_codes",
      choices = child_choices,
      selected = current_children,
      server = TRUE
    )
  })
  
  supervision_draft <- reactive({
    proposal <- selected_supervision()
    if (is.null(proposal)) {
      return(NULL)
    }
    
    parent_code <- trimws(input$supervision_parent_code)
    child_codes <- unique(trimws(input$supervision_child_codes))
    child_codes <- child_codes[nzchar(child_codes)]
    child_codes <- setdiff(child_codes, parent_code)
    
    parent_row <- ref_ogr %>%
      filter(code_ogr == parent_code) %>%
      slice(1)
    
    child_rows <- ref_ogr %>%
      filter(code_ogr %in% child_codes) %>%
      distinct(code_ogr, .keep_all = TRUE)
    
    list(
      code_ogr_parent = parent_code,
      code_rome_parent = if (nrow(parent_row) == 0) "" else parent_row$code_rome[1],
      libelle_parent = if (nrow(parent_row) == 0) "" else parent_row$libelle_appellation_metier[1],
      child_codes = child_rows$code_ogr,
      child_labels = child_rows$libelle_appellation_metier
    )
  })
  
  output$supervision_action_context <- renderUI({
    proposal <- selected_supervision()
    
    if (is.null(proposal)) {
      return(tags$div(
        class = "supervision-status-banner",
        tags$div(class = "supervision-status-kicker", "Selection"),
        tags$div(class = "supervision-status-title", "Choisis une proposition dans une file"),
        tags$div(class = "supervision-status-copy", "L'action disponible s'adaptera automatiquement a l'etat du fichier : prise en charge, decision de supervision ou reprise.")
      ))
    }
    
    status <- proposal$statut_proposition[1]
    owner <- first_non_empty(proposal$idep_agent_supervision[1], "Aucun superviseur")
    
    if (identical(status, "a_superviser")) {
      return(tags$div(
        class = "supervision-status-banner is-queue",
        tags$div(class = "supervision-status-kicker", "Etape 1"),
        tags$div(class = "supervision-status-title", "Proposition en attente de prise en charge"),
        tags$div(class = "supervision-status-copy", paste0("Cette proposition est encore en file d'attente. Prochaine action : la prendre en charge pour ouvrir les decisions supervision."))
      ))
    }
    
    if (identical(status, "en_cours")) {
      return(tags$div(
        class = "supervision-status-banner is-progress",
        tags$div(class = "supervision-status-kicker", "Etape 2"),
        tags$div(class = "supervision-status-title", paste("Dossier ouvert par", owner)),
        tags$div(class = "supervision-status-copy", "Tu peux maintenant valider en l'etat, modifier puis envoyer en validation MOA, ou rejeter avec commentaire.")
      ))
    }
    
    tags$div(
      class = "supervision-status-banner is-rejected",
      tags$div(class = "supervision-status-kicker", "Etape 3"),
      tags$div(class = "supervision-status-title", "Proposition rejetee"),
      tags$div(class = "supervision-status-copy", "Le rejet reste consultable. Tu peux recreer une reprise d'edition sans perdre la trace du fichier source.")
    )
  })
  
  output$supervision_action_panel <- renderUI({
    proposal <- selected_supervision()
    
    if (is.null(proposal)) {
      return(tags$p(class = "supervision-note", "Selectionne une ligne pour faire apparaitre l'action utile."))
    }
    
    status <- proposal$statut_proposition[1]
    
    if (identical(status, "a_superviser")) {
      return(tags$div(
        class = "supervision-actions",
        actionButton("take_in_charge_supervision", "Prendre en charge", class = "btn-default")
      ))
    }
    
    if (identical(status, "en_cours")) {
      return(tags$div(
        class = "supervision-actions",
        actionButton("validate_supervision_as_is", "Envoyer en validation MOA", class = "btn-success"),
        actionButton("validate_supervision_with_changes", "Modifier puis envoyer en validation MOA", class = "btn-primary"),
        actionButton("reject_supervision", "Rejeter", class = "btn-warning")
      ))
    }
    
    tags$div(
      class = "supervision-actions",
      actionButton("reintegrate_supervision", "Creer une reprise d'edition", class = "btn-info")
    )
  })
  
  output$supervision_edit_state <- renderUI({
    proposal <- selected_supervision()
    
    if (is.null(proposal)) {
      return(tags$div(class = "supervision-edit-help", "La zone d'ajustement se precharge sur la proposition selectionnee."))
    }
    
    if (identical(proposal$statut_proposition[1], "en_cours")) {
      return(tags$div(class = "supervision-edit-help", "Tu es en mode decision supervision. Si tu modifies parent ou enfants, le bouton bleu enverra directement la version supervision en validation MOA."))
    }
    
    if (identical(proposal$statut_proposition[1], "a_superviser")) {
      return(tags$div(class = "supervision-edit-help", "La proposition est encore en attente. Prends-la d'abord en charge pour entrer en decision supervision."))
    }
    
    tags$div(class = "supervision-edit-help", "Ce fichier est rejete. La zone reste lisible, mais la bonne action est de creer une reprise d'edition.")
  })
  
  output$supervision_kpi_panel <- renderUI({
    queue_n <- nrow(supervision_queue())
    in_progress_n <- nrow(supervision_in_progress())
    rejected_n <- nrow(supervision_rejected())
    validation_n <- nrow(validation_pending())
    stock_n <- nrow(stock_families())
    
    oldest_pending_days <- if (queue_n == 0) {
      "0"
    } else {
      pending_times <- suppressWarnings(as.POSIXct(supervision_queue()$horodatage_edition, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()))
      pending_times <- pending_times[!is.na(pending_times)]
      if (length(pending_times) == 0) "0" else sprintf("%.1f", as.numeric(difftime(Sys.time(), min(pending_times), units = "days")))
    }
    
    card <- function(label, value, help) {
      tags$div(
        class = "supervision-kpi",
        tags$div(class = "supervision-kpi-label", label),
        tags$div(class = "supervision-kpi-value", value),
        tags$div(class = "supervision-kpi-help", help)
      )
    }
    
    tags$div(
      class = "supervision-kpis",
      card("A superviser", queue_n, "Propositions en attente de prise en charge"),
      card("En cours", in_progress_n, "Propositions ouvertes par les superviseurs"),
      card("Rejetes", rejected_n, "Fichiers a suivre ou a relancer"),
      card("A valider", validation_n, "Propositions parties en validation"),
      card("Stock familles", stock_n, paste("Anciennete max file supervision :", oldest_pending_days, "jours"))
    )
  })
  
  make_supervision_table <- function(df, mode = c("queue", "progress", "rejected")) {
    mode <- match.arg(mode)
    
    displayed <- df %>%
      transmute(
        `ID proposition` = id_proposition,
        Operation = type_operation,
        `IDEP edition` = idep_agent_edition,
        `IDEP supervision` = idep_agent_supervision,
        `Code parent` = code_ogr_parent,
        Parent = libelle_parent,
        `Nb enfants` = nb_enfants,
        `Decision supervision` = decision_supervision,
        `Horodatage edition` = horodatage_edition,
        `Prise en charge supervision` = horodatage_prise_en_charge_supervision,
        `Decision supervision le` = horodatage_decision_supervision
      )

    hidden_targets <- switch(
      mode,
      queue = c(3, 7, 9, 10),
      progress = c(7, 10),
      rejected = integer(0)
    )
    column_defs <- if (length(hidden_targets) == 0) {
      list()
    } else {
      list(list(visible = FALSE, targets = hidden_targets))
    }
    
    datatable(
      displayed,
      extensions = "Buttons",
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "colvis", text = "Choisir les colonnes")
        ),
        columnDefs = column_defs
      )
    ) %>%
      formatStyle(
        "Operation",
        backgroundColor = styleEqual(c("creation", "modification"), c("#eef7ff", "#fff1d6")),
        color = styleEqual(c("creation", "modification"), c("#1f5f8b", "#a75f00")),
        fontWeight = styleEqual(c("creation", "modification"), c("700", "700"))
      ) %>%
      formatStyle(
        "Decision supervision",
        backgroundColor = styleEqual(c("valide_en_etat", "modifie_et_valide", "rejete"), c("#e9f7ef", "#eef7ff", "#fff1d6")),
        color = styleEqual(c("valide_en_etat", "modifie_et_valide", "rejete"), c("#1e8449", "#1f5f8b", "#b45f06")),
        fontWeight = styleEqual(c("valide_en_etat", "modifie_et_valide", "rejete"), c("700", "700", "700"))
      )
  }
  
  output$table_supervision_queue <- renderDT({
    make_supervision_table(supervision_queue(), mode = "queue")
  })
  
  output$table_supervision_in_progress <- renderDT({
    make_supervision_table(supervision_in_progress(), mode = "progress")
  })
  
  output$table_supervision_rejected <- renderDT({
    make_supervision_table(supervision_rejected(), mode = "rejected")
  })
  
  output$supervision_edit_preview <- renderUI({
    draft <- supervision_draft()
    
    if (is.null(draft) || !nzchar(draft$code_ogr_parent)) {
      return(tags$p(class = "supervision-note", "Selectionne une proposition pour precharger la reprise supervision."))
    }
    
    child_df <- child_df_from_vectors(draft$child_codes, draft$child_labels)
    
    tags$div(
      tags$hr(),
      tags$p(tags$strong("Parent retenu : "), paste0(draft$libelle_parent, " [", draft$code_ogr_parent, "]")),
      tags$p(tags$strong("Enfants retenus : "), nrow(child_df))
    )
  })
  
  output$supervision_modification_compare <- DT::renderDT({
    compare <- supervision_modification_compare()
    
    if (is.null(compare) || !isTRUE(compare$available)) {
      return(NULL)
    }
    
    make_modification_compare_datatable(compare$child_compare)
  })
  
  output$supervision_detail <- renderUI({
    proposal <- selected_supervision()
    
    if (is.null(proposal)) {
      return(tags$p("Selectionne une proposition dans une des files de supervision."))
    }
    
    current_children <- child_df_from_vectors(
      split_pipe_values(proposal$codes_ogr_enfants[1]),
      split_pipe_values(proposal$libelles_enfants[1])
    )
    
    edition_children <- child_df_from_vectors(
      split_pipe_values(proposal$codes_ogr_enfants_edition[1]),
      split_pipe_values(proposal$libelles_enfants_edition[1])
    )
    
    compare <- supervision_modification_compare()
    
    box <- function(label, value) {
      tags$div(
        class = "supervision-detail-box",
        tags$div(class = "supervision-detail-label", label),
        tags$div(class = "supervision-detail-value", ifelse(nzchar(value), value, ""))
      )
    }
    
    tagList(
      tags$div(
        class = "supervision-detail-grid",
        box("ID proposition", proposal$id_proposition[1]),
        box("Lignee", proposal$id_lignee[1]),
        box("Phase courante", paste(proposal$phase_proposition[1], proposal$statut_proposition[1])),
        box("Prochaine action", dplyr::case_when(
          proposal$statut_proposition[1] == "a_superviser" ~ "Prendre en charge",
          proposal$statut_proposition[1] == "en_cours" ~ "Decider puis envoyer MOA ou rejeter",
          proposal$statut_proposition[1] == "rejetes" ~ "Creer une reprise",
          TRUE ~ ""
        )),
        box("Operation", proposal$type_operation[1]),
        box("Edition", paste(proposal$idep_agent_edition[1], proposal$horodatage_edition[1])),
        box("Supervision", paste(first_non_empty(proposal$idep_agent_supervision[1], "Non prise en charge"), first_non_empty(proposal$horodatage_prise_en_charge_supervision[1], ""))),
        box("Parent edition", paste0(proposal$libelle_parent_edition[1], " [", proposal$code_ogr_parent_edition[1], "]")),
        box("Parent courant", paste0(proposal$libelle_parent[1], " [", proposal$code_ogr_parent[1], "]")),
        box("Delta edition", paste("Ajoutes :", first_non_empty(proposal$delta_enfants_ajoutes_edition[1], "Aucun"), "| Retires :", first_non_empty(proposal$delta_enfants_retires_edition[1], "Aucun"))),
        box("Enfants edition", if (nrow(edition_children) == 0) "Aucun" else as.character(nrow(edition_children))),
        box("Enfants courants", if (nrow(current_children) == 0) "Aucun" else as.character(nrow(current_children))),
        box("Decision supervision", first_non_empty(proposal$decision_supervision[1], "Pas encore tranchee")),
        box("Commentaire supervision", first_non_empty(proposal$commentaire_supervision[1], "Aucun commentaire")),
        box("Source", first_non_empty(proposal$id_proposition_source[1], "Aucune reprise"))
      ),
      if (!is.null(compare) && isTRUE(compare$is_modification)) {
        tagList(
          tags$hr(),
          tags$h4("Comparatif avant / apres"),
          if (!isTRUE(compare$available)) {
            tags$p(class = "supervision-note", compare$reason)
          } else {
            tagList(
              tags$p(
                class = "supervision-note",
                paste("Base stock :", compare$base_family_id)
              ),
              tags$div(
                class = "supervision-detail-grid",
                box("Parent avant", compare$parent_before),
                box("Parent apres", compare$parent_after),
                box("Enfants avant", as.character(compare$before_child_count)),
                box("Enfants apres", as.character(compare$after_child_count))
              ),
              if (nrow(compare$child_compare) == 0) {
                tags$p(class = "supervision-note", "Aucun enfant a comparer entre l'avant et l'apres.")
              } else {
                DT::DTOutput("supervision_modification_compare")
              }
            )
          }
        )
      }
    )
  })
  
  observeEvent(input$take_in_charge_supervision, {
    proposal <- selected_supervision()
    
    if (is.null(proposal)) {
      showNotification("Selectionne une proposition a prendre en charge.", type = "warning")
      return()
    }
    
    if (!identical(proposal$statut_proposition[1], "a_superviser")) {
      showNotification("La prise en charge ne s'applique qu'aux propositions a superviser.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$supervision_agent))) {
      showNotification("Renseigne l'IDEP supervision.", type = "warning")
      return()
    }
    
    take_in_charge_supervision(proposal, input$supervision_agent)
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "supervision_status_tab", selected = "En cours")
    showNotification("Proposition prise en charge. Les decisions supervision sont maintenant ouvertes.", type = "message", duration = 5)
  })
  
  observeEvent(input$validate_supervision_as_is, {
    proposal <- selected_supervision()
    
    if (is.null(proposal) || !identical(proposal$statut_proposition[1], "en_cours")) {
      showNotification("Prends d'abord en charge une proposition avant de la valider.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$supervision_agent))) {
      showNotification("Renseigne l'IDEP supervision.", type = "warning")
      return()
    }
    
    workflow_checks <- build_family_conflict_checks(
      draft = proposal_row_to_draft(proposal),
      stock_families = stock_families(),
      workflow_proposals = workflow_active_proposals(),
      current_proposal = proposal
    )
    
    if (has_blocking_family_conflicts(workflow_checks)) {
      show_family_conflict_modal(
        title = "Doublon ou chevauchement detecte",
        checks = workflow_checks,
        closing_copy = "Corrigez la proposition avant de l'envoyer en validation MOA."
      )
      return()
    }
    
    save_supervision_decision(
      proposal_row = proposal,
      idep_agent_supervision = input$supervision_agent,
      commentaire_supervision = input$supervision_comment,
      decision = "valide_en_etat",
      ref_df = ref_ogr
    )
    
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "supervision_status_tab", selected = "A superviser")
    showNotification("Proposition envoyee a la validation MOA en l'etat.", type = "message", duration = 6)
  })
  
  observeEvent(input$validate_supervision_with_changes, {
    proposal <- selected_supervision()
    draft <- supervision_draft()
    
    if (is.null(proposal) || !identical(proposal$statut_proposition[1], "en_cours")) {
      showNotification("Prends d'abord en charge une proposition avant de la modifier.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$supervision_agent))) {
      showNotification("Renseigne l'IDEP supervision.", type = "warning")
      return()
    }
    
    if (is.null(draft) || !nzchar(draft$code_ogr_parent) || length(draft$child_codes) == 0) {
      showNotification("Definis un parent et au moins un enfant pour la version supervision.", type = "warning")
      return()
    }
    
    workflow_checks <- build_family_conflict_checks(
      draft = draft,
      stock_families = stock_families(),
      workflow_proposals = workflow_active_proposals(),
      current_proposal = proposal
    )
    
    if (has_blocking_family_conflicts(workflow_checks)) {
      show_family_conflict_modal(
        title = "Doublon ou chevauchement detecte",
        checks = workflow_checks,
        closing_copy = "Corrigez la version supervision avant de l'envoyer en validation MOA."
      )
      return()
    }
    
    save_supervision_decision(
      proposal_row = proposal,
      idep_agent_supervision = input$supervision_agent,
      commentaire_supervision = input$supervision_comment,
      decision = "modifie_et_valide",
      parent_code = draft$code_ogr_parent,
      child_codes = draft$child_codes,
      ref_df = ref_ogr
    )
    
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "supervision_status_tab", selected = "A superviser")
    showNotification("Version supervision modifiee envoyee a la validation MOA.", type = "message", duration = 6)
  })
  
  observeEvent(input$reject_supervision, {
    proposal <- selected_supervision()
    
    if (is.null(proposal) || !identical(proposal$statut_proposition[1], "en_cours")) {
      showNotification("Prends d'abord en charge une proposition avant de la rejeter.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$supervision_agent))) {
      showNotification("Renseigne l'IDEP supervision.", type = "warning")
      return()
    }
    
    save_supervision_decision(
      proposal_row = proposal,
      idep_agent_supervision = input$supervision_agent,
      commentaire_supervision = input$supervision_comment,
      decision = "rejete",
      ref_df = ref_ogr
    )
    
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "supervision_status_tab", selected = "Rejetes")
    showNotification("Proposition rejetee et deplacee dans les rejets supervision.", type = "message", duration = 6)
  })
  
  observeEvent(input$reintegrate_supervision, {
    proposal <- selected_supervision()
    
    if (is.null(proposal) || !identical(proposal$statut_proposition[1], "rejetes")) {
      showNotification("Selectionne un rejet supervision pour creer une reprise.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$reprise_idep_agent))) {
      showNotification("Renseigne l'IDEP reprise edition.", type = "warning")
      return()
    }
    
    reintegrate_rejected_proposal(
      proposal_row = proposal,
      idep_agent_edition = input$reprise_idep_agent
    )
    
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "supervision_status_tab", selected = "A superviser")
    showNotification("Une nouvelle reprise d'edition a ete creee et remise dans la file de supervision.", type = "message", duration = 6)
  })
}
