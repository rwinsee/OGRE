validation_server <- function(input, output, session) {
  if (is.null(session$userData$workflow_refresh)) {
    session$userData$workflow_refresh <- reactiveVal(0)
  }
  
  refresh_token <- session$userData$workflow_refresh
  
  validation_states <- reactive({
    refresh_token()
    load_proposals("validation") %>%
      arrange(desc(horodatage_decision_supervision), desc(horodatage_edition), id_proposition)
  })
  
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
  
  validation_queue <- reactive({
    validation_states() %>%
      filter(statut_proposition == "a_valider")
  })
  
  validation_in_progress <- reactive({
    validation_states() %>%
      filter(statut_proposition == "en_cours")
  })
  
  validation_waiting <- reactive({
    validation_states() %>%
      filter(statut_proposition == "en_attente")
  })
  
  validation_rejected <- reactive({
    validation_states() %>%
      filter(statut_proposition == "rejetes")
  })
  
  validation_validated <- reactive({
    validation_states() %>%
      filter(statut_proposition == "valides")
  })
  
  validation_open_scope <- reactive({
    bind_rows(
      validation_queue(),
      validation_in_progress(),
      validation_waiting()
    ) %>%
      distinct(id_proposition, .keep_all = TRUE)
  })
  
  stock_families <- reactive({
    refresh_token()
    load_families()
  })
  
  edep_reference <- reactive({
    refresh_token()
    load_edep_reference()
  })
  
  edep_links <- reactive({
    refresh_token()
    load_edep_links()
  })
  
  selected_validation <- reactive({
    active_tab <- input$validation_status_tab
    
    if (is.null(active_tab) || identical(active_tab, "A valider")) {
      idx <- input$table_validation_queue_rows_selected
      df <- validation_queue()
    } else if (identical(active_tab, "En cours")) {
      idx <- input$table_validation_in_progress_rows_selected
      df <- validation_in_progress()
    } else if (identical(active_tab, "En attente")) {
      idx <- input$table_validation_waiting_rows_selected
      df <- validation_waiting()
    } else if (identical(active_tab, "Rejetes")) {
      idx <- input$table_validation_rejected_rows_selected
      df <- validation_rejected()
    } else if (identical(active_tab, "Validees")) {
      idx <- input$table_validation_validated_rows_selected
      df <- validation_validated()
    } else {
      return(NULL)
    }
    
    if (length(idx) != 1 || nrow(df) < idx) {
      return(NULL)
    }
    
    df[idx, , drop = FALSE]
  })
  
  output$validation_kpi_panel <- renderUI({
    open_scope <- validation_open_scope()
    supervision_all <- supervision_states()
    stock <- stock_families()
    edep <- edep_reference()
    edep_links_df <- edep_links()
    
    ref_codes <- unique(ref_ogr$code_ogr[nzchar(ref_ogr$code_ogr)])
    parent_codes <- unique(stock$code_ogr_parent[nzchar(stock$code_ogr_parent)])
    child_codes <- unique(unlist(lapply(stock$codes_ogr_enfants, split_pipe_values)))
    linked_codes <- union(parent_codes, child_codes)
    codes_without_children <- setdiff(ref_codes, parent_codes)
    codes_without_link <- setdiff(ref_codes, linked_codes)
    
    supervision_queue_n <- nrow(supervision_queue())
    supervision_in_progress_n <- nrow(supervision_in_progress())
    supervision_rejected_n <- nrow(supervision_rejected())
    supervision_total_n <- nrow(supervision_all)
    queue_n <- nrow(validation_queue())
    in_progress_n <- nrow(validation_in_progress())
    waiting_n <- nrow(validation_waiting())
    rejected_n <- nrow(validation_rejected())
    validated_n <- nrow(validation_validated())
    modification_n <- sum(open_scope$type_operation == "modification", na.rm = TRUE)
    impacted_parent_n <- length(unique(open_scope$code_ogr_parent[nzchar(open_scope$code_ogr_parent)]))
    open_links_n <- sum(open_scope$nb_enfants, na.rm = TRUE)
    distinct_parents <- length(unique(stock$code_ogr_parent[nzchar(stock$code_ogr_parent)]))
    distinct_children <- length(unique(edep_links_df$code_ogr_enfant[nzchar(edep_links_df$code_ogr_enfant)]))
    edep_code_n <- nrow(edep)
    edep_removed_n <- length(setdiff(child_codes, parent_codes))
    oldest_supervision_days <- if (supervision_queue_n == 0) {
      "0"
    } else {
      pending_times <- suppressWarnings(as.POSIXct(supervision_queue()$horodatage_edition, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()))
      pending_times <- pending_times[!is.na(pending_times)]
      if (length(pending_times) == 0) {
        "0"
      } else {
        sprintf("%.1f", as.numeric(difftime(Sys.time(), min(pending_times), units = "days")))
      }
    }
    oldest_pending_days <- if (queue_n == 0) {
      "0"
    } else {
      pending_times <- suppressWarnings(as.POSIXct(validation_queue()$horodatage_decision_supervision, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()))
      pending_times <- pending_times[!is.na(pending_times)]
      if (length(pending_times) == 0) {
        "0"
      } else {
        sprintf("%.1f", as.numeric(difftime(Sys.time(), min(pending_times), units = "days")))
      }
    }
    
    card <- function(label, value, help) {
      tags$div(
        class = "validation-kpi",
        tags$div(class = "validation-kpi-label", label),
        tags$div(class = "validation-kpi-value", value),
        tags$div(class = "validation-kpi-help", help)
      )
    }
    
    section_panel <- function(title, copy, cards) {
      tags$div(
        class = "validation-subpanel-content is-kpi",
        tags$div(class = "validation-kpi-section-title", title),
        tags$div(class = "validation-kpi-section-copy", copy),
        tags$div(class = "validation-kpis", cards)
      )
    }
    
    tags$div(
      class = "validation-kpi-section",
      div(
        class = "validation-subpanel-tabs",
        tabsetPanel(
          id = "validation_kpi_tabs",
          type = "tabs",
          tabPanel(
            "Couverture",
            section_panel(
              "Couverture referentiel",
              "Lecture globale des codes OGR couverts aujourd'hui par le stock publie.",
              tagList(
                card("Codes referentiel", length(ref_codes), "Volume total des codes OGR charges"),
                card("Codes ROME EDEP", edep_code_n, paste(edep_removed_n, "codes enfant exclus du referentiel aval")),
                card("Codes parents", length(parent_codes), "Codes utilises au moins une fois comme parent"),
                card("Codes enfants", length(child_codes), "Codes mobilises comme enfants"),
                card("Propositions edition", supervision_total_n, "Propositions issues de l'edition visibles en supervision"),
                card("Codes sans filiation", length(codes_without_link), paste(length(codes_without_children), "codes n'ont aucun enfant"))
              )
            )
          ),
          tabPanel(
            "Supervision",
            section_panel(
              "Vue supervision",
              "Le valideur voit l'etat amont de la supervision avant d'instruire la file MOA.",
              tagList(
                card("A superviser", supervision_queue_n, paste("Backlog supervision,", oldest_supervision_days, "jour(s) max d'attente")),
                card("En cours supervision", supervision_in_progress_n, "Propositions actuellement ouvertes par les superviseurs"),
                card("Rejetes supervision", supervision_rejected_n, "Fichiers rejetes ou a relancer cote supervision"),
                card("Envoyees a validation", queue_n, "Propositions deja transmises par la supervision vers la MOA")
              )
            )
          ),
          tabPanel(
            "Validation MOA",
            section_panel(
              "Vue validation MOA",
              "Pilotage de la decision MOA, du stock publie et du referentiel EDEP.",
              tagList(
                card("A valider", queue_n, paste("Backlog MOA entrant,", oldest_pending_days, "jour(s) max d'attente")),
                card("En cours", in_progress_n, "Dossiers actuellement ouverts par la MOA"),
                card("En attente", waiting_n, "Dossiers temporises en attente de reprise"),
                card("Rejetes", rejected_n, "Dossiers rejetes par la MOA"),
                card("Validees", validated_n, "Publications deja actees"),
                card("Modifications", modification_n, "Dossiers ouverts qui remplacent une famille stock"),
                card("Parents impactes", impacted_parent_n, "Parents distincts concernes par la file ouverte"),
                card("Liens a publier", open_links_n, "Volume d'enfants portes par la file ouverte"),
                card("Stock familles", nrow(stock), "Familles actuellement publiees"),
                card("Parents EDEP", distinct_parents, "Parents distincts dans le referentiel maison"),
                card("Liens EDEP", nrow(edep_links_df), paste(distinct_children, "enfants distincts relies"))
              )
            )
          )
        )
      )
    )
  })
  
  make_validation_table <- function(df, mode = c("queue", "progress", "waiting", "rejected", "validated")) {
    mode <- match.arg(mode)

    displayed <- df %>%
      transmute(
        `ID proposition` = id_proposition,
        Operation = type_operation,
        `IDEP edition` = idep_agent_edition,
        `IDEP supervision` = idep_agent_supervision,
        `IDEP validation` = idep_agent_validation,
        `Code parent` = code_ogr_parent,
        Parent = libelle_parent,
        `Nb enfants` = nb_enfants,
        `Decision supervision` = decision_supervision,
        `Decision validation` = decision_validation,
        `Horodatage edition` = horodatage_edition,
        `Horodatage supervision` = horodatage_decision_supervision,
        `Horodatage validation` = horodatage_decision_validation
      )

    hidden_targets <- switch(
      mode,
      queue = c(4, 9, 12),
      progress = c(9, 12),
      waiting = c(9, 12),
      rejected = integer(0),
      validated = integer(0)
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
      ) %>%
      formatStyle(
        "Decision validation",
        backgroundColor = styleEqual(c("validee", "rejetee"), c("#e9f7ef", "#ffe9ef")),
        color = styleEqual(c("validee", "rejetee"), c("#1e8449", "#a23b5a")),
        fontWeight = styleEqual(c("validee", "rejetee"), c("700", "700"))
      )
  }
  
  output$table_validation_queue <- renderDT({
    make_validation_table(validation_queue(), mode = "queue")
  })
  
  output$table_validation_in_progress <- renderDT({
    make_validation_table(validation_in_progress(), mode = "progress")
  })
  
  output$table_validation_waiting <- renderDT({
    make_validation_table(validation_waiting(), mode = "waiting")
  })
  
  output$table_validation_rejected <- renderDT({
    make_validation_table(validation_rejected(), mode = "rejected")
  })
  
  output$table_validation_validated <- renderDT({
    make_validation_table(validation_validated(), mode = "validated")
  })
  
  output$table_validation_stock <- renderDT({
    datatable(
      stock_families() %>%
        select(
          id_famille,
          id_proposition_source,
          date_validation,
          idep_agent_validation,
          code_ogr_parent,
          code_rome_parent,
          libelle_parent,
          nb_enfants,
          codes_ogr_enfants
        ),
      extensions = "Buttons",
      selection = "none",
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "colvis", text = "Choisir les colonnes")
        )
      )
    )
  })
  
  output$table_validation_edep <- renderDT({
    datatable(
      edep_reference() %>%
        select(
          code_ogr,
          libelle_appellation_metier,
          code_rome,
          classification,
          role_edep,
          a_famille_edep,
          est_enfant_edep,
          nb_enfants_edep,
          codes_enfants_edep,
          codes_parents_amont_edep,
          libelle_grand_domaine,
          libelle_domaine_professionnel
        ),
      extensions = "Buttons",
      selection = "none",
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "colvis", text = "Choisir les colonnes")
        )
      )
    )
  })
  
  output$table_validation_edep_links <- renderDT({
    datatable(
      edep_links() %>%
        select(
          id_famille,
          id_proposition_source,
          date_validation,
          idep_agent_validation,
          code_ogr_parent,
          libelle_parent,
          code_ogr_enfant,
          libelle_enfant,
          rang_enfant,
          nb_enfants_famille
        ),
      selection = "none",
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$validation_action_context <- renderUI({
    proposal <- selected_validation()
    
    if (is.null(proposal)) {
      return(tags$div(
        class = "validation-status-banner",
        tags$div(class = "validation-status-kicker", "Selection"),
        tags$div(class = "validation-status-title", "Choisis une proposition dans une file MOA"),
        tags$div(class = "validation-status-copy", "La MOA pilote maintenant la fin du cycle : prise en charge, attente, rejet ou publication dans le stock.")
      ))
    }
    
    status <- proposal$statut_proposition[1]
    owner <- first_non_empty(proposal$idep_agent_validation[1], "Aucun validateur")
    
    if (identical(status, "a_valider")) {
      return(tags$div(
        class = "validation-status-banner is-queue",
        tags$div(class = "validation-status-kicker", "Etape MOA 1"),
        tags$div(class = "validation-status-title", "Dossier en attente de prise en charge"),
        tags$div(class = "validation-status-copy", "La proposition arrive de supervision. Prochaine action : l'ouvrir pour instruire la decision MOA.")
      ))
    }
    
    if (identical(status, "en_cours")) {
      return(tags$div(
        class = "validation-status-banner is-progress",
        tags$div(class = "validation-status-kicker", "Etape MOA 2"),
        tags$div(class = "validation-status-title", paste("Instruction en cours par", owner)),
        tags$div(class = "validation-status-copy", "Tu peux publier dans le stock, mettre en attente si le dossier doit temporiser, ou rejeter.")
      ))
    }
    
    if (identical(status, "en_attente")) {
      return(tags$div(
        class = "validation-status-banner is-waiting",
        tags$div(class = "validation-status-kicker", "Etape MOA 3"),
        tags$div(class = "validation-status-title", "Dossier en attente"),
        tags$div(class = "validation-status-copy", "Le dossier est suspendu. Tu peux le reprendre, le publier ou le rejeter.")
      ))
    }
    
    if (identical(status, "rejetes")) {
      return(tags$div(
        class = "validation-status-banner is-rejected",
        tags$div(class = "validation-status-kicker", "Etape MOA 4"),
        tags$div(class = "validation-status-title", "Dossier rejete par la MOA"),
        tags$div(class = "validation-status-copy", "Le rejet reste visible pour audit et relecture."))
      )
    }
    
    tags$div(
      class = "validation-status-banner is-published",
      tags$div(class = "validation-status-kicker", "Etape MOA 5"),
      tags$div(class = "validation-status-title", "Dossier publie"),
      tags$div(class = "validation-status-copy", "La famille est desormais dans le stock et dans le referentiel EDEP.")
    )
  })
  
  output$validation_action_panel <- renderUI({
    proposal <- selected_validation()
    
    if (is.null(proposal)) {
      return(tags$p(class = "validation-note", "Selectionne une ligne pour afficher l'action utile."))
    }
    
    status <- proposal$statut_proposition[1]
    
    if (identical(status, "a_valider")) {
      return(tags$div(
        class = "validation-actions",
        actionButton("take_in_charge_validation", "Prendre en charge", class = "btn-default")
      ))
    }
    
    if (identical(status, "en_cours")) {
      return(tags$div(
        class = "validation-actions",
        actionButton("set_validation_waiting", "Mettre en attente", class = "btn-warning"),
        actionButton("publish_validation", "Publier dans le stock", class = "btn-success"),
        actionButton("reject_validation", "Rejeter", class = "btn-danger")
      ))
    }
    
    if (identical(status, "en_attente")) {
      return(tags$div(
        class = "validation-actions",
        actionButton("resume_validation", "Reprendre la validation", class = "btn-default"),
        actionButton("publish_validation", "Publier dans le stock", class = "btn-success"),
        actionButton("reject_validation", "Rejeter", class = "btn-danger")
      ))
    }
    
    tags$p(class = "validation-note", "Aucune action supplementaire n'est ouverte sur cet etat.")
  })
  
  output$validation_publish_preview <- renderUI({
    proposal <- selected_validation()
    
    if (is.null(proposal)) {
      return(tags$p(class = "validation-impact", "Selectionne une proposition pour voir l'impact de publication sur le stock et le referentiel EDEP."))
    }
    
    impact_text <- if (proposal$type_operation[1] == "modification" && nzchar(proposal$base_famille_id[1])) {
      paste("La publication remplacera la famille stock", proposal$base_famille_id[1], "par une nouvelle version alimentee depuis la validation.")
    } else {
      "La publication creera une nouvelle famille dans le stock."
    }
    
    tags$div(
      class = "validation-impact",
      tags$p(tags$strong("Operation : "), proposal$type_operation[1]),
      tags$p(tags$strong("Famille stock cible : "), first_non_empty(proposal$base_famille_id[1], "Aucune")),
      tags$p(tags$strong("Impact stock : "), impact_text),
      tags$p(tags$strong("Impact referentiel EDEP : "), paste("parent", proposal$code_ogr_parent[1], "avec", proposal$nb_enfants[1], "liens enfant a publier")),
      if (nzchar(proposal$id_famille_stock_publiee[1])) {
        tags$p(tags$strong("Famille publiee : "), proposal$id_famille_stock_publiee[1])
      }
    )
  })
  
  output$validation_detail <- renderUI({
    proposal <- selected_validation()
    
    if (is.null(proposal)) {
      return(tags$p("Selectionne une proposition dans une file validation."))
    }
    
    child_df <- child_df_from_vectors(
      split_pipe_values(proposal$codes_ogr_enfants[1]),
      split_pipe_values(proposal$libelles_enfants[1])
    )
    
    box <- function(label, value) {
      tags$div(
        class = "validation-detail-box",
        tags$div(class = "validation-detail-label", label),
        tags$div(class = "validation-detail-value", ifelse(nzchar(value), value, ""))
      )
    }
    
    tagList(
      tags$div(
        class = "validation-detail-grid",
        box("ID proposition", proposal$id_proposition[1]),
        box("Lignee", proposal$id_lignee[1]),
        box("Phase courante", paste(proposal$phase_proposition[1], proposal$statut_proposition[1])),
        box("Operation", proposal$type_operation[1]),
        box("Edition", paste(proposal$idep_agent_edition[1], proposal$horodatage_edition[1])),
        box("Supervision", paste(first_non_empty(proposal$idep_agent_supervision[1], "NA"), first_non_empty(proposal$horodatage_decision_supervision[1], ""))),
        box("Validation", paste(first_non_empty(proposal$idep_agent_validation[1], "NA"), first_non_empty(proposal$horodatage_decision_validation[1], ""))),
        box("Decision supervision", first_non_empty(proposal$decision_supervision[1], "Aucune")),
        box("Decision validation", first_non_empty(proposal$decision_validation[1], "Aucune")),
        box("Commentaire validation", first_non_empty(proposal$commentaire_validation[1], "Aucun")),
        box("Parent courant", paste0(proposal$libelle_parent[1], " [", proposal$code_ogr_parent[1], "]")),
        box("Famille stock publiee", first_non_empty(proposal$id_famille_stock_publiee[1], "Pas encore publiee"))
      ),
      tags$hr(),
      tags$h4("Contenu courant"),
      if (nrow(child_df) == 0) {
        tags$p("Aucun enfant.")
      } else {
        tags$table(
          class = "table table-striped table-bordered",
          tags$thead(tags$tr(tags$th("Code OGR"), tags$th("Libelle"))),
          tags$tbody(lapply(seq_len(nrow(child_df)), function(i) {
            tags$tr(tags$td(child_df$code_ogr[i]), tags$td(child_df$libelle[i]))
          }))
        )
      }
    )
  })
  
  observeEvent(input$take_in_charge_validation, {
    proposal <- selected_validation()
    
    if (is.null(proposal) || !identical(proposal$statut_proposition[1], "a_valider")) {
      showNotification("Selectionne une proposition a valider pour la prendre en charge.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$validation_agent))) {
      showNotification("Renseigne l'IDEP validation.", type = "warning")
      return()
    }
    
    take_in_charge_validation(proposal, input$validation_agent)
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "validation_status_tab", selected = "En cours")
    showNotification("Proposition prise en charge par la MOA.", type = "message", duration = 5)
  })
  
  observeEvent(input$set_validation_waiting, {
    proposal <- selected_validation()
    
    if (is.null(proposal) || !identical(proposal$statut_proposition[1], "en_cours")) {
      showNotification("Ouvre d'abord un dossier en validation pour le mettre en attente.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$validation_agent))) {
      showNotification("Renseigne l'IDEP validation.", type = "warning")
      return()
    }
    
    set_validation_waiting(proposal, input$validation_agent, input$validation_comment)
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "validation_status_tab", selected = "En attente")
    showNotification("Dossier place en attente cote MOA.", type = "message", duration = 5)
  })
  
  observeEvent(input$resume_validation, {
    proposal <- selected_validation()
    
    if (is.null(proposal) || !identical(proposal$statut_proposition[1], "en_attente")) {
      showNotification("Selectionne un dossier en attente pour le reprendre.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$validation_agent))) {
      showNotification("Renseigne l'IDEP validation.", type = "warning")
      return()
    }
    
    resume_validation_work(proposal, input$validation_agent, input$validation_comment)
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "validation_status_tab", selected = "En cours")
    showNotification("Dossier repris en validation MOA.", type = "message", duration = 5)
  })
  
  observeEvent(input$reject_validation, {
    proposal <- selected_validation()
    
    if (is.null(proposal) || !(proposal$statut_proposition[1] %in% c("en_cours", "en_attente"))) {
      showNotification("Le rejet MOA s'applique a un dossier en cours ou en attente.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$validation_agent))) {
      showNotification("Renseigne l'IDEP validation.", type = "warning")
      return()
    }
    
    save_validation_decision(proposal, input$validation_agent, input$validation_comment, decision = "rejetee")
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "validation_status_tab", selected = "Rejetes")
    showNotification("Proposition rejetee par la MOA.", type = "message", duration = 6)
  })
  
  observeEvent(input$publish_validation, {
    proposal <- selected_validation()
    
    if (is.null(proposal) || !(proposal$statut_proposition[1] %in% c("en_cours", "en_attente"))) {
      showNotification("La publication MOA s'applique a un dossier en cours ou en attente.", type = "warning")
      return()
    }
    
    if (!nzchar(trimws(input$validation_agent))) {
      showNotification("Renseigne l'IDEP validation.", type = "warning")
      return()
    }
    
    publish_validation_to_stock(proposal, input$validation_agent, input$validation_comment)
    refresh_token(refresh_token() + 1)
    updateTabsetPanel(session, "validation_status_tab", selected = "Validees")
    showNotification("Proposition publiee dans le stock et injectee dans le referentiel EDEP.", type = "message", duration = 7)
  })
}
