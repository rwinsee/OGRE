edition_server <- function(input, output, session) {
  if (is.null(session$userData$workflow_refresh)) {
    session$userData$workflow_refresh <- reactiveVal(0)
  }
  
  refresh_token <- session$userData$workflow_refresh
  pending_conflict_draft <- reactiveVal(NULL)
  
  selected_reference <- reactive({
    idx <- input$table_ref_rows_selected
    
    if (length(idx) == 0) {
      return(reference_template()[0, , drop = FALSE])
    }
    
    ref_ogr[idx, , drop = FALSE]
  })
  
  available_reference <- reactive({
    idx <- input$table_ref_rows_selected
    
    if (length(idx) == 0) {
      return(ref_ogr)
    }
    
    ref_ogr[idx, , drop = FALSE]
  })
  
  selection_recommendations <- reactive({
    build_selection_recommendations(selected_reference())
  })
  
  recommendation_detail <- reactive({
    build_recommendation_context(selected_reference(), ref_ogr)
  })
  
  observe({
    ref_df <- available_reference() %>%
      distinct(code_ogr, .keep_all = TRUE)
    ref_choices <- build_reference_choices(ref_df)
    
    parent_code <- isolate(input$parent_code)
    if (!nzchar(parent_code) || !(parent_code %in% unname(ref_choices))) {
      parent_code <- character(0)
    }
    
    updateSelectizeInput(
      session = session,
      inputId = "parent_code",
      choices = ref_choices,
      selected = parent_code,
      server = TRUE
    )
  })
  
  observe({
    ref_df <- available_reference() %>%
      distinct(code_ogr, .keep_all = TRUE)
    ref_choices <- build_reference_choices(ref_df)
    
    dt_idx <- input$table_ref_rows_selected
    parent_code <- input$parent_code
    if (!nzchar(parent_code) || !(parent_code %in% unname(ref_choices))) {
      parent_code <- ""
    }
    
    child_choices <- ref_choices[unname(ref_choices) != parent_code]
    current_children <- isolate(input$child_codes)
    
    if (length(dt_idx) > 0 && nzchar(parent_code)) {
      current_children <- unname(child_choices)
    } else {
      current_children <- current_children[current_children %in% unname(child_choices)]
    }
    
    updateSelectizeInput(
      session = session,
      inputId = "child_codes",
      choices = child_choices,
      selected = current_children,
      server = TRUE
    )
  })
  
  output$recommendation_panel <- renderUI({
    reco <- selection_recommendations()
    
    if (reco$selection_size == 0) {
      return(tags$div(
        class = "reco-box",
        tags$p("Selectionnez quelques lignes dans le referentiel pour obtenir des recommandations sur le parent.")
      ))
    }
    
    rome_parent_text <- if (nrow(reco$rome_parent_summary) == 0) {
      "Aucun code ROME parent renseigne sur la selection."
    } else if (nrow(reco$rome_parent_summary) == 1) {
      paste0(
        "Selection homogene : toutes les lignes pointent vers le code ROME parent ",
        reco$rome_parent_summary$code_rome_parent[1],
        "."
      )
    } else {
      paste0(
        "Selection mixte : ",
        nrow(reco$rome_parent_summary),
        " codes ROME parent differents detectes."
      )
    }
    
    candidate_labels <- if (nrow(reco$candidate_parents) == 0) {
      tags$p("Aucune appellation PRINCIPALE dans la selection.")
    } else {
      tagList(lapply(seq_len(nrow(reco$candidate_parents)), function(i) {
        tags$span(
          class = "reco-chip",
          build_reference_label(
            reco$candidate_parents$code_ogr[i],
            reco$candidate_parents$libelle_appellation_metier[i]
          )
        )
      }))
    }
    
    suggestion_block <- if (nzchar(reco$suggested_parent_code)) {
      tagList(
        tags$div(class = "reco-label", "Parent conseille"),
        tags$div(class = "reco-line", tags$span(class = "reco-value", reco$suggested_parent_label)),
        actionButton("apply_recommended_parent", "Appliquer cette preconisation", class = "btn-default")
      )
    } else {
      tagList(
        tags$div(class = "reco-label", "Parent conseille"),
        tags$p("Pas de parent unique conseille pour l'instant. Garde la main sur le choix.")
      )
    }
    
    detail_button <- tags$div(
      class = "action-row",
      actionButton("show_recommendation_detail", "Voir le detail")
    )
    
    tags$div(
      class = "reco-box",
      tags$div(class = "reco-line", paste("Selection courante :", reco$selection_size, "codes OGR")),
      tags$div(
        class = "reco-line",
        paste("Classification :", reco$principal_count, "PRINCIPALE et", reco$synonym_count, "SYNONYME")
      ),
      tags$div(class = "reco-line", rome_parent_text),
      tags$div(class = "reco-label", "Candidats parent (appellations principales)"),
      candidate_labels,
      tags$hr(),
      suggestion_block,
      detail_button
    )
  })
  
  output$recommendation_detail_summary <- renderUI({
    detail <- recommendation_detail()
    code_detail <- detail$code_detail
    rome_detail <- detail$rome_detail
    
    card <- function(label, value, help, alt = FALSE) {
      tags$div(
        class = paste("reco-summary-card", if (alt) "alt"),
        tags$div(class = "reco-summary-label", label),
        tags$div(class = "reco-summary-value", value),
        tags$div(class = "reco-summary-help", help)
      )
    }
    
    tags$div(
      class = "reco-summary-grid",
      card("Codes OGR", nrow(code_detail), "Nombre de lignes dans la selection"),
      card("Codes ROME", nrow(rome_detail), "Nombre de contextes ROME touches", alt = TRUE),
      card("Principales", sum(code_detail$classification == "PRINCIPALE"), "Appellations principales visibles"),
      card("Synonymes", sum(code_detail$classification == "SYNONYME"), "Appellations synonymes visibles", alt = TRUE)
    )
  })
  
  output$recommendation_code_table <- renderDT({
    detail <- recommendation_detail()
    code_display <- detail$code_detail %>%
      transmute(
        code_ogr = code_ogr,
        libelle = libelle,
        classification = ifelse(
          classification == "PRINCIPALE",
          "<span class='reco-badge reco-badge-parent'>PRINCIPALE</span>",
          "<span class='reco-badge reco-badge-neutral'>SYNONYME</span>"
        ),
        code_rome = code_rome,
        code_rome_parent = code_rome_parent,
        lecture = dplyr::case_when(
          lecture_preco == "Plutot parent" ~ "<span class='reco-badge reco-badge-parent'>Plutot parent</span>",
          lecture_preco == "Plutot enfant" ~ "<span class='reco-badge reco-badge-child'>Plutot enfant</span>",
          TRUE ~ paste0("<span class='reco-badge reco-badge-neutral'>", lecture_preco, "</span>")
        )
      )
    
    datatable(
      code_display,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 6,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "tip",
        columnDefs = list(
          list(width = "90px", targets = 0),
          list(width = "280px", targets = 1),
          list(width = "140px", targets = 2),
          list(width = "90px", targets = 3),
          list(width = "110px", targets = 4),
          list(width = "150px", targets = 5)
        )
      )
    )
  })
  
  output$recommendation_rome_cards <- renderUI({
    detail <- recommendation_detail()
    rome_detail <- detail$rome_detail
    
    if (nrow(rome_detail) == 0) {
      return(tags$p("Aucun contexte ROME a afficher."))
    }
    
    tags$div(
      class = "reco-rome-stack",
      lapply(seq_len(nrow(rome_detail)), function(i) {
        row <- rome_detail[i, , drop = FALSE]
        
        tags$div(
          class = "reco-rome-card",
          tags$div(
            class = "reco-rome-head",
            tags$div(
              tags$div(class = "reco-rome-title", paste("ROME", row$code_rome[1])),
              tags$div(class = "reco-rome-subtitle", paste("ROME parent :", row$code_rome_parent[1]))
            ),
            tags$div(
              class = "reco-rome-metrics",
              tags$span(class = "reco-rome-metric", paste(row$nb_codes_selectionnes[1], "codes selectionnes")),
              tags$span(class = "reco-rome-metric", paste(row$nb_principales_rome[1], "principales visibles")),
              tags$span(class = "reco-rome-metric", paste(row$nb_synonymes_rome[1], "synonymes visibles"))
            )
          ),
          tags$div(
            class = "reco-rome-block",
            tags$div(class = "reco-rome-block-title", "Dans ta selection"),
            pipe_values_to_tags(row$codes_selectionnes[1], empty_label = "Aucun code selectionne")
          ),
          tags$div(
            class = "reco-rome-block",
            tags$div(class = "reco-rome-block-title", "Principales du meme ROME"),
            pipe_values_to_tags(row$principales_du_rome[1], empty_label = "Aucune principale visible")
          ),
          tags$div(
            class = "reco-rome-block",
            tags$div(class = "reco-rome-block-title", "Synonymes du meme ROME"),
            pipe_values_to_tags(row$synonymes_du_rome[1], empty_label = "Aucun synonyme visible")
          )
        )
      })
    )
  })
  
  families_data <- reactive({
    refresh_token()
    load_families() %>%
      arrange(desc(date_creation), id_famille)
  })
  
  proposals_data <- reactive({
    refresh_token()
    load_proposals("edition") %>%
      arrange(desc(horodatage_edition), id_proposition)
  })
  
  families_view <- reactive({
    enrich_family_diagnostics(families_data())
  })
  
  comparable_existing_filiations <- reactive({
    active_proposals <- proposals_data() %>%
      filter(statut_proposition != "rejetes")
    
    bind_rows(
      families_data(),
      project_proposals_as_families(active_proposals)
    ) %>%
      normalize_families()
  })
  
  selected_family <- reactive({
    idx <- input$table_families_rows_selected
    families <- families_view()
    
    if (length(idx) != 1 || nrow(families) < idx) {
      return(NULL)
    }
    
    families[idx, , drop = FALSE]
  })
  
  selected_proposal <- reactive({
    idx <- input$table_proposals_rows_selected
    proposals <- proposals_data()
    
    if (length(idx) != 1 || nrow(proposals) < idx) {
      return(NULL)
    }
    
    proposals[idx, , drop = FALSE]
  })
  
  draft_family <- reactive({
    parent_code <- trimws(input$parent_code)
    child_codes <- unique(trimws(input$child_codes))
    child_codes <- child_codes[nzchar(child_codes)]
    child_codes <- setdiff(child_codes, parent_code)
    
    selected_ref <- available_reference()
    
    if (!nzchar(parent_code)) {
      return(NULL)
    }
    
    parent_row <- selected_ref %>%
      filter(code_ogr == parent_code) %>%
      slice(1)
    
    if (nrow(parent_row) == 0) {
      return(NULL)
    }
    
    child_rows <- selected_ref %>%
      filter(code_ogr %in% child_codes) %>%
      distinct(code_ogr, .keep_all = TRUE)
    
    if (nrow(child_rows) > 0) {
      child_rows <- child_rows[match(child_codes, child_rows$code_ogr), , drop = FALSE]
    }
    
    list(
      id_famille = "BROUILLON",
      idep_agent = trimws(input$idep_agent),
      code_ogr_parent = parent_row$code_ogr[1],
      code_rome_parent = parent_row$code_rome[1],
      libelle_parent = parent_row$libelle_appellation_metier[1],
      child_codes = as.character(child_rows$code_ogr),
      child_labels = as.character(child_rows$libelle_appellation_metier)
    )
  })
  
  output$family_preview <- renderUI({
    draft <- draft_family()
    
    if (is.null(draft)) {
      return(tags$p("Aucun parent selectionne."))
    }
    
    child_df <- child_df_from_vectors(draft$child_codes, draft$child_labels)
    base_family <- find_base_family_for_draft(draft, families_data())
    operation_label <- if (is.null(base_family)) "Creation" else "Modification"
    
    tags$div(
      tags$p(tags$strong("IDEP agent : "), ifelse(nzchar(draft$idep_agent), draft$idep_agent, "non renseigne")),
      tags$p(tags$strong("Type de proposition : "), operation_label),
      tags$p(tags$strong("Parent : "), draft$libelle_parent),
      tags$p(tags$strong("Code parent : "), draft$code_ogr_parent),
      tags$p(tags$strong("Code ROME parent : "), draft$code_rome_parent),
      if (!is.null(base_family)) {
        tags$p(tags$strong("Famille stock cible : "), base_family$id_famille[1])
      },
      tags$p(tags$strong("Nombre d'enfants : "), nrow(child_df)),
      if (nrow(child_df) == 0) {
        tags$p("Aucun enfant selectionne.")
      } else {
        tags$table(
          class = "table table-striped table-bordered preview-table",
          tags$thead(
            tags$tr(
              tags$th("Code OGR"),
              tags$th("Libelle enfant")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(child_df)), function(i) {
              tags$tr(
                tags$td(child_df$code_ogr[i]),
                tags$td(child_df$libelle[i])
              )
            })
          )
        )
      }
    )
  })
  
  output$table_ref <- renderDT({
    datatable(
      ref_ogr %>%
        select(
          code_ogr,
          code_rome,
          libelle_rome,
          libelle_domaine_professionnel,
          libelle_grand_domaine,
          libelle_appellation_metier,
          libelle_appellation_long,
          libelle_appellation_court
        ),
      filter = "top",
      selection = "multiple",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$table_families <- renderDT({
    datatable(
      families_view() %>%
        select(
          id_famille,
          idep_agent,
          code_rome_parent,
          code_ogr_parent,
          libelle_parent,
          doublon_parent,
          doublon_enfant,
          codes_enfants_en_doublon,
          codes_ogr_enfants,
          nb_enfants,
          date_creation,
          fichier_stockage
        ),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        "doublon_parent",
        backgroundColor = styleEqual(c("OUI", "NON"), c("#ffe3e0", "#e9f7ef")),
        color = styleEqual(c("OUI", "NON"), c("#a93226", "#1e8449")),
        fontWeight = styleEqual(c("OUI", "NON"), c("700", "500"))
      ) %>%
      formatStyle(
        "doublon_enfant",
        backgroundColor = styleEqual(c("OUI", "NON"), c("#fff1d6", "#eef7ff")),
        color = styleEqual(c("OUI", "NON"), c("#b45f06", "#1f5f8b")),
        fontWeight = styleEqual(c("OUI", "NON"), c("700", "500"))
      )
  })

  output$stock_families_notice <- renderUI({
    if (nrow(families_data()) > 0) {
      return(NULL)
    }

    tags$div(
      class = "app-card",
      tags$p(
        tags$strong("Aucune famille publiee pour le moment. "),
        "Le stock sera alimente apres publication d'une proposition en validation MOA."
      )
    )
  })
  
  output$table_proposals <- renderDT({
    datatable(
      proposals_data() %>%
        select(
          id_proposition,
          phase_proposition,
          statut_proposition,
          type_operation,
          idep_agent_edition,
          code_ogr_parent,
          libelle_parent,
          nb_enfants,
          base_famille_id,
          delta_enfants_ajoutes_edition,
          delta_enfants_retires_edition,
          decision_supervision,
          horodatage_edition
        ),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        "type_operation",
        backgroundColor = styleEqual(c("creation", "modification"), c("#eef7ff", "#fff1d6")),
        color = styleEqual(c("creation", "modification"), c("#1f5f8b", "#a75f00")),
        fontWeight = styleEqual(c("creation", "modification"), c("700", "700"))
      ) %>%
      formatStyle(
        "statut_proposition",
        backgroundColor = styleEqual(c("a_superviser", "en_cours", "rejetes", "a_valider"), c("#ffe9ef", "#eef7ff", "#fff1d6", "#e9f7ef")),
        color = styleEqual(c("a_superviser", "en_cours", "rejetes", "a_valider"), c("#a23b5a", "#1f5f8b", "#b45f06", "#1e8449")),
        fontWeight = styleEqual(c("a_superviser", "en_cours", "rejetes", "a_valider"), c("700", "700", "700", "700"))
      )
  })
  
  output$proposal_detail <- renderUI({
    proposal <- selected_proposal()
    
    if (is.null(proposal)) {
      return(tags$p("Selectionnez une proposition pour voir son detail."))
    }
    
    child_df <- child_df_from_vectors(
      split_pipe_values(proposal$codes_ogr_enfants[1]),
      split_pipe_values(proposal$libelles_enfants[1])
    )
    
    tags$div(
      class = "app-card",
      tags$p(tags$strong("ID proposition : "), proposal$id_proposition[1]),
      tags$p(tags$strong("Phase : "), proposal$phase_proposition[1]),
      tags$p(tags$strong("Statut : "), proposal$statut_proposition[1]),
      tags$p(tags$strong("Type : "), proposal$type_operation[1]),
      tags$p(tags$strong("Lignee : "), proposal$id_lignee[1]),
      if (nzchar(proposal$id_proposition_source[1])) {
        tags$p(tags$strong("Source : "), proposal$id_proposition_source[1])
      },
      tags$p(tags$strong("IDEP edition : "), proposal$idep_agent_edition[1]),
      tags$p(tags$strong("Horodatage edition : "), proposal$horodatage_edition[1]),
      tags$p(tags$strong("Famille stock cible : "), ifelse(nzchar(proposal$base_famille_id[1]), proposal$base_famille_id[1], "Aucune")),
      tags$p(tags$strong("Code parent : "), proposal$code_ogr_parent[1]),
      tags$p(tags$strong("Parent : "), proposal$libelle_parent[1]),
      if (nzchar(proposal$delta_enfants_ajoutes_edition[1])) {
        tags$p(tags$strong("Enfants ajoutes en edition : "), proposal$delta_enfants_ajoutes_edition[1])
      },
      if (nzchar(proposal$delta_enfants_retires_edition[1])) {
        tags$p(tags$strong("Enfants retires en edition : "), proposal$delta_enfants_retires_edition[1])
      },
      if (nzchar(proposal$decision_supervision[1])) {
        tags$p(tags$strong("Decision supervision : "), proposal$decision_supervision[1])
      },
      if (nzchar(proposal$commentaire_supervision[1])) {
        tags$p(tags$strong("Commentaire supervision : "), proposal$commentaire_supervision[1])
      },
      tags$h4("Contenu propose"),
      if (nrow(child_df) == 0) {
        tags$p("Aucun enfant.")
      } else {
        tags$table(
          class = "table table-striped table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Code OGR"),
              tags$th("Libelle")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(child_df)), function(i) {
              tags$tr(
                tags$td(child_df$code_ogr[i]),
                tags$td(child_df$libelle[i])
              )
            })
          )
        )
      }
    )
  })
  
  output$family_detail <- renderUI({
    family <- selected_family()
    
    if (nrow(families_data()) == 0) {
      return(tags$p("Aucune famille n'est encore publiee dans le stock."))
    }
    
    if (is.null(family)) {
      return(tags$p("Selectionnez une famille dans l'onglet des familles enregistrees."))
    }
    
    child_df <- child_df_from_vectors(
      split_pipe_values(family$codes_ogr_enfants[1]),
      split_pipe_values(family$libelles_enfants[1])
    )
    
    tags$div(
      class = "app-card",
      tags$p(tags$strong("ID : "), family$id_famille[1]),
      tags$p(tags$strong("IDEP agent : "), family$idep_agent[1]),
      tags$p(tags$strong("Fichier : "), family$fichier_stockage[1]),
      tags$p(tags$strong("Code ROME parent : "), family$code_rome_parent[1]),
      tags$p(tags$strong("Code parent : "), family$code_ogr_parent[1]),
      tags$p(tags$strong("Parent : "), family$libelle_parent[1]),
      tags$p(tags$strong("Doublon parent : "), family$doublon_parent[1]),
      tags$p(tags$strong("Doublon enfant : "), family$doublon_enfant[1]),
      if (nzchar(family$codes_enfants_en_doublon[1])) {
        tags$p(tags$strong("Codes enfants en doublon : "), family$codes_enfants_en_doublon[1])
      },
      tags$p(tags$strong("Date creation : "), family$date_creation[1]),
      tags$h4("Enfants"),
      if (nrow(child_df) == 0) {
        tags$p("Aucun enfant.")
      } else {
        tags$table(
          class = "table table-striped table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Code OGR"),
              tags$th("Libelle")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(child_df)), function(i) {
              tags$tr(
                tags$td(child_df$code_ogr[i]),
                tags$td(child_df$libelle[i])
              )
            })
          )
        )
      }
    )
  })
  
  observeEvent(input$apply_recommended_parent, {
    reco <- selection_recommendations()
    
    if (!nzchar(reco$suggested_parent_code)) {
      showNotification("Aucune preconisation unique a appliquer sur cette selection.", type = "warning")
      return()
    }
    
    updateSelectizeInput(
      session = session,
      inputId = "parent_code",
      selected = reco$suggested_parent_code,
      server = TRUE
    )
  })
  
  observeEvent(input$show_recommendation_detail, {
    reco <- selection_recommendations()
    
    if (reco$selection_size == 0) {
      showNotification("Selectionnez d'abord quelques lignes dans le referentiel.", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "Detail d'aide a la preconisation",
      tags$div(
        class = "reco-detail-wrap",
        uiOutput("recommendation_detail_summary"),
        tags$div(
          class = "reco-section",
          tags$div(class = "reco-section-title", "Lecture rapide des codes selectionnes"),
          tags$div(
            class = "reco-section-help",
            "Une ligne par code OGR, avec une lecture courte de ce qu'on voit pour choisir le parent."
          ),
          DTOutput("recommendation_code_table")
        ),
        tags$div(
          class = "reco-section",
          tags$div(class = "reco-section-title", "Contexte ROME"),
          tags$div(
            class = "reco-section-help",
            "Pour chaque code ROME touche par ta selection, tu vois les principales et synonymes disponibles autour."
          ),
          uiOutput("recommendation_rome_cards")
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  show_family_simulation <- function(family) {
    if (is.null(family)) {
      showNotification("Choisissez un parent puis des enfants pour lancer la simulation.", type = "warning")
      return(invisible(NULL))
    }
    
    if (!has_plotly) {
      showModal(modalDialog(
        title = "Simulation de filiation",
        p("Le package 'plotly' n'est pas installe sur cette machine."),
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
      return(invisible(NULL))
    }
    
    if (is.list(family)) {
      parent_label <- family$libelle_parent
      parent_code <- family$code_ogr_parent
      child_labels <- family$child_labels
      child_codes <- family$child_codes
      family_title <- family$id_famille
    } else {
      parent_label <- family$libelle_parent[1]
      parent_code <- family$code_ogr_parent[1]
      child_labels <- split_pipe_values(family$libelles_enfants[1])
      child_codes <- split_pipe_values(family$codes_ogr_enfants[1])
      family_title <- family$id_famille[1]
    }
    
    max_len <- max(length(child_labels), length(child_codes))
    if (max_len == 0) {
      child_labels <- character(0)
      child_codes <- character(0)
    } else {
      length(child_labels) <- max_len
      length(child_codes) <- max_len
      child_labels[is.na(child_labels)] <- ""
      child_codes[is.na(child_codes)] <- ""
    }
    
    output$family_plot <- plotly::renderPlotly({
      if (length(child_labels) == 0) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              title = "Aucun enfant a afficher",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              paper_bgcolor = "#f7f9fd",
              plot_bgcolor = "#f7f9fd"
            )
        )
      }
      
      labels <- c(parent_label, child_labels)
      codes <- c(parent_code, child_codes)
      child_palette <- rep_len(
        c("#FF6B6B", "#FFD166", "#06D6A0", "#4D96FF", "#9B5DE5", "#F15BB5", "#00BBF9", "#F9844A"),
        length(child_labels)
      )
      node_colors <- c("#003049", child_palette)
      link_colors <- c("rgba(0, 48, 73, 0.55)", hex_to_rgba(child_palette, alpha = 0.42))
      custom_text <- c(
        paste0("<b>Parent</b><br>", parent_label, "<br>Code OGR : ", parent_code),
        paste0("<b>Enfant</b><br>", child_labels, "<br>Code OGR : ", child_codes)
      )
      node_x <- c(0.12, rep(0.84, length(child_labels)))
      node_y <- c(
        0.5,
        if (length(child_labels) == 1) {
          0.5
        } else {
          seq(0.08, 0.92, length.out = length(child_labels))
        }
      )
      
      plotly::plot_ly(
        type = "sankey",
        arrangement = "fixed",
        node = list(
          pad = 26,
          thickness = 28,
          line = list(color = "#ffffff", width = 2),
          label = labels,
          color = node_colors,
          x = node_x,
          y = node_y,
          customdata = custom_text,
          hovertemplate = "%{customdata}<extra></extra>"
        ),
        link = list(
          source = rep(0, length(child_labels)),
          target = seq_along(child_labels),
          value = rep(1, length(child_labels)),
          color = link_colors[-1],
          hovertemplate = paste0(
            "<b>Flux de filiation</b><br>",
            parent_label,
            " -> %{target.label}<extra></extra>"
          )
        )
      ) %>%
        plotly::layout(
          title = list(text = paste("Simulation de filiation -", family_title)),
          font = list(family = "Arial", size = 14, color = "#17324d"),
          paper_bgcolor = "#fffdf8",
          plot_bgcolor = "#fffdf8",
          margin = list(l = 20, r = 20, t = 78, b = 24),
          annotations = list(
            list(
              x = 0.01,
              y = 1.12,
              xref = "paper",
              yref = "paper",
              text = paste0("Parent : <b>", parent_label, "</b>"),
              showarrow = FALSE,
              align = "left",
              font = list(size = 13, color = "#52627c")
            ),
            list(
              x = 0.01,
              y = 1.05,
              xref = "paper",
              yref = "paper",
              text = paste0("Enfants relies : <b>", length(child_labels), "</b>"),
              showarrow = FALSE,
              align = "left",
              font = list(size = 12, color = "#7a5c00")
            )
          )
        )
    })
    
    showModal(modalDialog(
      title = "Simulation de filiation",
      plotly::plotlyOutput("family_plot", height = "620px"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  }
  
  save_draft_proposal <- function(draft) {
    create_proposal(
      draft = draft,
      stock_families = families_data(),
      phase = "supervision",
      statut = "a_superviser",
      origin_phase = "edition"
    )
    
    refresh_token(refresh_token() + 1)
    
    updateSelectizeInput(
      session = session,
      inputId = "parent_code",
      selected = character(0),
      server = TRUE
    )
    updateSelectizeInput(
      session = session,
      inputId = "child_codes",
      selected = character(0),
      server = TRUE
    )
    
    showNotification("Proposition creee et deposee dans la file de supervision.", type = "message")
  }
  
  observeEvent(input$simulate_draft, {
    show_family_simulation(draft_family())
  })
  
  observeEvent(input$create_proposal, {
    draft <- draft_family()
    stock_families <- families_data()
    existing_proposals <- proposals_data() %>%
      filter(statut_proposition != "rejetes")
    
    if (!nzchar(trimws(input$idep_agent))) {
      showNotification("Renseignez l'IDEP agent.", type = "warning")
      return()
    }
    
    if (is.null(draft) || !nzchar(draft$code_ogr_parent)) {
      showNotification("Choisissez un parent.", type = "warning")
      return()
    }
    
    if (length(draft$child_labels) == 0) {
      showNotification("Choisissez au moins un enfant.", type = "warning")
      return()
    }
    
    duplicate_stock <- find_duplicate_family(draft, stock_families)
    if (!is.null(duplicate_stock)) {
      showModal(modalDialog(
        title = "Famille deja existante",
        p("Cette combinaison parent / enfants existe deja dans le stock."),
        p(paste("Famille stock :", duplicate_stock$id_famille[1])),
        p(paste("Parent :", duplicate_stock$libelle_parent[1])),
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
      return()
    }
    
    duplicate_proposal <- find_duplicate_family(draft, project_proposals_as_families(existing_proposals))
    if (!is.null(duplicate_proposal)) {
      showModal(modalDialog(
        title = "Proposition deja existante",
        p("Cette combinaison parent / enfants existe deja dans le workflow."),
        p(paste("Proposition :", duplicate_proposal$id_famille[1])),
        p(paste("Parent :", duplicate_proposal$libelle_parent[1])),
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
      return()
    }
    
    child_conflicts <- find_child_conflicts(draft, comparable_existing_filiations())
    if (nrow(child_conflicts) > 0) {
      pending_conflict_draft(draft)
      
      showModal(modalDialog(
        title = "Enfants deja affilies",
        p("Certains enfants sont deja rattaches a une autre famille ou deja presents dans une proposition."),
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
            lapply(seq_len(min(nrow(child_conflicts), 10)), function(i) {
              tags$tr(
                tags$td(child_conflicts$child_code[i]),
                tags$td(child_conflicts$child_label[i]),
                tags$td(child_conflicts$family_id[i]),
                tags$td(child_conflicts$family_parent[i])
              )
            })
          )
        ),
        p("Vous pouvez annuler ou enregistrer quand meme."),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Annuler"),
          actionButton("confirm_conflict_save", "Enregistrer quand meme", class = "btn-warning")
        )
      ))
      return()
    }
    
    save_draft_proposal(draft)
  })
  
  observeEvent(input$confirm_conflict_save, {
    draft <- pending_conflict_draft()
    if (is.null(draft)) {
      return()
    }
    
    removeModal()
    pending_conflict_draft(NULL)
    save_draft_proposal(draft)
  })
  
  observeEvent(input$delete_family, {
    family <- selected_family()
    
    if (is.null(family)) {
      showNotification("Selectionnez une famille a supprimer.", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "Supprimer la famille",
      p("Voulez-vous vraiment supprimer cette famille ?"),
      p(paste("ID :", family$id_famille[1])),
      p(paste("Parent :", family$libelle_parent[1])),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_delete_family", "Supprimer", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_family, {
    family <- selected_family()
    if (is.null(family)) {
      removeModal()
      return()
    }
    
    delete_family_by_id(family$id_famille[1])
    removeModal()
    refresh_token(refresh_token() + 1)
    showNotification("Famille supprimee.", type = "message")
  })
  
  observeEvent(input$delete_proposal, {
    proposal <- selected_proposal()
    
    if (is.null(proposal)) {
      showNotification("Selectionnez une proposition a supprimer.", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "Supprimer la proposition",
      p("Voulez-vous vraiment supprimer cette proposition du workflow ?"),
      p(paste("ID :", proposal$id_proposition[1])),
      p(paste("Parent :", proposal$libelle_parent[1])),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_delete_proposal", "Supprimer", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_proposal, {
    proposal <- selected_proposal()
    if (is.null(proposal)) {
      removeModal()
      return()
    }
    
    delete_proposal_by_id(proposal$id_proposition[1], phase = proposal$phase_proposition[1])
    removeModal()
    refresh_token(refresh_token() + 1)
    showNotification("Proposition supprimee.", type = "message")
  })
}

