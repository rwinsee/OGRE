supervision_ui <- function() {
  inline_info <- function(text) {
    tags$span(
      class = "supervision-inline-info",
      tabindex = "0",
      `data-tooltip` = text,
      `aria-label` = text,
      "i"
    )
  }

  label_with_info <- function(label, info_text) {
    tags$span(
      class = "supervision-label-with-info",
      span(label),
      inline_info(info_text)
    )
  }

  section_title_with_info <- function(title, info_text) {
    div(
      class = "supervision-section-head",
      div(class = "supervision-section-title", title),
      inline_info(info_text)
    )
  }

  tagList(
    tags$head(
      tags$style(HTML("
        .supervision-grid {
          display: grid;
          grid-template-columns: 380px minmax(0, 1fr);
          gap: 18px;
        }
        .supervision-card {
          background: #ffffff;
          border: 1px solid #d9e2ee;
          border-radius: 18px;
          padding: 18px;
          box-shadow: 0 12px 24px rgba(23, 43, 77, 0.08);
          margin-bottom: 18px;
        }
        .supervision-kpis {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
          gap: 12px;
          margin-bottom: 18px;
        }
        .supervision-kpi {
          background: linear-gradient(160deg, #ffffff 0%, #f3f7fd 100%);
          border: 1px solid #d8e2f0;
          border-radius: 16px;
          padding: 14px;
        }
        .supervision-kpi-label {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: #60708f;
          margin-bottom: 8px;
        }
        .supervision-kpi-value {
          font-size: 28px;
          font-weight: 700;
          color: #17324d;
          line-height: 1;
          margin-bottom: 8px;
        }
        .supervision-kpi-help {
          font-size: 12px;
          color: #6f819a;
        }
        .supervision-actions {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
          margin-top: 10px;
        }
        .supervision-status-banner {
          border-radius: 16px;
          padding: 14px 16px;
          margin-bottom: 14px;
          border: 1px solid #d9e2ee;
          background: linear-gradient(160deg, #f8fbff 0%, #eef4fb 100%);
        }
        .supervision-status-banner.is-queue {
          background: linear-gradient(160deg, #fff4f7 0%, #ffe9ef 100%);
          border-color: #f2cad5;
        }
        .supervision-status-banner.is-progress {
          background: linear-gradient(160deg, #eef7ff 0%, #e3f0ff 100%);
          border-color: #cfe1f7;
        }
        .supervision-status-banner.is-rejected {
          background: linear-gradient(160deg, #fff6e8 0%, #fff0d9 100%);
          border-color: #f3ddab;
        }
        .supervision-status-kicker {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: #60708f;
          margin-bottom: 6px;
        }
        .supervision-status-title {
          font-size: 18px;
          font-weight: 700;
          color: #17324d;
          margin-bottom: 6px;
        }
        .supervision-status-copy {
          font-size: 13px;
          color: #5f718d;
          line-height: 1.6;
        }
        .supervision-note {
          font-size: 12px;
          color: #6d7d94;
          line-height: 1.5;
        }
        .supervision-section-head {
          display: flex;
          align-items: center;
          gap: 8px;
          margin-bottom: 10px;
          flex-wrap: wrap;
        }
        .supervision-section-title {
          font-size: 16px;
          font-weight: 700;
          color: #17324d;
          margin-bottom: 10px;
        }
        .supervision-section-head .supervision-section-title {
          margin-bottom: 0;
        }
        .supervision-label-with-info {
          display: inline-flex;
          align-items: center;
          gap: 6px;
        }
        .supervision-inline-info {
          position: relative;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 18px;
          height: 18px;
          border-radius: 999px;
          background: #17324d;
          color: #ffffff;
          font-size: 11px;
          font-weight: 700;
          line-height: 1;
          cursor: help;
          flex: 0 0 auto;
        }
        .supervision-inline-info::after {
          content: attr(data-tooltip);
          position: absolute;
          left: 50%;
          top: calc(100% + 10px);
          transform: translateX(-50%);
          min-width: 220px;
          max-width: 280px;
          padding: 10px 12px;
          border-radius: 12px;
          background: #17324d;
          color: #ffffff;
          font-size: 12px;
          line-height: 1.45;
          white-space: normal;
          box-shadow: 0 14px 26px rgba(23, 43, 77, 0.18);
          opacity: 0;
          visibility: hidden;
          pointer-events: none;
          transition: opacity 0.12s ease;
          z-index: 20;
        }
        .supervision-inline-info::before {
          content: '';
          position: absolute;
          left: 50%;
          top: calc(100% + 4px);
          transform: translateX(-50%);
          border-left: 6px solid transparent;
          border-right: 6px solid transparent;
          border-bottom: 6px solid #17324d;
          opacity: 0;
          visibility: hidden;
          transition: opacity 0.12s ease;
          z-index: 21;
        }
        .supervision-inline-info:hover::after,
        .supervision-inline-info:hover::before,
        .supervision-inline-info:focus::after,
        .supervision-inline-info:focus::before {
          opacity: 1;
          visibility: visible;
        }
        .supervision-inline-info:focus {
          outline: 2px solid #8cb6dd;
          outline-offset: 2px;
        }
        .supervision-tab-intro {
          display: flex;
          align-items: center;
          justify-content: flex-start;
          gap: 8px;
          flex-wrap: wrap;
          margin: 12px 0;
          color: #5f7793;
          font-size: 12px;
          line-height: 1.5;
        }
        .supervision-dt .dt-buttons {
          margin-bottom: 10px;
        }
        .supervision-dt .dt-button {
          border: 1px solid #d6e1ef !important;
          border-radius: 999px !important;
          background: #f7f9fc !important;
          color: #35506b !important;
          font-size: 12px !important;
          font-weight: 700 !important;
          padding: 7px 12px !important;
        }
        .supervision-dt .dt-button:hover {
          background: #eef3f9 !important;
          border-color: #c4d4e8 !important;
        }
        .supervision-detail-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 12px;
        }
        .supervision-detail-box {
          background: #f8fbff;
          border: 1px solid #d8e2f0;
          border-radius: 14px;
          padding: 12px;
        }
        .supervision-detail-label {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: #60708f;
          margin-bottom: 6px;
        }
        .supervision-detail-value {
          font-size: 14px;
          color: #17324d;
          line-height: 1.5;
          word-break: break-word;
        }
        .supervision-edit-help {
          font-size: 13px;
          color: #5f718d;
          line-height: 1.5;
          margin-bottom: 12px;
        }
        @media (max-width: 1240px) {
          .supervision-grid {
            grid-template-columns: 1fr;
          }
          .supervision-kpis {
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
          }
        }
        @media (max-width: 760px) {
          .supervision-kpis {
            grid-template-columns: 1fr;
          }
          .supervision-detail-grid {
            grid-template-columns: 1fr;
          }
        }
      "))
    ),
    fluidRow(
      column(
        width = 12,
        uiOutput("supervision_kpi_panel")
      )
    ),
    div(
      class = "supervision-grid",
      div(
        div(
          class = "supervision-card",
          section_title_with_info("Prise en charge", "Cette zone change selon l'etat du dossier : prise en charge, decision de supervision ou reprise d'edition."),
          uiOutput("supervision_action_context"),
          textInput(
            "supervision_agent",
            label_with_info("IDEP supervision", "Cet identifiant trace le superviseur qui prend en charge ou tranche le dossier."),
            value = "DFEC5Z"
          ),
          textAreaInput(
            "supervision_comment",
            label_with_info("Commentaire supervision", "Utile pour motiver une modification, un rejet ou une consigne transmise a la MOA."),
            rows = 4,
            placeholder = "Motiver la decision, la modification ou le rejet."
          ),
          textInput(
            "reprise_idep_agent",
            label_with_info("IDEP reprise edition", "Utilise cet identifiant quand tu recrees une reprise vers l'edition apres rejet."),
            value = "DFEC5Z"
          ),
          div(class = "supervision-note", "La supervision peut prendre en charge, modifier, rejeter ou recreer une reprise sans effacer le fichier source."),
          uiOutput("supervision_action_panel")
        ),
        div(
          class = "supervision-card",
          section_title_with_info("Ajustement supervision", "Cette zone sert a corriger le parent ou les enfants avant envoi en validation MOA."),
          uiOutput("supervision_edit_state"),
          selectizeInput(
            "supervision_parent_code",
            label_with_info("Parent supervision", "Choisis ici le parent retenu apres arbitrage supervision."),
            choices = character(0),
            selected = NULL,
            options = list(placeholder = "Choisir le parent pour la supervision")
          ),
          selectizeInput(
            "supervision_child_codes",
            label_with_info("Enfants supervision", "Garde uniquement les enfants a transmettre dans la version supervision."),
            choices = character(0),
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Choisir les enfants pour la supervision")
          ),
          uiOutput("supervision_edit_preview")
        )
      ),
      div(
        div(
          class = "supervision-card",
          tabsetPanel(
            id = "supervision_status_tab",
            type = "tabs",
            tabPanel(
              "A superviser",
              div(
                class = "supervision-tab-intro",
                span("Vue compacte par defaut pour faciliter la prise en main."),
                inline_info("Les colonnes supervision sont masquees tant que le dossier est juste en attente. Utilise le bouton Choisir les colonnes pour afficher plus de details.")
              ),
              div(class = "supervision-dt", DTOutput("table_supervision_queue"))
            ),
            tabPanel(
              "En cours",
              div(
                class = "supervision-tab-intro",
                span("Ici, le dossier est deja pris en charge."),
                inline_info("La vue montre davantage d'informations supervision, et tu peux toujours personnaliser l'affichage des colonnes.")
              ),
              div(class = "supervision-dt", DTOutput("table_supervision_in_progress"))
            ),
            tabPanel(
              "Rejetes",
              div(
                class = "supervision-tab-intro",
                span("Les rejets restent consultables pour comprendre la decision."),
                inline_info("Le bouton Choisir les colonnes permet aussi de reafficher certains champs si tu veux analyser un cas plus finement.")
              ),
              div(class = "supervision-dt", DTOutput("table_supervision_rejected"))
            )
          )
        ),
        div(
          class = "supervision-card",
          section_title_with_info("Detail de la proposition", "Ce bloc resume l'etat courant du dossier et ce qui a deja ete decide ou modifie."),
          uiOutput("supervision_detail")
        )
      )
    )
  )
}
