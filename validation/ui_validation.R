validation_ui <- function() {
  inline_info <- function(text) {
    tags$span(
      class = "validation-inline-info",
      tabindex = "0",
      `data-tooltip` = text,
      `aria-label` = text,
      "i"
    )
  }

  label_with_info <- function(label, info_text) {
    tags$span(
      class = "validation-label-with-info",
      span(label),
      inline_info(info_text)
    )
  }

  section_title_with_info <- function(title, info_text) {
    div(
      class = "validation-section-head",
      div(class = "validation-section-title", title),
      inline_info(info_text)
    )
  }

  tagList(
    tags$head(
      tags$style(HTML("
        .validation-grid {
          display: grid;
          grid-template-columns: 380px minmax(0, 1fr);
          gap: 18px;
        }
        .validation-card {
          background: #ffffff;
          border: 1px solid #d9e2ee;
          border-radius: 18px;
          padding: 18px;
          box-shadow: 0 12px 24px rgba(23, 43, 77, 0.08);
          margin-bottom: 18px;
        }
        .validation-kpis {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
          gap: 12px;
          margin-bottom: 18px;
        }
        .validation-kpi-section {
          background: #ffffff;
          border: 1px solid #d9e2ee;
          border-radius: 18px;
          padding: 18px;
          box-shadow: 0 12px 24px rgba(23, 43, 77, 0.08);
          margin-bottom: 18px;
        }
        .validation-kpi-section-title {
          font-size: 16px;
          font-weight: 700;
          color: #17324d;
          margin-bottom: 4px;
        }
        .validation-kpi-section-copy {
          font-size: 12px;
          color: #6f819a;
          margin-bottom: 14px;
        }
        .validation-kpi {
          background: linear-gradient(160deg, #ffffff 0%, #f3f7fd 100%);
          border: 1px solid #d8e2f0;
          border-radius: 16px;
          padding: 14px;
        }
        .validation-kpi-label {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: #60708f;
          margin-bottom: 8px;
        }
        .validation-kpi-value {
          font-size: 28px;
          font-weight: 700;
          color: #17324d;
          line-height: 1;
          margin-bottom: 8px;
        }
        .validation-kpi-help {
          font-size: 12px;
          color: #6f819a;
        }
        .validation-actions {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
          margin-top: 10px;
        }
        .validation-status-banner {
          border-radius: 16px;
          padding: 14px 16px;
          margin-bottom: 14px;
          border: 1px solid #d9e2ee;
          background: linear-gradient(160deg, #f8fbff 0%, #eef4fb 100%);
        }
        .validation-status-banner.is-queue {
          background: linear-gradient(160deg, #eef8ff 0%, #e3f0ff 100%);
          border-color: #cfe1f7;
        }
        .validation-status-banner.is-progress {
          background: linear-gradient(160deg, #ecfff4 0%, #e0f7eb 100%);
          border-color: #cbe9d5;
        }
        .validation-status-banner.is-waiting {
          background: linear-gradient(160deg, #fff8eb 0%, #fff0d9 100%);
          border-color: #f3ddab;
        }
        .validation-status-banner.is-rejected {
          background: linear-gradient(160deg, #fff4f7 0%, #ffe9ef 100%);
          border-color: #f2cad5;
        }
        .validation-status-banner.is-published {
          background: linear-gradient(160deg, #eef7ff 0%, #e8f2ff 100%);
          border-color: #cfdcf5;
        }
        .validation-status-kicker {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: #60708f;
          margin-bottom: 6px;
        }
        .validation-status-title {
          font-size: 18px;
          font-weight: 700;
          color: #17324d;
          margin-bottom: 6px;
        }
        .validation-status-copy {
          font-size: 13px;
          color: #5f718d;
          line-height: 1.6;
        }
        .validation-note {
          font-size: 12px;
          color: #6d7d94;
          line-height: 1.5;
        }
        .validation-section-head {
          display: flex;
          align-items: center;
          gap: 8px;
          margin-bottom: 10px;
          flex-wrap: wrap;
        }
        .validation-section-title {
          font-size: 16px;
          font-weight: 700;
          color: #17324d;
          margin-bottom: 10px;
        }
        .validation-section-head .validation-section-title {
          margin-bottom: 0;
        }
        .validation-label-with-info {
          display: inline-flex;
          align-items: center;
          gap: 6px;
        }
        .validation-inline-info {
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
        .validation-inline-info::after {
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
        .validation-inline-info::before {
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
        .validation-inline-info:hover::after,
        .validation-inline-info:hover::before,
        .validation-inline-info:focus::after,
        .validation-inline-info:focus::before {
          opacity: 1;
          visibility: visible;
        }
        .validation-inline-info:focus {
          outline: 2px solid #8cb6dd;
          outline-offset: 2px;
        }
        .validation-detail-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 12px;
        }
        .validation-detail-box {
          background: #f8fbff;
          border: 1px solid #d8e2f0;
          border-radius: 14px;
          padding: 12px;
        }
        .validation-detail-label {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: #60708f;
          margin-bottom: 6px;
        }
        .validation-detail-value {
          font-size: 14px;
          color: #17324d;
          line-height: 1.5;
          word-break: break-word;
        }
        .validation-impact {
          font-size: 13px;
          color: #5f718d;
          line-height: 1.6;
        }
        .validation-tab-note {
          font-size: 12px;
          color: #6d7d94;
          margin-bottom: 12px;
        }
        .validation-subpanel-tabs > .nav {
          display: flex;
          gap: 8px;
          flex-wrap: wrap;
          border-bottom: none;
          margin: 4px 0 14px;
        }
        .validation-subpanel-tabs > .nav > li {
          float: none;
          margin: 0;
        }
        .validation-subpanel-tabs > .nav > li > a {
          border: 1px solid #d6e1ef;
          border-radius: 999px;
          background: #f7f9fc;
          color: #35506b;
          font-size: 12px;
          font-weight: 700;
          padding: 8px 14px;
        }
        .validation-subpanel-tabs > .nav > li > a:hover {
          background: #eef3f9;
          border-color: #c4d4e8;
        }
        .validation-subpanel-tabs > .nav > li.active > a,
        .validation-subpanel-tabs > .nav > li.active > a:hover,
        .validation-subpanel-tabs > .nav > li.active > a:focus {
          background: #17324d;
          border-color: #17324d;
          color: #ffffff;
        }
        .validation-subpanel-content {
          min-height: 180px;
        }
        .validation-subpanel-content.is-kpi {
          min-height: 0;
        }
        .validation-tab-intro {
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
        .validation-dt .dt-buttons {
          margin-bottom: 10px;
        }
        .validation-dt .dt-button {
          border: 1px solid #d6e1ef !important;
          border-radius: 999px !important;
          background: #f7f9fc !important;
          color: #35506b !important;
          font-size: 12px !important;
          font-weight: 700 !important;
          padding: 7px 12px !important;
        }
        .validation-dt .dt-button:hover {
          background: #eef3f9 !important;
          border-color: #c4d4e8 !important;
        }
        @media (max-width: 1240px) {
          .validation-grid {
            grid-template-columns: 1fr;
          }
          .validation-kpis {
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
          }
        }
        @media (max-width: 760px) {
          .validation-kpis {
            grid-template-columns: 1fr;
          }
          .validation-detail-grid {
            grid-template-columns: 1fr;
          }
        }
      "))
    ),
    uiOutput("validation_kpi_panel"),
    div(
      class = "validation-grid",
      div(
        div(
          class = "validation-card",
          div(
            class = "validation-subpanel-tabs",
            tabsetPanel(
              id = "validation_left_panel",
              type = "tabs",
              tabPanel(
                "Decision MOA",
                div(
                  class = "validation-subpanel-content",
                  section_title_with_info("Decision MOA", "Cette zone suit le cycle MOA : prise en charge, attente, rejet ou publication dans le stock."),
                  uiOutput("validation_action_context"),
                  textInput(
                    "validation_agent",
                    label_with_info("IDEP validation", "Cet identifiant trace le valideur MOA qui prend la decision sur le dossier."),
                    value = "DFEC5Z"
                  ),
                  textAreaInput(
                    "validation_comment",
                    label_with_info("Commentaire validation", "Utile pour expliquer une mise en attente, un rejet ou un arbitrage avant publication."),
                    rows = 4,
                    placeholder = "Motiver la mise en attente, le rejet ou la validation."
                  ),
                  div(class = "validation-note", "La MOA peut prendre en charge, mettre en attente, rejeter ou publier la proposition dans le stock et le referentiel EDEP."),
                  uiOutput("validation_action_panel")
                )
              ),
              tabPanel(
                "Impact de publication",
                div(
                  class = "validation-subpanel-content",
                  section_title_with_info("Impact de publication", "Ce bloc aide a comprendre ce qui sera cree ou remplace dans le stock et dans le referentiel EDEP."),
                  uiOutput("validation_publish_preview")
                )
              )
            )
          )
        )
      ),
      div(
        div(
          class = "validation-card",
          tabsetPanel(
            id = "validation_status_tab",
            type = "tabs",
            tabPanel(
              "A valider",
              div(
                class = "validation-tab-intro",
                span("Vue compacte par defaut pour faciliter l'instruction MOA."),
                inline_info("Les colonnes purement validation sont masquees tant que le dossier est juste en attente de prise en charge. Utilise Choisir les colonnes pour afficher plus de details.")
              ),
              div(class = "validation-dt", DTOutput("table_validation_queue"))
            ),
            tabPanel(
              "En cours",
              div(
                class = "validation-tab-intro",
                span("Le dossier est deja ouvert cote MOA."),
                inline_info("La vue montre davantage d'informations de validation, et tu peux personnaliser les colonnes affichees.")
              ),
              div(class = "validation-dt", DTOutput("table_validation_in_progress"))
            ),
            tabPanel(
              "En attente",
              div(
                class = "validation-tab-intro",
                span("Les dossiers temporises restent consultables."),
                inline_info("Pratique pour reprendre un dossier plus tard sans perdre le contexte de validation.")
              ),
              div(class = "validation-dt", DTOutput("table_validation_waiting"))
            ),
            tabPanel(
              "Rejetes",
              div(
                class = "validation-tab-intro",
                span("Les rejets MOA restent visibles pour relecture."),
                inline_info("Utilise Choisir les colonnes si tu veux reafficher plus d'informations sur la decision prise.")
              ),
              div(class = "validation-dt", DTOutput("table_validation_rejected"))
            ),
            tabPanel(
              "Validees",
              div(
                class = "validation-tab-intro",
                span("Cette file montre les propositions deja publiees."),
                inline_info("Tu peux comparer la decision finale et le moment ou la publication a ete actee.")
              ),
              div(class = "validation-dt", DTOutput("table_validation_validated"))
            ),
            tabPanel(
              "Stock familles",
              div(
                class = "validation-tab-intro",
                span("Le stock recense les familles deja publiees."),
                inline_info("Cette vue sert a verifier l'etat publie actuel, distinct des dossiers encore en cours de validation.")
              ),
              div(class = "validation-dt", DTOutput("table_validation_stock"))
            ),
            tabPanel(
              "Referentiel EDEP",
              div(
                class = "validation-tab-intro",
                span("Vue maison parent -> enfant publiee par la MOA."),
                inline_info("Cette table montre le referentiel aval alimente par les publications MOA.")
              ),
              div(class = "validation-dt", DTOutput("table_validation_edep"))
            )
          )
        ),
        div(
          class = "validation-card",
          section_title_with_info("Detail validation", "Ce bloc resume le dossier, la decision courante et le contenu qui sera potentiellement publie."),
          uiOutput("validation_detail")
        )
      )
    )
  )
}
