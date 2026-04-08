supervision_ui <- function() {
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
        .supervision-section-title {
          font-size: 16px;
          font-weight: 700;
          color: #17324d;
          margin-bottom: 10px;
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
          div(class = "supervision-section-title", "Prise en charge"),
          uiOutput("supervision_action_context"),
          textInput("supervision_agent", "IDEP supervision", value = "DFEC5Z"),
          textAreaInput("supervision_comment", "Commentaire supervision", rows = 4, placeholder = "Motiver la decision, la modification ou le rejet."),
          textInput("reprise_idep_agent", "IDEP reprise edition", value = "DFEC5Z"),
          div(class = "supervision-note", "La supervision peut prendre en charge, modifier, rejeter ou recreer une reprise sans effacer le fichier source."),
          uiOutput("supervision_action_panel")
        ),
        div(
          class = "supervision-card",
          div(class = "supervision-section-title", "Ajustement supervision"),
          uiOutput("supervision_edit_state"),
          selectizeInput(
            "supervision_parent_code",
            "Parent supervision",
            choices = character(0),
            selected = NULL,
            options = list(placeholder = "Choisir le parent pour la supervision")
          ),
          selectizeInput(
            "supervision_child_codes",
            "Enfants supervision",
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
              br(),
              DTOutput("table_supervision_queue")
            ),
            tabPanel(
              "En cours",
              br(),
              DTOutput("table_supervision_in_progress")
            ),
            tabPanel(
              "Rejetes",
              br(),
              DTOutput("table_supervision_rejected")
            )
          )
        ),
        div(
          class = "supervision-card",
          div(class = "supervision-section-title", "Detail de la proposition"),
          uiOutput("supervision_detail")
        )
      )
    )
  )
}
