validation_ui <- function() {
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
        .validation-section-title {
          font-size: 16px;
          font-weight: 700;
          color: #17324d;
          margin-bottom: 10px;
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
          div(class = "validation-section-title", "Decision MOA"),
          uiOutput("validation_action_context"),
          textInput("validation_agent", "IDEP validation", value = "DFEC5Z"),
          textAreaInput("validation_comment", "Commentaire validation", rows = 4, placeholder = "Motiver la mise en attente, le rejet ou la validation."),
          div(class = "validation-note", "La MOA peut prendre en charge, mettre en attente, rejeter ou publier la proposition dans le stock et le referentiel EDEP."),
          uiOutput("validation_action_panel")
        ),
        div(
          class = "validation-card",
          div(class = "validation-section-title", "Impact de publication"),
          uiOutput("validation_publish_preview")
        )
      ),
      div(
        div(
          class = "validation-card",
          tabsetPanel(
            id = "validation_status_tab",
            type = "tabs",
            tabPanel("A valider", br(), DTOutput("table_validation_queue")),
            tabPanel("En cours", br(), DTOutput("table_validation_in_progress")),
            tabPanel("En attente", br(), DTOutput("table_validation_waiting")),
            tabPanel("Rejetes", br(), DTOutput("table_validation_rejected")),
            tabPanel("Validees", br(), DTOutput("table_validation_validated")),
            tabPanel("Stock familles", br(), DTOutput("table_validation_stock")),
            tabPanel(
              "Referentiel EDEP",
              br(),
              div(class = "validation-tab-note", "Vue maison parent -> enfant publiee par la MOA."),
              DTOutput("table_validation_edep")
            )
          )
        ),
        div(
          class = "validation-card",
          div(class = "validation-section-title", "Detail validation"),
          uiOutput("validation_detail")
        )
      )
    )
  )
}
