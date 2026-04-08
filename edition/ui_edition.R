edition_ui <- function() {
  tagList(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(180deg, #f5f7fb 0%, #eef2f9 100%);
      }
      .app-card {
        background: #ffffff;
        border: 1px solid #d9e1ef;
        border-radius: 18px;
        box-shadow: 0 10px 24px rgba(23, 43, 77, 0.08);
        padding: 18px;
        margin-bottom: 18px;
      }
      .action-row {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 8px;
      }
      .preview-table {
        margin-top: 10px;
      }
      .preview-table th {
        background: #f4f7fc;
      }
      .reco-box {
        background: linear-gradient(160deg, #f8fbff 0%, #eef4fb 100%);
        border: 1px solid #d8e2f0;
        border-radius: 14px;
        padding: 14px;
      }
      .reco-line {
        color: #30445f;
        margin-bottom: 8px;
      }
      .reco-label {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #60708f;
        margin-bottom: 6px;
      }
      .reco-value {
        font-weight: 700;
        color: #15314b;
      }
      .reco-chip {
        display: inline-block;
        background: #ffffff;
        border: 1px solid #cfdae9;
        border-radius: 999px;
        padding: 6px 10px;
        margin: 0 8px 8px 0;
        color: #28415c;
      }
      .reco-detail-wrap {
        display: flex;
        flex-direction: column;
        gap: 16px;
      }
      .reco-summary-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 10px;
      }
      .reco-summary-card {
        background: linear-gradient(160deg, #fff9ef 0%, #fff3dc 100%);
        border: 1px solid #f3ddab;
        border-radius: 14px;
        padding: 12px;
      }
      .reco-summary-card.alt {
        background: linear-gradient(160deg, #eef8ff 0%, #e0f0ff 100%);
        border-color: #c9def6;
      }
      .reco-summary-label {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #7c6a37;
        margin-bottom: 6px;
      }
      .reco-summary-card.alt .reco-summary-label {
        color: #41617f;
      }
      .reco-summary-value {
        font-size: 24px;
        font-weight: 700;
        color: #3d2f11;
        line-height: 1.05;
        margin-bottom: 4px;
      }
      .reco-summary-card.alt .reco-summary-value {
        color: #17324d;
      }
      .reco-summary-help {
        font-size: 12px;
        color: #6f6658;
      }
      .reco-section {
        background: #fbfcfe;
        border: 1px solid #dbe4f0;
        border-radius: 16px;
        padding: 14px;
      }
      .reco-section-title {
        font-size: 15px;
        font-weight: 700;
        color: #17324d;
        margin-bottom: 6px;
      }
      .reco-section-help {
        font-size: 12px;
        color: #6d7d94;
        margin-bottom: 12px;
      }
      .reco-badge {
        display: inline-block;
        border-radius: 999px;
        padding: 4px 10px;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.03em;
      }
      .reco-badge-parent {
        background: #dff7ea;
        color: #146c43;
      }
      .reco-badge-child {
        background: #fff0d9;
        color: #a75f00;
      }
      .reco-badge-neutral {
        background: #e9eef5;
        color: #405469;
      }
      .reco-rome-stack {
        display: flex;
        flex-direction: column;
        gap: 12px;
        max-height: 420px;
        overflow-y: auto;
        padding-right: 4px;
      }
      .reco-rome-card {
        background: #ffffff;
        border: 1px solid #d6e0ed;
        border-radius: 16px;
        padding: 14px;
      }
      .reco-rome-head {
        display: flex;
        justify-content: space-between;
        gap: 10px;
        align-items: flex-start;
        margin-bottom: 10px;
      }
      .reco-rome-title {
        font-size: 16px;
        font-weight: 700;
        color: #16324a;
      }
      .reco-rome-subtitle {
        font-size: 12px;
        color: #6c7b8d;
        margin-top: 2px;
      }
      .reco-rome-metrics {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
      }
      .reco-rome-metric {
        background: #f4f7fc;
        border: 1px solid #dbe4f0;
        border-radius: 999px;
        padding: 5px 10px;
        font-size: 12px;
        color: #29435d;
      }
      .reco-rome-block {
        margin-top: 10px;
      }
      .reco-rome-block-title {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #60708f;
        margin-bottom: 6px;
      }
      .reco-ref-chip {
        display: inline-block;
        background: #f4f7fc;
        border: 1px solid #d7e1ee;
        border-radius: 12px;
        padding: 6px 8px;
        margin: 0 6px 6px 0;
        font-size: 12px;
        color: #29435d;
        vertical-align: top;
      }
      .reco-ref-chip.is-empty {
        background: #f8fafc;
        color: #8491a3;
      }
      @media (max-width: 1200px) {
        .reco-summary-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }
      @media (max-width: 700px) {
        .reco-summary-grid {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  fluidRow(
    column(
      width = 4,
      div(
        class = "app-card",
        h4("Composer une famille"),
        p("Filtrez et selectionnez des lignes dans le referentiel, puis choisissez un parent et les enfants restants."),
        textInput("idep_agent", "IDEP agent", value = "DFEC5Z"),
        selectizeInput(
          "parent_code",
          "Parent",
          choices = character(0),
          selected = NULL,
          options = list(placeholder = "Choisir le parent")
        ),
        selectizeInput(
          "child_codes",
          "Enfants",
          choices = character(0),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "Choisir les enfants")
        ),
        div(
          class = "action-row",
          actionButton("simulate_draft", "Simuler la filiation"),
          actionButton("create_proposal", "Creer la proposition", class = "btn-primary")
        )
      ),
      div(
        class = "app-card",
        h4("Resume de la proposition"),
        uiOutput("family_preview")
      ),
      div(
        class = "app-card",
        h4("Preconisations"),
        uiOutput("recommendation_panel")
      )
    ),
    column(
      width = 8,
      tabsetPanel(
        tabPanel("Referentiel", DTOutput("table_ref")),
        tabPanel(
          "Stock familles",
          div(
            class = "action-row",
            actionButton("delete_family", "Supprimer la famille selectionnee", class = "btn-danger")
          ),
          br(),
          DTOutput("table_families")
        ),
        tabPanel(
          "Propositions",
          div(
            class = "action-row",
            actionButton("delete_proposal", "Supprimer la proposition selectionnee", class = "btn-danger")
          ),
          br(),
          DTOutput("table_proposals"),
          br(),
          uiOutput("proposal_detail")
        ),
        tabPanel("Detail stock", uiOutput("family_detail"))
      )
    )
  )
)

}
