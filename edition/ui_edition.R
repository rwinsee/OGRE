edition_ui <- function() {
  info_note <- function(...) {
    div(
      class = "info-note",
      span(class = "info-dot", "i"),
      div(...)
    )
  }

  helper_details <- function(summary_text, ...) {
    tags$details(
      class = "helper-details",
      tags$summary(
        span(class = "info-dot", "i"),
        span(summary_text)
      ),
      div(class = "helper-details-body", ...)
    )
  }

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
      .card-head {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 10px;
        flex-wrap: wrap;
        margin-bottom: 8px;
      }
      .card-head h4 {
        margin: 0;
      }
      .card-copy {
        font-size: 13px;
        line-height: 1.55;
        color: #60708f;
        margin-bottom: 12px;
      }
      .card-badge {
        display: inline-flex;
        align-items: center;
        padding: 5px 10px;
        border-radius: 999px;
        border: 1px solid #d6e1ef;
        background: #f5f8fc;
        color: #35506b;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.03em;
      }
      .section-kicker {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #5f7793;
        margin-bottom: 6px;
      }
      .guide-step {
        display: flex;
        align-items: flex-start;
        gap: 10px;
        margin-top: 12px;
      }
      .guide-step-index {
        width: 28px;
        height: 28px;
        border-radius: 999px;
        background: #17324d;
        color: #ffffff;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-size: 13px;
        font-weight: 700;
        flex: 0 0 auto;
      }
      .guide-step-title {
        font-size: 14px;
        font-weight: 700;
        color: #17324d;
        margin-bottom: 2px;
      }
      .guide-step-copy {
        font-size: 13px;
        line-height: 1.5;
        color: #5a6c83;
      }
      .info-note {
        display: flex;
        align-items: flex-start;
        gap: 10px;
        background: #f6f9fd;
        border: 1px solid #dce6f2;
        border-radius: 14px;
        padding: 10px 12px;
        margin: -4px 0 12px;
      }
      .info-note p {
        margin: 0;
        font-size: 13px;
        line-height: 1.5;
        color: #51657f;
      }
      .info-dot {
        width: 20px;
        height: 20px;
        border-radius: 999px;
        background: #17324d;
        color: #ffffff;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-size: 12px;
        font-weight: 700;
        flex: 0 0 auto;
        margin-top: 1px;
      }
      .helper-details {
        border: 1px solid #dce5f1;
        border-radius: 14px;
        background: #fcfdff;
        padding: 0 12px;
        margin-top: 10px;
      }
      .helper-details summary {
        list-style: none;
        display: flex;
        align-items: center;
        gap: 8px;
        cursor: pointer;
        padding: 12px 0;
        font-size: 13px;
        font-weight: 700;
        color: #17324d;
      }
      .helper-details summary::-webkit-details-marker {
        display: none;
      }
      .helper-details-body {
        padding-bottom: 12px;
      }
      .helper-details-body p {
        margin: 0 0 8px;
        font-size: 13px;
        line-height: 1.55;
        color: #5a6c83;
      }
      .helper-details-body p:last-child {
        margin-bottom: 0;
      }
      .action-row {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 8px;
      }
      .action-help {
        margin-top: 10px;
        font-size: 12px;
        line-height: 1.5;
        color: #687993;
      }
      .tab-intro {
        margin-bottom: 12px;
      }
      .tab-intro .info-note {
        margin-bottom: 0;
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
      @media (max-width: 991px) {
        .card-head {
          align-items: flex-start;
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
        div(class = "section-kicker", "Edition"),
        div(
          class = "card-head",
          h4("Composer une famille"),
          span(class = "card-badge", "Etape guidee")
        ),
        p(
          class = "card-copy",
          "Pour un usage fluide, avancez en trois temps : selection dans le referentiel, choix du parent, puis verification avant creation."
        ),
        div(
          class = "guide-step",
          span(class = "guide-step-index", "1"),
          div(
            div(class = "guide-step-title", "Reperez les lignes utiles"),
            div(class = "guide-step-copy", "Commencez dans l'onglet Selection pour filtrer et retenir seulement les metiers a regrouper.")
          )
        ),
        div(
          class = "guide-step",
          span(class = "guide-step-index", "2"),
          div(
            div(class = "guide-step-title", "Choisissez un parent"),
            div(class = "guide-step-copy", "Le parent devient la ligne de reference. Les autres lignes constituent les enfants de la famille.")
          )
        ),
        div(
          class = "guide-step",
          span(class = "guide-step-index", "3"),
          div(
            div(class = "guide-step-title", "Verifiez avant envoi"),
            div(class = "guide-step-copy", "Le resume et les preconisations servent a repere les incoherences avant de creer la proposition.")
          )
        ),
        helper_details(
          "Repere rapide pour demarrer",
          p("La verification ne sauvegarde rien. Elle permet juste de controler la composition de la famille avant envoi."),
          p("Si un doute persiste, comparez avec le stock existant ou une proposition deja creee dans les onglets de droite.")
        ),
        textInput("idep_agent", "1. IDEP agent", value = "DFEC5Z"),
        info_note(
          p("Cet identifiant sert a tracer qui cree ou modifie la proposition dans le workflow.")
        ),
        selectizeInput(
          "parent_code",
          "2. Parent",
          choices = character(0),
          selected = NULL,
          options = list(placeholder = "Choisir le parent")
        ),
        info_note(
          p("Choisissez la ligne qui represente le mieux la famille. Elle servira de point d'ancrage pour la lecture metier.")
        ),
        selectizeInput(
          "child_codes",
          "3. Enfants",
          choices = character(0),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "Choisir les enfants")
        ),
        info_note(
          p("Ajoutez ici les autres lignes rattachees au parent. Evitez de melanger des lignes qui n'auraient qu'un lien trop faible.")
        ),
        helper_details(
          "Comment choisir le parent et les enfants ?",
          p("Le parent est souvent la ligne la plus stable ou la plus representative du regroupement attendu."),
          p("Les enfants doivent rester lisibles pour un utilisateur non expert : si l'ensemble parait trop heterogene, il vaut mieux scinder la famille.")
        ),
        div(
          class = "action-row",
          actionButton("simulate_draft", "Verifier la filiation"),
          actionButton("create_proposal", "Creer la proposition", class = "btn-primary")
        ),
        p(
          class = "action-help",
          "Verifier la filiation permet de controler sans enregistrer. Creer la proposition lance ensuite le passage dans le workflow."
        )
      ),
      div(
        class = "app-card",
        div(
          class = "card-head",
          h4("Resume de la proposition"),
          span(class = "card-badge", "Controle")
        ),
        p(
          class = "card-copy",
          "Ce bloc confirme ce qui sera envoye. S'il y a un doute ici, revenez sur le parent ou la liste des enfants."
        ),
        uiOutput("family_preview")
      ),
      div(
        class = "app-card",
        div(
          class = "card-head",
          h4("Preconisations"),
          span(class = "card-badge", "Aide a la decision")
        ),
        p(
          class = "card-copy",
          "Les preconisations n'imposent rien. Elles servent a reperer rapidement les regroupements a confirmer ou a retravailler."
        ),
        helper_details(
          "Que lire en priorite ?",
          p("Commencez par les ecarts forts entre parent et enfants, puis utilisez les cartes detaillees pour comprendre si l'ecart est acceptable."),
          p("Si une recommandation vous semble logique metierement, vous pouvez conserver la famille : le but est d'eclairer la decision, pas de la bloquer.")
        ),
        uiOutput("recommendation_panel")
      )
    ),
    column(
      width = 8,
      tabsetPanel(
        tabPanel(
          "Selection",
          div(
            class = "tab-intro",
            info_note(
              p("Selectionnez dans le referentiel les lignes a regrouper. Cette selection alimente ensuite les listes Parent et Enfants.")
            ),
            helper_details(
              "Bon reflexe de depart",
              p("Commencez par un filtre simple sur un mot metier, un code ou une famille proche."),
              p("Ne gardez que les lignes qui doivent vraiment vivre ensemble pour eviter une famille trop large ou trop floue.")
            )
          ),
          DTOutput("table_ref")
        ),
        tabPanel(
          "Stock existant",
          div(
            class = "tab-intro",
            info_note(
              p("Consultez le stock pour verifier si une famille proche existe deja avant d'en creer une nouvelle.")
            )
          ),
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
            class = "tab-intro",
            info_note(
              p("Retrouvez ici les propositions deja creees, leur contenu detaille et les suppressions eventuelles avant traitement.")
            )
          ),
          div(
            class = "action-row",
            actionButton("delete_proposal", "Supprimer la proposition selectionnee", class = "btn-danger")
          ),
          br(),
          DTOutput("table_proposals"),
          br(),
          uiOutput("proposal_detail")
        ),
        tabPanel(
          "Detail stock",
          div(
            class = "tab-intro",
            info_note(
              p("Ce detail permet de comparer votre idee de famille avec une famille deja publiee dans le stock.")
            )
          ),
          uiOutput("family_detail")
        )
      )
    )
  )
)

}
