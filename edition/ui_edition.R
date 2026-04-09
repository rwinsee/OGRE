edition_ui <- function() {
  helper_details <- function(summary_text, ...) {
    tags$details(
      class = "helper-details",
      tags$summary(
        span(class = "info-dot info-dot-inline", "i"),
        span(summary_text)
      ),
      div(class = "helper-details-body", ...)
    )
  }

  inline_info <- function(text) {
    tags$span(
      class = "inline-info",
      tabindex = "0",
      `data-tooltip` = text,
      `aria-label` = text,
      "i"
    )
  }

  label_with_info <- function(label, info_text) {
    tags$span(
      class = "label-with-info",
      span(label),
      inline_info(info_text)
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
      .info-dot-inline {
        margin-top: 0;
      }
      .label-with-info {
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
      .inline-info {
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
      }
      .inline-info::after {
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
        text-transform: none;
        letter-spacing: normal;
        white-space: normal;
        box-shadow: 0 14px 26px rgba(23, 43, 77, 0.18);
        opacity: 0;
        visibility: hidden;
        pointer-events: none;
        transition: opacity 0.12s ease, transform 0.12s ease;
        z-index: 20;
      }
      .inline-info::before {
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
      .inline-info:hover::after,
      .inline-info:hover::before,
      .inline-info:focus::after,
      .inline-info:focus::before {
        opacity: 1;
        visibility: visible;
      }
      .inline-info:hover::after,
      .inline-info:focus::after {
        transform: translateX(-50%) translateY(0);
      }
      .inline-info:focus {
        outline: 2px solid #8cb6dd;
        outline-offset: 2px;
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
        display: flex;
        align-items: center;
        justify-content: flex-start;
        gap: 8px;
        flex-wrap: wrap;
        color: #5f7793;
        font-size: 12px;
      }
      .tab-intro-copy {
        line-height: 1.5;
      }
      .preview-table {
        margin-top: 10px;
      }
      .preview-table th {
        background: #f4f7fc;
      }
      .subpanel-tabs > .nav {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
        border-bottom: none;
        margin: 4px 0 14px;
      }
      .subpanel-tabs > .nav > li {
        float: none;
        margin: 0;
      }
      .subpanel-tabs > .nav > li > a {
        border: 1px solid #d6e1ef;
        border-radius: 999px;
        background: #f7f9fc;
        color: #35506b;
        font-size: 12px;
        font-weight: 700;
        padding: 8px 14px;
      }
      .subpanel-tabs > .nav > li > a:hover {
        background: #eef3f9;
        border-color: #c4d4e8;
      }
      .subpanel-tabs > .nav > li.active > a,
      .subpanel-tabs > .nav > li.active > a:hover,
      .subpanel-tabs > .nav > li.active > a:focus {
        background: #17324d;
        border-color: #17324d;
        color: #ffffff;
      }
      .subpanel-content {
        min-height: 180px;
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
        textInput(
          "idep_agent",
          label_with_info(
            "1. IDEP agent",
            "Cet identifiant sert a tracer qui cree ou modifie la proposition dans le workflow."
          ),
          value = "DFEC5Z"
        ),
        selectizeInput(
          "parent_code",
          label_with_info(
            "2. Parent",
            "Choisissez la ligne la plus representative de la famille. Elle sert de point d'ancrage pour la lecture metier."
          ),
          choices = character(0),
          selected = NULL,
          options = list(placeholder = "Choisir le parent")
        ),
        selectizeInput(
          "child_codes",
          label_with_info(
            "3. Enfants",
            "Ajoutez ici les autres lignes rattachees au parent. Si le groupe parait trop heterogene, mieux vaut scinder la famille."
          ),
          choices = character(0),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "Choisir les enfants")
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
          h4("Analyse de la proposition"),
          span(class = "card-badge", "Controle + aide")
        ),
        p(
          class = "card-copy",
          "Basculez entre le resume et les preconisations sans descendre plus bas dans la page."
        ),
        div(
          class = "subpanel-tabs",
          tabsetPanel(
            id = "edition_analysis_panel",
            type = "tabs",
            tabPanel(
              "Resume",
              div(
                class = "subpanel-content",
                p(
                  class = "card-copy",
                  "Ce bloc confirme ce qui sera envoye. S'il y a un doute ici, revenez sur le parent ou la liste des enfants."
                ),
                uiOutput("family_preview")
              )
            ),
            tabPanel(
              "Preconisations",
              div(
                class = "subpanel-content",
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
            )
          )
        )
      )
    ),
    column(
      width = 8,
      tabsetPanel(
        tabPanel(
          "Selection",
          div(
            class = "tab-intro",
            span(class = "tab-intro-copy", "Selectionnez dans le referentiel les lignes a regrouper."),
            inline_info("Cette selection alimente ensuite les listes Parent et Enfants dans la zone de composition."),
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
            span(class = "tab-intro-copy", "Consultez le stock avant de creer une nouvelle famille."),
            inline_info("Cet onglet sert surtout a verifier qu'une famille proche n'existe pas deja.")
          ),
          uiOutput("stock_families_notice"),
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
            span(class = "tab-intro-copy", "Retrouvez ici les propositions deja creees."),
            inline_info("Vous pouvez y relire le detail d'une proposition avant traitement ou supprimer une proposition si besoin.")
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
            span(class = "tab-intro-copy", "Comparez votre idee avec une famille deja publiee."),
            inline_info("Pratique pour verifier si votre nouvelle famille apporte vraiment quelque chose par rapport a l'existant.")
          ),
          uiOutput("family_detail")
        )
      )
    )
  )
)

}
